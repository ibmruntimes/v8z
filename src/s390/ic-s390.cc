// Copyright 2012 the V8 project authors. All rights reserved.
//
// Copyright IBM Corp. 2012-2014. All rights reserved.
//
// Redistribution and use in source and binary forms, with or without
// modification, are permitted provided that the following conditions are
// met:
//
//     * Redistributions of source code must retain the above copyright
//       notice, this list of conditions and the following disclaimer.
//     * Redistributions in binary form must reproduce the above
//       copyright notice, this list of conditions and the following
//       disclaimer in the documentation and/or other materials provided
//       with the distribution.
//     * Neither the name of Google Inc. nor the names of its
//       contributors may be used to endorse or promote products derived
//       from this software without specific prior written permission.
//
// THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
// "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
// LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
// A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
// OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
// SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
// LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
// DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
// THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
// (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
// OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

#include "v8.h"

#if defined(V8_TARGET_ARCH_S390)

#include "assembler-s390.h"
#include "code-stubs.h"
#include "codegen.h"
#include "disasm.h"
#include "ic-inl.h"
#include "runtime.h"
#include "stub-cache.h"

namespace v8 {
namespace internal {


// ----------------------------------------------------------------------------
// Static IC stub generators.
//

#define __ ACCESS_MASM(masm)

static void GenerateGlobalInstanceTypeCheck(MacroAssembler* masm,
                                            Register type,
                                            Label* global_object) {
  // Register usage:
  //   type: holds the receiver instance type on entry.
  __ Cmpi(type, Operand(JS_GLOBAL_OBJECT_TYPE));
  __ beq(global_object);
  __ Cmpi(type, Operand(JS_BUILTINS_OBJECT_TYPE));
  __ beq(global_object);
  __ Cmpi(type, Operand(JS_GLOBAL_PROXY_TYPE));
  __ beq(global_object);
}


// Generated code falls through if the receiver is a regular non-global
// JS object with slow properties and no interceptors.
static void GenerateStringDictionaryReceiverCheck(MacroAssembler* masm,
                                                  Register receiver,
                                                  Register elements,
                                                  Register t0,
                                                  Register t1,
                                                  Label* miss) {
  // Register usage:
  //   receiver: holds the receiver on entry and is unchanged.
  //   elements: holds the property dictionary on fall through.
  // Scratch registers:
  //   t0: used to holds the receiver map.
  //   t1: used to holds the receiver instance type, receiver bit mask and
  //       elements map.

  // Check that the receiver isn't a smi.
  __ JumpIfSmi(receiver, miss);

  // Check that the receiver is a valid JS object.
  __ CompareObjectType(receiver, t0, t1, FIRST_SPEC_OBJECT_TYPE);
  __ blt(miss);

  // If this assert fails, we have to check upper bound too.
  STATIC_ASSERT(LAST_TYPE == LAST_SPEC_OBJECT_TYPE);

  GenerateGlobalInstanceTypeCheck(masm, t1, miss);

  // Check that the global object does not require access checks.
  __ LoadlB(t1, FieldMemOperand(t0, Map::kBitFieldOffset));
  __ mov(r0, Operand((1 << Map::kIsAccessCheckNeeded) |
                     (1 << Map::kHasNamedInterceptor)));
  __ AndP(r0, t1);
  __ bne(miss /*, cr0*/);

  __ LoadP(elements, FieldMemOperand(receiver, JSObject::kPropertiesOffset));
  __ LoadP(t1, FieldMemOperand(elements, HeapObject::kMapOffset));
  __ CompareRoot(t1, Heap::kHashTableMapRootIndex);
  __ bne(miss);
}


// Helper function used from LoadIC/CallIC GenerateNormal.
//
// elements: Property dictionary. It is not clobbered if a jump to the miss
//           label is done.
// name:     Property name. It is not clobbered if a jump to the miss label is
//           done
// result:   Register for the result. It is only updated if a jump to the miss
//           label is not done. Can be the same as elements or name clobbering
//           one of these in the case of not jumping to the miss label.
// The two scratch registers need to be different from elements, name and
// result.
// The generated code assumes that the receiver has slow properties,
// is not a global object and does not have interceptors.
static void GenerateDictionaryLoad(MacroAssembler* masm,
                                   Label* miss,
                                   Register elements,
                                   Register name,
                                   Register result,
                                   Register scratch1,
                                   Register scratch2) {
  // Main use of the scratch registers.
  // scratch1: Used as temporary and to hold the capacity of the property
  //           dictionary.
  // scratch2: Used as temporary.
  Label done;

  // Probe the dictionary.
  StringDictionaryLookupStub::GeneratePositiveLookup(masm,
                                                     miss,
                                                     &done,
                                                     elements,
                                                     name,
                                                     scratch1,
                                                     scratch2);

  // If probing finds an entry check that the value is a normal
  // property.
  __ bind(&done);  // scratch2 == elements + 4 * index
  const int kElementsStartOffset = StringDictionary::kHeaderSize +
      StringDictionary::kElementsStartIndex * kPointerSize;
  const int kDetailsOffset = kElementsStartOffset + 2 * kPointerSize;
  __ LoadP(scratch1, FieldMemOperand(scratch2, kDetailsOffset));
  __ LoadRR(r0, scratch2);
  __ LoadSmiLiteral(scratch2, Smi::FromInt(PropertyDetails::TypeField::kMask));
  __ AndP(scratch2, scratch1/*, SetRC*/);
  // Should be okay to remove RC
  __ bne(miss /*, cr0*/);
  __ LoadRR(scratch2, r0);

  // Get the value at the masked, scaled index and return.
  __ LoadP(result,
         FieldMemOperand(scratch2, kElementsStartOffset + 1 * kPointerSize));
}


// Helper function used from StoreIC::GenerateNormal.
//
// elements: Property dictionary. It is not clobbered if a jump to the miss
//           label is done.
// name:     Property name. It is not clobbered if a jump to the miss label is
//           done
// value:    The value to store.
// The two scratch registers need to be different from elements, name and
// result.
// The generated code assumes that the receiver has slow properties,
// is not a global object and does not have interceptors.
static void GenerateDictionaryStore(MacroAssembler* masm,
                                    Label* miss,
                                    Register elements,
                                    Register name,
                                    Register value,
                                    Register scratch1,
                                    Register scratch2) {
  // Main use of the scratch registers.
  // scratch1: Used as temporary and to hold the capacity of the property
  //           dictionary.
  // scratch2: Used as temporary.
  Label done;

  // Probe the dictionary.
  StringDictionaryLookupStub::GeneratePositiveLookup(masm,
                                                     miss,
                                                     &done,
                                                     elements,
                                                     name,
                                                     scratch1,
                                                     scratch2);

  // If probing finds an entry in the dictionary check that the value
  // is a normal property that is not read only.
  __ bind(&done);  // scratch2 == elements + 4 * index
  const int kElementsStartOffset = StringDictionary::kHeaderSize +
      StringDictionary::kElementsStartIndex * kPointerSize;
  const int kDetailsOffset = kElementsStartOffset + 2 * kPointerSize;
  int kTypeAndReadOnlyMask = PropertyDetails::TypeField::kMask |
    PropertyDetails::AttributesField::encode(READ_ONLY);
  __ LoadP(scratch1, FieldMemOperand(scratch2, kDetailsOffset));
  __ LoadRR(r0, scratch2);
  __ LoadSmiLiteral(scratch2, Smi::FromInt(kTypeAndReadOnlyMask));
  __ AndP(scratch2, scratch1/*, SetRC*/);  // Should be OK to remove RC
  __ bne(miss /*, cr0*/);
  __ LoadRR(scratch2, r0);

  // Store the value at the masked, scaled index and return.
  const int kValueOffset = kElementsStartOffset + kPointerSize;
  __ AddP(scratch2, Operand(kValueOffset - kHeapObjectTag));
  __ StoreP(value, MemOperand(scratch2));

  // Update the write barrier. Make sure not to clobber the value.
  __ LoadRR(scratch1, value);
  __ RecordWrite(
      elements, scratch2, scratch1, kLRHasNotBeenSaved, kDontSaveFPRegs);
}


void LoadIC::GenerateArrayLength(MacroAssembler* masm) {
  // ----------- S t a t e -------------
  //  -- r4    : name
  //  -- lr    : return address
  //  -- r2    : receiver
  //  -- sp[0] : receiver
  // -----------------------------------
  Label miss;

  StubCompiler::GenerateLoadArrayLength(masm, r2, r5, &miss);
  __ bind(&miss);
  StubCompiler::GenerateLoadMiss(masm, Code::LOAD_IC);
}


void LoadIC::GenerateStringLength(MacroAssembler* masm, bool support_wrappers) {
  // ----------- S t a t e -------------
  //  -- r4    : name
  //  -- lr    : return address
  //  -- r2    : receiver
  //  -- sp[0] : receiver
  // -----------------------------------
  Label miss;

  StubCompiler::GenerateLoadStringLength(masm, r2, r3, r5, &miss,
                                         support_wrappers);
  // Cache miss: Jump to runtime.
  __ bind(&miss);
  StubCompiler::GenerateLoadMiss(masm, Code::LOAD_IC);
}


void LoadIC::GenerateFunctionPrototype(MacroAssembler* masm) {
  // ----------- S t a t e -------------
  //  -- r4    : name
  //  -- lr    : return address
  //  -- r2    : receiver
  //  -- sp[0] : receiver
  // -----------------------------------
  Label miss;

  StubCompiler::GenerateLoadFunctionPrototype(masm, r2, r3, r5, &miss);
  __ bind(&miss);
  StubCompiler::GenerateLoadMiss(masm, Code::LOAD_IC);
}


// Checks the receiver for special cases (value type, slow case bits).
// Falls through for regular JS object.
static void GenerateKeyedLoadReceiverCheck(MacroAssembler* masm,
                                           Register receiver,
                                           Register map,
                                           Register scratch,
                                           int interceptor_bit,
                                           Label* slow) {
  // Check that the object isn't a smi.
  __ JumpIfSmi(receiver, slow);
  // Get the map of the receiver.
  __ LoadP(map, FieldMemOperand(receiver, HeapObject::kMapOffset));
  // Check bit field.
  __ LoadlB(scratch, FieldMemOperand(map, Map::kBitFieldOffset));
  ASSERT(((1 << Map::kIsAccessCheckNeeded) | (1 << interceptor_bit)) < 0x8000);
  __ mov(r0,
          Operand((1 << Map::kIsAccessCheckNeeded) | (1 << interceptor_bit)));
  __ AndP(r0, scratch);
  __ bne(slow /*, cr0*/);
  // Check that the object is some kind of JS object EXCEPT JS Value type.
  // In the case that the object is a value-wrapper object,
  // we enter the runtime system to make sure that indexing into string
  // objects work as intended.
  ASSERT(JS_OBJECT_TYPE > JS_VALUE_TYPE);
  __ LoadlB(scratch, FieldMemOperand(map, Map::kInstanceTypeOffset));
  __ Cmpi(scratch, Operand(JS_OBJECT_TYPE));
  __ blt(slow);
}


// Loads an indexed element from a fast case array.
// If not_fast_array is NULL, doesn't perform the elements map check.
static void GenerateFastArrayLoad(MacroAssembler* masm,
                                  Register receiver,
                                  Register key,
                                  Register elements,
                                  Register scratch1,
                                  Register scratch2,
                                  Register result,
                                  Label* not_fast_array,
                                  Label* out_of_range) {
  // Register use:
  //
  // receiver - holds the receiver on entry.
  //            Unchanged unless 'result' is the same register.
  //
  // key      - holds the smi key on entry.
  //            Unchanged unless 'result' is the same register.
  //
  // elements - holds the elements of the receiver on exit.
  //
  // result   - holds the result on exit if the load succeeded.
  //            Allowed to be the the same as 'receiver' or 'key'.
  //            Unchanged on bailout so 'receiver' and 'key' can be safely
  //            used by further computation.
  //
  // Scratch registers:
  //
  // scratch1 - used to hold elements map and elements length.
  //            Holds the elements map if not_fast_array branch is taken.
  //
  // scratch2 - used to hold the loaded value.

  __ LoadP(elements, FieldMemOperand(receiver, JSObject::kElementsOffset));
  if (not_fast_array != NULL) {
    // Check that the object is in fast mode and writable.
    __ LoadP(scratch1, FieldMemOperand(elements, HeapObject::kMapOffset));
    __ CompareRoot(scratch1, Heap::kFixedArrayMapRootIndex);
    __ bne(not_fast_array);
  } else {
    __ AssertFastElements(elements);
  }
  // Check that the key (index) is within bounds.
  __ LoadP(scratch1, FieldMemOperand(elements, FixedArray::kLengthOffset));
  __ Cmpl(key, scratch1);
  __ bge(out_of_range);
  // Fast case: Do the load.
  __ AddP(scratch1, elements,
                    Operand(FixedArray::kHeaderSize - kHeapObjectTag));
  // The key is a smi.
  __ SmiToPtrArrayOffset(scratch2, key);
  __ LoadP(scratch2, MemOperand(scratch2, scratch1));
  __ CompareRoot(scratch2, Heap::kTheHoleValueRootIndex);
  // In case the loaded value is the_hole we have to consult GetProperty
  // to ensure the prototype chain is searched.
  __ beq(out_of_range);
  __ LoadRR(result, scratch2);
}


// Checks whether a key is an array index string or a symbol string.
// Falls through if a key is a symbol.
static void GenerateKeyStringCheck(MacroAssembler* masm,
                                   Register key,
                                   Register map,
                                   Register hash,
                                   Label* index_string,
                                   Label* not_symbol) {
  // assumes that r7 is free for scratch use
  // The key is not a smi.
  // Is it a string?
  __ CompareObjectType(key, map, hash, FIRST_NONSTRING_TYPE);
  __ bge(not_symbol);

  // Is the string an array index, with cached numeric value?
  __ LoadlW(hash, FieldMemOperand(key, String::kHashFieldOffset));
  __ mov(r7, Operand(String::kContainsCachedArrayIndexMask));
  __ LoadRR(r0, r7);
  __ AndP(r0, hash/*, SetRC*/);  // Should be OK to remove RC
  __ beq(index_string /*, cr0*/);

  // Is the string a symbol?
  // map: key map
  __ LoadlB(hash, FieldMemOperand(map, Map::kInstanceTypeOffset));
  STATIC_ASSERT(kSymbolTag != 0);
  __ mov(r0, Operand(kIsSymbolMask));
  __ AndP(r0, hash);
  __ beq(not_symbol /*, cr0*/);
}


// Defined in ic.cc.
Object* CallIC_Miss(Arguments args);

// The generated code does not accept smi keys.
// The generated code falls through if both probes miss.
void CallICBase::GenerateMonomorphicCacheProbe(MacroAssembler* masm,
                                               int argc,
                                               Code::Kind kind,
                                               Code::ExtraICState extra_state) {
  // ----------- S t a t e -------------
  //  -- r3    : receiver
  //  -- r4    : name
  // -----------------------------------
  Label number, non_number, non_string, boolean, probe, miss;

  // Probe the stub cache.
  Code::Flags flags = Code::ComputeFlags(kind,
                                         MONOMORPHIC,
                                         extra_state,
                                         Code::NORMAL,
                                         argc);
  Isolate::Current()->stub_cache()->GenerateProbe(
      masm, flags, r3, r4, r5, r6, r7, r8);

  // If the stub cache probing failed, the receiver might be a value.
  // For value objects, we use the map of the prototype objects for
  // the corresponding JSValue for the cache and that is what we need
  // to probe.
  //
  // Check for number.
  __ JumpIfSmi(r3, &number);
  __ CompareObjectType(r3, r5, r5, HEAP_NUMBER_TYPE);
  __ bne(&non_number);
  __ bind(&number);
  StubCompiler::GenerateLoadGlobalFunctionPrototype(
      masm, Context::NUMBER_FUNCTION_INDEX, r3);
  __ b(&probe);

  // Check for string.
  __ bind(&non_number);
  __ Cmpli(r5, Operand(FIRST_NONSTRING_TYPE));
  __ bge(&non_string);
  StubCompiler::GenerateLoadGlobalFunctionPrototype(
      masm, Context::STRING_FUNCTION_INDEX, r3);
  __ b(&probe);

  // Check for boolean.
  __ bind(&non_string);
  __ CompareRoot(r3, Heap::kTrueValueRootIndex);
  __ beq(&boolean);
  __ CompareRoot(r3, Heap::kFalseValueRootIndex);
  __ bne(&miss);
  __ bind(&boolean);
  StubCompiler::GenerateLoadGlobalFunctionPrototype(
      masm, Context::BOOLEAN_FUNCTION_INDEX, r3);

  // Probe the stub cache for the value object.
  __ bind(&probe);
  Isolate::Current()->stub_cache()->GenerateProbe(
      masm, flags, r3, r4, r5, r6, r7, r8);

  __ bind(&miss);
}


static void GenerateFunctionTailCall(MacroAssembler* masm,
                                     int argc,
                                     Label* miss,
                                     Register scratch) {
  // r3: function

  // Check that the value isn't a smi.
  __ JumpIfSmi(r3, miss);

  // Check that the value is a JSFunction.
  __ CompareObjectType(r3, scratch, scratch, JS_FUNCTION_TYPE);
  __ bne(miss);

  // Invoke the function.
  ParameterCount actual(argc);
  __ InvokeFunction(r3, actual, JUMP_FUNCTION,
                    NullCallWrapper(), CALL_AS_METHOD);
}


void CallICBase::GenerateNormal(MacroAssembler* masm, int argc) {
  // ----------- S t a t e -------------
  //  -- r4    : name
  //  -- lr    : return address
  // -----------------------------------
  Label miss;

  // Get the receiver of the function from the stack into r3.
  __ LoadP(r3, MemOperand(sp, argc * kPointerSize), r0);

  GenerateStringDictionaryReceiverCheck(masm, r3, r2, r5, r6, &miss);

  // r2: elements
  // Search the dictionary - put result in register r3.
  GenerateDictionaryLoad(masm, &miss, r2, r4, r3, r5, r6);

  GenerateFunctionTailCall(masm, argc, &miss, r6);

  __ bind(&miss);
}


void CallICBase::GenerateMiss(MacroAssembler* masm,
                              int argc,
                              IC::UtilityId id,
                              Code::ExtraICState extra_state) {
  // ----------- S t a t e -------------
  //  -- r4    : name
  //  -- lr    : return address
  // -----------------------------------
  Isolate* isolate = masm->isolate();

  if (id == IC::kCallIC_Miss) {
    __ IncrementCounter(isolate->counters()->call_miss(), 1, r5, r6);
  } else {
    __ IncrementCounter(isolate->counters()->keyed_call_miss(), 1, r5, r6);
  }

  // Get the receiver of the function from the stack.
  __ LoadP(r5, MemOperand(sp, argc * kPointerSize), r0);

  {
    FrameScope scope(masm, StackFrame::INTERNAL);

    // Push the receiver and the name of the function.
    __ Push(r5, r4);

    // Call the entry.
    __ LoadImmP(r2, Operand(2));
    __ mov(r3, Operand(ExternalReference(IC_Utility(id), isolate)));

    CEntryStub stub(1);
    __ CallStub(&stub);

    // Move result to r3 and leave the internal frame.
    __ LoadRR(r3, r2);
  }

  // Check if the receiver is a global object of some sort.
  // This can happen only for regular CallIC but not KeyedCallIC.
  if (id == IC::kCallIC_Miss) {
    Label invoke, global;
    __ LoadP(r4, MemOperand(sp, argc * kPointerSize), r0);  // receiver
    __ JumpIfSmi(r4, &invoke);
    __ CompareObjectType(r4, r5, r5, JS_GLOBAL_OBJECT_TYPE);
    __ beq(&global);
    __ Cmpi(r5, Operand(JS_BUILTINS_OBJECT_TYPE));
    __ bne(&invoke);

    // Patch the receiver on the stack.
    __ bind(&global);
    __ LoadP(r4, FieldMemOperand(r4, GlobalObject::kGlobalReceiverOffset));
    __ StoreP(r4, MemOperand(sp, argc * kPointerSize));
    __ bind(&invoke);
  }

  // Invoke the function.
  CallKind call_kind = CallICBase::Contextual::decode(extra_state)
      ? CALL_AS_FUNCTION
      : CALL_AS_METHOD;
  ParameterCount actual(argc);
  __ InvokeFunction(r3,
                    actual,
                    JUMP_FUNCTION,
                    NullCallWrapper(),
                    call_kind);
}


void CallIC::GenerateMegamorphic(MacroAssembler* masm,
                                 int argc,
                                 Code::ExtraICState extra_ic_state) {
  // ----------- S t a t e -------------
  //  -- r4    : name
  //  -- lr    : return address
  // -----------------------------------

  // Get the receiver of the function from the stack into r3.
  __ LoadP(r3, MemOperand(sp, argc * kPointerSize), r0);
  GenerateMonomorphicCacheProbe(masm, argc, Code::CALL_IC, extra_ic_state);
  GenerateMiss(masm, argc, extra_ic_state);
}


void KeyedCallIC::GenerateMegamorphic(MacroAssembler* masm, int argc) {
  // ----------- S t a t e -------------
  //  -- r4    : name
  //  -- lr    : return address
  // -----------------------------------

  // Get the receiver of the function from the stack into r3.
  __ LoadP(r3, MemOperand(sp, argc * kPointerSize), r0);

  Label do_call, slow_call, slow_load, slow_reload_receiver;
  Label check_number_dictionary, check_string, lookup_monomorphic_cache;
  Label index_smi, index_string;

  // Check that the key is a smi.
  __ JumpIfNotSmi(r4, &check_string);
  __ bind(&index_smi);
  // Now the key is known to be a smi. This place is also jumped to from below
  // where a numeric string is converted to a smi.

  GenerateKeyedLoadReceiverCheck(
      masm, r3, r2, r5, Map::kHasIndexedInterceptor, &slow_call);

  GenerateFastArrayLoad(
      masm, r3, r4, r6, r5, r2, r3, &check_number_dictionary,
      &slow_load);
  Counters* counters = masm->isolate()->counters();
  __ IncrementCounter(counters->keyed_call_generic_smi_fast(), 1, r2, r5);

  __ bind(&do_call);
  // receiver in r3 is not used after this point.
  // r4: key
  // r3: function
  GenerateFunctionTailCall(masm, argc, &slow_call, r2);

  __ bind(&check_number_dictionary);
  // r4: key
  // r5: elements map
  // r6: elements
  // Check whether the elements is a number dictionary.
  __ CompareRoot(r5, Heap::kHashTableMapRootIndex);
  __ bne(&slow_load);
  __ SmiUntag(r2, r4);
  // r2: untagged index
  __ LoadFromNumberDictionary(&slow_load, r6, r4, r3, r2, r5, r7);
  __ IncrementCounter(counters->keyed_call_generic_smi_dict(), 1, r2, r5);
  __ b(&do_call);

  __ bind(&slow_load);
  // This branch is taken when calling KeyedCallIC_Miss is neither required
  // nor beneficial.
  __ IncrementCounter(counters->keyed_call_generic_slow_load(), 1, r2, r5);
  {
    FrameScope scope(masm, StackFrame::INTERNAL);
    __ push(r4);  // save the key
    __ Push(r3, r4);  // pass the receiver and the key
    __ CallRuntime(Runtime::kKeyedGetProperty, 2);
    __ pop(r4);  // restore the key
  }
  __ LoadRR(r3, r2);
  __ b(&do_call);

  __ bind(&check_string);
  GenerateKeyStringCheck(masm, r4, r2, r5, &index_string, &slow_call);

  // The key is known to be a symbol.
  // If the receiver is a regular JS object with slow properties then do
  // a quick inline probe of the receiver's dictionary.
  // Otherwise do the monomorphic cache probe.
  GenerateKeyedLoadReceiverCheck(
      masm, r3, r2, r5, Map::kHasNamedInterceptor,
      &lookup_monomorphic_cache);

  __ LoadP(r2, FieldMemOperand(r3, JSObject::kPropertiesOffset));
  __ LoadP(r5, FieldMemOperand(r2, HeapObject::kMapOffset));
  __ CompareRoot(r5, Heap::kHashTableMapRootIndex);
  __ bne(&lookup_monomorphic_cache);

  GenerateDictionaryLoad(masm, &slow_load, r2, r4, r3, r5, r6);
  __ IncrementCounter(counters->keyed_call_generic_lookup_dict(), 1, r2,
                      r5);
  __ b(&do_call);

  __ bind(&lookup_monomorphic_cache);
  __ IncrementCounter(counters->keyed_call_generic_lookup_cache(), 1, r2,
                      r5);
  GenerateMonomorphicCacheProbe(masm,
                                argc,
                                Code::KEYED_CALL_IC,
                                Code::kNoExtraICState);
  // Fall through on miss.

  __ bind(&slow_call);
  // This branch is taken if:
  // - the receiver requires boxing or access check,
  // - the key is neither smi nor symbol,
  // - the value loaded is not a function,
  // - there is hope that the runtime will create a monomorphic call stub
  //   that will get fetched next time.
  __ IncrementCounter(counters->keyed_call_generic_slow(), 1, r2, r5);
  GenerateMiss(masm, argc);

  __ bind(&index_string);
  __ IndexFromHash(r5, r4);
  // Now jump to the place where smi keys are handled.
  __ b(&index_smi);
}


void KeyedCallIC::GenerateNormal(MacroAssembler* masm, int argc) {
  // ----------- S t a t e -------------
  //  -- r4    : name
  //  -- lr    : return address
  // -----------------------------------

  // Check if the name is a string.
  Label miss;
  __ JumpIfSmi(r4, &miss);
  __ IsObjectJSStringType(r4, r2, &miss);

  CallICBase::GenerateNormal(masm, argc);
  __ bind(&miss);
  GenerateMiss(masm, argc);
}


// Defined in ic.cc.
Object* LoadIC_Miss(Arguments args);

void LoadIC::GenerateMegamorphic(MacroAssembler* masm) {
  // ----------- S t a t e -------------
  //  -- r4    : name
  //  -- lr    : return address
  //  -- r2    : receiver
  //  -- sp[0] : receiver
  // -----------------------------------

  // Probe the stub cache.
  Code::Flags flags =
      Code::ComputeFlags(Code::LOAD_IC, MONOMORPHIC);
  Isolate::Current()->stub_cache()->GenerateProbe(
      masm, flags, r2, r4, r5, r6, r7, r8);

  // Cache miss: Jump to runtime.
  GenerateMiss(masm);
}


void LoadIC::GenerateNormal(MacroAssembler* masm) {
  // ----------- S t a t e -------------
  //  -- r4    : name
  //  -- lr    : return address
  //  -- r2    : receiver
  //  -- sp[0] : receiver
  // -----------------------------------
  Label miss;

  GenerateStringDictionaryReceiverCheck(masm, r2, r3, r5, r6, &miss);

  // r3: elements
  GenerateDictionaryLoad(masm, &miss, r3, r4, r2, r5, r6);
  __ Ret();

  // Cache miss: Jump to runtime.
  __ bind(&miss);
  GenerateMiss(masm);
}


void LoadIC::GenerateMiss(MacroAssembler* masm) {
  // ----------- S t a t e -------------
  //  -- r4    : name
  //  -- lr    : return address
  //  -- r2    : receiver
  //  -- sp[0] : receiver
  // -----------------------------------
  Isolate* isolate = masm->isolate();

  __ IncrementCounter(isolate->counters()->load_miss(), 1, r5, r6);

  __ Push(r2, r4);

  // Perform tail call to the entry.
  ExternalReference ref =
      ExternalReference(IC_Utility(kLoadIC_Miss), isolate);
  __ TailCallExternalReference(ref, 2, 1);
}


static MemOperand GenerateMappedArgumentsLookup(MacroAssembler* masm,
                                                Register object,
                                                Register key,
                                                Register scratch1,
                                                Register scratch2,
                                                Register scratch3,
                                                Label* unmapped_case,
                                                Label* slow_case) {
  Heap* heap = masm->isolate()->heap();

  // Check that the receiver is a JSObject. Because of the map check
  // later, we do not need to check for interceptors or whether it
  // requires access checks.
  __ JumpIfSmi(object, slow_case);
  // Check that the object is some kind of JSObject.
  __ CompareObjectType(object, scratch1, scratch2, FIRST_JS_RECEIVER_TYPE);
  __ blt(slow_case);

  // Check that the key is a positive smi.
  __ mov(scratch1, Operand(0x80000001));
  __ LoadRR(r0, scratch1);
  __ AndP(r0, key/*, SetRC*/);  // Should be OK to remove RC
  __ bne(slow_case /*, cr0*/);

  // Load the elements into scratch1 and check its map.
  Handle<Map> arguments_map(heap->non_strict_arguments_elements_map());
  __ LoadP(scratch1, FieldMemOperand(object, JSObject::kElementsOffset));
  __ CheckMap(scratch1, scratch2, arguments_map, slow_case, DONT_DO_SMI_CHECK);

  // Check if element is in the range of mapped arguments. If not, jump
  // to the unmapped lookup with the parameter map in scratch1.
  __ LoadP(scratch2, FieldMemOperand(scratch1, FixedArray::kLengthOffset));
  __ SubSmiLiteral(scratch2, scratch2, Smi::FromInt(2), r0);
  __ Cmpl(key, scratch2);
  __ bge(unmapped_case);

  // Load element index and check whether it is the hole.
  const int kOffset =
      FixedArray::kHeaderSize + 2 * kPointerSize - kHeapObjectTag;

  __ SmiToPtrArrayOffset(scratch3, key);
  __ AddP(scratch3, Operand(kOffset));

  __ LoadP(scratch2, MemOperand(scratch1, scratch3));
  __ CompareRoot(scratch2, Heap::kTheHoleValueRootIndex);
  __ beq(unmapped_case);

  // Load value from context and return it. We can reuse scratch1 because
  // we do not jump to the unmapped lookup (which requires the parameter
  // map in scratch1).
  __ LoadP(scratch1, FieldMemOperand(scratch1, FixedArray::kHeaderSize));
  __ SmiToPtrArrayOffset(scratch3, scratch2);
  __ AddP(scratch3, Operand(Context::kHeaderSize - kHeapObjectTag));
  __ AddP(scratch1, scratch3);
  return MemOperand(scratch1);
}


static MemOperand GenerateUnmappedArgumentsLookup(MacroAssembler* masm,
                                                  Register key,
                                                  Register parameter_map,
                                                  Register scratch,
                                                  Label* slow_case) {
  // Element is in arguments backing store, which is referenced by the
  // second element of the parameter_map. The parameter_map register
  // must be loaded with the parameter map of the arguments object and is
  // overwritten.
  const int kBackingStoreOffset = FixedArray::kHeaderSize + kPointerSize;
  Register backing_store = parameter_map;
  __ LoadP(backing_store, FieldMemOperand(parameter_map, kBackingStoreOffset));
  Handle<Map> fixed_array_map(masm->isolate()->heap()->fixed_array_map());
  __ CheckMap(backing_store, scratch, fixed_array_map, slow_case,
              DONT_DO_SMI_CHECK);
  __ LoadP(scratch, FieldMemOperand(backing_store, FixedArray::kLengthOffset));
  __ Cmpl(key, scratch);
  __ bge(slow_case);
  __ SmiToPtrArrayOffset(scratch, key);
  __ AddP(scratch, Operand(FixedArray::kHeaderSize - kHeapObjectTag));
  __ AddP(backing_store, scratch);
  return MemOperand(backing_store);
}


void KeyedLoadIC::GenerateNonStrictArguments(MacroAssembler* masm) {
  // ---------- S t a t e --------------
  //  -- lr     : return address
  //  -- r2     : key
  //  -- r3     : receiver
  // -----------------------------------
  Label slow, notin;
  MemOperand mapped_location =
      GenerateMappedArgumentsLookup(masm, r3, r2, r4, r5, r6, &notin,
                                    &slow);
  __ LoadP(r2, mapped_location);
  __ Ret();
  __ bind(&notin);
  // The unmapped lookup expects that the parameter map is in r4.
  MemOperand unmapped_location =
      GenerateUnmappedArgumentsLookup(masm, r2, r4, r5, &slow);
  __ LoadP(r4, unmapped_location);
  __ CompareRoot(r4, Heap::kTheHoleValueRootIndex);
  __ beq(&slow);
  __ LoadRR(r2, r4);
  __ Ret();
  __ bind(&slow);
  GenerateMiss(masm, false);
}


void KeyedStoreIC::GenerateNonStrictArguments(MacroAssembler* masm) {
  // ---------- S t a t e --------------
  //  -- r2     : value
  //  -- r3     : key
  //  -- r4     : receiver
  //  -- lr     : return address
  // -----------------------------------
  Label slow, notin;
  MemOperand mapped_location =
      GenerateMappedArgumentsLookup(masm, r4, r3, r5, r6, r7, &notin,
                                    &slow);
  __ StoreP(r2, mapped_location);
  __ LoadRR(r8, r5);  // r5 is modified by GenerateMappedArgumentsLookup
  __ LoadRR(r1, r2);
  __ RecordWrite(r5, r8, r1, kLRHasNotBeenSaved, kDontSaveFPRegs);
  __ Ret();
  __ bind(&notin);
  // The unmapped lookup expects that the parameter map is in r5.
  MemOperand unmapped_location =
      GenerateUnmappedArgumentsLookup(masm, r3, r5, r6, &slow);
  __ StoreP(r2, unmapped_location);
  __ LoadRR(r8, r5);  // r5 is modified by GenerateUnmappedArgumentsLookup
  __ LoadRR(r1, r2);
  __ RecordWrite(r5, r8, r1, kLRHasNotBeenSaved, kDontSaveFPRegs);
  __ Ret();
  __ bind(&slow);
  GenerateMiss(masm, false);
}


void KeyedCallIC::GenerateNonStrictArguments(MacroAssembler* masm,
                                             int argc) {
  // ----------- S t a t e -------------
  //  -- r4    : name
  //  -- lr    : return address
  // -----------------------------------
  Label slow, notin;
  // Load receiver.
  __ LoadP(r3, MemOperand(sp, argc * kPointerSize), r0);
  MemOperand mapped_location =
      GenerateMappedArgumentsLookup(masm, r3, r4, r5, r6, r7, &notin,
                                    &slow);
  __ LoadP(r3, mapped_location);
  GenerateFunctionTailCall(masm, argc, &slow, r5);
  __ bind(&notin);
  // The unmapped lookup expects that the parameter map is in r5.
  MemOperand unmapped_location =
      GenerateUnmappedArgumentsLookup(masm, r4, r5, r6, &slow);
  __ LoadP(r3, unmapped_location);
  __ CompareRoot(r3, Heap::kTheHoleValueRootIndex);
  __ beq(&slow);
  GenerateFunctionTailCall(masm, argc, &slow, r2);
  __ bind(&slow);
  GenerateMiss(masm, argc);
}


Object* KeyedLoadIC_Miss(Arguments args);


void KeyedLoadIC::GenerateMiss(MacroAssembler* masm, bool force_generic) {
  // ---------- S t a t e --------------
  //  -- lr     : return address
  //  -- r2     : key
  //  -- r3     : receiver
  // -----------------------------------
  Isolate* isolate = masm->isolate();

  __ IncrementCounter(isolate->counters()->keyed_load_miss(), 1, r5, r6);

  __ Push(r3, r2);

  // Perform tail call to the entry.
  ExternalReference ref = force_generic
      ? ExternalReference(IC_Utility(kKeyedLoadIC_MissForceGeneric), isolate)
      : ExternalReference(IC_Utility(kKeyedLoadIC_Miss), isolate);

  __ TailCallExternalReference(ref, 2, 1);
}


void KeyedLoadIC::GenerateRuntimeGetProperty(MacroAssembler* masm) {
  // ---------- S t a t e --------------
  //  -- lr     : return address
  //  -- r2     : key
  //  -- r3     : receiver
  // -----------------------------------

  __ Push(r3, r2);

  __ TailCallRuntime(Runtime::kKeyedGetProperty, 2, 1);
}


void KeyedLoadIC::GenerateGeneric(MacroAssembler* masm) {
  // ---------- S t a t e --------------
  //  -- lr     : return address
  //  -- r2     : key
  //  -- r3     : receiver
  // -----------------------------------
  Label slow, check_string, index_smi, index_string, property_array_property;
  Label probe_dictionary, check_number_dictionary;

  Register key = r2;
  Register receiver = r3;

  Isolate* isolate = masm->isolate();

  // Check that the key is a smi.
  __ JumpIfNotSmi(key, &check_string);
  __ bind(&index_smi);
  // Now the key is known to be a smi. This place is also jumped to from below
  // where a numeric string is converted to a smi.

  GenerateKeyedLoadReceiverCheck(
      masm, receiver, r4, r5, Map::kHasIndexedInterceptor, &slow);

  // Check the receiver's map to see if it has fast elements.
  __ CheckFastElements(r4, r5, &check_number_dictionary);

  GenerateFastArrayLoad(
      masm, receiver, key, r6, r5, r4, r2, NULL, &slow);
  __ IncrementCounter(isolate->counters()->keyed_load_generic_smi(), 1, r4,
                      r5);
  __ Ret();

  __ bind(&check_number_dictionary);
  __ LoadP(r6, FieldMemOperand(receiver, JSObject::kElementsOffset));
  __ LoadP(r5, FieldMemOperand(r6, JSObject::kMapOffset));

  // Check whether the elements is a number dictionary.
  // r2: key
  // r5: elements map
  // r6: elements
  __ CompareRoot(r5, Heap::kHashTableMapRootIndex);
  __ bne(&slow);
  __ SmiUntag(r4, r2);
  __ LoadFromNumberDictionary(&slow, r6, r2, r2, r4, r5, r7);
  __ Ret();

  // Slow case, key and receiver still in r2 and r3.
  __ bind(&slow);
  __ IncrementCounter(isolate->counters()->keyed_load_generic_slow(),
                      1, r4, r5);
  GenerateRuntimeGetProperty(masm);

  __ bind(&check_string);
  GenerateKeyStringCheck(masm, key, r4, r5, &index_string, &slow);

  GenerateKeyedLoadReceiverCheck(
      masm, receiver, r4, r5, Map::kHasNamedInterceptor, &slow);

  // If the receiver is a fast-case object, check the keyed lookup
  // cache. Otherwise probe the dictionary.
  __ LoadP(r5, FieldMemOperand(r3, JSObject::kPropertiesOffset));
  __ LoadP(r6, FieldMemOperand(r5, HeapObject::kMapOffset));
  __ CompareRoot(r6, Heap::kHashTableMapRootIndex);
  __ beq(&probe_dictionary);

  // Load the map of the receiver, compute the keyed lookup cache hash
  // based on 32 bits of the map pointer and the string hash.
  __ LoadP(r4, FieldMemOperand(r3, HeapObject::kMapOffset));
  __ LoadRR(r5, r4);
  __ sra(r5, Operand(KeyedLookupCache::kMapHashShift));
  __ LoadlW(r6, FieldMemOperand(r2, String::kHashFieldOffset));
  __ sra(r6, Operand(String::kHashShift));
  __ XorP(r5, r6);
  int mask = KeyedLookupCache::kCapacityMask & KeyedLookupCache::kHashMask;
  __ mov(r6, Operand(mask));
  __ AndP(r5, r6);

  // Load the key (consisting of map and symbol) from the cache and
  // check for match.
  Label load_in_object_property;
  static const int kEntriesPerBucket = KeyedLookupCache::kEntriesPerBucket;
  Label hit_on_nth_entry[kEntriesPerBucket];
  ExternalReference cache_keys =
      ExternalReference::keyed_lookup_cache_keys(isolate);

  __ mov(r6, Operand(cache_keys));
  __ LoadRR(r0, r4);
  __ ShiftLeftP(r4, r5, Operand(kPointerSizeLog2 + 1));
  __ AddP(r6, r4);
  __ LoadRR(r4, r0);

  for (int i = 0; i < kEntriesPerBucket - 1; i++) {
    Label try_next_entry;
    // Load map and move r6 to next entry.
    __ LoadP(r7, MemOperand(r6));
    __ AddP(r6, Operand(kPointerSize * 2));
    __ CmpRR(r4, r7);
    __ bne(&try_next_entry);
    __ LoadP(r7, MemOperand(r6, -kPointerSize));  // Load symbol
    __ CmpRR(r2, r7);
    __ beq(&hit_on_nth_entry[i]);
    __ bind(&try_next_entry);
  }

  // Last entry: Load map and move r6 to symbol.
  __ LoadP(r7, MemOperand(r6));
  __ AddP(r6, Operand(kPointerSize));
  __ CmpRR(r4, r7);
  __ bne(&slow);
  __ LoadP(r7, MemOperand(r6));
  __ CmpRR(r2, r7);
  __ bne(&slow);

  // Get field offset.
  // r2     : key
  // r3     : receiver
  // r4     : receiver's map
  // r5     : lookup cache index
  ExternalReference cache_field_offsets =
      ExternalReference::keyed_lookup_cache_field_offsets(isolate);

  // Hit on nth entry.
  for (int i = kEntriesPerBucket - 1; i >= 0; i--) {
    __ bind(&hit_on_nth_entry[i]);
    __ mov(r6, Operand(cache_field_offsets));
    if (i != 0) {
      __ AddP(r5, Operand(i));
    }
    __ ShiftLeftP(r7, r5, Operand(2));
    __ LoadlW(r7, MemOperand(r7, r6));
    __ LoadlB(r8, FieldMemOperand(r4, Map::kInObjectPropertiesOffset));
    __ Sub(r7, r7, r8);
    __ Cmpi(r7, Operand::Zero());
    __ bge(&property_array_property);
    if (i != 0) {
      __ b(&load_in_object_property);
    }
  }

  // Load in-object property.
  __ bind(&load_in_object_property);
  __ LoadlB(r8, FieldMemOperand(r4, Map::kInstanceSizeOffset));
  __ AddP(r8, r7);  // Index from start of object.
  __ Sub(r3, Operand(kHeapObjectTag));  // Remove the heap tag.
  __ ShiftLeftP(r2, r8, Operand(kPointerSizeLog2));
  __ LoadP(r2, MemOperand(r2, r3));
  __ IncrementCounter(isolate->counters()->keyed_load_generic_lookup_cache(),
                      1, r4, r5);
  __ Ret();

  // Load property array property.
  __ bind(&property_array_property);
  __ LoadP(r3, FieldMemOperand(r3, JSObject::kPropertiesOffset));
  __ AddP(r3, Operand(FixedArray::kHeaderSize - kHeapObjectTag));
  __ ShiftLeftP(r2, r7, Operand(kPointerSizeLog2));
  __ LoadP(r2, MemOperand(r2, r3));
  __ IncrementCounter(isolate->counters()->keyed_load_generic_lookup_cache(),
                      1, r4, r5);
  __ Ret();

  // Do a quick inline probe of the receiver's dictionary, if it
  // exists.
  __ bind(&probe_dictionary);
  // r3: receiver
  // r2: key
  // r5: elements
  __ LoadP(r4, FieldMemOperand(r3, HeapObject::kMapOffset));
  __ LoadlB(r4, FieldMemOperand(r4, Map::kInstanceTypeOffset));
  GenerateGlobalInstanceTypeCheck(masm, r4, &slow);
  // Load the property to r2.
  GenerateDictionaryLoad(masm, &slow, r5, r2, r2, r4, r6);
  __ IncrementCounter(isolate->counters()->keyed_load_generic_symbol(),
                      1, r4, r5);
  __ Ret();

  __ bind(&index_string);
  __ IndexFromHash(r5, key);
  // Now jump to the place where smi keys are handled.
  __ b(&index_smi);
}


void KeyedLoadIC::GenerateString(MacroAssembler* masm) {
  // ---------- S t a t e --------------
  //  -- lr     : return address
  //  -- r2     : key (index)
  //  -- r3     : receiver
  // -----------------------------------
  Label miss;

  Register receiver = r3;
  Register index = r2;
  Register scratch = r5;
  Register result = r2;

  StringCharAtGenerator char_at_generator(receiver,
                                          index,
                                          scratch,
                                          result,
                                          &miss,  // When not a string.
                                          &miss,  // When not a number.
                                          &miss,  // When index out of range.
                                          STRING_INDEX_IS_ARRAY_INDEX);
  char_at_generator.GenerateFast(masm);
  __ Ret();

  StubRuntimeCallHelper call_helper;
  char_at_generator.GenerateSlow(masm, call_helper);

  __ bind(&miss);
  GenerateMiss(masm, false);
}


void KeyedLoadIC::GenerateIndexedInterceptor(MacroAssembler* masm) {
  // ---------- S t a t e --------------
  //  -- lr     : return address
  //  -- r2     : key
  //  -- r3     : receiver
  // -----------------------------------
  Label slow;

  // Check that the receiver isn't a smi.
  __ JumpIfSmi(r3, &slow);

  // Check that the key is an array index, that is Uint32.
  __ TestIfPositiveSmi(r2, r0);
  __ bne(&slow /*, cr0*/);

  // Get the map of the receiver.
  __ LoadP(r4, FieldMemOperand(r3, HeapObject::kMapOffset));

  // Check that it has indexed interceptor and access checks
  // are not enabled for this object.
  __ LoadlB(r5, FieldMemOperand(r4, Map::kBitFieldOffset));
  __ AndPI(r5, Operand(kSlowCaseBitFieldMask));
  __ Cmpi(r5, Operand(1 << Map::kHasIndexedInterceptor));
  __ bne(&slow);

  // Everything is fine, call runtime.
  __ Push(r3, r2);  // Receiver, key.

  // Perform tail call to the entry.
  __ TailCallExternalReference(
      ExternalReference(IC_Utility(kKeyedLoadPropertyWithInterceptor),
                        masm->isolate()),
      2,
      1);

  __ bind(&slow);
  GenerateMiss(masm, false);
}


void KeyedStoreIC::GenerateMiss(MacroAssembler* masm, bool force_generic) {
  // ---------- S t a t e --------------
  //  -- r2     : value
  //  -- r3     : key
  //  -- r4     : receiver
  //  -- lr     : return address
  // -----------------------------------

  // Push receiver, key and value for runtime call.
  __ Push(r4, r3, r2);

  ExternalReference ref = force_generic
      ? ExternalReference(IC_Utility(kKeyedStoreIC_MissForceGeneric),
                          masm->isolate())
      : ExternalReference(IC_Utility(kKeyedStoreIC_Miss), masm->isolate());
  __ TailCallExternalReference(ref, 3, 1);
}


void KeyedStoreIC::GenerateSlow(MacroAssembler* masm) {
  // ---------- S t a t e --------------
  //  -- r2     : value
  //  -- r3     : key
  //  -- r4     : receiver
  //  -- lr     : return address
  // -----------------------------------

  // Push receiver, key and value for runtime call.
  __ Push(r4, r3, r2);

  // The slow case calls into the runtime to complete the store without causing
  // an IC miss that would otherwise cause a transition to the generic stub.
  ExternalReference ref =
      ExternalReference(IC_Utility(kKeyedStoreIC_Slow), masm->isolate());
  __ TailCallExternalReference(ref, 3, 1);
}


void KeyedStoreIC::GenerateTransitionElementsSmiToDouble(MacroAssembler* masm) {
  // ---------- S t a t e --------------
  //  -- r4     : receiver
  //  -- r5     : target map
  //  -- lr     : return address
  // -----------------------------------
  // Must return the modified receiver in r0.
  if (!FLAG_trace_elements_transitions) {
    Label fail;
    ElementsTransitionGenerator::GenerateSmiToDouble(masm, &fail);
    __ LoadRR(r2, r4);
    __ Ret();
    __ bind(&fail);
  }

  __ push(r4);
  __ TailCallRuntime(Runtime::kTransitionElementsSmiToDouble, 1, 1);
}


void KeyedStoreIC::GenerateTransitionElementsDoubleToObject(
    MacroAssembler* masm) {
  // ---------- S t a t e --------------
  //  -- r4     : receiver
  //  -- r5     : target map
  //  -- lr     : return address
  // -----------------------------------
  // Must return the modified receiver in r2.
  if (!FLAG_trace_elements_transitions) {
    Label fail;
    ElementsTransitionGenerator::GenerateDoubleToObject(masm, &fail);
    __ LoadRR(r2, r4);
    __ Ret();
    __ bind(&fail);
  }

  __ push(r4);
  __ TailCallRuntime(Runtime::kTransitionElementsDoubleToObject, 1, 1);
}


void KeyedStoreIC::GenerateRuntimeSetProperty(MacroAssembler* masm,
                                              StrictModeFlag strict_mode) {
  // ---------- S t a t e --------------
  //  -- r2     : value
  //  -- r3     : key
  //  -- r4     : receiver
  //  -- lr     : return address
  // -----------------------------------

  // Push receiver, key and value for runtime call.
  __ Push(r4, r3, r2);

  __ LoadSmiLiteral(r3, Smi::FromInt(NONE));          // PropertyAttributes
  __ LoadSmiLiteral(r2, Smi::FromInt(strict_mode));   // Strict mode.
  __ Push(r3, r2);

  __ TailCallRuntime(Runtime::kSetProperty, 5, 1);
}


static void KeyedStoreGenerateGenericHelper(
    MacroAssembler* masm,
    Label* fast_object,
    Label* fast_double,
    Label* slow,
    KeyedStoreCheckMap check_map,
    KeyedStoreIncrementLength increment_length,
    Register value,
    Register key,
    Register receiver,
    Register receiver_map,
    Register elements_map,
    Register elements) {
  Label transition_smi_elements;
  Label finish_object_store, non_double_value, transition_double_elements;
  Label fast_double_without_map_check;

  // Fast case: Do the store, could be either Object or double.
  __ bind(fast_object);
  Register scratch_value = r6;
  Register address = r7;
  if (check_map == kCheckMap) {
    __ LoadP(elements_map, FieldMemOperand(elements, HeapObject::kMapOffset));
    __ mov(scratch_value,
            Operand(masm->isolate()->factory()->fixed_array_map()));
    __ CmpRR(elements_map, scratch_value);
    __ bne(fast_double);
  }
  // Smi stores don't require further checks.
  Label non_smi_value;
  __ JumpIfNotSmi(value, &non_smi_value);

  if (increment_length == kIncrementLength) {
    // Add 1 to receiver->length.
    __ AddSmiLiteral(scratch_value, key, Smi::FromInt(1), r0);
    __ StoreP(scratch_value, FieldMemOperand(receiver, JSArray::kLengthOffset));
  }
  // It's irrelevant whether array is smi-only or not when writing a smi.
  __ AddP(address, elements, Operand(FixedArray::kHeaderSize - kHeapObjectTag));
  __ SmiToPtrArrayOffset(scratch_value, key);
  __ StorePX(value, MemOperand(address, scratch_value));
  __ Ret();

  __ bind(&non_smi_value);
  // Escape to elements kind transition case.
  __ CheckFastObjectElements(receiver_map, scratch_value,
                             &transition_smi_elements);

  // Fast elements array, store the value to the elements backing store.
  __ bind(&finish_object_store);
  if (increment_length == kIncrementLength) {
    // Add 1 to receiver->length.
    __ AddSmiLiteral(scratch_value, key, Smi::FromInt(1), r0);
    __ StoreP(scratch_value, FieldMemOperand(receiver, JSArray::kLengthOffset));
  }
  __ AddP(address, elements, Operand(FixedArray::kHeaderSize - kHeapObjectTag));
  __ SmiToPtrArrayOffset(scratch_value, key);
  __ StoreP(value, MemOperand(address, scratch_value));
  __ la(address, MemOperand(address, scratch_value));
  // Update write barrier for the elements array address.
  __ LoadRR(scratch_value, value);  // Preserve the value which is returned.
  __ RecordWrite(elements,
                 address,
                 scratch_value,
                 kLRHasNotBeenSaved,
                 kDontSaveFPRegs,
                 EMIT_REMEMBERED_SET,
                 OMIT_SMI_CHECK);
  __ Ret();

  __ bind(fast_double);
  if (check_map == kCheckMap) {
    // Check for fast double array case. If this fails, call through to the
    // runtime.
    __ CompareRoot(elements_map, Heap::kFixedDoubleArrayMapRootIndex);
    __ bne(slow);
  }
  __ bind(&fast_double_without_map_check);
  __ StoreNumberToDoubleElements(value,
                                 key,
                                 receiver,
                                 elements,  // Overwritten.
                                 r5,        // Scratch regs...
                                 r6,
                                 r7,
                                 r8,
                                 &transition_double_elements);
  if (increment_length == kIncrementLength) {
    // Add 1 to receiver->length.
    __ AddSmiLiteral(scratch_value, key, Smi::FromInt(1), r0);
    __ StoreP(scratch_value, FieldMemOperand(receiver, JSArray::kLengthOffset));
  }
  __ Ret();

  __ bind(&transition_smi_elements);
  // Transition the array appropriately depending on the value type.
  __ LoadP(r6, FieldMemOperand(value, HeapObject::kMapOffset));
  __ CompareRoot(r6, Heap::kHeapNumberMapRootIndex);
  __ bne(&non_double_value);

  // Value is a double. Transition FAST_SMI_ELEMENTS ->
  // FAST_DOUBLE_ELEMENTS and complete the store.
  __ LoadTransitionedArrayMapConditional(FAST_SMI_ELEMENTS,
                                         FAST_DOUBLE_ELEMENTS,
                                         receiver_map,
                                         r6,
                                         slow);
  ASSERT(receiver_map.is(r5));  // Transition code expects map in r5
  ElementsTransitionGenerator::GenerateSmiToDouble(masm, slow);
  __ LoadP(elements, FieldMemOperand(receiver, JSObject::kElementsOffset));
  __ b(&fast_double_without_map_check);

  __ bind(&non_double_value);
  // Value is not a double, FAST_SMI_ELEMENTS -> FAST_ELEMENTS
  __ LoadTransitionedArrayMapConditional(FAST_SMI_ELEMENTS,
                                         FAST_ELEMENTS,
                                         receiver_map,
                                         r6,
                                         slow);
  ASSERT(receiver_map.is(r5));  // Transition code expects map in r5
  ElementsTransitionGenerator::GenerateMapChangeElementsTransition(masm);
  __ LoadP(elements, FieldMemOperand(receiver, JSObject::kElementsOffset));
  __ b(&finish_object_store);

  __ bind(&transition_double_elements);
  // Elements are FAST_DOUBLE_ELEMENTS, but value is an Object that's not a
  // HeapNumber. Make sure that the receiver is a Array with FAST_ELEMENTS and
  // transition array from FAST_DOUBLE_ELEMENTS to FAST_ELEMENTS
  __ LoadTransitionedArrayMapConditional(FAST_DOUBLE_ELEMENTS,
                                         FAST_ELEMENTS,
                                         receiver_map,
                                         r6,
                                         slow);
  ASSERT(receiver_map.is(r5));  // Transition code expects map in r5
  ElementsTransitionGenerator::GenerateDoubleToObject(masm, slow);
  __ LoadP(elements, FieldMemOperand(receiver, JSObject::kElementsOffset));
  __ b(&finish_object_store);
}


void KeyedStoreIC::GenerateGeneric(MacroAssembler* masm,
                                   StrictModeFlag strict_mode) {
  // ---------- S t a t e --------------
  //  -- r2     : value
  //  -- r3     : key
  //  -- r4     : receiver
  //  -- lr     : return address
  // -----------------------------------
  Label slow, fast_object, fast_object_grow;
  Label fast_double, fast_double_grow;
  Label array, extra, check_if_double_array;

  // Register usage.
  Register value = r2;
  Register key = r3;
  Register receiver = r4;
  Register receiver_map = r5;
  Register elements_map = r8;
  Register elements = r9;  // Elements array of the receiver.
  // r6 and r7 are used as general scratch registers.

  // Check that the key is a smi.
  __ JumpIfNotSmi(key, &slow);
  // Check that the object isn't a smi.
  __ JumpIfSmi(receiver, &slow);
  // Get the map of the object.
  __ LoadP(receiver_map, FieldMemOperand(receiver, HeapObject::kMapOffset));
  // Check that the receiver does not require access checks.  We need
  // to do this because this generic stub does not perform map checks.
  __ LoadlB(ip, FieldMemOperand(receiver_map, Map::kBitFieldOffset));
  __ mov(r0, Operand(1 << Map::kIsAccessCheckNeeded));
  __ AndP(r0, ip);
  __ bne(&slow /*, cr0*/);
  // Check if the object is a JS array or not.
  __ LoadlB(r6, FieldMemOperand(receiver_map, Map::kInstanceTypeOffset));
  __ Cmpi(r6, Operand(JS_ARRAY_TYPE));
  __ beq(&array);
  // Check that the object is some kind of JSObject.
  __ Cmpi(r6, Operand(FIRST_JS_OBJECT_TYPE));
  __ blt(&slow);

  // Object case: Check key against length in the elements array.
  __ LoadP(elements, FieldMemOperand(receiver, JSObject::kElementsOffset));
  // Check array bounds. Both the key and the length of FixedArray are smis.
  __ LoadP(ip, FieldMemOperand(elements, FixedArray::kLengthOffset));
  __ Cmpl(key, ip);
  __ blt(&fast_object);

  // Slow case, handle jump to runtime.
  __ bind(&slow);
  // Entry registers are intact.
  // r2: value.
  // r3: key.
  // r4: receiver.
  GenerateRuntimeSetProperty(masm, strict_mode);

  // Extra capacity case: Check if there is extra capacity to
  // perform the store and update the length. Used for adding one
  // element to the array by writing to array[array.length].
  __ bind(&extra);
  // Condition code from comparing key and array length is still available.
  __ bne(&slow);  // Only support writing to writing to array[array.length].
  // Check for room in the elements backing store.
  // Both the key and the length of FixedArray are smis.
  __ LoadP(ip, FieldMemOperand(elements, FixedArray::kLengthOffset));
  __ Cmpl(key, ip);
  __ bge(&slow);
  __ LoadP(elements_map, FieldMemOperand(elements, HeapObject::kMapOffset));
  __ mov(ip, Operand(masm->isolate()->factory()->fixed_array_map()));
  __ CmpRR(elements_map, ip);  // PPC - I think I can re-use ip here
  __ bne(&check_if_double_array);
  __ b(&fast_object_grow);

  __ bind(&check_if_double_array);
  __ mov(ip, Operand(masm->isolate()->factory()->fixed_double_array_map()));
  __ CmpRR(elements_map, ip);  // PPC - another ip re-use
  __ bne(&slow);
  __ b(&fast_double_grow);

  // Array case: Get the length and the elements array from the JS
  // array. Check that the array is in fast mode (and writable); if it
  // is the length is always a smi.
  __ bind(&array);
  __ LoadP(elements, FieldMemOperand(receiver, JSObject::kElementsOffset));

  // Check the key against the length in the array.
  __ LoadP(ip, FieldMemOperand(receiver, JSArray::kLengthOffset));
  __ Cmpl(key, ip);
  __ bge(&extra);

  KeyedStoreGenerateGenericHelper(masm, &fast_object, &fast_double,
                                  &slow, kCheckMap, kDontIncrementLength,
                                  value, key, receiver, receiver_map,
                                  elements_map, elements);
  KeyedStoreGenerateGenericHelper(masm, &fast_object_grow, &fast_double_grow,
                                  &slow, kDontCheckMap, kIncrementLength,
                                  value, key, receiver, receiver_map,
                                  elements_map, elements);
}


void StoreIC::GenerateMegamorphic(MacroAssembler* masm,
                                  StrictModeFlag strict_mode) {
  // ----------- S t a t e -------------
  //  -- r2    : value
  //  -- r3    : receiver
  //  -- r4    : name
  //  -- lr    : return address
  // -----------------------------------

  // Get the receiver from the stack and probe the stub cache.
  Code::Flags flags =
      Code::ComputeFlags(Code::STORE_IC, MONOMORPHIC, strict_mode);

  Isolate::Current()->stub_cache()->GenerateProbe(
      masm, flags, r3, r4, r5, r6, r7, r8);

  // Cache miss: Jump to runtime.
  GenerateMiss(masm);
}


void StoreIC::GenerateMiss(MacroAssembler* masm) {
  // ----------- S t a t e -------------
  //  -- r2    : value
  //  -- r3    : receiver
  //  -- r4    : name
  //  -- lr    : return address
  // -----------------------------------

  __ Push(r3, r4, r2);

  // Perform tail call to the entry.
  ExternalReference ref =
      ExternalReference(IC_Utility(kStoreIC_Miss), masm->isolate());
  __ TailCallExternalReference(ref, 3, 1);
}


void StoreIC::GenerateArrayLength(MacroAssembler* masm) {
  // ----------- S t a t e -------------
  //  -- r2    : value
  //  -- r3    : receiver
  //  -- r4    : name
  //  -- lr    : return address
  // -----------------------------------
  //
  // This accepts as a receiver anything JSArray::SetElementsLength accepts
  // (currently anything except for external arrays which means anything with
  // elements of FixedArray type).  Value must be a number, but only smis are
  // accepted as the most common case.

  Label miss;

  Register receiver = r3;
  Register value = r2;
  Register scratch = r5;

  // Check that the receiver isn't a smi.
  __ JumpIfSmi(receiver, &miss);

  // Check that the object is a JS array.
  __ CompareObjectType(receiver, scratch, scratch, JS_ARRAY_TYPE);
  __ bne(&miss);

  // Check that elements are FixedArray.
  // We rely on StoreIC_ArrayLength below to deal with all types of
  // fast elements (including COW).
  __ LoadP(scratch, FieldMemOperand(receiver, JSArray::kElementsOffset));
  __ CompareObjectType(scratch, scratch, scratch, FIXED_ARRAY_TYPE);
  __ bne(&miss);

  // Check that the array has fast properties, otherwise the length
  // property might have been redefined.
  __ LoadP(scratch, FieldMemOperand(receiver, JSArray::kPropertiesOffset));
  __ LoadP(scratch, FieldMemOperand(scratch, FixedArray::kMapOffset));
  __ CompareRoot(scratch, Heap::kHashTableMapRootIndex);
  __ beq(&miss);

  // Check that value is a smi.
  __ JumpIfNotSmi(value, &miss);

  // Prepare tail call to StoreIC_ArrayLength.
  __ Push(receiver, value);

  ExternalReference ref =
      ExternalReference(IC_Utility(kStoreIC_ArrayLength), masm->isolate());
  __ TailCallExternalReference(ref, 2, 1);

  __ bind(&miss);

  GenerateMiss(masm);
}


void StoreIC::GenerateNormal(MacroAssembler* masm) {
  // ----------- S t a t e -------------
  //  -- r2    : value
  //  -- r3    : receiver
  //  -- r4    : name
  //  -- lr    : return address
  // -----------------------------------
  Label miss;

  GenerateStringDictionaryReceiverCheck(masm, r3, r5, r6, r7, &miss);

  GenerateDictionaryStore(masm, &miss, r5, r4, r2, r6, r7);
  Counters* counters = masm->isolate()->counters();
  __ IncrementCounter(counters->store_normal_hit(), 1, r6, r7);
  __ Ret();

  __ bind(&miss);
  __ IncrementCounter(counters->store_normal_miss(), 1, r6, r7);
  GenerateMiss(masm);
}


void StoreIC::GenerateGlobalProxy(MacroAssembler* masm,
                                  StrictModeFlag strict_mode) {
  // ----------- S t a t e -------------
  //  -- r2    : value
  //  -- r3    : receiver
  //  -- r4    : name
  //  -- lr    : return address
  // -----------------------------------

  __ Push(r3, r4, r2);

  __ LoadSmiLiteral(r3, Smi::FromInt(NONE));  // PropertyAttributes
  __ LoadSmiLiteral(r2, Smi::FromInt(strict_mode));
  __ Push(r3, r2);

  // Do tail-call to runtime routine.
  __ TailCallRuntime(Runtime::kSetProperty, 5, 1);
}


#undef __


Condition CompareIC::ComputeCondition(Token::Value op) {
  switch (op) {
    case Token::EQ_STRICT:
    case Token::EQ:
      return eq;
    case Token::LT:
      return lt;
    case Token::GT:
      return gt;
    case Token::LTE:
      return le;
    case Token::GTE:
      return ge;
    default:
      UNREACHABLE();
      return kNoCondition;
  }
}


void CompareIC::UpdateCaches(Handle<Object> x, Handle<Object> y) {
  HandleScope scope;
  Handle<Code> rewritten;
  State previous_state = GetState();
  State state = TargetState(previous_state, false, x, y);
  if (state == GENERIC) {
    CompareStub stub(GetCondition(), strict(), NO_COMPARE_FLAGS, r3, r2);
    rewritten = stub.GetCode();
  } else {
    ICCompareStub stub(op_, state);
    if (state == KNOWN_OBJECTS) {
      stub.set_known_map(Handle<Map>(Handle<JSObject>::cast(x)->map()));
    }
    rewritten = stub.GetCode();
  }
  set_target(*rewritten);

#ifdef DEBUG
  if (FLAG_trace_ic) {
    PrintF("[CompareIC (%s->%s)#%s]\n",
           GetStateName(previous_state),
           GetStateName(state),
           Token::Name(op_));
  }
#endif

  // Activate inlined smi code.
  if (previous_state == UNINITIALIZED) {
    PatchInlinedSmiCode(address(), ENABLE_INLINED_SMI_CHECK);
  }
}

//
// This code is paired with the JumpPatchSite class in full-codegen-s390.cc
//
void PatchInlinedSmiCode(Address address, InlinedSmiCheck check) {
  Address cmp_instruction_address =
      address + Assembler::kCallTargetAddressOffset;

  // If the instruction following the call is not a cmp rx, #yyy, nothing
  // was inlined.
  Instr instr = Assembler::instr_at(cmp_instruction_address);
  if (Instruction::S390OpcodeValue(cmp_instruction_address) != CHI) {
    return;
  }

  Address basrAddr = address + Assembler::kCallTargetAddressOffset - 2;
  if (Instruction::S390OpcodeValue(basrAddr) != BASR) {
    return;
  }
  // The delta to the start of the map check instruction and the
  // condition code uses at the patched jump.
  int delta = instr & 0x0000ffff;
  // int delta = Assembler::GetCmpImmediateRawImmediate(instr);
  // delta +=
      // Assembler::GetCmpImmediateRegister(instr).code() * kOff16Mask;
  // If the delta is 0 the instruction is cmp r0, #0 which also signals that
  // nothing was inlined.
  if (delta == 0) {
    return;
  }

#ifdef DEBUG
  if (FLAG_trace_ic) {
    PrintF("[  patching ic at %p, cmp=%p, delta=%d\n",
           address, cmp_instruction_address, delta);
  }
#endif

  // Expected sequence to enable by changing the following
  //   CR/CGR  Rx, Rx    // 2 / 4 bytes
  //   LR  R0, R0        // 2 bytes   // 31-bit only!
  //   BRC/BRCL          // 4 / 6 bytes
  // into
  //   TMLL    Rx, XXX   // 4 bytes
  //   BRC/BRCL          // 4 / 6 bytes
  // And vice versa to disable.

  // The following constant is the size of the CR/CGR + LR + LR
  const int kPatchAreaSizeNoBranch = 4;
  Address patch_address = cmp_instruction_address - delta;
  Address branch_address = patch_address + kPatchAreaSizeNoBranch;

  Instr instr_at_patch = Assembler::instr_at(patch_address);
  SixByteInstr  branch_instr = Assembler::instr_at(branch_address);

  // This is patching a conditional "jump if not smi/jump if smi" site.
  // Enabling by changing from
  //   cmp cr0, rx, rx
  // to
  //  rlwinm(r0, value, 0, 31, 31, SetRC);
  //  bc(label, BT/BF, 2)
  // and vice-versa to be disabled again.
  size_t patch_size = 0;
  if (Instruction::S390OpcodeValue(branch_address) == BRC) {
    patch_size = kPatchAreaSizeNoBranch + 4;
  } else if (Instruction::S390OpcodeValue(branch_address) == BRCL) {
    patch_size = kPatchAreaSizeNoBranch + 6;
  } else {
    ASSERT(false);
  }
  CodePatcher patcher(patch_address, patch_size);
  Register reg;
  reg.code_ = instr_at_patch & 0xf;
  if (check == ENABLE_INLINED_SMI_CHECK) {
    // ASSERT(Assembler::IsCmpRegister(instr_at_patch));
    // ASSERT_EQ(Assembler::GetRA(instr_at_patch).code(),
              // Assembler::GetRB(instr_at_patch).code());
    patcher.masm()->TestIfSmi(reg);
  } else {
    // Emit the Nop to make bigger place for patching
    // (replaced by lr + nill)
    ASSERT(check == DISABLE_INLINED_SMI_CHECK);
    patcher.masm()->CmpRR(reg, reg);
#ifndef V8_TARGET_ARCH_S390X
    patcher.masm()->nop();
#endif
  }

  Condition cc = al;
  if (Instruction::S390OpcodeValue(branch_address) == BRC) {
    cc = static_cast<Condition>((branch_instr & 0x00f00000) >> 20);
    ASSERT((cc == ne) || (cc == eq));
    cc = (cc == ne) ? eq : ne;
    patcher.masm()->brc(cc, Operand((branch_instr & 0xffff) << 1));
  } else if (Instruction::S390OpcodeValue(branch_address) == BRCL) {
    cc = static_cast<Condition>(
        (branch_instr & (static_cast<uint64_t>(0x00f0) << 32)) >> 36);
    ASSERT((cc == ne) || (cc == eq));
    cc = (cc == ne) ? eq : ne;
    patcher.masm()->brcl(cc, Operand((branch_instr & 0xffffffff) << 1));
  } else {
    ASSERT(false);
  }
  // Invert the logic of the branch
}


} }  // namespace v8::internal

#endif  // V8_TARGET_ARCH_S390
