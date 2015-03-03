// Copyright 2012 the V8 project authors. All rights reserved.
//
// Copyright IBM Corp. 2012-2014. All rights reserved.
//
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.

#include "src/v8.h"

#if V8_TARGET_ARCH_S390

#include "src/s390/assembler-s390.h"

#include "src/code-stubs.h"
#include "src/codegen.h"
#include "src/disasm.h"
#include "src/ic-inl.h"
#include "src/runtime.h"
#include "src/stub-cache.h"

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
  __ CmpP(type, Operand(JS_GLOBAL_OBJECT_TYPE));
  __ beq(global_object);
  __ CmpP(type, Operand(JS_BUILTINS_OBJECT_TYPE));
  __ beq(global_object);
  __ CmpP(type, Operand(JS_GLOBAL_PROXY_TYPE));
  __ beq(global_object);
}


// Helper function used from LoadIC GenerateNormal.
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
  NameDictionaryLookupStub::GeneratePositiveLookup(masm,
                                                     miss,
                                                     &done,
                                                     elements,
                                                     name,
                                                     scratch1,
                                                     scratch2);

  // If probing finds an entry check that the value is a normal
  // property.
  __ bind(&done);  // scratch2 == elements + 4 * index
  const int kElementsStartOffset = NameDictionary::kHeaderSize +
      NameDictionary::kElementsStartIndex * kPointerSize;
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
  NameDictionaryLookupStub::GeneratePositiveLookup(masm,
                                                     miss,
                                                     &done,
                                                     elements,
                                                     name,
                                                     scratch1,
                                                     scratch2);

  // If probing finds an entry in the dictionary check that the value
  // is a normal property that is not read only.
  __ bind(&done);  // scratch2 == elements + 4 * index
  const int kElementsStartOffset = NameDictionary::kHeaderSize +
      NameDictionary::kElementsStartIndex * kPointerSize;
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
  DCHECK(((1 << Map::kIsAccessCheckNeeded) | (1 << interceptor_bit)) < 0x8000);
  __ mov(r0,
          Operand((1 << Map::kIsAccessCheckNeeded) | (1 << interceptor_bit)));
  __ AndP(r0, scratch);
  __ bne(slow /*, cr0*/);
  // Check that the object is some kind of JS object EXCEPT JS Value type.
  // In the case that the object is a value-wrapper object,
  // we enter the runtime system to make sure that indexing into string
  // objects work as intended.
  DCHECK(JS_OBJECT_TYPE > JS_VALUE_TYPE);
  __ LoadlB(scratch, FieldMemOperand(map, Map::kInstanceTypeOffset));
  __ CmpP(scratch, Operand(JS_OBJECT_TYPE));
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
  __ CmpLogicalP(key, scratch1);
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


// Checks whether a key is an array index string or a unique name.
// Falls through if a key is a unique name.
static void GenerateKeyNameCheck(MacroAssembler* masm,
                                   Register key,
                                   Register map,
                                   Register hash,
                                   Label* index_string,
                                   Label* not_unique) {
  // assumes that r7 is free for scratch use
  // The key is not a smi.
  Label unique;
  // Is it a name?
  __ CompareObjectType(key, map, hash, LAST_UNIQUE_NAME_TYPE);
  __ bgt(not_unique);
  STATIC_ASSERT(LAST_UNIQUE_NAME_TYPE == FIRST_NONSTRING_TYPE);
  __ beq(&unique, Label::kNear);

  // Is the string an array index, with cached numeric value?
  __ LoadlW(hash, FieldMemOperand(key, Name::kHashFieldOffset));
  __ mov(r7, Operand(Name::kContainsCachedArrayIndexMask));
  __ LoadRR(r0, r7);
  __ AndP(r0, hash/*, SetRC*/);  // Should be OK to remove RC
  __ beq(index_string /*, cr0*/);

  // Is the string internalized? We know it's a string, so a single
  // bit test is enough.
  // map: key map
  __ LoadlB(hash, FieldMemOperand(map, Map::kInstanceTypeOffset));
  __ bind(&unique);
}


void LoadIC::GenerateMegamorphic(MacroAssembler* masm) {
  // The return address is in lr.
  Register receiver = ReceiverRegister();
  Register name = NameRegister();
  DCHECK(receiver.is(r3));
  DCHECK(name.is(r4));

  // Probe the stub cache.
  Code::Flags flags = Code::RemoveTypeAndHolderFromFlags(
      Code::ComputeHandlerFlags(Code::LOAD_IC));
  masm->isolate()->stub_cache()->GenerateProbe(
      masm, flags, receiver, name, r5, r6, r7, r8);

  // Cache miss: Jump to runtime.
  GenerateMiss(masm);
}


void LoadIC::GenerateNormal(MacroAssembler* masm) {
  Register dictionary = r2;
  DCHECK(!dictionary.is(ReceiverRegister()));
  DCHECK(!dictionary.is(NameRegister()));

  Label slow;

  __ LoadP(dictionary,
         FieldMemOperand(ReceiverRegister(), JSObject::kPropertiesOffset));
  GenerateDictionaryLoad(masm, &slow, dictionary, NameRegister(), r2, r5, r6);
  __ Ret();

  // Dictionary load failed, go slow (but don't miss).
  __ bind(&slow);
  GenerateRuntimeGetProperty(masm);
}


// A register that isn't one of the parameters to the load ic.
static const Register LoadIC_TempRegister() { return r5; }


void LoadIC::GenerateMiss(MacroAssembler* masm) {
  // The return address is in lr.
  Isolate* isolate = masm->isolate();

  __ IncrementCounter(isolate->counters()->load_miss(), 1, r5, r6);

  __ LoadRR(LoadIC_TempRegister(), ReceiverRegister());
  __ Push(LoadIC_TempRegister(), NameRegister());

  // Perform tail call to the entry.
  ExternalReference ref =
      ExternalReference(IC_Utility(kLoadIC_Miss), isolate);
  __ TailCallExternalReference(ref, 2, 1);
}


void LoadIC::GenerateRuntimeGetProperty(MacroAssembler* masm) {
  // The return address is in lr.

  __ LoadRR(LoadIC_TempRegister(), ReceiverRegister());
  __ Push(LoadIC_TempRegister(), NameRegister());

  __ TailCallRuntime(Runtime::kGetProperty, 2, 1);
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
  Handle<Map> arguments_map(heap->sloppy_arguments_elements_map());
  __ LoadP(scratch1, FieldMemOperand(object, JSObject::kElementsOffset));
  __ CheckMap(scratch1, scratch2, arguments_map, slow_case, DONT_DO_SMI_CHECK);

  // Check if element is in the range of mapped arguments. If not, jump
  // to the unmapped lookup with the parameter map in scratch1.
  __ LoadP(scratch2, FieldMemOperand(scratch1, FixedArray::kLengthOffset));
  __ SubSmiLiteral(scratch2, scratch2, Smi::FromInt(2), r0);
  __ CmpLogicalP(key, scratch2);
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
  return MemOperand(scratch1, scratch3);
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
  __ CmpLogicalP(key, scratch);
  __ bge(slow_case);
  __ SmiToPtrArrayOffset(scratch, key);
  __ AddP(scratch, Operand(FixedArray::kHeaderSize - kHeapObjectTag));
  return MemOperand(backing_store, scratch);
}


void KeyedLoadIC::GenerateSloppyArguments(MacroAssembler* masm) {
  // The return address is in lr.
  Register receiver = ReceiverRegister();
  Register key = NameRegister();
  DCHECK(receiver.is(r3));
  DCHECK(key.is(r4));

  Label slow, notin;
  MemOperand mapped_location =
      GenerateMappedArgumentsLookup(
          masm, receiver, key, r2, r5, r6, &notin, &slow);
  __ LoadP(r2, mapped_location);
  __ Ret();
  __ bind(&notin);
  // The unmapped lookup expects that the parameter map is in r2.
  MemOperand unmapped_location =
      GenerateUnmappedArgumentsLookup(masm, key, r2, r5, &slow);
  __ LoadP(r2, unmapped_location);
  __ CompareRoot(r2, Heap::kTheHoleValueRootIndex);
  __ beq(&slow, Label::kNear);
  __ Ret();
  __ bind(&slow);
  GenerateMiss(masm);
}


void KeyedStoreIC::GenerateSloppyArguments(MacroAssembler* masm) {
  Register receiver = ReceiverRegister();
  Register key = NameRegister();
  Register value = ValueRegister();
  DCHECK(receiver.is(r3));
  DCHECK(key.is(r4));
  DCHECK(value.is(r2));

  Label slow, notin;
  MemOperand mapped_location = GenerateMappedArgumentsLookup(
      masm, receiver, key, r5, r6, r7, &notin, &slow);
  Register mapped_base = mapped_location.rb();
  Register mapped_offset = mapped_location.rx();
  __ StoreP(value, mapped_location);
  __ AddP(r8, mapped_base, mapped_offset);
  __ LoadRR(r1, value);
  __ RecordWrite(r5, r8, r1, kLRHasNotBeenSaved, kDontSaveFPRegs);
  __ Ret();
  __ bind(&notin);
  // The unmapped lookup expects that the parameter map is in r5.
  MemOperand unmapped_location =
      GenerateUnmappedArgumentsLookup(masm, key, r5, r6, &slow);
  Register unmapped_base = unmapped_location.rb();
  Register unmapped_offset = unmapped_location.rx();
  __ StoreP(value, unmapped_location);
  __ AddP(r8, unmapped_base, unmapped_offset);
  __ LoadRR(r1, value);
  __ RecordWrite(r5, r8, r1, kLRHasNotBeenSaved, kDontSaveFPRegs);
  __ Ret();
  __ bind(&slow);
  GenerateMiss(masm);
}


void KeyedLoadIC::GenerateMiss(MacroAssembler* masm) {
  // The return address is in lr.
  Isolate* isolate = masm->isolate();

  __ IncrementCounter(isolate->counters()->keyed_load_miss(), 1, r5, r6);

  __ Push(ReceiverRegister(), NameRegister());

  // Perform tail call to the entry.
  ExternalReference ref =
      ExternalReference(IC_Utility(kKeyedLoadIC_Miss), isolate);

  __ TailCallExternalReference(ref, 2, 1);
}


// IC register specifications
const Register LoadIC::ReceiverRegister() { return r3; }
const Register LoadIC::NameRegister() { return r4; }


const Register LoadIC::SlotRegister() {
  DCHECK(FLAG_vector_ics);
  return r2;
}


const Register LoadIC::VectorRegister() {
  DCHECK(FLAG_vector_ics);
  return r5;
}


const Register StoreIC::ReceiverRegister() { return r3; }
const Register StoreIC::NameRegister() { return r4; }
const Register StoreIC::ValueRegister() { return r2; }


const Register KeyedStoreIC::MapRegister() {
  return r5;
}


void KeyedLoadIC::GenerateRuntimeGetProperty(MacroAssembler* masm) {
  // The return address is in lr.

  __ Push(ReceiverRegister(), NameRegister());

  __ TailCallRuntime(Runtime::kKeyedGetProperty, 2, 1);
}


void KeyedLoadIC::GenerateGeneric(MacroAssembler* masm) {
  // The return address is in lr.
  Label slow, check_name, index_smi, index_name, property_array_property;
  Label probe_dictionary, check_number_dictionary;

  Register key = NameRegister();
  Register receiver = ReceiverRegister();
  DCHECK(key.is(r4));
  DCHECK(receiver.is(r3));

  Isolate* isolate = masm->isolate();

  // Check that the key is a smi.
  __ JumpIfNotSmi(key, &check_name);
  __ bind(&index_smi);
  // Now the key is known to be a smi. This place is also jumped to from below
  // where a numeric string is converted to a smi.

  GenerateKeyedLoadReceiverCheck(
      masm, receiver, r2, r5, Map::kHasIndexedInterceptor, &slow);

  // Check the receiver's map to see if it has fast elements.
  __ CheckFastElements(r2, r5, &check_number_dictionary);

  GenerateFastArrayLoad(
      masm, receiver, key, r2, r5, r6, r2, NULL, &slow);
  __ IncrementCounter(isolate->counters()->keyed_load_generic_smi(), 1, r6,
                      r5);
  __ Ret();

  __ bind(&check_number_dictionary);
  __ LoadP(r6, FieldMemOperand(receiver, JSObject::kElementsOffset));
  __ LoadP(r5, FieldMemOperand(r6, JSObject::kMapOffset));

  // Check whether the elements is a number dictionary.
  // r5: elements map
  // r6: elements
  __ CompareRoot(r5, Heap::kHashTableMapRootIndex);
  __ bne(&slow, Label::kNear);
  __ SmiUntag(r2, key);
  __ LoadFromNumberDictionary(&slow, r6, key, r2, r2, r5, r7);
  __ Ret();

  // Slow case, key and receiver still in r2 and r3.
  __ bind(&slow);
  __ IncrementCounter(isolate->counters()->keyed_load_generic_slow(),
                      1, r6, r5);
  GenerateRuntimeGetProperty(masm);

  __ bind(&check_name);
  GenerateKeyNameCheck(masm, key, r2, r5, &index_name, &slow);

  GenerateKeyedLoadReceiverCheck(
      masm, receiver, r2, r5, Map::kHasNamedInterceptor, &slow);

  // If the receiver is a fast-case object, check the keyed lookup
  // cache. Otherwise probe the dictionary.
  __ LoadP(r5, FieldMemOperand(receiver, JSObject::kPropertiesOffset));
  __ LoadP(r6, FieldMemOperand(r5, HeapObject::kMapOffset));
  __ CompareRoot(r6, Heap::kHashTableMapRootIndex);
  __ beq(&probe_dictionary);

  // Load the map of the receiver, compute the keyed lookup cache hash
  // based on 32 bits of the map pointer and the name hash.
  __ LoadP(r2, FieldMemOperand(receiver, HeapObject::kMapOffset));
  __ LoadRR(r5, r2);
  __ sra(r5, Operand(KeyedLookupCache::kMapHashShift));
  __ LoadlW(r6, FieldMemOperand(key, Name::kHashFieldOffset));
  __ sra(r6, Operand(Name::kHashShift));
  __ XorP(r5, r6);
  int mask = KeyedLookupCache::kCapacityMask & KeyedLookupCache::kHashMask;
  __ mov(r6, Operand(mask));
  __ AndP(r5, r6);

  // Load the key (consisting of map and unique name) from the cache and
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
    __ CmpP(r2, r7);
    __ bne(&try_next_entry, Label::kNear);
    __ LoadP(r7, MemOperand(r6, -kPointerSize));  // Load name
    __ CmpP(key, r7);
    __ beq(&hit_on_nth_entry[i]);
    __ bind(&try_next_entry);
  }

  // Last entry: Load map and move r6 to name.
  __ LoadP(r7, MemOperand(r6));
  __ AddP(r6, Operand(kPointerSize));
  __ CmpP(r2, r7);
  __ bne(&slow);
  __ LoadP(r7, MemOperand(r6));
  __ CmpP(key, r7);
  __ bne(&slow);

  // Get field offset.
  // r2     : receiver's map
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
    __ LoadlB(r8, FieldMemOperand(r2, Map::kInObjectPropertiesOffset));
    __ SubP(r7, r7, r8);
    __ CmpP(r7, Operand::Zero());
    __ bge(&property_array_property);
    if (i != 0) {
      __ b(&load_in_object_property);
    }
  }

  // Load in-object property.
  __ bind(&load_in_object_property);
  __ LoadlB(r8, FieldMemOperand(r2, Map::kInstanceSizeOffset));
  __ AddP(r8, r7);  // Index from start of object.
  __ SubP(receiver, Operand(kHeapObjectTag));  // Remove the heap tag.
  __ ShiftLeftP(r2, r8, Operand(kPointerSizeLog2));
  __ LoadP(r2, MemOperand(r2, receiver));
  __ IncrementCounter(isolate->counters()->keyed_load_generic_lookup_cache(),
                      1, r6, r5);
  __ Ret();

  // Load property array property.
  __ bind(&property_array_property);
  __ LoadP(receiver, FieldMemOperand(receiver, JSObject::kPropertiesOffset));
  __ AddP(receiver, Operand(FixedArray::kHeaderSize - kHeapObjectTag));
  __ ShiftLeftP(r2, r7, Operand(kPointerSizeLog2));
  __ LoadP(r2, MemOperand(r2, receiver));
  __ IncrementCounter(isolate->counters()->keyed_load_generic_lookup_cache(),
                      1, r6, r5);
  __ Ret();

  // Do a quick inline probe of the receiver's dictionary, if it
  // exists.
  __ bind(&probe_dictionary);
  // r5: elements
  __ LoadP(r2, FieldMemOperand(receiver, HeapObject::kMapOffset));
  __ LoadlB(r2, FieldMemOperand(r2, Map::kInstanceTypeOffset));
  GenerateGlobalInstanceTypeCheck(masm, r2, &slow);
  // Load the property to r2.
  GenerateDictionaryLoad(masm, &slow, r5, key, r2, r7, r6);
  __ IncrementCounter(isolate->counters()->keyed_load_generic_symbol(),
                      1, r6, r5);
  __ Ret();

  __ bind(&index_name);
  __ IndexFromHash(r5, key);
  // Now jump to the place where smi keys are handled.
  __ b(&index_smi);
}


void KeyedLoadIC::GenerateString(MacroAssembler* masm) {
  // Return address is in lr.
  Label miss;

  Register receiver = ReceiverRegister();
  Register index = NameRegister();
  Register scratch = r5;
  Register result = r2;
  DCHECK(!scratch.is(receiver) && !scratch.is(index));

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
  GenerateMiss(masm);
}


void KeyedLoadIC::GenerateIndexedInterceptor(MacroAssembler* masm) {
  // Return address is in lr.
  Label slow;

  Register receiver = ReceiverRegister();
  Register key = NameRegister();
  Register scratch1 = r5;
  Register scratch2 = r6;
  DCHECK(!scratch1.is(receiver) && !scratch1.is(key));
  DCHECK(!scratch2.is(receiver) && !scratch2.is(key));

  // Check that the receiver isn't a smi.
  __ JumpIfSmi(receiver, &slow);

  // Check that the key is an array index, that is Uint32.
  __ TestIfPositiveSmi(key, r0);
  __ bne(&slow /*, cr0*/);

  // Get the map of the receiver.
  __ LoadP(scratch1, FieldMemOperand(receiver, HeapObject::kMapOffset));

  // Check that it has indexed interceptor and access checks
  // are not enabled for this object.
  __ LoadlB(scratch2, FieldMemOperand(scratch1, Map::kBitFieldOffset));
  __ AndP(scratch2, Operand(kSlowCaseBitFieldMask));
  __ CmpP(scratch2, Operand(1 << Map::kHasIndexedInterceptor));
  __ bne(&slow);

  // Everything is fine, call runtime.
  __ Push(receiver, key);  // Receiver, key.

  // Perform tail call to the entry.
  __ TailCallExternalReference(
      ExternalReference(IC_Utility(kLoadElementWithInterceptor),
                        masm->isolate()),
      2, 1);

  __ bind(&slow);
  GenerateMiss(masm);
}


void KeyedStoreIC::GenerateMiss(MacroAssembler* masm) {
  // Push receiver, key and value for runtime call.
  __ Push(ReceiverRegister(), NameRegister(), ValueRegister());

  ExternalReference ref =
      ExternalReference(IC_Utility(kKeyedStoreIC_Miss), masm->isolate());
  __ TailCallExternalReference(ref, 3, 1);
}


void StoreIC::GenerateSlow(MacroAssembler* masm) {
  // Push receiver, key and value for runtime call.
  __ Push(ReceiverRegister(), NameRegister(), ValueRegister());

  // The slow case calls into the runtime to complete the store without causing
  // an IC miss that would otherwise cause a transition to the generic stub.
  ExternalReference ref =
      ExternalReference(IC_Utility(kStoreIC_Slow), masm->isolate());
  __ TailCallExternalReference(ref, 3, 1);
}


void KeyedStoreIC::GenerateSlow(MacroAssembler* masm) {
  // Push receiver, key and value for runtime call.
  __ Push(ReceiverRegister(), NameRegister(), ValueRegister());

  // The slow case calls into the runtime to complete the store without causing
  // an IC miss that would otherwise cause a transition to the generic stub.
  ExternalReference ref =
      ExternalReference(IC_Utility(kKeyedStoreIC_Slow), masm->isolate());
  __ TailCallExternalReference(ref, 3, 1);
}


void KeyedStoreIC::GenerateRuntimeSetProperty(MacroAssembler* masm,
                                              StrictMode strict_mode) {
  // Push receiver, key and value for runtime call.
  __ Push(ReceiverRegister(), NameRegister(), ValueRegister());

  __ LoadSmiLiteral(r2, Smi::FromInt(strict_mode));   // Strict mode.
  __ Push(r2);

  __ TailCallRuntime(Runtime::kSetProperty, 4, 1);
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
    __ CmpP(elements_map, scratch_value);
    __ bne(fast_double);
  }

  // HOLECHECK: guards "A[i] = V"
  // We have to go to the runtime if the current value is the hole because
  // there may be a callback on the element
  Label holecheck_passed1;
  // @TODO(joransiu) : Fold AddP into memref of LoadP
  __ AddP(address, elements, Operand(FixedArray::kHeaderSize - kHeapObjectTag));
  __ SmiToPtrArrayOffset(scratch_value, key);
  __ LoadP(scratch_value, MemOperand(address, scratch_value));
  __ CmpP(scratch_value, Operand(masm->isolate()->factory()->the_hole_value()));
  __ bne(&holecheck_passed1, Label::kNear);
  __ JumpIfDictionaryInPrototypeChain(receiver, elements_map, scratch_value,
                                      slow);

  __ bind(&holecheck_passed1);

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
  __ StoreP(value, MemOperand(address, scratch_value));
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

  // HOLECHECK: guards "A[i] double hole?"
  // We have to see if the double version of the hole is present. If so
  // go to the runtime.
  // @TODO(joransiu) : Fold AddP Operand into LoadlW
  __ AddP(address, elements,
          Operand((FixedDoubleArray::kHeaderSize + Register::kExponentOffset -
                   kHeapObjectTag)));
  __ SmiToDoubleArrayOffset(scratch_value, key);
  __ LoadlW(scratch_value, MemOperand(address, scratch_value));
  __ CmpP(scratch_value, Operand(kHoleNanUpper32));
  __ bne(&fast_double_without_map_check, Label::kNear);
  __ JumpIfDictionaryInPrototypeChain(receiver, elements_map, scratch_value,
                                      slow);

  __ bind(&fast_double_without_map_check);
  __ StoreNumberToDoubleElements(value, key, elements, r5, d0,
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
  AllocationSiteMode mode = AllocationSite::GetMode(FAST_SMI_ELEMENTS,
                                                    FAST_DOUBLE_ELEMENTS);
  ElementsTransitionGenerator::GenerateSmiToDouble(
      masm, receiver, key, value, receiver_map, mode, slow);
  __ LoadP(elements, FieldMemOperand(receiver, JSObject::kElementsOffset));
  __ b(&fast_double_without_map_check);

  __ bind(&non_double_value);
  // Value is not a double, FAST_SMI_ELEMENTS -> FAST_ELEMENTS
  __ LoadTransitionedArrayMapConditional(FAST_SMI_ELEMENTS,
                                         FAST_ELEMENTS,
                                         receiver_map,
                                         r6,
                                         slow);
  mode = AllocationSite::GetMode(FAST_SMI_ELEMENTS, FAST_ELEMENTS);
  ElementsTransitionGenerator::GenerateMapChangeElementsTransition(
      masm, receiver, key, value, receiver_map, mode, slow);
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
  mode = AllocationSite::GetMode(FAST_DOUBLE_ELEMENTS, FAST_ELEMENTS);
  ElementsTransitionGenerator::GenerateDoubleToObject(
      masm, receiver, key, value, receiver_map, mode, slow);
  __ LoadP(elements, FieldMemOperand(receiver, JSObject::kElementsOffset));
  __ b(&finish_object_store);
}


void KeyedStoreIC::GenerateGeneric(MacroAssembler* masm,
                                   StrictMode strict_mode) {
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
  Register value = ValueRegister();
  Register key = NameRegister();
  Register receiver = ReceiverRegister();
  DCHECK(receiver.is(r3));
  DCHECK(key.is(r4));
  DCHECK(value.is(r2));
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
  // Check that the receiver does not require access checks and is not observed.
  // The generic stub does not perform map checks or handle observed objects.
  __ LoadlB(ip, FieldMemOperand(receiver_map, Map::kBitFieldOffset));
  __ AndP(r0, ip, Operand(1 << Map::kIsAccessCheckNeeded |
                          1 << Map::kIsObserved));
  __ bne(&slow, Label::kNear /*, cr0*/);
  // Check if the object is a JS array or not.
  __ LoadlB(r6, FieldMemOperand(receiver_map, Map::kInstanceTypeOffset));
  __ CmpP(r6, Operand(JS_ARRAY_TYPE));
  __ beq(&array);
  // Check that the object is some kind of JSObject.
  __ CmpP(r6, Operand(FIRST_JS_OBJECT_TYPE));
  __ blt(&slow, Label::kNear);

  // Object case: Check key against length in the elements array.
  __ LoadP(elements, FieldMemOperand(receiver, JSObject::kElementsOffset));
  // Check array bounds. Both the key and the length of FixedArray are smis.
  __ CmpLogicalP(key, FieldMemOperand(elements, FixedArray::kLengthOffset));
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
  __ CmpLogicalP(key, FieldMemOperand(elements, FixedArray::kLengthOffset));
  __ bge(&slow);
  __ LoadP(elements_map, FieldMemOperand(elements, HeapObject::kMapOffset));
  __ mov(ip, Operand(masm->isolate()->factory()->fixed_array_map()));
  __ CmpP(elements_map, ip);
  __ bne(&check_if_double_array, Label::kNear);
  __ b(&fast_object_grow);

  __ bind(&check_if_double_array);
  __ mov(ip, Operand(masm->isolate()->factory()->fixed_double_array_map()));
  __ CmpP(elements_map, ip);
  __ bne(&slow);
  __ b(&fast_double_grow);

  // Array case: Get the length and the elements array from the JS
  // array. Check that the array is in fast mode (and writable); if it
  // is the length is always a smi.
  __ bind(&array);
  __ LoadP(elements, FieldMemOperand(receiver, JSObject::kElementsOffset));

  // Check the key against the length in the array.
  __ CmpLogicalP(key, FieldMemOperand(receiver, JSArray::kLengthOffset));
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


void StoreIC::GenerateMegamorphic(MacroAssembler* masm) {
  Register receiver = ReceiverRegister();
  Register name = NameRegister();
  DCHECK(receiver.is(r3));
  DCHECK(name.is(r4));
  DCHECK(ValueRegister().is(r2));

  // Get the receiver from the stack and probe the stub cache.
  Code::Flags flags = Code::RemoveTypeAndHolderFromFlags(
      Code::ComputeHandlerFlags(Code::STORE_IC));

  masm->isolate()->stub_cache()->GenerateProbe(
      masm, flags, receiver, name, r5, r6, r7, r8);

  // Cache miss: Jump to runtime.
  GenerateMiss(masm);
}


void StoreIC::GenerateMiss(MacroAssembler* masm) {
  __ Push(ReceiverRegister(), NameRegister(), ValueRegister());

  // Perform tail call to the entry.
  ExternalReference ref =
      ExternalReference(IC_Utility(kStoreIC_Miss), masm->isolate());
  __ TailCallExternalReference(ref, 3, 1);
}


void StoreIC::GenerateNormal(MacroAssembler* masm) {
  Label miss;
  Register receiver = ReceiverRegister();
  Register name = NameRegister();
  Register value = ValueRegister();
  Register dictionary = r5;
  DCHECK(receiver.is(r3));
  DCHECK(name.is(r4));
  DCHECK(value.is(r2));

  __ LoadP(dictionary, FieldMemOperand(receiver, JSObject::kPropertiesOffset));

  GenerateDictionaryStore(masm, &miss, dictionary, name, value, r6, r7);
  Counters* counters = masm->isolate()->counters();
  __ IncrementCounter(counters->store_normal_hit(), 1, r6, r7);
  __ Ret();

  __ bind(&miss);
  __ IncrementCounter(counters->store_normal_miss(), 1, r6, r7);
  GenerateMiss(masm);
}


void StoreIC::GenerateRuntimeSetProperty(MacroAssembler* masm,
                                         StrictMode strict_mode) {
  __ Push(ReceiverRegister(), NameRegister(), ValueRegister());

  __ LoadSmiLiteral(r2, Smi::FromInt(strict_mode));
  __ Push(r2);

  // Do tail-call to runtime routine.
  __ TailCallRuntime(Runtime::kSetProperty, 4, 1);
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


bool CompareIC::HasInlinedSmiCode(Address address) {
  // The address of the instruction following the call.
  Address cmp_instruction_address =
      Assembler::return_address_from_call_start(address);

  // If the instruction following the call is not a CHI, nothing
  // was inlined.
  return (Instruction::S390OpcodeValue(cmp_instruction_address) == CHI);
}


//
// This code is paired with the JumpPatchSite class in full-codegen-s390.cc
//
void PatchInlinedSmiCode(Address address, InlinedSmiCheck check) {
  Address cmp_instruction_address =
      Assembler::return_address_from_call_start(address);

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

  if (FLAG_trace_ic) {
    PrintF("[  patching ic at %p, cmp=%p, delta=%d\n",
           address, cmp_instruction_address, delta);
  }

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
    DCHECK(false);
  }
  CodePatcher patcher(patch_address, patch_size);
  Register reg;
  reg.code_ = instr_at_patch & 0xf;
  if (check == ENABLE_INLINED_SMI_CHECK) {
    // DCHECK(Assembler::IsCmpRegister(instr_at_patch));
    // DCHECK_EQ(Assembler::GetRA(instr_at_patch).code(),
              // Assembler::GetRB(instr_at_patch).code());
    patcher.masm()->TestIfSmi(reg);
  } else {
    // Emit the Nop to make bigger place for patching
    // (replaced by lr + nill)
    DCHECK(check == DISABLE_INLINED_SMI_CHECK);
    patcher.masm()->CmpP(reg, reg);
#ifndef V8_TARGET_ARCH_S390X
    patcher.masm()->nop();
#endif
  }

  Condition cc = al;
  if (Instruction::S390OpcodeValue(branch_address) == BRC) {
    cc = static_cast<Condition>((branch_instr & 0x00f00000) >> 20);
    DCHECK((cc == ne) || (cc == eq));
    cc = (cc == ne) ? eq : ne;
    patcher.masm()->brc(cc, Operand((branch_instr & 0xffff) << 1));
  } else if (Instruction::S390OpcodeValue(branch_address) == BRCL) {
    cc = static_cast<Condition>(
        (branch_instr & (static_cast<uint64_t>(0x00f0) << 32)) >> 36);
    DCHECK((cc == ne) || (cc == eq));
    cc = (cc == ne) ? eq : ne;
    patcher.masm()->brcl(cc, Operand((branch_instr & 0xffffffff) << 1));
  } else {
    DCHECK(false);
  }
  // Invert the logic of the branch
}


} }  // namespace v8::internal

#endif  // V8_TARGET_ARCH_S390
