// Copyright 2012 the V8 project authors. All rights reserved.
//
// Copyright IBM Corp. 2012, 2013. All rights reserved.
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

#include "codegen.h"
#include "debug.h"
#include "deoptimizer.h"
#include "full-codegen.h"
#include "runtime.h"

namespace v8 {
namespace internal {


#define __ ACCESS_MASM(masm)

void Builtins::Generate_Adaptor(MacroAssembler* masm,
                                CFunctionId id,
                                BuiltinExtraArguments extra_args) {
  // ----------- S t a t e -------------
  //  -- r2                 : number of arguments excluding receiver
  //  -- r3                 : called function (only guaranteed when
  //                          extra_args requires it)
  //  -- cp                 : context
  //  -- sp[0]              : last argument
  //  -- ...
  //  -- sp[4 * (argc - 1)] : first argument (argc == r0)
  //  -- sp[4 * argc]       : receiver
  // -----------------------------------

  // Insert extra arguments.
  int num_extra_args = 0;
  if (extra_args == NEEDS_CALLED_FUNCTION) {
    num_extra_args = 1;
    __ push(r3);
  } else {
    ASSERT(extra_args == NO_EXTRA_ARGUMENTS);
  }

  // JumpToExternalReference expects r0 to contain the number of arguments
  // including the receiver and the extra arguments.
  __ AddP(r2, Operand(num_extra_args + 1));
  __ JumpToExternalReference(ExternalReference(id, masm->isolate()));
}


// Load the built-in InternalArray function from the current context.
static void GenerateLoadInternalArrayFunction(MacroAssembler* masm,
                                              Register result) {
  // Load the native context.

  __ LoadP(result,
           MemOperand(cp, Context::SlotOffset(Context::GLOBAL_OBJECT_INDEX)));
  __ LoadP(result,
           FieldMemOperand(result, GlobalObject::kNativeContextOffset));
  // Load the InternalArray function from the native context.
  __ LoadP(result,
           MemOperand(result,
                      Context::SlotOffset(
                        Context::INTERNAL_ARRAY_FUNCTION_INDEX)));
}


// Load the built-in Array function from the current context.
static void GenerateLoadArrayFunction(MacroAssembler* masm, Register result) {
  // Load the native context.

  __ LoadP(result,
           MemOperand(cp, Context::SlotOffset(Context::GLOBAL_OBJECT_INDEX)));
  __ LoadP(result,
           FieldMemOperand(result, GlobalObject::kNativeContextOffset));
  // Load the Array function from the native context.
  __ LoadP(result,
           MemOperand(result,
                      Context::SlotOffset(Context::ARRAY_FUNCTION_INDEX)));
}


// Allocate an empty JSArray. The allocated array is put into the result
// register. An elements backing store is allocated with size initial_capacity
// and filled with the hole values.
static void AllocateEmptyJSArray(MacroAssembler* masm,
                                 Register array_function,
                                 Register result,
                                 Register scratch1,
                                 Register scratch2,
                                 Register scratch3,
                                 Label* gc_required) {
  const int initial_capacity = JSArray::kPreallocatedArrayElements;
  STATIC_ASSERT(initial_capacity >= 0);
  __ LoadInitialArrayMap(array_function, scratch2, scratch1, false);

  // Allocate the JSArray object together with space for a fixed array with the
  // requested elements.
  int size = JSArray::kSize;
  if (initial_capacity > 0) {
    size += FixedArray::SizeFor(initial_capacity);
  }
  __ AllocateInNewSpace(size,
                        result,
                        scratch2,
                        scratch3,
                        gc_required,
                        TAG_OBJECT);

  // Future optimization: defer tagging the result pointer for more
  // efficient 64-bit memory accesses (due to alignment requirements
  // on the memoperand offset).

  // Allocated the JSArray. Now initialize the fields except for the elements
  // array.
  // result: JSObject
  // scratch1: initial map
  // scratch2: start of next object
  __ StoreP(scratch1, FieldMemOperand(result, JSObject::kMapOffset));
  __ LoadRoot(scratch1, Heap::kEmptyFixedArrayRootIndex);
  __ StoreP(scratch1, FieldMemOperand(result, JSArray::kPropertiesOffset));
  // Field JSArray::kElementsOffset is initialized later.
  __ LoadImmP(scratch3,  Operand(0, RelocInfo::NONE));
  __ StoreP(scratch3, FieldMemOperand(result, JSArray::kLengthOffset));

  if (initial_capacity == 0) {
    __ StoreP(scratch1, FieldMemOperand(result, JSArray::kElementsOffset));
    return;
  }

  // Calculate the location of the elements array and set elements array member
  // of the JSArray.
  // result: JSObject
  // scratch2: start of next object
  __ LoadRR(scratch1, result);
  __ AddP(scratch1, Operand(JSArray::kSize));
  __ StoreP(scratch1, FieldMemOperand(result, JSArray::kElementsOffset));

  // Clear the heap tag on the elements array.
  __ Sub(scratch1, Operand(kHeapObjectTag));

  // Initialize the FixedArray and fill it with holes. FixedArray length is
  // stored as a smi.
  // result: JSObject
  // scratch1: elements array (untagged)
  // scratch2: start of next object
  __ LoadRoot(scratch3, Heap::kFixedArrayMapRootIndex);
  STATIC_ASSERT(0 * kPointerSize == FixedArray::kMapOffset);
  __ StoreP(scratch3, MemOperand(scratch1));
  __ AddP(scratch1, Operand(kPointerSize));
  __ LoadSmiLiteral(scratch3,  Smi::FromInt(initial_capacity));
  STATIC_ASSERT(1 * kPointerSize == FixedArray::kLengthOffset);
  __ StoreP(scratch3, MemOperand(scratch1));
  __ AddP(scratch1, Operand(kPointerSize));

  // Fill the FixedArray with the hole value. Inline the code if short.
  STATIC_ASSERT(2 * kPointerSize == FixedArray::kHeaderSize);
  __ LoadRoot(scratch3, Heap::kTheHoleValueRootIndex);
  static const int kLoopUnfoldLimit = 4;
  if (initial_capacity <= kLoopUnfoldLimit) {
    for (int i = 0; i < initial_capacity; i++) {
      __ StoreP(scratch3, MemOperand(scratch1));
      __ AddP(scratch1, Operand(kPointerSize));
    }
  } else {
    Label loop, entry;
    __ LoadRR(scratch2, scratch1);
    __ AddP(scratch2, Operand(initial_capacity * kPointerSize));
    __ b(&entry);
    __ bind(&loop);
    __ StoreP(scratch3, MemOperand(scratch1));
    __ AddP(scratch1, Operand(kPointerSize));
    __ bind(&entry);
    __ CmpRR(scratch1, scratch2);
    __ blt(&loop);
  }
}

// Allocate a JSArray with the number of elements stored in a register. The
// register array_function holds the built-in Array function and the register
// array_size holds the size of the array as a smi. The allocated array is put
// into the result register and beginning and end of the FixedArray elements
// storage is put into registers elements_array_storage and elements_array_end
// (see  below for when that is not the case). If the parameter fill_with_holes
// is true the allocated elements backing store is filled with the hole values
// otherwise it is left uninitialized. When the backing store is filled the
// register elements_array_storage is scratched.
static void AllocateJSArray(MacroAssembler* masm,
                            Register array_function,  // Array function.
                            Register array_size,  // As a smi, cannot be 0.
                            Register result,
                            Register elements_array_storage,
                            Register elements_array_end,
                            Register scratch1,
                            Register scratch2,
                            bool fill_with_hole,
                            Label* gc_required) {
  // Load the initial map from the array function.
  __ LoadInitialArrayMap(array_function, scratch2,
                         elements_array_storage, fill_with_hole);

  if (FLAG_debug_code) {  // Assert that array size is not zero.
    __ Cmpi(array_size, Operand::Zero());
    __ Assert(ne, "array size is unexpectedly 0");
  }

  // Allocate the JSArray object together with space for a FixedArray with the
  // requested number of elements.  We omit the TAG_OBJECT flag and defer
  // tagging the pointer until the end so that we can more efficiently perform
  // aligned memory accesses.
  __ LoadImmP(elements_array_end,
         Operand((JSArray::kSize + FixedArray::kHeaderSize) / kPointerSize));
  __ SmiUntag(scratch1, array_size);
  __ AddP(elements_array_end, scratch1);
  __ AllocateInNewSpace(
      elements_array_end,
      result,
      scratch1,
      scratch2,
      gc_required,
      static_cast<AllocationFlags>(SIZE_IN_WORDS));

  // Allocated the JSArray. Now initialize the fields except for the elements
  // array.
  // result: JSObject (untagged)
  // elements_array_storage: initial map
  // array_size: size of array (smi)
  __ StoreP(elements_array_storage, MemOperand(result, JSObject::kMapOffset));
  __ LoadRoot(elements_array_storage, Heap::kEmptyFixedArrayRootIndex);
  __ StoreP(elements_array_storage,
            MemOperand(result, JSArray::kPropertiesOffset));
  // Field JSArray::kElementsOffset is initialized later.
  __ StoreP(array_size, MemOperand(result, JSArray::kLengthOffset));

  // Calculate the location of the elements array and set elements array member
  // of the JSArray.
  // result: JSObject (untagged)
  // array_size: size of array (smi)
  __ LoadRR(elements_array_storage, result);
  __ AddP(elements_array_storage, Operand(JSArray::kSize + kHeapObjectTag));
  __ StoreP(elements_array_storage,
            MemOperand(result, JSArray::kElementsOffset));

  // Clear the heap tag on the elements array.
  STATIC_ASSERT(kSmiTag == 0);
  __ Sub(elements_array_storage,
         elements_array_storage,
         Operand(kHeapObjectTag));
  // Initialize the fixed array and fill it with holes. FixedArray length is
  // stored as a smi.
  // result: JSObject (untagged)
  // elements_array_storage: elements array (untagged)
  // array_size: size of array (smi)
  __ LoadRoot(scratch1, Heap::kFixedArrayMapRootIndex);
  ASSERT_EQ(0 * kPointerSize, FixedArray::kMapOffset);
  __ StoreP(scratch1, MemOperand(elements_array_storage));
  __ AddP(elements_array_storage, Operand(kPointerSize));
  STATIC_ASSERT(kSmiTag == 0);
  ASSERT_EQ(1 * kPointerSize, FixedArray::kLengthOffset);
  __ StoreP(array_size, MemOperand(elements_array_storage));
  __ AddP(elements_array_storage, Operand(kPointerSize));

  // Calculate elements array and elements array end.
  // result: JSObject (untagged)
  // elements_array_storage: elements array element storage
  // array_size: smi-tagged size of elements array
  __ SmiToPtrArrayOffset(scratch1, array_size);
  __ LoadRR(elements_array_end, elements_array_storage);
  __ AddP(elements_array_end, scratch1);

  // Fill the allocated FixedArray with the hole value if requested.
  // result: JSObject (untagged)
  // elements_array_storage: elements array element storage
  // elements_array_end: start of next object
  if (fill_with_hole) {
    Label loop, entry;
    __ LoadRoot(scratch1, Heap::kTheHoleValueRootIndex);
    __ b(&entry);
    __ bind(&loop);
    __ StoreP(scratch1, MemOperand(elements_array_storage));
    __ AddP(elements_array_storage, Operand(kPointerSize));
    __ bind(&entry);
    __ CmpRR(elements_array_storage, elements_array_end);
    __ blt(&loop);
  }

  // Tag object
  __ AddP(result, Operand(kHeapObjectTag));
}

// Create a new array for the built-in Array function. This function allocates
// the JSArray object and the FixedArray elements array and initializes these.
// If the Array cannot be constructed in native code the runtime is called. This
// function assumes the following state:
//   r2: argc
//   r3: constructor (built-in Array function)
//   lr: return address
//   sp[0]: last argument
// This function is used for both construct and normal calls of Array. The only
// difference between handling a construct call and a normal call is that for a
// construct call the constructor function in r1_p needs to be preserved for
// entering the generic code. In both cases argc in r0 needs to be preserved.
// Both registers are preserved by this code so no need to differentiate between
// construct call and normal call.
static void ArrayNativeCode(MacroAssembler* masm,
                            Label* call_generic_code) {
  Counters* counters = masm->isolate()->counters();
  Label argc_one_or_more, argc_two_or_more, not_empty_array, empty_array,
      has_non_smi_element, finish, cant_transition_map, not_double;

  // Check for array construction with zero arguments or one.
  __ Cmpi(r2, Operand(0, RelocInfo::NONE));
  __ bne(&argc_one_or_more);

  // Handle construction of an empty array.
  __ bind(&empty_array);
  AllocateEmptyJSArray(masm,
                       r3,
                       r4,
                       r5,
                       r6,
                       r7,
                       call_generic_code);
  __ IncrementCounter(counters->array_function_native(), 1, r5, r6);
  // Set up return value, remove receiver from stack and return.
  __ LoadRR(r2, r4);
  __ la(sp, MemOperand(sp, kPointerSize));
  __ Ret();

  // Check for one argument. Bail out if argument is not smi or if it is
  // negative.
  __ bind(&argc_one_or_more);
  __ Cmpi(r2, Operand(1));
  __ bne(&argc_two_or_more);
  STATIC_ASSERT(kSmiTag == 0);
  __ LoadP(r4, MemOperand(sp));  // Get the argument from the stack.
  __ Cmpi(r4, Operand::Zero());
  __ bne(&not_empty_array);
  __ Drop(1);  // Adjust stack.
  __ LoadImmP(r2, Operand::Zero());  // Treat this as a call with argc of zero
  __ b(&empty_array);

  __ bind(&not_empty_array);
  __ TestIfPositiveSmi(r4, r5);
  __ bne(call_generic_code /*, cr0*/);

  // Handle construction of an empty array of a certain size. Bail out if size
  // is too large to actually allocate an elements array.
  STATIC_ASSERT(kSmiTag == 0);
  __ CmpSmiLiteral(r4, Smi::FromInt(JSObject::kInitialMaxFastElementArray),
                   r0);
  __ bge(call_generic_code);

  // r2: argc
  // r3: constructor
  // r4: array_size (smi)
  // sp[0]: argument
  AllocateJSArray(masm,
                  r3,
                  r4,
                  r5,
                  r6,
                  r7,
                  r8,
                  r9,
                  true,
                  call_generic_code);
  __ IncrementCounter(counters->array_function_native(), 1, r4, r6);
  // Set up return value, remove receiver and argument from stack and return.
  __ LoadRR(r2, r5);
  __ la(sp, MemOperand(sp, 2 * kPointerSize));
  __ Ret();

  // Handle construction of an array from a list of arguments.
  __ bind(&argc_two_or_more);
  // Convet argc to a smi.
  __ SmiTag(r4, r2);

  // r2: argc
  // r3: constructor
  // r4: array_size (smi)
  // sp[0]: last argument
  AllocateJSArray(masm,
                  r3,
                  r4,
                  r5,
                  r6,
                  r7,
                  r8,
                  r9,
                  false,
                  call_generic_code);
  __ IncrementCounter(counters->array_function_native(), 1, r4, r8);

  // Fill arguments as array elements. Copy from the top of the stack (last
  // element) to the array backing store filling it backwards. Note:
  // elements_array_end points after the backing store therefore PreIndex is
  // used when filling the backing store.
  // r2: argc
  // r5: JSArray
  // r6: elements_array storage start (untagged)
  // r7: elements_array_end (untagged)
  // sp[0]: last argument
  Label loop, entry;
  __ LoadRR(r9, sp);
  __ b(&entry);
  __ bind(&loop);
  __ LoadP(r4, MemOperand(r9));
  __ AddP(r9, Operand(kPointerSize));
  if (FLAG_smi_only_arrays) {
    __ JumpIfNotSmi(r4, &has_non_smi_element);
  }
  __ StoreP(r4, MemOperand(r7, -kPointerSize));
  __ lay(r7, MemOperand(r7, -kPointerSize));
  __ bind(&entry);
  __ CmpRR(r6, r7);
  __ blt(&loop);

  __ bind(&finish);
  __ LoadRR(sp, r9);

  // Remove caller arguments and receiver from the stack, setup return value and
  // return.
  // r2: argc
  // r5: JSArray
  // sp[0]: receiver
  __ la(sp, MemOperand(sp, kPointerSize));
  __ LoadRR(r2, r5);
  __ Ret();

  __ bind(&has_non_smi_element);
  // Double values are handled by the runtime.
  __ CheckMap(r4, r1,
      Heap::kHeapNumberMapRootIndex, &not_double, DONT_DO_SMI_CHECK);
  __ bind(&cant_transition_map);
  __ UndoAllocationInNewSpace(r5, r6);
  __ b(call_generic_code);

  __ bind(&not_double);
  // Transition FAST_SMI_ELEMENTS to FAST_ELEMENTS.
  // r5: JSArray
  __ LoadP(r4, FieldMemOperand(r5, HeapObject::kMapOffset));
  __ LoadTransitionedArrayMapConditional(FAST_SMI_ELEMENTS,
                                         FAST_ELEMENTS,
                                         r4,
                                         r1,
                                         &cant_transition_map);
  __ StoreP(r4, FieldMemOperand(r5, HeapObject::kMapOffset));
  __ RecordWriteField(r5,
                      HeapObject::kMapOffset,
                      r4,
                      r1,
                      kLRHasNotBeenSaved,
                      kDontSaveFPRegs,
                      EMIT_REMEMBERED_SET,
                      OMIT_SMI_CHECK);
  Label loop2;
  __ Sub(r9, Operand(kPointerSize));
  __ bind(&loop2);
  __ LoadP(r4, MemOperand(r9));
  __ AddP(r9, Operand(kPointerSize));
  __ StoreP(r4, MemOperand(r7, -kPointerSize));
  __ lay(r7, MemOperand(r7, -kPointerSize));
  __ CmpRR(r6, r7);
  __ blt(&loop2);
  __ b(&finish);
}


void Builtins::Generate_InternalArrayCode(MacroAssembler* masm) {
  // ----------- S t a t e -------------
  //  -- r2     : number of arguments
  //  -- lr     : return address
  //  -- sp[...]: constructor arguments
  // -----------------------------------
  Label generic_array_code, one_or_more_arguments, two_or_more_arguments;

  // Get the InternalArray function.
  GenerateLoadInternalArrayFunction(masm, r3);

  if (FLAG_debug_code) {
    // Initial map for the builtin InternalArray functions should be maps.
    __ LoadP(r4,
        FieldMemOperand(r3, JSFunction::kPrototypeOrInitialMapOffset));
    STATIC_ASSERT(kSmiTagMask < 0x8000);
    __ mov(r0, Operand(kSmiTagMask));
    __ AndP(r0, r4);
    __ Assert(ne, "Unexpected initial map for InternalArray function", cr0);
    __ CompareObjectType(r4, r5, r6, MAP_TYPE);
    __ Assert(eq, "Unexpected initial map for InternalArray function");
  }

  // Run the native code for the InternalArray function called as a normal
  // function.
  ArrayNativeCode(masm, &generic_array_code);

  // Jump to the generic array code if the specialized code cannot handle the
  // construction.
  __ bind(&generic_array_code);

  Handle<Code> array_code =
      masm->isolate()->builtins()->InternalArrayCodeGeneric();
  __ Jump(array_code, RelocInfo::CODE_TARGET);
}


void Builtins::Generate_ArrayCode(MacroAssembler* masm) {
  // ----------- S t a t e -------------
  //  -- r2     : number of arguments
  //  -- lr     : return address
  //  -- sp[...]: constructor arguments
  // -----------------------------------
  Label generic_array_code, one_or_more_arguments, two_or_more_arguments;

  // Get the Array function.
  GenerateLoadArrayFunction(masm, r3);

  if (FLAG_debug_code) {
    // Initial map for the builtin Array functions should be maps.
    __ LoadP(r4,
        FieldMemOperand(r3, JSFunction::kPrototypeOrInitialMapOffset));
    STATIC_ASSERT(kSmiTagMask < 0x8000);
    __ mov(r0, Operand(kSmiTagMask));
    __ AndP(r0, r4);
    __ Assert(ne, "Unexpected initial map for Array function", cr0);
    __ CompareObjectType(r4, r5, r6, MAP_TYPE);
    __ Assert(eq, "Unexpected initial map for Array function");
  }

  // Run the native code for the Array function called as a normal function.
  ArrayNativeCode(masm, &generic_array_code);

  // Jump to the generic array code if the specialized code cannot handle
  // the construction.
  __ bind(&generic_array_code);

  Handle<Code> array_code =
      masm->isolate()->builtins()->ArrayCodeGeneric();
  __ Jump(array_code, RelocInfo::CODE_TARGET);
}


void Builtins::Generate_ArrayConstructCode(MacroAssembler* masm) {
  // ----------- S t a t e -------------
  //  -- r2     : number of arguments
  //  -- r3     : constructor function
  //  -- lr     : return address
  //  -- sp[...]: constructor arguments
  // -----------------------------------
  Label generic_constructor;

  if (FLAG_debug_code) {
    // The array construct code is only set for the builtin and internal
    // Array functions which always have a map.
    // Initial map for the builtin Array function should be a map.
    __ LoadP(r4,
        FieldMemOperand(r3, JSFunction::kPrototypeOrInitialMapOffset));
    __ mov(r0, Operand(kSmiTagMask));
    __ AndP(r0, r4);
    __ Assert(ne, "Unexpected initial map for Array function", cr0);
    __ CompareObjectType(r4, r5, r6, MAP_TYPE);
    __ Assert(eq, "Unexpected initial map for Array function");
  }

  // Run the native code for the Array function called as a constructor.
  ArrayNativeCode(masm, &generic_constructor);

  // Jump to the generic construct code in case the specialized code cannot
  // handle the construction.
  __ bind(&generic_constructor);
  Handle<Code> generic_construct_stub =
      masm->isolate()->builtins()->JSConstructStubGeneric();
  __ Jump(generic_construct_stub, RelocInfo::CODE_TARGET);
}


void Builtins::Generate_StringConstructCode(MacroAssembler* masm) {
  // ----------- S t a t e -------------
  //  -- r2                     : number of arguments
  //  -- r3                     : constructor function
  //  -- lr                     : return address
  //  -- sp[(argc - n - 1) * 4] : arg[n] (zero based)
  //  -- sp[argc * 4]           : receiver
  // -----------------------------------
  Counters* counters = masm->isolate()->counters();
  __ IncrementCounter(counters->string_ctor_calls(), 1, r4, r5);

  Register function = r3;
  if (FLAG_debug_code) {
    __ LoadGlobalFunction(Context::STRING_FUNCTION_INDEX, r4);
    __ CmpRR(function, r4);
    __ Assert(eq, "Unexpected String function");
  }

  // Load the first arguments in r2 and get rid of the rest.
  Label no_arguments;
  __ Cmpi(r2, Operand(0, RelocInfo::NONE));
  __ beq(&no_arguments);
  // First args = sp[(argc - 1) * 4].
  __ Sub(r2, Operand(1));
  __ ShiftLeftImm(r2, r2, Operand(kPointerSizeLog2));
  __ la(sp, MemOperand(sp,r2));
  __ LoadP(r2, MemOperand(sp));
  // sp now point to args[0], drop args[0] + receiver.
  __ Drop(2);

  Register argument = r4;
  Label not_cached, argument_is_string;
  NumberToStringStub::GenerateLookupNumberStringCache(
      masm,
      r2,        // Input.
      argument,  // Result.
      r5,        // Scratch.
      r6,        // Scratch.
      r7,        // Scratch.
      false,     // Is it a Smi?
      &not_cached);
  __ IncrementCounter(counters->string_ctor_cached_number(), 1, r5, r6);
  __ bind(&argument_is_string);

  // ----------- S t a t e -------------
  //  -- r4     : argument converted to string
  //  -- r3     : constructor function
  //  -- lr     : return address
  // -----------------------------------

  Label gc_required;
  __ AllocateInNewSpace(JSValue::kSize,
                        r2,  // Result.
                        r5,  // Scratch.
                        r6,  // Scratch.
                        &gc_required,
                        TAG_OBJECT);

  // Initialising the String Object.
  Register map = r5;
  __ LoadGlobalFunctionInitialMap(function, map, r6);
  if (FLAG_debug_code) {
    __ LoadlB(r6, FieldMemOperand(map, Map::kInstanceSizeOffset));
    __ Cmpi(r6, Operand(JSValue::kSize >> kPointerSizeLog2));
    __ Assert(eq, "Unexpected string wrapper instance size");
    __ LoadlB(r6, FieldMemOperand(map, Map::kUnusedPropertyFieldsOffset));
    __ Cmpi(r6, Operand(0, RelocInfo::NONE));
    __ Assert(eq, "Unexpected unused properties of string wrapper");
  }
  __ StoreP(map, FieldMemOperand(r2, HeapObject::kMapOffset));

  __ LoadRoot(r5, Heap::kEmptyFixedArrayRootIndex);
  __ StoreP(r5, FieldMemOperand(r2, JSObject::kPropertiesOffset));
  __ StoreP(r5, FieldMemOperand(r2, JSObject::kElementsOffset));

  __ StoreP(argument, FieldMemOperand(r2, JSValue::kValueOffset));

  // Ensure the object is fully initialized.
  STATIC_ASSERT(JSValue::kSize == 4 * kPointerSize);

  __ Ret();

  // The argument was not found in the number to string cache. Check
  // if it's a string already before calling the conversion builtin.
  Label convert_argument;
  __ bind(&not_cached);
  __ JumpIfSmi(r2, &convert_argument);

  // Is it a String?
  __ LoadP(r4, FieldMemOperand(r2, HeapObject::kMapOffset));
  __ LoadlB(r5, FieldMemOperand(r4, Map::kInstanceTypeOffset));
  STATIC_ASSERT(kNotStringTag != 0);
  __ mov(r0, Operand(kIsNotStringMask));
  __ AndP(r0, r5);
  __ bne(&convert_argument /*, cr0*/);
  __ LoadRR(argument, r2);
  __ IncrementCounter(counters->string_ctor_conversions(), 1, r5, r6);
  __ b(&argument_is_string);

  // Invoke the conversion builtin and put the result into r4.
  __ bind(&convert_argument);
  __ push(function);  // Preserve the function.
  __ IncrementCounter(counters->string_ctor_conversions(), 1, r5, r6);
  {
    FrameScope scope(masm, StackFrame::INTERNAL);
    __ push(r2);
    __ InvokeBuiltin(Builtins::TO_STRING, CALL_FUNCTION);
  }
  __ pop(function);
  __ LoadRR(argument, r2);
  __ b(&argument_is_string);

  // Load the empty string into r4, remove the receiver from the
  // stack, and jump back to the case where the argument is a string.
  __ bind(&no_arguments);
  __ LoadRoot(argument, Heap::kEmptyStringRootIndex);
  __ Drop(1);
  __ b(&argument_is_string);

  // At this point the argument is already a string. Call runtime to
  // create a string wrapper.
  __ bind(&gc_required);
  __ IncrementCounter(counters->string_ctor_gc_required(), 1, r5, r6);
  {
    FrameScope scope(masm, StackFrame::INTERNAL);
    __ push(argument);
    __ CallRuntime(Runtime::kNewStringWrapper, 1);
  }
  __ Ret();
}


static void GenerateTailCallToSharedCode(MacroAssembler* masm) {
  __ LoadP(r4, FieldMemOperand(r3, JSFunction::kSharedFunctionInfoOffset));
  __ LoadP(r4, FieldMemOperand(r4, SharedFunctionInfo::kCodeOffset));
  __ AddP(r4, Operand(Code::kHeaderSize - kHeapObjectTag));
  __ b(r4);
}


void Builtins::Generate_InRecompileQueue(MacroAssembler* masm) {
  GenerateTailCallToSharedCode(masm);
}


void Builtins::Generate_ParallelRecompile(MacroAssembler* masm) {
  {
    FrameScope scope(masm, StackFrame::INTERNAL);

    // Push a copy of the function onto the stack.
    __ push(r3);
    // Push call kind information.
    __ push(r7);

    __ push(r3);  // Function is also the parameter to the runtime call.
    __ CallRuntime(Runtime::kParallelRecompile, 1);

    // Restore call kind information.
    __ pop(r7);
    // Restore receiver.
    __ pop(r3);

    // Tear down internal frame.
  }

  GenerateTailCallToSharedCode(masm);
}


static void Generate_JSConstructStubHelper(MacroAssembler* masm,
                                           bool is_api_function,
                                           bool count_constructions) {
  // ----------- S t a t e -------------
  //  -- r2     : number of arguments
  //  -- r3     : constructor function
  //  -- lr     : return address
  //  -- sp[...]: constructor arguments
  // -----------------------------------

  // Should never count constructions for api objects.
  ASSERT(!is_api_function || !count_constructions);

  Isolate* isolate = masm->isolate();

  // Enter a construct frame.
  {
    FrameScope scope(masm, StackFrame::CONSTRUCT);

    // Preserve the two incoming parameters on the stack.
    __ SmiTag(r2);
    __ push(r2);  // Smi-tagged arguments count.
    __ push(r3);  // Constructor function.

    // Try to allocate the object without transitioning into C code. If any of
    // the preconditions is not met, the code bails out to the runtime call.
    Label rt_call, allocated;
    if (FLAG_inline_new) {
      Label undo_allocation;
#ifdef ENABLE_DEBUGGER_SUPPORT
      ExternalReference debug_step_in_fp =
          ExternalReference::debug_step_in_fp_address(isolate);
      __ mov(r4, Operand(debug_step_in_fp));
      __ LoadP(r4, MemOperand(r4));
      __ Cmpi(r4, Operand::Zero());
      __ bne(&rt_call);
#endif

      // Load the initial map and verify that it is in fact a map.
      // r3: constructor function
      __ LoadP(r4, FieldMemOperand(r3,
                                   JSFunction::kPrototypeOrInitialMapOffset));
      __ JumpIfSmi(r4, &rt_call);
      __ CompareObjectType(r4, r5, r6, MAP_TYPE);
      __ bne(&rt_call);

      // Check that the constructor is not constructing a JSFunction (see
      // comments in Runtime_NewObject in runtime.cc). In which case the
      // initial map's instance type would be JS_FUNCTION_TYPE.
      // r3: constructor function
      // r4: initial map
      __ CompareInstanceType(r4, r5, JS_FUNCTION_TYPE);
      __ beq(&rt_call);

      if (count_constructions) {
        Label allocate;
        // Decrease generous allocation count.
        __ LoadP(r5, FieldMemOperand(r3,
                                     JSFunction::kSharedFunctionInfoOffset));
        MemOperand constructor_count =
            FieldMemOperand(r5, SharedFunctionInfo::kConstructionCountOffset);
        __ LoadlB(r6, constructor_count);
        __ AddP(r6, Operand(-1));
        __ stc(r6, constructor_count);
        __ Cmpi(r6, Operand::Zero());
        __ bne(&allocate);

        __ push(r3);
        __ push(r4);

        __ push(r3);  // constructor
        // The call will replace the stub, so the countdown is only done once.
        __ CallRuntime(Runtime::kFinalizeInstanceSize, 1);

        __ pop(r4);
        __ pop(r3);

        __ bind(&allocate);
      }

      // Now allocate the JSObject on the heap.
      // r3: constructor function
      // r4: initial map
      __ LoadlB(r5, FieldMemOperand(r4, Map::kInstanceSizeOffset));
      __ AllocateInNewSpace(r5, r6, r7, r8, &rt_call, SIZE_IN_WORDS);

      // Allocated the JSObject, now initialize the fields. Map is set to
      // initial map and properties and elements are set to empty fixed array.
      // r3: constructor function
      // r4: initial map
      // r5: object size
      // r6: JSObject (not tagged)
      __ LoadRoot(r8, Heap::kEmptyFixedArrayRootIndex);
      __ LoadRR(r7, r6);
      ASSERT_EQ(0 * kPointerSize, JSObject::kMapOffset);
      __ StoreP(r4, MemOperand(r7));
      ASSERT_EQ(1 * kPointerSize, JSObject::kPropertiesOffset);
      __ StoreP(r8, MemOperand(r7, JSObject::kPropertiesOffset));
      ASSERT_EQ(2 * kPointerSize, JSObject::kElementsOffset);
      __ StoreP(r8, MemOperand(r7, JSObject::kElementsOffset));
      __ AddP(r7, Operand(3 * kPointerSize));

      // Fill all the in-object properties with the appropriate filler.
      // r3: constructor function
      // r4: initial map
      // r5: object size (in words)
      // r6: JSObject (not tagged)
      // r7: First in-object property of JSObject (not tagged)
      uint32_t byte;
      __ ShiftLeftImm(r8, r5, Operand(kPointerSizeLog2));
      __ AddP(r8, r6);  // End of object.
      ASSERT_EQ(3 * kPointerSize, JSObject::kHeaderSize);
      __ LoadRoot(r9, Heap::kUndefinedValueRootIndex);
      if (count_constructions) {
        __ LoadlW(r2, FieldMemOperand(r4, Map::kInstanceSizesOffset));
        // Fetch Map::kPreAllocatedPropertyFieldsByte field from r2
        // and multiply by kPointerSizeLog2
        STATIC_ASSERT(Map::kPreAllocatedPropertyFieldsByte < 4);
        byte = Map::kPreAllocatedPropertyFieldsByte;
#if __BYTE_ORDER == __BIG_ENDIAN
        byte = 3 - byte;
#endif
        __ ExtractBitRange(r2, r2,
                           ((byte + 1) * kBitsPerByte) - 1,
                           byte * kBitsPerByte);
        __ ShiftLeftImm(r2, r2, Operand(kPointerSizeLog2));
        __ AddP(r2, r7);
        // r2: offset of first field after pre-allocated fields
        if (FLAG_debug_code) {
          __ CmpRR(r2, r8);
          __ Assert(le, "Unexpected number of pre-allocated property fields.");
        }
        __ InitializeFieldsWithFiller(r7, r2, r9);
        // To allow for truncation.
        __ LoadRoot(r9, Heap::kOnePointerFillerMapRootIndex);
      }
      __ InitializeFieldsWithFiller(r7, r8, r9);

      // Add the object tag to make the JSObject real, so that we can continue
      // and jump into the continuation code at any time from now on. Any
      // failures need to undo the allocation, so that the heap is in a
      // consistent state and verifiable.
      __ AddP(r6, Operand(kHeapObjectTag));

      // Check if a non-empty properties array is needed. Continue with
      // allocated object if not fall through to runtime call if it is.
      // r3: constructor function
      // r6: JSObject
      // r7: start of next object (not tagged)
      __ LoadlB(r5, FieldMemOperand(r4, Map::kUnusedPropertyFieldsOffset));
      // The field instance sizes contains both pre-allocated property fields
      // and in-object properties.
      __ LoadlW(r2, FieldMemOperand(r4, Map::kInstanceSizesOffset));
      // Fetch Map::kPreAllocatedPropertyFieldsByte field from r2
      STATIC_ASSERT(Map::kPreAllocatedPropertyFieldsByte < 4);
      byte = Map::kPreAllocatedPropertyFieldsByte;
#if __BYTE_ORDER == __BIG_ENDIAN
      byte = 3 - byte;
#endif
      __ ExtractBitRange(r8, r2,
                         ((byte + 1) * kBitsPerByte) - 1,
                         byte * kBitsPerByte);
      __ AddP(r5, r8);
      STATIC_ASSERT(Map::kInObjectPropertiesByte < 4);
      byte = Map::kInObjectPropertiesByte;
#if __BYTE_ORDER == __BIG_ENDIAN
      byte = 3 - byte;
#endif
      __ ExtractBitRange(r8, r2,
                         ((byte + 1) * kBitsPerByte) - 1,
                         byte * kBitsPerByte);
      __ Sub(r5, r5, r8);  // roohack - sub order may be incorrect
      __ Cmpi(r5, Operand::Zero());

      // Done if no extra properties are to be allocated.
      __ beq(&allocated);
      __ Assert(ge, "Property allocation count failed.");

      // Scale the number of elements by pointer size and add the header for
      // FixedArrays to the start of the next object calculation from above.
      // r3: constructor
      // r5: number of elements in properties array
      // r6: JSObject
      // r7: start of next object
      __ LoadRR(r2, r5);
      __ AddP(r2, Operand(FixedArray::kHeaderSize / kPointerSize));
      __ AllocateInNewSpace(
          r2,
          r7,
          r8,
          r4,
          &undo_allocation,
          static_cast<AllocationFlags>(RESULT_CONTAINS_TOP | SIZE_IN_WORDS));

      // Initialize the FixedArray.
      // r3: constructor
      // r5: number of elements in properties array
      // r6: JSObject
      // r7: FixedArray (not tagged)
      __ LoadRoot(r8, Heap::kFixedArrayMapRootIndex);
      __ LoadRR(r4, r7);
      ASSERT_EQ(0 * kPointerSize, JSObject::kMapOffset);
      __ StoreP(r8, MemOperand(r4));
      ASSERT_EQ(1 * kPointerSize, FixedArray::kLengthOffset);
      __ SmiTag(r2, r5);
      __ StoreP(r2, MemOperand(r4, kPointerSize));
      __ AddP(r4, Operand(2 * kPointerSize));

      // Initialize the fields to undefined.
      // r3: constructor function
      // r4: First element of FixedArray (not tagged)
      // r5: number of elements in properties array
      // r6: JSObject
      // r7: FixedArray (not tagged)
      __ ShiftLeftImm(r8, r5, Operand(kPointerSizeLog2));
      __ AddP(r8, r4);  // End of object.
      ASSERT_EQ(2 * kPointerSize, FixedArray::kHeaderSize);
      { Label loop, entry;
        if (count_constructions) {
          __ LoadRoot(r9, Heap::kUndefinedValueRootIndex);
        } else if (FLAG_debug_code) {
          __ LoadRoot(r13, Heap::kUndefinedValueRootIndex);
          __ CmpRR(r9, r13);
          __ Assert(eq, "Undefined value not loaded.");
        }
        __ b(&entry);
        __ bind(&loop);
        __ StoreP(r9, MemOperand(r4));
        __ AddP(r4, Operand(kPointerSize));
        __ bind(&entry);
        __ CmpRR(r4, r8);
        __ blt(&loop);
      }

      // Store the initialized FixedArray into the properties field of
      // the JSObject
      // r3: constructor function
      // r6: JSObject
      // r7: FixedArray (not tagged)
      __ AddP(r7, Operand(kHeapObjectTag));  // Add the heap tag.
      __ StoreP(r7, FieldMemOperand(r6, JSObject::kPropertiesOffset));

      // Continue with JSObject being successfully allocated
      // r3: constructor function
      // r6: JSObject
      __ b(&allocated);

      // Undo the setting of the new top so that the heap is verifiable. For
      // example, the map's unused properties potentially do not match the
      // allocated objects unused properties.
      // r6: JSObject (previous new top)
      __ bind(&undo_allocation);
      __ UndoAllocationInNewSpace(r6, r7);
    }

    // Allocate the new receiver object using the runtime call.
    // r3: constructor function
    __ bind(&rt_call);
    __ push(r3);  // argument for Runtime_NewObject
    __ CallRuntime(Runtime::kNewObject, 1);
    __ LoadRR(r6, r2);

    // Receiver for constructor call allocated.
    // r6: JSObject
    __ bind(&allocated);
    __ push(r6);
    __ push(r6);

    // Reload the number of arguments and the constructor from the stack.
    // sp[0]: receiver
    // sp[1]: receiver
    // sp[2]: constructor function
    // sp[3]: number of arguments (smi-tagged)
    __ LoadP(r3, MemOperand(sp, 2 * kPointerSize));
    __ LoadP(r5, MemOperand(sp, 3 * kPointerSize));

    // Set up pointer to last argument.
    __ LoadRR(r4, fp);
    __ AddP(r4, Operand(StandardFrameConstants::kCallerSPOffset));

    // Set up number of arguments for function call below
    __ SmiUntag(r2, r5);

    // Copy arguments and receiver to the expression stack.
    // r2: number of arguments
    // r3: constructor function
    // r4: address of last argument (caller sp)
    // r5: number of arguments (smi-tagged)
    // sp[0]: receiver
    // sp[1]: receiver
    // sp[2]: constructor function
    // sp[3]: number of arguments (smi-tagged)
    Label loop, no_args;
    __ Cmpi(r2, Operand::Zero());
    __ beq(&no_args);
    __ ShiftLeftImm(ip, r2, Operand(kPointerSizeLog2));
    __ bind(&loop);
    __ Sub(ip, Operand(kPointerSize));
    __ LoadP(r0, MemOperand(r4, ip));
    __ push(r0);
    __ BranchOnCount(r2, &loop);
    __ SmiUntag(r2, r5);
    __ bind(&no_args);

    // Call the function.
    // r2: number of arguments
    // r3: constructor function
    if (is_api_function) {
      __ LoadP(cp, FieldMemOperand(r3, JSFunction::kContextOffset));
      Handle<Code> code =
          masm->isolate()->builtins()->HandleApiCallConstruct();
      ParameterCount expected(0);
      __ InvokeCode(code, expected, expected,
                    RelocInfo::CODE_TARGET, CALL_FUNCTION, CALL_AS_METHOD);
    } else {
      ParameterCount actual(r2);
      __ InvokeFunction(r3, actual, CALL_FUNCTION,  // roohack
                        NullCallWrapper(), CALL_AS_METHOD);
    }

    // Store offset of return address for deoptimizer.
    if (!is_api_function && !count_constructions) {
      masm->isolate()->heap()->SetConstructStubDeoptPCOffset(masm->pc_offset());
    }

    // Restore context from the frame.
    // r2: result
    // sp[0]: receiver
    // sp[1]: constructor function
    // sp[2]: number of arguments (smi-tagged)
    __ LoadP(cp, MemOperand(fp, StandardFrameConstants::kContextOffset));

    // If the result is an object (in the ECMA sense), we should get rid
    // of the receiver and use the result; see ECMA-262 section 13.2.2-7
    // on page 74.
    Label use_receiver, exit;

    // If the result is a smi, it is *not* an object in the ECMA sense.
    // r2: result
    // sp[0]: receiver (newly allocated object)
    // sp[1]: constructor function
    // sp[2]: number of arguments (smi-tagged)
    __ JumpIfSmi(r2, &use_receiver);

    // If the type of the result (stored in its map) is less than
    // FIRST_SPEC_OBJECT_TYPE, it is not an object in the ECMA sense.
    __ CompareObjectType(r2, r5, r5, FIRST_SPEC_OBJECT_TYPE);
    __ bge(&exit);

    // Throw away the result of the constructor invocation and use the
    // on-stack receiver as the result.
    __ bind(&use_receiver);
    __ LoadP(r2, MemOperand(sp));

    // Remove receiver from the stack, remove caller arguments, and
    // return.
    __ bind(&exit);
    // r2: result
    // sp[0]: receiver (newly allocated object)
    // sp[1]: constructor function
    // sp[2]: number of arguments (smi-tagged)
    __ LoadP(r3, MemOperand(sp, 2 * kPointerSize));

    // Leave construct frame.
  }

  __ SmiToPtrArrayOffset(r3, r3);
  __ la(sp, MemOperand(sp, r3, kPointerSize));
  __ IncrementCounter(isolate->counters()->constructed_objects(),
                      1, r3, r4);
  __ Ret();
}


void Builtins::Generate_JSConstructStubCountdown(MacroAssembler* masm) {
  Generate_JSConstructStubHelper(masm, false, true);
}


void Builtins::Generate_JSConstructStubGeneric(MacroAssembler* masm) {
  Generate_JSConstructStubHelper(masm, false, false);
}


void Builtins::Generate_JSConstructStubApi(MacroAssembler* masm) {
  Generate_JSConstructStubHelper(masm, true, false);
}


static void Generate_JSEntryTrampolineHelper(MacroAssembler* masm,
                                             bool is_construct) {
  // Called from Generate_JS_Entry
  // r2: code entry
  // r3: function
  // r4: receiver
  // r5: argc
  // r6: argv
  // r0,r7-r9, cp may be clobbered

  // Clear the context before we push it when entering the internal frame.
  __ LoadImmP(cp, Operand(0, RelocInfo::NONE));

  // Enter an internal frame.
  {
    // FrameScope ends up calling MacroAssembler::EnterFrame here
    FrameScope scope(masm, StackFrame::INTERNAL);

    // Set up the context from the function argument.
    __ LoadP(cp, FieldMemOperand(r3, JSFunction::kContextOffset));

    __ InitializeRootRegister();

    // Push the function and the receiver onto the stack.
    __ lay(sp, MemOperand(sp, -2 * kPointerSize));
    __ StoreP(r3, MemOperand(sp, 1 * kPointerSize));
    __ StoreP(r4, MemOperand(sp, 0 * kPointerSize));

    // Copy arguments to the stack in a loop from argv to sp.
    // The arguments are actually placed in reverse order on sp
    // compared to argv (i.e. arg1 is highest memory in sp).
    // r3: function
    // r5: argc
    // r6: argv, i.e. points to first arg
    // r7: scratch reg to hold scaled argc
    // r8: scratch reg to hold arg handle
    // r9: scratch reg to hold index into argv
    Label argLoop, argExit;
    intptr_t zero = 0;
    __ ShiftLeftImm(r7, r5, Operand(kPointerSizeLog2));
    __ SubRR(sp, r7);    // Buy the stack frame to fit args
    __ LoadImmP(r9, Operand(zero));  // Initialize argv index
    __ bind(&argLoop);
    __ CmpPH(r7, Operand(zero));
    __ beq(&argExit);
    __ lay(r7, MemOperand(r7, -kPointerSize));
    __ LoadP(r8, MemOperand(r9, r6));  // read next parameter
    __ la(r9, MemOperand(r9, kPointerSize));  // r9++;
    __ LoadP(r0, MemOperand(r8));  // dereference handle
    __ StoreP(r0, MemOperand(r7, sp));  // push parameter
    __ b(&argLoop);
    __ bind(&argExit);

    // Initialize all JavaScript callee-saved registers, since they will be seen
    // by the garbage collector as part of handlers.
    __ LoadRoot(r6, Heap::kUndefinedValueRootIndex);
    __ LoadRR(r7, r6);
    __ LoadRR(r8, r6);
    __ LoadRR(r9, r6);

    // Invoke the code and pass argc as r2.
    __ LoadRR(r2, r5);
    if (is_construct) {
      CallConstructStub stub(NO_CALL_FUNCTION_FLAGS);
      __ CallStub(&stub);
    } else {
      ParameterCount actual(r2);
      __ InvokeFunction(r3, actual, CALL_FUNCTION,
                        NullCallWrapper(), CALL_AS_METHOD);
    }
    // Exit the JS frame and remove the parameters (except function), and
    // return.
  }
  __ b(r14);

  // r2: result
}


void Builtins::Generate_JSEntryTrampoline(MacroAssembler* masm) {
  Generate_JSEntryTrampolineHelper(masm, false);
}


void Builtins::Generate_JSConstructEntryTrampoline(MacroAssembler* masm) {
  Generate_JSEntryTrampolineHelper(masm, true);
}


void Builtins::Generate_LazyCompile(MacroAssembler* masm) {
  // Enter an internal frame.
  {
    FrameScope scope(masm, StackFrame::INTERNAL);

    // Preserve the function.
    __ push(r3);
    // Push call kind information.
    __ push(r7);

    // Push the function on the stack as the argument to the runtime function.
    __ push(r3);
    __ CallRuntime(Runtime::kLazyCompile, 1);
    // Calculate the entry point.
    __ LoadRR(r4, r2);
    __ AddP(r4, Operand(Code::kHeaderSize - kHeapObjectTag));

    // Restore call kind information.
    __ pop(r7);
    // Restore saved function.
    __ pop(r3);

    // Tear down internal frame.
  }

  // Do a tail-call of the compiled function.
  __ Jump(r4);
}


void Builtins::Generate_LazyRecompile(MacroAssembler* masm) {
  // Enter an internal frame.
  {
    FrameScope scope(masm, StackFrame::INTERNAL);

    // Preserve the function.
    __ push(r3);
    // Push call kind information.
    __ push(r7);

    // Push the function on the stack as the argument to the runtime function.
    __ push(r3);
    __ CallRuntime(Runtime::kLazyRecompile, 1);
    // Calculate the entry point.
    __ LoadRR(r4, r2);
    __ AddP(r4, Operand(Code::kHeaderSize - kHeapObjectTag));

    // Restore call kind information.
    __ pop(r7);
    // Restore saved function.
    __ pop(r3);

    // Tear down internal frame.
  }

  // Do a tail-call of the compiled function.
  __ Jump(r4);
}


static void Generate_NotifyDeoptimizedHelper(MacroAssembler* masm,
                                             Deoptimizer::BailoutType type) {
  {
    FrameScope scope(masm, StackFrame::INTERNAL);
    // Pass the function and deoptimization type to the runtime system.
    __ LoadSmiLiteral(r2, Smi::FromInt(static_cast<int>(type)));
    __ push(r2);
    __ CallRuntime(Runtime::kNotifyDeoptimized, 1);
  }

  // Get the full codegen state from the stack and untag it -> r8.
  __ LoadP(r8, MemOperand(sp, 0 * kPointerSize));
  __ SmiUntag(r8);
  // Switch on the state.
  Label with_tos_register, unknown_state;
  __ Cmpi(r8, Operand(FullCodeGenerator::NO_REGISTERS));
  __ bne(&with_tos_register);
  __ la(sp, MemOperand(sp, 1 * kPointerSize));  // Remove state.
  __ Ret();

  __ bind(&with_tos_register);
  __ LoadP(r2, MemOperand(sp, 1 * kPointerSize));
  __ Cmpi(r8, Operand(FullCodeGenerator::TOS_REG));
  __ bne(&unknown_state);
  __ la(sp, MemOperand(sp, 2 * kPointerSize));  // Remove state.
  __ Ret();

  __ bind(&unknown_state);
  __ stop("no cases left");
}


void Builtins::Generate_NotifyDeoptimized(MacroAssembler* masm) {
  Generate_NotifyDeoptimizedHelper(masm, Deoptimizer::EAGER);
}


void Builtins::Generate_NotifyLazyDeoptimized(MacroAssembler* masm) {
  Generate_NotifyDeoptimizedHelper(masm, Deoptimizer::LAZY);
}


void Builtins::Generate_NotifyOSR(MacroAssembler* masm) {
  // For now, we are relying on the fact that Runtime::NotifyOSR
  // doesn't do any garbage collection which allows us to save/restore
  // the registers without worrying about which of them contain
  // pointers. This seems a bit fragile.
  __ LoadRR(r0, r14);
  RegList saved_regs =
      (kJSCallerSaved | kCalleeSaved | r0.bit() | fp.bit()) & ~sp.bit();
  __ MultiPush(saved_regs);
  {
    FrameScope scope(masm, StackFrame::INTERNAL);
    __ CallRuntime(Runtime::kNotifyOSR, 0);
  }
  __ MultiPop(saved_regs);
  __ LoadRR(r14, r0);
  __ Ret();
}


void Builtins::Generate_OnStackReplacement(MacroAssembler* masm) {
  // Lookup the function in the JavaScript frame and push it as an
  // argument to the on-stack replacement function.
  __ LoadP(r2, MemOperand(fp, JavaScriptFrameConstants::kFunctionOffset));
  {
    FrameScope scope(masm, StackFrame::INTERNAL);
    __ push(r2);
    __ CallRuntime(Runtime::kCompileForOnStackReplacement, 1);
  }

  // If the result was -1 it means that we couldn't optimize the
  // function. Just return and continue in the unoptimized version.
  Label skip;
  __ CmpSmiLiteral(r2, Smi::FromInt(-1), r0);
  __ bne(&skip);
  __ Ret();

  __ bind(&skip);
  // Untag the AST id and push it on the stack.
  __ SmiUntag(r2);
  __ push(r2);

  // Generate the code for doing the frame-to-frame translation using
  // the deoptimizer infrastructure.
  Deoptimizer::EntryGenerator generator(masm, Deoptimizer::OSR);
  generator.Generate();
}


void Builtins::Generate_FunctionCall(MacroAssembler* masm) {
  // 1. Make sure we have at least one argument.
  // r2: actual number of arguments
  { Label done;
    __ Cmpi(r2, Operand::Zero());
    __ bne(&done);
    __ LoadRoot(r4, Heap::kUndefinedValueRootIndex);
    __ push(r4);
    __ AddP(r2, Operand(1));
    __ bind(&done);
  }

  // 2. Get the function to call (passed as receiver) from the stack, check
  //    if it is a function.
  // r2: actual number of arguments
  Label slow, non_function;
  __ ShiftLeftImm(r3, r2, Operand(kPointerSizeLog2));
  __ AddP(r3, sp);
  __ LoadP(r3, MemOperand(r3));
  __ JumpIfSmi(r3, &non_function);
  __ CompareObjectType(r3, r4, r4, JS_FUNCTION_TYPE);
  __ bne(&slow);

  // 3a. Patch the first argument if necessary when calling a function.
  // r2: actual number of arguments
  // r3: function
  Label shift_arguments;
  __ LoadImmP(r6, Operand(0, RelocInfo::NONE));  // indicate regular
                                                   // JS_FUNCTION
  { Label convert_to_object, use_global_receiver, patch_receiver;
    // Change context eagerly in case we need the global receiver.
    __ LoadP(cp, FieldMemOperand(r3, JSFunction::kContextOffset));

    // Do not transform the receiver for strict mode functions.
    __ LoadP(r4,
             FieldMemOperand(r3, JSFunction::kSharedFunctionInfoOffset));
    __ LoadlW(r5,
              FieldMemOperand(r4, SharedFunctionInfo::kCompilerHintsOffset));
    __ TestBit(r5,
#if V8_TARGET_ARCH_S390X
               SharedFunctionInfo::kStrictModeFunction,
#else
               SharedFunctionInfo::kStrictModeFunction + kSmiTagSize,
#endif
               r0);
    __ bne(&shift_arguments /*, cr0*/);

    // Do not transform the receiver for native (Compilerhints already in r5).
    __ TestBit(r5,
#if V8_TARGET_ARCH_S390X
               SharedFunctionInfo::kNative,
#else
               SharedFunctionInfo::kNative + kSmiTagSize,
#endif
               r0);
    __ bne(&shift_arguments /*, cr0*/);

    // Compute the receiver in non-strict mode.
    __ ShiftLeftImm(ip, r2, Operand(kPointerSizeLog2));
    __ LoadRR(r4, sp);
    __ AddP(r4, ip);
    __ LoadP(r4, MemOperand(r4, -kPointerSize));
    // r2: actual number of arguments
    // r3: function
    // r4: first argument
    __ JumpIfSmi(r4, &convert_to_object);

    __ LoadRoot(r5, Heap::kUndefinedValueRootIndex);
    __ CmpRR(r4, r5);
    __ beq(&use_global_receiver);
    __ LoadRoot(r5, Heap::kNullValueRootIndex);
    __ CmpRR(r4, r5);
    __ beq(&use_global_receiver);

    STATIC_ASSERT(LAST_SPEC_OBJECT_TYPE == LAST_TYPE);
    __ CompareObjectType(r4, r5, r5, FIRST_SPEC_OBJECT_TYPE);
    __ bge(&shift_arguments);

    __ bind(&convert_to_object);

    {
      // Enter an internal frame in order to preserve argument count.
      FrameScope scope(masm, StackFrame::INTERNAL);
      __ SmiTag(r2);
      __ push(r2);

      __ push(r4);
      __ InvokeBuiltin(Builtins::TO_OBJECT, CALL_FUNCTION);
      __ LoadRR(r4, r2);

      __ pop(r2);
      __ SmiUntag(r2);

      // Exit the internal frame.
    }

    // Restore the function to r3, and the flag to r6.
    __ ShiftLeftImm(r6, r2, Operand(kPointerSizeLog2));
    __ AddP(r6, sp);
    __ LoadP(r3, MemOperand(r6));
    __ LoadImmP(r6, Operand(0, RelocInfo::NONE));
    __ b(&patch_receiver);

    // Use the global receiver object from the called function as the
    // receiver.
    __ bind(&use_global_receiver);
    const int kGlobalIndex =
        Context::kHeaderSize + Context::GLOBAL_OBJECT_INDEX * kPointerSize;
    __ LoadP(r4, FieldMemOperand(cp, kGlobalIndex));
    __ LoadP(r4, FieldMemOperand(r4, GlobalObject::kNativeContextOffset));
    __ LoadP(r4, FieldMemOperand(r4, kGlobalIndex));
    __ LoadP(r4, FieldMemOperand(r4, GlobalObject::kGlobalReceiverOffset));

    __ bind(&patch_receiver);
    __ ShiftLeftImm(ip, r2, Operand(kPointerSizeLog2));
    __ LoadRR(r5, sp);
    __ AddP(r5, ip);
    __ StoreP(r4, MemOperand(r5, -kPointerSize));

    __ b(&shift_arguments);
  }

  // 3b. Check for function proxy.
  __ bind(&slow);
  __ LoadImmP(r6, Operand(1, RelocInfo::NONE));  // indicate function proxy
  __ Cmpi(r4, Operand(JS_FUNCTION_PROXY_TYPE));
  __ beq(&shift_arguments);
  __ bind(&non_function);
  __ LoadImmP(r6, Operand(2, RelocInfo::NONE));  // indicate non-function

  // 3c. Patch the first argument when calling a non-function.  The
  //     CALL_NON_FUNCTION builtin expects the non-function callee as
  //     receiver, so overwrite the first argument which will ultimately
  //     become the receiver.
  // r2: actual number of arguments
  // r3: function
  // r6: call type (0: JS function, 1: function proxy, 2: non-function)
  __ ShiftLeftImm(ip, r2, Operand(kPointerSizeLog2));
  __ LoadRR(r4, sp);
  __ AddP(r4, ip);
  __ StoreP(r3, MemOperand(r4, -kPointerSize));

  // 4. Shift arguments and return address one slot down on the stack
  //    (overwriting the original receiver).  Adjust argument count to make
  //    the original first argument the new receiver.
  // r2: actual number of arguments
  // r3: function
  // r6: call type (0: JS function, 1: function proxy, 2: non-function)
  __ bind(&shift_arguments);
  { Label loop;
    // Calculate the copy start address (destination). Copy end address is sp.
    __ ShiftLeftImm(ip, r2, Operand(kPointerSizeLog2));
    __ LoadRR(r4, sp);
    __ AddP(r4, ip);

    __ bind(&loop);
    __ LoadP(ip, MemOperand(r4, -kPointerSize));
    __ StoreP(ip, MemOperand(r4));
    __ Sub(r4, Operand(kPointerSize));
    __ CmpRR(r4, sp);
    __ bne(&loop);
    // Adjust the actual number of arguments and remove the top element
    // (which is a copy of the last argument).
    __ Sub(r2, Operand(1));
    __ pop();
  }

  // 5a. Call non-function via tail call to CALL_NON_FUNCTION builtin,
  //     or a function proxy via CALL_FUNCTION_PROXY.
  // r2: actual number of arguments
  // r3: function
  // r6: call type (0: JS function, 1: function proxy, 2: non-function)
  { Label function, non_proxy;
    __ Cmpi(r6, Operand::Zero());
    __ beq(&function);
    // Expected number of arguments is 0 for CALL_NON_FUNCTION.
    __ LoadImmP(r4, Operand(0, RelocInfo::NONE));
    __ SetCallKind(r7, CALL_AS_METHOD);
    __ Cmpi(r6, Operand(1));
    __ bne(&non_proxy);

    __ push(r3);  // re-add proxy object as additional argument
    __ AddP(r2, Operand(1));
    __ GetBuiltinEntry(r5, Builtins::CALL_FUNCTION_PROXY);
    __ Jump(masm->isolate()->builtins()->ArgumentsAdaptorTrampoline(),
            RelocInfo::CODE_TARGET);

    __ bind(&non_proxy);
    __ GetBuiltinEntry(r5, Builtins::CALL_NON_FUNCTION);
    __ Jump(masm->isolate()->builtins()->ArgumentsAdaptorTrampoline(),
            RelocInfo::CODE_TARGET);
    __ bind(&function);
  }

  // 5b. Get the code to call from the function and check that the number of
  //     expected arguments matches what we're providing.  If so, jump
  //     (tail-call) to the code in register edx without checking arguments.
  // r2: actual number of arguments
  // r3: function
  __ LoadP(r5, FieldMemOperand(r3, JSFunction::kSharedFunctionInfoOffset));
  __ LoadW(r4, FieldMemOperand(r5,
           SharedFunctionInfo::kFormalParameterCountOffset));
#if !defined(V8_TARGET_ARCH_S390X)
  __ SmiUntag(r4);
#endif
  __ LoadP(r5, FieldMemOperand(r3, JSFunction::kCodeEntryOffset));
  __ SetCallKind(r7, CALL_AS_METHOD);
  __ CmpRR(r4, r2);  // Check formal and actual parameter counts.
  Label skip;
  __ beq(&skip);
  __ Jump(masm->isolate()->builtins()->ArgumentsAdaptorTrampoline(),
          RelocInfo::CODE_TARGET);

  __ bind(&skip);
  ParameterCount expected(0);
  __ InvokeCode(r5, expected, expected, JUMP_FUNCTION,
                NullCallWrapper(), CALL_AS_METHOD);
}


void Builtins::Generate_FunctionApply(MacroAssembler* masm) {
  const int kIndexOffset    = -5 * kPointerSize;
  const int kLimitOffset    = -4 * kPointerSize;
  const int kArgsOffset     =  2 * kPointerSize;
  const int kRecvOffset     =  3 * kPointerSize;
  const int kFunctionOffset =  4 * kPointerSize;

  {
    FrameScope frame_scope(masm, StackFrame::INTERNAL);

    __ LoadP(r2, MemOperand(fp, kFunctionOffset));  // get the function
    __ push(r2);
    __ LoadP(r2, MemOperand(fp, kArgsOffset));  // get the args array
    __ push(r2);
    __ InvokeBuiltin(Builtins::APPLY_PREPARE, CALL_FUNCTION);

    // Check the stack for overflow. We are not trying to catch
    // interruptions (e.g. debug break and preemption) here, so the "real stack
    // limit" is checked.
    Label okay;
    __ LoadRoot(r4, Heap::kRealStackLimitRootIndex);
    // Make r4 the space we have left. The stack might already be overflowed
    // here which will cause r4 to become negative.
    __ Sub(r4, sp, r4);
    // Check if the arguments will overflow the stack.
    __ SmiToPtrArrayOffset(r0, r2);
    __ CmpRR(r4, r0);
    __ bgt(&okay);  // Signed comparison.

    // Out of stack space.
    __ LoadP(r3, MemOperand(fp, kFunctionOffset));
    __ push(r3);
    __ push(r2);
    __ InvokeBuiltin(Builtins::APPLY_OVERFLOW, CALL_FUNCTION);
    // End of stack check.

    // Push current limit and index.
    __ bind(&okay);
    __ push(r2);  // limit
    __ LoadImmP(r3, Operand(0, RelocInfo::NONE));  // initial index
    __ push(r3);

    // Get the receiver.
    __ LoadP(r2, MemOperand(fp, kRecvOffset));

    // Check that the function is a JS function (otherwise it must be a proxy).
    Label push_receiver;
    __ LoadP(r3, MemOperand(fp, kFunctionOffset));
    __ CompareObjectType(r3, r4, r4, JS_FUNCTION_TYPE);
    __ bne(&push_receiver);

    // Change context eagerly to get the right global object if necessary.
    __ LoadP(cp, FieldMemOperand(r3, JSFunction::kContextOffset));
    // Load the shared function info while the function is still in r3.
    __ LoadP(r4,
             FieldMemOperand(r3, JSFunction::kSharedFunctionInfoOffset));

    // Compute the receiver.
    // Do not transform the receiver for strict mode functions.
    Label call_to_object, use_global_receiver;
    __ LoadlW(r4,
              FieldMemOperand(r4, SharedFunctionInfo::kCompilerHintsOffset));
    __ TestBit(r4,
#if V8_TARGET_ARCH_S390X
               SharedFunctionInfo::kStrictModeFunction,
#else
               SharedFunctionInfo::kStrictModeFunction + kSmiTagSize,
#endif
               r0);
    __ bne(&push_receiver /*, cr0*/);

    // Do not transform the receiver for strict mode functions.
    __ TestBit(r4,
#if V8_TARGET_ARCH_S390X
               SharedFunctionInfo::kNative,
#else
               SharedFunctionInfo::kNative + kSmiTagSize,
#endif
               r0);
    __ bne(&push_receiver /*, cr0*/);

    // Compute the receiver in non-strict mode.
    __ JumpIfSmi(r2, &call_to_object);
    __ LoadRoot(r3, Heap::kNullValueRootIndex);
    __ CmpRR(r2, r3);
    __ beq(&use_global_receiver);
    __ LoadRoot(r3, Heap::kUndefinedValueRootIndex);
    __ CmpRR(r2, r3);
    __ beq(&use_global_receiver);

    // Check if the receiver is already a JavaScript object.
    // r2: receiver
    STATIC_ASSERT(LAST_SPEC_OBJECT_TYPE == LAST_TYPE);
    __ CompareObjectType(r2, r3, r3, FIRST_SPEC_OBJECT_TYPE);
    __ bge(&push_receiver);

    // Convert the receiver to a regular object.
    // r2: receiver
    __ bind(&call_to_object);
    __ push(r2);
    __ InvokeBuiltin(Builtins::TO_OBJECT, CALL_FUNCTION);
    __ b(&push_receiver);

    // Use the current global receiver object as the receiver.
    __ bind(&use_global_receiver);
    const int kGlobalOffset =
        Context::kHeaderSize + Context::GLOBAL_OBJECT_INDEX * kPointerSize;
    __ LoadP(r2, FieldMemOperand(cp, kGlobalOffset));
    __ LoadP(r2, FieldMemOperand(r2, GlobalObject::kNativeContextOffset));
    __ LoadP(r2, FieldMemOperand(r2, kGlobalOffset));
    __ LoadP(r2, FieldMemOperand(r2, GlobalObject::kGlobalReceiverOffset));

    // Push the receiver.
    // r2: receiver
    __ bind(&push_receiver);
    __ push(r2);

    // Copy all arguments from the array to the stack.
    Label entry, loop;
    __ LoadP(r2, MemOperand(fp, kIndexOffset));
    __ b(&entry);

    // Load the current argument from the arguments array and push it to the
    // stack.
    // r2: current argument index
    __ bind(&loop);
    __ LoadP(r3, MemOperand(fp, kArgsOffset));
    __ push(r3);
    __ push(r2);

    // Call the runtime to access the property in the arguments array.
    __ CallRuntime(Runtime::kGetProperty, 2);
    __ push(r2);

    // Use inline caching to access the arguments.
    __ LoadP(r2, MemOperand(fp, kIndexOffset));
    __ AddSmiLiteral(r2, r2, Smi::FromInt(1), r0);
    __ StoreP(r2, MemOperand(fp, kIndexOffset));

    // Test if the copy loop has finished copying all the elements from the
    // arguments object.
    __ bind(&entry);
    __ LoadP(r3, MemOperand(fp, kLimitOffset));
    __ CmpRR(r2, r3);
    __ bne(&loop);

    // Invoke the function.
    Label call_proxy;
    ParameterCount actual(r2);
    __ SmiUntag(r2);
    __ LoadP(r3, MemOperand(fp, kFunctionOffset));
    __ CompareObjectType(r3, r4, r4, JS_FUNCTION_TYPE);
    __ bne(&call_proxy);
    __ InvokeFunction(r3, actual, CALL_FUNCTION,
                      NullCallWrapper(), CALL_AS_METHOD);

    frame_scope.GenerateLeaveFrame();
    __ la(sp, MemOperand(sp, 3 * kPointerSize));
    __ Ret();

    // Invoke the function proxy.
    __ bind(&call_proxy);
    __ push(r3);  // add function proxy as last argument
    __ AddP(r2, Operand(1));
    __ LoadImmP(r4, Operand(0, RelocInfo::NONE));
    __ SetCallKind(r7, CALL_AS_METHOD);
    __ GetBuiltinEntry(r5, Builtins::CALL_FUNCTION_PROXY);
    __ Call(masm->isolate()->builtins()->ArgumentsAdaptorTrampoline(),
            RelocInfo::CODE_TARGET);

    // Tear down the internal frame and remove function, receiver and args.
  }
  __ la(sp, MemOperand(sp, 3 * kPointerSize));
  __ Ret();
}

static void EnterArgumentsAdaptorFrame(MacroAssembler* masm) {
  __ SmiTag(r2);
  __ LoadSmiLiteral(r6, Smi::FromInt(StackFrame::ARGUMENTS_ADAPTOR));
  // Stack updated as such:
  //    old SP --->
  //                 R14 Return Addr
  //                 Old FP                     <--- New FP
  //                 Argument Adapter SMI
  //                 Function
  //                 ArgC as SMI                <--- New SP
  __ lay(sp, MemOperand(sp, -5 * kPointerSize));

  // Cleanse the top nibble of 31-bit pointers.
  __ CleanseP(r14);

  __ StoreP(r14, MemOperand(sp, 4 * kPointerSize));
  __ StoreP(fp, MemOperand(sp, 3 * kPointerSize));
  __ StoreP(r6, MemOperand(sp, 2 * kPointerSize));
  __ StoreP(r3, MemOperand(sp, 1 * kPointerSize));
  __ StoreP(r2, MemOperand(sp, 0 * kPointerSize));
  __ la(fp, MemOperand(sp, 3 * kPointerSize));
}


static void LeaveArgumentsAdaptorFrame(MacroAssembler* masm) {
  // ----------- S t a t e -------------
  //  -- r2 : result being passed through
  // -----------------------------------
  // Get the number of arguments passed (as a smi), tear down the frame and
  // then tear down the parameters.
  __ LoadRR(sp, fp);
  __ LoadP(r3, MemOperand(fp, -3 * kPointerSize));
  __ SmiToPtrArrayOffset(r3, r3);
  __ LoadP(fp, MemOperand(fp));
  __ LoadP(r14, MemOperand(sp, kPointerSize));
  // adjust SP with 3 ptrs for receiver + fp + lr
  __ la(sp, MemOperand(r3, sp, 3 * kPointerSize));
}


void Builtins::Generate_ArgumentsAdaptorTrampoline(MacroAssembler* masm) {
  // ----------- S t a t e -------------
  //  -- r2 : actual number of arguments
  //  -- r3 : function (passed through to callee)
  //  -- r4 : expected number of arguments
  //  -- r5 : code entry to call
  //  -- r7 : call kind information
  // -----------------------------------

  Label invoke, dont_adapt_arguments;

  Label enough, too_few;
  __ CmpRR(r2, r4);
  __ blt(&too_few);
  __ Cmpi(r4, Operand(SharedFunctionInfo::kDontAdaptArgumentsSentinel));
  __ beq(&dont_adapt_arguments);

  {  // Enough parameters: actual >= expected
    __ bind(&enough);
    EnterArgumentsAdaptorFrame(masm);

    // Calculate copy start address into r2 and copy end address into r4.
    // r2: actual number of arguments as a smi
    // r3: function
    // r4: expected number of arguments
    // r5: code entry to call
    __ SmiToPtrArrayOffset(r2, r2);
    __ AddP(r2, fp);
    // adjust for return address and receiver
    __ AddP(r2, Operand(2 * kPointerSize));
    __ ShiftLeftImm(r4, r4, Operand(kPointerSizeLog2));
    __ Sub(r4, r2, r4);

    // Copy the arguments (including the receiver) to the new stack frame.
    // r2: copy start address
    // r3: function
    // r4: copy end address
    // r5: code entry to call

    Label copy;
    __ bind(&copy);
    __ LoadP(ip, MemOperand(r2, 0));
    __ push(ip);
    __ CmpRR(r2, r4);  // Compare before moving to next argument.
    __ lay(r2, MemOperand(r2, -kPointerSize));
    __ bne(&copy);

    __ b(&invoke);
  }

  {  // Too few parameters: Actual < expected
    __ bind(&too_few);
    EnterArgumentsAdaptorFrame(masm);

    // Calculate copy start address into r0 and copy end address is fp.
    // r2: actual number of arguments as a smi
    // r3: function
    // r4: expected number of arguments
    // r5: code entry to call
    __ SmiToPtrArrayOffset(r2, r2);
    __ AddP(r2, fp);

    // Copy the arguments (including the receiver) to the new stack frame.
    // r2: copy start address
    // r3: function
    // r4: expected number of arguments
    // r5: code entry to call
    Label copy;
    __ bind(&copy);
    // Adjust load for return address and receiver.
    __ LoadP(ip, MemOperand(r2, 2 * kPointerSize));
    __ push(ip);
    __ CmpRR(r2, fp);  // Compare before moving to next argument.
    __ lay(r2, MemOperand(r2, -kPointerSize));
    __ bne(&copy);

    // Fill the remaining expected arguments with undefined.
    // r3: function
    // r4: expected number of arguments
    // r5: code entry to call
    __ LoadRoot(ip, Heap::kUndefinedValueRootIndex);
    __ ShiftLeftImm(r4, r4, Operand(kPointerSizeLog2));
    __ Sub(r4, fp, r4);
    __ Sub(r4, Operand(4 * kPointerSize));  // Adjust for frame.

    Label fill;
    __ bind(&fill);
    __ push(ip);
    __ CmpRR(sp, r4);
    __ bne(&fill);
  }

  // Call the entry point.
  __ bind(&invoke);
  __ Call(r5);

  // Store offset of return address for deoptimizer.
  masm->isolate()->heap()->SetArgumentsAdaptorDeoptPCOffset(masm->pc_offset());

  // Exit frame and return.
  LeaveArgumentsAdaptorFrame(masm);
  __ Ret();


  // -------------------------------------------
  // Dont adapt arguments.
  // -------------------------------------------
  __ bind(&dont_adapt_arguments);
  __ Jump(r5);
}


#undef __

} }  // namespace v8::internal

#endif  // V8_TARGET_ARCH_S390
