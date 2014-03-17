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


// @TODO Rename PPC regs. This file uses: r0, r1, r3-r11, r22.

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
  //  -- r3_p                 : number of arguments excluding receiver
  //  -- r4_p                 : called function (only guaranteed when
  //                          extra_args requires it)
  //  -- cp                 : context
  //  -- sp[0]              : last argument
  //  -- ...
  //  -- sp[4 * (argc - 1)] : first argument (argc == r0_p)
  //  -- sp[4 * argc]       : receiver
  // -----------------------------------

  // Insert extra arguments.
  int num_extra_args = 0;
  if (extra_args == NEEDS_CALLED_FUNCTION) {
    num_extra_args = 1;
    __ push(r4_p);
  } else {
    ASSERT(extra_args == NO_EXTRA_ARGUMENTS);
  }

  // JumpToExternalReference expects r0_p to contain the number of arguments
  // including the receiver and the extra arguments.
  __ AddP(r3_p, Operand(num_extra_args + 1));
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
//   r3_p: argc
//   r4_p: constructor (built-in Array function)
//   lr: return address
//   sp[0]: last argument
// This function is used for both construct and normal calls of Array. The only
// difference between handling a construct call and a normal call is that for a
// construct call the constructor function in r1_p needs to be preserved for
// entering the generic code. In both cases argc in r0_p needs to be preserved.
// Both registers are preserved by this code so no need to differentiate between
// construct call and normal call.
static void ArrayNativeCode(MacroAssembler* masm,
                            Label* call_generic_code) {
  Counters* counters = masm->isolate()->counters();
  Label argc_one_or_more, argc_two_or_more, not_empty_array, empty_array,
      has_non_smi_element, finish, cant_transition_map, not_double;

  // Check for array construction with zero arguments or one.
  __ Cmpi(r3_p, Operand(0, RelocInfo::NONE));
  __ bne(&argc_one_or_more);

  // Handle construction of an empty array.
  __ bind(&empty_array);
  AllocateEmptyJSArray(masm,
                       r4_p,
                       r5_p,
                       r6_p,
                       r7_p,
                       r8_p,
                       call_generic_code);
  __ IncrementCounter(counters->array_function_native(), 1, r6_p, r7_p);
  // Set up return value, remove receiver from stack and return.
  __ LoadRR(r3_p, r5_p);
  __ AddP(sp, Operand(kPointerSize));
  __ Ret();

  // Check for one argument. Bail out if argument is not smi or if it is
  // negative.
  __ bind(&argc_one_or_more);
  __ Cmpi(r3_p, Operand(1));
  __ bne(&argc_two_or_more);
  STATIC_ASSERT(kSmiTag == 0);
  __ LoadP(r5_p, MemOperand(sp));  // Get the argument from the stack.
  __ Cmpi(r5_p, Operand::Zero());
  __ bne(&not_empty_array);
  __ Drop(1);  // Adjust stack.
  __ LoadImmP(r3_p, Operand::Zero());  // Treat this as a call with argc of zero
  __ b(&empty_array);

  __ bind(&not_empty_array);
  __ TestIfPositiveSmi(r5_p, r6_p);
  __ bne(call_generic_code /*, cr0*/);

  // Handle construction of an empty array of a certain size. Bail out if size
  // is too large to actually allocate an elements array.
  STATIC_ASSERT(kSmiTag == 0);
  __ CmpSmiLiteral(r5_p, Smi::FromInt(JSObject::kInitialMaxFastElementArray),
                   r0_p);
  __ bge(call_generic_code);

  // r3_p: argc
  // r4_p: constructor
  // r5_p: array_size (smi)
  // sp[0]: argument
  AllocateJSArray(masm,
                  r4_p,
                  r5_p,
                  r6_p,
                  r7_p,
                  r8_p,
                  r9_p,
                  r10_p,
                  true,
                  call_generic_code);
  __ IncrementCounter(counters->array_function_native(), 1, r5_p, r7_p);
  // Set up return value, remove receiver and argument from stack and return.
  __ LoadRR(r3_p, r6_p);
  __ AddP(sp, Operand(2 * kPointerSize));
  __ Ret();

  // Handle construction of an array from a list of arguments.
  __ bind(&argc_two_or_more);
  // Convet argc to a smi.
  __ SmiTag(r5_p, r3_p);

  // r3_p: argc
  // r4_p: constructor
  // r5_p: array_size (smi)
  // sp[0]: last argument
  AllocateJSArray(masm,
                  r4_p,
                  r5_p,
                  r6_p,
                  r7_p,
                  r8_p,
                  r9_p,
                  r10_p,
                  false,
                  call_generic_code);
  __ IncrementCounter(counters->array_function_native(), 1, r5_p, r9_p);

  // Fill arguments as array elements. Copy from the top of the stack (last
  // element) to the array backing store filling it backwards. Note:
  // elements_array_end points after the backing store therefore PreIndex is
  // used when filling the backing store.
  // r3_p: argc
  // r6_p: JSArray
  // r7_p: elements_array storage start (untagged)
  // r8_p: elements_array_end (untagged)
  // sp[0]: last argument
  Label loop, entry;
  __ LoadRR(r10_p, sp);
  __ b(&entry);
  __ bind(&loop);
  __ LoadP(r5_p, MemOperand(r10_p));
  __ AddP(r10_p, Operand(kPointerSize));
  if (FLAG_smi_only_arrays) {
    __ JumpIfNotSmi(r5_p, &has_non_smi_element);
  }
  __ StorePU(r5_p, MemOperand(r8_p, -kPointerSize));
  __ bind(&entry);
  __ CmpRR(r7_p, r8_p);
  __ blt(&loop);

  __ bind(&finish);
  __ LoadRR(sp, r10_p);

  // Remove caller arguments and receiver from the stack, setup return value and
  // return.
  // r3_p: argc
  // r6_p: JSArray
  // sp[0]: receiver
  __ AddP(sp, Operand(kPointerSize));
  __ LoadRR(r3_p, r6_p);
  __ Ret();

  __ bind(&has_non_smi_element);
  // Double values are handled by the runtime.
  __ CheckMap(r5_p, r22_p,
      Heap::kHeapNumberMapRootIndex, &not_double, DONT_DO_SMI_CHECK);
  __ bind(&cant_transition_map);
  __ UndoAllocationInNewSpace(r6_p, r7_p);
  __ b(call_generic_code);

  __ bind(&not_double);
  // Transition FAST_SMI_ELEMENTS to FAST_ELEMENTS.
  // r6_p: JSArray
  __ LoadP(r5_p, FieldMemOperand(r6_p, HeapObject::kMapOffset));
  __ LoadTransitionedArrayMapConditional(FAST_SMI_ELEMENTS,
                                         FAST_ELEMENTS,
                                         r5_p,
                                         r22_p,
                                         &cant_transition_map);
  __ StoreP(r5_p, FieldMemOperand(r6_p, HeapObject::kMapOffset));
  __ RecordWriteField(r6_p,
                      HeapObject::kMapOffset,
                      r5_p,
                      r22_p,
                      kLRHasNotBeenSaved,
                      kDontSaveFPRegs,
                      EMIT_REMEMBERED_SET,
                      OMIT_SMI_CHECK);
  Label loop2;
  __ Sub(r10_p, Operand(kPointerSize));
  __ bind(&loop2);
  __ LoadP(r5_p, MemOperand(r10_p));
  __ AddP(r10_p, Operand(kPointerSize));
  __ StorePU(r5_p, MemOperand(r8_p, -kPointerSize));
  __ CmpRR(r7_p, r8_p);
  __ blt(&loop2);
  __ b(&finish);
}


void Builtins::Generate_InternalArrayCode(MacroAssembler* masm) {
  // ----------- S t a t e -------------
  //  -- r3_p     : number of arguments
  //  -- lr     : return address
  //  -- sp[...]: constructor arguments
  // -----------------------------------
  Label generic_array_code, one_or_more_arguments, two_or_more_arguments;

  // Get the InternalArray function.
  GenerateLoadInternalArrayFunction(masm, r4_p);

  if (FLAG_debug_code) {
    // Initial map for the builtin InternalArray functions should be maps.
    __ LoadP(r5_p,
        FieldMemOperand(r4_p, JSFunction::kPrototypeOrInitialMapOffset));
    STATIC_ASSERT(kSmiTagMask < 0x8000);
    __ LoadRR(r0_p, r5_p);
    __ AndP(r0_p, Operand(kSmiTagMask));
    __ Assert(ne, "Unexpected initial map for InternalArray function", cr0);
    __ CompareObjectType(r5_p, r6_p, r7_p, MAP_TYPE);
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
  //  -- r3_p     : number of arguments
  //  -- lr     : return address
  //  -- sp[...]: constructor arguments
  // -----------------------------------
  Label generic_array_code, one_or_more_arguments, two_or_more_arguments;

  // Get the Array function.
  GenerateLoadArrayFunction(masm, r4_p);

  if (FLAG_debug_code) {
    // Initial map for the builtin Array functions should be maps.
    __ LoadP(r5_p,
        FieldMemOperand(r4_p, JSFunction::kPrototypeOrInitialMapOffset));
    STATIC_ASSERT(kSmiTagMask < 0x8000);
    __ LoadRR(r0_p, r5_p);
    __ AndP(r0_p, Operand(kSmiTagMask));
    __ Assert(ne, "Unexpected initial map for Array function", cr0);
    __ CompareObjectType(r5_p, r6_p, r7_p, MAP_TYPE);
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
  //  -- r3_p     : number of arguments
  //  -- r4_p     : constructor function
  //  -- lr     : return address
  //  -- sp[...]: constructor arguments
  // -----------------------------------
  Label generic_constructor;

  if (FLAG_debug_code) {
    // The array construct code is only set for the builtin and internal
    // Array functions which always have a map.
    // Initial map for the builtin Array function should be a map.
    __ LoadP(r5_p,
        FieldMemOperand(r4_p, JSFunction::kPrototypeOrInitialMapOffset));
    __ LoadRR(r0_p, r5_p);
    __ AndP(r0_p, Operand(kSmiTagMask));
    __ Assert(ne, "Unexpected initial map for Array function", cr0);
    __ CompareObjectType(r5_p, r6_p, r7_p, MAP_TYPE);
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
  //  -- r3_p                     : number of arguments
  //  -- r4_p                     : constructor function
  //  -- lr                     : return address
  //  -- sp[(argc - n - 1) * 4] : arg[n] (zero based)
  //  -- sp[argc * 4]           : receiver
  // -----------------------------------
  Counters* counters = masm->isolate()->counters();
  __ IncrementCounter(counters->string_ctor_calls(), 1, r5_p, r6_p);

  Register function = r4_p;
  if (FLAG_debug_code) {
    __ LoadGlobalFunction(Context::STRING_FUNCTION_INDEX, r5_p);
    __ CmpRR(function, r5_p);
    __ Assert(eq, "Unexpected String function");
  }

  // Load the first arguments in r3_p and get rid of the rest.
  Label no_arguments;
  __ Cmpi(r3_p, Operand(0, RelocInfo::NONE));
  __ beq(&no_arguments);
  // First args = sp[(argc - 1) * 4].
  __ Sub(r3_p, Operand(1));
  __ ShiftLeftImm(r3_p, r3_p, Operand(kPointerSizeLog2));
  __ AddP(sp, r3_p);
  __ LoadP(r3_p, MemOperand(sp));
  // sp now point to args[0], drop args[0] + receiver.
  __ Drop(2);

  Register argument = r5_p;
  Label not_cached, argument_is_string;
  NumberToStringStub::GenerateLookupNumberStringCache(
      masm,
      r3_p,        // Input.
      argument,  // Result.
      r6_p,        // Scratch.
      r7_p,        // Scratch.
      r8_p,        // Scratch.
      false,     // Is it a Smi?
      &not_cached);
  __ IncrementCounter(counters->string_ctor_cached_number(), 1, r6_p, r7_p);
  __ bind(&argument_is_string);

  // ----------- S t a t e -------------
  //  -- r5_p     : argument converted to string
  //  -- r4_p     : constructor function
  //  -- lr     : return address
  // -----------------------------------

  Label gc_required;
  __ AllocateInNewSpace(JSValue::kSize,
                        r3_p,  // Result.
                        r6_p,  // Scratch.
                        r7_p,  // Scratch.
                        &gc_required,
                        TAG_OBJECT);

  // Initialising the String Object.
  Register map = r6_p;
  __ LoadGlobalFunctionInitialMap(function, map, r7_p);
  if (FLAG_debug_code) {
    __ LoadlB(r7_p, FieldMemOperand(map, Map::kInstanceSizeOffset));
    __ Cmpi(r7_p, Operand(JSValue::kSize >> kPointerSizeLog2));
    __ Assert(eq, "Unexpected string wrapper instance size");
    __ LoadlB(r7_p, FieldMemOperand(map, Map::kUnusedPropertyFieldsOffset));
    __ Cmpi(r7_p, Operand(0, RelocInfo::NONE));
    __ Assert(eq, "Unexpected unused properties of string wrapper");
  }
  __ StoreP(map, FieldMemOperand(r3_p, HeapObject::kMapOffset));

  __ LoadRoot(r6_p, Heap::kEmptyFixedArrayRootIndex);
  __ StoreP(r6_p, FieldMemOperand(r3_p, JSObject::kPropertiesOffset));
  __ StoreP(r6_p, FieldMemOperand(r3_p, JSObject::kElementsOffset));

  __ StoreP(argument, FieldMemOperand(r3_p, JSValue::kValueOffset));

  // Ensure the object is fully initialized.
  STATIC_ASSERT(JSValue::kSize == 4 * kPointerSize);

  __ Ret();

  // The argument was not found in the number to string cache. Check
  // if it's a string already before calling the conversion builtin.
  Label convert_argument;
  __ bind(&not_cached);
  __ JumpIfSmi(r3_p, &convert_argument);

  // Is it a String?
  __ LoadP(r5_p, FieldMemOperand(r3_p, HeapObject::kMapOffset));
  __ LoadlB(r6_p, FieldMemOperand(r5_p, Map::kInstanceTypeOffset));
  STATIC_ASSERT(kNotStringTag != 0);
  __ LoadRR(r0_p, r6_p);
  __ AndP(r0_p, Operand(kIsNotStringMask));
  __ bne(&convert_argument /*, cr0*/);
  __ LoadRR(argument, r3_p);
  __ IncrementCounter(counters->string_ctor_conversions(), 1, r6_p, r7_p);
  __ b(&argument_is_string);

  // Invoke the conversion builtin and put the result into r5_p.
  __ bind(&convert_argument);
  __ push(function);  // Preserve the function.
  __ IncrementCounter(counters->string_ctor_conversions(), 1, r6_p, r7_p);
  {
    FrameScope scope(masm, StackFrame::INTERNAL);
    __ push(r3_p);
    __ InvokeBuiltin(Builtins::TO_STRING, CALL_FUNCTION);
  }
  __ pop(function);
  __ LoadRR(argument, r3_p);
  __ b(&argument_is_string);

  // Load the empty string into r5_p, remove the receiver from the
  // stack, and jump back to the case where the argument is a string.
  __ bind(&no_arguments);
  __ LoadRoot(argument, Heap::kEmptyStringRootIndex);
  __ Drop(1);
  __ b(&argument_is_string);

  // At this point the argument is already a string. Call runtime to
  // create a string wrapper.
  __ bind(&gc_required);
  __ IncrementCounter(counters->string_ctor_gc_required(), 1, r6_p, r7_p);
  {
    FrameScope scope(masm, StackFrame::INTERNAL);
    __ push(argument);
    __ CallRuntime(Runtime::kNewStringWrapper, 1);
  }
  __ Ret();
}


static void GenerateTailCallToSharedCode(MacroAssembler* masm) {
  __ LoadP(r5_p, FieldMemOperand(r4_p, JSFunction::kSharedFunctionInfoOffset));
  __ LoadP(r5_p, FieldMemOperand(r5_p, SharedFunctionInfo::kCodeOffset));
  __ AddP(r5_p, Operand(Code::kHeaderSize - kHeapObjectTag));
  __ mtctr(r5_p);
  __ bcr();
}


void Builtins::Generate_InRecompileQueue(MacroAssembler* masm) {
  GenerateTailCallToSharedCode(masm);
}


void Builtins::Generate_ParallelRecompile(MacroAssembler* masm) {
  {
    FrameScope scope(masm, StackFrame::INTERNAL);

    // Push a copy of the function onto the stack.
    __ push(r4_p);
    // Push call kind information.
    __ push(r8_p);

    __ push(r4_p);  // Function is also the parameter to the runtime call.
    __ CallRuntime(Runtime::kParallelRecompile, 1);

    // Restore call kind information.
    __ pop(r8_p);
    // Restore receiver.
    __ pop(r4_p);

    // Tear down internal frame.
  }

  GenerateTailCallToSharedCode(masm);
}


static void Generate_JSConstructStubHelper(MacroAssembler* masm,
                                           bool is_api_function,
                                           bool count_constructions) {
  // ----------- S t a t e -------------
  //  -- r3_p     : number of arguments
  //  -- r4_p     : constructor function
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
    __ SmiTag(r3_p);
    __ push(r3_p);  // Smi-tagged arguments count.
    __ push(r4_p);  // Constructor function.

    // Try to allocate the object without transitioning into C code. If any of
    // the preconditions is not met, the code bails out to the runtime call.
    Label rt_call, allocated;
    if (FLAG_inline_new) {
      Label undo_allocation;
#ifdef ENABLE_DEBUGGER_SUPPORT
      ExternalReference debug_step_in_fp =
          ExternalReference::debug_step_in_fp_address(isolate);
      __ mov(r5_p, Operand(debug_step_in_fp));
      __ LoadP(r5_p, MemOperand(r5_p));
      __ Cmpi(r5_p, Operand::Zero());
      __ bne(&rt_call);
#endif

      // Load the initial map and verify that it is in fact a map.
      // r4_p: constructor function
      __ LoadP(r5_p, FieldMemOperand(r4_p,
                                   JSFunction::kPrototypeOrInitialMapOffset));
      __ JumpIfSmi(r5_p, &rt_call);
      __ CompareObjectType(r5_p, r6_p, r7_p, MAP_TYPE);
      __ bne(&rt_call);

      // Check that the constructor is not constructing a JSFunction (see
      // comments in Runtime_NewObject in runtime.cc). In which case the
      // initial map's instance type would be JS_FUNCTION_TYPE.
      // r4_p: constructor function
      // r5_p: initial map
      __ CompareInstanceType(r5_p, r6_p, JS_FUNCTION_TYPE);
      __ beq(&rt_call);

      if (count_constructions) {
        Label allocate;
        // Decrease generous allocation count.
        __ LoadP(r6_p, FieldMemOperand(r4_p,
                                     JSFunction::kSharedFunctionInfoOffset));
        MemOperand constructor_count =
            FieldMemOperand(r6_p, SharedFunctionInfo::kConstructionCountOffset);
        __ LoadlB(r7_p, constructor_count);
        __ AddP(r7_p, Operand(-1));
        __ stb(r7_p, constructor_count);
        __ Cmpi(r7_p, Operand::Zero());
        __ bne(&allocate);

        __ push(r4_p);
        __ push(r5_p);

        __ push(r4_p);  // constructor
        // The call will replace the stub, so the countdown is only done once.
        __ CallRuntime(Runtime::kFinalizeInstanceSize, 1);

        __ pop(r5_p);
        __ pop(r4_p);

        __ bind(&allocate);
      }

      // Now allocate the JSObject on the heap.
      // r4_p: constructor function
      // r5_p: initial map
      __ LoadlB(r6_p, FieldMemOperand(r5_p, Map::kInstanceSizeOffset));
      __ AllocateInNewSpace(r6_p, r7_p, r8_p, r9_p, &rt_call, SIZE_IN_WORDS);

      // Allocated the JSObject, now initialize the fields. Map is set to
      // initial map and properties and elements are set to empty fixed array.
      // r4_p: constructor function
      // r5_p: initial map
      // r6_p: object size
      // r7_p: JSObject (not tagged)
      __ LoadRoot(r9_p, Heap::kEmptyFixedArrayRootIndex);
      __ LoadRR(r8_p, r7_p);
      ASSERT_EQ(0 * kPointerSize, JSObject::kMapOffset);
      __ StoreP(r5_p, MemOperand(r8_p));
      ASSERT_EQ(1 * kPointerSize, JSObject::kPropertiesOffset);
      __ StorePU(r9_p, MemOperand(r8_p, kPointerSize));
      ASSERT_EQ(2 * kPointerSize, JSObject::kElementsOffset);
      __ StorePU(r9_p, MemOperand(r8_p, kPointerSize));
      __ AddP(r8_p, Operand(kPointerSize));

      // Fill all the in-object properties with the appropriate filler.
      // r4_p: constructor function
      // r5_p: initial map
      // r6_p: object size (in words)
      // r7_p: JSObject (not tagged)
      // r8_p: First in-object property of JSObject (not tagged)
      uint32_t byte;
      __ ShiftLeftImm(r9_p, r6_p, Operand(kPointerSizeLog2));
      __ AddP(r9_p, r7_p);  // End of object.
      ASSERT_EQ(3 * kPointerSize, JSObject::kHeaderSize);
      __ LoadRoot(r10_p, Heap::kUndefinedValueRootIndex);
      if (count_constructions) {
        __ LoadlW(r3_p, FieldMemOperand(r5_p, Map::kInstanceSizesOffset));
        // Fetch Map::kPreAllocatedPropertyFieldsByte field from r3_p
        // and multiply by kPointerSizeLog2
        STATIC_ASSERT(Map::kPreAllocatedPropertyFieldsByte < 4);
        byte = Map::kPreAllocatedPropertyFieldsByte;
#if __BYTE_ORDER == __BIG_ENDIAN
        byte = 3 - byte;
#endif
        __ ExtractBitRange(r3_p, r3_p,
                           ((byte + 1) * kBitsPerByte) - 1,
                           byte * kBitsPerByte);
        __ ShiftLeftImm(r3_p, r3_p, Operand(kPointerSizeLog2));
        __ AddP(r3_p, r8_p);
        // r3_p: offset of first field after pre-allocated fields
        if (FLAG_debug_code) {
          __ CmpRR(r3_p, r9_p);
          __ Assert(le, "Unexpected number of pre-allocated property fields.");
        }
        __ InitializeFieldsWithFiller(r8_p, r3_p, r10_p);
        // To allow for truncation.
        __ LoadRoot(r10_p, Heap::kOnePointerFillerMapRootIndex);
      }
      __ InitializeFieldsWithFiller(r8_p, r9_p, r10_p);

      // Add the object tag to make the JSObject real, so that we can continue
      // and jump into the continuation code at any time from now on. Any
      // failures need to undo the allocation, so that the heap is in a
      // consistent state and verifiable.
      __ AddP(r7_p, Operand(kHeapObjectTag));

      // Check if a non-empty properties array is needed. Continue with
      // allocated object if not fall through to runtime call if it is.
      // r4_p: constructor function
      // r7_p: JSObject
      // r8_p: start of next object (not tagged)
      __ LoadlB(r6_p, FieldMemOperand(r5_p, Map::kUnusedPropertyFieldsOffset));
      // The field instance sizes contains both pre-allocated property fields
      // and in-object properties.
      __ LoadlW(r3_p, FieldMemOperand(r5_p, Map::kInstanceSizesOffset));
      // Fetch Map::kPreAllocatedPropertyFieldsByte field from r3_p
      STATIC_ASSERT(Map::kPreAllocatedPropertyFieldsByte < 4);
      byte = Map::kPreAllocatedPropertyFieldsByte;
#if __BYTE_ORDER == __BIG_ENDIAN
      byte = 3 - byte;
#endif
      __ ExtractBitRange(r9_p, r3_p,
                         ((byte + 1) * kBitsPerByte) - 1,
                         byte * kBitsPerByte);
      __ AddP(r6_p, r9_p);
      STATIC_ASSERT(Map::kInObjectPropertiesByte < 4);
      byte = Map::kInObjectPropertiesByte;
#if __BYTE_ORDER == __BIG_ENDIAN
      byte = 3 - byte;
#endif
      __ ExtractBitRange(r9_p, r3_p,
                         ((byte + 1) * kBitsPerByte) - 1,
                         byte * kBitsPerByte);
      __ Sub(r6_p, r6_p, r9_p);  // roohack - sub order may be incorrect
      __ Cmpi(r6_p, Operand::Zero());

      // Done if no extra properties are to be allocated.
      __ beq(&allocated);
      __ Assert(ge, "Property allocation count failed.");

      // Scale the number of elements by pointer size and add the header for
      // FixedArrays to the start of the next object calculation from above.
      // r4_p: constructor
      // r6_p: number of elements in properties array
      // r7_p: JSObject
      // r8_p: start of next object
      __ LoadRR(r3_p, r6_p);
      __ AddP(r3_p, Operand(FixedArray::kHeaderSize / kPointerSize));
      __ AllocateInNewSpace(
          r3_p,
          r8_p,
          r9_p,
          r5_p,
          &undo_allocation,
          static_cast<AllocationFlags>(RESULT_CONTAINS_TOP | SIZE_IN_WORDS));

      // Initialize the FixedArray.
      // r4_p: constructor
      // r6_p: number of elements in properties array
      // r7_p: JSObject
      // r8_p: FixedArray (not tagged)
      __ LoadRoot(r9_p, Heap::kFixedArrayMapRootIndex);
      __ LoadRR(r5_p, r8_p);
      ASSERT_EQ(0 * kPointerSize, JSObject::kMapOffset);
      __ StoreP(r9_p, MemOperand(r5_p));
      ASSERT_EQ(1 * kPointerSize, FixedArray::kLengthOffset);
      __ SmiTag(r3_p, r6_p);
      __ StorePU(r3_p, MemOperand(r5_p, kPointerSize));
      __ AddP(r5_p, Operand(kPointerSize));

      // Initialize the fields to undefined.
      // r4_p: constructor function
      // r5_p: First element of FixedArray (not tagged)
      // r6_p: number of elements in properties array
      // r7_p: JSObject
      // r8_p: FixedArray (not tagged)
      __ ShiftLeftImm(r9_p, r6_p, Operand(kPointerSizeLog2));
      __ AddP(r9_p, r5_p);  // End of object.
      ASSERT_EQ(2 * kPointerSize, FixedArray::kHeaderSize);
      { Label loop, entry;
        if (count_constructions) {
          __ LoadRoot(r10_p, Heap::kUndefinedValueRootIndex);
        } else if (FLAG_debug_code) {
          __ LoadRoot(r11_p, Heap::kUndefinedValueRootIndex);
          __ CmpRR(r10_p, r11_p);
          __ Assert(eq, "Undefined value not loaded.");
        }
        __ b(&entry);
        __ bind(&loop);
        __ StoreP(r10_p, MemOperand(r5_p));
        __ AddP(r5_p, Operand(kPointerSize));
        __ bind(&entry);
        __ CmpRR(r5_p, r9_p);
        __ blt(&loop);
      }

      // Store the initialized FixedArray into the properties field of
      // the JSObject
      // r4_p: constructor function
      // r7_p: JSObject
      // r8_p: FixedArray (not tagged)
      __ AddP(r8_p, Operand(kHeapObjectTag));  // Add the heap tag.
      __ StoreP(r8_p, FieldMemOperand(r7_p, JSObject::kPropertiesOffset));

      // Continue with JSObject being successfully allocated
      // r4_p: constructor function
      // r7_p: JSObject
      __ b(&allocated);

      // Undo the setting of the new top so that the heap is verifiable. For
      // example, the map's unused properties potentially do not match the
      // allocated objects unused properties.
      // r7_p: JSObject (previous new top)
      __ bind(&undo_allocation);
      __ UndoAllocationInNewSpace(r7_p, r8_p);
    }

    // Allocate the new receiver object using the runtime call.
    // r4_p: constructor function
    __ bind(&rt_call);
    __ push(r4_p);  // argument for Runtime_NewObject
    __ CallRuntime(Runtime::kNewObject, 1);
    __ LoadRR(r7_p, r3_p);

    // Receiver for constructor call allocated.
    // r7_p: JSObject
    __ bind(&allocated);
    __ push(r7_p);
    __ push(r7_p);

    // Reload the number of arguments and the constructor from the stack.
    // sp[0]: receiver
    // sp[1]: receiver
    // sp[2]: constructor function
    // sp[3]: number of arguments (smi-tagged)
    __ LoadP(r4_p, MemOperand(sp, 2 * kPointerSize));
    __ LoadP(r6_p, MemOperand(sp, 3 * kPointerSize));

    // Set up pointer to last argument.
    __ LoadRR(r5_p, fp);
    __ AddP(r5_p, Operand(StandardFrameConstants::kCallerSPOffset));

    // Set up number of arguments for function call below
    __ SmiUntag(r3_p, r6_p);

    // Copy arguments and receiver to the expression stack.
    // r3_p: number of arguments
    // r4_p: constructor function
    // r5_p: address of last argument (caller sp)
    // r6_p: number of arguments (smi-tagged)
    // sp[0]: receiver
    // sp[1]: receiver
    // sp[2]: constructor function
    // sp[3]: number of arguments (smi-tagged)
    Label loop, no_args;
    __ Cmpi(r3_p, Operand::Zero());
    __ beq(&no_args);
    __ ShiftLeftImm(ip, r3_p, Operand(kPointerSizeLog2));
    __ mtctr(r3_p);
    __ bind(&loop);
    __ Sub(ip, Operand(kPointerSize));
    __ LoadP(r0_p, MemOperand(r5_p, ip));
    __ push(r0_p);
    __ bdnz(&loop);
    __ bind(&no_args);

    // Call the function.
    // r3_p: number of arguments
    // r4_p: constructor function
    if (is_api_function) {
      __ LoadP(cp, FieldMemOperand(r4_p, JSFunction::kContextOffset));
      Handle<Code> code =
          masm->isolate()->builtins()->HandleApiCallConstruct();
      ParameterCount expected(0);
      __ InvokeCode(code, expected, expected,
                    RelocInfo::CODE_TARGET, CALL_FUNCTION, CALL_AS_METHOD);
    } else {
      ParameterCount actual(r3_p);
      __ InvokeFunction(r4_p, actual, CALL_FUNCTION,  // roohack
                        NullCallWrapper(), CALL_AS_METHOD);
    }

    // Store offset of return address for deoptimizer.
    if (!is_api_function && !count_constructions) {
      masm->isolate()->heap()->SetConstructStubDeoptPCOffset(masm->pc_offset());
    }

    // Restore context from the frame.
    // r3_p: result
    // sp[0]: receiver
    // sp[1]: constructor function
    // sp[2]: number of arguments (smi-tagged)
    __ LoadP(cp, MemOperand(fp, StandardFrameConstants::kContextOffset));

    // If the result is an object (in the ECMA sense), we should get rid
    // of the receiver and use the result; see ECMA-262 section 13.2.2-7
    // on page 74.
    Label use_receiver, exit;

    // If the result is a smi, it is *not* an object in the ECMA sense.
    // r3_p: result
    // sp[0]: receiver (newly allocated object)
    // sp[1]: constructor function
    // sp[2]: number of arguments (smi-tagged)
    __ JumpIfSmi(r3_p, &use_receiver);

    // If the type of the result (stored in its map) is less than
    // FIRST_SPEC_OBJECT_TYPE, it is not an object in the ECMA sense.
    __ CompareObjectType(r3_p, r6_p, r6_p, FIRST_SPEC_OBJECT_TYPE);
    __ bge(&exit);

    // Throw away the result of the constructor invocation and use the
    // on-stack receiver as the result.
    __ bind(&use_receiver);
    __ LoadP(r3_p, MemOperand(sp));

    // Remove receiver from the stack, remove caller arguments, and
    // return.
    __ bind(&exit);
    // r3_p: result
    // sp[0]: receiver (newly allocated object)
    // sp[1]: constructor function
    // sp[2]: number of arguments (smi-tagged)
    __ LoadP(r4_p, MemOperand(sp, 2 * kPointerSize));

    // Leave construct frame.
  }

  __ SmiToPtrArrayOffset(r4_p, r4_p);
  __ AddP(sp, r4_p);
  __ AddP(sp, Operand(kPointerSize));
  __ IncrementCounter(isolate->counters()->constructed_objects(),
                      1, r4_p, r5_p);
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
    __ ShiftLeftImm(r7, r5, Operand(kPointerSizeLog2));
    __ SubRR(sp, r7);    // Buy the stack frame to fit args
    __ LoadImmP(r9, Operand(0));  // Initialize argv index
    __ bind(&argLoop);
    __ CmpPH(r7, Operand(0));
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
    __ push(r4_p);
    // Push call kind information.
    __ push(r8_p);

    // Push the function on the stack as the argument to the runtime function.
    __ push(r4_p);
    __ CallRuntime(Runtime::kLazyCompile, 1);
    // Calculate the entry point.
    __ LoadRR(r5_p, r3_p);
    __ AddP(r5_p, Operand(Code::kHeaderSize - kHeapObjectTag));

    // Restore call kind information.
    __ pop(r8_p);
    // Restore saved function.
    __ pop(r4_p);

    // Tear down internal frame.
  }

  // Do a tail-call of the compiled function.
  __ Jump(r5_p);
}


void Builtins::Generate_LazyRecompile(MacroAssembler* masm) {
  // Enter an internal frame.
  {
    FrameScope scope(masm, StackFrame::INTERNAL);

    // Preserve the function.
    __ push(r4_p);
    // Push call kind information.
    __ push(r8_p);

    // Push the function on the stack as the argument to the runtime function.
    __ push(r4_p);
    __ CallRuntime(Runtime::kLazyRecompile, 1);
    // Calculate the entry point.
    __ LoadRR(r5_p, r3_p);
    __ AddP(r5_p, Operand(Code::kHeaderSize - kHeapObjectTag));

    // Restore call kind information.
    __ pop(r8_p);
    // Restore saved function.
    __ pop(r4_p);

    // Tear down internal frame.
  }

  // Do a tail-call of the compiled function.
  __ Jump(r5_p);
}


static void Generate_NotifyDeoptimizedHelper(MacroAssembler* masm,
                                             Deoptimizer::BailoutType type) {
  {
    FrameScope scope(masm, StackFrame::INTERNAL);
    // Pass the function and deoptimization type to the runtime system.
    __ LoadSmiLiteral(r3_p, Smi::FromInt(static_cast<int>(type)));
    __ push(r3_p);
    __ CallRuntime(Runtime::kNotifyDeoptimized, 1);
  }

  // Get the full codegen state from the stack and untag it -> r9_p.
  __ LoadP(r9_p, MemOperand(sp, 0 * kPointerSize));
  __ SmiUntag(r9_p);
  // Switch on the state.
  Label with_tos_register, unknown_state;
  __ Cmpi(r9_p, Operand(FullCodeGenerator::NO_REGISTERS));
  __ bne(&with_tos_register);
  __ AddP(sp, Operand(1 * kPointerSize));  // Remove state.
  __ Ret();

  __ bind(&with_tos_register);
  __ LoadP(r3_p, MemOperand(sp, 1 * kPointerSize));
  __ Cmpi(r9_p, Operand(FullCodeGenerator::TOS_REG));
  __ bne(&unknown_state);
  __ AddP(sp, Operand(2 * kPointerSize));  // Remove state.
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
  __ LoadRR(r0_p, r14);
  RegList saved_regs =
      (kJSCallerSaved | kCalleeSaved | r0_p.bit() | fp.bit()) & ~sp.bit();
  __ MultiPush(saved_regs);
  {
    FrameScope scope(masm, StackFrame::INTERNAL);
    __ CallRuntime(Runtime::kNotifyOSR, 0);
  }
  __ MultiPop(saved_regs);
  __ LoadRR(r14, r0_p);
  __ Ret();
}


void Builtins::Generate_OnStackReplacement(MacroAssembler* masm) {
  // Lookup the function in the JavaScript frame and push it as an
  // argument to the on-stack replacement function.
  __ LoadP(r3_p, MemOperand(fp, JavaScriptFrameConstants::kFunctionOffset));
  {
    FrameScope scope(masm, StackFrame::INTERNAL);
    __ push(r3_p);
    __ CallRuntime(Runtime::kCompileForOnStackReplacement, 1);
  }

  // If the result was -1 it means that we couldn't optimize the
  // function. Just return and continue in the unoptimized version.
  Label skip;
  __ CmpSmiLiteral(r3_p, Smi::FromInt(-1), r0_p);
  __ bne(&skip);
  __ Ret();

  __ bind(&skip);
  // Untag the AST id and push it on the stack.
  __ SmiUntag(r3_p);
  __ push(r3_p);

  // Generate the code for doing the frame-to-frame translation using
  // the deoptimizer infrastructure.
  Deoptimizer::EntryGenerator generator(masm, Deoptimizer::OSR);
  generator.Generate();
}


void Builtins::Generate_FunctionCall(MacroAssembler* masm) {
  // 1. Make sure we have at least one argument.
  // r3_p: actual number of arguments
  { Label done;
    __ Cmpi(r3_p, Operand::Zero());
    __ bne(&done);
    __ LoadRoot(r5_p, Heap::kUndefinedValueRootIndex);
    __ push(r5_p);
    __ AddP(r3_p, Operand(1));
    __ bind(&done);
  }

  // 2. Get the function to call (passed as receiver) from the stack, check
  //    if it is a function.
  // r3_p: actual number of arguments
  Label slow, non_function;
  __ ShiftLeftImm(r4_p, r3_p, Operand(kPointerSizeLog2));
  __ AddP(r4_p, sp);
  __ LoadP(r4_p, MemOperand(r4_p));
  __ JumpIfSmi(r4_p, &non_function);
  __ CompareObjectType(r4_p, r5_p, r5_p, JS_FUNCTION_TYPE);
  __ bne(&slow);

  // 3a. Patch the first argument if necessary when calling a function.
  // r3_p: actual number of arguments
  // r4_p: function
  Label shift_arguments;
  __ LoadImmP(r7_p, Operand(0, RelocInfo::NONE));  // indicate regular
                                                   // JS_FUNCTION
  { Label convert_to_object, use_global_receiver, patch_receiver;
    // Change context eagerly in case we need the global receiver.
    __ LoadP(cp, FieldMemOperand(r4_p, JSFunction::kContextOffset));

    // Do not transform the receiver for strict mode functions.
    __ LoadP(r5_p,
             FieldMemOperand(r4_p, JSFunction::kSharedFunctionInfoOffset));
    __ LoadlW(r6_p,
              FieldMemOperand(r5_p, SharedFunctionInfo::kCompilerHintsOffset));
    __ TestBit(r6_p,
#if V8_TARGET_ARCH_S390X
               SharedFunctionInfo::kStrictModeFunction,
#else
               SharedFunctionInfo::kStrictModeFunction + kSmiTagSize,
#endif
               r0_p);
    __ bne(&shift_arguments /*, cr0*/);

    // Do not transform the receiver for native (Compilerhints already in r6_p).
    __ TestBit(r6_p,
#if V8_TARGET_ARCH_S390X
               SharedFunctionInfo::kNative,
#else
               SharedFunctionInfo::kNative + kSmiTagSize,
#endif
               r0_p);
    __ bne(&shift_arguments /*, cr0*/);

    // Compute the receiver in non-strict mode.
    __ ShiftLeftImm(ip, r3_p, Operand(kPointerSizeLog2));
    __ LoadRR(r5_p, sp);
    __ AddP(r5_p, ip);
    __ LoadP(r5_p, MemOperand(r5_p, -kPointerSize));
    // r3_p: actual number of arguments
    // r4_p: function
    // r5_p: first argument
    __ JumpIfSmi(r5_p, &convert_to_object);

    __ LoadRoot(r6_p, Heap::kUndefinedValueRootIndex);
    __ CmpRR(r5_p, r6_p);
    __ beq(&use_global_receiver);
    __ LoadRoot(r6_p, Heap::kNullValueRootIndex);
    __ CmpRR(r5_p, r6_p);
    __ beq(&use_global_receiver);

    STATIC_ASSERT(LAST_SPEC_OBJECT_TYPE == LAST_TYPE);
    __ CompareObjectType(r5_p, r6_p, r6_p, FIRST_SPEC_OBJECT_TYPE);
    __ bge(&shift_arguments);

    __ bind(&convert_to_object);

    {
      // Enter an internal frame in order to preserve argument count.
      FrameScope scope(masm, StackFrame::INTERNAL);
      __ SmiTag(r3_p);
      __ push(r3_p);

      __ push(r5_p);
      __ InvokeBuiltin(Builtins::TO_OBJECT, CALL_FUNCTION);
      __ LoadRR(r5_p, r3_p);

      __ pop(r3_p);
      __ SmiUntag(r3_p);

      // Exit the internal frame.
    }

    // Restore the function to r4_p, and the flag to r7_p.
    __ ShiftLeftImm(r7_p, r3_p, Operand(kPointerSizeLog2));
    __ AddP(r7_p, sp);
    __ LoadP(r4_p, MemOperand(r7_p));
    __ LoadImmP(r7_p, Operand(0, RelocInfo::NONE));
    __ b(&patch_receiver);

    // Use the global receiver object from the called function as the
    // receiver.
    __ bind(&use_global_receiver);
    const int kGlobalIndex =
        Context::kHeaderSize + Context::GLOBAL_OBJECT_INDEX * kPointerSize;
    __ LoadP(r5_p, FieldMemOperand(cp, kGlobalIndex));
    __ LoadP(r5_p, FieldMemOperand(r5_p, GlobalObject::kNativeContextOffset));
    __ LoadP(r5_p, FieldMemOperand(r5_p, kGlobalIndex));
    __ LoadP(r5_p, FieldMemOperand(r5_p, GlobalObject::kGlobalReceiverOffset));

    __ bind(&patch_receiver);
    __ ShiftLeftImm(ip, r3_p, Operand(kPointerSizeLog2));
    __ LoadRR(r6_p, sp);
    __ AddP(r6_p, ip);
    __ StoreP(r5_p, MemOperand(r6_p, -kPointerSize));

    __ b(&shift_arguments);
  }

  // 3b. Check for function proxy.
  __ bind(&slow);
  __ LoadImmP(r7_p, Operand(1, RelocInfo::NONE));  // indicate function proxy
  __ Cmpi(r5_p, Operand(JS_FUNCTION_PROXY_TYPE));
  __ beq(&shift_arguments);
  __ bind(&non_function);
  __ LoadImmP(r7_p, Operand(2, RelocInfo::NONE));  // indicate non-function

  // 3c. Patch the first argument when calling a non-function.  The
  //     CALL_NON_FUNCTION builtin expects the non-function callee as
  //     receiver, so overwrite the first argument which will ultimately
  //     become the receiver.
  // r3_p: actual number of arguments
  // r4_p: function
  // r7_p: call type (0: JS function, 1: function proxy, 2: non-function)
  __ ShiftLeftImm(ip, r3_p, Operand(kPointerSizeLog2));
  __ LoadRR(r5_p, sp);
  __ AddP(r5_p, ip);
  __ StoreP(r4_p, MemOperand(r5_p, -kPointerSize));

  // 4. Shift arguments and return address one slot down on the stack
  //    (overwriting the original receiver).  Adjust argument count to make
  //    the original first argument the new receiver.
  // r3_p: actual number of arguments
  // r4_p: function
  // r7_p: call type (0: JS function, 1: function proxy, 2: non-function)
  __ bind(&shift_arguments);
  { Label loop;
    // Calculate the copy start address (destination). Copy end address is sp.
    __ ShiftLeftImm(ip, r3_p, Operand(kPointerSizeLog2));
    __ LoadRR(r5_p, sp);
    __ AddP(r5_p, ip);

    __ bind(&loop);
    __ LoadP(ip, MemOperand(r5_p, -kPointerSize));
    __ StoreP(ip, MemOperand(r5_p));
    __ Sub(r5_p, Operand(kPointerSize));
    __ CmpRR(r5_p, sp);
    __ bne(&loop);
    // Adjust the actual number of arguments and remove the top element
    // (which is a copy of the last argument).
    __ Sub(r3_p, Operand(1));
    __ pop();
  }

  // 5a. Call non-function via tail call to CALL_NON_FUNCTION builtin,
  //     or a function proxy via CALL_FUNCTION_PROXY.
  // r3_p: actual number of arguments
  // r4_p: function
  // r7_p: call type (0: JS function, 1: function proxy, 2: non-function)
  { Label function, non_proxy;
    __ Cmpi(r7_p, Operand::Zero());
    __ beq(&function);
    // Expected number of arguments is 0 for CALL_NON_FUNCTION.
    __ LoadImmP(r5_p, Operand(0, RelocInfo::NONE));
    __ SetCallKind(r8_p, CALL_AS_METHOD);
    __ Cmpi(r7_p, Operand(1));
    __ bne(&non_proxy);

    __ push(r4_p);  // re-add proxy object as additional argument
    __ AddP(r3_p, Operand(1));
    __ GetBuiltinEntry(r6_p, Builtins::CALL_FUNCTION_PROXY);
    __ Jump(masm->isolate()->builtins()->ArgumentsAdaptorTrampoline(),
            RelocInfo::CODE_TARGET);

    __ bind(&non_proxy);
    __ GetBuiltinEntry(r6_p, Builtins::CALL_NON_FUNCTION);
    __ Jump(masm->isolate()->builtins()->ArgumentsAdaptorTrampoline(),
            RelocInfo::CODE_TARGET);
    __ bind(&function);
  }

  // 5b. Get the code to call from the function and check that the number of
  //     expected arguments matches what we're providing.  If so, jump
  //     (tail-call) to the code in register edx without checking arguments.
  // r3_p: actual number of arguments
  // r4_p: function
  __ LoadP(r6_p, FieldMemOperand(r4_p, JSFunction::kSharedFunctionInfoOffset));
  __ LoadW(r5_p, FieldMemOperand(r9_p,
           SharedFunctionInfo::kFormalParameterCountOffset));
#if !defined(V8_TARGET_ARCH_S390X)
  __ SmiUntag(r5_p);
#endif
  __ LoadP(r6_p, FieldMemOperand(r4_p, JSFunction::kCodeEntryOffset));
  __ SetCallKind(r8_p, CALL_AS_METHOD);
  __ CmpRR(r5_p, r3_p);  // Check formal and actual parameter counts.
  Label skip;
  __ beq(&skip);
  __ Jump(masm->isolate()->builtins()->ArgumentsAdaptorTrampoline(),
          RelocInfo::CODE_TARGET);

  __ bind(&skip);
  ParameterCount expected(0);
  __ InvokeCode(r6_p, expected, expected, JUMP_FUNCTION,
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

    __ LoadP(r3_p, MemOperand(fp, kFunctionOffset));  // get the function
    __ push(r3_p);
    __ LoadP(r3_p, MemOperand(fp, kArgsOffset));  // get the args array
    __ push(r3_p);
    __ InvokeBuiltin(Builtins::APPLY_PREPARE, CALL_FUNCTION);

    // Check the stack for overflow. We are not trying to catch
    // interruptions (e.g. debug break and preemption) here, so the "real stack
    // limit" is checked.
    Label okay;
    __ LoadRoot(r5_p, Heap::kRealStackLimitRootIndex);
    // Make r5_p the space we have left. The stack might already be overflowed
    // here which will cause r5_p to become negative.
    __ Sub(r5_p, sp, r5_p);
    // Check if the arguments will overflow the stack.
    __ SmiToPtrArrayOffset(r0_p, r3_p);
    __ CmpRR(r5_p, r0_p);
    __ bgt(&okay);  // Signed comparison.

    // Out of stack space.
    __ LoadP(r4_p, MemOperand(fp, kFunctionOffset));
    __ push(r4_p);
    __ push(r3_p);
    __ InvokeBuiltin(Builtins::APPLY_OVERFLOW, CALL_FUNCTION);
    // End of stack check.

    // Push current limit and index.
    __ bind(&okay);
    __ push(r3_p);  // limit
    __ LoadImmP(r4_p, Operand(0, RelocInfo::NONE));  // initial index
    __ push(r4_p);

    // Get the receiver.
    __ LoadP(r3_p, MemOperand(fp, kRecvOffset));

    // Check that the function is a JS function (otherwise it must be a proxy).
    Label push_receiver;
    __ LoadP(r4_p, MemOperand(fp, kFunctionOffset));
    __ CompareObjectType(r4_p, r5_p, r5_p, JS_FUNCTION_TYPE);
    __ bne(&push_receiver);

    // Change context eagerly to get the right global object if necessary.
    __ LoadP(cp, FieldMemOperand(r4_p, JSFunction::kContextOffset));
    // Load the shared function info while the function is still in r4_p.
    __ LoadP(r5_p,
             FieldMemOperand(r4_p, JSFunction::kSharedFunctionInfoOffset));

    // Compute the receiver.
    // Do not transform the receiver for strict mode functions.
    Label call_to_object, use_global_receiver;
    __ LoadlW(r5_p,
              FieldMemOperand(r5_p, SharedFunctionInfo::kCompilerHintsOffset));
    __ TestBit(r5_p,
#if V8_TARGET_ARCH_S390X
               SharedFunctionInfo::kStrictModeFunction,
#else
               SharedFunctionInfo::kStrictModeFunction + kSmiTagSize,
#endif
               r0_p);
    __ bne(&push_receiver /*, cr0*/);

    // Do not transform the receiver for strict mode functions.
    __ TestBit(r5_p,
#if V8_TARGET_ARCH_S390X
               SharedFunctionInfo::kNative,
#else
               SharedFunctionInfo::kNative + kSmiTagSize,
#endif
               r0_p);
    __ bne(&push_receiver /*, cr0*/);

    // Compute the receiver in non-strict mode.
    __ JumpIfSmi(r3_p, &call_to_object);
    __ LoadRoot(r4_p, Heap::kNullValueRootIndex);
    __ CmpRR(r3_p, r4_p);
    __ beq(&use_global_receiver);
    __ LoadRoot(r4_p, Heap::kUndefinedValueRootIndex);
    __ CmpRR(r3_p, r4_p);
    __ beq(&use_global_receiver);

    // Check if the receiver is already a JavaScript object.
    // r3_p: receiver
    STATIC_ASSERT(LAST_SPEC_OBJECT_TYPE == LAST_TYPE);
    __ CompareObjectType(r3_p, r4_p, r4_p, FIRST_SPEC_OBJECT_TYPE);
    __ bge(&push_receiver);

    // Convert the receiver to a regular object.
    // r3_p: receiver
    __ bind(&call_to_object);
    __ push(r3_p);
    __ InvokeBuiltin(Builtins::TO_OBJECT, CALL_FUNCTION);
    __ b(&push_receiver);

    // Use the current global receiver object as the receiver.
    __ bind(&use_global_receiver);
    const int kGlobalOffset =
        Context::kHeaderSize + Context::GLOBAL_OBJECT_INDEX * kPointerSize;
    __ LoadP(r3_p, FieldMemOperand(cp, kGlobalOffset));
    __ LoadP(r3_p, FieldMemOperand(r3_p, GlobalObject::kNativeContextOffset));
    __ LoadP(r3_p, FieldMemOperand(r3_p, kGlobalOffset));
    __ LoadP(r3_p, FieldMemOperand(r3_p, GlobalObject::kGlobalReceiverOffset));

    // Push the receiver.
    // r3_p: receiver
    __ bind(&push_receiver);
    __ push(r3_p);

    // Copy all arguments from the array to the stack.
    Label entry, loop;
    __ LoadP(r3_p, MemOperand(fp, kIndexOffset));
    __ b(&entry);

    // Load the current argument from the arguments array and push it to the
    // stack.
    // r3_p: current argument index
    __ bind(&loop);
    __ LoadP(r4_p, MemOperand(fp, kArgsOffset));
    __ push(r4_p);
    __ push(r3_p);

    // Call the runtime to access the property in the arguments array.
    __ CallRuntime(Runtime::kGetProperty, 2);
    __ push(r3_p);

    // Use inline caching to access the arguments.
    __ LoadP(r3_p, MemOperand(fp, kIndexOffset));
    __ AddSmiLiteral(r3_p, r3_p, Smi::FromInt(1), r0_p);
    __ StoreP(r3_p, MemOperand(fp, kIndexOffset));

    // Test if the copy loop has finished copying all the elements from the
    // arguments object.
    __ bind(&entry);
    __ LoadP(r4_p, MemOperand(fp, kLimitOffset));
    __ CmpRR(r3_p, r4_p);
    __ bne(&loop);

    // Invoke the function.
    Label call_proxy;
    ParameterCount actual(r3_p);
    __ SmiUntag(r3_p);
    __ LoadP(r4_p, MemOperand(fp, kFunctionOffset));
    __ CompareObjectType(r4_p, r5_p, r5_p, JS_FUNCTION_TYPE);
    __ bne(&call_proxy);
    __ InvokeFunction(r4_p, actual, CALL_FUNCTION,
                      NullCallWrapper(), CALL_AS_METHOD);

    frame_scope.GenerateLeaveFrame();
    __ AddP(sp, Operand(3 * kPointerSize));
    __ Ret();

    // Invoke the function proxy.
    __ bind(&call_proxy);
    __ push(r4_p);  // add function proxy as last argument
    __ AddP(r3_p, Operand(1));
    __ LoadImmP(r5_p, Operand(0, RelocInfo::NONE));
    __ SetCallKind(r8_p, CALL_AS_METHOD);
    __ GetBuiltinEntry(r6_p, Builtins::CALL_FUNCTION_PROXY);
    __ Call(masm->isolate()->builtins()->ArgumentsAdaptorTrampoline(),
            RelocInfo::CODE_TARGET);

    // Tear down the internal frame and remove function, receiver and args.
  }
  __ AddP(sp, Operand(3 * kPointerSize));
  __ Ret();
}

static void EnterArgumentsAdaptorFrame(MacroAssembler* masm) {
  __ SmiTag(r2);
  // @TODO Make sure r7 is the correct register to use here for S390
  __ LoadSmiLiteral(r7, Smi::FromInt(StackFrame::ARGUMENTS_ADAPTOR));
  // Stack updated as such:
  //    old SP --->
  //                 R14 Return Addr
  //                 Old FP                     <--- New FP
  //                 Argument Adapter SMI
  //                 Function
  //                 ArgC as SMI                <--- New SP
  __ lay(sp, MemOperand(sp, -5 * kPointerSize));
  __ StoreP(r14, MemOperand(sp, 4 * kPointerSize));
  __ StoreP(fp, MemOperand(sp, 3 * kPointerSize));
  __ StoreP(r7, MemOperand(sp, 2 * kPointerSize));
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
  //  -- r3_p : actual number of arguments
  //  -- r4_p : function (passed through to callee)
  //  -- r5_p : expected number of arguments
  //  -- r6_p : code entry to call
  //  -- r8_p : call kind information
  // -----------------------------------

  Label invoke, dont_adapt_arguments;

  Label enough, too_few;
  __ CmpRR(r3_p, r5_p);
  __ blt(&too_few);
  __ Cmpi(r5_p, Operand(SharedFunctionInfo::kDontAdaptArgumentsSentinel));
  __ beq(&dont_adapt_arguments);

  {  // Enough parameters: actual >= expected
    __ bind(&enough);
    EnterArgumentsAdaptorFrame(masm);

    // Calculate copy start address into r3_p and copy end address into r5_p.
    // r3_p: actual number of arguments as a smi
    // r4_p: function
    // r5_p: expected number of arguments
    // r6_p: code entry to call
    __ SmiToPtrArrayOffset(r3_p, r3_p);
    __ AddP(r3_p, fp);
    // adjust for return address and receiver
    __ AddP(r3_p, Operand(2 * kPointerSize));
    __ ShiftLeftImm(r5_p, r5_p, Operand(kPointerSizeLog2));
    __ Sub(r5_p, r3_p, r5_p);

    // Copy the arguments (including the receiver) to the new stack frame.
    // r3_p: copy start address
    // r4_p: function
    // r5_p: copy end address
    // r6_p: code entry to call

    Label copy;
    __ bind(&copy);
    __ LoadP(ip, MemOperand(r3_p, 0));
    __ push(ip);
    __ CmpRR(r3_p, r5_p);  // Compare before moving to next argument.
    __ Sub(r3_p, Operand(kPointerSize));
    __ bne(&copy);

    __ b(&invoke);
  }

  {  // Too few parameters: Actual < expected
    __ bind(&too_few);
    EnterArgumentsAdaptorFrame(masm);

    // Calculate copy start address into r0_p and copy end address is fp.
    // r3_p: actual number of arguments as a smi
    // r4_p: function
    // r5_p: expected number of arguments
    // r6_p: code entry to call
    __ SmiToPtrArrayOffset(r3_p, r3_p);
    __ AddP(r3_p, fp);

    // Copy the arguments (including the receiver) to the new stack frame.
    // r3_p: copy start address
    // r4_p: function
    // r5_p: expected number of arguments
    // r6_p: code entry to call
    Label copy;
    __ bind(&copy);
    // Adjust load for return address and receiver.
    __ LoadP(ip, MemOperand(r3_p, 2 * kPointerSize));
    __ push(ip);
    __ CmpRR(r3_p, fp);  // Compare before moving to next argument.
    __ Sub(r3_p, Operand(kPointerSize));
    __ bne(&copy);

    // Fill the remaining expected arguments with undefined.
    // r4_p: function
    // r5_p: expected number of arguments
    // r6_p: code entry to call
    __ LoadRoot(ip, Heap::kUndefinedValueRootIndex);
    __ ShiftLeftImm(r5_p, r5_p, Operand(kPointerSizeLog2));
    __ Sub(r5_p, fp, r5_p);
    __ Sub(r5_p, Operand(4 * kPointerSize));  // Adjust for frame.

    Label fill;
    __ bind(&fill);
    __ push(ip);
    __ CmpRR(sp, r5_p);
    __ bne(&fill);
  }

  // Call the entry point.
  __ bind(&invoke);
  __ Call(r6_p);

  // Store offset of return address for deoptimizer.
  masm->isolate()->heap()->SetArgumentsAdaptorDeoptPCOffset(masm->pc_offset());

  // Exit frame and return.
  LeaveArgumentsAdaptorFrame(masm);
  __ Ret();


  // -------------------------------------------
  // Dont adapt arguments.
  // -------------------------------------------
  __ bind(&dont_adapt_arguments);
  __ Jump(r6_p);
}


#undef __

} }  // namespace v8::internal

#endif  // V8_TARGET_ARCH_S390
