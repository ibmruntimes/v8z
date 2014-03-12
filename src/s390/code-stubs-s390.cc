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

#include "bootstrapper.h"
#include "code-stubs.h"
#include "regexp-macro-assembler.h"
#include "s390/regexp-macro-assembler-s390.h"

namespace v8 {
namespace internal {


#define __ ACCESS_MASM(masm)

static void EmitIdenticalObjectComparison(MacroAssembler* masm,
                                          Label* slow,
                                          Condition cond,
                                          bool never_nan_nan);
static void EmitSmiNonsmiComparison(MacroAssembler* masm,
                                    Register lhs,
                                    Register rhs,
                                    Label* lhs_not_nan,
                                    Label* slow,
                                    bool strict);
static void EmitStrictTwoHeapObjectCompare(MacroAssembler* masm,
                                           Register lhs,
                                           Register rhs);


// Check if the operand is a heap number.
static void EmitCheckForHeapNumber(MacroAssembler* masm, Register operand,
                                   Register scratch1, Register scratch2,
                                   Label* not_a_heap_number) {
  __ LoadP(scratch1, FieldMemOperand(operand, HeapObject::kMapOffset));
  __ LoadRoot(scratch2, Heap::kHeapNumberMapRootIndex);
  __ CmpRR(scratch1, scratch2);
  __ bne(not_a_heap_number);
}


void ToNumberStub::Generate(MacroAssembler* masm) {
  // The ToNumber stub takes one argument in eax.
  Label check_heap_number, call_builtin;
  __ JumpIfNotSmi(r3_p, &check_heap_number);
  __ Ret();

  __ bind(&check_heap_number);
  EmitCheckForHeapNumber(masm, r3_p, r4_p, ip, &call_builtin);
  __ Ret();

  __ bind(&call_builtin);
  __ push(r3_p);
  __ InvokeBuiltin(Builtins::TO_NUMBER, JUMP_FUNCTION);
}


void FastNewClosureStub::Generate(MacroAssembler* masm) {
  // Create a new closure from the given function info in new
  // space. Set the context to the current context in cp.
  Counters* counters = masm->isolate()->counters();

  Label gc;

  // Pop the function info from the stack.
  __ pop(r6_p);

  // Attempt to allocate new JSFunction in new space.
  __ AllocateInNewSpace(JSFunction::kSize,
                        r3_p,
                        r4_p,
                        r5_p,
                        &gc,
                        TAG_OBJECT);

  __ IncrementCounter(counters->fast_new_closure_total(), 1, r9_p, r10_p);

  int map_index = (language_mode_ == CLASSIC_MODE)
      ? Context::FUNCTION_MAP_INDEX
      : Context::STRICT_MODE_FUNCTION_MAP_INDEX;

  // Compute the function map in the current native context and set that
  // as the map of the allocated object.
  __ LoadP(r5_p,
           MemOperand(cp, Context::SlotOffset(Context::GLOBAL_OBJECT_INDEX)));
  __ LoadP(r5_p, FieldMemOperand(r5_p, GlobalObject::kNativeContextOffset));
  __ LoadP(r8_p, MemOperand(r5_p, Context::SlotOffset(map_index)));
  __ StoreP(r8_p, FieldMemOperand(r3_p, HeapObject::kMapOffset));

  // Initialize the rest of the function. We don't have to update the
  // write barrier because the allocated object is in new space.
  __ LoadRoot(r4_p, Heap::kEmptyFixedArrayRootIndex);
  __ LoadRoot(r8_p, Heap::kTheHoleValueRootIndex);
  __ StoreP(r4_p, FieldMemOperand(r3_p, JSObject::kPropertiesOffset));
  __ StoreP(r4_p, FieldMemOperand(r3_p, JSObject::kElementsOffset));
  __ StoreP(r8_p, FieldMemOperand(r3_p,
            JSFunction::kPrototypeOrInitialMapOffset));
  __ StoreP(r6_p, FieldMemOperand(r3_p, JSFunction::kSharedFunctionInfoOffset));
  __ StoreP(cp, FieldMemOperand(r3_p, JSFunction::kContextOffset));
  __ StoreP(r4_p, FieldMemOperand(r3_p, JSFunction::kLiteralsOffset));

  // Initialize the code pointer in the function to be the one
  // found in the shared function info object.
  // But first check if there is an optimized version for our context.
  Label check_optimized;
  Label install_unoptimized;
  if (FLAG_cache_optimized_code) {
    __ LoadP(r4_p,
            FieldMemOperand(r6_p, SharedFunctionInfo::kOptimizedCodeMapOffset));
    __ Cmpi(r4_p, Operand::Zero());
    __ bne(&check_optimized);
  }
  __ bind(&install_unoptimized);
  __ LoadRoot(r7_p, Heap::kUndefinedValueRootIndex);
  __ StoreP(r7_p, FieldMemOperand(r3_p, JSFunction::kNextFunctionLinkOffset));
  __ LoadP(r6_p, FieldMemOperand(r6_p, SharedFunctionInfo::kCodeOffset));
  __ AddP(r6_p, Operand(Code::kHeaderSize - kHeapObjectTag));
  __ StoreP(r6_p, FieldMemOperand(r3_p, JSFunction::kCodeEntryOffset));

  // Return result. The argument function info has been popped already.
  __ Ret();

  __ bind(&check_optimized);

  __ IncrementCounter(counters->fast_new_closure_try_optimized(), 1, r9_p,
                      r10_p);

  // r5_p holds native context, r4_p points to fixed array of 3-element entries
  // (native context, optimized code, literals).
  // The optimized code map must never be empty, so check the first elements.
  Label install_optimized;
  // Speculatively move code object into r7_p
  __ LoadP(r7_p, FieldMemOperand(r4_p, FixedArray::kHeaderSize + kPointerSize));
  __ LoadP(r8_p, FieldMemOperand(r4_p, FixedArray::kHeaderSize));
  __ CmpRR(r5_p, r8_p);
  __ beq(&install_optimized);

  // Iterate through the rest of map backwards.  r7_p holds an index as a Smi.
  Label loop;
  __ LoadP(r7_p, FieldMemOperand(r4_p, FixedArray::kLengthOffset));
  __ bind(&loop);
  // Do not double check first entry.

  __ CmpSmiLiteral(r7_p, Smi::FromInt(SharedFunctionInfo::kEntryLength), r0_p);
  __ beq(&install_unoptimized);
  // Skip an entry.
  __ SubSmiLiteral(r7_p, r7_p, Smi::FromInt(SharedFunctionInfo::kEntryLength),
                   r0_p);
  __ LoadRR(r8_p, r4_p);
  __ AddP(r8_p, Operand(FixedArray::kHeaderSize - kHeapObjectTag));
  __ SmiToPtrArrayOffset(r9_p, r7_p);
  __ LoadP(r8_p, MemOperand(r8_p, r9_p));
  __ CmpRR(r5_p, r8_p);
  __ bne(&loop);
  // Hit: fetch the optimized code.
  // TODO(penguin): potential to use x-form for this sequence
  __ LoadRR(r8_p, r4_p);
  __ AddP(r8_p, Operand(FixedArray::kHeaderSize - kHeapObjectTag));
  __ SmiToPtrArrayOffset(r9_p, r7_p);
  __ AddP(r8_p, r9_p);
  __ LoadPU(r7_p, MemOperand(r8_p, kPointerSize));

  __ bind(&install_optimized);
  __ IncrementCounter(counters->fast_new_closure_install_optimized(),
                      1, r9_p, r10_p);

  // TODO(fschneider): Idea: store proper code pointers in the map and either
  // unmangle them on marking or do nothing as the whole map is discarded on
  // major GC anyway.
  __ AddP(r7_p, Operand(Code::kHeaderSize - kHeapObjectTag));
  __ StoreP(r7_p, FieldMemOperand(r3_p, JSFunction::kCodeEntryOffset));

  // Now link a function into a list of optimized functions.
  __ LoadP(r7_p, ContextOperand(r5_p, Context::OPTIMIZED_FUNCTIONS_LIST));

  __ StoreP(r7_p, FieldMemOperand(r3_p, JSFunction::kNextFunctionLinkOffset));
  // No need for write barrier as JSFunction (eax) is in the new space.

  __ StoreP(r3_p, ContextOperand(r5_p, Context::OPTIMIZED_FUNCTIONS_LIST));
  // Store JSFunction (eax) into edx before issuing write barrier as
  // it clobbers all the registers passed.
  __ LoadRR(r7_p, r3_p);
  __ RecordWriteContextSlot(
      r5_p,
      Context::SlotOffset(Context::OPTIMIZED_FUNCTIONS_LIST),
      r7_p,
      r4_p,
      kLRHasNotBeenSaved,
      kDontSaveFPRegs);

  // Return result. The argument function info has been popped already.
  __ Ret();

  // Create a new closure through the slower runtime call.
  __ bind(&gc);
  __ LoadRoot(r7_p, Heap::kFalseValueRootIndex);
  __ Push(cp, r6_p, r7_p);
  __ TailCallRuntime(Runtime::kNewClosure, 3, 1);
}


void FastNewContextStub::Generate(MacroAssembler* masm) {
  // Try to allocate the context in new space.
  Label gc;
  int length = slots_ + Context::MIN_CONTEXT_SLOTS;

  // Attempt to allocate the context in new space.
  __ AllocateInNewSpace(FixedArray::SizeFor(length),
                        r3_p,
                        r4_p,
                        r5_p,
                        &gc,
                        TAG_OBJECT);

  // Load the function from the stack.
  __ LoadP(r6_p, MemOperand(sp, 0));

  // Set up the object header.
  __ LoadRoot(r4_p, Heap::kFunctionContextMapRootIndex);
  __ LoadSmiLiteral(r5_p, Smi::FromInt(length));
  __ StoreP(r5_p, FieldMemOperand(r3_p, FixedArray::kLengthOffset));
  __ StoreP(r4_p, FieldMemOperand(r3_p, HeapObject::kMapOffset));

  // Set up the fixed slots, copy the global object from the previous context.
  __ LoadP(r5_p,
           MemOperand(cp, Context::SlotOffset(Context::GLOBAL_OBJECT_INDEX)));
  __ LoadSmiLiteral(r4_p, Smi::FromInt(0));
  __ StoreP(r6_p,
            MemOperand(r3_p, Context::SlotOffset(Context::CLOSURE_INDEX)));
  __ StoreP(cp, MemOperand(r3_p, Context::SlotOffset(Context::PREVIOUS_INDEX)));
  __ StoreP(r4_p,
            MemOperand(r3_p, Context::SlotOffset(Context::EXTENSION_INDEX)));
  __ StoreP(r5_p,
           MemOperand(r3_p, Context::SlotOffset(Context::GLOBAL_OBJECT_INDEX)));

  // Initialize the rest of the slots to undefined.
  __ LoadRoot(r4_p, Heap::kUndefinedValueRootIndex);
  for (int i = Context::MIN_CONTEXT_SLOTS; i < length; i++) {
    __ StoreP(r4_p, MemOperand(r3_p, Context::SlotOffset(i)));
  }

  // Remove the on-stack argument and return.
  __ LoadRR(cp, r3_p);
  __ pop();
  __ Ret();

  // Need to collect. Call into runtime system.
  __ bind(&gc);
  __ TailCallRuntime(Runtime::kNewFunctionContext, 1, 1);
}


void FastNewBlockContextStub::Generate(MacroAssembler* masm) {
  // Stack layout on entry:
  //
  // [sp]: function.
  // [sp + kPointerSize]: serialized scope info

  // Try to allocate the context in new space.
  Label gc;
  int length = slots_ + Context::MIN_CONTEXT_SLOTS;
  __ AllocateInNewSpace(FixedArray::SizeFor(length),
                        r3_p, r4_p, r5_p, &gc, TAG_OBJECT);

  // Load the function from the stack.
  __ LoadP(r6_p, MemOperand(sp, 0));

  // Load the serialized scope info from the stack.
  __ LoadP(r4_p, MemOperand(sp, 1 * kPointerSize));

  // Set up the object header.
  __ LoadRoot(r5_p, Heap::kBlockContextMapRootIndex);
  __ StoreP(r5_p, FieldMemOperand(r3_p, HeapObject::kMapOffset));
  __ LoadSmiLiteral(r5_p, Smi::FromInt(length));
  __ StoreP(r5_p, FieldMemOperand(r3_p, FixedArray::kLengthOffset));

  // If this block context is nested in the native context we get a smi
  // sentinel instead of a function. The block context should get the
  // canonical empty function of the native context as its closure which
  // we still have to look up.
  Label after_sentinel;
  __ JumpIfNotSmi(r6_p, &after_sentinel);
  if (FLAG_debug_code) {
    const char* message = "Expected 0 as a Smi sentinel";
    __ Cmpi(r6_p, Operand::Zero());
    __ Assert(eq, message);
  }
  __ LoadP(r6_p, GlobalObjectOperand());
  __ LoadP(r6_p, FieldMemOperand(r6_p, GlobalObject::kNativeContextOffset));
  __ LoadP(r6_p, ContextOperand(r6_p, Context::CLOSURE_INDEX));
  __ bind(&after_sentinel);

  // Set up the fixed slots, copy the global object from the previous context.
  __ LoadP(r5_p, ContextOperand(cp, Context::GLOBAL_OBJECT_INDEX));
  __ StoreP(r6_p, ContextOperand(r3_p, Context::CLOSURE_INDEX));
  __ StoreP(cp, ContextOperand(r3_p, Context::PREVIOUS_INDEX));
  __ StoreP(r4_p, ContextOperand(r3_p, Context::EXTENSION_INDEX));
  __ StoreP(r5_p, ContextOperand(r3_p, Context::GLOBAL_OBJECT_INDEX));

  // Initialize the rest of the slots to the hole value.
  __ LoadRoot(r4_p, Heap::kTheHoleValueRootIndex);
  for (int i = 0; i < slots_; i++) {
    __ StoreP(r4_p, ContextOperand(r3_p, i + Context::MIN_CONTEXT_SLOTS));
  }

  // Remove the on-stack argument and return.
  __ LoadRR(cp, r3_p);
  __ AddP(sp, Operand(2 * kPointerSize));
  __ Ret();

  // Need to collect. Call into runtime system.
  __ bind(&gc);
  __ TailCallRuntime(Runtime::kPushBlockContext, 2, 1);
}


static void GenerateFastCloneShallowArrayCommon(
    MacroAssembler* masm,
    int length,
    FastCloneShallowArrayStub::Mode mode,
    Label* fail) {
  // Registers on entry:
  //
  // r6_p: boilerplate literal array.
  ASSERT(mode != FastCloneShallowArrayStub::CLONE_ANY_ELEMENTS);

  // All sizes here are multiples of kPointerSize.
  int elements_size = 0;
  if (length > 0) {
    elements_size = mode == FastCloneShallowArrayStub::CLONE_DOUBLE_ELEMENTS
        ? FixedDoubleArray::SizeFor(length)
        : FixedArray::SizeFor(length);
  }
  int size = JSArray::kSize + elements_size;

  // Allocate both the JS array and the elements array in one big
  // allocation. This avoids multiple limit checks.
  __ AllocateInNewSpace(size,
                        r3_p,
                        r4_p,
                        r5_p,
                        fail,
                        TAG_OBJECT);

  // Copy the JS array part.
  for (int i = 0; i < JSArray::kSize; i += kPointerSize) {
    if ((i != JSArray::kElementsOffset) || (length == 0)) {
      __ LoadP(r4_p, FieldMemOperand(r6_p, i));
      __ StoreP(r4_p, FieldMemOperand(r3_p, i));
    }
  }

  if (length > 0) {
    // Get hold of the elements array of the boilerplate and setup the
    // elements pointer in the resulting object.
    __ LoadP(r6_p, FieldMemOperand(r6_p, JSArray::kElementsOffset));
    __ LoadRR(r5_p, r3_p);
    __ AddP(r5_p, Operand(JSArray::kSize));
    __ StoreP(r5_p, FieldMemOperand(r3_p, JSArray::kElementsOffset));

    // Copy the elements array.
    ASSERT((elements_size % kPointerSize) == 0);
    __ CopyFields(r5_p, r6_p, r4_p.bit(), elements_size / kPointerSize);
  }
}

void FastCloneShallowArrayStub::Generate(MacroAssembler* masm) {
  // Stack layout on entry:
  //
  // [sp]: constant elements.
  // [sp + kPointerSize]: literal index.
  // [sp + (2 * kPointerSize)]: literals array.

  // Load boilerplate object into r3_p and check if we need to create a
  // boilerplate.
  Label slow_case;
  __ LoadP(r6_p, MemOperand(sp, 2 * kPointerSize));
  __ LoadP(r3_p, MemOperand(sp, 1 * kPointerSize));
  __ AddP(r6_p, Operand(FixedArray::kHeaderSize - kHeapObjectTag));

  __ LoadRR(r0_p, r3_p);
  __ SmiToPtrArrayOffset(r3_p, r3_p);
  __ LoadP(r6_p, MemOperand(r6_p, r3_p));
  __ LoadRR(r3_p, r0_p);

  __ CompareRoot(r6_p, Heap::kUndefinedValueRootIndex);
  __ beq(&slow_case);

  FastCloneShallowArrayStub::Mode mode = mode_;
  if (mode == CLONE_ANY_ELEMENTS) {
    Label double_elements, check_fast_elements;
    __ LoadP(r3_p, FieldMemOperand(r6_p, JSArray::kElementsOffset));
    __ LoadP(r3_p, FieldMemOperand(r3_p, HeapObject::kMapOffset));
    __ CompareRoot(r3_p, Heap::kFixedCOWArrayMapRootIndex);
    __ bne(&check_fast_elements);
    GenerateFastCloneShallowArrayCommon(masm, 0,
                                        COPY_ON_WRITE_ELEMENTS, &slow_case);
    // Return and remove the on-stack parameters.
    __ AddP(sp, Operand(3 * kPointerSize));
    __ Ret();

    __ bind(&check_fast_elements);
    __ CompareRoot(r3_p, Heap::kFixedArrayMapRootIndex);
    __ bne(&double_elements);
    GenerateFastCloneShallowArrayCommon(masm, length_,
                                        CLONE_ELEMENTS, &slow_case);
    // Return and remove the on-stack parameters.
    __ AddP(sp, Operand(3 * kPointerSize));
    __ Ret();

    __ bind(&double_elements);
    mode = CLONE_DOUBLE_ELEMENTS;
    // Fall through to generate the code to handle double elements.
  }

  if (FLAG_debug_code) {
    const char* message;
    Heap::RootListIndex expected_map_index;
    if (mode == CLONE_ELEMENTS) {
      message = "Expected (writable) fixed array";
      expected_map_index = Heap::kFixedArrayMapRootIndex;
    } else if (mode == CLONE_DOUBLE_ELEMENTS) {
      message = "Expected (writable) fixed double array";
      expected_map_index = Heap::kFixedDoubleArrayMapRootIndex;
    } else {
      ASSERT(mode == COPY_ON_WRITE_ELEMENTS);
      message = "Expected copy-on-write fixed array";
      expected_map_index = Heap::kFixedCOWArrayMapRootIndex;
    }
    __ push(r6_p);
    __ LoadP(r6_p, FieldMemOperand(r6_p, JSArray::kElementsOffset));
    __ LoadP(r6_p, FieldMemOperand(r6_p, HeapObject::kMapOffset));
    __ CompareRoot(r6_p, expected_map_index);
    __ Assert(eq, message);
    __ pop(r6_p);
  }

  GenerateFastCloneShallowArrayCommon(masm, length_, mode, &slow_case);

  // Return and remove the on-stack parameters.
  __ AddP(sp, Operand(3 * kPointerSize));
  __ Ret();

  __ bind(&slow_case);
  __ TailCallRuntime(Runtime::kCreateArrayLiteralShallow, 3, 1);
}


void FastCloneShallowObjectStub::Generate(MacroAssembler* masm) {
  // Stack layout on entry:
  //
  // [sp]: object literal flags.
  // [sp + kPointerSize]: constant properties.
  // [sp + (2 * kPointerSize)]: literal index.
  // [sp + (3 * kPointerSize)]: literals array.

  // Load boilerplate object into r3_p and check if we need to create a
  // boilerplate.
  Label slow_case;
  __ LoadP(r6_p, MemOperand(sp, 3 * kPointerSize));
  __ LoadP(r3_p, MemOperand(sp, 2 * kPointerSize));
  __ AddP(r6_p, Operand(FixedArray::kHeaderSize - kHeapObjectTag));
  __ LoadRR(r0_p, r3_p);
  __ SmiToPtrArrayOffset(r3_p, r3_p);
  __ LoadP(r6_p, MemOperand(r6_p, r3_p));
  __ LoadRR(r3_p, r0_p);

  __ CompareRoot(r6_p, Heap::kUndefinedValueRootIndex);
  __ beq(&slow_case);

  // Check that the boilerplate contains only fast properties and we can
  // statically determine the instance size.
  int size = JSObject::kHeaderSize + length_ * kPointerSize;
  __ LoadP(r3_p, FieldMemOperand(r6_p, HeapObject::kMapOffset));
  __ LoadlB(r3_p, FieldMemOperand(r3_p, Map::kInstanceSizeOffset));
  __ Cmpi(r3_p, Operand(size >> kPointerSizeLog2));
  __ bne(&slow_case);

  // Allocate the JS object and copy header together with all in-object
  // properties from the boilerplate.
  __ AllocateInNewSpace(size, r3_p, r4_p, r5_p, &slow_case, TAG_OBJECT);
  for (int i = 0; i < size; i += kPointerSize) {
    __ LoadP(r4_p, FieldMemOperand(r6_p, i));
    __ StoreP(r4_p, FieldMemOperand(r3_p, i));
  }

  // Return and remove the on-stack parameters.
  __ AddP(sp, Operand(4 * kPointerSize));
  __ Ret();

  __ bind(&slow_case);
  __ TailCallRuntime(Runtime::kCreateObjectLiteralShallow, 4, 1);
}


// Takes a Smi and converts to an IEEE 64 bit floating point value in two
// registers.  The format is 1 sign bit, 11 exponent bits (biased 1023) and
// 52 fraction bits (20 in the first word, 32 in the second).  Zeros is a
// scratch register.  Destroys the source register.  No GC occurs during this
// stub so you don't have to set up the frame.
class ConvertToDoubleStub : public CodeStub {
 public:
  ConvertToDoubleStub(Register result_reg_1,
                      Register result_reg_2,
                      Register source_reg,
                      Register scratch_reg)
      : result1_(result_reg_1),
        result2_(result_reg_2),
        source_(source_reg),
        zeros_(scratch_reg) { }

 private:
  Register result1_;
  Register result2_;
  Register source_;
  Register zeros_;

  // Minor key encoding in 16 bits.
  class ModeBits: public BitField<OverwriteMode, 0, 2> {};
  class OpBits: public BitField<Token::Value, 2, 14> {};

  Major MajorKey() { return ConvertToDouble; }
  int MinorKey() {
    // Encode the parameters in a unique 16 bit value.
    return  result1_.code() +
           (result2_.code() << 4) +
           (source_.code() << 8) +
           (zeros_.code() << 12);
  }

  void Generate(MacroAssembler* masm);
};

void FloatingPointHelper::LoadSmis(MacroAssembler* masm,
                                   Register scratch1,
                                   Register scratch2) {
  __ SmiToDoubleFPRegister(r3_p, d2, scratch1);
  __ SmiToDoubleFPRegister(r4_p, d1, scratch1);
}

// needs cleanup for extra parameters that are unused
void FloatingPointHelper::LoadOperands(
    MacroAssembler* masm,
    Register heap_number_map,
    Register scratch1,
    Register scratch2,
    Label* slow) {
  // Load right operand (r3_p) to d2
  LoadNumber(masm, r3_p, d2, heap_number_map, scratch1, scratch2, slow);

  // Load left operand (r4_p) to d1
  LoadNumber(masm, r4_p, d1, heap_number_map, scratch1, scratch2, slow);
}

// needs cleanup for extra parameters that are unused
// also needs a scratch double register instead of d3
void FloatingPointHelper::LoadNumber(MacroAssembler* masm,
                                     Register object,
                                     DwVfpRegister dst,
                                     Register heap_number_map,
                                     Register scratch1,
                                     Register scratch2,
                                     Label* not_number) {
  __ AssertRootValue(heap_number_map,
                     Heap::kHeapNumberMapRootIndex,
                     "HeapNumberMap register clobbered.");

  Label is_smi, done;

  // Smi-check
  __ UntagAndJumpIfSmi(scratch1, object, &is_smi);
  // Heap number check
  __ JumpIfNotHeapNumber(object, heap_number_map, scratch1, not_number);

  // Handle loading a double from a heap number
  // Load the double from tagged HeapNumber to double register.
  __ LoadF(dst, FieldMemOperand(object, HeapNumber::kValueOffset));
  __ b(&done);

  // Handle loading a double from a smi.
  __ bind(&is_smi);

  // Convert untagged smi to double using FP instructions.
  FloatingPointHelper::ConvertIntToDouble(masm, scratch1, dst);

  __ bind(&done);
}


void FloatingPointHelper::ConvertNumberToInt32(MacroAssembler* masm,
                                               Register object,
                                               Register dst,
                                               Register heap_number_map,
                                               Register scratch1,
                                               Register scratch2,
                                               Register scratch3,
                                               DwVfpRegister double_scratch,
                                               Label* not_number) {
  __ AssertRootValue(heap_number_map,
                     Heap::kHeapNumberMapRootIndex,
                     "HeapNumberMap register clobbered.");
  Label done;
  Label not_in_int32_range;

  __ UntagAndJumpIfSmi(dst, object, &done);
  __ LoadP(scratch1, FieldMemOperand(object, HeapNumber::kMapOffset));
  __ CmpRR(scratch1, heap_number_map);
  __ bne(not_number);
  __ ConvertToInt32(object,
                    dst,
                    scratch1,
                    scratch2,
                    double_scratch,
                    &not_in_int32_range);
  __ b(&done);

  __ bind(&not_in_int32_range);
  __ LoadlW(scratch1, FieldMemOperand(object, HeapNumber::kExponentOffset));
  __ LoadlW(scratch2, FieldMemOperand(object, HeapNumber::kMantissaOffset));

  __ EmitOutOfInt32RangeTruncate(dst,
                                 scratch1,
                                 scratch2,
                                 scratch3);
  __ bind(&done);
}


void FloatingPointHelper::ConvertIntToDouble(MacroAssembler* masm,
                                             Register src,
                                             DwVfpRegister double_dst) {
  __ cdfbr(double_dst, src);
}


void FloatingPointHelper::ConvertUnsignedIntToDouble(MacroAssembler* masm,
                                                     Register src,
                                                     DwVfpRegister double_dst) {
  __ cdlfbr(Condition(5), Condition(5), double_dst, src);
}

void FloatingPointHelper::ConvertIntToFloat(MacroAssembler* masm,
                                            const DwVfpRegister dst,
                                            const Register src,
                                            const Register int_scratch) {
  __ Sub(sp, Operand(8));  // reserve one temporary double on the stack

  // sign-extend src to 64-bit and store it to temp double on the stack
#if V8_TARGET_ARCH_S390X
  __ lgfr(int_scratch, src);
  __ stg(int_scratch, MemOperand(sp, 0));
#else
  __ srawi(int_scratch, src, 31);
#if __FLOAT_WORD_ORDER == __LITTLE_ENDIAN
  __ StoreW(int_scratch, MemOperand(sp, 4));
  __ StoreW(src, MemOperand(sp, 0));
#else
  __ StoreW(int_scratch, MemOperand(sp, 0));
  __ StoreW(src, MemOperand(sp, 4));
#endif
#endif

  // load sign-extended src into FPR
  __ LoadF(dst, MemOperand(sp, 0));

  __ AddP(sp, Operand(8));  // restore stack

  __ fcfid(dst, dst);
  __ frsp(dst, dst);
}

void FloatingPointHelper::LoadNumberAsInt32Double(MacroAssembler* masm,
                                                  Register object,
                                                  DwVfpRegister double_dst,
                                                  DwVfpRegister double_scratch,
                                                  Register heap_number_map,
                                                  Register scratch1,
                                                  Register scratch2,
                                                  Label* not_int32) {
  ASSERT(!scratch1.is(object) && !scratch2.is(object));
  ASSERT(!scratch1.is(scratch2));
  ASSERT(!heap_number_map.is(object) &&
         !heap_number_map.is(scratch1) &&
         !heap_number_map.is(scratch2));

  Label done, obj_is_not_smi;

  __ JumpIfNotSmi(object, &obj_is_not_smi);
  __ SmiUntag(scratch1, object);
  ConvertIntToDouble(masm, scratch1, double_dst);
  __ b(&done);

  __ bind(&obj_is_not_smi);
  __ AssertRootValue(heap_number_map,
                     Heap::kHeapNumberMapRootIndex,
                     "HeapNumberMap register clobbered.");
  __ JumpIfNotHeapNumber(object, heap_number_map, scratch1, not_int32);

  // Load the double value.
  __ LoadF(double_dst, FieldMemOperand(object, HeapNumber::kValueOffset));

  __ EmitVFPTruncate(kRoundToZero,
                     scratch1,
                     double_dst,
                     scratch2,
                     double_scratch,
                     kCheckForInexactConversion);

  // Jump to not_int32 if the operation did not succeed.
  __ b(Condition(CC_OF), not_int32);

  __ bind(&done);
}


void FloatingPointHelper::LoadNumberAsInt32(MacroAssembler* masm,
                                            Register object,
                                            Register dst,
                                            Register heap_number_map,
                                            Register scratch1,
                                            Register scratch2,
                                            Register scratch3,
                                            DwVfpRegister double_scratch0,
                                            DwVfpRegister double_scratch1,
                                            Label* not_int32) {
  ASSERT(!dst.is(object));
  ASSERT(!scratch1.is(object) && !scratch2.is(object) && !scratch3.is(object));
  ASSERT(!scratch1.is(scratch2) &&
         !scratch1.is(scratch3) &&
         !scratch2.is(scratch3));

  Label done;

  __ UntagAndJumpIfSmi(dst, object, &done);

  __ AssertRootValue(heap_number_map,
                     Heap::kHeapNumberMapRootIndex,
                     "HeapNumberMap register clobbered.");
  __ JumpIfNotHeapNumber(object, heap_number_map, scratch1, not_int32);

  // Load the double value.
  __ LoadF(double_scratch0, FieldMemOperand(object, HeapNumber::kValueOffset));

  __ EmitVFPTruncate(kRoundToZero,
                     dst,
                     double_scratch0,
                     scratch1,
                     double_scratch1,
                     kCheckForInexactConversion);

  // Jump to not_int32 if the operation did not succeed.
  __ b(Condition(CC_OF), not_int32);

  __ bind(&done);
}


void FloatingPointHelper::DoubleIs32BitInteger(MacroAssembler* masm,
                                               Register src1,
                                               Register src2,
                                               Register dst,
                                               Register scratch,
                                               Label* not_int32) {
  // Get exponent alone in scratch.
  STATIC_ASSERT(HeapNumber::kExponentMask == 0x7ff00000u);
  __ ExtractBitMask(scratch, src1, HeapNumber::kExponentMask);

  // Substract the bias from the exponent.
  __ AddP(scratch, Operand(-HeapNumber::kExponentBias));

  // src1: higher (exponent) part of the double value.
  // src2: lower (mantissa) part of the double value.
  // scratch: unbiased exponent.

  // Fast cases. Check for obvious non 32-bit integer values.
  // Negative exponent cannot yield 32-bit integers.
  __ Cmpi(scratch, Operand::Zero());
  __ blt(not_int32);
  // Exponent greater than 31 cannot yield 32-bit integers.
  // Also, a positive value with an exponent equal to 31 is outside of the
  // signed 32-bit integer range.
  // Another way to put it is that if (exponent - signbit) > 30 then the
  // number cannot be represented as an int32.
  Register tmp = dst;
  __ ExtractSignBit32(tmp, src1);
  __ Sub(tmp, scratch, tmp);
  __ Cmpi(tmp, Operand(30));
  __ bgt(not_int32);
  // - Check whether bits [21:0] in the mantissa are not null.
  __ TestBitRange(src2, 21, 0, r0_p);
  __ bne(not_int32 /*, cr0*/);

  // Otherwise the exponent needs to be big enough to shift left all the
  // non zero bits left. So we need the (30 - exponent) last bits of the
  // 31 higher bits of the mantissa to be null.
  // Because bits [21:0] are null, we can check instead that the
  // (32 - exponent) last bits of the 32 higher bits of the mantissa are null.

  // Get the 32 higher bits of the mantissa in dst.
  STATIC_ASSERT(HeapNumber::kMantissaBitsInTopWord == 20);
  STATIC_ASSERT(HeapNumber::kNonMantissaBitsInTopWord == 12);
  __ ExtractBitRange(dst, src2, 31, HeapNumber::kMantissaBitsInTopWord);
  __ slwi(src1, src1, Operand(HeapNumber::kNonMantissaBitsInTopWord));
  __ OrP(dst, src1);

  // Create the mask and test the lower bits (of the higher bits).
  __ subfic(scratch, scratch, Operand(32));
  __ LoadImmP(src2, Operand(1));
  __ ShiftLeft(src1, src2, scratch);
  __ AddP(src1, Operand(-1));
  __ LoadRR(r0_p, dst);
  __ AndP(r0_p, src1/*, SetRC*/);
  // Removing RC should be okay
  __ bne(not_int32 /*, cr0*/);
}


void FloatingPointHelper::CallCCodeForDoubleOperation(
    MacroAssembler* masm,
    Token::Value op,
    Register heap_number_result,
    Register scratch) {
  // d1 - first arg, d2 - second arg
  // d1 return value

  // Assert that heap_number_result is callee-saved.
  // PowerPC doesn't preserve r8_p.. need to handle this specially
  // We currently always use r8_p to pass it.
  ASSERT(heap_number_result.is(r8_p));
  __ push(r8_p);

  // Push the current return address before the C call. Return will be
  // through pop() below.
  __ mflr(r0_p);
  __ push(r0_p);
  __ PrepareCallCFunction(0, 2, scratch);

  {
    AllowExternalCallThatCantCauseGC scope(masm);
    __ CallCFunction(
        ExternalReference::double_fp_operation(op, masm->isolate()), 0, 2);
  }
  // load saved r8_p value, restore lr
  __ pop(r0_p);
  __ mtlr(r0_p);
  __ pop(r8_p);

  // Store answer in the overwritable heap number. Double returned in d1
  __ StoreF(d1, FieldMemOperand(heap_number_result, HeapNumber::kValueOffset));

  // Place heap_number_result in r3_p and return to the pushed return address.
  __ LoadRR(r3_p, heap_number_result);
  __ blr();
}

// Handle the case where the lhs and rhs are the same object.
// Equality is almost reflexive (everything but NaN), so this is a test
// for "identity and not NaN".
static void EmitIdenticalObjectComparison(MacroAssembler* masm,
                                          Label* slow,
                                          Condition cond,
                                          bool never_nan_nan) {
  Label not_identical;
  Label heap_number, return_equal;
  __ CmpRR(r3_p, r4_p);
  __ bne(&not_identical);

  // The two objects are identical.  If we know that one of them isn't NaN then
  // we now know they test equal.
  if (cond != eq || !never_nan_nan) {
    // Test for NaN. Sadly, we can't just compare to FACTORY->nan_value(),
    // so we do the second best thing - test it ourselves.
    // They are both equal and they are not both Smis so both of them are not
    // Smis.  If it's not a heap number, then return equal.
    if (cond == lt || cond == gt) {
      __ CompareObjectType(r3_p, r7_p, r7_p, FIRST_SPEC_OBJECT_TYPE);
      __ bge(slow);
    } else {
      __ CompareObjectType(r3_p, r7_p, r7_p, HEAP_NUMBER_TYPE);
      __ beq(&heap_number);
      // Comparing JS objects with <=, >= is complicated.
      if (cond != eq) {
        __ Cmpi(r7_p, Operand(FIRST_SPEC_OBJECT_TYPE));
        __ bge(slow);
        // Normally here we fall through to return_equal, but undefined is
        // special: (undefined == undefined) == true, but
        // (undefined <= undefined) == false!  See ECMAScript 11.8.5.
        if (cond == le || cond == ge) {
          __ Cmpi(r7_p, Operand(ODDBALL_TYPE));
          __ bne(&return_equal);
          __ LoadRoot(r5_p, Heap::kUndefinedValueRootIndex);
          __ CmpRR(r3_p, r5_p);
          __ bne(&return_equal);
          if (cond == le) {
            // undefined <= undefined should fail.
            __ LoadImmP(r3_p, Operand(GREATER));
          } else  {
            // undefined >= undefined should fail.
            __ LoadImmP(r3_p, Operand(LESS));
          }
          __ Ret();
        }
      }
    }
  }

  __ bind(&return_equal);
  if (cond == lt) {
    __ LoadImmP(r3_p, Operand(GREATER));  // Things aren't less than themselves.
  } else if (cond == gt) {
    __ LoadImmP(r3_p, Operand(LESS));  // Things aren't greater than themselves.
  } else {
    __ LoadImmP(r3_p, Operand(EQUAL));  // Things are <=, >=, ==, === themselves
  }
  __ Ret();

  if (cond != eq || !never_nan_nan) {
    // For less and greater we don't have to check for NaN since the result of
    // x < x is false regardless.  For the others here is some code to check
    // for NaN.
    if (cond != lt && cond != gt) {
      __ bind(&heap_number);
      // It is a heap number, so return non-equal if it's NaN and equal if it's
      // not NaN.

      // The representation of NaN values has all exponent bits (52..62) set,
      // and not all mantissa bits (0..51) clear.
      // Read top bits of double representation (second word of value).
      __ LoadlW(r5_p, FieldMemOperand(r3_p, HeapNumber::kExponentOffset));
      // Test that exponent bits are all set.
      STATIC_ASSERT(HeapNumber::kExponentMask == 0x7ff00000u);
      __ ExtractBitMask(r6_p, r5_p, HeapNumber::kExponentMask);
      __ Cmpli(r6_p, Operand(0x7ff));
      __ bne(&return_equal);

      // Shift out flag and all exponent bits, retaining only mantissa.
      __ slwi(r5_p, r5_p, Operand(HeapNumber::kNonMantissaBitsInTopWord));
      // Or with all low-bits of mantissa.
      __ LoadlW(r6_p, FieldMemOperand(r3_p, HeapNumber::kMantissaOffset));
      __ LoadRR(r3_p, r5_p);
      __ OrP(r3_p, r6_p);
      __ Cmpi(r3_p, Operand::Zero());
      // For equal we already have the right value in r3_p:  Return zero (equal)
      // if all bits in mantissa are zero (it's an Infinity) and non-zero if
      // not (it's a NaN).  For <= and >= we need to load r0_p with the failing
      // value if it's a NaN.
      if (cond != eq) {
        Label not_equal;
        __ bne(&not_equal);
        // All-zero means Infinity means equal.
        __ Ret();
        __ bind(&not_equal);
        if (cond == le) {
          __ LoadImmP(r3_p, Operand(GREATER));  // NaN <= NaN should fail.
        } else {
          __ LoadImmP(r3_p, Operand(LESS));     // NaN >= NaN should fail.
        }
      }
      __ Ret();
    }
    // No fall through here.
  }

  __ bind(&not_identical);
}


// See comment at call site.
static void EmitSmiNonsmiComparison(MacroAssembler* masm,
                                    Register lhs,
                                    Register rhs,
                                    Label* lhs_not_nan,
                                    Label* slow,
                                    bool strict) {
  ASSERT((lhs.is(r3_p) && rhs.is(r4_p)) ||
         (lhs.is(r4_p) && rhs.is(r3_p)));

  Label rhs_is_smi;
  __ JumpIfSmi(rhs, &rhs_is_smi);

  // Lhs is a Smi.  Check whether the rhs is a heap number.
  __ CompareObjectType(rhs, r6_p, r7_p, HEAP_NUMBER_TYPE);
  if (strict) {
    // If rhs is not a number and lhs is a Smi then strict equality cannot
    // succeed.  Return non-equal
    // If rhs is r3_p then there is already a non zero value in it.
    Label skip;
    __ beq(&skip);
    if (!rhs.is(r3_p)) {
      __ mov(r3_p, Operand(NOT_EQUAL));
    }
    __ Ret();
    __ bind(&skip);
  } else {
    // Smi compared non-strictly with a non-Smi non-heap-number.  Call
    // the runtime.
    __ bne(slow);
  }

  // Lhs is a smi, rhs is a number.
  // Convert lhs to a double in d7.
  __ SmiToDoubleFPRegister(lhs, d7, r10_p);
  // Load the double from rhs, tagged HeapNumber r3_p, to d6.
  __ LoadF(d6, FieldMemOperand(rhs, HeapNumber::kValueOffset));

  // We now have both loaded as doubles but we can skip the lhs nan check
  // since it's a smi.
  __ b(lhs_not_nan);

  __ bind(&rhs_is_smi);
  // Rhs is a smi.  Check whether the non-smi lhs is a heap number.
  __ CompareObjectType(lhs, r7_p, r7_p, HEAP_NUMBER_TYPE);
  if (strict) {
    // If lhs is not a number and rhs is a smi then strict equality cannot
    // succeed.  Return non-equal.
    // If lhs is r3_p then there is already a non zero value in it.
    Label skip;
    __ beq(&skip);
    if (!lhs.is(r3_p)) {
      __ mov(r3_p, Operand(NOT_EQUAL));
    }
    __ Ret();
    __ bind(&skip);
  } else {
    // Smi compared non-strictly with a non-smi non-heap-number.  Call
    // the runtime.
    __ bne(slow);
  }

  // Rhs is a smi, lhs is a heap number.
  // Load the double from lhs, tagged HeapNumber r4_p, to d7.
  __ LoadF(d7, FieldMemOperand(lhs, HeapNumber::kValueOffset));
  // Convert rhs to a double in d6.
  __ SmiToDoubleFPRegister(rhs, d6, r10_p);
  // Fall through to both_loaded_as_doubles.
}

// See comment at call site.
static void EmitStrictTwoHeapObjectCompare(MacroAssembler* masm,
                                           Register lhs,
                                           Register rhs) {
    ASSERT((lhs.is(r3_p) && rhs.is(r4_p)) ||
           (lhs.is(r4_p) && rhs.is(r3_p)));

    // If either operand is a JS object or an oddball value, then they are
    // not equal since their pointers are different.
    // There is no test for undetectability in strict equality.
    STATIC_ASSERT(LAST_TYPE == LAST_SPEC_OBJECT_TYPE);
    Label first_non_object;
    // Get the type of the first operand into r5_p and compare it with
    // FIRST_SPEC_OBJECT_TYPE.
    __ CompareObjectType(rhs, r5_p, r5_p, FIRST_SPEC_OBJECT_TYPE);
    __ blt(&first_non_object);

    // Return non-zero (r3_p is not zero)
    Label return_not_equal;
    __ bind(&return_not_equal);
    __ Ret();

    __ bind(&first_non_object);
    // Check for oddballs: true, false, null, undefined.
    __ Cmpi(r5_p, Operand(ODDBALL_TYPE));
    __ beq(&return_not_equal);

    __ CompareObjectType(lhs, r6_p, r6_p, FIRST_SPEC_OBJECT_TYPE);
    __ bge(&return_not_equal);

    // Check for oddballs: true, false, null, undefined.
    __ Cmpi(r6_p, Operand(ODDBALL_TYPE));
    __ beq(&return_not_equal);

    // Now that we have the types we might as well check for symbol-symbol.
    // Ensure that no non-strings have the symbol bit set.
    STATIC_ASSERT(LAST_TYPE < kNotStringTag + kIsSymbolMask);
    STATIC_ASSERT(kSymbolTag != 0);
    __ AndP(r5_p, r6_p);
    __ LoadRR(r0_p, r5_p);
    __ AndP(r0_p, Operand(kIsSymbolMask));
    __ bne(&return_not_equal /*, cr0*/);
}


static void EmitCheckForTwoHeapNumbers(MacroAssembler* masm,
                                       Register lhs,
                                       Register rhs,
                                       Label* both_loaded_as_doubles,
                                       Label* not_heap_numbers,
                                       Label* slow) {
  ASSERT((lhs.is(r3_p) && rhs.is(r4_p)) ||
         (lhs.is(r4_p) && rhs.is(r3_p)));

  __ CompareObjectType(rhs, r6_p, r5_p, HEAP_NUMBER_TYPE);
  __ bne(not_heap_numbers);
  __ LoadP(r5_p, FieldMemOperand(lhs, HeapObject::kMapOffset));
  __ CmpRR(r5_p, r6_p);
  __ bne(slow);  // First was a heap number, second wasn't.  Go slow case.

  // Both are heap numbers.  Load them up then jump to the code we have
  // for that.
  __ LoadF(d6, FieldMemOperand(rhs, HeapNumber::kValueOffset));
  __ LoadF(d7, FieldMemOperand(lhs, HeapNumber::kValueOffset));

  __ b(both_loaded_as_doubles);
}


// Fast negative check for symbol-to-symbol equality.
static void EmitCheckForSymbolsOrObjects(MacroAssembler* masm,
                                         Register lhs,
                                         Register rhs,
                                         Label* possible_strings,
                                         Label* not_both_strings) {
  ASSERT((lhs.is(r3_p) && rhs.is(r4_p)) ||
         (lhs.is(r4_p) && rhs.is(r3_p)));

  // r5_p is object type of rhs.
  // Ensure that no non-strings have the symbol bit set.
  Label object_test;
  STATIC_ASSERT(kSymbolTag != 0);
  __ LoadRR(r0_p, r5_p);
  __ AndP(r0_p, Operand(kIsNotStringMask));
  __ bne(&object_test /*, cr0*/);
  __ LoadRR(r0_p, r5_p);
  __ AndP(r0_p, Operand(kIsSymbolMask));
  __ beq(possible_strings /*, cr0*/);
  __ CompareObjectType(lhs, r6_p, r6_p, FIRST_NONSTRING_TYPE);
  __ bge(not_both_strings);
  __ LoadRR(r0_p, r6_p);
  __ AndP(r0_p, Operand(kIsSymbolMask));
  __ beq(possible_strings /*, cr0*/);

  // Both are symbols.  We already checked they weren't the same pointer
  // so they are not equal.
  __ LoadImmP(r3_p, Operand(NOT_EQUAL));
  __ Ret();

  __ bind(&object_test);
  __ Cmpi(r5_p, Operand(FIRST_SPEC_OBJECT_TYPE));
  __ blt(not_both_strings);
  __ CompareObjectType(lhs, r5_p, r6_p, FIRST_SPEC_OBJECT_TYPE);
  __ blt(not_both_strings);
  // If both objects are undetectable, they are equal. Otherwise, they
  // are not equal, since they are different objects and an object is not
  // equal to undefined.
  __ LoadP(r6_p, FieldMemOperand(rhs, HeapObject::kMapOffset));
  __ LoadlB(r5_p, FieldMemOperand(r5_p, Map::kBitFieldOffset));
  __ LoadlB(r6_p, FieldMemOperand(r6_p, Map::kBitFieldOffset));
  __ LoadRR(r3_p, r6_p);
  __ AndP(r3_p, r5_p);
  __ AndP(r3_p, Operand(1 << Map::kIsUndetectable));
  __ xori(r3_p, r3_p, Operand(1 << Map::kIsUndetectable));
  __ Ret();
}


void NumberToStringStub::GenerateLookupNumberStringCache(MacroAssembler* masm,
                                                         Register object,
                                                         Register result,
                                                         Register scratch1,
                                                         Register scratch2,
                                                         Register scratch3,
                                                         bool object_is_smi,
                                                         Label* not_found) {
  // Use of registers. Register result is used as a temporary.
  Register number_string_cache = result;
  Register mask = scratch3;

  // Load the number string cache.
  __ LoadRoot(number_string_cache, Heap::kNumberStringCacheRootIndex);

  // Make the hash mask from the length of the number string cache. It
  // contains two elements (number and string) for each cache entry.
  __ LoadP(mask, FieldMemOperand(number_string_cache,
                                 FixedArray::kLengthOffset));
  // Divide length by two (length is a smi).
  __ ShiftRightArithImm(mask, mask, kSmiTagSize + kSmiShiftSize + 1);
  __ Sub(mask, Operand(1));  // Make mask.

  // Calculate the entry in the number string cache. The hash value in the
  // number string cache for smis is just the smi value, and the hash for
  // doubles is the xor of the upper and lower words. See
  // Heap::GetNumberStringCache.
  Isolate* isolate = masm->isolate();
  Label is_smi;
  Label load_result_from_cache;
  if (!object_is_smi) {
    __ JumpIfSmi(object, &is_smi);

    __ CheckMap(object,
                scratch1,
                Heap::kHeapNumberMapRootIndex,
                not_found,
                DONT_DO_SMI_CHECK);

    STATIC_ASSERT(8 == kDoubleSize);
    __ LoadlW(scratch1, FieldMemOperand(object, HeapNumber::kExponentOffset));
    __ LoadlW(scratch2, FieldMemOperand(object, HeapNumber::kMantissaOffset));
    __ XorP(scratch1, scratch2);
    __ AndP(scratch1, mask);

    // Calculate address of entry in string cache: each entry consists
    // of two pointer sized fields.
    __ ShiftLeftImm(scratch1, scratch1, Operand(kPointerSizeLog2 + 1));
    __ LoadRR(scratch1, number_string_cache);
    __ AddP(scratch1, scratch1);

    Register probe = mask;
    __ LoadP(probe, FieldMemOperand(scratch1, FixedArray::kHeaderSize));
    __ JumpIfSmi(probe, not_found);
    __ LoadF(d0, FieldMemOperand(object, HeapNumber::kValueOffset));
    __ LoadF(d1, FieldMemOperand(probe, HeapNumber::kValueOffset));
    __ fcmpu(d0, d1);
    __ bne(not_found);  // The cache did not contain this value.
    __ b(&load_result_from_cache);
  }

  __ bind(&is_smi);
  Register scratch = scratch1;
  __ SmiUntag(scratch, object);
  __ AndP(scratch, mask);
  // Calculate address of entry in string cache: each entry consists
  // of two pointer sized fields.
  __ ShiftLeftImm(scratch, scratch, Operand(kPointerSizeLog2 + 1));
  __ LoadRR(scratch, number_string_cache);
  __ AddP(scratch, scratch);

  // Check if the entry is the smi we are looking for.
  Register probe = mask;
  __ LoadP(probe, FieldMemOperand(scratch, FixedArray::kHeaderSize));
  __ CmpRR(object, probe);
  __ bne(not_found);

  // Get the result from the cache.
  __ bind(&load_result_from_cache);
  __ LoadP(result,
         FieldMemOperand(scratch, FixedArray::kHeaderSize + kPointerSize));
  __ IncrementCounter(isolate->counters()->number_to_string_native(),
                      1,
                      scratch1,
                      scratch2);
}


void NumberToStringStub::Generate(MacroAssembler* masm) {
  Label runtime;

  __ LoadP(r4_p, MemOperand(sp, 0));

  // Generate code to lookup number in the number string cache.
  GenerateLookupNumberStringCache(masm, r4_p, r3_p, r5_p, r6_p, r7_p, false,
                                  &runtime);
  __ AddP(sp, Operand(1 * kPointerSize));
  __ Ret();

  __ bind(&runtime);
  // Handle number to string in the runtime system if not found in the cache.
  __ TailCallRuntime(Runtime::kNumberToStringSkipCache, 1, 1);
}


// On entry lhs_ and rhs_ are the values to be compared.
// On exit r3_p is 0, positive or negative to indicate the result of
// the comparison.
void CompareStub::Generate(MacroAssembler* masm) {
  ASSERT((lhs_.is(r3_p) && rhs_.is(r4_p)) ||
         (lhs_.is(r4_p) && rhs_.is(r3_p)));

  Label slow;  // Call builtin.
  Label not_smis, both_loaded_as_doubles, lhs_not_nan;

  if (include_smi_compare_) {
    Label not_two_smis, smi_done;
    __ LoadRR(r5_p, r3_p);
    __ OrP(r5_p, r4_p);
    __ JumpIfNotSmi(r5_p, &not_two_smis);
    __ SmiUntag(r4_p);
    __ SmiUntag(r3_p);
    __ Sub(r3_p, r4_p, r3_p);
    __ Ret();
    __ bind(&not_two_smis);
  } else if (FLAG_debug_code) {
    __ LoadRR(r5_p, r3_p);
    __ OrP(r5_p, r4_p);
    STATIC_ASSERT(kSmiTagMask < 0x8000);
    __ LoadRR(r0_p, r5_p);
    __ AndP(r0_p, Operand(kSmiTagMask));
    __ Assert(ne, "CompareStub: unexpected smi operands.", cr0);
    // TODO(JOHN): might be a probelm here b/c cr0 isn't set.
  }

  // NOTICE! This code is only reached after a smi-fast-case check, so
  // it is certain that at least one operand isn't a smi.

  // Handle the case where the objects are identical.  Either returns the answer
  // or goes to slow.  Only falls through if the objects were not identical.
  EmitIdenticalObjectComparison(masm, &slow, cc_, never_nan_nan_);

  // If either is a Smi (we know that not both are), then they can only
  // be strictly equal if the other is a HeapNumber.
  STATIC_ASSERT(kSmiTag == 0);
  ASSERT_EQ(0, Smi::FromInt(0));
  __ LoadRR(r5_p, rhs_);
  __ AndP(r5_p, lhs_);
  __ JumpIfNotSmi(r5_p, &not_smis);
  // One operand is a smi.  EmitSmiNonsmiComparison generates code that can:
  // 1) Return the answer.
  // 2) Go to slow.
  // 3) Fall through to both_loaded_as_doubles.
  // 4) Jump to lhs_not_nan.
  // In cases 3 and 4 we have found out we were dealing with a number-number
  // comparison.  The double values of the numbers have been loaded
  // into d7 and d6.
  EmitSmiNonsmiComparison(masm, lhs_, rhs_, &lhs_not_nan, &slow, strict_);

  __ bind(&both_loaded_as_doubles);
  // The arguments have been converted to doubles and stored in d6 and d7
  Isolate* isolate = masm->isolate();
  __ bind(&lhs_not_nan);
  Label no_nan;
  __ fcmpu(d7, d6);

  Label nan, equal, less_than;
  __ bunordered(&nan);
  __ beq(&equal);
  __ blt(&less_than);
  __ LoadImmP(r3_p, Operand(GREATER));
  __ Ret();
  __ bind(&equal);
  __ LoadImmP(r3_p, Operand(EQUAL));
  __ Ret();
  __ bind(&less_than);
  __ LoadImmP(r3_p, Operand(LESS));
  __ Ret();

  __ bind(&nan);
  // If one of the sides was a NaN then the v flag is set.  Load r3_p with
  // whatever it takes to make the comparison fail, since comparisons with NaN
  // always fail.
  if (cc_ == lt || cc_ == le) {
    __ LoadImmP(r3_p, Operand(GREATER));
  } else {
    __ LoadImmP(r3_p, Operand(LESS));
  }
  __ Ret();

  __ bind(&not_smis);
  // At this point we know we are dealing with two different objects,
  // and neither of them is a Smi.  The objects are in rhs_ and lhs_.
  if (strict_) {
    // This returns non-equal for some object types, or falls through if it
    // was not lucky.
    EmitStrictTwoHeapObjectCompare(masm, lhs_, rhs_);
  }

  Label check_for_symbols;
  Label flat_string_check;
  // Check for heap-number-heap-number comparison.  Can jump to slow case, or
  // load both doubles into r3_p, r4_p, r5_p, r6_p and jump to the code that
  // handles that case.  If the inputs are not doubles then jumps to
  // check_for_symbols. In this case r5_p will contain the type of rhs_.
  // Never falls through.
  EmitCheckForTwoHeapNumbers(masm,
                             lhs_,
                             rhs_,
                             &both_loaded_as_doubles,
                             &check_for_symbols,
                             &flat_string_check);

  __ bind(&check_for_symbols);
  // In the strict case the EmitStrictTwoHeapObjectCompare already took care of
  // symbols.
  if (cc_ == eq && !strict_) {
    // Returns an answer for two symbols or two detectable objects.
    // Otherwise jumps to string case or not both strings case.
    // Assumes that r5_p is the type of rhs_ on entry.
    EmitCheckForSymbolsOrObjects(masm, lhs_, rhs_, &flat_string_check, &slow);
  }

  // Check for both being sequential ASCII strings, and inline if that is the
  // case.
  __ bind(&flat_string_check);

  __ JumpIfNonSmisNotBothSequentialAsciiStrings(lhs_, rhs_, r5_p, r6_p, &slow);

  __ IncrementCounter(isolate->counters()->string_compare_native(), 1, r5_p,
                      r6_p);
  if (cc_ == eq) {
    StringCompareStub::GenerateFlatAsciiStringEquals(masm,
                                                     lhs_,
                                                     rhs_,
                                                     r5_p,
                                                     r6_p);
  } else {
    StringCompareStub::GenerateCompareFlatAsciiStrings(masm,
                                                       lhs_,
                                                       rhs_,
                                                       r5_p,
                                                       r6_p,
                                                       r7_p);
  }
  // Never falls through to here.

  __ bind(&slow);

  __ Push(lhs_, rhs_);
  // Figure out which native to call and setup the arguments.
  Builtins::JavaScript native;
  if (cc_ == eq) {
    native = strict_ ? Builtins::STRICT_EQUALS : Builtins::EQUALS;
  } else {
    native = Builtins::COMPARE;
    int ncr;  // NaN compare result
    if (cc_ == lt || cc_ == le) {
      ncr = GREATER;
    } else {
      ASSERT(cc_ == gt || cc_ == ge);  // remaining cases
      ncr = LESS;
    }
    __ LoadSmiLiteral(r3_p, Smi::FromInt(ncr));
    __ push(r3_p);
  }

  // Call the native; it returns -1 (less), 0 (equal), or 1 (greater)
  // tagged as a small integer.
  __ InvokeBuiltin(native, JUMP_FUNCTION);
}


// The stub expects its argument in the tos_ register and returns its result in
// it, too: zero for false, and a non-zero value for true.
void ToBooleanStub::Generate(MacroAssembler* masm) {
  // This stub overrides SometimesSetsUpAFrame() to return false.  That means
  // we cannot call anything that could cause a GC from this stub.
  Label patch;
  const Register map = r22_p.is(tos_) ? r10_p : r22_p;

  // undefined -> false.
  CheckOddball(masm, UNDEFINED, Heap::kUndefinedValueRootIndex, false);

  // Boolean -> its value.
  CheckOddball(masm, BOOLEAN, Heap::kFalseValueRootIndex, false);
  CheckOddball(masm, BOOLEAN, Heap::kTrueValueRootIndex, true);

  // 'null' -> false.
  CheckOddball(masm, NULL_TYPE, Heap::kNullValueRootIndex, false);

  if (types_.Contains(SMI)) {
    // Smis: 0 -> false, all other -> true
    Label not_smi;
    __ JumpIfNotSmi(tos_,  &not_smi);
    // tos_ contains the correct return value already
    __ Ret();
    __ bind(&not_smi);
  } else if (types_.NeedsMap()) {
    // If we need a map later and have a Smi -> patch.
    __ JumpIfSmi(tos_, &patch);
  }

  if (types_.NeedsMap()) {
    __ LoadP(map, FieldMemOperand(tos_, HeapObject::kMapOffset));

    if (types_.CanBeUndetectable()) {
      Label not_undetectable;
      __ LoadlB(ip, FieldMemOperand(map, Map::kBitFieldOffset));
      STATIC_ASSERT((1 << Map::kIsUndetectable) < 0x8000);
      __ LoadRR(r0_p, ip);
      __ AndP(r0_p, Operand(1 << Map::kIsUndetectable));
      __ beq(&not_undetectable /*, cr0*/);
      // Undetectable -> false.
      __ LoadImmP(tos_, Operand(0, RelocInfo::NONE));
      __ Ret();
      __ bind(&not_undetectable);
    }
  }

  if (types_.Contains(SPEC_OBJECT)) {
    // Spec object -> true.
    Label not_js_object;
    __ CompareInstanceType(map, ip, FIRST_SPEC_OBJECT_TYPE);
    // tos_ contains the correct non-zero return value already.
    __ blt(&not_js_object);
    __ Ret();
    __ bind(&not_js_object);
  }

  if (types_.Contains(STRING)) {
    // String value -> false iff empty.
    Label not_string;
    __ CompareInstanceType(map, ip, FIRST_NONSTRING_TYPE);
    __ bge(&not_string);
    __ LoadP(tos_, FieldMemOperand(tos_, String::kLengthOffset));
    __ Ret();  // the string length is OK as the return value
    __ bind(&not_string);
  }

  if (types_.Contains(HEAP_NUMBER)) {
    // Heap number -> false iff +0, -0, or NaN.
    Label not_heap_number, nan_or_zero;
    __ CompareRoot(map, Heap::kHeapNumberMapRootIndex);
    __ bne(&not_heap_number);

    __ LoadF(d1, FieldMemOperand(tos_, HeapNumber::kValueOffset));
    __ LoadImmP(r0_p, Operand::Zero());
    __ push(r0_p);
#if !V8_TARGET_ARCH_S390X
    __ push(r0_p);
#endif
    __ LoadF(d2, MemOperand(sp, 0));
    __ AddP(sp, Operand(8));
    __ fcmpu(d1, d2);
    // "tos_" is a register, and contains a non zero value by default.
    // Hence we only need to overwrite "tos_" with zero to return false for
    // FP_ZERO or FP_NAN cases. Otherwise, by default it returns true.
    __ bunordered(&nan_or_zero);
    __ beq(&nan_or_zero);
    __ Ret();

    __ bind(&nan_or_zero);
    __ LoadImmP(tos_, Operand::Zero());
    __ Ret();

    __ bind(&not_heap_number);
  }

  __ bind(&patch);
  GenerateTypeTransition(masm);
}


void ToBooleanStub::CheckOddball(MacroAssembler* masm,
                                 Type type,
                                 Heap::RootListIndex value,
                                 bool result) {
  if (types_.Contains(type)) {
    // If we see an expected oddball, return its ToBoolean value tos_.
    Label different_value;
    __ LoadRoot(ip, value);
    __ CmpRR(tos_, ip);
    __ bne(&different_value);
    // The value of a root is never NULL, so we can avoid loading a non-null
    // value into tos_ when we want to return 'true'.
    if (!result) {
      __ LoadImmP(tos_, Operand(0, RelocInfo::NONE));
    }
    // Intel has some logic here not present on ARM
    // unclear if it's needed or not
    __ Ret();
    __ bind(&different_value);
  }
}


void ToBooleanStub::GenerateTypeTransition(MacroAssembler* masm) {
  if (!tos_.is(r6_p)) {
    __ LoadRR(r6_p, tos_);
  }
  __ LoadSmiLiteral(r5_p, Smi::FromInt(tos_.code()));
  __ LoadSmiLiteral(r4_p, Smi::FromInt(types_.ToByte()));
  __ Push(r6_p, r5_p, r4_p);
  // Patch the caller to an appropriate specialized stub and return the
  // operation result to the caller of the stub.
  __ TailCallExternalReference(
      ExternalReference(IC_Utility(IC::kToBoolean_Patch), masm->isolate()),
      3,
      1);
}


void StoreBufferOverflowStub::Generate(MacroAssembler* masm) {
  // We don't allow a GC during a store buffer overflow so there is no need to
  // store the registers in any particular way, but we do have to store and
  // restore them.
  __ mflr(r0_p);
  __ MultiPush(kJSCallerSaved | r0_p.bit());
  if (save_doubles_ == kSaveFPRegs) {
    const int kNumRegs = DwVfpRegister::kNumVolatileRegisters;
    __ Sub(sp, Operand(kDoubleSize * kNumRegs));
    for (int i = 0; i < kNumRegs; i++) {
      DwVfpRegister reg = DwVfpRegister::from_code(i);
      __ StoreF(reg, MemOperand(sp, i * kDoubleSize));
    }
  }
  const int argument_count = 1;
  const int fp_argument_count = 0;
  const Register scratch = r4_p;

  AllowExternalCallThatCantCauseGC scope(masm);
  __ PrepareCallCFunction(argument_count, fp_argument_count, scratch);
  __ mov(r3_p, Operand(ExternalReference::isolate_address()));
  __ CallCFunction(
      ExternalReference::store_buffer_overflow_function(masm->isolate()),
      argument_count);
  if (save_doubles_ == kSaveFPRegs) {
    const int kNumRegs = DwVfpRegister::kNumVolatileRegisters;
    for (int i = 0; i < kNumRegs; i++) {
      DwVfpRegister reg = DwVfpRegister::from_code(i);
      __ LoadF(reg, MemOperand(sp, i * kDoubleSize));
    }
    __ AddP(sp, Operand(kDoubleSize * kNumRegs));
  }
  __ MultiPop(kJSCallerSaved | r0_p.bit());
  __ mtlr(r0_p);
  __ Ret();
}


void UnaryOpStub::PrintName(StringStream* stream) {
  const char* op_name = Token::Name(op_);
  const char* overwrite_name = NULL;  // Make g++ happy.
  switch (mode_) {
    case UNARY_NO_OVERWRITE: overwrite_name = "Alloc"; break;
    case UNARY_OVERWRITE: overwrite_name = "Overwrite"; break;
  }
  stream->Add("UnaryOpStub_%s_%s_%s",
              op_name,
              overwrite_name,
              UnaryOpIC::GetName(operand_type_));
}


// TODO(svenpanne): Use virtual functions instead of switch.
void UnaryOpStub::Generate(MacroAssembler* masm) {
  switch (operand_type_) {
    case UnaryOpIC::UNINITIALIZED:
      GenerateTypeTransition(masm);
      break;
    case UnaryOpIC::SMI:
      GenerateSmiStub(masm);
      break;
    case UnaryOpIC::HEAP_NUMBER:
      GenerateHeapNumberStub(masm);
      break;
    case UnaryOpIC::GENERIC:
      GenerateGenericStub(masm);
      break;
  }
}


void UnaryOpStub::GenerateTypeTransition(MacroAssembler* masm) {
  __ LoadRR(r6_p, r3_p);  // the operand
  __ LoadSmiLiteral(r5_p, Smi::FromInt(op_));
  __ LoadSmiLiteral(r4_p, Smi::FromInt(mode_));
  __ LoadSmiLiteral(r3_p, Smi::FromInt(operand_type_));
  __ Push(r6_p, r5_p, r4_p, r3_p);

  __ TailCallExternalReference(
      ExternalReference(IC_Utility(IC::kUnaryOp_Patch), masm->isolate()), 4, 1);
}


// TODO(svenpanne): Use virtual functions instead of switch.
void UnaryOpStub::GenerateSmiStub(MacroAssembler* masm) {
  switch (op_) {
    case Token::SUB:
      GenerateSmiStubSub(masm);
      break;
    case Token::BIT_NOT:
      GenerateSmiStubBitNot(masm);
      break;
    default:
      UNREACHABLE();
  }
}


void UnaryOpStub::GenerateSmiStubSub(MacroAssembler* masm) {
  Label non_smi, slow;
  GenerateSmiCodeSub(masm, &non_smi, &slow);
  __ bind(&non_smi);
  __ bind(&slow);
  GenerateTypeTransition(masm);
}


void UnaryOpStub::GenerateSmiStubBitNot(MacroAssembler* masm) {
  Label non_smi;
  GenerateSmiCodeBitNot(masm, &non_smi);
  __ bind(&non_smi);
  GenerateTypeTransition(masm);
}


void UnaryOpStub::GenerateSmiCodeSub(MacroAssembler* masm,
                                     Label* non_smi,
                                     Label* slow) {
  __ JumpIfNotSmi(r3_p, non_smi);

  // The result of negating zero or the smallest negative smi is not a smi.
  __ TestBitRange(r3_p, kBitsPerPointer - 2, 0, r0_p);
  __ beq(slow /*, cr0*/);

  // Return '- value'.
  __ Negate(r3_p, r3_p);
  __ Ret();
}


void UnaryOpStub::GenerateSmiCodeBitNot(MacroAssembler* masm,
                                        Label* non_smi) {
  __ JumpIfNotSmi(r3_p, non_smi);

  // Flip bits and revert inverted smi-tag.
  ASSERT(kSmiTagMask == 1);
  __ notx(r3_p, r3_p);
  __ ClearRightImm(r3_p, r3_p, Operand(kSmiTagSize + kSmiShiftSize));
  __ Ret();
}


// TODO(svenpanne): Use virtual functions instead of switch.
void UnaryOpStub::GenerateHeapNumberStub(MacroAssembler* masm) {
  switch (op_) {
    case Token::SUB:
      GenerateHeapNumberStubSub(masm);
      break;
    case Token::BIT_NOT:
      GenerateHeapNumberStubBitNot(masm);
      break;
    default:
      UNREACHABLE();
  }
}


void UnaryOpStub::GenerateHeapNumberStubSub(MacroAssembler* masm) {
  Label non_smi, slow, call_builtin;
  GenerateSmiCodeSub(masm, &non_smi, &call_builtin);
  __ bind(&non_smi);
  GenerateHeapNumberCodeSub(masm, &slow);
  __ bind(&slow);
  GenerateTypeTransition(masm);
  __ bind(&call_builtin);
  GenerateGenericCodeFallback(masm);
}


void UnaryOpStub::GenerateHeapNumberStubBitNot(MacroAssembler* masm) {
  Label non_smi, slow;
  GenerateSmiCodeBitNot(masm, &non_smi);
  __ bind(&non_smi);
  GenerateHeapNumberCodeBitNot(masm, &slow);
  __ bind(&slow);
  GenerateTypeTransition(masm);
}

void UnaryOpStub::GenerateHeapNumberCodeSub(MacroAssembler* masm,
                                            Label* slow) {
  EmitCheckForHeapNumber(masm, r3_p, r4_p, r9_p, slow);
  // r3_p is a heap number.  Get a new heap number in r4_p.
  if (mode_ == UNARY_OVERWRITE) {
    __ LoadlW(r5_p, FieldMemOperand(r3_p, HeapNumber::kExponentOffset));
    __ xoris(r5_p, r5_p, Operand(HeapNumber::kSignMask >> 16));  // Flip sign.
    __ StoreW(r5_p, FieldMemOperand(r3_p, HeapNumber::kExponentOffset));
  } else {
    Label slow_allocate_heapnumber, heapnumber_allocated;
    __ AllocateHeapNumber(r4_p, r5_p, r6_p, r9_p, &slow_allocate_heapnumber);
    __ b(&heapnumber_allocated);

    __ bind(&slow_allocate_heapnumber);
    {
      FrameScope scope(masm, StackFrame::INTERNAL);
      __ push(r3_p);
      __ CallRuntime(Runtime::kNumberAlloc, 0);
      __ LoadRR(r4_p, r3_p);
      __ pop(r3_p);
    }

    __ bind(&heapnumber_allocated);
    __ LoadlW(r6_p, FieldMemOperand(r3_p, HeapNumber::kMantissaOffset));
    __ LoadlW(r5_p, FieldMemOperand(r3_p, HeapNumber::kExponentOffset));
    __ StoreW(r6_p, FieldMemOperand(r4_p, HeapNumber::kMantissaOffset));
    __ mov(r0_p, Operand(HeapNumber::kSignMask));
    __ XorP(r5_p, r0_p);
    __ StoreW(r5_p, FieldMemOperand(r4_p, HeapNumber::kExponentOffset));
    __ LoadRR(r3_p, r4_p);
  }
  __ Ret();
}


void UnaryOpStub::GenerateHeapNumberCodeBitNot(
    MacroAssembler* masm, Label* slow) {
  Label impossible;

  EmitCheckForHeapNumber(masm, r3_p, r4_p, r9_p, slow);
  // Convert the heap number in r3_p to an untagged integer in r4_p.
  __ ConvertToInt32(r3_p, r4_p, r5_p, r6_p, d0, slow);

  // Do the bitwise operation and check if the result fits in a smi.
  __ notx(r4_p, r4_p);

#if !V8_TARGET_ARCH_S390X
  Label try_float;
  __ JumpIfNotSmiCandidate(r4_p, r5_p, &try_float);
#endif

  // Tag the result as a smi and we're done.
  __ SmiTag(r3_p, r4_p);
  __ Ret();

#if !V8_TARGET_ARCH_S390X
  // Try to store the result in a heap number.
  __ bind(&try_float);
  if (mode_ == UNARY_NO_OVERWRITE) {
    Label slow_allocate_heapnumber, heapnumber_allocated;
    // Allocate a new heap number without zapping r0, which we need if it fails.
    __ AllocateHeapNumber(r5_p, r6_p, r7_p, r9_p, &slow_allocate_heapnumber);
    __ b(&heapnumber_allocated);

    __ bind(&slow_allocate_heapnumber);
    {
      FrameScope scope(masm, StackFrame::INTERNAL);
      __ push(r3_p);  // Push the heap number, not the untagged int32.
      __ CallRuntime(Runtime::kNumberAlloc, 0);
      __ LoadRR(r5_p, r3_p);  // Move the new heap number into r5_p.
      // Get the heap number into r3_p, now that the new heap number is in r5_p.
      __ pop(r3_p);
    }

    // Convert the heap number in r3_p to an untagged integer in r4_p.
    // This can't go slow-case because it's the same number we already
    // converted once again.
    __ ConvertToInt32(r3_p, r4_p, r6_p, r7_p, d0, &impossible);
    __ notx(r4_p, r4_p);

    __ bind(&heapnumber_allocated);
    __ LoadRR(r3_p, r5_p);  // Move newly allocated heap number to r0_p.
  }

  // Convert the int32 in r4_p to the heap number in r3_p.
  FloatingPointHelper::ConvertIntToDouble(
      masm, r4_p, d0);
  __ StoreF(d0, FieldMemOperand(r3_p, HeapNumber::kValueOffset));
  __ Ret();

  __ bind(&impossible);
  if (FLAG_debug_code) {
    __ stop("Incorrect assumption in bit-not stub");
  }
#endif
}


// TODO(svenpanne): Use virtual functions instead of switch.
void UnaryOpStub::GenerateGenericStub(MacroAssembler* masm) {
  switch (op_) {
    case Token::SUB:
      GenerateGenericStubSub(masm);
      break;
    case Token::BIT_NOT:
      GenerateGenericStubBitNot(masm);
      break;
    default:
      UNREACHABLE();
  }
}


void UnaryOpStub::GenerateGenericStubSub(MacroAssembler* masm) {
  Label non_smi, slow;
  GenerateSmiCodeSub(masm, &non_smi, &slow);
  __ bind(&non_smi);
  GenerateHeapNumberCodeSub(masm, &slow);
  __ bind(&slow);
  GenerateGenericCodeFallback(masm);
}


void UnaryOpStub::GenerateGenericStubBitNot(MacroAssembler* masm) {
  Label non_smi, slow;
  GenerateSmiCodeBitNot(masm, &non_smi);
  __ bind(&non_smi);
  GenerateHeapNumberCodeBitNot(masm, &slow);
  __ bind(&slow);
  GenerateGenericCodeFallback(masm);
}


void UnaryOpStub::GenerateGenericCodeFallback(MacroAssembler* masm) {
  // Handle the slow case by jumping to the JavaScript builtin.
  __ push(r3_p);
  switch (op_) {
    case Token::SUB:
      __ InvokeBuiltin(Builtins::UNARY_MINUS, JUMP_FUNCTION);
      break;
    case Token::BIT_NOT:
      __ InvokeBuiltin(Builtins::BIT_NOT, JUMP_FUNCTION);
      break;
    default:
      UNREACHABLE();
  }
}


void BinaryOpStub::GenerateTypeTransition(MacroAssembler* masm) {
  Label get_result;

  __ Push(r4_p, r3_p);

  __ LoadSmiLiteral(r5_p, Smi::FromInt(MinorKey()));
  __ LoadSmiLiteral(r4_p, Smi::FromInt(op_));
  __ LoadSmiLiteral(r3_p, Smi::FromInt(operands_type_));
  __ Push(r5_p, r4_p, r3_p);

  __ TailCallExternalReference(
      ExternalReference(IC_Utility(IC::kBinaryOp_Patch),
                        masm->isolate()),
      5,
      1);
}


void BinaryOpStub::GenerateTypeTransitionWithSavedArgs(
    MacroAssembler* masm) {
  UNIMPLEMENTED();
}


void BinaryOpStub::Generate(MacroAssembler* masm) {
  // Explicitly allow generation of nested stubs. It is safe here because
  // generation code does not use any raw pointers.
  AllowStubCallsScope allow_stub_calls(masm, true);

  switch (operands_type_) {
    case BinaryOpIC::UNINITIALIZED:
      GenerateTypeTransition(masm);
      break;
    case BinaryOpIC::SMI:
      GenerateSmiStub(masm);
      break;
    case BinaryOpIC::INT32:
      GenerateInt32Stub(masm);
      break;
    case BinaryOpIC::HEAP_NUMBER:
      GenerateHeapNumberStub(masm);
      break;
    case BinaryOpIC::ODDBALL:
      GenerateOddballStub(masm);
      break;
    case BinaryOpIC::BOTH_STRING:
      GenerateBothStringStub(masm);
      break;
    case BinaryOpIC::STRING:
      GenerateStringStub(masm);
      break;
    case BinaryOpIC::GENERIC:
      GenerateGeneric(masm);
      break;
    default:
      UNREACHABLE();
  }
}


void BinaryOpStub::PrintName(StringStream* stream) {
  const char* op_name = Token::Name(op_);
  const char* overwrite_name;
  switch (mode_) {
    case NO_OVERWRITE: overwrite_name = "Alloc"; break;
    case OVERWRITE_RIGHT: overwrite_name = "OverwriteRight"; break;
    case OVERWRITE_LEFT: overwrite_name = "OverwriteLeft"; break;
    default: overwrite_name = "UnknownOverwrite"; break;
  }
  stream->Add("BinaryOpStub_%s_%s_%s",
              op_name,
              overwrite_name,
              BinaryOpIC::GetName(operands_type_));
}


void BinaryOpStub::GenerateSmiSmiOperation(MacroAssembler* masm) {
  Register left = r4_p;
  Register right = r3_p;
  Register scratch1 = r10_p;
  Register scratch2 = r22_p;

  ASSERT(right.is(r3_p));
  STATIC_ASSERT(kSmiTag == 0);

  Label not_smi_result;
  switch (op_) {
    case Token::ADD: {
      Label undo_add, add_no_overflow;
      // C = A+B; C overflows if A/B have same sign and C has diff sign than A
      __ LoadRR(r0_p, right);
      __ XorP(r0_p, left);
      __ LoadRR(scratch1, right);
      __ Add(right, left, right);  // Add optimistically.
      __ TestSignBit(r0_p, r0_p);
      __ bne(&add_no_overflow /*, cr0*/);
      __ LoadRR(r0_p, right);
      __ XorP(r0_p, scratch1);
      __ TestSignBit(r0_p, r0_p);
      __ bne(&undo_add /*, cr0*/);
      __ bind(&add_no_overflow);
      __ Ret();
      __ bind(&undo_add);
      __ LoadRR(right, scratch1);  // Revert optimistic add.
      break;
    }
    case Token::SUB: {
      Label undo_sub, sub_no_overflow;
      // C = A-B; C overflows if A/B have diff signs and C has diff sign than A
      __ LoadRR(r0_p, right);
      __ XorP(r0_p, left);
      __ LoadRR(scratch1, right);
      __ Sub(right, left, right);  // Subtract optimistically.
      __ TestSignBit(r0_p, r0_p);
      __ beq(&sub_no_overflow /*, cr0*/);
      __ LoadRR(r0_p, right);
      __ XorP(r0_p, left);
      __ TestSignBit(r0_p, r0_p);
      __ bne(&undo_sub /*, cr0*/);
      __ bind(&sub_no_overflow);
      __ Ret();
      __ bind(&undo_sub);
      __ LoadRR(right, scratch1);  // Revert optimistic subtract.
      break;
    }
    case Token::MUL: {
      Label mul_zero, mul_neg_zero;
#if V8_TARGET_ARCH_S390X
      // Remove tag from both operands.
      __ SmiUntag(ip, right);
      __ SmiUntag(r0_p, left);
      // Do multiplication
      // scratch1 = product (untagged)
      // scratch2 = sign-extended higher 32 bits of product.
      __ Mul(scratch1, r0_p, ip);
      // Check for overflowing the smi range - no overflow if higher 33 bits of
      // the result are identical.
      __ TestIfInt32(scratch1, scratch2, ip);
      __ bne(&not_smi_result);
#else
      // Remove tag from one of the operands. This way the multiplication result
      // will be a smi if it fits the smi range.
      __ SmiUntag(ip, right);
      // Do multiplication
      // scratch1 = lower 32 bits of product.
      // scratch2 = higher 32 bits of product.
      __ mullw(scratch1, left, ip);
      __ mulhw(scratch2, left, ip);
      // Check for overflowing the smi range - no overflow if higher 33 bits of
      // the result are identical.
      __ TestIfInt32(scratch2, scratch1, ip);
      __ bne(&not_smi_result);
#endif
      // Go slow on zero result to handle -0.
      __ Cmpi(scratch1, Operand::Zero());
      __ beq(&mul_zero);
#if V8_TARGET_ARCH_S390X
      __ SmiTag(right, scratch1);
#else
      __ LoadRR(right, scratch1);
#endif
      __ Ret();
      __ bind(&mul_zero);
      // We need -0 if we were multiplying a negative number with 0 to get 0.
      // We know one of them was zero.
      __ Add(scratch2, right, left);
      __ Cmpi(scratch2, Operand::Zero());
      __ blt(&mul_neg_zero);
      __ LoadSmiLiteral(right, Smi::FromInt(0));
      __ Ret();  // Return smi 0 if the non-zero one was positive.
      __ bind(&mul_neg_zero);
      // We fall through here if we multiplied a negative number with 0, because
      // that would mean we should produce -0.
      break;
    }
    case Token::DIV: {
      Label check_neg_zero;
      __ SmiUntag(ip, left);
      __ SmiUntag(scratch2, right, SetRC);
      __ Div(scratch1, ip, scratch2);
      // Check for zero on the right hand side.
      __ beq(&not_smi_result /*, cr0*/);
      // Not Smi if remainder is non-zero.
      __ Mul(scratch2, scratch2, scratch1);
      __ CmpRR(ip, scratch2);
      __ bne(&not_smi_result);
      // If the result is 0, we need to check for the -0 case.
      __ SmiTag(scratch2, scratch1/*, SetRC*/);  // already set in s390
      __ beq(&check_neg_zero /*, cr0*/);  // should be save to do so
      // Check for Smi overflow
      __ XorP(scratch1, scratch2/*, SetRC*/);  // Safe to remove rc
      __ blt(&not_smi_result /*, cr0*/);
      __ LoadRR(right, scratch2);
      __ Ret();

      // If divisor (right) is negative, we must produce -0.
      __ bind(&check_neg_zero);
      __ Cmpi(right, Operand::Zero());
      __ blt(&not_smi_result);
      __ LoadRR(right, scratch2);
      __ Ret();
      break;
    }
    case Token::MOD: {
      Label check_neg_zero;
      __ SmiUntag(ip, left);
      __ SmiUntag(scratch2, right, SetRC);
      __ Div(scratch1, ip, scratch2);
      // Check for zero on the right hand side.
      __ beq(&not_smi_result /*, cr0*/);
      __ Mul(scratch1, scratch2, scratch1);
      __ Sub(scratch1, ip, scratch1/*, LeaveOE, SetRC*/);
      // Removing RC looks okay.
      // If the result is 0, we need to check for the -0 case.
      __ beq(&check_neg_zero /*, cr0*/);
#if !V8_TARGET_ARCH_S390X
      // Check that the signed result fits in a Smi.
      __ JumpIfNotSmiCandidate(scratch1, scratch2, &not_smi_result);
#endif
      __ SmiTag(right, scratch1);
      __ Ret();

      // If dividend (left) is negative, we must produce -0.
      __ bind(&check_neg_zero);
      __ Cmpi(left, Operand::Zero());
      __ blt(&not_smi_result);
      __ LoadSmiLiteral(right, Smi::FromInt(0));
      __ Ret();
      break;
    }
    case Token::BIT_OR:
      __ OrP(right, left);
      __ Ret();
      break;
    case Token::BIT_AND:
      __ AndP(right, left);
      __ Ret();
      break;
    case Token::BIT_XOR:
      __ XorP(right, left);
      __ Ret();
      break;
    case Token::SAR:
      // Remove tags from right operand.
      __ GetLeastBitsFromSmi(scratch1, right, 5);
      __ ShiftRightArith(right, left, scratch1);
      // Smi tag result.
      __ ClearRightImm(right, right, Operand(kSmiTagSize + kSmiShiftSize));
      __ Ret();
      break;
    case Token::SHR:
      // Remove tags from operands. We can't do this on a 31 bit number
      // because then the 0s get shifted into bit 30 instead of bit 31.
      __ SmiUntag(scratch1, left);
      __ GetLeastBitsFromSmi(scratch2, right, 5);
      __ srw(scratch1, scratch1, scratch2);
      // Unsigned shift is not allowed to produce a negative number.
      __ JumpIfNotUnsignedSmiCandidate(scratch1, r0_p, &not_smi_result);
      // Smi tag result.
      __ SmiTag(right, scratch1);
      __ Ret();
      break;
    case Token::SHL:
      // Remove tags from operands.
      __ SmiUntag(scratch1, left);
      __ GetLeastBitsFromSmi(scratch2, right, 5);
      __ ShiftLeft(scratch1, scratch1, scratch2);
#if !V8_TARGET_ARCH_S390X
      // Check that the signed result fits in a Smi.
      __ JumpIfNotSmiCandidate(scratch1, scratch2, &not_smi_result);
#endif
      __ SmiTag(right, scratch1);
      __ Ret();
      break;
    default:
      UNREACHABLE();
  }
  __ bind(&not_smi_result);
}


void BinaryOpStub::GenerateFPOperation(MacroAssembler* masm,
                                       bool smi_operands,
                                       Label* not_numbers,
                                       Label* gc_required) {
  Register left = r4_p;
  Register right = r3_p;
  Register scratch1 = r10_p;
  Register scratch2 = r22_p;
  Register scratch3 = r7_p;

  ASSERT(smi_operands || (not_numbers != NULL));
  if (smi_operands) {
    __ AssertSmi(left);
    __ AssertSmi(right);
  }

  Register heap_number_map = r9_p;
  __ LoadRoot(heap_number_map, Heap::kHeapNumberMapRootIndex);

  switch (op_) {
    case Token::ADD:
    case Token::SUB:
    case Token::MUL:
    case Token::DIV:
    case Token::MOD: {
      // Load left and right operands into d1 and d2
      // Allocate new heap number for result.
      Register result = r8_p;
      GenerateHeapResultAllocation(
          masm, result, heap_number_map, scratch1, scratch2, gc_required);

      // Load the operands.
      if (smi_operands) {
        FloatingPointHelper::LoadSmis(masm, scratch1, scratch2);
      } else {
        FloatingPointHelper::LoadOperands(masm,
                                          heap_number_map,
                                          scratch1,
                                          scratch2,
                                          not_numbers);
      }

      // Calculate the result.
      // Using FP registers:
      //   d1: Left value
      //   d2: Right value
      switch (op_) {
        case Token::ADD:
          __ adbr(d1, d2);
          break;
        case Token::SUB:
          __ sdbr(d1, d2);
          break;
        case Token::MUL:
          __ mdbr(d1, d2);
          break;
        case Token::DIV:
          __ ddbr(d1, d2);
          break;
        case Token::MOD:
          // Call the C function to handle the double operation.
          FloatingPointHelper::CallCCodeForDoubleOperation(masm,
                                                           op_,
                                                           result,
                                                           scratch1);
          if (FLAG_debug_code) {
            __ stop("Unreachable code.");
          }
          break;
        default:
          UNREACHABLE();
      }
      __ StoreF(d1, FieldMemOperand(result, HeapNumber::kValueOffset));
      __ LoadRR(r3_p, result);
      __ Ret();
      break;
    }
    case Token::BIT_OR:
    case Token::BIT_XOR:
    case Token::BIT_AND:
    case Token::SAR:
    case Token::SHR:
    case Token::SHL: {
      if (smi_operands) {
        __ SmiUntag(r6_p, left);
        __ SmiUntag(r5_p, right);
      } else {
        // Convert operands to 32-bit integers. Right in r5_p and left in r6_p.
        FloatingPointHelper::ConvertNumberToInt32(masm,
                                                  left,
                                                  r6_p,
                                                  heap_number_map,
                                                  scratch1,
                                                  scratch2,
                                                  scratch3,
                                                  d0,
                                                  not_numbers);
        FloatingPointHelper::ConvertNumberToInt32(masm,
                                                  right,
                                                  r5_p,
                                                  heap_number_map,
                                                  scratch1,
                                                  scratch2,
                                                  scratch3,
                                                  d0,
                                                  not_numbers);
      }

      Label result_not_a_smi;
      switch (op_) {
        case Token::BIT_OR:
          __ OrP(r5_p, r6_p);
          break;
        case Token::BIT_XOR:
          __ XorP(r5_p, r6_p);
          break;
        case Token::BIT_AND:
          __ AndP(r5_p, r6_p);
          break;
        case Token::SAR:
          // Use only the 5 least significant bits of the shift count.
          __ GetLeastBitsFromInt32(r5_p, r5_p, 5);
          __ sraw(r5_p, r6_p, r5_p);
          break;
        case Token::SHR:
        {
          // Use only the 5 least significant bits of the shift count.
          __ GetLeastBitsFromInt32(r5_p, r5_p, 5);
          // SHR is special because it is required to produce a positive answer.
          // The code below for writing into heap numbers isn't capable of
          // writing the register as an unsigned int so we go to slow case if we
          // hit this case.
#if V8_TARGET_ARCH_S390X
          const Condition cond = ne;
          __ srw(r5_p, r6_p, r5_p);
          __ TestSignBit32(r5_p, r0_p);
#else
          const Condition cond = lt;
          __ srw(r5_p, r6_p, r5_p, SetRC);
#endif
          __ b(cond, &result_not_a_smi /*, cr0*/);
          break;
        }
        case Token::SHL:
          // Use only the 5 least significant bits of the shift count.
          __ GetLeastBitsFromInt32(r5_p, r5_p, 5);
          __ ShiftLeft(r5_p, r6_p, r5_p);
          break;
        default:
          UNREACHABLE();
      }

#if !V8_TARGET_ARCH_S390X
      // Check that the *signed* result fits in a smi.
      __ JumpIfNotSmiCandidate(r5_p, r6_p, &result_not_a_smi);
#endif
      __ SmiTag(r3_p, r5_p);
      __ Ret();

      // Allocate new heap number for result.
      __ bind(&result_not_a_smi);
      Register result = r8_p;
      if (smi_operands) {
        __ AllocateHeapNumber(
            result, scratch1, scratch2, heap_number_map, gc_required);
      } else {
        GenerateHeapResultAllocation(
            masm, result, heap_number_map, scratch1, scratch2, gc_required);
      }

      // r5_p: Answer as signed int32.
      // r8_p: Heap number to write answer into.

      // Nothing can go wrong now, so move the heap number to r3_p, which is the
      // result.
      __ LoadRR(r3_p, r8_p);

      // Convert the int32 in r5_p to the heap number in r3_p. As
      // mentioned above SHR needs to always produce a positive result.
      if (op_ == Token::SHR) {
        FloatingPointHelper::ConvertUnsignedIntToDouble(
          masm, r5_p, d0);
      } else {
        FloatingPointHelper::ConvertIntToDouble(
          masm, r5_p, d0);
      }
      __ StoreF(d0, FieldMemOperand(r3_p, HeapNumber::kValueOffset));
      __ Ret();
      break;
    }
    default:
      UNREACHABLE();
  }
}


// Generate the smi code. If the operation on smis are successful this return is
// generated. If the result is not a smi and heap number allocation is not
// requested the code falls through. If number allocation is requested but a
// heap number cannot be allocated the code jumps to the lable gc_required.
void BinaryOpStub::GenerateSmiCode(
    MacroAssembler* masm,
    Label* use_runtime,
    Label* gc_required,
    SmiCodeGenerateHeapNumberResults allow_heapnumber_results) {
  Label not_smis;

  Register left = r4_p;
  Register right = r3_p;
  Register scratch1 = r10_p;

  // Perform combined smi check on both operands.
  __ LoadRR(scratch1, right);
  __ OrP(scratch1, left);
  STATIC_ASSERT(kSmiTag == 0);
  __ JumpIfNotSmi(scratch1, &not_smis);

  // If the smi-smi operation results in a smi return is generated.
  GenerateSmiSmiOperation(masm);

  // If heap number results are possible generate the result in an allocated
  // heap number.
  if (allow_heapnumber_results == ALLOW_HEAPNUMBER_RESULTS) {
    GenerateFPOperation(masm, true, use_runtime, gc_required);
  }
  __ bind(&not_smis);
}


void BinaryOpStub::GenerateSmiStub(MacroAssembler* masm) {
  Label not_smis, call_runtime;

  if (result_type_ == BinaryOpIC::UNINITIALIZED ||
      result_type_ == BinaryOpIC::SMI) {
    // Only allow smi results.
    GenerateSmiCode(masm, &call_runtime, NULL, NO_HEAPNUMBER_RESULTS);
  } else {
    // Allow heap number result and don't make a transition if a heap number
    // cannot be allocated.
    GenerateSmiCode(masm,
                    &call_runtime,
                    &call_runtime,
                    ALLOW_HEAPNUMBER_RESULTS);
  }

  // Code falls through if the result is not returned as either a smi or heap
  // number.
  GenerateTypeTransition(masm);

  __ bind(&call_runtime);
  GenerateCallRuntime(masm);
}


void BinaryOpStub::GenerateStringStub(MacroAssembler* masm) {
  ASSERT(operands_type_ == BinaryOpIC::STRING);
  ASSERT(op_ == Token::ADD);
  // Try to add arguments as strings, otherwise, transition to the generic
  // BinaryOpIC type.
  GenerateAddStrings(masm);
  GenerateTypeTransition(masm);
}


void BinaryOpStub::GenerateBothStringStub(MacroAssembler* masm) {
  Label call_runtime;
  ASSERT(operands_type_ == BinaryOpIC::BOTH_STRING);
  ASSERT(op_ == Token::ADD);
  // If both arguments are strings, call the string add stub.
  // Otherwise, do a transition.

  // Registers containing left and right operands respectively.
  Register left = r4_p;
  Register right = r3_p;

  // Test if left operand is a string.
  __ JumpIfSmi(left, &call_runtime);
  __ CompareObjectType(left, r5_p, r5_p, FIRST_NONSTRING_TYPE);
  __ bge(&call_runtime);

  // Test if right operand is a string.
  __ JumpIfSmi(right, &call_runtime);
  __ CompareObjectType(right, r5_p, r5_p, FIRST_NONSTRING_TYPE);
  __ bge(&call_runtime);

  StringAddStub string_add_stub(NO_STRING_CHECK_IN_STUB);
  GenerateRegisterArgsPush(masm);
  __ TailCallStub(&string_add_stub);

  __ bind(&call_runtime);
  GenerateTypeTransition(masm);
}


void BinaryOpStub::GenerateInt32Stub(MacroAssembler* masm) {
  ASSERT(operands_type_ == BinaryOpIC::INT32);

  Register left = r4_p;
  Register right = r3_p;
  Register scratch1 = r10_p;
  Register scratch2 = r11_p;
  DwVfpRegister double_scratch0 = d0;
  DwVfpRegister double_scratch1 = d1;

  Register heap_number_result = no_reg;
  Register heap_number_map = r9_p;
  __ LoadRoot(heap_number_map, Heap::kHeapNumberMapRootIndex);

  Label call_runtime;
  // Labels for type transition, used for wrong input or output types.
  // Both label are currently actually bound to the same position. We use two
  // different label to differentiate the cause leading to type transition.
  Label transition;

  // Smi-smi fast case.
  Label skip;
  __ LoadRR(scratch1, right);
  __ OrP(scratch1, left);
  __ JumpIfNotSmi(scratch1, &skip);
  GenerateSmiSmiOperation(masm);
  // Fall through if the result is not a smi.
  __ bind(&skip);

  switch (op_) {
    case Token::ADD:
    case Token::SUB:
    case Token::MUL:
    case Token::DIV:
    case Token::MOD: {
      // Load both operands and check that they are 32-bit integer.
      // Jump to type transition if they are not. The registers r3_p and r4_p
      // (right and left) are preserved for the runtime call.
      FloatingPointHelper::LoadNumberAsInt32Double(masm,
                                                   right,
                                                   d2,
                                                   d8,
                                                   heap_number_map,
                                                   scratch1,
                                                   scratch2,
                                                   &transition);
      FloatingPointHelper::LoadNumberAsInt32Double(masm,
                                                   left,
                                                   d1,
                                                   d8,
                                                   heap_number_map,
                                                   scratch1,
                                                   scratch2,
                                                   &transition);

      Label return_heap_number;
      switch (op_) {
        case Token::ADD:
          __ adbr(d1, d2);
          break;
        case Token::SUB:
          __ sdbr(d1, d2);
          break;
        case Token::MUL:
          __ mdbr(d1, d2);
          break;
        case Token::DIV:
          __ ddbr(d1, d2);
          break;
        case Token::MOD: {
          Label pop_and_call_runtime;

          // Allocate a heap number to store the result.
          heap_number_result = r8_p;
          GenerateHeapResultAllocation(masm,
                                       heap_number_result,
                                       heap_number_map,
                                       scratch1,
                                       scratch2,
                                       &pop_and_call_runtime);

          // Call the C function to handle the double operation.
          FloatingPointHelper::CallCCodeForDoubleOperation(
              masm, op_, heap_number_result, scratch1);
          if (FLAG_debug_code) {
            __ stop("Unreachable code.");
          }

          __ bind(&pop_and_call_runtime);
          __ b(&call_runtime);
          break;
        }
        default:
          UNREACHABLE();
      }

      if (op_ != Token::DIV) {
        // These operations produce an integer result.
        // Try to return a smi if we can.
        // Otherwise return a heap number if allowed, or jump to type
        // transition.

        __ EmitVFPTruncate(kRoundToZero,
                           scratch1,
                           d1,
                           scratch2,
                           d8);

        // result does not fit in a 32-bit integer.
        Label *not_int32 = ((result_type_ <= BinaryOpIC::INT32) ?
                            &transition : &return_heap_number);
        __ b(Condition(CC_OF), not_int32);

#if !V8_TARGET_ARCH_S390X
        // Check if the result fits in a smi.
        // If not try to return a heap number.
        __ JumpIfNotSmiCandidate(scratch1, scratch2, &return_heap_number);
#endif
        // Check for minus zero. Return heap number for minus zero.
        Label not_zero;
        __ Cmpi(scratch1, Operand::Zero());
        __ bne(&not_zero);

        __ Sub(sp, Operand(8));
        __ StoreF(d1, MemOperand(sp, 0));
#if V8_TARGET_ARCH_S390X
        __ ld(scratch2, MemOperand(sp, 0));
#else
#if __FLOAT_WORD_ORDER == __LITTLE_ENDIAN
        __ LoadlW(scratch2, MemOperand(sp, 4));
#else
        __ LoadlW(scratch2, MemOperand(sp, 0));
#endif
#endif
        __ AddP(sp, Operand(8));

        __ TestSignBit(scratch2, r0_p);
        __ bne(&return_heap_number /*, cr0*/);
        __ bind(&not_zero);

        // Tag the result and return.
        __ SmiTag(r3_p, scratch1);
        __ Ret();
      } else {
        // DIV just falls through to allocating a heap number.
      }

      __ bind(&return_heap_number);
      // Return a heap number, or fall through to type transition or runtime
      // call if we can't.
      if (result_type_ >= ((op_ == Token::DIV) ? BinaryOpIC::HEAP_NUMBER
                                               : BinaryOpIC::INT32)) {
        heap_number_result = r8_p;
        GenerateHeapResultAllocation(masm,
                                     heap_number_result,
                                     heap_number_map,
                                     scratch1,
                                     scratch2,
                                     &call_runtime);
        __ StoreF(d1, FieldMemOperand(heap_number_result,
                                    HeapNumber::kValueOffset));
        __ LoadRR(r3_p, heap_number_result);
        __ Ret();
      }

      // A DIV operation expecting an integer result falls through
      // to type transition.
      break;
    }

    case Token::BIT_OR:
    case Token::BIT_XOR:
    case Token::BIT_AND:
    case Token::SAR:
    case Token::SHR:
    case Token::SHL: {
      Label return_heap_number;
      Register scratch3 = r8_p;
      // Convert operands to 32-bit integers. Right in r5_p and left in r6_p.
      // The registers r3_p and r4_p (right and left) are preserved for the
      // runtime call.
      FloatingPointHelper::LoadNumberAsInt32(masm,
                                             left,
                                             r6_p,
                                             heap_number_map,
                                             scratch1,
                                             scratch2,
                                             scratch3,
                                             double_scratch0,
                                             double_scratch1,
                                             &transition);
      FloatingPointHelper::LoadNumberAsInt32(masm,
                                             right,
                                             r5_p,
                                             heap_number_map,
                                             scratch1,
                                             scratch2,
                                             scratch3,
                                             double_scratch0,
                                             double_scratch1,
                                             &transition);

      // The ECMA-262 standard specifies that, for shift operations, only the
      // 5 least significant bits of the shift value should be used.
      switch (op_) {
        case Token::BIT_OR:
          __ OrP(r5_p, r6_p);
          break;
        case Token::BIT_XOR:
          __ XorP(r5_p, r6_p);
          break;
        case Token::BIT_AND:
          __ AndP(r5_p, r6_p);
          break;
        case Token::SAR:
          __ GetLeastBitsFromInt32(r5_p, r5_p, 5);
          __ sraw(r5_p, r6_p, r5_p);
          break;
        case Token::SHR:
        {
          __ GetLeastBitsFromInt32(r5_p, r5_p, 5);
          // SHR is special because it is required to produce a positive answer.
          // We only get a negative result if the shift value (r5_p) is 0.
          // This result cannot be respresented as a signed 32-bit integer, try
          // to return a heap number if we can.
#if V8_TARGET_ARCH_S390X
          const Condition cond = ne;
          __ srw(r5_p, r6_p, r5_p);
          __ TestSignBit32(r5_p, r0_p);
#else
          const Condition cond = lt;
          __ srw(r5_p, r6_p, r5_p, SetRC);
#endif
          __ b(cond, ((result_type_ <= BinaryOpIC::INT32)
                      ? &transition
                      : &return_heap_number) /*, cr0*/);
          break;
        }
        case Token::SHL:
          __ AndP(r5_p, Operand(0x1f));
          __ ShiftLeft(r5_p, r6_p, r5_p);
          break;
        default:
          UNREACHABLE();
      }

#if !V8_TARGET_ARCH_S390X
      // Check if the result fits in a smi.
      // If not try to return a heap number. (We know the result is an int32.)
      __ JumpIfNotSmiCandidate(r5_p, scratch1, &return_heap_number);
#endif
      // Tag the result and return.
      __ SmiTag(r3_p, r5_p);
      __ Ret();

      __ bind(&return_heap_number);
      heap_number_result = r8_p;
      GenerateHeapResultAllocation(masm,
                                   heap_number_result,
                                   heap_number_map,
                                   scratch1,
                                   scratch2,
                                   &call_runtime);

      if (op_ != Token::SHR) {
        // Convert the result to a floating point value.
        FloatingPointHelper::ConvertIntToDouble(masm, r5_p, double_scratch0);
      } else {
        // The result must be interpreted as an unsigned 32-bit integer.
        FloatingPointHelper::ConvertUnsignedIntToDouble(masm, r5_p,
                                                        double_scratch0);
      }

      // Store the result.
      __ StoreF(double_scratch0, FieldMemOperand(heap_number_result,
                                               HeapNumber::kValueOffset));
      __ LoadRR(r3_p, heap_number_result);
      __ Ret();

      break;
    }

    default:
      UNREACHABLE();
  }

  // We never expect DIV to yield an integer result, so we always generate
  // type transition code for DIV operations expecting an integer result: the
  // code will fall through to this type transition.
  if (transition.is_linked() ||
      ((op_ == Token::DIV) && (result_type_ <= BinaryOpIC::INT32))) {
    __ bind(&transition);
    GenerateTypeTransition(masm);
  }

  __ bind(&call_runtime);
  GenerateCallRuntime(masm);
}


void BinaryOpStub::GenerateOddballStub(MacroAssembler* masm) {
  Label call_runtime;

  if (op_ == Token::ADD) {
    // Handle string addition here, because it is the only operation
    // that does not do a ToNumber conversion on the operands.
    GenerateAddStrings(masm);
  }

  // Convert oddball arguments to numbers.
  Label check, done;
  __ CompareRoot(r4_p, Heap::kUndefinedValueRootIndex);
  __ bne(&check);
  if (Token::IsBitOp(op_)) {
    __ LoadSmiLiteral(r4_p, Smi::FromInt(0));
  } else {
    __ LoadRoot(r4_p, Heap::kNanValueRootIndex);
  }
  __ b(&done);
  __ bind(&check);
  __ CompareRoot(r3_p, Heap::kUndefinedValueRootIndex);
  __ bne(&done);
  if (Token::IsBitOp(op_)) {
    __ LoadSmiLiteral(r3_p, Smi::FromInt(0));
  } else {
    __ LoadRoot(r3_p, Heap::kNanValueRootIndex);
  }
  __ bind(&done);

  GenerateHeapNumberStub(masm);
}


void BinaryOpStub::GenerateHeapNumberStub(MacroAssembler* masm) {
  Label call_runtime;
  GenerateFPOperation(masm, false, &call_runtime, &call_runtime);

  __ bind(&call_runtime);
  GenerateCallRuntime(masm);
}


void BinaryOpStub::GenerateGeneric(MacroAssembler* masm) {
  Label call_runtime, call_string_add_or_runtime;

  GenerateSmiCode(masm, &call_runtime, &call_runtime, ALLOW_HEAPNUMBER_RESULTS);

  GenerateFPOperation(masm, false, &call_string_add_or_runtime, &call_runtime);

  __ bind(&call_string_add_or_runtime);
  if (op_ == Token::ADD) {
    GenerateAddStrings(masm);
  }

  __ bind(&call_runtime);
  GenerateCallRuntime(masm);
}


void BinaryOpStub::GenerateAddStrings(MacroAssembler* masm) {
  ASSERT(op_ == Token::ADD);
  Label left_not_string, call_runtime;

  Register left = r4_p;
  Register right = r3_p;

  // Check if left argument is a string.
  __ JumpIfSmi(left, &left_not_string);
  __ CompareObjectType(left, r5_p, r5_p, FIRST_NONSTRING_TYPE);
  __ bge(&left_not_string);

  StringAddStub string_add_left_stub(NO_STRING_CHECK_LEFT_IN_STUB);
  GenerateRegisterArgsPush(masm);
  __ TailCallStub(&string_add_left_stub);

  // Left operand is not a string, test right.
  __ bind(&left_not_string);
  __ JumpIfSmi(right, &call_runtime);
  __ CompareObjectType(right, r5_p, r5_p, FIRST_NONSTRING_TYPE);
  __ bge(&call_runtime);

  StringAddStub string_add_right_stub(NO_STRING_CHECK_RIGHT_IN_STUB);
  GenerateRegisterArgsPush(masm);
  __ TailCallStub(&string_add_right_stub);

  // At least one argument is not a string.
  __ bind(&call_runtime);
}


void BinaryOpStub::GenerateCallRuntime(MacroAssembler* masm) {
  GenerateRegisterArgsPush(masm);
  switch (op_) {
    case Token::ADD:
      __ InvokeBuiltin(Builtins::ADD, JUMP_FUNCTION);
      break;
    case Token::SUB:
      __ InvokeBuiltin(Builtins::SUB, JUMP_FUNCTION);
      break;
    case Token::MUL:
      __ InvokeBuiltin(Builtins::MUL, JUMP_FUNCTION);
      break;
    case Token::DIV:
      __ InvokeBuiltin(Builtins::DIV, JUMP_FUNCTION);
      break;
    case Token::MOD:
      __ InvokeBuiltin(Builtins::MOD, JUMP_FUNCTION);
      break;
    case Token::BIT_OR:
      __ InvokeBuiltin(Builtins::BIT_OR, JUMP_FUNCTION);
      break;
    case Token::BIT_AND:
      __ InvokeBuiltin(Builtins::BIT_AND, JUMP_FUNCTION);
      break;
    case Token::BIT_XOR:
      __ InvokeBuiltin(Builtins::BIT_XOR, JUMP_FUNCTION);
      break;
    case Token::SAR:
      __ InvokeBuiltin(Builtins::SAR, JUMP_FUNCTION);
      break;
    case Token::SHR:
      __ InvokeBuiltin(Builtins::SHR, JUMP_FUNCTION);
      break;
    case Token::SHL:
      __ InvokeBuiltin(Builtins::SHL, JUMP_FUNCTION);
      break;
    default:
      UNREACHABLE();
  }
}


void BinaryOpStub::GenerateHeapResultAllocation(MacroAssembler* masm,
                                                Register result,
                                                Register heap_number_map,
                                                Register scratch1,
                                                Register scratch2,
                                                Label* gc_required) {
  // Code below will scratch result if allocation fails. To keep both arguments
  // intact for the runtime call result cannot be one of these.
  ASSERT(!result.is(r3_p) && !result.is(r4_p));

  if (mode_ == OVERWRITE_LEFT || mode_ == OVERWRITE_RIGHT) {
    Label skip_allocation, allocated;
    Register overwritable_operand = mode_ == OVERWRITE_LEFT ? r4_p : r3_p;
    // If the overwritable operand is already an object, we skip the
    // allocation of a heap number.
    __ JumpIfNotSmi(overwritable_operand, &skip_allocation);
    // Allocate a heap number for the result.
    __ AllocateHeapNumber(
        result, scratch1, scratch2, heap_number_map, gc_required);
    __ b(&allocated);
    __ bind(&skip_allocation);
    // Use object holding the overwritable operand for result.
    __ LoadRR(result, overwritable_operand);
    __ bind(&allocated);
  } else {
    ASSERT(mode_ == NO_OVERWRITE);
    __ AllocateHeapNumber(
        result, scratch1, scratch2, heap_number_map, gc_required);
  }
}


void BinaryOpStub::GenerateRegisterArgsPush(MacroAssembler* masm) {
  __ Push(r4_p, r3_p);
}


void TranscendentalCacheStub::Generate(MacroAssembler* masm) {
  // Untagged case: double input in d2, double result goes
  //   into d2.
  // Tagged case: tagged input on top of stack and in r3_p,
  //   tagged result (heap number) goes into r3_p.

  Label input_not_smi;
  Label loaded;
  Label calculate;
  Label invalid_cache;
  const Register scratch0 = r22_p;
  const Register scratch1 = r10_p;
  const Register cache_entry = r3_p;
  const bool tagged = (argument_type_ == TAGGED);

  if (tagged) {
    // Argument is a number and is on stack and in r3_p.
    // Load argument and check if it is a smi.
    __ JumpIfNotSmi(r3_p, &input_not_smi);

    // Input is a smi. Convert to double and load the low and high words
    // of the double into r5_p, r6_p.
    __ SmiToDoubleFPRegister(r3_p, d6, scratch0);
    __ Sub(sp, Operand(8));
    __ StoreF(d6, MemOperand(sp, 0));
#if __FLOAT_WORD_ORDER == __LITTLE_ENDIAN
    __ LoadlW(r5_p, MemOperand(sp));
    __ LoadlW(r6_p, MemOperand(sp, 4));
#else
    __ LoadlW(r5_p, MemOperand(sp, 4));
    __ LoadlW(r6_p, MemOperand(sp));
#endif
    __ AddP(sp, Operand(8));
    __ b(&loaded);

    __ bind(&input_not_smi);
    // Check if input is a HeapNumber.
    __ CheckMap(r3_p,
                r4_p,
                Heap::kHeapNumberMapRootIndex,
                &calculate,
                DONT_DO_SMI_CHECK);
    // Input is a HeapNumber. Load it to a double register and store the
    // low and high words into r5_p, r6_p.
    __ LoadlW(r6_p, FieldMemOperand(r3_p, HeapNumber::kExponentOffset));
    __ LoadlW(r5_p, FieldMemOperand(r3_p, HeapNumber::kMantissaOffset));
  } else {
    // Input is untagged double in d2. Output goes to d2.
    __ Sub(sp, Operand(8));
    __ StoreF(d2, MemOperand(sp, 0));
#if __FLOAT_WORD_ORDER == __LITTLE_ENDIAN
    __ LoadlW(r5_p, MemOperand(sp, 4));
    __ LoadlW(r6_p, MemOperand(sp));
#else
    __ LoadlW(r5_p, MemOperand(sp));
    __ LoadlW(r6_p, MemOperand(sp, 4));
#endif
    __ AddP(sp, Operand(8));
  }
    __ bind(&loaded);
  // r5_p = low 32 bits of double value
  // r6_p = high 32 bits of double value
  // Compute hash (the shifts are arithmetic):
  //   h = (low ^ high); h ^= h >> 16; h ^= h >> 8; h = h & (cacheSize - 1);
  __ LoadRR(r4_p, r6_p);
  __ XorP(r4_p, r5_p);
  __ srawi(scratch0, r4_p, 16);
  __ XorP(r4_p, scratch0);
  __ srawi(scratch0, r4_p, 8);
  __ XorP(r4_p, scratch0);
  ASSERT(IsPowerOf2(TranscendentalCache::SubCache::kCacheSize));
  __ AndP(r4_p, Operand(TranscendentalCache::SubCache::kCacheSize - 1));

  // r5_p = low 32 bits of double value.
  // r6_p = high 32 bits of double value.
  // r4_p = TranscendentalCache::hash(double value).
  Isolate* isolate = masm->isolate();
  ExternalReference cache_array =
      ExternalReference::transcendental_cache_array_address(isolate);
  __ mov(cache_entry, Operand(cache_array));
  // cache_entry points to cache array.
  int cache_array_index
      = type_ * sizeof(isolate->transcendental_cache()->caches_[0]);
  __ LoadP(cache_entry, MemOperand(cache_entry, cache_array_index), r0_p);
  // r3_p points to the cache for the type type_.
  // If NULL, the cache hasn't been initialized yet, so go through runtime.
  __ Cmpi(cache_entry, Operand(0, RelocInfo::NONE));
  __ beq(&invalid_cache);

#ifdef DEBUG
  // Check that the layout of cache elements match expectations.
  { TranscendentalCache::SubCache::Element test_elem[2];
    char* elem_start = reinterpret_cast<char*>(&test_elem[0]);
    char* elem2_start = reinterpret_cast<char*>(&test_elem[1]);
    char* elem_in0 = reinterpret_cast<char*>(&(test_elem[0].in[0]));
    char* elem_in1 = reinterpret_cast<char*>(&(test_elem[0].in[1]));
    char* elem_out = reinterpret_cast<char*>(&(test_elem[0].output));
    // Two uint_32's and a pointer.
#if V8_TARGET_ARCH_S390X
    CHECK_EQ(16, static_cast<int>(elem2_start - elem_start));
#else
    CHECK_EQ(12, static_cast<int>(elem2_start - elem_start));
#endif
    CHECK_EQ(0, static_cast<int>(elem_in0 - elem_start));
    CHECK_EQ(kIntSize, static_cast<int>(elem_in1 - elem_start));
    CHECK_EQ(2 * kIntSize, static_cast<int>(elem_out - elem_start));
  }
#endif

#if V8_TARGET_ARCH_S390X
  // Find the address of the r4_p'th entry in the cache, i.e., &r3_p[r4_p*16].
  __ ShiftLeftImm(scratch0, r4_p, Operand(4));
#else
  // Find the address of the r4_p'th entry in the cache, i.e., &r3_p[r4_p*12].
  __ ShiftLeftImm(scratch0, r4_p, Operand(1));
  __ AddP(r4_p, scratch0);
  __ ShiftLeftImm(scratch0, r4_p, Operand(2));
#endif
  __ AddP(cache_entry, scratch0);
  // Check if cache matches: Double value is stored in uint32_t[2] array.
  __ LoadlW(r7_p, MemOperand(cache_entry, 0));
  __ LoadlW(r8_p, MemOperand(cache_entry, 4));
  __ LoadP(r9_p, MemOperand(cache_entry, 8));
  __ CmpRR(r5_p, r7_p);
  __ bne(&calculate);
  __ CmpRR(r6_p, r8_p);
  __ bne(&calculate);
  // Cache hit. Load result, cleanup and return.
  Counters* counters = masm->isolate()->counters();
  __ IncrementCounter(
      counters->transcendental_cache_hit(), 1, scratch0, scratch1);
  if (tagged) {
    // Pop input value from stack and load result into r3_p.
    __ pop();
    __ LoadRR(r3_p, r9_p);
  } else {
    // Load result into d2.
    __ LoadF(d2, FieldMemOperand(r9_p, HeapNumber::kValueOffset));
  }
  __ Ret();

  __ bind(&calculate);
  __ IncrementCounter(
      counters->transcendental_cache_miss(), 1, scratch0, scratch1);
  if (tagged) {
    __ bind(&invalid_cache);
    ExternalReference runtime_function =
        ExternalReference(RuntimeFunction(), masm->isolate());
    __ TailCallExternalReference(runtime_function, 1, 1);
  } else {
    Label no_update;
    Label skip_cache;

    // Call C function to calculate the result and update the cache.
    // r3_p: precalculated cache entry address.
    // r5_p and r6_p: parts of the double value.
    // Store r3_p, r5_p and r6_p on stack for later before calling C function.
    __ Push(r6_p, r5_p, cache_entry);
    GenerateCallCFunction(masm, scratch0);
    __ GetCFunctionDoubleResult(d2);

    // Try to update the cache. If we cannot allocate a
    // heap number, we return the result without updating.
    __ Pop(r6_p, r5_p, cache_entry);
    __ LoadRoot(r8_p, Heap::kHeapNumberMapRootIndex);
    __ AllocateHeapNumber(r9_p, scratch0, scratch1, r8_p, &no_update);
    __ StoreF(d2, FieldMemOperand(r9_p, HeapNumber::kValueOffset));
    __ StoreW(r5_p, MemOperand(cache_entry, 0));
    __ StoreW(r6_p, MemOperand(cache_entry, 4));
    __ StoreP(r9_p, MemOperand(cache_entry, 8));
    __ Ret();

    __ bind(&invalid_cache);
    // The cache is invalid. Call runtime which will recreate the
    // cache.
    __ LoadRoot(r8_p, Heap::kHeapNumberMapRootIndex);
    __ AllocateHeapNumber(r3_p, scratch0, scratch1, r8_p, &skip_cache);
    __ StoreF(d2, FieldMemOperand(r3_p, HeapNumber::kValueOffset));
    {
      FrameScope scope(masm, StackFrame::INTERNAL);
      __ push(r3_p);
      __ CallRuntime(RuntimeFunction(), 1);
    }
    __ LoadF(d2, FieldMemOperand(r3_p, HeapNumber::kValueOffset));
    __ Ret();

    __ bind(&skip_cache);
    // Call C function to calculate the result and answer directly
    // without updating the cache.
    GenerateCallCFunction(masm, scratch0);
    __ GetCFunctionDoubleResult(d2);
    __ bind(&no_update);

    // We return the value in d2 without adding it to the cache, but
    // we cause a scavenging GC so that future allocations will succeed.
    {
      FrameScope scope(masm, StackFrame::INTERNAL);

      // Allocate an aligned object larger than a HeapNumber.
      ASSERT(2 * kDoubleSize >= HeapNumber::kSize);
      __ LoadSmiLiteral(scratch0, Smi::FromInt(2 * kDoubleSize));
      __ push(scratch0);
      __ CallRuntimeSaveDoubles(Runtime::kAllocateInNewSpace);
    }
    __ Ret();
  }
}


void TranscendentalCacheStub::GenerateCallCFunction(MacroAssembler* masm,
                                                    Register scratch) {
  Isolate* isolate = masm->isolate();

  __ mflr(r0_p);
  __ push(r0_p);
  __ PrepareCallCFunction(0, 1, scratch);
  __ ldr(d1, d2);
  AllowExternalCallThatCantCauseGC scope(masm);
  switch (type_) {
    case TranscendentalCache::SIN:
      __ CallCFunction(ExternalReference::math_sin_double_function(isolate),
          0, 1);
      break;
    case TranscendentalCache::COS:
      __ CallCFunction(ExternalReference::math_cos_double_function(isolate),
          0, 1);
      break;
    case TranscendentalCache::TAN:
      __ CallCFunction(ExternalReference::math_tan_double_function(isolate),
          0, 1);
      break;
    case TranscendentalCache::LOG:
      __ CallCFunction(ExternalReference::math_log_double_function(isolate),
          0, 1);
      break;
    default:
      UNIMPLEMENTED();
      break;
  }
  __ pop(r0_p);
  __ mtlr(r0_p);
}


Runtime::FunctionId TranscendentalCacheStub::RuntimeFunction() {
  switch (type_) {
    // Add more cases when necessary.
    case TranscendentalCache::SIN: return Runtime::kMath_sin;
    case TranscendentalCache::COS: return Runtime::kMath_cos;
    case TranscendentalCache::TAN: return Runtime::kMath_tan;
    case TranscendentalCache::LOG: return Runtime::kMath_log;
    default:
      UNIMPLEMENTED();
      return Runtime::kAbort;
  }
}


void StackCheckStub::Generate(MacroAssembler* masm) {
  __ TailCallRuntime(Runtime::kStackGuard, 0, 1);
}


void InterruptStub::Generate(MacroAssembler* masm) {
  __ TailCallRuntime(Runtime::kInterrupt, 0, 1);
}


void MathPowStub::Generate(MacroAssembler* masm) {
  const Register base = r4_p;
  const Register exponent = r5_p;
  const Register heapnumbermap = r8_p;
  const Register heapnumber = r3_p;
  const DoubleRegister double_base = d1;
  const DoubleRegister double_exponent = d2;
  const DoubleRegister double_result = d3;
  const DoubleRegister double_scratch = d0;
  const Register scratch = r22_p;
  const Register scratch2 = r10_p;

  Label call_runtime, done, int_exponent;
  if (exponent_type_ == ON_STACK) {
    Label base_is_smi, unpack_exponent;
    // The exponent and base are supplied as arguments on the stack.
    // This can only happen if the stub is called from non-optimized code.
    // Load input parameters from stack to double registers.
    __ LoadP(base, MemOperand(sp, 1 * kPointerSize));
    __ LoadP(exponent, MemOperand(sp, 0 * kPointerSize));

    __ LoadRoot(heapnumbermap, Heap::kHeapNumberMapRootIndex);

    __ UntagAndJumpIfSmi(scratch, base, &base_is_smi);
    __ LoadP(scratch, FieldMemOperand(base, JSObject::kMapOffset));
    __ CmpRR(scratch, heapnumbermap);
    __ bne(&call_runtime);

    __ LoadF(double_base, FieldMemOperand(base, HeapNumber::kValueOffset));
    __ b(&unpack_exponent);

    __ bind(&base_is_smi);
    FloatingPointHelper::ConvertIntToDouble(masm, scratch, double_base);
    __ bind(&unpack_exponent);

    __ UntagAndJumpIfSmi(scratch, exponent, &int_exponent);
    __ LoadP(scratch, FieldMemOperand(exponent, JSObject::kMapOffset));
    __ CmpRR(scratch, heapnumbermap);
    __ bne(&call_runtime);

    __ LoadF(double_exponent,
           FieldMemOperand(exponent, HeapNumber::kValueOffset));
  } else if (exponent_type_ == TAGGED) {
    // Base is already in double_base.
    __ UntagAndJumpIfSmi(scratch, exponent, &int_exponent);

    __ LoadF(double_exponent,
           FieldMemOperand(exponent, HeapNumber::kValueOffset));
  }

  if (exponent_type_ != INTEGER) {
    // Detect integer exponents stored as double.
    __ EmitVFPTruncate(kRoundToZero,
                       scratch,
                       double_exponent,
                       scratch2,
                       double_scratch,
                       kCheckForInexactConversion);
    __ b(Condition(CC_EQ | CC_LT | CC_GT), &int_exponent);

    if (exponent_type_ == ON_STACK) {
      // Detect square root case.  Crankshaft detects constant +/-0.5 at
      // compile time and uses DoMathPowHalf instead.  We then skip this check
      // for non-constant cases of +/-0.5 as these hardly occur.
      Label not_plus_half, not_minus_inf1, not_minus_inf2;

      // Test for 0.5.
      __ LoadDoubleLiteral(double_scratch, 0.5, scratch);
      __ fcmpu(double_exponent, double_scratch);
      __ bne(&not_plus_half);

      // Calculates square root of base.  Check for the special case of
      // Math.pow(-Infinity, 0.5) == Infinity (ECMA spec, 15.8.2.13).
      __ LoadDoubleLiteral(double_scratch, -V8_INFINITY, scratch);
      __ fcmpu(double_base, double_scratch);
      __ bne(&not_minus_inf1);
      __ lndbr(double_result, double_scratch);
      __ b(&done);
      __ bind(&not_minus_inf1);

      // Add +0 to convert -0 to +0.
      __ ldr(double_scratch, double_base);
      __ adbr(double_scratch, kDoubleRegZero);
      __ sqdbr(double_result, double_scratch);
      __ b(&done);

      __ bind(&not_plus_half);
      __ LoadDoubleLiteral(double_scratch, -0.5, scratch);
      __ fcmpu(double_exponent, double_scratch);
      __ bne(&call_runtime);

      // Calculates square root of base.  Check for the special case of
      // Math.pow(-Infinity, -0.5) == 0 (ECMA spec, 15.8.2.13).
      __ LoadDoubleLiteral(double_scratch, -V8_INFINITY, scratch);
      __ fcmpu(double_base, double_scratch);
      __ bne(&not_minus_inf2);
      __ ldr(double_result, kDoubleRegZero);
      __ b(&done);
      __ bind(&not_minus_inf2);

      // Add +0 to convert -0 to +0.
      __ ldr(double_scratch, double_base);
      __ adbr(double_scratch, kDoubleRegZero);
      __ LoadDoubleLiteral(double_result, 1.0, scratch);
      __ sqdbr(double_scratch, double_scratch);
      __ ddbr(double_result, double_scratch);
      __ b(&done);
    }

    __ mflr(r0_p);
    __ push(r0_p);
    {
      AllowExternalCallThatCantCauseGC scope(masm);
      __ PrepareCallCFunction(0, 2, scratch);
      __ SetCallCDoubleArguments(double_base, double_exponent);
      __ CallCFunction(
          ExternalReference::power_double_double_function(masm->isolate()),
          0, 2);
    }
    __ pop(r0_p);
    __ mtlr(r0_p);
    __ GetCFunctionDoubleResult(double_result);
    __ b(&done);
  }

  // Calculate power with integer exponent.
  __ bind(&int_exponent);

  // Get two copies of exponent in the registers scratch and exponent.
  if (exponent_type_ == INTEGER) {
    __ LoadRR(scratch, exponent);
  } else {
    // Exponent has previously been stored into scratch as untagged integer.
    __ LoadRR(exponent, scratch);
  }
  __ ldr(double_scratch, double_base);  // Back up base.
  __ LoadImmP(scratch2, Operand(1));
  FloatingPointHelper::ConvertIntToDouble(masm, scratch2, double_result);

  // Get absolute value of exponent.
  Label positive_exponent;
  __ Cmpi(scratch, Operand::Zero());
  __ bge(&positive_exponent);
  __ Negate(scratch, scratch);
  __ bind(&positive_exponent);

  Label while_true, no_carry, loop_end;
  __ bind(&while_true);
  __ LoadRR(scratch2, scratch);
  __ AndP(scratch2, Operand(1));
  __ beq(&no_carry /*, cr0*/);
  __ mdbr(double_result, double_scratch);
  __ bind(&no_carry);
  __ ShiftRightArithImm(scratch, scratch, 1, SetRC);
  __ beq(&loop_end /*, cr0*/);
  __ mdbr(double_scratch, double_scratch);
  __ b(&while_true);
  __ bind(&loop_end);

  __ Cmpi(exponent, Operand::Zero());
  __ bge(&done);

  // get 1/double_result:
  __ ldr(double_scratch, double_result);
  __ LoadImmP(scratch2, Operand(1));
  FloatingPointHelper::ConvertIntToDouble(masm, scratch2, double_result);
  __ ddbr(double_result, double_scratch);

  // Test whether result is zero.  Bail out to check for subnormal result.
  // Due to subnormals, x^-y == (1/x)^y does not hold in all cases.
  __ fcmpu(double_result, kDoubleRegZero);
  __ bne(&done);
  // double_exponent may not containe the exponent value if the input was a
  // smi.  We set it with exponent value before bailing out.
  FloatingPointHelper::ConvertIntToDouble(masm, exponent, double_exponent);

  // Returning or bailing out.
  Counters* counters = masm->isolate()->counters();
  if (exponent_type_ == ON_STACK) {
    // The arguments are still on the stack.
    __ bind(&call_runtime);
    __ TailCallRuntime(Runtime::kMath_pow_cfunction, 2, 1);

    // The stub is called from non-optimized code, which expects the result
    // as heap number in exponent.
    __ bind(&done);
    __ AllocateHeapNumber(
        heapnumber, scratch, scratch2, heapnumbermap, &call_runtime);
    __ StoreF(double_result,
            FieldMemOperand(heapnumber, HeapNumber::kValueOffset));
    ASSERT(heapnumber.is(r3_p));
    __ IncrementCounter(counters->math_pow(), 1, scratch, scratch2);
    __ Ret(2);
  } else {
    __ mflr(r0_p);
    __ push(r0_p);
    {
      AllowExternalCallThatCantCauseGC scope(masm);
      __ PrepareCallCFunction(0, 2, scratch);
      __ SetCallCDoubleArguments(double_base, double_exponent);
      __ CallCFunction(
          ExternalReference::power_double_double_function(masm->isolate()),
          0, 2);
    }
    __ pop(r0_p);
    __ mtlr(r0_p);
    __ GetCFunctionDoubleResult(double_result);

    __ bind(&done);
    __ IncrementCounter(counters->math_pow(), 1, scratch, scratch2);
    __ Ret();
  }
}


bool CEntryStub::NeedsImmovableCode() {
  return true;
}


bool CEntryStub::IsPregenerated() {
  return (!save_doubles_ || ISOLATE->fp_stubs_generated()) &&
          result_size_ == 1;
}


void CodeStub::GenerateStubsAheadOfTime() {
  CEntryStub::GenerateAheadOfTime();
  StoreBufferOverflowStub::GenerateFixedRegStubsAheadOfTime();
  RecordWriteStub::GenerateFixedRegStubsAheadOfTime();
}


void CodeStub::GenerateFPStubs() {
  CEntryStub save_doubles(1, kSaveFPRegs);
  Handle<Code> code = save_doubles.GetCode();
  code->set_is_pregenerated(true);
  StoreBufferOverflowStub stub(kSaveFPRegs);
  stub.GetCode()->set_is_pregenerated(true);
  code->GetIsolate()->set_fp_stubs_generated(true);
}


void CEntryStub::GenerateAheadOfTime() {
  CEntryStub stub(1, kDontSaveFPRegs);
  Handle<Code> code = stub.GetCode();
  code->set_is_pregenerated(true);
}


void CEntryStub::GenerateCore(MacroAssembler* masm,
                              Label* throw_normal_exception,
                              Label* throw_termination_exception,
                              Label* throw_out_of_memory_exception,
                              bool do_gc,
                              bool always_allocate) {
  // r2: result parameter for PerformGC, if any
  // r7: number of arguments (C callee-saved)
  // r8: pointer to builtin function (C callee-saved)
  // r9: pointer to first argument (C callee-saved)

  // r14: number of arguments including receiver  (C callee-saved)
  // r15: pointer to builtin function  (C callee-saved)
  // r16: pointer to the first argument (C callee-saved)
  Isolate* isolate = masm->isolate();
  Register isolate_reg = no_reg;

  if (do_gc) {
    // Passing r3.
    __ PrepareCallCFunction(1, 0, r4);
    __ CallCFunction(ExternalReference::perform_gc_function(isolate),
        1, 0);
  }

  ExternalReference scope_depth =
      ExternalReference::heap_always_allocate_scope_depth(isolate);
  if (always_allocate) {
    __ mov(r2, Operand(scope_depth));
    // @TODO Can exploit ASI here on z10 or newer.
    __ LoadlW(r3, MemOperand(r2));
    __ AddP(r3, Operand(1));
    __ StoreW(r3, MemOperand(r2));
  }

  // PPC LINUX ABI:
  // The #if below used to be !USE_SIMULATOR but needed
  // to change to support nativesim=true builds
#if defined(V8_HOST_ARCH_S39064) || defined(V8_HOST_ARCH_S390)
  // Call C built-in on native hardware.
#if defined(V8_TARGET_ARCH_S390X)

#if !ABI_RETURNS_OBJECT_PAIRS_IN_REGS
  if (result_size_ < 2) {
#if __BYTE_ORDER == __LITTLE_ENDIAN
    __ LoadRR(r2, r7);
#else
    // r3 = argc << 32 (for alignment), r4 = argv
    __ ShiftLeftImm(r2, r7, Operand(32));
#endif
    __ LoadRR(r3, r9);
    isolate_reg = r4;
  } else {
    ASSERT_EQ(2, result_size_);
    // The return value is 16-byte non-scalar value.
    // Use frame storage reserved by calling function to pass return
    // buffer as implicit first argument.
    __ la(r2, MemOperand(sp, (kStackFrameExtraParamSlot + 1) * kPointerSize));
#if __BYTE_ORDER == __LITTLE_ENDIAN
    __ LoadRR(r3, r7);
#else
    // r4 = argc << 32 (for alignment), r5 = argv
    __ ShiftLeftImm(r3, r7, Operand(32));
#endif
    __ LoadRR(r4, r9);
    isolate_reg = r5;
  }
#else
#if __BYTE_ORDER == __LITTLE_ENDIAN
  __ LoadRR(r2, r7);
#else
  // r3 = argc << 32 (for alignment), r4 = argv
  __ ShiftLeftImm(r2, r7, Operand(32));
#endif
  __ LoadRR(r3, r9);
  isolate_reg = r4;
#endif

#else
  // zLinux 31-bit
  // r2 = argc, r3 = argv
  __ LoadRR(r2, r7);
  __ LoadRR(r3, r9);
  isolate_reg = r4;
/* PPC code commented out.. left for reference in case we need to pass by
 * reference
#else  // 32-bit linux
  // Use frame storage reserved by calling function
  // PPC passes C++ objects by reference not value
  // This builds an object in the stack frame
  __ la(r2, MemOperand(sp, (kStackFrameExtraParamSlot + 1) * kPointerSize));
  __ StoreP(r7, MemOperand(r2));
  __ StoreP(r9, MemOperand(r2, kPointerSize));
  isolate_reg = r3;
*/
#endif
#else  // Simulated
  // Call C built-in using simulator.
  // r3 = argc, r4 = argv
  // @TODO Make sure this is correct for S390
#if defined(V8_TARGET_ARCH_S390X) && __BYTE_ORDER == __BIG_ENDIAN
  __ ShiftLeftImm(r2, r7, Operand(32));
#else
  __ LoadRR(r2, r7);
#endif
  __ LoadRR(r3, r9);
  isolate_reg = r4;
#endif

  __ mov(isolate_reg, Operand(ExternalReference::isolate_address()));

#if ABI_USES_FUNCTION_DESCRIPTORS && !defined(USE_SIMULATOR)
  // Native AIX/PPC64 Linux use a function descriptor.
  // @TODO Haven't touched this code for S390.. See if it's applicable.
  // especially the ToRegister(2) part.
  __ LoadP(ToRegister(2), MemOperand(r8, kPointerSize));  // TOC
  __ LoadP(ip, MemOperand(r8, 0));  // Instruction address
  Register target = ip;
#else
  Register target = r8;
#endif

  // To let the GC traverse the return address of the exit frames, we need to
  // know where the return address is. The CEntryStub is unmovable, so
  // we can store the address on the stack to be able to find it again and
  // we never have to restore it, because it will not change.
  // Compute the return address in lr to return to after the jump below. Pc is
  // already at '+ 8' from the current instruction but return is after three
  // instructions so add another 4 to pc to get the return address.
  { Assembler::BlockTrampolinePoolScope block_trampoline_pool(masm);
    Label return_label;
    __ larl(r0, &return_label);  // Generate the return addr of call later.
    __ StoreP(r0, MemOperand(sp, kStackFrameExtraParamSlot * kPointerSize));

    // zLinux ABI requires caller's frame to have sufficient space for callee
    // preserved regsiter save area.
    // @TODO Make sure this is in the right place and we need to guard it
    // with appropriate #ifdefs
    __ lay(sp, MemOperand(sp, -kCalleeRegisterSaveAreaSize));
    __ Call(target);
    __ bind(&return_label);
    __ lay(sp, MemOperand(sp, +kCalleeRegisterSaveAreaSize));
  }

  if (always_allocate) {
    // It's okay to clobber r4 and r5 here. Don't mess with r2 and r3
    // though (contain the result).
    __ mov(r4, Operand(scope_depth));
    // @TODO Can exploit ASI here on z10 or newer.
    __ LoadlW(r5, MemOperand(r4));
    __ Sub(r5, Operand(1));
    __ StoreW(r5, MemOperand(r4));
  }

  // check for failure result
  Label failure_returned;
  STATIC_ASSERT(((kFailureTag + 1) & kFailureTagMask) == 0);
#if defined(V8_TARGET_ARCH_PPC64) && !ABI_RETURNS_OBJECT_PAIRS_IN_REGS
  // If return value is on the stack, pop it to registers.
  if (result_size_ > 1) {
    ASSERT_EQ(2, result_size_);
    __ LoadP(r3, MemOperand(r2, kPointerSize));
    __ LoadP(r2, MemOperand(r2));
  }
#endif
  // Lower 2 bits of r5 are 0 iff r3 has failure tag.
  __ LoadRR(r4, r2);
  __ Add(r4, Operand(1));
  STATIC_ASSERT(kFailureTagMask < 0x8000);
  __ nill(r4, Operand(kFailureTagMask));
  __ beq(&failure_returned);  // Branch if and result is zero.

  // Exit C frame and return.
  // r2:r3: result
  // sp: stack pointer
  // fp: frame pointer
  //  Callee-saved register r7 still holds argc.
  __ LeaveExitFrame(save_doubles_, r7);
  __ b(r14);

  // check if we should retry or throw exception
  Label retry;
  __ bind(&failure_returned);
  STATIC_ASSERT(Failure::RETRY_AFTER_GC == 0);
  __ LoadRR(r0_p, r3_p);
  __ AndP(r0_p,
          Operand(((1 << kFailureTypeTagSize) - 1) << kFailureTagSize));
  __ beq(&retry /*, cr0*/);

  // Special handling of out of memory exceptions.
  Failure* out_of_memory = Failure::OutOfMemoryException();
  __ Cmpi(r3_p, Operand(reinterpret_cast<intptr_t>(out_of_memory)));
  __ beq(throw_out_of_memory_exception);

  // Retrieve the pending exception and clear the variable.
  __ mov(r6_p, Operand(isolate->factory()->the_hole_value()));
  __ mov(ip, Operand(ExternalReference(Isolate::kPendingExceptionAddress,
                                       isolate)));
  __ LoadP(r3_p, MemOperand(ip));
  __ StoreP(r6_p, MemOperand(ip));

  // Special handling of termination exceptions which are uncatchable
  // by javascript code.
  __ mov(r6_p, Operand(isolate->factory()->termination_exception()));
  __ CmpRR(r3_p, r6_p);
  __ beq(throw_termination_exception);

  // Handle normal exception.
  __ b(throw_normal_exception);

  __ bind(&retry);  // pass last failure (r3) as parameter (r3) when retrying
}


void CEntryStub::Generate(MacroAssembler* masm) {
  // Called from JavaScript; parameters are on stack as if calling JS function
  // r2: number of arguments including receiver
  // r3: pointer to builtin function
  // fp: frame pointer  (restored after C call)
  // sp: stack pointer  (restored as callee's sp after C call)
  // cp: current context  (C callee-saved)

  // Result returned in r3 or r3+r4 by default.

  // NOTE: Invocations of builtins may return failure objects
  // instead of a proper result. The builtin entry handles
  // this by performing a garbage collection and retrying the
  // builtin once.

  // Compute the argv pointer in a callee-saved register.
  __ ShiftLeftImm(r9, r2, Operand(kPointerSizeLog2));
  __ lay(r9, MemOperand(r9, sp, -kPointerSize));

  // Enter the exit frame that transitions from JavaScript to C++.
  FrameScope scope(masm, StackFrame::MANUAL);

  // Need at least one extra slot for return address location.
  int arg_stack_space = 1;

  // PPC LINUX ABI:
  // The #if immediately below was !USE_SIMULATOR, but needed
  // to change to support nativesim=true builds
#if defined(V8_HOST_ARCH_S39064) || defined(V8_HOST_ARCH_S390)
#if defined(V8_TARGET_ARCH_S390X) && !ABI_RETURNS_OBJECT_PAIRS_IN_REGS
  // Pass buffer for return value on stack if necessary
  if (result_size_ > 1) {
    ASSERT_EQ(2, result_size_);
    arg_stack_space += 2;
  }
#elif !defined(_AIX)
  // 32-bit linux
  // Pass C++ objects by reference not value
  arg_stack_space += 2;
#endif
#endif

  __ EnterExitFrame(save_doubles_, arg_stack_space);

  // Set up argc and the builtin function in callee-saved registers.
  __ LoadRR(r7, r2);  // argc
  __ LoadRR(r8, r3);  // builtin function

  // r7: number of arguments (C callee-saved)
  // r8: pointer to builtin function (C callee-saved)
  // r9: pointer to first argument (C callee-saved)

  // @TODO Figure out the rest of this exception stuff for S390 registers

  Label throw_normal_exception;
  Label throw_termination_exception;
  Label throw_out_of_memory_exception;

  // Call into the runtime system.
  GenerateCore(masm,
               &throw_normal_exception,
               &throw_termination_exception,
               &throw_out_of_memory_exception,
               false,
               false);

  // Do space-specific GC and retry runtime call.
  GenerateCore(masm,
               &throw_normal_exception,
               &throw_termination_exception,
               &throw_out_of_memory_exception,
               true,
               false);

  // Do full GC and retry runtime call one final time.
  Failure* failure = Failure::InternalError();
  __ mov(r3, Operand(reinterpret_cast<intptr_t>(failure)));
  GenerateCore(masm,
               &throw_normal_exception,
               &throw_termination_exception,
               &throw_out_of_memory_exception,
               true,
               true);

  __ bind(&throw_out_of_memory_exception);
  // Set external caught exception to false.
  Isolate* isolate = masm->isolate();
  ExternalReference external_caught(Isolate::kExternalCaughtExceptionAddress,
                                    isolate);
  __ LoadImmP(r3_p, Operand(false, RelocInfo::NONE));
  __ mov(r5_p, Operand(external_caught));
  __ StoreP(r3_p, MemOperand(r5_p));

  // Set pending exception and r0_p to out of memory exception.
  Failure* out_of_memory = Failure::OutOfMemoryException();
  __ mov(r3_p, Operand(reinterpret_cast<intptr_t>(out_of_memory)));
  __ mov(r5_p, Operand(ExternalReference(Isolate::kPendingExceptionAddress,
                                       isolate)));
  __ StoreP(r3_p, MemOperand(r5_p));
  // Fall through to the next label.

  __ bind(&throw_termination_exception);
  __ ThrowUncatchable(r3_p);

  __ bind(&throw_normal_exception);
  __ Throw(r3_p);
}


void JSEntryStub::GenerateBody(MacroAssembler* masm, bool is_construct) {
  // r2: code entry
  // r3: function
  // r4: receiver
  // r5: argc
  // r6: argv

  Label invoke, handler_entry, exit;

  // Called from C
#if ABI_USES_FUNCTION_DESCRIPTORS
  __ function_descriptor();
#endif
  // zLinux ABI
  //    Incoming parameters:
  //          r2: code entry
  //          r3: function
  //          r4: receiver
  //          r5: argc
  //          r6: argv
  //    Requires us to save the callee-preserved registers r6-r13
  //    General convention is to also save r14 (return addr) and
  //    sp/r15 as well in a single STM/STMG
  __ lay(sp, MemOperand(sp, -10 * kPointerSize));
  __ StoreMultipleP(r6, sp, MemOperand(sp, 0));

  // Floating point regs FPR0 - FRP13 are volatile
  // FPR14-FPR31 are non-volatile, but sub-calls will save them for us
  // @TODO Figure out if we need to preserve any S390 FP regs

//  int offset_to_argv = kPointerSize * 22; // matches (22*4) above
//  __ LoadlW(r7, MemOperand(sp, offset_to_argv));

  // Push a frame with special values setup to mark it as an entry frame.
  //   Bad FP (-1)
  //   SMI Marker
  //   SMI Marker
  //   kCEntryFPAddress
  //   Frame type
  Isolate* isolate = masm->isolate();
  __ lay(sp, MemOperand(sp, -5 * kPointerSize));
  // Push a bad frame pointer to fail if it is used.
  __ LoadImmP(r10, Operand(-1));
  int marker = is_construct ? StackFrame::ENTRY_CONSTRUCT : StackFrame::ENTRY;
  __ LoadSmiLiteral(r9, Smi::FromInt(marker));
  __ LoadSmiLiteral(r8, Smi::FromInt(marker));
  // Save copies of the top frame descriptor on the stack.
  __ mov(r7, Operand(ExternalReference(Isolate::kCEntryFPAddress, isolate)));
  __ LoadP(r7, MemOperand(r7));
  __ StoreMultipleP(r7, r10, MemOperand(sp, kPointerSize));

  // Set up frame pointer for the frame to be pushed.
  // Need to add kPointerSize, because sp has one extra
  // frame already for the frame type being pushed later.
  __ lay(fp, MemOperand(sp, -EntryFrameConstants::kCallerFPOffset +
                            kPointerSize));


  // If this is the outermost JS call, set js_entry_sp value.
  Label non_outermost_js;
  ExternalReference js_entry_sp(Isolate::kJSEntrySPAddress, isolate);
  __ mov(r8, Operand(ExternalReference(js_entry_sp)));
  __ LoadAndTestP(r9, MemOperand(r8));
  __ bne(&non_outermost_js);
  __ StoreP(fp, MemOperand(r8));
  __ LoadSmiLiteral(ip, Smi::FromInt(StackFrame::OUTERMOST_JSENTRY_FRAME));
  Label cont;
  __ b(&cont);
  __ bind(&non_outermost_js);
  __ LoadSmiLiteral(ip, Smi::FromInt(StackFrame::INNER_JSENTRY_FRAME));

  __ bind(&cont);
  __ StoreP(ip, MemOperand(sp));  // frame-type

  // Jump to a faked try block that does the invoke, with a faked catch
  // block that sets the pending exception.
  __ b(&invoke);

  __ bind(&handler_entry);
  handler_offset_ = handler_entry.pos();
  // Caught exception: Store result (exception) in the pending exception
  // field in the JSEnv and return a failure sentinel.  Coming in here the
  // fp will be invalid because the PushTryHandler below sets it to 0 to
  // signal the existence of the JSEntry frame.
  __ mov(ip, Operand(ExternalReference(Isolate::kPendingExceptionAddress,
                                       isolate)));

  // @TODO Verify that on S390 r2 is the correct register here. (PPC used r3)
  __ StoreP(r2, MemOperand(ip));
  __ mov(r2, Operand(reinterpret_cast<intptr_t>(Failure::Exception())));
  __ b(&exit);

  // Invoke: Link this frame into the handler chain.  There's only one
  // handler block in this code object, so its index is 0.
  __ bind(&invoke);
  // Must preserve r0-r4, r5-r7 are available. (needs update for PPC)
  __ PushTryHandler(StackHandler::JS_ENTRY, 0);
  // If an exception not caught by another handler occurs, this handler
  // returns control to the code after the b(&invoke) above, which
  // restores all kCalleeSaved registers (including cp and fp) to their
  // saved values before returning a failure to C.

  // Clear any pending exceptions.
  __ mov(ip, Operand(ExternalReference(Isolate::kPendingExceptionAddress,
                                       isolate)));
  __ mov(r8, Operand(isolate->factory()->the_hole_value()));
  __ StoreP(r8, MemOperand(ip));

  // Invoke the function by calling through JS entry trampoline builtin.
  // Notice that we cannot store a reference to the trampoline code directly in
  // this stub, because runtime stubs are not traversed when doing GC.

  // Expected registers by Builtins::JSEntryTrampoline
  // r2: code entry
  // r3: function
  // r4: receiver
  // r5: argc
  // r6: argv
  if (is_construct) {
    ExternalReference construct_entry(Builtins::kJSConstructEntryTrampoline,
                                      isolate);
    __ mov(ip, Operand(construct_entry));
  } else {
    ExternalReference entry(Builtins::kJSEntryTrampoline, isolate);
    __ mov(ip, Operand(entry));
  }
  __ LoadP(ip, MemOperand(ip));  // deref address

  // Branch and link to JSEntryTrampoline.
  // the address points to the start of the code object, skip the header
  __ AddP(ip, Operand(Code::kHeaderSize - kHeapObjectTag));
  __ basr(r14, ip);

  // Unlink this frame from the handler chain.
  __ PopTryHandler();

  __ bind(&exit);  // r2 holds result
  // Check if the current stack frame is marked as the outermost JS frame.
  Label non_outermost_js_2;
  __ pop(r8);
  __ CmpSmiLiteral(r8, Smi::FromInt(StackFrame::OUTERMOST_JSENTRY_FRAME), r0);
  __ bne(&non_outermost_js_2);
  __ mov(r9, Operand::Zero());
  __ mov(r8, Operand(ExternalReference(js_entry_sp)));
  __ StoreP(r9, MemOperand(r8));
  __ bind(&non_outermost_js_2);

  // Restore the top frame descriptors from the stack.
  __ pop(r6);
  __ mov(ip,
         Operand(ExternalReference(Isolate::kCEntryFPAddress, isolate)));
  __ StoreP(r6, MemOperand(ip));

  // Reset the stack to the callee saved registers.
  __ AddP(sp, Operand(-EntryFrameConstants::kCallerFPOffset));

  // Restore callee-saved registers and return.
#ifdef DEBUG
  if (FLAG_debug_code) {
    Label here;
    __ b(&here /*, SetLK*/);
    __ bind(&here);
  }
#endif

  // Reload callee-saved preserved regs, return address reg (r14) and sp
  __ LoadMultipleP(r6, sp, MemOperand(sp, 0));
  __ lay(sp, MemOperand(sp, 10 * kPointerSize));
  __ b(r14);
}


// Uses registers r3_p to r7_p.
// Expected input (depending on whether args are in registers or on the stack):
// * object: r3_p or at sp + 1 * kPointerSize.
// * function: r4_p or at sp.
//
// An inlined call site may have been generated before calling this stub.
// In this case the offset to the inline site to patch is passed on the stack,
// in the safepoint slot for register r7_p.
// (See LCodeGen::DoInstanceOfKnownGlobal)
void InstanceofStub::Generate(MacroAssembler* masm) {
  // Call site inlining and patching implies arguments in registers.
  ASSERT(HasArgsInRegisters() || !HasCallSiteInlineCheck());
  // ReturnTrueFalse is only implemented for inlined call sites.
  ASSERT(!ReturnTrueFalseObject() || HasCallSiteInlineCheck());

  // Fixed register usage throughout the stub:
  const Register object = r3_p;  // Object (lhs).
  Register map = r6_p;  // Map of the object.
  const Register function = r4_p;  // Function (rhs).
  const Register prototype = r7_p;  // Prototype of the function.
  const Register inline_site = r9_p;
  const Register scratch = r5_p;
  const Register scratch2 = r8_p;
  Register scratch3 = no_reg;

#if V8_TARGET_ARCH_S390X
  const int32_t kDeltaToLoadBoolResult = 9 * Assembler::kInstrSize;
#else
  const int32_t kDeltaToLoadBoolResult = 5 * Assembler::kInstrSize;
#endif

  Label slow, loop, is_instance, is_not_instance, not_js_object;

  if (!HasArgsInRegisters()) {
    __ LoadP(object, MemOperand(sp, 1 * kPointerSize));
    __ LoadP(function, MemOperand(sp, 0));
  }

  // Check that the left hand is a JS object and load map.
  __ JumpIfSmi(object, &not_js_object);
  __ IsObjectJSObjectType(object, map, scratch, &not_js_object);

  // If there is a call site cache don't look in the global cache, but do the
  // real lookup and update the call site cache.
  if (!HasCallSiteInlineCheck()) {
    Label miss;
    __ CompareRoot(function, Heap::kInstanceofCacheFunctionRootIndex);
    __ bne(&miss);
    __ CompareRoot(map, Heap::kInstanceofCacheMapRootIndex);
    __ bne(&miss);
    __ LoadRoot(r3_p, Heap::kInstanceofCacheAnswerRootIndex);
    __ Ret(HasArgsInRegisters() ? 0 : 2);

    __ bind(&miss);
  }

  // Get the prototype of the function.
  __ TryGetFunctionPrototype(function, prototype, scratch, &slow, true);

  // Check that the function prototype is a JS object.
  __ JumpIfSmi(prototype, &slow);
  __ IsObjectJSObjectType(prototype, scratch, scratch, &slow);

  // Update the global instanceof or call site inlined cache with the current
  // map and function. The cached answer will be set when it is known below.
  if (!HasCallSiteInlineCheck()) {
    __ StoreRoot(function, Heap::kInstanceofCacheFunctionRootIndex);
    __ StoreRoot(map, Heap::kInstanceofCacheMapRootIndex);
  } else {
    ASSERT(HasArgsInRegisters());
    // Patch the (relocated) inlined map check.

    // The offset was stored in r7_p safepoint slot.
    // (See LCodeGen::DoDeferredLInstanceOfKnownGlobal)
    __ LoadFromSafepointRegisterSlot(scratch, r7_p);
    __ mflr(inline_site);
    __ Sub(inline_site, inline_site, scratch);
    // Get the map location in scratch and patch it.
    __ GetRelocatedValueLocation(inline_site, scratch, scratch2);
    __ StoreP(map,
         FieldMemOperand(scratch, JSGlobalPropertyCell::kValueOffset));
  }

  // Register mapping: r6_p is object map and r7_p is function prototype.
  // Get prototype of object into r5_p.
  __ LoadP(scratch, FieldMemOperand(map, Map::kPrototypeOffset));

  // We don't need map any more. Use it as a scratch register.
  scratch3 = map;
  map = no_reg;

  // Loop through the prototype chain looking for the function prototype.
  __ LoadRoot(scratch3, Heap::kNullValueRootIndex);
  __ bind(&loop);
  __ CmpRR(scratch, prototype);
  __ beq(&is_instance);
  __ CmpRR(scratch, scratch3);
  __ beq(&is_not_instance);
  __ LoadP(scratch, FieldMemOperand(scratch, HeapObject::kMapOffset));
  __ LoadP(scratch, FieldMemOperand(scratch, Map::kPrototypeOffset));
  __ b(&loop);

  __ bind(&is_instance);
  if (!HasCallSiteInlineCheck()) {
    __ LoadSmiLiteral(r3_p, Smi::FromInt(0));
    __ StoreRoot(r3_p, Heap::kInstanceofCacheAnswerRootIndex);
  } else {
    // Patch the call site to return true.
    __ LoadRoot(r3_p, Heap::kTrueValueRootIndex);
    __ AddP(inline_site, Operand(kDeltaToLoadBoolResult));
    // Get the boolean result location in scratch and patch it.
    __ PatchRelocatedValue(inline_site, scratch, r3_p);

    if (!ReturnTrueFalseObject()) {
      __ LoadSmiLiteral(r3_p, Smi::FromInt(0));
    }
  }
  __ Ret(HasArgsInRegisters() ? 0 : 2);

  __ bind(&is_not_instance);
  if (!HasCallSiteInlineCheck()) {
    __ LoadSmiLiteral(r3_p, Smi::FromInt(1));
    __ StoreRoot(r3_p, Heap::kInstanceofCacheAnswerRootIndex);
  } else {
    // Patch the call site to return false.
    __ LoadRoot(r3_p, Heap::kFalseValueRootIndex);
    __ AddP(inline_site, Operand(kDeltaToLoadBoolResult));
    // Get the boolean result location in scratch and patch it.
    __ PatchRelocatedValue(inline_site, scratch, r3_p);

    if (!ReturnTrueFalseObject()) {
      __ LoadSmiLiteral(r3_p, Smi::FromInt(1));
    }
  }
  __ Ret(HasArgsInRegisters() ? 0 : 2);

  Label object_not_null, object_not_null_or_smi;
  __ bind(&not_js_object);
  // Before null, smi and string value checks, check that the rhs is a function
  // as for a non-function rhs an exception needs to be thrown.
  __ JumpIfSmi(function, &slow);
  __ CompareObjectType(function, scratch3, scratch, JS_FUNCTION_TYPE);
  __ bne(&slow);

  // Null is not instance of anything.
  __ Cmpi(scratch, Operand(masm->isolate()->factory()->null_value()));
  __ bne(&object_not_null);
  __ LoadSmiLiteral(r3_p, Smi::FromInt(1));
  __ Ret(HasArgsInRegisters() ? 0 : 2);

  __ bind(&object_not_null);
  // Smi values are not instances of anything.
  __ JumpIfNotSmi(object, &object_not_null_or_smi);
  __ LoadSmiLiteral(r3_p, Smi::FromInt(1));
  __ Ret(HasArgsInRegisters() ? 0 : 2);

  __ bind(&object_not_null_or_smi);
  // String values are not instances of anything.
  __ IsObjectJSStringType(object, scratch, &slow);
  __ LoadSmiLiteral(r3_p, Smi::FromInt(1));
  __ Ret(HasArgsInRegisters() ? 0 : 2);

  // Slow-case.  Tail call builtin.
  __ bind(&slow);
  if (!ReturnTrueFalseObject()) {
    if (HasArgsInRegisters()) {
      __ Push(r3_p, r4_p);
    }
  __ InvokeBuiltin(Builtins::INSTANCE_OF, JUMP_FUNCTION);
  } else {
    {
      FrameScope scope(masm, StackFrame::INTERNAL);
      __ Push(r3_p, r4_p);
      __ InvokeBuiltin(Builtins::INSTANCE_OF, CALL_FUNCTION);
    }
    Label true_value, done;
    __ Cmpi(r3_p, Operand::Zero());
    __ beq(&true_value);

    __ LoadRoot(r3_p, Heap::kFalseValueRootIndex);
    __ b(&done);

    __ bind(&true_value);
    __ LoadRoot(r3_p, Heap::kTrueValueRootIndex);

    __ bind(&done);
    __ Ret(HasArgsInRegisters() ? 0 : 2);
  }
}


Register InstanceofStub::left() { return r3_p; }


Register InstanceofStub::right() { return r4_p; }


void ArgumentsAccessStub::GenerateReadElement(MacroAssembler* masm) {
  // The displacement is the offset of the last parameter (if any)
  // relative to the frame pointer.
  const int kDisplacement =
      StandardFrameConstants::kCallerSPOffset - kPointerSize;

  // Check that the key is a smi.
  Label slow;
  __ JumpIfNotSmi(r4_p, &slow);

  // Check if the calling frame is an arguments adaptor frame.
  Label adaptor;
  __ LoadP(r5_p, MemOperand(fp, StandardFrameConstants::kCallerFPOffset));
  __ LoadP(r6_p, MemOperand(r5_p, StandardFrameConstants::kContextOffset));
  STATIC_ASSERT(StackFrame::ARGUMENTS_ADAPTOR < 0x3fffu);
  __ CmpSmiLiteral(r6_p, Smi::FromInt(StackFrame::ARGUMENTS_ADAPTOR), r0_p);
  __ beq(&adaptor);

  // Check index against formal parameters count limit passed in
  // through register r3_p. Use unsigned comparison to get negative
  // check for free.
  __ Cmpl(r4_p, r3_p);
  __ bge(&slow);

  // Read the argument from the stack and return it.
  __ Sub(r6_p, r3_p, r4_p);
  __ SmiToPtrArrayOffset(r6_p, r6_p);
  __ AddP(r6_p, fp);
  __ LoadP(r3_p, MemOperand(r6_p, kDisplacement));
  __ blr();

  // Arguments adaptor case: Check index against actual arguments
  // limit found in the arguments adaptor frame. Use unsigned
  // comparison to get negative check for free.
  __ bind(&adaptor);
  __ LoadP(r3_p,
           MemOperand(r5_p, ArgumentsAdaptorFrameConstants::kLengthOffset));
  __ Cmpl(r4_p, r3_p);
  __ bge(&slow);

  // Read the argument from the adaptor frame and return it.
  __ Sub(r6_p, r3_p, r4_p);
  __ SmiToPtrArrayOffset(r6_p, r6_p);
  __ AddP(r6_p, r5_p);
  __ LoadP(r3_p, MemOperand(r6_p, kDisplacement));
  __ blr();

  // Slow-case: Handle non-smi or out-of-bounds access to arguments
  // by calling the runtime system.
  __ bind(&slow);
  __ push(r4_p);
  __ TailCallRuntime(Runtime::kGetArgumentsProperty, 1, 1);
}


void ArgumentsAccessStub::GenerateNewNonStrictSlow(MacroAssembler* masm) {
  // sp[0] : number of parameters
  // sp[1] : receiver displacement
  // sp[2] : function

  // Check if the calling frame is an arguments adaptor frame.
  Label runtime;
  __ LoadP(r6_p, MemOperand(fp, StandardFrameConstants::kCallerFPOffset));
  __ LoadP(r5_p, MemOperand(r6_p, StandardFrameConstants::kContextOffset));
  STATIC_ASSERT(StackFrame::ARGUMENTS_ADAPTOR < 0x3fffu);
  __ CmpSmiLiteral(r5_p, Smi::FromInt(StackFrame::ARGUMENTS_ADAPTOR), r0_p);
  __ bne(&runtime);

  // Patch the arguments.length and the parameters pointer in the current frame.
  __ LoadP(r5_p,
           MemOperand(r6_p, ArgumentsAdaptorFrameConstants::kLengthOffset));
  __ StoreP(r5_p, MemOperand(sp, 0 * kPointerSize));
  __ SmiToPtrArrayOffset(r5_p, r5_p);
  __ AddP(r6_p, r5_p);
  __ AddP(r6_p, Operand(StandardFrameConstants::kCallerSPOffset));
  __ StoreP(r6_p, MemOperand(sp, 1 * kPointerSize));

  __ bind(&runtime);
  __ TailCallRuntime(Runtime::kNewArgumentsFast, 3, 1);
}


void ArgumentsAccessStub::GenerateNewNonStrictFast(MacroAssembler* masm) {
  // Stack layout:
  //  sp[0] : number of parameters (tagged)
  //  sp[1] : address of receiver argument
  //  sp[2] : function
  // Registers used over whole function:
  //  r9_p : allocated object (tagged)
  //  r22_p : mapped parameter count (tagged)

  __ LoadP(r4_p, MemOperand(sp, 0 * kPointerSize));
  // r4_p = parameter count (tagged)

  // Check if the calling frame is an arguments adaptor frame.
  Label runtime;
  Label adaptor_frame, try_allocate;
  __ LoadP(r6_p, MemOperand(fp, StandardFrameConstants::kCallerFPOffset));
  __ LoadP(r5_p, MemOperand(r6_p, StandardFrameConstants::kContextOffset));
  STATIC_ASSERT(StackFrame::ARGUMENTS_ADAPTOR < 0x3fffu);
  __ CmpSmiLiteral(r5_p, Smi::FromInt(StackFrame::ARGUMENTS_ADAPTOR), r0_p);
  __ beq(&adaptor_frame);

  // No adaptor, parameter count = argument count.
  __ LoadRR(r5_p, r4_p);
  __ b(&try_allocate);

  // We have an adaptor frame. Patch the parameters pointer.
  __ bind(&adaptor_frame);
  __ LoadP(r5_p,
           MemOperand(r6_p, ArgumentsAdaptorFrameConstants::kLengthOffset));
  __ SmiToPtrArrayOffset(r7_p, r5_p);
  __ AddP(r6_p, r7_p);
  __ AddP(r6_p, Operand(StandardFrameConstants::kCallerSPOffset));
  __ StoreP(r6_p, MemOperand(sp, 1 * kPointerSize));

  // r4_p = parameter count (tagged)
  // r5_p = argument count (tagged)
  // Compute the mapped parameter count = min(r4_p, r5_p) in r4_p.
  Label skip;
  __ CmpRR(r4_p, r5_p);
  __ blt(&skip);
  __ LoadRR(r4_p, r5_p);
  __ bind(&skip);

  __ bind(&try_allocate);

  // Compute the sizes of backing store, parameter map, and arguments object.
  // 1. Parameter map, has 2 extra words containing context and backing store.
  const int kParameterMapHeaderSize =
      FixedArray::kHeaderSize + 2 * kPointerSize;
  // If there are no mapped parameters, we do not need the parameter_map.
  Label skip2, skip3;
  __ CmpSmiLiteral(r4_p, Smi::FromInt(0), r0_p);
  __ bne(&skip2);
  __ LoadImmP(r22_p, Operand::Zero());
  __ b(&skip3);
  __ bind(&skip2);
  __ SmiToPtrArrayOffset(r22_p, r4_p);
  __ AddP(r22_p, Operand(kParameterMapHeaderSize));
  __ bind(&skip3);

  // 2. Backing store.
  __ SmiToPtrArrayOffset(r7_p, r5_p);
  __ AddP(r22_p, r7_p);
  __ AddP(r22_p, Operand(FixedArray::kHeaderSize));

  // 3. Arguments object.
  __ AddP(r22_p, Operand(Heap::kArgumentsObjectSize));

  // Do the allocation of all three objects in one go.
  __ AllocateInNewSpace(r22_p, r3_p, r6_p, r7_p, &runtime, TAG_OBJECT);

  // r3_p = address of new object(s) (tagged)
  // r5_p = argument count (tagged)
  // Get the arguments boilerplate from the current native context into r4_p.
  const int kNormalOffset =
      Context::SlotOffset(Context::ARGUMENTS_BOILERPLATE_INDEX);
  const int kAliasedOffset =
      Context::SlotOffset(Context::ALIASED_ARGUMENTS_BOILERPLATE_INDEX);

  __ LoadP(r7_p, MemOperand(r20_p,
             Context::SlotOffset(Context::GLOBAL_OBJECT_INDEX)));
  __ LoadP(r7_p, FieldMemOperand(r7_p, GlobalObject::kNativeContextOffset));
  Label skip4, skip5;
  __ Cmpi(r4_p, Operand::Zero());
  __ bne(&skip4);
  __ LoadP(r7_p, MemOperand(r7_p, kNormalOffset));
  __ b(&skip5);
  __ bind(&skip4);
  __ LoadP(r7_p, MemOperand(r7_p, kAliasedOffset));
  __ bind(&skip5);

  // r3_p = address of new object (tagged)
  // r4_p = mapped parameter count (tagged)
  // r5_p = argument count (tagged)
  // r7_p = address of boilerplate object (tagged)
  // Copy the JS object part.
  for (int i = 0; i < JSObject::kHeaderSize; i += kPointerSize) {
    __ LoadP(r6_p, FieldMemOperand(r7_p, i));
    __ StoreP(r6_p, FieldMemOperand(r3_p, i));
  }

  // Set up the callee in-object property.
  STATIC_ASSERT(Heap::kArgumentsCalleeIndex == 1);
  __ LoadP(r6_p, MemOperand(sp, 2 * kPointerSize));
  const int kCalleeOffset = JSObject::kHeaderSize +
      Heap::kArgumentsCalleeIndex * kPointerSize;
  __ StoreP(r6_p, FieldMemOperand(r3_p, kCalleeOffset));

  // Use the length (smi tagged) and set that as an in-object property too.
  STATIC_ASSERT(Heap::kArgumentsLengthIndex == 0);
  const int kLengthOffset = JSObject::kHeaderSize +
      Heap::kArgumentsLengthIndex * kPointerSize;
  __ StoreP(r5_p, FieldMemOperand(r3_p, kLengthOffset));

  // Set up the elements pointer in the allocated arguments object.
  // If we allocated a parameter map, r7_p will point there, otherwise
  // it will point to the backing store.
  __ LoadRR(r7_p, r3_p);
  __ AddP(r7_p, Operand(Heap::kArgumentsObjectSize));
  __ StoreP(r7_p, FieldMemOperand(r3_p, JSObject::kElementsOffset));

  // r3_p = address of new object (tagged)
  // r4_p = mapped parameter count (tagged)
  // r5_p = argument count (tagged)
  // r7_p = address of parameter map or backing store (tagged)
  // Initialize parameter map. If there are no mapped arguments, we're done.
  Label skip_parameter_map, skip6;
  __ CmpSmiLiteral(r4_p, Smi::FromInt(0), r0_p);
  __ bne(&skip6);
  // Move backing store address to r6_p, because it is
  // expected there when filling in the unmapped arguments.
  __ LoadRR(r6_p, r7_p);
  __ b(&skip_parameter_map);
  __ bind(&skip6);

  __ LoadRoot(r9_p, Heap::kNonStrictArgumentsElementsMapRootIndex);
  __ StoreP(r9_p, FieldMemOperand(r7_p, FixedArray::kMapOffset));
  __ AddSmiLiteral(r9_p, r4_p, Smi::FromInt(2), r0_p);
  __ StoreP(r9_p, FieldMemOperand(r7_p, FixedArray::kLengthOffset));
  __ StoreP(r20_p, FieldMemOperand(r7_p,
                                 FixedArray::kHeaderSize + 0 * kPointerSize));
  __ SmiToPtrArrayOffset(r9_p, r4_p);
  __ AddP(r9_p, r7_p);
  __ AddP(r9_p, Operand(kParameterMapHeaderSize));
  __ StoreP(r9_p, FieldMemOperand(r7_p,
                                FixedArray::kHeaderSize + 1 * kPointerSize));

  // Copy the parameter slots and the holes in the arguments.
  // We need to fill in mapped_parameter_count slots. They index the context,
  // where parameters are stored in reverse order, at
  //   MIN_CONTEXT_SLOTS .. MIN_CONTEXT_SLOTS+parameter_count-1
  // The mapped parameter thus need to get indices
  //   MIN_CONTEXT_SLOTS+parameter_count-1 ..
  //       MIN_CONTEXT_SLOTS+parameter_count-mapped_parameter_count
  // We loop from right to left.
  Label parameters_loop, parameters_test;
  __ LoadRR(r9_p, r4_p);
  __ LoadP(r22_p, MemOperand(sp, 0 * kPointerSize));
  __ AddSmiLiteral(r22_p, r22_p,
                   Smi::FromInt(Context::MIN_CONTEXT_SLOTS), r0_p);
  __ Sub(r22_p, r22_p, r4_p);
  __ LoadRoot(r10_p, Heap::kTheHoleValueRootIndex);
  __ SmiToPtrArrayOffset(r6_p, r9_p);
  __ AddP(r6_p, r7_p);
  __ AddP(r6_p, Operand(kParameterMapHeaderSize));

  // r9_p = loop variable (tagged)
  // r4_p = mapping index (tagged)
  // r6_p = address of backing store (tagged)
  // r7_p = address of parameter map (tagged)
  // r8_p = temporary scratch (a.o., for address calculation)
  // r10_p = the hole value
  __ b(&parameters_test);

  __ bind(&parameters_loop);
  __ SubSmiLiteral(r9_p, r9_p, Smi::FromInt(1), r0_p);
  __ SmiToPtrArrayOffset(r8_p, r9_p);
  __ AddP(r8_p, Operand(kParameterMapHeaderSize - kHeapObjectTag));
  __ StorePX(r22_p, MemOperand(r8_p, r7_p));
  __ Sub(r8_p, Operand(kParameterMapHeaderSize - FixedArray::kHeaderSize));
  __ StorePX(r10_p, MemOperand(r8_p, r6_p));
  __ AddSmiLiteral(r22_p, r22_p, Smi::FromInt(1), r0_p);
  __ bind(&parameters_test);
  __ CmpSmiLiteral(r9_p, Smi::FromInt(0), r0_p);
  __ bne(&parameters_loop);

  __ bind(&skip_parameter_map);
  // r5_p = argument count (tagged)
  // r6_p = address of backing store (tagged)
  // r8_p = scratch
  // Copy arguments header and remaining slots (if there are any).
  __ LoadRoot(r8_p, Heap::kFixedArrayMapRootIndex);
  __ StoreP(r8_p, FieldMemOperand(r6_p, FixedArray::kMapOffset));
  __ StoreP(r5_p, FieldMemOperand(r6_p, FixedArray::kLengthOffset));

  Label arguments_loop, arguments_test;
  __ LoadRR(r22_p, r4_p);
  __ LoadP(r7_p, MemOperand(sp, 1 * kPointerSize));
  __ SmiToPtrArrayOffset(r8_p, r22_p);
  __ Sub(r7_p, r7_p, r8_p);
  __ b(&arguments_test);

  __ bind(&arguments_loop);
  __ Sub(r7_p, Operand(kPointerSize));
  __ LoadP(r9_p, MemOperand(r7_p, 0));
  __ SmiToPtrArrayOffset(r8_p, r22_p);
  __ AddP(r8_p, r6_p);
  __ StoreP(r9_p, FieldMemOperand(r8_p, FixedArray::kHeaderSize));
  __ AddSmiLiteral(r22_p, r22_p, Smi::FromInt(1), r0_p);

  __ bind(&arguments_test);
  __ CmpRR(r22_p, r5_p);
  __ blt(&arguments_loop);

  // Return and remove the on-stack parameters.
  __ AddP(sp, Operand(3 * kPointerSize));
  __ Ret();

  // Do the runtime call to allocate the arguments object.
  // r5_p = argument count (tagged)
  __ bind(&runtime);
  __ StoreP(r5_p, MemOperand(sp, 0 * kPointerSize));  // Patch argument count.
  __ TailCallRuntime(Runtime::kNewArgumentsFast, 3, 1);
}

void ArgumentsAccessStub::GenerateNewStrict(MacroAssembler* masm) {
  // sp[0] : number of parameters
  // sp[4] : receiver displacement
  // sp[8] : function
  // Check if the calling frame is an arguments adaptor frame.
  Label adaptor_frame, try_allocate, runtime;
  __ LoadP(r5_p, MemOperand(fp, StandardFrameConstants::kCallerFPOffset));
  __ LoadP(r6_p, MemOperand(r5_p, StandardFrameConstants::kContextOffset));
  STATIC_ASSERT(StackFrame::ARGUMENTS_ADAPTOR < 0x3fffu);
  __ CmpSmiLiteral(r6_p, Smi::FromInt(StackFrame::ARGUMENTS_ADAPTOR), r0_p);
  __ beq(&adaptor_frame);

  // Get the length from the frame.
  __ LoadP(r4_p, MemOperand(sp, 0));
  __ b(&try_allocate);

  // Patch the arguments.length and the parameters pointer.
  __ bind(&adaptor_frame);
  __ LoadP(r4_p,
           MemOperand(r5_p, ArgumentsAdaptorFrameConstants::kLengthOffset));
  __ StoreP(r4_p, MemOperand(sp, 0));
  __ SmiToPtrArrayOffset(r6_p, r4_p);
  __ AddP(r6_p, r5_p);
  __ AddP(r6_p, Operand(StandardFrameConstants::kCallerSPOffset));
  __ StoreP(r6_p, MemOperand(sp, 1 * kPointerSize));

  // Try the new space allocation. Start out with computing the size
  // of the arguments object and the elements array in words.
  Label add_arguments_object;
  __ bind(&try_allocate);
  __ Cmpi(r4_p, Operand(0, RelocInfo::NONE));
  __ beq(&add_arguments_object);
  __ SmiUntag(r4_p);
  __ AddP(r4_p, Operand(FixedArray::kHeaderSize / kPointerSize));
  __ bind(&add_arguments_object);
  __ AddP(r4_p, Operand(Heap::kArgumentsObjectSizeStrict / kPointerSize));

  // Do the allocation of both objects in one go.
  __ AllocateInNewSpace(r4_p,
                        r3_p,
                        r5_p,
                        r6_p,
                        &runtime,
                        static_cast<AllocationFlags>(TAG_OBJECT |
                                                     SIZE_IN_WORDS));

  // Get the arguments boilerplate from the current native context.
  __ LoadP(r7_p,
           MemOperand(cp, Context::SlotOffset(Context::GLOBAL_OBJECT_INDEX)));
  __ LoadP(r7_p, FieldMemOperand(r7_p, GlobalObject::kNativeContextOffset));
  __ LoadP(r7_p, MemOperand(r7_p, Context::SlotOffset(
      Context::STRICT_MODE_ARGUMENTS_BOILERPLATE_INDEX)));

  // Copy the JS object part.
  __ CopyFields(r3_p, r7_p, r6_p.bit(), JSObject::kHeaderSize / kPointerSize);

  // Get the length (smi tagged) and set that as an in-object property too.
  STATIC_ASSERT(Heap::kArgumentsLengthIndex == 0);
  __ LoadP(r4_p, MemOperand(sp, 0 * kPointerSize));
  __ StoreP(r4_p, FieldMemOperand(r3_p, JSObject::kHeaderSize +
                                Heap::kArgumentsLengthIndex * kPointerSize));

  // If there are no actual arguments, we're done.
  Label done;
  __ Cmpi(r4_p, Operand(0, RelocInfo::NONE));
  __ beq(&done);

  // Get the parameters pointer from the stack.
  __ LoadP(r5_p, MemOperand(sp, 1 * kPointerSize));

  // Set up the elements pointer in the allocated arguments object and
  // initialize the header in the elements fixed array.
  __ LoadRR(r7_p, r3_p);
  __ AddP(r7_p, Operand(Heap::kArgumentsObjectSizeStrict));
  __ StoreP(r7_p, FieldMemOperand(r3_p, JSObject::kElementsOffset));
  __ LoadRoot(r6_p, Heap::kFixedArrayMapRootIndex);
  __ StoreP(r6_p, FieldMemOperand(r7_p, FixedArray::kMapOffset));
  __ StoreP(r4_p, FieldMemOperand(r7_p, FixedArray::kLengthOffset));
  // Untag the length for the loop.
  __ SmiUntag(r4_p);

  // Copy the fixed array slots.
  Label loop;
  // Set up r7_p to point to the first array slot.
  __ AddP(r7_p, Operand(FixedArray::kHeaderSize - kHeapObjectTag));
  __ bind(&loop);
  // Pre-decrement r5_p with kPointerSize on each iteration.
  // Pre-decrement in order to skip receiver.
  __ LoadPU(r6_p, MemOperand(r5_p, -kPointerSize));
  // Post-increment r7_p with kPointerSize on each iteration.
  __ StoreP(r6_p, MemOperand(r7_p));
  __ AddP(r7_p, Operand(kPointerSize));
  __ Sub(r4_p, Operand(1));
  __ Cmpi(r4_p, Operand(0, RelocInfo::NONE));
  __ bne(&loop);

  // Return and remove the on-stack parameters.
  __ bind(&done);
  __ AddP(sp, Operand(3 * kPointerSize));
  __ Ret();

  // Do the runtime call to allocate the arguments object.
  __ bind(&runtime);
  __ TailCallRuntime(Runtime::kNewStrictArgumentsFast, 3, 1);
}


void RegExpExecStub::Generate(MacroAssembler* masm) {
  // Just jump directly to runtime if native RegExp is not selected at compile
  // time or if regexp entry in generated code is turned off runtime switch or
  // at compilation.
#ifdef V8_INTERPRETED_REGEXP
  __ TailCallRuntime(Runtime::kRegExpExec, 4, 1);
#else  // V8_INTERPRETED_REGEXP

  // Stack frame on entry.
  //  sp[0]: last_match_info (expected JSArray)
  //  sp[4]: previous index
  //  sp[8]: subject string
  //  sp[12]: JSRegExp object

  const int kLastMatchInfoOffset = 0 * kPointerSize;
  const int kPreviousIndexOffset = 1 * kPointerSize;
  const int kSubjectOffset = 2 * kPointerSize;
  const int kJSRegExpOffset = 3 * kPointerSize;

  Label runtime, invoke_regexp, br_over, encoding_type_UC16;

  // Allocation of registers for this function. These are in callee save
  // registers and will be preserved by the call to the native RegExp code, as
  // this code is called using the normal C calling convention. When calling
  // directly from generated code the native RegExp code will not do a GC and
  // therefore the content of these registers are safe to use after the call.
  Register subject = r26_p;
  Register regexp_data = r27_p;
  Register last_match_info_elements = r28_p;
  Register code = r29_p;

  // Ensure register assigments are consistent with callee save masks
  ASSERT(subject.bit() & (kCalleeSaved & kRegExpCalleeSaved));
  ASSERT(regexp_data.bit() & (kCalleeSaved & kRegExpCalleeSaved));
  ASSERT(last_match_info_elements.bit() & (kCalleeSaved & kRegExpCalleeSaved));
  ASSERT(code.bit() & (kCalleeSaved & kRegExpCalleeSaved));

  // Ensure that a RegExp stack is allocated.
  Isolate* isolate = masm->isolate();
  ExternalReference address_of_regexp_stack_memory_address =
      ExternalReference::address_of_regexp_stack_memory_address(isolate);
  ExternalReference address_of_regexp_stack_memory_size =
      ExternalReference::address_of_regexp_stack_memory_size(isolate);
  __ mov(r3_p, Operand(address_of_regexp_stack_memory_size));
  __ LoadP(r3_p, MemOperand(r3_p, 0));
  __ Cmpi(r3_p, Operand::Zero());
  __ beq(&runtime);

  // Check that the first argument is a JSRegExp object.
  __ LoadP(r3_p, MemOperand(sp, kJSRegExpOffset));
  STATIC_ASSERT(kSmiTag == 0);
  __ JumpIfSmi(r3_p, &runtime);
  __ CompareObjectType(r3_p, r4_p, r4_p, JS_REGEXP_TYPE);
  __ bne(&runtime);

  // Check that the RegExp has been compiled (data contains a fixed array).
  __ LoadP(regexp_data, FieldMemOperand(r3_p, JSRegExp::kDataOffset));
  if (FLAG_debug_code) {
    STATIC_ASSERT(kSmiTagMask == 1);
    __ LoadRR(r0_p, regexp_data);
    __ AndP(r0_p, Operand(kSmiTagMask));
    __ Check(ne, "Unexpected type for RegExp data, FixedArray expected", cr0);
    __ CompareObjectType(regexp_data, r3_p, r3_p, FIXED_ARRAY_TYPE);
    __ Check(eq, "Unexpected type for RegExp data, FixedArray expected");
  }

  // regexp_data: RegExp data (FixedArray)
  // Check the type of the RegExp. Only continue if type is JSRegExp::IRREGEXP.
  __ LoadP(r3_p, FieldMemOperand(regexp_data, JSRegExp::kDataTagOffset));
  // ASSERT(Smi::FromInt(JSRegExp::IRREGEXP) < (char *)0xffffu);
  __ CmpSmiLiteral(r3_p, Smi::FromInt(JSRegExp::IRREGEXP), r0_p);
  __ bne(&runtime);

  // regexp_data: RegExp data (FixedArray)
  // Check that the number of captures fit in the static offsets vector buffer.
  __ LoadP(r5_p,
         FieldMemOperand(regexp_data, JSRegExp::kIrregexpCaptureCountOffset));
  // Calculate number of capture registers (number_of_captures + 1) * 2.
  __ SmiToShortArrayOffset(r5_p, r5_p);
  __ AddP(r5_p, Operand(2));
  // Check that the static offsets vector buffer is large enough.
  // STATIC_ASSERT(Isolate::kJSRegexpStaticOffsetsVectorSize < 0xffffu);
  __ Cmpli(r5_p, Operand(Isolate::kJSRegexpStaticOffsetsVectorSize));
  __ bgt(&runtime);

  // r5_p: Number of capture registers
  // regexp_data: RegExp data (FixedArray)
  // Check that the second argument is a string.
  __ LoadP(subject, MemOperand(sp, kSubjectOffset));
  __ JumpIfSmi(subject, &runtime);
  Condition is_string = masm->IsObjectStringType(subject, r3_p);
  __ b(NegateCondition(is_string), &runtime /*, cr0*/);
  // Get the length of the string to r6_p.
  __ LoadP(r6_p, FieldMemOperand(subject, String::kLengthOffset));

  // r5_p: Number of capture registers
  // r6_p: Length of subject string as a smi
  // subject: Subject string
  // regexp_data: RegExp data (FixedArray)
  // Check that the third argument is a positive smi less than the subject
  // string length. A negative value will be greater (unsigned comparison).
  __ LoadP(r3_p, MemOperand(sp, kPreviousIndexOffset));
  __ JumpIfNotSmi(r3_p, &runtime);
  __ Cmpl(r6_p, r3_p);
  __ ble(&runtime);

  // r5_p: Number of capture registers
  // subject: Subject string
  // regexp_data: RegExp data (FixedArray)
  // Check that the fourth object is a JSArray object.
  __ LoadP(r3_p, MemOperand(sp, kLastMatchInfoOffset));
  __ JumpIfSmi(r3_p, &runtime);
  __ CompareObjectType(r3_p, r4_p, r4_p, JS_ARRAY_TYPE);
  __ bne(&runtime);
  // Check that the JSArray is in fast case.
  __ LoadP(last_match_info_elements,
         FieldMemOperand(r3_p, JSArray::kElementsOffset));
  __ LoadP(r3_p, FieldMemOperand(last_match_info_elements,
                               HeapObject::kMapOffset));
  __ CompareRoot(r3_p, Heap::kFixedArrayMapRootIndex);
  __ bne(&runtime);
  // Check that the last match info has space for the capture registers and the
  // additional information.
  __ LoadP(r3_p,
         FieldMemOperand(last_match_info_elements, FixedArray::kLengthOffset));
  __ AddP(r5_p, Operand(RegExpImpl::kLastMatchOverhead));
  __ SmiUntag(r0_p, r3_p);
  __ CmpRR(r5_p, r0_p);
  __ bgt(&runtime);

  // Reset offset for possibly sliced string.
  __ LoadImmP(r11_p, Operand::Zero());
  // subject: Subject string
  // regexp_data: RegExp data (FixedArray)
  // Check the representation and encoding of the subject string.
  Label seq_string;
  __ LoadP(r3_p, FieldMemOperand(subject, HeapObject::kMapOffset));
  __ LoadlB(r3_p, FieldMemOperand(r3_p, Map::kInstanceTypeOffset));
  // First check for flat string.  None of the following string type tests will
  // succeed if subject is not a string or a short external string.
  STATIC_ASSERT((kIsNotStringMask |
                  kStringRepresentationMask |
                  kShortExternalStringMask) == 0x93);
  __ LoadRR(r4_p, r3_p);
  __ AndP(r4_p, Operand(kIsNotStringMask |
                          kStringRepresentationMask |
                          kShortExternalStringMask));
  STATIC_ASSERT((kStringTag | kSeqStringTag) == 0);
  __ beq(&seq_string /*, cr0*/);

  // subject: Subject string
  // regexp_data: RegExp data (FixedArray)
  // r4_p: whether subject is a string and if yes, its string representation
  // Check for flat cons string or sliced string.
  // A flat cons string is a cons string where the second part is the empty
  // string. In that case the subject string is just the first part of the cons
  // string. Also in this case the first part of the cons string is known to be
  // a sequential string or an external string.
  // In the case of a sliced string its offset has to be taken into account.
  Label cons_string, external_string, check_encoding;
  STATIC_ASSERT(kConsStringTag < kExternalStringTag);
  STATIC_ASSERT(kSlicedStringTag > kExternalStringTag);
  STATIC_ASSERT(kIsNotStringMask > kExternalStringTag);
  STATIC_ASSERT(kShortExternalStringTag > kExternalStringTag);
  STATIC_ASSERT(kExternalStringTag < 0xffffu);
  __ Cmpi(r4_p, Operand(kExternalStringTag));
  __ blt(&cons_string);
  __ beq(&external_string);

  // Catch non-string subject or short external string.
  STATIC_ASSERT(kNotStringTag != 0 && kShortExternalStringTag !=0);
  STATIC_ASSERT((kNotStringTag | kShortExternalStringTag) < 0xffffu);
  __ LoadRR(r0_p, r4_p);
  __ AndP(r0_p, Operand(kIsNotStringMask | kShortExternalStringMask));
  __ bne(&runtime /*, cr0*/);

  // String is sliced.
  __ LoadP(r11_p, FieldMemOperand(subject, SlicedString::kOffsetOffset));
  __ SmiUntag(r11_p);
  __ LoadP(subject, FieldMemOperand(subject, SlicedString::kParentOffset));
  // r11_p: offset of sliced string, smi-tagged.
  __ b(&check_encoding);
  // String is a cons string, check whether it is flat.
  __ bind(&cons_string);
  __ LoadP(r3_p, FieldMemOperand(subject, ConsString::kSecondOffset));
  __ CompareRoot(r3_p, Heap::kEmptyStringRootIndex);
  __ bne(&runtime);
  __ LoadP(subject, FieldMemOperand(subject, ConsString::kFirstOffset));
  // Is first part of cons or parent of slice a flat string?
  __ bind(&check_encoding);
  __ LoadP(r3_p, FieldMemOperand(subject, HeapObject::kMapOffset));
  __ LoadlB(r3_p, FieldMemOperand(r3_p, Map::kInstanceTypeOffset));
  STATIC_ASSERT(kSeqStringTag == 0);
  STATIC_ASSERT(kStringRepresentationMask == 3);
  __ LoadRR(r0_p, r3_p);
  __ AndP(r0_p, Operand(kStringRepresentationMask));
  __ bne(&external_string /*, cr0*/);

  __ bind(&seq_string);
  // subject: Subject string
  // regexp_data: RegExp data (FixedArray)
  // r3_p: Instance type of subject string
  STATIC_ASSERT(4 == kAsciiStringTag);
  STATIC_ASSERT(kTwoByteStringTag == 0);
  // Find the code object based on the assumptions above.
  STATIC_ASSERT(kStringEncodingMask == 4);
  __ ExtractBitMask(r6_p, r3_p, kStringEncodingMask, SetRC);
  __ beq(&encoding_type_UC16 /*, cr0*/);
  __ LoadP(code, FieldMemOperand(regexp_data, JSRegExp::kDataAsciiCodeOffset));
  __ b(&br_over);
  __ bind(&encoding_type_UC16);
  __ LoadP(code, FieldMemOperand(regexp_data, JSRegExp::kDataUC16CodeOffset));
  __ bind(&br_over);

  // Check that the irregexp code has been generated for the actual string
  // encoding. If it has, the field contains a code object otherwise it contains
  // a smi (code flushing support).
  __ JumpIfSmi(code, &runtime);

  // r6_p: encoding of subject string (1 if ASCII, 0 if two_byte);
  // code: Address of generated regexp code
  // subject: Subject string
  // regexp_data: RegExp data (FixedArray)
  // Load used arguments before starting to push arguments for call to native
  // RegExp code to avoid handling changing stack height.
  __ LoadP(r4_p, MemOperand(sp, kPreviousIndexOffset));
  __ SmiUntag(r4_p);

  // r4_p: previous index
  // r6_p: encoding of subject string (1 if ASCII, 0 if two_byte);
  // code: Address of generated regexp code
  // subject: Subject string
  // regexp_data: RegExp data (FixedArray)
  // All checks done. Now push arguments for native regexp code.
  __ IncrementCounter(isolate->counters()->regexp_entry_native(), 1, r3_p,
                       r5_p);

  // Isolates: note we add an additional parameter here (isolate pointer).
  const int kRegExpExecuteArguments = 10;
  const int kParameterRegisters = 8;
  __ EnterExitFrame(false, kRegExpExecuteArguments - kParameterRegisters);

  // Stack pointer now points to cell where return address is to be written.
  // Arguments are before that on the stack or in registers.

  // Argument 10 (in stack parameter area): Pass current isolate address.
  __ mov(r3_p, Operand(ExternalReference::isolate_address()));
  __ StoreP(r3_p,
            MemOperand(sp, (kStackFrameExtraParamSlot + 1) * kPointerSize));

  // Argument 9 is a dummy that reserves the space used for
  // the return address added by the ExitFrame in native calls.

  // Argument 8 (r10_p): Indicate that this is a direct call from JavaScript.
  __ LoadImmP(r10_p, Operand(1));

  // Argument 7 (r9_p): Start (high end) of backtracking stack memory area.
  __ mov(r3_p, Operand(address_of_regexp_stack_memory_address));
  __ LoadP(r3_p, MemOperand(r3_p, 0));
  __ mov(r5_p, Operand(address_of_regexp_stack_memory_size));
  __ LoadP(r5_p, MemOperand(r5_p, 0));
  __ LoadRR(r9_p, r3_p);
  __ AddP(r9_p, r5_p);

  // Argument 6 (r8_p): Set the number of capture registers to zero to force
  // global egexps to behave as non-global.  This does not affect non-global
  // regexps.
  __ LoadImmP(r8_p, Operand::Zero());

  // Argument 5 (r7_p): static offsets vector buffer.
  __ mov(r7_p,
         Operand(ExternalReference::address_of_static_offsets_vector(isolate)));

  // For arguments 4 (r6_p) and 3 (r5_p) get string length, calculate start of
  // string data and calculate the shift of the index (0 for ASCII and 1 for
  // two byte).
  __ LoadRR(r22_p, subject);
  __ AddP(r22_p, Operand(SeqString::kHeaderSize - kHeapObjectTag));
  __ xori(r6_p, r6_p, Operand(1));
  // Load the length from the original subject string from the previous stack
  // frame. Therefore we have to use fp, which points exactly to two pointer
  // sizes below the previous sp. (Because creating a new stack frame pushes
  // the previous fp onto the stack and moves up sp by 2 * kPointerSize.)
  __ LoadP(subject, MemOperand(fp, kSubjectOffset + 2 * kPointerSize));
  // If slice offset is not 0, load the length from the original sliced string.
  // Argument 4, r6_p: End of string data
  // Argument 3, r5_p: Start of string data
  // Prepare start and end index of the input.
  __ ShiftLeft(r11_p, r11_p, r6_p);
  __ Add(r11_p, r22_p, r11_p);
  __ ShiftLeft(r5_p, r4_p, r6_p);
  __ Add(r5_p, r11_p, r5_p);

  __ LoadP(r22_p, FieldMemOperand(subject, String::kLengthOffset));
  __ SmiUntag(r22_p);
  __ ShiftLeft(r6_p, r22_p, r6_p);
  __ Add(r6_p, r11_p, r6_p);

  // Argument 2 (r4_p): Previous index.
  // Already there

  // Argument 1 (r3_p): Subject string.
  __ LoadRR(r3_p, subject);

  // Locate the code entry and call it.
  __ AddP(code, Operand(Code::kHeaderSize - kHeapObjectTag));


#if defined(USE_SIMULATOR) && \
  (defined(_AIX) || defined(V8_TARGET_ARCH_S390X))
  // Even Simulated AIX/PPC64 Linux uses a function descriptor for the
  // RegExp routine.  Extract the instruction address here since
  // DirectCEntryStub::GenerateCall will not do it for calls out to
  // what it thinks is C code compiled for the simulator/host
  // platform.
  __ LoadP(code, MemOperand(code, 0));  // Instruction address
#endif

  DirectCEntryStub stub;
  stub.GenerateCall(masm, code);

  __ LeaveExitFrame(false, no_reg);

  // r3_p: result
  // subject: subject string (callee saved)
  // regexp_data: RegExp data (callee saved)
  // last_match_info_elements: Last match info elements (callee saved)

  // Check the result.
  Label success;

  __ Cmpi(r3_p, Operand(1));
  // We expect exactly one result since we force the called regexp to behave
  // as non-global.
  __ beq(&success);
  Label failure;
  __ Cmpi(r3_p, Operand(NativeRegExpMacroAssembler::FAILURE));
  __ beq(&failure);
  __ Cmpi(r3_p, Operand(NativeRegExpMacroAssembler::EXCEPTION));
  // If not exception it can only be retry. Handle that in the runtime system.
  __ bne(&runtime);
  // Result must now be exception. If there is no pending exception already a
  // stack overflow (on the backtrack stack) was detected in RegExp code but
  // haven't created the exception yet. Handle that in the runtime system.
  // TODO(592): Rerunning the RegExp to get the stack overflow exception.
  __ mov(r4_p, Operand(isolate->factory()->the_hole_value()));
  __ mov(r5_p, Operand(ExternalReference(Isolate::kPendingExceptionAddress,
                                       isolate)));
  __ LoadP(r3_p, MemOperand(r5_p, 0));
  __ CmpRR(r3_p, r4_p);
  __ beq(&runtime);

  __ StoreP(r4_p, MemOperand(r5_p, 0));  // Clear pending exception.

  // Check if the exception is a termination. If so, throw as uncatchable.
  __ CompareRoot(r3_p, Heap::kTerminationExceptionRootIndex);

  Label termination_exception;
  __ beq(&termination_exception);

  __ Throw(r3_p);

  __ bind(&termination_exception);
  __ ThrowUncatchable(r3_p);

  __ bind(&failure);
  // For failure and exception return null.
  __ mov(r3_p, Operand(masm->isolate()->factory()->null_value()));
  __ AddP(sp, Operand(4 * kPointerSize));
  __ Ret();

  // Process the result from the native regexp code.
  __ bind(&success);
  __ LoadP(r4_p,
         FieldMemOperand(regexp_data, JSRegExp::kIrregexpCaptureCountOffset));
  // Calculate number of capture registers (number_of_captures + 1) * 2.
  __ SmiToShortArrayOffset(r4_p, r4_p);
  __ AddP(r4_p, Operand(2));

  // r4_p: number of capture registers
  // r26_p: subject string
  // Store the capture count.
  __ SmiTag(r5_p, r4_p);
  __ StoreP(r5_p, FieldMemOperand(last_match_info_elements,
                                RegExpImpl::kLastCaptureCountOffset));
  // Store last subject and last input.
  __ StoreP(subject,
            FieldMemOperand(last_match_info_elements,
                            RegExpImpl::kLastSubjectOffset));
  __ LoadRR(r5_p, subject);
  __ RecordWriteField(last_match_info_elements,
                      RegExpImpl::kLastSubjectOffset,
                      r5_p,
                      r10_p,
                      kLRHasNotBeenSaved,
                      kDontSaveFPRegs);
  __ StoreP(subject,
            FieldMemOperand(last_match_info_elements,
                            RegExpImpl::kLastInputOffset));
  __ RecordWriteField(last_match_info_elements,
                      RegExpImpl::kLastInputOffset,
                      subject,
                      r10_p,
                      kLRHasNotBeenSaved,
                      kDontSaveFPRegs);

  // Get the static offsets vector filled by the native regexp code.
  ExternalReference address_of_static_offsets_vector =
      ExternalReference::address_of_static_offsets_vector(isolate);
  __ mov(r5_p, Operand(address_of_static_offsets_vector));

  // r4_p: number of capture registers
  // r5_p: offsets vector
  Label next_capture;
  // Capture register counter starts from number of capture registers and
  // counts down until wraping after zero.
  __ LoadRR(r3_p, last_match_info_elements);
  __ AddP(r3_p, Operand(RegExpImpl::kFirstCaptureOffset - kHeapObjectTag -
                  kPointerSize));
  __ AddP(r5_p, Operand(-kIntSize));  // bias down for lwzu
  __ mtctr(r4_p);
  __ bind(&next_capture);
  // Read the value from the static offsets vector buffer.
  __ lwzu(r6_p, MemOperand(r5_p, kIntSize));
  // Store the smi value in the last match info.
  __ SmiTag(r6_p);
  __ StorePU(r6_p, MemOperand(r3_p, kPointerSize));
  __ bdnz(&next_capture);

  // Return last match info.
  __ LoadP(r3_p, MemOperand(sp, kLastMatchInfoOffset));
  __ AddP(sp, Operand(4 * kPointerSize));
  __ Ret();

  // External string.  Short external strings have already been ruled out.
  // r3_p: scratch
  __ bind(&external_string);
  __ LoadP(r3_p, FieldMemOperand(subject, HeapObject::kMapOffset));
  __ LoadlB(r3_p, FieldMemOperand(r3_p, Map::kInstanceTypeOffset));
  if (FLAG_debug_code) {
    // Assert that we do not have a cons or slice (indirect strings) here.
    // Sequential strings have already been ruled out.
    STATIC_ASSERT(kIsIndirectStringMask == 1);
    __ LoadRR(r0_p, r3_p);
    __ AndP(r0_p, Operand(kIsIndirectStringMask));
    __ Assert(eq, "external string expected, but not found", cr0);
  }
  __ LoadP(subject,
         FieldMemOperand(subject, ExternalString::kResourceDataOffset));
  // Move the pointer so that offset-wise, it looks like a sequential string.
  STATIC_ASSERT(SeqTwoByteString::kHeaderSize == SeqAsciiString::kHeaderSize);
  __ Sub(subject,
         subject,
         Operand(SeqTwoByteString::kHeaderSize - kHeapObjectTag));
  __ b(&seq_string);

  // Do the runtime call to execute the regexp.
  __ bind(&runtime);
  __ TailCallRuntime(Runtime::kRegExpExec, 4, 1);
#endif  // V8_INTERPRETED_REGEXP
}


void RegExpConstructResultStub::Generate(MacroAssembler* masm) {
  const int kMaxInlineLength = 100;
  Label slowcase;
  Label done;
  Factory* factory = masm->isolate()->factory();

  __ LoadP(r4_p, MemOperand(sp, kPointerSize * 2));
  __ JumpIfNotSmi(r4_p, &slowcase);
  __ CmplSmiLiteral(r4_p, Smi::FromInt(kMaxInlineLength), r0_p);
  __ bgt(&slowcase);
  // Allocate RegExpResult followed by FixedArray with size in ebx.
  // JSArray:   [Map][empty properties][Elements][Length-smi][index][input]
  // Elements:  [Map][Length][..elements..]
  // Size of JSArray with two in-object properties and the header of a
  // FixedArray.
  int objects_size =
      (JSRegExpResult::kSize + FixedArray::kHeaderSize) / kPointerSize;
  __ SmiUntag(r8_p, r4_p);
  __ LoadRR(r5_p, r8_p);
  __ AddP(r5_p, Operand(objects_size));
  // Future optimization: defer tagging the result pointer for more
  // efficient 64-bit memory accesses (due to alignment requirements
  // on the memoperand offset).
  __ AllocateInNewSpace(
      r5_p,  // In: Size, in words.
      r3_p,  // Out: Start of allocation (tagged).
      r6_p,  // Scratch register.
      r7_p,  // Scratch register.
      &slowcase,
      static_cast<AllocationFlags>(TAG_OBJECT | SIZE_IN_WORDS));
  // r3_p: Start of allocated area, object-tagged.
  // r4_p: Number of elements in array, as smi.
  // r8_p: Number of elements, untagged.

  // Set JSArray map to global.regexp_result_map().
  // Set empty properties FixedArray.
  // Set elements to point to FixedArray allocated right after the JSArray.
  // Interleave operations for better latency.
  __ LoadP(r5_p, ContextOperand(cp, Context::GLOBAL_OBJECT_INDEX));
  __ LoadRR(r6_p, r3_p);
  __ AddP(r6_p, Operand(JSRegExpResult::kSize));
  __ mov(r7_p, Operand(factory->empty_fixed_array()));
  __ LoadP(r5_p, FieldMemOperand(r5_p, GlobalObject::kNativeContextOffset));
  __ StoreP(r6_p, FieldMemOperand(r3_p, JSObject::kElementsOffset));
  __ LoadP(r5_p, ContextOperand(r5_p, Context::REGEXP_RESULT_MAP_INDEX));
  __ StoreP(r7_p, FieldMemOperand(r3_p, JSObject::kPropertiesOffset));
  __ StoreP(r5_p, FieldMemOperand(r3_p, HeapObject::kMapOffset));

  // Set input, index and length fields from arguments.
  __ LoadP(r4_p, MemOperand(sp, kPointerSize * 0));
  __ LoadP(r5_p, MemOperand(sp, kPointerSize * 1));
  __ LoadP(r9_p, MemOperand(sp, kPointerSize * 2));
  __ StoreP(r4_p, FieldMemOperand(r3_p, JSRegExpResult::kInputOffset));
  __ StoreP(r5_p, FieldMemOperand(r3_p, JSRegExpResult::kIndexOffset));
  __ StoreP(r9_p, FieldMemOperand(r3_p, JSArray::kLengthOffset));

  // Fill out the elements FixedArray.
  // r3_p: JSArray, tagged.
  // r6_p: FixedArray, tagged.
  // r8_p: Number of elements in array, untagged.

  // Set map.
  __ mov(r5_p, Operand(factory->fixed_array_map()));
  __ StoreP(r5_p, FieldMemOperand(r6_p, HeapObject::kMapOffset));
  // Set FixedArray length.
  __ SmiTag(r9_p, r8_p);
  __ StoreP(r9_p, FieldMemOperand(r6_p, FixedArray::kLengthOffset));
  // Fill contents of fixed-array with undefined.
  __ LoadRoot(r5_p, Heap::kUndefinedValueRootIndex);
  __ AddP(r6_p, Operand(FixedArray::kHeaderSize - kHeapObjectTag));
  // Fill fixed array elements with undefined.
  // r3_p: JSArray, tagged.
  // r5_p: undefined.
  // r6_p: Start of elements in FixedArray.
  // r8_p: Number of elements to fill.
  Label loop;
  __ Cmpi(r8_p, Operand::Zero());
  __ bind(&loop);
  __ ble(&done);  // Jump if r8_p is negative or zero.
  __ Sub(r8_p, Operand(1));
  __ ShiftLeftImm(ip, r8_p, Operand(kPointerSizeLog2));
  __ StorePX(r5_p, MemOperand(ip, r6_p));
  __ Cmpi(r8_p, Operand::Zero());
  __ b(&loop);

  __ bind(&done);
  __ AddP(sp, Operand(3 * kPointerSize));
  __ Ret();

  __ bind(&slowcase);
  __ TailCallRuntime(Runtime::kRegExpConstructResult, 3, 1);
}


static void GenerateRecordCallTarget(MacroAssembler* masm) {
  // Cache the called function in a global property cell.  Cache states
  // are uninitialized, monomorphic (indicated by a JSFunction), and
  // megamorphic.
  // r4_p : the function to call
  // r5_p : cache cell for call target
  Label initialize, done;
  const Register scratch = r6_p;

  ASSERT_EQ(*TypeFeedbackCells::MegamorphicSentinel(masm->isolate()),
            masm->isolate()->heap()->undefined_value());
  ASSERT_EQ(*TypeFeedbackCells::UninitializedSentinel(masm->isolate()),
            masm->isolate()->heap()->the_hole_value());

  // Load the cache state into scratch.
  __ LoadP(scratch, FieldMemOperand(r5_p, JSGlobalPropertyCell::kValueOffset));

  // A monomorphic cache hit or an already megamorphic state: invoke the
  // function without changing the state.
  __ CmpRR(scratch, r4_p);
  __ beq(&done);
  __ CompareRoot(scratch, Heap::kUndefinedValueRootIndex);
  __ beq(&done);

  // A monomorphic miss (i.e, here the cache is not uninitialized) goes
  // megamorphic.
  __ CompareRoot(scratch, Heap::kTheHoleValueRootIndex);
  __ beq(&initialize);
  // MegamorphicSentinel is an immortal immovable object (undefined) so no
  // write-barrier is needed.
  __ LoadRoot(ip, Heap::kUndefinedValueRootIndex);
  __ StoreP(ip, FieldMemOperand(r5_p, JSGlobalPropertyCell::kValueOffset));
  __ b(&done);

  // An uninitialized cache is patched with the function.
  __ bind(&initialize);
  __ StoreP(r4_p, FieldMemOperand(r5_p, JSGlobalPropertyCell::kValueOffset));
  // No need for a write barrier here - cells are rescanned.

  __ bind(&done);
}


void CallFunctionStub::Generate(MacroAssembler* masm) {
  // r4_p : the function to call
  // r5_p : cache cell for call target
  Label slow, non_function;

  // The receiver might implicitly be the global object. This is
  // indicated by passing the hole as the receiver to the call
  // function stub.
  if (ReceiverMightBeImplicit()) {
    Label call;
    // Get the receiver from the stack.
    // function, receiver [, arguments]
    __ LoadP(r7_p, MemOperand(sp, argc_ * kPointerSize), r0_p);
    // Call as function is indicated with the hole.
    __ CompareRoot(r7_p, Heap::kTheHoleValueRootIndex);
    __ bne(&call);
    // Patch the receiver on the stack with the global receiver object.
    __ LoadP(r6_p,
             MemOperand(cp, Context::SlotOffset(Context::GLOBAL_OBJECT_INDEX)));
    __ LoadP(r6_p, FieldMemOperand(r6_p, GlobalObject::kGlobalReceiverOffset));
    __ StoreP(r6_p, MemOperand(sp, argc_ * kPointerSize));
    __ bind(&call);
  }

  // Check that the function is really a JavaScript function.
  // r4_p: pushed function (to be verified)
  __ JumpIfSmi(r4_p, &non_function);
  // Get the map of the function object.
  __ CompareObjectType(r4_p, r6_p, r6_p, JS_FUNCTION_TYPE);
  __ bne(&slow);

  if (RecordCallTarget()) {
    GenerateRecordCallTarget(masm);
  }

  // Fast-case: Invoke the function now.
  // r4_p: pushed function
  ParameterCount actual(argc_);

  if (ReceiverMightBeImplicit()) {
    Label call_as_function;
    __ CompareRoot(r7_p, Heap::kTheHoleValueRootIndex);
    __ beq(&call_as_function);
    __ InvokeFunction(r4_p,
                      actual,
                      JUMP_FUNCTION,
                      NullCallWrapper(),
                      CALL_AS_METHOD);
    __ bind(&call_as_function);
  }
  __ InvokeFunction(r4_p,
                    actual,
                    JUMP_FUNCTION,
                    NullCallWrapper(),
                    CALL_AS_FUNCTION);

  // Slow-case: Non-function called.
  __ bind(&slow);
  if (RecordCallTarget()) {
    // If there is a call target cache, mark it megamorphic in the
    // non-function case.  MegamorphicSentinel is an immortal immovable
    // object (undefined) so no write barrier is needed.
    ASSERT_EQ(*TypeFeedbackCells::MegamorphicSentinel(masm->isolate()),
              masm->isolate()->heap()->undefined_value());
    __ LoadRoot(ip, Heap::kUndefinedValueRootIndex);
    __ StoreP(ip, FieldMemOperand(r5_p, JSGlobalPropertyCell::kValueOffset));
  }
  // Check for function proxy.
  STATIC_ASSERT(JS_FUNCTION_PROXY_TYPE < 0xffffu);
  __ Cmpi(r6_p, Operand(JS_FUNCTION_PROXY_TYPE));
  __ bne(&non_function);
  __ push(r4_p);  // put proxy as additional argument
  __ LoadImmP(r3_p, Operand(argc_ + 1));
  __ LoadImmP(r5_p, Operand::Zero());
  __ GetBuiltinEntry(r6_p, Builtins::CALL_FUNCTION_PROXY);
  __ SetCallKind(r8_p, CALL_AS_METHOD);
  {
    Handle<Code> adaptor =
      masm->isolate()->builtins()->ArgumentsAdaptorTrampoline();
    __ Jump(adaptor, RelocInfo::CODE_TARGET);
  }

  // CALL_NON_FUNCTION expects the non-function callee as receiver (instead
  // of the original receiver from the call site).
  __ bind(&non_function);
  __ StoreP(r4_p, MemOperand(sp, argc_ * kPointerSize));
  __ LoadImmP(r3_p, Operand(argc_));  // Set up the number of arguments.
  __ LoadImmP(r5_p, Operand::Zero());
  __ GetBuiltinEntry(r6_p, Builtins::CALL_NON_FUNCTION);
  __ SetCallKind(r8_p, CALL_AS_METHOD);
  __ Jump(masm->isolate()->builtins()->ArgumentsAdaptorTrampoline(),
          RelocInfo::CODE_TARGET);
}


void CallConstructStub::Generate(MacroAssembler* masm) {
  // r3_p : number of arguments
  // r4_p : the function to call
  // r5_p : cache cell for call target
  Label slow, non_function_call;

  // Check that the function is not a smi.
  __ JumpIfSmi(r4_p, &non_function_call);
  // Check that the function is a JSFunction.
  __ CompareObjectType(r4_p, r6_p, r6_p, JS_FUNCTION_TYPE);
  __ bne(&slow);

  if (RecordCallTarget()) {
    GenerateRecordCallTarget(masm);
  }

  // Jump to the function-specific construct stub.
  __ LoadP(r5_p, FieldMemOperand(r4_p, JSFunction::kSharedFunctionInfoOffset));
  __ LoadP(r5_p, FieldMemOperand(r5_p,
           SharedFunctionInfo::kConstructStubOffset));
  __ LoadRR(r0_p, r5_p);
  __ AddP(r0_p, Operand(Code::kHeaderSize - kHeapObjectTag));
  __ Jump(r0_p);

  // r3_p: number of arguments
  // r4_p: called object
  // r6_p: object type
  Label do_call;
  __ bind(&slow);
  STATIC_ASSERT(JS_FUNCTION_PROXY_TYPE < 0xffffu);
  __ Cmpi(r6_p, Operand(JS_FUNCTION_PROXY_TYPE));
  __ bne(&non_function_call);
  __ GetBuiltinEntry(r6_p, Builtins::CALL_FUNCTION_PROXY_AS_CONSTRUCTOR);
  __ b(&do_call);

  __ bind(&non_function_call);
  __ GetBuiltinEntry(r6_p, Builtins::CALL_NON_FUNCTION_AS_CONSTRUCTOR);
  __ bind(&do_call);
  // Set expected number of arguments to zero (not changing r3_p).
  __ LoadImmP(r5_p, Operand::Zero());
  __ SetCallKind(r8_p, CALL_AS_METHOD);
  __ Jump(masm->isolate()->builtins()->ArgumentsAdaptorTrampoline(),
          RelocInfo::CODE_TARGET);
}


// Unfortunately you have to run without snapshots to see most of these
// names in the profile since most compare stubs end up in the snapshot.
void CompareStub::PrintName(StringStream* stream) {
  ASSERT((lhs_.is(r3_p) && rhs_.is(r4_p)) ||
         (lhs_.is(r4_p) && rhs_.is(r3_p)));
  const char* cc_name;
  switch (cc_) {
    case lt: cc_name = "LT"; break;
    case gt: cc_name = "GT"; break;
    case le: cc_name = "LE"; break;
    case ge: cc_name = "GE"; break;
    case eq: cc_name = "EQ"; break;
    case ne: cc_name = "NE"; break;
    default: cc_name = "UnknownCondition"; break;
  }
  bool is_equality = cc_ == eq || cc_ == ne;
  stream->Add("CompareStub_%s", cc_name);
  stream->Add(lhs_.is(r3_p) ? "_r3" : "_r4");
  stream->Add(rhs_.is(r3_p) ? "_r3" : "_r4");
  if (strict_ && is_equality) stream->Add("_STRICT");
  if (never_nan_nan_ && is_equality) stream->Add("_NO_NAN");
  if (!include_number_compare_) stream->Add("_NO_NUMBER");
  if (!include_smi_compare_) stream->Add("_NO_SMI");
}


int CompareStub::MinorKey() {
  // Encode the three parameters in a unique 16 bit value. To avoid duplicate
  // stubs the never NaN NaN condition is only taken into account if the
  // condition is equals.
  ASSERT(static_cast<unsigned>(cc_) < (1 << 12));
  ASSERT((lhs_.is(r3_p) && rhs_.is(r4_p)) ||
         (lhs_.is(r4_p) && rhs_.is(r3_p)));
  return ConditionField::encode(static_cast<unsigned>(cc_))
         | RegisterField::encode(lhs_.is(r3_p))
         | StrictField::encode(strict_)
         | NeverNanNanField::encode(cc_ == eq ? never_nan_nan_ : false)
         | IncludeNumberCompareField::encode(include_number_compare_)
         | IncludeSmiCompareField::encode(include_smi_compare_);
}


// StringCharCodeAtGenerator
void StringCharCodeAtGenerator::GenerateFast(MacroAssembler* masm) {
  Label flat_string;
  Label ascii_string;
  Label got_char_code;
  Label sliced_string;

  // If the receiver is a smi trigger the non-string case.
  __ JumpIfSmi(object_, receiver_not_string_);

  // Fetch the instance type of the receiver into result register.
  __ LoadP(result_, FieldMemOperand(object_, HeapObject::kMapOffset));
  __ LoadlB(result_, FieldMemOperand(result_, Map::kInstanceTypeOffset));
  // If the receiver is not a string trigger the non-string case.
  __ LoadRR(r0_p, result_);
  __ AndP(r0_p, Operand(kIsNotStringMask));
  __ bne(receiver_not_string_ /*, cr0*/);

  // If the index is non-smi trigger the non-smi case.
  __ JumpIfNotSmi(index_, &index_not_smi_);
  __ bind(&got_smi_index_);

  // Check for index out of range.
  __ LoadP(ip, FieldMemOperand(object_, String::kLengthOffset));
  __ Cmpl(ip, index_);
  __ ble(index_out_of_range_);

  __ SmiUntag(index_);

  StringCharLoadGenerator::Generate(masm,
                                    object_,
                                    index_,
                                    result_,
                                    &call_runtime_);

  __ SmiTag(result_, result_);
  __ bind(&exit_);
}


void StringCharCodeAtGenerator::GenerateSlow(
    MacroAssembler* masm,
    const RuntimeCallHelper& call_helper) {
  __ Abort("Unexpected fallthrough to CharCodeAt slow case");

  // Index is not a smi.
  __ bind(&index_not_smi_);
  // If index is a heap number, try converting it to an integer.
  __ CheckMap(index_,
              result_,
              Heap::kHeapNumberMapRootIndex,
              index_not_number_,
              DONT_DO_SMI_CHECK);
  call_helper.BeforeCall(masm);
  __ push(object_);
  __ push(index_);  // Consumed by runtime conversion function.
  if (index_flags_ == STRING_INDEX_IS_NUMBER) {
    __ CallRuntime(Runtime::kNumberToIntegerMapMinusZero, 1);
  } else {
    ASSERT(index_flags_ == STRING_INDEX_IS_ARRAY_INDEX);
    // NumberToSmi discards numbers that are not exact integers.
    __ CallRuntime(Runtime::kNumberToSmi, 1);
  }
  // Save the conversion result before the pop instructions below
  // have a chance to overwrite it.
  __ Move(index_, r3_p);
  __ pop(object_);
  // Reload the instance type.
  __ LoadP(result_, FieldMemOperand(object_, HeapObject::kMapOffset));
  __ LoadlB(result_, FieldMemOperand(result_, Map::kInstanceTypeOffset));
  call_helper.AfterCall(masm);
  // If index is still not a smi, it must be out of range.
  __ JumpIfNotSmi(index_, index_out_of_range_);
  // Otherwise, return to the fast path.
  __ b(&got_smi_index_);

  // Call runtime. We get here when the receiver is a string and the
  // index is a number, but the code of getting the actual character
  // is too complex (e.g., when the string needs to be flattened).
  __ bind(&call_runtime_);
  call_helper.BeforeCall(masm);
  __ SmiTag(index_);
  __ Push(object_, index_);
  __ CallRuntime(Runtime::kStringCharCodeAt, 2);
  __ Move(result_, r3_p);
  call_helper.AfterCall(masm);
  __ b(&exit_);

  __ Abort("Unexpected fallthrough from CharCodeAt slow case");
}


// -------------------------------------------------------------------------
// StringCharFromCodeGenerator

  void StringCharFromCodeGenerator::GenerateFast(MacroAssembler* masm) {
  // Fast case of Heap::LookupSingleCharacterStringFromCode.
  ASSERT(IsPowerOf2(String::kMaxAsciiCharCode + 1));
  __ LoadSmiLiteral(r0_p, Smi::FromInt(~String::kMaxAsciiCharCode));
  __ ori(r0_p, r0_p, Operand(kSmiTagMask));
  __ AndP(r0_p, code_);
  __ Cmpi(r0_p, Operand::Zero());
  __ bne(&slow_case_);

  __ LoadRoot(result_, Heap::kSingleCharacterStringCacheRootIndex);
  // At this point code register contains smi tagged ASCII char code.
  __ LoadRR(r0_p, code_);
  __ SmiToPtrArrayOffset(code_, code_);
  __ AddP(result_, code_);
  __ LoadRR(code_, r0_p);
  __ LoadP(result_, FieldMemOperand(result_, FixedArray::kHeaderSize));
  __ CompareRoot(result_, Heap::kUndefinedValueRootIndex);
  __ beq(&slow_case_);
  __ bind(&exit_);
}


void StringCharFromCodeGenerator::GenerateSlow(
    MacroAssembler* masm,
    const RuntimeCallHelper& call_helper) {
  __ Abort("Unexpected fallthrough to CharFromCode slow case");

  __ bind(&slow_case_);
  call_helper.BeforeCall(masm);
  __ push(code_);
  __ CallRuntime(Runtime::kCharFromCode, 1);
  __ Move(result_, r3_p);
  call_helper.AfterCall(masm);
  __ b(&exit_);

  __ Abort("Unexpected fallthrough from CharFromCode slow case");
}


// -------------------------------------------------------------------------
// StringCharAtGenerator

void StringCharAtGenerator::GenerateFast(MacroAssembler* masm) {
  char_code_at_generator_.GenerateFast(masm);
  char_from_code_generator_.GenerateFast(masm);
}


void StringCharAtGenerator::GenerateSlow(
    MacroAssembler* masm,
    const RuntimeCallHelper& call_helper) {
  char_code_at_generator_.GenerateSlow(masm, call_helper);
  char_from_code_generator_.GenerateSlow(masm, call_helper);
}


void StringHelper::GenerateCopyCharacters(MacroAssembler* masm,
                                          Register dest,
                                          Register src,
                                          Register count,
                                          Register scratch,
                                          bool ascii) {
  Label loop;
  __ bind(&loop);
  // This loop just copies one character at a time, as it is only used for very
  // short strings.
  if (ascii) {
    __ LoadlB(scratch, MemOperand(src));
    __ stb(scratch, MemOperand(dest));
    __ AddP(src, Operand(1));
    __ AddP(dest, Operand(1));
  } else {
    __ lhz(scratch, MemOperand(src));
    __ sth(scratch, MemOperand(dest));
    __ AddP(src, Operand(2));
    __ AddP(dest, Operand(2));
  }
  __ Sub(count, Operand(1));
  __ Cmpi(count, Operand::Zero());
  __ bgt(&loop);
}


enum CopyCharactersFlags {
  COPY_ASCII = 1,
  DEST_ALWAYS_ALIGNED = 2
};


// roohack - optimization opportunity here, stringcopy is important
// and the current version below is very dumb
void StringHelper::GenerateCopyCharactersLong(MacroAssembler* masm,
                                              Register dest,
                                              Register src,
                                              Register count,
                                              Register scratch1,
                                              Register scratch2,
                                              Register scratch3,
                                              Register scratch4,
                                              Register scratch5,
                                              int flags) {
  bool ascii = (flags & COPY_ASCII) != 0;
  bool dest_always_aligned = (flags & DEST_ALWAYS_ALIGNED) != 0;

  if (dest_always_aligned && FLAG_debug_code) {
    // Check that destination is actually word aligned if the flag says
    // that it is.
    __ LoadRR(r0_p, dest);
    __ AndP(r0_p, Operand(kPointerAlignmentMask));
    __ Check(eq, "Destination of copy not aligned.", cr0);
  }

  // Nothing to do for zero characters.
  Label done;
  if (!ascii) {  // for non-ascii, double the length
    __ AddP(count, count);
  }
  __ Cmpi(count, Operand(0, RelocInfo::NONE));
  __ beq(&done);

  // Assume that you cannot read (or write) unaligned.
  Label byte_loop;
  __ AddP(count, dest);
  Register limit = count;  // Read until src equals this.
  // Copy bytes from src to dst until dst hits limit.
  __ bind(&byte_loop);
  __ CmpRR(dest, limit);
  __ bge(&done);
  __ LoadlB(scratch1, MemOperand(src));
  __ AddP(src, Operand(1));
  __ stb(scratch1, MemOperand(dest));
  __ AddP(dest, Operand(1));
  __ b(&byte_loop);

  __ bind(&done);
}


void StringHelper::GenerateTwoCharacterSymbolTableProbe(MacroAssembler* masm,
                                                        Register c1,
                                                        Register c2,
                                                        Register scratch1,
                                                        Register scratch2,
                                                        Register scratch3,
                                                        Register scratch4,
                                                        Register scratch5,
                                                        Label* not_found) {
  // Register scratch3 is the general scratch register in this function.
  Register scratch = scratch3;

  // Make sure that both characters are not digits as such strings has a
  // different hash algorithm. Don't try to look for these in the symbol table.
  Label not_array_index;
  __ Sub(scratch, c1, Operand(static_cast<intptr_t>('0')));
  __ Cmpli(scratch, Operand(static_cast<intptr_t>('9' - '0')));
  __ bgt(&not_array_index);
  __ Sub(scratch, c2, Operand(static_cast<intptr_t>('0')));
  __ Cmpli(scratch, Operand(static_cast<intptr_t>('9' - '0')));
  __ bgt(&not_array_index);

  // If check failed combine both characters into single halfword.
  // This is required by the contract of the method: code at the
  // not_found branch expects this combination in c1 register
#if __BYTE_ORDER == __BIG_ENDIAN
  __ ShiftLeftImm(c1, c1, Operand(kBitsPerByte));
  __ OrP(c1, c2);
#else
  __ ShiftLeftImm(r0_p, c2, Operand(kBitsPerByte));
  __ OrP(c1, r0_p);
#endif
  __ b(not_found);

  __ bind(&not_array_index);
  // Calculate the two character string hash.
  Register hash = scratch1;
  StringHelper::GenerateHashInit(masm, hash, c1, scratch);
  StringHelper::GenerateHashAddCharacter(masm, hash, c2, scratch);
  StringHelper::GenerateHashGetHash(masm, hash, scratch);

  // Collect the two characters in a register.
  Register chars = c1;
#if __BYTE_ORDER == __BIG_ENDIAN
  __ ShiftLeftImm(c1, c1, Operand(kBitsPerByte));
  __ LoadRR(chars, c2);
  __ OrP(chars, c1);
#else
  __ ShiftLeftImm(r0_p, c2, Operand(kBitsPerByte));
  __ LoadRR(chars, r0_p);
  __ OrP(chars, c1);
#endif

  // chars: two character string, char 1 in byte 0 and char 2 in byte 1.
  // hash:  hash of two character string.

  // Load symbol table
  // Load address of first element of the symbol table.
  Register symbol_table = c2;
  __ LoadRoot(symbol_table, Heap::kSymbolTableRootIndex);

  Register undefined = scratch4;
  __ LoadRoot(undefined, Heap::kUndefinedValueRootIndex);

  // Calculate capacity mask from the symbol table capacity.
  Register mask = scratch2;
  __ LoadP(mask, FieldMemOperand(symbol_table, SymbolTable::kCapacityOffset));
  __ SmiUntag(mask);
  __ Sub(mask, Operand(1));

  // Calculate untagged address of the first element of the symbol table.
  Register first_symbol_table_element = symbol_table;
  __ LoadRR(first_symbol_table_element, symbol_table);
  __ AddP(first_symbol_table_element,
         Operand(SymbolTable::kElementsStartOffset - kHeapObjectTag));

  // Registers
  // chars: two character string, char 1 in byte 0 and char 2 in byte 1.
  // hash:  hash of two character string
  // mask:  capacity mask
  // first_symbol_table_element: address of the first element of
  //                             the symbol table
  // undefined: the undefined object
  // scratch: -

  // Perform a number of probes in the symbol table.
  const int kProbes = 4;
  Label found_in_symbol_table;
  Label next_probe[kProbes];
  Register candidate = scratch5;  // Scratch register contains candidate.
  for (int i = 0; i < kProbes; i++) {
    // Calculate entry in symbol table.
    if (i > 0) {
      __ LoadRR(candidate, hash);
      __ AddP(candidate, Operand(SymbolTable::GetProbeOffset(i)));
    } else {
      __ LoadRR(candidate, hash);
    }

    __ AndP(candidate, mask);

    // Load the entry from the symble table.
    STATIC_ASSERT(SymbolTable::kEntrySize == 1);
    __ ShiftLeftImm(scratch, candidate, Operand(kPointerSizeLog2));
    __ LoadP(candidate, MemOperand(scratch, first_symbol_table_element));

    // If entry is undefined no string with this hash can be found.
    Label is_string;
    __ CompareObjectType(candidate, scratch, scratch, ODDBALL_TYPE);
    __ bne(&is_string);

    __ CmpRR(undefined, candidate);
    __ beq(not_found);
    // Must be the hole (deleted entry).
    if (FLAG_debug_code) {
      __ LoadRoot(ip, Heap::kTheHoleValueRootIndex);
      __ CmpRR(ip, candidate);
      __ Assert(eq, "oddball in symbol table is not undefined or the hole");
    }
    __ b(&next_probe[i]);

    __ bind(&is_string);

    // Check that the candidate is a non-external ASCII string.  The instance
    // type is still in the scratch register from the CompareObjectType
    // operation.
    __ JumpIfInstanceTypeIsNotSequentialAscii(scratch, scratch, &next_probe[i]);

    // If length is not 2 the string is not a candidate.
    __ LoadP(scratch, FieldMemOperand(candidate, String::kLengthOffset));
    __ CmpSmiLiteral(scratch, Smi::FromInt(2), r0_p);
    __ bne(&next_probe[i]);

    // Check if the two characters match.
    __ lhz(scratch, FieldMemOperand(candidate, SeqAsciiString::kHeaderSize));
    __ CmpRR(chars, scratch);
    __ beq(&found_in_symbol_table);
    __ bind(&next_probe[i]);
  }

  // No matching 2 character string found by probing.
  __ b(not_found);

  // Scratch register contains result when we fall through to here.
  Register result = candidate;
  __ bind(&found_in_symbol_table);
  __ LoadRR(r3_p, result);
}


void StringHelper::GenerateHashInit(MacroAssembler* masm,
                                    Register hash,
                                    Register character,
                                    Register scratch) {
  // hash = character + (character << 10);
  __ LoadRoot(hash, Heap::kHashSeedRootIndex);
  // Untag smi seed and add the character.
  __ SmiUntag(scratch, hash);
  __ LoadRR(hash, character);
  __ AddP(hash, scratch);
  // hash += hash << 10;
  __ slwi(scratch, hash, Operand(10));
  __ AddP(hash, scratch);
  // hash ^= hash >> 6;
  __ srwi(scratch, hash, Operand(6));
  __ XorP(hash, scratch);
}


void StringHelper::GenerateHashAddCharacter(MacroAssembler* masm,
                                            Register hash,
                                            Register character,
                                            Register scratch) {
  // hash += character;
  __ AddP(hash, character);
  // hash += hash << 10;
  __ slwi(scratch, hash, Operand(10));
  __ AddP(hash, scratch);
  // hash ^= hash >> 6;
  __ srwi(scratch, hash, Operand(6));
  __ XorP(hash, scratch);
}


void StringHelper::GenerateHashGetHash(MacroAssembler* masm,
                                       Register hash,
                                       Register scratch) {
  // hash += hash << 3;
  __ slwi(scratch, hash, Operand(3));
  __ AddP(hash, scratch);
  // hash ^= hash >> 11;
  __ srwi(scratch, hash, Operand(11));
  __ XorP(hash, scratch);
  // hash += hash << 15;
  __ slwi(scratch, hash, Operand(15));
  __ AddP(hash, scratch);

  __ mov(scratch, Operand(String::kHashBitMask));
  __ AndP(hash, scratch/*, SetRC*/);  // Should be okay to remove RC

  // if (hash == 0) hash = 27;
  Label done;
  __ bne(&done /*, cr0*/);
  __ LoadImmP(hash, Operand(StringHasher::kZeroHash));
  __ bind(&done);
}


void SubStringStub::Generate(MacroAssembler* masm) {
  Label runtime;

  // Stack frame on entry.
  //  lr: return address
  //  sp[0]: to
  //  sp[4]: from
  //  sp[8]: string

  // This stub is called from the native-call %_SubString(...), so
  // nothing can be assumed about the arguments. It is tested that:
  //  "string" is a sequential string,
  //  both "from" and "to" are smis, and
  //  0 <= from <= to <= string.length.
  // If any of these assumptions fail, we call the runtime system.

  const int kToOffset = 0 * kPointerSize;
  const int kFromOffset = 1 * kPointerSize;
  const int kStringOffset = 2 * kPointerSize;

  __ LoadP(r5_p, MemOperand(sp, kToOffset));
  __ LoadP(r6_p, MemOperand(sp, kFromOffset));

  // If either to or from had the smi tag bit set, then fail to generic runtime
  __ JumpIfNotSmi(r5_p, &runtime);
  __ JumpIfNotSmi(r6_p, &runtime);
  __ SmiUntag(r5_p);
  __ SmiUntag(r6_p, SetRC);
  // Both r5_p and r6_p are untagged integers.

  // We want to bailout to runtime here if From is negative.
  __ blt(&runtime /*, cr0*/);  // From < 0.

  __ Cmpl(r6_p, r5_p);
  __ bgt(&runtime);  // Fail if from > to.
  __ Sub(r5_p, r5_p, r6_p);

  // Make sure first argument is a string.
  __ LoadP(r3_p, MemOperand(sp, kStringOffset));
  __ JumpIfSmi(r3_p, &runtime);
  Condition is_string = masm->IsObjectStringType(r3_p, r4_p);
  __ b(NegateCondition(is_string), &runtime /*, cr0*/);

  // Short-cut for the case of trivial substring.
  Label return_r3;
  // r3_p: original string
  // r5_p: result string length
  __ LoadP(r7_p, FieldMemOperand(r3_p, String::kLengthOffset));
  __ SmiUntag(r0_p, r7_p);
  __ Cmpl(r5_p, r0_p);
  // Return original string.
  __ beq(&return_r3);
  // Longer than original string's length or negative: unsafe arguments.
  __ bgt(&runtime);
  // Shorter than original string's length: an actual substring.

  // Deal with different string types: update the index if necessary
  // and put the underlying string into r8_p.
  // r3_p: original string
  // r4_p: instance type
  // r5_p: length
  // r6_p: from index (untagged)
  Label underlying_unpacked, sliced_string, seq_or_external_string;
  // If the string is not indirect, it can only be sequential or external.
  STATIC_ASSERT(kIsIndirectStringMask == (kSlicedStringTag & kConsStringTag));
  STATIC_ASSERT(kIsIndirectStringMask != 0);
  __ LoadRR(r0_p, r4_p);
  __ AndP(r0_p, Operand(kIsIndirectStringMask));
  __ beq(&seq_or_external_string /*, cr0*/);

  __ LoadRR(r0_p, r4_p);
  __ AndP(r0_p, Operand(kSlicedNotConsMask));
  __ bne(&sliced_string /*, cr0*/);
  // Cons string.  Check whether it is flat, then fetch first part.
  __ LoadP(r8_p, FieldMemOperand(r3_p, ConsString::kSecondOffset));
  __ CompareRoot(r8_p, Heap::kEmptyStringRootIndex);
  __ bne(&runtime);
  __ LoadP(r8_p, FieldMemOperand(r3_p, ConsString::kFirstOffset));
  // Update instance type.
  __ LoadP(r4_p, FieldMemOperand(r8_p, HeapObject::kMapOffset));
  __ LoadlB(r4_p, FieldMemOperand(r4_p, Map::kInstanceTypeOffset));
  __ b(&underlying_unpacked);

  __ bind(&sliced_string);
  // Sliced string.  Fetch parent and correct start index by offset.
  __ LoadP(r8_p, FieldMemOperand(r3_p, SlicedString::kParentOffset));
  __ LoadP(r7_p, FieldMemOperand(r3_p, SlicedString::kOffsetOffset));
  __ SmiUntag(r4_p, r7_p);
  __ AddP(r6_p, r4_p);  // Add offset to index.
  // Update instance type.
  __ LoadP(r4_p, FieldMemOperand(r8_p, HeapObject::kMapOffset));
  __ LoadlB(r4_p, FieldMemOperand(r4_p, Map::kInstanceTypeOffset));
  __ b(&underlying_unpacked);

  __ bind(&seq_or_external_string);
  // Sequential or external string.  Just move string to the expected register.
  __ LoadRR(r8_p, r3_p);

  __ bind(&underlying_unpacked);

  if (FLAG_string_slices) {
    Label copy_routine;
    // r8_p: underlying subject string
    // r4_p: instance type of underlying subject string
    // r5_p: length
    // r6_p: adjusted start index (untagged)
    __ Cmpi(r5_p, Operand(SlicedString::kMinLength));
    // Short slice.  Copy instead of slicing.
    __ blt(&copy_routine);
    // Allocate new sliced string.  At this point we do not reload the instance
    // type including the string encoding because we simply rely on the info
    // provided by the original string.  It does not matter if the original
    // string's encoding is wrong because we always have to recheck encoding of
    // the newly created string's parent anyways due to externalized strings.
    Label two_byte_slice, set_slice_header;
    STATIC_ASSERT((kStringEncodingMask & kAsciiStringTag) != 0);
    STATIC_ASSERT((kStringEncodingMask & kTwoByteStringTag) == 0);
    __ LoadRR(r0_p, r4_p);
    __ AndP(r0_p, Operand(kStringEncodingMask));
    __ beq(&two_byte_slice /*, cr0*/);
    __ AllocateAsciiSlicedString(r3_p, r5_p, r9_p, r10_p, &runtime);
    __ b(&set_slice_header);
    __ bind(&two_byte_slice);
    __ AllocateTwoByteSlicedString(r3_p, r5_p, r9_p, r10_p, &runtime);
    __ bind(&set_slice_header);
    __ SmiTag(r6_p);
    __ StoreP(r8_p, FieldMemOperand(r3_p, SlicedString::kParentOffset));
    __ StoreP(r6_p, FieldMemOperand(r3_p, SlicedString::kOffsetOffset));
    __ b(&return_r3);

    __ bind(&copy_routine);
  }

  // r8_p: underlying subject string
  // r4_p: instance type of underlying subject string
  // r5_p: length
  // r6_p: adjusted start index (untagged)
  Label two_byte_sequential, sequential_string, allocate_result;
  STATIC_ASSERT(kExternalStringTag != 0);
  STATIC_ASSERT(kSeqStringTag == 0);
  __ LoadRR(r0_p, r4_p);
  __ AndP(r0_p, Operand(kExternalStringTag));
  __ beq(&sequential_string /*, cr0*/);

  // Handle external string.
  // Rule out short external strings.
  STATIC_CHECK(kShortExternalStringTag != 0);
  __ LoadRR(r0_p, r4_p);
  __ AndP(r0_p, Operand(kShortExternalStringTag));
  __ bne(&runtime /*, cr0*/);
  __ LoadP(r8_p, FieldMemOperand(r8_p, ExternalString::kResourceDataOffset));
  // r8_p already points to the first character of underlying string.
  __ b(&allocate_result);

  __ bind(&sequential_string);
  // Locate first character of underlying subject string.
  STATIC_ASSERT(SeqTwoByteString::kHeaderSize == SeqAsciiString::kHeaderSize);
  __ AddP(r8_p, Operand(SeqAsciiString::kHeaderSize - kHeapObjectTag));

  __ bind(&allocate_result);
  // Sequential acii string.  Allocate the result.
  STATIC_ASSERT((kAsciiStringTag & kStringEncodingMask) != 0);
  __ LoadRR(r0_p, r4_p);
  __ AndP(r0_p, Operand(kStringEncodingMask));
  __ beq(&two_byte_sequential /*, cr0*/);

  // Allocate and copy the resulting ASCII string.
  __ AllocateAsciiString(r3_p, r5_p, r7_p, r9_p, r10_p, &runtime);

  // Locate first character of substring to copy.
  __ AddP(r8_p, r6_p);
  // Locate first character of result.
  __ LoadRR(r4_p, r3_p);
  __ AddP(r4_p, Operand(SeqAsciiString::kHeaderSize - kHeapObjectTag));

  // r3_p: result string
  // r4_p: first character of result string
  // r5_p: result string length
  // r8_p: first character of substring to copy
  STATIC_ASSERT((SeqAsciiString::kHeaderSize & kObjectAlignmentMask) == 0);
  StringHelper::GenerateCopyCharactersLong(masm, r4_p, r8_p, r5_p, r6_p, r7_p,
      r9_p, r10_p, r22_p, COPY_ASCII | DEST_ALWAYS_ALIGNED);
  __ b(&return_r3);

  // Allocate and copy the resulting two-byte string.
  __ bind(&two_byte_sequential);
  __ AllocateTwoByteString(r3_p, r5_p, r7_p, r9_p, r10_p, &runtime);

  // Locate first character of substring to copy.
  __ ShiftLeftImm(r4_p, r6_p, Operand(1));
  __ AddP(r8_p, r4_p);
  // Locate first character of result.
  __ LoadRR(r4_p, r3_p);
  __ AddP(r4_p, Operand(SeqTwoByteString::kHeaderSize - kHeapObjectTag));

  // r3_p: result string.
  // r4_p: first character of result.
  // r5_p: result length.
  // r8_p: first character of substring to copy.
  STATIC_ASSERT((SeqTwoByteString::kHeaderSize & kObjectAlignmentMask) == 0);
  StringHelper::GenerateCopyCharactersLong(
      masm, r4_p, r8_p, r5_p, r6_p, r7_p, r9_p, r10_p, r22_p,
      DEST_ALWAYS_ALIGNED);

  __ bind(&return_r3);
  Counters* counters = masm->isolate()->counters();
  __ IncrementCounter(counters->sub_string_native(), 1, r6_p, r7_p);
  __ AddP(sp, Operand(3 * kPointerSize));
  __ Ret();

  // Just jump to runtime to create the sub string.
  __ bind(&runtime);
  __ TailCallRuntime(Runtime::kSubString, 3, 1);
}


void StringCompareStub::GenerateFlatAsciiStringEquals(MacroAssembler* masm,
                                                      Register left,
                                                      Register right,
                                                      Register scratch1,
                                                      Register scratch2) {
  Register length = scratch1;

  // Compare lengths.
  Label strings_not_equal, check_zero_length;
  __ LoadP(length, FieldMemOperand(left, String::kLengthOffset));
  __ LoadP(scratch2, FieldMemOperand(right, String::kLengthOffset));
  __ CmpRR(length, scratch2);
  __ beq(&check_zero_length);
  __ bind(&strings_not_equal);
  __ LoadSmiLiteral(r3_p, Smi::FromInt(NOT_EQUAL));
  __ Ret();

  // Check if the length is zero.
  Label compare_chars;
  __ bind(&check_zero_length);
  STATIC_ASSERT(kSmiTag == 0);
  __ Cmpi(length, Operand::Zero());
  __ bne(&compare_chars);
  __ LoadSmiLiteral(r3_p, Smi::FromInt(EQUAL));
  __ Ret();

  // Compare characters.
  __ bind(&compare_chars);
  GenerateAsciiCharsCompareLoop(masm,
                                left, right, length, scratch2,
                                &strings_not_equal);

  // Characters are equal.
  __ LoadSmiLiteral(r3_p, Smi::FromInt(EQUAL));
  __ Ret();
}


void StringCompareStub::GenerateCompareFlatAsciiStrings(MacroAssembler* masm,
                                                        Register left,
                                                        Register right,
                                                        Register scratch1,
                                                        Register scratch2,
                                                        Register scratch3) {
  Label skip, result_not_equal, compare_lengths;
  // Find minimum length and length difference.
  __ LoadP(scratch1, FieldMemOperand(left, String::kLengthOffset));
  __ LoadP(scratch2, FieldMemOperand(right, String::kLengthOffset));
  __ Sub(scratch3, scratch1, scratch2/*, LeaveOE, SetRC*/);
  // Removing RC looks okay here.
  Register length_delta = scratch3;
  __ ble(&skip /*, cr0*/);
  __ LoadRR(scratch1, scratch2);
  __ bind(&skip);
  Register min_length = scratch1;
  STATIC_ASSERT(kSmiTag == 0);
  __ Cmpi(min_length, Operand::Zero());
  __ beq(&compare_lengths);

  // Compare loop.
  GenerateAsciiCharsCompareLoop(masm,
                                left, right, min_length, scratch2,
                                &result_not_equal);

  // Compare lengths - strings up to min-length are equal.
  __ bind(&compare_lengths);
  ASSERT(Smi::FromInt(EQUAL) == static_cast<Smi*>(0));
  // Use length_delta as result if it's zero.
  __ LoadRR(r3_p, length_delta);
  __ Cmpi(r3_p, Operand::Zero());
  __ bind(&result_not_equal);
  // Conditionally update the result based either on length_delta or
  // the last comparion performed in the loop above.
  Label less_equal, equal;
  __ ble(&less_equal);
  __ LoadSmiLiteral(r3_p, Smi::FromInt(GREATER));
  __ Ret();
  __ bind(&less_equal);
  __ beq(&equal);
  __ LoadSmiLiteral(r3_p, Smi::FromInt(LESS));
  __ bind(&equal);
  __ Ret();
}


void StringCompareStub::GenerateAsciiCharsCompareLoop(
    MacroAssembler* masm,
    Register left,
    Register right,
    Register length,
    Register scratch1,
    Label* chars_not_equal) {
  // Change index to run from -length to -1 by adding length to string
  // start. This means that loop ends when index reaches zero, which
  // doesn't need an additional compare.
  __ SmiUntag(length);
  __ LoadRR(scratch1, length);
  __ AddP(scratch1, Operand(SeqAsciiString::kHeaderSize - kHeapObjectTag));
  __ AddP(left, scratch1);
  __ AddP(right, scratch1);
  __ subfic(length, length, Operand::Zero());
  Register index = length;  // index = -length;

  // Compare loop.
  Label loop;
  __ bind(&loop);
  __ LoadlB(scratch1, MemOperand(left, index));
  __ LoadlB(r0_p, MemOperand(right, index));
  __ CmpRR(scratch1, r0_p);
  __ bne(chars_not_equal);
  __ AddP(index, Operand(1));
  __ Cmpi(index, Operand::Zero());
  __ bne(&loop);
}


void StringCompareStub::Generate(MacroAssembler* masm) {
  Label runtime;

  Counters* counters = masm->isolate()->counters();

  // Stack frame on entry.
  //  sp[0]: right string
  //  sp[4]: left string
  __ LoadP(r3_p, MemOperand(sp));  // Load right in r3_p, left in r4_p.
  __ LoadP(r4_p, MemOperand(sp, kPointerSize));

  Label not_same;
  __ CmpRR(r3_p, r4_p);
  __ bne(&not_same);
  STATIC_ASSERT(EQUAL == 0);
  STATIC_ASSERT(kSmiTag == 0);
  __ LoadSmiLiteral(r3_p, Smi::FromInt(EQUAL));
  __ IncrementCounter(counters->string_compare_native(), 1, r4_p, r5_p);
  __ AddP(sp, Operand(2 * kPointerSize));
  __ Ret();

  __ bind(&not_same);

  // Check that both objects are sequential ASCII strings.
  __ JumpIfNotBothSequentialAsciiStrings(r4_p, r3_p, r5_p, r6_p, &runtime);

  // Compare flat ASCII strings natively. Remove arguments from stack first.
  __ IncrementCounter(counters->string_compare_native(), 1, r5_p, r6_p);
  __ AddP(sp, Operand(2 * kPointerSize));
  GenerateCompareFlatAsciiStrings(masm, r4_p, r3_p, r5_p, r6_p, r7_p);

  // Call the runtime; it returns -1 (less), 0 (equal), or 1 (greater)
  // tagged as a small integer.
  __ bind(&runtime);
  __ TailCallRuntime(Runtime::kStringCompare, 2, 1);
}


void StringAddStub::Generate(MacroAssembler* masm) {
  Label call_runtime, call_builtin;
  Builtins::JavaScript builtin_id = Builtins::ADD;

  Counters* counters = masm->isolate()->counters();

  // Stack on entry:
  // sp[0]: second argument (right).
  // sp[4]: first argument (left).

  // Load the two arguments.
  __ LoadP(r3_p, MemOperand(sp, 1 * kPointerSize));  // First argument.
  __ LoadP(r4_p, MemOperand(sp, 0 * kPointerSize));  // Second argument.

  // Make sure that both arguments are strings if not known in advance.
  if (flags_ == NO_STRING_ADD_FLAGS) {
    __ JumpIfEitherSmi(r3_p, r4_p, &call_runtime);
    // Load instance types.
    __ LoadP(r7_p, FieldMemOperand(r3_p, HeapObject::kMapOffset));
    __ LoadP(r8_p, FieldMemOperand(r4_p, HeapObject::kMapOffset));
    __ LoadlB(r7_p, FieldMemOperand(r7_p, Map::kInstanceTypeOffset));
    __ LoadlB(r8_p, FieldMemOperand(r8_p, Map::kInstanceTypeOffset));
    STATIC_ASSERT(kStringTag == 0);
    // If either is not a string, go to runtime.
    __ LoadRR(r0_p, r7_p);
    __ AndP(r0_p, Operand(kIsNotStringMask));
    __ bne(&call_runtime /*, cr0*/);
    __ LoadRR(r0_p, r8_p);
    __ AndP(r0_p, Operand(kIsNotStringMask));
    __ bne(&call_runtime /*, cr0*/);
  } else {
    // Here at least one of the arguments is definitely a string.
    // We convert the one that is not known to be a string.
    if ((flags_ & NO_STRING_CHECK_LEFT_IN_STUB) == 0) {
      ASSERT((flags_ & NO_STRING_CHECK_RIGHT_IN_STUB) != 0);
      GenerateConvertArgument(
          masm, 1 * kPointerSize, r3_p, r5_p, r6_p, r7_p, r8_p, &call_builtin);
      builtin_id = Builtins::STRING_ADD_RIGHT;
    } else if ((flags_ & NO_STRING_CHECK_RIGHT_IN_STUB) == 0) {
      ASSERT((flags_ & NO_STRING_CHECK_LEFT_IN_STUB) != 0);
      GenerateConvertArgument(
          masm, 0 * kPointerSize, r4_p, r5_p, r6_p, r7_p, r8_p, &call_builtin);
      builtin_id = Builtins::STRING_ADD_LEFT;
    }
  }

  // Both arguments are strings.
  // r3_p: first string
  // r4_p: second string
  // r7_p: first string instance type (if flags_ == NO_STRING_ADD_FLAGS)
  // r8_p: second string instance type (if flags_ == NO_STRING_ADD_FLAGS)
  {
    Label first_not_empty, return_second, strings_not_empty;
    // Check if either of the strings are empty. In that case return the other.
    __ LoadP(r5_p, FieldMemOperand(r3_p, String::kLengthOffset));
    __ LoadP(r6_p, FieldMemOperand(r4_p, String::kLengthOffset));
    STATIC_ASSERT(kSmiTag == 0);
    // Test if first string is empty.
    __ CmpSmiLiteral(r5_p, Smi::FromInt(0), r0_p);
    __ bne(&first_not_empty);
    __ LoadRR(r3_p, r4_p);  // If first is empty, return second.
    __ b(&return_second);
    STATIC_ASSERT(kSmiTag == 0);
    __ bind(&first_not_empty);
     // Else test if second string is empty.
    __ CmpSmiLiteral(r6_p, Smi::FromInt(0), r0_p);
    __ bne(&strings_not_empty);  // If either string was empty, return r3_p.

    __ bind(&return_second);
    __ IncrementCounter(counters->string_add_native(), 1, r5_p, r6_p);
    __ AddP(sp, Operand(2 * kPointerSize));
    __ Ret();

    __ bind(&strings_not_empty);
  }

  __ SmiUntag(r5_p);
  __ SmiUntag(r6_p);
  // Both strings are non-empty.
  // r3_p: first string
  // r4_p: second string
  // r5_p: length of first string
  // r6_p: length of second string
  // r7_p: first string instance type (if flags_ == NO_STRING_ADD_FLAGS)
  // r8_p: second string instance type (if flags_ == NO_STRING_ADD_FLAGS)
  // Look at the length of the result of adding the two strings.
  Label string_add_flat_result, longer_than_two;
  // Adding two lengths can't overflow.
  STATIC_ASSERT(String::kMaxLength < String::kMaxLength * 2);
  __ LoadRR(r9_p, r5_p);
  __ AddP(r9_p, r6_p);
  // Use the symbol table when adding two one character strings, as it
  // helps later optimizations to return a symbol here.
  __ Cmpi(r9_p, Operand(2));
  __ bne(&longer_than_two);

  // Check that both strings are non-external ASCII strings.
  if (flags_ != NO_STRING_ADD_FLAGS) {
    __ LoadP(r7_p, FieldMemOperand(r3_p, HeapObject::kMapOffset));
    __ LoadP(r8_p, FieldMemOperand(r4_p, HeapObject::kMapOffset));
    __ LoadlB(r7_p, FieldMemOperand(r7_p, Map::kInstanceTypeOffset));
    __ LoadlB(r8_p, FieldMemOperand(r8_p, Map::kInstanceTypeOffset));
  }
  __ JumpIfBothInstanceTypesAreNotSequentialAscii(r7_p, r8_p, r9_p, r10_p,
                                                  &call_runtime);

  // Get the two characters forming the sub string.
  __ LoadlB(r5_p, FieldMemOperand(r3_p, SeqAsciiString::kHeaderSize));
  __ LoadlB(r6_p, FieldMemOperand(r4_p, SeqAsciiString::kHeaderSize));

  // Try to lookup two character string in symbol table. If it is not found
  // just allocate a new one.
  Label make_two_character_string;
  StringHelper::GenerateTwoCharacterSymbolTableProbe(
      masm, r5_p, r6_p, r9_p, r10_p, r7_p, r8_p, r22_p,
      &make_two_character_string);
  __ IncrementCounter(counters->string_add_native(), 1, r5_p, r6_p);
  __ AddP(sp, Operand(2 * kPointerSize));
  __ Ret();

  __ bind(&make_two_character_string);
  // Resulting string has length 2 and first chars of two strings
  // are combined into single halfword in r5_p register.
  // So we can fill resulting string without two loops by a single
  // halfword store instruction
  __ LoadImmP(r9_p, Operand(2));
  __ AllocateAsciiString(r3_p, r9_p, r7_p, r8_p, r22_p, &call_runtime);
  __ sth(r5_p, FieldMemOperand(r3_p, SeqAsciiString::kHeaderSize));
  __ IncrementCounter(counters->string_add_native(), 1, r5_p, r6_p);
  __ AddP(sp, Operand(2 * kPointerSize));
  __ Ret();

  __ bind(&longer_than_two);
  // Check if resulting string will be flat.
  __ Cmpi(r9_p, Operand(ConsString::kMinLength));
  __ blt(&string_add_flat_result);
  // Handle exceptionally long strings in the runtime system.
  STATIC_ASSERT((String::kMaxLength & 0x80000000) == 0);
  ASSERT(IsPowerOf2(String::kMaxLength + 1));
  // kMaxLength + 1 is representable as shifted literal, kMaxLength is not.
  __ mov(r10_p, Operand(String::kMaxLength + 1));
  __ Cmpl(r9_p, r10_p);
  __ bge(&call_runtime);

  // If result is not supposed to be flat, allocate a cons string object.
  // If both strings are ASCII the result is an ASCII cons string.
  if (flags_ != NO_STRING_ADD_FLAGS) {
    __ LoadP(r7_p, FieldMemOperand(r3_p, HeapObject::kMapOffset));
    __ LoadP(r8_p, FieldMemOperand(r4_p, HeapObject::kMapOffset));
    __ LoadlB(r7_p, FieldMemOperand(r7_p, Map::kInstanceTypeOffset));
    __ LoadlB(r8_p, FieldMemOperand(r8_p, Map::kInstanceTypeOffset));
  }
  Label non_ascii, allocated, ascii_data;
  STATIC_ASSERT(kTwoByteStringTag == 0);
  __ LoadRR(r0_p, r7_p);
  __ AndP(r0_p, Operand(kStringEncodingMask));
  __ beq(&non_ascii /*, cr0*/);
  __ LoadRR(r0_p, r8_p);
  __ AndP(r0_p, Operand(kStringEncodingMask));
  // r7_p: first instance type.
  // r8_p: second instance type.
  __ LoadRR(r0_p, r7_p);
  __ AndP(r0_p, Operand(kAsciiDataHintMask));
  __ bne(&ascii_data /*, cr0*/);
  __ LoadRR(r0_p, r8_p);
  __ AndP(r0_p, Operand(kAsciiDataHintMask));
  __ bne(&ascii_data /*, cr0*/);
  __ XorP(r7_p, r8_p);
  STATIC_ASSERT(kAsciiStringTag != 0 && kAsciiDataHintTag != 0);
  __ AndP(r7_p, Operand(kAsciiStringTag | kAsciiDataHintTag));
  __ Cmpi(r7_p, Operand(kAsciiStringTag | kAsciiDataHintTag));
  __ beq(&ascii_data);

  // Allocate a two byte cons string.
  __ AllocateTwoByteConsString(r10_p, r9_p, r7_p, r8_p, &call_runtime);
  __ b(&allocated);

  // We cannot encounter sliced strings or cons strings here since:
  STATIC_ASSERT(SlicedString::kMinLength >= ConsString::kMinLength);
  // Handle creating a flat result from either external or sequential strings.
  // Locate the first characters' locations.
  // r3_p: first string
  // r4_p: second string
  // r5_p: length of first string
  // r6_p: length of second string
  // r7_p: first string instance type (if flags_ == NO_STRING_ADD_FLAGS)
  // r8_p: second string instance type (if flags_ == NO_STRING_ADD_FLAGS)
  // r9_p: sum of lengths.
  Label first_prepared, second_prepared, external_string1, external_string2;
  __ bind(&string_add_flat_result);
  if (flags_ != NO_STRING_ADD_FLAGS) {
    __ LoadP(r7_p, FieldMemOperand(r3_p, HeapObject::kMapOffset));
    __ LoadP(r8_p, FieldMemOperand(r4_p, HeapObject::kMapOffset));
    __ LoadlB(r7_p, FieldMemOperand(r7_p, Map::kInstanceTypeOffset));
    __ LoadlB(r8_p, FieldMemOperand(r8_p, Map::kInstanceTypeOffset));
  }

  // Check whether both strings have same encoding
  __ LoadRR(r10_p, r8_p);
  __ XorP(r10_p, r7_p);
  __ LoadRR(r0_p, r10_p);
  __ AndP(r0_p, Operand(kStringEncodingMask));
  __ bne(&call_runtime /*, cr0*/);
  // TODO(JOHN): might be a problem here b/c addi didn't set RC

  STATIC_ASSERT(kSeqStringTag == 0);
  __ LoadRR(r0_p, r7_p);
  __ AndP(r0_p, Operand(kStringRepresentationMask));
  __ bne(&external_string1 /*, cr0*/);
  STATIC_ASSERT(SeqAsciiString::kHeaderSize == SeqTwoByteString::kHeaderSize);
  __ LoadRR(r10_p, r3_p);
  __ AddP(r10_p, Operand(SeqAsciiString::kHeaderSize - kHeapObjectTag));
  __ b(&first_prepared);
  // External string: rule out short external string and load string resource.
  STATIC_ASSERT(kShortExternalStringTag != 0);
  __ bind(&external_string1);
  __ LoadRR(r0_p, r7_p);
  __ AndP(r0_p, Operand(kShortExternalStringMask));
  __ bne(&call_runtime /*, cr0*/);
  __ LoadP(r10_p, FieldMemOperand(r3_p, ExternalString::kResourceDataOffset));
  __ bind(&first_prepared);

  STATIC_ASSERT(kSeqStringTag == 0);
  __ LoadRR(r0_p, r8_p);
  __ AndP(r0_p, Operand(kStringRepresentationMask));
  __ bne(&external_string2 /*, cr0*/);
  STATIC_ASSERT(SeqAsciiString::kHeaderSize == SeqTwoByteString::kHeaderSize);
  __ AddP(r4_p, Operand(SeqAsciiString::kHeaderSize - kHeapObjectTag));
  __ b(&second_prepared);
  // External string: rule out short external string and load string resource.
  STATIC_ASSERT(kShortExternalStringTag != 0);
  __ bind(&external_string2);
  __ LoadRR(r0_p, r8_p);
  __ AndP(r0_p, Operand(kShortExternalStringMask));
  __ bne(&call_runtime /*, cr0*/);
  __ LoadP(r4_p, FieldMemOperand(r4_p, ExternalString::kResourceDataOffset));
  __ bind(&second_prepared);

  Label non_ascii_string_add_flat_result;
  // r10_p: first character of first string
  // r4_p: first character of second string
  // r5_p: length of first string.
  // r6_p: length of second string.
  // r9_p: sum of lengths.
  // Both strings have the same encoding.
  STATIC_ASSERT(kTwoByteStringTag == 0);
  __ LoadRR(r0_p, r8_p);
  __ AndP(r0_p, Operand(kStringEncodingMask));
  __ beq(&non_ascii_string_add_flat_result /*, cr0*/);

  __ AllocateAsciiString(r3_p, r9_p, r7_p, r8_p, r22_p, &call_runtime);
  __ LoadRR(r9_p, r3_p);
  __ AddP(r9_p, Operand(SeqAsciiString::kHeaderSize - kHeapObjectTag));
  // r3_p: result string.
  // r10_p: first character of first string.
  // r4_p: first character of second string.
  // r5_p: length of first string.
  // r6_p: length of second string.
  // r9_p: first character of result.
  StringHelper::GenerateCopyCharacters(masm, r9_p, r10_p, r5_p, r7_p, true);
  // r9_p: next character of result.
  StringHelper::GenerateCopyCharacters(masm, r9_p, r4_p, r6_p, r7_p, true);
  __ IncrementCounter(counters->string_add_native(), 1, r5_p, r6_p);
  __ AddP(sp, Operand(2 * kPointerSize));
  __ Ret();

  __ bind(&non_ascii_string_add_flat_result);
  __ AllocateTwoByteString(r3_p, r9_p, r7_p, r8_p, r22_p, &call_runtime);
  __ LoadRR(r9_p, r3_p);
  __ AddP(r9_p, Operand(SeqTwoByteString::kHeaderSize - kHeapObjectTag));
  // r3_p: result string.
  // r10_p: first character of first string.
  // r4_p: first character of second string.
  // r5_p: length of first string.
  // r6_p: length of second string.
  // r9_p: first character of result.
  StringHelper::GenerateCopyCharacters(masm, r9_p, r10_p, r5_p, r7_p, false);
  // r9_p: next character of result.
  StringHelper::GenerateCopyCharacters(masm, r9_p, r4_p, r6_p, r7_p, false);
  __ IncrementCounter(counters->string_add_native(), 1, r5_p, r6_p);
  __ AddP(sp, Operand(2 * kPointerSize));
  __ Ret();

  // Just jump to runtime to add the two strings.
  __ bind(&call_runtime);
  __ TailCallRuntime(Runtime::kStringAdd, 2, 1);

  if (call_builtin.is_linked()) {
    __ bind(&call_builtin);
    __ InvokeBuiltin(builtin_id, JUMP_FUNCTION);
  }
}


void StringAddStub::GenerateConvertArgument(MacroAssembler* masm,
                                            int stack_offset,
                                            Register arg,
                                            Register scratch1,
                                            Register scratch2,
                                            Register scratch3,
                                            Register scratch4,
                                            Label* slow) {
  // First check if the argument is already a string.
  Label not_string, done;
  __ JumpIfSmi(arg, &not_string);
  __ CompareObjectType(arg, scratch1, scratch1, FIRST_NONSTRING_TYPE);
  __ blt(&done);

  // Check the number to string cache.
  Label not_cached;
  __ bind(&not_string);
  // Puts the cached result into scratch1.
  NumberToStringStub::GenerateLookupNumberStringCache(masm,
                                                      arg,
                                                      scratch1,
                                                      scratch2,
                                                      scratch3,
                                                      scratch4,
                                                      false,
                                                      &not_cached);
  __ LoadRR(arg, scratch1);
  __ StoreP(arg, MemOperand(sp, stack_offset));
  __ b(&done);

  // Check if the argument is a safe string wrapper.
  __ bind(&not_cached);
  __ JumpIfSmi(arg, slow);
  __ CompareObjectType(
      arg, scratch1, scratch2, JS_VALUE_TYPE);  // map -> scratch1.
  __ bne(slow);
  __ LoadlB(scratch2, FieldMemOperand(scratch1, Map::kBitField2Offset));
  __ AndP(scratch2,
          Operand(1 << Map::kStringWrapperSafeForDefaultValueOf));
  __ Cmpi(scratch2,
         Operand(1 << Map::kStringWrapperSafeForDefaultValueOf));
  __ bne(slow);
  __ LoadP(arg, FieldMemOperand(arg, JSValue::kValueOffset));
  __ StoreP(arg, MemOperand(sp, stack_offset));

  __ bind(&done);
}


void ICCompareStub::GenerateSmis(MacroAssembler* masm) {
  ASSERT(state_ == CompareIC::SMIS);
  Label miss;
  __ LoadRR(r5_p, r3_p);
  __ OrP(r5_p, r4_p);
  __ JumpIfNotSmi(r5_p, &miss);

  if (GetCondition() == eq) {
    // For equality we do not care about the sign of the result.
    // __ sub(r3_p, r3_p, r4_p, SetCC);
     __ Sub(r3_p, r3_p, r4_p);
  } else {
    // Untag before subtracting to avoid handling overflow.
    __ SmiUntag(r4_p);
    __ SmiUntag(r3_p);
    __ Sub(r3_p, r4_p, r3_p);
  }
  __ Ret();

  __ bind(&miss);
  GenerateMiss(masm);
}


void ICCompareStub::GenerateHeapNumbers(MacroAssembler* masm) {
  ASSERT(state_ == CompareIC::HEAP_NUMBERS);
  Label generic_stub;
  Label unordered, maybe_undefined1, maybe_undefined2;
  Label miss;
  Label equal, less_than;

  __ LoadRR(r5_p, r3_p);
  __ AndP(r5_p, r4_p);
  __ JumpIfSmi(r5_p, &generic_stub);

  __ CompareObjectType(r3_p, r5_p, r5_p, HEAP_NUMBER_TYPE);
  __ bne(&maybe_undefined1);
  __ CompareObjectType(r4_p, r5_p, r5_p, HEAP_NUMBER_TYPE);
  __ bne(&maybe_undefined2);

  // Inlining the double comparison and falling back to the general compare
  // stub if NaN is involved

  // Load left and right operand
  // likely we can combine the constants to remove the sub
  __ LoadF(d0, FieldMemOperand(r4_p, HeapNumber::kValueOffset));
  __ LoadF(d1, FieldMemOperand(r3_p, HeapNumber::kValueOffset));

  // Compare operands
  __ fcmpu(d0, d1);

  // Don't base result on status bits when a NaN is involved.
  __ bunordered(&unordered);

  // Return a result of -1, 0, or 1, based on status bits.
  __ beq(&equal);
  __ blt(&less_than);
  //  assume greater than
  __ LoadImmP(r3_p, Operand(GREATER));
  __ Ret();
  __ bind(&equal);
  __ LoadImmP(r3_p, Operand(EQUAL));
  __ Ret();
  __ bind(&less_than);
  __ LoadImmP(r3_p, Operand(LESS));
  __ Ret();

  __ bind(&unordered);

  CompareStub stub(GetCondition(), strict(), NO_COMPARE_FLAGS, r4_p, r3_p);
  __ bind(&generic_stub);
  __ Jump(stub.GetCode(), RelocInfo::CODE_TARGET);

  __ bind(&maybe_undefined1);
  if (Token::IsOrderedRelationalCompareOp(op_)) {
    __ CompareRoot(r3_p, Heap::kUndefinedValueRootIndex);
    __ bne(&miss);
    __ CompareObjectType(r4_p, r5_p, r5_p, HEAP_NUMBER_TYPE);
    __ bne(&maybe_undefined2);
    __ b(&unordered);
  }

  __ bind(&maybe_undefined2);
  if (Token::IsOrderedRelationalCompareOp(op_)) {
    __ CompareRoot(r4_p, Heap::kUndefinedValueRootIndex);
    __ beq(&unordered);
  }

  __ bind(&miss);
  GenerateMiss(masm);
}


void ICCompareStub::GenerateSymbols(MacroAssembler* masm) {
  ASSERT(state_ == CompareIC::SYMBOLS);
  Label miss, not_equal;

  // Registers containing left and right operands respectively.
  Register left = r4_p;
  Register right = r3_p;
  Register tmp1 = r5_p;
  Register tmp2 = r6_p;

  // Check that both operands are heap objects.
  __ JumpIfEitherSmi(left, right, &miss);

  // Check that both operands are symbols.
  __ LoadP(tmp1, FieldMemOperand(left, HeapObject::kMapOffset));
  __ LoadP(tmp2, FieldMemOperand(right, HeapObject::kMapOffset));
  __ LoadlB(tmp1, FieldMemOperand(tmp1, Map::kInstanceTypeOffset));
  __ LoadlB(tmp2, FieldMemOperand(tmp2, Map::kInstanceTypeOffset));
  STATIC_ASSERT(kSymbolTag != 0);
  __ AndP(tmp1, tmp2);
  __ LoadRR(r0_p, tmp1);
  __ AndP(r0_p, Operand(kIsSymbolMask));
  __ beq(&miss /*, cr0*/);

  // Symbols are compared by identity.
  __ CmpRR(left, right);
  __ bne(&not_equal);
  // Make sure r3_p is non-zero. At this point input operands are
  // guaranteed to be non-zero.
  ASSERT(right.is(r3_p));
  STATIC_ASSERT(EQUAL == 0);
  STATIC_ASSERT(kSmiTag == 0);
  __ LoadSmiLiteral(r3_p, Smi::FromInt(EQUAL));
  __ bind(&not_equal);
  __ Ret();

  __ bind(&miss);
  GenerateMiss(masm);
}


void ICCompareStub::GenerateStrings(MacroAssembler* masm) {
  ASSERT(state_ == CompareIC::STRINGS);
  Label miss, not_identical, is_symbol;

  bool equality = Token::IsEqualityOp(op_);

  // Registers containing left and right operands respectively.
  Register left = r4_p;
  Register right = r3_p;
  Register tmp1 = r5_p;
  Register tmp2 = r6_p;
  Register tmp3 = r7_p;
  Register tmp4 = r8_p;

  // Check that both operands are heap objects.
  __ JumpIfEitherSmi(left, right, &miss);

  // Check that both operands are strings. This leaves the instance
  // types loaded in tmp1 and tmp2.
  __ LoadP(tmp1, FieldMemOperand(left, HeapObject::kMapOffset));
  __ LoadP(tmp2, FieldMemOperand(right, HeapObject::kMapOffset));
  __ LoadlB(tmp1, FieldMemOperand(tmp1, Map::kInstanceTypeOffset));
  __ LoadlB(tmp2, FieldMemOperand(tmp2, Map::kInstanceTypeOffset));
  STATIC_ASSERT(kNotStringTag != 0);
  __ LoadRR(tmp3, tmp2);
  __ OrP(tmp3, tmp1);
  __ LoadRR(r0_p, tmp3);
  __ AndP(r0_p, Operand(kIsNotStringMask));
  __ bne(&miss /*, cr0*/);
  // TODO(JOHN): might be a problem b/c cr0 is not set

  // Fast check for identical strings.
  __ CmpRR(left, right);
  STATIC_ASSERT(EQUAL == 0);
  STATIC_ASSERT(kSmiTag == 0);
  __ bne(&not_identical);
  __ LoadSmiLiteral(r3_p, Smi::FromInt(EQUAL));
  __ Ret();
  __ bind(&not_identical);

  // Handle not identical strings.

  // Check that both strings are symbols. If they are, we're done
  // because we already know they are not identical.
  if (equality) {
    ASSERT(GetCondition() == eq);
    STATIC_ASSERT(kSymbolTag != 0);
    __ LoadRR(tmp3, tmp1);
    __ AndP(tmp3, tmp2);
    __ LoadRR(r0_p, tmp3);
    __ AndP(r0_p, Operand(kIsSymbolMask));
    __ beq(&is_symbol /*, cr0*/);
    // Make sure r3_p is non-zero. At this point input operands are
    // guaranteed to be non-zero.
    ASSERT(right.is(r3_p));
    __ Ret();
    __ bind(&is_symbol);
  }

  // Check that both strings are sequential ASCII.
  Label runtime;
  __ JumpIfBothInstanceTypesAreNotSequentialAscii(
      tmp1, tmp2, tmp3, tmp4, &runtime);

  // Compare flat ASCII strings. Returns when done.
  if (equality) {
    StringCompareStub::GenerateFlatAsciiStringEquals(
        masm, left, right, tmp1, tmp2);
  } else {
    StringCompareStub::GenerateCompareFlatAsciiStrings(
        masm, left, right, tmp1, tmp2, tmp3);
  }

  // Handle more complex cases in runtime.
  __ bind(&runtime);
  __ Push(left, right);
  if (equality) {
    __ TailCallRuntime(Runtime::kStringEquals, 2, 1);
  } else {
    __ TailCallRuntime(Runtime::kStringCompare, 2, 1);
  }

  __ bind(&miss);
  GenerateMiss(masm);
}


void ICCompareStub::GenerateObjects(MacroAssembler* masm) {
  ASSERT(state_ == CompareIC::OBJECTS);
  Label miss;
  __ LoadRR(r5_p, r3_p);
  __ AndP(r5_p, r4_p);
  __ JumpIfSmi(r5_p, &miss);

  __ CompareObjectType(r3_p, r5_p, r5_p, JS_OBJECT_TYPE);
  __ bne(&miss);
  __ CompareObjectType(r4_p, r5_p, r5_p, JS_OBJECT_TYPE);
  __ bne(&miss);

  ASSERT(GetCondition() == eq);
  __ Sub(r3_p, r3_p, r4_p);
  __ Ret();

  __ bind(&miss);
  GenerateMiss(masm);
}


void ICCompareStub::GenerateKnownObjects(MacroAssembler* masm) {
  Label miss;
  __ LoadRR(r5_p, r3_p);
  __ AndP(r5_p, r4_p);
  __ JumpIfSmi(r5_p, &miss);
  __ LoadP(r5_p, FieldMemOperand(r3_p, HeapObject::kMapOffset));
  __ LoadP(r6_p, FieldMemOperand(r4_p, HeapObject::kMapOffset));
  __ Cmpi(r5_p, Operand(known_map_));
  __ bne(&miss);
  __ Cmpi(r6_p, Operand(known_map_));
  __ bne(&miss);

  __ Sub(r3_p, r3_p, r4_p);
  __ Ret();

  __ bind(&miss);
  GenerateMiss(masm);
}



void ICCompareStub::GenerateMiss(MacroAssembler* masm) {
  {
    // Call the runtime system in a fresh internal frame.
    ExternalReference miss =
        ExternalReference(IC_Utility(IC::kCompareIC_Miss), masm->isolate());

    FrameScope scope(masm, StackFrame::INTERNAL);
    __ Push(r4_p, r3_p);
    __ mflr(r0_p);
    __ push(r0_p);
    __ Push(r4_p, r3_p);
    __ LoadSmiLiteral(ip, Smi::FromInt(op_));
    __ push(ip);
    __ CallExternalReference(miss, 3);
    // Compute the entry point of the rewritten stub.
    __ LoadRR(r5_p, r3_p);
    __ AddP(r5_p, Operand(Code::kHeaderSize - kHeapObjectTag));
    // Restore registers.
    __ pop(r0_p);
    __ mtlr(r0_p);
    __ pop(r3_p);
    __ pop(r4_p);
  }

  __ Jump(r5_p);
}

// This stub is paired with DirectCEntryStub::GenerateCall
void DirectCEntryStub::Generate(MacroAssembler* masm) {
  // Retrieve return address
  __ LoadP(r0_p, MemOperand(sp, kStackFrameExtraParamSlot * kPointerSize));
  __ Jump(r0_p);
}


void DirectCEntryStub::GenerateCall(MacroAssembler* masm,
                                    ExternalReference function) {
  __ mov(r6_p, Operand(function));
  GenerateCall(masm, r6_p);
}


void DirectCEntryStub::GenerateCall(MacroAssembler* masm,
                                    Register target) {
#if !defined(USE_SIMULATOR) && \
  (defined(_AIX) || defined(V8_TARGET_ARCH_S390X))
  // Native AIX/PPC64 Linux use a function descriptor.
  __ LoadP(ToRegister(2), MemOperand(target, kPointerSize));  // TOC
  __ LoadP(target, MemOperand(target, 0));  // Instruction address
#endif

  __ mov(r0_p, Operand(reinterpret_cast<intptr_t>(GetCode().location()),
                     RelocInfo::CODE_TARGET));

  // Block the trampoline pool through the whole function to make sure the
  // number of generated instructions is constant.
  Assembler::BlockTrampolinePoolScope block_trampoline_pool(masm);

  // Push return address (accessible to GC through exit frame pc).
  Label start, here;
  __ bind(&start);
  __ b(&here /*, SetLK*/);
  __ bind(&here);
  __ mflr(ip);
  __ mtlr(r0_p);  // from above, so we know where to return
  __ AddP(ip, Operand(6 * Assembler::kInstrSize));
  __ StoreP(ip, MemOperand(sp, kStackFrameExtraParamSlot * kPointerSize));
  __ Jump(target);  // Call the C++ function.
  ASSERT_EQ(Assembler::kInstrSize +
            (6 * Assembler::kInstrSize),
            masm->SizeOfCodeGeneratedSince(&start));
}


void StringDictionaryLookupStub::GenerateNegativeLookup(MacroAssembler* masm,
                                                        Label* miss,
                                                        Label* done,
                                                        Register receiver,
                                                        Register properties,
                                                        Handle<String> name,
                                                        Register scratch0) {
  // If names of slots in range from 1 to kProbes - 1 for the hash value are
  // not equal to the name and kProbes-th slot is not used (its name is the
  // undefined value), it guarantees the hash table doesn't contain the
  // property. It's true even if some slots represent deleted properties
  // (their names are the hole value).
  for (int i = 0; i < kInlinedProbes; i++) {
    // scratch0 points to properties hash.
    // Compute the masked index: (hash + i + i * i) & mask.
    Register index = scratch0;
    // Capacity is smi 2^n.
    __ LoadP(index, FieldMemOperand(properties, kCapacityOffset));
    __ Sub(index, Operand(1));
    __ LoadSmiLiteral(ip, Smi::FromInt(name->Hash() +
                                       StringDictionary::GetProbeOffset(i)));
    __ AndP(index, ip);

    // Scale the index by multiplying by the entry size.
    ASSERT(StringDictionary::kEntrySize == 3);
    __ ShiftLeftImm(ip, index, Operand(1));
    __ AddP(index, ip);  // index *= 3.

    Register entity_name = scratch0;
    // Having undefined at this place means the name is not contained.
    Register tmp = properties;
    __ SmiToPtrArrayOffset(ip, index);
    __ LoadRR(tmp, properties);
    __ AddP(tmp, ip);
    __ LoadP(entity_name, FieldMemOperand(tmp, kElementsStartOffset));

    ASSERT(!tmp.is(entity_name));
    __ LoadRoot(tmp, Heap::kUndefinedValueRootIndex);
    __ CmpRR(entity_name, tmp);
    __ beq(done);

    if (i != kInlinedProbes - 1) {
      // Load the hole ready for use below:
      __ LoadRoot(tmp, Heap::kTheHoleValueRootIndex);

      // Stop if found the property.
      __ Cmpi(entity_name, Operand(Handle<String>(name)));
      __ beq(miss);

      Label the_hole;
      __ CmpRR(entity_name, tmp);
      __ beq(&the_hole);

      // Check if the entry name is not a symbol.
      __ LoadP(entity_name, FieldMemOperand(entity_name,
                                            HeapObject::kMapOffset));
      __ LoadlB(entity_name,
              FieldMemOperand(entity_name, Map::kInstanceTypeOffset));
      __ LoadRR(r0_p, entity_name);
      __ AndP(r0_p, Operand(kIsSymbolMask));
      __ beq(miss /*, cr0*/);

      __ bind(&the_hole);

      // Restore the properties.
      __ LoadP(properties,
             FieldMemOperand(receiver, JSObject::kPropertiesOffset));
    }
  }

  const int spill_mask =
      (r0_p.bit() | r9_p.bit() | r8_p.bit() | r7_p.bit() | r6_p.bit() |
       r5_p.bit() | r4_p.bit() | r3_p.bit());

  __ mflr(r0_p);
  __ MultiPush(spill_mask);

  __ LoadP(r3_p, FieldMemOperand(receiver, JSObject::kPropertiesOffset));
  __ mov(r4_p, Operand(Handle<String>(name)));
  StringDictionaryLookupStub stub(NEGATIVE_LOOKUP);
  __ CallStub(&stub);
  __ Cmpi(r3_p, Operand::Zero());

  __ MultiPop(spill_mask);  // MultiPop does not touch condition flags
  __ mtlr(r0_p);

  __ beq(done);
  __ bne(miss);
}


// Probe the string dictionary in the |elements| register. Jump to the
// |done| label if a property with the given name is found. Jump to
// the |miss| label otherwise.
// If lookup was successful |scratch2| will be equal to elements + 4 * index.
void StringDictionaryLookupStub::GeneratePositiveLookup(MacroAssembler* masm,
                                                        Label* miss,
                                                        Label* done,
                                                        Register elements,
                                                        Register name,
                                                        Register scratch1,
                                                        Register scratch2) {
  ASSERT(!elements.is(scratch1));
  ASSERT(!elements.is(scratch2));
  ASSERT(!name.is(scratch1));
  ASSERT(!name.is(scratch2));

  // Assert that name contains a string.
  __ AssertString(name);

  // Compute the capacity mask.
  __ LoadP(scratch1, FieldMemOperand(elements, kCapacityOffset));
  __ SmiUntag(scratch1);  // convert smi to int
  __ Sub(scratch1, Operand(1));

  // Generate an unrolled loop that performs a few probes before
  // giving up. Measurements done on Gmail indicate that 2 probes
  // cover ~93% of loads from dictionaries.
  for (int i = 0; i < kInlinedProbes; i++) {
    // Compute the masked index: (hash + i + i * i) & mask.
    __ LoadlW(scratch2, FieldMemOperand(name, String::kHashFieldOffset));
    if (i > 0) {
      // Add the probe offset (i + i * i) left shifted to avoid right shifting
      // the hash in a separate instruction. The value hash + i + i * i is right
      // shifted in the following and instruction.
      ASSERT(StringDictionary::GetProbeOffset(i) <
             1 << (32 - String::kHashFieldSlot));
      __ AddP(scratch2, Operand(
                  StringDictionary::GetProbeOffset(i) << String::kHashShift));
    }
    __ srwi(scratch2, scratch2, Operand(String::kHashShift));
    __ AndP(scratch2, scratch1);

    // Scale the index by multiplying by the element size.
    ASSERT(StringDictionary::kEntrySize == 3);
    // scratch2 = scratch2 * 3.
    __ ShiftLeftImm(ip, scratch2, Operand(1));
    __ AddP(scratch2, ip);

    // Check if the key is identical to the name.
    __ ShiftLeftImm(ip, scratch2, Operand(kPointerSizeLog2));
    __ LoadRR(scratch2, elements);
    __ AddP(scratch2, ip);
    __ LoadP(ip, FieldMemOperand(scratch2, kElementsStartOffset));
    __ CmpRR(name, ip);
    __ beq(done);
  }

  const int spill_mask =
      (r0_p.bit() | r9_p.bit() | r8_p.bit() | r7_p.bit() |
       r6_p.bit() | r5_p.bit() | r4_p.bit() | r3_p.bit()) &
      ~(scratch1.bit() | scratch2.bit());

  __ mflr(r0_p);
  __ MultiPush(spill_mask);
  if (name.is(r3_p)) {
    ASSERT(!elements.is(r4_p));
    __ LoadRR(r4_p, name);
    __ LoadRR(r3_p, elements);
  } else {
    __ LoadRR(r3_p, elements);
    __ LoadRR(r4_p, name);
  }
  StringDictionaryLookupStub stub(POSITIVE_LOOKUP);
  __ CallStub(&stub);
  __ Cmpi(r3_p, Operand::Zero());
  __ LoadRR(scratch2, r5_p);
  __ MultiPop(spill_mask);
  __ mtlr(r0_p);

  __ bne(done);
  __ beq(miss);
}


void StringDictionaryLookupStub::Generate(MacroAssembler* masm) {
  // This stub overrides SometimesSetsUpAFrame() to return false.  That means
  // we cannot call anything that could cause a GC from this stub.
  // Registers:
  //  result: StringDictionary to probe
  //  r4_p: key
  //  : StringDictionary to probe.
  //  index_: will hold an index of entry if lookup is successful.
  //          might alias with result_.
  // Returns:
  //  result_ is zero if lookup failed, non zero otherwise.

  Register result = r3_p;
  Register dictionary = r3_p;
  Register key = r4_p;
  Register index = r5_p;
  Register mask = r6_p;
  Register hash = r7_p;
  Register undefined = r8_p;
  Register entry_key = r9_p;
  Register scratch = r9_p;

  Label in_dictionary, maybe_in_dictionary, not_in_dictionary;

  __ LoadP(mask, FieldMemOperand(dictionary, kCapacityOffset));
  __ SmiUntag(mask);
  __ Sub(mask, Operand(1));

  __ LoadlW(hash, FieldMemOperand(key, String::kHashFieldOffset));

  __ LoadRoot(undefined, Heap::kUndefinedValueRootIndex);

  for (int i = kInlinedProbes; i < kTotalProbes; i++) {
    // Compute the masked index: (hash + i + i * i) & mask.
    // Capacity is smi 2^n.
    if (i > 0) {
      // Add the probe offset (i + i * i) left shifted to avoid right shifting
      // the hash in a separate instruction. The value hash + i + i * i is right
      // shifted in the following and instruction.
      ASSERT(StringDictionary::GetProbeOffset(i) <
             1 << (32 - String::kHashFieldSlot));
      __ LoadRR(index, hash);
      __ AddP(index, Operand(
                  StringDictionary::GetProbeOffset(i) << String::kHashShift));
    } else {
      __ LoadRR(index, hash);
    }
    __ srwi(r0_p, index, Operand(String::kHashShift));
    __ LoadRR(index, mask);
    __ AndP(index, r0_p);

    // Scale the index by multiplying by the entry size.
    ASSERT(StringDictionary::kEntrySize == 3);
    __ ShiftLeftImm(scratch, index, Operand(1));
    __ AddP(index, scratch);  // index *= 3.

    ASSERT_EQ(kSmiTagSize, 1);
    __ ShiftLeftImm(scratch, index, Operand(kPointerSizeLog2));
    __ LoadRR(index, dictionary);
    __ AddP(index, scratch);
    __ LoadP(entry_key, FieldMemOperand(index, kElementsStartOffset));

    // Having undefined at this place means the name is not contained.
    __ CmpRR(entry_key, undefined);
    __ beq(&not_in_dictionary);

    // Stop if found the property.
    __ CmpRR(entry_key, key);
    __ beq(&in_dictionary);

    if (i != kTotalProbes - 1 && mode_ == NEGATIVE_LOOKUP) {
      // Check if the entry name is not a symbol.
      __ LoadP(entry_key, FieldMemOperand(entry_key, HeapObject::kMapOffset));
      __ LoadlB(entry_key,
              FieldMemOperand(entry_key, Map::kInstanceTypeOffset));
      __ LoadRR(r0_p, entry_key);
      __ AndP(r0_p, Operand(kIsSymbolMask));
      __ beq(&maybe_in_dictionary /*, cr0*/);
    }
  }

  __ bind(&maybe_in_dictionary);
  // If we are doing negative lookup then probing failure should be
  // treated as a lookup success. For positive lookup probing failure
  // should be treated as lookup failure.
  if (mode_ == POSITIVE_LOOKUP) {
    __ LoadImmP(result, Operand::Zero());
    __ Ret();
  }

  __ bind(&in_dictionary);
  __ LoadImmP(result, Operand(1));
  __ Ret();

  __ bind(&not_in_dictionary);
  __ LoadImmP(result, Operand::Zero());
  __ Ret();
}


struct AheadOfTimeWriteBarrierStubList {
  Register object, value, address;
  RememberedSetAction action;
};

#define REG(Name) { kRegister_ ## Name ## _Code }

static const AheadOfTimeWriteBarrierStubList kAheadOfTime[] = {
  // Used in RegExpExecStub.
  { REG(r28), REG(r26), REG(r10), EMIT_REMEMBERED_SET },
  { REG(r28), REG(r5), REG(r10), EMIT_REMEMBERED_SET },
  // Used in CompileArrayPushCall.
  // Also used in StoreIC::GenerateNormal via GenerateDictionaryStore.
  // Also used in KeyedStoreIC::GenerateGeneric.
  { REG(r6), REG(r7), REG(r8), EMIT_REMEMBERED_SET },
  // Used in CompileStoreGlobal.
  { REG(r7), REG(r4), REG(r5), OMIT_REMEMBERED_SET },
  // Used in StoreStubCompiler::CompileStoreField via GenerateStoreField.
  { REG(r4), REG(r5), REG(r6), EMIT_REMEMBERED_SET },
  { REG(r6), REG(r5), REG(r4), EMIT_REMEMBERED_SET },
  // Used in KeyedStoreStubCompiler::CompileStoreField via GenerateStoreField.
  { REG(r5), REG(r4), REG(r6), EMIT_REMEMBERED_SET },
  { REG(r6), REG(r4), REG(r5), EMIT_REMEMBERED_SET },
  // KeyedStoreStubCompiler::GenerateStoreFastElement.
  { REG(r6), REG(r5), REG(r7), EMIT_REMEMBERED_SET },
  { REG(r5), REG(r6), REG(r7), EMIT_REMEMBERED_SET },
  // ElementsTransitionGenerator::GenerateMapChangeElementTransition
  // and ElementsTransitionGenerator::GenerateSmiToDouble
  // and ElementsTransitionGenerator::GenerateDoubleToObject
  { REG(r5), REG(r6), REG(r22), EMIT_REMEMBERED_SET },
  { REG(r5), REG(r6), REG(r22), OMIT_REMEMBERED_SET },
  // ElementsTransitionGenerator::GenerateDoubleToObject
  { REG(r9), REG(r5), REG(r3), EMIT_REMEMBERED_SET },
  { REG(r5), REG(r9), REG(r22), EMIT_REMEMBERED_SET },
  // StoreArrayLiteralElementStub::Generate
  { REG(r8), REG(r3), REG(r9), EMIT_REMEMBERED_SET },
  // FastNewClosureStub::Generate
  { REG(r5), REG(r7), REG(r4), EMIT_REMEMBERED_SET },
  // Null termination.
  { REG(no_reg), REG(no_reg), REG(no_reg), EMIT_REMEMBERED_SET}
};

#undef REG


bool RecordWriteStub::IsPregenerated() {
  for (const AheadOfTimeWriteBarrierStubList* entry = kAheadOfTime;
       !entry->object.is(no_reg);
       entry++) {
    if (object_.is(entry->object) &&
        value_.is(entry->value) &&
        address_.is(entry->address) &&
        remembered_set_action_ == entry->action &&
        save_fp_regs_mode_ == kDontSaveFPRegs) {
      return true;
    }
  }
  return false;
}


bool StoreBufferOverflowStub::IsPregenerated() {
  return save_doubles_ == kDontSaveFPRegs || ISOLATE->fp_stubs_generated();
}


void StoreBufferOverflowStub::GenerateFixedRegStubsAheadOfTime() {
  StoreBufferOverflowStub stub1(kDontSaveFPRegs);
  stub1.GetCode()->set_is_pregenerated(true);
}


void RecordWriteStub::GenerateFixedRegStubsAheadOfTime() {
  for (const AheadOfTimeWriteBarrierStubList* entry = kAheadOfTime;
       !entry->object.is(no_reg);
       entry++) {
    RecordWriteStub stub(entry->object,
                         entry->value,
                         entry->address,
                         entry->action,
                         kDontSaveFPRegs);
    stub.GetCode()->set_is_pregenerated(true);
  }
}


bool CodeStub::CanUseFPRegisters() {
  return true;
}


// Takes the input in 3 registers: address_ value_ and object_.  A pointer to
// the value has just been written into the object, now this stub makes sure
// we keep the GC informed.  The word in the object where the value has been
// written is in the address register.
void RecordWriteStub::Generate(MacroAssembler* masm) {
  Label skip_to_incremental_noncompacting;
  Label skip_to_incremental_compacting;
  const int crBit = Assembler::encode_crbit(cr2, CR_LT);

  // The first two branch instructions are generated with labels so as to
  // get the offset fixed up correctly by the bind(Label*) call.  We patch
  // it back and forth between branch condition True and False
  // when we start and stop incremental heap marking.
  // See RecordWriteStub::Patch for details.

  // Clear the bit, branch on True for NOP action initially
  __ crxor(crBit, crBit, crBit);
  __ blt(&skip_to_incremental_noncompacting /*, cr2*/);
  __ blt(&skip_to_incremental_compacting /*, cr2*/);

  if (remembered_set_action_ == EMIT_REMEMBERED_SET) {
    __ RememberedSetHelper(object_,
                           address_,
                           value_,
                           save_fp_regs_mode_,
                           MacroAssembler::kReturnAtEnd);
  }
  __ Ret();

  __ bind(&skip_to_incremental_noncompacting);
  GenerateIncremental(masm, INCREMENTAL);

  __ bind(&skip_to_incremental_compacting);
  GenerateIncremental(masm, INCREMENTAL_COMPACTION);

  // Initial mode of the stub is expected to be STORE_BUFFER_ONLY.
  // Will be checked in IncrementalMarking::ActivateGeneratedStub.
  // patching not required on PPC as the initial path is effectively NOP
}


void RecordWriteStub::GenerateIncremental(MacroAssembler* masm, Mode mode) {
  regs_.Save(masm);

  if (remembered_set_action_ == EMIT_REMEMBERED_SET) {
    Label dont_need_remembered_set;

    __ LoadP(regs_.scratch0(), MemOperand(regs_.address(), 0));
    __ JumpIfNotInNewSpace(regs_.scratch0(),  // Value.
                           regs_.scratch0(),
                           &dont_need_remembered_set);

    __ CheckPageFlag(regs_.object(),
                     regs_.scratch0(),
                     1 << MemoryChunk::SCAN_ON_SCAVENGE,
                     ne,
                     &dont_need_remembered_set);

    // First notify the incremental marker if necessary, then update the
    // remembered set.
    CheckNeedsToInformIncrementalMarker(
        masm, kUpdateRememberedSetOnNoNeedToInformIncrementalMarker, mode);
    InformIncrementalMarker(masm, mode);
    regs_.Restore(masm);
    __ RememberedSetHelper(object_,
                           address_,
                           value_,
                           save_fp_regs_mode_,
                           MacroAssembler::kReturnAtEnd);

    __ bind(&dont_need_remembered_set);
  }

  CheckNeedsToInformIncrementalMarker(
      masm, kReturnOnNoNeedToInformIncrementalMarker, mode);
  InformIncrementalMarker(masm, mode);
  regs_.Restore(masm);
  __ Ret();
}


void RecordWriteStub::InformIncrementalMarker(MacroAssembler* masm, Mode mode) {
  regs_.SaveCallerSaveRegisters(masm, save_fp_regs_mode_);
  int argument_count = 3;
  __ PrepareCallCFunction(argument_count, regs_.scratch0());
  Register address =
      r3_p.is(regs_.address()) ? regs_.scratch0() : regs_.address();
  ASSERT(!address.is(regs_.object()));
  ASSERT(!address.is(r3_p));
  __ LoadRR(address, regs_.address());
  __ LoadRR(r3_p, regs_.object());
  if (mode == INCREMENTAL_COMPACTION) {
    __ LoadRR(r4_p, address);
  } else {
    ASSERT(mode == INCREMENTAL);
    __ LoadP(r4_p, MemOperand(address, 0));
  }
  __ mov(r5_p, Operand(ExternalReference::isolate_address()));

  AllowExternalCallThatCantCauseGC scope(masm);
  if (mode == INCREMENTAL_COMPACTION) {
    __ CallCFunction(
        ExternalReference::incremental_evacuation_record_write_function(
            masm->isolate()),
        argument_count);
  } else {
    ASSERT(mode == INCREMENTAL);
    __ CallCFunction(
        ExternalReference::incremental_marking_record_write_function(
            masm->isolate()),
        argument_count);
  }
  regs_.RestoreCallerSaveRegisters(masm, save_fp_regs_mode_);
}


void RecordWriteStub::CheckNeedsToInformIncrementalMarker(
    MacroAssembler* masm,
    OnNoNeedToInformIncrementalMarker on_no_need,
    Mode mode) {
  Label on_black;
  Label need_incremental;
  Label need_incremental_pop_scratch;

  ASSERT((~Page::kPageAlignmentMask & 0xffff) == 0);
  __ lis(r0_p, Operand((~Page::kPageAlignmentMask >> 16)));
  __ LoadRR(regs_.scratch0(), regs_.object());
  __ AndP(regs_.scratch0(), r0_p);
  __ LoadP(regs_.scratch1(),
         MemOperand(regs_.scratch0(),
                    MemoryChunk::kWriteBarrierCounterOffset));
  __ Sub(regs_.scratch1(), regs_.scratch1(), Operand(1));
  __ StoreP(regs_.scratch1(),
            MemOperand(regs_.scratch0(),
                       MemoryChunk::kWriteBarrierCounterOffset));
  __ Cmpi(regs_.scratch1(), Operand::Zero());  // PPC, we could do better here
  __ blt(&need_incremental);

  // Let's look at the color of the object:  If it is not black we don't have
  // to inform the incremental marker.
  __ JumpIfBlack(regs_.object(), regs_.scratch0(), regs_.scratch1(), &on_black);

  regs_.Restore(masm);
  if (on_no_need == kUpdateRememberedSetOnNoNeedToInformIncrementalMarker) {
    __ RememberedSetHelper(object_,
                           address_,
                           value_,
                           save_fp_regs_mode_,
                           MacroAssembler::kReturnAtEnd);
  } else {
    __ Ret();
  }

  __ bind(&on_black);

  // Get the value from the slot.
  __ LoadP(regs_.scratch0(), MemOperand(regs_.address(), 0));

  if (mode == INCREMENTAL_COMPACTION) {
    Label ensure_not_white;

    __ CheckPageFlag(regs_.scratch0(),  // Contains value.
                     regs_.scratch1(),  // Scratch.
                     MemoryChunk::kEvacuationCandidateMask,
                     eq,
                     &ensure_not_white);

    __ CheckPageFlag(regs_.object(),
                     regs_.scratch1(),  // Scratch.
                     MemoryChunk::kSkipEvacuationSlotsRecordingMask,
                     eq,
                     &need_incremental);

    __ bind(&ensure_not_white);
  }

  // We need extra registers for this, so we push the object and the address
  // register temporarily.
  __ Push(regs_.object(), regs_.address());
  __ EnsureNotWhite(regs_.scratch0(),  // The value.
                    regs_.scratch1(),  // Scratch.
                    regs_.object(),  // Scratch.
                    regs_.address(),  // Scratch.
                    &need_incremental_pop_scratch);
  __ Pop(regs_.object(), regs_.address());

  regs_.Restore(masm);
  if (on_no_need == kUpdateRememberedSetOnNoNeedToInformIncrementalMarker) {
    __ RememberedSetHelper(object_,
                           address_,
                           value_,
                           save_fp_regs_mode_,
                           MacroAssembler::kReturnAtEnd);
  } else {
    __ Ret();
  }

  __ bind(&need_incremental_pop_scratch);
  __ Pop(regs_.object(), regs_.address());

  __ bind(&need_incremental);

  // Fall through when we need to inform the incremental marker.
}


void StoreArrayLiteralElementStub::Generate(MacroAssembler* masm) {
  // ----------- S t a t e -------------
  //  -- r3_p    : element value to store
  //  -- r4_p    : array literal
  //  -- r5_p    : map of array literal
  //  -- r6_p    : element index as smi
  //  -- r7_p    : array literal index in function as smi
  // -----------------------------------

  Label element_done;
  Label double_elements;
  Label smi_element;
  Label slow_elements;
  Label fast_elements;

  __ CheckFastElements(r5_p, r8_p, &double_elements);
  // FAST_*_SMI_ELEMENTS or FAST_*_ELEMENTS
  __ JumpIfSmi(r3_p, &smi_element);
  __ CheckFastSmiElements(r5_p, r8_p, &fast_elements);

  // Store into the array literal requires a elements transition. Call into
  // the runtime.
  __ bind(&slow_elements);
  // call.
  __ Push(r4_p, r6_p, r3_p);
  __ LoadP(r8_p, MemOperand(fp, JavaScriptFrameConstants::kFunctionOffset));
  __ LoadP(r8_p, FieldMemOperand(r8_p, JSFunction::kLiteralsOffset));
  __ Push(r8_p, r7_p);
  __ TailCallRuntime(Runtime::kStoreArrayLiteralElement, 5, 1);

  // Array literal has ElementsKind of FAST_*_ELEMENTS and value is an object.
  __ bind(&fast_elements);
  __ LoadP(r8_p, FieldMemOperand(r4_p, JSObject::kElementsOffset));
  __ SmiToPtrArrayOffset(r9_p, r6_p);
  __ AddP(r9_p, r8_p);
#if V8_TARGET_ARCH_S390X
  // add due to offset alignment requirements of StorePU
  __ AddP(r9_p, Operand(FixedArray::kHeaderSize - kHeapObjectTag));
  __ StoreP(r3_p, MemOperand(r9_p));
#else
  __ StorePU(r3_p, MemOperand(r9_p, FixedArray::kHeaderSize - kHeapObjectTag));
#endif
  // Update the write barrier for the array store.
  __ RecordWrite(r8_p, r9_p, r3_p, kLRHasNotBeenSaved, kDontSaveFPRegs,
                 EMIT_REMEMBERED_SET, OMIT_SMI_CHECK);
  __ Ret();

  // Array literal has ElementsKind of FAST_*_SMI_ELEMENTS or FAST_*_ELEMENTS,
  // and value is Smi.
  __ bind(&smi_element);
  __ LoadP(r8_p, FieldMemOperand(r4_p, JSObject::kElementsOffset));
  __ SmiToPtrArrayOffset(r9_p, r6_p);
  __ AddP(r9_p, r8_p);
  __ StoreP(r3_p, FieldMemOperand(r9_p, FixedArray::kHeaderSize));
  __ Ret();

  // Array literal has ElementsKind of FAST_DOUBLE_ELEMENTS.
  __ bind(&double_elements);
  __ LoadP(r8_p, FieldMemOperand(r4_p, JSObject::kElementsOffset));
  __ StoreNumberToDoubleElements(r3_p, r6_p, r4_p,
                                 // Overwrites all regs after this.
                                 r8_p, r9_p, r10_p, r22_p, r5_p,
                                 &slow_elements);
  __ Ret();
}


void ProfileEntryHookStub::MaybeCallEntryHook(MacroAssembler* masm) {
  if (entry_hook_ != NULL) {
    ProfileEntryHookStub stub;
    __ mflr(r0_p);
    __ push(r0_p);
    __ CallStub(&stub);
    __ pop(r0_p);
    __ mtlr(r0_p);
  }
}


void ProfileEntryHookStub::Generate(MacroAssembler* masm) {
  // The entry hook is a "push lr" instruction, followed by a call.
  const int32_t kReturnAddressDistanceFromFunctionStart =
      Assembler::kCallTargetAddressOffset + 2 * Assembler::kInstrSize;

  // Save live volatile registers.
  __ mflr(r3_p);
  __ Push(r3_p, r30_p, r4_p);
  const int32_t kNumSavedRegs = 3;

  // Compute the function's address for the first argument.
  __ Sub(r3_p, Operand(kReturnAddressDistanceFromFunctionStart));

  // The caller's return address is above the saved temporaries.
  // Grab that for the second argument to the hook.
  __ LoadRR(r4_p, sp);
  __ AddP(r4_p, Operand(kNumSavedRegs * kPointerSize));

  // Align the stack if necessary.
  int frame_alignment = masm->ActivationFrameAlignment();
  if (frame_alignment > kPointerSize) {
    __ LoadRR(r30_p, sp);
    ASSERT(IsPowerOf2(frame_alignment));
    ASSERT(-frame_alignment == -8);
    __ ClearRightImm(sp, sp, Operand(3));
  }

#if !defined(USE_SIMULATOR)
  __ mov(ip, Operand(reinterpret_cast<intptr_t>(&entry_hook_)));
  __ LoadP(ip, MemOperand(ip));

#if (defined(_AIX) || defined(V8_TARGET_ARCH_S390X))
  // Function descriptor
  __ LoadP(ToRegister(2), MemOperand(ip, kPointerSize));
  __ LoadP(ip, MemOperand(ip, 0));
#endif

  // PPC LINUX ABI:
  __ AddP(sp, Operand(-kNumRequiredStackFrameSlots * kPointerSize));
#else
  // Under the simulator we need to indirect the entry hook through a
  // trampoline function at a known address.
  Address trampoline_address = reinterpret_cast<Address>(
      reinterpret_cast<intptr_t>(EntryHookTrampoline));
  ApiFunction dispatcher(trampoline_address);
  __ mov(ip, Operand(ExternalReference(&dispatcher,
                                       ExternalReference::BUILTIN_CALL,
                                       masm->isolate())));
#endif
  __ Call(ip);

// For the most part this is true only when USE_SIMULATOR is true
// The exception is when built with nativesim=true, then we need
// Real PPC calling support plus simulation
#if defined(V8_HOST_ARCH_S39064) || defined(V8_HOST_ARCH_S390)
  __ AddP(sp, Operand(kNumRequiredStackFrameSlots * kPointerSize));
#endif

  // Restore the stack pointer if needed.
  if (frame_alignment > kPointerSize) {
    __ LoadRR(sp, r30_p);
  }

  __ Pop(r0_p, r30_p, r4_p);
  __ mtlr(r0_p);
  __ Ret();
}

#undef __
} }  // namespace v8::internal

#endif  // V8_TARGET_ARCH_S390
