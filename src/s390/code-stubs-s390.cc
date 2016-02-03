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
  __ CmpP(scratch1, scratch2);
  __ bne(not_a_heap_number);
}


void ToNumberStub::Generate(MacroAssembler* masm) {
  // The ToNumber stub takes one argument in eax.
  Label check_heap_number, call_builtin;
  __ JumpIfNotSmi(r2, &check_heap_number);
  __ Ret();

  __ bind(&check_heap_number);
  EmitCheckForHeapNumber(masm, r2, r3, ip, &call_builtin);
  __ Ret();

  __ bind(&call_builtin);
  __ push(r2);
  __ InvokeBuiltin(Builtins::TO_NUMBER, JUMP_FUNCTION);
}


void FastNewClosureStub::Generate(MacroAssembler* masm) {
  // Create a new closure from the given function info in new
  // space. Set the context to the current context in cp.
  Counters* counters = masm->isolate()->counters();

  Label gc;

  // Pop the function info from the stack.
  __ pop(r5);

  // Attempt to allocate new JSFunction in new space.
  __ AllocateInNewSpace(JSFunction::kSize,
                        r2,
                        r3,
                        r4,
                        &gc,
                        TAG_OBJECT);

  __ IncrementCounter(counters->fast_new_closure_total(), 1, r8, r9);

  int map_index = (language_mode_ == CLASSIC_MODE)
      ? Context::FUNCTION_MAP_INDEX
      : Context::STRICT_MODE_FUNCTION_MAP_INDEX;

  // Compute the function map in the current native context and set that
  // as the map of the allocated object.
  __ LoadP(r4,
           MemOperand(cp, Context::SlotOffset(Context::GLOBAL_OBJECT_INDEX)));
  __ LoadP(r4, FieldMemOperand(r4, GlobalObject::kNativeContextOffset));
  __ LoadP(r7, MemOperand(r4, Context::SlotOffset(map_index)));
  __ StoreP(r7, FieldMemOperand(r2, HeapObject::kMapOffset));

  // Initialize the rest of the function. We don't have to update the
  // write barrier because the allocated object is in new space.
  __ LoadRoot(r3, Heap::kEmptyFixedArrayRootIndex);
  __ LoadRoot(r7, Heap::kTheHoleValueRootIndex);
  __ StoreP(r3, FieldMemOperand(r2, JSObject::kPropertiesOffset));
  __ StoreP(r3, FieldMemOperand(r2, JSObject::kElementsOffset));
  __ StoreP(r7, FieldMemOperand(r2,
            JSFunction::kPrototypeOrInitialMapOffset));
  __ StoreP(r5, FieldMemOperand(r2, JSFunction::kSharedFunctionInfoOffset));
  __ StoreP(cp, FieldMemOperand(r2, JSFunction::kContextOffset));
  __ StoreP(r3, FieldMemOperand(r2, JSFunction::kLiteralsOffset));

  // Initialize the code pointer in the function to be the one
  // found in the shared function info object.
  // But first check if there is an optimized version for our context.
  Label check_optimized;
  Label install_unoptimized;
  if (FLAG_cache_optimized_code) {
    __ LoadP(r3,
            FieldMemOperand(r5, SharedFunctionInfo::kOptimizedCodeMapOffset));
    __ CmpP(r3, Operand::Zero());
    __ bne(&check_optimized);
  }
  __ bind(&install_unoptimized);
  __ LoadRoot(r6, Heap::kUndefinedValueRootIndex);
  __ StoreP(r6, FieldMemOperand(r2, JSFunction::kNextFunctionLinkOffset));
  __ LoadP(r5, FieldMemOperand(r5, SharedFunctionInfo::kCodeOffset));
  __ AddP(r5, Operand(Code::kHeaderSize - kHeapObjectTag));
  __ StoreP(r5, FieldMemOperand(r2, JSFunction::kCodeEntryOffset));

  // Return result. The argument function info has been popped already.
  __ Ret();

  __ bind(&check_optimized);

  __ IncrementCounter(counters->fast_new_closure_try_optimized(), 1, r8,
                      r9);

  // r4 holds native context, r3 points to fixed array of 3-element entries
  // (native context, optimized code, literals).
  // The optimized code map must never be empty, so check the first elements.
  Label install_optimized;
  // Speculatively move code object into r6
  __ LoadP(r6, FieldMemOperand(r3, FixedArray::kHeaderSize + kPointerSize));
  __ LoadP(r7, FieldMemOperand(r3, FixedArray::kHeaderSize));
  __ CmpP(r4, r7);
  __ beq(&install_optimized);

  // Iterate through the rest of map backwards.  r6 holds an index as a Smi.
  Label loop;
  __ LoadP(r6, FieldMemOperand(r3, FixedArray::kLengthOffset));
  __ bind(&loop);
  // Do not double check first entry.

  __ CmpSmiLiteral(r6, Smi::FromInt(SharedFunctionInfo::kEntryLength), r0);
  __ beq(&install_unoptimized);
  // Skip an entry.
  __ SubSmiLiteral(r6, r6, Smi::FromInt(SharedFunctionInfo::kEntryLength),
                   r0);
  __ AddP(r7, r3, Operand(FixedArray::kHeaderSize - kHeapObjectTag));
  __ SmiToPtrArrayOffset(r8, r6);
  __ LoadP(r7, MemOperand(r7, r8));
  __ CmpP(r4, r7);
  __ bne(&loop);
  // Hit: fetch the optimized code.
  // TODO(penguin): potential to use x-form for this sequence
  __ AddP(r7, r3, Operand(FixedArray::kHeaderSize - kHeapObjectTag));
  __ SmiToPtrArrayOffset(r8, r6);
  __ AddP(r7, r8);
  __ LoadP(r6, MemOperand(r7, kPointerSize));
  __ lay(r7, MemOperand(r7, kPointerSize));

  __ bind(&install_optimized);
  __ IncrementCounter(counters->fast_new_closure_install_optimized(),
                      1, r8, r9);

  // TODO(fschneider): Idea: store proper code pointers in the map and either
  // unmangle them on marking or do nothing as the whole map is discarded on
  // major GC anyway.
  __ AddP(r6, Operand(Code::kHeaderSize - kHeapObjectTag));
  __ StoreP(r6, FieldMemOperand(r2, JSFunction::kCodeEntryOffset));

  // Now link a function into a list of optimized functions.
  __ LoadP(r6, ContextOperand(r4, Context::OPTIMIZED_FUNCTIONS_LIST));

  __ StoreP(r6, FieldMemOperand(r2, JSFunction::kNextFunctionLinkOffset));
  // No need for write barrier as JSFunction (eax) is in the new space.

  __ StoreP(r2, ContextOperand(r4, Context::OPTIMIZED_FUNCTIONS_LIST));
  // Store JSFunction (eax) into edx before issuing write barrier as
  // it clobbers all the registers passed.
  __ LoadRR(r6, r2);
  __ RecordWriteContextSlot(
      r4,
      Context::SlotOffset(Context::OPTIMIZED_FUNCTIONS_LIST),
      r6,
      r3,
      kLRHasNotBeenSaved,
      kDontSaveFPRegs);

  // Return result. The argument function info has been popped already.
  __ Ret();

  // Create a new closure through the slower runtime call.
  __ bind(&gc);
  __ LoadRoot(r6, Heap::kFalseValueRootIndex);
  __ Push(cp, r5, r6);
  __ TailCallRuntime(Runtime::kNewClosure, 3, 1);
}


void FastNewContextStub::Generate(MacroAssembler* masm) {
  // Try to allocate the context in new space.
  Label gc;
  int length = slots_ + Context::MIN_CONTEXT_SLOTS;

  // Attempt to allocate the context in new space.
  __ AllocateInNewSpace(FixedArray::SizeFor(length),
                        r2,
                        r3,
                        r4,
                        &gc,
                        TAG_OBJECT);

  // Load the function from the stack.
  __ LoadP(r5, MemOperand(sp, 0));

  // Set up the object header.
  __ LoadRoot(r3, Heap::kFunctionContextMapRootIndex);
  __ LoadSmiLiteral(r4, Smi::FromInt(length));
  __ StoreP(r4, FieldMemOperand(r2, FixedArray::kLengthOffset));
  __ StoreP(r3, FieldMemOperand(r2, HeapObject::kMapOffset));

  // Set up the fixed slots, copy the global object from the previous context.
  __ LoadP(r4,
           MemOperand(cp, Context::SlotOffset(Context::GLOBAL_OBJECT_INDEX)));
  __ LoadSmiLiteral(r3, Smi::FromInt(0));
  __ StoreP(r5,
            MemOperand(r2, Context::SlotOffset(Context::CLOSURE_INDEX)));
  __ StoreP(cp, MemOperand(r2, Context::SlotOffset(Context::PREVIOUS_INDEX)));
  __ StoreP(r3,
            MemOperand(r2, Context::SlotOffset(Context::EXTENSION_INDEX)));
  __ StoreP(r4,
           MemOperand(r2, Context::SlotOffset(Context::GLOBAL_OBJECT_INDEX)));

  // Initialize the rest of the slots to undefined.
  __ LoadRoot(r3, Heap::kUndefinedValueRootIndex);
  for (int i = Context::MIN_CONTEXT_SLOTS; i < length; i++) {
    __ StoreP(r3, MemOperand(r2, Context::SlotOffset(i)));
  }

  // Remove the on-stack argument and return.
  __ LoadRR(cp, r2);
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
                        r2, r3, r4, &gc, TAG_OBJECT);

  // Load the function from the stack.
  __ LoadP(r5, MemOperand(sp, 0));

  // Load the serialized scope info from the stack.
  __ LoadP(r3, MemOperand(sp, 1 * kPointerSize));

  // Set up the object header.
  __ LoadRoot(r4, Heap::kBlockContextMapRootIndex);
  __ StoreP(r4, FieldMemOperand(r2, HeapObject::kMapOffset));
  __ LoadSmiLiteral(r4, Smi::FromInt(length));
  __ StoreP(r4, FieldMemOperand(r2, FixedArray::kLengthOffset));

  // If this block context is nested in the native context we get a smi
  // sentinel instead of a function. The block context should get the
  // canonical empty function of the native context as its closure which
  // we still have to look up.
  Label after_sentinel;
  __ JumpIfNotSmi(r5, &after_sentinel);
  if (FLAG_debug_code) {
    const char* message = "Expected 0 as a Smi sentinel";
    __ CmpP(r5, Operand::Zero());
    __ Assert(eq, message);
  }
  __ LoadP(r5, GlobalObjectOperand());
  __ LoadP(r5, FieldMemOperand(r5, GlobalObject::kNativeContextOffset));
  __ LoadP(r5, ContextOperand(r5, Context::CLOSURE_INDEX));
  __ bind(&after_sentinel);

  // Set up the fixed slots, copy the global object from the previous context.
  __ LoadP(r4, ContextOperand(cp, Context::GLOBAL_OBJECT_INDEX));
  __ StoreP(r5, ContextOperand(r2, Context::CLOSURE_INDEX));
  __ StoreP(cp, ContextOperand(r2, Context::PREVIOUS_INDEX));
  __ StoreP(r3, ContextOperand(r2, Context::EXTENSION_INDEX));
  __ StoreP(r4, ContextOperand(r2, Context::GLOBAL_OBJECT_INDEX));

  // Initialize the rest of the slots to the hole value.
  __ LoadRoot(r3, Heap::kTheHoleValueRootIndex);
  for (int i = 0; i < slots_; i++) {
    __ StoreP(r3, ContextOperand(r2, i + Context::MIN_CONTEXT_SLOTS));
  }

  // Remove the on-stack argument and return.
  __ LoadRR(cp, r2);
  __ la(sp, MemOperand(sp, (2 * kPointerSize)));
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
  // r5: boilerplate literal array.
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
                        r2,
                        r3,
                        r4,
                        fail,
                        TAG_OBJECT);

  // Copy the JS array part.
  for (int i = 0; i < JSArray::kSize; i += kPointerSize) {
    if ((i != JSArray::kElementsOffset) || (length == 0)) {
      __ LoadP(r3, FieldMemOperand(r5, i));
      __ StoreP(r3, FieldMemOperand(r2, i));
    }
  }

  if (length > 0) {
    // Get hold of the elements array of the boilerplate and setup the
    // elements pointer in the resulting object.
    __ LoadP(r5, FieldMemOperand(r5, JSArray::kElementsOffset));
    __ AddP(r4, r2, Operand(JSArray::kSize));
    __ StoreP(r4, FieldMemOperand(r2, JSArray::kElementsOffset));

    // Copy the elements array.
    ASSERT((elements_size % kPointerSize) == 0);
    __ CopyFields(r4, r5, r3.bit(), elements_size / kPointerSize);
  }
}

void FastCloneShallowArrayStub::Generate(MacroAssembler* masm) {
  // Stack layout on entry:
  //
  // [sp]: constant elements.
  // [sp + kPointerSize]: literal index.
  // [sp + (2 * kPointerSize)]: literals array.

  // Load boilerplate object into r2 and check if we need to create a
  // boilerplate.
  Label slow_case;
  __ LoadP(r5, MemOperand(sp, 2 * kPointerSize));
  __ LoadP(r2, MemOperand(sp, 1 * kPointerSize));
  __ AddP(r5, Operand(FixedArray::kHeaderSize - kHeapObjectTag));

  __ LoadRR(r0, r2);
  __ SmiToPtrArrayOffset(r2, r2);
  __ LoadP(r5, MemOperand(r5, r2));
  __ LoadRR(r2, r0);

  __ CompareRoot(r5, Heap::kUndefinedValueRootIndex);
  __ beq(&slow_case);

  FastCloneShallowArrayStub::Mode mode = mode_;
  if (mode == CLONE_ANY_ELEMENTS) {
    Label double_elements, check_fast_elements;
    __ LoadP(r2, FieldMemOperand(r5, JSArray::kElementsOffset));
    __ LoadP(r2, FieldMemOperand(r2, HeapObject::kMapOffset));
    __ CompareRoot(r2, Heap::kFixedCOWArrayMapRootIndex);
    __ bne(&check_fast_elements);
    GenerateFastCloneShallowArrayCommon(masm, 0,
                                        COPY_ON_WRITE_ELEMENTS, &slow_case);
    // Return and remove the on-stack parameters.
    __ la(sp, MemOperand(sp, (3 * kPointerSize)));
    __ Ret();

    __ bind(&check_fast_elements);
    __ CompareRoot(r2, Heap::kFixedArrayMapRootIndex);
    __ bne(&double_elements);
    GenerateFastCloneShallowArrayCommon(masm, length_,
                                        CLONE_ELEMENTS, &slow_case);
    // Return and remove the on-stack parameters.
    __ la(sp, MemOperand(sp, (3 * kPointerSize)));
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
    __ push(r5);
    __ LoadP(r5, FieldMemOperand(r5, JSArray::kElementsOffset));
    __ LoadP(r5, FieldMemOperand(r5, HeapObject::kMapOffset));
    __ CompareRoot(r5, expected_map_index);
    __ Assert(eq, message);
    __ pop(r5);
  }

  GenerateFastCloneShallowArrayCommon(masm, length_, mode, &slow_case);

  // Return and remove the on-stack parameters.
  __ la(sp, MemOperand(sp, (3 * kPointerSize)));
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

  // Load boilerplate object into r2 and check if we need to create a
  // boilerplate.
  Label slow_case;
  __ LoadP(r5, MemOperand(sp, 3 * kPointerSize));
  __ LoadP(r2, MemOperand(sp, 2 * kPointerSize));
  __ AddP(r5, Operand(FixedArray::kHeaderSize - kHeapObjectTag));
  __ LoadRR(r0, r2);
  __ SmiToPtrArrayOffset(r2, r2);
  __ LoadP(r5, MemOperand(r5, r2));
  __ LoadRR(r2, r0);

  __ CompareRoot(r5, Heap::kUndefinedValueRootIndex);
  __ beq(&slow_case);

  // Check that the boilerplate contains only fast properties and we can
  // statically determine the instance size.
  int size = JSObject::kHeaderSize + length_ * kPointerSize;
  __ LoadP(r2, FieldMemOperand(r5, HeapObject::kMapOffset));
  __ LoadlB(r2, FieldMemOperand(r2, Map::kInstanceSizeOffset));
  __ CmpP(r2, Operand(size >> kPointerSizeLog2));
  __ bne(&slow_case);

  // Allocate the JS object and copy header together with all in-object
  // properties from the boilerplate.
  __ AllocateInNewSpace(size, r2, r3, r4, &slow_case, TAG_OBJECT);
  for (int i = 0; i < size; i += kPointerSize) {
    __ LoadP(r3, FieldMemOperand(r5, i));
    __ StoreP(r3, FieldMemOperand(r2, i));
  }

  // Return and remove the on-stack parameters.
  __ la(sp, MemOperand(sp, (4 * kPointerSize)));
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
  __ SmiToDoubleFPRegister(r2, d2, scratch1);
  __ SmiToDoubleFPRegister(r3, d0, scratch1);
}

// needs cleanup for extra parameters that are unused
void FloatingPointHelper::LoadOperands(
    MacroAssembler* masm,
    Register heap_number_map,
    Register scratch1,
    Register scratch2,
    Label* slow) {
  // Load right operand (r2) to d2
  LoadNumber(masm, r2, d2, heap_number_map, scratch1, scratch2, slow);

  // Load left operand (r3) to d0
  LoadNumber(masm, r3, d0, heap_number_map, scratch1, scratch2, slow);
}

// needs cleanup for extra parameters that are unused
// also needs a scratch double register instead of d3
void FloatingPointHelper::LoadNumber(MacroAssembler* masm,
                                     Register object,
                                     DoubleRegister dst,
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
                                               DoubleRegister double_scratch,
                                               Label* not_number) {
  __ AssertRootValue(heap_number_map,
                     Heap::kHeapNumberMapRootIndex,
                     "HeapNumberMap register clobbered.");
  Label done;
  Label not_in_int32_range;

  __ UntagAndJumpIfSmi(dst, object, &done);
  __ LoadP(scratch1, FieldMemOperand(object, HeapNumber::kMapOffset));
  __ CmpP(scratch1, heap_number_map);
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
                                             DoubleRegister double_dst) {
  __ cdfbr(double_dst, src);
}


void FloatingPointHelper::ConvertUnsignedIntToDouble(MacroAssembler* masm,
                                                    Register src,
                                                    DoubleRegister double_dst) {
  __ llgfr(src, src);
  __ cdgbr(double_dst, src);
}

void FloatingPointHelper::ConvertIntToFloat(MacroAssembler* masm,
                                            const DoubleRegister dst,
                                            const Register src) {
  __ cefbr(dst, src);
}

void FloatingPointHelper::LoadNumberAsInt32Double(MacroAssembler* masm,
                                                  Register object,
                                                  DoubleRegister double_dst,
                                                  DoubleRegister double_scratch,
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
  __ bne(not_int32);

  __ bind(&done);
}


void FloatingPointHelper::LoadNumberAsInt32(MacroAssembler* masm,
                                            Register object,
                                            Register dst,
                                            Register heap_number_map,
                                            Register scratch1,
                                            Register scratch2,
                                            Register scratch3,
                                            DoubleRegister double_scratch0,
                                            DoubleRegister double_scratch1,
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
  __ bne(not_int32);

  __ bind(&done);
}


// TODO(ALANLI): we could use double->int conversion and see
// the condition code is CC_OF or not.
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
  __ CmpP(scratch, Operand::Zero());
  __ blt(not_int32);
  // Exponent greater than 31 cannot yield 32-bit integers.
  // Also, a positive value with an exponent equal to 31 is outside of the
  // signed 32-bit integer range.
  // Another way to put it is that if (exponent - signbit) > 30 then the
  // number cannot be represented as an int32.
  Register tmp = dst;
  __ ExtractSignBit32(tmp, src1);
  __ SubP(tmp, scratch, tmp);
  __ CmpP(tmp, Operand(30));
  __ bgt(not_int32);
  // - Check whether bits [21:0] in the mantissa are not null.
  __ TestBitRange(src2, 21, 0, r0);
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
  __ sll(src1, Operand(HeapNumber::kNonMantissaBitsInTopWord));
  __ OrP(dst, src1);

  // Create the mask and test the lower bits (of the higher bits).
  __ LoadComplementRR(scratch, scratch);
  __ AddP(scratch, Operand(32));
  __ LoadImmP(src2, Operand(1));
  __ ShiftLeftP(src1, src2, scratch);
  __ AddP(src1, Operand(-1));
  __ AndP(r0, src1, dst/*, SetRC*/);
  // Removing RC should be okay
  __ bne(not_int32 /*, cr0*/);
}


void FloatingPointHelper::CallCCodeForDoubleOperation(
    MacroAssembler* masm,
    Token::Value op,
    Register heap_number_result,
    Register scratch) {
  // d0 - first arg, d2 - second arg
  // d0 return value

  // Assert that heap_number_result is callee-saved.
  // PowerPC doesn't preserve r7.. need to handle this specially
  // We currently always use r7 to pass it.
  ASSERT(heap_number_result.is(r7));
  __ push(r7);

  // Push the current return address before the C call. Return will be
  // through pop() below.
  __ push(r14);
  __ PrepareCallCFunction(0, 2, scratch);

  {
    AllowExternalCallThatCantCauseGC scope(masm);
    __ CallCFunction(
        ExternalReference::double_fp_operation(op, masm->isolate()), 0, 2);
  }
  // load saved r7 value, restore lr
  __ pop(r14);
  __ pop(r7);

  // Store answer in the overwritable heap number. Double returned in d1
  __ StoreF(d0, FieldMemOperand(heap_number_result, HeapNumber::kValueOffset));

  // Place heap_number_result in r2 and return to the pushed return address.
  __ LoadRR(r2, heap_number_result);
  __ Ret();
}


void FloatingPointHelper::NumbersToSmis(MacroAssembler* masm,
                                        Register first,
                                        Register second,
                                        Register scratch1,
                                        Register scratch2,
                                        Register scratch3,
                                        Label* on_success,
                                        Label* on_not_smis)   {
  Register heap_number_map = scratch3;
  Register smi_result = scratch1;
  Label done;

  __ LoadRoot(heap_number_map, Heap::kHeapNumberMapRootIndex);

  Label first_smi;
  __ JumpIfSmi(first, &first_smi);
  __ CmpP(heap_number_map, FieldMemOperand(first, HeapObject::kMapOffset));
  __ bne(on_not_smis);

  // Convert HeapNumber to smi if possible.
  __ LoadF(d0, FieldMemOperand(first, HeapNumber::kValueOffset));
  __ lgdr(scratch2, d0);
  __ cfdbr(Condition(5), smi_result, d0);
  // Check if conversion was successful by converting back and
  // comparing to the original double's bits.
  __ cdfbr(d2, smi_result);
  __ lgdr(r0, d2);
  __ CmpP(scratch2, r0);
  __ bne(on_not_smis);
  __ SmiTag(first, smi_result);

  __ JumpIfSmi(second, (on_success != NULL) ? on_success : &done);
  __ bind(&first_smi);

  __ CmpP(heap_number_map, FieldMemOperand(second, HeapObject::kMapOffset));
  __ bne(on_not_smis);

  // Convert second to smi, if possible.
  __ LoadF(d0, FieldMemOperand(second, HeapNumber::kValueOffset));
  __ lgdr(scratch2, d0);
  __ cfdbr(Condition(5), smi_result, d0);
  __ cdfbr(d2, smi_result);
  __ lgdr(r0, d2);
  __ CmpP(scratch2, r0);
  __ bne(on_not_smis);
  __ SmiTag(second, smi_result);
  if (on_success != NULL) {
    __ b(on_success);
  } else {
    __ bind(&done);
  }
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
  __ CmpP(r2, r3);
  __ bne(&not_identical);

  // The two objects are identical.  If we know that one of them isn't NaN then
  // we now know they test equal.
  if (cond != eq || !never_nan_nan) {
    // Test for NaN. Sadly, we can't just compare to FACTORY->nan_value(),
    // so we do the second best thing - test it ourselves.
    // They are both equal and they are not both Smis so both of them are not
    // Smis.  If it's not a heap number, then return equal.
    if (cond == lt || cond == gt) {
      __ CompareObjectType(r2, r6, r6, FIRST_SPEC_OBJECT_TYPE);
      __ bge(slow);
    } else {
      __ CompareObjectType(r2, r6, r6, HEAP_NUMBER_TYPE);
      __ beq(&heap_number);
      // Comparing JS objects with <=, >= is complicated.
      if (cond != eq) {
        __ CmpP(r6, Operand(FIRST_SPEC_OBJECT_TYPE));
        __ bge(slow);
        // Normally here we fall through to return_equal, but undefined is
        // special: (undefined == undefined) == true, but
        // (undefined <= undefined) == false!  See ECMAScript 11.8.5.
        if (cond == le || cond == ge) {
          __ CmpP(r6, Operand(ODDBALL_TYPE));
          __ bne(&return_equal);
          __ CompareRoot(r2, Heap::kUndefinedValueRootIndex);
          __ bne(&return_equal);
          if (cond == le) {
            // undefined <= undefined should fail.
            __ LoadImmP(r2, Operand(GREATER));
          } else  {
            // undefined >= undefined should fail.
            __ LoadImmP(r2, Operand(LESS));
          }
          __ Ret();
        }
      }
    }
  }

  __ bind(&return_equal);
  if (cond == lt) {
    __ LoadImmP(r2, Operand(GREATER));  // Things aren't less than themselves.
  } else if (cond == gt) {
    __ LoadImmP(r2, Operand(LESS));  // Things aren't greater than themselves.
  } else {
    __ LoadImmP(r2, Operand(EQUAL));  // Things are <=, >=, ==, === themselves
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
      __ LoadlW(r4, FieldMemOperand(r2, HeapNumber::kExponentOffset));
      // Test that exponent bits are all set.
      STATIC_ASSERT(HeapNumber::kExponentMask == 0x7ff00000u);
      __ ExtractBitMask(r5, r4, HeapNumber::kExponentMask);
      __ CmpLogicalP(r5, Operand(0x7ff));
      __ bne(&return_equal);

      // Shift out flag and all exponent bits, retaining only mantissa.
      __ sll(r4, Operand(HeapNumber::kNonMantissaBitsInTopWord));
      // Or with all low-bits of mantissa.
      __ LoadlW(r5, FieldMemOperand(r2, HeapNumber::kMantissaOffset));
      __ OrP(r2, r5, r4);
      __ CmpP(r2, Operand::Zero());
      // For equal we already have the right value in r2:  Return zero (equal)
      // if all bits in mantissa are zero (it's an Infinity) and non-zero if
      // not (it's a NaN).  For <= and >= we need to load r0 with the failing
      // value if it's a NaN.
      if (cond != eq) {
        Label not_equal;
        __ bne(&not_equal);
        // All-zero means Infinity means equal.
        __ Ret();
        __ bind(&not_equal);
        if (cond == le) {
          __ LoadImmP(r2, Operand(GREATER));  // NaN <= NaN should fail.
        } else {
          __ LoadImmP(r2, Operand(LESS));     // NaN >= NaN should fail.
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
  ASSERT((lhs.is(r2) && rhs.is(r3)) ||
         (lhs.is(r3) && rhs.is(r2)));

  Label rhs_is_smi;
  __ JumpIfSmi(rhs, &rhs_is_smi);

  // Lhs is a Smi.  Check whether the rhs is a heap number.
  __ CompareObjectType(rhs, r5, r6, HEAP_NUMBER_TYPE);
  if (strict) {
    // If rhs is not a number and lhs is a Smi then strict equality cannot
    // succeed.  Return non-equal
    // If rhs is r2 then there is already a non zero value in it.
    Label skip;
    __ beq(&skip);
    if (!rhs.is(r2)) {
      __ mov(r2, Operand(NOT_EQUAL));
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
  __ SmiToDoubleFPRegister(lhs, d7, r9);
  // Load the double from rhs, tagged HeapNumber r2, to d6.
  __ LoadF(d6, FieldMemOperand(rhs, HeapNumber::kValueOffset));

  // We now have both loaded as doubles but we can skip the lhs nan check
  // since it's a smi.
  __ b(lhs_not_nan);

  __ bind(&rhs_is_smi);
  // Rhs is a smi.  Check whether the non-smi lhs is a heap number.
  __ CompareObjectType(lhs, r6, r6, HEAP_NUMBER_TYPE);
  if (strict) {
    // If lhs is not a number and rhs is a smi then strict equality cannot
    // succeed.  Return non-equal.
    // If lhs is r2 then there is already a non zero value in it.
    Label skip;
    __ beq(&skip);
    if (!lhs.is(r2)) {
      __ mov(r2, Operand(NOT_EQUAL));
    }
    __ Ret();
    __ bind(&skip);
  } else {
    // Smi compared non-strictly with a non-smi non-heap-number.  Call
    // the runtime.
    __ bne(slow);
  }

  // Rhs is a smi, lhs is a heap number.
  // Load the double from lhs, tagged HeapNumber r3, to d7.
  __ LoadF(d7, FieldMemOperand(lhs, HeapNumber::kValueOffset));
  // Convert rhs to a double in d6.
  __ SmiToDoubleFPRegister(rhs, d6, r9);
  // Fall through to both_loaded_as_doubles.
}

// See comment at call site.
static void EmitStrictTwoHeapObjectCompare(MacroAssembler* masm,
                                           Register lhs,
                                           Register rhs) {
    ASSERT((lhs.is(r2) && rhs.is(r3)) ||
           (lhs.is(r3) && rhs.is(r2)));

    // If either operand is a JS object or an oddball value, then they are
    // not equal since their pointers are different.
    // There is no test for undetectability in strict equality.
    STATIC_ASSERT(LAST_TYPE == LAST_SPEC_OBJECT_TYPE);
    Label first_non_object;
    // Get the type of the first operand into r4 and compare it with
    // FIRST_SPEC_OBJECT_TYPE.
    __ CompareObjectType(rhs, r4, r4, FIRST_SPEC_OBJECT_TYPE);
    __ blt(&first_non_object);

    // Return non-zero (r2 is not zero)
    Label return_not_equal;
    __ bind(&return_not_equal);
    __ Ret();

    __ bind(&first_non_object);
    // Check for oddballs: true, false, null, undefined.
    __ CmpP(r4, Operand(ODDBALL_TYPE));
    __ beq(&return_not_equal);

    __ CompareObjectType(lhs, r5, r5, FIRST_SPEC_OBJECT_TYPE);
    __ bge(&return_not_equal);

    // Check for oddballs: true, false, null, undefined.
    __ CmpP(r5, Operand(ODDBALL_TYPE));
    __ beq(&return_not_equal);

    // Now that we have the types we might as well check for symbol-symbol.
    // Ensure that no non-strings have the symbol bit set.
    STATIC_ASSERT(LAST_TYPE < kNotStringTag + kIsSymbolMask);
    STATIC_ASSERT(kSymbolTag != 0);
    __ AndP(r4, r5);
    __ AndP(r0, r4, Operand(kIsSymbolMask));
    __ bne(&return_not_equal /*, cr0*/);
}


static void EmitCheckForTwoHeapNumbers(MacroAssembler* masm,
                                       Register lhs,
                                       Register rhs,
                                       Label* both_loaded_as_doubles,
                                       Label* not_heap_numbers,
                                       Label* slow) {
  ASSERT((lhs.is(r2) && rhs.is(r3)) ||
         (lhs.is(r3) && rhs.is(r2)));

  __ CompareObjectType(rhs, r5, r4, HEAP_NUMBER_TYPE);
  __ bne(not_heap_numbers);
  __ LoadP(r4, FieldMemOperand(lhs, HeapObject::kMapOffset));
  __ CmpP(r4, r5);
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
  ASSERT((lhs.is(r2) && rhs.is(r3)) ||
         (lhs.is(r3) && rhs.is(r2)));

  // r4 is object type of rhs.
  // Ensure that no non-strings have the symbol bit set.
  Label object_test;
  STATIC_ASSERT(kSymbolTag != 0);
  __ mov(r0, Operand(kIsNotStringMask));
  __ AndP(r0, r4);
  __ bne(&object_test /*, cr0*/);
  __ mov(r0, Operand(kIsSymbolMask));
  __ AndP(r0, r4);
  __ beq(possible_strings /*, cr0*/);
  __ CompareObjectType(lhs, r5, r5, FIRST_NONSTRING_TYPE);
  __ bge(not_both_strings);
  __ mov(r0, Operand(kIsSymbolMask));
  __ AndP(r0, r5);
  __ beq(possible_strings /*, cr0*/);

  // Both are symbols.  We already checked they weren't the same pointer
  // so they are not equal.
  __ LoadImmP(r2, Operand(NOT_EQUAL));
  __ Ret();

  __ bind(&object_test);
  __ CmpP(r4, Operand(FIRST_SPEC_OBJECT_TYPE));
  __ blt(not_both_strings);
  __ CompareObjectType(lhs, r4, r5, FIRST_SPEC_OBJECT_TYPE);
  __ blt(not_both_strings);
  // If both objects are undetectable, they are equal. Otherwise, they
  // are not equal, since they are different objects and an object is not
  // equal to undefined.
  __ LoadP(r5, FieldMemOperand(rhs, HeapObject::kMapOffset));
  __ LoadlB(r4, FieldMemOperand(r4, Map::kBitFieldOffset));
  __ LoadlB(r5, FieldMemOperand(r5, Map::kBitFieldOffset));
  __ AndP(r2, r5, r4);
  __ AndP(r2, Operand(1 << Map::kIsUndetectable));
  __ XorP(r2, Operand(1 << Map::kIsUndetectable));
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
  __ ShiftRightArithP(mask, mask, Operand(kSmiTagSize + kSmiShiftSize + 1));
  __ SubP(mask, Operand(1));  // Make mask.

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
    __ ShiftLeftP(scratch1, scratch1, Operand(kPointerSizeLog2 + 1));
    __ AddP(scratch1, number_string_cache);

    Register probe = mask;
    __ LoadP(probe, FieldMemOperand(scratch1, FixedArray::kHeaderSize));
    __ JumpIfSmi(probe, not_found);
    __ LoadF(d0, FieldMemOperand(object, HeapNumber::kValueOffset));
    __ LoadF(d1, FieldMemOperand(probe, HeapNumber::kValueOffset));
    __ cdbr(d0, d1);
    __ bne(not_found);  // The cache did not contain this value.
    __ b(&load_result_from_cache);
  }

  __ bind(&is_smi);
  Register scratch = scratch1;
  __ SmiUntag(scratch, object);
  __ AndP(scratch, mask);
  // Calculate address of entry in string cache: each entry consists
  // of two pointer sized fields.
  __ ShiftLeftP(scratch, scratch, Operand(kPointerSizeLog2 + 1));
  __ AddP(scratch, number_string_cache);

  // Check if the entry is the smi we are looking for.
  Register probe = mask;
  __ LoadP(probe, FieldMemOperand(scratch, FixedArray::kHeaderSize));
  __ CmpP(object, probe);
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

  __ LoadP(r3, MemOperand(sp, 0));

  // Generate code to lookup number in the number string cache.
  GenerateLookupNumberStringCache(masm, r3, r2, r4, r5, r6, false,
                                  &runtime);
  __ la(sp, MemOperand(sp, (1 * kPointerSize)));
  __ Ret();

  __ bind(&runtime);
  // Handle number to string in the runtime system if not found in the cache.
  __ TailCallRuntime(Runtime::kNumberToStringSkipCache, 1, 1);
}


// On entry lhs_ and rhs_ are the values to be compared.
// On exit r2 is 0, positive or negative to indicate the result of
// the comparison.
void CompareStub::Generate(MacroAssembler* masm) {
  ASSERT((lhs_.is(r2) && rhs_.is(r3)) ||
         (lhs_.is(r3) && rhs_.is(r2)));

  Label slow;  // Call builtin.
  Label not_smis, both_loaded_as_doubles, lhs_not_nan;

  if (include_smi_compare_) {
    Label not_two_smis, smi_done;
    __ OrP(r4, r3, r2);
    __ JumpIfNotSmi(r4, &not_two_smis);
    __ SmiUntag(r3);
    __ SmiUntag(r2);
    __ SubP(r2, r3, r2);
    __ Ret();
    __ bind(&not_two_smis);
  } else if (FLAG_debug_code) {
    __ OrP(r4, r3, r2);
    STATIC_ASSERT(kSmiTagMask < 0x8000);
    __ mov(r0, Operand(kSmiTagMask));
    __ AndP(r0, r4);
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
  __ AndP(r4, lhs_, rhs_);
  __ JumpIfNotSmi(r4, &not_smis);
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
  __ cdbr(d7, d6);

  Label nan, equal, less_than;
  __ bunordered(&nan);
  __ beq(&equal);
  __ blt(&less_than);
  __ LoadImmP(r2, Operand(GREATER));
  __ Ret();
  __ bind(&equal);
  __ LoadImmP(r2, Operand(EQUAL));
  __ Ret();
  __ bind(&less_than);
  __ LoadImmP(r2, Operand(LESS));
  __ Ret();

  __ bind(&nan);
  // If one of the sides was a NaN then the v flag is set.  Load r2 with
  // whatever it takes to make the comparison fail, since comparisons with NaN
  // always fail.
  if (cc_ == lt || cc_ == le) {
    __ LoadImmP(r2, Operand(GREATER));
  } else {
    __ LoadImmP(r2, Operand(LESS));
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
  // load both doubles into r2, r3, r4, r5 and jump to the code that
  // handles that case.  If the inputs are not doubles then jumps to
  // check_for_symbols. In this case r4 will contain the type of rhs_.
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
    // Assumes that r4 is the type of rhs_ on entry.
    EmitCheckForSymbolsOrObjects(masm, lhs_, rhs_, &flat_string_check, &slow);
  }

  // Check for both being sequential ASCII strings, and inline if that is the
  // case.
  __ bind(&flat_string_check);

  __ JumpIfNonSmisNotBothSequentialAsciiStrings(lhs_, rhs_, r4, r5, &slow);

  __ IncrementCounter(isolate->counters()->string_compare_native(), 1, r4,
                      r5);
  if (cc_ == eq) {
    StringCompareStub::GenerateFlatAsciiStringEquals(masm,
                                                     lhs_,
                                                     rhs_,
                                                     r4,
                                                     r5);
  } else {
    StringCompareStub::GenerateCompareFlatAsciiStrings(masm,
                                                       lhs_,
                                                       rhs_,
                                                       r4,
                                                       r5,
                                                       r6);
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
    __ LoadSmiLiteral(r2, Smi::FromInt(ncr));
    __ push(r2);
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
  const Register map = r1.is(tos_) ? r9 : r1;

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
      __ tm(FieldMemOperand(map, Map::kBitFieldOffset),
            Operand(1 << Map::kIsUndetectable));
      STATIC_ASSERT((1 << Map::kIsUndetectable) < 0x8000);
      __ beq(&not_undetectable, Label::kNear);
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
    __ LoadImmP(r0, Operand::Zero());
    __ push(r0);
#if !V8_TARGET_ARCH_S390X
    __ push(r0);
#endif
    __ LoadF(d2, MemOperand(sp, 0));
    __ la(sp, MemOperand(sp, 8));
    __ cdbr(d1, d2);
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
    __ CompareRoot(tos_, value);
    __ bne(&different_value, Label::kNear);
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
  if (!tos_.is(r5)) {
    __ LoadRR(r5, tos_);
  }
  __ LoadSmiLiteral(r4, Smi::FromInt(tos_.code()));
  __ LoadSmiLiteral(r3, Smi::FromInt(types_.ToByte()));
  __ Push(r5, r4, r3);
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
  __ LoadRR(r0, r14);
  __ MultiPush(kJSCallerSaved | r0.bit());
  if (save_doubles_ == kSaveFPRegs) {
    const int kNumRegs = DoubleRegister::kNumVolatileRegisters;
    __ SubP(sp, Operand(kDoubleSize * kNumRegs));
    for (int i = 0; i < kNumRegs; i++) {
      DoubleRegister reg = DoubleRegister::from_code(i);
      __ StoreF(reg, MemOperand(sp, i * kDoubleSize));
    }
  }
  const int argument_count = 1;
  const int fp_argument_count = 0;
  const Register scratch = r3;

  AllowExternalCallThatCantCauseGC scope(masm);
  __ PrepareCallCFunction(argument_count, fp_argument_count, scratch);
  __ mov(r2, Operand(ExternalReference::isolate_address()));
  __ CallCFunction(
      ExternalReference::store_buffer_overflow_function(masm->isolate()),
      argument_count);
  if (save_doubles_ == kSaveFPRegs) {
    const int kNumRegs = DoubleRegister::kNumVolatileRegisters;
    for (int i = 0; i < kNumRegs; i++) {
      DoubleRegister reg = DoubleRegister::from_code(i);
      __ LoadF(reg, MemOperand(sp, i * kDoubleSize));
    }
    __ la(sp, MemOperand(sp, (kDoubleSize * kNumRegs)));
  }
  __ MultiPop(kJSCallerSaved | r0.bit());
  __ LoadRR(r14, r0);
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
  __ LoadRR(r5, r2);  // the operand
  __ LoadSmiLiteral(r4, Smi::FromInt(op_));
  __ LoadSmiLiteral(r3, Smi::FromInt(mode_));
  __ LoadSmiLiteral(r2, Smi::FromInt(operand_type_));
  __ Push(r5, r4, r3, r2);

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
  __ JumpIfNotSmi(r2, non_smi);

  // The result of negating zero or the smallest negative smi is not a smi.
  __ TestBitRange(r2, kBitsPerPointer - 2, 0, r0);
  __ beq(slow /*, cr0*/);

  // Return '- value'.
  __ LoadComplementRR(r2, r2);
  __ Ret();
}


void UnaryOpStub::GenerateSmiCodeBitNot(MacroAssembler* masm,
                                        Label* non_smi) {
  __ JumpIfNotSmi(r2, non_smi);

  // Flip bits and revert inverted smi-tag.
  ASSERT(kSmiTagMask == 1);
  __ NotP(r2);
  __ ClearRightImm(r2, r2, Operand(kSmiTagSize + kSmiShiftSize));
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
  EmitCheckForHeapNumber(masm, r2, r3, r8, slow);
  // r2 is a heap number.  Get a new heap number in r3.
  if (mode_ == UNARY_OVERWRITE) {
    __ LoadlW(r4, FieldMemOperand(r2, HeapNumber::kExponentOffset));
    __ xilf(r4, Operand(HeapNumber::kSignMask));  // Flip sign.
    __ StoreW(r4, FieldMemOperand(r2, HeapNumber::kExponentOffset));
  } else {
    Label slow_allocate_heapnumber, heapnumber_allocated;
    __ AllocateHeapNumber(r3, r4, r5, r8, &slow_allocate_heapnumber);
    __ b(&heapnumber_allocated);

    __ bind(&slow_allocate_heapnumber);
    {
      FrameScope scope(masm, StackFrame::INTERNAL);
      __ push(r2);
      __ CallRuntime(Runtime::kNumberAlloc, 0);
      __ LoadRR(r3, r2);
      __ pop(r2);
    }

    __ bind(&heapnumber_allocated);
    __ LoadlW(r5, FieldMemOperand(r2, HeapNumber::kMantissaOffset));
    __ LoadlW(r4, FieldMemOperand(r2, HeapNumber::kExponentOffset));
    __ StoreW(r5, FieldMemOperand(r3, HeapNumber::kMantissaOffset));
    __ mov(r0, Operand(HeapNumber::kSignMask));
    __ XorP(r4, r0);
    __ StoreW(r4, FieldMemOperand(r3, HeapNumber::kExponentOffset));
    __ LoadRR(r2, r3);
  }
  __ Ret();
}


void UnaryOpStub::GenerateHeapNumberCodeBitNot(
    MacroAssembler* masm, Label* slow) {
  Label impossible;

  EmitCheckForHeapNumber(masm, r2, r3, r8, slow);
  // Convert the heap number in r2 to an untagged integer in r3.
  __ ConvertToInt32(r2, r3, r4, r5, d0, slow);

  // Do the bitwise operation and check if the result fits in a smi.
  __ NotP(r3);

#if !V8_TARGET_ARCH_S390X
  Label try_float;
  __ JumpIfNotSmiCandidate(r3, r4, &try_float);
#endif

  // Tag the result as a smi and we're done.
  __ SmiTag(r2, r3);
  __ Ret();

#if !V8_TARGET_ARCH_S390X
  // Try to store the result in a heap number.
  __ bind(&try_float);
  if (mode_ == UNARY_NO_OVERWRITE) {
    Label slow_allocate_heapnumber, heapnumber_allocated;
    // Allocate a new heap number without zapping r0, which we need if it fails.
    __ AllocateHeapNumber(r4, r5, r6, r8, &slow_allocate_heapnumber);
    __ b(&heapnumber_allocated);

    __ bind(&slow_allocate_heapnumber);
    {
      FrameScope scope(masm, StackFrame::INTERNAL);
      __ push(r2);  // Push the heap number, not the untagged int32.
      __ CallRuntime(Runtime::kNumberAlloc, 0);
      __ LoadRR(r4, r2);  // Move the new heap number into r4.
      // Get the heap number into r2, now that the new heap number is in r4.
      __ pop(r2);
    }

    // Convert the heap number in r2 to an untagged integer in r3.
    // This can't go slow-case because it's the same number we already
    // converted once again.
    __ ConvertToInt32(r2, r3, r5, r6, d0, &impossible);
    __ NotP(r3);

    __ bind(&heapnumber_allocated);
    __ LoadRR(r2, r4);  // Move newly allocated heap number to r0.
  }

  // Convert the int32 in r3 to the heap number in r2.
  FloatingPointHelper::ConvertIntToDouble(masm, r3, d0);
  __ StoreF(d0, FieldMemOperand(r2, HeapNumber::kValueOffset));
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
  __ push(r2);
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

  __ Push(r3, r2);

  __ LoadSmiLiteral(r4, Smi::FromInt(MinorKey()));
  __ LoadSmiLiteral(r3, Smi::FromInt(op_));
  __ LoadSmiLiteral(r2, Smi::FromInt(operands_type_));
  __ Push(r4, r3, r2);

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
  Register left = r3;
  Register right = r2;
  Register scratch1 = r9;
  Register scratch2 = r1;

  ASSERT(right.is(r2));
  STATIC_ASSERT(kSmiTag == 0);

  Label not_smi_result;
  switch (op_) {
    case Token::ADD: {
      Label undo_add;
      __ LoadRR(scratch1, right);
      __ AddP(right, left, right);
      __ b(overflow, &undo_add, Label::kNear);
      __ Ret();
      __ bind(&undo_add);
      __ LoadRR(right, scratch1);  // Revert optimistic add.
      break;
    }
    case Token::SUB: {
      Label undo_sub;
      __ LoadRR(scratch1, right);

      // Cannot use right instead of scratch1 for 2nd operand of subtract
      // as SubP, when it cannot use SRK, will switch to SR/LCR sequence
      // which may not set underflow properly.
      __ SubP(right, left, scratch1);
      __ b(overflow, &undo_sub, Label::kNear);
      __ Ret();
      __ bind(&undo_sub);
      __ LoadRR(right, scratch1);  // Revert optimistic subtract.
      break;
    }
    case Token::MUL: {
      Label mul_zero, mul_neg_zero;
#if V8_TARGET_ARCH_S390X
      // Remove tag from both operands.
      __ SmiUntag(r1, left);         // r1 = r3
      __ SmiUntag(ip, right);
      // Do multiplication
      __ mr_z(r0, ip);  // r0:r1 = r1 * ip

      // Check for overflowing the smi range - no overflow if higher 33 bits of
      // the result are identical.
      __ lr(ip, r1);
      __ sra(ip, Operand(31));
      __ cr_z(ip, r0);
      // TODO(JOHN): The above 3 instr expended from 31-bit TestIfInt32
      __ bne(&not_smi_result);
#else
      // Remove tag from one of the operands. This way the multiplication result
      // will be a smi if it fits the smi range.
      __ SmiUntag(scratch2, right);  // r1 = right
      // Do multiplication
      // scratch1 = lower 32 bits of product.
      // scratch2 = higher 32 bits of product.
      __ mr_z(r0, left);  // r0:r1 = r1 * r3
      // Check for overflowing the smi range - no overflow if higher 33 bits of
      // the result are identical.
      __ TestIfInt32(r0, scratch2, ip);
      __ bne(&not_smi_result);
#endif
      // Go slow on zero result to handle -0.
      __ chi(r1, Operand::Zero());
      __ beq(&mul_zero);
#if V8_TARGET_ARCH_S390X
      __ SmiTag(right, scratch2);
#else
      __ LoadRR(right, scratch2);
#endif
      __ Ret();
      __ bind(&mul_zero);
      // We need -0 if we were multiplying a negative number with 0 to get 0.
      // We know one of them was zero.
      __ AddP(scratch2, right, left);
      __ CmpP(scratch2, Operand::Zero());
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
      __ SmiUntag(r0, left);
      __ LoadRR(r1, r0);
      __ ShiftRightArithP(r0, r0, Operand(31));  // right shift 32bit
      __ SmiUntag(r9, right);
      // Check for zero on the right hand side.
      __ beq(&not_smi_result);

      __ DivP(r0, r9);  // remainder in r0, quo in 1

      // Not Smi if remainder is non-zero.
      __ CmpP(r0, Operand::Zero());
      __ bne(&not_smi_result);
      // If the result is 0, we need to check for the -0 case.
      __ SmiTag(r0, r1);
      __ beq(&check_neg_zero);
      // Check for Smi overflow
      __ XorP(r1, r0);
      __ LoadAndTestRR(r1, r1);
      __ blt(&not_smi_result);
      __ LoadRR(right, r0);
      __ Ret();

      // If divisor (right) is negative, we must produce -0.
      __ bind(&check_neg_zero);
      __ CmpP(right, Operand::Zero());
      __ blt(&not_smi_result);
      __ LoadRR(right, r0);
      __ Ret();
      break;
    }
    case Token::MOD: {
      Label check_neg_zero;
      __ SmiUntag(r1, left);
#if !V8_TARGET_ARCH_S390X
      // DivP generates the DR instruction on 31-bit
      // DR treats the dividend as a 64-bit value comprised of R0:R1
      // Hence, we need to sign-extend the untagged 'left' value into R0
      __ ShiftRightArithP(r0, r1, Operand(31));
#endif
      __ SmiUntag(r9, right);
      // Check for zero on the right hand side.
      __ beq(&not_smi_result);

      // do the dividing, remainder in r0
      __ DivP(r0, r9);

      // if the result is zero, need to check -0 case
      __ CmpP(r0, Operand::Zero());  // have to use 32-bit comparision
      __ beq(&check_neg_zero);

      __ SmiTag(right, r0);
#if !V8_TARGET_ARCH_S390X
      // Check that the signed result fits in a Smi.
      __ JumpIfNotSmiCandidate(scratch1, scratch2, &not_smi_result);
#endif
      __ Ret();

      // If dividend (left) is negative, we must produce -0.
      __ bind(&check_neg_zero);
      __ CmpP(left, Operand::Zero());
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
      __ ShiftRightArithP(right, left, scratch1);
      // Smi tag result.
      __ ClearRightImm(right, right, Operand(kSmiTagSize + kSmiShiftSize));
      __ Ret();
      break;
    case Token::SHR:
      // Remove tags from operands. We can't do this on a 31 bit number
      // because then the 0s get shifted into bit 30 instead of bit 31.
      __ SmiUntag(scratch1, left);
      __ GetLeastBitsFromSmi(scratch2, right, 5);
      __ srl(scratch1, scratch2);
      // Unsigned shift is not allowed to produce a negative number.
      __ JumpIfNotUnsignedSmiCandidate(scratch1, r0, &not_smi_result);
      // Smi tag result.
      __ SmiTag(right, scratch1);
      __ Ret();
      break;
    case Token::SHL:
      // Remove tags from operands.
      __ SmiUntag(scratch1, left);
      __ GetLeastBitsFromSmi(scratch2, right, 5);
      __ ShiftLeftP(scratch1, scratch1, scratch2);
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
  Register left = r3;
  Register right = r2;
  Register scratch1 = r9;
  Register scratch2 = r1;
  Register scratch3 = r6;

  ASSERT(smi_operands || (not_numbers != NULL));
  if (smi_operands) {
    __ AssertSmi(left);
    __ AssertSmi(right);
  }

  Register heap_number_map = r8;
  __ LoadRoot(heap_number_map, Heap::kHeapNumberMapRootIndex);

  switch (op_) {
    case Token::ADD:
    case Token::SUB:
    case Token::MUL:
    case Token::DIV:
    case Token::MOD: {
      // Load left and right operands into d0 and d2
      // Allocate new heap number for result.
      Register result = r7;
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
      //   d0: Left value
      //   d2: Right value
      switch (op_) {
        case Token::ADD:
          __ adbr(d0, d2);
          break;
        case Token::SUB:
          __ sdbr(d0, d2);
          break;
        case Token::MUL:
          __ mdbr(d0, d2);
          break;
        case Token::DIV:
          __ ddbr(d0, d2);
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
      __ StoreF(d0, FieldMemOperand(result, HeapNumber::kValueOffset));
      __ LoadRR(r2, result);
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
        __ SmiUntag(r5, left);
        __ SmiUntag(r4, right);
      } else {
        // Convert operands to 32-bit integers. Right in r4 and left in r5.
        FloatingPointHelper::ConvertNumberToInt32(masm,
                                                  left,
                                                  r5,
                                                  heap_number_map,
                                                  scratch1,
                                                  scratch2,
                                                  scratch3,
                                                  d0,
                                                  not_numbers);
        FloatingPointHelper::ConvertNumberToInt32(masm,
                                                  right,
                                                  r4,
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
          __ OrP(r4, r5);
          break;
        case Token::BIT_XOR:
          __ XorP(r4, r5);
          break;
        case Token::BIT_AND:
          __ AndP(r4, r5);
          break;
        case Token::SAR:
          // Use only the 5 least significant bits of the shift count.
          __ GetLeastBitsFromInt32(r4, r4, 5);
          __ LoadRR(scratch1, r4);  // Reg shuffling as sra clobbers
          __ LoadRR(r4, r5);
          __ sra(r4, scratch1);
          break;
        case Token::SHR:
        {
          // Use only the 5 least significant bits of the shift count.
          __ GetLeastBitsFromInt32(r4, r4, 5);
          // SHR is special because it is required to produce a positive answer.
          // The code below for writing into heap numbers isn't capable of
          // writing the register as an unsigned int so we go to slow case if we
          // hit this case.
#if V8_TARGET_ARCH_S390X
          const Condition cond = ne;
          __ LoadRR(scratch1, r4);  // Reg shuffling as srl clobbers
          __ ShiftRight(r4, r5, scratch1);
          __ TestSignBit32(r4, r0);
#else
          const Condition cond = lt;
          __ LoadRR(scratch1, r4);  // Reg shuffling as srl clobbers
          __ ShiftRight(r4, r5, scratch1);
          __ ltr(r4, r4);           // Set the <,eq,> conditions
#endif
          __ b(cond, &result_not_a_smi /*, cr0*/);
          break;
        }
        case Token::SHL:
          // Use only the 5 least significant bits of the shift count.
          __ GetLeastBitsFromInt32(r4, r4, 5);
          __ LoadRR(scratch1, r4);
          __ ShiftLeftP(r4, r5, scratch1);
          break;
        default:
          UNREACHABLE();
      }

#if !V8_TARGET_ARCH_S390X
      // Check that the *signed* result fits in a smi.
      __ JumpIfNotSmiCandidate(r4, r5, &result_not_a_smi);
#endif
      __ SmiTag(r2, r4);
      __ Ret();

      // Allocate new heap number for result.
      __ bind(&result_not_a_smi);
      Register result = r7;
      if (smi_operands) {
        __ AllocateHeapNumber(
            result, scratch1, scratch2, heap_number_map, gc_required);
      } else {
        GenerateHeapResultAllocation(
            masm, result, heap_number_map, scratch1, scratch2, gc_required);
      }

      // r4: Answer as signed int32.
      // r7: Heap number to write answer into.

      // Nothing can go wrong now, so move the heap number to r2, which is the
      // result.
      __ LoadRR(r2, r7);

      // Convert the int32 in r4 to the heap number in r2. As
      // mentioned above SHR needs to always produce a positive result.
      if (op_ == Token::SHR) {
        FloatingPointHelper::ConvertUnsignedIntToDouble(
          masm, r4, d0);
      } else {
        FloatingPointHelper::ConvertIntToDouble(
          masm, r4, d0);
      }
      __ StoreF(d0, FieldMemOperand(r2, HeapNumber::kValueOffset));
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
  Label smi_values;

  Register left = r3;
  Register right = r2;
  Register scratch1 = r9;

  __ bind(&smi_values);

  // Perform combined smi check on both operands.
  __ OrP(scratch1, left, right);
  STATIC_ASSERT(kSmiTag == 0);
  __ JumpIfNotSmi(scratch1, &not_smis);

  // If the smi-smi operation results in a smi return is generated.
  GenerateSmiSmiOperation(masm);

  // If heap number results are possible generate the result in an allocated
  // heap number.
  if (allow_heapnumber_results == ALLOW_HEAPNUMBER_RESULTS) {
    GenerateFPOperation(masm, true, use_runtime, gc_required);
  }

#if !V8_TARGET_ARCH_S390X
  __ bind(&not_smis);
#else
  Label fail;
  Register scratch2 = r1;
  Register scratch3 = r7;

  __ b(&fail, Label::kNear);
  __ bind(&not_smis);
  // Non-smi operands reach the end of the code generated by
  // GenerateSmiCode, and fall through to subsequent code,
  // with the operands in left and right.
  // But first we check if non-smi values are HeapNumbers holding
  // values that could be smi.
  Comment done_comment(masm, "-- Enter non-smi code");
  FloatingPointHelper::NumbersToSmis(masm, left, right,
                                     scratch1, scratch2, scratch3,
                                     &smi_values, &fail);

  __ bind(&fail);
#endif
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
  Register left = r3;
  Register right = r2;

  // Test if left operand is a string.
  __ JumpIfSmi(left, &call_runtime);
  __ CompareObjectType(left, r4, r4, FIRST_NONSTRING_TYPE);
  __ bge(&call_runtime);

  // Test if right operand is a string.
  __ JumpIfSmi(right, &call_runtime);
  __ CompareObjectType(right, r4, r4, FIRST_NONSTRING_TYPE);
  __ bge(&call_runtime);

  StringAddStub string_add_stub(NO_STRING_CHECK_IN_STUB);
  GenerateRegisterArgsPush(masm);
  __ TailCallStub(&string_add_stub);

  __ bind(&call_runtime);
  GenerateTypeTransition(masm);
}


void BinaryOpStub::GenerateInt32Stub(MacroAssembler* masm) {
  ASSERT(operands_type_ == BinaryOpIC::INT32);

  Register left = r3;
  Register right = r2;
  Register scratch1 = r9;
  Register scratch2 = r1;
  DoubleRegister double_scratch0 = d0;
  DoubleRegister double_scratch1 = d1;

  Register heap_number_result = no_reg;
  Register heap_number_map = r8;
  __ LoadRoot(heap_number_map, Heap::kHeapNumberMapRootIndex);

  Label call_runtime;
  // Labels for type transition, used for wrong input or output types.
  // Both label are currently actually bound to the same position. We use two
  // different label to differentiate the cause leading to type transition.
  Label transition;

  // Smi-smi fast case.
  Label skip;
  __ OrP(scratch1, left, right);
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
      // Jump to type transition if they are not. The registers r2 and r3
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
                                                   d0,
                                                   d8,
                                                   heap_number_map,
                                                   scratch1,
                                                   scratch2,
                                                   &transition);

      Label return_heap_number;
      switch (op_) {
        case Token::ADD:
          __ adbr(d0, d2);
          break;
        case Token::SUB:
          __ sdbr(d0, d2);
          break;
        case Token::MUL:
          __ mdbr(d0, d2);
          break;
        case Token::DIV:
          __ ddbr(d0, d2);
          break;
        case Token::MOD: {
          Label pop_and_call_runtime;

          // Allocate a heap number to store the result.
          heap_number_result = r7;
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
                           d0,
                           scratch2,
                           d8);

        // result does not fit in a 32-bit integer.
        Label *not_int32 = ((result_type_ <= BinaryOpIC::INT32) ?
                            &transition : &return_heap_number);
        __ bne(not_int32);

#if !V8_TARGET_ARCH_S390X
        // Check if the result fits in a smi.
        // If not try to return a heap number.
        __ JumpIfNotSmiCandidate(scratch1, scratch2, &return_heap_number);
#endif
        // Check for minus zero. Return heap number for minus zero.
        Label not_zero;
        __ CmpP(scratch1, Operand::Zero());
        __ bne(&not_zero);

        __ SubP(sp, Operand(8));
        __ StoreF(d0, MemOperand(sp, 0));
#if V8_TARGET_ARCH_S390X
        __ lg(scratch2, MemOperand(sp, 0));
#else
#if __FLOAT_WORD_ORDER == __LITTLE_ENDIAN
        __ LoadlW(scratch2, MemOperand(sp, 4));
#else
        __ LoadlW(scratch2, MemOperand(sp, 0));
#endif
#endif
        __ la(sp, MemOperand(sp, 8));

        __ TestSignBit(scratch2, r0);
        __ bne(&return_heap_number /*, cr0*/);
        __ bind(&not_zero);

        // Tag the result and return.
        __ SmiTag(r2, scratch1);
        __ Ret();
      } else {
        // DIV just falls through to allocating a heap number.
      }

      __ bind(&return_heap_number);
      // Return a heap number, or fall through to type transition or runtime
      // call if we can't.
      if (result_type_ >= ((op_ == Token::DIV) ? BinaryOpIC::HEAP_NUMBER
                                               : BinaryOpIC::INT32)) {
        heap_number_result = r7;
        GenerateHeapResultAllocation(masm,
                                     heap_number_result,
                                     heap_number_map,
                                     scratch1,
                                     scratch2,
                                     &call_runtime);
        __ StoreF(d0, FieldMemOperand(heap_number_result,
                                    HeapNumber::kValueOffset));
        __ LoadRR(r2, heap_number_result);
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
      Register scratch3 = r7;
      // Convert operands to 32-bit integers. Right in r4 and left in r5.
      // The registers r2 and r3 (right and left) are preserved for the
      // runtime call.
      FloatingPointHelper::LoadNumberAsInt32(masm,
                                             left,
                                             r5,
                                             heap_number_map,
                                             scratch1,
                                             scratch2,
                                             scratch3,
                                             double_scratch0,
                                             double_scratch1,
                                             &transition);
      FloatingPointHelper::LoadNumberAsInt32(masm,
                                             right,
                                             r4,
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
          __ OrP(r4, r5);
          break;
        case Token::BIT_XOR:
          __ XorP(r4, r5);
          break;
        case Token::BIT_AND:
          __ AndP(r4, r5);
          break;
        case Token::SAR:
          __ GetLeastBitsFromInt32(r4, r4, 5);
          __ LoadRR(scratch1, r4);  // Reg shuffling as sra clobbers
          __ ShiftRightArith(r4, r5, scratch1);
          break;
        case Token::SHR:
        {
          __ GetLeastBitsFromInt32(r4, r4, 5);
          // SHR is special because it is required to produce a positive answer.
          // We only get a negative result if the shift value (r4) is 0.
          // This result cannot be respresented as a signed 32-bit integer, try
          // to return a heap number if we can.
#if V8_TARGET_ARCH_S390X
          const Condition cond = ne;
          __ LoadRR(scratch1, r4);  // Reg shuffling as srl clobbers
          __ ShiftRight(r4, r5, scratch1);
          __ TestSignBit32(r4, r0);
#else
          const Condition cond = lt;
          __ LoadRR(scratch1, r4);  // Reg shuffling as srl clobbers
          __ ShiftRight(r4, r5, scratch1);
          __ ltr(r4, r4);           // Set the <,eq,> conditions
#endif
          __ b(cond, ((result_type_ <= BinaryOpIC::INT32)
                      ? &transition
                      : &return_heap_number) /*, cr0*/);
          break;
        }
        case Token::SHL:
          __ AndP(scratch1, r4, Operand(0x1f));
          __ ShiftLeftP(r4, r5, scratch1);
          break;
        default:
          UNREACHABLE();
      }

#if !V8_TARGET_ARCH_S390X
      // Check if the result fits in a smi.
      // If not try to return a heap number. (We know the result is an int32.)
      __ JumpIfNotSmiCandidate(r4, scratch1, &return_heap_number);
#endif
      // Tag the result and return.
      __ SmiTag(r2, r4);
      __ Ret();

      __ bind(&return_heap_number);
      heap_number_result = r7;
      GenerateHeapResultAllocation(masm,
                                   heap_number_result,
                                   heap_number_map,
                                   scratch1,
                                   scratch2,
                                   &call_runtime);

      if (op_ != Token::SHR) {
        // Convert the result to a floating point value.
        FloatingPointHelper::ConvertIntToDouble(masm, r4, double_scratch0);
      } else {
        // The result must be interpreted as an unsigned 32-bit integer.
        FloatingPointHelper::ConvertUnsignedIntToDouble(masm, r4,
                                                        double_scratch0);
      }

      // Store the result.
      __ StoreF(double_scratch0, FieldMemOperand(heap_number_result,
                                               HeapNumber::kValueOffset));
      __ LoadRR(r2, heap_number_result);
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
  __ CompareRoot(r3, Heap::kUndefinedValueRootIndex);
  __ bne(&check);
  if (Token::IsBitOp(op_)) {
    __ LoadSmiLiteral(r3, Smi::FromInt(0));
  } else {
    __ LoadRoot(r3, Heap::kNanValueRootIndex);
  }
  __ b(&done);
  __ bind(&check);
  __ CompareRoot(r2, Heap::kUndefinedValueRootIndex);
  __ bne(&done);
  if (Token::IsBitOp(op_)) {
    __ LoadSmiLiteral(r2, Smi::FromInt(0));
  } else {
    __ LoadRoot(r2, Heap::kNanValueRootIndex);
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

  Register left = r3;
  Register right = r2;

  // Check if left argument is a string.
  __ JumpIfSmi(left, &left_not_string);
  __ CompareObjectType(left, r4, r4, FIRST_NONSTRING_TYPE);
  __ bge(&left_not_string);

  StringAddStub string_add_left_stub(NO_STRING_CHECK_LEFT_IN_STUB);
  GenerateRegisterArgsPush(masm);
  __ TailCallStub(&string_add_left_stub);

  // Left operand is not a string, test right.
  __ bind(&left_not_string);
  __ JumpIfSmi(right, &call_runtime);
  __ CompareObjectType(right, r4, r4, FIRST_NONSTRING_TYPE);
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
  ASSERT(!result.is(r2) && !result.is(r3));

  if (mode_ == OVERWRITE_LEFT || mode_ == OVERWRITE_RIGHT) {
    Label skip_allocation, allocated;
    Register overwritable_operand = mode_ == OVERWRITE_LEFT ? r3 : r2;
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
  __ Push(r3, r2);
}


void TranscendentalCacheStub::Generate(MacroAssembler* masm) {
  // Untagged case: double input in d2, double result goes
  //   into d2.
  // Tagged case: tagged input on top of stack and in r2,
  //   tagged result (heap number) goes into r2.

  Label input_not_smi;
  Label loaded;
  Label calculate;
  Label invalid_cache;
  const Register scratch0 = r1;
  const Register scratch1 = r9;
  const Register cache_entry = r2;
  const bool tagged = (argument_type_ == TAGGED);

  if (tagged) {
    // Argument is a number and is on stack and in r2.
    // Load argument and check if it is a smi.
    __ JumpIfNotSmi(r2, &input_not_smi);

    // Input is a smi. Convert to double and load the low and high words
    // of the double into r4, r5.
    __ SmiToDoubleFPRegister(r2, d6, scratch0);
    __ lay(sp, MemOperand(sp, -8));
    __ StoreF(d6, MemOperand(sp, 0));
#if __FLOAT_WORD_ORDER == __LITTLE_ENDIAN
    __ LoadlW(r4, MemOperand(sp));
    __ LoadlW(r5, MemOperand(sp, 4));
#else
    __ LoadlW(r4, MemOperand(sp, 4));
    __ LoadlW(r5, MemOperand(sp));
#endif
    __ la(sp, MemOperand(sp, +8));
    __ b(&loaded);

    __ bind(&input_not_smi);
    // Check if input is a HeapNumber.
    __ CheckMap(r2,
                r3,
                Heap::kHeapNumberMapRootIndex,
                &calculate,
                DONT_DO_SMI_CHECK);
    // Input is a HeapNumber. Load it to a double register and store the
    // low and high words into r4, r5.
    __ LoadlW(r5, FieldMemOperand(r2, HeapNumber::kExponentOffset));
    __ LoadlW(r4, FieldMemOperand(r2, HeapNumber::kMantissaOffset));
  } else {
    // Input is untagged double in d2. Output goes to d2.
    __ lay(sp, MemOperand(sp, -8));
    __ StoreF(d2, MemOperand(sp, 0));
#if __FLOAT_WORD_ORDER == __LITTLE_ENDIAN
    __ LoadlW(r4, MemOperand(sp, 4));
    __ LoadlW(r5, MemOperand(sp));
#else
    __ LoadlW(r4, MemOperand(sp));
    __ LoadlW(r5, MemOperand(sp, 4));
#endif
    __ la(sp, MemOperand(sp, 8));
  }
  __ bind(&loaded);
  // r4 = low 32 bits of double value
  // r5 = high 32 bits of double value
  // Compute hash (the shifts are arithmetic):
  //   h = (low ^ high); h ^= h >> 16; h ^= h >> 8; h = h & (cacheSize - 1);
  __ XorP(r3, r4, r5);
  __ ShiftRightArith(scratch0, r3, Operand(16));
  __ XorP(r3, scratch0);
  __ ShiftRightArith(scratch0, r3, Operand(8));
  __ XorP(r3, scratch0);
  ASSERT(IsPowerOf2(TranscendentalCache::SubCache::kCacheSize));
  __ AndP(r3, Operand(TranscendentalCache::SubCache::kCacheSize - 1));

  // r4 = low 32 bits of double value.
  // r5 = high 32 bits of double value.
  // r3 = TranscendentalCache::hash(double value).
  Isolate* isolate = masm->isolate();
  ExternalReference cache_array =
      ExternalReference::transcendental_cache_array_address(isolate);
  __ mov(cache_entry, Operand(cache_array));
  // cache_entry points to cache array.
  int cache_array_index
      = type_ * sizeof(isolate->transcendental_cache()->caches_[0]);
  // r2 points to the cache for the type type_.
  // If NULL, the cache hasn't been initialized yet, so go through runtime.
  __ LoadAndTestP(cache_entry, MemOperand(cache_entry, cache_array_index));
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
  // Find the address of the r3'th entry in the cache, i.e., &r2[r3*16].
  __ ShiftLeftP(scratch0, r3, Operand(4));
#else
  // Find the address of the r3'th entry in the cache, i.e., &r2[r3*12].
  __ ShiftLeftP(scratch0, r3, Operand(1));
  __ AddP(r3, scratch0);
  __ ShiftLeftP(scratch0, r3, Operand(2));
#endif
  __ AddP(cache_entry, scratch0);
  // Check if cache matches: Double value is stored in uint32_t[2] array.
  __ LoadlW(r6, MemOperand(cache_entry, 0));
  __ LoadlW(r7, MemOperand(cache_entry, 4));
  __ LoadP(r8, MemOperand(cache_entry, 8));
  __ CmpP(r4, r6);
  __ bne(&calculate);
  __ CmpP(r5, r7);
  __ bne(&calculate);
  // Cache hit. Load result, cleanup and return.
  Counters* counters = masm->isolate()->counters();
  __ IncrementCounter(
      counters->transcendental_cache_hit(), 1, scratch0, scratch1);
  if (tagged) {
    // Pop input value from stack and load result into r2.
    __ pop();
    __ LoadRR(r2, r8);
  } else {
    // Load result into d2.
    __ LoadF(d2, FieldMemOperand(r8, HeapNumber::kValueOffset));
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
    // r2: precalculated cache entry address.
    // r4 and r5: parts of the double value.
    // Store r2, r4 and r5 on stack for later before calling C function.
    __ Push(r5, r4, cache_entry);
    GenerateCallCFunction(masm, scratch0);
    __ GetCFunctionDoubleResult(d2);

    // Try to update the cache. If we cannot allocate a
    // heap number, we return the result without updating.
    __ Pop(r5, r4, cache_entry);
    __ LoadRoot(r7, Heap::kHeapNumberMapRootIndex);
    __ AllocateHeapNumber(r8, scratch0, scratch1, r7, &no_update);
    __ StoreF(d2, FieldMemOperand(r8, HeapNumber::kValueOffset));
    __ StoreW(r4, MemOperand(cache_entry, 0));
    __ StoreW(r5, MemOperand(cache_entry, 4));
    __ StoreP(r8, MemOperand(cache_entry, 8));
    __ Ret();

    __ bind(&invalid_cache);
    // The cache is invalid. Call runtime which will recreate the
    // cache.
    __ LoadRoot(r7, Heap::kHeapNumberMapRootIndex);
    __ AllocateHeapNumber(r2, scratch0, scratch1, r7, &skip_cache);
    __ StoreF(d2, FieldMemOperand(r2, HeapNumber::kValueOffset));
    {
      FrameScope scope(masm, StackFrame::INTERNAL);
      __ push(r2);
      __ CallRuntime(RuntimeFunction(), 1);
    }
    __ LoadF(d2, FieldMemOperand(r2, HeapNumber::kValueOffset));
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

  __ push(r14);
  __ PrepareCallCFunction(0, 1, scratch);
  __ ldr(d0, d2);
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
  __ pop(r14);
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
  const Register base = r3;
  const Register exponent = r4;
  const Register heapnumbermap = r7;
  const Register heapnumber = r2;
  const DoubleRegister double_base = d1;
  const DoubleRegister double_exponent = d2;
  const DoubleRegister double_result = d3;
  const DoubleRegister double_scratch = d0;
  const Register scratch = r1;
  const Register scratch2 = r9;

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
    __ CmpP(scratch, heapnumbermap);
    __ bne(&call_runtime);

    __ LoadF(double_base, FieldMemOperand(base, HeapNumber::kValueOffset));
    __ b(&unpack_exponent);

    __ bind(&base_is_smi);
    FloatingPointHelper::ConvertIntToDouble(masm, scratch, double_base);
    __ bind(&unpack_exponent);

    __ UntagAndJumpIfSmi(scratch, exponent, &int_exponent);
    __ LoadP(scratch, FieldMemOperand(exponent, JSObject::kMapOffset));
    __ CmpP(scratch, heapnumbermap);
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
    __ beq(&int_exponent);

    if (exponent_type_ == ON_STACK) {
      // Detect square root case.  Crankshaft detects constant +/-0.5 at
      // compile time and uses DoMathPowHalf instead.  We then skip this check
      // for non-constant cases of +/-0.5 as these hardly occur.
      Label not_plus_half, not_minus_inf1, not_minus_inf2;

      // Test for 0.5.
      __ LoadDoubleLiteral(double_scratch, 0.5, scratch);
      __ cdbr(double_exponent, double_scratch);
      __ bne(&not_plus_half);

      // Calculates square root of base.  Check for the special case of
      // Math.pow(-Infinity, 0.5) == Infinity (ECMA spec, 15.8.2.13).
      __ LoadDoubleLiteral(double_scratch, -V8_INFINITY, scratch);
      __ cdbr(double_base, double_scratch);
      __ bne(&not_minus_inf1);
      __ lcdbr(double_result, double_scratch);
      __ b(&done);
      __ bind(&not_minus_inf1);

      // Add +0 to convert -0 to +0.
      __ ldr(double_scratch, double_base);
      __ lzdr(kDoubleRegZero);
      __ adbr(double_scratch, kDoubleRegZero);
      __ sqdbr(double_result, double_scratch);
      __ b(&done);

      __ bind(&not_plus_half);
      __ LoadDoubleLiteral(double_scratch, -0.5, scratch);
      __ cdbr(double_exponent, double_scratch);
      __ bne(&call_runtime);

      // Calculates square root of base.  Check for the special case of
      // Math.pow(-Infinity, -0.5) == 0 (ECMA spec, 15.8.2.13).
      __ LoadDoubleLiteral(double_scratch, -V8_INFINITY, scratch);
      __ cdbr(double_base, double_scratch);
      __ bne(&not_minus_inf2);
      __ ldr(double_result, kDoubleRegZero);
      __ b(&done);
      __ bind(&not_minus_inf2);

      // Add +0 to convert -0 to +0.
      __ ldr(double_scratch, double_base);
      __ lzdr(kDoubleRegZero);
      __ adbr(double_scratch, kDoubleRegZero);
      __ LoadDoubleLiteral(double_result, 1.0, scratch);
      __ sqdbr(double_scratch, double_scratch);
      __ ddbr(double_result, double_scratch);
      __ b(&done);
    }

    __ push(r14);
    {
      AllowExternalCallThatCantCauseGC scope(masm);
      __ PrepareCallCFunction(0, 2, scratch);
      __ SetCallCDoubleArguments(double_base, double_exponent);
      __ CallCFunction(
          ExternalReference::power_double_double_function(masm->isolate()),
          0, 2);
    }
    __ pop(r14);
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
  __ CmpP(scratch, Operand::Zero());
  __ bge(&positive_exponent);
  __ LoadComplementRR(scratch, scratch);
  __ bind(&positive_exponent);

  Label while_true, no_carry, loop_end;
  __ bind(&while_true);
  __ mov(scratch2, Operand(1));
  __ AndP(scratch2, scratch);
  __ beq(&no_carry /*, cr0*/);
  __ mdbr(double_result, double_scratch);
  __ bind(&no_carry);
  __ ShiftRightArithP(scratch, scratch, Operand(1));
  __ beq(&loop_end /*, cr0*/);
  __ mdbr(double_scratch, double_scratch);
  __ b(&while_true);
  __ bind(&loop_end);

  __ CmpP(exponent, Operand::Zero());
  __ bge(&done);

  // get 1/double_result:
  __ ldr(double_scratch, double_result);
  __ LoadImmP(scratch2, Operand(1));
  FloatingPointHelper::ConvertIntToDouble(masm, scratch2, double_result);
  __ ddbr(double_result, double_scratch);

  // Test whether result is zero.  Bail out to check for subnormal result.
  // Due to subnormals, x^-y == (1/x)^y does not hold in all cases.
  __ cdbr(double_result, kDoubleRegZero);
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
    ASSERT(heapnumber.is(r2));
    __ IncrementCounter(counters->math_pow(), 1, scratch, scratch2);
    __ Ret(2);
  } else {
    __ push(r14);
    {
      AllowExternalCallThatCantCauseGC scope(masm);
      __ PrepareCallCFunction(0, 2, scratch);
      __ SetCallCDoubleArguments(double_base, double_exponent);
      __ CallCFunction(
          ExternalReference::power_double_double_function(masm->isolate()),
          0, 2);
    }
    __ pop(r14);
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
  // r6: number of arguments including receiver (C callee-saved)
  // r7: pointer to builtin function (C callee-saved)
  // r8: pointer to first argument (C callee-saved)
  Isolate* isolate = masm->isolate();
  Register isolate_reg = no_reg;

  if (do_gc) {
    // Passing r2.
    __ PrepareCallCFunction(1, 0, r3);
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
#if defined(V8_HOST_ARCH_S390)
  // Call C built-in on native hardware.

#if defined(V8_TARGET_ARCH_S390X)
  // zLinux 64-bit
  // Use frame storage reserved by calling function as ABI passes C++ objects
  // larger than 8 bytes by reference.  On 64-bit, Arguments is 16-bytes, so
  // we need to build the object on the stack.
#if ABI_RETURNS_OBJECT_PAIRS_IN_REGS
  __ la(r2, MemOperand(sp, (kStackFrameExtraParamSlot + 1) * kPointerSize));
  __ st(r6, MemOperand(r2));
  __ StoreP(r8, MemOperand(r2, kPointerSize));
  isolate_reg = r3;
#else  // ABI_RETURNS_OBJECT_PAIRS_IN_REGS
  // This is the default path for zLinux 64 native.
  if (result_size_ < 2) {
    __ la(r2, MemOperand(sp, (kStackFrameExtraParamSlot + 1) * kPointerSize));
    __ st(r6, MemOperand(r2));
    __ StoreP(r8, MemOperand(r2, kPointerSize));
    isolate_reg = r3;
  } else {
    // The result of the call is 16-byte non-scalar value (i.e.
    // ObjectPair), we need to use frame storage reserved by calling function to
    // pass return buffer as an implicit first argument.
    ASSERT_EQ(2, result_size_);
    __ la(r2, MemOperand(sp, (kStackFrameExtraParamSlot + 1) * kPointerSize));
    __ la(r3, MemOperand(sp, (kStackFrameExtraParamSlot + 3) * kPointerSize));
    __ st(r6, MemOperand(r3));
    __ StoreP(r8, MemOperand(r3, kPointerSize));
    isolate_reg = r4;
  }
#endif  // ABI_RETURNS_OBJECT_PAIRS_IN_REGS

#else   // V8_TARGET_ARCH_S390X
  // zLinux 31-bit
  // r2 = argc, r3 = argv
  __ LoadRR(r2, r6);
  __ LoadRR(r3, r8);
  isolate_reg = r4;
#endif  // V8_TARGET_ARCH_S390X

#else  // Simulated
  // Call C built-in using simulator.
  // r3 = argc, r4 = argv
  // @TODO Make sure this is correct for S390
#if defined(V8_TARGET_ARCH_S390X) && __BYTE_ORDER == __BIG_ENDIAN
  __ ShiftLeftP(r2, r6, Operand(32));
#else
  __ LoadRR(r2, r6);
#endif
  __ LoadRR(r3, r8);
  isolate_reg = r4;
#endif

  __ mov(isolate_reg, Operand(ExternalReference::isolate_address()));

#if ABI_USES_FUNCTION_DESCRIPTORS && !defined(USE_SIMULATOR)
  // Native AIX/PPC64 Linux use a function descriptor.
  // @TODO Haven't touched this code for S390.. See if it's applicable.
  // especially the ToRegister(2) part.
  __ LoadP(ToRegister(2), MemOperand(r7, kPointerSize));  // TOC
  __ LoadP(ip, MemOperand(r7, 0));  // Instruction address
  Register target = ip;
#else
  Register target = r7;
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
    __ larl(r14, &return_label);  // Generate the return addr of call later.
    __ StoreP(r14, MemOperand(sp, kStackFrameRASlot * kPointerSize));

    // zLinux ABI requires caller's frame to have sufficient space for callee
    // preserved regsiter save area.
    // __ lay(sp, MemOperand(sp, -kCalleeRegisterSaveAreaSize));
    __ positions_recorder()->WriteRecordedPositions();
    __ b(target);
    __ bind(&return_label);
    // __ la(sp, MemOperand(sp, +kCalleeRegisterSaveAreaSize));
  }

  if (always_allocate) {
    // It's okay to clobber r4 and r5 here. Don't mess with r2 and r3
    // though (contain the result).
    __ mov(r4, Operand(scope_depth));
    // @TODO Can exploit ASI here on z10 or newer.
    __ LoadlW(r5, MemOperand(r4));
    __ SubP(r5, Operand(1));
    __ StoreW(r5, MemOperand(r4));
  }

  // check for failure result
  Label failure_returned;
  STATIC_ASSERT(((kFailureTag + 1) & kFailureTagMask) == 0);
#if defined(V8_TARGET_ARCH_S390X) && !ABI_RETURNS_OBJECT_PAIRS_IN_REGS
  // If return value is on the stack, pop it to registers.
  if (result_size_ > 1) {
    ASSERT_EQ(2, result_size_);
    __ LoadP(r3, MemOperand(r2, kPointerSize));
    __ LoadP(r2, MemOperand(r2));
  }
#endif
  // Lower 2 bits of r5 are 0 iff r3 has failure tag.
  __ AddP(r4, r2, Operand(1));
  STATIC_ASSERT(kFailureTagMask < 0x8000);
  __ nill(r4, Operand(kFailureTagMask));
  __ beq(&failure_returned);  // Branch if and result is zero.

  // Exit C frame and return.
  // r2:r3: result
  // sp: stack pointer
  // fp: frame pointer
  //  Callee-saved register r6 still holds argc.
  __ LeaveExitFrame(save_doubles_, r6);
  __ b(r14);

  // check if we should retry or throw exception
  Label retry;
  __ bind(&failure_returned);
  STATIC_ASSERT(Failure::RETRY_AFTER_GC == 0);
  __ mov(r0,
          Operand(((1 << kFailureTypeTagSize) - 1) << kFailureTagSize));
  __ AndP(r0, r2);
  __ beq(&retry /*, cr0*/);

  // Special handling of out of memory exceptions.
  Failure* out_of_memory = Failure::OutOfMemoryException();
  __ CmpP(r2, Operand(reinterpret_cast<intptr_t>(out_of_memory)));
  __ beq(throw_out_of_memory_exception);

  // Retrieve the pending exception and clear the variable.
  __ mov(r5, Operand(isolate->factory()->the_hole_value()));
  __ mov(ip, Operand(ExternalReference(Isolate::kPendingExceptionAddress,
                                       isolate)));
  __ LoadP(r2, MemOperand(ip));
  __ StoreP(r5, MemOperand(ip));

  // Special handling of termination exceptions which are uncatchable
  // by javascript code.
  __ mov(r5, Operand(isolate->factory()->termination_exception()));
  __ CmpP(r2, r5);
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
  __ ShiftLeftP(r8, r2, Operand(kPointerSizeLog2));
  __ lay(r8, MemOperand(r8, sp, -kPointerSize));

  // Enter the exit frame that transitions from JavaScript to C++.
  FrameScope scope(masm, StackFrame::MANUAL);

  // Need at least one extra slot for return address location.
  int arg_stack_space = 1;

  // PPC LINUX ABI:
  // The #if immediately below was !USE_SIMULATOR, but needed
  // to change to support nativesim=true builds
#if defined(V8_HOST_ARCH_S390)

#if defined(V8_TARGET_ARCH_S390X) && !ABI_RETURNS_OBJECT_PAIRS_IN_REGS
  // Pass buffer for return value on stack if necessary
  if (result_size_ > 1) {
    ASSERT_EQ(2, result_size_);
    arg_stack_space += 2;
  }
#endif
#if defined(V8_TARGET_ARCH_S390X)
  // 64-bit linux pass Argument object by reference not value
  arg_stack_space += 2;
#endif

#endif

  __ EnterExitFrame(save_doubles_, arg_stack_space);

  // Set up argc and the builtin function in callee-saved registers.
  __ LoadRR(r6, r2);  // argc
  __ LoadRR(r7, r3);  // builtin function

  // r6: number of arguments (C callee-saved)
  // r7: pointer to builtin function (C callee-saved)
  // r8: pointer to first argument (C callee-saved)

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
  __ LoadImmP(r2, Operand(false, RelocInfo::NONE));
  __ mov(r4, Operand(external_caught));
  __ StoreP(r2, MemOperand(r4));

  // Set pending exception and r0 to out of memory exception.
  Failure* out_of_memory = Failure::OutOfMemoryException();
  __ mov(r2, Operand(reinterpret_cast<intptr_t>(out_of_memory)));
  __ mov(r4, Operand(ExternalReference(Isolate::kPendingExceptionAddress,
                                       isolate)));
  __ StoreP(r2, MemOperand(r4));
  // Fall through to the next label.

  __ bind(&throw_termination_exception);
  __ ThrowUncatchable(r2);

  __ bind(&throw_normal_exception);
  __ Throw(r2);
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

  // saving floating point registers
#if defined(V8_TARGET_ARCH_S390X)
  // 64bit ABI requires f8 to f15 be saved
  __ lay(sp, MemOperand(sp, -8 * kDoubleSize));
  __ std(d8, MemOperand(sp));
  __ std(d9, MemOperand(sp, 1 * kDoubleSize));
  __ std(d10, MemOperand(sp, 2 * kDoubleSize));
  __ std(d11, MemOperand(sp, 3 * kDoubleSize));
  __ std(d12, MemOperand(sp, 4 * kDoubleSize));
  __ std(d13, MemOperand(sp, 5 * kDoubleSize));
  __ std(d14, MemOperand(sp, 6 * kDoubleSize));
  __ std(d15, MemOperand(sp, 7 * kDoubleSize));
#else
  // 31bit ABI requires you to store f4 and f6:
  // http://refspecs.linuxbase.org/ELF/zSeries/lzsabi0_s390.html#AEN417
  __ lay(sp, MemOperand(sp, -2 * kDoubleSize));
  __ std(d4, MemOperand(sp));
  __ std(d6, MemOperand(sp, kDoubleSize));
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
  __ mov(r7, Operand(ExternalReference(js_entry_sp)));
  __ LoadAndTestP(r8, MemOperand(r7));
  __ bne(&non_outermost_js, Label::kNear);
  __ StoreP(fp, MemOperand(r7));
  __ LoadSmiLiteral(ip, Smi::FromInt(StackFrame::OUTERMOST_JSENTRY_FRAME));
  Label cont;
  __ b(&cont, Label::kNear);
  __ bind(&non_outermost_js);
  __ LoadSmiLiteral(ip, Smi::FromInt(StackFrame::INNER_JSENTRY_FRAME));

  __ bind(&cont);
  __ StoreP(ip, MemOperand(sp));  // frame-type

  // Jump to a faked try block that does the invoke, with a faked catch
  // block that sets the pending exception.
  __ b(&invoke, Label::kNear);

  __ bind(&handler_entry);
  handler_offset_ = handler_entry.pos();
  // Caught exception: Store result (exception) in the pending exception
  // field in the JSEnv and return a failure sentinel.  Coming in here the
  // fp will be invalid because the PushTryHandler below sets it to 0 to
  // signal the existence of the JSEntry frame.
  __ mov(ip, Operand(ExternalReference(Isolate::kPendingExceptionAddress,
                                       isolate)));

  __ StoreP(r2, MemOperand(ip));
  __ mov(r2, Operand(reinterpret_cast<intptr_t>(Failure::Exception())));
  __ b(&exit, Label::kNear);

  // Invoke: Link this frame into the handler chain.  There's only one
  // handler block in this code object, so its index is 0.
  __ bind(&invoke);
  // Must preserve r0-r4, r5-r7 are available. (@TODO needs update for S390)
  __ PushTryHandler(StackHandler::JS_ENTRY, 0);
  // If an exception not caught by another handler occurs, this handler
  // returns control to the code after the b(&invoke) above, which
  // restores all kCalleeSaved registers (including cp and fp) to their
  // saved values before returning a failure to C.

  // Clear any pending exceptions.
  __ mov(ip, Operand(ExternalReference(Isolate::kPendingExceptionAddress,
                                       isolate)));
  __ mov(r7, Operand(isolate->factory()->the_hole_value()));
  __ StoreP(r7, MemOperand(ip));

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
  Label return_addr;
  // __ basr(r14, ip);
  __ larl(r14, &return_addr);
  __ b(ip);
  __ bind(&return_addr);

  // Unlink this frame from the handler chain.
  __ PopTryHandler();

  __ bind(&exit);  // r2 holds result
  // Check if the current stack frame is marked as the outermost JS frame.
  Label non_outermost_js_2;
  __ pop(r7);
  __ CmpSmiLiteral(r7, Smi::FromInt(StackFrame::OUTERMOST_JSENTRY_FRAME), r0);
  __ bne(&non_outermost_js_2);
  __ mov(r8, Operand::Zero());
  __ mov(r7, Operand(ExternalReference(js_entry_sp)));
  __ StoreP(r8, MemOperand(r7));
  __ bind(&non_outermost_js_2);

  // Restore the top frame descriptors from the stack.
  __ pop(r5);
  __ mov(ip,
         Operand(ExternalReference(Isolate::kCEntryFPAddress, isolate)));
  __ StoreP(r5, MemOperand(ip));

  // Reset the stack to the callee saved registers.
  __ lay(sp, MemOperand(sp, -EntryFrameConstants::kCallerFPOffset));

  // Restore callee-saved registers and return.
#ifdef DEBUG
  if (FLAG_debug_code) {
    Label here;
    __ b(r14, &here /*, SetLK*/);
    __ bind(&here);
  }
#endif

  // Reload callee-saved preserved regs, return address reg (r14) and sp
  __ LoadMultipleP(r6, sp, MemOperand(sp, 0));
  __ la(sp, MemOperand(sp, 10 * kPointerSize));

  // saving floating point registers
#if defined(V8_TARGET_ARCH_S390X)
  // 64bit ABI requires f8 to f15 be saved
  __ ld(d8, MemOperand(sp));
  __ ld(d9, MemOperand(sp, 1 * kDoubleSize));
  __ ld(d10, MemOperand(sp, 2 * kDoubleSize));
  __ ld(d11, MemOperand(sp, 3 * kDoubleSize));
  __ ld(d12, MemOperand(sp, 4 * kDoubleSize));
  __ ld(d13, MemOperand(sp, 5 * kDoubleSize));
  __ ld(d14, MemOperand(sp, 6 * kDoubleSize));
  __ ld(d15, MemOperand(sp, 7 * kDoubleSize));
  __ la(sp, MemOperand(sp, 8 * kDoubleSize));
#else
  // 31bit ABI requires you to store f4 and f6:
  // http://refspecs.linuxbase.org/ELF/zSeries/lzsabi0_s390.html#AEN417
  __ ld(d4, MemOperand(sp));
  __ ld(d6, MemOperand(sp, kDoubleSize));
  __ la(sp, MemOperand(sp, 2 * kDoubleSize));
#endif

  __ b(r14);
}


// Uses registers r2 to r6.
// Expected input (depending on whether args are in registers or on the stack):
// * object: r2 or at sp + 1 * kPointerSize.
// * function: r3 or at sp.
//
// An inlined call site may have been generated before calling this stub.
// In this case the offset to the inline site to patch is passed on the stack,
// in the safepoint slot for register r6.
// (See LCodeGen::DoInstanceOfKnownGlobal)
void InstanceofStub::Generate(MacroAssembler* masm) {
  // Call site inlining and patching implies arguments in registers.
  ASSERT(HasArgsInRegisters() || !HasCallSiteInlineCheck());
  // ReturnTrueFalse is only implemented for inlined call sites.
  ASSERT(!ReturnTrueFalseObject() || HasCallSiteInlineCheck());

  // Fixed register usage throughout the stub:
  const Register object = r2;  // Object (lhs).
  Register map = r5;  // Map of the object.
  const Register function = r3;  // Function (rhs).
  const Register prototype = r6;  // Prototype of the function.
  const Register inline_site = r8;
  const Register scratch = r4;
  const Register scratch2 = r7;
  Register scratch3 = no_reg;

#if V8_TARGET_ARCH_S390X
  const int32_t kDeltaToLoadBoolResult = 28;  // IIHF + IILF + LG + CR + BRCL
#else
  const int32_t kDeltaToLoadBoolResult = 18;  // IILF + L + CR + BRCL
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
    __ LoadRoot(r2, Heap::kInstanceofCacheAnswerRootIndex);
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

    // The offset was stored in r6 safepoint slot.
    // (See LCodeGen::DoDeferredLInstanceOfKnownGlobal)
    __ LoadFromSafepointRegisterSlot(scratch, r6);
    __ CleanseP(r14);
    __ LoadRR(inline_site, r14);
    __ SubP(inline_site, inline_site, scratch);
    // Get the map location in scratch and patch it.
    __ GetRelocatedValueLocation(inline_site, scratch, scratch2);
    __ StoreP(map,
         FieldMemOperand(scratch, JSGlobalPropertyCell::kValueOffset));
  }

  // Register mapping: r5 is object map and r6 is function prototype.
  // Get prototype of object into r4.
  __ LoadP(scratch, FieldMemOperand(map, Map::kPrototypeOffset));

  // We don't need map any more. Use it as a scratch register.
  scratch3 = map;
  map = no_reg;

  // Loop through the prototype chain looking for the function prototype.
  __ LoadRoot(scratch3, Heap::kNullValueRootIndex);
  __ bind(&loop);
  __ CmpP(scratch, prototype);
  __ beq(&is_instance);
  __ CmpP(scratch, scratch3);
  __ beq(&is_not_instance);
  __ LoadP(scratch, FieldMemOperand(scratch, HeapObject::kMapOffset));
  __ LoadP(scratch, FieldMemOperand(scratch, Map::kPrototypeOffset));
  __ b(&loop);

  __ bind(&is_instance);
  if (!HasCallSiteInlineCheck()) {
    __ LoadSmiLiteral(r2, Smi::FromInt(0));
    __ StoreRoot(r2, Heap::kInstanceofCacheAnswerRootIndex);
  } else {
    // Patch the call site to return true.
    __ LoadRoot(r2, Heap::kTrueValueRootIndex);
    __ AddP(inline_site, Operand(kDeltaToLoadBoolResult));
    // Get the boolean result location in scratch and patch it.
    __ PatchRelocatedValue(inline_site, scratch, r2);

    if (!ReturnTrueFalseObject()) {
      __ LoadSmiLiteral(r2, Smi::FromInt(0));
    }
  }
  __ Ret(HasArgsInRegisters() ? 0 : 2);

  __ bind(&is_not_instance);
  if (!HasCallSiteInlineCheck()) {
    __ LoadSmiLiteral(r2, Smi::FromInt(1));
    __ StoreRoot(r2, Heap::kInstanceofCacheAnswerRootIndex);
  } else {
    // Patch the call site to return false.
    __ LoadRoot(r2, Heap::kFalseValueRootIndex);
    __ AddP(inline_site, Operand(kDeltaToLoadBoolResult));
    // Get the boolean result location in scratch and patch it.
    __ PatchRelocatedValue(inline_site, scratch, r2);

    if (!ReturnTrueFalseObject()) {
      __ LoadSmiLiteral(r2, Smi::FromInt(1));
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
  __ mov(r0, Operand(masm->isolate()->factory()->null_value()));
  __ CmpP(scratch, r0);
  __ bne(&object_not_null);
  __ LoadSmiLiteral(r2, Smi::FromInt(1));
  __ Ret(HasArgsInRegisters() ? 0 : 2);

  __ bind(&object_not_null);
  // Smi values are not instances of anything.
  __ JumpIfNotSmi(object, &object_not_null_or_smi);
  __ LoadSmiLiteral(r2, Smi::FromInt(1));
  __ Ret(HasArgsInRegisters() ? 0 : 2);

  __ bind(&object_not_null_or_smi);
  // String values are not instances of anything.
  __ IsObjectJSStringType(object, scratch, &slow);
  __ LoadSmiLiteral(r2, Smi::FromInt(1));
  __ Ret(HasArgsInRegisters() ? 0 : 2);

  // Slow-case.  Tail call builtin.
  __ bind(&slow);
  if (!ReturnTrueFalseObject()) {
    if (HasArgsInRegisters()) {
      __ Push(r2, r3);
    }
  __ InvokeBuiltin(Builtins::INSTANCE_OF, JUMP_FUNCTION);
  } else {
    {
      FrameScope scope(masm, StackFrame::INTERNAL);
      __ Push(r2, r3);
      __ InvokeBuiltin(Builtins::INSTANCE_OF, CALL_FUNCTION);
    }
    Label true_value, done;
    __ CmpP(r2, Operand::Zero());
    __ beq(&true_value);

    __ LoadRoot(r2, Heap::kFalseValueRootIndex);
    __ b(&done);

    __ bind(&true_value);
    __ LoadRoot(r2, Heap::kTrueValueRootIndex);

    __ bind(&done);
    __ Ret(HasArgsInRegisters() ? 0 : 2);
  }
}


Register InstanceofStub::left() { return r2; }


Register InstanceofStub::right() { return r3; }


void ArgumentsAccessStub::GenerateReadElement(MacroAssembler* masm) {
  // The displacement is the offset of the last parameter (if any)
  // relative to the frame pointer.
  const int kDisplacement =
      StandardFrameConstants::kCallerSPOffset - kPointerSize;

  // Check that the key is a smi.
  Label slow;
  __ JumpIfNotSmi(r3, &slow);

  // Check if the calling frame is an arguments adaptor frame.
  Label adaptor;
  __ LoadP(r4, MemOperand(fp, StandardFrameConstants::kCallerFPOffset));
  __ LoadP(r5, MemOperand(r4, StandardFrameConstants::kContextOffset));
  STATIC_ASSERT(StackFrame::ARGUMENTS_ADAPTOR < 0x3fffu);
  __ CmpSmiLiteral(r5, Smi::FromInt(StackFrame::ARGUMENTS_ADAPTOR), r0);
  __ beq(&adaptor);

  // Check index against formal parameters count limit passed in
  // through register r2. Use unsigned comparison to get negative
  // check for free.
  __ CmpLogicalP(r3, r2);
  __ bge(&slow);

  // Read the argument from the stack and return it.
  __ SubP(r5, r2, r3);
  __ SmiToPtrArrayOffset(r5, r5);
  __ AddP(r5, fp);
  __ LoadP(r2, MemOperand(r5, kDisplacement));
  __ Ret();

  // Arguments adaptor case: Check index against actual arguments
  // limit found in the arguments adaptor frame. Use unsigned
  // comparison to get negative check for free.
  __ bind(&adaptor);
  __ LoadP(r2,
           MemOperand(r4, ArgumentsAdaptorFrameConstants::kLengthOffset));
  __ CmpLogicalP(r3, r2);
  __ bge(&slow);

  // Read the argument from the adaptor frame and return it.
  __ SubP(r5, r2, r3);
  __ SmiToPtrArrayOffset(r5, r5);
  __ AddP(r5, r4);
  __ LoadP(r2, MemOperand(r5, kDisplacement));
  __ Ret();

  // Slow-case: Handle non-smi or out-of-bounds access to arguments
  // by calling the runtime system.
  __ bind(&slow);
  __ push(r3);
  __ TailCallRuntime(Runtime::kGetArgumentsProperty, 1, 1);
}


void ArgumentsAccessStub::GenerateNewNonStrictSlow(MacroAssembler* masm) {
  // sp[0] : number of parameters
  // sp[1] : receiver displacement
  // sp[2] : function

  // Check if the calling frame is an arguments adaptor frame.
  Label runtime;
  __ LoadP(r5, MemOperand(fp, StandardFrameConstants::kCallerFPOffset));
  __ LoadP(r4, MemOperand(r5, StandardFrameConstants::kContextOffset));
  STATIC_ASSERT(StackFrame::ARGUMENTS_ADAPTOR < 0x3fffu);
  __ CmpSmiLiteral(r4, Smi::FromInt(StackFrame::ARGUMENTS_ADAPTOR), r0);
  __ bne(&runtime);

  // Patch the arguments.length and the parameters pointer in the current frame.
  __ LoadP(r4,
           MemOperand(r5, ArgumentsAdaptorFrameConstants::kLengthOffset));
  __ StoreP(r4, MemOperand(sp, 0 * kPointerSize));
  __ SmiToPtrArrayOffset(r4, r4);
  __ AddP(r5, r4);
  __ AddP(r5, Operand(StandardFrameConstants::kCallerSPOffset));
  __ StoreP(r5, MemOperand(sp, 1 * kPointerSize));

  __ bind(&runtime);
  __ TailCallRuntime(Runtime::kNewArgumentsFast, 3, 1);
}


void ArgumentsAccessStub::GenerateNewNonStrictFast(MacroAssembler* masm) {
  // Stack layout:
  //  sp[0] : number of parameters (tagged)
  //  sp[1] : address of receiver argument
  //  sp[2] : function
  // Registers used over whole function:
  //  r8 : allocated object (tagged)
  //  r1 : mapped parameter count (tagged)

  __ LoadP(r3, MemOperand(sp, 0 * kPointerSize));
  // r3 = parameter count (tagged)

  // Check if the calling frame is an arguments adaptor frame.
  Label runtime;
  Label adaptor_frame, try_allocate;
  __ LoadP(r5, MemOperand(fp, StandardFrameConstants::kCallerFPOffset));
  __ LoadP(r4, MemOperand(r5, StandardFrameConstants::kContextOffset));
  STATIC_ASSERT(StackFrame::ARGUMENTS_ADAPTOR < 0x3fffu);
  __ CmpSmiLiteral(r4, Smi::FromInt(StackFrame::ARGUMENTS_ADAPTOR), r0);
  __ beq(&adaptor_frame);

  // No adaptor, parameter count = argument count.
  __ LoadRR(r4, r3);
  __ b(&try_allocate);

  // We have an adaptor frame. Patch the parameters pointer.
  __ bind(&adaptor_frame);
  __ LoadP(r4,
           MemOperand(r5, ArgumentsAdaptorFrameConstants::kLengthOffset));
  __ SmiToPtrArrayOffset(r6, r4);
  __ AddP(r5, r6);
  __ AddP(r5, Operand(StandardFrameConstants::kCallerSPOffset));
  __ StoreP(r5, MemOperand(sp, 1 * kPointerSize));

  // r3 = parameter count (tagged)
  // r4 = argument count (tagged)
  // Compute the mapped parameter count = min(r3, r4) in r3.
  Label skip;
  __ CmpP(r3, r4);
  __ blt(&skip);
  __ LoadRR(r3, r4);
  __ bind(&skip);

  __ bind(&try_allocate);

  // Compute the sizes of backing store, parameter map, and arguments object.
  // 1. Parameter map, has 2 extra words containing context and backing store.
  const int kParameterMapHeaderSize =
      FixedArray::kHeaderSize + 2 * kPointerSize;
  // If there are no mapped parameters, we do not need the parameter_map.
  Label skip2, skip3;
  __ CmpSmiLiteral(r3, Smi::FromInt(0), r0);
  __ bne(&skip2);
  __ LoadImmP(r1, Operand::Zero());
  __ b(&skip3);
  __ bind(&skip2);
  __ SmiToPtrArrayOffset(r1, r3);
  __ AddP(r1, Operand(kParameterMapHeaderSize));
  __ bind(&skip3);

  // 2. Backing store.
  __ SmiToPtrArrayOffset(r6, r4);
  __ AddP(r1, r6);
  __ AddP(r1, Operand(FixedArray::kHeaderSize));

  // 3. Arguments object.
  __ AddP(r1, Operand(Heap::kArgumentsObjectSize));

  // Do the allocation of all three objects in one go.
  __ AllocateInNewSpace(r1, r2, r5, r6, &runtime, TAG_OBJECT);

  // r2 = address of new object(s) (tagged)
  // r4 = argument count (tagged)
  // Get the arguments boilerplate from the current native context into r3.
  const int kNormalOffset =
      Context::SlotOffset(Context::ARGUMENTS_BOILERPLATE_INDEX);
  const int kAliasedOffset =
      Context::SlotOffset(Context::ALIASED_ARGUMENTS_BOILERPLATE_INDEX);

  __ LoadP(r6, MemOperand(cp,
             Context::SlotOffset(Context::GLOBAL_OBJECT_INDEX)));
  __ LoadP(r6, FieldMemOperand(r6, GlobalObject::kNativeContextOffset));
  Label skip4, skip5;
  __ CmpP(r3, Operand::Zero());
  __ bne(&skip4);
  __ LoadP(r6, MemOperand(r6, kNormalOffset));
  __ b(&skip5);
  __ bind(&skip4);
  __ LoadP(r6, MemOperand(r6, kAliasedOffset));
  __ bind(&skip5);

  // r2 = address of new object (tagged)
  // r3 = mapped parameter count (tagged)
  // r4 = argument count (tagged)
  // r6 = address of boilerplate object (tagged)
  // Copy the JS object part.
  for (int i = 0; i < JSObject::kHeaderSize; i += kPointerSize) {
    __ LoadP(r5, FieldMemOperand(r6, i));
    __ StoreP(r5, FieldMemOperand(r2, i));
  }

  // Set up the callee in-object property.
  STATIC_ASSERT(Heap::kArgumentsCalleeIndex == 1);
  __ LoadP(r5, MemOperand(sp, 2 * kPointerSize));
  const int kCalleeOffset = JSObject::kHeaderSize +
      Heap::kArgumentsCalleeIndex * kPointerSize;
  __ StoreP(r5, FieldMemOperand(r2, kCalleeOffset));

  // Use the length (smi tagged) and set that as an in-object property too.
  STATIC_ASSERT(Heap::kArgumentsLengthIndex == 0);
  const int kLengthOffset = JSObject::kHeaderSize +
      Heap::kArgumentsLengthIndex * kPointerSize;
  __ StoreP(r4, FieldMemOperand(r2, kLengthOffset));

  // Set up the elements pointer in the allocated arguments object.
  // If we allocated a parameter map, r6 will point there, otherwise
  // it will point to the backing store.
  __ AddP(r6, r2, Operand(Heap::kArgumentsObjectSize));
  __ StoreP(r6, FieldMemOperand(r2, JSObject::kElementsOffset));

  // r2 = address of new object (tagged)
  // r3 = mapped parameter count (tagged)
  // r4 = argument count (tagged)
  // r6 = address of parameter map or backing store (tagged)
  // Initialize parameter map. If there are no mapped arguments, we're done.
  Label skip_parameter_map, skip6;
  __ CmpSmiLiteral(r3, Smi::FromInt(0), r0);
  __ bne(&skip6);
  // Move backing store address to r5, because it is
  // expected there when filling in the unmapped arguments.
  __ LoadRR(r5, r6);
  __ b(&skip_parameter_map);
  __ bind(&skip6);

  __ LoadRoot(r8, Heap::kNonStrictArgumentsElementsMapRootIndex);
  __ StoreP(r8, FieldMemOperand(r6, FixedArray::kMapOffset));
  __ AddSmiLiteral(r8, r3, Smi::FromInt(2), r0);
  __ StoreP(r8, FieldMemOperand(r6, FixedArray::kLengthOffset));
  __ StoreP(cp, FieldMemOperand(r6,
                                 FixedArray::kHeaderSize + 0 * kPointerSize));
  __ SmiToPtrArrayOffset(r8, r3);
  __ AddP(r8, r6);
  __ AddP(r8, Operand(kParameterMapHeaderSize));
  __ StoreP(r8, FieldMemOperand(r6,
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
  __ LoadRR(r8, r3);
  __ LoadP(r1, MemOperand(sp, 0 * kPointerSize));
  __ AddSmiLiteral(r1, r1,
                   Smi::FromInt(Context::MIN_CONTEXT_SLOTS), r0);
  __ SubP(r1, r1, r3);
  __ LoadRoot(r9, Heap::kTheHoleValueRootIndex);
  __ SmiToPtrArrayOffset(r5, r8);
  __ AddP(r5, r6);
  __ AddP(r5, Operand(kParameterMapHeaderSize));

  // r8 = loop variable (tagged)
  // r3 = mapping index (tagged)
  // r5 = address of backing store (tagged)
  // r6 = address of parameter map (tagged)
  // r7 = temporary scratch (a.o., for address calculation)
  // r9 = the hole value
  __ b(&parameters_test);

  __ bind(&parameters_loop);
  __ SubSmiLiteral(r8, r8, Smi::FromInt(1), r0);
  __ SmiToPtrArrayOffset(r7, r8);
  __ AddP(r7, Operand(kParameterMapHeaderSize - kHeapObjectTag));
  __ StorePX(r1, MemOperand(r7, r6));
  __ SubP(r7, Operand(kParameterMapHeaderSize - FixedArray::kHeaderSize));
  __ StorePX(r9, MemOperand(r7, r5));
  __ AddSmiLiteral(r1, r1, Smi::FromInt(1), r0);
  __ bind(&parameters_test);
  __ CmpSmiLiteral(r8, Smi::FromInt(0), r0);
  __ bne(&parameters_loop);

  __ bind(&skip_parameter_map);
  // r4 = argument count (tagged)
  // r5 = address of backing store (tagged)
  // r7 = scratch
  // Copy arguments header and remaining slots (if there are any).
  __ LoadRoot(r7, Heap::kFixedArrayMapRootIndex);
  __ StoreP(r7, FieldMemOperand(r5, FixedArray::kMapOffset));
  __ StoreP(r4, FieldMemOperand(r5, FixedArray::kLengthOffset));

  Label arguments_loop, arguments_test;
  __ LoadRR(r1, r3);
  __ LoadP(r6, MemOperand(sp, 1 * kPointerSize));
  __ SmiToPtrArrayOffset(r7, r1);
  __ SubP(r6, r6, r7);
  __ b(&arguments_test);

  __ bind(&arguments_loop);
  __ SubP(r6, Operand(kPointerSize));
  __ LoadP(r8, MemOperand(r6, 0));
  __ SmiToPtrArrayOffset(r7, r1);
  __ AddP(r7, r5);
  __ StoreP(r8, FieldMemOperand(r7, FixedArray::kHeaderSize));
  __ AddSmiLiteral(r1, r1, Smi::FromInt(1), r0);

  __ bind(&arguments_test);
  __ CmpP(r1, r4);
  __ blt(&arguments_loop);

  // Return and remove the on-stack parameters.
  __ la(sp, MemOperand(sp, (3 * kPointerSize)));
  __ Ret();

  // Do the runtime call to allocate the arguments object.
  // r4 = argument count (tagged)
  __ bind(&runtime);
  __ StoreP(r4, MemOperand(sp, 0 * kPointerSize));  // Patch argument count.
  __ TailCallRuntime(Runtime::kNewArgumentsFast, 3, 1);
}

void ArgumentsAccessStub::GenerateNewStrict(MacroAssembler* masm) {
  // sp[0] : number of parameters
  // sp[4] : receiver displacement
  // sp[8] : function
  // Check if the calling frame is an arguments adaptor frame.
  Label adaptor_frame, try_allocate, runtime;
  __ LoadP(r4, MemOperand(fp, StandardFrameConstants::kCallerFPOffset));
  __ LoadP(r5, MemOperand(r4, StandardFrameConstants::kContextOffset));
  STATIC_ASSERT(StackFrame::ARGUMENTS_ADAPTOR < 0x3fffu);
  __ CmpSmiLiteral(r5, Smi::FromInt(StackFrame::ARGUMENTS_ADAPTOR), r0);
  __ beq(&adaptor_frame);

  // Get the length from the frame.
  __ LoadP(r3, MemOperand(sp, 0));
  __ b(&try_allocate);

  // Patch the arguments.length and the parameters pointer.
  __ bind(&adaptor_frame);
  __ LoadP(r3,
           MemOperand(r4, ArgumentsAdaptorFrameConstants::kLengthOffset));
  __ StoreP(r3, MemOperand(sp, 0));
  __ SmiToPtrArrayOffset(r5, r3);
  __ AddP(r5, r4);
  __ AddP(r5, Operand(StandardFrameConstants::kCallerSPOffset));
  __ StoreP(r5, MemOperand(sp, 1 * kPointerSize));

  // Try the new space allocation. Start out with computing the size
  // of the arguments object and the elements array in words.
  Label add_arguments_object;
  __ bind(&try_allocate);
  __ CmpP(r3, Operand(0, RelocInfo::NONE));
  __ beq(&add_arguments_object);
  __ SmiUntag(r3);
  __ AddP(r3, Operand(FixedArray::kHeaderSize / kPointerSize));
  __ bind(&add_arguments_object);
  __ AddP(r3, Operand(Heap::kArgumentsObjectSizeStrict / kPointerSize));

  // Do the allocation of both objects in one go.
  __ AllocateInNewSpace(r3,
                        r2,
                        r4,
                        r5,
                        &runtime,
                        static_cast<AllocationFlags>(TAG_OBJECT |
                                                     SIZE_IN_WORDS));

  // Get the arguments boilerplate from the current native context.
  __ LoadP(r6,
           MemOperand(cp, Context::SlotOffset(Context::GLOBAL_OBJECT_INDEX)));
  __ LoadP(r6, FieldMemOperand(r6, GlobalObject::kNativeContextOffset));
  __ LoadP(r6, MemOperand(r6, Context::SlotOffset(
      Context::STRICT_MODE_ARGUMENTS_BOILERPLATE_INDEX)));

  // Copy the JS object part.
  __ CopyFields(r2, r6, r5.bit(), JSObject::kHeaderSize / kPointerSize);

  // Get the length (smi tagged) and set that as an in-object property too.
  STATIC_ASSERT(Heap::kArgumentsLengthIndex == 0);
  __ LoadP(r3, MemOperand(sp, 0 * kPointerSize));
  __ StoreP(r3, FieldMemOperand(r2, JSObject::kHeaderSize +
                                Heap::kArgumentsLengthIndex * kPointerSize));

  // If there are no actual arguments, we're done.
  Label done;
  __ CmpP(r3, Operand(0, RelocInfo::NONE));
  __ beq(&done);

  // Get the parameters pointer from the stack.
  __ LoadP(r4, MemOperand(sp, 1 * kPointerSize));

  // Set up the elements pointer in the allocated arguments object and
  // initialize the header in the elements fixed array.
  __ AddP(r6, r2, Operand(Heap::kArgumentsObjectSizeStrict));
  __ StoreP(r6, FieldMemOperand(r2, JSObject::kElementsOffset));
  __ LoadRoot(r5, Heap::kFixedArrayMapRootIndex);
  __ StoreP(r5, FieldMemOperand(r6, FixedArray::kMapOffset));
  __ StoreP(r3, FieldMemOperand(r6, FixedArray::kLengthOffset));
  // Untag the length for the loop.
  __ SmiUntag(r3);

  // Copy the fixed array slots.
  Label loop;
  // Set up r6 to point to the first array slot.
  __ AddP(r6, Operand(FixedArray::kHeaderSize - kHeapObjectTag));
  __ bind(&loop);
  // Pre-decrement r4 with kPointerSize on each iteration.
  // Pre-decrement in order to skip receiver.
  __ LoadP(r5, MemOperand(r4, -kPointerSize));
  __ lay(r4, MemOperand(r4, -kPointerSize));
  // Post-increment r6 with kPointerSize on each iteration.
  __ StoreP(r5, MemOperand(r6));
  __ AddP(r6, Operand(kPointerSize));
  __ SubP(r3, Operand(1));
  __ CmpP(r3, Operand(0, RelocInfo::NONE));
  __ bne(&loop);

  // Return and remove the on-stack parameters.
  __ bind(&done);
  __ la(sp, MemOperand(sp, (3 * kPointerSize)));
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
  Register subject = r6;
  Register regexp_data = r7;
  Register last_match_info_elements = r8;
  Register code = r9;

  __ CleanseP(r14);

  __ lay(sp, MemOperand(sp, -13 * kPointerSize));
  __ StoreMultipleP(r3, sp, MemOperand(sp, 0));
  __ la(fp, MemOperand(sp, 13 * kPointerSize));

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
  __ mov(r2, Operand(address_of_regexp_stack_memory_size));
  __ LoadAndTestP(r2, MemOperand(r2));
  __ beq(&runtime);

  // Check that the first argument is a JSRegExp object.
  __ LoadP(r2, MemOperand(fp, kJSRegExpOffset));
  STATIC_ASSERT(kSmiTag == 0);
  __ JumpIfSmi(r2, &runtime);
  __ CompareObjectType(r2, r3, r3, JS_REGEXP_TYPE);
  __ bne(&runtime);

  // Check that the RegExp has been compiled (data contains a fixed array).
  __ LoadP(regexp_data, FieldMemOperand(r2, JSRegExp::kDataOffset));
  if (FLAG_debug_code) {
    STATIC_ASSERT(kSmiTagMask == 1);
    __ tmll(regexp_data, Operand(kSmiTagMask));
    __ Check(ne, "Unexpected type for RegExp data, FixedArray expected", cr0);
    __ CompareObjectType(regexp_data, r2, r2, FIXED_ARRAY_TYPE);
    __ Check(eq, "Unexpected type for RegExp data, FixedArray expected");
  }

  // regexp_data: RegExp data (FixedArray)
  // Check the type of the RegExp. Only continue if type is JSRegExp::IRREGEXP.
  __ LoadP(r2, FieldMemOperand(regexp_data, JSRegExp::kDataTagOffset));
  // ASSERT(Smi::FromInt(JSRegExp::IRREGEXP) < (char *)0xffffu);
  __ CmpSmiLiteral(r2, Smi::FromInt(JSRegExp::IRREGEXP), r0);
  __ bne(&runtime);

  // regexp_data: RegExp data (FixedArray)
  // Check that the number of captures fit in the static offsets vector buffer.
  __ LoadP(r4,
         FieldMemOperand(regexp_data, JSRegExp::kIrregexpCaptureCountOffset));
  // Calculate number of capture registers (number_of_captures + 1) * 2.
  __ SmiToShortArrayOffset(r4, r4);
  __ AddP(r4, Operand(2));
  // Check that the static offsets vector buffer is large enough.
  // STATIC_ASSERT(Isolate::kJSRegexpStaticOffsetsVectorSize < 0xffffu);
  __ CmpLogicalP(r4, Operand(Isolate::kJSRegexpStaticOffsetsVectorSize));
  __ bgt(&runtime);

  // r4: Number of capture registers
  // regexp_data: RegExp data (FixedArray)
  // Check that the second argument is a string.
  __ LoadP(subject, MemOperand(fp, kSubjectOffset));
  __ JumpIfSmi(subject, &runtime);
  Condition is_string = masm->IsObjectStringType(subject, r2);
  __ b(NegateCondition(is_string), &runtime /*, cr0*/);
  // Get the length of the string to r5.
  __ LoadP(r5, FieldMemOperand(subject, String::kLengthOffset));

  // r4: Number of capture registers
  // r5: Length of subject string as a smi
  // subject: Subject string
  // regexp_data: RegExp data (FixedArray)
  // Check that the third argument is a positive smi less than the subject
  // string length. A negative value will be greater (unsigned comparison).
  __ LoadP(r2, MemOperand(fp, kPreviousIndexOffset));
  __ JumpIfNotSmi(r2, &runtime);
  __ CmpLogicalP(r5, r2);
  __ ble(&runtime);

  // r4: Number of capture registers
  // subject: Subject string
  // regexp_data: RegExp data (FixedArray)
  // Check that the fourth object is a JSArray object.
  __ LoadP(r2, MemOperand(fp, kLastMatchInfoOffset));
  __ JumpIfSmi(r2, &runtime);
  __ CompareObjectType(r2, r3, r3, JS_ARRAY_TYPE);
  __ bne(&runtime);
  // Check that the JSArray is in fast case.
  __ LoadP(last_match_info_elements,
         FieldMemOperand(r2, JSArray::kElementsOffset));
  __ LoadP(r2, FieldMemOperand(last_match_info_elements,
                               HeapObject::kMapOffset));
  __ CompareRoot(r2, Heap::kFixedArrayMapRootIndex);
  __ bne(&runtime);
  // Check that the last match info has space for the capture registers and the
  // additional information.
  __ LoadP(r2,
         FieldMemOperand(last_match_info_elements, FixedArray::kLengthOffset));
  __ AddP(r4, Operand(RegExpImpl::kLastMatchOverhead));
  __ SmiUntag(r0, r2);
  __ CmpP(r4, r0);
  __ bgt(&runtime);

  // Reset offset for possibly sliced string.
  __ LoadImmP(r13, Operand::Zero());
  // subject: Subject string
  // regexp_data: RegExp data (FixedArray)
  // Check the representation and encoding of the subject string.
  Label seq_string;
  __ LoadP(r2, FieldMemOperand(subject, HeapObject::kMapOffset));
  __ LoadlB(r2, FieldMemOperand(r2, Map::kInstanceTypeOffset));
  // First check for flat string.  None of the following string type tests will
  // succeed if subject is not a string or a short external string.
  STATIC_ASSERT((kIsNotStringMask |
                  kStringRepresentationMask |
                  kShortExternalStringMask) == 0x93);
  __ mov(r3, Operand(kIsNotStringMask |
                          kStringRepresentationMask |
                          kShortExternalStringMask));
  __ AndP(r3, r2);
  STATIC_ASSERT((kStringTag | kSeqStringTag) == 0);
  __ beq(&seq_string /*, cr0*/);

  // subject: Subject string
  // regexp_data: RegExp data (FixedArray)
  // r3: whether subject is a string and if yes, its string representation
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
  __ CmpP(r3, Operand(kExternalStringTag));
  __ blt(&cons_string);
  __ beq(&external_string);

  // Catch non-string subject or short external string.
  STATIC_ASSERT(kNotStringTag != 0 && kShortExternalStringTag !=0);
  STATIC_ASSERT((kNotStringTag | kShortExternalStringTag) < 0xffffu);
  __ mov(r0, Operand(kIsNotStringMask | kShortExternalStringMask));
  __ AndP(r0, r3);
  __ bne(&runtime /*, cr0*/);

  // String is sliced.
  __ LoadP(r13, FieldMemOperand(subject, SlicedString::kOffsetOffset));
  __ SmiUntag(r13);
  __ LoadP(subject, FieldMemOperand(subject, SlicedString::kParentOffset));
  // r13: offset of sliced string, smi-tagged.
  __ b(&check_encoding);
  // String is a cons string, check whether it is flat.
  __ bind(&cons_string);
  __ LoadP(r2, FieldMemOperand(subject, ConsString::kSecondOffset));
  __ CompareRoot(r2, Heap::kEmptyStringRootIndex);
  __ bne(&runtime);
  __ LoadP(subject, FieldMemOperand(subject, ConsString::kFirstOffset));
  // Is first part of cons or parent of slice a flat string?
  __ bind(&check_encoding);
  __ LoadP(r2, FieldMemOperand(subject, HeapObject::kMapOffset));
  __ LoadlB(r2, FieldMemOperand(r2, Map::kInstanceTypeOffset));
  STATIC_ASSERT(kSeqStringTag == 0);
  STATIC_ASSERT(kStringRepresentationMask == 3);
  __ tmll(r2, Operand(kStringRepresentationMask));
  __ bne(&external_string /*, cr0*/);

  __ bind(&seq_string);
  // subject: Subject string
  // regexp_data: RegExp data (FixedArray)
  // r2: Instance type of subject string
  STATIC_ASSERT(4 == kAsciiStringTag);
  STATIC_ASSERT(kTwoByteStringTag == 0);
  // Find the code object based on the assumptions above.
  STATIC_ASSERT(kStringEncodingMask == 4);
  __ ExtractBitMask(r5, r2, kStringEncodingMask, SetRC);
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

  // r5: encoding of subject string (1 if ASCII, 0 if two_byte);
  // code: Address of generated regexp code
  // subject: Subject string
  // regexp_data: RegExp data (FixedArray)
  // Load used arguments before starting to push arguments for call to native
  // RegExp code to avoid handling changing stack height.
  __ LoadP(r3, MemOperand(fp, kPreviousIndexOffset));
  __ SmiUntag(r3);

  // r3: previous index
  // r5: encoding of subject string (1 if ASCII, 0 if two_byte);
  // code: Address of generated regexp code
  // subject: Subject string
  // regexp_data: RegExp data (FixedArray)
  // All checks done. Now push arguments for native regexp code.
  __ IncrementCounter(isolate->counters()->regexp_entry_native(), 1, r2,
                       r4);

  // Isolates: note we add an additional parameter here (isolate pointer).
  const int kRegExpExecuteArguments = 10;
  const int kParameterRegisters = 5;
  __ EnterExitFrame(false, kRegExpExecuteArguments - kParameterRegisters);

  // Stack pointer now points to cell where return address is to be written.
  // Arguments are before that on the stack or in registers.

  // Argument 10 (in stack parameter area): Pass current isolate address.
  __ mov(r2, Operand(ExternalReference::isolate_address()));
  __ StoreP(r2, MemOperand(sp,
       kStackFrameExtraParamSlot * kPointerSize + 4 * kPointerSize));

  // Argument 9 is a dummy that reserves the space used for
  // the return address added by the ExitFrame in native calls.
  __ mov(r2, Operand::Zero());
  __ StoreP(r2, MemOperand(sp,
        kStackFrameExtraParamSlot * kPointerSize + 3 * kPointerSize));

  // Argument 8: Indicate that this is a direct call from JavaScript.
  __ mov(r2, Operand(1));
  __ StoreP(r2, MemOperand(sp,
        kStackFrameExtraParamSlot * kPointerSize + 2 * kPointerSize));

  // Argument 7: Start (high end) of backtracking stack memory area.
  __ mov(r2, Operand(address_of_regexp_stack_memory_address));
  __ LoadP(r2, MemOperand(r2, 0));
  __ mov(r1, Operand(address_of_regexp_stack_memory_size));
  __ LoadP(r1, MemOperand(r1, 0));
  __ AddP(r2, r1);
  __ StoreP(r2, MemOperand(sp,
        kStackFrameExtraParamSlot * kPointerSize + 1 * kPointerSize));

  // Argument 6: Set the number of capture registers to zero to force
  // global egexps to behave as non-global.  This does not affect non-global
  // regexps.
  __ mov(r2, Operand::Zero());
  __ StoreP(r2, MemOperand(sp,
        kStackFrameExtraParamSlot * kPointerSize + 0 * kPointerSize));

  // Argument 1 (r2): Subject string.
  // Load the length from the original subject string from the previous stack
  // frame. Therefore we have to use fp, which points exactly to 15 pointer
  // sizes below the previous sp. (Because creating a new stack frame pushes
  // the previous fp onto the stack and moves up sp by 2 * kPointerSize and
  // 13 registers saved on the stack previously)
  __ LoadP(r2, MemOperand(fp, kSubjectOffset + 15 * kPointerSize));

  // Argument 2 (r3): Previous index.
  // Already there

  __ AddP(r1, r6, Operand(SeqString::kHeaderSize - kHeapObjectTag));

  // Argument 5 (r6): static offsets vector buffer.
  __ mov(r6,
         Operand(ExternalReference::address_of_static_offsets_vector(isolate)));

  // For arguments 4 (r5) and 3 (r4) get string length, calculate start of
  // string data and calculate the shift of the index (0 for ASCII and 1 for
  // two byte).
  __ XorP(r5, Operand(1));
  // If slice offset is not 0, load the length from the original sliced string.
  //
  // Argument 3, r4: Start of string data
  // Prepare start and end index of the input.
  __ ShiftLeftP(r13, r13, r5);
  __ AddP(r13, r1, r13);
  __ ShiftLeftP(r4, r3, r5);
  __ AddP(r4, r13, r4);

  // Argument 4, r5: End of string data
  __ LoadP(r1, FieldMemOperand(r2, String::kLengthOffset));
  __ SmiUntag(r1);
  __ ShiftLeftP(r0, r1, r5);
  __ AddP(r5, r13, r0);


  // Locate the code entry and call it.
  __ AddP(code, Operand(Code::kHeaderSize - kHeapObjectTag));


#if defined(USE_SIMULATOR) && defined(_AIX)
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

  __ la(fp, MemOperand(sp, 13 * kPointerSize));

  // r2: result
  // subject: subject string -- needed to reload
  __ LoadP(subject, MemOperand(fp, kSubjectOffset));

  // regexp_data: RegExp data (callee saved)
  // last_match_info_elements: Last match info elements (callee saved)

  // Check the result.
  Label success;

  __ CmpP(r2, Operand(1));
  // We expect exactly one result since we force the called regexp to behave
  // as non-global.
  __ beq(&success);
  Label failure;
  __ CmpP(r2, Operand(NativeRegExpMacroAssembler::FAILURE));
  __ beq(&failure);
  __ CmpP(r2, Operand(NativeRegExpMacroAssembler::EXCEPTION));
  // If not exception it can only be retry. Handle that in the runtime system.
  __ bne(&runtime);
  // Result must now be exception. If there is no pending exception already a
  // stack overflow (on the backtrack stack) was detected in RegExp code but
  // haven't created the exception yet. Handle that in the runtime system.
  // TODO(592): Rerunning the RegExp to get the stack overflow exception.
  __ mov(r3, Operand(isolate->factory()->the_hole_value()));
  __ mov(r4, Operand(ExternalReference(Isolate::kPendingExceptionAddress,
                                       isolate)));
  __ LoadP(r2, MemOperand(r4, 0));
  __ CmpP(r2, r3);
  __ beq(&runtime);

  __ StoreP(r3, MemOperand(r4, 0));  // Clear pending exception.

  // Check if the exception is a termination. If so, throw as uncatchable.
  __ CompareRoot(r2, Heap::kTerminationExceptionRootIndex);

  Label termination_exception;
  __ beq(&termination_exception);

  __ Throw(r2);

  __ bind(&termination_exception);
  __ ThrowUncatchable(r2);

  __ bind(&failure);
  // For failure and exception return null.
  __ mov(r2, Operand(masm->isolate()->factory()->null_value()));
  __ LoadMultipleP(r3, sp, MemOperand(sp, 0));
  __ la(sp, MemOperand(sp, 13 * kPointerSize));
  __ la(sp, MemOperand(sp, (4 * kPointerSize)));
  __ Ret();

  // Process the result from the native regexp code.
  __ bind(&success);
  __ LoadP(r3,
         FieldMemOperand(regexp_data, JSRegExp::kIrregexpCaptureCountOffset));
  // Calculate number of capture registers (number_of_captures + 1) * 2.
  __ SmiToShortArrayOffset(r3, r3);
  __ AddP(r3, Operand(2));

  // r3: number of capture registers
  // r6: subject string
  // Store the capture count.
  __ SmiTag(r4, r3);
  __ StoreP(r4, FieldMemOperand(last_match_info_elements,
                                RegExpImpl::kLastCaptureCountOffset));
  // Store last subject and last input.
  __ StoreP(subject,
            FieldMemOperand(last_match_info_elements,
                            RegExpImpl::kLastSubjectOffset));
  __ LoadRR(r4, subject);
  __ RecordWriteField(last_match_info_elements,
                      RegExpImpl::kLastSubjectOffset,
                      r4,
                      r9,
                      kLRHasNotBeenSaved,
                      kDontSaveFPRegs);
  __ StoreP(subject,
            FieldMemOperand(last_match_info_elements,
                            RegExpImpl::kLastInputOffset));
  __ RecordWriteField(last_match_info_elements,
                      RegExpImpl::kLastInputOffset,
                      subject,
                      r9,
                      kLRHasNotBeenSaved,
                      kDontSaveFPRegs);

  // Get the static offsets vector filled by the native regexp code.
  ExternalReference address_of_static_offsets_vector =
      ExternalReference::address_of_static_offsets_vector(isolate);
  __ mov(r4, Operand(address_of_static_offsets_vector));

  // r3: number of capture registers
  // r4: offsets vector
  Label next_capture;
  // Capture register counter starts from number of capture registers and
  // counts down until wraping after zero.
  __ AddP(r2, last_match_info_elements,
          Operand(RegExpImpl::kFirstCaptureOffset - kHeapObjectTag -
                  kPointerSize));
  __ AddP(r4, Operand(-kIntSize));  // bias down for lwzu
  __ bind(&next_capture);
  // Read the value from the static offsets vector buffer.
  __ ly(r5, MemOperand(r4, kIntSize));
  __ lay(r4, MemOperand(r4, kIntSize));
  // Store the smi value in the last match info.
  __ SmiTag(r5);
  __ StoreP(r5, MemOperand(r2, kPointerSize));
  __ lay(r2, MemOperand(r2, kPointerSize));
  __ BranchOnCount(r3, &next_capture);

  // Return last match info.
  __ LoadP(r2, MemOperand(fp, kLastMatchInfoOffset));
  __ LoadMultipleP(r3, sp, MemOperand(sp, 0));
  __ la(sp, MemOperand(sp, 13 * kPointerSize));
  __ la(sp, MemOperand(sp, (4 * kPointerSize)));
  __ Ret();

  // External string.  Short external strings have already been ruled out.
  // r2: scratch
  __ bind(&external_string);
  __ LoadP(r2, FieldMemOperand(subject, HeapObject::kMapOffset));
  __ LoadlB(r2, FieldMemOperand(r2, Map::kInstanceTypeOffset));
  if (FLAG_debug_code) {
    // Assert that we do not have a cons or slice (indirect strings) here.
    // Sequential strings have already been ruled out.
    STATIC_ASSERT(kIsIndirectStringMask == 1);
    __ tmll(r2, Operand(kIsIndirectStringMask));
    __ Assert(eq, "external string expected, but not found", cr0);
  }
  __ LoadP(subject,
         FieldMemOperand(subject, ExternalString::kResourceDataOffset));
  // Move the pointer so that offset-wise, it looks like a sequential string.
  STATIC_ASSERT(SeqTwoByteString::kHeaderSize == SeqAsciiString::kHeaderSize);
  __ SubP(subject,
         subject,
         Operand(SeqTwoByteString::kHeaderSize - kHeapObjectTag));
  __ b(&seq_string);

  // Do the runtime call to execute the regexp.
  __ bind(&runtime);

  __ LoadMultipleP(r3, sp, MemOperand(sp, 0));
  __ la(sp, MemOperand(sp, 13 * kPointerSize));

  __ TailCallRuntime(Runtime::kRegExpExec, 4, 1);
#endif  // V8_INTERPRETED_REGEXP
}


void RegExpConstructResultStub::Generate(MacroAssembler* masm) {
  const int kMaxInlineLength = 100;
  Label slowcase;
  Label done;
  Factory* factory = masm->isolate()->factory();

  __ LoadP(r3, MemOperand(sp, kPointerSize * 2));
  __ JumpIfNotSmi(r3, &slowcase);
  __ CmpLogicalSmiLiteral(r3, Smi::FromInt(kMaxInlineLength), r0);
  __ bgt(&slowcase);
  // Allocate RegExpResult followed by FixedArray with size in ebx.
  // JSArray:   [Map][empty properties][Elements][Length-smi][index][input]
  // Elements:  [Map][Length][..elements..]
  // Size of JSArray with two in-object properties and the header of a
  // FixedArray.
  int objects_size =
      (JSRegExpResult::kSize + FixedArray::kHeaderSize) / kPointerSize;
  __ SmiUntag(r7, r3);
  __ AddP(r4, r7, Operand(objects_size));
  // Future optimization: defer tagging the result pointer for more
  // efficient 64-bit memory accesses (due to alignment requirements
  // on the memoperand offset).
  __ AllocateInNewSpace(
      r4,  // In: Size, in words.
      r2,  // Out: Start of allocation (tagged).
      r5,  // Scratch register.
      r6,  // Scratch register.
      &slowcase,
      static_cast<AllocationFlags>(TAG_OBJECT | SIZE_IN_WORDS));
  // r2: Start of allocated area, object-tagged.
  // r3: Number of elements in array, as smi.
  // r7: Number of elements, untagged.

  // Set JSArray map to global.regexp_result_map().
  // Set empty properties FixedArray.
  // Set elements to point to FixedArray allocated right after the JSArray.
  // Interleave operations for better latency.
  __ LoadP(r4, ContextOperand(cp, Context::GLOBAL_OBJECT_INDEX));
  __ AddP(r5, r2, Operand(JSRegExpResult::kSize));
  __ mov(r6, Operand(factory->empty_fixed_array()));
  __ LoadP(r4, FieldMemOperand(r4, GlobalObject::kNativeContextOffset));
  __ StoreP(r5, FieldMemOperand(r2, JSObject::kElementsOffset));
  __ LoadP(r4, ContextOperand(r4, Context::REGEXP_RESULT_MAP_INDEX));
  __ StoreP(r6, FieldMemOperand(r2, JSObject::kPropertiesOffset));
  __ StoreP(r4, FieldMemOperand(r2, HeapObject::kMapOffset));

  // Set input, index and length fields from arguments.
  __ LoadP(r3, MemOperand(sp, kPointerSize * 0));
  __ LoadP(r4, MemOperand(sp, kPointerSize * 1));
  __ LoadP(r8, MemOperand(sp, kPointerSize * 2));
  __ StoreP(r3, FieldMemOperand(r2, JSRegExpResult::kInputOffset));
  __ StoreP(r4, FieldMemOperand(r2, JSRegExpResult::kIndexOffset));
  __ StoreP(r8, FieldMemOperand(r2, JSArray::kLengthOffset));

  // Fill out the elements FixedArray.
  // r2: JSArray, tagged.
  // r5: FixedArray, tagged.
  // r7: Number of elements in array, untagged.

  // Set map.
  __ mov(r4, Operand(factory->fixed_array_map()));
  __ StoreP(r4, FieldMemOperand(r5, HeapObject::kMapOffset));
  // Set FixedArray length.
  __ SmiTag(r8, r7);
  __ StoreP(r8, FieldMemOperand(r5, FixedArray::kLengthOffset));
  // Fill contents of fixed-array with undefined.
  __ LoadRoot(r4, Heap::kUndefinedValueRootIndex);
  __ AddP(r5, Operand(FixedArray::kHeaderSize - kHeapObjectTag));
  // Fill fixed array elements with undefined.
  // r2: JSArray, tagged.
  // r4: undefined.
  // r5: Start of elements in FixedArray.
  // r7: Number of elements to fill.
  Label loop;
  __ CmpP(r7, Operand::Zero());
  __ bind(&loop);
  __ ble(&done);  // Jump if r7 is negative or zero.
  __ SubP(r7, Operand(1));
  __ ShiftLeftP(ip, r7, Operand(kPointerSizeLog2));
  __ StorePX(r4, MemOperand(ip, r5));
  __ CmpP(r7, Operand::Zero());
  __ b(&loop);

  __ bind(&done);
  __ la(sp, MemOperand(sp, (3 * kPointerSize)));
  __ Ret();

  __ bind(&slowcase);
  __ TailCallRuntime(Runtime::kRegExpConstructResult, 3, 1);
}


static void GenerateRecordCallTarget(MacroAssembler* masm) {
  // Cache the called function in a global property cell.  Cache states
  // are uninitialized, monomorphic (indicated by a JSFunction), and
  // megamorphic.
  // r3 : the function to call
  // r4 : cache cell for call target
  Label initialize, done;
  const Register scratch = r5;

  ASSERT_EQ(*TypeFeedbackCells::MegamorphicSentinel(masm->isolate()),
            masm->isolate()->heap()->undefined_value());
  ASSERT_EQ(*TypeFeedbackCells::UninitializedSentinel(masm->isolate()),
            masm->isolate()->heap()->the_hole_value());

  // Load the cache state into scratch.
  __ LoadP(scratch, FieldMemOperand(r4, JSGlobalPropertyCell::kValueOffset));

  // A monomorphic cache hit or an already megamorphic state: invoke the
  // function without changing the state.
  __ CmpP(scratch, r3);
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
  __ StoreP(ip, FieldMemOperand(r4, JSGlobalPropertyCell::kValueOffset));
  __ b(&done);

  // An uninitialized cache is patched with the function.
  __ bind(&initialize);
  __ StoreP(r3, FieldMemOperand(r4, JSGlobalPropertyCell::kValueOffset));
  // No need for a write barrier here - cells are rescanned.

  __ bind(&done);
}


void CallFunctionStub::Generate(MacroAssembler* masm) {
  // r3 : the function to call
  // r4 : cache cell for call target
  Label slow, non_function;

  // The receiver might implicitly be the global object. This is
  // indicated by passing the hole as the receiver to the call
  // function stub.
  if (ReceiverMightBeImplicit()) {
    Label call;
    // Get the receiver from the stack.
    // function, receiver [, arguments]
    __ LoadP(r6, MemOperand(sp, argc_ * kPointerSize), r0);
    // Call as function is indicated with the hole.
    __ CompareRoot(r6, Heap::kTheHoleValueRootIndex);
    __ bne(&call);
    // Patch the receiver on the stack with the global receiver object.
    __ LoadP(r5,
             MemOperand(cp, Context::SlotOffset(Context::GLOBAL_OBJECT_INDEX)));
    __ LoadP(r5, FieldMemOperand(r5, GlobalObject::kGlobalReceiverOffset));
    __ StoreP(r5, MemOperand(sp, argc_ * kPointerSize));
    __ bind(&call);
  }

  // Check that the function is really a JavaScript function.
  // r3: pushed function (to be verified)
  __ JumpIfSmi(r3, &non_function);
  // Get the map of the function object.
  __ CompareObjectType(r3, r5, r5, JS_FUNCTION_TYPE);
  __ bne(&slow);

  if (RecordCallTarget()) {
    GenerateRecordCallTarget(masm);
  }

  // Fast-case: Invoke the function now.
  // r3: pushed function
  ParameterCount actual(argc_);

  if (ReceiverMightBeImplicit()) {
    Label call_as_function;
    __ CompareRoot(r6, Heap::kTheHoleValueRootIndex);
    __ beq(&call_as_function);
    __ InvokeFunction(r3,
                      actual,
                      JUMP_FUNCTION,
                      NullCallWrapper(),
                      CALL_AS_METHOD);
    __ bind(&call_as_function);
  }
  __ InvokeFunction(r3,
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
    __ StoreP(ip, FieldMemOperand(r4, JSGlobalPropertyCell::kValueOffset));
  }
  // Check for function proxy.
  STATIC_ASSERT(JS_FUNCTION_PROXY_TYPE < 0xffffu);
  __ CmpP(r5, Operand(JS_FUNCTION_PROXY_TYPE));
  __ bne(&non_function);
  __ push(r3);  // put proxy as additional argument
  __ LoadImmP(r2, Operand(argc_ + 1));
  __ LoadImmP(r4, Operand::Zero());
  __ GetBuiltinEntry(r5, Builtins::CALL_FUNCTION_PROXY);
  __ SetCallKind(r7, CALL_AS_METHOD);
  {
    Handle<Code> adaptor =
      masm->isolate()->builtins()->ArgumentsAdaptorTrampoline();
    __ Jump(adaptor, RelocInfo::CODE_TARGET);
  }

  // CALL_NON_FUNCTION expects the non-function callee as receiver (instead
  // of the original receiver from the call site).
  __ bind(&non_function);
  __ StoreP(r3, MemOperand(sp, argc_ * kPointerSize));
  __ LoadImmP(r2, Operand(argc_));  // Set up the number of arguments.
  __ LoadImmP(r4, Operand::Zero());
  __ GetBuiltinEntry(r5, Builtins::CALL_NON_FUNCTION);
  __ SetCallKind(r7, CALL_AS_METHOD);
  __ Jump(masm->isolate()->builtins()->ArgumentsAdaptorTrampoline(),
          RelocInfo::CODE_TARGET);
}


void CallConstructStub::Generate(MacroAssembler* masm) {
  // r2 : number of arguments
  // r3 : the function to call
  // r4 : cache cell for call target
  Label slow, non_function_call;

  // Check that the function is not a smi.
  __ JumpIfSmi(r3, &non_function_call);
  // Check that the function is a JSFunction.
  __ CompareObjectType(r3, r5, r5, JS_FUNCTION_TYPE);
  __ bne(&slow);

  if (RecordCallTarget()) {
    GenerateRecordCallTarget(masm);
  }

  // Jump to the function-specific construct stub.
  __ LoadP(r4, FieldMemOperand(r3, JSFunction::kSharedFunctionInfoOffset));
  __ LoadP(r4, FieldMemOperand(r4,
           SharedFunctionInfo::kConstructStubOffset));
  __ AddP(ip, r4, Operand(Code::kHeaderSize - kHeapObjectTag));
  __ Jump(ip);

  // r2: number of arguments
  // r3: called object
  // r5: object type
  Label do_call;
  __ bind(&slow);
  STATIC_ASSERT(JS_FUNCTION_PROXY_TYPE < 0xffffu);
  __ CmpP(r5, Operand(JS_FUNCTION_PROXY_TYPE));
  __ bne(&non_function_call);
  __ GetBuiltinEntry(r5, Builtins::CALL_FUNCTION_PROXY_AS_CONSTRUCTOR);
  __ b(&do_call);

  __ bind(&non_function_call);
  __ GetBuiltinEntry(r5, Builtins::CALL_NON_FUNCTION_AS_CONSTRUCTOR);
  __ bind(&do_call);
  // Set expected number of arguments to zero (not changing r2).
  __ LoadImmP(r4, Operand::Zero());
  __ SetCallKind(r7, CALL_AS_METHOD);
  __ Jump(masm->isolate()->builtins()->ArgumentsAdaptorTrampoline(),
          RelocInfo::CODE_TARGET);
}


// Unfortunately you have to run without snapshots to see most of these
// names in the profile since most compare stubs end up in the snapshot.
void CompareStub::PrintName(StringStream* stream) {
  ASSERT((lhs_.is(r2) && rhs_.is(r3)) ||
         (lhs_.is(r3) && rhs_.is(r2)));
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
  stream->Add(lhs_.is(r2) ? "_r3" : "_r4");
  stream->Add(rhs_.is(r2) ? "_r3" : "_r4");
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
  ASSERT((lhs_.is(r2) && rhs_.is(r3)) ||
         (lhs_.is(r3) && rhs_.is(r2)));
  return ConditionField::encode(static_cast<unsigned>(cc_))
         | RegisterField::encode(lhs_.is(r2))
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
  __ mov(r0, Operand(kIsNotStringMask));
  __ AndP(r0, result_);
  __ bne(receiver_not_string_ /*, cr0*/);

  // If the index is non-smi trigger the non-smi case.
  __ JumpIfNotSmi(index_, &index_not_smi_);
  __ bind(&got_smi_index_);

  // Check for index out of range.
  __ LoadP(ip, FieldMemOperand(object_, String::kLengthOffset));
  __ CmpLogicalP(ip, index_);
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
  __ Move(index_, r2);
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
  __ Move(result_, r2);
  call_helper.AfterCall(masm);
  __ b(&exit_);

  __ Abort("Unexpected fallthrough from CharCodeAt slow case");
}


// -------------------------------------------------------------------------
// StringCharFromCodeGenerator

  void StringCharFromCodeGenerator::GenerateFast(MacroAssembler* masm) {
  // Fast case of Heap::LookupSingleCharacterStringFromCode.
  ASSERT(IsPowerOf2(String::kMaxAsciiCharCode + 1));
  __ LoadSmiLiteral(r0, Smi::FromInt(~String::kMaxAsciiCharCode));
  __ OrP(r0, Operand(kSmiTagMask));
  __ AndP(r0, code_);
  __ CmpP(r0, Operand::Zero());
  __ bne(&slow_case_);

  __ LoadRoot(result_, Heap::kSingleCharacterStringCacheRootIndex);
  // At this point code register contains smi tagged ASCII char code.
  __ LoadRR(r0, code_);
  __ SmiToPtrArrayOffset(code_, code_);
  __ AddP(result_, code_);
  __ LoadRR(code_, r0);
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
  __ Move(result_, r2);
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
    __ stc(scratch, MemOperand(dest));
    __ AddP(src, Operand(1));
    __ AddP(dest, Operand(1));
  } else {
    __ LoadLogicalHalfWordP(scratch, MemOperand(src));
    __ sth(scratch, MemOperand(dest));
    __ AddP(src, Operand(2));
    __ AddP(dest, Operand(2));
  }
  __ SubP(count, Operand(1));
  __ CmpP(count, Operand::Zero());
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
    __ mov(r0, Operand(kPointerAlignmentMask));
    __ AndP(r0, dest);
    __ Check(eq, "Destination of copy not aligned.", cr0);
  }

  // Nothing to do for zero characters.
  Label done;
  if (!ascii) {  // for non-ascii, double the length
    __ AddP(count, count);
  }
  __ CmpP(count, Operand(0, RelocInfo::NONE));
  __ beq(&done);

  // Assume that you cannot read (or write) unaligned.
  Label byte_loop;
  __ AddP(count, dest);
  Register limit = count;  // Read until src equals this.
  // Copy bytes from src to dst until dst hits limit.
  __ bind(&byte_loop);
  __ CmpP(dest, limit);
  __ bge(&done);
  __ LoadlB(scratch1, MemOperand(src));
  __ AddP(src, Operand(1));
  __ stc(scratch1, MemOperand(dest));
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
  __ SubP(scratch, c1, Operand(static_cast<intptr_t>('0')));
  __ CmpLogicalP(scratch, Operand(static_cast<intptr_t>('9' - '0')));
  __ bgt(&not_array_index);
  __ SubP(scratch, c2, Operand(static_cast<intptr_t>('0')));
  __ CmpLogicalP(scratch, Operand(static_cast<intptr_t>('9' - '0')));
  __ bgt(&not_array_index);

  // If check failed combine both characters into single halfword.
  // This is required by the contract of the method: code at the
  // not_found branch expects this combination in c1 register
#if __BYTE_ORDER == __BIG_ENDIAN
  __ ShiftLeftP(c1, c1, Operand(kBitsPerByte));
  __ OrP(c1, c2);
#else
  __ ShiftLeftP(r0, c2, Operand(kBitsPerByte));
  __ OrP(c1, r0);
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
  __ ShiftLeftP(c1, c1, Operand(kBitsPerByte));
  __ OrP(chars, c2);
#else
  __ ShiftLeftP(r0, c2, Operand(kBitsPerByte));
  __ OrP(chars, r0);
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
  __ SubP(mask, Operand(1));

  // Calculate untagged address of the first element of the symbol table.
  Register first_symbol_table_element = symbol_table;
  __ AddP(first_symbol_table_element, symbol_table,
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
      __ AddP(candidate, hash, Operand(SymbolTable::GetProbeOffset(i)));
    } else {
      __ LoadRR(candidate, hash);
    }

    __ AndP(candidate, mask);

    // Load the entry from the symble table.
    STATIC_ASSERT(SymbolTable::kEntrySize == 1);
    __ ShiftLeftP(scratch, candidate, Operand(kPointerSizeLog2));
    __ LoadP(candidate, MemOperand(scratch, first_symbol_table_element));

    // If entry is undefined no string with this hash can be found.
    Label is_string;
    __ CompareObjectType(candidate, scratch, scratch, ODDBALL_TYPE);
    __ bne(&is_string);

    __ CmpP(undefined, candidate);
    __ beq(not_found);
    // Must be the hole (deleted entry).
    if (FLAG_debug_code) {
      __ CompareRoot(candidate, Heap::kTheHoleValueRootIndex);
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
    __ CmpSmiLiteral(scratch, Smi::FromInt(2), r0);
    __ bne(&next_probe[i]);

    // Check if the two characters match.
    __ LoadLogicalHalfWordP(scratch, FieldMemOperand(candidate,
                                                  SeqAsciiString::kHeaderSize));
    __ CmpP(chars, scratch);
    __ beq(&found_in_symbol_table);
    __ bind(&next_probe[i]);
  }

  // No matching 2 character string found by probing.
  __ b(not_found);

  // Scratch register contains result when we fall through to here.
  Register result = candidate;
  __ bind(&found_in_symbol_table);
  __ LoadRR(r2, result);
}


void StringHelper::GenerateHashInit(MacroAssembler* masm,
                                    Register hash,
                                    Register character,
                                    Register scratch) {
  // hash = character + (character << 10);
  __ LoadRoot(hash, Heap::kHashSeedRootIndex);
  // Untag smi seed and add the character.
  __ SmiUntag(scratch, hash);
  __ AddP(hash, character, scratch);
  // hash += hash << 10;
  __ ShiftLeft(scratch, hash, Operand(10));
  __ AddP(hash, scratch);
  // hash ^= hash >> 6;
  __ ShiftRight(scratch, hash, Operand(6));
  __ XorP(hash, scratch);
}


void StringHelper::GenerateHashAddCharacter(MacroAssembler* masm,
                                            Register hash,
                                            Register character,
                                            Register scratch) {
  // hash += character;
  __ AddP(hash, character);
  // hash += hash << 10;
  __ ShiftLeft(scratch, hash, Operand(10));
  __ AddP(hash, scratch);
  // hash ^= hash >> 6;
  __ ShiftRight(scratch, hash, Operand(6));
  __ XorP(hash, scratch);
}


void StringHelper::GenerateHashGetHash(MacroAssembler* masm,
                                       Register hash,
                                       Register scratch) {
  // hash += hash << 3;
  __ ShiftLeft(scratch, hash, Operand(3));
  __ AddP(hash, scratch);
  // hash ^= hash >> 11;
  __ ShiftRight(scratch, hash, Operand(11));
  __ XorP(hash, scratch);
  // hash += hash << 15;
  __ ShiftLeft(scratch, hash, Operand(15));
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

  __ LoadP(r4, MemOperand(sp, kToOffset));
  __ LoadP(r5, MemOperand(sp, kFromOffset));

  // If either to or from had the smi tag bit set, then fail to generic runtime
  __ JumpIfNotSmi(r4, &runtime);
  __ JumpIfNotSmi(r5, &runtime);
  __ SmiUntag(r4);
  __ SmiUntag(r5);
  // Both r4 and r5 are untagged integers.

  // We want to bailout to runtime here if From is negative.
  __ blt(&runtime /*, cr0*/);  // From < 0.

  __ CmpLogicalP(r5, r4);
  __ bgt(&runtime);  // Fail if from > to.
  __ SubP(r4, r4, r5);

  // Make sure first argument is a string.
  __ LoadP(r2, MemOperand(sp, kStringOffset));
  __ JumpIfSmi(r2, &runtime);
  Condition is_string = masm->IsObjectStringType(r2, r3);
  __ b(NegateCondition(is_string), &runtime /*, cr0*/);

  // Short-cut for the case of trivial substring.
  Label return_r3;
  // r2: original string
  // r4: result string length
  __ LoadP(r6, FieldMemOperand(r2, String::kLengthOffset));
  __ SmiUntag(r0, r6);
  __ CmpLogicalP(r4, r0);
  // Return original string.
  __ beq(&return_r3);
  // Longer than original string's length or negative: unsafe arguments.
  __ bgt(&runtime);
  // Shorter than original string's length: an actual substring.

  // Deal with different string types: update the index if necessary
  // and put the underlying string into r7.
  // r2: original string
  // r3: instance type
  // r4: length
  // r5: from index (untagged)
  Label underlying_unpacked, sliced_string, seq_or_external_string;
  // If the string is not indirect, it can only be sequential or external.
  STATIC_ASSERT(kIsIndirectStringMask == (kSlicedStringTag & kConsStringTag));
  STATIC_ASSERT(kIsIndirectStringMask != 0);
  __ mov(r0, Operand(kIsIndirectStringMask));
  __ AndP(r0, r3);
  __ beq(&seq_or_external_string /*, cr0*/);

  __ mov(r0, Operand(kSlicedNotConsMask));
  __ AndP(r0, r3);
  __ bne(&sliced_string /*, cr0*/);
  // Cons string.  Check whether it is flat, then fetch first part.
  __ LoadP(r7, FieldMemOperand(r2, ConsString::kSecondOffset));
  __ CompareRoot(r7, Heap::kEmptyStringRootIndex);
  __ bne(&runtime);
  __ LoadP(r7, FieldMemOperand(r2, ConsString::kFirstOffset));
  // Update instance type.
  __ LoadP(r3, FieldMemOperand(r7, HeapObject::kMapOffset));
  __ LoadlB(r3, FieldMemOperand(r3, Map::kInstanceTypeOffset));
  __ b(&underlying_unpacked);

  __ bind(&sliced_string);
  // Sliced string.  Fetch parent and correct start index by offset.
  __ LoadP(r7, FieldMemOperand(r2, SlicedString::kParentOffset));
  __ LoadP(r6, FieldMemOperand(r2, SlicedString::kOffsetOffset));
  __ SmiUntag(r3, r6);
  __ AddP(r5, r3);  // Add offset to index.
  // Update instance type.
  __ LoadP(r3, FieldMemOperand(r7, HeapObject::kMapOffset));
  __ LoadlB(r3, FieldMemOperand(r3, Map::kInstanceTypeOffset));
  __ b(&underlying_unpacked);

  __ bind(&seq_or_external_string);
  // Sequential or external string.  Just move string to the expected register.
  __ LoadRR(r7, r2);

  __ bind(&underlying_unpacked);

  if (FLAG_string_slices) {
    Label copy_routine;
    // r7: underlying subject string
    // r3: instance type of underlying subject string
    // r4: length
    // r5: adjusted start index (untagged)
    __ CmpP(r4, Operand(SlicedString::kMinLength));
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
    __ mov(r0, Operand(kStringEncodingMask));
    __ AndP(r0, r3);
    __ beq(&two_byte_slice /*, cr0*/);
    __ AllocateAsciiSlicedString(r2, r4, r8, r9, &runtime);
    __ b(&set_slice_header);
    __ bind(&two_byte_slice);
    __ AllocateTwoByteSlicedString(r2, r4, r8, r9, &runtime);
    __ bind(&set_slice_header);
    __ SmiTag(r5);
    __ StoreP(r7, FieldMemOperand(r2, SlicedString::kParentOffset));
    __ StoreP(r5, FieldMemOperand(r2, SlicedString::kOffsetOffset));
    __ b(&return_r3);

    __ bind(&copy_routine);
  }

  // r7: underlying subject string
  // r3: instance type of underlying subject string
  // r4: length
  // r5: adjusted start index (untagged)
  Label two_byte_sequential, sequential_string, allocate_result;
  STATIC_ASSERT(kExternalStringTag != 0);
  STATIC_ASSERT(kSeqStringTag == 0);
  __ mov(r0, Operand(kExternalStringTag));
  __ AndP(r0, r3);
  __ beq(&sequential_string /*, cr0*/);

  // Handle external string.
  // Rule out short external strings.
  STATIC_CHECK(kShortExternalStringTag != 0);
  __ mov(r0, Operand(kShortExternalStringTag));
  __ AndP(r0, r3);
  __ bne(&runtime /*, cr0*/);
  __ LoadP(r7, FieldMemOperand(r7, ExternalString::kResourceDataOffset));
  // r7 already points to the first character of underlying string.
  __ b(&allocate_result);

  __ bind(&sequential_string);
  // Locate first character of underlying subject string.
  STATIC_ASSERT(SeqTwoByteString::kHeaderSize == SeqAsciiString::kHeaderSize);
  __ AddP(r7, Operand(SeqAsciiString::kHeaderSize - kHeapObjectTag));

  __ bind(&allocate_result);
  // Sequential acii string.  Allocate the result.
  STATIC_ASSERT((kAsciiStringTag & kStringEncodingMask) != 0);
  __ mov(r0, Operand(kStringEncodingMask));
  __ AndP(r0, r3);
  __ beq(&two_byte_sequential /*, cr0*/);

  // Allocate and copy the resulting ASCII string.
  __ AllocateAsciiString(r2, r4, r6, r8, r9, &runtime);

  // Locate first character of substring to copy.
  __ AddP(r7, r5);
  // Locate first character of result.
  __ AddP(r3, r2, Operand(SeqAsciiString::kHeaderSize - kHeapObjectTag));

  // r2: result string
  // r3: first character of result string
  // r4: result string length
  // r7: first character of substring to copy
  STATIC_ASSERT((SeqAsciiString::kHeaderSize & kObjectAlignmentMask) == 0);
  StringHelper::GenerateCopyCharactersLong(masm, r3, r7, r4, r5, r6,
      r8, r9, r1, COPY_ASCII | DEST_ALWAYS_ALIGNED);
  __ b(&return_r3);

  // Allocate and copy the resulting two-byte string.
  __ bind(&two_byte_sequential);
  __ AllocateTwoByteString(r2, r4, r6, r8, r9, &runtime);

  // Locate first character of substring to copy.
  __ ShiftLeftP(r3, r5, Operand(1));
  __ AddP(r7, r3);
  // Locate first character of result.
  __ AddP(r3, r2, Operand(SeqTwoByteString::kHeaderSize - kHeapObjectTag));

  // r2: result string.
  // r3: first character of result.
  // r4: result length.
  // r7: first character of substring to copy.
  STATIC_ASSERT((SeqTwoByteString::kHeaderSize & kObjectAlignmentMask) == 0);
  StringHelper::GenerateCopyCharactersLong(
      masm, r3, r7, r4, r5, r6, r8, r9, r1,
      DEST_ALWAYS_ALIGNED);

  __ bind(&return_r3);
  Counters* counters = masm->isolate()->counters();
  __ IncrementCounter(counters->sub_string_native(), 1, r5, r6);
  __ la(sp, MemOperand(sp, (3 * kPointerSize)));
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
  __ CmpP(length, scratch2);
  __ beq(&check_zero_length);
  __ bind(&strings_not_equal);
  __ LoadSmiLiteral(r2, Smi::FromInt(NOT_EQUAL));
  __ Ret();

  // Check if the length is zero.
  Label compare_chars;
  __ bind(&check_zero_length);
  STATIC_ASSERT(kSmiTag == 0);
  __ CmpP(length, Operand::Zero());
  __ bne(&compare_chars);
  __ LoadSmiLiteral(r2, Smi::FromInt(EQUAL));
  __ Ret();

  // Compare characters.
  __ bind(&compare_chars);
  GenerateAsciiCharsCompareLoop(masm,
                                left, right, length, scratch2,
                                &strings_not_equal);

  // Characters are equal.
  __ LoadSmiLiteral(r2, Smi::FromInt(EQUAL));
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
  __ SubP(scratch3, scratch1, scratch2/*, LeaveOE, SetRC*/);
  // Removing RC looks okay here.
  Register length_delta = scratch3;
  __ ble(&skip /*, cr0*/);
  __ LoadRR(scratch1, scratch2);
  __ bind(&skip);
  Register min_length = scratch1;
  STATIC_ASSERT(kSmiTag == 0);
  __ CmpP(min_length, Operand::Zero());
  __ beq(&compare_lengths);

  // Compare loop.
  GenerateAsciiCharsCompareLoop(masm,
                                left, right, min_length, scratch2,
                                &result_not_equal);

  // Compare lengths - strings up to min-length are equal.
  __ bind(&compare_lengths);
  ASSERT(Smi::FromInt(EQUAL) == static_cast<Smi*>(0));
  // Use length_delta as result if it's zero.
  __ LoadRR(r2, length_delta);
  __ CmpP(length_delta, Operand::Zero());
  __ bind(&result_not_equal);
  // Conditionally update the result based either on length_delta or
  // the last comparion performed in the loop above.
  Label less_equal, equal;
  __ ble(&less_equal);
  __ LoadSmiLiteral(r2, Smi::FromInt(GREATER));
  __ Ret();
  __ bind(&less_equal);
  __ beq(&equal);
  __ LoadSmiLiteral(r2, Smi::FromInt(LESS));
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
  __ AddP(scratch1, length,
                    Operand(SeqAsciiString::kHeaderSize - kHeapObjectTag));
  __ AddP(left, scratch1);
  __ AddP(right, scratch1);
  __ LoadComplementRR(length, length);
  Register index = length;  // index = -length;

  // Compare loop.
  Label loop;
  __ bind(&loop);
  __ LoadlB(scratch1, MemOperand(left, index));
  __ LoadlB(r0, MemOperand(right, index));
  __ CmpP(scratch1, r0);
  __ bne(chars_not_equal);
  __ AddP(index, Operand(1));
  __ CmpP(index, Operand::Zero());
  __ bne(&loop);
}


void StringCompareStub::Generate(MacroAssembler* masm) {
  Label runtime;

  Counters* counters = masm->isolate()->counters();

  // Stack frame on entry.
  //  sp[0]: right string
  //  sp[4]: left string
  __ LoadP(r2, MemOperand(sp));  // Load right in r2, left in r3.
  __ LoadP(r3, MemOperand(sp, kPointerSize));

  Label not_same;
  __ CmpP(r2, r3);
  __ bne(&not_same);
  STATIC_ASSERT(EQUAL == 0);
  STATIC_ASSERT(kSmiTag == 0);
  __ LoadSmiLiteral(r2, Smi::FromInt(EQUAL));
  __ IncrementCounter(counters->string_compare_native(), 1, r3, r4);
  __ la(sp, MemOperand(sp, (2 * kPointerSize)));
  __ Ret();

  __ bind(&not_same);

  // Check that both objects are sequential ASCII strings.
  __ JumpIfNotBothSequentialAsciiStrings(r3, r2, r4, r5, &runtime);

  // Compare flat ASCII strings natively. Remove arguments from stack first.
  __ IncrementCounter(counters->string_compare_native(), 1, r4, r5);
  __ la(sp, MemOperand(sp, (2 * kPointerSize)));
  GenerateCompareFlatAsciiStrings(masm, r3, r2, r4, r5, r6);

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
  __ LoadP(r2, MemOperand(sp, 1 * kPointerSize));  // First argument.
  __ LoadP(r3, MemOperand(sp, 0 * kPointerSize));  // Second argument.

  // Make sure that both arguments are strings if not known in advance.
  if (flags_ == NO_STRING_ADD_FLAGS) {
    __ JumpIfEitherSmi(r2, r3, &call_runtime);
    // Load instance types.
    __ LoadP(r6, FieldMemOperand(r2, HeapObject::kMapOffset));
    __ LoadP(r7, FieldMemOperand(r3, HeapObject::kMapOffset));
    __ LoadlB(r6, FieldMemOperand(r6, Map::kInstanceTypeOffset));
    __ LoadlB(r7, FieldMemOperand(r7, Map::kInstanceTypeOffset));
    STATIC_ASSERT(kStringTag == 0);
    // If either is not a string, go to runtime.
    __ AndP(r0, r6, Operand(kIsNotStringMask));
    __ bne(&call_runtime /*, cr0*/);
    __ AndP(r0, r7, Operand(kIsNotStringMask));
    __ bne(&call_runtime /*, cr0*/);
  } else {
    // Here at least one of the arguments is definitely a string.
    // We convert the one that is not known to be a string.
    if ((flags_ & NO_STRING_CHECK_LEFT_IN_STUB) == 0) {
      ASSERT((flags_ & NO_STRING_CHECK_RIGHT_IN_STUB) != 0);
      GenerateConvertArgument(
          masm, 1 * kPointerSize, r2, r4, r5, r6, r7, &call_builtin);
      builtin_id = Builtins::STRING_ADD_RIGHT;
    } else if ((flags_ & NO_STRING_CHECK_RIGHT_IN_STUB) == 0) {
      ASSERT((flags_ & NO_STRING_CHECK_LEFT_IN_STUB) != 0);
      GenerateConvertArgument(
          masm, 0 * kPointerSize, r3, r4, r5, r6, r7, &call_builtin);
      builtin_id = Builtins::STRING_ADD_LEFT;
    }
  }

  // Both arguments are strings.
  // r2: first string
  // r3: second string
  // r6: first string instance type (if flags_ == NO_STRING_ADD_FLAGS)
  // r7: second string instance type (if flags_ == NO_STRING_ADD_FLAGS)
  {
    Label return_r2_string, strings_not_empty;
    // Check if either of the strings are empty. In that case return the other.
    STATIC_ASSERT(kSmiTag == 0);

    // Test second string first, as if is empty, we can jump to return
    // as r2 already has the first string.
    __ LoadAndTestP(r5, FieldMemOperand(r3, String::kLengthOffset));
    __ beq(&return_r2_string, Label::kNear);

    // Else test First String
    __ LoadAndTestP(r4, FieldMemOperand(r2, String::kLengthOffset));
    __ bne(&strings_not_empty, Label::kNear);

    // If first is empty, return second.
    __ LoadRR(r2, r3);

    // Return the string in r2
    __ bind(&return_r2_string);
    __ IncrementCounter(counters->string_add_native(), 1, r4, r5);
    __ la(sp, MemOperand(sp, (2 * kPointerSize)));
    __ Ret();

    __ bind(&strings_not_empty);
  }

  __ SmiUntag(r4);
  __ SmiUntag(r5);
  // Both strings are non-empty.
  // r2: first string
  // r3: second string
  // r4: length of first string
  // r5: length of second string
  // r6: first string instance type (if flags_ == NO_STRING_ADD_FLAGS)
  // r7: second string instance type (if flags_ == NO_STRING_ADD_FLAGS)
  // Look at the length of the result of adding the two strings.
  Label string_add_flat_result, longer_than_two;
  // Adding two lengths can't overflow.
  STATIC_ASSERT(String::kMaxLength < String::kMaxLength * 2);
  __ AddP(r8, r4, r5);
  // Use the symbol table when adding two one character strings, as it
  // helps later optimizations to return a symbol here.
  __ CmpP(r8, Operand(2));
  __ bne(&longer_than_two);

  // Check that both strings are non-external ASCII strings.
  if (flags_ != NO_STRING_ADD_FLAGS) {
    __ LoadP(r6, FieldMemOperand(r2, HeapObject::kMapOffset));
    __ LoadP(r7, FieldMemOperand(r3, HeapObject::kMapOffset));
    __ LoadlB(r6, FieldMemOperand(r6, Map::kInstanceTypeOffset));
    __ LoadlB(r7, FieldMemOperand(r7, Map::kInstanceTypeOffset));
  }
  __ JumpIfBothInstanceTypesAreNotSequentialAscii(r6, r7, r8, r9,
                                                  &call_runtime);

  // Get the two characters forming the sub string.
  __ LoadlB(r4, FieldMemOperand(r2, SeqAsciiString::kHeaderSize));
  __ LoadlB(r5, FieldMemOperand(r3, SeqAsciiString::kHeaderSize));

  // Try to lookup two character string in symbol table. If it is not found
  // just allocate a new one.
  Label make_two_character_string;
  StringHelper::GenerateTwoCharacterSymbolTableProbe(
      masm, r4, r5, r8, r9, r6, r7, r1,
      &make_two_character_string);
  __ IncrementCounter(counters->string_add_native(), 1, r4, r5);
  __ la(sp, MemOperand(sp, (2 * kPointerSize)));
  __ Ret();

  __ bind(&make_two_character_string);
  // Resulting string has length 2 and first chars of two strings
  // are combined into single halfword in r4 register.
  // So we can fill resulting string without two loops by a single
  // halfword store instruction
  __ LoadImmP(r8, Operand(2));
  __ AllocateAsciiString(r2, r8, r6, r7, r1, &call_runtime);
  __ sth(r4, FieldMemOperand(r2, SeqAsciiString::kHeaderSize));
  __ IncrementCounter(counters->string_add_native(), 1, r4, r5);
  __ la(sp, MemOperand(sp, (2 * kPointerSize)));
  __ Ret();

  __ bind(&longer_than_two);
  // Check if resulting string will be flat.
  __ CmpP(r8, Operand(ConsString::kMinLength));
  __ blt(&string_add_flat_result);
  // Handle exceptionally long strings in the runtime system.
  STATIC_ASSERT((String::kMaxLength & 0x80000000) == 0);
  ASSERT(IsPowerOf2(String::kMaxLength + 1));
  // kMaxLength + 1 is representable as shifted literal, kMaxLength is not.
  __ mov(r9, Operand(String::kMaxLength + 1));
  __ CmpLogicalP(r8, r9);
  __ bge(&call_runtime);

  // If result is not supposed to be flat, allocate a cons string object.
  // If both strings are ASCII the result is an ASCII cons string.
  if (flags_ != NO_STRING_ADD_FLAGS) {
    __ LoadP(r6, FieldMemOperand(r2, HeapObject::kMapOffset));
    __ LoadP(r7, FieldMemOperand(r3, HeapObject::kMapOffset));
    __ LoadlB(r6, FieldMemOperand(r6, Map::kInstanceTypeOffset));
    __ LoadlB(r7, FieldMemOperand(r7, Map::kInstanceTypeOffset));
  }
  Label non_ascii, allocated, ascii_data;
  STATIC_ASSERT(kTwoByteStringTag == 0);
  __ AndP(r0, r6, Operand(kStringEncodingMask));
  __ beq(&non_ascii /*, cr0*/);
  __ AndP(r0, r7, Operand(kStringEncodingMask));
  __ beq(&non_ascii /*, cr0*/);

  // Allocate an ASCII cons string.
  __ bind(&ascii_data);
  __ AllocateAsciiConsString(r9, r8, r6, r7, &call_runtime);
  __ bind(&allocated);
  // Fill the fields of the cons string.
  __ StoreP(r2, FieldMemOperand(r9, ConsString::kFirstOffset));
  __ StoreP(r3, FieldMemOperand(r9, ConsString::kSecondOffset));
  __ LoadRR(r2, r9);
  __ IncrementCounter(counters->string_add_native(), 1, r4, r5);
  __ lay(sp, MemOperand(sp, 2 * kPointerSize));
  __ Ret();

  __ bind(&non_ascii);
  // At least one of the strings is two-byte. Check whether it happens
  // to contain only ASCII characters.
  // r6: first instance type.
  // r7: second instance type.
  __ AndP(r0, r6, Operand(kAsciiDataHintMask));
  __ bne(&ascii_data /*, cr0*/);
  __ AndP(r0, r7, Operand(kAsciiDataHintMask));
  __ bne(&ascii_data /*, cr0*/);
  __ XorP(r6, r7);
  STATIC_ASSERT(kAsciiStringTag != 0 && kAsciiDataHintTag != 0);
  __ AndP(r6, Operand(kAsciiStringTag | kAsciiDataHintTag));
  __ CmpP(r6, Operand(kAsciiStringTag | kAsciiDataHintTag));
  __ beq(&ascii_data);

  // Allocate a two byte cons string.
  __ AllocateTwoByteConsString(r9, r8, r6, r7, &call_runtime);
  __ b(&allocated);

  // We cannot encounter sliced strings or cons strings here since:
  STATIC_ASSERT(SlicedString::kMinLength >= ConsString::kMinLength);
  // Handle creating a flat result from either external or sequential strings.
  // Locate the first characters' locations.
  // r2: first string
  // r3: second string
  // r4: length of first string
  // r5: length of second string
  // r6: first string instance type (if flags_ == NO_STRING_ADD_FLAGS)
  // r7: second string instance type (if flags_ == NO_STRING_ADD_FLAGS)
  // r8: sum of lengths.
  Label first_prepared, second_prepared, external_string1, external_string2;
  __ bind(&string_add_flat_result);
  if (flags_ != NO_STRING_ADD_FLAGS) {
    __ LoadP(r6, FieldMemOperand(r2, HeapObject::kMapOffset));
    __ LoadP(r7, FieldMemOperand(r3, HeapObject::kMapOffset));
    __ LoadlB(r6, FieldMemOperand(r6, Map::kInstanceTypeOffset));
    __ LoadlB(r7, FieldMemOperand(r7, Map::kInstanceTypeOffset));
  }

  // Check whether both strings have same encoding
  __ XorP(r9, r6, r7);
  __ AndP(r0, r9, Operand(kStringEncodingMask));
  __ bne(&call_runtime /*, cr0*/);
  // TODO(JOHN): might be a problem here b/c addi didn't set RC

  STATIC_ASSERT(kSeqStringTag == 0);
  __ AndP(r0, r6, Operand(kStringRepresentationMask));
  __ bne(&external_string1 /*, cr0*/);
  STATIC_ASSERT(SeqAsciiString::kHeaderSize == SeqTwoByteString::kHeaderSize);
  __ AddP(r9, r2, Operand(SeqAsciiString::kHeaderSize - kHeapObjectTag));
  __ b(&first_prepared);
  // External string: rule out short external string and load string resource.
  STATIC_ASSERT(kShortExternalStringTag != 0);
  __ bind(&external_string1);
  __ AndP(r0, r6, Operand(kShortExternalStringMask));
  __ bne(&call_runtime /*, cr0*/);
  __ LoadP(r9, FieldMemOperand(r2, ExternalString::kResourceDataOffset));
  __ bind(&first_prepared);

  STATIC_ASSERT(kSeqStringTag == 0);
  __ AndP(r0, r7, Operand(kStringRepresentationMask));
  __ bne(&external_string2 /*, cr0*/);
  STATIC_ASSERT(SeqAsciiString::kHeaderSize == SeqTwoByteString::kHeaderSize);
  __ AddP(r3, Operand(SeqAsciiString::kHeaderSize - kHeapObjectTag));
  __ b(&second_prepared);
  // External string: rule out short external string and load string resource.
  STATIC_ASSERT(kShortExternalStringTag != 0);
  __ bind(&external_string2);
  __ AndP(r0, r7, Operand(kShortExternalStringMask));
  __ bne(&call_runtime /*, cr0*/);
  __ LoadP(r3, FieldMemOperand(r3, ExternalString::kResourceDataOffset));
  __ bind(&second_prepared);

  Label non_ascii_string_add_flat_result;
  // r9: first character of first string
  // r3: first character of second string
  // r4: length of first string.
  // r5: length of second string.
  // r8: sum of lengths.
  // Both strings have the same encoding.
  STATIC_ASSERT(kTwoByteStringTag == 0);
  __ AndP(r0, r7, Operand(kStringEncodingMask));
  __ beq(&non_ascii_string_add_flat_result /*, cr0*/);

  __ AllocateAsciiString(r2, r8, r6, r7, r1, &call_runtime);
  __ AddP(r8, r2, Operand(SeqAsciiString::kHeaderSize - kHeapObjectTag));
  // r2: result string.
  // r9: first character of first string.
  // r3: first character of second string.
  // r4: length of first string.
  // r5: length of second string.
  // r8: first character of result.
  StringHelper::GenerateCopyCharacters(masm, r8, r9, r4, r6, true);
  // r8: next character of result.
  StringHelper::GenerateCopyCharacters(masm, r8, r3, r5, r6, true);
  __ IncrementCounter(counters->string_add_native(), 1, r4, r5);
  __ la(sp, MemOperand(sp, (2 * kPointerSize)));
  __ Ret();

  __ bind(&non_ascii_string_add_flat_result);
  __ AllocateTwoByteString(r2, r8, r6, r7, r1, &call_runtime);
  __ AddP(r8, r2, Operand(SeqTwoByteString::kHeaderSize - kHeapObjectTag));
  // r2: result string.
  // r9: first character of first string.
  // r3: first character of second string.
  // r4: length of first string.
  // r5: length of second string.
  // r8: first character of result.
  StringHelper::GenerateCopyCharacters(masm, r8, r9, r4, r6, false);
  // r8: next character of result.
  StringHelper::GenerateCopyCharacters(masm, r8, r3, r5, r6, false);
  __ IncrementCounter(counters->string_add_native(), 1, r4, r5);
  __ la(sp, MemOperand(sp, (2 * kPointerSize)));
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
  __ CmpP(scratch2,
         Operand(1 << Map::kStringWrapperSafeForDefaultValueOf));
  __ bne(slow);
  __ LoadP(arg, FieldMemOperand(arg, JSValue::kValueOffset));
  __ StoreP(arg, MemOperand(sp, stack_offset));

  __ bind(&done);
}


void ICCompareStub::GenerateSmis(MacroAssembler* masm) {
  ASSERT(state_ == CompareIC::SMIS);
  Label miss, done;
  __ OrP(r4, r3, r2);
  __ JumpIfNotSmi(r4, &miss);

  if (GetCondition() == eq) {
    // For equality we do not care about the sign of the result.
    // __ sub(r2, r2, r3, SetCC);
     __ SubP(r2, r2, r3);
  } else {
    // Untag before subtracting to avoid handling overflow.
    __ SmiUntag(r2);
    __ SmiUntag(r3);
    __ SubP(r2, r3, r2);
    __ bind(&done);
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

  __ AndP(r4, r3, r2);
  __ JumpIfSmi(r4, &generic_stub);

  __ CompareObjectType(r2, r4, r4, HEAP_NUMBER_TYPE);
  __ bne(&maybe_undefined1);
  __ CompareObjectType(r3, r4, r4, HEAP_NUMBER_TYPE);
  __ bne(&maybe_undefined2);

  // Inlining the double comparison and falling back to the general compare
  // stub if NaN is involved

  // Load left and right operand
  // likely we can combine the constants to remove the sub
  __ LoadF(d0, FieldMemOperand(r3, HeapNumber::kValueOffset));
  __ LoadF(d1, FieldMemOperand(r2, HeapNumber::kValueOffset));

  // Compare operands
  __ cdbr(d0, d1);

  // Don't base result on status bits when a NaN is involved.
  __ bunordered(&unordered);

  // Return a result of -1, 0, or 1, based on status bits.
  __ beq(&equal);
  __ blt(&less_than);
  //  assume greater than
  __ LoadImmP(r2, Operand(GREATER));
  __ Ret();
  __ bind(&equal);
  __ LoadImmP(r2, Operand(EQUAL));
  __ Ret();
  __ bind(&less_than);
  __ LoadImmP(r2, Operand(LESS));
  __ Ret();

  __ bind(&unordered);

  CompareStub stub(GetCondition(), strict(), NO_COMPARE_FLAGS, r3, r2);
  __ bind(&generic_stub);
  __ Jump(stub.GetCode(), RelocInfo::CODE_TARGET);

  __ bind(&maybe_undefined1);
  if (Token::IsOrderedRelationalCompareOp(op_)) {
    __ CompareRoot(r2, Heap::kUndefinedValueRootIndex);
    __ bne(&miss);
    __ CompareObjectType(r3, r4, r4, HEAP_NUMBER_TYPE);
    __ bne(&maybe_undefined2);
    __ b(&unordered);
  }

  __ bind(&maybe_undefined2);
  if (Token::IsOrderedRelationalCompareOp(op_)) {
    __ CompareRoot(r3, Heap::kUndefinedValueRootIndex);
    __ beq(&unordered);
  }

  __ bind(&miss);
  GenerateMiss(masm);
}


void ICCompareStub::GenerateSymbols(MacroAssembler* masm) {
  ASSERT(state_ == CompareIC::SYMBOLS);
  Label miss, not_equal;

  // Registers containing left and right operands respectively.
  Register left = r3;
  Register right = r2;
  Register tmp1 = r4;
  Register tmp2 = r5;

  // Check that both operands are heap objects.
  __ JumpIfEitherSmi(left, right, &miss);

  // Check that both operands are symbols.
  __ LoadP(tmp1, FieldMemOperand(left, HeapObject::kMapOffset));
  __ LoadP(tmp2, FieldMemOperand(right, HeapObject::kMapOffset));
  __ LoadlB(tmp1, FieldMemOperand(tmp1, Map::kInstanceTypeOffset));
  __ LoadlB(tmp2, FieldMemOperand(tmp2, Map::kInstanceTypeOffset));
  STATIC_ASSERT(kSymbolTag != 0);
  __ AndP(tmp1, tmp2);
  __ AndP(r0, tmp1, Operand(kIsSymbolMask));
  __ beq(&miss /*, cr0*/);

  // Symbols are compared by identity.
  __ CmpP(left, right);
  __ bne(&not_equal);
  // Make sure r2 is non-zero. At this point input operands are
  // guaranteed to be non-zero.
  ASSERT(right.is(r2));
  STATIC_ASSERT(EQUAL == 0);
  STATIC_ASSERT(kSmiTag == 0);
  __ LoadSmiLiteral(r2, Smi::FromInt(EQUAL));
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
  Register left = r3;
  Register right = r2;
  Register tmp1 = r4;
  Register tmp2 = r5;
  Register tmp3 = r6;
  Register tmp4 = r7;

  // Check that both operands are heap objects.
  __ JumpIfEitherSmi(left, right, &miss);

  // Check that both operands are strings. This leaves the instance
  // types loaded in tmp1 and tmp2.
  __ LoadP(tmp1, FieldMemOperand(left, HeapObject::kMapOffset));
  __ LoadP(tmp2, FieldMemOperand(right, HeapObject::kMapOffset));
  __ LoadlB(tmp1, FieldMemOperand(tmp1, Map::kInstanceTypeOffset));
  __ LoadlB(tmp2, FieldMemOperand(tmp2, Map::kInstanceTypeOffset));
  STATIC_ASSERT(kNotStringTag != 0);
  __ OrP(tmp3, tmp1, tmp2);
  __ AndP(r0, tmp3, Operand(kIsNotStringMask));
  __ bne(&miss /*, cr0*/);
  // TODO(JOHN): might be a problem b/c cr0 is not set

  // Fast check for identical strings.
  __ CmpP(left, right);
  STATIC_ASSERT(EQUAL == 0);
  STATIC_ASSERT(kSmiTag == 0);
  __ bne(&not_identical);
  __ LoadSmiLiteral(r2, Smi::FromInt(EQUAL));
  __ Ret();
  __ bind(&not_identical);

  // Handle not identical strings.

  // Check that both strings are symbols. If they are, we're done
  // because we already know they are not identical.
  if (equality) {
    ASSERT(GetCondition() == eq);
    STATIC_ASSERT(kSymbolTag != 0);
    __ AndP(tmp3, tmp2, tmp1);
    __ AndP(r0, tmp3, Operand(kIsSymbolMask));
    __ beq(&is_symbol /*, cr0*/);
    // Make sure r2 is non-zero. At this point input operands are
    // guaranteed to be non-zero.
    ASSERT(right.is(r2));
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
  __ AndP(r4, r3, r2);
  __ JumpIfSmi(r4, &miss);

  __ CompareObjectType(r2, r4, r4, JS_OBJECT_TYPE);
  __ bne(&miss);
  __ CompareObjectType(r3, r4, r4, JS_OBJECT_TYPE);
  __ bne(&miss);

  ASSERT(GetCondition() == eq);
  __ SubP(r2, r2, r3);
  __ Ret();

  __ bind(&miss);
  GenerateMiss(masm);
}


void ICCompareStub::GenerateKnownObjects(MacroAssembler* masm) {
  Label miss;
  __ AndP(r4, r3, r2);
  __ JumpIfSmi(r4, &miss);
  __ LoadP(r4, FieldMemOperand(r2, HeapObject::kMapOffset));
  __ LoadP(r5, FieldMemOperand(r3, HeapObject::kMapOffset));
  __ mov(r0, Operand(known_map_));
  __ CmpP(r4, r0);
  __ bne(&miss);
  __ mov(r0, Operand(known_map_));
  __ CmpP(r5, r0);
  __ bne(&miss);

  __ SubP(r2, r2, r3);
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
    __ Push(r3, r2);
    __ push(r14);
    __ Push(r3, r2);
    __ LoadSmiLiteral(ip, Smi::FromInt(op_));
    __ push(ip);
    __ CallExternalReference(miss, 3);
    // Compute the entry point of the rewritten stub.
    __ AddP(r4, r2, Operand(Code::kHeaderSize - kHeapObjectTag));
    // Restore registers.
    __ pop(r14);
    __ pop(r2);
    __ pop(r3);
  }

  __ Jump(r4);
}

// This stub is paired with DirectCEntryStub::GenerateCall
void DirectCEntryStub::Generate(MacroAssembler* masm) {
  // Retrieve return address
  __ LoadP(ip, MemOperand(sp, kStackFrameRASlot * kPointerSize));
  __ Jump(ip);
}


void DirectCEntryStub::GenerateCall(MacroAssembler* masm,
                                    ExternalReference function) {
  __ mov(r5, Operand(function));
  GenerateCall(masm, r5);
}


void DirectCEntryStub::GenerateCall(MacroAssembler* masm,
                                    Register target) {
#if !defined(USE_SIMULATOR) && (defined(_AIX))
  // Native AIX/PPC64 Linux use a function descriptor.
  __ LoadP(ToRegister(2), MemOperand(target, kPointerSize));  // TOC
  __ LoadP(target, MemOperand(target, 0));  // Instruction address
#endif

  __ mov(r14, Operand(reinterpret_cast<intptr_t>(GetCode().location()),
                     RelocInfo::CODE_TARGET));

  // Block the trampoline pool through the whole function to make sure the
  // number of generated instructions is constant.
  Assembler::BlockTrampolinePoolScope block_trampoline_pool(masm);

  // Push return address (accessible to GC through exit frame pc).
  Label start/*, here*/;
  Label return_addr;
  __ bind(&start);
  __ larl(r14, &return_addr);
  __ StoreP(r14, MemOperand(sp, kStackFrameRASlot * kPointerSize));

  __ b(target);  // Call the C++ function.
  __ bind(&return_addr);
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
    __ SubP(index, Operand(1));
    __ LoadSmiLiteral(ip, Smi::FromInt(name->Hash() +
                                       StringDictionary::GetProbeOffset(i)));
    __ AndP(index, ip);

    // Scale the index by multiplying by the entry size.
    ASSERT(StringDictionary::kEntrySize == 3);
    __ ShiftLeftP(ip, index, Operand(1));
    __ AddP(index, ip);  // index *= 3.

    Register entity_name = scratch0;
    // Having undefined at this place means the name is not contained.
    Register tmp = properties;
    __ SmiToPtrArrayOffset(ip, index);
    __ AddP(tmp, properties, ip);
    __ LoadP(entity_name, FieldMemOperand(tmp, kElementsStartOffset));

    ASSERT(!tmp.is(entity_name));
    __ CompareRoot(entity_name, Heap::kUndefinedValueRootIndex);
    __ beq(done);

    if (i != kInlinedProbes - 1) {
      // Load the hole ready for use below:
      __ LoadRoot(tmp, Heap::kTheHoleValueRootIndex);

      // Stop if found the property.
      __ CmpP(entity_name, Operand(Handle<String>(name)));
      __ beq(miss);

      Label the_hole;
      __ CmpP(entity_name, tmp);
      __ beq(&the_hole);

      // Check if the entry name is not a symbol.
      __ LoadP(entity_name, FieldMemOperand(entity_name,
                                            HeapObject::kMapOffset));
      __ LoadlB(entity_name,
              FieldMemOperand(entity_name, Map::kInstanceTypeOffset));
      __ AndP(r0, entity_name, Operand(kIsSymbolMask));
      __ beq(miss /*, cr0*/);

      __ bind(&the_hole);

      // Restore the properties.
      __ LoadP(properties,
             FieldMemOperand(receiver, JSObject::kPropertiesOffset));
    }
  }

  const int spill_mask =
      (r0.bit() | r8.bit() | r7.bit() | r6.bit() | r5.bit() |
       r4.bit() | r3.bit() | r2.bit());

  __ LoadRR(r0, r14);
  __ MultiPush(spill_mask);

  __ LoadP(r2, FieldMemOperand(receiver, JSObject::kPropertiesOffset));
  __ mov(r3, Operand(Handle<String>(name)));
  StringDictionaryLookupStub stub(NEGATIVE_LOOKUP);
  __ CallStub(&stub);
  __ CmpP(r2, Operand::Zero());

  __ MultiPop(spill_mask);  // MultiPop does not touch condition flags
  __ LoadRR(r14, r0);

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
  __ SubP(scratch1, Operand(1));

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
    __ srl(scratch2, Operand(String::kHashShift));
    __ AndP(scratch2, scratch1);

    // Scale the index by multiplying by the element size.
    ASSERT(StringDictionary::kEntrySize == 3);
    // scratch2 = scratch2 * 3.
    __ ShiftLeftP(ip, scratch2, Operand(1));
    __ AddP(scratch2, ip);

    // Check if the key is identical to the name.
    __ ShiftLeftP(ip, scratch2, Operand(kPointerSizeLog2));
    __ AddP(scratch2, elements, ip);
    __ LoadP(ip, FieldMemOperand(scratch2, kElementsStartOffset));
    __ CmpP(name, ip);
    __ beq(done);
  }

  const int spill_mask =
      (r0.bit() | r8.bit() | r7.bit() | r6.bit() |
       r5.bit() | r4.bit() | r3.bit() | r2.bit()) &
      ~(scratch1.bit() | scratch2.bit());

  __ LoadRR(r0, r14);
  __ MultiPush(spill_mask);
  if (name.is(r2)) {
    ASSERT(!elements.is(r3));
    __ LoadRR(r3, name);
    __ LoadRR(r2, elements);
  } else {
    __ LoadRR(r2, elements);
    __ LoadRR(r3, name);
  }
  StringDictionaryLookupStub stub(POSITIVE_LOOKUP);
  __ CallStub(&stub);
  __ LoadRR(r1, r2);
  __ LoadRR(scratch2, r4);
  __ MultiPop(spill_mask);
  __ LoadRR(r14, r0);

  __ CmpP(r1, Operand::Zero());
  __ bne(done);
  __ beq(miss);
}


void StringDictionaryLookupStub::Generate(MacroAssembler* masm) {
  // This stub overrides SometimesSetsUpAFrame() to return false.  That means
  // we cannot call anything that could cause a GC from this stub.
  // Registers:
  //  result: StringDictionary to probe
  //  r3: key
  //  : StringDictionary to probe.
  //  index_: will hold an index of entry if lookup is successful.
  //          might alias with result_.
  // Returns:
  //  result_ is zero if lookup failed, non zero otherwise.

  Register result = r2;
  Register dictionary = r2;
  Register key = r3;
  Register index = r4;
  Register mask = r5;
  Register hash = r6;
  Register undefined = r7;
  Register entry_key = r8;
  Register scratch = r8;

  Label in_dictionary, maybe_in_dictionary, not_in_dictionary;

  __ LoadP(mask, FieldMemOperand(dictionary, kCapacityOffset));
  __ SmiUntag(mask);
  __ SubP(mask, Operand(1));

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
      __ AddP(index, hash, Operand(
                  StringDictionary::GetProbeOffset(i) << String::kHashShift));
    } else {
      __ LoadRR(index, hash);
    }
    __ ShiftRight(r0, index, Operand(String::kHashShift));
    __ AndP(index, r0, mask);

    // Scale the index by multiplying by the entry size.
    ASSERT(StringDictionary::kEntrySize == 3);
    __ ShiftLeftP(scratch, index, Operand(1));
    __ AddP(index, scratch);  // index *= 3.

    ASSERT_EQ(kSmiTagSize, 1);
    __ ShiftLeftP(scratch, index, Operand(kPointerSizeLog2));
    __ AddP(index, dictionary, scratch);
    __ LoadP(entry_key, FieldMemOperand(index, kElementsStartOffset));

    // Having undefined at this place means the name is not contained.
    __ CmpP(entry_key, undefined);
    __ beq(&not_in_dictionary);

    // Stop if found the property.
    __ CmpP(entry_key, key);
    __ beq(&in_dictionary);

    if (i != kTotalProbes - 1 && mode_ == NEGATIVE_LOOKUP) {
      // Check if the entry name is not a symbol.
      __ LoadP(entry_key, FieldMemOperand(entry_key, HeapObject::kMapOffset));
      __ LoadlB(entry_key,
              FieldMemOperand(entry_key, Map::kInstanceTypeOffset));
      __ AndP(r0, entry_key, Operand(kIsSymbolMask));
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
  { REG(r8), REG(r6), REG(r9), EMIT_REMEMBERED_SET },
  { REG(r8), REG(r4), REG(r9), EMIT_REMEMBERED_SET },
  // Used in CompileArrayPushCall.
  // Also used in StoreIC::GenerateNormal via GenerateDictionaryStore.
  // Also used in KeyedStoreIC::GenerateGeneric.
  { REG(r5), REG(r6), REG(r7), EMIT_REMEMBERED_SET },
  // Used in CompileStoreGlobal.
  { REG(r6), REG(r3), REG(r4), OMIT_REMEMBERED_SET },
  // Used in StoreStubCompiler::CompileStoreField via GenerateStoreField.
  { REG(r3), REG(r4), REG(r5), EMIT_REMEMBERED_SET },
  { REG(r5), REG(r4), REG(r3), EMIT_REMEMBERED_SET },
  // Used in KeyedStoreStubCompiler::CompileStoreField via GenerateStoreField.
  { REG(r4), REG(r3), REG(r5), EMIT_REMEMBERED_SET },
  { REG(r5), REG(r3), REG(r4), EMIT_REMEMBERED_SET },
  // KeyedStoreStubCompiler::GenerateStoreFastElement.
  { REG(r5), REG(r4), REG(r6), EMIT_REMEMBERED_SET },
  { REG(r4), REG(r5), REG(r6), EMIT_REMEMBERED_SET },
  // ElementsTransitionGenerator::GenerateMapChangeElementTransition
  // and ElementsTransitionGenerator::GenerateSmiToDouble
  // and ElementsTransitionGenerator::GenerateDoubleToObject
  { REG(r4), REG(r5), REG(r1), EMIT_REMEMBERED_SET },
  { REG(r4), REG(r5), REG(r1), OMIT_REMEMBERED_SET },
  // ElementsTransitionGenerator::GenerateDoubleToObject
  { REG(r8), REG(r4), REG(r2), EMIT_REMEMBERED_SET },
  { REG(r4), REG(r8), REG(r1), EMIT_REMEMBERED_SET },
  // StoreArrayLiteralElementStub::Generate
  { REG(r7), REG(r2), REG(r8), EMIT_REMEMBERED_SET },
  // FastNewClosureStub::Generate
  { REG(r4), REG(r6), REG(r3), EMIT_REMEMBERED_SET },
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

  // The first two branch instructions are generated with labels so as to
  // get the offset fixed up correctly by the bind(Label*) call.  We patch
  // it back and forth between branch condition True and False
  // when we start and stop incremental heap marking.
  // See RecordWriteStub::Patch for details.

  // Clear the bit, branch on True for NOP action initially
  __ b(CC_NOP, &skip_to_incremental_noncompacting);
  __ b(CC_NOP, &skip_to_incremental_compacting);

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
      r2.is(regs_.address()) ? regs_.scratch0() : regs_.address();
  ASSERT(!address.is(regs_.object()));
  ASSERT(!address.is(r2));
  __ LoadRR(address, regs_.address());
  __ LoadRR(r2, regs_.object());
  if (mode == INCREMENTAL_COMPACTION) {
    __ LoadRR(r3, address);
  } else {
    ASSERT(mode == INCREMENTAL);
    __ LoadP(r3, MemOperand(address, 0));
  }
  __ mov(r4, Operand(ExternalReference::isolate_address()));

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
  __ AndP(regs_.scratch0(), regs_.object(), Operand(~Page::kPageAlignmentMask));
  __ LoadP(regs_.scratch1(),
         MemOperand(regs_.scratch0(),
                    MemoryChunk::kWriteBarrierCounterOffset));
  __ SubP(regs_.scratch1(), regs_.scratch1(), Operand(1));
  __ StoreP(regs_.scratch1(),
            MemOperand(regs_.scratch0(),
                       MemoryChunk::kWriteBarrierCounterOffset));
  __ CmpP(regs_.scratch1(), Operand::Zero());  // PPC, we could do better here
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
  //  -- r2    : element value to store
  //  -- r3    : array literal
  //  -- r4    : map of array literal
  //  -- r5    : element index as smi
  //  -- r6    : array literal index in function as smi
  // -----------------------------------

  Label element_done;
  Label double_elements;
  Label smi_element;
  Label slow_elements;
  Label fast_elements;

  __ CheckFastElements(r4, r7, &double_elements);
  // FAST_*_SMI_ELEMENTS or FAST_*_ELEMENTS
  __ JumpIfSmi(r2, &smi_element);
  __ CheckFastSmiElements(r4, r7, &fast_elements);

  // Store into the array literal requires a elements transition. Call into
  // the runtime.
  __ bind(&slow_elements);
  // call.
  __ Push(r3, r5, r2);
  __ LoadP(r7, MemOperand(fp, JavaScriptFrameConstants::kFunctionOffset));
  __ LoadP(r7, FieldMemOperand(r7, JSFunction::kLiteralsOffset));
  __ Push(r7, r6);
  __ TailCallRuntime(Runtime::kStoreArrayLiteralElement, 5, 1);

  // Array literal has ElementsKind of FAST_*_ELEMENTS and value is an object.
  __ bind(&fast_elements);
  __ LoadP(r7, FieldMemOperand(r3, JSObject::kElementsOffset));
  __ SmiToPtrArrayOffset(r8, r5);
  __ AddP(r8, r7);
  __ StoreP(r2, MemOperand(r8, FixedArray::kHeaderSize - kHeapObjectTag));
  __ lay(r8, MemOperand(r8, FixedArray::kHeaderSize - kHeapObjectTag));

  // Update the write barrier for the array store.
  __ RecordWrite(r7, r8, r2, kLRHasNotBeenSaved, kDontSaveFPRegs,
                 EMIT_REMEMBERED_SET, OMIT_SMI_CHECK);
  __ Ret();

  // Array literal has ElementsKind of FAST_*_SMI_ELEMENTS or FAST_*_ELEMENTS,
  // and value is Smi.
  __ bind(&smi_element);
  __ LoadP(r7, FieldMemOperand(r3, JSObject::kElementsOffset));
  __ SmiToPtrArrayOffset(r8, r5);
  __ AddP(r8, r7);
  __ StoreP(r2, FieldMemOperand(r8, FixedArray::kHeaderSize));
  __ Ret();

  // Array literal has ElementsKind of FAST_DOUBLE_ELEMENTS.
  __ bind(&double_elements);
  __ LoadP(r7, FieldMemOperand(r3, JSObject::kElementsOffset));
  __ StoreNumberToDoubleElements(r2, r5, r3,
                                 // Overwrites all regs after this.
                                 r7, r8, r9, r1, r4,
                                 &slow_elements);
  __ Ret();
}


void ProfileEntryHookStub::MaybeCallEntryHook(MacroAssembler* masm) {
  if (entry_hook_ != NULL) {
    ProfileEntryHookStub stub;
    __ push(r14);
    __ CallStub(&stub);
    __ pop(r14);
  }
}


void ProfileEntryHookStub::Generate(MacroAssembler* masm) {
  // The entry hook is a "push lr" instruction (LAY+ST/STG), followed by a call.

#if V8_TARGET_ARCH_S390X
  const int32_t kReturnAddressDistanceFromFunctionStart =
      Assembler::kCallTargetAddressOffset + 12;  // LAY + STG
#else
  const int32_t kReturnAddressDistanceFromFunctionStart =
      Assembler::kCallTargetAddressOffset + 10;  // LAY + ST
#endif

  // Save live volatile registers.
  __ CleanseP(r14);
  __ LoadRR(r2, r14);
  __ Push(r2, r7, r3);
  const int32_t kNumSavedRegs = 3;

  // Compute the function's address for the first argument.
  __ SubP(r2, Operand(kReturnAddressDistanceFromFunctionStart));

  // The caller's return address is above the saved temporaries.
  // Grab that for the second argument to the hook.
  __ lay(r3, MemOperand(sp, kNumSavedRegs * kPointerSize));

  // Align the stack if necessary.
  int frame_alignment = masm->ActivationFrameAlignment();
  if (frame_alignment > kPointerSize) {
    __ LoadRR(r7, sp);
    ASSERT(IsPowerOf2(frame_alignment));
    ASSERT(-frame_alignment == -8);
    __ ClearRightImm(sp, sp, Operand(3));
  }

#if !defined(USE_SIMULATOR)
  __ mov(ip, Operand(reinterpret_cast<intptr_t>(&entry_hook_)));
  __ LoadP(ip, MemOperand(ip));
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
  // zLinux ABI requires caller's frame to have sufficient space for callee
  // preserved register save area.  Simulator also follows the same convention
  // as it will whack the callee register save area.
  __ lay(sp, MemOperand(sp, -kCalleeRegisterSaveAreaSize -
                       kNumRequiredStackFrameSlots * kPointerSize));

  __ Call(ip);

  // zLinux ABI requires caller's frame to have sufficient space for callee
  // preserved regsiter save area.
  __ la(sp, MemOperand(sp, kCalleeRegisterSaveAreaSize +
                       kNumRequiredStackFrameSlots * kPointerSize));

  if (frame_alignment > kPointerSize) {
    __ LoadRR(sp, r7);
  }
  __ Pop(r14, r7, r3);
  __ Ret();
}

#undef __
} }  // namespace v8::internal

#endif  // V8_TARGET_ARCH_S390
