// Copyright 2012 the V8 project authors. All rights reserved.
//
// Copyright IBM Corp. 2012-2014. All rights reserved.
//
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.

#include "v8.h"

#if V8_TARGET_ARCH_S390

#include "codegen.h"
#include "macro-assembler.h"
#include "simulator-s390.h"

namespace v8 {
namespace internal {


#define __ masm.


#if defined(USE_SIMULATOR)
byte* fast_exp_s390_machine_code = NULL;
double fast_exp_simulator(double x) {
  return Simulator::current(Isolate::Current())->CallFPReturnsDouble(
      fast_exp_s390_machine_code, x, 0);
}
#endif


UnaryMathFunction CreateExpFunction() {
  if (!FLAG_fast_math) return &std::exp;
  size_t actual_size;
  byte* buffer = static_cast<byte*>(OS::Allocate(1 * KB, &actual_size, true));
  if (buffer == NULL) return &std::exp;
  ExternalReference::InitializeMathExpData();

  MacroAssembler masm(NULL, buffer, static_cast<int>(actual_size));

  {
    DoubleRegister input = d0;
    DoubleRegister result = d2;
    DoubleRegister double_scratch1 = d3;
    DoubleRegister double_scratch2 = d4;
    Register temp1 = r6;
    Register temp2 = r7;
    Register temp3 = r8;

  // Called from C
#if ABI_USES_FUNCTION_DESCRIPTORS
    __ function_descriptor();
#endif

    __ Push(temp3, temp2, temp1);
    MathExpGenerator::EmitMathExp(
        &masm, input, result, double_scratch1, double_scratch2,
        temp1, temp2, temp3);
    __ Pop(temp3, temp2, temp1);
    __ ldr(d0, result);
    __ Ret();
  }

  CodeDesc desc;
  masm.GetCode(&desc);
#if !ABI_USES_FUNCTION_DESCRIPTORS
  ASSERT(!RelocInfo::RequiresRelocation(desc));
#endif

  CPU::FlushICache(buffer, actual_size);
  OS::ProtectCode(buffer, actual_size);

#if !defined(USE_SIMULATOR)
  return FUNCTION_CAST<UnaryMathFunction>(buffer);
#else
  fast_exp_s390_machine_code = buffer;
  return &fast_exp_simulator;
#endif
}


UnaryMathFunction CreateSqrtFunction() {
#if defined(USE_SIMULATOR)
  return &std::sqrt;
#else
  size_t actual_size;
  byte* buffer = static_cast<byte*>(OS::Allocate(1 * KB, &actual_size, true));
  if (buffer == NULL) return &std::sqrt;

  MacroAssembler masm(NULL, buffer, static_cast<int>(actual_size));

  // Called from C
#if ABI_USES_FUNCTION_DESCRIPTORS
  __ function_descriptor();
#endif

  __ MovFromFloatParameter(d0);
  __ sqdbr(d0, d0);
  __ MovToFloatResult(d0);
  __ Ret();

  CodeDesc desc;
  masm.GetCode(&desc);
#if !ABI_USES_FUNCTION_DESCRIPTORS
  ASSERT(!RelocInfo::RequiresRelocation(desc));
#endif

  CPU::FlushICache(buffer, actual_size);
  OS::ProtectCode(buffer, actual_size);
  return FUNCTION_CAST<UnaryMathFunction>(buffer);
#endif
}

#undef __


// -------------------------------------------------------------------------
// Platform-specific RuntimeCallHelper functions.

void StubRuntimeCallHelper::BeforeCall(MacroAssembler* masm) const {
  masm->EnterFrame(StackFrame::INTERNAL);
  ASSERT(!masm->has_frame());
  masm->set_has_frame(true);
}


void StubRuntimeCallHelper::AfterCall(MacroAssembler* masm) const {
  masm->LeaveFrame(StackFrame::INTERNAL);
  ASSERT(masm->has_frame());
  masm->set_has_frame(false);
}


// -------------------------------------------------------------------------
// Code generators

#define __ ACCESS_MASM(masm)

void ElementsTransitionGenerator::GenerateMapChangeElementsTransition(
    MacroAssembler* masm, AllocationSiteMode mode,
    Label* allocation_memento_found) {
  // ----------- S t a t e -------------
  //  -- r2    : value
  //  -- r3    : key
  //  -- r4    : receiver
  //  -- r14     : return address
  //  -- r5    : target map, scratch for subsequent call
  //  -- r6    : scratch (elements)
  // -----------------------------------
  if (mode == TRACK_ALLOCATION_SITE) {
    ASSERT(allocation_memento_found != NULL);
    __ JumpIfJSArrayHasAllocationMemento(r4, r6, allocation_memento_found);
  }

  // Set transitioned map.
  __ StoreP(r5, FieldMemOperand(r4, HeapObject::kMapOffset));
  __ RecordWriteField(r4,
                      HeapObject::kMapOffset,
                      r5,
                      r13,
                      kLRHasNotBeenSaved,
                      kDontSaveFPRegs,
                      EMIT_REMEMBERED_SET,
                      OMIT_SMI_CHECK);
}


void ElementsTransitionGenerator::GenerateSmiToDouble(
    MacroAssembler* masm, AllocationSiteMode mode, Label* fail) {
  // ----------- S t a t e -------------
  //  -- r2    : value
  //  -- r3    : key
  //  -- r4    : receiver
  //  -- lr    : return address
  //  -- r5    : target map, scratch for subsequent call
  //  -- r6    : scratch (elements)
  // -----------------------------------
  Label loop, entry, convert_hole, gc_required, only_change_map, done;

  if (mode == TRACK_ALLOCATION_SITE) {
    __ JumpIfJSArrayHasAllocationMemento(r4, r6, fail);
  }

  // Check for empty arrays, which only require a map transition and no changes
  // to the backing store.
  __ LoadP(r6, FieldMemOperand(r4, JSObject::kElementsOffset));
  __ CompareRoot(r6, Heap::kEmptyFixedArrayRootIndex);
  __ beq(&only_change_map, Label::kNear);

  // Preserve lr and use r14 as a temporary register.
  __ push(r14);

  __ LoadP(r7, FieldMemOperand(r6, FixedArray::kLengthOffset));
  // r6: source FixedArray
  // r7: number of elements (smi-tagged)

  // Allocate new FixedDoubleArray.
  __ SmiToDoubleArrayOffset(r14, r7);
  __ AddP(r14, Operand(FixedDoubleArray::kHeaderSize));
  __ Allocate(r14, r8, r9, r13, &gc_required, DOUBLE_ALIGNMENT);
  // r8: destination FixedDoubleArray, not tagged as heap object.

   // Set destination FixedDoubleArray's length and map.
  __ LoadRoot(r13, Heap::kFixedDoubleArrayMapRootIndex);
  __ StoreP(r7, MemOperand(r8, FixedDoubleArray::kLengthOffset));
  // Update receiver's map.
  __ StoreP(r13, MemOperand(r8, HeapObject::kMapOffset));

  __ StoreP(r5, FieldMemOperand(r4, HeapObject::kMapOffset));
  __ RecordWriteField(r4,
                      HeapObject::kMapOffset,
                      r5,
                      r13,
                      kLRHasBeenSaved,
                      kDontSaveFPRegs,
                      OMIT_REMEMBERED_SET,
                      OMIT_SMI_CHECK);
  // Replace receiver's backing store with newly created FixedDoubleArray.
  __ AddP(r5, r8, Operand(kHeapObjectTag));
  __ StoreP(r5, FieldMemOperand(r4, JSObject::kElementsOffset));
  __ RecordWriteField(r4,
                      JSObject::kElementsOffset,
                      r5,
                      r13,
                      kLRHasBeenSaved,
                      kDontSaveFPRegs,
                      EMIT_REMEMBERED_SET,
                      OMIT_SMI_CHECK);

  // Prepare for conversion loop.
  __ AddP(r5, r6, Operand(FixedArray::kHeaderSize - kHeapObjectTag));
  __ AddP(r9, r8, Operand(FixedDoubleArray::kHeaderSize));
  __ SmiToDoubleArrayOffset(r8, r7);
  __ AddP(r8, r9);
#if V8_TARGET_ARCH_S390X
  __ mov(r6, Operand(kHoleNanInt64));
#else
  __ mov(r6, Operand(kHoleNanLower32));
  __ mov(r7, Operand(kHoleNanUpper32));
#endif
  // r5: begin of source FixedArray element fields, not tagged
  // r6: kHoleNanLower32
  // r7: kHoleNanUpper32
  // r8: end of destination FixedDoubleArray, not tagged
  // r9: begin of FixedDoubleArray element fields, not tagged

  __ b(&entry, Label::kNear);

  __ bind(&only_change_map);
  __ StoreP(r5, FieldMemOperand(r4, HeapObject::kMapOffset));
  __ RecordWriteField(r4,
                      HeapObject::kMapOffset,
                      r5,
                      r13,
                      kLRHasNotBeenSaved,
                      kDontSaveFPRegs,
                      OMIT_REMEMBERED_SET,
                      OMIT_SMI_CHECK);
  __ b(&done, Label::kNear);

  // Call into runtime if GC is required.
  __ bind(&gc_required);
  __ pop(r14);
  __ b(fail);

  // Convert and copy elements.
  __ bind(&loop);
  __ LoadP(r13, MemOperand(r5));
  __ AddP(r5, Operand(kPointerSize));
  // r13: current element
  __ UntagAndJumpIfNotSmi(r13, r13, &convert_hole);

  // Normal smi, convert to double and store.
  __ ConvertIntToDouble(r13, d0);
  __ StoreF(d0, MemOperand(r9, 0));
  __ la(r9, MemOperand(r9, 8));

  __ b(&entry, Label::kNear);

  // Hole found, store the-hole NaN.
  __ bind(&convert_hole);
  if (FLAG_debug_code) {
    // Restore a "smi-untagged" heap object.
    __ LoadP(r13, MemOperand(r5, -kPointerSize));
    __ CompareRoot(r13, Heap::kTheHoleValueRootIndex);
    __ Assert(eq, kObjectFoundInSmiOnlyArray);
  }
#if V8_TARGET_ARCH_S390X
  __ stg(r6, MemOperand(r9, 0));
#else
  // TODO (joransiu): Check if this works
  __ StoreW(r7, MemOperand(r9, Register::kExponentOffset));
  __ StoreW(r6, MemOperand(r9, Register::kMantissaOffset));
#endif
  __ AddP(r9, Operand(8));

  __ bind(&entry);
  __ CmpP(r9, r8);
  __ blt(&loop);

  __ pop(r14);
  __ bind(&done);
}


void ElementsTransitionGenerator::GenerateDoubleToObject(
    MacroAssembler* masm, AllocationSiteMode mode, Label* fail) {
  // ----------- S t a t e -------------
  //  -- r2    : value
  //  -- r3    : key
  //  -- r4    : receiver
  //  -- lr    : return address
  //  -- r5    : target map, scratch for subsequent call
  //  -- r6    : scratch (elements)
  // -----------------------------------
  Label entry, loop, convert_hole, gc_required, only_change_map;

  if (mode == TRACK_ALLOCATION_SITE) {
    __ JumpIfJSArrayHasAllocationMemento(r4, r6, fail);
  }

  // Check for empty arrays, which only require a map transition and no changes
  // to the backing store.
  __ LoadP(r6, FieldMemOperand(r4, JSObject::kElementsOffset));
  __ CompareRoot(r6, Heap::kEmptyFixedArrayRootIndex);
  __ beq(&only_change_map);

  __ Push(r5, r4, r3, r2);
  __ LoadP(r7, FieldMemOperand(r6, FixedArray::kLengthOffset));
  // r6: source FixedDoubleArray
  // r7: number of elements (smi-tagged)

  // Allocate new FixedArray.
  __ LoadImmP(r2, Operand(FixedDoubleArray::kHeaderSize));
  __ SmiToPtrArrayOffset(r0, r7);
  __ AddP(r2, r0);
  __ Allocate(r2, r8, r9, r13, &gc_required, NO_ALLOCATION_FLAGS);
  // r8: destination FixedArray, not tagged as heap object
  // Set destination FixedDoubleArray's length and map.
  __ LoadRoot(r13, Heap::kFixedArrayMapRootIndex);
  __ StoreP(r7, MemOperand(r8, FixedDoubleArray::kLengthOffset));
  __ StoreP(r13, MemOperand(r8, HeapObject::kMapOffset));

  // Prepare for conversion loop.
  __ AddP(r6, Operand(FixedDoubleArray::kHeaderSize - kHeapObjectTag));
  __ AddP(r5, r8, Operand(FixedArray::kHeaderSize));
  __ AddP(r8, Operand(kHeapObjectTag));
  __ SmiToPtrArrayOffset(r7, r7);
  __ AddP(r7, r5);
  __ LoadRoot(r9, Heap::kTheHoleValueRootIndex);
  __ LoadRoot(r13, Heap::kHeapNumberMapRootIndex);
  // Using offsetted addresses in r6 to fully take advantage of post-indexing.
  // r5: begin of destination FixedArray element fields, not tagged
  // r6: begin of source FixedDoubleArray element fields, not tagged
  // r7: end of destination FixedArray, not tagged
  // r8: destination FixedArray
  // r9: the-hole pointer
  // r13: heap number map
  __ b(&entry, Label::kNear);

  // Call into runtime if GC is required.
  __ bind(&gc_required);
  __ Pop(r5, r4, r3, r2);
  __ b(fail);

  __ bind(&loop);
  __ LoadlW(r3, MemOperand(r6, Register::kExponentOffset));
  __ AddP(r6, Operand(kDoubleSize));
  // r3: current element's upper 32 bit
  // r6: address of next element's upper 32 bit
  __ CmpP(r3, Operand(kHoleNanUpper32));
  __ beq(&convert_hole);

  // Non-hole double, copy value into a heap number.
   __ AllocateHeapNumber(r4, r2, r3, r1, &gc_required);
  // r4: new heap number
#if V8_TARGET_ARCH_S390X
  __ lg(r2, MemOperand(r6, -kDoubleSize));
  __ AddP(r3, r4, Operand(-1));  // subtract tag for std
  __ stg(r2, MemOperand(r3, HeapNumber::kValueOffset));
#else
  __ LoadlW(r2, MemOperand(r6, Register::kMantissaOffset - kDoubleSize));
  __ LoadlW(r3, MemOperand(r6, Register::kExponentOffset - kDoubleSize));
  __ StoreW(r2, FieldMemOperand(r4, HeapNumber::kMantissaOffset));
  __ StoreW(r3, FieldMemOperand(r4, HeapNumber::kExponentOffset));
#endif
  __ LoadRR(r2, r5);
  __ StoreP(r4, MemOperand(r5));
  __ AddP(r5, Operand(kPointerSize));
  __ RecordWrite(r8,
                 r2,
                 r4,
                 kLRHasNotBeenSaved,
                 kDontSaveFPRegs,
                 EMIT_REMEMBERED_SET,
                 OMIT_SMI_CHECK);
  __ b(&entry, Label::kNear);

  // Replace the-hole NaN with the-hole pointer.
  __ bind(&convert_hole);
  __ StoreP(r9, MemOperand(r5));
  __ AddP(r5, Operand(kPointerSize));

  __ bind(&entry);
  __ CmpLogicalP(r5, r7);
  __ blt(&loop);

  __ Pop(r5, r4, r3, r2);
  // Replace receiver's backing store with newly created and filled FixedArray.
  __ StoreP(r8, FieldMemOperand(r4, JSObject::kElementsOffset));
  __ RecordWriteField(r4,
                      JSObject::kElementsOffset,
                      r8,
                      r13,
                      kLRHasNotBeenSaved,
                      kDontSaveFPRegs,
                      EMIT_REMEMBERED_SET,
                      OMIT_SMI_CHECK);

  __ bind(&only_change_map);
  // Update receiver's map.
  __ StoreP(r5, FieldMemOperand(r4, HeapObject::kMapOffset));
  __ RecordWriteField(r4,
                      HeapObject::kMapOffset,
                      r5,
                      r13,
                      kLRHasNotBeenSaved,
                      kDontSaveFPRegs,
                      OMIT_REMEMBERED_SET,
                      OMIT_SMI_CHECK);
}


// roohack - assume ip can be used as a scratch register below
void StringCharLoadGenerator::Generate(MacroAssembler* masm,
                                       Register string,
                                       Register index,
                                       Register result,
                                       Label* call_runtime) {
  // Fetch the instance type of the receiver into result register.
  __ LoadP(result, FieldMemOperand(string, HeapObject::kMapOffset));
  __ LoadlB(result, FieldMemOperand(result, Map::kInstanceTypeOffset));

  // We need special handling for indirect strings.
  Label check_sequential;
  __ mov(r0, Operand(kIsIndirectStringMask));
  __ AndP(r0, result);
  __ beq(&check_sequential /*, cr0*/);

  // Dispatch on the indirect string shape: slice or cons.
  Label cons_string;
  __ mov(ip, Operand(kSlicedNotConsMask));
  __ LoadRR(r0, result);
  __ AndP(r0, ip/*, SetRC*/);  // Should be okay to remove RC
  __ beq(&cons_string /*, cr0*/);

  // Handle slices.
  Label indirect_string_loaded;
  __ LoadP(result, FieldMemOperand(string, SlicedString::kOffsetOffset));
  __ LoadP(string, FieldMemOperand(string, SlicedString::kParentOffset));
  __ SmiUntag(ip, result);
  __ AddP(index, ip);
  __ b(&indirect_string_loaded);

  // Handle cons strings.
  // Check whether the right hand side is the empty string (i.e. if
  // this is really a flat string in a cons string). If that is not
  // the case we would rather go to the runtime system now to flatten
  // the string.
  __ bind(&cons_string);
  __ LoadP(result, FieldMemOperand(string, ConsString::kSecondOffset));
  __ CompareRoot(result, Heap::kempty_stringRootIndex);
  __ bne(call_runtime);
  // Get the first of the two strings and load its instance type.
  __ LoadP(string, FieldMemOperand(string, ConsString::kFirstOffset));

  __ bind(&indirect_string_loaded);
  __ LoadP(result, FieldMemOperand(string, HeapObject::kMapOffset));
  __ LoadlB(result, FieldMemOperand(result, Map::kInstanceTypeOffset));

  // Distinguish sequential and external strings. Only these two string
  // representations can reach here (slices and flat cons strings have been
  // reduced to the underlying sequential or external string).
  Label external_string, check_encoding;
  __ bind(&check_sequential);
  STATIC_ASSERT(kSeqStringTag == 0);
  __ mov(r0, Operand(kStringRepresentationMask));
  __ AndP(r0, result);
  __ bne(&external_string /*, cr0*/);

  // Prepare sequential strings
  STATIC_ASSERT(SeqTwoByteString::kHeaderSize == SeqOneByteString::kHeaderSize);
  __ AddP(string, Operand(SeqTwoByteString::kHeaderSize - kHeapObjectTag));
  __ b(&check_encoding);

  // Handle external strings.
  __ bind(&external_string);
  if (FLAG_debug_code) {
    // Assert that we do not have a cons or slice (indirect strings) here.
    // Sequential strings have already been ruled out.
    __ mov(r0, Operand(kIsIndirectStringMask));
    __ AndP(r0, result);
    __ Assert(eq, kExternalStringExpectedButNotFound, cr0);
  }
  // Rule out short external strings.
  STATIC_CHECK(kShortExternalStringTag != 0);
  __ mov(r0, Operand(kShortExternalStringMask));
  __ AndP(r0, result);
  __ bne(call_runtime /*, cr0*/);
  __ LoadP(string,
           FieldMemOperand(string, ExternalString::kResourceDataOffset));

  Label ascii, done;
  __ bind(&check_encoding);
  STATIC_ASSERT(kTwoByteStringTag == 0);
  __ mov(r0, Operand(kStringEncodingMask));
  __ AndP(r0, result);
  __ bne(&ascii /*, cr0*/);
  // Two-byte string.
  __ ShiftLeftP(result, index, Operand(1));
  __ LoadLogicalHalfWordP(result, MemOperand(string, result));
  __ b(&done);
  __ bind(&ascii);
  // Ascii string.
  __ LoadlB(result, MemOperand(string, index));
  __ bind(&done);
}


static MemOperand ExpConstant(int index, Register base) {
  return MemOperand(base, index * kDoubleSize);
}


void MathExpGenerator::EmitMathExp(MacroAssembler* masm,
                                   DoubleRegister input,
                                   DoubleRegister result,
                                   DoubleRegister double_scratch1,
                                   DoubleRegister double_scratch2,
                                   Register temp1,
                                   Register temp2,
                                   Register temp3) {
  ASSERT(!input.is(result));
  ASSERT(!input.is(double_scratch1));
  ASSERT(!input.is(double_scratch2));
  ASSERT(!result.is(double_scratch1));
  ASSERT(!result.is(double_scratch2));
  ASSERT(!double_scratch1.is(double_scratch2));
  ASSERT(!temp1.is(temp2));
  ASSERT(!temp1.is(temp3));
  ASSERT(!temp2.is(temp3));
  ASSERT(ExternalReference::math_exp_constants(0).address() != NULL);

  Label zero, infinity, done;

  __ mov(temp3, Operand(ExternalReference::math_exp_constants(0)));

  __ LoadF(double_scratch1, ExpConstant(0, temp3));
  __ cdbr(double_scratch1, input);
  __ ldr(result, input);
  __ bunordered(&done);
  __ bge(&zero);

  __ LoadF(double_scratch2, ExpConstant(1, temp3));
  __ cdbr(input, double_scratch2);
  __ bge(&infinity);

  __ LoadF(double_scratch1, ExpConstant(3, temp3));
  __ LoadF(result, ExpConstant(4, temp3));
  // @TODO(Tara): verify madbr for correctness and use here instead of mdbr,adbr
  __ mdbr(double_scratch1, input);
  __ adbr(double_scratch1, result);

  // Move low word of double_scratch1 to temp2
  __ lgdr(temp2, double_scratch1);
  __ nihf(temp2, Operand::Zero());

  __ sdbr(double_scratch1, result);
  __ LoadF(result, ExpConstant(6, temp3));
  __ LoadF(double_scratch2, ExpConstant(5, temp3));
  __ mdbr(double_scratch1, double_scratch2);
  __ sdbr(double_scratch1, input);
  __ sdbr(result, double_scratch1);
  __ ldr(double_scratch2, double_scratch1);
  __ mdbr(double_scratch2, double_scratch2);
  __ mdbr(result, double_scratch2);
  __ LoadF(double_scratch2, ExpConstant(7, temp3));
  __ mdbr(result, double_scratch2);
  __ sdbr(result, double_scratch1);
  __ LoadF(double_scratch2, ExpConstant(8, temp3));
  __ adbr(result, double_scratch2);
  __ ShiftRight(temp1, temp2, Operand(11));
  __ AndP(temp2, Operand(0x7ff));
  __ AddP(temp1, Operand(0x3ff));

  // Must not call ExpConstant() after overwriting temp3!
  __ mov(temp3, Operand(ExternalReference::math_exp_log_table()));
  __ ShiftLeft(temp2, temp2, Operand(3));
#if V8_TARGET_ARCH_S39064
  __ lg(temp2, MemOperand(temp3, temp2));
  __ ShiftLeftP(temp1, temp1, Operand(52));
  __ OrP(temp2, temp1);
  __ stg(temp2, MemOperand(sp, 0));
#else
  __ AddP(ip, temp3, temp2);
  __ LoadlW(temp3, MemOperand(ip, Register::kExponentOffset));
  __ LoadlW(temp2, MemOperand(ip, Register::kMantissaOffset));
  __ ShiftLeft(temp1, temp1, Operand(20));
  __ OrP(temp3, temp1);
  __ StoreW(temp3, MemOperand(sp, Register::kExponentOffset));
  __ StoreW(temp2, MemOperand(sp, Register::kMantissaOffset));
#endif
  __ LoadF(double_scratch1, MemOperand(sp, 0));
  __ AddP(sp, sp, Operand(kDoubleSize));

  __ mdbr(result, double_scratch1);
  __ b(&done, Label::kNear);

  __ bind(&zero);
  __ ldr(result, kDoubleRegZero);
  __ b(&done);

  __ bind(&infinity);
  __ LoadF(result, ExpConstant(2, temp3));

  __ bind(&done);
}

#undef __

// @TODO(Tara): Figure how exactly CodeAgeSequence functions map to z
#ifdef DEBUG
// mflr ip
static const uint32_t kCodeAgePatchFirstInstruction = 0x7d8802a6;
#endif

static byte* GetNoCodeAgeSequence(uint32_t* length) {
  // The sequence of instructions that is patched out for aging code is the
  // following boilerplate stack-building prologue that is found in FUNCTIONS
  static bool initialized = false;
  static uint32_t sequence[kCodeAgeSequenceLength];
  byte* byte_sequence = reinterpret_cast<byte*>(sequence);
  *length = kCodeAgeSequenceLength * Assembler::kInstrSize;
  if (!initialized) {
    // Since patcher is a large object, allocate it dynamically when needed,
    // to avoid overloading the stack in stress conditions.
    SmartPointer<CodePatcher>
        patcher(new CodePatcher(byte_sequence, kCodeAgeSequenceLength));
    PredictableCodeSizeScope scope(patcher->masm(), *length);
    patcher->masm()->PushFixedFrame(r4);
    patcher->masm()->AddP(
        fp, sp, Operand(StandardFrameConstants::kFixedFrameSizeFromFp));
    for (int i = 0; i < kNoCodeAgeSequenceNops; i++) {
      patcher->masm()->nop();
    }
    initialized = true;
  }
  return byte_sequence;
}


bool Code::IsYoungSequence(byte* sequence) {
  uint32_t young_length;
  byte* young_sequence = GetNoCodeAgeSequence(&young_length);
  bool result = !memcmp(sequence, young_sequence, young_length);
  ASSERT(result ||
         Memory::uint32_at(sequence) == kCodeAgePatchFirstInstruction);
  return result;
}


void Code::GetCodeAgeAndParity(byte* sequence, Age* age,
                               MarkingParity* parity) {
  if (IsYoungSequence(sequence)) {
    *age = kNoAgeCodeAge;
    *parity = NO_MARKING_PARITY;
  } else {
    ConstantPoolArray *constant_pool = NULL;
    Address target_address = Assembler::target_address_at(
      sequence + kCodeAgingTargetDelta, constant_pool);
    Code* stub = GetCodeFromTargetAddress(target_address);
    GetCodeAgeAndParity(stub, age, parity);
  }
}


void Code::PatchPlatformCodeAge(Isolate* isolate,
                                byte* sequence,
                                Code::Age age,
                                MarkingParity parity) {
  uint32_t young_length;
  byte* young_sequence = GetNoCodeAgeSequence(&young_length);
  if (age == kNoAgeCodeAge) {
    CopyBytes(sequence, young_sequence, young_length);
    CPU::FlushICache(sequence, young_length);
  } else {
    // FIXED_SEQUENCE
    Code* stub = GetCodeAgeStub(isolate, age, parity);
    CodePatcher patcher(sequence, young_length / Assembler::kInstrSize);
    Assembler::BlockTrampolinePoolScope block_trampoline_pool(patcher.masm());
    intptr_t target = reinterpret_cast<intptr_t>(stub->instruction_start());
    // We use Call to compute the address of this patch sequence.
    // Preserve lr since it will be clobbered.  See
    // GenerateMakeCodeYoungAgainCommon for the stub code.
   // patcher.masm()->mflr(p);
    patcher.masm()->mov(r2, Operand(target));
    patcher.masm()->Call(r2);
    for (int i = 0; i < kCodeAgingSequenceNops; i++) {
      patcher.masm()->nop();
    }
  }
}


} }  // namespace v8::internal

#endif  // V8_TARGET_ARCH_S390
