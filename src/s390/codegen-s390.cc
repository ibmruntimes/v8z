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
#include "macro-assembler.h"

namespace v8 {
namespace internal {

#define __ ACCESS_MASM(masm)

UnaryMathFunction CreateTranscendentalFunction(TranscendentalCache::Type type) {
  switch (type) {
    case TranscendentalCache::SIN: return &sin;
    case TranscendentalCache::COS: return &cos;
    case TranscendentalCache::TAN: return &tan;
    case TranscendentalCache::LOG: return &log;
    default: UNIMPLEMENTED();
  }
  return NULL;
}


UnaryMathFunction CreateSqrtFunction() {
  return &sqrt;
}

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

void ElementsTransitionGenerator::GenerateMapChangeElementsTransition(
    MacroAssembler* masm) {
  // ----------- S t a t e -------------
  //  -- r3_p    : value
  //  -- r4_p    : key
  //  -- r5_p    : receiver
  //  -- lr    : return address
  //  -- r6_p    : target map, scratch for subsequent call
  //  -- r7_p    : scratch (elements)
  // -----------------------------------
  // Set transitioned map.
  __ StoreP(r6_p, FieldMemOperand(r5_p, HeapObject::kMapOffset));
  __ RecordWriteField(r5_p,
                      HeapObject::kMapOffset,
                      r6_p,
                      r22_p,
                      kLRHasNotBeenSaved,
                      kDontSaveFPRegs,
                      EMIT_REMEMBERED_SET,
                      OMIT_SMI_CHECK);
}


void ElementsTransitionGenerator::GenerateSmiToDouble(
    MacroAssembler* masm, Label* fail) {
  // ----------- S t a t e -------------
  //  -- r3_p    : value
  //  -- r4_p    : key
  //  -- r5_p    : receiver
  //  -- lr    : return address
  //  -- r6_p    : target map, scratch for subsequent call
  //  -- r7_p    : scratch (elements)
  // -----------------------------------
  Label loop, entry, convert_hole, gc_required, only_change_map, done;

  // Check for empty arrays, which only require a map transition and no changes
  // to the backing store.
  __ LoadP(r7_p, FieldMemOperand(r5_p, JSObject::kElementsOffset));
  __ CompareRoot(r7_p, Heap::kEmptyFixedArrayRootIndex);
  __ beq(&only_change_map);

  // Preserve lr and use r30_p as a temporary register.
  __ mflr(r0_p);
  __ Push(r0_p, r30_p);

  __ LoadP(r8_p, FieldMemOperand(r7_p, FixedArray::kLengthOffset));
  // r7_p: source FixedArray
  // r8_p: number of elements (smi-tagged)

  // Allocate new FixedDoubleArray.
  __ SmiToDoubleArrayOffset(r30_p, r8_p);
  __ AddP(r30_p, Operand(FixedDoubleArray::kHeaderSize + kPointerSize));
  __ AllocateInNewSpace(r30_p, r9_p, r10_p, r22_p,
                        &gc_required, NO_ALLOCATION_FLAGS);
  // r9_p: destination FixedDoubleArray, not tagged as heap object.

  // Align the array conveniently for doubles.
  // Store a filler value in the unused memory.
  Label aligned, aligned_done;
  __ LoadRR(r0_p, r9_p);
  __ AndP(r0_p, Operand(kDoubleAlignmentMask));
  __ mov(ip, Operand(masm->isolate()->factory()->one_pointer_filler_map()));
  __ beq(&aligned /*, cr0*/);
  // Store at the beginning of the allocated memory and update the base pointer.
  __ StoreP(ip, MemOperand(r9_p));
  __ AddP(r9_p, Operand(kPointerSize));
  __ b(&aligned_done);

  __ bind(&aligned);
  // Store the filler at the end of the allocated memory.
  __ Sub(r30_p, Operand(kPointerSize));
  __ StorePX(ip, MemOperand(r9_p, r30_p));

  __ bind(&aligned_done);

  // Set destination FixedDoubleArray's length and map.
  __ LoadRoot(r22_p, Heap::kFixedDoubleArrayMapRootIndex);
  __ StoreP(r8_p, MemOperand(r9_p, FixedDoubleArray::kLengthOffset));
  // Update receiver's map.
  __ StoreP(r22_p, MemOperand(r9_p, HeapObject::kMapOffset));

  __ StoreP(r6_p, FieldMemOperand(r5_p, HeapObject::kMapOffset));
  __ RecordWriteField(r5_p,
                      HeapObject::kMapOffset,
                      r6_p,
                      r22_p,
                      kLRHasBeenSaved,
                      kDontSaveFPRegs,
                      OMIT_REMEMBERED_SET,
                      OMIT_SMI_CHECK);
  // Replace receiver's backing store with newly created FixedDoubleArray.
  __ LoadRR(r6_p, r9_p);
  __ AddP(r6_p, Operand(kHeapObjectTag));
  __ StoreP(r6_p, FieldMemOperand(r5_p, JSObject::kElementsOffset));
  __ RecordWriteField(r5_p,
                      JSObject::kElementsOffset,
                      r6_p,
                      r22_p,
                      kLRHasBeenSaved,
                      kDontSaveFPRegs,
                      EMIT_REMEMBERED_SET,
                      OMIT_SMI_CHECK);

  // Prepare for conversion loop.
  __ LoadRR(r6_p, r7_p);
  __ AddP(r6_p, Operand(FixedArray::kHeaderSize - kHeapObjectTag));
  __ AddP(r10_p, Operand(FixedDoubleArray::kHeaderSize));
  __ SmiToDoubleArrayOffset(r9_p, r8_p);
  __ AddP(r9_p, r10_p);
#if V8_TARGET_ARCH_S390X
  __ mov(r7_p, Operand(kHoleNanInt64));
#else
  __ mov(r7_p, Operand(kHoleNanLower32));
  __ mov(r8_p, Operand(kHoleNanUpper32));
#endif
  // r6_p: begin of source FixedArray element fields, not tagged
  // r7_p: kHoleNanLower32
  // r8_p: kHoleNanUpper32
  // r9_p: end of destination FixedDoubleArray, not tagged
  // r10_p: begin of FixedDoubleArray element fields, not tagged

  __ b(&entry);

  __ bind(&only_change_map);
  __ StoreP(r6_p, FieldMemOperand(r5_p, HeapObject::kMapOffset));
  __ RecordWriteField(r5_p,
                      HeapObject::kMapOffset,
                      r6_p,
                      r22_p,
                      kLRHasBeenSaved,
                      kDontSaveFPRegs,
                      OMIT_REMEMBERED_SET,
                      OMIT_SMI_CHECK);
  __ b(&done);

  // Call into runtime if GC is required.
  __ bind(&gc_required);
  __ Pop(r0_p, r30_p);
  __ mtlr(r0_p);
  __ b(fail);

  // Convert and copy elements.
  __ bind(&loop);
  __ LoadP(r22_p, MemOperand(r6_p));
  __ AddP(r6_p, Operand(kPointerSize));
  // r22_p: current element
  __ UntagAndJumpIfNotSmi(r22_p, r22_p, &convert_hole);

  // Normal smi, convert to double and store.
  FloatingPointHelper::ConvertIntToDouble(
    masm, r22_p, d0);
  __ StoreF(d0, MemOperand(r10_p, 0));
  __ AddP(r10_p, Operand(8));

  __ b(&entry);

  // Hole found, store the-hole NaN.
  __ bind(&convert_hole);
  if (FLAG_debug_code) {
    // Restore a "smi-untagged" heap object.
    __ LoadP(r22_p, MemOperand(r6_p, -kPointerSize));
    __ CompareRoot(r22_p, Heap::kTheHoleValueRootIndex);
    __ Assert(eq, "object found in smi-only array");
  }
#if V8_TARGET_ARCH_S390X
  __ stg(r7_p, MemOperand(r10_p, 0));
#else
#if __FLOAT_WORD_ORDER == __LITTLE_ENDIAN
  __ StoreW(r7_p, MemOperand(r10_p, 0));
  __ StoreW(r8_p, MemOperand(r10_p, 4));
#else
  __ StoreW(r8_p, MemOperand(r10_p, 0));
  __ StoreW(r7_p, MemOperand(r10_p, 4));
#endif
#endif
  __ AddP(r10_p, Operand(8));

  __ bind(&entry);
  __ CmpRR(r10_p, r9_p);
  __ blt(&loop);

  __ Pop(r0_p, r30_p);
  __ mtlr(r0_p);
  __ bind(&done);
}


void ElementsTransitionGenerator::GenerateDoubleToObject(
    MacroAssembler* masm, Label* fail) {
  // ----------- S t a t e -------------
  //  -- r3_p    : value
  //  -- r4_p    : key
  //  -- r5_p    : receiver
  //  -- lr    : return address
  //  -- r6_p    : target map, scratch for subsequent call
  //  -- r7_p    : scratch (elements)
  // -----------------------------------
  Label entry, loop, convert_hole, gc_required, only_change_map;

  // Check for empty arrays, which only require a map transition and no changes
  // to the backing store.
  __ LoadP(r7_p, FieldMemOperand(r5_p, JSObject::kElementsOffset));
  __ CompareRoot(r7_p, Heap::kEmptyFixedArrayRootIndex);
  __ beq(&only_change_map);

  __ Push(r6_p, r5_p, r4_p, r3_p);
  __ LoadP(r8_p, FieldMemOperand(r7_p, FixedArray::kLengthOffset));
  // r7_p: source FixedDoubleArray
  // r8_p: number of elements (smi-tagged)

  // Allocate new FixedArray.
  __ lhi(r3_p, Operand(FixedDoubleArray::kHeaderSize));
  __ SmiToPtrArrayOffset(r0_p, r8_p);
  __ AddP(r3_p, r0_p);
  __ AllocateInNewSpace(r3_p, r9_p, r10_p, r22_p,
                       &gc_required, NO_ALLOCATION_FLAGS);
  // r9_p: destination FixedArray, not tagged as heap object
  // Set destination FixedDoubleArray's length and map.
  __ LoadRoot(r22_p, Heap::kFixedArrayMapRootIndex);
  __ StoreP(r8_p, MemOperand(r9_p, FixedDoubleArray::kLengthOffset));
  __ StoreP(r22_p, MemOperand(r9_p, HeapObject::kMapOffset));

  // Prepare for conversion loop.
  __ AddP(r7_p, Operand(FixedDoubleArray::kHeaderSize - kHeapObjectTag));
  __ LoadRR(r6_p, r9_p);
  __ AddP(r6_p, Operand(FixedArray::kHeaderSize));
  __ AddP(r9_p, Operand(kHeapObjectTag));
  __ SmiToPtrArrayOffset(r8_p, r8_p);
  __ AddP(r8_p, r6_p);
  __ LoadRoot(r10_p, Heap::kTheHoleValueRootIndex);
  __ LoadRoot(r22_p, Heap::kHeapNumberMapRootIndex);
  // Using offsetted addresses in r7_p to fully take advantage of post-indexing.
  // r6_p: begin of destination FixedArray element fields, not tagged
  // r7_p: begin of source FixedDoubleArray element fields, not tagged
  // r8_p: end of destination FixedArray, not tagged
  // r9_p: destination FixedArray
  // r10_p: the-hole pointer
  // r22_p: heap number map
  __ b(&entry);

  // Call into runtime if GC is required.
  __ bind(&gc_required);
  __ Pop(r6_p, r5_p, r4_p, r3_p);
  __ b(fail);

  __ bind(&loop);
#if __FLOAT_WORD_ORDER == __LITTLE_ENDIAN
  __ LoadlW(r4_p, MemOperand(r7_p, 4));
#else
  __ LoadlW(r4_p, MemOperand(r7_p));
#endif
  __ AddP(r7_p, Operand(8));
  // r4_p: current element's upper 32 bit
  // r7_p: address of next element's upper 32 bit
  __ Cmpi(r4_p, Operand(kHoleNanUpper32));
  __ beq(&convert_hole);

  // Non-hole double, copy value into a heap number.
  __ AllocateHeapNumber(r5_p, r3_p, r4_p, r22_p, &gc_required);
  // r5_p: new heap number
#if V8_TARGET_ARCH_S390X
  __ ld(r3_p, MemOperand(r7_p, -8));
  __ Add(r4_p, r5_p, Operand(-1));  // subtract tag for std
  __ stg(r3_p, MemOperand(r4_p, HeapNumber::kValueOffset));
#else
#if __FLOAT_WORD_ORDER == __LITTLE_ENDIAN
  __ LoadlW(r3_p, MemOperand(r7_p, -8));
  __ LoadlW(r4_p, MemOperand(r7_p, -4));
  __ StoreW(r3_p, FieldMemOperand(r5_p, HeapNumber::kValueOffset));
  __ StoreW(r4_p, FieldMemOperand(r5_p, HeapNumber::kValueOffset+4));
#else
  __ LoadlW(r3_p, MemOperand(r7_p, -4));
  __ LoadlW(r4_p, MemOperand(r7_p, -8));
  __ StoreW(r3_p, FieldMemOperand(r5_p, HeapNumber::kValueOffset+4));
  __ StoreW(r4_p, FieldMemOperand(r5_p, HeapNumber::kValueOffset));
#endif
#endif
  __ LoadRR(r3_p, r6_p);
  __ StoreP(r5_p, MemOperand(r6_p));
  __ AddP(r6_p, Operand(kPointerSize));
  __ RecordWrite(r9_p,
                 r3_p,
                 r5_p,
                 kLRHasNotBeenSaved,
                 kDontSaveFPRegs,
                 EMIT_REMEMBERED_SET,
                 OMIT_SMI_CHECK);
  __ b(&entry);

  // Replace the-hole NaN with the-hole pointer.
  __ bind(&convert_hole);
  __ StoreP(r10_p, MemOperand(r6_p));
  __ AddP(r6_p, Operand(kPointerSize));

  __ bind(&entry);
  __ Cmpl(r6_p, r8_p);
  __ blt(&loop);

  __ Pop(r6_p, r5_p, r4_p, r3_p);
  // Replace receiver's backing store with newly created and filled FixedArray.
  __ StoreP(r9_p, FieldMemOperand(r5_p, JSObject::kElementsOffset));
  __ RecordWriteField(r5_p,
                      JSObject::kElementsOffset,
                      r9_p,
                      r22_p,
                      kLRHasNotBeenSaved,
                      kDontSaveFPRegs,
                      EMIT_REMEMBERED_SET,
                      OMIT_SMI_CHECK);

  __ bind(&only_change_map);
  // Update receiver's map.
  __ StoreP(r6_p, FieldMemOperand(r5_p, HeapObject::kMapOffset));
  __ RecordWriteField(r5_p,
                      HeapObject::kMapOffset,
                      r6_p,
                      r22_p,
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
  __ LoadRR(r0_p, result);
  __ AndP(r0_p, Operand(kIsIndirectStringMask));
  __ beq(&check_sequential /*, cr0*/);

  // Dispatch on the indirect string shape: slice or cons.
  Label cons_string;
  __ mov(ip, Operand(kSlicedNotConsMask));
  __ LoadRR(r0_p, result);
  __ AndP(r0_p, ip/*, SetRC*/);  // Should be okay to remove RC
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
  __ CompareRoot(result, Heap::kEmptyStringRootIndex);
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
  __ LoadRR(r0_p, result);
  __ AndP(r0_p, Operand(kStringRepresentationMask));
  __ bne(&external_string /*, cr0*/);

  // Prepare sequential strings
  STATIC_ASSERT(SeqTwoByteString::kHeaderSize == SeqAsciiString::kHeaderSize);
  __ AddP(string, Operand(SeqTwoByteString::kHeaderSize - kHeapObjectTag));
  __ b(&check_encoding);

  // Handle external strings.
  __ bind(&external_string);
  if (FLAG_debug_code) {
    // Assert that we do not have a cons or slice (indirect strings) here.
    // Sequential strings have already been ruled out.
    __ LoadRR(r0_p, result);
    __ AndP(r0_p, Operand(kIsIndirectStringMask));
    __ Assert(eq, "external string expected, but not found", cr0);
  }
  // Rule out short external strings.
  STATIC_CHECK(kShortExternalStringTag != 0);
  __ LoadRR(r0_p, result);
  __ AndP(r0_p, Operand(kShortExternalStringMask));
  __ bne(call_runtime /*, cr0*/);
  __ LoadP(string,
           FieldMemOperand(string, ExternalString::kResourceDataOffset));

  Label ascii, done;
  __ bind(&check_encoding);
  STATIC_ASSERT(kTwoByteStringTag == 0);
  __ LoadRR(r0_p, result);
  __ AndP(r0_p, Operand(kStringEncodingMask));
  __ bne(&ascii /*, cr0*/);
  // Two-byte string.
  __ ShiftLeftImm(result, index, Operand(1));
  __ lhzx(result, MemOperand(string, result));
  __ b(&done);
  __ bind(&ascii);
  // Ascii string.
  __ LoadlB(result, MemOperand(string, index));
  __ bind(&done);
}

#undef __

} }  // namespace v8::internal

#endif  // V8_TARGET_ARCH_S390
