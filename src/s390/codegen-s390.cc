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
  //  -- r2    : value
  //  -- r3    : key
  //  -- r4    : receiver
  //  -- r14     : return address
  //  -- r5    : target map, scratch for subsequent call
  //  -- r6    : scratch (elements)
  // -----------------------------------
  // Set transitioned map.
  __ StoreP(r5, FieldMemOperand(r4, HeapObject::kMapOffset));
  __ RecordWriteField(r4,
                      HeapObject::kMapOffset,
                      r5,
                      r1,
                      kLRHasNotBeenSaved,
                      kDontSaveFPRegs,
                      EMIT_REMEMBERED_SET,
                      OMIT_SMI_CHECK);
}


void ElementsTransitionGenerator::GenerateSmiToDouble(
    MacroAssembler* masm, Label* fail) {
  // ----------- S t a t e -------------
  //  -- r2    : value
  //  -- r3    : key
  //  -- r4    : receiver
  //  -- lr    : return address
  //  -- r5    : target map, scratch for subsequent call
  //  -- r6    : scratch (elements)
  // -----------------------------------
  Label loop, entry, convert_hole, gc_required, only_change_map, done;

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
  __ AddP(r14, Operand(FixedDoubleArray::kHeaderSize + kPointerSize));
  __ AllocateInNewSpace(r14, r8, r9, r1,
                        &gc_required, NO_ALLOCATION_FLAGS);
  // r8: destination FixedDoubleArray, not tagged as heap object.

  // Align the array conveniently for doubles.
  // Store a filler value in the unused memory.
  Label aligned, aligned_done;
  __ tmll(r8, Operand(kDoubleAlignmentMask));
  __ mov(ip, Operand(masm->isolate()->factory()->one_pointer_filler_map()));
  __ beq(&aligned, Label::kNear);
  // Store at the beginning of the allocated memory and update the base pointer.
  __ StoreP(ip, MemOperand(r8));
  __ la(r8, MemOperand(r8, kPointerSize));
  __ b(&aligned_done, Label::kNear);

  __ bind(&aligned);
  // Store the filler at the end of the allocated memory.
  __ Sub(r14, Operand(kPointerSize));
  __ StorePX(ip, MemOperand(r8, r14));

  __ bind(&aligned_done);

  // Set destination FixedDoubleArray's length and map.
  __ LoadRoot(r1, Heap::kFixedDoubleArrayMapRootIndex);
  __ StoreP(r7, MemOperand(r8, FixedDoubleArray::kLengthOffset));
  // Update receiver's map.
  __ StoreP(r1, MemOperand(r8, HeapObject::kMapOffset));

  __ StoreP(r5, FieldMemOperand(r4, HeapObject::kMapOffset));
  __ RecordWriteField(r4,
                      HeapObject::kMapOffset,
                      r5,
                      r1,
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
                      r1,
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
                      r1,
                      kLRHasBeenSaved,
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
  __ LoadP(r1, MemOperand(r5));
  __ AddP(r5, Operand(kPointerSize));
  // r1: current element
  __ UntagAndJumpIfNotSmi(r1, r1, &convert_hole);

  // Normal smi, convert to double and store.
  FloatingPointHelper::ConvertIntToDouble(
    masm, r1, d0);
  __ StoreF(d0, MemOperand(r9, 0));
  __ la(r9, MemOperand(r9, 8));

  __ b(&entry, Label::kNear);

  // Hole found, store the-hole NaN.
  __ bind(&convert_hole);
  if (FLAG_debug_code) {
    // Restore a "smi-untagged" heap object.
    __ LoadP(r1, MemOperand(r5, -kPointerSize));
    __ CompareRoot(r1, Heap::kTheHoleValueRootIndex);
    __ Assert(eq, "object found in smi-only array");
  }
#if V8_TARGET_ARCH_S390X
  __ stg(r6, MemOperand(r9, 0));
#else
#if __FLOAT_WORD_ORDER == __LITTLE_ENDIAN
  __ StoreW(r6, MemOperand(r9, 0));
  __ StoreW(r7, MemOperand(r9, 4));
#else
  __ StoreW(r7, MemOperand(r9, 0));
  __ StoreW(r6, MemOperand(r9, 4));
#endif
#endif
  __ AddP(r9, Operand(8));

  __ bind(&entry);
  __ CmpRR(r9, r8);
  __ blt(&loop);

  __ pop(r14);
  __ bind(&done);
}


void ElementsTransitionGenerator::GenerateDoubleToObject(
    MacroAssembler* masm, Label* fail) {
  // ----------- S t a t e -------------
  //  -- r2    : value
  //  -- r3    : key
  //  -- r4    : receiver
  //  -- lr    : return address
  //  -- r5    : target map, scratch for subsequent call
  //  -- r6    : scratch (elements)
  // -----------------------------------
  Label entry, loop, convert_hole, gc_required, only_change_map;

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
  __ AllocateInNewSpace(r2, r8, r9, r1,
                       &gc_required, NO_ALLOCATION_FLAGS);
  // r8: destination FixedArray, not tagged as heap object
  // Set destination FixedDoubleArray's length and map.
  __ LoadRoot(r1, Heap::kFixedArrayMapRootIndex);
  __ StoreP(r7, MemOperand(r8, FixedDoubleArray::kLengthOffset));
  __ StoreP(r1, MemOperand(r8, HeapObject::kMapOffset));

  // Prepare for conversion loop.
  __ AddP(r6, Operand(FixedDoubleArray::kHeaderSize - kHeapObjectTag));
  __ AddP(r5, r8, Operand(FixedArray::kHeaderSize));
  __ AddP(r8, Operand(kHeapObjectTag));
  __ SmiToPtrArrayOffset(r7, r7);
  __ AddP(r7, r5);
  __ LoadRoot(r9, Heap::kTheHoleValueRootIndex);
  __ LoadRoot(r1, Heap::kHeapNumberMapRootIndex);
  // Using offsetted addresses in r6 to fully take advantage of post-indexing.
  // r5: begin of destination FixedArray element fields, not tagged
  // r6: begin of source FixedDoubleArray element fields, not tagged
  // r7: end of destination FixedArray, not tagged
  // r8: destination FixedArray
  // r9: the-hole pointer
  // r1: heap number map
  __ b(&entry);

  // Call into runtime if GC is required.
  __ bind(&gc_required);
  __ Pop(r5, r4, r3, r2);
  __ b(fail);

  __ bind(&loop);
#if __FLOAT_WORD_ORDER == __LITTLE_ENDIAN
  __ LoadlW(r3, MemOperand(r6, 4));
#else
  __ LoadlW(r3, MemOperand(r6));
#endif
  __ AddP(r6, Operand(8));
  // r3: current element's upper 32 bit
  // r6: address of next element's upper 32 bit
  __ Cmpi(r3, Operand(kHoleNanUpper32));
  __ beq(&convert_hole);

  // Non-hole double, copy value into a heap number.
  __ AllocateHeapNumber(r4, r2, r3, r1, &gc_required);
  // r4: new heap number
#if V8_TARGET_ARCH_S390X
  __ lg(r2, MemOperand(r6, -8));
  __ AddP(r3, r4, Operand(-1));  // subtract tag for std
  __ stg(r2, MemOperand(r3, HeapNumber::kValueOffset));
#else
#if __FLOAT_WORD_ORDER == __LITTLE_ENDIAN
  __ LoadlW(r2, MemOperand(r6, -8));
  __ LoadlW(r3, MemOperand(r6, -4));
  __ StoreW(r2, FieldMemOperand(r4, HeapNumber::kValueOffset));
  __ StoreW(r3, FieldMemOperand(r4, HeapNumber::kValueOffset+4));
#else
  __ LoadlW(r2, MemOperand(r6, -4));
  __ LoadlW(r3, MemOperand(r6, -8));
  __ StoreW(r2, FieldMemOperand(r4, HeapNumber::kValueOffset+4));
  __ StoreW(r3, FieldMemOperand(r4, HeapNumber::kValueOffset));
#endif
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
  __ b(&entry);

  // Replace the-hole NaN with the-hole pointer.
  __ bind(&convert_hole);
  __ StoreP(r9, MemOperand(r5));
  __ AddP(r5, Operand(kPointerSize));

  __ bind(&entry);
  __ Cmpl(r5, r7);
  __ blt(&loop);

  __ Pop(r5, r4, r3, r2);
  // Replace receiver's backing store with newly created and filled FixedArray.
  __ StoreP(r8, FieldMemOperand(r4, JSObject::kElementsOffset));
  __ RecordWriteField(r4,
                      JSObject::kElementsOffset,
                      r8,
                      r1,
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
                      r1,
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
  __ mov(r0, Operand(kStringRepresentationMask));
  __ AndP(r0, result);
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
    __ mov(r0, Operand(kIsIndirectStringMask));
    __ AndP(r0, result);
    __ Assert(eq, "external string expected, but not found", cr0);
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

#undef __

} }  // namespace v8::internal

#endif  // V8_TARGET_ARCH_S390
