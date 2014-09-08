// Copyright 2012 the V8 project authors. All rights reserved.
//
// Copyright IBM Corp. 2012-2014. All rights reserved.
//
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.

#ifndef V8_S390_FRAMES_S390_H_
#define V8_S390_FRAMES_S390_H_

namespace v8 {
namespace internal {


// Register list in load/store instructions
// Note that the bit values must match those used in actual instruction encoding
const int kNumRegs = 16;


// Caller-saved/arguments registers
const RegList kJSCallerSaved =
  1 << 1  |  // r1
  1 << 2  |  // r2  a1
  1 << 3  |  // r3  a2
  1 << 4  |  // r4  a3
  1 << 5;    // r5  a4

const int kNumJSCallerSaved = 5;

typedef Object* JSCallerSavedBuffer[kNumJSCallerSaved];

// Return the code of the n-th caller-saved register available to JavaScript
// e.g. JSCallerSavedReg(0) returns r0.code() == 0
int JSCallerSavedCode(int n);


// Callee-saved registers preserved when switching from C to JavaScript
// N.B.  Do not bother saving all non-volatiles -- only those that v8
//       modifies without saving/restoring inline.
const RegList kCalleeSaved =
  1 << 6 |   // r6 (argument passing in CEntryStub)
             //    (HandleScope logic in MacroAssembler)
  1 << 7 |   // r7 (argument passing in CEntryStub)
             //    (HandleScope logic in MacroAssembler)
  1 << 8 |   // r8 (argument passing in CEntryStub)
             //    (HandleScope logic in MacroAssembler)
  1 << 9 |   // r9 (HandleScope logic in MacroAssembler)
  1 << 10 |  // r10 (Roots register in Javascript)
  1 << 11 |  // r11 (fp in Javascript)
  1 << 13;   // r13 (cp in Javascript)

/*
  Legacy PPC Linkage left here for reference.
  1 <<  14 |  // r14 (general use)
  1 <<  15 |  // r15 (general use)
  1 <<  16 |  // r16 (general use)
  1 <<  17 |  // r17 (general use)
  1 <<  18 |  // r18 (general use / cp in Javascript code)
  1 <<  19 |  // r19 (roots array in Javascript code)
#if V8_OOL_CONSTANT_POOL
  1 <<  20 |  // r20 (constant pool array in Javascript code)
#endif
  1 <<  31;   // r31 (fp in Javascript code)
*/


const int kNumCalleeSaved = 7;

// Number of registers for which space is reserved in safepoints. Must be a
// multiple of 8.
// TODO(regis): Only 8 registers may actually be sufficient. Revisit.
const int kNumSafepointRegisters = 16;

// Define the list of registers actually saved at safepoints.
// Note that the number of saved registers may be smaller than the reserved
// space, i.e. kNumSafepointSavedRegisters <= kNumSafepointRegisters.
const RegList kSafepointSavedRegisters = kJSCallerSaved | kCalleeSaved;
const int kNumSafepointSavedRegisters = kNumJSCallerSaved + kNumCalleeSaved;

// The following constants describe the stack frame linkage area as
// defined by the ABI.

#if defined(V8_TARGET_ARCH_S390X)
// [0] Back Chain
// [1] Reserved for compiler use
// [2] GPR 2
// [3] GPR 3
// ...
// [15] GPR 15
// [16] FPR 0
// [17] FPR 2
// [18] FPR 4
// [19] FPR 6
const int kNumRequiredStackFrameSlots = 20;
const int kStackFrameRASlot = 14;
const int kStackFrameSPSlot = 15;
const int kStackFrameExtraParamSlot = 20;
#else
// [0] Back Chain
// [1] Reserved for compiler use
// [2] GPR 2
// [3] GPR 3
// ...
// [15] GPR 15
// [16..17] FPR 0
// [18..19] FPR 2
// [20..21] FPR 4
// [22..23] FPR 6
const int kNumRequiredStackFrameSlots = 24;
const int kStackFrameRASlot = 14;
const int kStackFrameSPSlot = 15;
const int kStackFrameExtraParamSlot = 24;
#endif

// zLinux ABI requires caller frames to include sufficient space for
// callee preserved register save area.
#if defined(V8_TARGET_ARCH_S390X)
const int kCalleeRegisterSaveAreaSize = 160;
#elif defined(V8_TARGET_ARCH_S390)
const int kCalleeRegisterSaveAreaSize = 96;
#else
const int kCalleeRegisterSaveAreaSize = 0;
#endif

// ----------------------------------------------------


class EntryFrameConstants : public AllStatic {
 public:
  static const int kCallerFPOffset =
      -(StandardFrameConstants::kFixedFrameSizeFromFp + kPointerSize);
};


class ExitFrameConstants : public AllStatic {
 public:
#if V8_OOL_CONSTANT_POOL
  static const int kFrameSize = 3 * kPointerSize;
  static const int kConstantPoolOffset = -3 * kPointerSize;
#else
  static const int kFrameSize = 2 * kPointerSize;
  static const int kConstantPoolOffset = 0;  // Not used.
#endif
  static const int kCodeOffset = -2 * kPointerSize;
  static const int kSPOffset = -1 * kPointerSize;

  // The caller fields are below the frame pointer on the stack.
  static const int kCallerFPOffset = 0 * kPointerSize;
  // The calling JS function is below FP.
  static const int kCallerPCOffset = 1 * kPointerSize;

  // FP-relative displacement of the caller's SP.  It points just
  // below the saved PC.
  static const int kCallerSPDisplacement = 2 * kPointerSize;
};


class JavaScriptFrameConstants : public AllStatic {
 public:
  // FP-relative.
  static const int kLocal0Offset = StandardFrameConstants::kExpressionsOffset;
  static const int kLastParameterOffset = +2 * kPointerSize;
  static const int kFunctionOffset = StandardFrameConstants::kMarkerOffset;

  // Caller SP-relative.
  static const int kParam0Offset   = -2 * kPointerSize;
  static const int kReceiverOffset = -1 * kPointerSize;
};


class ArgumentsAdaptorFrameConstants : public AllStatic {
 public:
  // FP-relative.
  static const int kLengthOffset = StandardFrameConstants::kExpressionsOffset;

  static const int kFrameSize =
      StandardFrameConstants::kFixedFrameSize + kPointerSize;
};


class ConstructFrameConstants : public AllStatic {
 public:
  // FP-relative.
  static const int kImplicitReceiverOffset = -6 * kPointerSize;
  static const int kConstructorOffset      = -5 * kPointerSize;
  static const int kLengthOffset           = -4 * kPointerSize;
  static const int kCodeOffset = StandardFrameConstants::kExpressionsOffset;

  static const int kFrameSize =
      StandardFrameConstants::kFixedFrameSize + 4 * kPointerSize;
};


class InternalFrameConstants : public AllStatic {
 public:
  // FP-relative.
  static const int kCodeOffset = StandardFrameConstants::kExpressionsOffset;
};


inline Object* JavaScriptFrame::function_slot_object() const {
  const int offset = JavaScriptFrameConstants::kFunctionOffset;
  return Memory::Object_at(fp() + offset);
}


inline void StackHandler::SetFp(Address slot, Address fp) {
  Memory::Address_at(slot) = fp;
}


} }  // namespace v8::internal

#endif  // V8_S390_FRAMES_S390_H_
