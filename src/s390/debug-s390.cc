// Copyright 2015 the V8 project authors. All rights reserved.
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.

#include "src/v8.h"

#if V8_TARGET_ARCH_S390

#include "src/codegen.h"
#include "src/debug.h"

namespace v8 {
namespace internal {

void BreakLocation::SetDebugBreakAtReturn() {
  // Patch the code changing the return from JS function sequence from
  // 31-bit:
  //   lr sp, fp             2-bytes
  //   l fp, 0(sp)           4-bytes
  //   l r14, 4(sp)          4-bytes
  //   la sp, <delta>(sp)    4-bytes
  //   br r14                2-bytes
  //
  // to a call to the debug break return code.
  // this uses a FIXED_SEQUENCE to load a 32bit constant
  //
  //   iilf r14, <address>   6-bytes
  //   basr r14, r14A        2-bytes
  //   bkpt (0x0001)         2-bytes
  //
  // The 64bit sequence is a bit longer:
  //   lgr sp, fp            4-bytes
  //   lg  fp, 0(sp)         6-bytes
  //   lg  r14, 8(sp)        6-bytes
  //   la  sp, <delta>(sp)   4-bytes
  //   br  r14               2-bytes
  //
  // Will be patched with:
  //   iihf r14, <high 32-bits address>    6-bytes
  //   iilf r14, <lower 32-bits address>   6-bytes
  //   basr r14, r14         2-bytes
  //   bkpt (0x0001)         2-bytes
  CodePatcher patcher(pc(), Assembler::kJSReturnSequenceLength);
  Assembler::BlockTrampolinePoolScope block_trampoline_pool(patcher.masm());
// printf("SetDebugBreakAtReturn: pc=%08x\n", (unsigned int)pc());
  patcher.masm()->mov(
      v8::internal::r14,
      Operand(reinterpret_cast<intptr_t>(debug_info_->GetIsolate()
                                             ->builtins()
                                             ->Return_DebugBreak()
                                             ->entry())));
  patcher.masm()->basr(v8::internal::r14, v8::internal::r14);
  patcher.masm()->bkpt(0);
}


void BreakLocation::SetDebugBreakAtSlot() {
  DCHECK(IsDebugBreakSlot());
  // Patch the code changing the debug break slot code from
  //
  //   oill r3, 0
  //   oill r3, 0
  //   oill r3, 0   64-bit only
  //   lr r0, r0    64-bit only
  //
  // to a call to the debug break code, using a FIXED_SEQUENCE.
  //
  //   iilf r14, <address>   6-bytes
  //   basr r14, r14A        2-bytes
  //
  // The 64bit sequence has an extra iihf.
  //
  //   iihf r14, <high 32-bits address>    6-bytes
  //   iilf r14, <lower 32-bits address>   6-bytes
  //   basr r14, r14         2-bytes
  CodePatcher patcher(pc(), Assembler::kDebugBreakSlotLength);
// printf("SetDebugBreakAtSlot: pc=%08x\n", (unsigned int)rinfo()->pc());
  Assembler::BlockTrampolinePoolScope block_trampoline_pool(patcher.masm());
  patcher.masm()->mov(
      v8::internal::r14,
      Operand(reinterpret_cast<intptr_t>(
          debug_info_->GetIsolate()->builtins()->Slot_DebugBreak()->entry())));
  patcher.masm()->basr(v8::internal::r14, v8::internal::r14);
}


#define __ ACCESS_MASM(masm)


static void Generate_DebugBreakCallHelper(MacroAssembler* masm,
                                          RegList object_regs,
                                          RegList non_object_regs) {
  {
    FrameScope scope(masm, StackFrame::INTERNAL);

    // Load padding words on stack.
    __ LoadSmiLiteral(ip, Smi::FromInt(LiveEdit::kFramePaddingValue));
    for (int i = 0; i < LiveEdit::kFramePaddingInitialSize; i++) {
      __ push(ip);
    }
    __ LoadSmiLiteral(ip, Smi::FromInt(LiveEdit::kFramePaddingInitialSize));
    __ push(ip);

    // Store the registers containing live values on the expression stack to
    // make sure that these are correctly updated during GC. Non object values
    // are stored as a smi causing it to be untouched by GC.
    DCHECK((object_regs & ~kJSCallerSaved) == 0);
    DCHECK((non_object_regs & ~kJSCallerSaved) == 0);
    DCHECK((object_regs & non_object_regs) == 0);
    if ((object_regs | non_object_regs) != 0) {
      for (int i = 0; i < kNumJSCallerSaved; i++) {
        int r = JSCallerSavedCode(i);
        Register reg = {r};
        if ((non_object_regs & (1 << r)) != 0) {
          if (FLAG_debug_code) {
            __ TestUnsignedSmiCandidate(reg, r0);
            __ Assert(eq, kUnableToEncodeValueAsSmi, cr0);
          }
          __ SmiTag(reg);
        }
      }
      __ MultiPush(object_regs | non_object_regs);
    }

#ifdef DEBUG
    __ RecordComment("// Calling from debug break to runtime - come in - over");
#endif
    __ mov(r2, Operand::Zero());  // no arguments
    __ mov(r3, Operand(ExternalReference::debug_break(masm->isolate())));

    CEntryStub ceb(masm->isolate(), 1);
    __ CallStub(&ceb);

    // Restore the register values from the expression stack.
    if ((object_regs | non_object_regs) != 0) {
      __ MultiPop(object_regs | non_object_regs);
      for (int i = 0; i < kNumJSCallerSaved; i++) {
        int r = JSCallerSavedCode(i);
        Register reg = {r};
        if ((non_object_regs & (1 << r)) != 0) {
          __ SmiUntag(reg);
        }
        if (FLAG_debug_code &&
            (((object_regs | non_object_regs) & (1 << r)) == 0)) {
          __ mov(reg, Operand(kDebugZapValue));
        }
      }
    }

    // Don't bother removing padding bytes pushed on the stack
    // as the frame is going to be restored right away.

    // Leave the internal frame.
  }

  // Now that the break point has been handled, resume normal execution by
  // jumping to the target address intended by the caller and that was
  // overwritten by the address of DebugBreakXXX.
  ExternalReference after_break_target =
      ExternalReference::debug_after_break_target_address(masm->isolate());
  __ mov(ip, Operand(after_break_target));
  __ LoadP(ip, MemOperand(ip));
  __ JumpToJSEntry(ip);
}


void DebugCodegen::GenerateCallICStubDebugBreak(MacroAssembler* masm) {
  // Register state for CallICStub
  // ----------- S t a t e -------------
  //  -- r3 : function
  //  -- r5 : slot in feedback array (smi)
  // -----------------------------------
  Generate_DebugBreakCallHelper(masm, r3.bit() | r5.bit(), 0);
}


void DebugCodegen::GenerateReturnDebugBreak(MacroAssembler* masm) {
  // In places other than IC call sites it is expected that r2 is TOS which
  // is an object - this is not generally the case so this should be used with
  // care.
  Generate_DebugBreakCallHelper(masm, r2.bit(), 0);
}


void DebugCodegen::GenerateCallFunctionStubDebugBreak(MacroAssembler* masm) {
  // Register state for CallFunctionStub (from code-stubs-s390.cc).
  // ----------- S t a t e -------------
  //  -- r3 : function
  // -----------------------------------
  Generate_DebugBreakCallHelper(masm, r3.bit(), 0);
}


void DebugCodegen::GenerateCallConstructStubDebugBreak(MacroAssembler* masm) {
  // Calling convention for CallConstructStub (from code-stubs-s390.cc)
  // ----------- S t a t e -------------
  //  -- r2     : number of arguments (not smi)
  //  -- r3     : constructor function
  // -----------------------------------
  Generate_DebugBreakCallHelper(masm, r3.bit(), r2.bit());
}


void DebugCodegen::GenerateCallConstructStubRecordDebugBreak(
    MacroAssembler* masm) {
  // Calling convention for CallConstructStub (from code-stubs-s390.cc)
  // ----------- S t a t e -------------
  //  -- r2     : number of arguments (not smi)
  //  -- r3     : constructor function
  //  -- r4     : feedback array
  //  -- r5     : feedback slot (smi)
  // -----------------------------------
  Generate_DebugBreakCallHelper(masm, r3.bit() | r4.bit() | r5.bit(), r2.bit());
}


void DebugCodegen::GenerateSlot(MacroAssembler* masm) {
  // Generate enough nop's to make space for a call instruction. Avoid emitting
  // the trampoline pool in the debug break slot code.
  Assembler::BlockTrampolinePoolScope block_trampoline_pool(masm);
  Label check_codesize;
  __ bind(&check_codesize);
  __ RecordDebugBreakSlot();
  for (int i = 0; i < Assembler::kDebugBreakSlotLength / 4; i++) {
    __ nop(MacroAssembler::DEBUG_BREAK_NOP);
  }
  if (Assembler::kDebugBreakSlotLength % 4 != 0) {
    __ nop();   // Generate a 2-byte NOP
  }
  DCHECK_EQ(Assembler::kDebugBreakSlotLength,
            masm->SizeOfCodeGeneratedSince(&check_codesize));
}


void DebugCodegen::GenerateSlotDebugBreak(MacroAssembler* masm) {
  // In the places where a debug break slot is inserted no registers can contain
  // object pointers.
  Generate_DebugBreakCallHelper(masm, 0, 0);
}


void DebugCodegen::GeneratePlainReturnLiveEdit(MacroAssembler* masm) {
  __ Ret();
}


void DebugCodegen::GenerateFrameDropperLiveEdit(MacroAssembler* masm) {
  ExternalReference restarter_frame_function_slot =
      ExternalReference::debug_restarter_frame_function_pointer_address(
          masm->isolate());
  __ mov(ip, Operand(restarter_frame_function_slot));
  __ LoadImmP(r3, Operand::Zero());
  __ StoreP(r3, MemOperand(ip, 0));

  // Load the function pointer off of our current stack frame.
  __ LoadP(r3, MemOperand(fp, StandardFrameConstants::kConstantPoolOffset -
                                  kPointerSize));

  // Pop return address and frame
  __ LeaveFrame(StackFrame::INTERNAL);

  // Load context from the function.
  __ LoadP(cp, FieldMemOperand(r3, JSFunction::kContextOffset));

  // Get function code.
  __ LoadP(ip, FieldMemOperand(r3, JSFunction::kSharedFunctionInfoOffset));
  __ LoadP(ip, FieldMemOperand(ip, SharedFunctionInfo::kCodeOffset));
  __ AddP(ip, Operand(Code::kHeaderSize - kHeapObjectTag));

  // Re-run JSFunction, r3 is function, cp is context.
  __ Jump(ip);
}


const bool LiveEdit::kFrameDropperSupported = true;

#undef __
}  // namespace internal
}  // namespace v8

#endif  // V8_TARGET_ARCH_S390
