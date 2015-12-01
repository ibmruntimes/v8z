// Copyright 2015 the V8 project authors. All rights reserved.
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.

#include "src/v8.h"

#if V8_TARGET_ARCH_S390

#include "src/codegen.h"
#include "src/debug/debug.h"

namespace v8 {
namespace internal {

#define __ ACCESS_MASM(masm)


void EmitDebugBreakSlot(MacroAssembler* masm) {
  Label check_size;
  __ bind(&check_size);
  for (int i = 0; i < Assembler::kDebugBreakSlotInstructions * 3; i++) {
    // 2 bytes * 3 to reserve enough space for 6 byte instructions.
    __ nop(MacroAssembler::DEBUG_BREAK_NOP);
  }
  DCHECK_EQ(Assembler::kDebugBreakSlotInstructions * 3,
            masm->SizeOfCodeGeneratedSince(&check_size));
}


void DebugCodegen::GenerateSlot(MacroAssembler* masm, RelocInfo::Mode mode,
                                int call_argc) {
  // Generate enough nop's to make space for a call instruction. Avoid emitting
  // the trampoline pool in the debug break slot code.
  Assembler::BlockTrampolinePoolScope block_trampoline_pool(masm);
  masm->RecordDebugBreakSlot(mode, call_argc);
  EmitDebugBreakSlot(masm);
}


void DebugCodegen::ClearDebugBreakSlot(Isolate* isolate, Address pc) {
  CodePatcher patcher(isolate, pc, Assembler::kDebugBreakSlotInstructions);
  EmitDebugBreakSlot(patcher.masm());
}


void DebugCodegen::PatchDebugBreakSlot(Isolate* isolate, Address pc,
                                       Handle<Code> code) {
  DCHECK_EQ(Code::BUILTIN, code->kind());
  CodePatcher patcher(isolate, pc, Assembler::kDebugBreakSlotInstructions);
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
// printf("SetDebugBreakAtSlot: pc=%08x\n", (unsigned int)rinfo()->pc());
  Assembler::BlockTrampolinePoolScope block_trampoline_pool(patcher.masm());
  patcher.masm()->mov(
      v8::internal::r14,
      Operand(reinterpret_cast<intptr_t>(code->entry())));
  patcher.masm()->basr(v8::internal::r14, v8::internal::r14);
}


void DebugCodegen::GenerateDebugBreakStub(MacroAssembler* masm,
                                          DebugBreakCallHelperMode mode) {
  __ RecordComment("Debug break");
  {
    FrameScope scope(masm, StackFrame::INTERNAL);

    // Load padding words on stack.
    __ LoadSmiLiteral(ip, Smi::FromInt(LiveEdit::kFramePaddingValue));
    for (int i = 0; i < LiveEdit::kFramePaddingInitialSize; i++) {
      __ push(ip);
    }
    __ LoadSmiLiteral(ip, Smi::FromInt(LiveEdit::kFramePaddingInitialSize));
    __ push(ip);

    if (mode == SAVE_RESULT_REGISTER) __ push(r2);

    __ mov(r2, Operand::Zero());  // no arguments
    __ mov(r3,
           Operand(ExternalReference(
               Runtime::FunctionForId(Runtime::kDebugBreak), masm->isolate())));

    CEntryStub ceb(masm->isolate(), 1);
    __ CallStub(&ceb);

    if (FLAG_debug_code) {
      for (int i = 0; i < kNumJSCallerSaved; i++) {
        Register reg = {JSCallerSavedCode(i)};
        __ mov(reg, Operand(kDebugZapValue));
      }
    }

    if (mode == SAVE_RESULT_REGISTER) __ pop(r2);

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
