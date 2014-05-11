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

#include "codegen.h"
#include "deoptimizer.h"
#include "full-codegen.h"
#include "safepoint-table.h"

namespace v8 {
namespace internal {

#if V8_TARGET_ARCH_S390X
const int Deoptimizer::table_entry_size_ = 28;
#else
const int Deoptimizer::table_entry_size_ = 24;
#endif


int Deoptimizer::patch_size() {
#if V8_TARGET_ARCH_S390X
  const int kCallInstructionSize = 16;
#else
  const int kCallInstructionSize = 10;
#endif
  return kCallInstructionSize;
}


void Deoptimizer::DeoptimizeFunction(JSFunction* function) {
  HandleScope scope;
  AssertNoAllocation no_allocation;

  if (!function->IsOptimized()) return;

  // The optimized code is going to be patched, so we cannot use it
  // any more.  Play safe and reset the whole cache.
  function->shared()->ClearOptimizedCodeMap();

  // Get the optimized code.
  Code* code = function->code();
  Address code_start_address = code->instruction_start();

  // Invalidate the relocation information, as it will become invalid by the
  // code patching below, and is not needed any more.
  code->InvalidateRelocation();

  // For each LLazyBailout instruction insert a call to the corresponding
  // deoptimization entry.
  DeoptimizationInputData* deopt_data =
      DeoptimizationInputData::cast(code->deoptimization_data());
#ifdef DEBUG
  Address prev_call_address = NULL;
#endif
  for (int i = 0; i < deopt_data->DeoptCount(); i++) {
    if (deopt_data->Pc(i)->value() == -1) continue;
    Address call_address = code_start_address + deopt_data->Pc(i)->value();
    Address deopt_entry = GetDeoptimizationEntry(i, LAZY);
    // We need calls to have a predictable size in the unoptimized code, but
    // this is optimized code, so we don't have to have a predictable size.
    int call_size_in_bytes =
        MacroAssembler::CallSizeNotPredictableCodeSize(deopt_entry,
                                                       RelocInfo::NONE);
    ASSERT(call_size_in_bytes <= patch_size());
    CodePatcher patcher(call_address, call_size_in_bytes);  // FIXME: 2ND ARG
    patcher.masm()->Call(deopt_entry, RelocInfo::NONE);
    ASSERT(prev_call_address == NULL ||
           call_address >= prev_call_address + patch_size());
    ASSERT(call_address + patch_size() <= code->instruction_end());
#ifdef DEBUG
    prev_call_address = call_address;
#endif
  }

  Isolate* isolate = code->GetIsolate();

  // Add the deoptimizing code to the list.
  DeoptimizingCodeListNode* node = new DeoptimizingCodeListNode(code);
  DeoptimizerData* data = isolate->deoptimizer_data();
  node->set_next(data->deoptimizing_code_list_);
  data->deoptimizing_code_list_ = node;

  // We might be in the middle of incremental marking with compaction.
  // Tell collector to treat this code object in a special way and
  // ignore all slots that might have been recorded on it.
  isolate->heap()->mark_compact_collector()->InvalidateCode(code);

  ReplaceCodeForRelatedFunctions(function, code);

  if (FLAG_trace_deopt) {
    PrintF("[forced deoptimization: ");
    function->PrintName();
    PrintF(" / %" V8PRIxPTR "]\n", reinterpret_cast<uintptr_t>(function));
  }
}


#if V8_TARGET_ARCH_S390X
static const int32_t kBranchBeforeStackCheck = 0xa7a40009;
static const int32_t kBranchBeforeInterrupt =  0xa7a40015;
#else
static const int32_t kBranchBeforeStackCheck = 0xa7a40006;
static const int32_t kBranchBeforeInterrupt =  0xa7a4000e;
#endif


// This code has some dependency on the FIXED_SEQUENCE lis/ori
void Deoptimizer::PatchStackCheckCodeAt(Code* unoptimized_code,
                                        Address pc_after,
                                        Code* check_code,
                                        Code* replacement_code) {
  // There are two 'Stack check' sequences from full-codegen-s390.cc
  // both have similar code - and FLAG_count_based_interrupts will
  // control which we expect to find
  //
  // The call of the stack guard check has the following form:
  // a7a4XXXX       bge +18/+12 -> 876
  // c0c8XXXXXXXX   iihf    r12, xxxxxxxx ;; (64-bit) load upper 32-bits
  // c0c9XXXXXXXX   iilf    r12, xxxxxxxx
  // 0dec           basr    r14, r12
  //    <-- pc_after

  // Check we have a branch and save through r12
  ASSERT(Assembler::instr_at(pc_after - 2) == 0x0dec);

#if V8_TARGET_ARCH_S390X
  ASSERT(Assembler::Is64BitLoadIntoIP(
      Assembler::instr_at(pc_after - 14),
      Assembler::instr_at(pc_after - 8)));
  if (FLAG_count_based_interrupts) {
    ASSERT_EQ(kBranchBeforeInterrupt,
              Assembler::instr_at(pc_after - 18));
  } else {
    ASSERT_EQ(kBranchBeforeStackCheck,
              Assembler::instr_at(pc_after - 18));
  }
#else
  ASSERT(Assembler::Is32BitLoadIntoIP(
      Assembler::instr_at(pc_after - 8)));
  if (FLAG_count_based_interrupts) {
    ASSERT_EQ(kBranchBeforeInterrupt,
              Assembler::instr_at(pc_after - 12));
  } else {
    ASSERT_EQ(kBranchBeforeStackCheck,
              Assembler::instr_at(pc_after - 12));
  }
#endif

#if V8_TARGET_ARCH_S390X
  CodePatcher patcher(pc_after - 18, 16);  // 16 bytes - BRC + IIHF + IILF

  // Assemble the 64 bit value from the IIHF and IILF and verify
  // that it is the stack guard code
  uint64_t stack_check_address =
                     (Assembler::instr_at(pc_after - 14) & 0xFFFFFFFF) << 32;
  stack_check_address |= Assembler::instr_at(pc_after - 8) & 0xFFFFFFFF;
#else
  CodePatcher patcher(pc_after - 12, 10);  // 10 bytes - BRC + IILF

  // Assemble the 32 bit value from IILF and verify that it is the
  // stack guard code
  uint32_t stack_check_address =
                     Assembler::instr_at(pc_after - 8) & 0xFFFFFFFF;
#endif
  USE(stack_check_address);
  ASSERT(stack_check_address ==
    reinterpret_cast<uintptr_t>(check_code->entry()));

  // Replace conditional jump with NOP.
  patcher.masm()->nop();   // @TODO Can probably use a 4-byte NOP.
  patcher.masm()->nop();

  // Now modify the two part load (or 5 part on 64bit)
  patcher.masm()->mov(ip,
    Operand(reinterpret_cast<uintptr_t>(replacement_code->entry())));

#if V8_TARGET_ARCH_S390X
  unoptimized_code->GetHeap()->incremental_marking()->RecordCodeTargetPatch(
      unoptimized_code, pc_after - 14, replacement_code);
#else
  unoptimized_code->GetHeap()->incremental_marking()->RecordCodeTargetPatch(
      unoptimized_code, pc_after - 8, replacement_code);
#endif
}


void Deoptimizer::RevertStackCheckCodeAt(Code* unoptimized_code,
                                         Address pc_after,
                                         Code* check_code,
                                         Code* replacement_code) {
  // Check we have a branch & link through r12 (ip)
  //   BASR R14, R12
  ASSERT(Assembler::instr_at(pc_after - 2) == 0x0dec);

#if V8_TARGET_ARCH_S390X
  ASSERT(Assembler::Is64BitLoadIntoIP(
      Assembler::instr_at(pc_after - 14),
      Assembler::instr_at(pc_after - 8)));
#else
  ASSERT(Assembler::Is32BitLoadIntoIP(
      Assembler::instr_at(pc_after - 8)));
#endif

#if V8_TARGET_ARCH_S390X
  // Replace NOP with conditional jump.
  CodePatcher patcher(pc_after - 18, 16);  // 16 bytes - LR + LR + IIHF + IILF
  if (FLAG_count_based_interrupts) {
    patcher.masm()->brc(ge, Operand(42));          // Insert BRC
    ASSERT_EQ(kBranchBeforeInterrupt, Assembler::instr_at(pc_after - 18));
  } else {
    patcher.masm()->brc(ge, Operand(18));  //  , BF,
                // v8::internal::Assembler::encode_crbit(cr7, CR_LT));  // bge
    ASSERT_EQ(kBranchBeforeStackCheck, Assembler::instr_at(pc_after - 18));
  }
#else
  // Replace NOP with conditional jump.
  CodePatcher patcher(pc_after - 12, 10);  // 10 bytes - LR + LR + IILF
  if (FLAG_count_based_interrupts) {
    patcher.masm()->brc(ge, Operand(28));          // Insert BRC
    ASSERT_EQ(kBranchBeforeInterrupt, Assembler::instr_at(pc_after - 12));
  } else {
    patcher.masm()->brc(ge, Operand(12));
    ASSERT_EQ(kBranchBeforeStackCheck, Assembler::instr_at(pc_after - 12));
  }
#endif

#if V8_TARGET_ARCH_S390X
  // Assemble the 64 bit value from the IIHF and IILF and verify
  // that it is the stack guard code
  uint64_t replacement_code_address =
                     (Assembler::instr_at(pc_after - 14) & 0xFFFFFFFF) << 32;
  replacement_code_address |= Assembler::instr_at(pc_after - 8) & 0xFFFFFFFF;
#else
  // Assemble the 32 bit value from IILF and verify that it is the
  // stack guard code
  uint32_t replacement_code_address =
                     Assembler::instr_at(pc_after - 8) & 0xFFFFFFFF;
#endif
  USE(replacement_code_address);
  ASSERT(replacement_code_address ==
    reinterpret_cast<uintptr_t>(replacement_code->entry()));

  // Now modify the two part load (or 5 part on 64bit)
  patcher.masm()->mov(ip,
    Operand(reinterpret_cast<uintptr_t>(check_code->entry())));

#if V8_TARGET_ARCH_S390X
  check_code->GetHeap()->incremental_marking()->RecordCodeTargetPatch(
      unoptimized_code, pc_after - 14, check_code);
#else
  check_code->GetHeap()->incremental_marking()->RecordCodeTargetPatch(
      unoptimized_code, pc_after - 8, check_code);
#endif
}


static int LookupBailoutId(DeoptimizationInputData* data, BailoutId ast_id) {
  ByteArray* translations = data->TranslationByteArray();
  int length = data->DeoptCount();
  for (int i = 0; i < length; i++) {
    if (data->AstId(i) == ast_id) {
      TranslationIterator it(translations,  data->TranslationIndex(i)->value());
      int value = it.Next();
      ASSERT(Translation::BEGIN == static_cast<Translation::Opcode>(value));
      // Read the number of frames.
      value = it.Next();
      if (value == 1) return i;
    }
  }
  UNREACHABLE();
  return -1;
}


void Deoptimizer::DoComputeOsrOutputFrame() {
  DeoptimizationInputData* data = DeoptimizationInputData::cast(
      optimized_code_->deoptimization_data());
  unsigned ast_id = data->OsrAstId()->value();

  int bailout_id = LookupBailoutId(data, BailoutId(ast_id));
  unsigned translation_index = data->TranslationIndex(bailout_id)->value();
  ByteArray* translations = data->TranslationByteArray();

  TranslationIterator iterator(translations, translation_index);
  Translation::Opcode opcode =
      static_cast<Translation::Opcode>(iterator.Next());
  ASSERT(Translation::BEGIN == opcode);
  USE(opcode);
  int count = iterator.Next();
  iterator.Skip(1);  // Drop JS frame count.
  ASSERT(count == 1);
  USE(count);

  opcode = static_cast<Translation::Opcode>(iterator.Next());
  USE(opcode);
  ASSERT(Translation::JS_FRAME == opcode);
  unsigned node_id = iterator.Next();
  USE(node_id);
  ASSERT(node_id == ast_id);
  int closure_id = iterator.Next();
  USE(closure_id);
  ASSERT_EQ(Translation::kSelfLiteralId, closure_id);
  unsigned height = iterator.Next();
  unsigned height_in_bytes = height * kPointerSize;
  USE(height_in_bytes);

  unsigned fixed_size = ComputeFixedSize(function_);
  unsigned input_frame_size = input_->GetFrameSize();
  ASSERT(fixed_size + height_in_bytes == input_frame_size);

  unsigned stack_slot_size = optimized_code_->stack_slots() * kPointerSize;
  unsigned outgoing_height = data->ArgumentsStackHeight(bailout_id)->value();
  unsigned outgoing_size = outgoing_height * kPointerSize;
  unsigned output_frame_size = fixed_size + stack_slot_size + outgoing_size;
  ASSERT(outgoing_size == 0);  // OSR does not happen in the middle of a call.

  if (FLAG_trace_osr) {
    PrintF("[on-stack replacement: begin 0x%08" V8PRIxPTR " ",
           reinterpret_cast<intptr_t>(function_));
    function_->PrintName();
    PrintF(" => node=%u, frame=%d->%d]\n",
           ast_id,
           input_frame_size,
           output_frame_size);
  }

  // There's only one output frame in the OSR case.
  output_count_ = 1;
  output_ = new FrameDescription*[1];
  output_[0] = new(output_frame_size) FrameDescription(
      output_frame_size, function_);
  output_[0]->SetFrameType(StackFrame::JAVA_SCRIPT);

  // Clear the incoming parameters in the optimized frame to avoid
  // confusing the garbage collector.
  unsigned output_offset = output_frame_size - kPointerSize;
  int parameter_count = function_->shared()->formal_parameter_count() + 1;
  for (int i = 0; i < parameter_count; ++i) {
    output_[0]->SetFrameSlot(output_offset, 0);
    output_offset -= kPointerSize;
  }

  // Translate the incoming parameters. This may overwrite some of the
  // incoming argument slots we've just cleared.
  int input_offset = input_frame_size - kPointerSize;
  bool ok = true;
  int limit = input_offset - (parameter_count * kPointerSize);
  while (ok && input_offset > limit) {
    ok = DoOsrTranslateCommand(&iterator, &input_offset);
  }

  // There are no translation commands for the caller's pc and fp, the
  // context, and the function.  Set them up explicitly.
  for (int i =  StandardFrameConstants::kCallerPCOffset;
       ok && i >=  StandardFrameConstants::kMarkerOffset;
       i -= kPointerSize) {
    uintptr_t input_value = input_->GetFrameSlot(input_offset);
    if (FLAG_trace_osr) {
      const char* name = "UNKNOWN";
      switch (i) {
        case StandardFrameConstants::kCallerPCOffset:
          name = "caller's pc";
          break;
        case StandardFrameConstants::kCallerFPOffset:
          name = "fp";
          break;
        case StandardFrameConstants::kContextOffset:
          name = "context";
          break;
        case StandardFrameConstants::kMarkerOffset:
          name = "function";
          break;
      }
      PrintF("    [sp + %d] <- 0x%08" V8PRIxPTR " ;"
             " [sp + %d] (fixed part - %s)\n",
             output_offset,
             input_value,
             input_offset,
             name);
    }

    output_[0]->SetFrameSlot(output_offset, input_->GetFrameSlot(input_offset));
    input_offset -= kPointerSize;
    output_offset -= kPointerSize;
  }

  // Translate the rest of the frame.
  while (ok && input_offset >= 0) {
    ok = DoOsrTranslateCommand(&iterator, &input_offset);
  }

  // If translation of any command failed, continue using the input frame.
  if (!ok) {
    delete output_[0];
    output_[0] = input_;
    output_[0]->SetPc(reinterpret_cast<uintptr_t>(from_));
  } else {
    // Set up the frame pointer and the context pointer.
    output_[0]->SetRegister(fp.code(), input_->GetRegister(fp.code()));
    output_[0]->SetRegister(cp.code(), input_->GetRegister(cp.code()));

    unsigned pc_offset = data->OsrPcOffset()->value();
    uintptr_t pc = reinterpret_cast<uintptr_t>(
        optimized_code_->entry() + pc_offset);
    output_[0]->SetPc(pc);
  }
  Code* continuation = isolate_->builtins()->builtin(Builtins::kNotifyOSR);
  output_[0]->SetContinuation(
      reinterpret_cast<uintptr_t>(continuation->entry()));

  if (FLAG_trace_osr) {
    PrintF("[on-stack replacement translation %s: 0x%08" V8PRIxPTR " ",
           ok ? "finished" : "aborted",
           reinterpret_cast<intptr_t>(function_));
    function_->PrintName();
    PrintF(" => pc=0x%0" V8PRIxPTR "]\n", output_[0]->GetPc());
  }
}


void Deoptimizer::DoComputeArgumentsAdaptorFrame(TranslationIterator* iterator,
                                                 int frame_index) {
  JSFunction* function = JSFunction::cast(ComputeLiteral(iterator->Next()));
  unsigned height = iterator->Next();
  unsigned height_in_bytes = height * kPointerSize;
  if (FLAG_trace_deopt) {
    PrintF("  translating arguments adaptor => height=%d\n", height_in_bytes);
  }

  unsigned fixed_frame_size = ArgumentsAdaptorFrameConstants::kFrameSize;
  unsigned output_frame_size = height_in_bytes + fixed_frame_size;

  // Allocate and store the output frame description.
  FrameDescription* output_frame =
      new(output_frame_size) FrameDescription(output_frame_size, function);
  output_frame->SetFrameType(StackFrame::ARGUMENTS_ADAPTOR);

  // Arguments adaptor can not be topmost or bottommost.
  ASSERT(frame_index > 0 && frame_index < output_count_ - 1);
  ASSERT(output_[frame_index] == NULL);
  output_[frame_index] = output_frame;

  // The top address of the frame is computed from the previous
  // frame's top and this frame's size.
  uintptr_t top_address;
  top_address = output_[frame_index - 1]->GetTop() - output_frame_size;
  output_frame->SetTop(top_address);

  // Compute the incoming parameter translation.
  int parameter_count = height;
  unsigned output_offset = output_frame_size;
  for (int i = 0; i < parameter_count; ++i) {
    output_offset -= kPointerSize;
    DoTranslateCommand(iterator, frame_index, output_offset);
  }

  // Read caller's PC from the previous frame.
  output_offset -= kPointerSize;
  intptr_t callers_pc = output_[frame_index - 1]->GetPc();
  output_frame->SetFrameSlot(output_offset, callers_pc);
  if (FLAG_trace_deopt) {
    PrintF("    0x%08" V8PRIxPTR ": [top + %d] <- 0x%08" V8PRIxPTR
           " ; caller's pc\n",
           top_address + output_offset, output_offset, callers_pc);
  }

  // Read caller's FP from the previous frame, and set this frame's FP.
  output_offset -= kPointerSize;
  intptr_t value = output_[frame_index - 1]->GetFp();
  output_frame->SetFrameSlot(output_offset, value);
  intptr_t fp_value = top_address + output_offset;
  output_frame->SetFp(fp_value);
  if (FLAG_trace_deopt) {
    PrintF("    0x%08" V8PRIxPTR ": [top + %d] <- 0x%08" V8PRIxPTR
           " ; caller's fp\n", fp_value, output_offset, value);
  }

  // A marker value is used in place of the context.
  output_offset -= kPointerSize;
  intptr_t context = reinterpret_cast<intptr_t>(
      Smi::FromInt(StackFrame::ARGUMENTS_ADAPTOR));
  output_frame->SetFrameSlot(output_offset, context);
  if (FLAG_trace_deopt) {
    PrintF("    0x%08" V8PRIxPTR ": [top + %d] <- 0x%08" V8PRIxPTR
           " ; context (adaptor sentinel)\n",
           top_address + output_offset, output_offset, context);
  }

  // The function was mentioned explicitly in the ARGUMENTS_ADAPTOR_FRAME.
  output_offset -= kPointerSize;
  value = reinterpret_cast<intptr_t>(function);
  output_frame->SetFrameSlot(output_offset, value);
  if (FLAG_trace_deopt) {
    PrintF("    0x%08" V8PRIxPTR ": [top + %d] <- 0x%08" V8PRIxPTR
           " ; function\n",
           top_address + output_offset, output_offset, value);
  }

  // Number of incoming arguments.
  output_offset -= kPointerSize;
  value = reinterpret_cast<uintptr_t>(Smi::FromInt(height - 1));
  output_frame->SetFrameSlot(output_offset, value);
  if (FLAG_trace_deopt) {
    PrintF("    0x%08" V8PRIxPTR ": [top + %d] <- 0x%08" V8PRIxPTR
           " ; argc (%d)\n",
           top_address + output_offset, output_offset, value, height - 1);
  }

  ASSERT(0 == output_offset);

  Builtins* builtins = isolate_->builtins();
  Code* adaptor_trampoline =
      builtins->builtin(Builtins::kArgumentsAdaptorTrampoline);
  uintptr_t pc = reinterpret_cast<uintptr_t>(
      adaptor_trampoline->instruction_start() +
      isolate_->heap()->arguments_adaptor_deopt_pc_offset()->value());
  output_frame->SetPc(pc);
}


void Deoptimizer::DoComputeConstructStubFrame(TranslationIterator* iterator,
                                              int frame_index) {
  Builtins* builtins = isolate_->builtins();
  Code* construct_stub = builtins->builtin(Builtins::kJSConstructStubGeneric);
  JSFunction* function = JSFunction::cast(ComputeLiteral(iterator->Next()));
  unsigned height = iterator->Next();
  unsigned height_in_bytes = height * kPointerSize;
  if (FLAG_trace_deopt) {
    PrintF("  translating construct stub => height=%d\n", height_in_bytes);
  }

  unsigned fixed_frame_size = 8 * kPointerSize;
  unsigned output_frame_size = height_in_bytes + fixed_frame_size;

  // Allocate and store the output frame description.
  FrameDescription* output_frame =
      new(output_frame_size) FrameDescription(output_frame_size, function);
  output_frame->SetFrameType(StackFrame::CONSTRUCT);

  // Construct stub can not be topmost or bottommost.
  ASSERT(frame_index > 0 && frame_index < output_count_ - 1);
  ASSERT(output_[frame_index] == NULL);
  output_[frame_index] = output_frame;

  // The top address of the frame is computed from the previous
  // frame's top and this frame's size.
  uintptr_t top_address;
  top_address = output_[frame_index - 1]->GetTop() - output_frame_size;
  output_frame->SetTop(top_address);

  // Compute the incoming parameter translation.
  int parameter_count = height;
  unsigned output_offset = output_frame_size;
  for (int i = 0; i < parameter_count; ++i) {
    output_offset -= kPointerSize;
    DoTranslateCommand(iterator, frame_index, output_offset);
  }

  // Read caller's PC from the previous frame.
  output_offset -= kPointerSize;
  intptr_t callers_pc = output_[frame_index - 1]->GetPc();
  output_frame->SetFrameSlot(output_offset, callers_pc);
  if (FLAG_trace_deopt) {
    PrintF("    0x%08" V8PRIxPTR ": [top + %d] <- 0x%08" V8PRIxPTR
           " ; caller's pc\n",
           top_address + output_offset, output_offset, callers_pc);
  }

  // Read caller's FP from the previous frame, and set this frame's FP.
  output_offset -= kPointerSize;
  intptr_t value = output_[frame_index - 1]->GetFp();
  output_frame->SetFrameSlot(output_offset, value);
  intptr_t fp_value = top_address + output_offset;
  output_frame->SetFp(fp_value);
  if (FLAG_trace_deopt) {
    PrintF("    0x%08" V8PRIxPTR ": [top + %d] <- 0x%08" V8PRIxPTR
           " ; caller's fp\n", fp_value, output_offset, value);
  }

  // The context can be gotten from the previous frame.
  output_offset -= kPointerSize;
  value = output_[frame_index - 1]->GetContext();
  output_frame->SetFrameSlot(output_offset, value);
  if (FLAG_trace_deopt) {
    PrintF("    0x%08" V8PRIxPTR ": [top + %d] <- 0x%08" V8PRIxPTR
           " ; context\n",
           top_address + output_offset, output_offset, value);
  }

  // A marker value is used in place of the function.
  output_offset -= kPointerSize;
  value = reinterpret_cast<intptr_t>(Smi::FromInt(StackFrame::CONSTRUCT));
  output_frame->SetFrameSlot(output_offset, value);
  if (FLAG_trace_deopt) {
    PrintF("    0x%08" V8PRIxPTR ": [top + %d] <- 0x%08" V8PRIxPTR
           " ; function (construct sentinel)\n",
           top_address + output_offset, output_offset, value);
  }

  // The output frame reflects a JSConstructStubGeneric frame.
  output_offset -= kPointerSize;
  value = reinterpret_cast<intptr_t>(construct_stub);
  output_frame->SetFrameSlot(output_offset, value);
  if (FLAG_trace_deopt) {
    PrintF("    0x%08" V8PRIxPTR ": [top + %d] <- 0x%08" V8PRIxPTR
           " ; code object\n",
           top_address + output_offset, output_offset, value);
  }

  // Number of incoming arguments.
  output_offset -= kPointerSize;
  value = reinterpret_cast<uintptr_t>(Smi::FromInt(height - 1));
  output_frame->SetFrameSlot(output_offset, value);
  if (FLAG_trace_deopt) {
    PrintF("    0x%08" V8PRIxPTR ": [top + %d] <- 0x%08" V8PRIxPTR
           " ; argc (%d)\n",
           top_address + output_offset, output_offset, value, height - 1);
  }

  // Constructor function being invoked by the stub.
  output_offset -= kPointerSize;
  value = reinterpret_cast<intptr_t>(function);
  output_frame->SetFrameSlot(output_offset, value);
  if (FLAG_trace_deopt) {
    PrintF("    0x%08" V8PRIxPTR ": [top + %d] <- 0x%08" V8PRIxPTR
           " ; constructor function\n",
           top_address + output_offset, output_offset, value);
  }

  // The newly allocated object was passed as receiver in the artificial
  // constructor stub environment created by HEnvironment::CopyForInlining().
  output_offset -= kPointerSize;
  value = output_frame->GetFrameSlot(output_frame_size - kPointerSize);
  output_frame->SetFrameSlot(output_offset, value);
  if (FLAG_trace_deopt) {
    PrintF("    0x%08" V8PRIxPTR ": [top + %d] <- 0x%08" V8PRIxPTR
           " ; allocated receiver\n",
           top_address + output_offset, output_offset, value);
  }

  ASSERT(0 == output_offset);

  uintptr_t pc = reinterpret_cast<uintptr_t>(
      construct_stub->instruction_start() +
      isolate_->heap()->construct_stub_deopt_pc_offset()->value());
  output_frame->SetPc(pc);
}


void Deoptimizer::DoComputeAccessorStubFrame(TranslationIterator* iterator,
                                             int frame_index,
                                             bool is_setter_stub_frame) {
  JSFunction* accessor = JSFunction::cast(ComputeLiteral(iterator->Next()));
  // The receiver (and the implicit return value, if any) are expected in
  // registers by the LoadIC/StoreIC, so they don't belong to the output stack
  // frame. This means that we have to use a height of 0.
  unsigned height = 0;
  unsigned height_in_bytes = height * kPointerSize;
  const char* kind = is_setter_stub_frame ? "setter" : "getter";
  if (FLAG_trace_deopt) {
    PrintF("  translating %s stub => height=%u\n", kind, height_in_bytes);
  }

  // We need 5 stack entries from StackFrame::INTERNAL (lr, fp, cp, frame type,
  // code object, see MacroAssembler::EnterFrame). For a setter stub frames we
  // need one additional entry for the implicit return value, see
  // StoreStubCompiler::CompileStoreViaSetter.
  unsigned fixed_frame_entries = 5 + (is_setter_stub_frame ? 1 : 0);
  unsigned fixed_frame_size = fixed_frame_entries * kPointerSize;
  unsigned output_frame_size = height_in_bytes + fixed_frame_size;

  // Allocate and store the output frame description.
  FrameDescription* output_frame =
      new(output_frame_size) FrameDescription(output_frame_size, accessor);
  output_frame->SetFrameType(StackFrame::INTERNAL);

  // A frame for an accessor stub can not be the topmost or bottommost one.
  ASSERT(frame_index > 0 && frame_index < output_count_ - 1);
  ASSERT(output_[frame_index] == NULL);
  output_[frame_index] = output_frame;

  // The top address of the frame is computed from the previous frame's top and
  // this frame's size.
  uintptr_t top_address = (output_[frame_index - 1]->GetTop() -
                           output_frame_size);
  output_frame->SetTop(top_address);

  unsigned output_offset = output_frame_size;

  // Read caller's PC from the previous frame.
  output_offset -= kPointerSize;
  intptr_t callers_pc = output_[frame_index - 1]->GetPc();
  output_frame->SetFrameSlot(output_offset, callers_pc);
  if (FLAG_trace_deopt) {
    PrintF("    0x%08" V8PRIxPTR ": [top + %u] <- 0x%08" V8PRIxPTR
           " ; caller's pc\n",
           top_address + output_offset, output_offset, callers_pc);
  }

  // Read caller's FP from the previous frame, and set this frame's FP.
  output_offset -= kPointerSize;
  intptr_t value = output_[frame_index - 1]->GetFp();
  output_frame->SetFrameSlot(output_offset, value);
  intptr_t fp_value = top_address + output_offset;
  output_frame->SetFp(fp_value);
  if (FLAG_trace_deopt) {
    PrintF("    0x%08" V8PRIxPTR ": [top + %u] <- 0x%08" V8PRIxPTR
           " ; caller's fp\n",
           fp_value, output_offset, value);
  }

  // The context can be gotten from the previous frame.
  output_offset -= kPointerSize;
  value = output_[frame_index - 1]->GetContext();
  output_frame->SetFrameSlot(output_offset, value);
  if (FLAG_trace_deopt) {
    PrintF("    0x%08" V8PRIxPTR ": [top + %u] <- 0x%08" V8PRIxPTR
           " ; context\n",
           top_address + output_offset, output_offset, value);
  }

  // A marker value is used in place of the function.
  output_offset -= kPointerSize;
  value = reinterpret_cast<intptr_t>(Smi::FromInt(StackFrame::INTERNAL));
  output_frame->SetFrameSlot(output_offset, value);
  if (FLAG_trace_deopt) {
    PrintF("    0x%08" V8PRIxPTR ": [top + %u] <- 0x%08" V8PRIxPTR
           " ; function (%s sentinel)\n",
           top_address + output_offset, output_offset, value, kind);
  }

  // Get Code object from accessor stub.
  output_offset -= kPointerSize;
  Builtins::Name name = is_setter_stub_frame ?
      Builtins::kStoreIC_Setter_ForDeopt :
      Builtins::kLoadIC_Getter_ForDeopt;
  Code* accessor_stub = isolate_->builtins()->builtin(name);
  value = reinterpret_cast<intptr_t>(accessor_stub);
  output_frame->SetFrameSlot(output_offset, value);
  if (FLAG_trace_deopt) {
    PrintF("    0x%08" V8PRIxPTR ": [top + %u] <- 0x%08" V8PRIxPTR
           " ; code object\n",
           top_address + output_offset, output_offset, value);
  }

  // Skip receiver.
  Translation::Opcode opcode =
      static_cast<Translation::Opcode>(iterator->Next());
  iterator->Skip(Translation::NumberOfOperandsFor(opcode));

  if (is_setter_stub_frame) {
    // The implicit return value was part of the artificial setter stub
    // environment.
    output_offset -= kPointerSize;
    DoTranslateCommand(iterator, frame_index, output_offset);
  }

  ASSERT(0 == output_offset);

  Smi* offset = is_setter_stub_frame ?
      isolate_->heap()->setter_stub_deopt_pc_offset() :
      isolate_->heap()->getter_stub_deopt_pc_offset();
  intptr_t pc = reinterpret_cast<intptr_t>(
      accessor_stub->instruction_start() + offset->value());
  output_frame->SetPc(pc);
}


// This code is very similar to ia32 code, but relies on register names (fp, sp)
// and how the frame is laid out.
void Deoptimizer::DoComputeJSFrame(TranslationIterator* iterator,
                                   int frame_index) {
  // Read the ast node id, function, and frame height for this output frame.
  BailoutId node_id = BailoutId(iterator->Next());
  JSFunction* function;
  if (frame_index != 0) {
    function = JSFunction::cast(ComputeLiteral(iterator->Next()));
  } else {
    int closure_id = iterator->Next();
    USE(closure_id);
    ASSERT_EQ(Translation::kSelfLiteralId, closure_id);
    function = function_;
  }
  unsigned height = iterator->Next();
  unsigned height_in_bytes = height * kPointerSize;
  if (FLAG_trace_deopt) {
    PrintF("  translating ");
    function->PrintName();
    PrintF(" => node=%d, height=%d\n", node_id.ToInt(), height_in_bytes);
  }

  // The 'fixed' part of the frame consists of the incoming parameters and
  // the part described by JavaScriptFrameConstants.
  unsigned fixed_frame_size = ComputeFixedSize(function);
  unsigned input_frame_size = input_->GetFrameSize();
  unsigned output_frame_size = height_in_bytes + fixed_frame_size;

  // Allocate and store the output frame description.
  FrameDescription* output_frame =
      new(output_frame_size) FrameDescription(output_frame_size, function);
  output_frame->SetFrameType(StackFrame::JAVA_SCRIPT);

  bool is_bottommost = (0 == frame_index);
  bool is_topmost = (output_count_ - 1 == frame_index);
  ASSERT(frame_index >= 0 && frame_index < output_count_);
  ASSERT(output_[frame_index] == NULL);
  output_[frame_index] = output_frame;

  // The top address for the bottommost output frame can be computed from
  // the input frame pointer and the output frame's height.  For all
  // subsequent output frames, it can be computed from the previous one's
  // top address and the current frame's size.
  uintptr_t top_address;
  if (is_bottommost) {
    // 2 = context and function in the frame.
    top_address =
        input_->GetRegister(fp.code()) - (2 * kPointerSize) - height_in_bytes;
  } else {
    top_address = output_[frame_index - 1]->GetTop() - output_frame_size;
  }
  output_frame->SetTop(top_address);

  // Compute the incoming parameter translation.
  int parameter_count = function->shared()->formal_parameter_count() + 1;
  unsigned output_offset = output_frame_size;
  unsigned input_offset = input_frame_size;
  for (int i = 0; i < parameter_count; ++i) {
    output_offset -= kPointerSize;
    DoTranslateCommand(iterator, frame_index, output_offset);
  }
  input_offset -= (parameter_count * kPointerSize);

  // There are no translation commands for the caller's pc and fp, the
  // context, and the function.  Synthesize their values and set them up
  // explicitly.
  //
  // The caller's pc for the bottommost output frame is the same as in the
  // input frame.  For all subsequent output frames, it can be read from the
  // previous one.  This frame's pc can be computed from the non-optimized
  // function code and AST id of the bailout.
  output_offset -= kPointerSize;
  input_offset -= kPointerSize;
  intptr_t value;
  if (is_bottommost) {
    value = input_->GetFrameSlot(input_offset);
  } else {
    value = output_[frame_index - 1]->GetPc();
  }
  output_frame->SetFrameSlot(output_offset, value);
  if (FLAG_trace_deopt) {
    PrintF("    0x%08" V8PRIxPTR ": [top + %d] <- 0x%08" V8PRIxPTR
           " ; caller's pc\n",
           top_address + output_offset, output_offset, value);
  }

  // The caller's frame pointer for the bottommost output frame is the same
  // as in the input frame.  For all subsequent output frames, it can be
  // read from the previous one.  Also compute and set this frame's frame
  // pointer.
  output_offset -= kPointerSize;
  input_offset -= kPointerSize;
  if (is_bottommost) {
    value = input_->GetFrameSlot(input_offset);
  } else {
    value = output_[frame_index - 1]->GetFp();
  }
  output_frame->SetFrameSlot(output_offset, value);
  intptr_t fp_value = top_address + output_offset;
  ASSERT(!is_bottommost || input_->GetRegister(fp.code()) == fp_value);
  output_frame->SetFp(fp_value);
  if (is_topmost) {
    output_frame->SetRegister(fp.code(), fp_value);
  }
  if (FLAG_trace_deopt) {
    PrintF("    0x%08" V8PRIxPTR ": [top + %d] <- 0x%08" V8PRIxPTR
           " ; caller's fp\n", fp_value, output_offset, value);
  }

  // For the bottommost output frame the context can be gotten from the input
  // frame. For all subsequent output frames it can be gotten from the function
  // so long as we don't inline functions that need local contexts.
  output_offset -= kPointerSize;
  input_offset -= kPointerSize;
  if (is_bottommost) {
    value = input_->GetFrameSlot(input_offset);
  } else {
    value = reinterpret_cast<intptr_t>(function->context());
  }
  output_frame->SetFrameSlot(output_offset, value);
  output_frame->SetContext(value);
  if (is_topmost) output_frame->SetRegister(cp.code(), value);
  if (FLAG_trace_deopt) {
    PrintF("    0x%08" V8PRIxPTR ": [top + %d] <- 0x%08" V8PRIxPTR
           " ; context\n",
           top_address + output_offset, output_offset, value);
  }

  // The function was mentioned explicitly in the BEGIN_FRAME.
  output_offset -= kPointerSize;
  input_offset -= kPointerSize;
  value = reinterpret_cast<uintptr_t>(function);
  // The function for the bottommost output frame should also agree with the
  // input frame.
  ASSERT(!is_bottommost || input_->GetFrameSlot(input_offset) == value);
  output_frame->SetFrameSlot(output_offset, value);
  if (FLAG_trace_deopt) {
    PrintF("    0x%08" V8PRIxPTR ": [top + %d] <- 0x%08" V8PRIxPTR
           " ; function\n",
           top_address + output_offset, output_offset, value);
  }

  // Translate the rest of the frame.
  for (unsigned i = 0; i < height; ++i) {
    output_offset -= kPointerSize;
    DoTranslateCommand(iterator, frame_index, output_offset);
  }
  ASSERT(0 == output_offset);

  // Compute this frame's PC, state, and continuation.
  Code* non_optimized_code = function->shared()->code();
  FixedArray* raw_data = non_optimized_code->deoptimization_data();
  DeoptimizationOutputData* data = DeoptimizationOutputData::cast(raw_data);
  Address start = non_optimized_code->instruction_start();
  unsigned pc_and_state = GetOutputInfo(data, node_id, function->shared());
  unsigned pc_offset = FullCodeGenerator::PcField::decode(pc_and_state);
  uintptr_t pc_value = reinterpret_cast<uintptr_t>(start + pc_offset);
  output_frame->SetPc(pc_value);
#if 0  // applicable on PPC?
  if (is_topmost) {
    output_frame->SetRegister(pc.code(), pc_value);
  }
#endif

  FullCodeGenerator::State state =
      FullCodeGenerator::StateField::decode(pc_and_state);
  output_frame->SetState(Smi::FromInt(state));


  // Set the continuation for the topmost frame.
  if (is_topmost && bailout_type_ != DEBUGGER) {
    Builtins* builtins = isolate_->builtins();
    Code* continuation = (bailout_type_ == EAGER)
        ? builtins->builtin(Builtins::kNotifyDeoptimized)
        : builtins->builtin(Builtins::kNotifyLazyDeoptimized);
    output_frame->SetContinuation(
        reinterpret_cast<uintptr_t>(continuation->entry()));
  }
}


void Deoptimizer::FillInputFrame(Address tos, JavaScriptFrame* frame) {
  // Set the register values. The values are not important as there are no
  // callee saved registers in JavaScript frames, so all registers are
  // spilled. Registers fp and sp are set to the correct values though.

  for (int i = 0; i < Register::kNumRegisters; i++) {
    input_->SetRegister(i, i * 4);
  }
  input_->SetRegister(sp.code(), reinterpret_cast<intptr_t>(frame->sp()));
  input_->SetRegister(fp.code(), reinterpret_cast<intptr_t>(frame->fp()));
  for (int i = 0; i < DoubleRegister::kNumAllocatableRegisters; i++) {
    input_->SetDoubleRegister(i, 0.0);
  }

  // Fill the frame content from the actual data on the frame.
  for (unsigned i = 0; i < input_->GetFrameSize(); i += kPointerSize) {
    input_->SetFrameSlot(i, reinterpret_cast<intptr_t>(
                           Memory::Address_at(tos + i)));
  }
}


#define __ masm()->

// This code tries to be close to ia32 code so that any changes can be
// easily ported.
void Deoptimizer::EntryGenerator::Generate() {
  GeneratePrologue();

  Isolate* isolate = masm()->isolate();

  // Save all the registers onto the stack
  const int kNumberOfRegisters = Register::kNumRegisters;

  RegList restored_regs = kJSCallerSaved | kCalleeSaved;

  const int kDoubleRegsSize =
      kDoubleSize * DoubleRegister::kNumAllocatableRegisters;

  // Save all FPU registers before messing with them.
  __ lay(sp, MemOperand(sp, -kDoubleRegsSize));
  for (int i = 0; i < DoubleRegister::kNumAllocatableRegisters; ++i) {
    DoubleRegister fpu_reg = DoubleRegister::FromAllocationIndex(i);
    int offset = i * kDoubleSize;
    __ StoreF(fpu_reg, MemOperand(sp, offset));
  }

  // Push all GPRs onto the stack
  __ lay(sp, MemOperand(sp, -kNumberOfRegisters * kPointerSize));
  __ StoreMultipleP(r0, sp, MemOperand(sp));   // Save all 16 registers

  const int kSavedRegistersAreaSize =
      (kNumberOfRegisters * kPointerSize) + kDoubleRegsSize;

  // Get the bailout id from the stack.
  __ LoadP(r4, MemOperand(sp, kSavedRegistersAreaSize));

  // Cleanse the Return address for 31-bit
  __ CleanseP(r14);

  // Get the address of the location in the code object if possible(r5)(return
  // address for lazy deoptimization) and compute the fp-to-sp delta in
  // register r6.
  if (type() == EAGER) {
    __ LoadImmP(r5, Operand::Zero());
    // Correct one word for bailout id.
    __ la(r6, MemOperand(sp, kSavedRegistersAreaSize + (1 * kPointerSize)));
  } else if (type() == OSR) {
    __ LoadRR(r5, r14);
    // Correct one word for bailout id.
    __ la(r6, MemOperand(sp, kSavedRegistersAreaSize + (1 * kPointerSize)));
  } else {
    __ LoadRR(r5, r14);
    // Correct two words for bailout id and return address.
    __ la(r6, MemOperand(sp, kSavedRegistersAreaSize + (2 * kPointerSize)));
  }
  __ Sub(r6, fp, r6);

  // Allocate a new deoptimizer object.
  // Pass six arguments in r2 to r7.
  __ PrepareCallCFunction(6, r7);
  __ LoadP(r2, MemOperand(fp, JavaScriptFrameConstants::kFunctionOffset));
  __ LoadImmP(r3, Operand(type()));  // bailout type,
  // r4: bailout id already loaded.
  // r5: code address or 0 already loaded.
  // r6: Fp-to-sp delta.
  // Parm6: isolate is passed on the stack.
  __ mov(r7, Operand(ExternalReference::isolate_address()));
  __ StoreP(r7, MemOperand(sp));

  // Call Deoptimizer::New().
  {
    AllowExternalCallThatCantCauseGC scope(masm());
    __ CallCFunction(ExternalReference::new_deoptimizer_function(isolate), 6);
  }

  // Preserve "deoptimizer" object in register r2 and get the input
  // frame descriptor pointer to r3 (deoptimizer->input_);
  __ LoadP(r3, MemOperand(r2, Deoptimizer::input_offset()));

  // Copy core registers into FrameDescription::registers_[kNumRegisters].
  ASSERT(Register::kNumRegisters == kNumberOfRegisters);
  __ mvc(MemOperand(r3, FrameDescription::registers_offset()), MemOperand(sp),
         kNumberOfRegisters * kPointerSize);
  /*
  for (int i = 0; i < kNumberOfRegisters; i++) {
    int offset = (i * kPointerSize) + FrameDescription::registers_offset();
    __ LoadP(r4, MemOperand(sp, i * kPointerSize));
    __ StoreP(r4, MemOperand(r3, offset));
  }
  */

  // Copy VFP registers to
  // double_registers_[DoubleRegister::kNumAllocatableRegisters]
  int double_regs_offset = FrameDescription::double_registers_offset();
  __ mvc(MemOperand(r3, double_regs_offset),
         MemOperand(sp, kNumberOfRegisters * kPointerSize),
         DoubleRegister::kNumAllocatableRegisters * kDoubleSize);
/*
  for (int i = 0; i < DoubleRegister::kNumAllocatableRegisters; ++i) {
    int dst_offset = i * kDoubleSize + double_regs_offset;
    int src_offset = i * kDoubleSize + kNumberOfRegisters * kPointerSize;
    __ LoadF(d0, MemOperand(sp, src_offset));
    __ StoreF(d0, MemOperand(r3, dst_offset));
  }
*/

  // Remove the bailout id, eventually return address, and the saved registers
  // from the stack.
  if (type() == EAGER || type() == OSR) {
    __ la(sp, MemOperand(sp, kSavedRegistersAreaSize + (1 * kPointerSize)));
  } else {
    __ la(sp, MemOperand(sp, kSavedRegistersAreaSize + (2 * kPointerSize)));
  }

  // Compute a pointer to the unwinding limit in register r4; that is
  // the first stack slot not part of the input frame.
  __ LoadP(r4, MemOperand(r3, FrameDescription::frame_size_offset()));
  __ AddP(r4, sp);

  // Unwind the stack down to - but not including - the unwinding
  // limit and copy the contents of the activation frame to the input
  // frame description.
  __ la(r5, MemOperand(r3, FrameDescription::frame_content_offset()));
  Label pop_loop;
  __ bind(&pop_loop);
  __ pop(r6);
  __ StoreP(r6, MemOperand(r5, 0));
  __ AddP(r5, Operand(sizeof(intptr_t)));
  __ CmpRR(r4, sp);
  __ bne(&pop_loop);

  // Compute the output frame in the deoptimizer.
  __ push(r2);  // Preserve deoptimizer object across call.
  // r2: deoptimizer object; r3: scratch.
  __ PrepareCallCFunction(1, r3);
  // Call Deoptimizer::ComputeOutputFrames().
  {
    AllowExternalCallThatCantCauseGC scope(masm());
    __ CallCFunction(
        ExternalReference::compute_output_frames_function(isolate), 1);
  }
  __ pop(r2);  // Restore deoptimizer object (class Deoptimizer).

  // Replace the current (input) frame with the output frames.
  Label outer_push_loop, inner_push_loop;
  // Outer loop state: r2 = current "FrameDescription** output_",
  // r3 = one past the last FrameDescription**.
  __ LoadlW(r3, MemOperand(r2, Deoptimizer::output_count_offset()));
  __ LoadP(r2,
           MemOperand(r2, Deoptimizer::output_offset()));  // r2 is output_.
  __ ShiftLeftImm(r3, r3, Operand(kPointerSizeLog2));
  __ AddP(r3, r2);
  __ bind(&outer_push_loop);
  // Inner loop state: r4 = current FrameDescription*, r5 = loop index.
  __ LoadP(r4, MemOperand(r2, 0));  // output_[ix]
  __ LoadP(r5, MemOperand(r4, FrameDescription::frame_size_offset()));

  __ bind(&inner_push_loop);
  __ AddP(r5, Operand(-sizeof(intptr_t)));
  __ LoadRR(r8, r4);
  __ AddP(r8, r5);
  __ LoadP(r9, MemOperand(r8, FrameDescription::frame_content_offset()));
  __ push(r9);
  __ Cmpi(r5, Operand::Zero());
  __ bne(&inner_push_loop);  // test for gt?

  __ AddP(r2, Operand(kPointerSize));
  __ CmpRR(r2, r3);
  __ blt(&outer_push_loop);

  // Push state, pc, and continuation from the last output frame.
  if (type() != OSR) {
    __ LoadP(r8, MemOperand(r4, FrameDescription::state_offset()));
    __ push(r8);
  }

  __ LoadP(r8, MemOperand(r4, FrameDescription::pc_offset()));
  __ push(r8);
  __ LoadP(r8, MemOperand(r4, FrameDescription::continuation_offset()));
  __ push(r8);

  // Restore the registers from the last output frame.
  ASSERT(!(ip.bit() & restored_regs));
  __ LoadRR(ip, r4);
  for (int i = kNumberOfRegisters - 1; i >= 0; i--) {
    int offset = (i * kPointerSize) + FrameDescription::registers_offset();
    if ((restored_regs & (1 << i)) != 0) {
      __ LoadP(ToRegister(i), MemOperand(ip, offset));
    }
  }

  __ InitializeRootRegister();

  __ pop(r9);  // get continuation, leave pc on stack
  __ pop(r14);
  __ Jump(r9);
  __ stop("Unreachable.");
}


void Deoptimizer::TableEntryGenerator::GeneratePrologue() {
  Assembler::BlockTrampolinePoolScope block_trampoline_pool(masm());

  // Create a sequence of deoptimization entries. Note that any
  // registers may be still live.
  Label done;
  for (int i = 0; i < count(); i++) {
    int start = masm()->pc_offset();
    USE(start);
    if (type() == EAGER) {
      __ lay(sp, MemOperand(sp, -kPointerSize));
      __ nop();
      __ nop();
#if V8_TARGET_ARCH_S390X
      __ nop();    // Should probably make it a 6-byte NOP
#endif
    } else {
      __ lay(sp, MemOperand(sp, -2 * kPointerSize));
      // Emulate ia32 like call by pushing return address to stack.
      __ StoreP(r14, MemOperand(sp, kPointerSize));
    }
    __ LoadImmP(ip, Operand(i));
    __ StoreP(ip, MemOperand(sp));
    __ b(&done);
    int end = masm()->pc_offset();
    USE(end);
    ASSERT(masm()->pc_offset() - start == table_entry_size_);
  }
  __ bind(&done);
}

#undef __

} }  // namespace v8::internal
