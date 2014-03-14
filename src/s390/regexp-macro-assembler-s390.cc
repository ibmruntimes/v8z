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

#include "unicode.h"
#include "log.h"
#include "code-stubs.h"
#include "regexp-stack.h"
#include "macro-assembler.h"
#include "regexp-macro-assembler.h"
#include "s390/regexp-macro-assembler-s390.h"

namespace v8 {
namespace internal {

#ifndef V8_INTERPRETED_REGEXP
/*
 * This assembler uses the following register assignment convention
 * - r25_p: Temporarily stores the index of capture start after a matching pass
 *        for a global regexp.
 * - r26_p: Pointer to current code object (Code*) including heap object tag.
 * - r27_p: Current position in input, as negative offset from end of string.
 *        Please notice that this is the byte offset, not the character offset!
 * - r28_p: Currently loaded character. Must be loaded using
 *        LoadCurrentCharacter before using any of the dispatch methods.
 * - r29_p: Points to tip of backtrack stack
 * - r30_p: End of input (points to byte after last character in input).
 * - r31_p: Frame pointer. Used to access arguments, local variables and
 *         RegExp registers.
 * - r12_p: IP register, used by assembler. Very volatile.
 * - r1_p/sp : Points to tip of C stack.
 *
 * The remaining registers are free for computations.
 * Each call to a public method should retain this convention.
 *
 * The stack will have the following structure:
 *  - fp[44]  Isolate* isolate   (address of the current isolate)
 *  - fp[40]  secondary link/return address used by native call.
 *  - fp[36]  lr save area (currently unused)
 *  - fp[32]  backchain    (currently unused)
 *  --- sp when called ---
 *  - fp[28]  return address     (lr).
 *  - fp[24]  old frame pointer  (r31_p).
 *  - fp[0..20]  backup of registers r25_p..r30_p
 *  --- frame pointer ----
 *  - fp[-4]  direct_call        (if 1, direct call from JavaScript code,
 *                                if 0, call through the runtime system).
 *  - fp[-8]  stack_area_base    (high end of the memory area to use as
 *                                backtracking stack).
 *  - fp[-12] capture array size (may fit multiple sets of matches)
 *  - fp[-16] int* capture_array (int[num_saved_registers_], for output).
 *  - fp[-20] end of input       (address of end of string).
 *  - fp[-24] start of input     (address of first character in string).
 *  - fp[-28] start index        (character index of start).
 *  - fp[-32] void* input_string (location of a handle containing the string).
 *  - fp[-36] success counter    (only for global regexps to count matches).
 *  - fp[-40] Offset of location before start of input (effectively character
 *            position -1). Used to initialize capture registers to a
 *            non-position.
 *  - fp[-44] At start (if 1, we are starting at the start of the
 *    string, otherwise 0)
 *  - fp[-48] register 0         (Only positions must be stored in the first
 *  -         register 1          num_saved_registers_ registers)
 *  -         ...
 *  -         register num_registers-1
 *  --- sp ---
 *
 * The first num_saved_registers_ registers are initialized to point to
 * "character -1" in the string (i.e., char_size() bytes before the first
 * character of the string). The remaining registers start out as garbage.
 *
 * The data up to the return address must be placed there by the calling
 * code and the remaining arguments are passed in registers, e.g. by calling the
 * code entry as cast to a function with the signature:
 * int (*match)(String* input_string,
 *              int start_index,
 *              Address start,
 *              Address end,
 *              int* capture_output_array,
 *              byte* stack_area_base,
 *              Address secondary_return_address,  // Only used by native call.
 *              bool direct_call = false)
 * The call is performed by NativeRegExpMacroAssembler::Execute()
 * (in regexp-macro-assembler.cc) via the CALL_GENERATED_REGEXP_CODE macro
 * in ppc/simulator-ppc.h.
 * When calling as a non-direct call (i.e., from C++ code), the return address
 * area is overwritten with the LR register by the RegExp code. When doing a
 * direct call from generated code, the return address is placed there by
 * the calling code, as in a normal exit frame.
 */

#define __ ACCESS_MASM(masm_)

RegExpMacroAssemblerPPC::RegExpMacroAssemblerPPC(
    Mode mode,
    int registers_to_save,
    Zone* zone)
    : NativeRegExpMacroAssembler(zone),
      masm_(new MacroAssembler(Isolate::Current(), NULL, kRegExpCodeSize)),
      mode_(mode),
      num_registers_(registers_to_save),
      num_saved_registers_(registers_to_save),
      entry_label_(),
      start_label_(),
      success_label_(),
      backtrack_label_(),
      exit_label_(),
      internal_failure_label_() {
  ASSERT_EQ(0, registers_to_save % 2);

  // Called from C
#if ABI_USES_FUNCTION_DESCRIPTORS
  __ function_descriptor();
#endif

  __ b(&entry_label_);   // We'll write the entry code later.
  // If the code gets too big or corrupted, an internal exception will be
  // raised, and we will exit right away.
  __ bind(&internal_failure_label_);
  __ LoadImmP(r3_p, Operand(FAILURE));
  __ Ret();
  __ bind(&start_label_);  // And then continue from here.
}


RegExpMacroAssemblerPPC::~RegExpMacroAssemblerPPC() {
  delete masm_;
  // Unuse labels in case we throw away the assembler without calling GetCode.
  entry_label_.Unuse();
  start_label_.Unuse();
  success_label_.Unuse();
  backtrack_label_.Unuse();
  exit_label_.Unuse();
  check_preempt_label_.Unuse();
  stack_overflow_label_.Unuse();
  internal_failure_label_.Unuse();
}


int RegExpMacroAssemblerPPC::stack_limit_slack()  {
  return RegExpStack::kStackLimitSlack;
}


void RegExpMacroAssemblerPPC::AdvanceCurrentPosition(int by) {
  if (by != 0) {
    __ AddP(current_input_offset(), Operand(by * char_size()));
  }
}


void RegExpMacroAssemblerPPC::AdvanceRegister(int reg, int by) {
  ASSERT(reg >= 0);
  ASSERT(reg < num_registers_);
  if (by != 0) {
    __ LoadP(r3_p, register_location(reg), r0_p);
    __ mov(r0_p, Operand(by));
    __ AddRR(r3_p, r0_p);
    __ StoreP(r3_p, register_location(reg));
  }
}


void RegExpMacroAssemblerPPC::Backtrack() {
  CheckPreemption();
  // Pop Code* offset from backtrack stack, add Code* and jump to location.
  Pop(r3_p);
  __ AddP(r3_p, code_pointer());
  __ mtctr(r3_p);
  __ bcr();
}


void RegExpMacroAssemblerPPC::Bind(Label* label) {
  __ bind(label);
}


void RegExpMacroAssemblerPPC::CheckCharacter(uint32_t c, Label* on_equal) {
  __ Cmpli(current_character(), Operand(c));
  BranchOrBacktrack(eq, on_equal);
}


void RegExpMacroAssemblerPPC::CheckCharacterGT(uc16 limit, Label* on_greater) {
  __ Cmpli(current_character(), Operand(limit));
  BranchOrBacktrack(gt, on_greater);
}


void RegExpMacroAssemblerPPC::CheckAtStart(Label* on_at_start) {
  Label not_at_start;
  // Did we start the match at the start of the string at all?
  __ LoadP(r3_p, MemOperand(frame_pointer(), kStartIndex));
  __ Cmpi(r3_p, Operand::Zero());
  BranchOrBacktrack(ne, &not_at_start);

  // If we did, are we still at the start of the input?
  __ LoadP(r4_p, MemOperand(frame_pointer(), kInputStart));
  __ LoadRR(r0_p, current_input_offset());
  __ LoadRR(r3_p, end_of_input_address());
  __ AddP(r3_p, r0_p);
  __ CmpRR(r4_p, r3_p);
  BranchOrBacktrack(eq, on_at_start);
  __ bind(&not_at_start);
}


void RegExpMacroAssemblerPPC::CheckNotAtStart(Label* on_not_at_start) {
  // Did we start the match at the start of the string at all?
  __ LoadP(r3_p, MemOperand(frame_pointer(), kStartIndex));
  __ Cmpi(r3_p, Operand::Zero());
  BranchOrBacktrack(ne, on_not_at_start);
  // If we did, are we still at the start of the input?
  __ LoadP(r4_p, MemOperand(frame_pointer(), kInputStart));
  __ LoadRR(r3_p, end_of_input_address());
  __ AddP(r3_p, current_input_offset());
  __ CmpRR(r3_p, r4_p);
  BranchOrBacktrack(ne, on_not_at_start);
}


void RegExpMacroAssemblerPPC::CheckCharacterLT(uc16 limit, Label* on_less) {
  __ Cmpli(current_character(), Operand(limit));
  BranchOrBacktrack(lt, on_less);
}


void RegExpMacroAssemblerPPC::CheckCharacters(Vector<const uc16> str,
                                              int cp_offset,
                                              Label* on_failure,
                                              bool check_end_of_string) {
  if (on_failure == NULL) {
    // Instead of inlining a backtrack for each test, (re)use the global
    // backtrack target.
    on_failure = &backtrack_label_;
  }

  if (check_end_of_string) {
    // Is last character of required match inside string.
    CheckPosition(cp_offset + str.length() - 1, on_failure);
  }

  __ LoadRR(r3_p, end_of_input_address());
  __ AddP(r3_p, current_input_offset());
  if (cp_offset != 0) {
    int byte_offset = cp_offset * char_size();
    __ AddP(r3_p, Operand(byte_offset));
  }

  // r3_p : Address of characters to match against str.
  int stored_high_byte = 0;
  for (int i = 0; i < str.length(); i++) {
    if (mode_ == ASCII) {
      __ LoadlB(r4_p, MemOperand(r3_p));
      __ AddP(r3_p, Operand(char_size()));
      ASSERT(str[i] <= String::kMaxAsciiCharCode);
      __ Cmpi(r4_p, Operand(str[i]));
    } else {
      __ lhz(r4_p, MemOperand(r3_p));
      __ AddP(r3_p, Operand(char_size()));
      uc16 match_char = str[i];
      int match_high_byte = (match_char >> 8);
      if (match_high_byte == 0) {
        __ Cmpi(r4_p, Operand(str[i]));
      } else {
        if (match_high_byte != stored_high_byte) {
          __ LoadImmP(r5_p, Operand(match_high_byte));
          stored_high_byte = match_high_byte;
        }
        __ LoadRR(r6_p, r5_p);
        __ AddP(r6_p, Operand(match_char & 0xff));
        __ CmpRR(r4_p, r6_p);
      }
    }
    BranchOrBacktrack(ne, on_failure);
  }
}


void RegExpMacroAssemblerPPC::CheckGreedyLoop(Label* on_equal) {
  Label backtrack_non_equal;
  __ LoadP(r3_p, MemOperand(backtrack_stackpointer(), 0));
  __ CmpRR(current_input_offset(), r3_p);
  __ bne(&backtrack_non_equal);
  __ AddP(backtrack_stackpointer(), Operand(kPointerSize));

  __ bind(&backtrack_non_equal);
  BranchOrBacktrack(eq, on_equal);
}


void RegExpMacroAssemblerPPC::CheckNotBackReferenceIgnoreCase(
    int start_reg,
    Label* on_no_match) {
  Label fallthrough;
  __ LoadP(r3_p, register_location(start_reg), r0_p);  // Index of start of
                                                       // capture
  __ LoadP(r4_p, register_location(start_reg + 1), r0_p);  // Index of end
  // Removing RC looks Okay here.
  __ Sub(r4_p, r4_p, r3_p/*, LeaveOE, SetRC*/);  // Length of capture.

  // If length is zero, either the capture is empty or it is not participating.
  // In either case succeed immediately.
  __ beq(&fallthrough /*, cr0*/);

  // Check that there are enough characters left in the input.
  // TODO(john): the RC bit set below is the condition for
  //              the BranchOrBacktrack function
  // __ Add(r0_p, r4_p, current_input_offset()/*, LeaveOE, SetRC*/);
  __ LoadRR(r0_p, r4_p);
  __ AddP(r0_p, current_input_offset());
//  __ cmn(r1_p, Operand(current_input_offset()));
  BranchOrBacktrack(gt, on_no_match, cr0);

  if (mode_ == ASCII) {
    Label success;
    Label fail;
    Label loop_check;

    // r3_p - offset of start of capture
    // r4_p - length of capture
    __ AddP(r3_p, end_of_input_address());
    __ LoadRR(r5_p, end_of_input_address());
    __ AddP(r5_p, current_input_offset());
    __ AddP(r4_p, r3_p);

    // r3_p - Address of start of capture.
    // r4_p - Address of end of capture
    // r5_p - Address of current input position.

    Label loop;
    __ bind(&loop);
    __ LoadlB(r6_p, MemOperand(r3_p));
    __ AddP(r3_p, Operand(char_size()));
    __ LoadlB(r25_p, MemOperand(r5_p));
    __ AddP(r5_p, Operand(char_size()));
    __ CmpRR(r25_p, r6_p);
    __ beq(&loop_check);

    // Mismatch, try case-insensitive match (converting letters to lower-case).
    __ ori(r6_p, r6_p, Operand(0x20));  // Convert capture character to
                                        // lower-case.
    __ ori(r25_p, r25_p, Operand(0x20));  // Also convert input character.
    __ CmpRR(r25_p, r6_p);
    __ bne(&fail);
    __ Sub(r6_p, Operand('a'));
    __ Cmpli(r6_p, Operand('z' - 'a'));  // Is r6_p a lowercase letter?
    __ bgt(&fail);


    __ bind(&loop_check);
    __ CmpRR(r3_p, r4_p);
    __ blt(&loop);
    __ b(&success);

    __ bind(&fail);
    BranchOrBacktrack(al, on_no_match);

    __ bind(&success);
    // Compute new value of character position after the matched part.
    __ Sub(current_input_offset(), r5_p, end_of_input_address());
  } else {
    ASSERT(mode_ == UC16);
    int argument_count = 4;
    __ PrepareCallCFunction(argument_count, r5_p);

    // r3_p - offset of start of capture
    // r4_p - length of capture

    // Put arguments into arguments registers.
    // Parameters are
    //   r3_p: Address byte_offset1 - Address captured substring's start.
    //   r4_p: Address byte_offset2 - Address of current character position.
    //   r5_p: size_t byte_length - length of capture in bytes(!)
    //   r6_p: Isolate* isolate

    // Address of start of capture.
    __ AddP(r3_p, end_of_input_address());
    // Length of capture.
    __ LoadRR(r5_p, r4_p);
    // Save length in callee-save register for use on return.
    __ LoadRR(r25_p, r4_p);
    // Address of current input position.
    __ LoadRR(r4_p, current_input_offset());
    __ AddP(r4_p, end_of_input_address());
    // Isolate.
    __ mov(r6_p, Operand(ExternalReference::isolate_address()));

    {
      AllowExternalCallThatCantCauseGC scope(masm_);
      ExternalReference function =
          ExternalReference::re_case_insensitive_compare_uc16(masm_->isolate());
      __ CallCFunction(function, argument_count);
    }

    // Check if function returned non-zero for success or zero for failure.
    __ Cmpi(r3_p, Operand::Zero());
    BranchOrBacktrack(eq, on_no_match);
    // On success, increment position by length of capture.
    __ AddP(current_input_offset(), r25_p);
  }

  __ bind(&fallthrough);
}


void RegExpMacroAssemblerPPC::CheckNotBackReference(
    int start_reg,
    Label* on_no_match) {
  Label fallthrough;
  Label success;

  // Find length of back-referenced capture.
  __ LoadP(r3_p, register_location(start_reg), r0_p);
  __ LoadP(r4_p, register_location(start_reg + 1), r0_p);
  // Removing RC looks Okay here.
  __ Sub(r4_p, r4_p, r3_p/*, LeaveOE, SetRC*/);  // Length to check.
  // Succeed on empty capture (including no capture).
  __ beq(&fallthrough /*, cr0*/);

  // Check that there are enough characters left in the input.
  // TODO(john): the RC bit set below is the condition
  //              for the BranchOrBacktrack function
  // __ Add(r0_p, r4_p, current_input_offset()/*, LeaveOE, SetRC*/);
  __ LoadRR(r0_p, r4_p);
  __ AddP(r0_p, current_input_offset());
  BranchOrBacktrack(gt, on_no_match, cr0);

  // Compute pointers to match string and capture string
  __ AddP(r3_p, end_of_input_address());
  __ LoadRR(r5_p, end_of_input_address());
  __ AddP(r5_p, current_input_offset());
  __ AddP(r4_p, r3_p);

  Label loop;
  __ bind(&loop);
  if (mode_ == ASCII) {
    __ LoadlB(r6_p, MemOperand(r3_p));
    __ AddP(r3_p, Operand(char_size()));
    __ LoadlB(r25_p, MemOperand(r5_p));
    __ AddP(r5_p, Operand(char_size()));
  } else {
    ASSERT(mode_ == UC16);
    __ lhz(r6_p, MemOperand(r3_p));
    __ AddP(r3_p, Operand(char_size()));
    __ lhz(r25_p, MemOperand(r5_p));
    __ AddP(r5_p, Operand(char_size()));
  }
  __ CmpRR(r6_p, r25_p);
  BranchOrBacktrack(ne, on_no_match);
  __ CmpRR(r3_p, r4_p);
  __ blt(&loop);

  // Move current character position to position after match.
  __ Sub(current_input_offset(), r5_p, end_of_input_address());
  __ bind(&fallthrough);
}


void RegExpMacroAssemblerPPC::CheckNotCharacter(unsigned c,
                                                Label* on_not_equal) {
  __ Cmpli(current_character(), Operand(c));
  BranchOrBacktrack(ne, on_not_equal);
}


void RegExpMacroAssemblerPPC::CheckCharacterAfterAnd(uint32_t c,
                                                     uint32_t mask,
                                                     Label* on_equal) {
  __ mov(r0_p, Operand(mask));
  if (c == 0) {
    __ LoadRR(r3_p, r0_p);
    __ AndP(r3_p, current_character()/*, SetRC*/);
    // Should be okay to remove rc
  } else {
    __ LoadRR(r3_p, r0_p);
    __ AndP(r3_p, current_character());
    // TODO(JOHN): dont know if removing cr0 causes a problem
    __ Cmpli(r3_p, Operand(c)/*, cr0*/);
  }
  BranchOrBacktrack(eq, on_equal, cr0);
}


void RegExpMacroAssemblerPPC::CheckNotCharacterAfterAnd(unsigned c,
                                                        unsigned mask,
                                                        Label* on_not_equal) {
  __ mov(r0_p, Operand(mask));
  if (c == 0) {
    __ LoadRR(r3_p, r0_p);
    __ AndP(r3_p, current_character()/*, SetRC*/);
    // Should be okay to remove rc
  } else {
    __ LoadRR(r3_p, r0_p);
    __ AndP(r3_p, current_character());
    // TODO(JOHN): dont know if removing cr0 causes a problem
    __ Cmpli(r3_p, Operand(c)/*, cr0*/);
  }
  BranchOrBacktrack(ne, on_not_equal, cr0);
}


void RegExpMacroAssemblerPPC::CheckNotCharacterAfterMinusAnd(
    uc16 c,
    uc16 minus,
    uc16 mask,
    Label* on_not_equal) {
  ASSERT(minus < String::kMaxUtf16CodeUnit);
  __ Sub(r3_p, current_character(), Operand(minus));
  __ mov(r0_p, Operand(mask));
  __ AndP(r3_p, r0_p);
  __ Cmpli(r3_p, Operand(c));
  BranchOrBacktrack(ne, on_not_equal);
}


void RegExpMacroAssemblerPPC::CheckCharacterInRange(
    uc16 from,
    uc16 to,
    Label* on_in_range) {
  __ mov(r0_p, Operand(from));
  __ Sub(r3_p, current_character(), r0_p);
  __ Cmpli(r3_p, Operand(to - from));
  BranchOrBacktrack(le, on_in_range);  // Unsigned lower-or-same condition.
}


void RegExpMacroAssemblerPPC::CheckCharacterNotInRange(
    uc16 from,
    uc16 to,
    Label* on_not_in_range) {
  __ mov(r0_p, Operand(from));
  __ Sub(r3_p, current_character(), r0_p);
  __ Cmpli(r3_p, Operand(to - from));
  BranchOrBacktrack(gt, on_not_in_range);  // Unsigned higher condition.
}


void RegExpMacroAssemblerPPC::CheckBitInTable(
    Handle<ByteArray> table,
    Label* on_bit_set) {
  __ mov(r3_p, Operand(table));
  if (mode_ != ASCII || kTableMask != String::kMaxAsciiCharCode) {
    __ LoadRR(r4_p, current_character());
    __ AndP(r4_p, Operand(kTableSize - 1));
    __ AddP(r4_p, Operand(ByteArray::kHeaderSize - kHeapObjectTag));
  } else {
    __ LoadRR(r4_p, current_character());
    __ AddP(r4_p, Operand(ByteArray::kHeaderSize - kHeapObjectTag));
  }
  __ LoadlB(r3_p, MemOperand(r3_p, r4_p));
  __ Cmpi(r3_p, Operand::Zero());
  BranchOrBacktrack(ne, on_bit_set);
}


bool RegExpMacroAssemblerPPC::CheckSpecialCharacterClass(uc16 type,
                                                         Label* on_no_match) {
  // Range checks (c in min..max) are generally implemented by an unsigned
  // (c - min) <= (max - min) check
  switch (type) {
  case 's':
    // Match space-characters
    if (mode_ == ASCII) {
      // ASCII space characters are '\t'..'\r' and ' '.
      Label success;
      __ Cmpi(current_character(), Operand(' '));
      __ beq(&success);
      // Check range 0x09..0x0d
      __ Sub(r3_p, current_character(), Operand('\t'));
      __ Cmpli(r3_p, Operand('\r' - '\t'));
      BranchOrBacktrack(gt, on_no_match);
      __ bind(&success);
      return true;
    }
    return false;
  case 'S':
    // Match non-space characters.
    if (mode_ == ASCII) {
      // ASCII space characters are '\t'..'\r' and ' '.
      __ Cmpi(current_character(), Operand(' '));
      BranchOrBacktrack(eq, on_no_match);
      __ Sub(r3_p, current_character(), Operand('\t'));
      __ Cmpli(r3_p, Operand('\r' - '\t'));
      BranchOrBacktrack(le, on_no_match);
      return true;
    }
    return false;
  case 'd':
    // Match ASCII digits ('0'..'9')
    __ Sub(r3_p, current_character(), Operand('0'));
    __ Cmpli(current_character(), Operand('9' - '0'));
    BranchOrBacktrack(gt, on_no_match);
    return true;
  case 'D':
    // Match non ASCII-digits
    __ Sub(r3_p, current_character(), Operand('0'));
    __ Cmpli(r3_p, Operand('9' - '0'));
    BranchOrBacktrack(le, on_no_match);
    return true;
  case '.': {
    // Match non-newlines (not 0x0a('\n'), 0x0d('\r'), 0x2028 and 0x2029)
    __ xori(r3_p, current_character(), Operand(0x01));
    // See if current character is '\n'^1 or '\r'^1, i.e., 0x0b or 0x0c
    __ Sub(r3_p, Operand(0x0b));
    __ Cmpli(r3_p, Operand(0x0c - 0x0b));
    BranchOrBacktrack(le, on_no_match);
    if (mode_ == UC16) {
      // Compare original value to 0x2028 and 0x2029, using the already
      // computed (current_char ^ 0x01 - 0x0b). I.e., check for
      // 0x201d (0x2028 - 0x0b) or 0x201e.
      __ Sub(r3_p, Operand(0x2028 - 0x0b));
      __ Cmpli(r3_p, Operand(1));
      BranchOrBacktrack(le, on_no_match);
    }
    return true;
  }
  case 'n': {
    // Match newlines (0x0a('\n'), 0x0d('\r'), 0x2028 and 0x2029)
    __ xori(r3_p, current_character(), Operand(0x01));
    // See if current character is '\n'^1 or '\r'^1, i.e., 0x0b or 0x0c
    __ Sub(r3_p, Operand(0x0b));
    __ Cmpli(r3_p, Operand(0x0c - 0x0b));
    if (mode_ == ASCII) {
      BranchOrBacktrack(gt, on_no_match);
    } else {
      Label done;
      __ ble(&done);
      // Compare original value to 0x2028 and 0x2029, using the already
      // computed (current_char ^ 0x01 - 0x0b). I.e., check for
      // 0x201d (0x2028 - 0x0b) or 0x201e.
      __ Sub(r3_p, Operand(0x2028 - 0x0b));
      __ Cmpli(r3_p, Operand(1));
      BranchOrBacktrack(gt, on_no_match);
      __ bind(&done);
    }
    return true;
  }
  case 'w': {
    if (mode_ != ASCII) {
      // Table is 128 entries, so all ASCII characters can be tested.
      __ Cmpi(current_character(), Operand('z'));
      BranchOrBacktrack(gt, on_no_match);
    }
    ExternalReference map = ExternalReference::re_word_character_map();
    __ mov(r3_p, Operand(map));
    __ LoadlB(r3_p, MemOperand(r3_p, current_character()));
    __ Cmpli(r3_p, Operand::Zero());
    BranchOrBacktrack(eq, on_no_match);
    return true;
  }
  case 'W': {
    Label done;
    if (mode_ != ASCII) {
      // Table is 128 entries, so all ASCII characters can be tested.
      __ Cmpli(current_character(), Operand('z'));
      __ bgt(&done);
    }
    ExternalReference map = ExternalReference::re_word_character_map();
    __ mov(r3_p, Operand(map));
    __ LoadlB(r3_p, MemOperand(r3_p, current_character()));
    __ Cmpli(r3_p, Operand::Zero());
    BranchOrBacktrack(ne, on_no_match);
    if (mode_ != ASCII) {
      __ bind(&done);
    }
    return true;
  }
  case '*':
    // Match any character.
    return true;
  // No custom implementation (yet): s(UC16), S(UC16).
  default:
    return false;
  }
}


void RegExpMacroAssemblerPPC::Fail() {
  __ LoadImmP(r3_p, Operand(FAILURE));
  __ b(&exit_label_);
}


Handle<HeapObject> RegExpMacroAssemblerPPC::GetCode(Handle<String> source) {
  Label return_r3;

  if (masm_->has_exception()) {
    // If the code gets corrupted due to long regular expressions and lack of
    // space on trampolines, an internal exception flag is set. If this case
    // is detected, we will jump into exit sequence right away.
    __ bind_to(&entry_label_, internal_failure_label_.pos());
  } else {
    // Finalize code - write the entry point code now we know how many
    // registers we need.

    // Entry code:
    __ bind(&entry_label_);

    // Tell the system that we have a stack frame.  Because the type
    // is MANUAL, no is generated.
    FrameScope scope(masm_, StackFrame::MANUAL);

    // Ensure register assigments are consistent with callee save mask
    ASSERT(r25_p.bit() & kRegExpCalleeSaved);
    ASSERT(code_pointer().bit() & kRegExpCalleeSaved);
    ASSERT(current_input_offset().bit() & kRegExpCalleeSaved);
    ASSERT(current_character().bit() & kRegExpCalleeSaved);
    ASSERT(backtrack_stackpointer().bit() & kRegExpCalleeSaved);
    ASSERT(end_of_input_address().bit() & kRegExpCalleeSaved);
    ASSERT(frame_pointer().bit() & kRegExpCalleeSaved);

    // Actually emit code to start a new stack frame.
    // Push arguments
    // Save callee-save registers.
    // Start new stack frame.
    // Store link register in existing stack-cell.
    // Order here should correspond to order of offset constants in header file.
    RegList registers_to_retain = kRegExpCalleeSaved;
    RegList argument_registers = r3_p.bit() | r4_p.bit() | r5_p.bit() |
      r6_p.bit() | r7_p.bit() | r8_p.bit() | r9_p.bit() | r10_p.bit();
    __ push(r14);
    __ MultiPush(argument_registers | registers_to_retain);
    // Set frame pointer in space for it if this is not a direct call
    // from generated code.
    __ LoadRR(frame_pointer(), sp);
    __ AddP(frame_pointer(), Operand(8 * kPointerSize));
    __ LoadImmP(r3_p, Operand::Zero());
    __ push(r3_p);  // Make room for success counter and initialize it to 0.
    __ push(r3_p);  // Make room for "position-1" constant (value is irrelevant)
    // Check if we have space on the stack for registers.
    Label stack_limit_hit;
    Label stack_ok;

    ExternalReference stack_limit =
      ExternalReference::address_of_stack_limit(masm_->isolate());
    __ mov(r3_p, Operand(stack_limit));
    __ LoadP(r3_p, MemOperand(r3_p));
    __ Sub(r3_p, sp, r3_p/*, LeaveOE, SetRC*/);  // Removing RC looks okay here
    // Handle it if the stack pointer is already below the stack limit.
    __ ble(&stack_limit_hit /*, cr0*/);
    // Check if there is room for the variable number of registers above
    // the stack limit.
    __ Cmpli(r3_p, Operand(num_registers_ * kPointerSize));
    __ bge(&stack_ok);
    // Exit with OutOfMemory exception. There is not enough space on the stack
    // for our working registers.
    __ LoadImmP(r3_p, Operand(EXCEPTION));
    __ b(&return_r3);

    __ bind(&stack_limit_hit);
    CallCheckStackGuardState(r3_p);
    __ Cmpi(r3_p, Operand::Zero());
    // If returned value is non-zero, we exit with the returned value as result.
    __ bne(&return_r3);

    __ bind(&stack_ok);

    // Allocate space on stack for registers.
    __ AddP(sp, Operand(-num_registers_ * kPointerSize));
    // Load string end.
    __ LoadP(end_of_input_address(), MemOperand(frame_pointer(), kInputEnd));
    // Load input start.
    __ LoadP(r3_p, MemOperand(frame_pointer(), kInputStart));
    // Find negative length (offset of start relative to end).
    __ Sub(current_input_offset(), r3_p, end_of_input_address());
    // Set r3_p to address of char before start of the input string
    // (effectively string position -1).
    __ LoadP(r4_p, MemOperand(frame_pointer(), kStartIndex));
    __ Sub(r3_p, current_input_offset(), Operand(char_size()));
    if (mode_ == UC16) {
      __ ShiftLeftImm(r0_p, r4_p, Operand(1));
      __ Sub(r3_p, r3_p, r0_p);
    } else {
      __ Sub(r3_p, r3_p, r4_p);
    }
    // Store this value in a local variable, for use when clearing
    // position registers.
    __ StoreP(r3_p, MemOperand(frame_pointer(), kInputStartMinusOne));

    // Initialize code pointer register
    __ mov(code_pointer(), Operand(masm_->CodeObject()));

    Label load_char_start_regexp, start_regexp;
    // Load newline if index is at start, previous character otherwise.
    __ Cmpi(r4_p, Operand::Zero());
    __ bne(&load_char_start_regexp);
    __ LoadImmP(current_character(), Operand('\n'));
    __ b(&start_regexp);

    // Global regexp restarts matching here.
    __ bind(&load_char_start_regexp);
    // Load previous char as initial value of current character register.
    LoadCurrentCharacterUnchecked(-1, 1);
    __ bind(&start_regexp);

    // Initialize on-stack registers.
    if (num_saved_registers_ > 0) {  // Always is, if generated from a regexp.
      // Fill saved registers with initial value = start offset - 1
      if (num_saved_registers_ > 8) {
        // One slot beyond address of register 0.
        __ LoadRR(r4_p, frame_pointer());
        __ AddP(r4_p, Operand(kRegisterZero + kPointerSize));
        __ LoadImmP(r5_p, Operand(num_saved_registers_));
        __ mtctr(r5_p);
        Label init_loop;
        __ bind(&init_loop);
        __ StorePU(r3_p, MemOperand(r4_p, -kPointerSize));
        __ bdnz(&init_loop);
      } else {
        for (int i = 0; i < num_saved_registers_; i++) {
          __ StoreP(r3_p, register_location(i));
        }
      }
    }

    // Initialize backtrack stack pointer.
    __ LoadP(backtrack_stackpointer(),
             MemOperand(frame_pointer(), kStackHighEnd));

    __ b(&start_label_);

    // Exit code:
    if (success_label_.is_linked()) {
      // Save captures when successful.
      __ bind(&success_label_);
      if (num_saved_registers_ > 0) {
        // copy captures to output
        __ LoadP(r4_p, MemOperand(frame_pointer(), kInputStart));
        __ LoadP(r3_p, MemOperand(frame_pointer(), kRegisterOutput));
        __ LoadP(r5_p, MemOperand(frame_pointer(), kStartIndex));
        __ Sub(r4_p, end_of_input_address(), r4_p);
        // r4_p is length of input in bytes.
        if (mode_ == UC16) {
          __ ShiftRightImm(r4_p, r4_p, Operand(1));
        }
        // r4_p is length of input in characters.
        __ AddP(r4_p, r5_p);
        // r4_p is length of string in characters.

        ASSERT_EQ(0, num_saved_registers_ % 2);
        // Always an even number of capture registers. This allows us to
        // unroll the loop once to add an operation between a load of a register
        // and the following use of that register.
        for (int i = 0; i < num_saved_registers_; i += 2) {
          __ LoadP(r5_p, register_location(i), r0_p);
          __ LoadP(r6_p, register_location(i + 1), r0_p);
          if (i == 0 && global_with_zero_length_check()) {
            // Keep capture start in r25_p for the zero-length check later.
            __ LoadRR(r25_p, r5_p);
          }
          if (mode_ == UC16) {
            __ ShiftRightArithImm(r5_p, r5_p, 1);
            __ Add(r5_p, r4_p, r5_p);
            __ ShiftRightArithImm(r6_p, r6_p, 1);
            __ Add(r6_p, r4_p, r6_p);
          } else {
            __ Add(r5_p, r4_p, r5_p);
            __ Add(r6_p, r4_p, r6_p);
          }
          __ StoreW(r5_p, MemOperand(r3_p));
          __ AddP(r3_p, Operand(kIntSize));
          __ StoreW(r6_p, MemOperand(r3_p));
          __ AddP(r3_p, Operand(kIntSize));
        }
      }

      if (global()) {
        // Restart matching if the regular expression is flagged as global.
        __ LoadP(r3_p, MemOperand(frame_pointer(), kSuccessfulCaptures));
        __ LoadP(r4_p, MemOperand(frame_pointer(), kNumOutputRegisters));
        __ LoadP(r5_p, MemOperand(frame_pointer(), kRegisterOutput));
        // Increment success counter.
        __ AddP(r3_p, Operand(1));
        __ StoreP(r3_p, MemOperand(frame_pointer(), kSuccessfulCaptures));
        // Capture results have been stored, so the number of remaining global
        // output registers is reduced by the number of stored captures.
        __ Sub(r4_p, Operand(num_saved_registers_));
        // Check whether we have enough room for another set of capture results.
        __ Cmpi(r4_p, Operand(num_saved_registers_));
        __ blt(&return_r3);

        __ StoreP(r4_p, MemOperand(frame_pointer(), kNumOutputRegisters));
        // Advance the location for output.
        __ AddP(r5_p, Operand(num_saved_registers_ * kIntSize));
        __ StoreP(r5_p, MemOperand(frame_pointer(), kRegisterOutput));

        // Prepare r3_p to initialize registers with its value in the next run.
        __ LoadP(r3_p, MemOperand(frame_pointer(), kInputStartMinusOne));

        if (global_with_zero_length_check()) {
          // Special case for zero-length matches.
          // r25_p: capture start index
          __ CmpRR(current_input_offset(), r25_p);
          // Not a zero-length match, restart.
          __ bne(&load_char_start_regexp);
          // Offset from the end is zero if we already reached the end.
          __ Cmpi(current_input_offset(), Operand::Zero());
          __ beq(&exit_label_);
          // Advance current position after a zero-length match.
          __ AddP(current_input_offset(), Operand((mode_ == UC16) ? 2 : 1));
        }

        __ b(&load_char_start_regexp);
      } else {
        __ LoadImmP(r3_p, Operand(SUCCESS));
      }
    }

    // Exit and return r3_p
    __ bind(&exit_label_);
    if (global()) {
      __ LoadP(r3_p, MemOperand(frame_pointer(), kSuccessfulCaptures));
    }

    __ bind(&return_r3);
    // Skip sp past regexp registers and local variables..
    __ LoadRR(sp, frame_pointer());
    // Restore registers r25_p..r31_p and return (restoring lr to pc).
    __ MultiPop(registers_to_retain);
    __ pop(r0_p);
    __ mtctr(r0_p);
    __ bcr();

    // Backtrack code (branch target for conditional backtracks).
    if (backtrack_label_.is_linked()) {
      __ bind(&backtrack_label_);
      Backtrack();
    }

    Label exit_with_exception;

    // Preempt-code
    if (check_preempt_label_.is_linked()) {
      SafeCallTarget(&check_preempt_label_);

      CallCheckStackGuardState(r3_p);
      __ Cmpi(r3_p, Operand::Zero());
      // If returning non-zero, we should end execution with the given
      // result as return value.
      __ bne(&return_r3);

      // String might have moved: Reload end of string from frame.
      __ LoadP(end_of_input_address(), MemOperand(frame_pointer(), kInputEnd));
      SafeReturn();
    }

    // Backtrack stack overflow code.
    if (stack_overflow_label_.is_linked()) {
      SafeCallTarget(&stack_overflow_label_);
      // Reached if the backtrack-stack limit has been hit.
      Label grow_failed;

      // Call GrowStack(backtrack_stackpointer(), &stack_base)
      static const int num_arguments = 3;
      __ PrepareCallCFunction(num_arguments, r3_p);
      __ LoadRR(r3_p, backtrack_stackpointer());
      __ LoadRR(r4_p, frame_pointer());
      __ AddP(r4_p, Operand(kStackHighEnd));
      __ mov(r5_p, Operand(ExternalReference::isolate_address()));
      ExternalReference grow_stack =
        ExternalReference::re_grow_stack(masm_->isolate());
      __ CallCFunction(grow_stack, num_arguments);
      // If return NULL, we have failed to grow the stack, and
      // must exit with a stack-overflow exception.
      __ Cmpi(r3_p, Operand::Zero());
      __ beq(&exit_with_exception);
      // Otherwise use return value as new stack pointer.
      __ LoadRR(backtrack_stackpointer(), r3_p);
      // Restore saved registers and continue.
      SafeReturn();
    }

    if (exit_with_exception.is_linked()) {
      // If any of the code above needed to exit with an exception.
      __ bind(&exit_with_exception);
      // Exit with Result EXCEPTION(-1) to signal thrown exception.
      __ LoadImmP(r3_p, Operand(EXCEPTION));
      __ b(&return_r3);
    }
  }

  CodeDesc code_desc;
  masm_->GetCode(&code_desc);
  Handle<Code> code = FACTORY->NewCode(code_desc,
                                       Code::ComputeFlags(Code::REGEXP),
                                       masm_->CodeObject());
  PROFILE(Isolate::Current(), RegExpCodeCreateEvent(*code, *source));
  return Handle<HeapObject>::cast(code);
}


void RegExpMacroAssemblerPPC::GoTo(Label* to) {
  BranchOrBacktrack(al, to);
}


void RegExpMacroAssemblerPPC::IfRegisterGE(int reg,
                                           int comparand,
                                           Label* if_ge) {
  __ LoadP(r3_p, register_location(reg), r0_p);
  __ Cmpi(r3_p, Operand(comparand));
  BranchOrBacktrack(ge, if_ge);
}


void RegExpMacroAssemblerPPC::IfRegisterLT(int reg,
                                           int comparand,
                                           Label* if_lt) {
  __ LoadP(r3_p, register_location(reg), r0_p);
  __ Cmpi(r3_p, Operand(comparand));
  BranchOrBacktrack(lt, if_lt);
}


void RegExpMacroAssemblerPPC::IfRegisterEqPos(int reg,
                                              Label* if_eq) {
  __ LoadP(r3_p, register_location(reg), r0_p);
  __ CmpRR(r3_p, current_input_offset());
  BranchOrBacktrack(eq, if_eq);
}


RegExpMacroAssembler::IrregexpImplementation
    RegExpMacroAssemblerPPC::Implementation() {
  return kPPCImplementation;
}


void RegExpMacroAssemblerPPC::LoadCurrentCharacter(int cp_offset,
                                                   Label* on_end_of_input,
                                                   bool check_bounds,
                                                   int characters) {
  ASSERT(cp_offset >= -1);      // ^ and \b can look behind one character.
  ASSERT(cp_offset < (1<<30));  // Be sane! (And ensure negation works)
  if (check_bounds) {
    CheckPosition(cp_offset + characters - 1, on_end_of_input);
  }
  LoadCurrentCharacterUnchecked(cp_offset, characters);
}


void RegExpMacroAssemblerPPC::PopCurrentPosition() {
  Pop(current_input_offset());
}


void RegExpMacroAssemblerPPC::PopRegister(int register_index) {
  Pop(r3_p);
  __ StoreP(r3_p, register_location(register_index));
}


void RegExpMacroAssemblerPPC::PushBacktrack(Label* label) {
  if (label->is_bound()) {
    int target = label->pos();
    __ mov(r3_p, Operand(target + Code::kHeaderSize - kHeapObjectTag));
  } else {
    Label after_constant;
    __ b(&after_constant);
    int offset = masm_->pc_offset();
    int cp_offset = offset + Code::kHeaderSize - kHeapObjectTag;
    __ emit(0);
    masm_->label_at_put(label, offset);
    __ bind(&after_constant);
    __ LoadlW(r3_p, MemOperand(code_pointer(), cp_offset));
  }
  Push(r3_p);
  CheckStackLimit();
}


void RegExpMacroAssemblerPPC::PushCurrentPosition() {
  Push(current_input_offset());
}


void RegExpMacroAssemblerPPC::PushRegister(int register_index,
                                           StackCheckFlag check_stack_limit) {
  __ LoadP(r3_p, register_location(register_index), r0_p);
  Push(r3_p);
  if (check_stack_limit) CheckStackLimit();
}


void RegExpMacroAssemblerPPC::ReadCurrentPositionFromRegister(int reg) {
  __ LoadP(current_input_offset(), register_location(reg), r0_p);
}


void RegExpMacroAssemblerPPC::ReadStackPointerFromRegister(int reg) {
  __ LoadP(backtrack_stackpointer(), register_location(reg), r0_p);
  __ LoadP(r3_p, MemOperand(frame_pointer(), kStackHighEnd));
  __ AddP(backtrack_stackpointer(), r3_p);
}


void RegExpMacroAssemblerPPC::SetCurrentPositionFromEnd(int by) {
  Label after_position;
  __ Cmpi(current_input_offset(), Operand(-by * char_size()));
  __ bge(&after_position);
  __ mov(current_input_offset(), Operand(-by * char_size()));
  // On RegExp code entry (where this operation is used), the character before
  // the current position is expected to be already loaded.
  // We have advanced the position, so it's safe to read backwards.
  LoadCurrentCharacterUnchecked(-1, 1);
  __ bind(&after_position);
}


void RegExpMacroAssemblerPPC::SetRegister(int register_index, int to) {
  ASSERT(register_index >= num_saved_registers_);  // Reserved for positions!
  __ mov(r3_p, Operand(to));
  __ StoreP(r3_p, register_location(register_index));
}


bool RegExpMacroAssemblerPPC::Succeed() {
  __ b(&success_label_);
  return global();
}


void RegExpMacroAssemblerPPC::WriteCurrentPositionToRegister(int reg,
                                                             int cp_offset) {
  if (cp_offset == 0) {
    __ StoreP(current_input_offset(), register_location(reg));
  } else {
    __ mov(r0_p, Operand(cp_offset * char_size()));
    __ LoadRR(r3_p, current_input_offset());
    __ AddP(r3_p, r0_p);
    __ StoreP(r3_p, register_location(reg));
  }
}


void RegExpMacroAssemblerPPC::ClearRegisters(int reg_from, int reg_to) {
  ASSERT(reg_from <= reg_to);
  __ LoadP(r3_p, MemOperand(frame_pointer(), kInputStartMinusOne));
  for (int reg = reg_from; reg <= reg_to; reg++) {
    __ StoreP(r3_p, register_location(reg));
  }
}


void RegExpMacroAssemblerPPC::WriteStackPointerToRegister(int reg) {
  __ LoadP(r4_p, MemOperand(frame_pointer(), kStackHighEnd));
  __ Sub(r3_p, backtrack_stackpointer(), r4_p);
  __ StoreP(r3_p, register_location(reg));
}


// Private methods:

void RegExpMacroAssemblerPPC::CallCheckStackGuardState(Register scratch) {
  static const int num_arguments = 3;
  __ PrepareCallCFunction(num_arguments, scratch);
  // RegExp code frame pointer.
  __ LoadRR(r5_p, frame_pointer());
  // Code* of self.
  __ mov(r4_p, Operand(masm_->CodeObject()));
  // r3_p becomes return address pointer.
  ExternalReference stack_guard_check =
      ExternalReference::re_check_stack_guard_state(masm_->isolate());
  CallCFunctionUsingStub(stack_guard_check, num_arguments);
}


// Helper function for reading a value out of a stack frame.
template <typename T>
static T& frame_entry(Address re_frame, int frame_offset) {
  return reinterpret_cast<T&>(Memory::int32_at(re_frame + frame_offset));
}


int RegExpMacroAssemblerPPC::CheckStackGuardState(Address* return_address,
                                                  Code* re_code,
                                                  Address re_frame) {
  Isolate* isolate = frame_entry<Isolate*>(re_frame, kIsolate);
  ASSERT(isolate == Isolate::Current());
  if (isolate->stack_guard()->IsStackOverflow()) {
    isolate->StackOverflow();
    return EXCEPTION;
  }

  // If not real stack overflow the stack guard was used to interrupt
  // execution for another purpose.

  // If this is a direct call from JavaScript retry the RegExp forcing the call
  // through the runtime system. Currently the direct call cannot handle a GC.
  if (frame_entry<int>(re_frame, kDirectCall) == 1) {
    return RETRY;
  }

  // Prepare for possible GC.
  HandleScope handles(isolate);
  Handle<Code> code_handle(re_code);

  Handle<String> subject(frame_entry<String*>(re_frame, kInputString));

  // Current string.
  bool is_ascii = subject->IsAsciiRepresentationUnderneath();

  ASSERT(re_code->instruction_start() <= *return_address);
  ASSERT(*return_address <=
      re_code->instruction_start() + re_code->instruction_size());

  MaybeObject* result = Execution::HandleStackGuardInterrupt(isolate);

  if (*code_handle != re_code) {  // Return address no longer valid
    int delta = code_handle->address() - re_code->address();
    // Overwrite the return address on the stack.
    *return_address += delta;
  }

  if (result->IsException()) {
    return EXCEPTION;
  }

  Handle<String> subject_tmp = subject;
  int slice_offset = 0;

  // Extract the underlying string and the slice offset.
  if (StringShape(*subject_tmp).IsCons()) {
    subject_tmp = Handle<String>(ConsString::cast(*subject_tmp)->first());
  } else if (StringShape(*subject_tmp).IsSliced()) {
    SlicedString* slice = SlicedString::cast(*subject_tmp);
    subject_tmp = Handle<String>(slice->parent());
    slice_offset = slice->offset();
  }

  // String might have changed.
  if (subject_tmp->IsAsciiRepresentation() != is_ascii) {
    // If we changed between an ASCII and an UC16 string, the specialized
    // code cannot be used, and we need to restart regexp matching from
    // scratch (including, potentially, compiling a new version of the code).
    return RETRY;
  }

  // Otherwise, the content of the string might have moved. It must still
  // be a sequential or external string with the same content.
  // Update the start and end pointers in the stack frame to the current
  // location (whether it has actually moved or not).
  ASSERT(StringShape(*subject_tmp).IsSequential() ||
      StringShape(*subject_tmp).IsExternal());

  // The original start address of the characters to match.
  const byte* start_address = frame_entry<const byte*>(re_frame, kInputStart);

  // Find the current start address of the same character at the current string
  // position.
  int start_index = frame_entry<intptr_t>(re_frame, kStartIndex);
  const byte* new_address = StringCharacterPosition(*subject_tmp,
                                                    start_index + slice_offset);

  if (start_address != new_address) {
    // If there is a difference, update the object pointer and start and end
    // addresses in the RegExp stack frame to match the new value.
    const byte* end_address = frame_entry<const byte* >(re_frame, kInputEnd);
    int byte_length = static_cast<int>(end_address - start_address);
    frame_entry<const String*>(re_frame, kInputString) = *subject;
    frame_entry<const byte*>(re_frame, kInputStart) = new_address;
    frame_entry<const byte*>(re_frame, kInputEnd) = new_address + byte_length;
  } else if (frame_entry<const String*>(re_frame, kInputString) != *subject) {
    // Subject string might have been a ConsString that underwent
    // short-circuiting during GC. That will not change start_address but
    // will change pointer inside the subject handle.
    frame_entry<const String*>(re_frame, kInputString) = *subject;
  }

  return 0;
}


MemOperand RegExpMacroAssemblerPPC::register_location(int register_index) {
  ASSERT(register_index < (1<<30));
  if (num_registers_ <= register_index) {
    num_registers_ = register_index + 1;
  }
  return MemOperand(frame_pointer(),
                    kRegisterZero - register_index * kPointerSize);
}


void RegExpMacroAssemblerPPC::CheckPosition(int cp_offset,
                                            Label* on_outside_input) {
  __ Cmpi(current_input_offset(), Operand(-cp_offset * char_size()));
  BranchOrBacktrack(ge, on_outside_input);
}


void RegExpMacroAssemblerPPC::BranchOrBacktrack(Condition condition,
                                                Label* to,
                                                CRegister cr) {
  if (condition == al) {  // Unconditional.
    if (to == NULL) {
      Backtrack();
      return;
    }
    __ b(to);
    return;
  }
  if (to == NULL) {
    __ b(condition, &backtrack_label_ /*, cr*/);
    return;
  }
  __ b(condition, to /*, cr*/);
}


void RegExpMacroAssemblerPPC::SafeCall(Label* to, Condition cond,
                                       CRegister cr) {
  __ b(cond, to /*, cr*/ /*, SetLK*/);
}


void RegExpMacroAssemblerPPC::SafeReturn() {
  __ pop(r14);
  __ mov(ip, Operand(masm_->CodeObject()));
  __ AddP(r14, ip);
  __ blr();
}


void RegExpMacroAssemblerPPC::SafeCallTarget(Label* name) {
  __ bind(name);
  __ LoadRR(r0_p, r14);
  __ mov(ip, Operand(masm_->CodeObject()));
  __ Sub(r0_p, r0_p, ip);
  __ push(r0_p);
}


void RegExpMacroAssemblerPPC::Push(Register source) {
  ASSERT(!source.is(backtrack_stackpointer()));
  __ StorePU(source, MemOperand(backtrack_stackpointer(), -kPointerSize));
}


void RegExpMacroAssemblerPPC::Pop(Register target) {
  ASSERT(!target.is(backtrack_stackpointer()));
  __ LoadP(target, MemOperand(backtrack_stackpointer()));
  __ AddP(backtrack_stackpointer(), Operand(kPointerSize));
}


void RegExpMacroAssemblerPPC::CheckPreemption() {
  // Check for preemption.
  ExternalReference stack_limit =
      ExternalReference::address_of_stack_limit(masm_->isolate());
  __ mov(r3_p, Operand(stack_limit));
  __ LoadP(r3_p, MemOperand(r3_p));
  __ Cmpl(sp, r3_p);
  SafeCall(&check_preempt_label_, le);
}


void RegExpMacroAssemblerPPC::CheckStackLimit() {
  ExternalReference stack_limit =
      ExternalReference::address_of_regexp_stack_limit(masm_->isolate());
  __ mov(r3_p, Operand(stack_limit));
  __ LoadP(r3_p, MemOperand(r3_p));
  __ Cmpl(backtrack_stackpointer(), r3_p);
  SafeCall(&stack_overflow_label_, le);
}


void RegExpMacroAssemblerPPC::CallCFunctionUsingStub(
    ExternalReference function,
    int num_arguments) {
  // Must pass all arguments in registers. The stub pushes on the stack.
  ASSERT(num_arguments <= 8);
  __ mov(code_pointer(), Operand(function));
  RegExpCEntryStub stub;
  __ CallStub(&stub);
  if (OS::ActivationFrameAlignment() > kPointerSize) {
    __ LoadP(sp, MemOperand(sp, 0));
  } else {
    __ AddP(sp, Operand(kNumRequiredStackFrameSlots * kPointerSize));
  }
  __ mov(code_pointer(), Operand(masm_->CodeObject()));
}


bool RegExpMacroAssemblerPPC::CanReadUnaligned() {
  return CpuFeatures::IsSupported(UNALIGNED_ACCESSES) && !slow_safe();
}


void RegExpMacroAssemblerPPC::LoadCurrentCharacterUnchecked(int cp_offset,
                                                            int characters) {
  Register offset = current_input_offset();
  if (cp_offset != 0) {
    // r25_p is not being used to store the capture start index at this point.
    __ LoadRR(r25_p, current_input_offset());
    __ AddP(r25_p, Operand(cp_offset * char_size()));
    offset = r25_p;
  }
  // The lwz, stw, lhz, sth instructions can do unaligned accesses, if the CPU
  // and the operating system running on the target allow it.
  // We assume we don't want to do unaligned loads on PPC, so this function
  // must only be used to load a single character at a time.

  ASSERT(characters == 1);
  __ LoadRR(current_character(), end_of_input_address());
  __ AddP(current_character(), offset);
  if (mode_ == ASCII) {
    __ LoadlB(current_character(), MemOperand(current_character()));
  } else {
    ASSERT(mode_ == UC16);
    __ lhz(current_character(), MemOperand(current_character()));
  }
}


void RegExpCEntryStub::Generate(MacroAssembler* masm_) {
  int stack_alignment = OS::ActivationFrameAlignment();
  if (stack_alignment < kPointerSize) stack_alignment = kPointerSize;

  // Stack is already aligned for call, so decrement by alignment
  // to make room for storing the return address.
  int extra_stack_slots = stack_alignment >> kPointerSizeLog2;

  __ LoadRR(r3_p, sp);
  __ AddP(r3_p, Operand(-stack_alignment));
  __ StoreP(r14, MemOperand(r3_p, 0));

  // PPC LINUX ABI:
  extra_stack_slots += kNumRequiredStackFrameSlots;
  __ AddP(sp, Operand(-extra_stack_slots * kPointerSize));

#if ABI_USES_FUNCTION_DESCRIPTORS && !defined(USE_SIMULATOR)
  // Native AIX/PPC64 Linux use a function descriptor.
  __ LoadP(ToRegister(2), MemOperand(r26_p, kPointerSize));  // TOC
  __ LoadP(ip, MemOperand(r26_p, 0));  // Instruction address
  Register target = ip;
#else
  Register target = r26_p;
#endif

  __ Call(target);

  __ AddP(sp, Operand(extra_stack_slots * kPointerSize));

  __ LoadP(r14, MemOperand(sp, -stack_alignment));
  __ blr();
}

#undef __

#endif  // V8_INTERPRETED_REGEXP

}}  // namespace v8::internal

#endif  // V8_TARGET_ARCH_S390
