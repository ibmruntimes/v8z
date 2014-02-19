// Copyright (c) 1994-2006 Sun Microsystems Inc.
// All Rights Reserved.
//
// Redistribution and use in source and binary forms, with or without
// modification, are permitted provided that the following conditions
// are met:
//
// - Redistributions of source code must retain the above copyright notice,
// this list of conditions and the following disclaimer.
//
// - Redistribution in binary form must reproduce the above copyright
// notice, this list of conditions and the following disclaimer in the
// documentation and/or other materials provided with the
// distribution.
//
// - Neither the name of Sun Microsystems or the names of contributors may
// be used to endorse or promote products derived from this software without
// specific prior written permission.
//
// THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
// "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
// LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
// FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
// COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
// INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
// (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
// SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
// HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT,
// STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
// ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED
// OF THE POSSIBILITY OF SUCH DAMAGE.

// The original source code covered by the above license above has been
// modified significantly by Google Inc.
// Copyright 2012 the V8 project authors. All rights reserved.

//
// Copyright IBM Corp. 2012, 2013. All rights reserved.
//

#include "v8.h"

#if defined(V8_TARGET_ARCH_S390)

#include "s390/assembler-s390-inl.h"
#include "serialize.h"

namespace v8 {
namespace internal {

#ifdef DEBUG
bool CpuFeatures::initialized_ = false;
#endif
unsigned CpuFeatures::supported_ = 0;
unsigned CpuFeatures::found_by_runtime_probing_ = 0;

// Get the CPU features enabled by the build.
static unsigned CpuFeaturesImpliedByCompiler() {
  unsigned answer = 0;
  return answer;
}

#if !defined(_AIX)
// This function uses types in elf.h
static bool is_processor(const char* p) {
  static bool read_tried = false;
  static char *auxv_cpu_type = NULL;

  if (!read_tried) {
    // Open the AUXV (auxilliary vector) psuedo-file
    int fd = open("/proc/self/auxv", O_RDONLY);

    read_tried = true;
    if (fd != -1) {
#if V8_TARGET_ARCH_S390X
      static Elf64_auxv_t buffer[16];
      Elf64_auxv_t *auxv_element;
#else
      static Elf32_auxv_t buffer[16];
      Elf32_auxv_t *auxv_element;
#endif
      int bytes_read = 0;
      while (bytes_read >= 0) {
        // Read a chunk of the AUXV
        bytes_read = read(fd, buffer, sizeof(buffer));
        // Locate and read the platform field of AUXV if it is in the chunk
        for (auxv_element = buffer;
             auxv_element+sizeof(auxv_element) <= buffer+bytes_read &&
             auxv_element->a_type != AT_NULL;
             auxv_element++) {
          if (auxv_element->a_type == AT_PLATFORM) {
            /* Note: Both auxv_cpu_type and buffer are static */
            auxv_cpu_type = reinterpret_cast<char*>(auxv_element->a_un.a_val);
            goto done_reading;
          }
        }
      }
      done_reading:
      close(fd);
    }
  }

  if (auxv_cpu_type == NULL) {
    return false;
  }
  return (strcmp(auxv_cpu_type, p) == 0);
}
#endif

void CpuFeatures::Probe() {
  unsigned standard_features = static_cast<unsigned>(
      OS::CpuFeaturesImpliedByPlatform()) | CpuFeaturesImpliedByCompiler();
  ASSERT(supported_ == 0 || supported_ == standard_features);
#ifdef DEBUG
  initialized_ = true;
#endif

  // Get the features implied by the OS and the compiler settings. This is the
  // minimal set of features which is also alowed for generated code in the
  // snapshot.
  supported_ |= standard_features;

  if (Serializer::enabled()) {
    // No probing for features if we might serialize (generate snapshot).
    return;
  }

  // Detect whether frim instruction is supported (POWER5+)
  // For now we will just check for processors we know do not
  // support it
#if !defined(_AIX)
  if (!is_processor("ppc970") /* G5 */ && !is_processor("ppc7450") /* G4 */) {
    // Assume support
    supported_ |= (1u << FPU);
  }
#else
  // Fallback: assume frim is supported -- will implement processor
  // detection for other PPC platforms in is_processor() if required
  supported_ |= (1u << FPU);
#endif
}

Register ToRegister(int num) {
  ASSERT(num >= 0 && num < kNumRegisters);
  const Register kRegisters[] = {
    r0,
    r1,
    r2, r3, r4, r5, r6, r7, r8, r9, r10,
    r11, ip, r13, r14, sp,
    r16, r17, r18, r19, r20, r21, r22, r23, r24,
    r25, r26, r27, r28, r29, r30, fp
  };
  return kRegisters[num];
}


// -----------------------------------------------------------------------------
// Implementation of RelocInfo

const int RelocInfo::kApplyMask = 1 << RelocInfo::INTERNAL_REFERENCE;


bool RelocInfo::IsCodedSpecially() {
  // The deserializer needs to know whether a pointer is specially
  // coded.  Being specially coded on PPC means that it is a lis/ori
  // instruction sequence, and that is always the case inside code
  // objects.
  return true;
}


void RelocInfo::PatchCode(byte* instructions, int instruction_count) {
  // Patch the code at the current address with the supplied instructions.
  Instr* pc = reinterpret_cast<Instr*>(pc_);
  Instr* instr = reinterpret_cast<Instr*>(instructions);
  for (int i = 0; i < instruction_count; i++) {
    *(pc + i) = *(instr + i);
  }

  // Indicate that code has changed.
  CPU::FlushICache(pc_, instruction_count * Assembler::kInstrSize);
}


// Patch the code at the current PC with a call to the target address.
// Additional guard instructions can be added if required.
void RelocInfo::PatchCodeWithCall(Address target, int guard_bytes) {
  // Patch the code at the current address with a call to the target.
  UNIMPLEMENTED();
}


// -----------------------------------------------------------------------------
// Implementation of Operand and MemOperand
// See assembler-s390-inl.h for inlined constructors

Operand::Operand(Handle<Object> handle) {
  rm_ = no_reg;
  // Verify all Objects referred by code are NOT in new space.
  Object* obj = *handle;
  ASSERT(!HEAP->InNewSpace(obj));
  if (obj->IsHeapObject()) {
    imm_ = reinterpret_cast<intptr_t>(handle.location());
    rmode_ = RelocInfo::EMBEDDED_OBJECT;
  } else {
    // no relocation needed
    imm_ =  reinterpret_cast<intptr_t>(obj);
    rmode_ = RelocInfo::NONE;
  }
}

MemOperand::MemOperand(Register rn, int32_t offset) {
  ra_ = rn;
  rb_ = r0;
  offset_ = offset;
}

MemOperand::MemOperand(Register ra, Register rb, int32_t offset) {
  ra_ = ra;
  rb_ = rb;
  offset_ = offset;
}

// -----------------------------------------------------------------------------
// Specific instructions, constants, and masks.

// Spare buffer.
static const int kMinimalBufferSize = 4*KB;


Assembler::Assembler(Isolate* arg_isolate, void* buffer, int buffer_size)
    : AssemblerBase(arg_isolate),
      recorded_ast_id_(TypeFeedbackId::None()),
      positions_recorder_(this),
      emit_debug_code_(FLAG_debug_code),
      predictable_code_size_(false) {
  if (buffer == NULL) {
    // Do our own buffer management.
    if (buffer_size <= kMinimalBufferSize) {
      buffer_size = kMinimalBufferSize;

      if (isolate()->assembler_spare_buffer() != NULL) {
        buffer = isolate()->assembler_spare_buffer();
        isolate()->set_assembler_spare_buffer(NULL);
      }
    }
    if (buffer == NULL) {
      buffer_ = NewArray<byte>(buffer_size);
    } else {
      buffer_ = static_cast<byte*>(buffer);
    }
    buffer_size_ = buffer_size;
    own_buffer_ = true;

  } else {
    // Use externally provided buffer instead.
    ASSERT(buffer_size > 0);
    buffer_ = static_cast<byte*>(buffer);
    buffer_size_ = buffer_size;
    own_buffer_ = false;
  }

  // Set up buffer pointers.
  ASSERT(buffer_ != NULL);
  pc_ = buffer_;
  reloc_info_writer.Reposition(buffer_ + buffer_size, pc_);

  no_trampoline_pool_before_ = 0;
  trampoline_pool_blocked_nesting_ = 0;
  // We leave space (kMaxBlockTrampolineSectionSize)
  // for BlockTrampolinePoolScope buffer.
  next_buffer_check_ = kMaxCondBranchReach - kMaxBlockTrampolineSectionSize;
  internal_trampoline_exception_ = false;
  last_bound_pos_ = 0;

  trampoline_emitted_ = false;
  unbound_labels_count_ = 0;

  ClearRecordedAstId();
}


Assembler::~Assembler() {
  if (own_buffer_) {
    if (isolate()->assembler_spare_buffer() == NULL &&
        buffer_size_ == kMinimalBufferSize) {
      isolate()->set_assembler_spare_buffer(buffer_);
    } else {
      DeleteArray(buffer_);
    }
  }
}


void Assembler::GetCode(CodeDesc* desc) {
  // Set up code descriptor.
  desc->buffer = buffer_;
  desc->buffer_size = buffer_size_;
  desc->instr_size = pc_offset();
  desc->reloc_size = (buffer_ + buffer_size_) - reloc_info_writer.pos();
}


void Assembler::Align(int m) {
  ASSERT(m >= 4 && IsPowerOf2(m));
  while ((pc_offset() & (m - 1)) != 0) {
    nop();
  }
}


void Assembler::CodeTargetAlign() {
  Align(8);
}


Condition Assembler::GetCondition(Instr instr) {
  switch (instr & kCondMask) {
    case BT:
      return eq;
    case BF:
      return ne;
    default:
      UNIMPLEMENTED();
    }
  return al;
}

// PowerPC

bool Assembler::IsLis(Instr instr) {
  return (instr & kOpcodeMask) == ADDIS;
}

bool Assembler::IsAddic(Instr instr) {
  return (instr & kOpcodeMask) == ADDIC;
}

bool Assembler::IsOri(Instr instr) {
  return (instr & kOpcodeMask) == ORI;
}


bool Assembler::IsBranch(Instr instr) {
  return ((instr & kOpcodeMask) == BCX);
}

// end PowerPC

Register Assembler::GetRA(Instr instr) {
  Register reg;
  reg.code_ = Instruction::RAValue(instr);
  return reg;
}

Register Assembler::GetRB(Instr instr) {
  Register reg;
  reg.code_ = Instruction::RBValue(instr);
  return reg;
}

#if V8_TARGET_ARCH_S390X
// This code assumes a FIXED_SEQUENCE for 64bit loads (lis/ori)
bool Assembler::Is64BitLoadIntoR12(Instr instr1, Instr instr2,
                             Instr instr3, Instr instr4, Instr instr5) {
  // Check the instructions are indeed a five part load (into r12)
  // 3d800000       lis     r12, 0
  // 618c0000       ori     r12, r12, 0
  // 798c07c6       rldicr  r12, r12, 32, 31
  // 658c00c3       oris    r12, r12, 195
  // 618ccd40       ori     r12, r12, 52544
  return(((instr1 >> 16) == 0x3d80) && ((instr2 >> 16) == 0x618c) &&
         (instr3 == 0x798c07c6) &&
         ((instr4 >> 16) == 0x658c) && ((instr5 >> 16) == 0x618c));
}
#else
// This code assumes a FIXED_SEQUENCE for 32bit loads (lis/ori)
bool Assembler::Is32BitLoadIntoR12(Instr instr1, Instr instr2) {
  // Check the instruction is indeed a two part load (into r12)
  // 3d802553       lis     r12, 9555
  // 618c5000       ori   r12, r12, 20480
  return(((instr1 >> 16) == 0x3d80) && ((instr2 >> 16) == 0x618c));
}
#endif

bool Assembler::IsCmpRegister(Instr instr) {
  return (((instr & kOpcodeMask) == EXT2) &&
          ((instr & kExt2OpcodeMask) == CMP));
}

bool Assembler::IsRlwinm(Instr instr) {
  return ((instr & kOpcodeMask) == RLWINMX);
}

#if V8_TARGET_ARCH_S390X
bool Assembler::IsRldicl(Instr instr) {
  return (((instr & kOpcodeMask) == EXT5) &&
          ((instr & kExt5OpcodeMask) == RLDICL));
}
#endif

bool Assembler::IsCmpImmediate(Instr instr) {
  return ((instr & kOpcodeMask) == CMPI);
}

Register Assembler::GetCmpImmediateRegister(Instr instr) {
  ASSERT(IsCmpImmediate(instr));
  return GetRA(instr);
}

int Assembler::GetCmpImmediateRawImmediate(Instr instr) {
  ASSERT(IsCmpImmediate(instr));
  return instr & kOff16Mask;
}

// Labels refer to positions in the (to be) generated code.
// There are bound, linked, and unused labels.
//
// Bound labels refer to known positions in the already
// generated code. pos() is the position the label refers to.
//
// Linked labels refer to unknown positions in the code
// to be generated; pos() is the position of the last
// instruction using the label.


// The link chain is terminated by a negative code position (must be aligned)
const int kEndOfChain = -4;


int Assembler::target_at(int pos)  {
  Instr instr = instr_at(pos);
  // check which type of branch this is 16 or 26 bit offset
  int opcode = instr & kOpcodeMask;
  if (BX == opcode) {
    int imm26 = ((instr & kImm26Mask) << 6) >> 6;
    imm26 &= ~(kAAMask|kLKMask);  // discard AA|LK bits if present
    if (imm26 == 0)
        return kEndOfChain;
    return pos + imm26;
  } else if (BCX == opcode) {
    int imm16 = SIGN_EXT_IMM16((instr & kImm16Mask));
    imm16 &= ~(kAAMask|kLKMask);  // discard AA|LK bits if present
    if (imm16 == 0)
        return kEndOfChain;
    return pos + imm16;
  } else if ((instr & ~kImm16Mask) == 0) {
    // Emitted label constant, not part of a branch (regexp PushBacktrack).
     if (instr == 0) {
       return kEndOfChain;
     } else {
       int32_t imm16 = SIGN_EXT_IMM16(instr);
       return (imm16 + pos);
     }
  }

  PPCPORT_UNIMPLEMENTED();
  ASSERT(false);
  return -1;
}

void Assembler::target_at_put(int pos, int target_pos) {
  Instr instr = instr_at(pos);
  int opcode = instr & kOpcodeMask;

  // check which type of branch this is 16 or 26 bit offset
  if (BX == opcode) {
    int imm26 = target_pos - pos;
    instr &= ((~kImm26Mask)|kAAMask|kLKMask);
    ASSERT(is_int26(imm26));
    instr_at_put(pos, instr | (imm26 & kImm26Mask));
    return;
  } else if (BCX == opcode) {
    int imm16 = target_pos - pos;
    instr &= ((~kImm16Mask)|kAAMask|kLKMask);
    ASSERT(is_int16(imm16));
    instr_at_put(pos, instr | (imm16 & kImm16Mask));
    return;
  } else if ((instr & ~kImm16Mask) == 0) {
    ASSERT(target_pos == kEndOfChain || target_pos >= 0);
    // Emitted label constant, not part of a branch.
    // Make label relative to Code* of generated Code object.
    instr_at_put(pos, target_pos + (Code::kHeaderSize - kHeapObjectTag));
    return;
  }

  ASSERT(false);
}

int Assembler::max_reach_from(int pos) {
  Instr instr = instr_at(pos);
  int opcode = instr & kOpcodeMask;

  // check which type of branch this is 16 or 26 bit offset
  if (BX == opcode) {
    return 26;
  } else if (BCX == opcode) {
    return 16;
  } else if ((instr & ~kImm16Mask) == 0) {
    // Emitted label constant, not part of a branch (regexp PushBacktrack).
    return 16;
  }

  ASSERT(false);
  return 0;
}

void Assembler::bind_to(Label* L, int pos) {
  ASSERT(0 <= pos && pos <= pc_offset());  // must have a valid binding position
  int32_t trampoline_pos = kInvalidSlotPos;
  if (L->is_linked() && !trampoline_emitted_) {
    unbound_labels_count_--;
    next_buffer_check_ += kTrampolineSlotsSize;
  }

  while (L->is_linked()) {
    int fixup_pos = L->pos();
    int32_t offset = pos - fixup_pos;
    int maxReach = max_reach_from(fixup_pos);
    next(L);  // call next before overwriting link with target at fixup_pos
    if (is_intn(offset, maxReach) == false) {
      if (trampoline_pos == kInvalidSlotPos) {
        trampoline_pos = get_trampoline_entry();
        CHECK(trampoline_pos != kInvalidSlotPos);
        target_at_put(trampoline_pos, pos);
      }
      target_at_put(fixup_pos, trampoline_pos);
    } else {
      target_at_put(fixup_pos, pos);
    }
  }
  L->bind_to(pos);

  // Keep track of the last bound label so we don't eliminate any instructions
  // before a bound label.
  if (pos > last_bound_pos_)
    last_bound_pos_ = pos;
}

void Assembler::bind(Label* L) {
  ASSERT(!L->is_bound());  // label can only be bound once
  bind_to(L, pc_offset());
}


void Assembler::next(Label* L) {
  ASSERT(L->is_linked());
  int link = target_at(L->pos());
  if (link == kEndOfChain) {
    L->Unuse();
  } else {
    ASSERT(link >= 0);
    L->link_to(link);
  }
}

bool Assembler::is_near(Label* L, Condition cond) {
  ASSERT(L->is_bound());
  if (L->is_bound() == false)
    return false;

  int maxReach = ((cond == al) ? 26 : 16);
  int offset = L->pos() - pc_offset();

  return is_intn(offset, maxReach);
}

void Assembler::a_form(Instr instr,
                       DwVfpRegister frt,
                       DwVfpRegister fra,
                       DwVfpRegister frb,
                       RCBit r) {
  emit(instr | frt.code()*B21 | fra.code()*B16 | frb.code()*B11 | r);
}

void Assembler::d_form(Instr instr,
                        Register rt,
                        Register ra,
                        const intptr_t val,
                        bool signed_disp) {
  if (signed_disp) {
    if (!is_int16(val)) {
      PrintF("val = %" V8PRIdPTR ", 0x%" V8PRIxPTR "\n", val, val);
    }
    ASSERT(is_int16(val));
  } else {
    if (!is_uint16(val)) {
      PrintF("val = %" V8PRIdPTR ", 0x%" V8PRIxPTR
             ", is_unsigned_imm16(val)=%d, kImm16Mask=0x%x\n",
             val, val, is_uint16(val), kImm16Mask);
    }
    ASSERT(is_uint16(val));
  }
  emit(instr | rt.code()*B21 | ra.code()*B16 | (kImm16Mask & val));
}

void Assembler::x_form(Instr instr,
                         Register ra,
                         Register rs,
                         Register rb,
                         RCBit r) {
  emit(instr | rs.code()*B21 | ra.code()*B16 | rb.code()*B11 | r);
}

void Assembler::xo_form(Instr instr,
                         Register rt,
                         Register ra,
                         Register rb,
                         OEBit o,
                         RCBit r) {
  emit(instr | rt.code()*B21 | ra.code()*B16 | rb.code()*B11 | o | r);
}

void Assembler::md_form(Instr instr,
                        Register ra,
                        Register rs,
                        int shift,
                        int maskbit,
                        RCBit r) {
  int sh0_4 = shift & 0x1f;
  int sh5   = (shift >> 5) & 0x1;
  int m0_4  = maskbit & 0x1f;
  int m5    = (maskbit >> 5) & 0x1;

  emit(instr | rs.code()*B21 | ra.code()*B16 |
       sh0_4*B11 | m0_4*B6 | m5*B5 | sh5*B1 | r);
}

// Returns the next free trampoline entry.
int32_t Assembler::get_trampoline_entry() {
  int32_t trampoline_entry = kInvalidSlotPos;

  if (!internal_trampoline_exception_) {
    trampoline_entry = trampoline_.take_slot();

    if (kInvalidSlotPos == trampoline_entry) {
      internal_trampoline_exception_ = true;
    }
  }
  return trampoline_entry;
}

int Assembler::branch_offset(Label* L, bool jump_elimination_allowed) {
  int target_pos;
  if (L->is_bound()) {
    target_pos = L->pos();
  } else {
    if (L->is_linked()) {
      target_pos = L->pos();  // L's link
    } else {
      // was: target_pos = kEndOfChain;
      // However, using branch to self to mark the first reference
      // should avoid most instances of branch offset overflow.  See
      // target_at() for where this is converted back to kEndOfChain.
      target_pos = pc_offset();
      if (!trampoline_emitted_) {
        unbound_labels_count_++;
        next_buffer_check_ -= kTrampolineSlotsSize;
      }
    }
    L->link_to(pc_offset());
  }

  return target_pos - pc_offset();
}


void Assembler::label_at_put(Label* L, int at_offset) {
  int target_pos;
  if (L->is_bound()) {
    target_pos = L->pos();
    instr_at_put(at_offset, target_pos + (Code::kHeaderSize - kHeapObjectTag));
  } else {
    if (L->is_linked()) {
      target_pos = L->pos();  // L's link
    } else {
      // was: target_pos = kEndOfChain;
      // However, using branch to self to mark the first reference
      // should avoid most instances of branch offset overflow.  See
      // target_at() for where this is converted back to kEndOfChain.
      target_pos = at_offset;
      if (!trampoline_emitted_) {
        unbound_labels_count_++;
        next_buffer_check_ -= kTrampolineSlotsSize;
      }
    }
    L->link_to(at_offset);

    Instr constant = target_pos - at_offset;
    ASSERT(is_int16(constant));
    instr_at_put(at_offset, constant);
  }
}


// Branch instructions.

// PowerPC
void Assembler::bclr(BOfield bo, LKBit lk) {
  positions_recorder()->WriteRecordedPositions();
  emit(EXT1 | bo | BCLRX | lk);
}

void Assembler::bcctr(BOfield bo, LKBit lk) {
  positions_recorder()->WriteRecordedPositions();
  emit(EXT1 | bo | BCCTRX | lk);
}

// Pseudo op - branch to link register
void Assembler::blr() {
  bclr(BA, LeaveLK);
}

// Pseudo op - branch to count register -- used for "jump"
void Assembler::bcr() {
  bcctr(BA, LeaveLK);
}

void Assembler::bc(int branch_offset, BOfield bo, int condition_bit, LKBit lk) {
  positions_recorder()->WriteRecordedPositions();
  ASSERT(is_int16(branch_offset));
  emit(BCX | bo | condition_bit*B16 | (kImm16Mask & branch_offset) | lk);
}

void Assembler::b(int branch_offset, LKBit lk) {
  positions_recorder()->WriteRecordedPositions();
  ASSERT((branch_offset & 3) == 0);
  int imm26 = branch_offset;
  ASSERT(is_int26(imm26));
  // todo add AA and LK bits
  emit(BX | (imm26 & kImm26Mask) | lk);
}

// Indirect Branch via register
void Assembler::br(Register target) {
  bcr((Condition)0xF, target);
}

// Indirect Conditional Branch via register
void Assembler::bcr(Condition m, Register target) {
  rr_form(BCR, m, target);
}

void Assembler::xori(Register dst, Register src, const Operand& imm) {
  d_form(XORI, src, dst, imm.imm_, false);
}

void Assembler::xoris(Register ra, Register rs, const Operand& imm) {
  d_form(XORIS, rs, ra, imm.imm_, false);
}

void Assembler::xor_(Register dst, Register src1, Register src2, RCBit rc) {
  x_form(EXT2 | XORX, dst, src1, src2, rc);
}

void Assembler::cntlzw_(Register ra, Register rs, RCBit rc) {
  x_form(EXT2 | CNTLZWX, ra, rs, r0, rc);
}

void Assembler::and_(Register ra, Register rs, Register rb, RCBit rc) {
  x_form(EXT2 | ANDX, ra, rs, rb, rc);
}


void Assembler::rlwinm(Register ra, Register rs,
                       int sh, int mb, int me, RCBit rc) {
  sh &= 0x1f;
  mb &= 0x1f;
  me &= 0x1f;
  emit(RLWINMX | rs.code()*B21 | ra.code()*B16 | sh*B11 | mb*B6 | me << 1 | rc);
}

void Assembler::rlwimi(Register ra, Register rs,
                       int sh, int mb, int me, RCBit rc) {
  sh &= 0x1f;
  mb &= 0x1f;
  me &= 0x1f;
  emit(RLWIMIX | rs.code()*B21 | ra.code()*B16 | sh*B11 | mb*B6 | me << 1 | rc);
}

void Assembler::slwi(Register dst, Register src, const Operand& val,
                     RCBit rc) {
  ASSERT((32 > val.imm_)&&(val.imm_ >= 0));
  rlwinm(dst, src, val.imm_, 0, 31-val.imm_, rc);
}
void Assembler::srwi(Register dst, Register src, const Operand& val,
                     RCBit rc) {
  ASSERT((32 > val.imm_)&&(val.imm_ >= 0));
  rlwinm(dst, src, 32-val.imm_, val.imm_, 31, rc);
}
void Assembler::clrrwi(Register dst, Register src, const Operand& val,
                       RCBit rc) {
  ASSERT((32 > val.imm_)&&(val.imm_ >= 0));
  rlwinm(dst, src, 0, 0, 31-val.imm_, rc);
}
void Assembler::clrlwi(Register dst, Register src, const Operand& val,
                       RCBit rc) {
  ASSERT((32 > val.imm_)&&(val.imm_ >= 0));
  rlwinm(dst, src, 0, val.imm_, 31, rc);
}


void Assembler::srawi(Register ra, Register rs, int sh, RCBit r) {
  emit(EXT2 | SRAWIX | rs.code()*B21 | ra.code()*B16 | sh*B11 | r);
}

void Assembler::srw(Register dst, Register src1, Register src2, RCBit r) {
  x_form(EXT2 | SRWX, dst, src1, src2, r);
}

void Assembler::slw(Register dst, Register src1, Register src2, RCBit r) {
  x_form(EXT2 | SLWX, dst, src1, src2, r);
}

void Assembler::sraw(Register ra, Register rs, Register rb, RCBit r) {
  x_form(EXT2 | SRAW, ra, rs, rb, r);
}

void Assembler::subi(Register dst, Register src, const Operand& imm) {
  addi(dst, src, Operand(-(imm.imm_)));
}

void Assembler::addc(Register dst, Register src1, Register src2,
                    OEBit o, RCBit r) {
  xo_form(EXT2 | ADDCX, dst, src1, src2, o, r);
}

void Assembler::addze(Register dst, Register src1, OEBit o, RCBit r) {
  // a special xo_form
  emit(EXT2 | ADDZEX | dst.code()*B21 | src1.code()*B16 | o | r);
}

void Assembler::sub(Register dst, Register src1, Register src2,
                    OEBit o, RCBit r) {
  xo_form(EXT2 | SUBFX, dst, src2, src1, o, r);
}

void Assembler::subfc(Register dst, Register src1, Register src2,
                    OEBit o, RCBit r) {
  xo_form(EXT2 | SUBFCX, dst, src2, src1, o, r);
}

void Assembler::subfic(Register dst, Register src, const Operand& imm) {
  d_form(SUBFIC, dst, src, imm.imm_, true);
}

void Assembler::add(Register dst, Register src1, Register src2,
                    OEBit o, RCBit r) {
  xo_form(EXT2 | ADDX, dst, src1, src2, o, r);
}

// Multiply low word
void Assembler::mullw(Register dst, Register src1, Register src2,
                    OEBit o, RCBit r) {
  xo_form(EXT2 | MULLW, dst, src1, src2, o, r);
}

// Multiply hi word
void Assembler::mulhw(Register dst, Register src1, Register src2,
                    OEBit o, RCBit r) {
  xo_form(EXT2 | MULHWX, dst, src1, src2, o, r);
}

// Divide word
void Assembler::divw(Register dst, Register src1, Register src2,
                     OEBit o, RCBit r) {
  xo_form(EXT2 | DIVW, dst, src1, src2, o, r);
}

void Assembler::addi(Register dst, Register src, const Operand& imm) {
    if (dst.code() == src.code()) {
#ifdef V8_TARGET_ARCH_S390X
        aghi(dst, imm);
#else
        ahi(dst, imm);
#endif
    } else {
#ifdef V8_TARGET_ARCH_S390X
        lghi(dst, imm);
        agr(dst, src);
#else
        lhi(dst, imm);
        ar(dst, src);
#endif
    }
}

void  Assembler::addis(Register dst, Register src, const Operand& imm) {
  d_form(ADDIS, dst, src, imm.imm_, true);
}

void Assembler::addic(Register dst, Register src, const Operand& imm) {
  d_form(ADDIC, dst, src, imm.imm_, true);
}

void  Assembler::andi(Register ra, Register rs, const Operand& imm) {
  // not correct
  if (ra.code() == rs.code()) {
    nilf(rs, imm);
  } else {
#ifdef V8_TARGET_ARCH_S390X
    agr(ra, rs);
    nill(ra, imm);
#else
    ar(ra, rs);
    nill(ra, imm);
#endif
  }
}

void Assembler::andis(Register ra, Register rs, const Operand& imm) {
  d_form(ANDISx, rs, ra, imm.imm_, false);
}

void Assembler::nor(Register dst, Register src1, Register src2, RCBit r) {
  x_form(EXT2 | NORX, dst, src1, src2, r);
}

void Assembler::notx(Register dst, Register src, RCBit r) {
  x_form(EXT2 | NORX, dst, src, src, r);
}

void Assembler::ori(Register ra, Register rs, const Operand& imm) {
    if (ra.code() != rs.code()) {
#ifdef V8_TARGET_ARCH_S390X
        agr(ra, rs);
#else
        ar(ra, rs);
#endif
    }
    oill(ra, imm);
}

void Assembler::oris(Register dst, Register src, const Operand& imm) {
  d_form(ORIS, src, dst, imm.imm_, false);
}

void Assembler::orx(Register dst, Register src1, Register src2, RCBit rc) {
  x_form(EXT2 | ORX, dst, src1, src2, rc);
}

void Assembler::cmpi(Register src1, const Operand& src2, CRegister cr) {
    intptr_t imm16 = src2.imm_;
    ASSERT(is_int16(imm16));
    imm16 &= kImm16Mask;
#if V8_TARGET_ARCH_S390X
    cghi(src1, src2);
#else
    chi(src1, src2);
#endif
}

void Assembler::cmpli(Register src1, const Operand& src2, CRegister cr) {
  uintptr_t uimm16 = src2.imm_;
  ASSERT(is_uint16(uimm16));
  uimm16 &= kImm16Mask;
  // we could you CLFHSI but z9 does not have it.
  // use CLFI instead
#if V8_TARGET_ARCH_S390X
    clgfhi(src1, src2);
#else
    clfi(src1, src2);
#endif
}

void Assembler::cmp(Register src1, Register src2, CRegister cr) {
#if V8_TARGET_ARCH_S390X
  int L = 1;
#else
  int L = 0;
#endif
  ASSERT(cr.code() >= 0 && cr.code() <= 7);
  emit(EXT2 | CMP | cr.code()*B23 | L*B21 | src1.code()*B16 |
       src2.code()*B11);
}

void Assembler::cmpl(Register src1, Register src2, CRegister cr) {
#if V8_TARGET_ARCH_S390X
    clgr(src1, src2);
#else
    clr(src1, src2);
#endif
}

// Load Halfword Immediate - 16-bit signed immediate
void Assembler::lhi(Register dst, const Operand& imm) {
  ri_form(LHI, dst, imm);
}

void  Assembler::lis(Register dst, const Operand& imm) {
  d_form(ADDIS, dst, r0, imm.imm_, true);
}

// Pseudo op - move register
void Assembler::mr(Register dst, Register src) {
#if V8_TARGET_ARCH_S390X
    lgr(dst, src);
#else
    lr(dst, src);
#endif
}

void Assembler::lbz(Register dst, const MemOperand &src) {
  ASSERT(!src.ra_.is(r0));
  d_form(LBZ, dst, src.ra(), src.offset(), true);
}

void Assembler::lbzx(Register rt, const MemOperand &src) {
  Register ra = src.ra();
  Register rb = src.rb();
  ASSERT(!ra.is(r0));
  emit(EXT2 | LBZX | rt.code()*B21 | ra.code()*B16 | rb.code()*B11 | LeaveRC);
}

void Assembler::lbzux(Register rt, const MemOperand & src) {
  Register ra = src.ra();
  Register rb = src.rb();
  ASSERT(!ra.is(r0));
  emit(EXT2 | LBZUX | rt.code()*B21 | ra.code()*B16 | rb.code()*B11 | LeaveRC);
}

void Assembler::lhz(Register dst, const MemOperand &src) {
#ifdef V8_TARGET_ARCH_S390X
    llgh(dst, src);
#else
    llh(dst, src);
#endif
}

void Assembler::lhzx(Register rt, const MemOperand &src) {
    // same as lhz, we can use RX form
#ifdef V8_TARGET_ARCH_S390X
    llgh(rt, src);
#else
    llh(rt, src);
#endif
}

void Assembler::lhzux(Register rt, const MemOperand & src) {
    Register ra = src.ra();
#ifdef V8_TARGET_ARCH_S390X
    lay(ra, src);
    llghr(rt, ra);
#else
    la(ra, src);
    llhr(rt, ra);
#endif
}

void Assembler::lwz(Register dst, const MemOperand &src) {
    Register rb = src.rb();
    ASSERT(rb.code() == 0);
#ifdef V8_TARGET_ARCH_S390X
    llgf(dst, src);
#else
    ly(dst, src);
#endif
}

void Assembler::lwzu(Register dst, const MemOperand &src) {
  d_form(LWZU, dst, src.ra(), src.offset(), true);
}

void Assembler::lwzx(Register rt, const MemOperand &src) {
    // same as lwz, but in lwz we have gpr0 as index reg
    // here displacement is 0
    ASSERT(src.offset() == 0);
#if V8_TARGET_ARCH_S390X
    llgf(rt, src);
#else
    ly(rt, src);
#endif
}

void Assembler::lwzux(Register rt, const MemOperand & src) {
  Register ra = src.ra();
  Register rb = src.rb();
  emit(EXT2 | LWZUX | rt.code()*B21 | ra.code()*B16 | rb.code()*B11 | LeaveRC);
}

void Assembler::lwa(Register dst, const MemOperand &src) {
#if V8_TARGET_ARCH_S390X
  int offset = src.offset();
  ASSERT(!(offset & 3) && is_int16(offset));
  offset = kImm16Mask & offset;
  emit(LD_ppc | dst.code()*B21 | src.ra().code()*B16 | offset | 2);
#else
  lwz(dst, src);
#endif
}

// 32-bit Load Multiple - short displacement (12-bits unsigned)
void Assembler::lm(Register r1, Register r2, const MemOperand& src) {
  rs_form(LM, r1, r2, src.ra(), src.offset());
}
// 32-bit Load Multiple - long displacement (20-bits signed)
void Assembler::lmy(Register r1, Register r2, const MemOperand& src) {
  rsy_form(LMY, r1, r2, src.ra(), src.offset());
}
// 64-bit Load Multiple - long displacement (20-bits signed)
void Assembler::lmg(Register r1, Register r2, const MemOperand& src) {
  rsy_form(LMG, r1, r2, src.ra(), src.offset());
}

void Assembler::stb(Register dst, const MemOperand &src) {
    Register rb = src.rb();
    ASSERT(rb.code() == 0);
    // temporarily use stcy here because stc in ppc contains
    // 16 bit displacement
    stcy(dst, src);
}

void Assembler::stbx(Register rs, const MemOperand &src) {
    ASSERT(src.offset() == 0);
    // temporarily use stcy here because stc in ppc contains
    // 16 bit displacement
    stcy(rs, src);
}

void Assembler::stbux(Register rs, const MemOperand &src) {
  Register ra = src.ra();
  Register rb = src.rb();
  emit(EXT2 | STBUX | rs.code()*B21 | ra.code()*B16 | rb.code()*B11 | LeaveRC);
}

void Assembler::sthx(Register rs, const MemOperand &src) {
  Register ra = src.ra();
  Register rb = src.rb();
  emit(EXT2 | STHX | rs.code()*B21 | ra.code()*B16 | rb.code()*B11 | LeaveRC);
}

void Assembler::sthux(Register rs, const MemOperand &src) {
  Register ra = src.ra();
  Register rb = src.rb();
  emit(EXT2 | STHUX | rs.code()*B21 | ra.code()*B16 | rb.code()*B11 | LeaveRC);
}

// 32-bit Store
void Assembler::st(Register dst, const MemOperand &src) {
  int offset = src.offset();
  if (!is_uint12(offset)) {
    // @TODO Remove once things are clean....
    // Check limits... should check if STY is usable and replace at source.
    PrintF("ST offset exceeded limits = %" V8PRIdPTR ", 0x%" V8PRIxPTR "\n",
           offset, offset);
  }
  rx_form(ST, dst, src.ra(), src.rb(), src.offset());
}

void Assembler::stwu(Register dst, const MemOperand &src) {
  d_form(STWU, dst, src.ra(), src.offset(), true);
}

void Assembler::stwux(Register rs, const MemOperand &src) {
  Register ra = src.ra();
  Register rb = src.rb();
  emit(EXT2 | STWUX | rs.code()*B21 | ra.code()*B16 | rb.code()*B11 | LeaveRC);
}

// 32-bit Store Multiple - short displacement (12-bits unsigned)
void Assembler::stm(Register r1, Register r2, const MemOperand& src) {
  rs_form(STM, r1, r2, src.ra(), src.offset());
}
// 32-bit Store Multiple - long displacement (20-bits signed)
void Assembler::stmy(Register r1, Register r2, const MemOperand& src) {
  rsy_form(STMY, r1, r2, src.ra(), src.offset());
}
// 64-bit Store Multiple - long displacement (20-bits signed)
void Assembler::stmg(Register r1, Register r2, const MemOperand& src) {
  rsy_form(STMG, r1, r2, src.ra(), src.offset());
}

void Assembler::extsb(Register rs, Register ra, RCBit rc) {
  emit(EXT2 | EXTSB | ra.code()*B21 | rs.code()*B16 | rc);
}

void Assembler::extsh(Register rs, Register ra, RCBit rc) {
  emit(EXT2 | EXTSH | ra.code()*B21 | rs.code()*B16 | rc);
}

void Assembler::neg(Register rt, Register ra, OEBit o, RCBit r) {
  emit(EXT2 | NEGX | rt.code()*B21 | ra.code()*B16 | o | r);
}

void Assembler::andc(Register dst, Register src1, Register src2, RCBit rc) {
  x_form(EXT2 | ANDCX, dst, src1, src2, rc);
}

#if V8_TARGET_ARCH_S390X
// 64bit specific instructions
void Assembler::ld(Register rd, const MemOperand &src) {
  int offset = src.offset();
  ASSERT(!(offset & 3) && is_int16(offset));
  offset = kImm16Mask & offset;
  emit(LD_ppc | rd.code()*B21 | src.ra().code()*B16 | offset);
}

void Assembler::ldx(Register rd, const MemOperand &src) {
  Register ra = src.ra();
  Register rb = src.rb();
  emit(EXT2 | LDX | rd.code()*B21 | ra.code()*B16 | rb.code()*B11);
}

void Assembler::ldu(Register rd, const MemOperand &src) {
  int offset = src.offset();
  ASSERT(!(offset & 3) && is_int16(offset));
  offset = kImm16Mask & offset;
  emit(LD_ppc | rd.code()*B21 | src.ra().code()*B16 | offset | 1);
}

void Assembler::ldux(Register rd, const MemOperand &src) {
  Register ra = src.ra();
  Register rb = src.rb();
  emit(EXT2 | LDUX | rd.code()*B21 | ra.code()*B16 | rb.code()*B11);
}

// 64-bit Store
void Assembler::stg(Register rs, const MemOperand &src) {
  int offset = src.offset();
  ASSERT(is_int20(offset));
  // RXY_from(STG, rs, src.ra(), src.rb(), src.offset());
  emit(STD_ppc | rs.code()*B21 | src.ra().code()*B16 | offset);
}

void Assembler::stdu(Register rs, const MemOperand &src) {
  int offset = src.offset();
  ASSERT(!(offset & 3) && is_int16(offset));
  offset = kImm16Mask & offset;
  emit(STD_ppc | rs.code()*B21 | src.ra().code()*B16 | offset | 1);
}

void Assembler::stdux(Register rs, const MemOperand &src) {
  Register ra = src.ra();
  Register rb = src.rb();
  emit(EXT2 | STDUX | rs.code()*B21 | ra.code()*B16 | rb.code()*B11);
}

void Assembler::rldic(Register ra, Register rs, int sh, int mb, RCBit r) {
  md_form(EXT5 | RLDIC, ra, rs, sh, mb, r);
}

void Assembler::rldicl(Register ra, Register rs, int sh, int mb, RCBit r) {
  md_form(EXT5 | RLDICL, ra, rs, sh, mb, r);
}

void Assembler::rldicr(Register ra, Register rs, int sh, int me, RCBit r) {
  md_form(EXT5 | RLDICR, ra, rs, sh, me, r);
}

void Assembler::sldi(Register dst, Register src, const Operand& val,
                     RCBit rc) {
  ASSERT((64 > val.imm_)&&(val.imm_ >= 0));
  rldicr(dst, src, val.imm_, 63-val.imm_, rc);
}
void Assembler::srdi(Register dst, Register src, const Operand& val,
                     RCBit rc) {
  ASSERT((64 > val.imm_)&&(val.imm_ >= 0));
  rldicl(dst, src, 64-val.imm_, val.imm_, rc);
}
void Assembler::clrrdi(Register dst, Register src, const Operand& val,
                       RCBit rc) {
  ASSERT((64 > val.imm_)&&(val.imm_ >= 0));
  rldicr(dst, src, 0, 63-val.imm_, rc);
}
void Assembler::clrldi(Register dst, Register src, const Operand& val,
                       RCBit rc) {
  ASSERT((64 > val.imm_)&&(val.imm_ >= 0));
  rldicl(dst, src, 0, val.imm_, rc);
}

void Assembler::sradi(Register ra, Register rs, int sh, RCBit r) {
  int sh0_4 = sh & 0x1f;
  int sh5   = (sh >> 5) & 0x1;

  emit(EXT2 | SRADIX | rs.code()*B21 | ra.code()*B16 | sh0_4*B11 | sh5*B1 | r);
}

void Assembler::srd(Register dst, Register src1, Register src2, RCBit r) {
  x_form(EXT2 | SRDX, dst, src1, src2, r);
}

void Assembler::sld(Register dst, Register src1, Register src2, RCBit r) {
  x_form(EXT2 | SLDX, dst, src1, src2, r);
}

void Assembler::srad(Register ra, Register rs, Register rb, RCBit r) {
  x_form(EXT2 | SRAD, ra, rs, rb, r);
}

void Assembler::cntlzd_(Register ra, Register rs, RCBit rc) {
  x_form(EXT2 | CNTLZDX, ra, rs, r0, rc);
}

void Assembler::extsw(Register rs, Register ra, RCBit rc) {
  emit(EXT2 | EXTSW | ra.code()*B21 | rs.code()*B16 | rc);
}

void Assembler::mulld(Register dst, Register src1, Register src2,
                      OEBit o, RCBit r) {
  xo_form(EXT2 | MULLD, dst, src1, src2, o, r);
}

void Assembler::divd(Register dst, Register src1, Register src2,
                     OEBit o, RCBit r) {
  xo_form(EXT2 | DIVD, dst, src1, src2, o, r);
}
#endif


void Assembler::fake_asm(enum FAKE_OPCODE_T fopcode) {
  ASSERT(fopcode < fLastFaker);
  emit(FAKE_OPCODE | FAKER_SUBOPCODE | fopcode);
}

void Assembler::marker_asm(int mcode) {
  if (::v8::internal::FLAG_trace_sim_stubs) {
    ASSERT(mcode < F_NEXT_AVAILABLE_STUB_MARKER);
    emit(FAKE_OPCODE | MARKER_SUBOPCODE | mcode);
  }
}

// Function descriptor for AIX.
// Code address skips the function descriptor "header".
// TOC and static chain are ignored and set to 0.
void Assembler::function_descriptor() {
  RecordRelocInfo(RelocInfo::INTERNAL_REFERENCE);
#if V8_TARGET_ARCH_S390X
  uint64_t value = reinterpret_cast<uint64_t>(pc_) + 3 * kPointerSize;
#if __BYTE_ORDER == __LITTLE_ENDIAN
  emit(static_cast<uint32_t>(value & 0xFFFFFFFF));
  emit(static_cast<uint32_t>(value >> 32));
#else
  emit(static_cast<uint32_t>(value >> 32));
  emit(static_cast<uint32_t>(value & 0xFFFFFFFF));
#endif
  emit(static_cast<Instr>(0));
  emit(static_cast<Instr>(0));
  emit(static_cast<Instr>(0));
  emit(static_cast<Instr>(0));
#else
  emit(reinterpret_cast<Instr>(pc_) + 3 * kPointerSize);
  emit(static_cast<Instr>(0));
  emit(static_cast<Instr>(0));
#endif
}
// end PowerPC

// Primarily used for loading constants
// This should really move to be in macro-assembler as it
// is really a pseudo instruction
// Some usages of this intend for a FIXED_SEQUENCE to be used
// Todo - break this dependency so we can optimize mov() in general
// and only use the generic version when we require a fixed sequence
void Assembler::mov(Register dst, const Operand& src) {
  BlockTrampolinePoolScope block_trampoline_pool(this);
  if (src.rmode_ != RelocInfo::NONE) {
    // some form of relocation needed
    RecordRelocInfo(src.rmode_, src.imm_);
  }

#if V8_TARGET_ARCH_S390X
  int64_t value = src.immediate();
  int32_t hi_32 = static_cast<int64_t>(value) >> 32;
  int32_t lo_32 = static_cast<int32_t>(value);
  int hi_word = static_cast<int>(hi_32) >> 16;
  int lo_word = static_cast<int>(hi_32) & 0xFFFF;
  lis(dst, Operand(SIGN_EXT_IMM16(hi_word)));
  ori(dst, dst, Operand(lo_word));
  sldi(dst, dst, Operand(32));
  hi_word = (static_cast<int>(lo_32) >> 16) & 0xFFFF;
  lo_word = static_cast<int>(lo_32) & 0xFFFF;
  oris(dst, dst, Operand(hi_word));
  ori(dst, dst, Operand(lo_word));
#else
  int value = src.immediate();
  if (!is_trampoline_pool_blocked()) {
    if (is_int16(value)) {
      lhi(dst, Operand(value));
      return;
    }
  }
  int hi_word = static_cast<int>(value) >> 16;
  int lo_word = static_cast<int>(value) & 0XFFFF;

  lis(dst, Operand(SIGN_EXT_IMM16(hi_word)));
  if ((!is_trampoline_pool_blocked()) && (lo_word == 0)) {
    return;
  }
  ori(dst, dst, Operand(lo_word));
#endif
}

// Special register instructions
void Assembler::crxor(int bt, int ba, int bb) {
  emit(EXT1 | CRXOR | bt*B21 | ba*B16 | bb*B11);
}

void Assembler::mflr(Register dst) {
  emit(EXT2 | MFSPR | dst.code()*B21 | 256 << 11);   // Ignore RC bit
}

void Assembler::mtlr(Register src) {
  emit(EXT2 | MTSPR | src.code()*B21 | 256 << 11);   // Ignore RC bit
}

void Assembler::mtctr(Register src) {
  emit(EXT2 | MTSPR | src.code()*B21 | 288 << 11);   // Ignore RC bit
}

void Assembler::mtxer(Register src) {
  emit(EXT2 | MTSPR | src.code()*B21 | 32 << 11);
}

void Assembler::mcrfs(int bf, int bfa) {
  emit(EXT4 | MCRFS | bf*B23 | bfa*B18);
}

void Assembler::mfcr(Register dst) {
  emit(EXT2 | MFCR | dst.code()*B21);
}

// end PowerPC

// Exception-generating instructions and debugging support.
// Stops with a non-negative code less than kNumOfWatchedStops support
// enabling/disabling and a counter feature. See simulator-ppc.h .
void Assembler::stop(const char* msg, Condition cond, int32_t code,
                     CRegister cr) {
  if (cond != al) {
    Label skip;
    b(NegateCondition(cond), &skip, cr);
    bkpt(0);
    bind(&skip);
  } else {
    bkpt(0);
  }
}

void Assembler::bkpt(uint32_t imm16) {
  emit(0x7d821008);
}


void Assembler::info(const char* msg, Condition cond, int32_t code,
                     CRegister cr) {
  if (::v8::internal::FLAG_trace_sim_stubs) {
    emit(0x7d9ff808);
#if V8_TARGET_ARCH_S390X
    uint64_t value = reinterpret_cast<uint64_t>(msg);
    emit(static_cast<uint32_t>(value >> 32));
    emit(static_cast<uint32_t>(value & 0xFFFFFFFF));
#else
    emit(reinterpret_cast<Instr>(msg));
#endif
  }
}

void Assembler::dcbf(Register ra, Register rb) {
    emit(EXT2 | DCBF | ra.code()*B16 | rb.code()*B11);
}

void Assembler::sync() {
    emit(EXT2 | SYNC);
}

void Assembler::icbi(Register ra, Register rb) {
    emit(EXT2 | ICBI | ra.code()*B16 | rb.code()*B11);
}

void Assembler::isync() {
    emit(EXT1 | ISYNC);
}

// Floating point support

void Assembler::lfd(const DwVfpRegister frt, const MemOperand &src) {
  int offset = src.offset();
  Register ra = src.ra();
  ASSERT(is_int16(offset));
  int imm16 = offset & kImm16Mask;
  // could be x_form instruction with some casting magic
  emit(LFD | frt.code()*B21 | ra.code()*B16 | imm16);
}

void Assembler::lfdu(const DwVfpRegister frt, const MemOperand &src) {
  int offset = src.offset();
  Register ra = src.ra();
  ASSERT(is_int16(offset));
  int imm16 = offset & kImm16Mask;
  // could be x_form instruction with some casting magic
  emit(LFDU | frt.code()*B21 | ra.code()*B16 | imm16);
}

void Assembler::lfdx(const DwVfpRegister frt, const MemOperand &src) {
  Register ra = src.ra();
  Register rb = src.rb();
  ASSERT(!ra.is(r0));
  emit(EXT2 | LFDX | frt.code()*B21 | ra.code()*B16 | rb.code()*B11 | LeaveRC);
}

void Assembler::lfdux(const DwVfpRegister frt, const MemOperand & src) {
  Register ra = src.ra();
  Register rb = src.rb();
  ASSERT(!ra.is(r0));
  emit(EXT2 | LFDUX | frt.code()*B21 | ra.code()*B16 | rb.code()*B11 | LeaveRC);
}

void Assembler::lfs(const DwVfpRegister frt, const MemOperand &src) {
  int offset = src.offset();
  Register ra = src.ra();
  ASSERT(is_int16(offset));
  ASSERT(!ra.is(r0));
  int imm16 = offset & kImm16Mask;
  // could be x_form instruction with some casting magic
  emit(LFS | frt.code()*B21 | ra.code()*B16 | imm16);
}

void Assembler::lfsu(const DwVfpRegister frt, const MemOperand &src) {
  int offset = src.offset();
  Register ra = src.ra();
  ASSERT(is_int16(offset));
  ASSERT(!ra.is(r0));
  int imm16 = offset & kImm16Mask;
  // could be x_form instruction with some casting magic
  emit(LFSU | frt.code()*B21 | ra.code()*B16 | imm16);
}

void Assembler::lfsx(const DwVfpRegister frt, const MemOperand &src) {
  Register ra = src.ra();
  Register rb = src.rb();
  ASSERT(!ra.is(r0));
  emit(EXT2 | LFSX | frt.code()*B21 | ra.code()*B16 | rb.code()*B11 | LeaveRC);
}

void Assembler::lfsux(const DwVfpRegister frt, const MemOperand & src) {
  Register ra = src.ra();
  Register rb = src.rb();
  ASSERT(!ra.is(r0));
  emit(EXT2 | LFSUX | frt.code()*B21 | ra.code()*B16 | rb.code()*B11 | LeaveRC);
}

void Assembler::stfd(const DwVfpRegister frs, const MemOperand &src) {
  int offset = src.offset();
  Register ra = src.ra();
  ASSERT(is_int16(offset));
  ASSERT(!ra.is(r0));
  int imm16 = offset & kImm16Mask;
  // could be x_form instruction with some casting magic
  emit(STFD | frs.code()*B21 | ra.code()*B16 | imm16);
}

void Assembler::stfdu(const DwVfpRegister frs, const MemOperand &src) {
  int offset = src.offset();
  Register ra = src.ra();
  ASSERT(is_int16(offset));
  ASSERT(!ra.is(r0));
  int imm16 = offset & kImm16Mask;
  // could be x_form instruction with some casting magic
  emit(STFDU | frs.code()*B21 | ra.code()*B16 | imm16);
}

void Assembler::stfdx(const DwVfpRegister frs, const MemOperand &src) {
  Register ra = src.ra();
  Register rb = src.rb();
  ASSERT(!ra.is(r0));
  emit(EXT2 | STFDX | frs.code()*B21 | ra.code()*B16 | rb.code()*B11 | LeaveRC);
}

void Assembler::stfdux(const DwVfpRegister frs, const MemOperand &src) {
  Register ra = src.ra();
  Register rb = src.rb();
  ASSERT(!ra.is(r0));
  emit(EXT2 | STFDUX | frs.code()*B21 | ra.code()*B16 | rb.code()*B11 |LeaveRC);
}

void Assembler::stfs(const DwVfpRegister frs, const MemOperand &src) {
  int offset = src.offset();
  Register ra = src.ra();
  ASSERT(is_int16(offset));
  ASSERT(!ra.is(r0));
  int imm16 = offset & kImm16Mask;
  // could be x_form instruction with some casting magic
  emit(STFS | frs.code()*B21 | ra.code()*B16 | imm16);
}

void Assembler::stfsu(const DwVfpRegister frs, const MemOperand &src) {
  int offset = src.offset();
  Register ra = src.ra();
  ASSERT(is_int16(offset));
  ASSERT(!ra.is(r0));
  int imm16 = offset & kImm16Mask;
  // could be x_form instruction with some casting magic
  emit(STFSU | frs.code()*B21 | ra.code()*B16 | imm16);
}

void Assembler::stfsx(const DwVfpRegister frs, const MemOperand &src) {
  Register ra = src.ra();
  Register rb = src.rb();
  ASSERT(!ra.is(r0));
  emit(EXT2 | STFSX | frs.code()*B21 | ra.code()*B16 | rb.code()*B11 |LeaveRC);
}

void Assembler::stfsux(const DwVfpRegister frs, const MemOperand &src) {
  Register ra = src.ra();
  Register rb = src.rb();
  ASSERT(!ra.is(r0));
  emit(EXT2 | STFSUX | frs.code()*B21 | ra.code()*B16 | rb.code()*B11 |LeaveRC);
}

void Assembler::fsub(const DwVfpRegister frt,
                     const DwVfpRegister fra,
                     const DwVfpRegister frb,
                     RCBit rc) {
  a_form(EXT4 | FSUB, frt, fra, frb, rc);
}

void Assembler::fadd(const DwVfpRegister frt,
                     const DwVfpRegister fra,
                     const DwVfpRegister frb,
                     RCBit rc) {
  a_form(EXT4 | FADD, frt, fra, frb, rc);
}
void Assembler::fmul(const DwVfpRegister frt,
                     const DwVfpRegister fra,
                     const DwVfpRegister frc,
                     RCBit rc) {
  emit(EXT4 | FMUL | frt.code()*B21 | fra.code()*B16 | frc.code()*B6 | rc);
}
void Assembler::fdiv(const DwVfpRegister frt,
                     const DwVfpRegister fra,
                     const DwVfpRegister frb,
                     RCBit rc) {
  a_form(EXT4 | FDIV, frt, fra, frb, rc);
}

void Assembler::fcmpu(const DwVfpRegister fra,
                      const DwVfpRegister frb,
                      CRegister cr) {
  ASSERT(cr.code() >= 0 && cr.code() <= 7);
  emit(EXT4 | FCMPU | cr.code()*B23 | fra.code()*B16 | frb.code()*B11);
}

void Assembler::fmr(const DwVfpRegister frt,
                    const DwVfpRegister frb,
                    RCBit rc) {
  emit(EXT4 | FMR | frt.code()*B21 | frb.code()*B11 | rc);
}

void Assembler::fctiwz(const DwVfpRegister frt,
                     const DwVfpRegister frb) {
  emit(EXT4 | FCTIWZ | frt.code()*B21 | frb.code()*B11);
}

void Assembler::fctiw(const DwVfpRegister frt,
                     const DwVfpRegister frb) {
  emit(EXT4 | FCTIW | frt.code()*B21 | frb.code()*B11);
}

void Assembler::frim(const DwVfpRegister frt,
                     const DwVfpRegister frb) {
  emit(EXT4 | FRIM | frt.code()*B21 | frb.code()*B11);
}

void Assembler::frsp(const DwVfpRegister frt,
                     const DwVfpRegister frb,
                     RCBit rc) {
  emit(EXT4 | FRSP | frt.code()*B21 | frb.code()*B11 | rc);
}

void Assembler::fcfid(const DwVfpRegister frt,
                      const DwVfpRegister frb,
                      RCBit rc) {
  emit(EXT4 | FCFID | frt.code()*B21 | frb.code()*B11 | rc);
}

void Assembler::fctid(const DwVfpRegister frt,
                      const DwVfpRegister frb,
                      RCBit rc) {
  emit(EXT4 | FCTID | frt.code()*B21 | frb.code()*B11 | rc);
}

void Assembler::fctidz(const DwVfpRegister frt,
                      const DwVfpRegister frb,
                      RCBit rc) {
  emit(EXT4 | FCTIDZ | frt.code()*B21 | frb.code()*B11 | rc);
}

void Assembler::fsel(const DwVfpRegister frt, const DwVfpRegister fra,
                     const DwVfpRegister frc, const DwVfpRegister frb,
                     RCBit rc) {
  emit(EXT4 | FSEL | frt.code()*B21 | fra.code()*B16 | frb.code()*B11 |
       frc.code()*B6 | rc);
}

void Assembler::fneg(const DwVfpRegister frt,
                     const DwVfpRegister frb,
                     RCBit rc) {
  emit(EXT4 | FNEG | frt.code()*B21 | frb.code()*B11 | rc);
}

void Assembler::mtfsfi(int bf, int immediate, RCBit rc) {
  emit(EXT4 | MTFSFI | bf*B23 | immediate*B12 | rc);
}

void Assembler::mffs(const DwVfpRegister frt, RCBit rc) {
  emit(EXT4 | MFFS | frt.code()*B21 | rc);
}

void Assembler::mtfsf(const DwVfpRegister frb, bool L,
                      int FLM, bool W, RCBit rc) {
  emit(EXT4 | MTFSF | frb.code()*B11 | W*B16 | FLM*B17 | L*B25 | rc);
}

void Assembler::fsqrt(const DwVfpRegister frt,
                      const DwVfpRegister frb,
                      RCBit rc) {
  emit(EXT4 | FSQRT | frt.code()*B21 | frb.code()*B11 | rc);
}

void Assembler::fabs(const DwVfpRegister frt,
                     const DwVfpRegister frb,
                     RCBit rc) {
  emit(EXT4 | FABS | frt.code()*B21 | frb.code()*B11 | rc);
}

// Pseudo instructions.
void Assembler::nop(int type) {
  switch (type) {
    case 0:
      ori(r0, r0, Operand::Zero());
      break;
    case DEBUG_BREAK_NOP:
      ori(r3, r3, Operand::Zero());
      break;
    default:
      UNIMPLEMENTED();
  }
}

// I format <insn> i
//    +--------+---------+
//    | OpCode |    i    |
//    +--------+---------+
//    0        8        15
//
#define I_FORM_EMIT(name, op)\
void Assembler::name(const Operand& i) {\
    i_form(op, i);\
}
void Assembler::i_form(Opcode op, const Operand& i) {
    ASSERT(is_uint8(i.imm_));
    ASSERT(is_uint8(op));
    emit2bytes(op << 8 | i.imm_);
}

// E format <insn>
//    +------------------+
//    |      OpCode      |
//    +------------------+
//    0                 15
//
#define E_FORM_EMIT(name, op)\
void Assembler::name() {\
    e_form(op);\
}
void Assembler::e_form(Opcode op) {
    ASSERT(is_uint16(op));
    emit2bytes(op);
}

// IE format: <insn> i1, i2
//    +--------+---------+--------+----+----+
//    |      OpCode      |////////| I1 | I2 |
//    +--------+---------+--------+----+----+
//    0        8         16      24   28   31
#define IE_FORM_EMIT(name, op)\
void Assembler::name(const Operand& i1, const Operand& i2) {\
    ie_form(op, i1, i2);\
}
void Assembler::ie_form(Opcode op, const Operand& i1, const Operand& i2) {
    ASSERT(is_uint16(op));
    ASSERT(is_uint4(i1.imm_));
    ASSERT(is_uint4(i2.imm_));
    emit4bytes((op << 16) |
               ((i1.imm_ & 0xf) * B4) |
               (i2.imm_ & 0xf));
}

// RR format: <insn> R1,R2
//    +--------+----+----+
//    | OpCode | R1 | R2 |
//    +--------+----+----+
//    0        8    12  15
#define RR_FORM_EMIT(name, op) \
void Assembler::name(Register r1, Register r2) { \
    rr_form(op, r1, r2); \
}
void Assembler::rr_form(Opcode op, Register r1, Register r2) {
    ASSERT(is_uint8(op));
    ASSERT(is_uint4(r1.code()));
    ASSERT(is_uint4(r2.code()));
    emit2bytes(op*B8 | r1.code()*B4 | r2.code());
}

// RR2 format: <insn> M1,R2
//    +--------+----+----+
//    | OpCode | M1 | R2 |
//    +--------+----+----+
//    0        8    12  15
#define RR2_FORM_EMIT(name, op) \
void Assembler::name(Condition m1, Register r2) { \
    rr_form(op, m1, r2); \
}
void Assembler::rr_form(Opcode op, Condition m1, Register r2) {
    ASSERT(is_uint8(op));
    ASSERT(is_uint4(m1));
    ASSERT(is_uint4(r2.code()));
    emit2bytes(op*B8 | m1*B4 | r2.code());
}


// RX format: <insn> R1,D2(X2,B2)
//    +--------+----+----+----+-------------+
//    | OpCode | R1 | X2 | B2 |     D2      |
//    +--------+----+----+----+-------------+
//    0        8    12   16   20           31
#define RX_FORM_EMIT(name, op) \
void Assembler::name(Register r, const MemOperand& opnd) { \
    name(r, opnd.getIndexRegister(), opnd.getBaseRegister(), \
         opnd.getDisplacement());\
}\
void Assembler::name(Register r1, Register x2, \
                     Register b2, Disp d2) {\
    rx_form(op, r1, x2, b2, d2);\
}
void Assembler::rx_form(Opcode op,
                        Register r1,
                        Register x2,
                        Register b2,
                        Disp d2) {
    ASSERT(is_uint8(op));
    ASSERT(is_uint12(d2));
    emit4bytes(op*B24 | r1.code()*B20 |
             x2.code()*B16 | b2.code()*B12 | d2);
}

// RI1 format: <insn> R1,I2
//    +--------+----+----+------------------+
//    | OpCode | R1 |OpCd|        I2        |
//    +--------+----+----+------------------+
//    0        8    12   16                31
#define RI1_FORM_EMIT(name, op) \
void Assembler::name(Register r, const Operand& i2) { \
    ri_form(op, r, i2);\
}
void Assembler::ri_form(Opcode op, Register r1, const Operand& i2) {
    ASSERT(is_uint12(op));
    ASSERT(is_int16(i2.imm_));
    emit4bytes((op & 0xFF0) * B20 |
             r1.code() * B20 |
             (op & 0xF) * B16 |
             (i2.imm_ & 0xFFFF));
}

// RI2 format: <insn> M1,I2
//    +--------+----+----+------------------+
//    | OpCode | M1 |OpCd|        I2        |
//    +--------+----+----+------------------+
//    0        8    12   16                31
#define RI2_FORM_EMIT(name, op) \
void Assembler::name(Condition m, const Operand& i2) {\
    ri_form(op, m, i2);\
}
void Assembler::ri_form(Opcode op, Condition m1, const Operand& i2) {
    ASSERT(is_uint12(op));
    ASSERT(is_uint4(m1));
    ASSERT(is_int16(i2.imm_));
    emit4bytes((op & 0xFF0) * B20 |
             m1 * B20 |
             (op & 0xF) * B16 |
             (i2.imm_ & 0xFFFF));
}

// RIE format: <insn> R1,R3,I2
//    +--------+----+----+------------------+--------+--------+
//    | OpCode | R1 | R3 |        I2        |////////| OpCode |
//    +--------+----+----+------------------+--------+--------+
//    0        8    12   16                 32       40      47
#define RIE_FORM_EMIT(name, op) \
void Assembler::name(Register r1, Register r3, \
                     const Operand& i2) {\
    rie_form(op, r1, r3, i2);\
}
void Assembler::rie_form(Opcode op, Register r1, Register r3,
                     const Operand& i2) {
    ASSERT(is_uint16(op));
    ASSERT(is_uint16(i2.imm_));
    uint64_t code = (static_cast<uint64_t>(op & 0xFF00)) * B32       |
                    (static_cast<uint64_t>(r1.code())) * B36         |
                    (static_cast<uint64_t>(r3.code())) * B32         |
                    (static_cast<uint64_t>(i2.imm_)) * B16 |
                    (static_cast<uint64_t>(op & 0x00FF));
    emit6bytes(code);
}

// RIL1 format: <insn> R1,I2
//   +--------+----+----+------------------------------------+
//   | OpCode | R1 |OpCd|                  I2                |
//   +--------+----+----+------------------------------------+
//   0        8    12   16                                  47
#define RIL1_FORM_EMIT(name, op) \
void Assembler::name(Register r, const Operand& i2) {\
    ril_form(op, r, i2);\
}
void Assembler::ril_form(Opcode op, Register r1, const Operand& i2) {
    ASSERT(is_uint12(op));
    ASSERT(is_uint4(r1.code()));
    uint64_t code = (static_cast<uint64_t>(op & 0xFF0)) * B36        |
                    (static_cast<uint64_t>(r1.code())) * B36         |
                    (static_cast<uint64_t>(op & 0x00F)) * B32        |
                    (static_cast<uint64_t>(i2.imm_));
    emit6bytes(code);
}

// RIL2 format: <insn> M1,I2
//   +--------+----+----+------------------------------------+
//   | OpCode | M1 |OpCd|                  I2                |
//   +--------+----+----+------------------------------------+
//   0        8    12   16                                  47
#define RIL2_FORM_EMIT(name, op) \
void Assembler::name(Condition m1, const Operand& i2) {\
    ril_form(op, m1, i2);\
}
void Assembler::ril_form(Opcode op, Condition m1, const Operand& i2) {
    ASSERT(is_uint12(op));
    ASSERT(is_uint4(m1));
    uint64_t code = (static_cast<uint64_t>(op & 0xFF0)) * B36        |
                    (static_cast<uint64_t>(m1)) * B36        |
                    (static_cast<uint64_t>(op & 0x00F)) * B32        |
                    (static_cast<uint64_t>(i2.imm_));
    emit6bytes(code);
}

// RRE format: <insn> R1,R2
//    +------------------+--------+----+----+
//    |      OpCode      |////////| R1 | R2 |
//    +------------------+--------+----+----+
//    0                  16       24   28  31
#define RRE_FORM_EMIT(name, op) \
void Assembler::name(Register r1, Register r2) {\
    rre_form(op, r1, r2);\
}
void Assembler::rre_form(Opcode op, Register r1, Register r2) {
    ASSERT(is_uint16(op));
    ASSERT(is_uint4(r1.code()));
    ASSERT(is_uint4(r2.code()));
    emit4bytes(op << 16 | r1.code()*B4 | r2.code());
}

// RRD format: <insn> R1,R3, R2
//    +------------------+----+----+----+----+
//    |      OpCode      | R1 |////| R3 | R2 |
//    +------------------+----+----+----+----+
//    0                  16  20   24   28   31
#define RRD_FORM_EMIT(name, op) \
void Assembler::name(Register r1, Register r3, \
                     Register r2) {\
    rrd_form(op, r1, r3, r2);\
}
void Assembler::rrd_form(Opcode op, Register r1, Register r3, \
                     Register r2) {
    emit4bytes(op << 16 | r1.code()*B12 | r3.code()*B4\
            | r2.code());
}

// RS1 format: <insn> R1,R3,D2(B2)
//    +--------+----+----+----+-------------+
//    | OpCode | R1 | R3 | B2 |     D2      |
//    +--------+----+----+----+-------------+
//    0        8    12   16   20           31
#define RS1_FORM_EMIT(name, op) \
void Assembler::name(Register r1, Register r3, \
                     Register b2, Disp d2) {\
    rs_form(op, r1, r3, b2, d2);\
}\
void Assembler::name(Register r1, Register r3, \
                     const MemOperand& opnd) {\
    name(r1, r3, opnd.getBaseRegister(), opnd.getDisplacement());\
}
void Assembler::rs_form(Opcode op,
                        Register r1,
                        Register r3,
                        Register b2,
                        const Disp d2) {
  ASSERT(is_uint12(d2));
  emit4bytes(op * B24 | r1.code() * B20 | r3.code() * B16 |
             b2.code() * B12 | d2);
}

// RS2 format: <insn> R1,M3,D2(B2)
//    +--------+----+----+----+-------------+
//    | OpCode | R1 | M3 | B2 |     D2      |
//    +--------+----+----+----+-------------+
//    0        8    12   16   20           31
#define RS2_FORM_EMIT(name, op) \
void Assembler::name(Register r1, Condition m3, \
                     Register b2, Disp d2) {\
    rs_form(op, r1, m3, b2, d2);\
}\
void Assembler::name(Register r1, Condition m3, \
                     const MemOperand& opnd) {\
    name(r1, m3, opnd.getBaseRegister(), opnd.getDisplacement());\
}
void Assembler::rs_form(Opcode op,
                        Register r1,
                        Condition m3,
                        Register b2,
                        const Disp d2) {
  ASSERT(is_uint12(d2));
  emit4bytes(op * B24 | r1.code() * B20 | m3 * B16 |
             b2.code() * B12 | d2);
}

// RSI format: <insn> R1,R3,I2
//    +--------+----+----+------------------------------------+
//    | OpCode | R1 | R3 |                  I2                |
//    +--------+----+----+------------------------------------+
//    0        8    12   16                                  47
#define RSI_FORM_EMIT(name, op)\
void Assembler::name(Register r1, Register r3, const Operand& i2) {\
    rsi_form(op, r1, r3, i2);\
}
void Assembler::rsi_form(Opcode op, Register r1,
                           Register r3, const Operand& i2) {
    ASSERT(is_uint8(op));
    uint64_t code = (static_cast<uint64_t>(op)) * B40                |
                    (static_cast<uint64_t>(r1.code() )) * B36        |
                    (static_cast<uint64_t>(r3.code() )) * B32        |
                    (static_cast<uint64_t>(i2.imm_));
    emit6bytes(code);
}

// RSL format: <insn> R1,R3,D2(B2)
//    +--------+----+----+----+-------------+--------+--------+
//    | OpCode | L1 |    | B2 |    D2       |        | OpCode |
//    +--------+----+----+----+-------------+--------+--------+
//    0        8    12   16   20            32       40      47
#define RSL_FORM_EMIT(name, op)\
void Assembler::name(Length l1, Register b2, Disp d2) {\
    rsl_form(op, l1, b2, d2);\
}
void Assembler::rsl_form(Opcode op, Length l1, Register b2, Disp d2) {
    ASSERT(is_uint8(op));
    uint64_t code = (static_cast<uint64_t>(op && 0xFF00)) * B32  |
                    (static_cast<uint64_t>(l1)) * B36            |
                    (static_cast<uint64_t>(b2.code())) * B28     |
                    (static_cast<uint64_t>(d2)) * B16            |
                    (static_cast<uint64_t>(op && 0x00FF));
    emit6bytes(code);
}

// RSY1 format: <insn> R1,R3,D2(B2)
//    +--------+----+----+----+-------------+--------+--------+
//    | OpCode | R1 | R3 | B2 |    DL2      |  DH2   | OpCode |
//    +--------+----+----+----+-------------+--------+--------+
//    0        8    12   16   20            32       40      47
#define RSY1_FORM_EMIT(name, op)\
void Assembler::name(Register r1, Register r3, Register b2, \
                     Disp d2) {\
    rsy_form(op, r1, r3, b2, d2);\
}\
void Assembler::name(Register r1, Register r3, const MemOperand& opnd) {\
    name(r1, r3, opnd.getBaseRegister(), opnd.getDisplacement());\
}
void Assembler::rsy_form(Opcode op,
                        Register r1,
                        Register r3,
                        Register b2,
                        const Disp d2) {
    ASSERT(is_int20(d2));
    ASSERT(is_uint16(op));
    uint64_t code = (static_cast<uint64_t>(op && 0xFF00)) * B32  |
                    (static_cast<uint64_t>(r1.code())) * B36     |
                    (static_cast<uint64_t>(r3.code())) * B32     |
                    (static_cast<uint64_t>(b2.code())) * B28     |
                    (static_cast<uint64_t>(d2 & 0x0FFF)) * B16   |
                    (static_cast<uint64_t>(d2 & 0x0FF000)) >> 4  |
                    (static_cast<uint64_t>(op && 0x00FF));
    emit6bytes(code);
}


// RSY2 format: <insn> R1,M3,D2(B2)
//    +--------+----+----+----+-------------+--------+--------+
//    | OpCode | R1 | M3 | B2 |    DL2      |  DH2   | OpCode |
//    +--------+----+----+----+-------------+--------+--------+
//    0        8    12   16   20            32       40      47
#define RSY2_FORM_EMIT(name, op)\
void Assembler::name(Register r1, Condition m3, Register b2, \
                     Disp d2) {\
    rsy_form(op, r1, m3, b2, d2);\
}\
void Assembler::name(Register r1, Condition m3, const MemOperand& opnd) {\
    name(r1, m3, opnd.getBaseRegister(), opnd.getDisplacement());\
}
void Assembler::rsy_form(Opcode op,
                        Register r1,
                        Condition m3,
                        Register b2,
                        const Disp d2) {
    ASSERT(is_int20(d2));
    ASSERT(is_uint16(op));
    uint64_t code = (static_cast<uint64_t>(op && 0xFF00)) * B32  |
                    (static_cast<uint64_t>(r1.code())) * B36     |
                    (static_cast<uint64_t>(m3)) * B32    |
                    (static_cast<uint64_t>(b2.code())) * B28     |
                    (static_cast<uint64_t>(d2 & 0x0FFF)) * B16   |
                    (static_cast<uint64_t>(d2 & 0x0FF000)) >> 4  |
                    (static_cast<uint64_t>(op && 0x00FF));
    emit6bytes(code);
}


// RXE format: <insn> R1,D2(X2,B2)
//    +--------+----+----+----+-------------+--------+--------+
//    | OpCode | R1 | X2 | B2 |     D2      |////////| OpCode |
//    +--------+----+----+----+-------------+--------+--------+
//    0        8    12   16   20            32       40      47
#define RXE_FORM_EMIT(name, op)\
void Assembler::name(Register r1, Register x2, Register b2, \
                     Disp d2) {\
    rxe_form(op, r1, x2, b2, d2);\
}\
void Assembler::name(Register r1, const MemOperand& opnd) {\
    name(r1, opnd.getIndexRegister(), opnd.getBaseRegister(), \
         opnd.getDisplacement());\
}
void Assembler::rxe_form(Opcode op, Register r1, Register x2, Register b2,
                     Disp d2) {
    ASSERT(is_uint12(d2));
    ASSERT(is_uint16(op));
    uint64_t code = (static_cast<uint64_t>(op && 0xFF00)) * B32  |
                    (static_cast<uint64_t>(r1.code())) * B36     |
                    (static_cast<uint64_t>(x2.code())) * B32     |
                    (static_cast<uint64_t>(b2.code())) * B28     |
                    (static_cast<uint64_t>(d2 & 0x0FFF)) * B16   |
                    (static_cast<uint64_t>(op && 0x00FF));
    emit6bytes(code);
}

// RXY format: <insn> R1,D2(X2,B2)
//    +--------+----+----+----+-------------+--------+--------+
//    | OpCode | R1 | X2 | B2 |     DL2     |   DH2  | OpCode |
//    +--------+----+----+----+-------------+--------+--------+
//    0        8    12   16   20            32   36   40      47
#define RXY_FORM_EMIT(name, op)\
void Assembler::name(Register r1, Register x2, Register b2, \
                     Disp d2) {\
    rxy_form(op, r1, x2, b2, d2);\
}\
void Assembler::name(Register r1, const MemOperand& opnd) {\
    name(r1, opnd.getIndexRegister(), opnd.getBaseRegister(), \
         opnd.getDisplacement());\
}
void Assembler::rxy_form(Opcode op, Register r1, Register x2, Register b2,
                     Disp d2) {
    ASSERT(is_int20(d2));
    ASSERT(is_uint16(op));
    uint64_t code = (static_cast<uint64_t>(op && 0xFF00)) * B32  |
                    (static_cast<uint64_t>(r1.code())) * B36     |
                    (static_cast<uint64_t>(x2.code())) * B32     |
                    (static_cast<uint64_t>(b2.code())) * B28     |
                    (static_cast<uint64_t>(d2 & 0x0FFF)) * B16   |
                    (static_cast<uint64_t>(d2 & 0x0FF000)) >> 4  |
                    (static_cast<uint64_t>(op && 0x00FF));
    emit6bytes(code);
}

// RRS format: <insn> R1,R2,M3,D4(B4)
//    +--------+----+----+----+-------------+----+---+--------+
//    | OpCode | R1 | R2 | B4 |     D4      | M3 |///| OpCode |
//    +--------+----+----+----+-------------+----+---+--------+
//    0        8    12   16   20            32   36   40      47
#define RRS_FORM_EMIT(name, op)\
void Assembler::name(Register r1, Register r2, Register b4, \
                     Disp d4, Condition m3) {\
    rrs_form(op, r1, r2, b4, d4, m3);\
}\
void Assembler::name(Register r1, Register r2, Condition m3, \
                     const MemOperand& opnd) {\
    name(r1, r2, opnd.getBaseRegister(), opnd.getDisplacement(), m3);\
}
void Assembler::rrs_form(Opcode op, Register r1, Register r2, Register b4,
                     Disp d4, Condition m3) {
    ASSERT(is_uint12(d4));
    ASSERT(is_uint16(op));
    uint64_t code = (static_cast<uint64_t>(op && 0xFF00)) * B32  |
                    (static_cast<uint64_t>(r1.code())) * B36     |
                    (static_cast<uint64_t>(r2.code())) * B32     |
                    (static_cast<uint64_t>(b4.code())) * B28     |
                    (static_cast<uint64_t>(d4)) * B16            |
                    (static_cast<uint64_t>(m3)) << 12    |
                    (static_cast<uint64_t>(op && 0x00FF));
    emit6bytes(code);
}

// RIS format: <insn> R1,I2,M3,D4(B4)
//    +--------+----+----+----+-------------+--------+--------+
//    | OpCode | R1 | M3 | B4 |     D4      |   I2   | OpCode |
//    +--------+----+----+----+-------------+--------+--------+
//    0        8    12   16   20            32        40      47
#define RIS_FORM_EMIT(name, op)\
void Assembler::name(Register r1, Condition m3, Register b4, \
                     Disp d4, const Operand& i2) {\
    ris_form(op, r1, m3, b4, d4, i2);\
}\
void Assembler::name(Register r1, const Operand& i2, Condition m3, \
                     const MemOperand& opnd) {\
    name(r1, m3, opnd.getBaseRegister(), opnd.getDisplacement(), i2);\
}
void Assembler::ris_form(Opcode op, Register r1, Condition m3, Register b4, \
                     Disp d4, const Operand& i2) {
    ASSERT(is_uint12(d4));
    ASSERT(is_uint16(op));
    ASSERT(is_uint8(i2.imm_));
    uint64_t code = (static_cast<uint64_t>(op && 0xFF00)) * B32  |
                    (static_cast<uint64_t>(r1.code())) * B36     |
                    (static_cast<uint64_t>(m3)) * B32    |
                    (static_cast<uint64_t>(b4.code())) * B28     |
                    (static_cast<uint64_t>(d4)) * B16            |
                    (static_cast<uint64_t>(i2.imm_)) << 8        |
                    (static_cast<uint64_t>(op && 0x00FF));
    emit6bytes(code);
}


// S format: <insn> D2(B2)
//    +------------------+----+-------------+
//    |      OpCode      | B2 |     D2      |
//    +------------------+----+-------------+
//    0                  16   20           31
#define S_FORM_EMIT(name, op)\
void Assembler::name(Register b1, Disp d2) {\
    s_form(op, b1, d2);\
}\
void Assembler::name(const MemOperand& opnd) {\
    name(opnd.getBaseRegister(), opnd.getDisplacement());\
}
void Assembler::s_form(Opcode op, Register b1, Disp d2) {
    ASSERT(is_uint12(d2));
    emit4bytes(op << 16 | b1.code()*B12 | d2);
}

// SI format: <insn> D1(B1),I2
//    +--------+---------+----+-------------+
//    | OpCode |   I2    | B1 |     D1      |
//    +--------+---------+----+-------------+
//    0        8         16   20           31
#define SI_FORM_EMIT(name, op)\
void Assembler::name(const Operand& i2, Register b1, \
                     Disp d1) {\
    si_form(op, i2, b1, d1);\
}\
void Assembler::name(const MemOperand& opnd, const Operand& i2) {\
    name(i2, opnd.getBaseRegister(), opnd.getDisplacement()); \
}
void Assembler::si_form(Opcode op, const Operand& i2, Register b1,
                     Disp d1) {
    emit4bytes((op & 0x00FF) << 24 | i2.imm_*B16 | b1.code()*B12 | d1);
}

// SIY format: <insn> D1(B1),I2
//    +--------+---------+----+-------------+--------+--------+
//    | OpCode |   I2    | B1 |     DL1     |  DH1   | OpCode |
//    +--------+---------+----+-------------+--------+--------+
//    0        8         16   20            32   36   40      47
#define SIY_FORM_EMIT(name, op)\
void Assembler::name(const Operand& i2, Register b1, \
                     Disp d1) {\
    siy_form(op, i2, b1, d1);\
}\
void Assembler::name(const MemOperand& opnd, const Operand& i2) {\
    name(i2, opnd.getBaseRegister(), opnd.getDisplacement());\
}
void Assembler::siy_form(Opcode op, const Operand& i2, Register b1, \
                     Disp d1) {
    ASSERT(is_uint12(d1));
    ASSERT(is_uint16(op));
    ASSERT(is_uint8(i2.imm_));
    uint64_t code = (static_cast<uint64_t>(op && 0xFF00)) * B32  |
                    (static_cast<uint64_t>(i2.imm_)) * B32       |
                    (static_cast<uint64_t>(b1.code())) * B28     |
                    (static_cast<uint64_t>(d1 & 0x0FFF)) * B16   |
                    (static_cast<uint64_t>(d1 & 0x0FF000)) >> 4  |
                    (static_cast<uint64_t>(op && 0x00FF));
    emit6bytes(code);
}

// SIL format: <insn> D1(B1),I2
//    +------------------+----+-------------+-----------------+
//    |     OpCode       | B1 |      D1     |        I2       |
//    +------------------+----+-------------+-----------------+
//    0                 16   20            32                47
#define SIL_FORM_EMIT(name, op)\
void Assembler::name(Register b1, Disp d1, \
                     const Operand& i2) {\
    sil_form(op, b1, d1, i2);\
}\
void Assembler::name(const MemOperand& opnd, const Operand& i2) {\
    name(opnd.getBaseRegister(), opnd.getDisplacement(), i2);\
}
void Assembler::sil_form(Opcode op, Register b1, Disp d1,
                     const Operand& i2) {
    ASSERT(is_uint12(d1));
    ASSERT(is_uint16(op));
    ASSERT(is_uint16(i2.imm_));
    uint64_t code = (static_cast<uint64_t>(op)) * B32            |
                    (static_cast<uint64_t>(b1.code())) * B28     |
                    (static_cast<uint64_t>(d1)) * B16            |
                    (static_cast<uint64_t>(i2.imm_));
    emit6bytes(code);
}

// RXF format: <insn> R1,R3,D2(X2,B2)
//    +--------+----+----+----+-------------+----+---+--------+
//    | OpCode | R3 | X2 | B2 |     D2      | R1 |///| OpCode |
//    +--------+----+----+----+-------------+----+---+--------+
//    0        8    12   16   20            32   36  40      47
#define RXF_FORM_EMIT(name, op)\
void Assembler::name(Register r1, Register r3, Register b2, \
                     Register x2, Disp d2) {\
    rxf_form(op, r1, r3, b2, x2, d2);\
}\
void Assembler::name(Register r1, Register r3, const MemOperand& opnd) {\
    name(r1, r3, opnd.getBaseRegister(), opnd.getIndexRegister(), \
         opnd.getDisplacement());\
}
void Assembler::rxf_form(Opcode op, Register r1, Register r3, Register b2, \
                     Register x2, Disp d2) {
    ASSERT(is_uint12(d2));
    ASSERT(is_uint16(op));
    uint64_t code = (static_cast<uint64_t>(op & 0xFF00)) * B32   |
                    (static_cast<uint64_t>(r3.code())) * B36     |
                    (static_cast<uint64_t>(x2.code())) * B32     |
                    (static_cast<uint64_t>(b2.code())) * B28     |
                    (static_cast<uint64_t>(d2))        * B16     |
                    (static_cast<uint64_t>(r1.code())) * B12     |
                    (static_cast<uint64_t>(op && 0x00FF));
    emit6bytes(code);
}

// SS1 format: <insn> D1(L,B1),D2(B3)
//    +--------+----+----+----+-------------+----+------------+
//    | OpCode |    L    | B1 |     D1      | B2 |     D2     |
//    +--------+----+----+----+-------------+----+------------+
//    0        8    12   16   20            32   36          47
#define SS1_FORM_EMIT(name, op)\
void Assembler::name(Length l, Register b1, Disp d1, \
                     Register b2, Disp d2) {\
    ss_form(op, l, b1, d1, b2, d2);\
}\
void Assembler::name(const MemOperand& opnd1, const MemOperand& opnd2) {\
    name(opnd1.getLength(), opnd1.getBaseRegister(), \
         opnd1.getDisplacement(), opnd2.getBaseRegister(), \
         opnd2.getDisplacement());\
}
void Assembler::ss_form(Opcode op, Length l, Register b1, Disp d1,
                     Register b2, Disp d2) {
    ASSERT(is_uint12(d2));
    ASSERT(is_uint12(d1));
    ASSERT(is_uint8(op));
    ASSERT(is_uint8(l));
    uint64_t code = (static_cast<uint64_t>(op)) * B40            |
                    (static_cast<uint64_t>(l)) * B32             |
                    (static_cast<uint64_t>(b1.code())) * B28     |
                    (static_cast<uint64_t>(d1))        * B16     |
                    (static_cast<uint64_t>(b2.code())) * B12     |
                    (static_cast<uint64_t>(d2));
    emit6bytes(code);
}

// SS2 format: <insn> D1(L1,B1), D2(L3,B3)
//    +--------+----+----+----+-------------+----+------------+
//    | OpCode | L1 | L2 | B1 |     D1      | B2 |     D2     |
//    +--------+----+----+----+-------------+----+------------+
//    0        8    12   16   20            32   36          47
#define SS2_FORM_EMIT(name, op)\
void Assembler::name(Length l1, Length l2, Register b1, \
                     Disp d1, Register b2, \
                     Disp d2) {\
    ss_form(op, l1, l2, b1, d1, b2, d2);\
}\
void Assembler::name(const MemOperand& opnd1, const MemOperand& opnd2) {\
    name(opnd1.getLength(), opnd2.getLength(), opnd1.getBaseRegister(), \
         opnd1.getDisplacement(), opnd2.getBaseRegister(), \
         opnd2.getDisplacement());\
}
void Assembler::ss_form(Opcode op, Length l1, Length l2, Register b1,
                     Disp d1, Register b2, Disp d2) {
    ASSERT(is_uint12(d2));
    ASSERT(is_uint12(d1));
    ASSERT(is_uint8(op));
    ASSERT(is_uint4(l2));
    ASSERT(is_uint4(l1));
    uint64_t code = (static_cast<uint64_t>(op)) * B40            |
                    (static_cast<uint64_t>(l1)) * B36            |
                    (static_cast<uint64_t>(l2)) * B32            |
                    (static_cast<uint64_t>(b1.code())) * B28     |
                    (static_cast<uint64_t>(d1))        * B16     |
                    (static_cast<uint64_t>(b2.code())) * B12     |
                    (static_cast<uint64_t>(d2));
    emit6bytes(code);
}

// SS3 format: <insn> D1(L1,B1), D2(I3,B2)
//    +--------+----+----+----+-------------+----+------------+
//    | OpCode | L1 | I3 | B1 |     D1      | B2 |     D2     |
//    +--------+----+----+----+-------------+----+------------+
//    0        8    12   16   20            32   36          47
#define SS3_FORM_EMIT(name, op)\
void Assembler::name(Length l1, const Operand& i3, Register b1, \
                     Disp d1, Register b2, \
                     Disp d2) {\
    ss_form(op, l1, i3, b1, d1, b2, d2);\
}\
void Assembler::name(const MemOperand& opnd1, const MemOperand& opnd2) {\
    ASSERT(false);\
}
void Assembler::ss_form(Opcode op, Length l1, const Operand& i3, Register b1,
                     Disp d1, Register b2, Disp d2) {
    ASSERT(is_uint12(d2));
    ASSERT(is_uint12(d1));
    ASSERT(is_uint8(op));
    ASSERT(is_uint4(l1));
    ASSERT(is_uint4(i3.imm_));
    uint64_t code = (static_cast<uint64_t>(op)) * B40            |
                    (static_cast<uint64_t>(l1)) * B36            |
                    (static_cast<uint64_t>(i3.imm_)) * B32       |
                    (static_cast<uint64_t>(b1.code())) * B28     |
                    (static_cast<uint64_t>(d1))        * B16     |
                    (static_cast<uint64_t>(b2.code())) * B12     |
                    (static_cast<uint64_t>(d2));
    emit6bytes(code);
}

// SS4 format: <insn> D1(R1,B1), D2(R3,B2)
//    +--------+----+----+----+-------------+----+------------+
//    | OpCode | R1 | R3 | B1 |     D1      | B2 |     D2     |
//    +--------+----+----+----+-------------+----+------------+
//    0        8    12   16   20            32   36          47
#define SS4_FORM_EMIT(name, op)\
void Assembler::name(Register r1, Register r3, Register b1, \
                     Disp d1, Register b2, \
                     Disp d2) {\
    ss_form(op, r1, r3, b1, d1, b2, d2);\
}\
void Assembler::name(const MemOperand& opnd1, const MemOperand& opnd2) {\
    ASSERT(false);\
}
void Assembler::ss_form(Opcode op, Register r1, Register r3, Register b1,
                     Disp d1, Register b2, Disp d2) {
    ASSERT(is_uint12(d2));
    ASSERT(is_uint12(d1));
    ASSERT(is_uint8(op));
    uint64_t code = (static_cast<uint64_t>(op)) * B40            |
                    (static_cast<uint64_t>(r1.code())) * B36     |
                    (static_cast<uint64_t>(r3.code())) * B32     |
                    (static_cast<uint64_t>(b1.code())) * B28     |
                    (static_cast<uint64_t>(d1))        * B16     |
                    (static_cast<uint64_t>(b2.code())) * B12     |
                    (static_cast<uint64_t>(d2));
    emit6bytes(code);
}

// SS5 format: <insn> D1(R1,B1), D2(R3,B2)
//    +--------+----+----+----+-------------+----+------------+
//    | OpCode | R1 | R3 | B2 |     D2      | B4 |     D4     |
//    +--------+----+----+----+-------------+----+------------+
//    0        8    12   16   20            32   36          47
#define SS5_FORM_EMIT(name, op)\
void Assembler::name(Register r1, Register r3, Register b2, \
                     Disp d2, Register b4, \
                     Disp d4) {\
    ss_form(op, r1, r3, b2, d2, b4, d4); /*SS5 use the same form as SS4*/ \
}\
void Assembler::name(const MemOperand& opnd1, const MemOperand& opnd2) {\
    ASSERT(false);\
}

#define SS6_FORM_EMIT(name, op) SS1_FORM_EMIT(name, op)

// SSE format: <insn> D1(B1),D2(B2)
//    +------------------+----+-------------+----+------------+
//    |      OpCode      | B1 |     D1      | B2 |     D2     |
//    +------------------+----+-------------+----+------------+
//    0        8    12   16   20            32   36           47
#define SSE_FORM_EMIT(name, op)\
void Assembler::name(Register b1, Disp d1, Register b2, \
                     Disp d2) {\
    sse_form(op, b1, d1, b2, d2);\
}\
void Assembler::name(const MemOperand& opnd1, const MemOperand& opnd2) {\
    name(op, opnd1.getBaseRegister(), opnd1.getDisplacement(), \
         opnd2.getBaseRegister(), opnd2.getDisplacement());\
}
void Assembler::sse_form(Opcode op, Register b1, Disp d1, Register b2,
                     Disp d2) {
    ASSERT(is_uint12(d2));
    ASSERT(is_uint12(d1));
    ASSERT(is_uint16(op));
    uint64_t code = (static_cast<uint64_t>(op)) * B32            |
                    (static_cast<uint64_t>(b1.code())) * B28     |
                    (static_cast<uint64_t>(d1))        * B16     |
                    (static_cast<uint64_t>(b2.code())) * B12     |
                    (static_cast<uint64_t>(d2));
    emit6bytes(code);
}

// SSF format: <insn> R3, D1(B1),D2(B2),R3
//    +--------+----+----+----+-------------+----+------------+
//    | OpCode | R3 |OpCd| B1 |     D1      | B2 |     D2     |
//    +--------+----+----+----+-------------+----+------------+
//    0        8    12   16   20            32   36           47
#define SSF_FORM_EMIT(name, op)\
void Assembler::name(Register r3, Register b1, Disp d1, \
                     Register b2, Disp d2) {\
    ssf_form(op, r3, b1, d1, b2, d2);\
}\
void Assembler::name(Register r3, const MemOperand& opnd1, \
                     const MemOperand& opnd2) {\
    name(r3, opnd1.getBaseRegister(), opnd1.getDisplacement(), \
         opnd2.getBaseRegister(), opnd2.getDisplacement());\
}
void Assembler::ssf_form(Opcode op, Register r3, Register b1, Disp d1,
                     Register b2, Disp d2) {
    ASSERT(is_uint12(d2));
    ASSERT(is_uint12(d1));
    ASSERT(is_uint12(op));
    uint64_t code = (static_cast<uint64_t>(op & 0xFF0)) * B36    |
                    (static_cast<uint64_t>(r3.code())) * B36     |
                    (static_cast<uint64_t>(op & 0x00F)) * B32    |
                    (static_cast<uint64_t>(b1.code())) * B28     |
                    (static_cast<uint64_t>(d1))        * B16     |
                    (static_cast<uint64_t>(b2.code())) * B12     |
                    (static_cast<uint64_t>(d2));
    emit6bytes(code);
}

//  RRF1 format: <insn> R1,R2,R3
//    +------------------+----+----+----+----+
//    |      OpCode      | R1 |    | R3 | R2 |
//    +------------------+----+----+----+----+
//    0                  16   20   24   28  31
#define RRF1_FORM_EMIT(name, op)\
void Assembler::name(Register r1, Register r3, Register r2) {\
    rrf1_form(op << 16 | r1.code()*B12 | r3.code()*B4 | r2.code());\
}
void Assembler::rrf1_form(uint32_t code) {
    emit4bytes(code);
}

//  RRF2 format: <insn> R1,R2,M3
//    +------------------+----+----+----+----+
//    |      OpCode      | M3 |    | R1 | R2 |
//    +------------------+----+----+----+----+
//    0                  16   20   24   28  31
#define RRF2_FORM_EMIT(name, op)\
void Assembler::name(Condition m3, Register r1, Register r2) {\
    rrf2_form(op << 16 |m3*B12 | r1.code()*B4 | r2.code());\
}
void Assembler::rrf2_form(uint32_t code) {
    emit4bytes(code);
}

//  RRF3 format: <insn> R1,R2,R3,M4
//    +------------------+----+----+----+----+
//    |      OpCode      | R3 | M4 | R1 | R2 |
//    +------------------+----+----+----+----+
//    0                  16   20   24   28  31
#define RRF3_FORM_EMIT(name, op)\
void Assembler::name(Register r3, Conition m4, Register r1, \
                     Register r2) {\
    rrf3_form(op << 16 | r3.code()*B12 | m4*B8 | \
              r1.code()*B4 | r2.code());\
}
void Assembler::rrf3_form(uint32_t code) {
    emit4bytes(code);
}

// end of S390 Instruction generation

// start of S390 instruction
RX_FORM_EMIT(a, A)
RXE_FORM_EMIT(adb, ADB)
RRE_FORM_EMIT(adbr, ADBR)
RRF1_FORM_EMIT(adtr, ADTR)
RRF1_FORM_EMIT(adtra, ADTRA)
RXE_FORM_EMIT(aeb, AEB)
RRE_FORM_EMIT(aebr, AEBR)
RIL1_FORM_EMIT(afi, AFI)
RXY_FORM_EMIT(ag, AG)
RXY_FORM_EMIT(agf, AGF)
RIL1_FORM_EMIT(agfi, AGFI)
RRE_FORM_EMIT(agfr, AGFR)
RI1_FORM_EMIT(aghi, AGHI)
RIE_FORM_EMIT(aghik, AGHIK)
RRE_FORM_EMIT(agr, AGR)
RRF1_FORM_EMIT(agrk, AGRK)
SIY_FORM_EMIT(agsi, AGSI)
RX_FORM_EMIT(ah, AH)
RRF1_FORM_EMIT(ahhhr, AHHHR)
RRF1_FORM_EMIT(ahhlr, AHHLR)
RI1_FORM_EMIT(ahi, AHI)
RIE_FORM_EMIT(ahik, AHIK)
RXY_FORM_EMIT(ahy, AHY)
RIL1_FORM_EMIT(aih, AIH)
RX_FORM_EMIT(al_z, AL)
RXY_FORM_EMIT(alc, ALC)
RXY_FORM_EMIT(alcg, ALCG)
RRE_FORM_EMIT(alcgr, ALCGR)
RRE_FORM_EMIT(alcr, ALCR)
RIL1_FORM_EMIT(alfi, ALFI)
RXY_FORM_EMIT(alg, ALG)
RXY_FORM_EMIT(algf, ALGF)
RIL1_FORM_EMIT(algfi, ALGFI)
RRE_FORM_EMIT(algfr, ALGFR)
RIE_FORM_EMIT(alghsik, ALGHSIK)
RRE_FORM_EMIT(algr, ALGR)
RRF1_FORM_EMIT(algrk, ALGRK)
SIY_FORM_EMIT(algsi, ALGSI)
RRF1_FORM_EMIT(alhhhr, ALHHHR)
RRF1_FORM_EMIT(alhhlr, ALHHLR)
RIE_FORM_EMIT(alhsik, ALHSIK)
RR_FORM_EMIT(alr, ALR)
RRF1_FORM_EMIT(alrk, ALRK)
SIY_FORM_EMIT(alsi, ALSI)
RIL1_FORM_EMIT(alsih, ALSIH)
RIL1_FORM_EMIT(alsihn, ALSIHN)
RXY_FORM_EMIT(aly, ALY)
SS2_FORM_EMIT(ap, AP)
RR_FORM_EMIT(ar, AR)
RRF1_FORM_EMIT(ark, ARK)
SIY_FORM_EMIT(asi, ASI)
RRE_FORM_EMIT(axbr, AXBR)
RRF1_FORM_EMIT(axtr, AXTR)
RRF1_FORM_EMIT(axtra, AXTRA)
RXY_FORM_EMIT(ay, AY)
RX_FORM_EMIT(bal, BAL)
RR_FORM_EMIT(balr, BALR)
RX_FORM_EMIT(bas, BAS)
RR_FORM_EMIT(basr, BASR)
RR_FORM_EMIT(bassm, BASSM)
RX_FORM_EMIT(bc, BC)
RX_FORM_EMIT(bct, BCT)
RXY_FORM_EMIT(bctg, BCTG)
RRE_FORM_EMIT(bctgr, BCTGR)
RR_FORM_EMIT(bctr, BCTR)
RI2_FORM_EMIT(bras, BRAS)
RIL1_FORM_EMIT(brasl, BRASL)
RI2_FORM_EMIT(brc, BRC)
RIL2_FORM_EMIT(brcl, BRCL)
RI2_FORM_EMIT(brct, BRCT)
RI2_FORM_EMIT(brctg, BRCTG)
RIL1_FORM_EMIT(brcth, BRCTH)
RSI_FORM_EMIT(brxh, BRXH)
RIE_FORM_EMIT(brxhg, BRXHG)
RSI_FORM_EMIT(brxle, BRXLE)
RIE_FORM_EMIT(brxlg, BRXLG)
RR_FORM_EMIT(bsm, BSM)
RS1_FORM_EMIT(bxle, BXLE)
RSY1_FORM_EMIT(bxleg, BXLEG)
RX_FORM_EMIT(c, C)
RXE_FORM_EMIT(cdb, CDB)
RRE_FORM_EMIT(cdbr, CDBR)
RRE_FORM_EMIT(cdfbr, CDFBR)
RRF2_FORM_EMIT(cdfbra, CDFBRA)
RRE_FORM_EMIT(cdftr, CDFTR)
RRE_FORM_EMIT(cdgbr, CDGBR)
RRF2_FORM_EMIT(cdgbra, CDGBRA)
RRE_FORM_EMIT(cdgtr, CDGTR)
RRF2_FORM_EMIT(cdgtra, CDGTRA)
RRF2_FORM_EMIT(cdlfbr, CDLFBR)
RRF2_FORM_EMIT(cdlftr, CDLFTR)
RRF2_FORM_EMIT(cdlgbr, CDLGBR)
RRF2_FORM_EMIT(cdlgtr, CDLGTR)
RS1_FORM_EMIT(cds, CDS)
RSY1_FORM_EMIT(cdsg, CDSG)
RRE_FORM_EMIT(cdstr, CDSTR)
RSY1_FORM_EMIT(cdsy, CDSY)
RRE_FORM_EMIT(cdtr, CDTR)
RRE_FORM_EMIT(cdutr, CDUTR)
RSL_FORM_EMIT(cdzt, CDZT)
RXE_FORM_EMIT(ceb, CEB)
RRE_FORM_EMIT(cebr, CEBR)
RRE_FORM_EMIT(cedtr, CEDTR)
RRE_FORM_EMIT(cefbr, CEFBR)
RRF2_FORM_EMIT(cefbra, CEFBRA)
RRE_FORM_EMIT(cegbr, CEGBR)
RRF2_FORM_EMIT(cegbra, CEGBRA)
RRF2_FORM_EMIT(celfbr, CELFBR)
RRF2_FORM_EMIT(celgbr, CELGBR)
RRE_FORM_EMIT(cextr, CEXTR)
S_FORM_EMIT(cfc, CFC)
RRF2_FORM_EMIT(cfdbr, CFDBR)
RRF2_FORM_EMIT(cfdbra, CFDBRA)
RRF2_FORM_EMIT(cfdr, CFDR)
RRF2_FORM_EMIT(cfdtr, CFDTR)
RRF2_FORM_EMIT(cfebr, CFEBR)
RRF2_FORM_EMIT(cfebra, CFEBRA)
RRF2_FORM_EMIT(cfer, CFER)
RIL1_FORM_EMIT(cfi, CFI)
RRF2_FORM_EMIT(cfxbr, CFXBR)
RRF2_FORM_EMIT(cfxbra, CFXBRA)
RRF2_FORM_EMIT(cfxr, CFXR)
RRF2_FORM_EMIT(cfxtr, CFXTR)
RXY_FORM_EMIT(cg, CG)
RRF2_FORM_EMIT(cgdbr, CGDBR)
RRF2_FORM_EMIT(cgdbra, CGDBRA)
RRF2_FORM_EMIT(cgdr, CGDR)
RRF2_FORM_EMIT(cgdtr, CGDTR)
RRF2_FORM_EMIT(cgdtra, CGDTRA)
RRF2_FORM_EMIT(cgebr, CGEBR)
RRF2_FORM_EMIT(cgebra, CGEBRA)
RRF2_FORM_EMIT(cger, CGER)
RXY_FORM_EMIT(cgf, CGF)
RIL1_FORM_EMIT(cgfi, CGFI)
RRE_FORM_EMIT(cgfr, CGFR)
RIL1_FORM_EMIT(cgfrl, CGFRL)
RXY_FORM_EMIT(cgh, CGH)
RI1_FORM_EMIT(cghi, CGHI)
RIL1_FORM_EMIT(cghrl, CGHRL)
SIL_FORM_EMIT(cghsi, CGHSI)
RIS_FORM_EMIT(cgib, CGIB)
RIE_FORM_EMIT(cgij, CGIJ)
RIE_FORM_EMIT(cgit, CGIT)
RRE_FORM_EMIT(cgr, CGR)
RRS_FORM_EMIT(cgrb, CGRB)
RIE_FORM_EMIT(cgrj, CGRJ)
RIL1_FORM_EMIT(cgrl, CGRL)
RRF2_FORM_EMIT(cgrt, CGRT)
RRF2_FORM_EMIT(cgxbr, CGXBR)
RRF2_FORM_EMIT(cgxbra, CGXBRA)
RRF2_FORM_EMIT(cgxr, CGXR)
RRF2_FORM_EMIT(cgxtr, CGXTR)
RRF2_FORM_EMIT(cgxtra, CGXTRA)
RX_FORM_EMIT(ch, CH)
RXY_FORM_EMIT(chf, CHF)
RRE_FORM_EMIT(chhr, CHHR)
SIL_FORM_EMIT(chhsi, CHHSI)
RI1_FORM_EMIT(chi, CHI)
RRE_FORM_EMIT(chlr, CHLR)
RIL1_FORM_EMIT(chrl, CHRL)
SIL_FORM_EMIT(chsi, CHSI)
RXY_FORM_EMIT(chy, CHY)
RIS_FORM_EMIT(cib, CIB)
RIL1_FORM_EMIT(cih, CIH)
RIE_FORM_EMIT(cij, CIJ)
RIE_FORM_EMIT(cit, CIT)
RRE_FORM_EMIT(cksm, CKSM)
RX_FORM_EMIT(cl, CL)
SS1_FORM_EMIT(clc, CLC)
RR_FORM_EMIT(clcl, CLCL)
RS1_FORM_EMIT(clcle, CLCLE)
RSY1_FORM_EMIT(clclu, CLCLU)
RRF2_FORM_EMIT(clfdbr, CLFDBR)
RRF2_FORM_EMIT(clfdtr, CLFDTR)
RRF2_FORM_EMIT(clfebr, CLFEBR)
SIL_FORM_EMIT(clfhsi, CLFHSI)
RIL1_FORM_EMIT(clfi, CLFI)
RIE_FORM_EMIT(clfit, CLFIT)
RRF2_FORM_EMIT(clfxbr, CLFXBR)
RRF2_FORM_EMIT(clfxtr, CLFXTR)
RXY_FORM_EMIT(clg, CLG)
RRF2_FORM_EMIT(clgdbr, CLGDBR)
RRF2_FORM_EMIT(clgdtr, CLGDTR)
RRF2_FORM_EMIT(clgebr, CLGEBR)
RXY_FORM_EMIT(clgf, CLGF)
RIL1_FORM_EMIT(clgfi, CLGFI)
RR_FORM_EMIT(clr, CLR)
SSF_FORM_EMIT(csst, CSST)
RRF2_FORM_EMIT(csxtr, CSXTR)
RSY1_FORM_EMIT(csy, CSY)
RRF2_FORM_EMIT(cu12, CU12)
RRF2_FORM_EMIT(cu14, CU14)
RRF2_FORM_EMIT(cu21, CU21)
RRF2_FORM_EMIT(cu24, CU24)
RRE_FORM_EMIT(cu41, CU41)
RRE_FORM_EMIT(cu42, CU42)
RRE_FORM_EMIT(cudtr, CUDTR)
RRE_FORM_EMIT(cuse, CUSE)
RRF2_FORM_EMIT(cutfu, CUTFU)
RRF2_FORM_EMIT(cuutf, CUUTF)
RRE_FORM_EMIT(cuxtr, CUXTR)
RX_FORM_EMIT(cvb, CVB)
RXY_FORM_EMIT(cvbg, CVBG)
RXY_FORM_EMIT(cvby, CVBY)
RX_FORM_EMIT(cvd, CVD)
RXY_FORM_EMIT(cvdg, CVDG)
RXY_FORM_EMIT(cvdy, CVDY)
RRE_FORM_EMIT(cxbr, CXBR)
RRE_FORM_EMIT(cxfbr, CXFBR)
RRF2_FORM_EMIT(cxfbra, CXFBRA)
RRE_FORM_EMIT(cxftr, CXFTR)
RRE_FORM_EMIT(cxgbr, CXGBR)
RRF2_FORM_EMIT(cxgbra, CXGBRA)
RRE_FORM_EMIT(cxgtr, CXGTR)
RRF2_FORM_EMIT(cxgtra, CXGTRA)
RRF2_FORM_EMIT(cxlfbr, CXLFBR)
RRF2_FORM_EMIT(cxlftr, CXLFTR)
RRF2_FORM_EMIT(cxlgbr, CXLGBR)
RRF2_FORM_EMIT(cxlgtr, CXLGTR)
RRE_FORM_EMIT(cxstr, CXSTR)
RRE_FORM_EMIT(cxtr, CXTR)
RRE_FORM_EMIT(cxutr, CXUTR)
RSL_FORM_EMIT(cxzt, CXZT)
RXY_FORM_EMIT(cy, CY)
RSL_FORM_EMIT(czdt, CZDT)
RSL_FORM_EMIT(czxt, CZXT)
RX_FORM_EMIT(d, D)
RXE_FORM_EMIT(ddb, DDB)
RRE_FORM_EMIT(ddbr, DDBR)
RRF1_FORM_EMIT(ddtr, DDTR)
RRF1_FORM_EMIT(ddtra, DDTRA)
RXE_FORM_EMIT(deb, DEB)
RRE_FORM_EMIT(debr, DEBR)
RRF1_FORM_EMIT(didbr, DIDBR)
RRF1_FORM_EMIT(diebr, DIEBR)
RXY_FORM_EMIT(dl, DL)
RXY_FORM_EMIT(dlg, DLG)
RRE_FORM_EMIT(dlgr, DLGR)
RRE_FORM_EMIT(dlr, DLR)
SS2_FORM_EMIT(dp, DP)
RR_FORM_EMIT(dr, DR)
RXY_FORM_EMIT(dsg, DSG)
RXY_FORM_EMIT(dsgf, DSGF)
RRE_FORM_EMIT(dsgfr, DSGFR)
RRE_FORM_EMIT(dsgr, DSGR)
RRE_FORM_EMIT(dxbr, DXBR)
RRF1_FORM_EMIT(dxtr, DXTR)
RRF1_FORM_EMIT(dxtra, DXTRA)
RRE_FORM_EMIT(ear, EAR)
RSY1_FORM_EMIT(ecag, ECAG)
SSF_FORM_EMIT(ectg, ECTG)
SS1_FORM_EMIT(ed, ED)
SS1_FORM_EMIT(edmk, EDMK)
RRE_FORM_EMIT(eedtr, EEDTR)
RRE_FORM_EMIT(eextr, EEXTR)
RRE_FORM_EMIT(efpc, EFPC)
RRE_FORM_EMIT(epsw, EPSW)
RRE_FORM_EMIT(esdtr, ESDTR)
RRE_FORM_EMIT(esxtr, ESXTR)
RRE_FORM_EMIT(etnd, ETND)
RX_FORM_EMIT(ex, EX)
RIL1_FORM_EMIT(exrl, EXRL)
RRF2_FORM_EMIT(fidbr, FIDBR)
RRF2_FORM_EMIT(fidbra, FIDBRA)
RRF2_FORM_EMIT(fidtr, FIDTR)
RRF2_FORM_EMIT(fiebr, FIEBR)
RRF2_FORM_EMIT(fiebra, FIEBRA)
RRF2_FORM_EMIT(fixbr, FIXBR)
RRF2_FORM_EMIT(fixbra, FIXBRA)
RRF2_FORM_EMIT(fixtr, FIXTR)
RRE_FORM_EMIT(flogr, FLOGR)
S_FORM_EMIT(hsch, HSCH)
RX_FORM_EMIT(ic_z, IC_z)
RS2_FORM_EMIT(icm, ICM)
RSY2_FORM_EMIT(icmh, ICMH)
RSY2_FORM_EMIT(icmy, ICMY)
RXY_FORM_EMIT(icy, ICY)
RRF1_FORM_EMIT(iedtr, IEDTR)
RRF1_FORM_EMIT(iextr, IEXTR)
RIL1_FORM_EMIT(iihf, IIHF)
RI1_FORM_EMIT(iihh, IIHH)
RI1_FORM_EMIT(iihl, IIHL)
RIL1_FORM_EMIT(iilf, IILF)
RI1_FORM_EMIT(iilh, IILH)
RI1_FORM_EMIT(iill, IILL)
RRE_FORM_EMIT(ipm, IPM)
RXE_FORM_EMIT(kdb, KDB)
RRE_FORM_EMIT(kdbr, KDBR)
RRE_FORM_EMIT(kdtr, KDTR)
RXE_FORM_EMIT(keb, KEB)
RRE_FORM_EMIT(kebr, KEBR)
RRE_FORM_EMIT(kimd, KIMD)
RRE_FORM_EMIT(klmd, KLMD)
RRE_FORM_EMIT(km, KM)
RRE_FORM_EMIT(kmac, KMAC)
RRE_FORM_EMIT(kmc, KMC)
RRF1_FORM_EMIT(kmctr, KMCTR)
RRE_FORM_EMIT(kmf, KMF)
RRE_FORM_EMIT(kmo, KMO)
RRE_FORM_EMIT(kxbr, KXBR)
RRE_FORM_EMIT(kxtr, KXTR)
RX_FORM_EMIT(l, L)
RX_FORM_EMIT(la, LA)
RSY1_FORM_EMIT(laa, LAA)
RSY1_FORM_EMIT(laag, LAAG)
RSY1_FORM_EMIT(laal, LAAL)
RSY1_FORM_EMIT(laalg, LAALG)
RX_FORM_EMIT(lae, LAE)
RXY_FORM_EMIT(laey, LAEY)
RSY1_FORM_EMIT(lan, LAN)
RSY1_FORM_EMIT(lang, LANG)
RSY1_FORM_EMIT(lao, LAO)
RSY1_FORM_EMIT(laog, LAOG)
RIL1_FORM_EMIT(larl, LARL)
RXY_FORM_EMIT(lat, LAT)
RSY1_FORM_EMIT(lax, LAX)
RSY1_FORM_EMIT(laxg, LAXG)
RXY_FORM_EMIT(lay, LAY)
RXY_FORM_EMIT(lb, LB)
RXY_FORM_EMIT(lbh, LBH)
RRE_FORM_EMIT(lbr, LBR)
RRE_FORM_EMIT(lcdbr, LCDBR)
RRE_FORM_EMIT(lcdfr, LCDFR)
RRE_FORM_EMIT(lcebr, LCEBR)
RRE_FORM_EMIT(lcgfr, LCGFR)
RRE_FORM_EMIT(lcgr, LCGR)
RR_FORM_EMIT(lcr, LCR)
RRE_FORM_EMIT(lcxbr, LCXBR)
RX_FORM_EMIT(ld, LD)
RXE_FORM_EMIT(ldeb, LDEB)
RRE_FORM_EMIT(ldebr, LDEBR)
RRF2_FORM_EMIT(ldetr, LDETR)
RRE_FORM_EMIT(ldgr, LDGR)
RR_FORM_EMIT(ldr, LDR)
RRE_FORM_EMIT(ldxbr, LDXBR)
RRF2_FORM_EMIT(ldxbra, LDXBRA)
RRF2_FORM_EMIT(ldxtr, LDXTR)
RXY_FORM_EMIT(ldy, LDY)
RX_FORM_EMIT(le_z, LE)
RRE_FORM_EMIT(ledbr, LEDBR)
RRF2_FORM_EMIT(ledbra, LEDBRA)
RRF2_FORM_EMIT(ledtr, LEDTR)
RR_FORM_EMIT(ler, LER)
RRE_FORM_EMIT(lexbr, LEXBR)
RRF2_FORM_EMIT(lexbra, LEXBRA)
RXY_FORM_EMIT(ley, LEY)
S_FORM_EMIT(lfas, LFAS)
RXY_FORM_EMIT(lfh, LFH)
RXY_FORM_EMIT(lfhat, LFHAT)
S_FORM_EMIT(lfpc, LFPC)
RXY_FORM_EMIT(lg, LG)
RXY_FORM_EMIT(lgat, LGAT)
RXY_FORM_EMIT(lgb, LGB)
RRE_FORM_EMIT(lgbr, LGBR)
RRE_FORM_EMIT(lgdr, LGDR)
RXY_FORM_EMIT(lgf, LGF)
RIL1_FORM_EMIT(lgfi, LGFI)
RRE_FORM_EMIT(lgfr, LGFR)
RIL1_FORM_EMIT(lgfrl, LGFRL)
RXY_FORM_EMIT(lgh, LGH)
RI1_FORM_EMIT(lghi, LGHI)
RRE_FORM_EMIT(lghr, LGHR)
RIL1_FORM_EMIT(lghrl, LGHRL)
RRE_FORM_EMIT(lgr, LGR)
RIL1_FORM_EMIT(lgrl, LGRL)
RX_FORM_EMIT(lh, LH)
RXY_FORM_EMIT(lhh, LHH)
RRE_FORM_EMIT(lhr, LHR)
RIL1_FORM_EMIT(lhrl, LHRL)
RXY_FORM_EMIT(lhy, LHY)
RXY_FORM_EMIT(llc, LLC)
RXY_FORM_EMIT(llch, LLCH)
RRE_FORM_EMIT(llcr, LLCR)
RXY_FORM_EMIT(llgc, LLGC)
RRE_FORM_EMIT(llgcr, LLGCR)
RXY_FORM_EMIT(llgf, LLGF)
RXY_FORM_EMIT(llgfat, LLGFAT)
RRE_FORM_EMIT(llgfr, LLGFR)
RIL1_FORM_EMIT(llgfrl, LLGFRL)
RXY_FORM_EMIT(llgh, LLGH)
RRE_FORM_EMIT(llghr, LLGHR)
RIL1_FORM_EMIT(llghrl, LLGHRL)
RXY_FORM_EMIT(llgt, LLGT)
RXY_FORM_EMIT(llgtat, LLGTAT)
RRE_FORM_EMIT(llgtr, LLGTR)
RXY_FORM_EMIT(llh, LLH)
RXY_FORM_EMIT(llhh, LLHH)
RRE_FORM_EMIT(llhr, LLHR)
RIL1_FORM_EMIT(llhrl, LLHRL)
RIL1_FORM_EMIT(llihf, LLIHF)
RI1_FORM_EMIT(llihh, LLIHH)
RI1_FORM_EMIT(llihl, LLIHL)
RIL1_FORM_EMIT(llilf, LLILF)
RI1_FORM_EMIT(llilh, LLILH)
RI1_FORM_EMIT(llill, LLILL)
SS5_FORM_EMIT(lmd, LMD)
RSY1_FORM_EMIT(lmh, LMH)
RRE_FORM_EMIT(lndbr, LNDBR)
RRE_FORM_EMIT(lndfr, LNDFR)
RRE_FORM_EMIT(lnebr, LNEBR)
RRE_FORM_EMIT(lngfr, LNGFR)
RRE_FORM_EMIT(lngr, LNGR)
RR_FORM_EMIT(lnr, LNR)
RRE_FORM_EMIT(lnxbr, LNXBR)
RSY1_FORM_EMIT(loc, LOC)
RSY1_FORM_EMIT(locg, LOCG)
RRF2_FORM_EMIT(locgr, LOCGR)
RRF2_FORM_EMIT(locr, LOCR)
SSF_FORM_EMIT(lpd, LPD)
RRE_FORM_EMIT(lpdbr, LPDBR)
RRE_FORM_EMIT(lpdfr, LPDFR)
SSF_FORM_EMIT(lpdg, LPDG)
RRE_FORM_EMIT(lpebr, LPEBR)
RRE_FORM_EMIT(lpgfr, LPGFR)
RRE_FORM_EMIT(lpgr, LPGR)
RXY_FORM_EMIT(lpq, LPQ)
RR_FORM_EMIT(lpr, LPR)
RRE_FORM_EMIT(lpxbr, LPXBR)
RR_FORM_EMIT(lr, LR)
RIL1_FORM_EMIT(lrl, LRL)
RXY_FORM_EMIT(lrv, LRV)
RXY_FORM_EMIT(lrvg, LRVG)
RRE_FORM_EMIT(lrvgr, LRVGR)
RXY_FORM_EMIT(lrvh, LRVH)
RRE_FORM_EMIT(lrvr, LRVR)
RXY_FORM_EMIT(lt_z, LT)
RRE_FORM_EMIT(ltdbr, LTDBR)
RRE_FORM_EMIT(ltdtr, LTDTR)
RRE_FORM_EMIT(ltebr, LTEBR)
RXY_FORM_EMIT(ltg, LTG)
RXY_FORM_EMIT(ltgf, LTGF)
RRE_FORM_EMIT(ltgfr, LTGFR)
RRE_FORM_EMIT(ltgr, LTGR)
RR_FORM_EMIT(ltr, LTR)
RRE_FORM_EMIT(ltxbr, LTXBR)
RRE_FORM_EMIT(ltxtr, LTXTR)
RXE_FORM_EMIT(lxdb, LXDB)
RRE_FORM_EMIT(lxdbr, LXDBR)
RRF2_FORM_EMIT(lxdtr, LXDTR)
RXE_FORM_EMIT(lxeb, LXEB)
RRE_FORM_EMIT(lxebr, LXEBR)
RRE_FORM_EMIT(lxr, LXR)
RXY_FORM_EMIT(ly, LY)
RRE_FORM_EMIT(lzdr, LZDR)
RRE_FORM_EMIT(lzer, LZER)
RRE_FORM_EMIT(lzxr, LZXR)
RX_FORM_EMIT(m, M)
RXF_FORM_EMIT(madb, MADB)
RRD_FORM_EMIT(madbr, MADBR)
RXF_FORM_EMIT(maeb, MAEB)
RRD_FORM_EMIT(maebr, MAEBR)
SI_FORM_EMIT(mc, MC)
RXE_FORM_EMIT(mdb, MDB)
RRE_FORM_EMIT(mdbr, MDBR)
RXE_FORM_EMIT(mdeb, MDEB)
RRE_FORM_EMIT(mdebr, MDEBR)
RRF1_FORM_EMIT(mdtr, MDTR)
RRF1_FORM_EMIT(mdtra, MDTRA)
RXE_FORM_EMIT(meeb, MEEB)
RRE_FORM_EMIT(meebr, MEEBR)
RXY_FORM_EMIT(mfy, MFY)
RI1_FORM_EMIT(mghi, MGHI)
RX_FORM_EMIT(mh, MH)
RI1_FORM_EMIT(mhi, MHI)
RXY_FORM_EMIT(mhy, MHY)
RXY_FORM_EMIT(ml, ML)
RXY_FORM_EMIT(mlg, MLG)
RRE_FORM_EMIT(mlgr, MLGR)
RRE_FORM_EMIT(mlr, MLR)
SS2_FORM_EMIT(mp, MP)
RR_FORM_EMIT(mr_z, MR)
RX_FORM_EMIT(ms, MS)
S_FORM_EMIT(msch, MSCH)
RXF_FORM_EMIT(msdb, MSDB)
RRD_FORM_EMIT(msdbr, MSDBR)
RXF_FORM_EMIT(mseb, MSEB)
RRD_FORM_EMIT(msebr, MSEBR)
RIL1_FORM_EMIT(msfi, MSFI)
RXY_FORM_EMIT(msg, MSG)
RXY_FORM_EMIT(msgf, MSGF)
RIL1_FORM_EMIT(msgfi, MSGFI)
RRE_FORM_EMIT(msgfr, MSGFR)
RRE_FORM_EMIT(msgr, MSGR)
RRE_FORM_EMIT(msr, MSR)
RXY_FORM_EMIT(msy, MSY)
SS1_FORM_EMIT(mvc, MVC)
SS1_FORM_EMIT(mvcin, MVCIN)
RR_FORM_EMIT(mvcl, MVCL)
RS1_FORM_EMIT(mvcle, MVCLE)
RSY1_FORM_EMIT(mvclu, MVCLU)
SIL_FORM_EMIT(mvghi, MVGHI)
SIL_FORM_EMIT(mvhhi, MVHHI)
SIL_FORM_EMIT(mvhi, MVHI)
SI_FORM_EMIT(mvi, MVI)
SIY_FORM_EMIT(mviy, MVIY)
SS1_FORM_EMIT(mvn, MVN)
SS2_FORM_EMIT(mvo, MVO)
RRE_FORM_EMIT(mvst, MVST)
SS1_FORM_EMIT(mvz, MVZ)
RRE_FORM_EMIT(mxbr, MXBR)
RXE_FORM_EMIT(mxdb, MXDB)
RRE_FORM_EMIT(mxdbr, MXDBR)
RRF1_FORM_EMIT(mxtr, MXTR)
RRF1_FORM_EMIT(mxtra, MXTRA)
RX_FORM_EMIT(n, N)
SS1_FORM_EMIT(nc, NC)
RXY_FORM_EMIT(ng, NG)
RRE_FORM_EMIT(ngr, NGR)
RRF1_FORM_EMIT(ngrk, NGRK)
SI_FORM_EMIT(ni, NI)
IE_FORM_EMIT(niai, NIAI)
RIL1_FORM_EMIT(nihf, NIHF)
RI1_FORM_EMIT(nihh, NIHH)
RI1_FORM_EMIT(nihl, NIHL)
RIL1_FORM_EMIT(nilf, NILF)
RI1_FORM_EMIT(nilh, NILH)
RI1_FORM_EMIT(nill, NILL)
SIY_FORM_EMIT(niy, NIY)
RR_FORM_EMIT(nr, NR)
RRF1_FORM_EMIT(nrk, NRK)
RXY_FORM_EMIT(ntstg, NTSTG)
RXY_FORM_EMIT(ny, NY)
RX_FORM_EMIT(o, O)
SS1_FORM_EMIT(oc, OC)
RXY_FORM_EMIT(og, OG)
RRE_FORM_EMIT(ogr, OGR)
RRF1_FORM_EMIT(ogrk, OGRK)
SI_FORM_EMIT(oi, OI)
RIL1_FORM_EMIT(oihf, OIHF)
RI1_FORM_EMIT(oihh, OIHH)
RI1_FORM_EMIT(oihl, OIHL)
RIL1_FORM_EMIT(oilf, OILF)
RI1_FORM_EMIT(oilh, OILH)
RI1_FORM_EMIT(oill, OILL)
SIY_FORM_EMIT(oiy, OIY)
RR_FORM_EMIT(or_z, OR)
RRF1_FORM_EMIT(ork, ORK)
RXY_FORM_EMIT(oy, OY)
SS2_FORM_EMIT(pack, PACK)
RRE_FORM_EMIT(pcc, PCC)
RXY_FORM_EMIT(pfd, PFD)
RIL2_FORM_EMIT(pfdrl, PFDRL)
E_FORM_EMIT(pfpo, PFPO)
SS1_FORM_EMIT(pka, PKA)
SS1_FORM_EMIT(pku, PKU)
SS5_FORM_EMIT(plo, PLO)
RRE_FORM_EMIT(popcnt, POPCNT)
RRF1_FORM_EMIT(ppa, PPA)
RRF1_FORM_EMIT(qadtr, QADTR)
RRF1_FORM_EMIT(qaxtr, QAXTR)
S_FORM_EMIT(rchp, RCHP)
RIE_FORM_EMIT(risbg, RISBG)
RIE_FORM_EMIT(risbgn, RISBGN)
RIE_FORM_EMIT(risbhg, RISBHG)
RIE_FORM_EMIT(risblg, RISBLG)
RSY1_FORM_EMIT(rll, RLL)
RSY1_FORM_EMIT(rllg, RLLG)
RIE_FORM_EMIT(rnsbg, RNSBG)
RIE_FORM_EMIT(rosbg, ROSBG)
// S_FORM_EMIT(rp, RP) RP is not a opcode
RRF1_FORM_EMIT(rrdtr, RRDTR)
RRF1_FORM_EMIT(rrxtr, RRXTR)
S_FORM_EMIT(rsch, RSCH)
RIE_FORM_EMIT(rxsbg, RXSBG)
RX_FORM_EMIT(s, S)
S_FORM_EMIT(sal, SAL)
RRE_FORM_EMIT(sar, SAR)
S_FORM_EMIT(schm, SCHM)
RXE_FORM_EMIT(sdb, SDB)
RRE_FORM_EMIT(sdbr, SDBR)
RRF1_FORM_EMIT(sdtr, SDTR)
RRF1_FORM_EMIT(sdtra, SDTRA)
RXE_FORM_EMIT(seb, SEB)
RRE_FORM_EMIT(sebr, SEBR)
RRE_FORM_EMIT(sfasr, SFASR)
RRE_FORM_EMIT(sfpc, SFPC)
RXY_FORM_EMIT(sg, SG)
RXY_FORM_EMIT(sgf, SGF)
RRE_FORM_EMIT(sgfr, SGFR)
RRE_FORM_EMIT(sgr, SGR)
RRF1_FORM_EMIT(sgrk, SGRK)
RX_FORM_EMIT(sh, SH)
RRF1_FORM_EMIT(shhhr, SHHHR)
RRF1_FORM_EMIT(shhlr, SHHLR)
RXY_FORM_EMIT(shy, SHY)
RX_FORM_EMIT(sl, SL)
RS1_FORM_EMIT(sla, SLA)
RSY1_FORM_EMIT(slag, SLAG)
RSY1_FORM_EMIT(slak, SLAK)
RXY_FORM_EMIT(slb, SLB)
RXY_FORM_EMIT(slbg, SLBG)
RRE_FORM_EMIT(slbgr, SLBGR)
RRE_FORM_EMIT(slbr, SLBR)
RS1_FORM_EMIT(slda, SLDA)
RS1_FORM_EMIT(sldl, SLDL)
RXF_FORM_EMIT(sldt, SLDT)
RIL1_FORM_EMIT(slfi, SLFI)
RXY_FORM_EMIT(slg, SLG)
RXY_FORM_EMIT(slgf, SLGF)
RIL1_FORM_EMIT(slgfi, SLGFI)
RRE_FORM_EMIT(slgfr, SLGFR)
RRE_FORM_EMIT(slgr, SLGR)
RRF1_FORM_EMIT(slgrk, SLGRK)
RRF1_FORM_EMIT(slhhhr, SLHHHR)
RRF1_FORM_EMIT(slhhlr, SLHHLR)
RS1_FORM_EMIT(sll, SLL)
RSY1_FORM_EMIT(sllg, SLLG)
RSY1_FORM_EMIT(sllk, SLLK)
RR_FORM_EMIT(slr, SLR)
RRF1_FORM_EMIT(slrk, SLRK)
RXF_FORM_EMIT(slxt, SLXT)
RXY_FORM_EMIT(sly, SLY)
SS2_FORM_EMIT(sp_z, SP)
RR_FORM_EMIT(spm, SPM)
RXE_FORM_EMIT(sqdb, SQDB)
RRE_FORM_EMIT(sqdbr, SQDBR)
RXE_FORM_EMIT(sqeb, SQEB)
RRE_FORM_EMIT(sqebr, SQEBR)
RRE_FORM_EMIT(sqxbr, SQXBR)
RR_FORM_EMIT(sr, SR)
RS1_FORM_EMIT(sra, SRA)
RSY1_FORM_EMIT(srag, SRAG)
RSY1_FORM_EMIT(srak, SRAK)
RS1_FORM_EMIT(srda, SRDA)
RS1_FORM_EMIT(srdl, SRDL)
RXF_FORM_EMIT(srdt, SRDT)
RRF1_FORM_EMIT(srk, SRK)
RS1_FORM_EMIT(srl, SRL)
RSY1_FORM_EMIT(srlg, SRLG)
RSY1_FORM_EMIT(srlk, SRLK)
S_FORM_EMIT(srnm, SRNM)
S_FORM_EMIT(srnmb, SRNMB)
S_FORM_EMIT(srnmt, SRNMT)
SS3_FORM_EMIT(srp, SRP)
RRE_FORM_EMIT(srst, SRST)
RRE_FORM_EMIT(srstu, SRSTU)
RXF_FORM_EMIT(srxt, SRXT)
S_FORM_EMIT(ssch, SSCH)
RXY_FORM_EMIT(stch, STCH)
S_FORM_EMIT(stck, STCK)
S_FORM_EMIT(stcke, STCKE)
S_FORM_EMIT(stckf, STCKF)
RS2_FORM_EMIT(stcm, STCM)
RSY2_FORM_EMIT(stcmh, STCMH)
RSY2_FORM_EMIT(stcmy, STCMY)
S_FORM_EMIT(stcps, STCPS)
S_FORM_EMIT(stcrw, STCRW)
RXY_FORM_EMIT(stcy, STCY)
RX_FORM_EMIT(std, STD)
RXY_FORM_EMIT(stdy, STDY)
RX_FORM_EMIT(ste, STE)
RXY_FORM_EMIT(stey, STEY)
RXY_FORM_EMIT(stfh, STFH)
S_FORM_EMIT(stfle, STFLE)
S_FORM_EMIT(stfpc, STFPC)
RIL1_FORM_EMIT(stgrl, STGRL)
RX_FORM_EMIT(sth, STH)
RXY_FORM_EMIT(sthh, STHH)
RIL1_FORM_EMIT(sthrl, STHRL)
RXY_FORM_EMIT(sthy, STHY)
RSY1_FORM_EMIT(stmh, STMH)
RSY2_FORM_EMIT(stoc, STOC)
RSY2_FORM_EMIT(stocg, STOCG)
RXY_FORM_EMIT(stpq, STPQ)
RIL1_FORM_EMIT(strl, STRL)
RXY_FORM_EMIT(strv, STRV)
RXY_FORM_EMIT(strvg, STRVG)
RXY_FORM_EMIT(strvh, STRVH)
S_FORM_EMIT(stsch, STSCH)
RXY_FORM_EMIT(sty, STY)
I_FORM_EMIT(svc, SVC)
RRE_FORM_EMIT(sxbr, SXBR)
RRF1_FORM_EMIT(sxtr, SXTR)
RRF1_FORM_EMIT(sxtra, SXTRA)
RXY_FORM_EMIT(sy, SY)
S_FORM_EMIT(tabort, TABORT)
RRF2_FORM_EMIT(tbdr, TBDR)
RRF2_FORM_EMIT(tbedr, TBEDR)
SIL_FORM_EMIT(tbegin, TBEGIN)
SIL_FORM_EMIT(tbeginc, TBEGINC)
RXE_FORM_EMIT(tcdb, TCDB)
RXE_FORM_EMIT(tceb, TCEB)
RXE_FORM_EMIT(tcxb, TCXB)
RXE_FORM_EMIT(tdcdt, TDCDT)
RXE_FORM_EMIT(tdcet, TDCET)
RXE_FORM_EMIT(tdcxt, TDCXT)
RXE_FORM_EMIT(tdgdt, TDGDT)
RXE_FORM_EMIT(tdget, TDGET)
RXE_FORM_EMIT(tdgxt, TDGXT)
S_FORM_EMIT(tend, TEND)
RRE_FORM_EMIT(thder, THDER)
RRE_FORM_EMIT(thdr, THDR)
SI_FORM_EMIT(tm, TM)
RI1_FORM_EMIT(tmh, TMH)
RI1_FORM_EMIT(tmhh, TMHH)
RI1_FORM_EMIT(tmhl, TMHL)
RI1_FORM_EMIT(tml, TML)
RI1_FORM_EMIT(tmlh, TMLH)
RI1_FORM_EMIT(tmll, TMLL)
SIY_FORM_EMIT(tmy, TMY)
RSL_FORM_EMIT(tp, TP)
S_FORM_EMIT(tpi, TPI)
SS1_FORM_EMIT(tr, TR)
RRE_FORM_EMIT(tre, TRE)
RRF2_FORM_EMIT(troo, TROO)
RRF2_FORM_EMIT(trot, TROT)
SS1_FORM_EMIT(trt, TRT)
RRF2_FORM_EMIT(trte, TRTE)
RRF2_FORM_EMIT(trto, TRTO)
SS1_FORM_EMIT(trtr, TRTR)
RRF2_FORM_EMIT(trtre, TRTRE)
RRF2_FORM_EMIT(trtt, TRTT)
S_FORM_EMIT(ts, TS)
S_FORM_EMIT(tsch, TSCH)
SS2_FORM_EMIT(unpk, UNPK)
SS1_FORM_EMIT(unpka, UNPKA)
SS1_FORM_EMIT(unpku, UNPKU)
E_FORM_EMIT(upt, UPT)
RX_FORM_EMIT(x, X)
SS1_FORM_EMIT(xc, XC)
RXY_FORM_EMIT(xg, XG)
RRE_FORM_EMIT(xgr, XGR)
RRF1_FORM_EMIT(xgrk, XGRK)
SI_FORM_EMIT(xi, XI)
RIL1_FORM_EMIT(xihf, XIHF)
RIL1_FORM_EMIT(xilf, XILF)
SIY_FORM_EMIT(xiy, XIY)
RR_FORM_EMIT(xr, XR)
RRF1_FORM_EMIT(xrk, XRK)
S_FORM_EMIT(xsch, XSCH)
RXY_FORM_EMIT(xy, XY)
SS2_FORM_EMIT(zap, ZAP)
// end of S390instructions


bool Assembler::IsNop(Instr instr, int type) {
  ASSERT((0 == type) || (DEBUG_BREAK_NOP == type));
  int reg = 0;
  if (DEBUG_BREAK_NOP == type) {
    reg = 3;
  }
  return instr == (ORI | reg*B21 | reg*B16);
}

// Debugging.
void Assembler::RecordJSReturn() {
  positions_recorder()->WriteRecordedPositions();
  CheckBuffer();
  RecordRelocInfo(RelocInfo::JS_RETURN);
}


void Assembler::RecordDebugBreakSlot() {
  positions_recorder()->WriteRecordedPositions();
  CheckBuffer();
  RecordRelocInfo(RelocInfo::DEBUG_BREAK_SLOT);
}


void Assembler::RecordComment(const char* msg) {
  if (FLAG_code_comments) {
    CheckBuffer();
    RecordRelocInfo(RelocInfo::COMMENT, reinterpret_cast<intptr_t>(msg));
  }
}


void Assembler::GrowBuffer() {
  if (!own_buffer_) FATAL("external code buffer is too small");

  // Compute new buffer size.
  CodeDesc desc;  // the new buffer
  if (buffer_size_ < 4*KB) {
    desc.buffer_size = 4*KB;
  } else if (buffer_size_ < 1*MB) {
    desc.buffer_size = 2*buffer_size_;
  } else {
    desc.buffer_size = buffer_size_ + 1*MB;
  }
  CHECK_GT(desc.buffer_size, 0);  // no overflow

  // Set up new buffer.
  desc.buffer = NewArray<byte>(desc.buffer_size);

  desc.instr_size = pc_offset();
  desc.reloc_size = (buffer_ + buffer_size_) - reloc_info_writer.pos();

  // Copy the data.
  intptr_t pc_delta = desc.buffer - buffer_;
  intptr_t rc_delta = (desc.buffer + desc.buffer_size) -
    (buffer_ + buffer_size_);
  memmove(desc.buffer, buffer_, desc.instr_size);
  memmove(reloc_info_writer.pos() + rc_delta,
          reloc_info_writer.pos(), desc.reloc_size);

  // Switch buffers.
  DeleteArray(buffer_);
  buffer_ = desc.buffer;
  buffer_size_ = desc.buffer_size;
  pc_ += pc_delta;
  reloc_info_writer.Reposition(reloc_info_writer.pos() + rc_delta,
                               reloc_info_writer.last_pc() + pc_delta);

  // None of our relocation types are pc relative pointing outside the code
  // buffer nor pc absolute pointing inside the code buffer, so there is no need
  // to relocate any emitted relocation entries.

#if ABI_USES_FUNCTION_DESCRIPTORS
  // Relocate runtime entries.
  for (RelocIterator it(desc); !it.done(); it.next()) {
    RelocInfo::Mode rmode = it.rinfo()->rmode();
    if (rmode == RelocInfo::INTERNAL_REFERENCE) {
      intptr_t* p = reinterpret_cast<intptr_t*>(it.rinfo()->pc());
      if (*p != 0) {  // 0 means uninitialized.
        *p += pc_delta;
      }
    }
  }
#endif
}


void Assembler::db(uint8_t data) {
  CheckBuffer();
  *reinterpret_cast<uint8_t*>(pc_) = data;
  pc_ += sizeof(uint8_t);
}


void Assembler::dd(uint32_t data) {
  CheckBuffer();
  *reinterpret_cast<uint32_t*>(pc_) = data;
  pc_ += sizeof(uint32_t);
}


void Assembler::RecordRelocInfo(RelocInfo::Mode rmode, intptr_t data) {
  RelocInfo rinfo(pc_, rmode, data, NULL);
  if (rmode >= RelocInfo::JS_RETURN && rmode <= RelocInfo::DEBUG_BREAK_SLOT) {
    // Adjust code for new modes.
    ASSERT(RelocInfo::IsDebugBreakSlot(rmode)
           || RelocInfo::IsJSReturn(rmode)
           || RelocInfo::IsComment(rmode)
           || RelocInfo::IsPosition(rmode));
  }
  if (rinfo.rmode() != RelocInfo::NONE) {
    // Don't record external references unless the heap will be serialized.
    if (rmode == RelocInfo::EXTERNAL_REFERENCE) {
#ifdef DEBUG
      if (!Serializer::enabled()) {
        Serializer::TooLateToEnableNow();
      }
#endif
      if (!Serializer::enabled() && !emit_debug_code()) {
        return;
      }
    }
    ASSERT(buffer_space() >= kMaxRelocSize);  // too late to grow buffer here
    if (rmode == RelocInfo::CODE_TARGET_WITH_ID) {
      RelocInfo reloc_info_with_ast_id(pc_,
                                       rmode,
                                       RecordedAstId().ToInt(),
                                       NULL);
      ClearRecordedAstId();
      reloc_info_writer.Write(&reloc_info_with_ast_id);
    } else {
      reloc_info_writer.Write(&rinfo);
    }
  }
}


void Assembler::BlockTrampolinePoolFor(int instructions) {
  BlockTrampolinePoolBefore(pc_offset() + instructions * kInstrSize);
}


void Assembler::CheckTrampolinePool() {
  // Some small sequences of instructions must not be broken up by the
  // insertion of a trampoline pool; such sequences are protected by setting
  // either trampoline_pool_blocked_nesting_ or no_trampoline_pool_before_,
  // which are both checked here. Also, recursive calls to CheckTrampolinePool
  // are blocked by trampoline_pool_blocked_nesting_.
  if ((trampoline_pool_blocked_nesting_ > 0) ||
      (pc_offset() < no_trampoline_pool_before_)) {
    // Emission is currently blocked; make sure we try again as soon as
    // possible.
    if (trampoline_pool_blocked_nesting_ > 0) {
      next_buffer_check_ = pc_offset() + kInstrSize;
    } else {
      next_buffer_check_ = no_trampoline_pool_before_;
    }
    return;
  }

  ASSERT(!trampoline_emitted_);
  ASSERT(unbound_labels_count_ >= 0);
  if (unbound_labels_count_ > 0) {
    // First we emit jump, then we emit trampoline pool.
    { BlockTrampolinePoolScope block_trampoline_pool(this);
      Label after_pool;
      b(&after_pool);

      int pool_start = pc_offset();
      for (int i = 0; i < unbound_labels_count_; i++) {
        b(&after_pool);
      }
      bind(&after_pool);
      trampoline_ = Trampoline(pool_start, unbound_labels_count_);

      trampoline_emitted_ = true;
      // As we are only going to emit trampoline once, we need to prevent any
      // further emission.
      next_buffer_check_ = kMaxInt;
    }
  } else {
    // Number of branches to unbound label at this point is zero, so we can
    // move next buffer check to maximum.
    next_buffer_check_ = pc_offset() +
      kMaxCondBranchReach - kMaxBlockTrampolineSectionSize;
  }
  return;
}
} }  // namespace v8::internal

#endif  // V8_TARGET_ARCH_S390
