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

// The original source code covered by the above license above has been modified
// significantly by Google Inc.
// Copyright 2012 the V8 project authors. All rights reserved.

//
// Copyright IBM Corp. 2012-2014. All rights reserved.
//

#ifndef V8_S390_ASSEMBLER_S390_INL_H_
#define V8_S390_ASSEMBLER_S390_INL_H_

#include "s390/assembler-s390.h"

#include "cpu.h"
#include "debug.h"


namespace v8 {
namespace internal {


int DoubleRegister::ToAllocationIndex(DoubleRegister reg) {
  int index = reg.code() - 1;  // d0 is skipped
  ASSERT(index < kNumAllocatableRegisters);
  ASSERT(!reg.is(kDoubleRegZero));
  ASSERT(!reg.is(kScratchDoubleReg));
  return index;
}

void RelocInfo::apply(intptr_t delta) {
  if (RelocInfo::IsInternalReference(rmode_)) {
    // absolute code pointer inside code object moves with the code object.
    intptr_t* p = reinterpret_cast<intptr_t*>(pc_);
    *p += delta;  // relocate entry
    CPU::FlushICache(p, sizeof(uintptr_t));
  }
  // We do not use pc relative addressing on PPC, so there is
  // nothing else to do.
}


Address RelocInfo::target_address() {
  ASSERT(IsCodeTarget(rmode_) || rmode_ == RUNTIME_ENTRY);
  return Assembler::target_address_at(pc_);
}


Address RelocInfo::target_address_address() {
  ASSERT(IsCodeTarget(rmode_) || rmode_ == RUNTIME_ENTRY
                              || rmode_ == EMBEDDED_OBJECT
                              || rmode_ == EXTERNAL_REFERENCE);

  // Read the address of the word containing the target_address in an
  // instruction stream.
  // The only architecture-independent user of this function is the serializer.
  // The serializer uses it to find out how many raw bytes of instruction to
  // output before the next target.
  // For an instruction like LIS/ADDIC where the target bits are mixed into the
  // instruction bits, the size of the target will be zero, indicating that the
  // serializer should not step forward in memory after a target is resolved
  // and written. In this case the target_address_address function should
  // return the end of the instructions to be patched, allowing the
  // deserializer to deserialize the instructions as raw bytes and put them in
  // place, ready to be patched with the target.

  return reinterpret_cast<Address>( pc_ + Assembler::kBytesForPtrConstant);
}


int RelocInfo::target_address_size() {
  return Assembler::kSpecialTargetSize;
}


void RelocInfo::set_target_address(Address target, WriteBarrierMode mode) {
  ASSERT(IsCodeTarget(rmode_) || rmode_ == RUNTIME_ENTRY);
  Assembler::set_target_address_at(pc_, target);
  if (mode == UPDATE_WRITE_BARRIER && host() != NULL && IsCodeTarget(rmode_)) {
    Object* target_code = Code::GetCodeFromTargetAddress(target);
    host()->GetHeap()->incremental_marking()->RecordWriteIntoCode(
        host(), this, HeapObject::cast(target_code));
  }
}


Address Assembler::target_address_from_return_address(Address pc) {
  return pc - kCallTargetAddressOffset;
}


Object* RelocInfo::target_object() {
  ASSERT(IsCodeTarget(rmode_) || rmode_ == EMBEDDED_OBJECT);
  return reinterpret_cast<Object*>(Assembler::target_address_at(pc_));
}


Handle<Object> RelocInfo::target_object_handle(Assembler* origin) {
  ASSERT(IsCodeTarget(rmode_) || rmode_ == EMBEDDED_OBJECT);
  return Handle<Object>(reinterpret_cast<Object**>(
      Assembler::target_address_at(pc_)));
}


Object** RelocInfo::target_object_address() {
  ASSERT(IsCodeTarget(rmode_) || rmode_ == EMBEDDED_OBJECT);
  reconstructed_obj_ptr_ =
      reinterpret_cast<Object*>(Assembler::target_address_at(pc_));
  return &reconstructed_obj_ptr_;
}


void RelocInfo::set_target_object(Object* target, WriteBarrierMode mode) {
  ASSERT(IsCodeTarget(rmode_) || rmode_ == EMBEDDED_OBJECT);
  Assembler::set_target_address_at(pc_, reinterpret_cast<Address>(target));
  if (mode == UPDATE_WRITE_BARRIER &&
      host() != NULL &&
      target->IsHeapObject()) {
    host()->GetHeap()->incremental_marking()->RecordWrite(
        host(), &Memory::Object_at(pc_), HeapObject::cast(target));
  }
}


Address* RelocInfo::target_reference_address() {
  ASSERT(rmode_ == EXTERNAL_REFERENCE);
  reconstructed_adr_ptr_ = Assembler::target_address_at(pc_);
  return &reconstructed_adr_ptr_;
}


Handle<JSGlobalPropertyCell> RelocInfo::target_cell_handle() {
  ASSERT(rmode_ == RelocInfo::GLOBAL_PROPERTY_CELL);
  Address address = Memory::Address_at(pc_);
  return Handle<JSGlobalPropertyCell>(
      reinterpret_cast<JSGlobalPropertyCell**>(address));
}


JSGlobalPropertyCell* RelocInfo::target_cell() {
  ASSERT(rmode_ == RelocInfo::GLOBAL_PROPERTY_CELL);
  return JSGlobalPropertyCell::FromValueAddress(Memory::Address_at(pc_));
}


void RelocInfo::set_target_cell(JSGlobalPropertyCell* cell,
                                WriteBarrierMode mode) {
  ASSERT(rmode_ == RelocInfo::GLOBAL_PROPERTY_CELL);
  Address address = cell->address() + JSGlobalPropertyCell::kValueOffset;
  Memory::Address_at(pc_) = address;
  if (mode == UPDATE_WRITE_BARRIER && host() != NULL) {
    // TODO(1550) We are passing NULL as a slot because cell can never be on
    // evacuation candidate.
    host()->GetHeap()->incremental_marking()->RecordWrite(
        host(), NULL, cell);
  }
}


Address RelocInfo::call_address() {
  ASSERT((IsJSReturn(rmode()) && IsPatchedReturnSequence()) ||
         (IsDebugBreakSlot(rmode()) && IsPatchedDebugBreakSlotSequence()));
  // The pc_ offset of 0 assumes patched return sequence per
  // BreakLocationIterator::SetDebugBreakAtReturn(), or debug break
  // slot per BreakLocationIterator::SetDebugBreakAtSlot().
  return Assembler::target_address_at(pc_);
}


void RelocInfo::set_call_address(Address target) {
  ASSERT((IsJSReturn(rmode()) && IsPatchedReturnSequence()) ||
         (IsDebugBreakSlot(rmode()) && IsPatchedDebugBreakSlotSequence()));
  Assembler::set_target_address_at(pc_, target);
  if (host() != NULL) {
    Object* target_code = Code::GetCodeFromTargetAddress(target);
    host()->GetHeap()->incremental_marking()->RecordWriteIntoCode(
        host(), this, HeapObject::cast(target_code));
  }
}


Object* RelocInfo::call_object() {
  return *call_object_address();
}


void RelocInfo::set_call_object(Object* target) {
  *call_object_address() = target;
}


Object** RelocInfo::call_object_address() {
  ASSERT((IsJSReturn(rmode()) && IsPatchedReturnSequence()) ||
         (IsDebugBreakSlot(rmode()) && IsPatchedDebugBreakSlotSequence()));
  return reinterpret_cast<Object**>(pc_ + 2 * Assembler::kInstrSize);
}


bool RelocInfo::IsPatchedReturnSequence() {
  //
  // The patched return sequence is defined by
  // BreakLocationIterator::SetDebugBreakAtReturn()
  // FIXED_SEQUENCE

  bool patched_return = true;
#if V8_TARGET_ARCH_S390X
  Opcode instr0 = Instruction::S390OpcodeValue(
                                      reinterpret_cast<const byte*>(pc_));
  Opcode instr1 = Instruction::S390OpcodeValue(
                                      reinterpret_cast<const byte*>(pc_+6));
  Opcode basr = Instruction::S390OpcodeValue(
                                      reinterpret_cast<const byte*>(pc_ + 12));
  Opcode bkpt = Instruction::S390OpcodeValue(
                                      reinterpret_cast<const byte*>(pc_ + 14));
  patched_return = (IIHF == instr0);
#else
  Opcode instr1 = Instruction::S390OpcodeValue(
                                      reinterpret_cast<const byte*>(pc_));
  Opcode basr = Instruction::S390OpcodeValue(
                                      reinterpret_cast<const byte*>(pc_ + 6));
  Opcode bkpt = Instruction::S390OpcodeValue(
                                      reinterpret_cast<const byte*>(pc_ + 8));
#endif
  patched_return = patched_return &&
                   (IILF == instr1) && (BASR == basr) && (BKPT == bkpt);

  return patched_return;
}


bool RelocInfo::IsPatchedDebugBreakSlotSequence() {
  SixByteInstr current_instr = Assembler::instr_at(pc_);
  return !Assembler::IsNop(current_instr, Assembler::DEBUG_BREAK_NOP);
}


void RelocInfo::Visit(ObjectVisitor* visitor) {
  RelocInfo::Mode mode = rmode();
  if (mode == RelocInfo::EMBEDDED_OBJECT) {
    visitor->VisitEmbeddedPointer(this);
  } else if (RelocInfo::IsCodeTarget(mode)) {
    visitor->VisitCodeTarget(this);
  } else if (mode == RelocInfo::GLOBAL_PROPERTY_CELL) {
    visitor->VisitGlobalPropertyCell(this);
  } else if (mode == RelocInfo::EXTERNAL_REFERENCE) {
    visitor->VisitExternalReference(this);
#ifdef ENABLE_DEBUGGER_SUPPORT
  // TODO(isolates): Get a cached isolate below.
  } else if (((RelocInfo::IsJSReturn(mode) &&
              IsPatchedReturnSequence()) ||
             (RelocInfo::IsDebugBreakSlot(mode) &&
              IsPatchedDebugBreakSlotSequence())) &&
             Isolate::Current()->debug()->has_break_points()) {
    visitor->VisitDebugTarget(this);
#endif
  } else if (mode == RelocInfo::RUNTIME_ENTRY) {
    visitor->VisitRuntimeEntry(this);
  }
}


template<typename StaticVisitor>
void RelocInfo::Visit(Heap* heap) {
  RelocInfo::Mode mode = rmode();
  if (mode == RelocInfo::EMBEDDED_OBJECT) {
    StaticVisitor::VisitEmbeddedPointer(heap, this);
  } else if (RelocInfo::IsCodeTarget(mode)) {
    StaticVisitor::VisitCodeTarget(heap, this);
  } else if (mode == RelocInfo::GLOBAL_PROPERTY_CELL) {
    StaticVisitor::VisitGlobalPropertyCell(heap, this);
  } else if (mode == RelocInfo::EXTERNAL_REFERENCE) {
    StaticVisitor::VisitExternalReference(this);
#ifdef ENABLE_DEBUGGER_SUPPORT
  } else if (heap->isolate()->debug()->has_break_points() &&
             ((RelocInfo::IsJSReturn(mode) &&
              IsPatchedReturnSequence()) ||
             (RelocInfo::IsDebugBreakSlot(mode) &&
              IsPatchedDebugBreakSlotSequence()))) {
    StaticVisitor::VisitDebugTarget(heap, this);
#endif
  } else if (mode == RelocInfo::RUNTIME_ENTRY) {
    StaticVisitor::VisitRuntimeEntry(this);
  }
}

// Operand constructors
Operand::Operand(intptr_t immediate, RelocInfo::Mode rmode)  {
  rm_ = no_reg;
  imm_ = immediate;
  rmode_ = rmode;
}

Operand::Operand(const ExternalReference& f)  {
  rm_ = no_reg;
  imm_ = reinterpret_cast<intptr_t>(f.address());
  rmode_ = RelocInfo::EXTERNAL_REFERENCE;
}

Operand::Operand(Smi* value) {
  rm_ = no_reg;
  imm_ =  reinterpret_cast<intptr_t>(value);
  rmode_ = RelocInfo::NONE;
}

Operand::Operand(Register rm) {
  rm_ = rm;
  rmode_ = RelocInfo::NONE;  // PPC -why doesn't ARM do this?
}

void Assembler::CheckBuffer() {
  if (buffer_space() <= kGap) {
    GrowBuffer();
  }
}

void Assembler::CheckTrampolinePoolQuick() {
  if (pc_offset() >= next_buffer_check_) {
    CheckTrampolinePool();
  }
}

// S390 specific emitting helpers
void Assembler::emit2bytes(uint16_t x) {
    CheckBuffer();
#if __BYTE_ORDER == __LITTLE_ENDIAN
    // We need to emit instructions in big endian format as disassembler /
    // simulator require the first byte of the instruction in order to decode
    // the instruction length.  Swap the bytes.
    x = ((x & 0x00FF) << 8) | ((x & 0xFF00) >> 8);
#endif
    *reinterpret_cast<uint16_t*>(pc_) = x;
    pc_ += 2;
    CheckTrampolinePoolQuick();
}

void Assembler::emit4bytes(uint32_t x) {
    CheckBuffer();
#if __BYTE_ORDER == __LITTLE_ENDIAN
    // We need to emit instructions in big endian format as disassembler /
    // simulator require the first byte of the instruction in order to decode
    // the instruction length.  Swap the bytes.
    x = ((x & 0x000000FF) << 24) | ((x & 0x0000FF00) << 8) |
        ((x & 0x00FF0000) >>  8) | ((x & 0xFF000000) >> 24);
#endif
    *reinterpret_cast<uint32_t*>(pc_) = x;
    pc_ += 4;
    CheckTrampolinePoolQuick();
}

void Assembler::emit6bytes(uint64_t x) {
    CheckBuffer();
#if __BYTE_ORDER == __LITTLE_ENDIAN
    // We need to emit instructions in big endian format as disassembler /
    // simulator require the first byte of the instruction in order to decode
    // the instruction length.  Swap the bytes.
    x = (static_cast<uint64_t>(x & 0xFF) << 40) |
        (static_cast<uint64_t>((x >>  8) & 0xFF) << 32) |
        (static_cast<uint64_t>((x >> 16) & 0xFF) << 24) |
        (static_cast<uint64_t>((x >> 24) & 0xFF) << 16) |
        (static_cast<uint64_t>((x >> 32) & 0xFF) << 8) |
        (static_cast<uint64_t>((x >> 40) & 0xFF));
    x |= (*reinterpret_cast<uint64_t*>(pc_) >> 48) << 48;
#else
    // We need to pad two bytes of zeros in order to get the 6-bytes
    // stored from low address.
    x = x << 16;
    x |= *reinterpret_cast<uint64_t*>(pc_) & 0xFFFF;
#endif
    // It is safe to store 8-bytes, as CheckBuffer() guarantees we have kGap
    // space left over.
    *reinterpret_cast<uint64_t*>(pc_) = x;
    pc_ += 6;
}
// end of S390 specific emitting helpers

bool Operand::is_reg() const {
  return rm_.is_valid();
}


// Fetch the 32bit value from the FIXED_SEQUENCE lis/ori
Address Assembler::target_address_at(Address pc) {
  // S390 Instruction!
  // We want to check for instructions generated by Asm::mov()
  Opcode op1 = Instruction::S390OpcodeValue(reinterpret_cast<const byte*>(pc));
  SixByteInstr instr_1 = Instruction::InstructionBits(
                                        reinterpret_cast<const byte*>(pc));

#if V8_TARGET_ARCH_S390X
  int instr1_length = Instruction::InstructionLength(
                                      reinterpret_cast<const byte*>(pc));
  Opcode op2 = Instruction::S390OpcodeValue(
                        reinterpret_cast<const byte*>(pc + instr1_length));
  SixByteInstr instr_2 = Instruction::InstructionBits(
                        reinterpret_cast<const byte*>(pc + instr1_length));
  // IIHF for hi_32, IILF for lo_32
  if (IIHF == op1 && IILF == op2) {
     return reinterpret_cast<Address>(((instr_1 & 0xFFFFFFFF) << 32) |
                                      ((instr_2 & 0xFFFFFFFF)));
  }
#else
  // IILF loads 32-bits
  if (IILF == op1) {
     return reinterpret_cast<Address>((instr_1 & 0xFFFFFFFF));
  }
#endif

  PPCPORT_UNIMPLEMENTED();
  return (Address)0;
}


// This sets the branch destination (which gets loaded at the call address).
// This is for calls and branches within generated code.  The serializer
// has already deserialized the lis/ori instructions etc.
// There is a FIXED_SEQUENCE assumption here
void Assembler::deserialization_set_special_target_at(
    Address instruction_payload, Address target) {
  set_target_address_at(instruction_payload - kBytesForPtrConstant, target);
}

// This code assumes the FIXED_SEQUENCE of lis/ori
void Assembler::set_target_address_at(Address pc, Address target) {
  // S390 Instruction!
  // We want to check for instructions generated by Asm::mov()
  Opcode op1 = Instruction::S390OpcodeValue(reinterpret_cast<const byte*>(pc));
  SixByteInstr instr_1 = Instruction::InstructionBits(
                                            reinterpret_cast<const byte*>(pc));
  bool patched = false;
#if V8_TARGET_ARCH_S390X
  int instr1_length = Instruction::InstructionLength(
                                            reinterpret_cast<const byte*>(pc));
  Opcode op2 = Instruction::S390OpcodeValue(
                            reinterpret_cast<const byte*>(pc + instr1_length));
  SixByteInstr instr_2 = Instruction::InstructionBits(
                            reinterpret_cast<const byte*>(pc + instr1_length));
  // IIHF for hi_32, IILF for lo_32
  if (IIHF == op1 && IILF == op2) {
    // IIHF
    instr_1 >>= 32;  // Zero out the lower 32-bits
    instr_1 <<= 32;
    instr_1 |= reinterpret_cast<uint64_t>(target) >> 32;

    Instruction::SetInstructionBits<SixByteInstr>(
                                      reinterpret_cast<byte*>(pc), instr_1);

    // IILF
    instr_2 >>= 32;
    instr_2 <<= 32;
    instr_2 |= reinterpret_cast<uint64_t>(target) & 0xFFFFFFFF;

    Instruction::SetInstructionBits<SixByteInstr>(
                      reinterpret_cast<byte*>(pc + instr1_length), instr_2);
    CPU::FlushICache(pc, 12);
    patched = true;
  }
#else
  // IILF loads 32-bits
  if (IILF == op1) {
    instr_1 >>= 32;  // Zero out the lower 32-bits
    instr_1 <<= 32;
    instr_1 |= reinterpret_cast<uint32_t>(target);

    Instruction::SetInstructionBits<SixByteInstr>(
                                     reinterpret_cast<byte*>(pc), instr_1);
    CPU::FlushICache(pc, 6);
    patched = true;
  }
#endif
  if (!patched)
    UNREACHABLE();
}

} }  // namespace v8::internal

#endif  // V8_S390_ASSEMBLER_S390_INL_H_
