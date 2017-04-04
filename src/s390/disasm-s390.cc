// Copyright 2014 the V8 project authors. All rights reserved.
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.

// A Disassembler object is used to disassemble a block of code instruction by
// instruction. The default implementation of the NameConverter object can be
// overriden to modify register names or to do symbol lookup on addresses.
//
// The example below will disassemble a block of code and print it to stdout.
//
//   NameConverter converter;
//   Disassembler d(converter);
//   for (byte* pc = begin; pc < end;) {
//     v8::internal::EmbeddedVector<char, 256> buffer;
//     byte* prev_pc = pc;
//     pc += d.InstructionDecode(buffer, pc);
//     printf("%p    %08x      %s\n",
//            prev_pc, *reinterpret_cast<int32_t*>(prev_pc), buffer);
//   }
//
// The Disassembler class also has a convenience method to disassemble a block
// of code into a FILE*, meaning that the above functionality could also be
// achieved by just calling Disassembler::Disassemble(stdout, begin, end);

#include <assert.h>
#include <stdarg.h>
#include <stdio.h>
#include <string.h>

#if V8_TARGET_ARCH_S390

#include "src/base/platform/platform.h"
#include "src/disasm.h"
#include "src/macro-assembler.h"
#include "src/s390/constants-s390.h"

namespace v8 {
namespace internal {

//------------------------------------------------------------------------------

// Decoder decodes and disassembles instructions into an output buffer.
// It uses the converter to convert register names and call destinations into
// more informative description.
class Decoder {
 public:
  Decoder(const disasm::NameConverter& converter, Vector<char> out_buffer)
      : converter_(converter), out_buffer_(out_buffer), out_buffer_pos_(0) {
    out_buffer_[out_buffer_pos_] = '\x0';
  }

  ~Decoder() {}

  // Writes one disassembled instruction into 'buffer' (0-terminated).
  // Returns the length of the disassembled machine instruction in bytes.
  int InstructionDecode(byte* instruction);

 private:
  // Bottleneck functions to print into the out_buffer.
  void PrintChar(const char ch);
  void Print(const char* str, bool ebc = false);

  // Printing of common values.
  void PrintRegister(int reg);
  void PrintDRegister(int reg);
  void PrintSoftwareInterrupt(SoftwareInterruptCodes svc);

  // Handle formatting of instructions and their options.
  int FormatRegister(Instruction* instr, const char* option);
  int FormatFloatingRegister(Instruction* instr, const char* option);
  int FormatMask(Instruction* instr, const char* option);
  int FormatDisplacement(Instruction* instr, const char* option);
  int FormatImmediate(Instruction* instr, const char* option);
  int FormatOption(Instruction* instr, const char* option);
  void Format(Instruction* instr, const char* format);
  void Unknown(Instruction* instr);
  void UnknownFormat(Instruction* instr, const char* opcname);

  bool DecodeTwoByte(Instruction* instr);
  bool DecodeFourByte(Instruction* instr);
  bool DecodeSixByte(Instruction* instr);

  const disasm::NameConverter& converter_;
  Vector<char> out_buffer_;
  int out_buffer_pos_;

  DISALLOW_COPY_AND_ASSIGN(Decoder);
};

// Support for assertions in the Decoder formatting functions.
#define STRING_STARTS_WITH(string, compare_string) \
  (strncmp(string, compare_string, strlen(compare_string)) == 0)

// Append the ch to the output buffer.
void Decoder::PrintChar(const char ch) { out_buffer_[out_buffer_pos_++] = ch; }

// Append the str to the output buffer.
void Decoder::Print(const char* str, bool ebc) {
  char cur = *str++;
  while (cur != '\x0' && (out_buffer_pos_ < (out_buffer_.length() - 1))) {
    if (ebc) cur = Ebcdic2Ascii(cur);
    PrintChar(cur);
    cur = *str++;
  }
  out_buffer_[out_buffer_pos_] = 0;
}

// Print the register name according to the active name converter.
void Decoder::PrintRegister(int reg) {
#ifdef V8_OS_ZOS
  Print(converter_.NameOfCPURegister(reg), true);
#else
  Print(converter_.NameOfCPURegister(reg));
#endif
}

// Print the double FP register name according to the active name converter.
void Decoder::PrintDRegister(int reg) {
  Print(DoubleRegister::from_code(reg).ToString());
}

// Print SoftwareInterrupt codes. Factoring this out reduces the complexity of
// the FormatOption method.
void Decoder::PrintSoftwareInterrupt(SoftwareInterruptCodes svc) {
  switch (svc) {
    case kCallRtRedirected:
      Print(u8"call rt redirected");
      return;
    case kBreakpoint:
      Print(u8"breakpoint");
      return;
    default:
      if (svc >= kStopCode) {
        out_buffer_pos_ += SNPrintF(out_buffer_ + out_buffer_pos_, u8"%d - 0x%x",
                                    svc & kStopCodeMask, svc & kStopCodeMask);
      } else {
        out_buffer_pos_ += SNPrintF(out_buffer_ + out_buffer_pos_, u8"%d", svc);
      }
      return;
  }
}

// Handle all register based formatting in this function to reduce the
// complexity of FormatOption.
int Decoder::FormatRegister(Instruction* instr, const char* format) {
  DCHECK(format[0] == '\x72');

  if (format[1] == '\x31') {  // 'r1: register resides in bit 8-11
    RRInstruction* rrinstr = reinterpret_cast<RRInstruction*>(instr);
    int reg = rrinstr->R1Value();
    PrintRegister(reg);
    return 2;
  } else if (format[1] == '\x32') {  // 'r2: register resides in bit 12-15
    RRInstruction* rrinstr = reinterpret_cast<RRInstruction*>(instr);
    int reg = rrinstr->R2Value();
    // indicating it is a r0 for displacement, in which case the offset
    // should be 0.
    if (format[2] == '\x64') {
      if (reg == 0) return 4;
      PrintRegister(reg);
      return 3;
    } else {
      PrintRegister(reg);
      return 2;
    }
  } else if (format[1] == '\x33') {  // 'r3: register resides in bit 16-19
    RSInstruction* rsinstr = reinterpret_cast<RSInstruction*>(instr);
    int reg = rsinstr->B2Value();
    PrintRegister(reg);
    return 2;
  } else if (format[1] == '\x34') {  // 'r4: register resides in bit 20-23
    RSInstruction* rsinstr = reinterpret_cast<RSInstruction*>(instr);
    int reg = rsinstr->B2Value();
    PrintRegister(reg);
    return 2;
  } else if (format[1] == '\x35') {  // 'r5: register resides in bit 24-28
    RREInstruction* rreinstr = reinterpret_cast<RREInstruction*>(instr);
    int reg = rreinstr->R1Value();
    PrintRegister(reg);
    return 2;
  } else if (format[1] == '\x36') {  // 'r6: register resides in bit 29-32
    RREInstruction* rreinstr = reinterpret_cast<RREInstruction*>(instr);
    int reg = rreinstr->R2Value();
    PrintRegister(reg);
    return 2;
  } else if (format[1] == '\x37') {  // 'r6: register resides in bit 32-35
    SSInstruction* ssinstr = reinterpret_cast<SSInstruction*>(instr);
    int reg = ssinstr->B2Value();
    PrintRegister(reg);
    return 2;
  }

  UNREACHABLE();
  return -1;
}

int Decoder::FormatFloatingRegister(Instruction* instr, const char* format) {
  DCHECK(format[0] == '\x66');

  // reuse 1, 5 and 6 because it is coresponding
  if (format[1] == '\x31') {  // 'r1: register resides in bit 8-11
    RRInstruction* rrinstr = reinterpret_cast<RRInstruction*>(instr);
    int reg = rrinstr->R1Value();
    PrintDRegister(reg);
    return 2;
  } else if (format[1] == '\x32') {  // 'f2: register resides in bit 12-15
    RRInstruction* rrinstr = reinterpret_cast<RRInstruction*>(instr);
    int reg = rrinstr->R2Value();
    PrintDRegister(reg);
    return 2;
  } else if (format[1] == '\x33') {  // 'f3: register resides in bit 16-19
    RRDInstruction* rrdinstr = reinterpret_cast<RRDInstruction*>(instr);
    int reg = rrdinstr->R1Value();
    PrintDRegister(reg);
    return 2;
  } else if (format[1] == '\x35') {  // 'f5: register resides in bit 24-28
    RREInstruction* rreinstr = reinterpret_cast<RREInstruction*>(instr);
    int reg = rreinstr->R1Value();
    PrintDRegister(reg);
    return 2;
  } else if (format[1] == '\x36') {  // 'f6: register resides in bit 29-32
    RREInstruction* rreinstr = reinterpret_cast<RREInstruction*>(instr);
    int reg = rreinstr->R2Value();
    PrintDRegister(reg);
    return 2;
  }
  UNREACHABLE();
  return -1;
}

// FormatOption takes a formatting string and interprets it based on
// the current instructions. The format string points to the first
// character of the option string (the option escape has already been
// consumed by the caller.)  FormatOption returns the number of
// characters that were consumed from the formatting string.
int Decoder::FormatOption(Instruction* instr, const char* format) {
  switch (format[0]) {
    case '\x6f': {
      if (instr->Bit(10) == 1) {
        Print(u8"o");
      }
      return 1;
    }
    case '\x2e': {
      if (instr->Bit(0) == 1) {
        Print(u8".");
      } else {
        Print(u8" ");  // ensure consistent spacing
      }
      return 1;
    }
    case '\x72': {
      return FormatRegister(instr, format);
    }
    case '\x66': {
      return FormatFloatingRegister(instr, format);
    }
    case '\x69': {  // int16
      return FormatImmediate(instr, format);
    }
    case '\x75': {  // uint16
      int32_t value = instr->Bits(15, 0);
      out_buffer_pos_ += SNPrintF(out_buffer_ + out_buffer_pos_, u8"%d", value);
      return 6;
    }
    case '\x6c': {
      // Link (LK) Bit 0
      if (instr->Bit(0) == 1) {
        Print(u8"l");
      }
      return 1;
    }
    case '\x61': {
      // Absolute Address Bit 1
      if (instr->Bit(1) == 1) {
        Print(u8"a");
      }
      return 1;
    }
    case '\x74': {  // 'target: target of branch instructions
      // target26 or target16
      DCHECK(STRING_STARTS_WITH(format, u8"target"));
      if ((format[6] == '\x32') && (format[7] == '\x36')) {
        int off = ((instr->Bits(25, 2)) << 8) >> 6;
        out_buffer_pos_ += SNPrintF(
            out_buffer_ + out_buffer_pos_, u8"%+d -> %s", off,
            converter_.NameOfAddress(reinterpret_cast<byte*>(instr) + off));
        return 8;
      } else if ((format[6] == '\x31') && (format[7] == '\x36')) {
        int off = ((instr->Bits(15, 2)) << 18) >> 16;
        out_buffer_pos_ += SNPrintF(
            out_buffer_ + out_buffer_pos_, u8"%+d -> %s", off,
            converter_.NameOfAddress(reinterpret_cast<byte*>(instr) + off));
        return 8;
      }
      case '\x6d': {
        return FormatMask(instr, format);
      }
    }
    case '\x64': {  // ds value for offset
      return FormatDisplacement(instr, format);
    }
    default: {
      UNREACHABLE();
      break;
    }
  }

  UNREACHABLE();
  return -1;
}

int Decoder::FormatMask(Instruction* instr, const char* format) {
  DCHECK(format[0] == '\x6d');
  int32_t value = 0;
  if ((format[1] == '\x31')) {  // prints the mask format in bits 8-12
    value = reinterpret_cast<RRInstruction*>(instr)->R1Value();
    out_buffer_pos_ += SNPrintF(out_buffer_ + out_buffer_pos_, u8"0x%x", value);
    return 2;
  } else if (format[1] == '\x32') {  // mask format in bits 16-19
    value = reinterpret_cast<RXInstruction*>(instr)->B2Value();
    out_buffer_pos_ += SNPrintF(out_buffer_ + out_buffer_pos_, u8"0x%x", value);
    return 2;
  } else if (format[1] == '\x33') {  // mask format in bits 20-23
    value = reinterpret_cast<RRFInstruction*>(instr)->M4Value();
    out_buffer_pos_ += SNPrintF(out_buffer_ + out_buffer_pos_, u8"0x%x", value);
    return 2;
  }

  out_buffer_pos_ += SNPrintF(out_buffer_ + out_buffer_pos_, u8"%d", value);
  return 2;
}

int Decoder::FormatDisplacement(Instruction* instr, const char* format) {
  DCHECK(format[0] == '\x64');

  if (format[1] == '\x31') {  // displacement in 20-31
    RSInstruction* rsinstr = reinterpret_cast<RSInstruction*>(instr);
    uint16_t value = rsinstr->D2Value();
    out_buffer_pos_ += SNPrintF(out_buffer_ + out_buffer_pos_, u8"%d", value);

    return 2;
  } else if (format[1] == '\x32') {  // displacement in 20-39
    RXYInstruction* rxyinstr = reinterpret_cast<RXYInstruction*>(instr);
    int32_t value = rxyinstr->D2Value();
    out_buffer_pos_ += SNPrintF(out_buffer_ + out_buffer_pos_, u8"%d", value);
    return 2;
  } else if (format[1] == '\x34') {  // SS displacement 2 36-47
    SSInstruction* ssInstr = reinterpret_cast<SSInstruction*>(instr);
    uint16_t value = ssInstr->D2Value();
    out_buffer_pos_ += SNPrintF(out_buffer_ + out_buffer_pos_, u8"%d", value);
    return 2;
  } else if (format[1] == '\x33') {  // SS displacement 1 20 - 32
    SSInstruction* ssInstr = reinterpret_cast<SSInstruction*>(instr);
    uint16_t value = ssInstr->D1Value();
    out_buffer_pos_ += SNPrintF(out_buffer_ + out_buffer_pos_, u8"%d", value);
    return 2;
  } else {  // s390 specific
    int32_t value = SIGN_EXT_IMM16(instr->Bits(15, 0) & ~3);
    out_buffer_pos_ += SNPrintF(out_buffer_ + out_buffer_pos_, u8"%d", value);
    return 1;
  }
}

int Decoder::FormatImmediate(Instruction* instr, const char* format) {
  DCHECK(format[0] == '\x69');

  if (format[1] == '\x31') {  // immediate in 16-31
    RIInstruction* riinstr = reinterpret_cast<RIInstruction*>(instr);
    int16_t value = riinstr->I2Value();
    out_buffer_pos_ += SNPrintF(out_buffer_ + out_buffer_pos_, u8"%d", value);
    return 2;
  } else if (format[1] == '\x32') {  // immediate in 16-48
    RILInstruction* rilinstr = reinterpret_cast<RILInstruction*>(instr);
    int32_t value = rilinstr->I2Value();
    out_buffer_pos_ += SNPrintF(out_buffer_ + out_buffer_pos_, u8"%d", value);
    return 2;
  } else if (format[1] == '\x33') {  // immediate in I format
    IInstruction* iinstr = reinterpret_cast<IInstruction*>(instr);
    int8_t value = iinstr->IValue();
    out_buffer_pos_ += SNPrintF(out_buffer_ + out_buffer_pos_, u8"%d", value);
    return 2;
  } else if (format[1] == '\x34') {  // immediate in 16-31, but outputs as offset
    RIInstruction* riinstr = reinterpret_cast<RIInstruction*>(instr);
    int16_t value = riinstr->I2Value() * 2;
    if (value >= 0)
      out_buffer_pos_ += SNPrintF(out_buffer_ + out_buffer_pos_, u8"*+");
    else
      out_buffer_pos_ += SNPrintF(out_buffer_ + out_buffer_pos_, u8"*");

    out_buffer_pos_ += SNPrintF(
        out_buffer_ + out_buffer_pos_, u8"%d -> %s", value,
        converter_.NameOfAddress(reinterpret_cast<byte*>(instr) + value));
    return 2;
  } else if (format[1] == '\x35') {  // immediate in 16-31, but outputs as offset
    RILInstruction* rilinstr = reinterpret_cast<RILInstruction*>(instr);
    int32_t value = rilinstr->I2Value() * 2;
    if (value >= 0)
      out_buffer_pos_ += SNPrintF(out_buffer_ + out_buffer_pos_, u8"*+");
    else
      out_buffer_pos_ += SNPrintF(out_buffer_ + out_buffer_pos_, u8"*");

    out_buffer_pos_ += SNPrintF(
        out_buffer_ + out_buffer_pos_, u8"%d -> %s", value,
        converter_.NameOfAddress(reinterpret_cast<byte*>(instr) + value));
    return 2;
  } else if (format[1] == '\x36') {  // unsigned immediate in 16-31
    RIInstruction* riinstr = reinterpret_cast<RIInstruction*>(instr);
    uint16_t value = riinstr->I2UnsignedValue();
    out_buffer_pos_ += SNPrintF(out_buffer_ + out_buffer_pos_, u8"%d", value);
    return 2;
  } else if (format[1] == '\x37') {  // unsigned immediate in 16-47
    RILInstruction* rilinstr = reinterpret_cast<RILInstruction*>(instr);
    uint32_t value = rilinstr->I2UnsignedValue();
    out_buffer_pos_ += SNPrintF(out_buffer_ + out_buffer_pos_, u8"%d", value);
    return 2;
  } else if (format[1] == '\x38') {  // unsigned immediate in 8-15
    SSInstruction* ssinstr = reinterpret_cast<SSInstruction*>(instr);
    uint8_t value = ssinstr->Length();
    out_buffer_pos_ += SNPrintF(out_buffer_ + out_buffer_pos_, u8"%d", value);
    return 2;
  } else if (format[1] == '\x39') {  // unsigned immediate in 16-23
    RIEInstruction* rie_instr = reinterpret_cast<RIEInstruction*>(instr);
    uint8_t value = rie_instr->I3Value();
    out_buffer_pos_ += SNPrintF(out_buffer_ + out_buffer_pos_, u8"%d", value);
    return 2;
  } else if (format[1] == '\x61') {  // unsigned immediate in 24-31
    RIEInstruction* rie_instr = reinterpret_cast<RIEInstruction*>(instr);
    uint8_t value = rie_instr->I4Value();
    out_buffer_pos_ += SNPrintF(out_buffer_ + out_buffer_pos_, u8"%d", value);
    return 2;
  } else if (format[1] == '\x62') {  // unsigned immediate in 32-39
    RIEInstruction* rie_instr = reinterpret_cast<RIEInstruction*>(instr);
    uint8_t value = rie_instr->I5Value();
    out_buffer_pos_ += SNPrintF(out_buffer_ + out_buffer_pos_, u8"%d", value);
    return 2;
  } else if (format[1] == '\x63') {  // signed immediate in 8-15
    SSInstruction* ssinstr = reinterpret_cast<SSInstruction*>(instr);
    int8_t value = ssinstr->Length();
    out_buffer_pos_ += SNPrintF(out_buffer_ + out_buffer_pos_, u8"%d", value);
    return 2;
  } else if (format[1] == '\x64') {  // signed immediate in 32-47
    SILInstruction* silinstr = reinterpret_cast<SILInstruction*>(instr);
    int16_t value = silinstr->I2Value();
    out_buffer_pos_ += SNPrintF(out_buffer_ + out_buffer_pos_, u8"%d", value);
    return 2;
  } else if (format[1] == '\x65') {  // immediate in 16-47, but outputs as offset
    RILInstruction* rilinstr = reinterpret_cast<RILInstruction*>(instr);
    int32_t value = rilinstr->I2Value() * 2;
    if (value >= 0)
      out_buffer_pos_ += SNPrintF(out_buffer_ + out_buffer_pos_, u8"*+");
    else
      out_buffer_pos_ += SNPrintF(out_buffer_ + out_buffer_pos_, u8"*");

    out_buffer_pos_ += SNPrintF(
        out_buffer_ + out_buffer_pos_, u8"%d -> %s", value,
        converter_.NameOfAddress(reinterpret_cast<byte*>(instr) + value));
    return 2;
  }

  UNREACHABLE();
  return -1;
}

// Format takes a formatting string for a whole instruction and prints it into
// the output buffer. All escaped options are handed to FormatOption to be
// parsed further.
void Decoder::Format(Instruction* instr, const char* format) {
  char cur = *format++;
  while ((cur != 0) && (out_buffer_pos_ < (out_buffer_.length() - 1))) {
    if (cur == '\x27') {  // Single quote is used as the formatting escape.
      format += FormatOption(instr, format);
    } else {
      out_buffer_[out_buffer_pos_++] = cur;
    }
    cur = *format++;
  }
  out_buffer_[out_buffer_pos_] = '\x0';
}

// The disassembler may end up decoding data inlined in the code. We do not want
// it to crash if the data does not ressemble any known instruction.
#define VERIFY(condition) \
  if (!(condition)) {     \
    Unknown(instr);       \
    return;               \
  }

// For currently unimplemented decodings the disassembler calls Unknown(instr)
// which will just print "unknown" of the instruction bits.
void Decoder::Unknown(Instruction* instr) { Format(instr, u8"unknown"); }

// For currently unimplemented decodings the disassembler calls
// UnknownFormat(instr) which will just print opcode name of the
// instruction bits.
void Decoder::UnknownFormat(Instruction* instr, const char* name) {
  char buffer[100];
  snprintf(buffer, sizeof(buffer), u8"%s (unknown-format)", name);
  Format(instr, buffer);
}

// Disassembles Two Byte S390 Instructions
// @return true if successfully decoded
bool Decoder::DecodeTwoByte(Instruction* instr) {
  // Print the Instruction bits.
  out_buffer_pos_ += SNPrintF(out_buffer_ + out_buffer_pos_, u8"%04x           ",
                              instr->InstructionBits<TwoByteInstr>());

  Opcode opcode = instr->S390OpcodeValue();
  switch (opcode) {
    case AR:
      Format(instr, u8"ar\t'r1,'r2");
      break;
    case SR:
      Format(instr, u8"sr\t'r1,'r2");
      break;
    case MR:
      Format(instr, u8"mr\t'r1,'r2");
      break;
    case DR:
      Format(instr, u8"dr\t'r1,'r2");
      break;
    case OR:
      Format(instr, u8"or\t'r1,'r2");
      break;
    case NR:
      Format(instr, u8"nr\t'r1,'r2");
      break;
    case XR:
      Format(instr, u8"xr\t'r1,'r2");
      break;
    case LR:
      Format(instr, u8"lr\t'r1,'r2");
      break;
    case CR:
      Format(instr, u8"cr\t'r1,'r2");
      break;
    case CLR:
      Format(instr, u8"clr\t'r1,'r2");
      break;
    case BCR:
      Format(instr, u8"bcr\t'm1,'r2");
      break;
    case LTR:
      Format(instr, u8"ltr\t'r1,'r2");
      break;
    case ALR:
      Format(instr, u8"alr\t'r1,'r2");
      break;
    case SLR:
      Format(instr, u8"slr\t'r1,'r2");
      break;
    case LNR:
      Format(instr, u8"lnr\t'r1,'r2");
      break;
    case LCR:
      Format(instr, u8"lcr\t'r1,'r2");
      break;
    case BASR:
      Format(instr, u8"basr\t'r1,'r2");
      break;
    case LDR:
      Format(instr, u8"ldr\t'f1,'f2");
      break;
    case BKPT:
      Format(instr, u8"bkpt");
      break;
    default:
      return false;
  }
  return true;
}

// Disassembles Four Byte S390 Instructions
// @return true if successfully decoded
bool Decoder::DecodeFourByte(Instruction* instr) {
  // Print the Instruction bits.
  out_buffer_pos_ += SNPrintF(out_buffer_ + out_buffer_pos_, u8"%08x       ",
                              instr->InstructionBits<FourByteInstr>());

  Opcode opcode = instr->S390OpcodeValue();
  switch (opcode) {
    case AHI:
      Format(instr, u8"ahi\t'r1,'i1");
      break;
    case AGHI:
      Format(instr, u8"aghi\t'r1,'i1");
      break;
    case LHI:
      Format(instr, u8"lhi\t'r1,'i1");
      break;
    case LGHI:
      Format(instr, u8"lghi\t'r1,'i1");
      break;
    case MHI:
      Format(instr, u8"mhi\t'r1,'i1");
      break;
    case MGHI:
      Format(instr, u8"mghi\t'r1,'i1");
      break;
    case CHI:
      Format(instr, u8"chi\t'r1,'i1");
      break;
    case CGHI:
      Format(instr, u8"cghi\t'r1,'i1");
      break;
    case BRAS:
      Format(instr, u8"bras\t'r1,'i1");
      break;
    case BRC:
      Format(instr, u8"brc\t'm1,'i4");
      break;
    case BRCT:
      Format(instr, u8"brct\t'r1,'i4");
      break;
    case BRCTG:
      Format(instr, u8"brctg\t'r1,'i4");
      break;
    case IIHH:
      Format(instr, u8"iihh\t'r1,'i1");
      break;
    case IIHL:
      Format(instr, u8"iihl\t'r1,'i1");
      break;
    case IILH:
      Format(instr, u8"iilh\t'r1,'i1");
      break;
    case IILL:
      Format(instr, u8"iill\t'r1,'i1");
      break;
    case OILL:
      Format(instr, u8"oill\t'r1,'i1");
      break;
    case TMLL:
      Format(instr, u8"tmll\t'r1,'i1");
      break;
    case STM:
      Format(instr, u8"stm\t'r1,'r2,'d1('r3)");
      break;
    case LM:
      Format(instr, u8"lm\t'r1,'r2,'d1('r3)");
      break;
    case SLL:
      Format(instr, u8"sll\t'r1,'d1('r3)");
      break;
    case SRL:
      Format(instr, u8"srl\t'r1,'d1('r3)");
      break;
    case SLA:
      Format(instr, u8"sla\t'r1,'d1('r3)");
      break;
    case SRA:
      Format(instr, u8"sra\t'r1,'d1('r3)");
      break;
    case SLDL:
      Format(instr, u8"sldl\t'r1,'d1('r3)");
      break;
    case AGR:
      Format(instr, u8"agr\t'r5,'r6");
      break;
    case AGFR:
      Format(instr, u8"agfr\t'r5,'r6");
      break;
    case ARK:
      Format(instr, u8"ark\t'r5,'r6,'r3");
      break;
    case AGRK:
      Format(instr, u8"agrk\t'r5,'r6,'r3");
      break;
    case SGR:
      Format(instr, u8"sgr\t'r5,'r6");
      break;
    case SGFR:
      Format(instr, u8"sgfr\t'r5,'r6");
      break;
    case SRK:
      Format(instr, u8"srk\t'r5,'r6,'r3");
      break;
    case SGRK:
      Format(instr, u8"sgrk\t'r5,'r6,'r3");
      break;
    case NGR:
      Format(instr, u8"ngr\t'r5,'r6");
      break;
    case NRK:
      Format(instr, u8"nrk\t'r5,'r6,'r3");
      break;
    case NGRK:
      Format(instr, u8"ngrk\t'r5,'r6,'r3");
      break;
    case NILL:
      Format(instr, u8"nill\t'r1,'i1");
      break;
    case NILH:
      Format(instr, u8"nilh\t'r1,'i1");
      break;
    case OGR:
      Format(instr, u8"ogr\t'r5,'r6");
      break;
    case ORK:
      Format(instr, u8"ork\t'r5,'r6,'r3");
      break;
    case OGRK:
      Format(instr, u8"ogrk\t'r5,'r6,'r3");
      break;
    case XGR:
      Format(instr, u8"xgr\t'r5,'r6");
      break;
    case XRK:
      Format(instr, u8"xrk\t'r5,'r6,'r3");
      break;
    case XGRK:
      Format(instr, u8"xgrk\t'r5,'r6,'r3");
      break;
    case CGR:
      Format(instr, u8"cgr\t'r5,'r6");
      break;
    case CLGR:
      Format(instr, u8"clgr\t'r5,'r6");
      break;
    case LLGFR:
      Format(instr, u8"llgfr\t'r5,'r6");
      break;
    case LBR:
      Format(instr, u8"lbr\t'r5,'r6");
      break;
    case LEDBR:
      Format(instr, u8"ledbr\t'f5,'f6");
      break;
    case LDEBR:
      Format(instr, u8"ldebr\t'f5,'f6");
      break;
    case LTGR:
      Format(instr, u8"ltgr\t'r5,'r6");
      break;
    case LTDBR:
      Format(instr, u8"ltdbr\t'f5,'f6");
      break;
    case LTEBR:
      Format(instr, u8"ltebr\t'f5,'f6");
      break;
    case LGR:
      Format(instr, u8"lgr\t'r5,'r6");
      break;
    case LGDR:
      Format(instr, u8"lgdr\t'r5,'f6");
      break;
    case LGFR:
      Format(instr, u8"lgfr\t'r5,'r6");
      break;
    case LTGFR:
      Format(instr, u8"ltgfr\t'r5,'r6");
      break;
    case LCGR:
      Format(instr, u8"lcgr\t'r5,'r6");
      break;
    case MSR:
      Format(instr, u8"msr\t'r5,'r6");
      break;
    case LGBR:
      Format(instr, u8"lgbr\t'r5,'r6");
      break;
    case LGHR:
      Format(instr, u8"lghr\t'r5,'r6");
      break;
    case MSGR:
      Format(instr, u8"msgr\t'r5,'r6");
      break;
    case DSGR:
      Format(instr, u8"dsgr\t'r5,'r6");
      break;
    case LZDR:
      Format(instr, u8"lzdr\t'f5");
      break;
    case MLR:
      Format(instr, u8"mlr\t'r5,'r6");
      break;
    case MLGR:
      Format(instr, u8"mlgr\t'r5,'r6");
      break;
    case ALCR:
      Format(instr, u8"alcr\t'r5,'r6");
      break;
    case ALGR:
      Format(instr, u8"algr\t'r5,'r6");
      break;
    case ALRK:
      Format(instr, u8"alrk\t'r5,'r6,'r3");
      break;
    case ALGRK:
      Format(instr, u8"algrk\t'r5,'r6,'r3");
      break;
    case SLGR:
      Format(instr, u8"slgr\t'r5,'r6");
      break;
    case SLBR:
      Format(instr, u8"slbr\t'r5,'r6");
      break;
    case DLR:
      Format(instr, u8"dlr\t'r5,'r6");
      break;
    case DLGR:
      Format(instr, u8"dlgr\t'r5,'r6");
      break;
    case SLRK:
      Format(instr, u8"slrk\t'r5,'r6,'r3");
      break;
    case SLGRK:
      Format(instr, u8"slgrk\t'r5,'r6,'r3");
      break;
    case LHR:
      Format(instr, u8"lhr\t'r5,'r6");
      break;
    case LLHR:
      Format(instr, u8"llhr\t'r5,'r6");
      break;
    case LLGHR:
      Format(instr, u8"llghr\t'r5,'r6");
      break;
    case LNGR:
      Format(instr, u8"lngr\t'r5,'r6");
      break;
    case A:
      Format(instr, u8"a\t'r1,'d1('r2d,'r3)");
      break;
    case S:
      Format(instr, u8"s\t'r1,'d1('r2d,'r3)");
      break;
    case M:
      Format(instr, u8"m\t'r1,'d1('r2d,'r3)");
      break;
    case D:
      Format(instr, u8"d\t'r1,'d1('r2d,'r3)");
      break;
    case O:
      Format(instr, u8"o\t'r1,'d1('r2d,'r3)");
      break;
    case N:
      Format(instr, u8"n\t'r1,'d1('r2d,'r3)");
      break;
    case L:
      Format(instr, u8"l\t'r1,'d1('r2d,'r3)");
      break;
    case C:
      Format(instr, u8"c\t'r1,'d1('r2d,'r3)");
      break;
    case AH:
      Format(instr, u8"ah\t'r1,'d1('r2d,'r3)");
      break;
    case SH:
      Format(instr, u8"sh\t'r1,'d1('r2d,'r3)");
      break;
    case MH:
      Format(instr, u8"mh\t'r1,'d1('r2d,'r3)");
      break;
    case AL:
      Format(instr, u8"al\t'r1,'d1('r2d,'r3)");
      break;
    case SL:
      Format(instr, u8"sl\t'r1,'d1('r2d,'r3)");
      break;
    case LA:
      Format(instr, u8"la\t'r1,'d1('r2d,'r3)");
      break;
    case CH:
      Format(instr, u8"ch\t'r1,'d1('r2d,'r3)");
      break;
    case CL:
      Format(instr, u8"cl\t'r1,'d1('r2d,'r3)");
      break;
    case CLI:
      Format(instr, u8"cli\t'd1('r3),'i8");
      break;
    case TM:
      Format(instr, u8"tm\t'd1('r3),'i8");
      break;
    case BC:
      Format(instr, u8"bc\t'm1,'d1('r2d,'r3)");
      break;
    case BCT:
      Format(instr, u8"bct\t'r1,'d1('r2d,'r3)");
      break;
    case ST:
      Format(instr, u8"st\t'r1,'d1('r2d,'r3)");
      break;
    case STC:
      Format(instr, u8"stc\t'r1,'d1('r2d,'r3)");
      break;
    case IC_z:
      Format(instr, u8"ic\t'r1,'d1('r2d,'r3)");
      break;
    case LD:
      Format(instr, u8"ld\t'f1,'d1('r2d,'r3)");
      break;
    case LE:
      Format(instr, u8"le\t'f1,'d1('r2d,'r3)");
      break;
    case LDGR:
      Format(instr, u8"ldgr\t'f5,'r6");
      break;
    case STE:
      Format(instr, u8"ste\t'f1,'d1('r2d,'r3)");
      break;
    case STD:
      Format(instr, u8"std\t'f1,'d1('r2d,'r3)");
      break;
    case CFDBR:
      Format(instr, u8"cfdbr\t'r5,'m2,'f6");
      break;
    case CDFBR:
      Format(instr, u8"cdfbr\t'f5,'m2,'r6");
      break;
    case CFEBR:
      Format(instr, u8"cfebr\t'r5,'m2,'f6");
      break;
    case CEFBR:
      Format(instr, u8"cefbr\t'f5,'m2,'r6");
      break;
    case CGEBR:
      Format(instr, u8"cgebr\t'r5,'m2,'f6");
      break;
    case CGDBR:
      Format(instr, u8"cgdbr\t'r5,'m2,'f6");
      break;
    case CEGBR:
      Format(instr, u8"cegbr\t'f5,'m2,'r6");
      break;
    case CDGBR:
      Format(instr, u8"cdgbr\t'f5,'m2,'r6");
      break;
    case CDLFBR:
      Format(instr, u8"cdlfbr\t'f5,'m2,'r6");
      break;
    case CDLGBR:
      Format(instr, u8"cdlgbr\t'f5,'m2,'r6");
      break;
    case CELGBR:
      Format(instr, u8"celgbr\t'f5,'m2,'r6");
      break;
    case CLFDBR:
      Format(instr, u8"clfdbr\t'r5,'m2,'f6");
      break;
    case CLGDBR:
      Format(instr, u8"clgdbr\t'r5,'m2,'f6");
      break;
    case AEBR:
      Format(instr, u8"aebr\t'f5,'f6");
      break;
    case SEBR:
      Format(instr, u8"sebr\t'f5,'f6");
      break;
    case MEEBR:
      Format(instr, u8"meebr\t'f5,'f6");
      break;
    case DEBR:
      Format(instr, u8"debr\t'f5,'f6");
      break;
    case ADBR:
      Format(instr, u8"adbr\t'f5,'f6");
      break;
    case SDBR:
      Format(instr, u8"sdbr\t'f5,'f6");
      break;
    case MDBR:
      Format(instr, u8"mdbr\t'f5,'f6");
      break;
    case DDBR:
      Format(instr, u8"ddbr\t'f5,'f6");
      break;
    case CDBR:
      Format(instr, u8"cdbr\t'f5,'f6");
      break;
    case CEBR:
      Format(instr, u8"cebr\t'f5,'f6");
      break;
    case SQDBR:
      Format(instr, u8"sqdbr\t'f5,'f6");
      break;
    case SQEBR:
      Format(instr, u8"sqebr\t'f5,'f6");
      break;
    case LCDBR:
      Format(instr, u8"lcdbr\t'f5,'f6");
      break;
    case STH:
      Format(instr, u8"sth\t'r1,'d1('r2d,'r3)");
      break;
    case SRDA:
      Format(instr, u8"srda\t'r1,'d1('r3)");
      break;
    case SRDL:
      Format(instr, u8"srdl\t'r1,'d1('r3)");
      break;
    case MADBR:
      Format(instr, u8"madbr\t'f3,'f5,'f6");
      break;
    case MSDBR:
      Format(instr, u8"msdbr\t'f3,'f5,'f6");
      break;
    case FLOGR:
      Format(instr, u8"flogr\t'r5,'r6");
      break;
    case FIEBRA:
      Format(instr, u8"fiebra\t'f5,'m2,'f6,'m3");
      break;
    case FIDBRA:
      Format(instr, u8"fidbra\t'f5,'m2,'f6,'m3");
      break;
    // TRAP4 is used in calling to native function. it will not be generated
    // in native code.
    case TRAP4: {
      Format(instr, u8"trap4");
      break;
    }
    default:
      return false;
  }
  return true;
}

// Disassembles Six Byte S390 Instructions
// @return true if successfully decoded
bool Decoder::DecodeSixByte(Instruction* instr) {
  // Print the Instruction bits.
  out_buffer_pos_ +=
      SNPrintF(out_buffer_ + out_buffer_pos_, u8"%012" PRIx64 u8"   ",
               instr->InstructionBits<SixByteInstr>());

  Opcode opcode = instr->S390OpcodeValue();
  switch (opcode) {
    case LLILF:
      Format(instr, u8"llilf\t'r1,'i7");
      break;
    case LLIHF:
      Format(instr, u8"llihf\t'r1,'i7");
      break;
    case AFI:
      Format(instr, u8"afi\t'r1,'i7");
      break;
    case ASI:
      Format(instr, u8"asi\t'd2('r3),'ic");
      break;
    case AGSI:
      Format(instr, u8"agsi\t'd2('r3),'ic");
      break;
    case ALFI:
      Format(instr, u8"alfi\t'r1,'i7");
      break;
    case AHIK:
      Format(instr, u8"ahik\t'r1,'r2,'i1");
      break;
    case AGHIK:
      Format(instr, u8"aghik\t'r1,'r2,'i1");
      break;
    case CLGFI:
      Format(instr, u8"clgfi\t'r1,'i7");
      break;
    case CLFI:
      Format(instr, u8"clfi\t'r1,'i7");
      break;
    case CFI:
      Format(instr, u8"cfi\t'r1,'i2");
      break;
    case CGFI:
      Format(instr, u8"cgfi\t'r1,'i2");
      break;
    case BRASL:
      Format(instr, u8"brasl\t'r1,'ie");
      break;
    case BRCL:
      Format(instr, u8"brcl\t'm1,'i5");
      break;
    case IIHF:
      Format(instr, u8"iihf\t'r1,'i7");
      break;
    case IILF:
      Format(instr, u8"iilf\t'r1,'i7");
      break;
    case XIHF:
      Format(instr, u8"xihf\t'r1,'i7");
      break;
    case XILF:
      Format(instr, u8"xilf\t'r1,'i7");
      break;
    case SLLK:
      Format(instr, u8"sllk\t'r1,'r2,'d2('r3)");
      break;
    case SLLG:
      Format(instr, u8"sllg\t'r1,'r2,'d2('r3)");
      break;
    case RLL:
      Format(instr, u8"rll\t'r1,'r2,'d2('r3)");
      break;
    case RLLG:
      Format(instr, u8"rllg\t'r1,'r2,'d2('r3)");
      break;
    case SRLK:
      Format(instr, u8"srlk\t'r1,'r2,'d2('r3)");
      break;
    case SRLG:
      Format(instr, u8"srlg\t'r1,'r2,'d2('r3)");
      break;
    case SLAK:
      Format(instr, u8"slak\t'r1,'r2,'d2('r3)");
      break;
    case SLAG:
      Format(instr, u8"slag\t'r1,'r2,'d2('r3)");
      break;
    case SRAK:
      Format(instr, u8"srak\t'r1,'r2,'d2('r3)");
      break;
    case SRAG:
      Format(instr, u8"srag\t'r1,'r2,'d2('r3)");
      break;
    case RISBG:
      Format(instr, u8"risbg\t'r1,'r2,'i9,'ia,'ib");
      break;
    case RISBGN:
      Format(instr, u8"risbgn\t'r1,'r2,'i9,'ia,'ib");
      break;
    case LMY:
      Format(instr, u8"lmy\t'r1,'r2,'d2('r3)");
      break;
    case LMG:
      Format(instr, u8"lmg\t'r1,'r2,'d2('r3)");
      break;
    case STMY:
      Format(instr, u8"stmy\t'r1,'r2,'d2('r3)");
      break;
    case STMG:
      Format(instr, u8"stmg\t'r1,'r2,'d2('r3)");
      break;
    case LT:
      Format(instr, u8"lt\t'r1,'d2('r2d,'r3)");
      break;
    case LTG:
      Format(instr, u8"ltg\t'r1,'d2('r2d,'r3)");
      break;
    case ML:
      Format(instr, u8"ml\t'r1,'d2('r2d,'r3)");
      break;
    case AY:
      Format(instr, u8"ay\t'r1,'d2('r2d,'r3)");
      break;
    case SY:
      Format(instr, u8"sy\t'r1,'d2('r2d,'r3)");
      break;
    case NY:
      Format(instr, u8"ny\t'r1,'d2('r2d,'r3)");
      break;
    case OY:
      Format(instr, u8"oy\t'r1,'d2('r2d,'r3)");
      break;
    case XY:
      Format(instr, u8"xy\t'r1,'d2('r2d,'r3)");
      break;
    case CY:
      Format(instr, u8"cy\t'r1,'d2('r2d,'r3)");
      break;
    case AHY:
      Format(instr, u8"ahy\t'r1,'d2('r2d,'r3)");
      break;
    case SHY:
      Format(instr, u8"shy\t'r1,'d2('r2d,'r3)");
      break;
    case LGH:
      Format(instr, u8"lgh\t'r1,'d2('r2d,'r3)");
      break;
    case AG:
      Format(instr, u8"ag\t'r1,'d2('r2d,'r3)");
      break;
    case AGF:
      Format(instr, u8"agf\t'r1,'d2('r2d,'r3)");
      break;
    case SG:
      Format(instr, u8"sg\t'r1,'d2('r2d,'r3)");
      break;
    case NG:
      Format(instr, u8"ng\t'r1,'d2('r2d,'r3)");
      break;
    case OG:
      Format(instr, u8"og\t'r1,'d2('r2d,'r3)");
      break;
    case XG:
      Format(instr, u8"xg\t'r1,'d2('r2d,'r3)");
      break;
    case CG:
      Format(instr, u8"cg\t'r1,'d2('r2d,'r3)");
      break;
    case LB:
      Format(instr, u8"lb\t'r1,'d2('r2d,'r3)");
      break;
    case LG:
      Format(instr, u8"lg\t'r1,'d2('r2d,'r3)");
      break;
    case LGF:
      Format(instr, u8"lgf\t'r1,'d2('r2d,'r3)");
      break;
    case LLGF:
      Format(instr, u8"llgf\t'r1,'d2('r2d,'r3)");
      break;
    case LY:
      Format(instr, u8"ly\t'r1,'d2('r2d,'r3)");
      break;
    case ALY:
      Format(instr, u8"aly\t'r1,'d2('r2d,'r3)");
      break;
    case ALG:
      Format(instr, u8"alg\t'r1,'d2('r2d,'r3)");
      break;
    case SLG:
      Format(instr, u8"slg\t'r1,'d2('r2d,'r3)");
      break;
    case SGF:
      Format(instr, u8"sgf\t'r1,'d2('r2d,'r3)");
      break;
    case SLY:
      Format(instr, u8"sly\t'r1,'d2('r2d,'r3)");
      break;
    case LLH:
      Format(instr, u8"llh\t'r1,'d2('r2d,'r3)");
      break;
    case LLGH:
      Format(instr, u8"llgh\t'r1,'d2('r2d,'r3)");
      break;
    case LLC:
      Format(instr, u8"llc\t'r1,'d2('r2d,'r3)");
      break;
    case LLGC:
      Format(instr, u8"llgc\t'r1,'d2('r2d,'r3)");
      break;
    case LDEB:
      Format(instr, u8"ldeb\t'f1,'d2('r2d,'r3)");
      break;
    case LAY:
      Format(instr, u8"lay\t'r1,'d2('r2d,'r3)");
      break;
    case LARL:
      Format(instr, u8"larl\t'r1,'i5");
      break;
    case LGB:
      Format(instr, u8"lgb\t'r1,'d2('r2d,'r3)");
      break;
    case CHY:
      Format(instr, u8"chy\t'r1,'d2('r2d,'r3)");
      break;
    case CLY:
      Format(instr, u8"cly\t'r1,'d2('r2d,'r3)");
      break;
    case CLIY:
      Format(instr, u8"cliy\t'd2('r3),'i8");
      break;
    case TMY:
      Format(instr, u8"tmy\t'd2('r3),'i8");
      break;
    case CLG:
      Format(instr, u8"clg\t'r1,'d2('r2d,'r3)");
      break;
    case BCTG:
      Format(instr, u8"bctg\t'r1,'d2('r2d,'r3)");
      break;
    case STY:
      Format(instr, u8"sty\t'r1,'d2('r2d,'r3)");
      break;
    case STG:
      Format(instr, u8"stg\t'r1,'d2('r2d,'r3)");
      break;
    case ICY:
      Format(instr, u8"icy\t'r1,'d2('r2d,'r3)");
      break;
    case MVC:
      Format(instr, u8"mvc\t'd3('i8,'r3),'d4('r7)");
      break;
    case MVHI:
      Format(instr, u8"mvhi\t'd3('r3),'id");
      break;
    case MVGHI:
      Format(instr, u8"mvghi\t'd3('r3),'id");
      break;
    case ALGFI:
      Format(instr, u8"algfi\t'r1,'i7");
      break;
    case SLGFI:
      Format(instr, u8"slgfi\t'r1,'i7");
      break;
    case SLFI:
      Format(instr, u8"slfi\t'r1,'i7");
      break;
    case NIHF:
      Format(instr, u8"nihf\t'r1,'i7");
      break;
    case NILF:
      Format(instr, u8"nilf\t'r1,'i7");
      break;
    case OIHF:
      Format(instr, u8"oihf\t'r1,'i7");
      break;
    case OILF:
      Format(instr, u8"oilf\t'r1,'i7");
      break;
    case MSFI:
      Format(instr, u8"msfi\t'r1,'i7");
      break;
    case MSGFI:
      Format(instr, u8"msgfi\t'r1,'i7");
      break;
    case LDY:
      Format(instr, u8"ldy\t'f1,'d2('r2d,'r3)");
      break;
    case LEY:
      Format(instr, u8"ley\t'f1,'d2('r2d,'r3)");
      break;
    case STEY:
      Format(instr, u8"stey\t'f1,'d2('r2d,'r3)");
      break;
    case STDY:
      Format(instr, u8"stdy\t'f1,'d2('r2d,'r3)");
      break;
    case ADB:
      Format(instr, u8"adb\t'r1,'d1('r2d, 'r3)");
      break;
    case SDB:
      Format(instr, u8"sdb\t'r1,'d1('r2d, 'r3)");
      break;
    case MDB:
      Format(instr, u8"mdb\t'r1,'d1('r2d, 'r3)");
      break;
    case DDB:
      Format(instr, u8"ddb\t'r1,'d1('r2d, 'r3)");
      break;
    case SQDB:
      Format(instr, u8"sqdb\t'r1,'d1('r2d, 'r3)");
      break;
    default:
      return false;
  }
  return true;
}

#undef VERIFIY

// Disassemble the instruction at *instr_ptr into the output buffer.
int Decoder::InstructionDecode(byte* instr_ptr) {
  Instruction* instr = Instruction::At(instr_ptr);
  int instrLength = instr->InstructionLength();

  if (2 == instrLength)
    DecodeTwoByte(instr);
  else if (4 == instrLength)
    DecodeFourByte(instr);
  else
    DecodeSixByte(instr);

  return instrLength;
}

}  // namespace internal
}  // namespace v8

//------------------------------------------------------------------------------

namespace disasm {

const char* NameConverter::NameOfAddress(byte* addr) const {
  v8::internal::SNPrintF(tmp_buffer_, u8"%p", addr);
  return tmp_buffer_.start();
}

const char* NameConverter::NameOfConstant(byte* addr) const {
  return NameOfAddress(addr);
}

const char* NameConverter::NameOfCPURegister(int reg) const {
  return v8::internal::Register::from_code(reg).ToString();
}

const char* NameConverter::NameOfByteCPURegister(int reg) const {
  UNREACHABLE();  // S390 does not have the concept of a byte register
  return u8"nobytereg";
}

const char* NameConverter::NameOfXMMRegister(int reg) const {
  // S390 does not have XMM register
  // TODO(joransiu): Consider update this for Vector Regs
  UNREACHABLE();
  return u8"noxmmreg";
}

const char* NameConverter::NameInCode(byte* addr) const {
  // The default name converter is called for unknown code. So we will not try
  // to access any memory.
  return u8"";
}

//------------------------------------------------------------------------------

Disassembler::Disassembler(const NameConverter& converter)
    : converter_(converter) {}

Disassembler::~Disassembler() {}

int Disassembler::InstructionDecode(v8::internal::Vector<char> buffer,
                                    byte* instruction) {
  v8::internal::Decoder d(converter_, buffer);
  return d.InstructionDecode(instruction);
}

// The S390 assembler does not currently use constant pools.
int Disassembler::ConstantPoolSizeAt(byte* instruction) { return -1; }

void Disassembler::Disassemble(FILE* f, byte* begin, byte* end) {
  NameConverter converter;
  Disassembler d(converter);
  for (byte* pc = begin; pc < end;) {
    v8::internal::EmbeddedVector<char, 128> buffer;
    buffer[0] = '\x0';
    byte* prev_pc = pc;
    pc += d.InstructionDecode(buffer, pc);
    v8::internal::PrintF(f, u8"%p    %08x      %s\n", prev_pc,
                         *reinterpret_cast<int32_t*>(prev_pc), buffer.start());
  }
}

}  // namespace disasm

#endif  // V8_TARGET_ARCH_S390
