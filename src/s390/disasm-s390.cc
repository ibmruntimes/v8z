// Copyright 2011 the V8 project authors. All rights reserved.
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
#include <stdio.h>
#include <stdarg.h>
#include <string.h>
#ifndef WIN32
#include <stdint.h>
#endif

#include "v8.h"

#if defined(V8_TARGET_ARCH_S390)

#include "constants-s390.h"
#include "disasm.h"
#include "macro-assembler.h"
#include "platform.h"


namespace v8 {
namespace internal {


//------------------------------------------------------------------------------

// Decoder decodes and disassembles instructions into an output buffer.
// It uses the converter to convert register names and call destinations into
// more informative description.
class Decoder {
 public:
  Decoder(const disasm::NameConverter& converter,
          Vector<char> out_buffer)
    : converter_(converter),
      out_buffer_(out_buffer),
      out_buffer_pos_(0) {
    out_buffer_[out_buffer_pos_] = '\0';
  }

  ~Decoder() {}

  // Writes one disassembled instruction into 'buffer' (0-terminated).
  // Returns the length of the disassembled machine instruction in bytes.
  int InstructionDecode(byte* instruction);

 private:
  // Bottleneck functions to print into the out_buffer.
  void PrintChar(const char ch);
  void Print(const char* str);

  // Printing of common values.
  void PrintRegister(int reg);
  void PrintDRegister(int reg);
  int FormatFPRegister(Instruction* instr, const char* format);
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
  void MarkerFormat(Instruction* instr, const char* opcname, int id);

  // S390 decoding
  bool DecodeTwoByte(Instruction* instr);
  bool DecodeFourByte(Instruction* instr);
  bool DecodeSixByte(Instruction* instr);

  // PowerPC decoding
  void DecodeExt1(Instruction* instr);
  void DecodeExt2(Instruction* instr);
  void DecodeExt4(Instruction* instr);
  void DecodeExt5(Instruction* instr);

  const disasm::NameConverter& converter_;
  Vector<char> out_buffer_;
  int out_buffer_pos_;

  DISALLOW_COPY_AND_ASSIGN(Decoder);
};


// Support for assertions in the Decoder formatting functions.
#define STRING_STARTS_WITH(string, compare_string) \
  (strncmp(string, compare_string, strlen(compare_string)) == 0)


// Append the ch to the output buffer.
void Decoder::PrintChar(const char ch) {
  out_buffer_[out_buffer_pos_++] = ch;
}


// Append the str to the output buffer.
void Decoder::Print(const char* str) {
  char cur = *str++;
  while (cur != '\0' && (out_buffer_pos_ < (out_buffer_.length() - 1))) {
    PrintChar(cur);
    cur = *str++;
  }
  out_buffer_[out_buffer_pos_] = 0;
}

// Print the register name according to the active name converter.
void Decoder::PrintRegister(int reg) {
  Print(converter_.NameOfCPURegister(reg));
}

// Print the double FP register name according to the active name converter.
void Decoder::PrintDRegister(int reg) {
  Print(FPRegisters::Name(reg));
}

// Print SoftwareInterrupt codes. Factoring this out reduces the complexity of
// the FormatOption method.
void Decoder::PrintSoftwareInterrupt(SoftwareInterruptCodes svc) {
  switch (svc) {
    case kCallRtRedirected:
      Print("call rt redirected");
      return;
    case kBreakpoint:
      Print("breakpoint");
      return;
    default:
      if (svc >= kStopCode) {
        out_buffer_pos_ += OS::SNPrintF(out_buffer_ + out_buffer_pos_,
                                        "%d - 0x%x",
                                        svc & kStopCodeMask,
                                        svc & kStopCodeMask);
      } else {
        out_buffer_pos_ += OS::SNPrintF(out_buffer_ + out_buffer_pos_,
                                        "%d",
                                        svc);
      }
      return;
  }
}

// Handle all register based formatting in this function to reduce the
// complexity of FormatOption.
int Decoder::FormatRegister(Instruction* instr, const char* format) {
  ASSERT(format[0] == 'r');

  if ((format[1] == 't') || (format[1] == 's')) {  // 'rt & 'rs register
    int reg = instr->RTValue();
    PrintRegister(reg);
    return 2;
  } else if (format[1] == 'a') {  // 'ra: RA register
    int reg = instr->RAValue();
    PrintRegister(reg);
    return 2;
  } else if (format[1] == 'b') {  // 'rb: RB register
    int reg = instr->RBValue();
    PrintRegister(reg);
    return 2;
  // S390 specific instructions, and they can be refactored
  } else if (format[1] == '1') {  // 'r1: register resides in bit 8-11
    RRInstruction* rrinstr = reinterpret_cast<RRInstruction*>(instr);
    int reg = rrinstr->R1Value();
    PrintRegister(reg);
    return 2;
  } else if (format[1] == '2') {  // 'r2: register resides in bit 12-15
    RRInstruction* rrinstr = reinterpret_cast<RRInstruction*>(instr);
    int reg = rrinstr->R2Value();
    // indicating it is a r0 for displacement, in which case the offset
    // should be 0.
    if (format[2] == 'd') {
      if (reg == 0)
        return 4;
      PrintRegister(reg);
      return 3;
    } else {
      PrintRegister(reg);
      return 2;
    }
  } else if (format[1] == '3') {  // 'r3: register resides in bit 16-19
    RSInstruction* rsinstr = reinterpret_cast<RSInstruction*>(instr);
    int reg = rsinstr->B2Value();
    PrintRegister(reg);
    return 2;
  } else if (format[1] == '4') {  // 'r4: register resides in bit 20-23
    RSInstruction* rsinstr = reinterpret_cast<RSInstruction*>(instr);
    int reg = rsinstr->B2Value();
    PrintRegister(reg);
    return 2;
  } else if (format[1] == '5') {  // 'r5: register resides in bit 24-28
    RREInstruction* rreinstr = reinterpret_cast<RREInstruction*>(instr);
    int reg = rreinstr->R1Value();
    PrintRegister(reg);
    return 2;
  } else if (format[1] == '6') {  // 'r6: register resides in bit 29-32
    RREInstruction* rreinstr = reinterpret_cast<RREInstruction*>(instr);
    int reg = rreinstr->R2Value();
    PrintRegister(reg);
    return 2;
  } else if (format[1] == '7') {  // 'r6: register resides in bit 32-35
    SSInstruction* ssinstr = reinterpret_cast<SSInstruction*>(instr);
    int reg = ssinstr->B2Value();
    PrintRegister(reg);
    return 2;
  }

  UNREACHABLE();
  return -1;
}

int Decoder::FormatFloatingRegister(Instruction* instr, const char* format) {
  ASSERT(format[0] == 'f');

  // reuse 1, 5 and 6 because it is coresponding
  if (format[1] == '1') {  // 'r1: register resides in bit 8-11
    RRInstruction* rrinstr = reinterpret_cast<RRInstruction*>(instr);
    int reg = rrinstr->R1Value();
    PrintDRegister(reg);
    return 2;
  } else if (format[1] == '2') {  // 'f2: register resides in bit 12-15
    RRInstruction* rrinstr = reinterpret_cast<RRInstruction*>(instr);
    int reg = rrinstr->R2Value();
    PrintDRegister(reg);
    return 2;
  } else if (format[1] == '5') {  // 'r5: register resides in bit 24-28
    RREInstruction* rreinstr = reinterpret_cast<RREInstruction*>(instr);
    int reg = rreinstr->R1Value();
    PrintDRegister(reg);
    return 2;
  } else if (format[1] == '6') {  // 'r6: register resides in bit 29-32
    RREInstruction* rreinstr = reinterpret_cast<RREInstruction*>(instr);
    int reg = rreinstr->R2Value();
    PrintDRegister(reg);
    return 2;
  }
  UNREACHABLE();
  return -1;
}

// Handle all FP register based formatting in this function to reduce the
// complexity of FormatOption.
int Decoder::FormatFPRegister(Instruction* instr, const char* format) {
  ASSERT(format[0] == 'D');

  int retval = 2;
  int reg = -1;
  if (format[1] == 't') {
    reg = instr->RTValue();
  } else if (format[1] == 'a') {
    reg = instr->RAValue();
  } else if (format[1] == 'b') {
    reg = instr->RBValue();
  } else if (format[1] == 'c') {
    reg = instr->RCValue();
  } else {
    UNREACHABLE();
  }

  PrintDRegister(reg);

  return retval;
}

// FormatOption takes a formatting string and interprets it based on
// the current instructions. The format string points to the first
// character of the option string (the option escape has already been
// consumed by the caller.)  FormatOption returns the number of
// characters that were consumed from the formatting string.
int Decoder::FormatOption(Instruction* instr, const char* format) {
  switch (format[0]) {
    case 'o': {
      if (instr->Bit(10) == 1) {
        Print("o");
      }
      return 1;
    }
    case '.': {
      if (instr->Bit(0) == 1) {
        Print(".");
      } else {
        Print(" ");  // ensure consistent spacing
      }
      return 1;
    }
    case 'r': {
      return FormatRegister(instr, format);
    }
    case 'f': {
      return FormatFloatingRegister(instr, format);
    }
    case 'D': {
      return FormatFPRegister(instr, format);
    }
    case 'i': {  // int16
      return FormatImmediate(instr, format);
    }
    case 'u': {  // uint16
      int32_t value = instr->Bits(15, 0);
      out_buffer_pos_ += OS::SNPrintF(out_buffer_ + out_buffer_pos_,
                                      "%d", value);
      return 6;
    }
    case 'l': {
      // Link (LK) Bit 0
      if (instr->Bit(0) == 1) {
        Print("l");
      }
      return 1;
    }
    case 'a': {
      // Absolute Address Bit 1
      if (instr->Bit(1) == 1) {
        Print("a");
      }
      return 1;
    }
    case 't': {  // 'target: target of branch instructions
      // target26 or target16
      ASSERT(STRING_STARTS_WITH(format, "target"));
      if ((format[6] == '2') && (format[7] == '6')) {
        int off = ((instr->Bits(25, 2)) << 8) >> 6;
        out_buffer_pos_ += OS::SNPrintF(out_buffer_ + out_buffer_pos_,
                                        "%+d -> %s",
                                        off,
                                        converter_.NameOfAddress(
                                        reinterpret_cast<byte*>(instr) + off));
        return 8;
      } else if ((format[6] == '1') && (format[7] == '6')) {
        int off = ((instr->Bits(15, 2)) << 18) >> 16;
        out_buffer_pos_ += OS::SNPrintF(out_buffer_ + out_buffer_pos_,
                                        "%+d -> %s",
                                        off,
                                        converter_.NameOfAddress(
                                        reinterpret_cast<byte*>(instr) + off));
        return 8;
      }
     case 's': {
       ASSERT(format[1] == 'h');
       int32_t value = 0;
       int32_t opcode = instr->OpcodeValue() << 26;
       int32_t sh = instr->Bits(15, 11);
       if (opcode == EXT5 ||
           (opcode == EXT2 &&
            instr->Bits(10, 2) << 2 == SRADIX)) {
         // SH Bits 1 and 15-11 (split field)
         value = (sh | (instr->Bit(1) << 5));
       } else {
         // SH Bits 15-11
         value = (sh << 26) >> 26;
       }
       out_buffer_pos_ += OS::SNPrintF(out_buffer_ + out_buffer_pos_,
                                     "%d", value);
       return 2;
     }
     case 'm': {
       return FormatMask(instr, format);
     }
    }
    case 'd': {  // ds value for offset
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
  ASSERT(format[0] == 'm');
  int32_t value = 0;
  if ((format[1] == '1')) {  // prints the mask format in bit 8-12
    value = reinterpret_cast<RRInstruction*>(instr)->R1Value();
    out_buffer_pos_ += OS::SNPrintF(out_buffer_ + out_buffer_pos_,
        "0x%x", value);
    return 2;
  } else if (format[1] == '2') {  // mask format in bit 16 - 19
    value = reinterpret_cast<RXInstruction*>(instr)->B2Value();
    out_buffer_pos_ += OS::SNPrintF(out_buffer_ + out_buffer_pos_,
        "0x%x", value);
    return 2;
  }

  if (format[1] == 'e') {
    if (instr->OpcodeValue() << 26 != EXT5) {
      // ME Bits 10-6
      value = (instr->Bits(10, 6) << 26) >> 26;
    } else {
      // ME Bits 5 and 10-6 (split field)
      value = (instr->Bits(10, 6) | (instr->Bit(5) << 5));
    }
  } else if (format[1] == 'b') {
    if (instr->OpcodeValue() << 26 != EXT5) {
      // MB Bits 5-1
      value = (instr->Bits(5, 1) << 26) >> 26;
    } else {
      // MB Bits 5 and 10-6 (split field)
      value = (instr->Bits(10, 6) | (instr->Bit(5) << 5));
    }
  } else {
    UNREACHABLE();  // bad format
  }
  out_buffer_pos_ += OS::SNPrintF(out_buffer_ + out_buffer_pos_,
      "%d", value);
  return 2;
}

int Decoder::FormatDisplacement(Instruction* instr, const char* format) {
  ASSERT(format[0] == 'd');

  if (format[1] == '1') {  // displacement in 20-31
    RSInstruction* rsinstr = reinterpret_cast<RSInstruction*>(instr);
    uint16_t value = rsinstr->D2Value();
    out_buffer_pos_ += OS::SNPrintF(out_buffer_ + out_buffer_pos_,
                                    "%d", value);

    return 2;
  } else if (format[1] == '2') {  // displacement in 20-39
    RXYInstruction* rxyinstr = reinterpret_cast<RXYInstruction*>(instr);
    int32_t value = rxyinstr->D2Value();
    out_buffer_pos_ += OS::SNPrintF(out_buffer_ + out_buffer_pos_,
                                    "%d", value);
    return 2;
  } else if (format[1] == '4') {  // SS displacement 2 36-47
    SSInstruction* ssInstr = reinterpret_cast<SSInstruction*>(instr);
    uint16_t value = ssInstr->D2Value();
    out_buffer_pos_ += OS::SNPrintF(out_buffer_ + out_buffer_pos_,
                                    "%d", value);
    return 2;
  } else if (format[1] == '3') {  // SS displacement 1 20 - 32
    SSInstruction* ssInstr = reinterpret_cast<SSInstruction*>(instr);
    uint16_t value = ssInstr->D1Value();
    out_buffer_pos_ += OS::SNPrintF(out_buffer_ + out_buffer_pos_,
                                    "%d", value);
    return 2;
  } else {  // ppc specific
      int32_t value = SIGN_EXT_IMM16(instr->Bits(15, 0) & ~3);
      out_buffer_pos_ += OS::SNPrintF(out_buffer_ + out_buffer_pos_,
                                      "%d", value);
      return 1;
  }
}

int Decoder::FormatImmediate(Instruction *instr, const char* format) {
  ASSERT(format[0] == 'i');

  if (format[1] == '1') {  // immediate in 16-31
    RIInstruction* riinstr = reinterpret_cast<RIInstruction*>(instr);
    int16_t value = riinstr->I2Value();
    out_buffer_pos_ += OS::SNPrintF(out_buffer_ + out_buffer_pos_,
                                    "%d", value);
    return 2;
  } else if (format[1] == '2') {  // immediate in 16-48
    RILInstruction* rilinstr = reinterpret_cast<RILInstruction*>(instr);
    int32_t value = rilinstr->I2Value();
    out_buffer_pos_ += OS::SNPrintF(out_buffer_ + out_buffer_pos_,
                                    "%d", value);
    return 2;
  } else if (format[1] == '3') {  // immediate in I format
    IInstruction* iinstr = reinterpret_cast<IInstruction*>(instr);
    int16_t value = iinstr->IValue();
    out_buffer_pos_ += OS::SNPrintF(out_buffer_ + out_buffer_pos_,
                                    "%d", value);
    return 2;
  } else if (format[1] == '4') {  // immediate in 16-31, but outputs as offset
    RIInstruction* riinstr = reinterpret_cast<RIInstruction*>(instr);
    int16_t value = riinstr->I2Value()*2;
    if (value >= 0)
      out_buffer_pos_ += OS::SNPrintF(out_buffer_ + out_buffer_pos_, "*+");
    else
      out_buffer_pos_ += OS::SNPrintF(out_buffer_ + out_buffer_pos_, "*");

    out_buffer_pos_ += OS::SNPrintF(out_buffer_ + out_buffer_pos_,
                                    "%d -> %s", value,
                                    converter_.NameOfAddress(
                                       reinterpret_cast<byte*>(instr) + value));
    return 2;
  } else if (format[1] == '5') {  // immediate in 16-31, but outputs as offset
    RILInstruction* rilinstr = reinterpret_cast<RILInstruction*>(instr);
    int32_t value = rilinstr->I2Value()*2;
    if (value >= 0)
      out_buffer_pos_ += OS::SNPrintF(out_buffer_ + out_buffer_pos_, "*+");
    else
      out_buffer_pos_ += OS::SNPrintF(out_buffer_ + out_buffer_pos_, "*");

    out_buffer_pos_ += OS::SNPrintF(out_buffer_ + out_buffer_pos_,
                                    "%d -> %s", value,
                                    converter_.NameOfAddress(
                                       reinterpret_cast<byte*>(instr) + value));
    return 2;
  } else if (format[1] == '6') {  // unsigned immediate in 16-31
    RIInstruction* riinstr = reinterpret_cast<RIInstruction*>(instr);
    uint16_t value = riinstr->I2UnsignedValue();
    out_buffer_pos_ += OS::SNPrintF(out_buffer_ + out_buffer_pos_,
                                    "%d", value);
    return 2;
  } else if (format[1] == '7') {  // unsigned immediate in 16-48
    RILInstruction* rilinstr = reinterpret_cast<RILInstruction*>(instr);
    uint32_t value = rilinstr->I2UnsignedValue();
    out_buffer_pos_ += OS::SNPrintF(out_buffer_ + out_buffer_pos_,
                                    "%d", value);
    return 2;
  } else if (format[1] == '8') {  // unsigned immediate in 8-16
    SSInstruction* ssinstr = reinterpret_cast<SSInstruction*>(instr);
    uint8_t value = ssinstr->Length();
    out_buffer_pos_ += OS::SNPrintF(out_buffer_ + out_buffer_pos_,
                                    "%d", value);
    return 2;
  } else {  // ppc specific
    int32_t value = (instr->Bits(15, 0) << 16) >> 16;
    out_buffer_pos_ += OS::SNPrintF(out_buffer_ + out_buffer_pos_,
        "%d", value);
    return 5;
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
    if (cur == '\'') {  // Single quote is used as the formatting escape.
      format += FormatOption(instr, format);
    } else {
      out_buffer_[out_buffer_pos_++] = cur;
    }
    cur = *format++;
  }
  out_buffer_[out_buffer_pos_]  = '\0';
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
void Decoder::Unknown(Instruction* instr) {
  Format(instr, "unknown");
}

// For currently unimplemented decodings the disassembler calls
// UnknownFormat(instr) which will just print opcode name of the
// instruction bits.
void Decoder::UnknownFormat(Instruction* instr, const char* name) {
  char buffer[100];
  snprintf(buffer, sizeof(buffer), "%s (unknown-format)", name);
  Format(instr, buffer);
}

void Decoder::MarkerFormat(Instruction* instr, const char* name, int id) {
  char buffer[100];
  snprintf(buffer, sizeof(buffer), "%s %d", name, id);
  Format(instr, buffer);
}

// PowerPC
void Decoder::DecodeExt1(Instruction* instr) {
  switch (instr->Bits(10, 1) << 1) {
    case MCRF: {
      UnknownFormat(instr, "mcrf");  // not used by V8
      break;
    }
    case BCLRX: {
      switch (instr->Bits(25, 21) << 21) {
        case DCBNZF: {
          UnknownFormat(instr, "bclrx-dcbnzf");
          break;
        }
        case DCBEZF: {
          UnknownFormat(instr, "bclrx-dcbezf");
          break;
        }
        case BF: {
          UnknownFormat(instr, "bclrx-bf");
          break;
        }
        case DCBNZT: {
          UnknownFormat(instr, "bclrx-dcbbzt");
          break;
        }
        case DCBEZT: {
          UnknownFormat(instr, "bclrx-dcbnezt");
          break;
        }
        case BT: {
          UnknownFormat(instr, "bclrx-bt");
          break;
        }
        case DCBNZ: {
          UnknownFormat(instr, "bclrx-dcbnz");
          break;
        }
        case DCBEZ: {
          UnknownFormat(instr, "bclrx-dcbez");  // not used by V8
          break;
        }
        case BA: {
          if (instr->Bit(0) == 1) {
            Format(instr, "blrl");
          } else {
            Format(instr, "blr");
          }
          break;
        }
      }
      break;
    }
    case BCCTRX: {
      switch (instr->Bits(25, 21) << 21) {
        case DCBNZF: {
          UnknownFormat(instr, "bcctrx-dcbnzf");
          break;
        }
        case DCBEZF: {
          UnknownFormat(instr, "bcctrx-dcbezf");
          break;
        }
        case BF: {
          UnknownFormat(instr, "bcctrx-bf");
          break;
        }
        case DCBNZT: {
          UnknownFormat(instr, "bcctrx-dcbnzt");
          break;
        }
        case DCBEZT: {
          UnknownFormat(instr, "bcctrx-dcbezf");
          break;
        }
        case BT: {
          UnknownFormat(instr, "bcctrx-bt");
          break;
        }
        case DCBNZ: {
          UnknownFormat(instr, "bcctrx-dcbnz");
          break;
        }
        case DCBEZ: {
          UnknownFormat(instr, "bcctrx-dcbez");
          break;
        }
        case BA: {
          if (instr->Bit(0) == 1) {
            Format(instr, "bctrl");
          } else {
            Format(instr, "bctr");
          }
          break;
        }
        default: {
          UNREACHABLE();
        }
      }
      break;
    }
    case CRNOR: {
      Format(instr, "crnor (stuff)");
      break;
    }
    case RFI: {
      Format(instr, "rfi (stuff)");
      break;
    }
    case CRANDC: {
      Format(instr, "crandc (stuff)");
      break;
    }
    case ISYNC: {
      Format(instr, "isync (stuff)");
      break;
    }
    case CRXOR: {
      Format(instr, "crxor (stuff)");
      break;
    }
    case CRNAND: {
      UnknownFormat(instr, "crnand");
      break;
    }
    case CRAND: {
      UnknownFormat(instr, "crand");
      break;
    }
    case CREQV: {
      UnknownFormat(instr, "creqv");
      break;
    }
    case CRORC: {
      UnknownFormat(instr, "crorc");
      break;
    }
    case CROR: {
      UnknownFormat(instr, "cror");
      break;
    }
    default: {
      Unknown(instr);  // not used by V8
    }
  }
}

void Decoder::DecodeExt2(Instruction* instr) {
  // Some encodings are 10-1 bits, handle those first
  switch (instr->Bits(10, 1) << 1) {
    case SRWX: {
      Format(instr, "srw'.    'ra, 'rs, 'rb");
      return;
    }
#if V8_TARGET_ARCH_S390X
    case SRDX: {
      Format(instr, "srd'.    'ra, 'rs, 'rb");
      return;
    }
#endif
    case SRAW: {
      Format(instr, "sraw'.   'ra, 'rs, 'rb");
      return;
    }
#if V8_TARGET_ARCH_S390X
    case SRAD: {
      Format(instr, "srad'.   'ra, 'rs, 'rb");
      return;
    }
#endif
    case LFSX: {
      Format(instr, "lfsx    'rt, 'ra, 'rb");
      return;
    }
    case LFSUX: {
      Format(instr, "lfsux   'rt, 'ra, 'rb");
      return;
    }
    case LFDUX: {
      Format(instr, "lfdux   'rt, 'ra, 'rb");
      return;
    }
    case STFSX: {
      Format(instr, "stfsx    'rs, 'ra, 'rb");
      return;
    }
    case STFSUX: {
      Format(instr, "stfsux   'rs, 'ra, 'rb");
      return;
    }
    case STFDUX: {
      Format(instr, "stfdux   'rs, 'ra, 'rb");
      return;
    }
  }

  switch (instr->Bits(10, 2) << 2) {
    case SRADIX: {
      Format(instr, "sradi'.  'ra,'rs,'sh");
      return;
    }
  }

  // ?? are all of these xo_form?
  switch (instr->Bits(9, 1) << 1) {
    case CMP: {
      Format(instr, "cmp     'ra, 'rb");
      break;
    }
    case SLWX: {
      Format(instr, "slw'.   'ra, 'rs, 'rb");
      break;
    }
#if V8_TARGET_ARCH_S390X
    case SLDX: {
      Format(instr, "sld'.   'ra, 'rs, 'rb");
      break;
    }
#endif
    case SUBFCX: {
      Format(instr, "subfc'. 'rt, 'ra, 'rb");
      break;
    }
    case ADDCX: {
      Format(instr, "addc'.   'rt, 'ra, 'rb");
      break;
    }
    case CNTLZWX: {
      Format(instr, "cntlzw'. 'ra, 'rs");
      break;
    }
#if V8_TARGET_ARCH_S390X
    case CNTLZDX: {
      Format(instr, "cntlzd'. 'ra, 'rs");
      break;
    }
#endif
    case ANDX: {
      Format(instr, "and'.    'ra, 'rs, 'rb");
      break;
    }
    case ANDCX: {
      Format(instr, "andc'.   'ra, 'rs, 'rb");
      break;
    }
    case CMPL: {
      Format(instr, "cmpl    'ra, 'rb");
      break;
    }
    case NEGX: {
      Format(instr, "neg'.    'rt, 'ra");
      break;
    }
    case NORX: {
      Format(instr, "nor'.    'rt, 'ra, 'rb");
      break;
    }
    case SUBFX: {
      Format(instr, "subf'.   'rt, 'ra, 'rb");
      break;
    }
    case MULHWX: {
      Format(instr, "mulhw'o'.  'rt, 'ra, 'rb");
      break;
    }
    case ADDZEX: {
      Format(instr, "addze'.   'rt, 'ra");
      break;
    }
    case MULLW: {
      Format(instr, "mullw'o'.  'rt, 'ra, 'rb");
      break;
    }
    case DIVW: {
      Format(instr, "divw'o'.   'rt, 'ra, 'rb");
      break;
    }
#if V8_TARGET_ARCH_S390X
    case DIVD: {
      Format(instr, "divd'o'.   'rt, 'ra, 'rb");
      break;
    }
#endif
    case ADDX: {
      Format(instr, "add'o     'rt, 'ra, 'rb");
      break;
    }
    case XORX: {
      Format(instr, "xor'.    'ra, 'rs, 'rb");
      break;
    }
    case ORX: {
      if ( instr->RTValue() == instr->RBValue() ) {
        Format(instr, "mr      'ra, 'rb");
      } else {
        Format(instr, "or      'ra, 'rs, 'rb");
      }
      break;
    }
    case MFSPR: {
      int spr = instr->Bits(20, 11);
      if (256 == spr) {
        Format(instr, "mflr    'rt");
      } else {
        Format(instr, "mfspr   'rt ??");
      }
      break;
    }
    case MTSPR: {
      int spr = instr->Bits(20, 11);
      if (256 == spr) {
        Format(instr, "mtlr    'rt");
      } else if (288 == spr) {
        Format(instr, "mtctr   'rt");
      } else {
        Format(instr, "mtspr   'rt ??");
      }
      break;
    }
    case MFCR: {
      Format(instr, "mfcr    'rt");
      break;
    }
    case STWX: {
      Format(instr, "stwx    'rs, 'ra, 'rb");
      break;
    }
    case STWUX: {
      Format(instr, "stwux   'rs, 'ra, 'rb");
      break;
    }
    case LWZX: {
      Format(instr, "lwzx    'rt, 'ra, 'rb");
      break;
    }
    case LWZUX: {
      Format(instr, "lwzux   'rt, 'ra, 'rb");
      break;
    }
#if V8_TARGET_ARCH_S390X
    case LDX: {
      Format(instr, "ldx     'rt, 'ra, 'rb");
      break;
    }
    case LDUX: {
      Format(instr, "ldux    'rt, 'ra, 'rb");
      break;
    }
    case STDX: {
      Format(instr, "stdx    'rt, 'ra, 'rb");
      break;
    }
    case STDUX: {
      Format(instr, "stdux   'rt, 'ra, 'rb");
      break;
    }
#endif
    default: {
      Unknown(instr);  // not used by V8
    }
  }
}

void Decoder::DecodeExt4(Instruction* instr) {
  switch (instr->Bits(5, 1) << 1) {
    case FDIV: {
      Format(instr, "fdiv'.   'Dt, 'Da, 'Db");
      return;
    }
    case FSUB: {
      Format(instr, "fsub'.   'Dt, 'Da, 'Db");
      return;
    }
    case FADD: {
      Format(instr, "fadd'.   'Dt, 'Da, 'Db");
      return;
    }
    case FSEL: {
      Format(instr, "fsel'.   'Dt, 'Da, 'Dc, 'Db");
      return;
    }
    case FMUL: {
      Format(instr, "fmul'.   'Dt, 'Da, 'Dc");
      return;
    }
  }

  switch (instr->Bits(10, 1) << 1) {
    case FCMPU: {
      Format(instr, "fcmpu   'Da, 'Db");
      break;
    }
    case FRSP: {
      Format(instr, "frsp'.   'Dt, 'Db");
      break;
    }
    case FCFID: {
      Format(instr, "fcfid'.  'Dt, 'Db");
      break;
    }
    case FCTID: {
      Format(instr, "fctid   'Dt, 'Db");
      break;
    }
    case FCTIDZ: {
      Format(instr, "fctidz  'Dt, 'Db");
      break;
    }
    case FCTIW: {
      Format(instr, "fctiw'. 'Dt, 'Db");
      break;
    }
    case FCTIWZ: {
      Format(instr, "fctiwz'. 'Dt, 'Db");
      break;
    }
    case MTFSFI: {
      Format(instr, "mtfsfi'.  ?,?");
      break;
    }
    case MFFS: {
      Format(instr, "mffs'.   'Dt");
      break;
    }
    case MTFSF: {
      Format(instr, "mtfsf'.  'Db ?,?,?");
      break;
    }
    case FABS: {
      Format(instr, "fabs'.   'Dt, 'Db");
      break;
    }
    case FRIM: {
      Format(instr, "frim    'Dt, 'Db");
      break;
    }
    default: {
      Unknown(instr);  // not used by V8
    }
  }
}

void Decoder::DecodeExt5(Instruction* instr) {
  switch (instr->Bits(4, 2) << 2) {
    case RLDICL: {
      Format(instr, "rldicl'. 'ra, 'rs, 'sh, 'mb");
      break;
    }
    case RLDICR: {
      Format(instr, "rldicr'. 'ra, 'rs, 'sh, 'me");
      break;
    }
    case RLDIC: {
      Format(instr, "rldic'.  'ra, 'rs, 'sh, 'mb");
      break;
    }
    default: {
      Unknown(instr);  // not used by V8
    }
  }
}

// Disassembles Two Byte S390 Instructions
// @return true if successfully decoded
bool Decoder::DecodeTwoByte(Instruction* instr) {
  // Print the Instruction bits.
  out_buffer_pos_ += OS::SNPrintF(out_buffer_ + out_buffer_pos_,
                                  "%04x           ",
                                  instr->InstructionBits<TwoByteInstr>());

  Opcode opcode = instr->S390OpcodeValue();
  switch (opcode) {
      case SVC: Format(instr, "svc\t'i3"); break;
      case AR: Format(instr, "ar\t'r1,'r2"); break;
      case SR: Format(instr, "sr\t'r1,'r2"); break;
      case MR: Format(instr, "mr\t'r1,'r2"); break;
      case DR: Format(instr, "dr\t'r1,'r2"); break;
      case OR: Format(instr, "or\t'r1,'r2"); break;
      case NR: Format(instr, "nr\t'r1,'r2"); break;
      case XR: Format(instr, "xr\t'r1,'r2"); break;
      case CGR: Format(instr, "cgr\t'r1,'r2"); break;
      case LR: Format(instr, "lr\t'r1,'r2"); break;
      case LLHR: Format(instr, "llhr\t'r1,'r2"); break;
      case CR: Format(instr, "cr\t'r1,'r2"); break;
      case CLR: Format(instr, "clr\t'r1,'r2"); break;
      case BCR: Format(instr, "bcr\t'm1,'r2"); break;
      case LTR: Format(instr, "ltr\t'r1,'r2"); break;
      case ALR: Format(instr, "alr\t'r1,'r2"); break;
      case SLR: Format(instr, "slr\t'r1,'r2"); break;
      case LBR: Format(instr, "lbr\t'r1,'r2"); break;
      case LNR: Format(instr, "lnr\t'r1,'r2"); break;
      case LCR: Format(instr, "lcr\t'r1,'r2"); break;
      case BASR: Format(instr, "basr\t'r1,'r2"); break;
      case LDR: Format(instr, "ldr\t'f1,'f2"); break;
      case BKPT: Format(instr, "bkpt"); break;
    default:
      return false;
  }
  return true;
}

// Disassembles Four Byte S390 Instructions
// @return true if successfully decoded
bool Decoder::DecodeFourByte(Instruction* instr) {
  // Print the Instruction bits.
  out_buffer_pos_ += OS::SNPrintF(out_buffer_ + out_buffer_pos_,
                                  "%08x       ",
                                  instr->InstructionBits<FourByteInstr>());

  Opcode opcode = instr->S390OpcodeValue();
  switch (opcode) {
      case AHI: Format(instr, "ahi\t'r1,'i1"); break;
      case AGHI: Format(instr, "aghi\t'r1,'i1"); break;
      case LHI: Format(instr, "lhi\t'r1,'i1"); break;
      case LGHI: Format(instr, "lghi\t'r1,'i1"); break;
      case MHI: Format(instr, "mhi\t'r1,'i1"); break;
      case MGHI: Format(instr, "mghi\t'r1,'i1"); break;
      case CHI: Format(instr, "chi\t'r1,'i1"); break;
      case CGHI: Format(instr, "cghi\t'r1,'i1"); break;
      case BRAS: Format(instr, "bras\t'r1,'i1"); break;
      case BRC: Format(instr, "brc\t'm1,'i4"); break;
      case BRCT: Format(instr, "brct\t'r1,'i4"); break;
      case BRCTG: Format(instr, "brctg\t'r1,'i4"); break;
      case IIHH: Format(instr, "iihh\t'r1,'i1"); break;
      case IIHL: Format(instr, "iihl\t'r1,'i1"); break;
      case IILH: Format(instr, "iilh\t'r1,'i1"); break;
      case IILL: Format(instr, "iill\t'r1,'i1"); break;
      case OILL: Format(instr, "oill\t'r1,'i1"); break;
      case TMLL: Format(instr, "tmll\t'r1,'i1"); break;
      case STM: Format(instr, "stm\t'r1,'r2,'d1('r3)"); break;
      case SLL: Format(instr, "sll\t'r1,'d1"); break;
      case SRL: Format(instr, "srl\t'r1,'d1"); break;
      case SLA: Format(instr, "sla\t'r1,'d1"); break;
      case SRA: Format(instr, "sra\t'r1,'d1"); break;
      case SLAG: Format(instr, "slag\t'r1,'r2,'d1('r3)"); break;
      case SRAG: Format(instr, "srag\t'r1,'r2,'d1('r3)"); break;
      case LM: Format(instr, "lm\t'r1,'r2,'d1('r3)"); break;
      case AGR: Format(instr, "agr\t'r5,'r6"); break;
      case SGR: Format(instr, "sgr\t'r5,'r6"); break;
      case NGR: Format(instr, "ngr\t'r5,'r6"); break;
      case NILL: Format(instr, "nill\t'r1,'i1"); break;
      case NILH: Format(instr, "nilh\t'r1,'i1"); break;
      case OGR: Format(instr, "ogr\t'r5,'r6"); break;
      case XGR: Format(instr, "xgr\t'r5,'r6"); break;
      case LTGR: Format(instr, "ltgr\t'r5,'r6"); break;
      case LGR: Format(instr, "lgr\t'r5,'r6"); break;
      case LGBR: Format(instr, "lgbr\t'r5,'r6"); break;
      case LGHR: Format(instr, "lghr\t'r5,'r6"); break;
      case MLR: Format(instr, "mlr\t'r5,'r6"); break;
      case MLGR: Format(instr, "mlgr\t'r5,'r6"); break;
      case ALGR: Format(instr, "algr\t'r5,'r6"); break;
      case SLGR: Format(instr, "slgr\t'r5,'r6"); break;
      case LLHR: Format(instr, "llhr\t'r5,'r6"); break;
      case LLGHR: Format(instr, "llghr\t'r5,'r6"); break;
      case LNGR: Format(instr, "lngr\t'r5,'r6"); break;
      case A: Format(instr, "a\t'r1,'d1('r2d,'r3)"); break;
      case S: Format(instr, "s\t'r1,'d1('r2d,'r3)"); break;
      case M: Format(instr, "m\t'r1,'d1('r2d,'r3)"); break;
      case D: Format(instr, "d\t'r1,'d1('r2d,'r3)"); break;
      case O: Format(instr, "o\t'r1,'d1('r2d,'r3)"); break;
      case L: Format(instr, "l\t'r1,'d1('r2d,'r3)"); break;
      case C: Format(instr, "c\t'r1,'d1('r2d,'r3)"); break;
      case AH: Format(instr, "ah\t'r1,'d1('r2d,'r3)"); break;
      case SH: Format(instr, "sh\t'r1,'d1('r2d,'r3)"); break;
      case MH: Format(instr, "mh\t'r1,'d1('r2d,'r3)"); break;
      case AHY: Format(instr, "ahy\t'r1,'d1('r2d,'r3)"); break;
      case SHY: Format(instr, "shy\t'r1,'d1('r2d,'r3)"); break;
      case LGH: Format(instr, "lgh\t'r1,'d1('r2d,'r3)"); break;
      case AL: Format(instr, "al\t'r1,'d1('r2d,'r3)"); break;
      case SL: Format(instr, "sl\t'r1,'d1('r2d,'r3)"); break;
      case LA: Format(instr, "la\t'r1,'d1('r2d,'r3)"); break;
      case LB: Format(instr, "lb\t'r1,'d1('r2d,'r3)"); break;
      case CH: Format(instr, "ch\t'r1,'d1('r2d,'r3)"); break;
      case CL: Format(instr, "cl\t'r1,'d1('r2d,'r3)"); break;
      case BC: Format(instr, "bc\t'm1,'d1('r2d,'r3)"); break;
      case BCT: Format(instr, "bct\t'r1,'d1('r2d,'r3)"); break;
      case ST: Format(instr, "st\t'r1,'d1('r2d,'r3)"); break;
      case STC: Format(instr, "stc\t'r1,'d1('r2d,'r3)"); break;
      case IC_z: Format(instr, "ic\t'r1,'d1('r2d,'r3)"); break;
      case LD: Format(instr, "ld\t'f1,'d1('r2d,'r3)"); break;
      case STE:Format(instr, "ste\t'f1,'d1('r2d,'r3)"); break;
      case STD:Format(instr, "std\t'f1,'d1('r2d,'r3)"); break;
      case CFDBR: Format(instr, "cfdbr\t'r5,'m2,'f6"); break;
      case CDFBR: Format(instr, "cdfbr\t'f5,'m2,'r6"); break;
      case CFEBR: Format(instr, "cfebr\t'r5,'m2,'f6"); break;
      case CEFBR: Format(instr, "cefbr\t'f5,'m2,'r6"); break;
      case CGDBR: Format(instr, "cgdbr\t'r5,'m2,'f6"); break;
      case CDGBR: Format(instr, "cdgbr\t'f5,'m2,'r6"); break;
      case CDLFBR: Format(instr, "cdlfbr\t'f5,'m2,'r6"); break;
      case CDLGBR: Format(instr, "cdlgbr\t'f5,'m2,'r6"); break;
      case CLFDBR: Format(instr, "clfdbr\t'r5,'m2,'f6"); break;
      case CLGDBR: Format(instr, "clgdbr\t'r5,'m2,'f6"); break;
      case ADBR: Format(instr, "adbr\t'f5,'f6"); break;
      case SDBR: Format(instr, "sdbr\t'f5,'f6"); break;
      case MDBR: Format(instr, "mdbr\t'f5,'f6"); break;
      case DDBR: Format(instr, "ddbr\t'f5,'f6"); break;
      case CDBR: Format(instr, "cdbr\t'f5,'f6"); break;
      case SQDBR: Format(instr, "sqdbr\t'f5,'f6"); break;
      case LNDBR: Format(instr, "lndbr\t'f5,'f6"); break;
      case STH:   Format(instr, "sth\t'r1,'d1('r2d,'r3)"); break;
      case SRDA: Format(instr, "srda\t'r1,'d1"); break;
      // TRAP4 is used in calling to native function. it will not be generated
      // in native code.
      case TRAP4: {
        Format(instr, "trap4"); break;
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
  out_buffer_pos_ += OS::SNPrintF(out_buffer_ + out_buffer_pos_,
                                  "%012llx   ",
                                  instr->InstructionBits<SixByteInstr>());

  Opcode opcode = instr->S390OpcodeValue();
  switch (opcode) {
    case LLILF: Format(instr, "llilf\t'r1,'i7"); break;
    case AFI: Format(instr, "afi\t'r1,'i7"); break;
    case ALFI: Format(instr, "alfi\t'r1,'i7"); break;
    case CLGFI: Format(instr, "clgfi\t'r1,'i7"); break;
    case CLFI: Format(instr, "clfi\t'r1,'i7"); break;
    case CFI: Format(instr, "cfi\t'r1,'i2"); break;
    case BRASL: Format(instr, "brasl\t'r1,'i2"); break;
    case BRCL: Format(instr, "brcl\t'm1,'i5"); break;
    case IIHF: Format(instr, "iihf\t'r1,'i7"); break;
    case IILF: Format(instr, "iilf\t'r1,'i7"); break;
    case XIHF: Format(instr, "xihf\t'r1,'i7"); break;
    case XILF: Format(instr, "xilf\t'r1,'i7"); break;
    case SLLG: Format(instr, "sllg\t'r1,'r2,'d2('r3)"); break;
    case SRLG: Format(instr, "srlg\t'r1,'r2,'d2('r3)"); break;
    case SLAG: Format(instr, "slag\t'r1,'r2,'d2('r3)"); break;
    case SRAG: Format(instr, "srag\t'r1,'r2,'d2('r3)"); break;
    case LMY: Format(instr, "lmy\t'r1,'r2,'d2('r3)"); break;
    case LMG: Format(instr, "lmg\t'r1,'r2,'d2('r3)"); break;
    case STMY: Format(instr, "stmy\t'r1,'r2,'d2('r3)"); break;
    case STMG: Format(instr, "stmg\t'r1,'r2,'d2('r3)"); break;
    case LT: Format(instr, "lt\t'r1,'d2('r2d,'r3)"); break;
    case LTG: Format(instr, "ltg\t'r1,'d2('r2d,'r3)"); break;
    case ML: Format(instr, "ml\t'r1,'d2('r2d,'r3)"); break;
    case AY: Format(instr, "ay\t'r1,'d2('r2d,'r3)"); break;
    case SY: Format(instr, "sy\t'r1,'d2('r2d,'r3)"); break;
    case NY: Format(instr, "ny\t'r1,'d2('r2d,'r3)"); break;
    case OY: Format(instr, "oy\t'r1,'d2('r2d,'r3)"); break;
    case XY: Format(instr, "xy\t'r1,'d2('r2d,'r3)"); break;
    case CY: Format(instr, "cy\t'r1,'d2('r2d,'r3)"); break;
    case AG: Format(instr, "ag\t'r1,'d2('r2d,'r3)"); break;
    case SG: Format(instr, "sg\t'r1,'d2('r2d,'r3)"); break;
    case NG: Format(instr, "ng\t'r1,'d2('r2d,'r3)"); break;
    case OG: Format(instr, "og\t'r1,'d2('r2d,'r3)"); break;
    case XG: Format(instr, "xg\t'r1,'d2('r2d,'r3)"); break;
    case CG: Format(instr, "cg\t'r1,'d2('r2d,'r3)"); break;
    case LG: Format(instr, "lg\t'r1,'d2('r2d,'r3)"); break;
    case LY: Format(instr, "ly\t'r1,'d2('r2d,'r3)"); break;
    case ALY: Format(instr, "aly\t'r1,'d2('r2d,'r3)"); break;
    case ALG: Format(instr, "alg\t'r1,'d2('r2d,'r3)"); break;
    case SLG: Format(instr, "slg\t'r1,'d2('r2d,'r3)"); break;
    case AGF: Format(instr, "agf\t'r1,'d2('r2d,'r3)"); break;
    case SGF: Format(instr, "sgf\t'r1,'d2('r2d,'r3)"); break;
    case SLY: Format(instr, "sly\t'r1,'d2('r2d,'r3)"); break;
    case LLH: Format(instr, "llh\t'r1,'d2('r2d,'r3)"); break;
    case LLGH: Format(instr, "llgh\t'r1,'d2('r2d,'r3)"); break;
    case LLC: Format(instr, "llc\t'r1,'d2('r2d,'r3)"); break;
    case LLGC: Format(instr, "llgc\t'r1,'d2('r2d,'r3)"); break;
    case LAY: Format(instr, "lay\t'r1,'d2('r2d,'r3)"); break;
    case LARL: Format(instr, "larl\t'r1,'i5"); break;
    case LGB: Format(instr, "lgb\t'r1,'d2('r2d,'r3)"); break;
    case CHY: Format(instr, "chy\t'r1,'d2('r2d,'r3)"); break;
    case CLY: Format(instr, "cly\t'r1,'d2('r2d,'r3)"); break;
    case CLG: Format(instr, "clg\t'r1,'d2('r2d,'r3)"); break;
    case BCTG: Format(instr, "bctg\t'r1,'d2('r2d,'r3)"); break;
    case STY: Format(instr, "sty\t'r1,'d2('r2d,'r3)"); break;
    case STG: Format(instr, "stg\t'r1,'d2('r2d,'r3)"); break;
    case ICY: Format(instr, "icy\t'r1,'d2('r2d,'r3)"); break;
    case MVC: Format(instr, "mvc\t'd3('i8,'r3),'d4('r7)"); break;
    case ALGFI: Format(instr, "algfi\t'r1,'i7"); break;
    case SLGFI: Format(instr, "slgfi\t'r1,'i7"); break;
    case SLFI: Format(instr, "slfi\t'r1,'i7"); break;
    case NIHF: Format(instr, "nihf\t'r1,'i7"); break;
    case NILF: Format(instr, "nilf\t'r1,'i7"); break;
    case OIHF: Format(instr, "oihf\t'r1,'i7"); break;
    case OILF: Format(instr, "oilf\t'r1,'i7"); break;
    case LDY: Format(instr, "ldy\t'r1,'d2('r2d,'r3)"); break;
    case STEY: Format(instr, "stey\t'r1,'d2('r2d,'r3)"); break;
    case STDY: Format(instr, "stdy\t'r1,'d2('r2d,'r3)"); break;
    case ADB: Format(instr, "adb\t'r1,'d1('r2d, 'r3)"); break;
    case SDB: Format(instr, "sdb\t'r1,'d1('r2d, 'r3)"); break;
    case MDB: Format(instr, "mdb\t'r1,'d1('r2d, 'r3)"); break;
    case DDB: Format(instr, "ddb\t'r1,'d1('r2d, 'r3)"); break;
    case SQDB: Format(instr, "sqdb\t'r1,'d1('r2d, 'r3)"); break;
    default:
      return false;
  }
  return true;
}

#undef VERIFIY

// Disassemble the instruction at *instr_ptr into the output buffer.
int Decoder::InstructionDecode(byte* instr_ptr) {
  Instruction* instr = Instruction::At(instr_ptr);

  // Try to decode as S390 instruction first.
  bool processed = true;
  int orig_out_buffer_pos_ = out_buffer_pos_;
  int instrLength = instr->InstructionLength();

  if (instrLength == 2)
    processed = DecodeTwoByte(instr);
  else if (instrLength == 4)
    processed = DecodeFourByte(instr);
  else if (instrLength == 6)
    processed = DecodeSixByte(instr);

  // @TODO Remove eventually.
  // if we cannot process as S390, treat it as PPC instr
  if (processed)
    return instrLength;


  // S390 will try to print the bits.  If ppc instruction
  // we'll reset it back to the original position.
  out_buffer_pos_ = orig_out_buffer_pos_;
  // Print raw instruction bytes.
  out_buffer_pos_ += OS::SNPrintF(out_buffer_ + out_buffer_pos_,
                                  "%08x       ",
                                  instr->InstructionBits());

  switch (instr->OpcodeValue() << 26) {
    case TWI: {
      PrintSoftwareInterrupt(instr->SvcValue());
      break;
    }
    case MULLI: {
      UnknownFormat(instr, "mulli");
      break;
    }
    case SUBFIC: {
      Format(instr, "subfic  'rt, 'ra, 'int16");
      break;
    }
    case CMPLI: {
      Format(instr, "cmpli   'ra, 'uint16");
      break;
    }
    case CMPI: {
      Format(instr, "cmpi    'ra, 'int16");
      break;
    }
    case ADDIC: {
      Format(instr, "addic   'rt, 'ra, 'int16");
      break;
    }
    case ADDICx: {
      UnknownFormat(instr, "addicx");
      break;
    }
    case ADDI: {
      if ( instr->RAValue() == 0 ) {
        // this is load immediate
        Format(instr, "li      'rt, 'int16");
      } else {
        Format(instr, "addi    'rt, 'ra, 'int16");
      }
      break;
    }
    case ADDIS: {
      if ( instr->RAValue() == 0 ) {
        Format(instr, "lis     'rt, 'int16");
      } else {
        Format(instr, "addis   'rt, 'ra, 'int16");
      }
      break;
    }
    case BCX: {
      int bo = instr->Bits(25, 21) << 21;
      int bi = instr->Bits(20, 16);
      switch (bi) {
        case 2:
        case 30:
          if (BT == bo) {
            Format(instr, "beq'l'a 'target16");
            break;
          }
          if (BF == bo) {
            Format(instr, "bne'l'a 'target16");
            break;
          }
          Format(instr, "bc'l'a 'target16");
          break;
        case 29:
          if (BT == bo) {
            Format(instr, "bgt'l'a 'target16");
            break;
          }
          if (BF == bo) {
            Format(instr, "ble'l'a 'target16");
            break;
          }
          Format(instr, "bc'l'a 'target16");
          break;
        case 28:
          if (BT == bo) {
            Format(instr, "blt'l'a 'target16");
            break;
          }
          if (BF == bo) {
            Format(instr, "bge'l'a 'target16");
            break;
          }
          Format(instr, "bc'l'a 'target16");
          break;
        default:
          Format(instr, "bc'l'a 'target16");
          break;
      }
      break;
    }
    case SC: {
      UnknownFormat(instr, "sc");
      break;
    }
    case BX: {
      Format(instr, "b'l'a 'target26");
      break;
    }
    case EXT1: {
      DecodeExt1(instr);
      break;
    }
    case RLWIMIX: {
      Format(instr, "rlwimi'. 'ra, 'rs, 'sh, 'me, 'mb");
      break;
    }
    case RLWINMX: {
      Format(instr, "rlwinm'. 'ra, 'rs, 'sh, 'me, 'mb");
      break;
    }
    case RLWNMX: {
      UnknownFormat(instr, "rlwnmx");
      break;
    }
    case XORI: {
      Format(instr, "xori    'ra, 'rs, 'uint16");
      break;
    }
    case XORIS: {
      Format(instr, "xoris   'ra, 'rs, 'uint16");
      break;
    }
    case ANDIx: {
      Format(instr, "andi.   'ra, 'rs, 'uint16");
      break;
    }
    case ANDISx: {
      Format(instr, "andis.  'ra, 'rs, 'uint16");
      break;
    }
    case EXT2: {
      DecodeExt2(instr);
      break;
    }
    case LWZ: {
      Format(instr, "lwz     'rt, 'int16('ra)");
      break;
    }
    case LWZU: {
      Format(instr, "lwzu    'rt, 'int16('ra)");
      break;
    }
    case LBZU: {
      Format(instr, "lbzu    'rt, 'int16('ra)");
      break;
    }
    case LHA: {
      Format(instr, "lha     'rt, 'int16('ra)");
      break;
    }
    case LHAU: {
      Format(instr, "lhau    'rt, 'int16('ra)");
      break;
    }
    case STH_ppc: {
      Format(instr, "sth 'rs, 'int16('ra)");
      break;
    }
    case STHU: {
      Format(instr, "sthu 'rs, 'int16('ra)");
      break;
    }
    case LMW: {
      UnknownFormat(instr, "lmw");
      break;
    }
    case STMW: {
      UnknownFormat(instr, "stmw");
      break;
    }
    case LFS: {
      Format(instr, "lfs     'Dt, 'int16('ra)");
      break;
    }
    case LFSU: {
      Format(instr, "lfsu    'Dt, 'int16('ra)");
      break;
    }
    case LFDU: {
      Format(instr, "lfdu    'Dt, 'int16('ra)");
      break;
    }
    case STFS: {
      Format(instr, "stfs    'Dt, 'int16('ra)");
      break;
    }
    case STFSU: {
      Format(instr, "stfsu   'Dt, 'int16('ra)");
      break;
    }
    case STFDU: {
      Format(instr, "stfdu   'Dt, 'int16('ra)");
      break;
    }
    case EXT3:
    case EXT4: {
      DecodeExt4(instr);
      break;
    }
    case EXT5: {
      DecodeExt5(instr);
      break;
    }
#if V8_TARGET_ARCH_S390X
    case LD_ppc: {
      switch (instr->Bits(1, 0)) {
        case 0:
          Format(instr, "ld      'rt, 'd('ra)");
          break;
        case 1:
          Format(instr, "ldu     'rt, 'd('ra)");
          break;
      }
      break;
    }
    case STD_ppc: {  // could be STD or STDU
      if (instr->Bit(0) == 0) {
        Format(instr, "std     'rs, 'd('ra)");
      } else {
        Format(instr, "stdu    'rs, 'd('ra)");
      }
      break;
    }
#endif

    case FAKE_OPCODE: {
      if (instr->Bits(MARKER_SUBOPCODE_BIT, MARKER_SUBOPCODE_BIT) == 1) {
        int marker_code = instr->Bits(STUB_MARKER_HIGH_BIT, 0);
        ASSERT(marker_code < F_NEXT_AVAILABLE_STUB_MARKER);
        MarkerFormat(instr, "stub-marker ", marker_code);
      } else {
        int fake_opcode = instr->Bits(FAKE_OPCODE_HIGH_BIT, 0);
        MarkerFormat(instr, "faker-opcode ", fake_opcode);
      }
      break;
    }
    default: {
      Unknown(instr);
      break;
    }
  }

  return Instruction::kInstrSize;
}


} }  // namespace v8::internal



//------------------------------------------------------------------------------

namespace disasm {


const char* NameConverter::NameOfAddress(byte* addr) const {
  v8::internal::OS::SNPrintF(tmp_buffer_, "%p", addr);
  return tmp_buffer_.start();
}


const char* NameConverter::NameOfConstant(byte* addr) const {
  return NameOfAddress(addr);
}


const char* NameConverter::NameOfCPURegister(int reg) const {
  return v8::internal::Registers::Name(reg);
}

const char* NameConverter::NameOfByteCPURegister(int reg) const {
  UNREACHABLE();  // PPC does not have the concept of a byte register
  return "nobytereg";
}


const char* NameConverter::NameOfXMMRegister(int reg) const {
  UNREACHABLE();  // PPC does not have any XMM registers
  return "noxmmreg";
}

const char* NameConverter::NameInCode(byte* addr) const {
  // The default name converter is called for unknown code. So we will not try
  // to access any memory.
  return "";
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


// The PPC assembler does not currently use constant pools.
int Disassembler::ConstantPoolSizeAt(byte* instruction) {
  return -1;
}


void Disassembler::Disassemble(FILE* f, byte* begin, byte* end) {
  NameConverter converter;
  Disassembler d(converter);
  for (byte* pc = begin; pc < end;) {
    v8::internal::EmbeddedVector<char, 128> buffer;
    buffer[0] = '\0';
    byte* prev_pc = pc;
    pc += d.InstructionDecode(buffer, pc);
    fprintf(f, "%p    %08x      %s\n",
            prev_pc, *reinterpret_cast<int32_t*>(prev_pc), buffer.start());
  }
}


}  // namespace disasm

#endif  // V8_TARGET_ARCH_S390
