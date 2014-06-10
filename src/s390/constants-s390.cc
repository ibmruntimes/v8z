// Copyright 2009 the V8 project authors. All rights reserved.
//
// Copyright IBM Corp. 2012, 2014. All rights reserved.
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

#include "constants-s390.h"


namespace v8 {
namespace internal {

// These register names are defined in a way to match the native disassembler
// formatting. See for example the command "objdump -d <binary file>".
const char* Registers::names_[kNumRegisters] = {
  "r0", "r1", "r2", "r3", "r4", "r5", "r6", "r7",
  "r8", "r9", "r10", "fp", "ip", "r13", "r14", "sp"
};


// List of alias names which can be used when referring to PPC registers.
const Registers::RegisterAlias Registers::aliases_[] = {
  {10, "sl"},
  {11, "r11"},
  {12, "r12"},
  {13, "r13"},
  {14, "r14"},
  {15, "r15"},
  {kNoRegister, NULL}
};


const char* Registers::Name(int reg) {
  const char* result;
  if ((0 <= reg) && (reg < kNumRegisters)) {
    result = names_[reg];
  } else {
    result = "noreg";
  }
  return result;
}

// Power
const char* FPRegisters::names_[kNumFPRegisters] = {
    "f0", "f1", "f2", "f3", "f4", "f5", "f6", "f7",
    "f8", "f9", "f10", "f11", "f12", "f13", "f14", "f15"
};
const char* FPRegisters::Name(int reg) {
  ASSERT((0 <= reg) && (reg < kNumFPRegisters));
  return names_[reg];
}
int FPRegisters::Number(const char* name) {
  for (int i = 0; i < kNumFPRegisters; i++) {
    if (strcmp(names_[i], name) == 0) {
        return i;
    }
  }

  // No register with the requested name found.
  return kNoRegister;
}

// end of Power

int Registers::Number(const char* name) {
  // Look through the canonical names.
  for (int i = 0; i < kNumRegisters; i++) {
    if (strcmp(names_[i], name) == 0) {
      return i;
    }
  }

  // Look through the alias names.
  int i = 0;
  while (aliases_[i].reg != kNoRegister) {
    if (strcmp(aliases_[i].name, name) == 0) {
      return aliases_[i].reg;
    }
    i++;
  }

  // No register with the requested name found.
  return kNoRegister;
}


} }  // namespace v8::internal

#endif  // V8_TARGET_ARCH_S390
