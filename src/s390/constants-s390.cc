// Copyright 2015 the V8 project authors. All rights reserved.
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.

#if V8_TARGET_ARCH_S390

#include "src/s390/constants-s390.h"

namespace v8 {
namespace internal {

// These register names are defined in a way to match the native disassembler
// formatting. See for example the command "objdump -d <binary file>".
const char* Registers::names_[kNumRegisters] = {
    u8"r0", u8"r1", u8"r2",  u8"r3", u8"r4", u8"r5",  u8"r6",  u8"r7",
    u8"r8", u8"r9", u8"r10", u8"fp", u8"ip", u8"r13", u8"r14", u8"sp"};

const char* DoubleRegisters::names_[kNumDoubleRegisters] = {
    u8"f0", u8"f1", u8"f2",  u8"f3",  u8"f4",  u8"f5",  u8"f6",  u8"f7",
    u8"f8", u8"f9", u8"f10", u8"f11", u8"f12", u8"f13", u8"f14", u8"f15"};

int DoubleRegisters::Number(const char* name) {
  for (int i = 0; i < kNumDoubleRegisters; i++) {
    if (strcmp(names_[i], name) == 0) {
      return i;
    }
  }

  // No register with the requested name found.
  return kNoRegister;
}

int Registers::Number(const char* name) {
  // Look through the canonical names.
  for (int i = 0; i < kNumRegisters; i++) {
    if (strcmp(names_[i], name) == 0) {
      return i;
    }
  }

  // No register with the requested name found.
  return kNoRegister;
}

}  // namespace internal
}  // namespace v8

#endif  // V8_TARGET_ARCH_S390
