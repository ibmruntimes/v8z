// Copyright 2015 the V8 project authors. All rights reserved.
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.

// CPU specific code for s390 independent of OS goes here.
#include "src/v8.h"

#if V8_TARGET_ARCH_S390

#include "src/assembler.h"
#include "src/macro-assembler.h"
#include "src/simulator.h"  // for cache flushing.

namespace v8 {
namespace internal {

void CpuFeatures::FlushICache(void* buffer, size_t size) {
  // Nothing to do flushing no instructions.
  if (size == 0) {
    return;
  }

#if defined(USE_SIMULATOR)
  // Not generating S390 instructions for C-code. This means that we are
  // building an S390 emulator based target.  We should notify the simulator
  // that the Icache was flushed.
  // None of this code ends up in the snapshot so there are no issues
  // around whether or not to generate the code when building snapshots.
  Simulator::FlushICache(Isolate::Current()->simulator_i_cache(), buffer, size);
#endif  // USE_SIMULATOR
}
}  // namespace internal
}  // namespace v8

#endif  // V8_TARGET_ARCH_S390
