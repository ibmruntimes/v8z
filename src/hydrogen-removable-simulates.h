// Copyright 2013 the V8 project authors. All rights reserved.
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.

#ifndef V8_HYDROGEN_REMOVABLE_SIMULATES_H_
#define V8_HYDROGEN_REMOVABLE_SIMULATES_H_

#include "src/hydrogen.h"

namespace v8 {
namespace internal {


class HMergeRemovableSimulatesPhase : public HPhase {
 public:
  explicit HMergeRemovableSimulatesPhase(HGraph* graph)
      : HPhase("\x48\x5f\x4d\x65\x72\x67\x65\x20\x72\x65\x6d\x6f\x76\x61\x62\x6c\x65\x20\x73\x69\x6d\x75\x6c\x61\x74\x65\x73", graph) { }

  void Run();

 private:
  DISALLOW_COPY_AND_ASSIGN(HMergeRemovableSimulatesPhase);
};


} }  // namespace v8::internal

#endif  // V8_HYDROGEN_REMOVABLE_SIMULATES_H_
