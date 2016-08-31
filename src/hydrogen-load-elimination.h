// Copyright 2013 the V8 project authors. All rights reserved.
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.

#ifndef V8_HYDROGEN_LOAD_ELIMINATION_H_
#define V8_HYDROGEN_LOAD_ELIMINATION_H_

#include "src/hydrogen.h"

namespace v8 {
namespace internal {

class HLoadEliminationPhase : public HPhase {
 public:
  explicit HLoadEliminationPhase(HGraph* graph)
      : HPhase("\x48\x5f\x4c\x6f\x61\x64\x20\x65\x6c\x69\x6d\x69\x6e\x61\x74\x69\x6f\x6e", graph) { }

  void Run();

 private:
  void EliminateLoads(HBasicBlock* block);
};


} }  // namespace v8::internal

#endif  // V8_HYDROGEN_LOAD_ELIMINATION_H_
