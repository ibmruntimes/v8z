// Copyright 2013 the V8 project authors. All rights reserved.
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.

#ifndef V8_HYDROGEN_REDUNDANT_PHI_H_
#define V8_HYDROGEN_REDUNDANT_PHI_H_

#include "src/hydrogen.h"

namespace v8 {
namespace internal {


// Replace all phis consisting of a single non-loop operand plus any number of
// loop operands by that single non-loop operand.
class HRedundantPhiEliminationPhase : public HPhase {
 public:
  explicit HRedundantPhiEliminationPhase(HGraph* graph)
      : HPhase("\x48\x5f\x52\x65\x64\x75\x6e\x64\x61\x6e\x74\x20\x70\x68\x69\x20\x65\x6c\x69\x6d\x69\x6e\x61\x74\x69\x6f\x6e", graph) { }

  void Run();
  void ProcessBlock(HBasicBlock* block);

 private:
  void ProcessPhis(const ZoneList<HPhi*>* phis);

  DISALLOW_COPY_AND_ASSIGN(HRedundantPhiEliminationPhase);
};


} }  // namespace v8::internal

#endif  // V8_HYDROGEN_REDUNDANT_PHI_H_
