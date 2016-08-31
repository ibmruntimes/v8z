// Copyright 2013 the V8 project authors. All rights reserved.
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.

#ifndef V8_HYDROGEN_STORE_ELIMINATION_H_
#define V8_HYDROGEN_STORE_ELIMINATION_H_

#include "src/hydrogen.h"
#include "src/hydrogen-alias-analysis.h"

namespace v8 {
namespace internal {

class HStoreEliminationPhase : public HPhase {
 public:
  explicit HStoreEliminationPhase(HGraph* graph)
    : HPhase("\x48\x5f\x53\x74\x6f\x72\x65\x20\x65\x6c\x69\x6d\x69\x6e\x61\x74\x69\x6f\x6e", graph),
      unobserved_(10, zone()),
      aliasing_() { }

  void Run();
 private:
  ZoneList<HStoreNamedField*> unobserved_;
  HAliasAnalyzer* aliasing_;

  void ProcessStore(HStoreNamedField* store);
  void ProcessLoad(HLoadNamedField* load);
  void ProcessInstr(HInstruction* instr, GVNFlagSet flags);
};


} }  // namespace v8::internal

#endif
