// Copyright 2013 the V8 project authors. All rights reserved.
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.

#ifndef V8_HYDROGEN_UINT32_ANALYSIS_H_
#define V8_HYDROGEN_UINT32_ANALYSIS_H_

#include "src/hydrogen.h"

namespace v8 {
namespace internal {


// Discover instructions that can be marked with kUint32 flag allowing
// them to produce full range uint32 values.
class HUint32AnalysisPhase : public HPhase {
 public:
  explicit HUint32AnalysisPhase(HGraph* graph)
      : HPhase("\x48\x5f\x43\x6f\x6d\x70\x75\x74\x65\x20\x73\x61\x66\x65\x20\x55\x49\x6e\x74\x33\x32\x20\x6f\x70\x65\x72\x61\x74\x69\x6f\x6e\x73", graph), phis_(4, zone()) { }

  void Run();

 private:
  INLINE(bool IsSafeUint32Use(HValue* val, HValue* use));
  INLINE(bool Uint32UsesAreSafe(HValue* uint32val));
  INLINE(bool CheckPhiOperands(HPhi* phi));
  INLINE(void UnmarkPhi(HPhi* phi, ZoneList<HPhi*>* worklist));
  INLINE(void UnmarkUnsafePhis());

  ZoneList<HPhi*> phis_;
};


} }  // namespace v8::internal

#endif  // V8_HYDROGEN_UINT32_ANALYSIS_H_
