// Copyright 2013 the V8 project authors. All rights reserved.
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.

#ifndef V8_HYDROGEN_MARK_DEOPTIMIZE_H_
#define V8_HYDROGEN_MARK_DEOPTIMIZE_H_

#include "src/hydrogen.h"

namespace v8 {
namespace internal {


// Compute DeoptimizeOnUndefined flag for phis.  Any phi that can reach a use
// with DeoptimizeOnUndefined set must have DeoptimizeOnUndefined set.
// Currently only HCompareNumericAndBranch, with double input representation,
// has this flag set.  The flag is used by HChange tagged->double, which must
// deoptimize if one of its uses has this flag set.
class HMarkDeoptimizeOnUndefinedPhase : public HPhase {
 public:
  explicit HMarkDeoptimizeOnUndefinedPhase(HGraph* graph)
      : HPhase("\x48\x5f\x4d\x61\x72\x6b\x20\x64\x65\x6f\x70\x74\x69\x6d\x69\x7a\x65\x20\x6f\x6e\x20\x75\x6e\x64\x65\x66\x69\x6e\x65\x64", graph),
        worklist_(16, zone()) {}

  void Run();

 private:
  void ProcessPhi(HPhi* phi);

  // Preallocated worklist used as an optimization so we don't have
  // to allocate a new ZoneList for every ProcessPhi() invocation.
  ZoneList<HPhi*> worklist_;

  DISALLOW_COPY_AND_ASSIGN(HMarkDeoptimizeOnUndefinedPhase);
};


class HComputeChangeUndefinedToNaN : public HPhase {
 public:
  explicit HComputeChangeUndefinedToNaN(HGraph* graph)
      : HPhase("\x48\x5f\x43\x6f\x6d\x70\x75\x74\x65\x20\x63\x68\x61\x6e\x67\x65\x20\x75\x6e\x64\x65\x66\x69\x6e\x65\x64\x20\x74\x6f\x20\x6e\x61\x6e", graph) {}

  void Run();

 private:
  DISALLOW_COPY_AND_ASSIGN(HComputeChangeUndefinedToNaN);
};


} }  // namespace v8::internal

#endif  // V8_HYDROGEN_MARK_DEOPTIMIZE_H_
