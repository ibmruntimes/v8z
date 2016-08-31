// Copyright 2013 the V8 project authors. All rights reserved.
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.

#ifndef V8_HYDROGEN_BCH_H_
#define V8_HYDROGEN_BCH_H_

#include "src/hydrogen.h"

namespace v8 {
namespace internal {


class HBoundsCheckHoistingPhase : public HPhase {
 public:
  explicit HBoundsCheckHoistingPhase(HGraph* graph)
      : HPhase("\x48\x5f\x42\x6f\x75\x6e\x64\x73\x20\x63\x68\x65\x63\x6b\x73\x20\x68\x6f\x69\x73\x74\x69\x6e\x67", graph) { }

  void Run() {
    HoistRedundantBoundsChecks();
  }

 private:
  void HoistRedundantBoundsChecks();

  DISALLOW_COPY_AND_ASSIGN(HBoundsCheckHoistingPhase);
};


} }  // namespace v8::internal

#endif  // V8_HYDROGEN_BCE_H_
