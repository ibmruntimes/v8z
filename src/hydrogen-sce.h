// Copyright 2013 the V8 project authors. All rights reserved.
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.

#ifndef V8_HYDROGEN_SCE_H_
#define V8_HYDROGEN_SCE_H_

#include "src/hydrogen.h"

namespace v8 {
namespace internal {


class HStackCheckEliminationPhase : public HPhase {
 public:
  explicit HStackCheckEliminationPhase(HGraph* graph)
      : HPhase("\x48\x5f\x53\x74\x61\x63\x6b\x20\x63\x68\x65\x63\x6b\x20\x65\x6c\x69\x6d\x69\x6e\x61\x74\x69\x6f\x6e", graph) { }

  void Run();
};


} }  // namespace v8::internal

#endif  // V8_HYDROGEN_SCE_H_
