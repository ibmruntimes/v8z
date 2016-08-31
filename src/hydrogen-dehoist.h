// Copyright 2013 the V8 project authors. All rights reserved.
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.

#ifndef V8_HYDROGEN_DEHOIST_H_
#define V8_HYDROGEN_DEHOIST_H_

#include "src/hydrogen.h"

namespace v8 {
namespace internal {


class HDehoistIndexComputationsPhase : public HPhase {
 public:
  explicit HDehoistIndexComputationsPhase(HGraph* graph)
      : HPhase("\x48\x5f\x44\x65\x68\x6f\x69\x73\x74\x20\x69\x6e\x64\x65\x78\x20\x63\x6f\x6d\x70\x75\x74\x61\x74\x69\x6f\x6e\x73", graph) { }

  void Run();

 private:
  DISALLOW_COPY_AND_ASSIGN(HDehoistIndexComputationsPhase);
};


} }  // namespace v8::internal

#endif  // V8_HYDROGEN_DEHOIST_H_
