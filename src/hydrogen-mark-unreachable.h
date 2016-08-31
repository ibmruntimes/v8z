// Copyright 2013 the V8 project authors. All rights reserved.
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.

#ifndef V8_HYDROGEN_MARK_UNREACHABLE_H_
#define V8_HYDROGEN_MARK_UNREACHABLE_H_

#include "src/hydrogen.h"

namespace v8 {
namespace internal {


class HMarkUnreachableBlocksPhase : public HPhase {
 public:
  explicit HMarkUnreachableBlocksPhase(HGraph* graph)
      : HPhase("\x48\x5f\x4d\x61\x72\x6b\x20\x75\x6e\x72\x65\x61\x63\x68\x61\x62\x6c\x65\x20\x62\x6c\x6f\x63\x6b\x73", graph) { }

  void Run();

 private:
  void MarkUnreachableBlocks();

  DISALLOW_COPY_AND_ASSIGN(HMarkUnreachableBlocksPhase);
};


} }  // namespace v8::internal

#endif  // V8_HYDROGEN_MARK_UNREACHABLE_H_
