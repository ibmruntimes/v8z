// Copyright 2013 the V8 project authors. All rights reserved.
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.

#ifndef V8_HYDROGEN_REPRESENTATION_CHANGES_H_
#define V8_HYDROGEN_REPRESENTATION_CHANGES_H_

#include "src/hydrogen.h"

namespace v8 {
namespace internal {


class HRepresentationChangesPhase : public HPhase {
 public:
  explicit HRepresentationChangesPhase(HGraph* graph)
      : HPhase("\x48\x5f\x52\x65\x70\x72\x65\x73\x65\x6e\x74\x61\x74\x69\x6f\x6e\x20\x63\x68\x61\x6e\x67\x65\x73", graph) { }

  void Run();

 private:
  void InsertRepresentationChangeForUse(HValue* value,
                                        HValue* use_value,
                                        int use_index,
                                        Representation to);
  void InsertRepresentationChangesForValue(HValue* value);
};


} }  // namespace v8::internal

#endif  // V8_HYDROGEN_REPRESENTATION_CHANGES_H_
