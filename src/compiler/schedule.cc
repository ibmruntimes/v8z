// Copyright 2013 the V8 project authors. All rights reserved.
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.

#include "src/compiler/node.h"
#include "src/compiler/node-properties.h"
#include "src/compiler/node-properties-inl.h"
#include "src/compiler/schedule.h"
#include "src/ostreams.h"

namespace v8 {
namespace internal {
namespace compiler {

OStream& operator<<(OStream& os, const BasicBlockData::Control& c) {
  switch (c) {
    case BasicBlockData::kNone:
      return os << "\x6e\x6f\x6e\x65";
    case BasicBlockData::kGoto:
      return os << "\x67\x6f\x74\x6f";
    case BasicBlockData::kBranch:
      return os << "\x62\x72\x61\x6e\x63\x68";
    case BasicBlockData::kReturn:
      return os << "\x72\x65\x74\x75\x72\x6e";
    case BasicBlockData::kThrow:
      return os << "\x74\x68\x72\x6f\x77";
    case BasicBlockData::kCall:
      return os << "\x63\x61\x6c\x6c";
    case BasicBlockData::kDeoptimize:
      return os << "\x64\x65\x6f\x70\x74\x69\x6d\x69\x7a\x65";
  }
  UNREACHABLE();
  return os;
}


OStream& operator<<(OStream& os, const Schedule& s) {
  // TODO(svenpanne) Const-correct the RPO stuff/iterators.
  BasicBlockVector* rpo = const_cast<Schedule*>(&s)->rpo_order();
  for (BasicBlockVectorIter i = rpo->begin(); i != rpo->end(); ++i) {
    BasicBlock* block = *i;
    os << "\x2d\x2d\x2d\x20\x42\x4c\x4f\x43\x4b\x20\x42" << block->id();
    if (block->PredecessorCount() != 0) os << "\x20\x3c\x2d\x20";
    BasicBlock::Predecessors predecessors = block->predecessors();
    bool comma = false;
    for (BasicBlock::Predecessors::iterator j = predecessors.begin();
         j != predecessors.end(); ++j) {
      if (comma) os << "\x2c\x20";
      comma = true;
      os << "\x42" << (*j)->id();
    }
    os << "\x20\x2d\x2d\x2d\xa";
    for (BasicBlock::const_iterator j = block->begin(); j != block->end();
         ++j) {
      Node* node = *j;
      os << "\x20\x20" << *node;
      if (!NodeProperties::IsControl(node)) {
        Bounds bounds = NodeProperties::GetBounds(node);
        os << "\x20\x3a\x20";
        bounds.lower->PrintTo(os);
        if (!bounds.upper->Is(bounds.lower)) {
          os << "\x2e\x2e";
          bounds.upper->PrintTo(os);
        }
      }
      os << "\xa";
    }
    BasicBlock::Control control = block->control_;
    if (control != BasicBlock::kNone) {
      os << "\x20\x20";
      if (block->control_input_ != NULL) {
        os << *block->control_input_;
      } else {
        os << "\x47\x6f\x74\x6f";
      }
      os << "\x20\x2d\x3e\x20";
      BasicBlock::Successors successors = block->successors();
      comma = false;
      for (BasicBlock::Successors::iterator j = successors.begin();
           j != successors.end(); ++j) {
        if (comma) os << "\x2c\x20";
        comma = true;
        os << "\x42" << (*j)->id();
      }
      os << "\xa";
    }
  }
  return os;
}
}  // namespace compiler
}  // namespace internal
}  // namespace v8
