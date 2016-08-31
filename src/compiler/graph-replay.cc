// Copyright 2014 the V8 project authors. All rights reserved.
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.

#include "src/compiler/graph-replay.h"

#include "src/compiler/common-operator.h"
#include "src/compiler/graph.h"
#include "src/compiler/graph-inl.h"
#include "src/compiler/node.h"
#include "src/compiler/operator.h"
#include "src/compiler/operator-properties-inl.h"

namespace v8 {
namespace internal {
namespace compiler {

#ifdef DEBUG

void GraphReplayPrinter::PrintReplay(Graph* graph) {
  GraphReplayPrinter replay;
  PrintF("\x20\x20\x4e\x6f\x64\x65\x2a\x20\x6e\x69\x6c\x20\x3d\x20\x67\x72\x61\x70\x68\x2e\x4e\x65\x77\x4e\x6f\x64\x65\x28\x63\x6f\x6d\x6d\x6f\x6e\x5f\x62\x75\x69\x6c\x64\x65\x72\x2e\x44\x65\x61\x64\x28\x29\x29\x3b\xa");
  graph->VisitNodeInputsFromEnd(&replay);
}


GenericGraphVisit::Control GraphReplayPrinter::Pre(Node* node) {
  PrintReplayOpCreator(node->op());
  PrintF("\x20\x20\x4e\x6f\x64\x65\x2a\x20\x6e\x6c\x84\x20\x3d\x20\x67\x72\x61\x70\x68\x2e\x4e\x65\x77\x4e\x6f\x64\x65\x28\x6f\x70", node->id());
  for (int i = 0; i < node->InputCount(); ++i) {
    PrintF("\x2c\x20\x6e\x69\x6c");
  }
  PrintF("\x29\x3b\x20\x55\x53\x45\x28\x6e\x6c\x84\x29\x3b\xa", node->id());
  return GenericGraphVisit::CONTINUE;
}


void GraphReplayPrinter::PostEdge(Node* from, int index, Node* to) {
  PrintF("\x20\x20\x6e\x6c\x84\x2d\x3e\x52\x65\x70\x6c\x61\x63\x65\x49\x6e\x70\x75\x74\x28\x6c\x84\x2c\x20\x6e\x6c\x84\x29\x3b\xa", from->id(), index, to->id());
}


void GraphReplayPrinter::PrintReplayOpCreator(Operator* op) {
  IrOpcode::Value opcode = static_cast<IrOpcode::Value>(op->opcode());
  const char* builder =
      IrOpcode::IsCommonOpcode(opcode) ? "\x63\x6f\x6d\x6d\x6f\x6e\x5f\x62\x75\x69\x6c\x64\x65\x72" : "\x6a\x73\x5f\x62\x75\x69\x6c\x64\x65\x72";
  const char* mnemonic = IrOpcode::IsCommonOpcode(opcode)
                             ? IrOpcode::Mnemonic(opcode)
                             : IrOpcode::Mnemonic(opcode) + 2;
  PrintF("\x20\x20\x6f\x70\x20\x3d\x20\x6c\xa2\x2e\x6c\xa2\x28", builder, mnemonic);
  switch (opcode) {
    case IrOpcode::kParameter:
    case IrOpcode::kNumberConstant:
      PrintF("\x30");
      break;
    case IrOpcode::kLoad:
      PrintF("\x75\x6e\x69\x71\x75\x65\x5f\x6e\x61\x6d\x65");
      break;
    case IrOpcode::kHeapConstant:
      PrintF("\x75\x6e\x69\x71\x75\x65\x5f\x63\x6f\x6e\x73\x74\x61\x6e\x74");
      break;
    case IrOpcode::kPhi:
      PrintF("\x6c\x84", op->InputCount());
      break;
    case IrOpcode::kEffectPhi:
      PrintF("\x6c\x84", OperatorProperties::GetEffectInputCount(op));
      break;
    case IrOpcode::kLoop:
    case IrOpcode::kMerge:
      PrintF("\x6c\x84", OperatorProperties::GetControlInputCount(op));
      break;
    default:
      break;
  }
  PrintF("\x29\x3b\xa");
}

#endif  // DEBUG
}
}
}  // namespace v8::internal::compiler
