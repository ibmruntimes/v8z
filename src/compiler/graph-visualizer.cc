// Copyright 2013 the V8 project authors. All rights reserved.
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.

#include "src/compiler/graph-visualizer.h"

#include "src/compiler/generic-algorithm.h"
#include "src/compiler/generic-node.h"
#include "src/compiler/generic-node-inl.h"
#include "src/compiler/graph.h"
#include "src/compiler/graph-inl.h"
#include "src/compiler/node.h"
#include "src/compiler/node-properties.h"
#include "src/compiler/node-properties-inl.h"
#include "src/compiler/opcodes.h"
#include "src/compiler/operator.h"
#include "src/ostreams.h"

namespace v8 {
namespace internal {
namespace compiler {

#define DEAD_COLOR "\x23\x39\x39\x39\x39\x39\x39"

class GraphVisualizer : public NullNodeVisitor {
 public:
  GraphVisualizer(OStream& os, const Graph* graph);  // NOLINT

  void Print();

  GenericGraphVisit::Control Pre(Node* node);
  GenericGraphVisit::Control PreEdge(Node* from, int index, Node* to);

 private:
  void AnnotateNode(Node* node);
  void PrintEdge(Node* from, int index, Node* to);

  NodeSet all_nodes_;
  NodeSet white_nodes_;
  bool use_to_def_;
  OStream& os_;
  const Graph* const graph_;

  DISALLOW_COPY_AND_ASSIGN(GraphVisualizer);
};


static Node* GetControlCluster(Node* node) {
  if (OperatorProperties::IsBasicBlockBegin(node->op())) {
    return node;
  } else if (OperatorProperties::GetControlInputCount(node->op()) == 1) {
    Node* control = NodeProperties::GetControlInput(node, 0);
    return OperatorProperties::IsBasicBlockBegin(control->op()) ? control
                                                                : NULL;
  } else {
    return NULL;
  }
}


GenericGraphVisit::Control GraphVisualizer::Pre(Node* node) {
  if (all_nodes_.count(node) == 0) {
    Node* control_cluster = GetControlCluster(node);
    if (control_cluster != NULL) {
      os_ << "\x20\x20\x73\x75\x62\x67\x72\x61\x70\x68\x20\x63\x6c\x75\x73\x74\x65\x72\x5f\x42\x61\x73\x69\x63\x42\x6c\x6f\x63\x6b" << control_cluster->id() << "\x20\x7b\xa";
    }
    os_ << "\x20\x20\x49\x44" << node->id() << "\x20\x5b\xa";
    AnnotateNode(node);
    os_ << "\x20\x20\x5d\xa";
    if (control_cluster != NULL) os_ << "\x20\x20\x7d\xa";
    all_nodes_.insert(node);
    if (use_to_def_) white_nodes_.insert(node);
  }
  return GenericGraphVisit::CONTINUE;
}


GenericGraphVisit::Control GraphVisualizer::PreEdge(Node* from, int index,
                                                    Node* to) {
  if (use_to_def_) return GenericGraphVisit::CONTINUE;
  // When going from def to use, only consider white -> other edges, which are
  // the dead nodes that use live nodes. We're probably not interested in
  // dead nodes that only use other dead nodes.
  if (white_nodes_.count(from) > 0) return GenericGraphVisit::CONTINUE;
  return GenericGraphVisit::SKIP;
}


class Escaped {
 public:
  explicit Escaped(const OStringStream& os) : str_(os.c_str()) {}

  friend OStream& operator<<(OStream& os, const Escaped& e) {
    for (const char* s = e.str_; *s != '\x0'; ++s) {
      if (needs_escape(*s)) os << "\\";
      os << *s;
    }
    return os;
  }

 private:
  static bool needs_escape(char ch) {
    switch (ch) {
      case '\x3e':
      case '\x3c':
      case '\x7c':
      case '\x7d':
      case '\x7b':
        return true;
      default:
        return false;
    }
  }

  const char* const str_;
};


static bool IsLikelyBackEdge(Node* from, int index, Node* to) {
  if (from->opcode() == IrOpcode::kPhi ||
      from->opcode() == IrOpcode::kEffectPhi) {
    Node* control = NodeProperties::GetControlInput(from, 0);
    return control->opcode() != IrOpcode::kMerge && control != to && index != 0;
  } else if (from->opcode() == IrOpcode::kLoop) {
    return index != 0;
  } else {
    return false;
  }
}


void GraphVisualizer::AnnotateNode(Node* node) {
  if (!use_to_def_) {
    os_ << "\x20\x20\x20\x20\x73\x74\x79\x6c\x65\x3d\x22\x66\x69\x6c\x6c\x65\x64\x22\xa"
        << "\x20\x20\x20\x20\x66\x69\x6c\x6c\x63\x6f\x6c\x6f\x72\x3d\x22" DEAD_COLOR "\x22\xa";
  }

  os_ << "\x20\x20\x20\x20\x73\x68\x61\x70\x65\x3d\x22\x72\x65\x63\x6f\x72\x64\x22\xa";
  switch (node->opcode()) {
    case IrOpcode::kEnd:
    case IrOpcode::kDead:
    case IrOpcode::kStart:
      os_ << "\x20\x20\x20\x20\x73\x74\x79\x6c\x65\x3d\x22\x64\x69\x61\x67\x6f\x6e\x61\x6c\x73\x22\xa";
      break;
    case IrOpcode::kMerge:
    case IrOpcode::kIfTrue:
    case IrOpcode::kIfFalse:
    case IrOpcode::kLoop:
      os_ << "\x20\x20\x20\x20\x73\x74\x79\x6c\x65\x3d\x22\x72\x6f\x75\x6e\x64\x65\x64\x22\xa";
      break;
    default:
      break;
  }

  OStringStream label;
  label << *node->op();
  os_ << "\x20\x20\x20\x20\x6c\x61\x62\x65\x6c\x3d\x22\x7b\x7b\x23" << node->id() << "\x3a" << Escaped(label);

  InputIter i = node->inputs().begin();
  for (int j = OperatorProperties::GetValueInputCount(node->op()); j > 0;
       ++i, j--) {
    os_ << "\x7c\x3c\x49" << i.index() << "\x3e\x23" << (*i)->id();
  }
  for (int j = OperatorProperties::GetContextInputCount(node->op()); j > 0;
       ++i, j--) {
    os_ << "\x7c\x3c\x49" << i.index() << "\x3e\x58\x20\x23" << (*i)->id();
  }
  for (int j = OperatorProperties::GetEffectInputCount(node->op()); j > 0;
       ++i, j--) {
    os_ << "\x7c\x3c\x49" << i.index() << "\x3e\x45\x20\x23" << (*i)->id();
  }

  if (!use_to_def_ || OperatorProperties::IsBasicBlockBegin(node->op()) ||
      GetControlCluster(node) == NULL) {
    for (int j = OperatorProperties::GetControlInputCount(node->op()); j > 0;
         ++i, j--) {
      os_ << "\x7c\x3c\x49" << i.index() << "\x3e\x43\x20\x23" << (*i)->id();
    }
  }
  os_ << "\x7d";

  if (FLAG_trace_turbo_types && !NodeProperties::IsControl(node)) {
    Bounds bounds = NodeProperties::GetBounds(node);
    OStringStream upper;
    bounds.upper->PrintTo(upper);
    OStringStream lower;
    bounds.lower->PrintTo(lower);
    os_ << "\x7c" << Escaped(upper) << "\x7c" << Escaped(lower);
  }
  os_ << "\x7d\x22\xa";
}


void GraphVisualizer::PrintEdge(Node* from, int index, Node* to) {
  bool unconstrained = IsLikelyBackEdge(from, index, to);
  os_ << "\x20\x20\x49\x44" << from->id();
  if (all_nodes_.count(to) == 0) {
    os_ << "\x3a\x49" << index << "\x3a\x6e\x20\x2d\x3e\x20\x44\x45\x41\x44\x5f\x49\x4e\x50\x55\x54";
  } else if (OperatorProperties::IsBasicBlockBegin(from->op()) ||
             GetControlCluster(from) == NULL ||
             (OperatorProperties::GetControlInputCount(from->op()) > 0 &&
              NodeProperties::GetControlInput(from) != to)) {
    os_ << "\x3a\x49" << index << "\x3a\x6e\x20\x2d\x3e\x20\x49\x44" << to->id() << "\x3a\x73";
    if (unconstrained) os_ << "\x20\x5b\x63\x6f\x6e\x73\x74\x72\x61\x69\x6e\x74\x3d\x66\x61\x6c\x73\x65\x2c\x73\x74\x79\x6c\x65\x3d\x64\x6f\x74\x74\x65\x64\x5d";
  } else {
    os_ << "\x20\x2d\x3e\x20\x49\x44" << to->id() << "\x3a\x73\x20\x5b\x63\x6f\x6c\x6f\x72\x3d\x74\x72\x61\x6e\x73\x70\x61\x72\x65\x6e\x74"
        << (unconstrained ? "\x2c\x20\x63\x6f\x6e\x73\x74\x72\x61\x69\x6e\x74\x3d\x66\x61\x6c\x73\x65" : "") << "\x5d";
  }
  os_ << "\xa";
}


void GraphVisualizer::Print() {
  os_ << "\x64\x69\x67\x72\x61\x70\x68\x20\x44\x20\x7b\xa"
      << "\x20\x20\x6e\x6f\x64\x65\x20\x5b\x66\x6f\x6e\x74\x73\x69\x7a\x65\x3d\x38\x2c\x68\x65\x69\x67\x68\x74\x3d\x30\x2e\x32\x35\x5d\xa"
      << "\x20\x20\x72\x61\x6e\x6b\x64\x69\x72\x3d\x22\x42\x54\x22\xa"
      << "\x20\x20\xa";

  // Make sure all nodes have been output before writing out the edges.
  use_to_def_ = true;
  // TODO(svenpanne) Remove the need for the const_casts.
  const_cast<Graph*>(graph_)->VisitNodeInputsFromEnd(this);
  white_nodes_.insert(const_cast<Graph*>(graph_)->start());

  // Visit all uses of white nodes.
  use_to_def_ = false;
  GenericGraphVisit::Visit<GraphVisualizer, NodeUseIterationTraits<Node> >(
      const_cast<Graph*>(graph_), white_nodes_.begin(), white_nodes_.end(),
      this);

  os_ << "\x20\x20\x44\x45\x41\x44\x5f\x49\x4e\x50\x55\x54\x20\x5b\xa"
      << "\x20\x20\x20\x20\x73\x74\x79\x6c\x65\x3d\x22\x66\x69\x6c\x6c\x65\x64\x22\x20\xa"
      << "\x20\x20\x20\x20\x66\x69\x6c\x6c\x63\x6f\x6c\x6f\x72\x3d\x22" DEAD_COLOR "\x22\xa"
      << "\x20\x20\x5d\xa"
      << "\xa";

  // With all the nodes written, add the edges.
  for (NodeSetIter i = all_nodes_.begin(); i != all_nodes_.end(); ++i) {
    Node::Inputs inputs = (*i)->inputs();
    for (Node::Inputs::iterator iter(inputs.begin()); iter != inputs.end();
         ++iter) {
      PrintEdge(iter.edge().from(), iter.edge().index(), iter.edge().to());
    }
  }
  os_ << "\x7d\xa";
}


GraphVisualizer::GraphVisualizer(OStream& os, const Graph* graph)  // NOLINT
    : all_nodes_(NodeSet::key_compare(),
                 NodeSet::allocator_type(graph->zone())),
      white_nodes_(NodeSet::key_compare(),
                   NodeSet::allocator_type(graph->zone())),
      use_to_def_(true),
      os_(os),
      graph_(graph) {}


OStream& operator<<(OStream& os, const AsDOT& ad) {
  GraphVisualizer(os, &ad.graph).Print();
  return os;
}
}
}
}  // namespace v8::internal::compiler
