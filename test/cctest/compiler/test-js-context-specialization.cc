// Copyright 2014 the V8 project authors. All rights reserved.
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.

#include "src/compiler/js-context-specialization.h"
#include "src/compiler/js-operator.h"
#include "src/compiler/node-matchers.h"
#include "src/compiler/node-properties-inl.h"
#include "src/compiler/simplified-node-factory.h"
#include "src/compiler/source-position.h"
#include "src/compiler/typer.h"
#include "test/cctest/cctest.h"
#include "test/cctest/compiler/function-tester.h"
#include "test/cctest/compiler/graph-builder-tester.h"

using namespace v8::internal;
using namespace v8::internal::compiler;

class ContextSpecializationTester
    : public HandleAndZoneScope,
      public DirectGraphBuilder,
      public SimplifiedNodeFactory<ContextSpecializationTester> {
 public:
  ContextSpecializationTester()
      : DirectGraphBuilder(new (main_zone()) Graph(main_zone())),
        common_(main_zone()),
        javascript_(main_zone()),
        simplified_(main_zone()),
        typer_(main_zone()),
        jsgraph_(graph(), common(), &typer_),
        info_(main_isolate(), main_zone()) {}

  Factory* factory() { return main_isolate()->factory(); }
  CommonOperatorBuilder* common() { return &common_; }
  JSOperatorBuilder* javascript() { return &javascript_; }
  SimplifiedOperatorBuilder* simplified() { return &simplified_; }
  JSGraph* jsgraph() { return &jsgraph_; }
  CompilationInfo* info() { return &info_; }

 private:
  CommonOperatorBuilder common_;
  JSOperatorBuilder javascript_;
  SimplifiedOperatorBuilder simplified_;
  Typer typer_;
  JSGraph jsgraph_;
  CompilationInfo info_;
};


TEST(ReduceJSLoadContext) {
  ContextSpecializationTester t;

  Node* start = t.NewNode(t.common()->Start(0));
  t.graph()->SetStart(start);

  // Make a context and initialize it a bit for this test.
  Handle<Context> native = t.factory()->NewNativeContext();
  Handle<Context> subcontext1 = t.factory()->NewNativeContext();
  Handle<Context> subcontext2 = t.factory()->NewNativeContext();
  subcontext2->set_previous(*subcontext1);
  subcontext1->set_previous(*native);
  Handle<Object> expected = t.factory()->InternalizeUtf8String("\x67\x62\x6f\x79\x21");
  const int slot = Context::GLOBAL_OBJECT_INDEX;
  native->set(slot, *expected);

  Node* const_context = t.jsgraph()->Constant(native);
  Node* deep_const_context = t.jsgraph()->Constant(subcontext2);
  Node* param_context = t.NewNode(t.common()->Parameter(0), start);
  JSContextSpecializer spec(t.info(), t.jsgraph(), const_context);

  {
    // Mutable slot, constant context, depth = 0 => do nothing.
    Node* load = t.NewNode(t.javascript()->LoadContext(0, 0, false),
                           const_context, const_context, start);
    Reduction r = spec.ReduceJSLoadContext(load);
    CHECK(!r.Changed());
  }

  {
    // Mutable slot, non-constant context, depth = 0 => do nothing.
    Node* load = t.NewNode(t.javascript()->LoadContext(0, 0, false),
                           param_context, param_context, start);
    Reduction r = spec.ReduceJSLoadContext(load);
    CHECK(!r.Changed());
  }

  {
    // Mutable slot, constant context, depth > 0 => fold-in parent context.
    Node* load = t.NewNode(
        t.javascript()->LoadContext(2, Context::GLOBAL_EVAL_FUN_INDEX, false),
        deep_const_context, deep_const_context, start);
    Reduction r = spec.ReduceJSLoadContext(load);
    CHECK(r.Changed());
    Node* new_context_input = NodeProperties::GetValueInput(r.replacement(), 0);
    CHECK_EQ(IrOpcode::kHeapConstant, new_context_input->opcode());
    ValueMatcher<Handle<Context> > match(new_context_input);
    CHECK_EQ(*native, *match.Value());
    ContextAccess access = static_cast<Operator1<ContextAccess>*>(
                               r.replacement()->op())->parameter();
    CHECK_EQ(Context::GLOBAL_EVAL_FUN_INDEX, access.index());
    CHECK_EQ(0, access.depth());
    CHECK_EQ(false, access.immutable());
  }

  {
    // Immutable slot, constant context, depth = 0 => specialize.
    Node* load = t.NewNode(t.javascript()->LoadContext(0, slot, true),
                           const_context, const_context, start);
    Reduction r = spec.ReduceJSLoadContext(load);
    CHECK(r.Changed());
    CHECK(r.replacement() != load);

    ValueMatcher<Handle<Object> > match(r.replacement());
    CHECK(match.HasValue());
    CHECK_EQ(*expected, *match.Value());
  }

  // TODO(titzer): test with other kinds of contexts, e.g. a function context.
  // TODO(sigurds): test that loads below create context are not optimized
}


TEST(ReduceJSStoreContext) {
  ContextSpecializationTester t;

  Node* start = t.NewNode(t.common()->Start(0));
  t.graph()->SetStart(start);

  // Make a context and initialize it a bit for this test.
  Handle<Context> native = t.factory()->NewNativeContext();
  Handle<Context> subcontext1 = t.factory()->NewNativeContext();
  Handle<Context> subcontext2 = t.factory()->NewNativeContext();
  subcontext2->set_previous(*subcontext1);
  subcontext1->set_previous(*native);
  Handle<Object> expected = t.factory()->InternalizeUtf8String("\x67\x62\x6f\x79\x21");
  const int slot = Context::GLOBAL_OBJECT_INDEX;
  native->set(slot, *expected);

  Node* const_context = t.jsgraph()->Constant(native);
  Node* deep_const_context = t.jsgraph()->Constant(subcontext2);
  Node* param_context = t.NewNode(t.common()->Parameter(0), start);
  JSContextSpecializer spec(t.info(), t.jsgraph(), const_context);

  {
    // Mutable slot, constant context, depth = 0 => do nothing.
    Node* load = t.NewNode(t.javascript()->StoreContext(0, 0), const_context,
                           const_context, start);
    Reduction r = spec.ReduceJSStoreContext(load);
    CHECK(!r.Changed());
  }

  {
    // Mutable slot, non-constant context, depth = 0 => do nothing.
    Node* load = t.NewNode(t.javascript()->StoreContext(0, 0), param_context,
                           param_context, start);
    Reduction r = spec.ReduceJSStoreContext(load);
    CHECK(!r.Changed());
  }

  {
    // Immutable slot, constant context, depth = 0 => do nothing.
    Node* load = t.NewNode(t.javascript()->StoreContext(0, slot), const_context,
                           const_context, start);
    Reduction r = spec.ReduceJSStoreContext(load);
    CHECK(!r.Changed());
  }

  {
    // Mutable slot, constant context, depth > 0 => fold-in parent context.
    Node* load = t.NewNode(
        t.javascript()->StoreContext(2, Context::GLOBAL_EVAL_FUN_INDEX),
        deep_const_context, deep_const_context, start);
    Reduction r = spec.ReduceJSStoreContext(load);
    CHECK(r.Changed());
    Node* new_context_input = NodeProperties::GetValueInput(r.replacement(), 0);
    CHECK_EQ(IrOpcode::kHeapConstant, new_context_input->opcode());
    ValueMatcher<Handle<Context> > match(new_context_input);
    CHECK_EQ(*native, *match.Value());
    ContextAccess access = static_cast<Operator1<ContextAccess>*>(
                               r.replacement()->op())->parameter();
    CHECK_EQ(Context::GLOBAL_EVAL_FUN_INDEX, access.index());
    CHECK_EQ(0, access.depth());
    CHECK_EQ(false, access.immutable());
  }
}


// TODO(titzer): factor out common code with effects checking in typed lowering.
static void CheckEffectInput(Node* effect, Node* use) {
  CHECK_EQ(effect, NodeProperties::GetEffectInput(use));
}


TEST(SpecializeToContext) {
  ContextSpecializationTester t;

  Node* start = t.NewNode(t.common()->Start(0));
  t.graph()->SetStart(start);

  // Make a context and initialize it a bit for this test.
  Handle<Context> native = t.factory()->NewNativeContext();
  Handle<Object> expected = t.factory()->InternalizeUtf8String("\x67\x62\x6f\x79\x21");
  const int slot = Context::GLOBAL_OBJECT_INDEX;
  native->set(slot, *expected);
  t.info()->SetContext(native);

  Node* const_context = t.jsgraph()->Constant(native);
  Node* param_context = t.NewNode(t.common()->Parameter(0), start);
  JSContextSpecializer spec(t.info(), t.jsgraph(), const_context);

  {
    // Check that SpecializeToContext() replaces values and forwards effects
    // correctly, and folds values from constant and non-constant contexts
    Node* effect_in = start;
    Node* load = t.NewNode(t.javascript()->LoadContext(0, slot, true),
                           const_context, const_context, effect_in);


    Node* value_use = t.ChangeTaggedToInt32(load);
    Node* other_load = t.NewNode(t.javascript()->LoadContext(0, slot, true),
                                 param_context, param_context, load);
    Node* effect_use = other_load;
    Node* other_use = t.ChangeTaggedToInt32(other_load);

    Node* add = t.NewNode(t.javascript()->Add(), value_use, other_use,
                          param_context, other_load, start);

    Node* ret = t.NewNode(t.common()->Return(), add, effect_use, start);
    Node* end = t.NewNode(t.common()->End(), ret);
    USE(end);
    t.graph()->SetEnd(end);

    // Double check the above graph is what we expect, or the test is broken.
    CheckEffectInput(effect_in, load);
    CheckEffectInput(load, effect_use);

    // Perform the substitution on the entire graph.
    spec.SpecializeToContext();

    // Effects should have been forwarded (not replaced with a value).
    CheckEffectInput(effect_in, effect_use);

    // Use of {other_load} should not have been replaced.
    CHECK_EQ(other_load, other_use->InputAt(0));

    Node* replacement = value_use->InputAt(0);
    ValueMatcher<Handle<Object> > match(replacement);
    CHECK(match.HasValue());
    CHECK_EQ(*expected, *match.Value());
  }
  // TODO(titzer): clean up above test and test more complicated effects.
}


TEST(SpecializeJSFunction_ToConstant1) {
  FunctionTester T(
      "\x28\x66\x75\x6e\x63\x74\x69\x6f\x6e\x28\x29\x20\x7b\x20\x76\x61\x72\x20\x78\x20\x3d\x20\x31\x3b\x20\x66\x75\x6e\x63\x74\x69\x6f\x6e\x20\x69\x6e\x63\x28\x61\x29"
      "\x20\x7b\x20\x72\x65\x74\x75\x72\x6e\x20\x61\x20\x2b\x20\x78\x3b\x20\x7d\x20\x72\x65\x74\x75\x72\x6e\x20\x69\x6e\x63\x3b\x20\x7d\x29\x28\x29");

  T.CheckCall(1.0, 0.0, 0.0);
  T.CheckCall(2.0, 1.0, 0.0);
  T.CheckCall(2.1, 1.1, 0.0);
}


TEST(SpecializeJSFunction_ToConstant2) {
  FunctionTester T(
      "\x28\x66\x75\x6e\x63\x74\x69\x6f\x6e\x28\x29\x20\x7b\x20\x76\x61\x72\x20\x78\x20\x3d\x20\x31\x2e\x35\x3b\x20\x76\x61\x72\x20\x79\x20\x3d\x20\x32\x2e\x32\x35\x3b\x20\x76\x61\x72\x20\x7a\x20\x3d\x20\x33\x2e\x37\x35\x3b"
      "\x20\x66\x75\x6e\x63\x74\x69\x6f\x6e\x20\x66\x28\x61\x29\x20\x7b\x20\x72\x65\x74\x75\x72\x6e\x20\x61\x20\x2d\x20\x78\x20\x2b\x20\x79\x20\x2d\x20\x7a\x3b\x20\x7d\x20\x72\x65\x74\x75\x72\x6e\x20\x66\x3b\x20\x7d\x29\x28\x29");

  T.CheckCall(-3.0, 0.0, 0.0);
  T.CheckCall(-2.0, 1.0, 0.0);
  T.CheckCall(-1.9, 1.1, 0.0);
}


TEST(SpecializeJSFunction_ToConstant3) {
  FunctionTester T(
      "\x28\x66\x75\x6e\x63\x74\x69\x6f\x6e\x28\x29\x20\x7b\x20\x76\x61\x72\x20\x78\x20\x3d\x20\x2d\x31\x31\x2e\x35\x3b\x20\x66\x75\x6e\x63\x74\x69\x6f\x6e\x20\x69\x6e\x63\x28\x29"
      "\x20\x7b\x20\x72\x65\x74\x75\x72\x6e\x20\x28\x66\x75\x6e\x63\x74\x69\x6f\x6e\x28\x61\x29\x20\x7b\x20\x72\x65\x74\x75\x72\x6e\x20\x61\x20\x2b\x20\x78\x3b\x20\x7d\x29\x3b\x20\x7d"
      "\x20\x72\x65\x74\x75\x72\x6e\x20\x69\x6e\x63\x28\x29\x3b\x20\x7d\x29\x28\x29");

  T.CheckCall(-11.5, 0.0, 0.0);
  T.CheckCall(-10.5, 1.0, 0.0);
  T.CheckCall(-10.4, 1.1, 0.0);
}


TEST(SpecializeJSFunction_ToConstant_uninit) {
  {
    FunctionTester T(
        "\x28\x66\x75\x6e\x63\x74\x69\x6f\x6e\x28\x29\x20\x7b\x20\x69\x66\x20\x28\x66\x61\x6c\x73\x65\x29\x20\x7b\x20\x76\x61\x72\x20\x78\x20\x3d\x20\x31\x3b\x20\x7d\x20\x66\x75\x6e\x63\x74\x69\x6f\x6e\x20\x69\x6e\x63\x28\x61\x29"
        "\x20\x7b\x20\x72\x65\x74\x75\x72\x6e\x20\x78\x3b\x20\x7d\x20\x72\x65\x74\x75\x72\x6e\x20\x69\x6e\x63\x3b\x20\x7d\x29\x28\x29");  // x is undefined!

    CHECK(T.Call(T.Val(0.0), T.Val(0.0)).ToHandleChecked()->IsUndefined());
    CHECK(T.Call(T.Val(2.0), T.Val(0.0)).ToHandleChecked()->IsUndefined());
    CHECK(T.Call(T.Val(-2.1), T.Val(0.0)).ToHandleChecked()->IsUndefined());
  }

  {
    FunctionTester T(
        "\x28\x66\x75\x6e\x63\x74\x69\x6f\x6e\x28\x29\x20\x7b\x20\x69\x66\x20\x28\x66\x61\x6c\x73\x65\x29\x20\x7b\x20\x76\x61\x72\x20\x78\x20\x3d\x20\x31\x3b\x20\x7d\x20\x66\x75\x6e\x63\x74\x69\x6f\x6e\x20\x69\x6e\x63\x28\x61\x29"
        "\x20\x7b\x20\x72\x65\x74\x75\x72\x6e\x20\x61\x20\x2b\x20\x78\x3b\x20\x7d\x20\x72\x65\x74\x75\x72\x6e\x20\x69\x6e\x63\x3b\x20\x7d\x29\x28\x29");  // x is undefined!

    CHECK(T.Call(T.Val(0.0), T.Val(0.0)).ToHandleChecked()->IsNaN());
    CHECK(T.Call(T.Val(2.0), T.Val(0.0)).ToHandleChecked()->IsNaN());
    CHECK(T.Call(T.Val(-2.1), T.Val(0.0)).ToHandleChecked()->IsNaN());
  }
}
