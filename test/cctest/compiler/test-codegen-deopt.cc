// Copyright 2014 the V8 project authors. All rights reserved.
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.

#include "src/v8.h"
#include "test/cctest/cctest.h"

#include "src/compiler/code-generator.h"
#include "src/compiler/common-operator.h"
#include "src/compiler/graph.h"
#include "src/compiler/instruction-selector.h"
#include "src/compiler/machine-operator.h"
#include "src/compiler/node.h"
#include "src/compiler/operator.h"
#include "src/compiler/raw-machine-assembler.h"
#include "src/compiler/register-allocator.h"
#include "src/compiler/schedule.h"

#include "src/full-codegen.h"
#include "src/parser.h"
#include "src/rewriter.h"

#include "test/cctest/compiler/function-tester.h"

using namespace v8::internal;
using namespace v8::internal::compiler;


#if V8_TURBOFAN_TARGET

typedef RawMachineAssembler::Label MLabel;

static Handle<JSFunction> NewFunction(const char* source) {
  return v8::Utils::OpenHandle(
      *v8::Handle<v8::Function>::Cast(CompileRun(source)));
}


class DeoptCodegenTester {
 public:
  explicit DeoptCodegenTester(HandleAndZoneScope* scope, const char* src)
      : scope_(scope),
        function(NewFunction(src)),
        info(function, scope->main_zone()),
        bailout_id(-1) {
    CHECK(Parser::Parse(&info));
    StrictMode strict_mode = info.function()->strict_mode();
    info.SetStrictMode(strict_mode);
    info.SetOptimizing(BailoutId::None(), Handle<Code>(function->code()));
    CHECK(Rewriter::Rewrite(&info));
    CHECK(Scope::Analyze(&info));
    CHECK_NE(NULL, info.scope());

    FunctionTester::EnsureDeoptimizationSupport(&info);

    DCHECK(info.shared_info()->has_deoptimization_support());

    graph = new (scope_->main_zone()) Graph(scope_->main_zone());
  }

  virtual ~DeoptCodegenTester() { delete code; }

  void GenerateCodeFromSchedule(Schedule* schedule) {
    OFStream os(stdout);
    os << *schedule;

    // Initialize the codegen and generate code.
    Linkage* linkage = new (scope_->main_zone()) Linkage(&info);
    code = new v8::internal::compiler::InstructionSequence(linkage, graph,
                                                           schedule);
    SourcePositionTable source_positions(graph);
    InstructionSelector selector(code, &source_positions);
    selector.SelectInstructions();

    os << "\x2d\x2d\x2d\x2d\x2d\x20\x49\x6e\x73\x74\x72\x75\x63\x74\x69\x6f\x6e\x20\x73\x65\x71\x75\x65\x6e\x63\x65\x20\x62\x65\x66\x6f\x72\x65\x20\x72\x65\x67\x69\x73\x74\x65\x72\x20\x61\x6c\x6c\x6f\x63\x61\x74\x69\x6f\x6e\x20\x2d\x2d\x2d\x2d\x2d\xa"
       << *code;

    RegisterAllocator allocator(code);
    CHECK(allocator.Allocate());

    os << "\x2d\x2d\x2d\x2d\x2d\x20\x49\x6e\x73\x74\x72\x75\x63\x74\x69\x6f\x6e\x20\x73\x65\x71\x75\x65\x6e\x63\x65\x20\x61\x66\x74\x65\x72\x20\x72\x65\x67\x69\x73\x74\x65\x72\x20\x61\x6c\x6c\x6f\x63\x61\x74\x69\x6f\x6e\x20\x2d\x2d\x2d\x2d\x2d\xa"
       << *code;

    compiler::CodeGenerator generator(code);
    result_code = generator.GenerateCode();

#ifdef DEBUG
    result_code->Print();
#endif
  }

  Zone* zone() { return scope_->main_zone(); }

  HandleAndZoneScope* scope_;
  Handle<JSFunction> function;
  CompilationInfo info;
  BailoutId bailout_id;
  Handle<Code> result_code;
  v8::internal::compiler::InstructionSequence* code;
  Graph* graph;
};


class TrivialDeoptCodegenTester : public DeoptCodegenTester {
 public:
  explicit TrivialDeoptCodegenTester(HandleAndZoneScope* scope)
      : DeoptCodegenTester(scope,
                           "\x66\x75\x6e\x63\x74\x69\x6f\x6e\x20\x66\x6f\x6f\x28\x29\x20\x7b\x20\x64\x65\x6f\x70\x74\x28\x29\x3b\x20\x72\x65\x74\x75\x72\x6e\x20\x34\x32\x3b\x20\x7d\x3b\x20\x66\x6f\x6f") {}

  void GenerateCode() {
    GenerateCodeFromSchedule(BuildGraphAndSchedule(graph));
  }

  Schedule* BuildGraphAndSchedule(Graph* graph) {
    Isolate* isolate = info.isolate();
    CommonOperatorBuilder common(zone());

    // Manually construct a schedule for the function below:
    // function foo() {
    //   deopt();
    // }

    MachineType parameter_reps[] = {kMachineTagged};
    MachineCallDescriptorBuilder descriptor_builder(kMachineTagged, 1,
                                                    parameter_reps);

    RawMachineAssembler m(graph, &descriptor_builder);

    Handle<Object> undef_object =
        Handle<Object>(isolate->heap()->undefined_value(), isolate);
    PrintableUnique<Object> undef_constant =
        PrintableUnique<Object>::CreateUninitialized(zone(), undef_object);
    Node* undef_node = m.NewNode(common.HeapConstant(undef_constant));

    Handle<JSFunction> deopt_function =
        NewFunction("\x66\x75\x6e\x63\x74\x69\x6f\x6e\x20\x64\x65\x6f\x70\x74\x28\x29\x20\x7b\x20\x25\x44\x65\x6f\x70\x74\x69\x6d\x69\x7a\x65\x46\x75\x6e\x63\x74\x69\x6f\x6e\x28\x66\x6f\x6f\x29\x3b\x20\x7d\x3b\x20\x64\x65\x6f\x70\x74");
    PrintableUnique<Object> deopt_fun_constant =
        PrintableUnique<Object>::CreateUninitialized(zone(), deopt_function);
    Node* deopt_fun_node = m.NewNode(common.HeapConstant(deopt_fun_constant));

    MLabel deopt, cont;
    Node* call = m.CallJS0(deopt_fun_node, undef_node, &cont, &deopt);

    m.Bind(&cont);
    m.NewNode(common.Continuation(), call);
    m.Return(undef_node);

    m.Bind(&deopt);
    m.NewNode(common.LazyDeoptimization(), call);

    bailout_id = GetCallBailoutId();
    Node* parameters = m.NewNode(common.StateValues(1), undef_node);
    Node* locals = m.NewNode(common.StateValues(0));
    Node* stack = m.NewNode(common.StateValues(0));

    Node* state_node =
        m.NewNode(common.FrameState(bailout_id), parameters, locals, stack);
    m.Deoptimize(state_node);

    // Schedule the graph:
    Schedule* schedule = m.Export();

    cont_block = cont.block();
    deopt_block = deopt.block();

    return schedule;
  }

  BailoutId GetCallBailoutId() {
    ZoneList<Statement*>* body = info.function()->body();
    for (int i = 0; i < body->length(); i++) {
      if (body->at(i)->IsExpressionStatement() &&
          body->at(i)->AsExpressionStatement()->expression()->IsCall()) {
        return body->at(i)->AsExpressionStatement()->expression()->id();
      }
    }
    CHECK(false);
    return BailoutId(-1);
  }

  BasicBlock* cont_block;
  BasicBlock* deopt_block;
};


TEST(TurboTrivialDeoptCodegen) {
  HandleAndZoneScope scope;
  InitializedHandleScope handles;

  FLAG_allow_natives_syntax = true;
  FLAG_turbo_deoptimization = true;

  TrivialDeoptCodegenTester t(&scope);
  t.GenerateCode();

  DeoptimizationInputData* data =
      DeoptimizationInputData::cast(t.result_code->deoptimization_data());

  Label* cont_label = t.code->GetLabel(t.cont_block);
  Label* deopt_label = t.code->GetLabel(t.deopt_block);

  // Check the patch table. It should patch the continuation address to the
  // deoptimization block address.
  CHECK_EQ(1, data->ReturnAddressPatchCount());
  CHECK_EQ(cont_label->pos(), data->ReturnAddressPc(0)->value());
  CHECK_EQ(deopt_label->pos(), data->PatchedAddressPc(0)->value());

  // Check that we deoptimize to the right AST id.
  CHECK_EQ(1, data->DeoptCount());
  CHECK_EQ(1, data->DeoptCount());
  CHECK_EQ(t.bailout_id.ToInt(), data->AstId(0).ToInt());
}


TEST(TurboTrivialDeoptCodegenAndRun) {
  HandleAndZoneScope scope;
  InitializedHandleScope handles;

  FLAG_allow_natives_syntax = true;
  FLAG_turbo_deoptimization = true;

  TrivialDeoptCodegenTester t(&scope);
  t.GenerateCode();

  t.function->ReplaceCode(*t.result_code);
  t.info.context()->native_context()->AddOptimizedCode(*t.result_code);

  Isolate* isolate = scope.main_isolate();
  Handle<Object> result;
  bool has_pending_exception =
      !Execution::Call(isolate, t.function,
                       isolate->factory()->undefined_value(), 0, NULL,
                       false).ToHandle(&result);
  CHECK(!has_pending_exception);
  CHECK(result->SameValue(Smi::FromInt(42)));
}


class TrivialRuntimeDeoptCodegenTester : public DeoptCodegenTester {
 public:
  explicit TrivialRuntimeDeoptCodegenTester(HandleAndZoneScope* scope)
      : DeoptCodegenTester(
            scope,
            "\x66\x75\x6e\x63\x74\x69\x6f\x6e\x20\x66\x6f\x6f\x28\x29\x20\x7b\x20\x25\x44\x65\x6f\x70\x74\x69\x6d\x69\x7a\x65\x46\x75\x6e\x63\x74\x69\x6f\x6e\x28\x66\x6f\x6f\x29\x3b\x20\x72\x65\x74\x75\x72\x6e\x20\x34\x32\x3b\x20\x7d\x3b\x20\x66\x6f\x6f") {}

  void GenerateCode() {
    GenerateCodeFromSchedule(BuildGraphAndSchedule(graph));
  }

  Schedule* BuildGraphAndSchedule(Graph* graph) {
    Isolate* isolate = info.isolate();
    CommonOperatorBuilder common(zone());

    // Manually construct a schedule for the function below:
    // function foo() {
    //   %DeoptimizeFunction(foo);
    // }

    MachineType parameter_reps[] = {kMachineTagged};
    MachineCallDescriptorBuilder descriptor_builder(kMachineTagged, 2,
                                                    parameter_reps);

    RawMachineAssembler m(graph, &descriptor_builder);

    Handle<Object> undef_object =
        Handle<Object>(isolate->heap()->undefined_value(), isolate);
    PrintableUnique<Object> undef_constant =
        PrintableUnique<Object>::CreateUninitialized(zone(), undef_object);
    Node* undef_node = m.NewNode(common.HeapConstant(undef_constant));

    PrintableUnique<Object> this_fun_constant =
        PrintableUnique<Object>::CreateUninitialized(zone(), function);
    Node* this_fun_node = m.NewNode(common.HeapConstant(this_fun_constant));

    MLabel deopt, cont;
    Node* call = m.CallRuntime1(Runtime::kDeoptimizeFunction, this_fun_node,
                                &cont, &deopt);

    m.Bind(&cont);
    m.NewNode(common.Continuation(), call);
    m.Return(undef_node);

    m.Bind(&deopt);
    m.NewNode(common.LazyDeoptimization(), call);

    bailout_id = GetCallBailoutId();
    Node* parameters = m.NewNode(common.StateValues(1), undef_node);
    Node* locals = m.NewNode(common.StateValues(0));
    Node* stack = m.NewNode(common.StateValues(0));

    Node* state_node =
        m.NewNode(common.FrameState(bailout_id), parameters, locals, stack);
    m.Deoptimize(state_node);

    // Schedule the graph:
    Schedule* schedule = m.Export();

    cont_block = cont.block();
    deopt_block = deopt.block();

    return schedule;
  }

  BailoutId GetCallBailoutId() {
    ZoneList<Statement*>* body = info.function()->body();
    for (int i = 0; i < body->length(); i++) {
      if (body->at(i)->IsExpressionStatement() &&
          body->at(i)->AsExpressionStatement()->expression()->IsCallRuntime()) {
        return body->at(i)->AsExpressionStatement()->expression()->id();
      }
    }
    CHECK(false);
    return BailoutId(-1);
  }

  BasicBlock* cont_block;
  BasicBlock* deopt_block;
};


TEST(TurboTrivialRuntimeDeoptCodegenAndRun) {
  HandleAndZoneScope scope;
  InitializedHandleScope handles;

  FLAG_allow_natives_syntax = true;
  FLAG_turbo_deoptimization = true;

  TrivialRuntimeDeoptCodegenTester t(&scope);
  t.GenerateCode();

  t.function->ReplaceCode(*t.result_code);
  t.info.context()->native_context()->AddOptimizedCode(*t.result_code);

  Isolate* isolate = scope.main_isolate();
  Handle<Object> result;
  bool has_pending_exception =
      !Execution::Call(isolate, t.function,
                       isolate->factory()->undefined_value(), 0, NULL,
                       false).ToHandle(&result);
  CHECK(!has_pending_exception);
  CHECK(result->SameValue(Smi::FromInt(42)));
}

#endif
