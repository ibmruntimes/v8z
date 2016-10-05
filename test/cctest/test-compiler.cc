// Copyright 2012 the V8 project authors. All rights reserved.
// Redistribution and use in source and binary forms, with or without
// modification, are permitted provided that the following conditions are
// met:
//
//     * Redistributions of source code must retain the above copyright
//       notice, this list of conditions and the following disclaimer.
//     * Redistributions in binary form must reproduce the above
//       copyright notice, this list of conditions and the following
//       disclaimer in the documentation and/or other materials provided
//       with the distribution.
//     * Neither the name of Google Inc. nor the names of its
//       contributors may be used to endorse or promote products derived
//       from this software without specific prior written permission.
//
// THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
// "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
// LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
// A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
// OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
// SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
// LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
// DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
// THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
// (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
// OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

#include <stdlib.h>
#include <wchar.h>

#include "src/v8.h"

#include "src/compiler.h"
#include "src/disasm.h"
#include "src/parser.h"
#include "test/cctest/cctest.h"

using namespace v8::internal;

static Handle<Object> GetGlobalProperty(const char* name) {
  Isolate* isolate = CcTest::i_isolate();
  return Object::GetProperty(
      isolate, isolate->global_object(), name).ToHandleChecked();
}


static void SetGlobalProperty(const char* name, Object* value) {
  Isolate* isolate = CcTest::i_isolate();
  Handle<Object> object(value, isolate);
  Handle<String> internalized_name =
      isolate->factory()->InternalizeUtf8String(name);
  Handle<JSObject> global(isolate->context()->global_object());
  Runtime::SetObjectProperty(isolate, global, internalized_name, object,
                             SLOPPY).Check();
}


static Handle<JSFunction> Compile(const char* source) {
  Isolate* isolate = CcTest::i_isolate();
  Handle<String> source_code = isolate->factory()->NewStringFromUtf8(
      CStrVector(source)).ToHandleChecked();
  Handle<SharedFunctionInfo> shared_function = Compiler::CompileScript(
      source_code, Handle<String>(), 0, 0, false,
      Handle<Context>(isolate->native_context()), NULL, NULL,
      v8::ScriptCompiler::kNoCompileOptions, NOT_NATIVES_CODE);
  return isolate->factory()->NewFunctionFromSharedFunctionInfo(
      shared_function, isolate->native_context());
}


static double Inc(Isolate* isolate, int x) {
  const char* source = "\x72\x65\x73\x75\x6c\x74\x20\x3d\x20\x6c\x84\x20\x2b\x20\x31\x3b";
  EmbeddedVector<char, 512> buffer;
  SNPrintF(buffer, source, x);

  Handle<JSFunction> fun = Compile(buffer.start());
  if (fun.is_null()) return -1;

  Handle<JSObject> global(isolate->context()->global_object());
  Execution::Call(isolate, fun, global, 0, NULL).Check();
  return GetGlobalProperty("\x72\x65\x73\x75\x6c\x74")->Number();
}


TEST(Inc) {
  CcTest::InitializeVM();
  v8::HandleScope scope(CcTest::isolate());
  CHECK_EQ(4.0, Inc(CcTest::i_isolate(), 3));
}


static double Add(Isolate* isolate, int x, int y) {
  Handle<JSFunction> fun = Compile("\x72\x65\x73\x75\x6c\x74\x20\x3d\x20\x78\x20\x2b\x20\x79\x3b");
  if (fun.is_null()) return -1;

  SetGlobalProperty("\x78", Smi::FromInt(x));
  SetGlobalProperty("\x79", Smi::FromInt(y));
  Handle<JSObject> global(isolate->context()->global_object());
  Execution::Call(isolate, fun, global, 0, NULL).Check();
  return GetGlobalProperty("\x72\x65\x73\x75\x6c\x74")->Number();
}


TEST(Add) {
  CcTest::InitializeVM();
  v8::HandleScope scope(CcTest::isolate());
  CHECK_EQ(5.0, Add(CcTest::i_isolate(), 2, 3));
}


static double Abs(Isolate* isolate, int x) {
  Handle<JSFunction> fun = Compile("\x69\x66\x20\x28\x78\x20\x3c\x20\x30\x29\x20\x72\x65\x73\x75\x6c\x74\x20\x3d\x20\x2d\x78\x3b\x20\x65\x6c\x73\x65\x20\x72\x65\x73\x75\x6c\x74\x20\x3d\x20\x78\x3b");
  if (fun.is_null()) return -1;

  SetGlobalProperty("\x78", Smi::FromInt(x));
  Handle<JSObject> global(isolate->context()->global_object());
  Execution::Call(isolate, fun, global, 0, NULL).Check();
  return GetGlobalProperty("\x72\x65\x73\x75\x6c\x74")->Number();
}


TEST(Abs) {
  CcTest::InitializeVM();
  v8::HandleScope scope(CcTest::isolate());
  CHECK_EQ(3.0, Abs(CcTest::i_isolate(), -3));
}


static double Sum(Isolate* isolate, int n) {
  Handle<JSFunction> fun =
      Compile("\x73\x20\x3d\x20\x30\x3b\x20\x77\x68\x69\x6c\x65\x20\x28\x6e\x20\x3e\x20\x30\x29\x20\x7b\x20\x73\x20\x2b\x3d\x20\x6e\x3b\x20\x6e\x20\x2d\x3d\x20\x31\x3b\x20\x7d\x3b\x20\x72\x65\x73\x75\x6c\x74\x20\x3d\x20\x73\x3b");
  if (fun.is_null()) return -1;

  SetGlobalProperty("\x6e", Smi::FromInt(n));
  Handle<JSObject> global(isolate->context()->global_object());
  Execution::Call(isolate, fun, global, 0, NULL).Check();
  return GetGlobalProperty("\x72\x65\x73\x75\x6c\x74")->Number();
}


TEST(Sum) {
  CcTest::InitializeVM();
  v8::HandleScope scope(CcTest::isolate());
  CHECK_EQ(5050.0, Sum(CcTest::i_isolate(), 100));
}


TEST(Print) {
  v8::HandleScope scope(CcTest::isolate());
  v8::Local<v8::Context> context = CcTest::NewContext(PRINT_EXTENSION);
  v8::Context::Scope context_scope(context);
  const char* source = "\x66\x6f\x72\x20\x28\x6e\x20\x3d\x20\x30\x3b\x20\x6e\x20\x3c\x20\x31\x30\x30\x3b\x20\x2b\x2b\x6e\x29\x20\x70\x72\x69\x6e\x74\x28\x6e\x2c\x20\x31\x2c\x20\x32\x29\x3b";
  Handle<JSFunction> fun = Compile(source);
  if (fun.is_null()) return;
  Handle<JSObject> global(CcTest::i_isolate()->context()->global_object());
  Execution::Call(CcTest::i_isolate(), fun, global, 0, NULL).Check();
}


// The following test method stems from my coding efforts today. It
// tests all the functionality I have added to the compiler today
TEST(Stuff) {
  CcTest::InitializeVM();
  v8::HandleScope scope(CcTest::isolate());
  const char* source =
    "\x72\x20\x3d\x20\x30\x3b\xa"
    "\x61\x20\x3d\x20\x6e\x65\x77\x20\x4f\x62\x6a\x65\x63\x74\x3b\xa"
    "\x69\x66\x20\x28\x61\x20\x3d\x3d\x20\x61\x29\x20\x72\x2b\x3d\x31\x3b\xa"  // 1
    "\x69\x66\x20\x28\x61\x20\x21\x3d\x20\x6e\x65\x77\x20\x4f\x62\x6a\x65\x63\x74\x28\x29\x29\x20\x72\x2b\x3d\x32\x3b\xa"  // 2
    "\x61\x2e\x78\x20\x3d\x20\x34\x32\x3b\xa"
    "\x69\x66\x20\x28\x61\x2e\x78\x20\x3d\x3d\x20\x34\x32\x29\x20\x72\x2b\x3d\x34\x3b\xa"  // 4
    "\x66\x75\x6e\x63\x74\x69\x6f\x6e\x20\x66\x6f\x6f\x28\x29\x20\x7b\x20\x76\x61\x72\x20\x78\x20\x3d\x20\x38\x37\x3b\x20\x72\x65\x74\x75\x72\x6e\x20\x78\x3b\x20\x7d\xa"
    "\x69\x66\x20\x28\x66\x6f\x6f\x28\x29\x20\x3d\x3d\x20\x38\x37\x29\x20\x72\x2b\x3d\x38\x3b\xa"  // 8
    "\x66\x75\x6e\x63\x74\x69\x6f\x6e\x20\x62\x61\x72\x28\x29\x20\x7b\x20\x76\x61\x72\x20\x78\x3b\x20\x78\x20\x3d\x20\x39\x39\x3b\x20\x72\x65\x74\x75\x72\x6e\x20\x78\x3b\x20\x7d\xa"
    "\x69\x66\x20\x28\x62\x61\x72\x28\x29\x20\x3d\x3d\x20\x39\x39\x29\x20\x72\x2b\x3d\x31\x36\x3b\xa"  // 16
    "\x66\x75\x6e\x63\x74\x69\x6f\x6e\x20\x62\x61\x7a\x28\x29\x20\x7b\x20\x76\x61\x72\x20\x78\x20\x3d\x20\x31\x2c\x20\x79\x2c\x20\x7a\x20\x3d\x20\x32\x3b\x20\x79\x20\x3d\x20\x33\x3b\x20\x72\x65\x74\x75\x72\x6e\x20\x78\x20\x2b\x20\x79\x20\x2b\x20\x7a\x3b\x20\x7d\xa"
    "\x69\x66\x20\x28\x62\x61\x7a\x28\x29\x20\x3d\x3d\x20\x36\x29\x20\x72\x2b\x3d\x33\x32\x3b\xa"  // 32
    "\x66\x75\x6e\x63\x74\x69\x6f\x6e\x20\x43\x6f\x6e\x73\x30\x28\x29\x20\x7b\x20\x74\x68\x69\x73\x2e\x78\x20\x3d\x20\x34\x32\x3b\x20\x74\x68\x69\x73\x2e\x79\x20\x3d\x20\x38\x37\x3b\x20\x7d\xa"
    "\x69\x66\x20\x28\x6e\x65\x77\x20\x43\x6f\x6e\x73\x30\x28\x29\x2e\x78\x20\x3d\x3d\x20\x34\x32\x29\x20\x72\x2b\x3d\x36\x34\x3b\xa"  // 64
    "\x69\x66\x20\x28\x6e\x65\x77\x20\x43\x6f\x6e\x73\x30\x28\x29\x2e\x79\x20\x3d\x3d\x20\x38\x37\x29\x20\x72\x2b\x3d\x31\x32\x38\x3b\xa"  // 128
    "\x66\x75\x6e\x63\x74\x69\x6f\x6e\x20\x43\x6f\x6e\x73\x32\x28\x78\x2c\x20\x79\x29\x20\x7b\x20\x74\x68\x69\x73\x2e\x73\x75\x6d\x20\x3d\x20\x78\x20\x2b\x20\x79\x3b\x20\x7d\xa"
    "\x69\x66\x20\x28\x6e\x65\x77\x20\x43\x6f\x6e\x73\x32\x28\x33\x2c\x34\x29\x2e\x73\x75\x6d\x20\x3d\x3d\x20\x37\x29\x20\x72\x2b\x3d\x32\x35\x36\x3b";  // 256

  Handle<JSFunction> fun = Compile(source);
  CHECK(!fun.is_null());
  Handle<JSObject> global(CcTest::i_isolate()->context()->global_object());
  Execution::Call(
      CcTest::i_isolate(), fun, global, 0, NULL).Check();
  CHECK_EQ(511.0, GetGlobalProperty("\x72")->Number());
}


TEST(UncaughtThrow) {
  CcTest::InitializeVM();
  v8::HandleScope scope(CcTest::isolate());

  const char* source = "\x74\x68\x72\x6f\x77\x20\x34\x32\x3b";
  Handle<JSFunction> fun = Compile(source);
  CHECK(!fun.is_null());
  Isolate* isolate = fun->GetIsolate();
  Handle<JSObject> global(isolate->context()->global_object());
  CHECK(Execution::Call(isolate, fun, global, 0, NULL).is_null());
  CHECK_EQ(42.0, isolate->pending_exception()->Number());
}


// Tests calling a builtin function from C/C++ code, and the builtin function
// performs GC. It creates a stack frame looks like following:
//   | C (PerformGC) |
//   |   JS-to-C     |
//   |      JS       |
//   |   C-to-JS     |
TEST(C2JSFrames) {
  FLAG_expose_gc = true;
  v8::HandleScope scope(CcTest::isolate());
  v8::Local<v8::Context> context =
    CcTest::NewContext(PRINT_EXTENSION | GC_EXTENSION);
  v8::Context::Scope context_scope(context);

  const char* source = "\x66\x75\x6e\x63\x74\x69\x6f\x6e\x20\x66\x6f\x6f\x28\x61\x29\x20\x7b\x20\x67\x63\x28\x29\x2c\x20\x70\x72\x69\x6e\x74\x28\x61\x29\x3b\x20\x7d";

  Handle<JSFunction> fun0 = Compile(source);
  CHECK(!fun0.is_null());
  Isolate* isolate = fun0->GetIsolate();

  // Run the generated code to populate the global object with 'foo'.
  Handle<JSObject> global(isolate->context()->global_object());
  Execution::Call(isolate, fun0, global, 0, NULL).Check();

  Handle<String> foo_string = isolate->factory()->InternalizeOneByteString(
      STATIC_ASCII_VECTOR("\x66\x6f\x6f"));
  Handle<Object> fun1 = Object::GetProperty(
      isolate->global_object(), foo_string).ToHandleChecked();
  CHECK(fun1->IsJSFunction());

  Handle<Object> argv[] = { isolate->factory()->InternalizeOneByteString(
      STATIC_ASCII_VECTOR("\x68\x65\x6c\x6c\x6f")) };
  Execution::Call(isolate,
                  Handle<JSFunction>::cast(fun1),
                  global,
                  ARRAY_SIZE(argv),
                  argv).Check();
}


// Regression 236. Calling InitLineEnds on a Script with undefined
// source resulted in crash.
TEST(Regression236) {
  CcTest::InitializeVM();
  Isolate* isolate = CcTest::i_isolate();
  Factory* factory = isolate->factory();
  v8::HandleScope scope(CcTest::isolate());

  Handle<Script> script = factory->NewScript(factory->empty_string());
  script->set_source(CcTest::heap()->undefined_value());
  CHECK_EQ(-1, Script::GetLineNumber(script, 0));
  CHECK_EQ(-1, Script::GetLineNumber(script, 100));
  CHECK_EQ(-1, Script::GetLineNumber(script, -1));
}


TEST(GetScriptLineNumber) {
  LocalContext context;
  v8::HandleScope scope(CcTest::isolate());
  v8::ScriptOrigin origin =
      v8::ScriptOrigin(v8::String::NewFromUtf8(CcTest::isolate(), "\x74\x65\x73\x74"));
  const char function_f[] = "\x66\x75\x6e\x63\x74\x69\x6f\x6e\x20\x66\x28\x29\x20\x7b\x7d";
  const int max_rows = 1000;
  const int buffer_size = max_rows + sizeof(function_f);
  ScopedVector<char> buffer(buffer_size);
  memset(buffer.start(), '\xa', buffer_size - 1);
  buffer[buffer_size - 1] = '\x0';

  for (int i = 0; i < max_rows; ++i) {
    if (i > 0)
      buffer[i - 1] = '\xa';
    MemCopy(&buffer[i], function_f, sizeof(function_f) - 1);
    v8::Handle<v8::String> script_body =
        v8::String::NewFromUtf8(CcTest::isolate(), buffer.start());
    v8::Script::Compile(script_body, &origin)->Run();
    v8::Local<v8::Function> f =
        v8::Local<v8::Function>::Cast(context->Global()->Get(
            v8::String::NewFromUtf8(CcTest::isolate(), "\x66")));
    CHECK_EQ(i, f->GetScriptLineNumber());
  }
}


TEST(FeedbackVectorPreservedAcrossRecompiles) {
  if (i::FLAG_always_opt || !i::FLAG_crankshaft) return;
  i::FLAG_allow_natives_syntax = true;
  CcTest::InitializeVM();
  if (!CcTest::i_isolate()->use_crankshaft()) return;
  v8::HandleScope scope(CcTest::isolate());

  // Make sure function f has a call that uses a type feedback slot.
  CompileRun("\x66\x75\x6e\x63\x74\x69\x6f\x6e\x20\x66\x75\x6e\x28\x29\x20\x7b\x7d\x3b"
             "\x66\x75\x6e\x31\x20\x3d\x20\x66\x75\x6e\x3b"
             "\x66\x75\x6e\x63\x74\x69\x6f\x6e\x20\x66\x28\x61\x29\x20\x7b\x20\x61\x28\x29\x3b\x20\x7d\x20\x66\x28\x66\x75\x6e\x31\x29\x3b");

  Handle<JSFunction> f =
      v8::Utils::OpenHandle(
          *v8::Handle<v8::Function>::Cast(
              CcTest::global()->Get(v8_str("\x66"))));

  // We shouldn't have deoptimization support. We want to recompile and
  // verify that our feedback vector preserves information.
  CHECK(!f->shared()->has_deoptimization_support());
  Handle<FixedArray> feedback_vector(f->shared()->feedback_vector());

  // Verify that we gathered feedback.
  int expected_count = FLAG_vector_ics ? 2 : 1;
  CHECK_EQ(expected_count, feedback_vector->length());
  CHECK(feedback_vector->get(expected_count - 1)->IsJSFunction());

  CompileRun("\x25\x4f\x70\x74\x69\x6d\x69\x7a\x65\x46\x75\x6e\x63\x74\x69\x6f\x6e\x4f\x6e\x4e\x65\x78\x74\x43\x61\x6c\x6c\x28\x66\x29\x3b\x20\x66\x28\x66\x75\x6e\x31\x29\x3b");

  // Verify that the feedback is still "gathered" despite a recompilation
  // of the full code.
  CHECK(f->IsOptimized());
  CHECK(f->shared()->has_deoptimization_support());
  CHECK(f->shared()->feedback_vector()->
        get(expected_count - 1)->IsJSFunction());
}


TEST(FeedbackVectorUnaffectedByScopeChanges) {
  if (i::FLAG_always_opt || !i::FLAG_lazy) return;
  CcTest::InitializeVM();
  v8::HandleScope scope(CcTest::isolate());

  CompileRun("\x66\x75\x6e\x63\x74\x69\x6f\x6e\x20\x62\x75\x69\x6c\x64\x65\x72\x28\x29\x20\x7b"
             "\x20\x20\x63\x61\x6c\x6c\x5f\x74\x61\x72\x67\x65\x74\x20\x3d\x20\x66\x75\x6e\x63\x74\x69\x6f\x6e\x28\x29\x20\x7b\x20\x72\x65\x74\x75\x72\x6e\x20\x33\x3b\x20\x7d\x3b"
             "\x20\x20\x72\x65\x74\x75\x72\x6e\x20\x28\x66\x75\x6e\x63\x74\x69\x6f\x6e\x28\x29\x20\x7b"
             "\x20\x20\x20\x20\x65\x76\x61\x6c\x28\x27\x27\x29\x3b"
             "\x20\x20\x20\x20\x72\x65\x74\x75\x72\x6e\x20\x66\x75\x6e\x63\x74\x69\x6f\x6e\x28\x29\x20\x7b"
             "\x20\x20\x20\x20\x20\x20\x27\x75\x73\x65\x20\x73\x74\x72\x69\x63\x74\x27\x3b"
             "\x20\x20\x20\x20\x20\x20\x63\x61\x6c\x6c\x5f\x74\x61\x72\x67\x65\x74\x28\x29\x3b"
             "\x20\x20\x20\x20\x7d"
             "\x20\x20\x7d\x29\x28\x29\x3b"
             "\x7d"
             "\x6d\x6f\x72\x70\x68\x69\x6e\x67\x5f\x63\x61\x6c\x6c\x20\x3d\x20\x62\x75\x69\x6c\x64\x65\x72\x28\x29\x3b");

  Handle<JSFunction> f =
      v8::Utils::OpenHandle(
          *v8::Handle<v8::Function>::Cast(
              CcTest::global()->Get(v8_str("\x6d\x6f\x72\x70\x68\x69\x6e\x67\x5f\x63\x61\x6c\x6c"))));

  int expected_count = FLAG_vector_ics ? 2 : 1;
  CHECK_EQ(expected_count, f->shared()->feedback_vector()->length());
  // And yet it's not compiled.
  CHECK(!f->shared()->is_compiled());

  CompileRun("\x6d\x6f\x72\x70\x68\x69\x6e\x67\x5f\x63\x61\x6c\x6c\x28\x29\x3b");

  // The vector should have the same size despite the new scoping.
  CHECK_EQ(expected_count, f->shared()->feedback_vector()->length());
  CHECK(f->shared()->is_compiled());
}


// Test that optimized code for different closures is actually shared
// immediately by the FastNewClosureStub when run in the same context.
TEST(OptimizedCodeSharing) {
  // Skip test if --cache-optimized-code is not activated by default because
  // FastNewClosureStub that is baked into the snapshot is incorrect.
  if (!FLAG_cache_optimized_code) return;
  FLAG_stress_compaction = false;
  FLAG_allow_natives_syntax = true;
  CcTest::InitializeVM();
  v8::HandleScope scope(CcTest::isolate());
  for (int i = 0; i < 10; i++) {
    LocalContext env;
    env->Global()->Set(v8::String::NewFromUtf8(CcTest::isolate(), "\x78"),
                       v8::Integer::New(CcTest::isolate(), i));
    CompileRun("\x66\x75\x6e\x63\x74\x69\x6f\x6e\x20\x4d\x61\x6b\x65\x43\x6c\x6f\x73\x75\x72\x65\x28\x29\x20\x7b"
               "\x20\x20\x72\x65\x74\x75\x72\x6e\x20\x66\x75\x6e\x63\x74\x69\x6f\x6e\x28\x29\x20\x7b\x20\x72\x65\x74\x75\x72\x6e\x20\x78\x3b\x20\x7d\x3b"
               "\x7d"
               "\x76\x61\x72\x20\x63\x6c\x6f\x73\x75\x72\x65\x30\x20\x3d\x20\x4d\x61\x6b\x65\x43\x6c\x6f\x73\x75\x72\x65\x28\x29\x3b"
               "\x25\x44\x65\x62\x75\x67\x50\x72\x69\x6e\x74\x28\x63\x6c\x6f\x73\x75\x72\x65\x30\x28\x29\x29\x3b"
               "\x25\x4f\x70\x74\x69\x6d\x69\x7a\x65\x46\x75\x6e\x63\x74\x69\x6f\x6e\x4f\x6e\x4e\x65\x78\x74\x43\x61\x6c\x6c\x28\x63\x6c\x6f\x73\x75\x72\x65\x30\x29\x3b"
               "\x25\x44\x65\x62\x75\x67\x50\x72\x69\x6e\x74\x28\x63\x6c\x6f\x73\x75\x72\x65\x30\x28\x29\x29\x3b"
               "\x76\x61\x72\x20\x63\x6c\x6f\x73\x75\x72\x65\x31\x20\x3d\x20\x4d\x61\x6b\x65\x43\x6c\x6f\x73\x75\x72\x65\x28\x29\x3b"
               "\x76\x61\x72\x20\x63\x6c\x6f\x73\x75\x72\x65\x32\x20\x3d\x20\x4d\x61\x6b\x65\x43\x6c\x6f\x73\x75\x72\x65\x28\x29\x3b");
    Handle<JSFunction> fun1 = v8::Utils::OpenHandle(
        *v8::Local<v8::Function>::Cast(env->Global()->Get(v8_str("\x63\x6c\x6f\x73\x75\x72\x65\x31"))));
    Handle<JSFunction> fun2 = v8::Utils::OpenHandle(
        *v8::Local<v8::Function>::Cast(env->Global()->Get(v8_str("\x63\x6c\x6f\x73\x75\x72\x65\x32"))));
    CHECK(fun1->IsOptimized()
          || !CcTest::i_isolate()->use_crankshaft() || !fun1->IsOptimizable());
    CHECK(fun2->IsOptimized()
          || !CcTest::i_isolate()->use_crankshaft() || !fun2->IsOptimizable());
    CHECK_EQ(fun1->code(), fun2->code());
  }
}


#ifdef ENABLE_DISASSEMBLER
static Handle<JSFunction> GetJSFunction(v8::Handle<v8::Object> obj,
                                 const char* property_name) {
  v8::Local<v8::Function> fun =
      v8::Local<v8::Function>::Cast(obj->Get(v8_str(property_name)));
  return v8::Utils::OpenHandle(*fun);
}


static void CheckCodeForUnsafeLiteral(Handle<JSFunction> f) {
  // Create a disassembler with default name lookup.
  disasm::NameConverter name_converter;
  disasm::Disassembler d(name_converter);

  if (f->code()->kind() == Code::FUNCTION) {
    Address pc = f->code()->instruction_start();
    int decode_size =
        Min(f->code()->instruction_size(),
            static_cast<int>(f->code()->back_edge_table_offset()));
    Address end = pc + decode_size;

    v8::internal::EmbeddedVector<char, 128> decode_buffer;
    v8::internal::EmbeddedVector<char, 128> smi_hex_buffer;
    Smi* smi = Smi::FromInt(12345678);
    SNPrintF(smi_hex_buffer, "\x30\x78\x25" V8PRIxPTR, reinterpret_cast<intptr_t>(smi));
    while (pc < end) {
      int num_const = d.ConstantPoolSizeAt(pc);
      if (num_const >= 0) {
        pc += (num_const + 1) * kPointerSize;
      } else {
        pc += d.InstructionDecode(decode_buffer, pc);
        CHECK(strstr(decode_buffer.start(), smi_hex_buffer.start()) == NULL);
      }
    }
  }
}


TEST(SplitConstantsInFullCompiler) {
  LocalContext context;
  v8::HandleScope scope(CcTest::isolate());

  CompileRun("\x66\x75\x6e\x63\x74\x69\x6f\x6e\x20\x66\x28\x29\x20\x7b\x20\x61\x20\x3d\x20\x31\x32\x33\x34\x35\x36\x37\x38\x20\x7d\x3b\x20\x66\x28\x29\x3b");
  CheckCodeForUnsafeLiteral(GetJSFunction(context->Global(), "\x66"));
  CompileRun("\x66\x75\x6e\x63\x74\x69\x6f\x6e\x20\x66\x28\x78\x29\x20\x7b\x20\x61\x20\x3d\x20\x31\x32\x33\x34\x35\x36\x37\x38\x20\x2b\x20\x78\x7d\x3b\x20\x66\x28\x31\x29\x3b");
  CheckCodeForUnsafeLiteral(GetJSFunction(context->Global(), "\x66"));
  CompileRun("\x66\x75\x6e\x63\x74\x69\x6f\x6e\x20\x66\x28\x78\x29\x20\x7b\x20\x76\x61\x72\x20\x61\x72\x67\x75\x6d\x65\x6e\x74\x73\x20\x3d\x20\x31\x3b\x20\x78\x20\x2b\x3d\x20\x31\x32\x33\x34\x35\x36\x37\x38\x7d\x3b\x20\x66\x28\x31\x29\x3b");
  CheckCodeForUnsafeLiteral(GetJSFunction(context->Global(), "\x66"));
  CompileRun("\x66\x75\x6e\x63\x74\x69\x6f\x6e\x20\x66\x28\x78\x29\x20\x7b\x20\x76\x61\x72\x20\x61\x72\x67\x75\x6d\x65\x6e\x74\x73\x20\x3d\x20\x31\x3b\x20\x78\x20\x3d\x20\x31\x32\x33\x34\x35\x36\x37\x38\x7d\x3b\x20\x66\x28\x31\x29\x3b");
  CheckCodeForUnsafeLiteral(GetJSFunction(context->Global(), "\x66"));
}
#endif
