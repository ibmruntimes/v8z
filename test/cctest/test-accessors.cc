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

#include "src/v8.h"

#include "src/api.h"
#include "src/frames-inl.h"
#include "src/string-stream.h"
#include "test/cctest/cctest.h"

using ::v8::ObjectTemplate;
using ::v8::Value;
using ::v8::Context;
using ::v8::Local;
using ::v8::String;
using ::v8::Script;
using ::v8::Function;
using ::v8::Extension;

static void handle_property(Local<String> name,
                            const v8::PropertyCallbackInfo<v8::Value>& info) {
  ApiTestFuzzer::Fuzz();
  info.GetReturnValue().Set(v8_num(900));
}

static void handle_property_2(Local<String> name,
                              const v8::PropertyCallbackInfo<v8::Value>& info) {
  ApiTestFuzzer::Fuzz();
  info.GetReturnValue().Set(v8_num(902));
}


static void handle_property(const v8::FunctionCallbackInfo<v8::Value>& info) {
  ApiTestFuzzer::Fuzz();
  CHECK_EQ(0, info.Length());
  info.GetReturnValue().Set(v8_num(907));
}


THREADED_TEST(PropertyHandler) {
  LocalContext env;
  v8::Isolate* isolate = env->GetIsolate();
  v8::HandleScope scope(isolate);
  Local<v8::FunctionTemplate> fun_templ = v8::FunctionTemplate::New(isolate);
  fun_templ->InstanceTemplate()->SetAccessor(v8_str("\x66\x6f\x6f"), handle_property);
  Local<v8::FunctionTemplate> getter_templ =
      v8::FunctionTemplate::New(isolate, handle_property);
  getter_templ->SetLength(0);
  fun_templ->
      InstanceTemplate()->SetAccessorProperty(v8_str("\x62\x61\x72"), getter_templ);
  fun_templ->InstanceTemplate()->
      SetNativeDataProperty(v8_str("\x69\x6e\x73\x74\x61\x6e\x63\x65\x5f\x66\x6f\x6f"), handle_property);
  fun_templ->SetNativeDataProperty(v8_str("\x6f\x62\x6a\x65\x63\x74\x5f\x66\x6f\x6f"), handle_property_2);
  Local<Function> fun = fun_templ->GetFunction();
  env->Global()->Set(v8_str("\x46\x75\x6e"), fun);
  Local<Script> getter;
  Local<Script> setter;
  // check function instance accessors
  getter = v8_compile("\x76\x61\x72\x20\x6f\x62\x6a\x20\x3d\x20\x6e\x65\x77\x20\x46\x75\x6e\x28\x29\x3b\x20\x6f\x62\x6a\x2e\x69\x6e\x73\x74\x61\x6e\x63\x65\x5f\x66\x6f\x6f\x3b");
  CHECK_EQ(900, getter->Run()->Int32Value());
  setter = v8_compile("\x6f\x62\x6a\x2e\x69\x6e\x73\x74\x61\x6e\x63\x65\x5f\x66\x6f\x6f\x20\x3d\x20\x39\x30\x31\x3b");
  CHECK_EQ(901, setter->Run()->Int32Value());
  getter = v8_compile("\x6f\x62\x6a\x2e\x62\x61\x72\x3b");
  CHECK_EQ(907, getter->Run()->Int32Value());
  setter = v8_compile("\x6f\x62\x6a\x2e\x62\x61\x72\x20\x3d\x20\x39\x30\x38\x3b");
  CHECK_EQ(908, setter->Run()->Int32Value());
  // check function static accessors
  getter = v8_compile("\x46\x75\x6e\x2e\x6f\x62\x6a\x65\x63\x74\x5f\x66\x6f\x6f\x3b");
  CHECK_EQ(902, getter->Run()->Int32Value());
  setter = v8_compile("\x46\x75\x6e\x2e\x6f\x62\x6a\x65\x63\x74\x5f\x66\x6f\x6f\x20\x3d\x20\x39\x30\x33\x3b");
  CHECK_EQ(903, setter->Run()->Int32Value());
}


static void GetIntValue(Local<String> property,
                        const v8::PropertyCallbackInfo<v8::Value>& info) {
  ApiTestFuzzer::Fuzz();
  int* value =
      static_cast<int*>(v8::Handle<v8::External>::Cast(info.Data())->Value());
  info.GetReturnValue().Set(v8_num(*value));
}


static void SetIntValue(Local<String> property,
                        Local<Value> value,
                        const v8::PropertyCallbackInfo<void>& info) {
  int* field =
      static_cast<int*>(v8::Handle<v8::External>::Cast(info.Data())->Value());
  *field = value->Int32Value();
}

int foo, bar, baz;

THREADED_TEST(GlobalVariableAccess) {
  foo = 0;
  bar = -4;
  baz = 10;
  v8::Isolate* isolate = CcTest::isolate();
  v8::HandleScope scope(isolate);
  v8::Handle<v8::FunctionTemplate> templ = v8::FunctionTemplate::New(isolate);
  templ->InstanceTemplate()->SetAccessor(
      v8_str("\x66\x6f\x6f"), GetIntValue, SetIntValue,
      v8::External::New(isolate, &foo));
  templ->InstanceTemplate()->SetAccessor(
      v8_str("\x62\x61\x72"), GetIntValue, SetIntValue,
      v8::External::New(isolate, &bar));
  templ->InstanceTemplate()->SetAccessor(
      v8_str("\x62\x61\x7a"), GetIntValue, SetIntValue,
      v8::External::New(isolate, &baz));
  LocalContext env(0, templ->InstanceTemplate());
  v8_compile("\x66\x6f\x6f\x20\x3d\x20\x28\x2b\x2b\x62\x61\x72\x29\x20\x2b\x20\x62\x61\x7a")->Run();
  CHECK_EQ(bar, -3);
  CHECK_EQ(foo, 7);
}


static int x_register[2] = {0, 0};
static v8::Handle<v8::Object> x_receiver;
static v8::Handle<v8::Object> x_holder;

template<class Info>
static void XGetter(const Info& info, int offset) {
  ApiTestFuzzer::Fuzz();
  v8::Isolate* isolate = CcTest::isolate();
  CHECK_EQ(isolate, info.GetIsolate());
  CHECK_EQ(x_receiver, info.This());
  info.GetReturnValue().Set(v8_num(x_register[offset]));
}


static void XGetter(Local<String> name,
                    const v8::PropertyCallbackInfo<v8::Value>& info) {
  CHECK_EQ(x_holder, info.Holder());
  XGetter(info, 0);
}


static void XGetter(const v8::FunctionCallbackInfo<v8::Value>& info) {
  CHECK_EQ(x_receiver, info.Holder());
  XGetter(info, 1);
}


template<class Info>
static void XSetter(Local<Value> value, const Info& info, int offset) {
  v8::Isolate* isolate = CcTest::isolate();
  CHECK_EQ(isolate, info.GetIsolate());
  CHECK_EQ(x_holder, info.This());
  CHECK_EQ(x_holder, info.Holder());
  x_register[offset] = value->Int32Value();
  info.GetReturnValue().Set(v8_num(-1));
}


static void XSetter(Local<String> name,
                    Local<Value> value,
                    const v8::PropertyCallbackInfo<void>& info) {
  XSetter(value, info, 0);
}


static void XSetter(const v8::FunctionCallbackInfo<v8::Value>& info) {
  CHECK_EQ(1, info.Length());
  XSetter(info[0], info, 1);
}


THREADED_TEST(AccessorIC) {
  LocalContext context;
  v8::Isolate* isolate = context->GetIsolate();
  v8::HandleScope scope(isolate);
  v8::Handle<v8::ObjectTemplate> obj = ObjectTemplate::New(isolate);
  obj->SetAccessor(v8_str("\x78\x30"), XGetter, XSetter);
  obj->SetAccessorProperty(v8_str("\x78\x31"),
                           v8::FunctionTemplate::New(isolate, XGetter),
                           v8::FunctionTemplate::New(isolate, XSetter));
  x_holder = obj->NewInstance();
  context->Global()->Set(v8_str("\x68\x6f\x6c\x64\x65\x72"), x_holder);
  x_receiver = v8::Object::New(isolate);
  context->Global()->Set(v8_str("\x6f\x62\x6a"), x_receiver);
  v8::Handle<v8::Array> array = v8::Handle<v8::Array>::Cast(CompileRun(
    "\x6f\x62\x6a\x2e\x5f\x5f\x70\x72\x6f\x74\x6f\x5f\x5f\x20\x3d\x20\x68\x6f\x6c\x64\x65\x72\x3b"
    "\x76\x61\x72\x20\x72\x65\x73\x75\x6c\x74\x20\x3d\x20\x5b\x5d\x3b"
    "\x76\x61\x72\x20\x6b\x65\x79\x5f\x30\x20\x3d\x20\x27\x78\x30\x27\x3b"
    "\x76\x61\x72\x20\x6b\x65\x79\x5f\x31\x20\x3d\x20\x27\x78\x31\x27\x3b"
    "\x66\x6f\x72\x20\x28\x76\x61\x72\x20\x6a\x20\x3d\x20\x30\x3b\x20\x6a\x20\x3c\x20\x31\x30\x3b\x20\x6a\x2b\x2b\x29\x20\x7b"
    "\x20\x20\x76\x61\x72\x20\x69\x20\x3d\x20\x34\x2a\x6a\x3b"
    "\x20\x20\x72\x65\x73\x75\x6c\x74\x2e\x70\x75\x73\x68\x28\x68\x6f\x6c\x64\x65\x72\x2e\x78\x30\x20\x3d\x20\x69\x29\x3b"
    "\x20\x20\x72\x65\x73\x75\x6c\x74\x2e\x70\x75\x73\x68\x28\x6f\x62\x6a\x2e\x78\x30\x29\x3b"
    "\x20\x20\x72\x65\x73\x75\x6c\x74\x2e\x70\x75\x73\x68\x28\x68\x6f\x6c\x64\x65\x72\x2e\x78\x31\x20\x3d\x20\x69\x20\x2b\x20\x31\x29\x3b"
    "\x20\x20\x72\x65\x73\x75\x6c\x74\x2e\x70\x75\x73\x68\x28\x6f\x62\x6a\x2e\x78\x31\x29\x3b"
    "\x20\x20\x72\x65\x73\x75\x6c\x74\x2e\x70\x75\x73\x68\x28\x68\x6f\x6c\x64\x65\x72\x5b\x6b\x65\x79\x5f\x30\x5d\x20\x3d\x20\x69\x20\x2b\x20\x32\x29\x3b"
    "\x20\x20\x72\x65\x73\x75\x6c\x74\x2e\x70\x75\x73\x68\x28\x6f\x62\x6a\x5b\x6b\x65\x79\x5f\x30\x5d\x29\x3b"
    "\x20\x20\x72\x65\x73\x75\x6c\x74\x2e\x70\x75\x73\x68\x28\x68\x6f\x6c\x64\x65\x72\x5b\x6b\x65\x79\x5f\x31\x5d\x20\x3d\x20\x69\x20\x2b\x20\x33\x29\x3b"
    "\x20\x20\x72\x65\x73\x75\x6c\x74\x2e\x70\x75\x73\x68\x28\x6f\x62\x6a\x5b\x6b\x65\x79\x5f\x31\x5d\x29\x3b"
    "\x7d"
    "\x72\x65\x73\x75\x6c\x74"));
  CHECK_EQ(80, array->Length());
  for (int i = 0; i < 80; i++) {
    v8::Handle<Value> entry = array->Get(v8::Integer::New(isolate, i));
    CHECK_EQ(v8::Integer::New(isolate, i/2), entry);
  }
}


template <int C>
static void HandleAllocatingGetter(
    Local<String> name,
    const v8::PropertyCallbackInfo<v8::Value>& info) {
  ApiTestFuzzer::Fuzz();
  for (int i = 0; i < C; i++)
    v8::String::NewFromUtf8(info.GetIsolate(), "\x66\x6f\x6f");
  info.GetReturnValue().Set(v8::String::NewFromUtf8(info.GetIsolate(), "\x66\x6f\x6f"));
}


THREADED_TEST(HandleScopePop) {
  LocalContext context;
  v8::Isolate* isolate = context->GetIsolate();
  v8::HandleScope scope(isolate);
  v8::Handle<v8::ObjectTemplate> obj = ObjectTemplate::New(isolate);
  obj->SetAccessor(v8_str("\x6f\x6e\x65"), HandleAllocatingGetter<1>);
  obj->SetAccessor(v8_str("\x6d\x61\x6e\x79"), HandleAllocatingGetter<1024>);
  v8::Handle<v8::Object> inst = obj->NewInstance();
  context->Global()->Set(v8::String::NewFromUtf8(isolate, "\x6f\x62\x6a"), inst);
  int count_before =
      i::HandleScope::NumberOfHandles(reinterpret_cast<i::Isolate*>(isolate));
  {
    v8::HandleScope scope(isolate);
    CompileRun(
        "\x66\x6f\x72\x20\x28\x76\x61\x72\x20\x69\x20\x3d\x20\x30\x3b\x20\x69\x20\x3c\x20\x31\x30\x30\x30\x3b\x20\x69\x2b\x2b\x29\x20\x7b"
        "\x20\x20\x6f\x62\x6a\x2e\x6f\x6e\x65\x3b"
        "\x20\x20\x6f\x62\x6a\x2e\x6d\x61\x6e\x79\x3b"
        "\x7d");
  }
  int count_after =
      i::HandleScope::NumberOfHandles(reinterpret_cast<i::Isolate*>(isolate));
  CHECK_EQ(count_before, count_after);
}

static void CheckAccessorArgsCorrect(
    Local<String> name,
    const v8::PropertyCallbackInfo<v8::Value>& info) {
  CHECK(info.GetIsolate() == CcTest::isolate());
  CHECK(info.This() == info.Holder());
  CHECK(
      info.Data()->Equals(v8::String::NewFromUtf8(CcTest::isolate(), "\x64\x61\x74\x61")));
  ApiTestFuzzer::Fuzz();
  CHECK(info.GetIsolate() == CcTest::isolate());
  CHECK(info.This() == info.Holder());
  CHECK(
      info.Data()->Equals(v8::String::NewFromUtf8(CcTest::isolate(), "\x64\x61\x74\x61")));
  CcTest::heap()->CollectAllGarbage(i::Heap::kNoGCFlags);
  CHECK(info.GetIsolate() == CcTest::isolate());
  CHECK(info.This() == info.Holder());
  CHECK(
      info.Data()->Equals(v8::String::NewFromUtf8(CcTest::isolate(), "\x64\x61\x74\x61")));
  info.GetReturnValue().Set(17);
}


THREADED_TEST(DirectCall) {
  LocalContext context;
  v8::Isolate* isolate = context->GetIsolate();
  v8::HandleScope scope(isolate);
  v8::Handle<v8::ObjectTemplate> obj = ObjectTemplate::New(isolate);
  obj->SetAccessor(v8_str("\x78\x78\x78"),
                   CheckAccessorArgsCorrect,
                   NULL,
                   v8::String::NewFromUtf8(isolate, "\x64\x61\x74\x61"));
  v8::Handle<v8::Object> inst = obj->NewInstance();
  context->Global()->Set(v8::String::NewFromUtf8(isolate, "\x6f\x62\x6a"),
                         inst);
  Local<Script> scr = v8::Script::Compile(
      v8::String::NewFromUtf8(isolate, "\x6f\x62\x6a\x2e\x78\x78\x78"));
  for (int i = 0; i < 10; i++) {
    Local<Value> result = scr->Run();
    CHECK(!result.IsEmpty());
    CHECK_EQ(17, result->Int32Value());
  }
}

static void EmptyGetter(Local<String> name,
                        const v8::PropertyCallbackInfo<v8::Value>& info) {
  CheckAccessorArgsCorrect(name, info);
  ApiTestFuzzer::Fuzz();
  CheckAccessorArgsCorrect(name, info);
  info.GetReturnValue().Set(v8::Handle<v8::Value>());
}


THREADED_TEST(EmptyResult) {
  LocalContext context;
  v8::Isolate* isolate = context->GetIsolate();
  v8::HandleScope scope(isolate);
  v8::Handle<v8::ObjectTemplate> obj = ObjectTemplate::New(isolate);
  obj->SetAccessor(v8_str("\x78\x78\x78"), EmptyGetter, NULL,
                   v8::String::NewFromUtf8(isolate, "\x64\x61\x74\x61"));
  v8::Handle<v8::Object> inst = obj->NewInstance();
  context->Global()->Set(v8::String::NewFromUtf8(isolate, "\x6f\x62\x6a"), inst);
  Local<Script> scr =
      v8::Script::Compile(v8::String::NewFromUtf8(isolate, "\x6f\x62\x6a\x2e\x78\x78\x78"));
  for (int i = 0; i < 10; i++) {
    Local<Value> result = scr->Run();
    CHECK(result == v8::Undefined(isolate));
  }
}


THREADED_TEST(NoReuseRegress) {
  // Check that the IC generated for the one test doesn't get reused
  // for the other.
  v8::Isolate* isolate = CcTest::isolate();
  v8::HandleScope scope(isolate);
  {
    v8::Handle<v8::ObjectTemplate> obj = ObjectTemplate::New(isolate);
    obj->SetAccessor(v8_str("\x78\x78\x78"), EmptyGetter, NULL,
                     v8::String::NewFromUtf8(isolate, "\x64\x61\x74\x61"));
    LocalContext context;
    v8::Handle<v8::Object> inst = obj->NewInstance();
    context->Global()->Set(v8::String::NewFromUtf8(isolate, "\x6f\x62\x6a"), inst);
    Local<Script> scr =
        v8::Script::Compile(v8::String::NewFromUtf8(isolate, "\x6f\x62\x6a\x2e\x78\x78\x78"));
    for (int i = 0; i < 2; i++) {
      Local<Value> result = scr->Run();
      CHECK(result == v8::Undefined(isolate));
    }
  }
  {
    v8::Handle<v8::ObjectTemplate> obj = ObjectTemplate::New(isolate);
    obj->SetAccessor(v8_str("\x78\x78\x78"),
                     CheckAccessorArgsCorrect,
                     NULL,
                     v8::String::NewFromUtf8(isolate, "\x64\x61\x74\x61"));
    LocalContext context;
    v8::Handle<v8::Object> inst = obj->NewInstance();
    context->Global()->Set(v8::String::NewFromUtf8(isolate, "\x6f\x62\x6a"), inst);
    Local<Script> scr =
        v8::Script::Compile(v8::String::NewFromUtf8(isolate, "\x6f\x62\x6a\x2e\x78\x78\x78"));
    for (int i = 0; i < 10; i++) {
      Local<Value> result = scr->Run();
      CHECK(!result.IsEmpty());
      CHECK_EQ(17, result->Int32Value());
    }
  }
}

static void ThrowingGetAccessor(
    Local<String> name,
    const v8::PropertyCallbackInfo<v8::Value>& info) {
  ApiTestFuzzer::Fuzz();
  info.GetIsolate()->ThrowException(v8_str("\x67"));
}


static void ThrowingSetAccessor(Local<String> name,
                                Local<Value> value,
                                const v8::PropertyCallbackInfo<void>& info) {
  info.GetIsolate()->ThrowException(value);
}


THREADED_TEST(Regress1054726) {
  LocalContext env;
  v8::Isolate* isolate = env->GetIsolate();
  v8::HandleScope scope(isolate);
  v8::Handle<v8::ObjectTemplate> obj = ObjectTemplate::New(isolate);
  obj->SetAccessor(v8_str("\x78"),
                   ThrowingGetAccessor,
                   ThrowingSetAccessor,
                   Local<Value>());

  env->Global()->Set(v8_str("\x6f\x62\x6a"), obj->NewInstance());

  // Use the throwing property setter/getter in a loop to force
  // the accessor ICs to be initialized.
  v8::Handle<Value> result;
  result = Script::Compile(v8_str(
      "\x76\x61\x72\x20\x72\x65\x73\x75\x6c\x74\x20\x3d\x20\x27\x27\x3b"
      "\x66\x6f\x72\x20\x28\x76\x61\x72\x20\x69\x20\x3d\x20\x30\x3b\x20\x69\x20\x3c\x20\x35\x3b\x20\x69\x2b\x2b\x29\x20\x7b"
      "\x20\x20\x74\x72\x79\x20\x7b\x20\x6f\x62\x6a\x2e\x78\x3b\x20\x7d\x20\x63\x61\x74\x63\x68\x20\x28\x65\x29\x20\x7b\x20\x72\x65\x73\x75\x6c\x74\x20\x2b\x3d\x20\x65\x3b\x20\x7d"
      "\x7d\x3b\x20\x72\x65\x73\x75\x6c\x74"))->Run();
  CHECK_EQ(v8_str("\x67\x67\x67\x67\x67"), result);

  result = Script::Compile(String::NewFromUtf8(
      isolate,
      "\x76\x61\x72\x20\x72\x65\x73\x75\x6c\x74\x20\x3d\x20\x27\x27\x3b"
      "\x66\x6f\x72\x20\x28\x76\x61\x72\x20\x69\x20\x3d\x20\x30\x3b\x20\x69\x20\x3c\x20\x35\x3b\x20\x69\x2b\x2b\x29\x20\x7b"
      "\x20\x20\x74\x72\x79\x20\x7b\x20\x6f\x62\x6a\x2e\x78\x20\x3d\x20\x69\x3b\x20\x7d\x20\x63\x61\x74\x63\x68\x20\x28\x65\x29\x20\x7b\x20\x72\x65\x73\x75\x6c\x74\x20\x2b\x3d\x20\x65\x3b\x20\x7d"
      "\x7d\x3b\x20\x72\x65\x73\x75\x6c\x74"))->Run();
  CHECK_EQ(v8_str("\x30\x31\x32\x33\x34"), result);
}


static void AllocGetter(Local<String> name,
                        const v8::PropertyCallbackInfo<v8::Value>& info) {
  ApiTestFuzzer::Fuzz();
  info.GetReturnValue().Set(v8::Array::New(info.GetIsolate(), 1000));
}


THREADED_TEST(Gc) {
  LocalContext env;
  v8::Isolate* isolate = env->GetIsolate();
  v8::HandleScope scope(isolate);
  v8::Handle<v8::ObjectTemplate> obj = ObjectTemplate::New(isolate);
  obj->SetAccessor(v8_str("\x78\x78\x78"), AllocGetter);
  env->Global()->Set(v8_str("\x6f\x62\x6a"), obj->NewInstance());
  Script::Compile(String::NewFromUtf8(
      isolate,
      "\x76\x61\x72\x20\x6c\x61\x73\x74\x20\x3d\x20\x5b\x5d\x3b"
      "\x66\x6f\x72\x20\x28\x76\x61\x72\x20\x69\x20\x3d\x20\x30\x3b\x20\x69\x20\x3c\x20\x32\x30\x34\x38\x3b\x20\x69\x2b\x2b\x29\x20\x7b"
      "\x20\x20\x76\x61\x72\x20\x72\x65\x73\x75\x6c\x74\x20\x3d\x20\x6f\x62\x6a\x2e\x78\x78\x78\x3b"
      "\x20\x20\x72\x65\x73\x75\x6c\x74\x5b\x30\x5d\x20\x3d\x20\x6c\x61\x73\x74\x3b"
      "\x20\x20\x6c\x61\x73\x74\x20\x3d\x20\x72\x65\x73\x75\x6c\x74\x3b"
      "\x7d"))->Run();
}


static void StackCheck(Local<String> name,
                       const v8::PropertyCallbackInfo<v8::Value>& info) {
  i::StackFrameIterator iter(reinterpret_cast<i::Isolate*>(info.GetIsolate()));
  for (int i = 0; !iter.done(); i++) {
    i::StackFrame* frame = iter.frame();
    CHECK(i != 0 || (frame->type() == i::StackFrame::EXIT));
    i::Code* code = frame->LookupCode();
    CHECK(code->IsCode());
    i::Address pc = frame->pc();
    CHECK(code->contains(pc));
    iter.Advance();
  }
}


THREADED_TEST(StackIteration) {
  LocalContext env;
  v8::Isolate* isolate = env->GetIsolate();
  v8::HandleScope scope(isolate);
  v8::Handle<v8::ObjectTemplate> obj = ObjectTemplate::New(isolate);
  i::StringStream::ClearMentionedObjectCache(
      reinterpret_cast<i::Isolate*>(isolate));
  obj->SetAccessor(v8_str("\x78\x78\x78"), StackCheck);
  env->Global()->Set(v8_str("\x6f\x62\x6a"), obj->NewInstance());
  Script::Compile(String::NewFromUtf8(
      isolate,
      "\x66\x75\x6e\x63\x74\x69\x6f\x6e\x20\x66\x6f\x6f\x28\x29\x20\x7b"
      "\x20\x20\x72\x65\x74\x75\x72\x6e\x20\x6f\x62\x6a\x2e\x78\x78\x78\x3b"
      "\x7d"
      "\x66\x6f\x72\x20\x28\x76\x61\x72\x20\x69\x20\x3d\x20\x30\x3b\x20\x69\x20\x3c\x20\x31\x30\x30\x3b\x20\x69\x2b\x2b\x29\x20\x7b"
      "\x20\x20\x66\x6f\x6f\x28\x29\x3b"
      "\x7d"))->Run();
}


static void AllocateHandles(Local<String> name,
                            const v8::PropertyCallbackInfo<v8::Value>& info) {
  for (int i = 0; i < i::kHandleBlockSize + 1; i++) {
    v8::Local<v8::Value>::New(info.GetIsolate(), name);
  }
  info.GetReturnValue().Set(v8::Integer::New(info.GetIsolate(), 100));
}


THREADED_TEST(HandleScopeSegment) {
  // Check that we can return values past popping of handle scope
  // segments.
  LocalContext env;
  v8::Isolate* isolate = env->GetIsolate();
  v8::HandleScope scope(isolate);
  v8::Handle<v8::ObjectTemplate> obj = ObjectTemplate::New(isolate);
  obj->SetAccessor(v8_str("\x78\x78\x78"), AllocateHandles);
  env->Global()->Set(v8_str("\x6f\x62\x6a"), obj->NewInstance());
  v8::Handle<v8::Value> result = Script::Compile(String::NewFromUtf8(
      isolate,
      "\x76\x61\x72\x20\x72\x65\x73\x75\x6c\x74\x3b"
      "\x66\x6f\x72\x20\x28\x76\x61\x72\x20\x69\x20\x3d\x20\x30\x3b\x20\x69\x20\x3c\x20\x34\x3b\x20\x69\x2b\x2b\x29"
      "\x20\x20\x72\x65\x73\x75\x6c\x74\x20\x3d\x20\x6f\x62\x6a\x2e\x78\x78\x78\x3b"
      "\x72\x65\x73\x75\x6c\x74\x3b"))->Run();
  CHECK_EQ(100, result->Int32Value());
}


void JSONStringifyEnumerator(const v8::PropertyCallbackInfo<v8::Array>& info) {
  v8::Handle<v8::Array> array = v8::Array::New(info.GetIsolate(), 1);
  array->Set(0, v8_str("\x72\x65\x67\x72\x65\x73\x73"));
  info.GetReturnValue().Set(array);
}


void JSONStringifyGetter(Local<String> name,
                         const v8::PropertyCallbackInfo<v8::Value>& info) {
  info.GetReturnValue().Set(v8_str("\x63\x72\x62\x75\x67\x2d\x31\x36\x31\x30\x32\x38"));
}


THREADED_TEST(JSONStringifyNamedInterceptorObject) {
  LocalContext env;
  v8::Isolate* isolate = env->GetIsolate();
  v8::HandleScope scope(isolate);

  v8::Handle<v8::ObjectTemplate> obj = ObjectTemplate::New(isolate);
  obj->SetNamedPropertyHandler(
      JSONStringifyGetter, NULL, NULL, NULL, JSONStringifyEnumerator);
  env->Global()->Set(v8_str("\x6f\x62\x6a"), obj->NewInstance());
  v8::Handle<v8::String> expected = v8_str("\x7b\x22\x72\x65\x67\x72\x65\x73\x73\x22\x3a\x22\x63\x72\x62\x75\x67\x2d\x31\x36\x31\x30\x32\x38\x22\x7d");
  CHECK(CompileRun("\x4a\x53\x4f\x4e\x2e\x73\x74\x72\x69\x6e\x67\x69\x66\x79\x28\x6f\x62\x6a\x29")->Equals(expected));
}


static v8::Local<v8::Context> expected_current_context;
static v8::Local<v8::Context> expected_calling_context;


static void check_contexts(const v8::FunctionCallbackInfo<v8::Value>& info) {
  ApiTestFuzzer::Fuzz();
  CHECK(expected_current_context == info.GetIsolate()->GetCurrentContext());
  CHECK(expected_calling_context == info.GetIsolate()->GetCallingContext());
}


THREADED_TEST(AccessorPropertyCrossContext) {
  LocalContext env;
  v8::Isolate* isolate = env->GetIsolate();
  v8::HandleScope scope(isolate);
  v8::Handle<v8::Function> fun = v8::Function::New(isolate, check_contexts);
  LocalContext switch_context;
  switch_context->Global()->Set(v8_str("\x66\x75\x6e"), fun);
  v8::TryCatch try_catch;
  expected_current_context = env.local();
  expected_calling_context = switch_context.local();
  CompileRun(
      "\x76\x61\x72\x20\x6f\x20\x3d\x20\x4f\x62\x6a\x65\x63\x74\x2e\x63\x72\x65\x61\x74\x65\x28\x6e\x75\x6c\x6c\x2c\x20\x7b\x20\x6e\x3a\x20\x7b\x20\x67\x65\x74\x3a\x66\x75\x6e\x20\x7d\x20\x7d\x29\x3b"
      "\x66\x6f\x72\x20\x28\x76\x61\x72\x20\x69\x20\x3d\x20\x30\x3b\x20\x69\x20\x3c\x20\x31\x30\x3b\x20\x69\x2b\x2b\x29\x20\x6f\x2e\x6e\x3b");
  CHECK(!try_catch.HasCaught());
}


THREADED_TEST(GlobalObjectAccessor) {
  LocalContext env;
  v8::Isolate* isolate = env->GetIsolate();
  v8::HandleScope scope(isolate);
  CompileRun(
      "\x76\x61\x72\x20\x73\x65\x74\x5f\x76\x61\x6c\x75\x65\x20\x3d\x20\x31\x3b"
      "\x4f\x62\x6a\x65\x63\x74\x2e\x64\x65\x66\x69\x6e\x65\x50\x72\x6f\x70\x65\x72\x74\x79\x28\x74\x68\x69\x73\x2e\x5f\x5f\x70\x72\x6f\x74\x6f\x5f\x5f\x2c\x20\x27\x78\x27\x2c\x20\x7b"
      "\x20\x20\x20\x20\x67\x65\x74\x20\x3a\x20\x66\x75\x6e\x63\x74\x69\x6f\x6e\x28\x29\x20\x7b\x20\x72\x65\x74\x75\x72\x6e\x20\x74\x68\x69\x73\x3b\x20\x7d\x2c"
      "\x20\x20\x20\x20\x73\x65\x74\x20\x3a\x20\x66\x75\x6e\x63\x74\x69\x6f\x6e\x28\x29\x20\x7b\x20\x73\x65\x74\x5f\x76\x61\x6c\x75\x65\x20\x3d\x20\x74\x68\x69\x73\x3b\x20\x7d"
      "\x7d\x29\x3b"
      "\x66\x75\x6e\x63\x74\x69\x6f\x6e\x20\x67\x65\x74\x74\x65\x72\x28\x29\x20\x7b\x20\x72\x65\x74\x75\x72\x6e\x20\x78\x3b\x20\x7d"
      "\x66\x75\x6e\x63\x74\x69\x6f\x6e\x20\x73\x65\x74\x74\x65\x72\x28\x29\x20\x7b\x20\x78\x20\x3d\x20\x31\x3b\x20\x7d"
      "\x66\x6f\x72\x20\x28\x76\x61\x72\x20\x69\x20\x3d\x20\x30\x3b\x20\x69\x20\x3c\x20\x34\x3b\x20\x69\x2b\x2b\x29\x20\x7b\x20\x67\x65\x74\x74\x65\x72\x28\x29\x3b\x20\x73\x65\x74\x74\x65\x72\x28\x29\x3b\x20\x7d");
  CHECK(v8::Utils::OpenHandle(*CompileRun("\x67\x65\x74\x74\x65\x72\x28\x29"))->IsJSGlobalProxy());
  CHECK(v8::Utils::OpenHandle(*CompileRun("\x73\x65\x74\x5f\x76\x61\x6c\x75\x65"))->IsJSGlobalProxy());
}
