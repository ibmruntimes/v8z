// Copyright 2013 the V8 project authors. All rights reserved.
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

#include <limits.h>

#include "src/v8.h"

#include "src/api.h"
#include "src/base/platform/platform.h"
#include "src/compilation-cache.h"
#include "src/execution.h"
#include "src/isolate.h"
#include "src/parser.h"
#include "src/snapshot.h"
#include "src/unicode-inl.h"
#include "src/utils.h"
#include "test/cctest/cctest.h"

using ::v8::Context;
using ::v8::Extension;
using ::v8::Function;
using ::v8::FunctionTemplate;
using ::v8::Handle;
using ::v8::HandleScope;
using ::v8::Local;
using ::v8::Message;
using ::v8::MessageCallback;
using ::v8::Object;
using ::v8::ObjectTemplate;
using ::v8::Persistent;
using ::v8::Script;
using ::v8::StackTrace;
using ::v8::String;
using ::v8::TryCatch;
using ::v8::Undefined;
using ::v8::V8;
using ::v8::Value;

static void ExpectBoolean(bool expected, Local<Value> result) {
  CHECK(result->IsBoolean());
  CHECK_EQ(expected, result->BooleanValue());
}


static void ExpectInt32(int32_t expected, Local<Value> result) {
  CHECK(result->IsInt32());
  CHECK_EQ(expected, result->Int32Value());
}


static void ExpectNumber(double expected, Local<Value> result) {
  CHECK(result->IsNumber());
  CHECK_EQ(expected, result->NumberValue());
}


static void ExpectUndefined(Local<Value> result) {
  CHECK(result->IsUndefined());
}


// Tests are sorted by order of implementation.

TEST(simple_value) {
  LocalContext env;
  v8::HandleScope scope(env->GetIsolate());
  Local<Value> result = CompileRun("\x30\x78\x32\x37\x31\x38\x32\x38\x3b");
  ExpectInt32(0x271828, result);
}


TEST(global_variable) {
  LocalContext env;
  v8::HandleScope scope(env->GetIsolate());
  Local<Value> result = CompileRun("\x76\x61\x72\x20\x6d\x79\x5f\x67\x6c\x6f\x62\x61\x6c\x5f\x76\x61\x72\x20\x3d\x20\x30\x78\x31\x32\x33\x3b\x20\x6d\x79\x5f\x67\x6c\x6f\x62\x61\x6c\x5f\x76\x61\x72\x3b");
  ExpectInt32(0x123, result);
}


TEST(simple_function_call) {
  LocalContext env;
  v8::HandleScope scope(env->GetIsolate());
  Local<Value> result = CompileRun(
      "\x66\x75\x6e\x63\x74\x69\x6f\x6e\x20\x66\x6f\x6f\x28\x29\x20\x7b\x20\x72\x65\x74\x75\x72\x6e\x20\x30\x78\x33\x31\x34\x3b\x20\x7d"
      "\x66\x6f\x6f\x28\x29\x3b");
  ExpectInt32(0x314, result);
}


TEST(binary_op) {
  LocalContext env;
  v8::HandleScope scope(env->GetIsolate());
  Local<Value> result = CompileRun(
      "\x66\x75\x6e\x63\x74\x69\x6f\x6e\x20\x66\x6f\x6f\x28\x29\x20\x7b"
      "\x20\x20\x76\x61\x72\x20\x61\x20\x3d\x20\x30\x78\x31\x32\x30\x30\x3b"
      "\x20\x20\x76\x61\x72\x20\x62\x20\x3d\x20\x30\x78\x30\x30\x33\x35\x3b"
      "\x20\x20\x72\x65\x74\x75\x72\x6e\x20\x32\x20\x2a\x20\x28\x61\x20\x2b\x20\x62\x20\x2d\x20\x31\x29\x3b"
      "\x7d"
      "\x66\x6f\x6f\x28\x29\x3b");
  ExpectInt32(0x2468, result);
}

static void if_comparison_testcontext_helper(
    char const * op,
    char const * lhs,
    char const * rhs,
    int          expect) {
  char buffer[256];
  snprintf(buffer, sizeof(buffer),
           "\x76\x61\x72\x20\x6c\x68\x73\x20\x3d\x20\x6c\xa2\x3b"
           "\x76\x61\x72\x20\x72\x68\x73\x20\x3d\x20\x6c\xa2\x3b"
           "\x69\x66\x20\x28\x20\x6c\x68\x73\x20\x6c\xa2\x20\x72\x68\x73\x20\x29\x20\x7b\x20\x31\x3b\x20\x7d"
           "\x65\x6c\x73\x65\x20\x7b\x20\x30\x3b\x20\x7d",
           lhs, rhs, op);
  Local<Value> result = CompileRun(buffer);
  ExpectInt32(expect, result);
}

static void if_comparison_effectcontext_helper(
    char const * op,
    char const * lhs,
    char const * rhs,
    int          expect) {
  char buffer[256];
  snprintf(buffer, sizeof(buffer),
           "\x76\x61\x72\x20\x6c\x68\x73\x20\x3d\x20\x6c\xa2\x3b"
           "\x76\x61\x72\x20\x72\x68\x73\x20\x3d\x20\x6c\xa2\x3b"
           "\x76\x61\x72\x20\x74\x65\x73\x74\x20\x3d\x20\x6c\x68\x73\x20\x6c\xa2\x20\x72\x68\x73\x3b"
           "\x69\x66\x20\x28\x20\x74\x65\x73\x74\x20\x29\x20\x7b\x20\x31\x3b\x20\x7d"
           "\x65\x6c\x73\x65\x20\x7b\x20\x30\x3b\x20\x7d",
           lhs, rhs, op);
  Local<Value> result = CompileRun(buffer);
  ExpectInt32(expect, result);
}

static void if_comparison_helper(
    char const * op,
    int          expect_when_lt,
    int          expect_when_eq,
    int          expect_when_gt) {
  // TODO(all): Non-SMI tests.

  if_comparison_testcontext_helper(op, "\x31", "\x33", expect_when_lt);
  if_comparison_testcontext_helper(op, "\x35", "\x35", expect_when_eq);
  if_comparison_testcontext_helper(op, "\x39", "\x37", expect_when_gt);

  if_comparison_effectcontext_helper(op, "\x31", "\x33", expect_when_lt);
  if_comparison_effectcontext_helper(op, "\x35", "\x35", expect_when_eq);
  if_comparison_effectcontext_helper(op, "\x39", "\x37", expect_when_gt);
}


TEST(if_comparison) {
  LocalContext env;
  v8::HandleScope scope(env->GetIsolate());

  if_comparison_helper("\x3c",   1, 0, 0);
  if_comparison_helper("\x3c\x3d",  1, 1, 0);
  if_comparison_helper("\x3d\x3d",  0, 1, 0);
  if_comparison_helper("\x3d\x3d\x3d", 0, 1, 0);
  if_comparison_helper("\x3e\x3d",  0, 1, 1);
  if_comparison_helper("\x3e",   0, 0, 1);
  if_comparison_helper("\x21\x3d",  1, 0, 1);
  if_comparison_helper("\x21\x3d\x3d", 1, 0, 1);
}


TEST(unary_plus) {
  LocalContext env;
  v8::HandleScope scope(env->GetIsolate());
  Local<Value> result;
  // SMI
  result = CompileRun("\x76\x61\x72\x20\x61\x20\x3d\x20\x31\x32\x33\x34\x3b\x20\x2b\x61");
  ExpectInt32(1234, result);
  // Number
  result = CompileRun("\x76\x61\x72\x20\x61\x20\x3d\x20\x31\x32\x33\x34\x2e\x35\x3b\x20\x2b\x61");
  ExpectNumber(1234.5, result);
  // String (SMI)
  result = CompileRun("\x76\x61\x72\x20\x61\x20\x3d\x20\x27\x31\x32\x33\x34\x27\x3b\x20\x2b\x61");
  ExpectInt32(1234, result);
  // String (Number)
  result = CompileRun("\x76\x61\x72\x20\x61\x20\x3d\x20\x27\x31\x32\x33\x34\x2e\x35\x27\x3b\x20\x2b\x61");
  ExpectNumber(1234.5, result);
  // Check side effects.
  result = CompileRun("\x76\x61\x72\x20\x61\x20\x3d\x20\x31\x32\x33\x34\x3b\x20\x2b\x28\x61\x20\x3d\x20\x34\x33\x32\x31\x29\x3b\x20\x61");
  ExpectInt32(4321, result);
}


TEST(unary_minus) {
  LocalContext env;
  v8::HandleScope scope(env->GetIsolate());
  Local<Value> result;
  result = CompileRun("\x76\x61\x72\x20\x61\x20\x3d\x20\x31\x32\x33\x34\x3b\x20\x2d\x61");
  ExpectInt32(-1234, result);
  result = CompileRun("\x76\x61\x72\x20\x61\x20\x3d\x20\x31\x32\x33\x34\x2e\x35\x3b\x20\x2d\x61");
  ExpectNumber(-1234.5, result);
  result = CompileRun("\x76\x61\x72\x20\x61\x20\x3d\x20\x31\x32\x33\x34\x3b\x20\x2d\x28\x61\x20\x3d\x20\x34\x33\x32\x31\x29\x3b\x20\x61");
  ExpectInt32(4321, result);
  result = CompileRun("\x76\x61\x72\x20\x61\x20\x3d\x20\x27\x31\x32\x33\x34\x27\x3b\x20\x2d\x61");
  ExpectInt32(-1234, result);
  result = CompileRun("\x76\x61\x72\x20\x61\x20\x3d\x20\x27\x31\x32\x33\x34\x2e\x35\x27\x3b\x20\x2d\x61");
  ExpectNumber(-1234.5, result);
}


TEST(unary_void) {
  LocalContext env;
  v8::HandleScope scope(env->GetIsolate());
  Local<Value> result;
  result = CompileRun("\x76\x61\x72\x20\x61\x20\x3d\x20\x31\x32\x33\x34\x3b\x20\x76\x6f\x69\x64\x20\x28\x61\x29\x3b");
  ExpectUndefined(result);
  result = CompileRun("\x76\x61\x72\x20\x61\x20\x3d\x20\x30\x3b\x20\x76\x6f\x69\x64\x20\x28\x61\x20\x3d\x20\x34\x32\x29\x3b\x20\x61");
  ExpectInt32(42, result);
  result = CompileRun("\x76\x61\x72\x20\x61\x20\x3d\x20\x30\x3b\x20\x76\x6f\x69\x64\x20\x28\x61\x20\x3d\x20\x34\x32\x29\x3b");
  ExpectUndefined(result);
}


TEST(unary_not) {
  LocalContext env;
  v8::HandleScope scope(env->GetIsolate());
  Local<Value> result;
  result = CompileRun("\x76\x61\x72\x20\x61\x20\x3d\x20\x31\x32\x33\x34\x3b\x20\x21\x61");
  ExpectBoolean(false, result);
  result = CompileRun("\x76\x61\x72\x20\x61\x20\x3d\x20\x30\x3b\x20\x21\x61");
  ExpectBoolean(true, result);
  result = CompileRun("\x76\x61\x72\x20\x61\x20\x3d\x20\x30\x3b\x20\x21\x28\x61\x20\x3d\x20\x31\x32\x33\x34\x29\x3b\x20\x61");
  ExpectInt32(1234, result);
  result = CompileRun("\x76\x61\x72\x20\x61\x20\x3d\x20\x27\x31\x32\x33\x34\x27\x3b\x20\x21\x61");
  ExpectBoolean(false, result);
  result = CompileRun("\x76\x61\x72\x20\x61\x20\x3d\x20\x27\x27\x3b\x20\x21\x61");
  ExpectBoolean(true, result);
  result = CompileRun("\x76\x61\x72\x20\x61\x20\x3d\x20\x31\x32\x33\x34\x3b\x20\x21\x21\x61");
  ExpectBoolean(true, result);
  result = CompileRun("\x76\x61\x72\x20\x61\x20\x3d\x20\x30\x3b\x20\x21\x21\x61");
  ExpectBoolean(false, result);
  result = CompileRun("\x76\x61\x72\x20\x61\x20\x3d\x20\x30\x3b\x20\x69\x66\x20\x28\x20\x21\x61\x20\x29\x20\x7b\x20\x31\x3b\x20\x7d\x20\x65\x6c\x73\x65\x20\x7b\x20\x30\x3b\x20\x7d");
  ExpectInt32(1, result);
  result = CompileRun("\x76\x61\x72\x20\x61\x20\x3d\x20\x31\x3b\x20\x69\x66\x20\x28\x20\x21\x61\x20\x29\x20\x7b\x20\x31\x3b\x20\x7d\x20\x65\x6c\x73\x65\x20\x7b\x20\x30\x3b\x20\x7d");
  ExpectInt32(0, result);
}
