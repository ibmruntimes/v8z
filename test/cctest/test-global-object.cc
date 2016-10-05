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

#include "src/v8.h"

#include "test/cctest/cctest.h"

using namespace v8;

// This test fails if properties on the prototype of the global object appear
// as declared globals.
TEST(StrictUndeclaredGlobalVariable) {
  HandleScope scope(CcTest::isolate());
  v8::Local<v8::String> var_name = v8_str("\x78");
  LocalContext context;
  v8::TryCatch try_catch;
  v8::Local<v8::Script> script = v8_compile("\x22\x75\x73\x65\x20\x73\x74\x72\x69\x63\x74\x22\x3b\x20\x78\x20\x3d\x20\x34\x32\x3b");
  v8::Handle<v8::Object> proto = v8::Object::New(CcTest::isolate());
  v8::Handle<v8::Object> global =
      context->Global()->GetPrototype().As<v8::Object>();
  proto->Set(var_name, v8_num(100));
  global->SetPrototype(proto);
  script->Run();
  CHECK(try_catch.HasCaught());
  v8::String::Utf8Value exception(try_catch.Exception());
  CHECK_EQ("\x52\x65\x66\x65\x72\x65\x6e\x63\x65\x45\x72\x72\x6f\x72\x3a\x20\x78\x20\x69\x73\x20\x6e\x6f\x74\x20\x64\x65\x66\x69\x6e\x65\x64", *exception);
}
