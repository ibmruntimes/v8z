// Copyright 2014 the V8 project authors. All rights reserved.
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

#include "test/cctest/trace-extension.h"

#include "src/sampler.h"
#include "test/cctest/cctest.h"

namespace v8 {
namespace internal {

const char* TraceExtension::kSource =
    "\x6e\x61\x74\x69\x76\x65\x20\x66\x75\x6e\x63\x74\x69\x6f\x6e\x20\x74\x72\x61\x63\x65\x28\x29\x3b"
    "\x6e\x61\x74\x69\x76\x65\x20\x66\x75\x6e\x63\x74\x69\x6f\x6e\x20\x6a\x73\x5f\x74\x72\x61\x63\x65\x28\x29\x3b"
    "\x6e\x61\x74\x69\x76\x65\x20\x66\x75\x6e\x63\x74\x69\x6f\x6e\x20\x6a\x73\x5f\x65\x6e\x74\x72\x79\x5f\x73\x70\x28\x29\x3b"
    "\x6e\x61\x74\x69\x76\x65\x20\x66\x75\x6e\x63\x74\x69\x6f\x6e\x20\x6a\x73\x5f\x65\x6e\x74\x72\x79\x5f\x73\x70\x5f\x6c\x65\x76\x65\x6c\x32\x28\x29\x3b";


v8::Handle<v8::FunctionTemplate> TraceExtension::GetNativeFunctionTemplate(
    v8::Isolate* isolate, v8::Handle<v8::String> name) {
  if (name->Equals(v8::String::NewFromUtf8(isolate, "\x74\x72\x61\x63\x65"))) {
    return v8::FunctionTemplate::New(isolate, TraceExtension::Trace);
  } else if (name->Equals(v8::String::NewFromUtf8(isolate, "\x6a\x73\x5f\x74\x72\x61\x63\x65"))) {
    return v8::FunctionTemplate::New(isolate, TraceExtension::JSTrace);
  } else if (name->Equals(v8::String::NewFromUtf8(isolate, "\x6a\x73\x5f\x65\x6e\x74\x72\x79\x5f\x73\x70"))) {
    return v8::FunctionTemplate::New(isolate, TraceExtension::JSEntrySP);
  } else if (name->Equals(v8::String::NewFromUtf8(isolate,
                                                  "\x6a\x73\x5f\x65\x6e\x74\x72\x79\x5f\x73\x70\x5f\x6c\x65\x76\x65\x6c\x32"))) {
    return v8::FunctionTemplate::New(isolate, TraceExtension::JSEntrySPLevel2);
  } else {
    CHECK(false);
    return v8::Handle<v8::FunctionTemplate>();
  }
}


Address TraceExtension::GetFP(const v8::FunctionCallbackInfo<v8::Value>& args) {
  // Convert frame pointer from encoding as smis in the arguments to a pointer.
  CHECK_EQ(2, args.Length());  // Ignore second argument on 32-bit platform.
#if defined(V8_HOST_ARCH_32_BIT)
  Address fp = *reinterpret_cast<Address*>(*args[0]);
#elif defined(V8_HOST_ARCH_64_BIT)
  int64_t low_bits = *reinterpret_cast<uint64_t*>(*args[0]) >> 32;
  int64_t high_bits = *reinterpret_cast<uint64_t*>(*args[1]);
  Address fp = reinterpret_cast<Address>(high_bits | low_bits);
#else
#error Host architecture is neither 32-bit nor 64-bit.
#endif
  printf("\x54\x72\x61\x63\x65\x3a\x20\x6c\x97\xa", fp);
  return fp;
}


static struct {
  TickSample* sample;
} trace_env = { NULL };


void TraceExtension::InitTraceEnv(TickSample* sample) {
  trace_env.sample = sample;
}


void TraceExtension::DoTrace(Address fp) {
  RegisterState regs;
  regs.fp = fp;
  // sp is only used to define stack high bound
  regs.sp =
      reinterpret_cast<Address>(trace_env.sample) - 10240;
  trace_env.sample->Init(CcTest::i_isolate(), regs);
}


void TraceExtension::Trace(const v8::FunctionCallbackInfo<v8::Value>& args) {
  DoTrace(GetFP(args));
}


// Hide c_entry_fp to emulate situation when sampling is done while
// pure JS code is being executed
static void DoTraceHideCEntryFPAddress(Address fp) {
  v8::internal::Address saved_c_frame_fp =
      *(CcTest::i_isolate()->c_entry_fp_address());
  CHECK(saved_c_frame_fp);
  *(CcTest::i_isolate()->c_entry_fp_address()) = 0;
  i::TraceExtension::DoTrace(fp);
  *(CcTest::i_isolate()->c_entry_fp_address()) = saved_c_frame_fp;
}


void TraceExtension::JSTrace(const v8::FunctionCallbackInfo<v8::Value>& args) {
  DoTraceHideCEntryFPAddress(GetFP(args));
}


Address TraceExtension::GetJsEntrySp() {
  CHECK_NE(NULL, CcTest::i_isolate()->thread_local_top());
  return CcTest::i_isolate()->js_entry_sp();
}


void TraceExtension::JSEntrySP(
    const v8::FunctionCallbackInfo<v8::Value>& args) {
  CHECK_NE(0, GetJsEntrySp());
}


void TraceExtension::JSEntrySPLevel2(
    const v8::FunctionCallbackInfo<v8::Value>& args) {
  v8::HandleScope scope(args.GetIsolate());
  const Address js_entry_sp = GetJsEntrySp();
  CHECK_NE(0, js_entry_sp);
  CompileRun("\x6a\x73\x5f\x65\x6e\x74\x72\x79\x5f\x73\x70\x28\x29\x3b");
  CHECK_EQ(js_entry_sp, GetJsEntrySp());
}


} }  // namespace v8::internal
