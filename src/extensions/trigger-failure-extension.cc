// Copyright 2013 the V8 project authors. All rights reserved.
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.

#include "src/extensions/trigger-failure-extension.h"

#include "src/base/logging.h"
#include "src/checks.h"

namespace v8 {
namespace internal {


const char* const TriggerFailureExtension::kSource =
    "\x6e\x61\x74\x69\x76\x65\x20\x66\x75\x6e\x63\x74\x69\x6f\x6e\x20\x74\x72\x69\x67\x67\x65\x72\x43\x68\x65\x63\x6b\x46\x61\x6c\x73\x65\x28\x29\x3b"
    "\x6e\x61\x74\x69\x76\x65\x20\x66\x75\x6e\x63\x74\x69\x6f\x6e\x20\x74\x72\x69\x67\x67\x65\x72\x41\x73\x73\x65\x72\x74\x46\x61\x6c\x73\x65\x28\x29\x3b"
    "\x6e\x61\x74\x69\x76\x65\x20\x66\x75\x6e\x63\x74\x69\x6f\x6e\x20\x74\x72\x69\x67\x67\x65\x72\x53\x6c\x6f\x77\x41\x73\x73\x65\x72\x74\x46\x61\x6c\x73\x65\x28\x29\x3b";


v8::Local<v8::FunctionTemplate>
TriggerFailureExtension::GetNativeFunctionTemplate(v8::Isolate* isolate,
                                                   v8::Local<v8::String> str) {
  if (strcmp(*v8::String::Utf8Value(str), "\x74\x72\x69\x67\x67\x65\x72\x43\x68\x65\x63\x6b\x46\x61\x6c\x73\x65") == 0) {
    return v8::FunctionTemplate::New(
        isolate,
        TriggerFailureExtension::TriggerCheckFalse);
  } else if (strcmp(*v8::String::Utf8Value(str), "\x74\x72\x69\x67\x67\x65\x72\x41\x73\x73\x65\x72\x74\x46\x61\x6c\x73\x65") == 0) {
    return v8::FunctionTemplate::New(
        isolate,
        TriggerFailureExtension::TriggerAssertFalse);
  } else {
    CHECK_EQ(0, strcmp(*v8::String::Utf8Value(str), "\x74\x72\x69\x67\x67\x65\x72\x53\x6c\x6f\x77\x41\x73\x73\x65\x72\x74\x46\x61\x6c\x73\x65"));
    return v8::FunctionTemplate::New(
        isolate,
        TriggerFailureExtension::TriggerSlowAssertFalse);
  }
}


void TriggerFailureExtension::TriggerCheckFalse(
    const v8::FunctionCallbackInfo<v8::Value>& args) {
  CHECK(false);
}


void TriggerFailureExtension::TriggerAssertFalse(
    const v8::FunctionCallbackInfo<v8::Value>& args) {
  DCHECK(false);
}


void TriggerFailureExtension::TriggerSlowAssertFalse(
    const v8::FunctionCallbackInfo<v8::Value>& args) {
  SLOW_DCHECK(false);
}

}  // namespace internal
}  // namespace v8
