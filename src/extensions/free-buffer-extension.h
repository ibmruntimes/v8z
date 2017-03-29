// Copyright 2013 the V8 project authors. All rights reserved.
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.

#ifndef V8_EXTENSIONS_FREE_BUFFER_EXTENSION_H_
#define V8_EXTENSIONS_FREE_BUFFER_EXTENSION_H_

#include "include/v8.h"

namespace v8 {
namespace internal {

class FreeBufferExtension : public v8::Extension {
 public:
  FreeBufferExtension()
      : v8::Extension("\x76\x38\x2f\x66\x72\x65\x65\x2d\x62\x75\x66\x66\x65\x72", "\x6e\x61\x74\x69\x76\x65\x20\x66\x75\x6e\x63\x74\x69\x6f\x6e\x20\x66\x72\x65\x65\x42\x75\x66\x66\x65\x72\x28\x29\x3b") {}
  virtual v8::Local<v8::FunctionTemplate> GetNativeFunctionTemplate(
      v8::Isolate* isolate, v8::Local<v8::String> name);
  static void FreeBuffer(const v8::FunctionCallbackInfo<v8::Value>& args);
};

}  // namespace internal
}  // namespace v8

#endif  // V8_EXTENSIONS_FREE_BUFFER_EXTENSION_H_
