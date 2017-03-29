// Copyright 2010 the V8 project authors. All rights reserved.
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.

#ifndef V8_EXTENSIONS_GC_EXTENSION_H_
#define V8_EXTENSIONS_GC_EXTENSION_H_

#include "include/v8.h"
#include "src/utils.h"

namespace v8 {
namespace internal {

class GCExtension : public v8::Extension {
 public:
  explicit GCExtension(const char* fun_name)
      : v8::Extension("\x76\x38\x2f\x67\x63",
                      BuildSource(buffer_, sizeof(buffer_), fun_name)) {}
  virtual v8::Local<v8::FunctionTemplate> GetNativeFunctionTemplate(
      v8::Isolate* isolate, v8::Local<v8::String> name);
  static void GC(const v8::FunctionCallbackInfo<v8::Value>& args);

 private:
  static const char* BuildSource(char* buf, size_t size, const char* fun_name) {
    SNPrintF(Vector<char>(buf, static_cast<int>(size)),
             "\x6e\x61\x74\x69\x76\x65\x20\x66\x75\x6e\x63\x74\x69\x6f\x6e\x20\x25\x73\x28\x29\x3b", fun_name);
    return buf;
  }

  char buffer_[50];
};

}  // namespace internal
}  // namespace v8

#endif  // V8_EXTENSIONS_GC_EXTENSION_H_
