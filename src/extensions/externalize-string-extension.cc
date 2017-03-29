// Copyright 2010 the V8 project authors. All rights reserved.
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.

#include "src/extensions/externalize-string-extension.h"

#include "src/api.h"
#include "src/handles.h"
#include "src/isolate.h"

namespace v8 {
namespace internal {

template <typename Char, typename Base>
class SimpleStringResource : public Base {
 public:
  // Takes ownership of |data|.
  SimpleStringResource(Char* data, size_t length)
      : data_(data),
        length_(length) {}

  virtual ~SimpleStringResource() { delete[] data_; }

  virtual const Char* data() const { return data_; }

  virtual size_t length() const { return length_; }

 private:
  Char* const data_;
  const size_t length_;
};


typedef SimpleStringResource<char, v8::String::ExternalOneByteStringResource>
    SimpleOneByteStringResource;
typedef SimpleStringResource<uc16, v8::String::ExternalStringResource>
    SimpleTwoByteStringResource;

const char* const ExternalizeStringExtension::kSource =
    "\x6e\x61\x74\x69\x76\x65\x20\x66\x75\x6e\x63\x74\x69\x6f\x6e\x20\x65\x78\x74\x65\x72\x6e\x61\x6c\x69\x7a\x65\x53\x74\x72\x69\x6e\x67\x28\x29\x3b"
    "\x6e\x61\x74\x69\x76\x65\x20\x66\x75\x6e\x63\x74\x69\x6f\x6e\x20\x69\x73\x4f\x6e\x65\x42\x79\x74\x65\x53\x74\x72\x69\x6e\x67\x28\x29\x3b"
    "\x66\x75\x6e\x63\x74\x69\x6f\x6e\x20\x78\x28\x29\x20\x7b\x20\x72\x65\x74\x75\x72\x6e\x20\x31\x3b\x20\x7d";

v8::Local<v8::FunctionTemplate>
ExternalizeStringExtension::GetNativeFunctionTemplate(
    v8::Isolate* isolate, v8::Local<v8::String> str) {
  if (strcmp(*v8::String::Utf8Value(str), "\x65\x78\x74\x65\x72\x6e\x61\x6c\x69\x7a\x65\x53\x74\x72\x69\x6e\x67") == 0) {
    return v8::FunctionTemplate::New(isolate,
                                     ExternalizeStringExtension::Externalize);
  } else {
    DCHECK(strcmp(*v8::String::Utf8Value(str), "\x69\x73\x4f\x6e\x65\x42\x79\x74\x65\x53\x74\x72\x69\x6e\x67") == 0);
    return v8::FunctionTemplate::New(isolate,
                                     ExternalizeStringExtension::IsOneByte);
  }
}


void ExternalizeStringExtension::Externalize(
    const v8::FunctionCallbackInfo<v8::Value>& args) {
  if (args.Length() < 1 || !args[0]->IsString()) {
    args.GetIsolate()->ThrowException(
        v8::String::NewFromUtf8(
            args.GetIsolate(),
            "\x46\x69\x72\x73\x74\x20\x70\x61\x72\x61\x6d\x65\x74\x65\x72\x20\x74\x6f\x20\x65\x78\x74\x65\x72\x6e\x61\x6c\x69\x7a\x65\x53\x74\x72\x69\x6e\x67\x28\x29\x20\x6d\x75\x73\x74\x20\x62\x65\x20\x61\x20\x73\x74\x72\x69\x6e\x67\x2e",
            NewStringType::kNormal).ToLocalChecked());
    return;
  }
  bool force_two_byte = false;
  if (args.Length() >= 2) {
    if (args[1]->IsBoolean()) {
      force_two_byte =
          args[1]
              ->BooleanValue(args.GetIsolate()->GetCurrentContext())
              .FromJust();
    } else {
      args.GetIsolate()->ThrowException(
          v8::String::NewFromUtf8(
              args.GetIsolate(),
              "\x53\x65\x63\x6f\x6e\x64\x20\x70\x61\x72\x61\x6d\x65\x74\x65\x72\x20\x74\x6f\x20\x65\x78\x74\x65\x72\x6e\x61\x6c\x69\x7a\x65\x53\x74\x72\x69\x6e\x67\x28\x29\x20\x6d\x75\x73\x74\x20\x62\x65\x20\x61\x20\x62\x6f\x6f\x6c\x65\x61\x6e\x2e",
              NewStringType::kNormal).ToLocalChecked());
      return;
    }
  }
  bool result = false;
  Handle<String> string = Utils::OpenHandle(*args[0].As<v8::String>());
  if (string->IsExternalString()) {
    args.GetIsolate()->ThrowException(
        v8::String::NewFromUtf8(args.GetIsolate(),
                                "\x65\x78\x74\x65\x72\x6e\x61\x6c\x69\x7a\x65\x53\x74\x72\x69\x6e\x67\x28\x29\x20\x63\x61\x6e\x27\x74\x20\x65\x78\x74\x65\x72\x6e\x61\x6c\x69\x7a\x65\x20\x74\x77\x69\x63\x65\x2e",
                                NewStringType::kNormal).ToLocalChecked());
    return;
  }
  if (string->IsOneByteRepresentation() && !force_two_byte) {
    uint8_t* data = new uint8_t[string->length()];
    String::WriteToFlat(*string, data, 0, string->length());
    SimpleOneByteStringResource* resource = new SimpleOneByteStringResource(
        reinterpret_cast<char*>(data), string->length());
    result = string->MakeExternal(resource);
    if (result) {
      i::Isolate* isolate = reinterpret_cast<i::Isolate*>(args.GetIsolate());
      isolate->heap()->RegisterExternalString(*string);
    }
    if (!result) delete resource;
  } else {
    uc16* data = new uc16[string->length()];
    String::WriteToFlat(*string, data, 0, string->length());
    SimpleTwoByteStringResource* resource = new SimpleTwoByteStringResource(
        data, string->length());
    result = string->MakeExternal(resource);
    if (result) {
      i::Isolate* isolate = reinterpret_cast<i::Isolate*>(args.GetIsolate());
      isolate->heap()->RegisterExternalString(*string);
    }
    if (!result) delete resource;
  }
  if (!result) {
    args.GetIsolate()->ThrowException(
        v8::String::NewFromUtf8(args.GetIsolate(),
                                "\x65\x78\x74\x65\x72\x6e\x61\x6c\x69\x7a\x65\x53\x74\x72\x69\x6e\x67\x28\x29\x20\x66\x61\x69\x6c\x65\x64\x2e",
                                NewStringType::kNormal).ToLocalChecked());
    return;
  }
}


void ExternalizeStringExtension::IsOneByte(
    const v8::FunctionCallbackInfo<v8::Value>& args) {
  if (args.Length() != 1 || !args[0]->IsString()) {
    args.GetIsolate()->ThrowException(
        v8::String::NewFromUtf8(
            args.GetIsolate(),
            "\x69\x73\x4f\x6e\x65\x42\x79\x74\x65\x53\x74\x72\x69\x6e\x67\x28\x29\x20\x72\x65\x71\x75\x69\x72\x65\x73\x20\x61\x20\x73\x69\x6e\x67\x6c\x65\x20\x73\x74\x72\x69\x6e\x67\x20\x61\x72\x67\x75\x6d\x65\x6e\x74\x2e",
            NewStringType::kNormal).ToLocalChecked());
    return;
  }
  bool is_one_byte =
      Utils::OpenHandle(*args[0].As<v8::String>())->IsOneByteRepresentation();
  args.GetReturnValue().Set(is_one_byte);
}

}  // namespace internal
}  // namespace v8
