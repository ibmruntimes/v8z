// Copyright 2012 the V8 project authors. All rights reserved.
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.

#ifndef V8_JSON_STRINGIFIER_H_
#define V8_JSON_STRINGIFIER_H_

#include "src/conversions.h"
#include "src/lookup.h"
#include "src/messages.h"
#include "src/string-builder.h"
#include "src/utils.h"

namespace v8 {
namespace internal {

class BasicJsonStringifier BASE_EMBEDDED {
 public:
  explicit BasicJsonStringifier(Isolate* isolate);

  MUST_USE_RESULT MaybeHandle<Object> Stringify(Handle<Object> object);

  MUST_USE_RESULT INLINE(static MaybeHandle<Object> StringifyString(
      Isolate* isolate,
      Handle<String> object));

 private:
  enum Result { UNCHANGED, SUCCESS, EXCEPTION };

  MUST_USE_RESULT MaybeHandle<Object> ApplyToJsonFunction(
      Handle<Object> object,
      Handle<Object> key);

  Result SerializeGeneric(Handle<Object> object,
                          Handle<Object> key,
                          bool deferred_comma,
                          bool deferred_key);

  // Entry point to serialize the object.
  INLINE(Result SerializeObject(Handle<Object> obj)) {
    return Serialize_<false>(obj, false, factory()->empty_string());
  }

  // Serialize an array element.
  // The index may serve as argument for the toJSON function.
  INLINE(Result SerializeElement(Isolate* isolate,
                                 Handle<Object> object,
                                 int i)) {
    return Serialize_<false>(object,
                             false,
                             Handle<Object>(Smi::FromInt(i), isolate));
  }

  // Serialize a object property.
  // The key may or may not be serialized depending on the property.
  // The key may also serve as argument for the toJSON function.
  INLINE(Result SerializeProperty(Handle<Object> object,
                                  bool deferred_comma,
                                  Handle<String> deferred_key)) {
    DCHECK(!deferred_key.is_null());
    return Serialize_<true>(object, deferred_comma, deferred_key);
  }

  template <bool deferred_string_key>
  Result Serialize_(Handle<Object> object, bool comma, Handle<Object> key);

  void SerializeDeferredKey(bool deferred_comma, Handle<Object> deferred_key) {
    if (deferred_comma) builder_.AppendCharacter('\x2c');
    SerializeString(Handle<String>::cast(deferred_key));
    builder_.AppendCharacter('\x3a');
  }

  Result SerializeSmi(Smi* object);

  Result SerializeDouble(double number);
  INLINE(Result SerializeHeapNumber(Handle<HeapNumber> object)) {
    return SerializeDouble(object->value());
  }

  Result SerializeJSValue(Handle<JSValue> object);

  INLINE(Result SerializeJSArray(Handle<JSArray> object));
  INLINE(Result SerializeJSObject(Handle<JSObject> object));

  Result SerializeJSArraySlow(Handle<JSArray> object, uint32_t start,
                              uint32_t length);

  void SerializeString(Handle<String> object);

  template <typename SrcChar, typename DestChar>
  INLINE(static void SerializeStringUnchecked_(
      Vector<const SrcChar> src,
      IncrementalStringBuilder::NoExtend<DestChar>* dest));

  template <typename SrcChar, typename DestChar>
  INLINE(void SerializeString_(Handle<String> string));

  template <typename Char>
  INLINE(static bool DoNotEscape(Char c));

  Result StackPush(Handle<Object> object);
  void StackPop();

  Factory* factory() { return isolate_->factory(); }

  Isolate* isolate_;
  IncrementalStringBuilder builder_;
  Handle<String> tojson_string_;
  Handle<JSArray> stack_;

  static const int kJsonEscapeTableEntrySize = 8;
  static const char* const JsonEscapeTable;
};


// Translation table to escape Latin1 characters.
// Table entries start at a multiple of 8 and are null-terminated.
const char* const BasicJsonStringifier::JsonEscapeTable =
    "\x5c\x75\x30\x30\x30\x30\x0\x20\x5c\x75\x30\x30\x30\x31\x0\x20\x5c\x75\x30\x30\x30\x32\x0\x20\x5c\x75\x30\x30\x30\x33\x0\x20"
    "\x5c\x75\x30\x30\x30\x34\x0\x20\x5c\x75\x30\x30\x30\x35\x0\x20\x5c\x75\x30\x30\x30\x36\x0\x20\x5c\x75\x30\x30\x30\x37\x0\x20"
    "\x5c\x62\x0\x20\x20\x20\x20\x20\x5c\x74\x0\x20\x20\x20\x20\x20\x5c\x6e\x0\x20\x20\x20\x20\x20\x5c\x75\x30\x30\x30\x62\x0\x20"
    "\x5c\x66\x0\x20\x20\x20\x20\x20\x5c\x72\x0\x20\x20\x20\x20\x20\x5c\x75\x30\x30\x30\x65\x0\x20\x5c\x75\x30\x30\x30\x66\x0\x20"
    "\x5c\x75\x30\x30\x31\x30\x0\x20\x5c\x75\x30\x30\x31\x31\x0\x20\x5c\x75\x30\x30\x31\x32\x0\x20\x5c\x75\x30\x30\x31\x33\x0\x20"
    "\x5c\x75\x30\x30\x31\x34\x0\x20\x5c\x75\x30\x30\x31\x35\x0\x20\x5c\x75\x30\x30\x31\x36\x0\x20\x5c\x75\x30\x30\x31\x37\x0\x20"
    "\x5c\x75\x30\x30\x31\x38\x0\x20\x5c\x75\x30\x30\x31\x39\x0\x20\x5c\x75\x30\x30\x31\x61\x0\x20\x5c\x75\x30\x30\x31\x62\x0\x20"
    "\x5c\x75\x30\x30\x31\x63\x0\x20\x5c\x75\x30\x30\x31\x64\x0\x20\x5c\x75\x30\x30\x31\x65\x0\x20\x5c\x75\x30\x30\x31\x66\x0\x20"
    "\x20\x0\x20\x20\x20\x20\x20\x20\x21\x0\x20\x20\x20\x20\x20\x20\x5c\x22\x0\x20\x20\x20\x20\x20\x23\x0\x20\x20\x20\x20\x20\x20"
    "\x24\x0\x20\x20\x20\x20\x20\x20\x25\x0\x20\x20\x20\x20\x20\x20\x26\x0\x20\x20\x20\x20\x20\x20\x27\x0\x20\x20\x20\x20\x20\x20"
    "\x28\x0\x20\x20\x20\x20\x20\x20\x29\x0\x20\x20\x20\x20\x20\x20\x2a\x0\x20\x20\x20\x20\x20\x20\x2b\x0\x20\x20\x20\x20\x20\x20"
    "\x2c\x0\x20\x20\x20\x20\x20\x20\x2d\x0\x20\x20\x20\x20\x20\x20\x2e\x0\x20\x20\x20\x20\x20\x20\x2f\x0\x20\x20\x20\x20\x20\x20"
    "\x30\x0\x20\x20\x20\x20\x20\x20\x31\x0\x20\x20\x20\x20\x20\x20\x32\x0\x20\x20\x20\x20\x20\x20\x33\x0\x20\x20\x20\x20\x20\x20"
    "\x34\x0\x20\x20\x20\x20\x20\x20\x35\x0\x20\x20\x20\x20\x20\x20\x36\x0\x20\x20\x20\x20\x20\x20\x37\x0\x20\x20\x20\x20\x20\x20"
    "\x38\x0\x20\x20\x20\x20\x20\x20\x39\x0\x20\x20\x20\x20\x20\x20\x3a\x0\x20\x20\x20\x20\x20\x20\x3b\x0\x20\x20\x20\x20\x20\x20"
    "\x3c\x0\x20\x20\x20\x20\x20\x20\x3d\x0\x20\x20\x20\x20\x20\x20\x3e\x0\x20\x20\x20\x20\x20\x20\x3f\x0\x20\x20\x20\x20\x20\x20"
    "\x40\x0\x20\x20\x20\x20\x20\x20\x41\x0\x20\x20\x20\x20\x20\x20\x42\x0\x20\x20\x20\x20\x20\x20\x43\x0\x20\x20\x20\x20\x20\x20"
    "\x44\x0\x20\x20\x20\x20\x20\x20\x45\x0\x20\x20\x20\x20\x20\x20\x46\x0\x20\x20\x20\x20\x20\x20\x47\x0\x20\x20\x20\x20\x20\x20"
    "\x48\x0\x20\x20\x20\x20\x20\x20\x49\x0\x20\x20\x20\x20\x20\x20\x4a\x0\x20\x20\x20\x20\x20\x20\x4b\x0\x20\x20\x20\x20\x20\x20"
    "\x4c\x0\x20\x20\x20\x20\x20\x20\x4d\x0\x20\x20\x20\x20\x20\x20\x4e\x0\x20\x20\x20\x20\x20\x20\x4f\x0\x20\x20\x20\x20\x20\x20"
    "\x50\x0\x20\x20\x20\x20\x20\x20\x51\x0\x20\x20\x20\x20\x20\x20\x52\x0\x20\x20\x20\x20\x20\x20\x53\x0\x20\x20\x20\x20\x20\x20"
    "\x54\x0\x20\x20\x20\x20\x20\x20\x55\x0\x20\x20\x20\x20\x20\x20\x56\x0\x20\x20\x20\x20\x20\x20\x57\x0\x20\x20\x20\x20\x20\x20"
    "\x58\x0\x20\x20\x20\x20\x20\x20\x59\x0\x20\x20\x20\x20\x20\x20\x5a\x0\x20\x20\x20\x20\x20\x20\x5b\x0\x20\x20\x20\x20\x20\x20"
    "\x5c\x5c\x0\x20\x20\x20\x20\x20\x5d\x0\x20\x20\x20\x20\x20\x20\x5e\x0\x20\x20\x20\x20\x20\x20\x5f\x0\x20\x20\x20\x20\x20\x20"
    "\x60\x0\x20\x20\x20\x20\x20\x20\x61\x0\x20\x20\x20\x20\x20\x20\x62\x0\x20\x20\x20\x20\x20\x20\x63\x0\x20\x20\x20\x20\x20\x20"
    "\x64\x0\x20\x20\x20\x20\x20\x20\x65\x0\x20\x20\x20\x20\x20\x20\x66\x0\x20\x20\x20\x20\x20\x20\x67\x0\x20\x20\x20\x20\x20\x20"
    "\x68\x0\x20\x20\x20\x20\x20\x20\x69\x0\x20\x20\x20\x20\x20\x20\x6a\x0\x20\x20\x20\x20\x20\x20\x6b\x0\x20\x20\x20\x20\x20\x20"
    "\x6c\x0\x20\x20\x20\x20\x20\x20\x6d\x0\x20\x20\x20\x20\x20\x20\x6e\x0\x20\x20\x20\x20\x20\x20\x6f\x0\x20\x20\x20\x20\x20\x20"
    "\x70\x0\x20\x20\x20\x20\x20\x20\x71\x0\x20\x20\x20\x20\x20\x20\x72\x0\x20\x20\x20\x20\x20\x20\x73\x0\x20\x20\x20\x20\x20\x20"
    "\x74\x0\x20\x20\x20\x20\x20\x20\x75\x0\x20\x20\x20\x20\x20\x20\x76\x0\x20\x20\x20\x20\x20\x20\x77\x0\x20\x20\x20\x20\x20\x20"
    "\x78\x0\x20\x20\x20\x20\x20\x20\x79\x0\x20\x20\x20\x20\x20\x20\x7a\x0\x20\x20\x20\x20\x20\x20\x7b\x0\x20\x20\x20\x20\x20\x20"
    "\x7c\x0\x20\x20\x20\x20\x20\x20\x7d\x0\x20\x20\x20\x20\x20\x20\x7e\x0\x20\x20\x20\x20\x20\x20\177\x0\x20\x20\x20\x20\x20\x20"
    "\200\0\x20\x20\x20\x20\x20\x20\201\0\x20\x20\x20\x20\x20\x20\202\0\x20\x20\x20\x20\x20\x20\203\0\x20\x20\x20\x20\x20\x20"
    "\204\0\x20\x20\x20\x20\x20\x20\205\0\x20\x20\x20\x20\x20\x20\206\0\x20\x20\x20\x20\x20\x20\207\0\x20\x20\x20\x20\x20\x20"
    "\210\0\x20\x20\x20\x20\x20\x20\211\0\x20\x20\x20\x20\x20\x20\212\0\x20\x20\x20\x20\x20\x20\213\0\x20\x20\x20\x20\x20\x20"
    "\214\0\x20\x20\x20\x20\x20\x20\215\0\x20\x20\x20\x20\x20\x20\216\0\x20\x20\x20\x20\x20\x20\217\0\x20\x20\x20\x20\x20\x20"
    "\220\0\x20\x20\x20\x20\x20\x20\221\0\x20\x20\x20\x20\x20\x20\222\0\x20\x20\x20\x20\x20\x20\223\0\x20\x20\x20\x20\x20\x20"
    "\224\0\x20\x20\x20\x20\x20\x20\225\0\x20\x20\x20\x20\x20\x20\226\0\x20\x20\x20\x20\x20\x20\227\0\x20\x20\x20\x20\x20\x20"
    "\230\0\x20\x20\x20\x20\x20\x20\231\0\x20\x20\x20\x20\x20\x20\232\0\x20\x20\x20\x20\x20\x20\233\0\x20\x20\x20\x20\x20\x20"
    "\234\0\x20\x20\x20\x20\x20\x20\235\0\x20\x20\x20\x20\x20\x20\236\0\x20\x20\x20\x20\x20\x20\237\0\x20\x20\x20\x20\x20\x20"
    "\240\0\x20\x20\x20\x20\x20\x20\241\0\x20\x20\x20\x20\x20\x20\242\0\x20\x20\x20\x20\x20\x20\243\0\x20\x20\x20\x20\x20\x20"
    "\244\0\x20\x20\x20\x20\x20\x20\245\0\x20\x20\x20\x20\x20\x20\246\0\x20\x20\x20\x20\x20\x20\247\0\x20\x20\x20\x20\x20\x20"
    "\250\0\x20\x20\x20\x20\x20\x20\251\0\x20\x20\x20\x20\x20\x20\252\0\x20\x20\x20\x20\x20\x20\253\0\x20\x20\x20\x20\x20\x20"
    "\254\0\x20\x20\x20\x20\x20\x20\255\0\x20\x20\x20\x20\x20\x20\256\0\x20\x20\x20\x20\x20\x20\257\0\x20\x20\x20\x20\x20\x20"
    "\260\0\x20\x20\x20\x20\x20\x20\261\0\x20\x20\x20\x20\x20\x20\262\0\x20\x20\x20\x20\x20\x20\263\0\x20\x20\x20\x20\x20\x20"
    "\264\0\x20\x20\x20\x20\x20\x20\265\0\x20\x20\x20\x20\x20\x20\266\0\x20\x20\x20\x20\x20\x20\267\0\x20\x20\x20\x20\x20\x20"
    "\270\0\x20\x20\x20\x20\x20\x20\271\0\x20\x20\x20\x20\x20\x20\272\0\x20\x20\x20\x20\x20\x20\273\0\x20\x20\x20\x20\x20\x20"
    "\274\0\x20\x20\x20\x20\x20\x20\275\0\x20\x20\x20\x20\x20\x20\276\0\x20\x20\x20\x20\x20\x20\277\0\x20\x20\x20\x20\x20\x20"
    "\300\0\x20\x20\x20\x20\x20\x20\301\0\x20\x20\x20\x20\x20\x20\302\0\x20\x20\x20\x20\x20\x20\303\0\x20\x20\x20\x20\x20\x20"
    "\304\0\x20\x20\x20\x20\x20\x20\305\0\x20\x20\x20\x20\x20\x20\306\0\x20\x20\x20\x20\x20\x20\307\0\x20\x20\x20\x20\x20\x20"
    "\310\0\x20\x20\x20\x20\x20\x20\311\0\x20\x20\x20\x20\x20\x20\312\0\x20\x20\x20\x20\x20\x20\313\0\x20\x20\x20\x20\x20\x20"
    "\314\0\x20\x20\x20\x20\x20\x20\315\0\x20\x20\x20\x20\x20\x20\316\0\x20\x20\x20\x20\x20\x20\317\0\x20\x20\x20\x20\x20\x20"
    "\320\0\x20\x20\x20\x20\x20\x20\321\0\x20\x20\x20\x20\x20\x20\322\0\x20\x20\x20\x20\x20\x20\323\0\x20\x20\x20\x20\x20\x20"
    "\324\0\x20\x20\x20\x20\x20\x20\325\0\x20\x20\x20\x20\x20\x20\326\0\x20\x20\x20\x20\x20\x20\327\0\x20\x20\x20\x20\x20\x20"
    "\330\0\x20\x20\x20\x20\x20\x20\331\0\x20\x20\x20\x20\x20\x20\332\0\x20\x20\x20\x20\x20\x20\333\0\x20\x20\x20\x20\x20\x20"
    "\334\0\x20\x20\x20\x20\x20\x20\335\0\x20\x20\x20\x20\x20\x20\336\0\x20\x20\x20\x20\x20\x20\337\0\x20\x20\x20\x20\x20\x20"
    "\340\0\x20\x20\x20\x20\x20\x20\341\0\x20\x20\x20\x20\x20\x20\342\0\x20\x20\x20\x20\x20\x20\343\0\x20\x20\x20\x20\x20\x20"
    "\344\0\x20\x20\x20\x20\x20\x20\345\0\x20\x20\x20\x20\x20\x20\346\0\x20\x20\x20\x20\x20\x20\347\0\x20\x20\x20\x20\x20\x20"
    "\350\0\x20\x20\x20\x20\x20\x20\351\0\x20\x20\x20\x20\x20\x20\352\0\x20\x20\x20\x20\x20\x20\353\0\x20\x20\x20\x20\x20\x20"
    "\354\0\x20\x20\x20\x20\x20\x20\355\0\x20\x20\x20\x20\x20\x20\356\0\x20\x20\x20\x20\x20\x20\357\0\x20\x20\x20\x20\x20\x20"
    "\360\0\x20\x20\x20\x20\x20\x20\361\0\x20\x20\x20\x20\x20\x20\362\0\x20\x20\x20\x20\x20\x20\363\0\x20\x20\x20\x20\x20\x20"
    "\364\0\x20\x20\x20\x20\x20\x20\365\0\x20\x20\x20\x20\x20\x20\366\0\x20\x20\x20\x20\x20\x20\367\0\x20\x20\x20\x20\x20\x20"
    "\370\0\x20\x20\x20\x20\x20\x20\371\0\x20\x20\x20\x20\x20\x20\372\0\x20\x20\x20\x20\x20\x20\373\0\x20\x20\x20\x20\x20\x20"
    "\374\0\x20\x20\x20\x20\x20\x20\375\0\x20\x20\x20\x20\x20\x20\376\0\x20\x20\x20\x20\x20\x20\377\0\x20\x20\x20\x20\x20\x20";


BasicJsonStringifier::BasicJsonStringifier(Isolate* isolate)
    : isolate_(isolate), builder_(isolate) {
  tojson_string_ = factory()->toJSON_string();
  stack_ = factory()->NewJSArray(8);
}


MaybeHandle<Object> BasicJsonStringifier::Stringify(Handle<Object> object) {
  Result result = SerializeObject(object);
  if (result == UNCHANGED) return factory()->undefined_value();
  if (result == SUCCESS) return builder_.Finish();
  DCHECK(result == EXCEPTION);
  return MaybeHandle<Object>();
}


MaybeHandle<Object> BasicJsonStringifier::StringifyString(
    Isolate* isolate,  Handle<String> object) {
  static const int kJsonQuoteWorstCaseBlowup = 6;
  static const int kSpaceForQuotes = 2;
  int worst_case_length =
      object->length() * kJsonQuoteWorstCaseBlowup + kSpaceForQuotes;

  if (worst_case_length > 32 * KB) {  // Slow path if too large.
    BasicJsonStringifier stringifier(isolate);
    return stringifier.Stringify(object);
  }

  object = String::Flatten(object);
  DCHECK(object->IsFlat());
  Handle<SeqString> result;
  if (object->IsOneByteRepresentationUnderneath()) {
    result = isolate->factory()
                 ->NewRawOneByteString(worst_case_length)
                 .ToHandleChecked();
    IncrementalStringBuilder::NoExtendString<uint8_t> no_extend(
        result, worst_case_length);
    no_extend.Append('\x22');
    SerializeStringUnchecked_(object->GetFlatContent().ToOneByteVector(),
                              &no_extend);
    no_extend.Append('\x22');
    return no_extend.Finalize();
  } else {
    result = isolate->factory()
                 ->NewRawTwoByteString(worst_case_length)
                 .ToHandleChecked();
    IncrementalStringBuilder::NoExtendString<uc16> no_extend(result,
                                                             worst_case_length);
    no_extend.Append('\x22');
    SerializeStringUnchecked_(object->GetFlatContent().ToUC16Vector(),
                              &no_extend);
    no_extend.Append('\x22');
    return no_extend.Finalize();
  }
}


MaybeHandle<Object> BasicJsonStringifier::ApplyToJsonFunction(
    Handle<Object> object, Handle<Object> key) {
  LookupIterator it(object, tojson_string_,
                    LookupIterator::PROTOTYPE_CHAIN_SKIP_INTERCEPTOR);
  Handle<Object> fun;
  ASSIGN_RETURN_ON_EXCEPTION(isolate_, fun, Object::GetProperty(&it), Object);
  if (!fun->IsCallable()) return object;

  // Call toJSON function.
  if (key->IsSmi()) key = factory()->NumberToString(key);
  Handle<Object> argv[] = { key };
  HandleScope scope(isolate_);
  ASSIGN_RETURN_ON_EXCEPTION(
      isolate_, object,
      Execution::Call(isolate_, fun, object, 1, argv),
      Object);
  return scope.CloseAndEscape(object);
}


BasicJsonStringifier::Result BasicJsonStringifier::StackPush(
    Handle<Object> object) {
  StackLimitCheck check(isolate_);
  if (check.HasOverflowed()) {
    isolate_->StackOverflow();
    return EXCEPTION;
  }

  int length = Smi::cast(stack_->length())->value();
  {
    DisallowHeapAllocation no_allocation;
    FixedArray* elements = FixedArray::cast(stack_->elements());
    for (int i = 0; i < length; i++) {
      if (elements->get(i) == *object) {
        AllowHeapAllocation allow_to_return_error;
        Handle<Object> error =
            factory()->NewTypeError(MessageTemplate::kCircularStructure);
        isolate_->Throw(*error);
        return EXCEPTION;
      }
    }
  }
  JSArray::SetLength(stack_, length + 1);
  FixedArray::cast(stack_->elements())->set(length, *object);
  return SUCCESS;
}


void BasicJsonStringifier::StackPop() {
  int length = Smi::cast(stack_->length())->value();
  stack_->set_length(Smi::FromInt(length - 1));
}


template <bool deferred_string_key>
BasicJsonStringifier::Result BasicJsonStringifier::Serialize_(
    Handle<Object> object, bool comma, Handle<Object> key) {
  if (object->IsJSObject()) {
    ASSIGN_RETURN_ON_EXCEPTION_VALUE(
        isolate_, object,
        ApplyToJsonFunction(object, key),
        EXCEPTION);
  }

  if (object->IsSmi()) {
    if (deferred_string_key) SerializeDeferredKey(comma, key);
    return SerializeSmi(Smi::cast(*object));
  }

  switch (HeapObject::cast(*object)->map()->instance_type()) {
    case HEAP_NUMBER_TYPE:
    case MUTABLE_HEAP_NUMBER_TYPE:
      if (deferred_string_key) SerializeDeferredKey(comma, key);
      return SerializeHeapNumber(Handle<HeapNumber>::cast(object));
    case ODDBALL_TYPE:
      switch (Oddball::cast(*object)->kind()) {
        case Oddball::kFalse:
          if (deferred_string_key) SerializeDeferredKey(comma, key);
          builder_.AppendCString("\x66\x61\x6c\x73\x65");
          return SUCCESS;
        case Oddball::kTrue:
          if (deferred_string_key) SerializeDeferredKey(comma, key);
          builder_.AppendCString("\x74\x72\x75\x65");
          return SUCCESS;
        case Oddball::kNull:
          if (deferred_string_key) SerializeDeferredKey(comma, key);
          builder_.AppendCString("\x6e\x75\x6c\x6c");
          return SUCCESS;
        default:
          return UNCHANGED;
      }
    case JS_ARRAY_TYPE:
      if (object->IsAccessCheckNeeded()) break;
      if (deferred_string_key) SerializeDeferredKey(comma, key);
      return SerializeJSArray(Handle<JSArray>::cast(object));
    case JS_VALUE_TYPE:
      if (deferred_string_key) SerializeDeferredKey(comma, key);
      return SerializeJSValue(Handle<JSValue>::cast(object));
    default:
      if (object->IsString()) {
        if (deferred_string_key) SerializeDeferredKey(comma, key);
        SerializeString(Handle<String>::cast(object));
        return SUCCESS;
      } else if (object->IsJSObject()) {
        if (object->IsCallable()) return UNCHANGED;
        // Go to slow path for global proxy and objects requiring access checks.
        if (object->IsAccessCheckNeeded() || object->IsJSGlobalProxy()) break;
        if (deferred_string_key) SerializeDeferredKey(comma, key);
        return SerializeJSObject(Handle<JSObject>::cast(object));
      }
  }

  return SerializeGeneric(object, key, comma, deferred_string_key);
}


BasicJsonStringifier::Result BasicJsonStringifier::SerializeGeneric(
    Handle<Object> object,
    Handle<Object> key,
    bool deferred_comma,
    bool deferred_key) {
  Handle<JSFunction> fun = isolate_->json_serialize_adapter();
  Handle<Object> argv[] = { key, object };
  Handle<Object> result;
  ASSIGN_RETURN_ON_EXCEPTION_VALUE(
      isolate_, result, Execution::Call(isolate_, fun, object, 2, argv),
      EXCEPTION);
  if (result->IsUndefined()) return UNCHANGED;
  if (deferred_key) {
    if (key->IsSmi()) key = factory()->NumberToString(key);
    SerializeDeferredKey(deferred_comma, key);
  }

  builder_.AppendString(Handle<String>::cast(result));
  return SUCCESS;
}


BasicJsonStringifier::Result BasicJsonStringifier::SerializeJSValue(
    Handle<JSValue> object) {
  String* class_name = object->class_name();
  if (class_name == isolate_->heap()->String_string()) {
    Handle<Object> value;
    ASSIGN_RETURN_ON_EXCEPTION_VALUE(
        isolate_, value, Object::ToString(isolate_, object), EXCEPTION);
    SerializeString(Handle<String>::cast(value));
  } else if (class_name == isolate_->heap()->Number_string()) {
    Handle<Object> value;
    ASSIGN_RETURN_ON_EXCEPTION_VALUE(isolate_, value, Object::ToNumber(object),
                                     EXCEPTION);
    if (value->IsSmi()) return SerializeSmi(Smi::cast(*value));
    SerializeHeapNumber(Handle<HeapNumber>::cast(value));
  } else if (class_name == isolate_->heap()->Boolean_string()) {
    Object* value = JSValue::cast(*object)->value();
    DCHECK(value->IsBoolean());
    builder_.AppendCString(value->IsTrue() ? "\x74\x72\x75\x65" : "\x66\x61\x6c\x73\x65");
  } else {
    // ES6 24.3.2.1 step 10.c, serialize as an ordinary JSObject.
    CHECK(!object->IsAccessCheckNeeded());
    CHECK(!object->IsJSGlobalProxy());
    return SerializeJSObject(object);
  }
  return SUCCESS;
}


BasicJsonStringifier::Result BasicJsonStringifier::SerializeSmi(Smi* object) {
  static const int kBufferSize = 100;
  char chars[kBufferSize];
  Vector<char> buffer(chars, kBufferSize);
  builder_.AppendCString(IntToCString(object->value(), buffer));
  return SUCCESS;
}


BasicJsonStringifier::Result BasicJsonStringifier::SerializeDouble(
    double number) {
  if (std::isinf(number) || std::isnan(number)) {
    builder_.AppendCString("\x6e\x75\x6c\x6c");
    return SUCCESS;
  }
  static const int kBufferSize = 100;
  char chars[kBufferSize];
  Vector<char> buffer(chars, kBufferSize);
  builder_.AppendCString(DoubleToCString(number, buffer));
  return SUCCESS;
}


BasicJsonStringifier::Result BasicJsonStringifier::SerializeJSArray(
    Handle<JSArray> object) {
  HandleScope handle_scope(isolate_);
  Result stack_push = StackPush(object);
  if (stack_push != SUCCESS) return stack_push;
  uint32_t length = 0;
  CHECK(object->length()->ToArrayLength(&length));
  builder_.AppendCharacter('\x5b');
  switch (object->GetElementsKind()) {
    case FAST_SMI_ELEMENTS: {
      Handle<FixedArray> elements(FixedArray::cast(object->elements()),
                                  isolate_);
      for (uint32_t i = 0; i < length; i++) {
        if (i > 0) builder_.AppendCharacter('\x2c');
        SerializeSmi(Smi::cast(elements->get(i)));
      }
      break;
    }
    case FAST_DOUBLE_ELEMENTS: {
      // Empty array is FixedArray but not FixedDoubleArray.
      if (length == 0) break;
      Handle<FixedDoubleArray> elements(
          FixedDoubleArray::cast(object->elements()), isolate_);
      for (uint32_t i = 0; i < length; i++) {
        if (i > 0) builder_.AppendCharacter('\x2c');
        SerializeDouble(elements->get_scalar(i));
      }
      break;
    }
    case FAST_ELEMENTS: {
      Handle<Object> old_length(object->length(), isolate_);
      for (uint32_t i = 0; i < length; i++) {
        if (object->length() != *old_length ||
            object->GetElementsKind() != FAST_ELEMENTS) {
          Result result = SerializeJSArraySlow(object, i, length);
          if (result != SUCCESS) return result;
          break;
        }
        if (i > 0) builder_.AppendCharacter('\x2c');
        Result result = SerializeElement(
            isolate_,
            Handle<Object>(FixedArray::cast(object->elements())->get(i),
                           isolate_),
            i);
        if (result == SUCCESS) continue;
        if (result == UNCHANGED) {
          builder_.AppendCString("\x6e\x75\x6c\x6c");
        } else {
          return result;
        }
      }
      break;
    }
    // The FAST_HOLEY_* cases could be handled in a faster way. They resemble
    // the non-holey cases except that a lookup is necessary for holes.
    default: {
      Result result = SerializeJSArraySlow(object, 0, length);
      if (result != SUCCESS) return result;
      break;
    }
  }
  builder_.AppendCharacter('\x5d');
  StackPop();
  return SUCCESS;
}


BasicJsonStringifier::Result BasicJsonStringifier::SerializeJSArraySlow(
    Handle<JSArray> object, uint32_t start, uint32_t length) {
  for (uint32_t i = start; i < length; i++) {
    if (i > 0) builder_.AppendCharacter('\x2c');
    Handle<Object> element;
    ASSIGN_RETURN_ON_EXCEPTION_VALUE(
        isolate_, element, JSReceiver::GetElement(isolate_, object, i),
        EXCEPTION);
    if (element->IsUndefined()) {
      builder_.AppendCString("\x6e\x75\x6c\x6c");
    } else {
      Result result = SerializeElement(isolate_, element, i);
      if (result == SUCCESS) continue;
      if (result == UNCHANGED) {
        builder_.AppendCString("\x6e\x75\x6c\x6c");
      } else {
        return result;
      }
    }
  }
  return SUCCESS;
}


BasicJsonStringifier::Result BasicJsonStringifier::SerializeJSObject(
    Handle<JSObject> object) {
  HandleScope handle_scope(isolate_);
  Result stack_push = StackPush(object);
  if (stack_push != SUCCESS) return stack_push;
  DCHECK(!object->IsJSGlobalProxy() && !object->IsJSGlobalObject());

  builder_.AppendCharacter('\x7b');
  bool comma = false;

  if (object->HasFastProperties() &&
      !object->HasIndexedInterceptor() &&
      !object->HasNamedInterceptor() &&
      object->elements()->length() == 0) {
    Handle<Map> map(object->map());
    for (int i = 0; i < map->NumberOfOwnDescriptors(); i++) {
      Handle<Name> name(map->instance_descriptors()->GetKey(i), isolate_);
      // TODO(rossberg): Should this throw?
      if (!name->IsString()) continue;
      Handle<String> key = Handle<String>::cast(name);
      PropertyDetails details = map->instance_descriptors()->GetDetails(i);
      if (details.IsDontEnum()) continue;
      Handle<Object> property;
      if (details.type() == DATA && *map == object->map()) {
        FieldIndex field_index = FieldIndex::ForDescriptor(*map, i);
        Isolate* isolate = object->GetIsolate();
        if (object->IsUnboxedDoubleField(field_index)) {
          double value = object->RawFastDoublePropertyAt(field_index);
          property = isolate->factory()->NewHeapNumber(value);

        } else {
          property = handle(object->RawFastPropertyAt(field_index), isolate);
        }
      } else {
        ASSIGN_RETURN_ON_EXCEPTION_VALUE(
            isolate_, property,
            Object::GetPropertyOrElement(object, key),
            EXCEPTION);
      }
      Result result = SerializeProperty(property, comma, key);
      if (!comma && result == SUCCESS) comma = true;
      if (result == EXCEPTION) return result;
    }
  } else {
    Handle<FixedArray> contents;
    ASSIGN_RETURN_ON_EXCEPTION_VALUE(
        isolate_, contents,
        JSReceiver::GetKeys(object, OWN_ONLY, ENUMERABLE_STRINGS), EXCEPTION);

    for (int i = 0; i < contents->length(); i++) {
      Object* key = contents->get(i);
      Handle<String> key_handle;
      MaybeHandle<Object> maybe_property;
      if (key->IsString()) {
        key_handle = Handle<String>(String::cast(key), isolate_);
        maybe_property = Object::GetPropertyOrElement(object, key_handle);
      } else {
        DCHECK(key->IsNumber());
        key_handle = factory()->NumberToString(Handle<Object>(key, isolate_));
        if (key->IsSmi()) {
          maybe_property =
              JSReceiver::GetElement(isolate_, object, Smi::cast(key)->value());
        } else {
          maybe_property = Object::GetPropertyOrElement(object, key_handle);
        }
      }
      Handle<Object> property;
      ASSIGN_RETURN_ON_EXCEPTION_VALUE(
          isolate_, property, maybe_property, EXCEPTION);
      Result result = SerializeProperty(property, comma, key_handle);
      if (!comma && result == SUCCESS) comma = true;
      if (result == EXCEPTION) return result;
    }
  }

  builder_.AppendCharacter('\x7d');
  StackPop();
  return SUCCESS;
}


template <typename SrcChar, typename DestChar>
void BasicJsonStringifier::SerializeStringUnchecked_(
    Vector<const SrcChar> src,
    IncrementalStringBuilder::NoExtend<DestChar>* dest) {
  // Assert that uc16 character is not truncated down to 8 bit.
  // The <uc16, char> version of this method must not be called.
  DCHECK(sizeof(DestChar) >= sizeof(SrcChar));

  for (int i = 0; i < src.length(); i++) {
    SrcChar c = src[i];
    if (DoNotEscape(c)) {
      dest->Append(c);
    } else {
      dest->AppendCString(&JsonEscapeTable[c * kJsonEscapeTableEntrySize]);
    }
  }
}


template <typename SrcChar, typename DestChar>
void BasicJsonStringifier::SerializeString_(Handle<String> string) {
  int length = string->length();
  builder_.Append<uint8_t, DestChar>('\x22');
  // We make a rough estimate to find out if the current string can be
  // serialized without allocating a new string part. The worst case length of
  // an escaped character is 6.  Shifting the remainin string length right by 3
  // is a more pessimistic estimate, but faster to calculate.
  int worst_case_length = length << 3;
  if (builder_.CurrentPartCanFit(worst_case_length)) {
    DisallowHeapAllocation no_gc;
    Vector<const SrcChar> vector = string->GetCharVector<SrcChar>();
    IncrementalStringBuilder::NoExtendBuilder<DestChar> no_extend(
        &builder_, worst_case_length);
    SerializeStringUnchecked_(vector, &no_extend);
  } else {
    FlatStringReader reader(isolate_, string);
    for (int i = 0; i < reader.length(); i++) {
      SrcChar c = reader.Get<SrcChar>(i);
      if (DoNotEscape(c)) {
        builder_.Append<SrcChar, DestChar>(c);
      } else {
        builder_.AppendCString(&JsonEscapeTable[c * kJsonEscapeTableEntrySize]);
      }
    }
  }

  builder_.Append<uint8_t, DestChar>('\x22');
}


template <>
bool BasicJsonStringifier::DoNotEscape(uint8_t c) {
  return c >= '\x23' && c <= '\x7e' && c != '\x5c';
}


template <>
bool BasicJsonStringifier::DoNotEscape(uint16_t c) {
  return c >= '\x23' && c != '\x5c' && c != 0x7f;
}


void BasicJsonStringifier::SerializeString(Handle<String> object) {
  object = String::Flatten(object);
  if (builder_.CurrentEncoding() == String::ONE_BYTE_ENCODING) {
    if (object->IsOneByteRepresentationUnderneath()) {
      SerializeString_<uint8_t, uint8_t>(object);
    } else {
      builder_.ChangeEncoding();
      SerializeString(object);
    }
  } else {
    if (object->IsOneByteRepresentationUnderneath()) {
      SerializeString_<uint8_t, uc16>(object);
    } else {
      SerializeString_<uc16, uc16>(object);
    }
  }
}

}  // namespace internal
}  // namespace v8

#endif  // V8_JSON_STRINGIFIER_H_
