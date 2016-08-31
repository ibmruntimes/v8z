// Copyright 2012 the V8 project authors. All rights reserved.
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.

#include "src/extensions/statistics-extension.h"

namespace v8 {
namespace internal {

const char* const StatisticsExtension::kSource =
    "\x6e\x61\x74\x69\x76\x65\x20\x66\x75\x6e\x63\x74\x69\x6f\x6e\x20\x67\x65\x74\x56\x38\x53\x74\x61\x74\x69\x73\x74\x69\x63\x73\x28\x29\x3b";


v8::Handle<v8::FunctionTemplate> StatisticsExtension::GetNativeFunctionTemplate(
    v8::Isolate* isolate,
    v8::Handle<v8::String> str) {
  DCHECK(strcmp(*v8::String::Utf8Value(str), "\x67\x65\x74\x56\x38\x53\x74\x61\x74\x69\x73\x74\x69\x63\x73") == 0);
  return v8::FunctionTemplate::New(isolate, StatisticsExtension::GetCounters);
}


static void AddCounter(v8::Isolate* isolate,
                       v8::Local<v8::Object> object,
                       StatsCounter* counter,
                       const char* name) {
  if (counter->Enabled()) {
    object->Set(v8::String::NewFromUtf8(isolate, name),
                v8::Number::New(isolate, *counter->GetInternalPointer()));
  }
}

static void AddNumber(v8::Isolate* isolate,
                      v8::Local<v8::Object> object,
                      intptr_t value,
                      const char* name) {
  object->Set(v8::String::NewFromUtf8(isolate, name),
              v8::Number::New(isolate, static_cast<double>(value)));
}


static void AddNumber64(v8::Isolate* isolate,
                        v8::Local<v8::Object> object,
                        int64_t value,
                        const char* name) {
  object->Set(v8::String::NewFromUtf8(isolate, name),
              v8::Number::New(isolate, static_cast<double>(value)));
}


void StatisticsExtension::GetCounters(
    const v8::FunctionCallbackInfo<v8::Value>& args) {
  Isolate* isolate = reinterpret_cast<Isolate*>(args.GetIsolate());
  Heap* heap = isolate->heap();

  if (args.Length() > 0) {  // GC if first argument evaluates to true.
    if (args[0]->IsBoolean() && args[0]->ToBoolean()->Value()) {
      heap->CollectAllGarbage(Heap::kNoGCFlags, "\x63\x6f\x75\x6e\x74\x65\x72\x73\x20\x65\x78\x74\x65\x6e\x73\x69\x6f\x6e");
    }
  }

  Counters* counters = isolate->counters();
  v8::Local<v8::Object> result = v8::Object::New(args.GetIsolate());

#define ADD_COUNTER(name, caption)                                            \
  AddCounter(args.GetIsolate(), result, counters->name(), #name);

  STATS_COUNTER_LIST_1(ADD_COUNTER)
  STATS_COUNTER_LIST_2(ADD_COUNTER)
#undef ADD_COUNTER
#define ADD_COUNTER(name)                                                      \
  AddCounter(args.GetIsolate(), result, counters->count_of_##name(),           \
             "\x63\x6f\x75\x6e\x74\x5f\x6f\x66\x5f" #name);                                               \
  AddCounter(args.GetIsolate(), result, counters->size_of_##name(),            \
             "\x73\x69\x7a\x65\x5f\x6f\x66\x5f" #name);

  INSTANCE_TYPE_LIST(ADD_COUNTER)
#undef ADD_COUNTER
#define ADD_COUNTER(name)                                                      \
  AddCounter(args.GetIsolate(), result, counters->count_of_CODE_TYPE_##name(), \
             "\x63\x6f\x75\x6e\x74\x5f\x6f\x66\x5f\x43\x4f\x44\x45\x5f\x54\x59\x50\x45\x5f" #name);                                     \
  AddCounter(args.GetIsolate(), result, counters->size_of_CODE_TYPE_##name(),  \
             "\x73\x69\x7a\x65\x5f\x6f\x66\x5f\x43\x4f\x44\x45\x5f\x54\x59\x50\x45\x5f" #name);

  CODE_KIND_LIST(ADD_COUNTER)
#undef ADD_COUNTER
#define ADD_COUNTER(name)                                                      \
  AddCounter(args.GetIsolate(), result,                                        \
             counters->count_of_FIXED_ARRAY_##name(),                          \
             "\x63\x6f\x75\x6e\x74\x5f\x6f\x66\x5f\x46\x49\x58\x45\x44\x5f\x41\x52\x52\x41\x59\x5f" #name);                                   \
  AddCounter(args.GetIsolate(), result,                                        \
             counters->size_of_FIXED_ARRAY_##name(),                           \
             "\x73\x69\x7a\x65\x5f\x6f\x66\x5f\x46\x49\x58\x45\x44\x5f\x41\x52\x52\x41\x59\x5f" #name);

  FIXED_ARRAY_SUB_INSTANCE_TYPE_LIST(ADD_COUNTER)
#undef ADD_COUNTER

  AddNumber(args.GetIsolate(), result, isolate->memory_allocator()->Size(),
            "\x74\x6f\x74\x61\x6c\x5f\x63\x6f\x6d\x6d\x69\x74\x74\x65\x64\x5f\x62\x79\x74\x65\x73");
  AddNumber(args.GetIsolate(), result, heap->new_space()->Size(),
            "\x6e\x65\x77\x5f\x73\x70\x61\x63\x65\x5f\x6c\x69\x76\x65\x5f\x62\x79\x74\x65\x73");
  AddNumber(args.GetIsolate(), result, heap->new_space()->Available(),
            "\x6e\x65\x77\x5f\x73\x70\x61\x63\x65\x5f\x61\x76\x61\x69\x6c\x61\x62\x6c\x65\x5f\x62\x79\x74\x65\x73");
  AddNumber(args.GetIsolate(), result, heap->new_space()->CommittedMemory(),
            "\x6e\x65\x77\x5f\x73\x70\x61\x63\x65\x5f\x63\x6f\x6d\x6d\x69\x74\x65\x64\x5f\x62\x79\x74\x65\x73");
  AddNumber(args.GetIsolate(), result, heap->old_pointer_space()->Size(),
            "\x6f\x6c\x64\x5f\x70\x6f\x69\x6e\x74\x65\x72\x5f\x73\x70\x61\x63\x65\x5f\x6c\x69\x76\x65\x5f\x62\x79\x74\x65\x73");
  AddNumber(args.GetIsolate(), result, heap->old_pointer_space()->Available(),
            "\x6f\x6c\x64\x5f\x70\x6f\x69\x6e\x74\x65\x72\x5f\x73\x70\x61\x63\x65\x5f\x61\x76\x61\x69\x6c\x61\x62\x6c\x65\x5f\x62\x79\x74\x65\x73");
  AddNumber(args.GetIsolate(), result,
            heap->old_pointer_space()->CommittedMemory(),
            "\x6f\x6c\x64\x5f\x70\x6f\x69\x6e\x74\x65\x72\x5f\x73\x70\x61\x63\x65\x5f\x63\x6f\x6d\x6d\x69\x74\x65\x64\x5f\x62\x79\x74\x65\x73");
  AddNumber(args.GetIsolate(), result, heap->old_data_space()->Size(),
            "\x6f\x6c\x64\x5f\x64\x61\x74\x61\x5f\x73\x70\x61\x63\x65\x5f\x6c\x69\x76\x65\x5f\x62\x79\x74\x65\x73");
  AddNumber(args.GetIsolate(), result, heap->old_data_space()->Available(),
            "\x6f\x6c\x64\x5f\x64\x61\x74\x61\x5f\x73\x70\x61\x63\x65\x5f\x61\x76\x61\x69\x6c\x61\x62\x6c\x65\x5f\x62\x79\x74\x65\x73");
  AddNumber(args.GetIsolate(), result,
            heap->old_data_space()->CommittedMemory(),
            "\x6f\x6c\x64\x5f\x64\x61\x74\x61\x5f\x73\x70\x61\x63\x65\x5f\x63\x6f\x6d\x6d\x69\x74\x65\x64\x5f\x62\x79\x74\x65\x73");
  AddNumber(args.GetIsolate(), result, heap->code_space()->Size(),
            "\x63\x6f\x64\x65\x5f\x73\x70\x61\x63\x65\x5f\x6c\x69\x76\x65\x5f\x62\x79\x74\x65\x73");
  AddNumber(args.GetIsolate(), result, heap->code_space()->Available(),
            "\x63\x6f\x64\x65\x5f\x73\x70\x61\x63\x65\x5f\x61\x76\x61\x69\x6c\x61\x62\x6c\x65\x5f\x62\x79\x74\x65\x73");
  AddNumber(args.GetIsolate(), result, heap->code_space()->CommittedMemory(),
            "\x63\x6f\x64\x65\x5f\x73\x70\x61\x63\x65\x5f\x63\x6f\x6d\x6d\x69\x74\x65\x64\x5f\x62\x79\x74\x65\x73");
  AddNumber(args.GetIsolate(), result, heap->cell_space()->Size(),
            "\x63\x65\x6c\x6c\x5f\x73\x70\x61\x63\x65\x5f\x6c\x69\x76\x65\x5f\x62\x79\x74\x65\x73");
  AddNumber(args.GetIsolate(), result, heap->cell_space()->Available(),
            "\x63\x65\x6c\x6c\x5f\x73\x70\x61\x63\x65\x5f\x61\x76\x61\x69\x6c\x61\x62\x6c\x65\x5f\x62\x79\x74\x65\x73");
  AddNumber(args.GetIsolate(), result, heap->cell_space()->CommittedMemory(),
            "\x63\x65\x6c\x6c\x5f\x73\x70\x61\x63\x65\x5f\x63\x6f\x6d\x6d\x69\x74\x65\x64\x5f\x62\x79\x74\x65\x73");
  AddNumber(args.GetIsolate(), result, heap->property_cell_space()->Size(),
            "\x70\x72\x6f\x70\x65\x72\x74\x79\x5f\x63\x65\x6c\x6c\x5f\x73\x70\x61\x63\x65\x5f\x6c\x69\x76\x65\x5f\x62\x79\x74\x65\x73");
  AddNumber(args.GetIsolate(), result, heap->property_cell_space()->Available(),
            "\x70\x72\x6f\x70\x65\x72\x74\x79\x5f\x63\x65\x6c\x6c\x5f\x73\x70\x61\x63\x65\x5f\x61\x76\x61\x69\x6c\x61\x62\x6c\x65\x5f\x62\x79\x74\x65\x73");
  AddNumber(args.GetIsolate(), result,
            heap->property_cell_space()->CommittedMemory(),
            "\x70\x72\x6f\x70\x65\x72\x74\x79\x5f\x63\x65\x6c\x6c\x5f\x73\x70\x61\x63\x65\x5f\x63\x6f\x6d\x6d\x69\x74\x65\x64\x5f\x62\x79\x74\x65\x73");
  AddNumber(args.GetIsolate(), result, heap->lo_space()->Size(),
            "\x6c\x6f\x5f\x73\x70\x61\x63\x65\x5f\x6c\x69\x76\x65\x5f\x62\x79\x74\x65\x73");
  AddNumber(args.GetIsolate(), result, heap->lo_space()->Available(),
            "\x6c\x6f\x5f\x73\x70\x61\x63\x65\x5f\x61\x76\x61\x69\x6c\x61\x62\x6c\x65\x5f\x62\x79\x74\x65\x73");
  AddNumber(args.GetIsolate(), result, heap->lo_space()->CommittedMemory(),
            "\x6c\x6f\x5f\x73\x70\x61\x63\x65\x5f\x63\x6f\x6d\x6d\x69\x74\x65\x64\x5f\x62\x79\x74\x65\x73");
  AddNumber64(args.GetIsolate(), result,
              heap->amount_of_external_allocated_memory(),
              "\x61\x6d\x6f\x75\x6e\x74\x5f\x6f\x66\x5f\x65\x78\x74\x65\x72\x6e\x61\x6c\x5f\x61\x6c\x6c\x6f\x63\x61\x74\x65\x64\x5f\x6d\x65\x6d\x6f\x72\x79");
  args.GetReturnValue().Set(result);
}

} }  // namespace v8::internal
