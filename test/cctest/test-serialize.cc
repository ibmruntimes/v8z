// Copyright 2007-2010 the V8 project authors. All rights reserved.
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

#include <signal.h>

#include <sys/stat.h>

#include "src/v8.h"

#include "src/bootstrapper.h"
#include "src/compilation-cache.h"
#include "src/debug.h"
#include "src/heap/spaces.h"
#include "src/ic-inl.h"
#include "src/natives.h"
#include "src/objects.h"
#include "src/runtime.h"
#include "src/scopeinfo.h"
#include "src/serialize.h"
#include "src/snapshot.h"
#include "test/cctest/cctest.h"

using namespace v8::internal;

static const unsigned kCounters = 256;
static int local_counters[kCounters];
static const char* local_counter_names[kCounters];


static unsigned CounterHash(const char* s) {
  unsigned hash = 0;
  while (*++s) {
    hash |= hash << 5;
    hash += *s;
  }
  return hash;
}


// Callback receiver to track counters in test.
static int* counter_function(const char* name) {
  unsigned hash = CounterHash(name) % kCounters;
  unsigned original_hash = hash;
  USE(original_hash);
  while (true) {
    if (local_counter_names[hash] == name) {
      return &local_counters[hash];
    }
    if (local_counter_names[hash] == 0) {
      local_counter_names[hash] = name;
      return &local_counters[hash];
    }
    if (strcmp(local_counter_names[hash], name) == 0) {
      return &local_counters[hash];
    }
    hash = (hash + 1) % kCounters;
    DCHECK(hash != original_hash);  // Hash table has been filled up.
  }
}


template <class T>
static Address AddressOf(T id) {
  return ExternalReference(id, CcTest::i_isolate()).address();
}


template <class T>
static uint32_t Encode(const ExternalReferenceEncoder& encoder, T id) {
  return encoder.Encode(AddressOf(id));
}


static int make_code(TypeCode type, int id) {
  return static_cast<uint32_t>(type) << kReferenceTypeShift | id;
}


TEST(ExternalReferenceEncoder) {
  Isolate* isolate = CcTest::i_isolate();
  isolate->stats_table()->SetCounterFunction(counter_function);
  v8::V8::Initialize();

  ExternalReferenceEncoder encoder(isolate);
  CHECK_EQ(make_code(BUILTIN, Builtins::kArrayCode),
           Encode(encoder, Builtins::kArrayCode));
  CHECK_EQ(make_code(v8::internal::RUNTIME_FUNCTION, Runtime::kAbort),
           Encode(encoder, Runtime::kAbort));
  ExternalReference total_compile_size =
      ExternalReference(isolate->counters()->total_compile_size());
  CHECK_EQ(make_code(STATS_COUNTER, Counters::k_total_compile_size),
           encoder.Encode(total_compile_size.address()));
  ExternalReference stack_limit_address =
      ExternalReference::address_of_stack_limit(isolate);
  CHECK_EQ(make_code(UNCLASSIFIED, 2),
           encoder.Encode(stack_limit_address.address()));
  ExternalReference real_stack_limit_address =
      ExternalReference::address_of_real_stack_limit(isolate);
  CHECK_EQ(make_code(UNCLASSIFIED, 3),
           encoder.Encode(real_stack_limit_address.address()));
  CHECK_EQ(make_code(UNCLASSIFIED, 8),
           encoder.Encode(ExternalReference::debug_break(isolate).address()));
  CHECK_EQ(
      make_code(UNCLASSIFIED, 4),
      encoder.Encode(ExternalReference::new_space_start(isolate).address()));
  CHECK_EQ(
      make_code(UNCLASSIFIED, 1),
      encoder.Encode(ExternalReference::roots_array_start(isolate).address()));
  CHECK_EQ(make_code(UNCLASSIFIED, 34),
           encoder.Encode(ExternalReference::cpu_features().address()));
}


TEST(ExternalReferenceDecoder) {
  Isolate* isolate = CcTest::i_isolate();
  isolate->stats_table()->SetCounterFunction(counter_function);
  v8::V8::Initialize();

  ExternalReferenceDecoder decoder(isolate);
  CHECK_EQ(AddressOf(Builtins::kArrayCode),
           decoder.Decode(make_code(BUILTIN, Builtins::kArrayCode)));
  CHECK_EQ(AddressOf(Runtime::kAbort),
           decoder.Decode(make_code(v8::internal::RUNTIME_FUNCTION,
                                    Runtime::kAbort)));
  ExternalReference total_compile_size =
      ExternalReference(isolate->counters()->total_compile_size());
  CHECK_EQ(total_compile_size.address(),
           decoder.Decode(
               make_code(STATS_COUNTER,
                         Counters::k_total_compile_size)));
  CHECK_EQ(ExternalReference::address_of_stack_limit(isolate).address(),
           decoder.Decode(make_code(UNCLASSIFIED, 2)));
  CHECK_EQ(ExternalReference::address_of_real_stack_limit(isolate).address(),
           decoder.Decode(make_code(UNCLASSIFIED, 3)));
  CHECK_EQ(ExternalReference::debug_break(isolate).address(),
           decoder.Decode(make_code(UNCLASSIFIED, 8)));
  CHECK_EQ(ExternalReference::new_space_start(isolate).address(),
           decoder.Decode(make_code(UNCLASSIFIED, 4)));
}


class FileByteSink : public SnapshotByteSink {
 public:
  explicit FileByteSink(const char* snapshot_file) {
    fp_ = v8::base::OS::FOpen(snapshot_file, "\x77\x62");
    file_name_ = snapshot_file;
    if (fp_ == NULL) {
      PrintF("\x55\x6e\x61\x62\x6c\x65\x20\x74\x6f\x20\x77\x72\x69\x74\x65\x20\x74\x6f\x20\x73\x6e\x61\x70\x73\x68\x6f\x74\x20\x66\x69\x6c\x65\x20\x22\x6c\xa2\x22\xa", snapshot_file);
      exit(1);
    }
  }
  virtual ~FileByteSink() {
    if (fp_ != NULL) {
      fclose(fp_);
    }
  }
  virtual void Put(byte b, const char* description) {
    if (fp_ != NULL) {
      fputc(b, fp_);
    }
  }
  virtual int Position() {
    return ftell(fp_);
  }
  void WriteSpaceUsed(
      int new_space_used,
      int pointer_space_used,
      int data_space_used,
      int code_space_used,
      int map_space_used,
      int cell_space_used,
      int property_cell_space_used);

 private:
  FILE* fp_;
  const char* file_name_;
};


void FileByteSink::WriteSpaceUsed(
      int new_space_used,
      int pointer_space_used,
      int data_space_used,
      int code_space_used,
      int map_space_used,
      int cell_space_used,
      int property_cell_space_used) {
  int file_name_length = StrLength(file_name_) + 10;
  Vector<char> name = Vector<char>::New(file_name_length + 1);
  SNPrintF(name, "\x6c\xa2\x2e\x73\x69\x7a\x65", file_name_);
  FILE* fp = v8::base::OS::FOpen(name.start(), "\x77");
  name.Dispose();
  fprintf(fp, "\x6e\x65\x77\x20\x6c\x84\xa", new_space_used);
  fprintf(fp, "\x70\x6f\x69\x6e\x74\x65\x72\x20\x6c\x84\xa", pointer_space_used);
  fprintf(fp, "\x64\x61\x74\x61\x20\x6c\x84\xa", data_space_used);
  fprintf(fp, "\x63\x6f\x64\x65\x20\x6c\x84\xa", code_space_used);
  fprintf(fp, "\x6d\x61\x70\x20\x6c\x84\xa", map_space_used);
  fprintf(fp, "\x63\x65\x6c\x6c\x20\x6c\x84\xa", cell_space_used);
  fprintf(fp, "\x70\x72\x6f\x70\x65\x72\x74\x79\x20\x63\x65\x6c\x6c\x20\x6c\x84\xa", property_cell_space_used);
  fclose(fp);
}


static bool WriteToFile(Isolate* isolate, const char* snapshot_file) {
  FileByteSink file(snapshot_file);
  StartupSerializer ser(isolate, &file);
  ser.Serialize();

  file.WriteSpaceUsed(
      ser.CurrentAllocationAddress(NEW_SPACE),
      ser.CurrentAllocationAddress(OLD_POINTER_SPACE),
      ser.CurrentAllocationAddress(OLD_DATA_SPACE),
      ser.CurrentAllocationAddress(CODE_SPACE),
      ser.CurrentAllocationAddress(MAP_SPACE),
      ser.CurrentAllocationAddress(CELL_SPACE),
      ser.CurrentAllocationAddress(PROPERTY_CELL_SPACE));

  return true;
}


static void Serialize() {
  // We have to create one context.  One reason for this is so that the builtins
  // can be loaded from v8natives.js and their addresses can be processed.  This
  // will clear the pending fixups array, which would otherwise contain GC roots
  // that would confuse the serialization/deserialization process.
  v8::Isolate* isolate = CcTest::isolate();
  {
    v8::HandleScope scope(isolate);
    v8::Context::New(isolate);
  }

  Isolate* internal_isolate = CcTest::i_isolate();
  internal_isolate->heap()->CollectAllGarbage(Heap::kNoGCFlags, "\x73\x65\x72\x69\x61\x6c\x69\x7a\x65");
  WriteToFile(internal_isolate, FLAG_testing_serialization_file);
}


// Test that the whole heap can be serialized.
TEST(Serialize) {
  if (!Snapshot::HaveASnapshotToStartFrom()) {
    CcTest::i_isolate()->enable_serializer();
    v8::V8::Initialize();
    Serialize();
  }
}


// Test that heap serialization is non-destructive.
TEST(SerializeTwice) {
  if (!Snapshot::HaveASnapshotToStartFrom()) {
    CcTest::i_isolate()->enable_serializer();
    v8::V8::Initialize();
    Serialize();
    Serialize();
  }
}


//----------------------------------------------------------------------------
// Tests that the heap can be deserialized.


static void ReserveSpaceForSnapshot(Deserializer* deserializer,
                                    const char* file_name) {
  int file_name_length = StrLength(file_name) + 10;
  Vector<char> name = Vector<char>::New(file_name_length + 1);
  SNPrintF(name, "\x6c\xa2\x2e\x73\x69\x7a\x65", file_name);
  FILE* fp = v8::base::OS::FOpen(name.start(), "\x72");
  name.Dispose();
  int new_size, pointer_size, data_size, code_size, map_size, cell_size,
      property_cell_size;
#ifdef _MSC_VER
  // Avoid warning about unsafe fscanf from MSVC.
  // Please note that this is only fine if %c and %s are not being used.
#define fscanf fscanf_s
#endif
  CHECK_EQ(1, fscanf(fp, "\x6e\x65\x77\x20\x6c\x84\xa", &new_size));
  CHECK_EQ(1, fscanf(fp, "\x70\x6f\x69\x6e\x74\x65\x72\x20\x6c\x84\xa", &pointer_size));
  CHECK_EQ(1, fscanf(fp, "\x64\x61\x74\x61\x20\x6c\x84\xa", &data_size));
  CHECK_EQ(1, fscanf(fp, "\x63\x6f\x64\x65\x20\x6c\x84\xa", &code_size));
  CHECK_EQ(1, fscanf(fp, "\x6d\x61\x70\x20\x6c\x84\xa", &map_size));
  CHECK_EQ(1, fscanf(fp, "\x63\x65\x6c\x6c\x20\x6c\x84\xa", &cell_size));
  CHECK_EQ(1, fscanf(fp, "\x70\x72\x6f\x70\x65\x72\x74\x79\x20\x63\x65\x6c\x6c\x20\x6c\x84\xa", &property_cell_size));
#ifdef _MSC_VER
#undef fscanf
#endif
  fclose(fp);
  deserializer->set_reservation(NEW_SPACE, new_size);
  deserializer->set_reservation(OLD_POINTER_SPACE, pointer_size);
  deserializer->set_reservation(OLD_DATA_SPACE, data_size);
  deserializer->set_reservation(CODE_SPACE, code_size);
  deserializer->set_reservation(MAP_SPACE, map_size);
  deserializer->set_reservation(CELL_SPACE, cell_size);
  deserializer->set_reservation(PROPERTY_CELL_SPACE, property_cell_size);
}


bool InitializeFromFile(const char* snapshot_file) {
  int len;
  byte* str = ReadBytes(snapshot_file, &len);
  if (!str) return false;
  bool success;
  {
    SnapshotByteSource source(str, len);
    Deserializer deserializer(&source);
    ReserveSpaceForSnapshot(&deserializer, snapshot_file);
    success = V8::Initialize(&deserializer);
  }
  DeleteArray(str);
  return success;
}


static void Deserialize() {
  CHECK(InitializeFromFile(FLAG_testing_serialization_file));
}


static void SanityCheck() {
  Isolate* isolate = CcTest::i_isolate();
  v8::HandleScope scope(CcTest::isolate());
#ifdef VERIFY_HEAP
  CcTest::heap()->Verify();
#endif
  CHECK(isolate->global_object()->IsJSObject());
  CHECK(isolate->native_context()->IsContext());
  CHECK(CcTest::heap()->string_table()->IsStringTable());
  isolate->factory()->InternalizeOneByteString(STATIC_ASCII_VECTOR("\x45\x6d\x70\x74\x79"));
}


DEPENDENT_TEST(Deserialize, Serialize) {
  // The serialize-deserialize tests only work if the VM is built without
  // serialization.  That doesn't matter.  We don't need to be able to
  // serialize a snapshot in a VM that is booted from a snapshot.
  if (!Snapshot::HaveASnapshotToStartFrom()) {
    v8::Isolate* isolate = CcTest::isolate();
    v8::HandleScope scope(isolate);
    Deserialize();

    v8::Local<v8::Context> env = v8::Context::New(isolate);
    env->Enter();

    SanityCheck();
  }
}


DEPENDENT_TEST(DeserializeFromSecondSerialization, SerializeTwice) {
  if (!Snapshot::HaveASnapshotToStartFrom()) {
    v8::Isolate* isolate = CcTest::isolate();
    v8::HandleScope scope(isolate);
    Deserialize();

    v8::Local<v8::Context> env = v8::Context::New(isolate);
    env->Enter();

    SanityCheck();
  }
}


DEPENDENT_TEST(DeserializeAndRunScript2, Serialize) {
  if (!Snapshot::HaveASnapshotToStartFrom()) {
    v8::Isolate* isolate = CcTest::isolate();
    v8::HandleScope scope(isolate);
    Deserialize();

    v8::Local<v8::Context> env = v8::Context::New(isolate);
    env->Enter();

    const char* c_source = "\x22\x31\x32\x33\x34\x22\x2e\x6c\x65\x6e\x67\x74\x68";
    v8::Local<v8::String> source = v8::String::NewFromUtf8(isolate, c_source);
    v8::Local<v8::Script> script = v8::Script::Compile(source);
    CHECK_EQ(4, script->Run()->Int32Value());
  }
}


DEPENDENT_TEST(DeserializeFromSecondSerializationAndRunScript2,
               SerializeTwice) {
  if (!Snapshot::HaveASnapshotToStartFrom()) {
    v8::Isolate* isolate = CcTest::isolate();
    v8::HandleScope scope(isolate);
    Deserialize();

    v8::Local<v8::Context> env = v8::Context::New(isolate);
    env->Enter();

    const char* c_source = "\x22\x31\x32\x33\x34\x22\x2e\x6c\x65\x6e\x67\x74\x68";
    v8::Local<v8::String> source = v8::String::NewFromUtf8(isolate, c_source);
    v8::Local<v8::Script> script = v8::Script::Compile(source);
    CHECK_EQ(4, script->Run()->Int32Value());
  }
}


TEST(PartialSerialization) {
  if (!Snapshot::HaveASnapshotToStartFrom()) {
    Isolate* isolate = CcTest::i_isolate();
    CcTest::i_isolate()->enable_serializer();
    v8::V8::Initialize();
    v8::Isolate* v8_isolate = reinterpret_cast<v8::Isolate*>(isolate);
    Heap* heap = isolate->heap();

    v8::Persistent<v8::Context> env;
    {
      HandleScope scope(isolate);
      env.Reset(v8_isolate, v8::Context::New(v8_isolate));
    }
    DCHECK(!env.IsEmpty());
    {
      v8::HandleScope handle_scope(v8_isolate);
      v8::Local<v8::Context>::New(v8_isolate, env)->Enter();
    }
    // Make sure all builtin scripts are cached.
    { HandleScope scope(isolate);
      for (int i = 0; i < Natives::GetBuiltinsCount(); i++) {
        isolate->bootstrapper()->NativesSourceLookup(i);
      }
    }
    heap->CollectAllGarbage(Heap::kNoGCFlags);
    heap->CollectAllGarbage(Heap::kNoGCFlags);

    Object* raw_foo;
    {
      v8::HandleScope handle_scope(v8_isolate);
      v8::Local<v8::String> foo = v8::String::NewFromUtf8(v8_isolate, "\x66\x6f\x6f");
      DCHECK(!foo.IsEmpty());
      raw_foo = *(v8::Utils::OpenHandle(*foo));
    }

    int file_name_length = StrLength(FLAG_testing_serialization_file) + 10;
    Vector<char> startup_name = Vector<char>::New(file_name_length + 1);
    SNPrintF(startup_name, "\x6c\xa2\x2e\x73\x74\x61\x72\x74\x75\x70", FLAG_testing_serialization_file);

    {
      v8::HandleScope handle_scope(v8_isolate);
      v8::Local<v8::Context>::New(v8_isolate, env)->Exit();
    }
    env.Reset();

    FileByteSink startup_sink(startup_name.start());
    StartupSerializer startup_serializer(isolate, &startup_sink);
    startup_serializer.SerializeStrongReferences();

    FileByteSink partial_sink(FLAG_testing_serialization_file);
    PartialSerializer p_ser(isolate, &startup_serializer, &partial_sink);
    p_ser.Serialize(&raw_foo);
    startup_serializer.SerializeWeakReferences();

    partial_sink.WriteSpaceUsed(
        p_ser.CurrentAllocationAddress(NEW_SPACE),
        p_ser.CurrentAllocationAddress(OLD_POINTER_SPACE),
        p_ser.CurrentAllocationAddress(OLD_DATA_SPACE),
        p_ser.CurrentAllocationAddress(CODE_SPACE),
        p_ser.CurrentAllocationAddress(MAP_SPACE),
        p_ser.CurrentAllocationAddress(CELL_SPACE),
        p_ser.CurrentAllocationAddress(PROPERTY_CELL_SPACE));

    startup_sink.WriteSpaceUsed(
        startup_serializer.CurrentAllocationAddress(NEW_SPACE),
        startup_serializer.CurrentAllocationAddress(OLD_POINTER_SPACE),
        startup_serializer.CurrentAllocationAddress(OLD_DATA_SPACE),
        startup_serializer.CurrentAllocationAddress(CODE_SPACE),
        startup_serializer.CurrentAllocationAddress(MAP_SPACE),
        startup_serializer.CurrentAllocationAddress(CELL_SPACE),
        startup_serializer.CurrentAllocationAddress(PROPERTY_CELL_SPACE));
    startup_name.Dispose();
  }
}


DEPENDENT_TEST(PartialDeserialization, PartialSerialization) {
  if (!Snapshot::HaveASnapshotToStartFrom()) {
    int file_name_length = StrLength(FLAG_testing_serialization_file) + 10;
    Vector<char> startup_name = Vector<char>::New(file_name_length + 1);
    SNPrintF(startup_name, "\x6c\xa2\x2e\x73\x74\x61\x72\x74\x75\x70", FLAG_testing_serialization_file);

    CHECK(InitializeFromFile(startup_name.start()));
    startup_name.Dispose();

    const char* file_name = FLAG_testing_serialization_file;

    int snapshot_size = 0;
    byte* snapshot = ReadBytes(file_name, &snapshot_size);

    Isolate* isolate = CcTest::i_isolate();
    Object* root;
    {
      SnapshotByteSource source(snapshot, snapshot_size);
      Deserializer deserializer(&source);
      ReserveSpaceForSnapshot(&deserializer, file_name);
      deserializer.DeserializePartial(isolate, &root);
      CHECK(root->IsString());
    }
    HandleScope handle_scope(isolate);
    Handle<Object> root_handle(root, isolate);


    Object* root2;
    {
      SnapshotByteSource source(snapshot, snapshot_size);
      Deserializer deserializer(&source);
      ReserveSpaceForSnapshot(&deserializer, file_name);
      deserializer.DeserializePartial(isolate, &root2);
      CHECK(root2->IsString());
      CHECK(*root_handle == root2);
    }
  }
}


TEST(ContextSerialization) {
  if (!Snapshot::HaveASnapshotToStartFrom()) {
    Isolate* isolate = CcTest::i_isolate();
    CcTest::i_isolate()->enable_serializer();
    v8::V8::Initialize();
    v8::Isolate* v8_isolate = reinterpret_cast<v8::Isolate*>(isolate);
    Heap* heap = isolate->heap();

    v8::Persistent<v8::Context> env;
    {
      HandleScope scope(isolate);
      env.Reset(v8_isolate, v8::Context::New(v8_isolate));
    }
    DCHECK(!env.IsEmpty());
    {
      v8::HandleScope handle_scope(v8_isolate);
      v8::Local<v8::Context>::New(v8_isolate, env)->Enter();
    }
    // Make sure all builtin scripts are cached.
    { HandleScope scope(isolate);
      for (int i = 0; i < Natives::GetBuiltinsCount(); i++) {
        isolate->bootstrapper()->NativesSourceLookup(i);
      }
    }
    // If we don't do this then we end up with a stray root pointing at the
    // context even after we have disposed of env.
    heap->CollectAllGarbage(Heap::kNoGCFlags);

    int file_name_length = StrLength(FLAG_testing_serialization_file) + 10;
    Vector<char> startup_name = Vector<char>::New(file_name_length + 1);
    SNPrintF(startup_name, "\x6c\xa2\x2e\x73\x74\x61\x72\x74\x75\x70", FLAG_testing_serialization_file);

    {
      v8::HandleScope handle_scope(v8_isolate);
      v8::Local<v8::Context>::New(v8_isolate, env)->Exit();
    }

    i::Object* raw_context = *v8::Utils::OpenPersistent(env);

    env.Reset();

    FileByteSink startup_sink(startup_name.start());
    StartupSerializer startup_serializer(isolate, &startup_sink);
    startup_serializer.SerializeStrongReferences();

    FileByteSink partial_sink(FLAG_testing_serialization_file);
    PartialSerializer p_ser(isolate, &startup_serializer, &partial_sink);
    p_ser.Serialize(&raw_context);
    startup_serializer.SerializeWeakReferences();

    partial_sink.WriteSpaceUsed(
        p_ser.CurrentAllocationAddress(NEW_SPACE),
        p_ser.CurrentAllocationAddress(OLD_POINTER_SPACE),
        p_ser.CurrentAllocationAddress(OLD_DATA_SPACE),
        p_ser.CurrentAllocationAddress(CODE_SPACE),
        p_ser.CurrentAllocationAddress(MAP_SPACE),
        p_ser.CurrentAllocationAddress(CELL_SPACE),
        p_ser.CurrentAllocationAddress(PROPERTY_CELL_SPACE));

    startup_sink.WriteSpaceUsed(
        startup_serializer.CurrentAllocationAddress(NEW_SPACE),
        startup_serializer.CurrentAllocationAddress(OLD_POINTER_SPACE),
        startup_serializer.CurrentAllocationAddress(OLD_DATA_SPACE),
        startup_serializer.CurrentAllocationAddress(CODE_SPACE),
        startup_serializer.CurrentAllocationAddress(MAP_SPACE),
        startup_serializer.CurrentAllocationAddress(CELL_SPACE),
        startup_serializer.CurrentAllocationAddress(PROPERTY_CELL_SPACE));
    startup_name.Dispose();
  }
}


DEPENDENT_TEST(ContextDeserialization, ContextSerialization) {
  if (!Snapshot::HaveASnapshotToStartFrom()) {
    int file_name_length = StrLength(FLAG_testing_serialization_file) + 10;
    Vector<char> startup_name = Vector<char>::New(file_name_length + 1);
    SNPrintF(startup_name, "\x6c\xa2\x2e\x73\x74\x61\x72\x74\x75\x70", FLAG_testing_serialization_file);

    CHECK(InitializeFromFile(startup_name.start()));
    startup_name.Dispose();

    const char* file_name = FLAG_testing_serialization_file;

    int snapshot_size = 0;
    byte* snapshot = ReadBytes(file_name, &snapshot_size);

    Isolate* isolate = CcTest::i_isolate();
    Object* root;
    {
      SnapshotByteSource source(snapshot, snapshot_size);
      Deserializer deserializer(&source);
      ReserveSpaceForSnapshot(&deserializer, file_name);
      deserializer.DeserializePartial(isolate, &root);
      CHECK(root->IsContext());
    }
    HandleScope handle_scope(isolate);
    Handle<Object> root_handle(root, isolate);


    Object* root2;
    {
      SnapshotByteSource source(snapshot, snapshot_size);
      Deserializer deserializer(&source);
      ReserveSpaceForSnapshot(&deserializer, file_name);
      deserializer.DeserializePartial(isolate, &root2);
      CHECK(root2->IsContext());
      CHECK(*root_handle != root2);
    }
  }
}


TEST(TestThatAlwaysSucceeds) {
}


TEST(TestThatAlwaysFails) {
  bool ArtificialFailure = false;
  CHECK(ArtificialFailure);
}


DEPENDENT_TEST(DependentTestThatAlwaysFails, TestThatAlwaysSucceeds) {
  bool ArtificialFailure2 = false;
  CHECK(ArtificialFailure2);
}


int CountBuiltins() {
  // Check that we have not deserialized any additional builtin.
  HeapIterator iterator(CcTest::heap());
  DisallowHeapAllocation no_allocation;
  int counter = 0;
  for (HeapObject* obj = iterator.next(); obj != NULL; obj = iterator.next()) {
    if (obj->IsCode() && Code::cast(obj)->kind() == Code::BUILTIN) counter++;
  }
  return counter;
}


TEST(SerializeToplevelOnePlusOne) {
  FLAG_serialize_toplevel = true;
  LocalContext context;
  Isolate* isolate = CcTest::i_isolate();
  isolate->compilation_cache()->Disable();  // Disable same-isolate code cache.

  v8::HandleScope scope(CcTest::isolate());

  const char* source = "\x31\x20\x2b\x20\x31";

  Handle<String> orig_source = isolate->factory()
                                   ->NewStringFromUtf8(CStrVector(source))
                                   .ToHandleChecked();
  Handle<String> copy_source = isolate->factory()
                                   ->NewStringFromUtf8(CStrVector(source))
                                   .ToHandleChecked();
  CHECK(!orig_source.is_identical_to(copy_source));
  CHECK(orig_source->Equals(*copy_source));

  ScriptData* cache = NULL;

  Handle<SharedFunctionInfo> orig = Compiler::CompileScript(
      orig_source, Handle<String>(), 0, 0, false,
      Handle<Context>(isolate->native_context()), NULL, &cache,
      v8::ScriptCompiler::kProduceCodeCache, NOT_NATIVES_CODE);

  int builtins_count = CountBuiltins();

  Handle<SharedFunctionInfo> copy;
  {
    DisallowCompilation no_compile_expected(isolate);
    copy = Compiler::CompileScript(
        copy_source, Handle<String>(), 0, 0, false,
        Handle<Context>(isolate->native_context()), NULL, &cache,
        v8::ScriptCompiler::kConsumeCodeCache, NOT_NATIVES_CODE);
  }

  CHECK_NE(*orig, *copy);
  CHECK(Script::cast(copy->script())->source() == *copy_source);

  Handle<JSFunction> copy_fun =
      isolate->factory()->NewFunctionFromSharedFunctionInfo(
          copy, isolate->native_context());
  Handle<JSObject> global(isolate->context()->global_object());
  Handle<Object> copy_result =
      Execution::Call(isolate, copy_fun, global, 0, NULL).ToHandleChecked();
  CHECK_EQ(2, Handle<Smi>::cast(copy_result)->value());

  CHECK_EQ(builtins_count, CountBuiltins());

  delete cache;
}


TEST(SerializeToplevelInternalizedString) {
  FLAG_serialize_toplevel = true;
  LocalContext context;
  Isolate* isolate = CcTest::i_isolate();
  isolate->compilation_cache()->Disable();  // Disable same-isolate code cache.

  v8::HandleScope scope(CcTest::isolate());

  const char* source = "\x27\x73\x74\x72\x69\x6e\x67\x31\x27";

  Handle<String> orig_source = isolate->factory()
                                   ->NewStringFromUtf8(CStrVector(source))
                                   .ToHandleChecked();
  Handle<String> copy_source = isolate->factory()
                                   ->NewStringFromUtf8(CStrVector(source))
                                   .ToHandleChecked();
  CHECK(!orig_source.is_identical_to(copy_source));
  CHECK(orig_source->Equals(*copy_source));

  Handle<JSObject> global(isolate->context()->global_object());
  ScriptData* cache = NULL;

  Handle<SharedFunctionInfo> orig = Compiler::CompileScript(
      orig_source, Handle<String>(), 0, 0, false,
      Handle<Context>(isolate->native_context()), NULL, &cache,
      v8::ScriptCompiler::kProduceCodeCache, NOT_NATIVES_CODE);
  Handle<JSFunction> orig_fun =
      isolate->factory()->NewFunctionFromSharedFunctionInfo(
          orig, isolate->native_context());
  Handle<Object> orig_result =
      Execution::Call(isolate, orig_fun, global, 0, NULL).ToHandleChecked();
  CHECK(orig_result->IsInternalizedString());

  int builtins_count = CountBuiltins();

  Handle<SharedFunctionInfo> copy;
  {
    DisallowCompilation no_compile_expected(isolate);
    copy = Compiler::CompileScript(
        copy_source, Handle<String>(), 0, 0, false,
        Handle<Context>(isolate->native_context()), NULL, &cache,
        v8::ScriptCompiler::kConsumeCodeCache, NOT_NATIVES_CODE);
  }
  CHECK_NE(*orig, *copy);
  CHECK(Script::cast(copy->script())->source() == *copy_source);

  Handle<JSFunction> copy_fun =
      isolate->factory()->NewFunctionFromSharedFunctionInfo(
          copy, isolate->native_context());
  CHECK_NE(*orig_fun, *copy_fun);
  Handle<Object> copy_result =
      Execution::Call(isolate, copy_fun, global, 0, NULL).ToHandleChecked();
  CHECK(orig_result.is_identical_to(copy_result));
  Handle<String> expected =
      isolate->factory()->NewStringFromAsciiChecked("\x73\x74\x72\x69\x6e\x67\x31");

  CHECK(Handle<String>::cast(copy_result)->Equals(*expected));
  CHECK_EQ(builtins_count, CountBuiltins());

  delete cache;
}


TEST(SerializeToplevelIsolates) {
  FLAG_serialize_toplevel = true;

  const char* source = "\x66\x75\x6e\x63\x74\x69\x6f\x6e\x20\x66\x28\x29\x20\x7b\x20\x72\x65\x74\x75\x72\x6e\x20\x27\x61\x62\x63\x27\x3b\x20\x7d\x3b\x20\x66\x28\x29\x20\x2b\x20\x27\x64\x65\x66\x27";
  v8::ScriptCompiler::CachedData* cache;

  v8::Isolate* isolate1 = v8::Isolate::New();
  v8::Isolate* isolate2 = v8::Isolate::New();
  {
    v8::Isolate::Scope iscope(isolate1);
    v8::HandleScope scope(isolate1);
    v8::Local<v8::Context> context = v8::Context::New(isolate1);
    v8::Context::Scope context_scope(context);

    v8::Local<v8::String> source_str = v8_str(source);
    v8::ScriptOrigin origin(v8_str("\x74\x65\x73\x74"));
    v8::ScriptCompiler::Source source(source_str, origin);
    v8::Local<v8::UnboundScript> script = v8::ScriptCompiler::CompileUnbound(
        isolate1, &source, v8::ScriptCompiler::kProduceCodeCache);
    const v8::ScriptCompiler::CachedData* data = source.GetCachedData();
    // Persist cached data.
    uint8_t* buffer = NewArray<uint8_t>(data->length);
    MemCopy(buffer, data->data, data->length);
    cache = new v8::ScriptCompiler::CachedData(
        buffer, data->length, v8::ScriptCompiler::CachedData::BufferOwned);

    v8::Local<v8::Value> result = script->BindToCurrentContext()->Run();
    CHECK(result->ToString()->Equals(v8_str("\x61\x62\x63\x64\x65\x66")));
  }
  isolate1->Dispose();

  {
    v8::Isolate::Scope iscope(isolate2);
    v8::HandleScope scope(isolate2);
    v8::Local<v8::Context> context = v8::Context::New(isolate2);
    v8::Context::Scope context_scope(context);

    v8::Local<v8::String> source_str = v8_str(source);
    v8::ScriptOrigin origin(v8_str("\x74\x65\x73\x74"));
    v8::ScriptCompiler::Source source(source_str, origin, cache);
    v8::Local<v8::UnboundScript> script;
    {
      DisallowCompilation no_compile(reinterpret_cast<Isolate*>(isolate2));
      script = v8::ScriptCompiler::CompileUnbound(
          isolate2, &source, v8::ScriptCompiler::kConsumeCodeCache);
    }
    v8::Local<v8::Value> result = script->BindToCurrentContext()->Run();
    CHECK(result->ToString()->Equals(v8_str("\x61\x62\x63\x64\x65\x66")));
  }
  isolate2->Dispose();
}
