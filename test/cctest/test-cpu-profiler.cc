// Copyright 2010 the V8 project authors. All rights reserved.
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
//
// Tests of profiles generator and utilities.

#include "src/v8.h"

#include "include/v8-profiler.h"
#include "src/base/platform/platform.h"
#include "src/cpu-profiler-inl.h"
#include "src/smart-pointers.h"
#include "src/utils.h"
#include "test/cctest/cctest.h"
#include "test/cctest/profiler-extension.h"
using i::CodeEntry;
using i::CpuProfile;
using i::CpuProfiler;
using i::CpuProfilesCollection;
using i::Heap;
using i::ProfileGenerator;
using i::ProfileNode;
using i::ProfilerEventsProcessor;
using i::ScopedVector;
using i::SmartPointer;
using i::Vector;


TEST(StartStop) {
  i::Isolate* isolate = CcTest::i_isolate();
  CpuProfilesCollection profiles(isolate->heap());
  ProfileGenerator generator(&profiles);
  SmartPointer<ProfilerEventsProcessor> processor(new ProfilerEventsProcessor(
          &generator, NULL, v8::base::TimeDelta::FromMicroseconds(100)));
  processor->Start();
  processor->StopSynchronously();
}


static void EnqueueTickSampleEvent(ProfilerEventsProcessor* proc,
                                   i::Address frame1,
                                   i::Address frame2 = NULL,
                                   i::Address frame3 = NULL) {
  i::TickSample* sample = proc->StartTickSample();
  sample->pc = frame1;
  sample->tos = frame1;
  sample->frames_count = 0;
  if (frame2 != NULL) {
    sample->stack[0] = frame2;
    sample->frames_count = 1;
  }
  if (frame3 != NULL) {
    sample->stack[1] = frame3;
    sample->frames_count = 2;
  }
  proc->FinishTickSample();
}

namespace {

class TestSetup {
 public:
  TestSetup()
      : old_flag_prof_browser_mode_(i::FLAG_prof_browser_mode) {
    i::FLAG_prof_browser_mode = false;
  }

  ~TestSetup() {
    i::FLAG_prof_browser_mode = old_flag_prof_browser_mode_;
  }

 private:
  bool old_flag_prof_browser_mode_;
};

}  // namespace


i::Code* CreateCode(LocalContext* env) {
  static int counter = 0;
  i::EmbeddedVector<char, 256> script;
  i::EmbeddedVector<char, 32> name;

  i::SNPrintF(name, "\x66\x75\x6e\x63\x74\x69\x6f\x6e\x5f\x6c\x84", ++counter);
  const char* name_start = name.start();
  i::SNPrintF(script,
      "\x66\x75\x6e\x63\x74\x69\x6f\x6e\x20\x6c\xa2\x28\x29\x20\x7b\xa"
           "\x76\x61\x72\x20\x63\x6f\x75\x6e\x74\x65\x72\x20\x3d\x20\x30\x3b\xa"
           "\x66\x6f\x72\x20\x28\x76\x61\x72\x20\x69\x20\x3d\x20\x30\x3b\x20\x69\x20\x3c\x20\x6c\x84\x3b\x20\x2b\x2b\x69\x29\x20\x63\x6f\x75\x6e\x74\x65\x72\x20\x2b\x3d\x20\x69\x3b\xa"
           "\x72\x65\x74\x75\x72\x6e\x20\x27\x6c\xa2\x5f\x27\x20\x2b\x20\x63\x6f\x75\x6e\x74\x65\x72\x3b\xa"
       "\x7d\xa"
       "\x6c\xa2\x28\x29\x3b\xa", name_start, counter, name_start, name_start);
  CompileRun(script.start());
  i::Handle<i::JSFunction> fun = v8::Utils::OpenHandle(
      *v8::Local<v8::Function>::Cast(
          (*env)->Global()->Get(v8_str(name_start))));
  return fun->code();
}


TEST(CodeEvents) {
  CcTest::InitializeVM();
  LocalContext env;
  i::Isolate* isolate = CcTest::i_isolate();
  i::Factory* factory = isolate->factory();
  TestSetup test_setup;

  i::HandleScope scope(isolate);

  i::Code* aaa_code = CreateCode(&env);
  i::Code* comment_code = CreateCode(&env);
  i::Code* args5_code = CreateCode(&env);
  i::Code* comment2_code = CreateCode(&env);
  i::Code* moved_code = CreateCode(&env);
  i::Code* args3_code = CreateCode(&env);
  i::Code* args4_code = CreateCode(&env);

  CpuProfilesCollection* profiles = new CpuProfilesCollection(isolate->heap());
  profiles->StartProfiling("", false);
  ProfileGenerator generator(profiles);
  SmartPointer<ProfilerEventsProcessor> processor(new ProfilerEventsProcessor(
          &generator, NULL, v8::base::TimeDelta::FromMicroseconds(100)));
  processor->Start();
  CpuProfiler profiler(isolate, profiles, &generator, processor.get());

  // Enqueue code creation events.
  const char* aaa_str = "\x61\x61\x61";
  i::Handle<i::String> aaa_name = factory->NewStringFromAsciiChecked(aaa_str);
  profiler.CodeCreateEvent(i::Logger::FUNCTION_TAG, aaa_code, *aaa_name);
  profiler.CodeCreateEvent(i::Logger::BUILTIN_TAG, comment_code, "\x63\x6f\x6d\x6d\x65\x6e\x74");
  profiler.CodeCreateEvent(i::Logger::STUB_TAG, args5_code, 5);
  profiler.CodeCreateEvent(i::Logger::BUILTIN_TAG, comment2_code, "\x63\x6f\x6d\x6d\x65\x6e\x74\x32");
  profiler.CodeMoveEvent(comment2_code->address(), moved_code->address());
  profiler.CodeCreateEvent(i::Logger::STUB_TAG, args3_code, 3);
  profiler.CodeCreateEvent(i::Logger::STUB_TAG, args4_code, 4);

  // Enqueue a tick event to enable code events processing.
  EnqueueTickSampleEvent(processor.get(), aaa_code->address());

  processor->StopSynchronously();

  // Check the state of profile generator.
  CodeEntry* aaa = generator.code_map()->FindEntry(aaa_code->address());
  CHECK_NE(NULL, aaa);
  CHECK_EQ(aaa_str, aaa->name());

  CodeEntry* comment = generator.code_map()->FindEntry(comment_code->address());
  CHECK_NE(NULL, comment);
  CHECK_EQ("\x63\x6f\x6d\x6d\x65\x6e\x74", comment->name());

  CodeEntry* args5 = generator.code_map()->FindEntry(args5_code->address());
  CHECK_NE(NULL, args5);
  CHECK_EQ("\x35", args5->name());

  CHECK_EQ(NULL, generator.code_map()->FindEntry(comment2_code->address()));

  CodeEntry* comment2 = generator.code_map()->FindEntry(moved_code->address());
  CHECK_NE(NULL, comment2);
  CHECK_EQ("\x63\x6f\x6d\x6d\x65\x6e\x74\x32", comment2->name());
}


template<typename T>
static int CompareProfileNodes(const T* p1, const T* p2) {
  return strcmp((*p1)->entry()->name(), (*p2)->entry()->name());
}


TEST(TickEvents) {
  TestSetup test_setup;
  LocalContext env;
  i::Isolate* isolate = CcTest::i_isolate();
  i::HandleScope scope(isolate);

  i::Code* frame1_code = CreateCode(&env);
  i::Code* frame2_code = CreateCode(&env);
  i::Code* frame3_code = CreateCode(&env);

  CpuProfilesCollection* profiles = new CpuProfilesCollection(isolate->heap());
  profiles->StartProfiling("", false);
  ProfileGenerator generator(profiles);
  SmartPointer<ProfilerEventsProcessor> processor(new ProfilerEventsProcessor(
          &generator, NULL, v8::base::TimeDelta::FromMicroseconds(100)));
  processor->Start();
  CpuProfiler profiler(isolate, profiles, &generator, processor.get());

  profiler.CodeCreateEvent(i::Logger::BUILTIN_TAG, frame1_code, "\x62\x62\x62");
  profiler.CodeCreateEvent(i::Logger::STUB_TAG, frame2_code, 5);
  profiler.CodeCreateEvent(i::Logger::BUILTIN_TAG, frame3_code, "\x64\x64\x64");

  EnqueueTickSampleEvent(processor.get(), frame1_code->instruction_start());
  EnqueueTickSampleEvent(
      processor.get(),
      frame2_code->instruction_start() + frame2_code->ExecutableSize() / 2,
      frame1_code->instruction_start() + frame2_code->ExecutableSize() / 2);
  EnqueueTickSampleEvent(
      processor.get(),
      frame3_code->instruction_end() - 1,
      frame2_code->instruction_end() - 1,
      frame1_code->instruction_end() - 1);

  processor->StopSynchronously();
  CpuProfile* profile = profiles->StopProfiling("");
  CHECK_NE(NULL, profile);

  // Check call trees.
  const i::List<ProfileNode*>* top_down_root_children =
      profile->top_down()->root()->children();
  CHECK_EQ(1, top_down_root_children->length());
  CHECK_EQ("\x62\x62\x62", top_down_root_children->last()->entry()->name());
  const i::List<ProfileNode*>* top_down_bbb_children =
      top_down_root_children->last()->children();
  CHECK_EQ(1, top_down_bbb_children->length());
  CHECK_EQ("\x35", top_down_bbb_children->last()->entry()->name());
  const i::List<ProfileNode*>* top_down_stub_children =
      top_down_bbb_children->last()->children();
  CHECK_EQ(1, top_down_stub_children->length());
  CHECK_EQ("\x64\x64\x64", top_down_stub_children->last()->entry()->name());
  const i::List<ProfileNode*>* top_down_ddd_children =
      top_down_stub_children->last()->children();
  CHECK_EQ(0, top_down_ddd_children->length());
}


// http://crbug/51594
// This test must not crash.
TEST(CrashIfStoppingLastNonExistentProfile) {
  CcTest::InitializeVM();
  TestSetup test_setup;
  CpuProfiler* profiler = CcTest::i_isolate()->cpu_profiler();
  profiler->StartProfiling("\x31");
  profiler->StopProfiling("\x32");
  profiler->StartProfiling("\x31");
  profiler->StopProfiling("");
}


// http://code.google.com/p/v8/issues/detail?id=1398
// Long stacks (exceeding max frames limit) must not be erased.
TEST(Issue1398) {
  TestSetup test_setup;
  LocalContext env;
  i::Isolate* isolate = CcTest::i_isolate();
  i::HandleScope scope(isolate);

  i::Code* code = CreateCode(&env);

  CpuProfilesCollection* profiles = new CpuProfilesCollection(isolate->heap());
  profiles->StartProfiling("", false);
  ProfileGenerator generator(profiles);
  SmartPointer<ProfilerEventsProcessor> processor(new ProfilerEventsProcessor(
          &generator, NULL, v8::base::TimeDelta::FromMicroseconds(100)));
  processor->Start();
  CpuProfiler profiler(isolate, profiles, &generator, processor.get());

  profiler.CodeCreateEvent(i::Logger::BUILTIN_TAG, code, "\x62\x62\x62");

  i::TickSample* sample = processor->StartTickSample();
  sample->pc = code->address();
  sample->tos = 0;
  sample->frames_count = i::TickSample::kMaxFramesCount;
  for (unsigned i = 0; i < sample->frames_count; ++i) {
    sample->stack[i] = code->address();
  }
  processor->FinishTickSample();

  processor->StopSynchronously();
  CpuProfile* profile = profiles->StopProfiling("");
  CHECK_NE(NULL, profile);

  int actual_depth = 0;
  const ProfileNode* node = profile->top_down()->root();
  while (node->children()->length() > 0) {
    node = node->children()->last();
    ++actual_depth;
  }

  CHECK_EQ(1 + i::TickSample::kMaxFramesCount, actual_depth);  // +1 for PC.
}


TEST(DeleteAllCpuProfiles) {
  CcTest::InitializeVM();
  TestSetup test_setup;
  CpuProfiler* profiler = CcTest::i_isolate()->cpu_profiler();
  CHECK_EQ(0, profiler->GetProfilesCount());
  profiler->DeleteAllProfiles();
  CHECK_EQ(0, profiler->GetProfilesCount());

  profiler->StartProfiling("\x31");
  profiler->StopProfiling("\x31");
  CHECK_EQ(1, profiler->GetProfilesCount());
  profiler->DeleteAllProfiles();
  CHECK_EQ(0, profiler->GetProfilesCount());
  profiler->StartProfiling("\x31");
  profiler->StartProfiling("\x32");
  profiler->StopProfiling("\x32");
  profiler->StopProfiling("\x31");
  CHECK_EQ(2, profiler->GetProfilesCount());
  profiler->DeleteAllProfiles();
  CHECK_EQ(0, profiler->GetProfilesCount());

  // Test profiling cancellation by the 'delete' command.
  profiler->StartProfiling("\x31");
  profiler->StartProfiling("\x32");
  CHECK_EQ(0, profiler->GetProfilesCount());
  profiler->DeleteAllProfiles();
  CHECK_EQ(0, profiler->GetProfilesCount());
}


static bool FindCpuProfile(v8::CpuProfiler* v8profiler,
                           const v8::CpuProfile* v8profile) {
  i::CpuProfiler* profiler = reinterpret_cast<i::CpuProfiler*>(v8profiler);
  const i::CpuProfile* profile =
      reinterpret_cast<const i::CpuProfile*>(v8profile);
  int length = profiler->GetProfilesCount();
  for (int i = 0; i < length; i++) {
    if (profile == profiler->GetProfile(i))
      return true;
  }
  return false;
}


TEST(DeleteCpuProfile) {
  LocalContext env;
  v8::HandleScope scope(env->GetIsolate());
  v8::CpuProfiler* cpu_profiler = env->GetIsolate()->GetCpuProfiler();
  i::CpuProfiler* iprofiler = reinterpret_cast<i::CpuProfiler*>(cpu_profiler);

  CHECK_EQ(0, iprofiler->GetProfilesCount());
  v8::Local<v8::String> name1 = v8::String::NewFromUtf8(env->GetIsolate(), "\x31");
  cpu_profiler->StartProfiling(name1);
  v8::CpuProfile* p1 = cpu_profiler->StopProfiling(name1);
  CHECK_NE(NULL, p1);
  CHECK_EQ(1, iprofiler->GetProfilesCount());
  CHECK(FindCpuProfile(cpu_profiler, p1));
  p1->Delete();
  CHECK_EQ(0, iprofiler->GetProfilesCount());

  v8::Local<v8::String> name2 = v8::String::NewFromUtf8(env->GetIsolate(), "\x32");
  cpu_profiler->StartProfiling(name2);
  v8::CpuProfile* p2 = cpu_profiler->StopProfiling(name2);
  CHECK_NE(NULL, p2);
  CHECK_EQ(1, iprofiler->GetProfilesCount());
  CHECK(FindCpuProfile(cpu_profiler, p2));
  v8::Local<v8::String> name3 = v8::String::NewFromUtf8(env->GetIsolate(), "\x33");
  cpu_profiler->StartProfiling(name3);
  v8::CpuProfile* p3 = cpu_profiler->StopProfiling(name3);
  CHECK_NE(NULL, p3);
  CHECK_EQ(2, iprofiler->GetProfilesCount());
  CHECK_NE(p2, p3);
  CHECK(FindCpuProfile(cpu_profiler, p3));
  CHECK(FindCpuProfile(cpu_profiler, p2));
  p2->Delete();
  CHECK_EQ(1, iprofiler->GetProfilesCount());
  CHECK(!FindCpuProfile(cpu_profiler, p2));
  CHECK(FindCpuProfile(cpu_profiler, p3));
  p3->Delete();
  CHECK_EQ(0, iprofiler->GetProfilesCount());
}


TEST(ProfileStartEndTime) {
  LocalContext env;
  v8::HandleScope scope(env->GetIsolate());
  v8::CpuProfiler* cpu_profiler = env->GetIsolate()->GetCpuProfiler();

  v8::Local<v8::String> profile_name =
      v8::String::NewFromUtf8(env->GetIsolate(), "\x74\x65\x73\x74");
  cpu_profiler->StartProfiling(profile_name);
  const v8::CpuProfile* profile = cpu_profiler->StopProfiling(profile_name);
  CHECK(profile->GetStartTime() <= profile->GetEndTime());
}


static v8::CpuProfile* RunProfiler(
    v8::Handle<v8::Context> env, v8::Handle<v8::Function> function,
    v8::Handle<v8::Value> argv[], int argc,
    unsigned min_js_samples, bool collect_samples = false) {
  v8::CpuProfiler* cpu_profiler = env->GetIsolate()->GetCpuProfiler();
  v8::Local<v8::String> profile_name =
      v8::String::NewFromUtf8(env->GetIsolate(), "\x6d\x79\x5f\x70\x72\x6f\x66\x69\x6c\x65");

  cpu_profiler->StartProfiling(profile_name, collect_samples);

  i::Sampler* sampler =
      reinterpret_cast<i::Isolate*>(env->GetIsolate())->logger()->sampler();
  sampler->StartCountingSamples();
  do {
    function->Call(env->Global(), argc, argv);
  } while (sampler->js_and_external_sample_count() < min_js_samples);

  v8::CpuProfile* profile = cpu_profiler->StopProfiling(profile_name);

  CHECK_NE(NULL, profile);
  // Dump collected profile to have a better diagnostic in case of failure.
  reinterpret_cast<i::CpuProfile*>(profile)->Print();

  return profile;
}


static bool ContainsString(v8::Handle<v8::String> string,
                           const Vector<v8::Handle<v8::String> >& vector) {
  for (int i = 0; i < vector.length(); i++) {
    if (string->Equals(vector[i]))
      return true;
  }
  return false;
}


static void CheckChildrenNames(const v8::CpuProfileNode* node,
                               const Vector<v8::Handle<v8::String> >& names) {
  int count = node->GetChildrenCount();
  for (int i = 0; i < count; i++) {
    v8::Handle<v8::String> name = node->GetChild(i)->GetFunctionName();
    CHECK(ContainsString(name, names));
    // Check that there are no duplicates.
    for (int j = 0; j < count; j++) {
      if (j == i) continue;
      CHECK_NE(name, node->GetChild(j)->GetFunctionName());
    }
  }
}


static const v8::CpuProfileNode* FindChild(v8::Isolate* isolate,
                                           const v8::CpuProfileNode* node,
                                           const char* name) {
  int count = node->GetChildrenCount();
  v8::Handle<v8::String> nameHandle = v8::String::NewFromUtf8(isolate, name);
  for (int i = 0; i < count; i++) {
    const v8::CpuProfileNode* child = node->GetChild(i);
    if (nameHandle->Equals(child->GetFunctionName())) return child;
  }
  return NULL;
}


static const v8::CpuProfileNode* GetChild(v8::Isolate* isolate,
                                          const v8::CpuProfileNode* node,
                                          const char* name) {
  const v8::CpuProfileNode* result = FindChild(isolate, node, name);
  if (!result) {
    char buffer[100];
    i::SNPrintF(Vector<char>(buffer, ARRAY_SIZE(buffer)),
                "\x46\x61\x69\x6c\x65\x64\x20\x74\x6f\x20\x47\x65\x74\x43\x68\x69\x6c\x64\x3a\x20\x6c\xa2", name);
    FATAL(buffer);
  }
  return result;
}


static void CheckSimpleBranch(v8::Isolate* isolate,
                              const v8::CpuProfileNode* node,
                              const char* names[], int length) {
  for (int i = 0; i < length; i++) {
    const char* name = names[i];
    node = GetChild(isolate, node, name);
    int expectedChildrenCount = (i == length - 1) ? 0 : 1;
    CHECK_EQ(expectedChildrenCount, node->GetChildrenCount());
  }
}


static const char* cpu_profiler_test_source = "\x66\x75\x6e\x63\x74\x69\x6f\x6e\x20\x6c\x6f\x6f\x70\x28\x74\x69\x6d\x65\x6f\x75\x74\x29\x20\x7b\xa"
"\x20\x20\x74\x68\x69\x73\x2e\x6d\x6d\x6d\x20\x3d\x20\x30\x3b\xa"
"\x20\x20\x76\x61\x72\x20\x73\x74\x61\x72\x74\x20\x3d\x20\x44\x61\x74\x65\x2e\x6e\x6f\x77\x28\x29\x3b\xa"
"\x20\x20\x77\x68\x69\x6c\x65\x20\x28\x44\x61\x74\x65\x2e\x6e\x6f\x77\x28\x29\x20\x2d\x20\x73\x74\x61\x72\x74\x20\x3c\x20\x74\x69\x6d\x65\x6f\x75\x74\x29\x20\x7b\xa"
"\x20\x20\x20\x20\x76\x61\x72\x20\x6e\x20\x3d\x20\x31\x30\x30\x2a\x31\x30\x30\x30\x3b\xa"
"\x20\x20\x20\x20\x77\x68\x69\x6c\x65\x28\x6e\x20\x3e\x20\x31\x29\x20\x7b\xa"
"\x20\x20\x20\x20\x20\x20\x6e\x2d\x2d\x3b\xa"
"\x20\x20\x20\x20\x20\x20\x74\x68\x69\x73\x2e\x6d\x6d\x6d\x20\x2b\x3d\x20\x6e\x20\x2a\x20\x6e\x20\x2a\x20\x6e\x3b\xa"
"\x20\x20\x20\x20\x7d\xa"
"\x20\x20\x7d\xa"
"\x7d\xa"
"\x66\x75\x6e\x63\x74\x69\x6f\x6e\x20\x64\x65\x6c\x61\x79\x28\x29\x20\x7b\x20\x74\x72\x79\x20\x7b\x20\x6c\x6f\x6f\x70\x28\x31\x30\x29\x3b\x20\x7d\x20\x63\x61\x74\x63\x68\x28\x65\x29\x20\x7b\x20\x7d\x20\x7d\xa"
"\x66\x75\x6e\x63\x74\x69\x6f\x6e\x20\x62\x61\x72\x28\x29\x20\x7b\x20\x64\x65\x6c\x61\x79\x28\x29\x3b\x20\x7d\xa"
"\x66\x75\x6e\x63\x74\x69\x6f\x6e\x20\x62\x61\x7a\x28\x29\x20\x7b\x20\x64\x65\x6c\x61\x79\x28\x29\x3b\x20\x7d\xa"
"\x66\x75\x6e\x63\x74\x69\x6f\x6e\x20\x66\x6f\x6f\x28\x29\x20\x7b\xa"
"\x20\x20\x20\x20\x74\x72\x79\x20\x7b\xa"
"\x20\x20\x20\x20\x20\x20\x20\x64\x65\x6c\x61\x79\x28\x29\x3b\xa"
"\x20\x20\x20\x20\x20\x20\x20\x62\x61\x72\x28\x29\x3b\xa"
"\x20\x20\x20\x20\x20\x20\x20\x64\x65\x6c\x61\x79\x28\x29\x3b\xa"
"\x20\x20\x20\x20\x20\x20\x20\x62\x61\x7a\x28\x29\x3b\xa"
"\x20\x20\x20\x20\x7d\x20\x63\x61\x74\x63\x68\x20\x28\x65\x29\x20\x7b\x20\x7d\xa"
"\x7d\xa"
"\x66\x75\x6e\x63\x74\x69\x6f\x6e\x20\x73\x74\x61\x72\x74\x28\x74\x69\x6d\x65\x6f\x75\x74\x29\x20\x7b\xa"
"\x20\x20\x76\x61\x72\x20\x73\x74\x61\x72\x74\x20\x3d\x20\x44\x61\x74\x65\x2e\x6e\x6f\x77\x28\x29\x3b\xa"
"\x20\x20\x64\x6f\x20\x7b\xa"
"\x20\x20\x20\x20\x66\x6f\x6f\x28\x29\x3b\xa"
"\x20\x20\x20\x20\x76\x61\x72\x20\x64\x75\x72\x61\x74\x69\x6f\x6e\x20\x3d\x20\x44\x61\x74\x65\x2e\x6e\x6f\x77\x28\x29\x20\x2d\x20\x73\x74\x61\x72\x74\x3b\xa"
"\x20\x20\x7d\x20\x77\x68\x69\x6c\x65\x20\x28\x64\x75\x72\x61\x74\x69\x6f\x6e\x20\x3c\x20\x74\x69\x6d\x65\x6f\x75\x74\x29\x3b\xa"
"\x20\x20\x72\x65\x74\x75\x72\x6e\x20\x64\x75\x72\x61\x74\x69\x6f\x6e\x3b\xa"
"\x7d\xa";


// Check that the profile tree for the script above will look like the
// following:
//
// [Top down]:
//  1062     0   (root) [-1]
//  1054     0    start [-1]
//  1054     1      foo [-1]
//   265     0        baz [-1]
//   265     1          delay [-1]
//   264   264            loop [-1]
//   525     3        delay [-1]
//   522   522          loop [-1]
//   263     0        bar [-1]
//   263     1          delay [-1]
//   262   262            loop [-1]
//     2     2    (program) [-1]
//     6     6    (garbage collector) [-1]
TEST(CollectCpuProfile) {
  LocalContext env;
  v8::HandleScope scope(env->GetIsolate());

  v8::Script::Compile(v8::String::NewFromUtf8(env->GetIsolate(),
                                              cpu_profiler_test_source))->Run();
  v8::Local<v8::Function> function = v8::Local<v8::Function>::Cast(
      env->Global()->Get(v8::String::NewFromUtf8(env->GetIsolate(), "\x73\x74\x61\x72\x74")));

  int32_t profiling_interval_ms = 200;
  v8::Handle<v8::Value> args[] = {
    v8::Integer::New(env->GetIsolate(), profiling_interval_ms)
  };
  v8::CpuProfile* profile =
      RunProfiler(env.local(), function, args, ARRAY_SIZE(args), 200);
  function->Call(env->Global(), ARRAY_SIZE(args), args);

  const v8::CpuProfileNode* root = profile->GetTopDownRoot();

  ScopedVector<v8::Handle<v8::String> > names(3);
  names[0] = v8::String::NewFromUtf8(
      env->GetIsolate(), ProfileGenerator::kGarbageCollectorEntryName);
  names[1] = v8::String::NewFromUtf8(env->GetIsolate(),
                                     ProfileGenerator::kProgramEntryName);
  names[2] = v8::String::NewFromUtf8(env->GetIsolate(), "\x73\x74\x61\x72\x74");
  CheckChildrenNames(root, names);

  const v8::CpuProfileNode* startNode =
      GetChild(env->GetIsolate(), root, "\x73\x74\x61\x72\x74");
  CHECK_EQ(1, startNode->GetChildrenCount());

  const v8::CpuProfileNode* fooNode =
      GetChild(env->GetIsolate(), startNode, "\x66\x6f\x6f");
  CHECK_EQ(3, fooNode->GetChildrenCount());

  const char* barBranch[] = { "\x62\x61\x72", "\x64\x65\x6c\x61\x79", "\x6c\x6f\x6f\x70" };
  CheckSimpleBranch(env->GetIsolate(), fooNode, barBranch,
                    ARRAY_SIZE(barBranch));
  const char* bazBranch[] = { "\x62\x61\x7a", "\x64\x65\x6c\x61\x79", "\x6c\x6f\x6f\x70" };
  CheckSimpleBranch(env->GetIsolate(), fooNode, bazBranch,
                    ARRAY_SIZE(bazBranch));
  const char* delayBranch[] = { "\x64\x65\x6c\x61\x79", "\x6c\x6f\x6f\x70" };
  CheckSimpleBranch(env->GetIsolate(), fooNode, delayBranch,
                    ARRAY_SIZE(delayBranch));

  profile->Delete();
}


static const char* hot_deopt_no_frame_entry_test_source =
"\x66\x75\x6e\x63\x74\x69\x6f\x6e\x20\x66\x6f\x6f\x28\x61\x2c\x20\x62\x29\x20\x7b\xa"
"\x20\x20\x20\x20\x74\x72\x79\x20\x7b\xa"
"\x20\x20\x20\x20\x20\x20\x72\x65\x74\x75\x72\x6e\x20\x61\x20\x2b\x20\x62\x3b\xa"
"\x20\x20\x20\x20\x7d\x20\x63\x61\x74\x63\x68\x20\x28\x65\x29\x20\x7b\x20\x7d\xa"
"\x7d\xa"
"\x66\x75\x6e\x63\x74\x69\x6f\x6e\x20\x73\x74\x61\x72\x74\x28\x74\x69\x6d\x65\x6f\x75\x74\x29\x20\x7b\xa"
"\x20\x20\x76\x61\x72\x20\x73\x74\x61\x72\x74\x20\x3d\x20\x44\x61\x74\x65\x2e\x6e\x6f\x77\x28\x29\x3b\xa"
"\x20\x20\x64\x6f\x20\x7b\xa"
"\x20\x20\x20\x20\x66\x6f\x72\x20\x28\x76\x61\x72\x20\x69\x20\x3d\x20\x31\x3b\x20\x69\x20\x3c\x20\x31\x30\x30\x30\x3b\x20\x2b\x2b\x69\x29\x20\x66\x6f\x6f\x28\x31\x2c\x20\x69\x29\x3b\xa"
"\x20\x20\x20\x20\x76\x61\x72\x20\x64\x75\x72\x61\x74\x69\x6f\x6e\x20\x3d\x20\x44\x61\x74\x65\x2e\x6e\x6f\x77\x28\x29\x20\x2d\x20\x73\x74\x61\x72\x74\x3b\xa"
"\x20\x20\x7d\x20\x77\x68\x69\x6c\x65\x20\x28\x64\x75\x72\x61\x74\x69\x6f\x6e\x20\x3c\x20\x74\x69\x6d\x65\x6f\x75\x74\x29\x3b\xa"
"\x20\x20\x72\x65\x74\x75\x72\x6e\x20\x64\x75\x72\x61\x74\x69\x6f\x6e\x3b\xa"
"\x7d\xa";

// Check that the profile tree for the script above will look like the
// following:
//
// [Top down]:
//  1062     0  (root) [-1]
//  1054     0    start [-1]
//  1054     1      foo [-1]
//     2     2    (program) [-1]
//     6     6    (garbage collector) [-1]
//
// The test checks no FP ranges are present in a deoptimized funcion.
// If 'foo' has no ranges the samples falling into the prologue will miss the
// 'start' function on the stack, so 'foo' will be attached to the (root).
TEST(HotDeoptNoFrameEntry) {
  LocalContext env;
  v8::HandleScope scope(env->GetIsolate());

  v8::Script::Compile(v8::String::NewFromUtf8(
      env->GetIsolate(),
      hot_deopt_no_frame_entry_test_source))->Run();
  v8::Local<v8::Function> function = v8::Local<v8::Function>::Cast(
      env->Global()->Get(v8::String::NewFromUtf8(env->GetIsolate(), "\x73\x74\x61\x72\x74")));

  int32_t profiling_interval_ms = 200;
  v8::Handle<v8::Value> args[] = {
    v8::Integer::New(env->GetIsolate(), profiling_interval_ms)
  };
  v8::CpuProfile* profile =
      RunProfiler(env.local(), function, args, ARRAY_SIZE(args), 200);
  function->Call(env->Global(), ARRAY_SIZE(args), args);

  const v8::CpuProfileNode* root = profile->GetTopDownRoot();

  ScopedVector<v8::Handle<v8::String> > names(3);
  names[0] = v8::String::NewFromUtf8(
      env->GetIsolate(), ProfileGenerator::kGarbageCollectorEntryName);
  names[1] = v8::String::NewFromUtf8(env->GetIsolate(),
                                     ProfileGenerator::kProgramEntryName);
  names[2] = v8::String::NewFromUtf8(env->GetIsolate(), "\x73\x74\x61\x72\x74");
  CheckChildrenNames(root, names);

  const v8::CpuProfileNode* startNode =
      GetChild(env->GetIsolate(), root, "\x73\x74\x61\x72\x74");
  CHECK_EQ(1, startNode->GetChildrenCount());

  GetChild(env->GetIsolate(), startNode, "\x66\x6f\x6f");

  profile->Delete();
}


TEST(CollectCpuProfileSamples) {
  LocalContext env;
  v8::HandleScope scope(env->GetIsolate());

  v8::Script::Compile(v8::String::NewFromUtf8(env->GetIsolate(),
                                              cpu_profiler_test_source))->Run();
  v8::Local<v8::Function> function = v8::Local<v8::Function>::Cast(
      env->Global()->Get(v8::String::NewFromUtf8(env->GetIsolate(), "\x73\x74\x61\x72\x74")));

  int32_t profiling_interval_ms = 200;
  v8::Handle<v8::Value> args[] = {
    v8::Integer::New(env->GetIsolate(), profiling_interval_ms)
  };
  v8::CpuProfile* profile =
      RunProfiler(env.local(), function, args, ARRAY_SIZE(args), 200, true);

  CHECK_LE(200, profile->GetSamplesCount());
  uint64_t end_time = profile->GetEndTime();
  uint64_t current_time = profile->GetStartTime();
  CHECK_LE(current_time, end_time);
  for (int i = 0; i < profile->GetSamplesCount(); i++) {
    CHECK_NE(NULL, profile->GetSample(i));
    uint64_t timestamp = profile->GetSampleTimestamp(i);
    CHECK_LE(current_time, timestamp);
    CHECK_LE(timestamp, end_time);
    current_time = timestamp;
  }

  profile->Delete();
}


static const char* cpu_profiler_test_source2 = "\x66\x75\x6e\x63\x74\x69\x6f\x6e\x20\x6c\x6f\x6f\x70\x28\x29\x20\x7b\x7d\xa"
"\x66\x75\x6e\x63\x74\x69\x6f\x6e\x20\x64\x65\x6c\x61\x79\x28\x29\x20\x7b\x20\x6c\x6f\x6f\x70\x28\x29\x3b\x20\x7d\xa"
"\x66\x75\x6e\x63\x74\x69\x6f\x6e\x20\x73\x74\x61\x72\x74\x28\x63\x6f\x75\x6e\x74\x29\x20\x7b\xa"
"\x20\x20\x76\x61\x72\x20\x6b\x20\x3d\x20\x30\x3b\xa"
"\x20\x20\x64\x6f\x20\x7b\xa"
"\x20\x20\x20\x20\x64\x65\x6c\x61\x79\x28\x29\x3b\xa"
"\x20\x20\x7d\x20\x77\x68\x69\x6c\x65\x20\x28\x2b\x2b\x6b\x20\x3c\x20\x63\x6f\x75\x6e\x74\x2a\x31\x30\x30\x2a\x31\x30\x30\x30\x29\x3b\xa"
"\x7d\xa";

// Check that the profile tree doesn't contain unexpected traces:
//  - 'loop' can be called only by 'delay'
//  - 'delay' may be called only by 'start'
// The profile will look like the following:
//
// [Top down]:
//   135     0   (root) [-1] #1
//   121    72    start [-1] #3
//    49    33      delay [-1] #4
//    16    16        loop [-1] #5
//    14    14    (program) [-1] #2
TEST(SampleWhenFrameIsNotSetup) {
  LocalContext env;
  v8::HandleScope scope(env->GetIsolate());

  v8::Script::Compile(v8::String::NewFromUtf8(
                          env->GetIsolate(), cpu_profiler_test_source2))->Run();
  v8::Local<v8::Function> function = v8::Local<v8::Function>::Cast(
      env->Global()->Get(v8::String::NewFromUtf8(env->GetIsolate(), "\x73\x74\x61\x72\x74")));

  int32_t repeat_count = 100;
#if defined(USE_SIMULATOR)
  // Simulators are much slower.
  repeat_count = 1;
#endif
  v8::Handle<v8::Value> args[] = {
    v8::Integer::New(env->GetIsolate(), repeat_count)
  };
  v8::CpuProfile* profile =
      RunProfiler(env.local(), function, args, ARRAY_SIZE(args), 100);

  const v8::CpuProfileNode* root = profile->GetTopDownRoot();

  ScopedVector<v8::Handle<v8::String> > names(3);
  names[0] = v8::String::NewFromUtf8(
      env->GetIsolate(), ProfileGenerator::kGarbageCollectorEntryName);
  names[1] = v8::String::NewFromUtf8(env->GetIsolate(),
                                     ProfileGenerator::kProgramEntryName);
  names[2] = v8::String::NewFromUtf8(env->GetIsolate(), "\x73\x74\x61\x72\x74");
  CheckChildrenNames(root, names);

  const v8::CpuProfileNode* startNode =
      FindChild(env->GetIsolate(), root, "\x73\x74\x61\x72\x74");
  // On slow machines there may be no meaningfull samples at all, skip the
  // check there.
  if (startNode && startNode->GetChildrenCount() > 0) {
    CHECK_EQ(1, startNode->GetChildrenCount());
    const v8::CpuProfileNode* delayNode =
        GetChild(env->GetIsolate(), startNode, "\x64\x65\x6c\x61\x79");
    if (delayNode->GetChildrenCount() > 0) {
      CHECK_EQ(1, delayNode->GetChildrenCount());
      GetChild(env->GetIsolate(), delayNode, "\x6c\x6f\x6f\x70");
    }
  }

  profile->Delete();
}


static const char* native_accessor_test_source = "\x66\x75\x6e\x63\x74\x69\x6f\x6e\x20\x73\x74\x61\x72\x74\x28\x63\x6f\x75\x6e\x74\x29\x20\x7b\xa"
"\x20\x20\x66\x6f\x72\x20\x28\x76\x61\x72\x20\x69\x20\x3d\x20\x30\x3b\x20\x69\x20\x3c\x20\x63\x6f\x75\x6e\x74\x3b\x20\x69\x2b\x2b\x29\x20\x7b\xa"
"\x20\x20\x20\x20\x76\x61\x72\x20\x6f\x20\x3d\x20\x69\x6e\x73\x74\x61\x6e\x63\x65\x2e\x66\x6f\x6f\x3b\xa"
"\x20\x20\x20\x20\x69\x6e\x73\x74\x61\x6e\x63\x65\x2e\x66\x6f\x6f\x20\x3d\x20\x6f\x20\x2b\x20\x31\x3b\xa"
"\x20\x20\x7d\xa"
"\x7d\xa";


class TestApiCallbacks {
 public:
  explicit TestApiCallbacks(int min_duration_ms)
      : min_duration_ms_(min_duration_ms),
        is_warming_up_(false) {}

  static void Getter(v8::Local<v8::String> name,
                     const v8::PropertyCallbackInfo<v8::Value>& info) {
    TestApiCallbacks* data = fromInfo(info);
    data->Wait();
  }

  static void Setter(v8::Local<v8::String> name,
                     v8::Local<v8::Value> value,
                     const v8::PropertyCallbackInfo<void>& info) {
    TestApiCallbacks* data = fromInfo(info);
    data->Wait();
  }

  static void Callback(const v8::FunctionCallbackInfo<v8::Value>& info) {
    TestApiCallbacks* data = fromInfo(info);
    data->Wait();
  }

  void set_warming_up(bool value) { is_warming_up_ = value; }

 private:
  void Wait() {
    if (is_warming_up_) return;
    double start = v8::base::OS::TimeCurrentMillis();
    double duration = 0;
    while (duration < min_duration_ms_) {
      v8::base::OS::Sleep(1);
      duration = v8::base::OS::TimeCurrentMillis() - start;
    }
  }

  template<typename T>
  static TestApiCallbacks* fromInfo(const T& info) {
    void* data = v8::External::Cast(*info.Data())->Value();
    return reinterpret_cast<TestApiCallbacks*>(data);
  }

  int min_duration_ms_;
  bool is_warming_up_;
};


// Test that native accessors are properly reported in the CPU profile.
// This test checks the case when the long-running accessors are called
// only once and the optimizer doesn't have chance to change the invocation
// code.
TEST(NativeAccessorUninitializedIC) {
  LocalContext env;
  v8::Isolate* isolate = env->GetIsolate();
  v8::HandleScope scope(isolate);

  v8::Local<v8::FunctionTemplate> func_template =
      v8::FunctionTemplate::New(isolate);
  v8::Local<v8::ObjectTemplate> instance_template =
      func_template->InstanceTemplate();

  TestApiCallbacks accessors(100);
  v8::Local<v8::External> data =
      v8::External::New(isolate, &accessors);
  instance_template->SetAccessor(
      v8::String::NewFromUtf8(isolate, "\x66\x6f\x6f"),
      &TestApiCallbacks::Getter, &TestApiCallbacks::Setter, data);
  v8::Local<v8::Function> func = func_template->GetFunction();
  v8::Local<v8::Object> instance = func->NewInstance();
  env->Global()->Set(v8::String::NewFromUtf8(isolate, "\x69\x6e\x73\x74\x61\x6e\x63\x65"),
                     instance);

  v8::Script::Compile(
      v8::String::NewFromUtf8(isolate, native_accessor_test_source))
      ->Run();
  v8::Local<v8::Function> function = v8::Local<v8::Function>::Cast(
      env->Global()->Get(v8::String::NewFromUtf8(isolate, "\x73\x74\x61\x72\x74")));

  int32_t repeat_count = 1;
  v8::Handle<v8::Value> args[] = { v8::Integer::New(isolate, repeat_count) };
  v8::CpuProfile* profile =
      RunProfiler(env.local(), function, args, ARRAY_SIZE(args), 180);

  const v8::CpuProfileNode* root = profile->GetTopDownRoot();
  const v8::CpuProfileNode* startNode =
      GetChild(isolate, root, "\x73\x74\x61\x72\x74");
  GetChild(isolate, startNode, "\x67\x65\x74\x20\x66\x6f\x6f");
  GetChild(isolate, startNode, "\x73\x65\x74\x20\x66\x6f\x6f");

  profile->Delete();
}


// Test that native accessors are properly reported in the CPU profile.
// This test makes sure that the accessors are called enough times to become
// hot and to trigger optimizations.
TEST(NativeAccessorMonomorphicIC) {
  LocalContext env;
  v8::Isolate* isolate = env->GetIsolate();
  v8::HandleScope scope(isolate);

  v8::Local<v8::FunctionTemplate> func_template =
      v8::FunctionTemplate::New(isolate);
  v8::Local<v8::ObjectTemplate> instance_template =
      func_template->InstanceTemplate();

  TestApiCallbacks accessors(1);
  v8::Local<v8::External> data =
      v8::External::New(isolate, &accessors);
  instance_template->SetAccessor(
      v8::String::NewFromUtf8(isolate, "\x66\x6f\x6f"),
      &TestApiCallbacks::Getter, &TestApiCallbacks::Setter, data);
  v8::Local<v8::Function> func = func_template->GetFunction();
  v8::Local<v8::Object> instance = func->NewInstance();
  env->Global()->Set(v8::String::NewFromUtf8(isolate, "\x69\x6e\x73\x74\x61\x6e\x63\x65"),
                     instance);

  v8::Script::Compile(
      v8::String::NewFromUtf8(isolate, native_accessor_test_source))
      ->Run();
  v8::Local<v8::Function> function = v8::Local<v8::Function>::Cast(
      env->Global()->Get(v8::String::NewFromUtf8(isolate, "\x73\x74\x61\x72\x74")));

  {
    // Make sure accessors ICs are in monomorphic state before starting
    // profiling.
    accessors.set_warming_up(true);
    int32_t warm_up_iterations = 3;
    v8::Handle<v8::Value> args[] = {
      v8::Integer::New(isolate, warm_up_iterations)
    };
    function->Call(env->Global(), ARRAY_SIZE(args), args);
    accessors.set_warming_up(false);
  }

  int32_t repeat_count = 100;
  v8::Handle<v8::Value> args[] = { v8::Integer::New(isolate, repeat_count) };
  v8::CpuProfile* profile =
      RunProfiler(env.local(), function, args, ARRAY_SIZE(args), 200);

  const v8::CpuProfileNode* root = profile->GetTopDownRoot();
  const v8::CpuProfileNode* startNode =
      GetChild(isolate, root, "\x73\x74\x61\x72\x74");
  GetChild(isolate, startNode, "\x67\x65\x74\x20\x66\x6f\x6f");
  GetChild(isolate, startNode, "\x73\x65\x74\x20\x66\x6f\x6f");

  profile->Delete();
}


static const char* native_method_test_source = "\x66\x75\x6e\x63\x74\x69\x6f\x6e\x20\x73\x74\x61\x72\x74\x28\x63\x6f\x75\x6e\x74\x29\x20\x7b\xa"
"\x20\x20\x66\x6f\x72\x20\x28\x76\x61\x72\x20\x69\x20\x3d\x20\x30\x3b\x20\x69\x20\x3c\x20\x63\x6f\x75\x6e\x74\x3b\x20\x69\x2b\x2b\x29\x20\x7b\xa"
"\x20\x20\x20\x20\x69\x6e\x73\x74\x61\x6e\x63\x65\x2e\x66\x6f\x6f\x4d\x65\x74\x68\x6f\x64\x28\x29\x3b\xa"
"\x20\x20\x7d\xa"
"\x7d\xa";


TEST(NativeMethodUninitializedIC) {
  LocalContext env;
  v8::Isolate* isolate = env->GetIsolate();
  v8::HandleScope scope(isolate);

  TestApiCallbacks callbacks(100);
  v8::Local<v8::External> data =
      v8::External::New(isolate, &callbacks);

  v8::Local<v8::FunctionTemplate> func_template =
      v8::FunctionTemplate::New(isolate);
  func_template->SetClassName(
      v8::String::NewFromUtf8(isolate, "\x54\x65\x73\x74\x5f\x49\x6e\x73\x74\x61\x6e\x63\x65\x43\x6f\x73\x74\x72\x75\x63\x74\x6f\x72"));
  v8::Local<v8::ObjectTemplate> proto_template =
      func_template->PrototypeTemplate();
  v8::Local<v8::Signature> signature =
      v8::Signature::New(isolate, func_template);
  proto_template->Set(v8::String::NewFromUtf8(isolate, "\x66\x6f\x6f\x4d\x65\x74\x68\x6f\x64"),
                      v8::FunctionTemplate::New(isolate,
                                                &TestApiCallbacks::Callback,
                                                data, signature, 0));

  v8::Local<v8::Function> func = func_template->GetFunction();
  v8::Local<v8::Object> instance = func->NewInstance();
  env->Global()->Set(v8::String::NewFromUtf8(isolate, "\x69\x6e\x73\x74\x61\x6e\x63\x65"),
                     instance);

  v8::Script::Compile(v8::String::NewFromUtf8(
                          isolate, native_method_test_source))->Run();
  v8::Local<v8::Function> function = v8::Local<v8::Function>::Cast(
      env->Global()->Get(v8::String::NewFromUtf8(isolate, "\x73\x74\x61\x72\x74")));

  int32_t repeat_count = 1;
  v8::Handle<v8::Value> args[] = { v8::Integer::New(isolate, repeat_count) };
  v8::CpuProfile* profile =
      RunProfiler(env.local(), function, args, ARRAY_SIZE(args), 100);

  const v8::CpuProfileNode* root = profile->GetTopDownRoot();
  const v8::CpuProfileNode* startNode =
      GetChild(isolate, root, "\x73\x74\x61\x72\x74");
  GetChild(isolate, startNode, "\x66\x6f\x6f\x4d\x65\x74\x68\x6f\x64");

  profile->Delete();
}


TEST(NativeMethodMonomorphicIC) {
  LocalContext env;
  v8::Isolate* isolate = env->GetIsolate();
  v8::HandleScope scope(isolate);

  TestApiCallbacks callbacks(1);
  v8::Local<v8::External> data =
      v8::External::New(isolate, &callbacks);

  v8::Local<v8::FunctionTemplate> func_template =
      v8::FunctionTemplate::New(isolate);
  func_template->SetClassName(
      v8::String::NewFromUtf8(isolate, "\x54\x65\x73\x74\x5f\x49\x6e\x73\x74\x61\x6e\x63\x65\x43\x6f\x73\x74\x72\x75\x63\x74\x6f\x72"));
  v8::Local<v8::ObjectTemplate> proto_template =
      func_template->PrototypeTemplate();
  v8::Local<v8::Signature> signature =
      v8::Signature::New(isolate, func_template);
  proto_template->Set(v8::String::NewFromUtf8(isolate, "\x66\x6f\x6f\x4d\x65\x74\x68\x6f\x64"),
                      v8::FunctionTemplate::New(isolate,
                                                &TestApiCallbacks::Callback,
                                                data, signature, 0));

  v8::Local<v8::Function> func = func_template->GetFunction();
  v8::Local<v8::Object> instance = func->NewInstance();
  env->Global()->Set(v8::String::NewFromUtf8(isolate, "\x69\x6e\x73\x74\x61\x6e\x63\x65"),
                     instance);

  v8::Script::Compile(v8::String::NewFromUtf8(
                          isolate, native_method_test_source))->Run();
  v8::Local<v8::Function> function = v8::Local<v8::Function>::Cast(
      env->Global()->Get(v8::String::NewFromUtf8(isolate, "\x73\x74\x61\x72\x74")));
  {
    // Make sure method ICs are in monomorphic state before starting
    // profiling.
    callbacks.set_warming_up(true);
    int32_t warm_up_iterations = 3;
    v8::Handle<v8::Value> args[] = {
      v8::Integer::New(isolate, warm_up_iterations)
    };
    function->Call(env->Global(), ARRAY_SIZE(args), args);
    callbacks.set_warming_up(false);
  }

  int32_t repeat_count = 100;
  v8::Handle<v8::Value> args[] = { v8::Integer::New(isolate, repeat_count) };
  v8::CpuProfile* profile =
      RunProfiler(env.local(), function, args, ARRAY_SIZE(args), 100);

  const v8::CpuProfileNode* root = profile->GetTopDownRoot();
  GetChild(isolate, root, "\x73\x74\x61\x72\x74");
  const v8::CpuProfileNode* startNode =
      GetChild(isolate, root, "\x73\x74\x61\x72\x74");
  GetChild(isolate, startNode, "\x66\x6f\x6f\x4d\x65\x74\x68\x6f\x64");

  profile->Delete();
}


static const char* bound_function_test_source =
    "\x66\x75\x6e\x63\x74\x69\x6f\x6e\x20\x66\x6f\x6f\x28\x29\x20\x7b\xa"
    "\x20\x20\x73\x74\x61\x72\x74\x50\x72\x6f\x66\x69\x6c\x69\x6e\x67\x28\x27\x6d\x79\x5f\x70\x72\x6f\x66\x69\x6c\x65\x27\x29\x3b\xa"
    "\x7d\xa"
    "\x66\x75\x6e\x63\x74\x69\x6f\x6e\x20\x73\x74\x61\x72\x74\x28\x29\x20\x7b\xa"
    "\x20\x20\x76\x61\x72\x20\x63\x61\x6c\x6c\x62\x61\x63\x6b\x20\x3d\x20\x66\x6f\x6f\x2e\x62\x69\x6e\x64\x28\x74\x68\x69\x73\x29\x3b\xa"
    "\x20\x20\x63\x61\x6c\x6c\x62\x61\x63\x6b\x28\x29\x3b\xa"
    "\x7d";


TEST(BoundFunctionCall) {
  v8::HandleScope scope(CcTest::isolate());
  v8::Local<v8::Context> env = CcTest::NewContext(PROFILER_EXTENSION);
  v8::Context::Scope context_scope(env);

  v8::Script::Compile(
      v8::String::NewFromUtf8(env->GetIsolate(), bound_function_test_source))
      ->Run();
  v8::Local<v8::Function> function = v8::Local<v8::Function>::Cast(
      env->Global()->Get(v8::String::NewFromUtf8(env->GetIsolate(), "\x73\x74\x61\x72\x74")));

  v8::CpuProfile* profile = RunProfiler(env, function, NULL, 0, 0);

  const v8::CpuProfileNode* root = profile->GetTopDownRoot();
  ScopedVector<v8::Handle<v8::String> > names(3);
  names[0] = v8::String::NewFromUtf8(
      env->GetIsolate(), ProfileGenerator::kGarbageCollectorEntryName);
  names[1] = v8::String::NewFromUtf8(env->GetIsolate(),
                                     ProfileGenerator::kProgramEntryName);
  names[2] = v8::String::NewFromUtf8(env->GetIsolate(), "\x73\x74\x61\x72\x74");
  // Don't allow |foo| node to be at the top level.
  CheckChildrenNames(root, names);

  const v8::CpuProfileNode* startNode =
      GetChild(env->GetIsolate(), root, "\x73\x74\x61\x72\x74");
  GetChild(env->GetIsolate(), startNode, "\x66\x6f\x6f");

  profile->Delete();
}


static const char* call_function_test_source = "\x66\x75\x6e\x63\x74\x69\x6f\x6e\x20\x62\x61\x72\x28\x69\x74\x65\x72\x61\x74\x69\x6f\x6e\x73\x29\x20\x7b\xa"
"\x7d\xa"
"\x66\x75\x6e\x63\x74\x69\x6f\x6e\x20\x73\x74\x61\x72\x74\x28\x64\x75\x72\x61\x74\x69\x6f\x6e\x29\x20\x7b\xa"
"\x20\x20\x76\x61\x72\x20\x73\x74\x61\x72\x74\x20\x3d\x20\x44\x61\x74\x65\x2e\x6e\x6f\x77\x28\x29\x3b\xa"
"\x20\x20\x77\x68\x69\x6c\x65\x20\x28\x44\x61\x74\x65\x2e\x6e\x6f\x77\x28\x29\x20\x2d\x20\x73\x74\x61\x72\x74\x20\x3c\x20\x64\x75\x72\x61\x74\x69\x6f\x6e\x29\x20\x7b\xa"
"\x20\x20\x20\x20\x74\x72\x79\x20\x7b\xa"
"\x20\x20\x20\x20\x20\x20\x62\x61\x72\x2e\x63\x61\x6c\x6c\x28\x74\x68\x69\x73\x2c\x20\x31\x30\x20\x2a\x20\x31\x30\x30\x30\x29\x3b\xa"
"\x20\x20\x20\x20\x7d\x20\x63\x61\x74\x63\x68\x28\x65\x29\x20\x7b\x7d\xa"
"\x20\x20\x7d\xa"
"\x7d";


// Test that if we sampled thread when it was inside FunctionCall buitin then
// its caller frame will be '(unresolved function)' as we have no reliable way
// to resolve it.
//
// [Top down]:
//    96     0   (root) [-1] #1
//     1     1    (garbage collector) [-1] #4
//     5     0    (unresolved function) [-1] #5
//     5     5      call [-1] #6
//    71    70    start [-1] #3
//     1     1      bar [-1] #7
//    19    19    (program) [-1] #2
TEST(FunctionCallSample) {
  LocalContext env;
  v8::HandleScope scope(env->GetIsolate());

  // Collect garbage that might have be generated while installing extensions.
  CcTest::heap()->CollectAllGarbage(Heap::kNoGCFlags);

  v8::Script::Compile(v8::String::NewFromUtf8(
                          env->GetIsolate(), call_function_test_source))->Run();
  v8::Local<v8::Function> function = v8::Local<v8::Function>::Cast(
      env->Global()->Get(v8::String::NewFromUtf8(env->GetIsolate(), "\x73\x74\x61\x72\x74")));

  int32_t duration_ms = 100;
  v8::Handle<v8::Value> args[] = {
    v8::Integer::New(env->GetIsolate(), duration_ms)
  };
  v8::CpuProfile* profile =
      RunProfiler(env.local(), function, args, ARRAY_SIZE(args), 100);

  const v8::CpuProfileNode* root = profile->GetTopDownRoot();
  {
    ScopedVector<v8::Handle<v8::String> > names(4);
    names[0] = v8::String::NewFromUtf8(
        env->GetIsolate(), ProfileGenerator::kGarbageCollectorEntryName);
    names[1] = v8::String::NewFromUtf8(env->GetIsolate(),
                                       ProfileGenerator::kProgramEntryName);
    names[2] = v8::String::NewFromUtf8(env->GetIsolate(), "\x73\x74\x61\x72\x74");
    names[3] = v8::String::NewFromUtf8(
        env->GetIsolate(), i::ProfileGenerator::kUnresolvedFunctionName);
    // Don't allow |bar| and |call| nodes to be at the top level.
    CheckChildrenNames(root, names);
  }

  // In case of GC stress tests all samples may be in GC phase and there
  // won't be |start| node in the profiles.
  bool is_gc_stress_testing =
      (i::FLAG_gc_interval != -1) || i::FLAG_stress_compaction;
  const v8::CpuProfileNode* startNode =
      FindChild(env->GetIsolate(), root, "\x73\x74\x61\x72\x74");
  CHECK(is_gc_stress_testing || startNode);
  if (startNode) {
    ScopedVector<v8::Handle<v8::String> > names(2);
    names[0] = v8::String::NewFromUtf8(env->GetIsolate(), "\x62\x61\x72");
    names[1] = v8::String::NewFromUtf8(env->GetIsolate(), "\x63\x61\x6c\x6c");
    CheckChildrenNames(startNode, names);
  }

  const v8::CpuProfileNode* unresolvedNode = FindChild(
      env->GetIsolate(), root, i::ProfileGenerator::kUnresolvedFunctionName);
  if (unresolvedNode) {
    ScopedVector<v8::Handle<v8::String> > names(1);
    names[0] = v8::String::NewFromUtf8(env->GetIsolate(), "\x63\x61\x6c\x6c");
    CheckChildrenNames(unresolvedNode, names);
  }

  profile->Delete();
}


static const char* function_apply_test_source = "\x66\x75\x6e\x63\x74\x69\x6f\x6e\x20\x62\x61\x72\x28\x69\x74\x65\x72\x61\x74\x69\x6f\x6e\x73\x29\x20\x7b\xa"
"\x7d\xa"
"\x66\x75\x6e\x63\x74\x69\x6f\x6e\x20\x74\x65\x73\x74\x28\x29\x20\x7b\xa"
"\x20\x20\x62\x61\x72\x2e\x61\x70\x70\x6c\x79\x28\x74\x68\x69\x73\x2c\x20\x5b\x31\x30\x20\x2a\x20\x31\x30\x30\x30\x5d\x29\x3b\xa"
"\x7d\xa"
"\x66\x75\x6e\x63\x74\x69\x6f\x6e\x20\x73\x74\x61\x72\x74\x28\x64\x75\x72\x61\x74\x69\x6f\x6e\x29\x20\x7b\xa"
"\x20\x20\x76\x61\x72\x20\x73\x74\x61\x72\x74\x20\x3d\x20\x44\x61\x74\x65\x2e\x6e\x6f\x77\x28\x29\x3b\xa"
"\x20\x20\x77\x68\x69\x6c\x65\x20\x28\x44\x61\x74\x65\x2e\x6e\x6f\x77\x28\x29\x20\x2d\x20\x73\x74\x61\x72\x74\x20\x3c\x20\x64\x75\x72\x61\x74\x69\x6f\x6e\x29\x20\x7b\xa"
"\x20\x20\x20\x20\x74\x72\x79\x20\x7b\xa"
"\x20\x20\x20\x20\x20\x20\x74\x65\x73\x74\x28\x29\x3b\xa"
"\x20\x20\x20\x20\x7d\x20\x63\x61\x74\x63\x68\x28\x65\x29\x20\x7b\x7d\xa"
"\x20\x20\x7d\xa"
"\x7d";


// [Top down]:
//    94     0   (root) [-1] #0 1
//     2     2    (garbage collector) [-1] #0 7
//    82    49    start [-1] #16 3
//     1     0      (unresolved function) [-1] #0 8
//     1     1        apply [-1] #0 9
//    32    21      test [-1] #16 4
//     2     2        bar [-1] #16 6
//     9     9        apply [-1] #0 5
//    10    10    (program) [-1] #0 2
TEST(FunctionApplySample) {
  LocalContext env;
  v8::HandleScope scope(env->GetIsolate());

  v8::Script::Compile(
      v8::String::NewFromUtf8(env->GetIsolate(), function_apply_test_source))
      ->Run();
  v8::Local<v8::Function> function = v8::Local<v8::Function>::Cast(
      env->Global()->Get(v8::String::NewFromUtf8(env->GetIsolate(), "\x73\x74\x61\x72\x74")));

  int32_t duration_ms = 100;
  v8::Handle<v8::Value> args[] = {
    v8::Integer::New(env->GetIsolate(), duration_ms)
  };

  v8::CpuProfile* profile =
      RunProfiler(env.local(), function, args, ARRAY_SIZE(args), 100);

  const v8::CpuProfileNode* root = profile->GetTopDownRoot();
  {
    ScopedVector<v8::Handle<v8::String> > names(3);
    names[0] = v8::String::NewFromUtf8(
        env->GetIsolate(), ProfileGenerator::kGarbageCollectorEntryName);
    names[1] = v8::String::NewFromUtf8(env->GetIsolate(),
                                       ProfileGenerator::kProgramEntryName);
    names[2] = v8::String::NewFromUtf8(env->GetIsolate(), "\x73\x74\x61\x72\x74");
    // Don't allow |test|, |bar| and |apply| nodes to be at the top level.
    CheckChildrenNames(root, names);
  }

  const v8::CpuProfileNode* startNode =
      FindChild(env->GetIsolate(), root, "\x73\x74\x61\x72\x74");
  if (startNode) {
    {
      ScopedVector<v8::Handle<v8::String> > names(2);
      names[0] = v8::String::NewFromUtf8(env->GetIsolate(), "\x74\x65\x73\x74");
      names[1] = v8::String::NewFromUtf8(
          env->GetIsolate(), ProfileGenerator::kUnresolvedFunctionName);
      CheckChildrenNames(startNode, names);
    }

    const v8::CpuProfileNode* testNode =
        FindChild(env->GetIsolate(), startNode, "\x74\x65\x73\x74");
    if (testNode) {
      ScopedVector<v8::Handle<v8::String> > names(3);
      names[0] = v8::String::NewFromUtf8(env->GetIsolate(), "\x62\x61\x72");
      names[1] = v8::String::NewFromUtf8(env->GetIsolate(), "\x61\x70\x70\x6c\x79");
      // apply calls "get length" before invoking the function itself
      // and we may get hit into it.
      names[2] = v8::String::NewFromUtf8(env->GetIsolate(), "\x67\x65\x74\x20\x6c\x65\x6e\x67\x74\x68");
      CheckChildrenNames(testNode, names);
    }

    if (const v8::CpuProfileNode* unresolvedNode =
            FindChild(env->GetIsolate(), startNode,
                      ProfileGenerator::kUnresolvedFunctionName)) {
      ScopedVector<v8::Handle<v8::String> > names(1);
      names[0] = v8::String::NewFromUtf8(env->GetIsolate(), "\x61\x70\x70\x6c\x79");
      CheckChildrenNames(unresolvedNode, names);
      GetChild(env->GetIsolate(), unresolvedNode, "\x61\x70\x70\x6c\x79");
    }
  }

  profile->Delete();
}


static const char* cpu_profiler_deep_stack_test_source =
"\x66\x75\x6e\x63\x74\x69\x6f\x6e\x20\x66\x6f\x6f\x28\x6e\x29\x20\x7b\xa"
"\x20\x20\x69\x66\x20\x28\x6e\x29\xa"
"\x20\x20\x20\x20\x66\x6f\x6f\x28\x6e\x20\x2d\x20\x31\x29\x3b\xa"
"\x20\x20\x65\x6c\x73\x65\xa"
"\x20\x20\x20\x20\x73\x74\x61\x72\x74\x50\x72\x6f\x66\x69\x6c\x69\x6e\x67\x28\x27\x6d\x79\x5f\x70\x72\x6f\x66\x69\x6c\x65\x27\x29\x3b\xa"
"\x7d\xa"
"\x66\x75\x6e\x63\x74\x69\x6f\x6e\x20\x73\x74\x61\x72\x74\x28\x29\x20\x7b\xa"
"\x20\x20\x66\x6f\x6f\x28\x32\x35\x30\x29\x3b\xa"
"\x7d\xa";


// Check a deep stack
//
// [Top down]:
//    0  (root) 0 #1
//    2    (program) 0 #2
//    0    start 21 #3 no reason
//    0      foo 21 #4 no reason
//    0        foo 21 #5 no reason
//                ....
//    0          foo 21 #253 no reason
//    1            startProfiling 0 #254
TEST(CpuProfileDeepStack) {
  v8::HandleScope scope(CcTest::isolate());
  v8::Local<v8::Context> env = CcTest::NewContext(PROFILER_EXTENSION);
  v8::Context::Scope context_scope(env);

  v8::Script::Compile(v8::String::NewFromUtf8(
      env->GetIsolate(), cpu_profiler_deep_stack_test_source))->Run();
  v8::Local<v8::Function> function = v8::Local<v8::Function>::Cast(
      env->Global()->Get(v8::String::NewFromUtf8(env->GetIsolate(), "\x73\x74\x61\x72\x74")));

  v8::CpuProfiler* cpu_profiler = env->GetIsolate()->GetCpuProfiler();
  v8::Local<v8::String> profile_name =
      v8::String::NewFromUtf8(env->GetIsolate(), "\x6d\x79\x5f\x70\x72\x6f\x66\x69\x6c\x65");
  function->Call(env->Global(), 0, NULL);
  v8::CpuProfile* profile = cpu_profiler->StopProfiling(profile_name);
  CHECK_NE(NULL, profile);
  // Dump collected profile to have a better diagnostic in case of failure.
  reinterpret_cast<i::CpuProfile*>(profile)->Print();

  const v8::CpuProfileNode* root = profile->GetTopDownRoot();
  {
    ScopedVector<v8::Handle<v8::String> > names(3);
    names[0] = v8::String::NewFromUtf8(
        env->GetIsolate(), ProfileGenerator::kGarbageCollectorEntryName);
    names[1] = v8::String::NewFromUtf8(env->GetIsolate(),
                                       ProfileGenerator::kProgramEntryName);
    names[2] = v8::String::NewFromUtf8(env->GetIsolate(), "\x73\x74\x61\x72\x74");
    CheckChildrenNames(root, names);
  }

  const v8::CpuProfileNode* node =
      GetChild(env->GetIsolate(), root, "\x73\x74\x61\x72\x74");
  for (int i = 0; i < 250; ++i) {
    node = GetChild(env->GetIsolate(), node, "\x66\x6f\x6f");
  }
  // TODO(alph):
  // In theory there must be one more 'foo' and a 'startProfiling' nodes,
  // but due to unstable top frame extraction these might be missing.

  profile->Delete();
}


static const char* js_native_js_test_source =
    "\x66\x75\x6e\x63\x74\x69\x6f\x6e\x20\x66\x6f\x6f\x28\x29\x20\x7b\xa"
    "\x20\x20\x73\x74\x61\x72\x74\x50\x72\x6f\x66\x69\x6c\x69\x6e\x67\x28\x27\x6d\x79\x5f\x70\x72\x6f\x66\x69\x6c\x65\x27\x29\x3b\xa"
    "\x7d\xa"
    "\x66\x75\x6e\x63\x74\x69\x6f\x6e\x20\x62\x61\x72\x28\x29\x20\x7b\xa"
    "\x20\x20\x74\x72\x79\x20\x7b\x20\x66\x6f\x6f\x28\x29\x3b\x20\x7d\x20\x63\x61\x74\x63\x68\x28\x65\x29\x20\x7b\x7d\xa"
    "\x7d\xa"
    "\x66\x75\x6e\x63\x74\x69\x6f\x6e\x20\x73\x74\x61\x72\x74\x28\x29\x20\x7b\xa"
    "\x20\x20\x74\x72\x79\x20\x7b\xa"
    "\x20\x20\x20\x20\x43\x61\x6c\x6c\x4a\x73\x46\x75\x6e\x63\x74\x69\x6f\x6e\x28\x62\x61\x72\x29\x3b\xa"
    "\x20\x20\x7d\x20\x63\x61\x74\x63\x68\x28\x65\x29\x20\x7b\x7d\xa"
    "\x7d";

static void CallJsFunction(const v8::FunctionCallbackInfo<v8::Value>& info) {
  v8::Handle<v8::Function> function = info[0].As<v8::Function>();
  v8::Handle<v8::Value> argv[] = { info[1] };
  function->Call(info.This(), ARRAY_SIZE(argv), argv);
}


// [Top down]:
//    58     0   (root) #0 1
//     2     2    (program) #0 2
//    56     1    start #16 3
//    55     0      CallJsFunction #0 4
//    55     1        bar #16 5
//    54    54          foo #16 6
TEST(JsNativeJsSample) {
  v8::HandleScope scope(CcTest::isolate());
  v8::Local<v8::Context> env = CcTest::NewContext(PROFILER_EXTENSION);
  v8::Context::Scope context_scope(env);

  v8::Local<v8::FunctionTemplate> func_template = v8::FunctionTemplate::New(
      env->GetIsolate(), CallJsFunction);
  v8::Local<v8::Function> func = func_template->GetFunction();
  func->SetName(v8::String::NewFromUtf8(env->GetIsolate(), "\x43\x61\x6c\x6c\x4a\x73\x46\x75\x6e\x63\x74\x69\x6f\x6e"));
  env->Global()->Set(
      v8::String::NewFromUtf8(env->GetIsolate(), "\x43\x61\x6c\x6c\x4a\x73\x46\x75\x6e\x63\x74\x69\x6f\x6e"), func);

  v8::Script::Compile(v8::String::NewFromUtf8(env->GetIsolate(),
                                              js_native_js_test_source))->Run();
  v8::Local<v8::Function> function = v8::Local<v8::Function>::Cast(
      env->Global()->Get(v8::String::NewFromUtf8(env->GetIsolate(), "\x73\x74\x61\x72\x74")));

  v8::CpuProfile* profile = RunProfiler(env, function, NULL, 0, 0);

  const v8::CpuProfileNode* root = profile->GetTopDownRoot();
  {
    ScopedVector<v8::Handle<v8::String> > names(3);
    names[0] = v8::String::NewFromUtf8(
        env->GetIsolate(), ProfileGenerator::kGarbageCollectorEntryName);
    names[1] = v8::String::NewFromUtf8(env->GetIsolate(),
                                       ProfileGenerator::kProgramEntryName);
    names[2] = v8::String::NewFromUtf8(env->GetIsolate(), "\x73\x74\x61\x72\x74");
    CheckChildrenNames(root, names);
  }

  const v8::CpuProfileNode* startNode =
      GetChild(env->GetIsolate(), root, "\x73\x74\x61\x72\x74");
  CHECK_EQ(1, startNode->GetChildrenCount());
  const v8::CpuProfileNode* nativeFunctionNode =
      GetChild(env->GetIsolate(), startNode, "\x43\x61\x6c\x6c\x4a\x73\x46\x75\x6e\x63\x74\x69\x6f\x6e");

  CHECK_EQ(1, nativeFunctionNode->GetChildrenCount());
  const v8::CpuProfileNode* barNode =
      GetChild(env->GetIsolate(), nativeFunctionNode, "\x62\x61\x72");

  CHECK_EQ(1, barNode->GetChildrenCount());
  GetChild(env->GetIsolate(), barNode, "\x66\x6f\x6f");

  profile->Delete();
}


static const char* js_native_js_runtime_js_test_source =
    "\x66\x75\x6e\x63\x74\x69\x6f\x6e\x20\x66\x6f\x6f\x28\x29\x20\x7b\xa"
    "\x20\x20\x73\x74\x61\x72\x74\x50\x72\x6f\x66\x69\x6c\x69\x6e\x67\x28\x27\x6d\x79\x5f\x70\x72\x6f\x66\x69\x6c\x65\x27\x29\x3b\xa"
    "\x7d\xa"
    "\x76\x61\x72\x20\x62\x6f\x75\x6e\x64\x20\x3d\x20\x66\x6f\x6f\x2e\x62\x69\x6e\x64\x28\x74\x68\x69\x73\x29\x3b\xa"
    "\x66\x75\x6e\x63\x74\x69\x6f\x6e\x20\x62\x61\x72\x28\x29\x20\x7b\xa"
    "\x20\x20\x74\x72\x79\x20\x7b\x20\x62\x6f\x75\x6e\x64\x28\x29\x3b\x20\x7d\x20\x63\x61\x74\x63\x68\x28\x65\x29\x20\x7b\x7d\xa"
    "\x7d\xa"
    "\x66\x75\x6e\x63\x74\x69\x6f\x6e\x20\x73\x74\x61\x72\x74\x28\x29\x20\x7b\xa"
    "\x20\x20\x74\x72\x79\x20\x7b\xa"
    "\x20\x20\x20\x20\x43\x61\x6c\x6c\x4a\x73\x46\x75\x6e\x63\x74\x69\x6f\x6e\x28\x62\x61\x72\x29\x3b\xa"
    "\x20\x20\x7d\x20\x63\x61\x74\x63\x68\x28\x65\x29\x20\x7b\x7d\xa"
    "\x7d";


// [Top down]:
//    57     0   (root) #0 1
//    55     1    start #16 3
//    54     0      CallJsFunction #0 4
//    54     3        bar #16 5
//    51    51          foo #16 6
//     2     2    (program) #0 2
TEST(JsNativeJsRuntimeJsSample) {
  v8::HandleScope scope(CcTest::isolate());
  v8::Local<v8::Context> env = CcTest::NewContext(PROFILER_EXTENSION);
  v8::Context::Scope context_scope(env);

  v8::Local<v8::FunctionTemplate> func_template = v8::FunctionTemplate::New(
      env->GetIsolate(), CallJsFunction);
  v8::Local<v8::Function> func = func_template->GetFunction();
  func->SetName(v8::String::NewFromUtf8(env->GetIsolate(), "\x43\x61\x6c\x6c\x4a\x73\x46\x75\x6e\x63\x74\x69\x6f\x6e"));
  env->Global()->Set(
      v8::String::NewFromUtf8(env->GetIsolate(), "\x43\x61\x6c\x6c\x4a\x73\x46\x75\x6e\x63\x74\x69\x6f\x6e"), func);

  v8::Script::Compile(
      v8::String::NewFromUtf8(env->GetIsolate(),
                              js_native_js_runtime_js_test_source))->Run();
  v8::Local<v8::Function> function = v8::Local<v8::Function>::Cast(
      env->Global()->Get(v8::String::NewFromUtf8(env->GetIsolate(), "\x73\x74\x61\x72\x74")));

  v8::CpuProfile* profile = RunProfiler(env, function, NULL, 0, 0);

  const v8::CpuProfileNode* root = profile->GetTopDownRoot();
  ScopedVector<v8::Handle<v8::String> > names(3);
  names[0] = v8::String::NewFromUtf8(
      env->GetIsolate(), ProfileGenerator::kGarbageCollectorEntryName);
  names[1] = v8::String::NewFromUtf8(env->GetIsolate(),
                                     ProfileGenerator::kProgramEntryName);
  names[2] = v8::String::NewFromUtf8(env->GetIsolate(), "\x73\x74\x61\x72\x74");
  CheckChildrenNames(root, names);

  const v8::CpuProfileNode* startNode =
      GetChild(env->GetIsolate(), root, "\x73\x74\x61\x72\x74");
  CHECK_EQ(1, startNode->GetChildrenCount());
  const v8::CpuProfileNode* nativeFunctionNode =
      GetChild(env->GetIsolate(), startNode, "\x43\x61\x6c\x6c\x4a\x73\x46\x75\x6e\x63\x74\x69\x6f\x6e");

  CHECK_EQ(1, nativeFunctionNode->GetChildrenCount());
  const v8::CpuProfileNode* barNode =
      GetChild(env->GetIsolate(), nativeFunctionNode, "\x62\x61\x72");

  // The child is in fact a bound foo.
  // A bound function has a wrapper that may make calls to
  // other functions e.g. "get length".
  CHECK_LE(1, barNode->GetChildrenCount());
  CHECK_GE(2, barNode->GetChildrenCount());
  GetChild(env->GetIsolate(), barNode, "\x66\x6f\x6f");

  profile->Delete();
}


static void CallJsFunction2(const v8::FunctionCallbackInfo<v8::Value>& info) {
  v8::base::OS::Print("\x49\x6e\x20\x43\x61\x6c\x6c\x4a\x73\x46\x75\x6e\x63\x74\x69\x6f\x6e\x32\xa");
  CallJsFunction(info);
}


static const char* js_native1_js_native2_js_test_source =
    "\x66\x75\x6e\x63\x74\x69\x6f\x6e\x20\x66\x6f\x6f\x28\x29\x20\x7b\xa"
    "\x20\x20\x74\x72\x79\x20\x7b\xa"
    "\x20\x20\x20\x20\x73\x74\x61\x72\x74\x50\x72\x6f\x66\x69\x6c\x69\x6e\x67\x28\x27\x6d\x79\x5f\x70\x72\x6f\x66\x69\x6c\x65\x27\x29\x3b\xa"
    "\x20\x20\x7d\x20\x63\x61\x74\x63\x68\x28\x65\x29\x20\x7b\x7d\xa"
    "\x7d\xa"
    "\x66\x75\x6e\x63\x74\x69\x6f\x6e\x20\x62\x61\x72\x28\x29\x20\x7b\xa"
    "\x20\x20\x43\x61\x6c\x6c\x4a\x73\x46\x75\x6e\x63\x74\x69\x6f\x6e\x32\x28\x66\x6f\x6f\x29\x3b\xa"
    "\x7d\xa"
    "\x66\x75\x6e\x63\x74\x69\x6f\x6e\x20\x73\x74\x61\x72\x74\x28\x29\x20\x7b\xa"
    "\x20\x20\x74\x72\x79\x20\x7b\xa"
    "\x20\x20\x20\x20\x43\x61\x6c\x6c\x4a\x73\x46\x75\x6e\x63\x74\x69\x6f\x6e\x31\x28\x62\x61\x72\x29\x3b\xa"
    "\x20\x20\x7d\x20\x63\x61\x74\x63\x68\x28\x65\x29\x20\x7b\x7d\xa"
    "\x7d";


// [Top down]:
//    57     0   (root) #0 1
//    55     1    start #16 3
//    54     0      CallJsFunction1 #0 4
//    54     0        bar #16 5
//    54     0          CallJsFunction2 #0 6
//    54    54            foo #16 7
//     2     2    (program) #0 2
TEST(JsNative1JsNative2JsSample) {
  v8::HandleScope scope(CcTest::isolate());
  v8::Local<v8::Context> env = CcTest::NewContext(PROFILER_EXTENSION);
  v8::Context::Scope context_scope(env);

  v8::Local<v8::FunctionTemplate> func_template = v8::FunctionTemplate::New(
      env->GetIsolate(), CallJsFunction);
  v8::Local<v8::Function> func1 = func_template->GetFunction();
  func1->SetName(v8::String::NewFromUtf8(env->GetIsolate(), "\x43\x61\x6c\x6c\x4a\x73\x46\x75\x6e\x63\x74\x69\x6f\x6e\x31"));
  env->Global()->Set(
      v8::String::NewFromUtf8(env->GetIsolate(), "\x43\x61\x6c\x6c\x4a\x73\x46\x75\x6e\x63\x74\x69\x6f\x6e\x31"), func1);

  v8::Local<v8::Function> func2 = v8::FunctionTemplate::New(
      env->GetIsolate(), CallJsFunction2)->GetFunction();
  func2->SetName(v8::String::NewFromUtf8(env->GetIsolate(), "\x43\x61\x6c\x6c\x4a\x73\x46\x75\x6e\x63\x74\x69\x6f\x6e\x32"));
  env->Global()->Set(
      v8::String::NewFromUtf8(env->GetIsolate(), "\x43\x61\x6c\x6c\x4a\x73\x46\x75\x6e\x63\x74\x69\x6f\x6e\x32"), func2);

  v8::Script::Compile(
      v8::String::NewFromUtf8(env->GetIsolate(),
                              js_native1_js_native2_js_test_source))->Run();
  v8::Local<v8::Function> function = v8::Local<v8::Function>::Cast(
      env->Global()->Get(v8::String::NewFromUtf8(env->GetIsolate(), "\x73\x74\x61\x72\x74")));

  v8::CpuProfile* profile = RunProfiler(env, function, NULL, 0, 0);

  const v8::CpuProfileNode* root = profile->GetTopDownRoot();
  ScopedVector<v8::Handle<v8::String> > names(3);
  names[0] = v8::String::NewFromUtf8(
      env->GetIsolate(), ProfileGenerator::kGarbageCollectorEntryName);
  names[1] = v8::String::NewFromUtf8(env->GetIsolate(),
                                     ProfileGenerator::kProgramEntryName);
  names[2] = v8::String::NewFromUtf8(env->GetIsolate(), "\x73\x74\x61\x72\x74");
  CheckChildrenNames(root, names);

  const v8::CpuProfileNode* startNode =
      GetChild(env->GetIsolate(), root, "\x73\x74\x61\x72\x74");
  CHECK_EQ(1, startNode->GetChildrenCount());
  const v8::CpuProfileNode* nativeNode1 =
      GetChild(env->GetIsolate(), startNode, "\x43\x61\x6c\x6c\x4a\x73\x46\x75\x6e\x63\x74\x69\x6f\x6e\x31");

  CHECK_EQ(1, nativeNode1->GetChildrenCount());
  const v8::CpuProfileNode* barNode =
      GetChild(env->GetIsolate(), nativeNode1, "\x62\x61\x72");

  CHECK_EQ(1, barNode->GetChildrenCount());
  const v8::CpuProfileNode* nativeNode2 =
      GetChild(env->GetIsolate(), barNode, "\x43\x61\x6c\x6c\x4a\x73\x46\x75\x6e\x63\x74\x69\x6f\x6e\x32");

  CHECK_EQ(1, nativeNode2->GetChildrenCount());
  GetChild(env->GetIsolate(), nativeNode2, "\x66\x6f\x6f");

  profile->Delete();
}


// [Top down]:
//     6     0   (root) #0 1
//     3     3    (program) #0 2
//     3     3    (idle) #0 3
TEST(IdleTime) {
  LocalContext env;
  v8::HandleScope scope(env->GetIsolate());
  v8::CpuProfiler* cpu_profiler = env->GetIsolate()->GetCpuProfiler();

  v8::Local<v8::String> profile_name =
      v8::String::NewFromUtf8(env->GetIsolate(), "\x6d\x79\x5f\x70\x72\x6f\x66\x69\x6c\x65");
  cpu_profiler->StartProfiling(profile_name);

  i::Isolate* isolate = CcTest::i_isolate();
  i::ProfilerEventsProcessor* processor = isolate->cpu_profiler()->processor();
  processor->AddCurrentStack(isolate);

  cpu_profiler->SetIdle(true);

  for (int i = 0; i < 3; i++) {
    processor->AddCurrentStack(isolate);
  }

  cpu_profiler->SetIdle(false);
  processor->AddCurrentStack(isolate);


  v8::CpuProfile* profile = cpu_profiler->StopProfiling(profile_name);
  CHECK_NE(NULL, profile);
  // Dump collected profile to have a better diagnostic in case of failure.
  reinterpret_cast<i::CpuProfile*>(profile)->Print();

  const v8::CpuProfileNode* root = profile->GetTopDownRoot();
  ScopedVector<v8::Handle<v8::String> > names(3);
  names[0] = v8::String::NewFromUtf8(
      env->GetIsolate(), ProfileGenerator::kGarbageCollectorEntryName);
  names[1] = v8::String::NewFromUtf8(env->GetIsolate(),
                                     ProfileGenerator::kProgramEntryName);
  names[2] = v8::String::NewFromUtf8(env->GetIsolate(),
                                     ProfileGenerator::kIdleEntryName);
  CheckChildrenNames(root, names);

  const v8::CpuProfileNode* programNode =
      GetChild(env->GetIsolate(), root, ProfileGenerator::kProgramEntryName);
  CHECK_EQ(0, programNode->GetChildrenCount());
  CHECK_GE(programNode->GetHitCount(), 3);

  const v8::CpuProfileNode* idleNode =
      GetChild(env->GetIsolate(), root, ProfileGenerator::kIdleEntryName);
  CHECK_EQ(0, idleNode->GetChildrenCount());
  CHECK_GE(idleNode->GetHitCount(), 3);

  profile->Delete();
}


static void CheckFunctionDetails(v8::Isolate* isolate,
                                 const v8::CpuProfileNode* node,
                                 const char* name, const char* script_name,
                                 int script_id, int line, int column) {
  CHECK_EQ(v8::String::NewFromUtf8(isolate, name),
           node->GetFunctionName());
  CHECK_EQ(v8::String::NewFromUtf8(isolate, script_name),
           node->GetScriptResourceName());
  CHECK_EQ(script_id, node->GetScriptId());
  CHECK_EQ(line, node->GetLineNumber());
  CHECK_EQ(column, node->GetColumnNumber());
}


TEST(FunctionDetails) {
  v8::HandleScope scope(CcTest::isolate());
  v8::Local<v8::Context> env = CcTest::NewContext(PROFILER_EXTENSION);
  v8::Context::Scope context_scope(env);

  v8::Handle<v8::Script> script_a = CompileWithOrigin(
          "\x20\x20\x20\x20\x66\x75\x6e\x63\x74\x69\x6f\x6e\x20\x66\x6f\x6f\xa\x28\x29\x20\x7b\x20\x74\x72\x79\x20\x7b\x20\x62\x61\x72\x28\x29\x3b\x20\x7d\x20\x63\x61\x74\x63\x68\x28\x65\x29\x20\x7b\x7d\x20\x7d\xa"
          "\x20\x66\x75\x6e\x63\x74\x69\x6f\x6e\x20\x62\x61\x72\x28\x29\x20\x7b\x20\x73\x74\x61\x72\x74\x50\x72\x6f\x66\x69\x6c\x69\x6e\x67\x28\x29\x3b\x20\x7d\xa",
          "\x73\x63\x72\x69\x70\x74\x5f\x61");
  script_a->Run();
  v8::Handle<v8::Script> script_b = CompileWithOrigin(
          "\xa\xa\x20\x20\x20\x66\x75\x6e\x63\x74\x69\x6f\x6e\x20\x62\x61\x7a\x28\x29\x20\x7b\x20\x74\x72\x79\x20\x7b\x20\x66\x6f\x6f\x28\x29\x3b\x20\x7d\x20\x63\x61\x74\x63\x68\x28\x65\x29\x20\x7b\x7d\x20\x7d\xa"
          "\xa\xab\x61\x7a\x28\x29\x3b\xa"
          "\x73\x74\x6f\x70\x50\x72\x6f\x66\x69\x6c\x69\x6e\x67\x28\x29\x3b\xa",
          "\x73\x63\x72\x69\x70\x74\x5f\x62");
  script_b->Run();
  const v8::CpuProfile* profile = i::ProfilerExtension::last_profile;
  const v8::CpuProfileNode* current = profile->GetTopDownRoot();
  reinterpret_cast<ProfileNode*>(
      const_cast<v8::CpuProfileNode*>(current))->Print(0);
  // The tree should look like this:
  //  0   (root) 0 #1
  //  0    "" 19 #2 no reason script_b:1
  //  0      baz 19 #3 TryCatchStatement script_b:3
  //  0        foo 18 #4 TryCatchStatement script_a:2
  //  1          bar 18 #5 no reason script_a:3
  const v8::CpuProfileNode* root = profile->GetTopDownRoot();
  const v8::CpuProfileNode* script = GetChild(env->GetIsolate(), root, "");
  CheckFunctionDetails(env->GetIsolate(), script, "", "\x73\x63\x72\x69\x70\x74\x5f\x62",
                       script_b->GetUnboundScript()->GetId(), 1, 1);
  const v8::CpuProfileNode* baz = GetChild(env->GetIsolate(), script, "\x62\x61\x7a");
  CheckFunctionDetails(env->GetIsolate(), baz, "\x62\x61\x7a", "\x73\x63\x72\x69\x70\x74\x5f\x62",
                       script_b->GetUnboundScript()->GetId(), 3, 16);
  const v8::CpuProfileNode* foo = GetChild(env->GetIsolate(), baz, "\x66\x6f\x6f");
  CheckFunctionDetails(env->GetIsolate(), foo, "\x66\x6f\x6f", "\x73\x63\x72\x69\x70\x74\x5f\x61",
                       script_a->GetUnboundScript()->GetId(), 2, 1);
  const v8::CpuProfileNode* bar = GetChild(env->GetIsolate(), foo, "\x62\x61\x72");
  CheckFunctionDetails(env->GetIsolate(), bar, "\x62\x61\x72", "\x73\x63\x72\x69\x70\x74\x5f\x61",
                       script_a->GetUnboundScript()->GetId(), 3, 14);
}


TEST(DontStopOnFinishedProfileDelete) {
  v8::HandleScope scope(CcTest::isolate());
  v8::Local<v8::Context> env = CcTest::NewContext(PROFILER_EXTENSION);
  v8::Context::Scope context_scope(env);
  v8::Isolate* isolate = env->GetIsolate();

  v8::CpuProfiler* profiler = env->GetIsolate()->GetCpuProfiler();
  i::CpuProfiler* iprofiler = reinterpret_cast<i::CpuProfiler*>(profiler);

  CHECK_EQ(0, iprofiler->GetProfilesCount());
  v8::Handle<v8::String> outer = v8::String::NewFromUtf8(isolate, "\x6f\x75\x74\x65\x72");
  profiler->StartProfiling(outer);
  CHECK_EQ(0, iprofiler->GetProfilesCount());

  v8::Handle<v8::String> inner = v8::String::NewFromUtf8(isolate, "\x69\x6e\x6e\x65\x72");
  profiler->StartProfiling(inner);
  CHECK_EQ(0, iprofiler->GetProfilesCount());

  v8::CpuProfile* inner_profile = profiler->StopProfiling(inner);
  CHECK(inner_profile);
  CHECK_EQ(1, iprofiler->GetProfilesCount());
  inner_profile->Delete();
  inner_profile = NULL;
  CHECK_EQ(0, iprofiler->GetProfilesCount());

  v8::CpuProfile* outer_profile = profiler->StopProfiling(outer);
  CHECK(outer_profile);
  CHECK_EQ(1, iprofiler->GetProfilesCount());
  outer_profile->Delete();
  outer_profile = NULL;
  CHECK_EQ(0, iprofiler->GetProfilesCount());
}
