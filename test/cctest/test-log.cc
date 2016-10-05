// Copyright 2006-2009 the V8 project authors. All rights reserved.
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
// Tests of logging functions from log.h

#ifdef __linux__
#include <pthread.h>
#include <signal.h>
#include <unistd.h>
#include <cmath>
#endif  // __linux__

#include "src/v8.h"

#include "src/cpu-profiler.h"
#include "src/log.h"
#include "src/log-utils.h"
#include "src/natives.h"
#include "src/utils.h"
#include "src/v8threads.h"
#include "src/vm-state-inl.h"
#include "test/cctest/cctest.h"

using v8::internal::Address;
using v8::internal::EmbeddedVector;
using v8::internal::Logger;
using v8::internal::StrLength;

namespace {


class ScopedLoggerInitializer {
 public:
  ScopedLoggerInitializer()
      : saved_log_(i::FLAG_log),
        saved_prof_(i::FLAG_prof),
        temp_file_(NULL),
        // Need to run this prior to creating the scope.
        trick_to_run_init_flags_(init_flags_()),
        scope_(CcTest::isolate()),
        env_(v8::Context::New(CcTest::isolate())),
        logger_(CcTest::i_isolate()->logger()) {
    env_->Enter();
  }

  ~ScopedLoggerInitializer() {
    env_->Exit();
    logger_->TearDown();
    if (temp_file_ != NULL) fclose(temp_file_);
    i::FLAG_prof = saved_prof_;
    i::FLAG_log = saved_log_;
  }

  v8::Handle<v8::Context>& env() { return env_; }

  Logger* logger() { return logger_; }

  FILE* StopLoggingGetTempFile() {
    temp_file_ = logger_->TearDown();
    CHECK_NE(NULL, temp_file_);
    fflush(temp_file_);
    rewind(temp_file_);
    return temp_file_;
  }

 private:
  static bool init_flags_() {
    i::FLAG_log = true;
    i::FLAG_prof = true;
    i::FLAG_logfile = i::Log::kLogToTemporaryFile;
    i::FLAG_logfile_per_isolate = false;
    return false;
  }

  const bool saved_log_;
  const bool saved_prof_;
  FILE* temp_file_;
  const bool trick_to_run_init_flags_;
  v8::HandleScope scope_;
  v8::Handle<v8::Context> env_;
  Logger* logger_;

  DISALLOW_COPY_AND_ASSIGN(ScopedLoggerInitializer);
};

}  // namespace


static const char* StrNStr(const char* s1, const char* s2, int n) {
  if (s1[n] == '\x0') return strstr(s1, s2);
  i::ScopedVector<char> str(n + 1);
  i::StrNCpy(str, s1, static_cast<size_t>(n));
  str[n] = '\x0';
  char* found = strstr(str.start(), s2);
  return found != NULL ? s1 + (found - str.start()) : NULL;
}


// BUG(913). Need to implement support for profiling multiple VM threads.
#if 0

namespace {

class LoopingThread : public v8::internal::Thread {
 public:
  explicit LoopingThread(v8::internal::Isolate* isolate)
      : v8::internal::Thread(isolate),
        semaphore_(new v8::internal::Semaphore(0)),
        run_(true) {
  }

  virtual ~LoopingThread() { delete semaphore_; }

  void Run() {
    self_ = pthread_self();
    RunLoop();
  }

  void SendSigProf() { pthread_kill(self_, SIGPROF); }

  void Stop() { run_ = false; }

  bool WaitForRunning() { return semaphore_->Wait(1000000); }

 protected:
  bool IsRunning() { return run_; }

  virtual void RunLoop() = 0;

  void SetV8ThreadId() {
    v8_thread_id_ = v8::V8::GetCurrentThreadId();
  }

  void SignalRunning() { semaphore_->Signal(); }

 private:
  v8::internal::Semaphore* semaphore_;
  bool run_;
  pthread_t self_;
  int v8_thread_id_;
};


class LoopingJsThread : public LoopingThread {
 public:
  explicit LoopingJsThread(v8::internal::Isolate* isolate)
      : LoopingThread(isolate) { }
  void RunLoop() {
    v8::Locker locker;
    CHECK(CcTest::i_isolate() != NULL);
    CHECK_GT(CcTest::i_isolate()->thread_manager()->CurrentId(), 0);
    SetV8ThreadId();
    while (IsRunning()) {
      v8::HandleScope scope;
      v8::Persistent<v8::Context> context = v8::Context::New();
      CHECK(!context.IsEmpty());
      {
        v8::Context::Scope context_scope(context);
        SignalRunning();
        CompileRun(
            "\x76\x61\x72\x20\x6a\x3b\x20\x66\x6f\x72\x20\x28\x76\x61\x72\x20\x69\x3d\x30\x3b\x20\x69\x3c\x31\x30\x30\x30\x30\x3b\x20\x2b\x2b\x69\x29\x20\x7b\x20\x6a\x20\x3d\x20\x4d\x61\x74\x68\x2e\x73\x69\x6e\x28\x69\x29\x3b\x20\x7d");
      }
      context.Dispose();
      i::OS::Sleep(1);
    }
  }
};


class LoopingNonJsThread : public LoopingThread {
 public:
  explicit LoopingNonJsThread(v8::internal::Isolate* isolate)
      : LoopingThread(isolate) { }
  void RunLoop() {
    v8::Locker locker;
    v8::Unlocker unlocker;
    // Now thread has V8's id, but will not run VM code.
    CHECK(CcTest::i_isolate() != NULL);
    CHECK_GT(CcTest::i_isolate()->thread_manager()->CurrentId(), 0);
    double i = 10;
    SignalRunning();
    while (IsRunning()) {
      i = std::sin(i);
      i::OS::Sleep(1);
    }
  }
};


class TestSampler : public v8::internal::Sampler {
 public:
  explicit TestSampler(v8::internal::Isolate* isolate)
      : Sampler(isolate, 0, true, true),
        semaphore_(new v8::internal::Semaphore(0)),
        was_sample_stack_called_(false) {
  }

  ~TestSampler() { delete semaphore_; }

  void SampleStack(v8::internal::TickSample*) {
    was_sample_stack_called_ = true;
  }

  void Tick(v8::internal::TickSample*) { semaphore_->Signal(); }

  bool WaitForTick() { return semaphore_->Wait(1000000); }

  void Reset() { was_sample_stack_called_ = false; }

  bool WasSampleStackCalled() { return was_sample_stack_called_; }

 private:
  v8::internal::Semaphore* semaphore_;
  bool was_sample_stack_called_;
};


}  // namespace

TEST(ProfMultipleThreads) {
  TestSampler* sampler = NULL;
  {
    v8::Locker locker;
    sampler = new TestSampler(CcTest::i_isolate());
    sampler->Start();
    CHECK(sampler->IsActive());
  }

  LoopingJsThread jsThread(CcTest::i_isolate());
  jsThread.Start();
  LoopingNonJsThread nonJsThread(CcTest::i_isolate());
  nonJsThread.Start();

  CHECK(!sampler->WasSampleStackCalled());
  jsThread.WaitForRunning();
  jsThread.SendSigProf();
  CHECK(sampler->WaitForTick());
  CHECK(sampler->WasSampleStackCalled());
  sampler->Reset();
  CHECK(!sampler->WasSampleStackCalled());
  nonJsThread.WaitForRunning();
  nonJsThread.SendSigProf();
  CHECK(!sampler->WaitForTick());
  CHECK(!sampler->WasSampleStackCalled());
  sampler->Stop();

  jsThread.Stop();
  nonJsThread.Stop();
  jsThread.Join();
  nonJsThread.Join();

  delete sampler;
}

#endif  // __linux__


// Test for issue http://crbug.com/23768 in Chromium.
// Heap can contain scripts with already disposed external sources.
// We need to verify that LogCompiledFunctions doesn't crash on them.
namespace {

class SimpleExternalString : public v8::String::ExternalStringResource {
 public:
  explicit SimpleExternalString(const char* source)
      : utf_source_(StrLength(source)) {
    for (int i = 0; i < utf_source_.length(); ++i)
      utf_source_[i] = source[i];
  }
  virtual ~SimpleExternalString() {}
  virtual size_t length() const { return utf_source_.length(); }
  virtual const uint16_t* data() const { return utf_source_.start(); }
 private:
  i::ScopedVector<uint16_t> utf_source_;
};

}  // namespace

TEST(Issue23768) {
  v8::HandleScope scope(CcTest::isolate());
  v8::Handle<v8::Context> env = v8::Context::New(CcTest::isolate());
  env->Enter();

  SimpleExternalString source_ext_str("\x28\x66\x75\x6e\x63\x74\x69\x6f\x6e\x20\x65\x78\x74\x28\x29\x20\x7b\x7d\x29\x28\x29\x3b");
  v8::Local<v8::String> source =
      v8::String::NewExternal(CcTest::isolate(), &source_ext_str);
  // Script needs to have a name in order to trigger InitLineEnds execution.
  v8::Handle<v8::String> origin =
      v8::String::NewFromUtf8(CcTest::isolate(), "\x69\x73\x73\x75\x65\x2d\x32\x33\x37\x36\x38\x2d\x74\x65\x73\x74");
  v8::Handle<v8::Script> evil_script = CompileWithOrigin(source, origin);
  CHECK(!evil_script.IsEmpty());
  CHECK(!evil_script->Run().IsEmpty());
  i::Handle<i::ExternalTwoByteString> i_source(
      i::ExternalTwoByteString::cast(*v8::Utils::OpenHandle(*source)));
  // This situation can happen if source was an external string disposed
  // by its owner.
  i_source->set_resource(NULL);

  // Must not crash.
  CcTest::i_isolate()->logger()->LogCompiledFunctions();
}


static void ObjMethod1(const v8::FunctionCallbackInfo<v8::Value>& args) {
}


TEST(LogCallbacks) {
  v8::Isolate* isolate = CcTest::isolate();
  ScopedLoggerInitializer initialize_logger;
  Logger* logger = initialize_logger.logger();

  v8::Local<v8::FunctionTemplate> obj =
      v8::Local<v8::FunctionTemplate>::New(isolate,
                                           v8::FunctionTemplate::New(isolate));
  obj->SetClassName(v8_str("\x4f\x62\x6a"));
  v8::Handle<v8::ObjectTemplate> proto = obj->PrototypeTemplate();
  v8::Local<v8::Signature> signature =
      v8::Signature::New(isolate, obj);
  proto->Set(v8_str("\x6d\x65\x74\x68\x6f\x64\x31"),
             v8::FunctionTemplate::New(isolate,
                                       ObjMethod1,
                                       v8::Handle<v8::Value>(),
                                       signature),
             static_cast<v8::PropertyAttribute>(v8::DontDelete));

  initialize_logger.env()->Global()->Set(v8_str("\x4f\x62\x6a"), obj->GetFunction());
  CompileRun("\x4f\x62\x6a\x2e\x70\x72\x6f\x74\x6f\x74\x79\x70\x65\x2e\x6d\x65\x74\x68\x6f\x64\x31\x2e\x74\x6f\x53\x74\x72\x69\x6e\x67\x28\x29\x3b");

  logger->LogCompiledFunctions();

  bool exists = false;
  i::Vector<const char> log(
      i::ReadFile(initialize_logger.StopLoggingGetTempFile(), &exists, true));
  CHECK(exists);

  i::EmbeddedVector<char, 100> ref_data;
  i::SNPrintF(ref_data,
              "\x63\x6f\x64\x65\x2d\x63\x72\x65\x61\x74\x69\x6f\x6e\x2c\x43\x61\x6c\x6c\x62\x61\x63\x6b\x2c\x2d\x32\x2c\x30\x78\x25" V8PRIxPTR "\x2c\x31\x2c\x22\x6d\x65\x74\x68\x6f\x64\x31\x22",
              reinterpret_cast<intptr_t>(ObjMethod1));

  CHECK_NE(NULL, StrNStr(log.start(), ref_data.start(), log.length()));
  log.Dispose();
}


static void Prop1Getter(v8::Local<v8::String> property,
                        const v8::PropertyCallbackInfo<v8::Value>& info) {
}

static void Prop1Setter(v8::Local<v8::String> property,
                        v8::Local<v8::Value> value,
                        const v8::PropertyCallbackInfo<void>& info) {
}

static void Prop2Getter(v8::Local<v8::String> property,
                        const v8::PropertyCallbackInfo<v8::Value>& info) {
}


TEST(LogAccessorCallbacks) {
  v8::Isolate* isolate = CcTest::isolate();
  ScopedLoggerInitializer initialize_logger;
  Logger* logger = initialize_logger.logger();

  v8::Local<v8::FunctionTemplate> obj =
      v8::Local<v8::FunctionTemplate>::New(isolate,
                                           v8::FunctionTemplate::New(isolate));
  obj->SetClassName(v8_str("\x4f\x62\x6a"));
  v8::Handle<v8::ObjectTemplate> inst = obj->InstanceTemplate();
  inst->SetAccessor(v8_str("\x70\x72\x6f\x70\x31"), Prop1Getter, Prop1Setter);
  inst->SetAccessor(v8_str("\x70\x72\x6f\x70\x32"), Prop2Getter);

  logger->LogAccessorCallbacks();

  bool exists = false;
  i::Vector<const char> log(
      i::ReadFile(initialize_logger.StopLoggingGetTempFile(), &exists, true));
  CHECK(exists);

  EmbeddedVector<char, 100> prop1_getter_record;
  i::SNPrintF(prop1_getter_record,
              "\x63\x6f\x64\x65\x2d\x63\x72\x65\x61\x74\x69\x6f\x6e\x2c\x43\x61\x6c\x6c\x62\x61\x63\x6b\x2c\x2d\x32\x2c\x30\x78\x25" V8PRIxPTR "\x2c\x31\x2c\x22\x67\x65\x74\x20\x70\x72\x6f\x70\x31\x22",
              reinterpret_cast<intptr_t>(Prop1Getter));
  CHECK_NE(NULL,
           StrNStr(log.start(), prop1_getter_record.start(), log.length()));

  EmbeddedVector<char, 100> prop1_setter_record;
  i::SNPrintF(prop1_setter_record,
              "\x63\x6f\x64\x65\x2d\x63\x72\x65\x61\x74\x69\x6f\x6e\x2c\x43\x61\x6c\x6c\x62\x61\x63\x6b\x2c\x2d\x32\x2c\x30\x78\x25" V8PRIxPTR "\x2c\x31\x2c\x22\x73\x65\x74\x20\x70\x72\x6f\x70\x31\x22",
              reinterpret_cast<intptr_t>(Prop1Setter));
  CHECK_NE(NULL,
           StrNStr(log.start(), prop1_setter_record.start(), log.length()));

  EmbeddedVector<char, 100> prop2_getter_record;
  i::SNPrintF(prop2_getter_record,
              "\x63\x6f\x64\x65\x2d\x63\x72\x65\x61\x74\x69\x6f\x6e\x2c\x43\x61\x6c\x6c\x62\x61\x63\x6b\x2c\x2d\x32\x2c\x30\x78\x25" V8PRIxPTR "\x2c\x31\x2c\x22\x67\x65\x74\x20\x70\x72\x6f\x70\x32\x22",
              reinterpret_cast<intptr_t>(Prop2Getter));
  CHECK_NE(NULL,
           StrNStr(log.start(), prop2_getter_record.start(), log.length()));
  log.Dispose();
}


typedef i::NativesCollection<i::TEST> TestSources;


// Test that logging of code create / move events is equivalent to traversal of
// a resulting heap.
TEST(EquivalenceOfLoggingAndTraversal) {
  // This test needs to be run on a "clean" V8 to ensure that snapshot log
  // is loaded. This is always true when running using tools/test.py because
  // it launches a new cctest instance for every test. To be sure that launching
  // cctest manually also works, please be sure that no tests below
  // are using V8.

  // Start with profiling to capture all code events from the beginning.
  ScopedLoggerInitializer initialize_logger;
  Logger* logger = initialize_logger.logger();

  // Compile and run a function that creates other functions.
  CompileRun(
      "\x28\x66\x75\x6e\x63\x74\x69\x6f\x6e\x20\x66\x28\x6f\x62\x6a\x29\x20\x7b\xa"
      "\x20\x20\x6f\x62\x6a\x2e\x74\x65\x73\x74\x20\x3d\xa"
      "\x20\x20\x20\x20\x28\x66\x75\x6e\x63\x74\x69\x6f\x6e\x20\x61\x28\x6a\x29\x20\x7b\x20\x72\x65\x74\x75\x72\x6e\x20\x66\x75\x6e\x63\x74\x69\x6f\x6e\x20\x62\x28\x29\x20\x7b\x20\x72\x65\x74\x75\x72\x6e\x20\x6a\x3b\x20\x7d\x20\x7d\x29\x28\x31\x30\x30\x29\x3b\xa"
      "\x7d\x29\x28\x74\x68\x69\x73\x29\x3b");
  logger->StopProfiler();
  CcTest::heap()->CollectAllGarbage(i::Heap::kMakeHeapIterableMask);
  logger->StringEvent("\x74\x65\x73\x74\x2d\x6c\x6f\x67\x67\x69\x6e\x67\x2d\x64\x6f\x6e\x65", "");

  // Iterate heap to find compiled functions, will write to log.
  logger->LogCompiledFunctions();
  logger->StringEvent("\x74\x65\x73\x74\x2d\x74\x72\x61\x76\x65\x72\x73\x61\x6c\x2d\x64\x6f\x6e\x65", "");

  bool exists = false;
  i::Vector<const char> log(
      i::ReadFile(initialize_logger.StopLoggingGetTempFile(), &exists, true));
  CHECK(exists);
  v8::Handle<v8::String> log_str = v8::String::NewFromUtf8(
      CcTest::isolate(), log.start(), v8::String::kNormalString, log.length());
  initialize_logger.env()->Global()->Set(v8_str("\x5f\x6c\x6f\x67"), log_str);

  i::Vector<const unsigned char> source = TestSources::GetScriptsSource();
  v8::Handle<v8::String> source_str = v8::String::NewFromUtf8(
      CcTest::isolate(), reinterpret_cast<const char*>(source.start()),
      v8::String::kNormalString, source.length());
  v8::TryCatch try_catch;
  v8::Handle<v8::Script> script = CompileWithOrigin(source_str, "");
  if (script.IsEmpty()) {
    v8::String::Utf8Value exception(try_catch.Exception());
    printf("\x63\x6f\x6d\x70\x69\x6c\x65\x3a\x20\x6c\xa2\xa", *exception);
    CHECK(false);
  }
  v8::Handle<v8::Value> result = script->Run();
  if (result.IsEmpty()) {
    v8::String::Utf8Value exception(try_catch.Exception());
    printf("\x72\x75\x6e\x3a\x20\x6c\xa2\xa", *exception);
    CHECK(false);
  }
  // The result either be a "true" literal or problem description.
  if (!result->IsTrue()) {
    v8::Local<v8::String> s = result->ToString();
    i::ScopedVector<char> data(s->Utf8Length() + 1);
    CHECK_NE(NULL, data.start());
    s->WriteUtf8(data.start());
    printf("\x6c\xa2\xa", data.start());
    // Make sure that our output is written prior crash due to CHECK failure.
    fflush(stdout);
    CHECK(false);
  }
}
