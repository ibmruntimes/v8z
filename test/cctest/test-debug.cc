// Copyright 2012 the V8 project authors. All rights reserved.
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

#include <stdlib.h>

#include "src/v8.h"

#include "src/api.h"
#include "src/base/platform/condition-variable.h"
#include "src/base/platform/platform.h"
#include "src/compilation-cache.h"
#include "src/debug.h"
#include "src/deoptimizer.h"
#include "src/frames.h"
#include "src/stub-cache.h"
#include "src/utils.h"
#include "test/cctest/cctest.h"


using ::v8::base::Mutex;
using ::v8::base::LockGuard;
using ::v8::base::ConditionVariable;
using ::v8::base::OS;
using ::v8::base::Semaphore;
using ::v8::internal::EmbeddedVector;
using ::v8::internal::Object;
using ::v8::internal::Handle;
using ::v8::internal::Heap;
using ::v8::internal::JSGlobalProxy;
using ::v8::internal::Code;
using ::v8::internal::Debug;
using ::v8::internal::Debugger;
using ::v8::internal::CommandMessage;
using ::v8::internal::CommandMessageQueue;
using ::v8::internal::StackFrame;
using ::v8::internal::StepAction;
using ::v8::internal::StepIn;  // From StepAction enum
using ::v8::internal::StepNext;  // From StepAction enum
using ::v8::internal::StepOut;  // From StepAction enum
using ::v8::internal::Vector;
using ::v8::internal::StrLength;

// Size of temp buffer for formatting small strings.
#define SMALL_STRING_BUFFER_SIZE 80

// --- H e l p e r   C l a s s e s


// Helper class for creating a V8 enviromnent for running tests
class DebugLocalContext {
 public:
  inline DebugLocalContext(
      v8::ExtensionConfiguration* extensions = 0,
      v8::Handle<v8::ObjectTemplate> global_template =
          v8::Handle<v8::ObjectTemplate>(),
      v8::Handle<v8::Value> global_object = v8::Handle<v8::Value>())
      : scope_(CcTest::isolate()),
        context_(
          v8::Context::New(CcTest::isolate(),
                           extensions,
                           global_template,
                           global_object)) {
    context_->Enter();
  }
  inline ~DebugLocalContext() {
    context_->Exit();
  }
  inline v8::Local<v8::Context> context() { return context_; }
  inline v8::Context* operator->() { return *context_; }
  inline v8::Context* operator*() { return *context_; }
  inline v8::Isolate* GetIsolate() { return context_->GetIsolate(); }
  inline bool IsReady() { return !context_.IsEmpty(); }
  void ExposeDebug() {
    v8::internal::Isolate* isolate =
        reinterpret_cast<v8::internal::Isolate*>(context_->GetIsolate());
    v8::internal::Factory* factory = isolate->factory();
    // Expose the debug context global object in the global object for testing.
    CHECK(isolate->debug()->Load());
    Handle<v8::internal::Context> debug_context =
        isolate->debug()->debug_context();
    debug_context->set_security_token(
        v8::Utils::OpenHandle(*context_)->security_token());

    Handle<JSGlobalProxy> global(Handle<JSGlobalProxy>::cast(
        v8::Utils::OpenHandle(*context_->Global())));
    Handle<v8::internal::String> debug_string =
        factory->InternalizeOneByteString(STATIC_ASCII_VECTOR("\x64\x65\x62\x75\x67"));
    v8::internal::Runtime::DefineObjectProperty(global, debug_string,
        handle(debug_context->global_proxy(), isolate), DONT_ENUM).Check();
  }

 private:
  v8::HandleScope scope_;
  v8::Local<v8::Context> context_;
};


// --- H e l p e r   F u n c t i o n s


// Compile and run the supplied source and return the fequested function.
static v8::Local<v8::Function> CompileFunction(DebugLocalContext* env,
                                               const char* source,
                                               const char* function_name) {
  v8::Script::Compile(v8::String::NewFromUtf8(env->GetIsolate(), source))
      ->Run();
  return v8::Local<v8::Function>::Cast((*env)->Global()->Get(
      v8::String::NewFromUtf8(env->GetIsolate(), function_name)));
}


// Compile and run the supplied source and return the requested function.
static v8::Local<v8::Function> CompileFunction(v8::Isolate* isolate,
                                               const char* source,
                                               const char* function_name) {
  v8::Script::Compile(v8::String::NewFromUtf8(isolate, source))->Run();
  v8::Local<v8::Object> global =
      CcTest::isolate()->GetCurrentContext()->Global();
  return v8::Local<v8::Function>::Cast(
      global->Get(v8::String::NewFromUtf8(isolate, function_name)));
}


// Is there any debug info for the function?
static bool HasDebugInfo(v8::Handle<v8::Function> fun) {
  Handle<v8::internal::JSFunction> f = v8::Utils::OpenHandle(*fun);
  Handle<v8::internal::SharedFunctionInfo> shared(f->shared());
  return Debug::HasDebugInfo(shared);
}


// Set a break point in a function and return the associated break point
// number.
static int SetBreakPoint(Handle<v8::internal::JSFunction> fun, int position) {
  static int break_point = 0;
  v8::internal::Isolate* isolate = fun->GetIsolate();
  v8::internal::Debug* debug = isolate->debug();
  debug->SetBreakPoint(
      fun,
      Handle<Object>(v8::internal::Smi::FromInt(++break_point), isolate),
      &position);
  return break_point;
}


// Set a break point in a function and return the associated break point
// number.
static int SetBreakPoint(v8::Handle<v8::Function> fun, int position) {
  return SetBreakPoint(v8::Utils::OpenHandle(*fun), position);
}


// Set a break point in a function using the Debug object and return the
// associated break point number.
static int SetBreakPointFromJS(v8::Isolate* isolate,
                               const char* function_name,
                               int line, int position) {
  EmbeddedVector<char, SMALL_STRING_BUFFER_SIZE> buffer;
  SNPrintF(buffer,
           "\x64\x65\x62\x75\x67\x2e\x44\x65\x62\x75\x67\x2e\x73\x65\x74\x42\x72\x65\x61\x6b\x50\x6f\x69\x6e\x74\x28\x6c\xa2\x2c\x6c\x84\x2c\x6c\x84\x29",
           function_name, line, position);
  buffer[SMALL_STRING_BUFFER_SIZE - 1] = '\x0';
  v8::Handle<v8::String> str = v8::String::NewFromUtf8(isolate, buffer.start());
  return v8::Script::Compile(str)->Run()->Int32Value();
}


// Set a break point in a script identified by id using the global Debug object.
static int SetScriptBreakPointByIdFromJS(v8::Isolate* isolate, int script_id,
                                         int line, int column) {
  EmbeddedVector<char, SMALL_STRING_BUFFER_SIZE> buffer;
  if (column >= 0) {
    // Column specified set script break point on precise location.
    SNPrintF(buffer,
             "\x64\x65\x62\x75\x67\x2e\x44\x65\x62\x75\x67\x2e\x73\x65\x74\x53\x63\x72\x69\x70\x74\x42\x72\x65\x61\x6b\x50\x6f\x69\x6e\x74\x42\x79\x49\x64\x28\x6c\x84\x2c\x6c\x84\x2c\x6c\x84\x29",
             script_id, line, column);
  } else {
    // Column not specified set script break point on line.
    SNPrintF(buffer,
             "\x64\x65\x62\x75\x67\x2e\x44\x65\x62\x75\x67\x2e\x73\x65\x74\x53\x63\x72\x69\x70\x74\x42\x72\x65\x61\x6b\x50\x6f\x69\x6e\x74\x42\x79\x49\x64\x28\x6c\x84\x2c\x6c\x84\x29",
             script_id, line);
  }
  buffer[SMALL_STRING_BUFFER_SIZE - 1] = '\x0';
  {
    v8::TryCatch try_catch;
    v8::Handle<v8::String> str =
        v8::String::NewFromUtf8(isolate, buffer.start());
    v8::Handle<v8::Value> value = v8::Script::Compile(str)->Run();
    CHECK(!try_catch.HasCaught());
    return value->Int32Value();
  }
}


// Set a break point in a script identified by name using the global Debug
// object.
static int SetScriptBreakPointByNameFromJS(v8::Isolate* isolate,
                                           const char* script_name, int line,
                                           int column) {
  EmbeddedVector<char, SMALL_STRING_BUFFER_SIZE> buffer;
  if (column >= 0) {
    // Column specified set script break point on precise location.
    SNPrintF(buffer,
             "\x64\x65\x62\x75\x67\x2e\x44\x65\x62\x75\x67\x2e\x73\x65\x74\x53\x63\x72\x69\x70\x74\x42\x72\x65\x61\x6b\x50\x6f\x69\x6e\x74\x42\x79\x4e\x61\x6d\x65\x28\x22\x6c\xa2\x22\x2c\x6c\x84\x2c\x6c\x84\x29",
             script_name, line, column);
  } else {
    // Column not specified set script break point on line.
    SNPrintF(buffer,
             "\x64\x65\x62\x75\x67\x2e\x44\x65\x62\x75\x67\x2e\x73\x65\x74\x53\x63\x72\x69\x70\x74\x42\x72\x65\x61\x6b\x50\x6f\x69\x6e\x74\x42\x79\x4e\x61\x6d\x65\x28\x22\x6c\xa2\x22\x2c\x6c\x84\x29",
             script_name, line);
  }
  buffer[SMALL_STRING_BUFFER_SIZE - 1] = '\x0';
  {
    v8::TryCatch try_catch;
    v8::Handle<v8::String> str =
        v8::String::NewFromUtf8(isolate, buffer.start());
    v8::Handle<v8::Value> value = v8::Script::Compile(str)->Run();
    CHECK(!try_catch.HasCaught());
    return value->Int32Value();
  }
}


// Clear a break point.
static void ClearBreakPoint(int break_point) {
  v8::internal::Isolate* isolate = CcTest::i_isolate();
  v8::internal::Debug* debug = isolate->debug();
  debug->ClearBreakPoint(
      Handle<Object>(v8::internal::Smi::FromInt(break_point), isolate));
}


// Clear a break point using the global Debug object.
static void ClearBreakPointFromJS(v8::Isolate* isolate,
                                  int break_point_number) {
  EmbeddedVector<char, SMALL_STRING_BUFFER_SIZE> buffer;
  SNPrintF(buffer,
           "\x64\x65\x62\x75\x67\x2e\x44\x65\x62\x75\x67\x2e\x63\x6c\x65\x61\x72\x42\x72\x65\x61\x6b\x50\x6f\x69\x6e\x74\x28\x6c\x84\x29",
           break_point_number);
  buffer[SMALL_STRING_BUFFER_SIZE - 1] = '\x0';
  v8::Script::Compile(v8::String::NewFromUtf8(isolate, buffer.start()))->Run();
}


static void EnableScriptBreakPointFromJS(v8::Isolate* isolate,
                                         int break_point_number) {
  EmbeddedVector<char, SMALL_STRING_BUFFER_SIZE> buffer;
  SNPrintF(buffer,
           "\x64\x65\x62\x75\x67\x2e\x44\x65\x62\x75\x67\x2e\x65\x6e\x61\x62\x6c\x65\x53\x63\x72\x69\x70\x74\x42\x72\x65\x61\x6b\x50\x6f\x69\x6e\x74\x28\x6c\x84\x29",
           break_point_number);
  buffer[SMALL_STRING_BUFFER_SIZE - 1] = '\x0';
  v8::Script::Compile(v8::String::NewFromUtf8(isolate, buffer.start()))->Run();
}


static void DisableScriptBreakPointFromJS(v8::Isolate* isolate,
                                          int break_point_number) {
  EmbeddedVector<char, SMALL_STRING_BUFFER_SIZE> buffer;
  SNPrintF(buffer,
           "\x64\x65\x62\x75\x67\x2e\x44\x65\x62\x75\x67\x2e\x64\x69\x73\x61\x62\x6c\x65\x53\x63\x72\x69\x70\x74\x42\x72\x65\x61\x6b\x50\x6f\x69\x6e\x74\x28\x6c\x84\x29",
           break_point_number);
  buffer[SMALL_STRING_BUFFER_SIZE - 1] = '\x0';
  v8::Script::Compile(v8::String::NewFromUtf8(isolate, buffer.start()))->Run();
}


static void ChangeScriptBreakPointConditionFromJS(v8::Isolate* isolate,
                                                  int break_point_number,
                                                  const char* condition) {
  EmbeddedVector<char, SMALL_STRING_BUFFER_SIZE> buffer;
  SNPrintF(buffer,
           "\x64\x65\x62\x75\x67\x2e\x44\x65\x62\x75\x67\x2e\x63\x68\x61\x6e\x67\x65\x53\x63\x72\x69\x70\x74\x42\x72\x65\x61\x6b\x50\x6f\x69\x6e\x74\x43\x6f\x6e\x64\x69\x74\x69\x6f\x6e\x28\x6c\x84\x2c\x20\x22\x6c\xa2\x22\x29",
           break_point_number, condition);
  buffer[SMALL_STRING_BUFFER_SIZE - 1] = '\x0';
  v8::Script::Compile(v8::String::NewFromUtf8(isolate, buffer.start()))->Run();
}


static void ChangeScriptBreakPointIgnoreCountFromJS(v8::Isolate* isolate,
                                                    int break_point_number,
                                                    int ignoreCount) {
  EmbeddedVector<char, SMALL_STRING_BUFFER_SIZE> buffer;
  SNPrintF(buffer,
           "\x64\x65\x62\x75\x67\x2e\x44\x65\x62\x75\x67\x2e\x63\x68\x61\x6e\x67\x65\x53\x63\x72\x69\x70\x74\x42\x72\x65\x61\x6b\x50\x6f\x69\x6e\x74\x49\x67\x6e\x6f\x72\x65\x43\x6f\x75\x6e\x74\x28\x6c\x84\x2c\x20\x6c\x84\x29",
           break_point_number, ignoreCount);
  buffer[SMALL_STRING_BUFFER_SIZE - 1] = '\x0';
  v8::Script::Compile(v8::String::NewFromUtf8(isolate, buffer.start()))->Run();
}


// Change break on exception.
static void ChangeBreakOnException(bool caught, bool uncaught) {
  v8::internal::Debug* debug = CcTest::i_isolate()->debug();
  debug->ChangeBreakOnException(v8::internal::BreakException, caught);
  debug->ChangeBreakOnException(v8::internal::BreakUncaughtException, uncaught);
}


// Change break on exception using the global Debug object.
static void ChangeBreakOnExceptionFromJS(v8::Isolate* isolate, bool caught,
                                         bool uncaught) {
  if (caught) {
    v8::Script::Compile(
        v8::String::NewFromUtf8(isolate, "\x64\x65\x62\x75\x67\x2e\x44\x65\x62\x75\x67\x2e\x73\x65\x74\x42\x72\x65\x61\x6b\x4f\x6e\x45\x78\x63\x65\x70\x74\x69\x6f\x6e\x28\x29"))
        ->Run();
  } else {
    v8::Script::Compile(
        v8::String::NewFromUtf8(isolate, "\x64\x65\x62\x75\x67\x2e\x44\x65\x62\x75\x67\x2e\x63\x6c\x65\x61\x72\x42\x72\x65\x61\x6b\x4f\x6e\x45\x78\x63\x65\x70\x74\x69\x6f\x6e\x28\x29"))
        ->Run();
  }
  if (uncaught) {
    v8::Script::Compile(
        v8::String::NewFromUtf8(
            isolate, "\x64\x65\x62\x75\x67\x2e\x44\x65\x62\x75\x67\x2e\x73\x65\x74\x42\x72\x65\x61\x6b\x4f\x6e\x55\x6e\x63\x61\x75\x67\x68\x74\x45\x78\x63\x65\x70\x74\x69\x6f\x6e\x28\x29"))->Run();
  } else {
    v8::Script::Compile(
        v8::String::NewFromUtf8(
            isolate, "\x64\x65\x62\x75\x67\x2e\x44\x65\x62\x75\x67\x2e\x63\x6c\x65\x61\x72\x42\x72\x65\x61\x6b\x4f\x6e\x55\x6e\x63\x61\x75\x67\x68\x74\x45\x78\x63\x65\x70\x74\x69\x6f\x6e\x28\x29"))->Run();
  }
}


// Prepare to step to next break location.
static void PrepareStep(StepAction step_action) {
  v8::internal::Debug* debug = CcTest::i_isolate()->debug();
  debug->PrepareStep(step_action, 1, StackFrame::NO_ID);
}


// This function is in namespace v8::internal to be friend with class
// v8::internal::Debug.
namespace v8 {
namespace internal {

// Collect the currently debugged functions.
Handle<FixedArray> GetDebuggedFunctions() {
  Debug* debug = CcTest::i_isolate()->debug();

  v8::internal::DebugInfoListNode* node = debug->debug_info_list_;

  // Find the number of debugged functions.
  int count = 0;
  while (node) {
    count++;
    node = node->next();
  }

  // Allocate array for the debugged functions
  Handle<FixedArray> debugged_functions =
      CcTest::i_isolate()->factory()->NewFixedArray(count);

  // Run through the debug info objects and collect all functions.
  count = 0;
  while (node) {
    debugged_functions->set(count++, *node->debug_info());
    node = node->next();
  }

  return debugged_functions;
}


// Check that the debugger has been fully unloaded.
void CheckDebuggerUnloaded(bool check_functions) {
  // Check that the debugger context is cleared and that there is no debug
  // information stored for the debugger.
  CHECK(CcTest::i_isolate()->debug()->debug_context().is_null());
  CHECK_EQ(NULL, CcTest::i_isolate()->debug()->debug_info_list_);

  // Collect garbage to ensure weak handles are cleared.
  CcTest::heap()->CollectAllGarbage(Heap::kNoGCFlags);
  CcTest::heap()->CollectAllGarbage(Heap::kMakeHeapIterableMask);

  // Iterate the head and check that there are no debugger related objects left.
  HeapIterator iterator(CcTest::heap());
  for (HeapObject* obj = iterator.next(); obj != NULL; obj = iterator.next()) {
    CHECK(!obj->IsDebugInfo());
    CHECK(!obj->IsBreakPointInfo());

    // If deep check of functions is requested check that no debug break code
    // is left in all functions.
    if (check_functions) {
      if (obj->IsJSFunction()) {
        JSFunction* fun = JSFunction::cast(obj);
        for (RelocIterator it(fun->shared()->code()); !it.done(); it.next()) {
          RelocInfo::Mode rmode = it.rinfo()->rmode();
          if (RelocInfo::IsCodeTarget(rmode)) {
            CHECK(!Debug::IsDebugBreak(it.rinfo()->target_address()));
          } else if (RelocInfo::IsJSReturn(rmode)) {
            CHECK(!Debug::IsDebugBreakAtReturn(it.rinfo()));
          }
        }
      }
    }
  }
}


} }  // namespace v8::internal


// Check that the debugger has been fully unloaded.
static void CheckDebuggerUnloaded(bool check_functions = false) {
  // Let debugger to unload itself synchronously
  v8::Debug::ProcessDebugMessages();

  v8::internal::CheckDebuggerUnloaded(check_functions);
}


// Inherit from BreakLocationIterator to get access to protected parts for
// testing.
class TestBreakLocationIterator: public v8::internal::BreakLocationIterator {
 public:
  explicit TestBreakLocationIterator(Handle<v8::internal::DebugInfo> debug_info)
    : BreakLocationIterator(debug_info, v8::internal::SOURCE_BREAK_LOCATIONS) {}
  v8::internal::RelocIterator* it() { return reloc_iterator_; }
  v8::internal::RelocIterator* it_original() {
    return reloc_iterator_original_;
  }
};


// Compile a function, set a break point and check that the call at the break
// location in the code is the expected debug_break function.
void CheckDebugBreakFunction(DebugLocalContext* env,
                             const char* source, const char* name,
                             int position, v8::internal::RelocInfo::Mode mode,
                             Code* debug_break) {
  v8::internal::Debug* debug = CcTest::i_isolate()->debug();

  // Create function and set the break point.
  Handle<v8::internal::JSFunction> fun = v8::Utils::OpenHandle(
      *CompileFunction(env, source, name));
  int bp = SetBreakPoint(fun, position);

  // Check that the debug break function is as expected.
  Handle<v8::internal::SharedFunctionInfo> shared(fun->shared());
  CHECK(Debug::HasDebugInfo(shared));
  TestBreakLocationIterator it1(Debug::GetDebugInfo(shared));
  it1.FindBreakLocationFromPosition(position, v8::internal::STATEMENT_ALIGNED);
  v8::internal::RelocInfo::Mode actual_mode = it1.it()->rinfo()->rmode();
  if (actual_mode == v8::internal::RelocInfo::CODE_TARGET_WITH_ID) {
    actual_mode = v8::internal::RelocInfo::CODE_TARGET;
  }
  CHECK_EQ(mode, actual_mode);
  if (mode != v8::internal::RelocInfo::JS_RETURN) {
    CHECK_EQ(debug_break,
        Code::GetCodeFromTargetAddress(it1.it()->rinfo()->target_address()));
  } else {
    CHECK(Debug::IsDebugBreakAtReturn(it1.it()->rinfo()));
  }

  // Clear the break point and check that the debug break function is no longer
  // there
  ClearBreakPoint(bp);
  CHECK(!debug->HasDebugInfo(shared));
  CHECK(debug->EnsureDebugInfo(shared, fun));
  TestBreakLocationIterator it2(Debug::GetDebugInfo(shared));
  it2.FindBreakLocationFromPosition(position, v8::internal::STATEMENT_ALIGNED);
  actual_mode = it2.it()->rinfo()->rmode();
  if (actual_mode == v8::internal::RelocInfo::CODE_TARGET_WITH_ID) {
    actual_mode = v8::internal::RelocInfo::CODE_TARGET;
  }
  CHECK_EQ(mode, actual_mode);
  if (mode == v8::internal::RelocInfo::JS_RETURN) {
    CHECK(!Debug::IsDebugBreakAtReturn(it2.it()->rinfo()));
  }
}


// --- D e b u g   E v e n t   H a n d l e r s
// ---
// --- The different tests uses a number of debug event handlers.
// ---


// Source for the JavaScript function which picks out the function
// name of a frame.
const char* frame_function_name_source =
    "\x66\x75\x6e\x63\x74\x69\x6f\x6e\x20\x66\x72\x61\x6d\x65\x5f\x66\x75\x6e\x63\x74\x69\x6f\x6e\x5f\x6e\x61\x6d\x65\x28\x65\x78\x65\x63\x5f\x73\x74\x61\x74\x65\x2c\x20\x66\x72\x61\x6d\x65\x5f\x6e\x75\x6d\x62\x65\x72\x29\x20\x7b"
    "\x20\x20\x72\x65\x74\x75\x72\x6e\x20\x65\x78\x65\x63\x5f\x73\x74\x61\x74\x65\x2e\x66\x72\x61\x6d\x65\x28\x66\x72\x61\x6d\x65\x5f\x6e\x75\x6d\x62\x65\x72\x29\x2e\x66\x75\x6e\x63\x28\x29\x2e\x6e\x61\x6d\x65\x28\x29\x3b"
    "\x7d";
v8::Local<v8::Function> frame_function_name;


// Source for the JavaScript function which pick out the name of the
// first argument of a frame.
const char* frame_argument_name_source =
    "\x66\x75\x6e\x63\x74\x69\x6f\x6e\x20\x66\x72\x61\x6d\x65\x5f\x61\x72\x67\x75\x6d\x65\x6e\x74\x5f\x6e\x61\x6d\x65\x28\x65\x78\x65\x63\x5f\x73\x74\x61\x74\x65\x2c\x20\x66\x72\x61\x6d\x65\x5f\x6e\x75\x6d\x62\x65\x72\x29\x20\x7b"
    "\x20\x20\x72\x65\x74\x75\x72\x6e\x20\x65\x78\x65\x63\x5f\x73\x74\x61\x74\x65\x2e\x66\x72\x61\x6d\x65\x28\x66\x72\x61\x6d\x65\x5f\x6e\x75\x6d\x62\x65\x72\x29\x2e\x61\x72\x67\x75\x6d\x65\x6e\x74\x4e\x61\x6d\x65\x28\x30\x29\x3b"
    "\x7d";
v8::Local<v8::Function> frame_argument_name;


// Source for the JavaScript function which pick out the value of the
// first argument of a frame.
const char* frame_argument_value_source =
    "\x66\x75\x6e\x63\x74\x69\x6f\x6e\x20\x66\x72\x61\x6d\x65\x5f\x61\x72\x67\x75\x6d\x65\x6e\x74\x5f\x76\x61\x6c\x75\x65\x28\x65\x78\x65\x63\x5f\x73\x74\x61\x74\x65\x2c\x20\x66\x72\x61\x6d\x65\x5f\x6e\x75\x6d\x62\x65\x72\x29\x20\x7b"
    "\x20\x20\x72\x65\x74\x75\x72\x6e\x20\x65\x78\x65\x63\x5f\x73\x74\x61\x74\x65\x2e\x66\x72\x61\x6d\x65\x28\x66\x72\x61\x6d\x65\x5f\x6e\x75\x6d\x62\x65\x72\x29\x2e\x61\x72\x67\x75\x6d\x65\x6e\x74\x56\x61\x6c\x75\x65\x28\x30\x29\x2e\x76\x61\x6c\x75\x65\x5f\x3b"
    "\x7d";
v8::Local<v8::Function> frame_argument_value;


// Source for the JavaScript function which pick out the name of the
// first argument of a frame.
const char* frame_local_name_source =
    "\x66\x75\x6e\x63\x74\x69\x6f\x6e\x20\x66\x72\x61\x6d\x65\x5f\x6c\x6f\x63\x61\x6c\x5f\x6e\x61\x6d\x65\x28\x65\x78\x65\x63\x5f\x73\x74\x61\x74\x65\x2c\x20\x66\x72\x61\x6d\x65\x5f\x6e\x75\x6d\x62\x65\x72\x29\x20\x7b"
    "\x20\x20\x72\x65\x74\x75\x72\x6e\x20\x65\x78\x65\x63\x5f\x73\x74\x61\x74\x65\x2e\x66\x72\x61\x6d\x65\x28\x66\x72\x61\x6d\x65\x5f\x6e\x75\x6d\x62\x65\x72\x29\x2e\x6c\x6f\x63\x61\x6c\x4e\x61\x6d\x65\x28\x30\x29\x3b"
    "\x7d";
v8::Local<v8::Function> frame_local_name;


// Source for the JavaScript function which pick out the value of the
// first argument of a frame.
const char* frame_local_value_source =
    "\x66\x75\x6e\x63\x74\x69\x6f\x6e\x20\x66\x72\x61\x6d\x65\x5f\x6c\x6f\x63\x61\x6c\x5f\x76\x61\x6c\x75\x65\x28\x65\x78\x65\x63\x5f\x73\x74\x61\x74\x65\x2c\x20\x66\x72\x61\x6d\x65\x5f\x6e\x75\x6d\x62\x65\x72\x29\x20\x7b"
    "\x20\x20\x72\x65\x74\x75\x72\x6e\x20\x65\x78\x65\x63\x5f\x73\x74\x61\x74\x65\x2e\x66\x72\x61\x6d\x65\x28\x66\x72\x61\x6d\x65\x5f\x6e\x75\x6d\x62\x65\x72\x29\x2e\x6c\x6f\x63\x61\x6c\x56\x61\x6c\x75\x65\x28\x30\x29\x2e\x76\x61\x6c\x75\x65\x5f\x3b"
    "\x7d";
v8::Local<v8::Function> frame_local_value;


// Source for the JavaScript function which picks out the source line for the
// top frame.
const char* frame_source_line_source =
    "\x66\x75\x6e\x63\x74\x69\x6f\x6e\x20\x66\x72\x61\x6d\x65\x5f\x73\x6f\x75\x72\x63\x65\x5f\x6c\x69\x6e\x65\x28\x65\x78\x65\x63\x5f\x73\x74\x61\x74\x65\x29\x20\x7b"
    "\x20\x20\x72\x65\x74\x75\x72\x6e\x20\x65\x78\x65\x63\x5f\x73\x74\x61\x74\x65\x2e\x66\x72\x61\x6d\x65\x28\x30\x29\x2e\x73\x6f\x75\x72\x63\x65\x4c\x69\x6e\x65\x28\x29\x3b"
    "\x7d";
v8::Local<v8::Function> frame_source_line;


// Source for the JavaScript function which picks out the source column for the
// top frame.
const char* frame_source_column_source =
    "\x66\x75\x6e\x63\x74\x69\x6f\x6e\x20\x66\x72\x61\x6d\x65\x5f\x73\x6f\x75\x72\x63\x65\x5f\x63\x6f\x6c\x75\x6d\x6e\x28\x65\x78\x65\x63\x5f\x73\x74\x61\x74\x65\x29\x20\x7b"
    "\x20\x20\x72\x65\x74\x75\x72\x6e\x20\x65\x78\x65\x63\x5f\x73\x74\x61\x74\x65\x2e\x66\x72\x61\x6d\x65\x28\x30\x29\x2e\x73\x6f\x75\x72\x63\x65\x43\x6f\x6c\x75\x6d\x6e\x28\x29\x3b"
    "\x7d";
v8::Local<v8::Function> frame_source_column;


// Source for the JavaScript function which picks out the script name for the
// top frame.
const char* frame_script_name_source =
    "\x66\x75\x6e\x63\x74\x69\x6f\x6e\x20\x66\x72\x61\x6d\x65\x5f\x73\x63\x72\x69\x70\x74\x5f\x6e\x61\x6d\x65\x28\x65\x78\x65\x63\x5f\x73\x74\x61\x74\x65\x29\x20\x7b"
    "\x20\x20\x72\x65\x74\x75\x72\x6e\x20\x65\x78\x65\x63\x5f\x73\x74\x61\x74\x65\x2e\x66\x72\x61\x6d\x65\x28\x30\x29\x2e\x66\x75\x6e\x63\x28\x29\x2e\x73\x63\x72\x69\x70\x74\x28\x29\x2e\x6e\x61\x6d\x65\x28\x29\x3b"
    "\x7d";
v8::Local<v8::Function> frame_script_name;


// Source for the JavaScript function which returns the number of frames.
static const char* frame_count_source =
    "\x66\x75\x6e\x63\x74\x69\x6f\x6e\x20\x66\x72\x61\x6d\x65\x5f\x63\x6f\x75\x6e\x74\x28\x65\x78\x65\x63\x5f\x73\x74\x61\x74\x65\x29\x20\x7b"
    "\x20\x20\x72\x65\x74\x75\x72\x6e\x20\x65\x78\x65\x63\x5f\x73\x74\x61\x74\x65\x2e\x66\x72\x61\x6d\x65\x43\x6f\x75\x6e\x74\x28\x29\x3b"
    "\x7d";
v8::Handle<v8::Function> frame_count;


// Global variable to store the last function hit - used by some tests.
char last_function_hit[80];

// Global variable to store the name for last script hit - used by some tests.
char last_script_name_hit[80];

// Global variables to store the last source position - used by some tests.
int last_source_line = -1;
int last_source_column = -1;

// Debug event handler which counts the break points which have been hit.
int break_point_hit_count = 0;
int break_point_hit_count_deoptimize = 0;
static void DebugEventBreakPointHitCount(
    const v8::Debug::EventDetails& event_details) {
  v8::DebugEvent event = event_details.GetEvent();
  v8::Handle<v8::Object> exec_state = event_details.GetExecutionState();
  v8::internal::Isolate* isolate = CcTest::i_isolate();
  Debug* debug = isolate->debug();
  // When hitting a debug event listener there must be a break set.
  CHECK_NE(debug->break_id(), 0);

  // Count the number of breaks.
  if (event == v8::Break) {
    break_point_hit_count++;
    if (!frame_function_name.IsEmpty()) {
      // Get the name of the function.
      const int argc = 2;
      v8::Handle<v8::Value> argv[argc] = {
        exec_state, v8::Integer::New(CcTest::isolate(), 0)
      };
      v8::Handle<v8::Value> result = frame_function_name->Call(exec_state,
                                                               argc, argv);
      if (result->IsUndefined()) {
        last_function_hit[0] = '\x0';
      } else {
        CHECK(result->IsString());
        v8::Handle<v8::String> function_name(result->ToString());
        function_name->WriteUtf8(last_function_hit);
      }
    }

    if (!frame_source_line.IsEmpty()) {
      // Get the source line.
      const int argc = 1;
      v8::Handle<v8::Value> argv[argc] = { exec_state };
      v8::Handle<v8::Value> result = frame_source_line->Call(exec_state,
                                                             argc, argv);
      CHECK(result->IsNumber());
      last_source_line = result->Int32Value();
    }

    if (!frame_source_column.IsEmpty()) {
      // Get the source column.
      const int argc = 1;
      v8::Handle<v8::Value> argv[argc] = { exec_state };
      v8::Handle<v8::Value> result = frame_source_column->Call(exec_state,
                                                               argc, argv);
      CHECK(result->IsNumber());
      last_source_column = result->Int32Value();
    }

    if (!frame_script_name.IsEmpty()) {
      // Get the script name of the function script.
      const int argc = 1;
      v8::Handle<v8::Value> argv[argc] = { exec_state };
      v8::Handle<v8::Value> result = frame_script_name->Call(exec_state,
                                                             argc, argv);
      if (result->IsUndefined()) {
        last_script_name_hit[0] = '\x0';
      } else {
        CHECK(result->IsString());
        v8::Handle<v8::String> script_name(result->ToString());
        script_name->WriteUtf8(last_script_name_hit);
      }
    }

    // Perform a full deoptimization when the specified number of
    // breaks have been hit.
    if (break_point_hit_count == break_point_hit_count_deoptimize) {
      i::Deoptimizer::DeoptimizeAll(isolate);
    }
  }
}


// Debug event handler which counts a number of events and collects the stack
// height if there is a function compiled for that.
int exception_hit_count = 0;
int uncaught_exception_hit_count = 0;
int last_js_stack_height = -1;

static void DebugEventCounterClear() {
  break_point_hit_count = 0;
  exception_hit_count = 0;
  uncaught_exception_hit_count = 0;
}

static void DebugEventCounter(
    const v8::Debug::EventDetails& event_details) {
  v8::DebugEvent event = event_details.GetEvent();
  v8::Handle<v8::Object> exec_state = event_details.GetExecutionState();
  v8::Handle<v8::Object> event_data = event_details.GetEventData();
  v8::internal::Debug* debug = CcTest::i_isolate()->debug();

  // When hitting a debug event listener there must be a break set.
  CHECK_NE(debug->break_id(), 0);

  // Count the number of breaks.
  if (event == v8::Break) {
    break_point_hit_count++;
  } else if (event == v8::Exception) {
    exception_hit_count++;

    // Check whether the exception was uncaught.
    v8::Local<v8::String> fun_name =
        v8::String::NewFromUtf8(CcTest::isolate(), "\x75\x6e\x63\x61\x75\x67\x68\x74");
    v8::Local<v8::Function> fun =
        v8::Local<v8::Function>::Cast(event_data->Get(fun_name));
    v8::Local<v8::Value> result = fun->Call(event_data, 0, NULL);
    if (result->IsTrue()) {
      uncaught_exception_hit_count++;
    }
  }

  // Collect the JavsScript stack height if the function frame_count is
  // compiled.
  if (!frame_count.IsEmpty()) {
    static const int kArgc = 1;
    v8::Handle<v8::Value> argv[kArgc] = { exec_state };
    // Using exec_state as receiver is just to have a receiver.
    v8::Handle<v8::Value> result =  frame_count->Call(exec_state, kArgc, argv);
    last_js_stack_height = result->Int32Value();
  }
}


// Debug event handler which evaluates a number of expressions when a break
// point is hit. Each evaluated expression is compared with an expected value.
// For this debug event handler to work the following two global varaibles
// must be initialized.
//   checks: An array of expressions and expected results
//   evaluate_check_function: A JavaScript function (see below)

// Structure for holding checks to do.
struct EvaluateCheck {
  const char* expr;  // An expression to evaluate when a break point is hit.
  v8::Handle<v8::Value> expected;  // The expected result.
};


// Array of checks to do.
struct EvaluateCheck* checks = NULL;
// Source for The JavaScript function which can do the evaluation when a break
// point is hit.
const char* evaluate_check_source =
    "\x66\x75\x6e\x63\x74\x69\x6f\x6e\x20\x65\x76\x61\x6c\x75\x61\x74\x65\x5f\x63\x68\x65\x63\x6b\x28\x65\x78\x65\x63\x5f\x73\x74\x61\x74\x65\x2c\x20\x65\x78\x70\x72\x2c\x20\x65\x78\x70\x65\x63\x74\x65\x64\x29\x20\x7b"
    "\x20\x20\x72\x65\x74\x75\x72\x6e\x20\x65\x78\x65\x63\x5f\x73\x74\x61\x74\x65\x2e\x66\x72\x61\x6d\x65\x28\x30\x29\x2e\x65\x76\x61\x6c\x75\x61\x74\x65\x28\x65\x78\x70\x72\x29\x2e\x76\x61\x6c\x75\x65\x28\x29\x20\x3d\x3d\x3d\x20\x65\x78\x70\x65\x63\x74\x65\x64\x3b"
    "\x7d";
v8::Local<v8::Function> evaluate_check_function;

// The actual debug event described by the longer comment above.
static void DebugEventEvaluate(
    const v8::Debug::EventDetails& event_details) {
  v8::DebugEvent event = event_details.GetEvent();
  v8::Handle<v8::Object> exec_state = event_details.GetExecutionState();
  v8::internal::Debug* debug = CcTest::i_isolate()->debug();
  // When hitting a debug event listener there must be a break set.
  CHECK_NE(debug->break_id(), 0);

  if (event == v8::Break) {
    for (int i = 0; checks[i].expr != NULL; i++) {
      const int argc = 3;
      v8::Handle<v8::Value> argv[argc] = {
          exec_state,
          v8::String::NewFromUtf8(CcTest::isolate(), checks[i].expr),
          checks[i].expected};
      v8::Handle<v8::Value> result =
          evaluate_check_function->Call(exec_state, argc, argv);
      if (!result->IsTrue()) {
        v8::String::Utf8Value utf8(checks[i].expected->ToString());
        V8_Fatal(__FILE__, __LINE__, "\x6c\xa2\x20\x21\x3d\x20\x6c\xa2", checks[i].expr, *utf8);
      }
    }
  }
}


// This debug event listener removes a breakpoint in a function
int debug_event_remove_break_point = 0;
static void DebugEventRemoveBreakPoint(
    const v8::Debug::EventDetails& event_details) {
  v8::DebugEvent event = event_details.GetEvent();
  v8::Handle<v8::Value> data = event_details.GetCallbackData();
  v8::internal::Debug* debug = CcTest::i_isolate()->debug();
  // When hitting a debug event listener there must be a break set.
  CHECK_NE(debug->break_id(), 0);

  if (event == v8::Break) {
    break_point_hit_count++;
    CHECK(data->IsFunction());
    ClearBreakPoint(debug_event_remove_break_point);
  }
}


// Debug event handler which counts break points hit and performs a step
// afterwards.
StepAction step_action = StepIn;  // Step action to perform when stepping.
static void DebugEventStep(
    const v8::Debug::EventDetails& event_details) {
  v8::DebugEvent event = event_details.GetEvent();
  v8::internal::Debug* debug = CcTest::i_isolate()->debug();
  // When hitting a debug event listener there must be a break set.
  CHECK_NE(debug->break_id(), 0);

  if (event == v8::Break) {
    break_point_hit_count++;
    PrepareStep(step_action);
  }
}


// Debug event handler which counts break points hit and performs a step
// afterwards. For each call the expected function is checked.
// For this debug event handler to work the following two global varaibles
// must be initialized.
//   expected_step_sequence: An array of the expected function call sequence.
//   frame_function_name: A JavaScript function (see below).

// String containing the expected function call sequence. Note: this only works
// if functions have name length of one.
const char* expected_step_sequence = NULL;

// The actual debug event described by the longer comment above.
static void DebugEventStepSequence(
    const v8::Debug::EventDetails& event_details) {
  v8::DebugEvent event = event_details.GetEvent();
  v8::Handle<v8::Object> exec_state = event_details.GetExecutionState();
  v8::internal::Debug* debug = CcTest::i_isolate()->debug();
  // When hitting a debug event listener there must be a break set.
  CHECK_NE(debug->break_id(), 0);

  if (event == v8::Break || event == v8::Exception) {
    // Check that the current function is the expected.
    CHECK(break_point_hit_count <
          StrLength(expected_step_sequence));
    const int argc = 2;
    v8::Handle<v8::Value> argv[argc] = {
      exec_state, v8::Integer::New(CcTest::isolate(), 0)
    };
    v8::Handle<v8::Value> result = frame_function_name->Call(exec_state,
                                                             argc, argv);
    CHECK(result->IsString());
    v8::String::Utf8Value function_name(result->ToString());
    CHECK_EQ(1, StrLength(*function_name));
    CHECK_EQ((*function_name)[0],
              expected_step_sequence[break_point_hit_count]);

    // Perform step.
    break_point_hit_count++;
    PrepareStep(step_action);
  }
}


// Debug event handler which performs a garbage collection.
static void DebugEventBreakPointCollectGarbage(
    const v8::Debug::EventDetails& event_details) {
  v8::DebugEvent event = event_details.GetEvent();
  v8::internal::Debug* debug = CcTest::i_isolate()->debug();
  // When hitting a debug event listener there must be a break set.
  CHECK_NE(debug->break_id(), 0);

  // Perform a garbage collection when break point is hit and continue. Based
  // on the number of break points hit either scavenge or mark compact
  // collector is used.
  if (event == v8::Break) {
    break_point_hit_count++;
    if (break_point_hit_count % 2 == 0) {
      // Scavenge.
      CcTest::heap()->CollectGarbage(v8::internal::NEW_SPACE);
    } else {
      // Mark sweep compact.
      CcTest::heap()->CollectAllGarbage(Heap::kNoGCFlags);
    }
  }
}


// Debug event handler which re-issues a debug break and calls the garbage
// collector to have the heap verified.
static void DebugEventBreak(
    const v8::Debug::EventDetails& event_details) {
  v8::DebugEvent event = event_details.GetEvent();
  v8::internal::Debug* debug = CcTest::i_isolate()->debug();
  // When hitting a debug event listener there must be a break set.
  CHECK_NE(debug->break_id(), 0);

  if (event == v8::Break) {
    // Count the number of breaks.
    break_point_hit_count++;

    // Run the garbage collector to enforce heap verification if option
    // --verify-heap is set.
    CcTest::heap()->CollectGarbage(v8::internal::NEW_SPACE);

    // Set the break flag again to come back here as soon as possible.
    v8::Debug::DebugBreak(CcTest::isolate());
  }
}


// Debug event handler which re-issues a debug break until a limit has been
// reached.
int max_break_point_hit_count = 0;
bool terminate_after_max_break_point_hit = false;
static void DebugEventBreakMax(
    const v8::Debug::EventDetails& event_details) {
  v8::DebugEvent event = event_details.GetEvent();
  v8::Handle<v8::Object> exec_state = event_details.GetExecutionState();
  v8::Isolate* v8_isolate = CcTest::isolate();
  v8::internal::Isolate* isolate = CcTest::i_isolate();
  v8::internal::Debug* debug = isolate->debug();
  // When hitting a debug event listener there must be a break set.
  CHECK_NE(debug->break_id(), 0);

  if (event == v8::Break) {
    if (break_point_hit_count < max_break_point_hit_count) {
      // Count the number of breaks.
      break_point_hit_count++;

      // Collect the JavsScript stack height if the function frame_count is
      // compiled.
      if (!frame_count.IsEmpty()) {
        static const int kArgc = 1;
        v8::Handle<v8::Value> argv[kArgc] = { exec_state };
        // Using exec_state as receiver is just to have a receiver.
        v8::Handle<v8::Value> result =
            frame_count->Call(exec_state, kArgc, argv);
        last_js_stack_height = result->Int32Value();
      }

      // Set the break flag again to come back here as soon as possible.
      v8::Debug::DebugBreak(v8_isolate);

    } else if (terminate_after_max_break_point_hit) {
      // Terminate execution after the last break if requested.
      v8::V8::TerminateExecution(v8_isolate);
    }

    // Perform a full deoptimization when the specified number of
    // breaks have been hit.
    if (break_point_hit_count == break_point_hit_count_deoptimize) {
      i::Deoptimizer::DeoptimizeAll(isolate);
    }
  }
}


// --- M e s s a g e   C a l l b a c k


// Message callback which counts the number of messages.
int message_callback_count = 0;

static void MessageCallbackCountClear() {
  message_callback_count = 0;
}

static void MessageCallbackCount(v8::Handle<v8::Message> message,
                                 v8::Handle<v8::Value> data) {
  message_callback_count++;
}


// --- T h e   A c t u a l   T e s t s


// Test that the debug break function is the expected one for different kinds
// of break locations.
TEST(DebugStub) {
  using ::v8::internal::Builtins;
  using ::v8::internal::Isolate;
  DebugLocalContext env;
  v8::HandleScope scope(env->GetIsolate());

  CheckDebugBreakFunction(&env,
                          "\x66\x75\x6e\x63\x74\x69\x6f\x6e\x20\x66\x31\x28\x29\x7b\x7d", "\x66\x31",
                          0,
                          v8::internal::RelocInfo::JS_RETURN,
                          NULL);
  CheckDebugBreakFunction(&env,
                          "\x66\x75\x6e\x63\x74\x69\x6f\x6e\x20\x66\x32\x28\x29\x7b\x78\x3d\x31\x3b\x7d", "\x66\x32",
                          0,
                          v8::internal::RelocInfo::CODE_TARGET,
                          CcTest::i_isolate()->builtins()->builtin(
                              Builtins::kStoreIC_DebugBreak));
  CheckDebugBreakFunction(&env,
                          "\x66\x75\x6e\x63\x74\x69\x6f\x6e\x20\x66\x33\x28\x29\x7b\x76\x61\x72\x20\x61\x3d\x78\x3b\x7d", "\x66\x33",
                          0,
                          v8::internal::RelocInfo::CODE_TARGET,
                          CcTest::i_isolate()->builtins()->builtin(
                              Builtins::kLoadIC_DebugBreak));

// TODO(1240753): Make the test architecture independent or split
// parts of the debugger into architecture dependent files. This
// part currently disabled as it is not portable between IA32/ARM.
// Currently on ICs for keyed store/load on ARM.
#if !defined (__arm__) && !defined(__thumb__)
  CheckDebugBreakFunction(
      &env,
      "\x66\x75\x6e\x63\x74\x69\x6f\x6e\x20\x66\x34\x28\x29\x7b\x76\x61\x72\x20\x69\x6e\x64\x65\x78\x3d\x27\x70\x72\x6f\x70\x65\x72\x74\x79\x4e\x61\x6d\x65\x27\x3b\x20\x76\x61\x72\x20\x61\x3d\x7b\x7d\x3b\x20\x61\x5b\x69\x6e\x64\x65\x78\x5d\x20\x3d\x20\x27\x78\x27\x3b\x7d",
      "\x66\x34",
      0,
      v8::internal::RelocInfo::CODE_TARGET,
      CcTest::i_isolate()->builtins()->builtin(
          Builtins::kKeyedStoreIC_DebugBreak));
  CheckDebugBreakFunction(
      &env,
      "\x66\x75\x6e\x63\x74\x69\x6f\x6e\x20\x66\x35\x28\x29\x7b\x76\x61\x72\x20\x69\x6e\x64\x65\x78\x3d\x27\x70\x72\x6f\x70\x65\x72\x74\x79\x4e\x61\x6d\x65\x27\x3b\x20\x76\x61\x72\x20\x61\x3d\x7b\x7d\x3b\x20\x72\x65\x74\x75\x72\x6e\x20\x61\x5b\x69\x6e\x64\x65\x78\x5d\x3b\x7d",
      "\x66\x35",
      0,
      v8::internal::RelocInfo::CODE_TARGET,
      CcTest::i_isolate()->builtins()->builtin(
          Builtins::kKeyedLoadIC_DebugBreak));
#endif

  CheckDebugBreakFunction(
      &env,
      "\x66\x75\x6e\x63\x74\x69\x6f\x6e\x20\x66\x36\x28\x61\x29\x7b\x72\x65\x74\x75\x72\x6e\x20\x61\x3d\x3d\x6e\x75\x6c\x6c\x3b\x7d",
      "\x66\x36",
      0,
      v8::internal::RelocInfo::CODE_TARGET,
      CcTest::i_isolate()->builtins()->builtin(
          Builtins::kCompareNilIC_DebugBreak));

  // Check the debug break code stubs for call ICs with different number of
  // parameters.
  // TODO(verwaest): XXX update test.
  // Handle<Code> debug_break_0 = v8::internal::ComputeCallDebugBreak(0);
  // Handle<Code> debug_break_1 = v8::internal::ComputeCallDebugBreak(1);
  // Handle<Code> debug_break_4 = v8::internal::ComputeCallDebugBreak(4);

  // CheckDebugBreakFunction(&env,
  //                         "function f4_0(){x();}", "f4_0",
  //                         0,
  //                         v8::internal::RelocInfo::CODE_TARGET,
  //                         *debug_break_0);

  // CheckDebugBreakFunction(&env,
  //                         "function f4_1(){x(1);}", "f4_1",
  //                         0,
  //                         v8::internal::RelocInfo::CODE_TARGET,
  //                         *debug_break_1);

  // CheckDebugBreakFunction(&env,
  //                         "function f4_4(){x(1,2,3,4);}", "f4_4",
  //                         0,
  //                         v8::internal::RelocInfo::CODE_TARGET,
  //                         *debug_break_4);
}


// Test that the debug info in the VM is in sync with the functions being
// debugged.
TEST(DebugInfo) {
  DebugLocalContext env;
  v8::HandleScope scope(env->GetIsolate());
  // Create a couple of functions for the test.
  v8::Local<v8::Function> foo =
      CompileFunction(&env, "\x66\x75\x6e\x63\x74\x69\x6f\x6e\x20\x66\x6f\x6f\x28\x29\x7b\x7d", "\x66\x6f\x6f");
  v8::Local<v8::Function> bar =
      CompileFunction(&env, "\x66\x75\x6e\x63\x74\x69\x6f\x6e\x20\x62\x61\x72\x28\x29\x7b\x7d", "\x62\x61\x72");
  // Initially no functions are debugged.
  CHECK_EQ(0, v8::internal::GetDebuggedFunctions()->length());
  CHECK(!HasDebugInfo(foo));
  CHECK(!HasDebugInfo(bar));
  // One function (foo) is debugged.
  int bp1 = SetBreakPoint(foo, 0);
  CHECK_EQ(1, v8::internal::GetDebuggedFunctions()->length());
  CHECK(HasDebugInfo(foo));
  CHECK(!HasDebugInfo(bar));
  // Two functions are debugged.
  int bp2 = SetBreakPoint(bar, 0);
  CHECK_EQ(2, v8::internal::GetDebuggedFunctions()->length());
  CHECK(HasDebugInfo(foo));
  CHECK(HasDebugInfo(bar));
  // One function (bar) is debugged.
  ClearBreakPoint(bp1);
  CHECK_EQ(1, v8::internal::GetDebuggedFunctions()->length());
  CHECK(!HasDebugInfo(foo));
  CHECK(HasDebugInfo(bar));
  // No functions are debugged.
  ClearBreakPoint(bp2);
  CHECK_EQ(0, v8::internal::GetDebuggedFunctions()->length());
  CHECK(!HasDebugInfo(foo));
  CHECK(!HasDebugInfo(bar));
}


// Test that a break point can be set at an IC store location.
TEST(BreakPointICStore) {
  break_point_hit_count = 0;
  DebugLocalContext env;
  v8::HandleScope scope(env->GetIsolate());

  v8::Debug::SetDebugEventListener(DebugEventBreakPointHitCount);
  v8::Script::Compile(v8::String::NewFromUtf8(env->GetIsolate(),
                                              "\x66\x75\x6e\x63\x74\x69\x6f\x6e\x20\x66\x6f\x6f\x28\x29\x7b\x62\x61\x72\x3d\x30\x3b\x7d"))->Run();
  v8::Local<v8::Function> foo = v8::Local<v8::Function>::Cast(
      env->Global()->Get(v8::String::NewFromUtf8(env->GetIsolate(), "\x66\x6f\x6f")));

  // Run without breakpoints.
  foo->Call(env->Global(), 0, NULL);
  CHECK_EQ(0, break_point_hit_count);

  // Run with breakpoint
  int bp = SetBreakPoint(foo, 0);
  foo->Call(env->Global(), 0, NULL);
  CHECK_EQ(1, break_point_hit_count);
  foo->Call(env->Global(), 0, NULL);
  CHECK_EQ(2, break_point_hit_count);

  // Run without breakpoints.
  ClearBreakPoint(bp);
  foo->Call(env->Global(), 0, NULL);
  CHECK_EQ(2, break_point_hit_count);

  v8::Debug::SetDebugEventListener(NULL);
  CheckDebuggerUnloaded();
}


// Test that a break point can be set at an IC load location.
TEST(BreakPointICLoad) {
  break_point_hit_count = 0;
  DebugLocalContext env;
  v8::HandleScope scope(env->GetIsolate());
  v8::Debug::SetDebugEventListener(DebugEventBreakPointHitCount);
  v8::Script::Compile(v8::String::NewFromUtf8(env->GetIsolate(), "\x62\x61\x72\x3d\x31"))
      ->Run();
  v8::Script::Compile(
      v8::String::NewFromUtf8(env->GetIsolate(), "\x66\x75\x6e\x63\x74\x69\x6f\x6e\x20\x66\x6f\x6f\x28\x29\x7b\x76\x61\x72\x20\x78\x3d\x62\x61\x72\x3b\x7d"))
      ->Run();
  v8::Local<v8::Function> foo = v8::Local<v8::Function>::Cast(
      env->Global()->Get(v8::String::NewFromUtf8(env->GetIsolate(), "\x66\x6f\x6f")));

  // Run without breakpoints.
  foo->Call(env->Global(), 0, NULL);
  CHECK_EQ(0, break_point_hit_count);

  // Run with breakpoint.
  int bp = SetBreakPoint(foo, 0);
  foo->Call(env->Global(), 0, NULL);
  CHECK_EQ(1, break_point_hit_count);
  foo->Call(env->Global(), 0, NULL);
  CHECK_EQ(2, break_point_hit_count);

  // Run without breakpoints.
  ClearBreakPoint(bp);
  foo->Call(env->Global(), 0, NULL);
  CHECK_EQ(2, break_point_hit_count);

  v8::Debug::SetDebugEventListener(NULL);
  CheckDebuggerUnloaded();
}


// Test that a break point can be set at an IC call location.
TEST(BreakPointICCall) {
  break_point_hit_count = 0;
  DebugLocalContext env;
  v8::HandleScope scope(env->GetIsolate());
  v8::Debug::SetDebugEventListener(DebugEventBreakPointHitCount);
  v8::Script::Compile(
      v8::String::NewFromUtf8(env->GetIsolate(), "\x66\x75\x6e\x63\x74\x69\x6f\x6e\x20\x62\x61\x72\x28\x29\x7b\x7d"))->Run();
  v8::Script::Compile(v8::String::NewFromUtf8(env->GetIsolate(),
                                              "\x66\x75\x6e\x63\x74\x69\x6f\x6e\x20\x66\x6f\x6f\x28\x29\x7b\x62\x61\x72\x28\x29\x3b\x7d"))->Run();
  v8::Local<v8::Function> foo = v8::Local<v8::Function>::Cast(
      env->Global()->Get(v8::String::NewFromUtf8(env->GetIsolate(), "\x66\x6f\x6f")));

  // Run without breakpoints.
  foo->Call(env->Global(), 0, NULL);
  CHECK_EQ(0, break_point_hit_count);

  // Run with breakpoint
  int bp = SetBreakPoint(foo, 0);
  foo->Call(env->Global(), 0, NULL);
  CHECK_EQ(1, break_point_hit_count);
  foo->Call(env->Global(), 0, NULL);
  CHECK_EQ(2, break_point_hit_count);

  // Run without breakpoints.
  ClearBreakPoint(bp);
  foo->Call(env->Global(), 0, NULL);
  CHECK_EQ(2, break_point_hit_count);

  v8::Debug::SetDebugEventListener(NULL);
  CheckDebuggerUnloaded();
}


// Test that a break point can be set at an IC call location and survive a GC.
TEST(BreakPointICCallWithGC) {
  break_point_hit_count = 0;
  DebugLocalContext env;
  v8::HandleScope scope(env->GetIsolate());
  v8::Debug::SetDebugEventListener(DebugEventBreakPointCollectGarbage);
  v8::Script::Compile(
      v8::String::NewFromUtf8(env->GetIsolate(), "\x66\x75\x6e\x63\x74\x69\x6f\x6e\x20\x62\x61\x72\x28\x29\x7b\x72\x65\x74\x75\x72\x6e\x20\x31\x3b\x7d"))
      ->Run();
  v8::Script::Compile(v8::String::NewFromUtf8(env->GetIsolate(),
                                              "\x66\x75\x6e\x63\x74\x69\x6f\x6e\x20\x66\x6f\x6f\x28\x29\x7b\x72\x65\x74\x75\x72\x6e\x20\x62\x61\x72\x28\x29\x3b\x7d"))
      ->Run();
  v8::Local<v8::Function> foo = v8::Local<v8::Function>::Cast(
      env->Global()->Get(v8::String::NewFromUtf8(env->GetIsolate(), "\x66\x6f\x6f")));

  // Run without breakpoints.
  CHECK_EQ(1, foo->Call(env->Global(), 0, NULL)->Int32Value());
  CHECK_EQ(0, break_point_hit_count);

  // Run with breakpoint.
  int bp = SetBreakPoint(foo, 0);
  CHECK_EQ(1, foo->Call(env->Global(), 0, NULL)->Int32Value());
  CHECK_EQ(1, break_point_hit_count);
  CHECK_EQ(1, foo->Call(env->Global(), 0, NULL)->Int32Value());
  CHECK_EQ(2, break_point_hit_count);

  // Run without breakpoints.
  ClearBreakPoint(bp);
  foo->Call(env->Global(), 0, NULL);
  CHECK_EQ(2, break_point_hit_count);

  v8::Debug::SetDebugEventListener(NULL);
  CheckDebuggerUnloaded();
}


// Test that a break point can be set at an IC call location and survive a GC.
TEST(BreakPointConstructCallWithGC) {
  break_point_hit_count = 0;
  DebugLocalContext env;
  v8::HandleScope scope(env->GetIsolate());
  v8::Debug::SetDebugEventListener(DebugEventBreakPointCollectGarbage);
  v8::Script::Compile(v8::String::NewFromUtf8(env->GetIsolate(),
                                              "\x66\x75\x6e\x63\x74\x69\x6f\x6e\x20\x62\x61\x72\x28\x29\x7b\x20\x74\x68\x69\x73\x2e\x78\x20\x3d\x20\x31\x3b\x7d"))
      ->Run();
  v8::Script::Compile(
      v8::String::NewFromUtf8(env->GetIsolate(),
                              "\x66\x75\x6e\x63\x74\x69\x6f\x6e\x20\x66\x6f\x6f\x28\x29\x7b\x72\x65\x74\x75\x72\x6e\x20\x6e\x65\x77\x20\x62\x61\x72\x28\x31\x29\x2e\x78\x3b\x7d"))->Run();
  v8::Local<v8::Function> foo = v8::Local<v8::Function>::Cast(
      env->Global()->Get(v8::String::NewFromUtf8(env->GetIsolate(), "\x66\x6f\x6f")));

  // Run without breakpoints.
  CHECK_EQ(1, foo->Call(env->Global(), 0, NULL)->Int32Value());
  CHECK_EQ(0, break_point_hit_count);

  // Run with breakpoint.
  int bp = SetBreakPoint(foo, 0);
  CHECK_EQ(1, foo->Call(env->Global(), 0, NULL)->Int32Value());
  CHECK_EQ(1, break_point_hit_count);
  CHECK_EQ(1, foo->Call(env->Global(), 0, NULL)->Int32Value());
  CHECK_EQ(2, break_point_hit_count);

  // Run without breakpoints.
  ClearBreakPoint(bp);
  foo->Call(env->Global(), 0, NULL);
  CHECK_EQ(2, break_point_hit_count);

  v8::Debug::SetDebugEventListener(NULL);
  CheckDebuggerUnloaded();
}


// Test that a break point can be set at a return store location.
TEST(BreakPointReturn) {
  break_point_hit_count = 0;
  DebugLocalContext env;
  v8::HandleScope scope(env->GetIsolate());

  // Create a functions for checking the source line and column when hitting
  // a break point.
  frame_source_line = CompileFunction(&env,
                                      frame_source_line_source,
                                      "\x66\x72\x61\x6d\x65\x5f\x73\x6f\x75\x72\x63\x65\x5f\x6c\x69\x6e\x65");
  frame_source_column = CompileFunction(&env,
                                        frame_source_column_source,
                                        "\x66\x72\x61\x6d\x65\x5f\x73\x6f\x75\x72\x63\x65\x5f\x63\x6f\x6c\x75\x6d\x6e");


  v8::Debug::SetDebugEventListener(DebugEventBreakPointHitCount);
  v8::Script::Compile(
      v8::String::NewFromUtf8(env->GetIsolate(), "\x66\x75\x6e\x63\x74\x69\x6f\x6e\x20\x66\x6f\x6f\x28\x29\x7b\x7d"))->Run();
  v8::Local<v8::Function> foo = v8::Local<v8::Function>::Cast(
      env->Global()->Get(v8::String::NewFromUtf8(env->GetIsolate(), "\x66\x6f\x6f")));

  // Run without breakpoints.
  foo->Call(env->Global(), 0, NULL);
  CHECK_EQ(0, break_point_hit_count);

  // Run with breakpoint
  int bp = SetBreakPoint(foo, 0);
  foo->Call(env->Global(), 0, NULL);
  CHECK_EQ(1, break_point_hit_count);
  CHECK_EQ(0, last_source_line);
  CHECK_EQ(15, last_source_column);
  foo->Call(env->Global(), 0, NULL);
  CHECK_EQ(2, break_point_hit_count);
  CHECK_EQ(0, last_source_line);
  CHECK_EQ(15, last_source_column);

  // Run without breakpoints.
  ClearBreakPoint(bp);
  foo->Call(env->Global(), 0, NULL);
  CHECK_EQ(2, break_point_hit_count);

  v8::Debug::SetDebugEventListener(NULL);
  CheckDebuggerUnloaded();
}


static void CallWithBreakPoints(v8::Local<v8::Object> recv,
                                v8::Local<v8::Function> f,
                                int break_point_count,
                                int call_count) {
  break_point_hit_count = 0;
  for (int i = 0; i < call_count; i++) {
    f->Call(recv, 0, NULL);
    CHECK_EQ((i + 1) * break_point_count, break_point_hit_count);
  }
}


// Test GC during break point processing.
TEST(GCDuringBreakPointProcessing) {
  break_point_hit_count = 0;
  DebugLocalContext env;
  v8::HandleScope scope(env->GetIsolate());

  v8::Debug::SetDebugEventListener(DebugEventBreakPointCollectGarbage);
  v8::Local<v8::Function> foo;

  // Test IC store break point with garbage collection.
  foo = CompileFunction(&env, "\x66\x75\x6e\x63\x74\x69\x6f\x6e\x20\x66\x6f\x6f\x28\x29\x7b\x62\x61\x72\x3d\x30\x3b\x7d", "\x66\x6f\x6f");
  SetBreakPoint(foo, 0);
  CallWithBreakPoints(env->Global(), foo, 1, 10);

  // Test IC load break point with garbage collection.
  foo = CompileFunction(&env, "\x62\x61\x72\x3d\x31\x3b\x66\x75\x6e\x63\x74\x69\x6f\x6e\x20\x66\x6f\x6f\x28\x29\x7b\x76\x61\x72\x20\x78\x3d\x62\x61\x72\x3b\x7d", "\x66\x6f\x6f");
  SetBreakPoint(foo, 0);
  CallWithBreakPoints(env->Global(), foo, 1, 10);

  // Test IC call break point with garbage collection.
  foo = CompileFunction(&env, "\x66\x75\x6e\x63\x74\x69\x6f\x6e\x20\x62\x61\x72\x28\x29\x7b\x7d\x3b\x66\x75\x6e\x63\x74\x69\x6f\x6e\x20\x66\x6f\x6f\x28\x29\x7b\x62\x61\x72\x28\x29\x3b\x7d", "\x66\x6f\x6f");
  SetBreakPoint(foo, 0);
  CallWithBreakPoints(env->Global(), foo, 1, 10);

  // Test return break point with garbage collection.
  foo = CompileFunction(&env, "\x66\x75\x6e\x63\x74\x69\x6f\x6e\x20\x66\x6f\x6f\x28\x29\x7b\x7d", "\x66\x6f\x6f");
  SetBreakPoint(foo, 0);
  CallWithBreakPoints(env->Global(), foo, 1, 25);

  // Test debug break slot break point with garbage collection.
  foo = CompileFunction(&env, "\x66\x75\x6e\x63\x74\x69\x6f\x6e\x20\x66\x6f\x6f\x28\x29\x7b\x76\x61\x72\x20\x61\x3b\x7d", "\x66\x6f\x6f");
  SetBreakPoint(foo, 0);
  CallWithBreakPoints(env->Global(), foo, 1, 25);

  v8::Debug::SetDebugEventListener(NULL);
  CheckDebuggerUnloaded();
}


// Call the function three times with different garbage collections in between
// and make sure that the break point survives.
static void CallAndGC(v8::Local<v8::Object> recv,
                      v8::Local<v8::Function> f) {
  break_point_hit_count = 0;

  for (int i = 0; i < 3; i++) {
    // Call function.
    f->Call(recv, 0, NULL);
    CHECK_EQ(1 + i * 3, break_point_hit_count);

    // Scavenge and call function.
    CcTest::heap()->CollectGarbage(v8::internal::NEW_SPACE);
    f->Call(recv, 0, NULL);
    CHECK_EQ(2 + i * 3, break_point_hit_count);

    // Mark sweep (and perhaps compact) and call function.
    CcTest::heap()->CollectAllGarbage(Heap::kNoGCFlags);
    f->Call(recv, 0, NULL);
    CHECK_EQ(3 + i * 3, break_point_hit_count);
  }
}


// Test that a break point can be set at a return store location.
TEST(BreakPointSurviveGC) {
  break_point_hit_count = 0;
  DebugLocalContext env;
  v8::HandleScope scope(env->GetIsolate());

  v8::Debug::SetDebugEventListener(DebugEventBreakPointHitCount);
  v8::Local<v8::Function> foo;

  // Test IC store break point with garbage collection.
  {
    CompileFunction(&env, "\x66\x75\x6e\x63\x74\x69\x6f\x6e\x20\x66\x6f\x6f\x28\x29\x7b\x7d", "\x66\x6f\x6f");
    foo = CompileFunction(&env, "\x66\x75\x6e\x63\x74\x69\x6f\x6e\x20\x66\x6f\x6f\x28\x29\x7b\x62\x61\x72\x3d\x30\x3b\x7d", "\x66\x6f\x6f");
    SetBreakPoint(foo, 0);
  }
  CallAndGC(env->Global(), foo);

  // Test IC load break point with garbage collection.
  {
    CompileFunction(&env, "\x66\x75\x6e\x63\x74\x69\x6f\x6e\x20\x66\x6f\x6f\x28\x29\x7b\x7d", "\x66\x6f\x6f");
    foo = CompileFunction(&env, "\x62\x61\x72\x3d\x31\x3b\x66\x75\x6e\x63\x74\x69\x6f\x6e\x20\x66\x6f\x6f\x28\x29\x7b\x76\x61\x72\x20\x78\x3d\x62\x61\x72\x3b\x7d", "\x66\x6f\x6f");
    SetBreakPoint(foo, 0);
  }
  CallAndGC(env->Global(), foo);

  // Test IC call break point with garbage collection.
  {
    CompileFunction(&env, "\x66\x75\x6e\x63\x74\x69\x6f\x6e\x20\x66\x6f\x6f\x28\x29\x7b\x7d", "\x66\x6f\x6f");
    foo = CompileFunction(&env,
                          "\x66\x75\x6e\x63\x74\x69\x6f\x6e\x20\x62\x61\x72\x28\x29\x7b\x7d\x3b\x66\x75\x6e\x63\x74\x69\x6f\x6e\x20\x66\x6f\x6f\x28\x29\x7b\x62\x61\x72\x28\x29\x3b\x7d",
                          "\x66\x6f\x6f");
    SetBreakPoint(foo, 0);
  }
  CallAndGC(env->Global(), foo);

  // Test return break point with garbage collection.
  {
    CompileFunction(&env, "\x66\x75\x6e\x63\x74\x69\x6f\x6e\x20\x66\x6f\x6f\x28\x29\x7b\x7d", "\x66\x6f\x6f");
    foo = CompileFunction(&env, "\x66\x75\x6e\x63\x74\x69\x6f\x6e\x20\x66\x6f\x6f\x28\x29\x7b\x7d", "\x66\x6f\x6f");
    SetBreakPoint(foo, 0);
  }
  CallAndGC(env->Global(), foo);

  // Test non IC break point with garbage collection.
  {
    CompileFunction(&env, "\x66\x75\x6e\x63\x74\x69\x6f\x6e\x20\x66\x6f\x6f\x28\x29\x7b\x7d", "\x66\x6f\x6f");
    foo = CompileFunction(&env, "\x66\x75\x6e\x63\x74\x69\x6f\x6e\x20\x66\x6f\x6f\x28\x29\x7b\x76\x61\x72\x20\x62\x61\x72\x3d\x30\x3b\x7d", "\x66\x6f\x6f");
    SetBreakPoint(foo, 0);
  }
  CallAndGC(env->Global(), foo);


  v8::Debug::SetDebugEventListener(NULL);
  CheckDebuggerUnloaded();
}


// Test that break points can be set using the global Debug object.
TEST(BreakPointThroughJavaScript) {
  break_point_hit_count = 0;
  DebugLocalContext env;
  v8::HandleScope scope(env->GetIsolate());
  env.ExposeDebug();

  v8::Debug::SetDebugEventListener(DebugEventBreakPointHitCount);
  v8::Script::Compile(
      v8::String::NewFromUtf8(env->GetIsolate(), "\x66\x75\x6e\x63\x74\x69\x6f\x6e\x20\x62\x61\x72\x28\x29\x7b\x7d"))->Run();
  v8::Script::Compile(v8::String::NewFromUtf8(env->GetIsolate(),
                                              "\x66\x75\x6e\x63\x74\x69\x6f\x6e\x20\x66\x6f\x6f\x28\x29\x7b\x62\x61\x72\x28\x29\x3b\x62\x61\x72\x28\x29\x3b\x7d"))
      ->Run();
  //                                               012345678901234567890
  //                                                         1         2
  // Break points are set at position 3 and 9
  v8::Local<v8::Script> foo =
      v8::Script::Compile(v8::String::NewFromUtf8(env->GetIsolate(), "\x66\x6f\x6f\x28\x29"));

  // Run without breakpoints.
  foo->Run();
  CHECK_EQ(0, break_point_hit_count);

  // Run with one breakpoint
  int bp1 = SetBreakPointFromJS(env->GetIsolate(), "\x66\x6f\x6f", 0, 3);
  foo->Run();
  CHECK_EQ(1, break_point_hit_count);
  foo->Run();
  CHECK_EQ(2, break_point_hit_count);

  // Run with two breakpoints
  int bp2 = SetBreakPointFromJS(env->GetIsolate(), "\x66\x6f\x6f", 0, 9);
  foo->Run();
  CHECK_EQ(4, break_point_hit_count);
  foo->Run();
  CHECK_EQ(6, break_point_hit_count);

  // Run with one breakpoint
  ClearBreakPointFromJS(env->GetIsolate(), bp2);
  foo->Run();
  CHECK_EQ(7, break_point_hit_count);
  foo->Run();
  CHECK_EQ(8, break_point_hit_count);

  // Run without breakpoints.
  ClearBreakPointFromJS(env->GetIsolate(), bp1);
  foo->Run();
  CHECK_EQ(8, break_point_hit_count);

  v8::Debug::SetDebugEventListener(NULL);
  CheckDebuggerUnloaded();

  // Make sure that the break point numbers are consecutive.
  CHECK_EQ(1, bp1);
  CHECK_EQ(2, bp2);
}


// Test that break points on scripts identified by name can be set using the
// global Debug object.
TEST(ScriptBreakPointByNameThroughJavaScript) {
  break_point_hit_count = 0;
  DebugLocalContext env;
  v8::HandleScope scope(env->GetIsolate());
  env.ExposeDebug();

  v8::Debug::SetDebugEventListener(DebugEventBreakPointHitCount);

  v8::Local<v8::String> script = v8::String::NewFromUtf8(
    env->GetIsolate(),
    "\x66\x75\x6e\x63\x74\x69\x6f\x6e\x20\x66\x28\x29\x20\x7b\xa"
    "\x20\x20\x66\x75\x6e\x63\x74\x69\x6f\x6e\x20\x68\x28\x29\x20\x7b\xa"
    "\x20\x20\x20\x20\x61\x20\x3d\x20\x30\x3b\x20\x20\x2f\x2f\x20\x6c\x69\x6e\x65\x20\x32\xa"
    "\x20\x20\x7d\xa"
    "\x20\x20\x62\x20\x3d\x20\x31\x3b\x20\x20\x2f\x2f\x20\x6c\x69\x6e\x65\x20\x34\xa"
    "\x20\x20\x72\x65\x74\x75\x72\x6e\x20\x68\x28\x29\x3b\xa"
    "\x7d\xa"
    "\xa"
    "\x66\x75\x6e\x63\x74\x69\x6f\x6e\x20\x67\x28\x29\x20\x7b\xa"
    "\x20\x20\x66\x75\x6e\x63\x74\x69\x6f\x6e\x20\x68\x28\x29\x20\x7b\xa"
    "\x20\x20\x20\x20\x61\x20\x3d\x20\x30\x3b\xa"
    "\x20\x20\x7d\xa"
    "\x20\x20\x62\x20\x3d\x20\x32\x3b\x20\x20\x2f\x2f\x20\x6c\x69\x6e\x65\x20\x31\x32\xa"
    "\x20\x20\x68\x28\x29\x3b\xa"
    "\x20\x20\x62\x20\x3d\x20\x33\x3b\x20\x20\x2f\x2f\x20\x6c\x69\x6e\x65\x20\x31\x34\xa"
    "\x20\x20\x66\x28\x29\x3b\x20\x20\x20\x20\x2f\x2f\x20\x6c\x69\x6e\x65\x20\x31\x35\xa"
    "\x7d");

  // Compile the script and get the two functions.
  v8::ScriptOrigin origin =
      v8::ScriptOrigin(v8::String::NewFromUtf8(env->GetIsolate(), "\x74\x65\x73\x74"));
  v8::Script::Compile(script, &origin)->Run();
  v8::Local<v8::Function> f = v8::Local<v8::Function>::Cast(
      env->Global()->Get(v8::String::NewFromUtf8(env->GetIsolate(), "\x66")));
  v8::Local<v8::Function> g = v8::Local<v8::Function>::Cast(
      env->Global()->Get(v8::String::NewFromUtf8(env->GetIsolate(), "\x67")));

  // Call f and g without break points.
  break_point_hit_count = 0;
  f->Call(env->Global(), 0, NULL);
  CHECK_EQ(0, break_point_hit_count);
  g->Call(env->Global(), 0, NULL);
  CHECK_EQ(0, break_point_hit_count);

  // Call f and g with break point on line 12.
  int sbp1 = SetScriptBreakPointByNameFromJS(env->GetIsolate(), "\x74\x65\x73\x74", 12, 0);
  break_point_hit_count = 0;
  f->Call(env->Global(), 0, NULL);
  CHECK_EQ(0, break_point_hit_count);
  g->Call(env->Global(), 0, NULL);
  CHECK_EQ(1, break_point_hit_count);

  // Remove the break point again.
  break_point_hit_count = 0;
  ClearBreakPointFromJS(env->GetIsolate(), sbp1);
  f->Call(env->Global(), 0, NULL);
  CHECK_EQ(0, break_point_hit_count);
  g->Call(env->Global(), 0, NULL);
  CHECK_EQ(0, break_point_hit_count);

  // Call f and g with break point on line 2.
  int sbp2 = SetScriptBreakPointByNameFromJS(env->GetIsolate(), "\x74\x65\x73\x74", 2, 0);
  break_point_hit_count = 0;
  f->Call(env->Global(), 0, NULL);
  CHECK_EQ(1, break_point_hit_count);
  g->Call(env->Global(), 0, NULL);
  CHECK_EQ(2, break_point_hit_count);

  // Call f and g with break point on line 2, 4, 12, 14 and 15.
  int sbp3 = SetScriptBreakPointByNameFromJS(env->GetIsolate(), "\x74\x65\x73\x74", 4, 0);
  int sbp4 = SetScriptBreakPointByNameFromJS(env->GetIsolate(), "\x74\x65\x73\x74", 12, 0);
  int sbp5 = SetScriptBreakPointByNameFromJS(env->GetIsolate(), "\x74\x65\x73\x74", 14, 0);
  int sbp6 = SetScriptBreakPointByNameFromJS(env->GetIsolate(), "\x74\x65\x73\x74", 15, 0);
  break_point_hit_count = 0;
  f->Call(env->Global(), 0, NULL);
  CHECK_EQ(2, break_point_hit_count);
  g->Call(env->Global(), 0, NULL);
  CHECK_EQ(7, break_point_hit_count);

  // Remove all the break points again.
  break_point_hit_count = 0;
  ClearBreakPointFromJS(env->GetIsolate(), sbp2);
  ClearBreakPointFromJS(env->GetIsolate(), sbp3);
  ClearBreakPointFromJS(env->GetIsolate(), sbp4);
  ClearBreakPointFromJS(env->GetIsolate(), sbp5);
  ClearBreakPointFromJS(env->GetIsolate(), sbp6);
  f->Call(env->Global(), 0, NULL);
  CHECK_EQ(0, break_point_hit_count);
  g->Call(env->Global(), 0, NULL);
  CHECK_EQ(0, break_point_hit_count);

  v8::Debug::SetDebugEventListener(NULL);
  CheckDebuggerUnloaded();

  // Make sure that the break point numbers are consecutive.
  CHECK_EQ(1, sbp1);
  CHECK_EQ(2, sbp2);
  CHECK_EQ(3, sbp3);
  CHECK_EQ(4, sbp4);
  CHECK_EQ(5, sbp5);
  CHECK_EQ(6, sbp6);
}


TEST(ScriptBreakPointByIdThroughJavaScript) {
  break_point_hit_count = 0;
  DebugLocalContext env;
  v8::HandleScope scope(env->GetIsolate());
  env.ExposeDebug();

  v8::Debug::SetDebugEventListener(DebugEventBreakPointHitCount);

  v8::Local<v8::String> source = v8::String::NewFromUtf8(
    env->GetIsolate(),
    "\x66\x75\x6e\x63\x74\x69\x6f\x6e\x20\x66\x28\x29\x20\x7b\xa"
    "\x20\x20\x66\x75\x6e\x63\x74\x69\x6f\x6e\x20\x68\x28\x29\x20\x7b\xa"
    "\x20\x20\x20\x20\x61\x20\x3d\x20\x30\x3b\x20\x20\x2f\x2f\x20\x6c\x69\x6e\x65\x20\x32\xa"
    "\x20\x20\x7d\xa"
    "\x20\x20\x62\x20\x3d\x20\x31\x3b\x20\x20\x2f\x2f\x20\x6c\x69\x6e\x65\x20\x34\xa"
    "\x20\x20\x72\x65\x74\x75\x72\x6e\x20\x68\x28\x29\x3b\xa"
    "\x7d\xa"
    "\xa"
    "\x66\x75\x6e\x63\x74\x69\x6f\x6e\x20\x67\x28\x29\x20\x7b\xa"
    "\x20\x20\x66\x75\x6e\x63\x74\x69\x6f\x6e\x20\x68\x28\x29\x20\x7b\xa"
    "\x20\x20\x20\x20\x61\x20\x3d\x20\x30\x3b\xa"
    "\x20\x20\x7d\xa"
    "\x20\x20\x62\x20\x3d\x20\x32\x3b\x20\x20\x2f\x2f\x20\x6c\x69\x6e\x65\x20\x31\x32\xa"
    "\x20\x20\x68\x28\x29\x3b\xa"
    "\x20\x20\x62\x20\x3d\x20\x33\x3b\x20\x20\x2f\x2f\x20\x6c\x69\x6e\x65\x20\x31\x34\xa"
    "\x20\x20\x66\x28\x29\x3b\x20\x20\x20\x20\x2f\x2f\x20\x6c\x69\x6e\x65\x20\x31\x35\xa"
    "\x7d");

  // Compile the script and get the two functions.
  v8::ScriptOrigin origin =
      v8::ScriptOrigin(v8::String::NewFromUtf8(env->GetIsolate(), "\x74\x65\x73\x74"));
  v8::Local<v8::Script> script = v8::Script::Compile(source, &origin);
  script->Run();
  v8::Local<v8::Function> f = v8::Local<v8::Function>::Cast(
      env->Global()->Get(v8::String::NewFromUtf8(env->GetIsolate(), "\x66")));
  v8::Local<v8::Function> g = v8::Local<v8::Function>::Cast(
      env->Global()->Get(v8::String::NewFromUtf8(env->GetIsolate(), "\x67")));

  // Get the script id knowing that internally it is a 32 integer.
  int script_id = script->GetUnboundScript()->GetId();

  // Call f and g without break points.
  break_point_hit_count = 0;
  f->Call(env->Global(), 0, NULL);
  CHECK_EQ(0, break_point_hit_count);
  g->Call(env->Global(), 0, NULL);
  CHECK_EQ(0, break_point_hit_count);

  // Call f and g with break point on line 12.
  int sbp1 = SetScriptBreakPointByIdFromJS(env->GetIsolate(), script_id, 12, 0);
  break_point_hit_count = 0;
  f->Call(env->Global(), 0, NULL);
  CHECK_EQ(0, break_point_hit_count);
  g->Call(env->Global(), 0, NULL);
  CHECK_EQ(1, break_point_hit_count);

  // Remove the break point again.
  break_point_hit_count = 0;
  ClearBreakPointFromJS(env->GetIsolate(), sbp1);
  f->Call(env->Global(), 0, NULL);
  CHECK_EQ(0, break_point_hit_count);
  g->Call(env->Global(), 0, NULL);
  CHECK_EQ(0, break_point_hit_count);

  // Call f and g with break point on line 2.
  int sbp2 = SetScriptBreakPointByIdFromJS(env->GetIsolate(), script_id, 2, 0);
  break_point_hit_count = 0;
  f->Call(env->Global(), 0, NULL);
  CHECK_EQ(1, break_point_hit_count);
  g->Call(env->Global(), 0, NULL);
  CHECK_EQ(2, break_point_hit_count);

  // Call f and g with break point on line 2, 4, 12, 14 and 15.
  int sbp3 = SetScriptBreakPointByIdFromJS(env->GetIsolate(), script_id, 4, 0);
  int sbp4 = SetScriptBreakPointByIdFromJS(env->GetIsolate(), script_id, 12, 0);
  int sbp5 = SetScriptBreakPointByIdFromJS(env->GetIsolate(), script_id, 14, 0);
  int sbp6 = SetScriptBreakPointByIdFromJS(env->GetIsolate(), script_id, 15, 0);
  break_point_hit_count = 0;
  f->Call(env->Global(), 0, NULL);
  CHECK_EQ(2, break_point_hit_count);
  g->Call(env->Global(), 0, NULL);
  CHECK_EQ(7, break_point_hit_count);

  // Remove all the break points again.
  break_point_hit_count = 0;
  ClearBreakPointFromJS(env->GetIsolate(), sbp2);
  ClearBreakPointFromJS(env->GetIsolate(), sbp3);
  ClearBreakPointFromJS(env->GetIsolate(), sbp4);
  ClearBreakPointFromJS(env->GetIsolate(), sbp5);
  ClearBreakPointFromJS(env->GetIsolate(), sbp6);
  f->Call(env->Global(), 0, NULL);
  CHECK_EQ(0, break_point_hit_count);
  g->Call(env->Global(), 0, NULL);
  CHECK_EQ(0, break_point_hit_count);

  v8::Debug::SetDebugEventListener(NULL);
  CheckDebuggerUnloaded();

  // Make sure that the break point numbers are consecutive.
  CHECK_EQ(1, sbp1);
  CHECK_EQ(2, sbp2);
  CHECK_EQ(3, sbp3);
  CHECK_EQ(4, sbp4);
  CHECK_EQ(5, sbp5);
  CHECK_EQ(6, sbp6);
}


// Test conditional script break points.
TEST(EnableDisableScriptBreakPoint) {
  break_point_hit_count = 0;
  DebugLocalContext env;
  v8::HandleScope scope(env->GetIsolate());
  env.ExposeDebug();

  v8::Debug::SetDebugEventListener(DebugEventBreakPointHitCount);

  v8::Local<v8::String> script = v8::String::NewFromUtf8(
    env->GetIsolate(),
    "\x66\x75\x6e\x63\x74\x69\x6f\x6e\x20\x66\x28\x29\x20\x7b\xa"
    "\x20\x20\x61\x20\x3d\x20\x30\x3b\x20\x20\x2f\x2f\x20\x6c\x69\x6e\x65\x20\x31\xa"
    "\x7d\x3b");

  // Compile the script and get function f.
  v8::ScriptOrigin origin =
      v8::ScriptOrigin(v8::String::NewFromUtf8(env->GetIsolate(), "\x74\x65\x73\x74"));
  v8::Script::Compile(script, &origin)->Run();
  v8::Local<v8::Function> f = v8::Local<v8::Function>::Cast(
      env->Global()->Get(v8::String::NewFromUtf8(env->GetIsolate(), "\x66")));

  // Set script break point on line 1 (in function f).
  int sbp = SetScriptBreakPointByNameFromJS(env->GetIsolate(), "\x74\x65\x73\x74", 1, 0);

  // Call f while enabeling and disabling the script break point.
  break_point_hit_count = 0;
  f->Call(env->Global(), 0, NULL);
  CHECK_EQ(1, break_point_hit_count);

  DisableScriptBreakPointFromJS(env->GetIsolate(), sbp);
  f->Call(env->Global(), 0, NULL);
  CHECK_EQ(1, break_point_hit_count);

  EnableScriptBreakPointFromJS(env->GetIsolate(), sbp);
  f->Call(env->Global(), 0, NULL);
  CHECK_EQ(2, break_point_hit_count);

  DisableScriptBreakPointFromJS(env->GetIsolate(), sbp);
  f->Call(env->Global(), 0, NULL);
  CHECK_EQ(2, break_point_hit_count);

  // Reload the script and get f again checking that the disabeling survives.
  v8::Script::Compile(script, &origin)->Run();
  f = v8::Local<v8::Function>::Cast(
      env->Global()->Get(v8::String::NewFromUtf8(env->GetIsolate(), "\x66")));
  f->Call(env->Global(), 0, NULL);
  CHECK_EQ(2, break_point_hit_count);

  EnableScriptBreakPointFromJS(env->GetIsolate(), sbp);
  f->Call(env->Global(), 0, NULL);
  CHECK_EQ(3, break_point_hit_count);

  v8::Debug::SetDebugEventListener(NULL);
  CheckDebuggerUnloaded();
}


// Test conditional script break points.
TEST(ConditionalScriptBreakPoint) {
  break_point_hit_count = 0;
  DebugLocalContext env;
  v8::HandleScope scope(env->GetIsolate());
  env.ExposeDebug();

  v8::Debug::SetDebugEventListener(DebugEventBreakPointHitCount);

  v8::Local<v8::String> script = v8::String::NewFromUtf8(
    env->GetIsolate(),
    "\x63\x6f\x75\x6e\x74\x20\x3d\x20\x30\x3b\xa"
    "\x66\x75\x6e\x63\x74\x69\x6f\x6e\x20\x66\x28\x29\x20\x7b\xa"
    "\x20\x20\x67\x28\x63\x6f\x75\x6e\x74\x2b\x2b\x29\x3b\x20\x20\x2f\x2f\x20\x6c\x69\x6e\x65\x20\x32\xa"
    "\x7d\x3b\xa"
    "\x66\x75\x6e\x63\x74\x69\x6f\x6e\x20\x67\x28\x78\x29\x20\x7b\xa"
    "\x20\x20\x76\x61\x72\x20\x61\x3d\x78\x3b\x20\x20\x2f\x2f\x20\x6c\x69\x6e\x65\x20\x35\xa"
    "\x7d\x3b");

  // Compile the script and get function f.
  v8::ScriptOrigin origin =
      v8::ScriptOrigin(v8::String::NewFromUtf8(env->GetIsolate(), "\x74\x65\x73\x74"));
  v8::Script::Compile(script, &origin)->Run();
  v8::Local<v8::Function> f = v8::Local<v8::Function>::Cast(
      env->Global()->Get(v8::String::NewFromUtf8(env->GetIsolate(), "\x66")));

  // Set script break point on line 5 (in function g).
  int sbp1 = SetScriptBreakPointByNameFromJS(env->GetIsolate(), "\x74\x65\x73\x74", 5, 0);

  // Call f with different conditions on the script break point.
  break_point_hit_count = 0;
  ChangeScriptBreakPointConditionFromJS(env->GetIsolate(), sbp1, "\x66\x61\x6c\x73\x65");
  f->Call(env->Global(), 0, NULL);
  CHECK_EQ(0, break_point_hit_count);

  ChangeScriptBreakPointConditionFromJS(env->GetIsolate(), sbp1, "\x74\x72\x75\x65");
  break_point_hit_count = 0;
  f->Call(env->Global(), 0, NULL);
  CHECK_EQ(1, break_point_hit_count);

  ChangeScriptBreakPointConditionFromJS(env->GetIsolate(), sbp1, "\x78\x20\x25\x20\x32\x20\x3d\x3d\x20\x30");
  break_point_hit_count = 0;
  for (int i = 0; i < 10; i++) {
    f->Call(env->Global(), 0, NULL);
  }
  CHECK_EQ(5, break_point_hit_count);

  // Reload the script and get f again checking that the condition survives.
  v8::Script::Compile(script, &origin)->Run();
  f = v8::Local<v8::Function>::Cast(
      env->Global()->Get(v8::String::NewFromUtf8(env->GetIsolate(), "\x66")));

  break_point_hit_count = 0;
  for (int i = 0; i < 10; i++) {
    f->Call(env->Global(), 0, NULL);
  }
  CHECK_EQ(5, break_point_hit_count);

  v8::Debug::SetDebugEventListener(NULL);
  CheckDebuggerUnloaded();
}


// Test ignore count on script break points.
TEST(ScriptBreakPointIgnoreCount) {
  break_point_hit_count = 0;
  DebugLocalContext env;
  v8::HandleScope scope(env->GetIsolate());
  env.ExposeDebug();

  v8::Debug::SetDebugEventListener(DebugEventBreakPointHitCount);

  v8::Local<v8::String> script = v8::String::NewFromUtf8(
    env->GetIsolate(),
    "\x66\x75\x6e\x63\x74\x69\x6f\x6e\x20\x66\x28\x29\x20\x7b\xa"
    "\x20\x20\x61\x20\x3d\x20\x30\x3b\x20\x20\x2f\x2f\x20\x6c\x69\x6e\x65\x20\x31\xa"
    "\x7d\x3b");

  // Compile the script and get function f.
  v8::ScriptOrigin origin =
      v8::ScriptOrigin(v8::String::NewFromUtf8(env->GetIsolate(), "\x74\x65\x73\x74"));
  v8::Script::Compile(script, &origin)->Run();
  v8::Local<v8::Function> f = v8::Local<v8::Function>::Cast(
      env->Global()->Get(v8::String::NewFromUtf8(env->GetIsolate(), "\x66")));

  // Set script break point on line 1 (in function f).
  int sbp = SetScriptBreakPointByNameFromJS(env->GetIsolate(), "\x74\x65\x73\x74", 1, 0);

  // Call f with different ignores on the script break point.
  break_point_hit_count = 0;
  ChangeScriptBreakPointIgnoreCountFromJS(env->GetIsolate(), sbp, 1);
  f->Call(env->Global(), 0, NULL);
  CHECK_EQ(0, break_point_hit_count);
  f->Call(env->Global(), 0, NULL);
  CHECK_EQ(1, break_point_hit_count);

  ChangeScriptBreakPointIgnoreCountFromJS(env->GetIsolate(), sbp, 5);
  break_point_hit_count = 0;
  for (int i = 0; i < 10; i++) {
    f->Call(env->Global(), 0, NULL);
  }
  CHECK_EQ(5, break_point_hit_count);

  // Reload the script and get f again checking that the ignore survives.
  v8::Script::Compile(script, &origin)->Run();
  f = v8::Local<v8::Function>::Cast(
      env->Global()->Get(v8::String::NewFromUtf8(env->GetIsolate(), "\x66")));

  break_point_hit_count = 0;
  for (int i = 0; i < 10; i++) {
    f->Call(env->Global(), 0, NULL);
  }
  CHECK_EQ(5, break_point_hit_count);

  v8::Debug::SetDebugEventListener(NULL);
  CheckDebuggerUnloaded();
}


// Test that script break points survive when a script is reloaded.
TEST(ScriptBreakPointReload) {
  break_point_hit_count = 0;
  DebugLocalContext env;
  v8::HandleScope scope(env->GetIsolate());
  env.ExposeDebug();

  v8::Debug::SetDebugEventListener(DebugEventBreakPointHitCount);

  v8::Local<v8::Function> f;
  v8::Local<v8::String> script = v8::String::NewFromUtf8(
    env->GetIsolate(),
    "\x66\x75\x6e\x63\x74\x69\x6f\x6e\x20\x66\x28\x29\x20\x7b\xa"
    "\x20\x20\x66\x75\x6e\x63\x74\x69\x6f\x6e\x20\x68\x28\x29\x20\x7b\xa"
    "\x20\x20\x20\x20\x61\x20\x3d\x20\x30\x3b\x20\x20\x2f\x2f\x20\x6c\x69\x6e\x65\x20\x32\xa"
    "\x20\x20\x7d\xa"
    "\x20\x20\x62\x20\x3d\x20\x31\x3b\x20\x20\x2f\x2f\x20\x6c\x69\x6e\x65\x20\x34\xa"
    "\x20\x20\x72\x65\x74\x75\x72\x6e\x20\x68\x28\x29\x3b\xa"
    "\x7d");

  v8::ScriptOrigin origin_1 =
      v8::ScriptOrigin(v8::String::NewFromUtf8(env->GetIsolate(), "\x31"));
  v8::ScriptOrigin origin_2 =
      v8::ScriptOrigin(v8::String::NewFromUtf8(env->GetIsolate(), "\x32"));

  // Set a script break point before the script is loaded.
  SetScriptBreakPointByNameFromJS(env->GetIsolate(), "\x31", 2, 0);

  // Compile the script and get the function.
  v8::Script::Compile(script, &origin_1)->Run();
  f = v8::Local<v8::Function>::Cast(
      env->Global()->Get(v8::String::NewFromUtf8(env->GetIsolate(), "\x66")));

  // Call f and check that the script break point is active.
  break_point_hit_count = 0;
  f->Call(env->Global(), 0, NULL);
  CHECK_EQ(1, break_point_hit_count);

  // Compile the script again with a different script data and get the
  // function.
  v8::Script::Compile(script, &origin_2)->Run();
  f = v8::Local<v8::Function>::Cast(
      env->Global()->Get(v8::String::NewFromUtf8(env->GetIsolate(), "\x66")));

  // Call f and check that no break points are set.
  break_point_hit_count = 0;
  f->Call(env->Global(), 0, NULL);
  CHECK_EQ(0, break_point_hit_count);

  // Compile the script again and get the function.
  v8::Script::Compile(script, &origin_1)->Run();
  f = v8::Local<v8::Function>::Cast(
      env->Global()->Get(v8::String::NewFromUtf8(env->GetIsolate(), "\x66")));

  // Call f and check that the script break point is active.
  break_point_hit_count = 0;
  f->Call(env->Global(), 0, NULL);
  CHECK_EQ(1, break_point_hit_count);

  v8::Debug::SetDebugEventListener(NULL);
  CheckDebuggerUnloaded();
}


// Test when several scripts has the same script data
TEST(ScriptBreakPointMultiple) {
  break_point_hit_count = 0;
  DebugLocalContext env;
  v8::HandleScope scope(env->GetIsolate());
  env.ExposeDebug();

  v8::Debug::SetDebugEventListener(DebugEventBreakPointHitCount);

  v8::Local<v8::Function> f;
  v8::Local<v8::String> script_f =
      v8::String::NewFromUtf8(env->GetIsolate(),
                              "\x66\x75\x6e\x63\x74\x69\x6f\x6e\x20\x66\x28\x29\x20\x7b\xa"
                              "\x20\x20\x61\x20\x3d\x20\x30\x3b\x20\x20\x2f\x2f\x20\x6c\x69\x6e\x65\x20\x31\xa"
                              "\x7d");

  v8::Local<v8::Function> g;
  v8::Local<v8::String> script_g =
      v8::String::NewFromUtf8(env->GetIsolate(),
                              "\x66\x75\x6e\x63\x74\x69\x6f\x6e\x20\x67\x28\x29\x20\x7b\xa"
                              "\x20\x20\x62\x20\x3d\x20\x30\x3b\x20\x20\x2f\x2f\x20\x6c\x69\x6e\x65\x20\x31\xa"
                              "\x7d");

  v8::ScriptOrigin origin =
      v8::ScriptOrigin(v8::String::NewFromUtf8(env->GetIsolate(), "\x74\x65\x73\x74"));

  // Set a script break point before the scripts are loaded.
  int sbp = SetScriptBreakPointByNameFromJS(env->GetIsolate(), "\x74\x65\x73\x74", 1, 0);

  // Compile the scripts with same script data and get the functions.
  v8::Script::Compile(script_f, &origin)->Run();
  f = v8::Local<v8::Function>::Cast(
      env->Global()->Get(v8::String::NewFromUtf8(env->GetIsolate(), "\x66")));
  v8::Script::Compile(script_g, &origin)->Run();
  g = v8::Local<v8::Function>::Cast(
      env->Global()->Get(v8::String::NewFromUtf8(env->GetIsolate(), "\x67")));

  // Call f and g and check that the script break point is active.
  break_point_hit_count = 0;
  f->Call(env->Global(), 0, NULL);
  CHECK_EQ(1, break_point_hit_count);
  g->Call(env->Global(), 0, NULL);
  CHECK_EQ(2, break_point_hit_count);

  // Clear the script break point.
  ClearBreakPointFromJS(env->GetIsolate(), sbp);

  // Call f and g and check that the script break point is no longer active.
  break_point_hit_count = 0;
  f->Call(env->Global(), 0, NULL);
  CHECK_EQ(0, break_point_hit_count);
  g->Call(env->Global(), 0, NULL);
  CHECK_EQ(0, break_point_hit_count);

  // Set script break point with the scripts loaded.
  sbp = SetScriptBreakPointByNameFromJS(env->GetIsolate(), "\x74\x65\x73\x74", 1, 0);

  // Call f and g and check that the script break point is active.
  break_point_hit_count = 0;
  f->Call(env->Global(), 0, NULL);
  CHECK_EQ(1, break_point_hit_count);
  g->Call(env->Global(), 0, NULL);
  CHECK_EQ(2, break_point_hit_count);

  v8::Debug::SetDebugEventListener(NULL);
  CheckDebuggerUnloaded();
}


// Test the script origin which has both name and line offset.
TEST(ScriptBreakPointLineOffset) {
  break_point_hit_count = 0;
  DebugLocalContext env;
  v8::HandleScope scope(env->GetIsolate());
  env.ExposeDebug();

  v8::Debug::SetDebugEventListener(DebugEventBreakPointHitCount);

  v8::Local<v8::Function> f;
  v8::Local<v8::String> script = v8::String::NewFromUtf8(
      env->GetIsolate(),
      "\x66\x75\x6e\x63\x74\x69\x6f\x6e\x20\x66\x28\x29\x20\x7b\xa"
      "\x20\x20\x61\x20\x3d\x20\x30\x3b\x20\x20\x2f\x2f\x20\x6c\x69\x6e\x65\x20\x38\x20\x61\x73\x20\x74\x68\x69\x73\x20\x73\x63\x72\x69\x70\x74\x20\x68\x61\x73\x20\x6c\x69\x6e\x65\x20\x6f\x66\x66\x73\x65\x74\x20\x37\xa"
      "\x20\x20\x62\x20\x3d\x20\x30\x3b\x20\x20\x2f\x2f\x20\x6c\x69\x6e\x65\x20\x39\x20\x61\x73\x20\x74\x68\x69\x73\x20\x73\x63\x72\x69\x70\x74\x20\x68\x61\x73\x20\x6c\x69\x6e\x65\x20\x6f\x66\x66\x73\x65\x74\x20\x37\xa"
      "\x7d");

  // Create script origin both name and line offset.
  v8::ScriptOrigin origin(
      v8::String::NewFromUtf8(env->GetIsolate(), "\x74\x65\x73\x74\x2e\x68\x74\x6d\x6c"),
      v8::Integer::New(env->GetIsolate(), 7));

  // Set two script break points before the script is loaded.
  int sbp1 =
      SetScriptBreakPointByNameFromJS(env->GetIsolate(), "\x74\x65\x73\x74\x2e\x68\x74\x6d\x6c", 8, 0);
  int sbp2 =
      SetScriptBreakPointByNameFromJS(env->GetIsolate(), "\x74\x65\x73\x74\x2e\x68\x74\x6d\x6c", 9, 0);

  // Compile the script and get the function.
  v8::Script::Compile(script, &origin)->Run();
  f = v8::Local<v8::Function>::Cast(
      env->Global()->Get(v8::String::NewFromUtf8(env->GetIsolate(), "\x66")));

  // Call f and check that the script break point is active.
  break_point_hit_count = 0;
  f->Call(env->Global(), 0, NULL);
  CHECK_EQ(2, break_point_hit_count);

  // Clear the script break points.
  ClearBreakPointFromJS(env->GetIsolate(), sbp1);
  ClearBreakPointFromJS(env->GetIsolate(), sbp2);

  // Call f and check that no script break points are active.
  break_point_hit_count = 0;
  f->Call(env->Global(), 0, NULL);
  CHECK_EQ(0, break_point_hit_count);

  // Set a script break point with the script loaded.
  sbp1 = SetScriptBreakPointByNameFromJS(env->GetIsolate(), "\x74\x65\x73\x74\x2e\x68\x74\x6d\x6c", 9, 0);

  // Call f and check that the script break point is active.
  break_point_hit_count = 0;
  f->Call(env->Global(), 0, NULL);
  CHECK_EQ(1, break_point_hit_count);

  v8::Debug::SetDebugEventListener(NULL);
  CheckDebuggerUnloaded();
}


// Test script break points set on lines.
TEST(ScriptBreakPointLine) {
  DebugLocalContext env;
  v8::HandleScope scope(env->GetIsolate());
  env.ExposeDebug();

  // Create a function for checking the function when hitting a break point.
  frame_function_name = CompileFunction(&env,
                                        frame_function_name_source,
                                        "\x66\x72\x61\x6d\x65\x5f\x66\x75\x6e\x63\x74\x69\x6f\x6e\x5f\x6e\x61\x6d\x65");

  v8::Debug::SetDebugEventListener(DebugEventBreakPointHitCount);

  v8::Local<v8::Function> f;
  v8::Local<v8::Function> g;
  v8::Local<v8::String> script =
      v8::String::NewFromUtf8(env->GetIsolate(),
                              "\x61\x20\x3d\x20\x30\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x2f\x2f\x20\x6c\x69\x6e\x65\x20\x30\xa"
                              "\x66\x75\x6e\x63\x74\x69\x6f\x6e\x20\x66\x28\x29\x20\x7b\xa"
                              "\x20\x20\x61\x20\x3d\x20\x31\x3b\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x2f\x2f\x20\x6c\x69\x6e\x65\x20\x32\xa"
                              "\x7d\xa"
                              "\x20\x61\x20\x3d\x20\x32\x3b\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x2f\x2f\x20\x6c\x69\x6e\x65\x20\x34\xa"
                              "\x20\x20\x2f\x2a\x20\x78\x78\x20\x2a\x2f\x20\x66\x75\x6e\x63\x74\x69\x6f\x6e\x20\x67\x28\x29\x20\x7b\x20\x20\x2f\x2f\x20\x6c\x69\x6e\x65\x20\x35\xa"
                              "\x20\x20\x20\x20\x66\x75\x6e\x63\x74\x69\x6f\x6e\x20\x68\x28\x29\x20\x7b\x20\x20\x20\x20\x20\x20\x20\x20\x20\x2f\x2f\x20\x6c\x69\x6e\x65\x20\x36\xa"
                              "\x20\x20\x20\x20\x20\x20\x61\x20\x3d\x20\x33\x3b\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x2f\x2f\x20\x6c\x69\x6e\x65\x20\x37\xa"
                              "\x20\x20\x20\x20\x7d\xa"
                              "\x20\x20\x20\x20\x68\x28\x29\x3b\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x2f\x2f\x20\x6c\x69\x6e\x65\x20\x39\xa"
                              "\x20\x20\x20\x20\x61\x20\x3d\x20\x34\x3b\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x2f\x2f\x20\x6c\x69\x6e\x65\x20\x31\x30\xa"
                              "\x20\x20\x7d\xa"
                              "\x20\x61\x3d\x35\x3b\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x2f\x2f\x20\x6c\x69\x6e\x65\x20\x31\x32");

  // Set a couple script break point before the script is loaded.
  int sbp1 =
      SetScriptBreakPointByNameFromJS(env->GetIsolate(), "\x74\x65\x73\x74\x2e\x68\x74\x6d\x6c", 0, -1);
  int sbp2 =
      SetScriptBreakPointByNameFromJS(env->GetIsolate(), "\x74\x65\x73\x74\x2e\x68\x74\x6d\x6c", 1, -1);
  int sbp3 =
      SetScriptBreakPointByNameFromJS(env->GetIsolate(), "\x74\x65\x73\x74\x2e\x68\x74\x6d\x6c", 5, -1);

  // Compile the script and get the function.
  break_point_hit_count = 0;
  v8::ScriptOrigin origin(
      v8::String::NewFromUtf8(env->GetIsolate(), "\x74\x65\x73\x74\x2e\x68\x74\x6d\x6c"),
      v8::Integer::New(env->GetIsolate(), 0));
  v8::Script::Compile(script, &origin)->Run();
  f = v8::Local<v8::Function>::Cast(
      env->Global()->Get(v8::String::NewFromUtf8(env->GetIsolate(), "\x66")));
  g = v8::Local<v8::Function>::Cast(
      env->Global()->Get(v8::String::NewFromUtf8(env->GetIsolate(), "\x67")));

  // Check that a break point was hit when the script was run.
  CHECK_EQ(1, break_point_hit_count);
  CHECK_EQ(0, StrLength(last_function_hit));

  // Call f and check that the script break point.
  f->Call(env->Global(), 0, NULL);
  CHECK_EQ(2, break_point_hit_count);
  CHECK_EQ("\x66", last_function_hit);

  // Call g and check that the script break point.
  g->Call(env->Global(), 0, NULL);
  CHECK_EQ(3, break_point_hit_count);
  CHECK_EQ("\x67", last_function_hit);

  // Clear the script break point on g and set one on h.
  ClearBreakPointFromJS(env->GetIsolate(), sbp3);
  int sbp4 =
      SetScriptBreakPointByNameFromJS(env->GetIsolate(), "\x74\x65\x73\x74\x2e\x68\x74\x6d\x6c", 6, -1);

  // Call g and check that the script break point in h is hit.
  g->Call(env->Global(), 0, NULL);
  CHECK_EQ(4, break_point_hit_count);
  CHECK_EQ("\x68", last_function_hit);

  // Clear break points in f and h. Set a new one in the script between
  // functions f and g and test that there is no break points in f and g any
  // more.
  ClearBreakPointFromJS(env->GetIsolate(), sbp2);
  ClearBreakPointFromJS(env->GetIsolate(), sbp4);
  int sbp5 =
      SetScriptBreakPointByNameFromJS(env->GetIsolate(), "\x74\x65\x73\x74\x2e\x68\x74\x6d\x6c", 4, -1);
  break_point_hit_count = 0;
  f->Call(env->Global(), 0, NULL);
  g->Call(env->Global(), 0, NULL);
  CHECK_EQ(0, break_point_hit_count);

  // Reload the script which should hit two break points.
  break_point_hit_count = 0;
  v8::Script::Compile(script, &origin)->Run();
  CHECK_EQ(2, break_point_hit_count);
  CHECK_EQ(0, StrLength(last_function_hit));

  // Set a break point in the code after the last function decleration.
  int sbp6 =
      SetScriptBreakPointByNameFromJS(env->GetIsolate(), "\x74\x65\x73\x74\x2e\x68\x74\x6d\x6c", 12, -1);

  // Reload the script which should hit three break points.
  break_point_hit_count = 0;
  v8::Script::Compile(script, &origin)->Run();
  CHECK_EQ(3, break_point_hit_count);
  CHECK_EQ(0, StrLength(last_function_hit));

  // Clear the last break points, and reload the script which should not hit any
  // break points.
  ClearBreakPointFromJS(env->GetIsolate(), sbp1);
  ClearBreakPointFromJS(env->GetIsolate(), sbp5);
  ClearBreakPointFromJS(env->GetIsolate(), sbp6);
  break_point_hit_count = 0;
  v8::Script::Compile(script, &origin)->Run();
  CHECK_EQ(0, break_point_hit_count);

  v8::Debug::SetDebugEventListener(NULL);
  CheckDebuggerUnloaded();
}


// Test top level script break points set on lines.
TEST(ScriptBreakPointLineTopLevel) {
  DebugLocalContext env;
  v8::HandleScope scope(env->GetIsolate());
  env.ExposeDebug();

  v8::Debug::SetDebugEventListener(DebugEventBreakPointHitCount);

  v8::Local<v8::String> script =
      v8::String::NewFromUtf8(env->GetIsolate(),
                              "\x66\x75\x6e\x63\x74\x69\x6f\x6e\x20\x66\x28\x29\x20\x7b\xa"
                              "\x20\x20\x61\x20\x3d\x20\x31\x3b\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x2f\x2f\x20\x6c\x69\x6e\x65\x20\x31\xa"
                              "\x7d\xa"
                              "\x61\x20\x3d\x20\x32\x3b\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x2f\x2f\x20\x6c\x69\x6e\x65\x20\x33\xa");
  v8::Local<v8::Function> f;
  {
    v8::HandleScope scope(env->GetIsolate());
    CompileRunWithOrigin(script, "\x74\x65\x73\x74\x2e\x68\x74\x6d\x6c");
  }
  f = v8::Local<v8::Function>::Cast(
      env->Global()->Get(v8::String::NewFromUtf8(env->GetIsolate(), "\x66")));

  CcTest::heap()->CollectAllGarbage(Heap::kNoGCFlags);

  SetScriptBreakPointByNameFromJS(env->GetIsolate(), "\x74\x65\x73\x74\x2e\x68\x74\x6d\x6c", 3, -1);

  // Call f and check that there was no break points.
  break_point_hit_count = 0;
  f->Call(env->Global(), 0, NULL);
  CHECK_EQ(0, break_point_hit_count);

  // Recompile and run script and check that break point was hit.
  break_point_hit_count = 0;
  CompileRunWithOrigin(script, "\x74\x65\x73\x74\x2e\x68\x74\x6d\x6c");
  CHECK_EQ(1, break_point_hit_count);

  // Call f and check that there are still no break points.
  break_point_hit_count = 0;
  f = v8::Local<v8::Function>::Cast(
      env->Global()->Get(v8::String::NewFromUtf8(env->GetIsolate(), "\x66")));
  CHECK_EQ(0, break_point_hit_count);

  v8::Debug::SetDebugEventListener(NULL);
  CheckDebuggerUnloaded();
}


// Test that it is possible to add and remove break points in a top level
// function which has no references but has not been collected yet.
TEST(ScriptBreakPointTopLevelCrash) {
  DebugLocalContext env;
  v8::HandleScope scope(env->GetIsolate());
  env.ExposeDebug();

  v8::Debug::SetDebugEventListener(DebugEventBreakPointHitCount);

  v8::Local<v8::String> script_source =
      v8::String::NewFromUtf8(env->GetIsolate(),
                              "\x66\x75\x6e\x63\x74\x69\x6f\x6e\x20\x66\x28\x29\x20\x7b\xa"
                              "\x20\x20\x72\x65\x74\x75\x72\x6e\x20\x30\x3b\xa"
                              "\x7d\xa"
                              "\x66\x28\x29");

  int sbp1 =
      SetScriptBreakPointByNameFromJS(env->GetIsolate(), "\x74\x65\x73\x74\x2e\x68\x74\x6d\x6c", 3, -1);
  {
    v8::HandleScope scope(env->GetIsolate());
    break_point_hit_count = 0;
    CompileRunWithOrigin(script_source, "\x74\x65\x73\x74\x2e\x68\x74\x6d\x6c");
    CHECK_EQ(1, break_point_hit_count);
  }

  int sbp2 =
      SetScriptBreakPointByNameFromJS(env->GetIsolate(), "\x74\x65\x73\x74\x2e\x68\x74\x6d\x6c", 3, -1);
  ClearBreakPointFromJS(env->GetIsolate(), sbp1);
  ClearBreakPointFromJS(env->GetIsolate(), sbp2);

  v8::Debug::SetDebugEventListener(NULL);
  CheckDebuggerUnloaded();
}


// Test that it is possible to remove the last break point for a function
// inside the break handling of that break point.
TEST(RemoveBreakPointInBreak) {
  DebugLocalContext env;
  v8::HandleScope scope(env->GetIsolate());

  v8::Local<v8::Function> foo =
      CompileFunction(&env, "\x66\x75\x6e\x63\x74\x69\x6f\x6e\x20\x66\x6f\x6f\x28\x29\x7b\x61\x3d\x31\x3b\x7d", "\x66\x6f\x6f");
  debug_event_remove_break_point = SetBreakPoint(foo, 0);

  // Register the debug event listener pasing the function
  v8::Debug::SetDebugEventListener(DebugEventRemoveBreakPoint, foo);

  break_point_hit_count = 0;
  foo->Call(env->Global(), 0, NULL);
  CHECK_EQ(1, break_point_hit_count);

  break_point_hit_count = 0;
  foo->Call(env->Global(), 0, NULL);
  CHECK_EQ(0, break_point_hit_count);

  v8::Debug::SetDebugEventListener(NULL);
  CheckDebuggerUnloaded();
}


// Test that the debugger statement causes a break.
TEST(DebuggerStatement) {
  break_point_hit_count = 0;
  DebugLocalContext env;
  v8::HandleScope scope(env->GetIsolate());
  v8::Debug::SetDebugEventListener(DebugEventBreakPointHitCount);
  v8::Script::Compile(
      v8::String::NewFromUtf8(env->GetIsolate(), "\x66\x75\x6e\x63\x74\x69\x6f\x6e\x20\x62\x61\x72\x28\x29\x7b\x64\x65\x62\x75\x67\x67\x65\x72\x7d"))
      ->Run();
  v8::Script::Compile(
      v8::String::NewFromUtf8(env->GetIsolate(),
                              "\x66\x75\x6e\x63\x74\x69\x6f\x6e\x20\x66\x6f\x6f\x28\x29\x7b\x64\x65\x62\x75\x67\x67\x65\x72\x3b\x64\x65\x62\x75\x67\x67\x65\x72\x3b\x7d"))->Run();
  v8::Local<v8::Function> foo = v8::Local<v8::Function>::Cast(
      env->Global()->Get(v8::String::NewFromUtf8(env->GetIsolate(), "\x66\x6f\x6f")));
  v8::Local<v8::Function> bar = v8::Local<v8::Function>::Cast(
      env->Global()->Get(v8::String::NewFromUtf8(env->GetIsolate(), "\x62\x61\x72")));

  // Run function with debugger statement
  bar->Call(env->Global(), 0, NULL);
  CHECK_EQ(1, break_point_hit_count);

  // Run function with two debugger statement
  foo->Call(env->Global(), 0, NULL);
  CHECK_EQ(3, break_point_hit_count);

  v8::Debug::SetDebugEventListener(NULL);
  CheckDebuggerUnloaded();
}


// Test setting a breakpoint on the debugger statement.
TEST(DebuggerStatementBreakpoint) {
    break_point_hit_count = 0;
    DebugLocalContext env;
    v8::HandleScope scope(env->GetIsolate());
    v8::Debug::SetDebugEventListener(DebugEventBreakPointHitCount);
    v8::Script::Compile(
        v8::String::NewFromUtf8(env->GetIsolate(), "\x66\x75\x6e\x63\x74\x69\x6f\x6e\x20\x66\x6f\x6f\x28\x29\x7b\x64\x65\x62\x75\x67\x67\x65\x72\x3b\x7d"))
        ->Run();
    v8::Local<v8::Function> foo = v8::Local<v8::Function>::Cast(
        env->Global()->Get(v8::String::NewFromUtf8(env->GetIsolate(), "\x66\x6f\x6f")));

    // The debugger statement triggers breakpint hit
    foo->Call(env->Global(), 0, NULL);
    CHECK_EQ(1, break_point_hit_count);

    int bp = SetBreakPoint(foo, 0);

    // Set breakpoint does not duplicate hits
    foo->Call(env->Global(), 0, NULL);
    CHECK_EQ(2, break_point_hit_count);

    ClearBreakPoint(bp);
    v8::Debug::SetDebugEventListener(NULL);
    CheckDebuggerUnloaded();
}


// Test that the evaluation of expressions when a break point is hit generates
// the correct results.
TEST(DebugEvaluate) {
  DebugLocalContext env;
  v8::Isolate* isolate = env->GetIsolate();
  v8::HandleScope scope(isolate);
  env.ExposeDebug();

  // Create a function for checking the evaluation when hitting a break point.
  evaluate_check_function = CompileFunction(&env,
                                            evaluate_check_source,
                                            "\x65\x76\x61\x6c\x75\x61\x74\x65\x5f\x63\x68\x65\x63\x6b");
  // Register the debug event listener
  v8::Debug::SetDebugEventListener(DebugEventEvaluate);

  // Different expected vaules of x and a when in a break point (u = undefined,
  // d = Hello, world!).
  struct EvaluateCheck checks_uu[] = {
    {"\x78", v8::Undefined(isolate)},
    {"\x61", v8::Undefined(isolate)},
    {NULL, v8::Handle<v8::Value>()}
  };
  struct EvaluateCheck checks_hu[] = {
    {"\x78", v8::String::NewFromUtf8(env->GetIsolate(), "\x48\x65\x6c\x6c\x6f\x2c\x20\x77\x6f\x72\x6c\x64\x21")},
    {"\x61", v8::Undefined(isolate)},
    {NULL, v8::Handle<v8::Value>()}
  };
  struct EvaluateCheck checks_hh[] = {
    {"\x78", v8::String::NewFromUtf8(env->GetIsolate(), "\x48\x65\x6c\x6c\x6f\x2c\x20\x77\x6f\x72\x6c\x64\x21")},
    {"\x61", v8::String::NewFromUtf8(env->GetIsolate(), "\x48\x65\x6c\x6c\x6f\x2c\x20\x77\x6f\x72\x6c\x64\x21")},
    {NULL, v8::Handle<v8::Value>()}
  };

  // Simple test function. The "y=0" is in the function foo to provide a break
  // location. For "y=0" the "y" is at position 15 in the barbar function
  // therefore setting breakpoint at position 15 will break at "y=0" and
  // setting it higher will break after.
  v8::Local<v8::Function> foo = CompileFunction(&env,
    "\x66\x75\x6e\x63\x74\x69\x6f\x6e\x20\x66\x6f\x6f\x28\x78\x29\x20\x7b"
    "\x20\x20\x76\x61\x72\x20\x61\x3b"
    "\x20\x20\x79\x3d\x30\x3b"  // To ensure break location 1.
    "\x20\x20\x61\x3d\x78\x3b"
    "\x20\x20\x79\x3d\x30\x3b"  // To ensure break location 2.
    "\x7d",
    "\x66\x6f\x6f");
  const int foo_break_position_1 = 15;
  const int foo_break_position_2 = 29;

  // Arguments with one parameter "Hello, world!"
  v8::Handle<v8::Value> argv_foo[1] = {
      v8::String::NewFromUtf8(env->GetIsolate(), "\x48\x65\x6c\x6c\x6f\x2c\x20\x77\x6f\x72\x6c\x64\x21")};

  // Call foo with breakpoint set before a=x and undefined as parameter.
  int bp = SetBreakPoint(foo, foo_break_position_1);
  checks = checks_uu;
  foo->Call(env->Global(), 0, NULL);

  // Call foo with breakpoint set before a=x and parameter "Hello, world!".
  checks = checks_hu;
  foo->Call(env->Global(), 1, argv_foo);

  // Call foo with breakpoint set after a=x and parameter "Hello, world!".
  ClearBreakPoint(bp);
  SetBreakPoint(foo, foo_break_position_2);
  checks = checks_hh;
  foo->Call(env->Global(), 1, argv_foo);

  // Test function with an inner function. The "y=0" is in function barbar
  // to provide a break location. For "y=0" the "y" is at position 8 in the
  // barbar function therefore setting breakpoint at position 8 will break at
  // "y=0" and setting it higher will break after.
  v8::Local<v8::Function> bar = CompileFunction(&env,
    "\x79\x20\x3d\x20\x30\x3b"
    "\x78\x20\x3d\x20\x27\x47\x6f\x6f\x64\x62\x79\x65\x2c\x20\x77\x6f\x72\x6c\x64\x21\x27\x3b"
    "\x66\x75\x6e\x63\x74\x69\x6f\x6e\x20\x62\x61\x72\x28\x78\x2c\x20\x62\x29\x20\x7b"
    "\x20\x20\x76\x61\x72\x20\x61\x3b"
    "\x20\x20\x66\x75\x6e\x63\x74\x69\x6f\x6e\x20\x62\x61\x72\x62\x61\x72\x28\x29\x20\x7b"
    "\x20\x20\x20\x20\x79\x3d\x30\x3b\x20\x2f\x2a\x20\x54\x6f\x20\x65\x6e\x73\x75\x72\x65\x20\x62\x72\x65\x61\x6b\x20\x6c\x6f\x63\x61\x74\x69\x6f\x6e\x2e\x2a\x2f"
    "\x20\x20\x20\x20\x61\x3d\x78\x3b"
    "\x20\x20\x7d\x3b"
    "\x20\x20\x64\x65\x62\x75\x67\x2e\x44\x65\x62\x75\x67\x2e\x63\x6c\x65\x61\x72\x41\x6c\x6c\x42\x72\x65\x61\x6b\x50\x6f\x69\x6e\x74\x73\x28\x29\x3b"
    "\x20\x20\x62\x61\x72\x62\x61\x72\x28\x29\x3b"
    "\x20\x20\x79\x3d\x30\x3b\x61\x3d\x78\x3b"
    "\x7d",
    "\x62\x61\x72");
  const int barbar_break_position = 8;

  // Call bar setting breakpoint before a=x in barbar and undefined as
  // parameter.
  checks = checks_uu;
  v8::Handle<v8::Value> argv_bar_1[2] = {
    v8::Undefined(isolate),
    v8::Number::New(isolate, barbar_break_position)
  };
  bar->Call(env->Global(), 2, argv_bar_1);

  // Call bar setting breakpoint before a=x in barbar and parameter
  // "Hello, world!".
  checks = checks_hu;
  v8::Handle<v8::Value> argv_bar_2[2] = {
    v8::String::NewFromUtf8(env->GetIsolate(), "\x48\x65\x6c\x6c\x6f\x2c\x20\x77\x6f\x72\x6c\x64\x21"),
    v8::Number::New(env->GetIsolate(), barbar_break_position)
  };
  bar->Call(env->Global(), 2, argv_bar_2);

  // Call bar setting breakpoint after a=x in barbar and parameter
  // "Hello, world!".
  checks = checks_hh;
  v8::Handle<v8::Value> argv_bar_3[2] = {
    v8::String::NewFromUtf8(env->GetIsolate(), "\x48\x65\x6c\x6c\x6f\x2c\x20\x77\x6f\x72\x6c\x64\x21"),
    v8::Number::New(env->GetIsolate(), barbar_break_position + 1)
  };
  bar->Call(env->Global(), 2, argv_bar_3);

  v8::Debug::SetDebugEventListener(NULL);
  CheckDebuggerUnloaded();
}


int debugEventCount = 0;
static void CheckDebugEvent(const v8::Debug::EventDetails& eventDetails) {
  if (eventDetails.GetEvent() == v8::Break) ++debugEventCount;
}


// Test that the conditional breakpoints work event if code generation from
// strings is prohibited in the debugee context.
TEST(ConditionalBreakpointWithCodeGenerationDisallowed) {
  DebugLocalContext env;
  v8::HandleScope scope(env->GetIsolate());
  env.ExposeDebug();

  v8::Debug::SetDebugEventListener(CheckDebugEvent);

  v8::Local<v8::Function> foo = CompileFunction(&env,
    "\x66\x75\x6e\x63\x74\x69\x6f\x6e\x20\x66\x6f\x6f\x28\x78\x29\x20\x7b\xa"
    "\x20\x20\x76\x61\x72\x20\x73\x20\x3d\x20\x27\x53\x74\x72\x69\x6e\x67\x20\x76\x61\x6c\x75\x65\x32\x27\x3b\xa"
    "\x20\x20\x72\x65\x74\x75\x72\x6e\x20\x73\x20\x2b\x20\x78\x3b\xa"
    "\x7d",
    "\x66\x6f\x6f");

  // Set conditional breakpoint with condition 'true'.
  CompileRun("\x64\x65\x62\x75\x67\x2e\x44\x65\x62\x75\x67\x2e\x73\x65\x74\x42\x72\x65\x61\x6b\x50\x6f\x69\x6e\x74\x28\x66\x6f\x6f\x2c\x20\x32\x2c\x20\x30\x2c\x20\x27\x74\x72\x75\x65\x27\x29");

  debugEventCount = 0;
  env->AllowCodeGenerationFromStrings(false);
  foo->Call(env->Global(), 0, NULL);
  CHECK_EQ(1, debugEventCount);

  v8::Debug::SetDebugEventListener(NULL);
  CheckDebuggerUnloaded();
}


bool checkedDebugEvals = true;
v8::Handle<v8::Function> checkGlobalEvalFunction;
v8::Handle<v8::Function> checkFrameEvalFunction;
static void CheckDebugEval(const v8::Debug::EventDetails& eventDetails) {
  if (eventDetails.GetEvent() == v8::Break) {
    ++debugEventCount;
    v8::HandleScope handleScope(CcTest::isolate());

    v8::Handle<v8::Value> args[] = { eventDetails.GetExecutionState() };
    CHECK(checkGlobalEvalFunction->Call(
        eventDetails.GetEventContext()->Global(), 1, args)->IsTrue());
    CHECK(checkFrameEvalFunction->Call(
        eventDetails.GetEventContext()->Global(), 1, args)->IsTrue());
  }
}


// Test that the evaluation of expressions when a break point is hit generates
// the correct results in case code generation from strings is disallowed in the
// debugee context.
TEST(DebugEvaluateWithCodeGenerationDisallowed) {
  DebugLocalContext env;
  v8::HandleScope scope(env->GetIsolate());
  env.ExposeDebug();

  v8::Debug::SetDebugEventListener(CheckDebugEval);

  v8::Local<v8::Function> foo = CompileFunction(&env,
    "\x76\x61\x72\x20\x67\x6c\x6f\x62\x61\x6c\x20\x3d\x20\x27\x47\x6c\x6f\x62\x61\x6c\x27\x3b\xa"
    "\x66\x75\x6e\x63\x74\x69\x6f\x6e\x20\x66\x6f\x6f\x28\x78\x29\x20\x7b\xa"
    "\x20\x20\x76\x61\x72\x20\x6c\x6f\x63\x61\x6c\x20\x3d\x20\x27\x4c\x6f\x63\x61\x6c\x27\x3b\xa"
    "\x20\x20\x64\x65\x62\x75\x67\x67\x65\x72\x3b\xa"
    "\x20\x20\x72\x65\x74\x75\x72\x6e\x20\x6c\x6f\x63\x61\x6c\x20\x2b\x20\x78\x3b\xa"
    "\x7d",
    "\x66\x6f\x6f");
  checkGlobalEvalFunction = CompileFunction(&env,
    "\x66\x75\x6e\x63\x74\x69\x6f\x6e\x20\x63\x68\x65\x63\x6b\x47\x6c\x6f\x62\x61\x6c\x45\x76\x61\x6c\x28\x65\x78\x65\x63\x5f\x73\x74\x61\x74\x65\x29\x20\x7b\xa"
    "\x20\x20\x72\x65\x74\x75\x72\x6e\x20\x65\x78\x65\x63\x5f\x73\x74\x61\x74\x65\x2e\x65\x76\x61\x6c\x75\x61\x74\x65\x47\x6c\x6f\x62\x61\x6c\x28\x27\x67\x6c\x6f\x62\x61\x6c\x27\x29\x2e\x76\x61\x6c\x75\x65\x28\x29\x20\x3d\x3d\x3d\x20\x27\x47\x6c\x6f\x62\x61\x6c\x27\x3b\xa"
    "\x7d",
    "\x63\x68\x65\x63\x6b\x47\x6c\x6f\x62\x61\x6c\x45\x76\x61\x6c");

  checkFrameEvalFunction = CompileFunction(&env,
    "\x66\x75\x6e\x63\x74\x69\x6f\x6e\x20\x63\x68\x65\x63\x6b\x46\x72\x61\x6d\x65\x45\x76\x61\x6c\x28\x65\x78\x65\x63\x5f\x73\x74\x61\x74\x65\x29\x20\x7b\xa"
    "\x20\x20\x72\x65\x74\x75\x72\x6e\x20\x65\x78\x65\x63\x5f\x73\x74\x61\x74\x65\x2e\x66\x72\x61\x6d\x65\x28\x30\x29\x2e\x65\x76\x61\x6c\x75\x61\x74\x65\x28\x27\x6c\x6f\x63\x61\x6c\x27\x29\x2e\x76\x61\x6c\x75\x65\x28\x29\x20\x3d\x3d\x3d\x20\x27\x4c\x6f\x63\x61\x6c\x27\x3b\xa"
    "\x7d",
    "\x63\x68\x65\x63\x6b\x46\x72\x61\x6d\x65\x45\x76\x61\x6c");
  debugEventCount = 0;
  env->AllowCodeGenerationFromStrings(false);
  foo->Call(env->Global(), 0, NULL);
  CHECK_EQ(1, debugEventCount);

  checkGlobalEvalFunction.Clear();
  checkFrameEvalFunction.Clear();
  v8::Debug::SetDebugEventListener(NULL);
  CheckDebuggerUnloaded();
}


// Copies a C string to a 16-bit string.  Does not check for buffer overflow.
// Does not use the V8 engine to convert strings, so it can be used
// in any thread.  Returns the length of the string.
int AsciiToUtf16(const char* input_buffer, uint16_t* output_buffer) {
  int i;
  for (i = 0; input_buffer[i] != '\x0'; ++i) {
    // ASCII does not use chars > 127, but be careful anyway.
    output_buffer[i] = static_cast<unsigned char>(input_buffer[i]);
  }
  output_buffer[i] = 0;
  return i;
}


// Copies a 16-bit string to a C string by dropping the high byte of
// each character.  Does not check for buffer overflow.
// Can be used in any thread.  Requires string length as an input.
int Utf16ToAscii(const uint16_t* input_buffer, int length,
                 char* output_buffer, int output_len = -1) {
  if (output_len >= 0) {
    if (length > output_len - 1) {
      length = output_len - 1;
    }
  }

  for (int i = 0; i < length; ++i) {
    output_buffer[i] = static_cast<char>(input_buffer[i]);
  }
  output_buffer[length] = '\x0';
  return length;
}


// We match parts of the message to get evaluate result int value.
bool GetEvaluateStringResult(char *message, char* buffer, int buffer_size) {
  if (strstr(message, "\x22\x63\x6f\x6d\x6d\x61\x6e\x64\x22\x3a\x22\x65\x76\x61\x6c\x75\x61\x74\x65\x22") == NULL) {
    return false;
  }
  const char* prefix = "\x22\x74\x65\x78\x74\x22\x3a\x22";
  char* pos1 = strstr(message, prefix);
  if (pos1 == NULL) {
    return false;
  }
  pos1 += strlen(prefix);
  char* pos2 = strchr(pos1, '\x22');
  if (pos2 == NULL) {
    return false;
  }
  Vector<char> buf(buffer, buffer_size);
  int len = static_cast<int>(pos2 - pos1);
  if (len > buffer_size - 1) {
    len = buffer_size - 1;
  }
  StrNCpy(buf, pos1, len);
  buffer[buffer_size - 1] = '\x0';
  return true;
}


struct EvaluateResult {
  static const int kBufferSize = 20;
  char buffer[kBufferSize];
};

struct DebugProcessDebugMessagesData {
  static const int kArraySize = 5;
  int counter;
  EvaluateResult results[kArraySize];

  void reset() {
    counter = 0;
  }
  EvaluateResult* current() {
    return &results[counter % kArraySize];
  }
  void next() {
    counter++;
  }
};

DebugProcessDebugMessagesData process_debug_messages_data;

static void DebugProcessDebugMessagesHandler(
    const v8::Debug::Message& message) {
  v8::Handle<v8::String> json = message.GetJSON();
  v8::String::Utf8Value utf8(json);
  EvaluateResult* array_item = process_debug_messages_data.current();

  bool res = GetEvaluateStringResult(*utf8,
                                     array_item->buffer,
                                     EvaluateResult::kBufferSize);
  if (res) {
    process_debug_messages_data.next();
  }
}


// Test that the evaluation of expressions works even from ProcessDebugMessages
// i.e. with empty stack.
TEST(DebugEvaluateWithoutStack) {
  v8::Debug::SetMessageHandler(DebugProcessDebugMessagesHandler);

  DebugLocalContext env;
  v8::HandleScope scope(env->GetIsolate());

  const char* source =
      "\x76\x61\x72\x20\x76\x31\x20\x3d\x20\x27\x50\x69\x6e\x67\x75\x69\x6e\x27\x3b\xa\x20\x66\x75\x6e\x63\x74\x69\x6f\x6e\x20\x67\x65\x74\x41\x6e\x69\x6d\x61\x6c\x28\x29\x20\x7b\x20\x72\x65\x74\x75\x72\x6e\x20\x27\x43\x61\x70\x79\x27\x20\x2b\x20\x27\x62\x61\x72\x61\x27\x3b\x20\x7d";

  v8::Script::Compile(v8::String::NewFromUtf8(env->GetIsolate(), source))
      ->Run();

  v8::Debug::ProcessDebugMessages();

  const int kBufferSize = 1000;
  uint16_t buffer[kBufferSize];

  const char* command_111 = "\x7b\x22\x73\x65\x71\x22\x3a\x31\x31\x31\x2c"
      "\x22\x74\x79\x70\x65\x22\x3a\x22\x72\x65\x71\x75\x65\x73\x74\x22\x2c"
      "\x22\x63\x6f\x6d\x6d\x61\x6e\x64\x22\x3a\x22\x65\x76\x61\x6c\x75\x61\x74\x65\x22\x2c"
      "\x22\x61\x72\x67\x75\x6d\x65\x6e\x74\x73\x22\x3a\x7b"
      "\x20\x20\x20\x20\x22\x67\x6c\x6f\x62\x61\x6c\x22\x3a\x74\x72\x75\x65\x2c"
      "\x20\x20\x20\x20\x22\x65\x78\x70\x72\x65\x73\x73\x69\x6f\x6e\x22\x3a\x22\x76\x31\x22\x2c\x22\x64\x69\x73\x61\x62\x6c\x65\x5f\x62\x72\x65\x61\x6b\x22\x3a\x74\x72\x75\x65"
      "\x7d\x7d";

  v8::Isolate* isolate = CcTest::isolate();
  v8::Debug::SendCommand(isolate, buffer, AsciiToUtf16(command_111, buffer));

  const char* command_112 = "\x7b\x22\x73\x65\x71\x22\x3a\x31\x31\x32\x2c"
      "\x22\x74\x79\x70\x65\x22\x3a\x22\x72\x65\x71\x75\x65\x73\x74\x22\x2c"
      "\x22\x63\x6f\x6d\x6d\x61\x6e\x64\x22\x3a\x22\x65\x76\x61\x6c\x75\x61\x74\x65\x22\x2c"
      "\x22\x61\x72\x67\x75\x6d\x65\x6e\x74\x73\x22\x3a\x7b"
      "\x20\x20\x20\x20\x22\x67\x6c\x6f\x62\x61\x6c\x22\x3a\x74\x72\x75\x65\x2c"
      "\x20\x20\x20\x20\x22\x65\x78\x70\x72\x65\x73\x73\x69\x6f\x6e\x22\x3a\x22\x67\x65\x74\x41\x6e\x69\x6d\x61\x6c\x28\x29\x22\x2c\x22\x64\x69\x73\x61\x62\x6c\x65\x5f\x62\x72\x65\x61\x6b\x22\x3a\x74\x72\x75\x65"
      "\x7d\x7d";

  v8::Debug::SendCommand(isolate, buffer, AsciiToUtf16(command_112, buffer));

  const char* command_113 = "\x7b\x22\x73\x65\x71\x22\x3a\x31\x31\x33\x2c"
     "\x22\x74\x79\x70\x65\x22\x3a\x22\x72\x65\x71\x75\x65\x73\x74\x22\x2c"
     "\x22\x63\x6f\x6d\x6d\x61\x6e\x64\x22\x3a\x22\x65\x76\x61\x6c\x75\x61\x74\x65\x22\x2c"
     "\x22\x61\x72\x67\x75\x6d\x65\x6e\x74\x73\x22\x3a\x7b"
     "\x20\x20\x20\x20\x22\x67\x6c\x6f\x62\x61\x6c\x22\x3a\x74\x72\x75\x65\x2c"
     "\x20\x20\x20\x20\x22\x65\x78\x70\x72\x65\x73\x73\x69\x6f\x6e\x22\x3a\x22\x32\x33\x39\x20\x2b\x20\x35\x36\x36\x22\x2c\x22\x64\x69\x73\x61\x62\x6c\x65\x5f\x62\x72\x65\x61\x6b\x22\x3a\x74\x72\x75\x65"
     "\x7d\x7d";

  v8::Debug::SendCommand(isolate, buffer, AsciiToUtf16(command_113, buffer));

  v8::Debug::ProcessDebugMessages();

  CHECK_EQ(3, process_debug_messages_data.counter);

  CHECK_EQ(strcmp("\x50\x69\x6e\x67\x75\x69\x6e", process_debug_messages_data.results[0].buffer), 0);
  CHECK_EQ(strcmp("\x43\x61\x70\x79\x62\x61\x72\x61", process_debug_messages_data.results[1].buffer),
           0);
  CHECK_EQ(strcmp("\x38\x30\x35", process_debug_messages_data.results[2].buffer), 0);

  v8::Debug::SetMessageHandler(NULL);
  v8::Debug::SetDebugEventListener(NULL);
  CheckDebuggerUnloaded();
}


// Simple test of the stepping mechanism using only store ICs.
TEST(DebugStepLinear) {
  DebugLocalContext env;
  v8::HandleScope scope(env->GetIsolate());

  // Create a function for testing stepping.
  v8::Local<v8::Function> foo = CompileFunction(&env,
                                                "\x66\x75\x6e\x63\x74\x69\x6f\x6e\x20\x66\x6f\x6f\x28\x29\x7b\x61\x3d\x31\x3b\x62\x3d\x31\x3b\x63\x3d\x31\x3b\x7d",
                                                "\x66\x6f\x6f");

  // Run foo to allow it to get optimized.
  CompileRun("\x61\x3d\x30\x3b\x20\x62\x3d\x30\x3b\x20\x63\x3d\x30\x3b\x20\x66\x6f\x6f\x28\x29\x3b");

  SetBreakPoint(foo, 3);

  // Register a debug event listener which steps and counts.
  v8::Debug::SetDebugEventListener(DebugEventStep);

  step_action = StepIn;
  break_point_hit_count = 0;
  foo->Call(env->Global(), 0, NULL);

  // With stepping all break locations are hit.
  CHECK_EQ(4, break_point_hit_count);

  v8::Debug::SetDebugEventListener(NULL);
  CheckDebuggerUnloaded();

  // Register a debug event listener which just counts.
  v8::Debug::SetDebugEventListener(DebugEventBreakPointHitCount);

  SetBreakPoint(foo, 3);
  break_point_hit_count = 0;
  foo->Call(env->Global(), 0, NULL);

  // Without stepping only active break points are hit.
  CHECK_EQ(1, break_point_hit_count);

  v8::Debug::SetDebugEventListener(NULL);
  CheckDebuggerUnloaded();
}


// Test of the stepping mechanism for keyed load in a loop.
TEST(DebugStepKeyedLoadLoop) {
  DebugLocalContext env;
  v8::HandleScope scope(env->GetIsolate());

  // Register a debug event listener which steps and counts.
  v8::Debug::SetDebugEventListener(DebugEventStep);

  // Create a function for testing stepping of keyed load. The statement 'y=1'
  // is there to have more than one breakable statement in the loop, TODO(315).
  v8::Local<v8::Function> foo = CompileFunction(
      &env,
      "\x66\x75\x6e\x63\x74\x69\x6f\x6e\x20\x66\x6f\x6f\x28\x61\x29\x20\x7b\xa"
      "\x20\x20\x76\x61\x72\x20\x78\x3b\xa"
      "\x20\x20\x76\x61\x72\x20\x6c\x65\x6e\x20\x3d\x20\x61\x2e\x6c\x65\x6e\x67\x74\x68\x3b\xa"
      "\x20\x20\x66\x6f\x72\x20\x28\x76\x61\x72\x20\x69\x20\x3d\x20\x30\x3b\x20\x69\x20\x3c\x20\x6c\x65\x6e\x3b\x20\x69\x2b\x2b\x29\x20\x7b\xa"
      "\x20\x20\x20\x20\x79\x20\x3d\x20\x31\x3b\xa"
      "\x20\x20\x20\x20\x78\x20\x3d\x20\x61\x5b\x69\x5d\x3b\xa"
      "\x20\x20\x7d\xa"
      "\x7d\xa"
      "\x79\x3d\x30\xa",
      "\x66\x6f\x6f");

  // Create array [0,1,2,3,4,5,6,7,8,9]
  v8::Local<v8::Array> a = v8::Array::New(env->GetIsolate(), 10);
  for (int i = 0; i < 10; i++) {
    a->Set(v8::Number::New(env->GetIsolate(), i),
           v8::Number::New(env->GetIsolate(), i));
  }

  // Call function without any break points to ensure inlining is in place.
  const int kArgc = 1;
  v8::Handle<v8::Value> args[kArgc] = { a };
  foo->Call(env->Global(), kArgc, args);

  // Set up break point and step through the function.
  SetBreakPoint(foo, 3);
  step_action = StepNext;
  break_point_hit_count = 0;
  foo->Call(env->Global(), kArgc, args);

  // With stepping all break locations are hit.
  CHECK_EQ(35, break_point_hit_count);

  v8::Debug::SetDebugEventListener(NULL);
  CheckDebuggerUnloaded();
}


// Test of the stepping mechanism for keyed store in a loop.
TEST(DebugStepKeyedStoreLoop) {
  DebugLocalContext env;
  v8::HandleScope scope(env->GetIsolate());

  // Register a debug event listener which steps and counts.
  v8::Debug::SetDebugEventListener(DebugEventStep);

  // Create a function for testing stepping of keyed store. The statement 'y=1'
  // is there to have more than one breakable statement in the loop, TODO(315).
  v8::Local<v8::Function> foo = CompileFunction(
      &env,
      "\x66\x75\x6e\x63\x74\x69\x6f\x6e\x20\x66\x6f\x6f\x28\x61\x29\x20\x7b\xa"
      "\x20\x20\x76\x61\x72\x20\x6c\x65\x6e\x20\x3d\x20\x61\x2e\x6c\x65\x6e\x67\x74\x68\x3b\xa"
      "\x20\x20\x66\x6f\x72\x20\x28\x76\x61\x72\x20\x69\x20\x3d\x20\x30\x3b\x20\x69\x20\x3c\x20\x6c\x65\x6e\x3b\x20\x69\x2b\x2b\x29\x20\x7b\xa"
      "\x20\x20\x20\x20\x79\x20\x3d\x20\x31\x3b\xa"
      "\x20\x20\x20\x20\x61\x5b\x69\x5d\x20\x3d\x20\x34\x32\x3b\xa"
      "\x20\x20\x7d\xa"
      "\x7d\xa"
      "\x79\x3d\x30\xa",
      "\x66\x6f\x6f");

  // Create array [0,1,2,3,4,5,6,7,8,9]
  v8::Local<v8::Array> a = v8::Array::New(env->GetIsolate(), 10);
  for (int i = 0; i < 10; i++) {
    a->Set(v8::Number::New(env->GetIsolate(), i),
           v8::Number::New(env->GetIsolate(), i));
  }

  // Call function without any break points to ensure inlining is in place.
  const int kArgc = 1;
  v8::Handle<v8::Value> args[kArgc] = { a };
  foo->Call(env->Global(), kArgc, args);

  // Set up break point and step through the function.
  SetBreakPoint(foo, 3);
  step_action = StepNext;
  break_point_hit_count = 0;
  foo->Call(env->Global(), kArgc, args);

  // With stepping all break locations are hit.
  CHECK_EQ(34, break_point_hit_count);

  v8::Debug::SetDebugEventListener(NULL);
  CheckDebuggerUnloaded();
}


// Test of the stepping mechanism for named load in a loop.
TEST(DebugStepNamedLoadLoop) {
  DebugLocalContext env;
  v8::HandleScope scope(env->GetIsolate());

  // Register a debug event listener which steps and counts.
  v8::Debug::SetDebugEventListener(DebugEventStep);

  // Create a function for testing stepping of named load.
  v8::Local<v8::Function> foo = CompileFunction(
      &env,
      "\x66\x75\x6e\x63\x74\x69\x6f\x6e\x20\x66\x6f\x6f\x28\x29\x20\x7b\xa"
          "\x20\x20\x76\x61\x72\x20\x61\x20\x3d\x20\x5b\x5d\x3b\xa"
          "\x20\x20\x76\x61\x72\x20\x73\x20\x3d\x20\x22\x22\x3b\xa"
          "\x20\x20\x66\x6f\x72\x20\x28\x76\x61\x72\x20\x69\x20\x3d\x20\x30\x3b\x20\x69\x20\x3c\x20\x31\x30\x3b\x20\x69\x2b\x2b\x29\x20\x7b\xa"
          "\x20\x20\x20\x20\x76\x61\x72\x20\x76\x20\x3d\x20\x6e\x65\x77\x20\x56\x28\x69\x2c\x20\x69\x20\x2b\x20\x31\x29\x3b\xa"
          "\x20\x20\x20\x20\x76\x2e\x79\x3b\xa"
          "\x20\x20\x20\x20\x61\x2e\x6c\x65\x6e\x67\x74\x68\x3b\xa"  // Special case: array length.
          "\x20\x20\x20\x20\x73\x2e\x6c\x65\x6e\x67\x74\x68\x3b\xa"  // Special case: string length.
          "\x20\x20\x7d\xa"
          "\x7d\xa"
          "\x66\x75\x6e\x63\x74\x69\x6f\x6e\x20\x56\x28\x78\x2c\x20\x79\x29\x20\x7b\xa"
          "\x20\x20\x74\x68\x69\x73\x2e\x78\x20\x3d\x20\x78\x3b\xa"
          "\x20\x20\x74\x68\x69\x73\x2e\x79\x20\x3d\x20\x79\x3b\xa"
          "\x7d\xa",
          "\x66\x6f\x6f");

  // Call function without any break points to ensure inlining is in place.
  foo->Call(env->Global(), 0, NULL);

  // Set up break point and step through the function.
  SetBreakPoint(foo, 4);
  step_action = StepNext;
  break_point_hit_count = 0;
  foo->Call(env->Global(), 0, NULL);

  // With stepping all break locations are hit.
  CHECK_EQ(55, break_point_hit_count);

  v8::Debug::SetDebugEventListener(NULL);
  CheckDebuggerUnloaded();
}


static void DoDebugStepNamedStoreLoop(int expected) {
  DebugLocalContext env;
  v8::HandleScope scope(env->GetIsolate());

  // Register a debug event listener which steps and counts.
  v8::Debug::SetDebugEventListener(DebugEventStep);

  // Create a function for testing stepping of named store.
  v8::Local<v8::Function> foo = CompileFunction(
      &env,
      "\x66\x75\x6e\x63\x74\x69\x6f\x6e\x20\x66\x6f\x6f\x28\x29\x20\x7b\xa"
          "\x20\x20\x76\x61\x72\x20\x61\x20\x3d\x20\x7b\x61\x3a\x31\x7d\x3b\xa"
          "\x20\x20\x66\x6f\x72\x20\x28\x76\x61\x72\x20\x69\x20\x3d\x20\x30\x3b\x20\x69\x20\x3c\x20\x31\x30\x3b\x20\x69\x2b\x2b\x29\x20\x7b\xa"
          "\x20\x20\x20\x20\x61\x2e\x61\x20\x3d\x20\x32\xa"
          "\x20\x20\x7d\xa"
          "\x7d\xa",
          "\x66\x6f\x6f");

  // Call function without any break points to ensure inlining is in place.
  foo->Call(env->Global(), 0, NULL);

  // Set up break point and step through the function.
  SetBreakPoint(foo, 3);
  step_action = StepNext;
  break_point_hit_count = 0;
  foo->Call(env->Global(), 0, NULL);

  // With stepping all expected break locations are hit.
  CHECK_EQ(expected, break_point_hit_count);

  v8::Debug::SetDebugEventListener(NULL);
  CheckDebuggerUnloaded();
}


// Test of the stepping mechanism for named load in a loop.
TEST(DebugStepNamedStoreLoop) {
  DoDebugStepNamedStoreLoop(24);
}


// Test the stepping mechanism with different ICs.
TEST(DebugStepLinearMixedICs) {
  DebugLocalContext env;
  v8::HandleScope scope(env->GetIsolate());

  // Register a debug event listener which steps and counts.
  v8::Debug::SetDebugEventListener(DebugEventStep);

  // Create a function for testing stepping.
  v8::Local<v8::Function> foo = CompileFunction(&env,
      "\x66\x75\x6e\x63\x74\x69\x6f\x6e\x20\x62\x61\x72\x28\x29\x20\x7b\x7d\x3b"
      "\x66\x75\x6e\x63\x74\x69\x6f\x6e\x20\x66\x6f\x6f\x28\x29\x20\x7b"
      "\x20\x20\x76\x61\x72\x20\x78\x3b"
      "\x20\x20\x76\x61\x72\x20\x69\x6e\x64\x65\x78\x3d\x27\x6e\x61\x6d\x65\x27\x3b"
      "\x20\x20\x76\x61\x72\x20\x79\x20\x3d\x20\x7b\x7d\x3b"
      "\x20\x20\x61\x3d\x31\x3b\x62\x3d\x32\x3b\x78\x3d\x61\x3b\x79\x5b\x69\x6e\x64\x65\x78\x5d\x3d\x33\x3b\x78\x3d\x79\x5b\x69\x6e\x64\x65\x78\x5d\x3b\x62\x61\x72\x28\x29\x3b\x7d", "\x66\x6f\x6f");

  // Run functions to allow them to get optimized.
  CompileRun("\x61\x3d\x30\x3b\x20\x62\x3d\x30\x3b\x20\x62\x61\x72\x28\x29\x3b\x20\x66\x6f\x6f\x28\x29\x3b");

  SetBreakPoint(foo, 0);

  step_action = StepIn;
  break_point_hit_count = 0;
  foo->Call(env->Global(), 0, NULL);

  // With stepping all break locations are hit.
  CHECK_EQ(11, break_point_hit_count);

  v8::Debug::SetDebugEventListener(NULL);
  CheckDebuggerUnloaded();

  // Register a debug event listener which just counts.
  v8::Debug::SetDebugEventListener(DebugEventBreakPointHitCount);

  SetBreakPoint(foo, 0);
  break_point_hit_count = 0;
  foo->Call(env->Global(), 0, NULL);

  // Without stepping only active break points are hit.
  CHECK_EQ(1, break_point_hit_count);

  v8::Debug::SetDebugEventListener(NULL);
  CheckDebuggerUnloaded();
}


TEST(DebugStepDeclarations) {
  DebugLocalContext env;
  v8::HandleScope scope(env->GetIsolate());

  // Register a debug event listener which steps and counts.
  v8::Debug::SetDebugEventListener(DebugEventStep);

  // Create a function for testing stepping. Run it to allow it to get
  // optimized.
  const char* src = "\x66\x75\x6e\x63\x74\x69\x6f\x6e\x20\x66\x6f\x6f\x28\x29\x20\x7b\x20"
                    "\x20\x20\x76\x61\x72\x20\x61\x3b"
                    "\x20\x20\x76\x61\x72\x20\x62\x20\x3d\x20\x31\x3b"
                    "\x20\x20\x76\x61\x72\x20\x63\x20\x3d\x20\x66\x6f\x6f\x3b"
                    "\x20\x20\x76\x61\x72\x20\x64\x20\x3d\x20\x4d\x61\x74\x68\x2e\x66\x6c\x6f\x6f\x72\x3b"
                    "\x20\x20\x76\x61\x72\x20\x65\x20\x3d\x20\x62\x20\x2b\x20\x64\x28\x31\x2e\x32\x29\x3b"
                    "\x7d"
                    "\x66\x6f\x6f\x28\x29";
  v8::Local<v8::Function> foo = CompileFunction(&env, src, "\x66\x6f\x6f");

  SetBreakPoint(foo, 0);

  // Stepping through the declarations.
  step_action = StepIn;
  break_point_hit_count = 0;
  foo->Call(env->Global(), 0, NULL);
  CHECK_EQ(6, break_point_hit_count);

  // Get rid of the debug event listener.
  v8::Debug::SetDebugEventListener(NULL);
  CheckDebuggerUnloaded();
}


TEST(DebugStepLocals) {
  DebugLocalContext env;
  v8::HandleScope scope(env->GetIsolate());

  // Register a debug event listener which steps and counts.
  v8::Debug::SetDebugEventListener(DebugEventStep);

  // Create a function for testing stepping. Run it to allow it to get
  // optimized.
  const char* src = "\x66\x75\x6e\x63\x74\x69\x6f\x6e\x20\x66\x6f\x6f\x28\x29\x20\x7b\x20"
                    "\x20\x20\x76\x61\x72\x20\x61\x2c\x62\x3b"
                    "\x20\x20\x61\x20\x3d\x20\x31\x3b"
                    "\x20\x20\x62\x20\x3d\x20\x61\x20\x2b\x20\x32\x3b"
                    "\x20\x20\x62\x20\x3d\x20\x31\x20\x2b\x20\x32\x20\x2b\x20\x33\x3b"
                    "\x20\x20\x61\x20\x3d\x20\x4d\x61\x74\x68\x2e\x66\x6c\x6f\x6f\x72\x28\x62\x29\x3b"
                    "\x7d"
                    "\x66\x6f\x6f\x28\x29";
  v8::Local<v8::Function> foo = CompileFunction(&env, src, "\x66\x6f\x6f");

  SetBreakPoint(foo, 0);

  // Stepping through the declarations.
  step_action = StepIn;
  break_point_hit_count = 0;
  foo->Call(env->Global(), 0, NULL);
  CHECK_EQ(6, break_point_hit_count);

  // Get rid of the debug event listener.
  v8::Debug::SetDebugEventListener(NULL);
  CheckDebuggerUnloaded();
}


TEST(DebugStepIf) {
  DebugLocalContext env;
  v8::Isolate* isolate = env->GetIsolate();
  v8::HandleScope scope(isolate);

  // Register a debug event listener which steps and counts.
  v8::Debug::SetDebugEventListener(DebugEventStep);

  // Create a function for testing stepping. Run it to allow it to get
  // optimized.
  const int argc = 1;
  const char* src = "\x66\x75\x6e\x63\x74\x69\x6f\x6e\x20\x66\x6f\x6f\x28\x78\x29\x20\x7b\x20"
                    "\x20\x20\x61\x20\x3d\x20\x31\x3b"
                    "\x20\x20\x69\x66\x20\x28\x78\x29\x20\x7b"
                    "\x20\x20\x20\x20\x62\x20\x3d\x20\x31\x3b"
                    "\x20\x20\x7d\x20\x65\x6c\x73\x65\x20\x7b"
                    "\x20\x20\x20\x20\x63\x20\x3d\x20\x31\x3b"
                    "\x20\x20\x20\x20\x64\x20\x3d\x20\x31\x3b"
                    "\x20\x20\x7d"
                    "\x7d"
                    "\x61\x3d\x30\x3b\x20\x62\x3d\x30\x3b\x20\x63\x3d\x30\x3b\x20\x64\x3d\x30\x3b\x20\x66\x6f\x6f\x28\x29";
  v8::Local<v8::Function> foo = CompileFunction(&env, src, "\x66\x6f\x6f");
  SetBreakPoint(foo, 0);

  // Stepping through the true part.
  step_action = StepIn;
  break_point_hit_count = 0;
  v8::Handle<v8::Value> argv_true[argc] = { v8::True(isolate) };
  foo->Call(env->Global(), argc, argv_true);
  CHECK_EQ(4, break_point_hit_count);

  // Stepping through the false part.
  step_action = StepIn;
  break_point_hit_count = 0;
  v8::Handle<v8::Value> argv_false[argc] = { v8::False(isolate) };
  foo->Call(env->Global(), argc, argv_false);
  CHECK_EQ(5, break_point_hit_count);

  // Get rid of the debug event listener.
  v8::Debug::SetDebugEventListener(NULL);
  CheckDebuggerUnloaded();
}


TEST(DebugStepSwitch) {
  DebugLocalContext env;
  v8::Isolate* isolate = env->GetIsolate();
  v8::HandleScope scope(isolate);

  // Register a debug event listener which steps and counts.
  v8::Debug::SetDebugEventListener(DebugEventStep);

  // Create a function for testing stepping. Run it to allow it to get
  // optimized.
  const int argc = 1;
  const char* src = "\x66\x75\x6e\x63\x74\x69\x6f\x6e\x20\x66\x6f\x6f\x28\x78\x29\x20\x7b\x20"
                    "\x20\x20\x61\x20\x3d\x20\x31\x3b"
                    "\x20\x20\x73\x77\x69\x74\x63\x68\x20\x28\x78\x29\x20\x7b"
                    "\x20\x20\x20\x20\x63\x61\x73\x65\x20\x31\x3a"
                    "\x20\x20\x20\x20\x20\x20\x62\x20\x3d\x20\x31\x3b"
                    "\x20\x20\x20\x20\x63\x61\x73\x65\x20\x32\x3a"
                    "\x20\x20\x20\x20\x20\x20\x63\x20\x3d\x20\x31\x3b"
                    "\x20\x20\x20\x20\x20\x20\x62\x72\x65\x61\x6b\x3b"
                    "\x20\x20\x20\x20\x63\x61\x73\x65\x20\x33\x3a"
                    "\x20\x20\x20\x20\x20\x20\x64\x20\x3d\x20\x31\x3b"
                    "\x20\x20\x20\x20\x20\x20\x65\x20\x3d\x20\x31\x3b"
                    "\x20\x20\x20\x20\x20\x20\x66\x20\x3d\x20\x31\x3b"
                    "\x20\x20\x20\x20\x20\x20\x62\x72\x65\x61\x6b\x3b"
                    "\x20\x20\x7d"
                    "\x7d"
                    "\x61\x3d\x30\x3b\x20\x62\x3d\x30\x3b\x20\x63\x3d\x30\x3b\x20\x64\x3d\x30\x3b\x20\x65\x3d\x30\x3b\x20\x66\x3d\x30\x3b\x20\x66\x6f\x6f\x28\x29";
  v8::Local<v8::Function> foo = CompileFunction(&env, src, "\x66\x6f\x6f");
  SetBreakPoint(foo, 0);

  // One case with fall-through.
  step_action = StepIn;
  break_point_hit_count = 0;
  v8::Handle<v8::Value> argv_1[argc] = { v8::Number::New(isolate, 1) };
  foo->Call(env->Global(), argc, argv_1);
  CHECK_EQ(6, break_point_hit_count);

  // Another case.
  step_action = StepIn;
  break_point_hit_count = 0;
  v8::Handle<v8::Value> argv_2[argc] = { v8::Number::New(isolate, 2) };
  foo->Call(env->Global(), argc, argv_2);
  CHECK_EQ(5, break_point_hit_count);

  // Last case.
  step_action = StepIn;
  break_point_hit_count = 0;
  v8::Handle<v8::Value> argv_3[argc] = { v8::Number::New(isolate, 3) };
  foo->Call(env->Global(), argc, argv_3);
  CHECK_EQ(7, break_point_hit_count);

  // Get rid of the debug event listener.
  v8::Debug::SetDebugEventListener(NULL);
  CheckDebuggerUnloaded();
}


TEST(DebugStepWhile) {
  DebugLocalContext env;
  v8::Isolate* isolate = env->GetIsolate();
  v8::HandleScope scope(isolate);

  // Register a debug event listener which steps and counts.
  v8::Debug::SetDebugEventListener(DebugEventStep);

  // Create a function for testing stepping. Run it to allow it to get
  // optimized.
  const int argc = 1;
  const char* src = "\x66\x75\x6e\x63\x74\x69\x6f\x6e\x20\x66\x6f\x6f\x28\x78\x29\x20\x7b\x20"
                    "\x20\x20\x76\x61\x72\x20\x61\x20\x3d\x20\x30\x3b"
                    "\x20\x20\x77\x68\x69\x6c\x65\x20\x28\x61\x20\x3c\x20\x78\x29\x20\x7b"
                    "\x20\x20\x20\x20\x61\x2b\x2b\x3b"
                    "\x20\x20\x7d"
                    "\x7d"
                    "\x66\x6f\x6f\x28\x29";
  v8::Local<v8::Function> foo = CompileFunction(&env, src, "\x66\x6f\x6f");
  SetBreakPoint(foo, 8);  // "\x76\x61\x72\x20\x61\x20\x3d\x20\x30\x3b"

  // Looping 0 times.  We still should break at the while-condition once.
  step_action = StepIn;
  break_point_hit_count = 0;
  v8::Handle<v8::Value> argv_0[argc] = { v8::Number::New(isolate, 0) };
  foo->Call(env->Global(), argc, argv_0);
  CHECK_EQ(3, break_point_hit_count);

  // Looping 10 times.
  step_action = StepIn;
  break_point_hit_count = 0;
  v8::Handle<v8::Value> argv_10[argc] = { v8::Number::New(isolate, 10) };
  foo->Call(env->Global(), argc, argv_10);
  CHECK_EQ(23, break_point_hit_count);

  // Looping 100 times.
  step_action = StepIn;
  break_point_hit_count = 0;
  v8::Handle<v8::Value> argv_100[argc] = { v8::Number::New(isolate, 100) };
  foo->Call(env->Global(), argc, argv_100);
  CHECK_EQ(203, break_point_hit_count);

  // Get rid of the debug event listener.
  v8::Debug::SetDebugEventListener(NULL);
  CheckDebuggerUnloaded();
}


TEST(DebugStepDoWhile) {
  DebugLocalContext env;
  v8::Isolate* isolate = env->GetIsolate();
  v8::HandleScope scope(isolate);

  // Register a debug event listener which steps and counts.
  v8::Debug::SetDebugEventListener(DebugEventStep);

  // Create a function for testing stepping. Run it to allow it to get
  // optimized.
  const int argc = 1;
  const char* src = "\x66\x75\x6e\x63\x74\x69\x6f\x6e\x20\x66\x6f\x6f\x28\x78\x29\x20\x7b\x20"
                    "\x20\x20\x76\x61\x72\x20\x61\x20\x3d\x20\x30\x3b"
                    "\x20\x20\x64\x6f\x20\x7b"
                    "\x20\x20\x20\x20\x61\x2b\x2b\x3b"
                    "\x20\x20\x7d\x20\x77\x68\x69\x6c\x65\x20\x28\x61\x20\x3c\x20\x78\x29"
                    "\x7d"
                    "\x66\x6f\x6f\x28\x29";
  v8::Local<v8::Function> foo = CompileFunction(&env, src, "\x66\x6f\x6f");
  SetBreakPoint(foo, 8);  // "\x76\x61\x72\x20\x61\x20\x3d\x20\x30\x3b"

  // Looping 10 times.
  step_action = StepIn;
  break_point_hit_count = 0;
  v8::Handle<v8::Value> argv_10[argc] = { v8::Number::New(isolate, 10) };
  foo->Call(env->Global(), argc, argv_10);
  CHECK_EQ(22, break_point_hit_count);

  // Looping 100 times.
  step_action = StepIn;
  break_point_hit_count = 0;
  v8::Handle<v8::Value> argv_100[argc] = { v8::Number::New(isolate, 100) };
  foo->Call(env->Global(), argc, argv_100);
  CHECK_EQ(202, break_point_hit_count);

  // Get rid of the debug event listener.
  v8::Debug::SetDebugEventListener(NULL);
  CheckDebuggerUnloaded();
}


TEST(DebugStepFor) {
  DebugLocalContext env;
  v8::Isolate* isolate = env->GetIsolate();
  v8::HandleScope scope(isolate);

  // Register a debug event listener which steps and counts.
  v8::Debug::SetDebugEventListener(DebugEventStep);

  // Create a function for testing stepping. Run it to allow it to get
  // optimized.
  const int argc = 1;
  const char* src = "\x66\x75\x6e\x63\x74\x69\x6f\x6e\x20\x66\x6f\x6f\x28\x78\x29\x20\x7b\x20"
                    "\x20\x20\x61\x20\x3d\x20\x31\x3b"
                    "\x20\x20\x66\x6f\x72\x20\x28\x69\x20\x3d\x20\x30\x3b\x20\x69\x20\x3c\x20\x78\x3b\x20\x69\x2b\x2b\x29\x20\x7b"
                    "\x20\x20\x20\x20\x62\x20\x3d\x20\x31\x3b"
                    "\x20\x20\x7d"
                    "\x7d"
                    "\x61\x3d\x30\x3b\x20\x62\x3d\x30\x3b\x20\x69\x3d\x30\x3b\x20\x66\x6f\x6f\x28\x29";
  v8::Local<v8::Function> foo = CompileFunction(&env, src, "\x66\x6f\x6f");

  SetBreakPoint(foo, 8);  // "\x61\x20\x3d\x20\x31\x3b"

  // Looping 10 times.
  step_action = StepIn;
  break_point_hit_count = 0;
  v8::Handle<v8::Value> argv_10[argc] = { v8::Number::New(isolate, 10) };
  foo->Call(env->Global(), argc, argv_10);
  CHECK_EQ(23, break_point_hit_count);

  // Looping 100 times.
  step_action = StepIn;
  break_point_hit_count = 0;
  v8::Handle<v8::Value> argv_100[argc] = { v8::Number::New(isolate, 100) };
  foo->Call(env->Global(), argc, argv_100);
  CHECK_EQ(203, break_point_hit_count);

  // Get rid of the debug event listener.
  v8::Debug::SetDebugEventListener(NULL);
  CheckDebuggerUnloaded();
}


TEST(DebugStepForContinue) {
  DebugLocalContext env;
  v8::Isolate* isolate = env->GetIsolate();
  v8::HandleScope scope(isolate);

  // Register a debug event listener which steps and counts.
  v8::Debug::SetDebugEventListener(DebugEventStep);

  // Create a function for testing stepping. Run it to allow it to get
  // optimized.
  const int argc = 1;
  const char* src = "\x66\x75\x6e\x63\x74\x69\x6f\x6e\x20\x66\x6f\x6f\x28\x78\x29\x20\x7b\x20"
                    "\x20\x20\x76\x61\x72\x20\x61\x20\x3d\x20\x30\x3b"
                    "\x20\x20\x76\x61\x72\x20\x62\x20\x3d\x20\x30\x3b"
                    "\x20\x20\x76\x61\x72\x20\x63\x20\x3d\x20\x30\x3b"
                    "\x20\x20\x66\x6f\x72\x20\x28\x76\x61\x72\x20\x69\x20\x3d\x20\x30\x3b\x20\x69\x20\x3c\x20\x78\x3b\x20\x69\x2b\x2b\x29\x20\x7b"
                    "\x20\x20\x20\x20\x61\x2b\x2b\x3b"
                    "\x20\x20\x20\x20\x69\x66\x20\x28\x61\x20\x25\x20\x32\x20\x3d\x3d\x20\x30\x29\x20\x63\x6f\x6e\x74\x69\x6e\x75\x65\x3b"
                    "\x20\x20\x20\x20\x62\x2b\x2b\x3b"
                    "\x20\x20\x20\x20\x63\x2b\x2b\x3b"
                    "\x20\x20\x7d"
                    "\x20\x20\x72\x65\x74\x75\x72\x6e\x20\x62\x3b"
                    "\x7d"
                    "\x66\x6f\x6f\x28\x29";
  v8::Local<v8::Function> foo = CompileFunction(&env, src, "\x66\x6f\x6f");
  v8::Handle<v8::Value> result;
  SetBreakPoint(foo, 8);  // "\x76\x61\x72\x20\x61\x20\x3d\x20\x30\x3b"

  // Each loop generates 4 or 5 steps depending on whether a is equal.

  // Looping 10 times.
  step_action = StepIn;
  break_point_hit_count = 0;
  v8::Handle<v8::Value> argv_10[argc] = { v8::Number::New(isolate, 10) };
  result = foo->Call(env->Global(), argc, argv_10);
  CHECK_EQ(5, result->Int32Value());
  CHECK_EQ(52, break_point_hit_count);

  // Looping 100 times.
  step_action = StepIn;
  break_point_hit_count = 0;
  v8::Handle<v8::Value> argv_100[argc] = { v8::Number::New(isolate, 100) };
  result = foo->Call(env->Global(), argc, argv_100);
  CHECK_EQ(50, result->Int32Value());
  CHECK_EQ(457, break_point_hit_count);

  // Get rid of the debug event listener.
  v8::Debug::SetDebugEventListener(NULL);
  CheckDebuggerUnloaded();
}


TEST(DebugStepForBreak) {
  DebugLocalContext env;
  v8::Isolate* isolate = env->GetIsolate();
  v8::HandleScope scope(isolate);

  // Register a debug event listener which steps and counts.
  v8::Debug::SetDebugEventListener(DebugEventStep);

  // Create a function for testing stepping. Run it to allow it to get
  // optimized.
  const int argc = 1;
  const char* src = "\x66\x75\x6e\x63\x74\x69\x6f\x6e\x20\x66\x6f\x6f\x28\x78\x29\x20\x7b\x20"
                    "\x20\x20\x76\x61\x72\x20\x61\x20\x3d\x20\x30\x3b"
                    "\x20\x20\x76\x61\x72\x20\x62\x20\x3d\x20\x30\x3b"
                    "\x20\x20\x76\x61\x72\x20\x63\x20\x3d\x20\x30\x3b"
                    "\x20\x20\x66\x6f\x72\x20\x28\x76\x61\x72\x20\x69\x20\x3d\x20\x30\x3b\x20\x69\x20\x3c\x20\x31\x30\x30\x30\x3b\x20\x69\x2b\x2b\x29\x20\x7b"
                    "\x20\x20\x20\x20\x61\x2b\x2b\x3b"
                    "\x20\x20\x20\x20\x69\x66\x20\x28\x61\x20\x3d\x3d\x20\x78\x29\x20\x62\x72\x65\x61\x6b\x3b"
                    "\x20\x20\x20\x20\x62\x2b\x2b\x3b"
                    "\x20\x20\x20\x20\x63\x2b\x2b\x3b"
                    "\x20\x20\x7d"
                    "\x20\x20\x72\x65\x74\x75\x72\x6e\x20\x62\x3b"
                    "\x7d"
                    "\x66\x6f\x6f\x28\x29";
  v8::Local<v8::Function> foo = CompileFunction(&env, src, "\x66\x6f\x6f");
  v8::Handle<v8::Value> result;
  SetBreakPoint(foo, 8);  // "\x76\x61\x72\x20\x61\x20\x3d\x20\x30\x3b"

  // Each loop generates 5 steps except for the last (when break is executed)
  // which only generates 4.

  // Looping 10 times.
  step_action = StepIn;
  break_point_hit_count = 0;
  v8::Handle<v8::Value> argv_10[argc] = { v8::Number::New(isolate, 10) };
  result = foo->Call(env->Global(), argc, argv_10);
  CHECK_EQ(9, result->Int32Value());
  CHECK_EQ(55, break_point_hit_count);

  // Looping 100 times.
  step_action = StepIn;
  break_point_hit_count = 0;
  v8::Handle<v8::Value> argv_100[argc] = { v8::Number::New(isolate, 100) };
  result = foo->Call(env->Global(), argc, argv_100);
  CHECK_EQ(99, result->Int32Value());
  CHECK_EQ(505, break_point_hit_count);

  // Get rid of the debug event listener.
  v8::Debug::SetDebugEventListener(NULL);
  CheckDebuggerUnloaded();
}


TEST(DebugStepForIn) {
  DebugLocalContext env;
  v8::HandleScope scope(env->GetIsolate());

  // Register a debug event listener which steps and counts.
  v8::Debug::SetDebugEventListener(DebugEventStep);

  // Create a function for testing stepping. Run it to allow it to get
  // optimized.
  v8::Local<v8::Function> foo;
  const char* src_1 = "\x66\x75\x6e\x63\x74\x69\x6f\x6e\x20\x66\x6f\x6f\x28\x29\x20\x7b\x20"
                      "\x20\x20\x76\x61\x72\x20\x61\x20\x3d\x20\x5b\x31\x2c\x20\x32\x5d\x3b"
                      "\x20\x20\x66\x6f\x72\x20\x28\x78\x20\x69\x6e\x20\x61\x29\x20\x7b"
                      "\x20\x20\x20\x20\x62\x20\x3d\x20\x30\x3b"
                      "\x20\x20\x7d"
                      "\x7d"
                      "\x66\x6f\x6f\x28\x29";
  foo = CompileFunction(&env, src_1, "\x66\x6f\x6f");
  SetBreakPoint(foo, 0);  // "\x76\x61\x72\x20\x61\x20\x3d\x20\x2e\x2e\x2e"

  step_action = StepIn;
  break_point_hit_count = 0;
  foo->Call(env->Global(), 0, NULL);
  CHECK_EQ(6, break_point_hit_count);

  // Create a function for testing stepping. Run it to allow it to get
  // optimized.
  const char* src_2 = "\x66\x75\x6e\x63\x74\x69\x6f\x6e\x20\x66\x6f\x6f\x28\x29\x20\x7b\x20"
                      "\x20\x20\x76\x61\x72\x20\x61\x20\x3d\x20\x7b\x61\x3a\x5b\x31\x2c\x20\x32\x2c\x20\x33\x5d\x7d\x3b"
                      "\x20\x20\x66\x6f\x72\x20\x28\x78\x20\x69\x6e\x20\x61\x2e\x61\x29\x20\x7b"
                      "\x20\x20\x20\x20\x62\x20\x3d\x20\x30\x3b"
                      "\x20\x20\x7d"
                      "\x7d"
                      "\x66\x6f\x6f\x28\x29";
  foo = CompileFunction(&env, src_2, "\x66\x6f\x6f");
  SetBreakPoint(foo, 0);  // "\x76\x61\x72\x20\x61\x20\x3d\x20\x2e\x2e\x2e"

  step_action = StepIn;
  break_point_hit_count = 0;
  foo->Call(env->Global(), 0, NULL);
  CHECK_EQ(8, break_point_hit_count);

  // Get rid of the debug event listener.
  v8::Debug::SetDebugEventListener(NULL);
  CheckDebuggerUnloaded();
}


TEST(DebugStepWith) {
  DebugLocalContext env;
  v8::HandleScope scope(env->GetIsolate());

  // Register a debug event listener which steps and counts.
  v8::Debug::SetDebugEventListener(DebugEventStep);

  // Create a function for testing stepping. Run it to allow it to get
  // optimized.
  const char* src = "\x66\x75\x6e\x63\x74\x69\x6f\x6e\x20\x66\x6f\x6f\x28\x78\x29\x20\x7b\x20"
                    "\x20\x20\x76\x61\x72\x20\x61\x20\x3d\x20\x7b\x7d\x3b"
                    "\x20\x20\x77\x69\x74\x68\x20\x28\x61\x29\x20\x7b\x7d"
                    "\x20\x20\x77\x69\x74\x68\x20\x28\x62\x29\x20\x7b\x7d"
                    "\x7d"
                    "\x66\x6f\x6f\x28\x29";
  env->Global()->Set(v8::String::NewFromUtf8(env->GetIsolate(), "\x62"),
                     v8::Object::New(env->GetIsolate()));
  v8::Local<v8::Function> foo = CompileFunction(&env, src, "\x66\x6f\x6f");
  v8::Handle<v8::Value> result;
  SetBreakPoint(foo, 8);  // "\x76\x61\x72\x20\x61\x20\x3d\x20\x7b\x7d\x3b"

  step_action = StepIn;
  break_point_hit_count = 0;
  foo->Call(env->Global(), 0, NULL);
  CHECK_EQ(4, break_point_hit_count);

  // Get rid of the debug event listener.
  v8::Debug::SetDebugEventListener(NULL);
  CheckDebuggerUnloaded();
}


TEST(DebugConditional) {
  DebugLocalContext env;
  v8::Isolate* isolate = env->GetIsolate();
  v8::HandleScope scope(isolate);

  // Register a debug event listener which steps and counts.
  v8::Debug::SetDebugEventListener(DebugEventStep);

  // Create a function for testing stepping. Run it to allow it to get
  // optimized.
  const char* src = "\x66\x75\x6e\x63\x74\x69\x6f\x6e\x20\x66\x6f\x6f\x28\x78\x29\x20\x7b\x20"
                    "\x20\x20\x76\x61\x72\x20\x61\x3b"
                    "\x20\x20\x61\x20\x3d\x20\x78\x20\x3f\x20\x31\x20\x3a\x20\x32\x3b"
                    "\x20\x20\x72\x65\x74\x75\x72\x6e\x20\x61\x3b"
                    "\x7d"
                    "\x66\x6f\x6f\x28\x29";
  v8::Local<v8::Function> foo = CompileFunction(&env, src, "\x66\x6f\x6f");
  SetBreakPoint(foo, 0);  // "\x76\x61\x72\x20\x61\x3b"

  step_action = StepIn;
  break_point_hit_count = 0;
  foo->Call(env->Global(), 0, NULL);
  CHECK_EQ(5, break_point_hit_count);

  step_action = StepIn;
  break_point_hit_count = 0;
  const int argc = 1;
  v8::Handle<v8::Value> argv_true[argc] = { v8::True(isolate) };
  foo->Call(env->Global(), argc, argv_true);
  CHECK_EQ(5, break_point_hit_count);

  // Get rid of the debug event listener.
  v8::Debug::SetDebugEventListener(NULL);
  CheckDebuggerUnloaded();
}


TEST(StepInOutSimple) {
  DebugLocalContext env;
  v8::HandleScope scope(env->GetIsolate());

  // Create a function for checking the function when hitting a break point.
  frame_function_name = CompileFunction(&env,
                                        frame_function_name_source,
                                        "\x66\x72\x61\x6d\x65\x5f\x66\x75\x6e\x63\x74\x69\x6f\x6e\x5f\x6e\x61\x6d\x65");

  // Register a debug event listener which steps and counts.
  v8::Debug::SetDebugEventListener(DebugEventStepSequence);

  // Create a function for testing stepping. Run it to allow it to get
  // optimized.
  const char* src = "\x66\x75\x6e\x63\x74\x69\x6f\x6e\x20\x61\x28\x29\x20\x7b\x62\x28\x29\x3b\x63\x28\x29\x3b\x7d\x3b\x20"
                    "\x66\x75\x6e\x63\x74\x69\x6f\x6e\x20\x62\x28\x29\x20\x7b\x63\x28\x29\x3b\x7d\x3b\x20"
                    "\x66\x75\x6e\x63\x74\x69\x6f\x6e\x20\x63\x28\x29\x20\x7b\x7d\x3b\x20"
                    "\x61\x28\x29\x3b\x20\x62\x28\x29\x3b\x20\x63\x28\x29";
  v8::Local<v8::Function> a = CompileFunction(&env, src, "\x61");
  SetBreakPoint(a, 0);

  // Step through invocation of a with step in.
  step_action = StepIn;
  break_point_hit_count = 0;
  expected_step_sequence = "\x61\x62\x63\x62\x61\x63\x61";
  a->Call(env->Global(), 0, NULL);
  CHECK_EQ(StrLength(expected_step_sequence),
           break_point_hit_count);

  // Step through invocation of a with step next.
  step_action = StepNext;
  break_point_hit_count = 0;
  expected_step_sequence = "\x61\x61\x61";
  a->Call(env->Global(), 0, NULL);
  CHECK_EQ(StrLength(expected_step_sequence),
           break_point_hit_count);

  // Step through invocation of a with step out.
  step_action = StepOut;
  break_point_hit_count = 0;
  expected_step_sequence = "\x61";
  a->Call(env->Global(), 0, NULL);
  CHECK_EQ(StrLength(expected_step_sequence),
           break_point_hit_count);

  // Get rid of the debug event listener.
  v8::Debug::SetDebugEventListener(NULL);
  CheckDebuggerUnloaded();
}


TEST(StepInOutTree) {
  DebugLocalContext env;
  v8::HandleScope scope(env->GetIsolate());

  // Create a function for checking the function when hitting a break point.
  frame_function_name = CompileFunction(&env,
                                        frame_function_name_source,
                                        "\x66\x72\x61\x6d\x65\x5f\x66\x75\x6e\x63\x74\x69\x6f\x6e\x5f\x6e\x61\x6d\x65");

  // Register a debug event listener which steps and counts.
  v8::Debug::SetDebugEventListener(DebugEventStepSequence);

  // Create a function for testing stepping. Run it to allow it to get
  // optimized.
  const char* src = "\x66\x75\x6e\x63\x74\x69\x6f\x6e\x20\x61\x28\x29\x20\x7b\x62\x28\x63\x28\x64\x28\x29\x29\x2c\x64\x28\x29\x29\x3b\x63\x28\x64\x28\x29\x29\x3b\x64\x28\x29\x7d\x3b\x20"
                    "\x66\x75\x6e\x63\x74\x69\x6f\x6e\x20\x62\x28\x78\x2c\x79\x29\x20\x7b\x63\x28\x29\x3b\x7d\x3b\x20"
                    "\x66\x75\x6e\x63\x74\x69\x6f\x6e\x20\x63\x28\x78\x29\x20\x7b\x7d\x3b\x20"
                    "\x66\x75\x6e\x63\x74\x69\x6f\x6e\x20\x64\x28\x29\x20\x7b\x7d\x3b\x20"
                    "\x61\x28\x29\x3b\x20\x62\x28\x29\x3b\x20\x63\x28\x29\x3b\x20\x64\x28\x29";
  v8::Local<v8::Function> a = CompileFunction(&env, src, "\x61");
  SetBreakPoint(a, 0);

  // Step through invocation of a with step in.
  step_action = StepIn;
  break_point_hit_count = 0;
  expected_step_sequence = "\x61\x64\x61\x63\x61\x64\x61\x62\x63\x62\x61\x64\x61\x63\x61\x64\x61";
  a->Call(env->Global(), 0, NULL);
  CHECK_EQ(StrLength(expected_step_sequence),
           break_point_hit_count);

  // Step through invocation of a with step next.
  step_action = StepNext;
  break_point_hit_count = 0;
  expected_step_sequence = "\x61\x61\x61\x61";
  a->Call(env->Global(), 0, NULL);
  CHECK_EQ(StrLength(expected_step_sequence),
           break_point_hit_count);

  // Step through invocation of a with step out.
  step_action = StepOut;
  break_point_hit_count = 0;
  expected_step_sequence = "\x61";
  a->Call(env->Global(), 0, NULL);
  CHECK_EQ(StrLength(expected_step_sequence),
           break_point_hit_count);

  // Get rid of the debug event listener.
  v8::Debug::SetDebugEventListener(NULL);
  CheckDebuggerUnloaded(true);
}


TEST(StepInOutBranch) {
  DebugLocalContext env;
  v8::HandleScope scope(env->GetIsolate());

  // Create a function for checking the function when hitting a break point.
  frame_function_name = CompileFunction(&env,
                                        frame_function_name_source,
                                        "\x66\x72\x61\x6d\x65\x5f\x66\x75\x6e\x63\x74\x69\x6f\x6e\x5f\x6e\x61\x6d\x65");

  // Register a debug event listener which steps and counts.
  v8::Debug::SetDebugEventListener(DebugEventStepSequence);

  // Create a function for testing stepping. Run it to allow it to get
  // optimized.
  const char* src = "\x66\x75\x6e\x63\x74\x69\x6f\x6e\x20\x61\x28\x29\x20\x7b\x62\x28\x66\x61\x6c\x73\x65\x29\x3b\x63\x28\x29\x3b\x7d\x3b\x20"
                    "\x66\x75\x6e\x63\x74\x69\x6f\x6e\x20\x62\x28\x78\x29\x20\x7b\x69\x66\x28\x78\x29\x7b\x63\x28\x29\x3b\x7d\x3b\x7d\x3b\x20"
                    "\x66\x75\x6e\x63\x74\x69\x6f\x6e\x20\x63\x28\x29\x20\x7b\x7d\x3b\x20"
                    "\x61\x28\x29\x3b\x20\x62\x28\x29\x3b\x20\x63\x28\x29";
  v8::Local<v8::Function> a = CompileFunction(&env, src, "\x61");
  SetBreakPoint(a, 0);

  // Step through invocation of a.
  step_action = StepIn;
  break_point_hit_count = 0;
  expected_step_sequence = "\x61\x62\x62\x61\x63\x61";
  a->Call(env->Global(), 0, NULL);
  CHECK_EQ(StrLength(expected_step_sequence),
           break_point_hit_count);

  // Get rid of the debug event listener.
  v8::Debug::SetDebugEventListener(NULL);
  CheckDebuggerUnloaded();
}


// Test that step in does not step into native functions.
TEST(DebugStepNatives) {
  DebugLocalContext env;
  v8::HandleScope scope(env->GetIsolate());

  // Create a function for testing stepping.
  v8::Local<v8::Function> foo = CompileFunction(
      &env,
      "\x66\x75\x6e\x63\x74\x69\x6f\x6e\x20\x66\x6f\x6f\x28\x29\x7b\x64\x65\x62\x75\x67\x67\x65\x72\x3b\x4d\x61\x74\x68\x2e\x73\x69\x6e\x28\x31\x29\x3b\x7d",
      "\x66\x6f\x6f");

  // Register a debug event listener which steps and counts.
  v8::Debug::SetDebugEventListener(DebugEventStep);

  step_action = StepIn;
  break_point_hit_count = 0;
  foo->Call(env->Global(), 0, NULL);

  // With stepping all break locations are hit.
  CHECK_EQ(3, break_point_hit_count);

  v8::Debug::SetDebugEventListener(NULL);
  CheckDebuggerUnloaded();

  // Register a debug event listener which just counts.
  v8::Debug::SetDebugEventListener(DebugEventBreakPointHitCount);

  break_point_hit_count = 0;
  foo->Call(env->Global(), 0, NULL);

  // Without stepping only active break points are hit.
  CHECK_EQ(1, break_point_hit_count);

  v8::Debug::SetDebugEventListener(NULL);
  CheckDebuggerUnloaded();
}


// Test that step in works with function.apply.
TEST(DebugStepFunctionApply) {
  DebugLocalContext env;
  v8::HandleScope scope(env->GetIsolate());

  // Create a function for testing stepping.
  v8::Local<v8::Function> foo = CompileFunction(
      &env,
      "\x66\x75\x6e\x63\x74\x69\x6f\x6e\x20\x62\x61\x72\x28\x78\x2c\x20\x79\x2c\x20\x7a\x29\x20\x7b\x20\x69\x66\x20\x28\x78\x20\x3d\x3d\x20\x31\x29\x20\x7b\x20\x61\x20\x3d\x20\x79\x3b\x20\x62\x20\x3d\x20\x7a\x3b\x20\x7d\x20\x7d"
      "\x66\x75\x6e\x63\x74\x69\x6f\x6e\x20\x66\x6f\x6f\x28\x29\x7b\x20\x64\x65\x62\x75\x67\x67\x65\x72\x3b\x20\x62\x61\x72\x2e\x61\x70\x70\x6c\x79\x28\x74\x68\x69\x73\x2c\x20\x5b\x31\x2c\x32\x2c\x33\x5d\x29\x3b\x20\x7d",
      "\x66\x6f\x6f");

  // Register a debug event listener which steps and counts.
  v8::Debug::SetDebugEventListener(DebugEventStep);

  step_action = StepIn;
  break_point_hit_count = 0;
  foo->Call(env->Global(), 0, NULL);

  // With stepping all break locations are hit.
  CHECK_EQ(7, break_point_hit_count);

  v8::Debug::SetDebugEventListener(NULL);
  CheckDebuggerUnloaded();

  // Register a debug event listener which just counts.
  v8::Debug::SetDebugEventListener(DebugEventBreakPointHitCount);

  break_point_hit_count = 0;
  foo->Call(env->Global(), 0, NULL);

  // Without stepping only the debugger statement is hit.
  CHECK_EQ(1, break_point_hit_count);

  v8::Debug::SetDebugEventListener(NULL);
  CheckDebuggerUnloaded();
}


// Test that step in works with function.call.
TEST(DebugStepFunctionCall) {
  DebugLocalContext env;
  v8::Isolate* isolate = env->GetIsolate();
  v8::HandleScope scope(isolate);

  // Create a function for testing stepping.
  v8::Local<v8::Function> foo = CompileFunction(
      &env,
      "\x66\x75\x6e\x63\x74\x69\x6f\x6e\x20\x62\x61\x72\x28\x78\x2c\x20\x79\x2c\x20\x7a\x29\x20\x7b\x20\x69\x66\x20\x28\x78\x20\x3d\x3d\x20\x31\x29\x20\x7b\x20\x61\x20\x3d\x20\x79\x3b\x20\x62\x20\x3d\x20\x7a\x3b\x20\x7d\x20\x7d"
      "\x66\x75\x6e\x63\x74\x69\x6f\x6e\x20\x66\x6f\x6f\x28\x61\x29\x7b\x20\x64\x65\x62\x75\x67\x67\x65\x72\x3b"
      "\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x69\x66\x20\x28\x61\x29\x20\x7b"
      "\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x62\x61\x72\x2e\x63\x61\x6c\x6c\x28\x74\x68\x69\x73\x2c\x20\x31\x2c\x20\x32\x2c\x20\x33\x29\x3b"
      "\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x7d\x20\x65\x6c\x73\x65\x20\x7b"
      "\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x62\x61\x72\x2e\x63\x61\x6c\x6c\x28\x74\x68\x69\x73\x2c\x20\x30\x29\x3b"
      "\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x7d"
      "\x7d",
      "\x66\x6f\x6f");

  // Register a debug event listener which steps and counts.
  v8::Debug::SetDebugEventListener(DebugEventStep);
  step_action = StepIn;

  // Check stepping where the if condition in bar is false.
  break_point_hit_count = 0;
  foo->Call(env->Global(), 0, NULL);
  CHECK_EQ(6, break_point_hit_count);

  // Check stepping where the if condition in bar is true.
  break_point_hit_count = 0;
  const int argc = 1;
  v8::Handle<v8::Value> argv[argc] = { v8::True(isolate) };
  foo->Call(env->Global(), argc, argv);
  CHECK_EQ(8, break_point_hit_count);

  v8::Debug::SetDebugEventListener(NULL);
  CheckDebuggerUnloaded();

  // Register a debug event listener which just counts.
  v8::Debug::SetDebugEventListener(DebugEventBreakPointHitCount);

  break_point_hit_count = 0;
  foo->Call(env->Global(), 0, NULL);

  // Without stepping only the debugger statement is hit.
  CHECK_EQ(1, break_point_hit_count);

  v8::Debug::SetDebugEventListener(NULL);
  CheckDebuggerUnloaded();
}


// Tests that breakpoint will be hit if it's set in script.
TEST(PauseInScript) {
  DebugLocalContext env;
  v8::HandleScope scope(env->GetIsolate());
  env.ExposeDebug();

  // Register a debug event listener which counts.
  v8::Debug::SetDebugEventListener(DebugEventCounter);

  // Create a script that returns a function.
  const char* src = "\x28\x66\x75\x6e\x63\x74\x69\x6f\x6e\x20\x28\x65\x76\x74\x29\x20\x7b\x7d\x29";
  const char* script_name = "\x53\x74\x65\x70\x49\x6e\x48\x61\x6e\x64\x6c\x65\x72\x54\x65\x73\x74";

  // Set breakpoint in the script.
  SetScriptBreakPointByNameFromJS(env->GetIsolate(), script_name, 0, -1);
  break_point_hit_count = 0;

  v8::ScriptOrigin origin(
      v8::String::NewFromUtf8(env->GetIsolate(), script_name),
      v8::Integer::New(env->GetIsolate(), 0));
  v8::Handle<v8::Script> script = v8::Script::Compile(
      v8::String::NewFromUtf8(env->GetIsolate(), src), &origin);
  v8::Local<v8::Value> r = script->Run();

  CHECK(r->IsFunction());
  CHECK_EQ(1, break_point_hit_count);

  // Get rid of the debug event listener.
  v8::Debug::SetDebugEventListener(NULL);
  CheckDebuggerUnloaded();
}


// Test break on exceptions. For each exception break combination the number
// of debug event exception callbacks and message callbacks are collected. The
// number of debug event exception callbacks are used to check that the
// debugger is called correctly and the number of message callbacks is used to
// check that uncaught exceptions are still returned even if there is a break
// for them.
TEST(BreakOnException) {
  DebugLocalContext env;
  v8::HandleScope scope(env->GetIsolate());
  env.ExposeDebug();

  // Create functions for testing break on exception.
  CompileFunction(&env, "\x66\x75\x6e\x63\x74\x69\x6f\x6e\x20\x74\x68\x72\x6f\x77\x73\x28\x29\x7b\x74\x68\x72\x6f\x77\x20\x31\x3b\x7d", "\x74\x68\x72\x6f\x77\x73");
  v8::Local<v8::Function> caught =
      CompileFunction(&env,
                      "\x66\x75\x6e\x63\x74\x69\x6f\x6e\x20\x63\x61\x75\x67\x68\x74\x28\x29\x7b\x74\x72\x79\x20\x7b\x74\x68\x72\x6f\x77\x73\x28\x29\x3b\x7d\x20\x63\x61\x74\x63\x68\x28\x65\x29\x20\x7b\x7d\x3b\x7d",
                      "\x63\x61\x75\x67\x68\x74");
  v8::Local<v8::Function> notCaught =
      CompileFunction(&env, "\x66\x75\x6e\x63\x74\x69\x6f\x6e\x20\x6e\x6f\x74\x43\x61\x75\x67\x68\x74\x28\x29\x7b\x74\x68\x72\x6f\x77\x73\x28\x29\x3b\x7d", "\x6e\x6f\x74\x43\x61\x75\x67\x68\x74");

  v8::V8::AddMessageListener(MessageCallbackCount);
  v8::Debug::SetDebugEventListener(DebugEventCounter);

  // Initial state should be no break on exceptions.
  DebugEventCounterClear();
  MessageCallbackCountClear();
  caught->Call(env->Global(), 0, NULL);
  CHECK_EQ(0, exception_hit_count);
  CHECK_EQ(0, uncaught_exception_hit_count);
  CHECK_EQ(0, message_callback_count);
  notCaught->Call(env->Global(), 0, NULL);
  CHECK_EQ(0, exception_hit_count);
  CHECK_EQ(0, uncaught_exception_hit_count);
  CHECK_EQ(1, message_callback_count);

  // No break on exception
  DebugEventCounterClear();
  MessageCallbackCountClear();
  ChangeBreakOnException(false, false);
  caught->Call(env->Global(), 0, NULL);
  CHECK_EQ(0, exception_hit_count);
  CHECK_EQ(0, uncaught_exception_hit_count);
  CHECK_EQ(0, message_callback_count);
  notCaught->Call(env->Global(), 0, NULL);
  CHECK_EQ(0, exception_hit_count);
  CHECK_EQ(0, uncaught_exception_hit_count);
  CHECK_EQ(1, message_callback_count);

  // Break on uncaught exception
  DebugEventCounterClear();
  MessageCallbackCountClear();
  ChangeBreakOnException(false, true);
  caught->Call(env->Global(), 0, NULL);
  CHECK_EQ(0, exception_hit_count);
  CHECK_EQ(0, uncaught_exception_hit_count);
  CHECK_EQ(0, message_callback_count);
  notCaught->Call(env->Global(), 0, NULL);
  CHECK_EQ(1, exception_hit_count);
  CHECK_EQ(1, uncaught_exception_hit_count);
  CHECK_EQ(1, message_callback_count);

  // Break on exception and uncaught exception
  DebugEventCounterClear();
  MessageCallbackCountClear();
  ChangeBreakOnException(true, true);
  caught->Call(env->Global(), 0, NULL);
  CHECK_EQ(1, exception_hit_count);
  CHECK_EQ(0, uncaught_exception_hit_count);
  CHECK_EQ(0, message_callback_count);
  notCaught->Call(env->Global(), 0, NULL);
  CHECK_EQ(2, exception_hit_count);
  CHECK_EQ(1, uncaught_exception_hit_count);
  CHECK_EQ(1, message_callback_count);

  // Break on exception
  DebugEventCounterClear();
  MessageCallbackCountClear();
  ChangeBreakOnException(true, false);
  caught->Call(env->Global(), 0, NULL);
  CHECK_EQ(1, exception_hit_count);
  CHECK_EQ(0, uncaught_exception_hit_count);
  CHECK_EQ(0, message_callback_count);
  notCaught->Call(env->Global(), 0, NULL);
  CHECK_EQ(2, exception_hit_count);
  CHECK_EQ(1, uncaught_exception_hit_count);
  CHECK_EQ(1, message_callback_count);

  // No break on exception using JavaScript
  DebugEventCounterClear();
  MessageCallbackCountClear();
  ChangeBreakOnExceptionFromJS(env->GetIsolate(), false, false);
  caught->Call(env->Global(), 0, NULL);
  CHECK_EQ(0, exception_hit_count);
  CHECK_EQ(0, uncaught_exception_hit_count);
  CHECK_EQ(0, message_callback_count);
  notCaught->Call(env->Global(), 0, NULL);
  CHECK_EQ(0, exception_hit_count);
  CHECK_EQ(0, uncaught_exception_hit_count);
  CHECK_EQ(1, message_callback_count);

  // Break on uncaught exception using JavaScript
  DebugEventCounterClear();
  MessageCallbackCountClear();
  ChangeBreakOnExceptionFromJS(env->GetIsolate(), false, true);
  caught->Call(env->Global(), 0, NULL);
  CHECK_EQ(0, exception_hit_count);
  CHECK_EQ(0, uncaught_exception_hit_count);
  CHECK_EQ(0, message_callback_count);
  notCaught->Call(env->Global(), 0, NULL);
  CHECK_EQ(1, exception_hit_count);
  CHECK_EQ(1, uncaught_exception_hit_count);
  CHECK_EQ(1, message_callback_count);

  // Break on exception and uncaught exception using JavaScript
  DebugEventCounterClear();
  MessageCallbackCountClear();
  ChangeBreakOnExceptionFromJS(env->GetIsolate(), true, true);
  caught->Call(env->Global(), 0, NULL);
  CHECK_EQ(1, exception_hit_count);
  CHECK_EQ(0, message_callback_count);
  CHECK_EQ(0, uncaught_exception_hit_count);
  notCaught->Call(env->Global(), 0, NULL);
  CHECK_EQ(2, exception_hit_count);
  CHECK_EQ(1, uncaught_exception_hit_count);
  CHECK_EQ(1, message_callback_count);

  // Break on exception using JavaScript
  DebugEventCounterClear();
  MessageCallbackCountClear();
  ChangeBreakOnExceptionFromJS(env->GetIsolate(), true, false);
  caught->Call(env->Global(), 0, NULL);
  CHECK_EQ(1, exception_hit_count);
  CHECK_EQ(0, uncaught_exception_hit_count);
  CHECK_EQ(0, message_callback_count);
  notCaught->Call(env->Global(), 0, NULL);
  CHECK_EQ(2, exception_hit_count);
  CHECK_EQ(1, uncaught_exception_hit_count);
  CHECK_EQ(1, message_callback_count);

  v8::Debug::SetDebugEventListener(NULL);
  CheckDebuggerUnloaded();
  v8::V8::RemoveMessageListeners(MessageCallbackCount);
}


// Test break on exception from compiler errors. When compiling using
// v8::Script::Compile there is no JavaScript stack whereas when compiling using
// eval there are JavaScript frames.
TEST(BreakOnCompileException) {
  DebugLocalContext env;
  v8::HandleScope scope(env->GetIsolate());

  // For this test, we want to break on uncaught exceptions:
  ChangeBreakOnException(false, true);

  // Create a function for checking the function when hitting a break point.
  frame_count = CompileFunction(&env, frame_count_source, "\x66\x72\x61\x6d\x65\x5f\x63\x6f\x75\x6e\x74");

  v8::V8::AddMessageListener(MessageCallbackCount);
  v8::Debug::SetDebugEventListener(DebugEventCounter);

  DebugEventCounterClear();
  MessageCallbackCountClear();

  // Check initial state.
  CHECK_EQ(0, exception_hit_count);
  CHECK_EQ(0, uncaught_exception_hit_count);
  CHECK_EQ(0, message_callback_count);
  CHECK_EQ(-1, last_js_stack_height);

  // Throws SyntaxError: Unexpected end of input
  v8::Script::Compile(v8::String::NewFromUtf8(env->GetIsolate(), "\x2b\x2b\x2b"));
  CHECK_EQ(1, exception_hit_count);
  CHECK_EQ(1, uncaught_exception_hit_count);
  CHECK_EQ(1, message_callback_count);
  CHECK_EQ(0, last_js_stack_height);  // No JavaScript stack.

  // Throws SyntaxError: Unexpected identifier
  v8::Script::Compile(v8::String::NewFromUtf8(env->GetIsolate(), "\x78\x20\x78"));
  CHECK_EQ(2, exception_hit_count);
  CHECK_EQ(2, uncaught_exception_hit_count);
  CHECK_EQ(2, message_callback_count);
  CHECK_EQ(0, last_js_stack_height);  // No JavaScript stack.

  // Throws SyntaxError: Unexpected end of input
  v8::Script::Compile(v8::String::NewFromUtf8(env->GetIsolate(), "\x65\x76\x61\x6c\x28\x27\x2b\x2b\x2b\x27\x29"))
      ->Run();
  CHECK_EQ(3, exception_hit_count);
  CHECK_EQ(3, uncaught_exception_hit_count);
  CHECK_EQ(3, message_callback_count);
  CHECK_EQ(1, last_js_stack_height);

  // Throws SyntaxError: Unexpected identifier
  v8::Script::Compile(v8::String::NewFromUtf8(env->GetIsolate(), "\x65\x76\x61\x6c\x28\x27\x78\x20\x78\x27\x29"))
      ->Run();
  CHECK_EQ(4, exception_hit_count);
  CHECK_EQ(4, uncaught_exception_hit_count);
  CHECK_EQ(4, message_callback_count);
  CHECK_EQ(1, last_js_stack_height);
}


TEST(StepWithException) {
  DebugLocalContext env;
  v8::HandleScope scope(env->GetIsolate());

  // For this test, we want to break on uncaught exceptions:
  ChangeBreakOnException(false, true);

  // Create a function for checking the function when hitting a break point.
  frame_function_name = CompileFunction(&env,
                                        frame_function_name_source,
                                        "\x66\x72\x61\x6d\x65\x5f\x66\x75\x6e\x63\x74\x69\x6f\x6e\x5f\x6e\x61\x6d\x65");

  // Register a debug event listener which steps and counts.
  v8::Debug::SetDebugEventListener(DebugEventStepSequence);

  // Create functions for testing stepping.
  const char* src = "\x66\x75\x6e\x63\x74\x69\x6f\x6e\x20\x61\x28\x29\x20\x7b\x20\x6e\x28\x29\x3b\x20\x7d\x3b\x20"
                    "\x66\x75\x6e\x63\x74\x69\x6f\x6e\x20\x62\x28\x29\x20\x7b\x20\x63\x28\x29\x3b\x20\x7d\x3b\x20"
                    "\x66\x75\x6e\x63\x74\x69\x6f\x6e\x20\x63\x28\x29\x20\x7b\x20\x6e\x28\x29\x3b\x20\x7d\x3b\x20"
                    "\x66\x75\x6e\x63\x74\x69\x6f\x6e\x20\x64\x28\x29\x20\x7b\x20\x78\x20\x3d\x20\x31\x3b\x20\x74\x72\x79\x20\x7b\x20\x65\x28\x29\x3b\x20\x7d\x20\x63\x61\x74\x63\x68\x28\x78\x29\x20\x7b\x20\x78\x20\x3d\x20\x32\x3b\x20\x7d\x20\x7d\x3b\x20"
                    "\x66\x75\x6e\x63\x74\x69\x6f\x6e\x20\x65\x28\x29\x20\x7b\x20\x6e\x28\x29\x3b\x20\x7d\x3b\x20"
                    "\x66\x75\x6e\x63\x74\x69\x6f\x6e\x20\x66\x28\x29\x20\x7b\x20\x78\x20\x3d\x20\x31\x3b\x20\x74\x72\x79\x20\x7b\x20\x67\x28\x29\x3b\x20\x7d\x20\x63\x61\x74\x63\x68\x28\x78\x29\x20\x7b\x20\x78\x20\x3d\x20\x32\x3b\x20\x7d\x20\x7d\x3b\x20"
                    "\x66\x75\x6e\x63\x74\x69\x6f\x6e\x20\x67\x28\x29\x20\x7b\x20\x68\x28\x29\x3b\x20\x7d\x3b\x20"
                    "\x66\x75\x6e\x63\x74\x69\x6f\x6e\x20\x68\x28\x29\x20\x7b\x20\x78\x20\x3d\x20\x31\x3b\x20\x74\x68\x72\x6f\x77\x20\x31\x3b\x20\x7d\x3b\x20";

  // Step through invocation of a.
  v8::Local<v8::Function> a = CompileFunction(&env, src, "\x61");
  SetBreakPoint(a, 0);
  step_action = StepIn;
  break_point_hit_count = 0;
  expected_step_sequence = "\x61\x61";
  a->Call(env->Global(), 0, NULL);
  CHECK_EQ(StrLength(expected_step_sequence),
           break_point_hit_count);

  // Step through invocation of b + c.
  v8::Local<v8::Function> b = CompileFunction(&env, src, "\x62");
  SetBreakPoint(b, 0);
  step_action = StepIn;
  break_point_hit_count = 0;
  expected_step_sequence = "\x62\x63\x63";
  b->Call(env->Global(), 0, NULL);
  CHECK_EQ(StrLength(expected_step_sequence),
           break_point_hit_count);
  // Step through invocation of d + e.
  v8::Local<v8::Function> d = CompileFunction(&env, src, "\x64");
  SetBreakPoint(d, 0);
  ChangeBreakOnException(false, true);
  step_action = StepIn;
  break_point_hit_count = 0;
  expected_step_sequence = "\x64\x64\x65\x64\x64";
  d->Call(env->Global(), 0, NULL);
  CHECK_EQ(StrLength(expected_step_sequence),
           break_point_hit_count);

  // Step through invocation of d + e now with break on caught exceptions.
  ChangeBreakOnException(true, true);
  step_action = StepIn;
  break_point_hit_count = 0;
  expected_step_sequence = "\x64\x64\x65\x65\x64\x64";
  d->Call(env->Global(), 0, NULL);
  CHECK_EQ(StrLength(expected_step_sequence),
           break_point_hit_count);

  // Step through invocation of f + g + h.
  v8::Local<v8::Function> f = CompileFunction(&env, src, "\x66");
  SetBreakPoint(f, 0);
  ChangeBreakOnException(false, true);
  step_action = StepIn;
  break_point_hit_count = 0;
  expected_step_sequence = "\x66\x66\x67\x68\x68\x66\x66";
  f->Call(env->Global(), 0, NULL);
  CHECK_EQ(StrLength(expected_step_sequence),
           break_point_hit_count);

  // Step through invocation of f + g + h now with break on caught exceptions.
  ChangeBreakOnException(true, true);
  step_action = StepIn;
  break_point_hit_count = 0;
  expected_step_sequence = "\x66\x66\x67\x68\x68\x68\x66\x66";
  f->Call(env->Global(), 0, NULL);
  CHECK_EQ(StrLength(expected_step_sequence),
           break_point_hit_count);

  // Get rid of the debug event listener.
  v8::Debug::SetDebugEventListener(NULL);
  CheckDebuggerUnloaded();
}


TEST(DebugBreak) {
  i::FLAG_stress_compaction = false;
#ifdef VERIFY_HEAP
  i::FLAG_verify_heap = true;
#endif
  DebugLocalContext env;
  v8::Isolate* isolate = env->GetIsolate();
  v8::HandleScope scope(isolate);

  // Register a debug event listener which sets the break flag and counts.
  v8::Debug::SetDebugEventListener(DebugEventBreak);

  // Create a function for testing stepping.
  const char* src = "\x66\x75\x6e\x63\x74\x69\x6f\x6e\x20\x66\x30\x28\x29\x20\x7b\x7d"
                    "\x66\x75\x6e\x63\x74\x69\x6f\x6e\x20\x66\x31\x28\x78\x31\x29\x20\x7b\x7d"
                    "\x66\x75\x6e\x63\x74\x69\x6f\x6e\x20\x66\x32\x28\x78\x31\x2c\x78\x32\x29\x20\x7b\x7d"
                    "\x66\x75\x6e\x63\x74\x69\x6f\x6e\x20\x66\x33\x28\x78\x31\x2c\x78\x32\x2c\x78\x33\x29\x20\x7b\x7d";
  v8::Local<v8::Function> f0 = CompileFunction(&env, src, "\x66\x30");
  v8::Local<v8::Function> f1 = CompileFunction(&env, src, "\x66\x31");
  v8::Local<v8::Function> f2 = CompileFunction(&env, src, "\x66\x32");
  v8::Local<v8::Function> f3 = CompileFunction(&env, src, "\x66\x33");

  // Call the function to make sure it is compiled.
  v8::Handle<v8::Value> argv[] = { v8::Number::New(isolate, 1),
                                   v8::Number::New(isolate, 1),
                                   v8::Number::New(isolate, 1),
                                   v8::Number::New(isolate, 1) };

  // Call all functions to make sure that they are compiled.
  f0->Call(env->Global(), 0, NULL);
  f1->Call(env->Global(), 0, NULL);
  f2->Call(env->Global(), 0, NULL);
  f3->Call(env->Global(), 0, NULL);

  // Set the debug break flag.
  v8::Debug::DebugBreak(env->GetIsolate());

  // Call all functions with different argument count.
  break_point_hit_count = 0;
  for (unsigned int i = 0; i < ARRAY_SIZE(argv); i++) {
    f0->Call(env->Global(), i, argv);
    f1->Call(env->Global(), i, argv);
    f2->Call(env->Global(), i, argv);
    f3->Call(env->Global(), i, argv);
  }

  // One break for each function called.
  CHECK_EQ(4 * ARRAY_SIZE(argv), break_point_hit_count);

  // Get rid of the debug event listener.
  v8::Debug::SetDebugEventListener(NULL);
  CheckDebuggerUnloaded();
}


// Test to ensure that JavaScript code keeps running while the debug break
// through the stack limit flag is set but breaks are disabled.
TEST(DisableBreak) {
  DebugLocalContext env;
  v8::HandleScope scope(env->GetIsolate());

  // Register a debug event listener which sets the break flag and counts.
  v8::Debug::SetDebugEventListener(DebugEventCounter);

  // Create a function for testing stepping.
  const char* src = "\x66\x75\x6e\x63\x74\x69\x6f\x6e\x20\x66\x28\x29\x20\x7b\x67\x28\x29\x7d\x3b\x66\x75\x6e\x63\x74\x69\x6f\x6e\x20\x67\x28\x29\x7b\x69\x3d\x30\x3b\x20\x77\x68\x69\x6c\x65\x28\x69\x3c\x31\x30\x29\x7b\x69\x2b\x2b\x7d\x7d";
  v8::Local<v8::Function> f = CompileFunction(&env, src, "\x66");

  // Set the debug break flag.
  v8::Debug::DebugBreak(env->GetIsolate());

  // Call all functions with different argument count.
  break_point_hit_count = 0;
  f->Call(env->Global(), 0, NULL);
  CHECK_EQ(1, break_point_hit_count);

  {
    v8::Debug::DebugBreak(env->GetIsolate());
    i::Isolate* isolate = reinterpret_cast<i::Isolate*>(env->GetIsolate());
    v8::internal::DisableBreak disable_break(isolate->debug(), true);
    f->Call(env->Global(), 0, NULL);
    CHECK_EQ(1, break_point_hit_count);
  }

  f->Call(env->Global(), 0, NULL);
  CHECK_EQ(2, break_point_hit_count);

  // Get rid of the debug event listener.
  v8::Debug::SetDebugEventListener(NULL);
  CheckDebuggerUnloaded();
}

static const char* kSimpleExtensionSource =
  "\x28\x66\x75\x6e\x63\x74\x69\x6f\x6e\x20\x46\x6f\x6f\x28\x29\x20\x7b"
  "\x20\x20\x72\x65\x74\x75\x72\x6e\x20\x34\x3b"
  "\x7d\x29\x28\x29\x20";

// http://crbug.com/28933
// Test that debug break is disabled when bootstrapper is active.
TEST(NoBreakWhenBootstrapping) {
  v8::Isolate* isolate = CcTest::isolate();
  v8::HandleScope scope(isolate);

  // Register a debug event listener which sets the break flag and counts.
  v8::Debug::SetDebugEventListener(DebugEventCounter);

  // Set the debug break flag.
  v8::Debug::DebugBreak(isolate);
  break_point_hit_count = 0;
  {
    // Create a context with an extension to make sure that some JavaScript
    // code is executed during bootstrapping.
    v8::RegisterExtension(new v8::Extension("\x73\x69\x6d\x70\x6c\x65\x74\x65\x73\x74",
                                            kSimpleExtensionSource));
    const char* extension_names[] = { "\x73\x69\x6d\x70\x6c\x65\x74\x65\x73\x74" };
    v8::ExtensionConfiguration extensions(1, extension_names);
    v8::HandleScope handle_scope(isolate);
    v8::Context::New(isolate, &extensions);
  }
  // Check that no DebugBreak events occured during the context creation.
  CHECK_EQ(0, break_point_hit_count);

  // Get rid of the debug event listener.
  v8::Debug::SetDebugEventListener(NULL);
  CheckDebuggerUnloaded();
}


static void NamedEnum(const v8::PropertyCallbackInfo<v8::Array>& info) {
  v8::Handle<v8::Array> result = v8::Array::New(info.GetIsolate(), 3);
  result->Set(v8::Integer::New(info.GetIsolate(), 0),
              v8::String::NewFromUtf8(info.GetIsolate(), "\x61"));
  result->Set(v8::Integer::New(info.GetIsolate(), 1),
              v8::String::NewFromUtf8(info.GetIsolate(), "\x62"));
  result->Set(v8::Integer::New(info.GetIsolate(), 2),
              v8::String::NewFromUtf8(info.GetIsolate(), "\x63"));
  info.GetReturnValue().Set(result);
}


static void IndexedEnum(const v8::PropertyCallbackInfo<v8::Array>& info) {
  v8::Isolate* isolate = info.GetIsolate();
  v8::Handle<v8::Array> result = v8::Array::New(isolate, 2);
  result->Set(v8::Integer::New(isolate, 0), v8::Number::New(isolate, 1));
  result->Set(v8::Integer::New(isolate, 1), v8::Number::New(isolate, 10));
  info.GetReturnValue().Set(result);
}


static void NamedGetter(v8::Local<v8::String> name,
                        const v8::PropertyCallbackInfo<v8::Value>& info) {
  v8::String::Utf8Value n(name);
  if (strcmp(*n, "\x61") == 0) {
    info.GetReturnValue().Set(v8::String::NewFromUtf8(info.GetIsolate(), "\x41\x41"));
    return;
  } else if (strcmp(*n, "\x62") == 0) {
    info.GetReturnValue().Set(v8::String::NewFromUtf8(info.GetIsolate(), "\x42\x42"));
    return;
  } else if (strcmp(*n, "\x63") == 0) {
    info.GetReturnValue().Set(v8::String::NewFromUtf8(info.GetIsolate(), "\x43\x43"));
    return;
  } else {
    info.GetReturnValue().SetUndefined();
    return;
  }
  info.GetReturnValue().Set(name);
}


static void IndexedGetter(uint32_t index,
                          const v8::PropertyCallbackInfo<v8::Value>& info) {
  info.GetReturnValue().Set(static_cast<double>(index + 1));
}


TEST(InterceptorPropertyMirror) {
  // Create a V8 environment with debug access.
  DebugLocalContext env;
  v8::Isolate* isolate = env->GetIsolate();
  v8::HandleScope scope(isolate);
  env.ExposeDebug();

  // Create object with named interceptor.
  v8::Handle<v8::ObjectTemplate> named = v8::ObjectTemplate::New(isolate);
  named->SetNamedPropertyHandler(NamedGetter, NULL, NULL, NULL, NamedEnum);
  env->Global()->Set(
      v8::String::NewFromUtf8(isolate, "\x69\x6e\x74\x65\x72\x63\x65\x70\x74\x65\x64\x5f\x6e\x61\x6d\x65\x64"),
      named->NewInstance());

  // Create object with indexed interceptor.
  v8::Handle<v8::ObjectTemplate> indexed = v8::ObjectTemplate::New(isolate);
  indexed->SetIndexedPropertyHandler(IndexedGetter,
                                     NULL,
                                     NULL,
                                     NULL,
                                     IndexedEnum);
  env->Global()->Set(
      v8::String::NewFromUtf8(isolate, "\x69\x6e\x74\x65\x72\x63\x65\x70\x74\x65\x64\x5f\x69\x6e\x64\x65\x78\x65\x64"),
      indexed->NewInstance());

  // Create object with both named and indexed interceptor.
  v8::Handle<v8::ObjectTemplate> both = v8::ObjectTemplate::New(isolate);
  both->SetNamedPropertyHandler(NamedGetter, NULL, NULL, NULL, NamedEnum);
  both->SetIndexedPropertyHandler(IndexedGetter, NULL, NULL, NULL, IndexedEnum);
  env->Global()->Set(
      v8::String::NewFromUtf8(isolate, "\x69\x6e\x74\x65\x72\x63\x65\x70\x74\x65\x64\x5f\x62\x6f\x74\x68"),
      both->NewInstance());

  // Get mirrors for the three objects with interceptor.
  CompileRun(
      "\x76\x61\x72\x20\x6e\x61\x6d\x65\x64\x5f\x6d\x69\x72\x72\x6f\x72\x20\x3d\x20\x64\x65\x62\x75\x67\x2e\x4d\x61\x6b\x65\x4d\x69\x72\x72\x6f\x72\x28\x69\x6e\x74\x65\x72\x63\x65\x70\x74\x65\x64\x5f\x6e\x61\x6d\x65\x64\x29\x3b"
      "\x76\x61\x72\x20\x69\x6e\x64\x65\x78\x65\x64\x5f\x6d\x69\x72\x72\x6f\x72\x20\x3d\x20\x64\x65\x62\x75\x67\x2e\x4d\x61\x6b\x65\x4d\x69\x72\x72\x6f\x72\x28\x69\x6e\x74\x65\x72\x63\x65\x70\x74\x65\x64\x5f\x69\x6e\x64\x65\x78\x65\x64\x29\x3b"
      "\x76\x61\x72\x20\x62\x6f\x74\x68\x5f\x6d\x69\x72\x72\x6f\x72\x20\x3d\x20\x64\x65\x62\x75\x67\x2e\x4d\x61\x6b\x65\x4d\x69\x72\x72\x6f\x72\x28\x69\x6e\x74\x65\x72\x63\x65\x70\x74\x65\x64\x5f\x62\x6f\x74\x68\x29");
  CHECK(CompileRun(
       "\x6e\x61\x6d\x65\x64\x5f\x6d\x69\x72\x72\x6f\x72\x20\x69\x6e\x73\x74\x61\x6e\x63\x65\x6f\x66\x20\x64\x65\x62\x75\x67\x2e\x4f\x62\x6a\x65\x63\x74\x4d\x69\x72\x72\x6f\x72")->BooleanValue());
  CHECK(CompileRun(
        "\x69\x6e\x64\x65\x78\x65\x64\x5f\x6d\x69\x72\x72\x6f\x72\x20\x69\x6e\x73\x74\x61\x6e\x63\x65\x6f\x66\x20\x64\x65\x62\x75\x67\x2e\x4f\x62\x6a\x65\x63\x74\x4d\x69\x72\x72\x6f\x72")->BooleanValue());
  CHECK(CompileRun(
        "\x62\x6f\x74\x68\x5f\x6d\x69\x72\x72\x6f\x72\x20\x69\x6e\x73\x74\x61\x6e\x63\x65\x6f\x66\x20\x64\x65\x62\x75\x67\x2e\x4f\x62\x6a\x65\x63\x74\x4d\x69\x72\x72\x6f\x72")->BooleanValue());

  // Get the property names from the interceptors
  CompileRun(
      "\x6e\x61\x6d\x65\x64\x5f\x6e\x61\x6d\x65\x73\x20\x3d\x20\x6e\x61\x6d\x65\x64\x5f\x6d\x69\x72\x72\x6f\x72\x2e\x70\x72\x6f\x70\x65\x72\x74\x79\x4e\x61\x6d\x65\x73\x28\x29\x3b"
      "\x69\x6e\x64\x65\x78\x65\x64\x5f\x6e\x61\x6d\x65\x73\x20\x3d\x20\x69\x6e\x64\x65\x78\x65\x64\x5f\x6d\x69\x72\x72\x6f\x72\x2e\x70\x72\x6f\x70\x65\x72\x74\x79\x4e\x61\x6d\x65\x73\x28\x29\x3b"
      "\x62\x6f\x74\x68\x5f\x6e\x61\x6d\x65\x73\x20\x3d\x20\x62\x6f\x74\x68\x5f\x6d\x69\x72\x72\x6f\x72\x2e\x70\x72\x6f\x70\x65\x72\x74\x79\x4e\x61\x6d\x65\x73\x28\x29");
  CHECK_EQ(3, CompileRun("\x6e\x61\x6d\x65\x64\x5f\x6e\x61\x6d\x65\x73\x2e\x6c\x65\x6e\x67\x74\x68")->Int32Value());
  CHECK_EQ(2, CompileRun("\x69\x6e\x64\x65\x78\x65\x64\x5f\x6e\x61\x6d\x65\x73\x2e\x6c\x65\x6e\x67\x74\x68")->Int32Value());
  CHECK_EQ(5, CompileRun("\x62\x6f\x74\x68\x5f\x6e\x61\x6d\x65\x73\x2e\x6c\x65\x6e\x67\x74\x68")->Int32Value());

  // Check the expected number of properties.
  const char* source;
  source = "\x6e\x61\x6d\x65\x64\x5f\x6d\x69\x72\x72\x6f\x72\x2e\x70\x72\x6f\x70\x65\x72\x74\x69\x65\x73\x28\x29\x2e\x6c\x65\x6e\x67\x74\x68";
  CHECK_EQ(3, CompileRun(source)->Int32Value());

  source = "\x69\x6e\x64\x65\x78\x65\x64\x5f\x6d\x69\x72\x72\x6f\x72\x2e\x70\x72\x6f\x70\x65\x72\x74\x69\x65\x73\x28\x29\x2e\x6c\x65\x6e\x67\x74\x68";
  CHECK_EQ(2, CompileRun(source)->Int32Value());

  source = "\x62\x6f\x74\x68\x5f\x6d\x69\x72\x72\x6f\x72\x2e\x70\x72\x6f\x70\x65\x72\x74\x69\x65\x73\x28\x29\x2e\x6c\x65\x6e\x67\x74\x68";
  CHECK_EQ(5, CompileRun(source)->Int32Value());

  // 1 is PropertyKind.Named;
  source = "\x62\x6f\x74\x68\x5f\x6d\x69\x72\x72\x6f\x72\x2e\x70\x72\x6f\x70\x65\x72\x74\x69\x65\x73\x28\x31\x29\x2e\x6c\x65\x6e\x67\x74\x68";
  CHECK_EQ(3, CompileRun(source)->Int32Value());

  // 2 is PropertyKind.Indexed;
  source = "\x62\x6f\x74\x68\x5f\x6d\x69\x72\x72\x6f\x72\x2e\x70\x72\x6f\x70\x65\x72\x74\x69\x65\x73\x28\x32\x29\x2e\x6c\x65\x6e\x67\x74\x68";
  CHECK_EQ(2, CompileRun(source)->Int32Value());

  // 3 is PropertyKind.Named  | PropertyKind.Indexed;
  source = "\x62\x6f\x74\x68\x5f\x6d\x69\x72\x72\x6f\x72\x2e\x70\x72\x6f\x70\x65\x72\x74\x69\x65\x73\x28\x33\x29\x2e\x6c\x65\x6e\x67\x74\x68";
  CHECK_EQ(5, CompileRun(source)->Int32Value());

  // Get the interceptor properties for the object with only named interceptor.
  CompileRun("\x76\x61\x72\x20\x6e\x61\x6d\x65\x64\x5f\x76\x61\x6c\x75\x65\x73\x20\x3d\x20\x6e\x61\x6d\x65\x64\x5f\x6d\x69\x72\x72\x6f\x72\x2e\x70\x72\x6f\x70\x65\x72\x74\x69\x65\x73\x28\x29");

  // Check that the properties are interceptor properties.
  for (int i = 0; i < 3; i++) {
    EmbeddedVector<char, SMALL_STRING_BUFFER_SIZE> buffer;
    SNPrintF(buffer,
             "\x6e\x61\x6d\x65\x64\x5f\x76\x61\x6c\x75\x65\x73\x5b\x6c\x84\x5d\x20\x69\x6e\x73\x74\x61\x6e\x63\x65\x6f\x66\x20\x64\x65\x62\x75\x67\x2e\x50\x72\x6f\x70\x65\x72\x74\x79\x4d\x69\x72\x72\x6f\x72", i);
    CHECK(CompileRun(buffer.start())->BooleanValue());

    SNPrintF(buffer, "\x6e\x61\x6d\x65\x64\x5f\x76\x61\x6c\x75\x65\x73\x5b\x6c\x84\x5d\x2e\x70\x72\x6f\x70\x65\x72\x74\x79\x54\x79\x70\x65\x28\x29", i);
    CHECK_EQ(v8::internal::INTERCEPTOR,
             CompileRun(buffer.start())->Int32Value());

    SNPrintF(buffer, "\x6e\x61\x6d\x65\x64\x5f\x76\x61\x6c\x75\x65\x73\x5b\x6c\x84\x5d\x2e\x69\x73\x4e\x61\x74\x69\x76\x65\x28\x29", i);
    CHECK(CompileRun(buffer.start())->BooleanValue());
  }

  // Get the interceptor properties for the object with only indexed
  // interceptor.
  CompileRun("\x76\x61\x72\x20\x69\x6e\x64\x65\x78\x65\x64\x5f\x76\x61\x6c\x75\x65\x73\x20\x3d\x20\x69\x6e\x64\x65\x78\x65\x64\x5f\x6d\x69\x72\x72\x6f\x72\x2e\x70\x72\x6f\x70\x65\x72\x74\x69\x65\x73\x28\x29");

  // Check that the properties are interceptor properties.
  for (int i = 0; i < 2; i++) {
    EmbeddedVector<char, SMALL_STRING_BUFFER_SIZE> buffer;
    SNPrintF(buffer,
             "\x69\x6e\x64\x65\x78\x65\x64\x5f\x76\x61\x6c\x75\x65\x73\x5b\x6c\x84\x5d\x20\x69\x6e\x73\x74\x61\x6e\x63\x65\x6f\x66\x20\x64\x65\x62\x75\x67\x2e\x50\x72\x6f\x70\x65\x72\x74\x79\x4d\x69\x72\x72\x6f\x72", i);
    CHECK(CompileRun(buffer.start())->BooleanValue());
  }

  // Get the interceptor properties for the object with both types of
  // interceptors.
  CompileRun("\x76\x61\x72\x20\x62\x6f\x74\x68\x5f\x76\x61\x6c\x75\x65\x73\x20\x3d\x20\x62\x6f\x74\x68\x5f\x6d\x69\x72\x72\x6f\x72\x2e\x70\x72\x6f\x70\x65\x72\x74\x69\x65\x73\x28\x29");

  // Check that the properties are interceptor properties.
  for (int i = 0; i < 5; i++) {
    EmbeddedVector<char, SMALL_STRING_BUFFER_SIZE> buffer;
    SNPrintF(buffer, "\x62\x6f\x74\x68\x5f\x76\x61\x6c\x75\x65\x73\x5b\x6c\x84\x5d\x20\x69\x6e\x73\x74\x61\x6e\x63\x65\x6f\x66\x20\x64\x65\x62\x75\x67\x2e\x50\x72\x6f\x70\x65\x72\x74\x79\x4d\x69\x72\x72\x6f\x72", i);
    CHECK(CompileRun(buffer.start())->BooleanValue());
  }

  // Check the property names.
  source = "\x62\x6f\x74\x68\x5f\x76\x61\x6c\x75\x65\x73\x5b\x30\x5d\x2e\x6e\x61\x6d\x65\x28\x29\x20\x3d\x3d\x20\x27\x61\x27";
  CHECK(CompileRun(source)->BooleanValue());

  source = "\x62\x6f\x74\x68\x5f\x76\x61\x6c\x75\x65\x73\x5b\x31\x5d\x2e\x6e\x61\x6d\x65\x28\x29\x20\x3d\x3d\x20\x27\x62\x27";
  CHECK(CompileRun(source)->BooleanValue());

  source = "\x62\x6f\x74\x68\x5f\x76\x61\x6c\x75\x65\x73\x5b\x32\x5d\x2e\x6e\x61\x6d\x65\x28\x29\x20\x3d\x3d\x20\x27\x63\x27";
  CHECK(CompileRun(source)->BooleanValue());

  source = "\x62\x6f\x74\x68\x5f\x76\x61\x6c\x75\x65\x73\x5b\x33\x5d\x2e\x6e\x61\x6d\x65\x28\x29\x20\x3d\x3d\x20\x31";
  CHECK(CompileRun(source)->BooleanValue());

  source = "\x62\x6f\x74\x68\x5f\x76\x61\x6c\x75\x65\x73\x5b\x34\x5d\x2e\x6e\x61\x6d\x65\x28\x29\x20\x3d\x3d\x20\x31\x30";
  CHECK(CompileRun(source)->BooleanValue());
}


TEST(HiddenPrototypePropertyMirror) {
  // Create a V8 environment with debug access.
  DebugLocalContext env;
  v8::Isolate* isolate = env->GetIsolate();
  v8::HandleScope scope(isolate);
  env.ExposeDebug();

  v8::Handle<v8::FunctionTemplate> t0 = v8::FunctionTemplate::New(isolate);
  t0->InstanceTemplate()->Set(v8::String::NewFromUtf8(isolate, "\x78"),
                              v8::Number::New(isolate, 0));
  v8::Handle<v8::FunctionTemplate> t1 = v8::FunctionTemplate::New(isolate);
  t1->SetHiddenPrototype(true);
  t1->InstanceTemplate()->Set(v8::String::NewFromUtf8(isolate, "\x79"),
                              v8::Number::New(isolate, 1));
  v8::Handle<v8::FunctionTemplate> t2 = v8::FunctionTemplate::New(isolate);
  t2->SetHiddenPrototype(true);
  t2->InstanceTemplate()->Set(v8::String::NewFromUtf8(isolate, "\x7a"),
                              v8::Number::New(isolate, 2));
  v8::Handle<v8::FunctionTemplate> t3 = v8::FunctionTemplate::New(isolate);
  t3->InstanceTemplate()->Set(v8::String::NewFromUtf8(isolate, "\x75"),
                              v8::Number::New(isolate, 3));

  // Create object and set them on the global object.
  v8::Handle<v8::Object> o0 = t0->GetFunction()->NewInstance();
  env->Global()->Set(v8::String::NewFromUtf8(isolate, "\x6f\x30"), o0);
  v8::Handle<v8::Object> o1 = t1->GetFunction()->NewInstance();
  env->Global()->Set(v8::String::NewFromUtf8(isolate, "\x6f\x31"), o1);
  v8::Handle<v8::Object> o2 = t2->GetFunction()->NewInstance();
  env->Global()->Set(v8::String::NewFromUtf8(isolate, "\x6f\x32"), o2);
  v8::Handle<v8::Object> o3 = t3->GetFunction()->NewInstance();
  env->Global()->Set(v8::String::NewFromUtf8(isolate, "\x6f\x33"), o3);

  // Get mirrors for the four objects.
  CompileRun(
      "\x76\x61\x72\x20\x6f\x30\x5f\x6d\x69\x72\x72\x6f\x72\x20\x3d\x20\x64\x65\x62\x75\x67\x2e\x4d\x61\x6b\x65\x4d\x69\x72\x72\x6f\x72\x28\x6f\x30\x29\x3b"
      "\x76\x61\x72\x20\x6f\x31\x5f\x6d\x69\x72\x72\x6f\x72\x20\x3d\x20\x64\x65\x62\x75\x67\x2e\x4d\x61\x6b\x65\x4d\x69\x72\x72\x6f\x72\x28\x6f\x31\x29\x3b"
      "\x76\x61\x72\x20\x6f\x32\x5f\x6d\x69\x72\x72\x6f\x72\x20\x3d\x20\x64\x65\x62\x75\x67\x2e\x4d\x61\x6b\x65\x4d\x69\x72\x72\x6f\x72\x28\x6f\x32\x29\x3b"
      "\x76\x61\x72\x20\x6f\x33\x5f\x6d\x69\x72\x72\x6f\x72\x20\x3d\x20\x64\x65\x62\x75\x67\x2e\x4d\x61\x6b\x65\x4d\x69\x72\x72\x6f\x72\x28\x6f\x33\x29");
  CHECK(CompileRun("\x6f\x30\x5f\x6d\x69\x72\x72\x6f\x72\x20\x69\x6e\x73\x74\x61\x6e\x63\x65\x6f\x66\x20\x64\x65\x62\x75\x67\x2e\x4f\x62\x6a\x65\x63\x74\x4d\x69\x72\x72\x6f\x72")->BooleanValue());
  CHECK(CompileRun("\x6f\x31\x5f\x6d\x69\x72\x72\x6f\x72\x20\x69\x6e\x73\x74\x61\x6e\x63\x65\x6f\x66\x20\x64\x65\x62\x75\x67\x2e\x4f\x62\x6a\x65\x63\x74\x4d\x69\x72\x72\x6f\x72")->BooleanValue());
  CHECK(CompileRun("\x6f\x32\x5f\x6d\x69\x72\x72\x6f\x72\x20\x69\x6e\x73\x74\x61\x6e\x63\x65\x6f\x66\x20\x64\x65\x62\x75\x67\x2e\x4f\x62\x6a\x65\x63\x74\x4d\x69\x72\x72\x6f\x72")->BooleanValue());
  CHECK(CompileRun("\x6f\x33\x5f\x6d\x69\x72\x72\x6f\x72\x20\x69\x6e\x73\x74\x61\x6e\x63\x65\x6f\x66\x20\x64\x65\x62\x75\x67\x2e\x4f\x62\x6a\x65\x63\x74\x4d\x69\x72\x72\x6f\x72")->BooleanValue());

  // Check that each object has one property.
  CHECK_EQ(1, CompileRun(
              "\x6f\x30\x5f\x6d\x69\x72\x72\x6f\x72\x2e\x70\x72\x6f\x70\x65\x72\x74\x79\x4e\x61\x6d\x65\x73\x28\x29\x2e\x6c\x65\x6e\x67\x74\x68")->Int32Value());
  CHECK_EQ(1, CompileRun(
              "\x6f\x31\x5f\x6d\x69\x72\x72\x6f\x72\x2e\x70\x72\x6f\x70\x65\x72\x74\x79\x4e\x61\x6d\x65\x73\x28\x29\x2e\x6c\x65\x6e\x67\x74\x68")->Int32Value());
  CHECK_EQ(1, CompileRun(
              "\x6f\x32\x5f\x6d\x69\x72\x72\x6f\x72\x2e\x70\x72\x6f\x70\x65\x72\x74\x79\x4e\x61\x6d\x65\x73\x28\x29\x2e\x6c\x65\x6e\x67\x74\x68")->Int32Value());
  CHECK_EQ(1, CompileRun(
              "\x6f\x33\x5f\x6d\x69\x72\x72\x6f\x72\x2e\x70\x72\x6f\x70\x65\x72\x74\x79\x4e\x61\x6d\x65\x73\x28\x29\x2e\x6c\x65\x6e\x67\x74\x68")->Int32Value());

  // Set o1 as prototype for o0. o1 has the hidden prototype flag so all
  // properties on o1 should be seen on o0.
  o0->Set(v8::String::NewFromUtf8(isolate, "\x5f\x5f\x70\x72\x6f\x74\x6f\x5f\x5f"), o1);
  CHECK_EQ(2, CompileRun(
              "\x6f\x30\x5f\x6d\x69\x72\x72\x6f\x72\x2e\x70\x72\x6f\x70\x65\x72\x74\x79\x4e\x61\x6d\x65\x73\x28\x29\x2e\x6c\x65\x6e\x67\x74\x68")->Int32Value());
  CHECK_EQ(0, CompileRun(
              "\x6f\x30\x5f\x6d\x69\x72\x72\x6f\x72\x2e\x70\x72\x6f\x70\x65\x72\x74\x79\x28\x27\x78\x27\x29\x2e\x76\x61\x6c\x75\x65\x28\x29\x2e\x76\x61\x6c\x75\x65\x28\x29")->Int32Value());
  CHECK_EQ(1, CompileRun(
              "\x6f\x30\x5f\x6d\x69\x72\x72\x6f\x72\x2e\x70\x72\x6f\x70\x65\x72\x74\x79\x28\x27\x79\x27\x29\x2e\x76\x61\x6c\x75\x65\x28\x29\x2e\x76\x61\x6c\x75\x65\x28\x29")->Int32Value());

  // Set o2 as prototype for o0 (it will end up after o1 as o1 has the hidden
  // prototype flag. o2 also has the hidden prototype flag so all properties
  // on o2 should be seen on o0 as well as properties on o1.
  o0->Set(v8::String::NewFromUtf8(isolate, "\x5f\x5f\x70\x72\x6f\x74\x6f\x5f\x5f"), o2);
  CHECK_EQ(3, CompileRun(
              "\x6f\x30\x5f\x6d\x69\x72\x72\x6f\x72\x2e\x70\x72\x6f\x70\x65\x72\x74\x79\x4e\x61\x6d\x65\x73\x28\x29\x2e\x6c\x65\x6e\x67\x74\x68")->Int32Value());
  CHECK_EQ(0, CompileRun(
              "\x6f\x30\x5f\x6d\x69\x72\x72\x6f\x72\x2e\x70\x72\x6f\x70\x65\x72\x74\x79\x28\x27\x78\x27\x29\x2e\x76\x61\x6c\x75\x65\x28\x29\x2e\x76\x61\x6c\x75\x65\x28\x29")->Int32Value());
  CHECK_EQ(1, CompileRun(
              "\x6f\x30\x5f\x6d\x69\x72\x72\x6f\x72\x2e\x70\x72\x6f\x70\x65\x72\x74\x79\x28\x27\x79\x27\x29\x2e\x76\x61\x6c\x75\x65\x28\x29\x2e\x76\x61\x6c\x75\x65\x28\x29")->Int32Value());
  CHECK_EQ(2, CompileRun(
              "\x6f\x30\x5f\x6d\x69\x72\x72\x6f\x72\x2e\x70\x72\x6f\x70\x65\x72\x74\x79\x28\x27\x7a\x27\x29\x2e\x76\x61\x6c\x75\x65\x28\x29\x2e\x76\x61\x6c\x75\x65\x28\x29")->Int32Value());

  // Set o3 as prototype for o0 (it will end up after o1 and o2 as both o1 and
  // o2 has the hidden prototype flag. o3 does not have the hidden prototype
  // flag so properties on o3 should not be seen on o0 whereas the properties
  // from o1 and o2 should still be seen on o0.
  // Final prototype chain: o0 -> o1 -> o2 -> o3
  // Hidden prototypes:           ^^    ^^
  o0->Set(v8::String::NewFromUtf8(isolate, "\x5f\x5f\x70\x72\x6f\x74\x6f\x5f\x5f"), o3);
  CHECK_EQ(3, CompileRun(
              "\x6f\x30\x5f\x6d\x69\x72\x72\x6f\x72\x2e\x70\x72\x6f\x70\x65\x72\x74\x79\x4e\x61\x6d\x65\x73\x28\x29\x2e\x6c\x65\x6e\x67\x74\x68")->Int32Value());
  CHECK_EQ(1, CompileRun(
              "\x6f\x33\x5f\x6d\x69\x72\x72\x6f\x72\x2e\x70\x72\x6f\x70\x65\x72\x74\x79\x4e\x61\x6d\x65\x73\x28\x29\x2e\x6c\x65\x6e\x67\x74\x68")->Int32Value());
  CHECK_EQ(0, CompileRun(
              "\x6f\x30\x5f\x6d\x69\x72\x72\x6f\x72\x2e\x70\x72\x6f\x70\x65\x72\x74\x79\x28\x27\x78\x27\x29\x2e\x76\x61\x6c\x75\x65\x28\x29\x2e\x76\x61\x6c\x75\x65\x28\x29")->Int32Value());
  CHECK_EQ(1, CompileRun(
              "\x6f\x30\x5f\x6d\x69\x72\x72\x6f\x72\x2e\x70\x72\x6f\x70\x65\x72\x74\x79\x28\x27\x79\x27\x29\x2e\x76\x61\x6c\x75\x65\x28\x29\x2e\x76\x61\x6c\x75\x65\x28\x29")->Int32Value());
  CHECK_EQ(2, CompileRun(
              "\x6f\x30\x5f\x6d\x69\x72\x72\x6f\x72\x2e\x70\x72\x6f\x70\x65\x72\x74\x79\x28\x27\x7a\x27\x29\x2e\x76\x61\x6c\x75\x65\x28\x29\x2e\x76\x61\x6c\x75\x65\x28\x29")->Int32Value());
  CHECK(CompileRun("\x6f\x30\x5f\x6d\x69\x72\x72\x6f\x72\x2e\x70\x72\x6f\x70\x65\x72\x74\x79\x28\x27\x75\x27\x29\x2e\x69\x73\x55\x6e\x64\x65\x66\x69\x6e\x65\x64\x28\x29")->BooleanValue());

  // The prototype (__proto__) for o0 should be o3 as o1 and o2 are hidden.
  CHECK(CompileRun("\x6f\x30\x5f\x6d\x69\x72\x72\x6f\x72\x2e\x70\x72\x6f\x74\x6f\x4f\x62\x6a\x65\x63\x74\x28\x29\x20\x3d\x3d\x20\x6f\x33\x5f\x6d\x69\x72\x72\x6f\x72")->BooleanValue());
}


static void ProtperyXNativeGetter(
    v8::Local<v8::String> property,
    const v8::PropertyCallbackInfo<v8::Value>& info) {
  info.GetReturnValue().Set(10);
}


TEST(NativeGetterPropertyMirror) {
  // Create a V8 environment with debug access.
  DebugLocalContext env;
  v8::Isolate* isolate = env->GetIsolate();
  v8::HandleScope scope(isolate);
  env.ExposeDebug();

  v8::Handle<v8::String> name = v8::String::NewFromUtf8(isolate, "\x78");
  // Create object with named accessor.
  v8::Handle<v8::ObjectTemplate> named = v8::ObjectTemplate::New(isolate);
  named->SetAccessor(name, &ProtperyXNativeGetter, NULL,
      v8::Handle<v8::Value>(), v8::DEFAULT, v8::None);

  // Create object with named property getter.
  env->Global()->Set(v8::String::NewFromUtf8(isolate, "\x69\x6e\x73\x74\x61\x6e\x63\x65"),
                     named->NewInstance());
  CHECK_EQ(10, CompileRun("\x69\x6e\x73\x74\x61\x6e\x63\x65\x2e\x78")->Int32Value());

  // Get mirror for the object with property getter.
  CompileRun("\x76\x61\x72\x20\x69\x6e\x73\x74\x61\x6e\x63\x65\x5f\x6d\x69\x72\x72\x6f\x72\x20\x3d\x20\x64\x65\x62\x75\x67\x2e\x4d\x61\x6b\x65\x4d\x69\x72\x72\x6f\x72\x28\x69\x6e\x73\x74\x61\x6e\x63\x65\x29\x3b");
  CHECK(CompileRun(
      "\x69\x6e\x73\x74\x61\x6e\x63\x65\x5f\x6d\x69\x72\x72\x6f\x72\x20\x69\x6e\x73\x74\x61\x6e\x63\x65\x6f\x66\x20\x64\x65\x62\x75\x67\x2e\x4f\x62\x6a\x65\x63\x74\x4d\x69\x72\x72\x6f\x72")->BooleanValue());

  CompileRun("\x76\x61\x72\x20\x6e\x61\x6d\x65\x64\x5f\x6e\x61\x6d\x65\x73\x20\x3d\x20\x69\x6e\x73\x74\x61\x6e\x63\x65\x5f\x6d\x69\x72\x72\x6f\x72\x2e\x70\x72\x6f\x70\x65\x72\x74\x79\x4e\x61\x6d\x65\x73\x28\x29\x3b");
  CHECK_EQ(1, CompileRun("\x6e\x61\x6d\x65\x64\x5f\x6e\x61\x6d\x65\x73\x2e\x6c\x65\x6e\x67\x74\x68")->Int32Value());
  CHECK(CompileRun("\x6e\x61\x6d\x65\x64\x5f\x6e\x61\x6d\x65\x73\x5b\x30\x5d\x20\x3d\x3d\x20\x27\x78\x27")->BooleanValue());
  CHECK(CompileRun(
      "\x69\x6e\x73\x74\x61\x6e\x63\x65\x5f\x6d\x69\x72\x72\x6f\x72\x2e\x70\x72\x6f\x70\x65\x72\x74\x79\x28\x27\x78\x27\x29\x2e\x76\x61\x6c\x75\x65\x28\x29\x2e\x69\x73\x4e\x75\x6d\x62\x65\x72\x28\x29")->BooleanValue());
  CHECK(CompileRun(
      "\x69\x6e\x73\x74\x61\x6e\x63\x65\x5f\x6d\x69\x72\x72\x6f\x72\x2e\x70\x72\x6f\x70\x65\x72\x74\x79\x28\x27\x78\x27\x29\x2e\x76\x61\x6c\x75\x65\x28\x29\x2e\x76\x61\x6c\x75\x65\x28\x29\x20\x3d\x3d\x20\x31\x30")->BooleanValue());
}


static void ProtperyXNativeGetterThrowingError(
    v8::Local<v8::String> property,
    const v8::PropertyCallbackInfo<v8::Value>& info) {
  CompileRun("\x74\x68\x72\x6f\x77\x20\x6e\x65\x77\x20\x45\x72\x72\x6f\x72\x28\x27\x45\x72\x72\x6f\x72\x20\x6d\x65\x73\x73\x61\x67\x65\x27\x29\x3b");
}


TEST(NativeGetterThrowingErrorPropertyMirror) {
  // Create a V8 environment with debug access.
  DebugLocalContext env;
  v8::Isolate* isolate = env->GetIsolate();
  v8::HandleScope scope(isolate);
  env.ExposeDebug();

  v8::Handle<v8::String> name = v8::String::NewFromUtf8(isolate, "\x78");
  // Create object with named accessor.
  v8::Handle<v8::ObjectTemplate> named = v8::ObjectTemplate::New(isolate);
  named->SetAccessor(name, &ProtperyXNativeGetterThrowingError, NULL,
      v8::Handle<v8::Value>(), v8::DEFAULT, v8::None);

  // Create object with named property getter.
  env->Global()->Set(v8::String::NewFromUtf8(isolate, "\x69\x6e\x73\x74\x61\x6e\x63\x65"),
                     named->NewInstance());

  // Get mirror for the object with property getter.
  CompileRun("\x76\x61\x72\x20\x69\x6e\x73\x74\x61\x6e\x63\x65\x5f\x6d\x69\x72\x72\x6f\x72\x20\x3d\x20\x64\x65\x62\x75\x67\x2e\x4d\x61\x6b\x65\x4d\x69\x72\x72\x6f\x72\x28\x69\x6e\x73\x74\x61\x6e\x63\x65\x29\x3b");
  CHECK(CompileRun(
      "\x69\x6e\x73\x74\x61\x6e\x63\x65\x5f\x6d\x69\x72\x72\x6f\x72\x20\x69\x6e\x73\x74\x61\x6e\x63\x65\x6f\x66\x20\x64\x65\x62\x75\x67\x2e\x4f\x62\x6a\x65\x63\x74\x4d\x69\x72\x72\x6f\x72")->BooleanValue());
  CompileRun("\x6e\x61\x6d\x65\x64\x5f\x6e\x61\x6d\x65\x73\x20\x3d\x20\x69\x6e\x73\x74\x61\x6e\x63\x65\x5f\x6d\x69\x72\x72\x6f\x72\x2e\x70\x72\x6f\x70\x65\x72\x74\x79\x4e\x61\x6d\x65\x73\x28\x29\x3b");
  CHECK_EQ(1, CompileRun("\x6e\x61\x6d\x65\x64\x5f\x6e\x61\x6d\x65\x73\x2e\x6c\x65\x6e\x67\x74\x68")->Int32Value());
  CHECK(CompileRun("\x6e\x61\x6d\x65\x64\x5f\x6e\x61\x6d\x65\x73\x5b\x30\x5d\x20\x3d\x3d\x20\x27\x78\x27")->BooleanValue());
  CHECK(CompileRun(
      "\x69\x6e\x73\x74\x61\x6e\x63\x65\x5f\x6d\x69\x72\x72\x6f\x72\x2e\x70\x72\x6f\x70\x65\x72\x74\x79\x28\x27\x78\x27\x29\x2e\x76\x61\x6c\x75\x65\x28\x29\x2e\x69\x73\x45\x72\x72\x6f\x72\x28\x29")->BooleanValue());

  // Check that the message is that passed to the Error constructor.
  CHECK(CompileRun(
      "\x69\x6e\x73\x74\x61\x6e\x63\x65\x5f\x6d\x69\x72\x72\x6f\x72\x2e\x70\x72\x6f\x70\x65\x72\x74\x79\x28\x27\x78\x27\x29\x2e\x76\x61\x6c\x75\x65\x28\x29\x2e\x6d\x65\x73\x73\x61\x67\x65\x28\x29\x20\x3d\x3d\x20\x27\x45\x72\x72\x6f\x72\x20\x6d\x65\x73\x73\x61\x67\x65\x27")->
          BooleanValue());
}


// Test that hidden properties object is not returned as an unnamed property
// among regular properties.
// See http://crbug.com/26491
TEST(NoHiddenProperties) {
  // Create a V8 environment with debug access.
  DebugLocalContext env;
  v8::Isolate* isolate = env->GetIsolate();
  v8::HandleScope scope(isolate);
  env.ExposeDebug();

  // Create an object in the global scope.
  const char* source = "\x76\x61\x72\x20\x6f\x62\x6a\x20\x3d\x20\x7b\x61\x3a\x20\x31\x7d\x3b";
  v8::Script::Compile(v8::String::NewFromUtf8(isolate, source))
      ->Run();
  v8::Local<v8::Object> obj = v8::Local<v8::Object>::Cast(
      env->Global()->Get(v8::String::NewFromUtf8(isolate, "\x6f\x62\x6a")));
  // Set a hidden property on the object.
  obj->SetHiddenValue(
      v8::String::NewFromUtf8(isolate, "\x76\x38\x3a\x3a\x74\x65\x73\x74\x2d\x64\x65\x62\x75\x67\x3a\x3a\x61"),
      v8::Int32::New(isolate, 11));

  // Get mirror for the object with property getter.
  CompileRun("\x76\x61\x72\x20\x6f\x62\x6a\x5f\x6d\x69\x72\x72\x6f\x72\x20\x3d\x20\x64\x65\x62\x75\x67\x2e\x4d\x61\x6b\x65\x4d\x69\x72\x72\x6f\x72\x28\x6f\x62\x6a\x29\x3b");
  CHECK(CompileRun(
      "\x6f\x62\x6a\x5f\x6d\x69\x72\x72\x6f\x72\x20\x69\x6e\x73\x74\x61\x6e\x63\x65\x6f\x66\x20\x64\x65\x62\x75\x67\x2e\x4f\x62\x6a\x65\x63\x74\x4d\x69\x72\x72\x6f\x72")->BooleanValue());
  CompileRun("\x76\x61\x72\x20\x6e\x61\x6d\x65\x64\x5f\x6e\x61\x6d\x65\x73\x20\x3d\x20\x6f\x62\x6a\x5f\x6d\x69\x72\x72\x6f\x72\x2e\x70\x72\x6f\x70\x65\x72\x74\x79\x4e\x61\x6d\x65\x73\x28\x29\x3b");
  // There should be exactly one property. But there is also an unnamed
  // property whose value is hidden properties dictionary. The latter
  // property should not be in the list of reguar properties.
  CHECK_EQ(1, CompileRun("\x6e\x61\x6d\x65\x64\x5f\x6e\x61\x6d\x65\x73\x2e\x6c\x65\x6e\x67\x74\x68")->Int32Value());
  CHECK(CompileRun("\x6e\x61\x6d\x65\x64\x5f\x6e\x61\x6d\x65\x73\x5b\x30\x5d\x20\x3d\x3d\x20\x27\x61\x27")->BooleanValue());
  CHECK(CompileRun(
      "\x6f\x62\x6a\x5f\x6d\x69\x72\x72\x6f\x72\x2e\x70\x72\x6f\x70\x65\x72\x74\x79\x28\x27\x61\x27\x29\x2e\x76\x61\x6c\x75\x65\x28\x29\x2e\x76\x61\x6c\x75\x65\x28\x29\x20\x3d\x3d\x20\x31")->BooleanValue());

  // Object created by t0 will become hidden prototype of object 'obj'.
  v8::Handle<v8::FunctionTemplate> t0 = v8::FunctionTemplate::New(isolate);
  t0->InstanceTemplate()->Set(v8::String::NewFromUtf8(isolate, "\x62"),
                              v8::Number::New(isolate, 2));
  t0->SetHiddenPrototype(true);
  v8::Handle<v8::FunctionTemplate> t1 = v8::FunctionTemplate::New(isolate);
  t1->InstanceTemplate()->Set(v8::String::NewFromUtf8(isolate, "\x63"),
                              v8::Number::New(isolate, 3));

  // Create proto objects, add hidden properties to them and set them on
  // the global object.
  v8::Handle<v8::Object> protoObj = t0->GetFunction()->NewInstance();
  protoObj->SetHiddenValue(
      v8::String::NewFromUtf8(isolate, "\x76\x38\x3a\x3a\x74\x65\x73\x74\x2d\x64\x65\x62\x75\x67\x3a\x3a\x62"),
      v8::Int32::New(isolate, 12));
  env->Global()->Set(v8::String::NewFromUtf8(isolate, "\x70\x72\x6f\x74\x6f\x4f\x62\x6a"),
                     protoObj);
  v8::Handle<v8::Object> grandProtoObj = t1->GetFunction()->NewInstance();
  grandProtoObj->SetHiddenValue(
      v8::String::NewFromUtf8(isolate, "\x76\x38\x3a\x3a\x74\x65\x73\x74\x2d\x64\x65\x62\x75\x67\x3a\x3a\x63"),
      v8::Int32::New(isolate, 13));
  env->Global()->Set(
      v8::String::NewFromUtf8(isolate, "\x67\x72\x61\x6e\x64\x50\x72\x6f\x74\x6f\x4f\x62\x6a"),
      grandProtoObj);

  // Setting prototypes: obj->protoObj->grandProtoObj
  protoObj->Set(v8::String::NewFromUtf8(isolate, "\x5f\x5f\x70\x72\x6f\x74\x6f\x5f\x5f"),
                grandProtoObj);
  obj->Set(v8::String::NewFromUtf8(isolate, "\x5f\x5f\x70\x72\x6f\x74\x6f\x5f\x5f"), protoObj);

  // Get mirror for the object with property getter.
  CompileRun("\x76\x61\x72\x20\x6f\x62\x6a\x5f\x6d\x69\x72\x72\x6f\x72\x20\x3d\x20\x64\x65\x62\x75\x67\x2e\x4d\x61\x6b\x65\x4d\x69\x72\x72\x6f\x72\x28\x6f\x62\x6a\x29\x3b");
  CHECK(CompileRun(
      "\x6f\x62\x6a\x5f\x6d\x69\x72\x72\x6f\x72\x20\x69\x6e\x73\x74\x61\x6e\x63\x65\x6f\x66\x20\x64\x65\x62\x75\x67\x2e\x4f\x62\x6a\x65\x63\x74\x4d\x69\x72\x72\x6f\x72")->BooleanValue());
  CompileRun("\x76\x61\x72\x20\x6e\x61\x6d\x65\x64\x5f\x6e\x61\x6d\x65\x73\x20\x3d\x20\x6f\x62\x6a\x5f\x6d\x69\x72\x72\x6f\x72\x2e\x70\x72\x6f\x70\x65\x72\x74\x79\x4e\x61\x6d\x65\x73\x28\x29\x3b");
  // There should be exactly two properties - one from the object itself and
  // another from its hidden prototype.
  CHECK_EQ(2, CompileRun("\x6e\x61\x6d\x65\x64\x5f\x6e\x61\x6d\x65\x73\x2e\x6c\x65\x6e\x67\x74\x68")->Int32Value());
  CHECK(CompileRun("\x6e\x61\x6d\x65\x64\x5f\x6e\x61\x6d\x65\x73\x2e\x73\x6f\x72\x74\x28\x29\x3b\x20\x6e\x61\x6d\x65\x64\x5f\x6e\x61\x6d\x65\x73\x5b\x30\x5d\x20\x3d\x3d\x20\x27\x61\x27\x20\x26\x26"
                   "\x6e\x61\x6d\x65\x64\x5f\x6e\x61\x6d\x65\x73\x5b\x31\x5d\x20\x3d\x3d\x20\x27\x62\x27")->BooleanValue());
  CHECK(CompileRun(
      "\x6f\x62\x6a\x5f\x6d\x69\x72\x72\x6f\x72\x2e\x70\x72\x6f\x70\x65\x72\x74\x79\x28\x27\x61\x27\x29\x2e\x76\x61\x6c\x75\x65\x28\x29\x2e\x76\x61\x6c\x75\x65\x28\x29\x20\x3d\x3d\x20\x31")->BooleanValue());
  CHECK(CompileRun(
      "\x6f\x62\x6a\x5f\x6d\x69\x72\x72\x6f\x72\x2e\x70\x72\x6f\x70\x65\x72\x74\x79\x28\x27\x62\x27\x29\x2e\x76\x61\x6c\x75\x65\x28\x29\x2e\x76\x61\x6c\x75\x65\x28\x29\x20\x3d\x3d\x20\x32")->BooleanValue());
}


// Multithreaded tests of JSON debugger protocol

// Support classes

// Provides synchronization between N threads, where N is a template parameter.
// The Wait() call blocks a thread until it is called for the Nth time, then all
// calls return.  Each ThreadBarrier object can only be used once.
template <int N>
class ThreadBarrier V8_FINAL {
 public:
  ThreadBarrier() : num_blocked_(0) {}

  ~ThreadBarrier() {
    LockGuard<Mutex> lock_guard(&mutex_);
    if (num_blocked_ != 0) {
      CHECK_EQ(N, num_blocked_);
    }
  }

  void Wait() {
    LockGuard<Mutex> lock_guard(&mutex_);
    CHECK_LT(num_blocked_, N);
    num_blocked_++;
    if (N == num_blocked_) {
      // Signal and unblock all waiting threads.
      cv_.NotifyAll();
      printf("\x42\x41\x52\x52\x49\x45\x52\xa\xa");
      fflush(stdout);
    } else {  // Wait for the semaphore.
      while (num_blocked_ < N) {
        cv_.Wait(&mutex_);
      }
    }
    CHECK_EQ(N, num_blocked_);
  }

 private:
  ConditionVariable cv_;
  Mutex mutex_;
  int num_blocked_;

  STATIC_ASSERT(N > 0);

  DISALLOW_COPY_AND_ASSIGN(ThreadBarrier);
};


// A set containing enough barriers and semaphores for any of the tests.
class Barriers {
 public:
  Barriers() : semaphore_1(0), semaphore_2(0) {}
  ThreadBarrier<2> barrier_1;
  ThreadBarrier<2> barrier_2;
  ThreadBarrier<2> barrier_3;
  ThreadBarrier<2> barrier_4;
  ThreadBarrier<2> barrier_5;
  v8::base::Semaphore semaphore_1;
  v8::base::Semaphore semaphore_2;
};


// We match parts of the message to decide if it is a break message.
bool IsBreakEventMessage(char *message) {
  const char* type_event = "\x22\x74\x79\x70\x65\x22\x3a\x22\x65\x76\x65\x6e\x74\x22";
  const char* event_break = "\x22\x65\x76\x65\x6e\x74\x22\x3a\x22\x62\x72\x65\x61\x6b\x22";
  // Does the message contain both type:event and event:break?
  return strstr(message, type_event) != NULL &&
         strstr(message, event_break) != NULL;
}


// We match parts of the message to decide if it is a exception message.
bool IsExceptionEventMessage(char *message) {
  const char* type_event = "\x22\x74\x79\x70\x65\x22\x3a\x22\x65\x76\x65\x6e\x74\x22";
  const char* event_exception = "\x22\x65\x76\x65\x6e\x74\x22\x3a\x22\x65\x78\x63\x65\x70\x74\x69\x6f\x6e\x22";
  // Does the message contain both type:event and event:exception?
  return strstr(message, type_event) != NULL &&
      strstr(message, event_exception) != NULL;
}


// We match the message wether it is an evaluate response message.
bool IsEvaluateResponseMessage(char* message) {
  const char* type_response = "\x22\x74\x79\x70\x65\x22\x3a\x22\x72\x65\x73\x70\x6f\x6e\x73\x65\x22";
  const char* command_evaluate = "\x22\x63\x6f\x6d\x6d\x61\x6e\x64\x22\x3a\x22\x65\x76\x61\x6c\x75\x61\x74\x65\x22";
  // Does the message contain both type:response and command:evaluate?
  return strstr(message, type_response) != NULL &&
         strstr(message, command_evaluate) != NULL;
}


static int StringToInt(const char* s) {
  return atoi(s);  // NOLINT
}


// We match parts of the message to get evaluate result int value.
int GetEvaluateIntResult(char *message) {
  const char* value = "\x22\x76\x61\x6c\x75\x65\x22\x3a";
  char* pos = strstr(message, value);
  if (pos == NULL) {
    return -1;
  }
  int res = -1;
  res = StringToInt(pos + strlen(value));
  return res;
}


// We match parts of the message to get hit breakpoint id.
int GetBreakpointIdFromBreakEventMessage(char *message) {
  const char* breakpoints = "\x22\x62\x72\x65\x61\x6b\x70\x6f\x69\x6e\x74\x73\x22\x3a\x5b";
  char* pos = strstr(message, breakpoints);
  if (pos == NULL) {
    return -1;
  }
  int res = -1;
  res = StringToInt(pos + strlen(breakpoints));
  return res;
}


// We match parts of the message to get total frames number.
int GetTotalFramesInt(char *message) {
  const char* prefix = "\x22\x74\x6f\x74\x61\x6c\x46\x72\x61\x6d\x65\x73\x22\x3a";
  char* pos = strstr(message, prefix);
  if (pos == NULL) {
    return -1;
  }
  pos += strlen(prefix);
  int res = StringToInt(pos);
  return res;
}


// We match parts of the message to get source line.
int GetSourceLineFromBreakEventMessage(char *message) {
  const char* source_line = "\x22\x73\x6f\x75\x72\x63\x65\x4c\x69\x6e\x65\x22\x3a";
  char* pos = strstr(message, source_line);
  if (pos == NULL) {
    return -1;
  }
  int res = -1;
  res = StringToInt(pos + strlen(source_line));
  return res;
}


/* Test MessageQueues */
/* Tests the message queues that hold debugger commands and
 * response messages to the debugger.  Fills queues and makes
 * them grow.
 */
Barriers message_queue_barriers;

// This is the debugger thread, that executes no v8 calls except
// placing JSON debugger commands in the queue.
class MessageQueueDebuggerThread : public v8::base::Thread {
 public:
  MessageQueueDebuggerThread()
      : Thread(Options("\x4d\x65\x73\x73\x61\x67\x65\x51\x75\x65\x75\x65\x44\x65\x62\x75\x67\x67\x65\x72\x54\x68\x72\x65\x61\x64")) {}
  void Run();
};


static void MessageHandler(const v8::Debug::Message& message) {
  v8::Handle<v8::String> json = message.GetJSON();
  v8::String::Utf8Value utf8(json);
  if (IsBreakEventMessage(*utf8)) {
    // Lets test script wait until break occurs to send commands.
    // Signals when a break is reported.
    message_queue_barriers.semaphore_2.Signal();
  }

  // Allow message handler to block on a semaphore, to test queueing of
  // messages while blocked.
  message_queue_barriers.semaphore_1.Wait();
}


void MessageQueueDebuggerThread::Run() {
  const int kBufferSize = 1000;
  uint16_t buffer_1[kBufferSize];
  uint16_t buffer_2[kBufferSize];
  const char* command_1 =
      "\x7b\x22\x73\x65\x71\x22\x3a\x31\x31\x37\x2c"
       "\x22\x74\x79\x70\x65\x22\x3a\x22\x72\x65\x71\x75\x65\x73\x74\x22\x2c"
       "\x22\x63\x6f\x6d\x6d\x61\x6e\x64\x22\x3a\x22\x65\x76\x61\x6c\x75\x61\x74\x65\x22\x2c"
       "\x22\x61\x72\x67\x75\x6d\x65\x6e\x74\x73\x22\x3a\x7b\x22\x65\x78\x70\x72\x65\x73\x73\x69\x6f\x6e\x22\x3a\x22\x31\x2b\x32\x22\x7d\x7d";
  const char* command_2 =
    "\x7b\x22\x73\x65\x71\x22\x3a\x31\x31\x38\x2c"
     "\x22\x74\x79\x70\x65\x22\x3a\x22\x72\x65\x71\x75\x65\x73\x74\x22\x2c"
     "\x22\x63\x6f\x6d\x6d\x61\x6e\x64\x22\x3a\x22\x65\x76\x61\x6c\x75\x61\x74\x65\x22\x2c"
     "\x22\x61\x72\x67\x75\x6d\x65\x6e\x74\x73\x22\x3a\x7b\x22\x65\x78\x70\x72\x65\x73\x73\x69\x6f\x6e\x22\x3a\x22\x31\x2b\x61\x22\x7d\x7d";
  const char* command_3 =
    "\x7b\x22\x73\x65\x71\x22\x3a\x31\x31\x39\x2c"
     "\x22\x74\x79\x70\x65\x22\x3a\x22\x72\x65\x71\x75\x65\x73\x74\x22\x2c"
     "\x22\x63\x6f\x6d\x6d\x61\x6e\x64\x22\x3a\x22\x65\x76\x61\x6c\x75\x61\x74\x65\x22\x2c"
     "\x22\x61\x72\x67\x75\x6d\x65\x6e\x74\x73\x22\x3a\x7b\x22\x65\x78\x70\x72\x65\x73\x73\x69\x6f\x6e\x22\x3a\x22\x63\x2e\x64\x20\x2a\x20\x62\x22\x7d\x7d";
  const char* command_continue =
    "\x7b\x22\x73\x65\x71\x22\x3a\x31\x30\x36\x2c"
     "\x22\x74\x79\x70\x65\x22\x3a\x22\x72\x65\x71\x75\x65\x73\x74\x22\x2c"
     "\x22\x63\x6f\x6d\x6d\x61\x6e\x64\x22\x3a\x22\x63\x6f\x6e\x74\x69\x6e\x75\x65\x22\x7d";
  const char* command_single_step =
    "\x7b\x22\x73\x65\x71\x22\x3a\x31\x30\x37\x2c"
     "\x22\x74\x79\x70\x65\x22\x3a\x22\x72\x65\x71\x75\x65\x73\x74\x22\x2c"
     "\x22\x63\x6f\x6d\x6d\x61\x6e\x64\x22\x3a\x22\x63\x6f\x6e\x74\x69\x6e\x75\x65\x22\x2c"
     "\x22\x61\x72\x67\x75\x6d\x65\x6e\x74\x73\x22\x3a\x7b\x22\x73\x74\x65\x70\x61\x63\x74\x69\x6f\x6e\x22\x3a\x22\x6e\x65\x78\x74\x22\x7d\x7d";

  /* Interleaved sequence of actions by the two threads:*/
  // Main thread compiles and runs source_1
  message_queue_barriers.semaphore_1.Signal();
  message_queue_barriers.barrier_1.Wait();
  // Post 6 commands, filling the command queue and making it expand.
  // These calls return immediately, but the commands stay on the queue
  // until the execution of source_2.
  // Note: AsciiToUtf16 executes before SendCommand, so command is copied
  // to buffer before buffer is sent to SendCommand.
  v8::Isolate* isolate = CcTest::isolate();
  v8::Debug::SendCommand(isolate, buffer_1, AsciiToUtf16(command_1, buffer_1));
  v8::Debug::SendCommand(isolate, buffer_2, AsciiToUtf16(command_2, buffer_2));
  v8::Debug::SendCommand(isolate, buffer_2, AsciiToUtf16(command_3, buffer_2));
  v8::Debug::SendCommand(isolate, buffer_2, AsciiToUtf16(command_3, buffer_2));
  v8::Debug::SendCommand(isolate, buffer_2, AsciiToUtf16(command_3, buffer_2));
  message_queue_barriers.barrier_2.Wait();
  // Main thread compiles and runs source_2.
  // Queued commands are executed at the start of compilation of source_2(
  // beforeCompile event).
  // Free the message handler to process all the messages from the queue. 7
  // messages are expected: 2 afterCompile events and 5 responses.
  // All the commands added so far will fail to execute as long as call stack
  // is empty on beforeCompile event.
  for (int i = 0; i < 6 ; ++i) {
    message_queue_barriers.semaphore_1.Signal();
  }
  message_queue_barriers.barrier_3.Wait();
  // Main thread compiles and runs source_3.
  // Don't stop in the afterCompile handler.
  message_queue_barriers.semaphore_1.Signal();
  // source_3 includes a debugger statement, which causes a break event.
  // Wait on break event from hitting "debugger" statement
  message_queue_barriers.semaphore_2.Wait();
  // These should execute after the "debugger" statement in source_2
  v8::Debug::SendCommand(isolate, buffer_1, AsciiToUtf16(command_1, buffer_1));
  v8::Debug::SendCommand(isolate, buffer_2, AsciiToUtf16(command_2, buffer_2));
  v8::Debug::SendCommand(isolate, buffer_2, AsciiToUtf16(command_3, buffer_2));
  v8::Debug::SendCommand(
      isolate, buffer_2, AsciiToUtf16(command_single_step, buffer_2));
  // Run after 2 break events, 4 responses.
  for (int i = 0; i < 6 ; ++i) {
    message_queue_barriers.semaphore_1.Signal();
  }
  // Wait on break event after a single step executes.
  message_queue_barriers.semaphore_2.Wait();
  v8::Debug::SendCommand(isolate, buffer_1, AsciiToUtf16(command_2, buffer_1));
  v8::Debug::SendCommand(
      isolate, buffer_2, AsciiToUtf16(command_continue, buffer_2));
  // Run after 2 responses.
  for (int i = 0; i < 2 ; ++i) {
    message_queue_barriers.semaphore_1.Signal();
  }
  // Main thread continues running source_3 to end, waits for this thread.
}


// This thread runs the v8 engine.
TEST(MessageQueues) {
  MessageQueueDebuggerThread message_queue_debugger_thread;

  // Create a V8 environment
  DebugLocalContext env;
  v8::HandleScope scope(env->GetIsolate());
  v8::Debug::SetMessageHandler(MessageHandler);
  message_queue_debugger_thread.Start();

  const char* source_1 = "\x61\x20\x3d\x20\x33\x3b\x20\x62\x20\x3d\x20\x34\x3b\x20\x63\x20\x3d\x20\x6e\x65\x77\x20\x4f\x62\x6a\x65\x63\x74\x28\x29\x3b\x20\x63\x2e\x64\x20\x3d\x20\x35\x3b";
  const char* source_2 = "\x65\x20\x3d\x20\x31\x37\x3b";
  const char* source_3 = "\x61\x20\x3d\x20\x34\x3b\x20\x64\x65\x62\x75\x67\x67\x65\x72\x3b\x20\x61\x20\x3d\x20\x35\x3b\x20\x61\x20\x3d\x20\x36\x3b\x20\x61\x20\x3d\x20\x37\x3b";

  // See MessageQueueDebuggerThread::Run for interleaved sequence of
  // API calls and events in the two threads.
  CompileRun(source_1);
  message_queue_barriers.barrier_1.Wait();
  message_queue_barriers.barrier_2.Wait();
  CompileRun(source_2);
  message_queue_barriers.barrier_3.Wait();
  CompileRun(source_3);
  message_queue_debugger_thread.Join();
  fflush(stdout);
}


class TestClientData : public v8::Debug::ClientData {
 public:
  TestClientData() {
    constructor_call_counter++;
  }
  virtual ~TestClientData() {
    destructor_call_counter++;
  }

  static void ResetCounters() {
    constructor_call_counter = 0;
    destructor_call_counter = 0;
  }

  static int constructor_call_counter;
  static int destructor_call_counter;
};

int TestClientData::constructor_call_counter = 0;
int TestClientData::destructor_call_counter = 0;


// Tests that MessageQueue doesn't destroy client data when expands and
// does destroy when it dies.
TEST(MessageQueueExpandAndDestroy) {
  TestClientData::ResetCounters();
  { // Create a scope for the queue.
    CommandMessageQueue queue(1);
    queue.Put(CommandMessage::New(Vector<uint16_t>::empty(),
                                  new TestClientData()));
    queue.Put(CommandMessage::New(Vector<uint16_t>::empty(),
                                  new TestClientData()));
    queue.Put(CommandMessage::New(Vector<uint16_t>::empty(),
                                  new TestClientData()));
    CHECK_EQ(0, TestClientData::destructor_call_counter);
    queue.Get().Dispose();
    CHECK_EQ(1, TestClientData::destructor_call_counter);
    queue.Put(CommandMessage::New(Vector<uint16_t>::empty(),
                                  new TestClientData()));
    queue.Put(CommandMessage::New(Vector<uint16_t>::empty(),
                                  new TestClientData()));
    queue.Put(CommandMessage::New(Vector<uint16_t>::empty(),
                                  new TestClientData()));
    queue.Put(CommandMessage::New(Vector<uint16_t>::empty(),
                                  new TestClientData()));
    queue.Put(CommandMessage::New(Vector<uint16_t>::empty(),
                                  new TestClientData()));
    CHECK_EQ(1, TestClientData::destructor_call_counter);
    queue.Get().Dispose();
    CHECK_EQ(2, TestClientData::destructor_call_counter);
  }
  // All the client data should be destroyed when the queue is destroyed.
  CHECK_EQ(TestClientData::destructor_call_counter,
           TestClientData::destructor_call_counter);
}


static int handled_client_data_instances_count = 0;
static void MessageHandlerCountingClientData(
    const v8::Debug::Message& message) {
  if (message.GetClientData() != NULL) {
    handled_client_data_instances_count++;
  }
}


// Tests that all client data passed to the debugger are sent to the handler.
TEST(SendClientDataToHandler) {
  // Create a V8 environment
  DebugLocalContext env;
  v8::Isolate* isolate = env->GetIsolate();
  v8::HandleScope scope(isolate);
  TestClientData::ResetCounters();
  handled_client_data_instances_count = 0;
  v8::Debug::SetMessageHandler(MessageHandlerCountingClientData);
  const char* source_1 = "\x61\x20\x3d\x20\x33\x3b\x20\x62\x20\x3d\x20\x34\x3b\x20\x63\x20\x3d\x20\x6e\x65\x77\x20\x4f\x62\x6a\x65\x63\x74\x28\x29\x3b\x20\x63\x2e\x64\x20\x3d\x20\x35\x3b";
  const int kBufferSize = 1000;
  uint16_t buffer[kBufferSize];
  const char* command_1 =
      "\x7b\x22\x73\x65\x71\x22\x3a\x31\x31\x37\x2c"
       "\x22\x74\x79\x70\x65\x22\x3a\x22\x72\x65\x71\x75\x65\x73\x74\x22\x2c"
       "\x22\x63\x6f\x6d\x6d\x61\x6e\x64\x22\x3a\x22\x65\x76\x61\x6c\x75\x61\x74\x65\x22\x2c"
       "\x22\x61\x72\x67\x75\x6d\x65\x6e\x74\x73\x22\x3a\x7b\x22\x65\x78\x70\x72\x65\x73\x73\x69\x6f\x6e\x22\x3a\x22\x31\x2b\x32\x22\x7d\x7d";
  const char* command_2 =
    "\x7b\x22\x73\x65\x71\x22\x3a\x31\x31\x38\x2c"
     "\x22\x74\x79\x70\x65\x22\x3a\x22\x72\x65\x71\x75\x65\x73\x74\x22\x2c"
     "\x22\x63\x6f\x6d\x6d\x61\x6e\x64\x22\x3a\x22\x65\x76\x61\x6c\x75\x61\x74\x65\x22\x2c"
     "\x22\x61\x72\x67\x75\x6d\x65\x6e\x74\x73\x22\x3a\x7b\x22\x65\x78\x70\x72\x65\x73\x73\x69\x6f\x6e\x22\x3a\x22\x31\x2b\x61\x22\x7d\x7d";
  const char* command_continue =
    "\x7b\x22\x73\x65\x71\x22\x3a\x31\x30\x36\x2c"
     "\x22\x74\x79\x70\x65\x22\x3a\x22\x72\x65\x71\x75\x65\x73\x74\x22\x2c"
     "\x22\x63\x6f\x6d\x6d\x61\x6e\x64\x22\x3a\x22\x63\x6f\x6e\x74\x69\x6e\x75\x65\x22\x7d";

  v8::Debug::SendCommand(isolate, buffer, AsciiToUtf16(command_1, buffer),
                         new TestClientData());
  v8::Debug::SendCommand(
      isolate, buffer, AsciiToUtf16(command_2, buffer), NULL);
  v8::Debug::SendCommand(isolate, buffer, AsciiToUtf16(command_2, buffer),
                         new TestClientData());
  v8::Debug::SendCommand(isolate, buffer, AsciiToUtf16(command_2, buffer),
                         new TestClientData());
  // All the messages will be processed on beforeCompile event.
  CompileRun(source_1);
  v8::Debug::SendCommand(
      isolate, buffer, AsciiToUtf16(command_continue, buffer));
  CHECK_EQ(3, TestClientData::constructor_call_counter);
  CHECK_EQ(TestClientData::constructor_call_counter,
           handled_client_data_instances_count);
  CHECK_EQ(TestClientData::constructor_call_counter,
           TestClientData::destructor_call_counter);
}


/* Test ThreadedDebugging */
/* This test interrupts a running infinite loop that is
 * occupying the v8 thread by a break command from the
 * debugger thread.  It then changes the value of a
 * global object, to make the loop terminate.
 */

Barriers threaded_debugging_barriers;

class V8Thread : public v8::base::Thread {
 public:
  V8Thread() : Thread(Options("\x56\x38\x54\x68\x72\x65\x61\x64")) {}
  void Run();
};

class DebuggerThread : public v8::base::Thread {
 public:
  DebuggerThread() : Thread(Options("\x44\x65\x62\x75\x67\x67\x65\x72\x54\x68\x72\x65\x61\x64")) {}
  void Run();
};


static void ThreadedAtBarrier1(
    const v8::FunctionCallbackInfo<v8::Value>& args) {
  threaded_debugging_barriers.barrier_1.Wait();
}


static void ThreadedMessageHandler(const v8::Debug::Message& message) {
  static char print_buffer[1000];
  v8::String::Value json(message.GetJSON());
  Utf16ToAscii(*json, json.length(), print_buffer);
  if (IsBreakEventMessage(print_buffer)) {
    // Check that we are inside the while loop.
    int source_line = GetSourceLineFromBreakEventMessage(print_buffer);
    CHECK(8 <= source_line && source_line <= 13);
    threaded_debugging_barriers.barrier_2.Wait();
  }
}


void V8Thread::Run() {
  const char* source =
      "\x66\x6c\x61\x67\x20\x3d\x20\x74\x72\x75\x65\x3b\xa"
      "\x66\x75\x6e\x63\x74\x69\x6f\x6e\x20\x62\x61\x72\x28\x20\x6e\x65\x77\x5f\x76\x61\x6c\x75\x65\x20\x29\x20\x7b\xa"
      "\x20\x20\x66\x6c\x61\x67\x20\x3d\x20\x6e\x65\x77\x5f\x76\x61\x6c\x75\x65\x3b\xa"
      "\x20\x20\x72\x65\x74\x75\x72\x6e\x20\x22\x52\x65\x74\x75\x72\x6e\x20\x66\x72\x6f\x6d\x20\x62\x61\x72\x28\x22\x20\x2b\x20\x6e\x65\x77\x5f\x76\x61\x6c\x75\x65\x20\x2b\x20\x22\x29\x22\x3b\xa"
      "\x7d\xa"
      "\xa"
      "\x66\x75\x6e\x63\x74\x69\x6f\x6e\x20\x66\x6f\x6f\x28\x29\x20\x7b\xa"
      "\x20\x20\x76\x61\x72\x20\x78\x20\x3d\x20\x31\x3b\xa"
      "\x20\x20\x77\x68\x69\x6c\x65\x20\x28\x20\x66\x6c\x61\x67\x20\x3d\x3d\x20\x74\x72\x75\x65\x20\x29\x20\x7b\xa"
      "\x20\x20\x20\x20\x69\x66\x20\x28\x20\x78\x20\x3d\x3d\x20\x31\x20\x29\x20\x7b\xa"
      "\x20\x20\x20\x20\x20\x20\x54\x68\x72\x65\x61\x64\x65\x64\x41\x74\x42\x61\x72\x72\x69\x65\x72\x31\x28\x29\x3b\xa"
      "\x20\x20\x20\x20\x7d\xa"
      "\x20\x20\x20\x20\x78\x20\x3d\x20\x78\x20\x2b\x20\x31\x3b\xa"
      "\x20\x20\x7d\xa"
      "\x7d\xa"
      "\xa"
      "\x66\x6f\x6f\x28\x29\x3b\xa";

  v8::Isolate* isolate = CcTest::isolate();
  v8::Isolate::Scope isolate_scope(isolate);
  DebugLocalContext env;
  v8::HandleScope scope(env->GetIsolate());
  v8::Debug::SetMessageHandler(&ThreadedMessageHandler);
  v8::Handle<v8::ObjectTemplate> global_template =
      v8::ObjectTemplate::New(env->GetIsolate());
  global_template->Set(
      v8::String::NewFromUtf8(env->GetIsolate(), "\x54\x68\x72\x65\x61\x64\x65\x64\x41\x74\x42\x61\x72\x72\x69\x65\x72\x31"),
      v8::FunctionTemplate::New(isolate, ThreadedAtBarrier1));
  v8::Handle<v8::Context> context = v8::Context::New(isolate,
                                                     NULL,
                                                     global_template);
  v8::Context::Scope context_scope(context);

  CompileRun(source);
}


void DebuggerThread::Run() {
  const int kBufSize = 1000;
  uint16_t buffer[kBufSize];

  const char* command_1 = "\x7b\x22\x73\x65\x71\x22\x3a\x31\x30\x32\x2c"
      "\x22\x74\x79\x70\x65\x22\x3a\x22\x72\x65\x71\x75\x65\x73\x74\x22\x2c"
      "\x22\x63\x6f\x6d\x6d\x61\x6e\x64\x22\x3a\x22\x65\x76\x61\x6c\x75\x61\x74\x65\x22\x2c"
      "\x22\x61\x72\x67\x75\x6d\x65\x6e\x74\x73\x22\x3a\x7b\x22\x65\x78\x70\x72\x65\x73\x73\x69\x6f\x6e\x22\x3a\x22\x62\x61\x72\x28\x66\x61\x6c\x73\x65\x29\x22\x7d\x7d";
  const char* command_2 = "\x7b\x22\x73\x65\x71\x22\x3a\x31\x30\x33\x2c"
      "\x22\x74\x79\x70\x65\x22\x3a\x22\x72\x65\x71\x75\x65\x73\x74\x22\x2c"
      "\x22\x63\x6f\x6d\x6d\x61\x6e\x64\x22\x3a\x22\x63\x6f\x6e\x74\x69\x6e\x75\x65\x22\x7d";

  v8::Isolate* isolate = CcTest::isolate();
  threaded_debugging_barriers.barrier_1.Wait();
  v8::Debug::DebugBreak(isolate);
  threaded_debugging_barriers.barrier_2.Wait();
  v8::Debug::SendCommand(isolate, buffer, AsciiToUtf16(command_1, buffer));
  v8::Debug::SendCommand(isolate, buffer, AsciiToUtf16(command_2, buffer));
}


TEST(ThreadedDebugging) {
  DebuggerThread debugger_thread;
  V8Thread v8_thread;

  // Create a V8 environment
  v8_thread.Start();
  debugger_thread.Start();

  v8_thread.Join();
  debugger_thread.Join();
}


/* Test RecursiveBreakpoints */
/* In this test, the debugger evaluates a function with a breakpoint, after
 * hitting a breakpoint in another function.  We do this with both values
 * of the flag enabling recursive breakpoints, and verify that the second
 * breakpoint is hit when enabled, and missed when disabled.
 */

class BreakpointsV8Thread : public v8::base::Thread {
 public:
  BreakpointsV8Thread() : Thread(Options("\x42\x72\x65\x61\x6b\x70\x6f\x69\x6e\x74\x73\x56\x38\x54\x68\x72\x65\x61\x64")) {}
  void Run();
};

class BreakpointsDebuggerThread : public v8::base::Thread {
 public:
  explicit BreakpointsDebuggerThread(bool global_evaluate)
      : Thread(Options("\x42\x72\x65\x61\x6b\x70\x6f\x69\x6e\x74\x73\x44\x65\x62\x75\x67\x67\x65\x72\x54\x68\x72\x65\x61\x64")),
        global_evaluate_(global_evaluate) {}
  void Run();

 private:
  bool global_evaluate_;
};


Barriers* breakpoints_barriers;
int break_event_breakpoint_id;
int evaluate_int_result;

static void BreakpointsMessageHandler(const v8::Debug::Message& message) {
  static char print_buffer[1000];
  v8::String::Value json(message.GetJSON());
  Utf16ToAscii(*json, json.length(), print_buffer);

  if (IsBreakEventMessage(print_buffer)) {
    break_event_breakpoint_id =
        GetBreakpointIdFromBreakEventMessage(print_buffer);
    breakpoints_barriers->semaphore_1.Signal();
  } else if (IsEvaluateResponseMessage(print_buffer)) {
    evaluate_int_result = GetEvaluateIntResult(print_buffer);
    breakpoints_barriers->semaphore_1.Signal();
  }
}


void BreakpointsV8Thread::Run() {
  const char* source_1 = "\x76\x61\x72\x20\x79\x5f\x67\x6c\x6f\x62\x61\x6c\x20\x3d\x20\x33\x3b\xa"
    "\x66\x75\x6e\x63\x74\x69\x6f\x6e\x20\x63\x61\x74\x28\x20\x6e\x65\x77\x5f\x76\x61\x6c\x75\x65\x20\x29\x20\x7b\xa"
    "\x20\x20\x76\x61\x72\x20\x78\x20\x3d\x20\x6e\x65\x77\x5f\x76\x61\x6c\x75\x65\x3b\xa"
    "\x20\x20\x79\x5f\x67\x6c\x6f\x62\x61\x6c\x20\x3d\x20\x79\x5f\x67\x6c\x6f\x62\x61\x6c\x20\x2b\x20\x34\x3b\xa"
    "\x20\x20\x78\x20\x3d\x20\x33\x20\x2a\x20\x78\x20\x2b\x20\x31\x3b\xa"
    "\x20\x20\x79\x5f\x67\x6c\x6f\x62\x61\x6c\x20\x3d\x20\x79\x5f\x67\x6c\x6f\x62\x61\x6c\x20\x2b\x20\x35\x3b\xa"
    "\x20\x20\x72\x65\x74\x75\x72\x6e\x20\x78\x3b\xa"
    "\x7d\xa"
    "\xa"
    "\x66\x75\x6e\x63\x74\x69\x6f\x6e\x20\x64\x6f\x67\x28\x29\x20\x7b\xa"
    "\x20\x20\x76\x61\x72\x20\x78\x20\x3d\x20\x31\x3b\xa"
    "\x20\x20\x78\x20\x3d\x20\x79\x5f\x67\x6c\x6f\x62\x61\x6c\x3b"
    "\x20\x20\x76\x61\x72\x20\x7a\x20\x3d\x20\x33\x3b"
    "\x20\x20\x78\x20\x2b\x3d\x20\x31\x30\x30\x3b\xa"
    "\x20\x20\x72\x65\x74\x75\x72\x6e\x20\x78\x3b\xa"
    "\x7d\xa"
    "\xa";
  const char* source_2 = "\x63\x61\x74\x28\x31\x37\x29\x3b\xa"
    "\x63\x61\x74\x28\x31\x39\x29\x3b\xa";

  v8::Isolate* isolate = CcTest::isolate();
  v8::Isolate::Scope isolate_scope(isolate);
  DebugLocalContext env;
  v8::HandleScope scope(isolate);
  v8::Debug::SetMessageHandler(&BreakpointsMessageHandler);

  CompileRun(source_1);
  breakpoints_barriers->barrier_1.Wait();
  breakpoints_barriers->barrier_2.Wait();
  CompileRun(source_2);
}


void BreakpointsDebuggerThread::Run() {
  const int kBufSize = 1000;
  uint16_t buffer[kBufSize];

  const char* command_1 = "\x7b\x22\x73\x65\x71\x22\x3a\x31\x30\x31\x2c"
      "\x22\x74\x79\x70\x65\x22\x3a\x22\x72\x65\x71\x75\x65\x73\x74\x22\x2c"
      "\x22\x63\x6f\x6d\x6d\x61\x6e\x64\x22\x3a\x22\x73\x65\x74\x62\x72\x65\x61\x6b\x70\x6f\x69\x6e\x74\x22\x2c"
      "\x22\x61\x72\x67\x75\x6d\x65\x6e\x74\x73\x22\x3a\x7b\x22\x74\x79\x70\x65\x22\x3a\x22\x66\x75\x6e\x63\x74\x69\x6f\x6e\x22\x2c\x22\x74\x61\x72\x67\x65\x74\x22\x3a\x22\x63\x61\x74\x22\x2c\x22\x6c\x69\x6e\x65\x22\x3a\x33\x7d\x7d";
  const char* command_2 = "\x7b\x22\x73\x65\x71\x22\x3a\x31\x30\x32\x2c"
      "\x22\x74\x79\x70\x65\x22\x3a\x22\x72\x65\x71\x75\x65\x73\x74\x22\x2c"
      "\x22\x63\x6f\x6d\x6d\x61\x6e\x64\x22\x3a\x22\x73\x65\x74\x62\x72\x65\x61\x6b\x70\x6f\x69\x6e\x74\x22\x2c"
      "\x22\x61\x72\x67\x75\x6d\x65\x6e\x74\x73\x22\x3a\x7b\x22\x74\x79\x70\x65\x22\x3a\x22\x66\x75\x6e\x63\x74\x69\x6f\x6e\x22\x2c\x22\x74\x61\x72\x67\x65\x74\x22\x3a\x22\x64\x6f\x67\x22\x2c\x22\x6c\x69\x6e\x65\x22\x3a\x33\x7d\x7d";
  const char* command_3;
  if (this->global_evaluate_) {
    command_3 = "\x7b\x22\x73\x65\x71\x22\x3a\x31\x30\x33\x2c"
        "\x22\x74\x79\x70\x65\x22\x3a\x22\x72\x65\x71\x75\x65\x73\x74\x22\x2c"
        "\x22\x63\x6f\x6d\x6d\x61\x6e\x64\x22\x3a\x22\x65\x76\x61\x6c\x75\x61\x74\x65\x22\x2c"
        "\x22\x61\x72\x67\x75\x6d\x65\x6e\x74\x73\x22\x3a\x7b\x22\x65\x78\x70\x72\x65\x73\x73\x69\x6f\x6e\x22\x3a\x22\x64\x6f\x67\x28\x29\x22\x2c\x22\x64\x69\x73\x61\x62\x6c\x65\x5f\x62\x72\x65\x61\x6b\x22\x3a\x66\x61\x6c\x73\x65\x2c"
        "\x22\x67\x6c\x6f\x62\x61\x6c\x22\x3a\x74\x72\x75\x65\x7d\x7d";
  } else {
    command_3 = "\x7b\x22\x73\x65\x71\x22\x3a\x31\x30\x33\x2c"
        "\x22\x74\x79\x70\x65\x22\x3a\x22\x72\x65\x71\x75\x65\x73\x74\x22\x2c"
        "\x22\x63\x6f\x6d\x6d\x61\x6e\x64\x22\x3a\x22\x65\x76\x61\x6c\x75\x61\x74\x65\x22\x2c"
        "\x22\x61\x72\x67\x75\x6d\x65\x6e\x74\x73\x22\x3a\x7b\x22\x65\x78\x70\x72\x65\x73\x73\x69\x6f\x6e\x22\x3a\x22\x64\x6f\x67\x28\x29\x22\x2c\x22\x64\x69\x73\x61\x62\x6c\x65\x5f\x62\x72\x65\x61\x6b\x22\x3a\x66\x61\x6c\x73\x65\x7d\x7d";
  }
  const char* command_4;
  if (this->global_evaluate_) {
    command_4 = "\x7b\x22\x73\x65\x71\x22\x3a\x31\x30\x34\x2c"
        "\x22\x74\x79\x70\x65\x22\x3a\x22\x72\x65\x71\x75\x65\x73\x74\x22\x2c"
        "\x22\x63\x6f\x6d\x6d\x61\x6e\x64\x22\x3a\x22\x65\x76\x61\x6c\x75\x61\x74\x65\x22\x2c"
        "\x22\x61\x72\x67\x75\x6d\x65\x6e\x74\x73\x22\x3a\x7b\x22\x65\x78\x70\x72\x65\x73\x73\x69\x6f\x6e\x22\x3a\x22\x31\x30\x30\x20\x2b\x20\x38\x22\x2c\x22\x64\x69\x73\x61\x62\x6c\x65\x5f\x62\x72\x65\x61\x6b\x22\x3a\x74\x72\x75\x65\x2c"
        "\x22\x67\x6c\x6f\x62\x61\x6c\x22\x3a\x74\x72\x75\x65\x7d\x7d";
  } else {
    command_4 = "\x7b\x22\x73\x65\x71\x22\x3a\x31\x30\x34\x2c"
        "\x22\x74\x79\x70\x65\x22\x3a\x22\x72\x65\x71\x75\x65\x73\x74\x22\x2c"
        "\x22\x63\x6f\x6d\x6d\x61\x6e\x64\x22\x3a\x22\x65\x76\x61\x6c\x75\x61\x74\x65\x22\x2c"
        "\x22\x61\x72\x67\x75\x6d\x65\x6e\x74\x73\x22\x3a\x7b\x22\x65\x78\x70\x72\x65\x73\x73\x69\x6f\x6e\x22\x3a\x22\x78\x20\x2b\x20\x31\x22\x2c\x22\x64\x69\x73\x61\x62\x6c\x65\x5f\x62\x72\x65\x61\x6b\x22\x3a\x74\x72\x75\x65\x7d\x7d";
  }
  const char* command_5 = "\x7b\x22\x73\x65\x71\x22\x3a\x31\x30\x35\x2c"
      "\x22\x74\x79\x70\x65\x22\x3a\x22\x72\x65\x71\x75\x65\x73\x74\x22\x2c"
      "\x22\x63\x6f\x6d\x6d\x61\x6e\x64\x22\x3a\x22\x63\x6f\x6e\x74\x69\x6e\x75\x65\x22\x7d";
  const char* command_6 = "\x7b\x22\x73\x65\x71\x22\x3a\x31\x30\x36\x2c"
      "\x22\x74\x79\x70\x65\x22\x3a\x22\x72\x65\x71\x75\x65\x73\x74\x22\x2c"
      "\x22\x63\x6f\x6d\x6d\x61\x6e\x64\x22\x3a\x22\x63\x6f\x6e\x74\x69\x6e\x75\x65\x22\x7d";
  const char* command_7;
  if (this->global_evaluate_) {
    command_7 = "\x7b\x22\x73\x65\x71\x22\x3a\x31\x30\x37\x2c"
        "\x22\x74\x79\x70\x65\x22\x3a\x22\x72\x65\x71\x75\x65\x73\x74\x22\x2c"
        "\x22\x63\x6f\x6d\x6d\x61\x6e\x64\x22\x3a\x22\x65\x76\x61\x6c\x75\x61\x74\x65\x22\x2c"
        "\x22\x61\x72\x67\x75\x6d\x65\x6e\x74\x73\x22\x3a\x7b\x22\x65\x78\x70\x72\x65\x73\x73\x69\x6f\x6e\x22\x3a\x22\x64\x6f\x67\x28\x29\x22\x2c\x22\x64\x69\x73\x61\x62\x6c\x65\x5f\x62\x72\x65\x61\x6b\x22\x3a\x74\x72\x75\x65\x2c"
        "\x22\x67\x6c\x6f\x62\x61\x6c\x22\x3a\x74\x72\x75\x65\x7d\x7d";
  } else {
    command_7 = "\x7b\x22\x73\x65\x71\x22\x3a\x31\x30\x37\x2c"
        "\x22\x74\x79\x70\x65\x22\x3a\x22\x72\x65\x71\x75\x65\x73\x74\x22\x2c"
        "\x22\x63\x6f\x6d\x6d\x61\x6e\x64\x22\x3a\x22\x65\x76\x61\x6c\x75\x61\x74\x65\x22\x2c"
        "\x22\x61\x72\x67\x75\x6d\x65\x6e\x74\x73\x22\x3a\x7b\x22\x65\x78\x70\x72\x65\x73\x73\x69\x6f\x6e\x22\x3a\x22\x64\x6f\x67\x28\x29\x22\x2c\x22\x64\x69\x73\x61\x62\x6c\x65\x5f\x62\x72\x65\x61\x6b\x22\x3a\x74\x72\x75\x65\x7d\x7d";
  }
  const char* command_8 = "\x7b\x22\x73\x65\x71\x22\x3a\x31\x30\x38\x2c"
      "\x22\x74\x79\x70\x65\x22\x3a\x22\x72\x65\x71\x75\x65\x73\x74\x22\x2c"
      "\x22\x63\x6f\x6d\x6d\x61\x6e\x64\x22\x3a\x22\x63\x6f\x6e\x74\x69\x6e\x75\x65\x22\x7d";


  v8::Isolate* isolate = CcTest::isolate();
  v8::Isolate::Scope isolate_scope(isolate);
  // v8 thread initializes, runs source_1
  breakpoints_barriers->barrier_1.Wait();
  // 1:Set breakpoint in cat() (will get id 1).
  v8::Debug::SendCommand(isolate, buffer, AsciiToUtf16(command_1, buffer));
  // 2:Set breakpoint in dog() (will get id 2).
  v8::Debug::SendCommand(isolate, buffer, AsciiToUtf16(command_2, buffer));
  breakpoints_barriers->barrier_2.Wait();
  // V8 thread starts compiling source_2.
  // Automatic break happens, to run queued commands
  // breakpoints_barriers->semaphore_1.Wait();
  // Commands 1 through 3 run, thread continues.
  // v8 thread runs source_2 to breakpoint in cat().
  // message callback receives break event.
  breakpoints_barriers->semaphore_1.Wait();
  // Must have hit breakpoint #1.
  CHECK_EQ(1, break_event_breakpoint_id);
  // 4:Evaluate dog() (which has a breakpoint).
  v8::Debug::SendCommand(isolate, buffer, AsciiToUtf16(command_3, buffer));
  // V8 thread hits breakpoint in dog().
  breakpoints_barriers->semaphore_1.Wait();  // wait for break event
  // Must have hit breakpoint #2.
  CHECK_EQ(2, break_event_breakpoint_id);
  // 5:Evaluate (x + 1).
  v8::Debug::SendCommand(isolate, buffer, AsciiToUtf16(command_4, buffer));
  // Evaluate (x + 1) finishes.
  breakpoints_barriers->semaphore_1.Wait();
  // Must have result 108.
  CHECK_EQ(108, evaluate_int_result);
  // 6:Continue evaluation of dog().
  v8::Debug::SendCommand(isolate, buffer, AsciiToUtf16(command_5, buffer));
  // Evaluate dog() finishes.
  breakpoints_barriers->semaphore_1.Wait();
  // Must have result 107.
  CHECK_EQ(107, evaluate_int_result);
  // 7:Continue evaluation of source_2, finish cat(17), hit breakpoint
  // in cat(19).
  v8::Debug::SendCommand(isolate, buffer, AsciiToUtf16(command_6, buffer));
  // Message callback gets break event.
  breakpoints_barriers->semaphore_1.Wait();  // wait for break event
  // Must have hit breakpoint #1.
  CHECK_EQ(1, break_event_breakpoint_id);
  // 8: Evaluate dog() with breaks disabled.
  v8::Debug::SendCommand(isolate, buffer, AsciiToUtf16(command_7, buffer));
  // Evaluate dog() finishes.
  breakpoints_barriers->semaphore_1.Wait();
  // Must have result 116.
  CHECK_EQ(116, evaluate_int_result);
  // 9: Continue evaluation of source2, reach end.
  v8::Debug::SendCommand(isolate, buffer, AsciiToUtf16(command_8, buffer));
}


void TestRecursiveBreakpointsGeneric(bool global_evaluate) {
  BreakpointsDebuggerThread breakpoints_debugger_thread(global_evaluate);
  BreakpointsV8Thread breakpoints_v8_thread;

  // Create a V8 environment
  Barriers stack_allocated_breakpoints_barriers;
  breakpoints_barriers = &stack_allocated_breakpoints_barriers;

  breakpoints_v8_thread.Start();
  breakpoints_debugger_thread.Start();

  breakpoints_v8_thread.Join();
  breakpoints_debugger_thread.Join();
}


TEST(RecursiveBreakpoints) {
  TestRecursiveBreakpointsGeneric(false);
}


TEST(RecursiveBreakpointsGlobal) {
  TestRecursiveBreakpointsGeneric(true);
}


static void DummyDebugEventListener(
    const v8::Debug::EventDetails& event_details) {
}


TEST(SetDebugEventListenerOnUninitializedVM) {
  v8::Debug::SetDebugEventListener(DummyDebugEventListener);
}


static void DummyMessageHandler(const v8::Debug::Message& message) {
}


TEST(SetMessageHandlerOnUninitializedVM) {
  v8::Debug::SetMessageHandler(DummyMessageHandler);
}


// Source for a JavaScript function which returns the data parameter of a
// function called in the context of the debugger. If no data parameter is
// passed it throws an exception.
static const char* debugger_call_with_data_source =
    "\x66\x75\x6e\x63\x74\x69\x6f\x6e\x20\x64\x65\x62\x75\x67\x67\x65\x72\x5f\x63\x61\x6c\x6c\x5f\x77\x69\x74\x68\x5f\x64\x61\x74\x61\x28\x65\x78\x65\x63\x5f\x73\x74\x61\x74\x65\x2c\x20\x64\x61\x74\x61\x29\x20\x7b"
    "\x20\x20\x69\x66\x20\x28\x64\x61\x74\x61\x29\x20\x72\x65\x74\x75\x72\x6e\x20\x64\x61\x74\x61\x3b"
    "\x20\x20\x74\x68\x72\x6f\x77\x20\x27\x4e\x6f\x20\x64\x61\x74\x61\x21\x27"
    "\x7d";
v8::Handle<v8::Function> debugger_call_with_data;


// Source for a JavaScript function which returns the data parameter of a
// function called in the context of the debugger. If no data parameter is
// passed it throws an exception.
static const char* debugger_call_with_closure_source =
    "\x76\x61\x72\x20\x78\x20\x3d\x20\x33\x3b"
    "\x28\x66\x75\x6e\x63\x74\x69\x6f\x6e\x20\x28\x65\x78\x65\x63\x5f\x73\x74\x61\x74\x65\x29\x20\x7b"
    "\x20\x20\x69\x66\x20\x28\x65\x78\x65\x63\x5f\x73\x74\x61\x74\x65\x2e\x79\x29\x20\x72\x65\x74\x75\x72\x6e\x20\x78\x20\x2d\x20\x31\x3b"
    "\x20\x20\x65\x78\x65\x63\x5f\x73\x74\x61\x74\x65\x2e\x79\x20\x3d\x20\x78\x3b"
    "\x20\x20\x72\x65\x74\x75\x72\x6e\x20\x65\x78\x65\x63\x5f\x73\x74\x61\x74\x65\x2e\x79"
    "\x7d\x29";
v8::Handle<v8::Function> debugger_call_with_closure;

// Function to retrieve the number of JavaScript frames by calling a JavaScript
// in the debugger.
static void CheckFrameCount(const v8::FunctionCallbackInfo<v8::Value>& args) {
  CHECK(v8::Debug::Call(frame_count)->IsNumber());
  CHECK_EQ(args[0]->Int32Value(),
           v8::Debug::Call(frame_count)->Int32Value());
}


// Function to retrieve the source line of the top JavaScript frame by calling a
// JavaScript function in the debugger.
static void CheckSourceLine(const v8::FunctionCallbackInfo<v8::Value>& args) {
  CHECK(v8::Debug::Call(frame_source_line)->IsNumber());
  CHECK_EQ(args[0]->Int32Value(),
           v8::Debug::Call(frame_source_line)->Int32Value());
}


// Function to test passing an additional parameter to a JavaScript function
// called in the debugger. It also tests that functions called in the debugger
// can throw exceptions.
static void CheckDataParameter(
    const v8::FunctionCallbackInfo<v8::Value>& args) {
  v8::Handle<v8::String> data =
      v8::String::NewFromUtf8(args.GetIsolate(), "\x54\x65\x73\x74");
  CHECK(v8::Debug::Call(debugger_call_with_data, data)->IsString());

  for (int i = 0; i < 3; i++) {
    v8::TryCatch catcher;
    CHECK(v8::Debug::Call(debugger_call_with_data).IsEmpty());
    CHECK(catcher.HasCaught());
    CHECK(catcher.Exception()->IsString());
  }
}


// Function to test using a JavaScript with closure in the debugger.
static void CheckClosure(const v8::FunctionCallbackInfo<v8::Value>& args) {
  CHECK(v8::Debug::Call(debugger_call_with_closure)->IsNumber());
  CHECK_EQ(3, v8::Debug::Call(debugger_call_with_closure)->Int32Value());
}


// Test functions called through the debugger.
TEST(CallFunctionInDebugger) {
  // Create and enter a context with the functions CheckFrameCount,
  // CheckSourceLine and CheckDataParameter installed.
  v8::Isolate* isolate = CcTest::isolate();
  v8::HandleScope scope(isolate);
  v8::Handle<v8::ObjectTemplate> global_template =
      v8::ObjectTemplate::New(isolate);
  global_template->Set(
      v8::String::NewFromUtf8(isolate, "\x43\x68\x65\x63\x6b\x46\x72\x61\x6d\x65\x43\x6f\x75\x6e\x74"),
      v8::FunctionTemplate::New(isolate, CheckFrameCount));
  global_template->Set(
      v8::String::NewFromUtf8(isolate, "\x43\x68\x65\x63\x6b\x53\x6f\x75\x72\x63\x65\x4c\x69\x6e\x65"),
      v8::FunctionTemplate::New(isolate, CheckSourceLine));
  global_template->Set(
      v8::String::NewFromUtf8(isolate, "\x43\x68\x65\x63\x6b\x44\x61\x74\x61\x50\x61\x72\x61\x6d\x65\x74\x65\x72"),
      v8::FunctionTemplate::New(isolate, CheckDataParameter));
  global_template->Set(
      v8::String::NewFromUtf8(isolate, "\x43\x68\x65\x63\x6b\x43\x6c\x6f\x73\x75\x72\x65"),
      v8::FunctionTemplate::New(isolate, CheckClosure));
  v8::Handle<v8::Context> context = v8::Context::New(isolate,
                                                     NULL,
                                                     global_template);
  v8::Context::Scope context_scope(context);

  // Compile a function for checking the number of JavaScript frames.
  v8::Script::Compile(
      v8::String::NewFromUtf8(isolate, frame_count_source))->Run();
  frame_count = v8::Local<v8::Function>::Cast(context->Global()->Get(
      v8::String::NewFromUtf8(isolate, "\x66\x72\x61\x6d\x65\x5f\x63\x6f\x75\x6e\x74")));

  // Compile a function for returning the source line for the top frame.
  v8::Script::Compile(v8::String::NewFromUtf8(isolate,
                                              frame_source_line_source))->Run();
  frame_source_line = v8::Local<v8::Function>::Cast(context->Global()->Get(
      v8::String::NewFromUtf8(isolate, "\x66\x72\x61\x6d\x65\x5f\x73\x6f\x75\x72\x63\x65\x5f\x6c\x69\x6e\x65")));

  // Compile a function returning the data parameter.
  v8::Script::Compile(v8::String::NewFromUtf8(isolate,
                                              debugger_call_with_data_source))
      ->Run();
  debugger_call_with_data = v8::Local<v8::Function>::Cast(
      context->Global()->Get(v8::String::NewFromUtf8(
          isolate, "\x64\x65\x62\x75\x67\x67\x65\x72\x5f\x63\x61\x6c\x6c\x5f\x77\x69\x74\x68\x5f\x64\x61\x74\x61")));

  // Compile a function capturing closure.
  debugger_call_with_closure =
      v8::Local<v8::Function>::Cast(v8::Script::Compile(
          v8::String::NewFromUtf8(isolate,
                                  debugger_call_with_closure_source))->Run());

  // Calling a function through the debugger returns 0 frames if there are
  // no JavaScript frames.
  CHECK_EQ(v8::Integer::New(isolate, 0),
           v8::Debug::Call(frame_count));

  // Test that the number of frames can be retrieved.
  v8::Script::Compile(
      v8::String::NewFromUtf8(isolate, "\x43\x68\x65\x63\x6b\x46\x72\x61\x6d\x65\x43\x6f\x75\x6e\x74\x28\x31\x29"))->Run();
  v8::Script::Compile(v8::String::NewFromUtf8(isolate,
                                              "\x66\x75\x6e\x63\x74\x69\x6f\x6e\x20\x66\x28\x29\x20\x7b"
                                              "\x20\x20\x43\x68\x65\x63\x6b\x46\x72\x61\x6d\x65\x43\x6f\x75\x6e\x74\x28\x32\x29\x3b"
                                              "\x7d\x3b\x20\x66\x28\x29"))->Run();

  // Test that the source line can be retrieved.
  v8::Script::Compile(
      v8::String::NewFromUtf8(isolate, "\x43\x68\x65\x63\x6b\x53\x6f\x75\x72\x63\x65\x4c\x69\x6e\x65\x28\x30\x29"))->Run();
  v8::Script::Compile(v8::String::NewFromUtf8(isolate,
                                              "\x66\x75\x6e\x63\x74\x69\x6f\x6e\x20\x66\x28\x29\x20\x7b\xa"
                                              "\x20\x20\x43\x68\x65\x63\x6b\x53\x6f\x75\x72\x63\x65\x4c\x69\x6e\x65\x28\x31\x29\xa"
                                              "\x20\x20\x43\x68\x65\x63\x6b\x53\x6f\x75\x72\x63\x65\x4c\x69\x6e\x65\x28\x32\x29\xa"
                                              "\x20\x20\x43\x68\x65\x63\x6b\x53\x6f\x75\x72\x63\x65\x4c\x69\x6e\x65\x28\x33\x29\xa"
                                              "\x7d\x3b\x20\x66\x28\x29"))->Run();

  // Test that a parameter can be passed to a function called in the debugger.
  v8::Script::Compile(v8::String::NewFromUtf8(isolate,
                                              "\x43\x68\x65\x63\x6b\x44\x61\x74\x61\x50\x61\x72\x61\x6d\x65\x74\x65\x72\x28\x29"))->Run();

  // Test that a function with closure can be run in the debugger.
  v8::Script::Compile(
      v8::String::NewFromUtf8(isolate, "\x43\x68\x65\x63\x6b\x43\x6c\x6f\x73\x75\x72\x65\x28\x29"))->Run();

  // Test that the source line is correct when there is a line offset.
  v8::ScriptOrigin origin(v8::String::NewFromUtf8(isolate, "\x74\x65\x73\x74"),
                          v8::Integer::New(isolate, 7));
  v8::Script::Compile(
      v8::String::NewFromUtf8(isolate, "\x43\x68\x65\x63\x6b\x53\x6f\x75\x72\x63\x65\x4c\x69\x6e\x65\x28\x37\x29"), &origin)
      ->Run();
  v8::Script::Compile(v8::String::NewFromUtf8(isolate,
                                              "\x66\x75\x6e\x63\x74\x69\x6f\x6e\x20\x66\x28\x29\x20\x7b\xa"
                                              "\x20\x20\x43\x68\x65\x63\x6b\x53\x6f\x75\x72\x63\x65\x4c\x69\x6e\x65\x28\x38\x29\xa"
                                              "\x20\x20\x43\x68\x65\x63\x6b\x53\x6f\x75\x72\x63\x65\x4c\x69\x6e\x65\x28\x39\x29\xa"
                                              "\x20\x20\x43\x68\x65\x63\x6b\x53\x6f\x75\x72\x63\x65\x4c\x69\x6e\x65\x28\x31\x30\x29\xa"
                                              "\x7d\x3b\x20\x66\x28\x29"),
                      &origin)->Run();
}


// Debugger message handler which counts the number of breaks.
static void SendContinueCommand();
static void MessageHandlerBreakPointHitCount(
    const v8::Debug::Message& message) {
  if (message.IsEvent() && message.GetEvent() == v8::Break) {
    // Count the number of breaks.
    break_point_hit_count++;

    SendContinueCommand();
  }
}


// Test that clearing the debug event listener actually clears all break points
// and related information.
TEST(DebuggerUnload) {
  DebugLocalContext env;

  // Check debugger is unloaded before it is used.
  CheckDebuggerUnloaded();

  // Set a debug event listener.
  break_point_hit_count = 0;
  v8::Debug::SetDebugEventListener(DebugEventBreakPointHitCount);
  {
    v8::HandleScope scope(env->GetIsolate());
    // Create a couple of functions for the test.
    v8::Local<v8::Function> foo =
        CompileFunction(&env, "\x66\x75\x6e\x63\x74\x69\x6f\x6e\x20\x66\x6f\x6f\x28\x29\x7b\x78\x3d\x31\x7d", "\x66\x6f\x6f");
    v8::Local<v8::Function> bar =
        CompileFunction(&env, "\x66\x75\x6e\x63\x74\x69\x6f\x6e\x20\x62\x61\x72\x28\x29\x7b\x79\x3d\x32\x7d", "\x62\x61\x72");

    // Set some break points.
    SetBreakPoint(foo, 0);
    SetBreakPoint(foo, 4);
    SetBreakPoint(bar, 0);
    SetBreakPoint(bar, 4);

    // Make sure that the break points are there.
    break_point_hit_count = 0;
    foo->Call(env->Global(), 0, NULL);
    CHECK_EQ(2, break_point_hit_count);
    bar->Call(env->Global(), 0, NULL);
    CHECK_EQ(4, break_point_hit_count);
  }

  // Remove the debug event listener without clearing breakpoints. Do this
  // outside a handle scope.
  v8::Debug::SetDebugEventListener(NULL);
  CheckDebuggerUnloaded(true);

  // Now set a debug message handler.
  break_point_hit_count = 0;
  v8::Debug::SetMessageHandler(MessageHandlerBreakPointHitCount);
  {
    v8::HandleScope scope(env->GetIsolate());

    // Get the test functions again.
    v8::Local<v8::Function> foo(v8::Local<v8::Function>::Cast(
        env->Global()->Get(v8::String::NewFromUtf8(env->GetIsolate(), "\x66\x6f\x6f"))));

    foo->Call(env->Global(), 0, NULL);
    CHECK_EQ(0, break_point_hit_count);

    // Set break points and run again.
    SetBreakPoint(foo, 0);
    SetBreakPoint(foo, 4);
    foo->Call(env->Global(), 0, NULL);
    CHECK_EQ(2, break_point_hit_count);
  }

  // Remove the debug message handler without clearing breakpoints. Do this
  // outside a handle scope.
  v8::Debug::SetMessageHandler(NULL);
  CheckDebuggerUnloaded(true);
}


// Sends continue command to the debugger.
static void SendContinueCommand() {
  const int kBufferSize = 1000;
  uint16_t buffer[kBufferSize];
  const char* command_continue =
    "\x7b\x22\x73\x65\x71\x22\x3a\x30\x2c"
     "\x22\x74\x79\x70\x65\x22\x3a\x22\x72\x65\x71\x75\x65\x73\x74\x22\x2c"
     "\x22\x63\x6f\x6d\x6d\x61\x6e\x64\x22\x3a\x22\x63\x6f\x6e\x74\x69\x6e\x75\x65\x22\x7d";

  v8::Debug::SendCommand(
      CcTest::isolate(), buffer, AsciiToUtf16(command_continue, buffer));
}


// Debugger message handler which counts the number of times it is called.
static int message_handler_hit_count = 0;
static void MessageHandlerHitCount(const v8::Debug::Message& message) {
  message_handler_hit_count++;

  static char print_buffer[1000];
  v8::String::Value json(message.GetJSON());
  Utf16ToAscii(*json, json.length(), print_buffer);
  if (IsExceptionEventMessage(print_buffer)) {
    // Send a continue command for exception events.
    SendContinueCommand();
  }
}


// Test clearing the debug message handler.
TEST(DebuggerClearMessageHandler) {
  DebugLocalContext env;
  v8::HandleScope scope(env->GetIsolate());

  // Check debugger is unloaded before it is used.
  CheckDebuggerUnloaded();

  // Set a debug message handler.
  v8::Debug::SetMessageHandler(MessageHandlerHitCount);

  // Run code to throw a unhandled exception. This should end up in the message
  // handler.
  CompileRun("\x74\x68\x72\x6f\x77\x20\x31");

  // The message handler should be called.
  CHECK_GT(message_handler_hit_count, 0);

  // Clear debug message handler.
  message_handler_hit_count = 0;
  v8::Debug::SetMessageHandler(NULL);

  // Run code to throw a unhandled exception. This should end up in the message
  // handler.
  CompileRun("\x74\x68\x72\x6f\x77\x20\x31");

  // The message handler should not be called more.
  CHECK_EQ(0, message_handler_hit_count);

  CheckDebuggerUnloaded(true);
}


// Debugger message handler which clears the message handler while active.
static void MessageHandlerClearingMessageHandler(
    const v8::Debug::Message& message) {
  message_handler_hit_count++;

  // Clear debug message handler.
  v8::Debug::SetMessageHandler(NULL);
}


// Test clearing the debug message handler while processing a debug event.
TEST(DebuggerClearMessageHandlerWhileActive) {
  DebugLocalContext env;
  v8::HandleScope scope(env->GetIsolate());

  // Check debugger is unloaded before it is used.
  CheckDebuggerUnloaded();

  // Set a debug message handler.
  v8::Debug::SetMessageHandler(MessageHandlerClearingMessageHandler);

  // Run code to throw a unhandled exception. This should end up in the message
  // handler.
  CompileRun("\x74\x68\x72\x6f\x77\x20\x31");

  // The message handler should be called.
  CHECK_EQ(1, message_handler_hit_count);

  CheckDebuggerUnloaded(true);
}


// Test for issue http://code.google.com/p/v8/issues/detail?id=289.
// Make sure that DebugGetLoadedScripts doesn't return scripts
// with disposed external source.
class EmptyExternalStringResource : public v8::String::ExternalStringResource {
 public:
  EmptyExternalStringResource() { empty_[0] = 0; }
  virtual ~EmptyExternalStringResource() {}
  virtual size_t length() const { return empty_.length(); }
  virtual const uint16_t* data() const { return empty_.start(); }
 private:
  ::v8::internal::EmbeddedVector<uint16_t, 1> empty_;
};


TEST(DebugGetLoadedScripts) {
  DebugLocalContext env;
  v8::HandleScope scope(env->GetIsolate());
  env.ExposeDebug();

  EmptyExternalStringResource source_ext_str;
  v8::Local<v8::String> source =
      v8::String::NewExternal(env->GetIsolate(), &source_ext_str);
  v8::Handle<v8::Script> evil_script(v8::Script::Compile(source));
  // "use" evil_script to make the compiler happy.
  (void) evil_script;
  Handle<i::ExternalTwoByteString> i_source(
      i::ExternalTwoByteString::cast(*v8::Utils::OpenHandle(*source)));
  // This situation can happen if source was an external string disposed
  // by its owner.
  i_source->set_resource(0);

  bool allow_natives_syntax = i::FLAG_allow_natives_syntax;
  i::FLAG_allow_natives_syntax = true;
  CompileRun(
      "\x76\x61\x72\x20\x73\x63\x72\x69\x70\x74\x73\x20\x3d\x20\x25\x44\x65\x62\x75\x67\x47\x65\x74\x4c\x6f\x61\x64\x65\x64\x53\x63\x72\x69\x70\x74\x73\x28\x29\x3b"
      "\x76\x61\x72\x20\x63\x6f\x75\x6e\x74\x20\x3d\x20\x73\x63\x72\x69\x70\x74\x73\x2e\x6c\x65\x6e\x67\x74\x68\x3b"
      "\x66\x6f\x72\x20\x28\x76\x61\x72\x20\x69\x20\x3d\x20\x30\x3b\x20\x69\x20\x3c\x20\x63\x6f\x75\x6e\x74\x3b\x20\x2b\x2b\x69\x29\x20\x7b"
      "\x20\x20\x73\x63\x72\x69\x70\x74\x73\x5b\x69\x5d\x2e\x6c\x69\x6e\x65\x5f\x65\x6e\x64\x73\x3b"
      "\x7d");
  // Must not crash while accessing line_ends.
  i::FLAG_allow_natives_syntax = allow_natives_syntax;

  // Some scripts are retrieved - at least the number of native scripts.
  CHECK_GT((*env)
               ->Global()
               ->Get(v8::String::NewFromUtf8(env->GetIsolate(), "\x63\x6f\x75\x6e\x74"))
               ->Int32Value(),
           8);
}


// Test script break points set on lines.
TEST(ScriptNameAndData) {
  DebugLocalContext env;
  v8::HandleScope scope(env->GetIsolate());
  env.ExposeDebug();

  // Create functions for retrieving script name and data for the function on
  // the top frame when hitting a break point.
  frame_script_name = CompileFunction(&env,
                                      frame_script_name_source,
                                      "\x66\x72\x61\x6d\x65\x5f\x73\x63\x72\x69\x70\x74\x5f\x6e\x61\x6d\x65");

  v8::Debug::SetDebugEventListener(DebugEventBreakPointHitCount);

  // Test function source.
  v8::Local<v8::String> script = v8::String::NewFromUtf8(env->GetIsolate(),
                                                         "\x66\x75\x6e\x63\x74\x69\x6f\x6e\x20\x66\x28\x29\x20\x7b\xa"
                                                         "\x20\x20\x64\x65\x62\x75\x67\x67\x65\x72\x3b\xa"
                                                         "\x7d\xa");

  v8::ScriptOrigin origin1 =
      v8::ScriptOrigin(v8::String::NewFromUtf8(env->GetIsolate(), "\x6e\x61\x6d\x65"));
  v8::Handle<v8::Script> script1 = v8::Script::Compile(script, &origin1);
  script1->Run();
  v8::Local<v8::Function> f;
  f = v8::Local<v8::Function>::Cast(
      env->Global()->Get(v8::String::NewFromUtf8(env->GetIsolate(), "\x66")));

  f->Call(env->Global(), 0, NULL);
  CHECK_EQ(1, break_point_hit_count);
  CHECK_EQ("\x6e\x61\x6d\x65", last_script_name_hit);

  // Compile the same script again without setting data. As the compilation
  // cache is disabled when debugging expect the data to be missing.
  v8::Script::Compile(script, &origin1)->Run();
  f = v8::Local<v8::Function>::Cast(
      env->Global()->Get(v8::String::NewFromUtf8(env->GetIsolate(), "\x66")));
  f->Call(env->Global(), 0, NULL);
  CHECK_EQ(2, break_point_hit_count);
  CHECK_EQ("\x6e\x61\x6d\x65", last_script_name_hit);

  v8::Local<v8::String> data_obj_source = v8::String::NewFromUtf8(
      env->GetIsolate(),
      "\x28\x7b\x20\x61\x3a\x20\x27\x61\x62\x63\x27\x2c\xa"
      "\x20\x20\x62\x3a\x20\x31\x32\x33\x2c\xa"
      "\x20\x20\x74\x6f\x53\x74\x72\x69\x6e\x67\x3a\x20\x66\x75\x6e\x63\x74\x69\x6f\x6e\x28\x29\x20\x7b\x20\x72\x65\x74\x75\x72\x6e\x20\x74\x68\x69\x73\x2e\x61\x20\x2b\x20\x27\x20\x27\x20\x2b\x20\x74\x68\x69\x73\x2e\x62\x3b\x20\x7d\xa"
      "\x7d\x29\xa");
  v8::Script::Compile(data_obj_source)->Run();
  v8::ScriptOrigin origin2 =
      v8::ScriptOrigin(v8::String::NewFromUtf8(env->GetIsolate(), "\x6e\x65\x77\x20\x6e\x61\x6d\x65"));
  v8::Handle<v8::Script> script2 = v8::Script::Compile(script, &origin2);
  script2->Run();
  f = v8::Local<v8::Function>::Cast(
      env->Global()->Get(v8::String::NewFromUtf8(env->GetIsolate(), "\x66")));
  f->Call(env->Global(), 0, NULL);
  CHECK_EQ(3, break_point_hit_count);
  CHECK_EQ("\x6e\x65\x77\x20\x6e\x61\x6d\x65", last_script_name_hit);

  v8::Handle<v8::Script> script3 = v8::Script::Compile(script, &origin2);
  script3->Run();
  f = v8::Local<v8::Function>::Cast(
      env->Global()->Get(v8::String::NewFromUtf8(env->GetIsolate(), "\x66")));
  f->Call(env->Global(), 0, NULL);
  CHECK_EQ(4, break_point_hit_count);
}


static v8::Handle<v8::Context> expected_context;
static v8::Handle<v8::Value> expected_context_data;


// Check that the expected context is the one generating the debug event.
static void ContextCheckMessageHandler(const v8::Debug::Message& message) {
  CHECK(message.GetEventContext() == expected_context);
  CHECK(message.GetEventContext()->GetEmbedderData(0)->StrictEquals(
      expected_context_data));
  message_handler_hit_count++;

  static char print_buffer[1000];
  v8::String::Value json(message.GetJSON());
  Utf16ToAscii(*json, json.length(), print_buffer);

  // Send a continue command for break events.
  if (IsBreakEventMessage(print_buffer)) {
    SendContinueCommand();
  }
}


// Test which creates two contexts and sets different embedder data on each.
// Checks that this data is set correctly and that when the debug message
// handler is called the expected context is the one active.
TEST(ContextData) {
  v8::Isolate* isolate = CcTest::isolate();
  v8::HandleScope scope(isolate);

  // Create two contexts.
  v8::Handle<v8::Context> context_1;
  v8::Handle<v8::Context> context_2;
  v8::Handle<v8::ObjectTemplate> global_template =
      v8::Handle<v8::ObjectTemplate>();
  v8::Handle<v8::Value> global_object = v8::Handle<v8::Value>();
  context_1 = v8::Context::New(isolate, NULL, global_template, global_object);
  context_2 = v8::Context::New(isolate, NULL, global_template, global_object);

  v8::Debug::SetMessageHandler(ContextCheckMessageHandler);

  // Default data value is undefined.
  CHECK(context_1->GetEmbedderData(0)->IsUndefined());
  CHECK(context_2->GetEmbedderData(0)->IsUndefined());

  // Set and check different data values.
  v8::Handle<v8::String> data_1 = v8::String::NewFromUtf8(isolate, "\x31");
  v8::Handle<v8::String> data_2 = v8::String::NewFromUtf8(isolate, "\x32");
  context_1->SetEmbedderData(0, data_1);
  context_2->SetEmbedderData(0, data_2);
  CHECK(context_1->GetEmbedderData(0)->StrictEquals(data_1));
  CHECK(context_2->GetEmbedderData(0)->StrictEquals(data_2));

  // Simple test function which causes a break.
  const char* source = "\x66\x75\x6e\x63\x74\x69\x6f\x6e\x20\x66\x28\x29\x20\x7b\x20\x64\x65\x62\x75\x67\x67\x65\x72\x3b\x20\x7d";

  // Enter and run function in the first context.
  {
    v8::Context::Scope context_scope(context_1);
    expected_context = context_1;
    expected_context_data = data_1;
    v8::Local<v8::Function> f = CompileFunction(isolate, source, "\x66");
    f->Call(context_1->Global(), 0, NULL);
  }


  // Enter and run function in the second context.
  {
    v8::Context::Scope context_scope(context_2);
    expected_context = context_2;
    expected_context_data = data_2;
    v8::Local<v8::Function> f = CompileFunction(isolate, source, "\x66");
    f->Call(context_2->Global(), 0, NULL);
  }

  // Two times compile event and two times break event.
  CHECK_GT(message_handler_hit_count, 4);

  v8::Debug::SetMessageHandler(NULL);
  CheckDebuggerUnloaded();
}


// Debug message handler which issues a debug break when it hits a break event.
static int message_handler_break_hit_count = 0;
static void DebugBreakMessageHandler(const v8::Debug::Message& message) {
  // Schedule a debug break for break events.
  if (message.IsEvent() && message.GetEvent() == v8::Break) {
    message_handler_break_hit_count++;
    if (message_handler_break_hit_count == 1) {
      v8::Debug::DebugBreak(message.GetIsolate());
    }
  }

  // Issue a continue command if this event will not cause the VM to start
  // running.
  if (!message.WillStartRunning()) {
    SendContinueCommand();
  }
}


// Test that a debug break can be scheduled while in a message handler.
TEST(DebugBreakInMessageHandler) {
  DebugLocalContext env;
  v8::HandleScope scope(env->GetIsolate());

  v8::Debug::SetMessageHandler(DebugBreakMessageHandler);

  // Test functions.
  const char* script = "\x66\x75\x6e\x63\x74\x69\x6f\x6e\x20\x66\x28\x29\x20\x7b\x20\x64\x65\x62\x75\x67\x67\x65\x72\x3b\x20\x67\x28\x29\x3b\x20\x7d\x20\x66\x75\x6e\x63\x74\x69\x6f\x6e\x20\x67\x28\x29\x20\x7b\x20\x7d";
  CompileRun(script);
  v8::Local<v8::Function> f = v8::Local<v8::Function>::Cast(
      env->Global()->Get(v8::String::NewFromUtf8(env->GetIsolate(), "\x66")));
  v8::Local<v8::Function> g = v8::Local<v8::Function>::Cast(
      env->Global()->Get(v8::String::NewFromUtf8(env->GetIsolate(), "\x67")));

  // Call f then g. The debugger statement in f will casue a break which will
  // cause another break.
  f->Call(env->Global(), 0, NULL);
  CHECK_EQ(2, message_handler_break_hit_count);
  // Calling g will not cause any additional breaks.
  g->Call(env->Global(), 0, NULL);
  CHECK_EQ(2, message_handler_break_hit_count);
}


#ifndef V8_INTERPRETED_REGEXP
// Debug event handler which gets the function on the top frame and schedules a
// break a number of times.
static void DebugEventDebugBreak(
    const v8::Debug::EventDetails& event_details) {
  v8::DebugEvent event = event_details.GetEvent();
  v8::Handle<v8::Object> exec_state = event_details.GetExecutionState();

  if (event == v8::Break) {
    break_point_hit_count++;

    // Get the name of the top frame function.
    if (!frame_function_name.IsEmpty()) {
      // Get the name of the function.
      const int argc = 2;
      v8::Handle<v8::Value> argv[argc] = {
        exec_state, v8::Integer::New(CcTest::isolate(), 0)
      };
      v8::Handle<v8::Value> result = frame_function_name->Call(exec_state,
                                                               argc, argv);
      if (result->IsUndefined()) {
        last_function_hit[0] = '\x0';
      } else {
        CHECK(result->IsString());
        v8::Handle<v8::String> function_name(result->ToString());
        function_name->WriteUtf8(last_function_hit);
      }
    }

    // Keep forcing breaks.
    if (break_point_hit_count < 20) {
      v8::Debug::DebugBreak(CcTest::isolate());
    }
  }
}


TEST(RegExpDebugBreak) {
  // This test only applies to native regexps.
  DebugLocalContext env;
  v8::HandleScope scope(env->GetIsolate());

  // Create a function for checking the function when hitting a break point.
  frame_function_name = CompileFunction(&env,
                                        frame_function_name_source,
                                        "\x66\x72\x61\x6d\x65\x5f\x66\x75\x6e\x63\x74\x69\x6f\x6e\x5f\x6e\x61\x6d\x65");

  // Test RegExp which matches white spaces and comments at the begining of a
  // source line.
  const char* script =
    "\x76\x61\x72\x20\x73\x6f\x75\x72\x63\x65\x4c\x69\x6e\x65\x42\x65\x67\x69\x6e\x6e\x69\x6e\x67\x53\x6b\x69\x70\x20\x3d\x20\x2f\x5e\x28\x3f\x3a\x5b\x20\x5c\x76\x5c\x68\x5d\x2a\x28\x3f\x3a\x5c\x2f\x5c\x2a\x2e\x2a\x3f\x5c\x2a\x5c\x2f\x29\x2a\x29\x2a\x2f\x3b\xa"
    "\x66\x75\x6e\x63\x74\x69\x6f\x6e\x20\x66\x28\x73\x29\x20\x7b\x20\x72\x65\x74\x75\x72\x6e\x20\x73\x2e\x6d\x61\x74\x63\x68\x28\x73\x6f\x75\x72\x63\x65\x4c\x69\x6e\x65\x42\x65\x67\x69\x6e\x6e\x69\x6e\x67\x53\x6b\x69\x70\x29\x5b\x30\x5d\x2e\x6c\x65\x6e\x67\x74\x68\x3b\x20\x7d";

  v8::Local<v8::Function> f = CompileFunction(env->GetIsolate(), script, "\x66");
  const int argc = 1;
  v8::Handle<v8::Value> argv[argc] = {
      v8::String::NewFromUtf8(env->GetIsolate(), "\x20\x20\x2f\x2a\x20\x78\x78\x78\x20\x2a\x2f\x20\x61\x3d\x30\x3b")};
  v8::Local<v8::Value> result = f->Call(env->Global(), argc, argv);
  CHECK_EQ(12, result->Int32Value());

  v8::Debug::SetDebugEventListener(DebugEventDebugBreak);
  v8::Debug::DebugBreak(env->GetIsolate());
  result = f->Call(env->Global(), argc, argv);

  // Check that there was only one break event. Matching RegExp should not
  // cause Break events.
  CHECK_EQ(1, break_point_hit_count);
  CHECK_EQ("\x66", last_function_hit);
}
#endif  // V8_INTERPRETED_REGEXP


// Common part of EvalContextData and NestedBreakEventContextData tests.
static void ExecuteScriptForContextCheck(
    v8::Debug::MessageHandler message_handler) {
  // Create a context.
  v8::Handle<v8::Context> context_1;
  v8::Handle<v8::ObjectTemplate> global_template =
      v8::Handle<v8::ObjectTemplate>();
  context_1 =
      v8::Context::New(CcTest::isolate(), NULL, global_template);

  v8::Debug::SetMessageHandler(message_handler);

  // Default data value is undefined.
  CHECK(context_1->GetEmbedderData(0)->IsUndefined());

  // Set and check a data value.
  v8::Handle<v8::String> data_1 =
      v8::String::NewFromUtf8(CcTest::isolate(), "\x31");
  context_1->SetEmbedderData(0, data_1);
  CHECK(context_1->GetEmbedderData(0)->StrictEquals(data_1));

  // Simple test function with eval that causes a break.
  const char* source = "\x66\x75\x6e\x63\x74\x69\x6f\x6e\x20\x66\x28\x29\x20\x7b\x20\x65\x76\x61\x6c\x28\x27\x64\x65\x62\x75\x67\x67\x65\x72\x3b\x27\x29\x3b\x20\x7d";

  // Enter and run function in the context.
  {
    v8::Context::Scope context_scope(context_1);
    expected_context = context_1;
    expected_context_data = data_1;
    v8::Local<v8::Function> f = CompileFunction(CcTest::isolate(), source, "\x66");
    f->Call(context_1->Global(), 0, NULL);
  }

  v8::Debug::SetMessageHandler(NULL);
}


// Test which creates a context and sets embedder data on it. Checks that this
// data is set correctly and that when the debug message handler is called for
// break event in an eval statement the expected context is the one returned by
// Message.GetEventContext.
TEST(EvalContextData) {
  v8::HandleScope scope(CcTest::isolate());

  ExecuteScriptForContextCheck(ContextCheckMessageHandler);

  // One time compile event and one time break event.
  CHECK_GT(message_handler_hit_count, 2);
  CheckDebuggerUnloaded();
}


static bool sent_eval = false;
static int break_count = 0;
static int continue_command_send_count = 0;
// Check that the expected context is the one generating the debug event
// including the case of nested break event.
static void DebugEvalContextCheckMessageHandler(
    const v8::Debug::Message& message) {
  CHECK(message.GetEventContext() == expected_context);
  CHECK(message.GetEventContext()->GetEmbedderData(0)->StrictEquals(
      expected_context_data));
  message_handler_hit_count++;

  static char print_buffer[1000];
  v8::String::Value json(message.GetJSON());
  Utf16ToAscii(*json, json.length(), print_buffer);

  v8::Isolate* isolate = message.GetIsolate();
  if (IsBreakEventMessage(print_buffer)) {
    break_count++;
    if (!sent_eval) {
      sent_eval = true;

      const int kBufferSize = 1000;
      uint16_t buffer[kBufferSize];
      const char* eval_command =
          "\x7b\x22\x73\x65\x71\x22\x3a\x30\x2c"
          "\x22\x74\x79\x70\x65\x22\x3a\x22\x72\x65\x71\x75\x65\x73\x74\x22\x2c"
          "\x22\x63\x6f\x6d\x6d\x61\x6e\x64\x22\x3a\x22\x65\x76\x61\x6c\x75\x61\x74\x65\x22\x2c"
          "\x22\x61\x72\x67\x75\x6d\x65\x6e\x74\x73\x22\x3a\x7b\x22\x65\x78\x70\x72\x65\x73\x73\x69\x6f\x6e\x22\x3a\x22\x64\x65\x62\x75\x67\x67\x65\x72\x3b\x22\x2c"
          "\x22\x67\x6c\x6f\x62\x61\x6c\x22\x3a\x74\x72\x75\x65\x2c\x22\x64\x69\x73\x61\x62\x6c\x65\x5f\x62\x72\x65\x61\x6b\x22\x3a\x66\x61\x6c\x73\x65\x7d\x7d";

      // Send evaluate command.
      v8::Debug::SendCommand(
          isolate, buffer, AsciiToUtf16(eval_command, buffer));
      return;
    } else {
      // It's a break event caused by the evaluation request above.
      SendContinueCommand();
      continue_command_send_count++;
    }
  } else if (IsEvaluateResponseMessage(print_buffer) &&
      continue_command_send_count < 2) {
    // Response to the evaluation request. We're still on the breakpoint so
    // send continue.
    SendContinueCommand();
    continue_command_send_count++;
  }
}


// Tests that context returned for break event is correct when the event occurs
// in 'evaluate' debugger request.
TEST(NestedBreakEventContextData) {
  v8::HandleScope scope(CcTest::isolate());
  break_count = 0;
  message_handler_hit_count = 0;

  ExecuteScriptForContextCheck(DebugEvalContextCheckMessageHandler);

  // One time compile event and two times break event.
  CHECK_GT(message_handler_hit_count, 3);

  // One break from the source and another from the evaluate request.
  CHECK_EQ(break_count, 2);
  CheckDebuggerUnloaded();
}


// Debug event listener which counts the after compile events.
int after_compile_message_count = 0;
static void AfterCompileMessageHandler(const v8::Debug::Message& message) {
  // Count the number of scripts collected.
  if (message.IsEvent()) {
    if (message.GetEvent() == v8::AfterCompile) {
      after_compile_message_count++;
    } else if (message.GetEvent() == v8::Break) {
      SendContinueCommand();
    }
  }
}


// Tests that after compile event is sent as many times as there are scripts
// compiled.
TEST(AfterCompileMessageWhenMessageHandlerIsReset) {
  DebugLocalContext env;
  v8::HandleScope scope(env->GetIsolate());
  after_compile_message_count = 0;
  const char* script = "\x76\x61\x72\x20\x61\x3d\x31";

  v8::Debug::SetMessageHandler(AfterCompileMessageHandler);
  v8::Script::Compile(v8::String::NewFromUtf8(env->GetIsolate(), script))
      ->Run();
  v8::Debug::SetMessageHandler(NULL);

  v8::Debug::SetMessageHandler(AfterCompileMessageHandler);
  v8::Debug::DebugBreak(env->GetIsolate());
  v8::Script::Compile(v8::String::NewFromUtf8(env->GetIsolate(), script))
      ->Run();

  // Setting listener to NULL should cause debugger unload.
  v8::Debug::SetMessageHandler(NULL);
  CheckDebuggerUnloaded();

  // Compilation cache should be disabled when debugger is active.
  CHECK_EQ(2, after_compile_message_count);
}


// Syntax error event handler which counts a number of events.
int compile_error_event_count = 0;

static void CompileErrorEventCounterClear() {
  compile_error_event_count = 0;
}

static void CompileErrorEventCounter(
    const v8::Debug::EventDetails& event_details) {
  v8::DebugEvent event = event_details.GetEvent();

  if (event == v8::CompileError) {
    compile_error_event_count++;
  }
}


// Tests that syntax error event is sent as many times as there are scripts
// with syntax error compiled.
TEST(SyntaxErrorMessageOnSyntaxException) {
  DebugLocalContext env;
  v8::HandleScope scope(env->GetIsolate());

  // For this test, we want to break on uncaught exceptions:
  ChangeBreakOnException(false, true);

  v8::Debug::SetDebugEventListener(CompileErrorEventCounter);

  CompileErrorEventCounterClear();

  // Check initial state.
  CHECK_EQ(0, compile_error_event_count);

  // Throws SyntaxError: Unexpected end of input
  v8::Script::Compile(v8::String::NewFromUtf8(env->GetIsolate(), "\x2b\x2b\x2b"));
  CHECK_EQ(1, compile_error_event_count);

  v8::Script::Compile(
    v8::String::NewFromUtf8(env->GetIsolate(), "/sel\\/: \\"));
  CHECK_EQ(2, compile_error_event_count);

  v8::Script::Compile(
    v8::String::NewFromUtf8(env->GetIsolate(), "\x4a\x53\x4f\x4e\x2e\x70\x61\x72\x73\x65\x28\x27\x31\x32\x33\x34\x3a\x27\x29"));
  CHECK_EQ(2, compile_error_event_count);

  v8::Script::Compile(
    v8::String::NewFromUtf8(env->GetIsolate(), "\x6e\x65\x77\x20\x52\x65\x67\x45\x78\x70\x28\x27\x2f\x5c\x2f\x5c\x5c\x27\x29\x3b"));
  CHECK_EQ(2, compile_error_event_count);

  v8::Script::Compile(v8::String::NewFromUtf8(env->GetIsolate(), "\x74\x68\x72\x6f\x77\x20\x31\x3b"));
  CHECK_EQ(2, compile_error_event_count);
}


// Tests that break event is sent when message handler is reset.
TEST(BreakMessageWhenMessageHandlerIsReset) {
  DebugLocalContext env;
  v8::HandleScope scope(env->GetIsolate());
  after_compile_message_count = 0;
  const char* script = "\x66\x75\x6e\x63\x74\x69\x6f\x6e\x20\x66\x28\x29\x20\x7b\x7d\x3b";

  v8::Debug::SetMessageHandler(AfterCompileMessageHandler);
  v8::Script::Compile(v8::String::NewFromUtf8(env->GetIsolate(), script))
      ->Run();
  v8::Debug::SetMessageHandler(NULL);

  v8::Debug::SetMessageHandler(AfterCompileMessageHandler);
  v8::Debug::DebugBreak(env->GetIsolate());
  v8::Local<v8::Function> f = v8::Local<v8::Function>::Cast(
      env->Global()->Get(v8::String::NewFromUtf8(env->GetIsolate(), "\x66")));
  f->Call(env->Global(), 0, NULL);

  // Setting message handler to NULL should cause debugger unload.
  v8::Debug::SetMessageHandler(NULL);
  CheckDebuggerUnloaded();

  // Compilation cache should be disabled when debugger is active.
  CHECK_EQ(1, after_compile_message_count);
}


static int exception_event_count = 0;
static void ExceptionMessageHandler(const v8::Debug::Message& message) {
  if (message.IsEvent() && message.GetEvent() == v8::Exception) {
    exception_event_count++;
    SendContinueCommand();
  }
}


// Tests that exception event is sent when message handler is reset.
TEST(ExceptionMessageWhenMessageHandlerIsReset) {
  DebugLocalContext env;
  v8::HandleScope scope(env->GetIsolate());

  // For this test, we want to break on uncaught exceptions:
  ChangeBreakOnException(false, true);

  exception_event_count = 0;
  const char* script = "\x66\x75\x6e\x63\x74\x69\x6f\x6e\x20\x66\x28\x29\x20\x7b\x74\x68\x72\x6f\x77\x20\x6e\x65\x77\x20\x45\x72\x72\x6f\x72\x28\x29\x7d\x3b";

  v8::Debug::SetMessageHandler(AfterCompileMessageHandler);
  v8::Script::Compile(v8::String::NewFromUtf8(env->GetIsolate(), script))
      ->Run();
  v8::Debug::SetMessageHandler(NULL);

  v8::Debug::SetMessageHandler(ExceptionMessageHandler);
  v8::Local<v8::Function> f = v8::Local<v8::Function>::Cast(
      env->Global()->Get(v8::String::NewFromUtf8(env->GetIsolate(), "\x66")));
  f->Call(env->Global(), 0, NULL);

  // Setting message handler to NULL should cause debugger unload.
  v8::Debug::SetMessageHandler(NULL);
  CheckDebuggerUnloaded();

  CHECK_EQ(1, exception_event_count);
}


// Tests after compile event is sent when there are some provisional
// breakpoints out of the scripts lines range.
TEST(ProvisionalBreakpointOnLineOutOfRange) {
  DebugLocalContext env;
  v8::HandleScope scope(env->GetIsolate());
  env.ExposeDebug();
  const char* script = "\x66\x75\x6e\x63\x74\x69\x6f\x6e\x20\x66\x28\x29\x20\x7b\x7d\x3b";
  const char* resource_name = "\x74\x65\x73\x74\x5f\x72\x65\x73\x6f\x75\x72\x63\x65";

  // Set a couple of provisional breakpoint on lines out of the script lines
  // range.
  int sbp1 = SetScriptBreakPointByNameFromJS(env->GetIsolate(), resource_name,
                                             3, -1 /* no column */);
  int sbp2 =
      SetScriptBreakPointByNameFromJS(env->GetIsolate(), resource_name, 5, 5);

  after_compile_message_count = 0;
  v8::Debug::SetMessageHandler(AfterCompileMessageHandler);

  v8::ScriptOrigin origin(
      v8::String::NewFromUtf8(env->GetIsolate(), resource_name),
      v8::Integer::New(env->GetIsolate(), 10),
      v8::Integer::New(env->GetIsolate(), 1));
  // Compile a script whose first line number is greater than the breakpoints'
  // lines.
  v8::Script::Compile(v8::String::NewFromUtf8(env->GetIsolate(), script),
                      &origin)->Run();

  // If the script is compiled successfully there is exactly one after compile
  // event. In case of an exception in debugger code after compile event is not
  // sent.
  CHECK_EQ(1, after_compile_message_count);

  ClearBreakPointFromJS(env->GetIsolate(), sbp1);
  ClearBreakPointFromJS(env->GetIsolate(), sbp2);
  v8::Debug::SetMessageHandler(NULL);
}


static void BreakMessageHandler(const v8::Debug::Message& message) {
  i::Isolate* isolate = CcTest::i_isolate();
  if (message.IsEvent() && message.GetEvent() == v8::Break) {
    // Count the number of breaks.
    break_point_hit_count++;

    i::HandleScope scope(isolate);
    message.GetJSON();

    SendContinueCommand();
  } else if (message.IsEvent() && message.GetEvent() == v8::AfterCompile) {
    i::HandleScope scope(isolate);

    int current_count = break_point_hit_count;

    // Force serialization to trigger some internal JS execution.
    message.GetJSON();

    CHECK_EQ(current_count, break_point_hit_count);
  }
}


// Test that if DebugBreak is forced it is ignored when code from
// debug-delay.js is executed.
TEST(NoDebugBreakInAfterCompileMessageHandler) {
  DebugLocalContext env;
  v8::HandleScope scope(env->GetIsolate());

  // Register a debug event listener which sets the break flag and counts.
  v8::Debug::SetMessageHandler(BreakMessageHandler);

  // Set the debug break flag.
  v8::Debug::DebugBreak(env->GetIsolate());

  // Create a function for testing stepping.
  const char* src = "\x66\x75\x6e\x63\x74\x69\x6f\x6e\x20\x66\x28\x29\x20\x7b\x20\x65\x76\x61\x6c\x28\x27\x76\x61\x72\x20\x78\x20\x3d\x20\x31\x30\x3b\x27\x29\x3b\x20\x7d\x20";
  v8::Local<v8::Function> f = CompileFunction(&env, src, "\x66");

  // There should be only one break event.
  CHECK_EQ(1, break_point_hit_count);

  // Set the debug break flag again.
  v8::Debug::DebugBreak(env->GetIsolate());
  f->Call(env->Global(), 0, NULL);
  // There should be one more break event when the script is evaluated in 'f'.
  CHECK_EQ(2, break_point_hit_count);

  // Get rid of the debug message handler.
  v8::Debug::SetMessageHandler(NULL);
  CheckDebuggerUnloaded();
}


static int counting_message_handler_counter;

static void CountingMessageHandler(const v8::Debug::Message& message) {
  if (message.IsResponse()) counting_message_handler_counter++;
}


// Test that debug messages get processed when ProcessDebugMessages is called.
TEST(ProcessDebugMessages) {
  DebugLocalContext env;
  v8::Isolate* isolate = env->GetIsolate();
  v8::HandleScope scope(isolate);

  counting_message_handler_counter = 0;

  v8::Debug::SetMessageHandler(CountingMessageHandler);

  const int kBufferSize = 1000;
  uint16_t buffer[kBufferSize];
  const char* scripts_command =
    "\x7b\x22\x73\x65\x71\x22\x3a\x30\x2c"
     "\x22\x74\x79\x70\x65\x22\x3a\x22\x72\x65\x71\x75\x65\x73\x74\x22\x2c"
     "\x22\x63\x6f\x6d\x6d\x61\x6e\x64\x22\x3a\x22\x73\x63\x72\x69\x70\x74\x73\x22\x7d";

  // Send scripts command.
  v8::Debug::SendCommand(
      isolate, buffer, AsciiToUtf16(scripts_command, buffer));

  CHECK_EQ(0, counting_message_handler_counter);
  v8::Debug::ProcessDebugMessages();
  // At least one message should come
  CHECK_GE(counting_message_handler_counter, 1);

  counting_message_handler_counter = 0;

  v8::Debug::SendCommand(
      isolate, buffer, AsciiToUtf16(scripts_command, buffer));
  v8::Debug::SendCommand(
      isolate, buffer, AsciiToUtf16(scripts_command, buffer));
  CHECK_EQ(0, counting_message_handler_counter);
  v8::Debug::ProcessDebugMessages();
  // At least two messages should come
  CHECK_GE(counting_message_handler_counter, 2);

  // Get rid of the debug message handler.
  v8::Debug::SetMessageHandler(NULL);
  CheckDebuggerUnloaded();
}


class SendCommandThread : public v8::base::Thread {
 public:
  explicit SendCommandThread(v8::Isolate* isolate)
      : Thread(Options("\x53\x65\x6e\x64\x43\x6f\x6d\x6d\x61\x6e\x64\x54\x68\x72\x65\x61\x64")),
        semaphore_(0),
        isolate_(isolate) {}

  static void ProcessDebugMessages(v8::Isolate* isolate, void* data) {
    v8::Debug::ProcessDebugMessages();
    reinterpret_cast<v8::base::Semaphore*>(data)->Signal();
  }

  virtual void Run() {
    semaphore_.Wait();
    const int kBufferSize = 1000;
    uint16_t buffer[kBufferSize];
    const char* scripts_command =
      "\x7b\x22\x73\x65\x71\x22\x3a\x30\x2c"
       "\x22\x74\x79\x70\x65\x22\x3a\x22\x72\x65\x71\x75\x65\x73\x74\x22\x2c"
       "\x22\x63\x6f\x6d\x6d\x61\x6e\x64\x22\x3a\x22\x73\x63\x72\x69\x70\x74\x73\x22\x7d";
    int length = AsciiToUtf16(scripts_command, buffer);
    // Send scripts command.

    for (int i = 0; i < 100; i++) {
      CHECK_EQ(i, counting_message_handler_counter);
      // Queue debug message.
      v8::Debug::SendCommand(isolate_, buffer, length);
      // Synchronize with the main thread to force message processing.
      isolate_->RequestInterrupt(ProcessDebugMessages, &semaphore_);
      semaphore_.Wait();
    }

    v8::V8::TerminateExecution(isolate_);
  }

  void StartSending() {
    semaphore_.Signal();
  }

 private:
  v8::base::Semaphore semaphore_;
  v8::Isolate* isolate_;
};


static SendCommandThread* send_command_thread_ = NULL;

static void StartSendingCommands(
    const v8::FunctionCallbackInfo<v8::Value>& info) {
  send_command_thread_->StartSending();
}


TEST(ProcessDebugMessagesThreaded) {
  DebugLocalContext env;
  v8::Isolate* isolate = env->GetIsolate();
  v8::HandleScope scope(isolate);

  counting_message_handler_counter = 0;

  v8::Debug::SetMessageHandler(CountingMessageHandler);
  send_command_thread_ = new SendCommandThread(isolate);
  send_command_thread_->Start();

  v8::Handle<v8::FunctionTemplate> start =
      v8::FunctionTemplate::New(isolate, StartSendingCommands);
  env->Global()->Set(v8_str("\x73\x74\x61\x72\x74"), start->GetFunction());

  CompileRun("\x73\x74\x61\x72\x74\x28\x29\x3b\x20\x77\x68\x69\x6c\x65\x20\x28\x74\x72\x75\x65\x29\x20\x7b\x20\x7d");

  CHECK_EQ(100, counting_message_handler_counter);

  v8::Debug::SetMessageHandler(NULL);
  CheckDebuggerUnloaded();
}


struct BacktraceData {
  static int frame_counter;
  static void MessageHandler(const v8::Debug::Message& message) {
    char print_buffer[1000];
    v8::String::Value json(message.GetJSON());
    Utf16ToAscii(*json, json.length(), print_buffer, 1000);

    if (strstr(print_buffer, "\x62\x61\x63\x6b\x74\x72\x61\x63\x65") == NULL) {
      return;
    }
    frame_counter = GetTotalFramesInt(print_buffer);
  }
};

int BacktraceData::frame_counter;


// Test that debug messages get processed when ProcessDebugMessages is called.
TEST(Backtrace) {
  DebugLocalContext env;
  v8::Isolate* isolate = env->GetIsolate();
  v8::HandleScope scope(isolate);

  v8::Debug::SetMessageHandler(BacktraceData::MessageHandler);

  const int kBufferSize = 1000;
  uint16_t buffer[kBufferSize];
  const char* scripts_command =
    "\x7b\x22\x73\x65\x71\x22\x3a\x30\x2c"
     "\x22\x74\x79\x70\x65\x22\x3a\x22\x72\x65\x71\x75\x65\x73\x74\x22\x2c"
     "\x22\x63\x6f\x6d\x6d\x61\x6e\x64\x22\x3a\x22\x62\x61\x63\x6b\x74\x72\x61\x63\x65\x22\x7d";

  // Check backtrace from ProcessDebugMessages.
  BacktraceData::frame_counter = -10;
  v8::Debug::SendCommand(
      isolate,
      buffer,
      AsciiToUtf16(scripts_command, buffer),
      NULL);
  v8::Debug::ProcessDebugMessages();
  CHECK_EQ(BacktraceData::frame_counter, 0);

  v8::Handle<v8::String> void0 =
      v8::String::NewFromUtf8(env->GetIsolate(), "\x76\x6f\x69\x64\x28\x30\x29");
  v8::Handle<v8::Script> script = CompileWithOrigin(void0, void0);

  // Check backtrace from "void(0)" script.
  BacktraceData::frame_counter = -10;
  v8::Debug::SendCommand(
      isolate,
      buffer,
      AsciiToUtf16(scripts_command, buffer),
      NULL);
  script->Run();
  CHECK_EQ(BacktraceData::frame_counter, 1);

  // Get rid of the debug message handler.
  v8::Debug::SetMessageHandler(NULL);
  CheckDebuggerUnloaded();
}


TEST(GetMirror) {
  DebugLocalContext env;
  v8::Isolate* isolate = env->GetIsolate();
  v8::HandleScope scope(isolate);
  v8::Handle<v8::Value> obj =
      v8::Debug::GetMirror(v8::String::NewFromUtf8(isolate, "\x68\x6f\x64\x6a\x61"));
  v8::ScriptCompiler::Source source(v8_str(
      "\x66\x75\x6e\x63\x74\x69\x6f\x6e\x20\x72\x75\x6e\x54\x65\x73\x74\x28\x6d\x69\x72\x72\x6f\x72\x29\x20\x7b"
      "\x20\x20\x72\x65\x74\x75\x72\x6e\x20\x6d\x69\x72\x72\x6f\x72\x2e\x69\x73\x53\x74\x72\x69\x6e\x67\x28\x29\x20\x26\x26\x20\x28\x6d\x69\x72\x72\x6f\x72\x2e\x6c\x65\x6e\x67\x74\x68\x28\x29\x20\x3d\x3d\x20\x35\x29\x3b"
      "\x7d"
      ""
      "\x72\x75\x6e\x54\x65\x73\x74\x3b"));
  v8::Handle<v8::Function> run_test = v8::Handle<v8::Function>::Cast(
      v8::ScriptCompiler::CompileUnbound(isolate, &source)
          ->BindToCurrentContext()
          ->Run());
  v8::Handle<v8::Value> result = run_test->Call(env->Global(), 1, &obj);
  CHECK(result->IsTrue());
}


// Test that the debug break flag works with function.apply.
TEST(DebugBreakFunctionApply) {
  DebugLocalContext env;
  v8::HandleScope scope(env->GetIsolate());

  // Create a function for testing breaking in apply.
  v8::Local<v8::Function> foo = CompileFunction(
      &env,
      "\x66\x75\x6e\x63\x74\x69\x6f\x6e\x20\x62\x61\x7a\x28\x78\x29\x20\x7b\x20\x7d"
      "\x66\x75\x6e\x63\x74\x69\x6f\x6e\x20\x62\x61\x72\x28\x78\x29\x20\x7b\x20\x62\x61\x7a\x28\x29\x3b\x20\x7d"
      "\x66\x75\x6e\x63\x74\x69\x6f\x6e\x20\x66\x6f\x6f\x28\x29\x7b\x20\x62\x61\x72\x2e\x61\x70\x70\x6c\x79\x28\x74\x68\x69\x73\x2c\x20\x5b\x31\x5d\x29\x3b\x20\x7d",
      "\x66\x6f\x6f");

  // Register a debug event listener which steps and counts.
  v8::Debug::SetDebugEventListener(DebugEventBreakMax);

  // Set the debug break flag before calling the code using function.apply.
  v8::Debug::DebugBreak(env->GetIsolate());

  // Limit the number of debug breaks. This is a regression test for issue 493
  // where this test would enter an infinite loop.
  break_point_hit_count = 0;
  max_break_point_hit_count = 10000;  // 10000 => infinite loop.
  foo->Call(env->Global(), 0, NULL);

  // When keeping the debug break several break will happen.
  CHECK_GT(break_point_hit_count, 1);

  v8::Debug::SetDebugEventListener(NULL);
  CheckDebuggerUnloaded();
}


v8::Handle<v8::Context> debugee_context;
v8::Handle<v8::Context> debugger_context;


// Property getter that checks that current and calling contexts
// are both the debugee contexts.
static void NamedGetterWithCallingContextCheck(
    v8::Local<v8::String> name,
    const v8::PropertyCallbackInfo<v8::Value>& info) {
  CHECK_EQ(0, strcmp(*v8::String::Utf8Value(name), "\x61"));
  v8::Handle<v8::Context> current = info.GetIsolate()->GetCurrentContext();
  CHECK(current == debugee_context);
  CHECK(current != debugger_context);
  v8::Handle<v8::Context> calling = info.GetIsolate()->GetCallingContext();
  CHECK(calling == debugee_context);
  CHECK(calling != debugger_context);
  info.GetReturnValue().Set(1);
}


// Debug event listener that checks if the first argument of a function is
// an object with property 'a' == 1. If the property has custom accessor
// this handler will eventually invoke it.
static void DebugEventGetAtgumentPropertyValue(
    const v8::Debug::EventDetails& event_details) {
  v8::DebugEvent event = event_details.GetEvent();
  v8::Handle<v8::Object> exec_state = event_details.GetExecutionState();
  if (event == v8::Break) {
    break_point_hit_count++;
    CHECK(debugger_context == CcTest::isolate()->GetCurrentContext());
    v8::Handle<v8::Function> func = v8::Handle<v8::Function>::Cast(CompileRun(
        "\x28\x66\x75\x6e\x63\x74\x69\x6f\x6e\x28\x65\x78\x65\x63\x5f\x73\x74\x61\x74\x65\x29\x20\x7b\xa"
        "\x20\x20\x20\x20\x72\x65\x74\x75\x72\x6e\x20\x28\x65\x78\x65\x63\x5f\x73\x74\x61\x74\x65\x2e\x66\x72\x61\x6d\x65\x28\x30\x29\x2e\x61\x72\x67\x75\x6d\x65\x6e\x74\x56\x61\x6c\x75\x65\x28\x30\x29\x2e\x70\x72\x6f\x70\x65\x72\x74\x79\x28\x27\x61\x27\x29\x2e\xa"
        "\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x76\x61\x6c\x75\x65\x28\x29\x2e\x76\x61\x6c\x75\x65\x28\x29\x20\x3d\x3d\x20\x31\x29\x3b\xa"
        "\x7d\x29"));
    const int argc = 1;
    v8::Handle<v8::Value> argv[argc] = { exec_state };
    v8::Handle<v8::Value> result = func->Call(exec_state, argc, argv);
    CHECK(result->IsTrue());
  }
}


TEST(CallingContextIsNotDebugContext) {
  v8::internal::Debug* debug = CcTest::i_isolate()->debug();
  // Create and enter a debugee context.
  DebugLocalContext env;
  v8::Isolate* isolate = env->GetIsolate();
  v8::HandleScope scope(isolate);
  env.ExposeDebug();

  // Save handles to the debugger and debugee contexts to be used in
  // NamedGetterWithCallingContextCheck.
  debugee_context = env.context();
  debugger_context = v8::Utils::ToLocal(debug->debug_context());

  // Create object with 'a' property accessor.
  v8::Handle<v8::ObjectTemplate> named = v8::ObjectTemplate::New(isolate);
  named->SetAccessor(v8::String::NewFromUtf8(isolate, "\x61"),
                     NamedGetterWithCallingContextCheck);
  env->Global()->Set(v8::String::NewFromUtf8(isolate, "\x6f\x62\x6a"),
                     named->NewInstance());

  // Register the debug event listener
  v8::Debug::SetDebugEventListener(DebugEventGetAtgumentPropertyValue);

  // Create a function that invokes debugger.
  v8::Local<v8::Function> foo = CompileFunction(
      &env,
      "\x66\x75\x6e\x63\x74\x69\x6f\x6e\x20\x62\x61\x72\x28\x78\x29\x20\x7b\x20\x64\x65\x62\x75\x67\x67\x65\x72\x3b\x20\x7d"
      "\x66\x75\x6e\x63\x74\x69\x6f\x6e\x20\x66\x6f\x6f\x28\x29\x7b\x20\x62\x61\x72\x28\x6f\x62\x6a\x29\x3b\x20\x7d",
      "\x66\x6f\x6f");

  break_point_hit_count = 0;
  foo->Call(env->Global(), 0, NULL);
  CHECK_EQ(1, break_point_hit_count);

  v8::Debug::SetDebugEventListener(NULL);
  debugee_context = v8::Handle<v8::Context>();
  debugger_context = v8::Handle<v8::Context>();
  CheckDebuggerUnloaded();
}


TEST(DebugContextIsPreservedBetweenAccesses) {
  v8::HandleScope scope(CcTest::isolate());
  v8::Debug::SetDebugEventListener(DebugEventBreakPointHitCount);
  v8::Local<v8::Context> context1 = v8::Debug::GetDebugContext();
  v8::Local<v8::Context> context2 = v8::Debug::GetDebugContext();
  CHECK(v8::Utils::OpenHandle(*context1).is_identical_to(
            v8::Utils::OpenHandle(*context2)));
  v8::Debug::SetDebugEventListener(NULL);
}


static v8::Handle<v8::Value> expected_callback_data;
static void DebugEventContextChecker(const v8::Debug::EventDetails& details) {
  CHECK(details.GetEventContext() == expected_context);
  CHECK_EQ(expected_callback_data, details.GetCallbackData());
}


// Check that event details contain context where debug event occured.
TEST(DebugEventContext) {
  v8::Isolate* isolate = CcTest::isolate();
  v8::HandleScope scope(isolate);
  expected_context = v8::Context::New(isolate);
  expected_callback_data = v8::Int32::New(isolate, 2010);
  v8::Debug::SetDebugEventListener(DebugEventContextChecker,
                                    expected_callback_data);
  v8::Context::Scope context_scope(expected_context);
  v8::Script::Compile(
      v8::String::NewFromUtf8(isolate, "\x28\x66\x75\x6e\x63\x74\x69\x6f\x6e\x28\x29\x7b\x64\x65\x62\x75\x67\x67\x65\x72\x3b\x7d\x29\x28\x29\x3b"))->Run();
  expected_context.Clear();
  v8::Debug::SetDebugEventListener(NULL);
  expected_context_data = v8::Handle<v8::Value>();
  CheckDebuggerUnloaded();
}


static void* expected_break_data;
static bool was_debug_break_called;
static bool was_debug_event_called;
static void DebugEventBreakDataChecker(const v8::Debug::EventDetails& details) {
  if (details.GetEvent() == v8::BreakForCommand) {
    CHECK_EQ(expected_break_data, details.GetClientData());
    was_debug_event_called = true;
  } else if (details.GetEvent() == v8::Break) {
    was_debug_break_called = true;
  }
}


// Check that event details contain context where debug event occured.
TEST(DebugEventBreakData) {
  DebugLocalContext env;
  v8::Isolate* isolate = env->GetIsolate();
  v8::HandleScope scope(isolate);
  v8::Debug::SetDebugEventListener(DebugEventBreakDataChecker);

  TestClientData::constructor_call_counter = 0;
  TestClientData::destructor_call_counter = 0;

  expected_break_data = NULL;
  was_debug_event_called = false;
  was_debug_break_called = false;
  v8::Debug::DebugBreakForCommand(isolate, NULL);
  v8::Script::Compile(v8::String::NewFromUtf8(env->GetIsolate(),
                                              "\x28\x66\x75\x6e\x63\x74\x69\x6f\x6e\x28\x78\x29\x7b\x72\x65\x74\x75\x72\x6e\x20\x78\x3b\x7d\x29\x28\x31\x29\x3b"))
      ->Run();
  CHECK(was_debug_event_called);
  CHECK(!was_debug_break_called);

  TestClientData* data1 = new TestClientData();
  expected_break_data = data1;
  was_debug_event_called = false;
  was_debug_break_called = false;
  v8::Debug::DebugBreakForCommand(isolate, data1);
  v8::Script::Compile(v8::String::NewFromUtf8(env->GetIsolate(),
                                              "\x28\x66\x75\x6e\x63\x74\x69\x6f\x6e\x28\x78\x29\x7b\x72\x65\x74\x75\x72\x6e\x20\x78\x2b\x31\x3b\x7d\x29\x28\x31\x29\x3b"))
      ->Run();
  CHECK(was_debug_event_called);
  CHECK(!was_debug_break_called);

  expected_break_data = NULL;
  was_debug_event_called = false;
  was_debug_break_called = false;
  v8::Debug::DebugBreak(isolate);
  v8::Script::Compile(v8::String::NewFromUtf8(env->GetIsolate(),
                                              "\x28\x66\x75\x6e\x63\x74\x69\x6f\x6e\x28\x78\x29\x7b\x72\x65\x74\x75\x72\x6e\x20\x78\x2b\x32\x3b\x7d\x29\x28\x31\x29\x3b"))
      ->Run();
  CHECK(!was_debug_event_called);
  CHECK(was_debug_break_called);

  TestClientData* data2 = new TestClientData();
  expected_break_data = data2;
  was_debug_event_called = false;
  was_debug_break_called = false;
  v8::Debug::DebugBreak(isolate);
  v8::Debug::DebugBreakForCommand(isolate, data2);
  v8::Script::Compile(v8::String::NewFromUtf8(env->GetIsolate(),
                                              "\x28\x66\x75\x6e\x63\x74\x69\x6f\x6e\x28\x78\x29\x7b\x72\x65\x74\x75\x72\x6e\x20\x78\x2b\x33\x3b\x7d\x29\x28\x31\x29\x3b"))
      ->Run();
  CHECK(was_debug_event_called);
  CHECK(was_debug_break_called);

  CHECK_EQ(2, TestClientData::constructor_call_counter);
  CHECK_EQ(TestClientData::constructor_call_counter,
           TestClientData::destructor_call_counter);

  v8::Debug::SetDebugEventListener(NULL);
  CheckDebuggerUnloaded();
}

static bool debug_event_break_deoptimize_done = false;

static void DebugEventBreakDeoptimize(
    const v8::Debug::EventDetails& event_details) {
  v8::DebugEvent event = event_details.GetEvent();
  v8::Handle<v8::Object> exec_state = event_details.GetExecutionState();
  if (event == v8::Break) {
    if (!frame_function_name.IsEmpty()) {
      // Get the name of the function.
      const int argc = 2;
      v8::Handle<v8::Value> argv[argc] = {
        exec_state, v8::Integer::New(CcTest::isolate(), 0)
      };
      v8::Handle<v8::Value> result =
          frame_function_name->Call(exec_state, argc, argv);
      if (!result->IsUndefined()) {
        char fn[80];
        CHECK(result->IsString());
        v8::Handle<v8::String> function_name(result->ToString());
        function_name->WriteUtf8(fn);
        if (strcmp(fn, "\x62\x61\x72") == 0) {
          i::Deoptimizer::DeoptimizeAll(CcTest::i_isolate());
          debug_event_break_deoptimize_done = true;
        }
      }
    }

    v8::Debug::DebugBreak(CcTest::isolate());
  }
}


// Test deoptimization when execution is broken using the debug break stack
// check interrupt.
TEST(DeoptimizeDuringDebugBreak) {
  DebugLocalContext env;
  v8::HandleScope scope(env->GetIsolate());
  env.ExposeDebug();

  // Create a function for checking the function when hitting a break point.
  frame_function_name = CompileFunction(&env,
                                        frame_function_name_source,
                                        "\x66\x72\x61\x6d\x65\x5f\x66\x75\x6e\x63\x74\x69\x6f\x6e\x5f\x6e\x61\x6d\x65");


  // Set a debug event listener which will keep interrupting execution until
  // debug break. When inside function bar it will deoptimize all functions.
  // This tests lazy deoptimization bailout for the stack check, as the first
  // time in function bar when using debug break and no break points will be at
  // the initial stack check.
  v8::Debug::SetDebugEventListener(DebugEventBreakDeoptimize);

  // Compile and run function bar which will optimize it for some flag settings.
  v8::Script::Compile(v8::String::NewFromUtf8(
                          env->GetIsolate(), "\x66\x75\x6e\x63\x74\x69\x6f\x6e\x20\x62\x61\x72\x28\x29\x7b\x7d\x3b\x20\x62\x61\x72\x28\x29"))->Run();

  // Set debug break and call bar again.
  v8::Debug::DebugBreak(env->GetIsolate());
  v8::Script::Compile(v8::String::NewFromUtf8(env->GetIsolate(), "\x62\x61\x72\x28\x29"))
      ->Run();

  CHECK(debug_event_break_deoptimize_done);

  v8::Debug::SetDebugEventListener(NULL);
}


static void DebugEventBreakWithOptimizedStack(
    const v8::Debug::EventDetails& event_details) {
  v8::Isolate* isolate = event_details.GetEventContext()->GetIsolate();
  v8::DebugEvent event = event_details.GetEvent();
  v8::Handle<v8::Object> exec_state = event_details.GetExecutionState();
  if (event == v8::Break) {
    if (!frame_function_name.IsEmpty()) {
      for (int i = 0; i < 2; i++) {
        const int argc = 2;
        v8::Handle<v8::Value> argv[argc] = {
          exec_state, v8::Integer::New(isolate, i)
        };
        // Get the name of the function in frame i.
        v8::Handle<v8::Value> result =
            frame_function_name->Call(exec_state, argc, argv);
        CHECK(result->IsString());
        v8::Handle<v8::String> function_name(result->ToString());
        CHECK(function_name->Equals(v8::String::NewFromUtf8(isolate, "\x6c\x6f\x6f\x70")));
        // Get the name of the first argument in frame i.
        result = frame_argument_name->Call(exec_state, argc, argv);
        CHECK(result->IsString());
        v8::Handle<v8::String> argument_name(result->ToString());
        CHECK(argument_name->Equals(v8::String::NewFromUtf8(isolate, "\x63\x6f\x75\x6e\x74")));
        // Get the value of the first argument in frame i. If the
        // funtion is optimized the value will be undefined, otherwise
        // the value will be '1 - i'.
        //
        // TODO(3141533): We should be able to get the real value for
        // optimized frames.
        result = frame_argument_value->Call(exec_state, argc, argv);
        CHECK(result->IsUndefined() || (result->Int32Value() == 1 - i));
        // Get the name of the first local variable.
        result = frame_local_name->Call(exec_state, argc, argv);
        CHECK(result->IsString());
        v8::Handle<v8::String> local_name(result->ToString());
        CHECK(local_name->Equals(v8::String::NewFromUtf8(isolate, "\x6c\x6f\x63\x61\x6c")));
        // Get the value of the first local variable. If the function
        // is optimized the value will be undefined, otherwise it will
        // be 42.
        //
        // TODO(3141533): We should be able to get the real value for
        // optimized frames.
        result = frame_local_value->Call(exec_state, argc, argv);
        CHECK(result->IsUndefined() || (result->Int32Value() == 42));
      }
    }
  }
}


static void ScheduleBreak(const v8::FunctionCallbackInfo<v8::Value>& args) {
  v8::Debug::SetDebugEventListener(DebugEventBreakWithOptimizedStack);
  v8::Debug::DebugBreak(args.GetIsolate());
}


TEST(DebugBreakStackInspection) {
  DebugLocalContext env;
  v8::HandleScope scope(env->GetIsolate());

  frame_function_name =
      CompileFunction(&env, frame_function_name_source, "\x66\x72\x61\x6d\x65\x5f\x66\x75\x6e\x63\x74\x69\x6f\x6e\x5f\x6e\x61\x6d\x65");
  frame_argument_name =
      CompileFunction(&env, frame_argument_name_source, "\x66\x72\x61\x6d\x65\x5f\x61\x72\x67\x75\x6d\x65\x6e\x74\x5f\x6e\x61\x6d\x65");
  frame_argument_value = CompileFunction(&env,
                                         frame_argument_value_source,
                                         "\x66\x72\x61\x6d\x65\x5f\x61\x72\x67\x75\x6d\x65\x6e\x74\x5f\x76\x61\x6c\x75\x65");
  frame_local_name =
      CompileFunction(&env, frame_local_name_source, "\x66\x72\x61\x6d\x65\x5f\x6c\x6f\x63\x61\x6c\x5f\x6e\x61\x6d\x65");
  frame_local_value =
      CompileFunction(&env, frame_local_value_source, "\x66\x72\x61\x6d\x65\x5f\x6c\x6f\x63\x61\x6c\x5f\x76\x61\x6c\x75\x65");

  v8::Handle<v8::FunctionTemplate> schedule_break_template =
      v8::FunctionTemplate::New(env->GetIsolate(), ScheduleBreak);
  v8::Handle<v8::Function> schedule_break =
      schedule_break_template->GetFunction();
  env->Global()->Set(v8_str("\x73\x63\x68\x65\x64\x75\x6c\x65\x42\x72\x65\x61\x6b"), schedule_break);

  const char* src =
      "\x66\x75\x6e\x63\x74\x69\x6f\x6e\x20\x6c\x6f\x6f\x70\x28\x63\x6f\x75\x6e\x74\x29\x20\x7b"
      "\x20\x20\x76\x61\x72\x20\x6c\x6f\x63\x61\x6c\x20\x3d\x20\x34\x32\x3b"
      "\x20\x20\x69\x66\x20\x28\x63\x6f\x75\x6e\x74\x20\x3c\x20\x31\x29\x20\x7b\x20\x73\x63\x68\x65\x64\x75\x6c\x65\x42\x72\x65\x61\x6b\x28\x29\x3b\x20\x6c\x6f\x6f\x70\x28\x63\x6f\x75\x6e\x74\x20\x2b\x20\x31\x29\x3b\x20\x7d"
      "\x7d"
      "\x6c\x6f\x6f\x70\x28\x30\x29\x3b";
  v8::Script::Compile(v8::String::NewFromUtf8(env->GetIsolate(), src))->Run();
}


// Test that setting the terminate execution flag during debug break processing.
static void TestDebugBreakInLoop(const char* loop_head,
                                 const char** loop_bodies,
                                 const char* loop_tail) {
  // Receive 100 breaks for each test and then terminate JavaScript execution.
  static const int kBreaksPerTest = 100;

  for (int i = 0; loop_bodies[i] != NULL; i++) {
    // Perform a lazy deoptimization after various numbers of breaks
    // have been hit.
    for (int j = 0; j < 7; j++) {
      break_point_hit_count_deoptimize = j;
      if (j == 6) {
        break_point_hit_count_deoptimize = kBreaksPerTest;
      }

      break_point_hit_count = 0;
      max_break_point_hit_count = kBreaksPerTest;
      terminate_after_max_break_point_hit = true;

      EmbeddedVector<char, 1024> buffer;
      SNPrintF(buffer,
               "\x66\x75\x6e\x63\x74\x69\x6f\x6e\x20\x66\x28\x29\x20\x7b\x6c\xa2\x6c\xa2\x6c\xa2\x7d",
               loop_head, loop_bodies[i], loop_tail);

      // Function with infinite loop.
      CompileRun(buffer.start());

      // Set the debug break to enter the debugger as soon as possible.
      v8::Debug::DebugBreak(CcTest::isolate());

      // Call function with infinite loop.
      CompileRun("\x66\x28\x29\x3b");
      CHECK_EQ(kBreaksPerTest, break_point_hit_count);

      CHECK(!v8::V8::IsExecutionTerminating());
    }
  }
}


TEST(DebugBreakLoop) {
  DebugLocalContext env;
  v8::HandleScope scope(env->GetIsolate());

  // Register a debug event listener which sets the break flag and counts.
  v8::Debug::SetDebugEventListener(DebugEventBreakMax);

  // Create a function for getting the frame count when hitting the break.
  frame_count = CompileFunction(&env, frame_count_source, "\x66\x72\x61\x6d\x65\x5f\x63\x6f\x75\x6e\x74");

  CompileRun("\x76\x61\x72\x20\x61\x20\x3d\x20\x31\x3b");
  CompileRun("\x66\x75\x6e\x63\x74\x69\x6f\x6e\x20\x67\x28\x29\x20\x7b\x20\x7d");
  CompileRun("\x66\x75\x6e\x63\x74\x69\x6f\x6e\x20\x68\x28\x29\x20\x7b\x20\x7d");

  const char* loop_bodies[] = {
      "",
      "\x67\x28\x29",
      "\x69\x66\x20\x28\x61\x20\x3d\x3d\x20\x30\x29\x20\x7b\x20\x67\x28\x29\x20\x7d",
      "\x69\x66\x20\x28\x61\x20\x3d\x3d\x20\x31\x29\x20\x7b\x20\x67\x28\x29\x20\x7d",
      "\x69\x66\x20\x28\x61\x20\x3d\x3d\x20\x30\x29\x20\x7b\x20\x67\x28\x29\x20\x7d\x20\x65\x6c\x73\x65\x20\x7b\x20\x68\x28\x29\x20\x7d",
      "\x69\x66\x20\x28\x61\x20\x3d\x3d\x20\x30\x29\x20\x7b\x20\x63\x6f\x6e\x74\x69\x6e\x75\x65\x20\x7d",
      "\x69\x66\x20\x28\x61\x20\x3d\x3d\x20\x31\x29\x20\x7b\x20\x63\x6f\x6e\x74\x69\x6e\x75\x65\x20\x7d",
      "\x73\x77\x69\x74\x63\x68\x20\x28\x61\x29\x20\x7b\x20\x63\x61\x73\x65\x20\x31\x3a\x20\x67\x28\x29\x3b\x20\x7d",
      "\x73\x77\x69\x74\x63\x68\x20\x28\x61\x29\x20\x7b\x20\x63\x61\x73\x65\x20\x31\x3a\x20\x63\x6f\x6e\x74\x69\x6e\x75\x65\x3b\x20\x7d",
      "\x73\x77\x69\x74\x63\x68\x20\x28\x61\x29\x20\x7b\x20\x63\x61\x73\x65\x20\x31\x3a\x20\x67\x28\x29\x3b\x20\x62\x72\x65\x61\x6b\x3b\x20\x64\x65\x66\x61\x75\x6c\x74\x3a\x20\x68\x28\x29\x20\x7d",
      "\x73\x77\x69\x74\x63\x68\x20\x28\x61\x29\x20\x7b\x20\x63\x61\x73\x65\x20\x31\x3a\x20\x63\x6f\x6e\x74\x69\x6e\x75\x65\x3b\x20\x62\x72\x65\x61\x6b\x3b\x20\x64\x65\x66\x61\x75\x6c\x74\x3a\x20\x68\x28\x29\x20\x7d",
      NULL
  };

  TestDebugBreakInLoop("\x77\x68\x69\x6c\x65\x20\x28\x74\x72\x75\x65\x29\x20\x7b", loop_bodies, "\x7d");
  TestDebugBreakInLoop("\x77\x68\x69\x6c\x65\x20\x28\x61\x20\x3d\x3d\x20\x31\x29\x20\x7b", loop_bodies, "\x7d");

  TestDebugBreakInLoop("\x64\x6f\x20\x7b", loop_bodies, "\x7d\x20\x77\x68\x69\x6c\x65\x20\x28\x74\x72\x75\x65\x29");
  TestDebugBreakInLoop("\x64\x6f\x20\x7b", loop_bodies, "\x7d\x20\x77\x68\x69\x6c\x65\x20\x28\x61\x20\x3d\x3d\x20\x31\x29");

  TestDebugBreakInLoop("\x66\x6f\x72\x20\x28\x3b\x3b\x29\x20\x7b", loop_bodies, "\x7d");
  TestDebugBreakInLoop("\x66\x6f\x72\x20\x28\x3b\x61\x20\x3d\x3d\x20\x31\x3b\x29\x20\x7b", loop_bodies, "\x7d");

  // Get rid of the debug event listener.
  v8::Debug::SetDebugEventListener(NULL);
  CheckDebuggerUnloaded();
}


v8::Local<v8::Script> inline_script;

static void DebugBreakInlineListener(
    const v8::Debug::EventDetails& event_details) {
  v8::DebugEvent event = event_details.GetEvent();
  if (event != v8::Break) return;

  int expected_frame_count = 4;
  int expected_line_number[] = {1, 4, 7, 12};

  i::Handle<i::Object> compiled_script = v8::Utils::OpenHandle(*inline_script);
  i::Handle<i::Script> source_script = i::Handle<i::Script>(i::Script::cast(
      i::JSFunction::cast(*compiled_script)->shared()->script()));

  int break_id = CcTest::i_isolate()->debug()->break_id();
  char script[128];
  i::Vector<char> script_vector(script, sizeof(script));
  SNPrintF(script_vector, "\x25\x6c\xc7\x85\x74\x46\x72\x61\x6d\x65\x43\x6f\x75\x6e\x74\x28\x6c\x84\x29", break_id);
  v8::Local<v8::Value> result = CompileRun(script);

  int frame_count = result->Int32Value();
  CHECK_EQ(expected_frame_count, frame_count);

  for (int i = 0; i < frame_count; i++) {
    // The 5. element in the returned array of GetFrameDetails contains the
    // source position of that frame.
    SNPrintF(script_vector, "\x25\x6c\xc7\x85\x74\x46\x72\x61\x6d\x65\x44\x65\x74\x61\x69\x6c\x73\x28\x6c\x84\x2c\x20\x6c\x84\x29\x5b\x35\x5d", break_id, i);
    v8::Local<v8::Value> result = CompileRun(script);
    CHECK_EQ(expected_line_number[i],
             i::Script::GetLineNumber(source_script, result->Int32Value()));
  }
  v8::Debug::SetDebugEventListener(NULL);
  v8::V8::TerminateExecution(CcTest::isolate());
}


TEST(DebugBreakInline) {
  i::FLAG_allow_natives_syntax = true;
  DebugLocalContext env;
  v8::HandleScope scope(env->GetIsolate());
  const char* source =
      "\x66\x75\x6e\x63\x74\x69\x6f\x6e\x20\x64\x65\x62\x75\x67\x28\x62\x29\x20\x7b\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\xa"
      "\x20\x20\x69\x66\x20\x28\x62\x29\x20\x64\x65\x62\x75\x67\x67\x65\x72\x3b\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\xa"
      "\x7d\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\xa"
      "\x66\x75\x6e\x63\x74\x69\x6f\x6e\x20\x66\x28\x62\x29\x20\x7b\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\xa"
      "\x20\x20\x64\x65\x62\x75\x67\x28\x62\x29\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\xa"
      "\x7d\x3b\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\xa"
      "\x66\x75\x6e\x63\x74\x69\x6f\x6e\x20\x67\x28\x62\x29\x20\x7b\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\xa"
      "\x20\x20\x66\x28\x62\x29\x3b\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\xa"
      "\x7d\x3b\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\xa"
      "\x67\x28\x66\x61\x6c\x73\x65\x29\x3b\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\xa"
      "\x67\x28\x66\x61\x6c\x73\x65\x29\x3b\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\xa"
      "\x25\x4f\x70\x74\x69\x6d\x69\x7a\x65\x46\x75\x6e\x63\x74\x69\x6f\x6e\x4f\x6e\x4e\x65\x78\x74\x43\x61\x6c\x6c\x28\x67\x29\x3b\x20\xa"
      "\x67\x28\x74\x72\x75\x65\x29\x3b";
  v8::Debug::SetDebugEventListener(DebugBreakInlineListener);
  inline_script =
      v8::Script::Compile(v8::String::NewFromUtf8(env->GetIsolate(), source));
  inline_script->Run();
}


static void DebugEventStepNext(
    const v8::Debug::EventDetails& event_details) {
  v8::DebugEvent event = event_details.GetEvent();
  if (event == v8::Break) {
    PrepareStep(StepNext);
  }
}


static void RunScriptInANewCFrame(const char* source) {
  v8::TryCatch try_catch;
  CompileRun(source);
  CHECK(try_catch.HasCaught());
}


TEST(Regress131642) {
  // Bug description:
  // When doing StepNext through the first script, the debugger is not reset
  // after exiting through exception.  A flawed implementation enabling the
  // debugger to step into Array.prototype.forEach breaks inside the callback
  // for forEach in the second script under the assumption that we are in a
  // recursive call.  In an attempt to step out, we crawl the stack using the
  // recorded frame pointer from the first script and fail when not finding it
  // on the stack.
  DebugLocalContext env;
  v8::HandleScope scope(env->GetIsolate());
  v8::Debug::SetDebugEventListener(DebugEventStepNext);

  // We step through the first script.  It exits through an exception.  We run
  // this inside a new frame to record a different FP than the second script
  // would expect.
  const char* script_1 = "\x64\x65\x62\x75\x67\x67\x65\x72\x3b\x20\x74\x68\x72\x6f\x77\x20\x6e\x65\x77\x20\x45\x72\x72\x6f\x72\x28\x29\x3b";
  RunScriptInANewCFrame(script_1);

  // The second script uses forEach.
  const char* script_2 = "\x5b\x30\x5d\x2e\x66\x6f\x72\x45\x61\x63\x68\x28\x66\x75\x6e\x63\x74\x69\x6f\x6e\x28\x29\x20\x7b\x20\x7d\x29\x3b";
  CompileRun(script_2);

  v8::Debug::SetDebugEventListener(NULL);
}


// Import from test-heap.cc
int CountNativeContexts();


static void NopListener(const v8::Debug::EventDetails& event_details) {
}


TEST(DebuggerCreatesContextIffActive) {
  DebugLocalContext env;
  v8::HandleScope scope(env->GetIsolate());
  CHECK_EQ(1, CountNativeContexts());

  v8::Debug::SetDebugEventListener(NULL);
  CompileRun("\x64\x65\x62\x75\x67\x67\x65\x72\x3b");
  CHECK_EQ(1, CountNativeContexts());

  v8::Debug::SetDebugEventListener(NopListener);
  CompileRun("\x64\x65\x62\x75\x67\x67\x65\x72\x3b");
  CHECK_EQ(2, CountNativeContexts());

  v8::Debug::SetDebugEventListener(NULL);
}


TEST(LiveEditEnabled) {
  v8::internal::FLAG_allow_natives_syntax = true;
  LocalContext env;
  v8::HandleScope scope(env->GetIsolate());
  v8::Debug::SetLiveEditEnabled(env->GetIsolate(), true);
  CompileRun("\x6c\xd3\x89\x76\x65\x45\x64\x69\x74\x43\x6f\x6d\x70\x61\x72\x65\x53\x74\x72\x69\x6e\x67\x73\x28\x27\x27\x2c\x20\x27\x27\x29");
}


TEST(LiveEditDisabled) {
  v8::internal::FLAG_allow_natives_syntax = true;
  LocalContext env;
  v8::HandleScope scope(env->GetIsolate());
  v8::Debug::SetLiveEditEnabled(env->GetIsolate(), false);
  CompileRun("\x6c\xd3\x89\x76\x65\x45\x64\x69\x74\x43\x6f\x6d\x70\x61\x72\x65\x53\x74\x72\x69\x6e\x67\x73\x28\x27\x27\x2c\x20\x27\x27\x29");
}


TEST(PrecompiledFunction) {
  // Regression test for crbug.com/346207. If we have preparse data, parsing the
  // function in the presence of the debugger (and breakpoints) should still
  // succeed. The bug was that preparsing was done lazily and parsing was done
  // eagerly, so, the symbol streams didn't match.
  DebugLocalContext env;
  v8::HandleScope scope(env->GetIsolate());
  env.ExposeDebug();
  v8::Debug::SetDebugEventListener(DebugBreakInlineListener);

  v8::Local<v8::Function> break_here =
      CompileFunction(&env, "\x66\x75\x6e\x63\x74\x69\x6f\x6e\x20\x62\x72\x65\x61\x6b\x5f\x68\x65\x72\x65\x28\x29\x7b\x7d", "\x62\x72\x65\x61\x6b\x5f\x68\x65\x72\x65");
  SetBreakPoint(break_here, 0);

  const char* source =
      "\x76\x61\x72\x20\x61\x20\x3d\x20\x62\x20\x3d\x20\x63\x20\x3d\x20\x31\x3b\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\xa"
      "\x66\x75\x6e\x63\x74\x69\x6f\x6e\x20\x74\x68\x69\x73\x5f\x69\x73\x5f\x6c\x61\x7a\x79\x28\x29\x20\x7b\x20\x20\x20\x20\x20\x20\x20\xa"
      // This symbol won't appear in the preparse data.
      "\x20\x20\x76\x61\x72\x20\x61\x3b\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\xa"
      "\x7d\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\xa"
      "\x66\x75\x6e\x63\x74\x69\x6f\x6e\x20\x62\x61\x72\x28\x29\x20\x7b\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\xa"
      "\x20\x20\x72\x65\x74\x75\x72\x6e\x20\x22\x62\x61\x72\x22\x3b\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\xa"
      "\x7d\x3b\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\xa"
      "\x61\x20\x3d\x20\x62\x20\x3d\x20\x63\x20\x3d\x20\x32\x3b\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\xa"
      "\x62\x61\x72\x28\x29\x3b\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\xa";
  v8::Local<v8::Value> result = ParserCacheCompileRun(source);
  CHECK(result->IsString());
  v8::String::Utf8Value utf8(result);
  CHECK_EQ("\x62\x61\x72", *utf8);

  v8::Debug::SetDebugEventListener(NULL);
  CheckDebuggerUnloaded();
}


static void DebugBreakStackTraceListener(
    const v8::Debug::EventDetails& event_details) {
  v8::StackTrace::CurrentStackTrace(CcTest::isolate(), 10);
}


static void AddDebugBreak(const v8::FunctionCallbackInfo<v8::Value>& args) {
  v8::Debug::DebugBreak(args.GetIsolate());
}


TEST(DebugBreakStackTrace) {
  DebugLocalContext env;
  v8::HandleScope scope(env->GetIsolate());
  v8::Debug::SetDebugEventListener(DebugBreakStackTraceListener);
  v8::Handle<v8::FunctionTemplate> add_debug_break_template =
      v8::FunctionTemplate::New(env->GetIsolate(), AddDebugBreak);
  v8::Handle<v8::Function> add_debug_break =
      add_debug_break_template->GetFunction();
  env->Global()->Set(v8_str("\x61\x64\x64\x5f\x64\x65\x62\x75\x67\x5f\x62\x72\x65\x61\x6b"), add_debug_break);

  CompileRun("\x28\x66\x75\x6e\x63\x74\x69\x6f\x6e\x20\x6c\x6f\x6f\x70\x28\x29\x20\x7b"
             "\x20\x20\x66\x6f\x72\x20\x28\x76\x61\x72\x20\x6a\x20\x3d\x20\x30\x3b\x20\x6a\x20\x3c\x20\x31\x30\x30\x30\x3b\x20\x6a\x2b\x2b\x29\x20\x7b"
             "\x20\x20\x20\x20\x66\x6f\x72\x20\x28\x76\x61\x72\x20\x69\x20\x3d\x20\x30\x3b\x20\x69\x20\x3c\x20\x31\x30\x30\x30\x3b\x20\x69\x2b\x2b\x29\x20\x7b"
             "\x20\x20\x20\x20\x20\x20\x69\x66\x20\x28\x69\x20\x3d\x3d\x20\x39\x39\x39\x29\x20\x61\x64\x64\x5f\x64\x65\x62\x75\x67\x5f\x62\x72\x65\x61\x6b\x28\x29\x3b"
             "\x20\x20\x20\x20\x7d"
             "\x20\x20\x7d"
             "\x7d\x29\x28\x29");
}


v8::base::Semaphore terminate_requested_semaphore(0);
v8::base::Semaphore terminate_fired_semaphore(0);
bool terminate_already_fired = false;


static void DebugBreakTriggerTerminate(
    const v8::Debug::EventDetails& event_details) {
  if (event_details.GetEvent() != v8::Break || terminate_already_fired) return;
  terminate_requested_semaphore.Signal();
  // Wait for at most 2 seconds for the terminate request.
  CHECK(terminate_fired_semaphore.WaitFor(v8::base::TimeDelta::FromSeconds(2)));
  terminate_already_fired = true;
}


class TerminationThread : public v8::base::Thread {
 public:
  explicit TerminationThread(v8::Isolate* isolate)
      : Thread(Options("\x74\x65\x72\x6d\x69\x6e\x61\x74\x6f\x72")), isolate_(isolate) {}

  virtual void Run() {
    terminate_requested_semaphore.Wait();
    v8::V8::TerminateExecution(isolate_);
    terminate_fired_semaphore.Signal();
  }

 private:
  v8::Isolate* isolate_;
};


TEST(DebugBreakOffThreadTerminate) {
  DebugLocalContext env;
  v8::Isolate* isolate = env->GetIsolate();
  v8::HandleScope scope(isolate);
  v8::Debug::SetDebugEventListener(DebugBreakTriggerTerminate);
  TerminationThread terminator(isolate);
  terminator.Start();
  v8::TryCatch try_catch;
  v8::Debug::DebugBreak(isolate);
  CompileRun("\x77\x68\x69\x6c\x65\x20\x28\x74\x72\x75\x65\x29\x3b");
  CHECK(try_catch.HasTerminated());
}
