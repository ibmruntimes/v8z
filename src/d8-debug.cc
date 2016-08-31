// Copyright 2012 the V8 project authors. All rights reserved.
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.

#include "src/d8.h"
#include "src/d8-debug.h"

namespace v8 {

void PrintPrompt(bool is_running) {
  const char* prompt = is_running? "\x3e\x20" : "\x64\x62\x67\x3e\x20";
  printf("\x6c\xa2", prompt);
  fflush(stdout);
}


void HandleDebugEvent(const Debug::EventDetails& event_details) {
  // TODO(svenpanne) There should be a way to retrieve this in the callback.
  Isolate* isolate = Isolate::GetCurrent();
  HandleScope scope(isolate);

  DebugEvent event = event_details.GetEvent();
  // Check for handled event.
  if (event != Break && event != Exception && event != AfterCompile) {
    return;
  }

  TryCatch try_catch;

  // Get the toJSONProtocol function on the event and get the JSON format.
  Local<String> to_json_fun_name =
      String::NewFromUtf8(isolate, "\x74\x6f\x4a\x53\x4f\x4e\x50\x72\x6f\x74\x6f\x63\x6f\x6c");
  Handle<Object> event_data = event_details.GetEventData();
  Local<Function> to_json_fun =
      Local<Function>::Cast(event_data->Get(to_json_fun_name));
  Local<Value> event_json = to_json_fun->Call(event_data, 0, NULL);
  if (try_catch.HasCaught()) {
    Shell::ReportException(isolate, &try_catch);
    return;
  }

  // Print the event details.
  Handle<Object> details =
      Shell::DebugMessageDetails(isolate, Handle<String>::Cast(event_json));
  if (try_catch.HasCaught()) {
    Shell::ReportException(isolate, &try_catch);
    return;
  }
  String::Utf8Value str(details->Get(String::NewFromUtf8(isolate, "\x74\x65\x78\x74")));
  if (str.length() == 0) {
    // Empty string is used to signal not to process this event.
    return;
  }
  printf("\x6c\xa2\xa", *str);

  // Get the debug command processor.
  Local<String> fun_name =
      String::NewFromUtf8(isolate, "\x64\x65\x62\x75\x67\x43\x6f\x6d\x6d\x61\x6e\x64\x50\x72\x6f\x63\x65\x73\x73\x6f\x72");
  Handle<Object> exec_state = event_details.GetExecutionState();
  Local<Function> fun = Local<Function>::Cast(exec_state->Get(fun_name));
  Local<Object> cmd_processor =
      Local<Object>::Cast(fun->Call(exec_state, 0, NULL));
  if (try_catch.HasCaught()) {
    Shell::ReportException(isolate, &try_catch);
    return;
  }

  static const int kBufferSize = 256;
  bool running = false;
  while (!running) {
    char command[kBufferSize];
    PrintPrompt(running);
    char* str = fgets(command, kBufferSize, stdin);
    if (str == NULL) break;

    // Ignore empty commands.
    if (strlen(command) == 0) continue;

    TryCatch try_catch;

    // Convert the debugger command to a JSON debugger request.
    Handle<Value> request = Shell::DebugCommandToJSONRequest(
        isolate, String::NewFromUtf8(isolate, command));
    if (try_catch.HasCaught()) {
      Shell::ReportException(isolate, &try_catch);
      continue;
    }

    // If undefined is returned the command was handled internally and there is
    // no JSON to send.
    if (request->IsUndefined()) {
      continue;
    }

    Handle<String> fun_name;
    Handle<Function> fun;
    // All the functions used below take one argument.
    static const int kArgc = 1;
    Handle<Value> args[kArgc];

    // Invoke the JavaScript to convert the debug command line to a JSON
    // request, invoke the JSON request and convert the JSON respose to a text
    // representation.
    fun_name = String::NewFromUtf8(isolate, "\x70\x72\x6f\x63\x65\x73\x73\x44\x65\x62\x75\x67\x52\x65\x71\x75\x65\x73\x74");
    fun = Handle<Function>::Cast(cmd_processor->Get(fun_name));
    args[0] = request;
    Handle<Value> response_val = fun->Call(cmd_processor, kArgc, args);
    if (try_catch.HasCaught()) {
      Shell::ReportException(isolate, &try_catch);
      continue;
    }
    Handle<String> response = Handle<String>::Cast(response_val);

    // Convert the debugger response into text details and the running state.
    Handle<Object> response_details =
        Shell::DebugMessageDetails(isolate, response);
    if (try_catch.HasCaught()) {
      Shell::ReportException(isolate, &try_catch);
      continue;
    }
    String::Utf8Value text_str(
        response_details->Get(String::NewFromUtf8(isolate, "\x74\x65\x78\x74")));
    if (text_str.length() > 0) {
      printf("\x6c\xa2\xa", *text_str);
    }
    running = response_details->Get(String::NewFromUtf8(isolate, "\x72\x75\x6e\x6e\x69\x6e\x67"))
                  ->ToBoolean()
                  ->Value();
  }
}

}  // namespace v8
