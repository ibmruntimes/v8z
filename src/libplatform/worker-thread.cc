// Copyright 2013 the V8 project authors. All rights reserved.
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.

#include "src/libplatform/worker-thread.h"

#include "include/v8-platform.h"
#include "src/libplatform/task-queue.h"

namespace v8 {
namespace platform {

WorkerThread::WorkerThread(TaskQueue* queue)
    : Thread(Options("\x56\x38\x20\x57\x6f\x72\x6b\x65\x72\x54\x68\x72\x65\x61\x64")), queue_(queue) {
  Start();
}


WorkerThread::~WorkerThread() {
  Join();
}


void WorkerThread::Run() {
  while (Task* task = queue_->GetNext()) {
    task->Run();
    delete task;
  }
}

} }  // namespace v8::platform
