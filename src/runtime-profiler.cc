// Copyright 2012 the V8 project authors. All rights reserved.
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.

#include "src/v8.h"

#include "src/runtime-profiler.h"

#include "src/assembler.h"
#include "src/base/platform/platform.h"
#include "src/bootstrapper.h"
#include "src/code-stubs.h"
#include "src/compilation-cache.h"
#include "src/execution.h"
#include "src/full-codegen.h"
#include "src/global-handles.h"
#include "src/heap/mark-compact.h"
#include "src/isolate-inl.h"
#include "src/scopeinfo.h"

namespace v8 {
namespace internal {


// Number of times a function has to be seen on the stack before it is
// optimized.
static const int kProfilerTicksBeforeOptimization = 2;
// If the function optimization was disabled due to high deoptimization count,
// but the function is hot and has been seen on the stack this number of times,
// then we try to reenable optimization for this function.
static const int kProfilerTicksBeforeReenablingOptimization = 250;
// If a function does not have enough type info (according to
// FLAG_type_info_threshold), but has seen a huge number of ticks,
// optimize it as it is.
static const int kTicksWhenNotEnoughTypeInfo = 100;
// We only have one byte to store the number of ticks.
STATIC_ASSERT(kProfilerTicksBeforeOptimization < 256);
STATIC_ASSERT(kProfilerTicksBeforeReenablingOptimization < 256);
STATIC_ASSERT(kTicksWhenNotEnoughTypeInfo < 256);

// Maximum size in bytes of generate code for a function to allow OSR.
static const int kOSRCodeSizeAllowanceBase =
    100 * FullCodeGenerator::kCodeSizeMultiplier;

static const int kOSRCodeSizeAllowancePerTick =
    4 * FullCodeGenerator::kCodeSizeMultiplier;

// Maximum size in bytes of generated code for a function to be optimized
// the very first time it is seen on the stack.
static const int kMaxSizeEarlyOpt =
    5 * FullCodeGenerator::kCodeSizeMultiplier;


RuntimeProfiler::RuntimeProfiler(Isolate* isolate)
    : isolate_(isolate),
      any_ic_changed_(false) {
}


static void GetICCounts(Code* shared_code, int* ic_with_type_info_count,
                        int* ic_generic_count, int* ic_total_count,
                        int* type_info_percentage, int* generic_percentage) {
  *ic_total_count = 0;
  *ic_generic_count = 0;
  *ic_with_type_info_count = 0;
  Object* raw_info = shared_code->type_feedback_info();
  if (raw_info->IsTypeFeedbackInfo()) {
    TypeFeedbackInfo* info = TypeFeedbackInfo::cast(raw_info);
    *ic_with_type_info_count = info->ic_with_type_info_count();
    *ic_generic_count = info->ic_generic_count();
    *ic_total_count = info->ic_total_count();
  }
  if (*ic_total_count > 0) {
    *type_info_percentage = 100 * *ic_with_type_info_count / *ic_total_count;
    *generic_percentage = 100 * *ic_generic_count / *ic_total_count;
  } else {
    *type_info_percentage = 100;  // Compared against lower bound.
    *generic_percentage = 0;      // Compared against upper bound.
  }
}


void RuntimeProfiler::Optimize(JSFunction* function, const char* reason) {
  DCHECK(function->IsOptimizable());

  if (FLAG_trace_opt && function->PassesFilter(FLAG_hydrogen_filter)) {
    PrintF("\x5b\x6d\x61\x72\x6b\x69\x6e\x67\x20");
    function->ShortPrint();
    PrintF("\x20\x66\x6f\x72\x20\x72\x65\x63\x6f\x6d\x70\x69\x6c\x61\x74\x69\x6f\x6e\x2c\x20\x72\x65\x61\x73\x6f\x6e\x3a\x20\x6c\xa2", reason);
    if (FLAG_type_info_threshold > 0) {
      int typeinfo, generic, total, type_percentage, generic_percentage;
      GetICCounts(function->shared()->code(), &typeinfo, &generic, &total,
                  &type_percentage, &generic_percentage);
      PrintF("\x2c\x20\x49\x43\x73\x20\x77\x69\x74\x68\x20\x74\x79\x70\x65\x69\x6e\x66\x6f\x3a\x20\x6c\x84\x2f\x6c\x84\x20\x28\x6c\x84\x25\x25\x29", typeinfo, total,
             type_percentage);
      PrintF("\x2c\x20\x67\x65\x6e\x65\x72\x69\x63\x20\x49\x43\x73\x3a\x20\x6c\x84\x2f\x6c\x84\x20\x28\x6c\x84\x25\x25\x29", generic, total, generic_percentage);
    }
    PrintF("\x5d\xa");
  }


  if (isolate_->concurrent_recompilation_enabled() &&
      !isolate_->bootstrapper()->IsActive()) {
    if (isolate_->concurrent_osr_enabled() &&
        isolate_->optimizing_compiler_thread()->IsQueuedForOSR(function)) {
      // Do not attempt regular recompilation if we already queued this for OSR.
      // TODO(yangguo): This is necessary so that we don't install optimized
      // code on a function that is already optimized, since OSR and regular
      // recompilation race.  This goes away as soon as OSR becomes one-shot.
      return;
    }
    DCHECK(!function->IsInOptimizationQueue());
    function->MarkForConcurrentOptimization();
  } else {
    // The next call to the function will trigger optimization.
    function->MarkForOptimization();
  }
}


void RuntimeProfiler::AttemptOnStackReplacement(JSFunction* function,
                                                int loop_nesting_levels) {
  SharedFunctionInfo* shared = function->shared();
  // See AlwaysFullCompiler (in compiler.cc) comment on why we need
  // Debug::has_break_points().
  if (!FLAG_use_osr ||
      isolate_->DebuggerHasBreakPoints() ||
      function->IsBuiltin()) {
    return;
  }

  // If the code is not optimizable, don't try OSR.
  if (!shared->code()->optimizable()) return;

  // We are not prepared to do OSR for a function that already has an
  // allocated arguments object.  The optimized code would bypass it for
  // arguments accesses, which is unsound.  Don't try OSR.
  if (shared->uses_arguments()) return;

  // We're using on-stack replacement: patch the unoptimized code so that
  // any back edge in any unoptimized frame will trigger on-stack
  // replacement for that frame.
  if (FLAG_trace_osr) {
    PrintF("\x5b\x4f\x53\x52\x20\x2d\x20\x70\x61\x74\x63\x68\x69\x6e\x67\x20\x62\x61\x63\x6b\x20\x65\x64\x67\x65\x73\x20\x69\x6e\x20");
    function->PrintName();
    PrintF("\x5d\xa");
  }

  for (int i = 0; i < loop_nesting_levels; i++) {
    BackEdgeTable::Patch(isolate_, shared->code());
  }
}


void RuntimeProfiler::OptimizeNow() {
  HandleScope scope(isolate_);

  if (isolate_->DebuggerHasBreakPoints()) return;

  DisallowHeapAllocation no_gc;

  // Run through the JavaScript frames and collect them. If we already
  // have a sample of the function, we mark it for optimizations
  // (eagerly or lazily).
  int frame_count = 0;
  int frame_count_limit = FLAG_frame_count;
  for (JavaScriptFrameIterator it(isolate_);
       frame_count++ < frame_count_limit && !it.done();
       it.Advance()) {
    JavaScriptFrame* frame = it.frame();
    JSFunction* function = frame->function();

    SharedFunctionInfo* shared = function->shared();
    Code* shared_code = shared->code();

    List<JSFunction*> functions(4);
    frame->GetFunctions(&functions);
    for (int i = functions.length(); --i >= 0; ) {
      SharedFunctionInfo* shared_function_info = functions[i]->shared();
      int ticks = shared_function_info->profiler_ticks();
      if (ticks < Smi::kMaxValue) {
        shared_function_info->set_profiler_ticks(ticks + 1);
      }
    }

    if (shared_code->kind() != Code::FUNCTION) continue;
    if (function->IsInOptimizationQueue()) continue;

    if (FLAG_always_osr) {
      AttemptOnStackReplacement(function, Code::kMaxLoopNestingMarker);
      // Fall through and do a normal optimized compile as well.
    } else if (!frame->is_optimized() &&
        (function->IsMarkedForOptimization() ||
         function->IsMarkedForConcurrentOptimization() ||
         function->IsOptimized())) {
      // Attempt OSR if we are still running unoptimized code even though the
      // the function has long been marked or even already been optimized.
      int ticks = shared_code->profiler_ticks();
      int allowance = kOSRCodeSizeAllowanceBase +
                      ticks * kOSRCodeSizeAllowancePerTick;
      if (shared_code->CodeSize() > allowance) {
        if (ticks < 255) shared_code->set_profiler_ticks(ticks + 1);
      } else {
        AttemptOnStackReplacement(function);
      }
      continue;
    }

    // Only record top-level code on top of the execution stack and
    // avoid optimizing excessively large scripts since top-level code
    // will be executed only once.
    const int kMaxToplevelSourceSize = 10 * 1024;
    if (shared->is_toplevel() &&
        (frame_count > 1 || shared->SourceSize() > kMaxToplevelSourceSize)) {
      continue;
    }

    // Do not record non-optimizable functions.
    if (shared->optimization_disabled()) {
      if (shared->deopt_count() >= FLAG_max_opt_count) {
        // If optimization was disabled due to many deoptimizations,
        // then check if the function is hot and try to reenable optimization.
        int ticks = shared_code->profiler_ticks();
        if (ticks >= kProfilerTicksBeforeReenablingOptimization) {
          shared_code->set_profiler_ticks(0);
          shared->TryReenableOptimization();
        } else {
          shared_code->set_profiler_ticks(ticks + 1);
        }
      }
      continue;
    }
    if (!function->IsOptimizable()) continue;

    int ticks = shared_code->profiler_ticks();

    if (ticks >= kProfilerTicksBeforeOptimization) {
      int typeinfo, generic, total, type_percentage, generic_percentage;
      GetICCounts(shared_code, &typeinfo, &generic, &total, &type_percentage,
                  &generic_percentage);
      if (type_percentage >= FLAG_type_info_threshold &&
          generic_percentage <= FLAG_generic_ic_threshold) {
        // If this particular function hasn't had any ICs patched for enough
        // ticks, optimize it now.
        Optimize(function, "\x68\x6f\x74\x20\x61\x6e\x64\x20\x73\x74\x61\x62\x6c\x65");
      } else if (ticks >= kTicksWhenNotEnoughTypeInfo) {
        Optimize(function, "\x6e\x6f\x74\x20\x6d\x75\x63\x68\x20\x74\x79\x70\x65\x20\x69\x6e\x66\x6f\x20\x62\x75\x74\x20\x76\x65\x72\x79\x20\x68\x6f\x74");
      } else {
        shared_code->set_profiler_ticks(ticks + 1);
        if (FLAG_trace_opt_verbose) {
          PrintF("\x5b\x6e\x6f\x74\x20\x79\x65\x74\x20\x6f\x70\x74\x69\x6d\x69\x7a\x69\x6e\x67\x20");
          function->PrintName();
          PrintF("\x2c\x20\x6e\x6f\x74\x20\x65\x6e\x6f\x75\x67\x68\x20\x74\x79\x70\x65\x20\x69\x6e\x66\x6f\x3a\x20\x6c\x84\x2f\x6c\x84\x20\x28\x6c\x84\x25\x25\x29\x5d\xa", typeinfo, total,
                 type_percentage);
        }
      }
    } else if (!any_ic_changed_ &&
               shared_code->instruction_size() < kMaxSizeEarlyOpt) {
      // If no IC was patched since the last tick and this function is very
      // small, optimistically optimize it now.
      int typeinfo, generic, total, type_percentage, generic_percentage;
      GetICCounts(shared_code, &typeinfo, &generic, &total, &type_percentage,
                  &generic_percentage);
      if (type_percentage >= FLAG_type_info_threshold &&
          generic_percentage <= FLAG_generic_ic_threshold) {
        Optimize(function, "\x73\x6d\x61\x6c\x6c\x20\x66\x75\x6e\x63\x74\x69\x6f\x6e");
      } else {
        shared_code->set_profiler_ticks(ticks + 1);
      }
    } else {
      shared_code->set_profiler_ticks(ticks + 1);
    }
  }
  any_ic_changed_ = false;
}


} }  // namespace v8::internal
