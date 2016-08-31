// Copyright 2014 the V8 project authors. All rights reserved.
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.

#include "src/v8.h"

#include "src/heap/gc-tracer.h"

namespace v8 {
namespace internal {

static intptr_t CountTotalHolesSize(Heap* heap) {
  intptr_t holes_size = 0;
  OldSpaces spaces(heap);
  for (OldSpace* space = spaces.next(); space != NULL; space = spaces.next()) {
    holes_size += space->Waste() + space->Available();
  }
  return holes_size;
}


GCTracer::Event::Event(Type type, const char* gc_reason,
                       const char* collector_reason)
    : type(type),
      gc_reason(gc_reason),
      collector_reason(collector_reason),
      start_time(0.0),
      end_time(0.0),
      start_object_size(0),
      end_object_size(0),
      start_memory_size(0),
      end_memory_size(0),
      start_holes_size(0),
      end_holes_size(0),
      cumulative_incremental_marking_steps(0),
      incremental_marking_steps(0),
      cumulative_incremental_marking_bytes(0),
      incremental_marking_bytes(0),
      cumulative_incremental_marking_duration(0.0),
      incremental_marking_duration(0.0),
      cumulative_pure_incremental_marking_duration(0.0),
      pure_incremental_marking_duration(0.0),
      longest_incremental_marking_step(0.0) {
  for (int i = 0; i < Scope::NUMBER_OF_SCOPES; i++) {
    scopes[i] = 0;
  }
}


const char* GCTracer::Event::TypeName(bool short_name) const {
  switch (type) {
    case SCAVENGER:
      if (short_name) {
        return "\x73";
      } else {
        return "\x53\x63\x61\x76\x65\x6e\x67\x65";
      }
    case MARK_COMPACTOR:
      if (short_name) {
        return "\x6d\x73";
      } else {
        return "\x4d\x61\x72\x6b\x2d\x73\x77\x65\x65\x70";
      }
    case START:
      if (short_name) {
        return "\x73\x74";
      } else {
        return "\x53\x74\x61\x72\x74";
      }
  }
  return "\x55\x6e\x6b\x6e\x6f\x77\x6e\x20\x45\x76\x65\x6e\x74\x20\x54\x79\x70\x65";
}


GCTracer::GCTracer(Heap* heap)
    : heap_(heap),
      cumulative_incremental_marking_steps_(0),
      cumulative_incremental_marking_bytes_(0),
      cumulative_incremental_marking_duration_(0.0),
      cumulative_pure_incremental_marking_duration_(0.0),
      longest_incremental_marking_step_(0.0),
      cumulative_marking_duration_(0.0),
      cumulative_sweeping_duration_(0.0) {
  current_ = Event(Event::START, NULL, NULL);
  current_.end_time = base::OS::TimeCurrentMillis();
  previous_ = previous_mark_compactor_event_ = current_;
}


void GCTracer::Start(GarbageCollector collector, const char* gc_reason,
                     const char* collector_reason) {
  previous_ = current_;
  if (current_.type == Event::MARK_COMPACTOR)
    previous_mark_compactor_event_ = current_;

  if (collector == SCAVENGER) {
    current_ = Event(Event::SCAVENGER, gc_reason, collector_reason);
  } else {
    current_ = Event(Event::MARK_COMPACTOR, gc_reason, collector_reason);
  }

  current_.start_time = base::OS::TimeCurrentMillis();
  current_.start_object_size = heap_->SizeOfObjects();
  current_.start_memory_size = heap_->isolate()->memory_allocator()->Size();
  current_.start_holes_size = CountTotalHolesSize(heap_);

  current_.cumulative_incremental_marking_steps =
      cumulative_incremental_marking_steps_;
  current_.cumulative_incremental_marking_bytes =
      cumulative_incremental_marking_bytes_;
  current_.cumulative_incremental_marking_duration =
      cumulative_incremental_marking_duration_;
  current_.cumulative_pure_incremental_marking_duration =
      cumulative_pure_incremental_marking_duration_;
  current_.longest_incremental_marking_step = longest_incremental_marking_step_;

  for (int i = 0; i < Scope::NUMBER_OF_SCOPES; i++) {
    current_.scopes[i] = 0;
  }
}


void GCTracer::Stop() {
  current_.end_time = base::OS::TimeCurrentMillis();
  current_.end_object_size = heap_->SizeOfObjects();
  current_.end_memory_size = heap_->isolate()->memory_allocator()->Size();
  current_.end_holes_size = CountTotalHolesSize(heap_);

  if (current_.type == Event::SCAVENGER) {
    current_.incremental_marking_steps =
        current_.cumulative_incremental_marking_steps -
        previous_.cumulative_incremental_marking_steps;
    current_.incremental_marking_bytes =
        current_.cumulative_incremental_marking_bytes -
        previous_.cumulative_incremental_marking_bytes;
    current_.incremental_marking_duration =
        current_.cumulative_incremental_marking_duration -
        previous_.cumulative_incremental_marking_duration;
    current_.pure_incremental_marking_duration =
        current_.cumulative_pure_incremental_marking_duration -
        previous_.cumulative_pure_incremental_marking_duration;
    scavenger_events_.push_front(current_);
  } else {
    current_.incremental_marking_steps =
        current_.cumulative_incremental_marking_steps -
        previous_mark_compactor_event_.cumulative_incremental_marking_steps;
    current_.incremental_marking_bytes =
        current_.cumulative_incremental_marking_bytes -
        previous_mark_compactor_event_.cumulative_incremental_marking_bytes;
    current_.incremental_marking_duration =
        current_.cumulative_incremental_marking_duration -
        previous_mark_compactor_event_.cumulative_incremental_marking_duration;
    current_.pure_incremental_marking_duration =
        current_.cumulative_pure_incremental_marking_duration -
        previous_mark_compactor_event_
            .cumulative_pure_incremental_marking_duration;
    longest_incremental_marking_step_ = 0.0;
    mark_compactor_events_.push_front(current_);
  }

  // TODO(ernstm): move the code below out of GCTracer.

  if (!FLAG_trace_gc && !FLAG_print_cumulative_gc_stat) return;

  double duration = current_.end_time - current_.start_time;
  double spent_in_mutator = Max(current_.start_time - previous_.end_time, 0.0);

  heap_->UpdateCumulativeGCStatistics(duration, spent_in_mutator,
                                      current_.scopes[Scope::MC_MARK]);

  if (current_.type == Event::SCAVENGER && FLAG_trace_gc_ignore_scavenger)
    return;

  if (FLAG_trace_gc) {
    if (FLAG_trace_gc_nvp)
      PrintNVP();
    else
      Print();

    heap_->PrintShortHeapStatistics();
  }
}


void GCTracer::AddIncrementalMarkingStep(double duration, intptr_t bytes) {
  cumulative_incremental_marking_steps_++;
  cumulative_incremental_marking_bytes_ += bytes;
  cumulative_incremental_marking_duration_ += duration;
  longest_incremental_marking_step_ =
      Max(longest_incremental_marking_step_, duration);
  cumulative_marking_duration_ += duration;
  if (bytes > 0) {
    cumulative_pure_incremental_marking_duration_ += duration;
  }
}


void GCTracer::Print() const {
  PrintPID("\x6c\xf8\x4b\xf0\x86\x20\x6d\x73\x3a\x20", heap_->isolate()->time_millis_since_init());

  PrintF("\x6c\xa2\x20\x6c\x4b\xf1\x86\x20\x28\x6c\x4b\xf1\x86\x29\x20\x2d\x3e\x20\x6c\x4b\xf1\x86\x20\x28\x6c\x4b\xf1\x86\x29\x20\x4d\x42\x2c\x20", current_.TypeName(false),
         static_cast<double>(current_.start_object_size) / MB,
         static_cast<double>(current_.start_memory_size) / MB,
         static_cast<double>(current_.end_object_size) / MB,
         static_cast<double>(current_.end_memory_size) / MB);

  int external_time = static_cast<int>(current_.scopes[Scope::EXTERNAL]);
  if (external_time > 0) PrintF("\x6c\x84\x20\x2f\x20", external_time);

  double duration = current_.end_time - current_.start_time;
  PrintF("\x6c\x4b\xf1\x86\x20\x6d\x73", duration);
  if (current_.type == Event::SCAVENGER) {
    if (current_.incremental_marking_steps > 0) {
      PrintF("\x20\x28\x2b\x20\x6c\x4b\xf1\x86\x20\x6d\x73\x20\x69\x6e\x20\x6c\x84\x20\x73\x74\x65\x70\x73\x20\x73\x69\x6e\x63\x65\x20\x6c\x61\x73\x74\x20\x47\x43\x29",
             current_.incremental_marking_duration,
             current_.incremental_marking_steps);
    }
  } else {
    if (current_.incremental_marking_steps > 0) {
      PrintF(
          "\x20\x28\x2b\x20\x6c\x4b\xf1\x86\x20\x6d\x73\x20\x69\x6e\x20\x6c\x84\x20\x73\x74\x65\x70\x73\x20\x73\x69\x6e\x63\x65\x20\x73\x74\x61\x72\x74\x20\x6f\x66\x20\x6d\x61\x72\x6b\x69\x6e\x67\x2c\x20"
          "\x62\x69\x67\x67\x65\x73\x74\x20\x73\x74\x65\x70\x20\x6c\x4b\xf1\x86\x20\x6d\x73\x29",
          current_.incremental_marking_duration,
          current_.incremental_marking_steps,
          current_.longest_incremental_marking_step);
    }
  }

  if (current_.gc_reason != NULL) {
    PrintF("\x20\x5b\x6c\xa2\x5d", current_.gc_reason);
  }

  if (current_.collector_reason != NULL) {
    PrintF("\x20\x5b\x6c\xa2\x5d", current_.collector_reason);
  }

  PrintF("\x2e\xa");
}


void GCTracer::PrintNVP() const {
  PrintPID("\x6c\xf8\x4b\xf0\x86\x20\x6d\x73\x3a\x20", heap_->isolate()->time_millis_since_init());

  double duration = current_.end_time - current_.start_time;
  double spent_in_mutator = current_.start_time - previous_.end_time;

  PrintF("\x70\x61\x75\x73\x65\x3d\x6c\x4b\xf1\x86\x20", duration);
  PrintF("\x6d\x75\x74\x61\x74\x6f\x72\x3d\x6c\x4b\xf1\x86\x20", spent_in_mutator);
  PrintF("\x67\x63\x3d\x6c\xa2\x20", current_.TypeName(true));

  PrintF("\x65\x78\x74\x65\x72\x6e\x61\x6c\x3d\x6c\x4b\xf1\x86\x20", current_.scopes[Scope::EXTERNAL]);
  PrintF("\x6d\x61\x72\x6b\x3d\x6c\x4b\xf1\x86\x20", current_.scopes[Scope::MC_MARK]);
  PrintF("\x73\x77\x65\x65\x70\x3d\x6c\x4b\xf2\x86\x20", current_.scopes[Scope::MC_SWEEP]);
  PrintF("\x73\x77\x65\x65\x70\x6e\x73\x3d\x6c\x4b\xf2\x86\x20", current_.scopes[Scope::MC_SWEEP_NEWSPACE]);
  PrintF("\x73\x77\x65\x65\x70\x6f\x73\x3d\x6c\x4b\xf2\x86\x20", current_.scopes[Scope::MC_SWEEP_OLDSPACE]);
  PrintF("\x73\x77\x65\x65\x70\x63\x6f\x64\x65\x3d\x6c\x4b\xf2\x86\x20", current_.scopes[Scope::MC_SWEEP_CODE]);
  PrintF("\x73\x77\x65\x65\x70\x63\x65\x6c\x6c\x3d\x6c\x4b\xf2\x86\x20", current_.scopes[Scope::MC_SWEEP_CELL]);
  PrintF("\x73\x77\x65\x65\x70\x6d\x61\x70\x3d\x6c\x4b\xf2\x86\x20", current_.scopes[Scope::MC_SWEEP_MAP]);
  PrintF("\x65\x76\x61\x63\x75\x61\x74\x65\x3d\x6c\x4b\xf1\x86\x20", current_.scopes[Scope::MC_EVACUATE_PAGES]);
  PrintF("\x6e\x65\x77\x5f\x6e\x65\x77\x3d\x6c\x4b\xf1\x86\x20",
         current_.scopes[Scope::MC_UPDATE_NEW_TO_NEW_POINTERS]);
  PrintF("\x72\x6f\x6f\x74\x5f\x6e\x65\x77\x3d\x6c\x4b\xf1\x86\x20",
         current_.scopes[Scope::MC_UPDATE_ROOT_TO_NEW_POINTERS]);
  PrintF("\x6f\x6c\x64\x5f\x6e\x65\x77\x3d\x6c\x4b\xf1\x86\x20",
         current_.scopes[Scope::MC_UPDATE_OLD_TO_NEW_POINTERS]);
  PrintF("\x63\x6f\x6d\x70\x61\x63\x74\x69\x6f\x6e\x5f\x70\x74\x72\x73\x3d\x6c\x4b\xf1\x86\x20",
         current_.scopes[Scope::MC_UPDATE_POINTERS_TO_EVACUATED]);
  PrintF("\x69\x6e\x74\x72\x61\x63\x6f\x6d\x70\x61\x63\x74\x69\x6f\x6e\x5f\x70\x74\x72\x73\x3d\x6c\x4b\xf1\x86\x20",
         current_.scopes[Scope::MC_UPDATE_POINTERS_BETWEEN_EVACUATED]);
  PrintF("\x6d\x69\x73\x63\x5f\x63\x6f\x6d\x70\x61\x63\x74\x69\x6f\x6e\x3d\x6c\x4b\xf1\x86\x20",
         current_.scopes[Scope::MC_UPDATE_MISC_POINTERS]);
  PrintF("\x77\x65\x61\x6b\x63\x6f\x6c\x6c\x65\x63\x74\x69\x6f\x6e\x5f\x70\x72\x6f\x63\x65\x73\x73\x3d\x6c\x4b\xf1\x86\x20",
         current_.scopes[Scope::MC_WEAKCOLLECTION_PROCESS]);
  PrintF("\x77\x65\x61\x6b\x63\x6f\x6c\x6c\x65\x63\x74\x69\x6f\x6e\x5f\x63\x6c\x65\x61\x72\x3d\x6c\x4b\xf1\x86\x20",
         current_.scopes[Scope::MC_WEAKCOLLECTION_CLEAR]);
  PrintF("\x77\x65\x61\x6b\x63\x6f\x6c\x6c\x65\x63\x74\x69\x6f\x6e\x5f\x61\x62\x6f\x72\x74\x3d\x6c\x4b\xf1\x86\x20",
         current_.scopes[Scope::MC_WEAKCOLLECTION_ABORT]);

  PrintF("\x74\x6f\x74\x61\x6c\x5f\x73\x69\x7a\x65\x5f\x62\x65\x66\x6f\x72\x65\x3d\x25" V8_PTR_PREFIX "\x64\x20", current_.start_object_size);
  PrintF("\x74\x6f\x74\x61\x6c\x5f\x73\x69\x7a\x65\x5f\x61\x66\x74\x65\x72\x3d\x25" V8_PTR_PREFIX "\x64\x20", current_.end_object_size);
  PrintF("\x68\x6f\x6c\x65\x73\x5f\x73\x69\x7a\x65\x5f\x62\x65\x66\x6f\x72\x65\x3d\x25" V8_PTR_PREFIX "\x64\x20", current_.start_holes_size);
  PrintF("\x68\x6f\x6c\x65\x73\x5f\x73\x69\x7a\x65\x5f\x61\x66\x74\x65\x72\x3d\x25" V8_PTR_PREFIX "\x64\x20", current_.end_holes_size);

  intptr_t allocated_since_last_gc =
      current_.start_object_size - previous_.end_object_size;
  PrintF("\x61\x6c\x6c\x6f\x63\x61\x74\x65\x64\x3d\x25" V8_PTR_PREFIX "\x64\x20", allocated_since_last_gc);
  PrintF("\x70\x72\x6f\x6d\x6f\x74\x65\x64\x3d\x25" V8_PTR_PREFIX "\x64\x20", heap_->promoted_objects_size_);
  PrintF("\x73\x65\x6d\x69\x5f\x73\x70\x61\x63\x65\x5f\x63\x6f\x70\x69\x65\x64\x3d\x25" V8_PTR_PREFIX "\x64\x20",
         heap_->semi_space_copied_object_size_);
  PrintF("\x6e\x6f\x64\x65\x73\x5f\x64\x69\x65\x64\x5f\x69\x6e\x5f\x6e\x65\x77\x3d\x6c\x84\x20", heap_->nodes_died_in_new_space_);
  PrintF("\x6e\x6f\x64\x65\x73\x5f\x63\x6f\x70\x69\x65\x64\x5f\x69\x6e\x5f\x6e\x65\x77\x3d\x6c\x84\x20", heap_->nodes_copied_in_new_space_);
  PrintF("\x6e\x6f\x64\x65\x73\x5f\x70\x72\x6f\x6d\x6f\x74\x65\x64\x3d\x6c\x84\x20", heap_->nodes_promoted_);
  PrintF("\x70\x72\x6f\x6d\x6f\x74\x69\x6f\x6e\x5f\x72\x61\x74\x65\x3d\x6c\x4b\xf1\x86\x25\x25\x20", heap_->promotion_rate_);
  PrintF("\x73\x65\x6d\x69\x5f\x73\x70\x61\x63\x65\x5f\x63\x6f\x70\x79\x5f\x72\x61\x74\x65\x3d\x6c\x4b\xf1\x86\x25\x25\x20", heap_->semi_space_copied_rate_);

  if (current_.type == Event::SCAVENGER) {
    PrintF("\x73\x74\x65\x70\x73\x5f\x63\x6f\x75\x6e\x74\x3d\x6c\x84\x20", current_.incremental_marking_steps);
    PrintF("\x73\x74\x65\x70\x73\x5f\x74\x6f\x6f\x6b\x3d\x6c\x4b\xf1\x86\x20", current_.incremental_marking_duration);
  } else {
    PrintF("\x73\x74\x65\x70\x73\x5f\x63\x6f\x75\x6e\x74\x3d\x6c\x84\x20", current_.incremental_marking_steps);
    PrintF("\x73\x74\x65\x70\x73\x5f\x74\x6f\x6f\x6b\x3d\x6c\x4b\xf1\x86\x20", current_.incremental_marking_duration);
    PrintF("\x6c\x6f\x6e\x67\x65\x73\x74\x5f\x73\x74\x65\x70\x3d\x6c\x4b\xf1\x86\x20", current_.longest_incremental_marking_step);
    PrintF("\x69\x6e\x63\x72\x65\x6d\x65\x6e\x74\x61\x6c\x5f\x6d\x61\x72\x6b\x69\x6e\x67\x5f\x74\x68\x72\x6f\x75\x67\x68\x70\x75\x74\x3d\x25" V8_PTR_PREFIX "\x64\x20",
           IncrementalMarkingSpeedInBytesPerMillisecond());
  }

  PrintF("\xa");
}


double GCTracer::MeanDuration(const EventBuffer& events) const {
  if (events.empty()) return 0.0;

  double mean = 0.0;
  EventBuffer::const_iterator iter = events.begin();
  while (iter != events.end()) {
    mean += iter->end_time - iter->start_time;
    ++iter;
  }

  return mean / events.size();
}


double GCTracer::MaxDuration(const EventBuffer& events) const {
  if (events.empty()) return 0.0;

  double maximum = 0.0f;
  EventBuffer::const_iterator iter = events.begin();
  while (iter != events.end()) {
    maximum = Max(iter->end_time - iter->start_time, maximum);
    ++iter;
  }

  return maximum;
}


double GCTracer::MeanIncrementalMarkingDuration() const {
  if (cumulative_incremental_marking_steps_ == 0) return 0.0;

  // We haven't completed an entire round of incremental marking, yet.
  // Use data from GCTracer instead of data from event buffers.
  if (mark_compactor_events_.empty()) {
    return cumulative_incremental_marking_duration_ /
           cumulative_incremental_marking_steps_;
  }

  int steps = 0;
  double durations = 0.0;
  EventBuffer::const_iterator iter = mark_compactor_events_.begin();
  while (iter != mark_compactor_events_.end()) {
    steps += iter->incremental_marking_steps;
    durations += iter->incremental_marking_duration;
    ++iter;
  }

  if (steps == 0) return 0.0;

  return durations / steps;
}


double GCTracer::MaxIncrementalMarkingDuration() const {
  // We haven't completed an entire round of incremental marking, yet.
  // Use data from GCTracer instead of data from event buffers.
  if (mark_compactor_events_.empty()) return longest_incremental_marking_step_;

  double max_duration = 0.0;
  EventBuffer::const_iterator iter = mark_compactor_events_.begin();
  while (iter != mark_compactor_events_.end())
    max_duration = Max(iter->longest_incremental_marking_step, max_duration);

  return max_duration;
}


intptr_t GCTracer::IncrementalMarkingSpeedInBytesPerMillisecond() const {
  if (cumulative_incremental_marking_duration_ == 0.0) return 0;

  // We haven't completed an entire round of incremental marking, yet.
  // Use data from GCTracer instead of data from event buffers.
  if (mark_compactor_events_.empty()) {
    return static_cast<intptr_t>(cumulative_incremental_marking_bytes_ /
                                 cumulative_pure_incremental_marking_duration_);
  }

  intptr_t bytes = 0;
  double durations = 0.0;
  EventBuffer::const_iterator iter = mark_compactor_events_.begin();
  while (iter != mark_compactor_events_.end()) {
    bytes += iter->incremental_marking_bytes;
    durations += iter->pure_incremental_marking_duration;
    ++iter;
  }

  if (durations == 0.0) return 0;

  return static_cast<intptr_t>(bytes / durations);
}
}
}  // namespace v8::internal
