// Copyright 2013 the V8 project authors. All rights reserved.
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.

#include "src/allocation-site-scopes.h"

namespace v8 {
namespace internal {


Handle<AllocationSite> AllocationSiteCreationContext::EnterNewScope() {
  Handle<AllocationSite> scope_site;
  if (top().is_null()) {
    // We are creating the top level AllocationSite as opposed to a nested
    // AllocationSite.
    InitializeTraversal(isolate()->factory()->NewAllocationSite());
    scope_site = Handle<AllocationSite>(*top(), isolate());
    if (FLAG_trace_creation_allocation_sites) {
      PrintF("\x2a\x2a\x2a\x20\x43\x72\x65\x61\x74\x69\x6e\x67\x20\x74\x6f\x70\x20\x6c\x65\x76\x65\x6c\x20\x41\x6c\x6c\x6f\x63\x61\x74\x69\x6f\x6e\x53\x69\x74\x65\x20\x6c\x97\xa",
             static_cast<void*>(*scope_site));
    }
  } else {
    DCHECK(!current().is_null());
    scope_site = isolate()->factory()->NewAllocationSite();
    if (FLAG_trace_creation_allocation_sites) {
      PrintF("\x43\x72\x65\x61\x74\x69\x6e\x67\x20\x6e\x65\x73\x74\x65\x64\x20\x73\x69\x74\x65\x20\x28\x74\x6f\x70\x2c\x20\x63\x75\x72\x72\x65\x6e\x74\x2c\x20\x6e\x65\x77\x29\x20\x28\x6c\x97\x2c\x20\x6c\x97\x2c\x20\x6c\x97\x29\xa",
             static_cast<void*>(*top()),
             static_cast<void*>(*current()),
             static_cast<void*>(*scope_site));
    }
    current()->set_nested_site(*scope_site);
    update_current_site(*scope_site);
  }
  DCHECK(!scope_site.is_null());
  return scope_site;
}


void AllocationSiteCreationContext::ExitScope(
    Handle<AllocationSite> scope_site,
    Handle<JSObject> object) {
  if (!object.is_null()) {
    bool top_level = !scope_site.is_null() &&
        top().is_identical_to(scope_site);

    scope_site->set_transition_info(*object);
    if (FLAG_trace_creation_allocation_sites) {
      if (top_level) {
        PrintF("\x2a\x2a\x2a\x20\x53\x65\x74\x74\x69\x6e\x67\x20\x41\x6c\x6c\x6f\x63\x61\x74\x69\x6f\x6e\x53\x69\x74\x65\x20\x6c\x97\x20\x74\x72\x61\x6e\x73\x69\x74\x69\x6f\x6e\x5f\x69\x6e\x66\x6f\x20\x6c\x97\xa",
               static_cast<void*>(*scope_site),
               static_cast<void*>(*object));
      } else {
        PrintF("\x53\x65\x74\x74\x69\x6e\x67\x20\x41\x6c\x6c\x6f\x63\x61\x74\x69\x6f\x6e\x53\x69\x74\x65\x20\x28\x6c\x97\x2c\x20\x6c\x97\x29\x20\x74\x72\x61\x6e\x73\x69\x74\x69\x6f\x6e\x5f\x69\x6e\x66\x6f\x20\x6c\x97\xa",
               static_cast<void*>(*top()),
               static_cast<void*>(*scope_site),
               static_cast<void*>(*object));
      }
    }
  }
}


bool AllocationSiteUsageContext::ShouldCreateMemento(Handle<JSObject> object) {
  if (activated_ && AllocationSite::CanTrack(object->map()->instance_type())) {
    if (FLAG_allocation_site_pretenuring ||
        AllocationSite::GetMode(object->GetElementsKind()) ==
        TRACK_ALLOCATION_SITE) {
      if (FLAG_trace_creation_allocation_sites) {
        PrintF("\x2a\x2a\x2a\x20\x43\x72\x65\x61\x74\x69\x6e\x67\x20\x4d\x65\x6d\x65\x6e\x74\x6f\x20\x66\x6f\x72\x20\x6c\xa2\x20\x6c\x97\xa",
               object->IsJSArray() ? "\x4a\x53\x41\x72\x72\x61\x79" : "\x4a\x53\x4f\x62\x6a\x65\x63\x74",
               static_cast<void*>(*object));
      }
      return true;
    }
  }
  return false;
}

} }  // namespace v8::internal
