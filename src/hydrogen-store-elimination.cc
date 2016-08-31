// Copyright 2013 the V8 project authors. All rights reserved.
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.

#include "src/hydrogen-instructions.h"
#include "src/hydrogen-store-elimination.h"

namespace v8 {
namespace internal {

#define TRACE(x) if (FLAG_trace_store_elimination) PrintF x

// Performs a block-by-block local analysis for removable stores.
void HStoreEliminationPhase::Run() {
  GVNFlagSet flags;  // Use GVN flags as an approximation for some instructions.
  flags.RemoveAll();

  flags.Add(kArrayElements);
  flags.Add(kArrayLengths);
  flags.Add(kStringLengths);
  flags.Add(kBackingStoreFields);
  flags.Add(kDoubleArrayElements);
  flags.Add(kDoubleFields);
  flags.Add(kElementsPointer);
  flags.Add(kInobjectFields);
  flags.Add(kExternalMemory);
  flags.Add(kStringChars);
  flags.Add(kTypedArrayElements);

  for (int i = 0; i < graph()->blocks()->length(); i++) {
    unobserved_.Rewind(0);
    HBasicBlock* block = graph()->blocks()->at(i);
    if (!block->IsReachable()) continue;
    for (HInstructionIterator it(block); !it.Done(); it.Advance()) {
      HInstruction* instr = it.Current();
      if (instr->CheckFlag(HValue::kIsDead)) continue;

      // TODO(titzer): eliminate unobserved HStoreKeyed instructions too.
      switch (instr->opcode()) {
        case HValue::kStoreNamedField:
          // Remove any unobserved stores overwritten by this store.
          ProcessStore(HStoreNamedField::cast(instr));
          break;
        case HValue::kLoadNamedField:
          // Observe any unobserved stores on this object + field.
          ProcessLoad(HLoadNamedField::cast(instr));
          break;
        default:
          ProcessInstr(instr, flags);
          break;
      }
    }
  }
}


void HStoreEliminationPhase::ProcessStore(HStoreNamedField* store) {
  HValue* object = store->object()->ActualValue();
  int i = 0;
  while (i < unobserved_.length()) {
    HStoreNamedField* prev = unobserved_.at(i);
    if (aliasing_->MustAlias(object, prev->object()->ActualValue()) &&
        prev->CanBeReplacedWith(store)) {
      // This store is guaranteed to overwrite the previous store.
      prev->DeleteAndReplaceWith(NULL);
      TRACE(("\x2b\x2b\x20\x55\x6e\x6f\x62\x73\x65\x72\x76\x65\x64\x20\x73\x74\x6f\x72\x65\x20\x53\x6c\x84\x20\x6f\x76\x65\x72\x77\x72\x69\x74\x74\x65\x6e\x20\x62\x79\x20\x53\x6c\x84\xa",
             prev->id(), store->id()));
      unobserved_.Remove(i);
    } else {
      // TODO(titzer): remove map word clearing from folded allocations.
      i++;
    }
  }
  // Only non-transitioning stores are removable.
  if (!store->has_transition()) {
    TRACE(("\x2d\x2d\x20\x4d\x69\x67\x68\x74\x20\x72\x65\x6d\x6f\x76\x65\x20\x73\x74\x6f\x72\x65\x20\x53\x6c\x84\xa", store->id()));
    unobserved_.Add(store, zone());
  }
}


void HStoreEliminationPhase::ProcessLoad(HLoadNamedField* load) {
  HValue* object = load->object()->ActualValue();
  int i = 0;
  while (i < unobserved_.length()) {
    HStoreNamedField* prev = unobserved_.at(i);
    if (aliasing_->MayAlias(object, prev->object()->ActualValue()) &&
        load->access().Equals(prev->access())) {
      TRACE(("\x2d\x2d\x20\x4f\x62\x73\x65\x72\x76\x65\x64\x20\x73\x74\x6f\x72\x65\x20\x53\x6c\x84\x20\x62\x79\x20\x6c\x6f\x61\x64\x20\x4c\x6c\x84\xa", prev->id(), load->id()));
      unobserved_.Remove(i);
    } else {
      i++;
    }
  }
}


void HStoreEliminationPhase::ProcessInstr(HInstruction* instr,
    GVNFlagSet flags) {
  if (unobserved_.length() == 0) return;  // Nothing to do.
  if (instr->CanDeoptimize()) {
    TRACE(("\x2d\x2d\x20\x4f\x62\x73\x65\x72\x76\x65\x64\x20\x73\x74\x6f\x72\x65\x73\x20\x61\x74\x20\x49\x6c\x84\x20\x28\x6c\xa2\x20\x6d\x69\x67\x68\x74\x20\x64\x65\x6f\x70\x74\x69\x6d\x69\x7a\x65\x29\xa",
           instr->id(), instr->Mnemonic()));
    unobserved_.Rewind(0);
    return;
  }
  if (instr->CheckChangesFlag(kNewSpacePromotion)) {
    TRACE(("\x2d\x2d\x20\x4f\x62\x73\x65\x72\x76\x65\x64\x20\x73\x74\x6f\x72\x65\x73\x20\x61\x74\x20\x49\x6c\x84\x20\x28\x6c\xa2\x20\x6d\x69\x67\x68\x74\x20\x47\x43\x29\xa",
           instr->id(), instr->Mnemonic()));
    unobserved_.Rewind(0);
    return;
  }
  if (instr->DependsOnFlags().ContainsAnyOf(flags)) {
    TRACE(("\x2d\x2d\x20\x4f\x62\x73\x65\x72\x76\x65\x64\x20\x73\x74\x6f\x72\x65\x73\x20\x61\x74\x20\x49\x6c\x84\x20\x28\x47\x56\x4e\x20\x66\x6c\x61\x67\x73\x20\x6f\x66\x20\x6c\xa2\x29\xa",
           instr->id(), instr->Mnemonic()));
    unobserved_.Rewind(0);
    return;
  }
}

} }  // namespace v8::internal
