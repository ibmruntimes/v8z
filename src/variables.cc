// Copyright 2011 the V8 project authors. All rights reserved.
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.

#include "src/v8.h"

#include "src/ast.h"
#include "src/scopes.h"
#include "src/variables.h"

namespace v8 {
namespace internal {

// ----------------------------------------------------------------------------
// Implementation Variable.

const char* Variable::Mode2String(VariableMode mode) {
  switch (mode) {
    case VAR: return "\x56\x41\x52";
    case CONST_LEGACY: return "\x43\x4f\x4e\x53\x54\x5f\x4c\x45\x47\x41\x43\x59";
    case LET: return "\x4c\x45\x54";
    case CONST: return "\x43\x4f\x4e\x53\x54";
    case MODULE: return "\x4d\x4f\x44\x55\x4c\x45";
    case DYNAMIC: return "\x44\x59\x4e\x41\x4d\x49\x43";
    case DYNAMIC_GLOBAL: return "\x44\x59\x4e\x41\x4d\x49\x43\x5f\x47\x4c\x4f\x42\x41\x4c";
    case DYNAMIC_LOCAL: return "\x44\x59\x4e\x41\x4d\x49\x43\x5f\x4c\x4f\x43\x41\x4c";
    case INTERNAL: return "\x49\x4e\x54\x45\x52\x4e\x41\x4c";
    case TEMPORARY: return "\x54\x45\x4d\x50\x4f\x52\x41\x52\x59";
  }
  UNREACHABLE();
  return NULL;
}


Variable::Variable(Scope* scope, const AstRawString* name, VariableMode mode,
                   bool is_valid_ref, Kind kind,
                   InitializationFlag initialization_flag,
                   MaybeAssignedFlag maybe_assigned_flag, Interface* interface)
    : scope_(scope),
      name_(name),
      mode_(mode),
      kind_(kind),
      location_(UNALLOCATED),
      index_(-1),
      initializer_position_(RelocInfo::kNoPosition),
      local_if_not_shadowed_(NULL),
      is_valid_ref_(is_valid_ref),
      force_context_allocation_(false),
      is_used_(false),
      initialization_flag_(initialization_flag),
      maybe_assigned_(maybe_assigned_flag),
      interface_(interface) {
  // Var declared variables never need initialization.
  DCHECK(!(mode == VAR && initialization_flag == kNeedsInitialization));
}


bool Variable::IsGlobalObjectProperty() const {
  // Temporaries are never global, they must always be allocated in the
  // activation frame.
  return (IsDynamicVariableMode(mode_) ||
          (IsDeclaredVariableMode(mode_) && !IsLexicalVariableMode(mode_)))
      && scope_ != NULL && scope_->is_global_scope();
}


int Variable::CompareIndex(Variable* const* v, Variable* const* w) {
  int x = (*v)->index();
  int y = (*w)->index();
  // Consider sorting them according to type as well?
  return x - y;
}

} }  // namespace v8::internal
