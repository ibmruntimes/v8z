// Copyright 2014 the V8 project authors. All rights reserved.
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.

// This file is an internal atomic implementation, use atomicops.h instead.
//

#ifndef V8_ATOMICOPS_INTERNALS_S390_XLC_H_
#define V8_ATOMICOPS_INTERNALS_S390_XLC_H_

namespace v8 {
namespace base {

// TODO(mcornac): Verify function merged from ISL 3.14.
inline bool __sync_bool_compare_and_swap(volatile Atomic32 *ptr,
                                         Atomic32 oldval,
                                         Atomic32 newval) {
  if (*ptr == oldval) {
    *ptr = newval;
    return 1;
  } else {
    return 0;
  }
}

// TODO(mcornac): Verify function merged from ISL 3.14.
inline Atomic32 __sync_val_compare_and_swap(volatile Atomic32 *ptr,
                                             Atomic32 oldval,
                                             Atomic32 newval) {
  Atomic32 tmp;
  if (*ptr == oldval) {
    tmp = *ptr;
    *ptr = newval;
    return tmp;
  } else {
    return 0;
  }
}

// TODO(mcornac): Implement.
inline Atomic32 __sync_add_and_fetch(volatile Atomic32 *ptr, Atomic32 value) {
  Atomic32 tmp = *ptr;
  *ptr = tmp + value;
  return tmp;
}

#ifdef V8_TARGET_ARCH_S390X
// TODO(mcornac): Implement.
inline bool __sync_bool_compare_and_swap(volatile Atomic64 *ptr,
                                         Atomic64 oldval,
                                         Atomic64 newval) {
  if (*ptr == oldval) {
    *ptr = newval;
    return 1;
  } else {
    return 0;
  }
}

// TODO(mcornac): Verify function merged from ISL 3.14.
inline Atomic64 __sync_val_compare_and_swap(volatile Atomic64 *ptr,
                                            Atomic64 oldval,
                                            Atomic64 newval) {
  Atomic64 tmp;
  if (*ptr == oldval) {
    tmp = *ptr;
    *ptr = newval;
    return tmp;
  } else {
    return 0;
  }
}

// TODO(mcornac): Implement.
inline bool __sync_add_and_fetch(volatile Atomic64 *ptr, Atomic64 value) {
  Atomic64 tmp = *ptr;
  *ptr = tmp + value;
  return tmp;
}
#endif

// TODO(mcornac): Implement
inline bool __sync_synchronize() {
  return 0;
}

} }  // namespace v8::base

#endif  // V8_ATOMICOPS_INTERNALS_S390_XLC_H_
