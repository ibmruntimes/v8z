// Copyright 2014 the V8 project authors. All rights reserved.
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.

// This file is an internal atomic implementation, use atomicops.h instead.
//

#ifndef V8_ATOMICOPS_INTERNALS_370_XLC_H_
#define V8_ATOMICOPS_INTERNALS_370_XLC_H_

namespace v8 {
namespace base {
// Guru FOLLOWING FUNCTION HAS TO BE REPLACED with atomic implementation
inline bool __sync_bool_compare_and_swap ( volatile Atomic32 *ptr, Atomic32 oldval, Atomic32 newval) {
  if ( *ptr== oldval){
    *ptr=newval;
    return 1;
    }else {
    return 0;
  }
}
inline Atomic32  __sync_val_compare_and_swap ( volatile Atomic32 *ptr, Atomic32 oldval, Atomic32 newval) {
  Atomic32 temp;
  if ( *ptr== oldval){
    temp=*ptr;
    *ptr=newval;
    return temp;
  } else {
    return 0;
  }
}
inline Atomic64  __sync_val_compare_and_swap ( volatile Atomic64 *ptr, Atomic64 oldval, Atomic64 newval) {
  Atomic64 temp;
  if ( *ptr== oldval){
    temp=*ptr;
    *ptr=newval;
    return temp;
  } else {
    return 0;
  }
}

inline Atomic32 NoBarrier_CompareAndSwap(volatile Atomic32* ptr,
                                         Atomic32 old_value,
                                         Atomic32 new_value) {
  return(__sync_val_compare_and_swap( ptr, old_value, new_value));
}

inline Atomic32 NoBarrier_AtomicExchange(volatile Atomic32* ptr,
                                         Atomic32 new_value) {
   Atomic32 old_value;
   do {
     old_value = *ptr;
   } while (__sync_bool_compare_and_swap(ptr, old_value, new_value));
  return old_value;
}

inline Atomic32 NoBarrier_AtomicIncrement(volatile Atomic32* ptr,
                                          Atomic32 increment) {
  return Barrier_AtomicIncrement(ptr, increment);
}

inline Atomic32 Barrier_AtomicIncrement(volatile Atomic32* ptr,
                                        Atomic32 increment) {
  for (;;) {
    Atomic32 old_value = *ptr;
    Atomic32 new_value = old_value + increment;
    if (__sync_bool_compare_and_swap(ptr, old_value, new_value)) {
      return new_value;
      // The exchange took place as expected.
    }
    // Otherwise, *ptr changed mid-loop and we need to retry.
  }
}

inline Atomic32 Acquire_CompareAndSwap(volatile Atomic32* ptr,
                                       Atomic32 old_value,
                                       Atomic32 new_value) {
  return NoBarrier_CompareAndSwap(ptr, old_value, new_value);
}

inline Atomic32 Release_CompareAndSwap(volatile Atomic32* ptr,
                                       Atomic32 old_value,
                                       Atomic32 new_value) {
  return NoBarrier_CompareAndSwap(ptr, old_value, new_value);
}

inline void NoBarrier_Store(volatile Atomic32* ptr, Atomic32 value) {
  *ptr = value;
}

inline void MemoryBarrier() {
  // gcc built-in
  // Guru__sync_synchronize();
   __asm__ __volatile__("sync" : : : "memory");
}

inline void Acquire_Store(volatile Atomic32* ptr, Atomic32 value) {
  *ptr = value;
  MemoryBarrier();
}

inline void Release_Store(volatile Atomic32* ptr, Atomic32 value) {
  MemoryBarrier();
  *ptr = value;
}

inline Atomic32 NoBarrier_Load(volatile const Atomic32* ptr) {
  return *ptr;
}

inline Atomic32 Acquire_Load(volatile const Atomic32* ptr) {
  Atomic32 value = *ptr;
  MemoryBarrier();
  return value;
}

inline Atomic32 Release_Load(volatile const Atomic32* ptr) {
  MemoryBarrier();
  return *ptr;
}

#if defined ( V8_TARGET_ARCH_S390X ) || defined (V8_TARGET_ARCH_MVS)
inline Atomic64 NoBarrier_CompareAndSwap(volatile Atomic64* ptr,
                                         Atomic64 old_value,
                                         Atomic64 new_value) {
   return(__sync_val_compare_and_swap( ptr, old_value, new_value));
}

inline Atomic64 Acquire_CompareAndSwap(volatile Atomic64* ptr,
                                       Atomic64 old_value,
                                       Atomic64 new_value) {
  return NoBarrier_CompareAndSwap(ptr, old_value, new_value);
}

inline Atomic64 Release_CompareAndSwap(volatile Atomic64* ptr,
                                       Atomic64 old_value,
                                       Atomic64 new_value) {
  return NoBarrier_CompareAndSwap(ptr, old_value, new_value);
}

inline void NoBarrier_Store(volatile Atomic64* ptr, Atomic64 value) {
  *ptr = value;
}

inline void Acquire_Store(volatile Atomic64* ptr, Atomic64 value) {
  *ptr = value;
  MemoryBarrier();
}

inline void Release_Store(volatile Atomic64* ptr, Atomic64 value) {
  MemoryBarrier();
  *ptr = value;
}

inline Atomic64 NoBarrier_Load(volatile const Atomic64* ptr) {
  return *ptr;
}

inline Atomic64 Acquire_Load(volatile const Atomic64* ptr) {
  Atomic64 value = *ptr;
  MemoryBarrier();
  return value;
}

inline Atomic64 Release_Load(volatile const Atomic64* ptr) {
  MemoryBarrier();
  return *ptr;
}

#endif

} }  // namespace v8::base

#endif  // V8_ATOMICOPS_INTERNALS_370_XLC_H_
