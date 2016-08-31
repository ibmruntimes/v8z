// Copyright 2010 the V8 project authors. All rights reserved.
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.

// This file is an internal atomic implementation, use atomicops.h instead.
//
// LinuxKernelCmpxchg and Barrier_AtomicIncrement are from Google Gears.

#ifndef V8_BASE_ATOMICOPS_INTERNALS_ARM_GCC_H_
#define V8_BASE_ATOMICOPS_INTERNALS_ARM_GCC_H_

#if defined(__QNXNTO__)
#include <sys/cpuinline.h>
#endif

namespace v8 {
namespace base {

// Memory barriers on ARM are funky, but the kernel is here to help:
//
// * ARMv5 didn't support SMP, there is no memory barrier instruction at
//   all on this architecture, or when targeting its machine code.
//
// * Some ARMv6 CPUs support SMP. A full memory barrier can be produced by
//   writing a random value to a very specific coprocessor register.
//
// * On ARMv7, the "dmb" instruction is used to perform a full memory
//   barrier (though writing to the co-processor will still work).
//   However, on single core devices (e.g. Nexus One, or Nexus S),
//   this instruction will take up to 200 ns, which is huge, even though
//   it's completely un-needed on these devices.
//
// * There is no easy way to determine at runtime if the device is
//   single or multi-core. However, the kernel provides a useful helper
//   function at a fixed memory address (0xffff0fa0), which will always
//   perform a memory barrier in the most efficient way. I.e. on single
//   core devices, this is an empty function that exits immediately.
//   On multi-core devices, it implements a full memory barrier.
//
// * This source could be compiled to ARMv5 machine code that runs on a
//   multi-core ARMv6 or ARMv7 device. In this case, memory barriers
//   are needed for correct execution. Always call the kernel helper, even
//   when targeting ARMv5TE.
//

inline void MemoryBarrier() {
#if defined(__linux__) || defined(__ANDROID__)
  // Note: This is a function call, which is also an implicit compiler barrier.
  typedef void (*KernelMemoryBarrierFunc)();
  ((KernelMemoryBarrierFunc)0xffff0fa0)();
#elif defined(__QNXNTO__)
  __cpu_membarrier();
#else
#error MemoryBarrier() is not implemented on this platform.
#endif
}

// An ARM toolchain would only define one of these depending on which
// variant of the target architecture is being used. This tests against
// any known ARMv6 or ARMv7 variant, where it is possible to directly
// use ldrex/strex instructions to implement fast atomic operations.
#if defined(__ARM_ARCH_7__) || defined(__ARM_ARCH_7A__) || \
    defined(__ARM_ARCH_7R__) || defined(__ARM_ARCH_7M__) || \
    defined(__ARM_ARCH_6__) || defined(__ARM_ARCH_6J__) || \
    defined(__ARM_ARCH_6K__) || defined(__ARM_ARCH_6Z__) || \
    defined(__ARM_ARCH_6KZ__) || defined(__ARM_ARCH_6T2__)

inline Atomic32 NoBarrier_CompareAndSwap(volatile Atomic32* ptr,
                                         Atomic32 old_value,
                                         Atomic32 new_value) {
  Atomic32 prev_value;
  int reloop;
  do {
    // The following is equivalent to:
    //
    //   prev_value = LDREX(ptr)
    //   reloop = 0
    //   if (prev_value != old_value)
    //      reloop = STREX(ptr, new_value)
    __asm__ __volatile__("\x20\x20\x20\x20\x6c\x64\x72\x65\x78\x20\x25\x30\x2c\x20\x5b\x25\x33\x5d\xa"
                         "\x20\x20\x20\x20\x6d\x6f\x76\x20\x25\x31\x2c\x20\x23\x30\xa"
                         "\x20\x20\x20\x20\x63\x6d\x70\x20\x25\x30\x2c\x20\x25\x34\xa"
#ifdef __thumb2__
                         "\x20\x20\x20\x20\x69\x74\x20\x65\x71\xa"
#endif
                         "\x20\x20\x20\x20\x73\x74\x72\x65\x78\x65\x71\x20\x25\x31\x2c\x20\x25\x35\x2c\x20\x5b\x25\x33\x5d\xa"
                         : "\x3d\x26\x72"(prev_value), "\x3d\x26\x72"(reloop), "\x2b\x6d"(*ptr)
                         : "\x72"(ptr), "\x72"(old_value), "\x72"(new_value)
                         : "\x63\x63", "\x6d\x65\x6d\x6f\x72\x79");
  } while (reloop != 0);
  return prev_value;
}

inline Atomic32 Acquire_CompareAndSwap(volatile Atomic32* ptr,
                                       Atomic32 old_value,
                                       Atomic32 new_value) {
  Atomic32 result = NoBarrier_CompareAndSwap(ptr, old_value, new_value);
  MemoryBarrier();
  return result;
}

inline Atomic32 Release_CompareAndSwap(volatile Atomic32* ptr,
                                       Atomic32 old_value,
                                       Atomic32 new_value) {
  MemoryBarrier();
  return NoBarrier_CompareAndSwap(ptr, old_value, new_value);
}

inline Atomic32 NoBarrier_AtomicIncrement(volatile Atomic32* ptr,
                                          Atomic32 increment) {
  Atomic32 value;
  int reloop;
  do {
    // Equivalent to:
    //
    //  value = LDREX(ptr)
    //  value += increment
    //  reloop = STREX(ptr, value)
    //
    __asm__ __volatile__("\x20\x20\x20\x20\x6c\x64\x72\x65\x78\x20\x25\x30\x2c\x20\x5b\x25\x33\x5d\xa"
                         "\x20\x20\x20\x20\x61\x64\x64\x20\x25\x30\x2c\x20\x25\x30\x2c\x20\x25\x34\xa"
                         "\x20\x20\x20\x20\x73\x74\x72\x65\x78\x20\x25\x31\x2c\x20\x25\x30\x2c\x20\x5b\x25\x33\x5d\xa"
                         : "\x3d\x26\x72"(value), "\x3d\x26\x72"(reloop), "\x2b\x6d"(*ptr)
                         : "\x72"(ptr), "\x72"(increment)
                         : "\x63\x63", "\x6d\x65\x6d\x6f\x72\x79");
  } while (reloop);
  return value;
}

inline Atomic32 Barrier_AtomicIncrement(volatile Atomic32* ptr,
                                        Atomic32 increment) {
  // TODO(digit): Investigate if it's possible to implement this with
  // a single MemoryBarrier() operation between the LDREX and STREX.
  // See http://crbug.com/246514
  MemoryBarrier();
  Atomic32 result = NoBarrier_AtomicIncrement(ptr, increment);
  MemoryBarrier();
  return result;
}

inline Atomic32 NoBarrier_AtomicExchange(volatile Atomic32* ptr,
                                         Atomic32 new_value) {
  Atomic32 old_value;
  int reloop;
  do {
    // old_value = LDREX(ptr)
    // reloop = STREX(ptr, new_value)
    __asm__ __volatile__("\x20\x20\x20\x6c\x64\x72\x65\x78\x20\x25\x30\x2c\x20\x5b\x25\x33\x5d\xa"
                         "\x20\x20\x20\x73\x74\x72\x65\x78\x20\x25\x31\x2c\x20\x25\x34\x2c\x20\x5b\x25\x33\x5d\xa"
                         : "\x3d\x26\x72"(old_value), "\x3d\x26\x72"(reloop), "\x2b\x6d"(*ptr)
                         : "\x72"(ptr), "\x72"(new_value)
                         : "\x63\x63", "\x6d\x65\x6d\x6f\x72\x79");
  } while (reloop != 0);
  return old_value;
}

// This tests against any known ARMv5 variant.
#elif defined(__ARM_ARCH_5__) || defined(__ARM_ARCH_5T__) || \
      defined(__ARM_ARCH_5TE__) || defined(__ARM_ARCH_5TEJ__)

// The kernel also provides a helper function to perform an atomic
// compare-and-swap operation at the hard-wired address 0xffff0fc0.
// On ARMv5, this is implemented by a special code path that the kernel
// detects and treats specially when thread pre-emption happens.
// On ARMv6 and higher, it uses LDREX/STREX instructions instead.
//
// Note that this always perform a full memory barrier, there is no
// need to add calls MemoryBarrier() before or after it. It also
// returns 0 on success, and 1 on exit.
//
// Available and reliable since Linux 2.6.24. Both Android and ChromeOS
// use newer kernel revisions, so this should not be a concern.
namespace {

inline int LinuxKernelCmpxchg(Atomic32 old_value,
                              Atomic32 new_value,
                              volatile Atomic32* ptr) {
  typedef int (*KernelCmpxchgFunc)(Atomic32, Atomic32, volatile Atomic32*);
  return ((KernelCmpxchgFunc)0xffff0fc0)(old_value, new_value, ptr);
}

}  // namespace

inline Atomic32 NoBarrier_CompareAndSwap(volatile Atomic32* ptr,
                                         Atomic32 old_value,
                                         Atomic32 new_value) {
  Atomic32 prev_value;
  for (;;) {
    prev_value = *ptr;
    if (prev_value != old_value)
      return prev_value;
    if (!LinuxKernelCmpxchg(old_value, new_value, ptr))
      return old_value;
  }
}

inline Atomic32 NoBarrier_AtomicExchange(volatile Atomic32* ptr,
                                         Atomic32 new_value) {
  Atomic32 old_value;
  do {
    old_value = *ptr;
  } while (LinuxKernelCmpxchg(old_value, new_value, ptr));
  return old_value;
}

inline Atomic32 NoBarrier_AtomicIncrement(volatile Atomic32* ptr,
                                          Atomic32 increment) {
  return Barrier_AtomicIncrement(ptr, increment);
}

inline Atomic32 Barrier_AtomicIncrement(volatile Atomic32* ptr,
                                        Atomic32 increment) {
  for (;;) {
    // Atomic exchange the old value with an incremented one.
    Atomic32 old_value = *ptr;
    Atomic32 new_value = old_value + increment;
    if (!LinuxKernelCmpxchg(old_value, new_value, ptr)) {
      // The exchange took place as expected.
      return new_value;
    }
    // Otherwise, *ptr changed mid-loop and we need to retry.
  }
}

inline Atomic32 Acquire_CompareAndSwap(volatile Atomic32* ptr,
                                       Atomic32 old_value,
                                       Atomic32 new_value) {
  Atomic32 prev_value;
  for (;;) {
    prev_value = *ptr;
    if (prev_value != old_value) {
      // Always ensure acquire semantics.
      MemoryBarrier();
      return prev_value;
    }
    if (!LinuxKernelCmpxchg(old_value, new_value, ptr))
      return old_value;
  }
}

inline Atomic32 Release_CompareAndSwap(volatile Atomic32* ptr,
                                       Atomic32 old_value,
                                       Atomic32 new_value) {
  // This could be implemented as:
  //    MemoryBarrier();
  //    return NoBarrier_CompareAndSwap();
  //
  // But would use 3 barriers per succesful CAS. To save performance,
  // use Acquire_CompareAndSwap(). Its implementation guarantees that:
  // - A succesful swap uses only 2 barriers (in the kernel helper).
  // - An early return due to (prev_value != old_value) performs
  //   a memory barrier with no store, which is equivalent to the
  //   generic implementation above.
  return Acquire_CompareAndSwap(ptr, old_value, new_value);
}

#else
#  error "\x59\x6f\x75\x72\x20\x43\x50\x55\x27\x73\x20\x41\x52\x4d\x20\x61\x72\x63\x68\x69\x74\x65\x63\x74\x75\x72\x65\x20\x69\x73\x20\x6e\x6f\x74\x20\x73\x75\x70\x70\x6f\x72\x74\x65\x64\x20\x79\x65\x74"
#endif

// NOTE: Atomicity of the following load and store operations is only
// guaranteed in case of 32-bit alignement of |ptr| values.

inline void NoBarrier_Store(volatile Atomic32* ptr, Atomic32 value) {
  *ptr = value;
}

inline void Acquire_Store(volatile Atomic32* ptr, Atomic32 value) {
  *ptr = value;
  MemoryBarrier();
}

inline void Release_Store(volatile Atomic32* ptr, Atomic32 value) {
  MemoryBarrier();
  *ptr = value;
}

inline Atomic32 NoBarrier_Load(volatile const Atomic32* ptr) { return *ptr; }

inline Atomic32 Acquire_Load(volatile const Atomic32* ptr) {
  Atomic32 value = *ptr;
  MemoryBarrier();
  return value;
}

inline Atomic32 Release_Load(volatile const Atomic32* ptr) {
  MemoryBarrier();
  return *ptr;
}

// Byte accessors.

inline void NoBarrier_Store(volatile Atomic8* ptr, Atomic8 value) {
  *ptr = value;
}

inline Atomic8 NoBarrier_Load(volatile const Atomic8* ptr) { return *ptr; }

} }  // namespace v8::base

#endif  // V8_BASE_ATOMICOPS_INTERNALS_ARM_GCC_H_
