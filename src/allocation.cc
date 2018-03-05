// Copyright 2012 the V8 project authors. All rights reserved.
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.

#include "src/allocation.h"

#include <stdlib.h>  // For free, malloc.
#include "src/base/bits.h"
#include "src/base/logging.h"
#include "src/base/platform/platform.h"
#include "src/utils.h"
#include "src/v8.h"

#if V8_LIBC_BIONIC
#include <malloc.h>  // NOLINT
#endif

namespace v8 {
namespace internal {

namespace {

void* AlignedAllocInternal(size_t size, size_t alignment) {
  void* ptr;
#if V8_OS_WIN
  ptr = _aligned_malloc(size, alignment);
#elif V8_LIBC_BIONIC
  // posix_memalign is not exposed in some Android versions, so we fall back to
  // memalign. See http://code.google.com/p/android/issues/detail?id=35391.
  ptr = memalign(alignment, size);
#elif V8_OS_ZOS
  void **aligned_ptr;
  int offset = alignment - 1 + sizeof(void *);
  ptr = (void *)malloc(size + offset);
  if (ptr == NULL) {
     return ptr;
  }
  aligned_ptr = (void **)(((size_t)(ptr) + offset) & ~(alignment -1));
  aligned_ptr[-1] = ptr;
  return aligned_ptr;
#else
  if (posix_memalign(&ptr, alignment, size)) ptr = NULL;
#endif
  if (ptr == NULL) V8::FatalProcessOutOfMemory("AlignedAlloc");
  return ptr;
}

}  // namespace

void* Malloced::New(size_t size) {
  void* result = malloc(size);
  if (result == nullptr) {
    V8::GetCurrentPlatform()->OnCriticalMemoryPressure();
    result = malloc(size);
    if (result == nullptr) {
      V8::FatalProcessOutOfMemory("Malloced operator new");
    }
  }
  return result;
}


void Malloced::Delete(void* p) {
  free(p);
}


char* StrDup(const char* str) {
  int length = StrLength(str);
  char* result = NewArray<char>(length + 1);
  MemCopy(result, str, length);
  result[length] = '\0';
  return result;
}


char* StrNDup(const char* str, int n) {
  int length = StrLength(str);
  if (n < length) length = n;
  char* result = NewArray<char>(length + 1);
  MemCopy(result, str, length);
  result[length] = '\0';
  return result;
}


void* AlignedAlloc(size_t size, size_t alignment) {
  DCHECK_LE(V8_ALIGNOF(void*), alignment);
  DCHECK(base::bits::IsPowerOfTwo(alignment));
  void* ptr = AlignedAllocInternal(size, alignment);
  if (ptr == nullptr) {
    V8::GetCurrentPlatform()->OnCriticalMemoryPressure();
    ptr = AlignedAllocInternal(size, alignment);
    if (ptr == nullptr) {
      V8::FatalProcessOutOfMemory("AlignedAlloc");
    }
  }
}


void AlignedFree(void *ptr) {
#if V8_OS_WIN
  _aligned_free(ptr);
#elif V8_LIBC_BIONIC
  // Using free is not correct in general, but for V8_LIBC_BIONIC it is.
  free(ptr);
#elif V8_OS_ZOS
  free(((void **)ptr)[-1]);
#else
  free(ptr);
#endif
}

bool AllocVirtualMemory(size_t size, void* hint, base::VirtualMemory* result) {
  base::VirtualMemory first_try(size, hint);
  if (first_try.IsReserved()) {
    result->TakeControl(&first_try);
    return true;
  }

  V8::GetCurrentPlatform()->OnCriticalMemoryPressure();
  base::VirtualMemory second_try(size, hint);
  result->TakeControl(&second_try);
  return result->IsReserved();
}

bool AlignedAllocVirtualMemory(size_t size, size_t alignment, void* hint,
                               base::VirtualMemory* result) {
  base::VirtualMemory first_try(size, alignment, hint);
  if (first_try.IsReserved()) {
    result->TakeControl(&first_try);
    return true;
  }

  V8::GetCurrentPlatform()->OnCriticalMemoryPressure();
  base::VirtualMemory second_try(size, alignment, hint);
  result->TakeControl(&second_try);
  return result->IsReserved();
}

}  // namespace internal
}  // namespace v8
