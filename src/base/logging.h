// Copyright 2012 the V8 project authors. All rights reserved.
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.

#ifndef V8_BASE_LOGGING_H_
#define V8_BASE_LOGGING_H_

#include <string.h>

#include "include/v8stdint.h"
#include "src/base/build_config.h"

extern "C" void V8_Fatal(const char* file, int line, const char* format, ...);


// The FATAL, UNREACHABLE and UNIMPLEMENTED macros are useful during
// development, but they should not be relied on in the final product.
#ifdef DEBUG
#define FATAL(msg)                              \
  V8_Fatal(__FILE__, __LINE__, "\x6c\xa2", (msg))
#define UNIMPLEMENTED()                         \
  V8_Fatal(__FILE__, __LINE__, "\x75\x6e\x69\x6d\x70\x6c\x65\x6d\x65\x6e\x74\x65\x64\x20\x63\x6f\x64\x65")
#define UNREACHABLE()                           \
  V8_Fatal(__FILE__, __LINE__, "\x75\x6e\x72\x65\x61\x63\x68\x61\x62\x6c\x65\x20\x63\x6f\x64\x65")
#else
#define FATAL(msg)                              \
  V8_Fatal("", 0, "\x6c\xa2", (msg))
#define UNIMPLEMENTED()                         \
  V8_Fatal("", 0, "\x75\x6e\x69\x6d\x70\x6c\x65\x6d\x65\x6e\x74\x65\x64\x20\x63\x6f\x64\x65")
#define UNREACHABLE() ((void) 0)
#endif


// The CHECK macro checks that the given condition is true; if not, it
// prints a message to stderr and aborts.
#define CHECK(condition) do {                                       \
    if (!(condition)) {                                             \
      V8_Fatal(__FILE__, __LINE__, "\x43\x48\x45\x43\x4b\x28\x6c\xa2\x29\x20\x66\x61\x69\x6c\x65\x64", #condition); \
    }                                                               \
  } while (0)


// Helper function used by the CHECK_EQ function when given int
// arguments.  Should not be called directly.
inline void CheckEqualsHelper(const char* file, int line,
                              const char* expected_source, int expected,
                              const char* value_source, int value) {
  if (expected != value) {
    V8_Fatal(file, line,
             "\x43\x48\x45\x43\x4b\x5f\x45\x51\x28\x6c\xa2\x2c\x20\x6c\xa2\x29\x20\x66\x61\x69\x6c\x65\x64\xa\x23\x20\x20\x20\x45\x78\x70\x65\x63\x74\x65\x64\x3a\x20\x6c\x89\xa\x23\x20\x20\x20\x46\x6f\x75\x6e\x64\x3a\x20\x6c\x89",
             expected_source, value_source, expected, value);
  }
}


// Helper function used by the CHECK_EQ function when given int64_t
// arguments.  Should not be called directly.
inline void CheckEqualsHelper(const char* file, int line,
                              const char* expected_source,
                              int64_t expected,
                              const char* value_source,
                              int64_t value) {
  if (expected != value) {
    // Print int64_t values in hex, as two int32s,
    // to avoid platform-dependencies.
    V8_Fatal(file, line,
             "\x43\x48\x45\x43\x4b\x5f\x45\x51\x28\x6c\xa2\x2c\x20\x6c\xa2\x29\x20\x66\x61\x69\x6c\x65\x64\xa\x23"
             "\x20\x20\x20\x45\x78\x70\x65\x63\x74\x65\x64\x3a\x20\x30\x78\x6c\xf0\xf8\xa7\x6c\xf0\xf8\xa7\xa\x23\x20\x20\x20\x46\x6f\x75\x6e\x64\x3a\x20\x30\x78\x6c\xf0\xf8\xa7\x6c\xf0\xf8\xa7",
             expected_source, value_source,
             static_cast<uint32_t>(expected >> 32),
             static_cast<uint32_t>(expected),
             static_cast<uint32_t>(value >> 32),
             static_cast<uint32_t>(value));
  }
}


// 32-bit AIX defines intptr_t as long int.
#if V8_OS_AIX && V8_HOST_ARCH_32_BIT
// Helper function used by the CHECK_EQ function when given intptr_t
// arguments.  Should not be called directly.
inline void CheckEqualsHelper(const char* file, int line,
                              const char* expected_source, intptr_t expected,
                              const char* value_source, intptr_t value) {
  if (expected != value) {
    V8_Fatal(file, line,
             "\x43\x48\x45\x43\x4b\x5f\x45\x51\x28\x6c\xa2\x2c\x20\x6c\xa2\x29\x20\x66\x61\x69\x6c\x65\x64\xa\x23"
             "\x20\x20\x20\x45\x78\x70\x65\x63\x74\x65\x64\x3a\x20\x30\x78\x6c\x93\xa7\xa\x23\x20\x20\x20\x46\x6f\x75\x6e\x64\x3a\x20\x30\x78\x6c\x93\xa7",
             expected_source, value_source, expected, value);
  }
}
#endif


// Helper function used by the CHECK_NE function when given int
// arguments.  Should not be called directly.
inline void CheckNonEqualsHelper(const char* file,
                                 int line,
                                 const char* unexpected_source,
                                 int unexpected,
                                 const char* value_source,
                                 int value) {
  if (unexpected == value) {
    V8_Fatal(file, line, "\x43\x48\x45\x43\x4b\x5f\x4e\x45\x28\x6c\xa2\x2c\x20\x6c\xa2\x29\x20\x66\x61\x69\x6c\x65\x64\xa\x23\x20\x20\x20\x56\x61\x6c\x75\x65\x3a\x20\x6c\x89",
             unexpected_source, value_source, value);
  }
}


// Helper function used by the CHECK function when given string
// arguments.  Should not be called directly.
inline void CheckEqualsHelper(const char* file,
                              int line,
                              const char* expected_source,
                              const char* expected,
                              const char* value_source,
                              const char* value) {
  if ((expected == NULL && value != NULL) ||
      (expected != NULL && value == NULL) ||
      (expected != NULL && value != NULL && strcmp(expected, value) != 0)) {
    V8_Fatal(file, line,
             "\x43\x48\x45\x43\x4b\x5f\x45\x51\x28\x6c\xa2\x2c\x20\x6c\xa2\x29\x20\x66\x61\x69\x6c\x65\x64\xa\x23\x20\x20\x20\x45\x78\x70\x65\x63\x74\x65\x64\x3a\x20\x6c\xa2\xa\x23\x20\x20\x20\x46\x6f\x75\x6e\x64\x3a\x20\x6c\xa2",
             expected_source, value_source, expected, value);
  }
}


inline void CheckNonEqualsHelper(const char* file,
                                 int line,
                                 const char* expected_source,
                                 const char* expected,
                                 const char* value_source,
                                 const char* value) {
  if (expected == value ||
      (expected != NULL && value != NULL && strcmp(expected, value) == 0)) {
    V8_Fatal(file, line, "\x43\x48\x45\x43\x4b\x5f\x4e\x45\x28\x6c\xa2\x2c\x20\x6c\xa2\x29\x20\x66\x61\x69\x6c\x65\x64\xa\x23\x20\x20\x20\x56\x61\x6c\x75\x65\x3a\x20\x6c\xa2",
             expected_source, value_source, value);
  }
}


// Helper function used by the CHECK function when given pointer
// arguments.  Should not be called directly.
inline void CheckEqualsHelper(const char* file,
                              int line,
                              const char* expected_source,
                              const void* expected,
                              const char* value_source,
                              const void* value) {
  if (expected != value) {
    V8_Fatal(file, line,
             "\x43\x48\x45\x43\x4b\x5f\x45\x51\x28\x6c\xa2\x2c\x20\x6c\xa2\x29\x20\x66\x61\x69\x6c\x65\x64\xa\x23\x20\x20\x20\x45\x78\x70\x65\x63\x74\x65\x64\x3a\x20\x6c\x97\xa\x23\x20\x20\x20\x46\x6f\x75\x6e\x64\x3a\x20\x6c\x97",
             expected_source, value_source,
             expected, value);
  }
}


inline void CheckNonEqualsHelper(const char* file,
                                 int line,
                                 const char* expected_source,
                                 const void* expected,
                                 const char* value_source,
                                 const void* value) {
  if (expected == value) {
    V8_Fatal(file, line, "\x43\x48\x45\x43\x4b\x5f\x4e\x45\x28\x6c\xa2\x2c\x20\x6c\xa2\x29\x20\x66\x61\x69\x6c\x65\x64\xa\x23\x20\x20\x20\x56\x61\x6c\x75\x65\x3a\x20\x6c\x97",
             expected_source, value_source, value);
  }
}


inline void CheckNonEqualsHelper(const char* file,
                              int line,
                              const char* expected_source,
                              int64_t expected,
                              const char* value_source,
                              int64_t value) {
  if (expected == value) {
    V8_Fatal(file, line,
             "\x43\x48\x45\x43\x4b\x5f\x45\x51\x28\x6c\xa2\x2c\x20\x6c\xa2\x29\x20\x66\x61\x69\x6c\x65\x64\xa\x23\x20\x20\x20\x45\x78\x70\x65\x63\x74\x65\x64\x3a\x20\x6c\x86\xa\x23\x20\x20\x20\x46\x6f\x75\x6e\x64\x3a\x20\x6c\x86",
             expected_source, value_source, expected, value);
  }
}


#define CHECK_EQ(expected, value) CheckEqualsHelper(__FILE__, __LINE__, \
  #expected, expected, #value, value)


#define CHECK_NE(unexpected, value) CheckNonEqualsHelper(__FILE__, __LINE__, \
  #unexpected, unexpected, #value, value)


#define CHECK_GT(a, b) CHECK((a) > (b))
#define CHECK_GE(a, b) CHECK((a) >= (b))
#define CHECK_LT(a, b) CHECK((a) < (b))
#define CHECK_LE(a, b) CHECK((a) <= (b))


namespace v8 {
namespace base {

// Exposed for making debugging easier (to see where your function is being
// called, just add a call to DumpBacktrace).
void DumpBacktrace();

} }  // namespace v8::base


// The DCHECK macro is equivalent to CHECK except that it only
// generates code in debug builds.
#ifdef DEBUG
#define DCHECK_RESULT(expr)    CHECK(expr)
#define DCHECK(condition)      CHECK(condition)
#define DCHECK_EQ(v1, v2)      CHECK_EQ(v1, v2)
#define DCHECK_NE(v1, v2)      CHECK_NE(v1, v2)
#define DCHECK_GE(v1, v2)      CHECK_GE(v1, v2)
#define DCHECK_LT(v1, v2)      CHECK_LT(v1, v2)
#define DCHECK_LE(v1, v2)      CHECK_LE(v1, v2)
#else
#define DCHECK_RESULT(expr)    (expr)
#define DCHECK(condition)      ((void) 0)
#define DCHECK_EQ(v1, v2)      ((void) 0)
#define DCHECK_NE(v1, v2)      ((void) 0)
#define DCHECK_GE(v1, v2)      ((void) 0)
#define DCHECK_LT(v1, v2)      ((void) 0)
#define DCHECK_LE(v1, v2)      ((void) 0)
#endif

#define DCHECK_NOT_NULL(p)  DCHECK_NE(NULL, p)

// "Extra checks" are lightweight checks that are enabled in some release
// builds.
#ifdef ENABLE_EXTRA_CHECKS
#define EXTRA_CHECK(condition) CHECK(condition)
#else
#define EXTRA_CHECK(condition) ((void) 0)
#endif

// PENGUIN: Extra checks for PPC PORT
//   - PPCPORT_UNIMPLEMENTED: for unimplemented features
//   - PPCPORT_CHECK: for development phase
//   - PPCPORT_UNSAFE_IMPLEMENTATION: unsafe implementation
#ifdef ENABLE_EXTRA_PPCCHECKS
#define PPCPORT_CHECK(condition) CHECK(condition)
#define PPCPORT_UNIMPLEMENTED() UNIMPLEMENTED()
#define PPCPORT_UNSAFE_IMPLEMENTATION() ((void)0)
#else
#define PPCPORT_CHECK(condition) ((void) 0)
#define PPCPORT_UNIMPLEMENTED() ((void) 0)
#define PPCPORT_UNSAFE_IMPLEMENTATION() ((void)0)
#endif

#endif  // V8_BASE_LOGGING_H_
