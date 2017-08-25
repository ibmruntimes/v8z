// Copyright 2006-2008 the V8 project authors. All rights reserved.
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.

#include "src/base/logging.h"

#if V8_LIBC_GLIBC || V8_OS_BSD
#include <cxxabi.h>
#include <dlfcn.h>
#include <execinfo.h>
#elif V8_OS_QNX
#include <backtrace.h>
#elif V8_OS_ZOS
#include <unistd.h>
#endif  // V8_LIBC_GLIBC || V8_OS_BSD

#include <cstdio>
#include <cstdlib>
#include <iostream>

#include "src/base/platform/platform.h"

namespace v8 {
namespace base {

#ifdef V8_OS_ZOS
#define OVERLOAD_OPERATOR_FOR(T) \
    OStream& OStream::operator<<(T val) { \
      std::ostringstream oss; \
      oss << val; \
      std::string str = oss.str(); \
      std::transform(str.begin(), str.end(), str.begin(), Ebcdic2Ascii); \
      std::operator<<(*this, str.c_str()); \
      return *this; \
    }
#else
#define OVERLOAD_OPERATOR_FOR(T) \
    OStream& OStream::operator<<(T val) { \
      std::ostream::operator<<(val); \
      return *this; \
    }
#endif
    OVERLOAD_OPERATOR_FOR(bool)
    OVERLOAD_OPERATOR_FOR(short)
    OVERLOAD_OPERATOR_FOR(unsigned short)
    OVERLOAD_OPERATOR_FOR(int)
    OVERLOAD_OPERATOR_FOR(unsigned int)
    OVERLOAD_OPERATOR_FOR(long)
    OVERLOAD_OPERATOR_FOR(unsigned long)
    OVERLOAD_OPERATOR_FOR(long long)
    OVERLOAD_OPERATOR_FOR(unsigned long long)
    OVERLOAD_OPERATOR_FOR(float)
    OVERLOAD_OPERATOR_FOR(double)
    OVERLOAD_OPERATOR_FOR(long double)
    OVERLOAD_OPERATOR_FOR(void*)
    OVERLOAD_OPERATOR_FOR(const char)
    OVERLOAD_OPERATOR_FOR(const unsigned char)
    OVERLOAD_OPERATOR_FOR(const signed char)
#undef OVERLOAD_OPERATOR_FOR

#define OVERLOAD_OPERATOR_FOR_GENERAL_CASE(T) \
    OStream& OStream::operator<< (T val) { \
      std::ostream::operator<<(val); \
      return *this; \
    }
    OVERLOAD_OPERATOR_FOR_GENERAL_CASE(OStream::pfostream)
    OVERLOAD_OPERATOR_FOR_GENERAL_CASE(OStream::pfios_base)
    OVERLOAD_OPERATOR_FOR_GENERAL_CASE(OStream::pfios)
#undef OVERLOAD_OPERATOR_FOR_SPECIAL_CASE

OStream& OStream::operator<< (const char* val) {
  std::operator<<(*this, val);
  return *this;
}

OStream& OStream::operator<< (const unsigned char *const val) {
  std::operator<<(*this, val);
  return *this;
}

// Explicit instantiations for commonly used comparisons.
#define DEFINE_MAKE_CHECK_OP_STRING(type)              \
  template std::string* MakeCheckOpString<type, type>( \
      type const&, type const&, char const*);
DEFINE_MAKE_CHECK_OP_STRING(int)
DEFINE_MAKE_CHECK_OP_STRING(long)       // NOLINT(runtime/int)
DEFINE_MAKE_CHECK_OP_STRING(long long)  // NOLINT(runtime/int)
DEFINE_MAKE_CHECK_OP_STRING(unsigned int)
DEFINE_MAKE_CHECK_OP_STRING(unsigned long)       // NOLINT(runtime/int)
DEFINE_MAKE_CHECK_OP_STRING(unsigned long long)  // NOLINT(runtime/int)
DEFINE_MAKE_CHECK_OP_STRING(char const*)
DEFINE_MAKE_CHECK_OP_STRING(void const*)
#undef DEFINE_MAKE_CHECK_OP_STRING


// Explicit instantiations for floating point checks.
#define DEFINE_CHECK_OP_IMPL(NAME)                          \
  template std::string* Check##NAME##Impl<float, float>(    \
      float const& lhs, float const& rhs, char const* msg); \
  template std::string* Check##NAME##Impl<double, double>(  \
      double const& lhs, double const& rhs, char const* msg);
DEFINE_CHECK_OP_IMPL(EQ)
DEFINE_CHECK_OP_IMPL(NE)
DEFINE_CHECK_OP_IMPL(LE)
DEFINE_CHECK_OP_IMPL(LT)
DEFINE_CHECK_OP_IMPL(GE)
DEFINE_CHECK_OP_IMPL(GT)
#undef DEFINE_CHECK_OP_IMPL


// Attempts to dump a backtrace (if supported).
void DumpBacktrace() {
#if V8_LIBC_GLIBC || V8_OS_BSD
  void* trace[100];
  int size = backtrace(trace, arraysize(trace));
  OS::PrintError(u8"\n==== C stack trace ===============================\n\n");
  if (size == 0) {
    OS::PrintError(u8"(empty)\n");
  } else {
    for (int i = 1; i < size; ++i) {
      OS::PrintError(u8"%2d: ", i);
      Dl_info info;
      char* demangled = NULL;
      if (!dladdr(trace[i], &info) || !info.dli_sname) {
        OS::PrintError(u8"%p\n", trace[i]);
      } else if ((demangled = abi::__cxa_demangle(info.dli_sname, 0, 0, 0))) {
        OS::PrintError(u8"%s\n", demangled);
        free(demangled);
      } else {
        OS::PrintError(u8"%s\n", info.dli_sname);
      }
    }
  }
#elif V8_OS_QNX
  char out[1024];
  bt_accessor_t acc;
  bt_memmap_t memmap;
  bt_init_accessor(&acc, BT_SELF);
  bt_load_memmap(&acc, &memmap);
  bt_sprn_memmap(&memmap, out, sizeof(out));
  OS::PrintError(out);
  bt_addr_t trace[100];
  int size = bt_get_backtrace(&acc, trace, arraysize(trace));
  OS::PrintError(u8"\n==== C stack trace ===============================\n\n");
  if (size == 0) {
    OS::PrintError(u8"(empty)\n");
  } else {
    bt_sprnf_addrs(&memmap, trace, size, const_cast<char*>("%a\n"),
                   out, sizeof(out), NULL);
    OS::PrintError(out);
  }
  bt_unload_memmap(&memmap);
  bt_release_accessor(&acc);
#endif  // V8_LIBC_GLIBC || V8_OS_BSD
}

}  // namespace base
}  // namespace v8


// Contains protection against recursive calls (faults while handling faults).
#ifdef V8_OS_ZOS
// TODO: Rmove this once compiler can handle u8 prefixed literals in macro definitions
extern "C" void V8_Fatal_e(const char* file, int line, const char* format, const char* str) {
  size_t format_size = strlen(format);
  size_t str_size = strlen(str);
  char format_a[format_size+1];
  char str_a[str_size+1];
  strcpy(format_a, format);
  strcpy(str_a, str);
  format_a[format_size] = '\0';
  str_a[str_size] = '\0';
  __e2a_s(format_a);
  __e2a_s(str_a);
  V8_Fatal(file, line, format_a, str_a);
}
#endif

extern "C" void V8_Fatal(const char* file, int line, const char* format, ...) {
  fflush(stdout);
  fflush(stderr);
  char filename[256];
  strcpy(filename, file);
#ifdef V8_OS_ZOS
  __e2a_s(filename);
#endif
  v8::base::OS::PrintError(u8"\n\n#\n# Fatal error in %s, line %d\n# ", filename,
                           line);
  va_list arguments;
  va_start(arguments, format);
  v8::base::OS::VPrintError(format, arguments);
  va_end(arguments);
  v8::base::OS::PrintError(u8"\n#\n");
  v8::base::DumpBacktrace();
  fflush(stderr);
  v8::base::OS::Abort();
}

extern "C" void V8_RuntimeError(const char* file, int line,
                                const char* message) {
  fflush(stdout);
  fflush(stderr);
  v8::base::OS::PrintError(u8"\n\n#\n# Runtime error in %s, line %d\n# ", file,
                           line);
  v8::base::OS::PrintError(u8"\n# %s\n", message);
  v8::base::DumpBacktrace();
  fflush(stderr);
}
