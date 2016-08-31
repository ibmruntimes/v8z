// Copyright 2006-2008 the V8 project authors. All rights reserved.
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.

#include "src/base/logging.h"

#if V8_LIBC_GLIBC || V8_OS_BSD
# include <cxxabi.h>
# include <execinfo.h>
#elif V8_OS_QNX
# include <backtrace.h>
#endif  // V8_LIBC_GLIBC || V8_OS_BSD
#include <stdio.h>
#include <stdlib.h>

#include "src/base/platform/platform.h"

namespace v8 {
namespace base {

// Attempts to dump a backtrace (if supported).
void DumpBacktrace() {
#if V8_LIBC_GLIBC || V8_OS_BSD
  void* trace[100];
  int size = backtrace(trace, ARRAY_SIZE(trace));
  char** symbols = backtrace_symbols(trace, size);
  OS::PrintError("\xa\x3d\x3d\x3d\x3d\x20\x43\x20\x73\x74\x61\x63\x6b\x20\x74\x72\x61\x63\x65\x20\x3d\x3d\x3d\x3d\x3d\x3d\x3d\x3d\x3d\x3d\x3d\x3d\x3d\x3d\x3d\x3d\x3d\x3d\x3d\x3d\x3d\x3d\x3d\x3d\x3d\x3d\x3d\x3d\x3d\x3d\x3d\xa\xa");
  if (size == 0) {
    OS::PrintError("\x28\x65\x6d\x70\x74\x79\x29\xa");
  } else if (symbols == NULL) {
    OS::PrintError("\x28\x6e\x6f\x20\x73\x79\x6d\x62\x6f\x6c\x73\x29\xa");
  } else {
    for (int i = 1; i < size; ++i) {
      OS::PrintError("\x6c\xf2\x84\x3a\x20", i);
      char mangled[201];
      if (sscanf(symbols[i], "\x25\x2a\x5b\x5e\x28\x5d\x25\x2a\x5b\x28\x5d\x25\x32\x30\x30\x5b\x5e\x29\x2b\x5d", mangled) == 1) {  // NOLINT
        int status;
        size_t length;
        char* demangled = abi::__cxa_demangle(mangled, NULL, &length, &status);
        OS::PrintError("\x6c\xa2\xa", demangled != NULL ? demangled : mangled);
        free(demangled);
      } else {
        OS::PrintError("\x3f\x3f\xa");
      }
    }
  }
  free(symbols);
#elif V8_OS_QNX
  char out[1024];
  bt_accessor_t acc;
  bt_memmap_t memmap;
  bt_init_accessor(&acc, BT_SELF);
  bt_load_memmap(&acc, &memmap);
  bt_sprn_memmap(&memmap, out, sizeof(out));
  OS::PrintError(out);
  bt_addr_t trace[100];
  int size = bt_get_backtrace(&acc, trace, ARRAY_SIZE(trace));
  OS::PrintError("\xa\x3d\x3d\x3d\x3d\x20\x43\x20\x73\x74\x61\x63\x6b\x20\x74\x72\x61\x63\x65\x20\x3d\x3d\x3d\x3d\x3d\x3d\x3d\x3d\x3d\x3d\x3d\x3d\x3d\x3d\x3d\x3d\x3d\x3d\x3d\x3d\x3d\x3d\x3d\x3d\x3d\x3d\x3d\x3d\x3d\x3d\x3d\xa\xa");
  if (size == 0) {
    OS::PrintError("\x28\x65\x6d\x70\x74\x79\x29\xa");
  } else {
    bt_sprnf_addrs(&memmap, trace, size, const_cast<char*>("\x6c\x81\xa"),
                   out, sizeof(out), NULL);
    OS::PrintError(out);
  }
  bt_unload_memmap(&memmap);
  bt_release_accessor(&acc);
#endif  // V8_LIBC_GLIBC || V8_OS_BSD
}

} }  // namespace v8::base


// Contains protection against recursive calls (faults while handling faults).
extern "C" void V8_Fatal(const char* file, int line, const char* format, ...) {
  fflush(stdout);
  fflush(stderr);
  v8::base::OS::PrintError("\xa\xa\x23\xa\x23\x20\x46\x61\x74\x61\x6c\x20\x65\x72\x72\x6f\x72\x20\x69\x6e\x20\x6c\xa2\x2c\x20\x6c\x69\x6e\x65\x20\x6c\x84\xa\x23\x20", file,
                           line);
  va_list arguments;
  va_start(arguments, format);
  v8::base::OS::VPrintError(format, arguments);
  va_end(arguments);
  v8::base::OS::PrintError("\xa\x23\xa");
  v8::base::DumpBacktrace();
  fflush(stderr);
  v8::base::OS::Abort();
}
