// Copyright 2012 the V8 project authors. All rights reserved.
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.

// Platform-specific code for zOS/Unix goes here. For the POSIX-compatible
// parts, the implementation is in platform-posix.cc.

#include <pthread.h>
#include <signal.h>
#include <stdlib.h>
#include <sys/resource.h>
#include <sys/time.h>
#include <sys/types.h>

// Ubuntu Dapper requires memory pages to be marked as
// executable. Otherwise, OS raises an exception when executing code
// in that page.
#include <errno.h>
#include <fcntl.h>      // open
#include <stdarg.h>
#include <strings.h>    // index
#undef index
#include <sys/mman.h>   // mmap & munmap
#include <sys/stat.h>   // open
#include <sys/types.h>  // mmap & munmap
#include <unistd.h>     // sysconf

// GLibc on ARM defines mcontext_t has a typedef for 'struct sigcontext'.
// Old versions of the C library <signal.h> didn't define the type.
#if defined(__ANDROID__) && !defined(__BIONIC_HAVE_UCONTEXT_T) && \
    (defined(__arm__) || defined(__aarch64__)) && \
    !defined(__BIONIC_HAVE_STRUCT_SIGCONTEXT)
#include <asm/sigcontext.h>  // NOLINT
#endif

#if defined(LEAK_SANITIZER)
#include <sanitizer/lsan_interface.h>
#endif

#include <cmath>

#undef MAP_TYPE

#include "src/base/macros.h"
#include "src/base/platform/platform.h"
#include "src/base/platform/platform-posix.h"
#include "src/s390/semaphore-zos.h"
#include "src/base/sys-info.h"

#define MAP_FAILED ((void *)-1L)

namespace v8 {
namespace base {


#ifdef __arm__

bool OS::ArmUsingHardFloat() {
  // GCC versions 4.6 and above define __ARM_PCS or __ARM_PCS_VFP to specify
  // the Floating Point ABI used (PCS stands for Procedure Call Standard).
  // We use these as well as a couple of other defines to statically determine
  // what FP ABI used.
  // GCC versions 4.4 and below don't support hard-fp.
  // GCC versions 4.5 may support hard-fp without defining __ARM_PCS or
  // __ARM_PCS_VFP.

#define GCC_VERSION (__GNUC__ * 10000                                          \
                     + __GNUC_MINOR__ * 100                                    \
                     + __GNUC_PATCHLEVEL__)
#if GCC_VERSION >= 40600
#if defined(__ARM_PCS_VFP)
  return true;
#else
  return false;
#endif

#elif GCC_VERSION < 40500
  return false;

#else
#if defined(__ARM_PCS_VFP)
  return true;
#elif defined(__ARM_PCS) || defined(__SOFTFP__) || defined(__SOFTFP) || \
      !defined(__VFP_FP__)
  return false;
#else
#error "\x59\x6f\x75\x72\x20\x76\x65\x72\x73\x69\x6f\x6e\x20\x6f\x66\x20\x47\x43\x43\x20\x64\x6f\x65\x73\x20\x6e\x6f\x74\x20\x72\x65\x70\x6f\x72\x74\x20\x74\x68\x65\x20\x46\x50\x20\x41\x42\x49\x20\x63\x6f\x6d\x70\x69\x6c\x65\x64\x20\x66\x6f\x72\x2e"          \
       "\x50\x6c\x65\x61\x73\x65\x20\x72\x65\x70\x6f\x72\x74\x20\x69\x74\x20\x6f\x6e\x20\x74\x68\x69\x73\x20\x69\x73\x73\x75\x65"                                        \
       "\x68\x74\x74\x70\x3a\x2f\x2f\x63\x6f\x64\x65\x2e\x67\x6f\x6f\x67\x6c\x65\x2e\x63\x6f\x6d\x2f\x70\x2f\x76\x38\x2f\x69\x73\x73\x75\x65\x73\x2f\x64\x65\x74\x61\x69\x6c\x3f\x69\x64\x3d\x32\x31\x34\x30"

#endif
#endif
#undef GCC_VERSION
}

#endif  // def __arm__


#define asm __asm__ volatile

static const int kMegaByte = 1024*1024;

static void * anon_mmap(void * addr, size_t len) {
   int retcode;
   bool rmode64 = SysInfo::ExecutablePagesAbove2GB();
   if (rmode64 && len % kMegaByte == 0) {
     __mopl_t mopl_instance;
     void * p = NULL;
     size_t request_size = len / kMegaByte;
     memset(&mopl_instance,0,sizeof(mopl_instance));
     mopl_instance.__mopldumppriority = __MO_DUMP_PRIORITY_HEAP;
     mopl_instance.__moplrequestsize = request_size;
     retcode = __moservices(__MO_GETSTOR,sizeof(mopl_instance), &mopl_instance, &p);
     return (retcode == 0) ? p : MAP_FAILED;
   } else {
     int retcode;
     char * p;
#pragma convert("ibm-1047")
#if defined(__64BIT__)
      __asm(" SYSSTATE ARCHLVL=2,AMODE64=YES\x15"
            " STORAGE OBTAIN,LENGTH=(%2),BNDRY=PAGE,COND=YES,ADDR=(%0),RTCD=(%1),"
            "LOC=(31,64)\x15"
# if defined(__clang__)
            :"=NR:r1"(p),"=NR:r15"(retcode): "NR:r0"(len): "r0","r1","r14","r15");
# else
            :"=r"(p),"=r"(retcode): "r"(len): "r0","r1","r14","r15");
# endif
#else
      __asm(" SYSSTATE ARCHLVL=2\x15"
            " STORAGE OBTAIN,LENGTH=(%2),BNDRY=PAGE,COND=YES,ADDR=(%0),RTCD=(%1)\x15"
# if defined(__clang__)
            :"=NR:r1"(p),"=NR:r15"(retcode): "NR:r0"(len): "r0","r1","r14","r15");
# else
            :"=r"(p),"=r"(retcode): "r"(len): "r0","r1","r14","r15");
# endif
#endif
#pragma convert(pop)
    return (retcode == 0) ? p : MAP_FAILED;
   }
}


static int anon_munmap(void * addr, size_t len) {
   int retcode;
   bool rmode64 = SysInfo::ExecutablePagesAbove2GB();
   if (rmode64 && len % kMegaByte == 0) {
     retcode == __moservices(__MO_DETACH,0,NULL, &addr);
   } else {
#pragma convert("ibm-1047")
#if defined (__64BIT__)
  __asm(" SYSSTATE ARCHLVL=2,AMODE64=YES\x15"
          " STORAGE RELEASE,LENGTH=(%2),ADDR=(%1),RTCD=(%0),COND=YES\x15"
# if defined(__clang__)
          :"=NR:r15"(retcode): "NR:r1"(addr), "NR:r0"(len) : "r0","r1","r14","r15");
# else
          :"=r"(retcode): "r"(addr), "r"(len) : "r0","r1","r14","r15");
# endif
#else
  __asm(" SYSSTATE ARCHLVL=2\x15"
          " STORAGE RELEASE,LENGTH=(%2),ADDR=(%1),RTCD=(%0),COND=YES\x15"
# if defined(__clang__)
          :"=NR:r15"(retcode): "NR:r1"(addr), "NR:r0"(len) : "r0","r1","r14","r15");
# else
          :"=r"(retcode): "r"(addr), "r"(len) : "r0","r1","r14","r15");
# endif
#endif
#pragma convert(pop)
   }
   return retcode;
}


void OS::Free(void* address, const size_t size) {
  // TODO(1240712): munmap has a return value which is ignored here.
  int result = anon_munmap(address, size);
  USE(result);
  DCHECK(result == 0);
}


void OS::ConvertToASCII(char * str) {
  size_t length =  __e2a_s(str);
  DCHECK_NE(length, -1);
}

class ZOSTimezoneCache : public PosixTimezoneCache {
  const char * LocalTimezone(double time) override;

  double LocalTimeOffset() override;

  ~ZOSTimezoneCache() override {}
};


const char* ZOSTimezoneCache::LocalTimezone(double time) {

  if (isnan(time)) return "";
  time_t tv = static_cast<time_t>(std::floor(time/msPerSecond));
  struct tm tm;
  struct tm* t= localtime_r(&tv, &tm);
  if (NULL == t) 
    return "";

#ifdef __MVS__
  char *tz = new char[strlen(tzname[0]) + 1];
  memcpy(tz, tzname[0], strlen(tzname[0]) + 1);
  __e2a_s(tz);
  return tz; // The location of the timezone string on zOS
#else
  return tzname[0];
#endif
  /*
  double offset_secs = LocalTimeOffset(cache) / msPerSecond;
  int offset_hrs  = (int)offset_secs/3600;
  if ( offset_hrs == 0)
    return u8"GMT";
  else if (offset_hrs > -6 && offset_hrs <= -5)
    return u8"EST";
  //Todo(muntasir) Add the rest of the timezones
  return "";*/
}

double ZOSTimezoneCache::LocalTimeOffset() {
  time_t tv = time(NULL);
  struct tm* gmt = gmtime(&tv);
  double gm_secs = gmt->tm_sec + (gmt->tm_min * 60) + (gmt->tm_hour * 3600);
  struct tm* localt = localtime(&tv);
  double local_secs = localt->tm_sec + (localt->tm_min * 60) +
                      (localt->tm_hour * 3600);
  return (local_secs - gm_secs) * msPerSecond -
         (localt->tm_isdst > 0 ? 3600 * msPerSecond : 0);
}

TimezoneCache * OS::CreateTimezoneCache() { return new ZOSTimezoneCache(); }

void* OS::Allocate(const size_t requested, size_t* allocated,
                   OS::MemoryPermission access, void * hint) {
  const size_t msize = RoundUp(requested, AllocateAlignment());
  int prot = GetProtectionFromMemoryPermission(access);
  void * mbase = static_cast<void*>(anon_mmap(OS::GetRandomMmapAddr(),
                                    sizeof(char) * msize));
  if (mbase == MAP_FAILED) return NULL;
  *allocated = msize;
  return mbase;
}

std::vector<OS::SharedLibraryAddress> OS::GetSharedLibraryAddresses() {
  std::vector<SharedLibraryAddress> result;
  // This function assumes that the layout of the file is as follows:
  // hex_start_addr-hex_end_addr rwxp <unused data> [binary_file_name]
  // If we encounter an unexpected situation we abort scanning further entries.
  FILE* fp = fopen("\x2f\x70\x72\x6f\x63\x2f\x73\x65\x6c\x66\x2f\x6d\x61\x70\x73", "\x72");
  if (fp == NULL) return result;

  // Allocate enough room to be able to store a full file name.
  const int kLibNameLen = FILENAME_MAX + 1;
  char* lib_name = reinterpret_cast<char*>(malloc(kLibNameLen));

  // This loop will terminate once the scanning hits an EOF.
  while (true) {
    uintptr_t start, end;
    char attr_r, attr_w, attr_x, attr_p;
    // Parse the addresses and permission bits at the beginning of the line.
    if (fscanf(fp, "\x25" V8PRIxPTR "\x2d\x25" V8PRIxPTR, &start, &end) != 2) break;
    if (fscanf(fp, "\x20\x6c\x83\x6c\x83\x6c\x83\x6c\x83", &attr_r, &attr_w, &attr_x, &attr_p) != 4) break;

    int c;
    if (attr_r == '\x72' && attr_w != '\x77' && attr_x == '\x78') {
      // Found a read-only executable entry. Skip characters until we reach
      // the beginning of the filename or the end of the line.
      do {
        c = getc(fp);
      } while ((c != EOF) && (c != '\xa') && (c != '\x2f') && (c != '\x5b'));
      if (c == EOF) break;  // EOF: Was unexpected, just exit.

      // Process the filename if found.
      if ((c == '\x2f') || (c == '\x5b')) {
        // Push the '/' or '[' back into the stream to be read below.
        ungetc(c, fp);

		// Read to the end of the line. Exit if the read fails.
        if (fgets(lib_name, kLibNameLen, fp) == NULL) break;

        // Drop the newline character read by fgets. We do not need to check
        // for a zero-length string because we know that we at least read the
        // '/' or '[' character.
        lib_name[strlen(lib_name) - 1] = '\x0';
      } else {
        // No library name found, just record the raw address range.
        snprintf(lib_name, kLibNameLen,
                 "\x25\x30\x38" V8PRIxPTR "\x2d\x25\x30\x38" V8PRIxPTR, start, end);
      }
      result.push_back(SharedLibraryAddress(lib_name, start, end));
    } else {
      // Entry not describing executable data. Skip to end of line to set up
      // reading the next entry.
      do {
        c = getc(fp);
      } while ((c != EOF) && (c != '\xa'));
      if (c == EOF) break;
    }
  }
  free(lib_name);
  fclose(fp);
  return result;
}


void OS::SignalCodeMovingGC() {
  // Support for ll_prof.py.
  //
  // The Linux profiler built into the kernel logs all mmap's with
  // PROT_EXEC so that analysis tools can properly attribute ticks. We
  // do a mmap with a name known by ll_prof.py and immediately munmap
  // it. This injects a GC marker into the stream of events generated
  // by the kernel and allows us to synchronize V8 code log and the
  // kernel log.
  int size = sysconf(_SC_PAGESIZE);
  FILE* f = NULL;
  if (f == NULL) {
    OS::PrintError("\x46\x61\x69\x6c\x65\x64\x20\x74\x6f\x20\x6f\x70\x65\x6e\x20\x6c\xa2\xa", "");
    OS::Abort();
  }
  void* addr = mmap(OS::GetRandomMmapAddr(),
                    size,
#if defined(__native_client__)
                    // The Native Client port of V8 uses an interpreter,
                    // so code pages don't need PROT_EXEC.
                    PROT_READ,
#else
                    PROT_READ | PROT_EXEC,
#endif
                    MAP_PRIVATE,
                    fileno(f),
                    0);
  OS::Free(addr, size);
  fclose(f);
}

static const int kMmapFd = -1;
// Constants used for mmap.
static const int kMmapFdOffset = 0;

VirtualMemory::VirtualMemory() : address_(NULL), size_(0), reservation_(NULL) { }


VirtualMemory::VirtualMemory(size_t size, void * hint)
    : address_(ReserveRegion(size, hint)), size_(size), reservation_(address_) { }


VirtualMemory::VirtualMemory(size_t size, size_t alignment, void * hint)
    : address_(NULL), size_(0), reservation_(NULL) {
  DCHECK((alignment % OS::AllocateAlignment()) == 0);
  bool rmode64 = SysInfo::ExecutablePagesAbove2GB();
  if (rmode64 && size % kMegaByte == 0) {
     void * reservation = anon_mmap(OS::GetRandomMmapAddr(),
                                    size);
     if (reservation == MAP_FAILED) return;
     address_ = reservation;
     size_ = size;
     reservation_ = address_;
     return;
  }

  size_t request_size = RoundUp(size + alignment,
                                static_cast<intptr_t>(OS::AllocateAlignment()));

  void* reservation = anon_mmap(hint,
                           request_size);
  if (reservation == MAP_FAILED) {
      request_size = RoundUp(size + alignment,
                             static_cast<intptr_t>(kMegaByte));
      reservation = anon_mmap(hint, request_size);
      if (reservation == MAP_FAILED) 
         return;
    uint8_t * base = static_cast<uint8_t *>(reservation);
    uint8_t * aligned_base = RoundUp(base, alignment);
    DCHECK_LE(base, aligned_base);
    address_ = aligned_base;
    size = request_size;
    reservation_ = reservation;
    return;
  }
  uint8_t* base = static_cast<uint8_t*>(reservation);
  uint8_t* aligned_base = RoundUp(base, alignment);
  DCHECK_LE(base, aligned_base);

  // Unmap extra memory reserved before and after the desired block.
  if (aligned_base != base) {
    size_t prefix_size = static_cast<size_t>(aligned_base - base);
    OS::Free(base, prefix_size);
    request_size -= prefix_size;
  }

  size_t aligned_size = RoundUp(size, OS::AllocateAlignment());
  DCHECK_LE(aligned_size, request_size);

  if (aligned_size != request_size) {
    size_t suffix_size = request_size - aligned_size;
    OS::Free(aligned_base + aligned_size, suffix_size);
    request_size -= suffix_size;
  }

  DCHECK(aligned_size == request_size);

  address_ = static_cast<void*>(aligned_base);
  size_ = aligned_size;
  reservation_ = reservation;
#if defined(LEAK_SANITIZER)
  __lsan_register_root_region(address_, size_);
#endif
}


VirtualMemory::~VirtualMemory() {
  if (IsReserved()) {
    bool result = ReleaseRegion(reservation_, size());
    DCHECK(result);
    USE(result);
  }
}

/*
bool VirtualMemory::IsReserved() {
  return address_ != NULL;
}
*/

void VirtualMemory::Reset() {
  address_ = NULL;
  size_ = 0;
}


bool VirtualMemory::Commit(void* address, size_t size, bool is_executable) {
  return CommitRegion(address, size, is_executable);
}


bool VirtualMemory::Uncommit(void* address, size_t size) {
  return UncommitRegion(address, size);
}


bool VirtualMemory::Guard(void* address) {
  OS::Guard(address, OS::CommitPageSize());
  return true;
}


void* VirtualMemory::ReserveRegion(size_t size, void * hint) {
  void* result = anon_mmap(hint,
                      size);

  if (result == MAP_FAILED) return NULL;
#if defined(LEAK_SANITIZER)
  __lsan_register_root_region(result, size);
#endif
  return result;
}


bool VirtualMemory::CommitRegion(void* base, size_t size, bool is_executable) {
 /*mprotect can not be called on pages allocated with
   STORAGE OBTAIN, for now we will leave this operation as a
   NOP..might need to use CHANGEKEY macro to implement something
   akin to mprotect in the future*/
   return true;
}


bool VirtualMemory::UncommitRegion(void* base, size_t size) {
 /*mprotect can not be called on pages allocated with
   STORAGE OBTAIN, for now we will leave this operation as a
   NOP..might need to use CHANGEKEY macro to implement something
   akin to mprotect in the future*/
   return true;
}

bool VirtualMemory::ReleaseRegion(void* base, size_t size) {
#if defined(LEAK_SANITIZER)
  __lsan_unregister_root_region(base, size);
#endif
  return anon_munmap(base, size) == 0;
}

bool VirtualMemory::ReleasePartialRegion(void * base, size_t size,
                                         void * free_start, size_t free_size) {
  return anon_munmap(free_start, free_size) == 0;
}

bool VirtualMemory::HasLazyCommits() {
  return true;
}


inline int GetFirstFlagFrom(const char* format_e, int start = 0) {
  int flag_pos = start;
  // find the first flag
  for (; format_e[flag_pos] != '\0' && format_e[flag_pos] != '%'; flag_pos++);
  return flag_pos;
}


FILE* OS::FOpenASCII(const char* path_a, const char* mode_a) {
  int path_len = strlen(path_a);
  int mode_len = strlen(mode_a);
  char *path = new char[path_len + 1];
  char *mode = new char[mode_len + 1];
  memmove(path, path_a, path_len + 1);
  memmove(mode, mode_a, mode_len + 1);
  __a2e_s(path);
  __a2e_s(mode);
  FILE* file = fopen(path, mode);
  delete[] path, mode;
  if (file == NULL) return NULL;
  struct stat file_stat;
  if (fstat(fileno(file), &file_stat) != 0) return NULL;
  bool is_regular_file = ((file_stat.st_mode & S_IFREG) != 0);
  if (is_regular_file) return file;
  fclose(file);
  return NULL;
}


void OS::VFPrintASCII(FILE* out, const char* format_a, va_list args) {
  size_t format_len = strlen(format_a);
  char buffer_e[format_len + 1];
  char * format_e = buffer_e;
  memcpy(format_e, format_a, format_len + 1);
  __a2e_s(format_e);
  int first_flag = GetFirstFlagFrom(format_e);
  if (first_flag > 0)
    OS::FPrint(out, "%.*s", first_flag, format_e);
  format_e += first_flag;
  if (format_e[0] == '\0') return;

  do {
    int next_flag = GetFirstFlagFrom(format_e, 1);
    char tmp = format_e[next_flag];
    format_e[next_flag] = '\0';
    char flag = format_e[1];
    if (flag == 's') {
      // convert arg
      char * str = va_arg(args, char *);
      size_t str_len = strlen(str);
      char str_e[str_len + 1];
      memcpy(str_e, str, str_len + 1);
      __a2e_s(str_e);
      OS::FPrint(out, format_e, str_e);
    } else if (flag == 'd') {
      int num = va_arg(args, int);
      OS::FPrint(out, format_e, num);
    } else if (flag == 'c') {
      OS::FPrint(out, format_e, Ascii2Ebcdic(va_arg(args, char)));
    } else {
      OS::VFPrint(out, format_e, args);
    }
    format_e[next_flag] = tmp;
    format_e += next_flag;
  } while (format_e[0] != '\0');
}


int OS::SNPrintFASCII(char* str, int length, const char* format, ...) {
  va_list args;
  va_start(args, format);
  int result = VSNPrintFASCII(str, length, format, args);
  va_end(args);
  return result;
}


int OS::VSNPrintFASCII(char* str,
                  int length,
                  const char* format,
                  va_list args) {
  int n = VSNPrintFASCII(str, length, format, args);
  if (n < 0 || n >= length) {
    // If the length is zero, the assignment fails.
    if (length > 0)
      str[length - 1] = '\x0';
    return -1;
  } else {
    return n;
  }
}

/*
int OS::SNPrintFASCII(Vector<char> str, const char* format, ...) {
  va_list args;
  va_start(args, format);
  int result = VSNPrintFASCII(str, format, args);
  va_end(args);
  return result;
}


int OS::VSNPrintFASCII(Vector<char> str, const char* format, va_list args) {
  return OS::VSNPrintFASCII(str.start(), str.length(), format, args);
}
*/

void OS::FPrintASCII(FILE* out, const char* format_a, ...) {
  va_list args;
  va_start(args, format_a);
  OS::VFPrintASCII(out, format_a, args);
  va_end(args);
}


void OS::PrintASCII(const char* format_a, ...) {
  va_list args;
  va_start(args, format_a);
  OS::VFPrintASCII(stdout, format_a, args);
  va_end(args);
}
} }  // namespace v8::base
