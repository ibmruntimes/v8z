// Copyright 2012 the V8 project authors. All rights reserved.
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.

// Platform-specific code for zOS/Unix goes here. For the POSIX-compatible
// parts, the implementation is in platform-posix.cc.
#if !defined(_AE_BIMODAL)
#define _AE_BIMODAL 1
#endif

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

#include <mutex>
#include <unordered_map>

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

//------------------------------------------accounting for memory allocation

#pragma convert("IBM-1047")
static int mem_account(void) {
  static int res = -1;
  if (-1 == res) {
    res = 0;
    char* ma = getenv("__MEM_ACCOUNT");
    if (ma && 0 == strcmp("1", ma)) {
      res = 1;
    }
  }
  return res;
}

typedef unsigned long value_type;
typedef unsigned long key_type;

struct __hash_func {
  size_t operator()(const key_type& k) const {
    int s = 0;
    key_type n = k;
    while (0 == (n & 1) && s < (sizeof(key_type) - 1)) {
      n = n >> 1;
      ++s;
    }
    return s + (n * 0x744dcf5364d7d667UL);
  }
};

typedef std::unordered_map<key_type, value_type, __hash_func>::const_iterator
    cursor_t;
typedef std::unordered_map<key_type, bool, __hash_func>::const_iterator
    rmode_cursor_t;

class __Cache {
  std::unordered_map<key_type, value_type, __hash_func> cache;
  std::unordered_map<key_type, bool, __hash_func> rmode_cache;
  std::mutex access_lock;

 public:
  void addptr(const void* ptr, size_t v) {
    unsigned long k = (unsigned long)ptr;
    std::lock_guard<std::mutex> guard(access_lock);
    cache[k] = v;
    if (mem_account()) fprintf(stderr, "ADDED: @%lx size %lu\n", k, v);
  }
  void addptr_rmode64(const void* ptr, bool rmode64) {
    unsigned long k = (unsigned long)ptr;
    std::lock_guard<std::mutex> guard(access_lock);
    rmode_cache[k] = rmode64;
    if (mem_account()) fprintf(stderr, "ADDED: RMODE64\n");
  }
  int is_exist_ptr(const void* ptr) {
    unsigned long k = (unsigned long)ptr;
    std::lock_guard<std::mutex> guard(access_lock);
    cursor_t c = cache.find(k);
    if (c != cache.end()) {
      return 1;
    }
    return 0;
  }
  int is_rmode64(const void* ptr) {
    unsigned long k = (unsigned long)ptr;
    std::lock_guard<std::mutex> guard(access_lock);
    rmode_cursor_t c = rmode_cache.find(k);
    if (c != rmode_cache.end()) {
      return 1;
    }
    return 0;
  }
  void show(void) {
    std::lock_guard<std::mutex> guard(access_lock);
    if (mem_account())
      for (cursor_t it = cache.begin(); it != cache.end(); ++it) {
        fprintf(stderr, "LIST: @%lx size %lu\n", it->first, it->second);
      }
  }
  void freeptr(const void* ptr) {
    unsigned long k = (unsigned long)ptr;
    std::lock_guard<std::mutex> guard(access_lock);
    cursor_t c = cache.find(k);
    if (c != cache.end()) {
      cache.erase(c);
    }
  }
  ~__Cache() {
    std::lock_guard<std::mutex> guard(access_lock);
    if (mem_account())
      for (cursor_t it = cache.begin(); it != cache.end(); ++it) {
        fprintf(stderr,
                "Error: DEBRIS (allocated but never free'd): @%lx size %lu\n",
                it->first, it->second);
      }
  }
};

static __Cache alloc_info;

static void * anon_mmap_inner(void * addr, size_t len, bool * is_above_bar) {
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
     *is_above_bar = true;
     alloc_info.addptr_rmode64(p, *is_above_bar);
     return (retcode == 0) ? p : MAP_FAILED;
   } else {
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
    *is_above_bar = false;
    return (retcode == 0) ? p : MAP_FAILED;
   }
}


static int anon_munmap_inner(void * addr, size_t len, bool is_above_bar) {
   int retcode;
   if (is_above_bar) {
     retcode = __moservices(__MO_DETACH,0,NULL,&addr);
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


static void* anon_mmap(void* _, size_t len, bool* is_above_bar) {
  void* ret = anon_mmap_inner(_, len, is_above_bar);
  if (ret == MAP_FAILED) {
    if (mem_account())
      fprintf(stderr, "Error: anon_mmap request size %zu failed\n", len);
    return ret;
  }
  alloc_info.addptr(ret, (int)len);
  if (mem_account()) fprintf(stderr, "Allocated @%p size %d\n", ret, (int)len);
  return ret;
}

static int anon_munmap(void* addr, size_t len) {
  if (alloc_info.is_exist_ptr(addr)) {
    if (mem_account())
      fprintf(stderr, "Address found, attempt to free @%p size %d\n", addr,
              (int)len);
    int rc = anon_munmap_inner(addr, len, alloc_info.is_rmode64(addr));
    if (rc != 0) {
      if (mem_account())
        fprintf(stderr, "Error: anon_munmap @%p size %zu failed\n", addr, len);
      return rc;
    }
    alloc_info.freeptr(addr);
    return 0;
  } else {
    if (mem_account())
      fprintf(stderr, "Error: attempt to free %p size %d (not allocated)\n", addr,
              (int)len);
    return 0;
  }
}
#pragma convert(pop)
//----------------------------------------------------------------------------------------------

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
  bool rmode;
  void * mbase = static_cast<void*>(anon_mmap(OS::GetRandomMmapAddr(),
                                    sizeof(char) * msize, &rmode));
  if (mbase == MAP_FAILED) return NULL;
  *allocated = msize;
  return mbase;
}

std::vector<OS::SharedLibraryAddress> OS::GetSharedLibraryAddresses() {
  std::vector<SharedLibraryAddress> result;
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
  munmap(addr, size);
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

  bool is_above_bar = false;

  // Memory pages with 1MB alignment will be allocated using __moservices
  bool rmode64 = SysInfo::ExecutablePagesAbove2GB();
  if (rmode64 && size % kMegaByte == 0) {
     void * reservation = anon_mmap(hint,
                                    size, &is_above_bar);
     if (reservation == MAP_FAILED) return;
     address_ = reservation;
     size_ = size;
     reservation_ = address_;
     return;
  }

  size_t request_size = RoundUp(size + alignment,
                                static_cast<intptr_t>(OS::AllocateAlignment()));

  void* reservation = anon_mmap(hint,
                           request_size, &is_above_bar);
  if (reservation == MAP_FAILED) {
      request_size = RoundUp(size + alignment,
                             static_cast<intptr_t>(kMegaByte));
      reservation = anon_mmap(hint, request_size, &is_above_bar);
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

  if (!is_above_bar) {
	uint8_t* base = static_cast<uint8_t*>(reservation);
	uint8_t* aligned_base = RoundUp(base, alignment);
	DCHECK_LE(base, aligned_base);

	// Unmap extra memory reserved before and after the desired block.
	/*if (aligned_base != base) {
		size_t prefix_size = static_cast<size_t>(aligned_base - base);
		OS::Free(base, prefix_size);
		request_size -= prefix_size;
	}*/

	size_t aligned_size = RoundUp(size, OS::AllocateAlignment());
	DCHECK_LE(aligned_size, request_size);

	if (aligned_size != request_size) {
		size_t suffix_size = request_size - aligned_size;
		if ((aligned_base + aligned_size) == reservation)
			OS::Free(aligned_base + aligned_size, suffix_size);
		request_size -= suffix_size;
	}

	DCHECK(aligned_size == request_size);
	address_ = static_cast<void*>(aligned_base);
	size_ = aligned_size;
  } else {
	address_ = static_cast<void*>(reservation);
	size_ = request_size;
  }

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


//bool VirtualMemory::IsReserved() {
//  return reservation_ != NULL;
//}


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
  bool is_above_bar;
  void* result = anon_mmap(hint,
                      size, &is_above_bar);

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
  //On z/OS we cannot release a partial region, unless it is from the start and using rmode32
  if (alloc_info.is_rmode64(base) && base == free_start && size == free_size)  {
  	return anon_munmap(base, size) == 0;
  }
  else if (!alloc_info.is_rmode64(base) && base == free_start) {
  	return anon_munmap(base, free_size) == 0;
  }
  return true;
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
