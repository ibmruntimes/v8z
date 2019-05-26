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
    char *ma = getenv("__MEM_ACCOUNT");
    if (ma && 0 == strcmp("1", ma)) {
      res = 1;
    }
  }
  return res;
}

static int gettcbtoken(char *out, int type) {
  typedef struct token_parm {
    char token[16];
    char *__ptr32 ascb;
    char type;
    char reserved[3];
  } token_parm_t;
  token_parm_t *tt = (token_parm_t *)__malloc31(sizeof(token_parm_t));
  memset(tt, 0, sizeof(token_parm_t));
  tt->type = type;
  long workreg;
  __asm(" L %0,16(0,0) \n"
        " L %0,772(%0,0) \n"
        " L %0,212(%0,0) \n"
        " PC 0(%0) \n"
        : "=NR:r15"(workreg) // also return code
        : "NR:r1"(tt)
        : );
  memcpy(out, (char *)tt, 16);
  free(tt);
  return workreg;
}

struct iarv64parm {
  unsigned char xversion __attribute__((__aligned__(16))); //    0
  unsigned char xrequest;                                  //    1
  unsigned xmotknsource_system : 1;                        //    2
  unsigned xmotkncreator_system : 1;                       //    2(1)
  unsigned xmatch_motoken : 1;                             //    2(2)
  unsigned xflags0_rsvd1 : 5;                              //    2(3)
  unsigned char xkey;                                      //    3
  unsigned keyused_key : 1;                                //    4
  unsigned keyused_usertkn : 1;                            //    4(1)
  unsigned keyused_ttoken : 1;                             //    4(2)
  unsigned keyused_convertstart : 1;                       //    4(3)
  unsigned keyused_guardsize64 : 1;                        //    4(4)
  unsigned keyused_convertsize64 : 1;                      //    4(5)
  unsigned keyused_motkn : 1;                              //    4(6)
  unsigned keyused_ownerjobname : 1;                       //    4(7)
  unsigned xcond_yes : 1;                                  //    5
  unsigned xfprot_no : 1;                                  //    5(1)
  unsigned xcontrol_auth : 1;                              //    5(2)
  unsigned xguardloc_high : 1;                             //    5(3)
  unsigned xchangeaccess_global : 1;                       //    5(4)
  unsigned xpageframesize_1meg : 1;                        //    5(5)
  unsigned xpageframesize_max : 1;                         //    5(6)
  unsigned xpageframesize_all : 1;                         //    5(7)
  unsigned xmatch_usertoken : 1;                           //    6
  unsigned xaffinity_system : 1;                           //    6(1)
  unsigned xuse2gto32g_yes : 1;                            //    6(2)
  unsigned xowner_no : 1;                                  //    6(3)
  unsigned xv64select_no : 1;                              //    6(4)
  unsigned xsvcdumprgn_no : 1;                             //    6(5)
  unsigned xv64shared_no : 1;                              //    6(6)
  unsigned xsvcdumprgn_all : 1;                            //    6(7)
  unsigned xlong_no : 1;                                   //    7
  unsigned xclear_no : 1;                                  //    7(1)
  unsigned xview_readonly : 1;                             //    7(2)
  unsigned xview_sharedwrite : 1;                          //    7(3)
  unsigned xview_hidden : 1;                               //    7(4)
  unsigned xconvert_toguard : 1;                           //    7(5)
  unsigned xconvert_fromguard : 1;                         //    7(6)
  unsigned xkeepreal_no : 1;                               //    7(7)
  unsigned long long xsegments;                            //    8
  unsigned char xttoken[16];                               //   16
  unsigned long long xusertkn;                             //   32
  void *xorigin;                                           //   40
  void *xranglist;                                         //   48
  void *xmemobjstart;                                      //   56
  unsigned xguardsize;                                     //   64
  unsigned xconvertsize;                                   //   68
  unsigned xaletvalue;                                     //   72
  int xnumrange;                                           //   76
  void *__ptr32 xv64listptr;                               //   80
  unsigned xv64listlength;                                 //   84
  unsigned long long xconvertstart;                        //   88
  unsigned long long xconvertsize64;                       //   96
  unsigned long long xguardsize64;                         //  104
  char xusertoken[8];                                      //  112
  unsigned char xdumppriority;                             //  120
  unsigned xdumpprotocol_yes : 1;                          //  121
  unsigned xorder_dumppriority : 1;                        //  121(1)
  unsigned xtype_pageable : 1;                             //  121(2)
  unsigned xtype_dref : 1;                                 //  121(3)
  unsigned xownercom_home : 1;                             //  121(4)
  unsigned xownercom_primary : 1;                          //  121(5)
  unsigned xownercom_system : 1;                           //  121(6)
  unsigned xownercom_byasid : 1;                           //  121(7)
  unsigned xv64common_no : 1;                              //  122
  unsigned xmemlimit_no : 1;                               //  122(1)
  unsigned xdetachfixed_yes : 1;                           //  122(2)
  unsigned xdoauthchecks_yes : 1;                          //  122(3)
  unsigned xlocalsysarea_yes : 1;                          //  122(4)
  unsigned xamountsize_4k : 1;                             //  122(5)
  unsigned xamountsize_1meg : 1;                           //  122(6)
  unsigned xmemlimit_cond : 1;                             //  122(7)
  unsigned keyused_dump : 1;                               //  123
  unsigned keyused_optionvalue : 1;                        //  123(1)
  unsigned keyused_svcdumprgn : 1;                         //  123(2)
  unsigned xattribute_defs : 1;                            //  123(3)
  unsigned xattribute_ownergone : 1;                       //  123(4)
  unsigned xattribute_notownergone : 1;                    //  123(5)
  unsigned xtrackinfo_yes : 1;                             //  123(6)
  unsigned xunlocked_yes : 1;                              //  123(7)
  unsigned char xdump;                                     //  124
  unsigned xpageframesize_pageable1meg : 1;                //  125
  unsigned xpageframesize_dref1meg : 1;                    //  125(1)
  unsigned xsadmp_yes : 1;                                 //  125(2)
  unsigned xsadmp_no : 1;                                  //  125(3)
  unsigned xuse2gto64g_yes : 1;                            //  125(4)
  unsigned xdiscardpages_yes : 1;                          //  125(5)
  unsigned xexecutable_yes : 1;                            //  125(6)
  unsigned xexecutable_no : 1;                             //  125(7)
  unsigned short xownerasid;                               //  126
  unsigned char xoptionvalue;                              //  128
  unsigned char xrsv0001[8];                               //  129
  unsigned char xownerjobname[8];                          //  137
  unsigned char xrsv0004[7];                               //  145
  void *xdmapagetable;                                     //  152
  unsigned long long xunits;                               //  160
  unsigned keyused_units : 1;                              //  168
  unsigned xunitsize_1m : 1;                               //  168(1)
  unsigned xunitsize_2g : 1;                               //  168(2)
  unsigned xpageframesize_1m : 1;                          //  168(3)
  unsigned xpageframesize_2g : 1;                          //  168(4)
  unsigned xtype_fixed : 1;                                //  168(5)
  unsigned xflags9_rsvd1 : 2;                              //  168(6)
  unsigned char xrsv0005[7];                               //  169
};
static long long __iarv64(void *parm, void **ptr, long long *reason_code_ptr) {
  long long rc;
  long long reason;
  __asm volatile(" lgr 1,%3 \n"
                 " llgtr 14,14 \n"
                 " l 14,16(0,0) \n"
                 " l 14,772(14,0) \n"
                 " l 14,208(14,0) \n"
                 " la 15,14 \n"
                 " or 14,15 \n"
                 " pc 0(14) \n"
                 " stg 1,%0 \n"
                 " stg 15,%1 \n"
                 " stg 0,%2 \n"
                 : "=m"(*ptr), "=m"(rc), "=m"(reason)
                 : "r"(parm)
                 : "r0", "r1", "r14", "r15");
  if (rc != 0 && reason_code_ptr != 0) {
    *reason_code_ptr = reason;
  }
  return rc;
}

static void *__iarv64_alloc(int segs, const char *token) {
  void *ptr = 0;
  long long rc, reason;
  struct iarv64parm parm __attribute__((__aligned__(16)));
  memset(&parm, 0, sizeof(parm));
  parm.xversion = 5;
  parm.xrequest = 1;
  parm.xcond_yes = 1;
  parm.xsegments = segs;
  parm.xorigin = 0;
  parm.xdumppriority = 99;
  parm.xtype_pageable = 1;
  parm.xdump = 32;
  parm.xsadmp_no = 1;
  parm.xpageframesize_pageable1meg = 1;
  parm.xuse2gto64g_yes = 1;
  parm.xexecutable_yes = 1;
  parm.keyused_ttoken = 1;
  memcpy(&parm.xttoken, token, 16);
  rc = __iarv64(&parm, &ptr, &reason);
  if (mem_account())
    fprintf(stderr, "__iav64_alloc: pid %d tid %d ptr=%p size=%lu rc=%lld\n",
            getpid(), (int)(pthread_self().__ & 0x7fffffff), parm.xorigin,
            (unsigned long)(segs * 1024 * 1024), rc);
  if (rc == 0) {
    ptr = parm.xorigin;
  }
  return ptr;
}

static int __iarv64_free(void *ptr, const char *token) {
  long long rc, reason;
  void *org = ptr;
  struct iarv64parm parm __attribute__((__aligned__(16)));
  memset(&parm, 0, sizeof(parm));
  parm.xversion = 5;
  parm.xrequest = 3;
  parm.xcond_yes = 1;
  parm.xsadmp_no = 1;
  parm.xmemobjstart = ptr;
  parm.keyused_ttoken = 1;
  memcpy(&parm.xttoken, token, 16);
  rc = __iarv64(&parm, &ptr, &reason);
  if (mem_account())
    fprintf(stderr, "__iarv64_free pid %d tid %d ptr=%p rc=%lld\n", getpid(),
            (int)(pthread_self().__ & 0x7fffffff), org, rc);
  return rc;
}

static void* __mo_alloc(int segs) {
  __mopl_t moparm;
  void* p = 0;
  memset(&moparm, 0, sizeof(moparm));
  moparm.__mopldumppriority = __MO_DUMP_PRIORITY_STACK + 5;
  moparm.__moplrequestsize = segs;
  moparm.__moplgetstorflags = __MOPL_PAGEFRAMESIZE_PAGEABLE1MEG;
  int rc = __moservices(__MO_GETSTOR, sizeof(moparm), &moparm, &p);
  if (rc == 0 && moparm.__mopl_iarv64_rc == 0) {
    return p;
  }
  perror("__moservices GETSTOR");
  return 0;
}

static int __mo_free(void* ptr) {
  int rc = __moservices(__MO_DETACH, 0, NULL, &ptr);
  if (rc) {
    perror("__moservices DETACH");
  }
  return rc;
}

typedef unsigned long value_type;
typedef unsigned long key_type;

struct __hash_func {
  size_t operator()(const key_type &k) const {
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
  std::mutex access_lock;
  char tcbtoken[16];
  unsigned short asid;

 public:
  __Cache() {
    gettcbtoken(tcbtoken, 3);
    asid = ((unsigned short*)(*(char* __ptr32*)(0x224)))[18];
  }
  void addptr(const void* ptr, size_t v) {
    unsigned long k = (unsigned long)ptr;
    std::lock_guard<std::mutex> guard(access_lock);
    cache[k] = v;
    if (mem_account()) fprintf(stderr, "ADDED: @%lx size %lu\n", k, v);
  }
#if defined(__USE_IARV64)
  void* alloc_seg(int segs) {
    std::lock_guard<std::mutex> guard(access_lock);
    unsigned short this_asid =
        ((unsigned short*)(*(char* __ptr32*)(0x224)))[18];
    if (asid != this_asid) {
      // a fork occurred
      asid = this_asid;
      gettcbtoken(tcbtoken, 3);
    }
    void* p = __iarv64_alloc(segs, tcbtoken);
    if (p) {
      unsigned long k = (unsigned long)p;
      cache[k] = segs * 1024 * 1024;
      if (mem_account())
        fprintf(stderr, "ADDED:@%lx size %lu RMODE64\n", k,
                (size_t)(segs * 1024 * 1024));
    }
    return p;
  }
  int free_seg(void* ptr) {
    unsigned long k = (unsigned long)ptr;
    std::lock_guard<std::mutex> guard(access_lock);
    unsigned short this_asid =
        ((unsigned short*)(*(char* __ptr32*)(0x224)))[18];
    if (asid != this_asid) {
      // a fork occurred
      asid = this_asid;
      gettcbtoken(tcbtoken, 3);
    }
    int rc = __iarv64_free(ptr, tcbtoken);
    if (rc == 0) {
      cursor_t c = cache.find(k);
      if (c != cache.end()) {
        cache.erase(c);
      }
    }
    return rc;
  }
#else
  void* alloc_seg(int segs) {
    void* p = __mo_alloc(segs);
    std::lock_guard<std::mutex> guard(access_lock);
    if (p) {
      unsigned long k = (unsigned long)p;
      cache[k] = segs * 1024 * 1024;
      if (mem_account())
        fprintf(stderr, "ADDED:@%lx size %lu RMODE64\n", k,
                (size_t)(segs * 1024 * 1024));
    }
    return p;
  }
  int free_seg(void* ptr) {
    unsigned long k = (unsigned long)ptr;
    int rc = __mo_free(ptr);
    std::lock_guard<std::mutex> guard(access_lock);
    if (rc == 0) {
      cursor_t c = cache.find(k);
      if (c != cache.end()) {
        cache.erase(c);
      }
    }
    return rc;
  }
#endif
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
    cursor_t c = cache.find(k);
    if (c != cache.end()) {
      if (0 != (k & 0xffffffff80000000UL))
        return 1;
      else
        return 0;
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

static void *anon_mmap_inner(void *addr, size_t len, bool *is_above_bar) {
  int retcode;
  if (len % kMegaByte == 0) {
    size_t request_size = len / kMegaByte;
    *is_above_bar = true;
    void *p = alloc_info.alloc_seg(request_size);
    if (p)
      return p;
    else
      return MAP_FAILED;
  } else {
    char *p;
#if defined(__64BIT__)
    __asm(" SYSSTATE ARCHLVL=2,AMODE64=YES\x15"
          " STORAGE OBTAIN,LENGTH=(%2),BNDRY=PAGE,COND=YES,ADDR=(%0),RTCD=(%1),"
          "LOC=(31,64)\x15"
#if defined(__clang__)
          : "=NR:r1"(p), "=NR:r15"(retcode)
          : "NR:r0"(len)
          : "r0", "r1", "r14", "r15");
#else
          : "=r"(p), "=r"(retcode)
          : "r"(len)
          : "r0", "r1", "r14", "r15");
#endif
#else
    __asm(" SYSSTATE ARCHLVL=2\x15"
          " STORAGE "
          "OBTAIN,LENGTH=(%2),BNDRY=PAGE,COND=YES,ADDR=(%0),RTCD=(%1)\x15"
#if defined(__clang__)
          : "=NR:r1"(p), "=NR:r15"(retcode)
          : "NR:r0"(len)
          : "r0", "r1", "r14", "r15");
#else
          : "=r"(p), "=r"(retcode)
          : "r"(len)
          : "r0", "r1", "r14", "r15");
#endif
#endif
    *is_above_bar = false;
    if (retcode == 0) {
      alloc_info.addptr(p, len);
      return p;
    }
    return MAP_FAILED;
  }
}

static int anon_munmap_inner(void *addr, size_t len, bool is_above_bar) {
  int retcode;
  if (is_above_bar) {
    return alloc_info.free_seg(addr);
  } else {
#if defined(__64BIT__)
    __asm(" SYSSTATE ARCHLVL=2,AMODE64=YES\x15"
          " STORAGE RELEASE,LENGTH=(%2),ADDR=(%1),RTCD=(%0),COND=YES\x15"
#if defined(__clang__)
          : "=NR:r15"(retcode)
          : "NR:r1"(addr), "NR:r0"(len)
          : "r0", "r1", "r14", "r15");
#else
          : "=r"(retcode)
          : "r"(addr), "r"(len)
          : "r0", "r1", "r14", "r15");
#endif
#else
    __asm(" SYSSTATE ARCHLVL=2\x15"
          " STORAGE RELEASE,LENGTH=(%2),ADDR=(%1),RTCD=(%0),COND=YES\x15"
#if defined(__clang__)
          : "=NR:r15"(retcode)
          : "NR:r1"(addr), "NR:r0"(len)
          : "r0", "r1", "r14", "r15");
#else
          : "=r"(retcode)
          : "r"(addr), "r"(len)
          : "r0", "r1", "r14", "r15");
#endif
#endif
    if (0 == retcode)
      alloc_info.freeptr(addr);
  }
  return retcode;
}

static void *anon_mmap(void *_, size_t len, bool *is_above_bar) {
  void *ret = anon_mmap_inner(_, len, is_above_bar);
  if (ret == MAP_FAILED) {
    if (mem_account())
      fprintf(stderr, "Error: anon_mmap request size %zu failed\n", len);
    return ret;
  }
  return ret;
}

static int anon_munmap(void *addr, size_t len) {
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
    return 0;
  } else {
    if (mem_account())
      fprintf(stderr, "Error: attempt to free %p size %d (not allocated)\n",
              addr, (int)len);
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

  if (size % kMegaByte == 0) {
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
