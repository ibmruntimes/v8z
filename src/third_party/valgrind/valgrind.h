/* -*- c -*-
   ----------------------------------------------------------------

   Notice that the following BSD-style license applies to this one
   file (valgrind.h) only.  The rest of Valgrind is licensed under the
   terms of the GNU General Public License, version 2, unless
   otherwise indicated.  See the COPYING file in the source
   distribution for details.

   ----------------------------------------------------------------

   This file is part of Valgrind, a dynamic binary instrumentation
   framework.

   Copyright (C) 2000-2010 Julian Seward.  All rights reserved.

   Redistribution and use in source and binary forms, with or without
   modification, are permitted provided that the following conditions
   are met:

   1. Redistributions of source code must retain the above copyright
      notice, this list of conditions and the following disclaimer.

   2. The origin of this software must not be misrepresented; you must
      not claim that you wrote the original software.  If you use this
      software in a product, an acknowledgment in the product
      documentation would be appreciated but is not required.

   3. Altered source versions must be plainly marked as such, and must
      not be misrepresented as being the original software.

   4. The name of the author may not be used to endorse or promote
      products derived from this software without specific prior written
      permission.

   THIS SOFTWARE IS PROVIDED BY THE AUTHOR ``AS IS'' AND ANY EXPRESS
   OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
   WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
   ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY
   DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
   DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE
   GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
   INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
   WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
   NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
   SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

   ----------------------------------------------------------------

   Notice that the above BSD-style license applies to this one file
   (valgrind.h) only.  The entire rest of Valgrind is licensed under
   the terms of the GNU General Public License, version 2.  See the
   COPYING file in the source distribution for details.

   ----------------------------------------------------------------
*/


/* This file is for inclusion into client (your!) code.

   You can use these macros to manipulate and query Valgrind's
   execution inside your own programs.

   The resulting executables will still run without Valgrind, just a
   little bit more slowly than they otherwise would, but otherwise
   unchanged.  When not running on valgrind, each client request
   consumes very few (eg. 7) instructions, so the resulting performance
   loss is negligible unless you plan to execute client requests
   millions of times per second.  Nevertheless, if that is still a
   problem, you can compile with the NVALGRIND symbol defined (gcc
   -DNVALGRIND) so that client requests are not even compiled in.  */

#ifndef __VALGRIND_H
#define __VALGRIND_H


/* ------------------------------------------------------------------ */
/* VERSION NUMBER OF VALGRIND                                         */
/* ------------------------------------------------------------------ */

/* Specify Valgrind's version number, so that user code can
   conditionally compile based on our version number.  Note that these
   were introduced at version 3.6 and so do not exist in version 3.5
   or earlier.  The recommended way to use them to check for "version
   X.Y or later" is (eg)

#if defined(__VALGRIND_MAJOR__) && defined(__VALGRIND_MINOR__)   \
    && (__VALGRIND_MAJOR__ > 3                                   \
        || (__VALGRIND_MAJOR__ == 3 && __VALGRIND_MINOR__ >= 6))
*/
#define __VALGRIND_MAJOR__    3
#define __VALGRIND_MINOR__    6


#include <stdarg.h>
#include <stdint.h>

/* Nb: this file might be included in a file compiled with -ansi.  So
   we can't use C++ style "//" comments nor the "asm" keyword (instead
   use "__asm__"). */

/* Derive some tags indicating what the target platform is.  Note
   that in this file we're using the compiler's CPP symbols for
   identifying architectures, which are different to the ones we use
   within the rest of Valgrind.  Note, __powerpc__ is active for both
   32 and 64-bit PPC, whereas __powerpc64__ is only active for the
   latter (on Linux, that is).

   Misc note: how to find out what's predefined in gcc by default:
   gcc -Wp,-dM somefile.c
*/
#undef PLAT_x86_darwin
#undef PLAT_amd64_darwin
#undef PLAT_x86_win32
#undef PLAT_x86_linux
#undef PLAT_amd64_linux
#undef PLAT_ppc32_linux
#undef PLAT_ppc64_linux
#undef PLAT_arm_linux
#undef PLAT_s390x_linux


#if defined(__APPLE__) && defined(__i386__)
#  define PLAT_x86_darwin 1
#elif defined(__APPLE__) && defined(__x86_64__)
#  define PLAT_amd64_darwin 1
#elif defined(__MINGW32__) || defined(__CYGWIN32__) \
      || (defined(_WIN32) && defined(_M_IX86))
#  define PLAT_x86_win32 1
#elif defined(__linux__) && defined(__i386__)
#  define PLAT_x86_linux 1
#elif defined(__linux__) && defined(__x86_64__)
#  define PLAT_amd64_linux 1
#elif defined(__linux__) && defined(__powerpc__) && !defined(__powerpc64__)
#  define PLAT_ppc32_linux 1
#elif defined(__linux__) && defined(__powerpc__) && defined(__powerpc64__)
#  define PLAT_ppc64_linux 1
#elif defined(__linux__) && defined(__arm__)
#  define PLAT_arm_linux 1
#elif defined(__linux__) && defined(__s390__) && defined(__s390x__)
#  define PLAT_s390x_linux 1
#else
/* If we're not compiling for our target platform, don't generate
   any inline asms.  */
#  if !defined(NVALGRIND)
#    define NVALGRIND 1
#  endif
#endif


/* ------------------------------------------------------------------ */
/* ARCHITECTURE SPECIFICS for SPECIAL INSTRUCTIONS.  There is nothing */
/* in here of use to end-users -- skip to the next section.           */
/* ------------------------------------------------------------------ */

/*
 * VALGRIND_DO_CLIENT_REQUEST(): a statement that invokes a Valgrind client
 * request. Accepts both pointers and integers as arguments.
 *
 * VALGRIND_DO_CLIENT_REQUEST_EXPR(): a C expression that invokes a Valgrind
 * client request and whose value equals the client request result. Accepts
 * both pointers and integers as arguments.
 */

#define VALGRIND_DO_CLIENT_REQUEST(_zzq_rlval, _zzq_default,            \
                                   _zzq_request, _zzq_arg1, _zzq_arg2,  \
                                   _zzq_arg3, _zzq_arg4, _zzq_arg5)     \
  { (_zzq_rlval) = VALGRIND_DO_CLIENT_REQUEST_EXPR((_zzq_default),      \
                        (_zzq_request), (_zzq_arg1), (_zzq_arg2),       \
                        (_zzq_arg3), (_zzq_arg4), (_zzq_arg5)); }

#if defined(NVALGRIND)

/* Define NVALGRIND to completely remove the Valgrind magic sequence
   from the compiled code (analogous to NDEBUG's effects on
   assert()) */
#define VALGRIND_DO_CLIENT_REQUEST_EXPR(                          \
        _zzq_default, _zzq_request,                               \
        _zzq_arg1, _zzq_arg2, _zzq_arg3, _zzq_arg4, _zzq_arg5)    \
      (_zzq_default)

#else  /* ! NVALGRIND */

/* The following defines the magic code sequences which the JITter
   spots and handles magically.  Don't look too closely at them as
   they will rot your brain.

   The assembly code sequences for all architectures is in this one
   file.  This is because this file must be stand-alone, and we don't
   want to have multiple files.

   For VALGRIND_DO_CLIENT_REQUEST, we must ensure that the default
   value gets put in the return slot, so that everything works when
   this is executed not under Valgrind.  Args are passed in a memory
   block, and so there's no intrinsic limit to the number that could
   be passed, but it's currently five.

   The macro args are:
      _zzq_rlval    result lvalue
      _zzq_default  default value (result returned when running on real CPU)
      _zzq_request  request code
      _zzq_arg1..5  request params

   The other two macros are used to support function wrapping, and are
   a lot simpler.  VALGRIND_GET_NR_CONTEXT returns the value of the
   guest's NRADDR pseudo-register and whatever other information is
   needed to safely run the call original from the wrapper: on
   ppc64-linux, the R2 value at the divert point is also needed.  This
   information is abstracted into a user-visible type, OrigFn.

   VALGRIND_CALL_NOREDIR_* behaves the same as the following on the
   guest, but guarantees that the branch instruction will not be
   redirected: x86: call *%eax, amd64: call *%rax, ppc32/ppc64:
   branch-and-link-to-r11.  VALGRIND_CALL_NOREDIR is just text, not a
   complete inline asm, since it needs to be combined with more magic
   inline asm stuff to be useful.
*/

/* ------------------------- x86-{linux,darwin} ---------------- */

#if defined(PLAT_x86_linux)  ||  defined(PLAT_x86_darwin)  \
    ||  (defined(PLAT_x86_win32) && defined(__GNUC__))

typedef
   struct {
      unsigned int nraddr; /* where's the code? */
   }
   OrigFn;

#define __SPECIAL_INSTRUCTION_PREAMBLE                            \
                     "\x72\x6f\x6c\x6c\x20\x24\x33\x2c\x20\x20\x25\x6c\x85\x84\x89\x20\x3b\x20\x72\x6f\x6c\x6c\x20\x24\x31\x33\x2c\x20\x25\x6c\x85\x84\x89\xa\x9"      \
                     "\x72\x6f\x6c\x6c\x20\x24\x32\x39\x2c\x20\x25\x6c\x85\x84\x89\x20\x3b\x20\x72\x6f\x6c\x6c\x20\x24\x31\x39\x2c\x20\x25\x6c\x85\x84\x89\xa\x9"

#define VALGRIND_DO_CLIENT_REQUEST_EXPR(                          \
        _zzq_default, _zzq_request,                               \
        _zzq_arg1, _zzq_arg2, _zzq_arg3, _zzq_arg4, _zzq_arg5)    \
  __extension__                                                   \
  ({volatile unsigned int _zzq_args[6];                           \
    volatile unsigned int _zzq_result;                            \
    _zzq_args[0] = (unsigned int)(_zzq_request);                  \
    _zzq_args[1] = (unsigned int)(_zzq_arg1);                     \
    _zzq_args[2] = (unsigned int)(_zzq_arg2);                     \
    _zzq_args[3] = (unsigned int)(_zzq_arg3);                     \
    _zzq_args[4] = (unsigned int)(_zzq_arg4);                     \
    _zzq_args[5] = (unsigned int)(_zzq_arg5);                     \
    __asm__ volatile(__SPECIAL_INSTRUCTION_PREAMBLE               \
                     /* %EDX = client_request ( %EAX ) */         \
                     "\x78\x63\x68\x67\x6c\x20\x25\x6c\x85\x62\x78\x2c\x25\x6c\x85\x62\x78"                          \
                     : "\x3d\x64" (_zzq_result)                         \
                     : "\x61" (&_zzq_args[0]), "\x30" (_zzq_default)    \
                     : "\x63\x63", "\x6d\x65\x6d\x6f\x72\x79"                             \
                    );                                            \
    _zzq_result;                                                  \
  })

#define VALGRIND_GET_NR_CONTEXT(_zzq_rlval)                       \
  { volatile OrigFn* _zzq_orig = &(_zzq_rlval);                   \
    volatile unsigned int __addr;                                 \
    __asm__ volatile(__SPECIAL_INSTRUCTION_PREAMBLE               \
                     /* %EAX = guest_NRADDR */                    \
                     "\x78\x63\x68\x67\x6c\x20\x25\x6c\x85\x83\xa7\x2c\x25\x6c\x85\x83\xa7"                          \
                     : "\x3d\x61" (__addr)                              \
                     :                                            \
                     : "\x63\x63", "\x6d\x65\x6d\x6f\x72\x79"                             \
                    );                                            \
    _zzq_orig->nraddr = __addr;                                   \
  }

#define VALGRIND_CALL_NOREDIR_EAX                                 \
                     __SPECIAL_INSTRUCTION_PREAMBLE               \
                     /* call-noredir *%EAX */                     \
                     "\x78\x63\x68\x67\x6c\x20\x25\x6c\x85\x84\xa7\x2c\x25\x6c\x85\x84\xa7\xa\x9"
#endif /* PLAT_x86_linux || PLAT_x86_darwin || (PLAT_x86_win32 && __GNUC__) */

/* ------------------------- x86-Win32 ------------------------- */

#if defined(PLAT_x86_win32) && !defined(__GNUC__)

typedef
   struct {
      unsigned int nraddr; /* where's the code? */
   }
   OrigFn;

#if defined(_MSC_VER)

#define __SPECIAL_INSTRUCTION_PREAMBLE                            \
                     __asm rol edi, 3  __asm rol edi, 13          \
                     __asm rol edi, 29 __asm rol edi, 19

#define VALGRIND_DO_CLIENT_REQUEST_EXPR(                          \
        _zzq_default, _zzq_request,                               \
        _zzq_arg1, _zzq_arg2, _zzq_arg3, _zzq_arg4, _zzq_arg5)    \
    valgrind_do_client_request_expr((uintptr_t)(_zzq_default),    \
        (uintptr_t)(_zzq_request), (uintptr_t)(_zzq_arg1),        \
        (uintptr_t)(_zzq_arg2), (uintptr_t)(_zzq_arg3),           \
        (uintptr_t)(_zzq_arg4), (uintptr_t)(_zzq_arg5))

static __inline uintptr_t
valgrind_do_client_request_expr(uintptr_t _zzq_default, uintptr_t _zzq_request,
                                uintptr_t _zzq_arg1, uintptr_t _zzq_arg2,
                                uintptr_t _zzq_arg3, uintptr_t _zzq_arg4,
                                uintptr_t _zzq_arg5)
{
    volatile uintptr_t _zzq_args[6];
    volatile unsigned int _zzq_result;
    _zzq_args[0] = (uintptr_t)(_zzq_request);
    _zzq_args[1] = (uintptr_t)(_zzq_arg1);
    _zzq_args[2] = (uintptr_t)(_zzq_arg2);
    _zzq_args[3] = (uintptr_t)(_zzq_arg3);
    _zzq_args[4] = (uintptr_t)(_zzq_arg4);
    _zzq_args[5] = (uintptr_t)(_zzq_arg5);
    __asm { __asm lea eax, _zzq_args __asm mov edx, _zzq_default
            __SPECIAL_INSTRUCTION_PREAMBLE
            /* %EDX = client_request ( %EAX ) */
            __asm xchg ebx,ebx
            __asm mov _zzq_result, edx
    }
    return _zzq_result;
}

#define VALGRIND_GET_NR_CONTEXT(_zzq_rlval)                       \
  { volatile OrigFn* _zzq_orig = &(_zzq_rlval);                   \
    volatile unsigned int __addr;                                 \
    __asm { __SPECIAL_INSTRUCTION_PREAMBLE                        \
            /* %EAX = guest_NRADDR */                             \
            __asm xchg ecx,ecx                                    \
            __asm mov __addr, eax                                 \
    }                                                             \
    _zzq_orig->nraddr = __addr;                                   \
  }

#define VALGRIND_CALL_NOREDIR_EAX ERROR

#else
#error Unsupported compiler.
#endif

#endif /* PLAT_x86_win32 */

/* ------------------------ amd64-{linux,darwin} --------------- */

#if defined(PLAT_amd64_linux)  ||  defined(PLAT_amd64_darwin)

typedef
   struct {
      uint64_t nraddr; /* where's the code? */
   }
   OrigFn;

#define __SPECIAL_INSTRUCTION_PREAMBLE                            \
                     "\x72\x6f\x6c\x71\x20\x24\x33\x2c\x20\x20\x25\x25\x72\x64\x69\x20\x3b\x20\x72\x6f\x6c\x71\x20\x24\x31\x33\x2c\x20\x25\x25\x72\x64\x69\xa\x9"      \
                     "\x72\x6f\x6c\x71\x20\x24\x36\x31\x2c\x20\x25\x25\x72\x64\x69\x20\x3b\x20\x72\x6f\x6c\x71\x20\x24\x35\x31\x2c\x20\x25\x25\x72\x64\x69\xa\x9"

#define VALGRIND_DO_CLIENT_REQUEST_EXPR(                          \
        _zzq_default, _zzq_request,                               \
        _zzq_arg1, _zzq_arg2, _zzq_arg3, _zzq_arg4, _zzq_arg5)    \
    __extension__                                                 \
    ({ volatile uint64_t _zzq_args[6];              \
    volatile uint64_t _zzq_result;                  \
    _zzq_args[0] = (uint64_t)(_zzq_request);        \
    _zzq_args[1] = (uint64_t)(_zzq_arg1);           \
    _zzq_args[2] = (uint64_t)(_zzq_arg2);           \
    _zzq_args[3] = (uint64_t)(_zzq_arg3);           \
    _zzq_args[4] = (uint64_t)(_zzq_arg4);           \
    _zzq_args[5] = (uint64_t)(_zzq_arg5);           \
    __asm__ volatile(__SPECIAL_INSTRUCTION_PREAMBLE               \
                     /* %RDX = client_request ( %RAX ) */         \
                     "\x78\x63\x68\x67\x71\x20\x25\x25\x72\x62\x78\x2c\x25\x25\x72\x62\x78"                          \
                     : "\x3d\x64" (_zzq_result)                         \
                     : "\x61" (&_zzq_args[0]), "\x30" (_zzq_default)    \
                     : "\x63\x63", "\x6d\x65\x6d\x6f\x72\x79"                             \
                    );                                            \
    _zzq_result;                                                  \
    })

#define VALGRIND_GET_NR_CONTEXT(_zzq_rlval)                       \
  { volatile OrigFn* _zzq_orig = &(_zzq_rlval);                   \
    volatile uint64_t __addr;                       \
    __asm__ volatile(__SPECIAL_INSTRUCTION_PREAMBLE               \
                     /* %RAX = guest_NRADDR */                    \
                     "\x78\x63\x68\x67\x71\x20\x25\x25\x72\x63\x78\x2c\x25\x25\x72\x63\x78"                          \
                     : "\x3d\x61" (__addr)                              \
                     :                                            \
                     : "\x63\x63", "\x6d\x65\x6d\x6f\x72\x79"                             \
                    );                                            \
    _zzq_orig->nraddr = __addr;                                   \
  }

#define VALGRIND_CALL_NOREDIR_RAX                                 \
                     __SPECIAL_INSTRUCTION_PREAMBLE               \
                     /* call-noredir *%RAX */                     \
                     "\x78\x63\x68\x67\x71\x20\x25\x25\x72\x64\x78\x2c\x25\x25\x72\x64\x78\xa\x9"
#endif /* PLAT_amd64_linux || PLAT_amd64_darwin */

/* ------------------------ ppc32-linux ------------------------ */

#if defined(PLAT_ppc32_linux)

typedef
   struct {
      unsigned int nraddr; /* where's the code? */
   }
   OrigFn;

#define __SPECIAL_INSTRUCTION_PREAMBLE                            \
                     "\x72\x6c\x77\x69\x6e\x6d\x20\x30\x2c\x30\x2c\x33\x2c\x30\x2c\x30\x20\x20\x3b\x20\x72\x6c\x77\x69\x6e\x6d\x20\x30\x2c\x30\x2c\x31\x33\x2c\x30\x2c\x30\xa\x9"  \
                     "\x72\x6c\x77\x69\x6e\x6d\x20\x30\x2c\x30\x2c\x32\x39\x2c\x30\x2c\x30\x20\x3b\x20\x72\x6c\x77\x69\x6e\x6d\x20\x30\x2c\x30\x2c\x31\x39\x2c\x30\x2c\x30\xa\x9"

#define VALGRIND_DO_CLIENT_REQUEST_EXPR(                          \
        _zzq_default, _zzq_request,                               \
        _zzq_arg1, _zzq_arg2, _zzq_arg3, _zzq_arg4, _zzq_arg5)    \
                                                                  \
    __extension__                                                 \
  ({         unsigned int  _zzq_args[6];                          \
             unsigned int  _zzq_result;                           \
             unsigned int* _zzq_ptr;                              \
    _zzq_args[0] = (unsigned int)(_zzq_request);                  \
    _zzq_args[1] = (unsigned int)(_zzq_arg1);                     \
    _zzq_args[2] = (unsigned int)(_zzq_arg2);                     \
    _zzq_args[3] = (unsigned int)(_zzq_arg3);                     \
    _zzq_args[4] = (unsigned int)(_zzq_arg4);                     \
    _zzq_args[5] = (unsigned int)(_zzq_arg5);                     \
    _zzq_ptr = _zzq_args;                                         \
    __asm__ volatile("\x6d\x72\x20\x33\x2c\x25\x31\xa\x9" /*default*/                    \
                     "\x6d\x72\x20\x34\x2c\x25\x32\xa\x9" /*ptr*/                        \
                     __SPECIAL_INSTRUCTION_PREAMBLE               \
                     /* %R3 = client_request ( %R4 ) */           \
                     "\x6f\x72\x20\x31\x2c\x31\x2c\x31\xa\x9"                               \
                     "\x6d\x72\x20\x25\x30\x2c\x33"     /*result*/                     \
                     : "\x3d\x62" (_zzq_result)                         \
                     : "\x62" (_zzq_default), "\x62" (_zzq_ptr)         \
                     : "\x63\x63", "\x6d\x65\x6d\x6f\x72\x79", "\x72\x33", "\x72\x34");               \
    _zzq_result;                                                  \
    })

#define VALGRIND_GET_NR_CONTEXT(_zzq_rlval)                       \
  { volatile OrigFn* _zzq_orig = &(_zzq_rlval);                   \
    unsigned int __addr;                                          \
    __asm__ volatile(__SPECIAL_INSTRUCTION_PREAMBLE               \
                     /* %R3 = guest_NRADDR */                     \
                     "\x6f\x72\x20\x32\x2c\x32\x2c\x32\xa\x9"                               \
                     "\x6d\x72\x20\x25\x30\x2c\x33"                                    \
                     : "\x3d\x62" (__addr)                              \
                     :                                            \
                     : "\x63\x63", "\x6d\x65\x6d\x6f\x72\x79", "\x72\x33"                       \
                    );                                            \
    _zzq_orig->nraddr = __addr;                                   \
  }

#define VALGRIND_BRANCH_AND_LINK_TO_NOREDIR_R11                   \
                     __SPECIAL_INSTRUCTION_PREAMBLE               \
                     /* branch-and-link-to-noredir *%R11 */       \
                     "\x6f\x72\x20\x33\x2c\x33\x2c\x33\xa\x9"
#endif /* PLAT_ppc32_linux */

/* ------------------------ ppc64-linux ------------------------ */

#if defined(PLAT_ppc64_linux)

typedef
   struct {
      uint64_t nraddr; /* where's the code? */
      uint64_t r2;  /* what tocptr do we need? */
   }
   OrigFn;

#define __SPECIAL_INSTRUCTION_PREAMBLE                            \
                     "\x72\x6f\x74\x6c\x64\x69\x20\x30\x2c\x30\x2c\x33\x20\x20\x3b\x20\x72\x6f\x74\x6c\x64\x69\x20\x30\x2c\x30\x2c\x31\x33\xa\x9"          \
                     "\x72\x6f\x74\x6c\x64\x69\x20\x30\x2c\x30\x2c\x36\x31\x20\x3b\x20\x72\x6f\x74\x6c\x64\x69\x20\x30\x2c\x30\x2c\x35\x31\xa\x9"

#define VALGRIND_DO_CLIENT_REQUEST_EXPR(                          \
        _zzq_default, _zzq_request,                               \
        _zzq_arg1, _zzq_arg2, _zzq_arg3, _zzq_arg4, _zzq_arg5)    \
                                                                  \
  __extension__                                                   \
  ({         uint64_t  _zzq_args[6];                \
    register uint64_t  _zzq_result __asm__("\x72\x33");   \
    register uint64_t* _zzq_ptr __asm__("\x72\x34");      \
    _zzq_args[0] = (uint64_t)(_zzq_request);        \
    _zzq_args[1] = (uint64_t)(_zzq_arg1);           \
    _zzq_args[2] = (uint64_t)(_zzq_arg2);           \
    _zzq_args[3] = (uint64_t)(_zzq_arg3);           \
    _zzq_args[4] = (uint64_t)(_zzq_arg4);           \
    _zzq_args[5] = (uint64_t)(_zzq_arg5);           \
    _zzq_ptr = _zzq_args;                                         \
    __asm__ volatile(__SPECIAL_INSTRUCTION_PREAMBLE               \
                     /* %R3 = client_request ( %R4 ) */           \
                     "\x6f\x72\x20\x31\x2c\x31\x2c\x31"                                   \
                     : "\x3d\x72" (_zzq_result)                         \
                     : "\x30" (_zzq_default), "\x72" (_zzq_ptr)         \
                     : "\x63\x63", "\x6d\x65\x6d\x6f\x72\x79");                           \
    _zzq_result;                                                  \
  })

#define VALGRIND_GET_NR_CONTEXT(_zzq_rlval)                       \
  { volatile OrigFn* _zzq_orig = &(_zzq_rlval);                   \
    register uint64_t __addr __asm__("\x72\x33");         \
    __asm__ volatile(__SPECIAL_INSTRUCTION_PREAMBLE               \
                     /* %R3 = guest_NRADDR */                     \
                     "\x6f\x72\x20\x32\x2c\x32\x2c\x32"                                   \
                     : "\x3d\x72" (__addr)                              \
                     :                                            \
                     : "\x63\x63", "\x6d\x65\x6d\x6f\x72\x79"                             \
                    );                                            \
    _zzq_orig->nraddr = __addr;                                   \
    __asm__ volatile(__SPECIAL_INSTRUCTION_PREAMBLE               \
                     /* %R3 = guest_NRADDR_GPR2 */                \
                     "\x6f\x72\x20\x34\x2c\x34\x2c\x34"                                   \
                     : "\x3d\x72" (__addr)                              \
                     :                                            \
                     : "\x63\x63", "\x6d\x65\x6d\x6f\x72\x79"                             \
                    );                                            \
    _zzq_orig->r2 = __addr;                                       \
  }

#define VALGRIND_BRANCH_AND_LINK_TO_NOREDIR_R11                   \
                     __SPECIAL_INSTRUCTION_PREAMBLE               \
                     /* branch-and-link-to-noredir *%R11 */       \
                     "\x6f\x72\x20\x33\x2c\x33\x2c\x33\xa\x9"

#endif /* PLAT_ppc64_linux */

/* ------------------------- arm-linux ------------------------- */

#if defined(PLAT_arm_linux)

typedef
   struct {
      unsigned int nraddr; /* where's the code? */
   }
   OrigFn;

#define __SPECIAL_INSTRUCTION_PREAMBLE                            \
            "\x6d\x6f\x76\x20\x72\x31\x32\x2c\x20\x72\x31\x32\x2c\x20\x72\x6f\x72\x20\x23\x33\x20\x20\x3b\x20\x6d\x6f\x76\x20\x72\x31\x32\x2c\x20\x72\x31\x32\x2c\x20\x72\x6f\x72\x20\x23\x31\x33\x20\xa\x9"  \
            "\x6d\x6f\x76\x20\x72\x31\x32\x2c\x20\x72\x31\x32\x2c\x20\x72\x6f\x72\x20\x23\x32\x39\x20\x3b\x20\x6d\x6f\x76\x20\x72\x31\x32\x2c\x20\x72\x31\x32\x2c\x20\x72\x6f\x72\x20\x23\x31\x39\x20\xa\x9"

#define VALGRIND_DO_CLIENT_REQUEST_EXPR(                          \
        _zzq_default, _zzq_request,                               \
        _zzq_arg1, _zzq_arg2, _zzq_arg3, _zzq_arg4, _zzq_arg5)    \
                                                                  \
  __extension__                                                   \
  ({volatile unsigned int  _zzq_args[6];                          \
    volatile unsigned int  _zzq_result;                           \
    _zzq_args[0] = (unsigned int)(_zzq_request);                  \
    _zzq_args[1] = (unsigned int)(_zzq_arg1);                     \
    _zzq_args[2] = (unsigned int)(_zzq_arg2);                     \
    _zzq_args[3] = (unsigned int)(_zzq_arg3);                     \
    _zzq_args[4] = (unsigned int)(_zzq_arg4);                     \
    _zzq_args[5] = (unsigned int)(_zzq_arg5);                     \
    __asm__ volatile("\x6d\x6f\x76\x20\x72\x33\x2c\x20\x25\x31\xa\x9" /*default*/                 \
                     "\x6d\x6f\x76\x20\x72\x34\x2c\x20\x25\x32\xa\x9" /*ptr*/                     \
                     __SPECIAL_INSTRUCTION_PREAMBLE               \
                     /* R3 = client_request ( R4 ) */             \
                     "\x6f\x72\x72\x20\x72\x31\x30\x2c\x20\x72\x31\x30\x2c\x20\x72\x31\x30\xa\x9"                      \
                     "\x6d\x6f\x76\x20\x25\x30\x2c\x20\x72\x33"     /*result*/                  \
                     : "\x3d\x72" (_zzq_result)                         \
                     : "\x72" (_zzq_default), "\x72" (&_zzq_args[0])    \
                     : "\x63\x63","\x6d\x65\x6d\x6f\x72\x79", "\x72\x33", "\x72\x34");                \
    _zzq_result;                                                  \
  })

#define VALGRIND_GET_NR_CONTEXT(_zzq_rlval)                       \
  { volatile OrigFn* _zzq_orig = &(_zzq_rlval);                   \
    unsigned int __addr;                                          \
    __asm__ volatile(__SPECIAL_INSTRUCTION_PREAMBLE               \
                     /* R3 = guest_NRADDR */                      \
                     "\x6f\x72\x72\x20\x72\x31\x31\x2c\x20\x72\x31\x31\x2c\x20\x72\x31\x31\xa\x9"                      \
                     "\x6d\x6f\x76\x20\x25\x30\x2c\x20\x72\x33"                                 \
                     : "\x3d\x72" (__addr)                              \
                     :                                            \
                     : "\x63\x63", "\x6d\x65\x6d\x6f\x72\x79", "\x72\x33"                       \
                    );                                            \
    _zzq_orig->nraddr = __addr;                                   \
  }

#define VALGRIND_BRANCH_AND_LINK_TO_NOREDIR_R4                    \
                     __SPECIAL_INSTRUCTION_PREAMBLE               \
                     /* branch-and-link-to-noredir *%R4 */        \
                     "\x6f\x72\x72\x20\x72\x31\x32\x2c\x20\x72\x31\x32\x2c\x20\x72\x31\x32\xa\x9"

#endif /* PLAT_arm_linux */

/* ------------------------ s390x-linux ------------------------ */

#if defined(PLAT_s390x_linux)

typedef
  struct {
     uint64_t nraddr; /* where's the code? */
  }
  OrigFn;

/* __SPECIAL_INSTRUCTION_PREAMBLE will be used to identify Valgrind specific
 * code. This detection is implemented in platform specific toIR.c
 * (e.g. VEX/priv/guest_s390_decoder.c).
 */
#define __SPECIAL_INSTRUCTION_PREAMBLE                           \
                     "\x6c\x72\x20\x31\x35\x2c\x31\x35\xa\x9"                              \
                     "\x6c\x72\x20\x31\x2c\x31\xa\x9"                                \
                     "\x6c\x72\x20\x32\x2c\x32\xa\x9"                                \
                     "\x6c\x72\x20\x33\x2c\x33\xa\x9"

#define __CLIENT_REQUEST_CODE "\x6c\x72\x20\x32\x2c\x32\xa\x9"
#define __GET_NR_CONTEXT_CODE "\x6c\x72\x20\x33\x2c\x33\xa\x9"
#define __CALL_NO_REDIR_CODE  "\x6c\x72\x20\x34\x2c\x34\xa\x9"

#define VALGRIND_DO_CLIENT_REQUEST_EXPR(                         \
       _zzq_default, _zzq_request,                               \
       _zzq_arg1, _zzq_arg2, _zzq_arg3, _zzq_arg4, _zzq_arg5)    \
  __extension__                                                  \
 ({volatile uint64_t _zzq_args[6];                 \
   volatile uint64_t _zzq_result;                  \
   _zzq_args[0] = (uint64_t)(_zzq_request);        \
   _zzq_args[1] = (uint64_t)(_zzq_arg1);           \
   _zzq_args[2] = (uint64_t)(_zzq_arg2);           \
   _zzq_args[3] = (uint64_t)(_zzq_arg3);           \
   _zzq_args[4] = (uint64_t)(_zzq_arg4);           \
   _zzq_args[5] = (uint64_t)(_zzq_arg5);           \
   __asm__ volatile(/* r2 = args */                              \
                    "\x6c\x67\x72\x20\x32\x2c\x25\x31\xa\x9"                               \
                    /* r3 = default */                           \
                    "\x6c\x67\x72\x20\x33\x2c\x25\x32\xa\x9"                               \
                    __SPECIAL_INSTRUCTION_PREAMBLE               \
                    __CLIENT_REQUEST_CODE                        \
                    /* results = r3 */                           \
                    "\x6c\x67\x72\x20\x25\x30\x2c\x20\x33\xa\x9"                              \
                    : "\x3d\x64" (_zzq_result)                         \
                    : "\x61" (&_zzq_args[0]), "\x30" (_zzq_default)    \
                    : "\x63\x63", "\x32", "\x33", "\x6d\x65\x6d\x6f\x72\x79"                   \
                   );                                            \
   _zzq_result;                                                  \
 })

#define VALGRIND_GET_NR_CONTEXT(_zzq_rlval)                      \
 { volatile OrigFn* _zzq_orig = &(_zzq_rlval);                   \
   volatile uint64_t __addr;                       \
   __asm__ volatile(__SPECIAL_INSTRUCTION_PREAMBLE               \
                    __GET_NR_CONTEXT_CODE                        \
                    "\x6c\x67\x72\x20\x25\x30\x2c\x20\x33\xa\x9"                              \
                    : "\x3d\x61" (__addr)                              \
                    :                                            \
                    : "\x63\x63", "\x33", "\x6d\x65\x6d\x6f\x72\x79"                        \
                   );                                            \
   _zzq_orig->nraddr = __addr;                                   \
 }

#define VALGRIND_CALL_NOREDIR_R1                                 \
                    __SPECIAL_INSTRUCTION_PREAMBLE               \
                    __CALL_NO_REDIR_CODE

#endif /* PLAT_s390x_linux */

/* Insert assembly code for other platforms here... */

#endif /* NVALGRIND */


/* ------------------------------------------------------------------ */
/* PLATFORM SPECIFICS for FUNCTION WRAPPING.  This is all very        */
/* ugly.  It's the least-worst tradeoff I can think of.               */
/* ------------------------------------------------------------------ */

/* This section defines magic (a.k.a appalling-hack) macros for doing
   guaranteed-no-redirection macros, so as to get from function
   wrappers to the functions they are wrapping.  The whole point is to
   construct standard call sequences, but to do the call itself with a
   special no-redirect call pseudo-instruction that the JIT
   understands and handles specially.  This section is long and
   repetitious, and I can't see a way to make it shorter.

   The naming scheme is as follows:

      CALL_FN_{W,v}_{v,W,WW,WWW,WWWW,5W,6W,7W,etc}

   'W' stands for "word" and 'v' for "void".  Hence there are
   different macros for calling arity 0, 1, 2, 3, 4, etc, functions,
   and for each, the possibility of returning a word-typed result, or
   no result.
*/

/* Use these to write the name of your wrapper.  NOTE: duplicates
   VG_WRAP_FUNCTION_Z{U,Z} in pub_tool_redir.h. */

/* Use an extra level of macroisation so as to ensure the soname/fnname
   args are fully macro-expanded before pasting them together. */
#define VG_CONCAT4(_aa,_bb,_cc,_dd) _aa##_bb##_cc##_dd

#define I_WRAP_SONAME_FNNAME_ZU(soname,fnname)                    \
   VG_CONCAT4(_vgwZU_,soname,_,fnname)

#define I_WRAP_SONAME_FNNAME_ZZ(soname,fnname)                    \
   VG_CONCAT4(_vgwZZ_,soname,_,fnname)

/* Use this macro from within a wrapper function to collect the
   context (address and possibly other info) of the original function.
   Once you have that you can then use it in one of the CALL_FN_
   macros.  The type of the argument _lval is OrigFn. */
#define VALGRIND_GET_ORIG_FN(_lval)  VALGRIND_GET_NR_CONTEXT(_lval)

/* Derivatives of the main macros below, for calling functions
   returning void. */

#define CALL_FN_v_v(fnptr)                                        \
   do { volatile unsigned long _junk;                             \
        CALL_FN_W_v(_junk,fnptr); } while (0)

#define CALL_FN_v_W(fnptr, arg1)                                  \
   do { volatile unsigned long _junk;                             \
        CALL_FN_W_W(_junk,fnptr,arg1); } while (0)

#define CALL_FN_v_WW(fnptr, arg1,arg2)                            \
   do { volatile unsigned long _junk;                             \
        CALL_FN_W_WW(_junk,fnptr,arg1,arg2); } while (0)

#define CALL_FN_v_WWW(fnptr, arg1,arg2,arg3)                      \
   do { volatile unsigned long _junk;                             \
        CALL_FN_W_WWW(_junk,fnptr,arg1,arg2,arg3); } while (0)

#define CALL_FN_v_WWWW(fnptr, arg1,arg2,arg3,arg4)                \
   do { volatile unsigned long _junk;                             \
        CALL_FN_W_WWWW(_junk,fnptr,arg1,arg2,arg3,arg4); } while (0)

#define CALL_FN_v_5W(fnptr, arg1,arg2,arg3,arg4,arg5)             \
   do { volatile unsigned long _junk;                             \
        CALL_FN_W_5W(_junk,fnptr,arg1,arg2,arg3,arg4,arg5); } while (0)

#define CALL_FN_v_6W(fnptr, arg1,arg2,arg3,arg4,arg5,arg6)        \
   do { volatile unsigned long _junk;                             \
        CALL_FN_W_6W(_junk,fnptr,arg1,arg2,arg3,arg4,arg5,arg6); } while (0)

#define CALL_FN_v_7W(fnptr, arg1,arg2,arg3,arg4,arg5,arg6,arg7)   \
   do { volatile unsigned long _junk;                             \
        CALL_FN_W_7W(_junk,fnptr,arg1,arg2,arg3,arg4,arg5,arg6,arg7); } while (0)

/* ------------------------- x86-{linux,darwin} ---------------- */

#if defined(PLAT_x86_linux)  ||  defined(PLAT_x86_darwin)

/* These regs are trashed by the hidden call.  No need to mention eax
   as gcc can already see that, plus causes gcc to bomb. */
#define __CALLER_SAVED_REGS /*"\x65\x61\x78"*/ "\x65\x63\x78", "\x65\x64\x78"

/* These CALL_FN_ macros assume that on x86-linux, sizeof(unsigned
   long) == 4. */

#define CALL_FN_W_v(lval, orig)                                   \
   do {                                                           \
      volatile OrigFn        _orig = (orig);                      \
      volatile unsigned long _argvec[1];                          \
      volatile unsigned long _res;                                \
      _argvec[0] = (unsigned long)_orig.nraddr;                   \
      __asm__ volatile(                                           \
         "\x6d\x6f\x76\x6c\x20\x28\x25\x6c\x85\x81\xa7\x29\x2c\x20\x25\x6c\x85\x81\xa7\xa\x9"  /* target->%eax */            \
         VALGRIND_CALL_NOREDIR_EAX                                \
         : /*out*/   "\x3d\x61" (_res)                                  \
         : /*in*/    "\x61" (&_argvec[0])                            \
         : /*trash*/ "\x63\x63", "\x6d\x65\x6d\x6f\x72\x79", __CALLER_SAVED_REGS          \
      );                                                          \
      lval = (__typeof__(lval)) _res;                             \
   } while (0)

#define CALL_FN_W_W(lval, orig, arg1)                             \
   do {                                                           \
      volatile OrigFn        _orig = (orig);                      \
      volatile unsigned long _argvec[2];                          \
      volatile unsigned long _res;                                \
      _argvec[0] = (unsigned long)_orig.nraddr;                   \
      _argvec[1] = (unsigned long)(arg1);                         \
      __asm__ volatile(                                           \
         "\x73\x75\x62\x6c\x20\x24\x31\x32\x2c\x20\x25\x6c\x85\xa2\x97\xa\x9"                                    \
         "\x70\x75\x73\x68\x6c\x20\x34\x28\x25\x6c\x85\x81\xa7\x29\xa\x9"                                     \
         "\x6d\x6f\x76\x6c\x20\x28\x25\x6c\x85\x81\xa7\x29\x2c\x20\x25\x6c\x85\x81\xa7\xa\x9"  /* target->%eax */            \
         VALGRIND_CALL_NOREDIR_EAX                                \
         "\x61\x64\x64\x6c\x20\x24\x31\x36\x2c\x20\x25\x6c\x85\xa2\x97\xa"                                      \
         : /*out*/   "\x3d\x61" (_res)                                  \
         : /*in*/    "\x61" (&_argvec[0])                            \
         : /*trash*/ "\x63\x63", "\x6d\x65\x6d\x6f\x72\x79", __CALLER_SAVED_REGS          \
      );                                                          \
      lval = (__typeof__(lval)) _res;                             \
   } while (0)

#define CALL_FN_W_WW(lval, orig, arg1,arg2)                       \
   do {                                                           \
      volatile OrigFn        _orig = (orig);                      \
      volatile unsigned long _argvec[3];                          \
      volatile unsigned long _res;                                \
      _argvec[0] = (unsigned long)_orig.nraddr;                   \
      _argvec[1] = (unsigned long)(arg1);                         \
      _argvec[2] = (unsigned long)(arg2);                         \
      __asm__ volatile(                                           \
         "\x73\x75\x62\x6c\x20\x24\x38\x2c\x20\x25\x6c\x85\xa2\x97\xa\x9"                                     \
         "\x70\x75\x73\x68\x6c\x20\x38\x28\x25\x6c\x85\x81\xa7\x29\xa\x9"                                     \
         "\x70\x75\x73\x68\x6c\x20\x34\x28\x25\x6c\x85\x81\xa7\x29\xa\x9"                                     \
         "\x6d\x6f\x76\x6c\x20\x28\x25\x6c\x85\x81\xa7\x29\x2c\x20\x25\x6c\x85\x81\xa7\xa\x9"  /* target->%eax */            \
         VALGRIND_CALL_NOREDIR_EAX                                \
         "\x61\x64\x64\x6c\x20\x24\x31\x36\x2c\x20\x25\x6c\x85\xa2\x97\xa"                                      \
         : /*out*/   "\x3d\x61" (_res)                                  \
         : /*in*/    "\x61" (&_argvec[0])                            \
         : /*trash*/ "\x63\x63", "\x6d\x65\x6d\x6f\x72\x79", __CALLER_SAVED_REGS          \
      );                                                          \
      lval = (__typeof__(lval)) _res;                             \
   } while (0)

#define CALL_FN_W_WWW(lval, orig, arg1,arg2,arg3)                 \
   do {                                                           \
      volatile OrigFn        _orig = (orig);                      \
      volatile unsigned long _argvec[4];                          \
      volatile unsigned long _res;                                \
      _argvec[0] = (unsigned long)_orig.nraddr;                   \
      _argvec[1] = (unsigned long)(arg1);                         \
      _argvec[2] = (unsigned long)(arg2);                         \
      _argvec[3] = (unsigned long)(arg3);                         \
      __asm__ volatile(                                           \
         "\x73\x75\x62\x6c\x20\x24\x34\x2c\x20\x25\x6c\x85\xa2\x97\xa\x9"                                     \
         "\x70\x75\x73\x68\x6c\x20\x31\x32\x28\x25\x6c\x85\x81\xa7\x29\xa\x9"                                    \
         "\x70\x75\x73\x68\x6c\x20\x38\x28\x25\x6c\x85\x81\xa7\x29\xa\x9"                                     \
         "\x70\x75\x73\x68\x6c\x20\x34\x28\x25\x6c\x85\x81\xa7\x29\xa\x9"                                     \
         "\x6d\x6f\x76\x6c\x20\x28\x25\x6c\x85\x81\xa7\x29\x2c\x20\x25\x6c\x85\x81\xa7\xa\x9"  /* target->%eax */            \
         VALGRIND_CALL_NOREDIR_EAX                                \
         "\x61\x64\x64\x6c\x20\x24\x31\x36\x2c\x20\x25\x6c\x85\xa2\x97\xa"                                      \
         : /*out*/   "\x3d\x61" (_res)                                  \
         : /*in*/    "\x61" (&_argvec[0])                            \
         : /*trash*/ "\x63\x63", "\x6d\x65\x6d\x6f\x72\x79", __CALLER_SAVED_REGS          \
      );                                                          \
      lval = (__typeof__(lval)) _res;                             \
   } while (0)

#define CALL_FN_W_WWWW(lval, orig, arg1,arg2,arg3,arg4)           \
   do {                                                           \
      volatile OrigFn        _orig = (orig);                      \
      volatile unsigned long _argvec[5];                          \
      volatile unsigned long _res;                                \
      _argvec[0] = (unsigned long)_orig.nraddr;                   \
      _argvec[1] = (unsigned long)(arg1);                         \
      _argvec[2] = (unsigned long)(arg2);                         \
      _argvec[3] = (unsigned long)(arg3);                         \
      _argvec[4] = (unsigned long)(arg4);                         \
      __asm__ volatile(                                           \
         "\x70\x75\x73\x68\x6c\x20\x31\x36\x28\x25\x6c\x85\x81\xa7\x29\xa\x9"                                    \
         "\x70\x75\x73\x68\x6c\x20\x31\x32\x28\x25\x6c\x85\x81\xa7\x29\xa\x9"                                    \
         "\x70\x75\x73\x68\x6c\x20\x38\x28\x25\x6c\x85\x81\xa7\x29\xa\x9"                                     \
         "\x70\x75\x73\x68\x6c\x20\x34\x28\x25\x6c\x85\x81\xa7\x29\xa\x9"                                     \
         "\x6d\x6f\x76\x6c\x20\x28\x25\x6c\x85\x81\xa7\x29\x2c\x20\x25\x6c\x85\x81\xa7\xa\x9"  /* target->%eax */            \
         VALGRIND_CALL_NOREDIR_EAX                                \
         "\x61\x64\x64\x6c\x20\x24\x31\x36\x2c\x20\x25\x6c\x85\xa2\x97\xa"                                      \
         : /*out*/   "\x3d\x61" (_res)                                  \
         : /*in*/    "\x61" (&_argvec[0])                            \
         : /*trash*/ "\x63\x63", "\x6d\x65\x6d\x6f\x72\x79", __CALLER_SAVED_REGS          \
      );                                                          \
      lval = (__typeof__(lval)) _res;                             \
   } while (0)

#define CALL_FN_W_5W(lval, orig, arg1,arg2,arg3,arg4,arg5)        \
   do {                                                           \
      volatile OrigFn        _orig = (orig);                      \
      volatile unsigned long _argvec[6];                          \
      volatile unsigned long _res;                                \
      _argvec[0] = (unsigned long)_orig.nraddr;                   \
      _argvec[1] = (unsigned long)(arg1);                         \
      _argvec[2] = (unsigned long)(arg2);                         \
      _argvec[3] = (unsigned long)(arg3);                         \
      _argvec[4] = (unsigned long)(arg4);                         \
      _argvec[5] = (unsigned long)(arg5);                         \
      __asm__ volatile(                                           \
         "\x73\x75\x62\x6c\x20\x24\x31\x32\x2c\x20\x25\x6c\x85\xa2\x97\xa\x9"                                    \
         "\x70\x75\x73\x68\x6c\x20\x32\x30\x28\x25\x6c\x85\x81\xa7\x29\xa\x9"                                    \
         "\x70\x75\x73\x68\x6c\x20\x31\x36\x28\x25\x6c\x85\x81\xa7\x29\xa\x9"                                    \
         "\x70\x75\x73\x68\x6c\x20\x31\x32\x28\x25\x6c\x85\x81\xa7\x29\xa\x9"                                    \
         "\x70\x75\x73\x68\x6c\x20\x38\x28\x25\x6c\x85\x81\xa7\x29\xa\x9"                                     \
         "\x70\x75\x73\x68\x6c\x20\x34\x28\x25\x6c\x85\x81\xa7\x29\xa\x9"                                     \
         "\x6d\x6f\x76\x6c\x20\x28\x25\x6c\x85\x81\xa7\x29\x2c\x20\x25\x6c\x85\x81\xa7\xa\x9"  /* target->%eax */            \
         VALGRIND_CALL_NOREDIR_EAX                                \
         "\x61\x64\x64\x6c\x20\x24\x33\x32\x2c\x20\x25\x6c\x85\xa2\x97\xa"                                      \
         : /*out*/   "\x3d\x61" (_res)                                  \
         : /*in*/    "\x61" (&_argvec[0])                            \
         : /*trash*/ "\x63\x63", "\x6d\x65\x6d\x6f\x72\x79", __CALLER_SAVED_REGS          \
      );                                                          \
      lval = (__typeof__(lval)) _res;                             \
   } while (0)

#define CALL_FN_W_6W(lval, orig, arg1,arg2,arg3,arg4,arg5,arg6)   \
   do {                                                           \
      volatile OrigFn        _orig = (orig);                      \
      volatile unsigned long _argvec[7];                          \
      volatile unsigned long _res;                                \
      _argvec[0] = (unsigned long)_orig.nraddr;                   \
      _argvec[1] = (unsigned long)(arg1);                         \
      _argvec[2] = (unsigned long)(arg2);                         \
      _argvec[3] = (unsigned long)(arg3);                         \
      _argvec[4] = (unsigned long)(arg4);                         \
      _argvec[5] = (unsigned long)(arg5);                         \
      _argvec[6] = (unsigned long)(arg6);                         \
      __asm__ volatile(                                           \
         "\x73\x75\x62\x6c\x20\x24\x38\x2c\x20\x25\x6c\x85\xa2\x97\xa\x9"                                     \
         "\x70\x75\x73\x68\x6c\x20\x32\x34\x28\x25\x6c\x85\x81\xa7\x29\xa\x9"                                    \
         "\x70\x75\x73\x68\x6c\x20\x32\x30\x28\x25\x6c\x85\x81\xa7\x29\xa\x9"                                    \
         "\x70\x75\x73\x68\x6c\x20\x31\x36\x28\x25\x6c\x85\x81\xa7\x29\xa\x9"                                    \
         "\x70\x75\x73\x68\x6c\x20\x31\x32\x28\x25\x6c\x85\x81\xa7\x29\xa\x9"                                    \
         "\x70\x75\x73\x68\x6c\x20\x38\x28\x25\x6c\x85\x81\xa7\x29\xa\x9"                                     \
         "\x70\x75\x73\x68\x6c\x20\x34\x28\x25\x6c\x85\x81\xa7\x29\xa\x9"                                     \
         "\x6d\x6f\x76\x6c\x20\x28\x25\x6c\x85\x81\xa7\x29\x2c\x20\x25\x6c\x85\x81\xa7\xa\x9"  /* target->%eax */            \
         VALGRIND_CALL_NOREDIR_EAX                                \
         "\x61\x64\x64\x6c\x20\x24\x33\x32\x2c\x20\x25\x6c\x85\xa2\x97\xa"                                      \
         : /*out*/   "\x3d\x61" (_res)                                  \
         : /*in*/    "\x61" (&_argvec[0])                            \
         : /*trash*/ "\x63\x63", "\x6d\x65\x6d\x6f\x72\x79", __CALLER_SAVED_REGS          \
      );                                                          \
      lval = (__typeof__(lval)) _res;                             \
   } while (0)

#define CALL_FN_W_7W(lval, orig, arg1,arg2,arg3,arg4,arg5,arg6,   \
                                 arg7)                            \
   do {                                                           \
      volatile OrigFn        _orig = (orig);                      \
      volatile unsigned long _argvec[8];                          \
      volatile unsigned long _res;                                \
      _argvec[0] = (unsigned long)_orig.nraddr;                   \
      _argvec[1] = (unsigned long)(arg1);                         \
      _argvec[2] = (unsigned long)(arg2);                         \
      _argvec[3] = (unsigned long)(arg3);                         \
      _argvec[4] = (unsigned long)(arg4);                         \
      _argvec[5] = (unsigned long)(arg5);                         \
      _argvec[6] = (unsigned long)(arg6);                         \
      _argvec[7] = (unsigned long)(arg7);                         \
      __asm__ volatile(                                           \
         "\x73\x75\x62\x6c\x20\x24\x34\x2c\x20\x25\x6c\x85\xa2\x97\xa\x9"                                     \
         "\x70\x75\x73\x68\x6c\x20\x32\x38\x28\x25\x6c\x85\x81\xa7\x29\xa\x9"                                    \
         "\x70\x75\x73\x68\x6c\x20\x32\x34\x28\x25\x6c\x85\x81\xa7\x29\xa\x9"                                    \
         "\x70\x75\x73\x68\x6c\x20\x32\x30\x28\x25\x6c\x85\x81\xa7\x29\xa\x9"                                    \
         "\x70\x75\x73\x68\x6c\x20\x31\x36\x28\x25\x6c\x85\x81\xa7\x29\xa\x9"                                    \
         "\x70\x75\x73\x68\x6c\x20\x31\x32\x28\x25\x6c\x85\x81\xa7\x29\xa\x9"                                    \
         "\x70\x75\x73\x68\x6c\x20\x38\x28\x25\x6c\x85\x81\xa7\x29\xa\x9"                                     \
         "\x70\x75\x73\x68\x6c\x20\x34\x28\x25\x6c\x85\x81\xa7\x29\xa\x9"                                     \
         "\x6d\x6f\x76\x6c\x20\x28\x25\x6c\x85\x81\xa7\x29\x2c\x20\x25\x6c\x85\x81\xa7\xa\x9"  /* target->%eax */            \
         VALGRIND_CALL_NOREDIR_EAX                                \
         "\x61\x64\x64\x6c\x20\x24\x33\x32\x2c\x20\x25\x6c\x85\xa2\x97\xa"                                      \
         : /*out*/   "\x3d\x61" (_res)                                  \
         : /*in*/    "\x61" (&_argvec[0])                            \
         : /*trash*/ "\x63\x63", "\x6d\x65\x6d\x6f\x72\x79", __CALLER_SAVED_REGS          \
      );                                                          \
      lval = (__typeof__(lval)) _res;                             \
   } while (0)

#define CALL_FN_W_8W(lval, orig, arg1,arg2,arg3,arg4,arg5,arg6,   \
                                 arg7,arg8)                       \
   do {                                                           \
      volatile OrigFn        _orig = (orig);                      \
      volatile unsigned long _argvec[9];                          \
      volatile unsigned long _res;                                \
      _argvec[0] = (unsigned long)_orig.nraddr;                   \
      _argvec[1] = (unsigned long)(arg1);                         \
      _argvec[2] = (unsigned long)(arg2);                         \
      _argvec[3] = (unsigned long)(arg3);                         \
      _argvec[4] = (unsigned long)(arg4);                         \
      _argvec[5] = (unsigned long)(arg5);                         \
      _argvec[6] = (unsigned long)(arg6);                         \
      _argvec[7] = (unsigned long)(arg7);                         \
      _argvec[8] = (unsigned long)(arg8);                         \
      __asm__ volatile(                                           \
         "\x70\x75\x73\x68\x6c\x20\x33\x32\x28\x25\x6c\x85\x81\xa7\x29\xa\x9"                                    \
         "\x70\x75\x73\x68\x6c\x20\x32\x38\x28\x25\x6c\x85\x81\xa7\x29\xa\x9"                                    \
         "\x70\x75\x73\x68\x6c\x20\x32\x34\x28\x25\x6c\x85\x81\xa7\x29\xa\x9"                                    \
         "\x70\x75\x73\x68\x6c\x20\x32\x30\x28\x25\x6c\x85\x81\xa7\x29\xa\x9"                                    \
         "\x70\x75\x73\x68\x6c\x20\x31\x36\x28\x25\x6c\x85\x81\xa7\x29\xa\x9"                                    \
         "\x70\x75\x73\x68\x6c\x20\x31\x32\x28\x25\x6c\x85\x81\xa7\x29\xa\x9"                                    \
         "\x70\x75\x73\x68\x6c\x20\x38\x28\x25\x6c\x85\x81\xa7\x29\xa\x9"                                     \
         "\x70\x75\x73\x68\x6c\x20\x34\x28\x25\x6c\x85\x81\xa7\x29\xa\x9"                                     \
         "\x6d\x6f\x76\x6c\x20\x28\x25\x6c\x85\x81\xa7\x29\x2c\x20\x25\x6c\x85\x81\xa7\xa\x9"  /* target->%eax */            \
         VALGRIND_CALL_NOREDIR_EAX                                \
         "\x61\x64\x64\x6c\x20\x24\x33\x32\x2c\x20\x25\x6c\x85\xa2\x97\xa"                                      \
         : /*out*/   "\x3d\x61" (_res)                                  \
         : /*in*/    "\x61" (&_argvec[0])                            \
         : /*trash*/ "\x63\x63", "\x6d\x65\x6d\x6f\x72\x79", __CALLER_SAVED_REGS          \
      );                                                          \
      lval = (__typeof__(lval)) _res;                             \
   } while (0)

#define CALL_FN_W_9W(lval, orig, arg1,arg2,arg3,arg4,arg5,arg6,   \
                                 arg7,arg8,arg9)                  \
   do {                                                           \
      volatile OrigFn        _orig = (orig);                      \
      volatile unsigned long _argvec[10];                         \
      volatile unsigned long _res;                                \
      _argvec[0] = (unsigned long)_orig.nraddr;                   \
      _argvec[1] = (unsigned long)(arg1);                         \
      _argvec[2] = (unsigned long)(arg2);                         \
      _argvec[3] = (unsigned long)(arg3);                         \
      _argvec[4] = (unsigned long)(arg4);                         \
      _argvec[5] = (unsigned long)(arg5);                         \
      _argvec[6] = (unsigned long)(arg6);                         \
      _argvec[7] = (unsigned long)(arg7);                         \
      _argvec[8] = (unsigned long)(arg8);                         \
      _argvec[9] = (unsigned long)(arg9);                         \
      __asm__ volatile(                                           \
         "\x73\x75\x62\x6c\x20\x24\x31\x32\x2c\x20\x25\x6c\x85\xa2\x97\xa\x9"                                    \
         "\x70\x75\x73\x68\x6c\x20\x33\x36\x28\x25\x6c\x85\x81\xa7\x29\xa\x9"                                    \
         "\x70\x75\x73\x68\x6c\x20\x33\x32\x28\x25\x6c\x85\x81\xa7\x29\xa\x9"                                    \
         "\x70\x75\x73\x68\x6c\x20\x32\x38\x28\x25\x6c\x85\x81\xa7\x29\xa\x9"                                    \
         "\x70\x75\x73\x68\x6c\x20\x32\x34\x28\x25\x6c\x85\x81\xa7\x29\xa\x9"                                    \
         "\x70\x75\x73\x68\x6c\x20\x32\x30\x28\x25\x6c\x85\x81\xa7\x29\xa\x9"                                    \
         "\x70\x75\x73\x68\x6c\x20\x31\x36\x28\x25\x6c\x85\x81\xa7\x29\xa\x9"                                    \
         "\x70\x75\x73\x68\x6c\x20\x31\x32\x28\x25\x6c\x85\x81\xa7\x29\xa\x9"                                    \
         "\x70\x75\x73\x68\x6c\x20\x38\x28\x25\x6c\x85\x81\xa7\x29\xa\x9"                                     \
         "\x70\x75\x73\x68\x6c\x20\x34\x28\x25\x6c\x85\x81\xa7\x29\xa\x9"                                     \
         "\x6d\x6f\x76\x6c\x20\x28\x25\x6c\x85\x81\xa7\x29\x2c\x20\x25\x6c\x85\x81\xa7\xa\x9"  /* target->%eax */            \
         VALGRIND_CALL_NOREDIR_EAX                                \
         "\x61\x64\x64\x6c\x20\x24\x34\x38\x2c\x20\x25\x6c\x85\xa2\x97\xa"                                      \
         : /*out*/   "\x3d\x61" (_res)                                  \
         : /*in*/    "\x61" (&_argvec[0])                            \
         : /*trash*/ "\x63\x63", "\x6d\x65\x6d\x6f\x72\x79", __CALLER_SAVED_REGS          \
      );                                                          \
      lval = (__typeof__(lval)) _res;                             \
   } while (0)

#define CALL_FN_W_10W(lval, orig, arg1,arg2,arg3,arg4,arg5,arg6,  \
                                  arg7,arg8,arg9,arg10)           \
   do {                                                           \
      volatile OrigFn        _orig = (orig);                      \
      volatile unsigned long _argvec[11];                         \
      volatile unsigned long _res;                                \
      _argvec[0] = (unsigned long)_orig.nraddr;                   \
      _argvec[1] = (unsigned long)(arg1);                         \
      _argvec[2] = (unsigned long)(arg2);                         \
      _argvec[3] = (unsigned long)(arg3);                         \
      _argvec[4] = (unsigned long)(arg4);                         \
      _argvec[5] = (unsigned long)(arg5);                         \
      _argvec[6] = (unsigned long)(arg6);                         \
      _argvec[7] = (unsigned long)(arg7);                         \
      _argvec[8] = (unsigned long)(arg8);                         \
      _argvec[9] = (unsigned long)(arg9);                         \
      _argvec[10] = (unsigned long)(arg10);                       \
      __asm__ volatile(                                           \
         "\x73\x75\x62\x6c\x20\x24\x38\x2c\x20\x25\x6c\x85\xa2\x97\xa\x9"                                     \
         "\x70\x75\x73\x68\x6c\x20\x34\x30\x28\x25\x6c\x85\x81\xa7\x29\xa\x9"                                    \
         "\x70\x75\x73\x68\x6c\x20\x33\x36\x28\x25\x6c\x85\x81\xa7\x29\xa\x9"                                    \
         "\x70\x75\x73\x68\x6c\x20\x33\x32\x28\x25\x6c\x85\x81\xa7\x29\xa\x9"                                    \
         "\x70\x75\x73\x68\x6c\x20\x32\x38\x28\x25\x6c\x85\x81\xa7\x29\xa\x9"                                    \
         "\x70\x75\x73\x68\x6c\x20\x32\x34\x28\x25\x6c\x85\x81\xa7\x29\xa\x9"                                    \
         "\x70\x75\x73\x68\x6c\x20\x32\x30\x28\x25\x6c\x85\x81\xa7\x29\xa\x9"                                    \
         "\x70\x75\x73\x68\x6c\x20\x31\x36\x28\x25\x6c\x85\x81\xa7\x29\xa\x9"                                    \
         "\x70\x75\x73\x68\x6c\x20\x31\x32\x28\x25\x6c\x85\x81\xa7\x29\xa\x9"                                    \
         "\x70\x75\x73\x68\x6c\x20\x38\x28\x25\x6c\x85\x81\xa7\x29\xa\x9"                                     \
         "\x70\x75\x73\x68\x6c\x20\x34\x28\x25\x6c\x85\x81\xa7\x29\xa\x9"                                     \
         "\x6d\x6f\x76\x6c\x20\x28\x25\x6c\x85\x81\xa7\x29\x2c\x20\x25\x6c\x85\x81\xa7\xa\x9"  /* target->%eax */            \
         VALGRIND_CALL_NOREDIR_EAX                                \
         "\x61\x64\x64\x6c\x20\x24\x34\x38\x2c\x20\x25\x6c\x85\xa2\x97\xa"                                      \
         : /*out*/   "\x3d\x61" (_res)                                  \
         : /*in*/    "\x61" (&_argvec[0])                            \
         : /*trash*/ "\x63\x63", "\x6d\x65\x6d\x6f\x72\x79", __CALLER_SAVED_REGS          \
      );                                                          \
      lval = (__typeof__(lval)) _res;                             \
   } while (0)

#define CALL_FN_W_11W(lval, orig, arg1,arg2,arg3,arg4,arg5,       \
                                  arg6,arg7,arg8,arg9,arg10,      \
                                  arg11)                          \
   do {                                                           \
      volatile OrigFn        _orig = (orig);                      \
      volatile unsigned long _argvec[12];                         \
      volatile unsigned long _res;                                \
      _argvec[0] = (unsigned long)_orig.nraddr;                   \
      _argvec[1] = (unsigned long)(arg1);                         \
      _argvec[2] = (unsigned long)(arg2);                         \
      _argvec[3] = (unsigned long)(arg3);                         \
      _argvec[4] = (unsigned long)(arg4);                         \
      _argvec[5] = (unsigned long)(arg5);                         \
      _argvec[6] = (unsigned long)(arg6);                         \
      _argvec[7] = (unsigned long)(arg7);                         \
      _argvec[8] = (unsigned long)(arg8);                         \
      _argvec[9] = (unsigned long)(arg9);                         \
      _argvec[10] = (unsigned long)(arg10);                       \
      _argvec[11] = (unsigned long)(arg11);                       \
      __asm__ volatile(                                           \
         "\x73\x75\x62\x6c\x20\x24\x34\x2c\x20\x25\x6c\x85\xa2\x97\xa\x9"                                     \
         "\x70\x75\x73\x68\x6c\x20\x34\x34\x28\x25\x6c\x85\x81\xa7\x29\xa\x9"                                    \
         "\x70\x75\x73\x68\x6c\x20\x34\x30\x28\x25\x6c\x85\x81\xa7\x29\xa\x9"                                    \
         "\x70\x75\x73\x68\x6c\x20\x33\x36\x28\x25\x6c\x85\x81\xa7\x29\xa\x9"                                    \
         "\x70\x75\x73\x68\x6c\x20\x33\x32\x28\x25\x6c\x85\x81\xa7\x29\xa\x9"                                    \
         "\x70\x75\x73\x68\x6c\x20\x32\x38\x28\x25\x6c\x85\x81\xa7\x29\xa\x9"                                    \
         "\x70\x75\x73\x68\x6c\x20\x32\x34\x28\x25\x6c\x85\x81\xa7\x29\xa\x9"                                    \
         "\x70\x75\x73\x68\x6c\x20\x32\x30\x28\x25\x6c\x85\x81\xa7\x29\xa\x9"                                    \
         "\x70\x75\x73\x68\x6c\x20\x31\x36\x28\x25\x6c\x85\x81\xa7\x29\xa\x9"                                    \
         "\x70\x75\x73\x68\x6c\x20\x31\x32\x28\x25\x6c\x85\x81\xa7\x29\xa\x9"                                    \
         "\x70\x75\x73\x68\x6c\x20\x38\x28\x25\x6c\x85\x81\xa7\x29\xa\x9"                                     \
         "\x70\x75\x73\x68\x6c\x20\x34\x28\x25\x6c\x85\x81\xa7\x29\xa\x9"                                     \
         "\x6d\x6f\x76\x6c\x20\x28\x25\x6c\x85\x81\xa7\x29\x2c\x20\x25\x6c\x85\x81\xa7\xa\x9"  /* target->%eax */            \
         VALGRIND_CALL_NOREDIR_EAX                                \
         "\x61\x64\x64\x6c\x20\x24\x34\x38\x2c\x20\x25\x6c\x85\xa2\x97\xa"                                      \
         : /*out*/   "\x3d\x61" (_res)                                  \
         : /*in*/    "\x61" (&_argvec[0])                            \
         : /*trash*/ "\x63\x63", "\x6d\x65\x6d\x6f\x72\x79", __CALLER_SAVED_REGS          \
      );                                                          \
      lval = (__typeof__(lval)) _res;                             \
   } while (0)

#define CALL_FN_W_12W(lval, orig, arg1,arg2,arg3,arg4,arg5,       \
                                  arg6,arg7,arg8,arg9,arg10,      \
                                  arg11,arg12)                    \
   do {                                                           \
      volatile OrigFn        _orig = (orig);                      \
      volatile unsigned long _argvec[13];                         \
      volatile unsigned long _res;                                \
      _argvec[0] = (unsigned long)_orig.nraddr;                   \
      _argvec[1] = (unsigned long)(arg1);                         \
      _argvec[2] = (unsigned long)(arg2);                         \
      _argvec[3] = (unsigned long)(arg3);                         \
      _argvec[4] = (unsigned long)(arg4);                         \
      _argvec[5] = (unsigned long)(arg5);                         \
      _argvec[6] = (unsigned long)(arg6);                         \
      _argvec[7] = (unsigned long)(arg7);                         \
      _argvec[8] = (unsigned long)(arg8);                         \
      _argvec[9] = (unsigned long)(arg9);                         \
      _argvec[10] = (unsigned long)(arg10);                       \
      _argvec[11] = (unsigned long)(arg11);                       \
      _argvec[12] = (unsigned long)(arg12);                       \
      __asm__ volatile(                                           \
         "\x70\x75\x73\x68\x6c\x20\x34\x38\x28\x25\x6c\x85\x81\xa7\x29\xa\x9"                                    \
         "\x70\x75\x73\x68\x6c\x20\x34\x34\x28\x25\x6c\x85\x81\xa7\x29\xa\x9"                                    \
         "\x70\x75\x73\x68\x6c\x20\x34\x30\x28\x25\x6c\x85\x81\xa7\x29\xa\x9"                                    \
         "\x70\x75\x73\x68\x6c\x20\x33\x36\x28\x25\x6c\x85\x81\xa7\x29\xa\x9"                                    \
         "\x70\x75\x73\x68\x6c\x20\x33\x32\x28\x25\x6c\x85\x81\xa7\x29\xa\x9"                                    \
         "\x70\x75\x73\x68\x6c\x20\x32\x38\x28\x25\x6c\x85\x81\xa7\x29\xa\x9"                                    \
         "\x70\x75\x73\x68\x6c\x20\x32\x34\x28\x25\x6c\x85\x81\xa7\x29\xa\x9"                                    \
         "\x70\x75\x73\x68\x6c\x20\x32\x30\x28\x25\x6c\x85\x81\xa7\x29\xa\x9"                                    \
         "\x70\x75\x73\x68\x6c\x20\x31\x36\x28\x25\x6c\x85\x81\xa7\x29\xa\x9"                                    \
         "\x70\x75\x73\x68\x6c\x20\x31\x32\x28\x25\x6c\x85\x81\xa7\x29\xa\x9"                                    \
         "\x70\x75\x73\x68\x6c\x20\x38\x28\x25\x6c\x85\x81\xa7\x29\xa\x9"                                     \
         "\x70\x75\x73\x68\x6c\x20\x34\x28\x25\x6c\x85\x81\xa7\x29\xa\x9"                                     \
         "\x6d\x6f\x76\x6c\x20\x28\x25\x6c\x85\x81\xa7\x29\x2c\x20\x25\x6c\x85\x81\xa7\xa\x9"  /* target->%eax */            \
         VALGRIND_CALL_NOREDIR_EAX                                \
         "\x61\x64\x64\x6c\x20\x24\x34\x38\x2c\x20\x25\x6c\x85\xa2\x97\xa"                                      \
         : /*out*/   "\x3d\x61" (_res)                                  \
         : /*in*/    "\x61" (&_argvec[0])                            \
         : /*trash*/ "\x63\x63", "\x6d\x65\x6d\x6f\x72\x79", __CALLER_SAVED_REGS          \
      );                                                          \
      lval = (__typeof__(lval)) _res;                             \
   } while (0)

#endif /* PLAT_x86_linux || PLAT_x86_darwin */

/* ------------------------ amd64-{linux,darwin} --------------- */

#if defined(PLAT_amd64_linux)  ||  defined(PLAT_amd64_darwin)

/* ARGREGS: rdi rsi rdx rcx r8 r9 (the rest on stack in R-to-L order) */

/* These regs are trashed by the hidden call. */
#define __CALLER_SAVED_REGS /*"\x72\x61\x78",*/ "\x72\x63\x78", "\x72\x64\x78", "\x72\x73\x69",       \
                            "\x72\x64\x69", "\x72\x38", "\x72\x39", "\x72\x31\x30", "\x72\x31\x31"

/* This is all pretty complex.  It's so as to make stack unwinding
   work reliably.  See bug 243270.  The basic problem is the sub and
   add of 128 of %rsp in all of the following macros.  If gcc believes
   the CFA is in %rsp, then unwinding may fail, because what's at the
   CFA is not what gcc "expected" when it constructs the CFIs for the
   places where the macros are instantiated.

   But we can't just add a CFI annotation to increase the CFA offset
   by 128, to match the sub of 128 from %rsp, because we don't know
   whether gcc has chosen %rsp as the CFA at that point, or whether it
   has chosen some other register (eg, %rbp).  In the latter case,
   adding a CFI annotation to change the CFA offset is simply wrong.

   So the solution is to get hold of the CFA using
   __builtin_dwarf_cfa(), put it in a known register, and add a
   CFI annotation to say what the register is.  We choose %rbp for
   this (perhaps perversely), because:

   (1) %rbp is already subject to unwinding.  If a new register was
       chosen then the unwinder would have to unwind it in all stack
       traces, which is expensive, and

   (2) %rbp is already subject to precise exception updates in the
       JIT.  If a new register was chosen, we'd have to have precise
       exceptions for it too, which reduces performance of the
       generated code.

   However .. one extra complication.  We can't just whack the result
   of __builtin_dwarf_cfa() into %rbp and then add %rbp to the
   list of trashed registers at the end of the inline assembly
   fragments; gcc won't allow %rbp to appear in that list.  Hence
   instead we need to stash %rbp in %r15 for the duration of the asm,
   and say that %r15 is trashed instead.  gcc seems happy to go with
   that.

   Oh .. and this all needs to be conditionalised so that it is
   unchanged from before this commit, when compiled with older gccs
   that don't support __builtin_dwarf_cfa.  Furthermore, since
   this header file is freestanding, it has to be independent of
   config.h, and so the following conditionalisation cannot depend on
   configure time checks.

   Although it's not clear from
   'defined(__GNUC__) && defined(__GCC_HAVE_DWARF2_CFI_ASM)',
   this expression excludes Darwin.
   .cfi directives in Darwin assembly appear to be completely
   different and I haven't investigated how they work.

   For even more entertainment value, note we have to use the
   completely undocumented __builtin_dwarf_cfa(), which appears to
   really compute the CFA, whereas __builtin_frame_address(0) claims
   to but actually doesn't.  See
   https://bugs.kde.org/show_bug.cgi?id=243270#c47
*/
#if defined(__GNUC__) && defined(__GCC_HAVE_DWARF2_CFI_ASM)
#  define __FRAME_POINTER                                         \
      ,"\x72"(__builtin_dwarf_cfa())
#  define VALGRIND_CFI_PROLOGUE                                   \
      "\x6d\x6f\x76\x71\x20\x25\x25\x72\x62\x70\x2c\x20\x25\x25\x72\x31\x35\xa\x9"                                     \
      "\x6d\x6f\x76\x71\x20\x25\x32\x2c\x20\x25\x25\x72\x62\x70\xa\x9"                                        \
      "\x2e\x63\x66\x69\x5f\x72\x65\x6d\x65\x6d\x62\x65\x72\x5f\x73\x74\x61\x74\x65\xa\x9"                                   \
      "\x2e\x63\x66\x69\x5f\x64\x65\x66\x5f\x63\x66\x61\x20\x72\x62\x70\x2c\x20\x30\xa\x9"
#  define VALGRIND_CFI_EPILOGUE                                   \
      "\x6d\x6f\x76\x71\x20\x25\x25\x72\x31\x35\x2c\x20\x25\x25\x72\x62\x70\xa\x9"                                     \
      "\x2e\x63\x66\x69\x5f\x72\x65\x73\x74\x6f\x72\x65\x5f\x73\x74\x61\x74\x65\xa\x9"
#else
#  define __FRAME_POINTER
#  define VALGRIND_CFI_PROLOGUE
#  define VALGRIND_CFI_EPILOGUE
#endif


/* These CALL_FN_ macros assume that on amd64-linux, sizeof(unsigned
   long) == 8. */

/* NB 9 Sept 07.  There is a nasty kludge here in all these CALL_FN_
   macros.  In order not to trash the stack redzone, we need to drop
   %rsp by 128 before the hidden call, and restore afterwards.  The
   nastyness is that it is only by luck that the stack still appears
   to be unwindable during the hidden call - since then the behaviour
   of any routine using this macro does not match what the CFI data
   says.  Sigh.

   Why is this important?  Imagine that a wrapper has a stack
   allocated local, and passes to the hidden call, a pointer to it.
   Because gcc does not know about the hidden call, it may allocate
   that local in the redzone.  Unfortunately the hidden call may then
   trash it before it comes to use it.  So we must step clear of the
   redzone, for the duration of the hidden call, to make it safe.

   Probably the same problem afflicts the other redzone-style ABIs too
   (ppc64-linux); but for those, the stack is
   self describing (none of this CFI nonsense) so at least messing
   with the stack pointer doesn't give a danger of non-unwindable
   stack. */

#define CALL_FN_W_v(lval, orig)                                   \
   do {                                                           \
      volatile OrigFn        _orig = (orig);                      \
      volatile unsigned long _argvec[1];                          \
      volatile unsigned long _res;                                \
      _argvec[0] = (unsigned long)_orig.nraddr;                   \
      __asm__ volatile(                                           \
         VALGRIND_CFI_PROLOGUE                                    \
         "\x73\x75\x62\x71\x20\x24\x31\x32\x38\x2c\x25\x25\x72\x73\x70\xa\x9"                                    \
         "\x6d\x6f\x76\x71\x20\x28\x25\x25\x72\x61\x78\x29\x2c\x20\x25\x25\x72\x61\x78\xa\x9"  /* target->%rax */            \
         VALGRIND_CALL_NOREDIR_RAX                                \
         "\x61\x64\x64\x71\x20\x24\x31\x32\x38\x2c\x25\x25\x72\x73\x70\xa\x9"                                    \
         VALGRIND_CFI_EPILOGUE                                    \
         : /*out*/   "\x3d\x61" (_res)                                  \
         : /*in*/    "\x61" (&_argvec[0]) __FRAME_POINTER            \
         : /*trash*/ "\x63\x63", "\x6d\x65\x6d\x6f\x72\x79", __CALLER_SAVED_REGS, "\x72\x31\x35"   \
      );                                                          \
      lval = (__typeof__(lval)) _res;                             \
   } while (0)

#define CALL_FN_W_W(lval, orig, arg1)                             \
   do {                                                           \
      volatile OrigFn        _orig = (orig);                      \
      volatile unsigned long _argvec[2];                          \
      volatile unsigned long _res;                                \
      _argvec[0] = (unsigned long)_orig.nraddr;                   \
      _argvec[1] = (unsigned long)(arg1);                         \
      __asm__ volatile(                                           \
         VALGRIND_CFI_PROLOGUE                                    \
         "\x73\x75\x62\x71\x20\x24\x31\x32\x38\x2c\x25\x25\x72\x73\x70\xa\x9"                                    \
         "\x6d\x6f\x76\x71\x20\x38\x28\x25\x25\x72\x61\x78\x29\x2c\x20\x25\x25\x72\x64\x69\xa\x9"                               \
         "\x6d\x6f\x76\x71\x20\x28\x25\x25\x72\x61\x78\x29\x2c\x20\x25\x25\x72\x61\x78\xa\x9"  /* target->%rax */            \
         VALGRIND_CALL_NOREDIR_RAX                                \
         "\x61\x64\x64\x71\x20\x24\x31\x32\x38\x2c\x25\x25\x72\x73\x70\xa\x9"                                    \
         VALGRIND_CFI_EPILOGUE                                    \
         : /*out*/   "\x3d\x61" (_res)                                  \
         : /*in*/    "\x61" (&_argvec[0]) __FRAME_POINTER            \
         : /*trash*/ "\x63\x63", "\x6d\x65\x6d\x6f\x72\x79", __CALLER_SAVED_REGS, "\x72\x31\x35"   \
      );                                                          \
      lval = (__typeof__(lval)) _res;                             \
   } while (0)

#define CALL_FN_W_WW(lval, orig, arg1,arg2)                       \
   do {                                                           \
      volatile OrigFn        _orig = (orig);                      \
      volatile unsigned long _argvec[3];                          \
      volatile unsigned long _res;                                \
      _argvec[0] = (unsigned long)_orig.nraddr;                   \
      _argvec[1] = (unsigned long)(arg1);                         \
      _argvec[2] = (unsigned long)(arg2);                         \
      __asm__ volatile(                                           \
         VALGRIND_CFI_PROLOGUE                                    \
         "\x73\x75\x62\x71\x20\x24\x31\x32\x38\x2c\x25\x25\x72\x73\x70\xa\x9"                                    \
         "\x6d\x6f\x76\x71\x20\x31\x36\x28\x25\x25\x72\x61\x78\x29\x2c\x20\x25\x25\x72\x73\x69\xa\x9"                              \
         "\x6d\x6f\x76\x71\x20\x38\x28\x25\x25\x72\x61\x78\x29\x2c\x20\x25\x25\x72\x64\x69\xa\x9"                               \
         "\x6d\x6f\x76\x71\x20\x28\x25\x25\x72\x61\x78\x29\x2c\x20\x25\x25\x72\x61\x78\xa\x9"  /* target->%rax */            \
         VALGRIND_CALL_NOREDIR_RAX                                \
         "\x61\x64\x64\x71\x20\x24\x31\x32\x38\x2c\x25\x25\x72\x73\x70\xa\x9"                                    \
         VALGRIND_CFI_EPILOGUE                                    \
         : /*out*/   "\x3d\x61" (_res)                                  \
         : /*in*/    "\x61" (&_argvec[0]) __FRAME_POINTER            \
         : /*trash*/ "\x63\x63", "\x6d\x65\x6d\x6f\x72\x79", __CALLER_SAVED_REGS, "\x72\x31\x35"   \
      );                                                          \
      lval = (__typeof__(lval)) _res;                             \
   } while (0)

#define CALL_FN_W_WWW(lval, orig, arg1,arg2,arg3)                 \
   do {                                                           \
      volatile OrigFn        _orig = (orig);                      \
      volatile unsigned long _argvec[4];                          \
      volatile unsigned long _res;                                \
      _argvec[0] = (unsigned long)_orig.nraddr;                   \
      _argvec[1] = (unsigned long)(arg1);                         \
      _argvec[2] = (unsigned long)(arg2);                         \
      _argvec[3] = (unsigned long)(arg3);                         \
      __asm__ volatile(                                           \
         VALGRIND_CFI_PROLOGUE                                    \
         "\x73\x75\x62\x71\x20\x24\x31\x32\x38\x2c\x25\x25\x72\x73\x70\xa\x9"                                    \
         "\x6d\x6f\x76\x71\x20\x32\x34\x28\x25\x25\x72\x61\x78\x29\x2c\x20\x25\x25\x72\x64\x78\xa\x9"                              \
         "\x6d\x6f\x76\x71\x20\x31\x36\x28\x25\x25\x72\x61\x78\x29\x2c\x20\x25\x25\x72\x73\x69\xa\x9"                              \
         "\x6d\x6f\x76\x71\x20\x38\x28\x25\x25\x72\x61\x78\x29\x2c\x20\x25\x25\x72\x64\x69\xa\x9"                               \
         "\x6d\x6f\x76\x71\x20\x28\x25\x25\x72\x61\x78\x29\x2c\x20\x25\x25\x72\x61\x78\xa\x9"  /* target->%rax */            \
         VALGRIND_CALL_NOREDIR_RAX                                \
         "\x61\x64\x64\x71\x20\x24\x31\x32\x38\x2c\x25\x25\x72\x73\x70\xa\x9"                                    \
         VALGRIND_CFI_EPILOGUE                                    \
         : /*out*/   "\x3d\x61" (_res)                                  \
         : /*in*/    "\x61" (&_argvec[0]) __FRAME_POINTER            \
         : /*trash*/ "\x63\x63", "\x6d\x65\x6d\x6f\x72\x79", __CALLER_SAVED_REGS, "\x72\x31\x35"   \
      );                                                          \
      lval = (__typeof__(lval)) _res;                             \
   } while (0)

#define CALL_FN_W_WWWW(lval, orig, arg1,arg2,arg3,arg4)           \
   do {                                                           \
      volatile OrigFn        _orig = (orig);                      \
      volatile unsigned long _argvec[5];                          \
      volatile unsigned long _res;                                \
      _argvec[0] = (unsigned long)_orig.nraddr;                   \
      _argvec[1] = (unsigned long)(arg1);                         \
      _argvec[2] = (unsigned long)(arg2);                         \
      _argvec[3] = (unsigned long)(arg3);                         \
      _argvec[4] = (unsigned long)(arg4);                         \
      __asm__ volatile(                                           \
         VALGRIND_CFI_PROLOGUE                                    \
         "\x73\x75\x62\x71\x20\x24\x31\x32\x38\x2c\x25\x25\x72\x73\x70\xa\x9"                                    \
         "\x6d\x6f\x76\x71\x20\x33\x32\x28\x25\x25\x72\x61\x78\x29\x2c\x20\x25\x25\x72\x63\x78\xa\x9"                              \
         "\x6d\x6f\x76\x71\x20\x32\x34\x28\x25\x25\x72\x61\x78\x29\x2c\x20\x25\x25\x72\x64\x78\xa\x9"                              \
         "\x6d\x6f\x76\x71\x20\x31\x36\x28\x25\x25\x72\x61\x78\x29\x2c\x20\x25\x25\x72\x73\x69\xa\x9"                              \
         "\x6d\x6f\x76\x71\x20\x38\x28\x25\x25\x72\x61\x78\x29\x2c\x20\x25\x25\x72\x64\x69\xa\x9"                               \
         "\x6d\x6f\x76\x71\x20\x28\x25\x25\x72\x61\x78\x29\x2c\x20\x25\x25\x72\x61\x78\xa\x9"  /* target->%rax */            \
         VALGRIND_CALL_NOREDIR_RAX                                \
         "\x61\x64\x64\x71\x20\x24\x31\x32\x38\x2c\x25\x25\x72\x73\x70\xa\x9"                                    \
         VALGRIND_CFI_EPILOGUE                                    \
         : /*out*/   "\x3d\x61" (_res)                                  \
         : /*in*/    "\x61" (&_argvec[0]) __FRAME_POINTER            \
         : /*trash*/ "\x63\x63", "\x6d\x65\x6d\x6f\x72\x79", __CALLER_SAVED_REGS, "\x72\x31\x35"   \
      );                                                          \
      lval = (__typeof__(lval)) _res;                             \
   } while (0)

#define CALL_FN_W_5W(lval, orig, arg1,arg2,arg3,arg4,arg5)        \
   do {                                                           \
      volatile OrigFn        _orig = (orig);                      \
      volatile unsigned long _argvec[6];                          \
      volatile unsigned long _res;                                \
      _argvec[0] = (unsigned long)_orig.nraddr;                   \
      _argvec[1] = (unsigned long)(arg1);                         \
      _argvec[2] = (unsigned long)(arg2);                         \
      _argvec[3] = (unsigned long)(arg3);                         \
      _argvec[4] = (unsigned long)(arg4);                         \
      _argvec[5] = (unsigned long)(arg5);                         \
      __asm__ volatile(                                           \
         VALGRIND_CFI_PROLOGUE                                    \
         "\x73\x75\x62\x71\x20\x24\x31\x32\x38\x2c\x25\x25\x72\x73\x70\xa\x9"                                    \
         "\x6d\x6f\x76\x71\x20\x34\x30\x28\x25\x25\x72\x61\x78\x29\x2c\x20\x25\x25\x72\x38\xa\x9"                               \
         "\x6d\x6f\x76\x71\x20\x33\x32\x28\x25\x25\x72\x61\x78\x29\x2c\x20\x25\x25\x72\x63\x78\xa\x9"                              \
         "\x6d\x6f\x76\x71\x20\x32\x34\x28\x25\x25\x72\x61\x78\x29\x2c\x20\x25\x25\x72\x64\x78\xa\x9"                              \
         "\x6d\x6f\x76\x71\x20\x31\x36\x28\x25\x25\x72\x61\x78\x29\x2c\x20\x25\x25\x72\x73\x69\xa\x9"                              \
         "\x6d\x6f\x76\x71\x20\x38\x28\x25\x25\x72\x61\x78\x29\x2c\x20\x25\x25\x72\x64\x69\xa\x9"                               \
         "\x6d\x6f\x76\x71\x20\x28\x25\x25\x72\x61\x78\x29\x2c\x20\x25\x25\x72\x61\x78\xa\x9"  /* target->%rax */            \
         VALGRIND_CALL_NOREDIR_RAX                                \
         "\x61\x64\x64\x71\x20\x24\x31\x32\x38\x2c\x25\x25\x72\x73\x70\xa\x9"                                    \
         VALGRIND_CFI_EPILOGUE                                    \
         : /*out*/   "\x3d\x61" (_res)                                  \
         : /*in*/    "\x61" (&_argvec[0]) __FRAME_POINTER            \
         : /*trash*/ "\x63\x63", "\x6d\x65\x6d\x6f\x72\x79", __CALLER_SAVED_REGS, "\x72\x31\x35"   \
      );                                                          \
      lval = (__typeof__(lval)) _res;                             \
   } while (0)

#define CALL_FN_W_6W(lval, orig, arg1,arg2,arg3,arg4,arg5,arg6)   \
   do {                                                           \
      volatile OrigFn        _orig = (orig);                      \
      volatile unsigned long _argvec[7];                          \
      volatile unsigned long _res;                                \
      _argvec[0] = (unsigned long)_orig.nraddr;                   \
      _argvec[1] = (unsigned long)(arg1);                         \
      _argvec[2] = (unsigned long)(arg2);                         \
      _argvec[3] = (unsigned long)(arg3);                         \
      _argvec[4] = (unsigned long)(arg4);                         \
      _argvec[5] = (unsigned long)(arg5);                         \
      _argvec[6] = (unsigned long)(arg6);                         \
      __asm__ volatile(                                           \
         VALGRIND_CFI_PROLOGUE                                    \
         "\x73\x75\x62\x71\x20\x24\x31\x32\x38\x2c\x25\x25\x72\x73\x70\xa\x9"                                    \
         "\x6d\x6f\x76\x71\x20\x34\x38\x28\x25\x25\x72\x61\x78\x29\x2c\x20\x25\x25\x72\x39\xa\x9"                               \
         "\x6d\x6f\x76\x71\x20\x34\x30\x28\x25\x25\x72\x61\x78\x29\x2c\x20\x25\x25\x72\x38\xa\x9"                               \
         "\x6d\x6f\x76\x71\x20\x33\x32\x28\x25\x25\x72\x61\x78\x29\x2c\x20\x25\x25\x72\x63\x78\xa\x9"                              \
         "\x6d\x6f\x76\x71\x20\x32\x34\x28\x25\x25\x72\x61\x78\x29\x2c\x20\x25\x25\x72\x64\x78\xa\x9"                              \
         "\x6d\x6f\x76\x71\x20\x31\x36\x28\x25\x25\x72\x61\x78\x29\x2c\x20\x25\x25\x72\x73\x69\xa\x9"                              \
         "\x6d\x6f\x76\x71\x20\x38\x28\x25\x25\x72\x61\x78\x29\x2c\x20\x25\x25\x72\x64\x69\xa\x9"                               \
         "\x6d\x6f\x76\x71\x20\x28\x25\x25\x72\x61\x78\x29\x2c\x20\x25\x25\x72\x61\x78\xa\x9"  /* target->%rax */            \
         VALGRIND_CALL_NOREDIR_RAX                                \
         "\x61\x64\x64\x71\x20\x24\x31\x32\x38\x2c\x25\x25\x72\x73\x70\xa\x9"                                    \
         VALGRIND_CFI_EPILOGUE                                    \
         : /*out*/   "\x3d\x61" (_res)                                  \
         : /*in*/    "\x61" (&_argvec[0]) __FRAME_POINTER            \
         : /*trash*/ "\x63\x63", "\x6d\x65\x6d\x6f\x72\x79", __CALLER_SAVED_REGS, "\x72\x31\x35"   \
      );                                                          \
      lval = (__typeof__(lval)) _res;                             \
   } while (0)

#define CALL_FN_W_7W(lval, orig, arg1,arg2,arg3,arg4,arg5,arg6,   \
                                 arg7)                            \
   do {                                                           \
      volatile OrigFn        _orig = (orig);                      \
      volatile unsigned long _argvec[8];                          \
      volatile unsigned long _res;                                \
      _argvec[0] = (unsigned long)_orig.nraddr;                   \
      _argvec[1] = (unsigned long)(arg1);                         \
      _argvec[2] = (unsigned long)(arg2);                         \
      _argvec[3] = (unsigned long)(arg3);                         \
      _argvec[4] = (unsigned long)(arg4);                         \
      _argvec[5] = (unsigned long)(arg5);                         \
      _argvec[6] = (unsigned long)(arg6);                         \
      _argvec[7] = (unsigned long)(arg7);                         \
      __asm__ volatile(                                           \
         VALGRIND_CFI_PROLOGUE                                    \
         "\x73\x75\x62\x71\x20\x24\x31\x33\x36\x2c\x25\x25\x72\x73\x70\xa\x9"                                    \
         "\x70\x75\x73\x68\x71\x20\x35\x36\x28\x25\x25\x72\x61\x78\x29\xa\x9"                                    \
         "\x6d\x6f\x76\x71\x20\x34\x38\x28\x25\x25\x72\x61\x78\x29\x2c\x20\x25\x25\x72\x39\xa\x9"                               \
         "\x6d\x6f\x76\x71\x20\x34\x30\x28\x25\x25\x72\x61\x78\x29\x2c\x20\x25\x25\x72\x38\xa\x9"                               \
         "\x6d\x6f\x76\x71\x20\x33\x32\x28\x25\x25\x72\x61\x78\x29\x2c\x20\x25\x25\x72\x63\x78\xa\x9"                              \
         "\x6d\x6f\x76\x71\x20\x32\x34\x28\x25\x25\x72\x61\x78\x29\x2c\x20\x25\x25\x72\x64\x78\xa\x9"                              \
         "\x6d\x6f\x76\x71\x20\x31\x36\x28\x25\x25\x72\x61\x78\x29\x2c\x20\x25\x25\x72\x73\x69\xa\x9"                              \
         "\x6d\x6f\x76\x71\x20\x38\x28\x25\x25\x72\x61\x78\x29\x2c\x20\x25\x25\x72\x64\x69\xa\x9"                               \
         "\x6d\x6f\x76\x71\x20\x28\x25\x25\x72\x61\x78\x29\x2c\x20\x25\x25\x72\x61\x78\xa\x9"  /* target->%rax */            \
         VALGRIND_CALL_NOREDIR_RAX                                \
         "\x61\x64\x64\x71\x20\x24\x38\x2c\x20\x25\x25\x72\x73\x70\xa"                                       \
         "\x61\x64\x64\x71\x20\x24\x31\x33\x36\x2c\x25\x25\x72\x73\x70\xa\x9"                                    \
         VALGRIND_CFI_EPILOGUE                                    \
         : /*out*/   "\x3d\x61" (_res)                                  \
         : /*in*/    "\x61" (&_argvec[0]) __FRAME_POINTER            \
         : /*trash*/ "\x63\x63", "\x6d\x65\x6d\x6f\x72\x79", __CALLER_SAVED_REGS, "\x72\x31\x35"   \
      );                                                          \
      lval = (__typeof__(lval)) _res;                             \
   } while (0)

#define CALL_FN_W_8W(lval, orig, arg1,arg2,arg3,arg4,arg5,arg6,   \
                                 arg7,arg8)                       \
   do {                                                           \
      volatile OrigFn        _orig = (orig);                      \
      volatile unsigned long _argvec[9];                          \
      volatile unsigned long _res;                                \
      _argvec[0] = (unsigned long)_orig.nraddr;                   \
      _argvec[1] = (unsigned long)(arg1);                         \
      _argvec[2] = (unsigned long)(arg2);                         \
      _argvec[3] = (unsigned long)(arg3);                         \
      _argvec[4] = (unsigned long)(arg4);                         \
      _argvec[5] = (unsigned long)(arg5);                         \
      _argvec[6] = (unsigned long)(arg6);                         \
      _argvec[7] = (unsigned long)(arg7);                         \
      _argvec[8] = (unsigned long)(arg8);                         \
      __asm__ volatile(                                           \
         VALGRIND_CFI_PROLOGUE                                    \
         "\x73\x75\x62\x71\x20\x24\x31\x32\x38\x2c\x25\x25\x72\x73\x70\xa\x9"                                    \
         "\x70\x75\x73\x68\x71\x20\x36\x34\x28\x25\x25\x72\x61\x78\x29\xa\x9"                                    \
         "\x70\x75\x73\x68\x71\x20\x35\x36\x28\x25\x25\x72\x61\x78\x29\xa\x9"                                    \
         "\x6d\x6f\x76\x71\x20\x34\x38\x28\x25\x25\x72\x61\x78\x29\x2c\x20\x25\x25\x72\x39\xa\x9"                               \
         "\x6d\x6f\x76\x71\x20\x34\x30\x28\x25\x25\x72\x61\x78\x29\x2c\x20\x25\x25\x72\x38\xa\x9"                               \
         "\x6d\x6f\x76\x71\x20\x33\x32\x28\x25\x25\x72\x61\x78\x29\x2c\x20\x25\x25\x72\x63\x78\xa\x9"                              \
         "\x6d\x6f\x76\x71\x20\x32\x34\x28\x25\x25\x72\x61\x78\x29\x2c\x20\x25\x25\x72\x64\x78\xa\x9"                              \
         "\x6d\x6f\x76\x71\x20\x31\x36\x28\x25\x25\x72\x61\x78\x29\x2c\x20\x25\x25\x72\x73\x69\xa\x9"                              \
         "\x6d\x6f\x76\x71\x20\x38\x28\x25\x25\x72\x61\x78\x29\x2c\x20\x25\x25\x72\x64\x69\xa\x9"                               \
         "\x6d\x6f\x76\x71\x20\x28\x25\x25\x72\x61\x78\x29\x2c\x20\x25\x25\x72\x61\x78\xa\x9"  /* target->%rax */            \
         VALGRIND_CALL_NOREDIR_RAX                                \
         "\x61\x64\x64\x71\x20\x24\x31\x36\x2c\x20\x25\x25\x72\x73\x70\xa"                                      \
         "\x61\x64\x64\x71\x20\x24\x31\x32\x38\x2c\x25\x25\x72\x73\x70\xa\x9"                                    \
         VALGRIND_CFI_EPILOGUE                                    \
         : /*out*/   "\x3d\x61" (_res)                                  \
         : /*in*/    "\x61" (&_argvec[0]) __FRAME_POINTER            \
         : /*trash*/ "\x63\x63", "\x6d\x65\x6d\x6f\x72\x79", __CALLER_SAVED_REGS, "\x72\x31\x35"   \
      );                                                          \
      lval = (__typeof__(lval)) _res;                             \
   } while (0)

#define CALL_FN_W_9W(lval, orig, arg1,arg2,arg3,arg4,arg5,arg6,   \
                                 arg7,arg8,arg9)                  \
   do {                                                           \
      volatile OrigFn        _orig = (orig);                      \
      volatile unsigned long _argvec[10];                         \
      volatile unsigned long _res;                                \
      _argvec[0] = (unsigned long)_orig.nraddr;                   \
      _argvec[1] = (unsigned long)(arg1);                         \
      _argvec[2] = (unsigned long)(arg2);                         \
      _argvec[3] = (unsigned long)(arg3);                         \
      _argvec[4] = (unsigned long)(arg4);                         \
      _argvec[5] = (unsigned long)(arg5);                         \
      _argvec[6] = (unsigned long)(arg6);                         \
      _argvec[7] = (unsigned long)(arg7);                         \
      _argvec[8] = (unsigned long)(arg8);                         \
      _argvec[9] = (unsigned long)(arg9);                         \
      __asm__ volatile(                                           \
         VALGRIND_CFI_PROLOGUE                                    \
         "\x73\x75\x62\x71\x20\x24\x31\x33\x36\x2c\x25\x25\x72\x73\x70\xa\x9"                                    \
         "\x70\x75\x73\x68\x71\x20\x37\x32\x28\x25\x25\x72\x61\x78\x29\xa\x9"                                    \
         "\x70\x75\x73\x68\x71\x20\x36\x34\x28\x25\x25\x72\x61\x78\x29\xa\x9"                                    \
         "\x70\x75\x73\x68\x71\x20\x35\x36\x28\x25\x25\x72\x61\x78\x29\xa\x9"                                    \
         "\x6d\x6f\x76\x71\x20\x34\x38\x28\x25\x25\x72\x61\x78\x29\x2c\x20\x25\x25\x72\x39\xa\x9"                               \
         "\x6d\x6f\x76\x71\x20\x34\x30\x28\x25\x25\x72\x61\x78\x29\x2c\x20\x25\x25\x72\x38\xa\x9"                               \
         "\x6d\x6f\x76\x71\x20\x33\x32\x28\x25\x25\x72\x61\x78\x29\x2c\x20\x25\x25\x72\x63\x78\xa\x9"                              \
         "\x6d\x6f\x76\x71\x20\x32\x34\x28\x25\x25\x72\x61\x78\x29\x2c\x20\x25\x25\x72\x64\x78\xa\x9"                              \
         "\x6d\x6f\x76\x71\x20\x31\x36\x28\x25\x25\x72\x61\x78\x29\x2c\x20\x25\x25\x72\x73\x69\xa\x9"                              \
         "\x6d\x6f\x76\x71\x20\x38\x28\x25\x25\x72\x61\x78\x29\x2c\x20\x25\x25\x72\x64\x69\xa\x9"                               \
         "\x6d\x6f\x76\x71\x20\x28\x25\x25\x72\x61\x78\x29\x2c\x20\x25\x25\x72\x61\x78\xa\x9"  /* target->%rax */            \
         VALGRIND_CALL_NOREDIR_RAX                                \
         "\x61\x64\x64\x71\x20\x24\x32\x34\x2c\x20\x25\x25\x72\x73\x70\xa"                                      \
         "\x61\x64\x64\x71\x20\x24\x31\x33\x36\x2c\x25\x25\x72\x73\x70\xa\x9"                                    \
         VALGRIND_CFI_EPILOGUE                                    \
         : /*out*/   "\x3d\x61" (_res)                                  \
         : /*in*/    "\x61" (&_argvec[0]) __FRAME_POINTER            \
         : /*trash*/ "\x63\x63", "\x6d\x65\x6d\x6f\x72\x79", __CALLER_SAVED_REGS, "\x72\x31\x35"   \
      );                                                          \
      lval = (__typeof__(lval)) _res;                             \
   } while (0)

#define CALL_FN_W_10W(lval, orig, arg1,arg2,arg3,arg4,arg5,arg6,  \
                                  arg7,arg8,arg9,arg10)           \
   do {                                                           \
      volatile OrigFn        _orig = (orig);                      \
      volatile unsigned long _argvec[11];                         \
      volatile unsigned long _res;                                \
      _argvec[0] = (unsigned long)_orig.nraddr;                   \
      _argvec[1] = (unsigned long)(arg1);                         \
      _argvec[2] = (unsigned long)(arg2);                         \
      _argvec[3] = (unsigned long)(arg3);                         \
      _argvec[4] = (unsigned long)(arg4);                         \
      _argvec[5] = (unsigned long)(arg5);                         \
      _argvec[6] = (unsigned long)(arg6);                         \
      _argvec[7] = (unsigned long)(arg7);                         \
      _argvec[8] = (unsigned long)(arg8);                         \
      _argvec[9] = (unsigned long)(arg9);                         \
      _argvec[10] = (unsigned long)(arg10);                       \
      __asm__ volatile(                                           \
         VALGRIND_CFI_PROLOGUE                                    \
         "\x73\x75\x62\x71\x20\x24\x31\x32\x38\x2c\x25\x25\x72\x73\x70\xa\x9"                                    \
         "\x70\x75\x73\x68\x71\x20\x38\x30\x28\x25\x25\x72\x61\x78\x29\xa\x9"                                    \
         "\x70\x75\x73\x68\x71\x20\x37\x32\x28\x25\x25\x72\x61\x78\x29\xa\x9"                                    \
         "\x70\x75\x73\x68\x71\x20\x36\x34\x28\x25\x25\x72\x61\x78\x29\xa\x9"                                    \
         "\x70\x75\x73\x68\x71\x20\x35\x36\x28\x25\x25\x72\x61\x78\x29\xa\x9"                                    \
         "\x6d\x6f\x76\x71\x20\x34\x38\x28\x25\x25\x72\x61\x78\x29\x2c\x20\x25\x25\x72\x39\xa\x9"                               \
         "\x6d\x6f\x76\x71\x20\x34\x30\x28\x25\x25\x72\x61\x78\x29\x2c\x20\x25\x25\x72\x38\xa\x9"                               \
         "\x6d\x6f\x76\x71\x20\x33\x32\x28\x25\x25\x72\x61\x78\x29\x2c\x20\x25\x25\x72\x63\x78\xa\x9"                              \
         "\x6d\x6f\x76\x71\x20\x32\x34\x28\x25\x25\x72\x61\x78\x29\x2c\x20\x25\x25\x72\x64\x78\xa\x9"                              \
         "\x6d\x6f\x76\x71\x20\x31\x36\x28\x25\x25\x72\x61\x78\x29\x2c\x20\x25\x25\x72\x73\x69\xa\x9"                              \
         "\x6d\x6f\x76\x71\x20\x38\x28\x25\x25\x72\x61\x78\x29\x2c\x20\x25\x25\x72\x64\x69\xa\x9"                               \
         "\x6d\x6f\x76\x71\x20\x28\x25\x25\x72\x61\x78\x29\x2c\x20\x25\x25\x72\x61\x78\xa\x9"  /* target->%rax */            \
         VALGRIND_CALL_NOREDIR_RAX                                \
         "\x61\x64\x64\x71\x20\x24\x33\x32\x2c\x20\x25\x25\x72\x73\x70\xa"                                      \
         "\x61\x64\x64\x71\x20\x24\x31\x32\x38\x2c\x25\x25\x72\x73\x70\xa\x9"                                    \
         VALGRIND_CFI_EPILOGUE                                    \
         : /*out*/   "\x3d\x61" (_res)                                  \
         : /*in*/    "\x61" (&_argvec[0]) __FRAME_POINTER            \
         : /*trash*/ "\x63\x63", "\x6d\x65\x6d\x6f\x72\x79", __CALLER_SAVED_REGS, "\x72\x31\x35"   \
      );                                                          \
      lval = (__typeof__(lval)) _res;                             \
   } while (0)

#define CALL_FN_W_11W(lval, orig, arg1,arg2,arg3,arg4,arg5,arg6,  \
                                  arg7,arg8,arg9,arg10,arg11)     \
   do {                                                           \
      volatile OrigFn        _orig = (orig);                      \
      volatile unsigned long _argvec[12];                         \
      volatile unsigned long _res;                                \
      _argvec[0] = (unsigned long)_orig.nraddr;                   \
      _argvec[1] = (unsigned long)(arg1);                         \
      _argvec[2] = (unsigned long)(arg2);                         \
      _argvec[3] = (unsigned long)(arg3);                         \
      _argvec[4] = (unsigned long)(arg4);                         \
      _argvec[5] = (unsigned long)(arg5);                         \
      _argvec[6] = (unsigned long)(arg6);                         \
      _argvec[7] = (unsigned long)(arg7);                         \
      _argvec[8] = (unsigned long)(arg8);                         \
      _argvec[9] = (unsigned long)(arg9);                         \
      _argvec[10] = (unsigned long)(arg10);                       \
      _argvec[11] = (unsigned long)(arg11);                       \
      __asm__ volatile(                                           \
         VALGRIND_CFI_PROLOGUE                                    \
         "\x73\x75\x62\x71\x20\x24\x31\x33\x36\x2c\x25\x25\x72\x73\x70\xa\x9"                                    \
         "\x70\x75\x73\x68\x71\x20\x38\x38\x28\x25\x25\x72\x61\x78\x29\xa\x9"                                    \
         "\x70\x75\x73\x68\x71\x20\x38\x30\x28\x25\x25\x72\x61\x78\x29\xa\x9"                                    \
         "\x70\x75\x73\x68\x71\x20\x37\x32\x28\x25\x25\x72\x61\x78\x29\xa\x9"                                    \
         "\x70\x75\x73\x68\x71\x20\x36\x34\x28\x25\x25\x72\x61\x78\x29\xa\x9"                                    \
         "\x70\x75\x73\x68\x71\x20\x35\x36\x28\x25\x25\x72\x61\x78\x29\xa\x9"                                    \
         "\x6d\x6f\x76\x71\x20\x34\x38\x28\x25\x25\x72\x61\x78\x29\x2c\x20\x25\x25\x72\x39\xa\x9"                               \
         "\x6d\x6f\x76\x71\x20\x34\x30\x28\x25\x25\x72\x61\x78\x29\x2c\x20\x25\x25\x72\x38\xa\x9"                               \
         "\x6d\x6f\x76\x71\x20\x33\x32\x28\x25\x25\x72\x61\x78\x29\x2c\x20\x25\x25\x72\x63\x78\xa\x9"                              \
         "\x6d\x6f\x76\x71\x20\x32\x34\x28\x25\x25\x72\x61\x78\x29\x2c\x20\x25\x25\x72\x64\x78\xa\x9"                              \
         "\x6d\x6f\x76\x71\x20\x31\x36\x28\x25\x25\x72\x61\x78\x29\x2c\x20\x25\x25\x72\x73\x69\xa\x9"                              \
         "\x6d\x6f\x76\x71\x20\x38\x28\x25\x25\x72\x61\x78\x29\x2c\x20\x25\x25\x72\x64\x69\xa\x9"                               \
         "\x6d\x6f\x76\x71\x20\x28\x25\x25\x72\x61\x78\x29\x2c\x20\x25\x25\x72\x61\x78\xa\x9"  /* target->%rax */            \
         VALGRIND_CALL_NOREDIR_RAX                                \
         "\x61\x64\x64\x71\x20\x24\x34\x30\x2c\x20\x25\x25\x72\x73\x70\xa"                                      \
         "\x61\x64\x64\x71\x20\x24\x31\x33\x36\x2c\x25\x25\x72\x73\x70\xa\x9"                                    \
         VALGRIND_CFI_EPILOGUE                                    \
         : /*out*/   "\x3d\x61" (_res)                                  \
         : /*in*/    "\x61" (&_argvec[0]) __FRAME_POINTER            \
         : /*trash*/ "\x63\x63", "\x6d\x65\x6d\x6f\x72\x79", __CALLER_SAVED_REGS, "\x72\x31\x35"   \
      );                                                          \
      lval = (__typeof__(lval)) _res;                             \
   } while (0)

#define CALL_FN_W_12W(lval, orig, arg1,arg2,arg3,arg4,arg5,arg6,  \
                                arg7,arg8,arg9,arg10,arg11,arg12) \
   do {                                                           \
      volatile OrigFn        _orig = (orig);                      \
      volatile unsigned long _argvec[13];                         \
      volatile unsigned long _res;                                \
      _argvec[0] = (unsigned long)_orig.nraddr;                   \
      _argvec[1] = (unsigned long)(arg1);                         \
      _argvec[2] = (unsigned long)(arg2);                         \
      _argvec[3] = (unsigned long)(arg3);                         \
      _argvec[4] = (unsigned long)(arg4);                         \
      _argvec[5] = (unsigned long)(arg5);                         \
      _argvec[6] = (unsigned long)(arg6);                         \
      _argvec[7] = (unsigned long)(arg7);                         \
      _argvec[8] = (unsigned long)(arg8);                         \
      _argvec[9] = (unsigned long)(arg9);                         \
      _argvec[10] = (unsigned long)(arg10);                       \
      _argvec[11] = (unsigned long)(arg11);                       \
      _argvec[12] = (unsigned long)(arg12);                       \
      __asm__ volatile(                                           \
         VALGRIND_CFI_PROLOGUE                                    \
         "\x73\x75\x62\x71\x20\x24\x31\x32\x38\x2c\x25\x25\x72\x73\x70\xa\x9"                                    \
         "\x70\x75\x73\x68\x71\x20\x39\x36\x28\x25\x25\x72\x61\x78\x29\xa\x9"                                    \
         "\x70\x75\x73\x68\x71\x20\x38\x38\x28\x25\x25\x72\x61\x78\x29\xa\x9"                                    \
         "\x70\x75\x73\x68\x71\x20\x38\x30\x28\x25\x25\x72\x61\x78\x29\xa\x9"                                    \
         "\x70\x75\x73\x68\x71\x20\x37\x32\x28\x25\x25\x72\x61\x78\x29\xa\x9"                                    \
         "\x70\x75\x73\x68\x71\x20\x36\x34\x28\x25\x25\x72\x61\x78\x29\xa\x9"                                    \
         "\x70\x75\x73\x68\x71\x20\x35\x36\x28\x25\x25\x72\x61\x78\x29\xa\x9"                                    \
         "\x6d\x6f\x76\x71\x20\x34\x38\x28\x25\x25\x72\x61\x78\x29\x2c\x20\x25\x25\x72\x39\xa\x9"                               \
         "\x6d\x6f\x76\x71\x20\x34\x30\x28\x25\x25\x72\x61\x78\x29\x2c\x20\x25\x25\x72\x38\xa\x9"                               \
         "\x6d\x6f\x76\x71\x20\x33\x32\x28\x25\x25\x72\x61\x78\x29\x2c\x20\x25\x25\x72\x63\x78\xa\x9"                              \
         "\x6d\x6f\x76\x71\x20\x32\x34\x28\x25\x25\x72\x61\x78\x29\x2c\x20\x25\x25\x72\x64\x78\xa\x9"                              \
         "\x6d\x6f\x76\x71\x20\x31\x36\x28\x25\x25\x72\x61\x78\x29\x2c\x20\x25\x25\x72\x73\x69\xa\x9"                              \
         "\x6d\x6f\x76\x71\x20\x38\x28\x25\x25\x72\x61\x78\x29\x2c\x20\x25\x25\x72\x64\x69\xa\x9"                               \
         "\x6d\x6f\x76\x71\x20\x28\x25\x25\x72\x61\x78\x29\x2c\x20\x25\x25\x72\x61\x78\xa\x9"  /* target->%rax */            \
         VALGRIND_CALL_NOREDIR_RAX                                \
         "\x61\x64\x64\x71\x20\x24\x34\x38\x2c\x20\x25\x25\x72\x73\x70\xa"                                      \
         "\x61\x64\x64\x71\x20\x24\x31\x32\x38\x2c\x25\x25\x72\x73\x70\xa\x9"                                    \
         VALGRIND_CFI_EPILOGUE                                    \
         : /*out*/   "\x3d\x61" (_res)                                  \
         : /*in*/    "\x61" (&_argvec[0]) __FRAME_POINTER            \
         : /*trash*/ "\x63\x63", "\x6d\x65\x6d\x6f\x72\x79", __CALLER_SAVED_REGS, "\x72\x31\x35"   \
      );                                                          \
      lval = (__typeof__(lval)) _res;                             \
   } while (0)

#endif /* PLAT_amd64_linux || PLAT_amd64_darwin */

/* ------------------------ ppc32-linux ------------------------ */

#if defined(PLAT_ppc32_linux)

/* This is useful for finding out about the on-stack stuff:

   extern int f9  ( int,int,int,int,int,int,int,int,int );
   extern int f10 ( int,int,int,int,int,int,int,int,int,int );
   extern int f11 ( int,int,int,int,int,int,int,int,int,int,int );
   extern int f12 ( int,int,int,int,int,int,int,int,int,int,int,int );

   int g9 ( void ) {
      return f9(11,22,33,44,55,66,77,88,99);
   }
   int g10 ( void ) {
      return f10(11,22,33,44,55,66,77,88,99,110);
   }
   int g11 ( void ) {
      return f11(11,22,33,44,55,66,77,88,99,110,121);
   }
   int g12 ( void ) {
      return f12(11,22,33,44,55,66,77,88,99,110,121,132);
   }
*/

/* ARGREGS: r3 r4 r5 r6 r7 r8 r9 r10 (the rest on stack somewhere) */

/* These regs are trashed by the hidden call. */
#define __CALLER_SAVED_REGS                                       \
   "\x6c\x72", "\x63\x74\x72", "\x78\x65\x72",                                            \
   "\x63\x72\x30", "\x63\x72\x31", "\x63\x72\x32", "\x63\x72\x33", "\x63\x72\x34", "\x63\x72\x35", "\x63\x72\x36", "\x63\x72\x37",        \
   "\x72\x30", "\x72\x32", "\x72\x33", "\x72\x34", "\x72\x35", "\x72\x36", "\x72\x37", "\x72\x38", "\x72\x39", "\x72\x31\x30",   \
   "\x72\x31\x31", "\x72\x31\x32", "\x72\x31\x33"

/* These CALL_FN_ macros assume that on ppc32-linux,
   sizeof(unsigned long) == 4. */

#define CALL_FN_W_v(lval, orig)                                   \
   do {                                                           \
      volatile OrigFn        _orig = (orig);                      \
      volatile unsigned long _argvec[1];                          \
      volatile unsigned long _res;                                \
      _argvec[0] = (unsigned long)_orig.nraddr;                   \
      __asm__ volatile(                                           \
         "\x6d\x72\x20\x31\x31\x2c\x25\x31\xa\x9"                                           \
         "\x6c\x77\x7a\x20\x31\x31\x2c\x30\x28\x31\x31\x29\xa\x9"  /* target->r11 */                    \
         VALGRIND_BRANCH_AND_LINK_TO_NOREDIR_R11                  \
         "\x6d\x72\x20\x25\x30\x2c\x33"                                                \
         : /*out*/   "\x3d\x72" (_res)                                  \
         : /*in*/    "\x72" (&_argvec[0])                            \
         : /*trash*/ "\x63\x63", "\x6d\x65\x6d\x6f\x72\x79", __CALLER_SAVED_REGS          \
      );                                                          \
      lval = (__typeof__(lval)) _res;                             \
   } while (0)

#define CALL_FN_W_W(lval, orig, arg1)                             \
   do {                                                           \
      volatile OrigFn        _orig = (orig);                      \
      volatile unsigned long _argvec[2];                          \
      volatile unsigned long _res;                                \
      _argvec[0] = (unsigned long)_orig.nraddr;                   \
      _argvec[1] = (unsigned long)arg1;                           \
      __asm__ volatile(                                           \
         "\x6d\x72\x20\x31\x31\x2c\x25\x31\xa\x9"                                           \
         "\x6c\x77\x7a\x20\x33\x2c\x34\x28\x31\x31\x29\xa\x9"   /* arg1->r3 */                       \
         "\x6c\x77\x7a\x20\x31\x31\x2c\x30\x28\x31\x31\x29\xa\x9"  /* target->r11 */                    \
         VALGRIND_BRANCH_AND_LINK_TO_NOREDIR_R11                  \
         "\x6d\x72\x20\x25\x30\x2c\x33"                                                \
         : /*out*/   "\x3d\x72" (_res)                                  \
         : /*in*/    "\x72" (&_argvec[0])                            \
         : /*trash*/ "\x63\x63", "\x6d\x65\x6d\x6f\x72\x79", __CALLER_SAVED_REGS          \
      );                                                          \
      lval = (__typeof__(lval)) _res;                             \
   } while (0)

#define CALL_FN_W_WW(lval, orig, arg1,arg2)                       \
   do {                                                           \
      volatile OrigFn        _orig = (orig);                      \
      volatile unsigned long _argvec[3];                          \
      volatile unsigned long _res;                                \
      _argvec[0] = (unsigned long)_orig.nraddr;                   \
      _argvec[1] = (unsigned long)arg1;                           \
      _argvec[2] = (unsigned long)arg2;                           \
      __asm__ volatile(                                           \
         "\x6d\x72\x20\x31\x31\x2c\x25\x31\xa\x9"                                           \
         "\x6c\x77\x7a\x20\x33\x2c\x34\x28\x31\x31\x29\xa\x9"   /* arg1->r3 */                       \
         "\x6c\x77\x7a\x20\x34\x2c\x38\x28\x31\x31\x29\xa\x9"                                        \
         "\x6c\x77\x7a\x20\x31\x31\x2c\x30\x28\x31\x31\x29\xa\x9"  /* target->r11 */                    \
         VALGRIND_BRANCH_AND_LINK_TO_NOREDIR_R11                  \
         "\x6d\x72\x20\x25\x30\x2c\x33"                                                \
         : /*out*/   "\x3d\x72" (_res)                                  \
         : /*in*/    "\x72" (&_argvec[0])                            \
         : /*trash*/ "\x63\x63", "\x6d\x65\x6d\x6f\x72\x79", __CALLER_SAVED_REGS          \
      );                                                          \
      lval = (__typeof__(lval)) _res;                             \
   } while (0)

#define CALL_FN_W_WWW(lval, orig, arg1,arg2,arg3)                 \
   do {                                                           \
      volatile OrigFn        _orig = (orig);                      \
      volatile unsigned long _argvec[4];                          \
      volatile unsigned long _res;                                \
      _argvec[0] = (unsigned long)_orig.nraddr;                   \
      _argvec[1] = (unsigned long)arg1;                           \
      _argvec[2] = (unsigned long)arg2;                           \
      _argvec[3] = (unsigned long)arg3;                           \
      __asm__ volatile(                                           \
         "\x6d\x72\x20\x31\x31\x2c\x25\x31\xa\x9"                                           \
         "\x6c\x77\x7a\x20\x33\x2c\x34\x28\x31\x31\x29\xa\x9"   /* arg1->r3 */                       \
         "\x6c\x77\x7a\x20\x34\x2c\x38\x28\x31\x31\x29\xa\x9"                                        \
         "\x6c\x77\x7a\x20\x35\x2c\x31\x32\x28\x31\x31\x29\xa\x9"                                       \
         "\x6c\x77\x7a\x20\x31\x31\x2c\x30\x28\x31\x31\x29\xa\x9"  /* target->r11 */                    \
         VALGRIND_BRANCH_AND_LINK_TO_NOREDIR_R11                  \
         "\x6d\x72\x20\x25\x30\x2c\x33"                                                \
         : /*out*/   "\x3d\x72" (_res)                                  \
         : /*in*/    "\x72" (&_argvec[0])                            \
         : /*trash*/ "\x63\x63", "\x6d\x65\x6d\x6f\x72\x79", __CALLER_SAVED_REGS          \
      );                                                          \
      lval = (__typeof__(lval)) _res;                             \
   } while (0)

#define CALL_FN_W_WWWW(lval, orig, arg1,arg2,arg3,arg4)           \
   do {                                                           \
      volatile OrigFn        _orig = (orig);                      \
      volatile unsigned long _argvec[5];                          \
      volatile unsigned long _res;                                \
      _argvec[0] = (unsigned long)_orig.nraddr;                   \
      _argvec[1] = (unsigned long)arg1;                           \
      _argvec[2] = (unsigned long)arg2;                           \
      _argvec[3] = (unsigned long)arg3;                           \
      _argvec[4] = (unsigned long)arg4;                           \
      __asm__ volatile(                                           \
         "\x6d\x72\x20\x31\x31\x2c\x25\x31\xa\x9"                                           \
         "\x6c\x77\x7a\x20\x33\x2c\x34\x28\x31\x31\x29\xa\x9"   /* arg1->r3 */                       \
         "\x6c\x77\x7a\x20\x34\x2c\x38\x28\x31\x31\x29\xa\x9"                                        \
         "\x6c\x77\x7a\x20\x35\x2c\x31\x32\x28\x31\x31\x29\xa\x9"                                       \
         "\x6c\x77\x7a\x20\x36\x2c\x31\x36\x28\x31\x31\x29\xa\x9"  /* arg4->r6 */                       \
         "\x6c\x77\x7a\x20\x31\x31\x2c\x30\x28\x31\x31\x29\xa\x9"  /* target->r11 */                    \
         VALGRIND_BRANCH_AND_LINK_TO_NOREDIR_R11                  \
         "\x6d\x72\x20\x25\x30\x2c\x33"                                                \
         : /*out*/   "\x3d\x72" (_res)                                  \
         : /*in*/    "\x72" (&_argvec[0])                            \
         : /*trash*/ "\x63\x63", "\x6d\x65\x6d\x6f\x72\x79", __CALLER_SAVED_REGS          \
      );                                                          \
      lval = (__typeof__(lval)) _res;                             \
   } while (0)

#define CALL_FN_W_5W(lval, orig, arg1,arg2,arg3,arg4,arg5)        \
   do {                                                           \
      volatile OrigFn        _orig = (orig);                      \
      volatile unsigned long _argvec[6];                          \
      volatile unsigned long _res;                                \
      _argvec[0] = (unsigned long)_orig.nraddr;                   \
      _argvec[1] = (unsigned long)arg1;                           \
      _argvec[2] = (unsigned long)arg2;                           \
      _argvec[3] = (unsigned long)arg3;                           \
      _argvec[4] = (unsigned long)arg4;                           \
      _argvec[5] = (unsigned long)arg5;                           \
      __asm__ volatile(                                           \
         "\x6d\x72\x20\x31\x31\x2c\x25\x31\xa\x9"                                           \
         "\x6c\x77\x7a\x20\x33\x2c\x34\x28\x31\x31\x29\xa\x9"   /* arg1->r3 */                       \
         "\x6c\x77\x7a\x20\x34\x2c\x38\x28\x31\x31\x29\xa\x9"                                        \
         "\x6c\x77\x7a\x20\x35\x2c\x31\x32\x28\x31\x31\x29\xa\x9"                                       \
         "\x6c\x77\x7a\x20\x36\x2c\x31\x36\x28\x31\x31\x29\xa\x9"  /* arg4->r6 */                       \
         "\x6c\x77\x7a\x20\x37\x2c\x32\x30\x28\x31\x31\x29\xa\x9"                                       \
         "\x6c\x77\x7a\x20\x31\x31\x2c\x30\x28\x31\x31\x29\xa\x9"  /* target->r11 */                    \
         VALGRIND_BRANCH_AND_LINK_TO_NOREDIR_R11                  \
         "\x6d\x72\x20\x25\x30\x2c\x33"                                                \
         : /*out*/   "\x3d\x72" (_res)                                  \
         : /*in*/    "\x72" (&_argvec[0])                            \
         : /*trash*/ "\x63\x63", "\x6d\x65\x6d\x6f\x72\x79", __CALLER_SAVED_REGS          \
      );                                                          \
      lval = (__typeof__(lval)) _res;                             \
   } while (0)

#define CALL_FN_W_6W(lval, orig, arg1,arg2,arg3,arg4,arg5,arg6)   \
   do {                                                           \
      volatile OrigFn        _orig = (orig);                      \
      volatile unsigned long _argvec[7];                          \
      volatile unsigned long _res;                                \
      _argvec[0] = (unsigned long)_orig.nraddr;                   \
      _argvec[1] = (unsigned long)arg1;                           \
      _argvec[2] = (unsigned long)arg2;                           \
      _argvec[3] = (unsigned long)arg3;                           \
      _argvec[4] = (unsigned long)arg4;                           \
      _argvec[5] = (unsigned long)arg5;                           \
      _argvec[6] = (unsigned long)arg6;                           \
      __asm__ volatile(                                           \
         "\x6d\x72\x20\x31\x31\x2c\x25\x31\xa\x9"                                           \
         "\x6c\x77\x7a\x20\x33\x2c\x34\x28\x31\x31\x29\xa\x9"   /* arg1->r3 */                       \
         "\x6c\x77\x7a\x20\x34\x2c\x38\x28\x31\x31\x29\xa\x9"                                        \
         "\x6c\x77\x7a\x20\x35\x2c\x31\x32\x28\x31\x31\x29\xa\x9"                                       \
         "\x6c\x77\x7a\x20\x36\x2c\x31\x36\x28\x31\x31\x29\xa\x9"  /* arg4->r6 */                       \
         "\x6c\x77\x7a\x20\x37\x2c\x32\x30\x28\x31\x31\x29\xa\x9"                                       \
         "\x6c\x77\x7a\x20\x38\x2c\x32\x34\x28\x31\x31\x29\xa\x9"                                       \
         "\x6c\x77\x7a\x20\x31\x31\x2c\x30\x28\x31\x31\x29\xa\x9"  /* target->r11 */                    \
         VALGRIND_BRANCH_AND_LINK_TO_NOREDIR_R11                  \
         "\x6d\x72\x20\x25\x30\x2c\x33"                                                \
         : /*out*/   "\x3d\x72" (_res)                                  \
         : /*in*/    "\x72" (&_argvec[0])                            \
         : /*trash*/ "\x63\x63", "\x6d\x65\x6d\x6f\x72\x79", __CALLER_SAVED_REGS          \
      );                                                          \
      lval = (__typeof__(lval)) _res;                             \
   } while (0)

#define CALL_FN_W_7W(lval, orig, arg1,arg2,arg3,arg4,arg5,arg6,   \
                                 arg7)                            \
   do {                                                           \
      volatile OrigFn        _orig = (orig);                      \
      volatile unsigned long _argvec[8];                          \
      volatile unsigned long _res;                                \
      _argvec[0] = (unsigned long)_orig.nraddr;                   \
      _argvec[1] = (unsigned long)arg1;                           \
      _argvec[2] = (unsigned long)arg2;                           \
      _argvec[3] = (unsigned long)arg3;                           \
      _argvec[4] = (unsigned long)arg4;                           \
      _argvec[5] = (unsigned long)arg5;                           \
      _argvec[6] = (unsigned long)arg6;                           \
      _argvec[7] = (unsigned long)arg7;                           \
      __asm__ volatile(                                           \
         "\x6d\x72\x20\x31\x31\x2c\x25\x31\xa\x9"                                           \
         "\x6c\x77\x7a\x20\x33\x2c\x34\x28\x31\x31\x29\xa\x9"   /* arg1->r3 */                       \
         "\x6c\x77\x7a\x20\x34\x2c\x38\x28\x31\x31\x29\xa\x9"                                        \
         "\x6c\x77\x7a\x20\x35\x2c\x31\x32\x28\x31\x31\x29\xa\x9"                                       \
         "\x6c\x77\x7a\x20\x36\x2c\x31\x36\x28\x31\x31\x29\xa\x9"  /* arg4->r6 */                       \
         "\x6c\x77\x7a\x20\x37\x2c\x32\x30\x28\x31\x31\x29\xa\x9"                                       \
         "\x6c\x77\x7a\x20\x38\x2c\x32\x34\x28\x31\x31\x29\xa\x9"                                       \
         "\x6c\x77\x7a\x20\x39\x2c\x32\x38\x28\x31\x31\x29\xa\x9"                                       \
         "\x6c\x77\x7a\x20\x31\x31\x2c\x30\x28\x31\x31\x29\xa\x9"  /* target->r11 */                    \
         VALGRIND_BRANCH_AND_LINK_TO_NOREDIR_R11                  \
         "\x6d\x72\x20\x25\x30\x2c\x33"                                                \
         : /*out*/   "\x3d\x72" (_res)                                  \
         : /*in*/    "\x72" (&_argvec[0])                            \
         : /*trash*/ "\x63\x63", "\x6d\x65\x6d\x6f\x72\x79", __CALLER_SAVED_REGS          \
      );                                                          \
      lval = (__typeof__(lval)) _res;                             \
   } while (0)

#define CALL_FN_W_8W(lval, orig, arg1,arg2,arg3,arg4,arg5,arg6,   \
                                 arg7,arg8)                       \
   do {                                                           \
      volatile OrigFn        _orig = (orig);                      \
      volatile unsigned long _argvec[9];                          \
      volatile unsigned long _res;                                \
      _argvec[0] = (unsigned long)_orig.nraddr;                   \
      _argvec[1] = (unsigned long)arg1;                           \
      _argvec[2] = (unsigned long)arg2;                           \
      _argvec[3] = (unsigned long)arg3;                           \
      _argvec[4] = (unsigned long)arg4;                           \
      _argvec[5] = (unsigned long)arg5;                           \
      _argvec[6] = (unsigned long)arg6;                           \
      _argvec[7] = (unsigned long)arg7;                           \
      _argvec[8] = (unsigned long)arg8;                           \
      __asm__ volatile(                                           \
         "\x6d\x72\x20\x31\x31\x2c\x25\x31\xa\x9"                                           \
         "\x6c\x77\x7a\x20\x33\x2c\x34\x28\x31\x31\x29\xa\x9"   /* arg1->r3 */                       \
         "\x6c\x77\x7a\x20\x34\x2c\x38\x28\x31\x31\x29\xa\x9"                                        \
         "\x6c\x77\x7a\x20\x35\x2c\x31\x32\x28\x31\x31\x29\xa\x9"                                       \
         "\x6c\x77\x7a\x20\x36\x2c\x31\x36\x28\x31\x31\x29\xa\x9"  /* arg4->r6 */                       \
         "\x6c\x77\x7a\x20\x37\x2c\x32\x30\x28\x31\x31\x29\xa\x9"                                       \
         "\x6c\x77\x7a\x20\x38\x2c\x32\x34\x28\x31\x31\x29\xa\x9"                                       \
         "\x6c\x77\x7a\x20\x39\x2c\x32\x38\x28\x31\x31\x29\xa\x9"                                       \
         "\x6c\x77\x7a\x20\x31\x30\x2c\x33\x32\x28\x31\x31\x29\xa\x9" /* arg8->r10 */                      \
         "\x6c\x77\x7a\x20\x31\x31\x2c\x30\x28\x31\x31\x29\xa\x9"  /* target->r11 */                    \
         VALGRIND_BRANCH_AND_LINK_TO_NOREDIR_R11                  \
         "\x6d\x72\x20\x25\x30\x2c\x33"                                                \
         : /*out*/   "\x3d\x72" (_res)                                  \
         : /*in*/    "\x72" (&_argvec[0])                            \
         : /*trash*/ "\x63\x63", "\x6d\x65\x6d\x6f\x72\x79", __CALLER_SAVED_REGS          \
      );                                                          \
      lval = (__typeof__(lval)) _res;                             \
   } while (0)

#define CALL_FN_W_9W(lval, orig, arg1,arg2,arg3,arg4,arg5,arg6,   \
                                 arg7,arg8,arg9)                  \
   do {                                                           \
      volatile OrigFn        _orig = (orig);                      \
      volatile unsigned long _argvec[10];                         \
      volatile unsigned long _res;                                \
      _argvec[0] = (unsigned long)_orig.nraddr;                   \
      _argvec[1] = (unsigned long)arg1;                           \
      _argvec[2] = (unsigned long)arg2;                           \
      _argvec[3] = (unsigned long)arg3;                           \
      _argvec[4] = (unsigned long)arg4;                           \
      _argvec[5] = (unsigned long)arg5;                           \
      _argvec[6] = (unsigned long)arg6;                           \
      _argvec[7] = (unsigned long)arg7;                           \
      _argvec[8] = (unsigned long)arg8;                           \
      _argvec[9] = (unsigned long)arg9;                           \
      __asm__ volatile(                                           \
         "\x6d\x72\x20\x31\x31\x2c\x25\x31\xa\x9"                                           \
         "\x61\x64\x64\x69\x20\x31\x2c\x31\x2c\x2d\x31\x36\xa\x9"                                       \
         /* arg9 */                                               \
         "\x6c\x77\x7a\x20\x33\x2c\x33\x36\x28\x31\x31\x29\xa\x9"                                       \
         "\x73\x74\x77\x20\x33\x2c\x38\x28\x31\x29\xa\x9"                                         \
         /* args1-8 */                                            \
         "\x6c\x77\x7a\x20\x33\x2c\x34\x28\x31\x31\x29\xa\x9"   /* arg1->r3 */                       \
         "\x6c\x77\x7a\x20\x34\x2c\x38\x28\x31\x31\x29\xa\x9"                                        \
         "\x6c\x77\x7a\x20\x35\x2c\x31\x32\x28\x31\x31\x29\xa\x9"                                       \
         "\x6c\x77\x7a\x20\x36\x2c\x31\x36\x28\x31\x31\x29\xa\x9"  /* arg4->r6 */                       \
         "\x6c\x77\x7a\x20\x37\x2c\x32\x30\x28\x31\x31\x29\xa\x9"                                       \
         "\x6c\x77\x7a\x20\x38\x2c\x32\x34\x28\x31\x31\x29\xa\x9"                                       \
         "\x6c\x77\x7a\x20\x39\x2c\x32\x38\x28\x31\x31\x29\xa\x9"                                       \
         "\x6c\x77\x7a\x20\x31\x30\x2c\x33\x32\x28\x31\x31\x29\xa\x9" /* arg8->r10 */                      \
         "\x6c\x77\x7a\x20\x31\x31\x2c\x30\x28\x31\x31\x29\xa\x9"  /* target->r11 */                    \
         VALGRIND_BRANCH_AND_LINK_TO_NOREDIR_R11                  \
         "\x61\x64\x64\x69\x20\x31\x2c\x31\x2c\x31\x36\xa\x9"                                        \
         "\x6d\x72\x20\x25\x30\x2c\x33"                                                \
         : /*out*/   "\x3d\x72" (_res)                                  \
         : /*in*/    "\x72" (&_argvec[0])                            \
         : /*trash*/ "\x63\x63", "\x6d\x65\x6d\x6f\x72\x79", __CALLER_SAVED_REGS          \
      );                                                          \
      lval = (__typeof__(lval)) _res;                             \
   } while (0)

#define CALL_FN_W_10W(lval, orig, arg1,arg2,arg3,arg4,arg5,arg6,  \
                                  arg7,arg8,arg9,arg10)           \
   do {                                                           \
      volatile OrigFn        _orig = (orig);                      \
      volatile unsigned long _argvec[11];                         \
      volatile unsigned long _res;                                \
      _argvec[0] = (unsigned long)_orig.nraddr;                   \
      _argvec[1] = (unsigned long)arg1;                           \
      _argvec[2] = (unsigned long)arg2;                           \
      _argvec[3] = (unsigned long)arg3;                           \
      _argvec[4] = (unsigned long)arg4;                           \
      _argvec[5] = (unsigned long)arg5;                           \
      _argvec[6] = (unsigned long)arg6;                           \
      _argvec[7] = (unsigned long)arg7;                           \
      _argvec[8] = (unsigned long)arg8;                           \
      _argvec[9] = (unsigned long)arg9;                           \
      _argvec[10] = (unsigned long)arg10;                         \
      __asm__ volatile(                                           \
         "\x6d\x72\x20\x31\x31\x2c\x25\x31\xa\x9"                                           \
         "\x61\x64\x64\x69\x20\x31\x2c\x31\x2c\x2d\x31\x36\xa\x9"                                       \
         /* arg10 */                                              \
         "\x6c\x77\x7a\x20\x33\x2c\x34\x30\x28\x31\x31\x29\xa\x9"                                       \
         "\x73\x74\x77\x20\x33\x2c\x31\x32\x28\x31\x29\xa\x9"                                        \
         /* arg9 */                                               \
         "\x6c\x77\x7a\x20\x33\x2c\x33\x36\x28\x31\x31\x29\xa\x9"                                       \
         "\x73\x74\x77\x20\x33\x2c\x38\x28\x31\x29\xa\x9"                                         \
         /* args1-8 */                                            \
         "\x6c\x77\x7a\x20\x33\x2c\x34\x28\x31\x31\x29\xa\x9"   /* arg1->r3 */                       \
         "\x6c\x77\x7a\x20\x34\x2c\x38\x28\x31\x31\x29\xa\x9"                                        \
         "\x6c\x77\x7a\x20\x35\x2c\x31\x32\x28\x31\x31\x29\xa\x9"                                       \
         "\x6c\x77\x7a\x20\x36\x2c\x31\x36\x28\x31\x31\x29\xa\x9"  /* arg4->r6 */                       \
         "\x6c\x77\x7a\x20\x37\x2c\x32\x30\x28\x31\x31\x29\xa\x9"                                       \
         "\x6c\x77\x7a\x20\x38\x2c\x32\x34\x28\x31\x31\x29\xa\x9"                                       \
         "\x6c\x77\x7a\x20\x39\x2c\x32\x38\x28\x31\x31\x29\xa\x9"                                       \
         "\x6c\x77\x7a\x20\x31\x30\x2c\x33\x32\x28\x31\x31\x29\xa\x9" /* arg8->r10 */                      \
         "\x6c\x77\x7a\x20\x31\x31\x2c\x30\x28\x31\x31\x29\xa\x9"  /* target->r11 */                    \
         VALGRIND_BRANCH_AND_LINK_TO_NOREDIR_R11                  \
         "\x61\x64\x64\x69\x20\x31\x2c\x31\x2c\x31\x36\xa\x9"                                        \
         "\x6d\x72\x20\x25\x30\x2c\x33"                                                \
         : /*out*/   "\x3d\x72" (_res)                                  \
         : /*in*/    "\x72" (&_argvec[0])                            \
         : /*trash*/ "\x63\x63", "\x6d\x65\x6d\x6f\x72\x79", __CALLER_SAVED_REGS          \
      );                                                          \
      lval = (__typeof__(lval)) _res;                             \
   } while (0)

#define CALL_FN_W_11W(lval, orig, arg1,arg2,arg3,arg4,arg5,arg6,  \
                                  arg7,arg8,arg9,arg10,arg11)     \
   do {                                                           \
      volatile OrigFn        _orig = (orig);                      \
      volatile unsigned long _argvec[12];                         \
      volatile unsigned long _res;                                \
      _argvec[0] = (unsigned long)_orig.nraddr;                   \
      _argvec[1] = (unsigned long)arg1;                           \
      _argvec[2] = (unsigned long)arg2;                           \
      _argvec[3] = (unsigned long)arg3;                           \
      _argvec[4] = (unsigned long)arg4;                           \
      _argvec[5] = (unsigned long)arg5;                           \
      _argvec[6] = (unsigned long)arg6;                           \
      _argvec[7] = (unsigned long)arg7;                           \
      _argvec[8] = (unsigned long)arg8;                           \
      _argvec[9] = (unsigned long)arg9;                           \
      _argvec[10] = (unsigned long)arg10;                         \
      _argvec[11] = (unsigned long)arg11;                         \
      __asm__ volatile(                                           \
         "\x6d\x72\x20\x31\x31\x2c\x25\x31\xa\x9"                                           \
         "\x61\x64\x64\x69\x20\x31\x2c\x31\x2c\x2d\x33\x32\xa\x9"                                       \
         /* arg11 */                                              \
         "\x6c\x77\x7a\x20\x33\x2c\x34\x34\x28\x31\x31\x29\xa\x9"                                       \
         "\x73\x74\x77\x20\x33\x2c\x31\x36\x28\x31\x29\xa\x9"                                        \
         /* arg10 */                                              \
         "\x6c\x77\x7a\x20\x33\x2c\x34\x30\x28\x31\x31\x29\xa\x9"                                       \
         "\x73\x74\x77\x20\x33\x2c\x31\x32\x28\x31\x29\xa\x9"                                        \
         /* arg9 */                                               \
         "\x6c\x77\x7a\x20\x33\x2c\x33\x36\x28\x31\x31\x29\xa\x9"                                       \
         "\x73\x74\x77\x20\x33\x2c\x38\x28\x31\x29\xa\x9"                                         \
         /* args1-8 */                                            \
         "\x6c\x77\x7a\x20\x33\x2c\x34\x28\x31\x31\x29\xa\x9"   /* arg1->r3 */                       \
         "\x6c\x77\x7a\x20\x34\x2c\x38\x28\x31\x31\x29\xa\x9"                                        \
         "\x6c\x77\x7a\x20\x35\x2c\x31\x32\x28\x31\x31\x29\xa\x9"                                       \
         "\x6c\x77\x7a\x20\x36\x2c\x31\x36\x28\x31\x31\x29\xa\x9"  /* arg4->r6 */                       \
         "\x6c\x77\x7a\x20\x37\x2c\x32\x30\x28\x31\x31\x29\xa\x9"                                       \
         "\x6c\x77\x7a\x20\x38\x2c\x32\x34\x28\x31\x31\x29\xa\x9"                                       \
         "\x6c\x77\x7a\x20\x39\x2c\x32\x38\x28\x31\x31\x29\xa\x9"                                       \
         "\x6c\x77\x7a\x20\x31\x30\x2c\x33\x32\x28\x31\x31\x29\xa\x9" /* arg8->r10 */                      \
         "\x6c\x77\x7a\x20\x31\x31\x2c\x30\x28\x31\x31\x29\xa\x9"  /* target->r11 */                    \
         VALGRIND_BRANCH_AND_LINK_TO_NOREDIR_R11                  \
         "\x61\x64\x64\x69\x20\x31\x2c\x31\x2c\x33\x32\xa\x9"                                        \
         "\x6d\x72\x20\x25\x30\x2c\x33"                                                \
         : /*out*/   "\x3d\x72" (_res)                                  \
         : /*in*/    "\x72" (&_argvec[0])                            \
         : /*trash*/ "\x63\x63", "\x6d\x65\x6d\x6f\x72\x79", __CALLER_SAVED_REGS          \
      );                                                          \
      lval = (__typeof__(lval)) _res;                             \
   } while (0)

#define CALL_FN_W_12W(lval, orig, arg1,arg2,arg3,arg4,arg5,arg6,  \
                                arg7,arg8,arg9,arg10,arg11,arg12) \
   do {                                                           \
      volatile OrigFn        _orig = (orig);                      \
      volatile unsigned long _argvec[13];                         \
      volatile unsigned long _res;                                \
      _argvec[0] = (unsigned long)_orig.nraddr;                   \
      _argvec[1] = (unsigned long)arg1;                           \
      _argvec[2] = (unsigned long)arg2;                           \
      _argvec[3] = (unsigned long)arg3;                           \
      _argvec[4] = (unsigned long)arg4;                           \
      _argvec[5] = (unsigned long)arg5;                           \
      _argvec[6] = (unsigned long)arg6;                           \
      _argvec[7] = (unsigned long)arg7;                           \
      _argvec[8] = (unsigned long)arg8;                           \
      _argvec[9] = (unsigned long)arg9;                           \
      _argvec[10] = (unsigned long)arg10;                         \
      _argvec[11] = (unsigned long)arg11;                         \
      _argvec[12] = (unsigned long)arg12;                         \
      __asm__ volatile(                                           \
         "\x6d\x72\x20\x31\x31\x2c\x25\x31\xa\x9"                                           \
         "\x61\x64\x64\x69\x20\x31\x2c\x31\x2c\x2d\x33\x32\xa\x9"                                       \
         /* arg12 */                                              \
         "\x6c\x77\x7a\x20\x33\x2c\x34\x38\x28\x31\x31\x29\xa\x9"                                       \
         "\x73\x74\x77\x20\x33\x2c\x32\x30\x28\x31\x29\xa\x9"                                        \
         /* arg11 */                                              \
         "\x6c\x77\x7a\x20\x33\x2c\x34\x34\x28\x31\x31\x29\xa\x9"                                       \
         "\x73\x74\x77\x20\x33\x2c\x31\x36\x28\x31\x29\xa\x9"                                        \
         /* arg10 */                                              \
         "\x6c\x77\x7a\x20\x33\x2c\x34\x30\x28\x31\x31\x29\xa\x9"                                       \
         "\x73\x74\x77\x20\x33\x2c\x31\x32\x28\x31\x29\xa\x9"                                        \
         /* arg9 */                                               \
         "\x6c\x77\x7a\x20\x33\x2c\x33\x36\x28\x31\x31\x29\xa\x9"                                       \
         "\x73\x74\x77\x20\x33\x2c\x38\x28\x31\x29\xa\x9"                                         \
         /* args1-8 */                                            \
         "\x6c\x77\x7a\x20\x33\x2c\x34\x28\x31\x31\x29\xa\x9"   /* arg1->r3 */                       \
         "\x6c\x77\x7a\x20\x34\x2c\x38\x28\x31\x31\x29\xa\x9"                                        \
         "\x6c\x77\x7a\x20\x35\x2c\x31\x32\x28\x31\x31\x29\xa\x9"                                       \
         "\x6c\x77\x7a\x20\x36\x2c\x31\x36\x28\x31\x31\x29\xa\x9"  /* arg4->r6 */                       \
         "\x6c\x77\x7a\x20\x37\x2c\x32\x30\x28\x31\x31\x29\xa\x9"                                       \
         "\x6c\x77\x7a\x20\x38\x2c\x32\x34\x28\x31\x31\x29\xa\x9"                                       \
         "\x6c\x77\x7a\x20\x39\x2c\x32\x38\x28\x31\x31\x29\xa\x9"                                       \
         "\x6c\x77\x7a\x20\x31\x30\x2c\x33\x32\x28\x31\x31\x29\xa\x9" /* arg8->r10 */                      \
         "\x6c\x77\x7a\x20\x31\x31\x2c\x30\x28\x31\x31\x29\xa\x9"  /* target->r11 */                    \
         VALGRIND_BRANCH_AND_LINK_TO_NOREDIR_R11                  \
         "\x61\x64\x64\x69\x20\x31\x2c\x31\x2c\x33\x32\xa\x9"                                        \
         "\x6d\x72\x20\x25\x30\x2c\x33"                                                \
         : /*out*/   "\x3d\x72" (_res)                                  \
         : /*in*/    "\x72" (&_argvec[0])                            \
         : /*trash*/ "\x63\x63", "\x6d\x65\x6d\x6f\x72\x79", __CALLER_SAVED_REGS          \
      );                                                          \
      lval = (__typeof__(lval)) _res;                             \
   } while (0)

#endif /* PLAT_ppc32_linux */

/* ------------------------ ppc64-linux ------------------------ */

#if defined(PLAT_ppc64_linux)

/* ARGREGS: r3 r4 r5 r6 r7 r8 r9 r10 (the rest on stack somewhere) */

/* These regs are trashed by the hidden call. */
#define __CALLER_SAVED_REGS                                       \
   "\x6c\x72", "\x63\x74\x72", "\x78\x65\x72",                                            \
   "\x63\x72\x30", "\x63\x72\x31", "\x63\x72\x32", "\x63\x72\x33", "\x63\x72\x34", "\x63\x72\x35", "\x63\x72\x36", "\x63\x72\x37",        \
   "\x72\x30", "\x72\x32", "\x72\x33", "\x72\x34", "\x72\x35", "\x72\x36", "\x72\x37", "\x72\x38", "\x72\x39", "\x72\x31\x30",   \
   "\x72\x31\x31", "\x72\x31\x32", "\x72\x31\x33"

/* These CALL_FN_ macros assume that on ppc64-linux, sizeof(unsigned
   long) == 8. */

#define CALL_FN_W_v(lval, orig)                                   \
   do {                                                           \
      volatile OrigFn        _orig = (orig);                      \
      volatile unsigned long _argvec[3+0];                        \
      volatile unsigned long _res;                                \
      /* _argvec[0] holds current r2 across the call */           \
      _argvec[1] = (unsigned long)_orig.r2;                       \
      _argvec[2] = (unsigned long)_orig.nraddr;                   \
      __asm__ volatile(                                           \
         "\x6d\x72\x20\x31\x31\x2c\x25\x31\xa\x9"                                           \
         "\x73\x74\x64\x20\x32\x2c\x2d\x31\x36\x28\x31\x31\x29\xa\x9"  /* save tocptr */                   \
         "\x6c\x64\x20\x20\x20\x32\x2c\x2d\x38\x28\x31\x31\x29\xa\x9"  /* use nraddr's tocptr */           \
         "\x6c\x64\x20\x20\x31\x31\x2c\x20\x30\x28\x31\x31\x29\xa\x9"  /* target->r11 */                   \
         VALGRIND_BRANCH_AND_LINK_TO_NOREDIR_R11                  \
         "\x6d\x72\x20\x31\x31\x2c\x25\x31\xa\x9"                                           \
         "\x6d\x72\x20\x25\x30\x2c\x33\xa\x9"                                            \
         "\x6c\x64\x20\x32\x2c\x2d\x31\x36\x28\x31\x31\x29" /* restore tocptr */                      \
         : /*out*/   "\x3d\x72" (_res)                                  \
         : /*in*/    "\x72" (&_argvec[2])                            \
         : /*trash*/ "\x63\x63", "\x6d\x65\x6d\x6f\x72\x79", __CALLER_SAVED_REGS          \
      );                                                          \
      lval = (__typeof__(lval)) _res;                             \
   } while (0)

#define CALL_FN_W_W(lval, orig, arg1)                             \
   do {                                                           \
      volatile OrigFn        _orig = (orig);                      \
      volatile unsigned long _argvec[3+1];                        \
      volatile unsigned long _res;                                \
      /* _argvec[0] holds current r2 across the call */           \
      _argvec[1]   = (unsigned long)_orig.r2;                     \
      _argvec[2]   = (unsigned long)_orig.nraddr;                 \
      _argvec[2+1] = (unsigned long)arg1;                         \
      __asm__ volatile(                                           \
         "\x6d\x72\x20\x31\x31\x2c\x25\x31\xa\x9"                                           \
         "\x73\x74\x64\x20\x32\x2c\x2d\x31\x36\x28\x31\x31\x29\xa\x9"  /* save tocptr */                   \
         "\x6c\x64\x20\x20\x20\x32\x2c\x2d\x38\x28\x31\x31\x29\xa\x9"  /* use nraddr's tocptr */           \
         "\x6c\x64\x20\x20\x20\x33\x2c\x20\x38\x28\x31\x31\x29\xa\x9"  /* arg1->r3 */                      \
         "\x6c\x64\x20\x20\x31\x31\x2c\x20\x30\x28\x31\x31\x29\xa\x9"  /* target->r11 */                   \
         VALGRIND_BRANCH_AND_LINK_TO_NOREDIR_R11                  \
         "\x6d\x72\x20\x31\x31\x2c\x25\x31\xa\x9"                                           \
         "\x6d\x72\x20\x25\x30\x2c\x33\xa\x9"                                            \
         "\x6c\x64\x20\x32\x2c\x2d\x31\x36\x28\x31\x31\x29" /* restore tocptr */                      \
         : /*out*/   "\x3d\x72" (_res)                                  \
         : /*in*/    "\x72" (&_argvec[2])                            \
         : /*trash*/ "\x63\x63", "\x6d\x65\x6d\x6f\x72\x79", __CALLER_SAVED_REGS          \
      );                                                          \
      lval = (__typeof__(lval)) _res;                             \
   } while (0)

#define CALL_FN_W_WW(lval, orig, arg1,arg2)                       \
   do {                                                           \
      volatile OrigFn        _orig = (orig);                      \
      volatile unsigned long _argvec[3+2];                        \
      volatile unsigned long _res;                                \
      /* _argvec[0] holds current r2 across the call */           \
      _argvec[1]   = (unsigned long)_orig.r2;                     \
      _argvec[2]   = (unsigned long)_orig.nraddr;                 \
      _argvec[2+1] = (unsigned long)arg1;                         \
      _argvec[2+2] = (unsigned long)arg2;                         \
      __asm__ volatile(                                           \
         "\x6d\x72\x20\x31\x31\x2c\x25\x31\xa\x9"                                           \
         "\x73\x74\x64\x20\x32\x2c\x2d\x31\x36\x28\x31\x31\x29\xa\x9"  /* save tocptr */                   \
         "\x6c\x64\x20\x20\x20\x32\x2c\x2d\x38\x28\x31\x31\x29\xa\x9"  /* use nraddr's tocptr */           \
         "\x6c\x64\x20\x20\x20\x33\x2c\x20\x38\x28\x31\x31\x29\xa\x9"  /* arg1->r3 */                      \
         "\x6c\x64\x20\x20\x20\x34\x2c\x20\x31\x36\x28\x31\x31\x29\xa\x9" /* arg2->r4 */                      \
         "\x6c\x64\x20\x20\x31\x31\x2c\x20\x30\x28\x31\x31\x29\xa\x9"  /* target->r11 */                   \
         VALGRIND_BRANCH_AND_LINK_TO_NOREDIR_R11                  \
         "\x6d\x72\x20\x31\x31\x2c\x25\x31\xa\x9"                                           \
         "\x6d\x72\x20\x25\x30\x2c\x33\xa\x9"                                            \
         "\x6c\x64\x20\x32\x2c\x2d\x31\x36\x28\x31\x31\x29" /* restore tocptr */                      \
         : /*out*/   "\x3d\x72" (_res)                                  \
         : /*in*/    "\x72" (&_argvec[2])                            \
         : /*trash*/ "\x63\x63", "\x6d\x65\x6d\x6f\x72\x79", __CALLER_SAVED_REGS          \
      );                                                          \
      lval = (__typeof__(lval)) _res;                             \
   } while (0)

#define CALL_FN_W_WWW(lval, orig, arg1,arg2,arg3)                 \
   do {                                                           \
      volatile OrigFn        _orig = (orig);                      \
      volatile unsigned long _argvec[3+3];                        \
      volatile unsigned long _res;                                \
      /* _argvec[0] holds current r2 across the call */           \
      _argvec[1]   = (unsigned long)_orig.r2;                     \
      _argvec[2]   = (unsigned long)_orig.nraddr;                 \
      _argvec[2+1] = (unsigned long)arg1;                         \
      _argvec[2+2] = (unsigned long)arg2;                         \
      _argvec[2+3] = (unsigned long)arg3;                         \
      __asm__ volatile(                                           \
         "\x6d\x72\x20\x31\x31\x2c\x25\x31\xa\x9"                                           \
         "\x73\x74\x64\x20\x32\x2c\x2d\x31\x36\x28\x31\x31\x29\xa\x9"  /* save tocptr */                   \
         "\x6c\x64\x20\x20\x20\x32\x2c\x2d\x38\x28\x31\x31\x29\xa\x9"  /* use nraddr's tocptr */           \
         "\x6c\x64\x20\x20\x20\x33\x2c\x20\x38\x28\x31\x31\x29\xa\x9"  /* arg1->r3 */                      \
         "\x6c\x64\x20\x20\x20\x34\x2c\x20\x31\x36\x28\x31\x31\x29\xa\x9" /* arg2->r4 */                      \
         "\x6c\x64\x20\x20\x20\x35\x2c\x20\x32\x34\x28\x31\x31\x29\xa\x9" /* arg3->r5 */                      \
         "\x6c\x64\x20\x20\x31\x31\x2c\x20\x30\x28\x31\x31\x29\xa\x9"  /* target->r11 */                   \
         VALGRIND_BRANCH_AND_LINK_TO_NOREDIR_R11                  \
         "\x6d\x72\x20\x31\x31\x2c\x25\x31\xa\x9"                                           \
         "\x6d\x72\x20\x25\x30\x2c\x33\xa\x9"                                            \
         "\x6c\x64\x20\x32\x2c\x2d\x31\x36\x28\x31\x31\x29" /* restore tocptr */                      \
         : /*out*/   "\x3d\x72" (_res)                                  \
         : /*in*/    "\x72" (&_argvec[2])                            \
         : /*trash*/ "\x63\x63", "\x6d\x65\x6d\x6f\x72\x79", __CALLER_SAVED_REGS          \
      );                                                          \
      lval = (__typeof__(lval)) _res;                             \
   } while (0)

#define CALL_FN_W_WWWW(lval, orig, arg1,arg2,arg3,arg4)           \
   do {                                                           \
      volatile OrigFn        _orig = (orig);                      \
      volatile unsigned long _argvec[3+4];                        \
      volatile unsigned long _res;                                \
      /* _argvec[0] holds current r2 across the call */           \
      _argvec[1]   = (unsigned long)_orig.r2;                     \
      _argvec[2]   = (unsigned long)_orig.nraddr;                 \
      _argvec[2+1] = (unsigned long)arg1;                         \
      _argvec[2+2] = (unsigned long)arg2;                         \
      _argvec[2+3] = (unsigned long)arg3;                         \
      _argvec[2+4] = (unsigned long)arg4;                         \
      __asm__ volatile(                                           \
         "\x6d\x72\x20\x31\x31\x2c\x25\x31\xa\x9"                                           \
         "\x73\x74\x64\x20\x32\x2c\x2d\x31\x36\x28\x31\x31\x29\xa\x9"  /* save tocptr */                   \
         "\x6c\x64\x20\x20\x20\x32\x2c\x2d\x38\x28\x31\x31\x29\xa\x9"  /* use nraddr's tocptr */           \
         "\x6c\x64\x20\x20\x20\x33\x2c\x20\x38\x28\x31\x31\x29\xa\x9"  /* arg1->r3 */                      \
         "\x6c\x64\x20\x20\x20\x34\x2c\x20\x31\x36\x28\x31\x31\x29\xa\x9" /* arg2->r4 */                      \
         "\x6c\x64\x20\x20\x20\x35\x2c\x20\x32\x34\x28\x31\x31\x29\xa\x9" /* arg3->r5 */                      \
         "\x6c\x64\x20\x20\x20\x36\x2c\x20\x33\x32\x28\x31\x31\x29\xa\x9" /* arg4->r6 */                      \
         "\x6c\x64\x20\x20\x31\x31\x2c\x20\x30\x28\x31\x31\x29\xa\x9"  /* target->r11 */                   \
         VALGRIND_BRANCH_AND_LINK_TO_NOREDIR_R11                  \
         "\x6d\x72\x20\x31\x31\x2c\x25\x31\xa\x9"                                           \
         "\x6d\x72\x20\x25\x30\x2c\x33\xa\x9"                                            \
         "\x6c\x64\x20\x32\x2c\x2d\x31\x36\x28\x31\x31\x29" /* restore tocptr */                      \
         : /*out*/   "\x3d\x72" (_res)                                  \
         : /*in*/    "\x72" (&_argvec[2])                            \
         : /*trash*/ "\x63\x63", "\x6d\x65\x6d\x6f\x72\x79", __CALLER_SAVED_REGS          \
      );                                                          \
      lval = (__typeof__(lval)) _res;                             \
   } while (0)

#define CALL_FN_W_5W(lval, orig, arg1,arg2,arg3,arg4,arg5)        \
   do {                                                           \
      volatile OrigFn        _orig = (orig);                      \
      volatile unsigned long _argvec[3+5];                        \
      volatile unsigned long _res;                                \
      /* _argvec[0] holds current r2 across the call */           \
      _argvec[1]   = (unsigned long)_orig.r2;                     \
      _argvec[2]   = (unsigned long)_orig.nraddr;                 \
      _argvec[2+1] = (unsigned long)arg1;                         \
      _argvec[2+2] = (unsigned long)arg2;                         \
      _argvec[2+3] = (unsigned long)arg3;                         \
      _argvec[2+4] = (unsigned long)arg4;                         \
      _argvec[2+5] = (unsigned long)arg5;                         \
      __asm__ volatile(                                           \
         "\x6d\x72\x20\x31\x31\x2c\x25\x31\xa\x9"                                           \
         "\x73\x74\x64\x20\x32\x2c\x2d\x31\x36\x28\x31\x31\x29\xa\x9"  /* save tocptr */                   \
         "\x6c\x64\x20\x20\x20\x32\x2c\x2d\x38\x28\x31\x31\x29\xa\x9"  /* use nraddr's tocptr */           \
         "\x6c\x64\x20\x20\x20\x33\x2c\x20\x38\x28\x31\x31\x29\xa\x9"  /* arg1->r3 */                      \
         "\x6c\x64\x20\x20\x20\x34\x2c\x20\x31\x36\x28\x31\x31\x29\xa\x9" /* arg2->r4 */                      \
         "\x6c\x64\x20\x20\x20\x35\x2c\x20\x32\x34\x28\x31\x31\x29\xa\x9" /* arg3->r5 */                      \
         "\x6c\x64\x20\x20\x20\x36\x2c\x20\x33\x32\x28\x31\x31\x29\xa\x9" /* arg4->r6 */                      \
         "\x6c\x64\x20\x20\x20\x37\x2c\x20\x34\x30\x28\x31\x31\x29\xa\x9" /* arg5->r7 */                      \
         "\x6c\x64\x20\x20\x31\x31\x2c\x20\x30\x28\x31\x31\x29\xa\x9"  /* target->r11 */                   \
         VALGRIND_BRANCH_AND_LINK_TO_NOREDIR_R11                  \
         "\x6d\x72\x20\x31\x31\x2c\x25\x31\xa\x9"                                           \
         "\x6d\x72\x20\x25\x30\x2c\x33\xa\x9"                                            \
         "\x6c\x64\x20\x32\x2c\x2d\x31\x36\x28\x31\x31\x29" /* restore tocptr */                      \
         : /*out*/   "\x3d\x72" (_res)                                  \
         : /*in*/    "\x72" (&_argvec[2])                            \
         : /*trash*/ "\x63\x63", "\x6d\x65\x6d\x6f\x72\x79", __CALLER_SAVED_REGS          \
      );                                                          \
      lval = (__typeof__(lval)) _res;                             \
   } while (0)

#define CALL_FN_W_6W(lval, orig, arg1,arg2,arg3,arg4,arg5,arg6)   \
   do {                                                           \
      volatile OrigFn        _orig = (orig);                      \
      volatile unsigned long _argvec[3+6];                        \
      volatile unsigned long _res;                                \
      /* _argvec[0] holds current r2 across the call */           \
      _argvec[1]   = (unsigned long)_orig.r2;                     \
      _argvec[2]   = (unsigned long)_orig.nraddr;                 \
      _argvec[2+1] = (unsigned long)arg1;                         \
      _argvec[2+2] = (unsigned long)arg2;                         \
      _argvec[2+3] = (unsigned long)arg3;                         \
      _argvec[2+4] = (unsigned long)arg4;                         \
      _argvec[2+5] = (unsigned long)arg5;                         \
      _argvec[2+6] = (unsigned long)arg6;                         \
      __asm__ volatile(                                           \
         "\x6d\x72\x20\x31\x31\x2c\x25\x31\xa\x9"                                           \
         "\x73\x74\x64\x20\x32\x2c\x2d\x31\x36\x28\x31\x31\x29\xa\x9"  /* save tocptr */                   \
         "\x6c\x64\x20\x20\x20\x32\x2c\x2d\x38\x28\x31\x31\x29\xa\x9"  /* use nraddr's tocptr */           \
         "\x6c\x64\x20\x20\x20\x33\x2c\x20\x38\x28\x31\x31\x29\xa\x9"  /* arg1->r3 */                      \
         "\x6c\x64\x20\x20\x20\x34\x2c\x20\x31\x36\x28\x31\x31\x29\xa\x9" /* arg2->r4 */                      \
         "\x6c\x64\x20\x20\x20\x35\x2c\x20\x32\x34\x28\x31\x31\x29\xa\x9" /* arg3->r5 */                      \
         "\x6c\x64\x20\x20\x20\x36\x2c\x20\x33\x32\x28\x31\x31\x29\xa\x9" /* arg4->r6 */                      \
         "\x6c\x64\x20\x20\x20\x37\x2c\x20\x34\x30\x28\x31\x31\x29\xa\x9" /* arg5->r7 */                      \
         "\x6c\x64\x20\x20\x20\x38\x2c\x20\x34\x38\x28\x31\x31\x29\xa\x9" /* arg6->r8 */                      \
         "\x6c\x64\x20\x20\x31\x31\x2c\x20\x30\x28\x31\x31\x29\xa\x9"  /* target->r11 */                   \
         VALGRIND_BRANCH_AND_LINK_TO_NOREDIR_R11                  \
         "\x6d\x72\x20\x31\x31\x2c\x25\x31\xa\x9"                                           \
         "\x6d\x72\x20\x25\x30\x2c\x33\xa\x9"                                            \
         "\x6c\x64\x20\x32\x2c\x2d\x31\x36\x28\x31\x31\x29" /* restore tocptr */                      \
         : /*out*/   "\x3d\x72" (_res)                                  \
         : /*in*/    "\x72" (&_argvec[2])                            \
         : /*trash*/ "\x63\x63", "\x6d\x65\x6d\x6f\x72\x79", __CALLER_SAVED_REGS          \
      );                                                          \
      lval = (__typeof__(lval)) _res;                             \
   } while (0)

#define CALL_FN_W_7W(lval, orig, arg1,arg2,arg3,arg4,arg5,arg6,   \
                                 arg7)                            \
   do {                                                           \
      volatile OrigFn        _orig = (orig);                      \
      volatile unsigned long _argvec[3+7];                        \
      volatile unsigned long _res;                                \
      /* _argvec[0] holds current r2 across the call */           \
      _argvec[1]   = (unsigned long)_orig.r2;                     \
      _argvec[2]   = (unsigned long)_orig.nraddr;                 \
      _argvec[2+1] = (unsigned long)arg1;                         \
      _argvec[2+2] = (unsigned long)arg2;                         \
      _argvec[2+3] = (unsigned long)arg3;                         \
      _argvec[2+4] = (unsigned long)arg4;                         \
      _argvec[2+5] = (unsigned long)arg5;                         \
      _argvec[2+6] = (unsigned long)arg6;                         \
      _argvec[2+7] = (unsigned long)arg7;                         \
      __asm__ volatile(                                           \
         "\x6d\x72\x20\x31\x31\x2c\x25\x31\xa\x9"                                           \
         "\x73\x74\x64\x20\x32\x2c\x2d\x31\x36\x28\x31\x31\x29\xa\x9"  /* save tocptr */                   \
         "\x6c\x64\x20\x20\x20\x32\x2c\x2d\x38\x28\x31\x31\x29\xa\x9"  /* use nraddr's tocptr */           \
         "\x6c\x64\x20\x20\x20\x33\x2c\x20\x38\x28\x31\x31\x29\xa\x9"  /* arg1->r3 */                      \
         "\x6c\x64\x20\x20\x20\x34\x2c\x20\x31\x36\x28\x31\x31\x29\xa\x9" /* arg2->r4 */                      \
         "\x6c\x64\x20\x20\x20\x35\x2c\x20\x32\x34\x28\x31\x31\x29\xa\x9" /* arg3->r5 */                      \
         "\x6c\x64\x20\x20\x20\x36\x2c\x20\x33\x32\x28\x31\x31\x29\xa\x9" /* arg4->r6 */                      \
         "\x6c\x64\x20\x20\x20\x37\x2c\x20\x34\x30\x28\x31\x31\x29\xa\x9" /* arg5->r7 */                      \
         "\x6c\x64\x20\x20\x20\x38\x2c\x20\x34\x38\x28\x31\x31\x29\xa\x9" /* arg6->r8 */                      \
         "\x6c\x64\x20\x20\x20\x39\x2c\x20\x35\x36\x28\x31\x31\x29\xa\x9" /* arg7->r9 */                      \
         "\x6c\x64\x20\x20\x31\x31\x2c\x20\x30\x28\x31\x31\x29\xa\x9"  /* target->r11 */                   \
         VALGRIND_BRANCH_AND_LINK_TO_NOREDIR_R11                  \
         "\x6d\x72\x20\x31\x31\x2c\x25\x31\xa\x9"                                           \
         "\x6d\x72\x20\x25\x30\x2c\x33\xa\x9"                                            \
         "\x6c\x64\x20\x32\x2c\x2d\x31\x36\x28\x31\x31\x29" /* restore tocptr */                      \
         : /*out*/   "\x3d\x72" (_res)                                  \
         : /*in*/    "\x72" (&_argvec[2])                            \
         : /*trash*/ "\x63\x63", "\x6d\x65\x6d\x6f\x72\x79", __CALLER_SAVED_REGS          \
      );                                                          \
      lval = (__typeof__(lval)) _res;                             \
   } while (0)

#define CALL_FN_W_8W(lval, orig, arg1,arg2,arg3,arg4,arg5,arg6,   \
                                 arg7,arg8)                       \
   do {                                                           \
      volatile OrigFn        _orig = (orig);                      \
      volatile unsigned long _argvec[3+8];                        \
      volatile unsigned long _res;                                \
      /* _argvec[0] holds current r2 across the call */           \
      _argvec[1]   = (unsigned long)_orig.r2;                     \
      _argvec[2]   = (unsigned long)_orig.nraddr;                 \
      _argvec[2+1] = (unsigned long)arg1;                         \
      _argvec[2+2] = (unsigned long)arg2;                         \
      _argvec[2+3] = (unsigned long)arg3;                         \
      _argvec[2+4] = (unsigned long)arg4;                         \
      _argvec[2+5] = (unsigned long)arg5;                         \
      _argvec[2+6] = (unsigned long)arg6;                         \
      _argvec[2+7] = (unsigned long)arg7;                         \
      _argvec[2+8] = (unsigned long)arg8;                         \
      __asm__ volatile(                                           \
         "\x6d\x72\x20\x31\x31\x2c\x25\x31\xa\x9"                                           \
         "\x73\x74\x64\x20\x32\x2c\x2d\x31\x36\x28\x31\x31\x29\xa\x9"  /* save tocptr */                   \
         "\x6c\x64\x20\x20\x20\x32\x2c\x2d\x38\x28\x31\x31\x29\xa\x9"  /* use nraddr's tocptr */           \
         "\x6c\x64\x20\x20\x20\x33\x2c\x20\x38\x28\x31\x31\x29\xa\x9"  /* arg1->r3 */                      \
         "\x6c\x64\x20\x20\x20\x34\x2c\x20\x31\x36\x28\x31\x31\x29\xa\x9" /* arg2->r4 */                      \
         "\x6c\x64\x20\x20\x20\x35\x2c\x20\x32\x34\x28\x31\x31\x29\xa\x9" /* arg3->r5 */                      \
         "\x6c\x64\x20\x20\x20\x36\x2c\x20\x33\x32\x28\x31\x31\x29\xa\x9" /* arg4->r6 */                      \
         "\x6c\x64\x20\x20\x20\x37\x2c\x20\x34\x30\x28\x31\x31\x29\xa\x9" /* arg5->r7 */                      \
         "\x6c\x64\x20\x20\x20\x38\x2c\x20\x34\x38\x28\x31\x31\x29\xa\x9" /* arg6->r8 */                      \
         "\x6c\x64\x20\x20\x20\x39\x2c\x20\x35\x36\x28\x31\x31\x29\xa\x9" /* arg7->r9 */                      \
         "\x6c\x64\x20\x20\x31\x30\x2c\x20\x36\x34\x28\x31\x31\x29\xa\x9" /* arg8->r10 */                     \
         "\x6c\x64\x20\x20\x31\x31\x2c\x20\x30\x28\x31\x31\x29\xa\x9"  /* target->r11 */                   \
         VALGRIND_BRANCH_AND_LINK_TO_NOREDIR_R11                  \
         "\x6d\x72\x20\x31\x31\x2c\x25\x31\xa\x9"                                           \
         "\x6d\x72\x20\x25\x30\x2c\x33\xa\x9"                                            \
         "\x6c\x64\x20\x32\x2c\x2d\x31\x36\x28\x31\x31\x29" /* restore tocptr */                      \
         : /*out*/   "\x3d\x72" (_res)                                  \
         : /*in*/    "\x72" (&_argvec[2])                            \
         : /*trash*/ "\x63\x63", "\x6d\x65\x6d\x6f\x72\x79", __CALLER_SAVED_REGS          \
      );                                                          \
      lval = (__typeof__(lval)) _res;                             \
   } while (0)

#define CALL_FN_W_9W(lval, orig, arg1,arg2,arg3,arg4,arg5,arg6,   \
                                 arg7,arg8,arg9)                  \
   do {                                                           \
      volatile OrigFn        _orig = (orig);                      \
      volatile unsigned long _argvec[3+9];                        \
      volatile unsigned long _res;                                \
      /* _argvec[0] holds current r2 across the call */           \
      _argvec[1]   = (unsigned long)_orig.r2;                     \
      _argvec[2]   = (unsigned long)_orig.nraddr;                 \
      _argvec[2+1] = (unsigned long)arg1;                         \
      _argvec[2+2] = (unsigned long)arg2;                         \
      _argvec[2+3] = (unsigned long)arg3;                         \
      _argvec[2+4] = (unsigned long)arg4;                         \
      _argvec[2+5] = (unsigned long)arg5;                         \
      _argvec[2+6] = (unsigned long)arg6;                         \
      _argvec[2+7] = (unsigned long)arg7;                         \
      _argvec[2+8] = (unsigned long)arg8;                         \
      _argvec[2+9] = (unsigned long)arg9;                         \
      __asm__ volatile(                                           \
         "\x6d\x72\x20\x31\x31\x2c\x25\x31\xa\x9"                                           \
         "\x73\x74\x64\x20\x32\x2c\x2d\x31\x36\x28\x31\x31\x29\xa\x9"  /* save tocptr */                   \
         "\x6c\x64\x20\x20\x20\x32\x2c\x2d\x38\x28\x31\x31\x29\xa\x9"  /* use nraddr's tocptr */           \
         "\x61\x64\x64\x69\x20\x31\x2c\x31\x2c\x2d\x31\x32\x38\xa\x9"  /* expand stack frame */            \
         /* arg9 */                                               \
         "\x6c\x64\x20\x20\x33\x2c\x37\x32\x28\x31\x31\x29\xa\x9"                                       \
         "\x73\x74\x64\x20\x33\x2c\x31\x31\x32\x28\x31\x29\xa\x9"                                       \
         /* args1-8 */                                            \
         "\x6c\x64\x20\x20\x20\x33\x2c\x20\x38\x28\x31\x31\x29\xa\x9"  /* arg1->r3 */                      \
         "\x6c\x64\x20\x20\x20\x34\x2c\x20\x31\x36\x28\x31\x31\x29\xa\x9" /* arg2->r4 */                      \
         "\x6c\x64\x20\x20\x20\x35\x2c\x20\x32\x34\x28\x31\x31\x29\xa\x9" /* arg3->r5 */                      \
         "\x6c\x64\x20\x20\x20\x36\x2c\x20\x33\x32\x28\x31\x31\x29\xa\x9" /* arg4->r6 */                      \
         "\x6c\x64\x20\x20\x20\x37\x2c\x20\x34\x30\x28\x31\x31\x29\xa\x9" /* arg5->r7 */                      \
         "\x6c\x64\x20\x20\x20\x38\x2c\x20\x34\x38\x28\x31\x31\x29\xa\x9" /* arg6->r8 */                      \
         "\x6c\x64\x20\x20\x20\x39\x2c\x20\x35\x36\x28\x31\x31\x29\xa\x9" /* arg7->r9 */                      \
         "\x6c\x64\x20\x20\x31\x30\x2c\x20\x36\x34\x28\x31\x31\x29\xa\x9" /* arg8->r10 */                     \
         "\x6c\x64\x20\x20\x31\x31\x2c\x20\x30\x28\x31\x31\x29\xa\x9"  /* target->r11 */                   \
         VALGRIND_BRANCH_AND_LINK_TO_NOREDIR_R11                  \
         "\x6d\x72\x20\x31\x31\x2c\x25\x31\xa\x9"                                           \
         "\x6d\x72\x20\x25\x30\x2c\x33\xa\x9"                                            \
         "\x6c\x64\x20\x32\x2c\x2d\x31\x36\x28\x31\x31\x29\xa\x9" /* restore tocptr */                  \
         "\x61\x64\x64\x69\x20\x31\x2c\x31\x2c\x31\x32\x38"     /* restore frame */                   \
         : /*out*/   "\x3d\x72" (_res)                                  \
         : /*in*/    "\x72" (&_argvec[2])                            \
         : /*trash*/ "\x63\x63", "\x6d\x65\x6d\x6f\x72\x79", __CALLER_SAVED_REGS          \
      );                                                          \
      lval = (__typeof__(lval)) _res;                             \
   } while (0)

#define CALL_FN_W_10W(lval, orig, arg1,arg2,arg3,arg4,arg5,arg6,  \
                                  arg7,arg8,arg9,arg10)           \
   do {                                                           \
      volatile OrigFn        _orig = (orig);                      \
      volatile unsigned long _argvec[3+10];                       \
      volatile unsigned long _res;                                \
      /* _argvec[0] holds current r2 across the call */           \
      _argvec[1]   = (unsigned long)_orig.r2;                     \
      _argvec[2]   = (unsigned long)_orig.nraddr;                 \
      _argvec[2+1] = (unsigned long)arg1;                         \
      _argvec[2+2] = (unsigned long)arg2;                         \
      _argvec[2+3] = (unsigned long)arg3;                         \
      _argvec[2+4] = (unsigned long)arg4;                         \
      _argvec[2+5] = (unsigned long)arg5;                         \
      _argvec[2+6] = (unsigned long)arg6;                         \
      _argvec[2+7] = (unsigned long)arg7;                         \
      _argvec[2+8] = (unsigned long)arg8;                         \
      _argvec[2+9] = (unsigned long)arg9;                         \
      _argvec[2+10] = (unsigned long)arg10;                       \
      __asm__ volatile(                                           \
         "\x6d\x72\x20\x31\x31\x2c\x25\x31\xa\x9"                                           \
         "\x73\x74\x64\x20\x32\x2c\x2d\x31\x36\x28\x31\x31\x29\xa\x9"  /* save tocptr */                   \
         "\x6c\x64\x20\x20\x20\x32\x2c\x2d\x38\x28\x31\x31\x29\xa\x9"  /* use nraddr's tocptr */           \
         "\x61\x64\x64\x69\x20\x31\x2c\x31\x2c\x2d\x31\x32\x38\xa\x9"  /* expand stack frame */            \
         /* arg10 */                                              \
         "\x6c\x64\x20\x20\x33\x2c\x38\x30\x28\x31\x31\x29\xa\x9"                                       \
         "\x73\x74\x64\x20\x33\x2c\x31\x32\x30\x28\x31\x29\xa\x9"                                       \
         /* arg9 */                                               \
         "\x6c\x64\x20\x20\x33\x2c\x37\x32\x28\x31\x31\x29\xa\x9"                                       \
         "\x73\x74\x64\x20\x33\x2c\x31\x31\x32\x28\x31\x29\xa\x9"                                       \
         /* args1-8 */                                            \
         "\x6c\x64\x20\x20\x20\x33\x2c\x20\x38\x28\x31\x31\x29\xa\x9"  /* arg1->r3 */                      \
         "\x6c\x64\x20\x20\x20\x34\x2c\x20\x31\x36\x28\x31\x31\x29\xa\x9" /* arg2->r4 */                      \
         "\x6c\x64\x20\x20\x20\x35\x2c\x20\x32\x34\x28\x31\x31\x29\xa\x9" /* arg3->r5 */                      \
         "\x6c\x64\x20\x20\x20\x36\x2c\x20\x33\x32\x28\x31\x31\x29\xa\x9" /* arg4->r6 */                      \
         "\x6c\x64\x20\x20\x20\x37\x2c\x20\x34\x30\x28\x31\x31\x29\xa\x9" /* arg5->r7 */                      \
         "\x6c\x64\x20\x20\x20\x38\x2c\x20\x34\x38\x28\x31\x31\x29\xa\x9" /* arg6->r8 */                      \
         "\x6c\x64\x20\x20\x20\x39\x2c\x20\x35\x36\x28\x31\x31\x29\xa\x9" /* arg7->r9 */                      \
         "\x6c\x64\x20\x20\x31\x30\x2c\x20\x36\x34\x28\x31\x31\x29\xa\x9" /* arg8->r10 */                     \
         "\x6c\x64\x20\x20\x31\x31\x2c\x20\x30\x28\x31\x31\x29\xa\x9"  /* target->r11 */                   \
         VALGRIND_BRANCH_AND_LINK_TO_NOREDIR_R11                  \
         "\x6d\x72\x20\x31\x31\x2c\x25\x31\xa\x9"                                           \
         "\x6d\x72\x20\x25\x30\x2c\x33\xa\x9"                                            \
         "\x6c\x64\x20\x32\x2c\x2d\x31\x36\x28\x31\x31\x29\xa\x9" /* restore tocptr */                  \
         "\x61\x64\x64\x69\x20\x31\x2c\x31\x2c\x31\x32\x38"     /* restore frame */                   \
         : /*out*/   "\x3d\x72" (_res)                                  \
         : /*in*/    "\x72" (&_argvec[2])                            \
         : /*trash*/ "\x63\x63", "\x6d\x65\x6d\x6f\x72\x79", __CALLER_SAVED_REGS          \
      );                                                          \
      lval = (__typeof__(lval)) _res;                             \
   } while (0)

#define CALL_FN_W_11W(lval, orig, arg1,arg2,arg3,arg4,arg5,arg6,  \
                                  arg7,arg8,arg9,arg10,arg11)     \
   do {                                                           \
      volatile OrigFn        _orig = (orig);                      \
      volatile unsigned long _argvec[3+11];                       \
      volatile unsigned long _res;                                \
      /* _argvec[0] holds current r2 across the call */           \
      _argvec[1]   = (unsigned long)_orig.r2;                     \
      _argvec[2]   = (unsigned long)_orig.nraddr;                 \
      _argvec[2+1] = (unsigned long)arg1;                         \
      _argvec[2+2] = (unsigned long)arg2;                         \
      _argvec[2+3] = (unsigned long)arg3;                         \
      _argvec[2+4] = (unsigned long)arg4;                         \
      _argvec[2+5] = (unsigned long)arg5;                         \
      _argvec[2+6] = (unsigned long)arg6;                         \
      _argvec[2+7] = (unsigned long)arg7;                         \
      _argvec[2+8] = (unsigned long)arg8;                         \
      _argvec[2+9] = (unsigned long)arg9;                         \
      _argvec[2+10] = (unsigned long)arg10;                       \
      _argvec[2+11] = (unsigned long)arg11;                       \
      __asm__ volatile(                                           \
         "\x6d\x72\x20\x31\x31\x2c\x25\x31\xa\x9"                                           \
         "\x73\x74\x64\x20\x32\x2c\x2d\x31\x36\x28\x31\x31\x29\xa\x9"  /* save tocptr */                   \
         "\x6c\x64\x20\x20\x20\x32\x2c\x2d\x38\x28\x31\x31\x29\xa\x9"  /* use nraddr's tocptr */           \
         "\x61\x64\x64\x69\x20\x31\x2c\x31\x2c\x2d\x31\x34\x34\xa\x9"  /* expand stack frame */            \
         /* arg11 */                                              \
         "\x6c\x64\x20\x20\x33\x2c\x38\x38\x28\x31\x31\x29\xa\x9"                                       \
         "\x73\x74\x64\x20\x33\x2c\x31\x32\x38\x28\x31\x29\xa\x9"                                       \
         /* arg10 */                                              \
         "\x6c\x64\x20\x20\x33\x2c\x38\x30\x28\x31\x31\x29\xa\x9"                                       \
         "\x73\x74\x64\x20\x33\x2c\x31\x32\x30\x28\x31\x29\xa\x9"                                       \
         /* arg9 */                                               \
         "\x6c\x64\x20\x20\x33\x2c\x37\x32\x28\x31\x31\x29\xa\x9"                                       \
         "\x73\x74\x64\x20\x33\x2c\x31\x31\x32\x28\x31\x29\xa\x9"                                       \
         /* args1-8 */                                            \
         "\x6c\x64\x20\x20\x20\x33\x2c\x20\x38\x28\x31\x31\x29\xa\x9"  /* arg1->r3 */                      \
         "\x6c\x64\x20\x20\x20\x34\x2c\x20\x31\x36\x28\x31\x31\x29\xa\x9" /* arg2->r4 */                      \
         "\x6c\x64\x20\x20\x20\x35\x2c\x20\x32\x34\x28\x31\x31\x29\xa\x9" /* arg3->r5 */                      \
         "\x6c\x64\x20\x20\x20\x36\x2c\x20\x33\x32\x28\x31\x31\x29\xa\x9" /* arg4->r6 */                      \
         "\x6c\x64\x20\x20\x20\x37\x2c\x20\x34\x30\x28\x31\x31\x29\xa\x9" /* arg5->r7 */                      \
         "\x6c\x64\x20\x20\x20\x38\x2c\x20\x34\x38\x28\x31\x31\x29\xa\x9" /* arg6->r8 */                      \
         "\x6c\x64\x20\x20\x20\x39\x2c\x20\x35\x36\x28\x31\x31\x29\xa\x9" /* arg7->r9 */                      \
         "\x6c\x64\x20\x20\x31\x30\x2c\x20\x36\x34\x28\x31\x31\x29\xa\x9" /* arg8->r10 */                     \
         "\x6c\x64\x20\x20\x31\x31\x2c\x20\x30\x28\x31\x31\x29\xa\x9"  /* target->r11 */                   \
         VALGRIND_BRANCH_AND_LINK_TO_NOREDIR_R11                  \
         "\x6d\x72\x20\x31\x31\x2c\x25\x31\xa\x9"                                           \
         "\x6d\x72\x20\x25\x30\x2c\x33\xa\x9"                                            \
         "\x6c\x64\x20\x32\x2c\x2d\x31\x36\x28\x31\x31\x29\xa\x9" /* restore tocptr */                  \
         "\x61\x64\x64\x69\x20\x31\x2c\x31\x2c\x31\x34\x34"     /* restore frame */                   \
         : /*out*/   "\x3d\x72" (_res)                                  \
         : /*in*/    "\x72" (&_argvec[2])                            \
         : /*trash*/ "\x63\x63", "\x6d\x65\x6d\x6f\x72\x79", __CALLER_SAVED_REGS          \
      );                                                          \
      lval = (__typeof__(lval)) _res;                             \
   } while (0)

#define CALL_FN_W_12W(lval, orig, arg1,arg2,arg3,arg4,arg5,arg6,  \
                                arg7,arg8,arg9,arg10,arg11,arg12) \
   do {                                                           \
      volatile OrigFn        _orig = (orig);                      \
      volatile unsigned long _argvec[3+12];                       \
      volatile unsigned long _res;                                \
      /* _argvec[0] holds current r2 across the call */           \
      _argvec[1]   = (unsigned long)_orig.r2;                     \
      _argvec[2]   = (unsigned long)_orig.nraddr;                 \
      _argvec[2+1] = (unsigned long)arg1;                         \
      _argvec[2+2] = (unsigned long)arg2;                         \
      _argvec[2+3] = (unsigned long)arg3;                         \
      _argvec[2+4] = (unsigned long)arg4;                         \
      _argvec[2+5] = (unsigned long)arg5;                         \
      _argvec[2+6] = (unsigned long)arg6;                         \
      _argvec[2+7] = (unsigned long)arg7;                         \
      _argvec[2+8] = (unsigned long)arg8;                         \
      _argvec[2+9] = (unsigned long)arg9;                         \
      _argvec[2+10] = (unsigned long)arg10;                       \
      _argvec[2+11] = (unsigned long)arg11;                       \
      _argvec[2+12] = (unsigned long)arg12;                       \
      __asm__ volatile(                                           \
         "\x6d\x72\x20\x31\x31\x2c\x25\x31\xa\x9"                                           \
         "\x73\x74\x64\x20\x32\x2c\x2d\x31\x36\x28\x31\x31\x29\xa\x9"  /* save tocptr */                   \
         "\x6c\x64\x20\x20\x20\x32\x2c\x2d\x38\x28\x31\x31\x29\xa\x9"  /* use nraddr's tocptr */           \
         "\x61\x64\x64\x69\x20\x31\x2c\x31\x2c\x2d\x31\x34\x34\xa\x9"  /* expand stack frame */            \
         /* arg12 */                                              \
         "\x6c\x64\x20\x20\x33\x2c\x39\x36\x28\x31\x31\x29\xa\x9"                                       \
         "\x73\x74\x64\x20\x33\x2c\x31\x33\x36\x28\x31\x29\xa\x9"                                       \
         /* arg11 */                                              \
         "\x6c\x64\x20\x20\x33\x2c\x38\x38\x28\x31\x31\x29\xa\x9"                                       \
         "\x73\x74\x64\x20\x33\x2c\x31\x32\x38\x28\x31\x29\xa\x9"                                       \
         /* arg10 */                                              \
         "\x6c\x64\x20\x20\x33\x2c\x38\x30\x28\x31\x31\x29\xa\x9"                                       \
         "\x73\x74\x64\x20\x33\x2c\x31\x32\x30\x28\x31\x29\xa\x9"                                       \
         /* arg9 */                                               \
         "\x6c\x64\x20\x20\x33\x2c\x37\x32\x28\x31\x31\x29\xa\x9"                                       \
         "\x73\x74\x64\x20\x33\x2c\x31\x31\x32\x28\x31\x29\xa\x9"                                       \
         /* args1-8 */                                            \
         "\x6c\x64\x20\x20\x20\x33\x2c\x20\x38\x28\x31\x31\x29\xa\x9"  /* arg1->r3 */                      \
         "\x6c\x64\x20\x20\x20\x34\x2c\x20\x31\x36\x28\x31\x31\x29\xa\x9" /* arg2->r4 */                      \
         "\x6c\x64\x20\x20\x20\x35\x2c\x20\x32\x34\x28\x31\x31\x29\xa\x9" /* arg3->r5 */                      \
         "\x6c\x64\x20\x20\x20\x36\x2c\x20\x33\x32\x28\x31\x31\x29\xa\x9" /* arg4->r6 */                      \
         "\x6c\x64\x20\x20\x20\x37\x2c\x20\x34\x30\x28\x31\x31\x29\xa\x9" /* arg5->r7 */                      \
         "\x6c\x64\x20\x20\x20\x38\x2c\x20\x34\x38\x28\x31\x31\x29\xa\x9" /* arg6->r8 */                      \
         "\x6c\x64\x20\x20\x20\x39\x2c\x20\x35\x36\x28\x31\x31\x29\xa\x9" /* arg7->r9 */                      \
         "\x6c\x64\x20\x20\x31\x30\x2c\x20\x36\x34\x28\x31\x31\x29\xa\x9" /* arg8->r10 */                     \
         "\x6c\x64\x20\x20\x31\x31\x2c\x20\x30\x28\x31\x31\x29\xa\x9"  /* target->r11 */                   \
         VALGRIND_BRANCH_AND_LINK_TO_NOREDIR_R11                  \
         "\x6d\x72\x20\x31\x31\x2c\x25\x31\xa\x9"                                           \
         "\x6d\x72\x20\x25\x30\x2c\x33\xa\x9"                                            \
         "\x6c\x64\x20\x32\x2c\x2d\x31\x36\x28\x31\x31\x29\xa\x9" /* restore tocptr */                  \
         "\x61\x64\x64\x69\x20\x31\x2c\x31\x2c\x31\x34\x34"     /* restore frame */                   \
         : /*out*/   "\x3d\x72" (_res)                                  \
         : /*in*/    "\x72" (&_argvec[2])                            \
         : /*trash*/ "\x63\x63", "\x6d\x65\x6d\x6f\x72\x79", __CALLER_SAVED_REGS          \
      );                                                          \
      lval = (__typeof__(lval)) _res;                             \
   } while (0)

#endif /* PLAT_ppc64_linux */

/* ------------------------- arm-linux ------------------------- */

#if defined(PLAT_arm_linux)

/* These regs are trashed by the hidden call. */
#define __CALLER_SAVED_REGS "\x72\x30", "\x72\x31", "\x72\x32", "\x72\x33","\x72\x34","\x72\x31\x34"

/* These CALL_FN_ macros assume that on arm-linux, sizeof(unsigned
   long) == 4. */

#define CALL_FN_W_v(lval, orig)                                   \
   do {                                                           \
      volatile OrigFn        _orig = (orig);                      \
      volatile unsigned long _argvec[1];                          \
      volatile unsigned long _res;                                \
      _argvec[0] = (unsigned long)_orig.nraddr;                   \
      __asm__ volatile(                                           \
         "\x6c\x64\x72\x20\x72\x34\x2c\x20\x5b\x25\x31\x5d\x20\xa\x9"  /* target->r4 */                    \
         VALGRIND_BRANCH_AND_LINK_TO_NOREDIR_R4                   \
         "\x6d\x6f\x76\x20\x25\x30\x2c\x20\x72\x30\xa"                                           \
         : /*out*/   "\x3d\x72" (_res)                                  \
         : /*in*/    "\x30" (&_argvec[0])                            \
         : /*trash*/ "\x63\x63", "\x6d\x65\x6d\x6f\x72\x79", __CALLER_SAVED_REGS          \
      );                                                          \
      lval = (__typeof__(lval)) _res;                             \
   } while (0)

#define CALL_FN_W_W(lval, orig, arg1)                             \
   do {                                                           \
      volatile OrigFn        _orig = (orig);                      \
      volatile unsigned long _argvec[2];                          \
      volatile unsigned long _res;                                \
      _argvec[0] = (unsigned long)_orig.nraddr;                   \
      _argvec[1] = (unsigned long)(arg1);                         \
      __asm__ volatile(                                           \
         "\x6c\x64\x72\x20\x72\x30\x2c\x20\x5b\x25\x31\x2c\x20\x23\x34\x5d\x20\xa\x9"                                  \
         "\x6c\x64\x72\x20\x72\x34\x2c\x20\x5b\x25\x31\x5d\x20\xa\x9"  /* target->r4 */                    \
         VALGRIND_BRANCH_AND_LINK_TO_NOREDIR_R4                   \
         "\x6d\x6f\x76\x20\x25\x30\x2c\x20\x72\x30\xa"                                           \
         : /*out*/   "\x3d\x72" (_res)                                  \
         : /*in*/    "\x30" (&_argvec[0])                            \
         : /*trash*/ "\x63\x63", "\x6d\x65\x6d\x6f\x72\x79",  __CALLER_SAVED_REGS         \
      );                                                          \
      lval = (__typeof__(lval)) _res;                             \
   } while (0)

#define CALL_FN_W_WW(lval, orig, arg1,arg2)                       \
   do {                                                           \
      volatile OrigFn        _orig = (orig);                      \
      volatile unsigned long _argvec[3];                          \
      volatile unsigned long _res;                                \
      _argvec[0] = (unsigned long)_orig.nraddr;                   \
      _argvec[1] = (unsigned long)(arg1);                         \
      _argvec[2] = (unsigned long)(arg2);                         \
      __asm__ volatile(                                           \
         "\x6c\x64\x72\x20\x72\x30\x2c\x20\x5b\x25\x31\x2c\x20\x23\x34\x5d\x20\xa\x9"                                  \
         "\x6c\x64\x72\x20\x72\x31\x2c\x20\x5b\x25\x31\x2c\x20\x23\x38\x5d\x20\xa\x9"                                  \
         "\x6c\x64\x72\x20\x72\x34\x2c\x20\x5b\x25\x31\x5d\x20\xa\x9"  /* target->r4 */                    \
         VALGRIND_BRANCH_AND_LINK_TO_NOREDIR_R4                   \
         "\x6d\x6f\x76\x20\x25\x30\x2c\x20\x72\x30\xa"                                           \
         : /*out*/   "\x3d\x72" (_res)                                  \
         : /*in*/    "\x30" (&_argvec[0])                            \
         : /*trash*/ "\x63\x63", "\x6d\x65\x6d\x6f\x72\x79", __CALLER_SAVED_REGS          \
      );                                                          \
      lval = (__typeof__(lval)) _res;                             \
   } while (0)

#define CALL_FN_W_WWW(lval, orig, arg1,arg2,arg3)                 \
   do {                                                           \
      volatile OrigFn        _orig = (orig);                      \
      volatile unsigned long _argvec[4];                          \
      volatile unsigned long _res;                                \
      _argvec[0] = (unsigned long)_orig.nraddr;                   \
      _argvec[1] = (unsigned long)(arg1);                         \
      _argvec[2] = (unsigned long)(arg2);                         \
      _argvec[3] = (unsigned long)(arg3);                         \
      __asm__ volatile(                                           \
         "\x6c\x64\x72\x20\x72\x30\x2c\x20\x5b\x25\x31\x2c\x20\x23\x34\x5d\x20\xa\x9"                                  \
         "\x6c\x64\x72\x20\x72\x31\x2c\x20\x5b\x25\x31\x2c\x20\x23\x38\x5d\x20\xa\x9"                                  \
         "\x6c\x64\x72\x20\x72\x32\x2c\x20\x5b\x25\x31\x2c\x20\x23\x31\x32\x5d\x20\xa\x9"                                 \
         "\x6c\x64\x72\x20\x72\x34\x2c\x20\x5b\x25\x31\x5d\x20\xa\x9"  /* target->r4 */                    \
         VALGRIND_BRANCH_AND_LINK_TO_NOREDIR_R4                   \
         "\x6d\x6f\x76\x20\x25\x30\x2c\x20\x72\x30\xa"                                           \
         : /*out*/   "\x3d\x72" (_res)                                  \
         : /*in*/    "\x30" (&_argvec[0])                            \
         : /*trash*/ "\x63\x63", "\x6d\x65\x6d\x6f\x72\x79", __CALLER_SAVED_REGS          \
      );                                                          \
      lval = (__typeof__(lval)) _res;                             \
   } while (0)

#define CALL_FN_W_WWWW(lval, orig, arg1,arg2,arg3,arg4)           \
   do {                                                           \
      volatile OrigFn        _orig = (orig);                      \
      volatile unsigned long _argvec[5];                          \
      volatile unsigned long _res;                                \
      _argvec[0] = (unsigned long)_orig.nraddr;                   \
      _argvec[1] = (unsigned long)(arg1);                         \
      _argvec[2] = (unsigned long)(arg2);                         \
      _argvec[3] = (unsigned long)(arg3);                         \
      _argvec[4] = (unsigned long)(arg4);                         \
      __asm__ volatile(                                           \
         "\x6c\x64\x72\x20\x72\x30\x2c\x20\x5b\x25\x31\x2c\x20\x23\x34\x5d\x20\xa\x9"                                  \
         "\x6c\x64\x72\x20\x72\x31\x2c\x20\x5b\x25\x31\x2c\x20\x23\x38\x5d\x20\xa\x9"                                  \
         "\x6c\x64\x72\x20\x72\x32\x2c\x20\x5b\x25\x31\x2c\x20\x23\x31\x32\x5d\x20\xa\x9"                                 \
         "\x6c\x64\x72\x20\x72\x33\x2c\x20\x5b\x25\x31\x2c\x20\x23\x31\x36\x5d\x20\xa\x9"                                 \
         "\x6c\x64\x72\x20\x72\x34\x2c\x20\x5b\x25\x31\x5d\x20\xa\x9"  /* target->r4 */                    \
         VALGRIND_BRANCH_AND_LINK_TO_NOREDIR_R4                   \
         "\x6d\x6f\x76\x20\x25\x30\x2c\x20\x72\x30"                                             \
         : /*out*/   "\x3d\x72" (_res)                                  \
         : /*in*/    "\x30" (&_argvec[0])                            \
         : /*trash*/ "\x63\x63", "\x6d\x65\x6d\x6f\x72\x79", __CALLER_SAVED_REGS          \
      );                                                          \
      lval = (__typeof__(lval)) _res;                             \
   } while (0)

#define CALL_FN_W_5W(lval, orig, arg1,arg2,arg3,arg4,arg5)        \
   do {                                                           \
      volatile OrigFn        _orig = (orig);                      \
      volatile unsigned long _argvec[6];                          \
      volatile unsigned long _res;                                \
      _argvec[0] = (unsigned long)_orig.nraddr;                   \
      _argvec[1] = (unsigned long)(arg1);                         \
      _argvec[2] = (unsigned long)(arg2);                         \
      _argvec[3] = (unsigned long)(arg3);                         \
      _argvec[4] = (unsigned long)(arg4);                         \
      _argvec[5] = (unsigned long)(arg5);                         \
      __asm__ volatile(                                           \
         "\x6c\x64\x72\x20\x72\x30\x2c\x20\x5b\x25\x31\x2c\x20\x23\x32\x30\x5d\x20\xa\x9"                                 \
         "\x70\x75\x73\x68\x20\x7b\x72\x30\x7d\x20\xa\x9"                                         \
         "\x6c\x64\x72\x20\x72\x30\x2c\x20\x5b\x25\x31\x2c\x20\x23\x34\x5d\x20\xa\x9"                                  \
         "\x6c\x64\x72\x20\x72\x31\x2c\x20\x5b\x25\x31\x2c\x20\x23\x38\x5d\x20\xa\x9"                                  \
         "\x6c\x64\x72\x20\x72\x32\x2c\x20\x5b\x25\x31\x2c\x20\x23\x31\x32\x5d\x20\xa\x9"                                 \
         "\x6c\x64\x72\x20\x72\x33\x2c\x20\x5b\x25\x31\x2c\x20\x23\x31\x36\x5d\x20\xa\x9"                                 \
         "\x6c\x64\x72\x20\x72\x34\x2c\x20\x5b\x25\x31\x5d\x20\xa\x9"  /* target->r4 */                    \
         VALGRIND_BRANCH_AND_LINK_TO_NOREDIR_R4                   \
         "\x61\x64\x64\x20\x73\x70\x2c\x20\x73\x70\x2c\x20\x23\x34\x20\xa\x9"                                    \
         "\x6d\x6f\x76\x20\x25\x30\x2c\x20\x72\x30"                                             \
         : /*out*/   "\x3d\x72" (_res)                                  \
         : /*in*/    "\x30" (&_argvec[0])                            \
         : /*trash*/ "\x63\x63", "\x6d\x65\x6d\x6f\x72\x79", __CALLER_SAVED_REGS          \
      );                                                          \
      lval = (__typeof__(lval)) _res;                             \
   } while (0)

#define CALL_FN_W_6W(lval, orig, arg1,arg2,arg3,arg4,arg5,arg6)   \
   do {                                                           \
      volatile OrigFn        _orig = (orig);                      \
      volatile unsigned long _argvec[7];                          \
      volatile unsigned long _res;                                \
      _argvec[0] = (unsigned long)_orig.nraddr;                   \
      _argvec[1] = (unsigned long)(arg1);                         \
      _argvec[2] = (unsigned long)(arg2);                         \
      _argvec[3] = (unsigned long)(arg3);                         \
      _argvec[4] = (unsigned long)(arg4);                         \
      _argvec[5] = (unsigned long)(arg5);                         \
      _argvec[6] = (unsigned long)(arg6);                         \
      __asm__ volatile(                                           \
         "\x6c\x64\x72\x20\x72\x30\x2c\x20\x5b\x25\x31\x2c\x20\x23\x32\x30\x5d\x20\xa\x9"                                 \
         "\x6c\x64\x72\x20\x72\x31\x2c\x20\x5b\x25\x31\x2c\x20\x23\x32\x34\x5d\x20\xa\x9"                                 \
         "\x70\x75\x73\x68\x20\x7b\x72\x30\x2c\x20\x72\x31\x7d\x20\xa\x9"                                     \
         "\x6c\x64\x72\x20\x72\x30\x2c\x20\x5b\x25\x31\x2c\x20\x23\x34\x5d\x20\xa\x9"                                  \
         "\x6c\x64\x72\x20\x72\x31\x2c\x20\x5b\x25\x31\x2c\x20\x23\x38\x5d\x20\xa\x9"                                  \
         "\x6c\x64\x72\x20\x72\x32\x2c\x20\x5b\x25\x31\x2c\x20\x23\x31\x32\x5d\x20\xa\x9"                                 \
         "\x6c\x64\x72\x20\x72\x33\x2c\x20\x5b\x25\x31\x2c\x20\x23\x31\x36\x5d\x20\xa\x9"                                 \
         "\x6c\x64\x72\x20\x72\x34\x2c\x20\x5b\x25\x31\x5d\x20\xa\x9"  /* target->r4 */                    \
         VALGRIND_BRANCH_AND_LINK_TO_NOREDIR_R4                   \
         "\x61\x64\x64\x20\x73\x70\x2c\x20\x73\x70\x2c\x20\x23\x38\x20\xa\x9"                                    \
         "\x6d\x6f\x76\x20\x25\x30\x2c\x20\x72\x30"                                             \
         : /*out*/   "\x3d\x72" (_res)                                  \
         : /*in*/    "\x30" (&_argvec[0])                            \
         : /*trash*/ "\x63\x63", "\x6d\x65\x6d\x6f\x72\x79", __CALLER_SAVED_REGS          \
      );                                                          \
      lval = (__typeof__(lval)) _res;                             \
   } while (0)

#define CALL_FN_W_7W(lval, orig, arg1,arg2,arg3,arg4,arg5,arg6,   \
                                 arg7)                            \
   do {                                                           \
      volatile OrigFn        _orig = (orig);                      \
      volatile unsigned long _argvec[8];                          \
      volatile unsigned long _res;                                \
      _argvec[0] = (unsigned long)_orig.nraddr;                   \
      _argvec[1] = (unsigned long)(arg1);                         \
      _argvec[2] = (unsigned long)(arg2);                         \
      _argvec[3] = (unsigned long)(arg3);                         \
      _argvec[4] = (unsigned long)(arg4);                         \
      _argvec[5] = (unsigned long)(arg5);                         \
      _argvec[6] = (unsigned long)(arg6);                         \
      _argvec[7] = (unsigned long)(arg7);                         \
      __asm__ volatile(                                           \
         "\x6c\x64\x72\x20\x72\x30\x2c\x20\x5b\x25\x31\x2c\x20\x23\x32\x30\x5d\x20\xa\x9"                                 \
         "\x6c\x64\x72\x20\x72\x31\x2c\x20\x5b\x25\x31\x2c\x20\x23\x32\x34\x5d\x20\xa\x9"                                 \
         "\x6c\x64\x72\x20\x72\x32\x2c\x20\x5b\x25\x31\x2c\x20\x23\x32\x38\x5d\x20\xa\x9"                                 \
         "\x70\x75\x73\x68\x20\x7b\x72\x30\x2c\x20\x72\x31\x2c\x20\x72\x32\x7d\x20\xa\x9"                                 \
         "\x6c\x64\x72\x20\x72\x30\x2c\x20\x5b\x25\x31\x2c\x20\x23\x34\x5d\x20\xa\x9"                                  \
         "\x6c\x64\x72\x20\x72\x31\x2c\x20\x5b\x25\x31\x2c\x20\x23\x38\x5d\x20\xa\x9"                                  \
         "\x6c\x64\x72\x20\x72\x32\x2c\x20\x5b\x25\x31\x2c\x20\x23\x31\x32\x5d\x20\xa\x9"                                 \
         "\x6c\x64\x72\x20\x72\x33\x2c\x20\x5b\x25\x31\x2c\x20\x23\x31\x36\x5d\x20\xa\x9"                                 \
         "\x6c\x64\x72\x20\x72\x34\x2c\x20\x5b\x25\x31\x5d\x20\xa\x9"  /* target->r4 */                    \
         VALGRIND_BRANCH_AND_LINK_TO_NOREDIR_R4                   \
         "\x61\x64\x64\x20\x73\x70\x2c\x20\x73\x70\x2c\x20\x23\x31\x32\x20\xa\x9"                                   \
         "\x6d\x6f\x76\x20\x25\x30\x2c\x20\x72\x30"                                             \
         : /*out*/   "\x3d\x72" (_res)                                  \
         : /*in*/    "\x30" (&_argvec[0])                            \
         : /*trash*/ "\x63\x63", "\x6d\x65\x6d\x6f\x72\x79", __CALLER_SAVED_REGS          \
      );                                                          \
      lval = (__typeof__(lval)) _res;                             \
   } while (0)

#define CALL_FN_W_8W(lval, orig, arg1,arg2,arg3,arg4,arg5,arg6,   \
                                 arg7,arg8)                       \
   do {                                                           \
      volatile OrigFn        _orig = (orig);                      \
      volatile unsigned long _argvec[9];                          \
      volatile unsigned long _res;                                \
      _argvec[0] = (unsigned long)_orig.nraddr;                   \
      _argvec[1] = (unsigned long)(arg1);                         \
      _argvec[2] = (unsigned long)(arg2);                         \
      _argvec[3] = (unsigned long)(arg3);                         \
      _argvec[4] = (unsigned long)(arg4);                         \
      _argvec[5] = (unsigned long)(arg5);                         \
      _argvec[6] = (unsigned long)(arg6);                         \
      _argvec[7] = (unsigned long)(arg7);                         \
      _argvec[8] = (unsigned long)(arg8);                         \
      __asm__ volatile(                                           \
         "\x6c\x64\x72\x20\x72\x30\x2c\x20\x5b\x25\x31\x2c\x20\x23\x32\x30\x5d\x20\xa\x9"                                 \
         "\x6c\x64\x72\x20\x72\x31\x2c\x20\x5b\x25\x31\x2c\x20\x23\x32\x34\x5d\x20\xa\x9"                                 \
         "\x6c\x64\x72\x20\x72\x32\x2c\x20\x5b\x25\x31\x2c\x20\x23\x32\x38\x5d\x20\xa\x9"                                 \
         "\x6c\x64\x72\x20\x72\x33\x2c\x20\x5b\x25\x31\x2c\x20\x23\x33\x32\x5d\x20\xa\x9"                                 \
         "\x70\x75\x73\x68\x20\x7b\x72\x30\x2c\x20\x72\x31\x2c\x20\x72\x32\x2c\x20\x72\x33\x7d\x20\xa\x9"                             \
         "\x6c\x64\x72\x20\x72\x30\x2c\x20\x5b\x25\x31\x2c\x20\x23\x34\x5d\x20\xa\x9"                                  \
         "\x6c\x64\x72\x20\x72\x31\x2c\x20\x5b\x25\x31\x2c\x20\x23\x38\x5d\x20\xa\x9"                                  \
         "\x6c\x64\x72\x20\x72\x32\x2c\x20\x5b\x25\x31\x2c\x20\x23\x31\x32\x5d\x20\xa\x9"                                 \
         "\x6c\x64\x72\x20\x72\x33\x2c\x20\x5b\x25\x31\x2c\x20\x23\x31\x36\x5d\x20\xa\x9"                                 \
         "\x6c\x64\x72\x20\x72\x34\x2c\x20\x5b\x25\x31\x5d\x20\xa\x9"  /* target->r4 */                    \
         VALGRIND_BRANCH_AND_LINK_TO_NOREDIR_R4                   \
         "\x61\x64\x64\x20\x73\x70\x2c\x20\x73\x70\x2c\x20\x23\x31\x36\x20\xa\x9"                                   \
         "\x6d\x6f\x76\x20\x25\x30\x2c\x20\x72\x30"                                             \
         : /*out*/   "\x3d\x72" (_res)                                  \
         : /*in*/    "\x30" (&_argvec[0])                            \
         : /*trash*/ "\x63\x63", "\x6d\x65\x6d\x6f\x72\x79", __CALLER_SAVED_REGS          \
      );                                                          \
      lval = (__typeof__(lval)) _res;                             \
   } while (0)

#define CALL_FN_W_9W(lval, orig, arg1,arg2,arg3,arg4,arg5,arg6,   \
                                 arg7,arg8,arg9)                  \
   do {                                                           \
      volatile OrigFn        _orig = (orig);                      \
      volatile unsigned long _argvec[10];                         \
      volatile unsigned long _res;                                \
      _argvec[0] = (unsigned long)_orig.nraddr;                   \
      _argvec[1] = (unsigned long)(arg1);                         \
      _argvec[2] = (unsigned long)(arg2);                         \
      _argvec[3] = (unsigned long)(arg3);                         \
      _argvec[4] = (unsigned long)(arg4);                         \
      _argvec[5] = (unsigned long)(arg5);                         \
      _argvec[6] = (unsigned long)(arg6);                         \
      _argvec[7] = (unsigned long)(arg7);                         \
      _argvec[8] = (unsigned long)(arg8);                         \
      _argvec[9] = (unsigned long)(arg9);                         \
      __asm__ volatile(                                           \
         "\x6c\x64\x72\x20\x72\x30\x2c\x20\x5b\x25\x31\x2c\x20\x23\x32\x30\x5d\x20\xa\x9"                                 \
         "\x6c\x64\x72\x20\x72\x31\x2c\x20\x5b\x25\x31\x2c\x20\x23\x32\x34\x5d\x20\xa\x9"                                 \
         "\x6c\x64\x72\x20\x72\x32\x2c\x20\x5b\x25\x31\x2c\x20\x23\x32\x38\x5d\x20\xa\x9"                                 \
         "\x6c\x64\x72\x20\x72\x33\x2c\x20\x5b\x25\x31\x2c\x20\x23\x33\x32\x5d\x20\xa\x9"                                 \
         "\x6c\x64\x72\x20\x72\x34\x2c\x20\x5b\x25\x31\x2c\x20\x23\x33\x36\x5d\x20\xa\x9"                                 \
         "\x70\x75\x73\x68\x20\x7b\x72\x30\x2c\x20\x72\x31\x2c\x20\x72\x32\x2c\x20\x72\x33\x2c\x20\x72\x34\x7d\x20\xa\x9"                         \
         "\x6c\x64\x72\x20\x72\x30\x2c\x20\x5b\x25\x31\x2c\x20\x23\x34\x5d\x20\xa\x9"                                  \
         "\x6c\x64\x72\x20\x72\x31\x2c\x20\x5b\x25\x31\x2c\x20\x23\x38\x5d\x20\xa\x9"                                  \
         "\x6c\x64\x72\x20\x72\x32\x2c\x20\x5b\x25\x31\x2c\x20\x23\x31\x32\x5d\x20\xa\x9"                                 \
         "\x6c\x64\x72\x20\x72\x33\x2c\x20\x5b\x25\x31\x2c\x20\x23\x31\x36\x5d\x20\xa\x9"                                 \
         "\x6c\x64\x72\x20\x72\x34\x2c\x20\x5b\x25\x31\x5d\x20\xa\x9"  /* target->r4 */                    \
         VALGRIND_BRANCH_AND_LINK_TO_NOREDIR_R4                   \
         "\x61\x64\x64\x20\x73\x70\x2c\x20\x73\x70\x2c\x20\x23\x32\x30\x20\xa\x9"                                   \
         "\x6d\x6f\x76\x20\x25\x30\x2c\x20\x72\x30"                                             \
         : /*out*/   "\x3d\x72" (_res)                                  \
         : /*in*/    "\x30" (&_argvec[0])                            \
         : /*trash*/ "\x63\x63", "\x6d\x65\x6d\x6f\x72\x79", __CALLER_SAVED_REGS          \
      );                                                          \
      lval = (__typeof__(lval)) _res;                             \
   } while (0)

#define CALL_FN_W_10W(lval, orig, arg1,arg2,arg3,arg4,arg5,arg6,  \
                                  arg7,arg8,arg9,arg10)           \
   do {                                                           \
      volatile OrigFn        _orig = (orig);                      \
      volatile unsigned long _argvec[11];                         \
      volatile unsigned long _res;                                \
      _argvec[0] = (unsigned long)_orig.nraddr;                   \
      _argvec[1] = (unsigned long)(arg1);                         \
      _argvec[2] = (unsigned long)(arg2);                         \
      _argvec[3] = (unsigned long)(arg3);                         \
      _argvec[4] = (unsigned long)(arg4);                         \
      _argvec[5] = (unsigned long)(arg5);                         \
      _argvec[6] = (unsigned long)(arg6);                         \
      _argvec[7] = (unsigned long)(arg7);                         \
      _argvec[8] = (unsigned long)(arg8);                         \
      _argvec[9] = (unsigned long)(arg9);                         \
      _argvec[10] = (unsigned long)(arg10);                       \
      __asm__ volatile(                                           \
         "\x6c\x64\x72\x20\x72\x30\x2c\x20\x5b\x25\x31\x2c\x20\x23\x34\x30\x5d\x20\xa\x9"                                 \
         "\x70\x75\x73\x68\x20\x7b\x72\x30\x7d\x20\xa\x9"                                         \
         "\x6c\x64\x72\x20\x72\x30\x2c\x20\x5b\x25\x31\x2c\x20\x23\x32\x30\x5d\x20\xa\x9"                                 \
         "\x6c\x64\x72\x20\x72\x31\x2c\x20\x5b\x25\x31\x2c\x20\x23\x32\x34\x5d\x20\xa\x9"                                 \
         "\x6c\x64\x72\x20\x72\x32\x2c\x20\x5b\x25\x31\x2c\x20\x23\x32\x38\x5d\x20\xa\x9"                                 \
         "\x6c\x64\x72\x20\x72\x33\x2c\x20\x5b\x25\x31\x2c\x20\x23\x33\x32\x5d\x20\xa\x9"                                 \
         "\x6c\x64\x72\x20\x72\x34\x2c\x20\x5b\x25\x31\x2c\x20\x23\x33\x36\x5d\x20\xa\x9"                                 \
         "\x70\x75\x73\x68\x20\x7b\x72\x30\x2c\x20\x72\x31\x2c\x20\x72\x32\x2c\x20\x72\x33\x2c\x20\x72\x34\x7d\x20\xa\x9"                         \
         "\x6c\x64\x72\x20\x72\x30\x2c\x20\x5b\x25\x31\x2c\x20\x23\x34\x5d\x20\xa\x9"                                  \
         "\x6c\x64\x72\x20\x72\x31\x2c\x20\x5b\x25\x31\x2c\x20\x23\x38\x5d\x20\xa\x9"                                  \
         "\x6c\x64\x72\x20\x72\x32\x2c\x20\x5b\x25\x31\x2c\x20\x23\x31\x32\x5d\x20\xa\x9"                                 \
         "\x6c\x64\x72\x20\x72\x33\x2c\x20\x5b\x25\x31\x2c\x20\x23\x31\x36\x5d\x20\xa\x9"                                 \
         "\x6c\x64\x72\x20\x72\x34\x2c\x20\x5b\x25\x31\x5d\x20\xa\x9"  /* target->r4 */                    \
         VALGRIND_BRANCH_AND_LINK_TO_NOREDIR_R4                   \
         "\x61\x64\x64\x20\x73\x70\x2c\x20\x73\x70\x2c\x20\x23\x32\x34\x20\xa\x9"                                   \
         "\x6d\x6f\x76\x20\x25\x30\x2c\x20\x72\x30"                                             \
         : /*out*/   "\x3d\x72" (_res)                                  \
         : /*in*/    "\x30" (&_argvec[0])                            \
         : /*trash*/ "\x63\x63", "\x6d\x65\x6d\x6f\x72\x79", __CALLER_SAVED_REGS          \
      );                                                          \
      lval = (__typeof__(lval)) _res;                             \
   } while (0)

#define CALL_FN_W_11W(lval, orig, arg1,arg2,arg3,arg4,arg5,       \
                                  arg6,arg7,arg8,arg9,arg10,      \
                                  arg11)                          \
   do {                                                           \
      volatile OrigFn        _orig = (orig);                      \
      volatile unsigned long _argvec[12];                         \
      volatile unsigned long _res;                                \
      _argvec[0] = (unsigned long)_orig.nraddr;                   \
      _argvec[1] = (unsigned long)(arg1);                         \
      _argvec[2] = (unsigned long)(arg2);                         \
      _argvec[3] = (unsigned long)(arg3);                         \
      _argvec[4] = (unsigned long)(arg4);                         \
      _argvec[5] = (unsigned long)(arg5);                         \
      _argvec[6] = (unsigned long)(arg6);                         \
      _argvec[7] = (unsigned long)(arg7);                         \
      _argvec[8] = (unsigned long)(arg8);                         \
      _argvec[9] = (unsigned long)(arg9);                         \
      _argvec[10] = (unsigned long)(arg10);                       \
      _argvec[11] = (unsigned long)(arg11);                       \
      __asm__ volatile(                                           \
         "\x6c\x64\x72\x20\x72\x30\x2c\x20\x5b\x25\x31\x2c\x20\x23\x34\x30\x5d\x20\xa\x9"                                 \
         "\x6c\x64\x72\x20\x72\x31\x2c\x20\x5b\x25\x31\x2c\x20\x23\x34\x34\x5d\x20\xa\x9"                                 \
         "\x70\x75\x73\x68\x20\x7b\x72\x30\x2c\x20\x72\x31\x7d\x20\xa\x9"                                     \
         "\x6c\x64\x72\x20\x72\x30\x2c\x20\x5b\x25\x31\x2c\x20\x23\x32\x30\x5d\x20\xa\x9"                                 \
         "\x6c\x64\x72\x20\x72\x31\x2c\x20\x5b\x25\x31\x2c\x20\x23\x32\x34\x5d\x20\xa\x9"                                 \
         "\x6c\x64\x72\x20\x72\x32\x2c\x20\x5b\x25\x31\x2c\x20\x23\x32\x38\x5d\x20\xa\x9"                                 \
         "\x6c\x64\x72\x20\x72\x33\x2c\x20\x5b\x25\x31\x2c\x20\x23\x33\x32\x5d\x20\xa\x9"                                 \
         "\x6c\x64\x72\x20\x72\x34\x2c\x20\x5b\x25\x31\x2c\x20\x23\x33\x36\x5d\x20\xa\x9"                                 \
         "\x70\x75\x73\x68\x20\x7b\x72\x30\x2c\x20\x72\x31\x2c\x20\x72\x32\x2c\x20\x72\x33\x2c\x20\x72\x34\x7d\x20\xa\x9"                         \
         "\x6c\x64\x72\x20\x72\x30\x2c\x20\x5b\x25\x31\x2c\x20\x23\x34\x5d\x20\xa\x9"                                  \
         "\x6c\x64\x72\x20\x72\x31\x2c\x20\x5b\x25\x31\x2c\x20\x23\x38\x5d\x20\xa\x9"                                  \
         "\x6c\x64\x72\x20\x72\x32\x2c\x20\x5b\x25\x31\x2c\x20\x23\x31\x32\x5d\x20\xa\x9"                                 \
         "\x6c\x64\x72\x20\x72\x33\x2c\x20\x5b\x25\x31\x2c\x20\x23\x31\x36\x5d\x20\xa\x9"                                 \
         "\x6c\x64\x72\x20\x72\x34\x2c\x20\x5b\x25\x31\x5d\x20\xa\x9"  /* target->r4 */                    \
         VALGRIND_BRANCH_AND_LINK_TO_NOREDIR_R4                   \
         "\x61\x64\x64\x20\x73\x70\x2c\x20\x73\x70\x2c\x20\x23\x32\x38\x20\xa\x9"                                   \
         "\x6d\x6f\x76\x20\x25\x30\x2c\x20\x72\x30"                                             \
         : /*out*/   "\x3d\x72" (_res)                                  \
         : /*in*/    "\x30" (&_argvec[0])                            \
         : /*trash*/ "\x63\x63", "\x6d\x65\x6d\x6f\x72\x79",__CALLER_SAVED_REGS           \
      );                                                          \
      lval = (__typeof__(lval)) _res;                             \
   } while (0)

#define CALL_FN_W_12W(lval, orig, arg1,arg2,arg3,arg4,arg5,       \
                                  arg6,arg7,arg8,arg9,arg10,      \
                                  arg11,arg12)                    \
   do {                                                           \
      volatile OrigFn        _orig = (orig);                      \
      volatile unsigned long _argvec[13];                         \
      volatile unsigned long _res;                                \
      _argvec[0] = (unsigned long)_orig.nraddr;                   \
      _argvec[1] = (unsigned long)(arg1);                         \
      _argvec[2] = (unsigned long)(arg2);                         \
      _argvec[3] = (unsigned long)(arg3);                         \
      _argvec[4] = (unsigned long)(arg4);                         \
      _argvec[5] = (unsigned long)(arg5);                         \
      _argvec[6] = (unsigned long)(arg6);                         \
      _argvec[7] = (unsigned long)(arg7);                         \
      _argvec[8] = (unsigned long)(arg8);                         \
      _argvec[9] = (unsigned long)(arg9);                         \
      _argvec[10] = (unsigned long)(arg10);                       \
      _argvec[11] = (unsigned long)(arg11);                       \
      _argvec[12] = (unsigned long)(arg12);                       \
      __asm__ volatile(                                           \
         "\x6c\x64\x72\x20\x72\x30\x2c\x20\x5b\x25\x31\x2c\x20\x23\x34\x30\x5d\x20\xa\x9"                                 \
         "\x6c\x64\x72\x20\x72\x31\x2c\x20\x5b\x25\x31\x2c\x20\x23\x34\x34\x5d\x20\xa\x9"                                 \
         "\x6c\x64\x72\x20\x72\x32\x2c\x20\x5b\x25\x31\x2c\x20\x23\x34\x38\x5d\x20\xa\x9"                                 \
         "\x70\x75\x73\x68\x20\x7b\x72\x30\x2c\x20\x72\x31\x2c\x20\x72\x32\x7d\x20\xa\x9"                                 \
         "\x6c\x64\x72\x20\x72\x30\x2c\x20\x5b\x25\x31\x2c\x20\x23\x32\x30\x5d\x20\xa\x9"                                 \
         "\x6c\x64\x72\x20\x72\x31\x2c\x20\x5b\x25\x31\x2c\x20\x23\x32\x34\x5d\x20\xa\x9"                                 \
         "\x6c\x64\x72\x20\x72\x32\x2c\x20\x5b\x25\x31\x2c\x20\x23\x32\x38\x5d\x20\xa\x9"                                 \
         "\x6c\x64\x72\x20\x72\x33\x2c\x20\x5b\x25\x31\x2c\x20\x23\x33\x32\x5d\x20\xa\x9"                                 \
         "\x6c\x64\x72\x20\x72\x34\x2c\x20\x5b\x25\x31\x2c\x20\x23\x33\x36\x5d\x20\xa\x9"                                 \
         "\x70\x75\x73\x68\x20\x7b\x72\x30\x2c\x20\x72\x31\x2c\x20\x72\x32\x2c\x20\x72\x33\x2c\x20\x72\x34\x7d\x20\xa\x9"                         \
         "\x6c\x64\x72\x20\x72\x30\x2c\x20\x5b\x25\x31\x2c\x20\x23\x34\x5d\x20\xa\x9"                                  \
         "\x6c\x64\x72\x20\x72\x31\x2c\x20\x5b\x25\x31\x2c\x20\x23\x38\x5d\x20\xa\x9"                                  \
         "\x6c\x64\x72\x20\x72\x32\x2c\x20\x5b\x25\x31\x2c\x20\x23\x31\x32\x5d\x20\xa\x9"                                 \
         "\x6c\x64\x72\x20\x72\x33\x2c\x20\x5b\x25\x31\x2c\x20\x23\x31\x36\x5d\x20\xa\x9"                                 \
         "\x6c\x64\x72\x20\x72\x34\x2c\x20\x5b\x25\x31\x5d\x20\xa\x9"  /* target->r4 */                    \
         VALGRIND_BRANCH_AND_LINK_TO_NOREDIR_R4                   \
         "\x61\x64\x64\x20\x73\x70\x2c\x20\x73\x70\x2c\x20\x23\x33\x32\x20\xa\x9"                                   \
         "\x6d\x6f\x76\x20\x25\x30\x2c\x20\x72\x30"                                             \
         : /*out*/   "\x3d\x72" (_res)                                  \
         : /*in*/    "\x30" (&_argvec[0])                            \
         : /*trash*/ "\x63\x63", "\x6d\x65\x6d\x6f\x72\x79", __CALLER_SAVED_REGS          \
      );                                                          \
      lval = (__typeof__(lval)) _res;                             \
   } while (0)

#endif /* PLAT_arm_linux */

/* ------------------------- s390x-linux ------------------------- */

#if defined(PLAT_s390x_linux)

/* Similar workaround as amd64 (see above), but we use r11 as frame
   pointer and save the old r11 in r7. r11 might be used for
   argvec, therefore we copy argvec in r1 since r1 is clobbered
   after the call anyway.  */
#if defined(__GNUC__) && defined(__GCC_HAVE_DWARF2_CFI_ASM)
#  define __FRAME_POINTER                                         \
      ,"\x64"(__builtin_dwarf_cfa())
#  define VALGRIND_CFI_PROLOGUE                                   \
      "\x2e\x63\x66\x69\x5f\x72\x65\x6d\x65\x6d\x62\x65\x72\x5f\x73\x74\x61\x74\x65\xa\x9"                                   \
      "\x6c\x67\x72\x20\x31\x2c\x25\x31\xa\x9" /* copy the argvec pointer in r1 */          \
      "\x6c\x67\x72\x20\x37\x2c\x31\x31\xa\x9"                                              \
      "\x6c\x67\x72\x20\x31\x31\x2c\x25\x32\xa\x9"                                             \
      "\x2e\x63\x66\x69\x5f\x64\x65\x66\x5f\x63\x66\x61\x20\x72\x31\x31\x2c\x20\x30\xa\x9"
#  define VALGRIND_CFI_EPILOGUE                                   \
      "\x6c\x67\x72\x20\x31\x31\x2c\x20\x37\xa\x9"                                             \
      "\x2e\x63\x66\x69\x5f\x72\x65\x73\x74\x6f\x72\x65\x5f\x73\x74\x61\x74\x65\xa\x9"
#else
#  define __FRAME_POINTER
#  define VALGRIND_CFI_PROLOGUE                                   \
      "\x6c\x67\x72\x20\x31\x2c\x25\x31\xa\x9"
#  define VALGRIND_CFI_EPILOGUE
#endif




/* These regs are trashed by the hidden call. Note that we overwrite
   r14 in s390_irgen_noredir (VEX/priv/guest_s390_irgen.c) to give the
   function a proper return address. All others are ABI defined call
   clobbers. */
#define __CALLER_SAVED_REGS "\x30","\x31","\x32","\x33","\x34","\x35","\x31\x34", \
                           "\x66\x30","\x66\x31","\x66\x32","\x66\x33","\x66\x34","\x66\x35","\x66\x36","\x66\x37"


#define CALL_FN_W_v(lval, orig)                                  \
   do {                                                          \
      volatile OrigFn        _orig = (orig);                     \
      volatile unsigned long  _argvec[1];                        \
      volatile unsigned long _res;                               \
      _argvec[0] = (unsigned long)_orig.nraddr;                  \
      __asm__ volatile(                                          \
         VALGRIND_CFI_PROLOGUE                                   \
         "\x61\x67\x68\x69\x20\x31\x35\x2c\x2d\x31\x36\x30\xa\x9"                                      \
         "\x6c\x67\x20\x31\x2c\x20\x30\x28\x31\x29\xa\x9"  /* target->r1 */                      \
         VALGRIND_CALL_NOREDIR_R1                                \
         "\x6c\x67\x72\x20\x25\x30\x2c\x20\x32\xa\x9"                                         \
         "\x61\x67\x68\x69\x20\x31\x35\x2c\x31\x36\x30\xa\x9"                                       \
         VALGRIND_CFI_EPILOGUE                                   \
         : /*out*/   "\x3d\x64" (_res)                                 \
         : /*in*/    "\x64" (&_argvec[0]) __FRAME_POINTER           \
         : /*trash*/ "\x63\x63", "\x6d\x65\x6d\x6f\x72\x79", __CALLER_SAVED_REGS,"\x37"     \
      );                                                         \
      lval = (__typeof__(lval)) _res;                            \
   } while (0)

/* The call abi has the arguments in r2-r6 and stack */
#define CALL_FN_W_W(lval, orig, arg1)                            \
   do {                                                          \
      volatile OrigFn        _orig = (orig);                     \
      volatile unsigned long _argvec[2];                         \
      volatile unsigned long _res;                               \
      _argvec[0] = (unsigned long)_orig.nraddr;                  \
      _argvec[1] = (unsigned long)arg1;                          \
      __asm__ volatile(                                          \
         VALGRIND_CFI_PROLOGUE                                   \
         "\x61\x67\x68\x69\x20\x31\x35\x2c\x2d\x31\x36\x30\xa\x9"                                      \
         "\x6c\x67\x20\x32\x2c\x20\x38\x28\x31\x29\xa\x9"                                        \
         "\x6c\x67\x20\x31\x2c\x20\x30\x28\x31\x29\xa\x9"                                        \
         VALGRIND_CALL_NOREDIR_R1                                \
         "\x6c\x67\x72\x20\x25\x30\x2c\x20\x32\xa\x9"                                         \
         "\x61\x67\x68\x69\x20\x31\x35\x2c\x31\x36\x30\xa\x9"                                       \
         VALGRIND_CFI_EPILOGUE                                   \
         : /*out*/   "\x3d\x64" (_res)                                 \
         : /*in*/    "\x61" (&_argvec[0]) __FRAME_POINTER           \
         : /*trash*/ "\x63\x63", "\x6d\x65\x6d\x6f\x72\x79", __CALLER_SAVED_REGS,"\x37"     \
      );                                                         \
      lval = (__typeof__(lval)) _res;                            \
   } while (0)

#define CALL_FN_W_WW(lval, orig, arg1, arg2)                     \
   do {                                                          \
      volatile OrigFn        _orig = (orig);                     \
      volatile unsigned long _argvec[3];                         \
      volatile unsigned long _res;                               \
      _argvec[0] = (unsigned long)_orig.nraddr;                  \
      _argvec[1] = (unsigned long)arg1;                          \
      _argvec[2] = (unsigned long)arg2;                          \
      __asm__ volatile(                                          \
         VALGRIND_CFI_PROLOGUE                                   \
         "\x61\x67\x68\x69\x20\x31\x35\x2c\x2d\x31\x36\x30\xa\x9"                                      \
         "\x6c\x67\x20\x32\x2c\x20\x38\x28\x31\x29\xa\x9"                                        \
         "\x6c\x67\x20\x33\x2c\x31\x36\x28\x31\x29\xa\x9"                                        \
         "\x6c\x67\x20\x31\x2c\x20\x30\x28\x31\x29\xa\x9"                                        \
         VALGRIND_CALL_NOREDIR_R1                                \
         "\x6c\x67\x72\x20\x25\x30\x2c\x20\x32\xa\x9"                                         \
         "\x61\x67\x68\x69\x20\x31\x35\x2c\x31\x36\x30\xa\x9"                                       \
         VALGRIND_CFI_EPILOGUE                                   \
         : /*out*/   "\x3d\x64" (_res)                                 \
         : /*in*/    "\x61" (&_argvec[0]) __FRAME_POINTER           \
         : /*trash*/ "\x63\x63", "\x6d\x65\x6d\x6f\x72\x79", __CALLER_SAVED_REGS,"\x37"     \
      );                                                         \
      lval = (__typeof__(lval)) _res;                            \
   } while (0)

#define CALL_FN_W_WWW(lval, orig, arg1, arg2, arg3)              \
   do {                                                          \
      volatile OrigFn        _orig = (orig);                     \
      volatile unsigned long _argvec[4];                         \
      volatile unsigned long _res;                               \
      _argvec[0] = (unsigned long)_orig.nraddr;                  \
      _argvec[1] = (unsigned long)arg1;                          \
      _argvec[2] = (unsigned long)arg2;                          \
      _argvec[3] = (unsigned long)arg3;                          \
      __asm__ volatile(                                          \
         VALGRIND_CFI_PROLOGUE                                   \
         "\x61\x67\x68\x69\x20\x31\x35\x2c\x2d\x31\x36\x30\xa\x9"                                      \
         "\x6c\x67\x20\x32\x2c\x20\x38\x28\x31\x29\xa\x9"                                        \
         "\x6c\x67\x20\x33\x2c\x31\x36\x28\x31\x29\xa\x9"                                        \
         "\x6c\x67\x20\x34\x2c\x32\x34\x28\x31\x29\xa\x9"                                        \
         "\x6c\x67\x20\x31\x2c\x20\x30\x28\x31\x29\xa\x9"                                        \
         VALGRIND_CALL_NOREDIR_R1                                \
         "\x6c\x67\x72\x20\x25\x30\x2c\x20\x32\xa\x9"                                         \
         "\x61\x67\x68\x69\x20\x31\x35\x2c\x31\x36\x30\xa\x9"                                       \
         VALGRIND_CFI_EPILOGUE                                   \
         : /*out*/   "\x3d\x64" (_res)                                 \
         : /*in*/    "\x61" (&_argvec[0]) __FRAME_POINTER           \
         : /*trash*/ "\x63\x63", "\x6d\x65\x6d\x6f\x72\x79", __CALLER_SAVED_REGS,"\x37"     \
      );                                                         \
      lval = (__typeof__(lval)) _res;                            \
   } while (0)

#define CALL_FN_W_WWWW(lval, orig, arg1, arg2, arg3, arg4)       \
   do {                                                          \
      volatile OrigFn        _orig = (orig);                     \
      volatile unsigned long _argvec[5];                         \
      volatile unsigned long _res;                               \
      _argvec[0] = (unsigned long)_orig.nraddr;                  \
      _argvec[1] = (unsigned long)arg1;                          \
      _argvec[2] = (unsigned long)arg2;                          \
      _argvec[3] = (unsigned long)arg3;                          \
      _argvec[4] = (unsigned long)arg4;                          \
      __asm__ volatile(                                          \
         VALGRIND_CFI_PROLOGUE                                   \
         "\x61\x67\x68\x69\x20\x31\x35\x2c\x2d\x31\x36\x30\xa\x9"                                      \
         "\x6c\x67\x20\x32\x2c\x20\x38\x28\x31\x29\xa\x9"                                        \
         "\x6c\x67\x20\x33\x2c\x31\x36\x28\x31\x29\xa\x9"                                        \
         "\x6c\x67\x20\x34\x2c\x32\x34\x28\x31\x29\xa\x9"                                        \
         "\x6c\x67\x20\x35\x2c\x33\x32\x28\x31\x29\xa\x9"                                        \
         "\x6c\x67\x20\x31\x2c\x20\x30\x28\x31\x29\xa\x9"                                        \
         VALGRIND_CALL_NOREDIR_R1                                \
         "\x6c\x67\x72\x20\x25\x30\x2c\x20\x32\xa\x9"                                         \
         "\x61\x67\x68\x69\x20\x31\x35\x2c\x31\x36\x30\xa\x9"                                       \
         VALGRIND_CFI_EPILOGUE                                   \
         : /*out*/   "\x3d\x64" (_res)                                 \
         : /*in*/    "\x61" (&_argvec[0]) __FRAME_POINTER           \
         : /*trash*/ "\x63\x63", "\x6d\x65\x6d\x6f\x72\x79", __CALLER_SAVED_REGS,"\x37"     \
      );                                                         \
      lval = (__typeof__(lval)) _res;                            \
   } while (0)

#define CALL_FN_W_5W(lval, orig, arg1, arg2, arg3, arg4, arg5)   \
   do {                                                          \
      volatile OrigFn        _orig = (orig);                     \
      volatile unsigned long _argvec[6];                         \
      volatile unsigned long _res;                               \
      _argvec[0] = (unsigned long)_orig.nraddr;                  \
      _argvec[1] = (unsigned long)arg1;                          \
      _argvec[2] = (unsigned long)arg2;                          \
      _argvec[3] = (unsigned long)arg3;                          \
      _argvec[4] = (unsigned long)arg4;                          \
      _argvec[5] = (unsigned long)arg5;                          \
      __asm__ volatile(                                          \
         VALGRIND_CFI_PROLOGUE                                   \
         "\x61\x67\x68\x69\x20\x31\x35\x2c\x2d\x31\x36\x30\xa\x9"                                      \
         "\x6c\x67\x20\x32\x2c\x20\x38\x28\x31\x29\xa\x9"                                        \
         "\x6c\x67\x20\x33\x2c\x31\x36\x28\x31\x29\xa\x9"                                        \
         "\x6c\x67\x20\x34\x2c\x32\x34\x28\x31\x29\xa\x9"                                        \
         "\x6c\x67\x20\x35\x2c\x33\x32\x28\x31\x29\xa\x9"                                        \
         "\x6c\x67\x20\x36\x2c\x34\x30\x28\x31\x29\xa\x9"                                        \
         "\x6c\x67\x20\x31\x2c\x20\x30\x28\x31\x29\xa\x9"                                        \
         VALGRIND_CALL_NOREDIR_R1                                \
         "\x6c\x67\x72\x20\x25\x30\x2c\x20\x32\xa\x9"                                         \
         "\x61\x67\x68\x69\x20\x31\x35\x2c\x31\x36\x30\xa\x9"                                       \
         VALGRIND_CFI_EPILOGUE                                   \
         : /*out*/   "\x3d\x64" (_res)                                 \
         : /*in*/    "\x61" (&_argvec[0]) __FRAME_POINTER           \
         : /*trash*/ "\x63\x63", "\x6d\x65\x6d\x6f\x72\x79", __CALLER_SAVED_REGS,"\x36","\x37" \
      );                                                         \
      lval = (__typeof__(lval)) _res;                            \
   } while (0)

#define CALL_FN_W_6W(lval, orig, arg1, arg2, arg3, arg4, arg5,   \
                     arg6)                                       \
   do {                                                          \
      volatile OrigFn        _orig = (orig);                     \
      volatile unsigned long _argvec[7];                         \
      volatile unsigned long _res;                               \
      _argvec[0] = (unsigned long)_orig.nraddr;                  \
      _argvec[1] = (unsigned long)arg1;                          \
      _argvec[2] = (unsigned long)arg2;                          \
      _argvec[3] = (unsigned long)arg3;                          \
      _argvec[4] = (unsigned long)arg4;                          \
      _argvec[5] = (unsigned long)arg5;                          \
      _argvec[6] = (unsigned long)arg6;                          \
      __asm__ volatile(                                          \
         VALGRIND_CFI_PROLOGUE                                   \
         "\x61\x67\x68\x69\x20\x31\x35\x2c\x2d\x31\x36\x38\xa\x9"                                      \
         "\x6c\x67\x20\x32\x2c\x20\x38\x28\x31\x29\xa\x9"                                        \
         "\x6c\x67\x20\x33\x2c\x31\x36\x28\x31\x29\xa\x9"                                        \
         "\x6c\x67\x20\x34\x2c\x32\x34\x28\x31\x29\xa\x9"                                        \
         "\x6c\x67\x20\x35\x2c\x33\x32\x28\x31\x29\xa\x9"                                        \
         "\x6c\x67\x20\x36\x2c\x34\x30\x28\x31\x29\xa\x9"                                        \
         "\x6d\x76\x63\x20\x31\x36\x30\x28\x38\x2c\x31\x35\x29\x2c\x20\x34\x38\x28\x31\x29\xa\x9"                              \
         "\x6c\x67\x20\x31\x2c\x20\x30\x28\x31\x29\xa\x9"                                        \
         VALGRIND_CALL_NOREDIR_R1                                \
         "\x6c\x67\x72\x20\x25\x30\x2c\x20\x32\xa\x9"                                         \
         "\x61\x67\x68\x69\x20\x31\x35\x2c\x31\x36\x38\xa\x9"                                       \
         VALGRIND_CFI_EPILOGUE                                   \
         : /*out*/   "\x3d\x64" (_res)                                 \
         : /*in*/    "\x61" (&_argvec[0]) __FRAME_POINTER           \
         : /*trash*/ "\x63\x63", "\x6d\x65\x6d\x6f\x72\x79", __CALLER_SAVED_REGS,"\x36","\x37" \
      );                                                         \
      lval = (__typeof__(lval)) _res;                            \
   } while (0)

#define CALL_FN_W_7W(lval, orig, arg1, arg2, arg3, arg4, arg5,   \
                     arg6, arg7)                                 \
   do {                                                          \
      volatile OrigFn        _orig = (orig);                     \
      volatile unsigned long _argvec[8];                         \
      volatile unsigned long _res;                               \
      _argvec[0] = (unsigned long)_orig.nraddr;                  \
      _argvec[1] = (unsigned long)arg1;                          \
      _argvec[2] = (unsigned long)arg2;                          \
      _argvec[3] = (unsigned long)arg3;                          \
      _argvec[4] = (unsigned long)arg4;                          \
      _argvec[5] = (unsigned long)arg5;                          \
      _argvec[6] = (unsigned long)arg6;                          \
      _argvec[7] = (unsigned long)arg7;                          \
      __asm__ volatile(                                          \
         VALGRIND_CFI_PROLOGUE                                   \
         "\x61\x67\x68\x69\x20\x31\x35\x2c\x2d\x31\x37\x36\xa\x9"                                      \
         "\x6c\x67\x20\x32\x2c\x20\x38\x28\x31\x29\xa\x9"                                        \
         "\x6c\x67\x20\x33\x2c\x31\x36\x28\x31\x29\xa\x9"                                        \
         "\x6c\x67\x20\x34\x2c\x32\x34\x28\x31\x29\xa\x9"                                        \
         "\x6c\x67\x20\x35\x2c\x33\x32\x28\x31\x29\xa\x9"                                        \
         "\x6c\x67\x20\x36\x2c\x34\x30\x28\x31\x29\xa\x9"                                        \
         "\x6d\x76\x63\x20\x31\x36\x30\x28\x38\x2c\x31\x35\x29\x2c\x20\x34\x38\x28\x31\x29\xa\x9"                              \
         "\x6d\x76\x63\x20\x31\x36\x38\x28\x38\x2c\x31\x35\x29\x2c\x20\x35\x36\x28\x31\x29\xa\x9"                              \
         "\x6c\x67\x20\x31\x2c\x20\x30\x28\x31\x29\xa\x9"                                        \
         VALGRIND_CALL_NOREDIR_R1                                \
         "\x6c\x67\x72\x20\x25\x30\x2c\x20\x32\xa\x9"                                         \
         "\x61\x67\x68\x69\x20\x31\x35\x2c\x31\x37\x36\xa\x9"                                       \
         VALGRIND_CFI_EPILOGUE                                   \
         : /*out*/   "\x3d\x64" (_res)                                 \
         : /*in*/    "\x61" (&_argvec[0]) __FRAME_POINTER           \
         : /*trash*/ "\x63\x63", "\x6d\x65\x6d\x6f\x72\x79", __CALLER_SAVED_REGS,"\x36","\x37" \
      );                                                         \
      lval = (__typeof__(lval)) _res;                            \
   } while (0)

#define CALL_FN_W_8W(lval, orig, arg1, arg2, arg3, arg4, arg5,   \
                     arg6, arg7 ,arg8)                           \
   do {                                                          \
      volatile OrigFn        _orig = (orig);                     \
      volatile unsigned long _argvec[9];                         \
      volatile unsigned long _res;                               \
      _argvec[0] = (unsigned long)_orig.nraddr;                  \
      _argvec[1] = (unsigned long)arg1;                          \
      _argvec[2] = (unsigned long)arg2;                          \
      _argvec[3] = (unsigned long)arg3;                          \
      _argvec[4] = (unsigned long)arg4;                          \
      _argvec[5] = (unsigned long)arg5;                          \
      _argvec[6] = (unsigned long)arg6;                          \
      _argvec[7] = (unsigned long)arg7;                          \
      _argvec[8] = (unsigned long)arg8;                          \
      __asm__ volatile(                                          \
         VALGRIND_CFI_PROLOGUE                                   \
         "\x61\x67\x68\x69\x20\x31\x35\x2c\x2d\x31\x38\x34\xa\x9"                                      \
         "\x6c\x67\x20\x32\x2c\x20\x38\x28\x31\x29\xa\x9"                                        \
         "\x6c\x67\x20\x33\x2c\x31\x36\x28\x31\x29\xa\x9"                                        \
         "\x6c\x67\x20\x34\x2c\x32\x34\x28\x31\x29\xa\x9"                                        \
         "\x6c\x67\x20\x35\x2c\x33\x32\x28\x31\x29\xa\x9"                                        \
         "\x6c\x67\x20\x36\x2c\x34\x30\x28\x31\x29\xa\x9"                                        \
         "\x6d\x76\x63\x20\x31\x36\x30\x28\x38\x2c\x31\x35\x29\x2c\x20\x34\x38\x28\x31\x29\xa\x9"                              \
         "\x6d\x76\x63\x20\x31\x36\x38\x28\x38\x2c\x31\x35\x29\x2c\x20\x35\x36\x28\x31\x29\xa\x9"                              \
         "\x6d\x76\x63\x20\x31\x37\x36\x28\x38\x2c\x31\x35\x29\x2c\x20\x36\x34\x28\x31\x29\xa\x9"                              \
         "\x6c\x67\x20\x31\x2c\x20\x30\x28\x31\x29\xa\x9"                                        \
         VALGRIND_CALL_NOREDIR_R1                                \
         "\x6c\x67\x72\x20\x25\x30\x2c\x20\x32\xa\x9"                                         \
         "\x61\x67\x68\x69\x20\x31\x35\x2c\x31\x38\x34\xa\x9"                                       \
         VALGRIND_CFI_EPILOGUE                                   \
         : /*out*/   "\x3d\x64" (_res)                                 \
         : /*in*/    "\x61" (&_argvec[0]) __FRAME_POINTER           \
         : /*trash*/ "\x63\x63", "\x6d\x65\x6d\x6f\x72\x79", __CALLER_SAVED_REGS,"\x36","\x37" \
      );                                                         \
      lval = (__typeof__(lval)) _res;                            \
   } while (0)

#define CALL_FN_W_9W(lval, orig, arg1, arg2, arg3, arg4, arg5,   \
                     arg6, arg7 ,arg8, arg9)                     \
   do {                                                          \
      volatile OrigFn        _orig = (orig);                     \
      volatile unsigned long _argvec[10];                        \
      volatile unsigned long _res;                               \
      _argvec[0] = (unsigned long)_orig.nraddr;                  \
      _argvec[1] = (unsigned long)arg1;                          \
      _argvec[2] = (unsigned long)arg2;                          \
      _argvec[3] = (unsigned long)arg3;                          \
      _argvec[4] = (unsigned long)arg4;                          \
      _argvec[5] = (unsigned long)arg5;                          \
      _argvec[6] = (unsigned long)arg6;                          \
      _argvec[7] = (unsigned long)arg7;                          \
      _argvec[8] = (unsigned long)arg8;                          \
      _argvec[9] = (unsigned long)arg9;                          \
      __asm__ volatile(                                          \
         VALGRIND_CFI_PROLOGUE                                   \
         "\x61\x67\x68\x69\x20\x31\x35\x2c\x2d\x31\x39\x32\xa\x9"                                      \
         "\x6c\x67\x20\x32\x2c\x20\x38\x28\x31\x29\xa\x9"                                        \
         "\x6c\x67\x20\x33\x2c\x31\x36\x28\x31\x29\xa\x9"                                        \
         "\x6c\x67\x20\x34\x2c\x32\x34\x28\x31\x29\xa\x9"                                        \
         "\x6c\x67\x20\x35\x2c\x33\x32\x28\x31\x29\xa\x9"                                        \
         "\x6c\x67\x20\x36\x2c\x34\x30\x28\x31\x29\xa\x9"                                        \
         "\x6d\x76\x63\x20\x31\x36\x30\x28\x38\x2c\x31\x35\x29\x2c\x20\x34\x38\x28\x31\x29\xa\x9"                              \
         "\x6d\x76\x63\x20\x31\x36\x38\x28\x38\x2c\x31\x35\x29\x2c\x20\x35\x36\x28\x31\x29\xa\x9"                              \
         "\x6d\x76\x63\x20\x31\x37\x36\x28\x38\x2c\x31\x35\x29\x2c\x20\x36\x34\x28\x31\x29\xa\x9"                              \
         "\x6d\x76\x63\x20\x31\x38\x34\x28\x38\x2c\x31\x35\x29\x2c\x20\x37\x32\x28\x31\x29\xa\x9"                              \
         "\x6c\x67\x20\x31\x2c\x20\x30\x28\x31\x29\xa\x9"                                        \
         VALGRIND_CALL_NOREDIR_R1                                \
         "\x6c\x67\x72\x20\x25\x30\x2c\x20\x32\xa\x9"                                         \
         "\x61\x67\x68\x69\x20\x31\x35\x2c\x31\x39\x32\xa\x9"                                       \
         VALGRIND_CFI_EPILOGUE                                   \
         : /*out*/   "\x3d\x64" (_res)                                 \
         : /*in*/    "\x61" (&_argvec[0]) __FRAME_POINTER           \
         : /*trash*/ "\x63\x63", "\x6d\x65\x6d\x6f\x72\x79", __CALLER_SAVED_REGS,"\x36","\x37" \
      );                                                         \
      lval = (__typeof__(lval)) _res;                            \
   } while (0)

#define CALL_FN_W_10W(lval, orig, arg1, arg2, arg3, arg4, arg5,  \
                     arg6, arg7 ,arg8, arg9, arg10)              \
   do {                                                          \
      volatile OrigFn        _orig = (orig);                     \
      volatile unsigned long _argvec[11];                        \
      volatile unsigned long _res;                               \
      _argvec[0] = (unsigned long)_orig.nraddr;                  \
      _argvec[1] = (unsigned long)arg1;                          \
      _argvec[2] = (unsigned long)arg2;                          \
      _argvec[3] = (unsigned long)arg3;                          \
      _argvec[4] = (unsigned long)arg4;                          \
      _argvec[5] = (unsigned long)arg5;                          \
      _argvec[6] = (unsigned long)arg6;                          \
      _argvec[7] = (unsigned long)arg7;                          \
      _argvec[8] = (unsigned long)arg8;                          \
      _argvec[9] = (unsigned long)arg9;                          \
      _argvec[10] = (unsigned long)arg10;                        \
      __asm__ volatile(                                          \
         VALGRIND_CFI_PROLOGUE                                   \
         "\x61\x67\x68\x69\x20\x31\x35\x2c\x2d\x32\x30\x30\xa\x9"                                      \
         "\x6c\x67\x20\x32\x2c\x20\x38\x28\x31\x29\xa\x9"                                        \
         "\x6c\x67\x20\x33\x2c\x31\x36\x28\x31\x29\xa\x9"                                        \
         "\x6c\x67\x20\x34\x2c\x32\x34\x28\x31\x29\xa\x9"                                        \
         "\x6c\x67\x20\x35\x2c\x33\x32\x28\x31\x29\xa\x9"                                        \
         "\x6c\x67\x20\x36\x2c\x34\x30\x28\x31\x29\xa\x9"                                        \
         "\x6d\x76\x63\x20\x31\x36\x30\x28\x38\x2c\x31\x35\x29\x2c\x20\x34\x38\x28\x31\x29\xa\x9"                              \
         "\x6d\x76\x63\x20\x31\x36\x38\x28\x38\x2c\x31\x35\x29\x2c\x20\x35\x36\x28\x31\x29\xa\x9"                              \
         "\x6d\x76\x63\x20\x31\x37\x36\x28\x38\x2c\x31\x35\x29\x2c\x20\x36\x34\x28\x31\x29\xa\x9"                              \
         "\x6d\x76\x63\x20\x31\x38\x34\x28\x38\x2c\x31\x35\x29\x2c\x20\x37\x32\x28\x31\x29\xa\x9"                              \
         "\x6d\x76\x63\x20\x31\x39\x32\x28\x38\x2c\x31\x35\x29\x2c\x20\x38\x30\x28\x31\x29\xa\x9"                              \
         "\x6c\x67\x20\x31\x2c\x20\x30\x28\x31\x29\xa\x9"                                        \
         VALGRIND_CALL_NOREDIR_R1                                \
         "\x6c\x67\x72\x20\x25\x30\x2c\x20\x32\xa\x9"                                         \
         "\x61\x67\x68\x69\x20\x31\x35\x2c\x32\x30\x30\xa\x9"                                       \
         VALGRIND_CFI_EPILOGUE                                   \
         : /*out*/   "\x3d\x64" (_res)                                 \
         : /*in*/    "\x61" (&_argvec[0]) __FRAME_POINTER           \
         : /*trash*/ "\x63\x63", "\x6d\x65\x6d\x6f\x72\x79", __CALLER_SAVED_REGS,"\x36","\x37" \
      );                                                         \
      lval = (__typeof__(lval)) _res;                            \
   } while (0)

#define CALL_FN_W_11W(lval, orig, arg1, arg2, arg3, arg4, arg5,  \
                     arg6, arg7 ,arg8, arg9, arg10, arg11)       \
   do {                                                          \
      volatile OrigFn        _orig = (orig);                     \
      volatile unsigned long _argvec[12];                        \
      volatile unsigned long _res;                               \
      _argvec[0] = (unsigned long)_orig.nraddr;                  \
      _argvec[1] = (unsigned long)arg1;                          \
      _argvec[2] = (unsigned long)arg2;                          \
      _argvec[3] = (unsigned long)arg3;                          \
      _argvec[4] = (unsigned long)arg4;                          \
      _argvec[5] = (unsigned long)arg5;                          \
      _argvec[6] = (unsigned long)arg6;                          \
      _argvec[7] = (unsigned long)arg7;                          \
      _argvec[8] = (unsigned long)arg8;                          \
      _argvec[9] = (unsigned long)arg9;                          \
      _argvec[10] = (unsigned long)arg10;                        \
      _argvec[11] = (unsigned long)arg11;                        \
      __asm__ volatile(                                          \
         VALGRIND_CFI_PROLOGUE                                   \
         "\x61\x67\x68\x69\x20\x31\x35\x2c\x2d\x32\x30\x38\xa\x9"                                      \
         "\x6c\x67\x20\x32\x2c\x20\x38\x28\x31\x29\xa\x9"                                        \
         "\x6c\x67\x20\x33\x2c\x31\x36\x28\x31\x29\xa\x9"                                        \
         "\x6c\x67\x20\x34\x2c\x32\x34\x28\x31\x29\xa\x9"                                        \
         "\x6c\x67\x20\x35\x2c\x33\x32\x28\x31\x29\xa\x9"                                        \
         "\x6c\x67\x20\x36\x2c\x34\x30\x28\x31\x29\xa\x9"                                        \
         "\x6d\x76\x63\x20\x31\x36\x30\x28\x38\x2c\x31\x35\x29\x2c\x20\x34\x38\x28\x31\x29\xa\x9"                              \
         "\x6d\x76\x63\x20\x31\x36\x38\x28\x38\x2c\x31\x35\x29\x2c\x20\x35\x36\x28\x31\x29\xa\x9"                              \
         "\x6d\x76\x63\x20\x31\x37\x36\x28\x38\x2c\x31\x35\x29\x2c\x20\x36\x34\x28\x31\x29\xa\x9"                              \
         "\x6d\x76\x63\x20\x31\x38\x34\x28\x38\x2c\x31\x35\x29\x2c\x20\x37\x32\x28\x31\x29\xa\x9"                              \
         "\x6d\x76\x63\x20\x31\x39\x32\x28\x38\x2c\x31\x35\x29\x2c\x20\x38\x30\x28\x31\x29\xa\x9"                              \
         "\x6d\x76\x63\x20\x32\x30\x30\x28\x38\x2c\x31\x35\x29\x2c\x20\x38\x38\x28\x31\x29\xa\x9"                              \
         "\x6c\x67\x20\x31\x2c\x20\x30\x28\x31\x29\xa\x9"                                        \
         VALGRIND_CALL_NOREDIR_R1                                \
         "\x6c\x67\x72\x20\x25\x30\x2c\x20\x32\xa\x9"                                         \
         "\x61\x67\x68\x69\x20\x31\x35\x2c\x32\x30\x38\xa\x9"                                       \
         VALGRIND_CFI_EPILOGUE                                   \
         : /*out*/   "\x3d\x64" (_res)                                 \
         : /*in*/    "\x61" (&_argvec[0]) __FRAME_POINTER           \
         : /*trash*/ "\x63\x63", "\x6d\x65\x6d\x6f\x72\x79", __CALLER_SAVED_REGS,"\x36","\x37" \
      );                                                         \
      lval = (__typeof__(lval)) _res;                            \
   } while (0)

#define CALL_FN_W_12W(lval, orig, arg1, arg2, arg3, arg4, arg5,  \
                     arg6, arg7 ,arg8, arg9, arg10, arg11, arg12)\
   do {                                                          \
      volatile OrigFn        _orig = (orig);                     \
      volatile unsigned long _argvec[13];                        \
      volatile unsigned long _res;                               \
      _argvec[0] = (unsigned long)_orig.nraddr;                  \
      _argvec[1] = (unsigned long)arg1;                          \
      _argvec[2] = (unsigned long)arg2;                          \
      _argvec[3] = (unsigned long)arg3;                          \
      _argvec[4] = (unsigned long)arg4;                          \
      _argvec[5] = (unsigned long)arg5;                          \
      _argvec[6] = (unsigned long)arg6;                          \
      _argvec[7] = (unsigned long)arg7;                          \
      _argvec[8] = (unsigned long)arg8;                          \
      _argvec[9] = (unsigned long)arg9;                          \
      _argvec[10] = (unsigned long)arg10;                        \
      _argvec[11] = (unsigned long)arg11;                        \
      _argvec[12] = (unsigned long)arg12;                        \
      __asm__ volatile(                                          \
         VALGRIND_CFI_PROLOGUE                                   \
         "\x61\x67\x68\x69\x20\x31\x35\x2c\x2d\x32\x31\x36\xa\x9"                                      \
         "\x6c\x67\x20\x32\x2c\x20\x38\x28\x31\x29\xa\x9"                                        \
         "\x6c\x67\x20\x33\x2c\x31\x36\x28\x31\x29\xa\x9"                                        \
         "\x6c\x67\x20\x34\x2c\x32\x34\x28\x31\x29\xa\x9"                                        \
         "\x6c\x67\x20\x35\x2c\x33\x32\x28\x31\x29\xa\x9"                                        \
         "\x6c\x67\x20\x36\x2c\x34\x30\x28\x31\x29\xa\x9"                                        \
         "\x6d\x76\x63\x20\x31\x36\x30\x28\x38\x2c\x31\x35\x29\x2c\x20\x34\x38\x28\x31\x29\xa\x9"                              \
         "\x6d\x76\x63\x20\x31\x36\x38\x28\x38\x2c\x31\x35\x29\x2c\x20\x35\x36\x28\x31\x29\xa\x9"                              \
         "\x6d\x76\x63\x20\x31\x37\x36\x28\x38\x2c\x31\x35\x29\x2c\x20\x36\x34\x28\x31\x29\xa\x9"                              \
         "\x6d\x76\x63\x20\x31\x38\x34\x28\x38\x2c\x31\x35\x29\x2c\x20\x37\x32\x28\x31\x29\xa\x9"                              \
         "\x6d\x76\x63\x20\x31\x39\x32\x28\x38\x2c\x31\x35\x29\x2c\x20\x38\x30\x28\x31\x29\xa\x9"                              \
         "\x6d\x76\x63\x20\x32\x30\x30\x28\x38\x2c\x31\x35\x29\x2c\x20\x38\x38\x28\x31\x29\xa\x9"                              \
         "\x6d\x76\x63\x20\x32\x30\x38\x28\x38\x2c\x31\x35\x29\x2c\x20\x39\x36\x28\x31\x29\xa\x9"                              \
         "\x6c\x67\x20\x31\x2c\x20\x30\x28\x31\x29\xa\x9"                                        \
         VALGRIND_CALL_NOREDIR_R1                                \
         "\x6c\x67\x72\x20\x25\x30\x2c\x20\x32\xa\x9"                                         \
         "\x61\x67\x68\x69\x20\x31\x35\x2c\x32\x31\x36\xa\x9"                                       \
         VALGRIND_CFI_EPILOGUE                                   \
         : /*out*/   "\x3d\x64" (_res)                                 \
         : /*in*/    "\x61" (&_argvec[0]) __FRAME_POINTER           \
         : /*trash*/ "\x63\x63", "\x6d\x65\x6d\x6f\x72\x79", __CALLER_SAVED_REGS,"\x36","\x37" \
      );                                                         \
      lval = (__typeof__(lval)) _res;                            \
   } while (0)


#endif /* PLAT_s390x_linux */


/* ------------------------------------------------------------------ */
/* ARCHITECTURE INDEPENDENT MACROS for CLIENT REQUESTS.               */
/*                                                                    */
/* ------------------------------------------------------------------ */

/* Some request codes.  There are many more of these, but most are not
   exposed to end-user view.  These are the public ones, all of the
   form 0x1000 + small_number.

   Core ones are in the range 0x00000000--0x0000ffff.  The non-public
   ones start at 0x2000.
*/

/* These macros are used by tools -- they must be public, but don't
   embed them into other programs. */
#define VG_USERREQ_TOOL_BASE(a,b) \
   ((unsigned int)(((a)&0xff) << 24 | ((b)&0xff) << 16))
#define VG_IS_TOOL_USERREQ(a, b, v) \
   (VG_USERREQ_TOOL_BASE(a,b) == ((v) & 0xffff0000))

/* !! ABIWARNING !! ABIWARNING !! ABIWARNING !! ABIWARNING !!
   This enum comprises an ABI exported by Valgrind to programs
   which use client requests.  DO NOT CHANGE THE ORDER OF THESE
   ENTRIES, NOR DELETE ANY -- add new ones at the end. */
typedef
   enum { VG_USERREQ__RUNNING_ON_VALGRIND  = 0x1001,
          VG_USERREQ__DISCARD_TRANSLATIONS = 0x1002,

          /* These allow any function to be called from the simulated
             CPU but run on the real CPU.  Nb: the first arg passed to
             the function is always the ThreadId of the running
             thread!  So CLIENT_CALL0 actually requires a 1 arg
             function, etc. */
          VG_USERREQ__CLIENT_CALL0 = 0x1101,
          VG_USERREQ__CLIENT_CALL1 = 0x1102,
          VG_USERREQ__CLIENT_CALL2 = 0x1103,
          VG_USERREQ__CLIENT_CALL3 = 0x1104,

          /* Can be useful in regression testing suites -- eg. can
             send Valgrind's output to /dev/null and still count
             errors. */
          VG_USERREQ__COUNT_ERRORS = 0x1201,

          /* Allows a string (gdb monitor command) to be passed to the tool
             Used for interaction with vgdb/gdb */
          VG_USERREQ__GDB_MONITOR_COMMAND = 0x1202,

          /* These are useful and can be interpreted by any tool that
             tracks malloc() et al, by using vg_replace_malloc.c. */
          VG_USERREQ__MALLOCLIKE_BLOCK = 0x1301,
          VG_USERREQ__RESIZEINPLACE_BLOCK = 0x130b,
          VG_USERREQ__FREELIKE_BLOCK   = 0x1302,
          /* Memory pool support. */
          VG_USERREQ__CREATE_MEMPOOL   = 0x1303,
          VG_USERREQ__DESTROY_MEMPOOL  = 0x1304,
          VG_USERREQ__MEMPOOL_ALLOC    = 0x1305,
          VG_USERREQ__MEMPOOL_FREE     = 0x1306,
          VG_USERREQ__MEMPOOL_TRIM     = 0x1307,
          VG_USERREQ__MOVE_MEMPOOL     = 0x1308,
          VG_USERREQ__MEMPOOL_CHANGE   = 0x1309,
          VG_USERREQ__MEMPOOL_EXISTS   = 0x130a,

          /* Allow printfs to valgrind log. */
          /* The first two pass the va_list argument by value, which
             assumes it is the same size as or smaller than a UWord,
             which generally isn't the case.  Hence are deprecated.
             The second two pass the vargs by reference and so are
             immune to this problem. */
          /* both :: char* fmt, va_list vargs (DEPRECATED) */
          VG_USERREQ__PRINTF           = 0x1401,
          VG_USERREQ__PRINTF_BACKTRACE = 0x1402,
          /* both :: char* fmt, va_list* vargs */
          VG_USERREQ__PRINTF_VALIST_BY_REF = 0x1403,
          VG_USERREQ__PRINTF_BACKTRACE_VALIST_BY_REF = 0x1404,

          /* Stack support. */
          VG_USERREQ__STACK_REGISTER   = 0x1501,
          VG_USERREQ__STACK_DEREGISTER = 0x1502,
          VG_USERREQ__STACK_CHANGE     = 0x1503,

          /* Wine support */
          VG_USERREQ__LOAD_PDB_DEBUGINFO = 0x1601,

          /* Querying of debug info. */
          VG_USERREQ__MAP_IP_TO_SRCLOC = 0x1701
   } Vg_ClientRequest;

#if !defined(__GNUC__)
#  define __extension__ /* */
#endif


/* Returns the number of Valgrinds this code is running under.  That
   is, 0 if running natively, 1 if running under Valgrind, 2 if
   running under Valgrind which is running under another Valgrind,
   etc. */
#define RUNNING_ON_VALGRIND                                           \
    (unsigned)VALGRIND_DO_CLIENT_REQUEST_EXPR(0 /* if not */,         \
                                    VG_USERREQ__RUNNING_ON_VALGRIND,  \
                                    0, 0, 0, 0, 0)                    \


/* Discard translation of code in the range [_qzz_addr .. _qzz_addr +
   _qzz_len - 1].  Useful if you are debugging a JITter or some such,
   since it provides a way to make sure valgrind will retranslate the
   invalidated area.  Returns no value. */
#define VALGRIND_DISCARD_TRANSLATIONS(_qzz_addr,_qzz_len)         \
    (unsigned)VALGRIND_DO_CLIENT_REQUEST_EXPR(0,                  \
                               VG_USERREQ__DISCARD_TRANSLATIONS,  \
                               _qzz_addr, _qzz_len, 0, 0, 0)


/* These requests are for getting Valgrind itself to print something.
   Possibly with a backtrace.  This is a really ugly hack.  The return value
   is the number of characters printed, excluding the "**<pid>** " part at the
   start and the backtrace (if present). */

#if defined(__GNUC__) || defined(__INTEL_COMPILER)
/* Modern GCC will optimize the static routine out if unused,
   and unused attribute will shut down warnings about it.  */
static int VALGRIND_PRINTF(const char *format, ...)
   __attribute__((format(__printf__, 1, 2), __unused__));
#endif
static int
#if defined(_MSC_VER)
__inline
#endif
VALGRIND_PRINTF(const char *format, ...)
{
#if defined(NVALGRIND)
   return 0;
#else /* NVALGRIND */
#if defined(_MSC_VER)
   uintptr_t _qzz_res;
#else
   unsigned long _qzz_res;
#endif
   va_list vargs;
   va_start(vargs, format);
#if defined(_MSC_VER)
   _qzz_res = VALGRIND_DO_CLIENT_REQUEST_EXPR(0,
                              VG_USERREQ__PRINTF_VALIST_BY_REF,
                              (uintptr_t)format,
                              (uintptr_t)&vargs,
                              0, 0, 0);
#else
   _qzz_res = VALGRIND_DO_CLIENT_REQUEST_EXPR(0,
                              VG_USERREQ__PRINTF_VALIST_BY_REF,
                              (unsigned long)format,
                              (unsigned long)&vargs,
                              0, 0, 0);
#endif
   va_end(vargs);
   return (int)_qzz_res;
#endif /* NVALGRIND */
}

#if defined(__GNUC__) || defined(__INTEL_COMPILER)
static int VALGRIND_PRINTF_BACKTRACE(const char *format, ...)
   __attribute__((format(__printf__, 1, 2), __unused__));
#endif
static int
#if defined(_MSC_VER)
__inline
#endif
VALGRIND_PRINTF_BACKTRACE(const char *format, ...)
{
#if defined(NVALGRIND)
   return 0;
#else /* NVALGRIND */
#if defined(_MSC_VER)
   uintptr_t _qzz_res;
#else
   unsigned long _qzz_res;
#endif
   va_list vargs;
   va_start(vargs, format);
#if defined(_MSC_VER)
   _qzz_res = VALGRIND_DO_CLIENT_REQUEST_EXPR(0,
                              VG_USERREQ__PRINTF_BACKTRACE_VALIST_BY_REF,
                              (uintptr_t)format,
                              (uintptr_t)&vargs,
                              0, 0, 0);
#else
   _qzz_res = VALGRIND_DO_CLIENT_REQUEST_EXPR(0,
                              VG_USERREQ__PRINTF_BACKTRACE_VALIST_BY_REF,
                              (unsigned long)format,
                              (unsigned long)&vargs,
                              0, 0, 0);
#endif
   va_end(vargs);
   return (int)_qzz_res;
#endif /* NVALGRIND */
}


/* These requests allow control to move from the simulated CPU to the
   real CPU, calling an arbitary function.

   Note that the current ThreadId is inserted as the first argument.
   So this call:

     VALGRIND_NON_SIMD_CALL2(f, arg1, arg2)

   requires f to have this signature:

     Word f(Word tid, Word arg1, Word arg2)

   where "Word" is a word-sized type.

   Note that these client requests are not entirely reliable.  For example,
   if you call a function with them that subsequently calls printf(),
   there's a high chance Valgrind will crash.  Generally, your prospects of
   these working are made higher if the called function does not refer to
   any global variables, and does not refer to any libc or other functions
   (printf et al).  Any kind of entanglement with libc or dynamic linking is
   likely to have a bad outcome, for tricky reasons which we've grappled
   with a lot in the past.
*/
#define VALGRIND_NON_SIMD_CALL0(_qyy_fn)                          \
    VALGRIND_DO_CLIENT_REQUEST_EXPR(0 /* default return */,       \
                                    VG_USERREQ__CLIENT_CALL0,     \
                                    _qyy_fn,                      \
                                    0, 0, 0, 0)

#define VALGRIND_NON_SIMD_CALL1(_qyy_fn, _qyy_arg1)                    \
    VALGRIND_DO_CLIENT_REQUEST_EXPR(0 /* default return */,            \
                                    VG_USERREQ__CLIENT_CALL1,          \
                                    _qyy_fn,                           \
                                    _qyy_arg1, 0, 0, 0)

#define VALGRIND_NON_SIMD_CALL2(_qyy_fn, _qyy_arg1, _qyy_arg2)         \
    VALGRIND_DO_CLIENT_REQUEST_EXPR(0 /* default return */,            \
                                    VG_USERREQ__CLIENT_CALL2,          \
                                    _qyy_fn,                           \
                                    _qyy_arg1, _qyy_arg2, 0, 0)

#define VALGRIND_NON_SIMD_CALL3(_qyy_fn, _qyy_arg1, _qyy_arg2, _qyy_arg3) \
    VALGRIND_DO_CLIENT_REQUEST_EXPR(0 /* default return */,             \
                                    VG_USERREQ__CLIENT_CALL3,           \
                                    _qyy_fn,                            \
                                    _qyy_arg1, _qyy_arg2,               \
                                    _qyy_arg3, 0)


/* Counts the number of errors that have been recorded by a tool.  Nb:
   the tool must record the errors with VG_(maybe_record_error)() or
   VG_(unique_error)() for them to be counted. */
#define VALGRIND_COUNT_ERRORS                                     \
    (unsigned)VALGRIND_DO_CLIENT_REQUEST_EXPR(                    \
                               0 /* default return */,            \
                               VG_USERREQ__COUNT_ERRORS,          \
                               0, 0, 0, 0, 0)

/* Several Valgrind tools (Memcheck, Massif, Helgrind, DRD) rely on knowing
   when heap blocks are allocated in order to give accurate results.  This
   happens automatically for the standard allocator functions such as
   malloc(), calloc(), realloc(), memalign(), new, new[], free(), delete,
   delete[], etc.

   But if your program uses a custom allocator, this doesn't automatically
   happen, and Valgrind will not do as well.  For example, if you allocate
   superblocks with mmap() and then allocates chunks of the superblocks, all
   Valgrind's observations will be at the mmap() level and it won't know that
   the chunks should be considered separate entities.  In Memcheck's case,
   that means you probably won't get heap block overrun detection (because
   there won't be redzones marked as unaddressable) and you definitely won't
   get any leak detection.

   The following client requests allow a custom allocator to be annotated so
   that it can be handled accurately by Valgrind.

   VALGRIND_MALLOCLIKE_BLOCK marks a region of memory as having been allocated
   by a malloc()-like function.  For Memcheck (an illustrative case), this
   does two things:

   - It records that the block has been allocated.  This means any addresses
     within the block mentioned in error messages will be
     identified as belonging to the block.  It also means that if the block
     isn't freed it will be detected by the leak checker.

   - It marks the block as being addressable and undefined (if 'is_zeroed' is
     not set), or addressable and defined (if 'is_zeroed' is set).  This
     controls how accesses to the block by the program are handled.

   'addr' is the start of the usable block (ie. after any
   redzone), 'sizeB' is its size.  'rzB' is the redzone size if the allocator
   can apply redzones -- these are blocks of padding at the start and end of
   each block.  Adding redzones is recommended as it makes it much more likely
   Valgrind will spot block overruns.  `is_zeroed' indicates if the memory is
   zeroed (or filled with another predictable value), as is the case for
   calloc().

   VALGRIND_MALLOCLIKE_BLOCK should be put immediately after the point where a
   heap block -- that will be used by the client program -- is allocated.
   It's best to put it at the outermost level of the allocator if possible;
   for example, if you have a function my_alloc() which calls
   internal_alloc(), and the client request is put inside internal_alloc(),
   stack traces relating to the heap block will contain entries for both
   my_alloc() and internal_alloc(), which is probably not what you want.

   For Memcheck users: if you use VALGRIND_MALLOCLIKE_BLOCK to carve out
   custom blocks from within a heap block, B, that has been allocated with
   malloc/calloc/new/etc, then block B will be *ignored* during leak-checking
   -- the custom blocks will take precedence.

   VALGRIND_FREELIKE_BLOCK is the partner to VALGRIND_MALLOCLIKE_BLOCK.  For
   Memcheck, it does two things:

   - It records that the block has been deallocated.  This assumes that the
     block was annotated as having been allocated via
     VALGRIND_MALLOCLIKE_BLOCK.  Otherwise, an error will be issued.

   - It marks the block as being unaddressable.

   VALGRIND_FREELIKE_BLOCK should be put immediately after the point where a
   heap block is deallocated.

   VALGRIND_RESIZEINPLACE_BLOCK informs a tool about reallocation. For
   Memcheck, it does four things:

   - It records that the size of a block has been changed.  This assumes that
     the block was annotated as having been allocated via
     VALGRIND_MALLOCLIKE_BLOCK.  Otherwise, an error will be issued.

   - If the block shrunk, it marks the freed memory as being unaddressable.

   - If the block grew, it marks the new area as undefined and defines a red
     zone past the end of the new block.

   - The V-bits of the overlap between the old and the new block are preserved.

   VALGRIND_RESIZEINPLACE_BLOCK should be put after allocation of the new block
   and before deallocation of the old block.

   In many cases, these three client requests will not be enough to get your
   allocator working well with Memcheck.  More specifically, if your allocator
   writes to freed blocks in any way then a VALGRIND_MAKE_MEM_UNDEFINED call
   will be necessary to mark the memory as addressable just before the zeroing
   occurs, otherwise you'll get a lot of invalid write errors.  For example,
   you'll need to do this if your allocator recycles freed blocks, but it
   zeroes them before handing them back out (via VALGRIND_MALLOCLIKE_BLOCK).
   Alternatively, if your allocator reuses freed blocks for allocator-internal
   data structures, VALGRIND_MAKE_MEM_UNDEFINED calls will also be necessary.

   Really, what's happening is a blurring of the lines between the client
   program and the allocator... after VALGRIND_FREELIKE_BLOCK is called, the
   memory should be considered unaddressable to the client program, but the
   allocator knows more than the rest of the client program and so may be able
   to safely access it.  Extra client requests are necessary for Valgrind to
   understand the distinction between the allocator and the rest of the
   program.

   Ignored if addr == 0.
*/
#define VALGRIND_MALLOCLIKE_BLOCK(addr, sizeB, rzB, is_zeroed)    \
    VALGRIND_DO_CLIENT_REQUEST_EXPR(0,                            \
                               VG_USERREQ__MALLOCLIKE_BLOCK,      \
                               addr, sizeB, rzB, is_zeroed, 0)

/* See the comment for VALGRIND_MALLOCLIKE_BLOCK for details.
   Ignored if addr == 0.
*/
#define VALGRIND_RESIZEINPLACE_BLOCK(addr, oldSizeB, newSizeB, rzB) \
    VALGRIND_DO_CLIENT_REQUEST_EXPR(0,                            \
                               VG_USERREQ__RESIZEINPLACE_BLOCK,   \
                               addr, oldSizeB, newSizeB, rzB, 0)

/* See the comment for VALGRIND_MALLOCLIKE_BLOCK for details.
   Ignored if addr == 0.
*/
#define VALGRIND_FREELIKE_BLOCK(addr, rzB)                        \
    VALGRIND_DO_CLIENT_REQUEST_EXPR(0,                            \
                               VG_USERREQ__FREELIKE_BLOCK,        \
                               addr, rzB, 0, 0, 0)

/* Create a memory pool. */
#define VALGRIND_CREATE_MEMPOOL(pool, rzB, is_zeroed)             \
    VALGRIND_DO_CLIENT_REQUEST_EXPR(0,                            \
                               VG_USERREQ__CREATE_MEMPOOL,        \
                               pool, rzB, is_zeroed, 0, 0)

/* Destroy a memory pool. */
#define VALGRIND_DESTROY_MEMPOOL(pool)                            \
    VALGRIND_DO_CLIENT_REQUEST_EXPR(0,                            \
                               VG_USERREQ__DESTROY_MEMPOOL,       \
                               pool, 0, 0, 0, 0)

/* Associate a piece of memory with a memory pool. */
#define VALGRIND_MEMPOOL_ALLOC(pool, addr, size)                  \
    VALGRIND_DO_CLIENT_REQUEST_EXPR(0,                            \
                               VG_USERREQ__MEMPOOL_ALLOC,         \
                               pool, addr, size, 0, 0)

/* Disassociate a piece of memory from a memory pool. */
#define VALGRIND_MEMPOOL_FREE(pool, addr)                         \
    VALGRIND_DO_CLIENT_REQUEST_EXPR(0,                            \
                               VG_USERREQ__MEMPOOL_FREE,          \
                               pool, addr, 0, 0, 0)

/* Disassociate any pieces outside a particular range. */
#define VALGRIND_MEMPOOL_TRIM(pool, addr, size)                   \
    VALGRIND_DO_CLIENT_REQUEST_EXPR(0,                            \
                               VG_USERREQ__MEMPOOL_TRIM,          \
                               pool, addr, size, 0, 0)

/* Resize and/or move a piece associated with a memory pool. */
#define VALGRIND_MOVE_MEMPOOL(poolA, poolB)                       \
    VALGRIND_DO_CLIENT_REQUEST_EXPR(0,                            \
                               VG_USERREQ__MOVE_MEMPOOL,          \
                               poolA, poolB, 0, 0, 0)

/* Resize and/or move a piece associated with a memory pool. */
#define VALGRIND_MEMPOOL_CHANGE(pool, addrA, addrB, size)         \
    VALGRIND_DO_CLIENT_REQUEST_EXPR(0,                            \
                               VG_USERREQ__MEMPOOL_CHANGE,        \
                               pool, addrA, addrB, size, 0)

/* Return 1 if a mempool exists, else 0. */
#define VALGRIND_MEMPOOL_EXISTS(pool)                             \
    (unsigned)VALGRIND_DO_CLIENT_REQUEST_EXPR(0,                  \
                               VG_USERREQ__MEMPOOL_EXISTS,        \
                               pool, 0, 0, 0, 0)

/* Mark a piece of memory as being a stack. Returns a stack id. */
#define VALGRIND_STACK_REGISTER(start, end)                       \
    (unsigned)VALGRIND_DO_CLIENT_REQUEST_EXPR(0,                  \
                               VG_USERREQ__STACK_REGISTER,        \
                               start, end, 0, 0, 0)

/* Unmark the piece of memory associated with a stack id as being a
   stack. */
#define VALGRIND_STACK_DEREGISTER(id)                             \
    (unsigned)VALGRIND_DO_CLIENT_REQUEST_EXPR(0,                  \
                               VG_USERREQ__STACK_DEREGISTER,      \
                               id, 0, 0, 0, 0)

/* Change the start and end address of the stack id. */
#define VALGRIND_STACK_CHANGE(id, start, end)                     \
    VALGRIND_DO_CLIENT_REQUEST_EXPR(0,                            \
                               VG_USERREQ__STACK_CHANGE,          \
                               id, start, end, 0, 0)

/* Load PDB debug info for Wine PE image_map. */
#define VALGRIND_LOAD_PDB_DEBUGINFO(fd, ptr, total_size, delta)   \
    VALGRIND_DO_CLIENT_REQUEST_EXPR(0,                            \
                               VG_USERREQ__LOAD_PDB_DEBUGINFO,    \
                               fd, ptr, total_size, delta, 0)

/* Map a code address to a source file name and line number.  buf64
   must point to a 64-byte buffer in the caller's address space.  The
   result will be dumped in there and is guaranteed to be zero
   terminated.  If no info is found, the first byte is set to zero. */
#define VALGRIND_MAP_IP_TO_SRCLOC(addr, buf64)                    \
    (unsigned)VALGRIND_DO_CLIENT_REQUEST_EXPR(0,                  \
                               VG_USERREQ__MAP_IP_TO_SRCLOC,      \
                               addr, buf64, 0, 0, 0)


#undef PLAT_x86_darwin
#undef PLAT_amd64_darwin
#undef PLAT_x86_win32
#undef PLAT_x86_linux
#undef PLAT_amd64_linux
#undef PLAT_ppc32_linux
#undef PLAT_ppc64_linux
#undef PLAT_arm_linux
#undef PLAT_s390x_linux

#endif   /* __VALGRIND_H */
