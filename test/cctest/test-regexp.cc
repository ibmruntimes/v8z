// Copyright 2012 the V8 project authors. All rights reserved.
// Redistribution and use in source and binary forms, with or without
// modification, are permitted provided that the following conditions are
// met:
//
//     * Redistributions of source code must retain the above copyright
//       notice, this list of conditions and the following disclaimer.
//     * Redistributions in binary form must reproduce the above
//       copyright notice, this list of conditions and the following
//       disclaimer in the documentation and/or other materials provided
//       with the distribution.
//     * Neither the name of Google Inc. nor the names of its
//       contributors may be used to endorse or promote products derived
//       from this software without specific prior written permission.
//
// THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
// "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
// LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
// A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
// OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
// SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
// LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
// DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
// THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
// (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
// OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.


#include <stdlib.h>

#include "src/v8.h"

#include "src/ast.h"
#include "src/char-predicates-inl.h"
#include "src/jsregexp.h"
#include "src/ostreams.h"
#include "src/parser.h"
#include "src/regexp-macro-assembler.h"
#include "src/regexp-macro-assembler-irregexp.h"
#include "src/string-stream.h"
#include "src/zone-inl.h"
#ifdef V8_INTERPRETED_REGEXP
#include "src/interpreter-irregexp.h"
#else  // V8_INTERPRETED_REGEXP
#include "src/macro-assembler.h"
#if V8_TARGET_ARCH_ARM
#include "src/arm/assembler-arm.h"  // NOLINT
#include "src/arm/macro-assembler-arm.h"
#include "src/arm/regexp-macro-assembler-arm.h"
#endif
#if V8_TARGET_ARCH_ARM64
#include "src/arm64/assembler-arm64.h"
#include "src/arm64/macro-assembler-arm64.h"
#include "src/arm64/regexp-macro-assembler-arm64.h"
#endif
#if V8_TARGET_ARCH_S390
#include "src/s390/assembler-s390.h"
#include "src/s390/macro-assembler-s390.h"
#include "src/s390/regexp-macro-assembler-s390.h"
#endif
#if V8_TARGET_ARCH_PPC
#include "src/ppc/assembler-ppc.h"
#include "src/ppc/macro-assembler-ppc.h"
#include "src/ppc/regexp-macro-assembler-ppc.h"
#endif
#if V8_TARGET_ARCH_MIPS
#include "src/mips/assembler-mips.h"
#include "src/mips/macro-assembler-mips.h"
#include "src/mips/regexp-macro-assembler-mips.h"
#endif
#if V8_TARGET_ARCH_MIPS64
#include "src/mips64/assembler-mips64.h"
#include "src/mips64/macro-assembler-mips64.h"
#include "src/mips64/regexp-macro-assembler-mips64.h"
#endif
#if V8_TARGET_ARCH_X64
#include "src/x64/assembler-x64.h"
#include "src/x64/macro-assembler-x64.h"
#include "src/x64/regexp-macro-assembler-x64.h"
#endif
#if V8_TARGET_ARCH_IA32
#include "src/ia32/assembler-ia32.h"
#include "src/ia32/macro-assembler-ia32.h"
#include "src/ia32/regexp-macro-assembler-ia32.h"
#endif
#if V8_TARGET_ARCH_X87
#include "src/x87/assembler-x87.h"
#include "src/x87/macro-assembler-x87.h"
#include "src/x87/regexp-macro-assembler-x87.h"
#endif
#endif  // V8_INTERPRETED_REGEXP
#include "test/cctest/cctest.h"

using namespace v8::internal;


static bool CheckParse(const char* input) {
  V8::Initialize(NULL);
  v8::HandleScope scope(CcTest::isolate());
  Zone zone(CcTest::i_isolate());
  FlatStringReader reader(CcTest::i_isolate(), CStrVector(input));
  RegExpCompileData result;
  return v8::internal::RegExpParser::ParseRegExp(
      &reader, false, &result, &zone);
}


static void CheckParseEq(const char* input, const char* expected) {
  V8::Initialize(NULL);
  v8::HandleScope scope(CcTest::isolate());
  Zone zone(CcTest::i_isolate());
  FlatStringReader reader(CcTest::i_isolate(), CStrVector(input));
  RegExpCompileData result;
  CHECK(v8::internal::RegExpParser::ParseRegExp(
      &reader, false, &result, &zone));
  CHECK(result.tree != NULL);
  CHECK(result.error.is_null());
  OStringStream os;
  result.tree->Print(os, &zone);
  CHECK_EQ(expected, os.c_str());
}


static bool CheckSimple(const char* input) {
  V8::Initialize(NULL);
  v8::HandleScope scope(CcTest::isolate());
  Zone zone(CcTest::i_isolate());
  FlatStringReader reader(CcTest::i_isolate(), CStrVector(input));
  RegExpCompileData result;
  CHECK(v8::internal::RegExpParser::ParseRegExp(
      &reader, false, &result, &zone));
  CHECK(result.tree != NULL);
  CHECK(result.error.is_null());
  return result.simple;
}

struct MinMaxPair {
  int min_match;
  int max_match;
};


static MinMaxPair CheckMinMaxMatch(const char* input) {
  V8::Initialize(NULL);
  v8::HandleScope scope(CcTest::isolate());
  Zone zone(CcTest::i_isolate());
  FlatStringReader reader(CcTest::i_isolate(), CStrVector(input));
  RegExpCompileData result;
  CHECK(v8::internal::RegExpParser::ParseRegExp(
      &reader, false, &result, &zone));
  CHECK(result.tree != NULL);
  CHECK(result.error.is_null());
  int min_match = result.tree->min_match();
  int max_match = result.tree->max_match();
  MinMaxPair pair = { min_match, max_match };
  return pair;
}


#define CHECK_PARSE_ERROR(input) CHECK(!CheckParse(input))
#define CHECK_SIMPLE(input, simple) CHECK_EQ(simple, CheckSimple(input));
#define CHECK_MIN_MAX(input, min, max)                                         \
  { MinMaxPair min_max = CheckMinMaxMatch(input);                              \
    CHECK_EQ(min, min_max.min_match);                                          \
    CHECK_EQ(max, min_max.max_match);                                          \
  }

TEST(Parser) {
  V8::Initialize(NULL);

  CHECK_PARSE_ERROR("\x3f");

  CheckParseEq("\x61\x62\x63", "\x27\x61\x62\x63\x27");
  CheckParseEq("", "\x25");
  CheckParseEq("\x61\x62\x63\x7c\x64\x65\x66", "\x28\x7c\x20\x27\x61\x62\x63\x27\x20\x27\x64\x65\x66\x27\x29");
  CheckParseEq("\x61\x62\x63\x7c\x64\x65\x66\x7c\x67\x68\x69", "\x28\x7c\x20\x27\x61\x62\x63\x27\x20\x27\x64\x65\x66\x27\x20\x27\x67\x68\x69\x27\x29");
  CheckParseEq("\x5e\x78\x78\x78\x24", "\x28\x3a\x20\x40\x5e\x69\x20\x27\x78\x78\x78\x27\x20\x40\x24\x69\x29");
  CheckParseEq("\x61\x62\x5c\x62\x5c\x64\x5c\x62\x63\x64", "\x28\x3a\x20\x27\x61\x62\x27\x20\x40\x62\x20\x5b\x30\x2d\x39\x5d\x20\x40\x62\x20\x27\x63\x64\x27\x29");
  CheckParseEq("\x5c\x77\x7c\x5c\x64", "\x28\x7c\x20\x5b\x30\x2d\x39\x20\x41\x2d\x5a\x20\x5f\x20\x61\x2d\x7a\x5d\x20\x5b\x30\x2d\x39\x5d\x29");
  CheckParseEq("\x61\x2a", "\x28\x23\x20\x30\x20\x2d\x20\x67\x20\x27\x61\x27\x29");
  CheckParseEq("\x61\x2a\x3f", "\x28\x23\x20\x30\x20\x2d\x20\x6e\x20\x27\x61\x27\x29");
  CheckParseEq("\x61\x62\x63\x2b", "\x28\x3a\x20\x27\x61\x62\x27\x20\x28\x23\x20\x31\x20\x2d\x20\x67\x20\x27\x63\x27\x29\x29");
  CheckParseEq("\x61\x62\x63\x2b\x3f", "\x28\x3a\x20\x27\x61\x62\x27\x20\x28\x23\x20\x31\x20\x2d\x20\x6e\x20\x27\x63\x27\x29\x29");
  CheckParseEq("\x78\x79\x7a\x3f", "\x28\x3a\x20\x27\x78\x79\x27\x20\x28\x23\x20\x30\x20\x31\x20\x67\x20\x27\x7a\x27\x29\x29");
  CheckParseEq("\x78\x79\x7a\x3f\x3f", "\x28\x3a\x20\x27\x78\x79\x27\x20\x28\x23\x20\x30\x20\x31\x20\x6e\x20\x27\x7a\x27\x29\x29");
  CheckParseEq("\x78\x79\x7a\x7b\x30\x2c\x31\x7d", "\x28\x3a\x20\x27\x78\x79\x27\x20\x28\x23\x20\x30\x20\x31\x20\x67\x20\x27\x7a\x27\x29\x29");
  CheckParseEq("\x78\x79\x7a\x7b\x30\x2c\x31\x7d\x3f", "\x28\x3a\x20\x27\x78\x79\x27\x20\x28\x23\x20\x30\x20\x31\x20\x6e\x20\x27\x7a\x27\x29\x29");
  CheckParseEq("\x78\x79\x7a\x7b\x39\x33\x7d", "\x28\x3a\x20\x27\x78\x79\x27\x20\x28\x23\x20\x39\x33\x20\x39\x33\x20\x67\x20\x27\x7a\x27\x29\x29");
  CheckParseEq("\x78\x79\x7a\x7b\x39\x33\x7d\x3f", "\x28\x3a\x20\x27\x78\x79\x27\x20\x28\x23\x20\x39\x33\x20\x39\x33\x20\x6e\x20\x27\x7a\x27\x29\x29");
  CheckParseEq("\x78\x79\x7a\x7b\x31\x2c\x33\x32\x7d", "\x28\x3a\x20\x27\x78\x79\x27\x20\x28\x23\x20\x31\x20\x33\x32\x20\x67\x20\x27\x7a\x27\x29\x29");
  CheckParseEq("\x78\x79\x7a\x7b\x31\x2c\x33\x32\x7d\x3f", "\x28\x3a\x20\x27\x78\x79\x27\x20\x28\x23\x20\x31\x20\x33\x32\x20\x6e\x20\x27\x7a\x27\x29\x29");
  CheckParseEq("\x78\x79\x7a\x7b\x31\x2c\x7d", "\x28\x3a\x20\x27\x78\x79\x27\x20\x28\x23\x20\x31\x20\x2d\x20\x67\x20\x27\x7a\x27\x29\x29");
  CheckParseEq("\x78\x79\x7a\x7b\x31\x2c\x7d\x3f", "\x28\x3a\x20\x27\x78\x79\x27\x20\x28\x23\x20\x31\x20\x2d\x20\x6e\x20\x27\x7a\x27\x29\x29");
  CheckParseEq("\x61\x5c\x66\x62\x5c\x6e\x63\x5c\x72\x64\x5c\x74\x65\x5c\x76\x66", "\x27\x61\x5c\x78\x30\x63\x62\x5c\x78\x30\x61\x63\x5c\x78\x30\x64\x64\x5c\x78\x30\x39\x65\x5c\x78\x30\x62\x66\x27");
  CheckParseEq("\x61\x5c\x6e\x62\x5c\x62\x63", "\x28\x3a\x20\x27\x61\x5c\x78\x30\x61\x62\x27\x20\x40\x62\x20\x27\x63\x27\x29");
  CheckParseEq("\x28\x3f\x3a\x66\x6f\x6f\x29", "\x27\x66\x6f\x6f\x27");
  CheckParseEq("\x28\x3f\x3a\x20\x66\x6f\x6f\x20\x29", "\x27\x20\x66\x6f\x6f\x20\x27");
  CheckParseEq("\x28\x66\x6f\x6f\x7c\x62\x61\x72\x7c\x62\x61\x7a\x29", "\x28\x5e\x20\x28\x7c\x20\x27\x66\x6f\x6f\x27\x20\x27\x62\x61\x72\x27\x20\x27\x62\x61\x7a\x27\x29\x29");
  CheckParseEq("\x66\x6f\x6f\x7c\x28\x62\x61\x72\x7c\x62\x61\x7a\x29\x7c\x71\x75\x75\x78", "\x28\x7c\x20\x27\x66\x6f\x6f\x27\x20\x28\x5e\x20\x28\x7c\x20\x27\x62\x61\x72\x27\x20\x27\x62\x61\x7a\x27\x29\x29\x20\x27\x71\x75\x75\x78\x27\x29");
  CheckParseEq("\x66\x6f\x6f\x28\x3f\x3d\x62\x61\x72\x29\x62\x61\x7a", "\x28\x3a\x20\x27\x66\x6f\x6f\x27\x20\x28\x2d\x3e\x20\x2b\x20\x27\x62\x61\x72\x27\x29\x20\x27\x62\x61\x7a\x27\x29");
  CheckParseEq("\x66\x6f\x6f\x28\x3f\x21\x62\x61\x72\x29\x62\x61\x7a", "\x28\x3a\x20\x27\x66\x6f\x6f\x27\x20\x28\x2d\x3e\x20\x2d\x20\x27\x62\x61\x72\x27\x29\x20\x27\x62\x61\x7a\x27\x29");
  CheckParseEq("\x28\x29", "\x28\x5e\x20\x25\x29");
  CheckParseEq("\x28\x3f\x3d\x29", "\x28\x2d\x3e\x20\x2b\x20\x25\x29");
  CheckParseEq("\x5b\x5d", "\x5e\x5b\x5c\x78\x30\x30\x2d\x5c\x75\x66\x66\x66\x66\x5d");  // Doesn't compile on windows
  CheckParseEq("\x5b\x5e\x5d", "\x5b\x5c\x78\x30\x30\x2d\x5c\x75\x66\x66\x66\x66\x5d");  // \uffff isn't in codepage 1252
  CheckParseEq("\x5b\x78\x5d", "\x5b\x78\x5d");
  CheckParseEq("\x5b\x78\x79\x7a\x5d", "\x5b\x78\x20\x79\x20\x7a\x5d");
  CheckParseEq("\x5b\x61\x2d\x7a\x41\x2d\x5a\x30\x2d\x39\x5d", "\x5b\x61\x2d\x7a\x20\x41\x2d\x5a\x20\x30\x2d\x39\x5d");
  CheckParseEq("\x5b\x2d\x31\x32\x33\x5d", "\x5b\x2d\x20\x31\x20\x32\x20\x33\x5d");
  CheckParseEq("\x5b\x5e\x31\x32\x33\x5d", "\x5e\x5b\x31\x20\x32\x20\x33\x5d");
  CheckParseEq("\x5d", "\x27\x5d\x27");
  CheckParseEq("\x7d", "\x27\x7d\x27");
  CheckParseEq("\x5b\x61\x2d\x62\x2d\x63\x5d", "\x5b\x61\x2d\x62\x20\x2d\x20\x63\x5d");
  CheckParseEq("\x5b\x5c\x64\x5d", "\x5b\x30\x2d\x39\x5d");
  CheckParseEq("\x5b\x78\x5c\x64\x7a\x5d", "\x5b\x78\x20\x30\x2d\x39\x20\x7a\x5d");
  CheckParseEq("\x5b\x5c\x64\x2d\x7a\x5d", "\x5b\x30\x2d\x39\x20\x2d\x20\x7a\x5d");
  CheckParseEq("\x5b\x5c\x64\x2d\x5c\x64\x5d", "\x5b\x30\x2d\x39\x20\x2d\x20\x30\x2d\x39\x5d");
  CheckParseEq("\x5b\x7a\x2d\x5c\x64\x5d", "\x5b\x7a\x20\x2d\x20\x30\x2d\x39\x5d");
  // Control character outside character class.
  CheckParseEq("\x5c\x63\x6a\x5c\x63\x4a\x5c\x63\x69\x5c\x63\x49\x5c\x63\x6b\x5c\x63\x4b", "\x27\x5c\x78\x30\x61\x5c\x78\x30\x61\x5c\x78\x30\x39\x5c\x78\x30\x39\x5c\x78\x30\x62\x5c\x78\x30\x62\x27");
  CheckParseEq("\x5c\x63\x21", "\x27\x5c\x63\x21\x27");
  CheckParseEq("\x5c\x63\x5f", "\x27\x5c\x63\x5f\x27");
  CheckParseEq("\x5c\x63\x7e", "\x27\x5c\x63\x7e\x27");
  CheckParseEq("\x5c\x63\x31", "\x27\x5c\x63\x31\x27");
  // Control character inside character class.
  CheckParseEq("\x5b\x5c\x63\x21\x5d", "\x5b\x5c\x20\x63\x20\x21\x5d");
  CheckParseEq("\x5b\x5c\x63\x5f\x5d", "\x5b\x5c\x78\x31\x66\x5d");
  CheckParseEq("\x5b\x5c\x63\x7e\x5d", "\x5b\x5c\x20\x63\x20\x7e\x5d");
  CheckParseEq("\x5b\x5c\x63\x61\x5d", "\x5b\x5c\x78\x30\x31\x5d");
  CheckParseEq("\x5b\x5c\x63\x7a\x5d", "\x5b\x5c\x78\x31\x61\x5d");
  CheckParseEq("\x5b\x5c\x63\x41\x5d", "\x5b\x5c\x78\x30\x31\x5d");
  CheckParseEq("\x5b\x5c\x63\x5a\x5d", "\x5b\x5c\x78\x31\x61\x5d");
  CheckParseEq("\x5b\x5c\x63\x31\x5d", "\x5b\x5c\x78\x31\x31\x5d");

  CheckParseEq("\x5b\x61\x5c\x5d\x63\x5d", "\x5b\x61\x20\x5d\x20\x63\x5d");
  CheckParseEq("\x5c\x5b\x5c\x5d\x5c\x7b\x5c\x7d\x5c\x28\x5c\x29\x5c\x25\x5c\x5e\x5c\x23\x5c\x20", "\x27\x5b\x5d\x7b\x7d\x28\x29\x25\x5e\x23\x20\x27");
  CheckParseEq("\x5b\x5c\x5b\x5c\x5d\x5c\x7b\x5c\x7d\x5c\x28\x5c\x29\x5c\x25\x5c\x5e\x5c\x23\x5c\x20\x5d", "\x5b\x5b\x20\x5d\x20\x7b\x20\x7d\x20\x28\x20\x29\x20\x25\x20\x5e\x20\x23\x20\x20\x5d");
  CheckParseEq("\x5c\x30", "\x27\x5c\x78\x30\x30\x27");
  CheckParseEq("\x5c\x38", "\x27\x38\x27");
  CheckParseEq("\x5c\x39", "\x27\x39\x27");
  CheckParseEq("\x5c\x31\x31", "\x27\x5c\x78\x30\x39\x27");
  CheckParseEq("\x5c\x31\x31\x61", "\x27\x5c\x78\x30\x39\x61\x27");
  CheckParseEq("\x5c\x30\x31\x31", "\x27\x5c\x78\x30\x39\x27");
  CheckParseEq("\x5c\x30\x30\x30\x31\x31", "\x27\x5c\x78\x30\x30\x31\x31\x27");
  CheckParseEq("\x5c\x31\x31\x38", "\x27\x5c\x78\x30\x39\x38\x27");
  CheckParseEq("\x5c\x31\x31\x31", "\x27\x49\x27");
  CheckParseEq("\x5c\x31\x31\x31\x31", "\x27\x49\x31\x27");
  CheckParseEq("\x28\x78\x29\x28\x78\x29\x28\x78\x29\x5c\x31", "\x28\x3a\x20\x28\x5e\x20\x27\x78\x27\x29\x20\x28\x5e\x20\x27\x78\x27\x29\x20\x28\x5e\x20\x27\x78\x27\x29\x20\x28\x3c\x2d\x20\x31\x29\x29");
  CheckParseEq("\x28\x78\x29\x28\x78\x29\x28\x78\x29\x5c\x32", "\x28\x3a\x20\x28\x5e\x20\x27\x78\x27\x29\x20\x28\x5e\x20\x27\x78\x27\x29\x20\x28\x5e\x20\x27\x78\x27\x29\x20\x28\x3c\x2d\x20\x32\x29\x29");
  CheckParseEq("\x28\x78\x29\x28\x78\x29\x28\x78\x29\x5c\x33", "\x28\x3a\x20\x28\x5e\x20\x27\x78\x27\x29\x20\x28\x5e\x20\x27\x78\x27\x29\x20\x28\x5e\x20\x27\x78\x27\x29\x20\x28\x3c\x2d\x20\x33\x29\x29");
  CheckParseEq("\x28\x78\x29\x28\x78\x29\x28\x78\x29\x5c\x34", "\x28\x3a\x20\x28\x5e\x20\x27\x78\x27\x29\x20\x28\x5e\x20\x27\x78\x27\x29\x20\x28\x5e\x20\x27\x78\x27\x29\x20\x27\x5c\x78\x30\x34\x27\x29");
  CheckParseEq("\x28\x78\x29\x28\x78\x29\x28\x78\x29\x5c\x31\x2a",
               "\x28\x3a\x20\x28\x5e\x20\x27\x78\x27\x29\x20\x28\x5e\x20\x27\x78\x27\x29\x20\x28\x5e\x20\x27\x78\x27\x29"
               "\x20\x28\x23\x20\x30\x20\x2d\x20\x67\x20\x28\x3c\x2d\x20\x31\x29\x29\x29");
  CheckParseEq("\x28\x78\x29\x28\x78\x29\x28\x78\x29\x5c\x32\x2a",
               "\x28\x3a\x20\x28\x5e\x20\x27\x78\x27\x29\x20\x28\x5e\x20\x27\x78\x27\x29\x20\x28\x5e\x20\x27\x78\x27\x29"
               "\x20\x28\x23\x20\x30\x20\x2d\x20\x67\x20\x28\x3c\x2d\x20\x32\x29\x29\x29");
  CheckParseEq("\x28\x78\x29\x28\x78\x29\x28\x78\x29\x5c\x33\x2a",
               "\x28\x3a\x20\x28\x5e\x20\x27\x78\x27\x29\x20\x28\x5e\x20\x27\x78\x27\x29\x20\x28\x5e\x20\x27\x78\x27\x29"
               "\x20\x28\x23\x20\x30\x20\x2d\x20\x67\x20\x28\x3c\x2d\x20\x33\x29\x29\x29");
  CheckParseEq("\x28\x78\x29\x28\x78\x29\x28\x78\x29\x5c\x34\x2a",
               "\x28\x3a\x20\x28\x5e\x20\x27\x78\x27\x29\x20\x28\x5e\x20\x27\x78\x27\x29\x20\x28\x5e\x20\x27\x78\x27\x29"
               "\x20\x28\x23\x20\x30\x20\x2d\x20\x67\x20\x27\x5c\x78\x30\x34\x27\x29\x29");
  CheckParseEq("\x28\x78\x29\x28\x78\x29\x28\x78\x29\x28\x78\x29\x28\x78\x29\x28\x78\x29\x28\x78\x29\x28\x78\x29\x28\x78\x29\x28\x78\x29\x5c\x31\x30",
               "\x28\x3a\x20\x28\x5e\x20\x27\x78\x27\x29\x20\x28\x5e\x20\x27\x78\x27\x29\x20\x28\x5e\x20\x27\x78\x27\x29\x20\x28\x5e\x20\x27\x78\x27\x29\x20\x28\x5e\x20\x27\x78\x27\x29\x20\x28\x5e\x20\x27\x78\x27\x29"
               "\x20\x28\x5e\x20\x27\x78\x27\x29\x20\x28\x5e\x20\x27\x78\x27\x29\x20\x28\x5e\x20\x27\x78\x27\x29\x20\x28\x5e\x20\x27\x78\x27\x29\x20\x28\x3c\x2d\x20\x31\x30\x29\x29");
  CheckParseEq("\x28\x78\x29\x28\x78\x29\x28\x78\x29\x28\x78\x29\x28\x78\x29\x28\x78\x29\x28\x78\x29\x28\x78\x29\x28\x78\x29\x28\x78\x29\x5c\x31\x31",
               "\x28\x3a\x20\x28\x5e\x20\x27\x78\x27\x29\x20\x28\x5e\x20\x27\x78\x27\x29\x20\x28\x5e\x20\x27\x78\x27\x29\x20\x28\x5e\x20\x27\x78\x27\x29\x20\x28\x5e\x20\x27\x78\x27\x29\x20\x28\x5e\x20\x27\x78\x27\x29"
               "\x20\x28\x5e\x20\x27\x78\x27\x29\x20\x28\x5e\x20\x27\x78\x27\x29\x20\x28\x5e\x20\x27\x78\x27\x29\x20\x28\x5e\x20\x27\x78\x27\x29\x20\x27\x5c\x78\x30\x39\x27\x29");
  CheckParseEq("\x28\x61\x29\x5c\x31", "\x28\x3a\x20\x28\x5e\x20\x27\x61\x27\x29\x20\x28\x3c\x2d\x20\x31\x29\x29");
  CheckParseEq("\x28\x61\x5c\x31\x29", "\x28\x5e\x20\x27\x61\x27\x29");
  CheckParseEq("\x28\x5c\x31\x61\x29", "\x28\x5e\x20\x27\x61\x27\x29");
  CheckParseEq("\x28\x3f\x3d\x61\x29\x3f\x61", "\x27\x61\x27");
  CheckParseEq("\x28\x3f\x3d\x61\x29\x7b\x30\x2c\x31\x30\x7d\x61", "\x27\x61\x27");
  CheckParseEq("\x28\x3f\x3d\x61\x29\x7b\x31\x2c\x31\x30\x7d\x61", "\x28\x3a\x20\x28\x2d\x3e\x20\x2b\x20\x27\x61\x27\x29\x20\x27\x61\x27\x29");
  CheckParseEq("\x28\x3f\x3d\x61\x29\x7b\x39\x2c\x31\x30\x7d\x61", "\x28\x3a\x20\x28\x2d\x3e\x20\x2b\x20\x27\x61\x27\x29\x20\x27\x61\x27\x29");
  CheckParseEq("\x28\x3f\x21\x61\x29\x3f\x61", "\x27\x61\x27");
  CheckParseEq("\x5c\x31\x28\x61\x29", "\x28\x5e\x20\x27\x61\x27\x29");
  CheckParseEq("\x28\x3f\x21\x28\x61\x29\x29\x5c\x31", "\x28\x3a\x20\x28\x2d\x3e\x20\x2d\x20\x28\x5e\x20\x27\x61\x27\x29\x29\x20\x28\x3c\x2d\x20\x31\x29\x29");
  CheckParseEq("\x28\x3f\x21\x5c\x31\x28\x61\x5c\x31\x29\x5c\x31\x29\x5c\x31", "\x28\x3a\x20\x28\x2d\x3e\x20\x2d\x20\x28\x3a\x20\x28\x5e\x20\x27\x61\x27\x29\x20\x28\x3c\x2d\x20\x31\x29\x29\x29\x20\x28\x3c\x2d\x20\x31\x29\x29");
  CheckParseEq("\x5b\x5c\x30\x5d", "\x5b\x5c\x78\x30\x30\x5d");
  CheckParseEq("\x5b\x5c\x31\x31\x5d", "\x5b\x5c\x78\x30\x39\x5d");
  CheckParseEq("\x5b\x5c\x31\x31\x61\x5d", "\x5b\x5c\x78\x30\x39\x20\x61\x5d");
  CheckParseEq("\x5b\x5c\x30\x31\x31\x5d", "\x5b\x5c\x78\x30\x39\x5d");
  CheckParseEq("\x5b\x5c\x30\x30\x30\x31\x31\x5d", "\x5b\x5c\x78\x30\x30\x20\x31\x20\x31\x5d");
  CheckParseEq("\x5b\x5c\x31\x31\x38\x5d", "\x5b\x5c\x78\x30\x39\x20\x38\x5d");
  CheckParseEq("\x5b\x5c\x31\x31\x31\x5d", "\x5b\x49\x5d");
  CheckParseEq("\x5b\x5c\x31\x31\x31\x31\x5d", "\x5b\x49\x20\x31\x5d");
  CheckParseEq("\x5c\x78\x33\x34", "\x27\x34\x27");
  CheckParseEq("\x5c\x78\x36\x30", "\x27\x60\x27");
  CheckParseEq("\x5c\x78\x33\x7a", "\x27\x78\x33\x7a\x27");
  CheckParseEq("\x5c\x63", "\x27\x5c\x63\x27");
  CheckParseEq("\x5c\x75\x30\x30\x33\x34", "\x27\x34\x27");
  CheckParseEq("\x5c\x75\x30\x30\x33\x7a", "\x27\x75\x30\x30\x33\x7a\x27");
  CheckParseEq("\x66\x6f\x6f\x5b\x7a\x5d\x2a", "\x28\x3a\x20\x27\x66\x6f\x6f\x27\x20\x28\x23\x20\x30\x20\x2d\x20\x67\x20\x5b\x7a\x5d\x29\x29");

  CHECK_SIMPLE("", false);
  CHECK_SIMPLE("\x61", true);
  CHECK_SIMPLE("\x61\x7c\x62", false);
  CHECK_SIMPLE("\x61\x5c\x6e", false);
  CHECK_SIMPLE("\x5e\x61", false);
  CHECK_SIMPLE("\x61\x24", false);
  CHECK_SIMPLE("\x61\x5c\x62\x21", false);
  CHECK_SIMPLE("\x61\x5c\x42\x62", false);
  CHECK_SIMPLE("\x61\x2a", false);
  CHECK_SIMPLE("\x61\x2a\x3f", false);
  CHECK_SIMPLE("\x61\x3f", false);
  CHECK_SIMPLE("\x61\x3f\x3f", false);
  CHECK_SIMPLE("\x61\x7b\x30\x2c\x31\x7d\x3f", false);
  CHECK_SIMPLE("\x61\x7b\x31\x2c\x31\x7d\x3f", false);
  CHECK_SIMPLE("\x61\x7b\x31\x2c\x32\x7d\x3f", false);
  CHECK_SIMPLE("\x61\x2b\x3f", false);
  CHECK_SIMPLE("\x28\x61\x29", false);
  CHECK_SIMPLE("\x28\x61\x29\x5c\x31", false);
  CHECK_SIMPLE("\x28\x5c\x31\x61\x29", false);
  CHECK_SIMPLE("\x5c\x31\x28\x61\x29", false);
  CHECK_SIMPLE("\x61\x5c\x73", false);
  CHECK_SIMPLE("\x61\x5c\x53", false);
  CHECK_SIMPLE("\x61\x5c\x64", false);
  CHECK_SIMPLE("\x61\x5c\x44", false);
  CHECK_SIMPLE("\x61\x5c\x77", false);
  CHECK_SIMPLE("\x61\x5c\x57", false);
  CHECK_SIMPLE("\x61\x2e", false);
  CHECK_SIMPLE("\x61\x5c\x71", false);
  CHECK_SIMPLE("\x61\x5b\x61\x5d", false);
  CHECK_SIMPLE("\x61\x5b\x5e\x61\x5d", false);
  CHECK_SIMPLE("\x61\x5b\x61\x2d\x7a\x5d", false);
  CHECK_SIMPLE("\x61\x5b\x5c\x71\x5d", false);
  CHECK_SIMPLE("\x61\x28\x3f\x3a\x62\x29", false);
  CHECK_SIMPLE("\x61\x28\x3f\x3d\x62\x29", false);
  CHECK_SIMPLE("\x61\x28\x3f\x21\x62\x29", false);
  CHECK_SIMPLE("\x5c\x78\x36\x30", false);
  CHECK_SIMPLE("\x5c\x75\x30\x30\x36\x30", false);
  CHECK_SIMPLE("\x5c\x63\x41", false);
  CHECK_SIMPLE("\x5c\x71", false);
  CHECK_SIMPLE("\x5c\x31\x31\x31\x32", false);
  CHECK_SIMPLE("\x5c\x30", false);
  CHECK_SIMPLE("\x28\x61\x29\x5c\x31", false);
  CHECK_SIMPLE("\x28\x3f\x3d\x61\x29\x3f\x61", false);
  CHECK_SIMPLE("\x28\x3f\x21\x61\x29\x3f\x61\x5c\x31", false);
  CHECK_SIMPLE("\x28\x3f\x3a\x28\x3f\x3d\x61\x29\x29\x61\x5c\x31", false);

  CheckParseEq("\x61\x7b\x7d", "\x27\x61\x7b\x7d\x27");
  CheckParseEq("\x61\x7b\x2c\x7d", "\x27\x61\x7b\x2c\x7d\x27");
  CheckParseEq("\x61\x7b", "\x27\x61\x7b\x27");
  CheckParseEq("\x61\x7b\x7a\x7d", "\x27\x61\x7b\x7a\x7d\x27");
  CheckParseEq("\x61\x7b\x31\x7a\x7d", "\x27\x61\x7b\x31\x7a\x7d\x27");
  CheckParseEq("\x61\x7b\x31\x32\x7a\x7d", "\x27\x61\x7b\x31\x32\x7a\x7d\x27");
  CheckParseEq("\x61\x7b\x31\x32\x2c", "\x27\x61\x7b\x31\x32\x2c\x27");
  CheckParseEq("\x61\x7b\x31\x32\x2c\x33\x62", "\x27\x61\x7b\x31\x32\x2c\x33\x62\x27");
  CheckParseEq("\x7b\x7d", "\x27\x7b\x7d\x27");
  CheckParseEq("\x7b\x2c\x7d", "\x27\x7b\x2c\x7d\x27");
  CheckParseEq("\x7b", "\x27\x7b\x27");
  CheckParseEq("\x7b\x7a\x7d", "\x27\x7b\x7a\x7d\x27");
  CheckParseEq("\x7b\x31\x7a\x7d", "\x27\x7b\x31\x7a\x7d\x27");
  CheckParseEq("\x7b\x31\x32\x7a\x7d", "\x27\x7b\x31\x32\x7a\x7d\x27");
  CheckParseEq("\x7b\x31\x32\x2c", "\x27\x7b\x31\x32\x2c\x27");
  CheckParseEq("\x7b\x31\x32\x2c\x33\x62", "\x27\x7b\x31\x32\x2c\x33\x62\x27");

  CHECK_MIN_MAX("\x61", 1, 1);
  CHECK_MIN_MAX("\x61\x62\x63", 3, 3);
  CHECK_MIN_MAX("\x61\x5b\x62\x63\x5d\x64", 3, 3);
  CHECK_MIN_MAX("\x61\x7c\x62\x63", 1, 2);
  CHECK_MIN_MAX("\x61\x62\x7c\x63", 1, 2);
  CHECK_MIN_MAX("\x61\x7c\x7c\x62\x63", 0, 2);
  CHECK_MIN_MAX("\x7c", 0, 0);
  CHECK_MIN_MAX("\x28\x3f\x3a\x61\x62\x29", 2, 2);
  CHECK_MIN_MAX("\x28\x3f\x3a\x61\x62\x7c\x63\x64\x65\x29", 2, 3);
  CHECK_MIN_MAX("\x28\x3f\x3a\x61\x62\x29\x7c\x63\x64\x65", 2, 3);
  CHECK_MIN_MAX("\x28\x61\x62\x29", 2, 2);
  CHECK_MIN_MAX("\x28\x61\x62\x7c\x63\x64\x65\x29", 2, 3);
  CHECK_MIN_MAX("\x28\x61\x62\x29\x5c\x31", 2, 4);
  CHECK_MIN_MAX("\x28\x61\x62\x7c\x63\x64\x65\x29\x5c\x31", 2, 6);
  CHECK_MIN_MAX("\x28\x3f\x3a\x61\x62\x29\x3f", 0, 2);
  CHECK_MIN_MAX("\x28\x3f\x3a\x61\x62\x29\x2a", 0, RegExpTree::kInfinity);
  CHECK_MIN_MAX("\x28\x3f\x3a\x61\x62\x29\x2b", 2, RegExpTree::kInfinity);
  CHECK_MIN_MAX("\x61\x3f", 0, 1);
  CHECK_MIN_MAX("\x61\x2a", 0, RegExpTree::kInfinity);
  CHECK_MIN_MAX("\x61\x2b", 1, RegExpTree::kInfinity);
  CHECK_MIN_MAX("\x61\x3f\x3f", 0, 1);
  CHECK_MIN_MAX("\x61\x2a\x3f", 0, RegExpTree::kInfinity);
  CHECK_MIN_MAX("\x61\x2b\x3f", 1, RegExpTree::kInfinity);
  CHECK_MIN_MAX("\x28\x3f\x3a\x61\x3f\x29\x3f", 0, 1);
  CHECK_MIN_MAX("\x28\x3f\x3a\x61\x2a\x29\x3f", 0, RegExpTree::kInfinity);
  CHECK_MIN_MAX("\x28\x3f\x3a\x61\x2b\x29\x3f", 0, RegExpTree::kInfinity);
  CHECK_MIN_MAX("\x28\x3f\x3a\x61\x3f\x29\x2b", 0, RegExpTree::kInfinity);
  CHECK_MIN_MAX("\x28\x3f\x3a\x61\x2a\x29\x2b", 0, RegExpTree::kInfinity);
  CHECK_MIN_MAX("\x28\x3f\x3a\x61\x2b\x29\x2b", 1, RegExpTree::kInfinity);
  CHECK_MIN_MAX("\x28\x3f\x3a\x61\x3f\x29\x2a", 0, RegExpTree::kInfinity);
  CHECK_MIN_MAX("\x28\x3f\x3a\x61\x2a\x29\x2a", 0, RegExpTree::kInfinity);
  CHECK_MIN_MAX("\x28\x3f\x3a\x61\x2b\x29\x2a", 0, RegExpTree::kInfinity);
  CHECK_MIN_MAX("\x61\x7b\x30\x7d", 0, 0);
  CHECK_MIN_MAX("\x28\x3f\x3a\x61\x2b\x29\x7b\x30\x7d", 0, 0);
  CHECK_MIN_MAX("\x28\x3f\x3a\x61\x2b\x29\x7b\x30\x2c\x30\x7d", 0, 0);
  CHECK_MIN_MAX("\x61\x2a\x62", 1, RegExpTree::kInfinity);
  CHECK_MIN_MAX("\x61\x2b\x62", 2, RegExpTree::kInfinity);
  CHECK_MIN_MAX("\x61\x2a\x62\x7c\x63", 1, RegExpTree::kInfinity);
  CHECK_MIN_MAX("\x61\x2b\x62\x7c\x63", 1, RegExpTree::kInfinity);
  CHECK_MIN_MAX("\x28\x3f\x3a\x61\x7b\x35\x2c\x31\x30\x30\x30\x30\x30\x30\x7d\x29\x7b\x33\x2c\x31\x30\x30\x30\x30\x30\x30\x7d", 15, RegExpTree::kInfinity);
  CHECK_MIN_MAX("\x28\x3f\x3a\x61\x62\x29\x7b\x34\x2c\x37\x7d", 8, 14);
  CHECK_MIN_MAX("\x61\x5c\x62\x63", 2, 2);
  CHECK_MIN_MAX("\x61\x5c\x42\x63", 2, 2);
  CHECK_MIN_MAX("\x61\x5c\x73\x63", 3, 3);
  CHECK_MIN_MAX("\x61\x5c\x53\x63", 3, 3);
  CHECK_MIN_MAX("\x61\x28\x3f\x3d\x62\x29\x63", 2, 2);
  CHECK_MIN_MAX("\x61\x28\x3f\x3d\x62\x62\x62\x7c\x62\x62\x29\x63", 2, 2);
  CHECK_MIN_MAX("\x61\x28\x3f\x21\x62\x62\x62\x7c\x62\x62\x29\x63", 2, 2);
}


TEST(ParserRegression) {
  CheckParseEq("\x5b\x41\x2d\x5a\x24\x2d\x5d\x5b\x78\x5d", "\x28\x21\x20\x5b\x41\x2d\x5a\x20\x24\x20\x2d\x5d\x20\x5b\x78\x5d\x29");
  CheckParseEq("\x61\x7b\x33\x2c\x34\x2a\x7d", "\x28\x3a\x20\x27\x61\x7b\x33\x2c\x27\x20\x28\x23\x20\x30\x20\x2d\x20\x67\x20\x27\x34\x27\x29\x20\x27\x7d\x27\x29");
  CheckParseEq("\x7b", "\x27\x7b\x27");
  CheckParseEq("\x61\x7c", "\x28\x7c\x20\x27\x61\x27\x20\x25\x29");
}

static void ExpectError(const char* input,
                        const char* expected) {
  V8::Initialize(NULL);
  v8::HandleScope scope(CcTest::isolate());
  Zone zone(CcTest::i_isolate());
  FlatStringReader reader(CcTest::i_isolate(), CStrVector(input));
  RegExpCompileData result;
  CHECK(!v8::internal::RegExpParser::ParseRegExp(
      &reader, false, &result, &zone));
  CHECK(result.tree == NULL);
  CHECK(!result.error.is_null());
  SmartArrayPointer<char> str = result.error->ToCString(ALLOW_NULLS);
  CHECK_EQ(expected, str.get());
}


TEST(Errors) {
  const char* kEndBackslash = "\x5c\x20\x61\x74\x20\x65\x6e\x64\x20\x6f\x66\x20\x70\x61\x74\x74\x65\x72\x6e";
  ExpectError("\\", kEndBackslash);
  const char* kUnterminatedGroup = "\x55\x6e\x74\x65\x72\x6d\x69\x6e\x61\x74\x65\x64\x20\x67\x72\x6f\x75\x70";
  ExpectError("\x28\x66\x6f\x6f", kUnterminatedGroup);
  const char* kInvalidGroup = "\x49\x6e\x76\x61\x6c\x69\x64\x20\x67\x72\x6f\x75\x70";
  ExpectError("\x28\x3f", kInvalidGroup);
  const char* kUnterminatedCharacterClass = "\x55\x6e\x74\x65\x72\x6d\x69\x6e\x61\x74\x65\x64\x20\x63\x68\x61\x72\x61\x63\x74\x65\x72\x20\x63\x6c\x61\x73\x73";
  ExpectError("\x5b", kUnterminatedCharacterClass);
  ExpectError("\x5b\x61\x2d", kUnterminatedCharacterClass);
  const char* kNothingToRepeat = "\x4e\x6f\x74\x68\x69\x6e\x67\x20\x74\x6f\x20\x72\x65\x70\x65\x61\x74";
  ExpectError("\x2a", kNothingToRepeat);
  ExpectError("\x3f", kNothingToRepeat);
  ExpectError("\x2b", kNothingToRepeat);
  ExpectError("\x7b\x31\x7d", kNothingToRepeat);
  ExpectError("\x7b\x31\x2c\x32\x7d", kNothingToRepeat);
  ExpectError("\x7b\x31\x2c\x7d", kNothingToRepeat);

  // Check that we don't allow more than kMaxCapture captures
  const int kMaxCaptures = 1 << 16;  // Must match RegExpParser::kMaxCaptures.
  const char* kTooManyCaptures = "\x54\x6f\x6f\x20\x6d\x61\x6e\x79\x20\x63\x61\x70\x74\x75\x72\x65\x73";
  OStringStream os;
  for (int i = 0; i <= kMaxCaptures; i++) {
    os << "\x28\x29";
  }
  ExpectError(os.c_str(), kTooManyCaptures);
}


static bool IsDigit(uc16 c) {
  return ('\x30' <= c && c <= '\x39');
}


static bool NotDigit(uc16 c) {
  return !IsDigit(c);
}


static bool IsWhiteSpaceOrLineTerminator(uc16 c) {
  // According to ECMA 5.1, 15.10.2.12 the CharacterClassEscape \s includes
  // WhiteSpace (7.2) and LineTerminator (7.3) values.
  return v8::internal::WhiteSpaceOrLineTerminator::Is(c);
}


static bool NotWhiteSpaceNorLineTermiantor(uc16 c) {
  return !IsWhiteSpaceOrLineTerminator(c);
}


static bool NotWord(uc16 c) {
  return !IsRegExpWord(c);
}


static void TestCharacterClassEscapes(uc16 c, bool (pred)(uc16 c)) {
  Zone zone(CcTest::i_isolate());
  ZoneList<CharacterRange>* ranges =
      new(&zone) ZoneList<CharacterRange>(2, &zone);
  CharacterRange::AddClassEscape(c, ranges, &zone);
  for (unsigned i = 0; i < (1 << 16); i++) {
    bool in_class = false;
    for (int j = 0; !in_class && j < ranges->length(); j++) {
      CharacterRange& range = ranges->at(j);
      in_class = (range.from() <= i && i <= range.to());
    }
    CHECK_EQ(pred(i), in_class);
  }
}


TEST(CharacterClassEscapes) {
  v8::internal::V8::Initialize(NULL);
  TestCharacterClassEscapes('\x2e', IsRegExpNewline);
  TestCharacterClassEscapes('\x64', IsDigit);
  TestCharacterClassEscapes('\x44', NotDigit);
  TestCharacterClassEscapes('\x73', IsWhiteSpaceOrLineTerminator);
  TestCharacterClassEscapes('\x53', NotWhiteSpaceNorLineTermiantor);
  TestCharacterClassEscapes('\x77', IsRegExpWord);
  TestCharacterClassEscapes('\x57', NotWord);
}


static RegExpNode* Compile(const char* input,
                           bool multiline,
                           bool is_ascii,
                           Zone* zone) {
  V8::Initialize(NULL);
  Isolate* isolate = CcTest::i_isolate();
  FlatStringReader reader(isolate, CStrVector(input));
  RegExpCompileData compile_data;
  if (!v8::internal::RegExpParser::ParseRegExp(&reader, multiline,
                                               &compile_data, zone))
    return NULL;
  Handle<String> pattern = isolate->factory()->
      NewStringFromUtf8(CStrVector(input)).ToHandleChecked();
  Handle<String> sample_subject =
      isolate->factory()->NewStringFromUtf8(CStrVector("")).ToHandleChecked();
  RegExpEngine::Compile(&compile_data,
                        false,
                        false,
                        multiline,
                        pattern,
                        sample_subject,
                        is_ascii,
                        zone);
  return compile_data.node;
}


static void Execute(const char* input,
                    bool multiline,
                    bool is_ascii,
                    bool dot_output = false) {
  v8::HandleScope scope(CcTest::isolate());
  Zone zone(CcTest::i_isolate());
  RegExpNode* node = Compile(input, multiline, is_ascii, &zone);
  USE(node);
#ifdef DEBUG
  if (dot_output) {
    RegExpEngine::DotPrint(input, node, false);
  }
#endif  // DEBUG
}


class TestConfig {
 public:
  typedef int Key;
  typedef int Value;
  static const int kNoKey;
  static int NoValue() { return 0; }
  static inline int Compare(int a, int b) {
    if (a < b)
      return -1;
    else if (a > b)
      return 1;
    else
      return 0;
  }
};


const int TestConfig::kNoKey = 0;


static unsigned PseudoRandom(int i, int j) {
  return ~(~((i * 781) ^ (j * 329)));
}


TEST(SplayTreeSimple) {
  v8::internal::V8::Initialize(NULL);
  static const unsigned kLimit = 1000;
  Zone zone(CcTest::i_isolate());
  ZoneSplayTree<TestConfig> tree(&zone);
  bool seen[kLimit];
  for (unsigned i = 0; i < kLimit; i++) seen[i] = false;
#define CHECK_MAPS_EQUAL() do {                                      \
    for (unsigned k = 0; k < kLimit; k++)                            \
      CHECK_EQ(seen[k], tree.Find(k, &loc));                         \
  } while (false)
  for (int i = 0; i < 50; i++) {
    for (int j = 0; j < 50; j++) {
      unsigned next = PseudoRandom(i, j) % kLimit;
      if (seen[next]) {
        // We've already seen this one.  Check the value and remove
        // it.
        ZoneSplayTree<TestConfig>::Locator loc;
        CHECK(tree.Find(next, &loc));
        CHECK_EQ(next, loc.key());
        CHECK_EQ(3 * next, loc.value());
        tree.Remove(next);
        seen[next] = false;
        CHECK_MAPS_EQUAL();
      } else {
        // Check that it wasn't there already and then add it.
        ZoneSplayTree<TestConfig>::Locator loc;
        CHECK(!tree.Find(next, &loc));
        CHECK(tree.Insert(next, &loc));
        CHECK_EQ(next, loc.key());
        loc.set_value(3 * next);
        seen[next] = true;
        CHECK_MAPS_EQUAL();
      }
      int val = PseudoRandom(j, i) % kLimit;
      if (seen[val]) {
        ZoneSplayTree<TestConfig>::Locator loc;
        CHECK(tree.FindGreatestLessThan(val, &loc));
        CHECK_EQ(loc.key(), val);
        break;
      }
      val = PseudoRandom(i + j, i - j) % kLimit;
      if (seen[val]) {
        ZoneSplayTree<TestConfig>::Locator loc;
        CHECK(tree.FindLeastGreaterThan(val, &loc));
        CHECK_EQ(loc.key(), val);
        break;
      }
    }
  }
}


TEST(DispatchTableConstruction) {
  v8::internal::V8::Initialize(NULL);
  // Initialize test data.
  static const int kLimit = 1000;
  static const int kRangeCount = 8;
  static const int kRangeSize = 16;
  uc16 ranges[kRangeCount][2 * kRangeSize];
  for (int i = 0; i < kRangeCount; i++) {
    Vector<uc16> range(ranges[i], 2 * kRangeSize);
    for (int j = 0; j < 2 * kRangeSize; j++) {
      range[j] = PseudoRandom(i + 25, j + 87) % kLimit;
    }
    range.Sort();
    for (int j = 1; j < 2 * kRangeSize; j++) {
      CHECK(range[j-1] <= range[j]);
    }
  }
  // Enter test data into dispatch table.
  Zone zone(CcTest::i_isolate());
  DispatchTable table(&zone);
  for (int i = 0; i < kRangeCount; i++) {
    uc16* range = ranges[i];
    for (int j = 0; j < 2 * kRangeSize; j += 2)
      table.AddRange(CharacterRange(range[j], range[j + 1]), i, &zone);
  }
  // Check that the table looks as we would expect
  for (int p = 0; p < kLimit; p++) {
    OutSet* outs = table.Get(p);
    for (int j = 0; j < kRangeCount; j++) {
      uc16* range = ranges[j];
      bool is_on = false;
      for (int k = 0; !is_on && (k < 2 * kRangeSize); k += 2)
        is_on = (range[k] <= p && p <= range[k + 1]);
      CHECK_EQ(is_on, outs->Get(j));
    }
  }
}


// Test of debug-only syntax.
#ifdef DEBUG

TEST(ParsePossessiveRepetition) {
  bool old_flag_value = FLAG_regexp_possessive_quantifier;

  // Enable possessive quantifier syntax.
  FLAG_regexp_possessive_quantifier = true;

  CheckParseEq("\x61\x2a\x2b", "\x28\x23\x20\x30\x20\x2d\x20\x70\x20\x27\x61\x27\x29");
  CheckParseEq("\x61\x2b\x2b", "\x28\x23\x20\x31\x20\x2d\x20\x70\x20\x27\x61\x27\x29");
  CheckParseEq("\x61\x3f\x2b", "\x28\x23\x20\x30\x20\x31\x20\x70\x20\x27\x61\x27\x29");
  CheckParseEq("\x61\x7b\x31\x30\x2c\x32\x30\x7d\x2b", "\x28\x23\x20\x31\x30\x20\x32\x30\x20\x70\x20\x27\x61\x27\x29");
  CheckParseEq("\x7a\x61\x7b\x31\x30\x2c\x32\x30\x7d\x2b\x62", "\x28\x3a\x20\x27\x7a\x27\x20\x28\x23\x20\x31\x30\x20\x32\x30\x20\x70\x20\x27\x61\x27\x29\x20\x27\x62\x27\x29");

  // Disable possessive quantifier syntax.
  FLAG_regexp_possessive_quantifier = false;

  CHECK_PARSE_ERROR("\x61\x2a\x2b");
  CHECK_PARSE_ERROR("\x61\x2b\x2b");
  CHECK_PARSE_ERROR("\x61\x3f\x2b");
  CHECK_PARSE_ERROR("\x61\x7b\x31\x30\x2c\x32\x30\x7d\x2b");
  CHECK_PARSE_ERROR("\x61\x7b\x31\x30\x2c\x32\x30\x7d\x2b\x62");

  FLAG_regexp_possessive_quantifier = old_flag_value;
}

#endif

// Tests of interpreter.


#ifndef V8_INTERPRETED_REGEXP

#if V8_TARGET_ARCH_IA32
typedef RegExpMacroAssemblerIA32 ArchRegExpMacroAssembler;
#elif V8_TARGET_ARCH_X64
typedef RegExpMacroAssemblerX64 ArchRegExpMacroAssembler;
#elif V8_TARGET_ARCH_ARM
typedef RegExpMacroAssemblerARM ArchRegExpMacroAssembler;
#elif V8_TARGET_ARCH_ARM64
typedef RegExpMacroAssemblerARM64 ArchRegExpMacroAssembler;
#elif V8_TARGET_ARCH_S390
typedef RegExpMacroAssemblerS390 ArchRegExpMacroAssembler;
#elif V8_TARGET_ARCH_PPC
typedef RegExpMacroAssemblerPPC ArchRegExpMacroAssembler;
#elif V8_TARGET_ARCH_MIPS
typedef RegExpMacroAssemblerMIPS ArchRegExpMacroAssembler;
#elif V8_TARGET_ARCH_MIPS64
typedef RegExpMacroAssemblerMIPS ArchRegExpMacroAssembler;
#elif V8_TARGET_ARCH_X87
typedef RegExpMacroAssemblerX87 ArchRegExpMacroAssembler;
#endif

class ContextInitializer {
 public:
  ContextInitializer()
      : scope_(CcTest::isolate()),
        env_(v8::Context::New(CcTest::isolate())) {
    env_->Enter();
  }
  ~ContextInitializer() {
    env_->Exit();
  }
 private:
  v8::HandleScope scope_;
  v8::Handle<v8::Context> env_;
};


static ArchRegExpMacroAssembler::Result Execute(Code* code,
                                                String* input,
                                                int start_offset,
                                                const byte* input_start,
                                                const byte* input_end,
                                                int* captures) {
  return NativeRegExpMacroAssembler::Execute(
      code,
      input,
      start_offset,
      input_start,
      input_end,
      captures,
      0,
      CcTest::i_isolate());
}


TEST(MacroAssemblerNativeSuccess) {
  v8::V8::Initialize();
  ContextInitializer initializer;
  Isolate* isolate = CcTest::i_isolate();
  Factory* factory = isolate->factory();
  Zone zone(isolate);

  ArchRegExpMacroAssembler m(NativeRegExpMacroAssembler::ASCII, 4, &zone);

  m.Succeed();

  Handle<String> source = factory->NewStringFromStaticAscii("");
  Handle<Object> code_object = m.GetCode(source);
  Handle<Code> code = Handle<Code>::cast(code_object);

  int captures[4] = {42, 37, 87, 117};
  Handle<String> input = factory->NewStringFromStaticAscii("\x66\x6f\x6f\x66\x6f\x6f");
  Handle<SeqOneByteString> seq_input = Handle<SeqOneByteString>::cast(input);
  const byte* start_adr =
      reinterpret_cast<const byte*>(seq_input->GetCharsAddress());

  NativeRegExpMacroAssembler::Result result =
      Execute(*code,
              *input,
              0,
              start_adr,
              start_adr + seq_input->length(),
              captures);

  CHECK_EQ(NativeRegExpMacroAssembler::SUCCESS, result);
  CHECK_EQ(-1, captures[0]);
  CHECK_EQ(-1, captures[1]);
  CHECK_EQ(-1, captures[2]);
  CHECK_EQ(-1, captures[3]);
}


TEST(MacroAssemblerNativeSimple) {
  v8::V8::Initialize();
  ContextInitializer initializer;
  Isolate* isolate = CcTest::i_isolate();
  Factory* factory = isolate->factory();
  Zone zone(isolate);

  ArchRegExpMacroAssembler m(NativeRegExpMacroAssembler::ASCII, 4, &zone);

  Label fail, backtrack;
  m.PushBacktrack(&fail);
  m.CheckNotAtStart(NULL);
  m.LoadCurrentCharacter(2, NULL);
  m.CheckNotCharacter('\x6f', NULL);
  m.LoadCurrentCharacter(1, NULL, false);
  m.CheckNotCharacter('\x6f', NULL);
  m.LoadCurrentCharacter(0, NULL, false);
  m.CheckNotCharacter('\x66', NULL);
  m.WriteCurrentPositionToRegister(0, 0);
  m.WriteCurrentPositionToRegister(1, 3);
  m.AdvanceCurrentPosition(3);
  m.PushBacktrack(&backtrack);
  m.Succeed();
  m.Bind(&backtrack);
  m.Backtrack();
  m.Bind(&fail);
  m.Fail();

  Handle<String> source = factory->NewStringFromStaticAscii("\x5e\x66\x6f\x6f");
  Handle<Object> code_object = m.GetCode(source);
  Handle<Code> code = Handle<Code>::cast(code_object);

  int captures[4] = {42, 37, 87, 117};
  Handle<String> input = factory->NewStringFromStaticAscii("\x66\x6f\x6f\x66\x6f\x6f");
  Handle<SeqOneByteString> seq_input = Handle<SeqOneByteString>::cast(input);
  Address start_adr = seq_input->GetCharsAddress();

  NativeRegExpMacroAssembler::Result result =
      Execute(*code,
              *input,
              0,
              start_adr,
              start_adr + input->length(),
              captures);

  CHECK_EQ(NativeRegExpMacroAssembler::SUCCESS, result);
  CHECK_EQ(0, captures[0]);
  CHECK_EQ(3, captures[1]);
  CHECK_EQ(-1, captures[2]);
  CHECK_EQ(-1, captures[3]);

  input = factory->NewStringFromStaticAscii("\x62\x61\x72\x62\x61\x72\x62\x61\x72");
  seq_input = Handle<SeqOneByteString>::cast(input);
  start_adr = seq_input->GetCharsAddress();

  result = Execute(*code,
                   *input,
                   0,
                   start_adr,
                   start_adr + input->length(),
                   captures);

  CHECK_EQ(NativeRegExpMacroAssembler::FAILURE, result);
}


TEST(MacroAssemblerNativeSimpleUC16) {
  v8::V8::Initialize();
  ContextInitializer initializer;
  Isolate* isolate = CcTest::i_isolate();
  Factory* factory = isolate->factory();
  Zone zone(isolate);

  ArchRegExpMacroAssembler m(NativeRegExpMacroAssembler::UC16, 4, &zone);

  Label fail, backtrack;
  m.PushBacktrack(&fail);
  m.CheckNotAtStart(NULL);
  m.LoadCurrentCharacter(2, NULL);
  m.CheckNotCharacter('\x6f', NULL);
  m.LoadCurrentCharacter(1, NULL, false);
  m.CheckNotCharacter('\x6f', NULL);
  m.LoadCurrentCharacter(0, NULL, false);
  m.CheckNotCharacter('\x66', NULL);
  m.WriteCurrentPositionToRegister(0, 0);
  m.WriteCurrentPositionToRegister(1, 3);
  m.AdvanceCurrentPosition(3);
  m.PushBacktrack(&backtrack);
  m.Succeed();
  m.Bind(&backtrack);
  m.Backtrack();
  m.Bind(&fail);
  m.Fail();

  Handle<String> source = factory->NewStringFromStaticAscii("\x5e\x66\x6f\x6f");
  Handle<Object> code_object = m.GetCode(source);
  Handle<Code> code = Handle<Code>::cast(code_object);

  int captures[4] = {42, 37, 87, 117};
  const uc16 input_data[6] = {'\x66', '\x6f', '\x6f', '\x66', '\x6f',
                              static_cast<uc16>(0x2603)};
  Handle<String> input = factory->NewStringFromTwoByte(
      Vector<const uc16>(input_data, 6)).ToHandleChecked();
  Handle<SeqTwoByteString> seq_input = Handle<SeqTwoByteString>::cast(input);
  Address start_adr = seq_input->GetCharsAddress();

  NativeRegExpMacroAssembler::Result result =
      Execute(*code,
              *input,
              0,
              start_adr,
              start_adr + input->length(),
              captures);

  CHECK_EQ(NativeRegExpMacroAssembler::SUCCESS, result);
  CHECK_EQ(0, captures[0]);
  CHECK_EQ(3, captures[1]);
  CHECK_EQ(-1, captures[2]);
  CHECK_EQ(-1, captures[3]);

  const uc16 input_data2[9] = {'\x62', '\x61', '\x72', '\x62', '\x61', '\x72', '\x62', '\x61',
                               static_cast<uc16>(0x2603)};
  input = factory->NewStringFromTwoByte(
      Vector<const uc16>(input_data2, 9)).ToHandleChecked();
  seq_input = Handle<SeqTwoByteString>::cast(input);
  start_adr = seq_input->GetCharsAddress();

  result = Execute(*code,
                   *input,
                   0,
                   start_adr,
                   start_adr + input->length() * 2,
                   captures);

  CHECK_EQ(NativeRegExpMacroAssembler::FAILURE, result);
}


TEST(MacroAssemblerNativeBacktrack) {
  v8::V8::Initialize();
  ContextInitializer initializer;
  Isolate* isolate = CcTest::i_isolate();
  Factory* factory = isolate->factory();
  Zone zone(isolate);

  ArchRegExpMacroAssembler m(NativeRegExpMacroAssembler::ASCII, 0, &zone);

  Label fail;
  Label backtrack;
  m.LoadCurrentCharacter(10, &fail);
  m.Succeed();
  m.Bind(&fail);
  m.PushBacktrack(&backtrack);
  m.LoadCurrentCharacter(10, NULL);
  m.Succeed();
  m.Bind(&backtrack);
  m.Fail();

  Handle<String> source = factory->NewStringFromStaticAscii("\x2e\x2e\x2e\x2e\x2e\x2e\x2e\x2e\x2e\x2e");
  Handle<Object> code_object = m.GetCode(source);
  Handle<Code> code = Handle<Code>::cast(code_object);

  Handle<String> input = factory->NewStringFromStaticAscii("\x66\x6f\x6f\x66\x6f\x6f");
  Handle<SeqOneByteString> seq_input = Handle<SeqOneByteString>::cast(input);
  Address start_adr = seq_input->GetCharsAddress();

  NativeRegExpMacroAssembler::Result result =
      Execute(*code,
              *input,
              0,
              start_adr,
              start_adr + input->length(),
              NULL);

  CHECK_EQ(NativeRegExpMacroAssembler::FAILURE, result);
}


TEST(MacroAssemblerNativeBackReferenceASCII) {
  v8::V8::Initialize();
  ContextInitializer initializer;
  Isolate* isolate = CcTest::i_isolate();
  Factory* factory = isolate->factory();
  Zone zone(isolate);

  ArchRegExpMacroAssembler m(NativeRegExpMacroAssembler::ASCII, 4, &zone);

  m.WriteCurrentPositionToRegister(0, 0);
  m.AdvanceCurrentPosition(2);
  m.WriteCurrentPositionToRegister(1, 0);
  Label nomatch;
  m.CheckNotBackReference(0, &nomatch);
  m.Fail();
  m.Bind(&nomatch);
  m.AdvanceCurrentPosition(2);
  Label missing_match;
  m.CheckNotBackReference(0, &missing_match);
  m.WriteCurrentPositionToRegister(2, 0);
  m.Succeed();
  m.Bind(&missing_match);
  m.Fail();

  Handle<String> source = factory->NewStringFromStaticAscii("\x5e\x28\x2e\x2e\x29\x2e\x2e\x5c\x31");
  Handle<Object> code_object = m.GetCode(source);
  Handle<Code> code = Handle<Code>::cast(code_object);

  Handle<String> input = factory->NewStringFromStaticAscii("\x66\x6f\x6f\x6f\x66\x6f");
  Handle<SeqOneByteString> seq_input = Handle<SeqOneByteString>::cast(input);
  Address start_adr = seq_input->GetCharsAddress();

  int output[4];
  NativeRegExpMacroAssembler::Result result =
      Execute(*code,
              *input,
              0,
              start_adr,
              start_adr + input->length(),
              output);

  CHECK_EQ(NativeRegExpMacroAssembler::SUCCESS, result);
  CHECK_EQ(0, output[0]);
  CHECK_EQ(2, output[1]);
  CHECK_EQ(6, output[2]);
  CHECK_EQ(-1, output[3]);
}


TEST(MacroAssemblerNativeBackReferenceUC16) {
  v8::V8::Initialize();
  ContextInitializer initializer;
  Isolate* isolate = CcTest::i_isolate();
  Factory* factory = isolate->factory();
  Zone zone(isolate);

  ArchRegExpMacroAssembler m(NativeRegExpMacroAssembler::UC16, 4, &zone);

  m.WriteCurrentPositionToRegister(0, 0);
  m.AdvanceCurrentPosition(2);
  m.WriteCurrentPositionToRegister(1, 0);
  Label nomatch;
  m.CheckNotBackReference(0, &nomatch);
  m.Fail();
  m.Bind(&nomatch);
  m.AdvanceCurrentPosition(2);
  Label missing_match;
  m.CheckNotBackReference(0, &missing_match);
  m.WriteCurrentPositionToRegister(2, 0);
  m.Succeed();
  m.Bind(&missing_match);
  m.Fail();

  Handle<String> source = factory->NewStringFromStaticAscii("\x5e\x28\x2e\x2e\x29\x2e\x2e\x5c\x31");
  Handle<Object> code_object = m.GetCode(source);
  Handle<Code> code = Handle<Code>::cast(code_object);

  const uc16 input_data[6] = {'\x66', 0x2028, '\x6f', '\x6f', '\x66', 0x2028};
  Handle<String> input = factory->NewStringFromTwoByte(
      Vector<const uc16>(input_data, 6)).ToHandleChecked();
  Handle<SeqTwoByteString> seq_input = Handle<SeqTwoByteString>::cast(input);
  Address start_adr = seq_input->GetCharsAddress();

  int output[4];
  NativeRegExpMacroAssembler::Result result =
      Execute(*code,
              *input,
              0,
              start_adr,
              start_adr + input->length() * 2,
              output);

  CHECK_EQ(NativeRegExpMacroAssembler::SUCCESS, result);
  CHECK_EQ(0, output[0]);
  CHECK_EQ(2, output[1]);
  CHECK_EQ(6, output[2]);
  CHECK_EQ(-1, output[3]);
}



TEST(MacroAssemblernativeAtStart) {
  v8::V8::Initialize();
  ContextInitializer initializer;
  Isolate* isolate = CcTest::i_isolate();
  Factory* factory = isolate->factory();
  Zone zone(isolate);

  ArchRegExpMacroAssembler m(NativeRegExpMacroAssembler::ASCII, 0, &zone);

  Label not_at_start, newline, fail;
  m.CheckNotAtStart(&not_at_start);
  // Check that prevchar = '\n' and current = 'f'.
  m.CheckCharacter('\xa', &newline);
  m.Bind(&fail);
  m.Fail();
  m.Bind(&newline);
  m.LoadCurrentCharacter(0, &fail);
  m.CheckNotCharacter('\x66', &fail);
  m.Succeed();

  m.Bind(&not_at_start);
  // Check that prevchar = 'o' and current = 'b'.
  Label prevo;
  m.CheckCharacter('\x6f', &prevo);
  m.Fail();
  m.Bind(&prevo);
  m.LoadCurrentCharacter(0, &fail);
  m.CheckNotCharacter('\x62', &fail);
  m.Succeed();

  Handle<String> source = factory->NewStringFromStaticAscii("\x28\x5e\x66\x7c\x6f\x62\x29");
  Handle<Object> code_object = m.GetCode(source);
  Handle<Code> code = Handle<Code>::cast(code_object);

  Handle<String> input = factory->NewStringFromStaticAscii("\x66\x6f\x6f\x62\x61\x72");
  Handle<SeqOneByteString> seq_input = Handle<SeqOneByteString>::cast(input);
  Address start_adr = seq_input->GetCharsAddress();

  NativeRegExpMacroAssembler::Result result =
      Execute(*code,
              *input,
              0,
              start_adr,
              start_adr + input->length(),
              NULL);

  CHECK_EQ(NativeRegExpMacroAssembler::SUCCESS, result);

  result = Execute(*code,
                   *input,
                   3,
                   start_adr + 3,
                   start_adr + input->length(),
                   NULL);

  CHECK_EQ(NativeRegExpMacroAssembler::SUCCESS, result);
}


TEST(MacroAssemblerNativeBackRefNoCase) {
  v8::V8::Initialize();
  ContextInitializer initializer;
  Isolate* isolate = CcTest::i_isolate();
  Factory* factory = isolate->factory();
  Zone zone(isolate);

  ArchRegExpMacroAssembler m(NativeRegExpMacroAssembler::ASCII, 4, &zone);

  Label fail, succ;

  m.WriteCurrentPositionToRegister(0, 0);
  m.WriteCurrentPositionToRegister(2, 0);
  m.AdvanceCurrentPosition(3);
  m.WriteCurrentPositionToRegister(3, 0);
  m.CheckNotBackReferenceIgnoreCase(2, &fail);  // Match "\x41\x62\x43".
  m.CheckNotBackReferenceIgnoreCase(2, &fail);  // Match "\x41\x42\x43".
  Label expected_fail;
  m.CheckNotBackReferenceIgnoreCase(2, &expected_fail);
  m.Bind(&fail);
  m.Fail();

  m.Bind(&expected_fail);
  m.AdvanceCurrentPosition(3);  // Skip "\x78\x59\x7a"
  m.CheckNotBackReferenceIgnoreCase(2, &succ);
  m.Fail();

  m.Bind(&succ);
  m.WriteCurrentPositionToRegister(1, 0);
  m.Succeed();

  Handle<String> source =
      factory->NewStringFromStaticAscii("\x5e\x28\x61\x62\x63\x29\x5c\x31\x5c\x31\x28\x3f\x21\x5c\x31\x29\x2e\x2e\x2e\x28\x3f\x21\x5c\x31\x29");
  Handle<Object> code_object = m.GetCode(source);
  Handle<Code> code = Handle<Code>::cast(code_object);

  Handle<String> input =
      factory->NewStringFromStaticAscii("\x61\x42\x63\x41\x62\x43\x41\x42\x43\x78\x59\x7a\x61\x62");
  Handle<SeqOneByteString> seq_input = Handle<SeqOneByteString>::cast(input);
  Address start_adr = seq_input->GetCharsAddress();

  int output[4];
  NativeRegExpMacroAssembler::Result result =
      Execute(*code,
              *input,
              0,
              start_adr,
              start_adr + input->length(),
              output);

  CHECK_EQ(NativeRegExpMacroAssembler::SUCCESS, result);
  CHECK_EQ(0, output[0]);
  CHECK_EQ(12, output[1]);
  CHECK_EQ(0, output[2]);
  CHECK_EQ(3, output[3]);
}



TEST(MacroAssemblerNativeRegisters) {
  v8::V8::Initialize();
  ContextInitializer initializer;
  Isolate* isolate = CcTest::i_isolate();
  Factory* factory = isolate->factory();
  Zone zone(isolate);

  ArchRegExpMacroAssembler m(NativeRegExpMacroAssembler::ASCII, 6, &zone);

  uc16 foo_chars[3] = {'\x66', '\x6f', '\x6f'};
  Vector<const uc16> foo(foo_chars, 3);

  enum registers { out1, out2, out3, out4, out5, out6, sp, loop_cnt };
  Label fail;
  Label backtrack;
  m.WriteCurrentPositionToRegister(out1, 0);  // Output: [0]
  m.PushRegister(out1, RegExpMacroAssembler::kNoStackLimitCheck);
  m.PushBacktrack(&backtrack);
  m.WriteStackPointerToRegister(sp);
  // Fill stack and registers
  m.AdvanceCurrentPosition(2);
  m.WriteCurrentPositionToRegister(out1, 0);
  m.PushRegister(out1, RegExpMacroAssembler::kNoStackLimitCheck);
  m.PushBacktrack(&fail);
  // Drop backtrack stack frames.
  m.ReadStackPointerFromRegister(sp);
  // And take the first backtrack (to &backtrack)
  m.Backtrack();

  m.PushCurrentPosition();
  m.AdvanceCurrentPosition(2);
  m.PopCurrentPosition();

  m.Bind(&backtrack);
  m.PopRegister(out1);
  m.ReadCurrentPositionFromRegister(out1);
  m.AdvanceCurrentPosition(3);
  m.WriteCurrentPositionToRegister(out2, 0);  // [0,3]

  Label loop;
  m.SetRegister(loop_cnt, 0);  // loop counter
  m.Bind(&loop);
  m.AdvanceRegister(loop_cnt, 1);
  m.AdvanceCurrentPosition(1);
  m.IfRegisterLT(loop_cnt, 3, &loop);
  m.WriteCurrentPositionToRegister(out3, 0);  // [0,3,6]

  Label loop2;
  m.SetRegister(loop_cnt, 2);  // loop counter
  m.Bind(&loop2);
  m.AdvanceRegister(loop_cnt, -1);
  m.AdvanceCurrentPosition(1);
  m.IfRegisterGE(loop_cnt, 0, &loop2);
  m.WriteCurrentPositionToRegister(out4, 0);  // [0,3,6,9]

  Label loop3;
  Label exit_loop3;
  m.PushRegister(out4, RegExpMacroAssembler::kNoStackLimitCheck);
  m.PushRegister(out4, RegExpMacroAssembler::kNoStackLimitCheck);
  m.ReadCurrentPositionFromRegister(out3);
  m.Bind(&loop3);
  m.AdvanceCurrentPosition(1);
  m.CheckGreedyLoop(&exit_loop3);
  m.GoTo(&loop3);
  m.Bind(&exit_loop3);
  m.PopCurrentPosition();
  m.WriteCurrentPositionToRegister(out5, 0);  // [0,3,6,9,9,-1]

  m.Succeed();

  m.Bind(&fail);
  m.Fail();

  Handle<String> source =
      factory->NewStringFromStaticAscii("\x3c\x6c\x6f\x6f\x70\x20\x74\x65\x73\x74\x3e");
  Handle<Object> code_object = m.GetCode(source);
  Handle<Code> code = Handle<Code>::cast(code_object);

  // String long enough for test (content doesn't matter).
  Handle<String> input =
      factory->NewStringFromStaticAscii("\x66\x6f\x6f\x66\x6f\x6f\x66\x6f\x6f\x66\x6f\x6f\x66\x6f\x6f");
  Handle<SeqOneByteString> seq_input = Handle<SeqOneByteString>::cast(input);
  Address start_adr = seq_input->GetCharsAddress();

  int output[6];
  NativeRegExpMacroAssembler::Result result =
      Execute(*code,
              *input,
              0,
              start_adr,
              start_adr + input->length(),
              output);

  CHECK_EQ(NativeRegExpMacroAssembler::SUCCESS, result);
  CHECK_EQ(0, output[0]);
  CHECK_EQ(3, output[1]);
  CHECK_EQ(6, output[2]);
  CHECK_EQ(9, output[3]);
  CHECK_EQ(9, output[4]);
  CHECK_EQ(-1, output[5]);
}


TEST(MacroAssemblerStackOverflow) {
  v8::V8::Initialize();
  ContextInitializer initializer;
  Isolate* isolate = CcTest::i_isolate();
  Factory* factory = isolate->factory();
  Zone zone(isolate);

  ArchRegExpMacroAssembler m(NativeRegExpMacroAssembler::ASCII, 0, &zone);

  Label loop;
  m.Bind(&loop);
  m.PushBacktrack(&loop);
  m.GoTo(&loop);

  Handle<String> source =
      factory->NewStringFromStaticAscii("\x3c\x73\x74\x61\x63\x6b\x20\x6f\x76\x65\x72\x66\x6c\x6f\x77\x20\x74\x65\x73\x74\x3e");
  Handle<Object> code_object = m.GetCode(source);
  Handle<Code> code = Handle<Code>::cast(code_object);

  // String long enough for test (content doesn't matter).
  Handle<String> input =
      factory->NewStringFromStaticAscii("\x64\x75\x6d\x6d\x79");
  Handle<SeqOneByteString> seq_input = Handle<SeqOneByteString>::cast(input);
  Address start_adr = seq_input->GetCharsAddress();

  NativeRegExpMacroAssembler::Result result =
      Execute(*code,
              *input,
              0,
              start_adr,
              start_adr + input->length(),
              NULL);

  CHECK_EQ(NativeRegExpMacroAssembler::EXCEPTION, result);
  CHECK(isolate->has_pending_exception());
  isolate->clear_pending_exception();
}


TEST(MacroAssemblerNativeLotsOfRegisters) {
  v8::V8::Initialize();
  ContextInitializer initializer;
  Isolate* isolate = CcTest::i_isolate();
  Factory* factory = isolate->factory();
  Zone zone(isolate);

  ArchRegExpMacroAssembler m(NativeRegExpMacroAssembler::ASCII, 2, &zone);

  // At least 2048, to ensure the allocated space for registers
  // span one full page.
  const int large_number = 8000;
  m.WriteCurrentPositionToRegister(large_number, 42);
  m.WriteCurrentPositionToRegister(0, 0);
  m.WriteCurrentPositionToRegister(1, 1);
  Label done;
  m.CheckNotBackReference(0, &done);  // Performs a system-stack push.
  m.Bind(&done);
  m.PushRegister(large_number, RegExpMacroAssembler::kNoStackLimitCheck);
  m.PopRegister(1);
  m.Succeed();

  Handle<String> source =
      factory->NewStringFromStaticAscii("\x3c\x68\x75\x67\x65\x20\x72\x65\x67\x69\x73\x74\x65\x72\x20\x73\x70\x61\x63\x65\x20\x74\x65\x73\x74\x3e");
  Handle<Object> code_object = m.GetCode(source);
  Handle<Code> code = Handle<Code>::cast(code_object);

  // String long enough for test (content doesn't matter).
  Handle<String> input =
      factory->NewStringFromStaticAscii("\x73\x61\x6d\x70\x6c\x65\x20\x74\x65\x78\x74");
  Handle<SeqOneByteString> seq_input = Handle<SeqOneByteString>::cast(input);
  Address start_adr = seq_input->GetCharsAddress();

  int captures[2];
  NativeRegExpMacroAssembler::Result result =
      Execute(*code,
              *input,
              0,
              start_adr,
              start_adr + input->length(),
              captures);

  CHECK_EQ(NativeRegExpMacroAssembler::SUCCESS, result);
  CHECK_EQ(0, captures[0]);
  CHECK_EQ(42, captures[1]);

  isolate->clear_pending_exception();
}

#else  // V8_INTERPRETED_REGEXP

TEST(MacroAssembler) {
  V8::Initialize(NULL);
  byte codes[1024];
  Zone zone(CcTest::i_isolate());
  RegExpMacroAssemblerIrregexp m(Vector<byte>(codes, 1024), &zone);
  // ^f(o)o.
  Label start, fail, backtrack;

  m.SetRegister(4, 42);
  m.PushRegister(4, RegExpMacroAssembler::kNoStackLimitCheck);
  m.AdvanceRegister(4, 42);
  m.GoTo(&start);
  m.Fail();
  m.Bind(&start);
  m.PushBacktrack(&fail);
  m.CheckNotAtStart(NULL);
  m.LoadCurrentCharacter(0, NULL);
  m.CheckNotCharacter('\x66', NULL);
  m.LoadCurrentCharacter(1, NULL);
  m.CheckNotCharacter('\x6f', NULL);
  m.LoadCurrentCharacter(2, NULL);
  m.CheckNotCharacter('\x6f', NULL);
  m.WriteCurrentPositionToRegister(0, 0);
  m.WriteCurrentPositionToRegister(1, 3);
  m.WriteCurrentPositionToRegister(2, 1);
  m.WriteCurrentPositionToRegister(3, 2);
  m.AdvanceCurrentPosition(3);
  m.PushBacktrack(&backtrack);
  m.Succeed();
  m.Bind(&backtrack);
  m.ClearRegisters(2, 3);
  m.Backtrack();
  m.Bind(&fail);
  m.PopRegister(0);
  m.Fail();

  Isolate* isolate = CcTest::i_isolate();
  Factory* factory = isolate->factory();
  HandleScope scope(isolate);

  Handle<String> source = factory->NewStringFromStaticAscii("\x5e\x66\x28\x6f\x29\x6f");
  Handle<ByteArray> array = Handle<ByteArray>::cast(m.GetCode(source));
  int captures[5];

  const uc16 str1[] = {'\x66', '\x6f', '\x6f', '\x62', '\x61', '\x72'};
  Handle<String> f1_16 = factory->NewStringFromTwoByte(
      Vector<const uc16>(str1, 6)).ToHandleChecked();

  CHECK(IrregexpInterpreter::Match(isolate, array, f1_16, captures, 0));
  CHECK_EQ(0, captures[0]);
  CHECK_EQ(3, captures[1]);
  CHECK_EQ(1, captures[2]);
  CHECK_EQ(2, captures[3]);
  CHECK_EQ(84, captures[4]);

  const uc16 str2[] = {'\x62', '\x61', '\x72', '\x66', '\x6f', '\x6f'};
  Handle<String> f2_16 = factory->NewStringFromTwoByte(
      Vector<const uc16>(str2, 6)).ToHandleChecked();

  CHECK(!IrregexpInterpreter::Match(isolate, array, f2_16, captures, 0));
  CHECK_EQ(42, captures[0]);
}

#endif  // V8_INTERPRETED_REGEXP


TEST(AddInverseToTable) {
  v8::internal::V8::Initialize(NULL);
  static const int kLimit = 1000;
  static const int kRangeCount = 16;
  for (int t = 0; t < 10; t++) {
    Zone zone(CcTest::i_isolate());
    ZoneList<CharacterRange>* ranges =
        new(&zone) ZoneList<CharacterRange>(kRangeCount, &zone);
    for (int i = 0; i < kRangeCount; i++) {
      int from = PseudoRandom(t + 87, i + 25) % kLimit;
      int to = from + (PseudoRandom(i + 87, t + 25) % (kLimit / 20));
      if (to > kLimit) to = kLimit;
      ranges->Add(CharacterRange(from, to), &zone);
    }
    DispatchTable table(&zone);
    DispatchTableConstructor cons(&table, false, &zone);
    cons.set_choice_index(0);
    cons.AddInverse(ranges);
    for (int i = 0; i < kLimit; i++) {
      bool is_on = false;
      for (int j = 0; !is_on && j < kRangeCount; j++)
        is_on = ranges->at(j).Contains(i);
      OutSet* set = table.Get(i);
      CHECK_EQ(is_on, set->Get(0) == false);
    }
  }
  Zone zone(CcTest::i_isolate());
  ZoneList<CharacterRange>* ranges =
      new(&zone) ZoneList<CharacterRange>(1, &zone);
  ranges->Add(CharacterRange(0xFFF0, 0xFFFE), &zone);
  DispatchTable table(&zone);
  DispatchTableConstructor cons(&table, false, &zone);
  cons.set_choice_index(0);
  cons.AddInverse(ranges);
  CHECK(!table.Get(0xFFFE)->Get(0));
  CHECK(table.Get(0xFFFF)->Get(0));
}


static uc32 canonicalize(uc32 c) {
  unibrow::uchar canon[unibrow::Ecma262Canonicalize::kMaxWidth];
  int count = unibrow::Ecma262Canonicalize::Convert(c, '\x0', canon, NULL);
  if (count == 0) {
    return c;
  } else {
    CHECK_EQ(1, count);
    return canon[0];
  }
}


TEST(LatinCanonicalize) {
  unibrow::Mapping<unibrow::Ecma262UnCanonicalize> un_canonicalize;
  for (char lower = '\x61'; lower <= '\x7a'; lower++) {
    char upper = lower + ('\x41' - '\x61');
    CHECK_EQ(canonicalize(lower), canonicalize(upper));
    unibrow::uchar uncanon[unibrow::Ecma262UnCanonicalize::kMaxWidth];
    int length = un_canonicalize.get(lower, '\x0', uncanon);
    CHECK_EQ(2, length);
    CHECK_EQ(upper, uncanon[0]);
    CHECK_EQ(lower, uncanon[1]);
  }
  for (uc32 c = 128; c < (1 << 21); c++)
    CHECK_GE(canonicalize(c), 128);
  unibrow::Mapping<unibrow::ToUppercase> to_upper;
  // Canonicalization is only defined for the Basic Multilingual Plane.
  for (uc32 c = 0; c < (1 << 16); c++) {
    unibrow::uchar upper[unibrow::ToUppercase::kMaxWidth];
    int length = to_upper.get(c, '\x0', upper);
    if (length == 0) {
      length = 1;
      upper[0] = c;
    }
    uc32 u = upper[0];
    if (length > 1 || (c >= 128 && u < 128))
      u = c;
    CHECK_EQ(u, canonicalize(c));
  }
}


static uc32 CanonRangeEnd(uc32 c) {
  unibrow::uchar canon[unibrow::CanonicalizationRange::kMaxWidth];
  int count = unibrow::CanonicalizationRange::Convert(c, '\x0', canon, NULL);
  if (count == 0) {
    return c;
  } else {
    CHECK_EQ(1, count);
    return canon[0];
  }
}


TEST(RangeCanonicalization) {
  // Check that we arrive at the same result when using the basic
  // range canonicalization primitives as when using immediate
  // canonicalization.
  unibrow::Mapping<unibrow::Ecma262UnCanonicalize> un_canonicalize;
  int block_start = 0;
  while (block_start <= 0xFFFF) {
    uc32 block_end = CanonRangeEnd(block_start);
    unsigned block_length = block_end - block_start + 1;
    if (block_length > 1) {
      unibrow::uchar first[unibrow::Ecma262UnCanonicalize::kMaxWidth];
      int first_length = un_canonicalize.get(block_start, '\x0', first);
      for (unsigned i = 1; i < block_length; i++) {
        unibrow::uchar succ[unibrow::Ecma262UnCanonicalize::kMaxWidth];
        int succ_length = un_canonicalize.get(block_start + i, '\x0', succ);
        CHECK_EQ(first_length, succ_length);
        for (int j = 0; j < succ_length; j++) {
          int calc = first[j] + i;
          int found = succ[j];
          CHECK_EQ(calc, found);
        }
      }
    }
    block_start = block_start + block_length;
  }
}


TEST(UncanonicalizeEquivalence) {
  unibrow::Mapping<unibrow::Ecma262UnCanonicalize> un_canonicalize;
  unibrow::uchar chars[unibrow::Ecma262UnCanonicalize::kMaxWidth];
  for (int i = 0; i < (1 << 16); i++) {
    int length = un_canonicalize.get(i, '\x0', chars);
    for (int j = 0; j < length; j++) {
      unibrow::uchar chars2[unibrow::Ecma262UnCanonicalize::kMaxWidth];
      int length2 = un_canonicalize.get(chars[j], '\x0', chars2);
      CHECK_EQ(length, length2);
      for (int k = 0; k < length; k++)
        CHECK_EQ(static_cast<int>(chars[k]), static_cast<int>(chars2[k]));
    }
  }
}


static void TestRangeCaseIndependence(CharacterRange input,
                                      Vector<CharacterRange> expected) {
  Zone zone(CcTest::i_isolate());
  int count = expected.length();
  ZoneList<CharacterRange>* list =
      new(&zone) ZoneList<CharacterRange>(count, &zone);
  input.AddCaseEquivalents(list, false, &zone);
  CHECK_EQ(count, list->length());
  for (int i = 0; i < list->length(); i++) {
    CHECK_EQ(expected[i].from(), list->at(i).from());
    CHECK_EQ(expected[i].to(), list->at(i).to());
  }
}


static void TestSimpleRangeCaseIndependence(CharacterRange input,
                                            CharacterRange expected) {
  EmbeddedVector<CharacterRange, 1> vector;
  vector[0] = expected;
  TestRangeCaseIndependence(input, vector);
}


TEST(CharacterRangeCaseIndependence) {
  v8::internal::V8::Initialize(NULL);
  TestSimpleRangeCaseIndependence(CharacterRange::Singleton('\x61'),
                                  CharacterRange::Singleton('\x41'));
  TestSimpleRangeCaseIndependence(CharacterRange::Singleton('\x7a'),
                                  CharacterRange::Singleton('\x5a'));
  TestSimpleRangeCaseIndependence(CharacterRange('\x61', '\x7a'),
                                  CharacterRange('\x41', '\x5a'));
  TestSimpleRangeCaseIndependence(CharacterRange('\x63', '\x66'),
                                  CharacterRange('\x43', '\x46'));
  TestSimpleRangeCaseIndependence(CharacterRange('\x61', '\x62'),
                                  CharacterRange('\x41', '\x42'));
  TestSimpleRangeCaseIndependence(CharacterRange('\x79', '\x7a'),
                                  CharacterRange('\x59', '\x5a'));
  TestSimpleRangeCaseIndependence(CharacterRange('\x61' - 1, '\x7a' + 1),
                                  CharacterRange('\x41', '\x5a'));
  TestSimpleRangeCaseIndependence(CharacterRange('\x41', '\x5a'),
                                  CharacterRange('\x61', '\x7a'));
  TestSimpleRangeCaseIndependence(CharacterRange('\x43', '\x46'),
                                  CharacterRange('\x63', '\x66'));
  TestSimpleRangeCaseIndependence(CharacterRange('\x41' - 1, '\x5a' + 1),
                                  CharacterRange('\x61', '\x7a'));
  // Here we need to add [l-z] to complete the case independence of
  // [A-Za-z] but we expect [a-z] to be added since we always add a
  // whole block at a time.
  TestSimpleRangeCaseIndependence(CharacterRange('\x41', '\x6b'),
                                  CharacterRange('\x61', '\x7a'));
}


static bool InClass(uc16 c, ZoneList<CharacterRange>* ranges) {
  if (ranges == NULL)
    return false;
  for (int i = 0; i < ranges->length(); i++) {
    CharacterRange range = ranges->at(i);
    if (range.from() <= c && c <= range.to())
      return true;
  }
  return false;
}


TEST(CharClassDifference) {
  v8::internal::V8::Initialize(NULL);
  Zone zone(CcTest::i_isolate());
  ZoneList<CharacterRange>* base =
      new(&zone) ZoneList<CharacterRange>(1, &zone);
  base->Add(CharacterRange::Everything(), &zone);
  Vector<const int> overlay = CharacterRange::GetWordBounds();
  ZoneList<CharacterRange>* included = NULL;
  ZoneList<CharacterRange>* excluded = NULL;
  CharacterRange::Split(base, overlay, &included, &excluded, &zone);
  for (int i = 0; i < (1 << 16); i++) {
    bool in_base = InClass(i, base);
    if (in_base) {
      bool in_overlay = false;
      for (int j = 0; !in_overlay && j < overlay.length(); j += 2) {
        if (overlay[j] <= i && i < overlay[j+1])
          in_overlay = true;
      }
      CHECK_EQ(in_overlay, InClass(i, included));
      CHECK_EQ(!in_overlay, InClass(i, excluded));
    } else {
      CHECK(!InClass(i, included));
      CHECK(!InClass(i, excluded));
    }
  }
}


TEST(CanonicalizeCharacterSets) {
  v8::internal::V8::Initialize(NULL);
  Zone zone(CcTest::i_isolate());
  ZoneList<CharacterRange>* list =
      new(&zone) ZoneList<CharacterRange>(4, &zone);
  CharacterSet set(list);

  list->Add(CharacterRange(10, 20), &zone);
  list->Add(CharacterRange(30, 40), &zone);
  list->Add(CharacterRange(50, 60), &zone);
  set.Canonicalize();
  DCHECK_EQ(3, list->length());
  DCHECK_EQ(10, list->at(0).from());
  DCHECK_EQ(20, list->at(0).to());
  DCHECK_EQ(30, list->at(1).from());
  DCHECK_EQ(40, list->at(1).to());
  DCHECK_EQ(50, list->at(2).from());
  DCHECK_EQ(60, list->at(2).to());

  list->Rewind(0);
  list->Add(CharacterRange(10, 20), &zone);
  list->Add(CharacterRange(50, 60), &zone);
  list->Add(CharacterRange(30, 40), &zone);
  set.Canonicalize();
  DCHECK_EQ(3, list->length());
  DCHECK_EQ(10, list->at(0).from());
  DCHECK_EQ(20, list->at(0).to());
  DCHECK_EQ(30, list->at(1).from());
  DCHECK_EQ(40, list->at(1).to());
  DCHECK_EQ(50, list->at(2).from());
  DCHECK_EQ(60, list->at(2).to());

  list->Rewind(0);
  list->Add(CharacterRange(30, 40), &zone);
  list->Add(CharacterRange(10, 20), &zone);
  list->Add(CharacterRange(25, 25), &zone);
  list->Add(CharacterRange(100, 100), &zone);
  list->Add(CharacterRange(1, 1), &zone);
  set.Canonicalize();
  DCHECK_EQ(5, list->length());
  DCHECK_EQ(1, list->at(0).from());
  DCHECK_EQ(1, list->at(0).to());
  DCHECK_EQ(10, list->at(1).from());
  DCHECK_EQ(20, list->at(1).to());
  DCHECK_EQ(25, list->at(2).from());
  DCHECK_EQ(25, list->at(2).to());
  DCHECK_EQ(30, list->at(3).from());
  DCHECK_EQ(40, list->at(3).to());
  DCHECK_EQ(100, list->at(4).from());
  DCHECK_EQ(100, list->at(4).to());

  list->Rewind(0);
  list->Add(CharacterRange(10, 19), &zone);
  list->Add(CharacterRange(21, 30), &zone);
  list->Add(CharacterRange(20, 20), &zone);
  set.Canonicalize();
  DCHECK_EQ(1, list->length());
  DCHECK_EQ(10, list->at(0).from());
  DCHECK_EQ(30, list->at(0).to());
}


TEST(CharacterRangeMerge) {
  v8::internal::V8::Initialize(NULL);
  Zone zone(CcTest::i_isolate());
  ZoneList<CharacterRange> l1(4, &zone);
  ZoneList<CharacterRange> l2(4, &zone);
  // Create all combinations of intersections of ranges, both singletons and
  // longer.

  int offset = 0;

  // The five kinds of singleton intersections:
  //     X
  //   Y      - outside before
  //    Y     - outside touching start
  //     Y    - overlap
  //      Y   - outside touching end
  //       Y  - outside after

  for (int i = 0; i < 5; i++) {
    l1.Add(CharacterRange::Singleton(offset + 2), &zone);
    l2.Add(CharacterRange::Singleton(offset + i), &zone);
    offset += 6;
  }

  // The seven kinds of singleton/non-singleton intersections:
  //    XXX
  //  Y        - outside before
  //   Y       - outside touching start
  //    Y      - inside touching start
  //     Y     - entirely inside
  //      Y    - inside touching end
  //       Y   - outside touching end
  //        Y  - disjoint after

  for (int i = 0; i < 7; i++) {
    l1.Add(CharacterRange::Range(offset + 2, offset + 4), &zone);
    l2.Add(CharacterRange::Singleton(offset + i), &zone);
    offset += 8;
  }

  // The eleven kinds of non-singleton intersections:
  //
  //       XXXXXXXX
  // YYYY                  - outside before.
  //   YYYY                - outside touching start.
  //     YYYY              - overlapping start
  //       YYYY            - inside touching start
  //         YYYY          - entirely inside
  //           YYYY        - inside touching end
  //             YYYY      - overlapping end
  //               YYYY    - outside touching end
  //                 YYYY  - outside after
  //       YYYYYYYY        - identical
  //     YYYYYYYYYYYY      - containing entirely.

  for (int i = 0; i < 9; i++) {
    l1.Add(CharacterRange::Range(offset + 6, offset + 15), &zone);  // Length 8.
    l2.Add(CharacterRange::Range(offset + 2 * i, offset + 2 * i + 3), &zone);
    offset += 22;
  }
  l1.Add(CharacterRange::Range(offset + 6, offset + 15), &zone);
  l2.Add(CharacterRange::Range(offset + 6, offset + 15), &zone);
  offset += 22;
  l1.Add(CharacterRange::Range(offset + 6, offset + 15), &zone);
  l2.Add(CharacterRange::Range(offset + 4, offset + 17), &zone);
  offset += 22;

  // Different kinds of multi-range overlap:
  // XXXXXXXXXXXXXXXXXXXXXX         XXXXXXXXXXXXXXXXXXXXXX
  //   YYYY  Y  YYYY  Y  YYYY  Y  YYYY  Y  YYYY  Y  YYYY  Y

  l1.Add(CharacterRange::Range(offset, offset + 21), &zone);
  l1.Add(CharacterRange::Range(offset + 31, offset + 52), &zone);
  for (int i = 0; i < 6; i++) {
    l2.Add(CharacterRange::Range(offset + 2, offset + 5), &zone);
    l2.Add(CharacterRange::Singleton(offset + 8), &zone);
    offset += 9;
  }

  DCHECK(CharacterRange::IsCanonical(&l1));
  DCHECK(CharacterRange::IsCanonical(&l2));

  ZoneList<CharacterRange> first_only(4, &zone);
  ZoneList<CharacterRange> second_only(4, &zone);
  ZoneList<CharacterRange> both(4, &zone);
}


TEST(Graph) {
  V8::Initialize(NULL);
  Execute("\x5c\x62\x5c\x77\x2b\x5c\x62", false, true, true);
}
