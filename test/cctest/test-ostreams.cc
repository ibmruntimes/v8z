// Copyright 2014 the V8 project authors. All rights reserved.
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.

#include <string.h>
#include <limits>

#include "include/v8stdint.h"
#include "src/ostreams.h"
#include "test/cctest/cctest.h"

using namespace v8::internal;


TEST(OStringStreamConstructor) {
  OStringStream oss;
  const size_t expected_size = 0;
  CHECK(expected_size == oss.size());
  CHECK_GT(oss.capacity(), 0);
  CHECK_NE(NULL, oss.data());
  CHECK_EQ("", oss.c_str());
}


#define TEST_STRING            \
  "\x41\x73\x68\x20\x6e\x61\x7a\x67\x20\x64\x75\x72\x62\x61\x74\x75\x6c\x75\x6b\x2c\x20"      \
  "\x61\x73\x68\x20\x6e\x61\x7a\x67\x20\x67\x69\x6d\x62\x61\x74\x75\x6c\x2c\x20"        \
  "\x61\x73\x68\x20\x6e\x61\x7a\x67\x20\x74\x68\x72\x61\x6b\x61\x74\x75\x6c\x75\x6b\x2c\x20"     \
  "\x61\x67\x68\x20\x62\x75\x72\x7a\x75\x6d\x2d\x69\x73\x68\x69\x20\x6b\x72\x69\x6d\x70\x61\x74\x75\x6c\x2e"

TEST(OStringStreamGrow) {
  OStringStream oss;
  const int repeat = 30;
  size_t len = strlen(TEST_STRING);
  for (int i = 0; i < repeat; ++i) {
    oss.write(TEST_STRING, len);
  }
  const char* expected =
      TEST_STRING TEST_STRING TEST_STRING TEST_STRING TEST_STRING
      TEST_STRING TEST_STRING TEST_STRING TEST_STRING TEST_STRING
      TEST_STRING TEST_STRING TEST_STRING TEST_STRING TEST_STRING
      TEST_STRING TEST_STRING TEST_STRING TEST_STRING TEST_STRING
      TEST_STRING TEST_STRING TEST_STRING TEST_STRING TEST_STRING
      TEST_STRING TEST_STRING TEST_STRING TEST_STRING TEST_STRING;
  const size_t expected_len = len * repeat;
  CHECK(expected_len == oss.size());
  CHECK_GT(oss.capacity(), 0);
  CHECK_EQ(0, strncmp(expected, oss.data(), expected_len));
  CHECK_EQ(expected, oss.c_str());
}


template <class T>
static void check(const char* expected, T value) {
  OStringStream oss;
  oss << value << "\x20" << hex << value;
  CHECK_EQ(expected, oss.c_str());
}


TEST(NumericFormatting) {
  check<bool>("\x30\x20\x30", false);
  check<bool>("\x31\x20\x31", true);

  check<int16_t>("\x2d\x31\x32\x33\x34\x35\x20\x63\x66\x63\x37", -12345);
  check<int16_t>("\x2d\x33\x32\x37\x36\x38\x20\x38\x30\x30\x30", std::numeric_limits<int16_t>::min());
  check<int16_t>("\x33\x32\x37\x36\x37\x20\x37\x66\x66\x66", std::numeric_limits<int16_t>::max());

  check<uint16_t>("\x33\x34\x35\x36\x37\x20\x38\x37\x30\x37", 34567);
  check<uint16_t>("\x30\x20\x30", std::numeric_limits<uint16_t>::min());
  check<uint16_t>("\x36\x35\x35\x33\x35\x20\x66\x66\x66\x66", std::numeric_limits<uint16_t>::max());

  check<int32_t>("\x2d\x31\x32\x33\x34\x35\x36\x37\x20\x66\x66\x65\x64\x32\x39\x37\x39", -1234567);
  check<int32_t>("\x2d\x32\x31\x34\x37\x34\x38\x33\x36\x34\x38\x20\x38\x30\x30\x30\x30\x30\x30\x30", std::numeric_limits<int32_t>::min());
  check<int32_t>("\x32\x31\x34\x37\x34\x38\x33\x36\x34\x37\x20\x37\x66\x66\x66\x66\x66\x66\x66", std::numeric_limits<int32_t>::max());

  check<uint32_t>("\x33\x34\x35\x36\x37\x38\x39\x20\x33\x34\x62\x66\x31\x35", 3456789);
  check<uint32_t>("\x30\x20\x30", std::numeric_limits<uint32_t>::min());
  check<uint32_t>("\x34\x32\x39\x34\x39\x36\x37\x32\x39\x35\x20\x66\x66\x66\x66\x66\x66\x66\x66", std::numeric_limits<uint32_t>::max());

  check<int64_t>("\x2d\x31\x32\x33\x34\x35\x36\x37\x20\x66\x66\x66\x66\x66\x66\x66\x66\x66\x66\x65\x64\x32\x39\x37\x39", -1234567);
  check<int64_t>("\x2d\x39\x32\x32\x33\x33\x37\x32\x30\x33\x36\x38\x35\x34\x37\x37\x35\x38\x30\x38\x20\x38\x30\x30\x30\x30\x30\x30\x30\x30\x30\x30\x30\x30\x30\x30\x30",
                 std::numeric_limits<int64_t>::min());
  check<int64_t>("\x39\x32\x32\x33\x33\x37\x32\x30\x33\x36\x38\x35\x34\x37\x37\x35\x38\x30\x37\x20\x37\x66\x66\x66\x66\x66\x66\x66\x66\x66\x66\x66\x66\x66\x66\x66",
                 std::numeric_limits<int64_t>::max());

  check<uint64_t>("\x33\x34\x35\x36\x37\x38\x39\x20\x33\x34\x62\x66\x31\x35", 3456789);
  check<uint64_t>("\x30\x20\x30", std::numeric_limits<uint64_t>::min());
  check<uint64_t>("\x31\x38\x34\x34\x36\x37\x34\x34\x30\x37\x33\x37\x30\x39\x35\x35\x31\x36\x31\x35\x20\x66\x66\x66\x66\x66\x66\x66\x66\x66\x66\x66\x66\x66\x66\x66\x66",
                  std::numeric_limits<uint64_t>::max());

  check<float>("\x30\x20\x30", 0.0f);
  check<float>("\x31\x32\x33\x20\x31\x32\x33", 123.0f);
  check<float>("\x2d\x30\x2e\x35\x20\x2d\x30\x2e\x35", -0.5f);
  check<float>("\x31\x2e\x32\x35\x20\x31\x2e\x32\x35", 1.25f);
  check<float>("\x30\x2e\x30\x36\x32\x35\x20\x30\x2e\x30\x36\x32\x35", 6.25e-2f);

  check<double>("\x30\x20\x30", 0.0);
  check<double>("\x31\x32\x33\x20\x31\x32\x33", 123.0);
  check<double>("\x2d\x30\x2e\x35\x20\x2d\x30\x2e\x35", -0.5);
  check<double>("\x31\x2e\x32\x35\x20\x31\x2e\x32\x35", 1.25);
  check<double>("\x30\x2e\x30\x36\x32\x35\x20\x30\x2e\x30\x36\x32\x35", 6.25e-2);
}


#if defined(__GNUC__) && (__GNUC__ == 4) && (__GNUC_MINOR__ <= 4)
// Work around bad optimization by GCC 4.4.6 on PPC Linux
#pragma GCC optimize "O0"
#endif
TEST(CharacterOutput) {
  check<char>("\x61\x20\x61", '\x61');
  check<signed char>("\x42\x20\x42", '\x42');
  check<unsigned char>("\x39\x20\x39", '\x39');
  check<const char*>("\x62\x79\x65\x20\x62\x79\x65", "\x62\x79\x65");

  OStringStream os;
  os.put('\x48').write("\x65\x6c\x6c\x6f", 4);
  CHECK_EQ("\x48\x65\x6c\x6c\x6f", os.c_str());
}
#if defined(__GNUC__) && (__GNUC__ == 4) && (__GNUC_MINOR__ <= 4)
#pragma GCC reset_options
#endif


TEST(Manipulators) {
  OStringStream os;
  os << 123 << hex << 123 << endl << 123 << dec << 123 << 123;
  CHECK_EQ("\x31\x32\x33\x37\x62\xa7\x62\x31\x32\x33\x31\x32\x33", os.c_str());
}


class MiscStuff {
 public:
  MiscStuff(int i, double d, const char* s) : i_(i), d_(d), s_(s) { }

 private:
  friend OStream& operator<<(OStream& os, const MiscStuff& m);

  int i_;
  double d_;
  const char* s_;
};


OStream& operator<<(OStream& os, const MiscStuff& m) {
  return os << "\x7b\x69\x3a" << m.i_ << "\x2c\x20\x64\x3a" << m.d_ << "\x2c\x20\x73\x3a\x27" << m.s_ << "\x27\x7d";
}


TEST(CustomOutput) {
  OStringStream os;
  MiscStuff m(123, 4.5, "\x48\x75\x72\x7a\x21");
  os << m;
  CHECK_EQ("\x7b\x69\x3a\x31\x32\x33\x2c\x20\x64\x3a\x34\x2e\x35\x2c\x20\x73\x3a\x27\x48\x75\x72\x7a\x21\x27\x7d", os.c_str());
}
