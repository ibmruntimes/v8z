// Copyright 2006-2008 the V8 project authors. All rights reserved.
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
#include "test/cctest/cctest.h"

using namespace v8::internal;

// This test must be executed first!
TEST(Default) {
  CHECK(FLAG_testing_bool_flag);
  CHECK_EQ(13, FLAG_testing_int_flag);
  CHECK_EQ(2.5, FLAG_testing_float_flag);
  CHECK_EQ(0, strcmp(FLAG_testing_string_flag, "\x48\x65\x6c\x6c\x6f\x2c\x20\x77\x6f\x72\x6c\x64\x21"));
}


static void SetFlagsToDefault() {
  FlagList::ResetAllFlags();
  TestDefault();
}


TEST(Flags1) {
  FlagList::PrintHelp();
}


TEST(Flags2) {
  SetFlagsToDefault();
  int argc = 8;
  const char* argv[] = { "\x54\x65\x73\x74\x32", "\x2d\x6e\x6f\x74\x65\x73\x74\x69\x6e\x67\x2d\x62\x6f\x6f\x6c\x2d\x66\x6c\x61\x67",
                         "\x2d\x2d\x6e\x6f\x74\x65\x73\x74\x69\x6e\x67\x2d\x6d\x61\x79\x62\x65\x2d\x62\x6f\x6f\x6c\x2d\x66\x6c\x61\x67", "\x6e\x6f\x74\x61\x66\x6c\x61\x67",
                         "\x2d\x2d\x74\x65\x73\x74\x69\x6e\x67\x5f\x69\x6e\x74\x5f\x66\x6c\x61\x67\x3d\x37\x37", "\x2d\x74\x65\x73\x74\x69\x6e\x67\x5f\x66\x6c\x6f\x61\x74\x5f\x66\x6c\x61\x67\x3d\x2e\x32\x35",
                         "\x2d\x2d\x74\x65\x73\x74\x69\x6e\x67\x5f\x73\x74\x72\x69\x6e\x67\x5f\x66\x6c\x61\x67", "\x6e\x6f\x20\x77\x61\x79\x21" };
  CHECK_EQ(0, FlagList::SetFlagsFromCommandLine(&argc,
                                                const_cast<char **>(argv),
                                                false));
  CHECK_EQ(8, argc);
  CHECK(!FLAG_testing_bool_flag);
  CHECK(FLAG_testing_maybe_bool_flag.has_value);
  CHECK(!FLAG_testing_maybe_bool_flag.value);
  CHECK_EQ(77, FLAG_testing_int_flag);
  CHECK_EQ(.25, FLAG_testing_float_flag);
  CHECK_EQ(0, strcmp(FLAG_testing_string_flag, "\x6e\x6f\x20\x77\x61\x79\x21"));
}


TEST(Flags2b) {
  SetFlagsToDefault();
  const char* str =
      "\x20\x2d\x6e\x6f\x74\x65\x73\x74\x69\x6e\x67\x2d\x62\x6f\x6f\x6c\x2d\x66\x6c\x61\x67\x20\x6e\x6f\x74\x61\x66\x6c\x61\x67\x20\x20\x20\x2d\x2d\x74\x65\x73\x74\x69\x6e\x67\x5f\x69\x6e\x74\x5f\x66\x6c\x61\x67\x3d\x37\x37\x20"
      "\x2d\x6e\x6f\x74\x65\x73\x74\x69\x6e\x67\x2d\x6d\x61\x79\x62\x65\x2d\x62\x6f\x6f\x6c\x2d\x66\x6c\x61\x67\x20\x20\x20"
      "\x2d\x74\x65\x73\x74\x69\x6e\x67\x5f\x66\x6c\x6f\x61\x74\x5f\x66\x6c\x61\x67\x3d\x2e\x32\x35\x20\x20"
      "\x2d\x2d\x74\x65\x73\x74\x69\x6e\x67\x5f\x73\x74\x72\x69\x6e\x67\x5f\x66\x6c\x61\x67\x20\x20\x20\x6e\x6f\x5f\x77\x61\x79\x21\x20\x20";
  CHECK_EQ(0, FlagList::SetFlagsFromString(str, StrLength(str)));
  CHECK(!FLAG_testing_bool_flag);
  CHECK(FLAG_testing_maybe_bool_flag.has_value);
  CHECK(!FLAG_testing_maybe_bool_flag.value);
  CHECK_EQ(77, FLAG_testing_int_flag);
  CHECK_EQ(.25, FLAG_testing_float_flag);
  CHECK_EQ(0, strcmp(FLAG_testing_string_flag, "\x6e\x6f\x5f\x77\x61\x79\x21"));
}


TEST(Flags3) {
  SetFlagsToDefault();
  int argc = 9;
  const char* argv[] =
      { "\x54\x65\x73\x74\x33", "\x2d\x2d\x74\x65\x73\x74\x69\x6e\x67\x5f\x62\x6f\x6f\x6c\x5f\x66\x6c\x61\x67", "\x2d\x2d\x74\x65\x73\x74\x69\x6e\x67\x2d\x6d\x61\x79\x62\x65\x2d\x62\x6f\x6f\x6c\x2d\x66\x6c\x61\x67", "\x6e\x6f\x74\x61\x66\x6c\x61\x67",
        "\x2d\x2d\x74\x65\x73\x74\x69\x6e\x67\x5f\x69\x6e\x74\x5f\x66\x6c\x61\x67", "\x2d\x36\x36\x36",
        "\x2d\x2d\x74\x65\x73\x74\x69\x6e\x67\x5f\x66\x6c\x6f\x61\x74\x5f\x66\x6c\x61\x67", "\x2d\x31\x32\x45\x31\x30", "\x2d\x74\x65\x73\x74\x69\x6e\x67\x2d\x73\x74\x72\x69\x6e\x67\x2d\x66\x6c\x61\x67\x3d\x66\x6f\x6f\x2d\x62\x61\x72" };
  CHECK_EQ(0, FlagList::SetFlagsFromCommandLine(&argc,
                                                const_cast<char **>(argv),
                                                true));
  CHECK_EQ(2, argc);
  CHECK(FLAG_testing_bool_flag);
  CHECK(FLAG_testing_maybe_bool_flag.has_value);
  CHECK(FLAG_testing_maybe_bool_flag.value);
  CHECK_EQ(-666, FLAG_testing_int_flag);
  CHECK_EQ(-12E10, FLAG_testing_float_flag);
  CHECK_EQ(0, strcmp(FLAG_testing_string_flag, "\x66\x6f\x6f\x2d\x62\x61\x72"));
}


TEST(Flags3b) {
  SetFlagsToDefault();
  const char* str =
      "\x2d\x2d\x74\x65\x73\x74\x69\x6e\x67\x5f\x62\x6f\x6f\x6c\x5f\x66\x6c\x61\x67\x20\x2d\x2d\x74\x65\x73\x74\x69\x6e\x67\x2d\x6d\x61\x79\x62\x65\x2d\x62\x6f\x6f\x6c\x2d\x66\x6c\x61\x67\x20\x6e\x6f\x74\x61\x66\x6c\x61\x67\x20"
      "\x2d\x2d\x74\x65\x73\x74\x69\x6e\x67\x5f\x69\x6e\x74\x5f\x66\x6c\x61\x67\x20\x2d\x36\x36\x36\x20"
      "\x2d\x2d\x74\x65\x73\x74\x69\x6e\x67\x5f\x66\x6c\x6f\x61\x74\x5f\x66\x6c\x61\x67\x20\x2d\x31\x32\x45\x31\x30\x20"
      "\x2d\x74\x65\x73\x74\x69\x6e\x67\x2d\x73\x74\x72\x69\x6e\x67\x2d\x66\x6c\x61\x67\x3d\x66\x6f\x6f\x2d\x62\x61\x72";
  CHECK_EQ(0, FlagList::SetFlagsFromString(str, StrLength(str)));
  CHECK(FLAG_testing_bool_flag);
  CHECK(FLAG_testing_maybe_bool_flag.has_value);
  CHECK(FLAG_testing_maybe_bool_flag.value);
  CHECK_EQ(-666, FLAG_testing_int_flag);
  CHECK_EQ(-12E10, FLAG_testing_float_flag);
  CHECK_EQ(0, strcmp(FLAG_testing_string_flag, "\x66\x6f\x6f\x2d\x62\x61\x72"));
}


TEST(Flags4) {
  SetFlagsToDefault();
  int argc = 3;
  const char* argv[] = { "\x54\x65\x73\x74\x34", "\x2d\x2d\x74\x65\x73\x74\x69\x6e\x67\x5f\x62\x6f\x6f\x6c\x5f\x66\x6c\x61\x67", "\x2d\x2d\x66\x6f\x6f" };
  CHECK_EQ(0, FlagList::SetFlagsFromCommandLine(&argc,
                                                const_cast<char **>(argv),
                                                true));
  CHECK_EQ(2, argc);
  CHECK(!FLAG_testing_maybe_bool_flag.has_value);
}


TEST(Flags4b) {
  SetFlagsToDefault();
  const char* str = "\x2d\x2d\x74\x65\x73\x74\x69\x6e\x67\x5f\x62\x6f\x6f\x6c\x5f\x66\x6c\x61\x67\x20\x2d\x2d\x66\x6f\x6f";
  CHECK_EQ(2, FlagList::SetFlagsFromString(str, StrLength(str)));
  CHECK(!FLAG_testing_maybe_bool_flag.has_value);
}


TEST(Flags5) {
  SetFlagsToDefault();
  int argc = 2;
  const char* argv[] = { "\x54\x65\x73\x74\x35", "\x2d\x2d\x74\x65\x73\x74\x69\x6e\x67\x5f\x69\x6e\x74\x5f\x66\x6c\x61\x67\x3d\x22\x66\x6f\x6f\x62\x61\x72\x22" };
  CHECK_EQ(1, FlagList::SetFlagsFromCommandLine(&argc,
                                                const_cast<char **>(argv),
                                                true));
  CHECK_EQ(2, argc);
}


TEST(Flags5b) {
  SetFlagsToDefault();
  const char* str = "\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x2d\x2d\x74\x65\x73\x74\x69\x6e\x67\x5f\x69\x6e\x74\x5f\x66\x6c\x61\x67\x3d\x22\x66\x6f\x6f\x62\x61\x72\x22";
  CHECK_EQ(1, FlagList::SetFlagsFromString(str, StrLength(str)));
}


TEST(Flags6) {
  SetFlagsToDefault();
  int argc = 4;
  const char* argv[] = { "\x54\x65\x73\x74\x35", "\x2d\x2d\x74\x65\x73\x74\x69\x6e\x67\x2d\x69\x6e\x74\x2d\x66\x6c\x61\x67", "\x30",
                         "\x2d\x2d\x74\x65\x73\x74\x69\x6e\x67\x5f\x66\x6c\x6f\x61\x74\x5f\x66\x6c\x61\x67" };
  CHECK_EQ(3, FlagList::SetFlagsFromCommandLine(&argc,
                                                const_cast<char **>(argv),
                                                true));
  CHECK_EQ(2, argc);
}


TEST(Flags6b) {
  SetFlagsToDefault();
  const char* str = "\x20\x20\x20\x20\x20\x20\x20\x2d\x2d\x74\x65\x73\x74\x69\x6e\x67\x2d\x69\x6e\x74\x2d\x66\x6c\x61\x67\x20\x30\x20\x20\x20\x20\x20\x20\x2d\x2d\x74\x65\x73\x74\x69\x6e\x67\x5f\x66\x6c\x6f\x61\x74\x5f\x66\x6c\x61\x67\x20\x20\x20\x20";
  CHECK_EQ(3, FlagList::SetFlagsFromString(str, StrLength(str)));
}


TEST(FlagsJSArguments1) {
  SetFlagsToDefault();
  int argc = 6;
  const char* argv[] = {"\x54\x65\x73\x74\x4a\x53\x41\x72\x67\x73\x31",
                        "\x2d\x2d\x74\x65\x73\x74\x69\x6e\x67\x2d\x69\x6e\x74\x2d\x66\x6c\x61\x67", "\x34\x32",
                        "\x2d\x2d", "\x74\x65\x73\x74\x69\x6e\x67\x2d\x66\x6c\x6f\x61\x74\x2d\x66\x6c\x61\x67", "\x37"};
  CHECK_EQ(0, FlagList::SetFlagsFromCommandLine(&argc,
                                                const_cast<char **>(argv),
                                                true));
  CHECK_EQ(42, FLAG_testing_int_flag);
  CHECK_EQ(2.5, FLAG_testing_float_flag);
  CHECK_EQ(2, FLAG_js_arguments.argc);
  CHECK_EQ(0, strcmp(FLAG_js_arguments[0], "\x74\x65\x73\x74\x69\x6e\x67\x2d\x66\x6c\x6f\x61\x74\x2d\x66\x6c\x61\x67"));
  CHECK_EQ(0, strcmp(FLAG_js_arguments[1], "\x37"));
  CHECK_EQ(1, argc);
}


TEST(FlagsJSArguments1b) {
  SetFlagsToDefault();
  const char* str = "\x2d\x2d\x74\x65\x73\x74\x69\x6e\x67\x2d\x69\x6e\x74\x2d\x66\x6c\x61\x67\x20\x34\x32\x20\x2d\x2d\x20\x74\x65\x73\x74\x69\x6e\x67\x2d\x66\x6c\x6f\x61\x74\x2d\x66\x6c\x61\x67\x20\x37";
  CHECK_EQ(0, FlagList::SetFlagsFromString(str, StrLength(str)));
  CHECK_EQ(42, FLAG_testing_int_flag);
  CHECK_EQ(2.5, FLAG_testing_float_flag);
  CHECK_EQ(2, FLAG_js_arguments.argc);
  CHECK_EQ(0, strcmp(FLAG_js_arguments[0], "\x74\x65\x73\x74\x69\x6e\x67\x2d\x66\x6c\x6f\x61\x74\x2d\x66\x6c\x61\x67"));
  CHECK_EQ(0, strcmp(FLAG_js_arguments[1], "\x37"));
}


TEST(FlagsJSArguments2) {
  SetFlagsToDefault();
  const char* str = "\x2d\x2d\x74\x65\x73\x74\x69\x6e\x67\x2d\x69\x6e\x74\x2d\x66\x6c\x61\x67\x20\x34\x32\x20\x2d\x2d\x6a\x73\x2d\x61\x72\x67\x75\x6d\x65\x6e\x74\x73\x20\x74\x65\x73\x74\x69\x6e\x67\x2d\x66\x6c\x6f\x61\x74\x2d\x66\x6c\x61\x67\x20\x37";
  CHECK_EQ(0, FlagList::SetFlagsFromString(str, StrLength(str)));
  CHECK_EQ(42, FLAG_testing_int_flag);
  CHECK_EQ(2.5, FLAG_testing_float_flag);
  CHECK_EQ(2, FLAG_js_arguments.argc);
  CHECK_EQ(0, strcmp(FLAG_js_arguments[0], "\x74\x65\x73\x74\x69\x6e\x67\x2d\x66\x6c\x6f\x61\x74\x2d\x66\x6c\x61\x67"));
  CHECK_EQ(0, strcmp(FLAG_js_arguments[1], "\x37"));
}


TEST(FlagsJSArguments3) {
  SetFlagsToDefault();
  const char* str = "\x2d\x2d\x74\x65\x73\x74\x69\x6e\x67\x2d\x69\x6e\x74\x2d\x66\x6c\x61\x67\x20\x34\x32\x20\x2d\x2d\x6a\x73\x2d\x61\x72\x67\x75\x6d\x65\x6e\x74\x73\x3d\x74\x65\x73\x74\x69\x6e\x67\x2d\x66\x6c\x6f\x61\x74\x2d\x66\x6c\x61\x67\x20\x37";
  CHECK_EQ(0, FlagList::SetFlagsFromString(str, StrLength(str)));
  CHECK_EQ(42, FLAG_testing_int_flag);
  CHECK_EQ(2.5, FLAG_testing_float_flag);
  CHECK_EQ(2, FLAG_js_arguments.argc);
  CHECK_EQ(0, strcmp(FLAG_js_arguments[0], "\x74\x65\x73\x74\x69\x6e\x67\x2d\x66\x6c\x6f\x61\x74\x2d\x66\x6c\x61\x67"));
  CHECK_EQ(0, strcmp(FLAG_js_arguments[1], "\x37"));
}


TEST(FlagsJSArguments4) {
  SetFlagsToDefault();
  const char* str = "\x2d\x2d\x74\x65\x73\x74\x69\x6e\x67\x2d\x69\x6e\x74\x2d\x66\x6c\x61\x67\x20\x34\x32\x20\x2d\x2d";
  CHECK_EQ(0, FlagList::SetFlagsFromString(str, StrLength(str)));
  CHECK_EQ(42, FLAG_testing_int_flag);
  CHECK_EQ(0, FLAG_js_arguments.argc);
}


TEST(FlagsRemoveIncomplete) {
  // Test that processed command line arguments are removed, even
  // if the list of arguments ends unexpectedly.
  SetFlagsToDefault();
  int argc = 3;
  const char* argv[] = { "", "\x2d\x2d\x63\x72\x61\x6e\x6b\x73\x68\x61\x66\x74", "\x2d\x2d\x65\x78\x70\x6f\x73\x65\x2d\x64\x65\x62\x75\x67\x2d\x61\x73" };
  CHECK_EQ(2, FlagList::SetFlagsFromCommandLine(&argc,
                                                const_cast<char **>(argv),
                                                true));
  CHECK_NE(NULL, argv[1]);
  CHECK_EQ(argc, 2);
}
