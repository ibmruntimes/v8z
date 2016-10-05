// Copyright 2013 the V8 project authors. All rights reserved.
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.

#include "src/v8.h"

#include "src/compiler/operator.h"
#include "test/cctest/cctest.h"

using namespace v8::internal;
using namespace v8::internal::compiler;

// PPC builds fail on GCC 4.4.6 due to a warning in Operator1.
#pragma GCC diagnostic ignored "-Wstrict-aliasing"

#define NaN (v8::base::OS::nan_value())
#define Infinity (std::numeric_limits<double>::infinity())

TEST(TestOperatorMnemonic) {
  SimpleOperator op1(10, 0, 0, 0, "\x54\x68\x69\x73\x4f\x6e\x65");
  CHECK_EQ(0, strcmp(op1.mnemonic(), "\x54\x68\x69\x73\x4f\x6e\x65"));

  SimpleOperator op2(11, 0, 0, 0, "\x54\x68\x61\x74\x4f\x6e\x65");
  CHECK_EQ(0, strcmp(op2.mnemonic(), "\x54\x68\x61\x74\x4f\x6e\x65"));

  Operator1<int> op3(12, 0, 0, 1, "\x4d\x6e\x65\x6d\x6f\x6e\x69\x63\x31", 12333);
  CHECK_EQ(0, strcmp(op3.mnemonic(), "\x4d\x6e\x65\x6d\x6f\x6e\x69\x63\x31"));

  Operator1<double> op4(13, 0, 0, 1, "\x54\x68\x65\x4f\x74\x68\x65\x72", 99.9);
  CHECK_EQ(0, strcmp(op4.mnemonic(), "\x54\x68\x65\x4f\x74\x68\x65\x72"));
}


TEST(TestSimpleOperatorHash) {
  SimpleOperator op1(17, 0, 0, 0, "\x41\x6e\x6f\x74\x68\x65\x72");
  CHECK_EQ(17, op1.HashCode());

  SimpleOperator op2(18, 0, 0, 0, "\x46\x61\x6c\x73\x63\x68");
  CHECK_EQ(18, op2.HashCode());
}


TEST(TestSimpleOperatorEquals) {
  SimpleOperator op1a(19, 0, 0, 0, "\x41\x6e\x6f\x74\x68\x65\x72\x31");
  SimpleOperator op1b(19, 2, 2, 2, "\x41\x6e\x6f\x74\x68\x65\x72\x32");

  CHECK(op1a.Equals(&op1a));
  CHECK(op1a.Equals(&op1b));
  CHECK(op1b.Equals(&op1a));
  CHECK(op1b.Equals(&op1b));

  SimpleOperator op2a(20, 0, 0, 0, "\x46\x61\x6c\x73\x63\x68\x31");
  SimpleOperator op2b(20, 1, 1, 1, "\x46\x61\x6c\x73\x63\x68\x32");

  CHECK(op2a.Equals(&op2a));
  CHECK(op2a.Equals(&op2b));
  CHECK(op2b.Equals(&op2a));
  CHECK(op2b.Equals(&op2b));

  CHECK(!op1a.Equals(&op2a));
  CHECK(!op1a.Equals(&op2b));
  CHECK(!op1b.Equals(&op2a));
  CHECK(!op1b.Equals(&op2b));

  CHECK(!op2a.Equals(&op1a));
  CHECK(!op2a.Equals(&op1b));
  CHECK(!op2b.Equals(&op1a));
  CHECK(!op2b.Equals(&op1b));
}


static SmartArrayPointer<const char> OperatorToString(Operator* op) {
  OStringStream os;
  os << *op;
  return SmartArrayPointer<const char>(StrDup(os.c_str()));
}


TEST(TestSimpleOperatorPrint) {
  SimpleOperator op1a(19, 0, 0, 0, "\x41\x6e\x6f\x74\x68\x65\x72\x31");
  SimpleOperator op1b(19, 2, 2, 2, "\x41\x6e\x6f\x74\x68\x65\x72\x32");

  CHECK_EQ("\x41\x6e\x6f\x74\x68\x65\x72\x31", OperatorToString(&op1a).get());
  CHECK_EQ("\x41\x6e\x6f\x74\x68\x65\x72\x32", OperatorToString(&op1b).get());

  SimpleOperator op2a(20, 0, 0, 0, "\x46\x6c\x6f\x67\x31");
  SimpleOperator op2b(20, 1, 1, 1, "\x46\x6c\x6f\x67\x32");

  CHECK_EQ("\x46\x6c\x6f\x67\x31", OperatorToString(&op2a).get());
  CHECK_EQ("\x46\x6c\x6f\x67\x32", OperatorToString(&op2b).get());
}


TEST(TestOperator1intHash) {
  Operator1<int> op1a(23, 0, 0, 0, "\x57\x6f\x6c\x66\x69\x65", 11);
  Operator1<int> op1b(23, 2, 2, 2, "\x44\x6f\x67\x67\x69\x65", 11);

  CHECK_EQ(op1a.HashCode(), op1b.HashCode());

  Operator1<int> op2a(24, 0, 0, 0, "\x41\x72\x66\x69\x65", 3);
  Operator1<int> op2b(24, 0, 0, 0, "\x41\x72\x66\x69\x65", 4);

  CHECK_NE(op1a.HashCode(), op2a.HashCode());
  CHECK_NE(op2a.HashCode(), op2b.HashCode());
}


TEST(TestOperator1intEquals) {
  Operator1<int> op1a(23, 0, 0, 0, "\x53\x63\x72\x61\x74\x63\x68\x79", 11);
  Operator1<int> op1b(23, 2, 2, 2, "\x53\x63\x72\x61\x74\x63\x68\x79", 11);

  CHECK(op1a.Equals(&op1a));
  CHECK(op1a.Equals(&op1b));
  CHECK(op1b.Equals(&op1a));
  CHECK(op1b.Equals(&op1b));

  Operator1<int> op2a(24, 0, 0, 0, "\x49\x6d", 3);
  Operator1<int> op2b(24, 0, 0, 0, "\x49\x6d", 4);

  CHECK(op2a.Equals(&op2a));
  CHECK(!op2a.Equals(&op2b));
  CHECK(!op2b.Equals(&op2a));
  CHECK(op2b.Equals(&op2b));

  CHECK(!op1a.Equals(&op2a));
  CHECK(!op1a.Equals(&op2b));
  CHECK(!op1b.Equals(&op2a));
  CHECK(!op1b.Equals(&op2b));

  CHECK(!op2a.Equals(&op1a));
  CHECK(!op2a.Equals(&op1b));
  CHECK(!op2b.Equals(&op1a));
  CHECK(!op2b.Equals(&op1b));

  SimpleOperator op3(25, 0, 0, 0, "\x57\x65\x65\x70\x79");

  CHECK(!op1a.Equals(&op3));
  CHECK(!op1b.Equals(&op3));
  CHECK(!op2a.Equals(&op3));
  CHECK(!op2b.Equals(&op3));

  CHECK(!op3.Equals(&op1a));
  CHECK(!op3.Equals(&op1b));
  CHECK(!op3.Equals(&op2a));
  CHECK(!op3.Equals(&op2b));
}


TEST(TestOperator1intPrint) {
  Operator1<int> op1(12, 0, 0, 1, "\x4f\x70\x31\x54\x65\x73\x74", 0);
  CHECK_EQ("\x4f\x70\x31\x54\x65\x73\x74\x5b\x30\x5d", OperatorToString(&op1).get());

  Operator1<int> op2(12, 0, 0, 1, "\x4f\x70\x31\x54\x65\x73\x74", 66666666);
  CHECK_EQ("\x4f\x70\x31\x54\x65\x73\x74\x5b\x36\x36\x36\x36\x36\x36\x36\x36\x5d", OperatorToString(&op2).get());

  Operator1<int> op3(12, 0, 0, 1, "\x46\x6f\x6f\x42\x61\x72", 2347);
  CHECK_EQ("\x46\x6f\x6f\x42\x61\x72\x5b\x32\x33\x34\x37\x5d", OperatorToString(&op3).get());

  Operator1<int> op4(12, 0, 0, 1, "\x42\x61\x72\x46\x6f\x6f", -879);
  CHECK_EQ("\x42\x61\x72\x46\x6f\x6f\x5b\x2d\x38\x37\x39\x5d", OperatorToString(&op4).get());
}


TEST(TestOperator1doubleHash) {
  Operator1<double> op1a(23, 0, 0, 0, "\x57\x6f\x6c\x66\x69\x65", 11.77);
  Operator1<double> op1b(23, 2, 2, 2, "\x44\x6f\x67\x67\x69\x65", 11.77);

  CHECK_EQ(op1a.HashCode(), op1b.HashCode());

  Operator1<double> op2a(24, 0, 0, 0, "\x41\x72\x66\x69\x65", -6.7);
  Operator1<double> op2b(24, 0, 0, 0, "\x41\x72\x66\x69\x65", -6.8);

  CHECK_NE(op1a.HashCode(), op2a.HashCode());
  CHECK_NE(op2a.HashCode(), op2b.HashCode());
}


TEST(TestOperator1doubleEquals) {
  Operator1<double> op1a(23, 0, 0, 0, "\x53\x63\x72\x61\x74\x63\x68\x79", 11.77);
  Operator1<double> op1b(23, 2, 2, 2, "\x53\x63\x72\x61\x74\x63\x68\x79", 11.77);

  CHECK(op1a.Equals(&op1a));
  CHECK(op1a.Equals(&op1b));
  CHECK(op1b.Equals(&op1a));
  CHECK(op1b.Equals(&op1b));

  Operator1<double> op2a(24, 0, 0, 0, "\x49\x6d", 3.1);
  Operator1<double> op2b(24, 0, 0, 0, "\x49\x6d", 3.2);

  CHECK(op2a.Equals(&op2a));
  CHECK(!op2a.Equals(&op2b));
  CHECK(!op2b.Equals(&op2a));
  CHECK(op2b.Equals(&op2b));

  CHECK(!op1a.Equals(&op2a));
  CHECK(!op1a.Equals(&op2b));
  CHECK(!op1b.Equals(&op2a));
  CHECK(!op1b.Equals(&op2b));

  CHECK(!op2a.Equals(&op1a));
  CHECK(!op2a.Equals(&op1b));
  CHECK(!op2b.Equals(&op1a));
  CHECK(!op2b.Equals(&op1b));

  SimpleOperator op3(25, 0, 0, 0, "\x57\x65\x65\x70\x79");

  CHECK(!op1a.Equals(&op3));
  CHECK(!op1b.Equals(&op3));
  CHECK(!op2a.Equals(&op3));
  CHECK(!op2b.Equals(&op3));

  CHECK(!op3.Equals(&op1a));
  CHECK(!op3.Equals(&op1b));
  CHECK(!op3.Equals(&op2a));
  CHECK(!op3.Equals(&op2b));

  Operator1<double> op4a(24, 0, 0, 0, "\x42\x61\x73\x68\x66\x75\x6c", NaN);
  Operator1<double> op4b(24, 0, 0, 0, "\x42\x61\x73\x68\x66\x75\x6c", NaN);

  CHECK(op4a.Equals(&op4a));
  CHECK(op4a.Equals(&op4b));
  CHECK(op4b.Equals(&op4a));
  CHECK(op4b.Equals(&op4b));

  CHECK(!op3.Equals(&op4a));
  CHECK(!op3.Equals(&op4b));
  CHECK(!op3.Equals(&op4a));
  CHECK(!op3.Equals(&op4b));
}


TEST(TestOperator1doublePrint) {
  Operator1<double> op1(12, 0, 0, 1, "\x4f\x70\x31\x54\x65\x73\x74", 0);
  CHECK_EQ("\x4f\x70\x31\x54\x65\x73\x74\x5b\x30\x5d", OperatorToString(&op1).get());

  Operator1<double> op2(12, 0, 0, 1, "\x4f\x70\x31\x54\x65\x73\x74", 7.3);
  CHECK_EQ("\x4f\x70\x31\x54\x65\x73\x74\x5b\x37\x2e\x33\x5d", OperatorToString(&op2).get());

  Operator1<double> op3(12, 0, 0, 1, "\x46\x6f\x6f\x42\x61\x72", 2e+123);
  CHECK_EQ("\x46\x6f\x6f\x42\x61\x72\x5b\x32\x65\x2b\x31\x32\x33\x5d", OperatorToString(&op3).get());

  Operator1<double> op4(12, 0, 0, 1, "\x42\x61\x72\x46\x6f\x6f", Infinity);
  CHECK_EQ("\x42\x61\x72\x46\x6f\x6f\x5b\x69\x6e\x66\x5d", OperatorToString(&op4).get());

  Operator1<double> op5(12, 0, 0, 1, "\x42\x61\x72\x46\x6f\x6f", NaN);
  CHECK_EQ("\x42\x61\x72\x46\x6f\x6f\x5b\x6e\x61\x6e\x5d", OperatorToString(&op5).get());
}
