// Copyright 2006-2008 the V8 project authors. All rights reserved.
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.

#include "src/checks.h"

#include "src/v8.h"

namespace v8 {
namespace internal {

intptr_t HeapObjectTagMask() { return kHeapObjectTagMask; }

} }  // namespace v8::internal


static bool CheckEqualsStrict(volatile double* exp, volatile double* val) {
  v8::internal::DoubleRepresentation exp_rep(*exp);
  v8::internal::DoubleRepresentation val_rep(*val);
  if (isnan(exp_rep.value) && isnan(val_rep.value)) return true;
  return exp_rep.bits == val_rep.bits;
}


void CheckEqualsHelper(const char* file, int line, const char* expected_source,
                       double expected, const char* value_source,
                       double value) {
  // Force values to 64 bit memory to truncate 80 bit precision on IA32.
  volatile double* exp = new double[1];
  *exp = expected;
  volatile double* val = new double[1];
  *val = value;
  if (!CheckEqualsStrict(exp, val)) {
    V8_Fatal(file, line,
             "\x43\x48\x45\x43\x4b\x5f\x45\x51\x28\x6c\xa2\x2c\x20\x6c\xa2\x29\x20\x66\x61\x69\x6c\x65\x64\xa\x23\x20\x20\x20\x45\x78\x70\x65\x63\x74\x65\x64\x3a\x20\x6c\x86\xa\x23\x20\x20\x20\x46\x6f\x75\x6e\x64\x3a\x20\x6c\x86",
             expected_source, value_source, *exp, *val);
  }
  delete[] exp;
  delete[] val;
}


void CheckNonEqualsHelper(const char* file, int line,
                          const char* expected_source, double expected,
                          const char* value_source, double value) {
  // Force values to 64 bit memory to truncate 80 bit precision on IA32.
  volatile double* exp = new double[1];
  *exp = expected;
  volatile double* val = new double[1];
  *val = value;
  if (CheckEqualsStrict(exp, val)) {
    V8_Fatal(file, line,
             "\x43\x48\x45\x43\x4b\x5f\x45\x51\x28\x6c\xa2\x2c\x20\x6c\xa2\x29\x20\x66\x61\x69\x6c\x65\x64\xa\x23\x20\x20\x20\x45\x78\x70\x65\x63\x74\x65\x64\x3a\x20\x6c\x86\xa\x23\x20\x20\x20\x46\x6f\x75\x6e\x64\x3a\x20\x6c\x86",
             expected_source, value_source, *exp, *val);
  }
  delete[] exp;
  delete[] val;
}


void CheckEqualsHelper(const char* file,
                       int line,
                       const char* expected_source,
                       v8::Handle<v8::Value> expected,
                       const char* value_source,
                       v8::Handle<v8::Value> value) {
  if (!expected->Equals(value)) {
    v8::String::Utf8Value value_str(value);
    v8::String::Utf8Value expected_str(expected);
    V8_Fatal(file, line,
             "\x43\x48\x45\x43\x4b\x5f\x45\x51\x28\x6c\xa2\x2c\x20\x6c\xa2\x29\x20\x66\x61\x69\x6c\x65\x64\xa\x23\x20\x20\x20\x45\x78\x70\x65\x63\x74\x65\x64\x3a\x20\x6c\xa2\xa\x23\x20\x20\x20\x46\x6f\x75\x6e\x64\x3a\x20\x6c\xa2",
             expected_source, value_source, *expected_str, *value_str);
  }
}


void CheckNonEqualsHelper(const char* file,
                          int line,
                          const char* unexpected_source,
                          v8::Handle<v8::Value> unexpected,
                          const char* value_source,
                          v8::Handle<v8::Value> value) {
  if (unexpected->Equals(value)) {
    v8::String::Utf8Value value_str(value);
    V8_Fatal(file, line, "\x43\x48\x45\x43\x4b\x5f\x4e\x45\x28\x6c\xa2\x2c\x20\x6c\xa2\x29\x20\x66\x61\x69\x6c\x65\x64\xa\x23\x20\x20\x20\x56\x61\x6c\x75\x65\x3a\x20\x6c\xa2",
             unexpected_source, value_source, *value_str);
  }
}
