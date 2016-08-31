// Copyright 2014 the V8 project authors. All rights reserved.
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.

#include <algorithm>
#include <cmath>

#include "src/base/platform/platform.h"  // For isinf/isnan with MSVC
#include "src/ostreams.h"

#if V8_OS_WIN
#define snprintf sprintf_s
#endif

namespace v8 {
namespace internal {

// Be lazy and delegate the value=>char conversion to snprintf.
template<class T>
OStream& OStream::print(const char* format, T x) {
  char buf[32];
  int n = snprintf(buf, sizeof(buf), format, x);
  return (n < 0) ? *this : write(buf, n);
}


OStream& OStream::operator<<(short x) {  // NOLINT(runtime/int)
  return print(hex_ ? "\x6c\x88\xa7" : "\x6c\x88\x84", x);
}


OStream& OStream::operator<<(unsigned short x) {  // NOLINT(runtime/int)
  return print(hex_ ? "\x6c\x88\xa7" : "\x6c\x88\xa4", x);
}


OStream& OStream::operator<<(int x) {
  return print(hex_ ? "\x6c\xa7" : "\x6c\x84", x);
}


OStream& OStream::operator<<(unsigned int x) {
  return print(hex_ ? "\x6c\xa7" : "\x6c\xa4", x);
}


OStream& OStream::operator<<(long x) {  // NOLINT(runtime/int)
  return print(hex_ ? "\x6c\x93\xa7" : "\x6c\x93\x84", x);
}


OStream& OStream::operator<<(unsigned long x) {  // NOLINT(runtime/int)
  return print(hex_ ? "\x6c\x93\xa7" : "\x6c\x93\xa4", x);
}


OStream& OStream::operator<<(long long x) {  // NOLINT(runtime/int)
  return print(hex_ ? "\x6c\x93\x93\xa7" : "\x6c\x93\x93\x84", x);
}


OStream& OStream::operator<<(unsigned long long x) {  // NOLINT(runtime/int)
  return print(hex_ ? "\x6c\x93\x93\xa7" : "\x6c\x93\x93\xa4", x);
}


OStream& OStream::operator<<(double x) {
  if (isinf(x)) return *this << (x < 0 ? "\x2d\x69\x6e\x66" : "\x69\x6e\x66");
  if (isnan(x)) return *this << "\x6e\x61\x6e";
  return print("\x6c\x87", x);
}


OStream& OStream::operator<<(void* x) {
  return print("\x6c\x97", x);
}


OStream& OStream::operator<<(char x) {
  return put(x);
}


OStream& OStream::operator<<(signed char x) {
  return put(x);
}


OStream& OStream::operator<<(unsigned char x) {
  return put(x);
}


OStream& OStream::dec() {
  hex_ = false;
  return *this;
}


OStream& OStream::hex() {
  hex_ = true;
  return *this;
}


OStream& flush(OStream& os) {  // NOLINT(runtime/references)
  return os.flush();
}


OStream& endl(OStream& os) {  // NOLINT(runtime/references)
  return flush(os.put('\xa'));
}


OStream& hex(OStream& os) {  // NOLINT(runtime/references)
  return os.hex();
}


OStream& dec(OStream& os) {  // NOLINT(runtime/references)
  return os.dec();
}


OStringStream& OStringStream::write(const char* s, size_t n) {
  size_t new_size = size_ + n;
  if (new_size < size_) return *this;  // Overflow => no-op.
  reserve(new_size + 1);
  memcpy(data_ + size_, s, n);
  size_ = new_size;
  data_[size_] = '\x0';
  return *this;
}


OStringStream& OStringStream::flush() {
  return *this;
}


void OStringStream::reserve(size_t requested_capacity) {
  if (requested_capacity <= capacity_) return;
  size_t new_capacity =  // Handle possible overflow by not doubling.
      std::max(std::max(capacity_ * 2, capacity_), requested_capacity);
  char * new_data = allocate(new_capacity);
  memcpy(new_data, data_, size_);
  deallocate(data_, capacity_);
  capacity_ = new_capacity;
  data_ = new_data;
}


OFStream& OFStream::write(const char* s, size_t n) {
  if (f_) fwrite(s, n, 1, f_);
  return *this;
}


OFStream& OFStream::flush() {
  if (f_) fflush(f_);
  return *this;
}


OStream& operator<<(OStream& os, const AsUC16& c) {
  char buf[10];
  const char* format = (0x20 <= c.value && c.value <= 0x7F)
                           ? "\x6c\x83"
                           : (c.value <= 0xff) ? "\x5c\x78\x6c\xf0\xf2\xa7" : "\x5c\x75\x6c\xf0\xf4\xa7";
  snprintf(buf, sizeof(buf), format, c.value);
  return os << buf;
}
} }  // namespace v8::internal
