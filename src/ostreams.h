// Copyright 2014 the V8 project authors. All rights reserved.
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.

#ifndef V8_OSTREAMS_H_
#define V8_OSTREAMS_H_

#include <cstddef>
#include <cstdio>
#include <cstring>
#include <ostream>  // NOLINT
#include <streambuf>

#include "include/v8config.h"
#include "src/base/macros.h"

namespace v8 {
namespace internal {


class OFStreamBase : public std::streambuf {
 public:
  explicit OFStreamBase(FILE* f);
  virtual ~OFStreamBase();

 protected:
  FILE* const f_;

  virtual int sync();
  virtual int_type overflow(int_type c);
  virtual std::streamsize xsputn(const char* s, std::streamsize n);
};


// An output stream writing to a file.
class OFStream : public v8::base::OStream {
 public:
  explicit OFStream(FILE* f);
  virtual ~OFStream();

 private:
  OFStreamBase buf_;
};


// Wrappers to disambiguate uint16_t and uc16.
struct AsUC16 {
  explicit AsUC16(uint16_t v) : value(v) {}
  uint16_t value;
};


struct AsUC32 {
  explicit AsUC32(int32_t v) : value(v) {}
  int32_t value;
};


struct AsReversiblyEscapedUC16 {
  explicit AsReversiblyEscapedUC16(uint16_t v) : value(v) {}
  uint16_t value;
};

struct AsEscapedUC16ForJSON {
  explicit AsEscapedUC16ForJSON(uint16_t v) : value(v) {}
  uint16_t value;
};


// Writes the given character to the output escaping everything outside of
// printable/space ASCII range. Additionally escapes '\' making escaping
// reversible.
v8::base::OStream& operator<<(v8::base::OStream& os, const AsReversiblyEscapedUC16& c);
DEFINE_INSERT_OPERATOR_FOR_OSTREAM(const AsReversiblyEscapedUC16&);

// Same as AsReversiblyEscapedUC16 with additional escaping of \n, \r, " and '.
v8::base::OStream& operator<<(v8::base::OStream& os, const AsEscapedUC16ForJSON& c);
DEFINE_INSERT_OPERATOR_FOR_OSTREAM(const AsEscapedUC16ForJSON&);

// Writes the given character to the output escaping everything outside
// of printable ASCII range.
v8::base::OStream& operator<<(v8::base::OStream& os, const AsUC16& c);
DEFINE_INSERT_OPERATOR_FOR_OSTREAM(const AsUC16&);

// Writes the given character to the output escaping everything outside
// of printable ASCII range.
v8::base::OStream& operator<<(v8::base::OStream& os, const AsUC32& c);
DEFINE_INSERT_OPERATOR_FOR_OSTREAM(const AsUC32&);

}  // namespace internal
}  // namespace v8

#endif  // V8_OSTREAMS_H_
