// Copyright 2011 the V8 project authors. All rights reserved.
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.

#ifndef V8_CHAR_PREDICATES_INL_H_
#define V8_CHAR_PREDICATES_INL_H_

#include "src/char-predicates.h"

namespace v8 {
namespace internal {


// If c is in 'A'-'Z' or 'a'-'z', return its lower-case.
// Else, return something outside of 'A'-'Z' and 'a'-'z'.
// Note: it ignores LOCALE.
inline int AsciiAlphaToLower(uc32 c) {
  return c | 0x20;
}


inline bool IsCarriageReturn(uc32 c) {
  return c == 0x000D;
}


inline bool IsLineFeed(uc32 c) {
  return c == 0x000A;
}


inline bool IsInRange(int value, int lower_limit, int higher_limit) {
  DCHECK(lower_limit <= higher_limit);
  return static_cast<unsigned int>(value - lower_limit) <=
      static_cast<unsigned int>(higher_limit - lower_limit);
}

inline bool IsAsciiIdentifier(uc32 c) {
  return IsAlphaNumeric(c) || c == '\x24' || c == '\x5f';
}

inline bool IsAlphaNumeric(uc32 c) {
  return IsInRange(AsciiAlphaToLower(c), '\x61', '\x7a') || IsDecimalDigit(c);
}

inline bool IsDecimalDigit(uc32 c) {
  // ECMA-262, 3rd, 7.8.3 (p 16)
  return IsInRange(c, '\x30', '\x39');
}


inline bool IsHexDigit(uc32 c) {
  // ECMA-262, 3rd, 7.6 (p 15)
  return IsDecimalDigit(c) || IsInRange(AsciiAlphaToLower(c), '\x61', '\x66');
}


inline bool IsOctalDigit(uc32 c) {
  // ECMA-262, 6th, 7.8.3
  return IsInRange(c, '\x30', '\x37');
}


inline bool IsBinaryDigit(uc32 c) {
  // ECMA-262, 6th, 7.8.3
  return c == '\x30' || c == '\x31';
}


inline bool IsRegExpWord(uc16 c) {
  return IsInRange(AsciiAlphaToLower(c), '\x61', '\x7a')
      || IsDecimalDigit(c)
      || (c == '\x5f');
}


inline bool IsRegExpNewline(uc16 c) {
  switch (c) {
    //   CR           LF           LS           PS
    case 0x000A: case 0x000D: case 0x2028: case 0x2029:
      return false;
    default:
      return true;
  }
}


}  // namespace internal
}  // namespace v8

#endif  // V8_CHAR_PREDICATES_INL_H_
