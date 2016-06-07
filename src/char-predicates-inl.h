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
  uc32 ascii_c = (uc32)ebcdic2ascii((const char)c);
  return ascii_c | 0x20;
}


inline bool IsCarriageReturn(uc32 c) {
  uc32 ascii_c = (uc32)ebcdic2ascii((const char)c);
  return ascii_c == 0x000D;
}


inline bool IsLineFeed(uc32 c) {
  return ebcdic2ascii(c) == 0x000A;
}


inline bool IsInRange(int value, int lower_limit, int higher_limit) {
  DCHECK(lower_limit <= higher_limit);
  return static_cast<unsigned int>(value - lower_limit) <=
      static_cast<unsigned int>(higher_limit - lower_limit);
}


inline bool IsDecimalDigit(uc32 c) {
  // ECMA-262, 3rd, 7.8.3 (p 16)
  return IsInRange(ebcdic2ascii(c), ebcdic2ascii('0'), ebcdic2ascii('9'));
}


inline bool IsHexDigit(uc32 c) {
  // ECMA-262, 3rd, 7.6 (p 15)
  return IsDecimalDigit(c) ||
         IsInRange(AsciiAlphaToLower(c), ebcdic2ascii('a'), ebcdic2ascii('f'));
}


inline bool IsOctalDigit(uc32 c) {
  // ECMA-262, 6th, 7.8.3
  return IsInRange(ebcdic2ascii(c), ebcdic2ascii('0'), ebcdic2ascii('7'));
}


inline bool IsBinaryDigit(uc32 c) {
  // ECMA-262, 6th, 7.8.3
  return c == '0' || c == '1';
}


inline bool IsRegExpWord(uc16 c) {
  return IsInRange(AsciiAlphaToLower(c), ebcdic2ascii('a'), ebcdic2ascii('z'))
      || IsDecimalDigit(c)
      || (c == '_');
}


inline bool IsRegExpNewline(uc16 c) {
  uc16 ascii_c = (uc16)ebcdic2ascii((const char)c);
  switch (ascii_c) {
    //   CR           LF           LS           PS
    case 0x000A: case 0x000D: case 0x2028: case 0x2029:
      return false;
    default:
      return true;
  }
}


} }  // namespace v8::internal

#endif  // V8_CHAR_PREDICATES_INL_H_
