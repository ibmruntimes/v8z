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
#ifdef V8_OS_ZOS
  uc32 ascii_c = (uc32)ebcdic2ascii((const char)c);
#else
  uc32 ascii_c = c;
#endif
  return ascii_c | 0x20;
}


inline bool IsCarriageReturn(uc32 c) {
#ifdef V8_OS_ZOS
  uc32 ascii_c = (uc32)ebcdic2ascii((const char)c);
#else
  uc32 ascii_c = c;
#endif
  return ascii_c == 0x000D;
}


inline bool IsLineFeed(uc32 c) {
#ifdef V8_OS_ZOS
  return ebcdic2ascii(c) == 0x000A;
#else
  return c == 0x000A;
#endif
}


inline bool IsInRange(int value, int lower_limit, int higher_limit) {
  DCHECK(lower_limit <= higher_limit);
  return static_cast<unsigned int>(value - lower_limit) <=
      static_cast<unsigned int>(higher_limit - lower_limit);
}


inline bool IsDecimalDigit(uc32 c) {
  // ECMA-262, 3rd, 7.8.3 (p 16)
#ifdef V8_OS_ZOS
  return IsInRange(ebcdic2ascii(c), ebcdic2ascii('0'), ebcdic2ascii('9'));
#else
  return IsInRange(c, '0', '9');
#endif
}


inline bool IsHexDigit(uc32 c) {
  // ECMA-262, 3rd, 7.6 (p 15)
#ifdef V8_OS_ZOS
  return IsDecimalDigit(c) ||
         IsInRange(AsciiAlphaToLower(c), ebcdic2ascii('a'), ebcdic2ascii('f'));
#else
  return IsDecimalDigit(c) || IsInRange(AsciiAlphaToLower(c), 'a', 'f');
#endif
}


inline bool IsOctalDigit(uc32 c) {
  // ECMA-262, 6th, 7.8.3
#ifdef V8_OS_ZOS
  return IsInRange(ebcdic2ascii(c), ebcdic2ascii('0'), ebcdic2ascii('7'));
#else
  return IsInRange(c, '0', '7');
#endif
}


inline bool IsBinaryDigit(uc32 c) {
  // ECMA-262, 6th, 7.8.3
  return c == '0' || c == '1';
}


inline bool IsRegExpWord(uc16 c) {
#ifdef V8_OS_ZOS
  return IsInRange(AsciiAlphaToLower(c), ebcdic2ascii('a'), ebcdic2ascii('z'))
      || IsDecimalDigit(c)
      || (c == '_');
#else
  return IsInRange(AsciiAlphaToLower(c), 'a', 'z')
      || IsDecimalDigit(c)
      || (c == '_');
#endif
}


inline bool IsRegExpNewline(uc16 c) {
#ifdef V8_OS_ZOS
  uc16 ascii_c = (uc16)ebcdic2ascii((const char)c);
#else
  uc16 ascii_c = c;
#endif
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
