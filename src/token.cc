// Copyright 2006-2008 the V8 project authors. All rights reserved.
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.

#include "include/v8stdint.h"
#include "src/token.h"

namespace v8 {
namespace internal {

#pragma convert("ISO8859-1")
#define T(name, string, precedence) #name,
const char* const Token::name_[NUM_TOKENS] = {
  TOKEN_LIST(T, T)
};
#undef T
#pragma convert(pop)


#define T(name, string, precedence) string,
const char* const Token::string_[NUM_TOKENS] = {
  TOKEN_LIST(T, T)
};
#undef T


#define T(name, string, precedence) precedence,
const int8_t Token::precedence_[NUM_TOKENS] = {
  TOKEN_LIST(T, T)
};
#undef T


#define KT(a, b, c) '\x54',
#define KK(a, b, c) '\x4b',
const char Token::token_type[] = {
  TOKEN_LIST(KT, KK)
};
#undef KT
#undef KK

} }  // namespace v8::internal
