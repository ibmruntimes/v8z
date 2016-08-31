// Copyright 2012 the V8 project authors. All rights reserved.
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.

#ifndef V8_TOKEN_H_
#define V8_TOKEN_H_

#include "src/base/logging.h"

namespace v8 {
namespace internal {

// TOKEN_LIST takes a list of 3 macros M, all of which satisfy the
// same signature M(name, string, precedence), where name is the
// symbolic token name, string is the corresponding syntactic symbol
// (or NULL, for literals), and precedence is the precedence (or 0).
// The parameters are invoked for token categories as follows:
//
//   T: Non-keyword tokens
//   K: Keyword tokens

// IGNORE_TOKEN is a convenience macro that can be supplied as
// an argument (at any position) for a TOKEN_LIST call. It does
// nothing with tokens belonging to the respective category.

#define IGNORE_TOKEN(name, string, precedence)
#define TOKEN_LIST(T, K)                                             \
  /* End of source indicator. */                                     \
  T(EOS, "\x45\x4f\x53", 0)                                                   \
                                                                     \
  /* Punctuators (ECMA-262, section 7.7, page 15). */                \
  T(LPAREN, "\x28", 0)                                                  \
  T(RPAREN, "\x29", 0)                                                  \
  T(LBRACK, "\x5b", 0)                                                  \
  T(RBRACK, "\x5d", 0)                                                  \
  T(LBRACE, "\x7b", 0)                                                  \
  T(RBRACE, "\x7d", 0)                                                  \
  T(COLON, "\x3a", 0)                                                   \
  T(SEMICOLON, "\x3b", 0)                                               \
  T(PERIOD, "\x2e", 0)                                                  \
  T(CONDITIONAL, "\x3f", 3)                                             \
  T(INC, "\x2b\x2b", 0)                                                    \
  T(DEC, "\x2d\x2d", 0)                                                    \
  T(ARROW, "\x3d\x3e", 0)                                                  \
                                                                     \
  /* Assignment operators. */                                        \
  /* IsAssignmentOp() and Assignment::is_compound() relies on */     \
  /* this block of enum values being contiguous and sorted in the */ \
  /* same order! */                                                  \
  T(INIT_VAR, "\x3d\x69\x6e\x69\x74\x5f\x76\x61\x72", 2)                   /* AST-use only. */  \
  T(INIT_LET, "\x3d\x69\x6e\x69\x74\x5f\x6c\x65\x74", 2)                   /* AST-use only. */  \
  T(INIT_CONST, "\x3d\x69\x6e\x69\x74\x5f\x63\x6f\x6e\x73\x74", 2)               /* AST-use only. */  \
  T(INIT_CONST_LEGACY, "\x3d\x69\x6e\x69\x74\x5f\x63\x6f\x6e\x73\x74\x5f\x6c\x65\x67\x61\x63\x79", 2) /* AST-use only. */  \
  T(ASSIGN, "\x3d", 2)                                                  \
  T(ASSIGN_BIT_OR, "\x7c\x3d", 2)                                          \
  T(ASSIGN_BIT_XOR, "\x5e\x3d", 2)                                         \
  T(ASSIGN_BIT_AND, "\x26\x3d", 2)                                         \
  T(ASSIGN_SHL, "\x3c\x3c\x3d", 2)                                            \
  T(ASSIGN_SAR, "\x3e\x3e\x3d", 2)                                            \
  T(ASSIGN_SHR, "\x3e\x3e\x3e\x3d", 2)                                           \
  T(ASSIGN_ADD, "\x2b\x3d", 2)                                             \
  T(ASSIGN_SUB, "\x2d\x3d", 2)                                             \
  T(ASSIGN_MUL, "\x2a\x3d", 2)                                             \
  T(ASSIGN_DIV, "\x2f\x3d", 2)                                             \
  T(ASSIGN_MOD, "\x25\x3d", 2)                                             \
                                                                     \
  /* Binary operators sorted by precedence. */                       \
  /* IsBinaryOp() relies on this block of enum values */             \
  /* being contiguous and sorted in the same order! */               \
  T(COMMA, "\x2c", 1)                                                   \
  T(OR, "\x7c\x7c", 4)                                                     \
  T(AND, "\x26\x26", 5)                                                    \
  T(BIT_OR, "\x7c", 6)                                                  \
  T(BIT_XOR, "\x5e", 7)                                                 \
  T(BIT_AND, "\x26", 8)                                                 \
  T(SHL, "\x3c\x3c", 11)                                                   \
  T(SAR, "\x3e\x3e", 11)                                                   \
  T(SHR, "\x3e\x3e\x3e", 11)                                                  \
  T(ROR, "\x72\x6f\x74\x61\x74\x65\x20\x72\x69\x67\x68\x74", 11) /* only used by Crankshaft */           \
  T(ADD, "\x2b", 12)                                                    \
  T(SUB, "\x2d", 12)                                                    \
  T(MUL, "\x2a", 13)                                                    \
  T(DIV, "\x2f", 13)                                                    \
  T(MOD, "\x25", 13)                                                    \
                                                                     \
  /* Compare operators sorted by precedence. */                      \
  /* IsCompareOp() relies on this block of enum values */            \
  /* being contiguous and sorted in the same order! */               \
  T(EQ, "\x3d\x3d", 9)                                                     \
  T(NE, "\x21\x3d", 9)                                                     \
  T(EQ_STRICT, "\x3d\x3d\x3d", 9)                                             \
  T(NE_STRICT, "\x21\x3d\x3d", 9)                                             \
  T(LT, "\x3c", 10)                                                     \
  T(GT, "\x3e", 10)                                                     \
  T(LTE, "\x3c\x3d", 10)                                                   \
  T(GTE, "\x3e\x3d", 10)                                                   \
  K(INSTANCEOF, "\x69\x6e\x73\x74\x61\x6e\x63\x65\x6f\x66", 10)                                    \
  K(IN, "\x69\x6e", 10)                                                    \
                                                                     \
  /* Unary operators. */                                             \
  /* IsUnaryOp() relies on this block of enum values */              \
  /* being contiguous and sorted in the same order! */               \
  T(NOT, "\x21", 0)                                                     \
  T(BIT_NOT, "\x7e", 0)                                                 \
  K(DELETE, "\x64\x65\x6c\x65\x74\x65", 0)                                             \
  K(TYPEOF, "\x74\x79\x70\x65\x6f\x66", 0)                                             \
  K(VOID, "\x76\x6f\x69\x64", 0)                                                 \
                                                                     \
  /* Keywords (ECMA-262, section 7.5.2, page 13). */                 \
  K(BREAK, "\x62\x72\x65\x61\x6b", 0)                                               \
  K(CASE, "\x63\x61\x73\x65", 0)                                                 \
  K(CATCH, "\x63\x61\x74\x63\x68", 0)                                               \
  K(CONTINUE, "\x63\x6f\x6e\x74\x69\x6e\x75\x65", 0)                                         \
  K(DEBUGGER, "\x64\x65\x62\x75\x67\x67\x65\x72", 0)                                         \
  K(DEFAULT, "\x64\x65\x66\x61\x75\x6c\x74", 0)                                           \
  /* DELETE */                                                       \
  K(DO, "\x64\x6f", 0)                                                     \
  K(ELSE, "\x65\x6c\x73\x65", 0)                                                 \
  K(FINALLY, "\x66\x69\x6e\x61\x6c\x6c\x79", 0)                                           \
  K(FOR, "\x66\x6f\x72", 0)                                                   \
  K(FUNCTION, "\x66\x75\x6e\x63\x74\x69\x6f\x6e", 0)                                         \
  K(IF, "\x69\x66", 0)                                                     \
  /* IN */                                                           \
  /* INSTANCEOF */                                                   \
  K(NEW, "\x6e\x65\x77", 0)                                                   \
  K(RETURN, "\x72\x65\x74\x75\x72\x6e", 0)                                             \
  K(SWITCH, "\x73\x77\x69\x74\x63\x68", 0)                                             \
  K(THIS, "\x74\x68\x69\x73", 0)                                                 \
  K(THROW, "\x74\x68\x72\x6f\x77", 0)                                               \
  K(TRY, "\x74\x72\x79", 0)                                                   \
  /* TYPEOF */                                                       \
  K(VAR, "\x76\x61\x72", 0)                                                   \
  /* VOID */                                                         \
  K(WHILE, "\x77\x68\x69\x6c\x65", 0)                                               \
  K(WITH, "\x77\x69\x74\x68", 0)                                                 \
                                                                     \
  /* Literals (ECMA-262, section 7.8, page 16). */                   \
  K(NULL_LITERAL, "\x6e\x75\x6c\x6c", 0)                                         \
  K(TRUE_LITERAL, "\x74\x72\x75\x65", 0)                                         \
  K(FALSE_LITERAL, "\x66\x61\x6c\x73\x65", 0)                                       \
  T(NUMBER, NULL, 0)                                                 \
  T(STRING, NULL, 0)                                                 \
                                                                     \
  /* Identifiers (not keywords or future reserved words). */         \
  T(IDENTIFIER, NULL, 0)                                             \
                                                                     \
  /* Future reserved words (ECMA-262, section 7.6.1.2). */           \
  T(FUTURE_RESERVED_WORD, NULL, 0)                                   \
  T(FUTURE_STRICT_RESERVED_WORD, NULL, 0)                            \
  K(CONST, "\x63\x6f\x6e\x73\x74", 0)                                               \
  K(EXPORT, "\x65\x78\x70\x6f\x72\x74", 0)                                             \
  K(IMPORT, "\x69\x6d\x70\x6f\x72\x74", 0)                                             \
  K(LET, "\x6c\x65\x74", 0)                                                   \
  K(YIELD, "\x79\x69\x65\x6c\x64", 0)                                               \
                                                                     \
  /* Illegal token - not able to scan. */                            \
  T(ILLEGAL, "\x49\x4c\x4c\x45\x47\x41\x4c", 0)                                           \
                                                                     \
  /* Scanner-internal use only. */                                   \
  T(WHITESPACE, NULL, 0)

class Token {
 public:
  // All token values.
#define T(name, string, precedence) name,
  enum Value {
    TOKEN_LIST(T, T)
    NUM_TOKENS
  };
#undef T

  // Returns a string corresponding to the C++ token name
  // (e.g. "LT" for the token LT).
  static const char* Name(Value tok) {
    DCHECK(tok < NUM_TOKENS);  // tok is unsigned
    return name_[tok];
  }

  // Predicates
  static bool IsKeyword(Value tok) {
    return token_type[tok] == '\x4b';
  }

  static bool IsAssignmentOp(Value tok) {
    return INIT_VAR <= tok && tok <= ASSIGN_MOD;
  }

  static bool IsBinaryOp(Value op) {
    return COMMA <= op && op <= MOD;
  }

  static bool IsTruncatingBinaryOp(Value op) {
    return BIT_OR <= op && op <= ROR;
  }

  static bool IsCompareOp(Value op) {
    return EQ <= op && op <= IN;
  }

  static bool IsOrderedRelationalCompareOp(Value op) {
    return op == LT || op == LTE || op == GT || op == GTE;
  }

  static bool IsEqualityOp(Value op) {
    return op == EQ || op == EQ_STRICT;
  }

  static bool IsInequalityOp(Value op) {
    return op == NE || op == NE_STRICT;
  }

  static bool IsArithmeticCompareOp(Value op) {
    return IsOrderedRelationalCompareOp(op) ||
        IsEqualityOp(op) || IsInequalityOp(op);
  }

  static Value NegateCompareOp(Value op) {
    DCHECK(IsArithmeticCompareOp(op));
    switch (op) {
      case EQ: return NE;
      case NE: return EQ;
      case EQ_STRICT: return NE_STRICT;
      case NE_STRICT: return EQ_STRICT;
      case LT: return GTE;
      case GT: return LTE;
      case LTE: return GT;
      case GTE: return LT;
      default:
        UNREACHABLE();
        return op;
    }
  }

  static Value ReverseCompareOp(Value op) {
    DCHECK(IsArithmeticCompareOp(op));
    switch (op) {
      case EQ: return EQ;
      case NE: return NE;
      case EQ_STRICT: return EQ_STRICT;
      case NE_STRICT: return NE_STRICT;
      case LT: return GT;
      case GT: return LT;
      case LTE: return GTE;
      case GTE: return LTE;
      default:
        UNREACHABLE();
        return op;
    }
  }

  static bool IsBitOp(Value op) {
    return (BIT_OR <= op && op <= SHR) || op == BIT_NOT;
  }

  static bool IsUnaryOp(Value op) {
    return (NOT <= op && op <= VOID) || op == ADD || op == SUB;
  }

  static bool IsCountOp(Value op) {
    return op == INC || op == DEC;
  }

  static bool IsShiftOp(Value op) {
    return (SHL <= op) && (op <= SHR);
  }

  // Returns a string corresponding to the JS token string
  // (.e., "<" for the token LT) or NULL if the token doesn't
  // have a (unique) string (e.g. an IDENTIFIER).
  static const char* String(Value tok) {
    DCHECK(tok < NUM_TOKENS);  // tok is unsigned.
    return string_[tok];
  }

  // Returns the precedence > 0 for binary and compare
  // operators; returns 0 otherwise.
  static int Precedence(Value tok) {
    DCHECK(tok < NUM_TOKENS);  // tok is unsigned.
    return precedence_[tok];
  }

 private:
  static const char* const name_[NUM_TOKENS];
  static const char* const string_[NUM_TOKENS];
  static const int8_t precedence_[NUM_TOKENS];
  static const char token_type[NUM_TOKENS];
};

} }  // namespace v8::internal

#endif  // V8_TOKEN_H_
