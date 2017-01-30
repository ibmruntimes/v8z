// Copyright 2012 the V8 project authors. All rights reserved.
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.

#ifndef V8_PARSING_TOKEN_H_
#define V8_PARSING_TOKEN_H_

#include "src/base/logging.h"
#include "src/globals.h"

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
  T(EOS, u8"EOS", 0)                                                   \
                                                                     \
  /* Punctuators (ECMA-262, section 7.7, page 15). */                \
  T(LPAREN, u8"(", 0)                                                  \
  T(RPAREN, u8")", 0)                                                  \
  T(LBRACK, u8"[", 0)                                                  \
  T(RBRACK, u8"]", 0)                                                  \
  T(LBRACE, u8"{", 0)                                                  \
  T(RBRACE, u8"}", 0)                                                  \
  T(COLON, u8":", 0)                                                   \
  T(SEMICOLON, u8";", 0)                                               \
  T(PERIOD, u8".", 0)                                                  \
  T(ELLIPSIS, u8"...", 0)                                              \
  T(CONDITIONAL, u8"?", 3)                                             \
  T(INC, u8"++", 0)                                                    \
  T(DEC, u8"--", 0)                                                    \
  T(ARROW, u8"=>", 0)                                                  \
                                                                     \
  /* Assignment operators. */                                        \
  /* IsAssignmentOp() and Assignment::is_compound() relies on */     \
  /* this block of enum values being contiguous and sorted in the */ \
  /* same order! */                                                  \
  T(INIT, u8"=init", 2) /* AST-use only. */                            \
  T(ASSIGN, u8"=", 2)                                                  \
  T(ASSIGN_BIT_OR, u8"|=", 2)                                          \
  T(ASSIGN_BIT_XOR, u8"^=", 2)                                         \
  T(ASSIGN_BIT_AND, u8"&=", 2)                                         \
  T(ASSIGN_SHL, u8"<<=", 2)                                            \
  T(ASSIGN_SAR, u8">>=", 2)                                            \
  T(ASSIGN_SHR, u8">>>=", 2)                                           \
  T(ASSIGN_ADD, u8"+=", 2)                                             \
  T(ASSIGN_SUB, u8"-=", 2)                                             \
  T(ASSIGN_MUL, u8"*=", 2)                                             \
  T(ASSIGN_DIV, u8"/=", 2)                                             \
  T(ASSIGN_MOD, u8"%=", 2)                                             \
  T(ASSIGN_EXP, u8"**=", 2)                                            \
                                                                     \
  /* Binary operators sorted by precedence. */                       \
  /* IsBinaryOp() relies on this block of enum values */             \
  /* being contiguous and sorted in the same order! */               \
  T(COMMA, u8",", 1)                                                   \
  T(OR, u8"||", 4)                                                     \
  T(AND, u8"&&", 5)                                                    \
  T(BIT_OR, u8"|", 6)                                                  \
  T(BIT_XOR, u8"^", 7)                                                 \
  T(BIT_AND, u8"&", 8)                                                 \
  T(SHL, u8"<<", 11)                                                   \
  T(SAR, u8">>", 11)                                                   \
  T(SHR, u8">>>", 11)                                                  \
  T(ROR, u8"rotate right", 11) /* only used by Crankshaft */           \
  T(ADD, u8"+", 12)                                                    \
  T(SUB, u8"-", 12)                                                    \
  T(MUL, u8"*", 13)                                                    \
  T(DIV, u8"/", 13)                                                    \
  T(MOD, u8"%", 13)                                                    \
  T(EXP, u8"**", 14)                                                   \
                                                                     \
  /* Compare operators sorted by precedence. */                      \
  /* IsCompareOp() relies on this block of enum values */            \
  /* being contiguous and sorted in the same order! */               \
  T(EQ, u8"==", 9)                                                     \
  T(NE, u8"!=", 9)                                                     \
  T(EQ_STRICT, u8"===", 9)                                             \
  T(NE_STRICT, u8"!==", 9)                                             \
  T(LT, u8"<", 10)                                                     \
  T(GT, u8">", 10)                                                     \
  T(LTE, u8"<=", 10)                                                   \
  T(GTE, u8">=", 10)                                                   \
  K(INSTANCEOF, u8"instanceof", 10)                                    \
  K(IN, u8"in", 10)                                                    \
                                                                     \
  /* Unary operators. */                                             \
  /* IsUnaryOp() relies on this block of enum values */              \
  /* being contiguous and sorted in the same order! */               \
  T(NOT, u8"!", 0)                                                     \
  T(BIT_NOT, u8"~", 0)                                                 \
  K(DELETE, u8"delete", 0)                                             \
  K(TYPEOF, u8"typeof", 0)                                             \
  K(VOID, u8"void", 0)                                                 \
                                                                     \
  /* Keywords (ECMA-262, section 7.5.2, page 13). */                 \
  K(BREAK, u8"break", 0)                                               \
  K(CASE, u8"case", 0)                                                 \
  K(CATCH, u8"catch", 0)                                               \
  K(CONTINUE, u8"continue", 0)                                         \
  K(DEBUGGER, u8"debugger", 0)                                         \
  K(DEFAULT, u8"default", 0)                                           \
  /* DELETE */                                                       \
  K(DO, u8"do", 0)                                                     \
  K(ELSE, u8"else", 0)                                                 \
  K(FINALLY, u8"finally", 0)                                           \
  K(FOR, u8"for", 0)                                                   \
  K(FUNCTION, u8"function", 0)                                         \
  K(IF, u8"if", 0)                                                     \
  /* IN */                                                           \
  /* INSTANCEOF */                                                   \
  K(NEW, u8"new", 0)                                                   \
  K(RETURN, u8"return", 0)                                             \
  K(SWITCH, u8"switch", 0)                                             \
  K(THIS, u8"this", 0)                                                 \
  K(THROW, u8"throw", 0)                                               \
  K(TRY, u8"try", 0)                                                   \
  /* TYPEOF */                                                       \
  K(VAR, u8"var", 0)                                                   \
  /* VOID */                                                         \
  K(WHILE, u8"while", 0)                                               \
  K(WITH, u8"with", 0)                                                 \
                                                                     \
  /* Literals (ECMA-262, section 7.8, page 16). */                   \
  K(NULL_LITERAL, u8"null", 0)                                         \
  K(TRUE_LITERAL, u8"true", 0)                                         \
  K(FALSE_LITERAL, u8"false", 0)                                       \
  T(NUMBER, NULL, 0)                                                 \
  T(SMI, NULL, 0)                                                    \
  T(STRING, NULL, 0)                                                 \
                                                                     \
  /* Identifiers (not keywords or future reserved words). */         \
  T(IDENTIFIER, NULL, 0)                                             \
                                                                     \
  /* Future reserved words (ECMA-262, section 7.6.1.2). */           \
  T(FUTURE_RESERVED_WORD, NULL, 0)                                   \
  T(FUTURE_STRICT_RESERVED_WORD, NULL, 0)                            \
  K(CLASS, u8"class", 0)                                               \
  K(CONST, u8"const", 0)                                               \
  K(EXPORT, u8"export", 0)                                             \
  K(EXTENDS, u8"extends", 0)                                           \
  K(IMPORT, u8"import", 0)                                             \
  K(LET, u8"let", 0)                                                   \
  K(STATIC, u8"static", 0)                                             \
  K(YIELD, u8"yield", 0)                                               \
  K(SUPER, u8"super", 0)                                               \
                                                                     \
  /* Illegal token - not able to scan. */                            \
  T(ILLEGAL, u8"ILLEGAL", 0)                                           \
  T(ESCAPED_KEYWORD, NULL, 0)                                        \
  T(ESCAPED_STRICT_RESERVED_WORD, NULL, 0)                           \
                                                                     \
  /* Scanner-internal use only. */                                   \
  T(WHITESPACE, NULL, 0)                                             \
  T(UNINITIALIZED, NULL, 0)                                          \
                                                                     \
  /* ES6 Template Literals */                                        \
  T(TEMPLATE_SPAN, NULL, 0)                                          \
  T(TEMPLATE_TAIL, NULL, 0)


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

  static bool IsIdentifier(Value tok, LanguageMode language_mode,
                           bool is_generator) {
    switch (tok) {
      case IDENTIFIER:
        return true;
      case ESCAPED_STRICT_RESERVED_WORD:
      case FUTURE_STRICT_RESERVED_WORD:
      case LET:
      case STATIC:
        return is_sloppy(language_mode);
      case YIELD:
        return !is_generator && is_sloppy(language_mode);
      default:
        return false;
    }
    UNREACHABLE();
    return false;
  }

  static bool IsAssignmentOp(Value tok) {
    return INIT <= tok && tok <= ASSIGN_EXP;
  }

  static bool IsBinaryOp(Value op) { return COMMA <= op && op <= EXP; }

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

  static bool EvalComparison(Value op, double op1, double op2) {
    DCHECK(IsArithmeticCompareOp(op));
    switch (op) {
      case Token::EQ:
      case Token::EQ_STRICT: return (op1 == op2);
      case Token::NE: return (op1 != op2);
      case Token::LT: return (op1 < op2);
      case Token::GT: return (op1 > op2);
      case Token::LTE: return (op1 <= op2);
      case Token::GTE: return (op1 >= op2);
      default:
        UNREACHABLE();
        return false;
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

}  // namespace internal
}  // namespace v8

#endif  // V8_PARSING_TOKEN_H_
