// Copyright 2011 the V8 project authors. All rights reserved.
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.

// Features shared by parsing and pre-parsing scanners.

#include <cmath>

#include "src/v8.h"

#include "include/v8stdint.h"
#include "src/ast-value-factory.h"
#include "src/char-predicates-inl.h"
#include "src/conversions-inl.h"
#include "src/list-inl.h"
#include "src/parser.h"
#include "src/scanner.h"
namespace v8 {
namespace internal {


Handle<String> LiteralBuffer::Internalize(Isolate* isolate) const {
  if (is_one_byte()) {
    return isolate->factory()->InternalizeOneByteString(one_byte_literal());
  }
  return isolate->factory()->InternalizeTwoByteString(two_byte_literal());
}


// ----------------------------------------------------------------------------
// Scanner

Scanner::Scanner(UnicodeCache* unicode_cache)
    : unicode_cache_(unicode_cache),
      octal_pos_(Location::invalid()),
      harmony_scoping_(false),
      harmony_modules_(false),
      harmony_numeric_literals_(false) { }


void Scanner::Initialize(Utf16CharacterStream* source) {
  source_ = source;
  // Need to capture identifiers in order to recognize "get" and "set"
  // in object literals.
  Init();
  // Skip initial whitespace allowing HTML comment ends just like
  // after a newline and scan first token.
  has_line_terminator_before_next_ = true;
  SkipWhiteSpace();
  Scan();
}


uc32 Scanner::ScanHexNumber(int expected_length) {
  DCHECK(expected_length <= 4);  // prevent overflow

  uc32 digits[4] = { 0, 0, 0, 0 };
  uc32 x = 0;
  for (int i = 0; i < expected_length; i++) {
    digits[i] = c0_;
    int d = HexValue(c0_);
    if (d < 0) {
      // According to ECMA-262, 3rd, 7.8.4, page 18, these hex escapes
      // should be illegal, but other JS VMs just return the
      // non-escaped version of the original character.

      // Push back digits that we have advanced past.
      for (int j = i-1; j >= 0; j--) {
        PushBack(digits[j]);
      }
      return -1;
    }
    x = x * 16 + d;
    Advance();
  }

  return x;
}
/*
#if defined(V8_OS_ZOS)
static uint32_t max_token_index = 0xff;

// Table of one-character tokens, by character (0x00..0x7f only).
static const byte one_char_tokens[] = {
  Token::ILLEGAL,
  Token::ILLEGAL,
  Token::ILLEGAL,
  Token::ILLEGAL,
  Token::ILLEGAL,
  Token::ILLEGAL,
  Token::ILLEGAL,
  Token::ILLEGAL,
  Token::ILLEGAL,
  Token::ILLEGAL,
  Token::ILLEGAL,
  Token::ILLEGAL,
  Token::ILLEGAL,
  Token::ILLEGAL,
  Token::ILLEGAL,
  Token::ILLEGAL,
  Token::ILLEGAL,
  Token::ILLEGAL,
  Token::ILLEGAL,
  Token::ILLEGAL,
  Token::ILLEGAL,
  Token::ILLEGAL,
  Token::ILLEGAL,
  Token::ILLEGAL,
  Token::ILLEGAL,
  Token::ILLEGAL,
  Token::ILLEGAL,
  Token::ILLEGAL,
  Token::ILLEGAL,
  Token::ILLEGAL,
  Token::ILLEGAL,
  Token::ILLEGAL,
  Token::ILLEGAL,
  Token::ILLEGAL,
  Token::ILLEGAL,
  Token::ILLEGAL,
  Token::ILLEGAL,
  Token::ILLEGAL,
  Token::ILLEGAL,
  Token::ILLEGAL,
  Token::ILLEGAL,
  Token::ILLEGAL,
  Token::ILLEGAL,
  Token::ILLEGAL,
  Token::ILLEGAL,
  Token::ILLEGAL,
  Token::ILLEGAL,
  Token::ILLEGAL,
  Token::ILLEGAL,
  Token::ILLEGAL,
  Token::ILLEGAL,
  Token::ILLEGAL,
  Token::ILLEGAL,
  Token::ILLEGAL,
  Token::ILLEGAL,
  Token::ILLEGAL,
  Token::ILLEGAL,
  Token::ILLEGAL,
  Token::ILLEGAL,
  Token::ILLEGAL,
  Token::ILLEGAL,
  Token::ILLEGAL,
  Token::ILLEGAL,
  Token::ILLEGAL,
  Token::ILLEGAL,
  Token::ILLEGAL,
  Token::ILLEGAL,
  Token::ILLEGAL,
  Token::ILLEGAL,
  Token::ILLEGAL,
  Token::ILLEGAL,
  Token::ILLEGAL,
  Token::ILLEGAL,
  Token::ILLEGAL,
  Token::ILLEGAL,
  Token::ILLEGAL,
  Token::ILLEGAL,
  Token::LPAREN,       // 0x4d (
  Token::ILLEGAL,
  Token::ILLEGAL,
  Token::ILLEGAL,
  Token::ILLEGAL,
  Token::ILLEGAL,
  Token::ILLEGAL,
  Token::ILLEGAL,
  Token::ILLEGAL,
  Token::ILLEGAL,
  Token::ILLEGAL,
  Token::ILLEGAL,
  Token::ILLEGAL,
  Token::ILLEGAL,
  Token::ILLEGAL,
  Token::ILLEGAL,
  Token::RPAREN,     // 0x5d )
  Token::SEMICOLON,  // 0x5e ;
  Token::ILLEGAL,
  Token::ILLEGAL,
  Token::ILLEGAL,
  Token::ILLEGAL,
  Token::ILLEGAL,
  Token::ILLEGAL,
  Token::ILLEGAL,
  Token::ILLEGAL,
  Token::ILLEGAL,
  Token::ILLEGAL,
  Token::ILLEGAL,
  Token::ILLEGAL,
  Token::COMMA,        // 0x6b ,
  Token::ILLEGAL,
  Token::ILLEGAL,
  Token::ILLEGAL,
  Token::CONDITIONAL,  // 0x6f ?
  Token::ILLEGAL,
  Token::ILLEGAL,
  Token::ILLEGAL,
  Token::ILLEGAL,
  Token::ILLEGAL,
  Token::ILLEGAL,
  Token::ILLEGAL,
  Token::ILLEGAL,
  Token::ILLEGAL,
  Token::ILLEGAL,
  Token::COLON,        // 0x7a :
  Token::ILLEGAL,
  Token::ILLEGAL,
  Token::ILLEGAL,
  Token::ILLEGAL,
  Token::ILLEGAL,
  Token::ILLEGAL,
  Token::ILLEGAL,
  Token::ILLEGAL,
  Token::ILLEGAL,
  Token::ILLEGAL,
  Token::ILLEGAL,
  Token::ILLEGAL,
  Token::ILLEGAL,
  Token::ILLEGAL,
  Token::ILLEGAL,
  Token::ILLEGAL,
  Token::ILLEGAL,
  Token::ILLEGAL,
  Token::ILLEGAL,
  Token::ILLEGAL,
  Token::ILLEGAL,
  Token::ILLEGAL,
  Token::ILLEGAL,
  Token::ILLEGAL,
  Token::ILLEGAL,
  Token::ILLEGAL,
  Token::ILLEGAL,
  Token::ILLEGAL,
  Token::ILLEGAL,
  Token::ILLEGAL,
  Token::ILLEGAL,
  Token::ILLEGAL,
  Token::ILLEGAL,
  Token::ILLEGAL,
  Token::ILLEGAL,
  Token::ILLEGAL,
  Token::ILLEGAL,
  Token::ILLEGAL,
  Token::BIT_NOT,   // 0xa1 ~
  Token::ILLEGAL,
  Token::ILLEGAL,
  Token::ILLEGAL,
  Token::ILLEGAL,
  Token::ILLEGAL,
  Token::ILLEGAL,
  Token::ILLEGAL,
  Token::ILLEGAL,
  Token::ILLEGAL,
  Token::ILLEGAL,
  Token::ILLEGAL,
  Token::ILLEGAL,
  Token::ILLEGAL,
  Token::ILLEGAL,
  Token::ILLEGAL,
  Token::ILLEGAL,
  Token::ILLEGAL,
  Token::ILLEGAL,
  Token::ILLEGAL,
  Token::ILLEGAL,
  Token::ILLEGAL,
  Token::ILLEGAL,
  Token::ILLEGAL,
  Token::ILLEGAL,
  Token::LBRACK,   // 0xba [
  Token::RBRACK,   // 0xab ]
  Token::ILLEGAL,
  Token::ILLEGAL,
  Token::ILLEGAL,
  Token::ILLEGAL,
  Token::LBRACE,   // 0xc0 {
  Token::ILLEGAL,
  Token::ILLEGAL,
  Token::ILLEGAL,
  Token::ILLEGAL,
  Token::ILLEGAL,
  Token::ILLEGAL,
  Token::ILLEGAL,
  Token::ILLEGAL,
  Token::ILLEGAL,
  Token::ILLEGAL,
  Token::ILLEGAL,
  Token::ILLEGAL,
  Token::ILLEGAL,
  Token::ILLEGAL,
  Token::ILLEGAL,
  Token::RBRACE,   // 0xd0 }
  Token::ILLEGAL,
  Token::ILLEGAL,
  Token::ILLEGAL,
  Token::ILLEGAL,
  Token::ILLEGAL,
  Token::ILLEGAL,
  Token::ILLEGAL,
  Token::ILLEGAL,
  Token::ILLEGAL,
  Token::ILLEGAL,
  Token::ILLEGAL,
  Token::ILLEGAL,
  Token::ILLEGAL,
  Token::ILLEGAL,
  Token::ILLEGAL,
  Token::ILLEGAL,
  Token::ILLEGAL,
  Token::ILLEGAL,
  Token::ILLEGAL,
  Token::ILLEGAL,
  Token::ILLEGAL,
  Token::ILLEGAL,
  Token::ILLEGAL,
  Token::ILLEGAL,
  Token::ILLEGAL,
  Token::ILLEGAL,
  Token::ILLEGAL,
  Token::ILLEGAL,
  Token::ILLEGAL,
  Token::ILLEGAL,
  Token::ILLEGAL,
  Token::ILLEGAL,
  Token::ILLEGAL,
  Token::ILLEGAL,
  Token::ILLEGAL,
  Token::ILLEGAL,
  Token::ILLEGAL,
  Token::ILLEGAL,
  Token::ILLEGAL,
  Token::ILLEGAL,
  Token::ILLEGAL,
  Token::ILLEGAL,
  Token::ILLEGAL,
  Token::ILLEGAL,
  Token::ILLEGAL,
  Token::ILLEGAL,
  Token::ILLEGAL
};

#else*/
static uint32_t max_token_index = 0x7f;

// Table of one-character tokens, by character (0x00..0x7f only).
static const byte one_char_tokens[] = {
  Token::ILLEGAL,
  Token::ILLEGAL,
  Token::ILLEGAL,
  Token::ILLEGAL,
  Token::ILLEGAL,
  Token::ILLEGAL,
  Token::ILLEGAL,
  Token::ILLEGAL,
  Token::ILLEGAL,
  Token::ILLEGAL,
  Token::ILLEGAL,
  Token::ILLEGAL,
  Token::ILLEGAL,
  Token::ILLEGAL,
  Token::ILLEGAL,
  Token::ILLEGAL,
  Token::ILLEGAL,
  Token::ILLEGAL,
  Token::ILLEGAL,
  Token::ILLEGAL,
  Token::ILLEGAL,
  Token::ILLEGAL,
  Token::ILLEGAL,
  Token::ILLEGAL,
  Token::ILLEGAL,
  Token::ILLEGAL,
  Token::ILLEGAL,
  Token::ILLEGAL,
  Token::ILLEGAL,
  Token::ILLEGAL,
  Token::ILLEGAL,
  Token::ILLEGAL,
  Token::ILLEGAL,
  Token::ILLEGAL,
  Token::ILLEGAL,
  Token::ILLEGAL,
  Token::ILLEGAL,
  Token::ILLEGAL,
  Token::ILLEGAL,
  Token::ILLEGAL,
  Token::LPAREN,       // 0x28
  Token::RPAREN,       // 0x29
  Token::ILLEGAL,
  Token::ILLEGAL,
  Token::COMMA,        // 0x2c
  Token::ILLEGAL,
  Token::ILLEGAL,
  Token::ILLEGAL,
  Token::ILLEGAL,
  Token::ILLEGAL,
  Token::ILLEGAL,
  Token::ILLEGAL,
  Token::ILLEGAL,
  Token::ILLEGAL,
  Token::ILLEGAL,
  Token::ILLEGAL,
  Token::ILLEGAL,
  Token::ILLEGAL,
  Token::COLON,        // 0x3a
  Token::SEMICOLON,    // 0x3b
  Token::ILLEGAL,
  Token::ILLEGAL,
  Token::ILLEGAL,
  Token::CONDITIONAL,  // 0x3f
  Token::ILLEGAL,
  Token::ILLEGAL,
  Token::ILLEGAL,
  Token::ILLEGAL,
  Token::ILLEGAL,
  Token::ILLEGAL,
  Token::ILLEGAL,
  Token::ILLEGAL,
  Token::ILLEGAL,
  Token::ILLEGAL,
  Token::ILLEGAL,
  Token::ILLEGAL,
  Token::ILLEGAL,
  Token::ILLEGAL,
  Token::ILLEGAL,
  Token::ILLEGAL,
  Token::ILLEGAL,
  Token::ILLEGAL,
  Token::ILLEGAL,
  Token::ILLEGAL,
  Token::ILLEGAL,
  Token::ILLEGAL,
  Token::ILLEGAL,
  Token::ILLEGAL,
  Token::ILLEGAL,
  Token::ILLEGAL,
  Token::ILLEGAL,
  Token::LBRACK,     // 0x5b
  Token::ILLEGAL,
  Token::RBRACK,     // 0x5d
  Token::ILLEGAL,
  Token::ILLEGAL,
  Token::ILLEGAL,
  Token::ILLEGAL,
  Token::ILLEGAL,
  Token::ILLEGAL,
  Token::ILLEGAL,
  Token::ILLEGAL,
  Token::ILLEGAL,
  Token::ILLEGAL,
  Token::ILLEGAL,
  Token::ILLEGAL,
  Token::ILLEGAL,
  Token::ILLEGAL,
  Token::ILLEGAL,
  Token::ILLEGAL,
  Token::ILLEGAL,
  Token::ILLEGAL,
  Token::ILLEGAL,
  Token::ILLEGAL,
  Token::ILLEGAL,
  Token::ILLEGAL,
  Token::ILLEGAL,
  Token::ILLEGAL,
  Token::ILLEGAL,
  Token::ILLEGAL,
  Token::ILLEGAL,
  Token::ILLEGAL,
  Token::ILLEGAL,
  Token::LBRACE,       // 0x7b
  Token::ILLEGAL,
  Token::RBRACE,       // 0x7d
  Token::BIT_NOT,      // 0x7e
  Token::ILLEGAL
};
//#endif

// Ensure that tokens can be stored in a byte.
STATIC_ASSERT(Token::NUM_TOKENS <= 0x100);

Token::Value Scanner::Next() {
  current_ = next_;
  has_line_terminator_before_next_ = false;
  has_multiline_comment_before_next_ = false;

  if (static_cast<unsigned>(c0_) <= max_token_index) {
    Token::Value token = static_cast<Token::Value>(one_char_tokens[c0_]);
    if (token != Token::ILLEGAL) {
      int pos = source_pos();
      next_.token = token;
      next_.location.beg_pos = pos;
      next_.location.end_pos = pos + 1;
      Advance();
      return current_.token;
    }
  }
  Scan();
  return current_.token;
}


// TODO(yangguo): check whether this is actually necessary.
static inline bool IsLittleEndianByteOrderMark(uc32 c) {
  // The Unicode value U+FFFE is guaranteed never to be assigned as a
  // Unicode character; this implies that in a Unicode context the
  // 0xFF, 0xFE byte pattern can only be interpreted as the U+FEFF
  // character expressed in little-endian byte order (since it could
  // not be a U+FFFE character expressed in big-endian byte
  // order). Nevertheless, we check for it to be compatible with
  // Spidermonkey.
  return c == 0xFFFE;
}


bool Scanner::SkipWhiteSpace() {
  int start_position = source_pos();

  while (true) {
    while (true) {
      // Advance as long as character is a WhiteSpace or LineTerminator.
      // Remember if the latter is the case.
      if (unicode_cache_->IsLineTerminator(c0_)) {
        has_line_terminator_before_next_ = true;
      } else if (!unicode_cache_->IsWhiteSpace(c0_) &&
                 !IsLittleEndianByteOrderMark(c0_)) {
        break;
      }
      Advance();
    }

    // If there is an HTML comment end '-->' at the beginning of a
    // line (with only whitespace in front of it), we treat the rest
    // of the line as a comment. This is in line with the way
    // SpiderMonkey handles it.
    if (c0_ == '\x2d' && has_line_terminator_before_next_) {
      Advance();
      if (c0_ == '\x2d') {
        Advance();
        if (c0_ == '\x3e') {
          // Treat the rest of the line as a comment.
          SkipSingleLineComment();
          // Continue skipping white space after the comment.
          continue;
        }
        PushBack('\x2d');  // undo Advance()
      }
      PushBack('\x2d');  // undo Advance()
    }
    // Return whether or not we skipped any characters.
    return source_pos() != start_position;
  }
}


Token::Value Scanner::SkipSingleLineComment() {
  Advance();

  // The line terminator at the end of the line is not considered
  // to be part of the single-line comment; it is recognized
  // separately by the lexical grammar and becomes part of the
  // stream of input elements for the syntactic grammar (see
  // ECMA-262, section 7.4).
  while (c0_ >= 0 && !unicode_cache_->IsLineTerminator(c0_)) {
    Advance();
  }

  return Token::WHITESPACE;
}


Token::Value Scanner::SkipSourceURLComment() {
  TryToParseSourceURLComment();
  while (c0_ >= 0 && !unicode_cache_->IsLineTerminator(c0_)) {
    Advance();
  }

  return Token::WHITESPACE;
}


void Scanner::TryToParseSourceURLComment() {
  // Magic comments are of the form: //[#@]\s<name>=\s*<value>\s*.* and this
  // function will just return if it cannot parse a magic comment.
  if (!unicode_cache_->IsWhiteSpace(c0_))
    return;
  Advance();
  LiteralBuffer name;
  while (c0_ >= 0 && !unicode_cache_->IsWhiteSpaceOrLineTerminator(c0_) &&
         c0_ != '\x3d') {
    name.AddChar(c0_);
    Advance();
  }
  if (!name.is_one_byte()) return;
  Vector<const uint8_t> name_literal = name.one_byte_literal();
  LiteralBuffer* value;
  if (name_literal == STATIC_ASCII_VECTOR("\x73\x6f\x75\x72\x63\x65\x55\x52\x4c")) {
    value = &source_url_;
  } else if (name_literal == STATIC_ASCII_VECTOR("\x73\x6f\x75\x72\x63\x65\x4d\x61\x70\x70\x69\x6e\x67\x55\x52\x4c")) {
    value = &source_mapping_url_;
  } else {
    return;
  }
  if (c0_ != '\x3d')
    return;
  Advance();
  value->Reset();
  while (c0_ >= 0 && unicode_cache_->IsWhiteSpace(c0_)) {
    Advance();
  }
  while (c0_ >= 0 && !unicode_cache_->IsLineTerminator(c0_)) {
    // Disallowed characters.
    if (c0_ == '\x22' || c0_ == '\x27') {
      value->Reset();
      return;
    }
    if (unicode_cache_->IsWhiteSpace(c0_)) {
      break;
    }
    value->AddChar(c0_);
    Advance();
  }
  // Allow whitespace at the end.
  while (c0_ >= 0 && !unicode_cache_->IsLineTerminator(c0_)) {
    if (!unicode_cache_->IsWhiteSpace(c0_)) {
      value->Reset();
      break;
    }
    Advance();
  }
}


Token::Value Scanner::SkipMultiLineComment() {
  DCHECK(c0_ == '\x2a');
  Advance();

  while (c0_ >= 0) {
    uc32 ch = c0_;
    Advance();
    if (unicode_cache_->IsLineTerminator(ch)) {
      // Following ECMA-262, section 7.4, a comment containing
      // a newline will make the comment count as a line-terminator.
      has_multiline_comment_before_next_ = true;
    }
    // If we have reached the end of the multi-line comment, we
    // consume the '/' and insert a whitespace. This way all
    // multi-line comments are treated as whitespace.
    if (ch == '\x2a' && c0_ == '\x2f') {
      c0_ = '\x20';
      return Token::WHITESPACE;
    }
  }

  // Unterminated multi-line comment.
  return Token::ILLEGAL;
}


Token::Value Scanner::ScanHtmlComment() {
  // Check for <!-- comments.
  DCHECK(c0_ == '\x21');
  Advance();
  if (c0_ == '\x2d') {
    Advance();
    if (c0_ == '\x2d') return SkipSingleLineComment();
    PushBack('\x2d');  // undo Advance()
  }
  PushBack('\x21');  // undo Advance()
  DCHECK(c0_ == '\x21');
  return Token::LT;
}


void Scanner::Scan() {
  next_.literal_chars = NULL;
  Token::Value token;
  do {
    // Remember the position of the next token
    next_.location.beg_pos = source_pos();

    switch (c0_) {
      case '\x20':
      case '\x9':
        Advance();
        token = Token::WHITESPACE;
        break;

      case '\xa':
        Advance();
        has_line_terminator_before_next_ = true;
        token = Token::WHITESPACE;
        break;

      case '\x22': case '\x27':
        token = ScanString();
        break;

      case '\x3c':
        // < <= << <<= <!--
        Advance();
        if (c0_ == '\x3d') {
          token = Select(Token::LTE);
        } else if (c0_ == '\x3c') {
          token = Select('\x3d', Token::ASSIGN_SHL, Token::SHL);
        } else if (c0_ == '\x21') {
          token = ScanHtmlComment();
        } else {
          token = Token::LT;
        }
        break;

      case '\x3e':
        // > >= >> >>= >>> >>>=
        Advance();
        if (c0_ == '\x3d') {
          token = Select(Token::GTE);
        } else if (c0_ == '\x3e') {
          // >> >>= >>> >>>=
          Advance();
          if (c0_ == '\x3d') {
            token = Select(Token::ASSIGN_SAR);
          } else if (c0_ == '\x3e') {
            token = Select('\x3d', Token::ASSIGN_SHR, Token::SHR);
          } else {
            token = Token::SAR;
          }
        } else {
          token = Token::GT;
        }
        break;

      case '\x3d':
        // = == === =>
        Advance();
        if (c0_ == '\x3d') {
          token = Select('\x3d', Token::EQ_STRICT, Token::EQ);
        } else if (c0_ == '\x3e') {
          token = Select(Token::ARROW);
        } else {
          token = Token::ASSIGN;
        }
        break;

      case '\x21':
        // ! != !==
        Advance();
        if (c0_ == '\x3d') {
          token = Select('\x3d', Token::NE_STRICT, Token::NE);
        } else {
          token = Token::NOT;
        }
        break;

      case '\x2b':
        // + ++ +=
        Advance();
        if (c0_ == '\x2b') {
          token = Select(Token::INC);
        } else if (c0_ == '\x3d') {
          token = Select(Token::ASSIGN_ADD);
        } else {
          token = Token::ADD;
        }
        break;

      case '\x2d':
        // - -- --> -=
        Advance();
        if (c0_ == '\x2d') {
          Advance();
          if (c0_ == '\x3e' && has_line_terminator_before_next_) {
            // For compatibility with SpiderMonkey, we skip lines that
            // start with an HTML comment end '-->'.
            token = SkipSingleLineComment();
          } else {
            token = Token::DEC;
          }
        } else if (c0_ == '\x3d') {
          token = Select(Token::ASSIGN_SUB);
        } else {
          token = Token::SUB;
        }
        break;

      case '\x2a':
        // * *=
        token = Select('\x3d', Token::ASSIGN_MUL, Token::MUL);
        break;

      case '\x25':
        // % %=
        token = Select('\x3d', Token::ASSIGN_MOD, Token::MOD);
        break;

      case '\x2f':
        // /  // /* /=
        Advance();
        if (c0_ == '\x2f') {
          Advance();
          if (c0_ == '\x40' || c0_ == '\x23') {
            Advance();
            token = SkipSourceURLComment();
          } else {
            PushBack(c0_);
            token = SkipSingleLineComment();
          }
        } else if (c0_ == '\x2a') {
          token = SkipMultiLineComment();
        } else if (c0_ == '\x3d') {
          token = Select(Token::ASSIGN_DIV);
        } else {
          token = Token::DIV;
        }
        break;

      case '\x26':
        // & && &=
        Advance();
        if (c0_ == '\x26') {
          token = Select(Token::AND);
        } else if (c0_ == '\x3d') {
          token = Select(Token::ASSIGN_BIT_AND);
        } else {
          token = Token::BIT_AND;
        }
        break;

      case '\x7c':
        // | || |=
        Advance();
        if (c0_ == '\x7c') {
          token = Select(Token::OR);
        } else if (c0_ == '\x3d') {
          token = Select(Token::ASSIGN_BIT_OR);
        } else {
          token = Token::BIT_OR;
        }
        break;

      case '\x5e':
        // ^ ^=
        token = Select('\x3d', Token::ASSIGN_BIT_XOR, Token::BIT_XOR);
        break;

      case '\x2e':
        // . Number
        Advance();
        if (IsDecimalDigit(c0_)) {
          token = ScanNumber(true);
        } else {
          token = Token::PERIOD;
        }
        break;

      case '\x3a':
        token = Select(Token::COLON);
        break;

      case '\x3b':
        token = Select(Token::SEMICOLON);
        break;

      case '\x2c':
        token = Select(Token::COMMA);
        break;

      case '\x28':
        token = Select(Token::LPAREN);
        break;

      case '\x29':
        token = Select(Token::RPAREN);
        break;

      case '\x5b':
        token = Select(Token::LBRACK);
        break;

      case '\x5d':
        token = Select(Token::RBRACK);
        break;

      case '\x7b':
        token = Select(Token::LBRACE);
        break;

      case '\x7d':
        token = Select(Token::RBRACE);
        break;

      case '\x3f':
        token = Select(Token::CONDITIONAL);
        break;

      case '\x7e':
        token = Select(Token::BIT_NOT);
        break;

      default:
        if (unicode_cache_->IsIdentifierStart(c0_)) {
          token = ScanIdentifierOrKeyword();
        } else if (IsDecimalDigit(c0_)) {
          token = ScanNumber(false);
        } else if (SkipWhiteSpace()) {
          token = Token::WHITESPACE;
        } else if (c0_ < 0) {
          token = Token::EOS;
        } else {
          token = Select(Token::ILLEGAL);
        }
        break;
    }

    // Continue scanning for tokens as long as we're just skipping
    // whitespace.
  } while (token == Token::WHITESPACE);

  next_.location.end_pos = source_pos();
  next_.token = token;
}


void Scanner::SeekForward(int pos) {
  // After this call, we will have the token at the given position as
  // the "next" token. The "current" token will be invalid.
  if (pos == next_.location.beg_pos) return;
  int current_pos = source_pos();
  DCHECK_EQ(next_.location.end_pos, current_pos);
  // Positions inside the lookahead token aren't supported.
  DCHECK(pos >= current_pos);
  if (pos != current_pos) {
    source_->SeekForward(pos - source_->pos());
    Advance();
    // This function is only called to seek to the location
    // of the end of a function (at the "}" token). It doesn't matter
    // whether there was a line terminator in the part we skip.
    has_line_terminator_before_next_ = false;
    has_multiline_comment_before_next_ = false;
  }
  Scan();
}


bool Scanner::ScanEscape() {
  uc32 c = c0_;
  Advance();

  // Skip escaped newlines.
  if (unicode_cache_->IsLineTerminator(c)) {
    // Allow CR+LF newlines in multiline string literals.
    if (IsCarriageReturn(c) && IsLineFeed(c0_)) Advance();
    // Allow LF+CR newlines in multiline string literals.
    if (IsLineFeed(c) && IsCarriageReturn(c0_)) Advance();
    return true;
  }

  switch (c) {
    case '\x27':  // fall through
    case '\x22' :  // fall through
    case '\x5c': break;
    case '\x62' : c = '\x8'; break;
    case '\x66' : c = '\xc'; break;
    case '\x6e' : c = '\xa'; break;
    case '\x72' : c = '\xd'; break;
    case '\x74' : c = '\x9'; break;
    case '\x75' : {
      c = ScanHexNumber(4);
      if (c < 0) return false;
      break;
    }
    case '\x76' : c = '\xb'; break;
    case '\x78' : {
      c = ScanHexNumber(2);
      if (c < 0) return false;
      break;
    }
    case '\x30' :  // fall through
    case '\x31' :  // fall through
    case '\x32' :  // fall through
    case '\x33' :  // fall through
    case '\x34' :  // fall through
    case '\x35' :  // fall through
    case '\x36' :  // fall through
    case '\x37' : c = ScanOctalEscape(c, 2); break;
  }

  // According to ECMA-262, section 7.8.4, characters not covered by the
  // above cases should be illegal, but they are commonly handled as
  // non-escaped characters by JS VMs.
  AddLiteralChar(c);
  return true;
}


// Octal escapes of the forms '\0xx' and '\xxx' are not a part of
// ECMA-262. Other JS VMs support them.
uc32 Scanner::ScanOctalEscape(uc32 c, int length) {
  uc32 x = c - '\x30';
  int i = 0;
  for (; i < length; i++) {
    int d = c0_ - '\x30';
    if (d < 0 || d > 7) break;
    int nx = x * 8 + d;
    if (nx >= 256) break;
    x = nx;
    Advance();
  }
  // Anything except '\0' is an octal escape sequence, illegal in strict mode.
  // Remember the position of octal escape sequences so that an error
  // can be reported later (in strict mode).
  // We don't report the error immediately, because the octal escape can
  // occur before the "use strict" directive.
  if (c != '\x30' || i > 0) {
    octal_pos_ = Location(source_pos() - i - 1, source_pos() - 1);
  }
  return x;
}


Token::Value Scanner::ScanString() {
  uc32 quote = c0_;
  Advance();  // consume quote

  LiteralScope literal(this);
  while (c0_ != quote && c0_ >= 0
         && !unicode_cache_->IsLineTerminator(c0_)) {
    uc32 c = c0_;
    Advance();
    if (c == '\x5c') {
      if (c0_ < 0 || !ScanEscape()) return Token::ILLEGAL;
    } else {
      AddLiteralChar(c);
    }
  }
  if (c0_ != quote) return Token::ILLEGAL;
  literal.Complete();

  Advance();  // consume quote
  return Token::STRING;
}


void Scanner::ScanDecimalDigits() {
  while (IsDecimalDigit(c0_))
    AddLiteralCharAdvance();
}


Token::Value Scanner::ScanNumber(bool seen_period) {
  DCHECK(IsDecimalDigit(c0_));  // the first digit of the number or the fraction

  enum { DECIMAL, HEX, OCTAL, IMPLICIT_OCTAL, BINARY } kind = DECIMAL;

  LiteralScope literal(this);
  if (seen_period) {
    // we have already seen a decimal point of the float
    AddLiteralChar('\x2e');
    ScanDecimalDigits();  // we know we have at least one digit

  } else {
    // if the first character is '0' we must check for octals and hex
    if (c0_ == '\x30') {
      int start_pos = source_pos();  // For reporting octal positions.
      AddLiteralCharAdvance();

      // either 0, 0exxx, 0Exxx, 0.xxx, a hex number, a binary number or
      // an octal number.
      if (c0_ == '\x78' || c0_ == '\x58') {
        // hex number
        kind = HEX;
        AddLiteralCharAdvance();
        if (!IsHexDigit(c0_)) {
          // we must have at least one hex digit after 'x'/'X'
          return Token::ILLEGAL;
        }
        while (IsHexDigit(c0_)) {
          AddLiteralCharAdvance();
        }
      } else if (harmony_numeric_literals_ && (c0_ == '\x6f' || c0_ == '\x4f')) {
        kind = OCTAL;
        AddLiteralCharAdvance();
        if (!IsOctalDigit(c0_)) {
          // we must have at least one octal digit after 'o'/'O'
          return Token::ILLEGAL;
        }
        while (IsOctalDigit(c0_)) {
          AddLiteralCharAdvance();
        }
      } else if (harmony_numeric_literals_ && (c0_ == '\x62' || c0_ == '\x42')) {
        kind = BINARY;
        AddLiteralCharAdvance();
        if (!IsBinaryDigit(c0_)) {
          // we must have at least one binary digit after 'b'/'B'
          return Token::ILLEGAL;
        }
        while (IsBinaryDigit(c0_)) {
          AddLiteralCharAdvance();
        }
      } else if ('\x30' <= c0_ && c0_ <= '\x37') {
        // (possible) octal number
        kind = IMPLICIT_OCTAL;
        while (true) {
          if (c0_ == '\x38' || c0_ == '\x39') {
            kind = DECIMAL;
            break;
          }
          if (c0_  < '\x30' || '\x37'  < c0_) {
            // Octal literal finished.
            octal_pos_ = Location(start_pos, source_pos());
            break;
          }
          AddLiteralCharAdvance();
        }
      }
    }

    // Parse decimal digits and allow trailing fractional part.
    if (kind == DECIMAL) {
      ScanDecimalDigits();  // optional
      if (c0_ == '\x2e') {
        AddLiteralCharAdvance();
        ScanDecimalDigits();  // optional
      }
    }
  }

  // scan exponent, if any
  if (c0_ == '\x65' || c0_ == '\x45') {
    DCHECK(kind != HEX);  // '\x65'/'\x45' must be scanned as part of the hex number
    if (kind != DECIMAL) return Token::ILLEGAL;
    // scan exponent
    AddLiteralCharAdvance();
    if (c0_ == '\x2b' || c0_ == '\x2d')
      AddLiteralCharAdvance();
    if (!IsDecimalDigit(c0_)) {
      // we must have at least one decimal digit after 'e'/'E'
      return Token::ILLEGAL;
    }
    ScanDecimalDigits();
  }

  // The source character immediately following a numeric literal must
  // not be an identifier start or a decimal digit; see ECMA-262
  // section 7.8.3, page 17 (note that we read only one decimal digit
  // if the value is 0).
  if (IsDecimalDigit(c0_) || unicode_cache_->IsIdentifierStart(c0_))
    return Token::ILLEGAL;

  literal.Complete();

  return Token::NUMBER;
}


uc32 Scanner::ScanIdentifierUnicodeEscape() {
  Advance();
  if (c0_ != '\x75') return -1;
  Advance();
  uc32 result = ScanHexNumber(4);
  if (result < 0) PushBack('\x75');
  return result;
}


// ----------------------------------------------------------------------------
// Keyword Matcher

#define KEYWORDS(KEYWORD_GROUP, KEYWORD)                            \
  KEYWORD_GROUP('\x62')                                                \
  KEYWORD("\x62\x72\x65\x61\x6b", Token::BREAK)                                    \
  KEYWORD_GROUP('\x63')                                                \
  KEYWORD("\x63\x61\x73\x65", Token::CASE)                                      \
  KEYWORD("\x63\x61\x74\x63\x68", Token::CATCH)                                    \
  KEYWORD("\x63\x6c\x61\x73\x73", Token::FUTURE_RESERVED_WORD)                     \
  KEYWORD("\x63\x6f\x6e\x73\x74", Token::CONST)                                    \
  KEYWORD("\x63\x6f\x6e\x74\x69\x6e\x75\x65", Token::CONTINUE)                              \
  KEYWORD_GROUP('\x64')                                                \
  KEYWORD("\x64\x65\x62\x75\x67\x67\x65\x72", Token::DEBUGGER)                              \
  KEYWORD("\x64\x65\x66\x61\x75\x6c\x74", Token::DEFAULT)                                \
  KEYWORD("\x64\x65\x6c\x65\x74\x65", Token::DELETE)                                  \
  KEYWORD("\x64\x6f", Token::DO)                                          \
  KEYWORD_GROUP('\x65')                                                \
  KEYWORD("\x65\x6c\x73\x65", Token::ELSE)                                      \
  KEYWORD("\x65\x6e\x75\x6d", Token::FUTURE_RESERVED_WORD)                      \
  KEYWORD("\x65\x78\x70\x6f\x72\x74", harmony_modules                                 \
                    ? Token::EXPORT : Token::FUTURE_RESERVED_WORD)  \
  KEYWORD("\x65\x78\x74\x65\x6e\x64\x73", Token::FUTURE_RESERVED_WORD)                   \
  KEYWORD_GROUP('\x66')                                                \
  KEYWORD("\x66\x61\x6c\x73\x65", Token::FALSE_LITERAL)                            \
  KEYWORD("\x66\x69\x6e\x61\x6c\x6c\x79", Token::FINALLY)                                \
  KEYWORD("\x66\x6f\x72", Token::FOR)                                        \
  KEYWORD("\x66\x75\x6e\x63\x74\x69\x6f\x6e", Token::FUNCTION)                              \
  KEYWORD_GROUP('\x69')                                                \
  KEYWORD("\x69\x66", Token::IF)                                          \
  KEYWORD("\x69\x6d\x70\x6c\x65\x6d\x65\x6e\x74\x73", Token::FUTURE_STRICT_RESERVED_WORD)         \
  KEYWORD("\x69\x6d\x70\x6f\x72\x74", harmony_modules                                 \
                    ? Token::IMPORT : Token::FUTURE_RESERVED_WORD)  \
  KEYWORD("\x69\x6e", Token::IN)                                          \
  KEYWORD("\x69\x6e\x73\x74\x61\x6e\x63\x65\x6f\x66", Token::INSTANCEOF)                          \
  KEYWORD("\x69\x6e\x74\x65\x72\x66\x61\x63\x65", Token::FUTURE_STRICT_RESERVED_WORD)          \
  KEYWORD_GROUP('\x6c')                                                \
  KEYWORD("\x6c\x65\x74", harmony_scoping                                    \
                 ? Token::LET : Token::FUTURE_STRICT_RESERVED_WORD) \
  KEYWORD_GROUP('\x6e')                                                \
  KEYWORD("\x6e\x65\x77", Token::NEW)                                        \
  KEYWORD("\x6e\x75\x6c\x6c", Token::NULL_LITERAL)                              \
  KEYWORD_GROUP('\x70')                                                \
  KEYWORD("\x70\x61\x63\x6b\x61\x67\x65", Token::FUTURE_STRICT_RESERVED_WORD)            \
  KEYWORD("\x70\x72\x69\x76\x61\x74\x65", Token::FUTURE_STRICT_RESERVED_WORD)            \
  KEYWORD("\x70\x72\x6f\x74\x65\x63\x74\x65\x64", Token::FUTURE_STRICT_RESERVED_WORD)          \
  KEYWORD("\x70\x75\x62\x6c\x69\x63", Token::FUTURE_STRICT_RESERVED_WORD)             \
  KEYWORD_GROUP('\x72')                                                \
  KEYWORD("\x72\x65\x74\x75\x72\x6e", Token::RETURN)                                  \
  KEYWORD_GROUP('\x73')                                                \
  KEYWORD("\x73\x74\x61\x74\x69\x63", Token::FUTURE_STRICT_RESERVED_WORD)             \
  KEYWORD("\x73\x75\x70\x65\x72", Token::FUTURE_RESERVED_WORD)                     \
  KEYWORD("\x73\x77\x69\x74\x63\x68", Token::SWITCH)                                  \
  KEYWORD_GROUP('\x74')                                                \
  KEYWORD("\x74\x68\x69\x73", Token::THIS)                                      \
  KEYWORD("\x74\x68\x72\x6f\x77", Token::THROW)                                    \
  KEYWORD("\x74\x72\x75\x65", Token::TRUE_LITERAL)                              \
  KEYWORD("\x74\x72\x79", Token::TRY)                                        \
  KEYWORD("\x74\x79\x70\x65\x6f\x66", Token::TYPEOF)                                  \
  KEYWORD_GROUP('\x76')                                                \
  KEYWORD("\x76\x61\x72", Token::VAR)                                        \
  KEYWORD("\x76\x6f\x69\x64", Token::VOID)                                      \
  KEYWORD_GROUP('\x77')                                                \
  KEYWORD("\x77\x68\x69\x6c\x65", Token::WHILE)                                    \
  KEYWORD("\x77\x69\x74\x68", Token::WITH)                                      \
  KEYWORD_GROUP('\x79')                                                \
  KEYWORD("\x79\x69\x65\x6c\x64", Token::YIELD)


static Token::Value KeywordOrIdentifierToken(const uint8_t* input,
                                             int input_length,
                                             bool harmony_scoping,
                                             bool harmony_modules) {
  DCHECK(input_length >= 1);
  const int kMinLength = 2;
  const int kMaxLength = 10;
  if (input_length < kMinLength || input_length > kMaxLength) {
    return Token::IDENTIFIER;
  }
  switch (input[0]) {
    default:
#define KEYWORD_GROUP_CASE(ch)                                \
      break;                                                  \
    case ch:
#define KEYWORD(keyword, token)                               \
    {                                                         \
      /* 'keyword' is a char array, so sizeof(keyword) is */  \
      /* strlen(keyword) plus 1 for the NUL char. */          \
      const int keyword_length = sizeof(keyword) - 1;         \
      STATIC_ASSERT(keyword_length >= kMinLength);            \
      STATIC_ASSERT(keyword_length <= kMaxLength);            \
      if (input_length == keyword_length &&                   \
          input[1] == keyword[1] &&                           \
          (keyword_length <= 2 || input[2] == keyword[2]) &&  \
          (keyword_length <= 3 || input[3] == keyword[3]) &&  \
          (keyword_length <= 4 || input[4] == keyword[4]) &&  \
          (keyword_length <= 5 || input[5] == keyword[5]) &&  \
          (keyword_length <= 6 || input[6] == keyword[6]) &&  \
          (keyword_length <= 7 || input[7] == keyword[7]) &&  \
          (keyword_length <= 8 || input[8] == keyword[8]) &&  \
          (keyword_length <= 9 || input[9] == keyword[9])) {  \
        return token;                                         \
      }                                                       \
    }
    KEYWORDS(KEYWORD_GROUP_CASE, KEYWORD)
  }
  return Token::IDENTIFIER;
}


bool Scanner::IdentifierIsFutureStrictReserved(
    const AstRawString* string) const {
  // Keywords are always 1-byte strings.
  return string->is_one_byte() &&
         Token::FUTURE_STRICT_RESERVED_WORD ==
             KeywordOrIdentifierToken(string->raw_data(), string->length(),
                                      harmony_scoping_, harmony_modules_);
}


Token::Value Scanner::ScanIdentifierOrKeyword() {
  DCHECK(unicode_cache_->IsIdentifierStart(c0_));
  LiteralScope literal(this);
  // Scan identifier start character.
  if (c0_ == '\x5c') {
    uc32 c = ScanIdentifierUnicodeEscape();
    // Only allow legal identifier start characters.
    if (c < 0 ||
        c == '\x5c' ||  // No recursive escapes.
        !unicode_cache_->IsIdentifierStart(c)) {
      return Token::ILLEGAL;
    }
    AddLiteralChar(c);
    return ScanIdentifierSuffix(&literal);
  }

  uc32 first_char = c0_;
  Advance();
  AddLiteralChar(first_char);

  // Scan the rest of the identifier characters.
  while (unicode_cache_->IsIdentifierPart(c0_)) {
    if (c0_ != '\x5c') {
      uc32 next_char = c0_;
      Advance();
      AddLiteralChar(next_char);
      continue;
    }
    // Fallthrough if no longer able to complete keyword.
    return ScanIdentifierSuffix(&literal);
  }

  literal.Complete();

  if (next_.literal_chars->is_one_byte()) {
    Vector<const uint8_t> chars = next_.literal_chars->one_byte_literal();
    return KeywordOrIdentifierToken(chars.start(),
                                    chars.length(),
                                    harmony_scoping_,
                                    harmony_modules_);
  }

  return Token::IDENTIFIER;
}


Token::Value Scanner::ScanIdentifierSuffix(LiteralScope* literal) {
  // Scan the rest of the identifier characters.
  while (unicode_cache_->IsIdentifierPart(c0_)) {
    if (c0_ == '\x5c') {
      uc32 c = ScanIdentifierUnicodeEscape();
      // Only allow legal identifier part characters.
      if (c < 0 ||
          c == '\x5c' ||
          !unicode_cache_->IsIdentifierPart(c)) {
        return Token::ILLEGAL;
      }
      AddLiteralChar(c);
    } else {
      AddLiteralChar(c0_);
      Advance();
    }
  }
  literal->Complete();

  return Token::IDENTIFIER;
}


bool Scanner::ScanRegExpPattern(bool seen_equal) {
  // Scan: ('/' | '/=') RegularExpressionBody '/' RegularExpressionFlags
  bool in_character_class = false;

  // Previous token is either '/' or '/=', in the second case, the
  // pattern starts at =.
  next_.location.beg_pos = source_pos() - (seen_equal ? 2 : 1);
  next_.location.end_pos = source_pos() - (seen_equal ? 1 : 0);

  // Scan regular expression body: According to ECMA-262, 3rd, 7.8.5,
  // the scanner should pass uninterpreted bodies to the RegExp
  // constructor.
  LiteralScope literal(this);
  if (seen_equal) {
    AddLiteralChar('\x3d');
  }

  while (c0_ != '\x2f' || in_character_class) {
    if (unicode_cache_->IsLineTerminator(c0_) || c0_ < 0) return false;
    if (c0_ == '\x5c') {  // Escape sequence.
      AddLiteralCharAdvance();
      if (unicode_cache_->IsLineTerminator(c0_) || c0_ < 0) return false;
      AddLiteralCharAdvance();
      // If the escape allows more characters, i.e., \x??, \u????, or \c?,
      // only "safe" characters are allowed (letters, digits, underscore),
      // otherwise the escape isn't valid and the invalid character has
      // its normal meaning. I.e., we can just continue scanning without
      // worrying whether the following characters are part of the escape
      // or not, since any '/', '\\' or '[' is guaranteed to not be part
      // of the escape sequence.

      // TODO(896): At some point, parse RegExps more throughly to capture
      // octal esacpes in strict mode.
    } else {  // Unescaped character.
      if (c0_ == '\x5b') in_character_class = true;
      if (c0_ == '\x5d') in_character_class = false;
      AddLiteralCharAdvance();
    }
  }
  Advance();  // consume '\x2f'

  literal.Complete();

  return true;
}


bool Scanner::ScanLiteralUnicodeEscape() {
  DCHECK(c0_ == '\x5c');
  uc32 chars_read[6] = {'\x5c', '\x75', 0, 0, 0, 0};
  Advance();
  int i = 1;
  if (c0_ == '\x75') {
    i++;
    while (i < 6) {
      Advance();
      if (!IsHexDigit(c0_)) break;
      chars_read[i] = c0_;
      i++;
    }
  }
  if (i < 6) {
    // Incomplete escape. Undo all advances and return false.
    while (i > 0) {
      i--;
      PushBack(chars_read[i]);
    }
    return false;
  }
  // Complete escape. Add all chars to current literal buffer.
  for (int i = 0; i < 6; i++) {
    AddLiteralChar(chars_read[i]);
  }
  return true;
}


bool Scanner::ScanRegExpFlags() {
  // Scan regular expression flags.
  LiteralScope literal(this);
  while (unicode_cache_->IsIdentifierPart(c0_)) {
    if (c0_ != '\x5c') {
      AddLiteralCharAdvance();
    } else {
      if (!ScanLiteralUnicodeEscape()) {
        break;
      }
      Advance();
    }
  }
  literal.Complete();

  next_.location.end_pos = source_pos() - 1;
  return true;
}


const AstRawString* Scanner::CurrentSymbol(AstValueFactory* ast_value_factory) {
  if (is_literal_one_byte()) {
    return ast_value_factory->GetOneByteString(literal_one_byte_string());
  }
  return ast_value_factory->GetTwoByteString(literal_two_byte_string());
}


const AstRawString* Scanner::NextSymbol(AstValueFactory* ast_value_factory) {
  if (is_next_literal_one_byte()) {
    return ast_value_factory->GetOneByteString(next_literal_one_byte_string());
  }
  return ast_value_factory->GetTwoByteString(next_literal_two_byte_string());
}


double Scanner::DoubleValue() {
  DCHECK(is_literal_one_byte());
  return StringToDouble(
      unicode_cache_,
      literal_one_byte_string(),
      ALLOW_HEX | ALLOW_OCTAL | ALLOW_IMPLICIT_OCTAL | ALLOW_BINARY);
}


int Scanner::FindNumber(DuplicateFinder* finder, int value) {
  return finder->AddNumber(literal_one_byte_string(), value);
}


int Scanner::FindSymbol(DuplicateFinder* finder, int value) {
  if (is_literal_one_byte()) {
    return finder->AddOneByteSymbol(literal_one_byte_string(), value);
  }
  return finder->AddTwoByteSymbol(literal_two_byte_string(), value);
}


int DuplicateFinder::AddOneByteSymbol(Vector<const uint8_t> key, int value) {
  return AddSymbol(key, true, value);
}


int DuplicateFinder::AddTwoByteSymbol(Vector<const uint16_t> key, int value) {
  return AddSymbol(Vector<const uint8_t>::cast(key), false, value);
}


int DuplicateFinder::AddSymbol(Vector<const uint8_t> key,
                               bool is_one_byte,
                               int value) {
  uint32_t hash = Hash(key, is_one_byte);
  byte* encoding = BackupKey(key, is_one_byte);
  HashMap::Entry* entry = map_.Lookup(encoding, hash, true);
  int old_value = static_cast<int>(reinterpret_cast<intptr_t>(entry->value));
  entry->value =
    reinterpret_cast<void*>(static_cast<intptr_t>(value | old_value));
  return old_value;
}


int DuplicateFinder::AddNumber(Vector<const uint8_t> key, int value) {
  DCHECK(key.length() > 0);
  // Quick check for already being in canonical form.
  if (IsNumberCanonical(key)) {
    return AddOneByteSymbol(key, value);
  }

  int flags = ALLOW_HEX | ALLOW_OCTAL | ALLOW_IMPLICIT_OCTAL | ALLOW_BINARY;
  double double_value = StringToDouble(
      unicode_constants_, key, flags, 0.0);
  int length;
  const char* string;
  if (!isfinite(double_value)) {
    string = "\x49\x6e\x66\x69\x6e\x69\x74\x79";
    length = 8;  // strlen("\x49\x6e\x66\x69\x6e\x69\x74\x79");
  } else {
    string = DoubleToCString(double_value,
                             Vector<char>(number_buffer_, kBufferSize));
    length = StrLength(string);
  }
  return AddSymbol(Vector<const byte>(reinterpret_cast<const byte*>(string),
                                      length), true, value);
}


bool DuplicateFinder::IsNumberCanonical(Vector<const uint8_t> number) {
  // Test for a safe approximation of number literals that are already
  // in canonical form: max 15 digits, no leading zeroes, except an
  // integer part that is a single zero, and no trailing zeros below
  // the decimal point.
  int pos = 0;
  int length = number.length();
  if (number.length() > 15) return false;
  if (number[pos] == '\x30') {
    pos++;
  } else {
    while (pos < length &&
           static_cast<unsigned>(number[pos] - '\x30') <= ('\x39' - '\x30')) pos++;
  }
  if (length == pos) return true;
  if (number[pos] != '\x2e') return false;
  pos++;
  bool invalid_last_digit = true;
  while (pos < length) {
    uint8_t digit = number[pos] - '\x30';
    if (digit > '\x39' - '\x30') return false;
    invalid_last_digit = (digit == 0);
    pos++;
  }
  return !invalid_last_digit;
}


uint32_t DuplicateFinder::Hash(Vector<const uint8_t> key, bool is_one_byte) {
  // Primitive hash function, almost identical to the one used
  // for strings (except that it's seeded by the length and ASCII-ness).
  int length = key.length();
  uint32_t hash = (length << 1) | (is_one_byte ? 1 : 0) ;
  for (int i = 0; i < length; i++) {
    uint32_t c = key[i];
    hash = (hash + c) * 1025;
    hash ^= (hash >> 6);
  }
  return hash;
}


bool DuplicateFinder::Match(void* first, void* second) {
  // Decode lengths.
  // Length + ASCII-bit is encoded as base 128, most significant heptet first,
  // with a 8th bit being non-zero while there are more heptets.
  // The value encodes the number of bytes following, and whether the original
  // was ASCII.
  byte* s1 = reinterpret_cast<byte*>(first);
  byte* s2 = reinterpret_cast<byte*>(second);
  uint32_t length_one_byte_field = 0;
  byte c1;
  do {
    c1 = *s1;
    if (c1 != *s2) return false;
    length_one_byte_field = (length_one_byte_field << 7) | (c1 & 0x7f);
    s1++;
    s2++;
  } while ((c1 & 0x80) != 0);
  int length = static_cast<int>(length_one_byte_field >> 1);
  return memcmp(s1, s2, length) == 0;
}


byte* DuplicateFinder::BackupKey(Vector<const uint8_t> bytes,
                                 bool is_one_byte) {
  uint32_t one_byte_length = (bytes.length() << 1) | (is_one_byte ? 1 : 0);
  backing_store_.StartSequence();
  // Emit one_byte_length as base-128 encoded number, with the 7th bit set
  // on the byte of every heptet except the last, least significant, one.
  if (one_byte_length >= (1 << 7)) {
    if (one_byte_length >= (1 << 14)) {
      if (one_byte_length >= (1 << 21)) {
        if (one_byte_length >= (1 << 28)) {
          backing_store_.Add(
              static_cast<uint8_t>((one_byte_length >> 28) | 0x80));
        }
        backing_store_.Add(
            static_cast<uint8_t>((one_byte_length >> 21) | 0x80u));
      }
      backing_store_.Add(
          static_cast<uint8_t>((one_byte_length >> 14) | 0x80u));
    }
    backing_store_.Add(static_cast<uint8_t>((one_byte_length >> 7) | 0x80u));
  }
  backing_store_.Add(static_cast<uint8_t>(one_byte_length & 0x7f));

  backing_store_.AddBlock(bytes);
  return backing_store_.EndSequence().start();
}
} }  // namespace v8::internal
