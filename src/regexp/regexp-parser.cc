// Copyright 2016 the V8 project authors. All rights reserved.
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.

#include "src/regexp/regexp-parser.h"

#include "src/char-predicates-inl.h"
#include "src/factory.h"
#include "src/isolate.h"
#include "src/objects-inl.h"
#include "src/ostreams.h"
#include "src/regexp/jsregexp.h"
#include "src/utils.h"

#ifdef V8_I18N_SUPPORT
#include "unicode/uset.h"
#endif  // V8_I18N_SUPPORT

namespace v8 {
namespace internal {

RegExpParser::RegExpParser(FlatStringReader* in, Handle<String>* error,
                           JSRegExp::Flags flags, Isolate* isolate, Zone* zone)
    : isolate_(isolate),
      zone_(zone),
      error_(error),
      captures_(NULL),
      in_(in),
      current_(kEndMarker),
      ignore_case_(flags & JSRegExp::kIgnoreCase),
      multiline_(flags & JSRegExp::kMultiline),
      unicode_(flags & JSRegExp::kUnicode),
      next_pos_(0),
      captures_started_(0),
      capture_count_(0),
      has_more_(true),
      simple_(false),
      contains_anchor_(false),
      is_scanned_for_captures_(false),
      failed_(false) {
  Advance();
}

template <bool update_position>
inline uc32 RegExpParser::ReadNext() {
  int position = next_pos_;
  uc32 c0 = in()->Get(position);
  position++;
  // Read the whole surrogate pair in case of unicode flag, if possible.
  if (unicode() && position < in()->length() &&
      unibrow::Utf16::IsLeadSurrogate(static_cast<uc16>(c0))) {
    uc16 c1 = in()->Get(position);
    if (unibrow::Utf16::IsTrailSurrogate(c1)) {
      c0 = unibrow::Utf16::CombineSurrogatePair(static_cast<uc16>(c0), c1);
      position++;
    }
  }
  if (update_position) next_pos_ = position;
  return c0;
}


uc32 RegExpParser::Next() {
  if (has_next()) {
    return ReadNext<false>();
  } else {
    return kEndMarker;
  }
}


void RegExpParser::Advance() {
  if (has_next()) {
    StackLimitCheck check(isolate());
    if (check.HasOverflowed()) {
      ReportError(CStrVector(Isolate::kStackOverflowMessage));
    } else if (zone()->excess_allocation()) {
      ReportError(CStrVector(u8"Regular expression too large"));
    } else {
      current_ = ReadNext<true>();
    }
  } else {
    current_ = kEndMarker;
    // Advance so that position() points to 1-after-the-last-character. This is
    // important so that Reset() to this position works correctly.
    next_pos_ = in()->length() + 1;
    has_more_ = false;
  }
}


void RegExpParser::Reset(int pos) {
  next_pos_ = pos;
  has_more_ = (pos < in()->length());
  Advance();
}


void RegExpParser::Advance(int dist) {
  next_pos_ += dist - 1;
  Advance();
}


bool RegExpParser::simple() { return simple_; }

bool RegExpParser::IsSyntaxCharacterOrSlash(uc32 c) {
  switch (c) {
    case '\x5e':
    case '\x24':
    case '\x5c':
    case '\x2e':
    case '\x2a':
    case '\x2b':
    case '\x3f':
    case '\x28':
    case '\x29':
    case '\x5b':
    case '\x5d':
    case '\x7b':
    case '\x7d':
    case '\x7c':
    case '\x2f':
      return true;
    default:
      break;
  }
  return false;
}


RegExpTree* RegExpParser::ReportError(Vector<const char> message) {
  failed_ = true;
  *error_ = isolate()->factory()->NewStringFromAscii(message).ToHandleChecked();
  // Zip to the end to make sure the no more input is read.
  current_ = kEndMarker;
  next_pos_ = in()->length();
  return NULL;
}


#define CHECK_FAILED /**/); \
  if (failed_) return NULL; \
  ((void)0


// Pattern ::
//   Disjunction
RegExpTree* RegExpParser::ParsePattern() {
  RegExpTree* result = ParseDisjunction(CHECK_FAILED);
  DCHECK(!has_more());
  // If the result of parsing is a literal string atom, and it has the
  // same length as the input, then the atom is identical to the input.
  if (result->IsAtom() && result->AsAtom()->length() == in()->length()) {
    simple_ = true;
  }
  return result;
}


// Disjunction ::
//   Alternative
//   Alternative | Disjunction
// Alternative ::
//   [empty]
//   Term Alternative
// Term ::
//   Assertion
//   Atom
//   Atom Quantifier
RegExpTree* RegExpParser::ParseDisjunction() {
  // Used to store current state while parsing subexpressions.
  RegExpParserState initial_state(NULL, INITIAL, RegExpLookaround::LOOKAHEAD, 0,
                                  ignore_case(), unicode(), zone());
  RegExpParserState* state = &initial_state;
  // Cache the builder in a local variable for quick access.
  RegExpBuilder* builder = initial_state.builder();
  while (true) {
    switch (current()) {
      case kEndMarker:
        if (state->IsSubexpression()) {
          // Inside a parenthesized group when hitting end of input.
          return ReportError(CStrVector(u8"Unterminated group"));
        }
        DCHECK_EQ(INITIAL, state->group_type());
        // Parsing completed successfully.
        return builder->ToRegExp();
      case '\x29': {
        if (!state->IsSubexpression()) {
          return ReportError(CStrVector(u8"Unmatched '\x29'"));
        }
        DCHECK_NE(INITIAL, state->group_type());

        Advance();
        // End disjunction parsing and convert builder content to new single
        // regexp atom.
        RegExpTree* body = builder->ToRegExp();

        int end_capture_index = captures_started();

        int capture_index = state->capture_index();
        SubexpressionType group_type = state->group_type();

        // Build result of subexpression.
        if (group_type == CAPTURE) {
          RegExpCapture* capture = GetCapture(capture_index);
          capture->set_body(body);
          body = capture;
        } else if (group_type != GROUPING) {
          DCHECK(group_type == POSITIVE_LOOKAROUND ||
                 group_type == NEGATIVE_LOOKAROUND);
          bool is_positive = (group_type == POSITIVE_LOOKAROUND);
          body = new (zone()) RegExpLookaround(
              body, is_positive, end_capture_index - capture_index,
              capture_index, state->lookaround_type());
        }

        // Restore previous state.
        state = state->previous_state();
        builder = state->builder();

        builder->AddAtom(body);
        // For compatability with JSC and ES3, we allow quantifiers after
        // lookaheads, and break in all cases.
        break;
      }
      case '\x7c': {
        Advance();
        builder->NewAlternative();
        continue;
      }
      case '\x2a':
      case '\x2b':
      case '\x3f':
        return ReportError(CStrVector(u8"Nothing to repeat"));
      case '\x5e': {
        Advance();
        if (multiline()) {
          builder->AddAssertion(
              new (zone()) RegExpAssertion(RegExpAssertion::START_OF_LINE));
        } else {
          builder->AddAssertion(
              new (zone()) RegExpAssertion(RegExpAssertion::START_OF_INPUT));
          set_contains_anchor();
        }
        continue;
      }
      case '\x24': {
        Advance();
        RegExpAssertion::AssertionType assertion_type =
            multiline() ? RegExpAssertion::END_OF_LINE
                        : RegExpAssertion::END_OF_INPUT;
        builder->AddAssertion(new (zone()) RegExpAssertion(assertion_type));
        continue;
      }
      case '\x2e': {
        Advance();
        // everything except \x0a, \x0d, \u2028 and \u2029
        ZoneList<CharacterRange>* ranges =
            new (zone()) ZoneList<CharacterRange>(2, zone());
        CharacterRange::AddClassEscape('\x2e', ranges, zone());
        RegExpCharacterClass* cc =
            new (zone()) RegExpCharacterClass(ranges, false);
        builder->AddCharacterClass(cc);
        break;
      }
      case '\x28': {
        SubexpressionType subexpr_type = CAPTURE;
        RegExpLookaround::Type lookaround_type = state->lookaround_type();
        Advance();
        if (current() == '\x3f') {
          switch (Next()) {
            case '\x3a':
              subexpr_type = GROUPING;
              break;
            case '\x3d':
              lookaround_type = RegExpLookaround::LOOKAHEAD;
              subexpr_type = POSITIVE_LOOKAROUND;
              break;
            case '\x21':
              lookaround_type = RegExpLookaround::LOOKAHEAD;
              subexpr_type = NEGATIVE_LOOKAROUND;
              break;
            case '\x3c':
              if (FLAG_harmony_regexp_lookbehind) {
                Advance();
                lookaround_type = RegExpLookaround::LOOKBEHIND;
                if (Next() == '\x3d') {
                  subexpr_type = POSITIVE_LOOKAROUND;
                  break;
                } else if (Next() == '\x21') {
                  subexpr_type = NEGATIVE_LOOKAROUND;
                  break;
                }
              }
            // Fall through.
            default:
              return ReportError(CStrVector(u8"Invalid group"));
          }
          Advance(2);
        } else {
          if (captures_started_ >= kMaxCaptures) {
            return ReportError(CStrVector(u8"Too many captures"));
          }
          captures_started_++;
        }
        // Store current state and begin new disjunction parsing.
        state = new (zone()) RegExpParserState(
            state, subexpr_type, lookaround_type, captures_started_,
            ignore_case(), unicode(), zone());
        builder = state->builder();
        continue;
      }
      case '\x5b': {
        RegExpTree* cc = ParseCharacterClass(CHECK_FAILED);
        builder->AddCharacterClass(cc->AsCharacterClass());
        break;
      }
      // Atom ::
      //   \ AtomEscape
      case '\x5c':
        switch (Next()) {
          case kEndMarker:
            return ReportError(CStrVector(u8"\\ at end of pattern"));
          case '\x62':
            Advance(2);
            builder->AddAssertion(
                new (zone()) RegExpAssertion(RegExpAssertion::BOUNDARY));
            continue;
          case '\x42':
            Advance(2);
            builder->AddAssertion(
                new (zone()) RegExpAssertion(RegExpAssertion::NON_BOUNDARY));
            continue;
          // AtomEscape ::
          //   CharacterClassEscape
          //
          // CharacterClassEscape :: one of
          //   d D s S w W
          case '\x64':
          case '\x44':
          case '\x73':
          case '\x53':
          case '\x77':
          case '\x57': {
            uc32 c = Next();
            Advance(2);
            ZoneList<CharacterRange>* ranges =
                new (zone()) ZoneList<CharacterRange>(2, zone());
            CharacterRange::AddClassEscape(c, ranges, zone());
            RegExpCharacterClass* cc =
                new (zone()) RegExpCharacterClass(ranges, false);
            builder->AddCharacterClass(cc);
            break;
          }
          case '\x70':
          case '\x50': {
            uc32 p = Next();
            Advance(2);
            if (unicode()) {
              if (FLAG_harmony_regexp_property) {
                ZoneList<CharacterRange>* ranges =
                    new (zone()) ZoneList<CharacterRange>(2, zone());
                if (!ParsePropertyClass(ranges)) {
                  return ReportError(CStrVector(u8"Invalid property name"));
                }
                RegExpCharacterClass* cc =
                    new (zone()) RegExpCharacterClass(ranges, p == '\x50');
                builder->AddCharacterClass(cc);
              } else {
                // With /u, no identity escapes except for syntax characters
                // are allowed. Otherwise, all identity escapes are allowed.
                return ReportError(CStrVector(u8"Invalid escape"));
              }
            } else {
              builder->AddCharacter(p);
            }
            break;
          }
          case '\x31':
          case '\x32':
          case '\x33':
          case '\x34':
          case '\x35':
          case '\x36':
          case '\x37':
          case '\x38':
          case '\x39': {
            int index = 0;
            bool is_backref = ParseBackReferenceIndex(&index CHECK_FAILED);
            if (is_backref) {
              if (state->IsInsideCaptureGroup(index)) {
                // The back reference is inside the capture group it refers to.
                // Nothing can possibly have been captured yet, so we use empty
                // instead. This ensures that, when checking a back reference,
                // the capture registers of the referenced capture are either
                // both set or both cleared.
                builder->AddEmpty();
              } else {
                RegExpCapture* capture = GetCapture(index);
                RegExpTree* atom = new (zone()) RegExpBackReference(capture);
                builder->AddAtom(atom);
              }
              break;
            }
            // With /u, no identity escapes except for syntax characters
            // are allowed. Otherwise, all identity escapes are allowed.
            if (unicode()) {
              return ReportError(CStrVector(u8"Invalid escape"));
            }
            uc32 first_digit = Next();
            if (first_digit == '\x38' || first_digit == '\x39') {
              builder->AddCharacter(first_digit);
              Advance(2);
              break;
            }
          }
          // FALLTHROUGH
          case '\x30': {
            Advance();
            if (unicode() && Next() >= '\x30' && Next() <= '\x39') {
              // With /u, decimal escape with leading 0 are not parsed as octal.
              return ReportError(CStrVector(u8"Invalid decimal escape"));
            }
            uc32 octal = ParseOctalLiteral();
            builder->AddCharacter(octal);
            break;
          }
          // ControlEscape :: one of
          //   f n r t v
          case '\x66':
            Advance(2);
            builder->AddCharacter('\xc');
            break;
          case '\x6e':
            Advance(2);
            builder->AddCharacter('\xa');
            break;
          case '\x72':
            Advance(2);
            builder->AddCharacter('\xd');
            break;
          case '\x74':
            Advance(2);
            builder->AddCharacter('\x9');
            break;
          case '\x76':
            Advance(2);
            builder->AddCharacter('\xb');
            break;
          case '\x63': {
            Advance();
            uc32 controlLetter = Next();
            // Special case if it is an ASCII letter.
            // Convert lower case letters to uppercase.
            uc32 letter = controlLetter & ~('\x61' ^ '\x41');
            if (letter < '\x41' || '\x5a' < letter) {
              // controlLetter is not in range 'A'-'Z' or 'a'-'z'.
              // This is outside the specification. We match JSC in
              // reading the backslash as a literal character instead
              // of as starting an escape.
              if (unicode()) {
                // With /u, invalid escapes are not treated as identity escapes.
                return ReportError(CStrVector(u8"Invalid unicode escape"));
              }
              builder->AddCharacter('\x5c');
            } else {
              Advance(2);
              builder->AddCharacter(controlLetter & 0x1f);
            }
            break;
          }
          case '\x78': {
            Advance(2);
            uc32 value;
            if (ParseHexEscape(2, &value)) {
              builder->AddCharacter(value);
            } else if (!unicode()) {
              builder->AddCharacter('\x78');
            } else {
              // With /u, invalid escapes are not treated as identity escapes.
              return ReportError(CStrVector(u8"Invalid escape"));
            }
            break;
          }
          case '\x75': {
            Advance(2);
            uc32 value;
            if (ParseUnicodeEscape(&value)) {
              builder->AddEscapedUnicodeCharacter(value);
            } else if (!unicode()) {
              builder->AddCharacter('\x75');
            } else {
              // With /u, invalid escapes are not treated as identity escapes.
              return ReportError(CStrVector(u8"Invalid unicode escape"));
            }
            break;
          }
          default:
            Advance();
            // With /u, no identity escapes except for syntax characters
            // are allowed. Otherwise, all identity escapes are allowed.
            if (!unicode() || IsSyntaxCharacterOrSlash(current())) {
              builder->AddCharacter(current());
              Advance();
            } else {
              return ReportError(CStrVector(u8"Invalid escape"));
            }
            break;
        }
        break;
      case '\x7b': {
        int dummy;
        if (ParseIntervalQuantifier(&dummy, &dummy)) {
          return ReportError(CStrVector(u8"Nothing to repeat"));
        }
        // fallthrough
      }
      case '\x7d':
      case '\x5d':
        if (unicode()) {
          return ReportError(CStrVector(u8"Lone quantifier brackets"));
        }
      // fallthrough
      default:
        builder->AddUnicodeCharacter(current());
        Advance();
        break;
    }  // end switch(current())

    int min;
    int max;
    switch (current()) {
      // QuantifierPrefix ::
      //   *
      //   +
      //   ?
      //   {
      case '\x2a':
        min = 0;
        max = RegExpTree::kInfinity;
        Advance();
        break;
      case '\x2b':
        min = 1;
        max = RegExpTree::kInfinity;
        Advance();
        break;
      case '\x3f':
        min = 0;
        max = 1;
        Advance();
        break;
      case '\x7b':
        if (ParseIntervalQuantifier(&min, &max)) {
          if (max < min) {
            return ReportError(
                CStrVector(u8"numbers out of order in {} quantifier"));
          }
          break;
        } else if (unicode()) {
          // With /u, incomplete quantifiers are not allowed.
          return ReportError(CStrVector(u8"Incomplete quantifier"));
        }
        continue;
      default:
        continue;
    }
    RegExpQuantifier::QuantifierType quantifier_type = RegExpQuantifier::GREEDY;
    if (current() == '\x3f') {
      quantifier_type = RegExpQuantifier::NON_GREEDY;
      Advance();
    } else if (FLAG_regexp_possessive_quantifier && current() == '\x2b') {
      // FLAG_regexp_possessive_quantifier is a debug-only flag.
      quantifier_type = RegExpQuantifier::POSSESSIVE;
      Advance();
    }
    if (!builder->AddQuantifierToAtom(min, max, quantifier_type)) {
      return ReportError(CStrVector(u8"Invalid quantifier"));
    }
  }
}


#ifdef DEBUG
// Currently only used in an DCHECK.
static bool IsSpecialClassEscape(uc32 c) {
  switch (c) {
    case '\x64':
    case '\x44':
    case '\x73':
    case '\x53':
    case '\x77':
    case '\x57':
      return true;
    default:
      return false;
  }
}
#endif


// In order to know whether an escape is a backreference or not we have to scan
// the entire regexp and find the number of capturing parentheses.  However we
// don't want to scan the regexp twice unless it is necessary.  This mini-parser
// is called when needed.  It can see the difference between capturing and
// noncapturing parentheses and can skip character classes and backslash-escaped
// characters.
void RegExpParser::ScanForCaptures() {
  // Start with captures started previous to current position
  int capture_count = captures_started();
  // Add count of captures after this position.
  int n;
  while ((n = current()) != kEndMarker) {
    Advance();
    switch (n) {
      case '\x5c':
        Advance();
        break;
      case '\x5b': {
        int c;
        while ((c = current()) != kEndMarker) {
          Advance();
          if (c == '\x5c') {
            Advance();
          } else {
            if (c == '\x5d') break;
          }
        }
        break;
      }
      case '\x28':
        if (current() != '\x3f') capture_count++;
        break;
    }
  }
  capture_count_ = capture_count;
  is_scanned_for_captures_ = true;
}


bool RegExpParser::ParseBackReferenceIndex(int* index_out) {
  DCHECK_EQ('\x5c', current());
  DCHECK('\x31' <= Next() && Next() <= '\x39');
  // Try to parse a decimal literal that is no greater than the total number
  // of left capturing parentheses in the input.
  int start = position();
  int value = Next() - '\x30';
  Advance(2);
  while (true) {
    uc32 c = current();
    if (IsDecimalDigit(c)) {
      value = 10 * value + (c - '\x30');
      if (value > kMaxCaptures) {
        Reset(start);
        return false;
      }
      Advance();
    } else {
      break;
    }
  }
  if (value > captures_started()) {
    if (!is_scanned_for_captures_) {
      int saved_position = position();
      ScanForCaptures();
      Reset(saved_position);
    }
    if (value > capture_count_) {
      Reset(start);
      return false;
    }
  }
  *index_out = value;
  return true;
}


RegExpCapture* RegExpParser::GetCapture(int index) {
  // The index for the capture groups are one-based. Its index in the list is
  // zero-based.
  int know_captures =
      is_scanned_for_captures_ ? capture_count_ : captures_started_;
  DCHECK(index <= know_captures);
  if (captures_ == NULL) {
    captures_ = new (zone()) ZoneList<RegExpCapture*>(know_captures, zone());
  }
  while (captures_->length() < know_captures) {
    captures_->Add(new (zone()) RegExpCapture(captures_->length() + 1), zone());
  }
  return captures_->at(index - 1);
}


bool RegExpParser::RegExpParserState::IsInsideCaptureGroup(int index) {
  for (RegExpParserState* s = this; s != NULL; s = s->previous_state()) {
    if (s->group_type() != CAPTURE) continue;
    // Return true if we found the matching capture index.
    if (index == s->capture_index()) return true;
    // Abort if index is larger than what has been parsed up till this state.
    if (index > s->capture_index()) return false;
  }
  return false;
}


// QuantifierPrefix ::
//   { DecimalDigits }
//   { DecimalDigits , }
//   { DecimalDigits , DecimalDigits }
//
// Returns true if parsing succeeds, and set the min_out and max_out
// values. Values are truncated to RegExpTree::kInfinity if they overflow.
bool RegExpParser::ParseIntervalQuantifier(int* min_out, int* max_out) {
  DCHECK_EQ(current(), '\x7b');
  int start = position();
  Advance();
  int min = 0;
  if (!IsDecimalDigit(current())) {
    Reset(start);
    return false;
  }
  while (IsDecimalDigit(current())) {
    int next = current() - '\x30';
    if (min > (RegExpTree::kInfinity - next) / 10) {
      // Overflow. Skip past remaining decimal digits and return -1.
      do {
        Advance();
      } while (IsDecimalDigit(current()));
      min = RegExpTree::kInfinity;
      break;
    }
    min = 10 * min + next;
    Advance();
  }
  int max = 0;
  if (current() == '\x7d') {
    max = min;
    Advance();
  } else if (current() == '\x2c') {
    Advance();
    if (current() == '\x7d') {
      max = RegExpTree::kInfinity;
      Advance();
    } else {
      while (IsDecimalDigit(current())) {
        int next = current() - '\x30';
        if (max > (RegExpTree::kInfinity - next) / 10) {
          do {
            Advance();
          } while (IsDecimalDigit(current()));
          max = RegExpTree::kInfinity;
          break;
        }
        max = 10 * max + next;
        Advance();
      }
      if (current() != '\x7d') {
        Reset(start);
        return false;
      }
      Advance();
    }
  } else {
    Reset(start);
    return false;
  }
  *min_out = min;
  *max_out = max;
  return true;
}


uc32 RegExpParser::ParseOctalLiteral() {
  DCHECK(('\x30' <= current() && current() <= '\x37') || current() == kEndMarker);
  // For compatibility with some other browsers (not all), we parse
  // up to three octal digits with a value below 256.
  uc32 value = current() - '\x30';
  Advance();
  if ('\x30' <= current() && current() <= '\x37') {
    value = value * 8 + current() - '\x30';
    Advance();
    if (value < 32 && '\x30' <= current() && current() <= '\x37') {
      value = value * 8 + current() - '\x30';
      Advance();
    }
  }
  return value;
}


bool RegExpParser::ParseHexEscape(int length, uc32* value) {
  int start = position();
  uc32 val = 0;
  for (int i = 0; i < length; ++i) {
    uc32 c = current();
    int d = HexValue(c);
    if (d < 0) {
      Reset(start);
      return false;
    }
    val = val * 16 + d;
    Advance();
  }
  *value = val;
  return true;
}

// This parses RegExpUnicodeEscapeSequence as described in ECMA262.
bool RegExpParser::ParseUnicodeEscape(uc32* value) {
  // Accept both \uxxxx and \u{xxxxxx} (if harmony unicode escapes are
  // allowed). In the latter case, the number of hex digits between { } is
  // arbitrary. \ and u have already been read.
  if (current() == '\x7b' && unicode()) {
    int start = position();
    Advance();
    if (ParseUnlimitedLengthHexNumber(0x10ffff, value)) {
      if (current() == '\x7d') {
        Advance();
        return true;
      }
    }
    Reset(start);
    return false;
  }
  // \u but no {, or \u{...} escapes not allowed.
  bool result = ParseHexEscape(4, value);
  if (result && unicode() && unibrow::Utf16::IsLeadSurrogate(*value) &&
      current() == '\x5c') {
    // Attempt to read trail surrogate.
    int start = position();
    if (Next() == '\x75') {
      Advance(2);
      uc32 trail;
      if (ParseHexEscape(4, &trail) &&
          unibrow::Utf16::IsTrailSurrogate(trail)) {
        *value = unibrow::Utf16::CombineSurrogatePair(static_cast<uc16>(*value),
                                                      static_cast<uc16>(trail));
        return true;
      }
    }
    Reset(start);
  }
  return result;
}

#ifdef V8_I18N_SUPPORT
bool IsExactPropertyValueAlias(const char* property_name, UProperty property,
                               int32_t property_value) {
  const char* short_name =
      u_getPropertyValueName(property, property_value, U_SHORT_PROPERTY_NAME);
  if (short_name != NULL && strcmp(property_name, short_name) == 0) return true;
  for (int i = 0;; i++) {
    const char* long_name = u_getPropertyValueName(
        property, property_value,
        static_cast<UPropertyNameChoice>(U_LONG_PROPERTY_NAME + i));
    if (long_name == NULL) break;
    if (strcmp(property_name, long_name) == 0) return true;
  }
  return false;
}

bool LookupPropertyClass(UProperty property, const char* property_name,
                         ZoneList<CharacterRange>* result, Zone* zone) {
  int32_t property_value = u_getPropertyValueEnum(property, property_name);
  if (property_value == UCHAR_INVALID_CODE) return false;

  // We require the property name to match exactly to one of the property value
  // aliases. However, u_getPropertyValueEnum uses loose matching.
  if (!IsExactPropertyValueAlias(property_name, property, property_value)) {
    return false;
  }

  USet* set = uset_openEmpty();
  UErrorCode ec = U_ZERO_ERROR;
  uset_applyIntPropertyValue(set, property, property_value, &ec);
  bool success = ec == U_ZERO_ERROR && !uset_isEmpty(set);

  if (success) {
    uset_removeAllStrings(set);
    int item_count = uset_getItemCount(set);
    int item_result = 0;
    for (int i = 0; i < item_count; i++) {
      uc32 start = 0;
      uc32 end = 0;
      item_result += uset_getItem(set, i, &start, &end, nullptr, 0, &ec);
      result->Add(CharacterRange::Range(start, end), zone);
    }
    DCHECK_EQ(U_ZERO_ERROR, ec);
    DCHECK_EQ(0, item_result);
  }
  uset_close(set);
  return success;
}
#endif  // V8_I18N_SUPPORT

bool RegExpParser::ParsePropertyClass(ZoneList<CharacterRange>* result) {
#ifdef V8_I18N_SUPPORT
  List<char> property_name_list;
  if (current() == '\x7b') {
    for (Advance(); current() != '\x7d'; Advance()) {
      if (!has_next()) return false;
      property_name_list.Add(static_cast<char>(current()));
    }
  } else if (current() != kEndMarker) {
    property_name_list.Add(static_cast<char>(current()));
  } else {
    return false;
  }
  Advance();
  property_name_list.Add(0);  // null-terminate string.

  const char* property_name = property_name_list.ToConstVector().start();

#define PROPERTY_NAME_LOOKUP(PROPERTY)                                  \
  do {                                                                  \
    if (LookupPropertyClass(PROPERTY, property_name, result, zone())) { \
      return true;                                                      \
    }                                                                   \
  } while (false)

  // General_Category (gc) found in PropertyValueAliases.txt
  PROPERTY_NAME_LOOKUP(UCHAR_GENERAL_CATEGORY_MASK);
  // Script (sc) found in Scripts.txt
  PROPERTY_NAME_LOOKUP(UCHAR_SCRIPT);
  // To disambiguate from script names, block names have an "In"-prefix.
  if (property_name_list.length() > 3 && property_name[0] == '\x49' &&
      property_name[1] == '\x6e') {
    // Block (blk) found in Blocks.txt
    property_name += 2;
    PROPERTY_NAME_LOOKUP(UCHAR_BLOCK);
  }
#undef PROPERTY_NAME_LOOKUP
#endif  // V8_I18N_SUPPORT
  return false;
}

bool RegExpParser::ParseUnlimitedLengthHexNumber(int max_value, uc32* value) {
  uc32 x = 0;
  int d = HexValue(current());
  if (d < 0) {
    return false;
  }
  while (d >= 0) {
    x = x * 16 + d;
    if (x > max_value) {
      return false;
    }
    Advance();
    d = HexValue(current());
  }
  *value = x;
  return true;
}


uc32 RegExpParser::ParseClassCharacterEscape() {
  DCHECK(current() == '\x5c');
  DCHECK(has_next() && !IsSpecialClassEscape(Next()));
  Advance();
  switch (current()) {
    case '\x62':
      Advance();
      return '\x8';
    // ControlEscape :: one of
    //   f n r t v
    case '\x66':
      Advance();
      return '\xc';
    case '\x6e':
      Advance();
      return '\xa';
    case '\x72':
      Advance();
      return '\xd';
    case '\x74':
      Advance();
      return '\x9';
    case '\x76':
      Advance();
      return '\xb';
    case '\x63': {
      uc32 controlLetter = Next();
      uc32 letter = controlLetter & ~('\x41' ^ '\x61');
      // For compatibility with JSC, inside a character class. We also accept
      // digits and underscore as control characters, unless with /u.
      if (letter >= '\x41' && letter <= '\x5a') {
        Advance(2);
        // Control letters mapped to ASCII control characters in the range
        // 0x00-0x1f.
        return controlLetter & 0x1f;
      }
      if (unicode()) {
        // With /u, invalid escapes are not treated as identity escapes.
        ReportError(CStrVector(u8"Invalid class escape"));
        return 0;
      }
      if ((controlLetter >= '\x30' && controlLetter <= '\x39') ||
          controlLetter == '\x5f') {
        Advance(2);
        return controlLetter & 0x1f;
      }
      // We match JSC in reading the backslash as a literal
      // character instead of as starting an escape.
      return '\x5c';
    }
    case '\x30':
      // With /u, \0 is interpreted as NUL if not followed by another digit.
      if (unicode() && !(Next() >= '\x30' && Next() <= '\x39')) {
        Advance();
        return 0;
      }
    // Fall through.
    case '\x31':
    case '\x32':
    case '\x33':
    case '\x34':
    case '\x35':
    case '\x36':
    case '\x37':
      // For compatibility, we interpret a decimal escape that isn't
      // a back reference (and therefore either \0 or not valid according
      // to the specification) as a 1..3 digit octal character code.
      if (unicode()) {
        // With /u, decimal escape is not interpreted as octal character code.
        ReportError(CStrVector(u8"Invalid class escape"));
        return 0;
      }
      return ParseOctalLiteral();
    case '\x78': {
      Advance();
      uc32 value;
      if (ParseHexEscape(2, &value)) return value;
      if (unicode()) {
        // With /u, invalid escapes are not treated as identity escapes.
        ReportError(CStrVector(u8"Invalid escape"));
        return 0;
      }
      // If \x is not followed by a two-digit hexadecimal, treat it
      // as an identity escape.
      return '\x78';
    }
    case '\x75': {
      Advance();
      uc32 value;
      if (ParseUnicodeEscape(&value)) return value;
      if (unicode()) {
        // With /u, invalid escapes are not treated as identity escapes.
        ReportError(CStrVector(u8"Invalid unicode escape"));
        return 0;
      }
      // If \u is not followed by a two-digit hexadecimal, treat it
      // as an identity escape.
      return '\x75';
    }
    default: {
      uc32 result = current();
      // With /u, no identity escapes except for syntax characters and '-' are
      // allowed. Otherwise, all identity escapes are allowed.
      if (!unicode() || IsSyntaxCharacterOrSlash(result) || result == '\x2d') {
        Advance();
        return result;
      }
      ReportError(CStrVector(u8"Invalid escape"));
      return 0;
    }
  }
  return 0;
}


CharacterRange RegExpParser::ParseClassAtom(uc16* char_class) {
  DCHECK_EQ(0, *char_class);
  uc32 first = current();
  if (first == '\x5c') {
    switch (Next()) {
      case '\x77':
      case '\x57':
      case '\x64':
      case '\x44':
      case '\x73':
      case '\x53': {
        *char_class = Next();
        Advance(2);
        return CharacterRange::Singleton(0);  // Return dummy value.
      }
      case kEndMarker:
        return ReportError(CStrVector(u8"\\ at end of pattern"));
      default:
        first = ParseClassCharacterEscape(CHECK_FAILED);
    }
  } else {
    Advance();
  }

  return CharacterRange::Singleton(first);
}


static const uc16 kNoCharClass = 0;

// Adds range or pre-defined character class to character ranges.
// If char_class is not kInvalidClass, it's interpreted as a class
// escape (i.e., 's' means whitespace, from '\s').
static inline void AddRangeOrEscape(ZoneList<CharacterRange>* ranges,
                                    uc16 char_class, CharacterRange range,
                                    Zone* zone) {
  if (char_class != kNoCharClass) {
    CharacterRange::AddClassEscape(char_class, ranges, zone);
  } else {
    ranges->Add(range, zone);
  }
}

bool RegExpParser::ParseClassProperty(ZoneList<CharacterRange>* ranges) {
  if (!FLAG_harmony_regexp_property) return false;
  if (!unicode()) return false;
  if (current() != '\x5c') return false;
  uc32 next = Next();
  bool parse_success = false;
  if (next == '\x70') {
    Advance(2);
    parse_success = ParsePropertyClass(ranges);
  } else if (next == '\x50') {
    Advance(2);
    ZoneList<CharacterRange>* property_class =
        new (zone()) ZoneList<CharacterRange>(2, zone());
    parse_success = ParsePropertyClass(property_class);
    if (parse_success) {
      ZoneList<CharacterRange>* negated =
          new (zone()) ZoneList<CharacterRange>(2, zone());
      CharacterRange::Negate(property_class, negated, zone());
      const Vector<CharacterRange> negated_vector = negated->ToVector();
      ranges->AddAll(negated_vector, zone());
    }
  } else {
    return false;
  }
  if (!parse_success)
    ReportError(CStrVector(u8"Invalid property name in character class"));
  return parse_success;
}

RegExpTree* RegExpParser::ParseCharacterClass() {
  static const char* kUnterminated = u8"Unterminated character class";
  static const char* kRangeInvalid = u8"Invalid character class";
  static const char* kRangeOutOfOrder = u8"Range out of order in character class";

  DCHECK_EQ(current(), '\x5b');
  Advance();
  bool is_negated = false;
  if (current() == '\x5e') {
    is_negated = true;
    Advance();
  }
  ZoneList<CharacterRange>* ranges =
      new (zone()) ZoneList<CharacterRange>(2, zone());
  while (has_more() && current() != '\x5d') {
    bool parsed_property = ParseClassProperty(ranges CHECK_FAILED);
    if (parsed_property) continue;
    uc16 char_class = kNoCharClass;
    CharacterRange first = ParseClassAtom(&char_class CHECK_FAILED);
    if (current() == '\x2d') {
      Advance();
      if (current() == kEndMarker) {
        // If we reach the end we break out of the loop and let the
        // following code report an error.
        break;
      } else if (current() == '\x5d') {
        AddRangeOrEscape(ranges, char_class, first, zone());
        ranges->Add(CharacterRange::Singleton('\x2d'), zone());
        break;
      }
      uc16 char_class_2 = kNoCharClass;
      CharacterRange next = ParseClassAtom(&char_class_2 CHECK_FAILED);
      if (char_class != kNoCharClass || char_class_2 != kNoCharClass) {
        // Either end is an escaped character class. Treat the '-' verbatim.
        if (unicode()) {
          // ES2015 21.2.2.15.1 step 1.
          return ReportError(CStrVector(kRangeInvalid));
        }
        AddRangeOrEscape(ranges, char_class, first, zone());
        ranges->Add(CharacterRange::Singleton('\x2d'), zone());
        AddRangeOrEscape(ranges, char_class_2, next, zone());
        continue;
      }
      // ES2015 21.2.2.15.1 step 6.
      if (first.from() > next.to()) {
        return ReportError(CStrVector(kRangeOutOfOrder));
      }
      ranges->Add(CharacterRange::Range(first.from(), next.to()), zone());
    } else {
      AddRangeOrEscape(ranges, char_class, first, zone());
    }
  }
  if (!has_more()) {
    return ReportError(CStrVector(kUnterminated));
  }
  Advance();
  if (ranges->length() == 0) {
    ranges->Add(CharacterRange::Everything(), zone());
    is_negated = !is_negated;
  }
  return new (zone()) RegExpCharacterClass(ranges, is_negated);
}


#undef CHECK_FAILED


bool RegExpParser::ParseRegExp(Isolate* isolate, Zone* zone,
                               FlatStringReader* input, JSRegExp::Flags flags,
                               RegExpCompileData* result) {
  DCHECK(result != NULL);
  RegExpParser parser(input, &result->error, flags, isolate, zone);
  RegExpTree* tree = parser.ParsePattern();
  if (parser.failed()) {
    DCHECK(tree == NULL);
    DCHECK(!result->error.is_null());
  } else {
    DCHECK(tree != NULL);
    DCHECK(result->error.is_null());
    if (FLAG_trace_regexp_parser) {
      OFStream os(stdout);
      tree->Print(os, zone);
      os << u8"\n";
    }
    result->tree = tree;
    int capture_count = parser.captures_started();
    result->simple = tree->IsAtom() && parser.simple() && capture_count == 0;
    result->contains_anchor = parser.contains_anchor();
    result->capture_count = capture_count;
  }
  return !parser.failed();
}

RegExpBuilder::RegExpBuilder(Zone* zone, bool ignore_case, bool unicode)
    : zone_(zone),
      pending_empty_(false),
      ignore_case_(ignore_case),
      unicode_(unicode),
      characters_(NULL),
      pending_surrogate_(kNoPendingSurrogate),
      terms_(),
      alternatives_()
#ifdef DEBUG
      ,
      last_added_(ADD_NONE)
#endif
{
}


void RegExpBuilder::AddLeadSurrogate(uc16 lead_surrogate) {
  DCHECK(unibrow::Utf16::IsLeadSurrogate(lead_surrogate));
  FlushPendingSurrogate();
  // Hold onto the lead surrogate, waiting for a trail surrogate to follow.
  pending_surrogate_ = lead_surrogate;
}


void RegExpBuilder::AddTrailSurrogate(uc16 trail_surrogate) {
  DCHECK(unibrow::Utf16::IsTrailSurrogate(trail_surrogate));
  if (pending_surrogate_ != kNoPendingSurrogate) {
    uc16 lead_surrogate = pending_surrogate_;
    pending_surrogate_ = kNoPendingSurrogate;
    DCHECK(unibrow::Utf16::IsLeadSurrogate(lead_surrogate));
    uc32 combined =
        unibrow::Utf16::CombineSurrogatePair(lead_surrogate, trail_surrogate);
    if (NeedsDesugaringForIgnoreCase(combined)) {
      AddCharacterClassForDesugaring(combined);
    } else {
      ZoneList<uc16> surrogate_pair(2, zone());
      surrogate_pair.Add(lead_surrogate, zone());
      surrogate_pair.Add(trail_surrogate, zone());
      RegExpAtom* atom =
          new (zone()) RegExpAtom(surrogate_pair.ToConstVector());
      AddAtom(atom);
    }
  } else {
    pending_surrogate_ = trail_surrogate;
    FlushPendingSurrogate();
  }
}


void RegExpBuilder::FlushPendingSurrogate() {
  if (pending_surrogate_ != kNoPendingSurrogate) {
    DCHECK(unicode());
    uc32 c = pending_surrogate_;
    pending_surrogate_ = kNoPendingSurrogate;
    AddCharacterClassForDesugaring(c);
  }
}


void RegExpBuilder::FlushCharacters() {
  FlushPendingSurrogate();
  pending_empty_ = false;
  if (characters_ != NULL) {
    RegExpTree* atom = new (zone()) RegExpAtom(characters_->ToConstVector());
    characters_ = NULL;
    text_.Add(atom, zone());
    LAST(ADD_ATOM);
  }
}


void RegExpBuilder::FlushText() {
  FlushCharacters();
  int num_text = text_.length();
  if (num_text == 0) {
    return;
  } else if (num_text == 1) {
    terms_.Add(text_.last(), zone());
  } else {
    RegExpText* text = new (zone()) RegExpText(zone());
    for (int i = 0; i < num_text; i++) text_.Get(i)->AppendToText(text, zone());
    terms_.Add(text, zone());
  }
  text_.Clear();
}


void RegExpBuilder::AddCharacter(uc16 c) {
  FlushPendingSurrogate();
  pending_empty_ = false;
  if (NeedsDesugaringForIgnoreCase(c)) {
    AddCharacterClassForDesugaring(c);
  } else {
    if (characters_ == NULL) {
      characters_ = new (zone()) ZoneList<uc16>(4, zone());
    }
    characters_->Add(c, zone());
    LAST(ADD_CHAR);
  }
}


void RegExpBuilder::AddUnicodeCharacter(uc32 c) {
  if (c > unibrow::Utf16::kMaxNonSurrogateCharCode) {
    DCHECK(unicode());
    AddLeadSurrogate(unibrow::Utf16::LeadSurrogate(c));
    AddTrailSurrogate(unibrow::Utf16::TrailSurrogate(c));
  } else if (unicode() && unibrow::Utf16::IsLeadSurrogate(c)) {
    AddLeadSurrogate(c);
  } else if (unicode() && unibrow::Utf16::IsTrailSurrogate(c)) {
    AddTrailSurrogate(c);
  } else {
    AddCharacter(static_cast<uc16>(c));
  }
}

void RegExpBuilder::AddEscapedUnicodeCharacter(uc32 character) {
  // A lead or trail surrogate parsed via escape sequence will not
  // pair up with any preceding lead or following trail surrogate.
  FlushPendingSurrogate();
  AddUnicodeCharacter(character);
  FlushPendingSurrogate();
}

void RegExpBuilder::AddEmpty() { pending_empty_ = true; }


void RegExpBuilder::AddCharacterClass(RegExpCharacterClass* cc) {
  if (NeedsDesugaringForUnicode(cc)) {
    // With /u, character class needs to be desugared, so it
    // must be a standalone term instead of being part of a RegExpText.
    AddTerm(cc);
  } else {
    AddAtom(cc);
  }
}

void RegExpBuilder::AddCharacterClassForDesugaring(uc32 c) {
  AddTerm(new (zone()) RegExpCharacterClass(
      CharacterRange::List(zone(), CharacterRange::Singleton(c)), false));
}


void RegExpBuilder::AddAtom(RegExpTree* term) {
  if (term->IsEmpty()) {
    AddEmpty();
    return;
  }
  if (term->IsTextElement()) {
    FlushCharacters();
    text_.Add(term, zone());
  } else {
    FlushText();
    terms_.Add(term, zone());
  }
  LAST(ADD_ATOM);
}


void RegExpBuilder::AddTerm(RegExpTree* term) {
  FlushText();
  terms_.Add(term, zone());
  LAST(ADD_ATOM);
}


void RegExpBuilder::AddAssertion(RegExpTree* assert) {
  FlushText();
  terms_.Add(assert, zone());
  LAST(ADD_ASSERT);
}


void RegExpBuilder::NewAlternative() { FlushTerms(); }


void RegExpBuilder::FlushTerms() {
  FlushText();
  int num_terms = terms_.length();
  RegExpTree* alternative;
  if (num_terms == 0) {
    alternative = new (zone()) RegExpEmpty();
  } else if (num_terms == 1) {
    alternative = terms_.last();
  } else {
    alternative = new (zone()) RegExpAlternative(terms_.GetList(zone()));
  }
  alternatives_.Add(alternative, zone());
  terms_.Clear();
  LAST(ADD_NONE);
}


bool RegExpBuilder::NeedsDesugaringForUnicode(RegExpCharacterClass* cc) {
  if (!unicode()) return false;
  // TODO(yangguo): we could be smarter than this. Case-insensitivity does not
  // necessarily mean that we need to desugar. It's probably nicer to have a
  // separate pass to figure out unicode desugarings.
  if (ignore_case()) return true;
  ZoneList<CharacterRange>* ranges = cc->ranges(zone());
  CharacterRange::Canonicalize(ranges);
  for (int i = ranges->length() - 1; i >= 0; i--) {
    uc32 from = ranges->at(i).from();
    uc32 to = ranges->at(i).to();
    // Check for non-BMP characters.
    if (to >= kNonBmpStart) return true;
    // Check for lone surrogates.
    if (from <= kTrailSurrogateEnd && to >= kLeadSurrogateStart) return true;
  }
  return false;
}


bool RegExpBuilder::NeedsDesugaringForIgnoreCase(uc32 c) {
#ifdef V8_I18N_SUPPORT
  if (unicode() && ignore_case()) {
    USet* set = uset_open(c, c);
    uset_closeOver(set, USET_CASE_INSENSITIVE);
    uset_removeAllStrings(set);
    bool result = uset_size(set) > 1;
    uset_close(set);
    return result;
  }
  // In the case where ICU is not included, we act as if the unicode flag is
  // not set, and do not desugar.
#endif  // V8_I18N_SUPPORT
  return false;
}


RegExpTree* RegExpBuilder::ToRegExp() {
  FlushTerms();
  int num_alternatives = alternatives_.length();
  if (num_alternatives == 0) return new (zone()) RegExpEmpty();
  if (num_alternatives == 1) return alternatives_.last();
  return new (zone()) RegExpDisjunction(alternatives_.GetList(zone()));
}

bool RegExpBuilder::AddQuantifierToAtom(
    int min, int max, RegExpQuantifier::QuantifierType quantifier_type) {
  FlushPendingSurrogate();
  if (pending_empty_) {
    pending_empty_ = false;
    return true;
  }
  RegExpTree* atom;
  if (characters_ != NULL) {
    DCHECK(last_added_ == ADD_CHAR);
    // Last atom was character.
    Vector<const uc16> char_vector = characters_->ToConstVector();
    int num_chars = char_vector.length();
    if (num_chars > 1) {
      Vector<const uc16> prefix = char_vector.SubVector(0, num_chars - 1);
      text_.Add(new (zone()) RegExpAtom(prefix), zone());
      char_vector = char_vector.SubVector(num_chars - 1, num_chars);
    }
    characters_ = NULL;
    atom = new (zone()) RegExpAtom(char_vector);
    FlushText();
  } else if (text_.length() > 0) {
    DCHECK(last_added_ == ADD_ATOM);
    atom = text_.RemoveLast();
    FlushText();
  } else if (terms_.length() > 0) {
    DCHECK(last_added_ == ADD_ATOM);
    atom = terms_.RemoveLast();
    // With /u, lookarounds are not quantifiable.
    if (unicode() && atom->IsLookaround()) return false;
    if (atom->max_match() == 0) {
      // Guaranteed to only match an empty string.
      LAST(ADD_TERM);
      if (min == 0) {
        return true;
      }
      terms_.Add(atom, zone());
      return true;
    }
  } else {
    // Only call immediately after adding an atom or character!
    UNREACHABLE();
    return false;
  }
  terms_.Add(new (zone()) RegExpQuantifier(min, max, quantifier_type, atom),
             zone());
  LAST(ADD_TERM);
  return true;
}

}  // namespace internal
}  // namespace v8
