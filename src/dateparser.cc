// Copyright 2011 the V8 project authors. All rights reserved.
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.

#include "src/dateparser.h"

#include "src/char-predicates-inl.h"
#include "src/objects-inl.h"

namespace v8 {
namespace internal {

bool DateParser::DayComposer::Write(FixedArray* output) {
  if (index_ < 1) return false;
  // Day and month defaults to 1.
  while (index_ < kSize) {
    comp_[index_++] = 1;
  }

  int year = 0;  // Default year is 0 (=> 2000) for KJS compatibility.
  int month = kNone;
  int day = kNone;

  if (named_month_ == kNone) {
    if (is_iso_date_ || (index_ == 3 && !IsDay(comp_[0]))) {
      // YMD
      year = comp_[0];
      month = comp_[1];
      day = comp_[2];
    } else {
      // MD(Y)
      month = comp_[0];
      day = comp_[1];
      if (index_ == 3) year = comp_[2];
    }
  } else {
    month = named_month_;
    if (index_ == 1) {
      // MD or DM
      day = comp_[0];
    } else if (!IsDay(comp_[0])) {
      // YMD, MYD, or YDM
      year = comp_[0];
      day = comp_[1];
    } else {
      // DMY, MDY, or DYM
      day = comp_[0];
      year = comp_[1];
    }
  }

  if (!is_iso_date_) {
    if (Between(year, 0, 49)) year += 2000;
    else if (Between(year, 50, 99)) year += 1900;
  }

  if (!Smi::IsValid(year) || !IsMonth(month) || !IsDay(day)) return false;

  output->set(YEAR, Smi::FromInt(year));
  output->set(MONTH, Smi::FromInt(month - 1));  // 0-based
  output->set(DAY, Smi::FromInt(day));
  return true;
}


bool DateParser::TimeComposer::Write(FixedArray* output) {
  // All time slots default to 0
  while (index_ < kSize) {
    comp_[index_++] = 0;
  }

  int& hour = comp_[0];
  int& minute = comp_[1];
  int& second = comp_[2];
  int& millisecond = comp_[3];

  if (hour_offset_ != kNone) {
    if (!IsHour12(hour)) return false;
    hour %= 12;
    hour += hour_offset_;
  }

  if (!IsHour(hour) || !IsMinute(minute) ||
      !IsSecond(second) || !IsMillisecond(millisecond)) {
    // A 24th hour is allowed if minutes, seconds, and milliseconds are 0
    if (hour != 24 || minute != 0 || second != 0 || millisecond != 0) {
      return false;
    }
  }

  output->set(HOUR, Smi::FromInt(hour));
  output->set(MINUTE, Smi::FromInt(minute));
  output->set(SECOND, Smi::FromInt(second));
  output->set(MILLISECOND, Smi::FromInt(millisecond));
  return true;
}


bool DateParser::TimeZoneComposer::Write(FixedArray* output) {
  if (sign_ != kNone) {
    if (hour_ == kNone) hour_ = 0;
    if (minute_ == kNone) minute_ = 0;
    // Avoid signed integer overflow (undefined behavior) by doing unsigned
    // arithmetic.
    unsigned total_seconds_unsigned = hour_ * 3600U + minute_ * 60U;
    if (total_seconds_unsigned > Smi::kMaxValue) return false;
    int total_seconds = static_cast<int>(total_seconds_unsigned);
    if (sign_ < 0) {
      total_seconds = -total_seconds;
    }
    DCHECK(Smi::IsValid(total_seconds));
    output->set(UTC_OFFSET, Smi::FromInt(total_seconds));
  } else {
    output->set_null(UTC_OFFSET);
  }
  return true;
}

const int8_t DateParser::KeywordTable::
    array[][DateParser::KeywordTable::kEntrySize] = {
  {'\x6a', '\x61', '\x6e', DateParser::MONTH_NAME, 1},
  {'\x66', '\x65', '\x62', DateParser::MONTH_NAME, 2},
  {'\x6d', '\x61', '\x72', DateParser::MONTH_NAME, 3},
  {'\x61', '\x70', '\x72', DateParser::MONTH_NAME, 4},
  {'\x6d', '\x61', '\x79', DateParser::MONTH_NAME, 5},
  {'\x6a', '\x75', '\x6e', DateParser::MONTH_NAME, 6},
  {'\x6a', '\x75', '\x6c', DateParser::MONTH_NAME, 7},
  {'\x61', '\x75', '\x67', DateParser::MONTH_NAME, 8},
  {'\x73', '\x65', '\x70', DateParser::MONTH_NAME, 9},
  {'\x6f', '\x63', '\x74', DateParser::MONTH_NAME, 10},
  {'\x6e', '\x6f', '\x76', DateParser::MONTH_NAME, 11},
  {'\x64', '\x65', '\x63', DateParser::MONTH_NAME, 12},
  {'\x61', '\x6d', '\x0', DateParser::AM_PM, 0},
  {'\x70', '\x6d', '\x0', DateParser::AM_PM, 12},
  {'\x75', '\x74', '\x0', DateParser::TIME_ZONE_NAME, 0},
  {'\x75', '\x74', '\x63', DateParser::TIME_ZONE_NAME, 0},
  {'\x7a', '\x0', '\x0', DateParser::TIME_ZONE_NAME, 0},
  {'\x67', '\x6d', '\x74', DateParser::TIME_ZONE_NAME, 0},
  {'\x63', '\x64', '\x74', DateParser::TIME_ZONE_NAME, -5},
  {'\x63', '\x73', '\x74', DateParser::TIME_ZONE_NAME, -6},
  {'\x65', '\x64', '\x74', DateParser::TIME_ZONE_NAME, -4},
  {'\x65', '\x73', '\x74', DateParser::TIME_ZONE_NAME, -5},
  {'\x6d', '\x64', '\x74', DateParser::TIME_ZONE_NAME, -6},
  {'\x6d', '\x73', '\x74', DateParser::TIME_ZONE_NAME, -7},
  {'\x70', '\x64', '\x74', DateParser::TIME_ZONE_NAME, -7},
  {'\x70', '\x73', '\x74', DateParser::TIME_ZONE_NAME, -8},
  {'\x74', '\x0', '\x0', DateParser::TIME_SEPARATOR, 0},
  {'\x0', '\x0', '\x0', DateParser::INVALID, 0},
};


// We could use perfect hashing here, but this is not a bottleneck.
int DateParser::KeywordTable::Lookup(const uint32_t* pre, int len) {
  int i;
  for (i = 0; array[i][kTypeOffset] != INVALID; i++) {
    int j = 0;
    while (j < kPrefixLength &&
           pre[j] == static_cast<uint32_t>(array[i][j])) {
      j++;
    }
    // Check if we have a match and the length is legal.
    // Word longer than keyword is only allowed for month names.
    if (j == kPrefixLength &&
        (len <= kPrefixLength || array[i][kTypeOffset] == MONTH_NAME)) {
      return i;
    }
  }
  return i;
}


int DateParser::ReadMilliseconds(DateToken token) {
  // Read first three significant digits of the original numeral,
  // as inferred from the value and the number of digits.
  // I.e., use the number of digits to see if there were
  // leading zeros.
  int number = token.number();
  int length = token.length();
  if (length < 3) {
    // Less than three digits. Multiply to put most significant digit
    // in hundreds position.
    if (length == 1) {
      number *= 100;
    } else if (length == 2) {
      number *= 10;
    }
  } else if (length > 3) {
    if (length > kMaxSignificantDigits) length = kMaxSignificantDigits;
    // More than three digits. Divide by 10^(length - 3) to get three
    // most significant digits.
    int factor = 1;
    do {
      DCHECK(factor <= 100000000);  // factor won't overflow.
      factor *= 10;
      length--;
    } while (length > 3);
    number /= factor;
  }
  return number;
}


}  // namespace internal
}  // namespace v8
