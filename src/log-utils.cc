// Copyright 2009 the V8 project authors. All rights reserved.
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.

#include "src/log-utils.h"

#include "src/assert-scope.h"
#include "src/base/platform/platform.h"
#include "src/objects-inl.h"
#include "src/string-stream.h"
#include "src/utils.h"
#include "src/version.h"

namespace v8 {
namespace internal {


const char* const Log::kLogToTemporaryFile = u8"&";
const char* const Log::kLogToConsole = u8"-";


Log::Log(Logger* logger)
  : is_stopped_(false),
    output_handle_(NULL),
    message_buffer_(NULL),
    logger_(logger) {
}


void Log::Initialize(const char* log_file_name) {
  message_buffer_ = NewArray<char>(kMessageBufferSize);

  // --log-all enables all the log flags.
  if (FLAG_log_all) {
    FLAG_log_api = true;
    FLAG_log_code = true;
    FLAG_log_gc = true;
    FLAG_log_suspect = true;
    FLAG_log_handles = true;
    FLAG_log_regexp = true;
    FLAG_log_internal_timer_events = true;
  }

  // --prof implies --log-code.
  if (FLAG_prof) FLAG_log_code = true;

  // If we're logging anything, we need to open the log file.
  if (Log::InitLogAtStart()) {
    if (strcmp(log_file_name, kLogToConsole) == 0) {
      OpenStdout();
    } else if (strcmp(log_file_name, kLogToTemporaryFile) == 0) {
      OpenTemporaryFile();
    } else {
      OpenFile(log_file_name);
    }

    if (output_handle_ != nullptr) {
      Log::MessageBuilder msg(this);
      msg.Append(u8"v8-version,%d,%d,%d,%d,%d", Version::GetMajor(),
                 Version::GetMinor(), Version::GetBuild(), Version::GetPatch(),
                 Version::IsCandidate());
      msg.WriteToLogFile();
    }
  }
}


void Log::OpenStdout() {
  DCHECK(!IsEnabled());
  output_handle_ = stdout;
}


void Log::OpenTemporaryFile() {
  DCHECK(!IsEnabled());
  output_handle_ = base::OS::OpenTemporaryFile();
}


void Log::OpenFile(const char* name) {
  DCHECK(!IsEnabled());
  output_handle_ = base::OS::FOpen(name, base::OS::LogFileOpenMode);
}


FILE* Log::Close() {
  FILE* result = NULL;
  if (output_handle_ != NULL) {
    if (strcmp(FLAG_logfile, kLogToTemporaryFile) != 0) {
      fclose(output_handle_);
    } else {
      result = output_handle_;
    }
  }
  output_handle_ = NULL;

  DeleteArray(message_buffer_);
  message_buffer_ = NULL;

  is_stopped_ = false;
  return result;
}


Log::MessageBuilder::MessageBuilder(Log* log)
  : log_(log),
    lock_guard_(&log_->mutex_),
    pos_(0) {
  DCHECK(log_->message_buffer_ != NULL);
}


void Log::MessageBuilder::Append(const char* format, ...) {
  Vector<char> buf(log_->message_buffer_ + pos_,
                   Log::kMessageBufferSize - pos_);
  va_list args;
  va_start(args, format);
  AppendVA(format, args);
  va_end(args);
  DCHECK(pos_ <= Log::kMessageBufferSize);
}


void Log::MessageBuilder::AppendVA(const char* format, va_list args) {
  Vector<char> buf(log_->message_buffer_ + pos_,
                   Log::kMessageBufferSize - pos_);
  int result = v8::internal::VSNPrintF(buf, format, args);

  // Result is -1 if output was truncated.
  if (result >= 0) {
    pos_ += result;
  } else {
    pos_ = Log::kMessageBufferSize;
  }
  DCHECK(pos_ <= Log::kMessageBufferSize);
}


void Log::MessageBuilder::Append(const char c) {
  if (pos_ < Log::kMessageBufferSize) {
    log_->message_buffer_[pos_++] = c;
  }
  DCHECK(pos_ <= Log::kMessageBufferSize);
}


void Log::MessageBuilder::AppendDoubleQuotedString(const char* string) {
  Append('\x22');
  for (const char* p = string; *p != '\x0'; p++) {
    if (*p == '\x22') {
      Append('\x5c');
    }
    Append(*p);
  }
  Append('\x22');
}


void Log::MessageBuilder::Append(String* str) {
  DisallowHeapAllocation no_gc;  // Ensure string stay valid.
  int length = str->length();
  for (int i = 0; i < length; i++) {
    Append(static_cast<char>(str->Get(i)));
  }
}


void Log::MessageBuilder::AppendAddress(Address addr) {
  Append(u8"0x%" V8PRIxPTR, addr);
}


void Log::MessageBuilder::AppendSymbolName(Symbol* symbol) {
  DCHECK(symbol);
  Append(u8"symbol(");
  if (!symbol->name()->IsUndefined()) {
    Append(u8"\"");
    AppendDetailed(String::cast(symbol->name()), false);
    Append(u8"\" ");
  }
  Append(u8"hash %x)", symbol->Hash());
}


void Log::MessageBuilder::AppendDetailed(String* str, bool show_impl_info) {
  if (str == NULL) return;
  DisallowHeapAllocation no_gc;  // Ensure string stay valid.
  int len = str->length();
  if (len > 0x1000)
    len = 0x1000;
  if (show_impl_info) {
    Append(str->IsOneByteRepresentation() ? '\x61' : '\x32');
    if (StringShape(str).IsExternal())
      Append('\x65');
    if (StringShape(str).IsInternalized())
      Append('\x23');
    Append(u8":%i:", str->length());
  }
  for (int i = 0; i < len; i++) {
    uc32 c = str->Get(i);
    if (c > 0xff) {
      Append(u8"\\u%04x", c);
    } else if (c < 32 || c > 126) {
      Append(u8"\\x%02x", c);
    } else if (c == '\x2c') {
      Append(u8"\\,");
    } else if (c == '\x5c') {
      Append(u8"\\\\");
    } else if (c == '\x22') {
      Append(u8"\"\"");
    } else {
      Append(u8"%lc", c);
    }
  }
}


void Log::MessageBuilder::AppendStringPart(const char* str, int len) {
  if (pos_ + len > Log::kMessageBufferSize) {
    len = Log::kMessageBufferSize - pos_;
    DCHECK(len >= 0);
    if (len == 0) return;
  }
  Vector<char> buf(log_->message_buffer_ + pos_,
                   Log::kMessageBufferSize - pos_);
  StrNCpy(buf, str, len);
  pos_ += len;
  DCHECK(pos_ <= Log::kMessageBufferSize);
}


void Log::MessageBuilder::WriteToLogFile() {
  DCHECK(pos_ <= Log::kMessageBufferSize);
  // Assert that we do not already have a new line at the end.
  DCHECK(pos_ == 0 || log_->message_buffer_[pos_ - 1] != '\n');
  if (pos_ == Log::kMessageBufferSize) pos_--;
  log_->message_buffer_[pos_++] = '\xa';
  const int written = log_->WriteToFile(log_->message_buffer_, pos_);
  if (written != pos_) {
    log_->stop();
    log_->logger_->LogFailure();
  }
}


}  // namespace internal
}  // namespace v8
