// Copyright 2011 the V8 project authors. All rights reserved.
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.

#include <stdarg.h>
#include <sys/stat.h>

#include "src/v8.h"

#include "src/base/logging.h"
#include "src/base/platform/platform.h"
#include "src/utils.h"
#include <unistd.h>

namespace v8 {
namespace internal {


SimpleStringBuilder::SimpleStringBuilder(int size) {
  buffer_ = Vector<char>::New(size);
  position_ = 0;
}


void SimpleStringBuilder::AddString(const char* s) {
  AddSubstring(s, StrLength(s));
}


void SimpleStringBuilder::AddSubstring(const char* s, int n) {
  DCHECK(!is_finalized() && position_ + n <= buffer_.length());
  DCHECK(static_cast<size_t>(n) <= strlen(s));
  MemCopy(&buffer_[position_], s, n * kCharSize);
  position_ += n;
}


void SimpleStringBuilder::AddPadding(char c, int count) {
  for (int i = 0; i < count; i++) {
    AddCharacter(c);
  }
}


void SimpleStringBuilder::AddDecimalInteger(int32_t value) {
  uint32_t number = static_cast<uint32_t>(value);
  if (value < 0) {
    AddCharacter('\x2d');
    number = static_cast<uint32_t>(-value);
  }
  int digits = 1;
  for (uint32_t factor = 10; digits < 10; digits++, factor *= 10) {
    if (factor > number) break;
  }
  position_ += digits;
  for (int i = 1; i <= digits; i++) {
    buffer_[position_ - i] = '\x30' + static_cast<char>(number % 10);
    number /= 10;
  }
}


char* SimpleStringBuilder::Finalize() {
  DCHECK(!is_finalized() && position_ <= buffer_.length());
  // If there is no space for null termination, overwrite last character.
  if (position_ == buffer_.length()) {
    position_--;
    // Print ellipsis.
    for (int i = 3; i > 0 && position_ > i; --i) buffer_[position_ - i] = '\x2e';
  }
  buffer_[position_] = '\x0';
  // Make sure nobody managed to add a 0-character to the
  // buffer while building the string.
  DCHECK(strlen(buffer_.start()) == static_cast<size_t>(position_));
  position_ = -1;
  DCHECK(is_finalized());
  return buffer_.start();
}


void PrintF(const char* format, ...) {
  va_list arguments;
  va_start(arguments, format);
  base::OS::VPrint(format, arguments);
  va_end(arguments);
}


void PrintF(FILE* out, const char* format, ...) {
  va_list arguments;
  va_start(arguments, format);
  base::OS::VFPrint(out, format, arguments);
  va_end(arguments);
}

#pragma convert("IBM-1047")
inline int GetFirstFlagFrom(const char* format_e, int start = 0) {
  int flag_pos = start;
  for (; format_e[flag_pos] != '\0' && format_e[flag_pos] != '%'; flag_pos++); // find the first flag
  return flag_pos;
}

int VSNPrintFASCII(char* out, int length, const char* format_a, va_list args) {
  int bytes_written = 0, bytes_remain = length;
  size_t format_len = strlen(format_a);
  char buffer_e[format_len + 1];
  char * format_e = buffer_e;
  memcpy(format_e, format_a, format_len + 1);
  __a2e_s(format_e);
  int first_flag = GetFirstFlagFrom(format_e);
  if (first_flag > 0) {
    int size = v8::base::OS::SNPrintF(out, length, "%.*s", first_flag, format_e);
    CHECK(size >= 0);
    bytes_written += size;
    bytes_remain = length - bytes_written;
  }
  format_e += first_flag;
  if (format_e[0] == '\0') {
    __e2a_s(out);
    return bytes_written;
  }

  do {
    int next_flag = GetFirstFlagFrom(format_e, 2);
    char tmp = format_e[next_flag];
    int ret = 0;
    format_e[next_flag] = '\0';
    char flag = format_e[1];
    if (flag == 's') {
      // convert arg
      char * str = va_arg(args, char *);
      size_t str_len = strlen(str);
      char str_e[str_len + 1];
      memcpy(str_e, str, str_len + 1);
      __a2e_s(str_e);
      ret = v8::base::OS::SNPrintF(out + bytes_written, bytes_remain, format_e, str_e);
    } else if (flag == 'c') {
      ret = v8::base::OS::SNPrintF(out + bytes_written, bytes_remain, format_e, Ascii2Ebcdic(va_arg(args, char)));
    } else {
      ret = v8::base::OS::VSNPrintF(out + bytes_written, bytes_remain, format_e, args);
    }
    CHECK(ret >= 0);
    bytes_written += ret;
    bytes_remain = length - bytes_written;
    format_e[next_flag] = tmp;
    format_e += next_flag;
    bytes_remain = length - bytes_written;
  } while (format_e[0] != '\0' || bytes_remain <= 0);

  __e2a_s(out);
  return bytes_written;
}

int SNPrintFASCII(char * out, int length, const char* format_a, ...) {
  va_list args;
  va_start(args, format_a);
  int ret = VSNPrintFASCII(out, length, format_a, args);
  va_end(args);
  return ret;
}

int SNPrintFASCII(Vector<char> str, const char* format, ...) {
  va_list args;
  va_start(args, format);
  int result = VSNPrintFASCII(str, format, args);
  va_end(args);
  return result;
}


int VSNPrintFASCII(Vector<char> str, const char* format, va_list args) {
  return VSNPrintFASCII(str.start(), str.length(), format, args);
}
void VFPrintASCII(FILE* out, const char* format_a, va_list args) {
  base::OS::VFPrintASCII(out, format_a, args);
}

void FPrintASCII(FILE* out, const char* format_a, ...) {
  va_list args;
  va_start(args, format_a);
  VFPrintASCII(out, format_a, args);
  va_end(args);
}

void PrintASCII(const char* format_a, ...) {
  va_list args;
  va_start(args, format_a);
  VFPrintASCII(stdout, format_a, args);
  va_end(args);
}

#pragma convert(pop)

void PrintPID(const char* format, ...) {
  base::OS::Print("\x5b\x6c\x84\x5d\x20", base::OS::GetCurrentProcessId());
  va_list arguments;
  va_start(arguments, format);
  base::OS::VPrint(format, arguments);
  va_end(arguments);
}


int SNPrintF(Vector<char> str, const char* format, ...) {
  va_list args;
  va_start(args, format);
  int result = VSNPrintF(str, format, args);
  va_end(args);
  return result;
}


int VSNPrintF(Vector<char> str, const char* format, va_list args) {
  return base::OS::VSNPrintF(str.start(), str.length(), format, args);
}


void StrNCpy(Vector<char> dest, const char* src, size_t n) {
  base::OS::StrNCpy(dest.start(), dest.length(), src, n);
}


void Flush(FILE* out) {
  fflush(out);
}


char* ReadLine(const char* prompt) {
  char* result = NULL;
  char line_buf[256];
  int offset = 0;
  bool keep_going = true;
  fprintf(stdout, "%s", prompt);
  fflush(stdout);
  while (keep_going) {
    if (fgets(line_buf, sizeof(line_buf), stdin) == NULL) {
      // fgets got an error. Just give up.
      if (result != NULL) {
        DeleteArray(result);
      }
      return NULL;
    }
    int len = StrLength(line_buf);
    if (len > 1 &&
        line_buf[len - 2] == '\\' &&
        line_buf[len - 1] == '\n') {
      // When we read a line that ends with a "\" we remove the escape and
      // append the remainder.
      line_buf[len - 2] = '\n';
      line_buf[len - 1] = 0;
      len -= 1;
    } else if ((len > 0) && (line_buf[len - 1] == '\n')) {
      // Since we read a new line we are done reading the line. This
      // will exit the loop after copying this buffer into the result.
      keep_going = false;
    }
    if (result == NULL) {
      // Allocate the initial result and make room for the terminating '\0'
      result = NewArray<char>(len + 1);
    } else {
      // Allocate a new result with enough room for the new addition.
      int new_len = offset + len + 1;
      char* new_result = NewArray<char>(new_len);
      // Copy the existing input into the new array and set the new
      // array as the result.
      MemCopy(new_result, result, offset * kCharSize);
      DeleteArray(result);
      result = new_result;
    }
    // Copy the newly read line into the result.
    MemCopy(result + offset, line_buf, len * kCharSize);
    offset += len;
  }
  DCHECK(result != NULL);
  result[offset] = '\0';
  return result;
}


char* ReadCharsFromFile(FILE* file,
                        int* size,
                        int extra_space,
                        bool verbose,
                        const char* filename) {
  if (file == NULL || fseek(file, 0, SEEK_END) != 0) {
    if (verbose) {
      base::OS::PrintError("\x43\x61\x6e\x6e\x6f\x74\x20\x72\x65\x61\x64\x20\x66\x72\x6f\x6d\x20\x66\x69\x6c\x65\x20\x6c\xa2\x2e\xa", filename);
    }
    return NULL;
  }

  // Get the size of the file and rewind it.
  *size = ftell(file);
  rewind(file);

  char* result = NewArray<char>(*size + extra_space);
  for (int i = 0; i < *size && feof(file) == 0;) {
    int read = static_cast<int>(fread(&result[i], 1, *size - i, file));
    if (read != (*size - i) && ferror(file) != 0) {
      fclose(file);
      DeleteArray(result);
      return NULL;
    }
    i += read;
  }
  return result;
}


char* ReadCharsFromFile(const char* filename,
                        int* size,
                        int extra_space,
                        bool verbose) {
  FILE* file = base::OS::FOpenASCII(filename, "\x72\x62");
  char* result = ReadCharsFromFile(file, size, extra_space, verbose, filename);
  if (file != NULL) fclose(file);
  return result;
}


byte* ReadBytes(const char* filename, int* size, bool verbose) {
  char* chars = ReadCharsFromFile(filename, size, 0, verbose);
  return reinterpret_cast<byte*>(chars);
}


static Vector<const char> SetVectorContents(char* chars,
                                            int size,
                                            bool* exists) {
  if (!chars) {
    *exists = false;
    return Vector<const char>::empty();
  }
  chars[size] = '\x0';
  *exists = true;
  return Vector<const char>(chars, size);
}


Vector<const char> ReadFile(const char* filename,
                            bool* exists,
                            bool verbose) {
  int size;
  char* result = ReadCharsFromFile(filename, &size, 1, verbose);
  return SetVectorContents(result, size, exists);
}


Vector<const char> ReadFile(FILE* file,
                            bool* exists,
                            bool verbose) {
  int size;
  char* result = ReadCharsFromFile(file, &size, 1, verbose, "");
  return SetVectorContents(result, size, exists);
}


int WriteCharsToFile(const char* str, int size, FILE* f) {
  int total = 0;
  while (total < size) {
    int write = static_cast<int>(fwrite(str, 1, size - total, f));
    if (write == 0) {
      return total;
    }
    total += write;
    str += write;
  }
  return total;
}


int AppendChars(const char* filename,
                const char* str,
                int size,
                bool verbose) {
  FILE* f = base::OS::FOpen(filename, "\x61\x62");
  if (f == NULL) {
    if (verbose) {
      base::OS::PrintError("\x43\x61\x6e\x6e\x6f\x74\x20\x6f\x70\x65\x6e\x20\x66\x69\x6c\x65\x20\x6c\xa2\x20\x66\x6f\x72\x20\x77\x72\x69\x74\x69\x6e\x67\x2e\xa", filename);
    }
    return 0;
  }
  int written = WriteCharsToFile(str, size, f);
  fclose(f);
  return written;
}


int WriteChars(const char* filename,
               const char* str,
               int size,
               bool verbose) {
  FILE* f = base::OS::FOpen(filename, "\x77\x62");
  if (f == NULL) {
    if (verbose) {
      base::OS::PrintError("\x43\x61\x6e\x6e\x6f\x74\x20\x6f\x70\x65\x6e\x20\x66\x69\x6c\x65\x20\x6c\xa2\x20\x66\x6f\x72\x20\x77\x72\x69\x74\x69\x6e\x67\x2e\xa", filename);
    }
    return 0;
  }
  int written = WriteCharsToFile(str, size, f);
  fclose(f);
  return written;
}


int WriteBytes(const char* filename,
               const byte* bytes,
               int size,
               bool verbose) {
  const char* str = reinterpret_cast<const char*>(bytes);
  return WriteChars(filename, str, size, verbose);
}



void StringBuilder::AddFormatted(const char* format, ...) {
  va_list arguments;
  va_start(arguments, format);
  AddFormattedList(format, arguments);
  va_end(arguments);
}


void StringBuilder::AddFormattedList(const char* format, va_list list) {
  DCHECK(!is_finalized() && position_ <= buffer_.length());
  int n = VSNPrintF(buffer_ + position_, format, list);
  if (n < 0 || n >= (buffer_.length() - position_)) {
    position_ = buffer_.length();
  } else {
    position_ += n;
  }
}


#if V8_TARGET_ARCH_IA32 || V8_TARGET_ARCH_X87
static void MemMoveWrapper(void* dest, const void* src, size_t size) {
  memmove(dest, src, size);
}


// Initialize to library version so we can call this at any time during startup.
static MemMoveFunction memmove_function = &MemMoveWrapper;

// Defined in codegen-ia32.cc.
MemMoveFunction CreateMemMoveFunction();

// Copy memory area to disjoint memory area.
void MemMove(void* dest, const void* src, size_t size) {
  if (size == 0) return;
  // Note: here we rely on dependent reads being ordered. This is true
  // on all architectures we currently support.
  (*memmove_function)(dest, src, size);
}

#elif V8_OS_POSIX && V8_HOST_ARCH_ARM
void MemCopyUint16Uint8Wrapper(uint16_t* dest, const uint8_t* src,
                               size_t chars) {
  uint16_t* limit = dest + chars;
  while (dest < limit) {
    *dest++ = static_cast<uint16_t>(*src++);
  }
}


MemCopyUint8Function memcopy_uint8_function = &MemCopyUint8Wrapper;
MemCopyUint16Uint8Function memcopy_uint16_uint8_function =
    &MemCopyUint16Uint8Wrapper;
// Defined in codegen-arm.cc.
MemCopyUint8Function CreateMemCopyUint8Function(MemCopyUint8Function stub);
MemCopyUint16Uint8Function CreateMemCopyUint16Uint8Function(
    MemCopyUint16Uint8Function stub);

#elif V8_OS_POSIX && V8_HOST_ARCH_MIPS
MemCopyUint8Function memcopy_uint8_function = &MemCopyUint8Wrapper;
// Defined in codegen-mips.cc.
MemCopyUint8Function CreateMemCopyUint8Function(MemCopyUint8Function stub);
#endif


void init_memcopy_functions() {
#if V8_TARGET_ARCH_IA32 || V8_TARGET_ARCH_X87
  MemMoveFunction generated_memmove = CreateMemMoveFunction();
  if (generated_memmove != NULL) {
    memmove_function = generated_memmove;
  }
#elif V8_OS_POSIX && V8_HOST_ARCH_ARM
  memcopy_uint8_function = CreateMemCopyUint8Function(&MemCopyUint8Wrapper);
  memcopy_uint16_uint8_function =
      CreateMemCopyUint16Uint8Function(&MemCopyUint16Uint8Wrapper);
#elif V8_OS_POSIX && V8_HOST_ARCH_MIPS
  memcopy_uint8_function = CreateMemCopyUint8Function(&MemCopyUint8Wrapper);
#endif
}


bool DoubleToBoolean(double d) {
  // NaN, +0, and -0 should return the false object
#if V8_TARGET_LITTLE_ENDIAN
  union IeeeDoubleLittleEndianArchType u;
#else
  union IeeeDoubleBigEndianArchType u;
#endif
  u.d = d;
  if (u.bits.exp == 2047) {
    // Detect NaN for IEEE double precision floating point.
    if ((u.bits.man_low | u.bits.man_high) != 0) return false;
  }
  if (u.bits.exp == 0) {
    // Detect +0, and -0 for IEEE double precision floating point.
    if ((u.bits.man_low | u.bits.man_high) == 0) return false;
  }
  return true;
}


} }  // namespace v8::internal
