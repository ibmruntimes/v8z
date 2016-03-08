// Copyright 2014 the V8 project authors. All rights reserved.
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.

#ifndef V8_VECTOR_H_
#define V8_VECTOR_H_

#include <string.h>
#include <algorithm>

#include "src/allocation.h"
#include "src/checks.h"
#include "src/globals.h"

namespace v8 {
namespace internal {


template <typename T>
class Vector {
 public:
  Vector() : start_(NULL), length_(0) {}
  Vector(T* data, int length) : start_(data), length_(length) {
    DCHECK(length == 0 || (length > 0 && data != NULL));
  }

  static Vector<T> New(int length) {
    return Vector<T>(NewArray<T>(length), length);
  }

  // Returns a vector using the same backing storage as this one,
  // spanning from and including 'from', to but not including 'to'.
  Vector<T> SubVector(int from, int to) {
    SLOW_DCHECK(to <= length_);
    SLOW_DCHECK(from < to);
    DCHECK(0 <= from);
    return Vector<T>(start() + from, to - from);
  }

  // Returns the length of the vector.
  int length() const { return length_; }

  // Returns whether or not the vector is empty.
  bool is_empty() const { return length_ == 0; }

  // Returns the pointer to the start of the data in the vector.
  T* start() const { return start_; }

  // Access individual vector elements - checks bounds in debug mode.
  T& operator[](int index) const {
    DCHECK(0 <= index && index < length_);
    return start_[index];
  }

  const T& at(int index) const { return operator[](index); }

  T& first() { return start_[0]; }

  T& last() { return start_[length_ - 1]; }

  // Returns a clone of this vector with a new backing store.
  Vector<T> Clone() const {
    T* result = NewArray<T>(length_);
    for (int i = 0; i < length_; i++) result[i] = start_[i];
    return Vector<T>(result, length_);
  }

  void Sort(int (*cmp)(const T*, const T*)) {
    std::sort(start(), start() + length(), RawComparer(cmp));
  }

  void Sort() {
    std::sort(start(), start() + length());
  }

  void Truncate(int length) {
    DCHECK(length <= length_);
    length_ = length;
  }

  // Releases the array underlying this vector. Once disposed the
  // vector is empty.
  void Dispose() {
    DeleteArray(start_);
    start_ = NULL;
    length_ = 0;
  }

  inline Vector<T> operator+(int offset) {
    DCHECK(offset < length_);
    return Vector<T>(start_ + offset, length_ - offset);
  }

  // Factory method for creating empty vectors.
  static Vector<T> empty() { return Vector<T>(NULL, 0); }

  template<typename S>
  static Vector<T> cast(Vector<S> input) {
    return Vector<T>(reinterpret_cast<T*>(input.start()),
                     input.length() * sizeof(S) / sizeof(T));
  }

  bool operator==(const Vector<T>& other) const {
    if (length_ != other.length_) return false;
    if (start_ == other.start_) return true;
    for (int i = 0; i < length_; ++i) {
      if (start_[i] != other.start_[i]) {
        return false;
      }
    }
    return true;
  }

 protected:
  void set_start(T* start) { start_ = start; }

 private:
  T* start_;
  int length_;

  class RawComparer {
   public:
    explicit RawComparer(int (*cmp)(const T*, const T*)) : cmp_(cmp) {}
    bool operator()(const T& a, const T& b) {
      return cmp_(&a, &b) < 0;
    }

   private:
    int (*cmp_)(const T*, const T*);
  };
};

#ifdef V8_OS_ZOS
template <>
const char& Vector<const char>::operator[](int index) const {
      DCHECK(0 <= index && index < length_);
      static const char ebcdic2ascii[256] = {
      	       0,  1,  2,  3,156,  9,134,127,151,141,142, 11, 12, 13, 14, 15,
              16, 17, 18, 19,157,133,  8,135, 24, 25,146,143, 28, 29, 30, 31,
             128,129,130,131,132, 10, 23, 27,136,137,138,139,140,  5,  6,  7,
             144,145, 22,147,148,149,150,  4,152,153,154,155, 20, 21,158, 26,
              32,160,161,162,163,164,165,166,167,168, 91, 46, 60, 40, 43, 33,
              38,169,170,171,172,173,174,175,176,177, 93, 36, 42, 41, 59, 94,
              45, 47,178,179,180,181,182,183,184,185,124, 44, 37, 95, 62, 63,
             186,187,188,189,190,191,192,193,194, 96, 58, 35, 64, 39, 61, 34,
             195, 97, 98, 99,100,101,102,103,104,105,196,197,198,199,200,201,
             202,106,107,108,109,110,111,112,113,114,203,204,205,206,207,208,
             209,126,115,116,117,118,119,120,121,122,210,211,212,213,214,215,
             216,217,218,219,220,221,222,223,224,225,226,227,228,229,230,231,
             123, 65, 66, 67, 68, 69, 70, 71, 72, 73,232,233,234,235,236,237,
             125, 74, 75, 76, 77, 78, 79, 80, 81, 82,238,239,240,241,242,243,
              92,159, 83, 84, 85, 86, 87, 88, 89, 90,244,245,246,247,248,249,
              48, 49, 50, 51, 52, 53, 54, 55, 56, 57,250,251,252,253,254,255
       };  	
       return ebcdic2ascii[start_[index]];	
}
#endif

template <typename T>
class ScopedVector : public Vector<T> {
 public:
  explicit ScopedVector(int length) : Vector<T>(NewArray<T>(length), length) { }
  ~ScopedVector() {
    DeleteArray(this->start());
  }

 private:
  DISALLOW_IMPLICIT_CONSTRUCTORS(ScopedVector);
};


inline int StrLength(const char* string) {
  size_t length = strlen(string);
  DCHECK(length == static_cast<size_t>(static_cast<int>(length)));
  return static_cast<int>(length);
}


#define STATIC_ASCII_VECTOR(x)                        \
  v8::internal::Vector<const uint8_t>(reinterpret_cast<const uint8_t*>(x), \
                                      ARRAY_SIZE(x)-1)

inline Vector<const char> CStrVector(const char* data) {
  return Vector<const char>(data, StrLength(data));
}

inline Vector<const uint8_t> OneByteVector(const char* data, int length) {
  return Vector<const uint8_t>(reinterpret_cast<const uint8_t*>(data), length);
}

inline Vector<const uint8_t> OneByteVector(const char* data) {
  return OneByteVector(data, StrLength(data));
}

inline Vector<char> MutableCStrVector(char* data) {
  return Vector<char>(data, StrLength(data));
}

inline Vector<char> MutableCStrVector(char* data, int max) {
  int length = StrLength(data);
  return Vector<char>(data, (length < max) ? length : max);
}


} }  // namespace v8::internal

#endif  // V8_VECTOR_H_
