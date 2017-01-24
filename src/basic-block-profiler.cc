// Copyright 2014 the V8 project authors. All rights reserved.
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.

#include "src/basic-block-profiler.h"

#include <sstream>

namespace v8 {
namespace internal {

BasicBlockProfiler::Data::Data(size_t n_blocks)
    : n_blocks_(n_blocks), block_ids_(n_blocks_), counts_(n_blocks_, 0) {}


BasicBlockProfiler::Data::~Data() {}


static void InsertIntoString(std::ostringstream* os, std::string* string) {
  string->insert(0, os->str());
}


void BasicBlockProfiler::Data::SetCode(std::ostringstream* os) {
  InsertIntoString(os, &code_);
}


void BasicBlockProfiler::Data::SetFunctionName(std::ostringstream* os) {
  InsertIntoString(os, &function_name_);
}


void BasicBlockProfiler::Data::SetSchedule(std::ostringstream* os) {
  InsertIntoString(os, &schedule_);
}


void BasicBlockProfiler::Data::SetBlockId(size_t offset, size_t block_id) {
  DCHECK(offset < n_blocks_);
  block_ids_[offset] = block_id;
}


uint32_t* BasicBlockProfiler::Data::GetCounterAddress(size_t offset) {
  DCHECK(offset < n_blocks_);
  return &counts_[offset];
}


void BasicBlockProfiler::Data::ResetCounts() {
  for (size_t i = 0; i < n_blocks_; ++i) {
    counts_[i] = 0;
  }
}


BasicBlockProfiler::BasicBlockProfiler() {}


BasicBlockProfiler::Data* BasicBlockProfiler::NewData(size_t n_blocks) {
  Data* data = new Data(n_blocks);
  data_list_.push_back(data);
  return data;
}


BasicBlockProfiler::~BasicBlockProfiler() {
  for (DataList::iterator i = data_list_.begin(); i != data_list_.end(); ++i) {
    delete (*i);
  }
}


void BasicBlockProfiler::ResetCounts() {
  for (DataList::iterator i = data_list_.begin(); i != data_list_.end(); ++i) {
    (*i)->ResetCounts();
  }
}


std::ostream& operator<<(std::ostream& os, const BasicBlockProfiler& p) {
  os << u8"---- Start Profiling Data ----" << std::endl;
  typedef BasicBlockProfiler::DataList::const_iterator iterator;
  for (iterator i = p.data_list_.begin(); i != p.data_list_.end(); ++i) {
    os << **i;
  }
  os << u8"---- End Profiling Data ----" << std::endl;
  return os;
}


std::ostream& operator<<(std::ostream& os, const BasicBlockProfiler::Data& d) {
  const char* name = u8"unknown function";
  if (!d.function_name_.empty()) {
    name = d.function_name_.c_str();
  }
  if (!d.schedule_.empty()) {
    os << u8"schedule for " << name << std::endl;
    os << d.schedule_.c_str() << std::endl;
  }
  os << u8"block counts for " << name << u8":" << std::endl;
  for (size_t i = 0; i < d.n_blocks_; ++i) {
    os << u8"block " << d.block_ids_[i] << u8" : " << d.counts_[i] << std::endl;
  }
  os << std::endl;
  if (!d.code_.empty()) {
    os << d.code_.c_str() << std::endl;
  }
  return os;
}

}  // namespace internal
}  // namespace v8
