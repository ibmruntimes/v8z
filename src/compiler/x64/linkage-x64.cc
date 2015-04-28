// Copyright 2014 the V8 project authors. All rights reserved.
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.

<<<<<<< HEAD
#include "src/v8.h"

=======
>>>>>>> 4.3
#include "src/assembler.h"
#include "src/code-stubs.h"
#include "src/compiler/linkage.h"
#include "src/compiler/linkage-impl.h"
#include "src/zone.h"

namespace v8 {
namespace internal {
namespace compiler {

#ifdef _WIN64
const bool kWin64 = true;
#else
const bool kWin64 = false;
#endif

<<<<<<< HEAD
struct LinkageHelperTraits {
=======
struct X64LinkageHelperTraits {
>>>>>>> 4.3
  static Register ReturnValueReg() { return rax; }
  static Register ReturnValue2Reg() { return rdx; }
  static Register JSCallFunctionReg() { return rdi; }
  static Register ContextReg() { return rsi; }
  static Register RuntimeCallFunctionReg() { return rbx; }
  static Register RuntimeCallArgCountReg() { return rax; }
  static RegList CCalleeSaveRegisters() {
    if (kWin64) {
      return rbx.bit() | rdi.bit() | rsi.bit() | r12.bit() | r13.bit() |
             r14.bit() | r15.bit();
    } else {
      return rbx.bit() | r12.bit() | r13.bit() | r14.bit() | r15.bit();
    }
  }
  static Register CRegisterParameter(int i) {
    if (kWin64) {
      static Register register_parameters[] = {rcx, rdx, r8, r9};
      return register_parameters[i];
    } else {
      static Register register_parameters[] = {rdi, rsi, rdx, rcx, r8, r9};
      return register_parameters[i];
    }
  }
  static int CRegisterParametersLength() { return kWin64 ? 4 : 6; }
};

<<<<<<< HEAD

CallDescriptor* Linkage::GetJSCallDescriptor(int parameter_count, Zone* zone) {
  return LinkageHelper::GetJSCallDescriptor<LinkageHelperTraits>(
      zone, parameter_count);
=======
typedef LinkageHelper<X64LinkageHelperTraits> LH;

CallDescriptor* Linkage::GetJSCallDescriptor(Zone* zone, bool is_osr,
                                             int parameter_count,
                                             CallDescriptor::Flags flags) {
  return LH::GetJSCallDescriptor(zone, is_osr, parameter_count, flags);
>>>>>>> 4.3
}


CallDescriptor* Linkage::GetRuntimeCallDescriptor(
<<<<<<< HEAD
    Runtime::FunctionId function, int parameter_count,
    Operator::Property properties,
    CallDescriptor::DeoptimizationSupport can_deoptimize, Zone* zone) {
  return LinkageHelper::GetRuntimeCallDescriptor<LinkageHelperTraits>(
      zone, function, parameter_count, properties, can_deoptimize);
=======
    Zone* zone, Runtime::FunctionId function, int parameter_count,
    Operator::Properties properties) {
  return LH::GetRuntimeCallDescriptor(zone, function, parameter_count,
                                      properties);
>>>>>>> 4.3
}


CallDescriptor* Linkage::GetStubCallDescriptor(
<<<<<<< HEAD
    CodeStubInterfaceDescriptor* descriptor, int stack_parameter_count,
    CallDescriptor::DeoptimizationSupport can_deoptimize, Zone* zone) {
  return LinkageHelper::GetStubCallDescriptor<LinkageHelperTraits>(
      zone, descriptor, stack_parameter_count, can_deoptimize);
}


CallDescriptor* Linkage::GetSimplifiedCDescriptor(
    Zone* zone, int num_params, MachineType return_type,
    const MachineType* param_types) {
  return LinkageHelper::GetSimplifiedCDescriptor<LinkageHelperTraits>(
      zone, num_params, return_type, param_types);
}

}
}
}  // namespace v8::internal::compiler
=======
    Isolate* isolate, Zone* zone, const CallInterfaceDescriptor& descriptor,
    int stack_parameter_count, CallDescriptor::Flags flags,
    Operator::Properties properties, MachineType return_type) {
  return LH::GetStubCallDescriptor(isolate, zone, descriptor,
                                   stack_parameter_count, flags, properties,
                                   return_type);
}


CallDescriptor* Linkage::GetSimplifiedCDescriptor(Zone* zone,
                                                  const MachineSignature* sig) {
  return LH::GetSimplifiedCDescriptor(zone, sig);
}

}  // namespace compiler
}  // namespace internal
}  // namespace v8
>>>>>>> 4.3
