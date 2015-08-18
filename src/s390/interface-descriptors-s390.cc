// Copyright 2015 the V8 project authors. All rights reserved.
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.

#include "src/v8.h"

#if V8_TARGET_ARCH_S390

#include "src/interface-descriptors.h"

namespace v8 {
namespace internal {

const Register CallInterfaceDescriptor::ContextRegister() { return cp; }


const Register LoadDescriptor::ReceiverRegister() { return r3; }
const Register LoadDescriptor::NameRegister() { return r4; }


const Register VectorLoadICTrampolineDescriptor::SlotRegister() { return r2; }


const Register VectorLoadICDescriptor::VectorRegister() { return r5; }


const Register StoreDescriptor::ReceiverRegister() { return r3; }
const Register StoreDescriptor::NameRegister() { return r4; }
const Register StoreDescriptor::ValueRegister() { return r2; }


const Register StoreTransitionDescriptor::MapRegister() { return r5; }


const Register ElementTransitionAndStoreDescriptor::MapRegister() { return r5; }


const Register InstanceofDescriptor::left() { return r2; }
const Register InstanceofDescriptor::right() { return r3; }


const Register ArgumentsAccessReadDescriptor::index() { return r3; }
const Register ArgumentsAccessReadDescriptor::parameter_count() { return r2; }


const Register ApiGetterDescriptor::function_address() { return r4; }


const Register MathPowTaggedDescriptor::exponent() { return r4; }


const Register MathPowIntegerDescriptor::exponent() {
  return MathPowTaggedDescriptor::exponent();
}


const Register GrowArrayElementsDescriptor::ObjectRegister() { return r2; }
const Register GrowArrayElementsDescriptor::KeyRegister() { return r5; }
const Register GrowArrayElementsDescriptor::CapacityRegister() { return r4; }


void FastNewClosureDescriptor::Initialize(CallInterfaceDescriptorData* data) {
  Register registers[] = {cp, r4};
  data->Initialize(arraysize(registers), registers, NULL);
}


void FastNewContextDescriptor::Initialize(CallInterfaceDescriptorData* data) {
  Register registers[] = {cp, r3};
  data->Initialize(arraysize(registers), registers, NULL);
}


void ToNumberDescriptor::Initialize(CallInterfaceDescriptorData* data) {
  Register registers[] = {cp, r2};
  data->Initialize(arraysize(registers), registers, NULL);
}


void NumberToStringDescriptor::Initialize(CallInterfaceDescriptorData* data) {
  Register registers[] = {cp, r2};
  data->Initialize(arraysize(registers), registers, NULL);
}


void TypeofDescriptor::Initialize(CallInterfaceDescriptorData* data) {
  Register registers[] = {cp, r5};
  data->Initialize(arraysize(registers), registers, NULL);
}


void FastCloneShallowArrayDescriptor::Initialize(
    CallInterfaceDescriptorData* data) {
  Register registers[] = {cp, r5, r4, r3};
  Representation representations[] = {
      Representation::Tagged(), Representation::Tagged(), Representation::Smi(),
      Representation::Tagged()};
  data->Initialize(arraysize(registers), registers, representations);
}


void FastCloneShallowObjectDescriptor::Initialize(
    CallInterfaceDescriptorData* data) {
  Register registers[] = {cp, r5, r4, r3, r2};
  data->Initialize(arraysize(registers), registers, NULL);
}


void CreateAllocationSiteDescriptor::Initialize(
    CallInterfaceDescriptorData* data) {
  Register registers[] = {cp, r4, r5};
  Representation representations[] = {Representation::Tagged(),
                                      Representation::Tagged(),
                                      Representation::Smi()};
  data->Initialize(arraysize(registers), registers, representations);
}


void CreateWeakCellDescriptor::Initialize(CallInterfaceDescriptorData* data) {
  Register registers[] = {cp, r4, r5, r3};
  Representation representations[] = {
      Representation::Tagged(), Representation::Tagged(), Representation::Smi(),
      Representation::Tagged()};
  data->Initialize(arraysize(registers), registers, representations);
}


void StoreArrayLiteralElementDescriptor::Initialize(
    CallInterfaceDescriptorData* data) {
  Register registers[] = {cp, r5, r2};
  data->Initialize(arraysize(registers), registers, NULL);
}


void CallFunctionDescriptor::Initialize(CallInterfaceDescriptorData* data) {
  Register registers[] = {cp, r3};
  data->Initialize(arraysize(registers), registers, NULL);
}


void CallFunctionWithFeedbackDescriptor::Initialize(
    CallInterfaceDescriptorData* data) {
  Register registers[] = {cp, r3, r5};
  Representation representations[] = {Representation::Tagged(),
                                      Representation::Tagged(),
                                      Representation::Smi()};
  data->Initialize(arraysize(registers), registers, representations);
}


void CallFunctionWithFeedbackAndVectorDescriptor::Initialize(
    CallInterfaceDescriptorData* data) {
  Register registers[] = {cp, r3, r5, r4};
  Representation representations[] = {
      Representation::Tagged(), Representation::Tagged(), Representation::Smi(),
      Representation::Tagged()};
  data->Initialize(arraysize(registers), registers, representations);
}


void CallConstructDescriptor::Initialize(CallInterfaceDescriptorData* data) {
  // r2 : number of arguments
  // r3 : the function to call
  // r4 : feedback vector
  // r5 : (only if r4 is not the megamorphic symbol) slot in feedback
  //      vector (Smi)
  // TODO(turbofan): So far we don't gather type feedback and hence skip the
  // slot parameter, but ArrayConstructStub needs the vector to be undefined.
  Register registers[] = {cp, r2, r3, r4};
  data->Initialize(arraysize(registers), registers, NULL);
}


void RegExpConstructResultDescriptor::Initialize(
    CallInterfaceDescriptorData* data) {
  Register registers[] = {cp, r4, r3, r2};
  data->Initialize(arraysize(registers), registers, NULL);
}


void TransitionElementsKindDescriptor::Initialize(
    CallInterfaceDescriptorData* data) {
  Register registers[] = {cp, r2, r3};
  data->Initialize(arraysize(registers), registers, NULL);
}


void AllocateHeapNumberDescriptor::Initialize(
    CallInterfaceDescriptorData* data) {
  // register state
  // cp -- context
  Register registers[] = {cp};
  data->Initialize(arraysize(registers), registers, nullptr);
}


void ArrayConstructorConstantArgCountDescriptor::Initialize(
    CallInterfaceDescriptorData* data) {
  // register state
  // cp -- context
  // r2 -- number of arguments
  // r3 -- function
  // r4 -- allocation site with elements kind
  Register registers[] = {cp, r3, r4};
  data->Initialize(arraysize(registers), registers, NULL);
}


void ArrayConstructorDescriptor::Initialize(CallInterfaceDescriptorData* data) {
  // stack param count needs (constructor pointer, and single argument)
  Register registers[] = {cp, r3, r4, r2};
  Representation representations[] = {
      Representation::Tagged(), Representation::Tagged(),
      Representation::Tagged(), Representation::Integer32()};
  data->Initialize(arraysize(registers), registers, representations);
}


void InternalArrayConstructorConstantArgCountDescriptor::Initialize(
    CallInterfaceDescriptorData* data) {
  // register state
  // cp -- context
  // r2 -- number of arguments
  // r3 -- constructor function
  Register registers[] = {cp, r3};
  data->Initialize(arraysize(registers), registers, NULL);
}


void InternalArrayConstructorDescriptor::Initialize(
    CallInterfaceDescriptorData* data) {
  // stack param count needs (constructor pointer, and single argument)
  Register registers[] = {cp, r3, r2};
  Representation representations[] = {Representation::Tagged(),
                                      Representation::Tagged(),
                                      Representation::Integer32()};
  data->Initialize(arraysize(registers), registers, representations);
}


void CompareDescriptor::Initialize(CallInterfaceDescriptorData* data) {
  Register registers[] = {cp, r3, r2};
  data->Initialize(arraysize(registers), registers, NULL);
}


void CompareNilDescriptor::Initialize(CallInterfaceDescriptorData* data) {
  Register registers[] = {cp, r2};
  data->Initialize(arraysize(registers), registers, NULL);
}


void ToBooleanDescriptor::Initialize(CallInterfaceDescriptorData* data) {
  Register registers[] = {cp, r2};
  data->Initialize(arraysize(registers), registers, NULL);
}


void BinaryOpDescriptor::Initialize(CallInterfaceDescriptorData* data) {
  Register registers[] = {cp, r3, r2};
  data->Initialize(arraysize(registers), registers, NULL);
}


void BinaryOpWithAllocationSiteDescriptor::Initialize(
    CallInterfaceDescriptorData* data) {
  Register registers[] = {cp, r4, r3, r2};
  data->Initialize(arraysize(registers), registers, NULL);
}


void StringAddDescriptor::Initialize(CallInterfaceDescriptorData* data) {
  Register registers[] = {cp, r3, r2};
  data->Initialize(arraysize(registers), registers, NULL);
}


void KeyedDescriptor::Initialize(CallInterfaceDescriptorData* data) {
  Register registers[] = {
      cp,  // context
      r4,  // key
  };
  Representation representations[] = {
      Representation::Tagged(),  // context
      Representation::Tagged(),  // key
  };
  data->Initialize(arraysize(registers), registers, representations);
}


void NamedDescriptor::Initialize(CallInterfaceDescriptorData* data) {
  Register registers[] = {
      cp,  // context
      r4,  // name
  };
  Representation representations[] = {
      Representation::Tagged(),  // context
      Representation::Tagged(),  // name
  };
  data->Initialize(arraysize(registers), registers, representations);
}


void CallHandlerDescriptor::Initialize(CallInterfaceDescriptorData* data) {
  Register registers[] = {
      cp,  // context
      r2,  // receiver
  };
  Representation representations[] = {
      Representation::Tagged(),  // context
      Representation::Tagged(),  // receiver
  };
  data->Initialize(arraysize(registers), registers, representations);
}


void ArgumentAdaptorDescriptor::Initialize(CallInterfaceDescriptorData* data) {
  Register registers[] = {
      cp,  // context
      r3,  // JSFunction
      r2,  // actual number of arguments
      r4,  // expected number of arguments
  };
  Representation representations[] = {
      Representation::Tagged(),     // context
      Representation::Tagged(),     // JSFunction
      Representation::Integer32(),  // actual number of arguments
      Representation::Integer32(),  // expected number of arguments
  };
  data->Initialize(arraysize(registers), registers, representations);
}


void ApiFunctionDescriptor::Initialize(CallInterfaceDescriptorData* data) {
  Register registers[] = {
      cp,  // context
      r2,  // callee
      r6,  // call_data
      r4,  // holder
      r3,  // api_function_address
      r5,  // actual number of arguments
  };
  Representation representations[] = {
      Representation::Tagged(),     // context
      Representation::Tagged(),     // callee
      Representation::Tagged(),     // call_data
      Representation::Tagged(),     // holder
      Representation::External(),   // api_function_address
      Representation::Integer32(),  // actual number of arguments
  };
  data->Initialize(arraysize(registers), registers, representations);
}


void ApiAccessorDescriptor::Initialize(CallInterfaceDescriptorData* data) {
  Register registers[] = {
      cp,  // context
      r2,  // callee
      r6,  // call_data
      r4,  // holder
      r3,  // api_function_address
  };
  Representation representations[] = {
      Representation::Tagged(),    // context
      Representation::Tagged(),    // callee
      Representation::Tagged(),    // call_data
      Representation::Tagged(),    // holder
      Representation::External(),  // api_function_address
  };
  data->Initialize(arraysize(registers), registers, representations);
}


void MathRoundVariantDescriptor::Initialize(CallInterfaceDescriptorData* data) {
  Register registers[] = {
      cp,  // context
      r3,  // math rounding function
      r5,  // vector slot id
  };
  Representation representations[] = {
      Representation::Tagged(),  //
      Representation::Tagged(),  //
      Representation::Tagged(),  //
  };
  data->Initialize(arraysize(registers), registers, representations);
}
}
}  // namespace v8::internal

#endif  // V8_TARGET_ARCH_S390
