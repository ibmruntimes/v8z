// Copyright 2012 the V8 project authors. All rights reserved.
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.

#include "src/objects.h"

#include "src/disasm.h"
#include "src/disassembler.h"
#include "src/interpreter/bytecodes.h"
#include "src/objects-inl.h"
#include "src/ostreams.h"
#include "src/regexp/jsregexp.h"

namespace v8 {
namespace internal {

#ifdef OBJECT_PRINT

void Object::Print() {
  OFStream os(stdout);
  this->Print(os);
  os << std::flush;
}


void Object::Print(v8::base::OStream& os) {  // NOLINT
  if (IsSmi()) {
    Smi::cast(this)->SmiPrint(os);
  } else {
    HeapObject::cast(this)->HeapObjectPrint(os);
  }
}


void HeapObject::PrintHeader(v8::base::OStream& os, const char* id) {  // NOLINT
  os << reinterpret_cast<void*>(this) << u8": [" << id << u8"]";
}


void HeapObject::HeapObjectPrint(v8::base::OStream& os) {  // NOLINT
  InstanceType instance_type = map()->instance_type();

  HandleScope scope(GetIsolate());
  if (instance_type < FIRST_NONSTRING_TYPE) {
    String::cast(this)->StringPrint(os);
    return;
  }

  switch (instance_type) {
    case SYMBOL_TYPE:
      Symbol::cast(this)->SymbolPrint(os);
      break;
    case MAP_TYPE:
      Map::cast(this)->MapPrint(os);
      break;
    case HEAP_NUMBER_TYPE:
      HeapNumber::cast(this)->HeapNumberPrint(os);
      break;
    case MUTABLE_HEAP_NUMBER_TYPE:
      os << u8"<mutable ";
      HeapNumber::cast(this)->HeapNumberPrint(os);
      os << u8">";
      break;
    case SIMD128_VALUE_TYPE:
      Simd128Value::cast(this)->Simd128ValuePrint(os);
      break;
    case FIXED_DOUBLE_ARRAY_TYPE:
      FixedDoubleArray::cast(this)->FixedDoubleArrayPrint(os);
      break;
    case FIXED_ARRAY_TYPE:
      FixedArray::cast(this)->FixedArrayPrint(os);
      break;
    case BYTE_ARRAY_TYPE:
      ByteArray::cast(this)->ByteArrayPrint(os);
      break;
    case BYTECODE_ARRAY_TYPE:
      BytecodeArray::cast(this)->BytecodeArrayPrint(os);
      break;
    case TRANSITION_ARRAY_TYPE:
      TransitionArray::cast(this)->TransitionArrayPrint(os);
      break;
    case FREE_SPACE_TYPE:
      FreeSpace::cast(this)->FreeSpacePrint(os);
      break;

#define PRINT_FIXED_TYPED_ARRAY(Type, type, TYPE, ctype, size) \
  case Fixed##Type##Array::kInstanceType:                      \
    Fixed##Type##Array::cast(this)->FixedTypedArrayPrint(os);  \
    break;

    TYPED_ARRAYS(PRINT_FIXED_TYPED_ARRAY)
#undef PRINT_FIXED_TYPED_ARRAY

    case FILLER_TYPE:
      os << u8"filler";
      break;
    case JS_OBJECT_TYPE:  // fall through
    case JS_SPECIAL_API_OBJECT_TYPE:
    case JS_CONTEXT_EXTENSION_OBJECT_TYPE:
    case JS_ARRAY_TYPE:
    case JS_GENERATOR_OBJECT_TYPE:
    case JS_PROMISE_TYPE:
      JSObject::cast(this)->JSObjectPrint(os);
      break;
    case JS_REGEXP_TYPE:
      JSRegExp::cast(this)->JSRegExpPrint(os);
      break;
    case ODDBALL_TYPE:
      Oddball::cast(this)->to_string()->Print(os);
      break;
    case JS_MODULE_TYPE:
      JSModule::cast(this)->JSModulePrint(os);
      break;
    case JS_BOUND_FUNCTION_TYPE:
      JSBoundFunction::cast(this)->JSBoundFunctionPrint(os);
      break;
    case JS_FUNCTION_TYPE:
      JSFunction::cast(this)->JSFunctionPrint(os);
      break;
    case JS_GLOBAL_PROXY_TYPE:
      JSGlobalProxy::cast(this)->JSGlobalProxyPrint(os);
      break;
    case JS_GLOBAL_OBJECT_TYPE:
      JSGlobalObject::cast(this)->JSGlobalObjectPrint(os);
      break;
    case JS_VALUE_TYPE:
      JSValue::cast(this)->JSValuePrint(os);
      break;
    case JS_DATE_TYPE:
      JSDate::cast(this)->JSDatePrint(os);
      break;
    case CODE_TYPE:
      Code::cast(this)->CodePrint(os);
      break;
    case JS_PROXY_TYPE:
      JSProxy::cast(this)->JSProxyPrint(os);
      break;
    case JS_SET_TYPE:
      JSSet::cast(this)->JSSetPrint(os);
      break;
    case JS_MAP_TYPE:
      JSMap::cast(this)->JSMapPrint(os);
      break;
    case JS_SET_ITERATOR_TYPE:
      JSSetIterator::cast(this)->JSSetIteratorPrint(os);
      break;
    case JS_MAP_ITERATOR_TYPE:
      JSMapIterator::cast(this)->JSMapIteratorPrint(os);
      break;
    case JS_WEAK_MAP_TYPE:
      JSWeakMap::cast(this)->JSWeakMapPrint(os);
      break;
    case JS_WEAK_SET_TYPE:
      JSWeakSet::cast(this)->JSWeakSetPrint(os);
      break;
    case FOREIGN_TYPE:
      Foreign::cast(this)->ForeignPrint(os);
      break;
    case SHARED_FUNCTION_INFO_TYPE:
      SharedFunctionInfo::cast(this)->SharedFunctionInfoPrint(os);
      break;
    case JS_MESSAGE_OBJECT_TYPE:
      JSMessageObject::cast(this)->JSMessageObjectPrint(os);
      break;
    case CELL_TYPE:
      Cell::cast(this)->CellPrint(os);
      break;
    case PROPERTY_CELL_TYPE:
      PropertyCell::cast(this)->PropertyCellPrint(os);
      break;
    case WEAK_CELL_TYPE:
      WeakCell::cast(this)->WeakCellPrint(os);
      break;
    case JS_ARRAY_BUFFER_TYPE:
      JSArrayBuffer::cast(this)->JSArrayBufferPrint(os);
      break;
    case JS_TYPED_ARRAY_TYPE:
      JSTypedArray::cast(this)->JSTypedArrayPrint(os);
      break;
    case JS_DATA_VIEW_TYPE:
      JSDataView::cast(this)->JSDataViewPrint(os);
      break;
#define MAKE_STRUCT_CASE(NAME, Name, name) \
  case NAME##_TYPE:                        \
    Name::cast(this)->Name##Print(os);     \
    break;
  STRUCT_LIST(MAKE_STRUCT_CASE)
#undef MAKE_STRUCT_CASE

    default:
      os << u8"UNKNOWN TYPE " << map()->instance_type();
      UNREACHABLE();
      break;
  }
}


void Simd128Value::Simd128ValuePrint(v8::base::OStream& os) {  // NOLINT
#define PRINT_SIMD128_VALUE(TYPE, Type, type, lane_count, lane_type) \
  if (Is##Type()) return Type::cast(this)->Type##Print(os);
  SIMD128_TYPES(PRINT_SIMD128_VALUE)
#undef PRINT_SIMD128_VALUE
  UNREACHABLE();
}


void Float32x4::Float32x4Print(v8::base::OStream& os) {  // NOLINT
  char arr[100];
  Vector<char> buffer(arr, arraysize(arr));
  os << std::string(DoubleToCString(get_lane(0), buffer)) << u8", "
     << std::string(DoubleToCString(get_lane(1), buffer)) << u8", "
     << std::string(DoubleToCString(get_lane(2), buffer)) << u8", "
     << std::string(DoubleToCString(get_lane(3), buffer));
}


#define SIMD128_INT_PRINT_FUNCTION(type, lane_count)                \
  void type::type##Print(v8::base::OStream& os) {                        \
    char arr[100];                                                  \
    Vector<char> buffer(arr, arraysize(arr));                       \
    os << std::string(IntToCString(get_lane(0), buffer));           \
    for (int i = 1; i < lane_count; i++) {                          \
      os << u8", " << std::string(IntToCString(get_lane(i), buffer)); \
    }                                                               \
  }
SIMD128_INT_PRINT_FUNCTION(Int32x4, 4)
SIMD128_INT_PRINT_FUNCTION(Uint32x4, 4)
SIMD128_INT_PRINT_FUNCTION(Int16x8, 8)
SIMD128_INT_PRINT_FUNCTION(Uint16x8, 8)
SIMD128_INT_PRINT_FUNCTION(Int8x16, 16)
SIMD128_INT_PRINT_FUNCTION(Uint8x16, 16)
#undef SIMD128_INT_PRINT_FUNCTION


#define SIMD128_BOOL_PRINT_FUNCTION(type, lane_count)            \
  void type::type##Print(v8::base::OStream& os) {                     \
    char arr[100];                                               \
    Vector<char> buffer(arr, arraysize(arr));                    \
    os << std::string(get_lane(0) ? u8"true" : u8"false");           \
    for (int i = 1; i < lane_count; i++) {                       \
      os << u8", " << std::string(get_lane(i) ? u8"true" : u8"false"); \
    }                                                            \
  }
SIMD128_BOOL_PRINT_FUNCTION(Bool32x4, 4)
SIMD128_BOOL_PRINT_FUNCTION(Bool16x8, 8)
SIMD128_BOOL_PRINT_FUNCTION(Bool8x16, 16)
#undef SIMD128_BOOL_PRINT_FUNCTION


void ByteArray::ByteArrayPrint(v8::base::OStream& os) {  // NOLINT
  os << u8"byte array, data starts at " << GetDataStartAddress();
}


void BytecodeArray::BytecodeArrayPrint(v8::base::OStream& os) {  // NOLINT
  Disassemble(os);
}


void FreeSpace::FreeSpacePrint(v8::base::OStream& os) {  // NOLINT
  os << u8"free space, size " << Size();
}


template <class Traits>
void FixedTypedArray<Traits>::FixedTypedArrayPrint(
    v8::base::OStream& os) {  // NOLINT
  os << u8"fixed " << Traits::Designator();
}


void JSObject::PrintProperties(v8::base::OStream& os) {  // NOLINT
  if (HasFastProperties()) {
    DescriptorArray* descs = map()->instance_descriptors();
    for (int i = 0; i < map()->NumberOfOwnDescriptors(); i++) {
      os << u8"\n   ";
      descs->GetKey(i)->NamePrint(os);
      os << u8": ";
      switch (descs->GetType(i)) {
        case DATA: {
          FieldIndex index = FieldIndex::ForDescriptor(map(), i);
          if (IsUnboxedDoubleField(index)) {
            os << u8"<unboxed double> " << RawFastDoublePropertyAt(index);
          } else {
            os << Brief(RawFastPropertyAt(index));
          }
          os << u8" (data field at offset " << index.property_index() << u8")";
          break;
        }
        case ACCESSOR: {
          FieldIndex index = FieldIndex::ForDescriptor(map(), i);
          os << u8" (accessor field at offset " << index.property_index() << u8")";
          break;
        }
        case DATA_CONSTANT:
          os << Brief(descs->GetConstant(i)) << u8" (data constant)";
          break;
        case ACCESSOR_CONSTANT:
          os << Brief(descs->GetCallbacksObject(i)) << u8" (accessor constant)";
          break;
      }
    }
  } else if (IsJSGlobalObject()) {
    global_dictionary()->Print(os);
  } else {
    property_dictionary()->Print(os);
  }
}


template <class T>
static void DoPrintElements(v8::base::OStream& os, Object* object) {  // NOLINT
  T* p = T::cast(object);
  for (int i = 0; i < p->length(); i++) {
    os << u8"\n   " << i << u8": " << p->get_scalar(i);
  }
}


void JSObject::PrintElements(v8::base::OStream& os) {  // NOLINT
  // Don't call GetElementsKind, its validation code can cause the printer to
  // fail when debugging.
  switch (map()->elements_kind()) {
    case FAST_HOLEY_SMI_ELEMENTS:
    case FAST_SMI_ELEMENTS:
    case FAST_HOLEY_ELEMENTS:
    case FAST_ELEMENTS:
    case FAST_STRING_WRAPPER_ELEMENTS: {
      // Print in array notation for non-sparse arrays.
      FixedArray* p = FixedArray::cast(elements());
      for (int i = 0; i < p->length(); i++) {
        os << u8"\n   " << i << u8": " << Brief(p->get(i));
      }
      break;
    }
    case FAST_HOLEY_DOUBLE_ELEMENTS:
    case FAST_DOUBLE_ELEMENTS: {
      // Print in array notation for non-sparse arrays.
      if (elements()->length() > 0) {
        FixedDoubleArray* p = FixedDoubleArray::cast(elements());
        for (int i = 0; i < p->length(); i++) {
          os << u8"\n   " << i << u8": ";
          if (p->is_the_hole(i)) {
            os << u8"<the hole>";
          } else {
            os << p->get_scalar(i);
          }
        }
      }
      break;
    }


#define PRINT_ELEMENTS(Kind, Type)         \
  case Kind: {                             \
    DoPrintElements<Type>(os, elements()); \
    break;                                 \
  }

    PRINT_ELEMENTS(UINT8_ELEMENTS, FixedUint8Array)
    PRINT_ELEMENTS(UINT8_CLAMPED_ELEMENTS, FixedUint8ClampedArray)
    PRINT_ELEMENTS(INT8_ELEMENTS, FixedInt8Array)
    PRINT_ELEMENTS(UINT16_ELEMENTS, FixedUint16Array)
    PRINT_ELEMENTS(INT16_ELEMENTS, FixedInt16Array)
    PRINT_ELEMENTS(UINT32_ELEMENTS, FixedUint32Array)
    PRINT_ELEMENTS(INT32_ELEMENTS, FixedInt32Array)
    PRINT_ELEMENTS(FLOAT32_ELEMENTS, FixedFloat32Array)
    PRINT_ELEMENTS(FLOAT64_ELEMENTS, FixedFloat64Array)

#undef PRINT_ELEMENTS

    case DICTIONARY_ELEMENTS:
    case SLOW_STRING_WRAPPER_ELEMENTS:
      os << u8"\n - elements: ";
      elements()->Print(os);
      break;
    case FAST_SLOPPY_ARGUMENTS_ELEMENTS:
    case SLOW_SLOPPY_ARGUMENTS_ELEMENTS: {
      FixedArray* p = FixedArray::cast(elements());
      os << u8"\n   parameter map:";
      for (int i = 2; i < p->length(); i++) {
        os << u8" " << (i - 2) << u8":" << Brief(p->get(i));
      }
      os << u8"\n   context: " << Brief(p->get(0))
         << u8"\n   arguments: " << Brief(p->get(1));
      break;
    }
    case NO_ELEMENTS:
      break;
  }
}


static void JSObjectPrintHeader(v8::base::OStream& os, JSObject* obj,
                                const char* id) {  // NOLINT
  obj->PrintHeader(os, id);
  // Don't call GetElementsKind, its validation code can cause the printer to
  // fail when debugging.
  os << u8"\n - map = " << reinterpret_cast<void*>(obj->map()) << u8" ["
     << ElementsKindToString(obj->map()->elements_kind());
  if (obj->elements()->map() == obj->GetHeap()->fixed_cow_array_map()) {
    os << u8" (COW)";
  }
  PrototypeIterator iter(obj->GetIsolate(), obj);
  os << u8"]\n - prototype = " << reinterpret_cast<void*>(iter.GetCurrent());
  if (obj->elements()->length() > 0) {
    os << u8"\n - elements = " << Brief(obj->elements());
  }
}


static void JSObjectPrintBody(v8::base::OStream& os, JSObject* obj,  // NOLINT
                              bool print_elements = true) {
  os << u8"\n {";
  obj->PrintProperties(os);
  obj->PrintTransitions(os);
  if (print_elements) obj->PrintElements(os);
  os << u8"\n }\n";
}


void JSObject::JSObjectPrint(v8::base::OStream& os) {  // NOLINT
  JSObjectPrintHeader(os, this, u8"JSObject");
  JSObjectPrintBody(os, this);
}


void JSRegExp::JSRegExpPrint(v8::base::OStream& os) {  // NOLINT
  JSObjectPrintHeader(os, this, u8"JSRegExp");
  os << u8"\n - data = " << Brief(data());
  JSObjectPrintBody(os, this);
}


void JSModule::JSModulePrint(v8::base::OStream& os) {  // NOLINT
  JSObjectPrintHeader(os, this, u8"JSModule");
  os << u8"\n - context = " << Brief(context());
  os << u8" - scope_info = " << Brief(scope_info());
  JSObjectPrintBody(os, this);
}


void Symbol::SymbolPrint(v8::base::OStream& os) {  // NOLINT
  HeapObject::PrintHeader(os, u8"Symbol");
  os << u8"\n - hash: " << Hash();
  os << u8"\n - name: " << Brief(name());
  if (name()->IsUndefined()) {
    os << u8" (" << PrivateSymbolToName() << u8")";
  }
  os << u8"\n - private: " << is_private();
  os << u8"\n";
}


void Map::MapPrint(v8::base::OStream& os) {  // NOLINT
  HeapObject::PrintHeader(os, u8"Map");
  os << u8"\n - type: " << instance_type();
  os << u8"\n - instance size: " << instance_size();
  if (IsJSObjectMap()) {
    os << u8"\n - inobject properties: " << GetInObjectProperties();
  }
  os << u8"\n - elements kind: " << ElementsKindToString(elements_kind());
  os << u8"\n - unused property fields: " << unused_property_fields();
  os << u8"\n - enum length: ";
  if (EnumLength() == kInvalidEnumCacheSentinel) {
    os << u8"invalid";
  } else {
    os << EnumLength();
  }
  if (is_deprecated()) os << u8"\n - deprecated_map";
  if (is_stable()) os << u8"\n - stable_map";
  if (is_dictionary_map()) os << u8"\n - dictionary_map";
  if (has_hidden_prototype()) os << u8"\n - has_hidden_prototype";
  if (has_named_interceptor()) os << u8" - named_interceptor";
  if (has_indexed_interceptor()) os << u8"\n - indexed_interceptor";
  if (is_undetectable()) os << u8"\n - undetectable";
  if (is_callable()) os << u8"\n - callable";
  if (is_constructor()) os << u8"\n - constructor";
  if (is_access_check_needed()) os << u8"\n - access_check_needed";
  if (!is_extensible()) os << u8"\n - non-extensible";
  if (is_observed()) os << u8"\n - observed";
  if (is_prototype_map()) {
    os << u8"\n - prototype_map";
    os << u8"\n - prototype info: " << Brief(prototype_info());
  } else {
    os << u8"\n - back pointer: " << Brief(GetBackPointer());
  }
  os << u8"\n - instance descriptors " << (owns_descriptors() ? u8"(own) " : u8"")
     << u8"#" << NumberOfOwnDescriptors() << u8": "
     << Brief(instance_descriptors());
  if (FLAG_unbox_double_fields) {
    os << u8"\n - layout descriptor: " << Brief(layout_descriptor());
  }
  int nof_transitions = TransitionArray::NumberOfTransitions(raw_transitions());
  if (nof_transitions > 0) {
    os << u8"\n - transitions #" << nof_transitions << u8": "
       << Brief(raw_transitions());
    TransitionArray::PrintTransitions(os, raw_transitions(), false);
  }
  os << u8"\n - prototype: " << Brief(prototype());
  os << u8"\n - constructor: " << Brief(GetConstructor());
  os << u8"\n - code cache: " << Brief(code_cache());
  os << u8"\n - dependent code: " << Brief(dependent_code());
  os << u8"\n - construction counter: " << construction_counter();
  os << u8"\n";
}


void CodeCache::CodeCachePrint(v8::base::OStream& os) {  // NOLINT
  HeapObject::PrintHeader(os, u8"CodeCache");
  os << u8"\n - default_cache: " << Brief(default_cache());
  os << u8"\n - normal_type_cache: " << Brief(normal_type_cache());
}


void PolymorphicCodeCache::PolymorphicCodeCachePrint(
    v8::base::OStream& os) {  // NOLINT
  HeapObject::PrintHeader(os, u8"PolymorphicCodeCache");
  os << u8"\n - cache: " << Brief(cache());
}


void TypeFeedbackInfo::TypeFeedbackInfoPrint(v8::base::OStream& os) {  // NOLINT
  HeapObject::PrintHeader(os, u8"TypeFeedbackInfo");
  os << u8"\n - ic_total_count: " << ic_total_count()
     << u8", ic_with_type_info_count: " << ic_with_type_info_count()
     << u8", ic_generic_count: " << ic_generic_count() << u8"\n";
}


void AliasedArgumentsEntry::AliasedArgumentsEntryPrint(
    v8::base::OStream& os) {  // NOLINT
  HeapObject::PrintHeader(os, u8"AliasedArgumentsEntry");
  os << u8"\n - aliased_context_slot: " << aliased_context_slot();
}


void FixedArray::FixedArrayPrint(v8::base::OStream& os) {  // NOLINT
  HeapObject::PrintHeader(os, u8"FixedArray");
  os << u8"\n - length: " << length();
  for (int i = 0; i < length(); i++) {
    os << u8"\n  [" << i << u8"]: " << Brief(get(i));
  }
  os << u8"\n";
}


void FixedDoubleArray::FixedDoubleArrayPrint(v8::base::OStream& os) {  // NOLINT
  HeapObject::PrintHeader(os, u8"FixedDoubleArray");
  os << u8"\n - length: " << length();
  for (int i = 0; i < length(); i++) {
    os << u8"\n  [" << i << u8"]: ";
    if (is_the_hole(i)) {
      os << u8"<the hole>";
    } else {
      os << get_scalar(i);
    }
  }
  os << u8"\n";
}


void TransitionArray::TransitionArrayPrint(v8::base::OStream& os) {  // NOLINT
  HeapObject::PrintHeader(os, u8"TransitionArray");
  os << u8"\n - capacity: " << length();
  for (int i = 0; i < length(); i++) {
    os << u8"\n  [" << i << u8"]: " << Brief(get(i));
    if (i == kNextLinkIndex) os << u8" (next link)";
    if (i == kPrototypeTransitionsIndex) os << u8" (prototype transitions)";
    if (i == kTransitionLengthIndex) os << u8" (number of transitions)";
  }
  os << u8"\n";
}


void TypeFeedbackMetadata::Print() {
  OFStream os(stdout);
  TypeFeedbackMetadataPrint(os);
  os << std::flush;
}


void TypeFeedbackMetadata::TypeFeedbackMetadataPrint(
    v8::base::OStream& os) {  // NOLINT
  HeapObject::PrintHeader(os, u8"TypeFeedbackMetadata");
  os << u8"\n - length: " << length();
  if (length() == 0) {
    os << u8" (empty)\n";
    return;
  }

  TypeFeedbackMetadataIterator iter(this);
  while (iter.HasNext()) {
    FeedbackVectorSlot slot = iter.Next();
    FeedbackVectorSlotKind kind = iter.kind();
    os << u8"\n Slot " << slot << u8" " << kind;
  }
  os << u8"\n";
}


void TypeFeedbackVector::Print() {
  OFStream os(stdout);
  TypeFeedbackVectorPrint(os);
  os << std::flush;
}


void TypeFeedbackVector::TypeFeedbackVectorPrint(v8::base::OStream& os) {  // NOLINT
  HeapObject::PrintHeader(os, u8"TypeFeedbackVector");
  os << u8"\n - length: " << length();
  if (length() == 0) {
    os << u8" (empty)\n";
    return;
  }

  TypeFeedbackMetadataIterator iter(metadata());
  while (iter.HasNext()) {
    FeedbackVectorSlot slot = iter.Next();
    FeedbackVectorSlotKind kind = iter.kind();

    os << u8"\n Slot " << slot << u8" " << kind << u8" ";
    switch (kind) {
      case FeedbackVectorSlotKind::LOAD_IC: {
        LoadICNexus nexus(this, slot);
        os << Code::ICState2String(nexus.StateFromFeedback());
        break;
      }
      case FeedbackVectorSlotKind::KEYED_LOAD_IC: {
        KeyedLoadICNexus nexus(this, slot);
        os << Code::ICState2String(nexus.StateFromFeedback());
        break;
      }
      case FeedbackVectorSlotKind::CALL_IC: {
        CallICNexus nexus(this, slot);
        os << Code::ICState2String(nexus.StateFromFeedback());
        break;
      }
      case FeedbackVectorSlotKind::STORE_IC: {
        StoreICNexus nexus(this, slot);
        os << Code::ICState2String(nexus.StateFromFeedback());
        break;
      }
      case FeedbackVectorSlotKind::KEYED_STORE_IC: {
        KeyedStoreICNexus nexus(this, slot);
        os << Code::ICState2String(nexus.StateFromFeedback());
        break;
      }
      case FeedbackVectorSlotKind::GENERAL:
        break;
      case FeedbackVectorSlotKind::INVALID:
      case FeedbackVectorSlotKind::KINDS_NUMBER:
        UNREACHABLE();
        break;
    }

    int entry_size = iter.entry_size();
    for (int i = 0; i < entry_size; i++) {
      int index = GetIndex(slot) + i;
      os << u8"\n  [" << index << u8"]: " << Brief(get(index));
    }
  }
  os << u8"\n";
}


void JSValue::JSValuePrint(v8::base::OStream& os) {  // NOLINT
  JSObjectPrintHeader(os, this, u8"JSValue");
  os << u8"\n - value = " << Brief(value());
  JSObjectPrintBody(os, this);
}


void JSMessageObject::JSMessageObjectPrint(v8::base::OStream& os) {  // NOLINT
  JSObjectPrintHeader(os, this, u8"JSMessageObject");
  os << u8"\n - type: " << type();
  os << u8"\n - arguments: " << Brief(argument());
  os << u8"\n - start_position: " << start_position();
  os << u8"\n - end_position: " << end_position();
  os << u8"\n - script: " << Brief(script());
  os << u8"\n - stack_frames: " << Brief(stack_frames());
  JSObjectPrintBody(os, this);
}


void String::StringPrint(v8::base::OStream& os) {  // NOLINT
  if (StringShape(this).IsInternalized()) {
    os << u8"#";
  } else if (StringShape(this).IsCons()) {
    os << u8"c\"";
  } else {
    os << u8"\"";
  }

  const char truncated_epilogue[] = u8"...<truncated>";
  int len = length();
  if (!FLAG_use_verbose_printer) {
    if (len > 100) {
      len = 100 - sizeof(truncated_epilogue);
    }
  }
  for (int i = 0; i < len; i++) {
    os << AsUC16(Get(i));
  }
  if (len != length()) {
    os << truncated_epilogue;
  }

  if (!StringShape(this).IsInternalized()) os << u8"\"";
}


void Name::NamePrint(v8::base::OStream& os) {  // NOLINT
  if (IsString()) {
    String::cast(this)->StringPrint(os);
  } else if (IsSymbol()) {
    Symbol::cast(this)->name()->Print(os);
  } else {
    os << Brief(this);
  }
}


static const char* const weekdays[] = {
  u8"???", u8"Sun", u8"Mon", u8"Tue", u8"Wed", u8"Thu", u8"Fri", u8"Sat"
};


void JSDate::JSDatePrint(v8::base::OStream& os) {  // NOLINT
  JSObjectPrintHeader(os, this, u8"JSDate");
  os << u8"\n - value = " << Brief(value());
  if (!year()->IsSmi()) {
    os << u8"\n - time = NaN\n";
  } else {
    // TODO(svenpanne) Add some basic formatting to our streams.
    ScopedVector<char> buf(100);
    SNPrintF(
        buf, u8"\n - time = %s %04d/%02d/%02d %02d:%02d:%02d\n",
        weekdays[weekday()->IsSmi() ? Smi::cast(weekday())->value() + 1 : 0],
        year()->IsSmi() ? Smi::cast(year())->value() : -1,
        month()->IsSmi() ? Smi::cast(month())->value() : -1,
        day()->IsSmi() ? Smi::cast(day())->value() : -1,
        hour()->IsSmi() ? Smi::cast(hour())->value() : -1,
        min()->IsSmi() ? Smi::cast(min())->value() : -1,
        sec()->IsSmi() ? Smi::cast(sec())->value() : -1);
    os << buf.start();
  }
  JSObjectPrintBody(os, this);
}


void JSProxy::JSProxyPrint(v8::base::OStream& os) {  // NOLINT
  HeapObject::PrintHeader(os, u8"JSProxy");
  os << u8"\n - map = " << reinterpret_cast<void*>(map());
  os << u8"\n - target = ";
  target()->ShortPrint(os);
  os << u8"\n - handler = ";
  handler()->ShortPrint(os);
  os << u8"\n - hash = ";
  hash()->ShortPrint(os);
  os << u8"\n";
}


void JSSet::JSSetPrint(v8::base::OStream& os) {  // NOLINT
  JSObjectPrintHeader(os, this, u8"JSSet");
  os << u8" - table = " << Brief(table());
  JSObjectPrintBody(os, this);
}


void JSMap::JSMapPrint(v8::base::OStream& os) {  // NOLINT
  JSObjectPrintHeader(os, this, u8"JSMap");
  os << u8" - table = " << Brief(table());
  JSObjectPrintBody(os, this);
}


template <class Derived, class TableType>
void
OrderedHashTableIterator<Derived, TableType>::OrderedHashTableIteratorPrint(
    v8::base::OStream& os) {  // NOLINT
  os << u8"\n - table = " << Brief(table());
  os << u8"\n - index = " << Brief(index());
  os << u8"\n - kind = " << Brief(kind());
  os << u8"\n";
}


template void OrderedHashTableIterator<
    JSSetIterator,
    OrderedHashSet>::OrderedHashTableIteratorPrint(v8::base::OStream& os);  // NOLINT


template void OrderedHashTableIterator<
    JSMapIterator,
    OrderedHashMap>::OrderedHashTableIteratorPrint(v8::base::OStream& os);  // NOLINT


void JSSetIterator::JSSetIteratorPrint(v8::base::OStream& os) {  // NOLINT
  JSObjectPrintHeader(os, this, u8"JSSetIterator");
  OrderedHashTableIteratorPrint(os);
}


void JSMapIterator::JSMapIteratorPrint(v8::base::OStream& os) {  // NOLINT
  JSObjectPrintHeader(os, this, u8"JSMapIterator");
  OrderedHashTableIteratorPrint(os);
}


void JSWeakMap::JSWeakMapPrint(v8::base::OStream& os) {  // NOLINT
  JSObjectPrintHeader(os, this, u8"JSWeakMap");
  os << u8"\n - table = " << Brief(table());
  JSObjectPrintBody(os, this);
}


void JSWeakSet::JSWeakSetPrint(v8::base::OStream& os) {  // NOLINT
  JSObjectPrintHeader(os, this, u8"JSWeakSet");
  os << u8"\n - table = " << Brief(table());
  JSObjectPrintBody(os, this);
}


void JSArrayBuffer::JSArrayBufferPrint(v8::base::OStream& os) {  // NOLINT
  JSObjectPrintHeader(os, this, u8"JSArrayBuffer");
  os << u8"\n - backing_store = " << backing_store();
  os << u8"\n - byte_length = " << Brief(byte_length());
  if (was_neutered()) os << u8" - neutered\n";
  JSObjectPrintBody(os, this, !was_neutered());
}


void JSTypedArray::JSTypedArrayPrint(v8::base::OStream& os) {  // NOLINT
  JSObjectPrintHeader(os, this, u8"JSTypedArray");
  os << u8"\n - buffer = " << Brief(buffer());
  os << u8"\n - byte_offset = " << Brief(byte_offset());
  os << u8"\n - byte_length = " << Brief(byte_length());
  os << u8"\n - length = " << Brief(length());
  if (WasNeutered()) os << u8" - neutered\n";
  JSObjectPrintBody(os, this, !WasNeutered());
}


void JSDataView::JSDataViewPrint(v8::base::OStream& os) {  // NOLINT
  JSObjectPrintHeader(os, this, u8"JSDataView");
  os << u8"\n - buffer =" << Brief(buffer());
  os << u8"\n - byte_offset = " << Brief(byte_offset());
  os << u8"\n - byte_length = " << Brief(byte_length());
  if (WasNeutered()) os << u8" - neutered\n";
  JSObjectPrintBody(os, this, !WasNeutered());
}


void JSBoundFunction::JSBoundFunctionPrint(v8::base::OStream& os) {  // NOLINT
  JSObjectPrintHeader(os, this, u8"JSBoundFunction");
  os << u8"\n - bound_target_function = " << Brief(bound_target_function());
  os << u8"\n - bound_this = " << Brief(bound_this());
  os << u8"\n - bound_arguments = " << Brief(bound_arguments());
  JSObjectPrintBody(os, this);
}


void JSFunction::JSFunctionPrint(v8::base::OStream& os) {  // NOLINT
  JSObjectPrintHeader(os, this, u8"Function");
  os << u8"\n - initial_map = ";
  if (has_initial_map()) os << Brief(initial_map());
  os << u8"\n - shared_info = " << Brief(shared());
  os << u8"\n - name = " << Brief(shared()->name());
  os << u8"\n - formal_parameter_count = "
     << shared()->internal_formal_parameter_count();
  if (shared()->is_generator()) {
    os << u8"\n   - generator";
  }
  os << u8"\n - context = " << Brief(context());
  os << u8"\n - literals = " << Brief(literals());
  os << u8"\n - code = " << Brief(code());
  JSObjectPrintBody(os, this);
}


void SharedFunctionInfo::SharedFunctionInfoPrint(v8::base::OStream& os) {  // NOLINT
  HeapObject::PrintHeader(os, u8"SharedFunctionInfo");
  os << u8"\n - name = " << Brief(name());
  os << u8"\n - formal_parameter_count = " << internal_formal_parameter_count();
  os << u8"\n - expected_nof_properties = " << expected_nof_properties();
  os << u8"\n - ast_node_count = " << ast_node_count();
  os << u8"\n - instance class name = ";
  instance_class_name()->Print(os);
  os << u8"\n - code = " << Brief(code());
  if (HasSourceCode()) {
    os << u8"\n - source code = ";
    String* source = String::cast(Script::cast(script())->source());
    int start = start_position();
    int length = end_position() - start;
    base::SmartArrayPointer<char> source_string = source->ToCString(
        DISALLOW_NULLS, FAST_STRING_TRAVERSAL, start, length, NULL);
    os << source_string.get();
  }
  // Script files are often large, hard to read.
  // os << "\n - script =";
  // script()->Print(os);
  if (is_named_expression()) {
    os << u8"\n - named expression";
  } else if (is_anonymous_expression()) {
    os << u8"\n - anonymous expression";
  } else if (is_declaration()) {
    os << u8"\n - declaration";
  }
  os << u8"\n - function token position = " << function_token_position();
  os << u8"\n - start position = " << start_position();
  os << u8"\n - end position = " << end_position();
  os << u8"\n - debug info = " << Brief(debug_info());
  os << u8"\n - length = " << length();
  os << u8"\n - optimized_code_map = " << Brief(optimized_code_map());
  os << u8"\n - feedback_vector = ";
  feedback_vector()->TypeFeedbackVectorPrint(os);
  if (HasBytecodeArray()) {
    os << u8"\n - bytecode_array = " << bytecode_array();
  }
  os << u8"\n";
}


void JSGlobalProxy::JSGlobalProxyPrint(v8::base::OStream& os) {  // NOLINT
  os << u8"global_proxy ";
  JSObjectPrint(os);
  os << u8"native context : " << Brief(native_context());
  os << u8"\n";
}


void JSGlobalObject::JSGlobalObjectPrint(v8::base::OStream& os) {  // NOLINT
  os << u8"global ";
  JSObjectPrint(os);
  os << u8"native context : " << Brief(native_context());
  os << u8"\n";
}


void Cell::CellPrint(v8::base::OStream& os) {  // NOLINT
  HeapObject::PrintHeader(os, u8"Cell");
  os << u8"\n - value: " << Brief(value());
  os << u8"\n";
}


void PropertyCell::PropertyCellPrint(v8::base::OStream& os) {  // NOLINT
  HeapObject::PrintHeader(os, u8"PropertyCell");
  os << u8"\n - value: " << Brief(value());
  os << u8"\n - details: " << property_details();
  os << u8"\n";
}


void WeakCell::WeakCellPrint(v8::base::OStream& os) {  // NOLINT
  HeapObject::PrintHeader(os, u8"WeakCell");
  if (cleared()) {
    os << u8"\n - cleared";
  } else {
    os << u8"\n - value: " << Brief(value());
  }
  os << u8"\n";
}


void Code::CodePrint(v8::base::OStream& os) {  // NOLINT
  HeapObject::PrintHeader(os, u8"Code");
  os << u8"\n";
#ifdef ENABLE_DISASSEMBLER
  if (FLAG_use_verbose_printer) {
    Disassemble(NULL, os);
  }
#endif
}


void Foreign::ForeignPrint(v8::base::OStream& os) {  // NOLINT
  os << u8"foreign address : " << foreign_address();
  os << u8"\n";
}


void AccessorInfo::AccessorInfoPrint(v8::base::OStream& os) {  // NOLINT
  HeapObject::PrintHeader(os, u8"AccessorInfo");
  os << u8"\n - name: " << Brief(name());
  os << u8"\n - flag: " << flag();
  os << u8"\n - getter: " << Brief(getter());
  os << u8"\n - setter: " << Brief(setter());
  os << u8"\n - data: " << Brief(data());
  os << u8"\n";
}


void Box::BoxPrint(v8::base::OStream& os) {  // NOLINT
  HeapObject::PrintHeader(os, u8"Box");
  os << u8"\n - value: " << Brief(value());
  os << u8"\n";
}


void PrototypeInfo::PrototypeInfoPrint(v8::base::OStream& os) {  // NOLINT
  HeapObject::PrintHeader(os, u8"PrototypeInfo");
  os << u8"\n - prototype users: " << Brief(prototype_users());
  os << u8"\n - registry slot: " << registry_slot();
  os << u8"\n - validity cell: " << Brief(validity_cell());
  os << u8"\n";
}


void SloppyBlockWithEvalContextExtension::
    SloppyBlockWithEvalContextExtensionPrint(v8::base::OStream& os) {  // NOLINT
  HeapObject::PrintHeader(os, u8"SloppyBlockWithEvalContextExtension");
  os << u8"\n - scope_info: " << Brief(scope_info());
  os << u8"\n - extension: " << Brief(extension());
  os << u8"\n";
}


void AccessorPair::AccessorPairPrint(v8::base::OStream& os) {  // NOLINT
  HeapObject::PrintHeader(os, u8"AccessorPair");
  os << u8"\n - getter: " << Brief(getter());
  os << u8"\n - setter: " << Brief(setter());
  os << u8"\n";
}


void AccessCheckInfo::AccessCheckInfoPrint(v8::base::OStream& os) {  // NOLINT
  HeapObject::PrintHeader(os, u8"AccessCheckInfo");
  os << u8"\n - named_callback: " << Brief(named_callback());
  os << u8"\n - indexed_callback: " << Brief(indexed_callback());
  os << u8"\n - callback: " << Brief(callback());
  os << u8"\n - data: " << Brief(data());
  os << u8"\n";
}


void InterceptorInfo::InterceptorInfoPrint(v8::base::OStream& os) {  // NOLINT
  HeapObject::PrintHeader(os, u8"InterceptorInfo");
  os << u8"\n - getter: " << Brief(getter());
  os << u8"\n - setter: " << Brief(setter());
  os << u8"\n - query: " << Brief(query());
  os << u8"\n - deleter: " << Brief(deleter());
  os << u8"\n - enumerator: " << Brief(enumerator());
  os << u8"\n - data: " << Brief(data());
  os << u8"\n";
}


void CallHandlerInfo::CallHandlerInfoPrint(v8::base::OStream& os) {  // NOLINT
  HeapObject::PrintHeader(os, u8"CallHandlerInfo");
  os << u8"\n - callback: " << Brief(callback());
  os << u8"\n - data: " << Brief(data());
  os << u8"\n";
}


void FunctionTemplateInfo::FunctionTemplateInfoPrint(
    v8::base::OStream& os) {  // NOLINT
  HeapObject::PrintHeader(os, u8"FunctionTemplateInfo");
  os << u8"\n - class name: " << Brief(class_name());
  os << u8"\n - tag: " << Brief(tag());
  os << u8"\n - serial_number: " << Brief(serial_number());
  os << u8"\n - property_list: " << Brief(property_list());
  os << u8"\n - call_code: " << Brief(call_code());
  os << u8"\n - property_accessors: " << Brief(property_accessors());
  os << u8"\n - prototype_template: " << Brief(prototype_template());
  os << u8"\n - parent_template: " << Brief(parent_template());
  os << u8"\n - named_property_handler: " << Brief(named_property_handler());
  os << u8"\n - indexed_property_handler: " << Brief(indexed_property_handler());
  os << u8"\n - instance_template: " << Brief(instance_template());
  os << u8"\n - signature: " << Brief(signature());
  os << u8"\n - access_check_info: " << Brief(access_check_info());
  os << u8"\n - hidden_prototype: " << (hidden_prototype() ? u8"true" : u8"false");
  os << u8"\n - undetectable: " << (undetectable() ? u8"true" : u8"false");
  os << u8"\n - need_access_check: " << (needs_access_check() ? u8"true" : u8"false");
  os << u8"\n - instantiated: " << (instantiated() ? u8"true" : u8"false");
  os << u8"\n";
}


void ObjectTemplateInfo::ObjectTemplateInfoPrint(v8::base::OStream& os) {  // NOLINT
  HeapObject::PrintHeader(os, u8"ObjectTemplateInfo");
  os << u8"\n - tag: " << Brief(tag());
  os << u8"\n - serial_number: " << Brief(serial_number());
  os << u8"\n - property_list: " << Brief(property_list());
  os << u8"\n - property_accessors: " << Brief(property_accessors());
  os << u8"\n - constructor: " << Brief(constructor());
  os << u8"\n - internal_field_count: " << Brief(internal_field_count());
  os << u8"\n";
}


void AllocationSite::AllocationSitePrint(v8::base::OStream& os) {  // NOLINT
  HeapObject::PrintHeader(os, u8"AllocationSite");
  os << u8"\n - weak_next: " << Brief(weak_next());
  os << u8"\n - dependent code: " << Brief(dependent_code());
  os << u8"\n - nested site: " << Brief(nested_site());
  os << u8"\n - memento found count: "
     << Brief(Smi::FromInt(memento_found_count()));
  os << u8"\n - memento create count: "
     << Brief(Smi::FromInt(memento_create_count()));
  os << u8"\n - pretenure decision: "
     << Brief(Smi::FromInt(pretenure_decision()));
  os << u8"\n - transition_info: ";
  if (transition_info()->IsSmi()) {
    ElementsKind kind = GetElementsKind();
    os << u8"Array allocation with ElementsKind " << ElementsKindToString(kind);
  } else if (transition_info()->IsJSArray()) {
    os << u8"Array literal " << Brief(transition_info());
  } else {
    os << u8"unknown transition_info" << Brief(transition_info());
  }
  os << u8"\n";
}


void AllocationMemento::AllocationMementoPrint(v8::base::OStream& os) {  // NOLINT
  HeapObject::PrintHeader(os, u8"AllocationMemento");
  os << u8"\n - allocation site: ";
  if (IsValid()) {
    GetAllocationSite()->Print(os);
  } else {
    os << u8"<invalid>\n";
  }
}


void Script::ScriptPrint(v8::base::OStream& os) {  // NOLINT
  HeapObject::PrintHeader(os, u8"Script");
  os << u8"\n - source: " << Brief(source());
  os << u8"\n - name: " << Brief(name());
  os << u8"\n - line_offset: " << line_offset();
  os << u8"\n - column_offset: " << column_offset();
  os << u8"\n - type: " << type();
  os << u8"\n - id: " << id();
  os << u8"\n - context data: " << Brief(context_data());
  os << u8"\n - wrapper: " << Brief(wrapper());
  os << u8"\n - compilation type: " << compilation_type();
  os << u8"\n - line ends: " << Brief(line_ends());
  os << u8"\n - eval from shared: " << Brief(eval_from_shared());
  os << u8"\n - eval from instructions offset: "
     << eval_from_instructions_offset();
  os << u8"\n - shared function infos: " << Brief(shared_function_infos());
  os << u8"\n";
}


void DebugInfo::DebugInfoPrint(v8::base::OStream& os) {  // NOLINT
  HeapObject::PrintHeader(os, u8"DebugInfo");
  os << u8"\n - shared: " << Brief(shared());
  os << u8"\n - code: " << Brief(abstract_code());
  os << u8"\n - break_points: ";
  break_points()->Print(os);
}


void BreakPointInfo::BreakPointInfoPrint(v8::base::OStream& os) {  // NOLINT
  HeapObject::PrintHeader(os, u8"BreakPointInfo");
  os << u8"\n - code_offset: " << code_offset();
  os << u8"\n - source_position: " << source_position();
  os << u8"\n - statement_position: " << statement_position();
  os << u8"\n - break_point_objects: " << Brief(break_point_objects());
  os << u8"\n";
}


static void PrintBitMask(v8::base::OStream& os, uint32_t value) {  // NOLINT
  for (int i = 0; i < 32; i++) {
    if ((i & 7) == 0) os << u8" ";
    os << (((value & 1) == 0) ? u8"_" : u8"x");
    value >>= 1;
  }
}


void LayoutDescriptor::Print() {
  OFStream os(stdout);
  this->Print(os);
  os << std::flush;
}


void LayoutDescriptor::Print(v8::base::OStream& os) {  // NOLINT
  os << u8"Layout descriptor: ";
  if (IsUninitialized()) {
    os << u8"<uninitialized>";
  } else if (IsFastPointerLayout()) {
    os << u8"<all tagged>";
  } else if (IsSmi()) {
    os << u8"fast";
    PrintBitMask(os, static_cast<uint32_t>(Smi::cast(this)->value()));
  } else {
    os << u8"slow";
    int len = length();
    for (int i = 0; i < len; i++) {
      if (i > 0) os << u8" |";
      PrintBitMask(os, get_scalar(i));
    }
  }
  os << u8"\n";
}


#endif  // OBJECT_PRINT


#if TRACE_MAPS


void Name::NameShortPrint() {
  if (this->IsString()) {
    PrintF(u8"%s", String::cast(this)->ToCString().get());
  } else {
    DCHECK(this->IsSymbol());
    Symbol* s = Symbol::cast(this);
    if (s->name()->IsUndefined()) {
      PrintF(u8"#<%s>", s->PrivateSymbolToName());
    } else {
      PrintF(u8"<%s>", String::cast(s->name())->ToCString().get());
    }
  }
}


int Name::NameShortPrint(Vector<char> str) {
  if (this->IsString()) {
    return SNPrintF(str, u8"%s", String::cast(this)->ToCString().get());
  } else {
    DCHECK(this->IsSymbol());
    Symbol* s = Symbol::cast(this);
    if (s->name()->IsUndefined()) {
      return SNPrintF(str, u8"#<%s>", s->PrivateSymbolToName());
    } else {
      return SNPrintF(str, u8"<%s>", String::cast(s->name())->ToCString().get());
    }
  }
}


#endif  // TRACE_MAPS


#if defined(DEBUG) || defined(OBJECT_PRINT)
// This method is only meant to be called from gdb for debugging purposes.
// Since the string can also be in two-byte encoding, non-Latin1 characters
// will be ignored in the output.
char* String::ToAsciiArray() {
  // Static so that subsequent calls frees previously allocated space.
  // This also means that previous results will be overwritten.
  static char* buffer = NULL;
  if (buffer != NULL) delete[] buffer;
  buffer = new char[length() + 1];
  WriteToFlat(this, reinterpret_cast<uint8_t*>(buffer), 0, length());
  buffer[length()] = 0;
  return buffer;
}


void DescriptorArray::Print() {
  OFStream os(stdout);
  this->PrintDescriptors(os);
  os << std::flush;
}


void DescriptorArray::PrintDescriptors(v8::base::OStream& os) {  // NOLINT
  HandleScope scope(GetIsolate());
  os << u8"Descriptor array #" << number_of_descriptors();
  for (int i = 0; i < number_of_descriptors(); i++) {
    Descriptor desc;
    Get(i, &desc);
    os << u8"\n " << i << u8": " << desc;
  }
  os << u8"\n";
}


void TransitionArray::Print() {
  OFStream os(stdout);
  TransitionArray::PrintTransitions(os, this);
  os << u8"\n" << std::flush;
}


void TransitionArray::PrintTransitions(v8::base::OStream& os, Object* transitions,
                                       bool print_header) {  // NOLINT
  int num_transitions = NumberOfTransitions(transitions);
  if (print_header) {
    os << u8"Transition array #" << num_transitions << u8":";
  }
  for (int i = 0; i < num_transitions; i++) {
    Name* key = GetKey(transitions, i);
    Map* target = GetTarget(transitions, i);
    os << u8"\n   ";
#ifdef OBJECT_PRINT
    key->NamePrint(os);
#else
    key->ShortPrint(os);
#endif
    os << u8": ";
    Heap* heap = key->GetHeap();
    if (key == heap->nonextensible_symbol()) {
      os << u8"(transition to non-extensible)";
    } else if (key == heap->sealed_symbol()) {
      os << u8"(transition to sealed)";
    } else if (key == heap->frozen_symbol()) {
      os << u8"(transition to frozen)";
    } else if (key == heap->elements_transition_symbol()) {
      os << u8"(transition to " << ElementsKindToString(target->elements_kind())
         << u8")";
    } else if (key == heap->strict_function_transition_symbol()) {
      os << u8" (transition to strict function)";
    } else if (key == heap->observed_symbol()) {
      os << u8" (transition to Object.observe)";
    } else {
      PropertyDetails details = GetTargetDetails(key, target);
      os << u8"(transition to ";
      if (details.location() == kDescriptor) {
        os << u8"immutable ";
      }
      os << (details.kind() == kData ? u8"data" : u8"accessor");
      if (details.location() == kDescriptor) {
        Object* value =
            target->instance_descriptors()->GetValue(target->LastAdded());
        os << u8" " << Brief(value);
      }
      os << u8"), attrs: " << details.attributes();
    }
    os << u8" -> " << Brief(target);
  }
}


void JSObject::PrintTransitions(v8::base::OStream& os) {  // NOLINT
  Object* transitions = map()->raw_transitions();
  int num_transitions = TransitionArray::NumberOfTransitions(transitions);
  if (num_transitions == 0) return;
  os << u8"\n - transitions";
  TransitionArray::PrintTransitions(os, transitions, false);
}
#endif  // defined(DEBUG) || defined(OBJECT_PRINT)
}  // namespace internal
}  // namespace v8
