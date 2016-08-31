// Copyright 2012 the V8 project authors. All rights reserved.
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.

#include "src/v8.h"

#include "src/disasm.h"
#include "src/disassembler.h"
#include "src/heap/objects-visiting.h"
#include "src/jsregexp.h"
#include "src/ostreams.h"

namespace v8 {
namespace internal {

#ifdef OBJECT_PRINT

void Object::Print() {
  OFStream os(stdout);
  this->Print(os);
  os << flush;
}


void Object::Print(OStream& os) {  // NOLINT
  if (IsSmi()) {
    Smi::cast(this)->SmiPrint(os);
  } else {
    HeapObject::cast(this)->HeapObjectPrint(os);
  }
}


void HeapObject::PrintHeader(OStream& os, const char* id) {  // NOLINT
  os << "" << reinterpret_cast<void*>(this) << "\x3a\x20\x5b" << id << "\x5d\xa";
}


void HeapObject::HeapObjectPrint(OStream& os) {  // NOLINT
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
      os << "\x3c\x6d\x75\x74\x61\x62\x6c\x65\x20";
      HeapNumber::cast(this)->HeapNumberPrint(os);
      os << "\x3e";
      break;
    case FIXED_DOUBLE_ARRAY_TYPE:
      FixedDoubleArray::cast(this)->FixedDoubleArrayPrint(os);
      break;
    case CONSTANT_POOL_ARRAY_TYPE:
      ConstantPoolArray::cast(this)->ConstantPoolArrayPrint(os);
      break;
    case FIXED_ARRAY_TYPE:
      FixedArray::cast(this)->FixedArrayPrint(os);
      break;
    case BYTE_ARRAY_TYPE:
      ByteArray::cast(this)->ByteArrayPrint(os);
      break;
    case FREE_SPACE_TYPE:
      FreeSpace::cast(this)->FreeSpacePrint(os);
      break;

#define PRINT_EXTERNAL_ARRAY(Type, type, TYPE, ctype, size)            \
  case EXTERNAL_##TYPE##_ARRAY_TYPE:                                   \
    External##Type##Array::cast(this)->External##Type##ArrayPrint(os); \
    break;

     TYPED_ARRAYS(PRINT_EXTERNAL_ARRAY)
#undef PRINT_EXTERNAL_ARRAY

#define PRINT_FIXED_TYPED_ARRAY(Type, type, TYPE, ctype, size) \
  case Fixed##Type##Array::kInstanceType:                      \
    Fixed##Type##Array::cast(this)->FixedTypedArrayPrint(os);  \
    break;

    TYPED_ARRAYS(PRINT_FIXED_TYPED_ARRAY)
#undef PRINT_FIXED_TYPED_ARRAY

    case FILLER_TYPE:
      os << "\x66\x69\x6c\x6c\x65\x72";
      break;
    case JS_OBJECT_TYPE:  // fall through
    case JS_CONTEXT_EXTENSION_OBJECT_TYPE:
    case JS_ARRAY_TYPE:
    case JS_GENERATOR_OBJECT_TYPE:
    case JS_REGEXP_TYPE:
      JSObject::cast(this)->JSObjectPrint(os);
      break;
    case ODDBALL_TYPE:
      Oddball::cast(this)->to_string()->Print(os);
      break;
    case JS_MODULE_TYPE:
      JSModule::cast(this)->JSModulePrint(os);
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
    case JS_BUILTINS_OBJECT_TYPE:
      JSBuiltinsObject::cast(this)->JSBuiltinsObjectPrint(os);
      break;
    case JS_VALUE_TYPE:
      os << "\x56\x61\x6c\x75\x65\x20\x77\x72\x61\x70\x70\x65\x72\x20\x61\x72\x6f\x75\x6e\x64\x3a";
      JSValue::cast(this)->value()->Print(os);
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
    case JS_FUNCTION_PROXY_TYPE:
      JSFunctionProxy::cast(this)->JSFunctionProxyPrint(os);
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
      os << "\x55\x4e\x4b\x4e\x4f\x57\x4e\x20\x54\x59\x50\x45\x20" << map()->instance_type();
      UNREACHABLE();
      break;
  }
}


void ByteArray::ByteArrayPrint(OStream& os) {  // NOLINT
  os << "\x62\x79\x74\x65\x20\x61\x72\x72\x61\x79\x2c\x20\x64\x61\x74\x61\x20\x73\x74\x61\x72\x74\x73\x20\x61\x74\x20" << GetDataStartAddress();
}


void FreeSpace::FreeSpacePrint(OStream& os) {  // NOLINT
  os << "\x66\x72\x65\x65\x20\x73\x70\x61\x63\x65\x2c\x20\x73\x69\x7a\x65\x20" << Size();
}


#define EXTERNAL_ARRAY_PRINTER(Type, type, TYPE, ctype, size)           \
  void External##Type##Array::External##Type##ArrayPrint(OStream& os) { \
    os << "\x65\x78\x74\x65\x72\x6e\x61\x6c\x20" #type "\x20\x61\x72\x72\x61\x79";                                   \
  }

TYPED_ARRAYS(EXTERNAL_ARRAY_PRINTER)

#undef EXTERNAL_ARRAY_PRINTER


template <class Traits>
void FixedTypedArray<Traits>::FixedTypedArrayPrint(OStream& os) {  // NOLINT
  os << "\x66\x69\x78\x65\x64\x20" << Traits::Designator();
}


void JSObject::PrintProperties(OStream& os) {  // NOLINT
  if (HasFastProperties()) {
    DescriptorArray* descs = map()->instance_descriptors();
    for (int i = 0; i < map()->NumberOfOwnDescriptors(); i++) {
      os << "\x20\x20\x20";
      descs->GetKey(i)->NamePrint(os);
      os << "\x3a\x20";
      switch (descs->GetType(i)) {
        case FIELD: {
          FieldIndex index = FieldIndex::ForDescriptor(map(), i);
          os << Brief(RawFastPropertyAt(index)) << "\x20\x28\x66\x69\x65\x6c\x64\x20\x61\x74\x20\x6f\x66\x66\x73\x65\x74\x20"
             << index.property_index() << "\x29\xa";
          break;
        }
        case CONSTANT:
          os << Brief(descs->GetConstant(i)) << "\x20\x28\x63\x6f\x6e\x73\x74\x61\x6e\x74\x29\xa";
          break;
        case CALLBACKS:
          os << Brief(descs->GetCallbacksObject(i)) << "\x20\x28\x63\x61\x6c\x6c\x62\x61\x63\x6b\x29\xa";
          break;
        case NORMAL:  // only in slow mode
        case HANDLER:  // only in lookup results, not in descriptors
        case INTERCEPTOR:  // only in lookup results, not in descriptors
        // There are no transitions in the descriptor array.
        case NONEXISTENT:
          UNREACHABLE();
          break;
      }
    }
  } else {
    property_dictionary()->Print(os);
  }
}


template <class T>
static void DoPrintElements(OStream& os, Object* object) {  // NOLINT
  T* p = T::cast(object);
  for (int i = 0; i < p->length(); i++) {
    os << "\x20\x20\x20" << i << "\x3a\x20" << p->get_scalar(i) << "\xa";
  }
}


void JSObject::PrintElements(OStream& os) {  // NOLINT
  // Don't call GetElementsKind, its validation code can cause the printer to
  // fail when debugging.
  switch (map()->elements_kind()) {
    case FAST_HOLEY_SMI_ELEMENTS:
    case FAST_SMI_ELEMENTS:
    case FAST_HOLEY_ELEMENTS:
    case FAST_ELEMENTS: {
      // Print in array notation for non-sparse arrays.
      FixedArray* p = FixedArray::cast(elements());
      for (int i = 0; i < p->length(); i++) {
        os << "\x20\x20\x20" << i << "\x3a\x20" << Brief(p->get(i)) << "\xa";
      }
      break;
    }
    case FAST_HOLEY_DOUBLE_ELEMENTS:
    case FAST_DOUBLE_ELEMENTS: {
      // Print in array notation for non-sparse arrays.
      if (elements()->length() > 0) {
        FixedDoubleArray* p = FixedDoubleArray::cast(elements());
        for (int i = 0; i < p->length(); i++) {
          os << "\x20\x20\x20" << i << "\x3a\x20";
          if (p->is_the_hole(i)) {
            os << "\x3c\x74\x68\x65\x20\x68\x6f\x6c\x65\x3e";
          } else {
            os << p->get_scalar(i);
          }
          os << "\xa";
        }
      }
      break;
    }


#define PRINT_ELEMENTS(Kind, Type)         \
  case Kind: {                             \
    DoPrintElements<Type>(os, elements()); \
    break;                                 \
  }

    PRINT_ELEMENTS(EXTERNAL_UINT8_CLAMPED_ELEMENTS, ExternalUint8ClampedArray)
    PRINT_ELEMENTS(EXTERNAL_INT8_ELEMENTS, ExternalInt8Array)
    PRINT_ELEMENTS(EXTERNAL_UINT8_ELEMENTS,
        ExternalUint8Array)
    PRINT_ELEMENTS(EXTERNAL_INT16_ELEMENTS, ExternalInt16Array)
    PRINT_ELEMENTS(EXTERNAL_UINT16_ELEMENTS,
        ExternalUint16Array)
    PRINT_ELEMENTS(EXTERNAL_INT32_ELEMENTS, ExternalInt32Array)
    PRINT_ELEMENTS(EXTERNAL_UINT32_ELEMENTS,
        ExternalUint32Array)
    PRINT_ELEMENTS(EXTERNAL_FLOAT32_ELEMENTS, ExternalFloat32Array)
    PRINT_ELEMENTS(EXTERNAL_FLOAT64_ELEMENTS, ExternalFloat64Array)

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
      elements()->Print(os);
      break;
    case SLOPPY_ARGUMENTS_ELEMENTS: {
      FixedArray* p = FixedArray::cast(elements());
      os << "\x20\x20\x20\x70\x61\x72\x61\x6d\x65\x74\x65\x72\x20\x6d\x61\x70\x3a";
      for (int i = 2; i < p->length(); i++) {
        os << "\x20" << (i - 2) << "\x3a" << Brief(p->get(i));
      }
      os << "\xa\x20\x20\x20\x63\x6f\x6e\x74\x65\x78\x74\x3a\x20" << Brief(p->get(0))
         << "\xa\x20\x20\x20\x61\x72\x67\x75\x6d\x65\x6e\x74\x73\x3a\x20" << Brief(p->get(1)) << "\xa";
      break;
    }
  }
}


void JSObject::PrintTransitions(OStream& os) {  // NOLINT
  if (!map()->HasTransitionArray()) return;
  TransitionArray* transitions = map()->transitions();
  for (int i = 0; i < transitions->number_of_transitions(); i++) {
    Name* key = transitions->GetKey(i);
    os << "\x20\x20\x20";
    key->NamePrint(os);
    os << "\x3a\x20";
    if (key == GetHeap()->frozen_symbol()) {
      os << "\x20\x28\x74\x72\x61\x6e\x73\x69\x74\x69\x6f\x6e\x20\x74\x6f\x20\x66\x72\x6f\x7a\x65\x6e\x29\xa";
    } else if (key == GetHeap()->elements_transition_symbol()) {
      os << "\x20\x28\x74\x72\x61\x6e\x73\x69\x74\x69\x6f\x6e\x20\x74\x6f\x20"
         << ElementsKindToString(transitions->GetTarget(i)->elements_kind())
         << "\x29\xa";
    } else if (key == GetHeap()->observed_symbol()) {
      os << "\x20\x28\x74\x72\x61\x6e\x73\x69\x74\x69\x6f\x6e\x20\x74\x6f\x20\x4f\x62\x6a\x65\x63\x74\x2e\x6f\x62\x73\x65\x72\x76\x65\x29\xa";
    } else {
      switch (transitions->GetTargetDetails(i).type()) {
        case FIELD: {
          os << "\x20\x28\x74\x72\x61\x6e\x73\x69\x74\x69\x6f\x6e\x20\x74\x6f\x20\x66\x69\x65\x6c\x64\x29\xa";
          break;
        }
        case CONSTANT:
          os << "\x20\x28\x74\x72\x61\x6e\x73\x69\x74\x69\x6f\x6e\x20\x74\x6f\x20\x63\x6f\x6e\x73\x74\x61\x6e\x74\x29\xa";
          break;
        case CALLBACKS:
          os << "\x20\x28\x74\x72\x61\x6e\x73\x69\x74\x69\x6f\x6e\x20\x74\x6f\x20\x63\x61\x6c\x6c\x62\x61\x63\x6b\x29\xa";
          break;
        // Values below are never in the target descriptor array.
        case NORMAL:
        case HANDLER:
        case INTERCEPTOR:
        case NONEXISTENT:
          UNREACHABLE();
          break;
      }
    }
  }
}


void JSObject::JSObjectPrint(OStream& os) {  // NOLINT
  HeapObject::PrintHeader(os, "\x4a\x53\x4f\x62\x6a\x65\x63\x74");
  // Don't call GetElementsKind, its validation code can cause the printer to
  // fail when debugging.
  PrototypeIterator iter(GetIsolate(), this);
  os << "\x20\x2d\x20\x6d\x61\x70\x20\x3d\x20" << reinterpret_cast<void*>(map()) << "\x20\x5b"
     << ElementsKindToString(this->map()->elements_kind())
     << "\x5d\xa\x20\x2d\x20\x70\x72\x6f\x74\x6f\x74\x79\x70\x65\x20\x3d\x20" << reinterpret_cast<void*>(iter.GetCurrent())
     << "\xa\x20\x7b\xa";
  PrintProperties(os);
  PrintTransitions(os);
  PrintElements(os);
  os << "\x20\x7d\xa";
}


void JSModule::JSModulePrint(OStream& os) {  // NOLINT
  HeapObject::PrintHeader(os, "\x4a\x53\x4d\x6f\x64\x75\x6c\x65");
  os << "\x20\x2d\x20\x6d\x61\x70\x20\x3d\x20" << reinterpret_cast<void*>(map()) << "\xa"
     << "\x20\x2d\x20\x63\x6f\x6e\x74\x65\x78\x74\x20\x3d\x20";
  context()->Print(os);
  os << "\x20\x2d\x20\x73\x63\x6f\x70\x65\x5f\x69\x6e\x66\x6f\x20\x3d\x20" << Brief(scope_info())
     << ElementsKindToString(this->map()->elements_kind()) << "\x20\x7b\xa";
  PrintProperties(os);
  PrintElements(os);
  os << "\x20\x7d\xa";
}


static const char* TypeToString(InstanceType type) {
  switch (type) {
#define TYPE_TO_STRING(TYPE) case TYPE: return #TYPE;
  INSTANCE_TYPE_LIST(TYPE_TO_STRING)
#undef TYPE_TO_STRING
  }
  UNREACHABLE();
  return "\x55\x4e\x4b\x4e\x4f\x57\x4e";  // Keep the compiler happy.
}


void Symbol::SymbolPrint(OStream& os) {  // NOLINT
  HeapObject::PrintHeader(os, "\x53\x79\x6d\x62\x6f\x6c");
  os << "\x20\x2d\x20\x68\x61\x73\x68\x3a\x20" << Hash();
  os << "\xa\x20\x2d\x20\x6e\x61\x6d\x65\x3a\x20" << Brief(name());
  os << "\x20\x2d\x20\x70\x72\x69\x76\x61\x74\x65\x3a\x20" << is_private();
  os << "\xa";
}


void Map::MapPrint(OStream& os) {  // NOLINT
  HeapObject::PrintHeader(os, "\x4d\x61\x70");
  os << "\x20\x2d\x20\x74\x79\x70\x65\x3a\x20" << TypeToString(instance_type()) << "\xa";
  os << "\x20\x2d\x20\x69\x6e\x73\x74\x61\x6e\x63\x65\x20\x73\x69\x7a\x65\x3a\x20" << instance_size() << "\xa";
  os << "\x20\x2d\x20\x69\x6e\x6f\x62\x6a\x65\x63\x74\x20\x70\x72\x6f\x70\x65\x72\x74\x69\x65\x73\x3a\x20" << inobject_properties() << "\xa";
  os << "\x20\x2d\x20\x65\x6c\x65\x6d\x65\x6e\x74\x73\x20\x6b\x69\x6e\x64\x3a\x20" << ElementsKindToString(elements_kind());
  os << "\xa\x20\x2d\x20\x70\x72\x65\x2d\x61\x6c\x6c\x6f\x63\x61\x74\x65\x64\x20\x70\x72\x6f\x70\x65\x72\x74\x79\x20\x66\x69\x65\x6c\x64\x73\x3a\x20"
     << pre_allocated_property_fields() << "\xa";
  os << "\x20\x2d\x20\x75\x6e\x75\x73\x65\x64\x20\x70\x72\x6f\x70\x65\x72\x74\x79\x20\x66\x69\x65\x6c\x64\x73\x3a\x20" << unused_property_fields() << "\xa";
  if (is_hidden_prototype()) os << "\x20\x2d\x20\x68\x69\x64\x64\x65\x6e\x5f\x70\x72\x6f\x74\x6f\x74\x79\x70\x65\xa";
  if (has_named_interceptor()) os << "\x20\x2d\x20\x6e\x61\x6d\x65\x64\x5f\x69\x6e\x74\x65\x72\x63\x65\x70\x74\x6f\x72\xa";
  if (has_indexed_interceptor()) os << "\x20\x2d\x20\x69\x6e\x64\x65\x78\x65\x64\x5f\x69\x6e\x74\x65\x72\x63\x65\x70\x74\x6f\x72\xa";
  if (is_undetectable()) os << "\x20\x2d\x20\x75\x6e\x64\x65\x74\x65\x63\x74\x61\x62\x6c\x65\xa";
  if (has_instance_call_handler()) os << "\x20\x2d\x20\x69\x6e\x73\x74\x61\x6e\x63\x65\x5f\x63\x61\x6c\x6c\x5f\x68\x61\x6e\x64\x6c\x65\x72\xa";
  if (is_access_check_needed()) os << "\x20\x2d\x20\x61\x63\x63\x65\x73\x73\x5f\x63\x68\x65\x63\x6b\x5f\x6e\x65\x65\x64\x65\x64\xa";
  if (is_frozen()) {
    os << "\x20\x2d\x20\x66\x72\x6f\x7a\x65\x6e\xa";
  } else if (!is_extensible()) {
    os << "\x20\x2d\x20\x73\x65\x61\x6c\x65\x64\xa";
  }
  os << "\x20\x2d\x20\x62\x61\x63\x6b\x20\x70\x6f\x69\x6e\x74\x65\x72\x3a\x20" << Brief(GetBackPointer());
  os << "\xa\x20\x2d\x20\x69\x6e\x73\x74\x61\x6e\x63\x65\x20\x64\x65\x73\x63\x72\x69\x70\x74\x6f\x72\x73\x20" << (owns_descriptors() ? "\x28\x6f\x77\x6e\x29\x20" : "")
     << "\x23" << NumberOfOwnDescriptors() << "\x3a\x20"
     << Brief(instance_descriptors());
  if (HasTransitionArray()) {
    os << "\xa\x20\x2d\x20\x74\x72\x61\x6e\x73\x69\x74\x69\x6f\x6e\x73\x3a\x20" << Brief(transitions());
  }
  os << "\xa\x20\x2d\x20\x70\x72\x6f\x74\x6f\x74\x79\x70\x65\x3a\x20" << Brief(prototype());
  os << "\xa\x20\x2d\x20\x63\x6f\x6e\x73\x74\x72\x75\x63\x74\x6f\x72\x3a\x20" << Brief(constructor());
  os << "\xa\x20\x2d\x20\x63\x6f\x64\x65\x20\x63\x61\x63\x68\x65\x3a\x20" << Brief(code_cache());
  os << "\xa\x20\x2d\x20\x64\x65\x70\x65\x6e\x64\x65\x6e\x74\x20\x63\x6f\x64\x65\x3a\x20" << Brief(dependent_code());
  os << "\xa";
}


void CodeCache::CodeCachePrint(OStream& os) {  // NOLINT
  HeapObject::PrintHeader(os, "\x43\x6f\x64\x65\x43\x61\x63\x68\x65");
  os << "\xa\x20\x2d\x20\x64\x65\x66\x61\x75\x6c\x74\x5f\x63\x61\x63\x68\x65\x3a\x20" << Brief(default_cache());
  os << "\xa\x20\x2d\x20\x6e\x6f\x72\x6d\x61\x6c\x5f\x74\x79\x70\x65\x5f\x63\x61\x63\x68\x65\x3a\x20" << Brief(normal_type_cache());
}


void PolymorphicCodeCache::PolymorphicCodeCachePrint(OStream& os) {  // NOLINT
  HeapObject::PrintHeader(os, "\x50\x6f\x6c\x79\x6d\x6f\x72\x70\x68\x69\x63\x43\x6f\x64\x65\x43\x61\x63\x68\x65");
  os << "\xa\x20\x2d\x20\x63\x61\x63\x68\x65\x3a\x20" << Brief(cache());
}


void TypeFeedbackInfo::TypeFeedbackInfoPrint(OStream& os) {  // NOLINT
  HeapObject::PrintHeader(os, "\x54\x79\x70\x65\x46\x65\x65\x64\x62\x61\x63\x6b\x49\x6e\x66\x6f");
  os << "\x20\x2d\x20\x69\x63\x5f\x74\x6f\x74\x61\x6c\x5f\x63\x6f\x75\x6e\x74\x3a\x20" << ic_total_count()
     << "\x2c\x20\x69\x63\x5f\x77\x69\x74\x68\x5f\x74\x79\x70\x65\x5f\x69\x6e\x66\x6f\x5f\x63\x6f\x75\x6e\x74\x3a\x20" << ic_with_type_info_count()
     << "\x2c\x20\x69\x63\x5f\x67\x65\x6e\x65\x72\x69\x63\x5f\x63\x6f\x75\x6e\x74\x3a\x20" << ic_generic_count() << "\xa";
}


void AliasedArgumentsEntry::AliasedArgumentsEntryPrint(OStream& os) {  // NOLINT
  HeapObject::PrintHeader(os, "\x41\x6c\x69\x61\x73\x65\x64\x41\x72\x67\x75\x6d\x65\x6e\x74\x73\x45\x6e\x74\x72\x79");
  os << "\xa\x20\x2d\x20\x61\x6c\x69\x61\x73\x65\x64\x5f\x63\x6f\x6e\x74\x65\x78\x74\x5f\x73\x6c\x6f\x74\x3a\x20" << aliased_context_slot();
}


void FixedArray::FixedArrayPrint(OStream& os) {  // NOLINT
  HeapObject::PrintHeader(os, "\x46\x69\x78\x65\x64\x41\x72\x72\x61\x79");
  os << "\x20\x2d\x20\x6c\x65\x6e\x67\x74\x68\x3a\x20" << length();
  for (int i = 0; i < length(); i++) {
    os << "\xa\x20\x20\x5b" << i << "\x5d\x3a\x20" << Brief(get(i));
  }
  os << "\xa";
}


void FixedDoubleArray::FixedDoubleArrayPrint(OStream& os) {  // NOLINT
  HeapObject::PrintHeader(os, "\x46\x69\x78\x65\x64\x44\x6f\x75\x62\x6c\x65\x41\x72\x72\x61\x79");
  os << "\x20\x2d\x20\x6c\x65\x6e\x67\x74\x68\x3a\x20" << length();
  for (int i = 0; i < length(); i++) {
    os << "\xa\x20\x20\x5b" << i << "\x5d\x3a\x20";
    if (is_the_hole(i)) {
      os << "\x3c\x74\x68\x65\x20\x68\x6f\x6c\x65\x3e";
    } else {
      os << get_scalar(i);
    }
  }
  os << "\xa";
}


void ConstantPoolArray::ConstantPoolArrayPrint(OStream& os) {  // NOLINT
  HeapObject::PrintHeader(os, "\x43\x6f\x6e\x73\x74\x61\x6e\x74\x50\x6f\x6f\x6c\x41\x72\x72\x61\x79");
  os << "\x20\x2d\x20\x6c\x65\x6e\x67\x74\x68\x3a\x20" << length();
  for (int i = 0; i <= last_index(INT32, SMALL_SECTION); i++) {
    if (i <= last_index(INT64, SMALL_SECTION)) {
#if V8_TARGET_ARCH_64_BIT
      os << "\xa\x20\x20\x5b" << i << "\x5d\x3a\x20\x69\x6e\x74\x36\x34\x3a\x20" << get_int64_entry(i);
#else
      os << "\xa\x20\x20\x5b" << i << "\x5d\x3a\x20\x64\x6f\x75\x62\x6c\x65\x3a\x20" << get_int64_entry_as_double(i);
#endif
    } else if (i <= last_index(CODE_PTR, SMALL_SECTION)) {
      os << "\xa\x20\x20\x5b" << i << "\x5d\x3a\x20\x63\x6f\x64\x65\x20\x74\x61\x72\x67\x65\x74\x20\x70\x6f\x69\x6e\x74\x65\x72\x3a\x20"
         << reinterpret_cast<void*>(get_code_ptr_entry(i));
    } else if (i <= last_index(HEAP_PTR, SMALL_SECTION)) {
      os << "\xa\x20\x20\x5b" << i << "\x5d\x3a\x20\x68\x65\x61\x70\x20\x70\x6f\x69\x6e\x74\x65\x72\x3a\x20"
         << reinterpret_cast<void*>(get_heap_ptr_entry(i));
    } else if (i <= last_index(INT32, SMALL_SECTION)) {
      os << "\xa\x20\x20\x5b" << i << "\x5d\x3a\x20\x69\x6e\x74\x33\x32\x3a\x20" << get_int32_entry(i);
    }
  }
  if (is_extended_layout()) {
    os << "\xa\x20\x20\x45\x78\x74\x65\x6e\x64\x65\x64\x20\x73\x65\x63\x74\x69\x6f\x6e\x3a";
    for (int i = first_extended_section_index();
         i <= last_index(INT32, EXTENDED_SECTION); i++) {
      if (i <= last_index(INT64, EXTENDED_SECTION)) {
#if V8_TARGET_ARCH_64_BIT
        os << "\xa\x20\x20\x5b" << i << "\x5d\x3a\x20\x69\x6e\x74\x36\x34\x3a\x20" << get_int64_entry(i);
#else
        os << "\xa\x20\x20\x5b" << i << "\x5d\x3a\x20\x64\x6f\x75\x62\x6c\x65\x3a\x20" << get_int64_entry_as_double(i);
#endif
      } else if (i <= last_index(CODE_PTR, EXTENDED_SECTION)) {
        os << "\xa\x20\x20\x5b" << i << "\x5d\x3a\x20\x63\x6f\x64\x65\x20\x74\x61\x72\x67\x65\x74\x20\x70\x6f\x69\x6e\x74\x65\x72\x3a\x20"
           << reinterpret_cast<void*>(get_code_ptr_entry(i));
      } else if (i <= last_index(HEAP_PTR, EXTENDED_SECTION)) {
        os << "\xa\x20\x20\x5b" << i << "\x5d\x3a\x20\x68\x65\x61\x70\x20\x70\x6f\x69\x6e\x74\x65\x72\x3a\x20"
           << reinterpret_cast<void*>(get_heap_ptr_entry(i));
      } else if (i <= last_index(INT32, EXTENDED_SECTION)) {
        os << "\xa\x20\x20\x5b" << i << "\x5d\x3a\x20\x69\x6e\x74\x33\x32\x3a\x20" << get_int32_entry(i);
      }
    }
  }
  os << "\xa";
}


void JSValue::JSValuePrint(OStream& os) {  // NOLINT
  HeapObject::PrintHeader(os, "\x56\x61\x6c\x75\x65\x4f\x62\x6a\x65\x63\x74");
  value()->Print(os);
}


void JSMessageObject::JSMessageObjectPrint(OStream& os) {  // NOLINT
  HeapObject::PrintHeader(os, "\x4a\x53\x4d\x65\x73\x73\x61\x67\x65\x4f\x62\x6a\x65\x63\x74");
  os << "\x20\x2d\x20\x74\x79\x70\x65\x3a\x20" << Brief(type());
  os << "\xa\x20\x2d\x20\x61\x72\x67\x75\x6d\x65\x6e\x74\x73\x3a\x20" << Brief(arguments());
  os << "\xa\x20\x2d\x20\x73\x74\x61\x72\x74\x5f\x70\x6f\x73\x69\x74\x69\x6f\x6e\x3a\x20" << start_position();
  os << "\xa\x20\x2d\x20\x65\x6e\x64\x5f\x70\x6f\x73\x69\x74\x69\x6f\x6e\x3a\x20" << end_position();
  os << "\xa\x20\x2d\x20\x73\x63\x72\x69\x70\x74\x3a\x20" << Brief(script());
  os << "\xa\x20\x2d\x20\x73\x74\x61\x63\x6b\x5f\x66\x72\x61\x6d\x65\x73\x3a\x20" << Brief(stack_frames());
  os << "\xa";
}


void String::StringPrint(OStream& os) {  // NOLINT
  if (StringShape(this).IsInternalized()) {
    os << "\x23";
  } else if (StringShape(this).IsCons()) {
    os << "\x63\x22";
  } else {
    os << "\x22";
  }

  const char truncated_epilogue[] = "\x2e\x2e\x2e\x3c\x74\x72\x75\x6e\x63\x61\x74\x65\x64\x3e";
  int len = length();
  if (!FLAG_use_verbose_printer) {
    if (len > 100) {
      len = 100 - sizeof(truncated_epilogue);
    }
  }
  for (int i = 0; i < len; i++) {
#ifndef V8_OS_ZOS
      os << AsUC16(Get(i));
#else
      os << static_cast<char>(Get(i));
#endif
  }
  if (len != length()) {
    os << truncated_epilogue;
  }

  if (!StringShape(this).IsInternalized()) os << "\x22";
}


void Name::NamePrint(OStream& os) {  // NOLINT
  if (IsString())
    String::cast(this)->StringPrint(os);
  else
    os << Brief(this);
}


// This method is only meant to be called from gdb for debugging purposes.
// Since the string can also be in two-byte encoding, non-ASCII characters
// will be ignored in the output.
char* String::ToAsciiArray() {
  // Static so that subsequent calls frees previously allocated space.
  // This also means that previous results will be overwritten.
  static char* buffer = NULL;
  if (buffer != NULL) free(buffer);
  buffer = new char[length()+1];
  WriteToFlat(this, reinterpret_cast<uint8_t*>(buffer), 0, length());
  buffer[length()] = 0;
  return buffer;
}


static const char* const weekdays[] = {
  "\x3f\x3f\x3f", "\x53\x75\x6e", "\x4d\x6f\x6e", "\x54\x75\x65", "\x57\x65\x64", "\x54\x68\x75", "\x46\x72\x69", "\x53\x61\x74"
};


void JSDate::JSDatePrint(OStream& os) {  // NOLINT
  HeapObject::PrintHeader(os, "\x4a\x53\x44\x61\x74\x65");
  os << "\x20\x2d\x20\x6d\x61\x70\x20\x3d\x20" << reinterpret_cast<void*>(map()) << "\xa";
  os << "\x20\x2d\x20\x76\x61\x6c\x75\x65\x20\x3d\x20";
  value()->Print(os);
  if (!year()->IsSmi()) {
    os << "\x20\x2d\x20\x74\x69\x6d\x65\x20\x3d\x20\x4e\x61\x4e\xa";
  } else {
    // TODO(svenpanne) Add some basic formatting to our streams.
    Vector<char> buf = Vector<char>::New(100);
    SNPrintF(
        buf, "\x20\x2d\x20\x74\x69\x6d\x65\x20\x3d\x20\x6c\xa2\x20\x6c\xf0\xf4\x84\x2f\x6c\xf0\xf2\x84\x2f\x6c\xf0\xf2\x84\x20\x6c\xf0\xf2\x84\x3a\x6c\xf0\xf2\x84\x3a\x6c\xf0\xf2\x84\xa",
        weekdays[weekday()->IsSmi() ? Smi::cast(weekday())->value() + 1 : 0],
        year()->IsSmi() ? Smi::cast(year())->value() : -1,
        month()->IsSmi() ? Smi::cast(month())->value() : -1,
        day()->IsSmi() ? Smi::cast(day())->value() : -1,
        hour()->IsSmi() ? Smi::cast(hour())->value() : -1,
        min()->IsSmi() ? Smi::cast(min())->value() : -1,
        sec()->IsSmi() ? Smi::cast(sec())->value() : -1);
    os << buf.start();
  }
}


void JSProxy::JSProxyPrint(OStream& os) {  // NOLINT
  HeapObject::PrintHeader(os, "\x4a\x53\x50\x72\x6f\x78\x79");
  os << "\x20\x2d\x20\x6d\x61\x70\x20\x3d\x20" << reinterpret_cast<void*>(map()) << "\xa";
  os << "\x20\x2d\x20\x68\x61\x6e\x64\x6c\x65\x72\x20\x3d\x20";
  handler()->Print(os);
  os << "\xa\x20\x2d\x20\x68\x61\x73\x68\x20\x3d\x20";
  hash()->Print(os);
  os << "\xa";
}


void JSFunctionProxy::JSFunctionProxyPrint(OStream& os) {  // NOLINT
  HeapObject::PrintHeader(os, "\x4a\x53\x46\x75\x6e\x63\x74\x69\x6f\x6e\x50\x72\x6f\x78\x79");
  os << "\x20\x2d\x20\x6d\x61\x70\x20\x3d\x20" << reinterpret_cast<void*>(map()) << "\xa";
  os << "\x20\x2d\x20\x68\x61\x6e\x64\x6c\x65\x72\x20\x3d\x20";
  handler()->Print(os);
  os << "\xa\x20\x2d\x20\x63\x61\x6c\x6c\x5f\x74\x72\x61\x70\x20\x3d\x20";
  call_trap()->Print(os);
  os << "\xa\x20\x2d\x20\x63\x6f\x6e\x73\x74\x72\x75\x63\x74\x5f\x74\x72\x61\x70\x20\x3d\x20";
  construct_trap()->Print(os);
  os << "\xa";
}


void JSSet::JSSetPrint(OStream& os) {  // NOLINT
  HeapObject::PrintHeader(os, "\x4a\x53\x53\x65\x74");
  os << "\x20\x2d\x20\x6d\x61\x70\x20\x3d\x20" << reinterpret_cast<void*>(map()) << "\xa";
  os << "\x20\x2d\x20\x74\x61\x62\x6c\x65\x20\x3d\x20" << Brief(table());
  os << "\xa";
}


void JSMap::JSMapPrint(OStream& os) {  // NOLINT
  HeapObject::PrintHeader(os, "\x4a\x53\x4d\x61\x70");
  os << "\x20\x2d\x20\x6d\x61\x70\x20\x3d\x20" << reinterpret_cast<void*>(map()) << "\xa";
  os << "\x20\x2d\x20\x74\x61\x62\x6c\x65\x20\x3d\x20" << Brief(table());
  os << "\xa";
}


template <class Derived, class TableType>
void OrderedHashTableIterator<
    Derived, TableType>::OrderedHashTableIteratorPrint(OStream& os) {  // NOLINT
  os << "\x20\x2d\x20\x6d\x61\x70\x20\x3d\x20" << reinterpret_cast<void*>(map()) << "\xa";
  os << "\x20\x2d\x20\x74\x61\x62\x6c\x65\x20\x3d\x20" << Brief(table());
  os << "\xa\x20\x2d\x20\x69\x6e\x64\x65\x78\x20\x3d\x20" << Brief(index());
  os << "\xa\x20\x2d\x20\x6b\x69\x6e\x64\x20\x3d\x20" << Brief(kind());
  os << "\xa";
}


template void OrderedHashTableIterator<
    JSSetIterator,
    OrderedHashSet>::OrderedHashTableIteratorPrint(OStream& os);  // NOLINT


template void OrderedHashTableIterator<
    JSMapIterator,
    OrderedHashMap>::OrderedHashTableIteratorPrint(OStream& os);  // NOLINT


void JSSetIterator::JSSetIteratorPrint(OStream& os) {  // NOLINT
  HeapObject::PrintHeader(os, "\x4a\x53\x53\x65\x74\x49\x74\x65\x72\x61\x74\x6f\x72");
  OrderedHashTableIteratorPrint(os);
}


void JSMapIterator::JSMapIteratorPrint(OStream& os) {  // NOLINT
  HeapObject::PrintHeader(os, "\x4a\x53\x4d\x61\x70\x49\x74\x65\x72\x61\x74\x6f\x72");
  OrderedHashTableIteratorPrint(os);
}


void JSWeakMap::JSWeakMapPrint(OStream& os) {  // NOLINT
  HeapObject::PrintHeader(os, "\x4a\x53\x57\x65\x61\x6b\x4d\x61\x70");
  os << "\x20\x2d\x20\x6d\x61\x70\x20\x3d\x20" << reinterpret_cast<void*>(map()) << "\xa";
  os << "\x20\x2d\x20\x74\x61\x62\x6c\x65\x20\x3d\x20" << Brief(table());
  os << "\xa";
}


void JSWeakSet::JSWeakSetPrint(OStream& os) {  // NOLINT
  HeapObject::PrintHeader(os, "\x4a\x53\x57\x65\x61\x6b\x53\x65\x74");
  os << "\x20\x2d\x20\x6d\x61\x70\x20\x3d\x20" << reinterpret_cast<void*>(map()) << "\xa";
  os << "\x20\x2d\x20\x74\x61\x62\x6c\x65\x20\x3d\x20" << Brief(table());
  os << "\xa";
}


void JSArrayBuffer::JSArrayBufferPrint(OStream& os) {  // NOLINT
  HeapObject::PrintHeader(os, "\x4a\x53\x41\x72\x72\x61\x79\x42\x75\x66\x66\x65\x72");
  os << "\x20\x2d\x20\x6d\x61\x70\x20\x3d\x20" << reinterpret_cast<void*>(map()) << "\xa";
  os << "\x20\x2d\x20\x62\x61\x63\x6b\x69\x6e\x67\x5f\x73\x74\x6f\x72\x65\x20\x3d\x20" << backing_store() << "\xa";
  os << "\x20\x2d\x20\x62\x79\x74\x65\x5f\x6c\x65\x6e\x67\x74\x68\x20\x3d\x20" << Brief(byte_length());
  os << "\xa";
}


void JSTypedArray::JSTypedArrayPrint(OStream& os) {  // NOLINT
  HeapObject::PrintHeader(os, "\x4a\x53\x54\x79\x70\x65\x64\x41\x72\x72\x61\x79");
  os << "\x20\x2d\x20\x6d\x61\x70\x20\x3d\x20" << reinterpret_cast<void*>(map()) << "\xa";
  os << "\x20\x2d\x20\x62\x75\x66\x66\x65\x72\x20\x3d" << Brief(buffer());
  os << "\xa\x20\x2d\x20\x62\x79\x74\x65\x5f\x6f\x66\x66\x73\x65\x74\x20\x3d\x20" << Brief(byte_offset());
  os << "\xa\x20\x2d\x20\x62\x79\x74\x65\x5f\x6c\x65\x6e\x67\x74\x68\x20\x3d\x20" << Brief(byte_length());
  os << "\xa\x20\x2d\x20\x6c\x65\x6e\x67\x74\x68\x20\x3d\x20" << Brief(length());
  os << "\xa";
  PrintElements(os);
}


void JSDataView::JSDataViewPrint(OStream& os) {  // NOLINT
  HeapObject::PrintHeader(os, "\x4a\x53\x44\x61\x74\x61\x56\x69\x65\x77");
  os << "\x20\x2d\x20\x6d\x61\x70\x20\x3d\x20" << reinterpret_cast<void*>(map()) << "\xa";
  os << "\x20\x2d\x20\x62\x75\x66\x66\x65\x72\x20\x3d" << Brief(buffer());
  os << "\xa\x20\x2d\x20\x62\x79\x74\x65\x5f\x6f\x66\x66\x73\x65\x74\x20\x3d\x20" << Brief(byte_offset());
  os << "\xa\x20\x2d\x20\x62\x79\x74\x65\x5f\x6c\x65\x6e\x67\x74\x68\x20\x3d\x20" << Brief(byte_length());
  os << "\xa";
}


void JSFunction::JSFunctionPrint(OStream& os) {  // NOLINT
  HeapObject::PrintHeader(os, "\x46\x75\x6e\x63\x74\x69\x6f\x6e");
  os << "\x20\x2d\x20\x6d\x61\x70\x20\x3d\x20" << reinterpret_cast<void*>(map()) << "\xa";
  os << "\x20\x2d\x20\x69\x6e\x69\x74\x69\x61\x6c\x5f\x6d\x61\x70\x20\x3d\x20";
  if (has_initial_map()) os << Brief(initial_map());
  os << "\xa\x20\x2d\x20\x73\x68\x61\x72\x65\x64\x5f\x69\x6e\x66\x6f\x20\x3d\x20" << Brief(shared());
  os << "\xa\x20\x20\x20\x2d\x20\x6e\x61\x6d\x65\x20\x3d\x20" << Brief(shared()->name());
  os << "\xa\x20\x2d\x20\x63\x6f\x6e\x74\x65\x78\x74\x20\x3d\x20" << Brief(context());
  if (shared()->bound()) {
    os << "\xa\x20\x2d\x20\x62\x69\x6e\x64\x69\x6e\x67\x73\x20\x3d\x20" << Brief(function_bindings());
  } else {
    os << "\xa\x20\x2d\x20\x6c\x69\x74\x65\x72\x61\x6c\x73\x20\x3d\x20" << Brief(literals());
  }
  os << "\xa\x20\x2d\x20\x63\x6f\x64\x65\x20\x3d\x20" << Brief(code());
  os << "\xa";
  PrintProperties(os);
  PrintElements(os);
  os << "\xa";
}


void SharedFunctionInfo::SharedFunctionInfoPrint(OStream& os) {  // NOLINT
  HeapObject::PrintHeader(os, "\x53\x68\x61\x72\x65\x64\x46\x75\x6e\x63\x74\x69\x6f\x6e\x49\x6e\x66\x6f");
  os << "\x20\x2d\x20\x6e\x61\x6d\x65\x3a\x20" << Brief(name());
  os << "\xa\x20\x2d\x20\x65\x78\x70\x65\x63\x74\x65\x64\x5f\x6e\x6f\x66\x5f\x70\x72\x6f\x70\x65\x72\x74\x69\x65\x73\x3a\x20" << expected_nof_properties();
  os << "\xa\x20\x2d\x20\x61\x73\x74\x5f\x6e\x6f\x64\x65\x5f\x63\x6f\x75\x6e\x74\x3a\x20" << ast_node_count();
  os << "\xa\x20\x2d\x20\x69\x6e\x73\x74\x61\x6e\x63\x65\x20\x63\x6c\x61\x73\x73\x20\x6e\x61\x6d\x65\x20\x3d\x20";
  instance_class_name()->Print(os);
  os << "\xa\x20\x2d\x20\x63\x6f\x64\x65\x20\x3d\x20" << Brief(code());
  if (HasSourceCode()) {
    os << "\xa\x20\x2d\x20\x73\x6f\x75\x72\x63\x65\x20\x63\x6f\x64\x65\x20\x3d\x20";
    String* source = String::cast(Script::cast(script())->source());
    int start = start_position();
    int length = end_position() - start;
    SmartArrayPointer<char> source_string =
        source->ToCString(DISALLOW_NULLS,
                          FAST_STRING_TRAVERSAL,
                          start, length, NULL);
    os << source_string.get();
  }
  // Script files are often large, hard to read.
  // os << "\n - script =";
  // script()->Print(os);
  os << "\xa\x20\x2d\x20\x66\x75\x6e\x63\x74\x69\x6f\x6e\x20\x74\x6f\x6b\x65\x6e\x20\x70\x6f\x73\x69\x74\x69\x6f\x6e\x20\x3d\x20" << function_token_position();
  os << "\xa\x20\x2d\x20\x73\x74\x61\x72\x74\x20\x70\x6f\x73\x69\x74\x69\x6f\x6e\x20\x3d\x20" << start_position();
  os << "\xa\x20\x2d\x20\x65\x6e\x64\x20\x70\x6f\x73\x69\x74\x69\x6f\x6e\x20\x3d\x20" << end_position();
  os << "\xa\x20\x2d\x20\x69\x73\x20\x65\x78\x70\x72\x65\x73\x73\x69\x6f\x6e\x20\x3d\x20" << is_expression();
  os << "\xa\x20\x2d\x20\x64\x65\x62\x75\x67\x20\x69\x6e\x66\x6f\x20\x3d\x20" << Brief(debug_info());
  os << "\xa\x20\x2d\x20\x6c\x65\x6e\x67\x74\x68\x20\x3d\x20" << length();
  os << "\xa\x20\x2d\x20\x6f\x70\x74\x69\x6d\x69\x7a\x65\x64\x5f\x63\x6f\x64\x65\x5f\x6d\x61\x70\x20\x3d\x20" << Brief(optimized_code_map());
  os << "\xa\x20\x2d\x20\x66\x65\x65\x64\x62\x61\x63\x6b\x5f\x76\x65\x63\x74\x6f\x72\x20\x3d\x20";
  feedback_vector()->FixedArrayPrint(os);
  os << "\xa";
}


void JSGlobalProxy::JSGlobalProxyPrint(OStream& os) {  // NOLINT
  os << "\x67\x6c\x6f\x62\x61\x6c\x5f\x70\x72\x6f\x78\x79\x20";
  JSObjectPrint(os);
  os << "\x6e\x61\x74\x69\x76\x65\x20\x63\x6f\x6e\x74\x65\x78\x74\x20\x3a\x20" << Brief(native_context());
  os << "\xa";
}


void JSGlobalObject::JSGlobalObjectPrint(OStream& os) {  // NOLINT
  os << "\x67\x6c\x6f\x62\x61\x6c\x20";
  JSObjectPrint(os);
  os << "\x6e\x61\x74\x69\x76\x65\x20\x63\x6f\x6e\x74\x65\x78\x74\x20\x3a\x20" << Brief(native_context());
  os << "\xa";
}


void JSBuiltinsObject::JSBuiltinsObjectPrint(OStream& os) {  // NOLINT
  os << "\x62\x75\x69\x6c\x74\x69\x6e\x73\x20";
  JSObjectPrint(os);
}


void Cell::CellPrint(OStream& os) {  // NOLINT
  HeapObject::PrintHeader(os, "\x43\x65\x6c\x6c");
}


void PropertyCell::PropertyCellPrint(OStream& os) {  // NOLINT
  HeapObject::PrintHeader(os, "\x50\x72\x6f\x70\x65\x72\x74\x79\x43\x65\x6c\x6c");
}


void Code::CodePrint(OStream& os) {  // NOLINT
  HeapObject::PrintHeader(os, "\x43\x6f\x64\x65");
#ifdef ENABLE_DISASSEMBLER
  if (FLAG_use_verbose_printer) {
    Disassemble(NULL, os);
  }
#endif
}


void Foreign::ForeignPrint(OStream& os) {  // NOLINT
  os << "\x66\x6f\x72\x65\x69\x67\x6e\x20\x61\x64\x64\x72\x65\x73\x73\x20\x3a\x20" << foreign_address();
}


void ExecutableAccessorInfo::ExecutableAccessorInfoPrint(
    OStream& os) {  // NOLINT
  HeapObject::PrintHeader(os, "\x45\x78\x65\x63\x75\x74\x61\x62\x6c\x65\x41\x63\x63\x65\x73\x73\x6f\x72\x49\x6e\x66\x6f");
  os << "\xa\x20\x2d\x20\x6e\x61\x6d\x65\x3a\x20" << Brief(name());
  os << "\xa\x20\x2d\x20\x66\x6c\x61\x67\x3a\x20" << Brief(flag());
  os << "\xa\x20\x2d\x20\x67\x65\x74\x74\x65\x72\x3a\x20" << Brief(getter());
  os << "\xa\x20\x2d\x20\x73\x65\x74\x74\x65\x72\x3a\x20" << Brief(setter());
  os << "\xa\x20\x2d\x20\x64\x61\x74\x61\x3a\x20" << Brief(data());
  os << "\xa";
}


void DeclaredAccessorInfo::DeclaredAccessorInfoPrint(OStream& os) {  // NOLINT
  HeapObject::PrintHeader(os, "\x44\x65\x63\x6c\x61\x72\x65\x64\x41\x63\x63\x65\x73\x73\x6f\x72\x49\x6e\x66\x6f");
  os << "\xa\x20\x2d\x20\x6e\x61\x6d\x65\x3a\x20" << Brief(name());
  os << "\xa\x20\x2d\x20\x66\x6c\x61\x67\x3a\x20" << Brief(flag());
  os << "\xa\x20\x2d\x20\x64\x65\x73\x63\x72\x69\x70\x74\x6f\x72\x3a\x20" << Brief(descriptor());
  os << "\xa";
}


void DeclaredAccessorDescriptor::DeclaredAccessorDescriptorPrint(
    OStream& os) {  // NOLINT
  HeapObject::PrintHeader(os, "\x44\x65\x63\x6c\x61\x72\x65\x64\x41\x63\x63\x65\x73\x73\x6f\x72\x44\x65\x73\x63\x72\x69\x70\x74\x6f\x72");
  os << "\xa\x20\x2d\x20\x69\x6e\x74\x65\x72\x6e\x61\x6c\x20\x66\x69\x65\x6c\x64\x3a\x20" << Brief(serialized_data());
  os << "\xa";
}


void Box::BoxPrint(OStream& os) {  // NOLINT
  HeapObject::PrintHeader(os, "\x42\x6f\x78");
  os << "\xa\x20\x2d\x20\x76\x61\x6c\x75\x65\x3a\x20" << Brief(value());
  os << "\xa";
}


void AccessorPair::AccessorPairPrint(OStream& os) {  // NOLINT
  HeapObject::PrintHeader(os, "\x41\x63\x63\x65\x73\x73\x6f\x72\x50\x61\x69\x72");
  os << "\xa\x20\x2d\x20\x67\x65\x74\x74\x65\x72\x3a\x20" << Brief(getter());
  os << "\xa\x20\x2d\x20\x73\x65\x74\x74\x65\x72\x3a\x20" << Brief(setter());
  os << "\xa";
}


void AccessCheckInfo::AccessCheckInfoPrint(OStream& os) {  // NOLINT
  HeapObject::PrintHeader(os, "\x41\x63\x63\x65\x73\x73\x43\x68\x65\x63\x6b\x49\x6e\x66\x6f");
  os << "\xa\x20\x2d\x20\x6e\x61\x6d\x65\x64\x5f\x63\x61\x6c\x6c\x62\x61\x63\x6b\x3a\x20" << Brief(named_callback());
  os << "\xa\x20\x2d\x20\x69\x6e\x64\x65\x78\x65\x64\x5f\x63\x61\x6c\x6c\x62\x61\x63\x6b\x3a\x20" << Brief(indexed_callback());
  os << "\xa\x20\x2d\x20\x64\x61\x74\x61\x3a\x20" << Brief(data());
  os << "\xa";
}


void InterceptorInfo::InterceptorInfoPrint(OStream& os) {  // NOLINT
  HeapObject::PrintHeader(os, "\x49\x6e\x74\x65\x72\x63\x65\x70\x74\x6f\x72\x49\x6e\x66\x6f");
  os << "\xa\x20\x2d\x20\x67\x65\x74\x74\x65\x72\x3a\x20" << Brief(getter());
  os << "\xa\x20\x2d\x20\x73\x65\x74\x74\x65\x72\x3a\x20" << Brief(setter());
  os << "\xa\x20\x2d\x20\x71\x75\x65\x72\x79\x3a\x20" << Brief(query());
  os << "\xa\x20\x2d\x20\x64\x65\x6c\x65\x74\x65\x72\x3a\x20" << Brief(deleter());
  os << "\xa\x20\x2d\x20\x65\x6e\x75\x6d\x65\x72\x61\x74\x6f\x72\x3a\x20" << Brief(enumerator());
  os << "\xa\x20\x2d\x20\x64\x61\x74\x61\x3a\x20" << Brief(data());
  os << "\xa";
}


void CallHandlerInfo::CallHandlerInfoPrint(OStream& os) {  // NOLINT
  HeapObject::PrintHeader(os, "\x43\x61\x6c\x6c\x48\x61\x6e\x64\x6c\x65\x72\x49\x6e\x66\x6f");
  os << "\xa\x20\x2d\x20\x63\x61\x6c\x6c\x62\x61\x63\x6b\x3a\x20" << Brief(callback());
  os << "\xa\x20\x2d\x20\x64\x61\x74\x61\x3a\x20" << Brief(data());
  os << "\xa";
}


void FunctionTemplateInfo::FunctionTemplateInfoPrint(OStream& os) {  // NOLINT
  HeapObject::PrintHeader(os, "\x46\x75\x6e\x63\x74\x69\x6f\x6e\x54\x65\x6d\x70\x6c\x61\x74\x65\x49\x6e\x66\x6f");
  os << "\xa\x20\x2d\x20\x63\x6c\x61\x73\x73\x20\x6e\x61\x6d\x65\x3a\x20" << Brief(class_name());
  os << "\xa\x20\x2d\x20\x74\x61\x67\x3a\x20" << Brief(tag());
  os << "\xa\x20\x2d\x20\x70\x72\x6f\x70\x65\x72\x74\x79\x5f\x6c\x69\x73\x74\x3a\x20" << Brief(property_list());
  os << "\xa\x20\x2d\x20\x73\x65\x72\x69\x61\x6c\x5f\x6e\x75\x6d\x62\x65\x72\x3a\x20" << Brief(serial_number());
  os << "\xa\x20\x2d\x20\x63\x61\x6c\x6c\x5f\x63\x6f\x64\x65\x3a\x20" << Brief(call_code());
  os << "\xa\x20\x2d\x20\x70\x72\x6f\x70\x65\x72\x74\x79\x5f\x61\x63\x63\x65\x73\x73\x6f\x72\x73\x3a\x20" << Brief(property_accessors());
  os << "\xa\x20\x2d\x20\x70\x72\x6f\x74\x6f\x74\x79\x70\x65\x5f\x74\x65\x6d\x70\x6c\x61\x74\x65\x3a\x20" << Brief(prototype_template());
  os << "\xa\x20\x2d\x20\x70\x61\x72\x65\x6e\x74\x5f\x74\x65\x6d\x70\x6c\x61\x74\x65\x3a\x20" << Brief(parent_template());
  os << "\xa\x20\x2d\x20\x6e\x61\x6d\x65\x64\x5f\x70\x72\x6f\x70\x65\x72\x74\x79\x5f\x68\x61\x6e\x64\x6c\x65\x72\x3a\x20" << Brief(named_property_handler());
  os << "\xa\x20\x2d\x20\x69\x6e\x64\x65\x78\x65\x64\x5f\x70\x72\x6f\x70\x65\x72\x74\x79\x5f\x68\x61\x6e\x64\x6c\x65\x72\x3a\x20" << Brief(indexed_property_handler());
  os << "\xa\x20\x2d\x20\x69\x6e\x73\x74\x61\x6e\x63\x65\x5f\x74\x65\x6d\x70\x6c\x61\x74\x65\x3a\x20" << Brief(instance_template());
  os << "\xa\x20\x2d\x20\x73\x69\x67\x6e\x61\x74\x75\x72\x65\x3a\x20" << Brief(signature());
  os << "\xa\x20\x2d\x20\x61\x63\x63\x65\x73\x73\x5f\x63\x68\x65\x63\x6b\x5f\x69\x6e\x66\x6f\x3a\x20" << Brief(access_check_info());
  os << "\xa\x20\x2d\x20\x68\x69\x64\x64\x65\x6e\x5f\x70\x72\x6f\x74\x6f\x74\x79\x70\x65\x3a\x20" << (hidden_prototype() ? "\x74\x72\x75\x65" : "\x66\x61\x6c\x73\x65");
  os << "\xa\x20\x2d\x20\x75\x6e\x64\x65\x74\x65\x63\x74\x61\x62\x6c\x65\x3a\x20" << (undetectable() ? "\x74\x72\x75\x65" : "\x66\x61\x6c\x73\x65");
  os << "\xa\x20\x2d\x20\x6e\x65\x65\x64\x5f\x61\x63\x63\x65\x73\x73\x5f\x63\x68\x65\x63\x6b\x3a\x20" << (needs_access_check() ? "\x74\x72\x75\x65" : "\x66\x61\x6c\x73\x65");
  os << "\xa";
}


void ObjectTemplateInfo::ObjectTemplateInfoPrint(OStream& os) {  // NOLINT
  HeapObject::PrintHeader(os, "\x4f\x62\x6a\x65\x63\x74\x54\x65\x6d\x70\x6c\x61\x74\x65\x49\x6e\x66\x6f");
  os << "\x20\x2d\x20\x74\x61\x67\x3a\x20" << Brief(tag());
  os << "\xa\x20\x2d\x20\x70\x72\x6f\x70\x65\x72\x74\x79\x5f\x6c\x69\x73\x74\x3a\x20" << Brief(property_list());
  os << "\xa\x20\x2d\x20\x70\x72\x6f\x70\x65\x72\x74\x79\x5f\x61\x63\x63\x65\x73\x73\x6f\x72\x73\x3a\x20" << Brief(property_accessors());
  os << "\xa\x20\x2d\x20\x63\x6f\x6e\x73\x74\x72\x75\x63\x74\x6f\x72\x3a\x20" << Brief(constructor());
  os << "\xa\x20\x2d\x20\x69\x6e\x74\x65\x72\x6e\x61\x6c\x5f\x66\x69\x65\x6c\x64\x5f\x63\x6f\x75\x6e\x74\x3a\x20" << Brief(internal_field_count());
  os << "\xa";
}


void SignatureInfo::SignatureInfoPrint(OStream& os) {  // NOLINT
  HeapObject::PrintHeader(os, "\x53\x69\x67\x6e\x61\x74\x75\x72\x65\x49\x6e\x66\x6f");
  os << "\xa\x20\x2d\x20\x72\x65\x63\x65\x69\x76\x65\x72\x3a\x20" << Brief(receiver());
  os << "\xa\x20\x2d\x20\x61\x72\x67\x73\x3a\x20" << Brief(args());
  os << "\xa";
}


void TypeSwitchInfo::TypeSwitchInfoPrint(OStream& os) {  // NOLINT
  HeapObject::PrintHeader(os, "\x54\x79\x70\x65\x53\x77\x69\x74\x63\x68\x49\x6e\x66\x6f");
  os << "\xa\x20\x2d\x20\x74\x79\x70\x65\x73\x3a\x20" << Brief(types());
  os << "\xa";
}


void AllocationSite::AllocationSitePrint(OStream& os) {  // NOLINT
  HeapObject::PrintHeader(os, "\x41\x6c\x6c\x6f\x63\x61\x74\x69\x6f\x6e\x53\x69\x74\x65");
  os << "\x20\x2d\x20\x77\x65\x61\x6b\x5f\x6e\x65\x78\x74\x3a\x20" << Brief(weak_next());
  os << "\xa\x20\x2d\x20\x64\x65\x70\x65\x6e\x64\x65\x6e\x74\x20\x63\x6f\x64\x65\x3a\x20" << Brief(dependent_code());
  os << "\xa\x20\x2d\x20\x6e\x65\x73\x74\x65\x64\x20\x73\x69\x74\x65\x3a\x20" << Brief(nested_site());
  os << "\xa\x20\x2d\x20\x6d\x65\x6d\x65\x6e\x74\x6f\x20\x66\x6f\x75\x6e\x64\x20\x63\x6f\x75\x6e\x74\x3a\x20"
     << Brief(Smi::FromInt(memento_found_count()));
  os << "\xa\x20\x2d\x20\x6d\x65\x6d\x65\x6e\x74\x6f\x20\x63\x72\x65\x61\x74\x65\x20\x63\x6f\x75\x6e\x74\x3a\x20"
     << Brief(Smi::FromInt(memento_create_count()));
  os << "\xa\x20\x2d\x20\x70\x72\x65\x74\x65\x6e\x75\x72\x65\x20\x64\x65\x63\x69\x73\x69\x6f\x6e\x3a\x20"
     << Brief(Smi::FromInt(pretenure_decision()));
  os << "\xa\x20\x2d\x20\x74\x72\x61\x6e\x73\x69\x74\x69\x6f\x6e\x5f\x69\x6e\x66\x6f\x3a\x20";
  if (transition_info()->IsSmi()) {
    ElementsKind kind = GetElementsKind();
    os << "\x41\x72\x72\x61\x79\x20\x61\x6c\x6c\x6f\x63\x61\x74\x69\x6f\x6e\x20\x77\x69\x74\x68\x20\x45\x6c\x65\x6d\x65\x6e\x74\x73\x4b\x69\x6e\x64\x20" << ElementsKindToString(kind);
  } else if (transition_info()->IsJSArray()) {
    os << "\x41\x72\x72\x61\x79\x20\x6c\x69\x74\x65\x72\x61\x6c\x20" << Brief(transition_info());
  } else {
    os << "\x75\x6e\x6b\x6e\x6f\x77\x6e\x20\x74\x72\x61\x6e\x73\x69\x74\x69\x6f\x6e\x5f\x69\x6e\x66\x6f" << Brief(transition_info());
  }
  os << "\xa";
}


void AllocationMemento::AllocationMementoPrint(OStream& os) {  // NOLINT
  HeapObject::PrintHeader(os, "\x41\x6c\x6c\x6f\x63\x61\x74\x69\x6f\x6e\x4d\x65\x6d\x65\x6e\x74\x6f");
  os << "\x20\x2d\x20\x61\x6c\x6c\x6f\x63\x61\x74\x69\x6f\x6e\x20\x73\x69\x74\x65\x3a\x20";
  if (IsValid()) {
    GetAllocationSite()->Print(os);
  } else {
    os << "\x3c\x69\x6e\x76\x61\x6c\x69\x64\x3e\xa";
  }
}


void Script::ScriptPrint(OStream& os) {  // NOLINT
  HeapObject::PrintHeader(os, "\x53\x63\x72\x69\x70\x74");
  os << "\xa\x20\x2d\x20\x73\x6f\x75\x72\x63\x65\x3a\x20" << Brief(source());
  os << "\xa\x20\x2d\x20\x6e\x61\x6d\x65\x3a\x20" << Brief(name());
  os << "\xa\x20\x2d\x20\x6c\x69\x6e\x65\x5f\x6f\x66\x66\x73\x65\x74\x3a\x20" << Brief(line_offset());
  os << "\xa\x20\x2d\x20\x63\x6f\x6c\x75\x6d\x6e\x5f\x6f\x66\x66\x73\x65\x74\x3a\x20" << Brief(column_offset());
  os << "\xa\x20\x2d\x20\x74\x79\x70\x65\x3a\x20" << Brief(type());
  os << "\xa\x20\x2d\x20\x69\x64\x3a\x20" << Brief(id());
  os << "\xa\x20\x2d\x20\x63\x6f\x6e\x74\x65\x78\x74\x20\x64\x61\x74\x61\x3a\x20" << Brief(context_data());
  os << "\xa\x20\x2d\x20\x77\x72\x61\x70\x70\x65\x72\x3a\x20" << Brief(wrapper());
  os << "\xa\x20\x2d\x20\x63\x6f\x6d\x70\x69\x6c\x61\x74\x69\x6f\x6e\x20\x74\x79\x70\x65\x3a\x20" << compilation_type();
  os << "\xa\x20\x2d\x20\x6c\x69\x6e\x65\x20\x65\x6e\x64\x73\x3a\x20" << Brief(line_ends());
  os << "\xa\x20\x2d\x20\x65\x76\x61\x6c\x20\x66\x72\x6f\x6d\x20\x73\x68\x61\x72\x65\x64\x3a\x20" << Brief(eval_from_shared());
  os << "\xa\x20\x2d\x20\x65\x76\x61\x6c\x20\x66\x72\x6f\x6d\x20\x69\x6e\x73\x74\x72\x75\x63\x74\x69\x6f\x6e\x73\x20\x6f\x66\x66\x73\x65\x74\x3a\x20"
     << Brief(eval_from_instructions_offset());
  os << "\xa";
}


void DebugInfo::DebugInfoPrint(OStream& os) {  // NOLINT
  HeapObject::PrintHeader(os, "\x44\x65\x62\x75\x67\x49\x6e\x66\x6f");
  os << "\xa\x20\x2d\x20\x73\x68\x61\x72\x65\x64\x3a\x20" << Brief(shared());
  os << "\xa\x20\x2d\x20\x6f\x72\x69\x67\x69\x6e\x61\x6c\x5f\x63\x6f\x64\x65\x3a\x20" << Brief(original_code());
  os << "\xa\x20\x2d\x20\x63\x6f\x64\x65\x3a\x20" << Brief(code());
  os << "\xa\x20\x2d\x20\x62\x72\x65\x61\x6b\x5f\x70\x6f\x69\x6e\x74\x73\x3a\x20";
  break_points()->Print(os);
}


void BreakPointInfo::BreakPointInfoPrint(OStream& os) {  // NOLINT
  HeapObject::PrintHeader(os, "\x42\x72\x65\x61\x6b\x50\x6f\x69\x6e\x74\x49\x6e\x66\x6f");
  os << "\xa\x20\x2d\x20\x63\x6f\x64\x65\x5f\x70\x6f\x73\x69\x74\x69\x6f\x6e\x3a\x20" << code_position()->value();
  os << "\xa\x20\x2d\x20\x73\x6f\x75\x72\x63\x65\x5f\x70\x6f\x73\x69\x74\x69\x6f\x6e\x3a\x20" << source_position()->value();
  os << "\xa\x20\x2d\x20\x73\x74\x61\x74\x65\x6d\x65\x6e\x74\x5f\x70\x6f\x73\x69\x74\x69\x6f\x6e\x3a\x20" << statement_position()->value();
  os << "\xa\x20\x2d\x20\x62\x72\x65\x61\x6b\x5f\x70\x6f\x69\x6e\x74\x5f\x6f\x62\x6a\x65\x63\x74\x73\x3a\x20" << Brief(break_point_objects());
  os << "\xa";
}


void DescriptorArray::PrintDescriptors(OStream& os) {  // NOLINT
  os << "\x44\x65\x73\x63\x72\x69\x70\x74\x6f\x72\x20\x61\x72\x72\x61\x79\x20\x20" << number_of_descriptors() << "\xa";
  for (int i = 0; i < number_of_descriptors(); i++) {
    Descriptor desc;
    Get(i, &desc);
    os << "\x20" << i << "\x3a\x20" << desc;
  }
  os << "\xa";
}


void TransitionArray::PrintTransitions(OStream& os) {  // NOLINT
  os << "\x54\x72\x61\x6e\x73\x69\x74\x69\x6f\x6e\x20\x61\x72\x72\x61\x79\x20\x20\x6c\x84\xa", number_of_transitions();
  for (int i = 0; i < number_of_transitions(); i++) {
    os << "\x20" << i << "\x3a\x20";
    GetKey(i)->NamePrint(os);
    os << "\x3a\x20";
    switch (GetTargetDetails(i).type()) {
      case FIELD: {
        os << "\x20\x28\x74\x72\x61\x6e\x73\x69\x74\x69\x6f\x6e\x20\x74\x6f\x20\x66\x69\x65\x6c\x64\x29\xa";
        break;
      }
      case CONSTANT:
        os << "\x20\x28\x74\x72\x61\x6e\x73\x69\x74\x69\x6f\x6e\x20\x74\x6f\x20\x63\x6f\x6e\x73\x74\x61\x6e\x74\x29\xa";
        break;
      case CALLBACKS:
        os << "\x20\x28\x74\x72\x61\x6e\x73\x69\x74\x69\x6f\x6e\x20\x74\x6f\x20\x63\x61\x6c\x6c\x62\x61\x63\x6b\x29\xa";
        break;
      // Values below are never in the target descriptor array.
      case NORMAL:
      case HANDLER:
      case INTERCEPTOR:
      case NONEXISTENT:
        UNREACHABLE();
        break;
    }
  }
  os << "\xa";
}


#endif  // OBJECT_PRINT


} }  // namespace v8::internal
