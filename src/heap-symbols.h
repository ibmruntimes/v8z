// Copyright 2015 the V8 project authors. All rights reserved.
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.

#ifndef V8_HEAP_SYMBOLS_H_
#define V8_HEAP_SYMBOLS_H_

#define INTERNALIZED_STRING_LIST(V)                                \
  V(anonymous_string, u8"anonymous")                                 \
  V(apply_string, u8"apply")                                         \
  V(assign_string, u8"assign")                                       \
  V(arguments_string, u8"arguments")                                 \
  V(Arguments_string, u8"Arguments")                                 \
  V(Array_string, u8"Array")                                         \
  V(bind_string, u8"bind")                                           \
  V(bool16x8_string, u8"bool16x8")                                   \
  V(Bool16x8_string, u8"Bool16x8")                                   \
  V(bool32x4_string, u8"bool32x4")                                   \
  V(Bool32x4_string, u8"Bool32x4")                                   \
  V(bool8x16_string, u8"bool8x16")                                   \
  V(Bool8x16_string, u8"Bool8x16")                                   \
  V(boolean_string, u8"boolean")                                     \
  V(Boolean_string, u8"Boolean")                                     \
  V(bound__string, u8"bound ")                                       \
  V(byte_length_string, u8"byteLength")                              \
  V(byte_offset_string, u8"byteOffset")                              \
  V(call_string, u8"call")                                           \
  V(callee_string, u8"callee")                                       \
  V(caller_string, u8"caller")                                       \
  V(cell_value_string, u8"%cell_value")                              \
  V(char_at_string, u8"CharAt")                                      \
  V(closure_string, u8"(closure)")                                   \
  V(compare_ic_string, u8"==")                                       \
  V(configurable_string, u8"configurable")                           \
  V(constructor_string, u8"constructor")                             \
  V(construct_string, u8"construct")                                 \
  V(create_string, u8"create")                                       \
  V(Date_string, u8"Date")                                           \
  V(default_string, u8"default")                                     \
  V(defineProperty_string, u8"defineProperty")                       \
  V(deleteProperty_string, u8"deleteProperty")                       \
  V(display_name_string, u8"displayName")                            \
  V(done_string, u8"done")                                           \
  V(dot_result_string, u8".result")                                  \
  V(dot_string, u8".")                                               \
  V(entries_string, u8"entries")                                     \
  V(enumerable_string, u8"enumerable")                               \
  V(Error_string, u8"Error")                                         \
  V(eval_string, u8"eval")                                           \
  V(false_string, u8"false")                                         \
  V(float32x4_string, u8"float32x4")                                 \
  V(Float32x4_string, u8"Float32x4")                                 \
  V(for_api_string, u8"for_api")                                     \
  V(for_string, u8"for")                                             \
  V(function_string, u8"function")                                   \
  V(Function_string, u8"Function")                                   \
  V(Generator_string, u8"Generator")                                 \
  V(getOwnPropertyDescriptor_string, u8"getOwnPropertyDescriptor")   \
  V(getOwnPropertyDescriptors_string, u8"getOwnPropertyDescriptors") \
  V(getPrototypeOf_string, u8"getPrototypeOf")                       \
  V(get_string, u8"get")                                             \
  V(global_string, u8"global")                                       \
  V(has_string, u8"has")                                             \
  V(illegal_access_string, u8"illegal access")                       \
  V(illegal_argument_string, u8"illegal argument")                   \
  V(index_string, u8"index")                                         \
  V(infinity_string, u8"Infinity")                                   \
  V(input_string, u8"input")                                         \
  V(int16x8_string, u8"int16x8")                                     \
  V(Int16x8_string, u8"Int16x8")                                     \
  V(int32x4_string, u8"int32x4")                                     \
  V(Int32x4_string, u8"Int32x4")                                     \
  V(int8x16_string, u8"int8x16")                                     \
  V(Int8x16_string, u8"Int8x16")                                     \
  V(isExtensible_string, u8"isExtensible")                           \
  V(isView_string, u8"isView")                                       \
  V(KeyedLoadMonomorphic_string, u8"KeyedLoadMonomorphic")           \
  V(KeyedStoreMonomorphic_string, u8"KeyedStoreMonomorphic")         \
  V(last_index_string, u8"lastIndex")                                \
  V(length_string, u8"length")                                       \
  V(Map_string, u8"Map")                                             \
  V(minus_infinity_string, u8"-Infinity")                            \
  V(minus_zero_string, u8"-0")                                       \
  V(name_string, u8"name")                                           \
  V(nan_string, u8"NaN")                                             \
  V(next_string, u8"next")                                           \
  V(null_string, u8"null")                                           \
  V(null_to_string, u8"[object Null]")                               \
  V(number_string, u8"number")                                       \
  V(Number_string, u8"Number")                                       \
  V(object_string, u8"object")                                       \
  V(Object_string, u8"Object")                                       \
  V(ownKeys_string, u8"ownKeys")                                     \
  V(preventExtensions_string, u8"preventExtensions")                 \
  V(private_api_string, u8"private_api")                             \
  V(Promise_string, u8"Promise")                                     \
  V(proto_string, u8"__proto__")                                     \
  V(prototype_string, u8"prototype")                                 \
  V(Proxy_string, u8"Proxy")                                         \
  V(query_colon_string, u8"(?:)")                                    \
  V(RegExp_string, u8"RegExp")                                       \
  V(setPrototypeOf_string, u8"setPrototypeOf")                       \
  V(set_string, u8"set")                                             \
  V(Set_string, u8"Set")                                             \
  V(source_mapping_url_string, u8"source_mapping_url")               \
  V(source_string, u8"source")                                       \
  V(source_url_string, u8"source_url")                               \
  V(stack_string, u8"stack")                                         \
  V(strict_compare_ic_string, u8"===")                               \
  V(string_string, u8"string")                                       \
  V(String_string, u8"String")                                       \
  V(symbol_string, u8"symbol")                                       \
  V(Symbol_string, u8"Symbol")                                       \
  V(this_string, u8"this")                                           \
  V(throw_string, u8"throw")                                         \
  V(toJSON_string, u8"toJSON")                                       \
  V(toString_string, u8"toString")                                   \
  V(true_string, u8"true")                                           \
  V(uint16x8_string, u8"uint16x8")                                   \
  V(Uint16x8_string, u8"Uint16x8")                                   \
  V(uint32x4_string, u8"uint32x4")                                   \
  V(Uint32x4_string, u8"Uint32x4")                                   \
  V(uint8x16_string, u8"uint8x16")                                   \
  V(Uint8x16_string, u8"Uint8x16")                                   \
  V(undefined_string, u8"undefined")                                 \
  V(undefined_to_string, u8"[object Undefined]")                     \
  V(valueOf_string, u8"valueOf")                                     \
  V(values_string, u8"values")                                       \
  V(value_string, u8"value")                                         \
  V(WeakMap_string, u8"WeakMap")                                     \
  V(WeakSet_string, u8"WeakSet")                                     \
  V(writable_string, u8"writable")

#define PRIVATE_SYMBOL_LIST(V)              \
  V(array_iteration_kind_symbol)            \
  V(array_iterator_next_symbol)             \
  V(array_iterator_object_symbol)           \
  V(call_site_function_symbol)              \
  V(call_site_position_symbol)              \
  V(call_site_receiver_symbol)              \
  V(call_site_strict_symbol)                \
  V(class_end_position_symbol)              \
  V(class_start_position_symbol)            \
  V(detailed_stack_trace_symbol)            \
  V(elements_transition_symbol)             \
  V(error_end_pos_symbol)                   \
  V(error_script_symbol)                    \
  V(error_start_pos_symbol)                 \
  V(formatted_stack_trace_symbol)           \
  V(frozen_symbol)                          \
  V(hash_code_symbol)                       \
  V(hidden_properties_symbol)               \
  V(home_object_symbol)                     \
  V(internal_error_symbol)                  \
  V(intl_impl_object_symbol)                \
  V(intl_initialized_marker_symbol)         \
  V(intl_pattern_symbol)                    \
  V(intl_resolved_symbol)                   \
  V(megamorphic_symbol)                     \
  V(native_context_index_symbol)            \
  V(nonexistent_symbol)                     \
  V(nonextensible_symbol)                   \
  V(normal_ic_symbol)                       \
  V(not_mapped_symbol)                      \
  V(observed_symbol)                        \
  V(premonomorphic_symbol)                  \
  V(promise_combined_deferred_symbol)       \
  V(promise_debug_marker_symbol)            \
  V(promise_has_handler_symbol)             \
  V(promise_on_resolve_symbol)              \
  V(promise_on_reject_symbol)               \
  V(promise_raw_symbol)                     \
  V(promise_status_symbol)                  \
  V(promise_value_symbol)                   \
  V(sealed_symbol)                          \
  V(stack_trace_symbol)                     \
  V(strict_function_transition_symbol)      \
  V(string_iterator_iterated_string_symbol) \
  V(string_iterator_next_index_symbol)      \
  V(uninitialized_symbol)

#define PUBLIC_SYMBOL_LIST(V)                \
  V(iterator_symbol, Symbol.iterator)        \
  V(match_symbol, Symbol.match)              \
  V(replace_symbol, Symbol.replace)          \
  V(search_symbol, Symbol.search)            \
  V(species_symbol, Symbol.species)          \
  V(split_symbol, Symbol.split)              \
  V(to_primitive_symbol, Symbol.toPrimitive) \
  V(unscopables_symbol, Symbol.unscopables)

// Well-Known Symbols are "Public" symbols, which have a bit set which causes
// them to produce an undefined value when a load results in a failed access
// check. Because this behaviour is not specified properly as of yet, it only
// applies to a subset of spec-defined Well-Known Symbols.
#define WELL_KNOWN_SYMBOL_LIST(V)                           \
  V(has_instance_symbol, Symbol.hasInstance)                \
  V(is_concat_spreadable_symbol, Symbol.isConcatSpreadable) \
  V(to_string_tag_symbol, Symbol.toStringTag)

#endif  // V8_HEAP_SYMBOLS_H_
