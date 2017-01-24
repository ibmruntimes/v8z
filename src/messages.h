// Copyright 2006-2008 the V8 project authors. All rights reserved.
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.

// The infrastructure used for (localized) message reporting in V8.
//
// Note: there's a big unresolved issue about ownership of the data
// structures used by this framework.

#ifndef V8_MESSAGES_H_
#define V8_MESSAGES_H_

#include "src/base/smart-pointers.h"
#include "src/handles.h"
#include "src/list.h"

namespace v8 {
namespace internal {

// Forward declarations.
class JSMessageObject;
class LookupIterator;
class SourceInfo;

class MessageLocation {
 public:
  MessageLocation(Handle<Script> script, int start_pos, int end_pos,
                  Handle<JSFunction> function = Handle<JSFunction>())
      : script_(script),
        start_pos_(start_pos),
        end_pos_(end_pos),
        function_(function) {}
  MessageLocation() : start_pos_(-1), end_pos_(-1) { }

  Handle<Script> script() const { return script_; }
  int start_pos() const { return start_pos_; }
  int end_pos() const { return end_pos_; }
  Handle<JSFunction> function() const { return function_; }

 private:
  Handle<Script> script_;
  int start_pos_;
  int end_pos_;
  Handle<JSFunction> function_;
};


class CallSite {
 public:
  CallSite(Isolate* isolate, Handle<JSObject> call_site_obj);

  Handle<Object> GetFileName();
  Handle<Object> GetFunctionName();
  Handle<Object> GetScriptNameOrSourceUrl();
  Handle<Object> GetMethodName();
  // Return 1-based line number, including line offset.
  int GetLineNumber();
  // Return 1-based column number, including column offset if first line.
  int GetColumnNumber();
  bool IsNative();
  bool IsToplevel();
  bool IsEval();
  bool IsConstructor();

  bool IsValid() { return !fun_.is_null(); }

 private:
  Isolate* isolate_;
  Handle<Object> receiver_;
  Handle<JSFunction> fun_;
  int32_t pos_;
};

#define MESSAGE_TEMPLATES(T)                                                   \
  /* Error */                                                                  \
  T(None, u8"")                                                                  \
  T(CyclicProto, u8"Cyclic __proto__ value")                                     \
  T(Debugger, u8"Debugger: %")                                                   \
  T(DebuggerLoading, u8"Error loading debugger")                                 \
  T(DefaultOptionsMissing, u8"Internal % error. Default options are missing.")   \
  T(UncaughtException, u8"Uncaught %")                                           \
  T(Unsupported, u8"Not supported")                                              \
  T(WrongServiceType, u8"Internal error, wrong service type: %")                 \
  T(WrongValueType, u8"Internal error. Wrong value type.")                       \
  /* TypeError */                                                              \
  T(ApplyNonFunction,                                                          \
    u8"Function.prototype.apply was called on %, which is a % and not a "        \
    u8"function")                                                                \
  T(ArrayBufferTooShort,                                                       \
    u8"Derived ArrayBuffer constructor created a buffer which was too small")    \
  T(ArrayBufferSpeciesThis,                                                    \
    u8"ArrayBuffer subclass returned this from species constructor")             \
  T(ArrayFunctionsOnFrozen, u8"Cannot modify frozen array elements")             \
  T(ArrayFunctionsOnSealed, u8"Cannot add/remove sealed array elements")         \
  T(ArrayNotSubclassable, u8"Subclassing Arrays is not currently supported.")    \
  T(CalledNonCallable, u8"% is not a function")                                  \
  T(CalledNonCallableInstanceOf,                                               \
    u8"Right-hand side of 'instanceof' is not callable")                         \
  T(CalledOnNonObject, u8"% called on non-object")                               \
  T(CalledOnNullOrUndefined, u8"% called on null or undefined")                  \
  T(CallSiteExpectsFunction,                                                   \
    u8"CallSite expects function as second argument, got %")                     \
  T(CallSiteMethod, u8"CallSite method % expects CallSite as receiver")          \
  T(CannotConvertToPrimitive, u8"Cannot convert object to primitive value")      \
  T(CannotPreventExt, u8"Cannot prevent extensions")                             \
  T(CannotFreezeArrayBufferView,                                               \
    u8"Cannot freeze array buffer views with elements")                          \
  T(CircularStructure, u8"Converting circular structure to JSON")                \
  T(ConstructAbstractClass, u8"Abstract class % not directly constructable")     \
  T(ConstAssign, u8"Assignment to constant variable.")                           \
  T(ConstructorNonCallable,                                                    \
    u8"Class constructor % cannot be invoked without 'new'")                     \
  T(ConstructorNotFunction, u8"Constructor % requires 'new'")                    \
  T(ConstructorNotReceiver, u8"The .constructor property is not an object")      \
  T(CurrencyCode, u8"Currency code is required with currency style.")            \
  T(DataViewNotArrayBuffer,                                                    \
    u8"First argument to DataView constructor must be an ArrayBuffer")           \
  T(DateType, u8"this is not a Date object.")                                    \
  T(DebuggerFrame, u8"Debugger: Invalid frame index.")                           \
  T(DebuggerType, u8"Debugger: Parameters have wrong types.")                    \
  T(DeclarationMissingInitializer, u8"Missing initializer in % declaration")     \
  T(DefineDisallowed, u8"Cannot define property:%, object is not extensible.")   \
  T(DuplicateTemplateProperty, u8"Object template has duplicate property '%'")   \
  T(ExtendsValueGenerator,                                                     \
    u8"Class extends value % may not be a generator function")                   \
  T(ExtendsValueNotFunction,                                                   \
    u8"Class extends value % is not a function or null")                         \
  T(FirstArgumentNotRegExp,                                                    \
    u8"First argument to % must not be a regular expression")                    \
  T(FunctionBind, u8"Bind must be called on a function")                         \
  T(GeneratorRunning, u8"Generator is already running")                          \
  T(IllegalInvocation, u8"Illegal invocation")                                   \
  T(IncompatibleMethodReceiver, u8"Method % called on incompatible receiver %")  \
  T(InstanceofFunctionExpected,                                                \
    u8"Expecting a function in instanceof check, but got %")                     \
  T(InstanceofNonobjectProto,                                                  \
    u8"Function has non-object prototype '%' in instanceof check")               \
  T(InvalidArgument, u8"invalid_argument")                                       \
  T(InvalidInOperatorUse, u8"Cannot use 'in' operator to search for '%' in %")   \
  T(InvalidRegExpExecResult,                                                   \
    u8"RegExp exec method returned something other than an Object or null")      \
  T(InvalidSimdOperation, u8"% is not a valid type for this SIMD operation.")    \
  T(IteratorResultNotAnObject, u8"Iterator result % is not an object")           \
  T(IteratorValueNotAnObject, u8"Iterator value % is not an entry object")       \
  T(LanguageID, u8"Language ID should be string or object.")                     \
  T(MethodCalledOnWrongObject,                                                 \
    u8"Method % called on a non-object or on a wrong type of object.")           \
  T(MethodInvokedOnNullOrUndefined,                                            \
    u8"Method invoked on undefined or null value.")                              \
  T(MethodInvokedOnWrongType, u8"Method invoked on an object that is not %.")    \
  T(NoAccess, u8"no access")                                                     \
  T(NonCoercible, u8"Cannot match against 'undefined' or 'null'.")               \
  T(NonExtensibleProto, u8"% is not extensible")                                 \
  T(NonObjectInInstanceOfCheck,                                                \
    u8"Right-hand side of 'instanceof' is not an object")                        \
  T(NonObjectPropertyLoad, u8"Cannot read property '%' of %")                    \
  T(NonObjectPropertyStore, u8"Cannot set property '%' of %")                    \
  T(NoSetterInCallback, u8"Cannot set property % of % which has only a getter")  \
  T(NotAnIterator, u8"% is not an iterator")                                     \
  T(NotAPromise, u8"% is not a promise")                                         \
  T(NotConstructor, u8"% is not a constructor")                                  \
  T(NotDateObject, u8"this is not a Date object.")                               \
  T(NotIntlObject, u8"% is not an i18n object.")                                 \
  T(NotGeneric, u8"% is not generic")                                            \
  T(NotIterable, u8"% is not iterable")                                          \
  T(NotPropertyName, u8"% is not a valid property name")                         \
  T(NotTypedArray, u8"this is not a typed array.")                               \
  T(NotSharedTypedArray, u8"% is not a shared typed array.")                     \
  T(NotIntegerSharedTypedArray, u8"% is not an integer shared typed array.")     \
  T(NotInt32SharedTypedArray, u8"% is not an int32 shared typed array.")         \
  T(ObjectGetterExpectingFunction,                                             \
    u8"Object.prototype.__defineGetter__: Expecting function")                   \
  T(ObjectGetterCallable, u8"Getter must be a function: %")                      \
  T(ObjectNotExtensible, u8"Can't add property %, object is not extensible")     \
  T(ObjectSetterExpectingFunction,                                             \
    u8"Object.prototype.__defineSetter__: Expecting function")                   \
  T(ObjectSetterCallable, u8"Setter must be a function: %")                      \
  T(ObserveCallbackFrozen,                                                     \
    u8"Object.observe cannot deliver to a frozen function object")               \
  T(ObserveGlobalProxy, u8"% cannot be called on the global proxy object")       \
  T(ObserveAccessChecked, u8"% cannot be called on access-checked objects")      \
  T(ObserveInvalidAccept,                                                      \
    u8"Third argument to Object.observe must be an array of strings.")           \
  T(ObserveNonFunction, u8"Object.% cannot deliver to non-function")             \
  T(ObserveNonObject, u8"Object.% cannot % non-object")                          \
  T(ObserveNotifyNonNotifier, u8"notify called on non-notifier object")          \
  T(ObservePerformNonFunction, u8"Cannot perform non-function")                  \
  T(ObservePerformNonString, u8"Invalid non-string changeType")                  \
  T(ObserveTypeNonString,                                                      \
    u8"Invalid changeRecord with non-string 'type' property")                    \
  T(OrdinaryFunctionCalledAsConstructor,                                       \
    u8"Function object that's not a constructor was created with new")           \
  T(PromiseCyclic, u8"Chaining cycle detected for promise %")                    \
  T(PromiseExecutorAlreadyInvoked,                                             \
    u8"Promise executor has already been invoked with non-undefined arguments")  \
  T(PromiseNonCallable, u8"Promise resolve or reject function is not callable")  \
  T(PropertyDescObject, u8"Property description must be an object: %")           \
  T(PropertyNotFunction,                                                       \
    u8"'%' returned for property '%' of object '%' is not a function")           \
  T(ProtoObjectOrNull, u8"Object prototype may only be an Object or null: %")    \
  T(PrototypeParentNotAnObject,                                                \
    u8"Class extends value does not have valid prototype property %")            \
  T(ProxyConstructNonObject,                                                   \
    u8"'construct' on proxy: trap returned non-object ('%')")                    \
  T(ProxyDefinePropertyNonConfigurable,                                        \
    u8"'defineProperty' on proxy: trap returned truish for defining "            \
    u8"non-configurable property '%' which is either non-existant or "           \
    u8"configurable in the proxy target")                                        \
  T(ProxyDefinePropertyNonExtensible,                                          \
    u8"'defineProperty' on proxy: trap returned truish for adding property '%' " \
    u8" to the non-extensible proxy target")                                     \
  T(ProxyDefinePropertyIncompatible,                                           \
    u8"'defineProperty' on proxy: trap returned truish for adding property '%' " \
    u8" that is incompatible with the existing property in the proxy target")    \
  T(ProxyDeletePropertyNonConfigurable,                                        \
    u8"'deleteProperty' on proxy: trap returned truish for property '%' which "  \
    u8"is non-configurable in the proxy target")                                 \
  T(ProxyGetNonConfigurableData,                                               \
    u8"'get' on proxy: property '%' is a read-only and "                         \
    u8"non-configurable data property on the proxy target but the proxy "        \
    u8"did not return its actual value (expected '%' but got '%')")              \
  T(ProxyGetNonConfigurableAccessor,                                           \
    u8"'get' on proxy: property '%' is a non-configurable accessor "             \
    u8"property on the proxy target and does not have a getter function, but "   \
    u8"the trap did not return 'undefined' (got '%')")                           \
  T(ProxyGetOwnPropertyDescriptorIncompatible,                                 \
    u8"'getOwnPropertyDescriptor' on proxy: trap returned descriptor for "       \
    u8"property '%' that is incompatible with the existing property in the "     \
    u8"proxy target")                                                            \
  T(ProxyGetOwnPropertyDescriptorInvalid,                                      \
    u8"'getOwnPropertyDescriptor' on proxy: trap returned neither object nor "   \
    u8"undefined for property '%'")                                              \
  T(ProxyGetOwnPropertyDescriptorNonConfigurable,                              \
    u8"'getOwnPropertyDescriptor' on proxy: trap reported non-configurability "  \
    u8"for property '%' which is either non-existant or configurable in the "    \
    u8"proxy target")                                                            \
  T(ProxyGetOwnPropertyDescriptorNonExtensible,                                \
    u8"'getOwnPropertyDescriptor' on proxy: trap returned undefined for "        \
    u8"property '%' which exists in the non-extensible proxy target")            \
  T(ProxyGetOwnPropertyDescriptorUndefined,                                    \
    u8"'getOwnPropertyDescriptor' on proxy: trap returned undefined for "        \
    u8"property '%' which is non-configurable in the proxy target")              \
  T(ProxyGetPrototypeOfInvalid,                                                \
    u8"'getPrototypeOf' on proxy: trap returned neither object nor null")        \
  T(ProxyGetPrototypeOfNonExtensible,                                          \
    u8"'getPrototypeOf' on proxy: proxy target is non-extensible but the "       \
    u8"trap did not return its actual prototype")                                \
  T(ProxyHandlerOrTargetRevoked,                                               \
    u8"Cannot create proxy with a revoked proxy as target or handler")           \
  T(ProxyHasNonConfigurable,                                                   \
    u8"'has' on proxy: trap returned falsish for property '%' which exists in "  \
    u8"the proxy target as non-configurable")                                    \
  T(ProxyHasNonExtensible,                                                     \
    u8"'has' on proxy: trap returned falsish for property '%' but the proxy "    \
    u8"target is not extensible")                                                \
  T(ProxyIsExtensibleInconsistent,                                             \
    u8"'isExtensible' on proxy: trap result does not reflect extensibility of "  \
    u8"proxy target (which is '%')")                                             \
  T(ProxyNonObject,                                                            \
    u8"Cannot create proxy with a non-object as target or handler")              \
  T(ProxyOwnKeysMissing,                                                       \
    u8"'ownKeys' on proxy: trap result did not include '%'")                     \
  T(ProxyOwnKeysNonExtensible,                                                 \
    u8"'ownKeys' on proxy: trap returned extra keys but proxy target is "        \
    u8"non-extensible")                                                          \
  T(ProxyPreventExtensionsExtensible,                                          \
    u8"'preventExtensions' on proxy: trap returned truish but the proxy target " \
    u8"is extensible")                                                           \
  T(ProxyPrivate, u8"Cannot pass private property name to proxy trap")           \
  T(ProxyRevoked, u8"Cannot perform '%' on a proxy that has been revoked")       \
  T(ProxySetFrozenData,                                                        \
    u8"'set' on proxy: trap returned truish for property '%' which exists in "   \
    u8"the proxy target as a non-configurable and non-writable data property "   \
    u8"with a different value")                                                  \
  T(ProxySetFrozenAccessor,                                                    \
    u8"'set' on proxy: trap returned truish for property '%' which exists in "   \
    u8"the proxy target as a non-configurable and non-writable accessor "        \
    u8"property without a setter")                                               \
  T(ProxySetPrototypeOfNonExtensible,                                          \
    u8"'setPrototypeOf' on proxy: trap returned truish for setting a new "       \
    u8"prototype on the non-extensible proxy target")                            \
  T(ProxyTrapReturnedFalsish, u8"'%' on proxy: trap returned falsish")           \
  T(ProxyTrapReturnedFalsishFor,                                               \
    u8"'%' on proxy: trap returned falsish for property '%'")                    \
  T(ReadGlobalReferenceThroughProxy, u8"Trying to access '%' through proxy")     \
  T(RedefineDisallowed, u8"Cannot redefine property: %")                         \
  T(RedefineExternalArray,                                                     \
    u8"Cannot redefine a property of an object with external array elements")    \
  T(ReduceNoInitial, u8"Reduce of empty array with no initial value")            \
  T(RegExpFlags,                                                               \
    u8"Cannot supply flags when constructing one RegExp from another")           \
  T(RegExpNonObject, u8"% getter called on non-object %")                        \
  T(RegExpNonRegExp, u8"% getter called on non-RegExp object")                   \
  T(ReinitializeIntl, u8"Trying to re-initialize % object.")                     \
  T(ResolvedOptionsCalledOnNonObject,                                          \
    u8"resolvedOptions method called on a non-object or on a object that is "    \
    u8"not Intl.%.")                                                             \
  T(ResolverNotAFunction, u8"Promise resolver % is not a function")              \
  T(RestrictedFunctionProperties,                                              \
    u8"'caller' and 'arguments' are restricted function properties and cannot "  \
    u8"be accessed in this context.")                                            \
  T(ReturnMethodNotCallable, u8"The iterator's 'return' method is not callable") \
  T(StaticPrototype, u8"Classes may not have static property named prototype")   \
  T(StrictCannotAssign, u8"Cannot assign to read only '%' in strict mode")       \
  T(StrictDeleteProperty, u8"Cannot delete property '%' of %")                   \
  T(StrictPoisonPill,                                                          \
    u8"'caller', 'callee', and 'arguments' properties may not be accessed on "   \
    u8"strict mode functions or the arguments objects for calls to them")        \
  T(StrictReadOnlyProperty,                                                    \
    u8"Cannot assign to read only property '%' of % '%'")                        \
  T(StrictCannotCreateProperty, u8"Cannot create property '%' on % '%'")         \
  T(SymbolIteratorInvalid,                                                     \
    u8"Result of the Symbol.iterator method is not an object")                   \
  T(SymbolKeyFor, u8"% is not a symbol")                                         \
  T(SymbolToNumber, u8"Cannot convert a Symbol value to a number")               \
  T(SymbolToString, u8"Cannot convert a Symbol value to a string")               \
  T(SimdToNumber, u8"Cannot convert a SIMD value to a number")                   \
  T(ThrowMethodMissing, u8"The iterator does not provide a 'throw' method.")     \
  T(UndefinedOrNullToObject, u8"Cannot convert undefined or null to object")     \
  T(ValueAndAccessor,                                                          \
    u8"Invalid property descriptor. Cannot both specify accessors and a value "  \
    u8"or writable attribute, %")                                                \
  T(VarRedeclaration, u8"Identifier '%' has already been declared")              \
  T(WrongArgs, u8"%: Arguments list has wrong type")                             \
  /* ReferenceError */                                                         \
  T(NonMethod, u8"'super' is referenced from non-method")                        \
  T(NotDefined, u8"% is not defined")                                            \
  T(UnsupportedSuper, u8"Unsupported reference to 'super'")                      \
  /* RangeError */                                                             \
  T(DateRange, u8"Provided date is not in valid range.")                         \
  T(ExpectedTimezoneID,                                                        \
    u8"Expected Area/Location(/Location)* for time zone, got %")                 \
  T(ExpectedLocation,                                                          \
    u8"Expected letters optionally connected with underscores or hyphens for "   \
    u8"a location, got %")                                                       \
  T(InvalidArrayBufferLength, u8"Invalid array buffer length")                   \
  T(ArrayBufferAllocationFailed, u8"Array buffer allocation failed")             \
  T(InvalidArrayLength, u8"Invalid array length")                                \
  T(InvalidAtomicAccessIndex, u8"Invalid atomic access index")                   \
  T(InvalidCodePoint, u8"Invalid code point %")                                  \
  T(InvalidCountValue, u8"Invalid count value")                                  \
  T(InvalidCurrencyCode, u8"Invalid currency code: %")                           \
  T(InvalidDataViewAccessorOffset,                                             \
    u8"Offset is outside the bounds of the DataView")                            \
  T(InvalidDataViewLength, u8"Invalid data view length")                         \
  T(InvalidDataViewOffset, u8"Start offset is outside the bounds of the buffer") \
  T(InvalidHint, u8"Invalid hint: %")                                            \
  T(InvalidLanguageTag, u8"Invalid language tag: %")                             \
  T(InvalidWeakMapKey, u8"Invalid value used as weak map key")                   \
  T(InvalidWeakSetValue, u8"Invalid value used in weak set")                     \
  T(InvalidStringLength, u8"Invalid string length")                              \
  T(InvalidTimeValue, u8"Invalid time value")                                    \
  T(InvalidTypedArrayAlignment, u8"% of % should be a multiple of %")            \
  T(InvalidTypedArrayLength, u8"Invalid typed array length")                     \
  T(InvalidTypedArrayOffset, u8"Start offset is too large:")                     \
  T(LetInLexicalBinding, u8"let is disallowed as a lexically bound name")        \
  T(LocaleMatcher, u8"Illegal value for localeMatcher:%")                        \
  T(NormalizationForm, u8"The normalization form should be one of %.")           \
  T(NumberFormatRange, u8"% argument must be between 0 and 20")                  \
  T(PropertyValueOutOfRange, u8"% value is out of range.")                       \
  T(StackOverflow, u8"Maximum call stack size exceeded")                         \
  T(ToPrecisionFormatRange, u8"toPrecision() argument must be between 1 and 21") \
  T(ToRadixFormatRange, u8"toString() radix argument must be between 2 and 36")  \
  T(TypedArraySetNegativeOffset, u8"Start offset is negative")                   \
  T(TypedArraySetSourceTooLarge, u8"Source is too large")                        \
  T(UnsupportedTimeZone, u8"Unsupported time zone specified %")                  \
  T(ValueOutOfRange, u8"Value % out of range for % options property %")          \
  /* SyntaxError */                                                            \
  T(BadGetterArity, u8"Getter must not have any formal parameters.")             \
  T(BadSetterArity, u8"Setter must have exactly one formal parameter.")          \
  T(ConstructorIsAccessor, u8"Class constructor may not be an accessor")         \
  T(ConstructorIsGenerator, u8"Class constructor may not be a generator")        \
  T(DerivedConstructorReturn,                                                  \
    u8"Derived constructors may only return object or undefined")                \
  T(DuplicateConstructor, u8"A class may only have one constructor")             \
  T(DuplicateExport, u8"Duplicate export of '%'")                                \
  T(DuplicateProto,                                                            \
    u8"Duplicate __proto__ fields are not allowed in object literals")           \
  T(ForInOfLoopInitializer,                                                    \
    u8"% loop variable declaration may not have an initializer.")                \
  T(ForInOfLoopMultiBindings,                                                  \
    u8"Invalid left-hand side in % loop: Must have a single binding.")           \
  T(IllegalBreak, u8"Illegal break statement")                                   \
  T(IllegalContinue, u8"Illegal continue statement")                             \
  T(IllegalLanguageModeDirective,                                              \
    u8"Illegal '%' directive in function with non-simple parameter list")        \
  T(IllegalReturn, u8"Illegal return statement")                                 \
  T(InvalidEscapedReservedWord, u8"Keyword must not contain escaped characters") \
  T(InvalidEscapedMetaProperty, u8"'%' must not contain escaped characters")     \
  T(InvalidLhsInAssignment, u8"Invalid left-hand side in assignment")            \
  T(InvalidCoverInitializedName, u8"Invalid shorthand property initializer")     \
  T(InvalidDestructuringTarget, u8"Invalid destructuring assignment target")     \
  T(InvalidLhsInFor, u8"Invalid left-hand side in for-loop")                     \
  T(InvalidLhsInPostfixOp,                                                     \
    u8"Invalid left-hand side expression in postfix operation")                  \
  T(InvalidLhsInPrefixOp,                                                      \
    u8"Invalid left-hand side expression in prefix operation")                   \
  T(InvalidRegExpFlags, u8"Invalid flags supplied to RegExp constructor '%'")    \
  T(InvalidOrUnexpectedToken, u8"Invalid or unexpected token")                   \
  T(JsonParseUnexpectedEOS, u8"Unexpected end of JSON input")                    \
  T(JsonParseUnexpectedToken, u8"Unexpected token % in JSON at position %")      \
  T(JsonParseUnexpectedTokenNumber, u8"Unexpected number in JSON at position %") \
  T(JsonParseUnexpectedTokenString, u8"Unexpected string in JSON at position %") \
  T(LabelRedeclaration, u8"Label '%' has already been declared")                 \
  T(LabelledFunctionDeclaration,                                               \
    u8"Labelled function declaration not allowed as the body of a control flow " \
    u8"structure")                                                               \
  T(MalformedArrowFunParamList, u8"Malformed arrow function parameter list")     \
  T(MalformedRegExp, u8"Invalid regular expression: /%/: %")                     \
  T(MalformedRegExpFlags, u8"Invalid regular expression flags")                  \
  T(ModuleExportUndefined, u8"Export '%' is not defined in module")              \
  T(MultipleDefaultsInSwitch,                                                  \
    u8"More than one default clause in switch statement")                        \
  T(NewlineAfterThrow, u8"Illegal newline after throw")                          \
  T(NoCatchOrFinally, u8"Missing catch or finally after try")                    \
  T(NotIsvar, u8"builtin %%IS_VAR: not a variable")                              \
  T(ParamAfterRest, u8"Rest parameter must be last formal parameter")            \
  T(InvalidRestParameter,                                                      \
    u8"Rest parameter must be an identifier or destructuring pattern")           \
  T(PushPastSafeLength,                                                        \
    u8"Pushing % elements on an array-like of length % "                         \
    u8"is disallowed, as the total surpasses 2**53-1")                           \
  T(ElementAfterRest, u8"Rest element must be last element in array")            \
  T(BadSetterRestParameter,                                                    \
    u8"Setter function argument must not be a rest parameter")                   \
  T(ParamDupe, u8"Duplicate parameter name not allowed in this context")         \
  T(ParenthesisInArgString, u8"Function arg string contains parenthesis")        \
  T(RuntimeWrongNumArgs, u8"Runtime function given wrong number of arguments")   \
  T(SingleFunctionLiteral, u8"Single function literal required")                 \
  T(SloppyFunction,                                                            \
    u8"In non-strict mode code, functions can only be declared at top level, "   \
    u8"inside a block, or as the body of an if statement.")                      \
  T(SloppyLexical,                                                             \
    u8"Block-scoped declarations (let, const, function, class) not yet "         \
    u8"supported outside strict mode")                                           \
  T(SpeciesNotConstructor,                                                     \
    u8"object.constructor[Symbol.species] is not a constructor")                 \
  T(StrictDelete, u8"Delete of an unqualified identifier in strict mode.")       \
  T(StrictEvalArguments, u8"Unexpected eval or arguments in strict mode")        \
  T(StrictFunction,                                                            \
    u8"In strict mode code, functions can only be declared at top level or "     \
    u8"inside a block.")                                                         \
  T(StrictOctalLiteral, u8"Octal literals are not allowed in strict mode.")      \
  T(StrictWith, u8"Strict mode code may not include a with statement")           \
  T(TemplateOctalLiteral,                                                      \
    u8"Octal literals are not allowed in template strings.")                     \
  T(ThisFormalParameter, u8"'this' is not a valid formal parameter name")        \
  T(TooManyArguments,                                                          \
    u8"Too many arguments in function call (only 65535 allowed)")                \
  T(TooManyParameters,                                                         \
    u8"Too many parameters in function definition (only 65535 allowed)")         \
  T(TooManyVariables, u8"Too many variables declared (only 4194303 allowed)")    \
  T(TypedArrayTooShort,                                                        \
    u8"Derived TypedArray constructor created an array which was too small")     \
  T(UnexpectedEOS, u8"Unexpected end of input")                                  \
  T(UnexpectedFunctionSent,                                                    \
    u8"function.sent expression is not allowed outside a generator")             \
  T(UnexpectedReserved, u8"Unexpected reserved word")                            \
  T(UnexpectedStrictReserved, u8"Unexpected strict mode reserved word")          \
  T(UnexpectedSuper, u8"'super' keyword unexpected here")                        \
  T(UnexpectedNewTarget, u8"new.target expression is not allowed here")          \
  T(UnexpectedTemplateString, u8"Unexpected template string")                    \
  T(UnexpectedToken, u8"Unexpected token %")                                     \
  T(UnexpectedTokenIdentifier, u8"Unexpected identifier")                        \
  T(UnexpectedTokenNumber, u8"Unexpected number")                                \
  T(UnexpectedTokenString, u8"Unexpected string")                                \
  T(UnexpectedTokenRegExp, u8"Unexpected regular expression")                    \
  T(UnknownLabel, u8"Undefined label '%'")                                       \
  T(UnterminatedArgList, u8"missing ) after argument list")                      \
  T(UnterminatedRegExp, u8"Invalid regular expression: missing /")               \
  T(UnterminatedTemplate, u8"Unterminated template literal")                     \
  T(UnterminatedTemplateExpr, u8"Missing } in template expression")              \
  T(FoundNonCallableHasInstance, u8"Found non-callable @@hasInstance")           \
  T(InvalidHexEscapeSequence, u8"Invalid hexadecimal escape sequence")           \
  T(InvalidUnicodeEscapeSequence, u8"Invalid Unicode escape sequence")           \
  T(UndefinedUnicodeCodePoint, u8"Undefined Unicode code-point")                 \
  T(YieldInParameter, u8"Yield expression not allowed in formal parameter")      \
  /* EvalError */                                                              \
  T(CodeGenFromStrings, u8"%")                                                   \
  /* URIError */                                                               \
  T(URIMalformed, u8"URI malformed")

class MessageTemplate {
 public:
  enum Template {
#define TEMPLATE(NAME, STRING) k##NAME,
    MESSAGE_TEMPLATES(TEMPLATE)
#undef TEMPLATE
        kLastMessage
  };

  static const char* TemplateString(int template_index);

  static MaybeHandle<String> FormatMessage(int template_index,
                                           Handle<String> arg0,
                                           Handle<String> arg1,
                                           Handle<String> arg2);

  static Handle<String> FormatMessage(Isolate* isolate, int template_index,
                                      Handle<Object> arg);
};


// A message handler is a convenience interface for accessing the list
// of message listeners registered in an environment
class MessageHandler {
 public:
  // Returns a message object for the API to use.
  static Handle<JSMessageObject> MakeMessageObject(
      Isolate* isolate, MessageTemplate::Template type,
      MessageLocation* location, Handle<Object> argument,
      Handle<JSArray> stack_frames);

  // Report a formatted message (needs JS allocation).
  static void ReportMessage(Isolate* isolate, MessageLocation* loc,
                            Handle<JSMessageObject> message);

  static void DefaultMessageReport(Isolate* isolate, const MessageLocation* loc,
                                   Handle<Object> message_obj);
  static Handle<String> GetMessage(Isolate* isolate, Handle<Object> data);
  static base::SmartArrayPointer<char> GetLocalizedMessage(Isolate* isolate,
                                                           Handle<Object> data);
};


}  // namespace internal
}  // namespace v8

#endif  // V8_MESSAGES_H_
