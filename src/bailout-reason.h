// Copyright 2014 the V8 project authors. All rights reserved.
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.

#ifndef V8_BAILOUT_REASON_H_
#define V8_BAILOUT_REASON_H_

namespace v8 {
namespace internal {

// TODO(svenpanne) introduce an AbortReason and partition this list
#define ERROR_MESSAGES_LIST(V)                                                 \
  V(kNoReason, u8"no reason")                                                    \
                                                                               \
  V(k32BitValueInRegisterIsNotZeroExtended,                                    \
    u8"32 bit value in register is not zero-extended")                           \
  V(kAllocationIsNotDoubleAligned, u8"Allocation is not double aligned")         \
  V(kAPICallReturnedInvalidObject, u8"API call returned invalid object")         \
  V(kArgumentsObjectValueInATestContext,                                       \
    u8"Arguments object value in a test context")                                \
  V(kArrayBoilerplateCreationFailed, u8"Array boilerplate creation failed")      \
  V(kArrayIndexConstantValueTooBig, u8"Array index constant value too big")      \
  V(kAssignmentToArguments, u8"Assignment to arguments")                         \
  V(kAssignmentToLetVariableBeforeInitialization,                              \
    u8"Assignment to let variable before initialization")                        \
  V(kAssignmentToLOOKUPVariable, u8"Assignment to LOOKUP variable")              \
  V(kAssignmentToParameterFunctionUsesArgumentsObject,                         \
    u8"Assignment to parameter, function uses arguments object")                 \
  V(kAssignmentToParameterInArgumentsObject,                                   \
    u8"Assignment to parameter in arguments object")                             \
  V(kBadValueContextForArgumentsObjectValue,                                   \
    u8"Bad value context for arguments object value")                            \
  V(kBadValueContextForArgumentsValue,                                         \
    u8"Bad value context for arguments value")                                   \
  V(kBailedOutDueToDependencyChange, u8"Bailed out due to dependency change")    \
  V(kBailoutWasNotPrepared, u8"Bailout was not prepared")                        \
  V(kBothRegistersWereSmisInSelectNonSmi,                                      \
    u8"Both registers were smis in SelectNonSmi")                                \
  V(kClassLiteral, u8"Class literal")                                            \
  V(kCodeGenerationFailed, u8"Code generation failed")                           \
  V(kCodeObjectNotProperlyPatched, u8"Code object not properly patched")         \
  V(kCompoundAssignmentToLookupSlot, u8"Compound assignment to lookup slot")     \
  V(kComputedPropertyName, u8"Computed property name")                           \
  V(kContextAllocatedArguments, u8"Context-allocated arguments")                 \
  V(kCopyBuffersOverlap, u8"Copy buffers overlap")                               \
  V(kCouldNotGenerateZero, u8"Could not generate +0.0")                          \
  V(kCouldNotGenerateNegativeZero, u8"Could not generate -0.0")                  \
  V(kDebuggerStatement, u8"DebuggerStatement")                                   \
  V(kDeclarationInCatchContext, u8"Declaration in catch context")                \
  V(kDeclarationInWithContext, u8"Declaration in with context")                  \
  V(kDefaultNaNModeNotSet, u8"Default NaN mode not set")                         \
  V(kDeleteWithGlobalVariable, u8"Delete with global variable")                  \
  V(kDeleteWithNonGlobalVariable, u8"Delete with non-global variable")           \
  V(kDestinationOfCopyNotAligned, u8"Destination of copy not aligned")           \
  V(kDontDeleteCellsCannotContainTheHole,                                      \
    u8"DontDelete cells can't contain the hole")                                 \
  V(kDoExpressionUnmodelable,                                                  \
    u8"Encountered a do-expression with unmodelable control statements")         \
  V(kDoPushArgumentNotImplementedForDoubleType,                                \
    u8"DoPushArgument not implemented for double type")                          \
  V(kEliminatedBoundsCheckFailed, u8"Eliminated bounds check failed")            \
  V(kEmitLoadRegisterUnsupportedDoubleImmediate,                               \
    u8"EmitLoadRegister: Unsupported double immediate")                          \
  V(kEval, u8"eval")                                                             \
  V(kExpectedAlignmentMarker, u8"Expected alignment marker")                     \
  V(kExpectedAllocationSite, u8"Expected allocation site")                       \
  V(kExpectedFunctionObject, u8"Expected function object in register")           \
  V(kExpectedHeapNumber, u8"Expected HeapNumber")                                \
  V(kExpectedNativeContext, u8"Expected native context")                         \
  V(kExpectedNonIdenticalObjects, u8"Expected non-identical objects")            \
  V(kExpectedNonNullContext, u8"Expected non-null context")                      \
  V(kExpectedPositiveZero, u8"Expected +0.0")                                    \
  V(kExpectedNewSpaceObject, u8"Expected new space object")                      \
  V(kExpectedUndefinedOrCell, u8"Expected undefined or cell in register")        \
  V(kExpectingAlignmentForCopyBytes, u8"Expecting alignment for CopyBytes")      \
  V(kExportDeclaration, u8"Export declaration")                                  \
  V(kExternalStringExpectedButNotFound,                                        \
    u8"External string expected, but not found")                                 \
  V(kForInStatementWithNonLocalEachVariable,                                   \
    u8"ForInStatement with non-local each variable")                             \
  V(kForOfStatement, u8"ForOfStatement")                                         \
  V(kFrameIsExpectedToBeAligned, u8"Frame is expected to be aligned")            \
  V(kFunctionBeingDebugged, u8"Function is being debugged")                      \
  V(kFunctionCallsEval, u8"Function calls eval")                                 \
  V(kFunctionDataShouldBeBytecodeArrayOnInterpreterEntry,                      \
    u8"The function_data field should be a BytecodeArray on interpreter entry")  \
  V(kGeneratedCodeIsTooLarge, u8"Generated code is too large")                   \
  V(kGeneratorFailedToResume, u8"Generator failed to resume")                    \
  V(kGeneratorResumeMethod, u8"Generator resume method is being called")         \
  V(kGenerator, u8"Generator")                                                   \
  V(kGlobalFunctionsMustHaveInitialMap,                                        \
    u8"Global functions must have initial map")                                  \
  V(kHeapNumberMapRegisterClobbered, u8"HeapNumberMap register clobbered")       \
  V(kHydrogenFilter, u8"Optimization disabled by filter")                        \
  V(kImportDeclaration, u8"Import declaration")                                  \
  V(kIndexIsNegative, u8"Index is negative")                                     \
  V(kIndexIsTooLarge, u8"Index is too large")                                    \
  V(kInliningBailedOut, u8"Inlining bailed out")                                 \
  V(kInputGPRIsExpectedToHaveUpper32Cleared,                                   \
    u8"Input GPR is expected to have upper32 cleared")                           \
  V(kInputStringTooLong, u8"Input string too long")                              \
  V(kInteger32ToSmiFieldWritingToNonSmiLocation,                               \
    u8"Integer32ToSmiField writing to non-smi location")                         \
  V(kInvalidBytecode, u8"Invalid bytecode")                                      \
  V(kInvalidCaptureReferenced, u8"Invalid capture referenced")                   \
  V(kInvalidElementsKindForInternalArrayOrInternalPackedArray,                 \
    u8"Invalid ElementsKind for InternalArray or InternalPackedArray")           \
  V(kInvalidFullCodegenState, u8"invalid full-codegen state")                    \
  V(kInvalidHandleScopeLevel, u8"Invalid HandleScope level")                     \
  V(kInvalidLeftHandSideInAssignment, u8"Invalid left-hand side in assignment")  \
  V(kInvalidLhsInCompoundAssignment, u8"Invalid lhs in compound assignment")     \
  V(kInvalidLhsInCountOperation, u8"Invalid lhs in count operation")             \
  V(kInvalidMinLength, u8"Invalid min_length")                                   \
  V(kJSGlobalObjectNativeContextShouldBeANativeContext,                        \
    u8"JSGlobalObject::native_context should be a native context")               \
  V(kJSGlobalProxyContextShouldNotBeNull,                                      \
    u8"JSGlobalProxy::context() should not be null")                             \
  V(kJSObjectWithFastElementsMapHasSlowElements,                               \
    u8"JSObject with fast elements map has slow elements")                       \
  V(kLetBindingReInitialization, u8"Let binding re-initialization")              \
  V(kLiveBytesCountOverflowChunkSize, u8"Live Bytes Count overflow chunk size")  \
  V(kLiveEdit, u8"LiveEdit")                                                     \
  V(kLookupVariableInCountOperation, u8"Lookup variable in count operation")     \
  V(kMapBecameDeprecated, u8"Map became deprecated")                             \
  V(kMapBecameUnstable, u8"Map became unstable")                                 \
  V(kNativeFunctionLiteral, u8"Native function literal")                         \
  V(kNeedSmiLiteral, u8"Need a Smi literal here")                                \
  V(kNoCasesLeft, u8"No cases left")                                             \
  V(kNonInitializerAssignmentToConst, u8"Non-initializer assignment to const")   \
  V(kNonSmiIndex, u8"Non-smi index")                                             \
  V(kNonSmiKeyInArrayLiteral, u8"Non-smi key in array literal")                  \
  V(kNonSmiValue, u8"Non-smi value")                                             \
  V(kNonObject, u8"Non-object value")                                            \
  V(kNotEnoughVirtualRegistersForValues,                                       \
    u8"Not enough virtual registers for values")                                 \
  V(kNotEnoughSpillSlotsForOsr, u8"Not enough spill slots for OSR")              \
  V(kNotEnoughVirtualRegistersRegalloc,                                        \
    u8"Not enough virtual registers (regalloc)")                                 \
  V(kObjectFoundInSmiOnlyArray, u8"Object found in smi-only array")              \
  V(kObjectLiteralWithComplexProperty, u8"Object literal with complex property") \
  V(kOffsetOutOfRange, u8"Offset out of range")                                  \
  V(kOperandIsANumber, u8"Operand is a number")                                  \
  V(kOperandIsASmiAndNotABoundFunction,                                        \
    u8"Operand is a smi and not a bound function")                               \
  V(kOperandIsASmiAndNotAFunction, u8"Operand is a smi and not a function")      \
  V(kOperandIsASmiAndNotAName, u8"Operand is a smi and not a name")              \
  V(kOperandIsASmiAndNotAReceiver, u8"Operand is a smi and not a receiver")      \
  V(kOperandIsASmiAndNotAString, u8"Operand is a smi and not a string")          \
  V(kOperandIsASmi, u8"Operand is a smi")                                        \
  V(kOperandIsNotADate, u8"Operand is not a date")                               \
  V(kOperandIsNotABoundFunction, u8"Operand is not a bound function")            \
  V(kOperandIsNotAFunction, u8"Operand is not a function")                       \
  V(kOperandIsNotAName, u8"Operand is not a name")                               \
  V(kOperandIsNotANumber, u8"Operand is not a number")                           \
  V(kOperandIsNotAReceiver, u8"Operand is not a receiver")                       \
  V(kOperandIsNotASmi, u8"Operand is not a smi")                                 \
  V(kOperandIsNotAString, u8"Operand is not a string")                           \
  V(kOperandIsNotSmi, u8"Operand is not smi")                                    \
  V(kOperandNotANumber, u8"Operand not a number")                                \
  V(kObjectTagged, u8"The object is tagged")                                     \
  V(kObjectNotTagged, u8"The object is not tagged")                              \
  V(kOptimizationDisabled, u8"Optimization is disabled")                         \
  V(kOptimizedTooManyTimes, u8"Optimized too many times")                        \
  V(kOutOfVirtualRegistersWhileTryingToAllocateTempRegister,                   \
    u8"Out of virtual registers while trying to allocate temp register")         \
  V(kParseScopeError, u8"Parse/scope error")                                     \
  V(kPossibleDirectCallToEval, u8"Possible direct call to eval")                 \
  V(kReceivedInvalidReturnAddress, u8"Received invalid return address")          \
  V(kReferenceToAVariableWhichRequiresDynamicLookup,                           \
    u8"Reference to a variable which requires dynamic lookup")                   \
  V(kReferenceToGlobalLexicalVariable, u8"Reference to global lexical variable") \
  V(kReferenceToUninitializedVariable, u8"Reference to uninitialized variable")  \
  V(kRegisterDidNotMatchExpectedRoot, u8"Register did not match expected root")  \
  V(kRegisterWasClobbered, u8"Register was clobbered")                           \
  V(kRememberedSetPointerInNewSpace, u8"Remembered set pointer is in new space") \
  V(kRestParameter, u8"Rest parameters")                                         \
  V(kReturnAddressNotFoundInFrame, u8"Return address not found in frame")        \
  V(kSloppyFunctionExpectsJSReceiverReceiver,                                  \
    u8"Sloppy function expects JSReceiver as receiver.")                         \
  V(kSmiAdditionOverflow, u8"Smi addition overflow")                             \
  V(kSmiSubtractionOverflow, u8"Smi subtraction overflow")                       \
  V(kStackAccessBelowStackPointer, u8"Stack access below stack pointer")         \
  V(kStackFrameTypesMustMatch, u8"Stack frame types must match")                 \
  V(kSuperReference, u8"Super reference")                                        \
  V(kTailCall, u8"Tail call")                                                    \
  V(kTheCurrentStackPointerIsBelowCsp,                                         \
    u8"The current stack pointer is below csp")                                  \
  V(kTheSourceAndDestinationAreTheSame,                                        \
    u8"The source and destination are the same")                                 \
  V(kTheStackWasCorruptedByMacroAssemblerCall,                                 \
    u8"The stack was corrupted by MacroAssembler::Call()")                       \
  V(kTooManyParametersLocals, u8"Too many parameters/locals")                    \
  V(kTooManyParameters, u8"Too many parameters")                                 \
  V(kTooManySpillSlotsNeededForOSR, u8"Too many spill slots needed for OSR")     \
  V(kToOperand32UnsupportedImmediate, u8"ToOperand32 unsupported immediate.")    \
  V(kToOperandIsDoubleRegisterUnimplemented,                                   \
    u8"ToOperand IsDoubleRegister unimplemented")                                \
  V(kToOperandUnsupportedDoubleImmediate,                                      \
    u8"ToOperand Unsupported double immediate")                                  \
  V(kTryCatchStatement, u8"TryCatchStatement")                                   \
  V(kTryFinallyStatement, u8"TryFinallyStatement")                               \
  V(kUnalignedAllocationInNewSpace, u8"Unaligned allocation in new space")       \
  V(kUnalignedCellInWriteBarrier, u8"Unaligned cell in write barrier")           \
  V(kUnexpectedAllocationTop, u8"Unexpected allocation top")                     \
  V(kUnexpectedColorFound, u8"Unexpected color bit pattern found")               \
  V(kUnexpectedElementsKindInArrayConstructor,                                 \
    u8"Unexpected ElementsKind in array constructor")                            \
  V(kUnexpectedFallthroughFromCharCodeAtSlowCase,                              \
    u8"Unexpected fallthrough from CharCodeAt slow case")                        \
  V(kUnexpectedFallthroughFromCharFromCodeSlowCase,                            \
    u8"Unexpected fallthrough from CharFromCode slow case")                      \
  V(kUnexpectedFallThroughFromStringComparison,                                \
    u8"Unexpected fall-through from string comparison")                          \
  V(kUnexpectedFallthroughToCharCodeAtSlowCase,                                \
    u8"Unexpected fallthrough to CharCodeAt slow case")                          \
  V(kUnexpectedFallthroughToCharFromCodeSlowCase,                              \
    u8"Unexpected fallthrough to CharFromCode slow case")                        \
  V(kUnexpectedFPUStackDepthAfterInstruction,                                  \
    u8"Unexpected FPU stack depth after instruction")                            \
  V(kUnexpectedInitialMapForArrayFunction1,                                    \
    u8"Unexpected initial map for Array function (1)")                           \
  V(kUnexpectedInitialMapForArrayFunction2,                                    \
    u8"Unexpected initial map for Array function (2)")                           \
  V(kUnexpectedInitialMapForArrayFunction,                                     \
    u8"Unexpected initial map for Array function")                               \
  V(kUnexpectedInitialMapForInternalArrayFunction,                             \
    u8"Unexpected initial map for InternalArray function")                       \
  V(kUnexpectedLevelAfterReturnFromApiCall,                                    \
    u8"Unexpected level after return from api call")                             \
  V(kUnexpectedNegativeValue, u8"Unexpected negative value")                     \
  V(kUnexpectedNumberOfPreAllocatedPropertyFields,                             \
    u8"Unexpected number of pre-allocated property fields")                      \
  V(kUnexpectedFunctionIDForInvokeIntrinsic,                                   \
    u8"Unexpected runtime function id for the InvokeIntrinsic bytecode")         \
  V(kUnexpectedFPCRMode, u8"Unexpected FPCR mode.")                              \
  V(kUnexpectedSmi, u8"Unexpected smi value")                                    \
  V(kUnexpectedStackDepth, u8"Unexpected operand stack depth in full-codegen")   \
  V(kUnexpectedStackPointer, u8"The stack pointer is not the expected value")    \
  V(kUnexpectedStringType, u8"Unexpected string type")                           \
  V(kUnexpectedTypeForRegExpDataFixedArrayExpected,                            \
    u8"Unexpected type for RegExp data, FixedArray expected")                    \
  V(kUnexpectedValue, u8"Unexpected value")                                      \
  V(kUnsupportedConstCompoundAssignment,                                       \
    u8"Unsupported const compound assignment")                                   \
  V(kUnsupportedCountOperationWithConst,                                       \
    u8"Unsupported count operation with const")                                  \
  V(kUnsupportedDoubleImmediate, u8"Unsupported double immediate")               \
  V(kUnsupportedLetCompoundAssignment, u8"Unsupported let compound assignment")  \
  V(kUnsupportedLookupSlotInDeclaration,                                       \
    u8"Unsupported lookup slot in declaration")                                  \
  V(kUnsupportedNonPrimitiveCompare, u8"Unsupported non-primitive compare")      \
  V(kUnsupportedPhiUseOfArguments, u8"Unsupported phi use of arguments")         \
  V(kUnsupportedPhiUseOfConstVariable,                                         \
    u8"Unsupported phi use of const or let variable")                            \
  V(kUnexpectedReturnFromBytecodeHandler,                                      \
    u8"Unexpectedly returned from a bytecode handler")                           \
  V(kUnexpectedReturnFromThrow, u8"Unexpectedly returned from a throw")          \
  V(kUnsupportedSwitchStatement, u8"Unsupported switch statement")               \
  V(kUnsupportedTaggedImmediate, u8"Unsupported tagged immediate")               \
  V(kVariableResolvedToWithContext, u8"Variable resolved to with context")       \
  V(kWeShouldNotHaveAnEmptyLexicalContext,                                     \
    u8"We should not have an empty lexical context")                             \
  V(kWithStatement, u8"WithStatement")                                           \
  V(kWrongFunctionContext, u8"Wrong context passed to function")                 \
  V(kWrongAddressOrValuePassedToRecordWrite,                                   \
    u8"Wrong address or value passed to RecordWrite")                            \
  V(kWrongArgumentCountForInvokeIntrinsic,                                     \
    u8"Wrong number of arguments for intrinsic")                                 \
  V(kShouldNotDirectlyEnterOsrFunction,                                        \
    u8"Should not directly enter OSR-compiled function")                         \
  V(kYield, u8"Yield")

#define ERROR_MESSAGES_CONSTANTS(C, T) C,
enum BailoutReason {
  ERROR_MESSAGES_LIST(ERROR_MESSAGES_CONSTANTS) kLastErrorMessage
};
#undef ERROR_MESSAGES_CONSTANTS


const char* GetBailoutReason(BailoutReason reason);

}  // namespace internal
}  // namespace v8

#endif  // V8_BAILOUT_REASON_H_
