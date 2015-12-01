// Copyright 2015 the V8 project authors. All rights reserved.
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.

#if V8_TARGET_ARCH_S390

#include "src/codegen.h"
#include "src/debug/debug.h"
#include "src/deoptimizer.h"
#include "src/full-codegen/full-codegen.h"
#include "src/runtime/runtime.h"

namespace v8 {
namespace internal {


#define __ ACCESS_MASM(masm)


void Builtins::Generate_Adaptor(MacroAssembler* masm, CFunctionId id,
                                BuiltinExtraArguments extra_args) {
  // ----------- S t a t e -------------
  //  -- r2                 : number of arguments excluding receiver
  //                          (only guaranteed when the called function
  //                           is not marked as DontAdaptArguments)
  //  -- r3                 : called function
  //  -- sp[0]              : last argument
  //  -- ...
  //  -- sp[4 * (argc - 1)] : first argument
  //  -- sp[4 * argc]       : receiver
  // -----------------------------------
  __ AssertFunction(r3);

  // Make sure we operate in the context of the called function (for example
  // ConstructStubs implemented in C++ will be run in the context of the caller
  // instead of the callee, due to the way that [[Construct]] is defined for
  // ordinary functions).
  // TODO(bmeurer): Can we make this more robust?
  __ LoadP(cp, FieldMemOperand(r3, JSFunction::kContextOffset));

  // Insert extra arguments.
  int num_extra_args = 0;
  if (extra_args == NEEDS_CALLED_FUNCTION) {
    num_extra_args = 1;
    __ push(r3);
  } else {
    DCHECK(extra_args == NO_EXTRA_ARGUMENTS);
  }

  // JumpToExternalReference expects r2 to contain the number of arguments
  // including the receiver and the extra arguments.  But r2 is only valid
  // if the called function is marked as DontAdaptArguments, otherwise we
  // need to load the argument count from the SharedFunctionInfo.
  __ LoadP(r4, FieldMemOperand(r3, JSFunction::kSharedFunctionInfoOffset));
  __ LoadW(
      r4, FieldMemOperand(r4, SharedFunctionInfo::kFormalParameterCountOffset));
#if !V8_TARGET_ARCH_S390X
  __ SmiUntag(r4);
#endif
  __ CmpP(r4, Operand(SharedFunctionInfo::kDontAdaptArgumentsSentinel));
  Label skip;
  __ beq(&skip);
  __ LoadRR(r2, r4);
  __ bind(&skip);
  __ AddP(r2, r2, Operand(num_extra_args + 1));

  __ JumpToExternalReference(ExternalReference(id, masm->isolate()));
}


// Load the built-in InternalArray function from the current context.
static void GenerateLoadInternalArrayFunction(MacroAssembler* masm,
                                              Register result) {
  // Load the native context.

  __ LoadP(result,
           MemOperand(cp, Context::SlotOffset(Context::GLOBAL_OBJECT_INDEX)));
  __ LoadP(result,
           FieldMemOperand(result, JSGlobalObject::kNativeContextOffset));
  // Load the InternalArray function from the native context.
  __ LoadP(result,
           MemOperand(result, Context::SlotOffset(
                                  Context::INTERNAL_ARRAY_FUNCTION_INDEX)));
}


// Load the built-in Array function from the current context.
static void GenerateLoadArrayFunction(MacroAssembler* masm, Register result) {
  // Load the native context.

  __ LoadP(result,
           MemOperand(cp, Context::SlotOffset(Context::GLOBAL_OBJECT_INDEX)));
  __ LoadP(result,
           FieldMemOperand(result, JSGlobalObject::kNativeContextOffset));
  // Load the Array function from the native context.
  __ LoadP(
      result,
      MemOperand(result, Context::SlotOffset(Context::ARRAY_FUNCTION_INDEX)));
}


void Builtins::Generate_InternalArrayCode(MacroAssembler* masm) {
  // ----------- S t a t e -------------
  //  -- r2     : number of arguments
  //  -- lr     : return address
  //  -- sp[...]: constructor arguments
  // -----------------------------------
  Label generic_array_code, one_or_more_arguments, two_or_more_arguments;

  // Get the InternalArray function.
  GenerateLoadInternalArrayFunction(masm, r3);

  if (FLAG_debug_code) {
    // Initial map for the builtin InternalArray functions should be maps.
    __ LoadP(r4,
        FieldMemOperand(r3, JSFunction::kPrototypeOrInitialMapOffset));
    __ TestIfSmi(r4);
    __ Assert(ne, kUnexpectedInitialMapForInternalArrayFunction, cr0);
    __ CompareObjectType(r4, r5, r6, MAP_TYPE);
    __ Assert(eq, kUnexpectedInitialMapForInternalArrayFunction);
  }

  // Run the native code for the InternalArray function called as a normal
  // function.
  // tail call a stub
  InternalArrayConstructorStub stub(masm->isolate());
  __ TailCallStub(&stub);
}


void Builtins::Generate_ArrayCode(MacroAssembler* masm) {
  // ----------- S t a t e -------------
  //  -- r2     : number of arguments
  //  -- lr     : return address
  //  -- sp[...]: constructor arguments
  // -----------------------------------
  Label generic_array_code, one_or_more_arguments, two_or_more_arguments;

  // Get the Array function.
  GenerateLoadArrayFunction(masm, r3);

  if (FLAG_debug_code) {
    // Initial map for the builtin Array functions should be maps.
    __ LoadP(r4,
        FieldMemOperand(r3, JSFunction::kPrototypeOrInitialMapOffset));
    __ TestIfSmi(r4);
    __ Assert(ne, kUnexpectedInitialMapForArrayFunction, cr0);
    __ CompareObjectType(r4, r5, r6, MAP_TYPE);
    __ Assert(eq, kUnexpectedInitialMapForArrayFunction);
  }

  __ LoadRR(r5, r3);
  // Run the native code for the Array function called as a normal function.
  // tail call a stub
  __ LoadRoot(r4, Heap::kUndefinedValueRootIndex);
  ArrayConstructorStub stub(masm->isolate());
  __ TailCallStub(&stub);
}


// static
void Builtins::Generate_StringConstructor(MacroAssembler* masm) {
  // ----------- S t a t e -------------
  //  -- r2                     : number of arguments
  //  -- r3                     : constructor function
  //  -- lr                     : return address
  //  -- sp[(argc - n - 1) * 4] : arg[n] (zero based)
  //  -- sp[argc * 4]           : receiver
  // -----------------------------------
  // 1. Load the first argument into r2 and get rid of the rest (including the
  // receiver).
  Label no_arguments;
  {
    __ CmpP(r2, Operand::Zero());
    __ beq(&no_arguments);
    __ SubP(r2, r2, Operand(1));
    __ ShiftLeftP(r2, r2, Operand(kPointerSizeLog2));
    __ LoadP(r2, MemOperand(sp, r2));
    __ lay(sp, MemOperand(sp, r2));
    __ Drop(2);
  }

  // 2a. At least one argument, return r2 if it's a string, otherwise
  // dispatch to appropriate conversion.
  Label to_string, symbol_descriptive_string;
  {
    __ JumpIfSmi(r2, &to_string);
    STATIC_ASSERT(FIRST_NONSTRING_TYPE == SYMBOL_TYPE);
    __ CompareObjectType(r2, r3, r3, FIRST_NONSTRING_TYPE);
    __ bgt(&to_string);
    __ beq(&symbol_descriptive_string);
    __ Ret();
  }

  // 2b. No arguments, return the empty string (and pop the receiver).
  __ bind(&no_arguments);
  {
    __ LoadRoot(r2, Heap::kempty_stringRootIndex);
    __ Ret(1);
  }

  // 3a. Convert r2 to a string.
  __ bind(&to_string);
  {
    ToStringStub stub(masm->isolate());
    __ TailCallStub(&stub);
  }
  // 3b. Convert symbol in r2 to a string.
  __ bind(&symbol_descriptive_string);
  {
    __ Push(r2);
    __ TailCallRuntime(Runtime::kSymbolDescriptiveString, 1, 1);
  }
}

// static
void Builtins::Generate_StringConstructor_ConstructStub(MacroAssembler* masm) {
  // ----------- S t a t e -------------
  //  -- r2                     : number of arguments
  //  -- r3                     : constructor function
  //  -- r5                     : new target
  //  -- lr                     : return address
  //  -- sp[(argc - n - 1) * 4] : arg[n] (zero based)
  //  -- sp[argc * 4]           : receiver
  // -----------------------------------
 
  // 1. Load the first argument into r4 and get rid of the rest (including the
  // receiver).
  {
    Label no_arguments, done;
    __ CmpP(r2, Operand::Zero());
    __ beq(&no_arguments);
    __ SubP(r2, r2, Operand(1));
    __ ShiftLeftP(r4, r2, Operand(kPointerSizeLog2));
    __ LoadP(r4, MemOperand(sp, r4));
    __ lay(sp, MemOperand(sp, r4));
    __ Drop(2);
    __ b(&done);
    __ bind(&no_arguments);
    __ LoadRoot(r4, Heap::kempty_stringRootIndex);
    __ Drop(1);
    __ bind(&done);
  }
 
  // 2. Make sure r4 is a string.
  {
    Label convert, done_convert;
    __ JumpIfSmi(r4, &convert);
    __ CompareObjectType(r4, r6, r6, FIRST_NONSTRING_TYPE);
    __ blt(&done_convert);
    __ bind(&convert);
    {
      FrameAndConstantPoolScope scope(masm, StackFrame::INTERNAL);
      ToStringStub stub(masm->isolate());
      __ Push(r3, r5);
      __ LoadRR(r2, r4);
      __ CallStub(&stub);
      __ LoadRR(r4, r2);
      __ Pop(r3, r5);
    }
    __ bind(&done_convert);
  }

  // 3. Check if new target and constructor differ.
  Label new_object;
  __ CmpP(r3, r5);
  __ bne(&new_object);

  // 4. Allocate a JSValue wrapper for the string.
  {
    // ----------- S t a t e -------------
    //  -- r4 : the first argument
    //  -- r3 : constructor function
    //  -- r5 : new target
    //  -- lr : return address
    // -----------------------------------
    __ Allocate(JSValue::kSize, r2, r6, r7, &new_object, TAG_OBJECT);

    // Initialize the JSValue in r2.
    __ LoadGlobalFunctionInitialMap(r3, r5, r6);
    __ StoreP(r5, FieldMemOperand(r2, HeapObject::kMapOffset), r0);
    __ LoadRoot(r5, Heap::kEmptyFixedArrayRootIndex);
    __ StoreP(r5, FieldMemOperand(r2, JSObject::kPropertiesOffset), r0);
    __ StoreP(r5, FieldMemOperand(r2, JSObject::kElementsOffset), r0);
    __ StoreP(r4, FieldMemOperand(r2, JSValue::kValueOffset), r0);
    STATIC_ASSERT(JSValue::kSize == 4 * kPointerSize);
    __ Ret();
  }

  // 5. Fallback to the runtime to create new object.
  __ bind(&new_object);
  {
    FrameAndConstantPoolScope scope(masm, StackFrame::INTERNAL);
    __ Push(r4, r3, r5);  // first argument, constructor, new target
    __ CallRuntime(Runtime::kNewObject, 2);
    __ Pop(r4);
  }
  __ StoreP(r4, FieldMemOperand(r2, JSValue::kValueOffset), r0);
  __ Ret();
}


static void CallRuntimePassFunction(MacroAssembler* masm,
                                    Runtime::FunctionId function_id) {
  // ----------- S t a t e -------------
  //  -- r3 : target function (preserved for callee)
  //  -- r5 : new target (preserved for callee)
  // -----------------------------------

  FrameAndConstantPoolScope scope(masm, StackFrame::INTERNAL);
  // Push a copy of the target function and the new target.
  // Push function as parameter to the runtime call.
  __ Push(r3, r5, r3);

  __ CallRuntime(function_id, 1);
  // Restore target function and new target.
  __ Pop(r3, r5);
}


static void GenerateTailCallToSharedCode(MacroAssembler* masm) {
  __ LoadP(ip, FieldMemOperand(r3, JSFunction::kSharedFunctionInfoOffset));
  __ LoadP(ip, FieldMemOperand(ip, SharedFunctionInfo::kCodeOffset));
  __ AddP(ip, Operand(Code::kHeaderSize - kHeapObjectTag));
  __ JumpToJSEntry(ip);
}


static void GenerateTailCallToReturnedCode(MacroAssembler* masm) {
  __ AddP(ip, r2, Operand(Code::kHeaderSize - kHeapObjectTag));
  __ JumpToJSEntry(ip);
}


void Builtins::Generate_InOptimizationQueue(MacroAssembler* masm) {
  // Checking whether the queued function is ready for install is optional,
  // since we come across interrupts and stack checks elsewhere.  However,
  // not checking may delay installing ready functions, and always checking
  // would be quite expensive.  A good compromise is to first check against
  // stack limit as a cue for an interrupt signal.
  Label ok;
  __ CmpLogicalP(sp, RootMemOperand(Heap::kStackLimitRootIndex));
  __ bge(&ok, Label::kNear);

  CallRuntimePassFunction(masm, Runtime::kTryInstallOptimizedCode);
  GenerateTailCallToReturnedCode(masm);

  __ bind(&ok);
  GenerateTailCallToSharedCode(masm);
}


static void Generate_JSConstructStubHelper(MacroAssembler* masm,
                                           bool is_api_function,
                                           bool create_implicit_receiver) {
  // ----------- S t a t e -------------
  //  -- r2     : number of arguments
  //  -- r3     : constructor function
  //  -- r4     : allocation site or undefined
  //  -- r5     : new target
  //  -- lr     : return address
  //  -- sp[...]: constructor arguments
  // -----------------------------------

  Isolate* isolate = masm->isolate();

  // Enter a construct frame.
  {
    FrameAndConstantPoolScope scope(masm, StackFrame::CONSTRUCT);

    // Preserve the incoming parameters on the stack.
    __ AssertUndefinedOrAllocationSite(r4, r6);

    if (!create_implicit_receiver) {
      // Push new.target onto the construct frame. This is stored just below the
      // receiver on the stack.
      __ SmiTag(r6, r2);
      __ Push(r4, r6, r5);
      __ PushRoot(Heap::kTheHoleValueRootIndex);
    } else {
      __ SmiTag(r2);
      __ Push(r4, r2);

      // Try to allocate the object without transitioning into C code. If any of
      // the preconditions is not met, the code bails out to the runtime call.
      Label rt_call, allocated;
      if (FLAG_inline_new) {
        // Verify that the new target is a JSFunction.
        __ CompareObjectType(r5, r7, r6, JS_FUNCTION_TYPE);
        __ bne(&rt_call);

        // Load the initial map and verify that it is in fact a map.
        // r5: new target
        __ LoadP(r4,
                 FieldMemOperand(r5, JSFunction::kPrototypeOrInitialMapOffset));
        __ JumpIfSmi(r4, &rt_call);
        __ CompareObjectType(r4, r7, r6, MAP_TYPE);
        __ bne(&rt_call);

        // Fall back to runtime if the expected base constructor and base
        // constructor differ.
        __ LoadP(r7, FieldMemOperand(r4, Map::kConstructorOrBackPointerOffset));
        __ CmpP(r3, r7);
        __ bne(&rt_call);

        // Check that the constructor is not constructing a JSFunction (see
        // comments in Runtime_NewObject in runtime.cc). In which case the
        // initial map's instance type would be JS_FUNCTION_TYPE.
        // r3: constructor function
        // r4: initial map
        // r5: new target
        __ CompareInstanceType(r4, r7, JS_FUNCTION_TYPE);
        __ beq(&rt_call);

        if (!is_api_function) {
          Label allocate;
          MemOperand bit_field3 = FieldMemOperand(r4, Map::kBitField3Offset);
          // Check if slack tracking is enabled.
          __ LoadlW(r6, bit_field3);
          __ DecodeField<Map::Counter>(r2, r6);
          __ CmpP(r2, Operand(Map::kSlackTrackingCounterEnd));
          __ blt(&allocate);
          // Decrease generous allocation count.
          __ AddP(r6, r6, -(1 << Map::Counter::kShift));
          __ StoreW(r6, bit_field3);
          __ CmpP(r2, Operand(Map::kSlackTrackingCounterEnd));
          __ bne(&allocate);

          // Push the constructor, new target and map to the stack, and
          // the map again as an argument to the runtime call.
          __ Push(r3, r5, r4, r4);

          __ CallRuntime(Runtime::kFinalizeInstanceSize, 1);

          __ Pop(r3, r5, r4);
          __ LoadImmP(r2, Operand(Map::kSlackTrackingCounterEnd - 1));

          __ bind(&allocate);
        }

        // Now allocate the JSObject on the heap.
        // r3: constructor function
        // r4: initial map
        // r5: new target
        // r2: slack tracking counter (non-API function case)
        __ LoadlB(r9, FieldMemOperand(r4, Map::kInstanceSizeOffset));

        __ Allocate(r9, r6, r9, r8, &rt_call, SIZE_IN_WORDS);

        // Allocated the JSObject, now initialize the fields. Map is set to
        // initial map and properties and elements are set to empty fixed array.
        // r3: constructor function
        // r4: initial map
        // r5: new target
        // r6: JSObject (not tagged)
        // r9: start of next object
        // r2: slack tracking counter (non-API function case)
        __ LoadRoot(r8, Heap::kEmptyFixedArrayRootIndex);
        __ StoreP(r4, MemOperand(r6, JSObject::kMapOffset));
        __ StoreP(r8, MemOperand(r6, JSObject::kPropertiesOffset));
        __ StoreP(r8, MemOperand(r6, JSObject::kElementsOffset));
        __ AddP(r7, r6, Operand(JSObject::kElementsOffset + kPointerSize));

        // Fill all the in-object properties with the appropriate filler.
        // r7: First in-object property of JSObject (not tagged)
        __ LoadRoot(r8, Heap::kUndefinedValueRootIndex);

        if (!is_api_function) {
          Label no_inobject_slack_tracking;

          // Check if slack tracking is enabled.
          __ CmpP(r2, Operand(Map::kSlackTrackingCounterEnd));
          __ blt(&no_inobject_slack_tracking);

          // Allocate object with a slack.
          __ LoadlB(r2, FieldMemOperand(r4, Map::kUnusedPropertyFieldsOffset));
          __ ShiftLeftP(r2, r2, Operand(kPointerSizeLog2));
          __ SubP(r2, r9, r2);
          // r2: offset of first field after pre-allocated fields
          if (FLAG_debug_code) {
            __ CmpP(r7, r2);
            __ Assert(le, kUnexpectedNumberOfPreAllocatedPropertyFields);
          }
          __ InitializeFieldsWithFiller(r7, r2, r8);

          // To allow truncation fill the remaining fields with one pointer
          // filler map.
          __ LoadRoot(r8, Heap::kOnePointerFillerMapRootIndex);

          __ bind(&no_inobject_slack_tracking);
        }

        __ InitializeFieldsWithFiller(r7, r9, r8);

        // Add the object tag to make the JSObject real, so that we can continue
        // and jump into the continuation code at any time from now on.
        __ AddP(r6, r6, Operand(kHeapObjectTag));

        // Continue with JSObject being successfully allocated
        // r3: constructor function
        // r5: new target
        // r6: JSObject
        __ b(&allocated);
      }

      // Allocate the new receiver object using the runtime call.
      // r3: constructor function
      // r5: new target
      __ bind(&rt_call);

      // Push the constructor and new_target twice, second pair as arguments
      // to the runtime call.
      __ Push(r3, r5, r3, r5);
      __ CallRuntime(Runtime::kNewObject, 2);
      __ LoadRR(r6, r2);
      __ Pop(r3, r5);

      // Receiver for constructor call allocated.
      // r3: constructor function
      // r5: new target
      // r6: JSObject
      __ bind(&allocated);

      // Retrieve smi-tagged arguments count from the stack.
      __ LoadP(r2, MemOperand(sp));
      __ SmiUntag(r2);

      // Push new.target onto the construct frame. This is stored just below the
      // receiver on the stack.
      // Push the allocated receiver to the stack. We need two copies
      // because we may have to return the original one and the calling
      // conventions dictate that the called function pops the receiver.
      __ Push(r5, r6, r6);
    }

    // Set up pointer to last argument.
    __ la(r4, MemOperand(fp, StandardFrameConstants::kCallerSPOffset));

    // Copy arguments and receiver to the expression stack.
    // r2: number of arguments
    // r3: constructor function
    // r4: address of last argument (caller sp)
    // r5: new target
    // cr0: condition indicating whether r2 is zero
    // sp[0]: receiver
    // sp[1]: receiver
    // sp[2]: new.target
    // sp[3]: number of arguments (smi-tagged)
    Label loop, no_args;
    __ beq(&no_args, cr0);
    __ ShiftLeftP(ip, r2, Operand(kPointerSizeLog2));
    __ SubP(sp, sp, ip);
    __ bind(&loop);
    __ lay(ip, MemOperand(ip, -kPointerSize));
    __ LoadP(r0, MemOperand(ip, r4));
    __ StoreP(r0, MemOperand(ip, sp));
    __ BranchOnCount(r2, &loop);
    __ SmiUntag(r2, r5);
    __ bind(&no_args);

    // Call the function.
    // r2: number of arguments
    // r3: constructor function
    // r5: new target
    if (is_api_function) {
      __ LoadP(cp, FieldMemOperand(r3, JSFunction::kContextOffset));
      Handle<Code> code = masm->isolate()->builtins()->HandleApiCallConstruct();
      __ Call(code, RelocInfo::CODE_TARGET);
    } else {
      ParameterCount actual(r2);
      __ InvokeFunction(r3, r5, actual, CALL_FUNCTION, NullCallWrapper());
    }

    // Store offset of return address for deoptimizer.
    if (create_implicit_receiver && !is_api_function) {
      masm->isolate()->heap()->SetConstructStubDeoptPCOffset(masm->pc_offset());
    }

    // Restore context from the frame.
    // r2: result
    // sp[0]: receiver
    // sp[1]: new.target
    // sp[2]: number of arguments (smi-tagged)
    __ LoadP(cp, MemOperand(fp, StandardFrameConstants::kContextOffset));

    if (create_implicit_receiver) {
      // If the result is an object (in the ECMA sense), we should get rid
      // of the receiver and use the result; see ECMA-262 section 13.2.2-7
      // on page 74.
      Label use_receiver, exit;

      // If the result is a smi, it is *not* an object in the ECMA sense.
      // r2: result
      // sp[0]: receiver
      // sp[1]: new.target
      // sp[2]: number of arguments (smi-tagged)
      __ JumpIfSmi(r2, &use_receiver);

      // If the type of the result (stored in its map) is less than
      // FIRST_SPEC_OBJECT_TYPE, it is not an object in the ECMA sense.
      __ CompareObjectType(r2, r3, r5, FIRST_SPEC_OBJECT_TYPE);
      __ bge(&exit);

      // Throw away the result of the constructor invocation and use the
      // on-stack receiver as the result.
      __ bind(&use_receiver);
      __ LoadP(r2, MemOperand(sp));

      // Remove receiver from the stack, remove caller arguments, and
      // return.
      __ bind(&exit);
      // r2: result
      // sp[0]: receiver (newly allocated object)
      // sp[1]: new.target (new target)
      // sp[2]: number of arguments (smi-tagged)
      __ LoadP(r3, MemOperand(sp, 2 * kPointerSize));
    } else {
      __ LoadP(r3, MemOperand(sp, kPointerSize));
    }

    // Leave construct frame.
  }

  __ SmiToPtrArrayOffset(r3, r3);
  __ AddP(sp, sp, r3);
  __ AddP(sp, sp, Operand(kPointerSize));
  if (create_implicit_receiver) {
    __ IncrementCounter(isolate->counters()->constructed_objects(), 1, r3, r4);
  }
  __ Ret();
}


void Builtins::Generate_JSConstructStubGeneric(MacroAssembler* masm) {
  Generate_JSConstructStubHelper(masm, false, true);
}


void Builtins::Generate_JSConstructStubApi(MacroAssembler* masm) {
  Generate_JSConstructStubHelper(masm, true, true);
}


void Builtins::Generate_JSBuiltinsConstructStub(MacroAssembler* masm) {
  Generate_JSConstructStubHelper(masm, false, false);
}


void Builtins::Generate_ConstructedNonConstructable(MacroAssembler* masm) {
  FrameAndConstantPoolScope scope(masm, StackFrame::INTERNAL);
  __ push(r3);
  __ CallRuntime(Runtime::kThrowConstructedNonConstructable, 1);
}


enum IsTagged { kArgcIsSmiTagged, kArgcIsUntaggedInt };


// Clobbers r4; preserves all other registers.
static void Generate_CheckStackOverflow(MacroAssembler* masm, Register argc,
                                        IsTagged argc_is_tagged) {
  // Check the stack for overflow. We are not trying to catch
  // interruptions (e.g. debug break and preemption) here, so the "real stack
  // limit" is checked.
  Label okay;
  __ LoadRoot(r4, Heap::kRealStackLimitRootIndex);
  // Make r4 the space we have left. The stack might already be overflowed
  // here which will cause r4 to become negative.
  __ SubP(r4, sp, r4);
  // Check if the arguments will overflow the stack.
  if (argc_is_tagged == kArgcIsSmiTagged) {
    __ SmiToPtrArrayOffset(r0, argc);
  } else {
    DCHECK(argc_is_tagged == kArgcIsUntaggedInt);
    __ ShiftLeftP(r0, argc, Operand(kPointerSizeLog2));
  }
  __ CmpP(r4, r0);
  __ bgt(&okay);  // Signed comparison.

  // Out of stack space.
  __ CallRuntime(Runtime::kThrowStackOverflow, 0);

  __ bind(&okay);
}


static void Generate_JSEntryTrampolineHelper(MacroAssembler* masm,
                                             bool is_construct) {
  // Called from Generate_JS_Entry
  // r2: new.target
  // r3: function
  // r4: receiver
  // r5: argc
  // r6: argv
  // r0,r7-r9, cp may be clobbered
  ProfileEntryHookStub::MaybeCallEntryHook(masm);

  // Clear the context before we push it when entering the internal frame.
  __ LoadImmP(cp, Operand::Zero());

  // Enter an internal frame.
  {
    // FrameScope ends up calling MacroAssembler::EnterFrame here
    FrameScope scope(masm, StackFrame::INTERNAL);

    // Setup the context (we need to use the caller context from the isolate).
    ExternalReference context_address(Isolate::kContextAddress,
                                      masm->isolate());
    __ mov(cp, Operand(context_address));
    __ LoadP(cp, MemOperand(cp));

    __ InitializeRootRegister();

    // Push the function and the receiver onto the stack.
    __ Push(r3, r4);

    // Check if we have enough stack space to push all arguments.
    // Clobbers r4.
    Generate_CheckStackOverflow(masm, r5, kArgcIsUntaggedInt);

    // Copy arguments to the stack in a loop from argv to sp.
    // The arguments are actually placed in reverse order on sp
    // compared to argv (i.e. arg1 is highest memory in sp).
    // r3: function
    // r5: argc
    // r6: argv, i.e. points to first arg
    // r7: scratch reg to hold scaled argc
    // r8: scratch reg to hold arg handle
    // r9: scratch reg to hold index into argv
    Label argLoop, argExit;
    intptr_t zero = 0;
    __ ShiftLeftP(r7, r5, Operand(kPointerSizeLog2));
    __ SubRR(sp, r7);    // Buy the stack frame to fit args
    __ LoadImmP(r9, Operand(zero));  // Initialize argv index
    __ bind(&argLoop);
    __ CmpPH(r7, Operand(zero));
    __ beq(&argExit, Label::kNear);
    __ lay(r7, MemOperand(r7, -kPointerSize));
    __ LoadP(r8, MemOperand(r9, r6));  // read next parameter
    __ la(r9, MemOperand(r9, kPointerSize));  // r9++;
    __ LoadP(r0, MemOperand(r8));  // dereference handle
    __ StoreP(r0, MemOperand(r7, sp));  // push parameter
    __ b(&argLoop);
    __ bind(&argExit);

    // Setup new.target and argc.
    __ LoadRR(r6, r2);
    __ LoadRR(r2, r5);
    __ LoadRR(r5, r6);

    // Initialize all JavaScript callee-saved registers, since they will be seen
    // by the garbage collector as part of handlers.
    __ LoadRoot(r6, Heap::kUndefinedValueRootIndex);
    __ LoadRR(r7, r6);
    __ LoadRR(r8, r6);
    __ LoadRR(r9, r6);

    // Invoke the code.
    Handle<Code> builtin = is_construct
                               ? masm->isolate()->builtins()->Construct()
                               : masm->isolate()->builtins()->Call();
    __ Call(builtin, RelocInfo::CODE_TARGET);

    // Exit the JS frame and remove the parameters (except function), and
    // return.
  }
  __ b(r14);

  // r2: result
}


void Builtins::Generate_JSEntryTrampoline(MacroAssembler* masm) {
  Generate_JSEntryTrampolineHelper(masm, false);
}


void Builtins::Generate_JSConstructEntryTrampoline(MacroAssembler* masm) {
  Generate_JSEntryTrampolineHelper(masm, true);
}


// Generate code for entering a JS function with the interpreter.
// On entry to the function the receiver and arguments have been pushed on the
// stack left to right.  The actual argument count matches the formal parameter
// count expected by the function.
//
// The live registers are:
//   o r3: the JS function object being called.
//   o cp: our context
//   o pp: the caller's constant pool pointer (if enabled)
//   o fp: the caller's frame pointer
//   o sp: stack pointer
//   o lr: return address
//
// The function builds a JS frame.  Please see JavaScriptFrameConstants in
// frames-s390.h for its layout.
// TODO(rmcilroy): We will need to include the current bytecode pointer in the
// frame.
void Builtins::Generate_InterpreterEntryTrampoline(MacroAssembler* masm) {
  // Open a frame scope to indicate that there is a frame on the stack.  The
  // MANUAL indicates that the scope shouldn't actually generate code to set up
  // the frame (that is done below).
  FrameScope frame_scope(masm, StackFrame::MANUAL);
  __ PushFixedFrame(r3);
  __ AddP(fp, sp, Operand(StandardFrameConstants::kFixedFrameSizeFromFp));

  // Get the bytecode array from the function object and load the pointer to the
  // first entry into kInterpreterBytecodeRegister.
  __ LoadP(r2, FieldMemOperand(r3, JSFunction::kSharedFunctionInfoOffset));
  __ LoadP(kInterpreterBytecodeArrayRegister,
           FieldMemOperand(r2, SharedFunctionInfo::kFunctionDataOffset));

  if (FLAG_debug_code) {
    // Check function data field is actually a BytecodeArray object.
    __ TestIfSmi(kInterpreterBytecodeArrayRegister);
    __ Assert(ne, kFunctionDataShouldBeBytecodeArrayOnInterpreterEntry);
    __ CompareObjectType(kInterpreterBytecodeArrayRegister, r2, no_reg,
                         BYTECODE_ARRAY_TYPE);
    __ Assert(eq, kFunctionDataShouldBeBytecodeArrayOnInterpreterEntry);
  }

  // Allocate the local and temporary register file on the stack.
  {
    // Load frame size (word) from the BytecodeArray object.
    __ LoadlW(r4, FieldMemOperand(kInterpreterBytecodeArrayRegister,
                               BytecodeArray::kFrameSizeOffset));

    // Do a stack check to ensure we don't go over the limit.
    Label ok;
    __ SubP(r5, sp, r4);
    __ LoadRoot(r0, Heap::kRealStackLimitRootIndex);
    __ CmpLogicalP(r5, r0);
    __ bge(&ok);
    __ CallRuntime(Runtime::kThrowStackOverflow, 0);
    __ bind(&ok);

    // If ok, push undefined as the initial value for all register file entries.
    // TODO(rmcilroy): Consider doing more than one push per loop iteration.
    Label loop, no_args;
    __ LoadRoot(r5, Heap::kUndefinedValueRootIndex);
    __ ShiftRightP(r4, r4, Operand(kPointerSizeLog2));
    __ beq(&no_args);
    // __ mtctr(r4);
    __ LoadRR(r1, r4);
    __ bind(&loop);
    __ push(r5);
    __ SubP(r1, Operand(1));
    __ bne(&loop);
    // __ bdnz(&loop);
    __ bind(&no_args);
  }

  // TODO(rmcilroy): List of things not currently dealt with here but done in
  // fullcodegen's prologue:
  //  - Support profiler (specifically profiling_counter).
  //  - Call ProfileEntryHookStub when isolate has a function_entry_hook.
  //  - Allow simulator stop operations if FLAG_stop_at is set.
  //  - Code aging of the BytecodeArray object.

  // Perform stack guard check.
  {
    Label ok;
    __ LoadRoot(r0, Heap::kStackLimitRootIndex);
    __ CmpP(sp, r0);
    __ bge(&ok);
    __ push(kInterpreterBytecodeArrayRegister);
    __ CallRuntime(Runtime::kStackGuard, 0);
    __ pop(kInterpreterBytecodeArrayRegister);
    __ bind(&ok);
  }

  // Load accumulator, register file, bytecode offset, dispatch table into
  // registers.
  __ LoadRoot(kInterpreterAccumulatorRegister, Heap::kUndefinedValueRootIndex);
  __ SubP(
      kInterpreterRegisterFileRegister, fp,
      Operand(kPointerSize + StandardFrameConstants::kFixedFrameSizeFromFp));
  __ mov(kInterpreterBytecodeOffsetRegister,
         Operand(BytecodeArray::kHeaderSize - kHeapObjectTag));
  __ LoadRoot(kInterpreterDispatchTableRegister,
              Heap::kInterpreterTableRootIndex);
  __ AddP(kInterpreterDispatchTableRegister, kInterpreterDispatchTableRegister,
          Operand(FixedArray::kHeaderSize - kHeapObjectTag));

  // Dispatch to the first bytecode handler for the function.
  __ LoadlB(r3, MemOperand(kInterpreterBytecodeArrayRegister,
                         kInterpreterBytecodeOffsetRegister));
  __ ShiftLeftP(ip, r3, Operand(kPointerSizeLog2));
  __ LoadP(ip, MemOperand(kInterpreterDispatchTableRegister, ip));
  // TODO(rmcilroy): Make dispatch table point to code entrys to avoid untagging
  // and header removal.
  __ AddP(ip, ip, Operand(Code::kHeaderSize - kHeapObjectTag));
  __ Call(ip);
}


void Builtins::Generate_InterpreterExitTrampoline(MacroAssembler* masm) {
  // TODO(rmcilroy): List of things not currently dealt with here but done in
  // fullcodegen's EmitReturnSequence.
  //  - Supporting FLAG_trace for Runtime::TraceExit.
  //  - Support profiler (specifically decrementing profiling_counter
  //    appropriately and calling out to HandleInterrupts if necessary).

  // The return value is in accumulator, which is already in r2.

  // Leave the frame (also dropping the register file).
  __ LeaveFrame(StackFrame::JAVA_SCRIPT);

  // Drop receiver + arguments and return.
  __ LoadlW(r0, FieldMemOperand(kInterpreterBytecodeArrayRegister,
                             BytecodeArray::kParameterSizeOffset));
  __ AddP(sp, sp, r0);
  __ Ret();
}


static void Generate_InterpreterPushArgs(MacroAssembler* masm, Register index,
                                         Register count, Register scratch) {
  Label loop;
  __ AddP(index, index, Operand(kPointerSize));  // Bias up for LoadPU
  // __ mtctr(count);
  __ LoadRR(r0, count);
  __ bind(&loop);
  __ LoadP(scratch, MemOperand(index, -kPointerSize));
  __ lay(index, MemOperand(index, -kPointerSize));
  __ push(scratch);
  __ SubP(r0, Operand(1));
  __ bne(&loop);
  // __ bdnz(&loop);
}


// static
void Builtins::Generate_InterpreterPushArgsAndCall(MacroAssembler* masm) {
  // ----------- S t a t e -------------
  //  -- r2 : the number of arguments (not including the receiver)
  //  -- r4 : the address of the first argument to be pushed. Subsequent
  //          arguments should be consecutive above this, in the same order as
  //          they are to be pushed onto the stack.
  //  -- r3 : the target to call (can be any Object).
  // -----------------------------------

  // Calculate number of arguments (AddP one for receiver).
  __ AddP(r5, r2, Operand(1));

  // Push the arguments.
  Generate_InterpreterPushArgs(masm, r4, r5, r6);

  // Call the target.
  __ Jump(masm->isolate()->builtins()->Call(), RelocInfo::CODE_TARGET);
}


// static
void Builtins::Generate_InterpreterPushArgsAndConstruct(MacroAssembler* masm) {
  // ----------- S t a t e -------------
  // -- r2 : argument count (not including receiver)
  // -- r5 : new target
  // -- r3 : constructor to call
  // -- r4 : address of the first argument
  // -----------------------------------

  // Push a slot for the receiver to be constructed.
  __ push(r2);

  // Push the arguments (skip if none).
  Label skip;
  __ CmpP(r2, Operand::Zero());
  __ beq(&skip);
  Generate_InterpreterPushArgs(masm, r4, r2, r6);
  __ bind(&skip);

  // Call the constructor with r2, r3, and r5 unmodified.
  __ Jump(masm->isolate()->builtins()->Construct(), RelocInfo::CONSTRUCT_CALL);
}


void Builtins::Generate_CompileLazy(MacroAssembler* masm) {
  CallRuntimePassFunction(masm, Runtime::kCompileLazy);
  GenerateTailCallToReturnedCode(masm);
}


void Builtins::Generate_CompileOptimized(MacroAssembler* masm) {
  CallRuntimePassFunction(masm, Runtime::kCompileOptimized_NotConcurrent);
  GenerateTailCallToReturnedCode(masm);
}


void Builtins::Generate_CompileOptimizedConcurrent(MacroAssembler* masm) {
  CallRuntimePassFunction(masm, Runtime::kCompileOptimized_Concurrent);
  GenerateTailCallToReturnedCode(masm);
}


static void GenerateMakeCodeYoungAgainCommon(MacroAssembler* masm) {
  // For now, we are relying on the fact that make_code_young doesn't do any
  // garbage collection which allows us to save/restore the registers without
  // worrying about which of them contain pointers. We also don't build an
  // internal frame to make the code faster, since we shouldn't have to do stack
  // crawls in MakeCodeYoung. This seems a bit fragile.

  // Point r2 at the start of the PlatformCodeAge sequence.
  __ CleanseP(r14);
  __ SubP(r14, Operand(kCodeAgingSequenceLength));
  __ LoadRR(r2, r14);

  // The following registers must be saved and restored when calling through to
  // the runtime:
  //   r2 - contains return address (beginning of patch sequence)
  //   r3 - isolate
  //   r5 - new target
  //   lr - return address
  FrameScope scope(masm, StackFrame::MANUAL);
  __ MultiPush(r14.bit() | r2.bit() | r3.bit() | r5.bit() | fp.bit());
  __ PrepareCallCFunction(2, 0, r4);
  __ mov(r3, Operand(ExternalReference::isolate_address(masm->isolate())));
  __ CallCFunction(
      ExternalReference::get_make_code_young_function(masm->isolate()), 2);
  __ MultiPop(r14.bit() | r2.bit() | r3.bit() | r5.bit() | fp.bit());
  __ LoadRR(ip, r2);
  __ Jump(ip);
}

#define DEFINE_CODE_AGE_BUILTIN_GENERATOR(C)                  \
  void Builtins::Generate_Make##C##CodeYoungAgainEvenMarking( \
      MacroAssembler* masm) {                                 \
    GenerateMakeCodeYoungAgainCommon(masm);                   \
  }                                                           \
  void Builtins::Generate_Make##C##CodeYoungAgainOddMarking(  \
      MacroAssembler* masm) {                                 \
    GenerateMakeCodeYoungAgainCommon(masm);                   \
  }
CODE_AGE_LIST(DEFINE_CODE_AGE_BUILTIN_GENERATOR)
#undef DEFINE_CODE_AGE_BUILTIN_GENERATOR


void Builtins::Generate_MarkCodeAsExecutedOnce(MacroAssembler* masm) {
  // For now, we are relying on the fact that make_code_young doesn't do any
  // garbage collection which allows us to save/restore the registers without
  // worrying about which of them contain pointers. We also don't build an
  // internal frame to make the code faster, since we shouldn't have to do stack
  // crawls in MakeCodeYoung. This seems a bit fragile.

  // Point r2 at the start of the PlatformCodeAge sequence.
  __ CleanseP(r14);
  __ SubP(r14, Operand(kCodeAgingSequenceLength));
  __ LoadRR(r2, r14);

  // The following registers must be saved and restored when calling through to
  // the runtime:
  //   r2 - contains return address (beginning of patch sequence)
  //   r3 - isolate
  //   r5 - new target
  //   lr - return address
  FrameScope scope(masm, StackFrame::MANUAL);
  __ MultiPush(r14.bit() | r2.bit() | r3.bit() | r5.bit() | fp.bit());
  __ PrepareCallCFunction(2, 0, r4);
  __ mov(r3, Operand(ExternalReference::isolate_address(masm->isolate())));
  __ CallCFunction(
      ExternalReference::get_mark_code_as_executed_function(masm->isolate()),
      2);
  __ MultiPop(r14.bit() | r2.bit() | r3.bit() | r5.bit() | fp.bit());
  __ LoadRR(ip, r2);

  // Perform prologue operations usually performed by the young code stub.
  __ PushFixedFrame(r3);
  __ la(fp, MemOperand(sp, StandardFrameConstants::kFixedFrameSizeFromFp));

  // Jump to point after the code-age stub.
  __ AddP(r2, ip, Operand(kNoCodeAgeSequenceLength));
  __ Jump(r2);
}


void Builtins::Generate_MarkCodeAsExecutedTwice(MacroAssembler* masm) {
  GenerateMakeCodeYoungAgainCommon(masm);
}


void Builtins::Generate_MarkCodeAsToBeExecutedOnce(MacroAssembler* masm) {
  Generate_MarkCodeAsExecutedOnce(masm);
}


static void Generate_NotifyStubFailureHelper(MacroAssembler* masm,
                                             SaveFPRegsMode save_doubles) {
  {
    FrameScope scope(masm, StackFrame::INTERNAL);

    // Preserve registers across notification, this is important for compiled
    // stubs that tail call the runtime on deopts passing their parameters in
    // registers.
    __ MultiPush(kJSCallerSaved | kCalleeSaved);
    // Pass the function and deoptimization type to the runtime system.
    __ CallRuntime(Runtime::kNotifyStubFailure, 0, save_doubles);
    __ MultiPop(kJSCallerSaved | kCalleeSaved);
  }

  __ la(sp, MemOperand(sp, kPointerSize));  // Ignore state
  __ Ret();                                 // Jump to miss handler
}


void Builtins::Generate_NotifyStubFailure(MacroAssembler* masm) {
  Generate_NotifyStubFailureHelper(masm, kDontSaveFPRegs);
}


void Builtins::Generate_NotifyStubFailureSaveDoubles(MacroAssembler* masm) {
  Generate_NotifyStubFailureHelper(masm, kSaveFPRegs);
}


static void Generate_NotifyDeoptimizedHelper(MacroAssembler* masm,
                                             Deoptimizer::BailoutType type) {
  {
    FrameScope scope(masm, StackFrame::INTERNAL);
    // Pass the function and deoptimization type to the runtime system.
    __ LoadSmiLiteral(r2, Smi::FromInt(static_cast<int>(type)));
    __ push(r2);
    __ CallRuntime(Runtime::kNotifyDeoptimized, 1);
  }

  // Get the full codegen state from the stack and untag it -> r8.
  __ LoadP(r8, MemOperand(sp, 0 * kPointerSize));
  __ SmiUntag(r8);
  // Switch on the state.
  Label with_tos_register, unknown_state;
  __ CmpP(r8, Operand(FullCodeGenerator::NO_REGISTERS));
  __ bne(&with_tos_register);
  __ la(sp, MemOperand(sp, 1 * kPointerSize));  // Remove state.
  __ Ret();

  __ bind(&with_tos_register);
  __ LoadP(r2, MemOperand(sp, 1 * kPointerSize));
  __ CmpP(r8, Operand(FullCodeGenerator::TOS_REG));
  __ bne(&unknown_state);
  __ la(sp, MemOperand(sp, 2 * kPointerSize));  // Remove state.
  __ Ret();

  __ bind(&unknown_state);
  __ stop("no cases left");
}


void Builtins::Generate_NotifyDeoptimized(MacroAssembler* masm) {
  Generate_NotifyDeoptimizedHelper(masm, Deoptimizer::EAGER);
}


void Builtins::Generate_NotifySoftDeoptimized(MacroAssembler* masm) {
  Generate_NotifyDeoptimizedHelper(masm, Deoptimizer::SOFT);
}


void Builtins::Generate_NotifyLazyDeoptimized(MacroAssembler* masm) {
  Generate_NotifyDeoptimizedHelper(masm, Deoptimizer::LAZY);
}


// Clobbers registers {r6, r7, r8, r9}.
void CompatibleReceiverCheck(MacroAssembler* masm, Register receiver,
                             Register function_template_info,
                             Label* receiver_check_failed) {
  Register signature = r6;
  Register map = r7;
  Register constructor = r8;
  Register scratch = r9;

  __ CompareObjectType(receiver, map, no_reg, FIRST_JS_OBJECT_TYPE);
  __ blt(receiver_check_failed);

  // If there is no signature, return the holder.
  __ LoadP(signature, FieldMemOperand(function_template_info,
                                      FunctionTemplateInfo::kSignatureOffset));
  Label receiver_check_passed;
  __ JumpIfRoot(signature, Heap::kUndefinedValueRootIndex,
                &receiver_check_passed);

  // Walk the prototype chain.
  Label prototype_loop_start;
  __ bind(&prototype_loop_start);

  // End if the receiver is null or if it's a hidden type.
  __ JumpIfRoot(receiver, Heap::kNullValueRootIndex, receiver_check_failed);
  __ LoadP(map, FieldMemOperand(receiver, HeapObject::kMapOffset));
  __ LoadP(scratch, FieldMemOperand(map, Map::kBitField3Offset));
  __ DecodeField<Map::IsHiddenPrototype>(scratch, SetRC);
  __ bne(receiver_check_failed, cr0);


  // Get the constructor, if any.
  __ GetMapConstructor(constructor, map, scratch, scratch);
  __ CmpP(scratch, Operand(JS_FUNCTION_TYPE));
  Label next_prototype;
  __ bne(&next_prototype);
  Register type = constructor;
  __ LoadP(type,
           FieldMemOperand(constructor, JSFunction::kSharedFunctionInfoOffset));
  __ LoadP(type,
           FieldMemOperand(type, SharedFunctionInfo::kFunctionDataOffset));

  // Loop through the chain of inheriting function templates.
  Label function_template_loop;
  __ bind(&function_template_loop);

  // If the signatures match, we have a compatible receiver.
  __ CmpP(signature, type);
  __ beq(&receiver_check_passed);

  // If the current type is not a FunctionTemplateInfo, load the next prototype
  // in the chain.
  __ JumpIfSmi(type, &next_prototype);
  __ CompareObjectType(type, scratch, scratch, FUNCTION_TEMPLATE_INFO_TYPE);
  __ bne(&next_prototype);

  // Otherwise load the parent function template and iterate.
  __ LoadP(type,
           FieldMemOperand(type, FunctionTemplateInfo::kParentTemplateOffset));
  __ b(&function_template_loop);

  // Load the next prototype and iterate.
  __ bind(&next_prototype);
  __ LoadP(receiver, FieldMemOperand(map, Map::kPrototypeOffset));
  __ b(&prototype_loop_start);

  __ bind(&receiver_check_passed);
}


void Builtins::Generate_HandleFastApiCall(MacroAssembler* masm) {
  // ----------- S t a t e -------------
  //  -- r2                 : number of arguments excluding receiver
  //  -- r3                 : callee
  //  -- lr                 : return address
  //  -- sp[0]              : last argument
  //  -- ...
  //  -- sp[4 * (argc - 1)] : first argument
  //  -- sp[4 * argc]       : receiver
  // -----------------------------------

  // Load the receiver.
  __ ShiftLeftP(r1, r2, Operand(kPointerSizeLog2));
  __ LoadP(r4, MemOperand(sp, r1));

  // Update the receiver if this is a contextual call.
  Label set_global_proxy, valid_receiver;
  __ JumpIfRoot(r4, Heap::kUndefinedValueRootIndex, &set_global_proxy);

  // Load the FunctionTemplateInfo.
  __ bind(&valid_receiver);
  __ LoadP(r5, FieldMemOperand(r3, JSFunction::kSharedFunctionInfoOffset));
  __ LoadP(r5, FieldMemOperand(r5, SharedFunctionInfo::kFunctionDataOffset));

  // Do the compatible receiver check.
  Label receiver_check_failed;
  CompatibleReceiverCheck(masm, r4, r5, &receiver_check_failed);

  // Get the callback offset from the FunctionTemplateInfo, and jump to the
  // beginning of the code.
  __ LoadP(r6, FieldMemOperand(r5, FunctionTemplateInfo::kCallCodeOffset));
  __ LoadP(r6, FieldMemOperand(r6, CallHandlerInfo::kFastHandlerOffset));
  __ AddP(ip, r6, Operand(Code::kHeaderSize - kHeapObjectTag));
  __ JumpToJSEntry(ip);

  __ bind(&set_global_proxy);
  __ LoadGlobalProxy(r4);
  __ StoreP(r4, MemOperand(sp, r1));
  __ b(&valid_receiver);

  // Compatible receiver check failed: throw an Illegal Invocation exception.
  __ bind(&receiver_check_failed);
  // Drop the arguments (including the receiver);
  __ AddP(r1, r1, Operand(kPointerSize));
  __ AddP(sp, sp, r1);
  __ TailCallRuntime(Runtime::kThrowIllegalInvocation, 0, 1);
}


void Builtins::Generate_OnStackReplacement(MacroAssembler* masm) {
  // Lookup the function in the JavaScript frame.
  __ LoadP(r2, MemOperand(fp, JavaScriptFrameConstants::kFunctionOffset));
  {
    FrameScope scope(masm, StackFrame::INTERNAL);
    // Pass function as argument.
    __ push(r2);
    __ CallRuntime(Runtime::kCompileForOnStackReplacement, 1);
  }

  // If the code object is null, just return to the unoptimized code.
  Label skip;
  __ CmpSmiLiteral(r2, Smi::FromInt(0), r0);
  __ bne(&skip);
  __ Ret();

  __ bind(&skip);

  // Load deoptimization data from the code object.
  // <deopt_data> = <code>[#deoptimization_data_offset]
  __ LoadP(r3, FieldMemOperand(r2, Code::kDeoptimizationDataOffset));

    // Load the OSR entrypoint offset from the deoptimization data.
    // <osr_offset> = <deopt_data>[#header_size + #osr_pc_offset]
    __ LoadP(r3, FieldMemOperand(
             r3, FixedArray::OffsetOfElementAt(
                             DeoptimizationInputData::kOsrPcOffsetIndex)));
    __ SmiUntag(r3);

    // Compute the target address = code_obj + header_size + osr_offset
    // <entry_addr> = <code_obj> + #header_size + <osr_offset>
    __ AddP(r2, r3);
    __ AddP(r0, r2, Operand(Code::kHeaderSize - kHeapObjectTag));
    __ LoadRR(r14, r0);

    // And "return" to the OSR entry point of the function.
    __ Ret();
}


void Builtins::Generate_OsrAfterStackCheck(MacroAssembler* masm) {
  // We check the stack limit as indicator that recompilation might be done.
  Label ok;
  __ CmpLogicalP(sp, RootMemOperand(Heap::kStackLimitRootIndex));
  __ bge(&ok, Label::kNear);
  {
    FrameScope scope(masm, StackFrame::INTERNAL);
    __ CallRuntime(Runtime::kStackGuard, 0);
  }
  __ Jump(masm->isolate()->builtins()->OnStackReplacement(),
          RelocInfo::CODE_TARGET);

  __ bind(&ok);
  __ Ret();
}


// static
void Builtins::Generate_FunctionCall(MacroAssembler* masm) {
  // 1. Make sure we have at least one argument.
  // r2: actual number of arguments
  {
    Label done;
    __ CmpP(r2, Operand::Zero());
    __ bne(&done, Label::kNear);
    __ PushRoot(Heap::kUndefinedValueRootIndex);
    __ AddP(r2, Operand(1));
    __ bind(&done);
  }

  // r2: actual number of arguments
  // 2. Get the callable to call (passed as receiver) from the stack.
  __ ShiftLeftP(r4, r2, Operand(kPointerSizeLog2));
  __ LoadP(r3, MemOperand(sp, r4));

  // 3. Shift arguments and return address one slot down on the stack
  //    (overwriting the original receiver).  Adjust argument count to make
  //    the original first argument the new receiver.
  // r2: actual number of arguments
  // r3: callable
  {
    Label loop;
    // Calculate the copy start address (destination). Copy end address is sp.
    __ AddP(r4, sp, r4);

    __ bind(&loop);
    __ LoadP(ip, MemOperand(r4, -kPointerSize));
    __ StoreP(ip, MemOperand(r4));
    __ SubP(r4, Operand(kPointerSize));
    __ CmpP(r4, sp);
    __ bne(&loop);
    // Adjust the actual number of arguments and remove the top element
    // (which is a copy of the last argument).
    __ SubP(r2, Operand(1));
    __ pop();
  }

  // 4. Call the callable.
  __ Jump(masm->isolate()->builtins()->Call(), RelocInfo::CODE_TARGET);
}


static void Generate_PushAppliedArguments(MacroAssembler* masm,
                                          const int vectorOffset,
                                          const int argumentsOffset,
                                          const int indexOffset,
                                          const int limitOffset) {
  Register receiver = LoadDescriptor::ReceiverRegister();
  Register key = LoadDescriptor::NameRegister();
  Register slot = LoadDescriptor::SlotRegister();
  Register vector = LoadWithVectorDescriptor::VectorRegister();

  // Copy all arguments from the array to the stack.
  Label entry, loop;
  __ LoadP(key, MemOperand(fp, indexOffset));
  __ b(&entry);
  __ bind(&loop);
  __ LoadP(receiver, MemOperand(fp, argumentsOffset));

  // Use inline caching to speed up access to arguments.
  int slot_index = TypeFeedbackVector::PushAppliedArgumentsIndex();
  __ LoadSmiLiteral(slot, Smi::FromInt(slot_index));
  __ LoadP(vector, MemOperand(fp, vectorOffset));
  Handle<Code> ic =
      KeyedLoadICStub(masm->isolate(), LoadICState(kNoExtraICState)).GetCode();
  __ Call(ic, RelocInfo::CODE_TARGET);

  // Push the nth argument.
  __ push(r2);

  // Update the index on the stack and in register key.
  __ LoadP(key, MemOperand(fp, indexOffset));
  __ AddSmiLiteral(key, key, Smi::FromInt(1), r0);
  __ StoreP(key, MemOperand(fp, indexOffset));

  // Test if the copy loop has finished copying all the elements from the
  // arguments object.
  __ bind(&entry);
  __ LoadP(r0, MemOperand(fp, limitOffset));
  __ CmpP(key, r0);
  __ bne(&loop);

  // On exit, the pushed arguments count is in r2, untagged
  __ SmiUntag(r2, key);
}


// Used by FunctionApply and ReflectApply
static void Generate_ApplyHelper(MacroAssembler* masm, bool targetIsArgument) {
  const int kFormalParameters = targetIsArgument ? 3 : 2;
  const int kStackSize = kFormalParameters + 1;

  {
    FrameScope frame_scope(masm, StackFrame::INTERNAL);
    const int kArgumentsOffset = kFPOnStackSize + kPCOnStackSize;
    const int kReceiverOffset = kArgumentsOffset + kPointerSize;
    const int kFunctionOffset = kReceiverOffset + kPointerSize;
    const int kVectorOffset =
        InternalFrameConstants::kCodeOffset - 1 * kPointerSize;

    // Push the vector.
    __ LoadP(r3, FieldMemOperand(r3, JSFunction::kSharedFunctionInfoOffset));
    __ LoadP(r3,
             FieldMemOperand(r3, SharedFunctionInfo::kFeedbackVectorOffset));
    __ push(r3);

    __ LoadP(r2, MemOperand(fp, kFunctionOffset));  // get the function
    __ LoadP(r3, MemOperand(fp, kArgumentsOffset));  // get the args array
    __ Push(r2, r3);
    if (targetIsArgument) {
      __ InvokeBuiltin(Context::REFLECT_APPLY_PREPARE_BUILTIN_INDEX,
                       CALL_FUNCTION);
    } else {
      __ InvokeBuiltin(Context::APPLY_PREPARE_BUILTIN_INDEX, CALL_FUNCTION);
    }

    Generate_CheckStackOverflow(masm, r2, kArgcIsSmiTagged);

    // Push current limit and index.
    const int kIndexOffset = kVectorOffset - (2 * kPointerSize);
    const int kLimitOffset = kVectorOffset - (1 * kPointerSize);
    __ LoadImmP(r3, Operand::Zero());
    __ LoadP(r4, MemOperand(fp, kReceiverOffset));
    __ Push(r2, r3, r4);  // limit, initial index and receiver.

    // Copy all arguments from the array to the stack.
    Generate_PushAppliedArguments(masm, kVectorOffset, kArgumentsOffset,
                                  kIndexOffset, kLimitOffset);

    // Call the callable.
    // TODO(bmeurer): This should be a tail call according to ES6.
    __ LoadP(r3, MemOperand(fp, kFunctionOffset));
    __ Call(masm->isolate()->builtins()->Call(), RelocInfo::CODE_TARGET);

    // Tear down the internal frame and remove function, receiver and args.
  }
  __ la(sp, MemOperand(sp, kStackSize * kPointerSize));
  __ Ret();
}


static void Generate_ConstructHelper(MacroAssembler* masm) {
  const int kFormalParameters = 3;
  const int kStackSize = kFormalParameters + 1;

  {
    FrameScope frame_scope(masm, StackFrame::INTERNAL);
    const int kNewTargetOffset = kFPOnStackSize + kPCOnStackSize;
    const int kArgumentsOffset = kNewTargetOffset + kPointerSize;
    const int kFunctionOffset = kArgumentsOffset + kPointerSize;
    static const int kVectorOffset =
        InternalFrameConstants::kCodeOffset - 1 * kPointerSize;

    // Push the vector.
    __ LoadP(r3, FieldMemOperand(r3, JSFunction::kSharedFunctionInfoOffset));
    __ LoadP(r3,
             FieldMemOperand(r3, SharedFunctionInfo::kFeedbackVectorOffset));
    __ push(r3);

    // If newTarget is not supplied, set it to constructor
    Label validate_arguments;
    __ LoadP(r2, MemOperand(fp, kNewTargetOffset));
    __ CompareRoot(r2, Heap::kUndefinedValueRootIndex);
    __ bne(&validate_arguments, Label::kNear);
    __ LoadP(r2, MemOperand(fp, kFunctionOffset));
    __ StoreP(r2, MemOperand(fp, kNewTargetOffset));

    // Validate arguments
    __ bind(&validate_arguments);
    __ LoadP(r2, MemOperand(fp, kFunctionOffset));  // get the function
    __ push(r2);
    __ LoadP(r2, MemOperand(fp, kArgumentsOffset));  // get the args array
    __ push(r2);
    __ LoadP(r2, MemOperand(fp, kNewTargetOffset));  // get the new.target
    __ push(r2);
    __ InvokeBuiltin(Context::REFLECT_CONSTRUCT_PREPARE_BUILTIN_INDEX,
                     CALL_FUNCTION);

    Generate_CheckStackOverflow(masm, r2, kArgcIsSmiTagged);

    // Push current limit and index.
    const int kIndexOffset = kVectorOffset - (2 * kPointerSize);
    const int kLimitOffset = kVectorOffset - (1 * kPointerSize);
    __ LoadImmP(r3, Operand::Zero());
    __ Push(r2, r3);  // limit and initial index.
    // Push the constructor function as callee
    __ LoadP(r2, MemOperand(fp, kFunctionOffset));
    __ push(r2);

    // Copy all arguments from the array to the stack.
    Generate_PushAppliedArguments(masm, kVectorOffset, kArgumentsOffset,
                                  kIndexOffset, kLimitOffset);

    // Use undefined feedback vector
    __ LoadRoot(r4, Heap::kUndefinedValueRootIndex);
    __ LoadP(r3, MemOperand(fp, kFunctionOffset));
    __ LoadP(r5, MemOperand(fp, kNewTargetOffset));

    // Call the function.
    __ Call(masm->isolate()->builtins()->Construct(),
            RelocInfo::CONSTRUCT_CALL);

    // Leave internal frame.
  }
  __ lay(sp, MemOperand(sp, kStackSize * kPointerSize));
  __ Ret();
}


void Builtins::Generate_FunctionApply(MacroAssembler* masm) {
  Generate_ApplyHelper(masm, false);
}


void Builtins::Generate_ReflectApply(MacroAssembler* masm) {
  Generate_ApplyHelper(masm, true);
}


void Builtins::Generate_ReflectConstruct(MacroAssembler* masm) {
  Generate_ConstructHelper(masm);
}


static void ArgumentAdaptorStackCheck(MacroAssembler* masm,
                                      Label* stack_overflow) {
  // ----------- S t a t e -------------
  //  -- r2 : actual number of arguments
  //  -- r3 : function (passed through to callee)
  //  -- r4 : expected number of arguments
  //  -- r5 : new target (passed through to callee)
  // -----------------------------------
  // Check the stack for overflow. We are not trying to catch
  // interruptions (e.g. debug break and preemption) here, so the "real stack
  // limit" is checked.
  __ LoadRoot(r7, Heap::kRealStackLimitRootIndex);
  // Make r7 the space we have left. The stack might already be overflowed
  // here which will cause r7 to become negative.
  __ SubP(r7, sp, r7);
  // Check if the arguments will overflow the stack.
  __ ShiftLeftP(r0, r4, Operand(kPointerSizeLog2));
  __ CmpP(r7, r0);
  __ ble(stack_overflow);  // Signed comparison.
}


static void EnterArgumentsAdaptorFrame(MacroAssembler* masm) {
  __ SmiTag(r2);
  __ LoadSmiLiteral(r6, Smi::FromInt(StackFrame::ARGUMENTS_ADAPTOR));
  // Stack updated as such:
  //    old SP --->
  //                 R14 Return Addr
  //                 Old FP                     <--- New FP
  //                 Argument Adapter SMI
  //                 Function
  //                 ArgC as SMI                <--- New SP
  __ lay(sp, MemOperand(sp, -5 * kPointerSize));

  // Cleanse the top nibble of 31-bit pointers.
  __ CleanseP(r14);
  __ StoreP(r14, MemOperand(sp, 4 * kPointerSize));
  __ StoreP(fp, MemOperand(sp, 3 * kPointerSize));
  __ StoreP(r6, MemOperand(sp, 2 * kPointerSize));
  __ StoreP(r3, MemOperand(sp, 1 * kPointerSize));
  __ StoreP(r2, MemOperand(sp, 0 * kPointerSize));
  __ la(fp, MemOperand(sp,
        StandardFrameConstants::kFixedFrameSizeFromFp + kPointerSize));
}


static void LeaveArgumentsAdaptorFrame(MacroAssembler* masm) {
  // ----------- S t a t e -------------
  //  -- r2 : result being passed through
  // -----------------------------------
  // Get the number of arguments passed (as a smi), tear down the frame and
  // then tear down the parameters.
  __ LoadP(r3, MemOperand(fp, -(StandardFrameConstants::kFixedFrameSizeFromFp +
                                kPointerSize)));
  int stack_adjustment = kPointerSize;  // adjust for receiver
  __ LeaveFrame(StackFrame::ARGUMENTS_ADAPTOR, stack_adjustment);
  __ SmiToPtrArrayOffset(r3, r3);
  __ lay(sp, MemOperand(sp, r3));
}


// static
void Builtins::Generate_CallFunction(MacroAssembler* masm,
                                     ConvertReceiverMode mode) {
  // ----------- S t a t e -------------
  //  -- r2 : the number of arguments (not including the receiver)
  //  -- r3 : the function to call (checked to be a JSFunction)
  // -----------------------------------
  __ AssertFunction(r3);

  // See ES6 section 9.2.1 [[Call]] ( thisArgument, argumentsList)
  // Check that the function is not a "classConstructor".
  Label class_constructor;
  __ LoadP(r4, FieldMemOperand(r3, JSFunction::kSharedFunctionInfoOffset));
  __ LoadlW(r5, FieldMemOperand(r4, SharedFunctionInfo::kCompilerHintsOffset));
  __ TestBitMask(r5, SharedFunctionInfo::kClassConstructorBits, r0);
  __ bne(&class_constructor);

  // Enter the context of the function; ToObject has to run in the function
  // context, and we also need to take the global proxy from the function
  // context in case of conversion.
  __ LoadP(cp, FieldMemOperand(r3, JSFunction::kContextOffset));
  // We need to convert the receiver for non-native sloppy mode functions.
  Label done_convert;
  __ AndP(r0, r5, Operand((1 << SharedFunctionInfo::kStrictModeBit) |
                          (1 << SharedFunctionInfo::kNativeBit)));
  __ bne(&done_convert);
  {
    // ----------- S t a t e -------------
    //  -- r2 : the number of arguments (not including the receiver)
    //  -- r3 : the function to call (checked to be a JSFunction)
    //  -- r4 : the shared function info.
    //  -- cp : the function context.
    // -----------------------------------

    if (mode == ConvertReceiverMode::kNullOrUndefined) {
      // Patch receiver to global proxy.
      __ LoadGlobalProxy(r5);
    } else {
      Label convert_to_object, convert_receiver;
      __ ShiftLeftP(r5, r2, Operand(kPointerSizeLog2));
      __ LoadP(r5, MemOperand(sp, r5));
      __ JumpIfSmi(r5, &convert_to_object);
      STATIC_ASSERT(LAST_JS_RECEIVER_TYPE == LAST_TYPE);
      __ CompareObjectType(r5, r6, r6, FIRST_JS_RECEIVER_TYPE);
      __ bge(&done_convert);
      if (mode != ConvertReceiverMode::kNotNullOrUndefined) {
        Label convert_global_proxy;
        __ JumpIfRoot(r5, Heap::kUndefinedValueRootIndex,
                      &convert_global_proxy);
        __ JumpIfNotRoot(r5, Heap::kNullValueRootIndex, &convert_to_object);
        __ bind(&convert_global_proxy);
        {
          // Patch receiver to global proxy.
          __ LoadGlobalProxy(r5);
        }
        __ b(&convert_receiver);
      }
      __ bind(&convert_to_object);
      {
        // Convert receiver using ToObject.
        // TODO(bmeurer): Inline the allocation here to avoid building the frame
        // in the fast case? (fall back to AllocateInNewSpace?)
        FrameAndConstantPoolScope scope(masm, StackFrame::INTERNAL);
        __ SmiTag(r2);
        __ Push(r2, r3);
        __ LoadRR(r2, r5);
        ToObjectStub stub(masm->isolate());
        __ CallStub(&stub);
        __ LoadRR(r5, r2);
        __ Pop(r2, r3);
        __ SmiUntag(r2);
      }
      __ LoadP(r4, FieldMemOperand(r3, JSFunction::kSharedFunctionInfoOffset));
      __ bind(&convert_receiver);
    }
    __ ShiftLeftP(r6, r2, Operand(kPointerSizeLog2));
    __ StoreP(r5, MemOperand(sp, r6));
  }
  __ bind(&done_convert);

  // ----------- S t a t e -------------
  //  -- r2 : the number of arguments (not including the receiver)
  //  -- r3 : the function to call (checked to be a JSFunction)
  //  -- r4 : the shared function info.
  //  -- cp : the function context.
  // -----------------------------------

  __ LoadW(
      r4, FieldMemOperand(r4, SharedFunctionInfo::kFormalParameterCountOffset));
#if !V8_TARGET_ARCH_S390X
  __ SmiUntag(r4);
#endif
  __ LoadP(r6, FieldMemOperand(r3, JSFunction::kCodeEntryOffset));
  ParameterCount actual(r2);
  ParameterCount expected(r4);
  __ InvokeCode(r6, no_reg, expected, actual, JUMP_FUNCTION, NullCallWrapper());

  // The function is a "classConstructor", need to raise an exception.
  __ bind(&class_constructor);
  {
    FrameAndConstantPoolScope frame(masm, StackFrame::INTERNAL);
    __ push(r3);
    __ CallRuntime(Runtime::kThrowConstructorNonCallableError, 1);
  }
}


// static
void Builtins::Generate_Call(MacroAssembler* masm, ConvertReceiverMode mode) {
  // ----------- S t a t e -------------
  //  -- r2 : the number of arguments (not including the receiver)
  //  -- r3 : the target to call (can be any Object).
  // -----------------------------------

  Label non_callable, non_function, non_smi;
  __ JumpIfSmi(r3, &non_callable);
  __ bind(&non_smi);
  __ CompareObjectType(r3, r6, r7, JS_FUNCTION_TYPE);
  __ Jump(masm->isolate()->builtins()->CallFunction(mode),
          RelocInfo::CODE_TARGET, eq);
  __ CmpP(r7, Operand(JS_FUNCTION_PROXY_TYPE));
  __ bne(&non_function);

  // 1. Call to function proxy.
  // TODO(neis): This doesn't match the ES6 spec for [[Call]] on proxies.
  __ LoadP(r3, FieldMemOperand(r3, JSFunctionProxy::kCallTrapOffset));
  __ AssertNotSmi(r3);
  __ b(&non_smi);

  // 2. Call to something else, which might have a [[Call]] internal method (if
  // not we raise an exception).
  __ bind(&non_function);
  // Check if target has a [[Call]] internal method.
  __ LoadlB(r6, FieldMemOperand(r6, Map::kBitFieldOffset));
  __ TestBit(r6, Map::kIsCallable, r0);
  __ beq(&non_callable);
  // Overwrite the original receiver the (original) target.
  __ ShiftLeftP(r7, r2, Operand(kPointerSizeLog2));
  __ StoreP(r3, MemOperand(sp, r7));
  // Let the "call_as_function_delegate" take care of the rest.
  __ LoadGlobalFunction(Context::CALL_AS_FUNCTION_DELEGATE_INDEX, r3);
  __ Jump(masm->isolate()->builtins()->CallFunction(
              ConvertReceiverMode::kNotNullOrUndefined),
          RelocInfo::CODE_TARGET);

  // 3. Call to something that is not callable.
  __ bind(&non_callable);
  {
    FrameAndConstantPoolScope scope(masm, StackFrame::INTERNAL);
    __ Push(r3);
    __ CallRuntime(Runtime::kThrowCalledNonCallable, 1);
  }
}


// static
void Builtins::Generate_ConstructFunction(MacroAssembler* masm) {
  // ----------- S t a t e -------------
  //  -- r2 : the number of arguments (not including the receiver)
  //  -- r3 : the constructor to call (checked to be a JSFunction)
  //  -- r5 : the new target (checked to be a JSFunction)
  // -----------------------------------
  __ AssertFunction(r3);
  __ AssertFunction(r5);

  // Calling convention for function specific ConstructStubs require
  // r4 to contain either an AllocationSite or undefined.
  __ LoadRoot(r4, Heap::kUndefinedValueRootIndex);

  // Tail call to the function-specific construct stub (still in the caller
  // context at this point).
  __ LoadP(r6, FieldMemOperand(r3, JSFunction::kSharedFunctionInfoOffset));
  __ LoadP(r6, FieldMemOperand(r6, SharedFunctionInfo::kConstructStubOffset));
  __ AddP(ip, r6, Operand(Code::kHeaderSize - kHeapObjectTag));
  __ JumpToJSEntry(ip);
}


// static
void Builtins::Generate_ConstructProxy(MacroAssembler* masm) {
  // ----------- S t a t e -------------
  //  -- r2 : the number of arguments (not including the receiver)
  //  -- r3 : the constructor to call (checked to be a JSFunctionProxy)
  //  -- r5 : the new target (either the same as the constructor or
  //          the JSFunction on which new was invoked initially)
  // -----------------------------------

  // TODO(neis): This doesn't match the ES6 spec for [[Construct]] on proxies.
  __ LoadP(r3, FieldMemOperand(r3, JSFunctionProxy::kConstructTrapOffset));
  __ Jump(masm->isolate()->builtins()->Call(), RelocInfo::CODE_TARGET);
}


// static
void Builtins::Generate_Construct(MacroAssembler* masm) {
  // ----------- S t a t e -------------
  //  -- r2 : the number of arguments (not including the receiver)
  //  -- r3 : the constructor to call (can be any Object)
  //  -- r5 : the new target (either the same as the constructor or
  //          the JSFunction on which new was invoked initially)
  // -----------------------------------

  // Check if target is a Smi.
  Label non_constructor;
  __ JumpIfSmi(r3, &non_constructor);

  // Dispatch based on instance type.
  __ CompareObjectType(r3, r6, r7, JS_FUNCTION_TYPE);
  __ Jump(masm->isolate()->builtins()->ConstructFunction(),
          RelocInfo::CODE_TARGET, eq);
  __ CmpP(r7, Operand(JS_FUNCTION_PROXY_TYPE));
  __ Jump(masm->isolate()->builtins()->ConstructProxy(), RelocInfo::CODE_TARGET,
          eq);

  // Check if target has a [[Construct]] internal method.
  __ LoadlB(r4, FieldMemOperand(r6, Map::kBitFieldOffset));
  __ TestBit(r4, Map::kIsConstructor);
  __ beq(&non_constructor);

  // Called Construct on an exotic Object with a [[Construct]] internal method.
  {
    // Overwrite the original receiver with the (original) target.
    __ ShiftLeftP(r7, r2, Operand(kPointerSizeLog2));
    __ StoreP(r3, MemOperand(sp, r7));
    // Let the "call_as_constructor_delegate" take care of the rest.
    __ LoadGlobalFunction(Context::CALL_AS_CONSTRUCTOR_DELEGATE_INDEX, r3);
    __ Jump(masm->isolate()->builtins()->CallFunction(),
            RelocInfo::CODE_TARGET);
  }

  // Called Construct on an Object that doesn't have a [[Construct]] internal
  // method.
  __ bind(&non_constructor);
  __ Jump(masm->isolate()->builtins()->ConstructedNonConstructable(),
          RelocInfo::CODE_TARGET);
}


void Builtins::Generate_ArgumentsAdaptorTrampoline(MacroAssembler* masm) {
  // ----------- S t a t e -------------
  //  -- r2 : actual number of arguments
  //  -- r3 : function (passed through to callee)
  //  -- r4 : expected number of arguments
  //  -- r5 : new target (passed through to callee)
  // -----------------------------------

  Label invoke, dont_adapt_arguments, stack_overflow;

  Label enough, too_few;
  __ LoadP(ip, FieldMemOperand(r3, JSFunction::kCodeEntryOffset));
  __ CmpP(r2, r4);
  __ blt(&too_few);
  __ CmpP(r4, Operand(SharedFunctionInfo::kDontAdaptArgumentsSentinel));
  __ beq(&dont_adapt_arguments);

  {  // Enough parameters: actual >= expected
    __ bind(&enough);
    EnterArgumentsAdaptorFrame(masm);
    ArgumentAdaptorStackCheck(masm, &stack_overflow);

    // Calculate copy start address into r2 and copy end address into r6.
    // r2: actual number of arguments as a smi
    // r3: function
    // r4: expected number of arguments
    // r5: new target (passed through to callee)
    // ip: code entry to call
    __ SmiToPtrArrayOffset(r2, r2);
    __ AddP(r2, fp);
    // adjust for return address and receiver
    __ AddP(r2, r2, Operand(2 * kPointerSize));
    __ ShiftLeftP(r6, r4, Operand(kPointerSizeLog2));
    __ SubP(r6, r2, r6);

    // Copy the arguments (including the receiver) to the new stack frame.
    // r2: copy start address
    // r3: function
    // r4: expected number of arguments
    // r5: new target (passed through to callee)
    // r6: copy end address
    // ip: code entry to call

    Label copy;
    __ bind(&copy);
    __ LoadP(r0, MemOperand(r2, 0));
    __ push(r0);
    __ CmpP(r2, r6);  // Compare before moving to next argument.
    __ SubP(r2, r2, Operand(kPointerSize));
    __ bne(&copy);

    __ b(&invoke);
  }

  {  // Too few parameters: Actual < expected
    __ bind(&too_few);

    // If the function is strong we need to throw an error.
    Label no_strong_error;
    __ LoadP(r6, FieldMemOperand(r3, JSFunction::kSharedFunctionInfoOffset));
    __ LoadlW(r7, FieldMemOperand(r6, SharedFunctionInfo::kCompilerHintsOffset));
    __ TestBit(r7, SharedFunctionInfo::kStrongModeBit, r0);
    __ beq(&no_strong_error);

    // What we really care about is the required number of arguments.
    __ LoadlW(r6, FieldMemOperand(r6, SharedFunctionInfo::kLengthOffset));
#if V8_TARGET_ARCH_S390X
    // See commment near kLenghtOffset in src/objects.h
    __ ShiftRightArith(r6, r6, Operand(kSmiTagSize));
#else
    __ SmiUntag(r6);
#endif
    __ CmpP(r2, r6);
    __ bge(&no_strong_error);

    {
      FrameScope frame(masm, StackFrame::MANUAL);
      EnterArgumentsAdaptorFrame(masm);
      __ CallRuntime(Runtime::kThrowStrongModeTooFewArguments, 0);
    }

    __ bind(&no_strong_error);
    EnterArgumentsAdaptorFrame(masm);
    ArgumentAdaptorStackCheck(masm, &stack_overflow);

    // Calculate copy start address into r0 and copy end address is fp.
    // r2: actual number of arguments as a smi
    // r3: function
    // r4: expected number of arguments
    // r5: new target (passed through to callee)
    // ip: code entry to call
    __ SmiToPtrArrayOffset(r2, r2);
    __ lay(r2, MemOperand(r2, fp));

    // Copy the arguments (including the receiver) to the new stack frame.
    // r2: copy start address
    // r3: function
    // r4: expected number of arguments
    // r5: new target (passed through to callee)
    // ip: code entry to call
    Label copy;
    __ bind(&copy);
    // Adjust load for return address and receiver.
    __ LoadP(r0, MemOperand(r2, 2 * kPointerSize));
    __ push(r0);
    __ CmpP(r2, fp);  // Compare before moving to next argument.
    __ lay(r2, MemOperand(r2, -kPointerSize));
    __ bne(&copy);

    // Fill the remaining expected arguments with undefined.
    // r3: function
    // r4: expected number of argumentus
    // ip: code entry to call
    __ LoadRoot(r0, Heap::kUndefinedValueRootIndex);
    __ ShiftLeftP(r6, r4, Operand(kPointerSizeLog2));
    __ SubP(r6, fp, r6);
    // Adjust for frame.
    __ SubP(r6, r6, Operand(StandardFrameConstants::kFixedFrameSizeFromFp +
                            2 * kPointerSize));

    Label fill;
    __ bind(&fill);
    __ push(r0);
    __ CmpP(sp, r6);
    __ bne(&fill);
  }

  // Call the entry point.
  __ bind(&invoke);
  __ LoadRR(r2, r4);
  // r2 : expected number of arguments
  // r3 : function (passed through to callee)
  // r5 : new target (passed through to callee)
  __ CallJSEntry(ip);

  // Store offset of return address for deoptimizer.
  masm->isolate()->heap()->SetArgumentsAdaptorDeoptPCOffset(masm->pc_offset());

  // Exit frame and return.
  LeaveArgumentsAdaptorFrame(masm);
  __ Ret();


  // -------------------------------------------
  // Dont adapt arguments.
  // -------------------------------------------
  __ bind(&dont_adapt_arguments);
  __ JumpToJSEntry(ip);

  __ bind(&stack_overflow);
  {
    FrameScope frame(masm, StackFrame::MANUAL);
    __ CallRuntime(Runtime::kThrowStackOverflow, 0);
    __ bkpt(0);
  }
}


#undef __
}  // namespace internal
}  // namespace v8

#endif  // V8_TARGET_ARCH_S390
