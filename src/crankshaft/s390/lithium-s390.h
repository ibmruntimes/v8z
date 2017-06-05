// Copyright 2014 the V8 project authors. All rights reserved.
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.

#ifndef V8_CRANKSHAFT_S390_LITHIUM_S390_H_
#define V8_CRANKSHAFT_S390_LITHIUM_S390_H_

#include "src/crankshaft/hydrogen.h"
#include "src/crankshaft/lithium.h"
#include "src/crankshaft/lithium-allocator.h"
#include "src/safepoint-table.h"
#include "src/utils.h"

namespace v8 {
namespace internal {

// Forward declarations.
class LCodeGen;

#define LITHIUM_CONCRETE_INSTRUCTION_LIST(V) \
  V(AccessArgumentsAt)                       \
  V(AddI)                                    \
  V(Allocate)                                \
  V(ApplyArguments)                          \
  V(ArgumentsElements)                       \
  V(ArgumentsLength)                         \
  V(ArithmeticD)                             \
  V(ArithmeticT)                             \
  V(BitI)                                    \
  V(BoundsCheck)                             \
  V(Branch)                                  \
  V(CallWithDescriptor)                      \
  V(CallNewArray)                            \
  V(CallRuntime)                             \
  V(CheckArrayBufferNotNeutered)             \
  V(CheckInstanceType)                       \
  V(CheckNonSmi)                             \
  V(CheckMaps)                               \
  V(CheckMapValue)                           \
  V(CheckSmi)                                \
  V(CheckValue)                              \
  V(ClampDToUint8)                           \
  V(ClampIToUint8)                           \
  V(ClampTToUint8)                           \
  V(ClassOfTestAndBranch)                    \
  V(CompareNumericAndBranch)                 \
  V(CmpObjectEqAndBranch)                    \
  V(CmpHoleAndBranch)                        \
  V(CmpMapAndBranch)                         \
  V(CmpT)                                    \
  V(ConstantD)                               \
  V(ConstantE)                               \
  V(ConstantI)                               \
  V(ConstantS)                               \
  V(ConstantT)                               \
  V(ConstructDouble)                         \
  V(Context)                                 \
  V(DebugBreak)                              \
  V(DeclareGlobals)                          \
  V(Deoptimize)                              \
  V(DivByConstI)                             \
  V(DivByPowerOf2I)                          \
  V(DivI)                                    \
  V(DoubleBits)                              \
  V(DoubleToI)                               \
  V(DoubleToSmi)                             \
  V(Drop)                                    \
  V(Dummy)                                   \
  V(DummyUse)                                \
  V(FlooringDivByConstI)                     \
  V(FlooringDivByPowerOf2I)                  \
  V(FlooringDivI)                            \
  V(ForInCacheArray)                         \
  V(ForInPrepareMap)                         \
  V(GetCachedArrayIndex)                     \
  V(Goto)                                    \
  V(HasCachedArrayIndexAndBranch)            \
  V(HasInPrototypeChainAndBranch)            \
  V(HasInstanceTypeAndBranch)                \
  V(InnerAllocatedObject)                    \
  V(InstanceOf)                              \
  V(InstructionGap)                          \
  V(Integer32ToDouble)                       \
  V(InvokeFunction)                          \
  V(IsStringAndBranch)                       \
  V(IsSmiAndBranch)                          \
  V(IsUndetectableAndBranch)                 \
  V(Label)                                   \
  V(LazyBailout)                             \
  V(LoadContextSlot)                         \
  V(LoadRoot)                                \
  V(LoadFieldByIndex)                        \
  V(LoadFunctionPrototype)                   \
  V(LoadGlobalGeneric)                       \
  V(LoadKeyed)                               \
  V(LoadKeyedGeneric)                        \
  V(LoadNamedField)                          \
  V(LoadNamedGeneric)                        \
  V(MathAbs)                                 \
  V(MathClz32)                               \
  V(MathExp)                                 \
  V(MathFloor)                               \
  V(MathFround)                              \
  V(MathLog)                                 \
  V(MathMinMax)                              \
  V(MathPowHalf)                             \
  V(MathRound)                               \
  V(MathSqrt)                                \
  V(MaybeGrowElements)                       \
  V(ModByConstI)                             \
  V(ModByPowerOf2I)                          \
  V(ModI)                                    \
  V(MulI)                                    \
  V(MultiplyAddD)                            \
  V(MultiplySubD)                            \
  V(NumberTagD)                              \
  V(NumberTagI)                              \
  V(NumberTagU)                              \
  V(NumberUntagD)                            \
  V(OsrEntry)                                \
  V(Parameter)                               \
  V(Power)                                   \
  V(Prologue)                                \
  V(PushArgument)                            \
  V(Return)                                  \
  V(SeqStringGetChar)                        \
  V(SeqStringSetChar)                        \
  V(ShiftI)                                  \
  V(SmiTag)                                  \
  V(SmiUntag)                                \
  V(StackCheck)                              \
  V(StoreCodeEntry)                          \
  V(StoreContextSlot)                        \
  V(StoreKeyed)                              \
  V(StoreKeyedGeneric)                       \
  V(StoreNamedField)                         \
  V(StoreNamedGeneric)                       \
  V(StringAdd)                               \
  V(StringCharCodeAt)                        \
  V(StringCharFromCode)                      \
  V(StringCompareAndBranch)                  \
  V(SubI)                                    \
  V(RSubI)                                   \
  V(TaggedToI)                               \
  V(ThisFunction)                            \
  V(TransitionElementsKind)                  \
  V(TrapAllocationMemento)                   \
  V(Typeof)                                  \
  V(TypeofIsAndBranch)                       \
  V(Uint32ToDouble)                          \
  V(UnknownOSRValue)                         \
  V(WrapReceiver)

#define DECLARE_CONCRETE_INSTRUCTION(type, mnemonic)            \
  Opcode opcode() const final { return LInstruction::k##type; } \
  void CompileToNative(LCodeGen* generator) final;              \
  const char* Mnemonic() const final { return mnemonic; }       \
  static L##type* cast(LInstruction* instr) {                   \
    DCHECK(instr->Is##type());                                  \
    return reinterpret_cast<L##type*>(instr);                   \
  }

#define DECLARE_HYDROGEN_ACCESSOR(type) \
  H##type* hydrogen() const { return H##type::cast(hydrogen_value()); }

class LInstruction : public ZoneObject {
 public:
  LInstruction()
      : environment_(NULL),
        hydrogen_value_(NULL),
        bit_field_(IsCallBits::encode(false)) {}

  virtual ~LInstruction() {}

  virtual void CompileToNative(LCodeGen* generator) = 0;
  virtual const char* Mnemonic() const = 0;
  virtual void PrintTo(StringStream* stream);
  virtual void PrintDataTo(StringStream* stream);
  virtual void PrintOutputOperandTo(StringStream* stream);

  enum Opcode {
// Declare a unique enum value for each instruction.
#define DECLARE_OPCODE(type) k##type,
    LITHIUM_CONCRETE_INSTRUCTION_LIST(DECLARE_OPCODE) kNumberOfInstructions
#undef DECLARE_OPCODE
  };

  virtual Opcode opcode() const = 0;

// Declare non-virtual type testers for all leaf IR classes.
#define DECLARE_PREDICATE(type) \
  bool Is##type() const { return opcode() == k##type; }
  LITHIUM_CONCRETE_INSTRUCTION_LIST(DECLARE_PREDICATE)
#undef DECLARE_PREDICATE

  // Declare virtual predicates for instructions that don't have
  // an opcode.
  virtual bool IsGap() const { return false; }

  virtual bool IsControl() const { return false; }

  // Try deleting this instruction if possible.
  virtual bool TryDelete() { return false; }

  void set_environment(LEnvironment* env) { environment_ = env; }
  LEnvironment* environment() const { return environment_; }
  bool HasEnvironment() const { return environment_ != NULL; }

  void set_pointer_map(LPointerMap* p) { pointer_map_.set(p); }
  LPointerMap* pointer_map() const { return pointer_map_.get(); }
  bool HasPointerMap() const { return pointer_map_.is_set(); }

  void set_hydrogen_value(HValue* value) { hydrogen_value_ = value; }
  HValue* hydrogen_value() const { return hydrogen_value_; }

  void MarkAsCall() { bit_field_ = IsCallBits::update(bit_field_, true); }
  bool IsCall() const { return IsCallBits::decode(bit_field_); }

  void MarkAsSyntacticTailCall() {
    bit_field_ = IsSyntacticTailCallBits::update(bit_field_, true);
  }
  bool IsSyntacticTailCall() const {
    return IsSyntacticTailCallBits::decode(bit_field_);
  }

  // Interface to the register allocator and iterators.
  bool ClobbersTemps() const { return IsCall(); }
  bool ClobbersRegisters() const { return IsCall(); }
  virtual bool ClobbersDoubleRegisters(Isolate* isolate) const {
    return IsCall();
  }

  // Interface to the register allocator and iterators.
  bool IsMarkedAsCall() const { return IsCall(); }

  virtual bool HasResult() const = 0;
  virtual LOperand* result() const = 0;

  LOperand* FirstInput() { return InputAt(0); }
  LOperand* Output() { return HasResult() ? result() : NULL; }

  virtual bool HasInterestingComment(LCodeGen* gen) const { return true; }

#ifdef DEBUG
  void VerifyCall();
#endif

  virtual int InputCount() = 0;
  virtual LOperand* InputAt(int i) = 0;

 private:
  // Iterator support.
  friend class InputIterator;

  friend class TempIterator;
  virtual int TempCount() = 0;
  virtual LOperand* TempAt(int i) = 0;

  class IsCallBits : public BitField<bool, 0, 1> {};
  class IsSyntacticTailCallBits : public BitField<bool, IsCallBits::kNext, 1> {
  };

  LEnvironment* environment_;
  SetOncePointer<LPointerMap> pointer_map_;
  HValue* hydrogen_value_;
  int bit_field_;
};

// R = number of result operands (0 or 1).
template <int R>
class LTemplateResultInstruction : public LInstruction {
 public:
  // Allow 0 or 1 output operands.
  STATIC_ASSERT(R == 0 || R == 1);
  bool HasResult() const final { return R != 0 && result() != NULL; }
  void set_result(LOperand* operand) { results_[0] = operand; }
  LOperand* result() const override { return results_[0]; }

 protected:
  EmbeddedContainer<LOperand*, R> results_;
};

// R = number of result operands (0 or 1).
// I = number of input operands.
// T = number of temporary operands.
template <int R, int I, int T>
class LTemplateInstruction : public LTemplateResultInstruction<R> {
 protected:
  EmbeddedContainer<LOperand*, I> inputs_;
  EmbeddedContainer<LOperand*, T> temps_;

 private:
  // Iterator support.
  int InputCount() final { return I; }
  LOperand* InputAt(int i) final { return inputs_[i]; }

  int TempCount() final { return T; }
  LOperand* TempAt(int i) final { return temps_[i]; }
};

class LGap : public LTemplateInstruction<0, 0, 0> {
 public:
  explicit LGap(HBasicBlock* block) : block_(block) {
    parallel_moves_[BEFORE] = NULL;
    parallel_moves_[START] = NULL;
    parallel_moves_[END] = NULL;
    parallel_moves_[AFTER] = NULL;
  }

  // Can't use the DECLARE-macro here because of sub-classes.
  bool IsGap() const override { return true; }
  void PrintDataTo(StringStream* stream) override;
  static LGap* cast(LInstruction* instr) {
    DCHECK(instr->IsGap());
    return reinterpret_cast<LGap*>(instr);
  }

  bool IsRedundant() const;

  HBasicBlock* block() const { return block_; }

  enum InnerPosition {
    BEFORE,
    START,
    END,
    AFTER,
    FIRST_INNER_POSITION = BEFORE,
    LAST_INNER_POSITION = AFTER
  };

  LParallelMove* GetOrCreateParallelMove(InnerPosition pos, Zone* zone) {
    if (parallel_moves_[pos] == NULL) {
      parallel_moves_[pos] = new (zone) LParallelMove(zone);
    }
    return parallel_moves_[pos];
  }

  LParallelMove* GetParallelMove(InnerPosition pos) {
    return parallel_moves_[pos];
  }

 private:
  LParallelMove* parallel_moves_[LAST_INNER_POSITION + 1];
  HBasicBlock* block_;
};

class LInstructionGap final : public LGap {
 public:
  explicit LInstructionGap(HBasicBlock* block) : LGap(block) {}

  bool HasInterestingComment(LCodeGen* gen) const override {
    return !IsRedundant();
  }

  DECLARE_CONCRETE_INSTRUCTION(InstructionGap, "\x67\x61\x70")
};

class LGoto final : public LTemplateInstruction<0, 0, 0> {
 public:
  explicit LGoto(HBasicBlock* block) : block_(block) {}

  bool HasInterestingComment(LCodeGen* gen) const override;
  DECLARE_CONCRETE_INSTRUCTION(Goto, "\x67\x6f\x74\x6f")
  void PrintDataTo(StringStream* stream) override;
  bool IsControl() const override { return true; }

  int block_id() const { return block_->block_id(); }

 private:
  HBasicBlock* block_;
};

class LPrologue final : public LTemplateInstruction<0, 0, 0> {
 public:
  DECLARE_CONCRETE_INSTRUCTION(Prologue, "\x70\x72\x6f\x6c\x6f\x67\x75\x65")
};

class LLazyBailout final : public LTemplateInstruction<0, 0, 0> {
 public:
  LLazyBailout() : gap_instructions_size_(0) {}

  DECLARE_CONCRETE_INSTRUCTION(LazyBailout, "\x6c\x61\x7a\x79\x2d\x62\x61\x69\x6c\x6f\x75\x74")

  void set_gap_instructions_size(int gap_instructions_size) {
    gap_instructions_size_ = gap_instructions_size;
  }
  int gap_instructions_size() { return gap_instructions_size_; }

 private:
  int gap_instructions_size_;
};

class LDummy final : public LTemplateInstruction<1, 0, 0> {
 public:
  LDummy() {}
  DECLARE_CONCRETE_INSTRUCTION(Dummy, "\x64\x75\x6d\x6d\x79")
};

class LDummyUse final : public LTemplateInstruction<1, 1, 0> {
 public:
  explicit LDummyUse(LOperand* value) { inputs_[0] = value; }
  DECLARE_CONCRETE_INSTRUCTION(DummyUse, "\x64\x75\x6d\x6d\x79\x2d\x75\x73\x65")
};

class LDeoptimize final : public LTemplateInstruction<0, 0, 0> {
 public:
  bool IsControl() const override { return true; }
  DECLARE_CONCRETE_INSTRUCTION(Deoptimize, "\x64\x65\x6f\x70\x74\x69\x6d\x69\x7a\x65")
  DECLARE_HYDROGEN_ACCESSOR(Deoptimize)
};

class LLabel final : public LGap {
 public:
  explicit LLabel(HBasicBlock* block) : LGap(block), replacement_(NULL) {}

  bool HasInterestingComment(LCodeGen* gen) const override { return false; }
  DECLARE_CONCRETE_INSTRUCTION(Label, "\x6c\x61\x62\x65\x6c")

  void PrintDataTo(StringStream* stream) override;

  int block_id() const { return block()->block_id(); }
  bool is_loop_header() const { return block()->IsLoopHeader(); }
  bool is_osr_entry() const { return block()->is_osr_entry(); }
  Label* label() { return &label_; }
  LLabel* replacement() const { return replacement_; }
  void set_replacement(LLabel* label) { replacement_ = label; }
  bool HasReplacement() const { return replacement_ != NULL; }

 private:
  Label label_;
  LLabel* replacement_;
};

class LParameter final : public LTemplateInstruction<1, 0, 0> {
 public:
  virtual bool HasInterestingComment(LCodeGen* gen) const { return false; }
  DECLARE_CONCRETE_INSTRUCTION(Parameter, "\x70\x61\x72\x61\x6d\x65\x74\x65\x72")
};

class LUnknownOSRValue final : public LTemplateInstruction<1, 0, 0> {
 public:
  bool HasInterestingComment(LCodeGen* gen) const override { return false; }
  DECLARE_CONCRETE_INSTRUCTION(UnknownOSRValue, "\x75\x6e\x6b\x6e\x6f\x77\x6e\x2d\x6f\x73\x72\x2d\x76\x61\x6c\x75\x65")
};

template <int I, int T>
class LControlInstruction : public LTemplateInstruction<0, I, T> {
 public:
  LControlInstruction() : false_label_(NULL), true_label_(NULL) {}

  bool IsControl() const final { return true; }

  int SuccessorCount() { return hydrogen()->SuccessorCount(); }
  HBasicBlock* SuccessorAt(int i) { return hydrogen()->SuccessorAt(i); }

  int TrueDestination(LChunk* chunk) {
    return chunk->LookupDestination(true_block_id());
  }
  int FalseDestination(LChunk* chunk) {
    return chunk->LookupDestination(false_block_id());
  }

  Label* TrueLabel(LChunk* chunk) {
    if (true_label_ == NULL) {
      true_label_ = chunk->GetAssemblyLabel(TrueDestination(chunk));
    }
    return true_label_;
  }
  Label* FalseLabel(LChunk* chunk) {
    if (false_label_ == NULL) {
      false_label_ = chunk->GetAssemblyLabel(FalseDestination(chunk));
    }
    return false_label_;
  }

 protected:
  int true_block_id() { return SuccessorAt(0)->block_id(); }
  int false_block_id() { return SuccessorAt(1)->block_id(); }

 private:
  HControlInstruction* hydrogen() {
    return HControlInstruction::cast(this->hydrogen_value());
  }

  Label* false_label_;
  Label* true_label_;
};

class LWrapReceiver final : public LTemplateInstruction<1, 2, 0> {
 public:
  LWrapReceiver(LOperand* receiver, LOperand* function) {
    inputs_[0] = receiver;
    inputs_[1] = function;
  }

  DECLARE_CONCRETE_INSTRUCTION(WrapReceiver, "\x77\x72\x61\x70\x2d\x72\x65\x63\x65\x69\x76\x65\x72")
  DECLARE_HYDROGEN_ACCESSOR(WrapReceiver)

  LOperand* receiver() { return inputs_[0]; }
  LOperand* function() { return inputs_[1]; }
};

class LApplyArguments final : public LTemplateInstruction<1, 4, 0> {
 public:
  LApplyArguments(LOperand* function, LOperand* receiver, LOperand* length,
                  LOperand* elements) {
    inputs_[0] = function;
    inputs_[1] = receiver;
    inputs_[2] = length;
    inputs_[3] = elements;
  }

  DECLARE_CONCRETE_INSTRUCTION(ApplyArguments, "\x61\x70\x70\x6c\x79\x2d\x61\x72\x67\x75\x6d\x65\x6e\x74\x73")
  DECLARE_HYDROGEN_ACCESSOR(ApplyArguments)

  LOperand* function() { return inputs_[0]; }
  LOperand* receiver() { return inputs_[1]; }
  LOperand* length() { return inputs_[2]; }
  LOperand* elements() { return inputs_[3]; }
};

class LAccessArgumentsAt final : public LTemplateInstruction<1, 3, 0> {
 public:
  LAccessArgumentsAt(LOperand* arguments, LOperand* length, LOperand* index) {
    inputs_[0] = arguments;
    inputs_[1] = length;
    inputs_[2] = index;
  }

  DECLARE_CONCRETE_INSTRUCTION(AccessArgumentsAt, "\x61\x63\x63\x65\x73\x73\x2d\x61\x72\x67\x75\x6d\x65\x6e\x74\x73\x2d\x61\x74")

  LOperand* arguments() { return inputs_[0]; }
  LOperand* length() { return inputs_[1]; }
  LOperand* index() { return inputs_[2]; }

  void PrintDataTo(StringStream* stream) override;
};

class LArgumentsLength final : public LTemplateInstruction<1, 1, 0> {
 public:
  explicit LArgumentsLength(LOperand* elements) { inputs_[0] = elements; }

  LOperand* elements() { return inputs_[0]; }

  DECLARE_CONCRETE_INSTRUCTION(ArgumentsLength, "\x61\x72\x67\x75\x6d\x65\x6e\x74\x73\x2d\x6c\x65\x6e\x67\x74\x68")
};

class LArgumentsElements final : public LTemplateInstruction<1, 0, 0> {
 public:
  DECLARE_CONCRETE_INSTRUCTION(ArgumentsElements, "\x61\x72\x67\x75\x6d\x65\x6e\x74\x73\x2d\x65\x6c\x65\x6d\x65\x6e\x74\x73")
  DECLARE_HYDROGEN_ACCESSOR(ArgumentsElements)
};

class LModByPowerOf2I final : public LTemplateInstruction<1, 1, 0> {
 public:
  LModByPowerOf2I(LOperand* dividend, int32_t divisor) {
    inputs_[0] = dividend;
    divisor_ = divisor;
  }

  LOperand* dividend() { return inputs_[0]; }
  int32_t divisor() const { return divisor_; }

  DECLARE_CONCRETE_INSTRUCTION(ModByPowerOf2I, "\x6d\x6f\x64\x2d\x62\x79\x2d\x70\x6f\x77\x65\x72\x2d\x6f\x66\x2d\x32\x2d\x69")
  DECLARE_HYDROGEN_ACCESSOR(Mod)

 private:
  int32_t divisor_;
};

class LModByConstI final : public LTemplateInstruction<1, 1, 0> {
 public:
  LModByConstI(LOperand* dividend, int32_t divisor) {
    inputs_[0] = dividend;
    divisor_ = divisor;
  }

  LOperand* dividend() { return inputs_[0]; }
  int32_t divisor() const { return divisor_; }

  DECLARE_CONCRETE_INSTRUCTION(ModByConstI, "\x6d\x6f\x64\x2d\x62\x79\x2d\x63\x6f\x6e\x73\x74\x2d\x69")
  DECLARE_HYDROGEN_ACCESSOR(Mod)

 private:
  int32_t divisor_;
};

class LModI final : public LTemplateInstruction<1, 2, 0> {
 public:
  LModI(LOperand* left, LOperand* right) {
    inputs_[0] = left;
    inputs_[1] = right;
  }

  LOperand* left() { return inputs_[0]; }
  LOperand* right() { return inputs_[1]; }

  DECLARE_CONCRETE_INSTRUCTION(ModI, "\x6d\x6f\x64\x2d\x69")
  DECLARE_HYDROGEN_ACCESSOR(Mod)
};

class LDivByPowerOf2I final : public LTemplateInstruction<1, 1, 0> {
 public:
  LDivByPowerOf2I(LOperand* dividend, int32_t divisor) {
    inputs_[0] = dividend;
    divisor_ = divisor;
  }

  LOperand* dividend() { return inputs_[0]; }
  int32_t divisor() const { return divisor_; }

  DECLARE_CONCRETE_INSTRUCTION(DivByPowerOf2I, "\x64\x69\x76\x2d\x62\x79\x2d\x70\x6f\x77\x65\x72\x2d\x6f\x66\x2d\x32\x2d\x69")
  DECLARE_HYDROGEN_ACCESSOR(Div)

 private:
  int32_t divisor_;
};

class LDivByConstI final : public LTemplateInstruction<1, 1, 0> {
 public:
  LDivByConstI(LOperand* dividend, int32_t divisor) {
    inputs_[0] = dividend;
    divisor_ = divisor;
  }

  LOperand* dividend() { return inputs_[0]; }
  int32_t divisor() const { return divisor_; }

  DECLARE_CONCRETE_INSTRUCTION(DivByConstI, "\x64\x69\x76\x2d\x62\x79\x2d\x63\x6f\x6e\x73\x74\x2d\x69")
  DECLARE_HYDROGEN_ACCESSOR(Div)

 private:
  int32_t divisor_;
};

class LDivI final : public LTemplateInstruction<1, 2, 0> {
 public:
  LDivI(LOperand* dividend, LOperand* divisor) {
    inputs_[0] = dividend;
    inputs_[1] = divisor;
  }

  LOperand* dividend() { return inputs_[0]; }
  LOperand* divisor() { return inputs_[1]; }

  DECLARE_CONCRETE_INSTRUCTION(DivI, "\x64\x69\x76\x2d\x69")
  DECLARE_HYDROGEN_ACCESSOR(BinaryOperation)
};

class LFlooringDivByPowerOf2I final : public LTemplateInstruction<1, 1, 0> {
 public:
  LFlooringDivByPowerOf2I(LOperand* dividend, int32_t divisor) {
    inputs_[0] = dividend;
    divisor_ = divisor;
  }

  LOperand* dividend() { return inputs_[0]; }
  int32_t divisor() { return divisor_; }

  DECLARE_CONCRETE_INSTRUCTION(FlooringDivByPowerOf2I,
                               "\x66\x6c\x6f\x6f\x72\x69\x6e\x67\x2d\x64\x69\x76\x2d\x62\x79\x2d\x70\x6f\x77\x65\x72\x2d\x6f\x66\x2d\x32\x2d\x69")
  DECLARE_HYDROGEN_ACCESSOR(MathFloorOfDiv)

 private:
  int32_t divisor_;
};

class LFlooringDivByConstI final : public LTemplateInstruction<1, 1, 1> {
 public:
  LFlooringDivByConstI(LOperand* dividend, int32_t divisor, LOperand* temp) {
    inputs_[0] = dividend;
    divisor_ = divisor;
    temps_[0] = temp;
  }

  LOperand* dividend() { return inputs_[0]; }
  int32_t divisor() const { return divisor_; }
  LOperand* temp() { return temps_[0]; }

  DECLARE_CONCRETE_INSTRUCTION(FlooringDivByConstI, "\x66\x6c\x6f\x6f\x72\x69\x6e\x67\x2d\x64\x69\x76\x2d\x62\x79\x2d\x63\x6f\x6e\x73\x74\x2d\x69")
  DECLARE_HYDROGEN_ACCESSOR(MathFloorOfDiv)

 private:
  int32_t divisor_;
};

class LFlooringDivI final : public LTemplateInstruction<1, 2, 0> {
 public:
  LFlooringDivI(LOperand* dividend, LOperand* divisor) {
    inputs_[0] = dividend;
    inputs_[1] = divisor;
  }

  LOperand* dividend() { return inputs_[0]; }
  LOperand* divisor() { return inputs_[1]; }

  DECLARE_CONCRETE_INSTRUCTION(FlooringDivI, "\x66\x6c\x6f\x6f\x72\x69\x6e\x67\x2d\x64\x69\x76\x2d\x69")
  DECLARE_HYDROGEN_ACCESSOR(MathFloorOfDiv)
};

class LMulI final : public LTemplateInstruction<1, 2, 0> {
 public:
  LMulI(LOperand* left, LOperand* right) {
    inputs_[0] = left;
    inputs_[1] = right;
  }

  LOperand* left() { return inputs_[0]; }
  LOperand* right() { return inputs_[1]; }

  DECLARE_CONCRETE_INSTRUCTION(MulI, "\x6d\x75\x6c\x2d\x69")
  DECLARE_HYDROGEN_ACCESSOR(Mul)
};

// Instruction for computing multiplier * multiplicand + addend.
class LMultiplyAddD final : public LTemplateInstruction<1, 3, 0> {
 public:
  LMultiplyAddD(LOperand* addend, LOperand* multiplier,
                LOperand* multiplicand) {
    inputs_[0] = addend;
    inputs_[1] = multiplier;
    inputs_[2] = multiplicand;
  }

  LOperand* addend() { return inputs_[0]; }
  LOperand* multiplier() { return inputs_[1]; }
  LOperand* multiplicand() { return inputs_[2]; }

  DECLARE_CONCRETE_INSTRUCTION(MultiplyAddD, "\x6d\x75\x6c\x74\x69\x70\x6c\x79\x2d\x61\x64\x64\x2d\x64")
};

// Instruction for computing minuend - multiplier * multiplicand.
class LMultiplySubD final : public LTemplateInstruction<1, 3, 0> {
 public:
  LMultiplySubD(LOperand* minuend, LOperand* multiplier,
                LOperand* multiplicand) {
    inputs_[0] = minuend;
    inputs_[1] = multiplier;
    inputs_[2] = multiplicand;
  }

  LOperand* minuend() { return inputs_[0]; }
  LOperand* multiplier() { return inputs_[1]; }
  LOperand* multiplicand() { return inputs_[2]; }

  DECLARE_CONCRETE_INSTRUCTION(MultiplySubD, "\x6d\x75\x6c\x74\x69\x70\x6c\x79\x2d\x73\x75\x62\x2d\x64")
};

class LDebugBreak final : public LTemplateInstruction<0, 0, 0> {
 public:
  DECLARE_CONCRETE_INSTRUCTION(DebugBreak, "\x62\x72\x65\x61\x6b")
};

class LCompareNumericAndBranch final : public LControlInstruction<2, 0> {
 public:
  LCompareNumericAndBranch(LOperand* left, LOperand* right) {
    inputs_[0] = left;
    inputs_[1] = right;
  }

  LOperand* left() { return inputs_[0]; }
  LOperand* right() { return inputs_[1]; }

  DECLARE_CONCRETE_INSTRUCTION(CompareNumericAndBranch,
                               "\x63\x6f\x6d\x70\x61\x72\x65\x2d\x6e\x75\x6d\x65\x72\x69\x63\x2d\x61\x6e\x64\x2d\x62\x72\x61\x6e\x63\x68")
  DECLARE_HYDROGEN_ACCESSOR(CompareNumericAndBranch)

  Token::Value op() const { return hydrogen()->token(); }
  bool is_double() const { return hydrogen()->representation().IsDouble(); }

  void PrintDataTo(StringStream* stream) override;
};

class LMathFloor final : public LTemplateInstruction<1, 1, 0> {
 public:
  explicit LMathFloor(LOperand* value) { inputs_[0] = value; }

  LOperand* value() { return inputs_[0]; }

  DECLARE_CONCRETE_INSTRUCTION(MathFloor, "\x6d\x61\x74\x68\x2d\x66\x6c\x6f\x6f\x72")
  DECLARE_HYDROGEN_ACCESSOR(UnaryMathOperation)
};

class LMathRound final : public LTemplateInstruction<1, 1, 1> {
 public:
  LMathRound(LOperand* value, LOperand* temp) {
    inputs_[0] = value;
    temps_[0] = temp;
  }

  LOperand* value() { return inputs_[0]; }
  LOperand* temp() { return temps_[0]; }

  DECLARE_CONCRETE_INSTRUCTION(MathRound, "\x6d\x61\x74\x68\x2d\x72\x6f\x75\x6e\x64")
  DECLARE_HYDROGEN_ACCESSOR(UnaryMathOperation)
};

class LMathFround final : public LTemplateInstruction<1, 1, 0> {
 public:
  explicit LMathFround(LOperand* value) { inputs_[0] = value; }

  LOperand* value() { return inputs_[0]; }

  DECLARE_CONCRETE_INSTRUCTION(MathFround, "\x6d\x61\x74\x68\x2d\x66\x72\x6f\x75\x6e\x64")
};

class LMathAbs final : public LTemplateInstruction<1, 2, 0> {
 public:
  LMathAbs(LOperand* context, LOperand* value) {
    inputs_[1] = context;
    inputs_[0] = value;
  }

  LOperand* context() { return inputs_[1]; }
  LOperand* value() { return inputs_[0]; }

  DECLARE_CONCRETE_INSTRUCTION(MathAbs, "\x6d\x61\x74\x68\x2d\x61\x62\x73")
  DECLARE_HYDROGEN_ACCESSOR(UnaryMathOperation)
};

class LMathLog final : public LTemplateInstruction<1, 1, 0> {
 public:
  explicit LMathLog(LOperand* value) { inputs_[0] = value; }

  LOperand* value() { return inputs_[0]; }

  DECLARE_CONCRETE_INSTRUCTION(MathLog, "\x6d\x61\x74\x68\x2d\x6c\x6f\x67")
};

class LMathClz32 final : public LTemplateInstruction<1, 1, 0> {
 public:
  explicit LMathClz32(LOperand* value) { inputs_[0] = value; }

  LOperand* value() { return inputs_[0]; }

  DECLARE_CONCRETE_INSTRUCTION(MathClz32, "\x6d\x61\x74\x68\x2d\x63\x6c\x7a\x33\x32")
};

class LMathExp final : public LTemplateInstruction<1, 1, 3> {
 public:
  LMathExp(LOperand* value, LOperand* double_temp, LOperand* temp1,
           LOperand* temp2) {
    inputs_[0] = value;
    temps_[0] = temp1;
    temps_[1] = temp2;
    temps_[2] = double_temp;
    ExternalReference::InitializeMathExpData();
  }

  LOperand* value() { return inputs_[0]; }
  LOperand* temp1() { return temps_[0]; }
  LOperand* temp2() { return temps_[1]; }
  LOperand* double_temp() { return temps_[2]; }

  DECLARE_CONCRETE_INSTRUCTION(MathExp, "\x6d\x61\x74\x68\x2d\x65\x78\x70")
};

class LMathSqrt final : public LTemplateInstruction<1, 1, 0> {
 public:
  explicit LMathSqrt(LOperand* value) { inputs_[0] = value; }

  LOperand* value() { return inputs_[0]; }

  DECLARE_CONCRETE_INSTRUCTION(MathSqrt, "\x6d\x61\x74\x68\x2d\x73\x71\x72\x74")
};

class LMathPowHalf final : public LTemplateInstruction<1, 1, 0> {
 public:
  explicit LMathPowHalf(LOperand* value) { inputs_[0] = value; }

  LOperand* value() { return inputs_[0]; }

  DECLARE_CONCRETE_INSTRUCTION(MathPowHalf, "\x6d\x61\x74\x68\x2d\x70\x6f\x77\x2d\x68\x61\x6c\x66")
};

class LCmpObjectEqAndBranch final : public LControlInstruction<2, 0> {
 public:
  LCmpObjectEqAndBranch(LOperand* left, LOperand* right) {
    inputs_[0] = left;
    inputs_[1] = right;
  }

  LOperand* left() { return inputs_[0]; }
  LOperand* right() { return inputs_[1]; }

  DECLARE_CONCRETE_INSTRUCTION(CmpObjectEqAndBranch, "\x63\x6d\x70\x2d\x6f\x62\x6a\x65\x63\x74\x2d\x65\x71\x2d\x61\x6e\x64\x2d\x62\x72\x61\x6e\x63\x68")
  DECLARE_HYDROGEN_ACCESSOR(CompareObjectEqAndBranch)
};

class LCmpHoleAndBranch final : public LControlInstruction<1, 0> {
 public:
  explicit LCmpHoleAndBranch(LOperand* object) { inputs_[0] = object; }

  LOperand* object() { return inputs_[0]; }

  DECLARE_CONCRETE_INSTRUCTION(CmpHoleAndBranch, "\x63\x6d\x70\x2d\x68\x6f\x6c\x65\x2d\x61\x6e\x64\x2d\x62\x72\x61\x6e\x63\x68")
  DECLARE_HYDROGEN_ACCESSOR(CompareHoleAndBranch)
};

class LIsStringAndBranch final : public LControlInstruction<1, 1> {
 public:
  LIsStringAndBranch(LOperand* value, LOperand* temp) {
    inputs_[0] = value;
    temps_[0] = temp;
  }

  LOperand* value() { return inputs_[0]; }
  LOperand* temp() { return temps_[0]; }

  DECLARE_CONCRETE_INSTRUCTION(IsStringAndBranch, "\x69\x73\x2d\x73\x74\x72\x69\x6e\x67\x2d\x61\x6e\x64\x2d\x62\x72\x61\x6e\x63\x68")
  DECLARE_HYDROGEN_ACCESSOR(IsStringAndBranch)

  void PrintDataTo(StringStream* stream) override;
};

class LIsSmiAndBranch final : public LControlInstruction<1, 0> {
 public:
  explicit LIsSmiAndBranch(LOperand* value) { inputs_[0] = value; }

  LOperand* value() { return inputs_[0]; }

  DECLARE_CONCRETE_INSTRUCTION(IsSmiAndBranch, "\x69\x73\x2d\x73\x6d\x69\x2d\x61\x6e\x64\x2d\x62\x72\x61\x6e\x63\x68")
  DECLARE_HYDROGEN_ACCESSOR(IsSmiAndBranch)

  void PrintDataTo(StringStream* stream) override;
};

class LIsUndetectableAndBranch final : public LControlInstruction<1, 1> {
 public:
  explicit LIsUndetectableAndBranch(LOperand* value, LOperand* temp) {
    inputs_[0] = value;
    temps_[0] = temp;
  }

  LOperand* value() { return inputs_[0]; }
  LOperand* temp() { return temps_[0]; }

  DECLARE_CONCRETE_INSTRUCTION(IsUndetectableAndBranch,
                               "\x69\x73\x2d\x75\x6e\x64\x65\x74\x65\x63\x74\x61\x62\x6c\x65\x2d\x61\x6e\x64\x2d\x62\x72\x61\x6e\x63\x68")
  DECLARE_HYDROGEN_ACCESSOR(IsUndetectableAndBranch)

  void PrintDataTo(StringStream* stream) override;
};

class LStringCompareAndBranch final : public LControlInstruction<3, 0> {
 public:
  LStringCompareAndBranch(LOperand* context, LOperand* left, LOperand* right) {
    inputs_[0] = context;
    inputs_[1] = left;
    inputs_[2] = right;
  }

  LOperand* context() { return inputs_[0]; }
  LOperand* left() { return inputs_[1]; }
  LOperand* right() { return inputs_[2]; }

  DECLARE_CONCRETE_INSTRUCTION(StringCompareAndBranch,
                               "\x73\x74\x72\x69\x6e\x67\x2d\x63\x6f\x6d\x70\x61\x72\x65\x2d\x61\x6e\x64\x2d\x62\x72\x61\x6e\x63\x68")
  DECLARE_HYDROGEN_ACCESSOR(StringCompareAndBranch)

  Token::Value op() const { return hydrogen()->token(); }

  void PrintDataTo(StringStream* stream) override;
};

class LHasInstanceTypeAndBranch final : public LControlInstruction<1, 0> {
 public:
  explicit LHasInstanceTypeAndBranch(LOperand* value) { inputs_[0] = value; }

  LOperand* value() { return inputs_[0]; }

  DECLARE_CONCRETE_INSTRUCTION(HasInstanceTypeAndBranch,
                               "\x68\x61\x73\x2d\x69\x6e\x73\x74\x61\x6e\x63\x65\x2d\x74\x79\x70\x65\x2d\x61\x6e\x64\x2d\x62\x72\x61\x6e\x63\x68")
  DECLARE_HYDROGEN_ACCESSOR(HasInstanceTypeAndBranch)

  void PrintDataTo(StringStream* stream) override;
};

class LGetCachedArrayIndex final : public LTemplateInstruction<1, 1, 0> {
 public:
  explicit LGetCachedArrayIndex(LOperand* value) { inputs_[0] = value; }

  LOperand* value() { return inputs_[0]; }

  DECLARE_CONCRETE_INSTRUCTION(GetCachedArrayIndex, "\x67\x65\x74\x2d\x63\x61\x63\x68\x65\x64\x2d\x61\x72\x72\x61\x79\x2d\x69\x6e\x64\x65\x78")
  DECLARE_HYDROGEN_ACCESSOR(GetCachedArrayIndex)
};

class LHasCachedArrayIndexAndBranch final : public LControlInstruction<1, 0> {
 public:
  explicit LHasCachedArrayIndexAndBranch(LOperand* value) {
    inputs_[0] = value;
  }

  LOperand* value() { return inputs_[0]; }

  DECLARE_CONCRETE_INSTRUCTION(HasCachedArrayIndexAndBranch,
                               "\x68\x61\x73\x2d\x63\x61\x63\x68\x65\x64\x2d\x61\x72\x72\x61\x79\x2d\x69\x6e\x64\x65\x78\x2d\x61\x6e\x64\x2d\x62\x72\x61\x6e\x63\x68")
  DECLARE_HYDROGEN_ACCESSOR(HasCachedArrayIndexAndBranch)

  void PrintDataTo(StringStream* stream) override;
};

class LClassOfTestAndBranch final : public LControlInstruction<1, 1> {
 public:
  LClassOfTestAndBranch(LOperand* value, LOperand* temp) {
    inputs_[0] = value;
    temps_[0] = temp;
  }

  LOperand* value() { return inputs_[0]; }
  LOperand* temp() { return temps_[0]; }

  DECLARE_CONCRETE_INSTRUCTION(ClassOfTestAndBranch, "\x63\x6c\x61\x73\x73\x2d\x6f\x66\x2d\x74\x65\x73\x74\x2d\x61\x6e\x64\x2d\x62\x72\x61\x6e\x63\x68")
  DECLARE_HYDROGEN_ACCESSOR(ClassOfTestAndBranch)

  void PrintDataTo(StringStream* stream) override;
};

class LCmpT final : public LTemplateInstruction<1, 3, 0> {
 public:
  LCmpT(LOperand* context, LOperand* left, LOperand* right) {
    inputs_[0] = context;
    inputs_[1] = left;
    inputs_[2] = right;
  }

  LOperand* context() { return inputs_[0]; }
  LOperand* left() { return inputs_[1]; }
  LOperand* right() { return inputs_[2]; }

  DECLARE_CONCRETE_INSTRUCTION(CmpT, "\x63\x6d\x70\x2d\x74")
  DECLARE_HYDROGEN_ACCESSOR(CompareGeneric)

  Token::Value op() const { return hydrogen()->token(); }
};

class LInstanceOf final : public LTemplateInstruction<1, 3, 0> {
 public:
  LInstanceOf(LOperand* context, LOperand* left, LOperand* right) {
    inputs_[0] = context;
    inputs_[1] = left;
    inputs_[2] = right;
  }

  LOperand* context() const { return inputs_[0]; }
  LOperand* left() const { return inputs_[1]; }
  LOperand* right() const { return inputs_[2]; }

  DECLARE_CONCRETE_INSTRUCTION(InstanceOf, "\x69\x6e\x73\x74\x61\x6e\x63\x65\x2d\x6f\x66")
};

class LHasInPrototypeChainAndBranch final : public LControlInstruction<2, 0> {
 public:
  LHasInPrototypeChainAndBranch(LOperand* object, LOperand* prototype) {
    inputs_[0] = object;
    inputs_[1] = prototype;
  }

  LOperand* object() const { return inputs_[0]; }
  LOperand* prototype() const { return inputs_[1]; }

  DECLARE_CONCRETE_INSTRUCTION(HasInPrototypeChainAndBranch,
                               "\x68\x61\x73\x2d\x69\x6e\x2d\x70\x72\x6f\x74\x6f\x74\x79\x70\x65\x2d\x63\x68\x61\x69\x6e\x2d\x61\x6e\x64\x2d\x62\x72\x61\x6e\x63\x68")
  DECLARE_HYDROGEN_ACCESSOR(HasInPrototypeChainAndBranch)
};

class LBoundsCheck final : public LTemplateInstruction<0, 2, 0> {
 public:
  LBoundsCheck(LOperand* index, LOperand* length) {
    inputs_[0] = index;
    inputs_[1] = length;
  }

  LOperand* index() { return inputs_[0]; }
  LOperand* length() { return inputs_[1]; }

  DECLARE_CONCRETE_INSTRUCTION(BoundsCheck, "\x62\x6f\x75\x6e\x64\x73\x2d\x63\x68\x65\x63\x6b")
  DECLARE_HYDROGEN_ACCESSOR(BoundsCheck)
};

class LBitI final : public LTemplateInstruction<1, 2, 0> {
 public:
  LBitI(LOperand* left, LOperand* right) {
    inputs_[0] = left;
    inputs_[1] = right;
  }

  LOperand* left() { return inputs_[0]; }
  LOperand* right() { return inputs_[1]; }

  Token::Value op() const { return hydrogen()->op(); }

  DECLARE_CONCRETE_INSTRUCTION(BitI, "\x62\x69\x74\x2d\x69")
  DECLARE_HYDROGEN_ACCESSOR(Bitwise)
};

class LShiftI final : public LTemplateInstruction<1, 2, 0> {
 public:
  LShiftI(Token::Value op, LOperand* left, LOperand* right, bool can_deopt)
      : op_(op), can_deopt_(can_deopt) {
    inputs_[0] = left;
    inputs_[1] = right;
  }

  Token::Value op() const { return op_; }
  LOperand* left() { return inputs_[0]; }
  LOperand* right() { return inputs_[1]; }
  bool can_deopt() const { return can_deopt_; }

  DECLARE_CONCRETE_INSTRUCTION(ShiftI, "\x73\x68\x69\x66\x74\x2d\x69")

 private:
  Token::Value op_;
  bool can_deopt_;
};

class LSubI final : public LTemplateInstruction<1, 2, 0> {
 public:
  LSubI(LOperand* left, LOperand* right) {
    inputs_[0] = left;
    inputs_[1] = right;
  }

  LOperand* left() { return inputs_[0]; }
  LOperand* right() { return inputs_[1]; }

  DECLARE_CONCRETE_INSTRUCTION(SubI, "\x73\x75\x62\x2d\x69")
  DECLARE_HYDROGEN_ACCESSOR(Sub)
};

class LRSubI final : public LTemplateInstruction<1, 2, 0> {
 public:
  LRSubI(LOperand* left, LOperand* right) {
    inputs_[0] = left;
    inputs_[1] = right;
  }

  LOperand* left() { return inputs_[0]; }
  LOperand* right() { return inputs_[1]; }

  DECLARE_CONCRETE_INSTRUCTION(RSubI, "\x72\x73\x75\x62\x2d\x69")
  DECLARE_HYDROGEN_ACCESSOR(Sub)
};

class LConstantI final : public LTemplateInstruction<1, 0, 0> {
 public:
  DECLARE_CONCRETE_INSTRUCTION(ConstantI, "\x63\x6f\x6e\x73\x74\x61\x6e\x74\x2d\x69")
  DECLARE_HYDROGEN_ACCESSOR(Constant)

  int32_t value() const { return hydrogen()->Integer32Value(); }
};

class LConstantS final : public LTemplateInstruction<1, 0, 0> {
 public:
  DECLARE_CONCRETE_INSTRUCTION(ConstantS, "\x63\x6f\x6e\x73\x74\x61\x6e\x74\x2d\x73")
  DECLARE_HYDROGEN_ACCESSOR(Constant)

  Smi* value() const { return Smi::FromInt(hydrogen()->Integer32Value()); }
};

class LConstantD final : public LTemplateInstruction<1, 0, 0> {
 public:
  DECLARE_CONCRETE_INSTRUCTION(ConstantD, "\x63\x6f\x6e\x73\x74\x61\x6e\x74\x2d\x64")
  DECLARE_HYDROGEN_ACCESSOR(Constant)

  double value() const { return hydrogen()->DoubleValue(); }

  uint64_t bits() const { return hydrogen()->DoubleValueAsBits(); }
};

class LConstantE final : public LTemplateInstruction<1, 0, 0> {
 public:
  DECLARE_CONCRETE_INSTRUCTION(ConstantE, "\x63\x6f\x6e\x73\x74\x61\x6e\x74\x2d\x65")
  DECLARE_HYDROGEN_ACCESSOR(Constant)

  ExternalReference value() const {
    return hydrogen()->ExternalReferenceValue();
  }
};

class LConstantT final : public LTemplateInstruction<1, 0, 0> {
 public:
  DECLARE_CONCRETE_INSTRUCTION(ConstantT, "\x63\x6f\x6e\x73\x74\x61\x6e\x74\x2d\x74")
  DECLARE_HYDROGEN_ACCESSOR(Constant)

  Handle<Object> value(Isolate* isolate) const {
    return hydrogen()->handle(isolate);
  }
};

class LBranch final : public LControlInstruction<1, 0> {
 public:
  explicit LBranch(LOperand* value) { inputs_[0] = value; }

  LOperand* value() { return inputs_[0]; }

  DECLARE_CONCRETE_INSTRUCTION(Branch, "\x62\x72\x61\x6e\x63\x68")
  DECLARE_HYDROGEN_ACCESSOR(Branch)

  void PrintDataTo(StringStream* stream) override;
};

class LCmpMapAndBranch final : public LControlInstruction<1, 1> {
 public:
  LCmpMapAndBranch(LOperand* value, LOperand* temp) {
    inputs_[0] = value;
    temps_[0] = temp;
  }

  LOperand* value() { return inputs_[0]; }
  LOperand* temp() { return temps_[0]; }

  DECLARE_CONCRETE_INSTRUCTION(CmpMapAndBranch, "\x63\x6d\x70\x2d\x6d\x61\x70\x2d\x61\x6e\x64\x2d\x62\x72\x61\x6e\x63\x68")
  DECLARE_HYDROGEN_ACCESSOR(CompareMap)

  Handle<Map> map() const { return hydrogen()->map().handle(); }
};

class LSeqStringGetChar final : public LTemplateInstruction<1, 2, 0> {
 public:
  LSeqStringGetChar(LOperand* string, LOperand* index) {
    inputs_[0] = string;
    inputs_[1] = index;
  }

  LOperand* string() const { return inputs_[0]; }
  LOperand* index() const { return inputs_[1]; }

  DECLARE_CONCRETE_INSTRUCTION(SeqStringGetChar, "\x73\x65\x71\x2d\x73\x74\x72\x69\x6e\x67\x2d\x67\x65\x74\x2d\x63\x68\x61\x72")
  DECLARE_HYDROGEN_ACCESSOR(SeqStringGetChar)
};

class LSeqStringSetChar final : public LTemplateInstruction<1, 4, 0> {
 public:
  LSeqStringSetChar(LOperand* context, LOperand* string, LOperand* index,
                    LOperand* value) {
    inputs_[0] = context;
    inputs_[1] = string;
    inputs_[2] = index;
    inputs_[3] = value;
  }

  LOperand* string() { return inputs_[1]; }
  LOperand* index() { return inputs_[2]; }
  LOperand* value() { return inputs_[3]; }

  DECLARE_CONCRETE_INSTRUCTION(SeqStringSetChar, "\x73\x65\x71\x2d\x73\x74\x72\x69\x6e\x67\x2d\x73\x65\x74\x2d\x63\x68\x61\x72")
  DECLARE_HYDROGEN_ACCESSOR(SeqStringSetChar)
};

class LAddI final : public LTemplateInstruction<1, 2, 0> {
 public:
  LAddI(LOperand* left, LOperand* right) {
    inputs_[0] = left;
    inputs_[1] = right;
  }

  LOperand* left() { return inputs_[0]; }
  LOperand* right() { return inputs_[1]; }

  DECLARE_CONCRETE_INSTRUCTION(AddI, "\x61\x64\x64\x2d\x69")
  DECLARE_HYDROGEN_ACCESSOR(Add)
};

class LMathMinMax final : public LTemplateInstruction<1, 2, 0> {
 public:
  LMathMinMax(LOperand* left, LOperand* right) {
    inputs_[0] = left;
    inputs_[1] = right;
  }

  LOperand* left() { return inputs_[0]; }
  LOperand* right() { return inputs_[1]; }

  DECLARE_CONCRETE_INSTRUCTION(MathMinMax, "\x6d\x61\x74\x68\x2d\x6d\x69\x6e\x2d\x6d\x61\x78")
  DECLARE_HYDROGEN_ACCESSOR(MathMinMax)
};

class LPower final : public LTemplateInstruction<1, 2, 0> {
 public:
  LPower(LOperand* left, LOperand* right) {
    inputs_[0] = left;
    inputs_[1] = right;
  }

  LOperand* left() { return inputs_[0]; }
  LOperand* right() { return inputs_[1]; }

  DECLARE_CONCRETE_INSTRUCTION(Power, "\x70\x6f\x77\x65\x72")
  DECLARE_HYDROGEN_ACCESSOR(Power)
};

class LArithmeticD final : public LTemplateInstruction<1, 2, 0> {
 public:
  LArithmeticD(Token::Value op, LOperand* left, LOperand* right) : op_(op) {
    inputs_[0] = left;
    inputs_[1] = right;
  }

  Token::Value op() const { return op_; }
  LOperand* left() { return inputs_[0]; }
  LOperand* right() { return inputs_[1]; }

  Opcode opcode() const override { return LInstruction::kArithmeticD; }
  void CompileToNative(LCodeGen* generator) override;
  const char* Mnemonic() const override;

 private:
  Token::Value op_;
};

class LArithmeticT final : public LTemplateInstruction<1, 3, 0> {
 public:
  LArithmeticT(Token::Value op, LOperand* context, LOperand* left,
               LOperand* right)
      : op_(op) {
    inputs_[0] = context;
    inputs_[1] = left;
    inputs_[2] = right;
  }

  LOperand* context() { return inputs_[0]; }
  LOperand* left() { return inputs_[1]; }
  LOperand* right() { return inputs_[2]; }
  Token::Value op() const { return op_; }

  Opcode opcode() const override { return LInstruction::kArithmeticT; }
  void CompileToNative(LCodeGen* generator) override;
  const char* Mnemonic() const override;

  DECLARE_HYDROGEN_ACCESSOR(BinaryOperation)

 private:
  Token::Value op_;
};

class LReturn final : public LTemplateInstruction<0, 3, 0> {
 public:
  LReturn(LOperand* value, LOperand* context, LOperand* parameter_count) {
    inputs_[0] = value;
    inputs_[1] = context;
    inputs_[2] = parameter_count;
  }

  LOperand* value() { return inputs_[0]; }

  bool has_constant_parameter_count() {
    return parameter_count()->IsConstantOperand();
  }
  LConstantOperand* constant_parameter_count() {
    DCHECK(has_constant_parameter_count());
    return LConstantOperand::cast(parameter_count());
  }
  LOperand* parameter_count() { return inputs_[2]; }

  DECLARE_CONCRETE_INSTRUCTION(Return, "\x72\x65\x74\x75\x72\x6e")
};

class LLoadNamedField final : public LTemplateInstruction<1, 1, 0> {
 public:
  explicit LLoadNamedField(LOperand* object) { inputs_[0] = object; }

  LOperand* object() { return inputs_[0]; }

  DECLARE_CONCRETE_INSTRUCTION(LoadNamedField, "\x6c\x6f\x61\x64\x2d\x6e\x61\x6d\x65\x64\x2d\x66\x69\x65\x6c\x64")
  DECLARE_HYDROGEN_ACCESSOR(LoadNamedField)
};

class LLoadNamedGeneric final : public LTemplateInstruction<1, 2, 1> {
 public:
  LLoadNamedGeneric(LOperand* context, LOperand* object, LOperand* vector) {
    inputs_[0] = context;
    inputs_[1] = object;
    temps_[0] = vector;
  }

  LOperand* context() { return inputs_[0]; }
  LOperand* object() { return inputs_[1]; }
  LOperand* temp_vector() { return temps_[0]; }

  DECLARE_CONCRETE_INSTRUCTION(LoadNamedGeneric, "\x6c\x6f\x61\x64\x2d\x6e\x61\x6d\x65\x64\x2d\x67\x65\x6e\x65\x72\x69\x63")
  DECLARE_HYDROGEN_ACCESSOR(LoadNamedGeneric)

  Handle<Object> name() const { return hydrogen()->name(); }
};

class LLoadFunctionPrototype final : public LTemplateInstruction<1, 1, 0> {
 public:
  explicit LLoadFunctionPrototype(LOperand* function) { inputs_[0] = function; }

  LOperand* function() { return inputs_[0]; }

  DECLARE_CONCRETE_INSTRUCTION(LoadFunctionPrototype, "\x6c\x6f\x61\x64\x2d\x66\x75\x6e\x63\x74\x69\x6f\x6e\x2d\x70\x72\x6f\x74\x6f\x74\x79\x70\x65")
  DECLARE_HYDROGEN_ACCESSOR(LoadFunctionPrototype)
};

class LLoadRoot final : public LTemplateInstruction<1, 0, 0> {
 public:
  DECLARE_CONCRETE_INSTRUCTION(LoadRoot, "\x6c\x6f\x61\x64\x2d\x72\x6f\x6f\x74")
  DECLARE_HYDROGEN_ACCESSOR(LoadRoot)

  Heap::RootListIndex index() const { return hydrogen()->index(); }
};

class LLoadKeyed final : public LTemplateInstruction<1, 3, 0> {
 public:
  LLoadKeyed(LOperand* elements, LOperand* key, LOperand* backing_store_owner) {
    inputs_[0] = elements;
    inputs_[1] = key;
    inputs_[2] = backing_store_owner;
  }

  LOperand* elements() { return inputs_[0]; }
  LOperand* key() { return inputs_[1]; }
  LOperand* backing_store_owner() { return inputs_[2]; }
  ElementsKind elements_kind() const { return hydrogen()->elements_kind(); }
  bool is_fixed_typed_array() const {
    return hydrogen()->is_fixed_typed_array();
  }

  DECLARE_CONCRETE_INSTRUCTION(LoadKeyed, "\x6c\x6f\x61\x64\x2d\x6b\x65\x79\x65\x64")
  DECLARE_HYDROGEN_ACCESSOR(LoadKeyed)

  void PrintDataTo(StringStream* stream) override;
  uint32_t base_offset() const { return hydrogen()->base_offset(); }
};

class LLoadKeyedGeneric final : public LTemplateInstruction<1, 3, 1> {
 public:
  LLoadKeyedGeneric(LOperand* context, LOperand* object, LOperand* key,
                    LOperand* vector) {
    inputs_[0] = context;
    inputs_[1] = object;
    inputs_[2] = key;
    temps_[0] = vector;
  }

  LOperand* context() { return inputs_[0]; }
  LOperand* object() { return inputs_[1]; }
  LOperand* key() { return inputs_[2]; }
  LOperand* temp_vector() { return temps_[0]; }

  DECLARE_CONCRETE_INSTRUCTION(LoadKeyedGeneric, "\x6c\x6f\x61\x64\x2d\x6b\x65\x79\x65\x64\x2d\x67\x65\x6e\x65\x72\x69\x63")
  DECLARE_HYDROGEN_ACCESSOR(LoadKeyedGeneric)
};

class LLoadGlobalGeneric final : public LTemplateInstruction<1, 2, 1> {
 public:
  LLoadGlobalGeneric(LOperand* context, LOperand* global_object,
                     LOperand* vector) {
    inputs_[0] = context;
    inputs_[1] = global_object;
    temps_[0] = vector;
  }

  LOperand* context() { return inputs_[0]; }
  LOperand* global_object() { return inputs_[1]; }
  LOperand* temp_vector() { return temps_[0]; }

  DECLARE_CONCRETE_INSTRUCTION(LoadGlobalGeneric, "\x6c\x6f\x61\x64\x2d\x67\x6c\x6f\x62\x61\x6c\x2d\x67\x65\x6e\x65\x72\x69\x63")
  DECLARE_HYDROGEN_ACCESSOR(LoadGlobalGeneric)

  Handle<Object> name() const { return hydrogen()->name(); }
  TypeofMode typeof_mode() const { return hydrogen()->typeof_mode(); }
};

class LLoadContextSlot final : public LTemplateInstruction<1, 1, 0> {
 public:
  explicit LLoadContextSlot(LOperand* context) { inputs_[0] = context; }

  LOperand* context() { return inputs_[0]; }

  DECLARE_CONCRETE_INSTRUCTION(LoadContextSlot, "\x6c\x6f\x61\x64\x2d\x63\x6f\x6e\x74\x65\x78\x74\x2d\x73\x6c\x6f\x74")
  DECLARE_HYDROGEN_ACCESSOR(LoadContextSlot)

  int slot_index() { return hydrogen()->slot_index(); }

  void PrintDataTo(StringStream* stream) override;
};

class LStoreContextSlot final : public LTemplateInstruction<0, 2, 0> {
 public:
  LStoreContextSlot(LOperand* context, LOperand* value) {
    inputs_[0] = context;
    inputs_[1] = value;
  }

  LOperand* context() { return inputs_[0]; }
  LOperand* value() { return inputs_[1]; }

  DECLARE_CONCRETE_INSTRUCTION(StoreContextSlot, "\x73\x74\x6f\x72\x65\x2d\x63\x6f\x6e\x74\x65\x78\x74\x2d\x73\x6c\x6f\x74")
  DECLARE_HYDROGEN_ACCESSOR(StoreContextSlot)

  int slot_index() { return hydrogen()->slot_index(); }

  void PrintDataTo(StringStream* stream) override;
};

class LPushArgument final : public LTemplateInstruction<0, 1, 0> {
 public:
  explicit LPushArgument(LOperand* value) { inputs_[0] = value; }

  LOperand* value() { return inputs_[0]; }

  DECLARE_CONCRETE_INSTRUCTION(PushArgument, "\x70\x75\x73\x68\x2d\x61\x72\x67\x75\x6d\x65\x6e\x74")
};

class LDrop final : public LTemplateInstruction<0, 0, 0> {
 public:
  explicit LDrop(int count) : count_(count) {}

  int count() const { return count_; }

  DECLARE_CONCRETE_INSTRUCTION(Drop, "\x64\x72\x6f\x70")

 private:
  int count_;
};

class LStoreCodeEntry final : public LTemplateInstruction<0, 2, 0> {
 public:
  LStoreCodeEntry(LOperand* function, LOperand* code_object) {
    inputs_[0] = function;
    inputs_[1] = code_object;
  }

  LOperand* function() { return inputs_[0]; }
  LOperand* code_object() { return inputs_[1]; }

  void PrintDataTo(StringStream* stream) override;

  DECLARE_CONCRETE_INSTRUCTION(StoreCodeEntry, "\x73\x74\x6f\x72\x65\x2d\x63\x6f\x64\x65\x2d\x65\x6e\x74\x72\x79")
  DECLARE_HYDROGEN_ACCESSOR(StoreCodeEntry)
};

class LInnerAllocatedObject final : public LTemplateInstruction<1, 2, 0> {
 public:
  LInnerAllocatedObject(LOperand* base_object, LOperand* offset) {
    inputs_[0] = base_object;
    inputs_[1] = offset;
  }

  LOperand* base_object() const { return inputs_[0]; }
  LOperand* offset() const { return inputs_[1]; }

  void PrintDataTo(StringStream* stream) override;

  DECLARE_CONCRETE_INSTRUCTION(InnerAllocatedObject, "\x69\x6e\x6e\x65\x72\x2d\x61\x6c\x6c\x6f\x63\x61\x74\x65\x64\x2d\x6f\x62\x6a\x65\x63\x74")
};

class LThisFunction final : public LTemplateInstruction<1, 0, 0> {
 public:
  DECLARE_CONCRETE_INSTRUCTION(ThisFunction, "\x74\x68\x69\x73\x2d\x66\x75\x6e\x63\x74\x69\x6f\x6e")
  DECLARE_HYDROGEN_ACCESSOR(ThisFunction)
};

class LContext final : public LTemplateInstruction<1, 0, 0> {
 public:
  DECLARE_CONCRETE_INSTRUCTION(Context, "\x63\x6f\x6e\x74\x65\x78\x74")
  DECLARE_HYDROGEN_ACCESSOR(Context)
};

class LDeclareGlobals final : public LTemplateInstruction<0, 1, 0> {
 public:
  explicit LDeclareGlobals(LOperand* context) { inputs_[0] = context; }

  LOperand* context() { return inputs_[0]; }

  DECLARE_CONCRETE_INSTRUCTION(DeclareGlobals, "\x64\x65\x63\x6c\x61\x72\x65\x2d\x67\x6c\x6f\x62\x61\x6c\x73")
  DECLARE_HYDROGEN_ACCESSOR(DeclareGlobals)
};

class LCallWithDescriptor final : public LTemplateResultInstruction<1> {
 public:
  LCallWithDescriptor(CallInterfaceDescriptor descriptor,
                      const ZoneList<LOperand*>& operands, Zone* zone)
      : descriptor_(descriptor),
        inputs_(descriptor.GetRegisterParameterCount() +
                    kImplicitRegisterParameterCount,
                zone) {
    DCHECK(descriptor.GetRegisterParameterCount() +
               kImplicitRegisterParameterCount ==
           operands.length());
    inputs_.AddAll(operands, zone);
  }

  LOperand* target() const { return inputs_[0]; }

  const CallInterfaceDescriptor descriptor() { return descriptor_; }

  DECLARE_HYDROGEN_ACCESSOR(CallWithDescriptor)

  // The target and context are passed as implicit parameters that are not
  // explicitly listed in the descriptor.
  static const int kImplicitRegisterParameterCount = 2;

 private:
  DECLARE_CONCRETE_INSTRUCTION(CallWithDescriptor, "\x63\x61\x6c\x6c\x2d\x77\x69\x74\x68\x2d\x64\x65\x73\x63\x72\x69\x70\x74\x6f\x72")

  void PrintDataTo(StringStream* stream) override;

  int arity() const { return hydrogen()->argument_count() - 1; }

  CallInterfaceDescriptor descriptor_;
  ZoneList<LOperand*> inputs_;

  // Iterator support.
  int InputCount() final { return inputs_.length(); }
  LOperand* InputAt(int i) final { return inputs_[i]; }

  int TempCount() final { return 0; }
  LOperand* TempAt(int i) final { return NULL; }
};

class LInvokeFunction final : public LTemplateInstruction<1, 2, 0> {
 public:
  LInvokeFunction(LOperand* context, LOperand* function) {
    inputs_[0] = context;
    inputs_[1] = function;
  }

  LOperand* context() { return inputs_[0]; }
  LOperand* function() { return inputs_[1]; }

  DECLARE_CONCRETE_INSTRUCTION(InvokeFunction, "\x69\x6e\x76\x6f\x6b\x65\x2d\x66\x75\x6e\x63\x74\x69\x6f\x6e")
  DECLARE_HYDROGEN_ACCESSOR(InvokeFunction)

  void PrintDataTo(StringStream* stream) override;

  int arity() const { return hydrogen()->argument_count() - 1; }
};

class LCallNewArray final : public LTemplateInstruction<1, 2, 0> {
 public:
  LCallNewArray(LOperand* context, LOperand* constructor) {
    inputs_[0] = context;
    inputs_[1] = constructor;
  }

  LOperand* context() { return inputs_[0]; }
  LOperand* constructor() { return inputs_[1]; }

  DECLARE_CONCRETE_INSTRUCTION(CallNewArray, "\x63\x61\x6c\x6c\x2d\x6e\x65\x77\x2d\x61\x72\x72\x61\x79")
  DECLARE_HYDROGEN_ACCESSOR(CallNewArray)

  void PrintDataTo(StringStream* stream) override;

  int arity() const { return hydrogen()->argument_count() - 1; }
};

class LCallRuntime final : public LTemplateInstruction<1, 1, 0> {
 public:
  explicit LCallRuntime(LOperand* context) { inputs_[0] = context; }

  LOperand* context() { return inputs_[0]; }

  DECLARE_CONCRETE_INSTRUCTION(CallRuntime, "\x63\x61\x6c\x6c\x2d\x72\x75\x6e\x74\x69\x6d\x65")
  DECLARE_HYDROGEN_ACCESSOR(CallRuntime)

  bool ClobbersDoubleRegisters(Isolate* isolate) const override {
    return save_doubles() == kDontSaveFPRegs;
  }

  const Runtime::Function* function() const { return hydrogen()->function(); }
  int arity() const { return hydrogen()->argument_count(); }
  SaveFPRegsMode save_doubles() const { return hydrogen()->save_doubles(); }
};

class LInteger32ToDouble final : public LTemplateInstruction<1, 1, 0> {
 public:
  explicit LInteger32ToDouble(LOperand* value) { inputs_[0] = value; }

  LOperand* value() { return inputs_[0]; }

  DECLARE_CONCRETE_INSTRUCTION(Integer32ToDouble, "\x69\x6e\x74\x33\x32\x2d\x74\x6f\x2d\x64\x6f\x75\x62\x6c\x65")
};

class LUint32ToDouble final : public LTemplateInstruction<1, 1, 0> {
 public:
  explicit LUint32ToDouble(LOperand* value) { inputs_[0] = value; }

  LOperand* value() { return inputs_[0]; }

  DECLARE_CONCRETE_INSTRUCTION(Uint32ToDouble, "\x75\x69\x6e\x74\x33\x32\x2d\x74\x6f\x2d\x64\x6f\x75\x62\x6c\x65")
};

class LNumberTagI final : public LTemplateInstruction<1, 1, 2> {
 public:
  LNumberTagI(LOperand* value, LOperand* temp1, LOperand* temp2) {
    inputs_[0] = value;
    temps_[0] = temp1;
    temps_[1] = temp2;
  }

  LOperand* value() { return inputs_[0]; }
  LOperand* temp1() { return temps_[0]; }
  LOperand* temp2() { return temps_[1]; }

  DECLARE_CONCRETE_INSTRUCTION(NumberTagI, "\x6e\x75\x6d\x62\x65\x72\x2d\x74\x61\x67\x2d\x69")
};

class LNumberTagU final : public LTemplateInstruction<1, 1, 2> {
 public:
  LNumberTagU(LOperand* value, LOperand* temp1, LOperand* temp2) {
    inputs_[0] = value;
    temps_[0] = temp1;
    temps_[1] = temp2;
  }

  LOperand* value() { return inputs_[0]; }
  LOperand* temp1() { return temps_[0]; }
  LOperand* temp2() { return temps_[1]; }

  DECLARE_CONCRETE_INSTRUCTION(NumberTagU, "\x6e\x75\x6d\x62\x65\x72\x2d\x74\x61\x67\x2d\x75")
};

class LNumberTagD final : public LTemplateInstruction<1, 1, 2> {
 public:
  LNumberTagD(LOperand* value, LOperand* temp, LOperand* temp2) {
    inputs_[0] = value;
    temps_[0] = temp;
    temps_[1] = temp2;
  }

  LOperand* value() { return inputs_[0]; }
  LOperand* temp() { return temps_[0]; }
  LOperand* temp2() { return temps_[1]; }

  DECLARE_CONCRETE_INSTRUCTION(NumberTagD, "\x6e\x75\x6d\x62\x65\x72\x2d\x74\x61\x67\x2d\x64")
  DECLARE_HYDROGEN_ACCESSOR(Change)
};

class LDoubleToSmi final : public LTemplateInstruction<1, 1, 0> {
 public:
  explicit LDoubleToSmi(LOperand* value) { inputs_[0] = value; }

  LOperand* value() { return inputs_[0]; }

  DECLARE_CONCRETE_INSTRUCTION(DoubleToSmi, "\x64\x6f\x75\x62\x6c\x65\x2d\x74\x6f\x2d\x73\x6d\x69")
  DECLARE_HYDROGEN_ACCESSOR(UnaryOperation)

  bool truncating() { return hydrogen()->CanTruncateToInt32(); }
};

// Sometimes truncating conversion from a tagged value to an int32.
class LDoubleToI final : public LTemplateInstruction<1, 1, 0> {
 public:
  explicit LDoubleToI(LOperand* value) { inputs_[0] = value; }

  LOperand* value() { return inputs_[0]; }

  DECLARE_CONCRETE_INSTRUCTION(DoubleToI, "\x64\x6f\x75\x62\x6c\x65\x2d\x74\x6f\x2d\x69")
  DECLARE_HYDROGEN_ACCESSOR(UnaryOperation)

  bool truncating() { return hydrogen()->CanTruncateToInt32(); }
};

// Truncating conversion from a tagged value to an int32.
class LTaggedToI final : public LTemplateInstruction<1, 1, 2> {
 public:
  LTaggedToI(LOperand* value, LOperand* temp, LOperand* temp2) {
    inputs_[0] = value;
    temps_[0] = temp;
    temps_[1] = temp2;
  }

  LOperand* value() { return inputs_[0]; }
  LOperand* temp() { return temps_[0]; }
  LOperand* temp2() { return temps_[1]; }

  DECLARE_CONCRETE_INSTRUCTION(TaggedToI, "\x74\x61\x67\x67\x65\x64\x2d\x74\x6f\x2d\x69")
  DECLARE_HYDROGEN_ACCESSOR(Change)

  bool truncating() { return hydrogen()->CanTruncateToInt32(); }
};

class LSmiTag final : public LTemplateInstruction<1, 1, 0> {
 public:
  explicit LSmiTag(LOperand* value) { inputs_[0] = value; }

  LOperand* value() { return inputs_[0]; }

  DECLARE_CONCRETE_INSTRUCTION(SmiTag, "\x73\x6d\x69\x2d\x74\x61\x67")
  DECLARE_HYDROGEN_ACCESSOR(Change)
};

class LNumberUntagD final : public LTemplateInstruction<1, 1, 0> {
 public:
  explicit LNumberUntagD(LOperand* value) { inputs_[0] = value; }

  LOperand* value() { return inputs_[0]; }

  DECLARE_CONCRETE_INSTRUCTION(NumberUntagD, "\x64\x6f\x75\x62\x6c\x65\x2d\x75\x6e\x74\x61\x67")
  DECLARE_HYDROGEN_ACCESSOR(Change)
};

class LSmiUntag final : public LTemplateInstruction<1, 1, 0> {
 public:
  LSmiUntag(LOperand* value, bool needs_check) : needs_check_(needs_check) {
    inputs_[0] = value;
  }

  LOperand* value() { return inputs_[0]; }
  bool needs_check() const { return needs_check_; }

  DECLARE_CONCRETE_INSTRUCTION(SmiUntag, "\x73\x6d\x69\x2d\x75\x6e\x74\x61\x67")

 private:
  bool needs_check_;
};

class LStoreNamedField final : public LTemplateInstruction<0, 2, 1> {
 public:
  LStoreNamedField(LOperand* object, LOperand* value, LOperand* temp) {
    inputs_[0] = object;
    inputs_[1] = value;
    temps_[0] = temp;
  }

  LOperand* object() { return inputs_[0]; }
  LOperand* value() { return inputs_[1]; }
  LOperand* temp() { return temps_[0]; }

  DECLARE_CONCRETE_INSTRUCTION(StoreNamedField, "\x73\x74\x6f\x72\x65\x2d\x6e\x61\x6d\x65\x64\x2d\x66\x69\x65\x6c\x64")
  DECLARE_HYDROGEN_ACCESSOR(StoreNamedField)

  void PrintDataTo(StringStream* stream) override;

  Representation representation() const {
    return hydrogen()->field_representation();
  }
};

class LStoreNamedGeneric final : public LTemplateInstruction<0, 3, 2> {
 public:
  LStoreNamedGeneric(LOperand* context, LOperand* object, LOperand* value,
                     LOperand* slot, LOperand* vector) {
    inputs_[0] = context;
    inputs_[1] = object;
    inputs_[2] = value;
    temps_[0] = slot;
    temps_[1] = vector;
  }

  LOperand* context() { return inputs_[0]; }
  LOperand* object() { return inputs_[1]; }
  LOperand* value() { return inputs_[2]; }
  LOperand* temp_slot() { return temps_[0]; }
  LOperand* temp_vector() { return temps_[1]; }

  DECLARE_CONCRETE_INSTRUCTION(StoreNamedGeneric, "\x73\x74\x6f\x72\x65\x2d\x6e\x61\x6d\x65\x64\x2d\x67\x65\x6e\x65\x72\x69\x63")
  DECLARE_HYDROGEN_ACCESSOR(StoreNamedGeneric)

  void PrintDataTo(StringStream* stream) override;

  Handle<Object> name() const { return hydrogen()->name(); }
  LanguageMode language_mode() { return hydrogen()->language_mode(); }
};

class LStoreKeyed final : public LTemplateInstruction<0, 4, 0> {
 public:
  LStoreKeyed(LOperand* object, LOperand* key, LOperand* value,
              LOperand* backing_store_owner) {
    inputs_[0] = object;
    inputs_[1] = key;
    inputs_[2] = value;
    inputs_[3] = backing_store_owner;
  }

  bool is_fixed_typed_array() const {
    return hydrogen()->is_fixed_typed_array();
  }
  LOperand* elements() { return inputs_[0]; }
  LOperand* key() { return inputs_[1]; }
  LOperand* value() { return inputs_[2]; }
  LOperand* backing_store_owner() { return inputs_[3]; }
  ElementsKind elements_kind() const { return hydrogen()->elements_kind(); }

  DECLARE_CONCRETE_INSTRUCTION(StoreKeyed, "\x73\x74\x6f\x72\x65\x2d\x6b\x65\x79\x65\x64")
  DECLARE_HYDROGEN_ACCESSOR(StoreKeyed)

  void PrintDataTo(StringStream* stream) override;
  bool NeedsCanonicalization() {
    if (hydrogen()->value()->IsAdd() || hydrogen()->value()->IsSub() ||
        hydrogen()->value()->IsMul() || hydrogen()->value()->IsDiv()) {
      return false;
    }
    return hydrogen()->NeedsCanonicalization();
  }
  uint32_t base_offset() const { return hydrogen()->base_offset(); }
};

class LStoreKeyedGeneric final : public LTemplateInstruction<0, 4, 2> {
 public:
  LStoreKeyedGeneric(LOperand* context, LOperand* object, LOperand* key,
                     LOperand* value, LOperand* slot, LOperand* vector) {
    inputs_[0] = context;
    inputs_[1] = object;
    inputs_[2] = key;
    inputs_[3] = value;
    temps_[0] = slot;
    temps_[1] = vector;
  }

  LOperand* context() { return inputs_[0]; }
  LOperand* object() { return inputs_[1]; }
  LOperand* key() { return inputs_[2]; }
  LOperand* value() { return inputs_[3]; }
  LOperand* temp_slot() { return temps_[0]; }
  LOperand* temp_vector() { return temps_[1]; }

  DECLARE_CONCRETE_INSTRUCTION(StoreKeyedGeneric, "\x73\x74\x6f\x72\x65\x2d\x6b\x65\x79\x65\x64\x2d\x67\x65\x6e\x65\x72\x69\x63")
  DECLARE_HYDROGEN_ACCESSOR(StoreKeyedGeneric)

  void PrintDataTo(StringStream* stream) override;

  LanguageMode language_mode() { return hydrogen()->language_mode(); }
};

class LTransitionElementsKind final : public LTemplateInstruction<0, 2, 1> {
 public:
  LTransitionElementsKind(LOperand* object, LOperand* context,
                          LOperand* new_map_temp) {
    inputs_[0] = object;
    inputs_[1] = context;
    temps_[0] = new_map_temp;
  }

  LOperand* context() { return inputs_[1]; }
  LOperand* object() { return inputs_[0]; }
  LOperand* new_map_temp() { return temps_[0]; }

  DECLARE_CONCRETE_INSTRUCTION(TransitionElementsKind,
                               "\x74\x72\x61\x6e\x73\x69\x74\x69\x6f\x6e\x2d\x65\x6c\x65\x6d\x65\x6e\x74\x73\x2d\x6b\x69\x6e\x64")
  DECLARE_HYDROGEN_ACCESSOR(TransitionElementsKind)

  void PrintDataTo(StringStream* stream) override;

  Handle<Map> original_map() { return hydrogen()->original_map().handle(); }
  Handle<Map> transitioned_map() {
    return hydrogen()->transitioned_map().handle();
  }
  ElementsKind from_kind() { return hydrogen()->from_kind(); }
  ElementsKind to_kind() { return hydrogen()->to_kind(); }
};

class LTrapAllocationMemento final : public LTemplateInstruction<0, 1, 2> {
 public:
  LTrapAllocationMemento(LOperand* object, LOperand* temp1, LOperand* temp2) {
    inputs_[0] = object;
    temps_[0] = temp1;
    temps_[1] = temp2;
  }

  LOperand* object() { return inputs_[0]; }
  LOperand* temp1() { return temps_[0]; }
  LOperand* temp2() { return temps_[1]; }

  DECLARE_CONCRETE_INSTRUCTION(TrapAllocationMemento, "\x74\x72\x61\x70\x2d\x61\x6c\x6c\x6f\x63\x61\x74\x69\x6f\x6e\x2d\x6d\x65\x6d\x65\x6e\x74\x6f")
};

class LMaybeGrowElements final : public LTemplateInstruction<1, 5, 0> {
 public:
  LMaybeGrowElements(LOperand* context, LOperand* object, LOperand* elements,
                     LOperand* key, LOperand* current_capacity) {
    inputs_[0] = context;
    inputs_[1] = object;
    inputs_[2] = elements;
    inputs_[3] = key;
    inputs_[4] = current_capacity;
  }

  LOperand* context() { return inputs_[0]; }
  LOperand* object() { return inputs_[1]; }
  LOperand* elements() { return inputs_[2]; }
  LOperand* key() { return inputs_[3]; }
  LOperand* current_capacity() { return inputs_[4]; }

  DECLARE_HYDROGEN_ACCESSOR(MaybeGrowElements)
  DECLARE_CONCRETE_INSTRUCTION(MaybeGrowElements, "\x6d\x61\x79\x62\x65\x2d\x67\x72\x6f\x77\x2d\x65\x6c\x65\x6d\x65\x6e\x74\x73")
};

class LStringAdd final : public LTemplateInstruction<1, 3, 0> {
 public:
  LStringAdd(LOperand* context, LOperand* left, LOperand* right) {
    inputs_[0] = context;
    inputs_[1] = left;
    inputs_[2] = right;
  }

  LOperand* context() { return inputs_[0]; }
  LOperand* left() { return inputs_[1]; }
  LOperand* right() { return inputs_[2]; }

  DECLARE_CONCRETE_INSTRUCTION(StringAdd, "\x73\x74\x72\x69\x6e\x67\x2d\x61\x64\x64")
  DECLARE_HYDROGEN_ACCESSOR(StringAdd)
};

class LStringCharCodeAt final : public LTemplateInstruction<1, 3, 0> {
 public:
  LStringCharCodeAt(LOperand* context, LOperand* string, LOperand* index) {
    inputs_[0] = context;
    inputs_[1] = string;
    inputs_[2] = index;
  }

  LOperand* context() { return inputs_[0]; }
  LOperand* string() { return inputs_[1]; }
  LOperand* index() { return inputs_[2]; }

  DECLARE_CONCRETE_INSTRUCTION(StringCharCodeAt, "\x73\x74\x72\x69\x6e\x67\x2d\x63\x68\x61\x72\x2d\x63\x6f\x64\x65\x2d\x61\x74")
  DECLARE_HYDROGEN_ACCESSOR(StringCharCodeAt)
};

class LStringCharFromCode final : public LTemplateInstruction<1, 2, 0> {
 public:
  explicit LStringCharFromCode(LOperand* context, LOperand* char_code) {
    inputs_[0] = context;
    inputs_[1] = char_code;
  }

  LOperand* context() { return inputs_[0]; }
  LOperand* char_code() { return inputs_[1]; }

  DECLARE_CONCRETE_INSTRUCTION(StringCharFromCode, "\x73\x74\x72\x69\x6e\x67\x2d\x63\x68\x61\x72\x2d\x66\x72\x6f\x6d\x2d\x63\x6f\x64\x65")
  DECLARE_HYDROGEN_ACCESSOR(StringCharFromCode)
};

class LCheckValue final : public LTemplateInstruction<0, 1, 0> {
 public:
  explicit LCheckValue(LOperand* value) { inputs_[0] = value; }

  LOperand* value() { return inputs_[0]; }

  DECLARE_CONCRETE_INSTRUCTION(CheckValue, "\x63\x68\x65\x63\x6b\x2d\x76\x61\x6c\x75\x65")
  DECLARE_HYDROGEN_ACCESSOR(CheckValue)
};

class LCheckArrayBufferNotNeutered final
    : public LTemplateInstruction<0, 1, 0> {
 public:
  explicit LCheckArrayBufferNotNeutered(LOperand* view) { inputs_[0] = view; }

  LOperand* view() { return inputs_[0]; }

  DECLARE_CONCRETE_INSTRUCTION(CheckArrayBufferNotNeutered,
                               "\x63\x68\x65\x63\x6b\x2d\x61\x72\x72\x61\x79\x2d\x62\x75\x66\x66\x65\x72\x2d\x6e\x6f\x74\x2d\x6e\x65\x75\x74\x65\x72\x65\x64")
  DECLARE_HYDROGEN_ACCESSOR(CheckArrayBufferNotNeutered)
};

class LCheckInstanceType final : public LTemplateInstruction<0, 1, 0> {
 public:
  explicit LCheckInstanceType(LOperand* value) { inputs_[0] = value; }

  LOperand* value() { return inputs_[0]; }

  DECLARE_CONCRETE_INSTRUCTION(CheckInstanceType, "\x63\x68\x65\x63\x6b\x2d\x69\x6e\x73\x74\x61\x6e\x63\x65\x2d\x74\x79\x70\x65")
  DECLARE_HYDROGEN_ACCESSOR(CheckInstanceType)
};

class LCheckMaps final : public LTemplateInstruction<0, 1, 1> {
 public:
  explicit LCheckMaps(LOperand* value = NULL, LOperand* temp = NULL) {
    inputs_[0] = value;
    temps_[0] = temp;
  }

  LOperand* value() { return inputs_[0]; }
  LOperand* temp() { return temps_[0]; }

  DECLARE_CONCRETE_INSTRUCTION(CheckMaps, "\x63\x68\x65\x63\x6b\x2d\x6d\x61\x70\x73")
  DECLARE_HYDROGEN_ACCESSOR(CheckMaps)
};

class LCheckSmi final : public LTemplateInstruction<1, 1, 0> {
 public:
  explicit LCheckSmi(LOperand* value) { inputs_[0] = value; }

  LOperand* value() { return inputs_[0]; }

  DECLARE_CONCRETE_INSTRUCTION(CheckSmi, "\x63\x68\x65\x63\x6b\x2d\x73\x6d\x69")
};

class LCheckNonSmi final : public LTemplateInstruction<0, 1, 0> {
 public:
  explicit LCheckNonSmi(LOperand* value) { inputs_[0] = value; }

  LOperand* value() { return inputs_[0]; }

  DECLARE_CONCRETE_INSTRUCTION(CheckNonSmi, "\x63\x68\x65\x63\x6b\x2d\x6e\x6f\x6e\x2d\x73\x6d\x69")
  DECLARE_HYDROGEN_ACCESSOR(CheckHeapObject)
};

class LClampDToUint8 final : public LTemplateInstruction<1, 1, 0> {
 public:
  explicit LClampDToUint8(LOperand* unclamped) { inputs_[0] = unclamped; }

  LOperand* unclamped() { return inputs_[0]; }

  DECLARE_CONCRETE_INSTRUCTION(ClampDToUint8, "\x63\x6c\x61\x6d\x70\x2d\x64\x2d\x74\x6f\x2d\x75\x69\x6e\x74\x38")
};

class LClampIToUint8 final : public LTemplateInstruction<1, 1, 0> {
 public:
  explicit LClampIToUint8(LOperand* unclamped) { inputs_[0] = unclamped; }

  LOperand* unclamped() { return inputs_[0]; }

  DECLARE_CONCRETE_INSTRUCTION(ClampIToUint8, "\x63\x6c\x61\x6d\x70\x2d\x69\x2d\x74\x6f\x2d\x75\x69\x6e\x74\x38")
};

class LClampTToUint8 final : public LTemplateInstruction<1, 1, 1> {
 public:
  LClampTToUint8(LOperand* unclamped, LOperand* temp) {
    inputs_[0] = unclamped;
    temps_[0] = temp;
  }

  LOperand* unclamped() { return inputs_[0]; }
  LOperand* temp() { return temps_[0]; }

  DECLARE_CONCRETE_INSTRUCTION(ClampTToUint8, "\x63\x6c\x61\x6d\x70\x2d\x74\x2d\x74\x6f\x2d\x75\x69\x6e\x74\x38")
};

class LDoubleBits final : public LTemplateInstruction<1, 1, 0> {
 public:
  explicit LDoubleBits(LOperand* value) { inputs_[0] = value; }

  LOperand* value() { return inputs_[0]; }

  DECLARE_CONCRETE_INSTRUCTION(DoubleBits, "\x64\x6f\x75\x62\x6c\x65\x2d\x62\x69\x74\x73")
  DECLARE_HYDROGEN_ACCESSOR(DoubleBits)
};

class LConstructDouble final : public LTemplateInstruction<1, 2, 0> {
 public:
  LConstructDouble(LOperand* hi, LOperand* lo) {
    inputs_[0] = hi;
    inputs_[1] = lo;
  }

  LOperand* hi() { return inputs_[0]; }
  LOperand* lo() { return inputs_[1]; }

  DECLARE_CONCRETE_INSTRUCTION(ConstructDouble, "\x63\x6f\x6e\x73\x74\x72\x75\x63\x74\x2d\x64\x6f\x75\x62\x6c\x65")
};

class LAllocate final : public LTemplateInstruction<1, 2, 2> {
 public:
  LAllocate(LOperand* context, LOperand* size, LOperand* temp1,
            LOperand* temp2) {
    inputs_[0] = context;
    inputs_[1] = size;
    temps_[0] = temp1;
    temps_[1] = temp2;
  }

  LOperand* context() { return inputs_[0]; }
  LOperand* size() { return inputs_[1]; }
  LOperand* temp1() { return temps_[0]; }
  LOperand* temp2() { return temps_[1]; }

  DECLARE_CONCRETE_INSTRUCTION(Allocate, "\x61\x6c\x6c\x6f\x63\x61\x74\x65")
  DECLARE_HYDROGEN_ACCESSOR(Allocate)
};

class LTypeof final : public LTemplateInstruction<1, 2, 0> {
 public:
  LTypeof(LOperand* context, LOperand* value) {
    inputs_[0] = context;
    inputs_[1] = value;
  }

  LOperand* context() { return inputs_[0]; }
  LOperand* value() { return inputs_[1]; }

  DECLARE_CONCRETE_INSTRUCTION(Typeof, "\x74\x79\x70\x65\x6f\x66")
};

class LTypeofIsAndBranch final : public LControlInstruction<1, 0> {
 public:
  explicit LTypeofIsAndBranch(LOperand* value) { inputs_[0] = value; }

  LOperand* value() { return inputs_[0]; }

  DECLARE_CONCRETE_INSTRUCTION(TypeofIsAndBranch, "\x74\x79\x70\x65\x6f\x66\x2d\x69\x73\x2d\x61\x6e\x64\x2d\x62\x72\x61\x6e\x63\x68")
  DECLARE_HYDROGEN_ACCESSOR(TypeofIsAndBranch)

  Handle<String> type_literal() { return hydrogen()->type_literal(); }

  void PrintDataTo(StringStream* stream) override;
};

class LOsrEntry final : public LTemplateInstruction<0, 0, 0> {
 public:
  LOsrEntry() {}

  bool HasInterestingComment(LCodeGen* gen) const override { return false; }
  DECLARE_CONCRETE_INSTRUCTION(OsrEntry, "\x6f\x73\x72\x2d\x65\x6e\x74\x72\x79")
};

class LStackCheck final : public LTemplateInstruction<0, 1, 0> {
 public:
  explicit LStackCheck(LOperand* context) { inputs_[0] = context; }

  LOperand* context() { return inputs_[0]; }

  DECLARE_CONCRETE_INSTRUCTION(StackCheck, "\x73\x74\x61\x63\x6b\x2d\x63\x68\x65\x63\x6b")
  DECLARE_HYDROGEN_ACCESSOR(StackCheck)

  Label* done_label() { return &done_label_; }

 private:
  Label done_label_;
};

class LForInPrepareMap final : public LTemplateInstruction<1, 2, 0> {
 public:
  LForInPrepareMap(LOperand* context, LOperand* object) {
    inputs_[0] = context;
    inputs_[1] = object;
  }

  LOperand* context() { return inputs_[0]; }
  LOperand* object() { return inputs_[1]; }

  DECLARE_CONCRETE_INSTRUCTION(ForInPrepareMap, "\x66\x6f\x72\x2d\x69\x6e\x2d\x70\x72\x65\x70\x61\x72\x65\x2d\x6d\x61\x70")
};

class LForInCacheArray final : public LTemplateInstruction<1, 1, 0> {
 public:
  explicit LForInCacheArray(LOperand* map) { inputs_[0] = map; }

  LOperand* map() { return inputs_[0]; }

  DECLARE_CONCRETE_INSTRUCTION(ForInCacheArray, "\x66\x6f\x72\x2d\x69\x6e\x2d\x63\x61\x63\x68\x65\x2d\x61\x72\x72\x61\x79")

  int idx() { return HForInCacheArray::cast(this->hydrogen_value())->idx(); }
};

class LCheckMapValue final : public LTemplateInstruction<0, 2, 0> {
 public:
  LCheckMapValue(LOperand* value, LOperand* map) {
    inputs_[0] = value;
    inputs_[1] = map;
  }

  LOperand* value() { return inputs_[0]; }
  LOperand* map() { return inputs_[1]; }

  DECLARE_CONCRETE_INSTRUCTION(CheckMapValue, "\x63\x68\x65\x63\x6b\x2d\x6d\x61\x70\x2d\x76\x61\x6c\x75\x65")
};

class LLoadFieldByIndex final : public LTemplateInstruction<1, 2, 0> {
 public:
  LLoadFieldByIndex(LOperand* object, LOperand* index) {
    inputs_[0] = object;
    inputs_[1] = index;
  }

  LOperand* object() { return inputs_[0]; }
  LOperand* index() { return inputs_[1]; }

  DECLARE_CONCRETE_INSTRUCTION(LoadFieldByIndex, "\x6c\x6f\x61\x64\x2d\x66\x69\x65\x6c\x64\x2d\x62\x79\x2d\x69\x6e\x64\x65\x78")
};

class LChunkBuilder;
class LPlatformChunk final : public LChunk {
 public:
  LPlatformChunk(CompilationInfo* info, HGraph* graph) : LChunk(info, graph) {}

  int GetNextSpillIndex(RegisterKind kind);
  LOperand* GetNextSpillSlot(RegisterKind kind);
};

class LChunkBuilder final : public LChunkBuilderBase {
 public:
  LChunkBuilder(CompilationInfo* info, HGraph* graph, LAllocator* allocator)
      : LChunkBuilderBase(info, graph),
        current_instruction_(NULL),
        current_block_(NULL),
        next_block_(NULL),
        allocator_(allocator) {}

  // Build the sequence for the graph.
  LPlatformChunk* Build();

// Declare methods that deal with the individual node types.
#define DECLARE_DO(type) LInstruction* Do##type(H##type* node);
  HYDROGEN_CONCRETE_INSTRUCTION_LIST(DECLARE_DO)
#undef DECLARE_DO

  LInstruction* DoMultiplyAdd(HMul* mul, HValue* addend);
  LInstruction* DoMultiplySub(HValue* minuend, HMul* mul);
  LInstruction* DoRSub(HSub* instr);

  static bool HasMagicNumberForDivisor(int32_t divisor);

  LInstruction* DoMathFloor(HUnaryMathOperation* instr);
  LInstruction* DoMathRound(HUnaryMathOperation* instr);
  LInstruction* DoMathFround(HUnaryMathOperation* instr);
  LInstruction* DoMathAbs(HUnaryMathOperation* instr);
  LInstruction* DoMathLog(HUnaryMathOperation* instr);
  LInstruction* DoMathExp(HUnaryMathOperation* instr);
  LInstruction* DoMathSqrt(HUnaryMathOperation* instr);
  LInstruction* DoMathPowHalf(HUnaryMathOperation* instr);
  LInstruction* DoMathClz32(HUnaryMathOperation* instr);
  LInstruction* DoDivByPowerOf2I(HDiv* instr);
  LInstruction* DoDivByConstI(HDiv* instr);
  LInstruction* DoDivI(HDiv* instr);
  LInstruction* DoModByPowerOf2I(HMod* instr);
  LInstruction* DoModByConstI(HMod* instr);
  LInstruction* DoModI(HMod* instr);
  LInstruction* DoFlooringDivByPowerOf2I(HMathFloorOfDiv* instr);
  LInstruction* DoFlooringDivByConstI(HMathFloorOfDiv* instr);
  LInstruction* DoFlooringDivI(HMathFloorOfDiv* instr);

 private:
  // Methods for getting operands for Use / Define / Temp.
  LUnallocated* ToUnallocated(Register reg);
  LUnallocated* ToUnallocated(DoubleRegister reg);

  // Methods for setting up define-use relationships.
  MUST_USE_RESULT LOperand* Use(HValue* value, LUnallocated* operand);
  MUST_USE_RESULT LOperand* UseFixed(HValue* value, Register fixed_register);
  MUST_USE_RESULT LOperand* UseFixedDouble(HValue* value,
                                           DoubleRegister fixed_register);

  // A value that is guaranteed to be allocated to a register.
  // Operand created by UseRegister is guaranteed to be live until the end of
  // instruction. This means that register allocator will not reuse it's
  // register for any other operand inside instruction.
  // Operand created by UseRegisterAtStart is guaranteed to be live only at
  // instruction start. Register allocator is free to assign the same register
  // to some other operand used inside instruction (i.e. temporary or
  // output).
  MUST_USE_RESULT LOperand* UseRegister(HValue* value);
  MUST_USE_RESULT LOperand* UseRegisterAtStart(HValue* value);

  // An input operand in a register that may be trashed.
  MUST_USE_RESULT LOperand* UseTempRegister(HValue* value);

  // An input operand in a register or stack slot.
  MUST_USE_RESULT LOperand* Use(HValue* value);
  MUST_USE_RESULT LOperand* UseAtStart(HValue* value);

  // An input operand in a register, stack slot or a constant operand.
  MUST_USE_RESULT LOperand* UseOrConstant(HValue* value);
  MUST_USE_RESULT LOperand* UseOrConstantAtStart(HValue* value);

  // An input operand in a register or a constant operand.
  MUST_USE_RESULT LOperand* UseRegisterOrConstant(HValue* value);
  MUST_USE_RESULT LOperand* UseRegisterOrConstantAtStart(HValue* value);

  // An input operand in a constant operand.
  MUST_USE_RESULT LOperand* UseConstant(HValue* value);

  // An input operand in register, stack slot or a constant operand.
  // Will not be moved to a register even if one is freely available.
  MUST_USE_RESULT LOperand* UseAny(HValue* value) override;

  // Temporary operand that must be in a register.
  MUST_USE_RESULT LUnallocated* TempRegister();
  MUST_USE_RESULT LUnallocated* TempDoubleRegister();
  MUST_USE_RESULT LOperand* FixedTemp(Register reg);
  MUST_USE_RESULT LOperand* FixedTemp(DoubleRegister reg);

  // Methods for setting up define-use relationships.
  // Return the same instruction that they are passed.
  LInstruction* Define(LTemplateResultInstruction<1>* instr,
                       LUnallocated* result);
  LInstruction* DefineAsRegister(LTemplateResultInstruction<1>* instr);
  LInstruction* DefineAsSpilled(LTemplateResultInstruction<1>* instr,
                                int index);
  LInstruction* DefineSameAsFirst(LTemplateResultInstruction<1>* instr);
  LInstruction* DefineFixed(LTemplateResultInstruction<1>* instr, Register reg);
  LInstruction* DefineFixedDouble(LTemplateResultInstruction<1>* instr,
                                  DoubleRegister reg);
  LInstruction* AssignEnvironment(LInstruction* instr);
  LInstruction* AssignPointerMap(LInstruction* instr);

  enum CanDeoptimize { CAN_DEOPTIMIZE_EAGERLY, CANNOT_DEOPTIMIZE_EAGERLY };

  // By default we assume that instruction sequences generated for calls
  // cannot deoptimize eagerly and we do not attach environment to this
  // instruction.
  LInstruction* MarkAsCall(
      LInstruction* instr, HInstruction* hinstr,
      CanDeoptimize can_deoptimize = CANNOT_DEOPTIMIZE_EAGERLY);

  void VisitInstruction(HInstruction* current);
  void AddInstruction(LInstruction* instr, HInstruction* current);

  void DoBasicBlock(HBasicBlock* block, HBasicBlock* next_block);
  LInstruction* DoShift(Token::Value op, HBitwiseBinaryOperation* instr);
  LInstruction* DoArithmeticD(Token::Value op,
                              HArithmeticBinaryOperation* instr);
  LInstruction* DoArithmeticT(Token::Value op, HBinaryOperation* instr);

  HInstruction* current_instruction_;
  HBasicBlock* current_block_;
  HBasicBlock* next_block_;
  LAllocator* allocator_;

  DISALLOW_COPY_AND_ASSIGN(LChunkBuilder);
};

#undef DECLARE_HYDROGEN_ACCESSOR
#undef DECLARE_CONCRETE_INSTRUCTION
}  // namespace internal
}  // namespace v8

#endif  // V8_CRANKSHAFT_S390_LITHIUM_S390_H_
