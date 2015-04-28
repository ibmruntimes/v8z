// Copyright 2014 the V8 project authors. All rights reserved.
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.

#include "src/compiler/code-generator.h"

#include "src/arm/macro-assembler-arm.h"
#include "src/compiler/code-generator-impl.h"
#include "src/compiler/gap-resolver.h"
#include "src/compiler/node-matchers.h"
<<<<<<< HEAD
#include "src/compiler/node-properties-inl.h"
=======
>>>>>>> 4.3
#include "src/scopes.h"

namespace v8 {
namespace internal {
namespace compiler {

#define __ masm()->


#define kScratchReg r9


// Adds Arm-specific methods to convert InstructionOperands.
<<<<<<< HEAD
class ArmOperandConverter : public InstructionOperandConverter {
=======
class ArmOperandConverter FINAL : public InstructionOperandConverter {
>>>>>>> 4.3
 public:
  ArmOperandConverter(CodeGenerator* gen, Instruction* instr)
      : InstructionOperandConverter(gen, instr) {}

<<<<<<< HEAD
=======
  SwVfpRegister OutputFloat32Register(size_t index = 0) {
    return ToFloat32Register(instr_->OutputAt(index));
  }

  SwVfpRegister InputFloat32Register(size_t index) {
    return ToFloat32Register(instr_->InputAt(index));
  }

  SwVfpRegister ToFloat32Register(InstructionOperand* op) {
    return ToFloat64Register(op).low();
  }

  LowDwVfpRegister OutputFloat64Register(size_t index = 0) {
    return ToFloat64Register(instr_->OutputAt(index));
  }

  LowDwVfpRegister InputFloat64Register(size_t index) {
    return ToFloat64Register(instr_->InputAt(index));
  }

  LowDwVfpRegister ToFloat64Register(InstructionOperand* op) {
    return LowDwVfpRegister::from_code(ToDoubleRegister(op).code());
  }

>>>>>>> 4.3
  SBit OutputSBit() const {
    switch (instr_->flags_mode()) {
      case kFlags_branch:
      case kFlags_set:
        return SetCC;
      case kFlags_none:
        return LeaveCC;
    }
    UNREACHABLE();
    return LeaveCC;
  }

<<<<<<< HEAD
  Operand InputImmediate(int index) {
=======
  Operand InputImmediate(size_t index) {
>>>>>>> 4.3
    Constant constant = ToConstant(instr_->InputAt(index));
    switch (constant.type()) {
      case Constant::kInt32:
        return Operand(constant.ToInt32());
<<<<<<< HEAD
=======
      case Constant::kFloat32:
        return Operand(
            isolate()->factory()->NewNumber(constant.ToFloat32(), TENURED));
>>>>>>> 4.3
      case Constant::kFloat64:
        return Operand(
            isolate()->factory()->NewNumber(constant.ToFloat64(), TENURED));
      case Constant::kInt64:
      case Constant::kExternalReference:
      case Constant::kHeapObject:
<<<<<<< HEAD
=======
      case Constant::kRpoNumber:
>>>>>>> 4.3
        break;
    }
    UNREACHABLE();
    return Operand::Zero();
  }

<<<<<<< HEAD
  Operand InputOperand2(int first_index) {
    const int index = first_index;
=======
  Operand InputOperand2(size_t first_index) {
    const size_t index = first_index;
>>>>>>> 4.3
    switch (AddressingModeField::decode(instr_->opcode())) {
      case kMode_None:
      case kMode_Offset_RI:
      case kMode_Offset_RR:
        break;
      case kMode_Operand2_I:
        return InputImmediate(index + 0);
      case kMode_Operand2_R:
        return Operand(InputRegister(index + 0));
      case kMode_Operand2_R_ASR_I:
        return Operand(InputRegister(index + 0), ASR, InputInt5(index + 1));
      case kMode_Operand2_R_ASR_R:
        return Operand(InputRegister(index + 0), ASR, InputRegister(index + 1));
      case kMode_Operand2_R_LSL_I:
        return Operand(InputRegister(index + 0), LSL, InputInt5(index + 1));
      case kMode_Operand2_R_LSL_R:
        return Operand(InputRegister(index + 0), LSL, InputRegister(index + 1));
      case kMode_Operand2_R_LSR_I:
        return Operand(InputRegister(index + 0), LSR, InputInt5(index + 1));
      case kMode_Operand2_R_LSR_R:
        return Operand(InputRegister(index + 0), LSR, InputRegister(index + 1));
      case kMode_Operand2_R_ROR_I:
        return Operand(InputRegister(index + 0), ROR, InputInt5(index + 1));
      case kMode_Operand2_R_ROR_R:
        return Operand(InputRegister(index + 0), ROR, InputRegister(index + 1));
    }
    UNREACHABLE();
    return Operand::Zero();
  }

<<<<<<< HEAD
  MemOperand InputOffset(int* first_index) {
    const int index = *first_index;
=======
  MemOperand InputOffset(size_t* first_index) {
    const size_t index = *first_index;
>>>>>>> 4.3
    switch (AddressingModeField::decode(instr_->opcode())) {
      case kMode_None:
      case kMode_Operand2_I:
      case kMode_Operand2_R:
      case kMode_Operand2_R_ASR_I:
      case kMode_Operand2_R_ASR_R:
      case kMode_Operand2_R_LSL_I:
      case kMode_Operand2_R_LSL_R:
      case kMode_Operand2_R_LSR_I:
      case kMode_Operand2_R_LSR_R:
      case kMode_Operand2_R_ROR_I:
      case kMode_Operand2_R_ROR_R:
        break;
      case kMode_Offset_RI:
        *first_index += 2;
        return MemOperand(InputRegister(index + 0), InputInt32(index + 1));
      case kMode_Offset_RR:
        *first_index += 2;
        return MemOperand(InputRegister(index + 0), InputRegister(index + 1));
    }
    UNREACHABLE();
    return MemOperand(r0);
  }

<<<<<<< HEAD
  MemOperand InputOffset() {
    int index = 0;
    return InputOffset(&index);
=======
  MemOperand InputOffset(size_t first_index = 0) {
    return InputOffset(&first_index);
>>>>>>> 4.3
  }

  MemOperand ToMemOperand(InstructionOperand* op) const {
    DCHECK(op != NULL);
    DCHECK(!op->IsRegister());
    DCHECK(!op->IsDoubleRegister());
    DCHECK(op->IsStackSlot() || op->IsDoubleStackSlot());
    // The linkage computes where all spill slots are located.
    FrameOffset offset = linkage()->GetFrameOffset(op->index(), frame(), 0);
    return MemOperand(offset.from_stack_pointer() ? sp : fp, offset.offset());
  }
};


<<<<<<< HEAD
=======
namespace {

class OutOfLineLoadFloat32 FINAL : public OutOfLineCode {
 public:
  OutOfLineLoadFloat32(CodeGenerator* gen, SwVfpRegister result)
      : OutOfLineCode(gen), result_(result) {}

  void Generate() FINAL {
    __ vmov(result_, std::numeric_limits<float>::quiet_NaN());
  }

 private:
  SwVfpRegister const result_;
};


class OutOfLineLoadFloat64 FINAL : public OutOfLineCode {
 public:
  OutOfLineLoadFloat64(CodeGenerator* gen, DwVfpRegister result)
      : OutOfLineCode(gen), result_(result) {}

  void Generate() FINAL {
    __ vmov(result_, std::numeric_limits<double>::quiet_NaN(), kScratchReg);
  }

 private:
  DwVfpRegister const result_;
};


class OutOfLineLoadInteger FINAL : public OutOfLineCode {
 public:
  OutOfLineLoadInteger(CodeGenerator* gen, Register result)
      : OutOfLineCode(gen), result_(result) {}

  void Generate() FINAL { __ mov(result_, Operand::Zero()); }

 private:
  Register const result_;
};


Condition FlagsConditionToCondition(FlagsCondition condition) {
  switch (condition) {
    case kEqual:
      return eq;
    case kNotEqual:
      return ne;
    case kSignedLessThan:
      return lt;
    case kSignedGreaterThanOrEqual:
      return ge;
    case kSignedLessThanOrEqual:
      return le;
    case kSignedGreaterThan:
      return gt;
    case kUnsignedLessThan:
      return lo;
    case kUnsignedGreaterThanOrEqual:
      return hs;
    case kUnsignedLessThanOrEqual:
      return ls;
    case kUnsignedGreaterThan:
      return hi;
    case kOverflow:
      return vs;
    case kNotOverflow:
      return vc;
    case kUnorderedEqual:
    case kUnorderedNotEqual:
      break;
  }
  UNREACHABLE();
  return kNoCondition;
}

}  // namespace


#define ASSEMBLE_CHECKED_LOAD_FLOAT(width)                           \
  do {                                                               \
    auto result = i.OutputFloat##width##Register();                  \
    auto offset = i.InputRegister(0);                                \
    if (instr->InputAt(1)->IsRegister()) {                           \
      __ cmp(offset, i.InputRegister(1));                            \
    } else {                                                         \
      __ cmp(offset, i.InputImmediate(1));                           \
    }                                                                \
    auto ool = new (zone()) OutOfLineLoadFloat##width(this, result); \
    __ b(hs, ool->entry());                                          \
    __ vldr(result, i.InputOffset(2));                               \
    __ bind(ool->exit());                                            \
    DCHECK_EQ(LeaveCC, i.OutputSBit());                              \
  } while (0)


#define ASSEMBLE_CHECKED_LOAD_INTEGER(asm_instr)                \
  do {                                                          \
    auto result = i.OutputRegister();                           \
    auto offset = i.InputRegister(0);                           \
    if (instr->InputAt(1)->IsRegister()) {                      \
      __ cmp(offset, i.InputRegister(1));                       \
    } else {                                                    \
      __ cmp(offset, i.InputImmediate(1));                      \
    }                                                           \
    auto ool = new (zone()) OutOfLineLoadInteger(this, result); \
    __ b(hs, ool->entry());                                     \
    __ asm_instr(result, i.InputOffset(2));                     \
    __ bind(ool->exit());                                       \
    DCHECK_EQ(LeaveCC, i.OutputSBit());                         \
  } while (0)


#define ASSEMBLE_CHECKED_STORE_FLOAT(width)        \
  do {                                             \
    auto offset = i.InputRegister(0);              \
    if (instr->InputAt(1)->IsRegister()) {         \
      __ cmp(offset, i.InputRegister(1));          \
    } else {                                       \
      __ cmp(offset, i.InputImmediate(1));         \
    }                                              \
    auto value = i.InputFloat##width##Register(2); \
    __ vstr(value, i.InputOffset(3), lo);          \
    DCHECK_EQ(LeaveCC, i.OutputSBit());            \
  } while (0)


#define ASSEMBLE_CHECKED_STORE_INTEGER(asm_instr) \
  do {                                            \
    auto offset = i.InputRegister(0);             \
    if (instr->InputAt(1)->IsRegister()) {        \
      __ cmp(offset, i.InputRegister(1));         \
    } else {                                      \
      __ cmp(offset, i.InputImmediate(1));        \
    }                                             \
    auto value = i.InputRegister(2);              \
    __ asm_instr(value, i.InputOffset(3), lo);    \
    DCHECK_EQ(LeaveCC, i.OutputSBit());           \
  } while (0)


>>>>>>> 4.3
// Assembles an instruction after register allocation, producing machine code.
void CodeGenerator::AssembleArchInstruction(Instruction* instr) {
  ArmOperandConverter i(this, instr);

  switch (ArchOpcodeField::decode(instr->opcode())) {
<<<<<<< HEAD
    case kArchJmp:
      __ b(code_->GetLabel(i.InputBlock(0)));
=======
    case kArchCallCodeObject: {
      EnsureSpaceForLazyDeopt();
      if (instr->InputAt(0)->IsImmediate()) {
        __ Call(Handle<Code>::cast(i.InputHeapObject(0)),
                RelocInfo::CODE_TARGET);
      } else {
        __ add(ip, i.InputRegister(0),
               Operand(Code::kHeaderSize - kHeapObjectTag));
        __ Call(ip);
      }
      RecordCallPosition(instr);
      DCHECK_EQ(LeaveCC, i.OutputSBit());
      break;
    }
    case kArchCallJSFunction: {
      EnsureSpaceForLazyDeopt();
      Register func = i.InputRegister(0);
      if (FLAG_debug_code) {
        // Check the function's context matches the context argument.
        __ ldr(kScratchReg, FieldMemOperand(func, JSFunction::kContextOffset));
        __ cmp(cp, kScratchReg);
        __ Assert(eq, kWrongFunctionContext);
      }
      __ ldr(ip, FieldMemOperand(func, JSFunction::kCodeEntryOffset));
      __ Call(ip);
      RecordCallPosition(instr);
      DCHECK_EQ(LeaveCC, i.OutputSBit());
      break;
    }
    case kArchJmp:
      AssembleArchJump(i.InputRpo(0));
      DCHECK_EQ(LeaveCC, i.OutputSBit());
      break;
    case kArchLookupSwitch:
      AssembleArchLookupSwitch(instr);
      DCHECK_EQ(LeaveCC, i.OutputSBit());
      break;
    case kArchTableSwitch:
      AssembleArchTableSwitch(instr);
>>>>>>> 4.3
      DCHECK_EQ(LeaveCC, i.OutputSBit());
      break;
    case kArchNop:
      // don't emit code for nops.
      DCHECK_EQ(LeaveCC, i.OutputSBit());
      break;
<<<<<<< HEAD
=======
    case kArchDeoptimize: {
      int deopt_state_id =
          BuildTranslation(instr, -1, 0, OutputFrameStateCombine::Ignore());
      AssembleDeoptimizerCall(deopt_state_id, Deoptimizer::EAGER);
      break;
    }
>>>>>>> 4.3
    case kArchRet:
      AssembleReturn();
      DCHECK_EQ(LeaveCC, i.OutputSBit());
      break;
<<<<<<< HEAD
    case kArchDeoptimize: {
      int deoptimization_id = MiscField::decode(instr->opcode());
      BuildTranslation(instr, deoptimization_id);

      Address deopt_entry = Deoptimizer::GetDeoptimizationEntry(
          isolate(), deoptimization_id, Deoptimizer::LAZY);
      __ Call(deopt_entry, RelocInfo::RUNTIME_ENTRY);
      DCHECK_EQ(LeaveCC, i.OutputSBit());
      break;
    }
=======
    case kArchStackPointer:
      __ mov(i.OutputRegister(), sp);
      DCHECK_EQ(LeaveCC, i.OutputSBit());
      break;
    case kArchTruncateDoubleToI:
      __ TruncateDoubleToI(i.OutputRegister(), i.InputFloat64Register(0));
      DCHECK_EQ(LeaveCC, i.OutputSBit());
      break;
>>>>>>> 4.3
    case kArmAdd:
      __ add(i.OutputRegister(), i.InputRegister(0), i.InputOperand2(1),
             i.OutputSBit());
      break;
    case kArmAnd:
      __ and_(i.OutputRegister(), i.InputRegister(0), i.InputOperand2(1),
              i.OutputSBit());
      break;
    case kArmBic:
      __ bic(i.OutputRegister(), i.InputRegister(0), i.InputOperand2(1),
             i.OutputSBit());
      break;
    case kArmMul:
      __ mul(i.OutputRegister(), i.InputRegister(0), i.InputRegister(1),
             i.OutputSBit());
      break;
    case kArmMla:
      __ mla(i.OutputRegister(), i.InputRegister(0), i.InputRegister(1),
             i.InputRegister(2), i.OutputSBit());
      break;
    case kArmMls: {
      CpuFeatureScope scope(masm(), MLS);
      __ mls(i.OutputRegister(), i.InputRegister(0), i.InputRegister(1),
             i.InputRegister(2));
      DCHECK_EQ(LeaveCC, i.OutputSBit());
      break;
    }
<<<<<<< HEAD
=======
    case kArmSmmul:
      __ smmul(i.OutputRegister(), i.InputRegister(0), i.InputRegister(1));
      DCHECK_EQ(LeaveCC, i.OutputSBit());
      break;
    case kArmSmmla:
      __ smmla(i.OutputRegister(), i.InputRegister(0), i.InputRegister(1),
               i.InputRegister(2));
      DCHECK_EQ(LeaveCC, i.OutputSBit());
      break;
    case kArmUmull:
      __ umull(i.OutputRegister(0), i.OutputRegister(1), i.InputRegister(0),
               i.InputRegister(1), i.OutputSBit());
      break;
>>>>>>> 4.3
    case kArmSdiv: {
      CpuFeatureScope scope(masm(), SUDIV);
      __ sdiv(i.OutputRegister(), i.InputRegister(0), i.InputRegister(1));
      DCHECK_EQ(LeaveCC, i.OutputSBit());
      break;
    }
    case kArmUdiv: {
      CpuFeatureScope scope(masm(), SUDIV);
      __ udiv(i.OutputRegister(), i.InputRegister(0), i.InputRegister(1));
      DCHECK_EQ(LeaveCC, i.OutputSBit());
      break;
    }
    case kArmMov:
<<<<<<< HEAD
      __ Move(i.OutputRegister(), i.InputOperand2(0));
      DCHECK_EQ(LeaveCC, i.OutputSBit());
=======
      __ Move(i.OutputRegister(), i.InputOperand2(0), i.OutputSBit());
>>>>>>> 4.3
      break;
    case kArmMvn:
      __ mvn(i.OutputRegister(), i.InputOperand2(0), i.OutputSBit());
      break;
    case kArmOrr:
      __ orr(i.OutputRegister(), i.InputRegister(0), i.InputOperand2(1),
             i.OutputSBit());
      break;
    case kArmEor:
      __ eor(i.OutputRegister(), i.InputRegister(0), i.InputOperand2(1),
             i.OutputSBit());
      break;
    case kArmSub:
      __ sub(i.OutputRegister(), i.InputRegister(0), i.InputOperand2(1),
             i.OutputSBit());
      break;
    case kArmRsb:
      __ rsb(i.OutputRegister(), i.InputRegister(0), i.InputOperand2(1),
             i.OutputSBit());
      break;
    case kArmBfc: {
      CpuFeatureScope scope(masm(), ARMv7);
      __ bfc(i.OutputRegister(), i.InputInt8(1), i.InputInt8(2));
      DCHECK_EQ(LeaveCC, i.OutputSBit());
      break;
    }
    case kArmUbfx: {
      CpuFeatureScope scope(masm(), ARMv7);
      __ ubfx(i.OutputRegister(), i.InputRegister(0), i.InputInt8(1),
              i.InputInt8(2));
      DCHECK_EQ(LeaveCC, i.OutputSBit());
      break;
    }
<<<<<<< HEAD
    case kArmCallCodeObject: {
      if (instr->InputAt(0)->IsImmediate()) {
        Handle<Code> code = Handle<Code>::cast(i.InputHeapObject(0));
        __ Call(code, RelocInfo::CODE_TARGET);
        RecordSafepoint(instr->pointer_map(), Safepoint::kSimple, 0,
                        Safepoint::kNoLazyDeopt);
      } else {
        Register reg = i.InputRegister(0);
        int entry = Code::kHeaderSize - kHeapObjectTag;
        __ ldr(reg, MemOperand(reg, entry));
        __ Call(reg);
        RecordSafepoint(instr->pointer_map(), Safepoint::kSimple, 0,
                        Safepoint::kNoLazyDeopt);
      }
      bool lazy_deopt = (MiscField::decode(instr->opcode()) == 1);
      if (lazy_deopt) {
        RecordLazyDeoptimizationEntry(instr);
      }
      DCHECK_EQ(LeaveCC, i.OutputSBit());
      break;
    }
    case kArmCallJSFunction: {
      Register func = i.InputRegister(0);

      // TODO(jarin) The load of the context should be separated from the call.
      __ ldr(cp, FieldMemOperand(func, JSFunction::kContextOffset));
      __ ldr(ip, FieldMemOperand(func, JSFunction::kCodeEntryOffset));
      __ Call(ip);

      RecordSafepoint(instr->pointer_map(), Safepoint::kSimple, 0,
                      Safepoint::kNoLazyDeopt);
      RecordLazyDeoptimizationEntry(instr);
      DCHECK_EQ(LeaveCC, i.OutputSBit());
      break;
    }
    case kArmCallAddress: {
      DirectCEntryStub stub(isolate());
      stub.GenerateCall(masm(), i.InputRegister(0));
      DCHECK_EQ(LeaveCC, i.OutputSBit());
      break;
    }
    case kArmPush:
      __ Push(i.InputRegister(0));
      DCHECK_EQ(LeaveCC, i.OutputSBit());
      break;
    case kArmDrop: {
      int words = MiscField::decode(instr->opcode());
      __ Drop(words);
      DCHECK_EQ(LeaveCC, i.OutputSBit());
      break;
    }
=======
    case kArmSxtb:
      __ sxtb(i.OutputRegister(), i.InputRegister(0), i.InputInt32(1));
      DCHECK_EQ(LeaveCC, i.OutputSBit());
      break;
    case kArmSxth:
      __ sxth(i.OutputRegister(), i.InputRegister(0), i.InputInt32(1));
      DCHECK_EQ(LeaveCC, i.OutputSBit());
      break;
    case kArmSxtab:
      __ sxtab(i.OutputRegister(), i.InputRegister(0), i.InputRegister(1),
               i.InputInt32(2));
      DCHECK_EQ(LeaveCC, i.OutputSBit());
      break;
    case kArmSxtah:
      __ sxtah(i.OutputRegister(), i.InputRegister(0), i.InputRegister(1),
               i.InputInt32(2));
      DCHECK_EQ(LeaveCC, i.OutputSBit());
      break;
    case kArmUxtb:
      __ uxtb(i.OutputRegister(), i.InputRegister(0), i.InputInt32(1));
      DCHECK_EQ(LeaveCC, i.OutputSBit());
      break;
    case kArmUxth:
      __ uxth(i.OutputRegister(), i.InputRegister(0), i.InputInt32(1));
      DCHECK_EQ(LeaveCC, i.OutputSBit());
      break;
    case kArmUxtab:
      __ uxtab(i.OutputRegister(), i.InputRegister(0), i.InputRegister(1),
               i.InputInt32(2));
      DCHECK_EQ(LeaveCC, i.OutputSBit());
      break;
    case kArmUxtah:
      __ uxtah(i.OutputRegister(), i.InputRegister(0), i.InputRegister(1),
               i.InputInt32(2));
      DCHECK_EQ(LeaveCC, i.OutputSBit());
      break;
    case kArmClz:
      __ clz(i.OutputRegister(), i.InputRegister(0));
      DCHECK_EQ(LeaveCC, i.OutputSBit());
      break;
>>>>>>> 4.3
    case kArmCmp:
      __ cmp(i.InputRegister(0), i.InputOperand2(1));
      DCHECK_EQ(SetCC, i.OutputSBit());
      break;
    case kArmCmn:
      __ cmn(i.InputRegister(0), i.InputOperand2(1));
      DCHECK_EQ(SetCC, i.OutputSBit());
      break;
    case kArmTst:
      __ tst(i.InputRegister(0), i.InputOperand2(1));
      DCHECK_EQ(SetCC, i.OutputSBit());
      break;
    case kArmTeq:
      __ teq(i.InputRegister(0), i.InputOperand2(1));
      DCHECK_EQ(SetCC, i.OutputSBit());
      break;
    case kArmVcmpF64:
<<<<<<< HEAD
      __ VFPCompareAndSetFlags(i.InputDoubleRegister(0),
                               i.InputDoubleRegister(1));
      DCHECK_EQ(SetCC, i.OutputSBit());
      break;
    case kArmVaddF64:
      __ vadd(i.OutputDoubleRegister(), i.InputDoubleRegister(0),
              i.InputDoubleRegister(1));
      DCHECK_EQ(LeaveCC, i.OutputSBit());
      break;
    case kArmVsubF64:
      __ vsub(i.OutputDoubleRegister(), i.InputDoubleRegister(0),
              i.InputDoubleRegister(1));
      DCHECK_EQ(LeaveCC, i.OutputSBit());
      break;
    case kArmVmulF64:
      __ vmul(i.OutputDoubleRegister(), i.InputDoubleRegister(0),
              i.InputDoubleRegister(1));
      DCHECK_EQ(LeaveCC, i.OutputSBit());
      break;
    case kArmVmlaF64:
      __ vmla(i.OutputDoubleRegister(), i.InputDoubleRegister(1),
              i.InputDoubleRegister(2));
      DCHECK_EQ(LeaveCC, i.OutputSBit());
      break;
    case kArmVmlsF64:
      __ vmls(i.OutputDoubleRegister(), i.InputDoubleRegister(1),
              i.InputDoubleRegister(2));
      DCHECK_EQ(LeaveCC, i.OutputSBit());
      break;
    case kArmVdivF64:
      __ vdiv(i.OutputDoubleRegister(), i.InputDoubleRegister(0),
              i.InputDoubleRegister(1));
=======
      if (instr->InputAt(1)->IsDoubleRegister()) {
        __ VFPCompareAndSetFlags(i.InputFloat64Register(0),
                                 i.InputFloat64Register(1));
      } else {
        DCHECK(instr->InputAt(1)->IsImmediate());
        // 0.0 is the only immediate supported by vcmp instructions.
        DCHECK(i.InputDouble(1) == 0.0);
        __ VFPCompareAndSetFlags(i.InputFloat64Register(0), i.InputDouble(1));
      }
      DCHECK_EQ(SetCC, i.OutputSBit());
      break;
    case kArmVaddF64:
      __ vadd(i.OutputFloat64Register(), i.InputFloat64Register(0),
              i.InputFloat64Register(1));
      DCHECK_EQ(LeaveCC, i.OutputSBit());
      break;
    case kArmVsubF64:
      __ vsub(i.OutputFloat64Register(), i.InputFloat64Register(0),
              i.InputFloat64Register(1));
      DCHECK_EQ(LeaveCC, i.OutputSBit());
      break;
    case kArmVmulF64:
      __ vmul(i.OutputFloat64Register(), i.InputFloat64Register(0),
              i.InputFloat64Register(1));
      DCHECK_EQ(LeaveCC, i.OutputSBit());
      break;
    case kArmVmlaF64:
      __ vmla(i.OutputFloat64Register(), i.InputFloat64Register(1),
              i.InputFloat64Register(2));
      DCHECK_EQ(LeaveCC, i.OutputSBit());
      break;
    case kArmVmlsF64:
      __ vmls(i.OutputFloat64Register(), i.InputFloat64Register(1),
              i.InputFloat64Register(2));
      DCHECK_EQ(LeaveCC, i.OutputSBit());
      break;
    case kArmVdivF64:
      __ vdiv(i.OutputFloat64Register(), i.InputFloat64Register(0),
              i.InputFloat64Register(1));
>>>>>>> 4.3
      DCHECK_EQ(LeaveCC, i.OutputSBit());
      break;
    case kArmVmodF64: {
      // TODO(bmeurer): We should really get rid of this special instruction,
      // and generate a CallAddress instruction instead.
      FrameScope scope(masm(), StackFrame::MANUAL);
      __ PrepareCallCFunction(0, 2, kScratchReg);
<<<<<<< HEAD
      __ MovToFloatParameters(i.InputDoubleRegister(0),
                              i.InputDoubleRegister(1));
      __ CallCFunction(ExternalReference::mod_two_doubles_operation(isolate()),
                       0, 2);
      // Move the result in the double result register.
      __ MovFromFloatResult(i.OutputDoubleRegister());
      DCHECK_EQ(LeaveCC, i.OutputSBit());
      break;
    }
    case kArmVnegF64:
      __ vneg(i.OutputDoubleRegister(), i.InputDoubleRegister(0));
      break;
    case kArmVcvtF64S32: {
      SwVfpRegister scratch = kScratchDoubleReg.low();
      __ vmov(scratch, i.InputRegister(0));
      __ vcvt_f64_s32(i.OutputDoubleRegister(), scratch);
=======
      __ MovToFloatParameters(i.InputFloat64Register(0),
                              i.InputFloat64Register(1));
      __ CallCFunction(ExternalReference::mod_two_doubles_operation(isolate()),
                       0, 2);
      // Move the result in the double result register.
      __ MovFromFloatResult(i.OutputFloat64Register());
      DCHECK_EQ(LeaveCC, i.OutputSBit());
      break;
    }
    case kArmVsqrtF64:
      __ vsqrt(i.OutputFloat64Register(), i.InputFloat64Register(0));
      break;
    case kArmVrintmF64:
      __ vrintm(i.OutputFloat64Register(), i.InputFloat64Register(0));
      break;
    case kArmVrintpF64:
      __ vrintp(i.OutputFloat64Register(), i.InputFloat64Register(0));
      break;
    case kArmVrintzF64:
      __ vrintz(i.OutputFloat64Register(), i.InputFloat64Register(0));
      break;
    case kArmVrintaF64:
      __ vrinta(i.OutputFloat64Register(), i.InputFloat64Register(0));
      break;
    case kArmVnegF64:
      __ vneg(i.OutputFloat64Register(), i.InputFloat64Register(0));
      break;
    case kArmVcvtF32F64: {
      __ vcvt_f32_f64(i.OutputFloat32Register(), i.InputFloat64Register(0));
      DCHECK_EQ(LeaveCC, i.OutputSBit());
      break;
    }
    case kArmVcvtF64F32: {
      __ vcvt_f64_f32(i.OutputFloat64Register(), i.InputFloat32Register(0));
      DCHECK_EQ(LeaveCC, i.OutputSBit());
      break;
    }
    case kArmVcvtF64S32: {
      SwVfpRegister scratch = kScratchDoubleReg.low();
      __ vmov(scratch, i.InputRegister(0));
      __ vcvt_f64_s32(i.OutputFloat64Register(), scratch);
>>>>>>> 4.3
      DCHECK_EQ(LeaveCC, i.OutputSBit());
      break;
    }
    case kArmVcvtF64U32: {
      SwVfpRegister scratch = kScratchDoubleReg.low();
      __ vmov(scratch, i.InputRegister(0));
<<<<<<< HEAD
      __ vcvt_f64_u32(i.OutputDoubleRegister(), scratch);
=======
      __ vcvt_f64_u32(i.OutputFloat64Register(), scratch);
>>>>>>> 4.3
      DCHECK_EQ(LeaveCC, i.OutputSBit());
      break;
    }
    case kArmVcvtS32F64: {
      SwVfpRegister scratch = kScratchDoubleReg.low();
<<<<<<< HEAD
      __ vcvt_s32_f64(scratch, i.InputDoubleRegister(0));
=======
      __ vcvt_s32_f64(scratch, i.InputFloat64Register(0));
>>>>>>> 4.3
      __ vmov(i.OutputRegister(), scratch);
      DCHECK_EQ(LeaveCC, i.OutputSBit());
      break;
    }
    case kArmVcvtU32F64: {
      SwVfpRegister scratch = kScratchDoubleReg.low();
<<<<<<< HEAD
      __ vcvt_u32_f64(scratch, i.InputDoubleRegister(0));
=======
      __ vcvt_u32_f64(scratch, i.InputFloat64Register(0));
>>>>>>> 4.3
      __ vmov(i.OutputRegister(), scratch);
      DCHECK_EQ(LeaveCC, i.OutputSBit());
      break;
    }
<<<<<<< HEAD
    case kArmLoadWord8:
      __ ldrb(i.OutputRegister(), i.InputOffset());
      DCHECK_EQ(LeaveCC, i.OutputSBit());
      break;
    case kArmStoreWord8: {
      int index = 0;
=======
    case kArmVmovLowU32F64:
      __ VmovLow(i.OutputRegister(), i.InputFloat64Register(0));
      DCHECK_EQ(LeaveCC, i.OutputSBit());
      break;
    case kArmVmovLowF64U32:
      __ VmovLow(i.OutputFloat64Register(), i.InputRegister(1));
      DCHECK_EQ(LeaveCC, i.OutputSBit());
      break;
    case kArmVmovHighU32F64:
      __ VmovHigh(i.OutputRegister(), i.InputFloat64Register(0));
      DCHECK_EQ(LeaveCC, i.OutputSBit());
      break;
    case kArmVmovHighF64U32:
      __ VmovHigh(i.OutputFloat64Register(), i.InputRegister(1));
      DCHECK_EQ(LeaveCC, i.OutputSBit());
      break;
    case kArmVmovF64U32U32:
      __ vmov(i.OutputFloat64Register(), i.InputRegister(0),
              i.InputRegister(1));
      DCHECK_EQ(LeaveCC, i.OutputSBit());
      break;
    case kArmLdrb:
      __ ldrb(i.OutputRegister(), i.InputOffset());
      DCHECK_EQ(LeaveCC, i.OutputSBit());
      break;
    case kArmLdrsb:
      __ ldrsb(i.OutputRegister(), i.InputOffset());
      DCHECK_EQ(LeaveCC, i.OutputSBit());
      break;
    case kArmStrb: {
      size_t index = 0;
>>>>>>> 4.3
      MemOperand operand = i.InputOffset(&index);
      __ strb(i.InputRegister(index), operand);
      DCHECK_EQ(LeaveCC, i.OutputSBit());
      break;
    }
<<<<<<< HEAD
    case kArmLoadWord16:
      __ ldrh(i.OutputRegister(), i.InputOffset());
      break;
    case kArmStoreWord16: {
      int index = 0;
=======
    case kArmLdrh:
      __ ldrh(i.OutputRegister(), i.InputOffset());
      break;
    case kArmLdrsh:
      __ ldrsh(i.OutputRegister(), i.InputOffset());
      break;
    case kArmStrh: {
      size_t index = 0;
>>>>>>> 4.3
      MemOperand operand = i.InputOffset(&index);
      __ strh(i.InputRegister(index), operand);
      DCHECK_EQ(LeaveCC, i.OutputSBit());
      break;
    }
<<<<<<< HEAD
    case kArmLoadWord32:
      __ ldr(i.OutputRegister(), i.InputOffset());
      break;
    case kArmStoreWord32: {
      int index = 0;
=======
    case kArmLdr:
      __ ldr(i.OutputRegister(), i.InputOffset());
      break;
    case kArmStr: {
      size_t index = 0;
>>>>>>> 4.3
      MemOperand operand = i.InputOffset(&index);
      __ str(i.InputRegister(index), operand);
      DCHECK_EQ(LeaveCC, i.OutputSBit());
      break;
    }
<<<<<<< HEAD
    case kArmFloat64Load:
      __ vldr(i.OutputDoubleRegister(), i.InputOffset());
      DCHECK_EQ(LeaveCC, i.OutputSBit());
      break;
    case kArmFloat64Store: {
      int index = 0;
      MemOperand operand = i.InputOffset(&index);
      __ vstr(i.InputDoubleRegister(index), operand);
      DCHECK_EQ(LeaveCC, i.OutputSBit());
      break;
    }
=======
    case kArmVldrF32: {
      __ vldr(i.OutputFloat32Register(), i.InputOffset());
      DCHECK_EQ(LeaveCC, i.OutputSBit());
      break;
    }
    case kArmVstrF32: {
      size_t index = 0;
      MemOperand operand = i.InputOffset(&index);
      __ vstr(i.InputFloat32Register(index), operand);
      DCHECK_EQ(LeaveCC, i.OutputSBit());
      break;
    }
    case kArmVldrF64:
      __ vldr(i.OutputFloat64Register(), i.InputOffset());
      DCHECK_EQ(LeaveCC, i.OutputSBit());
      break;
    case kArmVstrF64: {
      size_t index = 0;
      MemOperand operand = i.InputOffset(&index);
      __ vstr(i.InputFloat64Register(index), operand);
      DCHECK_EQ(LeaveCC, i.OutputSBit());
      break;
    }
    case kArmPush:
      __ Push(i.InputRegister(0));
      DCHECK_EQ(LeaveCC, i.OutputSBit());
      break;
>>>>>>> 4.3
    case kArmStoreWriteBarrier: {
      Register object = i.InputRegister(0);
      Register index = i.InputRegister(1);
      Register value = i.InputRegister(2);
      __ add(index, object, index);
      __ str(value, MemOperand(index));
      SaveFPRegsMode mode =
          frame()->DidAllocateDoubleRegisters() ? kSaveFPRegs : kDontSaveFPRegs;
      LinkRegisterStatus lr_status = kLRHasNotBeenSaved;
      __ RecordWrite(object, index, value, lr_status, mode);
      DCHECK_EQ(LeaveCC, i.OutputSBit());
      break;
    }
<<<<<<< HEAD
  }
}


// Assembles branches after an instruction.
void CodeGenerator::AssembleArchBranch(Instruction* instr,
                                       FlagsCondition condition) {
  ArmOperandConverter i(this, instr);
  Label done;

  // Emit a branch. The true and false targets are always the last two inputs
  // to the instruction.
  BasicBlock* tblock = i.InputBlock(instr->InputCount() - 2);
  BasicBlock* fblock = i.InputBlock(instr->InputCount() - 1);
  bool fallthru = IsNextInAssemblyOrder(fblock);
  Label* tlabel = code()->GetLabel(tblock);
  Label* flabel = fallthru ? &done : code()->GetLabel(fblock);
  switch (condition) {
    case kUnorderedEqual:
      __ b(vs, flabel);
    // Fall through.
    case kEqual:
      __ b(eq, tlabel);
      break;
    case kUnorderedNotEqual:
      __ b(vs, tlabel);
    // Fall through.
    case kNotEqual:
      __ b(ne, tlabel);
      break;
    case kSignedLessThan:
      __ b(lt, tlabel);
      break;
    case kSignedGreaterThanOrEqual:
      __ b(ge, tlabel);
      break;
    case kSignedLessThanOrEqual:
      __ b(le, tlabel);
      break;
    case kSignedGreaterThan:
      __ b(gt, tlabel);
      break;
    case kUnorderedLessThan:
      __ b(vs, flabel);
    // Fall through.
    case kUnsignedLessThan:
      __ b(lo, tlabel);
      break;
    case kUnorderedGreaterThanOrEqual:
      __ b(vs, tlabel);
    // Fall through.
    case kUnsignedGreaterThanOrEqual:
      __ b(hs, tlabel);
      break;
    case kUnorderedLessThanOrEqual:
      __ b(vs, flabel);
    // Fall through.
    case kUnsignedLessThanOrEqual:
      __ b(ls, tlabel);
      break;
    case kUnorderedGreaterThan:
      __ b(vs, tlabel);
    // Fall through.
    case kUnsignedGreaterThan:
      __ b(hi, tlabel);
      break;
    case kOverflow:
      __ b(vs, tlabel);
      break;
    case kNotOverflow:
      __ b(vc, tlabel);
      break;
  }
  if (!fallthru) __ b(flabel);  // no fallthru to flabel.
  __ bind(&done);
=======
    case kCheckedLoadInt8:
      ASSEMBLE_CHECKED_LOAD_INTEGER(ldrsb);
      break;
    case kCheckedLoadUint8:
      ASSEMBLE_CHECKED_LOAD_INTEGER(ldrb);
      break;
    case kCheckedLoadInt16:
      ASSEMBLE_CHECKED_LOAD_INTEGER(ldrsh);
      break;
    case kCheckedLoadUint16:
      ASSEMBLE_CHECKED_LOAD_INTEGER(ldrh);
      break;
    case kCheckedLoadWord32:
      ASSEMBLE_CHECKED_LOAD_INTEGER(ldr);
      break;
    case kCheckedLoadFloat32:
      ASSEMBLE_CHECKED_LOAD_FLOAT(32);
      break;
    case kCheckedLoadFloat64:
      ASSEMBLE_CHECKED_LOAD_FLOAT(64);
      break;
    case kCheckedStoreWord8:
      ASSEMBLE_CHECKED_STORE_INTEGER(strb);
      break;
    case kCheckedStoreWord16:
      ASSEMBLE_CHECKED_STORE_INTEGER(strh);
      break;
    case kCheckedStoreWord32:
      ASSEMBLE_CHECKED_STORE_INTEGER(str);
      break;
    case kCheckedStoreFloat32:
      ASSEMBLE_CHECKED_STORE_FLOAT(32);
      break;
    case kCheckedStoreFloat64:
      ASSEMBLE_CHECKED_STORE_FLOAT(64);
      break;
  }
}


// Assembles branches after an instruction.
void CodeGenerator::AssembleArchBranch(Instruction* instr, BranchInfo* branch) {
  ArmOperandConverter i(this, instr);
  Label* tlabel = branch->true_label;
  Label* flabel = branch->false_label;
  Condition cc = FlagsConditionToCondition(branch->condition);
  __ b(cc, tlabel);
  if (!branch->fallthru) __ b(flabel);  // no fallthru to flabel.
}


void CodeGenerator::AssembleArchJump(RpoNumber target) {
  if (!IsNextInAssemblyOrder(target)) __ b(GetLabel(target));
>>>>>>> 4.3
}


// Assembles boolean materializations after an instruction.
void CodeGenerator::AssembleArchBoolean(Instruction* instr,
                                        FlagsCondition condition) {
  ArmOperandConverter i(this, instr);
<<<<<<< HEAD
  Label done;

  // Materialize a full 32-bit 1 or 0 value. The result register is always the
  // last output of the instruction.
  Label check;
  DCHECK_NE(0, instr->OutputCount());
  Register reg = i.OutputRegister(instr->OutputCount() - 1);
  Condition cc = kNoCondition;
  switch (condition) {
    case kUnorderedEqual:
      __ b(vc, &check);
      __ mov(reg, Operand(0));
      __ b(&done);
    // Fall through.
    case kEqual:
      cc = eq;
      break;
    case kUnorderedNotEqual:
      __ b(vc, &check);
      __ mov(reg, Operand(1));
      __ b(&done);
    // Fall through.
    case kNotEqual:
      cc = ne;
      break;
    case kSignedLessThan:
      cc = lt;
      break;
    case kSignedGreaterThanOrEqual:
      cc = ge;
      break;
    case kSignedLessThanOrEqual:
      cc = le;
      break;
    case kSignedGreaterThan:
      cc = gt;
      break;
    case kUnorderedLessThan:
      __ b(vc, &check);
      __ mov(reg, Operand(0));
      __ b(&done);
    // Fall through.
    case kUnsignedLessThan:
      cc = lo;
      break;
    case kUnorderedGreaterThanOrEqual:
      __ b(vc, &check);
      __ mov(reg, Operand(1));
      __ b(&done);
    // Fall through.
    case kUnsignedGreaterThanOrEqual:
      cc = hs;
      break;
    case kUnorderedLessThanOrEqual:
      __ b(vc, &check);
      __ mov(reg, Operand(0));
      __ b(&done);
    // Fall through.
    case kUnsignedLessThanOrEqual:
      cc = ls;
      break;
    case kUnorderedGreaterThan:
      __ b(vc, &check);
      __ mov(reg, Operand(1));
      __ b(&done);
    // Fall through.
    case kUnsignedGreaterThan:
      cc = hi;
      break;
    case kOverflow:
      cc = vs;
      break;
    case kNotOverflow:
      cc = vc;
      break;
  }
  __ bind(&check);
  __ mov(reg, Operand(0));
  __ mov(reg, Operand(1), LeaveCC, cc);
  __ bind(&done);
=======

  // Materialize a full 32-bit 1 or 0 value. The result register is always the
  // last output of the instruction.
  DCHECK_NE(0u, instr->OutputCount());
  Register reg = i.OutputRegister(instr->OutputCount() - 1);
  Condition cc = FlagsConditionToCondition(condition);
  __ mov(reg, Operand(0));
  __ mov(reg, Operand(1), LeaveCC, cc);
}


void CodeGenerator::AssembleArchLookupSwitch(Instruction* instr) {
  ArmOperandConverter i(this, instr);
  Register input = i.InputRegister(0);
  for (size_t index = 2; index < instr->InputCount(); index += 2) {
    __ cmp(input, Operand(i.InputInt32(index + 0)));
    __ b(eq, GetLabel(i.InputRpo(index + 1)));
  }
  AssembleArchJump(i.InputRpo(1));
}


void CodeGenerator::AssembleArchTableSwitch(Instruction* instr) {
  ArmOperandConverter i(this, instr);
  Register input = i.InputRegister(0);
  size_t const case_count = instr->InputCount() - 2;
  __ CheckConstPool(true, true);
  __ cmp(input, Operand(case_count));
  __ BlockConstPoolFor(case_count + 2);
  __ ldr(pc, MemOperand(pc, input, LSL, 2), lo);
  __ b(GetLabel(i.InputRpo(1)));
  for (size_t index = 0; index < case_count; ++index) {
    __ dd(GetLabel(i.InputRpo(index + 2)));
  }
}


void CodeGenerator::AssembleDeoptimizerCall(
    int deoptimization_id, Deoptimizer::BailoutType bailout_type) {
  Address deopt_entry = Deoptimizer::GetDeoptimizationEntry(
      isolate(), deoptimization_id, bailout_type);
  __ Call(deopt_entry, RelocInfo::RUNTIME_ENTRY);
>>>>>>> 4.3
}


void CodeGenerator::AssemblePrologue() {
  CallDescriptor* descriptor = linkage()->GetIncomingDescriptor();
<<<<<<< HEAD
  if (descriptor->kind() == CallDescriptor::kCallAddress) {
    __ Push(lr, fp);
    __ mov(fp, sp);
    const RegList saves = descriptor->CalleeSavedRegisters();
    if (saves != 0) {  // Save callee-saved registers.
      int register_save_area_size = 0;
=======
  int stack_slots = frame()->GetSpillSlotCount();
  if (descriptor->kind() == CallDescriptor::kCallAddress) {
    bool saved_pp;
    if (FLAG_enable_ool_constant_pool) {
      __ Push(lr, fp, pp);
      // Adjust FP to point to saved FP.
      __ sub(fp, sp, Operand(StandardFrameConstants::kConstantPoolOffset));
      saved_pp = true;
    } else {
      __ Push(lr, fp);
      __ mov(fp, sp);
      saved_pp = false;
    }
    const RegList saves = descriptor->CalleeSavedRegisters();
    if (saves != 0 || saved_pp) {
      // Save callee-saved registers.
      int register_save_area_size = saved_pp ? kPointerSize : 0;
>>>>>>> 4.3
      for (int i = Register::kNumRegisters - 1; i >= 0; i--) {
        if (!((1 << i) & saves)) continue;
        register_save_area_size += kPointerSize;
      }
      frame()->SetRegisterSaveAreaSize(register_save_area_size);
      __ stm(db_w, sp, saves);
    }
  } else if (descriptor->IsJSFunctionCall()) {
<<<<<<< HEAD
    CompilationInfo* info = linkage()->info();
    __ Prologue(info->IsCodePreAgingActive());
    frame()->SetRegisterSaveAreaSize(
        StandardFrameConstants::kFixedFrameSizeFromFp);

    // Sloppy mode functions and builtins need to replace the receiver with the
    // global proxy when called as functions (without an explicit receiver
    // object).
    // TODO(mstarzinger/verwaest): Should this be moved back into the CallIC?
    if (info->strict_mode() == SLOPPY && !info->is_native()) {
      Label ok;
      // +2 for return address and saved frame pointer.
      int receiver_slot = info->scope()->num_parameters() + 2;
      __ ldr(r2, MemOperand(fp, receiver_slot * kPointerSize));
      __ CompareRoot(r2, Heap::kUndefinedValueRootIndex);
      __ b(ne, &ok);
      __ ldr(r2, GlobalObjectOperand());
      __ ldr(r2, FieldMemOperand(r2, GlobalObject::kGlobalProxyOffset));
      __ str(r2, MemOperand(fp, receiver_slot * kPointerSize));
      __ bind(&ok);
    }

  } else {
=======
    CompilationInfo* info = this->info();
    __ Prologue(info->IsCodePreAgingActive());
    frame()->SetRegisterSaveAreaSize(
        StandardFrameConstants::kFixedFrameSizeFromFp);
  } else if (stack_slots > 0) {
>>>>>>> 4.3
    __ StubPrologue();
    frame()->SetRegisterSaveAreaSize(
        StandardFrameConstants::kFixedFrameSizeFromFp);
  }
<<<<<<< HEAD
  int stack_slots = frame()->GetSpillSlotCount();
=======

  if (info()->is_osr()) {
    // TurboFan OSR-compiled functions cannot be entered directly.
    __ Abort(kShouldNotDirectlyEnterOsrFunction);

    // Unoptimized code jumps directly to this entrypoint while the unoptimized
    // frame is still on the stack. Optimized code uses OSR values directly from
    // the unoptimized frame. Thus, all that needs to be done is to allocate the
    // remaining stack slots.
    if (FLAG_code_comments) __ RecordComment("-- OSR entrypoint --");
    osr_pc_offset_ = __ pc_offset();
    // TODO(titzer): cannot address target function == local #-1
    __ ldr(r1, MemOperand(fp, JavaScriptFrameConstants::kFunctionOffset));
    DCHECK(stack_slots >= frame()->GetOsrStackSlotCount());
    stack_slots -= frame()->GetOsrStackSlotCount();
  }

>>>>>>> 4.3
  if (stack_slots > 0) {
    __ sub(sp, sp, Operand(stack_slots * kPointerSize));
  }
}


void CodeGenerator::AssembleReturn() {
  CallDescriptor* descriptor = linkage()->GetIncomingDescriptor();
<<<<<<< HEAD
  if (descriptor->kind() == CallDescriptor::kCallAddress) {
    if (frame()->GetRegisterSaveAreaSize() > 0) {
      // Remove this frame's spill slots first.
      int stack_slots = frame()->GetSpillSlotCount();
=======
  int stack_slots = frame()->GetSpillSlotCount();
  if (descriptor->kind() == CallDescriptor::kCallAddress) {
    if (frame()->GetRegisterSaveAreaSize() > 0) {
      // Remove this frame's spill slots first.
>>>>>>> 4.3
      if (stack_slots > 0) {
        __ add(sp, sp, Operand(stack_slots * kPointerSize));
      }
      // Restore registers.
      const RegList saves = descriptor->CalleeSavedRegisters();
      if (saves != 0) {
        __ ldm(ia_w, sp, saves);
      }
    }
<<<<<<< HEAD
    __ mov(sp, fp);
    __ ldm(ia_w, sp, fp.bit() | lr.bit());
    __ Ret();
  } else {
    __ mov(sp, fp);
    __ ldm(ia_w, sp, fp.bit() | lr.bit());
    int pop_count =
        descriptor->IsJSFunctionCall() ? descriptor->ParameterCount() : 0;
    __ Drop(pop_count);
    __ Ret();
=======
    __ LeaveFrame(StackFrame::MANUAL);
    __ Ret();
  } else if (descriptor->IsJSFunctionCall() || stack_slots > 0) {
    __ LeaveFrame(StackFrame::MANUAL);
    int pop_count = descriptor->IsJSFunctionCall()
                        ? static_cast<int>(descriptor->JSParameterCount())
                        : 0;
    __ Drop(pop_count);
    __ Ret();
  } else {
    __ Ret();
>>>>>>> 4.3
  }
}


void CodeGenerator::AssembleMove(InstructionOperand* source,
                                 InstructionOperand* destination) {
  ArmOperandConverter g(this, NULL);
  // Dispatch on the source and destination operand kinds.  Not all
  // combinations are possible.
  if (source->IsRegister()) {
    DCHECK(destination->IsRegister() || destination->IsStackSlot());
    Register src = g.ToRegister(source);
    if (destination->IsRegister()) {
      __ mov(g.ToRegister(destination), src);
    } else {
      __ str(src, g.ToMemOperand(destination));
    }
  } else if (source->IsStackSlot()) {
    DCHECK(destination->IsRegister() || destination->IsStackSlot());
    MemOperand src = g.ToMemOperand(source);
    if (destination->IsRegister()) {
      __ ldr(g.ToRegister(destination), src);
    } else {
      Register temp = kScratchReg;
      __ ldr(temp, src);
      __ str(temp, g.ToMemOperand(destination));
    }
  } else if (source->IsConstant()) {
<<<<<<< HEAD
    if (destination->IsRegister() || destination->IsStackSlot()) {
      Register dst =
          destination->IsRegister() ? g.ToRegister(destination) : kScratchReg;
      Constant src = g.ToConstant(source);
=======
    Constant src = g.ToConstant(source);
    if (destination->IsRegister() || destination->IsStackSlot()) {
      Register dst =
          destination->IsRegister() ? g.ToRegister(destination) : kScratchReg;
>>>>>>> 4.3
      switch (src.type()) {
        case Constant::kInt32:
          __ mov(dst, Operand(src.ToInt32()));
          break;
        case Constant::kInt64:
          UNREACHABLE();
          break;
<<<<<<< HEAD
=======
        case Constant::kFloat32:
          __ Move(dst,
                  isolate()->factory()->NewNumber(src.ToFloat32(), TENURED));
          break;
>>>>>>> 4.3
        case Constant::kFloat64:
          __ Move(dst,
                  isolate()->factory()->NewNumber(src.ToFloat64(), TENURED));
          break;
        case Constant::kExternalReference:
          __ mov(dst, Operand(src.ToExternalReference()));
          break;
        case Constant::kHeapObject:
          __ Move(dst, src.ToHeapObject());
          break;
<<<<<<< HEAD
      }
      if (destination->IsStackSlot()) __ str(dst, g.ToMemOperand(destination));
    } else if (destination->IsDoubleRegister()) {
      DwVfpRegister result = g.ToDoubleRegister(destination);
      __ vmov(result, g.ToDouble(source));
    } else {
      DCHECK(destination->IsDoubleStackSlot());
      DwVfpRegister temp = kScratchDoubleReg;
      __ vmov(temp, g.ToDouble(source));
      __ vstr(temp, g.ToMemOperand(destination));
=======
        case Constant::kRpoNumber:
          UNREACHABLE();  // TODO(dcarney): loading RPO constants on arm.
          break;
      }
      if (destination->IsStackSlot()) __ str(dst, g.ToMemOperand(destination));
    } else if (src.type() == Constant::kFloat32) {
      if (destination->IsDoubleStackSlot()) {
        MemOperand dst = g.ToMemOperand(destination);
        __ mov(ip, Operand(bit_cast<int32_t>(src.ToFloat32())));
        __ str(ip, dst);
      } else {
        SwVfpRegister dst = g.ToFloat32Register(destination);
        __ vmov(dst, src.ToFloat32());
      }
    } else {
      DCHECK_EQ(Constant::kFloat64, src.type());
      DwVfpRegister dst = destination->IsDoubleRegister()
                              ? g.ToFloat64Register(destination)
                              : kScratchDoubleReg;
      __ vmov(dst, src.ToFloat64(), kScratchReg);
      if (destination->IsDoubleStackSlot()) {
        __ vstr(dst, g.ToMemOperand(destination));
      }
>>>>>>> 4.3
    }
  } else if (source->IsDoubleRegister()) {
    DwVfpRegister src = g.ToDoubleRegister(source);
    if (destination->IsDoubleRegister()) {
      DwVfpRegister dst = g.ToDoubleRegister(destination);
      __ Move(dst, src);
    } else {
      DCHECK(destination->IsDoubleStackSlot());
      __ vstr(src, g.ToMemOperand(destination));
    }
  } else if (source->IsDoubleStackSlot()) {
    DCHECK(destination->IsDoubleRegister() || destination->IsDoubleStackSlot());
    MemOperand src = g.ToMemOperand(source);
    if (destination->IsDoubleRegister()) {
      __ vldr(g.ToDoubleRegister(destination), src);
    } else {
      DwVfpRegister temp = kScratchDoubleReg;
      __ vldr(temp, src);
      __ vstr(temp, g.ToMemOperand(destination));
    }
  } else {
    UNREACHABLE();
  }
}


void CodeGenerator::AssembleSwap(InstructionOperand* source,
                                 InstructionOperand* destination) {
  ArmOperandConverter g(this, NULL);
  // Dispatch on the source and destination operand kinds.  Not all
  // combinations are possible.
  if (source->IsRegister()) {
    // Register-register.
    Register temp = kScratchReg;
    Register src = g.ToRegister(source);
    if (destination->IsRegister()) {
      Register dst = g.ToRegister(destination);
      __ Move(temp, src);
      __ Move(src, dst);
      __ Move(dst, temp);
    } else {
      DCHECK(destination->IsStackSlot());
      MemOperand dst = g.ToMemOperand(destination);
      __ mov(temp, src);
      __ ldr(src, dst);
      __ str(temp, dst);
    }
  } else if (source->IsStackSlot()) {
    DCHECK(destination->IsStackSlot());
    Register temp_0 = kScratchReg;
    SwVfpRegister temp_1 = kScratchDoubleReg.low();
    MemOperand src = g.ToMemOperand(source);
    MemOperand dst = g.ToMemOperand(destination);
    __ ldr(temp_0, src);
    __ vldr(temp_1, dst);
    __ str(temp_0, dst);
    __ vstr(temp_1, src);
  } else if (source->IsDoubleRegister()) {
    DwVfpRegister temp = kScratchDoubleReg;
    DwVfpRegister src = g.ToDoubleRegister(source);
    if (destination->IsDoubleRegister()) {
      DwVfpRegister dst = g.ToDoubleRegister(destination);
      __ Move(temp, src);
      __ Move(src, dst);
<<<<<<< HEAD
      __ Move(src, temp);
=======
      __ Move(dst, temp);
>>>>>>> 4.3
    } else {
      DCHECK(destination->IsDoubleStackSlot());
      MemOperand dst = g.ToMemOperand(destination);
      __ Move(temp, src);
      __ vldr(src, dst);
      __ vstr(temp, dst);
    }
  } else if (source->IsDoubleStackSlot()) {
    DCHECK(destination->IsDoubleStackSlot());
    Register temp_0 = kScratchReg;
    DwVfpRegister temp_1 = kScratchDoubleReg;
    MemOperand src0 = g.ToMemOperand(source);
    MemOperand src1(src0.rn(), src0.offset() + kPointerSize);
    MemOperand dst0 = g.ToMemOperand(destination);
    MemOperand dst1(dst0.rn(), dst0.offset() + kPointerSize);
    __ vldr(temp_1, dst0);  // Save destination in temp_1.
    __ ldr(temp_0, src0);   // Then use temp_0 to copy source to destination.
    __ str(temp_0, dst0);
    __ ldr(temp_0, src1);
    __ str(temp_0, dst1);
    __ vstr(temp_1, src0);
  } else {
    // No other combinations are possible.
    UNREACHABLE();
  }
}


<<<<<<< HEAD
void CodeGenerator::AddNopForSmiCodeInlining() {
  // On 32-bit ARM we do not insert nops for inlined Smi code.
  UNREACHABLE();
}

#ifdef DEBUG

// Checks whether the code between start_pc and end_pc is a no-op.
bool CodeGenerator::IsNopForSmiCodeInlining(Handle<Code> code, int start_pc,
                                            int end_pc) {
  return false;
}

#endif  // DEBUG

#undef __
}
}
}  // namespace v8::internal::compiler
=======
void CodeGenerator::AssembleJumpTable(Label** targets, size_t target_count) {
  // On 32-bit ARM we emit the jump tables inline.
  UNREACHABLE();
}


void CodeGenerator::AddNopForSmiCodeInlining() {
  // On 32-bit ARM we do not insert nops for inlined Smi code.
}


void CodeGenerator::EnsureSpaceForLazyDeopt() {
  int space_needed = Deoptimizer::patch_size();
  if (!info()->IsStub()) {
    // Ensure that we have enough space after the previous lazy-bailout
    // instruction for patching the code here.
    int current_pc = masm()->pc_offset();
    if (current_pc < last_lazy_deopt_pc_ + space_needed) {
      // Block literal pool emission for duration of padding.
      v8::internal::Assembler::BlockConstPoolScope block_const_pool(masm());
      int padding_size = last_lazy_deopt_pc_ + space_needed - current_pc;
      DCHECK_EQ(0, padding_size % v8::internal::Assembler::kInstrSize);
      while (padding_size > 0) {
        __ nop();
        padding_size -= v8::internal::Assembler::kInstrSize;
      }
    }
  }
  MarkLazyDeoptSite();
}

#undef __

}  // namespace compiler
}  // namespace internal
}  // namespace v8
>>>>>>> 4.3
