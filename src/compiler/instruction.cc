// Copyright 2014 the V8 project authors. All rights reserved.
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.

#include "src/compiler/instruction.h"

#include "src/compiler/common-operator.h"

namespace v8 {
namespace internal {
namespace compiler {

OStream& operator<<(OStream& os, const InstructionOperand& op) {
  switch (op.kind()) {
    case InstructionOperand::INVALID:
      return os << "\x28\x30\x29";
    case InstructionOperand::UNALLOCATED: {
      const UnallocatedOperand* unalloc = UnallocatedOperand::cast(&op);
      os << "\x76" << unalloc->virtual_register();
      if (unalloc->basic_policy() == UnallocatedOperand::FIXED_SLOT) {
        return os << "\x28\x3d" << unalloc->fixed_slot_index() << "\x53\x29";
      }
      switch (unalloc->extended_policy()) {
        case UnallocatedOperand::NONE:
          return os;
        case UnallocatedOperand::FIXED_REGISTER:
          return os << "\x28\x3d" << Register::AllocationIndexToString(
                                   unalloc->fixed_register_index()) << "\x29";
        case UnallocatedOperand::FIXED_DOUBLE_REGISTER:
          return os << "\x28\x3d" << DoubleRegister::AllocationIndexToString(
                                   unalloc->fixed_register_index()) << "\x29";
        case UnallocatedOperand::MUST_HAVE_REGISTER:
          return os << "\x28\x52\x29";
        case UnallocatedOperand::SAME_AS_FIRST_INPUT:
          return os << "\x28\x31\x29";
        case UnallocatedOperand::ANY:
          return os << "\x28\x2d\x29";
      }
    }
    case InstructionOperand::CONSTANT:
      return os << "\x5b\x63\x6f\x6e\x73\x74\x61\x6e\x74\x3a" << op.index() << "\x5d";
    case InstructionOperand::IMMEDIATE:
      return os << "\x5b\x69\x6d\x6d\x65\x64\x69\x61\x74\x65\x3a" << op.index() << "\x5d";
    case InstructionOperand::STACK_SLOT:
      return os << "\x5b\x73\x74\x61\x63\x6b\x3a" << op.index() << "\x5d";
    case InstructionOperand::DOUBLE_STACK_SLOT:
      return os << "\x5b\x64\x6f\x75\x62\x6c\x65\x5f\x73\x74\x61\x63\x6b\x3a" << op.index() << "\x5d";
    case InstructionOperand::REGISTER:
      return os << "\x5b" << Register::AllocationIndexToString(op.index())
                << "\x7c\x52\x5d";
    case InstructionOperand::DOUBLE_REGISTER:
      return os << "\x5b" << DoubleRegister::AllocationIndexToString(op.index())
                << "\x7c\x52\x5d";
  }
  UNREACHABLE();
  return os;
}


template <InstructionOperand::Kind kOperandKind, int kNumCachedOperands>
SubKindOperand<kOperandKind, kNumCachedOperands>*
    SubKindOperand<kOperandKind, kNumCachedOperands>::cache = NULL;


template <InstructionOperand::Kind kOperandKind, int kNumCachedOperands>
void SubKindOperand<kOperandKind, kNumCachedOperands>::SetUpCache() {
  if (cache) return;
  cache = new SubKindOperand[kNumCachedOperands];
  for (int i = 0; i < kNumCachedOperands; i++) {
    cache[i].ConvertTo(kOperandKind, i);
  }
}


template <InstructionOperand::Kind kOperandKind, int kNumCachedOperands>
void SubKindOperand<kOperandKind, kNumCachedOperands>::TearDownCache() {
  delete[] cache;
}


void InstructionOperand::SetUpCaches() {
#define INSTRUCTION_OPERAND_SETUP(name, type, number) \
  name##Operand::SetUpCache();
  INSTRUCTION_OPERAND_LIST(INSTRUCTION_OPERAND_SETUP)
#undef INSTRUCTION_OPERAND_SETUP
}


void InstructionOperand::TearDownCaches() {
#define INSTRUCTION_OPERAND_TEARDOWN(name, type, number) \
  name##Operand::TearDownCache();
  INSTRUCTION_OPERAND_LIST(INSTRUCTION_OPERAND_TEARDOWN)
#undef INSTRUCTION_OPERAND_TEARDOWN
}


OStream& operator<<(OStream& os, const MoveOperands& mo) {
  os << *mo.destination();
  if (!mo.source()->Equals(mo.destination())) os << "\x20\x3d\x20" << *mo.source();
  return os << "\x3b";
}


bool ParallelMove::IsRedundant() const {
  for (int i = 0; i < move_operands_.length(); ++i) {
    if (!move_operands_[i].IsRedundant()) return false;
  }
  return true;
}


OStream& operator<<(OStream& os, const ParallelMove& pm) {
  bool first = true;
  for (ZoneList<MoveOperands>::iterator move = pm.move_operands()->begin();
       move != pm.move_operands()->end(); ++move) {
    if (move->IsEliminated()) continue;
    if (!first) os << "\x20";
    first = false;
    os << *move;
  }
  return os;
}


void PointerMap::RecordPointer(InstructionOperand* op, Zone* zone) {
  // Do not record arguments as pointers.
  if (op->IsStackSlot() && op->index() < 0) return;
  DCHECK(!op->IsDoubleRegister() && !op->IsDoubleStackSlot());
  pointer_operands_.Add(op, zone);
}


void PointerMap::RemovePointer(InstructionOperand* op) {
  // Do not record arguments as pointers.
  if (op->IsStackSlot() && op->index() < 0) return;
  DCHECK(!op->IsDoubleRegister() && !op->IsDoubleStackSlot());
  for (int i = 0; i < pointer_operands_.length(); ++i) {
    if (pointer_operands_[i]->Equals(op)) {
      pointer_operands_.Remove(i);
      --i;
    }
  }
}


void PointerMap::RecordUntagged(InstructionOperand* op, Zone* zone) {
  // Do not record arguments as pointers.
  if (op->IsStackSlot() && op->index() < 0) return;
  DCHECK(!op->IsDoubleRegister() && !op->IsDoubleStackSlot());
  untagged_operands_.Add(op, zone);
}


OStream& operator<<(OStream& os, const PointerMap& pm) {
  os << "\x7b";
  for (ZoneList<InstructionOperand*>::iterator op =
           pm.pointer_operands_.begin();
       op != pm.pointer_operands_.end(); ++op) {
    if (op != pm.pointer_operands_.begin()) os << "\x3b";
    os << *op;
  }
  return os << "\x7d";
}


OStream& operator<<(OStream& os, const ArchOpcode& ao) {
  switch (ao) {
#define CASE(Name) \
  case k##Name:    \
    return os << #Name;
    ARCH_OPCODE_LIST(CASE)
#undef CASE
  }
  UNREACHABLE();
  return os;
}


OStream& operator<<(OStream& os, const AddressingMode& am) {
  switch (am) {
    case kMode_None:
      return os;
#define CASE(Name)   \
  case kMode_##Name: \
    return os << #Name;
      TARGET_ADDRESSING_MODE_LIST(CASE)
#undef CASE
  }
  UNREACHABLE();
  return os;
}


OStream& operator<<(OStream& os, const FlagsMode& fm) {
  switch (fm) {
    case kFlags_none:
      return os;
    case kFlags_branch:
      return os << "\x62\x72\x61\x6e\x63\x68";
    case kFlags_set:
      return os << "\x73\x65\x74";
  }
  UNREACHABLE();
  return os;
}


OStream& operator<<(OStream& os, const FlagsCondition& fc) {
  switch (fc) {
    case kEqual:
      return os << "\x65\x71\x75\x61\x6c";
    case kNotEqual:
      return os << "\x6e\x6f\x74\x20\x65\x71\x75\x61\x6c";
    case kSignedLessThan:
      return os << "\x73\x69\x67\x6e\x65\x64\x20\x6c\x65\x73\x73\x20\x74\x68\x61\x6e";
    case kSignedGreaterThanOrEqual:
      return os << "\x73\x69\x67\x6e\x65\x64\x20\x67\x72\x65\x61\x74\x65\x72\x20\x74\x68\x61\x6e\x20\x6f\x72\x20\x65\x71\x75\x61\x6c";
    case kSignedLessThanOrEqual:
      return os << "\x73\x69\x67\x6e\x65\x64\x20\x6c\x65\x73\x73\x20\x74\x68\x61\x6e\x20\x6f\x72\x20\x65\x71\x75\x61\x6c";
    case kSignedGreaterThan:
      return os << "\x73\x69\x67\x6e\x65\x64\x20\x67\x72\x65\x61\x74\x65\x72\x20\x74\x68\x61\x6e";
    case kUnsignedLessThan:
      return os << "\x75\x6e\x73\x69\x67\x6e\x65\x64\x20\x6c\x65\x73\x73\x20\x74\x68\x61\x6e";
    case kUnsignedGreaterThanOrEqual:
      return os << "\x75\x6e\x73\x69\x67\x6e\x65\x64\x20\x67\x72\x65\x61\x74\x65\x72\x20\x74\x68\x61\x6e\x20\x6f\x72\x20\x65\x71\x75\x61\x6c";
    case kUnsignedLessThanOrEqual:
      return os << "\x75\x6e\x73\x69\x67\x6e\x65\x64\x20\x6c\x65\x73\x73\x20\x74\x68\x61\x6e\x20\x6f\x72\x20\x65\x71\x75\x61\x6c";
    case kUnsignedGreaterThan:
      return os << "\x75\x6e\x73\x69\x67\x6e\x65\x64\x20\x67\x72\x65\x61\x74\x65\x72\x20\x74\x68\x61\x6e";
    case kUnorderedEqual:
      return os << "\x75\x6e\x6f\x72\x64\x65\x72\x65\x64\x20\x65\x71\x75\x61\x6c";
    case kUnorderedNotEqual:
      return os << "\x75\x6e\x6f\x72\x64\x65\x72\x65\x64\x20\x6e\x6f\x74\x20\x65\x71\x75\x61\x6c";
    case kUnorderedLessThan:
      return os << "\x75\x6e\x6f\x72\x64\x65\x72\x65\x64\x20\x6c\x65\x73\x73\x20\x74\x68\x61\x6e";
    case kUnorderedGreaterThanOrEqual:
      return os << "\x75\x6e\x6f\x72\x64\x65\x72\x65\x64\x20\x67\x72\x65\x61\x74\x65\x72\x20\x74\x68\x61\x6e\x20\x6f\x72\x20\x65\x71\x75\x61\x6c";
    case kUnorderedLessThanOrEqual:
      return os << "\x75\x6e\x6f\x72\x64\x65\x72\x65\x64\x20\x6c\x65\x73\x73\x20\x74\x68\x61\x6e\x20\x6f\x72\x20\x65\x71\x75\x61\x6c";
    case kUnorderedGreaterThan:
      return os << "\x75\x6e\x6f\x72\x64\x65\x72\x65\x64\x20\x67\x72\x65\x61\x74\x65\x72\x20\x74\x68\x61\x6e";
    case kOverflow:
      return os << "\x6f\x76\x65\x72\x66\x6c\x6f\x77";
    case kNotOverflow:
      return os << "\x6e\x6f\x74\x20\x6f\x76\x65\x72\x66\x6c\x6f\x77";
  }
  UNREACHABLE();
  return os;
}


OStream& operator<<(OStream& os, const Instruction& instr) {
  if (instr.OutputCount() > 1) os << "\x28";
  for (size_t i = 0; i < instr.OutputCount(); i++) {
    if (i > 0) os << "\x2c\x20";
    os << *instr.OutputAt(i);
  }

  if (instr.OutputCount() > 1) os << "\x29\x20\x3d\x20";
  if (instr.OutputCount() == 1) os << "\x20\x3d\x20";

  if (instr.IsGapMoves()) {
    const GapInstruction* gap = GapInstruction::cast(&instr);
    os << (instr.IsBlockStart() ? "\x20\x62\x6c\x6f\x63\x6b\x2d\x73\x74\x61\x72\x74" : "\x67\x61\x70\x20");
    for (int i = GapInstruction::FIRST_INNER_POSITION;
         i <= GapInstruction::LAST_INNER_POSITION; i++) {
      os << "\x28";
      if (gap->parallel_moves_[i] != NULL) os << *gap->parallel_moves_[i];
      os << "\x29\x20";
    }
  } else if (instr.IsSourcePosition()) {
    const SourcePositionInstruction* pos =
        SourcePositionInstruction::cast(&instr);
    os << "\x70\x6f\x73\x69\x74\x69\x6f\x6e\x20\x28" << pos->source_position().raw() << "\x29";
  } else {
    os << ArchOpcodeField::decode(instr.opcode());
    AddressingMode am = AddressingModeField::decode(instr.opcode());
    if (am != kMode_None) {
      os << "\x20\x3a\x20" << AddressingModeField::decode(instr.opcode());
    }
    FlagsMode fm = FlagsModeField::decode(instr.opcode());
    if (fm != kFlags_none) {
      os << "\x20\x26\x26\x20" << fm << "\x20\x69\x66\x20"
         << FlagsConditionField::decode(instr.opcode());
    }
  }
  if (instr.InputCount() > 0) {
    for (size_t i = 0; i < instr.InputCount(); i++) {
      os << "\x20" << *instr.InputAt(i);
    }
  }
  return os << "\xa";
}


OStream& operator<<(OStream& os, const Constant& constant) {
  switch (constant.type()) {
    case Constant::kInt32:
      return os << constant.ToInt32();
    case Constant::kInt64:
      return os << constant.ToInt64() << "\x6c";
    case Constant::kFloat64:
      return os << constant.ToFloat64();
    case Constant::kExternalReference:
      return os << constant.ToExternalReference().address();
    case Constant::kHeapObject:
      return os << Brief(*constant.ToHeapObject());
  }
  UNREACHABLE();
  return os;
}


Label* InstructionSequence::GetLabel(BasicBlock* block) {
  return GetBlockStart(block)->label();
}


BlockStartInstruction* InstructionSequence::GetBlockStart(BasicBlock* block) {
  return BlockStartInstruction::cast(InstructionAt(block->code_start_));
}


void InstructionSequence::StartBlock(BasicBlock* block) {
  block->code_start_ = static_cast<int>(instructions_.size());
  BlockStartInstruction* block_start =
      BlockStartInstruction::New(zone(), block);
  AddInstruction(block_start, block);
}


void InstructionSequence::EndBlock(BasicBlock* block) {
  int end = static_cast<int>(instructions_.size());
  DCHECK(block->code_start_ >= 0 && block->code_start_ < end);
  block->code_end_ = end;
}


int InstructionSequence::AddInstruction(Instruction* instr, BasicBlock* block) {
  // TODO(titzer): the order of these gaps is a holdover from Lithium.
  GapInstruction* gap = GapInstruction::New(zone());
  if (instr->IsControl()) instructions_.push_back(gap);
  int index = static_cast<int>(instructions_.size());
  instructions_.push_back(instr);
  if (!instr->IsControl()) instructions_.push_back(gap);
  if (instr->NeedsPointerMap()) {
    DCHECK(instr->pointer_map() == NULL);
    PointerMap* pointer_map = new (zone()) PointerMap(zone());
    pointer_map->set_instruction_position(index);
    instr->set_pointer_map(pointer_map);
    pointer_maps_.push_back(pointer_map);
  }
  return index;
}


BasicBlock* InstructionSequence::GetBasicBlock(int instruction_index) {
  // TODO(turbofan): Optimize this.
  for (;;) {
    DCHECK_LE(0, instruction_index);
    Instruction* instruction = InstructionAt(instruction_index--);
    if (instruction->IsBlockStart()) {
      return BlockStartInstruction::cast(instruction)->block();
    }
  }
}


bool InstructionSequence::IsReference(int virtual_register) const {
  return references_.find(virtual_register) != references_.end();
}


bool InstructionSequence::IsDouble(int virtual_register) const {
  return doubles_.find(virtual_register) != doubles_.end();
}


void InstructionSequence::MarkAsReference(int virtual_register) {
  references_.insert(virtual_register);
}


void InstructionSequence::MarkAsDouble(int virtual_register) {
  doubles_.insert(virtual_register);
}


void InstructionSequence::AddGapMove(int index, InstructionOperand* from,
                                     InstructionOperand* to) {
  GapAt(index)->GetOrCreateParallelMove(GapInstruction::START, zone())->AddMove(
      from, to, zone());
}


int InstructionSequence::AddDeoptimizationEntry(
    FrameStateDescriptor* descriptor) {
  int deoptimization_id = static_cast<int>(deoptimization_entries_.size());
  deoptimization_entries_.push_back(descriptor);
  return deoptimization_id;
}

FrameStateDescriptor* InstructionSequence::GetDeoptimizationEntry(
    int deoptimization_id) {
  return deoptimization_entries_[deoptimization_id];
}


int InstructionSequence::GetDeoptimizationEntryCount() {
  return static_cast<int>(deoptimization_entries_.size());
}


OStream& operator<<(OStream& os, const InstructionSequence& code) {
  for (size_t i = 0; i < code.immediates_.size(); ++i) {
    Constant constant = code.immediates_[i];
    os << "\x49\x4d\x4d\x23" << i << "\x3a\x20" << constant << "\xa";
  }
  int i = 0;
  for (ConstantMap::const_iterator it = code.constants_.begin();
       it != code.constants_.end(); ++i, ++it) {
    os << "\x43\x53\x54\x23" << i << "\x3a\x20\x76" << it->first << "\x20\x3d\x20" << it->second << "\xa";
  }
  for (int i = 0; i < code.BasicBlockCount(); i++) {
    BasicBlock* block = code.BlockAt(i);

    int bid = block->id();
    os << "\x52\x50\x4f\x23" << block->rpo_number_ << "\x3a\x20\x42" << bid;
    CHECK(block->rpo_number_ == i);
    if (block->IsLoopHeader()) {
      os << "\x20\x6c\x6f\x6f\x70\x20\x62\x6c\x6f\x63\x6b\x73\x3a\x20\x5b" << block->rpo_number_ << "\x2c\x20" << block->loop_end_
         << "\x29";
    }
    os << "\x20\x20\x69\x6e\x73\x74\x72\x75\x63\x74\x69\x6f\x6e\x73\x3a\x20\x5b" << block->code_start_ << "\x2c\x20" << block->code_end_
       << "\x29\xa\x20\x20\x70\x72\x65\x64\x65\x63\x65\x73\x73\x6f\x72\x73\x3a";

    BasicBlock::Predecessors predecessors = block->predecessors();
    for (BasicBlock::Predecessors::iterator iter = predecessors.begin();
         iter != predecessors.end(); ++iter) {
      os << "\x20\x42" << (*iter)->id();
    }
    os << "\xa";

    for (BasicBlock::const_iterator j = block->begin(); j != block->end();
         ++j) {
      Node* phi = *j;
      if (phi->opcode() != IrOpcode::kPhi) continue;
      os << "\x20\x20\x20\x20\x20\x70\x68\x69\x3a\x20\x76" << phi->id() << "\x20\x3d";
      Node::Inputs inputs = phi->inputs();
      for (Node::Inputs::iterator iter(inputs.begin()); iter != inputs.end();
           ++iter) {
        os << "\x20\x76" << (*iter)->id();
      }
      os << "\xa";
    }

    ScopedVector<char> buf(32);
    for (int j = block->first_instruction_index();
         j <= block->last_instruction_index(); j++) {
      // TODO(svenpanne) Add some basic formatting to our streams.
      SNPrintF(buf, "\x6c\xf5\x84", j);
      os << "\x20\x20\x20" << buf.start() << "\x3a\x20" << *code.InstructionAt(j);
    }

    os << "\x20\x20" << block->control_;

    if (block->control_input_ != NULL) {
      os << "\x20\x76" << block->control_input_->id();
    }

    BasicBlock::Successors successors = block->successors();
    for (BasicBlock::Successors::iterator iter = successors.begin();
         iter != successors.end(); ++iter) {
      os << "\x20\x42" << (*iter)->id();
    }
    os << "\xa";
  }
  return os;
}

}  // namespace compiler
}  // namespace internal
}  // namespace v8
