// Copyright 2015 the V8 project authors. All rights reserved.
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.

#ifndef V8_COMPILER_S390_INSTRUCTION_CODES_S390_H_
#define V8_COMPILER_S390_INSTRUCTION_CODES_S390_H_

namespace v8 {
namespace internal {
namespace compiler {

// S390-specific opcodes that specify which assembly sequence to emit.
// Most opcodes specify a single instruction.
#define TARGET_ARCH_OPCODE_LIST(V) \
  V(S390_And32)                     \
  V(S390_And64)                     \
  V(S390_Or32)                      \
  V(S390_Or64)                      \
  V(S390_AndComplement32)           \
  V(S390_AndComplement64)           \
  V(S390_OrComplement32)            \
  V(S390_OrComplement64)            \
  V(S390_Xor32)                     \
  V(S390_Xor64)                     \
  V(S390_ShiftLeft32)               \
  V(S390_ShiftLeft64)               \
  V(S390_ShiftRight32)              \
  V(S390_ShiftRight64)              \
  V(S390_ShiftRightAlg32)           \
  V(S390_ShiftRightAlg64)           \
  V(S390_RotRight32)                \
  V(S390_RotRight64)                \
  V(S390_Not32)                     \
  V(S390_Not64)                     \
  V(S390_RotLeftAndMask32)          \
  V(S390_RotLeftAndClear64)         \
  V(S390_RotLeftAndClearLeft64)     \
  V(S390_RotLeftAndClearRight64)    \
  V(S390_Add32)                     \
  V(S390_AddWithOverflow32)         \
  V(S390_Add64)                     \
  V(S390_AddFloat64)                \
  V(S390_Sub32)                     \
  V(S390_SubWithOverflow32)         \
  V(S390_Sub64)                     \
  V(S390_SubFloat64)                \
  V(S390_Mul32)                     \
  V(S390_Mul64)                     \
  V(S390_MulHigh32)                 \
  V(S390_MulHighU32)                \
  V(S390_MulFloat64)                \
  V(S390_Div32)                     \
  V(S390_Div64)                     \
  V(S390_DivU32)                    \
  V(S390_DivU64)                    \
  V(S390_DivFloat64)                \
  V(S390_Mod32)                     \
  V(S390_Mod64)                     \
  V(S390_ModU32)                    \
  V(S390_ModU64)                    \
  V(S390_ModFloat64)                \
  V(S390_Neg32)                     \
  V(S390_Neg64)                     \
  V(S390_NegFloat64)                \
  V(S390_SqrtFloat64)               \
  V(S390_FloorFloat64)              \
  V(S390_CeilFloat64)               \
  V(S390_TruncateFloat64)           \
  V(S390_RoundFloat64)              \
  V(S390_MaxFloat64)                \
  V(S390_MinFloat64)                \
  V(S390_Cntlz32)                   \
  V(S390_Cmp32)                     \
  V(S390_Cmp64)                     \
  V(S390_CmpFloat64)                \
  V(S390_Tst32)                     \
  V(S390_Tst64)                     \
  V(S390_Push)                      \
  V(S390_ExtendSignWord8)           \
  V(S390_ExtendSignWord16)          \
  V(S390_ExtendSignWord32)          \
  V(S390_Uint32ToUint64)            \
  V(S390_Int64ToInt32)              \
  V(S390_Int32ToFloat64)            \
  V(S390_Uint32ToFloat64)           \
  V(S390_Float32ToFloat64)          \
  V(S390_Float64ToInt32)            \
  V(S390_Float64ToUint32)           \
  V(S390_Float64ToFloat32)          \
  V(S390_Float64ExtractLowWord32)   \
  V(S390_Float64ExtractHighWord32)  \
  V(S390_Float64InsertLowWord32)    \
  V(S390_Float64InsertHighWord32)   \
  V(S390_Float64Construct)          \
  V(S390_LoadWordS8)                \
  V(S390_LoadWordU8)                \
  V(S390_LoadWordS16)               \
  V(S390_LoadWordU16)               \
  V(S390_LoadWordS32)               \
  V(S390_LoadWord64)                \
  V(S390_LoadFloat32)               \
  V(S390_LoadFloat64)               \
  V(S390_StoreWord8)                \
  V(S390_StoreWord16)               \
  V(S390_StoreWord32)               \
  V(S390_StoreWord64)               \
  V(S390_StoreFloat32)              \
  V(S390_StoreFloat64)              \
  V(S390_StoreWriteBarrier)


// Addressing modes represent the "shape" of inputs to an instruction.
// Many instructions support multiple addressing modes. Addressing modes
// are encoded into the InstructionCode of the instruction and tell the
// code generator after register allocation which assembler method to call.
//
// We use the following local notation for addressing modes:
//
// R = register
// O = register or stack slot
// D = double register
// I = immediate (handle, external, int32)
// MRI = [register + immediate]
// MRR = [register + register]
#define TARGET_ADDRESSING_MODE_LIST(V) \
  V(MRI) /* [%r0 + K] */               \
  V(MRR) /* [%r0 + %r1] */

}  // namespace compiler
}  // namespace internal
}  // namespace v8

#endif  // V8_COMPILER_S390_INSTRUCTION_CODES_S390_H_
