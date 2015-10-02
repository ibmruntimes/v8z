// Copyright (c) 1994-2006 Sun Microsystems Inc.
// All Rights Reserved.
//
// Redistribution and use in source and binary forms, with or without
// modification, are permitted provided that the following conditions
// are met:
//
// - Redistributions of source code must retain the above copyright notice,
// this list of conditions and the following disclaimer.
//
// - Redistribution in binary form must reproduce the above copyright
// notice, this list of conditions and the following disclaimer in the
// documentation and/or other materials provided with the
// distribution.
//
// - Neither the name of Sun Microsystems or the names of contributors may
// be used to endorse or promote products derived from this software without
// specific prior written permission.
//
// THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
// "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
// LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
// FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
// COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
// INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
// (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
// SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
// HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT,
// STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
// ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED
// OF THE POSSIBILITY OF SUCH DAMAGE.

// The original source code covered by the above license above has been
// modified significantly by Google Inc.
// Copyright 2015 the V8 project authors. All rights reserved.

// A light-weight S390 Assembler
// Generates user mode instructions for the S390 architecture

#ifndef V8_S390_ASSEMBLER_S390_H_
#define V8_S390_ASSEMBLER_S390_H_
#include <stdio.h>
#if V8_HOST_ARCH_S390
// elf.h include is required for auxv check for STFLE facility used
// for hardware detection, which is sensible only on s390 hosts.
#include <elf.h>
#endif

#include <fcntl.h>
#include <unistd.h>
#include "src/assembler.h"
#include "src/compiler.h"
#include "src/s390/constants-s390.h"

#define ABI_USES_FUNCTION_DESCRIPTORS 0

#define ABI_PASSES_HANDLES_IN_REGS 1

#define ABI_RETURNS_OBJECT_PAIRS_IN_REGS \
  (!V8_HOST_ARCH_S390 || (V8_TARGET_LITTLE_ENDIAN))

#define INSTR_AND_DATA_CACHE_COHERENCY LWSYNC

namespace v8 {
namespace internal {

// CPU Registers.
//
// 1) We would prefer to use an enum, but enum values are assignment-
// compatible with int, which has caused code-generation bugs.
//
// 2) We would prefer to use a class instead of a struct but we don't like
// the register initialization to depend on the particular initialization
// order (which appears to be different on OS X, Linux, and Windows for the
// installed versions of C++ we tried). Using a struct permits C-style
// "initialization". Also, the Register objects cannot be const as this
// forces initialization stubs in MSVC, making us dependent on initialization
// order.
//
// 3) By not using an enum, we are possibly preventing the compiler from
// doing certain constant folds, which may significantly reduce the
// code generated for some assembly instructions (because they boil down
// to a few constants). If this is a problem, we could change the code
// such that we use an enum in optimized mode, and the struct in debug
// mode. This way we get the compile-time error checking in debug mode
// and best performance in optimized code.

// Core register
struct Register {
  static const int kNumRegisters = 16;
  static const int kSizeInBytes = kPointerSize;

#if V8_TARGET_LITTLE_ENDIAN
  static const int kMantissaOffset = 0;
  static const int kExponentOffset = 4;
#else
  static const int kMantissaOffset = 4;
  static const int kExponentOffset = 0;
#endif

  static const int kAllocatableRangeBegin = 2;
  static const int kAllocatableRangeEnd = 9;
  static const int kAllocatableContext = 13;  // cp
  static const int kNumAllocatable =
      kAllocatableRangeEnd - kAllocatableRangeBegin + 1;
  static const int kMaxNumAllocatableRegisters =
      kNumAllocatable + 1;  // cp
  static int NumAllocatableRegisters() { return kMaxNumAllocatableRegisters; }

  static int ToAllocationIndex(Register reg) {
    int index;
    int code = reg.code();
    if (code == kAllocatableContext) {
      // Context is the last index
      index = NumAllocatableRegisters() - 1;
    } else {
      // r0-r1 are skipped
      index = code - kAllocatableRangeBegin;
    }
    DCHECK(index >= 0 && index < kMaxNumAllocatableRegisters);
    return index;
  }

  static Register FromAllocationIndex(int index) {
    DCHECK(index >= 0 && index < kMaxNumAllocatableRegisters);
    return index == kMaxNumAllocatableRegisters - 1 ?
      from_code(kAllocatableContext) :  // Last index is always 'cp' register.
      from_code(index + kAllocatableRangeBegin);  // r0-r1 are skipped
  }

  static const char* AllocationIndexToString(int index) {
    DCHECK(index >= 0 && index < kMaxNumAllocatableRegisters);
    const char* const names[] = {
        "r2", "r3", "r4", "r5", "r6", "r7", "r8", "r9", "cp",
    };
    return names[index];
  }

  static Register from_code(int code) {
    Register r = {code};
    return r;
  }

  bool is_valid() const { return 0 <= code_ && code_ < kNumRegisters; }
  bool is(Register reg) const { return code_ == reg.code_; }
  int code() const {
    DCHECK(is_valid());
    return code_;
  }
  int bit() const {
    DCHECK(is_valid());
    return 1 << code_;
  }

  void set_code(int code) {
    code_ = code;
    DCHECK(is_valid());
  }

  // Unfortunately we can't make this private in a struct.
  int code_;
};

typedef struct Register Register;


// These constants are used in several locations, including static initializers
const int kRegister_no_reg_Code = -1;
const int kRegister_r0_Code = 0;  // general scratch
const int kRegister_r1_Code = 1;
const int kRegister_r2_Code = 2;
const int kRegister_r3_Code = 3;
const int kRegister_r4_Code = 4;
const int kRegister_r5_Code = 5;
const int kRegister_r6_Code = 6;
const int kRegister_r7_Code = 7;
const int kRegister_r8_Code = 8;
const int kRegister_r9_Code = 9;
const int kRegister_r10_Code = 10;  // roots array pointer
const int kRegister_fp_Code = 11;  // frame pointer
const int kRegister_r12_Code = 12;  // ip (general scratch)
const int kRegister_r13_Code = 13;
const int kRegister_r14_Code = 14;
const int kRegister_sp_Code = 15;  // stack pointer

const Register no_reg = {kRegister_no_reg_Code};

// Give alias names to registers
const Register cp = {kRegister_r13_Code};  // JavaScript context pointer
const Register kRootRegister = {kRegister_r10_Code};  // Roots array pointer.

const Register r0  = {kRegister_r0_Code};
// Lithium scratch register - defined in lithium-codegen-s390.h
const Register r1  = {kRegister_r1_Code};
const Register r2  = {kRegister_r2_Code};
const Register r3  = {kRegister_r3_Code};
const Register r4  = {kRegister_r4_Code};
const Register r5  = {kRegister_r5_Code};
const Register r6  = {kRegister_r6_Code};
const Register r7  = {kRegister_r7_Code};
const Register r8  = {kRegister_r8_Code};
const Register r9  = {kRegister_r9_Code};
// Used as roots register.
const Register r10 = {kRegister_r10_Code};
const Register fp  = {kRegister_fp_Code};
// IP - Intra procedural register
const Register ip  = {kRegister_r12_Code};
// CP - Context Register
const Register r13  = {kRegister_r13_Code};
const Register r14  = {kRegister_r14_Code};
const Register sp   = {kRegister_sp_Code};

// Double word FP register.
struct DoubleRegister {
  static const int kNumRegisters = 16;
  static const int kMaxNumRegisters = kNumRegisters;
#ifdef V8_TARGET_ARCH_S390X
  static const int kNumVolatileRegisters = 8;     // d0-d7
#else
  static const int kNumVolatileRegisters = 14;     // d0-d15 except d4 and d6
#endif
  // TODO(JOHN): may not be true
  static const int kAllocatableRangeBegin = 1;
  static const int kAllocatableRangeEnd = 12;
  static const int kNumAllocatable =
      kAllocatableRangeEnd - kAllocatableRangeBegin + 1;
  static const int kMaxNumAllocatableRegisters =
      kNumAllocatable;
  static int NumAllocatableRegisters() { return kMaxNumAllocatableRegisters; }

  // TODO(turbofan)
  inline static int NumAllocatableAliasedRegisters() {
    return NumAllocatableRegisters();
  }

  static int ToAllocationIndex(DoubleRegister reg) {
    int code = reg.code();
    int index = code - kAllocatableRangeBegin;
    DCHECK(index < kMaxNumAllocatableRegisters);
    return index;
  }

  static DoubleRegister FromAllocationIndex(int index) {
    DCHECK(index >= 0 && index < kMaxNumAllocatableRegisters);
    return (from_code(index + kAllocatableRangeBegin));  // d0 is skipped
  }

  static const char* AllocationIndexToString(int index);
  static DoubleRegister from_code(int code) {
    DoubleRegister r = {code};
    return r;
  }

  bool is_valid() const { return 0 <= code_ && code_ < kNumRegisters; }
  bool is(DoubleRegister reg) const { return code_ == reg.code_; }

  int code() const {
    DCHECK(is_valid());
    return code_;
  }
  int bit() const {
    DCHECK(is_valid());
    return 1 << code_;
  }
  void split_code(int* vm, int* m) const {
    DCHECK(is_valid());
    *m = (code_ & 0x10) >> 4;
    *vm = code_ & 0x0F;
  }

  int code_;
};


typedef DoubleRegister DoubleRegister;

const DoubleRegister no_dreg = {-1};
const DoubleRegister d0 = {0};
const DoubleRegister d1 = {1};
const DoubleRegister d2 = {2};
const DoubleRegister d3 = {3};
const DoubleRegister d4 = {4};
const DoubleRegister d5 = {5};
const DoubleRegister d6 = {6};
const DoubleRegister d7 = {7};
const DoubleRegister d8 = {8};
const DoubleRegister d9 = {9};
const DoubleRegister d10 = {10};
const DoubleRegister d11 = {11};
const DoubleRegister d12 = {12};
const DoubleRegister d13 = {13};
const DoubleRegister d14 = {14};
const DoubleRegister d15 = {15};

// Aliases for double registers.  Defined using #define instead of
// "static const DoubleRegister&" because Clang complains otherwise when a
// compilation unit that includes this header doesn't use the variables.
#define kDoubleRegZero d14
#define kScratchDoubleReg d13

Register ToRegister(int num);

// Coprocessor register
struct CRegister {
  bool is_valid() const { return 0 <= code_ && code_ < 16; }
  bool is(CRegister creg) const { return code_ == creg.code_; }
  int code() const {
    DCHECK(is_valid());
    return code_;
  }
  int bit() const {
    DCHECK(is_valid());
    return 1 << code_;
  }

  // Unfortunately we can't make this private in a struct.
  int code_;
};


const CRegister no_creg = {-1};

const CRegister cr0 = {0};
const CRegister cr1 = {1};
const CRegister cr2 = {2};
const CRegister cr3 = {3};
const CRegister cr4 = {4};
const CRegister cr5 = {5};
const CRegister cr6 = {6};
const CRegister cr7 = {7};
const CRegister cr8 = {8};
const CRegister cr9 = {9};
const CRegister cr10 = {10};
const CRegister cr11 = {11};
const CRegister cr12 = {12};
const CRegister cr13 = {13};
const CRegister cr14 = {14};
const CRegister cr15 = {15};

// -----------------------------------------------------------------------------
// Machine instruction Operands

#if V8_TARGET_ARCH_S390X
const RelocInfo::Mode kRelocInfo_NONEPTR = RelocInfo::NONE64;
#else
const RelocInfo::Mode kRelocInfo_NONEPTR = RelocInfo::NONE32;
#endif

// Class Operand represents a shifter operand in data processing instructions
// defining immediate numbers and masks
typedef uint8_t Length;

struct Mask {
  uint8_t mask;
  uint8_t value() {return mask;}
  static Mask from_value(uint8_t input) {
    DCHECK(input <= 0x0F);
    Mask m = {input};
    return m;
  }
};

class Operand BASE_EMBEDDED {
 public:
  // immediate
  INLINE(explicit Operand(intptr_t immediate,
                          RelocInfo::Mode rmode = kRelocInfo_NONEPTR));
  INLINE(static Operand Zero()) { return Operand(static_cast<intptr_t>(0)); }
  INLINE(explicit Operand(const ExternalReference& f));
  explicit Operand(Handle<Object> handle);
  INLINE(explicit Operand(Smi* value));

  // rm
  INLINE(explicit Operand(Register rm));

  // Return true if this is a register operand.
  INLINE(bool is_reg() const);

  bool must_output_reloc_info(const Assembler* assembler) const;

  inline intptr_t immediate() const {
    DCHECK(!rm_.is_valid());
    return imm_;
  }

  inline void setBits(int n) {
    imm_ = (static_cast<uint32_t>(imm_) << (32 - n)) >> (32 - n);
  }

  Register rm() const { return rm_; }

 private:
  Register rm_;
  intptr_t imm_;  // valid if rm_ == no_reg
  RelocInfo::Mode rmode_;

  friend class Assembler;
  friend class MacroAssembler;
};

typedef int32_t Disp;

// Class MemOperand represents a memory operand in load and store instructions
// On S390, we have various flavours of memory operands:
//   1) a base register + 16 bit unsigned displacement
//   2) a base register + index register + 16 bit unsigned displacement
//   3) a base register + index register + 20 bit signed displacement
class MemOperand BASE_EMBEDDED {
 public:
  explicit MemOperand(Register rx, Disp offset = 0);
  explicit MemOperand(Register rx, Register rb, Disp offset = 0);

  int32_t offset() const {
    return offset_;
  }
  uint32_t getDisplacement() const { return offset(); }

  // Base register
  Register rb() const {
    DCHECK(!baseRegister.is(no_reg));
    return baseRegister;
  }

  Register getBaseRegister() const { return rb(); }

  // Index Register
  Register rx() const {
    DCHECK(!indexRegister.is(no_reg));
    return indexRegister;
  }
  Register getIndexRegister() const { return rx(); }

 private:
  Register baseRegister;     // base
  int32_t offset_;  // offset
  Register indexRegister;     // index

  friend class Assembler;
};


class DeferredRelocInfo {
 public:
  DeferredRelocInfo() {}
  DeferredRelocInfo(int position, RelocInfo::Mode rmode, intptr_t data)
      : position_(position), rmode_(rmode), data_(data) {}

  int position() const { return position_; }
  RelocInfo::Mode rmode() const { return rmode_; }
  intptr_t data() const { return data_; }

 private:
  int position_;
  RelocInfo::Mode rmode_;
  intptr_t data_;
};

class Assembler : public AssemblerBase {
 public:
  // Create an assembler. Instructions and relocation information are emitted
  // into a buffer, with the instructions starting from the beginning and the
  // relocation information starting from the end of the buffer. See CodeDesc
  // for a detailed comment on the layout (globals.h).
  //
  // If the provided buffer is NULL, the assembler allocates and grows its own
  // buffer, and buffer_size determines the initial buffer size. The buffer is
  // owned by the assembler and deallocated upon destruction of the assembler.
  //
  // If the provided buffer is not NULL, the assembler uses the provided buffer
  // for code generation and assumes its size to be buffer_size. If the buffer
  // is too small, a fatal error occurs. No deallocation of the buffer is done
  // upon destruction of the assembler.
  Assembler(Isolate* isolate, void* buffer, int buffer_size);
  virtual ~Assembler() {}

  // GetCode emits any pending (non-emitted) code and fills the descriptor
  // desc. GetCode() is idempotent; it returns the same result if no other
  // Assembler functions are invoked in between GetCode() calls.
  void GetCode(CodeDesc* desc);

  // Label operations & relative jumps (PPUM Appendix D)
  //
  // Takes a branch opcode (cc) and a label (L) and generates
  // either a backward branch or a forward branch and links it
  // to the label fixup chain. Usage:
  //
  // Label L;    // unbound label
  // j(cc, &L);  // forward branch to unbound label
  // bind(&L);   // bind label to the current pc
  // j(cc, &L);  // backward branch to bound label
  // bind(&L);   // illegal: a label may be bound only once
  //
  // Note: The same Label can be used for forward and backward branches
  // but it may be bound only once.

  void bind(Label* L);  // binds an unbound label L to the current code position

  // Links a label at the current pc_offset().  If already bound, returns the
  // bound position.  If already linked, returns the position of the prior link.
  // Otherwise, returns the current pc_offset().
  int link(Label* L);

  // Determines if Label is bound and near enough so that a single
  // branch instruction can be used to reach it.
  bool is_near(Label* L, Condition cond);

  // Returns the branch offset to the given label from the current code position
  // Links the label to the current position if it is still unbound
  // Manages the jump elimination optimization if the second parameter is true.
  int branch_offset(Label* L, bool jump_elimination_allowed) {
    int position = link(L);
    return position - pc_offset();
  }

  // Puts a labels target address at the given position.
  // The high 8 bits are set to zero.
  void label_at_put(Label* L, int at_offset);
  void load_label_offset(Register r1, Label* L);

  // Read/Modify the code target address in the branch/call instruction at pc.
  INLINE(static Address target_address_at(Address pc,
                                          Address constant_pool));
  INLINE(static void set_target_address_at(
      Address pc, Address constant_pool, Address target,
      ICacheFlushMode icache_flush_mode = FLUSH_ICACHE_IF_NEEDED));
  INLINE(static Address target_address_at(Address pc, Code* code)) {
    Address constant_pool = NULL;
    return target_address_at(pc, constant_pool);
  }
  INLINE(static void set_target_address_at(
      Address pc, Code* code, Address target,
      ICacheFlushMode icache_flush_mode = FLUSH_ICACHE_IF_NEEDED)) {
    Address constant_pool = NULL;
    set_target_address_at(pc, constant_pool, target, icache_flush_mode);
  }

  // Return the code target address at a call site from the return address
  // of that call in the instruction stream.
  inline static Address target_address_from_return_address(Address pc);

  // Given the address of the beginning of a call, return the address
  // in the instruction stream that the call will return to.
  INLINE(static Address return_address_from_call_start(Address pc));

  // Return the code target address of the patch debug break slot
  INLINE(static Address break_address_from_return_address(Address pc));

  inline Handle<Object> code_target_object_handle_at(Address pc);
  // This sets the branch destination.
  // This is for calls and branches within generated code.
  inline static void deserialization_set_special_target_at(
      Address instruction_payload, Code* code, Address target);

  // This sets the internal reference at the pc.
  inline static void deserialization_set_target_internal_reference_at(
      Address pc, Address target,
      RelocInfo::Mode mode = RelocInfo::INTERNAL_REFERENCE);

  // Size of an instruction.
  static const int kInstrSize = sizeof(Instr);

  // Here we are patching the address in the LUI/ORI instruction pair.
  // These values are used in the serialization process and must be zero for
  // PPC platform, as Code, Embedded Object or External-reference pointers
  // are split across two consecutive instructions and don't exist separately
  // in the code, so the serializer should not step forwards in memory after
  // a target is resolved and written.
  static const int kSpecialTargetSize = 0;

  // Number of bytes for instructions used to store pointer sized constant.
#if V8_TARGET_ARCH_S390X
  static const int kBytesForPtrConstant = 12;  // iihf + iilf
#else
  static const int kBytesForPtrConstant = 6;   // iilf
#endif

  // Distance between the instruction referring to the address of the call
  // target and the return address.

  // Offset between call target address and return address
  // for BRASL calls
  // Patch will be appiled to other FIXED_SEQUENCE call
  static const int kCallTargetAddressOffset = 6;

  // The length of FIXED_SEQUENCE call
  // iihf    r8, <address_hi>  // <64-bit only>
  // iilf    r8, <address_lo>
  // basr    r14, r8
#if V8_TARGET_ARCH_S390X
  static const int kCallSequenceLength = 14;
#else
  static const int kCallSequenceLength = 8;
#endif


  // This is the length of the BreakLocationIterator::SetDebugBreakAtReturn()
  // code patch FIXED_SEQUENCE in bytes!
  // JS Return Sequence = Call Sequence + BKPT
  static const int kJSReturnSequenceLength = kCallSequenceLength + 2;

  // This is the length of the code sequence from SetDebugBreakAtSlot()
  // FIXED_SEQUENCE in bytes!
  static const int kDebugBreakSlotLength = kCallSequenceLength;
  static const int kPatchDebugBreakSlotReturnOffset = kCallTargetAddressOffset;

  // Length to patch between the start of the JS return sequence
  // from SetDebugBreakAtReturn and the address from
  // break_address_from_return_address.
  //
  // frame->pc() in Debug::SetAfterBreakTarget will point to BKPT in
  // JS return sequence, so the length to patch will not include BKPT
  // instruction length.
  static const int kPatchReturnSequenceAddressOffset =
      kCallSequenceLength - kPatchDebugBreakSlotReturnOffset;

  // Length to patch between the start of the FIXED call sequence from
  // SetDebugBreakAtSlot() and the the address from
  // break_address_from_return_address.
  static const int kPatchDebugBreakSlotAddressOffset =
      kDebugBreakSlotLength - kPatchDebugBreakSlotReturnOffset;

  static inline int encode_crbit(const CRegister& cr, enum CRBit crbit) {
    return ((cr.code() * CRWIDTH) + crbit);
  }

  // ---------------------------------------------------------------------------
  // Code generation

  // Helper for unconditional branch to Label with update to save register
  void b(Register r, Label* l) {
    positions_recorder()->WriteRecordedPositions();
    int32_t halfwords = branch_offset(l, false) / 2;
    brasl(r, Operand(halfwords));
  }

  // Conditional Branch Instruction - Generates either BRC / BRCL
  void branchOnCond(Condition c, int branch_offset, bool is_bound = false);

  // Helpers for conditional branch to Label
  void b(Condition cond, Label* l, Label::Distance dist = Label::kFar) {
    branchOnCond(cond, branch_offset(l, false),
                 l->is_bound() || (dist == Label::kNear));
  }

  void bc_short(Condition cond, Label* l, Label::Distance dist = Label::kFar) {
    b(cond, l, Label::kNear);
  }
  // Helpers for conditional branch to Label
  void beq(Label * l, Label::Distance dist = Label::kFar) { b(eq, l, dist); }
  void bne(Label * l, Label::Distance dist = Label::kFar) { b(ne, l, dist); }
  void blt(Label * l, Label::Distance dist = Label::kFar) { b(lt, l, dist); }
  void ble(Label * l, Label::Distance dist = Label::kFar) { b(le, l, dist); }
  void bgt(Label * l, Label::Distance dist = Label::kFar) { b(gt, l, dist); }
  void bge(Label * l, Label::Distance dist = Label::kFar) { b(ge, l, dist); }
  void b(Label * l, Label::Distance dist = Label::kFar)   { b(al, l, dist); }
  void jmp(Label * l, Label::Distance dist = Label::kFar) { b(al, l, dist); }
  void bunordered(Label* l, Label::Distance dist = Label::kFar) {
                                                     b(unordered, l, dist); }
  void bordered(Label* l, Label::Distance dist = Label::kFar) {
                                                       b(ordered, l, dist); }

  // Helpers for conditional indirect branch off register
  void b(Condition cond, Register r) { bcr(cond, r); }
  void beq(Register r) { b(eq, r); }
  void bne(Register r) { b(ne, r); }
  void blt(Register r) { b(lt, r); }
  void ble(Register r) { b(le, r); }
  void bgt(Register r) { b(gt, r); }
  void bge(Register r) { b(ge, r); }
  void b(Register r)   { b(al, r); }
  void jmp(Register r) { b(al, r); }
  void bunordered(Register r) { b(unordered, r); }
  void bordered(Register r)   { b(ordered, r);   }

  // S390 native instructions
  // Indirect Conditional Branch via register
  void bcr(Condition m, Register target);

  // Conditional Branch Relative Long
  void brcl(Condition m, const Operand& opnd, bool isCodeTarget = false);
  // ---------------------------------------------------------------------------
  // Code generation

  // Insert the smallest number of nop instructions
  // possible to align the pc offset to a multiple
  // of m. m must be a power of 2 (>= 4).
  void Align(int m);
  // Insert the smallest number of zero bytes possible to align the pc offset
  // to a mulitple of m. m must be a power of 2 (>= 2).
  void DataAlign(int m);
  // Aligns code to something that's optimal for a jump target for the platform.
  void CodeTargetAlign();

  void breakpoint(bool do_print) {
    if (do_print) {
      printf("DebugBreak is inserted to %p\n", pc_);
    }
#if V8_HOST_ARCH_64_BIT
    int64_t value = reinterpret_cast<uint64_t>(&v8::base::OS::DebugBreak);
    int32_t hi_32 = static_cast<int64_t>(value) >> 32;
    int32_t lo_32 = static_cast<int32_t>(value);

    iihf(r1, Operand(hi_32));
    iilf(r1, Operand(lo_32));
#else
    iilf(r1, Operand(reinterpret_cast<uint32_t>(&v8::base::OS::DebugBreak)));
#endif
    basr(r14, r1);
  }

  // Load address relative long.
  void larl(Register r, Label *l) {
    larl(r, Operand(branch_offset(l, false)));
  }

  void call(Handle<Code> target, RelocInfo::Mode rmode,
            TypeFeedbackId ast_id = TypeFeedbackId::None());
  void jump(Handle<Code> target, RelocInfo::Mode rmode, Condition cond);

  void mvc(const MemOperand& opnd1, const MemOperand& opnd2, uint32_t length);
  void asi(const MemOperand&, const Operand&);
  void agsi(const MemOperand&, const Operand&);

  // Data-processing instructions

  // S390 instruction generation
#define E_FORM(name)\
void name()

#define IE_FORM(name)\
void name(const Operand& i1, const Operand& i2)

#define I_FORM(name)\
void name(const Operand& i)

#define RR_FORM(name)\
void name(Register r1, Register r2)

#define RR2_FORM(name)\
void name(Condition m1, Register r2)

#define RX_FORM(name)\
void name(Register r1, Register x2, Register b2, \
                 Disp d2);\
void name(Register r1, const MemOperand& opnd)

#define RI1_FORM(name)\
void name(Register r,  const Operand& i)

#define RI2_FORM(name)\
void name(Condition m, const Operand& i)

#define RIE_FORM(name)\
void name(Register r1, Register R3, const Operand& i)

#define RIE_F_FORM(name)\
void name(Register r1, Register r2, const Operand &i3, \
                     const Operand& i4, const Operand& i5)

#define RIL1_FORM(name)\
void name(Register r1, const Operand& i2)

#define RIL2_FORM(name)\
void name(Condition m1, const Operand& i2)

#define RXE_FORM(name)\
void name(Register r1, const MemOperand& opnd);\
void name(Register r1, Register b2, Register x2, \
          Disp d2)

#define RXF_FORM(name)\
void name(Register r1, Register r3, const MemOperand& opnd);\
void name(Register r1, Register r3, Register b2, \
                 Register x2, Disp d2)

#define RXY_FORM(name)\
void name(Register r1, Register x2, Register b2, \
                 Disp d2);\
void name(Register r1, const MemOperand& opnd)

#define RSI_FORM(name)\
void name(Register r1, Register r3, const Operand& i)

#define RIS_FORM(name)\
void name(Register r1, Condition m3, Register b4, \
          Disp d4, const Operand& i2);\
void name(Register r1, const Operand& i2, Condition m3, \
          const MemOperand& opnd)

#define SI_FORM(name)\
void name(const MemOperand& opnd, const Operand& i);\
void name(const Operand& i2, Register b1, Disp d1)

#define SIL_FORM(name)\
void name(Register b1, Disp d1, const Operand& i2);\
void name(const MemOperand& opnd, const Operand& i2)

#define RRE_FORM(name)\
void name(Register r1, Register r2)

#define RRF1_FORM(name)\
void name(Register r1, Register r2, Register r3)

#define RRF2_FORM(name)\
void name(Condition m1, Register r1, Register r2)

#define RRF3_FORM(name)\
void name(Register r3, Condition m4, Register r1, Register r2)

#define RS1_FORM(name)\
void name(Register r1, Register r3, const MemOperand& opnd);\
void name(Register r1, Register r3, Register b2, Disp d2)

#define RS2_FORM(name)\
void name(Register r1, Condition m3, const MemOperand& opnd);\
void name(Register r1, Condition m3, Register b2, Disp d2)

#define RSE_FORM(name)\
void name(Register r1, Register r3, const MemOperand& opnd);\
void name(Register r1, Register r3, Register b2, Disp d2)

#define RSL_FORM(name)\
void name(Length l, Register b2, Disp d2);\
void name(const MemOperand& opnd)

#define RSY1_FORM(name)\
void name(Register r1, Register r3, Register b2, Disp d2);\
void name(Register r1, Register r3, const MemOperand& opnd)

#define RSY2_FORM(name)\
void name(Register r1, Condition m3, Register b2, Disp d2);\
void name(Register r1, Condition m3, const MemOperand& opnd)

#define RRD_FORM(name)\
void name(Register r1, Register r3, Register r2)

#define RRS_FORM(name)\
void name(Register r1, Register r2, Register b4, \
          Disp d4, Condition m3);\
void name(Register r1, Register r2, Condition m3, \
          const MemOperand& opnd)

#define S_FORM(name)\
void name(Register b2, Disp d2);\
void name(const MemOperand& opnd)

#define SIY_FORM(name)\
void name(const Operand& i2, Register b1, Disp d1);\
void name(const MemOperand& opnd, const Operand& i)

#define SS1_FORM(name)\
void name(Register b1, Disp d1, \
          Register b3, Disp d2, Length length);\
void name(const MemOperand& opnd1, const MemOperand& opnd2, Length length)

#define SS2_FORM(name)\
void name(const MemOperand& opnd1, const MemOperand& opnd2, \
          Length length1, Length length2);\
void name(Register b1, \
          Disp d1, Register b2, Disp d2, Length l1, Length l2)

#define SS3_FORM(name)\
void name(const MemOperand& opnd1, const MemOperand& opnd2, Length length);\
void name(const Operand& i3, Register b1, \
          Disp d1, Register b2, Disp d2, Length l1)

#define SS4_FORM(name)\
void name(const MemOperand& opnd1, const MemOperand& opnd2);\
void name(Register r1, Register r3, Register b1, \
          Disp d1, Register b2, Disp d2)

#define SS5_FORM(name)\
void name(const MemOperand& opnd1, const MemOperand& opnd2);\
void name(Register r1, Register r3, Register b3, \
          Disp d2, Register b4, Disp d4)

#define SSE_FORM(name)\
void name(Register b1, Disp d1, \
          Register b2, Disp d2);\
void name(const MemOperand& opnd1, const MemOperand& opnd2)

#define SSF_FORM(name)\
void name(Register r3, Register b1, Disp d1, \
          Register b2, Disp d2);\
void name(Register r3, const MemOperand& opnd1, const MemOperand& opnd2)

// S390 instruction sets
RX_FORM(a);
RXE_FORM(adb);
RRE_FORM(adbr);
RIL1_FORM(afi);
RXY_FORM(ag);
RXY_FORM(agf);
RIL1_FORM(agfi);
RRE_FORM(agfr);
RI1_FORM(aghi);
RRE_FORM(agr);
RX_FORM(ah);
RI1_FORM(ahi);
RXY_FORM(ahy);
RX_FORM(al_z);
RIL1_FORM(alfi);
RXY_FORM(alg);
RIL1_FORM(algfi);
RRE_FORM(algr);
RR_FORM(alr);
RXY_FORM(aly);
SS2_FORM(ap);
RR_FORM(ar);
RXY_FORM(ay);
RX_FORM(bal);
RR_FORM(basr);
RX_FORM(bc);
RX_FORM(bct);
RXY_FORM(bctg);
RR_FORM(bctr);
RI1_FORM(bras);
RIL1_FORM(brasl);
RI2_FORM(brc);
RX_FORM(c);
RX_FORM(cd);
RRE_FORM(cdr);
RXE_FORM(cdb);
RXE_FORM(ceb);
RRE_FORM(cefbr);
RRF2_FORM(cfebr);
RIL1_FORM(cfi);
RXY_FORM(cg);
RIL1_FORM(cgfi);
RI1_FORM(cghi);
RX_FORM(ch);
RI1_FORM(chi);
RXY_FORM(chy);
RX_FORM(cl);
SS1_FORM(clc);
RIL1_FORM(clfi);
RXY_FORM(clg);
RIL1_FORM(clgfi);
RXY_FORM(cly);
RR_FORM(cr_z);
RXY_FORM(cy);
RX_FORM(d);
RXE_FORM(ddb);
RRE_FORM(ddbr);
RXY_FORM(dl);
RRE_FORM(dlr);
SS2_FORM(dp);
RR_FORM(dr);
RRE_FORM(dsgr);
SS1_FORM(ed);
RRE_FORM(epair);
RX_FORM(ex);
RRF2_FORM(fidbr);
RRE_FORM(flogr);
RX_FORM(ic_z);
RXY_FORM(icy);
RIL1_FORM(iihf);
RI1_FORM(iihh);
RI1_FORM(iihl);
RIL1_FORM(iilf);
RI1_FORM(iilh);
RI1_FORM(iill);
RX_FORM(l);
RSY1_FORM(lang);
RIL1_FORM(larl);
RSY1_FORM(lax);
RXY_FORM(lb);
RRE_FORM(lbr);
RRE_FORM(lcgr);
RR_FORM(lcr);
RX_FORM(le_z);
RXY_FORM(ley);
RXY_FORM(lg);
RXY_FORM(lgb);
RRE_FORM(lgbr);
RXY_FORM(lgf);
RRE_FORM(lgfr);
RXY_FORM(lgh);
RI1_FORM(lghi);
RRE_FORM(lghr);
RRE_FORM(lgr);
RX_FORM(lh);
RRE_FORM(lhr);
RXY_FORM(lhy);
RXY_FORM(llgf);
RRE_FORM(llgfr);
RXY_FORM(llgh);
RRE_FORM(llghr);
RXY_FORM(llh);
RRE_FORM(llhr);
RIL1_FORM(llihf);
RIL1_FORM(llilf);
RRE_FORM(lngr);
RR_FORM(lnr);
RSY1_FORM(loc);
RR_FORM(lr);
RXY_FORM(lrv);
RXY_FORM(lrvh);
RRE_FORM(ltgfr);
RRE_FORM(ltgr);
RR_FORM(ltr);
RXY_FORM(ly);
RX_FORM(m);
RXE_FORM(mdb);
RRE_FORM(mdbr);
RI1_FORM(mghi);
RX_FORM(mh);
RI1_FORM(mhi);
RXY_FORM(mhy);
RXY_FORM(ml);
RXY_FORM(mlg);
RRE_FORM(mlgr);
RRE_FORM(mlr);
RR_FORM(mr_z);
RX_FORM(ms);
RIL1_FORM(msfi);
RXY_FORM(msg);
RIL1_FORM(msgfi);
RRE_FORM(msgr);
RRE_FORM(msr);
RXY_FORM(msy);
SS4_FORM(mvck);
SSF_FORM(mvcos);
SS4_FORM(mvcs);
SS1_FORM(mvn);
RX_FORM(n);
SS1_FORM(nc);
RXY_FORM(ng);
RRE_FORM(ngr);
SI_FORM(ni);
RIL1_FORM(nihf);
RIL1_FORM(nilf);
RI1_FORM(nilh);
RI1_FORM(nill);
RR_FORM(nr);
RXY_FORM(ny);
RX_FORM(o);
RXY_FORM(og);
RRE_FORM(ogr);
RIL1_FORM(oihf);
RIL1_FORM(oilf);
RI1_FORM(oill);
RR_FORM(or_z);
RXY_FORM(oy);
SS2_FORM(pack);
// RRE_FORM(popcnt);
// RSY1_FORM(rll);
// RSY1_FORM(rllg);
RX_FORM(s);
S_FORM(sal);
RRE_FORM(sar);
RXE_FORM(sdb);
RRE_FORM(sdbr);
RXY_FORM(sg);
RXY_FORM(sgf);
RRE_FORM(sgfr);
RRE_FORM(sgr);
RX_FORM(sh);
RXY_FORM(shy);
RX_FORM(sl);
RIL1_FORM(slfi);
RXY_FORM(slg);
RXY_FORM(slgf);
RIL1_FORM(slgfi);
RRE_FORM(slgr);
RR_FORM(slr);
RXY_FORM(sly);
RR_FORM(sr);
RS1_FORM(srdl);
RX_FORM(ste);
RXY_FORM(stey);
RXY_FORM(strv);
I_FORM(svc);
RXY_FORM(sy);
RI1_FORM(tmll);
RSL_FORM(tp);
SS1_FORM(tr);
S_FORM(ts);
RX_FORM(x);
SS1_FORM(xc);
RXY_FORM(xg);
RRE_FORM(xgr);
RIL1_FORM(xihf);
RIL1_FORM(xilf);
RR_FORM(xr);
RXY_FORM(xy);
SS2_FORM(zap);


  // Load Address Instructions
  void la(Register r1, const MemOperand& src);
  void lay(Register r1, const MemOperand& src);

  // Load Instructions
  void lt_z(Register r1, const MemOperand& src);
  void ltg(Register r1, const MemOperand& src);

  // Load Logical Byte Instructions (aka. chars)
  void llc(Register r1, const MemOperand& src);
  void llgc(Register r1, const MemOperand& src);

  // Load Multiple Instructions
  void lm(Register r1, Register r2, const MemOperand& src);
  void lmy(Register r1, Register r2, const MemOperand& src);
  void lmg(Register r1, Register r2, const MemOperand& src);

  // Store Instructions
  void stm(Register r1, Register r2, const MemOperand& src);
  void stmy(Register r1, Register r2, const MemOperand& src);
  void stmg(Register r1, Register r2, const MemOperand& src);
  void st(Register dst, const MemOperand& src);
  void sty(Register dst, const MemOperand& src);
  void sth(Register dst, const MemOperand& src);
  void sthy(Register dst, const MemOperand& src);
  void stc(Register dst, const MemOperand& src);
  void stcy(Register dst, const MemOperand& src);

  // Compare Instructions
  void cr(Register r1, Register r2);
  void cgr(Register r1, Register r2);
  void clr(Register r1, Register r2);
  void clgr(Register r1, Register r2);
  void cli(const MemOperand& mem, const Operand& imm);
  void cliy(const MemOperand& mem, const Operand& imm);

  // Test Under Mask Instructions
  void tm(const MemOperand& mem, const Operand& imm);
  void tmy(const MemOperand& mem, const Operand& imm);

  // Rotate Instruction
  void rll(Register r1, Register r3, Register opnd);
  void rll(Register r1, Register r3, const Operand& opnd);
  void rll(Register r1, Register r3, Register r2, const Operand& opnd);
  void rllg(Register r1, Register r3, const Operand& opnd);
  void rllg(Register r1, Register r3, const Register opnd);
  void rllg(Register r1, Register r3, Register r2, const Operand& opnd);

  // Shift Instruction (32)
  void sll(Register r1, Register opnd);
  void sll(Register r1, const Operand& opnd);
  void sllk(Register r1, Register r3, Register opnd);
  void sllk(Register r1, Register r3, const Operand& opnd);
  void srl(Register r1, Register opnd);
  void srl(Register r1, const Operand& opnd);
  void srlk(Register r1, Register r3, Register opnd);
  void srlk(Register r1, Register r3, const Operand& opnd);
  void sra(Register r1, Register opnd);
  void sra(Register r1, const Operand& opnd);
  void srak(Register r1, Register r3, Register opnd);
  void srak(Register r1, Register r3, const Operand& opnd);
  void sla(Register r1, Register opnd);
  void sla(Register r1, const Operand& opnd);
  void slak(Register r1, Register r3, Register opnd);
  void slak(Register r1, Register r3, const Operand& opnd);

  // Data-processing instructions

  void sub(Register dst, Register src1, Register src2,
           OEBit s = LeaveOE, RCBit r = LeaveRC);

  // Shift Instructions (64)
  void sllg(Register r1, Register r3, const Operand& opnd);
  void sllg(Register r1, Register r3, const Register opnd);
  void srlg(Register r1, Register r3, const Operand& opnd);
  void srlg(Register r1, Register r3, const Register opnd);
  void srag(Register r1, Register r3, const Operand& opnd);
  void srag(Register r1, Register r3, const Register opnd);
  void srda(Register r1, const Operand& opnd);
  void srdl(Register r1, const Operand& opnd);
  void slag(Register r1, Register r3, const Operand& opnd);
  void slag(Register r1, Register r3, const Register opnd);

  // Rotate and Insert Selected Bits
  void risbg(Register dst, Register src, const Operand& startBit,
             const Operand& endBit, const Operand& shiftAmt,
             bool zeroBits = true);
  void risbgn(Register dst, Register src, const Operand& startBit,
              const Operand& endBit, const Operand& shiftAmt,
              bool zeroBits = true);

  // Arithmetic Instructions
  void ahik(Register r1, Register r3, const Operand& opnd);
  void ark(Register r1, Register r2, Register r3);
  void alrk(Register r1, Register r2, Register r3);
  void aghik(Register r1, Register r3, const Operand& opnd);
  void agrk(Register r1, Register r2, Register r3);
  void algrk(Register r1, Register r2, Register r3);
  void srk(Register r1, Register r2, Register r3);
  void slrk(Register r1, Register r2, Register r3);
  void sgrk(Register r1, Register r2, Register r3);
  void slgrk(Register r1, Register r2, Register r3);

  // Bitwise Instructions
  void nrk(Register r1, Register r2, Register r3);
  void ngrk(Register r1, Register r2, Register r3);
  void ork(Register r1, Register r2, Register r3);
  void ogrk(Register r1, Register r2, Register r3);
  void xrk(Register r1, Register r2, Register r3);
  void xgrk(Register r1, Register r2, Register r3);


  // GPR <-> FPR conversions
  void lgdr(Register r1, DoubleRegister f2);
  void ldgr(DoubleRegister f1, Register r2);

  // floating point instructions
  void ld(DoubleRegister r1, const MemOperand& opnd);
  void ldy(DoubleRegister r1, const MemOperand& opnd);
  void le_z(DoubleRegister r1, const MemOperand& opnd);
  void ley(DoubleRegister r1, const MemOperand& opnd);
  void ldr(DoubleRegister r1, DoubleRegister r2);
  void std(DoubleRegister r1, const MemOperand& opnd);
  void stdy(DoubleRegister r1, const MemOperand& opnd);
  void ste(DoubleRegister r1, const MemOperand& opnd);
  void stey(DoubleRegister r1, const MemOperand& opnd);

  void ledbr(DoubleRegister r1, DoubleRegister r2);
  void ldebr(DoubleRegister r1, DoubleRegister r2);
  void lpdbr(DoubleRegister r1,  DoubleRegister r2);
  // double type conversion
  void cfdbr(Condition m, Register fixReg, DoubleRegister fltReg);
  void cdfbr(DoubleRegister fltReg, Register fixReg);
  void cgdbr(Condition m, Register fixReg, DoubleRegister fltReg);
  void cdgbr(DoubleRegister fltReg, Register fixReg);
  void cdlfbr(Condition m3, Condition m4,
              DoubleRegister fltReg, Register fixReg);
  void cdlgbr(Condition m3, Condition m4,
              DoubleRegister fltReg, Register fixReg);
  void clfdbr(Condition m3, Condition m4,
              Register fixReg, DoubleRegister fltReg);
  void clgdbr(Condition m3, Condition m4,
              Register fixReg, DoubleRegister fltReg);

  // float type conversion
  void cfebr(Register fixReg, DoubleRegister fltReg);
  void cefbr(DoubleRegister fltReg, Register fixReg);

  void cdb(DoubleRegister r1, const MemOperand& opnd);
  void cdbr(DoubleRegister r1, DoubleRegister r2);
  void adb(DoubleRegister r1, const MemOperand& opnd);
  void adbr(DoubleRegister r1, DoubleRegister r2);
  void lzdr(DoubleRegister r1);
  void sdb(DoubleRegister r1, const MemOperand& opnd);
  void sdbr(DoubleRegister r1, DoubleRegister r2);
  void mdb(DoubleRegister r1, const MemOperand& opnd);
  void mdbr(DoubleRegister r1, DoubleRegister r2);
  void ddb(DoubleRegister r1, const MemOperand& opnd);
  void ddbr(DoubleRegister r1, DoubleRegister r2);
  void madbr(DoubleRegister r1, DoubleRegister r2, DoubleRegister r3);
  void msdbr(DoubleRegister r1, DoubleRegister r2, DoubleRegister r3);

  void sqdb(DoubleRegister r1, const MemOperand& opnd);
  void sqdbr(DoubleRegister r1, DoubleRegister r2);
  void lcdbr(DoubleRegister r1, DoubleRegister r2);
  void ldeb(DoubleRegister r1, const MemOperand& opnd);

  enum FIDBRA_MASK3 {
    FIDBRA_CURRENT_ROUNDING_MODE = 0,
    FIDBRA_ROUND_TO_NEAREST_AWAY_FROM_0 = 1,
    // ...
    FIDBRA_ROUND_TOWARD_0 = 5,
    FIDBRA_ROUND_TOWARD_POS_INF = 6,
    FIDBRA_ROUND_TOWARD_NEG_INF = 7
  };
  void fidbra(DoubleRegister d1, DoubleRegister d2, FIDBRA_MASK3 m3);

  // Branch Instructions
  void brct(Register r1, const Operand& opnd);
  void brctg(Register r1, const Operand& opnd);

  // Move integer
  void mvhi(const MemOperand& opnd1, const Operand& i2);
  void mvghi(const MemOperand& opnd1, const Operand& i2);

  void lhi(Register dst, const Operand& imm);

  void stg(Register rs, const MemOperand &src);

  // Exception-generating instructions and debugging support
  void stop(const char* msg,
            Condition cond = al,
            int32_t code = kDefaultStopCode,
            CRegister cr = cr7);

  void bkpt(uint32_t imm16);  // v5 and above

  // Different nop operations are used by the code generator to detect certain
  // states of the generated code.
  enum NopMarkerTypes {
    NON_MARKING_NOP = 0,
    GROUP_ENDING_NOP,
    DEBUG_BREAK_NOP,
    // IC markers.
    PROPERTY_ACCESS_INLINED,
    PROPERTY_ACCESS_INLINED_CONTEXT,
    PROPERTY_ACCESS_INLINED_CONTEXT_DONT_DELETE,
    // Helper values.
    LAST_CODE_MARKER,
    FIRST_IC_MARKER = PROPERTY_ACCESS_INLINED
  };

  void nop(int type = 0);   // 0 is the default non-marking type.

  // Check the code size generated from label to here.
  int SizeOfCodeGeneratedSince(Label* label) {
    return pc_offset() - label->pos();
  }

  // Class for scoping postponing the trampoline pool generation.
  class BlockTrampolinePoolScope {
   public:
    explicit BlockTrampolinePoolScope(Assembler* assem) : assem_(assem) {
      assem_->StartBlockTrampolinePool();
    }
    ~BlockTrampolinePoolScope() { assem_->EndBlockTrampolinePool(); }

   private:
    Assembler* assem_;

    DISALLOW_IMPLICIT_CONSTRUCTORS(BlockTrampolinePoolScope);
  };

  // Debugging

  // Mark address of the ExitJSFrame code.
  void RecordJSReturn();

  // Mark address of a debug break slot.
  void RecordDebugBreakSlot();

  // Record the AST id of the CallIC being compiled, so that it can be placed
  // in the relocation information.
  void SetRecordedAstId(TypeFeedbackId ast_id) {
// PPC - this shouldn't be failing roohack   DCHECK(recorded_ast_id_.IsNone());
    recorded_ast_id_ = ast_id;
  }

  TypeFeedbackId RecordedAstId() {
    // roohack - another issue??? DCHECK(!recorded_ast_id_.IsNone());
    return recorded_ast_id_;
  }

  void ClearRecordedAstId() { recorded_ast_id_ = TypeFeedbackId::None(); }

  // Record a comment relocation entry that can be used by a disassembler.
  // Use --code-comments to enable.
  void RecordComment(const char* msg);

  // Record a deoptimization reason that can be used by a log or cpu profiler.
  // Use --trace-deopt to enable.
  void RecordDeoptReason(const int reason, const SourcePosition position);

  // Writes a single byte or word of data in the code stream.  Used
  // for inline tables, e.g., jump-tables.
  void db(uint8_t data);
  void dd(uint32_t data);
  void dq(uint64_t data);
  void dp(uintptr_t data);

  PositionsRecorder* positions_recorder() { return &positions_recorder_; }

  void PatchConstantPoolAccessInstruction(int pc_offset, int offset,
                                          ConstantPoolEntry::Access access,
                                          ConstantPoolEntry::Type type) {
    // No embedded constant pool support.
    UNREACHABLE();
  }

  // Read/patch instructions
  SixByteInstr instr_at(int pos) {
    return Instruction::InstructionBits(buffer_ + pos);
  }
  template<typename T>
  void instr_at_put(int pos, T instr) {
    Instruction::SetInstructionBits<T>(buffer_ + pos, instr);
  }

  // Decodes instruction at pos, and returns its length
  int32_t instr_length_at(int pos) {
    return Instruction::InstructionLength(buffer_ + pos);
  }

  static SixByteInstr instr_at(byte* pc) {
    return Instruction::InstructionBits(pc);
  }

  static Condition GetCondition(Instr instr);

  static bool IsBranch(Instr instr);
  static Register GetRA(Instr instr);
  static Register GetRB(Instr instr);
#if V8_TARGET_ARCH_S390X
  static bool Is64BitLoadIntoIP(SixByteInstr instr1, SixByteInstr instr2);
#else
  static bool Is32BitLoadIntoIP(SixByteInstr instr);
#endif


  static bool IsCmpRegister(Instr instr);
  static bool IsCmpImmediate(Instr instr);
  static bool IsRlwinm(Instr instr);
  static bool IsNop(SixByteInstr instr, int type = NON_MARKING_NOP);

  // Postpone the generation of the trampoline pool for the specified number of
  // instructions.
  void CheckTrampolinePool();

  // The code currently calls CheckBuffer() too often. This has the side
  // effect of randomly growing the buffer in the middle of multi-instruction
  // sequences.
  //
  // This function allows outside callers to check and grow the buffer
  void EnsureSpaceFor(int space_needed);

  void EmitRelocations();
  void emit_label_addr(Label* label);

 public:
  byte* buffer_pos() const { return buffer_; }

 protected:
  // Relocation for a type-recording IC has the AST id added to it.  This
  // member variable is a way to pass the information from the call site to
  // the relocation info.
  TypeFeedbackId recorded_ast_id_;

  int buffer_space() const { return reloc_info_writer.pos() - pc_; }

  // Decode branch instruction at pos and return branch target pos
  int target_at(int pos);

  // Patch branch instruction at pos to branch to given branch target pos
  void target_at_put(int pos, int target_pos);

  // Record reloc info for current pc_
  void RecordRelocInfo(RelocInfo::Mode rmode, intptr_t data = 0);

  // Block the emission of the trampoline pool before pc_offset.
  void BlockTrampolinePoolBefore(int pc_offset) {
    if (no_trampoline_pool_before_ < pc_offset)
      no_trampoline_pool_before_ = pc_offset;
  }

  void StartBlockTrampolinePool() { trampoline_pool_blocked_nesting_++; }
  void EndBlockTrampolinePool() { trampoline_pool_blocked_nesting_--; }
  bool is_trampoline_pool_blocked() const {
    return trampoline_pool_blocked_nesting_ > 0;
  }

  bool has_exception() const { return internal_trampoline_exception_; }

  bool is_trampoline_emitted() const { return trampoline_emitted_; }

 private:
  // Code generation
  // The relocation writer's position is at least kGap bytes below the end of
  // the generated instructions. This is so that multi-instruction sequences do
  // not have to check for overflow. The same is true for writes of large
  // relocation info entries.
  static const int kGap = 32;

  // Repeated checking whether the trampoline pool should be emitted is rather
  // expensive. By default we only check again once a number of instructions
  // has been generated.
  int next_buffer_check_;  // pc offset of next buffer check.

  // Emission of the trampoline pool may be blocked in some code sequences.
  int trampoline_pool_blocked_nesting_;  // Block emission if this is not zero.
  int no_trampoline_pool_before_;  // Block emission before this pc offset.

  // Relocation info generation
  // Each relocation is encoded as a variable size value
  static const int kMaxRelocSize = RelocInfoWriter::kMaxSize;
  RelocInfoWriter reloc_info_writer;
  std::vector<DeferredRelocInfo> relocations_;

  // The bound position, before this we cannot do instruction elimination.
  int last_bound_pos_;

  // Code emission
  inline void CheckBuffer();
  void GrowBuffer(int needed = 0);

  inline int32_t emit_code_target(Handle<Code> target, RelocInfo::Mode rmode,
                     TypeFeedbackId ast_id = TypeFeedbackId::None());
  // S390 emitting helpers
  inline void emit2bytes(uint16_t x);
  inline void emit4bytes(uint32_t x);
  inline void emit6bytes(uint64_t x);

  inline void i_form(Opcode op, const Operand& i);
  inline void e_form(Opcode op);
  inline void ie_form(Opcode op, const Operand& i1, const Operand& i2);
  inline void rr_form(Opcode op, Register r1, Register r2);
  inline void rr_form(Opcode op, DoubleRegister r1, DoubleRegister r2);
  inline void rr_form(Opcode op, Condition m1, Register r2);

  inline void rr2_form(uint8_t op, Condition m1, Register r2);
  inline void rx_form(Opcode op,
                     Register r1,
                     Register x2,
                     Register b2,
                     Disp d2);
  inline void rx_form(Opcode op, DoubleRegister r1,
                      Register x2, Register b2, Disp d2);


// RI1 format: <insn> R1,I2
//    +--------+----+----+------------------+
//    | OpCode | R1 |OpCd|        I2        |
//    +--------+----+----+------------------+
//    0        8    12   16                31
  inline void ri_form(Opcode op, Register r1, const Operand& i2);
// RI2 format: <insn> M1,I2
//    +--------+----+----+------------------+
//    | OpCode | M1 |OpCd|        I2        |
//    +--------+----+----+------------------+
//    0        8    12   16                31
  inline void ri_form(Opcode op, Condition m1, const Operand& i2);
  inline void rie_form(Opcode op, Register r1, Register r3,
                     const Operand& i2);
  inline void rie_f_form(Opcode op, Register r1, Register r2, const Operand &i3,
                     const Operand& i4, const Operand& i5);
  inline void ril_form(Opcode op, Register r1, const Operand& i2);
  inline void ril_form(Opcode op, Condition m1, const Operand& i2);
  inline void rre_form(Opcode op, Register r1, Register r2);
  inline void rre_form(Opcode op, DoubleRegister r1,
                       DoubleRegister r2);
  inline void rrd_form(Opcode op, Register r1, Register r3,
                       Register r2);
// RS format: <insn> R1,M3,D2(B2)
//     +--------+----+----+----+-------------+
//     | OpCode | R1 | M3 | B2 |     D2      |
//     +--------+----+----+----+-------------+
//     0        8    12   16   20           31
//
  inline void rs_form(Opcode op,
                        Register r1,
                        Condition m3,
                        Register b2,
                        const Disp d2);
// RS format: <insn> R1,R3,D2(B2)
//    +--------+----+----+----+-------------+
//    | OpCode | R1 | R3 | B2 |     D2      |
//    +--------+----+----+----+-------------+
//    0        8    12   16   20           31
//
  inline void rs_form(Opcode op,
                        Register r1,
                        Register r3,
                        Register b2,
                        const Disp d2);

  inline void rsi_form(Opcode op, Register r1, Register r3, const Operand& i2);
  inline void rsl_form(Opcode op, Length l1, Register b2, Disp d2);

// RSY format: <insn> R1,R3,D2(B2)
//     +--------+----+----+----+-------------+--------+--------+
//     | OpCode | R1 | R3 | B2 |    DL2      |  DH2   | OpCode |
//     +--------+----+----+----+-------------+--------+--------+
//     0        8    12   16   20            32       40      47
  inline void rsy_form(Opcode op,
                        Register r1,
                        Register r3,
                        Register b2,
                        const Disp d2);

// RSY format: <insn> R1,M3,D2(B2)
//     +--------+----+----+----+-------------+--------+--------+
//     | OpCode | R1 | M3 | B2 |    DL2      |  DH2   | OpCode |
//     +--------+----+----+----+-------------+--------+--------+
//     0        8    12   16   20            32       40      47
  inline void rsy_form(Opcode op,
                        Register r1,
                        Condition m3,
                        Register b2,
                        const Disp d2);

  inline void rxe_form(Opcode op, Register r1, Register x2, Register b2,
                     Disp d2);
  inline void rxy_form(Opcode op, Register r1, Register x2, Register b2,
                     Disp d2);
  inline void rxy_form(Opcode op, DoubleRegister r1, Register x2,
                       Register b2, Disp d2);
  inline void rrs_form(Opcode op, Register r1, Register r2, Register b4,
                     Disp d4, Condition m3);
  inline void ris_form(Opcode op, Register r1, Condition m3, Register b4, \
                     Disp d4, const Operand& i2);
  inline void s_form(Opcode op, Register b1, Disp d2);
  inline void si_form(Opcode op, const Operand& i2, Register b1,
                     Disp d1);
  inline void siy_form(Opcode op, const Operand& i2, Register b1, \
                     Disp d1);
  inline void sil_form(Opcode op, Register b1, Disp d1,
                     const Operand& i2);
  inline void rxf_form(Opcode op, Register r1, Register r3, Register b2, \
                     Register x2, Disp d2);
  inline void ss_form(Opcode op, Length l, Register b1, Disp d1, \
                     Register b2, Disp d2);
  inline void ss_form(Opcode op, Length l1, Length l2, Register b1,
                     Disp d1, Register b2, Disp d2);
  inline void ss_form(Opcode op, Length l1, const Operand& i3, Register b1,
                     Disp d1, Register b2, Disp d2);
  inline void ss_form(Opcode op, Register r1, Register r2, Register b1,
                     Disp d1, Register b2, Disp d2);
  inline void sse_form(Opcode op, Register b1, Disp d1, Register b2,
                     Disp d2);
  inline void ssf_form(Opcode op, Register r3, Register b1, Disp d1,
                     Register b2, Disp d2);
  inline void rrf1_form(Opcode op, Register r1, Register r2, Register r3);
  inline void rrf1_form(uint32_t x);
  inline void rrf2_form(uint32_t x);
  inline void rrf3_form(uint32_t x);
  inline void rrfe_form(Opcode op, Condition m3, Condition m4, Register r1,
                        Register r2);

  inline void CheckTrampolinePoolQuick();

  // Labels
  void print(Label* L);
  int  max_reach_from(int pos);
  void bind_to(Label* L, int pos);
  void next(Label* L);

  class Trampoline {
   public:
    Trampoline() {
      next_slot_ = 0;
      free_slot_count_ = 0;
    }
    Trampoline(int start, int slot_count) {
      next_slot_ = start;
      free_slot_count_ = slot_count;
    }
    int take_slot() {
      int trampoline_slot = kInvalidSlotPos;
      if (free_slot_count_ <= 0) {
        // We have run out of space on trampolines.
        // Make sure we fail in debug mode, so we become aware of each case
        // when this happens.
        DCHECK(0);
        // Internal exception will be caught.
      } else {
        trampoline_slot = next_slot_;
        free_slot_count_--;
        next_slot_ += kTrampolineSlotsSize;
      }
      return trampoline_slot;
    }

   private:
    int next_slot_;
    int free_slot_count_;
  };

  int32_t get_trampoline_entry();
  int unbound_labels_count_;
  // If trampoline is emitted, generated code is becoming large. As
  // this is already a slow case which can possibly break our code
  // generation for the extreme case, we use this information to
  // trigger different mode of branch instruction generation, where we
  // no longer use a single branch instruction.
  bool trampoline_emitted_;
  static const int kTrampolineSlotsSize = kInstrSize;
  static const int kMaxCondBranchReach = (1 << (16 - 1)) - 1;
  static const int kMaxBlockTrampolineSectionSize = 64 * kInstrSize;
  static const int kInvalidSlotPos = -1;

  Trampoline trampoline_;
  bool internal_trampoline_exception_;

  friend class RegExpMacroAssemblerS390;
  friend class RelocInfo;
  friend class CodePatcher;
  friend class BlockTrampolinePoolScope;

  List< Handle<Code> > code_targets_;

  PositionsRecorder positions_recorder_;
  friend class PositionsRecorder;
  friend class EnsureSpace;
};


class EnsureSpace BASE_EMBEDDED {
 public:
  explicit EnsureSpace(Assembler* assembler) { assembler->CheckBuffer(); }
};
}
}  // namespace v8::internal

#endif  // V8_S390_ASSEMBLER_S390_H_
