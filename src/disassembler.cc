// Copyright 2011 the V8 project authors. All rights reserved.
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.

#include "src/v8.h"

#include "src/code-stubs.h"
#include "src/codegen.h"
#include "src/debug.h"
#include "src/deoptimizer.h"
#include "src/disasm.h"
#include "src/disassembler.h"
#include "src/macro-assembler.h"
#include "src/serialize.h"
#include "src/string-stream.h"

namespace v8 {
namespace internal {

#ifdef ENABLE_DISASSEMBLER

void Disassembler::Dump(FILE* f, byte* begin, byte* end) {
  for (byte* pc = begin; pc < end; pc++) {
    if (f == NULL) {
      PrintF("\x25" V8PRIxPTR "\x20\x20\x25\x34" V8PRIdPTR "\x20\x20\x6c\xf0\xf2\xa7\xa",
             reinterpret_cast<intptr_t>(pc),
             pc - begin,
             *pc);
    } else {
      PrintF(f, "\x25" V8PRIxPTR "\x20\x20\x25\x34" V8PRIdPTR "\x20\x20\x6c\xf0\xf2\xa7\xa",
             reinterpret_cast<uintptr_t>(pc), pc - begin, *pc);
    }
  }
}


class V8NameConverter: public disasm::NameConverter {
 public:
  explicit V8NameConverter(Code* code) : code_(code) {}
  virtual const char* NameOfAddress(byte* pc) const;
  virtual const char* NameInCode(byte* addr) const;
  Code* code() const { return code_; }
 private:
  Code* code_;

  EmbeddedVector<char, 128> v8_buffer_;
};


const char* V8NameConverter::NameOfAddress(byte* pc) const {
  const char* name = code_->GetIsolate()->builtins()->Lookup(pc);
  if (name != NULL) {
    SNPrintF(v8_buffer_, "\x6c\xa2\x20\x20\x28\x6c\x97\x29", name, pc);
    return v8_buffer_.start();
  }

  if (code_ != NULL) {
    int offs = static_cast<int>(pc - code_->instruction_start());
    // print as code offset, if it seems reasonable
    if (0 <= offs && offs < code_->instruction_size()) {
      SNPrintF(v8_buffer_, "\x6c\x84\x20\x20\x28\x6c\x97\x29", offs, pc);
      return v8_buffer_.start();
    }
  }

  return disasm::NameConverter::NameOfAddress(pc);
}


const char* V8NameConverter::NameInCode(byte* addr) const {
  // The V8NameConverter is used for well known code, so we can "safely"
  // dereference pointers in generated code.
  return (code_ != NULL) ? reinterpret_cast<const char*>(addr) : "";
}


static void DumpBuffer(FILE* f, StringBuilder* out) {
  if (f == NULL) {
    PrintF("\x6c\xa2\xa", out->Finalize());
  } else {
    PrintF(f, "\x6c\xa2\xa", out->Finalize());
  }
  out->Reset();
}


static const int kOutBufferSize = 2048 + String::kMaxShortPrintLength;
static const int kRelocInfoPosition = 57;

static int DecodeIt(Isolate* isolate,
                    FILE* f,
                    const V8NameConverter& converter,
                    byte* begin,
                    byte* end) {
  SealHandleScope shs(isolate);
  DisallowHeapAllocation no_alloc;
  ExternalReferenceEncoder ref_encoder(isolate);

  v8::internal::EmbeddedVector<char, 128> decode_buffer;
  v8::internal::EmbeddedVector<char, kOutBufferSize> out_buffer;
  StringBuilder out(out_buffer.start(), out_buffer.length());
  byte* pc = begin;
  disasm::Disassembler d(converter);
  RelocIterator* it = NULL;
  if (converter.code() != NULL) {
    it = new RelocIterator(converter.code());
  } else {
    // No relocation information when printing code stubs.
  }
#if !V8_TARGET_ARCH_PPC
  int constants = -1;  // no constants being decoded at the start
#endif

  while (pc < end) {
    // First decode instruction so that we know its length.
    byte* prev_pc = pc;
#if !V8_TARGET_ARCH_PPC
    if (constants > 0) {
      SNPrintF(decode_buffer,
               "\x6c\xf0\xf8\xa7\x20\x20\x20\x20\x20\x20\x20\x63\x6f\x6e\x73\x74\x61\x6e\x74",
               *reinterpret_cast<int32_t*>(pc));
      constants--;
      pc += 4;
    } else {
      int num_const = d.ConstantPoolSizeAt(pc);
      if (num_const >= 0) {
        SNPrintF(decode_buffer,
                 "\x6c\xf0\xf8\xa7\x20\x20\x20\x20\x20\x20\x20\x63\x6f\x6e\x73\x74\x61\x6e\x74\x20\x70\x6f\x6f\x6c\x20\x62\x65\x67\x69\x6e",
                 *reinterpret_cast<int32_t*>(pc));
        constants = num_const;
        pc += 4;
      } else if (it != NULL && !it->done() && it->rinfo()->pc() == pc &&
          it->rinfo()->rmode() == RelocInfo::INTERNAL_REFERENCE) {
#if ABI_USES_FUNCTION_DESCRIPTORS
        // z/OS.
        pc += Assembler::DecodeInternalReference(decode_buffer, pc);
#else
        // raw pointer embedded in code stream, e.g., jump table
        byte* ptr = *reinterpret_cast<byte**>(pc);
        SNPrintF(decode_buffer,
                 "\x25\x30\x38" V8PRIxPTR "\x20\x20\x20\x20\x20\x20\x6a\x75\x6d\x70\x20\x74\x61\x62\x6c\x65\x20\x65\x6e\x74\x72\x79\x20\x25\x34" V8PRIdPTR,
                 reinterpret_cast<intptr_t>(ptr),
                 ptr - begin);
        pc += 4;
#endif
      } else {
#elif ABI_USES_FUNCTION_DESCRIPTORS || V8_OOL_CONSTANT_POOL
    // V8_TARGET_ARCH_PPC
    {
      // Function descriptors are specially decoded and skipped.
      // Other internal references (load of ool constant pool pointer)
      // are not since they are a encoded as a regular mov sequence.
      int skip;
      if (it != NULL && !it->done() && it->rinfo()->pc() == pc &&
          it->rinfo()->rmode() == RelocInfo::INTERNAL_REFERENCE &&
          (skip = Assembler::DecodeInternalReference(decode_buffer, pc))) {
        pc += skip;
      } else {
#else
    {
      {
#endif
        decode_buffer[0] = '\x0';
        pc += d.InstructionDecode(decode_buffer, pc);
      }
    }

    // Collect RelocInfo for this instruction (prev_pc .. pc-1)
    List<const char*> comments(4);
    List<byte*> pcs(1);
    List<RelocInfo::Mode> rmodes(1);
    List<intptr_t> datas(1);
    if (it != NULL) {
      while (!it->done() && it->rinfo()->pc() < pc) {
        if (RelocInfo::IsComment(it->rinfo()->rmode())) {
          // For comments just collect the text.
          comments.Add(reinterpret_cast<const char*>(it->rinfo()->data()));
        } else {
          // For other reloc info collect all data.
          pcs.Add(it->rinfo()->pc());
          rmodes.Add(it->rinfo()->rmode());
          datas.Add(it->rinfo()->data());
        }
        it->next();
      }
    }

    // Comments.
    for (int i = 0; i < comments.length(); i++) {
      out.AddFormatted("\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x6c\xa2", comments[i]);
      DumpBuffer(f, &out);
    }

    // Instruction address and instruction offset.
    out.AddFormatted("\x6c\x97\x20\x20\x6c\xf4\x84\x20\x20", prev_pc, prev_pc - begin);

    // Instruction.
    out.AddFormatted("\x6c\xa2", decode_buffer.start());

    // Print all the reloc info for this instruction which are not comments.
    for (int i = 0; i < pcs.length(); i++) {
      // Put together the reloc info
      RelocInfo relocinfo(pcs[i], rmodes[i], datas[i], converter.code());

      // Indent the printing of the reloc info.
      if (i == 0) {
        // The first reloc info is printed after the disassembled instruction.
        out.AddPadding('\x20', kRelocInfoPosition - out.position());
      } else {
        // Additional reloc infos are printed on separate lines.
        DumpBuffer(f, &out);
        out.AddPadding('\x20', kRelocInfoPosition);
      }

      RelocInfo::Mode rmode = relocinfo.rmode();
      if (RelocInfo::IsPosition(rmode)) {
        if (RelocInfo::IsStatementPosition(rmode)) {
          out.AddFormatted("\x20\x20\x20\x20\x3b\x3b\x20\x64\x65\x62\x75\x67\x3a\x20\x73\x74\x61\x74\x65\x6d\x65\x6e\x74\x20\x6c\x84", relocinfo.data());
        } else {
          out.AddFormatted("\x20\x20\x20\x20\x3b\x3b\x20\x64\x65\x62\x75\x67\x3a\x20\x70\x6f\x73\x69\x74\x69\x6f\x6e\x20\x6c\x84", relocinfo.data());
        }
      } else if (rmode == RelocInfo::EMBEDDED_OBJECT) {
        HeapStringAllocator allocator;
        StringStream accumulator(&allocator);
        relocinfo.target_object()->ShortPrint(&accumulator);
        SmartArrayPointer<const char> obj_name = accumulator.ToCString();
        out.AddFormatted("\x20\x20\x20\x20\x3b\x3b\x20\x6f\x62\x6a\x65\x63\x74\x3a\x20\x6c\xa2", obj_name.get());
      } else if (rmode == RelocInfo::EXTERNAL_REFERENCE) {
        const char* reference_name =
            ref_encoder.NameOfAddress(relocinfo.target_reference());
        out.AddFormatted("\x20\x20\x20\x20\x3b\x3b\x20\x65\x78\x74\x65\x72\x6e\x61\x6c\x20\x72\x65\x66\x65\x72\x65\x6e\x63\x65\x20\x28\x6c\xa2\x29", reference_name);
      } else if (RelocInfo::IsCodeTarget(rmode)) {
        out.AddFormatted("\x20\x20\x20\x20\x3b\x3b\x20\x63\x6f\x64\x65\x3a");
        if (rmode == RelocInfo::CONSTRUCT_CALL) {
          out.AddFormatted("\x20\x63\x6f\x6e\x73\x74\x72\x75\x63\x74\x6f\x72\x2c");
        }
        Code* code = Code::GetCodeFromTargetAddress(relocinfo.target_address());
        Code::Kind kind = code->kind();
        if (code->is_inline_cache_stub()) {
          if (kind == Code::LOAD_IC &&
              LoadIC::GetContextualMode(code->extra_ic_state()) == CONTEXTUAL) {
            out.AddFormatted("\x20\x63\x6f\x6e\x74\x65\x78\x74\x75\x61\x6c\x2c");
          }
          InlineCacheState ic_state = code->ic_state();
          out.AddFormatted("\x20\x6c\xa2\x2c\x20\x6c\xa2", Code::Kind2String(kind),
              Code::ICState2String(ic_state));
          if (ic_state == MONOMORPHIC) {
            Code::StubType type = code->type();
            out.AddFormatted("\x2c\x20\x6c\xa2", Code::StubType2String(type));
          }
        } else if (kind == Code::STUB || kind == Code::HANDLER) {
          // Get the STUB key and extract major and minor key.
          uint32_t key = code->stub_key();
          uint32_t minor_key = CodeStub::MinorKeyFromKey(key);
          CodeStub::Major major_key = CodeStub::GetMajorKey(code);
          DCHECK(major_key == CodeStub::MajorKeyFromKey(key));
          out.AddFormatted("\x20\x6c\xa2\x2c\x20\x6c\xa2\x2c\x20", Code::Kind2String(kind),
                           CodeStub::MajorName(major_key, false));
          switch (major_key) {
            case CodeStub::CallFunction: {
              int argc = CallFunctionStub::ExtractArgcFromMinorKey(minor_key);
              out.AddFormatted("\x61\x72\x67\x63\x20\x3d\x20\x6c\x84", argc);
              break;
            }
            default:
              out.AddFormatted("\x6d\x69\x6e\x6f\x72\x3a\x20\x6c\x84", minor_key);
          }
        } else {
          out.AddFormatted("\x20\x6c\xa2", Code::Kind2String(kind));
        }
        if (rmode == RelocInfo::CODE_TARGET_WITH_ID) {
          out.AddFormatted("\x20\x28\x69\x64\x20\x3d\x20\x6c\x84\x29", static_cast<int>(relocinfo.data()));
        }
      } else if (RelocInfo::IsRuntimeEntry(rmode) &&
                 isolate->deoptimizer_data() != NULL) {
        // A runtime entry reloinfo might be a deoptimization bailout.
        Address addr = relocinfo.target_address();
        int id = Deoptimizer::GetDeoptimizationId(isolate,
                                                  addr,
                                                  Deoptimizer::EAGER);
        if (id == Deoptimizer::kNotDeoptimizationEntry) {
          id = Deoptimizer::GetDeoptimizationId(isolate,
                                                addr,
                                                Deoptimizer::LAZY);
          if (id == Deoptimizer::kNotDeoptimizationEntry) {
            id = Deoptimizer::GetDeoptimizationId(isolate,
                                                  addr,
                                                  Deoptimizer::SOFT);
            if (id == Deoptimizer::kNotDeoptimizationEntry) {
              out.AddFormatted("\x20\x20\x20\x20\x3b\x3b\x20\x6c\xa2", RelocInfo::RelocModeName(rmode));
            } else {
              out.AddFormatted("\x20\x20\x20\x20\x3b\x3b\x20\x73\x6f\x66\x74\x20\x64\x65\x6f\x70\x74\x69\x6d\x69\x7a\x61\x74\x69\x6f\x6e\x20\x62\x61\x69\x6c\x6f\x75\x74\x20\x6c\x84", id);
            }
          } else {
            out.AddFormatted("\x20\x20\x20\x20\x3b\x3b\x20\x6c\x61\x7a\x79\x20\x64\x65\x6f\x70\x74\x69\x6d\x69\x7a\x61\x74\x69\x6f\x6e\x20\x62\x61\x69\x6c\x6f\x75\x74\x20\x6c\x84", id);
          }
        } else {
          out.AddFormatted("\x20\x20\x20\x20\x3b\x3b\x20\x64\x65\x6f\x70\x74\x69\x6d\x69\x7a\x61\x74\x69\x6f\x6e\x20\x62\x61\x69\x6c\x6f\x75\x74\x20\x6c\x84", id);
        }
      } else {
        out.AddFormatted("\x20\x20\x20\x20\x3b\x3b\x20\x6c\xa2", RelocInfo::RelocModeName(rmode));
      }
    }
    DumpBuffer(f, &out);
  }

  // Emit comments following the last instruction (if any).
  if (it != NULL) {
    for ( ; !it->done(); it->next()) {
      if (RelocInfo::IsComment(it->rinfo()->rmode())) {
        out.AddFormatted("\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x6c\xa2",
                         reinterpret_cast<const char*>(it->rinfo()->data()));
        DumpBuffer(f, &out);
      }
    }
  }

  delete it;
  return static_cast<int>(pc - begin);
}


int Disassembler::Decode(Isolate* isolate, FILE* f, byte* begin, byte* end) {
  V8NameConverter defaultConverter(NULL);
  return DecodeIt(isolate, f, defaultConverter, begin, end);
}


// Called by Code::CodePrint.
void Disassembler::Decode(FILE* f, Code* code) {
  Isolate* isolate = code->GetIsolate();
  int size = code->instruction_size();
  int safepoint_offset = code->is_crankshafted()
      ? static_cast<int>(code->safepoint_table_offset()) : size;
  int back_edge_offset = (code->kind() == Code::FUNCTION)
      ? static_cast<int>(code->back_edge_table_offset()) : size;
  int constant_offset = FLAG_enable_ool_constant_pool_in_code
      ? code->constant_pool_offset() : size;

  // Stop before reaching any embedded tables
  int code_size = Min(safepoint_offset, back_edge_offset);
  byte* begin = code->instruction_start();
  byte* end = begin + Min(code_size, constant_offset);
  V8NameConverter v8NameConverter(code);
  DecodeIt(isolate, f, v8NameConverter, begin, end);

  if (constant_offset < code_size) {
    v8::internal::EmbeddedVector<char, kOutBufferSize> out_buffer;
    StringBuilder out(out_buffer.start(), out_buffer.length());
    int constant_size = code_size - constant_offset;
    DCHECK((constant_size & kPointerAlignmentMask) == 0);
    out.AddFormatted("\xaC\x6f\x6e\x73\x74\x61\x6e\x74\x20\x50\x6f\x6f\x6c\x20\x28\x73\x69\x7a\x65\x20\x3d\x20\x6c\x84\x29", constant_size);
    DumpBuffer(f, &out);
    intptr_t* ptr = reinterpret_cast<intptr_t*>(begin + constant_offset);
    for (int i = 0; i < constant_size; i += kPointerSize, ptr++) {
      out.AddFormatted("\x25\x30\x38" V8PRIxPTR "\x20\x20\x6c\xf4\x84\x20\x25\x30\x38" V8PRIxPTR, ptr, i, *ptr);
      DumpBuffer(f, &out);
    }
  }
}

#else  // ENABLE_DISASSEMBLER

void Disassembler::Dump(FILE* f, byte* begin, byte* end) {}
int Disassembler::Decode(Isolate* isolate, FILE* f, byte* begin, byte* end) {
  return 0;
}


void Disassembler::Decode(FILE* f, Code* code) {}

#endif  // ENABLE_DISASSEMBLER

} }  // namespace v8::internal
