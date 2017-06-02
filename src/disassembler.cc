// Copyright 2011 the V8 project authors. All rights reserved.
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.

#include "src/disassembler.h"

#include "src/code-stubs.h"
#include "src/codegen.h"
#include "src/debug/debug.h"
#include "src/deoptimizer.h"
#include "src/disasm.h"
#include "src/macro-assembler.h"
#include "src/snapshot/serializer-common.h"
#include "src/string-stream.h"

namespace v8 {
namespace internal {

#ifdef ENABLE_DISASSEMBLER

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
  const char* name =
      code_ == NULL ? NULL : code_->GetIsolate()->builtins()->Lookup(pc);

  if (name != NULL) {
    SNPrintF(v8_buffer_, "\x25\x73\x20\x20\x28\x25\x70\x29", name, pc);
    return v8_buffer_.start();
  }

  if (code_ != NULL) {
    int offs = static_cast<int>(pc - code_->instruction_start());
    // print as code offset, if it seems reasonable
    if (0 <= offs && offs < code_->instruction_size()) {
      SNPrintF(v8_buffer_, "\x25\x64\x20\x20\x28\x25\x70\x29", offs, pc);
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


static void DumpBuffer(v8::base::OStream* os, StringBuilder* out) {
  (*os) << out->Finalize() << std::endl;
  out->Reset();
}


static const int kOutBufferSize = 2048 + String::kMaxShortPrintLength;
static const int kRelocInfoPosition = 57;

static int DecodeIt(Isolate* isolate, v8::base::OStream* os,
                    const V8NameConverter& converter, byte* begin, byte* end) {
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
  int constants = -1;  // no constants being decoded at the start

  while (pc < end) {
    // First decode instruction so that we know its length.
    byte* prev_pc = pc;
    if (constants > 0) {
      SNPrintF(decode_buffer,
               "\x25\x30\x38\x78\x20\x20\x20\x20\x20\x20\x20\x63\x6f\x6e\x73\x74\x61\x6e\x74",
               *reinterpret_cast<int32_t*>(pc));
      constants--;
      pc += 4;
    } else {
      int num_const = d.ConstantPoolSizeAt(pc);
      if (num_const >= 0) {
        SNPrintF(decode_buffer,
                 "\x25\x30\x38\x78\x20\x20\x20\x20\x20\x20\x20\x63\x6f\x6e\x73\x74\x61\x6e\x74\x20\x70\x6f\x6f\x6c\x20\x62\x65\x67\x69\x6e\x20\x28\x6e\x75\x6d\x5f\x63\x6f\x6e\x73\x74\x20\x3d\x20\x25\x64\x29",
                 *reinterpret_cast<int32_t*>(pc), num_const);
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
        pc += sizeof(ptr);
#endif
      } else {
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
      out.AddFormatted("\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x25\x73", comments[i]);
      DumpBuffer(os, &out);
    }

    // Instruction address and instruction offset.
    out.AddFormatted("\x25\x70\x20\x20\x25\x34\x64\x20\x20", prev_pc, prev_pc - begin);

    // Instruction.
    out.AddFormatted("\x25\x73", decode_buffer.start());

    // Print all the reloc info for this instruction which are not comments.
    for (int i = 0; i < pcs.length(); i++) {
      // Put together the reloc info
      RelocInfo relocinfo(isolate, pcs[i], rmodes[i], datas[i],
                          converter.code());

      // Indent the printing of the reloc info.
      if (i == 0) {
        // The first reloc info is printed after the disassembled instruction.
        out.AddPadding('\x20', kRelocInfoPosition - out.position());
      } else {
        // Additional reloc infos are printed on separate lines.
        DumpBuffer(os, &out);
        out.AddPadding('\x20', kRelocInfoPosition);
      }

      RelocInfo::Mode rmode = relocinfo.rmode();
      if (RelocInfo::IsPosition(rmode)) {
        if (RelocInfo::IsStatementPosition(rmode)) {
          out.AddFormatted("\x20\x20\x20\x20\x3b\x3b\x20\x64\x65\x62\x75\x67\x3a\x20\x73\x74\x61\x74\x65\x6d\x65\x6e\x74\x20\x25\x64", relocinfo.data());
        } else {
          out.AddFormatted("\x20\x20\x20\x20\x3b\x3b\x20\x64\x65\x62\x75\x67\x3a\x20\x70\x6f\x73\x69\x74\x69\x6f\x6e\x20\x25\x64", relocinfo.data());
        }
      } else if (rmode == RelocInfo::DEOPT_REASON) {
        Deoptimizer::DeoptReason reason =
            static_cast<Deoptimizer::DeoptReason>(relocinfo.data());
        out.AddFormatted("\x20\x20\x20\x20\x3b\x3b\x20\x64\x65\x62\x75\x67\x3a\x20\x64\x65\x6f\x70\x74\x20\x72\x65\x61\x73\x6f\x6e\x20\x27\x25\x73\x27",
                         Deoptimizer::GetDeoptReason(reason));
      } else if (rmode == RelocInfo::EMBEDDED_OBJECT) {
        HeapStringAllocator allocator;
        StringStream accumulator(&allocator);
        relocinfo.target_object()->ShortPrint(&accumulator);
        base::SmartArrayPointer<const char> obj_name = accumulator.ToCString();
        out.AddFormatted("\x20\x20\x20\x20\x3b\x3b\x20\x6f\x62\x6a\x65\x63\x74\x3a\x20\x25\x73", obj_name.get());
      } else if (rmode == RelocInfo::EXTERNAL_REFERENCE) {
#ifndef V8_OS_ZOS
        const char* reference_name = ref_encoder.NameOfAddress(
            isolate, relocinfo.target_external_reference());
        out.AddFormatted("\x20\x20\x20\x20\x3b\x3b\x20\x65\x78\x74\x65\x72\x6e\x61\x6c\x20\x72\x65\x66\x65\x72\x65\x6e\x63\x65\x20\x28\x25\x73\x29", reference_name);
#endif
      } else if (RelocInfo::IsCodeTarget(rmode)) {
        out.AddFormatted("\x20\x20\x20\x20\x3b\x3b\x20\x63\x6f\x64\x65\x3a");
        Code* code = Code::GetCodeFromTargetAddress(relocinfo.target_address());
        Code::Kind kind = code->kind();
        if (code->is_inline_cache_stub()) {
          if (kind == Code::LOAD_IC &&
              LoadICState::GetTypeofMode(code->extra_ic_state()) ==
                  NOT_INSIDE_TYPEOF) {
            out.AddFormatted("\x20\x63\x6f\x6e\x74\x65\x78\x74\x75\x61\x6c\x2c");
          }
          InlineCacheState ic_state = code->ic_state();
          out.AddFormatted("\x20\x25\x73\x2c\x20\x25\x73", Code::Kind2String(kind),
              Code::ICState2String(ic_state));
          if (ic_state == MONOMORPHIC) {
            Code::StubType type = code->type();
            out.AddFormatted("\x2c\x20\x25\x73", Code::StubType2String(type));
          }
        } else if (kind == Code::STUB || kind == Code::HANDLER) {
          // Get the STUB key and extract major and minor key.
          uint32_t key = code->stub_key();
          uint32_t minor_key = CodeStub::MinorKeyFromKey(key);
          CodeStub::Major major_key = CodeStub::GetMajorKey(code);
          DCHECK(major_key == CodeStub::MajorKeyFromKey(key));
          const char * name = CodeStub::MajorName(major_key);
          out.AddFormatted("\x20\x25\x73\x2c\x20\x25\x73\x2c\x20", Code::Kind2String(kind), name);
          out.AddFormatted("\x6d\x69\x6e\x6f\x72\x3a\x20\x25\x64", minor_key);
        } else {
          out.AddFormatted("\x20\x25\x73", Code::Kind2String(kind));
        }
        if (rmode == RelocInfo::CODE_TARGET_WITH_ID) {
          out.AddFormatted("\x20\x28\x69\x64\x20\x3d\x20\x25\x64\x29", static_cast<int>(relocinfo.data()));
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
              out.AddFormatted("\x20\x20\x20\x20\x3b\x3b\x20\x25\x73", RelocInfo::RelocModeName(rmode));
            } else {
              out.AddFormatted("\x20\x20\x20\x20\x3b\x3b\x20\x73\x6f\x66\x74\x20\x64\x65\x6f\x70\x74\x69\x6d\x69\x7a\x61\x74\x69\x6f\x6e\x20\x62\x61\x69\x6c\x6f\x75\x74\x20\x25\x64", id);
            }
          } else {
            out.AddFormatted("\x20\x20\x20\x20\x3b\x3b\x20\x6c\x61\x7a\x79\x20\x64\x65\x6f\x70\x74\x69\x6d\x69\x7a\x61\x74\x69\x6f\x6e\x20\x62\x61\x69\x6c\x6f\x75\x74\x20\x25\x64", id);
          }
        } else {
          out.AddFormatted("\x20\x20\x20\x20\x3b\x3b\x20\x64\x65\x6f\x70\x74\x69\x6d\x69\x7a\x61\x74\x69\x6f\x6e\x20\x62\x61\x69\x6c\x6f\x75\x74\x20\x25\x64", id);
        }
      } else {
        out.AddFormatted("\x20\x20\x20\x20\x3b\x3b\x20\x25\x73", RelocInfo::RelocModeName(rmode));
      }
    }
    DumpBuffer(os, &out);
  }

  // Emit comments following the last instruction (if any).
  if (it != NULL) {
    for ( ; !it->done(); it->next()) {
      if (RelocInfo::IsComment(it->rinfo()->rmode())) {
        out.AddFormatted("\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x25\x73",
                         reinterpret_cast<const char*>(it->rinfo()->data()));
        DumpBuffer(os, &out);
      }
    }
  }

  delete it;
  return static_cast<int>(pc - begin);
}


int Disassembler::Decode(Isolate* isolate, v8::base::OStream* os, byte* begin,
                         byte* end, Code* code) {
  V8NameConverter v8NameConverter(code);
  return DecodeIt(isolate, os, v8NameConverter, begin, end);
}

#else  // ENABLE_DISASSEMBLER

int Disassembler::Decode(Isolate* isolate, v8::base::OStream* os, byte* begin,
                         byte* end, Code* code) {
  return 0;
}

#endif  // ENABLE_DISASSEMBLER

}  // namespace internal
}  // namespace v8
