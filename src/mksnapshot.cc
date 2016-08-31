// Copyright 2006-2008 the V8 project authors. All rights reserved.
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.

#include <errno.h>
#include <stdio.h>
#ifdef COMPRESS_STARTUP_DATA_BZ2
#include <bzlib.h>
#endif
#include <signal.h>

#include "src/v8.h"

#include "include/libplatform/libplatform.h"
#include "src/assembler.h"
#include "src/base/platform/platform.h"
#include "src/bootstrapper.h"
#include "src/flags.h"
#include "src/list.h"
#include "src/natives.h"
#include "src/serialize.h"


using namespace v8;


class Compressor {
 public:
  virtual ~Compressor() {}
  virtual bool Compress(i::Vector<i::byte> input) = 0;
  virtual i::Vector<i::byte>* output() = 0;
};


class SnapshotWriter {
 public:
  explicit SnapshotWriter(const char* snapshot_file)
      : fp_(GetFileDescriptorOrDie(snapshot_file))
      , raw_file_(NULL)
      , raw_context_file_(NULL)
      , startup_blob_file_(NULL)
      , compressor_(NULL) {
  }

  ~SnapshotWriter() {
    fclose(fp_);
    if (raw_file_) fclose(raw_file_);
    if (raw_context_file_) fclose(raw_context_file_);
    if (startup_blob_file_) fclose(startup_blob_file_);
  }

  void SetCompressor(Compressor* compressor) {
    compressor_ = compressor;
  }

  void SetRawFiles(const char* raw_file, const char* raw_context_file) {
    raw_file_ = GetFileDescriptorOrDie(raw_file);
    raw_context_file_ = GetFileDescriptorOrDie(raw_context_file);
  }

  void SetStartupBlobFile(const char* startup_blob_file) {
    if (startup_blob_file != NULL)
      startup_blob_file_ = GetFileDescriptorOrDie(startup_blob_file);
  }

  void WriteSnapshot(const i::List<i::byte>& snapshot_data,
                     const i::Serializer& serializer,
                     const i::List<i::byte>& context_snapshot_data,
                     const i::Serializer& context_serializer) const {
    WriteSnapshotFile(snapshot_data, serializer,
                      context_snapshot_data, context_serializer);
    MaybeWriteStartupBlob(snapshot_data, serializer,
                          context_snapshot_data, context_serializer);
  }

 private:
  void MaybeWriteStartupBlob(const i::List<i::byte>& snapshot_data,
                             const i::Serializer& serializer,
                             const i::List<i::byte>& context_snapshot_data,
                             const i::Serializer& context_serializer) const {
    if (!startup_blob_file_)
      return;

    i::List<i::byte> startup_blob;
    i::ListSnapshotSink sink(&startup_blob);

    int spaces[] = {
        i::NEW_SPACE, i::OLD_POINTER_SPACE, i::OLD_DATA_SPACE, i::CODE_SPACE,
        i::MAP_SPACE, i::CELL_SPACE,  i::PROPERTY_CELL_SPACE
    };

    i::byte* snapshot_bytes = snapshot_data.begin();
    sink.PutBlob(snapshot_bytes, snapshot_data.length(), "\x73\x6e\x61\x70\x73\x68\x6f\x74");
    for (size_t i = 0; i < ARRAY_SIZE(spaces); ++i)
      sink.PutInt(serializer.CurrentAllocationAddress(spaces[i]), "\x73\x70\x61\x63\x65\x73");

    i::byte* context_bytes = context_snapshot_data.begin();
    sink.PutBlob(context_bytes, context_snapshot_data.length(), "\x63\x6f\x6e\x74\x65\x78\x74");
    for (size_t i = 0; i < ARRAY_SIZE(spaces); ++i)
      sink.PutInt(context_serializer.CurrentAllocationAddress(spaces[i]),
                  "\x73\x70\x61\x63\x65\x73");

    size_t written = fwrite(startup_blob.begin(), 1, startup_blob.length(),
                            startup_blob_file_);
    if (written != (size_t)startup_blob.length()) {
      i::PrintF("\x57\x72\x69\x74\x69\x6e\x67\x20\x73\x6e\x61\x70\x73\x68\x6f\x74\x20\x66\x69\x6c\x65\x20\x66\x61\x69\x6c\x65\x64\x2e\x2e\x20\x41\x62\x6f\x72\x74\x69\x6e\x67\x2e\xa");
      exit(1);
    }
  }

  void WriteSnapshotFile(const i::List<i::byte>& snapshot_data,
                         const i::Serializer& serializer,
                         const i::List<i::byte>& context_snapshot_data,
                         const i::Serializer& context_serializer) const {
    WriteFilePrefix();
    WriteData("", snapshot_data, raw_file_);
    WriteData("\x63\x6f\x6e\x74\x65\x78\x74\x5f", context_snapshot_data, raw_context_file_);
    WriteMeta("\x63\x6f\x6e\x74\x65\x78\x74\x5f", context_serializer);
    WriteMeta("", serializer);
    WriteFileSuffix();
  }

  void WriteFilePrefix() const {
    fprintf(fp_, "\x2f\x2f\x20\x41\x75\x74\x6f\x67\x65\x6e\x65\x72\x61\x74\x65\x64\x20\x73\x6e\x61\x70\x73\x68\x6f\x74\x20\x66\x69\x6c\x65\x2e\x20\x44\x6f\x20\x6e\x6f\x74\x20\x65\x64\x69\x74\x2e\xa\xa");
    fprintf(fp_, "\x23\x69\x6e\x63\x6c\x75\x64\x65\x20\x22\x73\x72\x63\x2f\x76\x38\x2e\x68\x22\xa");
    fprintf(fp_, "\x23\x69\x6e\x63\x6c\x75\x64\x65\x20\x22\x73\x72\x63\x2f\x62\x61\x73\x65\x2f\x70\x6c\x61\x74\x66\x6f\x72\x6d\x2f\x70\x6c\x61\x74\x66\x6f\x72\x6d\x2e\x68\x22\xa\xa");
    fprintf(fp_, "\x23\x69\x6e\x63\x6c\x75\x64\x65\x20\x22\x73\x72\x63\x2f\x73\x6e\x61\x70\x73\x68\x6f\x74\x2e\x68\x22\xa\xa");
    fprintf(fp_, "\x6e\x61\x6d\x65\x73\x70\x61\x63\x65\x20\x76\x38\x20\x7b\xa");
    fprintf(fp_, "\x6e\x61\x6d\x65\x73\x70\x61\x63\x65\x20\x69\x6e\x74\x65\x72\x6e\x61\x6c\x20\x7b\xa\xa");
  }

  void WriteFileSuffix() const {
    fprintf(fp_, "\x7d\x20\x20\x2f\x2f\x20\x6e\x61\x6d\x65\x73\x70\x61\x63\x65\x20\x69\x6e\x74\x65\x72\x6e\x61\x6c\xa");
    fprintf(fp_, "\x7d\x20\x20\x2f\x2f\x20\x6e\x61\x6d\x65\x73\x70\x61\x63\x65\x20\x76\x38\xa");
  }

  void WriteData(const char* prefix, const i::List<i::byte>& source_data,
                 FILE* raw_file) const {
    const i::List<i::byte>* data_to_be_written = NULL;
    i::List<i::byte> compressed_data;
    if (!compressor_) {
      data_to_be_written = &source_data;
    } else if (compressor_->Compress(source_data.ToVector())) {
      compressed_data.AddAll(*compressor_->output());
      data_to_be_written = &compressed_data;
    } else {
      i::PrintF("\x43\x6f\x6d\x70\x72\x65\x73\x73\x69\x6f\x6e\x20\x66\x61\x69\x6c\x65\x64\x2e\x20\x41\x62\x6f\x72\x74\x69\x6e\x67\x2e\xa");
      exit(1);
    }

    DCHECK(data_to_be_written);
    MaybeWriteRawFile(data_to_be_written, raw_file);
    WriteData(prefix, source_data, data_to_be_written);
  }

  void MaybeWriteRawFile(const i::List<i::byte>* data, FILE* raw_file) const {
    if (!data || !raw_file)
      return;

    // Sanity check, whether i::List iterators truly return pointers to an
    // internal array.
    DCHECK(data->end() - data->begin() == data->length());

    size_t written = fwrite(data->begin(), 1, data->length(), raw_file);
    if (written != (size_t)data->length()) {
      i::PrintF("\x57\x72\x69\x74\x69\x6e\x67\x20\x72\x61\x77\x20\x66\x69\x6c\x65\x20\x66\x61\x69\x6c\x65\x64\x2e\x2e\x20\x41\x62\x6f\x72\x74\x69\x6e\x67\x2e\xa");
      exit(1);
    }
  }

  void WriteData(const char* prefix, const i::List<i::byte>& source_data,
                 const i::List<i::byte>* data_to_be_written) const {
    fprintf(fp_, "\x63\x6f\x6e\x73\x74\x20\x62\x79\x74\x65\x20\x53\x6e\x61\x70\x73\x68\x6f\x74\x3a\x3a\x6c\xa2\x84\x81\x74\x61\x5f\x5b\x5d\x20\x3d\x20\x7b\xa", prefix);
    WriteSnapshotData(data_to_be_written);
    fprintf(fp_, "\x7d\x3b\xa");
    fprintf(fp_, "\x63\x6f\x6e\x73\x74\x20\x69\x6e\x74\x20\x53\x6e\x61\x70\x73\x68\x6f\x74\x3a\x3a\x6c\xa2\xa2\x89\x7a\x65\x5f\x20\x3d\x20\x6c\x84\x3b\xa", prefix,
            data_to_be_written->length());

    if (data_to_be_written == &source_data) {
      fprintf(fp_, "\x63\x6f\x6e\x73\x74\x20\x62\x79\x74\x65\x2a\x20\x53\x6e\x61\x70\x73\x68\x6f\x74\x3a\x3a\x6c\xa2\x72\x61\x77\x5f\x64\x61\x74\x61\x5f\x20\x3d\x20\x53\x6e\x61\x70\x73\x68\x6f\x74\x3a\x3a\x6c\xa2\x84\x81\x74\x61\x5f\x3b\xa",
              prefix, prefix);
      fprintf(fp_, "\x63\x6f\x6e\x73\x74\x20\x69\x6e\x74\x20\x53\x6e\x61\x70\x73\x68\x6f\x74\x3a\x3a\x6c\xa2\x72\x61\x77\x5f\x73\x69\x7a\x65\x5f\x20\x3d\x20\x53\x6e\x61\x70\x73\x68\x6f\x74\x3a\x3a\x6c\xa2\xa2\x89\x7a\x65\x5f\x3b\xa",
              prefix, prefix);
    } else {
      fprintf(fp_, "\x63\x6f\x6e\x73\x74\x20\x62\x79\x74\x65\x2a\x20\x53\x6e\x61\x70\x73\x68\x6f\x74\x3a\x3a\x6c\xa2\x72\x61\x77\x5f\x64\x61\x74\x61\x5f\x20\x3d\x20\x4e\x55\x4c\x4c\x3b\xa", prefix);
      fprintf(fp_, "\x63\x6f\x6e\x73\x74\x20\x69\x6e\x74\x20\x53\x6e\x61\x70\x73\x68\x6f\x74\x3a\x3a\x6c\xa2\x72\x61\x77\x5f\x73\x69\x7a\x65\x5f\x20\x3d\x20\x6c\x84\x3b\xa",
              prefix, source_data.length());
    }
    fprintf(fp_, "\xa");
  }

  void WriteMeta(const char* prefix, const i::Serializer& ser) const {
    WriteSizeVar(ser, prefix, "\x6e\x65\x77", i::NEW_SPACE);
    WriteSizeVar(ser, prefix, "\x70\x6f\x69\x6e\x74\x65\x72", i::OLD_POINTER_SPACE);
    WriteSizeVar(ser, prefix, "\x64\x61\x74\x61", i::OLD_DATA_SPACE);
    WriteSizeVar(ser, prefix, "\x63\x6f\x64\x65", i::CODE_SPACE);
    WriteSizeVar(ser, prefix, "\x6d\x61\x70", i::MAP_SPACE);
    WriteSizeVar(ser, prefix, "\x63\x65\x6c\x6c", i::CELL_SPACE);
    WriteSizeVar(ser, prefix, "\x70\x72\x6f\x70\x65\x72\x74\x79\x5f\x63\x65\x6c\x6c", i::PROPERTY_CELL_SPACE);
    fprintf(fp_, "\xa");
  }

  void WriteSizeVar(const i::Serializer& ser, const char* prefix,
                    const char* name, int space) const {
    fprintf(fp_, "\x63\x6f\x6e\x73\x74\x20\x69\x6e\x74\x20\x53\x6e\x61\x70\x73\x68\x6f\x74\x3a\x3a\x6c\xa2\x6c\xa2\x5f\x73\x70\x61\x63\x65\x5f\x75\x73\x65\x64\x5f\x20\x3d\x20\x6c\x84\x3b\xa",
            prefix, name, ser.CurrentAllocationAddress(space));
  }

  void WriteSnapshotData(const i::List<i::byte>* data) const {
    for (int i = 0; i < data->length(); i++) {
      if ((i & 0x1f) == 0x1f)
        fprintf(fp_, "\xa");
      if (i > 0)
        fprintf(fp_, "\x2c");
      fprintf(fp_, "\x6c\xa4", static_cast<unsigned char>(data->at(i)));
    }
    fprintf(fp_, "\xa");
  }

  FILE* GetFileDescriptorOrDie(const char* filename) {
    FILE* fp = base::OS::FOpen(filename, "\x77\x62");
    if (fp == NULL) {
      i::PrintF("\x55\x6e\x61\x62\x6c\x65\x20\x74\x6f\x20\x6f\x70\x65\x6e\x20\x66\x69\x6c\x65\x20\x22\x6c\xa2\x22\x20\x66\x6f\x72\x20\x77\x72\x69\x74\x69\x6e\x67\x2e\xa", filename);
      exit(1);
    }
    return fp;
  }

  FILE* fp_;
  FILE* raw_file_;
  FILE* raw_context_file_;
  FILE* startup_blob_file_;
  Compressor* compressor_;
};


#ifdef COMPRESS_STARTUP_DATA_BZ2
class BZip2Compressor : public Compressor {
 public:
  BZip2Compressor() : output_(NULL) {}
  virtual ~BZip2Compressor() {
    delete output_;
  }
  virtual bool Compress(i::Vector<char> input) {
    delete output_;
    output_ = new i::ScopedVector<char>((input.length() * 101) / 100 + 1000);
    unsigned int output_length_ = output_->length();
    int result = BZ2_bzBuffToBuffCompress(output_->start(), &output_length_,
                                          input.start(), input.length(),
                                          9, 1, 0);
    if (result == BZ_OK) {
      output_->Truncate(output_length_);
      return true;
    } else {
      fprintf(stderr, "\x62\x7a\x6c\x69\x62\x20\x65\x72\x72\x6f\x72\x20\x63\x6f\x64\x65\x3a\x20\x6c\x84\xa", result);
      return false;
    }
  }
  virtual i::Vector<char>* output() { return output_; }

 private:
  i::ScopedVector<char>* output_;
};


class BZip2Decompressor : public StartupDataDecompressor {
 public:
  virtual ~BZip2Decompressor() { }

 protected:
  virtual int DecompressData(char* raw_data,
                             int* raw_data_size,
                             const char* compressed_data,
                             int compressed_data_size) {
    DCHECK_EQ(StartupData::kBZip2,
              V8::GetCompressedStartupDataAlgorithm());
    unsigned int decompressed_size = *raw_data_size;
    int result =
        BZ2_bzBuffToBuffDecompress(raw_data,
                                   &decompressed_size,
                                   const_cast<char*>(compressed_data),
                                   compressed_data_size,
                                   0, 1);
    if (result == BZ_OK) {
      *raw_data_size = decompressed_size;
    }
    return result;
  }
};
#endif


void DumpException(Handle<Message> message) {
  String::Utf8Value message_string(message->Get());
  String::Utf8Value message_line(message->GetSourceLine());
  fprintf(stderr, "\x6c\xa2\x20\x61\x74\x20\x6c\x69\x6e\x65\x20\x6c\x84\xa", *message_string, message->GetLineNumber());
  fprintf(stderr, "\x6c\xa2\xa", *message_line);
  for (int i = 0; i <= message->GetEndColumn(); ++i) {
    fprintf(stderr, "\x6c\x83", i < message->GetStartColumn() ? '\x20' : '\x5e');
  }
  fprintf(stderr, "\xa");
}


int main(int argc, char** argv) {
  V8::InitializeICU();
  v8::Platform* platform = v8::platform::CreateDefaultPlatform();
  v8::V8::InitializePlatform(platform);
  i::CpuFeatures::Probe(true);

  // By default, log code create information in the snapshot.
  i::FLAG_log_code = true;

  // Print the usage if an error occurs when parsing the command line
  // flags or if the help flag is set.
  int result = i::FlagList::SetFlagsFromCommandLine(&argc, argv, true);
  if (result > 0 || argc != 2 || i::FLAG_help) {
    ::printf("\x55\x73\x61\x67\x65\x3a\x20\x6c\xa2\x20\x5b\x66\x6c\x61\x67\x5d\x20\x2e\x2e\x2e\x20\x6f\x75\x74\x66\x69\x6c\x65\xa", argv[0]);
    i::FlagList::PrintHelp();
    return !i::FLAG_help;
  }
#ifdef COMPRESS_STARTUP_DATA_BZ2
  BZip2Decompressor natives_decompressor;
  int bz2_result = natives_decompressor.Decompress();
  if (bz2_result != BZ_OK) {
    fprintf(stderr, "\x62\x7a\x69\x70\x20\x65\x72\x72\x6f\x72\x20\x63\x6f\x64\x65\x3a\x20\x6c\x84\xa", bz2_result);
    exit(1);
  }
#endif
  i::FLAG_logfile_per_isolate = false;

  Isolate* isolate = v8::Isolate::New();
  { Isolate::Scope isolate_scope(isolate);
    i::Isolate* internal_isolate = reinterpret_cast<i::Isolate*>(isolate);
    internal_isolate->enable_serializer();

    Persistent<Context> context;
    {
      HandleScope handle_scope(isolate);
      context.Reset(isolate, Context::New(isolate));
    }

    if (context.IsEmpty()) {
      fprintf(stderr,
              "\xaE\x78\x63\x65\x70\x74\x69\x6f\x6e\x20\x74\x68\x72\x6f\x77\x6e\x20\x77\x68\x69\x6c\x65\x20\x63\x6f\x6d\x70\x69\x6c\x69\x6e\x67\x20\x6e\x61\x74\x69\x76\x65\x73\x20\x2d\x20\x73\x65\x65\x20\x61\x62\x6f\x76\x65\x2e\xa\xa");
      exit(1);
    }
    if (i::FLAG_extra_code != NULL) {
      // Capture 100 frames if anything happens.
      V8::SetCaptureStackTraceForUncaughtExceptions(true, 100);
      HandleScope scope(isolate);
      v8::Context::Scope cscope(v8::Local<v8::Context>::New(isolate, context));
      const char* name = i::FLAG_extra_code;
      FILE* file = base::OS::FOpen(name, "\x72\x62");
      if (file == NULL) {
        fprintf(stderr, "\x46\x61\x69\x6c\x65\x64\x20\x74\x6f\x20\x6f\x70\x65\x6e\x20\x27\x6c\xa2\x27\x3a\x20\x65\x72\x72\x6e\x6f\x20\x6c\x84\xa", name, errno);
        exit(1);
      }

      fseek(file, 0, SEEK_END);
      int size = ftell(file);
      rewind(file);

      char* chars = new char[size + 1];
      chars[size] = '\x0';
      for (int i = 0; i < size;) {
        int read = static_cast<int>(fread(&chars[i], 1, size - i, file));
        if (read < 0) {
          fprintf(stderr, "\x46\x61\x69\x6c\x65\x64\x20\x74\x6f\x20\x72\x65\x61\x64\x20\x27\x6c\xa2\x27\x3a\x20\x65\x72\x72\x6e\x6f\x20\x6c\x84\xa", name, errno);
          exit(1);
        }
        i += read;
      }
      fclose(file);
      Local<String> source = String::NewFromUtf8(isolate, chars);
      TryCatch try_catch;
      Local<Script> script = Script::Compile(source);
      if (try_catch.HasCaught()) {
        fprintf(stderr, "\x46\x61\x69\x6c\x75\x72\x65\x20\x63\x6f\x6d\x70\x69\x6c\x69\x6e\x67\x20\x27\x6c\xa2\x27\xa", name);
        DumpException(try_catch.Message());
        exit(1);
      }
      script->Run();
      if (try_catch.HasCaught()) {
        fprintf(stderr, "\x46\x61\x69\x6c\x75\x72\x65\x20\x72\x75\x6e\x6e\x69\x6e\x67\x20\x27\x6c\xa2\x27\xa", name);
        DumpException(try_catch.Message());
        exit(1);
      }
    }
    // Make sure all builtin scripts are cached.
    { HandleScope scope(isolate);
      for (int i = 0; i < i::Natives::GetBuiltinsCount(); i++) {
        internal_isolate->bootstrapper()->NativesSourceLookup(i);
      }
    }
    // If we don't do this then we end up with a stray root pointing at the
    // context even after we have disposed of the context.
    internal_isolate->heap()->CollectAllGarbage(
        i::Heap::kNoGCFlags, "\x6d\x6b\x73\x6e\x61\x70\x73\x68\x6f\x74");
    i::Object* raw_context = *v8::Utils::OpenPersistent(context);
    context.Reset();

    // This results in a somewhat smaller snapshot, probably because it gets
    // rid of some things that are cached between garbage collections.
    i::List<i::byte> snapshot_data;
    i::ListSnapshotSink snapshot_sink(&snapshot_data);
    i::StartupSerializer ser(internal_isolate, &snapshot_sink);
    ser.SerializeStrongReferences();

    i::List<i::byte> context_data;
    i::ListSnapshotSink contex_sink(&context_data);
    i::PartialSerializer context_ser(internal_isolate, &ser, &contex_sink);
    context_ser.Serialize(&raw_context);
    ser.SerializeWeakReferences();

    {
      SnapshotWriter writer(argv[1]);
      if (i::FLAG_raw_file && i::FLAG_raw_context_file)
        writer.SetRawFiles(i::FLAG_raw_file, i::FLAG_raw_context_file);
      if (i::FLAG_startup_blob)
        writer.SetStartupBlobFile(i::FLAG_startup_blob);
  #ifdef COMPRESS_STARTUP_DATA_BZ2
      BZip2Compressor bzip2;
      writer.SetCompressor(&bzip2);
  #endif
      writer.WriteSnapshot(snapshot_data, ser, context_data, context_ser);
    }
  }

  isolate->Dispose();
  V8::Dispose();
  V8::ShutdownPlatform();
  delete platform;
  return 0;
}
