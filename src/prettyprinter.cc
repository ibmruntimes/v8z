// Copyright 2012 the V8 project authors. All rights reserved.
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.

#include <stdarg.h>

#include "src/v8.h"

#include "src/ast-value-factory.h"
#include "src/base/platform/platform.h"
#include "src/prettyprinter.h"
#include "src/scopes.h"

namespace v8 {
namespace internal {

#ifdef DEBUG

PrettyPrinter::PrettyPrinter(Zone* zone) {
  output_ = NULL;
  size_ = 0;
  pos_ = 0;
  InitializeAstVisitor(zone);
}


PrettyPrinter::~PrettyPrinter() {
  DeleteArray(output_);
}


void PrettyPrinter::VisitBlock(Block* node) {
  if (!node->is_initializer_block()) Print("\x7b\x20");
  PrintStatements(node->statements());
  if (node->statements()->length() > 0) Print("\x20");
  if (!node->is_initializer_block()) Print("\x7d");
}


void PrettyPrinter::VisitVariableDeclaration(VariableDeclaration* node) {
  Print("\x76\x61\x72\x20");
  PrintLiteral(node->proxy()->name(), false);
  Print("\x3b");
}


void PrettyPrinter::VisitFunctionDeclaration(FunctionDeclaration* node) {
  Print("\x66\x75\x6e\x63\x74\x69\x6f\x6e\x20");
  PrintLiteral(node->proxy()->name(), false);
  Print("\x20\x3d\x20");
  PrintFunctionLiteral(node->fun());
  Print("\x3b");
}


void PrettyPrinter::VisitModuleDeclaration(ModuleDeclaration* node) {
  Print("\x6d\x6f\x64\x75\x6c\x65\x20");
  PrintLiteral(node->proxy()->name(), false);
  Print("\x20\x3d\x20");
  Visit(node->module());
  Print("\x3b");
}


void PrettyPrinter::VisitImportDeclaration(ImportDeclaration* node) {
  Print("\x69\x6d\x70\x6f\x72\x74\x20");
  PrintLiteral(node->proxy()->name(), false);
  Print("\x20\x66\x72\x6f\x6d\x20");
  Visit(node->module());
  Print("\x3b");
}


void PrettyPrinter::VisitExportDeclaration(ExportDeclaration* node) {
  Print("\x65\x78\x70\x6f\x72\x74\x20");
  PrintLiteral(node->proxy()->name(), false);
  Print("\x3b");
}


void PrettyPrinter::VisitModuleLiteral(ModuleLiteral* node) {
  VisitBlock(node->body());
}


void PrettyPrinter::VisitModuleVariable(ModuleVariable* node) {
  Visit(node->proxy());
}


void PrettyPrinter::VisitModulePath(ModulePath* node) {
  Visit(node->module());
  Print("\x2e");
  PrintLiteral(node->name(), false);
}


void PrettyPrinter::VisitModuleUrl(ModuleUrl* node) {
  Print("\x61\x74\x20");
  PrintLiteral(node->url(), true);
}


void PrettyPrinter::VisitModuleStatement(ModuleStatement* node) {
  Print("\x6d\x6f\x64\x75\x6c\x65\x20");
  PrintLiteral(node->proxy()->name(), false);
  Print("\x20");
  Visit(node->body());
}


void PrettyPrinter::VisitExpressionStatement(ExpressionStatement* node) {
  Visit(node->expression());
  Print("\x3b");
}


void PrettyPrinter::VisitEmptyStatement(EmptyStatement* node) {
  Print("\x3b");
}


void PrettyPrinter::VisitIfStatement(IfStatement* node) {
  Print("\x69\x66\x20\x28");
  Visit(node->condition());
  Print("\x29\x20");
  Visit(node->then_statement());
  if (node->HasElseStatement()) {
    Print("\x20\x65\x6c\x73\x65\x20");
    Visit(node->else_statement());
  }
}


void PrettyPrinter::VisitContinueStatement(ContinueStatement* node) {
  Print("\x63\x6f\x6e\x74\x69\x6e\x75\x65");
  ZoneList<const AstRawString*>* labels = node->target()->labels();
  if (labels != NULL) {
    Print("\x20");
    DCHECK(labels->length() > 0);  // guaranteed to have at least one entry
    PrintLiteral(labels->at(0), false);  // any label from the list is fine
  }
  Print("\x3b");
}


void PrettyPrinter::VisitBreakStatement(BreakStatement* node) {
  Print("\x62\x72\x65\x61\x6b");
  ZoneList<const AstRawString*>* labels = node->target()->labels();
  if (labels != NULL) {
    Print("\x20");
    DCHECK(labels->length() > 0);  // guaranteed to have at least one entry
    PrintLiteral(labels->at(0), false);  // any label from the list is fine
  }
  Print("\x3b");
}


void PrettyPrinter::VisitReturnStatement(ReturnStatement* node) {
  Print("\x72\x65\x74\x75\x72\x6e\x20");
  Visit(node->expression());
  Print("\x3b");
}


void PrettyPrinter::VisitWithStatement(WithStatement* node) {
  Print("\x77\x69\x74\x68\x20\x28");
  Visit(node->expression());
  Print("\x29\x20");
  Visit(node->statement());
}


void PrettyPrinter::VisitSwitchStatement(SwitchStatement* node) {
  PrintLabels(node->labels());
  Print("\x73\x77\x69\x74\x63\x68\x20\x28");
  Visit(node->tag());
  Print("\x29\x20\x7b\x20");
  ZoneList<CaseClause*>* cases = node->cases();
  for (int i = 0; i < cases->length(); i++)
    Visit(cases->at(i));
  Print("\x7d");
}


void PrettyPrinter::VisitCaseClause(CaseClause* clause) {
  if (clause->is_default()) {
    Print("\x64\x65\x66\x61\x75\x6c\x74");
  } else {
    Print("\x63\x61\x73\x65\x20");
    Visit(clause->label());
  }
  Print("\x3a\x20");
  PrintStatements(clause->statements());
  if (clause->statements()->length() > 0)
    Print("\x20");
}


void PrettyPrinter::VisitDoWhileStatement(DoWhileStatement* node) {
  PrintLabels(node->labels());
  Print("\x64\x6f\x20");
  Visit(node->body());
  Print("\x20\x77\x68\x69\x6c\x65\x20\x28");
  Visit(node->cond());
  Print("\x29\x3b");
}


void PrettyPrinter::VisitWhileStatement(WhileStatement* node) {
  PrintLabels(node->labels());
  Print("\x77\x68\x69\x6c\x65\x20\x28");
  Visit(node->cond());
  Print("\x29\x20");
  Visit(node->body());
}


void PrettyPrinter::VisitForStatement(ForStatement* node) {
  PrintLabels(node->labels());
  Print("\x66\x6f\x72\x20\x28");
  if (node->init() != NULL) {
    Visit(node->init());
    Print("\x20");
  } else {
    Print("\x3b\x20");
  }
  if (node->cond() != NULL) Visit(node->cond());
  Print("\x3b\x20");
  if (node->next() != NULL) {
    Visit(node->next());  // prints extra '\x3b', unfortunately
    // to fix: should use Expression for next
  }
  Print("\x29\x20");
  Visit(node->body());
}


void PrettyPrinter::VisitForInStatement(ForInStatement* node) {
  PrintLabels(node->labels());
  Print("\x66\x6f\x72\x20\x28");
  Visit(node->each());
  Print("\x20\x69\x6e\x20");
  Visit(node->enumerable());
  Print("\x29\x20");
  Visit(node->body());
}


void PrettyPrinter::VisitForOfStatement(ForOfStatement* node) {
  PrintLabels(node->labels());
  Print("\x66\x6f\x72\x20\x28");
  Visit(node->each());
  Print("\x20\x6f\x66\x20");
  Visit(node->iterable());
  Print("\x29\x20");
  Visit(node->body());
}


void PrettyPrinter::VisitTryCatchStatement(TryCatchStatement* node) {
  Print("\x74\x72\x79\x20");
  Visit(node->try_block());
  Print("\x20\x63\x61\x74\x63\x68\x20\x28");
  const bool quote = false;
  PrintLiteral(node->variable()->name(), quote);
  Print("\x29\x20");
  Visit(node->catch_block());
}


void PrettyPrinter::VisitTryFinallyStatement(TryFinallyStatement* node) {
  Print("\x74\x72\x79\x20");
  Visit(node->try_block());
  Print("\x20\x66\x69\x6e\x61\x6c\x6c\x79\x20");
  Visit(node->finally_block());
}


void PrettyPrinter::VisitDebuggerStatement(DebuggerStatement* node) {
  Print("\x64\x65\x62\x75\x67\x67\x65\x72\x20");
}


void PrettyPrinter::VisitFunctionLiteral(FunctionLiteral* node) {
  Print("\x28");
  PrintFunctionLiteral(node);
  Print("\x29");
}


void PrettyPrinter::VisitNativeFunctionLiteral(NativeFunctionLiteral* node) {
  Print("\x28");
  PrintLiteral(node->name(), false);
  Print("\x29");
}


void PrettyPrinter::VisitConditional(Conditional* node) {
  Visit(node->condition());
  Print("\x20\x3f\x20");
  Visit(node->then_expression());
  Print("\x20\x3a\x20");
  Visit(node->else_expression());
}


void PrettyPrinter::VisitLiteral(Literal* node) {
  PrintLiteral(node->value(), true);
}


void PrettyPrinter::VisitRegExpLiteral(RegExpLiteral* node) {
  Print("\x20\x52\x65\x67\x45\x78\x70\x28");
  PrintLiteral(node->pattern(), false);
  Print("\x2c");
  PrintLiteral(node->flags(), false);
  Print("\x29\x20");
}


void PrettyPrinter::VisitObjectLiteral(ObjectLiteral* node) {
  Print("\x7b\x20");
  for (int i = 0; i < node->properties()->length(); i++) {
    if (i != 0) Print("\x2c");
    ObjectLiteral::Property* property = node->properties()->at(i);
    Print("\x20");
    Visit(property->key());
    Print("\x3a\x20");
    Visit(property->value());
  }
  Print("\x20\x7d");
}


void PrettyPrinter::VisitArrayLiteral(ArrayLiteral* node) {
  Print("\x5b\x20");
  for (int i = 0; i < node->values()->length(); i++) {
    if (i != 0) Print("\x2c");
    Visit(node->values()->at(i));
  }
  Print("\x20\x5d");
}


void PrettyPrinter::VisitVariableProxy(VariableProxy* node) {
  PrintLiteral(node->name(), false);
}


void PrettyPrinter::VisitAssignment(Assignment* node) {
  Visit(node->target());
  Print("\x20\x6c\xa2\x20", Token::String(node->op()));
  Visit(node->value());
}


void PrettyPrinter::VisitYield(Yield* node) {
  Print("\x79\x69\x65\x6c\x64\x20");
  Visit(node->expression());
}


void PrettyPrinter::VisitThrow(Throw* node) {
  Print("\x74\x68\x72\x6f\x77\x20");
  Visit(node->exception());
}


void PrettyPrinter::VisitProperty(Property* node) {
  Expression* key = node->key();
  Literal* literal = key->AsLiteral();
  if (literal != NULL && literal->value()->IsInternalizedString()) {
    Print("\x28");
    Visit(node->obj());
    Print("\x29\x2e");
    PrintLiteral(literal->value(), false);
  } else {
    Visit(node->obj());
    Print("\x5b");
    Visit(key);
    Print("\x5d");
  }
}


void PrettyPrinter::VisitCall(Call* node) {
  Visit(node->expression());
  PrintArguments(node->arguments());
}


void PrettyPrinter::VisitCallNew(CallNew* node) {
  Print("\x6e\x65\x77\x20\x28");
  Visit(node->expression());
  Print("\x29");
  PrintArguments(node->arguments());
}


void PrettyPrinter::VisitCallRuntime(CallRuntime* node) {
  Print("\x25\x25");
  PrintLiteral(node->name(), false);
  PrintArguments(node->arguments());
}


void PrettyPrinter::VisitUnaryOperation(UnaryOperation* node) {
  Token::Value op = node->op();
  bool needsSpace =
      op == Token::DELETE || op == Token::TYPEOF || op == Token::VOID;
  Print("\x28\x6c\xa2\x6c\xa2", Token::String(op), needsSpace ? "\x20" : "");
  Visit(node->expression());
  Print("\x29");
}


void PrettyPrinter::VisitCountOperation(CountOperation* node) {
  Print("\x28");
  if (node->is_prefix()) Print("\x6c\xa2", Token::String(node->op()));
  Visit(node->expression());
  if (node->is_postfix()) Print("\x6c\xa2", Token::String(node->op()));
  Print("\x29");
}


void PrettyPrinter::VisitBinaryOperation(BinaryOperation* node) {
  Print("\x28");
  Visit(node->left());
  Print("\x20\x6c\xa2\x20", Token::String(node->op()));
  Visit(node->right());
  Print("\x29");
}


void PrettyPrinter::VisitCompareOperation(CompareOperation* node) {
  Print("\x28");
  Visit(node->left());
  Print("\x20\x6c\xa2\x20", Token::String(node->op()));
  Visit(node->right());
  Print("\x29");
}


void PrettyPrinter::VisitThisFunction(ThisFunction* node) {
  Print("\x3c\x74\x68\x69\x73\x2d\x66\x75\x6e\x63\x74\x69\x6f\x6e\x3e");
}


const char* PrettyPrinter::Print(AstNode* node) {
  Init();
  Visit(node);
  return output_;
}


const char* PrettyPrinter::PrintExpression(FunctionLiteral* program) {
  Init();
  ExpressionStatement* statement =
    program->body()->at(0)->AsExpressionStatement();
  Visit(statement->expression());
  return output_;
}


const char* PrettyPrinter::PrintProgram(FunctionLiteral* program) {
  Init();
  PrintStatements(program->body());
  Print("\xa");
  return output_;
}


void PrettyPrinter::PrintOut(Zone* zone, AstNode* node) {
  PrettyPrinter printer(zone);
  PrintF("\x6c\xa2", printer.Print(node));
}


void PrettyPrinter::Init() {
  if (size_ == 0) {
    DCHECK(output_ == NULL);
    const int initial_size = 256;
    output_ = NewArray<char>(initial_size);
    size_ = initial_size;
  }
  output_[0] = '\x0';
  pos_ = 0;
}


void PrettyPrinter::Print(const char* format, ...) {
  for (;;) {
    va_list arguments;
    va_start(arguments, format);
    int n = VSNPrintF(Vector<char>(output_, size_) + pos_,
                      format,
                      arguments);
    va_end(arguments);

    if (n >= 0) {
      // there was enough space - we are done
      pos_ += n;
      return;
    } else {
      // there was not enough space - allocate more and try again
      const int slack = 32;
      int new_size = size_ + (size_ >> 1) + slack;
      char* new_output = NewArray<char>(new_size);
      MemCopy(new_output, output_, pos_);
      DeleteArray(output_);
      output_ = new_output;
      size_ = new_size;
    }
  }
}


void PrettyPrinter::PrintStatements(ZoneList<Statement*>* statements) {
  if (statements == NULL) return;
  for (int i = 0; i < statements->length(); i++) {
    if (i != 0) Print("\x20");
    Visit(statements->at(i));
  }
}


void PrettyPrinter::PrintLabels(ZoneList<const AstRawString*>* labels) {
  if (labels != NULL) {
    for (int i = 0; i < labels->length(); i++) {
      PrintLiteral(labels->at(i), false);
      Print("\x3a\x20");
    }
  }
}


void PrettyPrinter::PrintArguments(ZoneList<Expression*>* arguments) {
  Print("\x28");
  for (int i = 0; i < arguments->length(); i++) {
    if (i != 0) Print("\x2c\x20");
    Visit(arguments->at(i));
  }
  Print("\x29");
}


void PrettyPrinter::PrintLiteral(Handle<Object> value, bool quote) {
  Object* object = *value;
  if (object->IsString()) {
    String* string = String::cast(object);
    if (quote) Print("\x22");
    for (int i = 0; i < string->length(); i++) {
      Print("\x6c\x83", string->Get(i));
    }
    if (quote) Print("\x22");
  } else if (object->IsNull()) {
    Print("\x6e\x75\x6c\x6c");
  } else if (object->IsTrue()) {
    Print("\x74\x72\x75\x65");
  } else if (object->IsFalse()) {
    Print("\x66\x61\x6c\x73\x65");
  } else if (object->IsUndefined()) {
    Print("\x75\x6e\x64\x65\x66\x69\x6e\x65\x64");
  } else if (object->IsNumber()) {
    Print("\x6c\x87", object->Number());
  } else if (object->IsJSObject()) {
    // regular expression
    if (object->IsJSFunction()) {
      Print("\x4a\x53\x2d\x46\x75\x6e\x63\x74\x69\x6f\x6e");
    } else if (object->IsJSArray()) {
      Print("\x4a\x53\x2d\x61\x72\x72\x61\x79\x5b\x6c\xa4\x5d", JSArray::cast(object)->length());
    } else if (object->IsJSObject()) {
      Print("\x4a\x53\x2d\x4f\x62\x6a\x65\x63\x74");
    } else {
      Print("\x3f\x55\x4e\x4b\x4e\x4f\x57\x4e\x3f");
    }
  } else if (object->IsFixedArray()) {
    Print("\x46\x69\x78\x65\x64\x41\x72\x72\x61\x79");
  } else {
    Print("\x3c\x75\x6e\x6b\x6e\x6f\x77\x6e\x20\x6c\x69\x74\x65\x72\x61\x6c\x20\x6c\x97\x3e", object);
  }
}


void PrettyPrinter::PrintLiteral(const AstRawString* value, bool quote) {
  PrintLiteral(value->string(), quote);
}


void PrettyPrinter::PrintParameters(Scope* scope) {
  Print("\x28");
  for (int i = 0; i < scope->num_parameters(); i++) {
    if (i  > 0) Print("\x2c\x20");
    PrintLiteral(scope->parameter(i)->name(), false);
  }
  Print("\x29");
}


void PrettyPrinter::PrintDeclarations(ZoneList<Declaration*>* declarations) {
  for (int i = 0; i < declarations->length(); i++) {
    if (i > 0) Print("\x20");
    Visit(declarations->at(i));
  }
}


void PrettyPrinter::PrintFunctionLiteral(FunctionLiteral* function) {
  Print("\x66\x75\x6e\x63\x74\x69\x6f\x6e\x20");
  PrintLiteral(function->name(), false);
  PrintParameters(function->scope());
  Print("\x20\x7b\x20");
  PrintDeclarations(function->scope()->declarations());
  PrintStatements(function->body());
  Print("\x20\x7d");
}


//-----------------------------------------------------------------------------

class IndentedScope BASE_EMBEDDED {
 public:
  IndentedScope(AstPrinter* printer, const char* txt)
      : ast_printer_(printer) {
    ast_printer_->PrintIndented(txt);
    ast_printer_->Print("\xa");
    ast_printer_->inc_indent();
  }

  virtual ~IndentedScope() {
    ast_printer_->dec_indent();
  }

 private:
  AstPrinter* ast_printer_;
};


//-----------------------------------------------------------------------------


AstPrinter::AstPrinter(Zone* zone) : PrettyPrinter(zone), indent_(0) {
}


AstPrinter::~AstPrinter() {
  DCHECK(indent_ == 0);
}


void AstPrinter::PrintIndented(const char* txt) {
  for (int i = 0; i < indent_; i++) {
    Print("\x2e\x20");
  }
  Print(txt);
}


void AstPrinter::PrintLiteralIndented(const char* info,
                                      Handle<Object> value,
                                      bool quote) {
  PrintIndented(info);
  Print("\x20");
  PrintLiteral(value, quote);
  Print("\xa");
}


void AstPrinter::PrintLiteralWithModeIndented(const char* info,
                                              Variable* var,
                                              Handle<Object> value) {
  if (var == NULL) {
    PrintLiteralIndented(info, value, true);
  } else {
    EmbeddedVector<char, 256> buf;
    int pos = SNPrintF(buf, "\x6c\xa2\x20\x28\x6d\x6f\x64\x65\x20\x3d\x20\x6c\xa2", info,
                       Variable::Mode2String(var->mode()));
    SNPrintF(buf + pos, "\x29");
    PrintLiteralIndented(buf.start(), value, true);
  }
}


void AstPrinter::PrintLabelsIndented(ZoneList<const AstRawString*>* labels) {
  if (labels == NULL || labels->length() == 0) return;
  PrintIndented("\x4c\x41\x42\x45\x4c\x53\x20");
  PrintLabels(labels);
  Print("\xa");
}


void AstPrinter::PrintIndentedVisit(const char* s, AstNode* node) {
  IndentedScope indent(this, s);
  Visit(node);
}


const char* AstPrinter::PrintProgram(FunctionLiteral* program) {
  Init();
  { IndentedScope indent(this, "\x46\x55\x4e\x43");
    PrintLiteralIndented("\x4e\x41\x4d\x45", program->name(), true);
    PrintLiteralIndented("\x49\x4e\x46\x45\x52\x52\x45\x44\x20\x4e\x41\x4d\x45", program->inferred_name(), true);
    PrintParameters(program->scope());
    PrintDeclarations(program->scope()->declarations());
    PrintStatements(program->body());
  }
  return Output();
}


void AstPrinter::PrintDeclarations(ZoneList<Declaration*>* declarations) {
  if (declarations->length() > 0) {
    IndentedScope indent(this, "\x44\x45\x43\x4c\x53");
    for (int i = 0; i < declarations->length(); i++) {
      Visit(declarations->at(i));
    }
  }
}


void AstPrinter::PrintParameters(Scope* scope) {
  if (scope->num_parameters() > 0) {
    IndentedScope indent(this, "\x50\x41\x52\x41\x4d\x53");
    for (int i = 0; i < scope->num_parameters(); i++) {
      PrintLiteralWithModeIndented("\x56\x41\x52", scope->parameter(i),
                                   scope->parameter(i)->name());
    }
  }
}


void AstPrinter::PrintStatements(ZoneList<Statement*>* statements) {
  for (int i = 0; i < statements->length(); i++) {
    Visit(statements->at(i));
  }
}


void AstPrinter::PrintArguments(ZoneList<Expression*>* arguments) {
  for (int i = 0; i < arguments->length(); i++) {
    Visit(arguments->at(i));
  }
}


void AstPrinter::VisitBlock(Block* node) {
  const char* block_txt = node->is_initializer_block() ? "\x42\x4c\x4f\x43\x4b\x20\x49\x4e\x49\x54" : "\x42\x4c\x4f\x43\x4b";
  IndentedScope indent(this, block_txt);
  PrintStatements(node->statements());
}


// TODO(svenpanne) Start with IndentedScope.
void AstPrinter::VisitVariableDeclaration(VariableDeclaration* node) {
  PrintLiteralWithModeIndented(Variable::Mode2String(node->mode()),
                               node->proxy()->var(),
                               node->proxy()->name());
}


// TODO(svenpanne) Start with IndentedScope.
void AstPrinter::VisitFunctionDeclaration(FunctionDeclaration* node) {
  PrintIndented("\x46\x55\x4e\x43\x54\x49\x4f\x4e\x20");
  PrintLiteral(node->proxy()->name(), true);
  Print("\x20\x3d\x20\x66\x75\x6e\x63\x74\x69\x6f\x6e\x20");
  PrintLiteral(node->fun()->name(), false);
  Print("\xa");
}


void AstPrinter::VisitModuleDeclaration(ModuleDeclaration* node) {
  IndentedScope indent(this, "\x4d\x4f\x44\x55\x4c\x45");
  PrintLiteralIndented("\x4e\x41\x4d\x45", node->proxy()->name(), true);
  Visit(node->module());
}


void AstPrinter::VisitImportDeclaration(ImportDeclaration* node) {
  IndentedScope indent(this, "\x49\x4d\x50\x4f\x52\x54");
  PrintLiteralIndented("\x4e\x41\x4d\x45", node->proxy()->name(), true);
  Visit(node->module());
}


void AstPrinter::VisitExportDeclaration(ExportDeclaration* node) {
  IndentedScope indent(this, "\x45\x58\x50\x4f\x52\x54\x20");
  PrintLiteral(node->proxy()->name(), true);
}


void AstPrinter::VisitModuleLiteral(ModuleLiteral* node) {
  IndentedScope indent(this, "\x4d\x4f\x44\x55\x4c\x45\x20\x4c\x49\x54\x45\x52\x41\x4c");
  VisitBlock(node->body());
}


void AstPrinter::VisitModuleVariable(ModuleVariable* node) {
  IndentedScope indent(this, "\x4d\x4f\x44\x55\x4c\x45\x20\x56\x41\x52\x49\x41\x42\x4c\x45");
  Visit(node->proxy());
}


void AstPrinter::VisitModulePath(ModulePath* node) {
  IndentedScope indent(this, "\x4d\x4f\x44\x55\x4c\x45\x20\x50\x41\x54\x48");
  PrintIndentedVisit("\x4d\x4f\x44\x55\x4c\x45\x20\x50\x41\x54\x48\x20\x50\x41\x52\x45\x4e\x54", node->module());
  PrintLiteralIndented("\x4e\x41\x4d\x45", node->name(), true);
}


void AstPrinter::VisitModuleUrl(ModuleUrl* node) {
  PrintLiteralIndented("\x55\x52\x4c", node->url(), true);
}


void AstPrinter::VisitModuleStatement(ModuleStatement* node) {
  IndentedScope indent(this, "\x4d\x4f\x44\x55\x4c\x45\x20\x53\x54\x41\x54\x45\x4d\x45\x4e\x54");
  PrintLiteralIndented("\x4e\x41\x4d\x45", node->proxy()->name(), true);
  PrintStatements(node->body()->statements());
}


void AstPrinter::VisitExpressionStatement(ExpressionStatement* node) {
  IndentedScope indent(this, "\x45\x58\x50\x52\x45\x53\x53\x49\x4f\x4e\x20\x53\x54\x41\x54\x45\x4d\x45\x4e\x54");
  Visit(node->expression());
}


void AstPrinter::VisitEmptyStatement(EmptyStatement* node) {
  IndentedScope indent(this, "\x45\x4d\x50\x54\x59");
}


void AstPrinter::VisitIfStatement(IfStatement* node) {
  IndentedScope indent(this, "\x49\x46");
  PrintIndentedVisit("\x43\x4f\x4e\x44\x49\x54\x49\x4f\x4e", node->condition());
  PrintIndentedVisit("\x54\x48\x45\x4e", node->then_statement());
  if (node->HasElseStatement()) {
    PrintIndentedVisit("\x45\x4c\x53\x45", node->else_statement());
  }
}


void AstPrinter::VisitContinueStatement(ContinueStatement* node) {
  IndentedScope indent(this, "\x43\x4f\x4e\x54\x49\x4e\x55\x45");
  PrintLabelsIndented(node->target()->labels());
}


void AstPrinter::VisitBreakStatement(BreakStatement* node) {
  IndentedScope indent(this, "\x42\x52\x45\x41\x4b");
  PrintLabelsIndented(node->target()->labels());
}


void AstPrinter::VisitReturnStatement(ReturnStatement* node) {
  IndentedScope indent(this, "\x52\x45\x54\x55\x52\x4e");
  Visit(node->expression());
}


void AstPrinter::VisitWithStatement(WithStatement* node) {
  IndentedScope indent(this, "\x57\x49\x54\x48");
  PrintIndentedVisit("\x4f\x42\x4a\x45\x43\x54", node->expression());
  PrintIndentedVisit("\x42\x4f\x44\x59", node->statement());
}


void AstPrinter::VisitSwitchStatement(SwitchStatement* node) {
  IndentedScope indent(this, "\x53\x57\x49\x54\x43\x48");
  PrintLabelsIndented(node->labels());
  PrintIndentedVisit("\x54\x41\x47", node->tag());
  for (int i = 0; i < node->cases()->length(); i++) {
    Visit(node->cases()->at(i));
  }
}


void AstPrinter::VisitCaseClause(CaseClause* clause) {
  if (clause->is_default()) {
    IndentedScope indent(this, "\x44\x45\x46\x41\x55\x4c\x54");
    PrintStatements(clause->statements());
  } else {
    IndentedScope indent(this, "\x43\x41\x53\x45");
    Visit(clause->label());
    PrintStatements(clause->statements());
  }
}


void AstPrinter::VisitDoWhileStatement(DoWhileStatement* node) {
  IndentedScope indent(this, "\x44\x4f");
  PrintLabelsIndented(node->labels());
  PrintIndentedVisit("\x42\x4f\x44\x59", node->body());
  PrintIndentedVisit("\x43\x4f\x4e\x44", node->cond());
}


void AstPrinter::VisitWhileStatement(WhileStatement* node) {
  IndentedScope indent(this, "\x57\x48\x49\x4c\x45");
  PrintLabelsIndented(node->labels());
  PrintIndentedVisit("\x43\x4f\x4e\x44", node->cond());
  PrintIndentedVisit("\x42\x4f\x44\x59", node->body());
}


void AstPrinter::VisitForStatement(ForStatement* node) {
  IndentedScope indent(this, "\x46\x4f\x52");
  PrintLabelsIndented(node->labels());
  if (node->init()) PrintIndentedVisit("\x49\x4e\x49\x54", node->init());
  if (node->cond()) PrintIndentedVisit("\x43\x4f\x4e\x44", node->cond());
  PrintIndentedVisit("\x42\x4f\x44\x59", node->body());
  if (node->next()) PrintIndentedVisit("\x4e\x45\x58\x54", node->next());
}


void AstPrinter::VisitForInStatement(ForInStatement* node) {
  IndentedScope indent(this, "\x46\x4f\x52\x20\x49\x4e");
  PrintIndentedVisit("\x46\x4f\x52", node->each());
  PrintIndentedVisit("\x49\x4e", node->enumerable());
  PrintIndentedVisit("\x42\x4f\x44\x59", node->body());
}


void AstPrinter::VisitForOfStatement(ForOfStatement* node) {
  IndentedScope indent(this, "\x46\x4f\x52\x20\x4f\x46");
  PrintIndentedVisit("\x46\x4f\x52", node->each());
  PrintIndentedVisit("\x4f\x46", node->iterable());
  PrintIndentedVisit("\x42\x4f\x44\x59", node->body());
}


void AstPrinter::VisitTryCatchStatement(TryCatchStatement* node) {
  IndentedScope indent(this, "\x54\x52\x59\x20\x43\x41\x54\x43\x48");
  PrintIndentedVisit("\x54\x52\x59", node->try_block());
  PrintLiteralWithModeIndented("\x43\x41\x54\x43\x48\x56\x41\x52",
                               node->variable(),
                               node->variable()->name());
  PrintIndentedVisit("\x43\x41\x54\x43\x48", node->catch_block());
}


void AstPrinter::VisitTryFinallyStatement(TryFinallyStatement* node) {
  IndentedScope indent(this, "\x54\x52\x59\x20\x46\x49\x4e\x41\x4c\x4c\x59");
  PrintIndentedVisit("\x54\x52\x59", node->try_block());
  PrintIndentedVisit("\x46\x49\x4e\x41\x4c\x4c\x59", node->finally_block());
}


void AstPrinter::VisitDebuggerStatement(DebuggerStatement* node) {
  IndentedScope indent(this, "\x44\x45\x42\x55\x47\x47\x45\x52");
}


void AstPrinter::VisitFunctionLiteral(FunctionLiteral* node) {
  IndentedScope indent(this, "\x46\x55\x4e\x43\x20\x4c\x49\x54\x45\x52\x41\x4c");
  PrintLiteralIndented("\x4e\x41\x4d\x45", node->name(), false);
  PrintLiteralIndented("\x49\x4e\x46\x45\x52\x52\x45\x44\x20\x4e\x41\x4d\x45", node->inferred_name(), false);
  PrintParameters(node->scope());
  // We don't want to see the function literal in this case: it
  // will be printed via PrintProgram when the code for it is
  // generated.
  // PrintStatements(node->body());
}


void AstPrinter::VisitNativeFunctionLiteral(NativeFunctionLiteral* node) {
  IndentedScope indent(this, "\x4e\x41\x54\x49\x56\x45\x20\x46\x55\x4e\x43\x20\x4c\x49\x54\x45\x52\x41\x4c");
  PrintLiteralIndented("\x4e\x41\x4d\x45", node->name(), false);
}


void AstPrinter::VisitConditional(Conditional* node) {
  IndentedScope indent(this, "\x43\x4f\x4e\x44\x49\x54\x49\x4f\x4e\x41\x4c");
  PrintIndentedVisit("\x43\x4f\x4e\x44\x49\x54\x49\x4f\x4e", node->condition());
  PrintIndentedVisit("\x54\x48\x45\x4e", node->then_expression());
  PrintIndentedVisit("\x45\x4c\x53\x45", node->else_expression());
}


// TODO(svenpanne) Start with IndentedScope.
void AstPrinter::VisitLiteral(Literal* node) {
  PrintLiteralIndented("\x4c\x49\x54\x45\x52\x41\x4c", node->value(), true);
}


void AstPrinter::VisitRegExpLiteral(RegExpLiteral* node) {
  IndentedScope indent(this, "\x52\x45\x47\x45\x58\x50\x20\x4c\x49\x54\x45\x52\x41\x4c");
  PrintLiteralIndented("\x50\x41\x54\x54\x45\x52\x4e", node->pattern(), false);
  PrintLiteralIndented("\x46\x4c\x41\x47\x53", node->flags(), false);
}


void AstPrinter::VisitObjectLiteral(ObjectLiteral* node) {
  IndentedScope indent(this, "\x4f\x42\x4a\x20\x4c\x49\x54\x45\x52\x41\x4c");
  for (int i = 0; i < node->properties()->length(); i++) {
    const char* prop_kind = NULL;
    switch (node->properties()->at(i)->kind()) {
      case ObjectLiteral::Property::CONSTANT:
        prop_kind = "\x50\x52\x4f\x50\x45\x52\x54\x59\x20\x2d\x20\x43\x4f\x4e\x53\x54\x41\x4e\x54";
        break;
      case ObjectLiteral::Property::COMPUTED:
        prop_kind = "\x50\x52\x4f\x50\x45\x52\x54\x59\x20\x2d\x20\x43\x4f\x4d\x50\x55\x54\x45\x44";
        break;
      case ObjectLiteral::Property::MATERIALIZED_LITERAL:
        prop_kind = "\x50\x52\x4f\x50\x45\x52\x54\x59\x20\x2d\x20\x4d\x41\x54\x45\x52\x49\x41\x4c\x49\x5a\x45\x44\x5f\x4c\x49\x54\x45\x52\x41\x4c";
        break;
      case ObjectLiteral::Property::PROTOTYPE:
        prop_kind = "\x50\x52\x4f\x50\x45\x52\x54\x59\x20\x2d\x20\x50\x52\x4f\x54\x4f\x54\x59\x50\x45";
        break;
      case ObjectLiteral::Property::GETTER:
        prop_kind = "\x50\x52\x4f\x50\x45\x52\x54\x59\x20\x2d\x20\x47\x45\x54\x54\x45\x52";
        break;
      case ObjectLiteral::Property::SETTER:
        prop_kind = "\x50\x52\x4f\x50\x45\x52\x54\x59\x20\x2d\x20\x53\x45\x54\x54\x45\x52";
        break;
      default:
        UNREACHABLE();
    }
    IndentedScope prop(this, prop_kind);
    PrintIndentedVisit("\x4b\x45\x59", node->properties()->at(i)->key());
    PrintIndentedVisit("\x56\x41\x4c\x55\x45", node->properties()->at(i)->value());
  }
}


void AstPrinter::VisitArrayLiteral(ArrayLiteral* node) {
  IndentedScope indent(this, "\x41\x52\x52\x41\x59\x20\x4c\x49\x54\x45\x52\x41\x4c");
  if (node->values()->length() > 0) {
    IndentedScope indent(this, "\x56\x41\x4c\x55\x45\x53");
    for (int i = 0; i < node->values()->length(); i++) {
      Visit(node->values()->at(i));
    }
  }
}


// TODO(svenpanne) Start with IndentedScope.
void AstPrinter::VisitVariableProxy(VariableProxy* node) {
  Variable* var = node->var();
  EmbeddedVector<char, 128> buf;
  int pos = SNPrintF(buf, "\x56\x41\x52\x20\x50\x52\x4f\x58\x59");
  switch (var->location()) {
    case Variable::UNALLOCATED:
      break;
    case Variable::PARAMETER:
      SNPrintF(buf + pos, "\x20\x70\x61\x72\x61\x6d\x65\x74\x65\x72\x5b\x6c\x84\x5d", var->index());
      break;
    case Variable::LOCAL:
      SNPrintF(buf + pos, "\x20\x6c\x6f\x63\x61\x6c\x5b\x6c\x84\x5d", var->index());
      break;
    case Variable::CONTEXT:
      SNPrintF(buf + pos, "\x20\x63\x6f\x6e\x74\x65\x78\x74\x5b\x6c\x84\x5d", var->index());
      break;
    case Variable::LOOKUP:
      SNPrintF(buf + pos, "\x20\x6c\x6f\x6f\x6b\x75\x70");
      break;
  }
  PrintLiteralWithModeIndented(buf.start(), var, node->name());
}


void AstPrinter::VisitAssignment(Assignment* node) {
  IndentedScope indent(this, Token::Name(node->op()));
  Visit(node->target());
  Visit(node->value());
}


void AstPrinter::VisitYield(Yield* node) {
  IndentedScope indent(this, "\x59\x49\x45\x4c\x44");
  Visit(node->expression());
}


void AstPrinter::VisitThrow(Throw* node) {
  IndentedScope indent(this, "\x54\x48\x52\x4f\x57");
  Visit(node->exception());
}


void AstPrinter::VisitProperty(Property* node) {
  IndentedScope indent(this, "\x50\x52\x4f\x50\x45\x52\x54\x59");
  Visit(node->obj());
  Literal* literal = node->key()->AsLiteral();
  if (literal != NULL && literal->value()->IsInternalizedString()) {
    PrintLiteralIndented("\x4e\x41\x4d\x45", literal->value(), false);
  } else {
    PrintIndentedVisit("\x4b\x45\x59", node->key());
  }
}


void AstPrinter::VisitCall(Call* node) {
  IndentedScope indent(this, "\x43\x41\x4c\x4c");
  Visit(node->expression());
  PrintArguments(node->arguments());
}


void AstPrinter::VisitCallNew(CallNew* node) {
  IndentedScope indent(this, "\x43\x41\x4c\x4c\x20\x4e\x45\x57");
  Visit(node->expression());
  PrintArguments(node->arguments());
}


void AstPrinter::VisitCallRuntime(CallRuntime* node) {
  IndentedScope indent(this, "\x43\x41\x4c\x4c\x20\x52\x55\x4e\x54\x49\x4d\x45");
  PrintLiteralIndented("\x4e\x41\x4d\x45", node->name(), false);
  PrintArguments(node->arguments());
}


void AstPrinter::VisitUnaryOperation(UnaryOperation* node) {
  IndentedScope indent(this, Token::Name(node->op()));
  Visit(node->expression());
}


void AstPrinter::VisitCountOperation(CountOperation* node) {
  EmbeddedVector<char, 128> buf;
  SNPrintF(buf, "\x6c\xa2\x20\x6c\xa2", (node->is_prefix() ? "\x50\x52\x45" : "\x50\x4f\x53\x54"),
           Token::Name(node->op()));
  IndentedScope indent(this, buf.start());
  Visit(node->expression());
}


void AstPrinter::VisitBinaryOperation(BinaryOperation* node) {
  IndentedScope indent(this, Token::Name(node->op()));
  Visit(node->left());
  Visit(node->right());
}


void AstPrinter::VisitCompareOperation(CompareOperation* node) {
  IndentedScope indent(this, Token::Name(node->op()));
  Visit(node->left());
  Visit(node->right());
}


void AstPrinter::VisitThisFunction(ThisFunction* node) {
  IndentedScope indent(this, "\x54\x48\x49\x53\x2d\x46\x55\x4e\x43\x54\x49\x4f\x4e");
}

#endif  // DEBUG

} }  // namespace v8::internal
