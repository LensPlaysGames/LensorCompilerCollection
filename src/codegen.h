#ifndef CODEGEN_H
#define CODEGEN_H

#include <codegen/codegen_forward.h>
#include <codegen/generic_object.h>

#include <ast.h>
#include <error.h>
#include <parser.h>
#include <stdio.h>
#include <vector.h>

extern bool debug_ir;
extern bool codegen_only;
extern bool annotate_code;

typedef Vector(IRInstruction *) InstructionVector;

CodegenContext *codegen_context_create
(AST *ast,
 enum CodegenOutputFormat format,
 enum CodegenCallingConvention call_convention,
 enum CodegenAssemblyDialect dialect,
 FILE* code);

void codegen_context_free(CodegenContext *context);

typedef struct IRStaticVariable {
  string name;
  Type *type;
  Node *decl;
  InstructionVector references;
  /// When non-null, points to the IRInstruction of the initialised value.
  /// This *must* be one of:
  /// - IR_LIT_INTEGER
  /// - IR_LIT_STRING
  IRInstruction *init;
} IRStaticVariable;

struct CodegenContext {
  FILE *code;
  GenericObjectFile *object;
  MIRInstructionVector *mir;
  AST *ast;

  Vector(IRFunction *) functions;
  Vector(IRStaticVariable *) static_vars;
  Vector(IRInstruction*) removed_instructions;
  IRFunction *function;
  IRFunction *entry;
  IRBlock *block;

  enum CodegenOutputFormat format;
  enum CodegenCallingConvention call_convention;
  enum CodegenAssemblyDialect dialect;
};

// TODO/FIXME: Make this a parameter affectable by command line arguments.
extern char codegen_verbose;

NODISCARD bool codegen
(enum CodegenLanguage,
 enum CodegenOutputFormat,
 enum CodegenCallingConvention,
 enum CodegenAssemblyDialect,
 const char *infile,
 const char *outfile,
 AST *ast,
 string ir);

#endif /* CODEGEN_H */
