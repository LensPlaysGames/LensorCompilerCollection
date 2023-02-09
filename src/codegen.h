#ifndef CODEGEN_H
#define CODEGEN_H

#include <codegen/codegen_forward.h>

#include <ast.h>
#include <error.h>
#include <parser.h>
#include <stdio.h>
#include <vector.h>

extern bool debug_ir;
extern bool codegen_only;

typedef Vector(IRInstruction *) InstructionVector;

CodegenContext *codegen_context_create
(AST *ast,
 enum CodegenOutputFormat format,
 enum CodegenCallingConvention call_convention,
 enum CodegenAssemblyDialect dialect,
 FILE* code);

void codegen_context_free(CodegenContext *context);

struct Register {
  /// If non-zero, this register is in use.
  char in_use;
  /// Identifies a register uniquely.
  RegisterDescriptor descriptor;
};

/// Architecture-specific register information.
// TODO: Can probably just get rid of this
struct RegisterPool {
  Register *registers;
  Register **scratch_registers;
  usz num_scratch_registers;
  usz num_registers;
};

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
  AST *ast;

  Vector(IRFunction *) functions;
  Vector(IRStaticVariable *) static_vars;
  Vector(IRInstruction*) removed_instructions;
  IRFunction *function;
  IRFunction *entry;
  IRBlock *block;

  RegisterPool register_pool;
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
