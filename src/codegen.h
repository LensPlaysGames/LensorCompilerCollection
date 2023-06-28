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
 CodegenArchitecture,
 CodegenTarget,
 CodegenCallingConvention call_convention,
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
  /// The IR
  Vector(IRFunction *) functions;
  Vector(IRStaticVariable *) static_vars;
  Vector(IRInstruction*) removed_instructions;

  /// Code emission (targets)
  FILE *code;
  GenericObjectFile *object;

  /// (Used to construct IR)
  AST *ast;
  IRFunction *function;
  IRFunction *entry;
  IRBlock *block;

  /// Options
  CodegenArchitecture arch;
  // TODO: Allow for multiple targets.
  CodegenTarget target;
  CodegenCallingConvention call_convention;
};

// TODO/FIXME: Make this a parameter affectable by command line arguments.
extern char codegen_verbose;

NODISCARD bool codegen
(CodegenLanguage,
 CodegenArchitecture arch,
 CodegenTarget target,
 CodegenCallingConvention,
 const char *infile,
 const char *outfile,
 AST *ast,
 string ir);

/// Mangle a function name.
void mangle_function_name(IRFunction *function);

#endif /* CODEGEN_H */
