#ifndef CODEGEN_H
#define CODEGEN_H

#include <codegen/codegen_forward.h>

#include <environment.h>
#include <error.h>
#include <parser.h>
#include <stdio.h>

CodegenContext *codegen_context_create_top_level
(ParsingContext *parse_context,
 enum CodegenOutputFormat format,
 enum CodegenCallingConvention call_convention,
 enum CodegenAssemblyDialect dialect,
 FILE* code);

CodegenContext *codegen_context_create(CodegenContext *parent);
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
  size_t num_scratch_registers;
  size_t num_registers;
};

struct CodegenContext {
  CodegenContext *parent;
  ParsingContext *parse_context;
  FILE* code;

  IRFunction *all_functions;
  IRFunction *function;
  IRBlock *block;

  /// LOCALS
  /// `-- SYMBOL (NAME) -> INTEGER (STACK OFFSET)
  Environment *locals;

  long long locals_offset;
  RegisterPool register_pool;
  enum CodegenOutputFormat format;
  enum CodegenCallingConvention call_convention;
  enum CodegenAssemblyDialect dialect;
  /// Architecture-specific data.
  void *arch_data;
};

// TODO/FIXME: Make this a parameter affectable by command line arguments.
extern char codegen_verbose;

Error codegen
(enum CodegenOutputFormat,
 enum CodegenCallingConvention,
 enum CodegenAssemblyDialect,
 char *output_filepath,
 ParsingContext *context,
 Node *program);

#endif /* CODEGEN_H */
