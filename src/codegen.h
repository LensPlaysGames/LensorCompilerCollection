#ifndef CODEGEN_H
#define CODEGEN_H

#include "codegen/codegen_forward.h"

#include <environment.h>
#include <error.h>
#include <parser.h>
#include <stdio.h>

struct Register {
  /// If non-zero, this register is in use.
  char in_use;
  /// Identifies a register uniquely.
  RegisterDescriptor descriptor;
};

/// Architecture-specific register information.
struct RegisterPool {
  Register *registers;
  Register **scratch_registers;
  size_t num_scratch_registers;
  size_t num_registers;
};

struct CodegenContext {
  CodegenContext *parent;
  /// LOCALS
  /// `-- SYMBOL (NAME) -> INTEGER (STACK OFFSET)
  FILE* code;
  Environment *locals;
  long long locals_offset;
  RegisterPool register_pool;
  enum CodegenOutputFormat format;
  enum CodegenCallingConvention call_convention;

  /// Architecture-specific data.
  void *arch_data;
};

// TODO/FIXME: Make this a parameter affectable by command line arguments.
extern char codegen_verbose;

Error codegen
(enum CodegenOutputFormat,
 enum CodegenCallingConvention,
 char *output_filepath,
 ParsingContext *context,
 Node *program);


char register_descriptor_is_valid(CodegenContext *cg_ctx, RegisterDescriptor descriptor);

RegisterDescriptor register_allocate(CodegenContext *cg_ctx);

void register_deallocate
(CodegenContext *cg_ctx, RegisterDescriptor descriptor);

#endif /* CODEGEN_H */
