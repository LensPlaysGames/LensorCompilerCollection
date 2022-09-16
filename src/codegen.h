#ifndef CODEGEN_H
#define CODEGEN_H

#include <environment.h>
#include <error.h>
#include <parser.h>
#include <stdio.h>

typedef int RegisterDescriptor;

typedef struct Register {
  /// If non-zero, this register is in use.
  char in_use;
  /// Identifies a register uniquely.
  RegisterDescriptor descriptor;
} Register;

/// Architecture-specific register information.
typedef struct RegisterPool {
  Register *registers;
  Register **scratch_registers;
  size_t num_scratch_registers;
  size_t num_registers;
} RegisterPool;

enum CodegenOutputFormat {
  CG_FMT_x86_64_GAS,
  CG_FMT_COUNT,

  CG_FMT_DEFAULT = CG_FMT_x86_64_GAS,
};

enum CodegenCallingConvention {
  CG_CALL_CONV_MSWIN,
  CG_CALL_CONV_LINUX,
  CG_CALL_CONV_COUNT,

  CG_CALL_CONV_DEFAULT = CG_CALL_CONV_MSWIN,
};

typedef struct CodegenContext {
  struct CodegenContext *parent;
  /// LOCALS
  /// `-- SYMBOL (NAME) -> INTEGER (STACK OFFSET)
  FILE* code;
  Environment *locals;
  long long locals_offset;
  RegisterPool register_pool;
  enum CodegenOutputFormat format;
  enum CodegenCallingConvention call_convention;
} CodegenContext;

// TODO/FIXME: Make this a parameter affectable by command line arguments.
extern char codegen_verbose;

Error codegen_program
(enum CodegenOutputFormat,
 enum CodegenCallingConvention,
 char *output_filepath,
 ParsingContext *context,
 Node *program);


void print_registers(CodegenContext *cg_ctx);

char register_descriptor_is_valid(CodegenContext *cg_ctx, RegisterDescriptor descriptor);

RegisterDescriptor register_allocate(CodegenContext *cg_ctx);

void register_deallocate
(CodegenContext *cg_ctx, RegisterDescriptor descriptor);

// Types of comparison to be implemented by codegen backend.
enum ComparisonType {
  COMPARE_EQ,
  COMPARE_NE,
  COMPARE_LT,
  COMPARE_LE,
  COMPARE_GT,
  COMPARE_GE,

  COMPARE_COUNT,
};

extern const char *comparison_suffixes_x86_64[COMPARE_COUNT];

#endif /* CODEGEN_H */
