#ifndef CODEGEN_H
#define CODEGEN_H

#include <environment.h>
#include <error.h>
#include <parser.h>

typedef int RegisterDescriptor;

typedef struct Register {
  /// What will be emitted when referencing this register, i.e "%rax"
  const char *name;
  /// If non-zero, this register is in use.
  char in_use;
  /// Identifies a register uniquely.
  RegisterDescriptor descriptor;
} Register;

/// Architecture-specific register information.
typedef struct RegisterPool {
  Register *regs;
  size_t num_scratch_regs;
  size_t num_regs;
} RegisterPool;

char *label_generate();

typedef struct CodegenContext {
  struct CodegenContext *parent;
  /// LOCALS
  /// `-- SYMBOL (NAME) -> INTEGER (STACK OFFSET)
  Environment *locals;
  RegisterPool registers;
  long long locals_offset;
} CodegenContext;

enum CodegenOutputFormat {
  CG_FMT_x86_64_MSWIN,
  CG_FMT_DEFAULT = CG_FMT_x86_64_MSWIN,
};

Error codegen_program(enum CodegenOutputFormat, char *output_filepath, ParsingContext *context, Node *program);

#endif /* CODEGEN_H */
