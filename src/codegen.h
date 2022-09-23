#ifndef CODEGEN_H
#define CODEGEN_H

#include <codegen/codegen_forward.h>

#include <environment.h>
#include <error.h>
#include <parser.h>
#include <stdio.h>

struct CodegenContext {
  CodegenContext *parent;
  /// LOCALS
  /// `-- SYMBOL (NAME) -> INTEGER (STACK OFFSET)
  FILE* code;
  Environment *locals;
  long long locals_base_offset;
  enum CodegenOutputFormat format;
  enum CodegenCallingConvention call_convention;
  enum CodegenAssemblyDialect dialect;
  BasicBlock *insert_point;
  Function *functions;
  Function *current_function;
  /// For generating unique function names
  size_t *func_count;
  /// For generating unique labels
  size_t *block_count;
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
