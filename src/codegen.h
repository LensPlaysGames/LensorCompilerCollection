#ifndef COMPILER_CODEGEN_H
#define COMPILER_CODEGEN_H

#include <error.h>
#include <parser.h>

typedef enum CodegenOutputFormat {
  OUTPUT_FMT_DEFAULT = 0,
  OUTPUT_FMT_x86_64_AT_T_ASM,
} CodegenOutputFormat;

Error codegen_program(CodegenOutputFormat, ParsingContext *context, Node *program);

#endif /* COMPILER_CODEGEN_H */
