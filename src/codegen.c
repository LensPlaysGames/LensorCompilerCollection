#include <codegen.h>

#include <error.h>
#include <environment.h>
#include <parser.h>
#include <stdio.h>

Error codegen_program_x86_64_att_asm(ParsingContext *context, Node *program) {
  Error err = ok;
  if (!program || program->type != NODE_TYPE_PROGRAM) {
    ERROR_PREP(err, ERROR_ARGUMENTS, "codegen_program() requires a program!");
    return err;
  }

  FILE *code = fopen("code.S", "wb");
  if (!code) {
    ERROR_PREP(err, ERROR_GENERIC, "codegen_program() could not open code file");
    return err;
  }

  // TODO: Generate code :^>

  // TODO: Allocate `.space` for all global variable declarations.

  Node *expression = program->children;
  Node *tmpnode1 = node_allocate();
  while (expression) {
    switch (expression->type) {
    default:
      break;
    case NODE_TYPE_VARIABLE_DECLARATION:
      // TODO: Get size of type, generate `<identifier>: .space <size>`.
      //       Keep track of identifer somehow.

      environment_get(*context->variables, expression->children, tmpnode1);
      environment_get(*context->types, tmpnode1, tmpnode1);
      print_node(tmpnode1,0);
      // TODO: Handle nested scopes (stack based variables)
      break;
    }

    expression = expression->next_child;
  }

  fclose(code);
  return ok;
}

Error codegen_program(CodegenOutputFormat format, ParsingContext *context, Node *program) {
  if (!context) {
    ERROR_CREATE(err, ERROR_ARGUMENTS, "codegen_program() must be passed a non-NULL context.");
    return err;
  }
  switch (format) {
  case OUTPUT_FMT_DEFAULT:
  case OUTPUT_FMT_x86_64_AT_T_ASM:
    return codegen_program_x86_64_att_asm(context, program);
  }
  return ok;
}
