#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include <codegen.h>
#include <error.h>
#include <environment.h>
#include <file_io.h>
#include <parser.h>
#include <typechecker.h>

void print_usage(char **argv) {
  printf("USAGE: %s <path_to_file_to_compile>\n", argv[0]);
}

int main(int argc, char **argv) {
  if (argc < 2) {
    print_usage(argv);
    return 0;
  }

  Node *program = node_allocate();
  ParsingContext *context = parse_context_default_create();
  Error err = parse_program(argv[1], context, program);

  print_node(program, 0);
  putchar('\n');

  parse_context_print(context,0);

  if (err.type) {
    print_error(err);
    return 1;
  }

  err = typecheck_program(context, program);
  if (err.type) {
    print_error(err);
    return 2;
  }

  err = codegen_program(CG_FMT_DEFAULT, context, program);
  if (err.type) {
    print_error(err);
    return 3;
  }

  node_free(program);

  return 0;
}
