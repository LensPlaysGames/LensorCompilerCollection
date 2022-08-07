#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include <error.h>
#include <file_io.h>
#include <environment.h>
#include <parser.h>

void print_usage(char **argv) {
  printf("USAGE: %s <path_to_file_to_compile>\n", argv[0]);
}

int main(int argc, char **argv) {
  if (argc < 2) {
    print_usage(argv);
    exit(0);
  }

  char *path = argv[1];
  char *contents = file_contents(path);

  if (contents) {
    //printf("Contents of %s:\n---\n\"%s\"\n---\n", path, contents);

    // TODO: Create API to heap allocate a program node, as well as add
    // expressions as children.
    ParsingContext *context = parse_context_create();
    Node *program = node_allocate();
    program->type = NODE_TYPE_PROGRAM;
    Node *expression = node_allocate();
    memset(expression,0,sizeof(Node));
    char *contents_it = contents;
    for (;;) {

      Error err = parse_expr(context, contents_it, &contents_it, expression);
      //printf("contents_it: \"%s\"\n", contents_it);

      if (!(*contents_it)) {
        break;
      }
      if (err.type != ERROR_NONE) {
        print_error(err);
        break;
      }
      node_add_child(program, expression);
    }

    print_node(program, 0);
    putchar('\n');

    node_free(program);
    free(contents);
  }

  return 0;
}
