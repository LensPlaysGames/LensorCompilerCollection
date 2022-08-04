#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

long file_size(FILE *file) {
  if (!file) { return 0; }
  fpos_t original = 0;
  if (fgetpos(file, &original) != 0) {
    printf("fgetpos() failed: %i\n", errno);
    return 0;
  }
  fseek(file, 0, SEEK_END);
  long out = ftell(file);
  if (fsetpos(file, &original) != 0) {
    printf("fsetpos() failed: %i\n", errno);
  }
  return out;
}

char *file_contents(char *path) {
  FILE *file = fopen(path, "r");
  if (!file) {
    printf("Could not open file at %s\n", path);
    return NULL;
  }
  long size = file_size(file);
  char *contents = malloc(size + 1);
  assert(contents && "Could not allocate buffer for file contents");
  char *write_it = contents;
  size_t bytes_read = 0;
  while (bytes_read < size) {
    size_t bytes_read_this_iteration = fread(write_it, 1, size - bytes_read, file);
    if (ferror(file)) {
      printf("Error while reading: %i\n", errno);
      free(contents);
      return NULL;
    }

    bytes_read += bytes_read_this_iteration;
    write_it += bytes_read_this_iteration;

    if (feof(file)) {
      break;
    }
  }
  contents[bytes_read] = '\0';
  return contents;
}

void print_usage(char **argv) {
  printf("USAGE: %s <path_to_file_to_compile>\n", argv[0]);
}

// TODO: Add file path, byte offset, etc.
typedef struct Error {
  enum ErrorType {
    ERROR_NONE = 0,
    ERROR_ARGUMENTS,
    ERROR_TYPE,
    ERROR_GENERIC,
    ERROR_SYNTAX,
    ERROR_TODO,
    ERROR_MAX,
  } type;
  char *msg;
} Error;

Error ok = { ERROR_NONE, NULL };

void print_error(Error err) {
  if (err.type == ERROR_NONE) {
    return;
  }
  printf("ERROR: ");
  assert(ERROR_MAX == 6);
  switch (err.type) {
  default:
    printf("Unkown error type...");
    break;
  case ERROR_TODO:
    printf("TODO (not implemented)");
    break;
  case ERROR_SYNTAX:
    printf("Invalid syntax");
    break;
  case ERROR_TYPE:
    printf("Mismatched types");
    break;
  case ERROR_ARGUMENTS:
    printf("Invalid arguments");
    break;
  case ERROR_GENERIC:
    break;
  case ERROR_NONE:
    break;
  }
  putchar('\n');
  if (err.msg) {
    printf("     : %s\n", err.msg);
  }
}

#define ERROR_CREATE(n, t, msg)                 \
  Error (n) = { (t), (msg) }

#define ERROR_PREP(n, t, message)               \
  (n).type = (t);                               \
  (n).msg = (message);

const char *whitespace = " \r\n";
const char *delimiters = " \r\n,():";

typedef struct Token {
  char *beginning;
  char *end;
} Token;

void print_token(Token t) {
  printf("%.*s", t.end - t.beginning, t.beginning);
}

/// Lex the next token from SOURCE, and point to it with BEG and END.
Error lex(char *source, Token *token) {
  Error err = ok;
  if (!source || !token) {
    ERROR_PREP(err, ERROR_ARGUMENTS, "Can not lex empty source.");
    return err;
  }
  token->beginning = source;
  token->beginning += strspn(token->beginning, whitespace);
  token->end = token->beginning;
  if (*(token->end) == '\0') { return err; }
  token->end += strcspn(token->beginning, delimiters);
  if (token->end == token->beginning) {
    token->end += 1;
  }
  return err;
}

//      Node-
//     /  |  \
//    0   1   2
//   / \
//  3   4
//
// Node
// `-- 0  ->  1  ->  2
//     `-- 3  ->  4

// A : integer = 420
//
// PROGRAM
// `-- VARIABLE_DECLARATION_INITIALIZED
//     `-- INTEGER (420) -> SYMBOL (A)

// TODO:
// |-- API to create new node.
// `-- API to add node as child.
typedef struct Node {
  // TODO: Think about how to document node types and how they fit in the AST.
  enum NodeType {
    // BEGIN LITERALS

    /// The definition of nothing; false, etc.
    NODE_TYPE_NONE,

    /// Just an integer.
    NODE_TYPE_INTEGER,

    /// When a literal is expected but no other literal is valid, it
    /// becomes a symbol.
    NODE_TYPE_SYMBOL,

    // END LITERALS

    /// Contains two children. The first determines type (and value),
    /// while the second contains the symbolic name of the variable.
    NODE_TYPE_VARIABLE_DECLARATION,
    NODE_TYPE_VARIABLE_DECLARATION_INITIALIZED,

    /// Contains two children that determine left and right acceptable
    /// types.
    NODE_TYPE_BINARY_OPERATOR,

    /// Contains a list of expressions to execute in sequence.
    NODE_TYPE_PROGRAM,

    NODE_TYPE_MAX,
  } type;
  union NodeValue {
    long long integer;
    char *symbol;
  } value;
  // Possible TODO: Parent?
  struct Node *children;
  struct Node *next_child;
} Node;

Node *node_allocate() {
  Node *node = calloc(1,sizeof(Node));
  assert(node && "Could not allocate memory for AST node");
  return node;
}

#define nonep(node)    ((node).type == NODE_TYPE_NONE)
#define integerp(node) ((node).type == NODE_TYPE_INTEGER)
#define symbolp(node)  ((node).type == NODE_TYPE_SYMBOL)

/// PARENT is modified, NEW_CHILD pointer is used verbatim.
void node_add_child(Node *parent, Node *new_child) {
  if (!parent || !new_child) { return; }
  if (parent->children) {
    Node *child = parent->children;
    while (child->next_child) {
      child = child->next_child;
    }
    child->next_child = new_child;
  } else {
    parent->children = new_child;
  }
}

/// @return Boolean-like value; 1 for success, 0 for failure.
int node_compare(Node *a, Node *b) {
  if (!a || !b) {
    if (!a && !b) {
      return 1;
    }
    return 0;
  }
  // TODO: This assert doesn't work, I don't know why :^(.
  assert(NODE_TYPE_MAX == 7 && "node_compare() must handle all node types");
  if (a->type != b->type) { return 0; }
  switch (a->type) {
  case NODE_TYPE_NONE:
    if (nonep(*b)) {
      return 1;
    }
    return 0;
    break;
  case NODE_TYPE_INTEGER:
    if (a->value.integer == b->value.integer) {
      return 1;
    }
    return 0;
    break;
  case NODE_TYPE_SYMBOL:
    if (a->value.symbol && b->value.symbol) {
      if (strcmp(a->value.symbol, b->value.symbol) == 0) {
        return 1;
      }
      return 0;
    } else if (!a->value.symbol && !b->value.symbol) {
      return 1;
    }
    return 0;
    break;
  case NODE_TYPE_BINARY_OPERATOR:
    printf("TODO: node_compare() BINARY OPERATOR\n");
    break;
  case NODE_TYPE_VARIABLE_DECLARATION:
    printf("TODO: node_compare() VARIABLE DECLARATION\n");
  case NODE_TYPE_VARIABLE_DECLARATION_INITIALIZED:
    printf("TODO: node_compare() VARIABLE DECLARATION INITIALIZED\n");
  case NODE_TYPE_PROGRAM:
    // TODO: Compare two programs.
    printf("TODO: Compare two programs.\n");
    break;
  }
  return 0;
}

Node *node_integer(long long value) {
  Node *integer = node_allocate();
  integer->type = NODE_TYPE_INTEGER;
  integer->value.integer = value;
  integer->children = NULL;
  integer->next_child = NULL;
  return integer;
}

// TODO: Think about caching used symbols and not creating duplicates!
Node *node_symbol(char *symbol_string) {
  Node *symbol = node_allocate();
  symbol->type = NODE_TYPE_SYMBOL;
  symbol->value.symbol = strdup(symbol_string);
  return symbol;
}

Node *node_symbol_from_buffer(char *buffer, size_t length) {
  assert(buffer && "Can not create AST symbol node from NULL buffer");
  char *symbol_string = malloc(length + 1);
  assert(symbol_string && "Could not allocate memory for symbol string");
  memcpy(symbol_string, buffer, length);
  symbol_string[length] = '\0';
  Node *symbol = node_allocate();
  symbol->type = NODE_TYPE_SYMBOL;
  symbol->value.symbol = symbol_string;
  return symbol;
}

void print_node(Node *node, size_t indent_level) {
  if (!node) { return; }

  // Print indent.
  for (size_t i = 0; i < indent_level; ++i) {
    putchar(' ');
  }
  // Print type + value.
  assert(NODE_TYPE_MAX == 7 && "print_node() must handle all node types");
  switch (node->type) {
  default:
    printf("UNKNOWN");
    break;
  case NODE_TYPE_NONE:
    printf("NONE");
    break;
  case NODE_TYPE_INTEGER:
    printf("INT:%lld", node->value.integer);
    break;
  case NODE_TYPE_SYMBOL:
    printf("SYM");
    if (node->value.symbol) {
      printf(":%s", node->value.symbol);
    }
    break;
  case NODE_TYPE_BINARY_OPERATOR:
    printf("BINARY OPERATOR");
    break;
  case NODE_TYPE_VARIABLE_DECLARATION:
    printf("VARIABLE DECLARATION");
    break;
  case NODE_TYPE_VARIABLE_DECLARATION_INITIALIZED:
    printf("VARIABLE DECLARATION INITIALIZED");
    break;
  case NODE_TYPE_PROGRAM:
    printf("PROGRAM");
    break;
  }
  putchar('\n');
  // Print children.
  Node *child = node->children;
  while (child) {
    print_node(child, indent_level + 4);
    child = child->next_child;
  }
}

void node_free(Node *root) {
  if (!root) { return; }
  Node *child = root->children;
  Node *next_child = NULL;
  while (child) {
    next_child = child->next_child;
    node_free(child);
    child = next_child;
  }
  if (symbolp(*root) && root->value.symbol) {
    free(root->value.symbol);
  }
  free(root);
}

// TODO:
// |-- API to create new Binding.
// `-- API to add Binding to environment.
typedef struct Binding {
  Node *id;
  Node *value;
  struct Binding *next;
} Binding;

// TODO: API to create new Environment.
typedef struct Environment {
  struct Environment *parent;
  Binding *bind;
} Environment;

Environment *environment_create(Environment *parent) {
  Environment *env = malloc(sizeof(Environment));
  assert(env && "Could not allocate memory for new environment");
  env->parent = parent;
  env->bind = NULL;
  return env;
}

/**
 * @retval 0 Failure.
 * @retval 1 Creation of new binding.
 * @retval 2 Existing binding value overwrite (ID unused).
 */
int environment_set(Environment *env, Node *id, Node *value) {
  // Over-write existing value if ID is already bound in environment.
  if (!env || !id || !value) {
    return 0;
  }
  Binding *binding_it = env->bind;
  while (binding_it) {
    if (node_compare(binding_it->id, id)) {
      binding_it->value = value;
      return 2;
    }
    binding_it = binding_it->next;
  }
  // Create new binding.
  Binding *binding = malloc(sizeof(Binding));
  assert(binding && "Could not allocate new binding for environment");
  binding->id = id;
  binding->value = value;
  binding->next = env->bind;
  env->bind = binding;
  return 1;
}

/// @return Boolean-like value; 1 for success, 0 for failure.
int environment_get(Environment env, Node *id, Node *result) {
  Binding *binding_it = env.bind;
  while (binding_it) {
    if (node_compare(binding_it->id, id)) {
      *result = *binding_it->value;
      return 1;
    }
    binding_it = binding_it->next;
  }
  return 0;
}

int environment_get_by_symbol(Environment env, char *symbol, Node *result) {
  Node *symbol_node = node_symbol(symbol);
  int status = environment_get(env, symbol_node, result);
  free(symbol_node);
  return status;
}

// @return Boolean-like value; 1 for success, 0 for failure.
int token_string_equalp(char* string, Token *token) {
  if (!string || !token) { return 0; }
  char *beg = token->beginning;
  while (*string && token->beginning < token->end) {
    if (*string != *beg) {
      return 0;
    }
    string++;
    beg++;
  }
  return 1;
}

/// @return Boolean-like value; 1 upon success, 0 for failure.
int parse_integer(Token *token, Node *node) {
  if (!token || !node) { return 0; }
  char *end = NULL;
  if (token->end - token->beginning == 1 && *(token->beginning) == '0') {
    node->type = NODE_TYPE_INTEGER;
    node->value.integer = 0;
  } else if ((node->value.integer = strtoll(token->beginning, &end, 10)) != 0) {
    if (end != token->end) {
      return 0;
    }
    node->type = NODE_TYPE_INTEGER;
  } else { return 0; }
  return 1;
}

typedef struct ParsingContext {
  // FIXME: "struct ParsingContext *parent;" ???
  Environment *types;
  Environment *variables;
} ParsingContext;

ParsingContext *parse_context_create() {
  ParsingContext *ctx = calloc(1, sizeof(ParsingContext));
  assert(ctx && "Could not allocate memory for parsing context.");
  ctx->types = environment_create(NULL);
  if (environment_set(ctx->types, node_symbol("integer"), node_integer(0)) == 0) {
    printf("ERROR: Failed to set builtin type in types environment.\n");
  }
  ctx->variables = environment_create(NULL);
  return ctx;
}

Error parse_expr
(ParsingContext *context,
 char *source,
 char **end,
 Node *result
 )
{
  size_t token_count = 0;
  Token current_token;
  current_token.beginning  = source;
  current_token.end        = source;
  Error err = ok;

  while ((err = lex(current_token.end, &current_token)).type == ERROR_NONE) {
    *end = current_token.end;
    size_t token_length = current_token.end - current_token.beginning;
    if (token_length == 0) { break; }
    if (parse_integer(&current_token, result)) {
      // look ahead for binary ops that include integers.
      Node lhs_integer = *result;
      err = lex(current_token.end, &current_token);
      if (err.type != ERROR_NONE) {
        return err;
      }
      *end = current_token.end;

      // TODO: Check for valid integer operator.
      // It would be cool to use an operator environment to look up
      // operators instead of hard-coding them. This would eventually
      // allow for user-defined operators, or stuff like that!

    } else {
      // TODO: Check for unary prefix operators.

      // TODO: Check that it isn't a binary operator (we should encounter left
      // side first and peek forward, rather than encounter it at top level).

      Node *symbol = node_symbol_from_buffer(current_token.beginning, token_length);

      //*result = *symbol;

      // TODO: Check if valid symbol for variable environment, then
      // attempt to pattern match variable access, assignment,
      // declaration, or declaration with initialization.

      err = lex(current_token.end, &current_token);
      if (err.type != ERROR_NONE) { return err; }
      *end = current_token.end;
      size_t token_length = current_token.end - current_token.beginning;
      if (token_length == 0) { break; }

      if (token_string_equalp(":", &current_token)) {
        err = lex(current_token.end, &current_token);
        if (err.type != ERROR_NONE) {
          return err;
        }
        *end = current_token.end;
        size_t token_length = current_token.end - current_token.beginning;
        if (token_length == 0) { break; }

        Node *expected_type_symbol =
          node_symbol_from_buffer(current_token.beginning, token_length);
        int status = environment_get(*context->types, expected_type_symbol, result);
        if (status == 0) {
          ERROR_PREP(err, ERROR_TYPE, "Invalid type within variable declaration");
          printf("\nINVALID TYPE: \"%s\"\n", expected_type_symbol->value.symbol);
          return err;
        } else {
          //printf("Found valid type: ");
          //print_node(expected_type_symbol,0);
          //putchar('\n');

          Node *var_decl = node_allocate();
          var_decl->type = NODE_TYPE_VARIABLE_DECLARATION;

          Node *type_node = node_allocate();
          type_node->type = result->type;

          node_add_child(var_decl, type_node);
          node_add_child(var_decl, symbol);

          *result = *var_decl;
          // Node contents transfer ownership, var_decl is now hollow shell.
          free(var_decl);

          return ok;
        }
      }

      printf("Unrecognized token: ");
      print_token(current_token);
      putchar('\n');

      return err;
    }

    printf("Intermediate node: ");
    print_node(result, 0);
    putchar('\n');

  }

  return err;
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
    Error err = parse_expr(context, contents_it, &contents_it, expression);
    node_add_child(program, expression);

    print_error(err);

    print_node(program, 0);
    putchar('\n');

    node_free(program);
    free(contents);
  }

  return 0;
}
