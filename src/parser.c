#include <parser.h>

#include <assert.h>
#include <error.h>
#include <environment.h>
#include <file_io.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

//================================================================ BEG lexer

// TODO: Allow multi-byte comment delimiters.
const char *comment_delimiters = ";#";
const char *whitespace         = " \r\n";
const char *delimiters         = " \r\n,():";

/// @return Boolean-like value: 1 for success, 0 for failure.
int comment_at_beginning(Token token) {
  const char *comment_it = comment_delimiters;
  while (*comment_it) {
    if (*(token.beginning) == *comment_it) {
      return 1;
    }
    comment_it++;
  }
  return 0;
}

/// Lex the next token from SOURCE, and point to it with BEG and END.
/// If BEG and END of token are equal, there is nothing more to lex.
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
  // Check if current line is a comment, and skip past it.
  while (comment_at_beginning(*token)) {
    // Skip to next newline.
    token->beginning = strpbrk(token->beginning, "\n");
    if (!token->beginning) {
      // If last line of file is comment, we're done lexing.
      token->end = token->beginning;
      return err;
    }
    // Skip to beginning of next token after comment.
    token->beginning += strspn(token->beginning, whitespace);
    token->end = token->beginning;
  }
  if (*(token->end) == '\0') { return err; }
  token->end += strcspn(token->beginning, delimiters);
  if (token->end == token->beginning) {
    token->end += 1;
  }
  return err;
}

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

void print_token(Token t) {
  if (t.end - t.beginning < 1) {
    printf("INVALID TOKEN POINTERS");
  } else {
    printf("%.*s", (int)(t.end - t.beginning), t.beginning);
  }
}

//================================================================ END lexer

Node *node_allocate() {
  Node *node = calloc(1,sizeof(Node));
  assert(node && "Could not allocate memory for AST node");
  return node;
}

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

int node_compare(Node *a, Node *b) {
  if (!a || !b) {
    if (!a && !b) {
      return 1;
    }
    return 0;
  }
  assert(NODE_TYPE_MAX == 10 && "node_compare() must handle all node types");
  if (a->type != b->type) { return 0; }
  switch (a->type) {
  case NODE_TYPE_NONE:
    if (nonep(*b)) {
      return 1;
    }
    break;
  case NODE_TYPE_INTEGER:
    if (a->value.integer == b->value.integer) {
      return 1;
    }
    break;
  case NODE_TYPE_SYMBOL:
    if (a->value.symbol && b->value.symbol) {
      if (strcmp(a->value.symbol, b->value.symbol) == 0) {
        return 1;
      }
      break;
    } else if (!a->value.symbol && !b->value.symbol) {
      return 1;
    }
    break;
  case NODE_TYPE_BINARY_OPERATOR:
    printf("TODO: node_compare() BINARY OPERATOR\n");
    break;
  case NODE_TYPE_FUNCTION:
    printf("TODO: node_compare() FUNCTION\n");
    break;
  case NODE_TYPE_FUNCTION_CALL:
    printf("TODO: node_compare() FUNCTION CALL\n");
    break;
  case NODE_TYPE_VARIABLE_REASSIGNMENT:
    printf("TODO: node_compare() VARIABLE REASSIGNMENT\n");
    break;
  case NODE_TYPE_VARIABLE_DECLARATION:
    printf("TODO: node_compare() VARIABLE DECLARATION\n");
    break;
  case NODE_TYPE_VARIABLE_DECLARATION_INITIALIZED:
    printf("TODO: node_compare() VARIABLE DECLARATION INITIALIZED\n");
    break;
  case NODE_TYPE_PROGRAM:
    printf("TODO: Compare two programs.\n");
    break;
  }
  return 0;
}

Node *node_none() {
  Node *none = node_allocate();
  none->type = NODE_TYPE_NONE;
  return none;
}

Node *node_integer(long long value) {
  Node *integer = node_allocate();
  integer->type = NODE_TYPE_INTEGER;
  integer->value.integer = value;
  return integer;
}

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

// Take ownership of type_symbol.
Error define_type(Environment *types, int type, Node *type_symbol, long long byte_size) {
  assert(types && "Can not add type to NULL types environment");
  assert(type_symbol && "Can not add NULL type symbol to types environment");
  assert(byte_size >= 0 && "Can not define new type with zero or negative byte size");

  Node *size_node = node_allocate();
  size_node->type = NODE_TYPE_INTEGER;
  size_node->value.integer = byte_size;

  Node *type_node = node_allocate();
  type_node->type = type;
  type_node->children = size_node;

  if(environment_set(types, type_symbol, type_node) == 1) {
    return ok;
  }
  // TYPE REDEFINITION ERROR
  printf("Type that was redefined: \"%s\"\n", type_symbol->value.symbol);
  ERROR_CREATE(err, ERROR_TYPE, "Redefinition of type!");
  return err;
}

void print_node(Node *node, size_t indent_level) {
  if (!node) { return; }

  // Print indent.
  for (size_t i = 0; i < indent_level; ++i) {
    putchar(' ');
  }
  // Print type + value.
  assert(NODE_TYPE_MAX == 10 && "print_node() must handle all node types");
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
  case NODE_TYPE_VARIABLE_REASSIGNMENT:
    printf("VARIABLE REASSIGNMENT");
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
  case NODE_TYPE_FUNCTION:
    printf("FUNCTION");
    break;
  case NODE_TYPE_FUNCTION_CALL:
    printf("FUNCTION CALL");
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

void node_copy(Node *a, Node *b) {
  if (!a || !b) { return; }
  b->type = a->type;
  // Handle all allocated values here.
  switch (a->type) {
  default:
    b->value = a->value;
    break;
  case NODE_TYPE_SYMBOL:
    b->value.symbol = strdup(a->value.symbol);
    assert(b->value.symbol && "node_copy(): Could not allocate memory for new symbol");
    break;
  }
  Node *child = a->children;
  Node *child_it = NULL;
  while (child) {
    Node *new_child = node_allocate();

    if (child_it) {
      child_it->next_child = new_child;
      child_it = child_it->next_child;
    } else {
      b->children = new_child;
      child_it = new_child;
    }

    node_copy(child, child_it);

    child = child->next_child;
  }
}

ParsingContext *parse_context_create(ParsingContext *parent) {
  ParsingContext *ctx = calloc(1, sizeof(ParsingContext));
  assert(ctx && "Could not allocate memory for parsing context.");
  ctx->parent = parent;
  ctx->operator = NULL;
  ctx->result = NULL;
  ctx->types = environment_create(NULL);
  ctx->variables = environment_create(NULL);
  ctx->functions = environment_create(NULL);
  return ctx;
}

ParsingContext *parse_context_default_create() {
  ParsingContext *ctx = parse_context_create(NULL);
  Error err = define_type(ctx->types,
                          NODE_TYPE_INTEGER,
                          node_symbol("integer"),
                          sizeof(long long));
  if (err.type != ERROR_NONE) {
    printf("ERROR: Failed to set builtin integer type in types environment.\n");
  }
  return ctx;
}

/// Update token, token length, and end of current token pointer.
Error lex_advance(Token *token, size_t *token_length, char **end) {
  if (!token || !token_length || !end) {
    ERROR_CREATE(err, ERROR_ARGUMENTS,
                 "lex_advance(): pointer arguments must not be NULL!");
    return err;
  }
  Error err = lex(token->end, token);
  *end = token->end;
  if (err.type != ERROR_NONE) { return err; }
  *token_length = token->end - token->beginning;
  return err;
}

typedef struct ExpectReturnValue {
  Error err;
  char found;
  char done;
} ExpectReturnValue;

ExpectReturnValue lex_expect
(char *expected,
 Token *current,
 size_t *current_length,
 char **end
 )
{
  ExpectReturnValue out;
  out.done = 0;
  out.found = 0;
  out.err = ok;
  if (!expected || !current || !current_length || !end) {
    ERROR_PREP(out.err, ERROR_ARGUMENTS,
               "lex_expect() must not be passed NULL pointers!");
    return out;
  }
  Token current_copy = *current;
  size_t current_length_copy = *current_length;
  char *end_value = *end;

  out.err = lex_advance(&current_copy, &current_length_copy, &end_value);
  if (out.err.type != ERROR_NONE) { return out; }
  if (current_length_copy == 0) {
    out.done = 1;
    return out;
  }

  //printf("Expecting \"%s\", got \"", expected);
  //print_token(current_copy);
  //printf("\"\n");

  if (token_string_equalp(expected, &current_copy)) {
    out.found = 1;
    *end = end_value;
    *current = current_copy;
    *current_length = current_length_copy;
  }

  return out;
}

Error parse_get_type(ParsingContext *context, Node *id, Node *result) {
  Error err = ok;
  while (context) {
    int status = environment_get(*context->types, id, result);
    if (status) { return ok; }
    context = context->parent;
  }
  result->type = NODE_TYPE_NONE;
  ERROR_PREP(err, ERROR_GENERIC, "Type is not found in environment.");
  return err;
}

#define EXPECT(expected, expected_string, current_token, current_length, end)   \
  expected = lex_expect(expected_string, &current_token, &current_length, end); \
  if (expected.err.type) { return expected.err; }                               \
  if (expected.done) { return ok; }

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

Error parse_expr
(ParsingContext *context,
 char *source,
 char **end,
 Node *result
 )
{
  ExpectReturnValue expected;
  size_t token_length = 0;
  Token current_token;
  current_token.beginning  = source;
  current_token.end        = source;
  Error err = ok;

  Node *working_result = result;

  while ((err = lex_advance(&current_token, &token_length, end)).type == ERROR_NONE) {
    //printf("lexed: "); print_token(current_token); putchar('\n');
    if (token_length == 0) { return ok; }

    if (parse_integer(&current_token, working_result)) {

      // TODO: Look ahead for binary ops that include integers.
      // It would be cool to use an operator environment to look up
      // operators instead of hard-coding them. This would eventually
      // allow for user-defined operators, or stuff like that!

    } else {

      Node *symbol = node_symbol_from_buffer(current_token.beginning, token_length);

      // TODO: Parse strings and other literal types.

      // TODO: Check for unary prefix operators.

      // TODO: Check that it isn't a binary operator (we should encounter left
      // side first and peek forward, rather than encounter it at top level).

      if (strcmp("defun", symbol->value.symbol) == 0) {
        // Begin function definition.
        // FUNCTION
        //   LIST of parameters
        //     PARAMETER
        //       SYMBOL:NAME
        //       SYMBOL:TYPE
        //   RETURN TYPE SYMBOL
        //   PROGRAM/LIST of expressions
        //     ...

        working_result->type = NODE_TYPE_FUNCTION;

        lex_advance(&current_token, &token_length, end);
        Node *function_name = node_symbol_from_buffer(current_token.beginning, token_length);

        EXPECT(expected, "(", current_token, token_length, end);
        if (!expected.found) {
          printf("Function Name: \"%s\"\n", function_name->value.symbol);
          ERROR_PREP(err, ERROR_SYNTAX,
                     "Expected opening parenthesis for parameter list after function name");
          return err;
        }

        Node *parameter_list = node_allocate();
        node_add_child(working_result, parameter_list);

        // FIXME?: Should we possibly create a parser stack and evaluate the
        // next expression, then ensure return value is var. decl. in stack
        // handling below?
        for (;;) {
          EXPECT(expected, ")", current_token, token_length, end);
          if (expected.found) { break; }
          if (expected.done) {
            ERROR_PREP(err, ERROR_SYNTAX,
                       "Expected closing parenthesis for parameter list");
            return err;
          }

          err = lex_advance(&current_token, &token_length, end);
          if (err.type) { return err; }
          Node *parameter_name = node_symbol_from_buffer(current_token.beginning, token_length);

          EXPECT(expected, ":", current_token, token_length, end);
          if (expected.done || !expected.found) {
            ERROR_PREP(err, ERROR_SYNTAX, "Parameter declaration requires a type annotation");
            return err;
          }

          lex_advance(&current_token, &token_length, end);
          Node *parameter_type = node_symbol_from_buffer(current_token.beginning, token_length);

          Node *parameter = node_allocate();
          node_add_child(parameter, parameter_name);
          node_add_child(parameter, parameter_type);

          node_add_child(parameter_list, parameter);

          EXPECT(expected, ",", current_token, token_length, end);
          if (expected.found) { continue; }

          EXPECT (expected, ")", current_token, token_length, end);
          if (!expected.found) {
            ERROR_PREP(err, ERROR_SYNTAX, "Expected closing parenthesis following parameter list");
            return err;
          }
          break;

        }

        // Parse return type.
        EXPECT(expected, ":", current_token, token_length, end);
        // TODO/FIXME: Should we allow implicit return type?
        if (expected.done || !expected.found) {
          ERROR_PREP(err, ERROR_SYNTAX,
                     "Function definition requires return type annotation following parameter list");
          return err;
        }

        lex_advance(&current_token, &token_length, end);
        Node *function_return_type = node_symbol_from_buffer(current_token.beginning, token_length);
        node_add_child(working_result, function_return_type);

        // Bind function to function name in functions environment.
        environment_set(context->functions, function_name, working_result);

        // Parse function body.
        EXPECT(expected, "{", current_token, token_length, end);
        if (expected.done || !expected.found) {
          ERROR_PREP(err, ERROR_SYNTAX, "Function definition requires body following return type: \"{ a + b }\"");
          return err;
        }

        context = parse_context_create(context);
        context->operator = node_symbol("defun");

        Node *param_it = working_result->children->children;
        while (param_it) {
          environment_set(context->variables,
                          param_it->children,
                          param_it->children->next_child);
          param_it = param_it->next_child;
        }

        Node *function_body = node_allocate();
        Node *function_first_expression = node_allocate();
        node_add_child(function_body, function_first_expression);
        node_add_child(working_result, function_body);
        working_result = function_first_expression;
        context->result = working_result;
        continue;

      } else {

        // TODO: Check if valid symbol for variable environment, then
        // attempt to pattern match variable access, assignment,
        // declaration, or declaration with initialization.

        EXPECT(expected, ":", current_token, token_length, end);
        if (expected.found) {

          // Re-assignment of existing variable (look for =)
          EXPECT(expected, "=", current_token, token_length, end);
          if (expected.found) {
            Node *variable_binding = node_allocate();
            if (!environment_get(*context->variables, symbol, variable_binding)) {
              // TODO: Add source location or something to the error.
              // TODO: Create new error type.
              printf("ID of undeclared variable: \"%s\"\n", symbol->value.symbol);
              ERROR_PREP(err, ERROR_GENERIC, "Reassignment of a variable that has not been declared!");
              return err;
            }
            free(variable_binding);

            working_result->type = NODE_TYPE_VARIABLE_REASSIGNMENT;
            node_add_child(working_result, symbol);
            Node *reassign_expr = node_allocate();
            node_add_child(working_result, reassign_expr);

            working_result = reassign_expr;
            continue;
          }

          err = lex_advance(&current_token, &token_length, end);
          if (err.type != ERROR_NONE) { return err; }
          if (token_length == 0) { break; }
          Node *type_symbol =
            node_symbol_from_buffer(current_token.beginning, token_length);
          Node *type_value = node_allocate();
          parse_get_type(context, type_symbol, type_value);
          if (nonep(*type_value)) {
            ERROR_PREP(err, ERROR_TYPE, "Invalid type within variable declaration");
            printf("\nINVALID TYPE: \"%s\"\n", type_symbol->value.symbol);
            return err;
          }
          free(type_value);

          Node *variable_binding = node_allocate();
          if (environment_get(*context->variables, symbol, variable_binding)) {
            // TODO: Create new error type.
            printf("ID of redefined variable: \"%s\"\n", symbol->value.symbol);
            ERROR_PREP(err, ERROR_GENERIC, "Redefinition of variable!");
            return err;
          }
          // Variable binding is shell-node for environment value contents.
          free(variable_binding);

          working_result->type = NODE_TYPE_VARIABLE_DECLARATION;

          Node *value_expression = node_none();

          // `symbol` is now owned by working_result, a var. decl.
          node_add_child(working_result, symbol);
          node_add_child(working_result, value_expression);

          // Context variables environment gains new binding.
          Node *symbol_for_env = node_allocate();
          node_copy(symbol, symbol_for_env);
          int status = environment_set(context->variables, symbol_for_env, type_symbol);
          if (status != 1) {
            printf("Variable: \"%s\", status: %d\n", symbol_for_env->value.symbol, status);
            ERROR_PREP(err, ERROR_GENERIC, "Failed to define variable!");
            return err;
          }

          EXPECT(expected, "=", current_token, token_length, end);
          if (expected.found) {
            working_result = value_expression;
            continue;
          }

          return ok;
        } else {
          // Symbol is not `defun` and it is not followed by an assignment operator `:`.

          // Check if it's a function call (lookahead for symbol)
          EXPECT(expected, "(", current_token, token_length, end);
          if (expected.found) {
            working_result->type = NODE_TYPE_FUNCTION_CALL;
            node_add_child(working_result, symbol);
            Node *argument_list = node_allocate();
            Node *first_argument = node_allocate();
            node_add_child(argument_list, first_argument);
            node_add_child(working_result, argument_list);
            working_result = first_argument;
            // Create a parsing stack with function call operator IG,
            // and then start parsing function argument expressions.

            context = parse_context_create(context);
            context->operator = node_symbol("funcall");
            context->result = working_result;

            continue;

          } else {
            // TODO: Check if it's a variable access (defined variable)
          }
        }

        printf("Unrecognized token: ");
        print_token(current_token);
        putchar('\n');

        ERROR_PREP(err, ERROR_SYNTAX, "Unrecognized token reached during parsing");
        return err;

      }

    }

    if (!context->parent) {
      break;
    }

    Node *operator = context->operator;
    if (operator->type != NODE_TYPE_SYMBOL) {
      ERROR_PREP(err, ERROR_TYPE,
                 "Parsing context operator must be symbol. Likely internal error :(");
      return err;
    }

    if (strcmp(operator->value.symbol, "defun") == 0) {
      // Evaluate next expression unless it's a closing brace.
      EXPECT(expected, "}", current_token, token_length, end);
      if (expected.done || expected.found) {
        // TODO: Should we pop parser context here?
        break;
      }

      context->result->next_child = node_allocate();
      working_result = context->result->next_child;
      context->result = working_result;
      continue;
    }
    if (strcmp(operator->value.symbol, "funcall") == 0) {
      EXPECT(expected, ")", current_token, token_length, end);
      if (expected.done || expected.found) { break; }

      // FIXME?: Should comma be optional?
      EXPECT(expected, ",", current_token, token_length, end);
      if (expected.done || !expected.found) {
        print_token(current_token);
        ERROR_PREP(err, ERROR_SYNTAX,
                   "Parameter list expected closing parenthesis or comma for another parameter");
        return err;
      }

      context->result->next_child = node_allocate();
      working_result = context->result->next_child;
      context->result = working_result;

      continue;
    }
  }

  return err;
}


Error parse_program(char *filepath, ParsingContext *context, Node *result) {
  Error err = ok;
  char *contents = file_contents(filepath);
  if (!contents) {
    printf("Filepath: \"%s\"\n", filepath);
    ERROR_PREP(err, ERROR_GENERIC, "parse_program(): Couldn't get file contents");
    return err;
  }
  result->type = NODE_TYPE_PROGRAM;
  char *contents_it = contents;
  for (;;) {
    Node *expression = node_allocate();
    node_add_child(result, expression);
    err = parse_expr(context, contents_it, &contents_it, expression);
    if (err.type != ERROR_NONE) {
      free(contents);
      return err;
    }
    // Check for end-of-parsing case (source and end are the same).
    if (!(*contents_it)) { break; }

    //printf("Parsed expression:\n");
    //print_node(expression,0);
    //putchar('\n');

  }
  free(contents);
  return ok;
}
