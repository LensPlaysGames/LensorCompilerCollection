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
const char *delimiters         = " \r\n,{}()[]<>:&@";

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
  if (*(token->end) == '\0') {
    return err;
  }
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
  new_child->parent = parent;
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
  assert(NODE_TYPE_MAX == 14 && "node_compare() must handle all node types");
  // Variable access and symbol are not same type but share same comparison.
  if (!((a->type == NODE_TYPE_SYMBOL || a->type == NODE_TYPE_VARIABLE_ACCESS)
        && (b->type == NODE_TYPE_SYMBOL || b->type == NODE_TYPE_VARIABLE_ACCESS)))
    {
      if (a->type != b->type) {
        return 0;
      }
    }
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
  case NODE_TYPE_VARIABLE_ACCESS:
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
  case NODE_TYPE_POINTER:
    printf("TODO: node_compare() POINTER\n");
    break;
  case NODE_TYPE_ADDRESSOF:
    printf("TODO: node_compare() ADDRESSOF\n");
    break;
  case NODE_TYPE_DEREFERENCE:
    printf("TODO: node_compare() DEREFERENCE\n");
    break;
  case NODE_TYPE_IF:
    printf("TODO: node_compare() IF\n");
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

#define NODE_TEXT_BUFFER_SIZE 512
char node_text_buffer[512];
char *node_text(Node *node) {
  assert(NODE_TYPE_MAX == 14 && "print_node() must handle all node types");
  if (!node) {
    return "NULL";
  }
  switch (node->type) {
  default:
    snprintf(node_text_buffer, NODE_TEXT_BUFFER_SIZE, "UNKNOWN");
    break;
  case NODE_TYPE_NONE:
    snprintf(node_text_buffer, NODE_TEXT_BUFFER_SIZE, "NONE");
    break;
  case NODE_TYPE_INTEGER:
    snprintf(node_text_buffer, NODE_TEXT_BUFFER_SIZE, "INT:%lld", node->value.integer);
    break;
  case NODE_TYPE_SYMBOL:
    snprintf(node_text_buffer, NODE_TEXT_BUFFER_SIZE, "SYM:%s", node->value.symbol);
    break;
  case NODE_TYPE_BINARY_OPERATOR:
    snprintf(node_text_buffer, NODE_TEXT_BUFFER_SIZE, "BINARY OPERATOR:%s", node->value.symbol);
    break;
  case NODE_TYPE_VARIABLE_REASSIGNMENT:
    snprintf(node_text_buffer, NODE_TEXT_BUFFER_SIZE, "VARIABLE REASSIGNMENT");
    break;
  case NODE_TYPE_VARIABLE_DECLARATION:
    snprintf(node_text_buffer, NODE_TEXT_BUFFER_SIZE, "VARIABLE DECLARATION");
    break;
  case NODE_TYPE_VARIABLE_ACCESS:
    snprintf(node_text_buffer, NODE_TEXT_BUFFER_SIZE, "VARIABLE ACCESS:%s", node->value.symbol);
    break;
  case NODE_TYPE_IF:
    snprintf(node_text_buffer, NODE_TEXT_BUFFER_SIZE, "IF");
    break;
  case NODE_TYPE_POINTER:
    snprintf(node_text_buffer, NODE_TEXT_BUFFER_SIZE, "POINTER");
    break;
  case NODE_TYPE_ADDRESSOF:
    snprintf(node_text_buffer, NODE_TEXT_BUFFER_SIZE, "ADDRESSOF");
    break;
  case NODE_TYPE_DEREFERENCE:
    snprintf(node_text_buffer, NODE_TEXT_BUFFER_SIZE, "DEREFERENCE");
    break;
  case NODE_TYPE_PROGRAM:
    snprintf(node_text_buffer, NODE_TEXT_BUFFER_SIZE, "PROGRAM");
    break;
  case NODE_TYPE_FUNCTION:
    snprintf(node_text_buffer, NODE_TEXT_BUFFER_SIZE, "FUNCTION");
    break;
  case NODE_TYPE_FUNCTION_CALL:
    snprintf(node_text_buffer, NODE_TEXT_BUFFER_SIZE, "FUNCTION CALL");
    break;
  }
  return node_text_buffer;
}

void print_node(Node *node, size_t indent_level) {
  if (!node) { return; }

  // Print indent.
  for (size_t i = 0; i < indent_level; ++i) {
    putchar(' ');
  }
  // Print type + value.
  printf("%s\n", node_text(node));
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

void parse_context_print(ParsingContext *top, size_t indent) {
  size_t indent_it = indent;
  while (indent_it--) { putchar(' '); }
  printf("TYPES:\n");
  environment_print(*top->types,indent + 2);

  indent_it = indent;
  while (indent_it--) { putchar(' '); }
  printf("VARIABLES:\n");
  environment_print(*top->variables,indent + 2);

  if (top->parent == NULL) {
    indent_it = indent;
    while (indent_it--) { putchar(' '); }
    printf("BINARY OPERATORS:\n");
    environment_print(*top->binary_operators,indent + 2);
  }

  indent_it = indent;
  while (indent_it--) { putchar(' '); }
  printf("FUNCTIONS:\n");
  environment_print(*top->functions,indent);

  ParsingContext *child = top->children;
  while (child) {
    parse_context_print(child,indent + 2);
    child = child->next_child;
  }
}

void parse_context_add_child(ParsingContext *parent, ParsingContext *child) {
  if (parent) {
    if (parent->children) {
      parent = parent->children;
      while (parent->next_child) { parent = parent->next_child; }
      parent->next_child = child;
    } else {
      parent->children = child;
    }
  }
}

ParsingStack *parse_stack_create(ParsingStack *parent) {
  ParsingStack *stack = malloc(sizeof(ParsingStack));
  assert(stack && "Could not allocate memory for new parser continuation stack.");
  stack->parent = parent;
  stack->operator = NULL;
  stack->result = NULL;
  return stack;
}

ParsingContext *parse_context_create(ParsingContext *parent) {
  ParsingContext *ctx = calloc(1, sizeof(ParsingContext));
  assert(ctx && "Could not allocate memory for parsing context.");
  if (!ctx) { return NULL; }
  ctx->parent = parent;
  // TODO: Add this new context as a child to given parent.
  parse_context_add_child(parent, ctx);
  ctx->children = NULL;
  ctx->next_child = NULL;
  ctx->types = environment_create(NULL);
  ctx->variables = environment_create(NULL);
  ctx->functions = environment_create(NULL);
  ctx->binary_operators = environment_create(NULL);
  return ctx;
}

ParsingContext *parse_context_default_create() {
  ParsingContext *ctx = parse_context_create(NULL);
  Error err = ok;
  err = define_type(ctx->types,
                    NODE_TYPE_INTEGER,
                    node_symbol("integer"),
                    sizeof(long long));
  if (err.type != ERROR_NONE) {
    printf("ERROR: Failed to set builtin integer type in types environment.\n");
  }
  err = define_type(ctx->types,
                    NODE_TYPE_FUNCTION,
                    node_symbol("function"),
                    sizeof(long long));
  if (err.type != ERROR_NONE) {
    printf("ERROR: Failed to set builtin function type in types environment.\n");
  }
  // TODO: Should we use type IDs vs type symbols?
  // FIXME: Use precedence enum!
  const char *binop_error_message = "ERROR: Failed to set builtin binary operator in environment.";

  err = define_binary_operator(ctx, "=", 3, "integer", "integer", "integer");
  if (err.type != ERROR_NONE) { puts(binop_error_message); }
  err = define_binary_operator(ctx, "<", 3, "integer", "integer", "integer");
  if (err.type != ERROR_NONE) { puts(binop_error_message); }
  err = define_binary_operator(ctx, ">", 3, "integer", "integer", "integer");
  if (err.type != ERROR_NONE) { puts(binop_error_message); }

  err = define_binary_operator(ctx, "+", 5, "integer", "integer", "integer");
  if (err.type != ERROR_NONE) { puts(binop_error_message); }
  err = define_binary_operator(ctx, "-", 5, "integer", "integer", "integer");
  if (err.type != ERROR_NONE) { puts(binop_error_message); }

  err = define_binary_operator(ctx, "*", 10, "integer", "integer", "integer");
  if (err.type != ERROR_NONE) { puts(binop_error_message); }
  err = define_binary_operator(ctx, "/", 10, "integer", "integer", "integer");
  if (err.type != ERROR_NONE) { puts(binop_error_message); }

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
  printf("Type not found: \"%s\"\n", id->value.symbol);
  ERROR_PREP(err, ERROR_GENERIC, "Type is not found in environment.");
  return err;
}

Error parse_get_variable(ParsingContext *context, Node *id, Node *result) {
  Error err = ok;
  while (context) {
    int status = environment_get(*context->variables, id, result);
    if (status) { return ok; }
    context = context->parent;
  }
  result->type = NODE_TYPE_NONE;
  //printf("Variable not found: \"%s\"\n", id->value.symbol);
  ERROR_PREP(err, ERROR_GENERIC, "Variable is not found in environment.");
  return err;
}

#define EXPECT(expected, expected_string, current_token, current_length, end) \
  expected = lex_expect(expected_string, current_token, current_length, end); \
  if (expected.err.type) { return expected.err; }

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

/// Set FOUND to 1 if an infix operator is found and parsing should continue, otherwise 0.
Error parse_binary_infix_operator
(ParsingContext *context, ParsingStack *stack,
 int *found,
 Token *current, size_t *length, char **end,
 long long *working_precedence,
 Node *result, Node **working_result
 )
{
  Error err = ok;
  // Look ahead for a binary infix operator.
  *found = 0;
  Token current_copy = *current;
  size_t length_copy = *length;
  char *end_copy = *end;
  err = lex_advance(&current_copy, &length_copy, &end_copy);
  if (err.type != ERROR_NONE) { return err; }
  Node *operator_symbol =
    node_symbol_from_buffer(current_copy.beginning, length_copy);
  Node *operator_value = node_allocate();
  ParsingContext *global = context;
  while (global->parent) { global = global->parent; }
  if (environment_get(*global->binary_operators, operator_symbol, operator_value)) {
    *current = current_copy;
    *length = length_copy;
    *end = end_copy;
    long long precedence = operator_value->children->value.integer;

    //printf("Got op. %s with precedence %lld (working %lld)\n",
    //       operator_symbol->value.symbol,
    //       precedence, working_precedence);
    //printf("working precedence: %lld\n", working_precedence);

    // TODO: Handle grouped expressions through parentheses using precedence stack.

    Node *result_pointer = precedence <= *working_precedence ? result : *working_result;
    if (precedence  <= *working_precedence) {
      if (stack) {
        result_pointer = stack->result;
      } else {
        result_pointer = result;
      }
    } else {
      result_pointer = *working_result;
    }

    Node *result_copy = node_allocate();
    node_copy(result_pointer, result_copy);
    result_pointer->type = NODE_TYPE_BINARY_OPERATOR;
    result_pointer->value.symbol = operator_symbol->value.symbol;
    result_pointer->children = result_copy;
    result_pointer->next_child = NULL;

    Node *rhs = node_allocate();
    node_add_child(result_pointer, rhs);
    *working_result = rhs;

    *working_precedence = precedence;

    *found = 1;
  }

  free(operator_symbol);
  free(operator_value);
  return ok;
}


enum StackOperatorReturnValue {
  STACK_HANDLED_INVALID = 0,
  STACK_HANDLED_BREAK = 1,
  STACK_HANDLED_PARSE = 2,
  STACK_HANDLED_CHECK = 3
};

/**
 * @retval 1 Break (stack was NULL most likely)
 * @retval 2 Continue Parsing (working_result was updated, possibly stack as well)
 * @retval 3 Continue Checking (stack was updated, may need to handle it as well)
 */
Error handle_stack_operator
(int *status,
 ParsingContext **context,
 ParsingStack **stack,
 Node **working_result,
 Node *result,
 long long *working_precedence,
 Token *current,
 size_t *length,
 char **end
 )
{
  if (!(*stack)) {
    *status = STACK_HANDLED_BREAK;
    return ok;
  }

  Error err = ok;
  ExpectReturnValue expected;
  memset(&expected, 0, sizeof(ExpectReturnValue));

  Node *operator = (*stack)->operator;
  if (!operator || operator->type != NODE_TYPE_SYMBOL) {
    ERROR_PREP(err, ERROR_TYPE,
               "Parsing context operator must be symbol. Likely internal error :(");
    return err;
  }

  if (strcmp(operator->value.symbol, "if-cond") == 0) {
    // TODO: Maybe eventually allow multiple expressions in an if
    // condition, or something like that.
    EXPECT(expected, "{", current, length, end);
    if (expected.found) {
      Node *if_then_body = node_allocate();
      (*stack)->result->next_child = if_then_body;
      Node *if_then_first_expr = node_allocate();
      node_add_child(if_then_body, if_then_first_expr);

      // Empty if-then-body handling.
      // TODO: Maybe warn?
      EXPECT(expected, "}", current, length, end);
      if (expected.found) {
        // TODO: First check for else...
        *stack = (*stack)->parent;
        *status = STACK_HANDLED_CHECK;
        return ok;
      }

      // TODO: Should new parsing context be created for scope of if body?
      // TODO: Don't leak stack->operator.
      (*stack)->operator = node_symbol("if-then-body");
      (*stack)->body = if_then_body;
      (*stack)->result = if_then_first_expr;
      *working_result = if_then_first_expr;
      *status = STACK_HANDLED_PARSE;
      return ok;
    }
    // TODO/FIXME: Ask the user how to proceed when if has no body.
    ERROR_PREP(err, ERROR_SYNTAX, "`if` expression requires a \"then\" body.");
    return err;
  }

  if (strcmp(operator->value.symbol, "if-then-body") == 0) {
    // Evaluate next expression unless it's a closing brace.
    EXPECT(expected, "}", current, length, end);
    if (expected.done || expected.found) {
      // TODO: Lookahead for else then parse if-else-body.
      EXPECT(expected, "else", current, length, end);
      if (expected.found) {
        EXPECT(expected, "{", current, length, end);
        if (expected.found) {
          Node *if_else_body = node_allocate();
          Node *if_else_first_expr = node_allocate();
          node_add_child(if_else_body, if_else_first_expr);

          (*stack)->body->next_child = if_else_body;

          // TODO: Don't leak stack operator!
          (*stack)->operator = node_symbol("if-else-body");
          (*stack)->body = if_else_body;
          (*stack)->result = if_else_first_expr;
          *working_result = if_else_first_expr;
          *status = STACK_HANDLED_PARSE;
          return ok;
        }
        ERROR_PREP(err, ERROR_SYNTAX, "`else` must be followed by body.");
        return err;
      }

      *stack = (*stack)->parent;
      *status = STACK_HANDLED_CHECK;
      return ok;
    }
    Node *next_expr = node_allocate();
    (*stack)->result->next_child = next_expr;
    (*stack)->result = next_expr;
    *working_result = next_expr;
    *status = STACK_HANDLED_PARSE;
    return ok;
  }

  if (strcmp(operator->value.symbol, "if-else-body") == 0) {
    // Evaluate next expression unless it's a closing brace.
    EXPECT(expected, "}", current, length, end);
    if (expected.done || expected.found) {
      *stack = (*stack)->parent;
      *status = STACK_HANDLED_CHECK;
      return ok;
    }
    Node *next_expr = node_allocate();
    node_add_child((*stack)->result, next_expr);
    *working_result = next_expr;
    *status = STACK_HANDLED_PARSE;
    return ok;
  }

  if (strcmp(operator->value.symbol, "lambda") == 0) {
    // Evaluate next expression unless it's a closing brace.
    EXPECT(expected, "}", current, length, end);
    if (expected.done || expected.found) {
      EXPECT(expected, "]", current, length, end);
      if (expected.done || expected.found) {
        *context = (*context)->parent;
        *stack = (*stack)->parent;
        *status = STACK_HANDLED_CHECK;
        return ok;
      } else {
        ERROR_PREP(err, ERROR_SYNTAX,
                   "Expected closing square bracket for following lambda body definition");
        return err;
      }
    }

    (*stack)->result->next_child = node_allocate();
    *working_result = (*stack)->result->next_child;
    (*stack)->result = *working_result;
    *status = STACK_HANDLED_PARSE;
    return ok;
  }
  if (strcmp(operator->value.symbol, "defun") == 0) {
    // Evaluate next expression unless it's a closing brace.
    EXPECT(expected, "}", current, length, end);
    if (expected.done || expected.found) {
      *context = (*context)->parent;
      *stack = (*stack)->parent;
      if (!(*stack)) {
        *status = STACK_HANDLED_BREAK;
      } else {
        *status = STACK_HANDLED_CHECK;
      }
      return ok;
    }

    (*stack)->result->next_child = node_allocate();
    *working_result = (*stack)->result->next_child;
    (*stack)->result = *working_result;
    *status = STACK_HANDLED_PARSE;
    return ok;
  }
  if (strcmp(operator->value.symbol, "funcall") == 0) {
    EXPECT(expected, ")", current, length, end);
    if (expected.done || expected.found) {

      *stack = (*stack)->parent;

      int found = 0;
      err = parse_binary_infix_operator(*context, *stack, &found, current, length,
                                        end, working_precedence, result, working_result);
      if (found) {
        *status = STACK_HANDLED_PARSE;
      } else {
        *status = STACK_HANDLED_CHECK;
      }
      return ok;
    }

    // FIXME?: Should comma be optional?
    EXPECT(expected, ",", current, length, end);
    if (expected.done || !expected.found) {
      print_token(*current);
      ERROR_PREP(err, ERROR_SYNTAX,
                 "Parameter list expected closing parenthesis or comma for another parameter");
      return err;
    }
    (*stack)->result->next_child = node_allocate();
    *working_result = (*stack)->result->next_child;
    (*stack)->result = *working_result;
    *status = STACK_HANDLED_PARSE;
    return ok;
  }
  *status = STACK_HANDLED_INVALID;
  ERROR_PREP(err, ERROR_GENERIC, "Internal error: Could not handle stack operator!");
  return err;
}

Error parse_expr
(ParsingContext *context,
 char *source,
 char **end,
 Node *result
 )
{
  ParsingStack *stack = NULL;
  ExpectReturnValue expected;
  size_t token_length = 0;
  Token current_token;
  current_token.beginning  = source;
  current_token.end        = source;
  Error err = ok;

  Node *working_result = result;
  long long working_precedence = 0;

  while ((err = lex_advance(&current_token, &token_length, end)).type == ERROR_NONE) {
    //printf("lexed: "); print_token(current_token); putchar('\n');
    if (token_length == 0) { return ok; }

    if (parse_integer(&current_token, working_result)) {

    } else {

      Node *symbol = node_symbol_from_buffer(current_token.beginning, token_length);

      if (strcmp("@", symbol->value.symbol) == 0) {
        working_result->type = NODE_TYPE_DEREFERENCE;
        Node *child = node_allocate();
        node_add_child(working_result, child);
        working_result = child;
        continue;
      }

      if (strcmp("&", symbol->value.symbol) == 0) {
        working_result->type = NODE_TYPE_ADDRESSOF;
        Node *child = node_allocate();
        node_add_child(working_result, child);
        working_result = child;
        continue;
      }

      if (strcmp("if", symbol->value.symbol) == 0) {
        Node *if_conditional = working_result;
        if_conditional->type = NODE_TYPE_IF;
        Node *condition_expression = node_allocate();
        node_add_child(if_conditional, condition_expression);
        working_result = condition_expression;

        stack = parse_stack_create(stack);
        stack->operator = node_symbol("if-cond");
        stack->result = working_result;
        continue;
      }

      // Parse lambda
      if (strcmp("[", symbol->value.symbol) == 0) {

        Node *lambda = working_result;
        lambda->type = NODE_TYPE_FUNCTION;

        // Return type
        lex_advance(&current_token, &token_length, end);
        Node *function_return_type = node_symbol_from_buffer(current_token.beginning, token_length);
        // TODO: Ensure function return type is a valid type.

        // Parameter list
        EXPECT(expected, "(", &current_token, &token_length, end);
        if (!expected.found || expected.done) {
          ERROR_PREP(err, ERROR_SYNTAX, "Parameter list required within lambda definition");
          return err;
        }

        Node *parameter_list = node_allocate();

        // FIXME?: Should we possibly create a parser stack and evaluate the
        // next expression, then ensure return value is var. decl. in stack
        // handling below?
        for (;;) {
          EXPECT(expected, ")", &current_token, &token_length, end);
          if (expected.found) { break; }
          if (expected.done) {
            ERROR_PREP(err, ERROR_SYNTAX,
                       "Expected closing parenthesis for parameter list");
            return err;
          }

          err = lex_advance(&current_token, &token_length, end);
          if (err.type) { return err; }
          Node *parameter_name = node_symbol_from_buffer(current_token.beginning, token_length);

          EXPECT(expected, ":", &current_token, &token_length, end);
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

          EXPECT(expected, ",", &current_token, &token_length, end);
          if (expected.found) { continue; }

          EXPECT (expected, ")", &current_token, &token_length, end);
          if (!expected.found) {
            ERROR_PREP(err, ERROR_SYNTAX, "Expected closing parenthesis following parameter list");
            return err;
          }
          break;

        }

        // TODO/FIXME: Do I need to bind unnamed function in environment?
        //environment_set(context->functions, function_name, working_result);

        // Parse function body.
        EXPECT(expected, "{", &current_token, &token_length, end);
        if (expected.done || !expected.found) {
          ERROR_PREP(err, ERROR_SYNTAX, "Function definition requires body following return type");
          return err;
        }

        context = parse_context_create(context);
        Node *param_it = parameter_list->children;
        while (param_it) {
          environment_set(context->variables,
                          param_it->children,
                          param_it->children->next_child);
          param_it = param_it->next_child;
        }

        Node *function_body = node_allocate();
        Node *function_first_expression = node_allocate();
        node_add_child(function_body, function_first_expression);
        working_result = function_first_expression;

        node_add_child(lambda, parameter_list);
        node_add_child(lambda, function_return_type);
        node_add_child(lambda, function_body);

        stack = parse_stack_create(stack);
        stack->operator = node_symbol("lambda");
        stack->result = working_result;

        continue;
      }

      // TODO: Parse strings and other literal types.

      // TODO: Check for unary prefix operators.

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

        Node *function = working_result;
        function->type = NODE_TYPE_FUNCTION;

        lex_advance(&current_token, &token_length, end);
        Node *function_name = node_symbol_from_buffer(current_token.beginning, token_length);

        EXPECT(expected, "(", &current_token, &token_length, end);
        if (!expected.found) {
          printf("Function Name: \"%s\"\n", function_name->value.symbol);
          ERROR_PREP(err, ERROR_SYNTAX,
                     "Expected opening parenthesis for parameter list after function name");
          return err;
        }

        Node *parameter_list = node_allocate();
        node_add_child(function, parameter_list);

        // FIXME?: Should we possibly create a parser stack and evaluate the
        // next expression, then ensure return value is var. decl. in stack
        // handling below?
        for (;;) {
          EXPECT(expected, ")", &current_token, &token_length, end);
          if (expected.found) { break; }
          if (expected.done) {
            ERROR_PREP(err, ERROR_SYNTAX,
                       "Expected closing parenthesis for parameter list");
            return err;
          }

          err = lex_advance(&current_token, &token_length, end);
          if (err.type) { return err; }
          Node *parameter_name = node_symbol_from_buffer(current_token.beginning, token_length);

          EXPECT(expected, ":", &current_token, &token_length, end);
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

          EXPECT(expected, ",", &current_token, &token_length, end);
          if (expected.found) { continue; }

          EXPECT (expected, ")", &current_token, &token_length, end);
          if (!expected.found) {
            ERROR_PREP(err, ERROR_SYNTAX, "Expected closing parenthesis following parameter list");
            return err;
          }
          break;

        }

        // Parse return type.
        EXPECT(expected, ":", &current_token, &token_length, end);
        // TODO/FIXME: Should we allow implicit return type?
        if (expected.done || !expected.found) {
          ERROR_PREP(err, ERROR_SYNTAX,
                     "Function definition requires return type annotation following parameter list");
          return err;
        }

        lex_advance(&current_token, &token_length, end);
        Node *function_return_type = node_symbol_from_buffer(current_token.beginning, token_length);
        node_add_child(function, function_return_type);

        // Bind function to function name in functions environment.
        environment_set(context->functions, function_name, working_result);

        // Parse function body.
        EXPECT(expected, "{", &current_token, &token_length, end);
        if (expected.done || !expected.found) {
          ERROR_PREP(err, ERROR_SYNTAX, "Function definition requires body following return type");
          return err;
        }

        context = parse_context_create(context);
        Node *param_it = function->children->children;
        while (param_it) {
          environment_set(context->variables,
                          param_it->children,
                          param_it->children->next_child);
          param_it = param_it->next_child;
        }

        Node *function_body = node_allocate();
        Node *function_first_expression = node_allocate();
        node_add_child(function_body, function_first_expression);
        node_add_child(function, function_body);
        working_result = function_first_expression;

        stack = parse_stack_create(stack);
        stack->operator = node_symbol("defun");
        stack->result = working_result;

        continue;

      }

      // Check if valid symbol for variable environment, then
      // attempt to pattern match variable access, assignment,
      // declaration, or declaration with initialization.

      // Check for variable access here.
      Node *var_binding = node_allocate();
      err = parse_get_variable(context, symbol, var_binding);
      if (!nonep(*var_binding)) {
        if (err.type) { return err; }

        // Create variable access node.
        working_result->type = NODE_TYPE_VARIABLE_ACCESS;
        working_result->value.symbol = strdup(symbol->value.symbol);

        // Lookahead for variable reassignment.
        EXPECT(expected, ":", &current_token, &token_length, end);
        if (expected.found) {
          EXPECT(expected, "=", &current_token, &token_length, end);
          if (expected.found) {
            Node *result_pointer = result;
            if (stack && stack->result) {
              result_pointer = stack->result;
            }
            Node *lhs = node_allocate();
            node_copy(result_pointer, lhs);
            memset(result_pointer, 0, sizeof(Node));
            result_pointer->type = NODE_TYPE_VARIABLE_REASSIGNMENT;
            node_add_child(result_pointer, lhs);
            Node *reassign_expr = node_allocate();
            node_add_child(result_pointer, reassign_expr);
            working_result = reassign_expr;
            continue;
          }
        }

      } else {
        // Symbol is not a valid variable name. Check for function call
        // or new variable declaration.

        EXPECT(expected, ":", &current_token, &token_length, end);
        if (!expected.done && expected.found) {

          err = lex_advance(&current_token, &token_length, end);
          if (err.type != ERROR_NONE) { return err; }
          if (token_length == 0) { break; }

          // TODO: parse_type() with ALL the arguments.

          Node *type_value = node_allocate();
          Node *type_value_it = type_value;
          // Loop over all pointer declaration symbols.
          while (current_token.beginning[0] == '@') {
            // Add one level of pointer indirection.
            type_value_it->type = NODE_TYPE_POINTER;
            Node *child = node_allocate();
            type_value->children = child;
            type_value_it = child;
            // Advance lexer.
            err = lex_advance(&current_token, &token_length, end);
            if (err.type != ERROR_NONE) { return err; }
            if (token_length == 0) { break; }
          }

          Node *type_symbol =
            node_symbol_from_buffer(current_token.beginning, token_length);
          // WHY ARE WE DOING THIS
          Node *type_validator = node_allocate();
          err = parse_get_type(context, type_symbol, type_validator);
          if (err.type) {
            free(type_value);
            return err;
          }
          if (nonep(*type_validator)) {
            ERROR_PREP(err, ERROR_TYPE, "Invalid type within variable declaration");
            printf("\nINVALID TYPE: \"%s\"\n", type_symbol->value.symbol);
            return err;
          }
          free(type_validator);

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

          // `symbol` is now owned by working_result, a var. decl.
          node_add_child(working_result, symbol);

          // Context variables environment gains new binding.
          Node *symbol_for_env = node_allocate();
          node_copy(symbol, symbol_for_env);

          Node *type_for_env = node_allocate();
          node_copy(type_value, type_for_env);
          Node *type_child = type_for_env;
          while (type_child->children) {
            type_child = type_child->children;
          }
          *type_child = *type_symbol;

          int status = environment_set(context->variables, symbol_for_env, type_for_env);
          if (status != 1) {
            printf("Variable: \"%s\", status: %d\n", symbol_for_env->value.symbol, status);
            ERROR_PREP(err, ERROR_GENERIC, "Failed to define variable!");
            return err;
          }

          // Check for initialization after declaration.
          EXPECT(expected, "=", &current_token, &token_length, end);
          if (expected.found) {

            working_result->next_child = node_allocate();

            Node **local_result = &result;
            if (stack) { local_result = &stack->result; }

            Node *reassign = node_allocate();
            reassign->type = NODE_TYPE_VARIABLE_REASSIGNMENT;
            Node *value_expression = node_allocate();

            // FIXME: This is a problem. We should use LHS copy.
            Node *lhs = node_allocate();
            lhs->type = NODE_TYPE_VARIABLE_ACCESS;
            lhs->value.symbol = strdup(symbol->value.symbol);
            node_add_child(reassign, lhs);
            node_add_child(reassign, value_expression);

            (*local_result)->next_child = reassign;
            *local_result = reassign;

            working_result = value_expression;
            continue;
          }

        } else {
          // Symbol is not `defun` and it is not followed by an assignment operator `:`.

          // Check if it's a function call (lookahead for symbol)
          EXPECT(expected, "(", &current_token, &token_length, end);
          if (!expected.done && expected.found) {
            working_result->type = NODE_TYPE_FUNCTION_CALL;
            node_add_child(working_result, symbol);
            Node *argument_list = node_allocate();
            node_add_child(working_result, argument_list);

            EXPECT(expected, ")", &current_token, &token_length, end);
            if (expected.done || expected.found) {
              // Done parsing function call.
              int found = 0;
              err = parse_binary_infix_operator(context, stack, &found, &current_token, &token_length,
                                                end, &working_precedence, result, &working_result);
              if (found) {
                // Parse RHS of binary infix operator.
                continue;
              } else {
                // Parsed function call, check for stack continuation.
                break;
              }
            }

            Node *first_argument = node_allocate();
            node_add_child(argument_list, first_argument);
            working_result = first_argument;

            // Create a parsing stack with function call operator,
            // and then start parsing function argument expressions.
            stack = parse_stack_create(stack);
            stack->operator = node_symbol("funcall");
            stack->result = working_result;
            continue;
          }

          if (!stack) {
            printf("Symbol: \"%s\"\n", node_text(symbol));
            ERROR_PREP(err, ERROR_SYNTAX, "Unknown symbol");
            return err;
          }
        }
      }
      free(var_binding);
    }

    // NOTE: We often need to continue from here.

    int found = 0;
    err = parse_binary_infix_operator(context, stack, &found, &current_token, &token_length,
                                      end, &working_precedence, result, &working_result);
    if (found) {
      continue;
    }

    // If no more parser stack, return with current result.
    // Otherwise, handle parser stack operator.
    if (!stack) { break; }

    int status = 1;
    do {
      err = handle_stack_operator(&status, &context, &stack,
                                  &working_result, result,
                                  &working_precedence,
                                  &current_token, &token_length, end);
      if (err.type) { return err; }
    } while (status == STACK_HANDLED_CHECK);

    if (status == STACK_HANDLED_BREAK) {
      break;
    }
    if (status == STACK_HANDLED_PARSE) {
      continue;
    }

    printf("status: %d\n", status);
    ERROR_PREP(err, ERROR_GENERIC,
               "Internal Compiler Error :(. Unhandled parse stack operator.");
    return err;
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

Error define_binary_operator
(ParsingContext *context,
 char *operator,
 int precedence,
 char *return_type,
 char *lhs_type,
 char *rhs_type
 )
{
  Node *binop = node_allocate();
  node_add_child(binop, node_integer(precedence));
  node_add_child(binop, node_symbol(return_type));
  node_add_child(binop, node_symbol(lhs_type));
  node_add_child(binop, node_symbol(rhs_type));

  // FIXME: Every binary operator definition is global for now!
  while (context->parent) { context = context->parent; }
  int status = environment_set(context->binary_operators, node_symbol(operator), binop);
  if (status == 0) {
    ERROR_CREATE(err, ERROR_GENERIC, "Could not define binary operator in environment");
    return err;
  }
  return ok;
}
