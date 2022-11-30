#include <parser.h>

#include <error.h>
#include <environment.h>
#include <file_io.h>
#include <inttypes.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

//================================================================ BEG lexer

// TODO: Allow multi-byte comment delimiters.
const char *comment_delimiters = ";#";
const char *whitespace         = " \t\r\n";
// TODO: Think harder about delimiters.
const char *delimiters         = " \t\r\n,{}()[]<>:&@";

bool comment_at_beginning(Token token) {
  const char *comment_it = comment_delimiters;
  while (*comment_it) {
    if (*(token.beginning) == *comment_it) {
      return true;
    }
    comment_it++;
  }
  return false;
}

// "a +- 4" == "a + -4"
// FIXME: If we removed whitespace from token string view during binary
// operator comparison than this would work.
// "a > > 4" != "a >> 4"

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

/** Keep beginning of token the same, but extend the end by another token.
 *
 * Begin lexing at the end of the given token.
 */
Error lex_extend(Token *token) {
  Error err = ok;
  Token new_token;
  err = lex(token->end, &new_token);
  token->end = new_token.end;
  if (err.type) { return err; }
  return err;
}

bool token_string_equalp(char* string, Token *token) {
  if (!string || !token) { return 0; }
  char *beg = token->beginning;
  while (*string && token->beginning < token->end) {
    if (*string != *beg) {
      return false;
    }
    string++;
    beg++;
  }
  return true;
}

void print_token(Token t) {
  if (t.end - t.beginning < 1) {
    printf("INVALID TOKEN POINTERS");
  } else {
    printf("%.*s", (int)(t.end - t.beginning), t.beginning);
  }
}

//================================================================ END lexer

char node_returns_value(Node *node) {
  if (!node) {
    return 0;
  }
  switch(node->type) {
  case NODE_TYPE_INTEGER:
  case NODE_TYPE_FUNCTION:
  case NODE_TYPE_FUNCTION_CALL:
  case NODE_TYPE_VARIABLE_ACCESS:
  case NODE_TYPE_IF:
  case NODE_TYPE_ADDRESSOF:
  case NODE_TYPE_DEREFERENCE:
  case NODE_TYPE_INDEX:
  case NODE_TYPE_BINARY_OPERATOR:
  case NODE_TYPE_CAST:
  case NODE_TYPE_PROGRAM:
    return 1;
    break;
  default:
    return 0;
    break;
  }
  return 0;
}

Node *node_allocate() {
  Node *node = calloc(1,sizeof(Node));
  ASSERT(node, "Could not allocate memory for AST node");
  return node;
}

void node_add_child(Node *parent, Node *new_child) {
  if (!parent || !new_child) { return; }
  new_child->parent = parent;
  if (parent->children) {
    Node *child = parent->children;
    if (child == new_child) {
      fprintf(stderr, "DEVELOPER WARNING: Refusal to make circular linked list in node_add_child()\n");
      return;
    }
    while (child->next_child) {
      child = child->next_child;
      if (child == new_child) {
        fprintf(stderr, "DEVELOPER WARNING: Refusal to make circular linked list in node_add_child()\n");
        return;
      }
    }
    child->next_child = new_child;
  } else {
    parent->children = new_child;
  }
}

int node_compare(Node *a, Node *b) {
  ASSERT(NODE_TYPE_MAX == 15, "node_compare() must handle all node types");

  // Actually really nice debug output when you need it.
  //printf("Comparing nodes:\n");
  //print_node(a, 2);
  //print_node(b, 2);
  //putchar('\n');

  if (!a || !b) {
    if (!a && !b) {
      return 1;
    }
    return 0;
  }

  // Compare types of nodes.
  // Variable access and symbol are not same type but share same comparison.
  if (!((a->type == NODE_TYPE_SYMBOL || a->type == NODE_TYPE_VARIABLE_ACCESS)
        && (b->type == NODE_TYPE_SYMBOL || b->type == NODE_TYPE_VARIABLE_ACCESS)))
    {
      if (a->type != b->type) {
        return 0;
      }
    }

  // Compare all children recursively!
  Node *a_child = a->children;
  Node *b_child = b->children;
  while (a_child && b_child) {
    if (node_compare(a_child, b_child) == 0) {
      return 0;
    }
    a_child = a_child->next_child;
    b_child = b_child->next_child;
  }
  if (a_child || b_child) {
    return 0;
  }

  // Compare value of node.
  switch (a->type) {
  default:
    return 1;
  case NODE_TYPE_INDEX:
  case NODE_TYPE_INTEGER:
    if (a->value.integer == b->value.integer) {
      return 1;
    }
    break;
  case NODE_TYPE_VARIABLE_ACCESS:
  case NODE_TYPE_SYMBOL:
  case NODE_TYPE_BINARY_OPERATOR:
    if (a->value.symbol && b->value.symbol) {
      if (strcmp(a->value.symbol, b->value.symbol) == 0) {
        return 1;
      }
      break;
    } else if (!a->value.symbol && !b->value.symbol) {
      return 1;
    }
    break;
  }
  return 0;
}

Node *node_none() {
  Node *none = node_allocate();
  none->type = NODE_TYPE_NONE;
  return none;
}

Node *node_integer(int64_t value) {
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
  ASSERT(buffer, "Can not create AST symbol node from NULL buffer");
  char *symbol_string = malloc(length + 1);
  ASSERT(symbol_string, "Could not allocate memory for symbol string");
  memcpy(symbol_string, buffer, length);
  symbol_string[length] = '\0';
  Node *symbol = node_allocate();
  symbol->type = NODE_TYPE_SYMBOL;
  symbol->value.symbol = symbol_string;
  return symbol;
}

// Take ownership of type_symbol.
Error define_type(Environment *types, int type, Node *type_symbol, long long byte_size) {
  ASSERT(types, "Can not add type to NULL types environment");
  ASSERT(type_symbol, "Can not add NULL type symbol to types environment");
  ASSERT(byte_size >= 0, "Can not define new type with zero or negative byte size");

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
  ASSERT(NODE_TYPE_MAX == 15, "print_node() must handle all node types");
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
    snprintf(node_text_buffer, NODE_TEXT_BUFFER_SIZE, "INT:%"PRId64, node->value.integer);
    break;
  case NODE_TYPE_SYMBOL:
    snprintf(node_text_buffer, NODE_TEXT_BUFFER_SIZE,
             "SYM:%s (%d)",
             node->value.symbol,
             node->pointer_indirection);
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
  case NODE_TYPE_INDEX:
    snprintf(node_text_buffer, NODE_TEXT_BUFFER_SIZE, "INDEX:%"PRId64, node->value.integer);
    break;
  case NODE_TYPE_CAST:
    snprintf(node_text_buffer, NODE_TEXT_BUFFER_SIZE, "TYPECAST");
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
  b->pointer_indirection = a->pointer_indirection;
  // Handle all allocated values here.
  switch (a->type) {
  default:
    b->value = a->value;
    break;
  case NODE_TYPE_SYMBOL:
    b->value.symbol = strdup(a->value.symbol);
    ASSERT(b->value.symbol, "node_copy(): Could not allocate memory for new symbol");
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

ParsingState parse_state_create(Token *current_token, size_t *token_length, char **end) {
  ParsingState out;
  out.current = current_token;
  out.length = token_length;
  out.end = end;
  return out;
}

void parse_state_update
(ParsingState *state,
 Token current_token,
 size_t token_length,
 char *end
 )
{
  *state->current = current_token;
  *state->length = token_length;
  *state->end = end;
}

void parse_state_update_from(ParsingState *state, ParsingState new_state) {
  *state->current = *new_state.current;
  *state->length  = *new_state.length;
  *state->end     = *new_state.end;
}

ParsingStack *parse_stack_create(ParsingStack *parent) {
  ParsingStack *stack = malloc(sizeof(ParsingStack));
  ASSERT(stack, "Could not allocate memory for new parser continuation stack.");
  stack->parent = parent;
  stack->operator = NULL;
  stack->result = NULL;
  return stack;
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
      if (parent == child) {
        fprintf(stderr, "DEVELOPER WARNING: Refusal to make circular linked list by parse_context_add_child()\n");
        return;
      }
      while (parent->next_child) {
        parent = parent->next_child;
        if (parent == child) {
          fprintf(stderr, "DEVELOPER WARNING: Refusal to make circular linked list by parse_context_add_child()\n");
          return;
        }
      }
      parent->next_child = child;
    } else {
      parent->children = child;
    }

  }
}

ParsingContext *parse_context_create(ParsingContext *parent) {
  ParsingContext *ctx = calloc(1, sizeof(ParsingContext));
  ASSERT(ctx, "Could not allocate memory for parsing context.");
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
  // TODO: Should we use type IDs vs type symbols?
  // FIXME: Use precedence enum!
  const char *binop_error_message = "ERROR: Failed to set builtin binary operator in environment.";

  err = define_binary_operator(ctx, "=", 3, "integer", "integer", "integer");
  if (err.type != ERROR_NONE) { puts(binop_error_message); }
  err = define_binary_operator(ctx, "<", 3, "integer", "integer", "integer");
  if (err.type != ERROR_NONE) { puts(binop_error_message); }
  err = define_binary_operator(ctx, ">", 3, "integer", "integer", "integer");
  if (err.type != ERROR_NONE) { puts(binop_error_message); }

  // TODO/FIXME: These are very much so temporary bitshifting operators!!!
  err = define_binary_operator(ctx, "<<", 4, "integer", "integer", "integer");
  if (err.type != ERROR_NONE) { puts(binop_error_message); }
  err = define_binary_operator(ctx, ">>", 4, "integer", "integer", "integer");
  if (err.type != ERROR_NONE) { puts(binop_error_message); }

  err = define_binary_operator(ctx, "+", 5, "integer", "integer", "integer");
  if (err.type != ERROR_NONE) { puts(binop_error_message); }
  err = define_binary_operator(ctx, "-", 5, "integer", "integer", "integer");
  if (err.type != ERROR_NONE) { puts(binop_error_message); }

  err = define_binary_operator(ctx, "*", 10, "integer", "integer", "integer");
  if (err.type != ERROR_NONE) { puts(binop_error_message); }
  err = define_binary_operator(ctx, "/", 10, "integer", "integer", "integer");
  if (err.type != ERROR_NONE) { puts(binop_error_message); }
  err = define_binary_operator(ctx, "%", 10, "integer", "integer", "integer");
  if (err.type != ERROR_NONE) { puts(binop_error_message); }

  return ctx;
}

/// Update token, token length, and end of current token pointer.
Error lex_advance(ParsingState *state) {
  if (!state || !state->current || !state->length || !state->end) {
    ERROR_CREATE(err, ERROR_ARGUMENTS,
                 "lex_advance(): pointer arguments must not be NULL!");
    return err;
  }
  Error err = lex(state->current->end, state->current);
  *state->end = state->current->end;
  if (err.type != ERROR_NONE) { return err; }
  *state->length = (size_t) (state->current->end - state->current->beginning);
  return err;
}

typedef struct ExpectReturnValue {
  Error err;
  char found;
  char done;
} ExpectReturnValue;

ExpectReturnValue lex_expect(char *expected, ParsingState *state) {
  ExpectReturnValue out;
  out.done = 0;
  out.found = 0;
  out.err = ok;
  if (!expected || !state || !state->current || !state->length || !state->end) {
    ERROR_PREP(out.err, ERROR_ARGUMENTS,
               "lex_expect() must not be passed NULL pointers!");
    return out;
  }
  Token current_copy = *state->current;
  size_t length_copy = *state->length;
  char *end_copy     = *state->end;
  ParsingState state_copy = parse_state_create(&current_copy, &length_copy, &end_copy);
  out.err = lex_advance(&state_copy);
  if (out.err.type != ERROR_NONE) { return out; }
  if (length_copy == 0) {
    out.done = 1;
    return out;
  }

  //printf("Expecting \"%s\", got \"", expected);
  //print_token(*state_copy.current);
  //printf("\"\n");

  if (token_string_equalp(expected, state_copy.current)) {
    out.found = 1;
    parse_state_update_from(state, state_copy);
  }

  return out;
}

Error parse_get_type(ParsingContext *context, Node *id, Node *result) {
  Error err = ok;

  // Handle pointers and functions, as they are both memory addresses.
  // This should ideally return target format dependant address size.
  if (id->pointer_indirection > 0
      || strcmp(id->value.symbol, "function") == 0
      || strcmp(id->value.symbol, "external function") == 0) {
    result->children = node_integer(8);
    return ok;
  }

  if (strcmp(id->value.symbol, "array") == 0) {
    // Get size of base type!
    Node *base_type = node_allocate();
    err = parse_get_type(context, id->children->next_child, base_type);
    if (err.type) { return err; }
    // Size of array is equal to size of base type multiplied by capacity of array.
    result->children = node_integer(id->children->value.integer * base_type->children->value.integer);
    free(base_type);
    return ok;
  }

  //printf("Searching following context for ");
  //print_node(id,0);
  //environment_print(*context->types,0);
  //putchar('\n');

  while (context) {
    int status = environment_get(*context->types, id, result);
    if (status) {
      //printf("Got thing:\n");
      //print_node(result,2);
      return ok;
    }
    context = context->parent;
  }
  result->type = NODE_TYPE_NONE;
  //printf("Type not found: \"%s\"\n", id->value.symbol);
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

#define EXPECT(expected, expected_string, state) \
  expected = lex_expect(expected_string, state); \
  if (expected.err.type) { return expected.err; }

bool parse_integer(Token *token, Node *node) {
  if (!token || !node) { return false; }
  char *end = NULL;
  if (token->end - token->beginning == 1 && *(token->beginning) == '0') {
    node->type = NODE_TYPE_INTEGER;
    node->value.integer = 0;
  } else if ((node->value.integer = strtoll(token->beginning, &end, 10)) != 0) {
    if (end != token->end) {
      return false;
    }
    node->type = NODE_TYPE_INTEGER;
  } else { return false; }
  return true;
}

/// Set FOUND to 1 if an infix operator is found and parsing should continue, otherwise 0.
Error parse_binary_infix_operator
(ParsingContext *context, ParsingStack *stack, ParsingState *state,
 int *found,
 long long *working_precedence, Node **working_result,
 Node *result
 )
{
  Error err = ok;
  // Look ahead for a binary infix operator.
  *found = 0;
  Token current_copy = *state->current;
  size_t length_copy = *state->length;
  char *end_copy     = *state->end;
  ParsingState state_copy = parse_state_create(&current_copy, &length_copy, &end_copy);
  err = lex_advance(&state_copy);
  if (err.type != ERROR_NONE) { return err; }

  // While state_copy.current->end isn't whitespace, is delimiter,
  // and is not null terminator, extend binary operator.
  // This is needed to catch binary operators like "<<" made up of
  // multiple delimiters.
  while (*state_copy.current->end != '\0'
         && strchr(whitespace, *state_copy.current->end) == NULL
         && strchr(delimiters, *state_copy.current->end) != NULL) {
    state_copy.current->end += 1;
    *state_copy.length += 1;
  }

  Node *operator_symbol =
    node_symbol_from_buffer(state_copy.current->beginning, *state_copy.length);

  Node *operator_value = node_allocate();
  ParsingContext *global = context;

  while (global->parent) { global = global->parent; }
  if (environment_get(*global->binary_operators, operator_symbol, operator_value)) {
    parse_state_update_from(state, state_copy);
    long long precedence = operator_value->children->value.integer;

    //printf("Got op. %s with precedence %lld (working %lld)\n",
    //       operator_symbol->value.symbol,
    //       precedence, *working_precedence);
    //printf("working precedence: %lld\n", *working_precedence);

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

  // TODO: Iterate advanced token end backwards until reaching a valid binary operator.

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
 ParsingState *state,
 Node **working_result,
 Node *result,
 long long *working_precedence
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

  if (strcmp(operator->value.symbol, "lambda-body") == 0) {
    EXPECT(expected, "}", state);
    if (expected.found) {

      // TODO: Lookahead for immediate lambda function call.

      // Eat lambda context.
      *context = (*context)->parent;
      // Stack is handled
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

  if (strcmp(operator->value.symbol, "lambdarameters") == 0) {
    EXPECT(expected, ")", state);
    if (expected.found) {
      // Pass stack to lambda-body
      EXPECT(expected, "{", state);
      if (!expected.found) {
        ERROR_PREP(err, ERROR_SYNTAX,
                   "Expected lambda body following lambda signature");
        return err;
      }

      Node *body = node_allocate();
      (*stack)->body->next_child = body;
      Node *first_expression = node_allocate();
      node_add_child(body, first_expression);

      // Enter new ParsingStack to handle lambda body.
      (*stack)->operator = node_symbol("lambda-body");
      (*stack)->body = body;
      (*stack)->result = first_expression;
      *working_result = first_expression;
      *status = STACK_HANDLED_PARSE;
      return ok;
    }

    // Eat the comma in-between variable declarations.
    EXPECT(expected, ",", state);
    if (expected.done) {
      ERROR_PREP(err, ERROR_SYNTAX, "Expected another parameter definition but got EOF!");
      return err;
    }

    Node *next_expr = node_allocate();
    (*stack)->result->next_child = next_expr;
    (*stack)->result = next_expr;
    *working_result = next_expr;
    *status = STACK_HANDLED_PARSE;
    return ok;
  }

  if (strcmp(operator->value.symbol, "if-condition") == 0) {
    // TODO: Maybe eventually allow multiple expressions in an if
    // condition, or something like that.
    EXPECT(expected, "{", state);
    if (expected.found) {
      Node *if_then_body = node_allocate();
      (*stack)->result->next_child = if_then_body;
      Node *if_then_first_expr = node_allocate();
      node_add_child(if_then_body, if_then_first_expr);

      // Empty if-then-body handling.
      // TODO: Maybe warn?
      EXPECT(expected, "}", state);
      if (expected.found) {

        // TODO: First check for else...

        *context = (*context)->parent;

        *stack = (*stack)->parent;
        *status = STACK_HANDLED_CHECK;
        return ok;
      }

      // TODO: Don't leak stack->operator.
      (*stack)->operator = node_symbol("if-then-body");
      (*stack)->body = if_then_body;
      (*stack)->result = if_then_first_expr;

      *working_result = if_then_first_expr;
      *status = STACK_HANDLED_PARSE;
      return ok;
    }
    // TODO/FIXME: Ask the user how to proceed when if has no body.
    ERROR_PREP(err, ERROR_SYNTAX,
               "Expected `{` after `if` condition. `if` expression requires a \"then\" body.");
    return err;
  }

  if (strcmp(operator->value.symbol, "if-then-body") == 0) {
    // Evaluate next expression unless it's a closing brace.
    EXPECT(expected, "}", state);
    if (expected.done) {
      ERROR_PREP(err, ERROR_SYNTAX, "EOF reached before end of if-then-body.");
      return err;
    }
    if (expected.found) {
      // Eat if-then-body context.
      *context = (*context)->parent;

      // Lookahead for else then parse if-else-body.
      EXPECT(expected, "else", state);
      if (expected.found) {
        EXPECT(expected, "{", state);
        if (expected.found) {

          *context = parse_context_create(*context);

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
    EXPECT(expected, "}", state);
    if (expected.done || expected.found) {
      *context = (*context)->parent;
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

  if (strcmp(operator->value.symbol, "funcall") == 0) {
    EXPECT(expected, ")", state);
    if (expected.done) {
      ERROR_PREP(err, ERROR_SYNTAX, "Expected closing parenthesis for functionc all before end of file.");
      return err;
    }
    if (expected.found) {

      *stack = (*stack)->parent;

      int found = 0;
      err = parse_binary_infix_operator(*context, *stack, state, &found,
                                        working_precedence, working_result,
                                        result);
      if (found) {
        *status = STACK_HANDLED_PARSE;
      } else {
        *status = STACK_HANDLED_CHECK;
      }
      return ok;
    }

    EXPECT(expected, ",", state);
    if (expected.done) {
      print_token(*state->current);
      ERROR_PREP(err, ERROR_SYNTAX,
                 "Parameter list expected closing parenthesis or comma for another parameter");
      return err;
    }

    Node *next_expr = node_allocate();
    (*stack)->result->next_child = next_expr;
    (*stack)->result = next_expr;
    *working_result = next_expr;
    *status = STACK_HANDLED_PARSE;
    return ok;
  }
  *status = STACK_HANDLED_INVALID;
  printf("Invalid stack operator: \"%s\"\n", (*stack)->operator->value.symbol);
  ERROR_PREP(err, ERROR_GENERIC, "Internal error: Could not handle stack operator!");
  return err;
}

Error parse_base_type
(ParsingContext *context,
 ParsingState *state,
 Node *type
 )
{
  Error err = ok;

  Token current_copy = *state->current;
  size_t length_copy = *state->length;
  char *end_copy = *state->end;
  ParsingState state_copy = parse_state_create(&current_copy, &length_copy, &end_copy);

  unsigned indirection_level = 0;
  // Loop over all pointer declaration symbols.
  while (state_copy.current->beginning[0] == '@') {
    // Add one level of pointer indirection.
    indirection_level += 1;
    // Advance lexer.
    err = lex_advance(&state_copy);
    if (err.type != ERROR_NONE) { return err; }
    if (length_copy == 0) {
      ERROR_PREP(err, ERROR_SYNTAX, "Expected a valid type following pointer type declaration symbol: `@`");
      return err;
    }
  }

  Node *type_symbol =
    node_symbol_from_buffer(state_copy.current->beginning, *state_copy.length);
  *type = *type_symbol;
  type->pointer_indirection = indirection_level;

  Node *type_validator = node_allocate();
  err = parse_get_type(context, type_symbol, type_validator);
  if (err.type) { return err; }
  if (nonep(*type_validator)) {
    ERROR_PREP(err, ERROR_TYPE, "Invalid type within variable declaration");
    printf("\nINVALID TYPE: \"%s\"\n", type_symbol->value.symbol);
    return err;
  }
  free(type_symbol);
  free(type_validator);

  parse_state_update_from(state, state_copy);

  return err;
}

Error parse_type(ParsingContext *context, ParsingState *state, Node *type, Node *parameter_names) {
  Error err = ok;
  ExpectReturnValue expected;
  bool external = 0;

  Node *parameter_names_last = NULL;

  // Parse type prefix keywords.

  // If state is at EXT, then advance lexer and mark external.
  if (external = token_string_equalp("ext", state->current), external) {
    err = lex_advance(state);
    if (err.type) { return err; }
    if (*state->length == 0) {
      ERROR_PREP(err, ERROR_SYNTAX, "Expected base type following \"ext\" type annotation keyword.");
      return err;
    }
  }

  err = parse_base_type(context, state, type);
  if (err.type) { return err; }

  // Parse function signature as type (instead of custom)

  EXPECT(expected, "(", state);
  if (expected.found) {
    Node *return_type = node_allocate();
    node_copy(type, return_type);
    free(type->value.symbol);
    Node *function_type =
      external
      ? node_symbol("external function")
      : node_symbol("function");
    *type = *function_type;
    free(function_type);
    node_add_child(type, return_type);

    // Parse parameters of function signature

    for (;;) {
      EXPECT(expected, ")", state);
      if (expected.done) {
        ERROR_PREP(err, ERROR_SYNTAX,
                   "Expected closing paren, `)`, at end of function signature type");
        return err;
      }
      if (expected.found) {
        break;
      }

      // Parse parameter name
      err = lex_advance(state);
      if (err.type != ERROR_NONE) { return err; }
      if (*state->length == 0) {
        ERROR_PREP(err, ERROR_SYNTAX, "I hope we never see this error");
        return err;
      }

      Node *parameter_name = node_symbol_from_buffer(state->current->beginning,
        (size_t)(state->current->end - state->current->beginning));

      // First time, write to parameter_names (or parameter_names_last)
      // Then on, write to parameter_names_last->next_child

      if (parameter_names_last) {
        parameter_names_last->next_child = parameter_name;
        parameter_names_last = parameter_names_last->next_child;
      } else {
        parameter_names_last = parameter_names;
        node_copy(parameter_name, parameter_names_last);
        node_free(parameter_name);
      }

      EXPECT(expected, ":", state);
      if (!expected.found) {
        ERROR_PREP(err, ERROR_SYNTAX,
                   "Expected type annotation operator following function parameter name");
        return err;
      }

      err = lex_advance(state);
      if (err.type != ERROR_NONE) { return err; }
      if (*state->length == 0) {
        ERROR_PREP(err, ERROR_SYNTAX, "Expected type following type annotation operator for function parameter");
        return err;
      }
      Node *parameter_type = node_allocate();
      err = parse_type(context, state, parameter_type, NULL);
      if (err.type) { return err; }

      // With finished parameter, add as child to function, or add to function context?
      node_add_child(type, parameter_type);
    }

    return err;
  }

  if (external) {
    // This is a non-function "ext" type.
    ERROR_PREP(err, ERROR_TYPE, "\"ext\" may only be used on function types (for now).");
    return err;
  }

  EXPECT(expected, "[", state);
  if (expected.found) {
    // Copy base type
    Node *base_type = node_allocate();
    node_copy(type, base_type);
    free(type->value.symbol);
    // Create array type
    Node *array_type = node_symbol("array");
    *type = *array_type;
    free(array_type);

    // Parse size integer
    err = lex_advance(state);
    if (err.type) { return err; }

    Node *array_size = node_allocate();
    if (parse_integer(state->current, array_size) == 0) {
      ERROR_PREP(err, ERROR_SYNTAX, "Array type annotation expects array size (an integer) after opening '['.");
      return err;
    };

    node_add_child(type, array_size);

    EXPECT(expected, "]", state);
    if (!expected.found) {
      ERROR_PREP(err, ERROR_SYNTAX,
                 "Array type annotation must have closing ']' after array size.");
      return err;
    }

    node_add_child(type, base_type);
  }

  return err;
}


Error parse_expr
(ParsingContext *context,
 char *source,
 char **end,
 Node *result
 )
{
  Error err = ok;

  ExpectReturnValue expected;
  ParsingStack *stack = NULL;

  size_t token_length = 0;
  Token current_token;
  current_token.beginning  = source;
  current_token.end        = source;

  ParsingState state = parse_state_create(&current_token, &token_length, end);
  Node *working_result = result;
  long long working_precedence = 0;

  while ((err = lex_advance(&state)).type == ERROR_NONE) {
    //printf("lexed: "); print_token(current_token); putchar('\n');
    if (token_length == 0) { return ok; }

    if (parse_integer(&current_token, working_result)) {

    } else {

      // Check for lambda
      Node *type = node_allocate();
      err = parse_base_type(context, &state, type);
      if (err.type == ERROR_NONE) {

        context = parse_context_create(context);

        Node *lambda = working_result;
        lambda->type = NODE_TYPE_FUNCTION;
        node_add_child(lambda, type);

        EXPECT(expected, "(", &state);
        if (!expected.found) {
          // Nothing except a lambda starts with a type annotation.
          ERROR_PREP(err, ERROR_SYNTAX,
                     "Expected parameter declaration(s) following lambda return type");
          return err;
        }

        Node *parameters = node_allocate();
        node_add_child(lambda, parameters);

        EXPECT(expected, ")", &state);
        if (expected.found) {

          EXPECT(expected, "{", &state);
          if (!expected.found) {
            ERROR_PREP(err, ERROR_SYNTAX,
                       "Expected lambda body following lambda signature");
            return err;
          }

          // TODO: Handle empty function body.

          Node *body = node_allocate();
          node_add_child(lambda, body);
          Node *first_expression = node_allocate();
          node_add_child(body, first_expression);

          // Enter new ParsingStack to handle lambda body.
          stack = parse_stack_create(stack);
          stack->operator = node_symbol("lambda-body");
          stack->body = body;
          stack->result = first_expression;
          working_result = first_expression;
          continue;
        }

        Node *first_parameter = node_allocate();
        node_add_child(parameters, first_parameter);

        // Enter new ParsingStack to handle lambda parameters.
        stack = parse_stack_create(stack);
        stack->operator = node_symbol("lambdarameters");
        stack->body = parameters;
        stack->result = first_parameter;
        working_result = first_parameter;
        continue;

      } else {
        // Error generated by parse_base_type() is discarded here.
        err = ok;

        Node *symbol = node_symbol_from_buffer(current_token.beginning, token_length);


        // FIXME: Experimental and not implemented in typechecker or codegen backends yet.
        // NOTE: Square bracket syntax is for sure experimental, and likely temporary.
        if (strcmp("[", symbol->value.symbol) == 0) {
          working_result->type = NODE_TYPE_CAST;

          err = lex_advance(&state);
          if (err.type) { return err; }

          Node *cast_type = node_allocate();
          err = parse_base_type(context, &state, cast_type);
          if (err.type) { return err; }

          node_add_child(working_result, cast_type);

          EXPECT(expected, "]", &state);
          if (!expected.found) {
            ERROR_PREP(err, ERROR_SYNTAX,
                       "There must be a closing square bracket, ']', after a type within a type cast expression.");
            return err;
          }

          Node *child = node_allocate();
          node_add_child(working_result, child);
          working_result = child;
          continue;
        }

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

          context = parse_context_create(context);

          stack = parse_stack_create(stack);
          stack->operator = node_symbol("if-condition");
          stack->result = condition_expression;

          working_result = condition_expression;
          continue;
        }

        // TODO: Parse strings and other literal types.

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

          // Lookahead for function call
          EXPECT(expected, "(", &state);
          if (expected.found) {
            // This is a function call!

            Node *var_access = node_allocate();
            node_copy(working_result, var_access);
            working_result->type = NODE_TYPE_FUNCTION_CALL;
            working_result->value.integer = 0;
            node_add_child(working_result, var_access);

            Node *parameters = node_allocate();
            node_add_child(working_result, parameters);

            EXPECT(expected, ")", &state);
            if (!expected.found) {
              Node *first_parameter = node_allocate();
              node_add_child(parameters, first_parameter);
              stack = parse_stack_create(stack);
              stack->operator = node_symbol("funcall");
              stack->result = first_parameter;
              working_result = first_parameter;
              continue;
            }
          } else {

            // Lookahead for array index operator.
            EXPECT(expected, "[", &state);
            if (expected.found) {

              Node *var_access = node_allocate();
              node_copy(working_result, var_access);
              working_result->type = NODE_TYPE_INDEX;
              node_add_child(working_result, var_access);

              err = lex_advance(&state);
              if (err.type) { return err; }

              Node *integer_offset = node_allocate();
              if (parse_integer(state.current, integer_offset) == 0) {
                ERROR_PREP(err, ERROR_SYNTAX,
                           "Expected integer following opening index operator: '['.");
                return err;
              }

              working_result->value.integer = integer_offset->value.integer;
              node_free(integer_offset);

              EXPECT(expected, "]", &state);
              if (!expected.found) {
                ERROR_PREP(err, ERROR_SYNTAX,
                           "Expected closing index operator following index integer: ']'.");
                return err;
              }

            }

            // Lookahead for variable reassignment.
            EXPECT(expected, ":", &state);
            if (expected.found) {
              EXPECT(expected, "=", &state);
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
          }
        } else {
          // Symbol is not a valid variable name. Check for function call
          // or new variable declaration.

          EXPECT(expected, ":", &state);
          if (!expected.done && expected.found) {

            EXPECT(expected, "=", &state);
            if (!expected.done && expected.found) {
              printf("Invalid Variable Symbol: \"%s\"\n", symbol->value.symbol);
              ERROR_PREP(err, ERROR_SYNTAX,
                         "Reassignment of undeclared variable is not allowed!");
              return err;
            }

            err = lex_advance(&state);
            if (err.type != ERROR_NONE) { return err; }
            if (token_length == 0) { break; }
            Node *decl_type = node_allocate();
            Node *param_names = node_allocate();
            err = parse_type(context, &state, decl_type, param_names);
            if (err.type) { return err; }

            Node *variable_binding = node_allocate();
            if (environment_get(*context->variables, symbol, variable_binding)) {
              // TODO: Create new error type.
              printf("ID of redefined variable: \"%s\"\n", symbol->value.symbol);
              ERROR_PREP(err, ERROR_GENERIC, "Redefinition of variable!");
              return err;
            }
            // Variable binding is shell-node for environment value contents.
            free(variable_binding);

            Node *variable_declaration = working_result;
            variable_declaration->type = NODE_TYPE_VARIABLE_DECLARATION;

            // `symbol` is now owned by working_result, a var. decl.
            node_add_child(variable_declaration, symbol);

            // Context variables environment gains new binding.
            Node *symbol_for_env = node_allocate();
            node_copy(symbol, symbol_for_env);

            int status = environment_set(context->variables, symbol_for_env, decl_type);
            if (status != 1) {
              printf("Variable: \"%s\", status: %d\n", symbol_for_env->value.symbol, status);
              ERROR_PREP(err, ERROR_GENERIC, "Failed to define variable!");
              return err;
            }

            if (strcmp(decl_type->value.symbol, "function") == 0) {
              EXPECT(expected, "{", &state);
              if (expected.found) {

                Node **local_result = &result;
                if (stack) { local_result = &stack->result; }

                Node *reassign = node_allocate();
                reassign->type = NODE_TYPE_VARIABLE_REASSIGNMENT;
                Node *lambda = node_allocate();

                Node *access = node_allocate();
                node_copy(symbol, access);
                access->type = NODE_TYPE_VARIABLE_ACCESS;
                node_add_child(reassign, access);

                node_add_child(reassign, lambda);

                (*local_result)->next_child = reassign;
                *local_result = reassign;

                // TODO: Expect empty body and handle that accordingly.

                context = parse_context_create(context);

                lambda->type = NODE_TYPE_FUNCTION;

                // Add return type of function signature type (first
                // child) as the first child of the lambda
                // (return type).
                Node *lambda_return_type = node_allocate();
                node_copy(decl_type->children, lambda_return_type);
                node_add_child(lambda, lambda_return_type);

                Node *parameters = node_allocate();
                node_add_child(lambda, parameters);

                // type->children->next_child refers to the first parameter
                // for each parameter, we need a new variable
                // declaration node as a child of `parameters`

                // TODO: parameter_names may be invalid, we should copy
                // global and use that copy.

                Node *param_name = param_names;
                Node *param_type = decl_type->children->next_child;

                while (param_type) {
                  Node *new_param = node_allocate();
                  new_param->type = NODE_TYPE_VARIABLE_DECLARATION;
                  Node *new_param_name = node_allocate();

                  node_copy(param_name, new_param_name);
                  node_add_child(new_param, new_param_name);

                  node_add_child(parameters, new_param);

                  // Context variables environment gains new binding.
                  Node *param_symbol_for_env = node_allocate();
                  node_copy(param_name, param_symbol_for_env);

                  int v_status = environment_set(context->variables, param_symbol_for_env, param_type);
                  if (v_status != 1) {
                    printf("Variable: \"%s\", status: %d\n", param_symbol_for_env->value.symbol, v_status);
                    ERROR_PREP(err, ERROR_GENERIC, "Failed to define parameter variable!");
                    return err;
                  }

                  param_type = param_type->next_child;
                  param_name = param_name->next_child;
                }

                // TODO: Free param_names list

                Node *body = node_allocate();
                node_add_child(lambda, body);
                Node *first_expression = node_allocate();
                node_add_child(body, first_expression);

                // Enter new ParsingStack to handle lambda body.
                stack = parse_stack_create(stack);
                stack->operator = node_symbol("lambda-body");
                stack->body = body;
                stack->result = first_expression;
                working_result = first_expression;
                continue;
              }
            }

            // TODO: Free param_names list

            // Check for initialization after declaration.
            EXPECT(expected, "=", &state);
            if (expected.found) {

              Node **local_result = &result;
              if (stack) { local_result = &stack->result; }

              // reassign
              // |-- lhs
              // `-- value_expression
              // reassign == lhs = value_expression

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
            // Symbol is unknown and is not followed by an assignment operator `:`.
            printf("Symbol: \"%s\"\n", node_text(symbol));
            ERROR_PREP(err, ERROR_SYNTAX, "Unknown symbol");
            return err;
          }
        }
        free(var_binding);
      }
    }

    // NOTE: We often need to continue from here.

    int found = 0;
    err = parse_binary_infix_operator(context, stack, &state, &found,
                                      &working_precedence, &working_result,
                                      result);
    if (found) { continue; }

    // If no more parser stack, return with current result.
    // Otherwise, handle parser stack operator.
    if (!stack) { break; }

    int status = 1;
    do {
      err = handle_stack_operator(&status, &context, &stack, &state,
                                  &working_result, result,
                                  &working_precedence);
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
