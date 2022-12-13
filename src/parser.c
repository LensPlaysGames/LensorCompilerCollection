#include <parser.h>

#include <error.h>
#include <environment.h>
#include <file_io.h>
#include <inttypes.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

/// Parser error.
/// FIXME: Too many of these. Clean this up a bit.
#define ERR_AT_CONTEXT(loc, context, ...)                                                              \
  do {                                                                                \
    issue_diagnostic(DIAG_ERR, (context)->filename, (context)->source, (loc), __VA_ARGS__); \
    return false;                                                                     \
  } while (0)
#define ERR_AT_STATE_CTX(state, context, ...) ERR_AT(token_span((context)->source.data, (state)->current), __VA_ARGS__)
#define ERR_AT_STATE(state, ...) ERR_AT(token_span((context)->source.data, (state)->current), __VA_ARGS__)
#define ERR_CTX(context, ...) ERR_AT_CONTEXT(token_span((context)->source.data, *(state)->current), (context), __VA_ARGS__)
#define ERR_AT(loc, ...) ERR_AT_CONTEXT(loc, context, __VA_ARGS__)
#define ERR(...) ERR_CTX(context, __VA_ARGS__)

//================================================================ BEG lexer

// TODO: Allow multi-byte comment delimiters.
const char *comment_delimiters = ";#";
const char *whitespace         = " \t\r\n";
// TODO: Think harder about delimiters.
const char *delimiters         = " \t\r\n,{}()[]<>:&|~!@";

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
void lex(char *source, Token *token) {
  if (!source || !token) ICE("Cannot lex from NULL source or token.");
  token->beginning = source;
  token->beginning += strspn(token->beginning, whitespace);
  token->end = token->beginning;
  if (*(token->end) == '\0') return;
  // Check if current line is a comment, and skip past it.
  while (comment_at_beginning(*token)) {
    // Skip to next newline.
    token->beginning = strpbrk(token->beginning, "\n");
    if (!token->beginning) {
      // If last line of file is comment, we're done lexing.
      token->end = token->beginning;
      return;
    }
    // Skip to beginning of next token after comment.
    token->beginning += strspn(token->beginning, whitespace);
    token->end = token->beginning;
  }
  if (*(token->end) == '\0') { return; }
  token->end += strcspn(token->beginning, delimiters);
  if (token->end == token->beginning) {
    token->end += 1;
  }
}

/** Keep beginning of token the same, but extend the end by another token.
 *
 * Begin lexing at the end of the given token.
 */
/// Currently unused
/*void lex_extend(Token *token) {
  Token new_token;
  lex(token->end, &new_token);
  token->end = new_token.end;
}*/

bool token_string_equalp(const char* str, Token *token) {
  if (!str || !token) { return 0; }
  char *beg = token->beginning;
  while (*str && token->beginning < token->end) {
    if (*str != *beg) {
      return false;
    }
    str++;
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

/// Get the location of a token.
loc token_span(const char* source, Token tok) {
  loc location;
  location.start = (u32)(tok.beginning - source);
  location.end   = (u32)(tok.end - source);
  return location;
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
  STATIC_ASSERT(NODE_TYPE_MAX == 17, "node_compare() must handle all node types");

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
  default: ICE("Unhandled AST node comparison of type %d\n", a->type);
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
static NODISCARD bool define_type(Environment *types, int type, Node *type_symbol, long long byte_size) {
  ASSERT(types, "Can not add type to NULL types environment");
  ASSERT(type_symbol, "Can not add NULL type symbol to types environment");
  ASSERT(byte_size >= 0, "Can not define new type with zero or negative byte size");

  Node *size_node = node_allocate();
  size_node->type = NODE_TYPE_INTEGER;
  size_node->value.integer = byte_size;

  Node *type_node = node_allocate();
  type_node->type = type;
  type_node->children = size_node;

  return environment_set(types, type_symbol, type_node) == 1;
}

#define NODE_TEXT_BUFFER_SIZE 512
char node_text_buffer[512];
char *node_text(Node *node) {
  STATIC_ASSERT(NODE_TYPE_MAX == 17, "print_node() must handle all node types");
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
  case NODE_TYPE_NOT:
    snprintf(node_text_buffer, NODE_TEXT_BUFFER_SIZE, "NOT");
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
  case NODE_TYPE_WHILE:
    snprintf(node_text_buffer, NODE_TEXT_BUFFER_SIZE, "WHILE");
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
  size_t indent_it;

#define INDENT()                            \
  indent_it = indent;                       \
  while (indent_it--) { putchar(' '); }

  INDENT();
  if (top->creates_stackframe) {
    printf("NEW STACKFRAME==================================================\n");
  } else {
    printf("NEW SCOPE=======================\n");
  }

  INDENT();
  printf("TYPES:\n");
  environment_print(*top->types,indent + 2);

  INDENT();
  printf("VARIABLES:\n");
  environment_print(*top->variables,indent + 2);

  if (top->parent == NULL) {
    INDENT();
    printf("BINARY OPERATORS:\n");
    environment_print(*top->binary_operators,indent + 2);
  }

  INDENT();
  printf("FUNCTIONS:\n");
  environment_print(*top->functions,indent);

  ParsingContext *child = top->children;
  while (child) {
    parse_context_print(child,indent + 2);
    child = child->next_child;
  }
#undef INDENT
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

ParsingContext *parse_context_create(ParsingContext *parent, CreatesStackframe creates_stackframe) {
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
  ctx->creates_stackframe = (bool)creates_stackframe;
  if (parent) {
    ctx->source = parent->source;
    ctx->filename = parent->filename;
  }
  return ctx;
}

ParsingContext *parse_context_default_create() {
  ParsingContext *ctx = parse_context_create(NULL, CREATES_STACKFRAME);
  bool ok = define_type(ctx->types, NODE_TYPE_INTEGER, node_symbol("integer"), sizeof(long long));
  if (!ok) ICE("Could not define primitive type 'integer'.");
  // TODO: Should we use type IDs vs type symbols?

  if (!define_binary_operator(ctx, "=", 3, "integer", "integer", "integer")  ||
      !define_binary_operator(ctx, "<", 3, "integer", "integer", "integer")  ||
      !define_binary_operator(ctx, ">", 3, "integer", "integer", "integer")  ||
      !define_binary_operator(ctx, "<<", 4, "integer", "integer", "integer") ||
      !define_binary_operator(ctx, ">>", 4, "integer", "integer", "integer") ||
      !define_binary_operator(ctx, "&", 4, "integer", "integer", "integer")  ||
      !define_binary_operator(ctx, "|", 4, "integer", "integer", "integer")  ||
      !define_binary_operator(ctx, "+", 5, "integer", "integer", "integer")  ||
      !define_binary_operator(ctx, "-", 5, "integer", "integer", "integer")  ||
      !define_binary_operator(ctx, "*", 10, "integer", "integer", "integer") ||
      !define_binary_operator(ctx, "/", 10, "integer", "integer", "integer") ||
      !define_binary_operator(ctx, "%", 10, "integer", "integer", "integer")) {
    ICE("Failed to define builtin binary operators.");
  }

  return ctx;
}

static bool at_end_of_file = false;

/// Update token, token length, and end of current token pointer.
void lex_advance(ParsingState *state) {
  if (!state || !state->current || !state->length || !state->end)
    ICE("Pointer arguments must not be NULL!");

  lex(state->current->end, state->current);
  *state->end = state->current->end;
  *state->length = (size_t) (state->current->end - state->current->beginning);
  char *end = *state->end;
  end += strspn(end, whitespace);
  if (*end == 0) at_end_of_file = true;
}

/// Returns true if the expected token was found, and sets
/// `done` to true if we’re at end of file if `done` is not NULL.
static bool lex_expect(const char *expected, ParsingState *state, bool *done) {
  if (!expected || !state || !state->current || !state->length || !state->end)
    ICE("lex_expect() must not be passed NULL pointers!");
  Token current_copy = *state->current;
  size_t length_copy = *state->length;
  char *end_copy     = *state->end;
  ParsingState state_copy = parse_state_create(&current_copy, &length_copy, &end_copy);
  lex_advance(&state_copy);
  if (length_copy == 0) {
    if (done) {
      *done = true;
      return false;
    } else {
      /// FIXME: No context here, so we can’t print the line, or the filename, or anything.
      issue_diagnostic(DIAG_ERR, "<input>", (span){.data = "", .size = 0}, (loc){0}, "Unexpected end of file.");
      exit(1);
    }
  }

  //printf("Expecting \"%s\", got \"", expected);
  //print_token(*state_copy.current);
  //printf("\"\n");

  if (token_string_equalp(expected, state_copy.current)) {
    parse_state_update_from(state, state_copy);
    return true;
  }

  return false;
}

#define EXPECT(expected) lex_expect(expected, state, NULL)

bool parse_get_type(ParsingContext *const context, Node *id, Node *result, bool may_fail) {
  // Handle pointers and functions, as they are both memory addresses.
  // This should ideally return target format dependant address size.
  if (id->pointer_indirection > 0
      || strcmp(id->value.symbol, "function") == 0
      || strcmp(id->value.symbol, "external function") == 0) {
    result->children = node_integer(8);
    return true;
  }

  if (strcmp(id->value.symbol, "array") == 0) {
    // Get size of base type!
    Node *base_type = node_allocate();
    if (!parse_get_type(context, id->children->next_child, base_type, false)) return false;
    // Size of array is equal to size of base type multiplied by capacity of array.
    result->children = node_integer(id->children->value.integer * base_type->children->value.integer);
    free(base_type);
    return true;
  }

  //printf("Searching following context for ");
  //print_node(id,0);
  //environment_print(*context->types,0);
  //putchar('\n');

  for (ParsingContext *ctx = context; ctx; ctx = ctx->parent) {
    int status = environment_get(*ctx->types, id, result);
    if (status) {
      //printf("Got thing:\n");
      //print_node(result,2);
      return true;
    }
  }

  result->type = NODE_TYPE_NONE;
  if (!may_fail) ERR_AT(id->source_location, "Type not found: \"%s\"", id->value.symbol);
  return false;
}

bool parse_get_variable(ParsingContext *const context, Node *id, Node *result, bool may_fail) {
  for (ParsingContext *ctx = context; ctx; ctx = ctx->parent) {
    int status = environment_get(*ctx->variables, id, result);
    if (status) return true;
  }

  result->type = NODE_TYPE_NONE;
  if (!may_fail) ERR_AT(id->source_location, "Variable not found: \"%s\"", id->value.symbol);
  return false;
}

NODISCARD bool parse_integer(Token *token, Node *node) {
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

/// Return true if an infix operator is found and parsing should continue, otherwise false.
static NODISCARD bool parse_binary_infix_operator
(ParsingContext *const context, ParsingStack *stack, ParsingState *state,
 long long *working_precedence, Node **working_result,
 Node *result
 )
{
  // Look ahead for a binary infix operator.
  bool found = false;
  Token current_copy = *state->current;
  size_t length_copy = *state->length;
  char *end_copy     = *state->end;
  ParsingState state_copy = parse_state_create(&current_copy, &length_copy, &end_copy);
  lex_advance(&state_copy);

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

    found = true;
  }

  // TODO: Iterate advanced token end backwards until reaching a valid binary operator.
  free(operator_symbol);
  free(operator_value);
  return found;
}

enum StackOperatorReturnValue {
  STACK_HANDLED_INVALID = 0,
  STACK_HANDLED_BREAK = 1,
  STACK_HANDLED_PARSE = 2,
  STACK_HANDLED_CHECK = 3
};

/**
 * @retval STACK_HANDLED_BREAK: Break (stack was NULL most likely)
 * @retval STACK_HANDLED_PARSE: Continue Parsing (working_result was updated, possibly stack as well)
 * @retval STACK_HANDLED_CHECK: Continue Checking (stack was updated, may need to handle it as well)
 */
static NODISCARD bool handle_stack_operator
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
    return true;
  }

  Node *operator = (*stack)->operator;
  if (!operator || operator->type != NODE_TYPE_SYMBOL)
    ICE("Parsing context operator must be symbol.");

  if (strcmp(operator->value.symbol, "lambda-body") == 0) {
    if (EXPECT("}")) {

      // TODO: Lookahead for immediate lambda function call.

      // Eat lambda context.
      *context = (*context)->parent;
      // Stack is handled
      *stack = (*stack)->parent;
      *status = STACK_HANDLED_CHECK;
      return true;
    }

    Node *next_expr = node_allocate();
    (*stack)->result->next_child = next_expr;
    (*stack)->result = next_expr;
    *working_result = next_expr;
    *status = STACK_HANDLED_PARSE;
    return true;
  }

  if (strcmp(operator->value.symbol, "lambdarameters") == 0) {
    if (EXPECT(")")) {
      // Pass stack to lambda-body
      if (!EXPECT("{")) ERR_CTX(*context, "Expected lambda body following lambda signature");

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
      return true;
    }

    // Eat the comma in-between variable declarations.
    bool done = false;
    lex_expect(",", state, &done);
    if (done) ERR_CTX(*context,"Expected another parameter definition but got EOF!");

    Node *next_expr = node_allocate();
    (*stack)->result->next_child = next_expr;
    (*stack)->result = next_expr;
    *working_result = next_expr;
    *status = STACK_HANDLED_PARSE;
    return true;
  }

  if (strcmp(operator->value.symbol, "if-condition") == 0) {
    // TODO: Maybe eventually allow multiple expressions in an if
    // condition, or something like that.
    if (EXPECT("{")) {
      Node *if_then_body = node_allocate();
      (*stack)->result->next_child = if_then_body;
      Node *if_then_first_expr = node_allocate();
      node_add_child(if_then_body, if_then_first_expr);

      // Empty if-then-body handling.
      // TODO: Maybe warn?
      if (EXPECT("}")) {

        // TODO: First check for else...

        *context = (*context)->parent;

        *stack = (*stack)->parent;
        *status = STACK_HANDLED_CHECK;
        return true;
      }

      // TODO: Don't leak stack->operator.
      (*stack)->operator = node_symbol("if-then-body");
      (*stack)->body = if_then_body;
      (*stack)->result = if_then_first_expr;

      *working_result = if_then_first_expr;
      *status = STACK_HANDLED_PARSE;
      return true;
    }
    // TODO/FIXME: Ask the user how to proceed when if has no body.
    ERR_CTX(*context, "Expected `{` after `if` condition. `if` expression requires a \"then\" body.");
  }

  if (strcmp(operator->value.symbol, "while-body") == 0) {
    // Evaluate next expression unless it's a closing brace.
    bool done = false;
    if (lex_expect("}", state, &done) || done) {
      *context = (*context)->parent;
      *stack = (*stack)->parent;
      *status = STACK_HANDLED_CHECK;
      return true;
    }
    Node *next_expr = node_allocate();
    (*stack)->result->next_child = next_expr;
    (*stack)->result = next_expr;
    *working_result = next_expr;
    *status = STACK_HANDLED_PARSE;
    return true;
  }

  if (strcmp(operator->value.symbol, "while-condition") == 0) {
    // TODO: Maybe eventually allow multiple expressions in an if
    // condition, or something like that.
    if (EXPECT("{")) {
      Node *while_body = node_allocate();
      (*stack)->result->next_child = while_body;
      Node *while_body_first_expr = node_allocate();
      node_add_child(while_body, while_body_first_expr);

      // Empty while-body handling.
      // TODO: Maybe warn?
      if (EXPECT("}")) {
        *context = (*context)->parent;
        *stack = (*stack)->parent;
        *status = STACK_HANDLED_CHECK;
        return true;
      }

      // TODO: Don't leak stack->operator.
      (*stack)->operator = node_symbol("while-body");
      (*stack)->body = while_body;
      (*stack)->result = while_body_first_expr;

      *working_result = while_body_first_expr;
      *status = STACK_HANDLED_PARSE;
      return true;
    }
    // TODO/FIXME: Ask the user how to proceed when if has no body.
    ERR_CTX(*context, "Expected `{` after `if` condition. `if` expression requires a \"then\" body.");
  }

  if (strcmp(operator->value.symbol, "if-then-body") == 0) {
    // Evaluate next expression unless it's a closing brace.
    if (EXPECT("}")) {
      // Eat if-then-body context.
      *context = (*context)->parent;

      // Lookahead for else then parse if-else-body.
      if (EXPECT("else")) {
        if (EXPECT("{")) {

          *context = parse_context_create(*context, DOESNT_CREATE_STACKFRAME);

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
          return true;
        }

        ERR_CTX(*context, "`else` must be followed by body.");
      }

      *stack = (*stack)->parent;
      *status = STACK_HANDLED_CHECK;
      return true;
    }
    Node *next_expr = node_allocate();
    (*stack)->result->next_child = next_expr;
    (*stack)->result = next_expr;
    *working_result = next_expr;
    *status = STACK_HANDLED_PARSE;
    return true;
  }

  if (strcmp(operator->value.symbol, "if-else-body") == 0) {
    // Evaluate next expression unless it's a closing brace.
    bool done = false;
    if (lex_expect("}", state, &done) || done) {
      *context = (*context)->parent;
      *stack = (*stack)->parent;
      *status = STACK_HANDLED_CHECK;
      return true;
    }
    Node *next_expr = node_allocate();
    (*stack)->result->next_child = next_expr;
    (*stack)->result = next_expr;
    *working_result = next_expr;
    *status = STACK_HANDLED_PARSE;
    return true;
  }

  if (strcmp(operator->value.symbol, "funcall") == 0) {
    if (EXPECT(")")) {
      *stack = (*stack)->parent;
      bool found = parse_binary_infix_operator(*context, *stack, state,
                                        working_precedence, working_result,
                                        result);
      *status = found ? STACK_HANDLED_PARSE : STACK_HANDLED_CHECK;
      return true;
    }

    bool done = false;
    lex_expect(",", state, &done);
    if (done) {
      print_token(*state->current);
      ERR_CTX(*context, "Parameter list expected closing parenthesis or comma for another parameter");
    }

    Node *next_expr = node_allocate();
    (*stack)->result->next_child = next_expr;
    (*stack)->result = next_expr;
    *working_result = next_expr;
    *status = STACK_HANDLED_PARSE;
    return true;
  }

  *status = STACK_HANDLED_INVALID;
  ICE("Invalid stack operator: \"%s\"\n", (*stack)->operator->value.symbol);
}

bool parse_base_type
(ParsingContext *context,
 ParsingState *state,
 Node *type,
 bool may_fail
 )
{
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
    lex_advance(&state_copy);
    if (length_copy == 0) {
      if (may_fail) return false;
      ERR("Expected a valid type following pointer type declaration symbol: `@`");
    }
  }

  Node *type_symbol =
    node_symbol_from_buffer(state_copy.current->beginning, *state_copy.length);
  *type = *type_symbol;
  type->pointer_indirection = indirection_level;

  Node *type_validator = node_allocate();
  if (!parse_get_type(context, type_symbol, type_validator, may_fail)) {
    /*if (may_fail) return false;
    ERR("Unknown type: %s", type_symbol->value.symbol);*/
    return false;
  }
  if (nonep(*type_validator)) {
    if (may_fail) return false;
    ERR("Invalid type in variable declaration: \"%s\"", type_symbol->value.symbol);
  }
  free(type_symbol);
  free(type_validator);

  parse_state_update_from(state, state_copy);
  return true;
}

bool parse_type(ParsingContext *context, ParsingState *state, Node *type, Node *parameter_names) {
  bool external = 0;

  Node *parameter_names_last = NULL;

  // Parse type prefix keywords.

  // If state is at EXT, then advance lexer and mark external.
  if (external = token_string_equalp("ext", state->current), external) {
    lex_advance(state);
    if (*state->length == 0)
      ERR("Expected base type following \"ext\" type annotation keyword.");
  }

  if (!parse_base_type(context, state, type, false)) return false;

  // Parse function signature as type (instead of custom)

  if (EXPECT("(")) {
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
      bool done = false;
      bool found = lex_expect(")", state, &done);
      if (done) ERR("Expected closing paren, `)`, at end of function signature type");
      if (found) break;

      // Parse parameter name
      lex_advance(state);
      if (*state->length == 0) ICE("I hope we never see this error");

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

      if (!EXPECT(":")) ERR("Expected type annotation operator following function parameter name");

      lex_advance(state);
      if (*state->length == 0) ERR("Expected type following type annotation operator for function parameter");
      Node *parameter_type = node_allocate();
      if (!parse_type(context, state, parameter_type, NULL)) return false;

      // With finished parameter, add as child to function, or add to function context?
      node_add_child(type, parameter_type);
    }

    return true;
  }

  if (external) {
    // This is a non-function "ext" type.
    issue_diagnostic(DIAG_SORRY, context->filename, context->source,
      token_span(context->source.data, *state->current),
      "\"ext\" may only be used on function types (for now).");
  }

  if (EXPECT("[")) {
    // Copy base type
    Node *base_type = node_allocate();
    node_copy(type, base_type);
    free(type->value.symbol);
    // Create array type
    Node *array_type = node_symbol("array");
    *type = *array_type;
    free(array_type);

    // Parse size integer
    lex_advance(state);

    Node *array_size = node_allocate();
    if (!parse_integer(state->current, array_size))
      ERR("Array type annotation expects array size (an integer) after opening '['.");

    node_add_child(type, array_size);

    if (!EXPECT("]")) ERR("Array type annotation must have closing ']' after array size.");

    node_add_child(type, base_type);
  }

  return true;
}


bool parse_expr
(ParsingContext *context,
 char *source,
 char **end,
 Node *result
 )
{
  ParsingStack *stack = NULL;

  size_t token_length = 0;
  Token current_token;
  current_token.beginning  = source;
  current_token.end        = source;

  ParsingState _state = parse_state_create(&current_token, &token_length, end);
  ParsingState *const state = &_state;
  Node *working_result = result;
  long long working_precedence = 0;

  for (;;) {
    //printf("lexed: "); print_token(current_token); putchar('\n');
    lex_advance(state);
    if (token_length == 0) { return true; }

    if (parse_integer(&current_token, working_result)) {

    } else {

      // Check for lambda
      Node *type = node_allocate();
      /// TODO: Construct error and return it rather than printing it immediately
      if (parse_base_type(context, state, type, true)) {

        context = parse_context_create(context, CREATES_STACKFRAME);

        Node *lambda = working_result;
        lambda->type = NODE_TYPE_FUNCTION;
        node_add_child(lambda, type);

        if (!EXPECT("(")) {
          // Nothing except a lambda starts with a type annotation.
          ERR("Expected parameter declaration(s) following lambda return type");
        }

        Node *parameters = node_allocate();
        node_add_child(lambda, parameters);

        if (EXPECT(")")) {
          if (!EXPECT("{")) ERR("Expected lambda body following lambda signature");

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
        Node *symbol = node_symbol_from_buffer(current_token.beginning, token_length);

        // FIXME: Experimental and not implemented in typechecker or codegen backends yet.
        // NOTE: Square bracket syntax is for sure experimental, and likely temporary.
        if (strcmp("[", symbol->value.symbol) == 0) {
          working_result->type = NODE_TYPE_CAST;

          lex_advance(state);
          Node *cast_type = node_allocate();
          if (!parse_base_type(context, state, cast_type, false)) return false;

          node_add_child(working_result, cast_type);

          if (!EXPECT("]")) ERR("Expected ']' after type in cast");

          Node *child = node_allocate();
          node_add_child(working_result, child);
          working_result = child;
          continue;
        }

        if (strcmp("~", symbol->value.symbol) == 0) {
          working_result->type = NODE_TYPE_NOT;
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

          context = parse_context_create(context, DOESNT_CREATE_STACKFRAME);

          stack = parse_stack_create(stack);
          stack->operator = node_symbol("if-condition");
          stack->result = condition_expression;

          working_result = condition_expression;
          continue;
        }

        if (strcmp("while", symbol->value.symbol) == 0) {
          Node *while_node = working_result;
          while_node->type = NODE_TYPE_WHILE;
          Node *condition_expression = node_allocate();
          node_add_child(while_node, condition_expression);

          context = parse_context_create(context, DOESNT_CREATE_STACKFRAME);

          stack = parse_stack_create(stack);
          stack->operator = node_symbol("while-condition");
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
        bool found = parse_get_variable(context, symbol, var_binding, true);
        if (!nonep(*var_binding)) {
          if (!found) ERR("Unknown variable: \"%s\"", symbol->value.symbol);

          // Create variable access node.
          working_result->type = NODE_TYPE_VARIABLE_ACCESS;
          working_result->value.symbol = strdup(symbol->value.symbol);

          // Break if we have a variable at end of file.
          if (at_end_of_file) goto end_of_file;

          // Lookahead for function call
          if (EXPECT("(")) {
            // This is a function call!

            Node *var_access = node_allocate();
            node_copy(working_result, var_access);
            working_result->type = NODE_TYPE_FUNCTION_CALL;
            working_result->value.integer = 0;
            node_add_child(working_result, var_access);

            Node *parameters = node_allocate();
            node_add_child(working_result, parameters);

            if (!EXPECT(")")) {
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
            if (EXPECT("[")) {
              Node *var_access = node_allocate();
              node_copy(working_result, var_access);
              working_result->type = NODE_TYPE_INDEX;
              node_add_child(working_result, var_access);

              lex_advance(state);
              Node *integer_offset = node_allocate();
              if (parse_integer(state->current, integer_offset) == 0)
                ERR("Expected integer literal in subscript");

              working_result->value.integer = integer_offset->value.integer;
              node_free(integer_offset);

              if (!EXPECT("]")) ERR("Missing ']' in subscript");
            }

            // Break if we have a subscript at end of file.
            if (at_end_of_file) goto end_of_file;

            // Lookahead for variable reassignment.
            if (EXPECT(":") && EXPECT("=")) {
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
          if (EXPECT(":")) {
            if (EXPECT("=")) ERR("Invalid Variable Symbol: \"%s\"", symbol->value.symbol);

            lex_advance(state);
            if (token_length == 0) { break; }
            Node *decl_type = node_allocate();
            Node *param_names = node_allocate();
            if (!parse_type(context, state, decl_type, param_names)) return false;

            Node *variable_binding = node_allocate();
            if (environment_get(*context->variables, symbol, variable_binding))
              ERR("Redefinition of variable: \"%s\"", symbol->value.symbol);
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
            if (status != 1)
              ICE("Failed to define variable: \"%s\", status: %d", symbol_for_env->value.symbol, status);

            if (strcmp(decl_type->value.symbol, "function") == 0) {
              if (EXPECT("{")) {
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

                context = parse_context_create(context, CREATES_STACKFRAME);

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
                  if (v_status != 1)
                    ICE("Failed to define parameter variable: \"%s\", status: %d\n", param_symbol_for_env->value.symbol, v_status);

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
            if (EXPECT("=")) {
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
            ERR("Unknown symbol: \"%s\".", node_text(symbol));
          }
        }
        free(var_binding);
      }
    }

    // NOTE: We often need to continue from here.
  end_of_file:;

    bool found = parse_binary_infix_operator(context, stack, state,
      &working_precedence, &working_result,
      result);
    if (found) { continue; }

    // If no more parser stack, return with current result.
    // Otherwise, handle parser stack operator.
    if (!stack) { break; }

    int status = 1;
    do {
      if (!handle_stack_operator(&status, &context, &stack, state,
             &working_result, result,
             &working_precedence)) return false;
    } while (status == STACK_HANDLED_CHECK);

    if (status == STACK_HANDLED_BREAK) break;
    if (status == STACK_HANDLED_PARSE) continue;

    ICE("Unhandled parse stack operator. Status: %d", status);
  }
  return true;
}


bool parse_program(const char *filepath, ParsingContext *context, Node *result) {
  string contents = file_contents(filepath);
  if (!contents.data) ICE("parse_program(): Could not read file: \"%s\"", filepath);
  result->type = NODE_TYPE_PROGRAM;
  context->filename = filepath;
  context->source = (span){.data = contents.data, .size = contents.size};
  char *contents_it = contents.data;
  for (;;) {
    Node *expression = node_allocate();
    node_add_child(result, expression);
    if (!parse_expr(context, contents_it, &contents_it, expression)) {
      free(contents.data);
      return false;
    }

    // Check for end-of-parsing case (source and end are the same).
    if (!(*contents_it)) { break; }

    //printf("Parsed expression:\n");
    //print_node(expression,0);
    //putchar('\n');

  }
  free(contents.data);
  return true;
}

bool define_binary_operator
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
  return environment_set(context->binary_operators, node_symbol(operator), binop) != 0;
}
