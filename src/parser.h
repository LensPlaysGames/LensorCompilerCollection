#ifndef COMPILER_PARSER_H
#define COMPILER_PARSER_H

#include <stddef.h>
#include <error.h>

typedef struct Environment Environment;

typedef struct Token {
  char *beginning;
  char *end;
} Token;

void print_token(Token t);
Error lex(char *source, Token *token);

typedef enum NodeType {
  // BEGIN NULL DENOTATION TYPES

  /// The definition of nothing; false, etc.
  NODE_TYPE_NONE = 0,

  /// Just an integer.
  NODE_TYPE_INTEGER,

  /// When a literal is expected but no other literal is valid, it
  /// becomes a symbol.
  NODE_TYPE_SYMBOL,

  // END NULL DENOTATION TYPES

  /// Contains three children.
  /// 1. Parameter List
  ///      Var. Decl.
  ///        <name symbol>
  ///        <type symbol>
  /// 2. Return Type
  /// 3. Expression List (Program)
  NODE_TYPE_FUNCTION,
  /// Contains two children
  /// 1. Function Symbol
  /// 2. Parameter List
  NODE_TYPE_FUNCTION_CALL,

  /// Contains one child.
  /// 1. SYMBOL (VARIABLE IDENTIFIER)
  NODE_TYPE_VARIABLE_DECLARATION,

  /// Contains variable symbol in value.
  NODE_TYPE_VARIABLE_ACCESS,

  /// Contains two children.
  /// 1. SYMBOL (VARIABLE IDENTIFIER)
  /// 2. VALUE EXPRESSION
  NODE_TYPE_VARIABLE_REASSIGNMENT,

  /// Contains two children.
  /// 1. Condition Expression
  /// 2. "THEN" Expression List
  /// TODO: 3. "ELSE" Expression List
  NODE_TYPE_IF,

  NODE_TYPE_ADDRESSOF,
  NODE_TYPE_DEREFERENCE,

  /// A valid binary operator. Contains two children.
  /// 1. Left Hand Side, often abbreviated as LHS
  /// 1. Right Hand Side, often abbreviated as RHS
  NODE_TYPE_BINARY_OPERATOR,

  /// Contains a list of expressions to execute in sequence.
  NODE_TYPE_PROGRAM,

  NODE_TYPE_MAX,
} NodeType;

typedef struct Node {
  int type;
  union NodeValue {
    long long integer;
    char *symbol;
  } value;
  struct Node *parent;
  struct Node *children;
  struct Node *next_child;
  /// Used during codegen to store result RegisterDescriptor.
  int result_register;
  unsigned int pointer_indirection;
} Node;

char *node_text(Node *node);

Node *node_allocate();

#define nonep(node)    ((node).type == NODE_TYPE_NONE)
#define integerp(node) ((node).type == NODE_TYPE_INTEGER)
#define symbolp(node)  ((node).type == NODE_TYPE_SYMBOL)

/// PARENT is modified, NEW_CHILD pointer is used verbatim.
void node_add_child(Node *parent, Node *new_child);

/// @return Boolean-like value; 1 for success, 0 for failure.
int node_compare(Node *a, Node *b);

/// Create a new node with integer type and given value.
Node *node_integer(long long value);

// TODO: Think about caching used symbols and not creating duplicates!
/// Create a new node with symbol type and given value.
Node *node_symbol(char *symbol_string);

/// Create a new node with symbol type and value copied from given buffer.
Node *node_symbol_from_buffer(char *buffer, size_t length);

void print_node(Node *node, size_t indent_level);

void node_free(Node *root);

/// Copy A into B, asserting allocations.
void node_copy(Node *a, Node *b);

// @return Boolean-like value; 1 for success, 0 for failure.
int token_string_equalp(char* string, Token *token);

/// @return Boolean-like value; 1 upon success, 0 for failure.
int parse_integer(Token *token, Node *node);

typedef struct ParsingState {
  Token *current;
  size_t *length;
  char **end;
} ParsingState;

ParsingState parse_state_create(Token *token, size_t *length, char **end);
void parse_state_update(ParsingState *state, Token token, size_t length, char *end);
void parse_state_update_from(ParsingState *state, ParsingState new_state);

// TODO: Separate context from stack.
typedef struct ParsingStack {
  struct ParsingStack *parent;
  Node *operator;
  Node *result;
  Node *body;
} ParsingStack;

// TODO: Shove ParsingContext within an AST Node.
typedef struct ParsingContext {
  /// Used for upward scope searching, mainly.
  struct ParsingContext *parent;
  /// Used for entering scopes as different stages of the compiler
  /// iterate and operate on the AST.
  struct ParsingContext *children;
  struct ParsingContext *next_child;
  /// TYPE
  /// `-- SYMBOL (IDENTIFIER) -> TYPE (NODE_TYPE)
  ///                            `-- BYTE_SIZE (N)
  Environment *types;
  /// VARIABLE
  /// `-- SYMBOL (NAME) -> SYMBOL (TYPE)
  Environment *variables;
  /// FUNCTION
  /// `-- SYMBOL (NAME) -> FUNCTION
  Environment *functions;
  /// BINARY INFIX OPERATOR
  /// `-- SYMBOL (OPERATOR) -> NONE
  ///                          `-- INTEGER (PRECEDENCE)
  ///                              -> SYMBOL (RETURN TYPE)
  ///                              -> SYMBOL (LHS TYPE)
  ///                              -> SYMBOL (RHS TYPE)
  Environment *binary_operators;
} ParsingContext;

void parse_context_print(ParsingContext *top, size_t indent);

/// PARENT is modified, CHILD is used verbatim.
void parse_context_add_child(ParsingContext *parent, ParsingContext *child);

Error define_binary_operator
(ParsingContext *context,
 char *operator,
 int precedence,
 char *return_type, char *lhs_type, char *rhs_type);

Error parse_type (ParsingContext *context, ParsingState *state, Node *type);

/** Get the value of a type symbol/ID in types environment.
 *  Return an error if type is not a valid symbol/ID found in context.
 */
Error parse_get_type(ParsingContext *context, Node *id, Node *result);

/** Get the value of a variable symbol/ID in variables environment.
 *  Return an error if variable is not a valid symbol/ID found in context.
 */
Error parse_get_variable(ParsingContext *context, Node *id, Node *result);

ParsingContext *parse_context_create(ParsingContext *parent);
ParsingContext *parse_context_default_create();

Error parse_program(char *filepath, ParsingContext *context, Node *result);

Error parse_expr(ParsingContext *context,
                 char *source, char **end,
                 Node *result);

#endif /* COMPILER_PARSER_H */
