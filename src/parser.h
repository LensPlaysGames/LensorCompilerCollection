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
  ///    1. Name Symbol
  ///    2. Type Symbol
  /// 2. Return Type Symbol
  /// 3. Expression List (Program)
  NODE_TYPE_FUNCTION,

  /// Contains two children.
  /// 1. SYMBOL (VARIABLE IDENTIFIER)
  /// 2. INITIALIZE EXPRESSION, or None.
  NODE_TYPE_VARIABLE_DECLARATION,
  NODE_TYPE_VARIABLE_DECLARATION_INITIALIZED,

  /// Contains two children.
  /// 1. SYMBOL (VARIABLE IDENTIFIER)
  /// 2. VALUE EXPRESSION
  NODE_TYPE_VARIABLE_REASSIGNMENT,

  /// Contains two children that determine left and right acceptable
  /// types.
  NODE_TYPE_BINARY_OPERATOR,

  /// Contains a list of expressions to execute in sequence.
  NODE_TYPE_PROGRAM,

  NODE_TYPE_MAX,
} NodeType;

typedef struct Node {
  // TODO: Think about how to document node types and how they fit in the AST.
  int type;
  union NodeValue {
    long long integer;
    char *symbol;
  } value;
  // Possible TODO: Parent?
  struct Node *children;
  struct Node *next_child;
} Node;

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

// TODO: Separate context from stack.
typedef struct ParsingStack {
  struct ParsingStack *parent;
  Node *operator;
  Node *result;
} ParsingStack;

// FIXME: Should this be an environment that contains other environments and things?
typedef struct ParsingContext {
  struct ParsingContext *parent;
  /// Used for stack continuation while parsing
  Node *operator;
  Node *result;
  /// TYPE
  /// `-- SYMBOL (IDENTIFIER) -> TYPE (NODE_TYPE)
  ///                            `-- BYTE_SIZE (N)
  Environment *types;
  /// VARIABLE
  /// `-- SYMBOL (NAME) -> SYMBOL (TYPE)
  Environment *variables;
  /// VARIABLE
  /// `-- SYMBOL (NAME) -> FUNCTION
  Environment *functions;
} ParsingContext;

Error parse_get_type(ParsingContext *context, Node *id, Node *result);

ParsingContext *parse_context_create(ParsingContext *parent);
ParsingContext *parse_context_default_create();

Error parse_expr(ParsingContext *context,
                 char *source, char **end,
                 Node *result);

#endif /* COMPILER_PARSER_H */
