#ifndef FUNCOMPILER_AST_H
#define FUNCOMPILER_AST_H

#include <codegen/codegen_forward.h>
#include <error.h>
#include <vector.h>

/// ===========================================================================
///  Enums.
/// ===========================================================================
/// The type of an AST node.
enum NodeKind {
  NODE_ROOT,
  NODE_FUNCTION,
  NODE_DECLARATION,
  NODE_IF,
  NODE_WHILE,
  NODE_BLOCK,
  NODE_CALL,
  NODE_CAST,
  NODE_BINARY,
  NODE_UNARY,
  NODE_LITERAL,
  NODE_VARIABLE_REFERENCE,
  NODE_FUNCTION_REFERENCE,

  NODE_TYPE_PRIMITIVE,
  NODE_TYPE_NAMED,
  NODE_TYPE_POINTER,
  NODE_TYPE_ARRAY,
  NODE_TYPE_FUNCTION,
};

/// The type of a token. These are only in this header
/// because some of them are also used in the AST.
enum TokenType {
  TK_INVALID,
  TK_EOF,

  TK_IDENT,
  TK_NUMBER,
  TK_STRING,

  TK_IF,
  TK_ELSE,
  TK_WHILE,
  TK_EXT,

  TK_LPAREN,
  TK_RPAREN,
  TK_LBRACK,
  TK_RBRACK,
  TK_LBRACE,
  TK_RBRACE,

  TK_COMMA,
  TK_COLON,
  TK_SEMICOLON,

  TK_PLUS,
  TK_MINUS,
  TK_STAR,
  TK_SLASH,
  TK_PERCENT,
  TK_AMPERSAND,
  TK_PIPE,
  TK_CARET,
  TK_TILDE,
  TK_EXCLAM,
  TK_AT,

  TK_SHL,
  TK_SHR,

  TK_EQ,
  TK_NE,
  TK_LT,
  TK_GT,
  TK_LE,
  TK_GE,

  TK_COLON_EQ,
};

/// The type of a symbol in the symbol table.
enum SymbolKind {
  SYM_VARIABLE,
  SYM_FUNCTION,
  SYM_TYPE,
};

/// ===========================================================================
///  Symbol table.
/// ===========================================================================
/// Forward declarations.
typedef struct Node Node;
typedef struct Scope Scope;
typedef VECTOR(Node *) Nodes;

/// A symbol in the symbol table.
typedef struct Symbol {
  /// The type of the symbol.
  enum SymbolKind kind;

  /// The name of the symbol.
  string name;

  /// The scope in which the symbol is defined.
  Scope *scope;

  /// The actual value of the symbol.
  Node *value;
} Symbol;

/// A scope in the AST.
struct Scope {
  /// The parent scope.
  struct Scope *parent;

  /// The symbols in this scope.
  VECTOR(Symbol *) symbols;

  /// All child scopes.
  VECTOR(Scope *) children;
};

/// ===========================================================================
///  AST Nodes.
/// ===========================================================================

/// Root node.
typedef struct NodeRoot {
  Nodes children;
} NodeRoot;

/// Named function.
typedef struct NodeFunction {
  Node *type;
  Node *body;
  string name;
  IRFunction *ir;
} NodeFunction;

/// Variable declaration.
typedef struct NodeDeclaration {
  Node *type;
  string name;
} NodeDeclaration;

/// If expression.
typedef struct NodeIf {
  Node *condition;
  Node *then;
  Node *else_;
} NodeIf;

/// While expression.
typedef struct NodeWhile {
  Node *condition;
  Node *body;
} NodeWhile;

/// Block.
typedef struct NodeBlock {
  Nodes children;
} NodeBlock;

/// Function call.
typedef struct NodeCall {
  Node *callee;
  Nodes arguments;
} NodeCall;

/// Typecast.
typedef struct NodeCast {
  Node *to_type;
  Node *value;
} NodeCast;

/// Binary expression.
typedef struct NodeBinary {
  enum TokenType op;
  Node *lhs;
  Node *rhs;
} NodeBinary;

/// Unary expression.
typedef struct NodeUnary {
  enum TokenType op;
  bool postfix;
  Node *value;
} NodeUnary;

/// Literal.
typedef struct NodeLiteral {
  enum TokenType type;
  union {
    u64 integer;
    size_t string_index;
  };
} NodeLiteral;

/// Variable reference.
typedef Symbol *NodeVariableReference;

/// Function refernece.
typedef Symbol *NodeFunctionReference;

/// Named type.
typedef Symbol *NodeTypeNamed;

/// Primitive type.
typedef struct NodeTypePrimitive {
  usz size;
  usz alignment;
  bool is_signed;
  span name;
} NodeTypePrimitive;

/// Pointer type.
typedef struct NodeTypePointer {
  Node *to;
  usz level;
} NodeTypePointer;

/// Array type.
typedef struct NodeTypeArray {
  Node *of;
  size_t size;
} NodeTypeArray;

/// Function type.
typedef struct NodeTypeFunction {
  Nodes parameters;
  Node *return_type;
} NodeTypeFunction;

/// A node in the AST.
struct Node {
  /// Type of the node.
  enum NodeKind kind;

  /// Location of the node.
  loc source_location;

  /// The IR instruction that this node is compiled to.
  IRInstruction *ir;

  /// Node data.
  union {
    NodeRoot root;
    NodeFunction function;
    NodeDeclaration declaration;
    NodeIf if_;
    NodeWhile while_;
    NodeBlock block;
    NodeCall call;
    NodeCast cast;
    NodeBinary binary;
    NodeUnary unary;
    NodeLiteral literal;
    NodeVariableReference var;
    NodeFunctionReference funcref;
    NodeTypePrimitive type_primitive;
    NodeTypeNamed type_named;
    NodeTypePointer type_pointer;
    NodeTypeArray type_array;
    NodeTypeFunction type_function;
  };
};

/// Data structure that stores an AST.
typedef struct AST {
  /// The root node of the AST.
  Node *root;

  /// Filename of the source file and source code.
  /// TODO: If we ever allow multiple source files, then we
  ///       need to store (an index to) the source file in `loc`.
  string filename;
  string source;

  /// All nodes in the AST. NEVER iterate over this, ever.
  Nodes _nodes_;

  /// Counter used for generating unique names.
  usz counter;

  /// Builtin types.
  Node *t_integer;

  /// Scopes.
  VECTOR(Scope *) scopes;

  /// String table.
  VECTOR(string) strings;

  /// Functions.
  VECTOR(Node *) functions;
} AST;

/// ===========================================================================
///  Scope/symbol functions.
/// ===========================================================================
/// Push a new scope.
void scope_push(AST *ast);

/// Pop the current scope. This does *not* delete the scope.
void scope_pop(AST *ast);

/// Add an empty symbol to a scope.
/// \return The symbol that was added, or NULL if the symbol already exists.
Symbol *scope_add_symbol(Scope *scope, enum SymbolKind kind, span name, Node *value);

/// Find a symbol in a scope.
/// \return The symbol, or NULL if it was not found.
Symbol *scope_find_symbol(Scope *scope, span name, bool this_scope_only);

/// Find a symbol in a scope or add it if it does not exist.
/// \return The symbol.
Symbol *scope_find_or_add_symbol(Scope *scope, enum SymbolKind kind, span name, bool this_scope_only);

/// ===========================================================================
///  Functions to create ast nodes.
/// ===========================================================================
/// Create a new function node.
Node *ast_make_function(
    AST *ast,
    loc source_location,
    Node *type,
    Node *body,
    span name
);

/// Create a new declaration node.
Node *ast_make_declaration(
    AST *ast,
    loc source_location,
    Node *type,
    span name
);

/// Create a new if expression.
Node *ast_make_if(
    AST *ast,
    loc source_location,
    Node *condition,
    Node *then,
    Node *else_
);

/// Create a new while expression.
Node *ast_make_while(
    AST *ast,
    loc source_location,
    Node *condition,
    Node *body
);

/// Create a new block expression.
Node *ast_make_block(
    AST *ast,
    loc source_location,
    Nodes children
);

/// Create a new call expression.
Node *ast_make_call(
    AST *ast,
    loc source_location,
    Node *callee,
    Nodes arguments
);

/// Create a new cast expression.
Node *ast_make_cast(
    AST *ast,
    loc source_location,
    Node *to_type,
    Node *value
);

/// Create a new binary expression.
Node *ast_make_binary(
    AST *ast,
    loc source_location,
    enum TokenType op,
    Node *lhs,
    Node *rhs
);

/// Create a new unary expression.
Node *ast_make_unary(
    AST *ast,
    loc source_location,
    enum TokenType op,
    bool postfix,
    Node *value
);

/// Create a new integer literal.
Node *ast_make_integer_literal(
    AST *ast,
    loc source_location,
    u64 value
);

/// Create a new string literal.
Node *ast_make_string_literal(
    AST *ast,
    loc source_location,
    span string
);

/// Create a new variable reference.
Node *ast_make_variable_reference(
    AST *ast,
    loc source_location,
    Symbol *symbol
);

/// Create a new function reference.
Node *ast_make_function_reference(
    AST *ast,
    loc source_location,
    Symbol *symbol
);

/// Create a new named type.
Node *ast_make_type_named(
    AST *ast,
    loc source_location,
    Symbol *symbol
);

/// Create a new pointer type.
Node *ast_make_type_pointer(
    AST *ast,
    loc source_location,
    Node *to,
    usz level
);

/// Create a new array type.
Node *ast_make_type_array(
    AST *ast,
    loc source_location,
    Node *of,
    size_t size
);

/// Create a new function type.
Node *ast_make_type_function(
    AST *ast,
    loc source_location,
    Node *return_type,
    Nodes parameters
);

/// ===========================================================================
///  Miscellaneous AST functions.
/// ===========================================================================
/// Create a new AST.
AST *ast_create();

/// Free an AST.
void ast_free(AST *ast);

/// Print an AST.
void ast_print(const AST *ast);

/// Print a node.
void ast_print_node(const Node *node, size_t indent);

/// Intern a string.
size_t ast_intern_string(AST *ast, span string);

#endif // FUNCOMPILER_AST_H
