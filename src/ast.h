#ifndef FUNCOMPILER_AST_H
#define FUNCOMPILER_AST_H

#include <codegen/codegen_forward.h>
#include <error.h>
#include <stdio.h>
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
};

/// The kind of a type.
enum TypeKind {
  TYPE_PRIMITIVE,
  TYPE_NAMED,
  TYPE_POINTER,
  TYPE_ARRAY,
  TYPE_FUNCTION,
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
  TK_AS,

  TK_LPAREN,
  TK_RPAREN,
  TK_LBRACK,
  TK_RBRACK,
  TK_LBRACE,
  TK_RBRACE,

  TK_COMMA,
  TK_COLON,
  TK_SEMICOLON,
  TK_DOT,

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
  TK_HASH,

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
typedef struct Type Type;
typedef struct Parameter Parameter;
typedef struct Scope Scope;
typedef Vector(Node *) Nodes;
typedef Vector(Type *) Types;
typedef Vector(Parameter) Parameters;

/// A symbol in the symbol table.
typedef struct Symbol {
  /// The type of the symbol.
  enum SymbolKind kind;

  /// The name of the symbol.
  string name;

  /// The scope in which the symbol is defined.
  Scope *scope;

  /// The actual value of the symbol.
  union {
    Node *node;
    Type *type;
  };
} Symbol;

/// A scope in the AST.
struct Scope {
  /// The parent scope.
  struct Scope *parent;

  /// The symbols in this scope.
  Vector(Symbol *) symbols;

  /// All child scopes.
  Vector(Scope *) children;
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
  Nodes param_decls;
  Node *body;
  string name;
  IRFunction *ir;
  bool global : 1;
} NodeFunction;

/// Variable declaration.
typedef struct NodeDeclaration {
  Node *init;
  string name;
  bool static_;
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
    usz string_index;
  };
} NodeLiteral;

/// Function refernece.
typedef struct NodeFunctionReference {
  string name;
  Symbol *resolved;
  Scope *scope;
} NodeFunctionReference;

/// Variable reference.
typedef Symbol *NodeVariableReference;

/// Named type.
typedef Symbol *TypeNamed;

/// Primitive type.
typedef struct TypePrimitive {
  usz size;
  usz alignment;
  span name;
  bool is_signed;
  uint8_t id;
} TypePrimitive;

/// Pointer type.
typedef struct TypePointer {
  Type *to;
} TypePointer;

/// Array type.
typedef struct TypeArray {
  Type *of;
  size_t size;
} TypeArray;

/// A function parameter.
struct Parameter {
  Type *type;
  string name;
  loc source_location;
};

/// Function type.
typedef struct TypeFunction {
  Parameters parameters;
  Type *return_type;
} TypeFunction;

/// A type.
struct Type {
  /// The kind of the type.
  enum TypeKind kind;

  /// Location of the type in the source code.
  loc source_location;

  /// The actual type data.
  union {
    TypePrimitive primitive;
    TypeNamed named;
    TypePointer pointer;
    TypeArray array;
    TypeFunction function;
  };
};

/// A node in the AST.
struct Node {
  /// Type of the node.
  enum NodeKind kind;

  /// Location of the node.
  loc source_location;

  /// The cached type of the node.
  Type *type;

  /// The parent node.
  Node *parent;

  /// The IR instruction that this node is compiled to.
  IRInstruction *ir;

  /// Various flags.
  bool type_checked : 1; /// Whether this node has been type checked.
  bool emitted : 1;      /// Whether this node has been emitted.

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

  /// All nodes/types/scopes in the AST. NEVER iterate over this, ever.
  Nodes _nodes_;
  Types _types_;
  Vector(Scope *) _scopes_;

  /// Counter used for generating unique names.
  usz counter;

  /// Builtin types.
  Type *t_void;
  Type *t_integer_literal;
  Type *t_integer;
  Type *t_byte;

  /// Scopes that are currently being parsed.
  Vector(Scope *) scope_stack;

  /// String table.
  Vector(string) strings;

  /// Functions.
  Vector(Node *) functions;
} AST;

/// ===========================================================================
///  Scope/symbol functions.
/// ===========================================================================
/// Push a new scope.
void scope_push(AST *ast);

/// Pop the current scope. This does *not* delete the scope.
void scope_pop(AST *ast);

/// Add an empty symbol to a scope, no matter what.
/// \return The symbol that was added.
Symbol *scope_add_symbol_unconditional(Scope *scope, enum SymbolKind kind, span name, void *value);

/// Add an empty symbol to a scope.
/// \return The symbol that was added, or NULL if the symbol already exists.
Symbol *scope_add_symbol(Scope *scope, enum SymbolKind kind, span name, void *value);

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
    Type *type,
    Nodes param_decls,
    Node *body,
    span name
);

/// Create a new declaration node.
Node *ast_make_declaration(
    AST *ast,
    loc source_location,
    Type *type,
    span name,
    Node *init
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
    Type *to,
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
    span symbol
);

/// Create a new named type.
Type *ast_make_type_named(
    AST *ast,
    loc source_location,
    Symbol *symbol
);

/// Create a new pointer type.
Type *ast_make_type_pointer(
    AST *ast,
    loc source_location,
    Type *to
);

/// Create a new array type.
Type *ast_make_type_array(
    AST *ast,
    loc source_location,
    Type *of,
    size_t size
);

/// Create a new function type.
Type *ast_make_type_function(
    AST *ast,
    loc source_location,
    Type *return_type,
    Parameters parameters
);

/// ===========================================================================
///  AST query functions.
/// ===========================================================================
/// Info about a type.
typedef struct Typeinfo {
  /// The type stripped of aliases etc. May be NULL
  /// if the type is incomplete.
  Type *type;

  /// The last alias in the chain.
  Type *last_alias;

  /// Flags.
  bool is_void : 1;
  bool is_incomplete : 1;
} Typeinfo;

/// Get a string representation of a type.
/// \return The string representation of the type. The string is allocated
///         as if with `malloc` and must be freed by the caller.
string ast_typename(const Type *type, bool colour);

/// Check if a type is incomplete.
bool ast_type_is_incomplete(const Type *type);

/// Get the size of a type.
usz ast_sizeof(const Type *type);

/// Get information about a type.
Typeinfo ast_typeinfo(AST *ast, Type *type);

/// Check if a type is void.
bool ast_is_void(AST *ast, Type *type);

/// ===========================================================================
///  Miscellaneous AST functions.
/// ===========================================================================
/// Create a new AST.
AST *ast_create();

/// Free an AST.
void ast_free(AST *ast);

/// Print an AST.
void ast_print(FILE *file, const AST *ast);

/// Print the scope tree of an AST.
void ast_print_scope_tree(FILE *file, const AST *ast);

/// Intern a string.
size_t ast_intern_string(AST *ast, span string);

/// Replace a node with another node.
void ast_replace_node(AST *ast, Node *old, Node *new);

#endif // FUNCOMPILER_AST_H
