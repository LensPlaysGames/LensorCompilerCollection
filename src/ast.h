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
  NODE_STRUCTURE_DECLARATION,
  NODE_MEMBER_ACCESS,
  NODE_FOR,
};

/// The kind of a type.
enum TypeKind {
  TYPE_PRIMITIVE,
  TYPE_NAMED,
  TYPE_POINTER,
  TYPE_ARRAY,
  TYPE_FUNCTION,
  TYPE_STRUCT,
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
  TK_TYPE,
  TK_VOID,
  TK_BYTE,
  TK_INTEGER_KW,
  TK_FOR,

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
  TK_COLON_COLON,
  TK_COLON_GT,
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
typedef struct Scope Scope;
typedef Vector(Node *) Nodes;
typedef Vector(Type *) Types;


/// A function parameter.
typedef struct Parameter {
  Type *type;
  string name;
  loc source_location;
} Parameter;

typedef struct Member {
  Type *type;
  string name;
  loc source_location;
  size_t byte_offset;
} Member;

typedef Vector(Parameter) Parameters;
typedef Vector(Member) Members;

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
  } val;
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
  bool global;
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

/// Structure declaration.
typedef Symbol *NodeStructDecl;

typedef struct NodeMemberAccess {
  string ident;
  Member *member;
  Node *struct_;
} NodeMemberAccess;

/// Variable reference.
typedef Symbol *NodeVariableReference;

typedef struct NodeFor {
  Node *init;
  Node *condition;
  Node *iterator;
  Node *body;
} NodeFor;

/// Named type.
typedef Symbol *TypeNamed;

/// Primitive type.
typedef struct TypePrimitive {
  usz size;
  usz alignment;
  span name;
  bool is_signed;
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

/// Function type.
typedef struct TypeFunction {
  Parameters parameters;
  Type *return_type;
} TypeFunction;

/// Struct type.
typedef struct TypeStruct {
  Node *decl; //> NODE_STRUCTURE_DECLARATION
  Members members;

  size_t byte_size;
  size_t alignment;
} TypeStruct;

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
    TypeStruct structure;
  };

  bool type_checked;
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
  /// Things that are lvalues store their address here.
  IRInstruction *address;

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
    NodeStructDecl struct_decl;
    NodeMemberAccess member_access;
    NodeFor for_;
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

/// Create a new for expression.
Node *ast_make_for(
    AST *ast,
    loc source_location,
    Node *init,
    Node *condition,
    Node *iterator,
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

Node *ast_make_structure_declaration(
    AST *ast,
    loc source_location,
    Symbol *symbol
);

Node *ast_make_member_access(
    AST *ast,
    loc source_location,
    span ident,
    Node *struct_
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

/// Create a new struct type.
Type *ast_make_type_struct(
    AST *ast,
    loc source_location,
    Members members
);

/// ===========================================================================
///  Type query functions.
/// ===========================================================================
/// Get a string representation of a type.
/// \return The string representation of the type. The string is allocated
///         with malloc() and must be freed by the caller.
NODISCARD string typename(Type *type, bool colour);

/** Get the canonical type of any given type.
 *
 * The ‘canonical’ type of a type T is T stripped of any aliases. E.g. in C, given
 *     typedef int foo;
 *     typedef foo bar
 * the canonical type of `bar` would be `int`.
 *
 * Currently, builtin types are just named types (i.e. typedefs in C
 * terminology) that refer to the actual primitive types, and if we
 * ever introduce something like typedef, it will just work out of the
 * box.
 *
 * \return NULL if the type is incomplete.
 */
NODISCARD Type *type_canonical(Type *type);

/// Get the last alias of a type.
///
/// This function strips nested named types until there is only one left.
NODISCARD Type *type_last_alias(Type *type);

/** Check if a type is incomplete.
 *
 * A type T is incomplete, iff
 * - the canonical type of T is void, or
 * - T has no canonical type. (e.g. if we have @foo, but foo is never
 *   defined, then foo (which is parsed as a named type) is incomplete
 *   because it has no canonical type.
 *
 * Basically, ‘incomplete’ means that we don’t know its size/alignment
 * and therefore, we can’t allocate a variable of that type.
 *
 * `void` is a special case because it is purposefully incomplete.
 *
 * \return true iff the type is incomplete.
 */
NODISCARD bool type_is_incomplete(Type *type);

/** Same as type_is_incomplete() but must be given a canonical type.
 *
 * \see type_canonical()
 * \see type_is_incomplete()
 *
 * \return true iff the given canonical type is incomplete.
 */
NODISCARD bool type_is_incomplete_canon(Type *type);

/// Get the size of a type, in bytes.
NODISCARD usz type_sizeof(Type *type);

/// Get the aligmnent of a type, in bytes.
NODISCARD usz type_alignof(Type *type);

/// Check if a type is void.
NODISCARD bool type_is_void(Type *type);

/// Check if a type is of pointer type.
NODISCARD bool type_is_pointer(Type *type);

/// Check if a type is of array type.
NODISCARD bool type_is_array(Type *type);


/// ===========================================================================
///  Miscellaneous AST functions.
/// ===========================================================================
/// Create a new AST.
NODISCARD AST *ast_create();

/// Free an AST.
void ast_free(AST *ast);

/// Print an AST.
void ast_print(FILE *file, const AST *ast);

/// Print the scope tree of an AST.
void ast_print_scope_tree(FILE *file, const AST *ast);

/// Print a node and all of it's children.
void ast_print_node(const Node *node);

/// Intern a string.
size_t ast_intern_string(AST *ast, span string);

/// Replace a node with another node.
void ast_replace_node(AST *ast, Node *old, Node *new);

/// ===========================================================================
///  Builtin types.
/// ===========================================================================
extern Type *const t_void;
extern Type *const t_void_ptr;
extern Type *const t_integer_literal;
extern Type *const t_integer;
extern Type *const t_byte;

#endif // FUNCOMPILER_AST_H
