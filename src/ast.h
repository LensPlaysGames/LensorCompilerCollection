#ifndef INTC_AST_H
#define INTC_AST_H

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
  NODE_FUNCTION_REFERENCE,
  NODE_INTRINSIC_CALL,
  NODE_MODULE_REFERENCE,
  NODE_VARIABLE_REFERENCE,
  NODE_STRUCTURE_DECLARATION,
  NODE_MEMBER_ACCESS,
  NODE_FOR,
  NODE_RETURN,
  NODE_COUNT
};

/// The kind of a type.
typedef enum TypeKind {
  TYPE_PRIMITIVE,
  TYPE_NAMED,
  TYPE_POINTER,
  TYPE_REFERENCE,
  TYPE_ARRAY,
  TYPE_FUNCTION,
  TYPE_STRUCT,
  TYPE_INTEGER,
  TYPE_COUNT
} TypeKind;

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
  TK_ARBITRARY_INT,
  TK_FOR,
  TK_RETURN,
  TK_EXPORT,

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

  // Tells the parser to generate a unique identifier per integer
  // value. Used to implement macro "defines".
  TK_GENSYM,
  TK_MACRO_ARG,
  // Yes, this is cursed. Thanks to macros, it is technically possible to
  // "lex" an expression (which calls into the parser and creates an AST
  // node...).
  TK_AST_NODE,

  TK_COUNT
};

/// Attributes.
typedef enum FunctionAttribute {
#define F(name, ...) FUNC_ATTR_##name,
  SHARED_FUNCTION_ATTRIBUTES(F)
  FRONTEND_FUNCTION_ATTRIBUTES(F)
  IR_FUNCTION_ATTRIBUTES(F)
#undef F
} FunctionAttribute;

/// Types of attributes
typedef enum VariableAttribute {
  ATTR_ALIGNAS,
  ATTR_COUNT
} VariableAttribute;

/// The type of a symbol in the symbol table.
enum SymbolKind {
  SYM_VARIABLE,
  SYM_FUNCTION,
  SYM_TYPE,
  SYM_COUNT
};

/// Intrinsics that need to be handled by the backend,
/// i.e. in MIR or later.
#define ALL_BACKEND_INTRINSICS(F) \
  F(BUILTIN_SYSCALL)              \
  F(BUILTIN_DEBUGTRAP)

/// Intrinsics that need to be gone after IR generation.
#define ALL_FRONTEND_INTRINSICS(F) \
  F(BUILTIN_INLINE)                \
  F(BUILTIN_LINE)                  \
  F(BUILTIN_FILENAME)

/// Helpers to ignore intrinsics that should never exist
/// in the backend.
#define IGNORE_FRONTEND_INTRINSIC(name) \
  case INTRIN_##name:
#define IGNORE_FRONTEND_INTRINSICS()                 \
  ALL_FRONTEND_INTRINSICS(IGNORE_FRONTEND_INTRINSIC) \
  case INTRIN_COUNT:                                 \
  case INTRIN_BACKEND_COUNT: UNREACHABLE();

/// Intrinsic functions.
enum IntrinsicKind {
#define F(name) INTRIN_##name,
  ALL_BACKEND_INTRINSICS(F)
  INTRIN_BACKEND_COUNT,
  ALL_FRONTEND_INTRINSICS(F)
  INTRIN_COUNT,
#undef F
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
///  Attributes.
/// ===========================================================================
typedef struct Attribute {
  int kind;
  union AttributeValue {
    usz integer;
  } value;
} Attribute;
typedef Vector(Attribute) Attributes;

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
  SymbolLinkage linkage;
} NodeFunction;

/// Variable declaration.
typedef struct NodeDeclaration {
  Node *init;
  string name;
  SymbolLinkage linkage;
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
  enum IntrinsicKind intrinsic; /// Only used by intrinsic calls.
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
    Nodes compound;
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

typedef struct NodeModuleReference {
  struct Module *ast;
} NodeModuleReference;

typedef struct NodeFor {
  Node *init;
  Node *condition;
  Node *iterator;
  Node *body;
} NodeFor;

typedef struct NodeReturn {
  Node *value;
} NodeReturn;

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
/// Reference type.
typedef struct TypePointer {
  Type *to;
} TypePointer;
typedef TypePointer TypeReference;

/// Array type.
typedef struct TypeArray {
  Type *of;
  size_t size;
} TypeArray;

/// Function type.
typedef struct TypeFunction {
  Parameters parameters;
  Type *return_type;
#define F(_, name) bool attr_##name;
  SHARED_FUNCTION_ATTRIBUTES(F)
  FRONTEND_FUNCTION_ATTRIBUTES(F)
#undef F
} TypeFunction;

/// Struct type.
typedef struct TypeStruct {
  Node *decl; //> NODE_STRUCTURE_DECLARATION
  Members members;

  size_t byte_size;
  size_t alignment;
} TypeStruct;

typedef struct TypeInteger {
  bool is_signed;
  usz bit_width;
} TypeInteger;

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
    TypeReference reference;
    TypeArray array;
    TypeFunction function;
    TypeStruct structure;
    TypeInteger integer;
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
    NodeFunctionReference funcref;
    NodeModuleReference module_ref;
    NodeVariableReference var;
    NodeStructDecl struct_decl;
    NodeMemberAccess member_access;
    NodeFor for_;
    NodeReturn return_;
  };
};

/// Data structure that stores an AST; a module.
// TODO: Rename to "Module"
typedef struct Module {
  /// The root node of the AST.
  Node *root;

  string module_name;
  bool is_module;

  Vector(struct Module *) imports;
  Vector(Node *) exports;

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
} Module;

/// ===========================================================================
///  Scope/symbol functions.
/// ===========================================================================
/// Push a new scope.
void scope_push(Module *ast);

/// Pop the current scope. This does *not* delete the scope.
void scope_pop(Module *ast);

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
  Module *ast,
    loc source_location,
    Type *type,
    SymbolLinkage linkage,
    Nodes param_decls,
    Node *body,
    span name
);

/// Create a new declaration node.
Node *ast_make_declaration(
  Module *ast,
    loc source_location,
    Type *type,
    SymbolLinkage linkage, ///< Pass whatever for locals.
    span name,
    Node *init
);

/// Create a new if expression.
Node *ast_make_if(
  Module *ast,
    loc source_location,
    Node *condition,
    Node *then,
    Node *else_
);

/// Create a new while expression.
Node *ast_make_while(
  Module *ast,
    loc source_location,
    Node *condition,
    Node *body
);

/// Create a new for expression.
Node *ast_make_for(
  Module *ast,
    loc source_location,
    Node *init,
    Node *condition,
    Node *iterator,
    Node *body
);

/// Create a new return expression.
Node *ast_make_return(
  Module *ast,
    loc source_location,
    Node *value
);

/// Create a new block expression.
Node *ast_make_block(
  Module *ast,
    loc source_location,
    Nodes children
);

/// Create a new call expression.
Node *ast_make_call(
  Module *ast,
    loc source_location,
    Node *callee,
    Nodes arguments
);

/// Create a new cast expression.
Node *ast_make_cast(
  Module *ast,
    loc source_location,
    Type *to,
    Node *value
);

/// Create a new binary expression.
Node *ast_make_binary(
  Module *ast,
    loc source_location,
    enum TokenType op,
    Node *lhs,
    Node *rhs
);

/// Create a new unary expression.
Node *ast_make_unary(
  Module *ast,
    loc source_location,
    enum TokenType op,
    bool postfix,
    Node *value
);

/// Create a new integer literal.
Node *ast_make_integer_literal(
  Module *ast,
    loc source_location,
    u64 value
);

/// Create a new string literal.
Node *ast_make_string_literal(
  Module *ast,
    loc source_location,
    span string
);

/// Create a new compound literal.
Node *ast_make_compound_literal(
  Module *ast,
    loc source_location
);
/// Add a node to an existing compound literal.
void ast_add_to_compound_literal(
    Node *compound,
    Node *node
);

/// Create a new variable reference.
Node *ast_make_variable_reference(
  Module *ast,
    loc source_location,
    Symbol *symbol
);

/// Create a new module reference.
Node *ast_make_module_reference(
  Module *ast,
    loc source_location,
  Module *module
);

/// Create a new function reference.
Node *ast_make_function_reference(
  Module *ast,
    loc source_location,
    span symbol
);

Node *ast_make_structure_declaration(
  Module *ast,
    loc source_location,
    Symbol *symbol
);

Node *ast_make_member_access(
  Module *ast,
    loc source_location,
    span ident,
    Node *struct_
);

/// Create a new named type.
Type *ast_make_type_named(
  Module *ast,
    loc source_location,
    Symbol *symbol
);

/// Create a new pointer type.
Type *ast_make_type_pointer(
  Module *ast,
    loc source_location,
    Type *to
);

/// Create a new reference type.
Type *ast_make_type_reference(
  Module *ast,
    loc source_location,
    Type *to
);

/// Create a new array type.
Type *ast_make_type_array(
  Module *ast,
    loc source_location,
    Type *of,
    size_t size
);

/// Create a new function type.
Type *ast_make_type_function(
  Module *ast,
    loc source_location,
    Type *return_type,
    Parameters parameters
);

/// Create a new struct type.
Type *ast_make_type_struct(
  Module *ast,
    loc source_location,
    Members members
);

/// Create a new integer type.
Type *ast_make_type_integer(
  Module *ast,
    loc source_location,
    bool is_signed,
    usz bit_width
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

/// Result type for the function below.
typedef struct IncompleteResult {
  bool incomplete;
  bool equal;
} IncompleteResult;
/// Compare two possibly incomplete types.
/// `a` and `b` must be the last alias of their corresponding types.
NODISCARD IncompleteResult compare_incomplete(Type *a, Type *b);

/// Get the element type of a type.
NODISCARD Type *type_get_element(Type *type);

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

/// Check if a type is of reference type.
NODISCARD bool type_is_reference(Type *type);

/// Check if a type is of array type.
NODISCARD bool type_is_array(Type *type);

/// Check if a type is of struct type.
NODISCARD bool type_is_struct(Type *type);

/// Return true iff the given type is an integer type *and* has the
/// possiblity of being negative (aka it is signed).
/// In all other cases, return false.
NODISCARD bool type_is_signed(Type *type);
NODISCARD bool type_is_signed_canon(Type *type);

/// Check if two canonical types are equal. You probably want to use
/// `convertible()`
/// \return Whether the types are equal.
NODISCARD bool type_equals_canon(Type *a, Type *b);

/// Check if two types are equal.
/// \return Whether the types are equal.
NODISCARD bool type_equals(Type *a, Type *b);

/// Check if a canonical type is of any integer type.
NODISCARD bool type_is_integer_canon(Type *t);

/// Check if a type is of any integer type.
NODISCARD bool type_is_integer(Type *type);

NODISCARD Type *type_strip_references(Type *type);

/// ===========================================================================
///  Miscellaneous AST functions.
/// ===========================================================================
/// Create a new AST.
NODISCARD Module *ast_create();

/// Free an AST.
void ast_free(Module *ast);

/// Print an AST.
void ast_print(FILE *file, const Module *ast);

/// Print the scope tree of an AST.
void ast_print_scope_tree(FILE *file, const Module *ast);

/// Print a node and all of it's children.
void ast_print_node(const Node *node);

/// Intern a string.
size_t ast_intern_string(Module *ast, span string);

/// Replace a node with another node.
void ast_replace_node(Module *ast, Node *old, Node *new);

/// Check if the expression contained in the given AST Node can be an
/// lvalue.
NODISCARD bool is_lvalue(Node *expr);

/// ===========================================================================
///  Builtin types.
/// ===========================================================================
extern Type *const t_void;
extern Type *const t_void_ptr;
extern Type *const t_integer_literal;
extern Type *const t_integer;
extern Type *const t_byte;

extern Type *primitive_types[4];

#endif // INTC_AST_H
