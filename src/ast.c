#include <ast.h>

/// ===========================================================================
///  Scope/symbol functions.
/// ===========================================================================
static Scope *scope_create(Scope *parent) {
  Scope *scope = calloc(1, sizeof(Scope));
  scope->parent = parent;
  return scope;
}

static void scope_delete(Scope *scope) {
  VECTOR_FOREACH_PTR (Symbol *, symbol, scope->symbols) {
    free(symbol->name.data);
    free(symbol);
  }

  VECTOR_DELETE(scope->symbols);
  free(scope);
}

void scope_push(AST *ast) {
  ASSERT(ast->scopes.size, "AST must have a global scope.");
  Scope *scope = scope_create(VECTOR_BACK(ast->scopes));
  VECTOR_PUSH(ast->scopes, scope);
}

void scope_pop(AST *ast) {
  ASSERT(ast->scopes.size > 1, "Cannot pop the global scope.");
  VECTOR_POP(ast->scopes);
}

Symbol *scope_add_symbol(Scope *scope, enum SymbolKind kind, span name, Node *value) {
  Symbol *symbol = calloc(1, sizeof(Symbol));
  symbol->kind = kind;
  symbol->name = string_dup(name);
  symbol->value = value;
  VECTOR_PUSH(scope->symbols, symbol);
  return symbol;
}

Symbol *scope_find_symbol(Scope *scope, span name, bool this_scope_only) {
  while (scope) {
    /// Return the symbol if it exists.
    VECTOR_FOREACH_PTR (Symbol *, symbol, scope->symbols)
      if (string_eq(symbol->name, name))
        return symbol;

    /// If we're only looking in the current scope, return NULL.
    if (this_scope_only) return NULL;

    /// Otherwise, search the parent scope.
    scope = scope->parent;
  }

  /// Symbol not found.
  return NULL;
}

Symbol *scope_find_or_add_symbol(Scope *scope, enum SymbolKind kind, span name, bool this_scope_only) {
  Symbol *symbol = scope_find_symbol(scope, name, this_scope_only);
  if (symbol) return symbol;
  return scope_add_symbol(scope, kind, name, NULL);
}

/// ===========================================================================
///  Functions to create ast nodes.
/// ===========================================================================
/// Internal helper to create a node.
static Node *mknode(AST *ast, enum NodeKind kind, loc source_location) {
  Node *node = calloc(1, sizeof(Node));
  node->kind = kind;
  node->source_location = source_location;
  VECTOR_PUSH(ast->_nodes_, node);
  return node;
}

/// Create a new function node.
Node *ast_make_function(
    AST *ast,
    loc source_location,
    Node *type,
    Node *body,
    span name
) {
  Node *node = mknode(ast, NODE_FUNCTION, source_location);
  node->function.name = string_dup(name);
  node->function.type = type;
  node->function.body = body;
  VECTOR_PUSH(ast->functions, node);
  return node;
}

/// Create a new declaration node.
Node *ast_make_declaration(
    AST *ast,
    loc source_location,
    Node *type,
    span name
) {
  Node *node = mknode(ast, NODE_DECLARATION, source_location);
  node->declaration.name = string_dup(name);
  node->declaration.type = type;
  return node;
}

/// Create a new if expression.
Node *ast_make_if(
    AST *ast,
    loc source_location,
    Node *condition,
    Node *then,
    Node *else_
) {
  Node *node = mknode(ast, NODE_IF, source_location);
  node->if_.condition = condition;
  node->if_.then = then;
  node->if_.else_ = else_;
  return node;
}

/// Create a new while expression.
Node *ast_make_while(
    AST *ast,
    loc source_location,
    Node *condition,
    Node *body
) {
  Node *node = mknode(ast, NODE_WHILE, source_location);
  node->while_.condition = condition;
  node->while_.body = body;
  return node;
}

/// Create a new block expression.
Node *ast_make_block(
    AST *ast,
    loc source_location,
    Nodes children
) {
  Node *node = mknode(ast, NODE_BLOCK, source_location);
  node->block.children = children;
  return node;
}

/// Create a new call expression.
Node *ast_make_call(
    AST *ast,
    loc source_location,
    Node *callee,
    Nodes arguments
) {
  Node *node = mknode(ast, NODE_CALL, source_location);
  node->call.callee = callee;
  node->call.arguments = arguments;
  return node;
}

/// Create a new cast expression.
Node *ast_make_cast(
    AST *ast,
    loc source_location,
    Node *to_type,
    Node *value
) {
  Node *node = mknode(ast, NODE_CAST, source_location);
  node->cast.to_type = to_type;
  node->cast.value = value;
  return node;
}

/// Create a new binary expression.
Node *ast_make_binary(
    AST *ast,
    loc source_location,
    enum TokenType op,
    Node *lhs,
    Node *rhs
) {
  Node *node = mknode(ast, NODE_BINARY, source_location);
  node->binary.op = op;
  node->binary.lhs = lhs;
  node->binary.rhs = rhs;
  return node;
}

/// Create a new unary expression.
Node *ast_make_unary(
    AST *ast,
    loc source_location,
    enum TokenType op,
    bool postfix,
    Node *value
) {
  Node *node = mknode(ast, NODE_UNARY, source_location);
  node->unary.op = op;
  node->unary.postfix = postfix;
  node->unary.value = value;
  return node;
}

/// Create a new integer literal.
Node *ast_make_integer_literal(
    AST *ast,
    loc source_location,
    u64 value
) {
  Node *node = mknode(ast, NODE_LITERAL, source_location);
  node->literal.type = TK_NUMBER;
  node->literal.integer = value;
  return node;
}

/// Create a new string literal.
Node *ast_make_string_literal(
    AST *ast,
    loc source_location,
    span string
) {
  Node *node = mknode(ast, NODE_LITERAL, source_location);
  node->literal.type = TK_STRING;
  node->literal.string_index = ast_intern_string(ast, string);
  return node;
}

/// Create a new variable reference.
Node *ast_make_variable_reference(
    AST *ast,
    loc source_location,
    Symbol *symbol
) {
  Node *node = mknode(ast, NODE_VARIABLE_REFERENCE, source_location);
  node->var = symbol;
  return node;
}

/// Create a new function reference.
Node *ast_make_function_reference(
    AST *ast,
    loc source_location,
    Symbol *symbol
) {
  Node *node = mknode(ast, NODE_FUNCTION_REFERENCE, source_location);
  node->funcref = symbol;
  return node;
}

/// Create a new named type.
Node *ast_make_type_named(
    AST *ast,
    loc source_location,
    Symbol *symbol
) {
  Node *node = mknode(ast, NODE_TYPE_NAMED, source_location);
  node->type_named = symbol;
  return node;
}

/// Create a new pointer type.
Node *ast_make_type_pointer(
    AST *ast,
    loc source_location,
    Node *to,
    usz level
) {
  Node *node = mknode(ast, NODE_TYPE_POINTER, source_location);
  node->type_pointer.to = to;
  node->type_pointer.level = level;
  return node;
}

/// Create a new array type.
Node *ast_make_type_array(
    AST *ast,
    loc source_location,
    Node *of,
    size_t size
) {
  Node *node = mknode(ast, NODE_TYPE_ARRAY, source_location);
  node->type_array.of = of;
  node->type_array.size = size;
  return node;
}

/// Create a new function type.
Node *ast_make_type_function(
    AST *ast,
    loc source_location,
    Node *return_type,
    Nodes parameters
) {
  Node *node = mknode(ast, NODE_TYPE_FUNCTION, source_location);
  node->type_function.parameters = parameters;
  node->type_function.return_type = return_type;
  return node;
}

/// ===========================================================================
///  Miscellaneous AST functions.
/// ===========================================================================
/// Create a new AST.
AST *ast_create() {
  AST *ast = calloc(1, sizeof(AST));

  /// Create the root node.
  ast->root = mknode(ast, NODE_ROOT, (loc){0, 0});

  /// Create the global scope.
  VECTOR_PUSH(ast->scopes, scope_create(NULL));

  /// Initialise the builtin types.
  ast->t_integer = mknode(ast, NODE_TYPE_PRIMITIVE, (loc){0, 0});
  ast->t_integer->type_primitive.size = 8;
  ast->t_integer->type_primitive.alignment = 8;
  ast->t_integer->type_primitive.is_signed = true;
  ast->t_integer->type_primitive.name = literal_span("integer");

  /// Add the builtin types to the global scope.
  scope_add_symbol(ast->scopes.data[0], SYM_TYPE, literal_span("integer"), ast->t_integer);

  /// Done.
  return ast;
}

/// Free an AST.
void ast_free(AST *ast) {
  /// Free all nodes.
  VECTOR_FOREACH_PTR (Node *, node, ast->_nodes_) free(node);
  VECTOR_DELETE(ast->_nodes_);

  /// Free all scopes.
  VECTOR_FOREACH_PTR (Scope *, scope, ast->scopes) scope_delete(scope);
  VECTOR_DELETE(ast->scopes);

  /// Free all interned strings.
  VECTOR_FOREACH (string, string, ast->strings) free(string->data);
  VECTOR_DELETE(ast->strings);

  /// Free the filename and source code.
  free(ast->filename.data);
  free(ast->source.data);

  /// Free the AST.
  free(ast);
}

/// Print an AST.
void ast_print(const AST *ast) {
  /// Print the root node.
  ast_print_node(ast->root, 0);
}

/// Print a node.
void ast_print_node(const Node *node, size_t indent) {
    TODO();
}

/// Intern a string.
size_t ast_intern_string(AST *ast, span str) {
  /// Check if the string is already interned.
  VECTOR_FOREACH_INDEX (i, ast->strings)
    if (string_eq(ast->strings.data[i], str)) return i;

  /// Intern the string.
  VECTOR_PUSH(ast->strings, string_dup(str));
  return ast->strings.size - 1;
}