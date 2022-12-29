#include <ast.h>
#include <parser.h>

#define TYPENAME_MAX_SIZE ((size_t)65536)

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
  VECTOR_DELETE(scope->children);
  free(scope);
}

void scope_push(AST *ast) {
  ASSERT(ast->scope_stack.size, "AST must have a global scope.");
  Scope *scope = scope_create(VECTOR_BACK(ast->scope_stack));
  VECTOR_PUSH(ast->scope_stack, scope);
  VECTOR_PUSH(ast->_scopes_, scope);
}

void scope_pop(AST *ast) {
  ASSERT(ast->scope_stack.size > 1, "Cannot pop the global scope.");
  (void) VECTOR_POP(ast->scope_stack);
}

Symbol *scope_add_symbol(Scope *scope, enum SymbolKind kind, span name, void *value) {
  /// Check if the symbol already exists.
  if (scope_find_symbol(scope, name, true)) return NULL;

  Symbol *symbol = calloc(1, sizeof(Symbol));
  symbol->kind = kind;
  symbol->name = string_dup(name);
  symbol->scope = scope;

  if (kind == SYM_TYPE) symbol->type = value;
  else symbol->node = value;
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
NODISCARD static Node *mknode(AST *ast, enum NodeKind kind, loc source_location) {
  Node *node = calloc(1, sizeof(Node));
  node->kind = kind;
  node->source_location = source_location;
  VECTOR_PUSH(ast->_nodes_, node);
  return node;
}

/// Internal helper to create a type.
NODISCARD static Type *mktype(AST *ast, enum TypeKind kind, loc source_location) {
  Type *type = calloc(1, sizeof(Type));
  type->kind = kind;
  type->source_location = source_location;
  VECTOR_PUSH(ast->_types_, type);
  return type;
}

/// Create a new function node.
Node *ast_make_function(
    AST *ast,
    loc source_location,
    Type *type,
    Nodes param_decls,
    Node *body,
    span name
) {
  Node *node = mknode(ast, NODE_FUNCTION, source_location);
  node->function.name = string_dup(name);
  node->type = type;
  node->function.body = body;
  node->function.param_decls = param_decls;
  node->function.global = true;
  node->parent = ast->root;
  if (body) body->parent = node;

  VECTOR_PUSH(ast->functions, node);
  return node;
}

/// Create a new declaration node.
Node *ast_make_declaration(
    AST *ast,
    loc source_location,
    Type *type,
    span name,
    Node *init
) {
  Node *node = mknode(ast, NODE_DECLARATION, source_location);
  node->declaration.name = string_dup(name);
  node->type = type;
  if (init) {
    node->declaration.init = init;
    init->parent = node;
  }
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
  condition->parent = node;
  then->parent = node;
  if (else_) else_->parent = node;
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
  condition->parent = node;
  body->parent = node;
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
  VECTOR_FOREACH_PTR (Node *, child, children) child->parent = node;
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
  callee->parent = node;
  VECTOR_FOREACH_PTR (Node *, argument, arguments) argument->parent = node;
  return node;
}

/// Create a new cast expression.
Node *ast_make_cast(
    AST *ast,
    loc source_location,
    Type *to,
    Node *value
) {
  Node *node = mknode(ast, NODE_CAST, source_location);
  node->type = to;
  node->cast.value = value;
  value->parent = node;
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
  lhs->parent = node;
  rhs->parent = node;
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
  value->parent = node;
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
    span str
) {
  Node *node = mknode(ast, NODE_LITERAL, source_location);
  node->literal.type = TK_STRING;
  node->literal.string_index = ast_intern_string(ast, str);
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
Type *ast_make_type_named(
    AST *ast,
    loc source_location,
    Symbol *symbol
) {
  Type *type = mktype(ast, TYPE_NAMED, source_location);
  type->named = symbol;
  return type;
}

/// Create a new pointer type.
Type *ast_make_type_pointer(
    AST *ast,
    loc source_location,
    Type *to
) {
  Type *type = mktype(ast, TYPE_POINTER, source_location);
  type->pointer.to = to;
  return type;
}

/// Create a new array type.
Type *ast_make_type_array(
    AST *ast,
    loc source_location,
    Type *of,
    size_t size
) {
  Type *type = mktype(ast, TYPE_ARRAY, source_location);
  type->array.of = of;
  type->array.size = size;
  return type;
}

/// Create a new function type.
Type *ast_make_type_function(
    AST *ast,
    loc source_location,
    Type *return_type,
    Parameters parameters
) {
  Type *type = mktype(ast, TYPE_FUNCTION, source_location);
  type->function.parameters = parameters;
  type->function.return_type = return_type;
  return type;
}


/// ===========================================================================
///  AST query functions.
/// ===========================================================================
/// Used by `ast_typename`.
void write_typename(string *s, const Type *type, bool colour) {
  if (!type) {
    if (colour) s->size += (usz) snprintf(s->data + s->size, TYPENAME_MAX_SIZE - s->size, "\033[m");
    s->size += (usz) snprintf(s->data + s->size, TYPENAME_MAX_SIZE - s->size, "(\?\?\?)");
    return;
  }

  /// Print the type.
  switch (type->kind) {
    default:
      s->size += (usz) snprintf(s->data + s->size, TYPENAME_MAX_SIZE - s->size, "(\?\?\?)");
      break;

    case TYPE_PRIMITIVE:
      if (colour) s->size += (usz) snprintf(s->data + s->size, TYPENAME_MAX_SIZE - s->size, "\033[36m");
      s->size += (usz) snprintf(s->data + s->size, TYPENAME_MAX_SIZE - s->size, "%.*s",
        (int) type->primitive.name.size, type->primitive.name.data);
      break;

    case TYPE_NAMED:
      if (colour) s->size += (usz) snprintf(s->data + s->size, TYPENAME_MAX_SIZE - s->size, "\033[36m");
      s->size += (usz) snprintf(s->data + s->size, TYPENAME_MAX_SIZE - s->size, "%.*s",
        (int) type->named->name.size, type->named->name.data);
      break;

    case TYPE_POINTER: {
      if (colour) s->size += (usz) snprintf(s->data + s->size, TYPENAME_MAX_SIZE - s->size, "\033[36m");
      s->size += (usz) snprintf(s->data + s->size, TYPENAME_MAX_SIZE - s->size, "@");
      if (type->pointer.to->kind == TYPE_FUNCTION) {
        if (colour) s->size += (usz) snprintf(s->data + s->size, TYPENAME_MAX_SIZE - s->size, "\033[31m");
        s->size += (usz) snprintf(s->data + s->size, TYPENAME_MAX_SIZE - s->size, "(");
      }
      write_typename(s, type->pointer.to, colour);
      if (type->pointer.to->kind == TYPE_FUNCTION) {
        s->size += (usz) snprintf(s->data + s->size, TYPENAME_MAX_SIZE - s->size, ")");
      }
    } break;

    case TYPE_ARRAY:
      write_typename(s, type->array.of, colour);
      if (colour) s->size += (usz) snprintf(s->data + s->size, TYPENAME_MAX_SIZE - s->size, "\033[31m");
      s->size += (usz) snprintf(s->data + s->size, TYPENAME_MAX_SIZE - s->size, "[");
      if (colour) s->size += (usz) snprintf(s->data + s->size, TYPENAME_MAX_SIZE - s->size, "\033[35m");
      s->size += (usz) snprintf(s->data + s->size, TYPENAME_MAX_SIZE - s->size, "%zu", type->array.size);
      if (colour) s->size += (usz) snprintf(s->data + s->size, TYPENAME_MAX_SIZE - s->size, "\033[31m");
      s->size += (usz) snprintf(s->data + s->size, TYPENAME_MAX_SIZE - s->size, "]");
      break;

    case TYPE_FUNCTION:
      write_typename(s, type->function.return_type, colour);
      if (colour) s->size += (usz) snprintf(s->data + s->size, TYPENAME_MAX_SIZE - s->size, "\033[31m");
      s->size += (usz) snprintf(s->data + s->size, TYPENAME_MAX_SIZE - s->size, " (");

      /// Parameters.
      VECTOR_FOREACH (Parameter, param, type->function.parameters) {
        write_typename(s, param->type, colour);
        if (param != type->function.parameters.data + type->function.parameters.size - 1) {
          if (colour) s->size += (usz) snprintf(s->data + s->size, TYPENAME_MAX_SIZE - s->size, "\033[31m");
          s->size += (usz) snprintf(s->data + s->size, TYPENAME_MAX_SIZE - s->size, ", ");
        }
      }

      if (colour) s->size += (usz) snprintf(s->data + s->size, TYPENAME_MAX_SIZE - s->size, "\033[31m");
      s->size += (usz) snprintf(s->data + s->size, TYPENAME_MAX_SIZE - s->size, ")");
      break;
  }
}

/// Get a string representation of a type.
string ast_typename(const Type *type, bool colour) {
  string s = {
    .data = calloc(1, TYPENAME_MAX_SIZE),
    .size = 0
  };
  write_typename(&s, type, colour);
  if (colour) s.size += (usz) snprintf(s.data + s.size, TYPENAME_MAX_SIZE - s.size, "\033[m");
  return s;
}

/// Check if a type is incomplete.
bool ast_type_is_incomplete(const Type *type) {
  while (type && type->kind == TYPE_NAMED) type = type->named->type;
  return !type;
}

/// Get the size of a type.
usz ast_sizeof(const Type *type) {
  switch (type->kind) {
    default: ICE("Invalid type kind: %d", type->kind);
    case TYPE_PRIMITIVE: return type->primitive.size;
    case TYPE_NAMED: return type->named ? ast_sizeof(type->named->type) : 0;
    case TYPE_POINTER: return sizeof(void *);
    case TYPE_ARRAY: return type->array.size * ast_sizeof(type->array.of);
    case TYPE_FUNCTION: return sizeof(void *);
  }
}

Typeinfo ast_typeinfo(AST *ast, Type * const type) {
  /// Resolve aliases.
  Type *base = type;
  Type *alias = type;
  while (base && base->kind == TYPE_NAMED) {
    alias = base;
    base = base->named ? base->named->type : NULL;
  }

  return (Typeinfo){
    .type = base,
    .last_alias = alias,
    .is_incomplete = !base,
    .is_void = alias == ast->t_void,
  };
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
  VECTOR_PUSH(ast->scope_stack, scope_create(NULL));
  VECTOR_PUSH(ast->_scopes_, VECTOR_BACK(ast->scope_stack));

  /// Initialise the builtin types.
  uint8_t primitive_type_id = 0;
  ast->t_integer = mktype(ast, TYPE_PRIMITIVE, (loc){0, 0});
  ast->t_integer->primitive.size = 8;
  ast->t_integer->primitive.alignment = 8;
  ast->t_integer->primitive.is_signed = true;
  ast->t_integer->primitive.name = literal_span("integer");
  ast->t_integer->primitive.id = primitive_type_id++;

  /// Declare void as a named type with no node associated with it.
  /// This implicitly means that void is an incomplete type.
  ast->t_void = mktype(ast, TYPE_NAMED, (loc){0, 0});

  /// Add the builtin types to the global scope.
  scope_add_symbol(ast->_scopes_.data[0], SYM_TYPE, literal_span("integer"), ast->t_integer);
  scope_add_symbol(ast->_scopes_.data[0], SYM_TYPE, literal_span("void"), ast->t_void);

  /// Done.
  return ast;
}

/// Free an AST.
void ast_free(AST *ast) {
  /// Some nodes may contain strings, vectors, etc.. Iterate over all
  /// nodes and free all resources they may have.
  VECTOR_FOREACH_PTR (Node *, node, ast->_nodes_) {
    switch (node->kind) {
      case NODE_FUNCTION:
        free(node->function.name.data);
        VECTOR_DELETE(node->function.param_decls);
        continue;

      case NODE_ROOT: VECTOR_DELETE(node->root.children); continue;
      case NODE_BLOCK: VECTOR_DELETE(node->block.children); continue;
      case NODE_CALL: VECTOR_DELETE(node->call.arguments); continue;
      case NODE_DECLARATION: free(node->declaration.name.data); continue;

      case NODE_IF:
      case NODE_WHILE:
      case NODE_CAST:
      case NODE_BINARY:
      case NODE_UNARY:
      case NODE_LITERAL:
      case NODE_VARIABLE_REFERENCE:
      case NODE_FUNCTION_REFERENCE: continue;
    }
    UNREACHABLE();
  }

  /// Now that that’s done, free all nodes.
  VECTOR_FOREACH_PTR (Node *, node, ast->_nodes_) free(node);
  VECTOR_DELETE(ast->_nodes_);
  VECTOR_DELETE(ast->functions);

  /// Free all types.
  VECTOR_FOREACH_PTR (Type *, type, ast->_types_) {
    if (type->kind == TYPE_FUNCTION) {
      VECTOR_FOREACH (Parameter, param, type->function.parameters) free(param->name.data);
      VECTOR_DELETE(type->function.parameters);
    }
    free(type);
  }
  VECTOR_DELETE(ast->_types_);

  /// Free all scopes.
  VECTOR_FOREACH_PTR (Scope *, scope, ast->_scopes_) scope_delete(scope);
  VECTOR_DELETE(ast->_scopes_);
  VECTOR_DELETE(ast->scope_stack);

  /// Free all interned strings.
  VECTOR_FOREACH (string, s, ast->strings) free(s->data);
  VECTOR_DELETE(ast->strings);

  /// Free the filename and source code.
  free(ast->filename.data);
  free(ast->source.data);

  /// Free the AST.
  free(ast);
}

/// Print the children of a node. Has more options.
static void ast_print_children(
    FILE *file,
    const Node *logical_grandparent,
    const Node *logical_parent,
    const Nodes *nodes,
    char leading_text[static AST_PRINT_BUFFER_SIZE]
);

/// Print a node.
static void ast_print_node(
  FILE *file,
  const Node *logical_parent,
  const Node *node,
  char leading_text[static AST_PRINT_BUFFER_SIZE]
) {
  switch (node->kind) {
    default: TODO("Print node of type %d", node->kind);

    case NODE_ROOT: {
      fprintf(file, "\033[31mRoot \033[35m<%d>\n", node->source_location.start);
      ast_print_children(file, logical_parent, node, &node->root.children, leading_text);
    } break;

    case NODE_FUNCTION: {
      /// Print the function name and type.
      string type_name = ast_typename(node->type, true);
      fprintf(file, "\033[31mFunction \033[35m<%d> \033[32m%.*s \033[31m: %.*s\n",
        node->source_location.start,
        (int) node->function.name.size, node->function.name.data,
        (int) type_name.size, type_name.data);
      free(type_name.data);

      /// Print the body.
      if (node->function.body) {
        ast_print_children(file, logical_parent, node, &(Nodes) {
          .data = (Node *[]) {node->function.body},
          .size = 1
        }, leading_text);
      }
    } break;

    case NODE_DECLARATION: {
      /// Print the declaration name and type.
      string type_name = ast_typename(node->type, true);
      fprintf(file, "\033[31mDeclaration \033[35m<%d> \033[m\033[38m%.*s \033[31m: %.*s\n",
        node->source_location.start,
        (int) node->declaration.name.size, node->declaration.name.data,
        (int) type_name.size, type_name.data);
      free(type_name.data);

      /// Print the initialiser if there is one exists.
      if (node->declaration.init) {
        ast_print_children(file, logical_parent, node, &(Nodes) {
          .data = (Node *[]) {node->declaration.init},
          .size = 1
        }, leading_text);
      }
    } break;

    case NODE_IF: {
      /// Print the condition.
      string type_name = ast_typename(node->type, true);
      fprintf(file, "\033[31mIf \033[35m<%d> %.*s\n",
        node->source_location.start,
        (int) type_name.size, type_name.data);
      free(type_name.data);

      /// Print the condition and branches.
      ast_print_children(file, logical_parent, node, &(Nodes) {
        .data = (Node *[]) {node->if_.condition, node->if_.then, node->if_.else_},
        .size = node->if_.else_ ? 3 : 2
      }, leading_text);
    } break;

    case NODE_WHILE: {
      fprintf(file, "\033[31mWhile \033[35m<%d>\n", node->source_location.start);
      ast_print_children(file, logical_parent, node, &(Nodes) {
        .data = (Node *[]) {node->while_.condition, node->while_.body},
        .size = 2
      }, leading_text);
    } break;

    case NODE_BLOCK: {
      string type_name = ast_typename(node->type, true);
      fprintf(file, "\033[31mBlock \033[35m<%d> %.*s\n",
        node->source_location.start,
        (int) type_name.size, type_name.data);
      ast_print_children(file, logical_parent, node, &node->block.children, leading_text);
      free(type_name.data);
    } break;

    case NODE_CALL: {
      string type_name = ast_typename(node->type, true);
      fprintf(file, "\033[31mCall \033[35m<%d> %.*s\n",
        node->source_location.start,
        (int) type_name.size, type_name.data);
      free(type_name.data);

      Nodes nodes = {0};
      if (node->call.callee) VECTOR_PUSH(nodes, node->call.callee);
      VECTOR_APPEND_ALL(nodes, node->call.arguments);
      ast_print_children(file, logical_parent, node, &nodes, leading_text);
      VECTOR_DELETE(nodes);
    } break;

    case NODE_CAST: {
      string type_name = ast_typename(node->type, true);
      fprintf(file, "\033[31mCast \033[35m<%d> %.*s\n",
        node->source_location.start,
        (int) type_name.size, type_name.data);
      free(type_name.data);
      ast_print_children(file, logical_parent, node, &(Nodes) {
        .data = (Node *[]) {node->cast.value},
        .size = 1
      }, leading_text);
    } break;

    case NODE_BINARY: {
      string type_name = ast_typename(node->type, true);
      fprintf(file, "\033[31mBinary \033[35m<%d> \033[32m%s \033[31m: %.*s\n",
        node->source_location.start,
        token_type_to_string(node->binary.op),
        (int) type_name.size, type_name.data);
      free(type_name.data);

      ast_print_children(file, logical_parent, node, &(Nodes) {
        .data = (Node *[]) {node->binary.lhs, node->binary.rhs},
        .size = 2
      }, leading_text);
    } break;

    case NODE_UNARY: {
      string type_name = ast_typename(node->type, true);
      fprintf(file, "\033[31mUnary \033[35m<%d> \033[32m%s \033[31m: %.*s\n",
        node->source_location.start,
        token_type_to_string(node->unary.op),
        (int) type_name.size, type_name.data);
      free(type_name.data);

      ast_print_children(file, logical_parent, node, &(Nodes) {
        .data = (Node *[]) {node->unary.value},
        .size = 1
      }, leading_text);
    } break;

    case NODE_LITERAL: {
      switch (node->literal.type) {
        case TK_NUMBER: {
          fprintf(file, "\033[31mLiteral \033[35m<%d> \033[35m%zi \033[36minteger\n",
            node->source_location.start,
            (isz) node->literal.integer);
        } break;

        case TK_STRING: {
          /// TODO: Get the actual string data from the AST.
          fprintf(file, "\033[31mLiteral \033[35m<%d> \033[33m%zu \033[36mstring\n",
            node->source_location.start,
            (isz) node->literal.string_index);
        } break;

        default: TODO("Print literal of type %d", node->literal.type);
      }
    } break;

    case NODE_VARIABLE_REFERENCE: {
      string type_name = ast_typename(node->type, true);
      fprintf(file, "\033[31mVar \033[35m<%d> \033[m\033[38m%.*s \033[31m: %.*s\n",
        node->source_location.start,
        (int) node->var->name.size, node->var->name.data,
        (int) type_name.size, type_name.data);
      free(type_name.data);
    } break;

    case NODE_FUNCTION_REFERENCE: {
      /// If our parent is the root, print the function instead.
      if (node->parent && node->parent->kind == NODE_ROOT)
        return ast_print_node(file, logical_parent, node->funcref->node, leading_text);

      /// Otherwise, print the function reference.
      string type_name = ast_typename(node->type, true);
      fprintf(file, "\033[31mFunction Ref \033[35m<%d> \033[32m%.*s \033[31m: %.*s\n",
        node->source_location.start,
        (int) node->funcref->name.size, node->funcref->name.data,
        (int) type_name.size, type_name.data);
      free(type_name.data);
    } break;
  }
}

/// Scope tree for printing scopes.
typedef struct scope_tree_node {
  const Scope *scope;
  VECTOR (struct scope_tree_node*) children;
} scope_tree_node;

/// Print a scope and the symbols it contains.
static void print_scope(FILE *file, scope_tree_node *node, char buffer[static AST_PRINT_BUFFER_SIZE]) {
    fprintf(file, "\033[31mScope\n");
    const Scope *s = node->scope;

    /// Print all symbols in this scope.
    VECTOR_FOREACH_PTR (Symbol*, sym, s->symbols) {
        /// Print the leading text.
        bool last_child = sym_ptr == s->symbols.data + s->symbols.size - 1 && !node->children.size;
        fprintf(file, "\033[31m%s%s", buffer, last_child ? "└─" : "├─");

        /// Print symbol.
        switch (sym->kind) {
            case SYM_TYPE:
                fprintf(file, "Type: \033[36m%.*s\033[31m -> ", (int) sym->name.size, sym->name.data);
                if (sym->type) {
                    string type_name = ast_typename(sym->type, true);
                    fprintf(file, "%.*s", (int) type_name.size, type_name.data);
                    free(type_name.data);
                } else {
                    fprintf(file, "\033[m???");
                }
                break;

            case SYM_VARIABLE:
                fprintf(file, "Variable: \033[m%.*s\033[31m : ", (int) sym->name.size, sym->name.data);
                if (sym->node->type) {
                    string type_name = ast_typename(sym->node->type, true);
                    fprintf(file, "%.*s", (int) type_name.size, type_name.data);
                    free(type_name.data);
                } else {
                    fprintf(file, "\033[m???");
                }
                break;

            case SYM_FUNCTION:
                fprintf(file, "Function \033[32m%.*s\033[31m : ", (int) sym->name.size, sym->name.data);
                if (sym->node->type) {
                    string type_name = ast_typename(sym->node->type, true);
                    fprintf(file, "%.*s", (int) type_name.size, type_name.data);
                    free(type_name.data);
                } else {
                    fprintf(file, "\033[m???");
                }
                break;
            default: UNREACHABLE();
        }

        /// Line break.
        fprintf(file, "\n");
    }

    /// Next, print all child scopes.
    VECTOR_FOREACH_PTR (scope_tree_node *, child, node->children) {
        /// Print the leading text.
        bool last_child = child_ptr == node->children.data + node->children.size - 1;
        fprintf(file, "\033[31m%s%s", buffer, last_child ? "└─" : "├─");

        /// Append the leading text for the next scope.
        usz sz = strlen(last_child ? "  " : "│ ");
        strncat(buffer, last_child ? "  " : "│ ", AST_PRINT_BUFFER_SIZE - strlen(buffer) - 1);

        /// Print the scope.
        print_scope(file, child, buffer);

        /// Remove the leading text for the next scope.
        buffer[strlen(buffer) - sz] = 0;
    }
}

/// Print the scope tree of an AST.
void ast_print_scope_tree(FILE *file, const AST *ast) {
    /// First, we need to build the scope tree.
    VECTOR(scope_tree_node) scope_tree = {0};

    /// Create a node for each scope.
    VECTOR_FOREACH_PTR (Scope*, sc, ast->_scopes_) {
        scope_tree_node node = {0};
        node.scope = sc;
        VECTOR_PUSH(scope_tree, node);
    }

    /// Now, we need to build the tree.
    VECTOR_FOREACH (scope_tree_node, node, scope_tree) {
        /// If this scope has a parent, add it to the parent's children.
        if (node->scope->parent) {
            scope_tree_node *n;
            VECTOR_FIND_IF(scope_tree, n, i, scope_tree.data[i].scope == node->scope->parent);
            ASSERT(n);
            VECTOR_PUSH(n->children, node);
        }
    }

    /// Now, we can print the tree.
    char buffer[AST_PRINT_BUFFER_SIZE] = {0};
    print_scope(file, scope_tree.data, buffer);
    VECTOR_DELETE(scope_tree);
}

/// Print an AST.
void ast_print(FILE *file, const AST *ast) {
  char buffer[AST_PRINT_BUFFER_SIZE] = {0};

  /// Print the root node.
  ast_print_node(file, NULL, ast->root, buffer);
}

/// Print the children of a node.
static void ast_print_children(
    FILE *file,
    const Node *logical_grandparent,
    const Node *logical_parent,
    const Nodes *nodes,
    char leading_text[static AST_PRINT_BUFFER_SIZE]
) {
  /// If the logical parent is merely used here, and not defined,
  /// then don’t do anything just yet.
  if (logical_parent->parent != logical_grandparent) return;

  /// Make sure we can append to the leading text.
  usz len = strlen(leading_text);
  ASSERT(len + sizeof("│ ") < AST_PRINT_BUFFER_SIZE);

  /// Print the children.
  VECTOR_FOREACH_PTR (Node*, node, *nodes) {
    /// Print the indentation and continue any lines from parent nodes.
    fprintf(file, "\033[31m%s%s", leading_text, node == VECTOR_BACK(*nodes) ? "└─" : "├─");

    /// Update the leading text.
    strncat(leading_text, node == VECTOR_BACK(*nodes) ? "  " : "│ ", AST_PRINT_BUFFER_SIZE - len);

    /// Print the node.
    ast_print_node(file, logical_parent, node, leading_text);

    /// Restore the leading text.
    leading_text[len] = 0;
  }
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