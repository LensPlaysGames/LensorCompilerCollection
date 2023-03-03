#include <ast.h>
#include <parser.h>

/// ===========================================================================
///  Builtin types.
/// ===========================================================================
/*typedef struct TypePrimitive {
  usz size;
  usz alignment;
  span name;
  bool is_signed;
  /// A unique ID that is used to compare primitives.
  uint8_t id;
} TypePrimitive;*/
static Type t_void_def = {
  .kind = TYPE_PRIMITIVE,
  .source_location = {0},
  .primitive = {
    .size = 0,
    .alignment = 0,
    .name = literal_span_raw("void"),
  },
};

static Type t_void_pointer_def = {
  .kind = TYPE_POINTER,
  .source_location = {0},
  .pointer = { .to = &t_void_def },
};

static Type t_integer_literal_def = {
  .kind = TYPE_PRIMITIVE,
  .source_location = {0},
  .primitive = {
    .size = 8,
    .alignment = 8,
    .name = literal_span_raw("<integer_literal>"),
    .is_signed = true,
  },
};

static Type t_integer_def = {
  .kind = TYPE_PRIMITIVE,
  .source_location = {0},
  .primitive = {
    .size = 8,
    .alignment = 8,
    .name = literal_span_raw("integer"),
    .is_signed = true,
  },
};

static Type t_byte_def = {
  .kind = TYPE_PRIMITIVE,
  .source_location = {0},
  .primitive = {
    .size = 1,
    .alignment = 1,
    .name = literal_span_raw("byte"),
    .is_signed = false,
  },
};

Type * const t_void = &t_void_def;
Type * const t_void_ptr = &t_void_pointer_def;
Type * const t_integer_literal = &t_integer_literal_def;
Type * const t_integer = &t_integer_def;
Type * const t_byte = &t_byte_def;

/// ===========================================================================
///  Scope/symbol functions.
/// ===========================================================================
static Scope *scope_create(Scope *parent) {
  Scope *scope = calloc(1, sizeof(Scope));
  scope->parent = parent;
  return scope;
}

static void scope_delete(Scope *scope) {
  foreach_ptr (Symbol *, symbol, scope->symbols) {
    free(symbol->name.data);
    free(symbol);
  }

  vector_delete(scope->symbols);
  vector_delete(scope->children);
  free(scope);
}

void scope_push(AST *ast) {
  ASSERT(ast->scope_stack.size, "AST must have a global scope.");
  Scope *scope = scope_create(vector_back(ast->scope_stack));
  vector_push(ast->scope_stack, scope);
  vector_push(ast->_scopes_, scope);
}

void scope_pop(AST *ast) {
  ASSERT(ast->scope_stack.size > 1, "Cannot pop the global scope.");
  (void) vector_pop(ast->scope_stack);
}

Symbol *scope_add_symbol_unconditional(Scope *scope, enum SymbolKind kind, span name, void *value) {
  Symbol *symbol = calloc(1, sizeof(Symbol));
  symbol->kind = kind;
  symbol->name = string_dup(name);
  symbol->scope = scope;
  if (kind == SYM_TYPE) symbol->val.type = value;
  else symbol->val.node = value;
  vector_push(scope->symbols, symbol);
  return symbol;
}


Symbol *scope_add_symbol(Scope *scope, enum SymbolKind kind, span name, void *value) {
  // Check if the symbol already exists.
  if (scope_find_symbol(scope, name, true)) return NULL;
  return scope_add_symbol_unconditional(scope, kind, name, value);
}

Symbol *scope_find_symbol(Scope *scope, span name, bool this_scope_only) {
  while (scope) {
    /// Return the symbol if it exists.
    foreach_ptr (Symbol *, symbol, scope->symbols)
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
  vector_push(ast->_nodes_, node);
  return node;
}

/// Internal helper to create a type.
NODISCARD static Type *mktype(AST *ast, enum TypeKind kind, loc source_location) {
  Type *type = calloc(1, sizeof(Type));
  type->kind = kind;
  type->source_location = source_location;
  vector_push(ast->_types_, type);
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

  vector_push(ast->functions, node);
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

/// Create a new for expression.
Node *ast_make_for(
    AST *ast,
    loc source_location,
    Node *init,
    Node *condition,
    Node *iterator,
    Node *body
) {
  Node *node = mknode(ast, NODE_FOR, source_location);
  node->for_.init = init;
  node->for_.condition = condition;
  node->for_.iterator = iterator;
  node->for_.body = body;
  body->parent = node;
  iterator->parent = node;
  condition->parent = node;
  init->parent = node;
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
  foreach_ptr (Node *, child, children) child->parent = node;
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
  foreach_ptr (Node *, argument, arguments) argument->parent = node;
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

Node *ast_make_structure_declaration(
    AST *ast,
    loc source_location,
    Symbol *symbol
) {
  Node *node = mknode(ast, NODE_STRUCTURE_DECLARATION, source_location);
  node->struct_decl = symbol;
  return node;
}

Node *ast_make_member_access(
    AST *ast,
    loc source_location,
    span ident,
    Node *struct_
) {
  Node *node = mknode(ast, NODE_MEMBER_ACCESS, source_location);
  node->member_access.ident = string_dup(ident);
  node->member_access.struct_ = struct_;
  return node;
}

/// Create a new function reference.
Node *ast_make_function_reference(
    AST *ast,
    loc source_location,
    span symbol
) {
  Node *node = mknode(ast, NODE_FUNCTION_REFERENCE, source_location);
  node->funcref.name = string_dup(symbol);
  node->funcref.resolved = NULL;
  node->funcref.scope = vector_back(ast->scope_stack);
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


/// Create a new struct type.
Type *ast_make_type_struct(
    AST *ast,
    loc source_location,
    Members members
) {
  Type *type = mktype(ast, TYPE_STRUCT, source_location);
  type->structure.members = members;
  return type;
}

/// ===========================================================================
///  AST query functions.
/// ===========================================================================
/// Used by `typename`.
static void write_typename(string_buffer *s, const Type *type) {
  if (!type) {
    format_to(s, "<null>");
    return;
  }

  /// Print the type.
  switch (type->kind) {
    default:
      format_to(s, "<unknown>");
      return;

    case TYPE_PRIMITIVE:
      format_to(s, "%36%S", type->primitive.name);
      return;

    case TYPE_NAMED:
      format_to(s, "%36%S", type->named->name);
      break;

    case TYPE_POINTER: {
      bool func = type->pointer.to->kind == TYPE_FUNCTION;
      format_to(s, "%36@");
      if (func) format_to(s, "%31(");
      write_typename(s, type->pointer.to);
      if (func) format_to(s, "%31)");
    } break;

    case TYPE_ARRAY:
      write_typename(s, type->array.of);
      format_to(s, "%31[%35%Z%31]", type->array.size);
      break;

    case TYPE_FUNCTION:
      write_typename(s, type->function.return_type);
      format_to(s, "%31(");

      /// Parameters.
      foreach (Parameter, param, type->function.parameters) {
        write_typename(s, param->type);
        if (param != type->function.parameters.data + type->function.parameters.size - 1)
          format_to(s, "%31, ");
      }
      format_to(s, "%31)");
      break;

    case TYPE_STRUCT:
      format_to(s, "struct");
      if (type->structure.decl) {
        format_to(s, " %S", type->structure.decl->struct_decl->name);
      }
      break;
  }
}

string typename(Type *type, bool colour) {
  bool save_thread_use_colours = thread_use_colours;
  thread_use_colours = colour;

  string_buffer buf = {0};
  format_to(&buf, "%m");
  write_typename(&buf, type);
  format_to(&buf, "%m");

  thread_use_colours = save_thread_use_colours;
  return (string){buf.data, buf.size};
}

Type *type_canonical(Type *type) {
    while (type && type->kind == TYPE_NAMED) type = type->named->val.type;
    return type;
}

Type *type_last_alias(Type *type) {
    while (type && type->kind == TYPE_NAMED && type->named->val.type)
      type = type->named->val.type;
    return type;
}

bool type_is_incomplete(Type *type) {
  return type_is_incomplete_canon(type_canonical(type));
}

bool type_is_incomplete_canon(Type *type) {
  return !type || type == t_void;
}

usz type_sizeof(Type *type) {
  switch (type->kind) {
    default: ICE("Invalid type kind: %d", type->kind);
    case TYPE_PRIMITIVE: return type->primitive.size;
    case TYPE_NAMED: return type->named->val.type ? type_sizeof(type->named->val.type) : 0;
    case TYPE_POINTER: return sizeof(void *);
    case TYPE_ARRAY: return type->array.size * type_sizeof(type->array.of);
    case TYPE_FUNCTION: return sizeof(void *);
    case TYPE_STRUCT: return type->structure.byte_size;
  }
}

usz type_alignof(Type *type) {
switch (type->kind) {
    default: ICE("Invalid type kind: %d", type->kind);
    case TYPE_PRIMITIVE: return type->primitive.alignment;
    case TYPE_NAMED: return type->named->val.type ? type_alignof(type->named->val.type) : 0;
    case TYPE_POINTER: return _Alignof(void *);
    case TYPE_ARRAY: return type->array.size * type_alignof(type->array.of);
    case TYPE_FUNCTION: return _Alignof(void *);
    case TYPE_STRUCT: return type->structure.alignment;
  }
}

bool type_is_void(Type *type) {
  return type_canonical(type) == t_void;
}

bool type_is_pointer(Type *type) {
  Type * t = type_canonical(type);
  return t && t->kind == TYPE_POINTER;
}

bool type_is_array(Type *type) {
  Type * t = type_canonical(type);
  return t && t->kind == TYPE_ARRAY;
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
  vector_push(ast->scope_stack, scope_create(NULL));
  vector_push(ast->_scopes_, vector_back(ast->scope_stack));

  /// Add the builtin types to the global scope.
  scope_add_symbol(ast->_scopes_.data[0], SYM_TYPE, literal_span("integer"), t_integer);
  scope_add_symbol(ast->_scopes_.data[0], SYM_TYPE, literal_span("byte"), t_byte);
  scope_add_symbol(ast->_scopes_.data[0], SYM_TYPE, literal_span("void"), t_void);

  /// Done.
  return ast;
}

/// Free an AST.
void ast_free(AST *ast) {
  /// Some nodes may contain strings, vectors, etc.. Iterate over all
  /// nodes and free all resources they may have.
  foreach_ptr (Node *, node, ast->_nodes_) {
    switch (node->kind) {
      case NODE_FUNCTION:
        free(node->function.name.data);
        vector_delete(node->function.param_decls);
        continue;

      case NODE_ROOT: vector_delete(node->root.children); continue;
      case NODE_BLOCK: vector_delete(node->block.children); continue;
      case NODE_CALL: vector_delete(node->call.arguments); continue;
      case NODE_DECLARATION: free(node->declaration.name.data); continue;
      case NODE_MEMBER_ACCESS: free(node->member_access.ident.data); continue;

      case NODE_IF:
      case NODE_WHILE:
      case NODE_FOR:
      case NODE_CAST:
      case NODE_BINARY:
      case NODE_UNARY:
      case NODE_LITERAL:
      case NODE_VARIABLE_REFERENCE:
      case NODE_STRUCTURE_DECLARATION:
        continue;
      case NODE_FUNCTION_REFERENCE:
        free(node->funcref.name.data);
        continue;
    }
    UNREACHABLE();
  }

  /// Now that that’s done, free all nodes.
  foreach_ptr (Node *, node, ast->_nodes_) free(node);
  vector_delete(ast->_nodes_);
  vector_delete(ast->functions);

  /// Free all types.
  foreach_ptr (Type *, type, ast->_types_) {
    if (type->kind == TYPE_FUNCTION) {
      foreach (Parameter, param, type->function.parameters) free(param->name.data);
      vector_delete(type->function.parameters);
    } else if (type->kind == TYPE_STRUCT) {
      foreach (Member, member, type->structure.members) free(member->name.data);
      vector_delete(type->structure.members);
    }
    free(type);
  }
  vector_delete(ast->_types_);

  /// Free all scopes. This also deletes all symbols.
  foreach_ptr (Scope *, scope, ast->_scopes_) scope_delete(scope);
  vector_delete(ast->_scopes_);
  vector_delete(ast->scope_stack);

  /// Free all interned strings.
  foreach (string, s, ast->strings) free(s->data);
  vector_delete(ast->strings);

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
    string_buffer *buf
);

static void print_struct_members(FILE *file, Members *members, string_buffer *leading_text) {
  foreach(Member, member, *members) {
    fprint(file, "%31%S%s", as_span(*leading_text), member == &vector_back(*members) ? "└─" : "├─");
    if (member->type->kind == TYPE_STRUCT && !member->type->structure.decl) {
      fprint(file, "%31Struct %35%S%31@%35%Z%31 %31#%35%Z%31/%35%Z\n",
             member->name,
             member->byte_offset,
             member->type->structure.byte_size,
             member->type->structure.alignment);

      /// Update the leading text.
      usz sz = leading_text->size;
      format_to(leading_text, "%s", member == &vector_back(*members) ? "  " : "│ ");

      /// Print the node.
      print_struct_members(file, &member->type->structure.members, leading_text);

      /// Restore the leading text.
      leading_text->size = sz;
    } else fprint(file, "%31Member %35%S%31@%35%Z%31: %T\n", member->name, member->byte_offset, member->type);
  }
}

/// Print a node.
void ast_print_node_internal(
  FILE *file,
  const Node *logical_parent,
  const Node *node,
  string_buffer *leading_text
) {
  switch (node->kind) {
    default: TODO("Print node of type %d", node->kind);

    case NODE_ROOT: {
      fprint(file, "%31Root %35<%u>\n", node->source_location.start);
      ast_print_children(file, logical_parent, node, &node->root.children, leading_text);
    } break;

    case NODE_FUNCTION: {
      /// Print the function name and type.
      fprint(file, "%31Function %35<%u> %32%S %31: %T\n",
        node->source_location.start,
        node->function.name,
        node->type);

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
      fprint(file, "%31Declaration %35<%u> %38%S %31: %T\n",
        node->source_location.start,
        node->declaration.name,
        node->type);

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
      fprint(file, "%31If %35<%u> %T\n", node->source_location.start, node->type);

      /// Print the condition and branches.
      ast_print_children(file, logical_parent, node, &(Nodes) {
        .data = (Node *[]) {node->if_.condition, node->if_.then, node->if_.else_},
        .size = node->if_.else_ ? 3 : 2
      }, leading_text);
    } break;

    case NODE_WHILE: {
      fprint(file, "%31While %35<%u>\n", node->source_location.start);
      ast_print_children(file, logical_parent, node, &(Nodes) {
        .data = (Node *[]) {node->while_.condition, node->while_.body},
        .size = 2
      }, leading_text);
    } break;

    case NODE_FOR: {
      fprint(file, "%31For %35<%u>\n", node->source_location.start);
      ast_print_children(file, logical_parent, node, &(Nodes) {
        .data = (Node *[]) {
          node->for_.init,
          node->for_.condition,
          node->for_.iterator,
          node->for_.body
        },
        .size = 4
      }, leading_text);
    } break;

    case NODE_BLOCK: {
      fprint(file, "%31Block %35<%u> %T\n", node->source_location.start, node->type);
      ast_print_children(file, logical_parent, node, &node->block.children, leading_text);
    } break;

    case NODE_CALL: {
      fprint(file, "%31Call %35<%u> %T\n", node->source_location.start, node->type);

      Nodes nodes = {0};
      if (node->call.callee) vector_push(nodes, node->call.callee);
      vector_append_all(nodes, node->call.arguments);
      ast_print_children(file, logical_parent, node, &nodes, leading_text);
      vector_delete(nodes);
    } break;

    case NODE_CAST: {
      fprint(file, "%31Cast %35<%u> %T\n", node->source_location.start, node->type);
      ast_print_children(file, logical_parent, node, &(Nodes) {
        .data = (Node *[]) {node->cast.value},
        .size = 1
      }, leading_text);
    } break;

    case NODE_BINARY: {
      fprint(file, "%31Binary %35<%u> %32%s %31: %T\n",
        node->source_location.start,
        token_type_to_string(node->binary.op),
        node->type);

      ast_print_children(file, logical_parent, node, &(Nodes) {
        .data = (Node *[]) {node->binary.lhs, node->binary.rhs},
        .size = 2
      }, leading_text);
    } break;

    case NODE_UNARY: {
      fprint(file, "%31Unary %35<%u> %32%s %31: %T\n",
        node->source_location.start,
        token_type_to_string(node->unary.op),
        node->type);

      ast_print_children(file, logical_parent, node, &(Nodes) {
        .data = (Node *[]) {node->unary.value},
        .size = 1
      }, leading_text);
    } break;

    case NODE_LITERAL: {
      switch (node->literal.type) {
        case TK_NUMBER: {
          fprint(file, "%31Literal %35<%u> %35%D %36integer\n",
            node->source_location.start,
            (i64) node->literal.integer);
        } break;

        case TK_STRING: {
          /// TODO: Get the actual string data from the AST.
          fprint(file, "%31Literal %35<%u> %33%Z %36string\n",
            node->source_location.start,
            (usz) node->literal.string_index);
        } break;

        default: TODO("Print literal of type %d", node->literal.type);
      }
    } break;

    case NODE_VARIABLE_REFERENCE: {
      fprint(file, "%31Var %35<%u> %38%S %31: %T\n",
        node->source_location.start,
        node->var->name,
        node->type);
    } break;

    case NODE_FUNCTION_REFERENCE: {
      /// Otherwise, print the function reference.
      fprint(file, "%31Function Ref %35<%u> %32%S %31: %T\n",
        node->source_location.start,
        node->funcref.name,
        node->type);
    } break;

    case NODE_STRUCTURE_DECLARATION: {
      /// Print the struct declaration.
      // here
      fprint(file, "%31Structure Declaration %35<%u> %36%S%m %31#%35%Z%31/%35%Z %31\n",
             node->source_location.start,
             node->struct_decl->name,
             node->struct_decl->val.type->structure.byte_size,
             node->struct_decl->val.type->structure.alignment);
      print_struct_members(file, &node->struct_decl->val.type->structure.members, leading_text);
    } break;

    case NODE_MEMBER_ACCESS: {
      fprint(file, "%31Member Access %35<%u> %36%T%31.%35%S %31: %T\n",
             node->source_location.start,
             node->member_access.struct_->type,
             node->member_access.ident,
             node->type);

      ast_print_children(file, logical_parent, node, &(Nodes) {
        .data = (Node *[]) {node->member_access.struct_},
        .size = 1
      }, leading_text);
    } break;
  }

  fprint(file, "%m");
}

void ast_print_node(const Node *node) {
  string_buffer buf = {0};
  ast_print_node_internal(stdout, NULL, node, &buf);
  vector_delete(buf);
}

/// Scope tree for printing scopes.
typedef struct scope_tree_node {
  const Scope *scope;
  Vector(struct scope_tree_node*) children;
} scope_tree_node;

/// Print a scope and the symbols it contains.
static void print_scope(FILE *file, scope_tree_node *node, string_buffer *buf) {
    fprint(file, "%31Scope\n");
    const Scope *s = node->scope;

    /// Print all symbols in this scope.
    foreach_ptr (Symbol*, sym, s->symbols) {
        /// Print the leading text.
        bool last_child = sym_ptr == s->symbols.data + s->symbols.size - 1 && !node->children.size;
        fprint(file, "%31%S%s", as_span(*buf), last_child ? "└─" : "├─");

        /// Print symbol.
        switch (sym->kind) {
            default: UNREACHABLE();
            case SYM_TYPE: fprint(file, "Type: %36%S%31 -> %T", sym->name, sym->val.type); break;
            case SYM_VARIABLE: fprint(file, "Variable: %m%S%31 : %T", sym->name, sym->val.node->type); break;
            case SYM_FUNCTION: fprint(file, "Function %32%S%31 : %T", sym->name, sym->val.node->type); break;
        }

        /// Line break.
        fprint(file, "\n");
    }

    /// Next, print all child scopes.
    foreach_ptr (scope_tree_node *, child, node->children) {
        /// Print the leading text.
        bool last_child = child_ptr == node->children.data + node->children.size - 1;
        fprint(file, "%31%S%s", as_span(*buf), last_child ? "└─" : "├─");

        /// Append the leading text for the next scope.
        usz sz = buf->size;
        format_to(buf, "%s", last_child ? "  " : "│ ");

        /// Print the scope.
        print_scope(file, child, buf);

        /// Remove the leading text for the next scope.
        buf->size = sz;
    }

    fprint(file, "%m");
}

/// Print the scope tree of an AST.
void ast_print_scope_tree(FILE *file, const AST *ast) {
    /// First, we need to build the scope tree.
    Vector(scope_tree_node) scope_tree = {0};

    /// Create a node for each scope.
    foreach_ptr (Scope*, sc, ast->_scopes_) {
        scope_tree_node node = {0};
        node.scope = sc;
        vector_push(scope_tree, node);
    }

    /// Now, we need to build the tree.
    foreach (scope_tree_node, node, scope_tree) {
        /// If this scope has a parent, add it to the parent's children.
        if (node->scope->parent) {
            scope_tree_node *n;
            vector_find_if(scope_tree, n, i, scope_tree.data[i].scope == node->scope->parent);
            ASSERT(n);
            vector_push(n->children, node);
        }
    }

    /// Now, we can print the tree.
    string_buffer buf = {0};
    print_scope(file, scope_tree.data, &buf);
    vector_delete(scope_tree);
}

/// Print an AST.
void ast_print(FILE *file, const AST *ast) {
  string_buffer buf = {0};

  /// Print the root node.
  ast_print_node_internal(file, NULL, ast->root, &buf);
}

/// Print the children of a node.
static void ast_print_children(
    FILE *file,
    const Node *logical_grandparent,
    const Node *logical_parent,
    const Nodes *nodes,
    string_buffer *buf
) {
  /// If the logical parent is merely used here, and not defined,
  /// then don’t do anything just yet.
  if (logical_parent->parent != logical_grandparent) return;

  /// Print the children.
  foreach_ptr (Node*, node, *nodes) {
    /// Print the indentation and continue any lines from parent nodes.
    fprint(file, "%31%S%s", as_span(*buf), node == vector_back(*nodes) ? "└─" : "├─");

    /// Update the leading text.
    usz sz = buf->size;
    format_to(buf, "%s", node == vector_back(*nodes) ? "  " : "│ ");

    /// Print the node.
    ast_print_node_internal(file, logical_parent, node, buf);

    /// Restore the leading text.
    buf->size = sz;
  }
}

/// Intern a string.
size_t ast_intern_string(AST *ast, span str) {
  /// Check if the string is already interned.
  foreach_index(i, ast->strings)
    if (string_eq(ast->strings.data[i], str)) return i;

  /// Intern the string.
  vector_push(ast->strings, string_dup(str));
  return ast->strings.size - 1;
}

/// Replace a node with another node.
void ast_replace_node(AST *ast, Node *old, Node *new) {
#define REPLACE_IN_CHILDREN(children)                              \
  do {                                                             \
    Node **ptr = NULL;                                             \
    vector_find_if((children), ptr, i, (children).data[i] == old); \
    if (ptr) *ptr = new;                                           \
  } while (0)

  (void) ast;

  /// Find the node in the parent.
  ASSERT(old->parent);
  switch (old->parent->kind) {
    case NODE_ROOT:
      REPLACE_IN_CHILDREN(old->parent->root.children);
      break;

    case NODE_FUNCTION:
      REPLACE_IN_CHILDREN(old->parent->function.param_decls);
      if (old->parent->function.body == old) old->parent->function.body = new;
      break;

    case NODE_DECLARATION:
      if (old->parent->declaration.init == old) old->parent->declaration.init = new;
      break;

    case NODE_IF:
      if (old->parent->if_.condition == old) old->parent->if_.condition = new;
      else if (old->parent->if_.then == old) old->parent->if_.then = new;
      else if (old->parent->if_.else_ == old) old->parent->if_.else_ = new;
      break;

    case NODE_WHILE:
      if (old->parent->while_.condition == old) old->parent->while_.condition = new;
      else if (old->parent->while_.body == old) old->parent->while_.body = new;
      break;

    case NODE_BLOCK:
      REPLACE_IN_CHILDREN(old->parent->block.children);
      break;

    case NODE_CALL:
      REPLACE_IN_CHILDREN(old->parent->call.arguments);
      if (old->parent->call.callee == old) old->parent->call.callee = new;
      break;

    case NODE_CAST:
      if (old->parent->cast.value == old) old->parent->cast.value = new;
      break;

    case NODE_BINARY:
      if (old->parent->binary.lhs == old) old->parent->binary.lhs = new;
      else if (old->parent->binary.rhs == old) old->parent->binary.rhs = new;
      break;

    case NODE_UNARY:
      if (old->parent->unary.value == old) old->parent->unary.value = new;
      break;

    case NODE_MEMBER_ACCESS:
      if (old->parent->member_access.struct_ == old) old->parent->member_access.struct_ = new;
      break;

    case NODE_LITERAL:
    case NODE_VARIABLE_REFERENCE:
    case NODE_FUNCTION_REFERENCE:
    case NODE_STRUCTURE_DECLARATION:
      break;
  }

#undef REPLACE_IN_CHILDREN
}
