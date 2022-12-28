#include <typechecker.h>

#include <error.h>
#include <parser.h>

#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define DIAG(diag, loc, ...)                                                                  \
  do {                                                                                        \
    issue_diagnostic(diag, (ast)->filename.data, as_span((ast)->source), (loc), __VA_ARGS__); \
    return false;                                                                             \
  } while (0)
#define ERR(loc, ...)   DIAG(DIAG_ERR, loc, __VA_ARGS__)
#define SORRY(loc, ...) DIAG(DIAG_SORRY, loc, __VA_ARGS__)

#define ERR_DO(code, loc, ...)                                                                    \
  do {                                                                                            \
    issue_diagnostic(DIAG_ERR, (ast)->filename.data, as_span((ast)->source), (loc), __VA_ARGS__); \
    code;                                                                                         \
    return false;                                                                                 \
  } while (0)

#define ERR_NOT_CONVERTIBLE(to, from)                                        \
  do {                                                                       \
    string to_str = ast_typename(to, false);                                 \
    string from_str = ast_typename(from, false);                             \
    ERR_DO(free(to_str.data); free(from_str.data), from->source_location,    \
      "Type \"%.*s\" is not convertible to \"%.*s\"",                        \
        (int) from_str.size, from_str.data, (int) to_str.size, to_str.data); \
  } while (0)

/// Check if two types are equal.
NODISCARD static bool types_equal(Type *a, Type *b) {
  ASSERT(a && b);

  /// Expand named types.
  while (a->kind == TYPE_NAMED && a->named->type) a = a->named->type;
  while (b->kind == TYPE_NAMED && b->named->type) b = b->named->type;

  /// If the type kinds are not the same, the the types are obviously not equal.
  if (a->kind != b->kind) return false;

  /// Compare the types.
  switch (a->kind) {
    default: ICE("Invalid type kind %d", a->kind);
    case TYPE_NAMED: UNREACHABLE();
    case TYPE_PRIMITIVE: return a->primitive.id == b->primitive.id;
    case TYPE_POINTER: return types_equal(a->pointer.to, b->pointer.to);
    case TYPE_ARRAY: return a->array.size == b->array.size && types_equal(a->array.of, b->array.of);
    case TYPE_FUNCTION: {
      if (a->function.parameters.size != b->function.parameters.size) return false;
      if (!types_equal(a->function.return_type, b->function.return_type)) return false;
      VECTOR_FOREACH_INDEX (i, a->function.parameters)
        if (!types_equal(a->function.parameters.data[i].type, b->function.parameters.data[i].type))
          return false;
      return true;
    }
  }
}

/// Check if from is convertible to to.
NODISCARD static bool convertible(Type *to, Type *from) {
  /// If the types are the same, they are convertible.
  if (types_equal(to, from)) return true;

  /// A function type is implicitly convertible to its
  /// corresponding pointer type.
  if (to->kind == TYPE_POINTER && from->kind == TYPE_FUNCTION)
    return types_equal(to->pointer.to, from);

  /// Otherwise, the types are not convertible.
  return false;
}

/// Get the common type of two types.
NODISCARD static Type *common_type(Type *a, Type *b) {
  /// TODO: integer stuff.
  if (types_equal(a, b)) return a;
  return NULL;
}

/// Check if a type is a pointer type.
NODISCARD static bool is_pointer(Type *type) { return type->kind == TYPE_POINTER; }

/// Check if a type is an array type.
NODISCARD static bool is_array(Type *type) { return type->kind == TYPE_ARRAY; }

/// Check if a type is an integer type.
NODISCARD static bool is_integer(Type *type) {
  /// Currently, all primitive types are integers.
  while (type->kind == TYPE_NAMED && type->named->type) type = type->named->type;
  return type->kind == TYPE_PRIMITIVE;
}

/// Check if an expression is an lvalue.
NODISCARD static bool is_lvalue(Node *expr) {
  switch (expr->kind) {
    default: return false;

    /// Declarations and variables are obviously lvalues.
    case NODE_DECLARATION:
    case NODE_VARIABLE_REFERENCE:
      return true;

    /// A dereference is an lvalue.
    case NODE_UNARY: return expr->unary.op == TK_AT;
  }
}

/// Resolve a function reference.
NODISCARD static bool resolve_function(AST *ast, Node *func) {
  if (func->kind == NODE_FUNCTION_REFERENCE && !func->funcref->node) {
    Symbol *sym = scope_find_symbol(func->funcref->scope, as_span(func->funcref->name), false);
    if (!sym || !sym->node) ERR(func->source_location, "Unknown symbol \"%.*s\".",
      (int) func->funcref->name.size, func->funcref->name.data);
    func->funcref->node = sym->node;
  }
  return true;
}

NODISCARD bool typecheck_expression(AST *ast, Node *expr) {
  /// Don’t typecheck the same expression twice.
  if (expr->type_checked) return true;
  expr->type_checked = true;

  /// Typecheck the expression.
  switch (expr->kind) {
    default: ICE("Invalid node type");

    /// Typecheck each child of the root.
    case NODE_ROOT:
      VECTOR_FOREACH_PTR (Node *, node, expr->root.children)
        if (!typecheck_expression(ast, node))
          return false;

      /// Replace function references in the root with the function nodes
      /// iff the source location of the function is the same as that of
      /// the function reference.
      ///
      /// This is so that if someone, for whatever reason, puts the name
      /// of the function as an expression in the root, it will just be
      /// removed rather than replaced with the function.
      VECTOR_FOREACH_INDEX(i, expr->root.children) {
        Node *node = expr->root.children.data[i];
        if (node->kind == NODE_FUNCTION_REFERENCE) {
          Node *func = node->funcref->node;
          if (
            func &&
            func->source_location.start == node->source_location.start &&
            func->source_location.end == node->source_location.end
          ) { expr->root.children.data[i] = func; }
        }
      }

      break;

    /// Typecheck the function body if there is one.
    case NODE_FUNCTION:
      if (!expr->function.body) break;
      if (!typecheck_expression(ast, expr->function.body)) return false;

      /// Make sure the return type of the body is convertible to that of the function.
      if (!convertible(expr->type->function.return_type, expr->function.body->type)) {
        string ret = ast_typename(expr->type->function.return_type, false);
        string body = ast_typename(expr->function.body->type, false);
        ERR_DO(free(ret.data); free(body.data), expr->source_location,
          "Type \"%.*s\" of function body is not convertible to return type \"%.*s\".",
            (int) body.size, body.data, (int) ret.size, ret.data);
      }

      break;

    /// Typecheck declarations.
    case NODE_DECLARATION:
      /// If there is an initialiser, then its type must match the type of the variable.
      if (expr->declaration.init) {
        if (!typecheck_expression(ast, expr->declaration.init)) return false;
        if (!convertible(expr->type, expr->declaration.init->type))
          ERR_NOT_CONVERTIBLE(expr->type, expr->declaration.init->type);
      }
      break;

    /// If expression.
    case NODE_IF:
      if (!typecheck_expression(ast, expr->if_.condition)) return false;
      if (!typecheck_expression(ast, expr->if_.then)) return false;

      /// If the then and else branch of an if expression both exist and have
      /// the a common type, then the type of the if expression is that type.
      if (expr->if_.else_) {
        if (!typecheck_expression(ast, expr->if_.else_)) return false;
        Type *common = common_type(expr->if_.then->type, expr->if_.else_->type);
        if (common) expr->type = common;
        else expr->type = ast->t_void;
      }

      /// Otherwise, the type of the if expression is void.
      else { expr->type = ast->t_void; }
      break;

    /// A while expression has type void.
    case NODE_WHILE:
      if (!typecheck_expression(ast, expr->while_.condition)) return false;
      if (!typecheck_expression(ast, expr->while_.body)) return false;
      expr->type = ast->t_void;
      break;

    /// Typecheck all children and set the type of the block
    /// to the type of the last child. TODO: noreturn?
    case NODE_BLOCK: {
      VECTOR_FOREACH_PTR (Node *, node, expr->block.children)
        if (!typecheck_expression(ast, node))
          return false;
      expr->type = expr->block.children.size ? VECTOR_BACK(expr->block.children)->type : ast->t_void;
    } break;

    /// First, resolve the function. Then, typecheck all parameters
    /// and set the type to the return type of the callee.
    case NODE_CALL: {
      /// Resolve the function if applicable.
      Node *callee = expr->call.callee;
      if (!resolve_function(ast, callee)) return false;

      /// Typecheck the callee.
      if (!typecheck_expression(ast, callee)) return false;

      /// Callee must be a function or a function pointer.
      if (callee->type->kind == TYPE_FUNCTION) {
        /// Set the resolved function as the new callee.
        expr->call.callee = callee = callee->funcref->node;
        if (!typecheck_expression(ast, callee)) return false;
      } else {
        /// Implicitly load the function pointer.
        if (callee->type->kind == TYPE_POINTER && callee->type->pointer.to->kind == TYPE_FUNCTION) {
          expr->call.callee = callee = ast_make_unary(ast, expr->source_location, TK_AT, false, callee);
          callee->parent = expr;
          if (!typecheck_expression(ast, callee)) return false;
        } else {
          string name = ast_typename(callee->type, false);
          ERR_DO(free(name.data), expr->source_location, "Cannot call non-function type \"%.*s\".",
            (int) name.size, name.data);
        }
      }

      /// Typecheck all arguments.
      VECTOR_FOREACH_PTR (Node *, param, expr->call.arguments)
        if (!typecheck_expression(ast, param))
          return false;

      /// Make sure we have the right number of arguments.
      if (expr->call.arguments.size != callee->type->function.parameters.size)
        ERR(callee->source_location, "Expected %zu arguments, got %zu.",
            callee->type->function.parameters.size, expr->call.arguments.size);

      /// Make sure all arguments are convertible to the parameter types.
      VECTOR_FOREACH_INDEX(i, expr->call.arguments) {
        Parameter *param = &callee->type->function.parameters.data[i];
        Node *arg = expr->call.arguments.data[i];
        if (!convertible(param->type, arg->type)) ERR_NOT_CONVERTIBLE(param->type, arg->type);
      }

      /// Set the type of the call to the return type of the callee.
      expr->type = callee->type->function.return_type;
    } break;

    /// Make sure a cast is even possible.
    case NODE_CAST: TODO();

    /// Binary expression. This is a complicated one.
    case NODE_BINARY: {
      /// Get this out of the way early.
      Node *const lhs = expr->binary.lhs, *const rhs = expr->binary.rhs;
      if (!typecheck_expression(ast, lhs)) return false;
      if (!typecheck_expression(ast, rhs)) return false;

      /// Typecheck the operator.
      switch (expr->binary.op) {
        default: ICE("Invalid binary operator \"%s\".", token_type_to_string(expr->binary.op));

        /// The subscript operator is basically pointer arithmetic.
        case TK_LBRACK:
          /// We can only subscript pointers and arrays.
          if (!is_pointer(lhs->type) && !is_array(lhs->type)) {
            string name = ast_typename(lhs->type, false);
            ERR_DO(free(name.data), lhs->source_location,
              "Cannot subscript non-pointer, non-array type \"%.*s\".",
                (int) name.size, name.data);
          }

          /// The RHS has to be an integer.
          if (!is_integer(rhs->type)) {
            string name = ast_typename(rhs->type, false);
            ERR_DO(free(name.data), rhs->source_location,
              "Cannot subscript with non-integer type \"%.*s\".",
                (int) name.size, name.data);
          }

          /// The result of a subscript expression is a pointer to the
          /// start of the array, offset by the RHS.
          expr->type = ast_make_type_pointer(ast, lhs->source_location, lhs->type->array.of);
          break;

        /// All of these are basically the same when it comes to types.
        case TK_GT:
        case TK_LT:
        case TK_GE:
        case TK_LE:
        case TK_EQ:
        case TK_NE:
          if (!is_integer(lhs->type)) {
            string name = ast_typename(lhs->type, false);
            ERR_DO(free(name.data), lhs->source_location,
              "Cannot compare non-integer type \"%.*s\".",
                (int) name.size, name.data);
          }

          if (!is_integer(rhs->type)) {
            string name = ast_typename(rhs->type, false);
            ERR_DO(free(name.data), rhs->source_location,
              "Cannot compare non-integer type \"%.*s\".",
                (int) name.size, name.data);
          }

          /// TODO: Change this to bool if we ever add a bool type.
          expr->type = ast->t_integer;
          break;

        /// Since pointer arithmetic is handled by the subscript operator,
        /// type checking for these is basically all the same.
        /// TODO: Implicit conversions when we add the `byte` type.
        case TK_PLUS:
        case TK_MINUS:
        case TK_STAR:
        case TK_SLASH:
        case TK_PERCENT:
        case TK_SHL:
        case TK_SHR:
        case TK_AMPERSAND:
        case TK_PIPE:
        case TK_CARET:
          if (!is_integer(lhs->type)) {
            string name = ast_typename(lhs->type, false);
            ERR_DO(free(name.data), lhs->source_location,
              "Cannot perform arithmetic on non-integer type \"%.*s\".",
                (int) name.size, name.data);
          }

          if (!is_integer(rhs->type)) {
            string name = ast_typename(rhs->type, false);
            ERR_DO(free(name.data), rhs->source_location,
              "Cannot perform arithmetic on non-integer type \"%.*s\".",
                (int) name.size, name.data);
          }

          expr->type = lhs->type;
          break;

        /// This is the complicated one.
        case TK_COLON_EQ:
          /// Make sure the lhs is an lvalue.
          if (!is_lvalue(lhs)) {
            string name = ast_typename(lhs->type, false);
            ERR_DO(free(name.data), lhs->source_location,
              "Cannot assign to non-lvalue type \"%.*s\".",
                (int) name.size, name.data);
          }

          /// Make sure the rhs is convertible to the lhs.
          if (!convertible(lhs->type, rhs->type)) ERR_NOT_CONVERTIBLE(lhs->type, rhs->type);

          /// Set the type of the expression to the type of the lhs.
          expr->type = lhs->type;
          break;
      }
    } break;

    /// Here be dragons.
    case NODE_UNARY:
      if (!typecheck_expression(ast, expr->unary.value)) return false;
      switch (expr->unary.op) {
        default: ICE("Invalid unary operator \"%s\".", token_type_to_string(expr->unary.op));

        /// We can only deference pointers.
        case TK_AT:
          if (!is_pointer(expr->unary.value->type))
            ERR(expr->unary.value->source_location,
                "Argument of \"@\" must be a pointer.");

          /// The result type of a dereference is the pointee.
          expr->type = expr->unary.value->type->pointer.to;
          break;

        /// Address of lvalue.
        case TK_AMPERSAND:
          if (!is_lvalue(expr->unary.value))
            ERR(expr->unary.value->source_location,
                "Argument of \"&\" must be an lvalue.");

          expr->type = ast_make_type_pointer(ast, expr->source_location, expr->unary.value->type);
          break;

        /// One’s complement negation.
        case TK_TILDE:
          if (!is_integer(expr->unary.value->type))
            ERR(expr->unary.value->source_location,
                "Argument of \"~\" must be an integer.");

          expr->type = expr->unary.value->type;
          break;
      }
      break;

    /// Just set the type.
    case NODE_LITERAL:
      if (expr->literal.type == TK_NUMBER) expr->type = ast->t_integer;
      else TODO("Literal type \"%s\".", token_type_to_string(expr->literal.type));
      break;

    /// The type of a variable reference is the type of the variable.
    case NODE_VARIABLE_REFERENCE:
      if (!typecheck_expression(ast, expr->var->node)) return false;
      expr->type = expr->var->node->type;
      break;

    /// Resolve the function reference and typecheck the function.
    case NODE_FUNCTION_REFERENCE:
      /// TODO: Replace this w/ the resolved function node.
      if (!resolve_function(ast, expr)) return false;
      if (!typecheck_expression(ast, expr->funcref->node)) return false;
      expr->type = expr->funcref->node->type;
      break;
  }

  /// If this is a pointer type, make sure it doesn’t point to an incomplete type.
  Type *base = expr->type;
  while (base && is_pointer(base)) base = base->pointer.to;
  if (base && is_pointer(expr->type /** (!) **/) && ast_type_is_incomplete(base)) {
    string name = ast_typename(expr->type->pointer.to, false);
    ERR_DO(free(name.data), expr->source_location,
      "Cannot use pointer to incomplete type \"%.*s\".",
        (int) name.size, name.data);
  }

  /// Done.
  return true;
}
