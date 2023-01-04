#include "ast.h"
#include "utils.h"
#include "vector.h"
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

NODISCARD static bool types_equal(AST *ast, Type *a, Type *b);

/// Check if two types are equal. You probably want to use
/// `convertible()` or `equivalent()` instead.
///
/// \param a A type that is not NULL and not of kind NAMED.
/// \param b A type that is not NULL and not of kind NAMED.
/// \return Whether the types are equal.
NODISCARD static bool types_equal_impl(AST *ast, Type *a, Type *b) {
  ASSERT(a && b);
  ASSERT(a->kind != TYPE_NAMED);
  ASSERT(b->kind != TYPE_NAMED);

  /// If the type kinds are not the same, the the types are obviously not equal.
  if (a->kind != b->kind) return false;

  /// Compare the types.
  switch (a->kind) {
    default: ICE("Invalid type kind %d", a->kind);
    case TYPE_NAMED: UNREACHABLE();
    case TYPE_PRIMITIVE:
      if (a == ast->t_integer_literal) return b == ast->t_integer_literal || b == ast->t_integer;
      if (b == ast->t_integer_literal) return a == ast->t_integer_literal || a == ast->t_integer;
      return a->primitive.id == b->primitive.id;
    case TYPE_POINTER: return types_equal(ast, a->pointer.to, b->pointer.to);
    case TYPE_ARRAY: return a->array.size == b->array.size && types_equal(ast, a->array.of, b->array.of);
    case TYPE_FUNCTION: {
      if (a->function.parameters.size != b->function.parameters.size) return false;
      if (!types_equal(ast, a->function.return_type, b->function.return_type)) return false;
      foreach_index(i, a->function.parameters)
        if (!types_equal(ast, a->function.parameters.data[i].type, b->function.parameters.data[i].type))
          return false;
      return true;
    }
  }
}

/// Check if two types are equal. You probably want to use `convertible` instead.
NODISCARD static bool types_equal(AST *ast, Type *a, Type *b) {
  Typeinfo ta = ast_typeinfo(ast, a);
  Typeinfo tb = ast_typeinfo(ast, b);

  /// If both are incomplete, compare the names.
  if (ta.is_incomplete && tb.is_incomplete) {
    ASSERT(ta.last_alias && tb.last_alias);
    return string_eq(ta.last_alias->named->name, tb.last_alias->named->name);
  }

  /// If one is incomplete, the types are not equal.
  if (ta.is_incomplete || tb.is_incomplete) return false;

  /// Compare the types.
  return types_equal_impl(ast, ta.type, tb.type);
}

/// Check if a type is an integer type.
NODISCARD static bool is_integer_impl(AST *ast, Typeinfo t) {
  /// Currently, all primitive types are integers.
  return t.type == ast->t_integer || t.type == ast->t_integer_literal  || t.type == ast->t_byte;
}

/// Check if a type is an integer type.
NODISCARD static bool is_integer(AST *ast, Type *type) {
  /// Currently, all primitive types are integers.
  Typeinfo t = ast_typeinfo(ast, type);
  return is_integer_impl(ast, t);
}

/// Check if from is convertible to to.
NODISCARD static bool convertible(AST *ast, Type * to_type, Type * from_type) {
  /// Expand types.
  Typeinfo to = ast_typeinfo(ast, to_type);
  Typeinfo from = ast_typeinfo(ast, from_type);

  /// Any type is implicitly convertible to void.
  if (to.is_void) return true;

  /// If the types are both incomplete, compare their names.
  if (to.is_incomplete && from.is_incomplete) {
    ASSERT(to.last_alias && from.last_alias);
    return string_eq(to.last_alias->named->name, from.last_alias->named->name);
  }

  /// If either type is incomplete, they are not convertible.
  if (to.is_incomplete || from.is_incomplete) return false;

  /// If the types are the same, they are convertible.
  if (types_equal(ast, to.type, from.type)) return true;

  /// A function type is implicitly convertible to its
  /// corresponding pointer type.
  if (to.type->kind == TYPE_POINTER && from.type->kind == TYPE_FUNCTION) {
    Typeinfo base = ast_typeinfo(ast, to.type->pointer.to);
    return !base.is_incomplete && types_equal_impl(ast, base.type, from.type);
  }

  /// Smaller integer types are implicitly convertible to larger
  /// integer types if the type being converted to is signed, or
  /// if the smaller type is unsigned.
  bool to_is_int = is_integer_impl(ast, to);
  bool from_is_int = is_integer_impl(ast, from);
  if (to_is_int && from_is_int) {
    if (
      to.type->primitive.size > from.type->primitive.size
      && (to.type->primitive.is_signed
         || !from.type->primitive.is_signed)
    ) return true;
  }

  /// Integer literals are convertible to any integer type.
  if (from.type == ast->t_integer_literal && to_is_int) return true;

  /// Otherwise, the types are not convertible.
  return false;
}

/// Get the common type of two types.
NODISCARD static Type *common_type(AST *ast, Type *a, Type *b) {
  /// TODO: integer stuff.
  if (types_equal(ast, a, b)) return a;
  return NULL;
}

/// Check if a type is a pointer type.
NODISCARD static bool is_pointer(Type *type) { return type->kind == TYPE_POINTER; }

/// Check if a type is an array type.
NODISCARD static bool is_array(Type *type) { return type->kind == TYPE_ARRAY; }

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
///
/// Terminology:
///
///   - A (formal) parameter is a parameter (type) of a function type or signature.
///
///   - An (actual) argument is a subexpression of a function call that is not
///     the callee.
///
///   - Two types, A and B, are *equivalent* iff
///       - 1. A and B are the same type, or
///       - 2. one is a function type and the other its corresponding function
///            pointer type, or
///       - 3. one is a named type whose underlying type is equivalent to the
///            other.
///
///   - A type A is *convertible* to a type B if there is a series of implicit
///     conversions that transforms A to B or if A and B are equivalent.
///
///   - An argument A is convertible/equivalent to a parameter P iff the type
///     of A is convertible/equivalent to the type of P.
///
/// To resolve an unresolved function reference, execute the following steps in
/// order. The unresolved function reference in question is hereinafter referred
/// to as ‘the function being resolved’.
///
/// 1. Collect all functions with the same name as the function being
///    resolved into an *overload set* O. We cannot filter out any
///    functions just yet.
///
/// 2. If the parent expression is a call expression, and the function being
///    resolved is the callee of the call, then:
///
///    2a  Typecheck all arguments of the call that are not unresolved
///        function references themselves. Note: This takes care of
///        resolving nested calls.
///
///    2b. Remove from O all functions that have a different number of
///        parameters than the call expression has arguments.
///
///    2c. Let A_1, ... A_n be the arguments of the call expression.
///
///    2d. For candidate C in O, let P_1, ... P_n be the parameters of C.
///        For each argument A_i of the call, iff it is not an unresolved
///        function, check if it is convertible to P_i. Remove C from O if
///        it is not. Note down the number of A_i’s that required a (series
///        of) implicit conversions to their corresponding P_i’s.
///
///    2e. If any of the A_i are unresolved functions, then each of those arguments:
///
///        2eα. Let F be that argument.
///
///        2eβ. Collect all functions with the same name as F into a set O(F).
///
///        2eγ. Remove from O all functions whose parameter P_F that corresponds
///             to F does not not match any of the functions in O(F), and from
///             O(F) all functions whose signature does not match any of the P_F
///             of any of the functions in O.
///
///        2eδ. If O(F) is empty, then this is a compiler error: there is no
///             matching overload for F.
///
///        2eε. If O(F) contains more than one element, then this is a compiler
///             error: F is ambiguous.
///
///        2eζ. Otherwise, resolve F to the last remaining element of O(F).
///
///    2f. Remove from O all functions except those with the least number of
///        implicit conversions as per step 2d.
///
/// 3. Otherwise, depending on the type of the parent expression,
///
///    3a. If the parent expression is a unary prefix expression with operator
///        address-of, then replace the parent expression with the unresolved
///        function and go to step 2/3 depending on the type of the new parent.
///
///    3b. If the parent expression is an assignment expression *or declaration*,
///        and the lvalue is not of function or function pointer type, this is a
///        type error. Otherwise, remove from O all functions that are not equivalent
///        to the lvalue being assigned to.
///
///    3c. If the parent expression is a return expression, and the return type of the
///        function F containing that return expression is not of function pointer type,
///        this is a type error. Otherwise, remove from O all functions that are not
///        equivalent to the return type of F.
///
///    3d. If the parent expression is a cast expression, then
///
///        3dα. If the result type of the cast is a function or function pointer type,
///             then remove from O all functions that are not equivalent to that type.
///
///        3dβ. Otherwise, if the O contains more than one element, then this is a
///             compiler error: the cast is ambiguous; we can’t infer the type of the
///             function here if we’re not casting to a function or function pointer type.
///
///    3e. Otherwise, do nothing and move on to step 4.
///
/// 4. If O is empty, then this is a compiler error: there is no matching
///    overload for the function being resolved.
///
/// 5. If O contains more than one element, then this is a compiler error:
///    the function being resolved is ambiguous.
///
/// 6. Otherwise, resolve the function reference to the last remaining element of O.
typedef struct OverloadedFunctionSymbol {
  Symbol *symbol;
  size_t score;
} OverloadedFunctionSymbol;

typedef Vector(OverloadedFunctionSymbol) OverloadedFunctionSymbols;

static OverloadedFunctionSymbols collect_overload_set(Node *func) {
  OverloadedFunctionSymbols overload_set = {0};
  for (Scope *scope = func->funcref.scope; scope; scope = scope->parent) {
    foreach_ptr(Symbol*, sym, scope->symbols) {
      if (sym->kind != SYM_FUNCTION) {
        continue;
      }
      if (string_eq(sym->name, func->funcref.name)) {
        OverloadedFunctionSymbol s;
        s.symbol = sym;
        s.score = 0;
        vector_push(overload_set, s);
      }
    }
  }
  return overload_set;
}

void print_overload_set(OverloadedFunctionSymbols syms) {
  printf("[ ");
  foreach(OverloadedFunctionSymbol, sym, syms) {
    string type = ast_typename(sym->symbol->node->type, false);
    printf("%.*s", (int)type.size, type.data);
    printf(", ");
  }
  printf(" ]\n");
}

NODISCARD static bool resolve_function(AST *ast, Node *func) {
  // Skip anything that is not a function reference, or any function
  // references previously resolved.
  if (func->kind != NODE_FUNCTION_REFERENCE || func->funcref.resolved)
    return true;

  /// 1. Collect all functions with the same name as the function being
  ///    resolved into an *overload set* O. We cannot filter out any
  ///    functions just yet.
  ///
  OverloadedFunctionSymbols overload_set = collect_overload_set(func);

  Vector(OverloadedFunctionSymbol) to_remove = {0};
# define do_removal() do {                                  \
      foreach(OverloadedFunctionSymbol, sym, to_remove) {   \
        vector_remove_element_unordered(overload_set, *sym); \
      }                                                     \
      vector_clear(to_remove);                              \
    } while (0)

 step2:
  vector_clear(to_remove);

  /// 2. If the parent expression is a call expression, and the function being
  ///    resolved is the callee of the call, then:
  ///
  if (func->parent->kind == NODE_CALL && func == func->parent->call.callee) {
    Node *call = func->parent;
    /// 2a  Typecheck all arguments of the call that are not unresolved
    ///     function references themselves. Note: This takes care of
    ///     resolving nested calls.
    ///
    foreach_ptr(Node*, arg, call->call.arguments) {
      if (arg->kind != NODE_FUNCTION_REFERENCE)
        typecheck_expression(ast, arg);
    }

    /// 2b. Remove from O all functions that have a different number of
    ///     parameters than the call expression has arguments.
    ///
    foreach(OverloadedFunctionSymbol, sym, overload_set) {
      if (sym->symbol->node->type->function.parameters.size != call->call.arguments.size) {
        vector_push(to_remove, *sym);
      }
    }
    do_removal();

    /// 2c. Let A_1, ... A_n be the arguments of the call expression.
    ///
    /// 2d. For candidate C in O, let P_1, ... P_n be the parameters of C.
    ///     For each argument A_i of the call, iff it is not an unresolved
    ///     function, check if it is convertible to P_i. Remove C from O if
    ///     it is not. Note down the number of A_i’s that required a (series
    ///     of) implicit conversions to their corresponding P_i’s.
    ///
    // TODO: Could optimise this by merging with above loop?
    foreach(OverloadedFunctionSymbol, candidate, overload_set) {
      foreach_index(i, call->call.arguments) {
        Node *arg = call->call.arguments.data[i];
        if (arg->kind == NODE_FUNCTION_REFERENCE) continue;
        if (!convertible(ast, candidate->symbol->node->type->function.parameters.data[i].type, arg->type)) {
          vector_push(to_remove, *candidate);
          break;
        }
        ++candidate->score;
      }
    }
    do_removal();

    /// 2e. If any of the A_i are unresolved functions, then each of those arguments:
    ///
    foreach_index(i, call->call.arguments) {
      Node *arg = call->call.arguments.data[i];
      if (arg->kind != NODE_FUNCTION_REFERENCE) continue;
      ///  2eα. Let F be that argument.
      ///
      ///  2eβ. Collect all functions with the same name as F into a set O(F).
      ///
      OverloadedFunctionSymbols arg_overload_set = collect_overload_set(arg);
      ///  2eγ. Remove from O all functions whose parameter P_F that corresponds
      ///       to F does not not match any of the functions in O(F), and from
      ///       O(F) all functions whose signature does not match any of the P_F
      ///       of any of the functions in O.
      ///
      /// foo : integer(func : integer(ptr : @integer))
      /// foo : integer(func : integer())
      /// foo(bar)
      /// bar : integer(n : integer)
      /// bar : integer()
      ///
      /// foo_1   bar_1
      ///      \ /
      ///       x
      ///      / \
      /// foo_2   bar_2
      ///
      foreach(OverloadedFunctionSymbol, overload, overload_set) {
        bool valid = false;
        foreach(OverloadedFunctionSymbol, arg_overload, arg_overload_set) {
          if (types_equal(ast, overload->symbol->node->type, arg_overload->symbol->node->type)) {
            valid = true;
            break;
          }
        }
        if (!valid) vector_push(to_remove, *overload);
      }
      do_removal();

      /// ... and from O(F) all functions whose signature does not
      /// match any of the P_F of any of the functions in O.

      foreach(OverloadedFunctionSymbol, arg_overload, arg_overload_set) {
        bool valid = false;
        foreach(OverloadedFunctionSymbol, overload, overload_set) {
          Type *param_type = overload->symbol->node->type->function.parameters.data[i].type;
          if (types_equal(ast, param_type, arg_overload->symbol->node->type)) {
            valid = true;
            break;
          }
        }
        if (!valid) vector_push(to_remove, *arg_overload);
      }
      do_removal();

      ///  2eδ. If O(F) is empty, then this is a compiler error: there is no
      ///       matching overload for F.
      ///
      if (arg_overload_set.size == 0)
        ERR(arg->source_location, "Could not resolve overloaded function.");

      ///  2eε. If O(F) contains more than one element, then this is a compiler
      ///       error: F is ambiguous.
      ///
      if (arg_overload_set.size != 1)
        ERR(arg->source_location, "Use of overloaded function is ambiguous.");

      ///  2eζ. Otherwise, resolve F to the last remaining element of O(F).
      ///
      arg->funcref.resolved = arg_overload_set.data[0].symbol;
    }

    /// 2f. Remove from O all functions except those with the least number of
    ///     implicit conversions as per step 2d.
    ///
    if (overload_set.size) {
      // gather minimum score
      size_t score = overload_set.data[0].score;
      foreach(OverloadedFunctionSymbol, sym, overload_set) {
        if (sym->score < score) {
          score = sym->score;
        }
      }

      // remove all higher than
      foreach(OverloadedFunctionSymbol, sym, overload_set) {
        if (sym->score > score) {
          vector_push(to_remove, *sym);
        }
      }
      do_removal();
    }
  } else {
    /// 3. Otherwise, depending on the type of the parent expression,
    ///
    Node *parent = func->parent;
    switch (parent->kind) {
    ///  3a. If the parent expression is a unary prefix expression with operator
    ///      address-of, then replace the parent expression with the unresolved
    ///      function and go to step 2/3 depending on the type of the new parent.
    ///
    case NODE_UNARY: {
      if (parent->unary.op == TK_AMPERSAND) {
        Node *grandparent = parent->parent;
        ast_replace_node(ast, parent, func);
        func->parent = grandparent;
        goto step2;
      }
    } break;
    ///    3b. If the parent expression is an assignment expression *or declaration*,
    ///        and the lvalue is not of function or function pointer type, this is a
    ///        type error. Otherwise, remove from O all functions that are not equivalent
    ///        to the lvalue being assigned to.
    ///
    case NODE_DECLARATION: {
      Type *decl_type = parent->type;
      if (decl_type->kind == TYPE_POINTER && decl_type->pointer.to->kind == TYPE_FUNCTION) {
        foreach(OverloadedFunctionSymbol, sym, overload_set) {
          if (!types_equal(ast, decl_type, sym->symbol->node->type)) {
            vector_push(to_remove, *sym);
          }
        }
        do_removal();
        break;
      }
      string decl_typename = ast_typename(decl_type, false);
      ERR_DO(free(decl_typename.data), func->source_location,
             "Overloaded function %.*s is not convertible to %.*s\n",
             (int) func->funcref.name.size, func->funcref.name.data,
             (int) decl_typename.size,      decl_typename.data);
    } break;
    case NODE_BINARY: {
      if (parent->binary.op != TK_COLON_EQ) break;
      if (func == parent->binary.lhs) return false;
      if (func == parent->binary.rhs) {
        Type *lvalue_type = parent->binary.lhs->type;
        if (lvalue_type->kind == TYPE_POINTER && lvalue_type->pointer.to->kind == TYPE_FUNCTION) {
          foreach(OverloadedFunctionSymbol, sym, overload_set) {
            if (!types_equal(ast, lvalue_type, sym->symbol->node->type)) {
              vector_push(to_remove, *sym);
            }
          }
          do_removal();
          break;
        }
        string lvalue_typename = ast_typename(lvalue_type, false);
        ERR_DO(free(lvalue_typename.data), func->source_location,
               "Overloaded function %.*s is not convertible to %.*s\n",
               (int) func->funcref.name.size, func->funcref.name.data,
               (int) lvalue_typename.size,      lvalue_typename.data);
      }
    } break;
    ///    3c. If the parent expression is a return expression, and the return type of the
    ///        function F containing that return expression is not of function pointer type,
    ///        this is a type error. Otherwise, remove from O all functions that are not
    ///        equivalent to the return type of F.
    ///
    /*
    // TODO: unimplemented
    case NODE_RETURN: {
      // FIXME: Should refer to return type of function that `parent` is within.
      Type *return_type = parent->type;
      if (return_type->kind == TYPE_POINTER && return_type->pointer.to->kind == TYPE_FUNCTION) {
        foreach(OverloadedFunctionSymbol, sym, overload_set) {
          if (!types_equal(ast, return_type, sym->symbol->node->type)) {
            vector_push(to_remove, *sym);
          }
        }
        foreach(OverloadedFunctionSymbol, sym, to_remove) {
          vector_remove_element_unordered(overload_set, *sym);
        }
        break;
      }
      string return_typename = ast_typename(return_type, false);
      ERR_DO(free(return_typename.data), func->source_location,
             "Overloaded function %.*s is not convertible to %.*s\n",
             (int) func->funcref.name.size, func->funcref.name.data,
             (int) return_typename.size,      return_typename.data);
    } break;
    */
    ///    3d. If the parent expression is a cast expression, then
    ///

    case NODE_CAST: {
      Type *cast_type = parent->type;
      if ((cast_type->kind == TYPE_POINTER && cast_type->pointer.to->kind == TYPE_FUNCTION)
          || (cast_type->kind == TYPE_FUNCTION)
          ) {
        ///  3dα. If the result type of the cast is a function or function pointer type,
        ///       then remove from O all functions that are not equivalent to that type.
        ///
        foreach(OverloadedFunctionSymbol, sym, overload_set) {
          if (!types_equal(ast, cast_type, sym->symbol->node->type)) {
            vector_push(to_remove, *sym);
          }
        }
        do_removal();
      } else {
        ///  3dβ. Otherwise, if the O contains more than one element, then this is a
        ///       compiler error: the cast is ambiguous; we can’t infer the type of the
        ///       function here if we’re not casting to a function or function pointer type.
        ///
        if (overload_set.size > 1)
          ERR(func->source_location, "Cast of overloaded function is ambiguous.");
      }
      } break;
    ///    3e. Otherwise, do nothing and move on to step 4.
    ///
    default: break;
    }
  }

  print_overload_set(overload_set);

  /// 4. If O is empty, then this is a compiler error: there is no matching
  ///    overload for the function being resolved.
  ///
  if (overload_set.size == 0)
    ERR(func->source_location, "Could not resolve overloaded function.");

  /// 5. If O contains more than one element, then that is an error:
  ///    the function being resolved is ambiguous.
  ///
  if (overload_set.size != 1)
    ERR(func->source_location, "Use of overloaded function is ambiguous (size == %zu).", overload_set.size);

  /// 6. Otherwise, resolve the function reference to the last remaining element of O.
  ///
  func->funcref.resolved = overload_set.data[0].symbol;
  // TODO: Is this correct?
  func->type = func->funcref.resolved->type;
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
      foreach_ptr (Node *, node, expr->root.children)
        if (!typecheck_expression(ast, node))
          return false;

      /// Replace function references in the root with the function nodes
      /// iff the source location of the function is the same as that of
      /// the function reference.
      ///
      /// This is so that if someone, for whatever reason, puts the name
      /// of the function as an expression in the root, it will just be
      /// removed rather than replaced with the function.
      foreach_index(i, expr->root.children) {
        Node *node = expr->root.children.data[i];
        if (node->kind == NODE_FUNCTION_REFERENCE) {
          Node *func = node->funcref.resolved->node;
          if (
            func &&
            func->source_location.start == node->source_location.start &&
            func->source_location.end == node->source_location.end
          ) { expr->root.children.data[i] = func; }
        }
      }

      /// If the last expression in the root is not of type integer,
      /// add a literal 0 so that `main()` returns 0.
      if (!expr->root.children.size || !convertible(ast, ast->t_integer, vector_back(expr->root.children)->type)) {
        Node *lit = ast_make_integer_literal(ast, (loc){0}, 0);
        vector_push(expr->root.children, lit);
        lit->parent = expr;
        ASSERT(typecheck_expression(ast, lit));
      }

      break;

    /// Typecheck the function body if there is one.
    case NODE_FUNCTION:
      if (!expr->function.body) break;
      if (!typecheck_expression(ast, expr->function.body)) return false;

      /// Make sure the return type of the body is convertible to that of the function.
      if (!convertible(ast, expr->type->function.return_type, expr->function.body->type)) {
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
        if (!convertible(ast, expr->type, expr->declaration.init->type))
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
        Type *common = common_type(ast, expr->if_.then->type, expr->if_.else_->type);
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
      foreach_ptr (Node *, node, expr->block.children)
        if (!typecheck_expression(ast, node))
          return false;
      expr->type = expr->block.children.size ? vector_back(expr->block.children)->type : ast->t_void;
    } break;

    /// First, resolve the function. Then, typecheck all parameters
    /// and set the type to the return type of the callee.
    case NODE_CALL: {
      Node *callee = expr->call.callee;

      /// Resolve the function if applicable.

      if (!resolve_function(ast, callee)) return false;

      /// Typecheck the callee.
      if (!typecheck_expression(ast, callee)) return false;

      /// Callee must be a function or a function pointer.
      if (callee->type->kind == TYPE_FUNCTION) {
        /// Set the resolved function as the new callee.
        if (callee->kind != NODE_FUNCTION) {
          expr->call.callee = callee = callee->funcref.resolved->node;
          if (!typecheck_expression(ast, callee)) return false;
        }
      } else {
        /// Implicitly load the function pointer.
        if (callee->type->kind == TYPE_POINTER && callee->type->pointer.to->kind == TYPE_FUNCTION) {
          expr->call.callee = callee = ast_make_unary(ast, expr->source_location, TK_AT, false, callee);
          callee->parent = expr;
          if (!typecheck_expression(ast, callee)) return false;
        } else {
          ast_print(stdout, ast);
          string name = ast_typename(callee->type, false);
          ERR_DO(free(name.data), expr->source_location, "Cannot call non-function type \"%.*s\".",
            (int) name.size, name.data);
        }
      }

      /// Typecheck all arguments.
      foreach_ptr (Node *, param, expr->call.arguments)
        if (!typecheck_expression(ast, param))
          return false;

      /// Make sure we have the right number of arguments.
      if (expr->call.arguments.size != callee->type->function.parameters.size)
        ERR(callee->source_location, "Expected %zu arguments, got %zu.",
            callee->type->function.parameters.size, expr->call.arguments.size);

      /// Make sure all arguments are convertible to the parameter types.
      foreach_index(i, expr->call.arguments) {
        Parameter *param = &callee->type->function.parameters.data[i];
        Node *arg = expr->call.arguments.data[i];
        if (!convertible(ast, param->type, arg->type)) ERR_NOT_CONVERTIBLE(param->type, arg->type);
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
          if (!is_integer(ast, rhs->type)) {
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
          if (!is_integer(ast, lhs->type)) {
            string name = ast_typename(lhs->type, false);
            ERR_DO(free(name.data), lhs->source_location,
              "Cannot compare non-integer type \"%.*s\".",
                (int) name.size, name.data);
          }

          if (!is_integer(ast, rhs->type)) {
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
          if (!is_integer(ast, lhs->type)) {
            string name = ast_typename(lhs->type, false);
            ERR_DO(free(name.data), lhs->source_location,
              "Cannot perform arithmetic on non-integer type \"%.*s\".",
                (int) name.size, name.data);
          }

          if (!is_integer(ast, rhs->type)) {
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
          if (!convertible(ast, lhs->type, rhs->type)) ERR_NOT_CONVERTIBLE(lhs->type, rhs->type);

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
          if (!is_integer(ast, expr->unary.value->type))
            ERR(expr->unary.value->source_location,
                "Argument of \"~\" must be an integer.");

          expr->type = expr->unary.value->type;
          break;
      }
      break;

    /// Just set the type.
    case NODE_LITERAL:
      if (expr->literal.type == TK_NUMBER) expr->type = ast->t_integer_literal;
      else TODO("Literal type \"%s\".", token_type_to_string(expr->literal.type));
      break;

    /// The type of a variable reference is the type of the variable.
    case NODE_VARIABLE_REFERENCE:
      if (!typecheck_expression(ast, expr->var->node)) return false;
      expr->type = expr->var->node->type;
      break;

    /// Resolve the function reference and typecheck the function.
    case NODE_FUNCTION_REFERENCE:
      if (!resolve_function(ast, expr)) return false;
      if (!typecheck_expression(ast, expr->funcref.resolved->node)) return false;
      ast_replace_node(ast, expr, expr->funcref.resolved->node);
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
