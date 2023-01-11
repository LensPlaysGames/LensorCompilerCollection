#include <typechecker.h>

#include <ast.h>
#include <error.h>
#include <parser.h>
#include <utils.h>
#include <vector.h>

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

#define ERR_DONT_RETURN(loc, ...) issue_diagnostic(DIAG_ERR, (ast)->filename.data, as_span((ast)->source), (loc), __VA_ARGS__)

#define ERR_NOT_CONVERTIBLE(where, to, from) ERR(where, "Type '%T' is not convertible to '%T'", from, to)

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

/// An overload candidate.
typedef struct Candidate {
  Symbol *symbol;
  size_t score;

  /// Whether the overload is valid or why it is invalid.
  enum {
    candidate_valid,
    invalid_parameter_count,        /// Candidate has too many/few parameters.
    invalid_argument_type,          /// Argument type is not convertible to parameter type.
    invalid_too_many_conversions,   /// Candidate is valid but not ideal.
    invalid_expected_type_mismatch, /// Candidate is not equivalent to the expected type of the parent expression.
    invalid_no_dependent_callee,    /// Candidate is an argument of a call with no matching callee.
    invalid_no_dependent_arg,       /// No matching overload for argument of function type.
  } validity;

  /// Index of the incompatible argument.
  usz invalid_arg_index;
} Candidate;

/// A set of overload candidates.
typedef Vector(Candidate) OverloadSet;

/// Collect all possible overload candidates for a function reference.
static OverloadSet collect_overload_set(Node *func) {
  OverloadSet overload_set = {0};
  for (Scope *scope = func->funcref.scope; scope; scope = scope->parent) {
    foreach_ptr(Symbol*, sym, scope->symbols) {
      if (sym->kind != SYM_FUNCTION) {
        continue;
      }
      if (string_eq(sym->name, func->funcref.name)) {
        Candidate s;
        s.symbol = sym;
        s.score = 0;
        s.validity = candidate_valid;
        vector_push(overload_set, s);
      }
    }
  }
  return overload_set;
}

/// Actually resolve a function.
///
/// The overloads sets passed to this function must be minimal, i.e.
/// all overloads that are not viable must already be marked as such.
///
/// \param ast The AST of the program.
/// \param overload_set The overload set of the symbol being resolved.
/// \param funcref The symbol being resolved.
/// \param dependent_overload_set (optional) The overload set of the callee if this is a function argument.
/// \param dependent_funcref (optional) The callee if this is a function argument.
/// \return Whether overload resolution was successful.
NODISCARD static bool resolve_overload(
  AST *ast,
  OverloadSet *overload_set,
  Node *funcref,
  OverloadSet *dependent_overload_set,
  Node *dependent_funcref
) {
  /// Determine the overloads that are still valid.
  Symbol *valid_overload = NULL;
  foreach_if (Candidate, sym, *overload_set, sym->validity == candidate_valid) {
    /// If O(F) contains more than one element, then the program is
    /// ill-formed: F is ambiguous.
    if (valid_overload)
      ERR(funcref->source_location, "Use of overloaded function is ambiguous.");

    /// Otherwise, save this overload for later.
    valid_overload = sym->symbol;
  }


  /// If O(F) is empty, then the program is ill-formed: there is no
  /// matching overload for F.
  if (!valid_overload) ERR(funcref->source_location, "Could not resolve overloaded function.");

  /// Otherwise, resolve F to the last remaining element of O(F).
  funcref->funcref.resolved = valid_overload;
  funcref->type = funcref->funcref.resolved->node->type;
  return true;
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
NODISCARD static bool resolve_function(AST *ast, Node *func) {
  /// 0. Skip anything that is not a function reference, or any function
  ///    references previously resolved.
  if (func->kind != NODE_FUNCTION_REFERENCE || func->funcref.resolved)
    return true;

  /// 1. Collect all functions with the same name as the function being
  ///    resolved into an *overload set* O. We cannot filter out any
  ///    functions just yet.
  OverloadSet overload_set = collect_overload_set(func), arg_overload_set = {0};

  /// Whether there was an error.
  bool ok = true;

 step2:
  /// 2. If the parent expression is a call expression, and the function being
  ///    resolved is the callee of the call, then:
  if (func->parent->kind == NODE_CALL && func == func->parent->call.callee) {
    Node *call = func->parent;

    /// 2a. Typecheck all arguments of the call that are not unresolved
    ///     function references themselves. Note: This takes care of
    ///     resolving nested calls.
    foreach_ptr (Node*, arg, call->call.arguments)
      if (arg->kind != NODE_FUNCTION_REFERENCE)
        if (!typecheck_expression(ast, arg))
          goto err;

    /// 2b. Remove from O all functions that have a different number of
    ///     parameters than the call expression has arguments.
    foreach (Candidate, sym, overload_set)
      if (sym->symbol->node->type->function.parameters.size != call->call.arguments.size)
        sym->validity = invalid_parameter_count;

    /// 2c. Let A_1, ... A_n be the arguments of the call expression.
    ///
    /// 2d. For candidate C in O, let P_1, ... P_n be the parameters of C.
    ///     For each argument A_i of the call, iff it is not an unresolved
    ///     function, check if it is convertible to P_i. Remove C from O if
    ///     it is not. Note down the number of A_i’s that required a (series
    ///     of) implicit conversions to their corresponding P_i’s.
    ///
    /// TODO: Could optimise this by merging with above loop.
    foreach_if (Candidate, candidate, overload_set, candidate->validity == candidate_valid) {
      foreach_index (i, call->call.arguments) {
        /// Skip function references.
        Node *arg = call->call.arguments.data[i];
        if (arg->kind == NODE_FUNCTION_REFERENCE) continue;

        /// Check if the argument is convertible to the parameter.
        Type *param_type = candidate->symbol->node->type->function.parameters.data[i].type;
        if (!convertible(ast, param_type, arg->type)) {
          candidate->validity = invalid_argument_type;
          candidate->invalid_arg_index = i;
          break;
        }

        /// If it is, check if a conversion was required.
        if (!types_equal(ast, param_type, arg->type)) candidate->score++;
      }
    }

    /// 2e. If any of the A_i are unresolved functions, then each of those arguments:
    foreach_index (n, call->call.arguments) {
      Node *arg = call->call.arguments.data[n];
      if (arg->kind != NODE_FUNCTION_REFERENCE) continue;

      /// 2eα. Let F be that argument.
      ///
      /// 2eβ. Collect all functions with the same name as F into a set O(F).
      vector_delete(arg_overload_set);
      arg_overload_set = collect_overload_set(arg);

      /// 2eγ. Remove from O all functions whose parameter P_F that corresponds
      ///      to F is not equivalent to any of the functions in O(F), and from
      ///      O(F) all functions that are not equivalent to any of the P_F of
      ///      any of the functions in O and from O(F) all functions that are not
      ///      equivalent to any of the P_F of any of the functions in O.
      ///
      /// For simplicity’s sake, we shall henceforth refer to O as O(foo) and
      /// O(F) as O(bar). The function being resolved is `foo` and the argument
      /// being resolved (the unresolved function reference) is `bar`.
      ///
      /// In other words, given a call `foo(..., bar, ...)`, whose n-th argument
      /// `bar` is an overloaded function name, and overload sets O(foo) and O(bar):
      ///
      ///    - Remove from O(foo) any candidate C(foo) whose n-th parameter is
      ///      not equivalent to any of the candidates C(bar) in O(bar).
      ///
      ///    - Remove from O(bar) any candidate C(bar) for which there is *no*
      ///      candidate C(foo) in O(foo) whose n-th parameter is equivalent to
      ///      C(bar).
      ///
      /// Example:
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
      /// The first foo() is removed since there is no bar() that matches its
      /// first parameter. The second bar() is removed since there is no foo()
      /// that would accept it as its first parameter.
      ///
      /// First, remove from O(foo) any candidate C(foo) whose n-th parameter is
      /// not equivalent to any of the candidates C(bar) in O(bar).
      foreach_if (Candidate, overload, overload_set, overload->validity == candidate_valid) {
        bool valid = false;

        /// For each candidate for bar(), check if there is an overload of foo()
        /// that accepts bar() as an argument in the corresponding parameter slot.
        Type *param_type = overload->symbol->node->type->function.parameters.data[n].type;
        foreach_if (Candidate, arg_overload, arg_overload_set, arg_overload->validity == candidate_valid) {
          Type *arg_type = arg_overload->symbol->node->type;
          if (types_equal(ast, param_type, arg_type)) {
            valid = true;
            break;
          }
        }

        /// A bar() is invalid if there is no such overload of foo().
        if (!valid) {
          overload->validity = invalid_no_dependent_arg;
          overload->invalid_arg_index = n;
        }
      }

      /// Secondly, remove from O(bar) any candidate C(bar) for which
      /// there is *no* candidate C(foo) in O(foo) whose n-th parameter
      /// is equivalent to C(bar).
      foreach_if (Candidate, arg_overload, arg_overload_set, arg_overload->validity == candidate_valid) {
        bool valid = false;

        /// For each candidate of foo(), check if there is at least one overload of
        /// bar() that matches the type of the parameter that bar() corresponds to.
        foreach_if (Candidate, overload, overload_set, overload->validity == candidate_valid) {
          Type *param_type = overload->symbol->node->type->function.parameters.data[n].type;
          if (types_equal(ast, param_type, arg_overload->symbol->node->type)) {
            valid = true;
            break;
          }
        }

        /// A foo() is invalid if there is no bar() that matches the parameter.
        if (!valid) {
          arg_overload->validity = invalid_no_dependent_callee;
          arg_overload->invalid_arg_index = n;
        }
      }

      /// 2eδ. Resolve F.
      if (!resolve_overload(ast, &arg_overload_set, arg, &overload_set, func)) goto err;

      /// One would think that it would now make sense to remove from O(foo) all candidates
      /// C(foo) whose n-th parameter does not match the resolved overload C(bar), seeing as
      /// we have thus far kept all candidates C(foo) that match *at least one* candidate C(bar).
      ///
      /// However, this is predicated on the assumption that more than one candidate C(bar)
      /// could match the n-th parameter of a candidate C(foo). This is not the case, as
      /// there can be no two functions that are different but have the same signature and
      /// name (because overloading on the return type is not permitted). If there somehow
      /// are two functions whose signatures conflict in this way, then `resolve_overload`
      /// above (or perhaps even some earlier check, where we make sure that the overload
      /// set itself is well-formed) would have already reported that as an error, and we
      /// wouldn’t be here in the first place.
      ///
      /// In other words, any candidates C(foo) that are still valid are valid because they
      /// match ‘a’ candidate C(bar) with the correct signature. Since there can be only one
      /// such candidate (since no two different functions may have equivalent signatures),
      /// ‘a’ candidate C(bar) is actually ‘the’ (resolved) candidate C(bar).
    }

    /// 2f. Remove from O all functions except those with the least number of
    ///     implicit conversions as per step 2d.
    if (overload_set.size) {
      /// Determine the candidate with the least number of implicit conversions.
      usz min_score = overload_set.data[0].score;
      foreach_if (Candidate, sym, overload_set, sym->validity == candidate_valid)
        if (sym->score < min_score)
          min_score = sym->score;

      /// Remove all candidates with a more implicit conversions.
      foreach_if (Candidate, sym, overload_set, sym->validity == candidate_valid)
        if (sym->score > min_score)
          sym->validity = invalid_too_many_conversions;
    }
  }

  /// 3. Otherwise, depending on the type of the parent expression,
  else {
    Node *parent = func->parent;
    switch (parent->kind) {
    /// 3a. If the parent expression is a unary prefix expression with operator
    ///     address-of, then replace the parent expression with the unresolved
    ///     function and go to step 2/3 depending on the type of the new parent.
    case NODE_UNARY: {
      if (parent->unary.op == TK_AMPERSAND) {
        Node *grandparent = parent->parent;
        ast_replace_node(ast, parent, func);
        func->parent = grandparent;
        goto step2;
      }
    } break;

    /// 3b. If the parent expression is a declaration,
    case NODE_DECLARATION: {
      Type *decl_type = parent->type;
      /// ... and the lvalue is not of function pointer type, this is a type error.
      if (decl_type->kind != TYPE_POINTER || decl_type->pointer.to->kind != TYPE_FUNCTION) {
        ERR_DONT_RETURN(func->source_location,
          "Overloaded function %S is not convertible to %T\n",
            func->funcref.name, decl_type);
        goto err;
      }

      /// Otherwise, remove from O all functions that are not equivalent to the
      /// lvalue being assigned to.
      foreach_if (Candidate, sym, overload_set, sym->validity == candidate_valid)
        if (!types_equal(ast, decl_type, sym->symbol->node->type))
          sym->validity = invalid_expected_type_mismatch;
    } break;

    /// 3c. If the parent expression is an assignment expression, then
    case NODE_BINARY: {
      if (parent->binary.op != TK_COLON_EQ) break;

      /// ... if we are the LHS, and this is a type error, as we cannot assign to a
      /// function reference. The code that typechecks binary expressions will take
      /// care of reporting this error.
      if (func == parent->binary.lhs) return false;
      ASSERT(func == parent->binary.rhs);

      /// If the lvalue is not of function pointer type, this is a type error.
      Type *lvalue_type = parent->binary.lhs->type;
      if (lvalue_type->kind != TYPE_POINTER || lvalue_type->pointer.to->kind != TYPE_FUNCTION) {
        ERR_DONT_RETURN(func->source_location,
          "Overloaded function %S is not convertible to %T\n",
            func->funcref.name, lvalue_type);
        goto err;
      }

      /// Otherwise, remove from O all functions that are not equivalent to
      /// the lvalue being assigned to.
      foreach_if (Candidate, sym, overload_set, sym->validity == candidate_valid)
        if (!types_equal(ast, lvalue_type, sym->symbol->node->type))
          sym->validity = invalid_expected_type_mismatch;
    } break;

    /// TODO: Infer return value of block expressions.
    /// 3d. If the parent expression is a return expression, and the return type of the
    ///     function F containing that return expression is not of function pointer type,
    ///     this is a type error. Otherwise, remove from O all functions that are not
    ///     equivalent to the return type of F.
    /*
    // TODO: unimplemented
    case NODE_RETURN: {
      // FIXME: Should refer to return type of function that `parent` is within.
      Type *return_type = parent->type;
      if (return_type->kind == TYPE_POINTER && return_type->pointer.to->kind == TYPE_FUNCTION) {
        foreach_if (OverloadedFunctionSymbol, sym, overload_set, sym->validity == candidate_valid)
          if (!types_equal(ast, return_type, sym->symbol->node->type))
            sym->validity = invalid_expected_type_mismatch;
        break;
      }

      ERR_DONT_RETURN(func->source_location,
        "Overloaded function %S is not convertible to %T\n",
          func->funcref.name return_type);
      goto err;
    } break;
    */

    /// 3e. If the parent expression is a cast expression, then
    case NODE_CAST: {
      Type *cast_type = parent->type;

      /// 3eα. If the result type of the cast is a function or function pointer type,
      ///      then remove from O all functions that are not equivalent to that type.
      if ((cast_type->kind == TYPE_POINTER && cast_type->pointer.to->kind == TYPE_FUNCTION) ||
          (cast_type->kind == TYPE_FUNCTION)) {
        foreach_if (Candidate, sym, overload_set, sym->validity == candidate_valid)
          if (!types_equal(ast, cast_type, sym->symbol->node->type))
            sym->validity = invalid_expected_type_mismatch;
      }

      /// 3eβ. Otherwise, if the O contains more than one element, then this is a
      ///      compiler error: the cast is ambiguous; we can’t infer the type of the
      ///      function here if we’re not casting to a function or function pointer type.
      else if (overload_set.size > 1) {
        ERR_DONT_RETURN(func->source_location, "Cast of overloaded function is ambiguous.");
        goto err;
      }
    } break;

    /// 3f. Otherwise, do nothing.
    default: break;
    }
  }

  /// 4. Resolve the function reference.
  ok = resolve_overload(ast, &overload_set, func, NULL, NULL);

  /// Clean up the vectors.
 done:
  vector_delete(overload_set);
  vector_delete(arg_overload_set);

  /// Done.
  return ok;

  /// There was an error. Free everything and return false.
 err:
  ok = false;
  goto done;
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
    case NODE_FUNCTION: {
      if (!expr->function.body) break;
      if (!typecheck_expression(ast, expr->function.body)) return false;

      /// Make sure the return type of the body is convertible to that of the function.
      Type *ret = expr->type->function.return_type;
      Type *body = expr->function.body->type;
      if (!convertible(ast, ret, body))
        ERR(expr->source_location,
          "Type '%T' of function body is not convertible to return type '%T'.",
            ret, body);
    } break;

    /// Typecheck declarations.
    case NODE_DECLARATION:
      /// If there is an initialiser, then its type must match the type of the variable.
      if (expr->declaration.init) {
        if (!typecheck_expression(ast, expr->declaration.init)) return false;
        if (!convertible(ast, expr->type, expr->declaration.init->type))
          ERR_NOT_CONVERTIBLE(expr->declaration.init->source_location, expr->type, expr->declaration.init->type);
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
          ERR(expr->source_location, "Cannot call non-function type '%T'.", callee->type);
        }
      }

      /// Typecheck all arguments.
      foreach_ptr (Node *, param, expr->call.arguments)
        if (!typecheck_expression(ast, param))
          return false;

      /// Make sure we have the right number of arguments.
      if (expr->call.arguments.size != callee->type->function.parameters.size)
        ERR(callee->source_location, "Expected %Z arguments, got %Z.",
            callee->type->function.parameters.size, expr->call.arguments.size);

      /// Make sure all arguments are convertible to the parameter types.
      foreach_index(i, expr->call.arguments) {
        Parameter *param = &callee->type->function.parameters.data[i];
        Node *arg = expr->call.arguments.data[i];
        if (!convertible(ast, param->type, arg->type)) ERR_NOT_CONVERTIBLE(arg->source_location, param->type, arg->type);
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
        default: ICE("Invalid binary operator '%s'.", token_type_to_string(expr->binary.op));

        /// The subscript operator is basically pointer arithmetic.
        case TK_LBRACK:
          /// We can only subscript pointers and arrays.
          if (!is_pointer(lhs->type) && !is_array(lhs->type))
            ERR(lhs->source_location,
              "Cannot subscript non-pointer, non-array type '%T'.",
                lhs->type);

          /// The RHS has to be an integer.
          if (!is_integer(ast, rhs->type))
            ERR(rhs->source_location,
              "Cannot subscript with non-integer type '%T'.",
                rhs->type);

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
          if (!is_integer(ast, lhs->type))
            ERR(lhs->source_location,
              "Cannot compare non-integer type '%T'.",
                lhs->type);

          if (!is_integer(ast, rhs->type))
            ERR(rhs->source_location,
              "Cannot compare non-integer type '%T'.",
                rhs->type);

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
          if (!is_integer(ast, lhs->type))
            ERR(lhs->source_location,
              "Cannot perform arithmetic on non-integer type '%T'.",
                lhs->type);

          if (!is_integer(ast, rhs->type))
            ERR(rhs->source_location,
              "Cannot perform arithmetic on non-integer type '%T'.",
                rhs->type);

          expr->type = lhs->type;
          break;

        /// This is the complicated one.
        case TK_COLON_EQ:
          /// Make sure the lhs is an lvalue.
          if (!is_lvalue(lhs))
            ERR(lhs->source_location,
              "Cannot assign to non-lvalue type '%T'.",
                lhs->type);

          /// Make sure the rhs is convertible to the lhs.
          if (!convertible(ast, lhs->type, rhs->type)) ERR_NOT_CONVERTIBLE(rhs->source_location, lhs->type, rhs->type);

          /// Set the type of the expression to the type of the lhs.
          expr->type = lhs->type;
          break;
      }
    } break;

    /// Here be dragons.
    case NODE_UNARY:
      if (!typecheck_expression(ast, expr->unary.value)) return false;
      switch (expr->unary.op) {
        default: ICE("Invalid unary operator '%s'.", token_type_to_string(expr->unary.op));

        /// We can only deference pointers.
        case TK_AT:
          if (!is_pointer(expr->unary.value->type))
            ERR(expr->unary.value->source_location,
              "Argument of '@' must be a pointer.");

          /// The result type of a dereference is the pointee.
          expr->type = expr->unary.value->type->pointer.to;
          break;

        /// Address of lvalue.
        case TK_AMPERSAND:
          if (!is_lvalue(expr->unary.value))
            ERR(expr->unary.value->source_location,
              "Argument of '&' must be an lvalue.");

          expr->type = ast_make_type_pointer(ast, expr->source_location, expr->unary.value->type);
          break;

        /// One’s complement negation.
        case TK_TILDE:
          if (!is_integer(ast, expr->unary.value->type))
            ERR(expr->unary.value->source_location,
              "Argument of '~' must be an integer.");

          expr->type = expr->unary.value->type;
          break;
      }
      break;

    /// Just set the type.
    case NODE_LITERAL:
      if (expr->literal.type == TK_NUMBER) expr->type = ast->t_integer_literal;
      else TODO("Literal type '%s'.", token_type_to_string(expr->literal.type));
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
  if (base && is_pointer(expr->type /** (!) **/) && ast_type_is_incomplete(base))
    ERR(expr->source_location,
      "Cannot use pointer to incomplete type '%T'.",
        expr->type->pointer.to);

  /// Done.
  return true;
}
