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

#define DIAG(diag, loc, ...) issue_diagnostic(diag, (ast)->filename.data, as_span((ast)->source), (loc), __VA_ARGS__)

#define ERR(loc, ...)                                                                  \
  do {                                                                                        \
    issue_diagnostic(DIAG_ERR, (ast)->filename.data, as_span((ast)->source), (loc), __VA_ARGS__); \
    return false;                                                                             \
  } while (0)
#define SORRY(loc, ...) DIAG(DIAG_SORRY, loc, __VA_ARGS__)

#define ERR_DONT_RETURN(loc, ...) issue_diagnostic(DIAG_ERR, (ast)->filename.data, as_span((ast)->source), (loc), __VA_ARGS__)

#define ERR_NOT_CONVERTIBLE(where, to, from) ERR(where, "Type '%T' is not convertible to '%T'", from, to)

NODISCARD static bool types_equal(Type *a, Type *b);

/// Check if two canonical types are equal. You probably want to use
/// `convertible()`
/// \return Whether the types are equal.
NODISCARD static bool types_equal_canon(Type *a, Type *b) {
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
      // t_integer_literal is implicitly equal to t_integer
      if (a == t_integer_literal) return b == t_integer_literal || b == t_integer;
      if (b == t_integer_literal) return a == t_integer_literal || a == t_integer;
      return a == b;
    case TYPE_POINTER: return types_equal(a->pointer.to, b->pointer.to);
    case TYPE_ARRAY: return a->array.size == b->array.size && types_equal(a->array.of, b->array.of);
    case TYPE_FUNCTION: {
      if (a->function.parameters.size != b->function.parameters.size) return false;
      if (!types_equal(a->function.return_type, b->function.return_type)) return false;
      foreach_index(i, a->function.parameters)
        if (!types_equal(a->function.parameters.data[i].type, b->function.parameters.data[i].type))
          return false;
      return true;

    case TYPE_STRUCT:
      if (a->structure.alignment != b->structure.alignment) return false;
      if (a->structure.byte_size != b->structure.byte_size) return false;
      if (a->structure.members.size != b->structure.members.size) return false;
      foreach_index(i, a->structure.members) {
        Member a_member = a->structure.members.data[i];
        Member b_member = a->structure.members.data[i];
        if (a_member.byte_offset != b_member.byte_offset) return false;
        if (!types_equal(a_member.type, b_member.type)) return false;
      }
      return true;
    }
  }
}

/// Result type for the function below.
typedef struct IncompleteResult {
  bool incomplete;
  bool equal;
} IncompleteResult;

/// Compare two possibly incomplete types.
/// `a` and `b` must be the last alias of their corresponding types.
IncompleteResult compare_incomplete(Type *a, Type *b) {
  if (type_is_incomplete(a) && type_is_incomplete(a)) {
    /// Void is always equal to itself.
    if (type_is_void(a) && type_is_void(b)) return (IncompleteResult){.incomplete = true, .equal = true};

    /// If both are named and have the same name, then they’re equal.
    if (a->kind == TYPE_NAMED && b->kind == TYPE_NAMED && string_eq(a->named->name, b->named->name))
      return (IncompleteResult){.incomplete = true, .equal = true};

    /// Otherwise, they’re not equal.
    return (IncompleteResult){.incomplete = true, .equal = false};
  }

  /// If one is incomplete, the types are not equal.
  if (type_is_incomplete(a) || type_is_incomplete(b))
    return (IncompleteResult){.incomplete = true, .equal = false};

  /// Not incomplete.
  return (IncompleteResult){.incomplete = false, .equal = false};
}

/// Check if two types are equal. You probably want to use `convertible` instead.
NODISCARD static bool types_equal(Type *a, Type *b) {
  Type *ta = type_last_alias(a);
  Type *tb = type_last_alias(b);

  /// If both are incomplete, compare the names.
  IncompleteResult res = compare_incomplete(ta, tb);
  if (res.incomplete) return res.equal;

  /// Compare the types.
  return types_equal_canon(type_canonical(ta), type_canonical(tb));
}

/// Check if a canonical type is an integer type.
NODISCARD static bool is_integer_canon(Type *t) {
  return t == t_integer || t == t_integer_literal  || t == t_byte;
}

/// Check if a type is an integer type.
NODISCARD static bool is_integer(Type *type) {
  /// Currently, all primitive types are integers.
  return is_integer_canon(type_canonical(type));
}

/// Check how well from is convertible to to.
///
/// \param to_type The type to convert to.
/// \param from_type The type to convert from.
/// \return -1 if the types are not convertible to one another.
/// \return 0 if the types are equivalent.
/// \return 1 if the types are convertible, but implicit conversions are required.
NODISCARD static isz convertible_score(Type *to_type, Type *from_type) {
  /// Expand types.
  Type *to_alias = type_last_alias(to_type);
  Type *from_alias = type_last_alias(from_type);

  /// Any type is implicitly convertible to void.
  if (type_is_void(to_alias)) return 0;

  /// If either type is NULL for some reason, we give up.
  if (!to_alias || !from_alias) return -1;

  /// If both are incomplete, compare the names.
  IncompleteResult res = compare_incomplete(to_alias, from_alias);
  if (res.incomplete) return res.equal ? 0 : -1;

  /// If the types are the same, they are convertible.
  Type *to = type_canonical(to_alias);
  Type *from = type_canonical(from_alias);
  if (types_equal_canon(to, from)) return 0;

  /// A function type is implicitly convertible to its
  /// corresponding pointer type.
  if (to->kind == TYPE_POINTER && from->kind == TYPE_FUNCTION) {
    Type *base = type_canonical(to->pointer.to);
    if (!type_is_incomplete_canon(base) && types_equal_canon(base, from)) return 0;
    return -1;
  }
  if (from->kind == TYPE_POINTER && to->kind == TYPE_FUNCTION) {
    Type *base = type_canonical(from->pointer.to);
    if (!type_is_incomplete_canon(base) && types_equal_canon(base, to)) return 0;
    return -1;
  }

  /// Smaller integer types are implicitly convertible to larger
  /// integer types if the type being converted to is signed, or
  /// if the smaller type is unsigned.
  bool to_is_int = is_integer_canon(to);
  bool from_is_int = is_integer_canon(from);
  if (to_is_int && from_is_int) {
    if (
      to->primitive.size > from->primitive.size
      && (to->primitive.is_signed || !from->primitive.is_signed)
    ) return 1;
  }

  /// Integer literals are convertible to any integer type.
  if (from == t_integer_literal && to_is_int) return 1;

  /// Otherwise, the types are not convertible.
  return -1;
}

/// Check if from is convertible to to.
NODISCARD static bool convertible(Type *to_type, Type *from_type) {
  return convertible_score(to_type, from_type) != -1;
}

/// Get the common type of two types.
NODISCARD static Type *common_type(Type *a, Type *b) {
  Type *ta = type_canonical(a);
  Type *tb = type_canonical(b);
  if (types_equal(a, b)) return a;

  /// Some integer types are implicitly convertible to other integer types.
  /// See also `convertible_score`.
  if (is_integer(ta) && is_integer(tb)) {
    if (
        ta->primitive.size > tb->primitive.size
        && (ta->primitive.is_signed || !tb->primitive.is_signed)
    ) return ta;
    if (
        tb->primitive.size > ta->primitive.size
        && (tb->primitive.is_signed || !ta->primitive.is_signed)
    ) return tb;
  }

  /// No common type.
  return NULL;
}

/// Check if an expression is an lvalue.
NODISCARD static bool is_lvalue(Node *expr) {
  switch (expr->kind) {
  default: return false;

    // FIXME: Add `if`

    /// Declarations and variables are obviously lvalues.
    case NODE_DECLARATION:
    case NODE_VARIABLE_REFERENCE:
    case NODE_MEMBER_ACCESS:
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
  bool ambiguous = false;
  foreach (Candidate, sym, *overload_set) {
    if (sym->validity != candidate_valid) continue;

    /// If O(F) contains more than one element, then the program is
    /// ill-formed: F is ambiguous.
    if (valid_overload) {
      ERR_DONT_RETURN(funcref->source_location, "Use of overloaded function is ambiguous.");

      /// Print the valid overloads.
      ambiguous = true;
      break;
    }

    /// Otherwise, save this overload for later.
    valid_overload = sym->symbol;
  }

  /// If O(F) is empty, then the program is ill-formed: there is no
  /// matching overload for F.
  if (!valid_overload || ambiguous) {
    if (!ambiguous) ERR(funcref->source_location, "Unknown Symbol");

    /// Print parameter types if this is a call.
    if (funcref->parent->kind == NODE_CALL) {
      eprint("\n    %B38Where%m\n");
      size_t index = 1;
      foreach_ptr (Node*, arg, funcref->parent->call.arguments)
        eprint("    %Z = %T\n", index++, arg->type);
    }

    /// Print all overloads.
    size_t index = 1;
    if (!ambiguous) eprint("\n    %B38Overload Set%m\n");
    else eprint("\n    %B38Candidates%m\n");
    foreach (Candidate, c, *overload_set) {
      if (ambiguous && c->validity != candidate_valid) continue;
      u32 line;
      seek_location(as_span(ast->source), c->symbol->val.node->source_location, &line, NULL, NULL);
      eprint("    %B38(%Z) %32%S %31: %T %m(%S:%u)\n",
        index++, c->symbol->name, c->symbol->val.node->type, ast->filename, line);
    }

    /// If the call is ambiguous, then we’re done.
    if (ambiguous) return false;

    /// We might want to print dependent overload sets.
    Vector(Node*) dependent_functions = {0};
    Vector(span) dependent_function_names = {0};

    /// Explain why each one is invalid.
    eprint("\n    %B38Invalid Overloads%m\n");
    index = 1;
    foreach (Candidate, c, *overload_set) {
      eprint("    %B38(%Z) %m", index++);
      switch (c->validity) {
        default: ICE("Unknown overload invalidation reason: %d", c->validity);

        /// We only get here if there are *no* valid candidates.
        case candidate_valid: ICE("candidate_valid not allowed here");

        /// Candidates are only invalidated with this error if there
        /// is at least one candidate that is otherwise valid, which,
        /// as we’ve just established, is impossible.
        case invalid_too_many_conversions: ICE("too_many_conversions not allowed here");

        /// Not enough / too many parameters.
        case invalid_parameter_count:
          eprint("Candidate takes %Z parameters, but %Z were provided",
                 c->symbol->val.node->type->function.parameters.size,
                 funcref->parent->call.arguments.size);
          break;

        /// Argument type is not convertible to parameter type.
        case invalid_argument_type:
          eprint("Argument of type '%T' is not convertible to parameter type '%T'.",
                 funcref->parent->call.arguments.data[c->invalid_arg_index]->type,
                 c->symbol->val.node->type->function.parameters.data[c->invalid_arg_index].type);
          break;

        /// TODO: Print a better error depending on the parent expression.
        case invalid_expected_type_mismatch:
          eprint("Candidate type '%T' is not convertible to '%T'",
                 c->symbol->val.node->type,
                 funcref->parent->type);
          break;

        /// No matching overload for argument of function type. Only
        /// arguments can be set to this validity.
        case invalid_no_dependent_callee:
          ASSERT(dependent_funcref);
          ASSERT(dependent_overload_set);
          eprint("Candidate type '%T' is not convertible to parameter type '%T'",
                 c->symbol->val.node->type,
                 dependent_funcref->parent->call.arguments.data[c->invalid_arg_index]->type);
          break;

        /// No matching overload for callee. Only callees can be set to this
        /// validity.
        case invalid_no_dependent_arg: {
          Node * arg = funcref->parent->call.arguments.data[c->invalid_arg_index];
          Node * param = c->symbol->val.node->function.param_decls.data[c->invalid_arg_index];
          eprint("No overload of %32%S%m with type %T", arg->funcref.name, param->type);

          /// Mark that we need to print the overload set of this function too.
          span *ptr;
          vector_find_if(dependent_function_names, ptr, i, string_eq(dependent_function_names.data[i], arg->funcref.name));
          if (!ptr) {
            vector_push(dependent_functions, arg);
            vector_push(dependent_function_names, as_span(arg->funcref.name));
          }
        } break;
      }
      eprint("\n");
    }

    /// Print the overload sets of all dependent functions.
    if (dependent_functions.size) {
      eprint("\n    %B38Dependent Overload Sets%m\n");
      foreach_ptr (Node*, n, dependent_functions) {
        eprint("        %B38Overloads of %B32%S%B38%m\n", n->funcref.name);

        OverloadSet o = collect_overload_set(n);
        foreach (Candidate, c, o) {
          u32 line;
          seek_location(as_span(ast->source), c->symbol->val.node->source_location, &line, NULL, NULL);
          eprint("        %32%S %31: %T %m(%S:%u)\n",
            c->symbol->name, c->symbol->val.node->type, ast->filename, line);
        }
        vector_delete(o);
      }
    }

    return false;
  }

  /// Otherwise, resolve F to the last remaining element of O(F).
  funcref->funcref.resolved = valid_overload;
  funcref->type = funcref->funcref.resolved->val.node->type;
  return true;
}

/// Remove overloads except those with the least implicit conversions.
void reduce_overload_set(OverloadSet *overload_set) {
  if (overload_set->size) {
    /// Determine the candidate with the least number of implicit conversions.
    usz min_score = overload_set->data[0].score;
    foreach (Candidate, sym, *overload_set)
      if (sym->validity == candidate_valid && sym->score < min_score)
        min_score = sym->score;

    /// Remove all candidates with a more implicit conversions.
    foreach (Candidate, sym, *overload_set)
      if (sym->validity == candidate_valid && sym->score > min_score)
        sym->validity = invalid_too_many_conversions;
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
NODISCARD static bool resolve_function(AST *ast, Node *func) {
  /// 0. Skip anything that is not a function reference, or any function
  ///    references previously resolved.
  if (func->kind != NODE_FUNCTION_REFERENCE || func->funcref.resolved)
    return true;

  /// 1. Collect all functions with the same name as the function being
  ///    resolved into an *overload set* O. We cannot filter out any
  ///    functions just yet.
  OverloadSet overload_set = collect_overload_set(func), arg_overload_set = {0};

  // Extra validation step: ensure all functions within overload set
  // have matching return type.
  Type *return_type = NULL;
  foreach (Candidate, candidate, overload_set) {
    if (!return_type) {
      return_type = candidate->symbol->val.node->type->function.return_type;
      continue;
    }
    if (!types_equal(candidate->symbol->val.node->type->function.return_type, return_type))
      ERR(candidate->symbol->val.node->source_location,
          "Function in overload set has mismatched return type %T (expecting %T)",
          candidate->symbol->val.node->type->function.return_type, return_type);
  }

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
      if (sym->symbol->val.node->type->function.parameters.size != call->call.arguments.size)
        sym->validity = invalid_parameter_count;


    /// 2c. Let A_1, ... A_n be the arguments of the call expression.
    ///
    /// 2d. For candidate C in O, let P_1, ... P_n be the parameters of C.
    ///     For each argument A_i of the call, iff it is not an unresolved
    ///     function, check if it is convertible to P_i. Remove C from O if
    ///     it is not. Note down the number of A_i’s that required a (series
    ///     of) implicit conversions to their corresponding P_i’s.
    ///
    ///     Also collect unresolved function references.
    /// TODO: Could optimise this by merging with above loop.
    typedef struct { usz index; OverloadSet overloads; } unresolved_func;
    Vector(unresolved_func) unresolved_functions = {0};
    foreach (Candidate, candidate, overload_set) {
      if (candidate->validity != candidate_valid) continue;
      foreach_index (i, call->call.arguments) {
        /// Note down the number of function references.
        Node *arg = call->call.arguments.data[i];
        if (arg->kind == NODE_FUNCTION_REFERENCE && ! arg->funcref.resolved) {
          unresolved_func uf = {0};
          uf.index = i;
          vector_push(unresolved_functions, uf);
          continue;
        }

        /// Check if the argument is convertible to the parameter.
        Type *param_type = candidate->symbol->val.node->type->function.parameters.data[i].type;
        isz score = convertible_score(param_type, arg->type);
        if (score == -1) {
          candidate->validity = invalid_argument_type;
          candidate->invalid_arg_index = i;
          break;
        }

        /// If it is, check if a conversion was required.
        candidate->score += (usz) score;
      }
    }

    /// 2e. If there are unresolved function references.
    if (unresolved_functions.size) {
      /// 2eα. Collect their overload sets.
      foreach (unresolved_func, uf, unresolved_functions) {
        uf->overloads = collect_overload_set(call->call.arguments.data[uf->index]);

        /// Confidence check.
        if (!uf->overloads.size) {
          ERR_DONT_RETURN(call->call.arguments.data[uf->index]->source_location, "Unknown symbol");
          goto cleanup_arg_overloads;
        }

        /// 2eβ. Remove from O all candidates C that do no accept any overload
        ///      of this argument as a parameter.
        foreach (Candidate, candidate, overload_set) {
          if (candidate->validity != candidate_valid) continue;
          Type *param_type = candidate->symbol->val.node->type->function.parameters.data[uf->index].type;

          bool found = false;
          foreach (Candidate, arg_candidate, uf->overloads) {
            if (convertible_score(param_type, arg_candidate->symbol->val.node->type) == 0) {
              found = true;
              break;
            }
          }

          if (!found) {
            candidate->validity = invalid_no_dependent_arg;
            candidate->invalid_arg_index = uf->index;
          }
        }
      }

      /// 2eγ. Remove from O all functions except those with the least number of
      ///     implicit conversions as per step 2d.
      reduce_overload_set(&overload_set);

      /// 2eδ. Resolve the function being resolved.
      if (!resolve_overload(ast, &overload_set, func, NULL, NULL)) goto cleanup_arg_overloads;

      /// 2eε. For each argument, remove from its overload set all candidates
      /// that are not equivalent to the type of the corresponding parameter
      /// of the resolved function.
      foreach (unresolved_func, uf, unresolved_functions) {
        foreach (Candidate, candidate, uf->overloads) {
          if (candidate->validity != candidate_valid) continue;
          Type *param_type = func->type->function.parameters.data[uf->index].type;
          if (convertible_score(param_type, candidate->symbol->val.node->type) != 0) {
            candidate->validity = invalid_no_dependent_callee;
            candidate->invalid_arg_index = uf->index;
          }
        }

        /// 2eζ. Resolve the argument.
        if (!resolve_overload(ast, &uf->overloads, call->call.arguments.data[uf->index], &overload_set, func))
          goto cleanup_arg_overloads;
      }

      /// Success, yay!
      goto done;

      /// Cleanup so we don’t leak memory.
    cleanup_arg_overloads:
      foreach (unresolved_func, uf, unresolved_functions)
        vector_delete(uf->overloads);
      vector_delete(unresolved_functions);
      goto err;
    }

    /// 2f. Remove from O all functions except those with the least number of
    ///     implicit conversions as per step 2d.
    ///
    /// Note: If we get here, then unresolved_functions is empty, so no cleanup
    /// required.
    reduce_overload_set(&overload_set);
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
      foreach (Candidate, sym, overload_set)
        if (sym->validity == candidate_valid && convertible_score(decl_type, sym->symbol->val.node->type) != 0)
          sym->validity = invalid_expected_type_mismatch;
    } break;

    /// 3c. If the parent expression is an assignment expression, then
    case NODE_BINARY: {
      if (parent->binary.op != TK_COLON_EQ) break;

      /// ... if we are the LHS, then this is a type error, as we cannot assign to a
      /// function reference.
      if (func == parent->binary.lhs) {
        if (overload_set.size) ERR(func->source_location, "Cannot assign to function '%S'", func->funcref.name);
        else ERR(func->source_location, "Unknown symbol '%S'", func->funcref.name);
      }
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
      foreach (Candidate, sym, overload_set)
        if (sym->validity == candidate_valid && convertible_score(lvalue_type, sym->symbol->val.node->type) != 0)
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
          if (convertible_score(ast, return_type, sym->symbol->node->type) != 0)
            sym->validity = invalid_expected_type_mismatch;
        break;
      }

      ERR_DONT_RETURN(func->source_location,
        "Overloaded function %S is not convertible to %T\n",
          func->funcref.name return_type);
      goto err;
    } break;
    */

    /// 3e. If the parent expression is a cast expression, then, ...
    case NODE_CAST: {
      Type *cast_type = parent->type;

      /// ... if the result type of the cast is a function or function pointer type,
      /// remove from O all functions that are not equivalent to that type.
      if ((cast_type->kind == TYPE_POINTER && cast_type->pointer.to->kind == TYPE_FUNCTION) ||
          (cast_type->kind == TYPE_FUNCTION)) {
        foreach (Candidate, sym, overload_set)
          if (sym->validity == candidate_valid && convertible_score(cast_type, sym->symbol->val.node->type) != 0)
            sym->validity = invalid_expected_type_mismatch;
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

NODISCARD static bool typecheck_type(AST *ast, Type *t) {
  if (t->type_checked) return true;
  t->type_checked = true;
  switch (t->kind) {
  default: ICE("Invalid type kind of type %T", t);
  case TYPE_PRIMITIVE: return true;
  case TYPE_POINTER: return typecheck_type(ast, t->pointer.to);
  case TYPE_NAMED: {
    if (t->named->val.type)
      return typecheck_type(ast, t->named->val.type);
    return true;
  }
  case TYPE_FUNCTION:
    if (!typecheck_type(ast, t->function.return_type)) return false;
    foreach(Parameter, param, t->function.parameters) {
      if (!typecheck_type(ast, param->type)) return false;
      if (type_is_incomplete(param->type)) {
        ERR(param->source_location,
            "Function parameter must not be of incomplete type");
        return false;
      }
    }
    return true;
  case TYPE_ARRAY:
    if (!typecheck_type(ast, t->array.of)) return false;
    if (!t->array.size)
      ERR(t->source_location,
          "Cannot create array of zero size: %T", t);
    return true;
  case TYPE_STRUCT:
    foreach(Member, member, t->structure.members) {
      if (!typecheck_type(ast, member->type)) return false;
    }

    foreach(Member, member, t->structure.members) {
      size_t alignment = type_alignof(member->type);
      if (alignment > t->structure.alignment)
        t->structure.alignment = alignment;

      t->structure.byte_size += (alignment - (t->structure.byte_size % alignment)) % alignment;

      member->byte_offset = t->structure.byte_size;
      t->structure.byte_size += type_sizeof(member->type);
    }

    t->structure.byte_size += (t->structure.alignment - (t->structure.byte_size % t->structure.alignment)) % t->structure.alignment;

    return true;
  }
  UNREACHABLE();
}

NODISCARD bool typecheck_expression(AST *ast, Node *expr) {
  /// Don’t typecheck the same expression twice.
  if (expr->type_checked) return true;
  expr->type_checked = true;

  if (expr->type && !typecheck_type(ast, expr->type)) return false;

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
          Node *func = node->funcref.resolved->val.node;
          if (
            func &&
            func->source_location.start == node->source_location.start &&
            func->source_location.end == node->source_location.end
          ) { expr->root.children.data[i] = func; }
        }
      }

      /// If the last expression in the root is not of type integer,
      /// add a literal 0 so that `main()` returns 0.
      if (!expr->root.children.size || !convertible(t_integer, vector_back(expr->root.children)->type)) {
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
      if (!convertible(ret, body)) {
        loc l = {0};
        if (expr->function.body->kind == NODE_BLOCK)
          l = vector_back_or(expr->function.body->block.children, expr)->source_location;
        else l = expr->function.body->source_location;
        ERR(l,
            "Type '%T' of function body is not convertible to return type '%T'.",
            body, ret);
      }
    } break;

    /// Typecheck declarations.
    case NODE_DECLARATION: {
      /// If there is an initialiser, then its type must match the type of the variable.
      if (expr->declaration.init) {
        if (!typecheck_expression(ast, expr->declaration.init)) return false;
        // Type inference :^)
        if (!expr->type) {
          expr->type = expr->declaration.init->type;
          if (expr->type == t_integer_literal) expr->type = t_integer;
        } else if (!convertible(expr->type, expr->declaration.init->type))
          ERR_NOT_CONVERTIBLE(expr->declaration.init->source_location, expr->type, expr->declaration.init->type);
      } else if (!expr->type) ERR(expr->source_location, "Cannot infer type of declaration without initialiser");

      if (!typecheck_type(ast, expr->type)) return false;

      /// Strip arrays and recursive typedefs.
      Type *base_type = type_canonical(expr->type);
      Type *array = NULL;
      while (base_type) {
        if (base_type->kind == TYPE_NAMED) base_type = type_canonical(base_type->named->val.type);
        else if (base_type->kind == TYPE_ARRAY) {
          array = base_type;
          base_type = type_canonical(base_type->array.of);
          break;
        } else break;
      }

      /// Make sure this isn’t an array of incomplete type.
      if (type_is_incomplete(base_type)) {
        ERR(expr->source_location, "Cannot declare %s of incomplete type '%T'",
            array ? "array" : "variable", expr->type);
      }

      if (base_type->kind == TYPE_FUNCTION) {
        ERR(expr->source_location, "Cannot declare %s of function type '%T'",
            array ? "array" : "variable", expr->type);
      }
      } break;

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
        else expr->type = t_void;
      }

      /// Otherwise, the type of the if expression is void.
      else { expr->type = t_void; }
      break;

    /// A while expression has type void.
    case NODE_WHILE:
      if (!typecheck_expression(ast, expr->while_.condition)) return false;
      if (!typecheck_expression(ast, expr->while_.body)) return false;
      expr->type = t_void;
      break;

    /// Typecheck all children and set the type of the block
    /// to the type of the last child. TODO: noreturn?
    case NODE_BLOCK: {
      foreach_ptr (Node *, node, expr->block.children)
        if (!typecheck_expression(ast, node))
          return false;
      expr->type = expr->block.children.size ? vector_back(expr->block.children)->type : t_void;
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
          expr->call.callee = callee = callee->funcref.resolved->val.node;
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
        if (!convertible(param->type, arg->type)) ERR_NOT_CONVERTIBLE(arg->source_location, param->type, arg->type);
      }

      /// Set the type of the call to the return type of the callee.
      expr->type = callee->type->function.return_type;
    } break;

    /// Make sure a cast is even possible.
    case NODE_CAST: {
      Type *t_to = expr->type;
      // TO any incomplete type is DISALLOWED
      if (type_is_incomplete(t_to))
        ERR(t_to->source_location, "Can not cast to incomplete type %T", t_to);

      if (!typecheck_expression(ast, expr->cast.value))
        return false;

      Type *t_from = expr->cast.value->type;

      // FROM any type T TO type T is ALLOWED
      if (types_equal(t_to, t_from)) break;

      // FROM any incomplete type is DISALLOWED
      if (type_is_incomplete(t_from))
        ERR(expr->cast.value->source_location, "Can not cast from an incomplete type %T", t_from);

      // FROM any pointer type TO any pointer type is ALLOWED
      // TODO: Check base type size + alignment...
      if (type_is_pointer(t_from) && type_is_pointer(t_to)) break;
      // FROM any pointer type TO any integer type is ALLOWED
      if (type_is_pointer(t_from) && is_integer(t_to)) break;
      // FROM any integer type TO any integer type is ALLOWED
      if (is_integer(t_from) && is_integer(t_to)) break;

      // FROM any integer type TO any pointer type is currently DISALLOWED, but very well may change
      if (is_integer(t_from) && type_is_pointer(t_to))
        ERR(expr->cast.value->source_location,
            "Can not cast from an integer type %T to pointer type %T",
            t_from, t_to);

      // FROM any array type TO any array type is DISALLOWED
      if (type_is_array(t_from) && type_is_array(t_to)) {
        ERR(expr->cast.value->source_location,
            "Can not cast between arrays.");
      }

      ERR(expr->cast.value->source_location,
          "Casting from %T to %T is not supported by the typechecker\n"
          "  Open an issue with the current maintainers if you feel like this is not the proper behaviour.", t_from, t_to);
    }

    /// Binary expression. This is a complicated one.
    case NODE_BINARY: {
      /// Get this out of the way early.
      Node *const lhs = expr->binary.lhs;
      Node *const rhs = expr->binary.rhs;
      if (!typecheck_expression(ast, lhs)) return false;
      if (!typecheck_expression(ast, rhs)) return false;

      /// Typecheck the operator.
      switch (expr->binary.op) {
        default: ICE("Invalid binary operator '%s'.", token_type_to_string(expr->binary.op));

        /// The subscript operator is basically pointer arithmetic.
        case TK_LBRACK:
          /// We can only subscript pointers and arrays.
          if (!type_is_pointer(lhs->type) && !type_is_array(lhs->type))
            ERR(lhs->source_location,
              "Cannot subscript non-pointer, non-array type '%T'.",
                lhs->type);

          /// The RHS has to be some sort of integer.
          if (!is_integer(rhs->type))
            ERR(rhs->source_location,
              "Cannot subscript with non-integer type '%T'.",
                rhs->type);

          if (rhs->kind == NODE_LITERAL && rhs->literal.type == TK_NUMBER) {
            if (type_is_array(lhs->type) && lhs->type->array.size <= rhs->literal.integer)
              ERR(rhs->source_location,
                  "Subscript %U out of bounds for array %T", rhs->literal.integer, lhs->type);
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
          if (!is_integer(lhs->type))
            ERR(lhs->source_location,
              "Cannot compare non-integer type '%T'.",
                lhs->type);

          if (!is_integer(rhs->type))
            ERR(rhs->source_location,
              "Cannot compare non-integer type '%T'.",
                rhs->type);

          /// TODO: Change this to bool if we ever add a bool type.
          expr->type = t_integer;
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
          if (!is_integer(lhs->type))
            ERR(lhs->source_location,
                "Cannot perform arithmetic on non-integer type '%T'.",
                lhs->type);

          if (!is_integer(rhs->type))
            ERR(rhs->source_location,
                "Cannot perform arithmetic on non-integer type '%T'.",
                rhs->type);

          expr->type = lhs->type;
          break;

        /// This is the complicated one.
        case TK_COLON_EQ:
        case TK_COLON_COLON:
          /// Make sure the lhs is an lvalue.
          if (!is_lvalue(lhs))
            ERR(lhs->source_location,
                "Cannot assign to non-lvalue type '%T'.",
                lhs->type);

          /// Make sure the rhs is convertible to the lhs.
          if (!convertible(lhs->type, rhs->type))
            ERR_NOT_CONVERTIBLE(rhs->source_location, lhs->type, rhs->type);

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
        case TK_AT: {
          if (!type_is_pointer(expr->unary.value->type)) {
            ERR(expr->unary.value->source_location,
                "Argument of '@' must be a pointer.");
          }

          Type *pointee_type = type_canonical(expr->unary.value->type->pointer.to);
          if (!pointee_type) {
            ERR(expr->unary.value->source_location,
                "Cannot dereference incomplete pointer type %T",
                expr->unary.value->type->pointer.to);
          }

          /// The result type of a dereference is the pointee.
          expr->type = expr->unary.value->type->pointer.to;
          } break;

        /// Address of lvalue.
        case TK_AMPERSAND:
          if (!is_lvalue(expr->unary.value))
            ERR(expr->unary.value->source_location,
              "Argument of '&' must be an lvalue.");

          expr->type = ast_make_type_pointer(ast, expr->source_location, expr->unary.value->type);
          break;

        /// One’s complement negation.
        case TK_TILDE:
          if (!is_integer(expr->unary.value->type))
            ERR(expr->unary.value->source_location,
              "Argument of '~' must be an integer.");

          expr->type = expr->unary.value->type;
          break;
      }
      break;

    /// Just set the type.
    case NODE_LITERAL:
      if (expr->literal.type == TK_NUMBER) expr->type = t_integer_literal;
      else if (expr->literal.type == TK_STRING) {
        string s = ast->strings.data[expr->literal.string_index];
        expr->type = ast_make_type_array(ast, expr->source_location, t_byte, s.size + 1);
      }
      else TODO("Literal type '%s'.", token_type_to_string(expr->literal.type));
      break;

    /// The type of a variable reference is the type of the variable.
    case NODE_VARIABLE_REFERENCE:
      if (!typecheck_expression(ast, expr->var->val.node)) return false;
      expr->type = expr->var->val.node->type;
      break;

    /// The type of a structure declaration is the type of the struct.
    case NODE_STRUCTURE_DECLARATION:
      return typecheck_type(ast, expr->struct_decl->val.type);

    /// The type of a structure declaration is the type of the struct.
    case NODE_MEMBER_ACCESS: {
      // TODO: Auto dereference left hand side of pointers.
      // Ensure struct_ is of struct type.
      if (!typecheck_expression(ast, expr->member_access.struct_)) return false;
      Type *struct_type = type_canonical(expr->member_access.struct_->type);
      if (!struct_type || struct_type->kind != TYPE_STRUCT)
        ERR(expr->member_access.struct_->source_location,
            "Cannot access member of type %T", struct_type);

      Member *member;
      vector_find_if(struct_type->structure.members, member, i,
        string_eq(
          struct_type->structure.members.data[i].name,
          expr->member_access.ident
        )
      );
      if (!member)
        ERR(expr->source_location,
            "Cannot access member \"%S\" that does not exist in \"%S\", an instance of %T",
            expr->member_access.ident, expr->member_access.struct_->struct_decl->name, struct_type);

      expr->member_access.member = member;
      expr->type = member->type;

      return true;
    }

    /// Resolve the function reference and typecheck the function.
    case NODE_FUNCTION_REFERENCE:
      if (!resolve_function(ast, expr)) return false;
      if (!typecheck_expression(ast, expr->funcref.resolved->val.node)) return false;
      ast_replace_node(ast, expr, expr->funcref.resolved->val.node);
      break;
  }

  /// If this is a pointer type, make sure it doesn’t point to an incomplete type.
  Type *base = expr->type;
  while (base && type_is_pointer(base)) base = base->pointer.to;
  if (base && type_is_pointer(expr->type /** (!) **/) && type_is_incomplete(base))
    ERR(expr->source_location,
        "Cannot use pointer to incomplete type '%T'.",
        expr->type->pointer.to);

  /// Done.
  return true;
}
