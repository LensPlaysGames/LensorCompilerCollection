#include <codegen/opt/opt-internal.h>

typedef Vector(IRBlock *) BlockVector;

/// ===========================================================================
///  Helpers
/// ===========================================================================
#define ctzll __builtin_ctzll

#define IR_REDUCE_BINARY(op)                                                           \
  IRInstruction *lhs = ir_lhs(i);                                                      \
  IRInstruction *rhs = ir_rhs(i);                                                      \
  IRType lhs_kind = ir_kind(lhs);                                                      \
  IRType rhs_kind = ir_kind(rhs);                                                      \
  if (lhs_kind == IR_IMMEDIATE && rhs_kind == IR_IMMEDIATE) {                          \
    ir_replace(i, ir_create_immediate(ctx, ir_typeof(i), ir_imm(lhs) op ir_imm(rhs))); \
    changed = true;                                                                    \
  }

static bool power_of_two(u64 value) {
  return value > 0 && (value & (value - 1)) == 0;
}

static bool has_side_effects(IRInstruction *i) {
  STATIC_ASSERT(IR_COUNT == 40, "Handle all instructions");
  switch (ir_kind(i)) {
    /// These do NOT have side effects.
    case IR_IMMEDIATE:
    case IR_LOAD:
    case IR_PARAMETER:
    case IR_NOT:
    case IR_STATIC_REF:
    case IR_FUNC_REF:
    case IR_LIT_INTEGER:
    case IR_LIT_STRING:
    case IR_ALLOCA:
    case IR_ZERO_EXTEND:
    case IR_SIGN_EXTEND:
    case IR_TRUNCATE:
    case IR_BITCAST:
    case IR_POISON:
      ALL_BINARY_INSTRUCTION_CASES()
      return false;

    case IR_CALL: {
      return !ir_call_is_direct(i) ||
             ir_call_tail(i) ||
             !ir_attribute(ir_callee(i).func, FUNC_ATTR_PURE);
    }

    default:
      return true;
  }
}

/// Check if this instruction may clobber memory.
static bool clobbers_memory(IRInstruction *inst){
  STATIC_ASSERT(IR_COUNT == 40, "Handle all instructions");
  switch (ir_kind(inst)) {
    case IR_COUNT: UNREACHABLE();

    case IR_CALL:
    case IR_INTRINSIC:
    case IR_STORE:
      return true;

    default: return false;
  }
}

/// Truncate a value.
static bool perform_truncation(u64 *out_value, u64 value, usz dest_size) {
  switch (dest_size) {
    case 1: *out_value = (u8) value; return true;
    case 2: *out_value = (u16) value; return true;
    case 4: *out_value = (u32) value; return true;
    case 8: *out_value = (u64) value; return true;
    default: return false;
  }
}

/// Sign-extend a value.
static bool perform_sign_extension(u64 *out_value, u64 value, usz dest_size, usz src_size) {
  /// Sign-extend from source size to destination size.
  switch (src_size) {
    case 1: value = (u64) (i64) (i8) value; break;
    case 2: value = (u64) (i64) (i16) value; break;
    case 4: value = (u64) (i64) (i32) value; break;
    case 8: value = (u64) (i64) (i64) value; break;
    default: return false;
  }

  /// Truncate to destination size.
  return perform_truncation(out_value, value, dest_size);
}

/// ===========================================================================
///  Instruction combination
/// ===========================================================================
/// Everything that merges instructions or performs strength reduction,
/// folding, etc. etc. goes here. If you’re unsure where to put something,
/// put it here.
///
/// Note: Take care to remove uses etc. *before* overwriting the `imm` field
/// as it is in a union together with whatever it is whose uses you want to
/// remove.
static bool opt_instcombine(CodegenContext *ctx, IRFunction *f) {
  bool changed = false;

  /// Div instructions to be replaced by shifts.
  Vector(struct shift { IRInstruction *div; IRInstruction *shift_amount; }) shift_divs = {0};

  FOREACH_BLOCK (b, f) {
    FOREACH_INSTRUCTION (i, b) {
      switch (ir_kind(i)) {
        default: break;
        case IR_ADD: {
          IR_REDUCE_BINARY(+)
          else {
            // Adding zero to something == no-op
            /// TODO: Canonicalisation.
            if (lhs_kind == IR_IMMEDIATE && ir_imm(lhs) == 0) {
              ir_replace_uses(i, rhs);
              changed = true;
            } else if (rhs_kind == IR_IMMEDIATE && ir_imm(rhs) == 0) {
              ir_replace_uses(i, lhs);
              changed = true;
            }
          }
        } break;

        case IR_SUB: {
          IR_REDUCE_BINARY(-)
          else {
            // Subtracting zero from something == no-op
            if (rhs_kind == IR_IMMEDIATE && ir_imm(rhs) == 0) {
              ir_replace_uses(i, lhs);
              changed = true;
            }
          }
        } break;

        case IR_MUL: {
          IR_REDUCE_BINARY(*)
          else {
            /// TODO: Canonicalisation.
            // Multiplying by zero == zero
            if ((lhs_kind == IR_IMMEDIATE && ir_imm(lhs) == 0)) {
              ir_replace_uses(i, lhs);
              changed = true;
            }
            // Multiplying by zero == zero
            else if ((rhs_kind == IR_IMMEDIATE && ir_imm(rhs) == 0)) {
              ir_replace_uses(i, rhs);
              changed = true;
            }
            // Multiplying 1 * rhs == rhs
            else if (lhs_kind == IR_IMMEDIATE && ir_imm(lhs) == 1) {
              ir_replace_uses(i, rhs);
              changed = true;
            }
            // Multiplying lhs * 1 == lhs
            else if (rhs_kind == IR_IMMEDIATE && ir_imm(rhs) == 1) {
              ir_replace_uses(i, lhs);
              changed = true;
            }
          }
        } break;

        case IR_DIV: {
          IR_REDUCE_BINARY(/)
          else {
            if (rhs_kind == IR_IMMEDIATE) {
              usz imm = ir_imm(rhs);
              /// Division by 1 does nothing.
              if (imm == 1) {
                ir_replace_uses(i, lhs);
                changed = true;
              }

              /// Replace division by a power of two with a shift.
              else if (power_of_two(imm)) {
                vector_push(shift_divs, (struct shift){
                  i,
                  ir_create_immediate(ctx, ir_typeof(i), (u64) ctzll(imm)),
                });
                changed = true;
              }
            }
          }
        } break;

        case IR_MOD: {
          IR_REDUCE_BINARY(%)
        } break;

        case IR_SHL: {
          IR_REDUCE_BINARY(<<)
        } break;

        case IR_SHR: {
          IR_REDUCE_BINARY(>>)
        } break;

        case IR_SAR: {
          IRInstruction *lhs = ir_lhs(i);
          IRInstruction *rhs = ir_rhs(i);
          IRType lhs_kind = ir_kind(lhs);
          IRType rhs_kind = ir_kind(rhs);
          if (lhs_kind == IR_IMMEDIATE && rhs_kind == IR_IMMEDIATE) {
            ir_replace(i, ir_create_immediate(ctx, ir_typeof(i), (u64) ((i64) ir_imm(lhs) >> (i64) ir_imm(rhs))));
            changed = true;
          }
        } break;

        case IR_AND: {
          IR_REDUCE_BINARY(&)
        } break;

        case IR_OR: {
          IR_REDUCE_BINARY(|)
        } break;

        case IR_NOT: {
          IRInstruction *op = ir_operand(i);
          if (ir_kind(op) == IR_IMMEDIATE) {
            ir_replace(i, ir_create_immediate(ctx, ir_typeof(i), ~ir_imm(op)));
            changed = true;
          }
        } break;

        case IR_LT: {
          IR_REDUCE_BINARY(<);
        } break;
        case IR_LE: {
          IR_REDUCE_BINARY(<=);
        } break;
        case IR_GT: {
          IR_REDUCE_BINARY(>);
        } break;
        case IR_GE: {
          IR_REDUCE_BINARY(>=);
        } break;
        case IR_NE: {
          IR_REDUCE_BINARY(!=)
          else if (lhs == rhs) {
            ir_replace(i, ir_create_immediate(ctx, ir_typeof(i), 0));
            changed = true;
          }
        } break;

        case IR_EQ: {
          IR_REDUCE_BINARY(==)
          else if (lhs == rhs) {
            ir_replace(i, ir_create_immediate(ctx, ir_typeof(i), 1));
            changed = true;
          }
        } break;

        case IR_ZERO_EXTEND: {
          IRInstruction *op = ir_operand(i);
          if (ir_kind(op) == IR_IMMEDIATE) {
            ir_replace(i, ir_create_immediate(ctx, ir_typeof(i), ir_imm(op)));
            changed = true;
          }
        } break;

        case IR_SIGN_EXTEND: {
          IRInstruction *op = ir_operand(i);
          if (ir_kind(op) == IR_IMMEDIATE) {
            u64 value = 0;
            if (
              perform_sign_extension(
                &value,
                ir_imm(op),
                type_sizeof(ir_typeof(i)),
                type_sizeof(ir_typeof(op))
              )
            ) {
              ir_replace(i, ir_create_immediate(ctx, ir_typeof(i), value));
              changed = true;
            }
          }
        } break;

        case IR_TRUNCATE: {
          IRInstruction *op = ir_operand(i);
          if (ir_kind(op) == IR_IMMEDIATE) {
            u64 value = 0;
            if (
              perform_truncation(
                &value,
                ir_imm(op),
                type_sizeof(ir_typeof(i))
              )
            ) {
              ir_replace(i, ir_create_immediate(ctx, ir_typeof(i), value));
              changed = true;
            }
          }
        }  break;

        /// Simplify conditional branches with constant conditions.
        case IR_BRANCH_CONDITIONAL: {
          IRInstruction *cond = ir_cond(i);
          if (ir_kind(cond) == IR_IMMEDIATE) {
            ir_replace(i, ir_create_br(ctx, ir_imm(cond) ? ir_then(i) : ir_else(i)));
            changed = true;
          }
        } break;

        /// Simplify PHIs that contain only a single argument.
        case IR_PHI: {
          if (ir_phi_args_count(i) > 1) break;
          ir_replace_uses(i, ir_phi_arg(i, 0)->value);
          changed = true;
        } break;

        /// Simplify indirect calls to direct calls.
        case IR_CALL: {
          if (ir_call_is_direct(i)) break;
          IRInstruction *callee = ir_callee(i).inst;
          switch (ir_kind(callee)) {
            default: break;

            case IR_FUNC_REF:
              ir_callee(i, ir_val(ir_func_ref_func(callee)), true);
              changed = true;
              break;

            case IR_BITCAST: {
              IRInstruction *op = ir_operand(callee);
              if (ir_kind(op) == IR_FUNC_REF) {
                ir_callee(i, ir_val(ir_func_ref_func(op)), true);
                changed = true;
              }
            } break;
          }
        } break;

        /// Collapse pointer copies.
        case IR_COPY: {
          /// FIXME: Enabling this optimisation breaks a bunch of stuff. Presumably,
          /// this is due to the clobbering problem that we have in ISel atm.
          /*if (type_is_pointer(ir_typeof(i)) && type_is_pointer(ir_typeof(i->operand))) {
            ir_replace(i, ir_operand(i));
            changed = true;
          }*/
        } break;
      }
    }
  }

  /// Perform replacements that require insertions.
  foreach (s, shift_divs) {
    ir_insert_before(s->div, s->shift_amount);
    ir_replace(s->div, ir_create_sar(ctx, ir_lhs(s->div), s->shift_amount));
  }

  return changed;
}

/// ===========================================================================
///  DCE
/// ===========================================================================
static bool opt_dce(IRFunction *f) {
  bool changed = false;
  IRInstructionVector to_remove = {0};
  FOREACH_BLOCK (b, f) {
    FOREACH_INSTRUCTION (i, b) {
      if (ir_use_count(i) == 0 && !has_side_effects(i)) {
        vector_push(to_remove, i);
        changed = true;
      }
    }
  }

  /// Remove all instructions.
  foreach_val (i, to_remove) ir_remove(i);
  vector_delete(to_remove);
  return changed;
}

/// ===========================================================================
///  TCE
/// ===========================================================================
typedef struct {
  IRInstruction *call;
  Vector(IRInstruction *) phis;
} tail_call_info;

/// See opt_tail_call_elim() for more info.
static bool tail_call_possible_iter(tail_call_info *tc, IRBlock *b) {
  /// Start at the call if this is the block containing the call,
  /// or at the first instruction of the block otherwise.
  IRInstruction **it = b == ir_parent(tc->call) ? ir_it(tc->call) + 1 : ir_begin(b);
  IRInstruction **end = ir_end(b);
  for (; it < end; it++) {
    IRInstruction *i = *it;
    IRType kind = ir_kind(i);
    switch (kind) {
      /// If this is a phi node, then the call or a previous phi
      /// must be an argument of the phi.
      case IR_PHI: {
        for (usz j = 0; j < ir_phi_args_count(i); j++) {
          const IRPhiArgument *arg = ir_phi_arg(i, j);
          if (arg->value == tc->call) { goto phi; }
          foreach_val (a, tc->phis) {
            if (a == arg->value) { goto phi; }
          }
        }
        return false;

      phi:
        vector_push(tc->phis, i);
        continue;
      }

      /// If we encounter a return instruction, then a tail call
      /// is only possible if the return value is the call, or
      /// any of the PHIs.
      case IR_RETURN: {
        IRInstruction *retval = ir_operand(i);
        foreach_val (a, tc->phis)
          if (a == retval)
            return true;
        return retval == tc->call;
      }

      /// If this is a branch, follow the branches.
      case IR_BRANCH : return tail_call_possible_iter(tc, ir_dest(i));
      case IR_BRANCH_CONDITIONAL:
        return tail_call_possible_iter(tc, ir_then(i)) &&
               tail_call_possible_iter(tc, ir_else(i));

      /// Any other instruction means that the call is not the last
      /// relevant instruction before a return.
      default: return false;
    }
  }

  return false;
}

static bool tail_call_possible(IRInstruction *i) {
  tail_call_info tc_info = {0};
  tc_info.call = i;
  bool possible = tail_call_possible_iter(&tc_info, ir_parent(i));
  vector_delete(tc_info.phis);
  return possible;
}

bool opt_try_convert_to_tail_call(IRInstruction *i) {
  /// An instruction is a tail call iff there are no other instruction
  /// between it and the next return instruction other than branches
  /// and phis.
  if (tail_call_possible(i)) {
    /// The actual tail call optimisation takes place in the code generator.
    ir_call_tail(i, true);
    ir_make_unreachable(ir_parent(i));
    return true;
  }

  return false;
}

static bool opt_tail_call_elim(IRFunction *f) {
  bool changed = false;
  FOREACH_BLOCK (b, f) {
    FOREACH_INSTRUCTION (i, b) {
      if (ir_kind(i) != IR_CALL) { continue; }

      /// We can’t have more than two tail calls in a single block.
      if (opt_try_convert_to_tail_call(i)) goto next_block;
    }
  next_block:;
  }
  return changed;
}

/// ===========================================================================
///  Mem2Reg
/// ===========================================================================
static bool opt_mem2reg(IRFunction *f) {
  bool changed = false;

  /// A stack variable.
  typedef struct {
    IRInstruction *alloca;
    IRInstruction *store;
    Vector(IRInstruction *) loads;
    bool unoptimisable;
  } stack_var;
  Vector(stack_var) vars = {0};

  /// Collect all stack variables that are stored into once, and
  /// whose address is never taken.
  FOREACH_BLOCK (b, f) {
    FOREACH_INSTRUCTION (i, b) {
      switch (ir_kind(i)) {
        default: break;

        /// New variable.
        case IR_ALLOCA: {
          stack_var v = {0};
          v.alloca = i;
          vector_push(vars, v);
        } break;

        /// Record the first store into a variable.
        case IR_STORE: {
          foreach (a, vars) {
            if (!a->unoptimisable && a->alloca == ir_store_addr(i)) {
              /// If there are multiple stores, mark the variable as unoptimisable.
              if (a->store) a->unoptimisable = true;
              else a->store = i;
              break;
            }
          }
        } break;

        /// Record all loads; also check for loads before the first store.
        case IR_LOAD: {
          foreach (a, vars) {
            if (!a->unoptimisable && a->alloca ==ir_operand(i)) {
              /// Load before store.
              if (!a->store) {
                a->unoptimisable = true;
                CodegenContext *ctx = ir_context(f);
                issue_diagnostic(
                  DIAG_WARN,
                  ctx->ast->filename.data,
                  as_span(ctx->ast->source),
                  ir_location(f), /// FIXME: Should be location of the load.
                  "Load of uninitialised variable in function %S",
                  ir_name(f)
                );
              } else {
                vector_push(a->loads, i);
              }
              break;
            }
          }
        } break;
      }
    }
  }

  /// Optimise all optimisable variables.
  foreach (a, vars) {
    /// If the variable is unoptimisable, do nothing.
    ///
    /// Since we don’t have `addressof` instructions or anything like
    /// that, check if the address is taken anywhere by checking if
    /// there are any uses of the alloca excepting the store and loads.
    if (a->unoptimisable || !a->store || ir_use_count(a->alloca) != a->loads.size + 1) {
      vector_delete(a->loads);
      continue;
    }

    /// If we get here, we can yeet the variable.
    changed = true;

    /// Replace all loads with the stored value.
    foreach_val (i, a->loads) ir_replace(i, ir_store_value(a->store));
    vector_delete(a->loads);

    /// Remove the store.
    ir_remove(a->store);

    /// Remove the alloca.
    ir_remove(a->alloca);
  }

  vector_delete(vars);
  return changed;
}

/// ===========================================================================
///  Analyse functions.
/// ===========================================================================
/// This function returns whether the pure attribute of the function has changed,
/// *not* whether it’s pure or not.
bool opt_check_pure(IRFunction *f) {
  /// Iterate over all instructions and check if they have nonlocal side effects.
  FOREACH_INSTRUCTION_IN_FUNCTION (i, b, f) {
    if (!has_side_effects(i)) continue;
    if (ir_is_branch(i)) continue;

    /// Even if an instruction generally has side effects, the function
    /// may still be pure, e.g. if the instruction is a call to a pure
    /// function or a store to a local variable.
    switch (ir_kind(i)) {
      case IR_STORE:
        if (ir_kind(ir_store_addr(i)) == IR_ALLOCA) continue;
        break;
      case IR_CALL:
        if (ir_call_is_direct(i) && ir_attribute(ir_callee(i).func, FUNC_ATTR_PURE)) continue;
        break;
      default: break;
    }

    /// Function is not pure.
    if (!ir_attribute(f, FUNC_ATTR_PURE)) return false;
    ir_attribute(f, FUNC_ATTR_PURE, false);
    return true;
  }

  /// Function is pure.
  if (ir_attribute(f, FUNC_ATTR_PURE)) return false;
  ir_attribute(f, FUNC_ATTR_PURE, true);
  return true;
}

/// Check if a function is a leaf function. This function returns whether
/// the leaf attribute of the function has changed, *not* whether it’s a
/// leaf or not.
bool opt_check_leaf(IRFunction *f) {
  /// A leaf function may not contain any calls except for recursive tail calls
  /// or tail calls to other leaf functions.
  FOREACH_INSTRUCTION_IN_FUNCTION (i, b, f) {
    if (ir_kind(i) != IR_CALL) continue;
    if (ir_call_is_direct(i) && ir_call_tail(i)) {
      IRFunction *callee = ir_callee(i).func;
      if (callee == f || ir_attribute(callee, FUNC_ATTR_LEAF)) continue;
    }

    /// Function is not a leaf.
    if (!ir_attribute(f, FUNC_ATTR_LEAF)) return false;
    ir_attribute(f, FUNC_ATTR_LEAF, false);
    return true;
  }

  /// Function is a leaf.
  if (ir_attribute(f, FUNC_ATTR_LEAF)) return false;
  ir_attribute(f, FUNC_ATTR_LEAF, true);
  return true;
}

/// Check whether a function does not return. This function returns whether
/// the noreturn attribute of the function has changed, *not* whether it actually
/// returns or not.
///
/// If a function is marked as noreturn, then that means that it NEVER returns.
/// A function that only sometimes doesn’t return is *not* noreturn.
bool opt_check_noreturn(IRFunction *f) {
  FOREACH_INSTRUCTION_IN_FUNCTION (i, b, f) {
    /// A function that contains a tail call returns, unless the callee does not return.
    ///
    /// Checking regular calls doesn’t help, since we’re checking whether a function may
    /// return, not whether it might not return; tail calls, however, are different, since
    /// they are basically like return statements. We don’t care whether or not regular calls
    /// return, but if a tail call returns, then we must also return.
    IRType kind = ir_kind(i);
    if (kind == IR_CALL && ir_call_tail(i)) {
      /// If the call is a direct call, we can check the noreturn attribute of the callee.
      /// We can’t know whether an indirect call returns, so we must assume that we return.
      if (!ir_call_is_direct(i) || !ir_attribute(ir_callee(i).func, FUNC_ATTR_NORETURN)) goto may_return;
    }

    /// If a return instruction is encountered, then this function obviously returns.
    else if (kind == IR_RETURN) {
    may_return:
      if (!ir_attribute(f, FUNC_ATTR_NORETURN)) return false;
      ir_attribute(f, FUNC_ATTR_NORETURN, false);
      return true;
    }
  }

  /// Function does not return.
  if (ir_attribute(f, FUNC_ATTR_NORETURN)) return false;
  ir_attribute(f, FUNC_ATTR_NORETURN, true);
  return true;
}

/// Check if a function is referenced by this instruction.
typedef Map(IRFunction*, bool) FuncBoolMap;
static void check_function_references(IRInstruction *inst, FuncBoolMap *referenced) {
  STATIC_ASSERT(IR_COUNT == 40, "Handle all instructions that can reference a function");
  switch (ir_kind(inst)) {
    default: break;
    case IR_FUNC_REF: map_set(*referenced, ir_func_ref_func(inst), true); break;
    case IR_CALL: {
      /// Only non-recursive direct calls count as references.
      IRFunction *callee = ir_callee(inst).func;
      if (ir_call_is_direct(inst) && ir_parent(ir_parent(inst)) != callee)
        map_set(*referenced, callee, true);
    } break;
  }
}

/// Analyse functions to determine whether they’re pure, leaf functions, etc.
bool opt_analyse_functions(CodegenContext *ctx) {
  bool ever_changed = false, changed;
  FuncBoolMap referenced_functions = {0};
  do {
    map_clear(referenced_functions);
    changed = false;

    /// Check function attributes.
    foreach_val (f, ctx->functions) {
      switch (ir_linkage(f)) {
        case LINKAGE_IMPORTED:
        case LINKAGE_REEXPORTED:
          continue;

        case LINKAGE_LOCALVAR:
        case LINKAGE_INTERNAL:
          map_set(referenced_functions, f, false);
          break;

        case LINKAGE_EXPORTED:
        case LINKAGE_USED:
          map_set(referenced_functions, f, true);
          break;
      }

      changed |= opt_check_pure(f);
      changed |= opt_check_leaf(f);
      changed |= opt_check_noreturn(f);
    }

    /// The entry point is always referenced.
    map_set(referenced_functions, ctx->entry, true);

    /// Check if the functions are ever referenced.
    FOREACH_INSTRUCTION_IN_CONTEXT (i, b, f, ctx)
      check_function_references(i, &referenced_functions);

    /// Also check in global variables.
    foreach_val (var, ctx->static_vars)
      if (var->init)
        check_function_references(var->init, &referenced_functions);

    /// Delete functions that are never referenced.
    foreach (entry, referenced_functions) {
      if (!entry->value) {
        ir_delete_function(entry->key);
        changed = true;
      }
    }

    if (changed) ever_changed = true;
  } while (changed);

  map_delete(referenced_functions);
  return ever_changed;
}

/// Remove stores and references to global variables that are
/// not exported and never loaded from or passed to an instruction
/// that clobbers memory.
static bool opt_remove_globals(CodegenContext *ctx) {
  /// If a variable is only ever used by stores and variable references,
  /// then we can remove it.
  foreach_val (var, ctx->static_vars) {
    var->referenced = false;
    foreach_val (ref, var->references) {
      FOREACH_USER(user, ref) {
        /// If the user is not a store or if the variable is the value
        /// being stored, and not the address being stored, to, we can’t
        /// remove it.
        if (ir_kind(user) != IR_STORE || ir_store_value(user) == ref) {
          var->referenced = true;
          goto next;
        }
      }
    }

    next:;
  }

  /// Remove all stores to such variables.
  bool changed = false;
  foreach_val (var, ctx->static_vars) {
    if (var->referenced) continue;
    foreach_val (ref, var->references) {
      for (usz i = 0, count = ir_use_count(ref); i < count; i++) {
        IRInstruction *user = ir_user_get(ref, 0 /** (!) **/);
        ASSERT(ir_kind(user) == IR_STORE);
        ir_remove(user);
        changed = true;
      }
    }
  }

  return changed;
}

/// ===========================================================================
///  Block reordering etc.
/// ===========================================================================
/// Map containing the predecessors of each block.
typedef MultiMap(IRBlock*, IRBlock*) Predecessors;

/// Collect the predecessors of each block and
/// remove unreachable blocks.
static bool collect_preds_and_prune(IRFunction *f, Predecessors *preds) {
  bool changed = false;

  mmap_clear(*preds);
  FOREACH_BLOCK (block, f) {
    STATIC_ASSERT(IR_COUNT == 40, "Handle all branch instructions");
    IRInstruction *br = ir_terminator(block);
    switch (ir_kind(br)) {
      default: break;
      case IR_BRANCH:
        mmap_insert(*preds, ir_dest(br), block);
        break;

      case IR_BRANCH_CONDITIONAL:
        mmap_insert(*preds, ir_then(br), block);
        mmap_insert(*preds, ir_else(br), block);
        break;
    }
  }

  /// Collect unreachable blocks.
  Vector(IRBlock *) to_remove = {0};
  FOREACH_BLOCK (block, f) {
    /// Entry block is always reachable.
    if (block_ptr == block_begin_ptr) continue;

    /// If the block has no predecessors, it’s unreachable.
    if (!map_get(*preds, block)) {
      vector_push(to_remove, block);
      changed = true;
    }
  }

  /// Remove them.
  foreach_val (block, to_remove) ir_delete_block(block);
  vector_delete(to_remove);
  return changed;
}

/// Perform jump threading and similar optimisations.
static bool opt_jump_threading(CodegenContext *ctx, IRFunction *f, Predecessors *preds) {
  bool changed = false;
  IRBlockVector to_remove = {0};

  /// Remove blocks that consist of a single direct branch.
  ///
  /// Also simplify conditional branches whose true and false
  /// blocks are the same and fold chains of unconditional branches.
  FOREACH_BLOCK (b, f) {
    if (vector_contains(to_remove, b)) continue;
    IRInstruction *last = ir_terminator(b);

    /// Merge trivially connected blocks.
    IRType last_kind = ir_kind(last);
    if (last_kind == IR_BRANCH) {
      /// If the block has more than one predecessor, we can’t remove it, but
      /// if it contains only a single branch instruction, we can replace this
      /// branch with that instruction.
      ///
      /// Note that this also works for conditional branches. The reason is that
      /// if the only instruction of a block is a conditional branch, then the
      /// condition must be the same for all of its predecessors and thus dominate
      /// all of its predecessors, otherwise, there would have to be a PHI. Thus,
      /// the condition also dominates this block in such cases, and we are free
      /// to copy the branch. The same is true for return instructions that return
      /// a value.
      IRBlock *successor = ir_dest(last);
      if (map_get(*preds, successor)->size != 1) {
        IRInstruction *first = *ir_begin(successor);
        STATIC_ASSERT(IR_COUNT == 40, "Handle all branch instructions");
        switch (ir_kind(first)) {
          default: continue;
          case IR_BRANCH: ir_dest(last, ir_dest(first)); break;
          case IR_UNREACHABLE:
            ir_replace(last, ir_create_unreachable(ctx));
            break;

          case IR_RETURN:
            ir_replace(last, ir_create_return(ctx, ir_operand(first)));
            break;

          case IR_BRANCH_CONDITIONAL:
            ir_replace(last, ir_create_cond_br(ctx, ir_cond(first), ir_then(first), ir_else(first)));
            break;
        }

        changed = true;
        continue;
      }

      /// Otherwise, eliminate the branch and move all instructions
      /// from the destination block into this block.
      ir_remove(last);
      ir_merge_blocks(b, successor);
      vector_push(to_remove, successor);
      changed = true;
    }

    /// Simplify conditional branches.
    else if (last_kind == IR_BRANCH_CONDITIONAL && ir_then(last) == ir_else(last)) {
      ir_replace(last, ir_create_br(ctx, ir_then(last)));
      changed = true;
    }
  }

  /// Remove all blocks that we have marked for removal.
  foreach_val (b, to_remove) ir_delete_block(b);
  vector_delete(to_remove);

  /// Done.
  return changed;
}

/// Simplify Control Flow Graph.
static bool opt_simplify_cfg(CodegenContext *ctx, IRFunction *f) {
  /// Compute predecessors and delete unreachable blocks.
  Predecessors preds = {0};
  bool ever_changed = false;
  for (;;) {
    collect_preds_and_prune(f, &preds);
    bool changed = opt_jump_threading(ctx, f, &preds);
    if (!changed) break;
    else ever_changed = true;
  }
  mmap_delete(preds);
  return ever_changed;
}

/// Check if two instructions refer to the same in-memory object.
static bool same_memory_object(IRInstruction *a, IRInstruction *b) {
  if (a == b) return true;

  /// Two static refs that refer to the same variable
  /// are the same object, unless they have different
  /// types; this can happen because of how references
  /// work.
  if (ir_kind(a) == ir_kind(b) && ir_kind(a) == IR_STATIC_REF) {
    return ir_static_ref_var(a) == ir_static_ref_var(b) && type_equals(ir_typeof(a), ir_typeof(b));
  }

  return false;
}

/// Foreach block, replace loads from a variable with the last value
/// stored to that variable in that block, if any.
///
/// Keep in mind that call instructions may modify locals, so if the
/// address of a variable is ever taken, we can’t forward stores to
/// that variable across calls.
static bool opt_store_forwarding(IRFunction *f) {
  Vector(struct var {
    IRInstruction *addr;
    IRInstruction *store;
    IRInstruction *last_value;
    bool escaped; /// Variable may be modified by a call etc.
    bool reload_required; /// Variable may have been modified since the previous store.
  }) vars = {0};
  bool changed = false;
  IRInstructionVector to_remove = {0};

  FOREACH_BLOCK (b, f) {
    vector_clear(vars);
    FOREACH_INSTRUCTION (i, b) {
      switch (ir_kind(i)) {
        case IR_STORE: {
          IRInstruction *addr = ir_store_addr(i);
          IRInstruction *value = ir_store_value(i);
          struct var *v = vector_find_if(el, vars, same_memory_object(el->addr, addr));
          if (v) {
            /// Update the stored value.
            v->last_value = value;

            /// Eliminate the previous store if the address is never used.
            if (v->store && (!v->escaped || !v->reload_required)) {
              vector_push(to_remove, v->store);
              changed = true;
            }

            /// Update the store.
            v->reload_required = false;
            v->store = i;

            /// Also clobber all other escaped values because of aliasing.
            foreach (var, vars)
              if (var != v && var->escaped)
                var->reload_required = true;
          } else {
            vector_push(
              vars,
              (struct var){
                .addr = addr,
                .store = i,
                .last_value = value,
                .escaped = false,
                .reload_required = false,
              }
            );

            /// Check if the address is ever used by any instruction other
            /// than a load or store; if it is, we can’t forward stores
            /// across calls, intrinsics, or other instructions that may
            /// modify memory.
            if (ir_kind(addr) == IR_ALLOCA) {
              FOREACH_USER(user, addr) {
                if (ir_kind(user) != IR_LOAD && ir_kind(user) != IR_STORE) {
                  vector_back(vars).escaped = true;
                  break;
                }
              }
            }

            /// Anything that isn’t an alloca always escapes.
            else {
              vector_back(vars).escaped = true;
            }
          }
        } break;

        case IR_LOAD: {
          IRInstruction *op = ir_operand(i);
          struct var *v = vector_find_if(el, vars, same_memory_object(el->addr, op));

          /// If the variable is not escaped or does not need
          /// to be reloaded, replace the load with the last
          /// known value.
          if (v && (!v->escaped || !v->reload_required)) {
            /// Replace only the uses so we don’t invalidate
            /// any iterators. DCE will yeet the load later.
            ir_replace_uses(i, v->last_value);
            changed = true;
          }

          /// If the load is not replaced, then we at least no
          /// longer need to reload it.
          else if (v) {
            v->reload_required = false;
            v->last_value = i;
          }

          /// If the variable hasn’t been encountered yet, add it.
          else {
            vector_push(
              vars,
              (struct var){
                .addr = op,
                .store = NULL,
                .last_value = i,
                .escaped = true, /// Certainly escaped since we don’t know where it came from.
                .reload_required = false,
              }
            );
          }
        } break;

        /// Instructions that may clobber memory.
        case IR_CALL:
        case IR_INTRINSIC: {
          foreach (var, vars)
            if (var->escaped)
              var->reload_required = true;
        } break;

        default:
          ASSERT(!clobbers_memory(i), "Handle instruction that clobbers memory correctly");
          break;
      }
    }
  }

  /// Remove all stores that we have marked for removal.
  foreach_val (i, to_remove) ir_remove(i);
  vector_delete(to_remove);
  vector_delete(vars);
  return changed;
}

/// ===========================================================================
///  Driver
/// ===========================================================================
#ifdef __clang__
PUSH_IGNORE_WARNING("-Wbitwise-instead-of-logical")
#endif

void codegen_optimise(CodegenContext *ctx) {
  opt_analyse_functions(ctx);

  /// Uncomment this to debug the function analysis pass.
  /// print("====== After Function Analysis ======\n");
  /// ir_set_ids(ctx);
  /// ir_print(stdout, ctx);

  /// Optimise each function individually.
  do {
    foreach_val (f, ctx->functions) {
      if (!ir_func_is_definition(f) || ir_attribute(f, FUNC_ATTR_NOOPT)) continue;
      do {
        /// Uncomment this to debug optimisation passes.
        /// print("====== OPTIMISATION PASS over %S ======\n", ir_name(f));
        /// ir_set_func_ids(f);
        /// ir_print_function(stdout, f);
      } while (
        opt_simplify_cfg(ctx, f) |
        opt_instcombine(ctx, f) |
        opt_dce(f) |
        opt_mem2reg(f) |
        opt_store_forwarding(f) |
        opt_tail_call_elim(f)
      );
    }
  }

  /// Cross-function optimisations.
  ///
  /// FIXME: Currently, the `opt_analyse_functions()` pass deletes a
  /// lot of unused functions and thus hides backend errors that would
  /// otherwise cause tests to fail when we try and emit those functions.
  /// At some point, we should comment out this pass here and fix all
  /// the backend errors that that will inevitably cause.
  while (opt_inline(ctx, 20) | opt_analyse_functions(ctx) | opt_remove_globals(ctx));
}

/// Called after RA.
void codegen_optimise_blocks(CodegenContext *ctx) {
  foreach_val (f, ctx->functions) {
    if (!ir_func_is_definition(f)) continue;
    opt_simplify_cfg(ctx, f);
  }
}