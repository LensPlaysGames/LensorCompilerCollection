#include <codegen/opt/opt-internal.h>
#include <ir/ir-impl.h>

#define ROOT_INLINE_ENTRY ((usz) -1)

typedef struct InlineContext {
  Vector(struct history_entry {
    IRInstruction *call; /// May point to freed memory, don’t dereference.
    IRFunction *callee;  /// The function called by this call.
    usz inlined_via;     /// Index into this history. -1 if root entry.
  }) history;
  Vector(IRInstruction *) not_inlinable;
  bool may_fail;
} InlineContext;

typedef struct {
  bool changed;
  bool failed;
} inline_result;

/// Compute the number of instructions in a function.
static isz instruction_count(IRFunction *f, bool include_parameters) {
  isz j = 0;
  FOREACH_INSTRUCTION_IN_FUNCTION (i, b, f)
    if (include_parameters || (ir_kind(i) != IR_PARAMETER))
      j++;
  return j;
}

/// Inline a call.
///
/// This will always inline at least one call, if possible, irrespective
/// of the value of the inlining threshold.
///
/// \param cg Codegen context.
/// \param stack Inline context.
/// \param call The call instruction to inline.
/// \param threshold Inlining threshold in number of instructions for
///        nested calls. If 0, inline everything; if -1, inline only
///        this call.
/// \param Whether this is allowed to fail; if it isn’t, we issue a
///        diagnostic on error.
/// \return An inline_result describing what happened.
static inline_result ir_inline_call(
  CodegenContext *ctx,
  InlineContext *ictx,
  IRInstruction *call,
  isz threshold
) {
  /// Save the instruction before and after the call.
  IRFunction *const callee = call->call.callee_function;
  IRBlock *const call_block = call->parent_block;
  const bool is_tail_call = call->call.tail_call;
  usz call_history_index = 0;
  bool may_fail = ictx->may_fail && !call->call.force_inline;

  /// Handle the degenerate case of the callee being empty.
  isz count = instruction_count(callee, false);
  if (count == 0) {
    ASSERT(call->users.size == 0, "Call to empty function cannot possibly return a value");
    ir_remove(call);
    return (inline_result) {
      .changed = false,
      .failed = true,
    };
  }

  /// Add number of parameters that the callee takes.
  count += (isz) callee->parameters.size;

  /// If the call does not yet exists in the history, add it. If it
  /// does, check if one of its parents is itself.
  {
    struct history_entry *e = NULL;
    foreach_index (i, ictx->history) {
      if (ictx->history.data[i].call == call) {
        e = ictx->history.data + i;
        call_history_index = i;
      }
    }

    /// Call exists.
    if (e) {
      ASSERT(e->inlined_via != ROOT_INLINE_ENTRY);
      e = ictx->history.data + e->inlined_via;
      for (;;) {
        /// If the inlining of this call can be traced back to the inlining
        /// of the same function, then we have an infinite loop.
        if (e->callee == callee) {
          if (!may_fail) {
            issue_diagnostic(
              DIAG_ERR,
              ctx->ast->filename.data,
              as_span(ctx->ast->source),
              (loc){0},
              "Failed to inline function %S into %S: Infinite loop detected",
              callee->name,
              call->parent_block->function->name
            );
          }

          return (inline_result) {
            .changed = false,
            .failed = true,
          };
        }

        /// Go to our parent’s parent.
        if (e->inlined_via == ROOT_INLINE_ENTRY) break;
        e = ictx->history.data + e->inlined_via;
      }
    }

    /// Call does not exist. Add it as a root to the history. This means
    /// this call was already in the function and wasn’t inlined from
    /// anywhere—at least not in this inlining pass.
    else {
      call_history_index = ictx->history.size;
      vector_push(
        ictx->history,
        (struct history_entry){
          .call = call,
          .callee = callee,
          .inlined_via = ROOT_INLINE_ENTRY,
        }
      );
    }
  }

  /// Remove the call from the list and everything after it
  /// from the block, but leave everything after it connected.
  /// Note that the call cannot be the last instruction in the
  /// block.
  IRInstructionVector after_call = {0};
  vector_move_to(after_call, call_block->instructions, index_in_block(call) + 1);

  /// Copy instructions from the callee into the caller, replacing
  /// any parameter references with the arguments to the call. Since
  /// there may be forward references, we need to create a skeleton
  /// of the IR first, and only then copy all the instruction data.
  ///
  /// This entails allocating as many blocks and IR instructions as
  /// there are in the callee, and then copying the instructions
  /// one by one. This way, we effectively create a mapping of
  /// instructions and blocks of the callee to these instructions
  /// and blocks, which can then be inserted into the caller.
  usz block_count = callee->blocks.size;
  Vector(IRInstruction *) instructions = {0};
  Vector(IRBlock *) blocks = {0};
  vector_reserve(instructions, (usz) count);
  vector_reserve(blocks, block_count);

  /// Allocate instructions separately; this is unfortunately necessary
  /// since they will be freed separately later on. The same also applies
  /// to the blocks, except that the first block, i.e. the block into which
  /// we start inserting, is mapped to the block containing the call.
  vector_push(blocks, call_block);
  for (usz i = 0; i < (usz) count; i++) vector_push(instructions, calloc(1, sizeof(IRInstruction)));
  for (usz i = 1; i < block_count; i++) {
    IRBlock *b = calloc(1, sizeof(IRBlock));
    b->function = call_block->function;
    vector_push(blocks, b);
  }

  /// Enumerate instructions.
  ///
  /// Any parameter references are mapped to the last N instructions of
  /// the instructions vector, which is where we are going to insert the
  /// call arguments. That way, parameter references are automatically
  /// replaced with the corresponding arguments.
  u32 block_id = 0;
  u32 instruction_id = 0;
  foreach_val (block, callee->blocks) {
    block->id = block_id++;
    foreach_val (inst, block->instructions) {
      if (inst->kind == IR_PARAMETER) {
        u32 mapped_index = (u32) ((usz) count - callee->parameters.size + inst->imm);
        inst->id = mapped_index;
      } else {
        inst->id = instruction_id++;
      }
    }
  }

  /// Put the arguments at the end of the instructions vector.
  foreach_index (i, call->call.arguments) {
    u32 mapped_index = (u32) ((usz) count - callee->parameters.size + i);
    instructions.data[mapped_index] = call->call.arguments.data[i];
  }

  /// Map an instruction or block to its replacement.
#define MAP(inst)        instructions.data[(inst)->id]
#define MAP_BLOCK(block) blocks.data[(block)->id]

  /// PHI and return block in case the callee contains more
  /// than one return instruction and returns a value.
  IRInstruction *return_value = NULL;
  IRBlock *return_block = NULL;

  /// To prevent infinite loops without progress, we only consider
  /// inlining to have ‘changed’ the program—i.e. made progress in
  /// optimisation—if we end up inlining more than one instruction,
  /// or a single instruction that is not a call.
  ///
  /// Control-flow instructions and PHIs do not count towards this.
  usz inlined = 0;

  /// Copy the instructions.
  foreach_val (block, callee->blocks) {
    foreach_val (inst, block->instructions) {
      /// Skip parameters.
      if (inst->kind == IR_PARAMETER) continue;

      /// Copy common data.
      IRInstruction *copy = MAP(inst);
      copy->kind = inst->kind;
      copy->type = inst->type;

      /// Copy instruction-specific data.
      STATIC_ASSERT(IR_COUNT == 40, "Handle all instructions in inliner");
      switch (inst->kind) {
        case IR_LIT_INTEGER:
        case IR_LIT_STRING:
        case IR_REGISTER:
        case IR_PARAMETER:
        case IR_POISON:
        case IR_COUNT: UNREACHABLE();

        case IR_IMMEDIATE: copy->imm = inst->imm; break;
        case IR_FUNC_REF: copy->function_ref = inst->function_ref; break;
        case IR_UNREACHABLE: break;
        case IR_ALLOCA: copy->alloca = inst->alloca; break;

        /// Static refs need to be registered.
        case IR_STATIC_REF:
          copy->static_ref = inst->static_ref;
          vector_push(inst->static_ref->references, copy);
          break;

        STATIC_ASSERT(INTRIN_BACKEND_COUNT == 3, "Handle all backend intrinsics in inliner");
        case IR_INTRINSIC:
          copy->call.intrinsic = inst->call.intrinsic;
          FALLTHROUGH;

        case IR_CALL: {
          copy->call.is_indirect = inst->call.is_indirect;
          copy->call.tail_call = inst->call.tail_call;
          if (inst->call.is_indirect) copy->call.callee_instruction = MAP(inst->call.callee_instruction);
          else copy->call.callee_function = inst->call.callee_function;
          foreach_val (arg, inst->call.arguments)
            vector_push(copy->call.arguments, MAP(arg));

          /// Record the origin of this call.
          if (inst->kind == IR_CALL) {
            vector_push(
              ictx->history,
              (struct history_entry){
                .callee = inst->call.is_indirect ? NULL : inst->call.callee_function,
                .call = copy,
                .inlined_via = call_history_index,
              }
            );
          }
        } break;

        case IR_LOAD:
        case IR_COPY:
        case IR_ZERO_EXTEND:
        case IR_SIGN_EXTEND:
        case IR_TRUNCATE:
        case IR_BITCAST:
        case IR_NOT:
          copy->operand = MAP(inst->operand);
          break;

        case IR_ADD:
        case IR_SUB:
        case IR_MUL:
        case IR_DIV:
        case IR_MOD:
        case IR_SHL:
        case IR_SAR:
        case IR_SHR:
        case IR_AND:
        case IR_OR:
        case IR_LT:
        case IR_LE:
        case IR_GT:
        case IR_GE:
        case IR_EQ:
        case IR_NE:
          copy->lhs = MAP(inst->lhs);
          copy->rhs = MAP(inst->rhs);
          break;

        case IR_STORE:
          copy->store.value = MAP(inst->store.value);
          copy->store.addr = MAP(inst->store.addr);
          break;

        case IR_BRANCH:
          copy->destination_block = MAP_BLOCK(inst->destination_block);
          break;

        case IR_BRANCH_CONDITIONAL:
          copy->cond_br.condition = MAP(inst->cond_br.condition);
          copy->cond_br.then = MAP_BLOCK(inst->cond_br.then);
          copy->cond_br.else_ = MAP_BLOCK(inst->cond_br.else_);
          break;

        case IR_PHI:
          foreach_index (arg, inst->phi_args) {
            IRPhiArgument new = {
              .value = MAP(inst->phi_args.data[arg].value),
              .block = MAP_BLOCK(inst->phi_args.data[arg].block),
            };
            vector_push(copy->phi_args, new);
            inlined--; /// This doesn’t count.
          }
          break;

        /// Returns need to be converted to branches to the return
        /// block, and their operands added to the return value phi.
        /// The only exception is if the callee contains only one return
        /// instruction at the very end, in which case we can just inline
        /// it.
        case IR_RETURN: {
          /// If this is a tail call, just emit the return instruction.
          if (is_tail_call) {
            copy->operand = MAP(inst->operand);
            break;
          }

          /// Otherwise, if this is the last return instruction in
          /// the function, and it’s also the only one, just set
          /// the return value and discard it.
          if (!return_block) {
            if (block == vector_back(callee->blocks) && inst == vector_back(block->instructions)) {
              if (inst->operand) {
                return_value = MAP(inst->operand);
                MAP(inst) = call; /// See below.
              }

              /// Continue because we want to drop this instruction, not insert it.
              continue;
            }

            /// If this is not the last return instruction, we need a
            /// separate return block.
            return_block = ir_block(ctx);
            if (inst->operand) {
              return_value = calloc(1, sizeof(IRInstruction));
              return_value->kind = IR_PHI;
              ir_insert_at_end(return_block, return_value);
            }
          }

          /// Add to the PHI and branch.
          copy->kind = IR_BRANCH;
          copy->destination_block = return_block;
          if (inst->operand) {
            IRPhiArgument new = {
              .value = MAP(inst->operand),
              .block = block,
            };
            vector_push(return_value->phi_args, new);

            /// Replace ourselves with the call instruction in
            /// the vector so uses are updated correctly later on.
            MAP(inst) = call;
          }
        } break;
      }

      /// Insert the instruction into the block.
      if (!ir_is_branch(copy)) inlined++;
      ir_insert_at_end(MAP_BLOCK(block), copy);
    }
  }

  /// Fix up uses.
  FOREACH_INSTRUCTION_IN_FUNCTION (instruction, b, callee) {
    IRInstruction *copy = MAP(instruction);
    foreach_val (user, instruction->users)
      mark_used(copy, MAP(user));
  }

  /// Fix up the return value by replacing all uses of the
  /// call with the return value.
  if (return_value) {
    remove_use(return_value, call);
    ir_replace_uses(call, return_value);
  }

  /// Delete the call.
  ir_remove(call);

  /// If we have a return block, insert it after the last block
  /// among the call block and the inlined blocks.
  if (return_block) {
    return_block->function = call_block->function;
    vector_push(blocks, return_block);
  }

  /// Add any instructions after the call to the last block. Note
  /// that if the call was a tail call, we just drop
  /// everything after the call and return.
  IRBlock *last = vector_back(blocks);

  /// Skip the first block of the vector because that’s the
  /// call block, which is already inserted.
  struct {
    IRBlock **data;
    usz size;
  } vector_span = {
    .data = blocks.data + 1,
    .size = blocks.size - 1,
  };

  /// Insert all blocks into the function after the call block.
  vector_insert_all(
    call_block->function->blocks,
    vector_find_if(el, call_block->function->blocks, *el == call_block) + 1,
    vector_span
  );

  /// Remove every instruction after a tail call.
  if (is_tail_call)
    foreach_val (inst, after_call)
      ir_remove(inst);

  /// Insert instructions after the call into the last block.
  else
    foreach_val (inst, after_call)
      ir_force_insert_at_end(last, inst);


  /// Check if we were able to make progress.
  bool changed = inlined != 1 || instructions.data[0]->kind != IR_CALL;

  /// Free unused instructions.
  foreach_val (i, instructions)
    if (i != call && !i->parent_block)
      free(i);

  /// Delete vectors.
  vector_delete(instructions);
  vector_delete(blocks);

  /// Done!
  return (inline_result) {
    .changed = changed,
    .failed = false,
  };
}

#undef REPLACE

/// Returns 1 if changed, -1 on error, 0 otherwise.
static inline_result inline_calls_in_function(
  CodegenContext *ctx,
  InlineContext *ictx,
  IRFunction *f,
  isz threshold
) {
  inline_result res = {0};
  vector_clear(ictx->history);

again:
  foreach_val (block, f->blocks) {
    foreach_val (inst, block->instructions) {
      /// Skip non-calls and indirect calls.
      if (inst->kind != IR_CALL) continue;
      if (inst->call.is_indirect) continue;

      /// Skip calls to external functions.
      IRFunction *callee = inst->call.callee_function;
      if (!ir_func_is_definition(callee)) continue;

      /// Skip calls that we’ve already determined are impossible to inline.
      if (vector_contains(ictx->not_inlinable, inst)) continue;

      /// Skip noinline functions unless the user has overriden this
      /// with __builtin_inline().
      if (callee->attr_noinline && !inst->call.force_inline) continue;

      /// Whether failure is acceptable.
      bool may_fail = ictx->may_fail && !inst->call.force_inline;

      /// Whether this has to be inlined.
      bool must_inline = inst->call.force_inline || callee->attr_inline || threshold == 0;

      /// Inline the call if requested.
      if (must_inline || threshold >= instruction_count(callee, false)) {
        /// If the callee is the caller, only allow inlining tail calls.
        if (f == callee) {
          if (!inst->call.tail_call) {
            /// If we must inline this call, try to check if it
            /// could be a tail call at least once.
            if (must_inline && !opt_try_convert_to_tail_call(inst)) {
              /// We can’t inline this.
              if (may_fail) issue_diagnostic(
                DIAG_ERR,
                ctx->ast->filename.data,
                as_span(ctx->ast->source),
                (loc){0},
                "Sorry, could not inline non-tail-recursive call"
              );
              res.failed = true;
              vector_push(ictx->not_inlinable, inst);
            }
          }

          /// Tail-recursion is better than inlining, so we
          /// leave tail-recursive calls alone.
          continue;
        }

        /// Inline it.
        inline_result inlined = ir_inline_call(ctx, ictx, inst, threshold);
        if (inlined.changed) res.changed = true;
        if (inlined.failed) {
          res.failed = true;
          vector_push(ictx->not_inlinable, inst);
        }

        /// This may have added new blocks *after* this block,
        /// so start over from the start of this block.
        goto again;
      }
    }
  }

  return res;
}

/// Run the inliner.
static inline_result run_inliner(CodegenContext *ctx, isz threshold, bool may_fail) {
  /// Disjoint-sets datastructure for Kruskal’s algorithm for cycle
  /// detection to make sure we don’t fall into an infinite loop.
  InlineContext ictx = {
    .history = {0},
    .not_inlinable = {0},
    .may_fail = may_fail,
  };

  inline_result res = {0};
  foreach_val (f, ctx->functions) {
    inline_result r = inline_calls_in_function(ctx, &ictx, f, f->attr_flatten ? 0 : threshold);
    if (r.failed) res.failed = true;
    if (r.changed) res.changed = true;
  }

  vector_delete(ictx.history);
  vector_delete(ictx.not_inlinable);
  return res;
}

bool opt_inline(CodegenContext *ctx, isz threshold) {
  return run_inliner(ctx, threshold, true).changed;
}

bool codegen_process_inline_calls(CodegenContext *ctx) {
  return !run_inliner(ctx, -1, false).failed;
}
