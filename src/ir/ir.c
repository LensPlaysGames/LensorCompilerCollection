#include <ast.h>
#include <codegen/codegen_forward.h>
#include <codegen/x86_64/arch_x86_64.h>
#include <ir/dom.h>
#include <ir/ir-impl.h>
#include <ir/ir.h>
#include <platform.h>
#include <stdlib.h>
#include <utils.h>

//#define DEBUG_USES

typedef IRInstruction Inst;
typedef IRBlock Block;
typedef IRFunction Func;
typedef IRValue Value;

/// ===========================================================================
///  Context Creation
/// ===========================================================================
CodegenContext *codegen_context_create(
  Module *ast,
  CodegenArchitecture arch,
  CodegenTarget target,
  CodegenCallingConvention call_convention,
  FILE *code
) {
  CodegenContext *context;

  STATIC_ASSERT(ARCH_COUNT == 2, "codegen_context_create() must exhaustively handle all codegen architectures.");
  STATIC_ASSERT(TARGET_COUNT == 6, "codegen_context_create() must exhaustively handle all codegen targets.");
  STATIC_ASSERT(CG_CALL_CONV_COUNT == 2, "codegen_context_create() must exhaustively handle all codegen calling conventions.");

  switch (arch) {
    case ARCH_X86_64:
      // Handle call_convention for creating codegen context!
      if (call_convention == CG_CALL_CONV_MSWIN) {
        context = codegen_context_x86_64_mswin_create();
      } else if (call_convention == CG_CALL_CONV_SYSV) {
        context = codegen_context_x86_64_linux_create();
      } else {
        ICE("Unrecognized calling convention!");
      }
      break;
    default: UNREACHABLE();
  }

  context->poison = calloc(1, sizeof(IRInstruction));
  context->poison->kind = IR_POISON;
  context->poison->ctx = context;

  context->arch = arch;
  context->target = target;
  context->call_convention = call_convention;

  context->ast = ast;
  context->code = code;

  return context;
}

void codegen_context_free(CodegenContext *context) {
  STATIC_ASSERT(ARCH_COUNT == 2, "codegen_context_free() must exhaustively handle all codegen architectures.");
  STATIC_ASSERT(TARGET_COUNT == 6, "codegen_context_free() must exhaustively handle all codegen targets.");
  STATIC_ASSERT(CG_CALL_CONV_COUNT == 2, "codegen_context_free() must exhaustively handle all codegen calling conventions.");

  /// Free all IR Functions.
  foreach_val (f, context->functions) ir_delete_function(f);

  /// Finally, delete the function vector.
  vector_delete(context->functions);

  /// Free static variables.
  foreach_val (var, context->static_vars) {
    free(var->name.data);
    free(var);
  }
  vector_delete(context->static_vars);

  /// Free free lists.
  foreach_val (b, context->free_blocks) free(b);
  foreach_val (i, context->free_instructions) free(i);
  vector_delete(context->free_blocks);
  vector_delete(context->free_instructions);

  /// Free backend-specific data.
  STATIC_ASSERT(ARCH_COUNT == 2, "Exhaustive handling of architectures");
  switch (context->arch) {
    default: UNREACHABLE();

    case ARCH_X86_64: {
      STATIC_ASSERT(CG_CALL_CONV_COUNT == 2, "Exhaustive handling of calling conventions");
      if (context->call_convention == CG_CALL_CONV_MSWIN) codegen_context_x86_64_mswin_free(context);
      else if (context->call_convention == CG_CALL_CONV_SYSV) codegen_context_x86_64_linux_free(context);
      else ICE("Unrecognized calling convention!");
    } break;
  }

  /// Free the context itself.
  free(context);
}

/// ===========================================================================
///  Helper Functions
/// ===========================================================================
void mark_used(IRInstruction *usee, IRInstruction *user) {
  vector_push_unique(usee->users, user);
}

void remove_use(IRInstruction *usee, IRInstruction *user) {
  if (!usee) return;

#ifdef DEBUG_USES
  eprint("[Use] Removing use of %%%u in %%%u\n", usee ? -1u : usee->id, user->id);
#endif

  vector_remove_element_unordered(usee->users, user);
}

static void ir_internal_unmark_usee(IRInstruction *user, IRInstruction **child, void *_) {
  (void) _;
  foreach_val (child_user, (*child)->users) {
    if (child_user == user) {
      vector_remove_element_unordered((*child)->users, child_user);
      break;
    }
  }
}

void ir_free_instruction_data(IRInstruction *i) {
  if (!i) return;

  STATIC_ASSERT(IR_COUNT == 40, "Handle all instruction types.");
  switch (i->kind) {
    default: break;
    case IR_INTRINSIC:
    case IR_CALL:
      vector_delete(i->call.arguments);
      break;

    case IR_PHI:
      vector_delete(i->phi_args);
      break;

    case IR_STATIC_REF:
      vector_remove_element_unordered(i->static_ref->references, i);
      break;
  }

  /// Free usage data.
  vector_delete(i->users);
}

/// This implements printing a single instruction.
static void ir_emit_instruction(
  string_buffer *out,
  IRInstruction *inst,
  bool dot_format
) {
  ASSERT(inst, "Can not emit NULL inst to file.");

  const usz id_max_width = 7;
  if (inst->id) {
    const usz width = number_width(inst->id);
    for (usz i = width; i < id_max_width; ++i) format_to(out, " ");
    format_to(out, "%34%%%u %31%s ", inst->id, dot_format ? "=" : "│");
  }
  else {
    for (usz i = 0; i < id_max_width; ++i) format_to(out, " ");
    format_to(out, "  %31│ ");
  }

  const usz result_max_width = 6;
  if (inst->result) {
    const usz result_length = number_width(inst->result);
    for (usz i = result_length; i < result_max_width; ++i) format_to(out, " ");
    format_to(out, "%34r%u %31│ ", inst->result);
  } else {
    for (usz i = 0; i < result_max_width; ++i) format_to(out, " ");
    format_to(out, "  %31│ ");
  }

  STATIC_ASSERT(IR_COUNT == 40, "Handle all instruction types.");
  switch (inst->kind) {
  case IR_POISON:
    format_to(out, "%33poison");
    break;

  case IR_IMMEDIATE:
    format_to(out, "%33imm %35%U", inst->imm);
    break;

  case IR_LIT_INTEGER:
    format_to(out, "%33lit.int %35%U", inst->imm);
    break;

  case IR_LIT_STRING:
    format_to(out, "%33lit.str %35%S", inst->str);
    break;

  case IR_INTRINSIC: {
    switch (inst->call.intrinsic) {
      default: format_to(out, "%33intrin.%d ", inst->call.intrinsic); break;
      case INTRIN_BUILTIN_SYSCALL: format_to(out, "%33intrin.syscall "); break;
      case INTRIN_BUILTIN_DEBUGTRAP: format_to(out, "%33intrin.debugtrap "); break;
      case INTRIN_BUILTIN_MEMCPY: format_to(out, "%33intrin.memcpy "); break;
    }

    format_to(out, "%31(");
    bool first = true;
    foreach_val (i, inst->call.arguments) {
      if (!first) { format_to(out, "%31, "); }
      else first = false;
      format_to(out, "%34%%%u", i->id);
    }
    format_to(out, "%31)");
  } break;

  case IR_CALL: {
    if (inst->call.tail_call) { format_to(out, "%33tail "); }
    if (!inst->call.is_indirect) {
      string name = inst->call.callee_function->name;
      format_to(out, "%33call %32%S", name);
    } else {
      format_to(out, "%33call %34%%%u", inst->call.callee_instruction->id);
    }
    format_to(out, "%31(");
    bool first = true;
    foreach_val (i, inst->call.arguments) {
      if (!first) { format_to(out, "%31, "); }
      else first = false;
      format_to(out, "%34%%%u", i->id);
    }
    format_to(out, "%31)");
  } break;
  case IR_STATIC_REF:
    format_to(out, "%33.ref %m%S", inst->static_ref->name);
    break;
  case IR_FUNC_REF:
    format_to(out, "%31ref %32%S", inst->function_ref->name);
    break;

#define PRINT_BINARY_INSTRUCTION(enumerator, name) case IR_##enumerator: \
    format_to(out, "%33" #name " %34%%%u%31, %34%%%u", inst->lhs->id, inst->rhs->id); break;
    ALL_BINARY_INSTRUCTION_TYPES(PRINT_BINARY_INSTRUCTION)
#undef PRINT_BINARY_INSTRUCTION

  case IR_NOT:
    format_to(out, "%33not %34%%%u", inst->operand->id);
    break;

  case IR_ZERO_EXTEND:
    format_to(out, "%33z.ext %34%%%u", inst->operand->id);
    break;

  case IR_SIGN_EXTEND:
    format_to(out, "%33s.ext %34%%%u", inst->operand->id);
    break;

  case IR_TRUNCATE:
    format_to(out, "%33truncate %34%%%u", inst->operand->id);
    break;

  case IR_BITCAST:
    format_to(out, "%33bitcast %34%%%u", inst->operand->id);
    break;

  case IR_COPY:
    format_to(out, "%33copy %34%%%u", inst->operand->id);
    break;

  case IR_PARAMETER:
    format_to(out, "%31.param %34%%%u", inst->id);
    break;

  case IR_RETURN:
    if (inst->operand) format_to(out, "%33ret %34%%%u", inst->operand->id);
    else format_to(out, "%33ret");
    break;
  case IR_BRANCH:
    format_to(out, "%33br bb%Z", inst->destination_block->id);
    break;
  case IR_BRANCH_CONDITIONAL:
    format_to(out, "%33br.cond %34%%%u%31, %33bb%Z%31, %33bb%Z",
            inst->cond_br.condition->id, inst->cond_br.then->id, inst->cond_br.else_->id);
    break;
  case IR_PHI: {
    format_to(out, "%33phi ");
    bool first = true;
    foreach (arg, inst->phi_args) {
      if (first) { first = false; }
      else { format_to(out, "%31, "); }
      format_to(out, "%31[%33bb%Z%31 : %34%%%u%31]", arg->block->id, arg->value->id);
    }
  } break;

  case IR_LOAD:
    format_to(out, "%33load %34%%%u", inst->operand->id);
    break;
  case IR_STORE:
    format_to(out, "%33store into %34%%%u%31, %T %34%%%u", inst->store.addr->id, inst->store.value->type, inst->store.value->id);
    break;
  case IR_REGISTER:
    format_to(out, "%31.reg %34%%%u", inst->result);
    break;
  case IR_ALLOCA:
    format_to(out, "%33alloca %34%U", inst->imm);
    break;

  /// No-op
  case IR_UNREACHABLE:
    format_to(out, "%33unreachable");
    break;
  default:
    ICE("Invalid IRType %d\n", inst->kind);
  }

  // Print type of instruction.
  if (inst->type && !type_is_void(inst->type))
    format_to(out, " %31%s %T", dot_format ? "→" : "|", inst->type);

#ifdef DEBUG_USES
  /// Print users
  format_to(out, "%m\033[60GUsers: ");
  foreach_val (user, inst->users) {
    format_to(out, "%%%u, ", user->id);
  }
#endif

  format_to(out, "%m");
}

typedef struct {
  IRInstruction *usee;
  IRInstruction *replacement;
} ir_internal_replace_use_t;
static void ir_internal_replace_use(IRInstruction *user, IRInstruction **child, void *data) {
  ir_internal_replace_use_t *replace = data;

#ifdef DEBUG_USES
  eprint("  Replacing uses of %%%u in %%%u with %%%u\n",
    replace->usee->id, user->id, replace->replacement->id);
#endif

  if (user == replace->replacement) return;
  if (*child == replace->usee) {
    *child = replace->replacement;
    remove_use(replace->usee, user);
    mark_used(replace->replacement, user);
  }
}

usz index_in_block(Inst *inst) {
  return (usz) (ir_it(inst) - inst->parent_block->instructions.data);
}

void ir_for_each_child(
  IRInstruction *user,
  void callback(IRInstruction *user, IRInstruction **child, void *data),
  void *data
) {
  STATIC_ASSERT(IR_COUNT == 40, "Handle all instruction types.");
  switch (user->kind) {
  case IR_PHI:
      foreach (arg, user->phi_args) {
      callback(user, &arg->value, data);
    }
    break;
  case IR_LOAD:
  case IR_COPY:
  case IR_NOT:
  case IR_ZERO_EXTEND:
  case IR_SIGN_EXTEND:
  case IR_TRUNCATE:
  case IR_BITCAST:
    callback(user, &user->operand, data);
    break;

  case IR_RETURN:
    if (user->operand) callback(user, &user->operand, data);
    break;

  case IR_STORE:
    callback(user, &user->store.addr, data);
    callback(user, &user->store.value, data);
    break;

  ALL_BINARY_INSTRUCTION_CASES()
    callback(user, &user->lhs, data);
    callback(user, &user->rhs, data);
    break;

  case IR_INTRINSIC:
  case IR_CALL:
    if (user->call.is_indirect) callback(user, &user->call.callee_instruction, data);
    foreach (arg, user->call.arguments) callback(user, arg, data);
    break;

  case IR_BRANCH_CONDITIONAL:
    callback(user, &user->cond_br.condition, data);
    break;

  case IR_PARAMETER:
  case IR_IMMEDIATE:
  case IR_BRANCH:
  case IR_ALLOCA:
  case IR_UNREACHABLE:
  case IR_REGISTER:
  case IR_STATIC_REF:
  case IR_FUNC_REF:
  case IR_POISON:
    break;
  default:
    ICE("Invalid IR instruction type %d", user->kind);
  }
}

bool ir_is_value(IRInstruction *instruction) {
  STATIC_ASSERT(IR_COUNT == 40, "Handle all instruction types.");
  // NOTE: If you are changing this switch, you also need to change
  // `needs_register()` in register_allocation.c
  switch (instruction->kind) {
    default: TODO("Handle %d IR instruction type in ir_is_value()", instruction->kind);
    case IR_IMMEDIATE:
    case IR_INTRINSIC:
    case IR_CALL:
    case IR_LOAD:
    case IR_PHI:
    case IR_COPY:
    case IR_PARAMETER:
    case IR_REGISTER:
    case IR_ALLOCA:
    case IR_STATIC_REF:
    case IR_FUNC_REF:
    case IR_NOT:
    case IR_ZERO_EXTEND:
    case IR_SIGN_EXTEND:
    case IR_TRUNCATE:
    case IR_BITCAST:
    case IR_POISON:
    ALL_BINARY_INSTRUCTION_CASES()
      return true;

    case IR_STORE:
    case IR_RETURN:
    case IR_BRANCH:
    case IR_BRANCH_CONDITIONAL:
    case IR_UNREACHABLE:
    case IR_LIT_INTEGER:
    case IR_LIT_STRING:
      return false;
  }
}

void ir_print_defun(FILE *file, IRFunction *f) {
  /// Function signature.
  fprint(
    file,
    "%31%s %32%S %31(",
    !ir_func_is_definition(f) ? "declare" : "defun",
    f->name
  );

  /// Parameters.
  bool first_param = true;
  for (usz i = 1; i <= f->parameters.size; ++i) {
    if (first_param) first_param = false;
    else fprint(file, "%31, ");
    fprint(file, "%34%%%u", f->parameters.data[i - 1]->id);
  }

  /// End of param list.
  fprint(file, "%31)");

  /// Attributes, if any.
#define F(_, name) if (f->attr_##name) fprint(file, " " #name);
  SHARED_FUNCTION_ATTRIBUTES(F)
  IR_FUNCTION_ATTRIBUTES(F)
#undef F
}

/// Clean up context free list so we don’t leak memory.
static void clean_free_list(CodegenContext *ctx) {
  /// Clean up free blocks list.
  if (ctx->free_blocks.size > 1000) {
    for (usz i = 0; i < 500; i++) {
      Block *b = vector_pop(ctx->free_blocks);
      free(b);
    }
  }

  /// Clean up free instructions list.
  if (ctx->free_instructions.size > 10000) {
    for (usz i = 0; i < 5000; i++) {
      Inst *inst = vector_pop(ctx->free_instructions);
      free(inst);
    }
  }
}

/// Free data used by a block.
static void free_block_data(Block *block) {
  if (block->name.size) free(block->name.data);
}

/// Delete an instruction. The `ctx` may be `NULL`.
static void ir_remove_impl(CodegenContext *ctx, IRInstruction *i) {
  if (i->users.size) {
    eprint("Cannot remove used instruction.\nInstruction:\n");
    if (i->parent_block->function) {
      ir_set_func_ids(i->parent_block->function);
      ir_print_instruction(stderr, i);
      eprint("In function:\n");
      ir_print_function(stderr, i->parent_block->function);
    } else {
      ir_print_instruction(stderr, i);
    }
    ICE("Cannot remove used instruction.");
  }

  /// Remove the instruction if it’s inserted in a block.
  if (i->parent_block) vector_remove_element(i->parent_block->instructions, i);

  /// Unmark usees.
  ir_for_each_child(i, ir_internal_unmark_usee, NULL);

  /// Delete instruction data.
  ir_free_instruction_data(i);

  /// Don’t delete the main poison value of a context, but allow
  /// deleting other poison values.
  if (i->kind == IR_POISON && i->ctx && i == i->ctx->poison) return;

  /// Unlink static refs.
  if (i->kind == IR_STATIC_REF)
    vector_remove_element_unordered(i->static_ref->references, i);

  /// Parameters should not be freed here.
  if (i->kind == IR_PARAMETER) return;

  /// Free the instruction.
  if (ctx) {
    i->kind = IR_COUNT;
    ASAN_POISON(i, sizeof(Inst));
    vector_push(ctx->free_instructions, i);
    clean_free_list(ctx);
  } else {
    free(i);
  }
}

static void dot_print_block(FILE *file, IRBlock *block, string_buffer *sb) {
  const Inst* const term =  ir_terminator(block);
  const bool cond_br = term->kind == IR_BRANCH_CONDITIONAL;
  const bool is_term = term->kind != IR_BRANCH && !cond_br;

  /// Emit all instructions to a string.
  vector_clear(*sb);
  foreach_val (i, block->instructions) {
    ir_emit_instruction(sb, i, true);
    format_to(sb, "\\l");
  }

  /// Remove all invalid chars.
  sb_replace(sb, literal_span("|"), literal_span(""));
  sb_replace(sb, literal_span("│"), literal_span(""));
  sb_replace(sb, literal_span("<"), literal_span(""));
  sb_replace(sb, literal_span(">"), literal_span(""));

  /// Print instructions.
  fprint(file, "    Block%p [label=\"{bb%Z|%S", block, block->id, as_span(*sb));

  /// Print branches.
  if (cond_br) fprint(file, "|{<s0>Then|<s1>Else}");

  /// Print rest of node.
  fprint(file, "}\", shape=record, style=filled");

  /// Print colour.
  if (cond_br) fprint(file, ",color=\"#b70d28ff\",fillcolor=\"#b70d2870\"");
  else if (is_term) fprint(file, ",color=\"#4287f5ff\",fillcolor=\"#4287f570\"");

  fprint(file, "];\n");
}

static void ir_print_dot_cfg_function(IRFunction *f, FILE* file) {
  string_buffer sb = {0};
  fprint(file, "digraph \"CFG for %S\" {\n", f->name);
  fprint(file, "    label=\"CFG for %S\";\nlabelloc=\"t\"\n", f->name);
  foreach_val (block, f->blocks) {
    /// Print connections to other blocks.
    const Inst* const term =  ir_terminator(block);
    switch (term->kind) {
      default: break;
      case IR_BRANCH:
        fprint(file, "    Block%p -> Block%p;\n", block, term->destination_block);
        break;
      case IR_BRANCH_CONDITIONAL:
        fprint(file, "    Block%p -> Block%p;\n", block, term->cond_br.then);
        fprint(file, "    Block%p -> Block%p;\n", block, term->cond_br.else_);
        break;
    }

    dot_print_block(file, block, &sb);
  }

  fprint(file, "}\n");
  vector_delete(sb);
}

static void ir_print_dot_dj_function(IRFunction *f, FILE* file) {
  string_buffer sb = {0};
  fprint(file, "digraph \"DJ Graph for %S\" {\n", f->name);
  fprint(file, "    label=\"DJ Graph for %S\";\nlabelloc=\"t\"\n", f->name);

  DominatorTree dom = dom_tree_build(f);

  ir_set_func_ids(f);
  foreach_val (block, f->blocks)
    fprint(file, "    Block%p [label=\"{bb%Z}\", shape=record, style=filled]\n", block, block->id);

  /// We don’t have join edges yet, so just print the
  /// dominator tree for now.
  foreach (entry, dom.doms)
    if (entry->value)
      fprint(file, "    Block%p -> Block%p;\n", entry->value, entry->key);

  fprint(file, "}\n");
  vector_delete(sb);
}

extern const char *print_dot_function;
static void print_dot_impl(CodegenContext *ctx, void print_dot (IRFunction *f, FILE* file)) {
  ASSERT(print_dot_function);
  span s = {
    .data = print_dot_function,
    .size = strlen(print_dot_function),
  };

  /// Find function.
  foreach_val (f, ctx->functions) {
    if (string_eq(f->name, s)) {
      thread_use_colours = false;
      string name = format("%S.dot", f->name);
      FILE *file = fopen(name.data, "w");
      ASSERT(file);

      ir_set_func_ids(f);
      print_dot(f, file);
      fclose(file);
      return;
    }
  }
}

/// ===========================================================================
///  Instruction Creation
/// ===========================================================================
NODISCARD static Inst *alloc(CodegenContext *ctx, IRType kind) {
  Inst *inst = NULL;

  if (ctx && ctx->free_instructions.size) {
    inst = vector_pop(ctx->free_instructions);
    ASAN_UNPOISON(inst, sizeof(Inst));
    *inst = (Inst){0};
  } else {
    inst = calloc(1, sizeof *inst);
  }

  inst->kind = kind;
  inst->type = t_void;
  return inst;
}

NODISCARD static Block* alloc_block(CodegenContext *ctx) {
  Block *block = NULL;

  if (ctx && ctx->free_blocks.size) {
    block = vector_pop(ctx->free_blocks);
    ASAN_UNPOISON(block, sizeof(Block));
    *block = (Block){0};
  } else {
    block = calloc(1, sizeof *block);
  }

  return block;
}

/// Create a basic block.
Block *ir_block(CodegenContext *ctx) { return alloc_block(ctx); }

/// Create a function.
Func *ir_create_function(
  CodegenContext *ctx,
  string name,
  Type *function_type,
  SymbolLinkage linkage
) {
  ASSERT(function_type->kind == TYPE_FUNCTION, "Cannot create function of non-function type");

  IRFunction *function = calloc(1, sizeof(IRFunction));
  function->name = name;
  function->type = function_type;

  /// Add it to the list of functions.
  function->context = ctx;
  function->linkage = linkage;
  vector_push(ctx->functions, function);

  /// A function definition *must* contain at least one block, so we
  /// start new functions out with an empty block.
  if (linkage != LINKAGE_IMPORTED && linkage != LINKAGE_REEXPORTED) {
    IRBlock *block = alloc_block(ctx);
    ctx->function = function;
    ir_block_attach(ctx, block);

    /// Generate param refs.
    ///
    /// We make sure that there is only ever one parameter reference
    /// for reach parameter to avoid duplicating them when we lower
    /// parameters that are split across registers in codegen.
    for (u64 i = 1; i <= function_type->function.parameters.size; ++i) {
      Inst *param = alloc(ctx, IR_PARAMETER);
      param->imm = i - 1;
      param->id = (u32) i;
      param->type = function_type->function.parameters.data[i - 1].type;
      vector_push(function->parameters, param);
      ir_insert_at_end(block, param);
    }
  }
  return function;
}

Inst *ir_create_alloca(CodegenContext *ctx, Type *type) {
  return ir_create_alloca_sized(ctx, type, type_sizeof(type));
}

Inst *ir_create_alloca_sized(CodegenContext *context, Type *type, usz size) {
  Inst *alloca = alloc(context, IR_ALLOCA);
  alloca->alloca.size = size;
  alloca->type = ast_make_type_pointer(context->ast, type->source_location, type);
  return alloca;
}

Inst *ir_create_bitcast(
  CodegenContext *ctx,
  Type *to_type,
  Inst *value
) {
  Inst *bitcast = alloc(ctx, IR_BITCAST);
  bitcast->operand = value;
  bitcast->type = to_type;
  mark_used(value, bitcast);
  return bitcast;
}

Inst *ir_create_br(
  CodegenContext *ctx,
  Block *destination
) {
  Inst *br = alloc(ctx, IR_BRANCH);
  br->destination_block = destination;
  return br;
}

Inst *ir_create_cond_br(
  CodegenContext *ctx,
  Inst *condition,
  Block *then_block,
  Block *else_block
) {
  Inst *br = alloc(ctx, IR_BRANCH_CONDITIONAL);
  br->cond_br.condition = condition;
  br->cond_br.then = then_block;
  br->cond_br.else_ = else_block;
  mark_used(condition, br);
  return br;
}

Inst *ir_create_copy(
  CodegenContext *ctx,
  Inst *source
) {
  Inst *copy = alloc(ctx, IR_COPY);
  copy->operand = source;
  copy->type = source->type;
  mark_used(source, copy);
  return copy;
}

Inst *ir_create_func_ref(CodegenContext *ctx, Func *function) {
  Inst *ref = alloc(ctx, IR_FUNC_REF);
  ref->function_ref = function;
  ref->type = function->type;
  return ref;
}

Inst *ir_create_immediate(
  CodegenContext *ctx,
  Type *type,
  u64 immediate
) {
  Inst *imm = alloc(ctx, IR_IMMEDIATE);
  imm->imm = immediate;
  imm->type = type;
  return imm;
}

Inst *ir_create_int_lit(CodegenContext *ctx, usz value) {
  Inst *lit = alloc(ctx, IR_LIT_INTEGER);
  lit->imm = value;
  lit->type = t_integer;
  return lit;
}

Inst *ir_create_interned_str_lit(CodegenContext *ctx, usz string_index) {
  ASSERT(string_index < ctx->ast->strings.size, "Invalid string index %Z", string_index);
  Inst *s = alloc(ctx, IR_LIT_STRING);
  s->str = ctx->ast->strings.data[string_index];
  s->string_index = string_index;
  return s;
}

Inst *ir_create_intrinsic(CodegenContext *ctx, Type *t, enum IntrinsicKind intrinsic) {
  Inst *i = alloc(ctx, IR_INTRINSIC);
  i->call.intrinsic = intrinsic;
  i->type = t;
  return i;
}

Inst *ir_create_load(
  CodegenContext *ctx,
  Type *type,
  Inst *address
) {
  Inst *load = alloc(ctx, IR_LOAD);
  load->type = type;
  load->operand = address;
  mark_used(address, load);
  return load;
}

IRInstruction *ir_create_memcpy(
  CodegenContext *context,
  IRInstruction *dest,
  IRInstruction *src,
  IRInstruction *size
) {
  IRInstruction *call = ir_create_intrinsic(context, t_void, INTRIN_BUILTIN_MEMCPY);
  vector_push(call->call.arguments, dest);
  vector_push(call->call.arguments, src);
  vector_push(call->call.arguments, size);
  mark_used(dest, call);
  mark_used(src, call);
  mark_used(size, call);
  return call;
}


Inst *ir_create_not(
  CodegenContext *ctx,
  Inst *op
) {
  Inst *not = alloc(ctx, IR_NOT);
  not->operand = op;
  not->type = op->type;
  mark_used(op, not);
  return not;
}

Inst *ir_parameter(Func *func, usz index) {
  ASSERT(ir_func_is_definition(func));
  ASSERT(index < func->parameters.size);
  return func->parameters.data[index];
}

Inst *ir_create_phi(CodegenContext *ctx, Type *type) {
  Inst *phi = alloc(ctx, IR_PHI);
  phi->type = type;
  return phi;
}

Inst *ir_create_register(CodegenContext *ctx, Type *type, Register result) {
  Inst *reg = alloc(ctx, IR_REGISTER);
  reg->type = type;
  reg->result = result;
  return reg;
}

Inst *ir_create_return(
  CodegenContext *ctx,
  Inst *retval
) {
  Inst *ret = alloc(ctx, IR_RETURN);
  ret->operand = retval;
  if (retval) mark_used(retval, ret);
  return ret;
}

Inst *ir_create_sext(
  CodegenContext *ctx,
  Type *result_type,
  Inst *value
) {
  Inst *sext = alloc(ctx, IR_SIGN_EXTEND);
  sext->type = result_type;
  sext->operand = value;
  mark_used(value, sext);
  return sext;
}

IRStaticVariable *ir_create_static(
  CodegenContext *ctx,
  Node *decl,
  Type *type,
  string name
) {
  ASSERT(decl && type);

  /// Create the variable.
  IRStaticVariable *v = calloc(1, sizeof *v);
  v->name = name;
  v->type = type;
  v->decl = decl;
  vector_push(ctx->static_vars, v);
  return v;
}

Inst *ir_create_static_ref(CodegenContext *ctx, IRStaticVariable *var) {
  Inst *ref = alloc(ctx, IR_STATIC_REF);
  ref->static_ref = var;
  vector_push(var->references, ref);
  ref->type = ast_make_type_pointer(ctx->ast, var->type->source_location, var->type);
  return ref;
}

Inst *ir_create_store(
  CodegenContext *ctx,
  Inst *data,
  Inst *address
) {
  Inst *store = alloc(ctx, IR_STORE);
  store->store.addr = address;
  store->store.value = data;
  mark_used(address, store);
  mark_used(data, store);
  return store;
}

Inst *ir_create_trunc(
  CodegenContext *ctx,
  Type *result_type,
  Inst *value
) {
  Inst *trunc = alloc(ctx, IR_TRUNCATE);
  trunc->type = result_type;
  trunc->operand = value;
  mark_used(value, trunc);
  return trunc;
}

Inst *ir_create_unreachable(CodegenContext *ctx) {
  return alloc(ctx, IR_UNREACHABLE);
}

Inst *ir_create_zext(
  CodegenContext *ctx,
  Type *result_type,
  Inst *value
) {
  Inst *zext = alloc(ctx, IR_ZERO_EXTEND);
  zext->type = result_type;
  zext->operand = value;
  mark_used(value, zext);
  return zext;
}

#define CREATE_BINARY_INSTRUCTION(enumerator, name)                   \
  Inst *ir_create_##name(CodegenContext *ctx, Inst *lhs, Inst *rhs) { \
    Inst *x = alloc(ctx, IR_##enumerator);                            \
    x->type = lhs->type;                                              \
    x->lhs = lhs;                                                     \
    x->rhs = rhs;                                                     \
    mark_used(lhs, x);                                                \
    mark_used(rhs, x);                                                \
    return x;                                                         \
  }

/// TODO: Should set type to bool once we have that.
#define CREATE_COMPARISON_INSTRUCTION(enumerator, name)               \
  Inst *ir_create_##name(CodegenContext *ctx, Inst *lhs, Inst *rhs) { \
    Inst *x = alloc(ctx, IR_##enumerator);                            \
    x->type = t_integer;                                              \
    x->lhs = lhs;                                                     \
    x->rhs = rhs;                                                     \
    mark_used(lhs, x);                                                \
    mark_used(rhs, x);                                                \
    return x;                                                         \
  }

ALL_BINARY_INSTRUCTION_TYPES_EXCEPT_COMPARISONS(CREATE_BINARY_INSTRUCTION)
ALL_BINARY_COMPARISON_TYPES(CREATE_COMPARISON_INSTRUCTION)

#undef CREATE_COMPARISON_INSTRUCTION
#undef CREATE_BINARY_INSTRUCTION

/// ===========================================================================
///  Instruction Insertion
/// ===========================================================================
Block *ir_block_attach(CodegenContext *context, Block *block) {
  ASSERT(context->function);
  ASSERT(!block->function);
  vector_push(context->function->blocks, block);
  block->function = context->function;
  context->insert_point = block;
  return block;
}

/// Insert an instruction into the current insert point.
///
/// \param context The codegen context.
/// \param instruction The instruction to insert.
/// \return The inserted instruction.
Inst *ir_insert(
  CodegenContext *context,
  Inst *inst
)  {
  ASSERT(context->insert_point != NULL, "Can not insert when context has NULL insertion block.");

  /// Handle closed blocks.
  if (ir_is_closed(context->insert_point)) {
    Block *new = alloc_block(context);
    ir_block_attach(context, new);
  }

  /// Append to insert block.
  ir_insert_at_end(context->insert_point, inst);
  return inst;
}

Inst *ir_insert_after(
  Inst *after,
  Inst *instruction
) {
  ASSERT(after->parent_block, "Cannot insert after floating instruction");
  ASSERT(!instruction->parent_block, "Cannot insert instruction that is already inserted");
  Inst **it = vector_find_if(el, after->parent_block->instructions, *el == after);

  ASSERT(it, "Instruction not found in parent block");
  vector_insert(after->parent_block->instructions, it + 1, instruction);
  instruction->parent_block = after->parent_block;
  return instruction;
}

Inst *ir_insert_at_end(
  Block *block,
  Inst *instruction
) {
  ASSERT(!ir_is_closed(block), "Cannot insert instruction into closed block");
  ASSERT(!instruction->parent_block, "Cannot insert instruction that is already inserted");
  return ir_force_insert_at_end(block, instruction);
}

Inst *ir_force_insert_at_end(
  Block *block,
  Inst *instruction
) {
  vector_push(block->instructions, instruction);
  instruction->parent_block = block;
  return instruction;
}


Inst *ir_insert_before(
  Inst *before,
  Inst *instruction
) {
  ASSERT(before->parent_block, "Cannot insert before floating instruction");
  ASSERT(!instruction->parent_block, "Cannot insert instruction that is already inserted");
  Inst **it = vector_find_if(el, before->parent_block->instructions, *el == before);

  ASSERT(it, "Instruction not found in parent block");
  vector_insert(before->parent_block->instructions, it, instruction);
  instruction->parent_block = before->parent_block;
  return instruction;
}

Inst *ir_insert_alloca(CodegenContext *context, Type *type) {
  return ir_insert(context, ir_create_alloca(context, type));
}

Inst *ir_insert_bitcast(CodegenContext *context, Type *to_type, Inst *value) {
  return ir_insert(context, ir_create_bitcast(context, to_type, value));
}

Inst *ir_insert_br(CodegenContext *context, Block *destination) {
  return ir_insert(context, ir_create_br(context, destination));
}

Inst *ir_insert_cond_br(CodegenContext *context, Inst *condition, Block *then_block, Block *else_block) {
  return ir_insert(context, ir_create_cond_br(context, condition, then_block, else_block));
}

Inst *ir_insert_copy(CodegenContext *context, Inst *source) {
  return ir_insert(context, ir_create_copy(context, source));
}

Inst *ir_insert_func_ref(CodegenContext *context, Func *function) {
  return ir_insert(context, ir_create_func_ref(context, function));
}

Inst *ir_insert_immediate(CodegenContext *context, Type *type, u64 immediate) {
  return ir_insert(context, ir_create_immediate(context, type, immediate));
}

Inst *ir_insert_intrinsic(CodegenContext *context, Type *t, enum IntrinsicKind intrinsic) {
  return ir_insert(context, ir_create_intrinsic(context, t, intrinsic));
}

Inst *ir_insert_load(CodegenContext *context, Type *type, Inst *address) {
  return ir_insert(context, ir_create_load(context, type, address));
}

Inst *ir_insert_not(CodegenContext *context, Inst *source) {
  return ir_insert(context, ir_create_not(context, source));
}

Inst *ir_insert_phi(CodegenContext *context, Type *type) {
  return ir_insert(context, ir_create_phi(context, type));
}

Inst *ir_insert_return(CodegenContext *context, Inst *ret) {
  return ir_insert(context, ir_create_return(context, ret));
}

Inst *ir_insert_sext(CodegenContext *context, Type *result_type, Inst *value) {
  return ir_insert(context, ir_create_sext(context, result_type, value));
}

Inst *ir_insert_static_ref(CodegenContext *context, IRStaticVariable *var) {
  return ir_insert(context, ir_create_static_ref(context, var));
}

Inst *ir_insert_store(CodegenContext *context, Inst *data, Inst *address) {
  return ir_insert(context, ir_create_store(context, data, address));
}

Inst *ir_insert_trunc(CodegenContext *context, Type *result_type, Inst *value) {
  return ir_insert(context, ir_create_trunc(context, result_type, value));
}

Inst *ir_insert_zext(CodegenContext *context, Type *result_type, Inst *value) {
  return ir_insert(context, ir_create_zext(context, result_type, value));
}

#define INSERT_BINARY_INSTRUCTION(enumerator, name)                   \
  Inst *ir_insert_##name(CodegenContext *ctx, Inst *lhs, Inst *rhs) { \
    return ir_insert(ctx, ir_create_##name(ctx, lhs, rhs));           \
  }

ALL_BINARY_INSTRUCTION_TYPES(INSERT_BINARY_INSTRUCTION)
#undef INSERT_BINARY_INSTRUCTION

/// ===========================================================================
///  Instruction Data Access
/// ===========================================================================
Block *ir_block_get(Func *function, usz n) {
  ASSERT(ir_func_is_definition(function));
  ASSERT(n < function->blocks.size);
  return function->blocks.data[n];
}

void ir_call_add_arg(Inst *call, Inst *value) {
  ASSERT(call->kind == IR_CALL || call->kind == IR_INTRINSIC);
  vector_push(call->call.arguments, value);
  mark_used(value, call);
}

usz ir_call_args_count(Inst *call) {
  ASSERT(call->kind == IR_CALL || call->kind == IR_INTRINSIC);
  return call->call.arguments.size;
}

Type *ir_call_callee_type(Inst *call) {
  ASSERT(call->kind == IR_CALL);
  if (call->call.is_indirect) {
    return type_is_pointer(call->call.callee_instruction->type)
           ? call->call.callee_instruction->type->pointer.to
           : call->call.callee_instruction->type;
  } else {
    return call->call.callee_function->type;
  }
}

void ir_call_insert_arg(Inst *call, usz n, Inst *value) {
  ASSERT(call->kind == IR_CALL || call->kind == IR_INTRINSIC);
  vector_insert_index(call->call.arguments, n, value);
  mark_used(value, call);
}

bool ir_call_is_direct(Inst *call) {
  ASSERT(call->kind == IR_CALL);
  return !call->call.is_indirect;
}

void ir_call_remove_arg(Inst *call, usz n) {
  ASSERT(call->kind == IR_CALL || call->kind == IR_INTRINSIC);
  ASSERT(n < call->call.arguments.size);
  remove_use(call->call.arguments.data[n], call);
  vector_remove_index(call->call.arguments, n);
}

void ir_call_replace_arg(Inst *call, usz n, Inst *value) {
  ASSERT(call->kind == IR_CALL || call->kind == IR_INTRINSIC);
  remove_use(call->call.arguments.data[n], call);
  call->call.arguments.data[n] = value;
  mark_used(value, call);
}

bool ir_is_closed(Block *block) {
  return block->instructions.size && ir_is_branch(vector_back(block->instructions));
}

Block *ir_entry_block(IRFunction *function) {
  ASSERT(ir_func_is_definition(function), "Cannot get entry block of function declaration");
  return function->blocks.data[0];
}

bool ir_func_is_definition(IRFunction *function) {
  return function->blocks.size > 0;
}

Func *ir_func_ref_func(Inst *func_ref) {
  ASSERT(func_ref->kind == IR_FUNC_REF);
  return func_ref->function_ref;
}

Inst *ir_inst_get(Block *block, usz n) {
  ASSERT(n < block->instructions.size);
  return block->instructions.data[n];
}

bool ir_is_branch(Inst *i) {
  STATIC_ASSERT(IR_COUNT == 40, "Handle all branch types.");
  switch (i->kind) {
    case IR_BRANCH:
    case IR_BRANCH_CONDITIONAL:
    case IR_RETURN:
    case IR_UNREACHABLE:
      return true;
    default:
      return false;
  }
}

IRType ir_kind(Inst *i) {
  return i->kind;
}

span ir_kind_to_str(IRType t) {
  STATIC_ASSERT(IR_COUNT == 40, "Handle all instruction types.");
  switch (t) {
    case IR_IMMEDIATE: return literal_span("imm");
    case IR_LIT_INTEGER: return literal_span("lit.int");
    case IR_LIT_STRING: return literal_span("lit.str");
    case IR_CALL: return literal_span("call");
    case IR_INTRINSIC: return literal_span("intrinsic");
    case IR_STATIC_REF: return literal_span(".ref");
    case IR_FUNC_REF: return literal_span("ref");
    case IR_POISON: return literal_span("poison");

#define PRINT_BINARY_INSTRUCTION(enumerator, name) \
  case IR_##enumerator: return literal_span(#name);
      ALL_BINARY_INSTRUCTION_TYPES(PRINT_BINARY_INSTRUCTION)
#undef PRINT_BINARY_INSTRUCTION

    case IR_NOT: return literal_span("not");
    case IR_ZERO_EXTEND: return literal_span("z.ext");
    case IR_SIGN_EXTEND: return literal_span("s.ext");
    case IR_TRUNCATE: return literal_span("truncate");
    case IR_BITCAST: return literal_span("bitcast");
    case IR_COPY: return literal_span("copy");
    case IR_PARAMETER: return literal_span(".param");
    case IR_RETURN: return literal_span("ret");
    case IR_BRANCH: return literal_span("br");
    case IR_BRANCH_CONDITIONAL: return literal_span("br.cond");
    case IR_PHI: return literal_span("phi");
    case IR_LOAD: return literal_span("load");
    case IR_STORE: return literal_span("store");
    case IR_REGISTER: return literal_span(".reg");
    case IR_ALLOCA: return literal_span("alloca");
    case IR_UNREACHABLE: return literal_span("unreachable");
    default: ICE("Invalid IRType %d\n", t);
  }
}

void ir_phi_add_arg(Inst *phi, Block *from, Inst *value) {
  ASSERT(phi->kind == IR_PHI);

  /// Replace the value if there already is an entry for that block.
  IRPhiArgument *old_arg = vector_find_if(el, phi->phi_args, el->block == from);
  if (old_arg) {
    remove_use(old_arg->value, phi);
    mark_used(value, phi);
    old_arg->value = value;
    return;
  }

  /// Otherwise, add a new entry.
  IRPhiArgument arg = {.block = from, .value = value};
  vector_push(phi->phi_args, arg);
  mark_used(value, phi);
}

const IRPhiArgument *ir_phi_arg(Inst *phi, usz n) {
  ASSERT(phi->kind == IR_PHI);
  return phi->phi_args.data + n;
}

usz ir_phi_args_count(Inst *phi) {
  ASSERT(phi->kind == IR_PHI);
  return phi->phi_args.size;
}

void ir_phi_remove_arg(Inst *phi, Block *block) {
  ASSERT(phi->kind == IR_PHI);

  /// Remove the argument if it exists.
  IRPhiArgument *arg = vector_find_if(el, phi->phi_args, el->block == block);
  if (!arg) return;
  remove_use(arg->value, phi);
  vector_remove_index(phi->phi_args, (usz) (arg - phi->phi_args.data));
}

void ir_set_type(Inst *i, Type *type) {
  i->type = type;
}

IRStaticVariable *ir_static_ref_var(Inst *ref) {
  ASSERT(ref->kind == IR_STATIC_REF);
  return ref->static_ref;
}

span ir_string_data(CodegenContext *ctx, Inst *lit) {
  ASSERT(lit->kind == IR_LIT_STRING);
  return as_span(ctx->ast->strings.data[lit->string_index]);
}

Inst *ir_terminator(Block *block) { return vector_back(block->instructions); }

usz ir_use_count(Inst *i) {
  return i->users.size;
}

Inst *ir_user_get(Inst *inst, usz n) {
  ASSERT(n < inst->users.size);
  return inst->users.data[n];
}

/// ===========================================================================
///  Operations on instructions.
/// ===========================================================================
CodegenContext *ir_context(IRFunction *func) {
  return func->context;
}

void ir_delete_block(IRBlock *block) {
  /// Remove all instructions from the block.
  while (block->instructions.size) {
    Inst* i = vector_back(block->instructions);

    /// Remove this instruction from PHIs that use it.
    foreach_val (user, i->users)
      if (user->kind == IR_PHI)
        ir_phi_remove_arg(user, block);

    /// Remove it from the block.
    ir_remove(i);
  }

  /// Free block name if there is one.
  free_block_data(block);

  /// Actually remove the block from the function.
  if (block->function)
    vector_remove_element(block->function->blocks, block);

  /// Add block to free list if possible or free it otherwise.
  if (block->function) {
    CodegenContext *ctx = block->function->context;
    ASAN_POISON(block, sizeof(Block));
    vector_push(ctx->free_blocks, block);

    /// Clean up free list a bit if its too big.
    clean_free_list(ctx);
  } else {
    free(block);
  }
}

void ir_delete_function(IRFunction *f) {
  CodegenContext *ctx = f->context;

  /// Free each block.
  while (f->blocks.size) {
    Block *b = vector_pop(f->blocks);

    /// Free each instruction.
    while (b->instructions.size) {
      Inst *i = vector_pop(b->instructions);
      if (i->kind == IR_PARAMETER) continue;
      ir_free_instruction_data(i);
      ASAN_POISON(i, sizeof(Inst));
      vector_push(ctx->free_instructions, i);
    }

    /// Free the block name.
    free_block_data(b);
    ASAN_POISON(b, sizeof(Block));
    vector_push(ctx->free_blocks, b);
  }

  /// Free each parameter instruction.
  while (f->parameters.size) {
    Inst *i = vector_pop(f->parameters);
    ir_free_instruction_data(i);
    ASAN_POISON(i, sizeof(Inst));
    vector_push(ctx->free_instructions, i);
  }

  /// Free the name, params, and block list.
  free(f->name.data);
  vector_delete(f->parameters);
  vector_delete(f->blocks);

  /// Free the function itself.
  free(f);

  /// Remove it from the list of functions.
  vector_remove_element(ctx->functions, f);

  /// Clean up free lists.
  clean_free_list(ctx);
}

void ir_force_remove(IRInstruction *instruction) {
  ASSERT(instruction->parent_block);
  if (instruction->parent_block->function) {
    ir_replace(instruction, instruction->parent_block->function->context->poison);
  } else {
    ir_replace(instruction, alloc(NULL, IR_POISON));
  }
}

void ir_make_unreachable(IRBlock *block) {
  if (block->function) {
    foreach_val (b, block->function->blocks) {
      foreach_val (i, b->instructions) {
        if (i->kind != IR_PHI) continue;
        ir_phi_remove_arg(i, block);
      }
    }
    ir_replace(vector_back(block->instructions), ir_create_unreachable(block->function->context));
  } else {
    ir_replace(vector_back(block->instructions), ir_create_unreachable(NULL));
  }
}

void ir_merge_blocks(IRBlock *into, IRBlock *from) {
  ASSERT(into->instructions.size == 0 || !ir_is_branch(vector_back(into->instructions)));
  vector_append(into->instructions, from->instructions);

  /// Update all PHIs in other block that have incoming values
  /// from the `from` block to point to `into` instead.
  if (into->function)
    foreach_val (b, into->function->blocks)
      foreach_val (i, b->instructions)
        if (i->kind == IR_PHI)
          foreach (arg, i->phi_args)
            if (arg->block == from)
              arg->block = into;

  /// Set parent of instructions to the block we’re inserting into and
  /// collect any PHIs that need fixing.
  IRInstructionVector phis_to_replace = {0};
  foreach_val (i, from->instructions) {
    i->parent_block = into;
    if (i->kind == IR_PHI) {
      /// Any PHIs that have an incoming value from the block we’re
      /// inserting into are replaced with that value.
      foreach (arg, i->phi_args) {
        if (arg->block != into) continue;
        ASSERT(!vector_contains(phis_to_replace, i));
        vector_push(phis_to_replace, i);

        /// Remove each other value.
        foreach (arg2, i->phi_args) {
          if (arg2->block == from) continue;
          remove_use(arg2->value, i);
        }

        /// Move the value we’re replacing with into first
        /// position and skip all other arguments.
        i->phi_args.data[0].value = arg->value;
        break;
      }
    }
  }

  /// Remove any PHIs that were marked for removal.
  foreach_val (phi, phis_to_replace)
    ir_replace(phi, phi->phi_args.data[0].value);

  /// Clear `from` block.
  vector_clear(from->instructions);
  vector_delete(phis_to_replace);
}

void ir_print_instruction(
  FILE *file,
  IRInstruction *inst
) {
  string_buffer sb = {0};
  ir_emit_instruction(&sb, inst, false);
  fprint(file, "%S\n", as_span(sb));
  vector_delete(sb);
}

void ir_print_block(
  FILE *file,
  IRBlock *block
) {
  fprint(file, "%33bb%Z%31:\n", block->id);
  foreach_val(i, block->instructions) ir_print_instruction(file, i);
  fprint(file, "%m");
}

void ir_print_function(
  FILE *file,
  IRFunction *function
) {
  ir_print_defun(file, function);
  if (ir_func_is_definition(function)) {
    fprint(file, " %31{\n");
    foreach_val(block, function->blocks) ir_print_block(file, block);
    fprint(file, "%31}");
  }
  fprint(file, "%m\n");
}

void ir_print(
  FILE *file,
  CodegenContext *context
) {
  ir_set_ids(context);
  foreach_val (function, context->functions) {
    if (function_ptr != context->functions.data) fprint(file, "\n");
    ir_print_function(file, function);
  }
  fprint(file, "%m");
}

void ir_print_dot_cfg(CodegenContext *ctx) {
  print_dot_impl(ctx, ir_print_dot_cfg_function);
}

void ir_print_dot_dj(CodegenContext *ctx) {
  print_dot_impl(ctx, ir_print_dot_dj_function);
}

void ir_remove(IRInstruction *i) {
  if (i->parent_block && i->parent_block->function) {
    ir_remove_impl(i->parent_block->function->context, i);
  } else {
    ir_remove_impl(NULL, i);
  }
}

/// Replace an instruction with a new instruction.
///
/// If the new instruction is not inserted anywhere, it will
/// be inserted in the same place as the old instruction. All
/// uses of the old instruction will be replaced with the new
/// one. The old instruction will be deleted.
///
/// Note: This invalidates any iterators that point to the
/// instruction list of the parent block of \c old, iff \c
/// new is already inserted somewhere.
///
/// \param old The instruction to replace and delete.
/// \param new The instruction to replace it with.
/// \return The new instruction.
IRInstruction *ir_replace(IRInstruction *old, IRInstruction *new) {
  ASSERT(old->parent_block);

  /// Insert new instruction if need be.
  if (!new->parent_block) {
    vector_replace_element(old->parent_block->instructions, old, new);
    new->parent_block = old->parent_block;
  }

  /// Replace uses.
  ir_replace_uses(old, new);

  /// Instruction should have no more uses.
  ASSERT(
    old->users.size == 0,
    "Instruction should not be used anymore. Did you mean to use ir_replace_uses() instead?"
  );

  /// Delete it.
  ir_remove_impl(old->parent_block->function->context, old);
  return new;
}

void ir_replace_uses(IRInstruction *instruction, IRInstruction *replacement) {
  if (instruction == replacement) { return; }
#ifdef DEBUG_USES
  eprint("[Use] Replacing uses of %%%u with %%%u\n", instruction->id, replacement->id);
#endif
  while (instruction->users.size) {
    /// Handle the case of an instruction being replaced with an
    /// instruction that uses it.
    usz i = 0;
    foreach_val (user, instruction->users)
      if (user == replacement)
        i++;
    if (i == instruction->users.size) break;

    ir_internal_replace_use_t replace = { instruction, replacement };
    ir_for_each_child(instruction->users.data[i], ir_internal_replace_use, &replace);
  }
}

void ir_set_func_ids(IRFunction *f) {
  /// We start counting at 1 so that 0 can indicate an invalid/removed element.
  usz block_id = 1;
  u32 instruction_id = (u32) f->parameters.size + 1;

  foreach_val (block, f->blocks) {
    block->id = block_id++;
    foreach_val (instruction, block->instructions) {
        if (instruction->kind == IR_PARAMETER || !ir_is_value(instruction)) instruction->id = 0;
        else instruction->id = instruction_id++;
    }
  }
}

void ir_set_ids(CodegenContext *context) {
  usz function_id = 0;

  foreach_val (function, context->functions) {
    function->id = function_id++;
    ir_set_func_ids(function);
  }
}

/// ===========================================================================
///  Accessors
/// ===========================================================================
/// Assert that an instruction is a binary instruction.
static void assert_is_binary(Inst *inst) {
  switch (inst->kind) {
    ALL_BINARY_INSTRUCTION_CASES()
      return;
    default:
      ICE("Expected binary instruction");
  }
}

/// Assert that an instruction can have an operand.
static void assert_has_operand(Inst *inst) {
  switch (inst->kind) {
    case IR_LOAD:
    case IR_RETURN:
    case IR_COPY:
    case IR_ZERO_EXTEND:
    case IR_SIGN_EXTEND:
    case IR_TRUNCATE:
    case IR_BITCAST:
    case IR_NOT:
      return;
    default:
      ICE("Expected instruction with operand");
  }
}

/// Assert that an instruction has an immediate value.
static void assert_has_imm(Inst *inst) {
  switch (inst->kind) {
    case IR_IMMEDIATE:
    case IR_PARAMETER:
    case IR_LIT_INTEGER:
      return;
    default:
      ICE("Expected instruction with immediate");
  }
}

/// Create a call instruction.
NODISCARD static Inst* ir_create_call_impl(CodegenContext *ctx, bool direct, Value val) {
  Inst *call = alloc(ctx, IR_CALL);
  call->call.is_indirect = !direct;
  if (direct) {
    call->call.callee_function = val.func;
    call->type = val.func->type->function.return_type;
  } else {
    call->call.callee_instruction = val.inst;
    call->type = ir_call_callee_type(call)->function.return_type;
    mark_used(val.inst, call);
  }
  return call;
}

Type *ir_typeof_impl_i(Inst *inst) { return inst->type; }
Type *ir_typeof_impl_f(Func *func) { return func->type; }

Inst *ir_create_call_impl_i(CodegenContext * ctx, Inst * i) {
  return ir_create_call_impl(ctx, false, (Value){.inst = i});
}

Inst *ir_create_call_impl_f(CodegenContext * ctx, Func * f) {
  return ir_create_call_impl(ctx, true, (Value){.func = f});
}

#define GET(name, var_name) case FUNC_ATTR_##name: return f->attr_##var_name;
#define SET(name, var_name) case FUNC_ATTR_##name: f->attr_##var_name = value; break;
#define ERR(name, var_name) case FUNC_ATTR_##name: ICE("IR functions do not have a '" #var_name "' attribute");

bool ir_attribute_impl_get(Func *f, enum FunctionAttribute a) {
  switch (a) {
    default: UNREACHABLE();
    SHARED_FUNCTION_ATTRIBUTES(GET)
    IR_FUNCTION_ATTRIBUTES(GET)
    FRONTEND_FUNCTION_ATTRIBUTES(ERR)
  }
}

void ir_attribute_impl_set(Func *f, enum FunctionAttribute a, bool value) {
  switch (a) {
    default: UNREACHABLE();
    SHARED_FUNCTION_ATTRIBUTES(SET)
    IR_FUNCTION_ATTRIBUTES(SET)
    FRONTEND_FUNCTION_ATTRIBUTES(ERR)
  }
}

#undef GET
#undef SET
#undef ERR

usz ir_func_regs_in_use_impl_get(Func *f) { return f->registers_in_use; }
void ir_func_regs_in_use_impl_set(Func *f, usz n) { f->registers_in_use = n; }

span ir_name_b_impl_get(Block *b) { return as_span(b->name); }
void ir_name_b_impl_set(Block *b, string name) {
  if (b->name.data) free(b->name.data);
  b->name = name;
}

span ir_name_f_impl_get(Func *f) { return as_span(f->name); }
void ir_name_f_impl_set(Func *f, string name) {
  if (f->name.data) free(f->name.data);
  f->name = name;
}

Block **ir_blocks_begin_impl(Func *f) { return f->blocks.data; }
Block **ir_blocks_end_impl(Func *f) { return f->blocks.data + f->blocks.size; }
Inst **ir_instructions_begin_impl(Block *b) { return b->instructions.data; }
Inst **ir_instructions_end_impl(Block *b) { return b->instructions.data + b->instructions.size; }
Inst **ir_users_begin_impl(Inst *i) { return i->users.data; }
Inst **ir_users_end_impl(Inst *i) { return i->users.data + i->users.size; }
Block *ir_parent_impl_i(Inst *i) { return i->parent_block; }
Func *ir_parent_impl_b(Block *b) { return b->function; }

Inst **ir_it_impl_i(Inst *i) {
  ASSERT(i->parent_block);
  return vector_find_if(el, i->parent_block->instructions, *el == i);
}

Block **ir_it_impl_b(Block *b) {
  ASSERT(b->function);
  return vector_find_if(el, b->function->blocks, *el == b);
}

SymbolLinkage ir_linkage_impl_f(Func *f) { return f->linkage; }
SymbolLinkage ir_linkage_impl_v(IRStaticVariable *var) { return var->linkage; }
usz ir_count_impl_b(Block *b) { return b->instructions.size; }
usz ir_count_impl_f(Func *f) { return f->blocks.size; }

void ir_debug_iterators_impl(
  const void *begin,
  const void *end,
  const void *expected_begin,
  const void *expected_end
) {
  ASSERT(begin == expected_begin, "Iterator invalidation (begin): Range has moved");
  ASSERT(end == expected_end, "Iterator invalidation (end): Range bounds have changed.");
}

usz ir_alloca_offset_impl_get(Inst *obj) {
  ASSERT(obj->kind == IR_ALLOCA);
  return obj->alloca.offset;
}

void ir_alloca_offset_impl_set(Inst *obj, usz val) {
  ASSERT(obj->kind == IR_ALLOCA);
  obj->alloca.offset = val;
};

usz ir_alloca_size_impl_get(Inst *obj) {
  ASSERT(obj->kind == IR_ALLOCA);
  return obj->alloca.size;
}

void ir_alloca_size_impl_set(Inst *obj, usz val) {
  ASSERT(obj->kind == IR_ALLOCA);
  obj->alloca.size = val;
};

Inst *ir_call_arg_impl_get(Inst *i, usz n) {
  ASSERT(i->kind == IR_CALL || i->kind == IR_INTRINSIC);
  ASSERT(n < i->call.arguments.size);
  return i->call.arguments.data[n];
}

void ir_call_arg_impl_set(Inst *i, usz n, Inst *val) {
  ASSERT(i->kind == IR_CALL || i->kind == IR_INTRINSIC);
  ASSERT(n < i->call.arguments.size);
  remove_use(i->call.arguments.data[n], i);
  i->call.arguments.data[n] = val;
  mark_used(val, i);
}

bool ir_call_force_inline_impl_get(Inst *obj) {
  ASSERT(obj->kind == IR_CALL);
  return obj->call.force_inline;
}

void ir_call_force_inline_impl_set(Inst *obj, bool val) {
  ASSERT(obj->kind == IR_CALL);
  obj->call.force_inline = val;
};

bool ir_call_tail_impl_get(Inst *obj) {
  ASSERT(obj->kind == IR_CALL);
  return obj->call.tail_call;
}

void ir_call_tail_impl_set(Inst *obj, bool val) {
  ASSERT(obj->kind == IR_CALL);
  obj->call.tail_call = val;
};

Value ir_callee_impl_get(Inst *call) {
  ASSERT(call->kind == IR_CALL);
  if (call->call.is_indirect) return (Value){.inst = call->call.callee_instruction};
  else return (Value){.func = call->call.callee_function};
}

void ir_callee_impl_set(Inst *call, Value val, bool direct) {
  ASSERT(call->kind == IR_CALL);
  if (call->call.is_indirect && call->call.callee_instruction)
    remove_use(call->call.callee_instruction, call);

  if (direct) {
    call->call.is_indirect = false;
    call->call.callee_function = val.func;
    call->type = val.func->type->function.return_type;
  } else {
    call->call.is_indirect = true;
    call->call.callee_instruction = val.inst;
    call->type = ir_call_callee_type(call)->function.return_type;
    mark_used(val.inst, call);
  }
}

Inst *ir_cond_impl_get(Inst *i) {
  ASSERT(i->kind == IR_BRANCH_CONDITIONAL);
  return i->cond_br.condition;
}

void ir_cond_impl_set(Inst *i, Inst *val) {
  ASSERT(i->kind == IR_BRANCH_CONDITIONAL);
  remove_use(i->cond_br.condition, i);
  i->cond_br.condition = val;
  mark_used(val, i);
}

Block *ir_dest_impl_get(Inst *obj) {
  ASSERT(obj->kind == IR_BRANCH);
  return obj->destination_block;
}

void ir_dest_impl_set(Inst *obj, Block *val) {
  ASSERT(obj->kind == IR_BRANCH);
  obj->destination_block = val;
};

Block *ir_else_impl_get(Inst *obj) {
  ASSERT(obj->kind == IR_BRANCH_CONDITIONAL);
  return obj->cond_br.else_;
}

void ir_else_impl_set(Inst *obj, Block *val) {
  ASSERT(obj->kind == IR_BRANCH_CONDITIONAL);
  obj->cond_br.else_ = val;
};

usz ir_imm_impl_get(Inst *obj) {
  assert_has_imm(obj);
  return obj->imm;
}

void ir_imm_impl_set(Inst *obj, usz val) {
  assert_has_imm(obj);
  obj->imm = val;
};

enum IntrinsicKind ir_intrinsic_kind_impl_get(IRInstruction *obj) {
  ASSERT(obj->kind == IR_INTRINSIC);
  return obj->call.intrinsic;
}

void ir_intrinsic_kind_impl_set(IRInstruction *obj, enum IntrinsicKind val) {
  ASSERT(obj->kind == IR_INTRINSIC);
  obj->call.intrinsic = val;
};

Inst *ir_lhs_impl_get(Inst *i) {
  assert_is_binary(i);
  return i->lhs;
}

void ir_lhs_impl_set(Inst *i, Inst *val) {
  assert_is_binary(i);
  remove_use(i->lhs, i);
  i->lhs = val;
  mark_used(val, i);
}

Inst *ir_operand_impl_get(Inst *i) {
  assert_has_operand(i);
  return i->operand;
}

void ir_operand_impl_set(Inst *i, Inst *val) {
  assert_has_operand(i);
  if (i->operand) remove_use(i->operand, i);
  i->operand = val;
  mark_used(val, i);
}

Inst *ir_rhs_impl_get(Inst *i) {
  assert_is_binary(i);
  return i->rhs;
}

void ir_rhs_impl_set(Inst *i, Inst *val) {
  assert_is_binary(i);
  remove_use(i->rhs, i);
  i->rhs = val;
  mark_used(val, i);
}

Inst *ir_static_var_init_impl_get(IRStaticVariable *var) {
  return var->init;
}

void ir_static_var_init_impl_set(IRStaticVariable *var, Inst *val) {
  if (var->init) remove_use(var->init, val);
  var->init = val;
  mark_used(val, val);
}

Inst *ir_store_addr_impl_get(Inst *i) {
  ASSERT(i->kind == IR_STORE);
  return i->store.addr;
}

void ir_store_addr_impl_set(Inst *i, Inst *val) {
  ASSERT(i->kind == IR_STORE);
  remove_use(i->store.addr, i);
  i->store.addr = val;
  mark_used(val, i);
}

Inst *ir_store_value_impl_get(Inst *i) {
  ASSERT(i->kind == IR_STORE);
  return i->store.value;
}

void ir_store_value_impl_set(Inst *i, Inst *val) {
  ASSERT(i->kind == IR_STORE);
  remove_use(i->store.value, i);
  i->store.value = val;
  mark_used(val, i);
}

Block *ir_then_impl_get(Inst *obj) {
  ASSERT(obj->kind == IR_BRANCH_CONDITIONAL);
  return obj->cond_br.then;
}

void ir_then_impl_set(Inst *obj, Block *val) {
  ASSERT(obj->kind == IR_BRANCH_CONDITIONAL);
  obj->cond_br.then = val;
};

/// Only to be used for trivial get/set pairs.
#define DEFINE_ACCESSORS(name, obj_type, field_type, field_name)       \
  field_type name##_impl_get(obj_type obj) { return obj->field_name; } \
  void name##_impl_set(obj_type obj, field_type val) { obj->field_name = val; }

DEFINE_ACCESSORS(ir_location_i, Inst *, loc, source_location);
DEFINE_ACCESSORS(ir_location_f, Func *, loc, source_location);
DEFINE_ACCESSORS(ir_mir_i, Inst *, MIRInstruction *, machine_inst);
DEFINE_ACCESSORS(ir_mir_b, Block *, MIRBlock *, machine_block);
DEFINE_ACCESSORS(ir_mir_f, Func *, MIRFunction *, machine_func);
DEFINE_ACCESSORS(ir_register, Inst *, Register, result);
