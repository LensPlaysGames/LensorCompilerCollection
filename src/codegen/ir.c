#include <codegen.h>
#include <codegen/ir.h>
#include <codegen/ir_backend.h>
#include <inttypes.h>
#include <codegen/codegen_platforms.h>

#include <error.h>
#include <stdarg.h>
#include <stdlib.h>
#include <string.h>
#include <vector.h>

/// Insert a value into the current basic block.
void insert(CodegenContext *context, Value *value) {
  ASSERT(context->insert_point, "Cannot insert without insert point");
  // Create a new block if the current one is closed.
  if (context->insert_point->closed) codegen_basic_block_create(context);

  // Insert the value into the current block.
  if (context->insert_point->end) {
    context->insert_point->end->next = value;
    value->prev = context->insert_point->end;
  } else {
    value->prev = NULL;
    context->insert_point->values = value;
  }
  context->insert_point->end = value;
  value->next = NULL;

  // Close the block if the value was a branch.
  if (value->type == IR_INSTRUCTION_BRANCH    ||
      value->type == IR_INSTRUCTION_BRANCH_IF ||
      value->type == IR_INSTRUCTION_RETURN) {
    context->insert_point->closed = 1;
  }

  value->parent = context->insert_point;
}

void mark_used_by(Value *value, Value *parent) {
  Use *use = calloc(1, sizeof(Use));
  use->parent = parent;
  use->next = value->uses;
  value->uses = use;
}

void codegen_comment(CodegenContext *context, const char* fmt, ...) {
  va_list ap;
  va_start(ap, fmt);
  size_t size = vsnprintf(NULL, 0, fmt, ap) + 1;
  va_end(ap);

  char* comment = malloc(size);

  va_start(ap, fmt);
  vsnprintf(comment, size, fmt, ap);
  va_end(ap);

  Value* v = calloc(1, sizeof *v);
  v->type = IR_INSTRUCTION_COMMENT;
  v->comment_value = comment;
  insert(context, v);
}

void codegen_comment_verbose(CodegenContext *context, const char* fmt, ...) {
  /// FIXME(Sirraide): code duplication
  if (codegen_verbose) {
    ASSERT(context->insert_point, "Cannot generate comment outside of function");

    va_list ap;
    va_start(ap, fmt);
    size_t size = vsnprintf(NULL, 0, fmt, ap) + 1;
    va_end(ap);

    char* comment = malloc(size + 1);

    va_start(ap, fmt);
    vsnprintf(comment, size, fmt, ap);
    va_end(ap);
  }
}

Value *codegen_create_external_call(CodegenContext *context, const char *name) {
  ASSERT(name, "Cannot create external call to NULL");

  Value *call = calloc(1, sizeof *call);
  call->type = IR_INSTRUCTION_CALL;
  call->call_value.type = FUNCTION_CALL_TYPE_EXTERNAL;
  call->call_value.external_callee = name;
  insert(context, call);
  return call;
}

Value *codegen_create_internal_call(CodegenContext *context, Value *callee) {
  //ASSERT(callee->type == IR_INSTRUCTION_FUNCTION_REF, "Can only call functions");

  Value *call = calloc(1, sizeof *call);
  call->type = IR_INSTRUCTION_CALL;
  call->call_value.type = FUNCTION_CALL_TYPE_INTERNAL;
  call->call_value.callee = callee;
  insert(context, call);
  return call;
}

void codegen_add_function_arg(CodegenContext *context, Value *call_value, Value *arg_value) {
  (void) context;
  ASSERT(call_value->type == IR_INSTRUCTION_CALL, "Argument 'call' must be a function call");
  FunctionCall *call = &call_value->call_value;
  FunctionCallArg *arg = calloc(1, sizeof *arg);
  arg->value = arg_value;
  if (call->args) {
    FunctionCallArg *func_args = call->args;
    while (func_args->next) { func_args = func_args->next; }
    func_args->next = arg;
  } else {
    call->args = arg;
  }
  mark_used_by(arg_value, call_value);
}

Function *codegen_function_create(CodegenContext *context, const char **name) {
  Function *f = calloc(1, sizeof *f);
  if (!name || !*name) {
    // Generate a symbol.
    size_t id = (*context->func_count)++;
    size_t size = snprintf(NULL, 0, "__function_%zu", id) + 1;
    char *buf = malloc(size);
    snprintf(buf, size, "__function_%zu", id);
    f->name = buf;
    if (name) *name = buf;
  } else {
    f->name = strdup(*name);
  }

  if (context->functions) {
    Function *func = context->functions;
    while (func->next) { func = func->next; }
    func->next = f;
  } else {
    context->functions = f;
  }

  f->return_block = codegen_basic_block_create_detached(context);

  context->current_function = f;
  codegen_basic_block_create(context);

  return f;
}

Value *codegen_function_ref(CodegenContext *context, Function *function) {
  (void) context;
  Value *ref = calloc(1, sizeof *ref);
  ref->type = IR_INSTRUCTION_FUNCTION_REF;
  ref->function_ref = function;
  return ref;
}

/// Create a basic block without attaching it.
BasicBlock *codegen_basic_block_create_detached(CodegenContext *context) {
  BasicBlock *bb = calloc(1, sizeof *bb);
  bb->id = (*context->block_count)++;
  return bb;
}

/// Create a basic block and attach it to the current function
BasicBlock * codegen_basic_block_create(CodegenContext *context) {
  BasicBlock *bb = codegen_basic_block_create_detached(context);
  codegen_basic_block_attach(context, bb);
  return bb;
}

/// Attach a block to the current function
void codegen_basic_block_attach_to(CodegenContext *context, BasicBlock* block, Function *f) {
  if (!f->last) {
    f->entry = block;
    f->last = block;
    block->prev = NULL;
  } else {
    // Create a dummy branch to the next block in case we want to reorder the blocks later.
    BasicBlock *last = f->last;
    if (!last->closed) {
      BasicBlock *insert_point = context->insert_point;
      context->insert_point = last;
      codegen_branch(context, block);
      context->insert_point = insert_point;
    }

    last->next = block;
    block->prev = last;
    f->last = block;
  }

  context->insert_point = block;
  block->parent = f;
}

/// Attach a block to the current function
void codegen_basic_block_attach(CodegenContext *context, BasicBlock* block) {
  ASSERT(context->current_function, "Cannot attach block if there is no function");
  codegen_basic_block_attach_to(context, block, context->current_function);
}

Value *codegen_load_global_address(CodegenContext *context, const char *name) {
  ASSERT(name, "Name may not be NULL");
  Value *global = calloc(1, sizeof *global);
  global->type = IR_INSTRUCTION_GLOBAL_REF;
  global->global_name = name;
  insert(context, global);
  return global;
}

Value *codegen_load_local_address(CodegenContext *context, Value *address) {
  Value *local = calloc(1, sizeof *local);
  local->type = IR_INSTRUCTION_LOCAL_REF;
  local->local_ref = address;
  mark_used_by(address, local);
  insert(context, local);
  return local;
}

Value *codegen_add(CodegenContext *context, Value *lhs, Value *rhs) {
  /// TODO: trivial constant folding.
  Value *sum = calloc(1, sizeof *sum);
  sum->type = IR_INSTRUCTION_ADD;
  sum->lhs = lhs;
  sum->rhs = rhs;
  mark_used_by(lhs, sum);
  mark_used_by(rhs, sum);
  insert(context, sum);
  return sum;
}

Value *codegen_subtract(CodegenContext *context, Value *lhs, Value *rhs) {
  /// TODO: trivial constant folding.
  Value *sum = calloc(1, sizeof *sum);
  sum->type = IR_INSTRUCTION_SUB;
  sum->lhs = lhs;
  sum->rhs = rhs;
  mark_used_by(lhs, sum);
  mark_used_by(rhs, sum);
  insert(context, sum);
  return sum;
}

Value *codegen_multiply(CodegenContext *context, Value *lhs, Value *rhs) {
  /// TODO: trivial constant folding.
  Value *sum = calloc(1, sizeof *sum);
  sum->type = IR_INSTRUCTION_MUL;
  sum->lhs = lhs;
  sum->rhs = rhs;
  mark_used_by(lhs, sum);
  mark_used_by(rhs, sum);
  insert(context, sum);
  return sum;
}

Value *codegen_divide(CodegenContext *context, Value *lhs, Value *rhs) {
  /// TODO: trivial constant folding.
  Value *sum = calloc(1, sizeof *sum);
  sum->type = IR_INSTRUCTION_DIV;
  sum->lhs = lhs;
  sum->rhs = rhs;
  mark_used_by(lhs, sum);
  mark_used_by(rhs, sum);
  insert(context, sum);
  return sum;
}

Value *codegen_modulo(CodegenContext *context, Value *lhs, Value *rhs) {
  /// TODO: trivial constant folding.
  Value *sum = calloc(1, sizeof *sum);
  sum->type = IR_INSTRUCTION_MOD;
  sum->lhs = lhs;
  sum->rhs = rhs;
  mark_used_by(lhs, sum);
  mark_used_by(rhs, sum);
  insert(context, sum);
  return sum;
}

Value *codegen_shift_left(CodegenContext *context, Value *lhs, Value *rhs) {
  /// TODO: trivial constant folding.
  Value *sum = calloc(1, sizeof *sum);
  sum->type = IR_INSTRUCTION_SHL;
  sum->lhs = lhs;
  sum->rhs = rhs;
  mark_used_by(lhs, sum);
  mark_used_by(rhs, sum);
  insert(context, sum);
  return sum;
}

Value *codegen_shift_right_arithmetic(CodegenContext *context, Value *lhs, Value *rhs) {
  /// TODO: trivial constant folding.
  Value *sum = calloc(1, sizeof *sum);
  sum->type = IR_INSTRUCTION_SAR;
  sum->lhs = lhs;
  sum->rhs = rhs;
  mark_used_by(lhs, sum);
  mark_used_by(rhs, sum);
  insert(context, sum);
  return sum;
}

Value *codegen_comparison(CodegenContext *context, enum ComparisonType type, Value *lhs, Value* rhs) {
  Value *cmp = calloc(1, sizeof *cmp);
  cmp->type = IR_INSTRUCTION_COMPARISON;
  cmp->comparison.type = type;
  cmp->comparison.lhs = lhs;
  cmp->comparison.rhs = rhs;
  mark_used_by(lhs, cmp);
  mark_used_by(rhs, cmp);
  insert(context, cmp);
  return cmp;
}

void codegen_branch_if(CodegenContext *context, Value *condition, BasicBlock *true_block, BasicBlock *false_block) {
  ASSERT(true_block, "True block may not be NULL");
  ASSERT(false_block, "False block may not be NULL");
  Value *br = calloc(1, sizeof *br);
  br->type = IR_INSTRUCTION_BRANCH_IF;
  br->cond_branch_value.condition = condition;
  br->cond_branch_value.true_branch = true_block;
  br->cond_branch_value.false_branch = false_block;
  mark_used_by(condition, br);

  BasicBlockPredecessor *pred_true = calloc(1, sizeof *pred_true);
  BasicBlockPredecessor *pred_false = calloc(1, sizeof *pred_false);
  pred_true->block = context->insert_point;
  pred_false->block = context->insert_point;
  pred_true->next = true_block->preds;
  pred_false->next = false_block->preds;
  true_block->preds = pred_true;
  false_block->preds = pred_false;

  insert(context, br);
}

void codegen_branch(CodegenContext *context, BasicBlock *block) {
  ASSERT(block, "Cannot branch to NULL block");
  Value *br = calloc(1, sizeof *br);
  br->type = IR_INSTRUCTION_BRANCH;
  br->branch_target = block;

  BasicBlockPredecessor *pred = calloc(1, sizeof *pred);
  pred->block = context->insert_point;
  pred->next = block->preds;
  block->preds = pred;

  insert(context, br);
}

Value *codegen_load_immediate(CodegenContext *context, int64_t immediate) {
  Value *imm = calloc(1, sizeof *imm);
  imm->type = IR_INSTRUCTION_IMMEDIATE;
  imm->immediate = immediate;
  insert(context, imm);
  return imm;
}

Value *codegen_phi_create(CodegenContext *context) {
  Value *phi = calloc(1, sizeof *phi);
  phi->type = IR_INSTRUCTION_PHI;
  insert(context, phi);
  return phi;
}

///  Add a value to a phi node.
void codegen_phi_add(CodegenContext *context, Value *phi, BasicBlock *block, Value *value) {
  (void) context;
  ASSERT(block, "Block may not be NULL");
  ASSERT(value, "Value may not be NULL");
  PHINodeEntry *entry = calloc(1, sizeof *entry);

  if (phi->phi_entries) {
    PHINodeEntry *e = phi->phi_entries;
    while (e->next) e = e->next;
    e->next = entry;
  } else {
    phi->phi_entries = entry;
  }

  entry->block = block;
  entry->value = value;
  mark_used_by(value, phi);
}

Value *codegen_alloca(CodegenContext *context, uint64_t size) {
  Value *alloc = calloc(1, sizeof *alloc);
  alloc->type = IR_INSTRUCTION_ALLOCA;
  alloc->immediate = size;
  insert(context, alloc);
  return alloc;
}

Value *codegen_load_global(CodegenContext *context, const char * name) {
  ASSERT(name, "Name may not be NULL");
  Value *global = calloc(1, sizeof *global);
  global->type = IR_INSTRUCTION_GLOBAL_VAL;
  global->global_name = name;
  insert(context, global);
  return global;
}

Value *codegen_load_local(CodegenContext *context, Value *val) {
  Value *local = calloc(1, sizeof *local);
  local->type = IR_INSTRUCTION_LOCAL_VAL;
  local->local_ref = val;
  mark_used_by(val, local);
  insert(context, local);
  return local;
}

void codegen_store_global(CodegenContext *context, Value *val, const char *name) {
  ASSERT(name, "Name may not be NULL");
  Value *store = calloc(1, sizeof *store);
  store->type = IR_INSTRUCTION_STORE_GLOBAL;
  store->global_store.name = name;
  store->global_store.value = val;
  mark_used_by(val, store);
  insert(context, store);
}

void codegen_store_local(CodegenContext *context, Value *src, Value *dest) {
  Value *store = calloc(1, sizeof *store);
  store->type = IR_INSTRUCTION_STORE_LOCAL;
  store->lhs = src;
  store->rhs = dest;
  mark_used_by(src, store);
  mark_used_by(dest, store);
  insert(context, store);
}

void codegen_store(CodegenContext *context, Value *src, Value *dest) {
  Value *store = calloc(1, sizeof *store);
  store->type = IR_INSTRUCTION_STORE;
  store->lhs = src;
  store->rhs = dest;
  mark_used_by(src, store);
  mark_used_by(dest, store);
  insert(context, store);
}

Value *codegen_bind_function_parameter(CodegenContext *context, Function* f, size_t index) {
  Value *param = calloc(1, sizeof *param);
  param->type = IR_INSTRUCTION_PARAM_REF;
  param->param_ref.func = f;
  param->param_ref.index = index;
  insert(context, param);
  return param;
}

void codegen_set_return_value(CodegenContext *context, Function* f, Value *value) {
  // The return value is a phi node to allow for early returns.
  if (!f->return_value) {
    Value *phi = calloc(1, sizeof *phi);
    phi->type = IR_INSTRUCTION_PHI;
    f->return_value = phi;
  }
  codegen_phi_add(context, f->return_value, context->insert_point, value);
}

void codegen_return(CodegenContext *context) {
  codegen_branch(context, context->insert_point->parent->return_block);
}

Value *create_copy(CodegenContext *context, Value *v) {
  (void) context;
  Value *copy = calloc(1, sizeof *copy);
  copy->type = IR_INSTRUCTION_COPY;
  copy->operand = v;
  mark_used_by(v, copy);
  return copy;
}

/// Returns 1 for convenience.
static int irprintf(const char *fmt, ...) {
  va_list ap;
  va_start(ap, fmt);

  for (;;) {
    const char *start = fmt;
    fmt = strpbrk(fmt, "%");
    if (!fmt) {
      printf("%s", start);
      return 1;
    }
    printf("%.*s", (int)(fmt - start), start);

    switch (*++fmt) {
      case 0: PANIC("Invalid format string");
      case '%':
        printf("%%");
        break;
      case 's':
        printf("%s", va_arg(ap, const char *));
        break;
      case 'b': {
        BasicBlock *block = va_arg(ap, BasicBlock *);
        if (block) codegen_dump_basic_block(block);
        else printf("(null)");
      } break;
      case 'v': {
        Value *v = va_arg(ap, Value *);
        if (v) codegen_dump_value(v);
        else printf("(null)");
      } break;
      case 'f': {
        Function *f = va_arg(ap, Function *);
        if (f) printf("[%s()]", f->name);
        else printf("(null)");
      } break;
    }
    fmt++;
  }

  va_end(ap);
  return 1;
}

void typecheck_ir(CodegenContext *context, Function *f) {
  (void) context;
  int err = 0;

  // Each block must end with a branch, have a parent, and have a start and an end.
  LIST_FOREACH (block, f->entry) {
    if (block->end->type != IR_INSTRUCTION_RETURN &&
        block->end->type != IR_INSTRUCTION_BRANCH &&
        block->end->type != IR_INSTRUCTION_BRANCH_IF) {
      err = irprintf("Error: Block does not end with a branch:\n%b", block);
    } else if (block->parent != f) {
      err = irprintf("Error: Block in %f has wrong parent %f:\n%b", f, block->parent, block);
    } else if (!block->values) {
      err = irprintf("Error: Block is empty:\n%b", block);
    } else if (!block->end) {
      err = irprintf("Error: Block has no end:\n%b", block);
    }
  }

  if (err) exit(1);
}

void codegen_function_finalise(CodegenContext *context, Function *f) {
  BasicBlock *insert_point = context->insert_point;

  // Attach the return block.
  codegen_basic_block_attach_to(context, f->return_block, f);
  context->insert_point = f->return_block;
  insert(context, f->return_value);

  Value *ret = calloc(1, sizeof *ret);
  ret->type = IR_INSTRUCTION_RETURN;
  insert(context, ret);

  context->insert_point = insert_point;

  typecheck_ir(context, f);
}

/// Very primitive IR printer.
void codegen_dump_value(Value *val) {
  STATIC_ASSERT(IR_INSTRUCTION_COUNT == 27, "Update this switch statement");
  ASSERT(val->type >= 0 && val->type < IR_INSTRUCTION_COUNT, "Invalid value type");
  static const char* cmp_op[] = { "eq", "ne", "lt", "le", "gt", "ge" };

  switch (val->type) {
    case IR_INSTRUCTION_CALL:
      if (val->call_value.type == FUNCTION_CALL_TYPE_EXTERNAL) {
        printf("    %%r%u = call %s (", val->reg, val->call_value.external_callee);
      } else {
        printf("    %%r%u = call %%r%u (", val->reg, val->call_value.callee->reg);
      }
      LIST_FOREACH (arg,  val->call_value.args) {
        printf("%%r%u", arg->value->reg);
        if (arg->next) printf (", ");
      }
      printf(")");
      break;
    case IR_INSTRUCTION_COMMENT:
      printf("    ;; %s", val->comment_value);
      break;
    case IR_INSTRUCTION_BRANCH:
      printf("    branch to bb%u", val->branch_target->id);
      break;
    case IR_INSTRUCTION_BRANCH_IF:
      printf("    branch on %%r%u to bb%u else bb%u",
        val->cond_branch_value.condition->reg,
        val->cond_branch_value.true_branch->id,
        val->cond_branch_value.false_branch->id);
      break;
    case IR_INSTRUCTION_RETURN:
      if (val->operand) codegen_dump_value(val->operand);
      printf("    return");
      break;
    case IR_INSTRUCTION_FUNCTION_REF:
      printf("    %%r%u = %s", val->reg, val->function_ref->name);
      break;
    case IR_INSTRUCTION_ADD:
      printf("    %%r%u = add %%r%u, %%r%u", val->reg, val->lhs->reg, val->rhs->reg);
      break;
    case IR_INSTRUCTION_SUB:
      printf("    %%r%u = sub %%r%u, %%r%u", val->reg, val->lhs->reg, val->rhs->reg);
      break;
    case IR_INSTRUCTION_MUL:
      printf("    %%r%u = mul %%r%u, %%r%u", val->reg, val->lhs->reg, val->rhs->reg);
      break;
    case IR_INSTRUCTION_DIV:
      printf("    %%r%u = div %%r%u, %%r%u", val->reg, val->lhs->reg, val->rhs->reg);
      break;
    case IR_INSTRUCTION_MOD:
      printf("    %%r%u = mod %%r%u, %%r%u", val->reg, val->lhs->reg, val->rhs->reg);
      break;
    case IR_INSTRUCTION_SHL:
      printf("    %%r%u = shl %%r%u, %%r%u", val->reg, val->lhs->reg, val->rhs->reg);
      break;
    case IR_INSTRUCTION_SAR:
      printf("    %%r%u = sar %%r%u, %%r%u", val->reg, val->lhs->reg, val->rhs->reg);
      break;
    case IR_INSTRUCTION_COMPARISON:
      printf("    %%r%u = cmp %s %%r%u, %%r%u", val->reg, cmp_op[val->comparison.type],
             val->comparison.lhs->reg, val->comparison.rhs->reg);
      break;
    case IR_INSTRUCTION_ALLOCA:
      printf("    %%r%u = alloca %" PRId64, val->reg, val->immediate);
      break;
    case IR_INSTRUCTION_IMMEDIATE:
      printf("    %%r%u = immediate %" PRId64, val->reg, val->immediate);
      break;
    case IR_INSTRUCTION_PHI:
      printf("    %%r%u = phi ", val->reg);
      LIST_FOREACH (e, val->phi_entries) {
        printf("[bb%u, %%r%u]", e->block->id, e->value->reg);
        if (e->next) printf(", ");
      }
      break;
    case IR_INSTRUCTION_GLOBAL_REF:
      printf("    %%r%u = global address %s", val->reg, val->global_name);
      break;
    case IR_INSTRUCTION_GLOBAL_VAL:
      printf("    %%r%u = global load %s", val->reg, val->global_name);
      break;
    case IR_INSTRUCTION_STORE_GLOBAL:
      printf("    global store %%r%u to %s", val->global_store.value->reg, val->global_store.name);
      break;
    case IR_INSTRUCTION_LOCAL_REF:
      printf("    %%r%u = local address %%r%u", val->reg, val->local_ref->reg);
      break;
    case IR_INSTRUCTION_LOCAL_VAL:
      printf("    %%r%u = local load %%r%u", val->reg, val->local_ref->reg);
      break;
    case IR_INSTRUCTION_STORE_LOCAL:
      printf("    local store %%r%u to %%r%u", val->lhs->reg, val->rhs->reg);
      break;
    case IR_INSTRUCTION_STORE:
      printf("    store %%r%u to [%%r%u]", val->lhs->reg, val->rhs->reg);
      break;
    case IR_INSTRUCTION_PARAM_REF:
      printf("    %%r%u = param %u", val->reg, val->param_ref.index);
      break;
    case IR_INSTRUCTION_COPY:
      printf("    %%r%u = copy %%r%u", val->reg, val->operand->reg);
      break;
    case IR_INSTRUCTION_REGISTER: break;
    case IR_INSTRUCTION_COUNT: UNREACHABLE();
  }
}

void codegen_dump_basic_block(BasicBlock *bb) {
  printf("bb%u:\n", bb->id);
  LIST_FOREACH (val, bb->values) {
    codegen_dump_value(val);
    printf("\n");
  }
}

void codegen_dump_function(Function *f) {
  printf("defun %s:\n", f->name);
  LIST_FOREACH (bb, f->entry) { codegen_dump_basic_block(bb); }
}

void codegen_dump_ir(CodegenContext *context) {
  LIST_FOREACH (f, context->functions) {codegen_dump_function(f); }
}

void insert_before(Value* value, Value *value_to_insert) {
  if (value->prev) { value->prev->next = value_to_insert; }
  value_to_insert->prev = value->prev;
  value_to_insert->next = value;
  value->prev = value_to_insert;

  value_to_insert->parent = value->parent;
  if (value->parent->values == value) { value->parent->values = value_to_insert; }
}

void insert_after(Value *value, Value *value_to_insert) {
  if (value->next) { value->next->prev = value_to_insert; }
  value_to_insert->next = value->next;
  value_to_insert->prev = value;
  value->next = value_to_insert;

  value_to_insert->parent = value->parent;
  if (value->parent->end == value) { value->parent->end = value_to_insert; }
}

void delete_from_block(Value *v) {
  ASSERT(v->parent->values != v || v->parent->end != v, "Cannot delete last value from block");

  if (v->prev) { v->prev->next = v->next; }
  if (v->next) { v->next->prev = v->prev; }
  if (v->parent->values == v) { v->parent->values = v->next; }
  if (v->parent->end == v) { v->parent->end = v->prev; }
  if (v->uses) LIST_DELETE(v->uses);

  v->parent = NULL;
  v->next = NULL;
  v->prev = NULL;
  v->uses = NULL;
}

void delete_value(Value *v) {
  if (v->parent) delete_from_block(v);
  free(v);
}
