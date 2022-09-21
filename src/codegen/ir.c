#include <codegen.h>
#include <codegen/ir.h>
#include <codegen/codegen_platforms.h>

#include <error.h>
#include <stdarg.h>
#include <stdlib.h>
#include <string.h>

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

  context->current_function = f;
  codegen_basic_block_create(context);

  return f;
}

Value *codegen_function_ref(CodegenContext *context, Function *function) {
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
void codegen_basic_block_attach(CodegenContext *context, BasicBlock* block) {
  ASSERT(context->current_function, "Cannot attach block if there is no function");

  if (!context->current_function->last) {
    context->current_function->entry = block;
    context->current_function->last = block;
    block->prev = NULL;
  } else {
    // Create a dummy branch to the next block in case we want to reorder the blocks later.
    BasicBlock *last = context->current_function->last;
    if (!last->closed) { codegen_branch(context, block); }

    last->next = block;
    block->prev = last;
    context->current_function->last = block;
  }

  context->insert_point = block;
  block->parent = context->current_function;
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
  insert(context, local);
  return local;
}

Value *codegen_add(CodegenContext *context, Value *lhs, Value *rhs) {
  /// TODO: trivial constant folding.
  Value *sum = calloc(1, sizeof *sum);
  sum->type = IR_INSTRUCTION_ADD;
  sum->lhs = lhs;
  sum->rhs = rhs;
  insert(context, sum);
  return sum;
}

Value *codegen_subtract(CodegenContext *context, Value *lhs, Value *rhs) {
  /// TODO: trivial constant folding.
  Value *sum = calloc(1, sizeof *sum);
  sum->type = IR_INSTRUCTION_SUB;
  sum->lhs = lhs;
  sum->rhs = rhs;
  insert(context, sum);
  return sum;
}

Value *codegen_multiply(CodegenContext *context, Value *lhs, Value *rhs) {
  /// TODO: trivial constant folding.
  Value *sum = calloc(1, sizeof *sum);
  sum->type = IR_INSTRUCTION_MUL;
  sum->lhs = lhs;
  sum->rhs = rhs;
  insert(context, sum);
  return sum;
}

Value *codegen_divide(CodegenContext *context, Value *lhs, Value *rhs) {
  /// TODO: trivial constant folding.
  Value *sum = calloc(1, sizeof *sum);
  sum->type = IR_INSTRUCTION_DIV;
  sum->lhs = lhs;
  sum->rhs = rhs;
  insert(context, sum);
  return sum;
}

Value *codegen_modulo(CodegenContext *context, Value *lhs, Value *rhs) {
  /// TODO: trivial constant folding.
  Value *sum = calloc(1, sizeof *sum);
  sum->type = IR_INSTRUCTION_MOD;
  sum->lhs = lhs;
  sum->rhs = rhs;
  insert(context, sum);
  return sum;
}

Value *codegen_shift_left(CodegenContext *context, Value *lhs, Value *rhs) {
  /// TODO: trivial constant folding.
  Value *sum = calloc(1, sizeof *sum);
  sum->type = IR_INSTRUCTION_SHL;
  sum->lhs = lhs;
  sum->rhs = rhs;
  insert(context, sum);
  return sum;
}

Value *codegen_shift_right_arithmetic(CodegenContext *context, Value *lhs, Value *rhs) {
  /// TODO: trivial constant folding.
  Value *sum = calloc(1, sizeof *sum);
  sum->type = IR_INSTRUCTION_SAR;
  sum->lhs = lhs;
  sum->rhs = rhs;
  insert(context, sum);
  return sum;
}

Value *codegen_comparison(CodegenContext *context, enum ComparisonType type, Value *lhs, Value* rhs) {
  Value *cmp = calloc(1, sizeof *cmp);
  cmp->type = IR_INSTRUCTION_COMPARISON;
  cmp->comparison.type = type;
  cmp->comparison.lhs = lhs;
  cmp->comparison.rhs = rhs;
  insert(context, cmp);
  return cmp;
}

void codegen_branch_if(CodegenContext *context, Value *value, BasicBlock *true_block, BasicBlock *false_block) {
  ASSERT(true_block, "True block may not be NULL");
  ASSERT(false_block, "False block may not be NULL");
  Value *br = calloc(1, sizeof *br);
  br->type = IR_INSTRUCTION_BRANCH_IF;
  br->cond_branch_value.condition = value;
  br->cond_branch_value.true_branch = true_block;
  br->cond_branch_value.false_branch = false_block;

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

Value *codegen_load_immediate(CodegenContext *context, long long int immediate) {
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
}

Value *codegen_alloca(CodegenContext *context, long long int size) {
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
  insert(context, local);
  return local;
}

void codegen_store_global(CodegenContext *context, Value *val, const char *name) {
  ASSERT(name, "Name may not be NULL");
  Value *store = calloc(1, sizeof *store);
  store->type = IR_INSTRUCTION_STORE_GLOBAL;
  store->global_store.name = name;
  store->global_store.value = val;
  insert(context, store);
}

void codegen_store_local(CodegenContext *context, Value *src, Value *dest) {
  Value *store = calloc(1, sizeof *store);
  store->type = IR_INSTRUCTION_STORE_LOCAL;
  store->lhs = src;
  store->rhs = dest;
  insert(context, store);
}

void codegen_store(CodegenContext *context, Value *src, Value *dest) {
  Value *store = calloc(1, sizeof *store);
  store->type = IR_INSTRUCTION_STORE;
  store->lhs = src;
  store->rhs = dest;
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
  // TODO(Sirraide): Should be a PHI node to support early return.
  f->return_value = value;
}

void codegen_return(CodegenContext *context) {
  Value *ret = calloc(1, sizeof *ret);
  ret->type = IR_INSTRUCTION_RETURN;
  insert(context, ret);
}

/// Very primitive IR printer.
void codegen_dump_value(CodegenContext *context, Value *val) {
  ASSERT(val->type > 0 && val->type < IR_INSTRUCTION_COUNT, "Invalid value type");
  switch (val->type) {
    case IR_INSTRUCTION_CALL:
      if (val->call_value.type == FUNCTION_CALL_TYPE_EXTERNAL) {
        printf("    %%r%zu = call %s (", val->virt_reg, val->call_value.external_callee);
      } else {
        printf("    %%r%zu = call %%r%zu (", val->virt_reg, val->call_value.callee->virt_reg);
      }
      for (FunctionCallArg *arg = val->call_value.args; arg; arg = arg->next) {
        printf("%%r%zu", arg->value->virt_reg);
        if (arg->next) printf (", ");
      }
      printf(")");
      break;
    case IR_INSTRUCTION_COMMENT:
      printf("    ;; %s", val->comment_value);
      break;
    case IR_INSTRUCTION_BRANCH:
      printf("    branch to bb%zu", val->branch_target->id);
      break;
    case IR_INSTRUCTION_BRANCH_IF:
      printf("    branch on %%r%zu to bb%zu else bb%zu",
        val->cond_branch_value.condition->virt_reg,
        val->cond_branch_value.true_branch->id,
        val->cond_branch_value.false_branch->id);
      break;
    case IR_INSTRUCTION_RETURN:
      printf("    return");
      break;
    case IR_INSTRUCTION_FUNCTION_REF:
      printf("    %%r%zu = %s", val->virt_reg, val->function_ref->name);
      break;
    case IR_INSTRUCTION_ADD:
      printf("    %%r%zu = add %%r%zu, %%r%zu", val->virt_reg, val->lhs->virt_reg, val->rhs->virt_reg);
      break;
    case IR_INSTRUCTION_SUB:
      printf("    %%r%zu = sub %%r%zu, %%r%zu", val->virt_reg, val->lhs->virt_reg, val->rhs->virt_reg);
      break;
    case IR_INSTRUCTION_MUL:
      printf("    %%r%zu = mul %%r%zu, %%r%zu", val->virt_reg, val->lhs->virt_reg, val->rhs->virt_reg);
      break;
    case IR_INSTRUCTION_DIV:
      printf("    %%r%zu = div %%r%zu, %%r%zu", val->virt_reg, val->lhs->virt_reg, val->rhs->virt_reg);
      break;
    case IR_INSTRUCTION_MOD:
      printf("    %%r%zu = mod %%r%zu, %%r%zu", val->virt_reg, val->lhs->virt_reg, val->rhs->virt_reg);
      break;
    case IR_INSTRUCTION_SHL:
      printf("    %%r%zu = shl %%r%zu, %%r%zu", val->virt_reg, val->lhs->virt_reg, val->rhs->virt_reg);
      break;
    case IR_INSTRUCTION_SAR:
      printf("    %%r%zu = sar %%r%zu, %%r%zu", val->virt_reg, val->lhs->virt_reg, val->rhs->virt_reg);
      break;
    case IR_INSTRUCTION_COMPARISON: {
      static const char* op[] = { "eq", "ne", "lt", "le", "gt", "ge" };
      printf("    %%r%zu = cmp %s %%r%zu, %%r%zu", val->virt_reg, op[val->comparison.type],
             val->comparison.lhs->virt_reg, val->comparison.rhs->virt_reg);
    } break;
    case IR_INSTRUCTION_ALLOCA:
      printf("    %%r%zu = alloca %llu", val->virt_reg, val->immediate);
      break;
    case IR_INSTRUCTION_IMMEDIATE:
      printf("    %%r%zu = immediate %llu", val->virt_reg, val->immediate);
      break;
    case IR_INSTRUCTION_PHI:
      printf("    %%r%zu = phi ", val->virt_reg);
      for (PHINodeEntry *e = val->phi_entries; e; e = e->next) {
        printf("[bb%zu, %%r%zu]", e->block->id, e->value->virt_reg);
        if (e->next) printf(", ");
      }
      printf("");
      break;
    case IR_INSTRUCTION_GLOBAL_REF:
      printf("    %%r%zu = global address %s", val->virt_reg, val->global_name);
      break;
    case IR_INSTRUCTION_GLOBAL_VAL:
      printf("    %%r%zu = global load %s", val->virt_reg, val->global_name);
      break;
    case IR_INSTRUCTION_STORE_GLOBAL:
      printf("    global store %%r%zu to %s", val->global_store.value->virt_reg, val->global_store.name);
      break;
    case IR_INSTRUCTION_LOCAL_REF:
      printf("    %%r%zu = local address %%r%zu", val->virt_reg, val->local_ref->virt_reg);
      break;
    case IR_INSTRUCTION_LOCAL_VAL:
      printf("    %%r%zu = local load %%r%zu", val->virt_reg, val->local_ref->virt_reg);
      break;
    case IR_INSTRUCTION_STORE_LOCAL:
      printf("    local store %%r%zu to %%r%zu", val->lhs->virt_reg, val->rhs->virt_reg);
      break;
    case IR_INSTRUCTION_STORE:
      printf("    store %%r%zu to [%%r%zu]", val->lhs->virt_reg, val->rhs->virt_reg);
      break;
    case IR_INSTRUCTION_PARAM_REF:
      printf("    %%r%zu = param %zu", val->virt_reg, val->param_ref.index);
      break;
    case IR_INSTRUCTION_COPY:
      printf("    %%r%zu = copy %%r%zu", val->virt_reg, val->operand->virt_reg);
      break;
    case IR_INSTRUCTION_COPY_REGISTER:
      printf("    %%r%zu = copy %%r%u", val->virt_reg, val->reg);
      break;
    case IR_INSTRUCTION_SPILL:
      printf("    spill");
      break;
    case IR_INSTRUCTION_UNSPILL:
      printf("    unspill");
      break;
    case IR_INSTRUCTION_ADD_TWO_ADDRESS:
      printf("    add %%r%zu, %%r%zu", val->lhs->virt_reg, val->rhs->virt_reg);
      break;
    case IR_INSTRUCTION_SUB_TWO_ADDRESS:
      printf("    sub %%r%zu, %%r%zu", val->lhs->virt_reg, val->rhs->virt_reg);
      break;
    case IR_INSTRUCTION_MUL_TWO_ADDRESS:
      printf("    mul %%r%zu, %%r%zu", val->lhs->virt_reg, val->rhs->virt_reg);
      break;
    case IR_INSTRUCTION_DIV_ONE_ADDRESS:
      printf("    div %%r%zu, %%r%u", val->left->virt_reg, val->right);
      break;
    case IR_INSTRUCTION_SHL_TWO_ADDRESS:
      printf("    shl %%r%zu, %%r%zu", val->lhs->virt_reg, val->rhs->virt_reg);
      break;
    case IR_INSTRUCTION_SAR_TWO_ADDRESS:
      printf("    sar %%r%zu, %%r%zu", val->lhs->virt_reg, val->rhs->virt_reg);
      break;
    case IR_INSTRUCTION_COUNT: UNREACHABLE();
  }
}

void codegen_dump_basic_block(CodegenContext *context, BasicBlock *bb) {
  printf("bb%zu:\n", bb->id);
  for (Value *val = bb->values; val; val = val->next) {
    codegen_dump_value(context, val);
    printf("\n");
  }
}

void codegen_dump_function(CodegenContext *context, Function *f) {
  printf("defun %s:\n", f->name);
  for (BasicBlock *bb = f->entry; bb; bb = bb->next) {
    codegen_dump_basic_block(context, bb);
  }
  printf("\n");
}

void codegen_dump_ir(CodegenContext *context) {
  for (Function *f = context->functions; f; f = f->next) {
    codegen_dump_function(context, f);
  }
}