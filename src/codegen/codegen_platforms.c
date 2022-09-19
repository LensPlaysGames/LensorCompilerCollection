#include <codegen.h>
#include <codegen/codegen_platforms.h>
#include <codegen/x86_64/arch_x86_64.h>
#include <error.h>
#include <stdarg.h>
#include <stdlib.h>
#include <string.h>

CodegenContext *codegen_context_create_top_level
(enum CodegenOutputFormat format,
 enum CodegenCallingConvention call_convention,
 enum CodegenAssemblyDialect dialect,
 FILE* code) {
  CodegenContext *cg_context;

  if (format == CG_FMT_x86_64_GAS) {
    // TODO: Handle call_convention for creating codegen context!
    if (call_convention == CG_CALL_CONV_MSWIN) {
      cg_context = codegen_context_x86_64_mswin_create(NULL);
      ASSERT(cg_context);
    } else if (call_convention == CG_CALL_CONV_LINUX) {
      // TODO: Create codegen context for GAS linux assembly.
      panic("Not implemented: Create codegen context for GAS linux x86_64 assembly.");
    } else {
      panic("Unrecognized calling convention!");
    }
  } else {
    panic("Unrecognized codegen format");
  }

  cg_context->code = code;
  cg_context->dialect = dialect;
  return cg_context;
}

/// Create a codegen context from a parent context.
CodegenContext *codegen_context_create(CodegenContext *parent) {
  ASSERT(parent, "create_codegen_context() can only create contexts when a parent is given.");
  ASSERT(CG_FMT_COUNT == 1, "create_codegen_context() must exhaustively handle all codegen output formats.");
  ASSERT(CG_CALL_CONV_COUNT == 2, "create_codegen_context() must exhaustively handle all calling conventions.");
  if (parent->format == CG_FMT_x86_64_GAS) {
    if (parent->call_convention == CG_CALL_CONV_MSWIN) {
      return codegen_context_x86_64_mswin_create(parent);
    } else if (parent->call_convention == CG_CALL_CONV_LINUX) {
      // return codegen_context_x86_64_gas_linux_create(parent);
    }
  }
  panic("create_codegen_context() could not create a new context from the given parent.");
  return NULL; // Unreachable
}

/// Free a codegen context.
void codegen_context_free(CodegenContext *context) {
  if (context->format == CG_FMT_x86_64_GAS) {
    if (context->call_convention == CG_CALL_CONV_MSWIN) {
      return codegen_context_x86_64_mswin_free(context);
    } else if (context->call_convention == CG_CALL_CONV_LINUX) {
      // return codegen_context_x86_64_gas_linux_free(parent);
    }
  }
  panic("free_codegen_context() could not free the given context.");
}

/// Insert a value into the current basic block.
void insert(CodegenContext *context, Value *value) {
  ASSERT(context->insert_point, "Cannot insert without insert point");
  // Create a new block if the current one is closed.
  if (context->insert_point->closed) codegen_basic_block_create(context);

  // Insert the value into the current block.
  if (context->insert_point->end) {
    context->insert_point->end->next_in_block = value;
    value->prev_in_block = context->insert_point->end;
  } else {
    value->prev_in_block = NULL;
    context->insert_point->values = value;
  }
  context->insert_point->end = value;
  value->next_in_block = NULL;

  // Close the block if the value was a branch.
  if (value->type == IR_INSTRUCTION_BRANCH    ||
      value->type == IR_INSTRUCTION_BRANCH_IF ||
      value->type == IR_INSTRUCTION_RETURN) {
    context->insert_point->closed = 1;
  }
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

Value *codegen_create_call(CodegenContext *context, char external) {
  Value *call = calloc(1, sizeof *call);
  call->type = IR_INSTRUCTION_CALL;
  call->call_value.type = external ? FUNCTION_CALL_TYPE_EXTERNAL : FUNCTION_CALL_TYPE_INTERNAL;
  insert(context, call);
  return call;
}

void codegen_add_function_arg(CodegenContext *context, Value *call_value, Value *arg) {
  ASSERT(call_value->type == IR_INSTRUCTION_CALL, "Argument 'call' must be a function call");
  FunctionCall* call = &call_value->call_value;
  if (call->args) {
    Value *func_args = call->args;
    while (func_args->next) { func_args = func_args->next; }
    func_args->next = arg;
    arg->prev = func_args;
  } else {
    call->args = arg;
  }
}

Function *codegen_function_create(CodegenContext *context, const char *name) {
  Function *f = calloc(1, sizeof *f);
  if (!name) {
    // Generate a symbol.
    size_t id = (*context->func_count)++;
    size_t size = snprintf(NULL, 0, "__function_%zu", id) + 1;
    char *buf = malloc(size);
    snprintf(buf, size, "__function_%zu", id);
    f->name = buf;
  } else {
    f->name = strdup(name);
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
}

Value *codegen_load_global_address(CodegenContext *context, const char *name) {
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

void codegen_branch_if(CodegenContext *context, Value *reg, BasicBlock *true_block, BasicBlock *false_block) {
  Value *br = calloc(1, sizeof *br);
  br->type = IR_INSTRUCTION_BRANCH;
  br->cond_branch_value.true_branch = true_block;
  br->cond_branch_value.false_branch = false_block;
  insert(context, br);
}

void codegen_branch(CodegenContext *context, BasicBlock *block) {
  Value *br = calloc(1, sizeof *br);
  br->type = IR_INSTRUCTION_BRANCH_IF;
  br->branch_target = block;
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

static void codegen_dump_value(CodegenContext *context, Value *val, size_t indent) {
  ASSERT(val->type > 0 && val->type < IR_INSTRUCTION_COUNT, "Invalid value type");

  for (size_t i = 0; i < indent; i++) putchar(' ');

  switch (val->type) {
    case IR_INSTRUCTION_CALL:
      printf("CALL\n");
      for (Value *arg = val->call_value.args; arg; arg = arg->next) {
        codegen_dump_value(context, arg, indent + 2);
      }
      break;
    case IR_INSTRUCTION_COMMENT:
      printf("COMMENT: \"%s\"\n", val->comment_value);
      break;
    case IR_INSTRUCTION_BRANCH:
      printf("BRANCH TO %p\n", val->branch_target);
      break;
    case IR_INSTRUCTION_BRANCH_IF:
      printf("BRANCH TRUE: %p, FALSE: %p\n",
             val->cond_branch_value.true_branch,
             val->cond_branch_value.false_branch);
      break;
    case IR_INSTRUCTION_RETURN:
      printf("RETURN\n");
      break;
    case IR_INSTRUCTION_FUNCTION_REF:
      printf("FUNCTION REFERENCE: %s\n", val->function_ref->name);
      break;
    case IR_INSTRUCTION_ADD:
      printf("ADD\n");
      codegen_dump_value(context, val->lhs, indent + 2);
      codegen_dump_value(context, val->rhs, indent + 2);
      break;
    case IR_INSTRUCTION_SUB:
      printf("SUB\n");
      codegen_dump_value(context, val->lhs, indent + 2);
      codegen_dump_value(context, val->rhs, indent + 2);
      break;
    case IR_INSTRUCTION_MUL:
      printf("MUL\n");
      codegen_dump_value(context, val->lhs, indent + 2);
      codegen_dump_value(context, val->rhs, indent + 2);
      break;
    case IR_INSTRUCTION_DIV:
      printf("DIV\n");
      codegen_dump_value(context, val->lhs, indent + 2);
      codegen_dump_value(context, val->rhs, indent + 2);
      break;
    case IR_INSTRUCTION_MOD:
      printf("MOD\n");
      codegen_dump_value(context, val->lhs, indent + 2);
      codegen_dump_value(context, val->rhs, indent + 2);
      break;
    case IR_INSTRUCTION_SHL:
      printf("SHL\n");
      codegen_dump_value(context, val->lhs, indent + 2);
      codegen_dump_value(context, val->rhs, indent + 2);
      break;
    case IR_INSTRUCTION_SAR:
      printf("SAR\n");
      codegen_dump_value(context, val->lhs, indent + 2);
      codegen_dump_value(context, val->rhs, indent + 2);
      break;
    case IR_INSTRUCTION_COMPARISON:
      printf("COMPARISON: %d\n", val->comparison.type);
      codegen_dump_value(context, val->comparison.lhs, indent + 2);
      codegen_dump_value(context, val->comparison.rhs, indent + 2);
      break;
    case IR_INSTRUCTION_ALLOCA:
      printf("ALLOCA: %llu\n", val->immediate);
      break;
    case IR_INSTRUCTION_IMMEDIATE:
      printf("IMMEDIATE: %llu\n", val->immediate);
      break;
    case IR_INSTRUCTION_PHI:
      printf("PHI\n");
      for (PHINodeEntry *e = val->phi_entries; e; e = e->next) {
        for (size_t i = 0; i < indent + 2; i++) putchar(' ');
        printf("BLOCK: %p\n", e->block);
        codegen_dump_value(context, e->value, indent + 2);
      }
      break;
    case IR_INSTRUCTION_GLOBAL_REF:
      printf("GLOBAL REF: \"%s\"\n", val->global_name);
      break;
    case IR_INSTRUCTION_GLOBAL_VAL:
      printf("GLOBAL VAL: \"%s\"\n", val->global_name);
      break;
    case IR_INSTRUCTION_STORE_GLOBAL:
      printf("GLOBAL STORE TO \"%s\"\n", val->global_store.name);
      codegen_dump_value(context, val->global_store.value, indent + 2);
      break;
    case IR_INSTRUCTION_LOCAL_REF:
      printf("LOCAL REF\n");
      codegen_dump_value(context, val->local_ref, indent + 2);
      break;
    case IR_INSTRUCTION_LOCAL_VAL:
      printf("LOCAL REF\n");
      codegen_dump_value(context, val->local_ref, indent + 2);
      break;
    case IR_INSTRUCTION_STORE_LOCAL:
      printf("LOCAL STORE\n");
      codegen_dump_value(context, val->lhs, indent + 2);
      codegen_dump_value(context, val->rhs, indent + 2);
      break;
    case IR_INSTRUCTION_STORE:
      printf("LOCAL STORE\n");
      codegen_dump_value(context, val->lhs, indent + 2);
      codegen_dump_value(context, val->rhs, indent + 2);
      break;
    case IR_INSTRUCTION_PARAM_REF:
      printf("PARAMETER %llu\n", val->immediate);
      break;
    case IR_INSTRUCTION_COUNT: UNREACHABLE();
  }
}

static void codegen_dump_basic_block(CodegenContext *context, BasicBlock *bb) {
  printf("  BLOCK: %p\n", bb);
  for (Value *val = bb->values; val; val = val->next_in_block) {
    codegen_dump_value(context, val, 4);
  }
}

static void codegen_dump_function(CodegenContext *context, Function *f) {
  printf("FUNCTION: %s\n", f->name);

  for (BasicBlock *bb = f->entry; bb; bb = bb->next) {
    codegen_dump_basic_block(context, bb);
  }
}

void codegen_dump_ir(CodegenContext *context) {
  for (Function *f = context->functions; f; f = f->next) {
    codegen_dump_function(context, f);
  }
}
