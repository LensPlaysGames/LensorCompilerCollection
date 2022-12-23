#include <codegen/codegen_forward.h>
#include <codegen/intermediate_representation.h>

#include <stdlib.h>
#include <inttypes.h>

//#define DEBUG_USES

void mark_used(IRInstruction *usee, IRInstruction *user) {
  VECTOR_FOREACH_PTR (IRInstruction *, i_user, usee->users) {
    ASSERT(i_user != user, "Instruction already marked as user.");
  }
  VECTOR_PUSH(usee->users, user);
}

void ir_remove_use(IRInstruction *usee, IRInstruction *user) {
#ifdef DEBUG_USES
  fprintf(stderr, "[Use] Removing use of %%%u in %%%u\n", usee->id, user->id);
#endif

  VECTOR_REMOVE_ELEMENT_UNORDERED(usee->users, user);
}

bool ir_is_branch(IRInstruction* i) {
  STATIC_ASSERT(IR_COUNT == 32, "Handle all branch types.");
  switch (i->type) {
    case IR_BRANCH:
    case IR_BRANCH_CONDITIONAL:
    case IR_RETURN:
    case IR_UNREACHABLE:
      return true;
    default:
      return false;
  }
}

bool ir_is_closed(IRBlock *block) {
    return block->instructions.last && ir_is_branch(block->instructions.last);
}

void set_pair_and_mark
(IRInstruction *parent,
 IRInstruction *lhs,
 IRInstruction *rhs
 )
{
  parent->lhs = lhs;
  parent->rhs = rhs;
  mark_used(lhs, parent);
  mark_used(rhs, parent);
}

void ir_insert_into_block(IRBlock *block, IRInstruction *instruction) {
  IRInstruction *branch = block->instructions.last;
  if (branch && ir_is_branch(branch)) {
    ir_set_ids(block->function->context);
    ir_femit(stdout, block->function->context);
    ICE("Cannot insert into closed block. Use ir_force_insert_into_block() instead if that was intended.");
  }
  ir_force_insert_into_block(block, instruction);
}

void ir_force_insert_into_block
(IRBlock *block,
 IRInstruction *i
 )
{
  i->parent_block = block;
  DLIST_PUSH_BACK(block->instructions, i);
}

void ir_insert
(CodegenContext *context,
 IRInstruction *new_instruction
 )
{
  ASSERT(context->block != NULL, "Can not insert when context has NULL insertion block.");
  ir_insert_into_block(context->block, new_instruction);
}

void insert_instruction_before(IRInstruction *i, IRInstruction *before) {
  ASSERT(i && before);
  DLIST_INSERT_BEFORE(before->parent_block->instructions, i, before);
  i->parent_block = before->parent_block;
}

void insert_instruction_after(IRInstruction *i, IRInstruction *after) {
  ASSERT(i && after);
  DLIST_INSERT_AFTER(after->parent_block->instructions, i, after);
  i->parent_block = after->parent_block;
}

void ir_remove(IRInstruction* instruction) {
  if (instruction->users.size) {
    fprintf(stderr, "Cannot remove used instruction."
                    "Instruction:\n");
    ir_set_func_ids(instruction->parent_block->function);
    ir_femit_instruction(stderr, instruction);
    fprintf(stderr, "In function:\n");
    ir_femit_function(stderr, instruction->parent_block->function);
    PANIC("Cannot remove used instruction.");
  }

  DLIST_REMOVE(instruction->parent_block->instructions, instruction);
  VECTOR_DELETE(instruction->users);
  ir_unmark_usees(instruction);
  /// Parameters / static refs should not be freed here.
  if (instruction->type != IR_PARAMETER && instruction->type != IR_STATIC_REF) {
    free(instruction);
  }
}

void ir_remove_and_free_block(IRBlock *block) {
  /// Remove all instructions from the block.
  while (block->instructions.first) {
    /// Remove this instruction from PHIs.
    if (block->instructions.first->type == IR_PHI) {
        VECTOR_FOREACH_PTR (IRInstruction *, user, block->instructions.first->users) {
            if (user->type == IR_PHI) ir_phi_remove_argument(user, block);
        }
    }

    /// Remove it from the blocks.
    ir_remove(block->instructions.first);
  }
  DLIST_REMOVE(block->function->blocks, block);
  free(block);
}

#define INSERT(instruction) ir_insert(context, (instruction))

void ir_femit_instruction
(FILE *file,
 IRInstruction *inst
 )
{
  ASSERT(inst, "Can not emit NULL inst to file.");

# define ID_FORMAT "%%%u | ", inst->id
  const size_t id_width = 10;
  size_t id_length = (size_t) snprintf(NULL, 0, ID_FORMAT);
  size_t difference = id_width - id_length;
  while (difference--) {
    fputc(' ', file);
  }
  if (inst->id) fprintf(file, ID_FORMAT);
  else fprintf(file, "   | ");
# undef ID_FORMAT

# define RESULT_FORMAT "r%d | ", inst->result
  if (inst->result) {
    const size_t result_width = 6;
    size_t result_length = (size_t) snprintf(NULL, 0, RESULT_FORMAT);
    size_t result_difference = result_width - result_length;
    while (result_difference--) {
      fputc(' ', file);
    }
    fprintf(file, RESULT_FORMAT);
  } else {
    fprintf(file, "    | ");
  }
# undef RESULT_FORMAT

  STATIC_ASSERT(IR_COUNT == 32, "Handle all instruction types.");
  switch (inst->type) {
  case IR_IMMEDIATE:
    fprintf(file, "imm %"PRId64, inst->imm);
    break;

  case IR_CALL: {
    if (inst->call.tail_call) { fprintf(file, "tail "); }
    if (!inst->call.is_indirect) {
      string name = inst->call.callee_function->name;
      fprintf(file, "call %.*s", (int) name.size, name.data);
    } else {
      fprintf(file, "call %%%u", inst->call.callee_instruction->id);
    }
    fputc('(', file);
    bool first = true;
    VECTOR_FOREACH_PTR (IRInstruction*, i, inst->call.arguments) {
      if (!first) { fprintf(file, ", "); }
      else first = false;
      fprintf(file, "%%%u", i->id);
    }
    fputc(')', file);
  } break;
  case IR_STATIC_REF:
    fprintf(file, ".ref %.*s", (int) inst->static_ref->name.size, inst->static_ref->name.data);
    break;
  case IR_FUNC_REF:
    fprintf(file, ".ref %.*s", (int) inst->function_ref->name.size, inst->function_ref->name.data);
    break;

  case IR_RETURN: fprintf(file, "ret %%%u", inst->operand->id); break;

#define PRINT_BINARY_INSTRUCTION(enumerator, name) case IR_##enumerator: \
    fprintf(file, #name " %%%u, %%%u", inst->lhs->id, inst->rhs->id); break;
    ALL_BINARY_INSTRUCTION_TYPES(PRINT_BINARY_INSTRUCTION)
#undef PRINT_BINARY_INSTRUCTION

  case IR_COPY: fprintf(file, "copy %%%u", inst->operand->id); break;
  case IR_PARAMETER: fprintf(file, ".param %%%u", inst->id); break;

  case IR_BRANCH:
    fprintf(file, "br bb%zu", inst->destination_block->id);
    break;
  case IR_BRANCH_CONDITIONAL:
    fprintf(file, "br.cond %%%u, bb%zu, bb%zu",
            inst->cond_br.condition->id,
            inst->cond_br.then->id,
            inst->cond_br.else_->id);
    break;
  case IR_PHI: {
    fprintf(file, "phi ");
    bool first = true;
    VECTOR_FOREACH_PTR (IRPhiArgument*, arg, inst->phi_args) {
      if (first) { first = false; }
      else { fprintf(file, ", "); }
      fprintf(file, "[bb%zu : %%%u]", arg->block->id, arg->value->id);
    }
  } break;
  case IR_LOAD:
    fprintf(file, "load %%%u", inst->operand->id);
    break;
  case IR_STORE:
    fprintf(file, "store into %%%u, %%%u", inst->store.addr->id, inst->store.value->id);
    break;
  case IR_REGISTER:
    fprintf(file, "register r%d", inst->result);
    break;
  case IR_ALLOCA:
    fprintf(file, "alloca %"PRId64, inst->imm);
    break;
  /// No-op
  case IR_UNREACHABLE:
    fprintf(file, "unreachable");
    break;
  default:
    ICE("Invalid IRType %d\n", inst->type);
  }

#ifdef DEBUG_USES
  /// Print users
  fprintf(file, "\033[60GUsers: ");
  VECTOR_FOREACH_PTR (IRInstruction*, user, inst->users) {
    fprintf(file, "%%%u, ", user->id);
  }
#endif

  fputc('\n', file);
}

void ir_femit_block
(FILE *file,
 IRBlock *block
 )
{
  fprintf(file, "bb%zu:\n", block->id);
  DLIST_FOREACH (IRInstruction*, instruction, block->instructions) {
    ir_femit_instruction(file, instruction);
  }
}

void ir_femit_function
(FILE *file,
 IRFunction *function
 )
{
  fprintf(file, "\n");
  ir_print_defun(file, function);
  fprintf(file, " {\n");
  DLIST_FOREACH (IRBlock*, block, function->blocks) {
    ir_femit_block(file, block);
  }
  fprintf(file, "}\n");
}

void ir_femit
(FILE *file,
 CodegenContext *context
 )
{
  fprintf(file, "=======================================================================================");
  ir_set_ids(context);
  VECTOR_FOREACH_PTR (IRFunction*, function, *context->functions) {
    ir_femit_function(file, function);
  }
}

void ir_set_func_ids(IRFunction *f) {
  /// We start counting at 1 so that 0 can indicate an invalid/removed element.
  size_t block_id = 1;
  u32 instruction_id = (u32) f->parameters.size + 1;

  DLIST_FOREACH (IRBlock *, block, f->blocks) {
    block->id = block_id++;
    DLIST_FOREACH (IRInstruction *, instruction, block->instructions) {
        if (instruction->type == IR_PARAMETER || !ir_is_value(instruction)) continue;
        instruction->id = instruction_id++;
    }
  }
}

void ir_set_ids(CodegenContext *context) {
  size_t function_id = 0;

  VECTOR_FOREACH_PTR (IRFunction*, function, *context->functions) {
    function->id = function_id++;
    ir_set_func_ids(function);
  }
}

void ir_add_function_call_argument
(CodegenContext *context,
 IRInstruction *call,
 IRInstruction *argument
 )
{
  (void) context;
  VECTOR_PUSH(call->call.arguments, argument);
  mark_used(argument, call);
}

IRBlock *ir_block_create() { return calloc(1, sizeof(IRBlock)); }

IRInstruction *ir_parameter
(CodegenContext *context,
 size_t index) {
  ASSERT(context->function, "No function!");
  ASSERT(index < context->function->parameters.size, "Parameter index out of bounds.");
  return context->function->parameters.data[index];
}

/// Add a parameter to a function. This alters the number of
/// parameters the function takes, so use it with caution.
void ir_add_parameter_to_function(IRFunction *f) {
  INSTRUCTION(parameter, IR_PARAMETER);
  parameter->imm = f->parameters.size;
  parameter->id = (u32) f->parameters.size;
  ir_insert(f->context, parameter);
  VECTOR_PUSH(f->parameters, parameter);
}

void ir_phi_add_argument
(IRInstruction *phi,
 IRPhiArgument *argument)
{
  VECTOR_PUSH(phi->phi_args, argument);
  mark_used(argument->value, phi);
}

void ir_phi_argument
(IRInstruction *phi,
 IRBlock *phi_predecessor,
 IRInstruction *argument
 )
{
  IRPhiArgument *arg = calloc(1, sizeof *arg);
  arg->block = phi_predecessor;
  arg->value = argument;

  VECTOR_PUSH(phi->phi_args, arg);
  mark_used(argument, phi);
}

void ir_phi_remove_argument(IRInstruction *phi, IRBlock *block) {
  VECTOR_FOREACH_PTR (IRPhiArgument*, argument, phi->phi_args) {
    if (argument->block == block) {
      ir_remove_use(argument->value, phi);
      VECTOR_REMOVE_ELEMENT_UNORDERED(phi->phi_args, argument);
      return;
    }
  }
}

IRInstruction *ir_phi(CodegenContext *context) {
  INSTRUCTION(phi, IR_PHI);
  INSERT(phi);
  return phi;
}

void ir_block_attach_to_function
(IRFunction *function,
 IRBlock *new_block
 )
{
  DLIST_PUSH_BACK(function->blocks, new_block);
  new_block->function = function;
}


void ir_block_attach
(CodegenContext *context,
 IRBlock *new_block
 )
{
  ir_block_attach_to_function(context->function, new_block);
  context->block = new_block;
}

IRFunction *ir_function(CodegenContext *context, span name, size_t params) {
  IRFunction *function = calloc(1, sizeof(IRFunction));
  function->name = string_dup(name);

  /// A function *must* contain at least one block, so we start new
  /// functions out with an empty block.
  IRBlock *block = ir_block_create();

  /// Set the current function and add it to the list of functions.
  context->function = function;
  function->context = context;
  ir_block_attach(context, block);
  VECTOR_PUSH(*context->functions, function);

  /// Generate param refs.
  for (u64 i = 1; i <= params; i++) {
    INSTRUCTION(param, IR_PARAMETER);
    param->imm = i - 1;
    param->id = (u32) i;
    VECTOR_PUSH(function->parameters, param);
    INSERT(param);
  }
  return function;
}

IRInstruction *ir_funcref(CodegenContext *context, IRFunction *function) {
  INSTRUCTION(funcref, IR_FUNC_REF);
  funcref->function_ref = function;
  INSERT(funcref);
  return funcref;
}

IRInstruction *ir_immediate
(CodegenContext *context,
 u64 immediate
 )
{
  INSTRUCTION(imm, IR_IMMEDIATE);
  imm->imm = immediate;
  INSERT(imm);
  return imm;
}

IRInstruction *ir_load
(CodegenContext *context,
 IRInstruction *address
 )
{
  INSTRUCTION(load, IR_LOAD);

  load->operand = address;
  mark_used(address, load);

  INSERT(load);
  return load;
}

IRInstruction *ir_direct_call
(CodegenContext *context,
 IRFunction *callee
 )
{
  ASSERT(callee, "Cannot create direct call to NULL function");
  (void) context;
  INSTRUCTION(call, IR_CALL);
  call->call.callee_function = callee;
  return call;
}

IRInstruction *ir_indirect_call
(CodegenContext *context,
 IRInstruction *function
 )
{
  (void) context;
  INSTRUCTION(call, IR_CALL);
  call->call.callee_instruction = function;
  call->call.is_indirect = true;
  mark_used(function, call);
  return call;
}

IRInstruction *ir_store
(CodegenContext *context,
 IRInstruction *data,
 IRInstruction *address
 )
{
  INSTRUCTION(store, IR_STORE);
  store->store.addr = address;
  store->store.value = data;
  mark_used(address, store);
  mark_used(data, store);
  INSERT(store);
  return store;
}

IRInstruction *ir_branch_conditional
(CodegenContext *context,
 IRInstruction *condition,
 IRBlock *then_block,
 IRBlock *otherwise_block
 )
{
  INSTRUCTION(branch, IR_BRANCH_CONDITIONAL);

  branch->cond_br.condition = condition;
  mark_used(condition, branch);

  branch->cond_br.then = then_block;
  branch->cond_br.else_ = otherwise_block;
  INSERT(branch);
  return branch;
}

IRInstruction *ir_branch_into_block
(IRBlock *destination,
 IRBlock *block
 )
{
  ASSERT(!ir_is_closed(block));
  INSTRUCTION(branch, IR_BRANCH);
  branch->destination_block = destination;
  branch->parent_block = block;
  DLIST_PUSH_BACK(block->instructions, branch);
  return branch;
}

IRInstruction *ir_branch
(CodegenContext *context,
 IRBlock *destination
 )
{
  return ir_branch_into_block(destination, context->block);
}

IRInstruction *ir_return(CodegenContext *context, IRInstruction* return_value) {
  INSTRUCTION(branch, IR_RETURN);
  branch->operand = return_value;
  INSERT(branch);
  mark_used(return_value, branch);
  return branch;
}

/// NOTE: Does not self insert!
IRInstruction *ir_copy_unused
(CodegenContext *context,
 IRInstruction *source
 )
{
  (void) context;
  INSTRUCTION(copy, IR_COPY);
  copy->operand = source;
  return copy;
}

/// NOTE: Does not self insert!
IRInstruction *ir_copy
(CodegenContext *context,
 IRInstruction *source
 )
{
  IRInstruction *copy = ir_copy_unused(context, source);
  mark_used(source, copy);
  return copy;
}

IRInstruction *ir_not
(CodegenContext *context,
 IRInstruction *source
 )
{
  INSTRUCTION(x, IR_NOT);
  x->operand = source;
  mark_used(source, x);
  INSERT(x);
  return x;
}

#define CREATE_BINARY_INSTRUCTION(enumerator, name)                                           \
  IRInstruction *ir_##name(CodegenContext *context, IRInstruction *lhs, IRInstruction *rhs) { \
    INSTRUCTION(x, IR_##enumerator);                                                               \
    set_pair_and_mark(x, lhs, rhs);                                                           \
    INSERT(x);                                                                                \
    return x;                                                                                 \
  }
ALL_BINARY_INSTRUCTION_TYPES(CREATE_BINARY_INSTRUCTION)
#undef CREATE_BINARY_INSTRUCTION

IRInstruction *ir_create_static
(CodegenContext *context,
 Type *ty,
 span name) {
  /// Create the variable.
  IRStaticVariable *v = calloc(1, sizeof *v);
  v->name = string_dup(name);
  v->type = ty;
  v->cached_size = ast_sizeof(ty);
  v->cached_alignment = 8; /// TODO.
  VECTOR_PUSH(context->static_vars, v);

  /// Create an instruction to reference it and return it.
  INSTRUCTION(ref, IR_STATIC_REF);
  ref->static_ref = v;
  v->reference = ref;
  INSERT(ref);
  return ref;
}

IRInstruction *ir_stack_allocate
(CodegenContext *context,
 usz size
 )
{
  INSTRUCTION(alloca, IR_ALLOCA);
  alloca->alloca.size = size;
  INSERT(alloca);
  return alloca;
}

typedef struct {
  IRInstruction *usee;
  IRInstruction *replacement;
} ir_internal_replace_use_t;
static void ir_internal_replace_use(IRInstruction *user, IRInstruction **child, void *data) {
  ir_internal_replace_use_t *replace = data;

#ifdef DEBUG_USES
  fprintf(stderr, "  Replacing uses of %%%u in %%%u with %%%u\n",
    replace->usee->id, user->id, replace->replacement->id);
#endif

  if (user == replace->replacement) return;
  if (*child == replace->usee) *child = replace->replacement;
}

void ir_for_each_child(
  IRInstruction *user,
  void callback(IRInstruction *user, IRInstruction **child, void *data),
  void *data
) {
  STATIC_ASSERT(IR_COUNT == 32, "Handle all instruction types.");
  switch (user->type) {
  case IR_PHI:
    VECTOR_FOREACH_PTR (IRPhiArgument*, arg, user->phi_args) {
      callback(user, &arg->value, data);
    }
    break;
  case IR_LOAD:
  case IR_COPY:
  case IR_RETURN:
  case IR_NOT:
    callback(user, &user->operand, data);
    break;

  case IR_STORE:
    callback(user, &user->store.addr, data);
    callback(user, &user->store.value, data);
    break;

  ALL_BINARY_INSTRUCTION_CASES()
    callback(user, &user->lhs, data);
    callback(user, &user->rhs, data);
    break;

  case IR_CALL:
    if (user->call.is_indirect) callback(user, &user->call.callee_instruction, data);
    VECTOR_FOREACH (IRInstruction*, arg, user->call.arguments) callback(user, arg, data);
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
    break;
  default:
    ICE("Invalid IR instruction type %d", user->type);
  }
}

bool ir_is_value(IRInstruction *instruction) {
  STATIC_ASSERT(IR_COUNT == 32, "Handle all instruction types.");
  switch (instruction->type) {
    default: UNREACHABLE();
    case IR_IMMEDIATE:
    case IR_CALL:
    case IR_LOAD:
    case IR_PHI:
    case IR_COPY:
    case IR_PARAMETER:
    case IR_REGISTER:
    case IR_ALLOCA:
    case IR_STATIC_REF:
    case IR_FUNC_REF:
    ALL_BINARY_INSTRUCTION_CASES()
      return true;

    case IR_STORE:
    case IR_RETURN:
    case IR_BRANCH:
    case IR_BRANCH_CONDITIONAL:
    case IR_UNREACHABLE:
      return false;
  }
}

void ir_print_defun(FILE *file, IRFunction *f) {
  /// Function signature.
  fprintf(file, "defun %.*s (", (int) f->name.size, f->name.data);

  /// Parameters.
  bool first_param = true;
  for (size_t i = 1; i <= f->parameters.size; ++i) {
    if (first_param) first_param = false;
    else fprintf(file, ", ");
    fprintf(file, "%%%u", f->parameters.data[i - 1]->id);
  }

  /// End of param list.
  fprintf(file, ")");

  /// Attributes, if any.
  if (f->attr_consteval) fprintf(file, " consteval");
  if (f->attr_forceinline) fprintf(file, " forceinline");
  if (f->attr_global) fprintf(file, " global");
  if (f->attr_leaf) fprintf(file, " leaf");
  if (f->attr_noreturn) fprintf(file, " noreturn");
  if (f->attr_pure) fprintf(file, " pure");
}

void ir_replace_uses(IRInstruction *instruction, IRInstruction *replacement) {
  if (instruction == replacement) { return; }
#ifdef DEBUG_USES
  fprintf(stderr, "[Use] Replacing uses of %%%u with %%%u\n", instruction->id, replacement->id);
#endif
  VECTOR_FOREACH_PTR (IRInstruction *, user, instruction->users) {
    ir_internal_replace_use_t replace = { instruction, replacement };
    ir_for_each_child(user, ir_internal_replace_use, &replace);
  }

  VECTOR_APPEND_ALL(replacement->users, instruction->users);
  VECTOR_CLEAR(instruction->users);
}

static void ir_internal_unmark_usee(IRInstruction *user, IRInstruction **child, void *_) {
  (void) _;
  VECTOR_FOREACH_PTR (IRInstruction *, child_user, (*child)->users) {
    if (child_user == user) {
      VECTOR_REMOVE_ELEMENT_UNORDERED((*child)->users, child_user);
      break;
    }
  }
}

void ir_unmark_usees(IRInstruction *instruction) {
  ir_for_each_child(instruction, ir_internal_unmark_usee, NULL);
}

void ir_mark_unreachable(IRBlock *block) {
  STATIC_ASSERT(IR_COUNT == 32, "Handle all branch types");
  IRInstruction *i = block->instructions.last;
  switch (i->type) {
    default: break;
    case IR_BRANCH: {
      IRInstruction *first = i->destination_block->instructions.first;
      while (first && first->type == IR_PHI) {
        ir_phi_remove_argument(first, block);
        first = first->next;
      }
    } break;
    case IR_BRANCH_CONDITIONAL: {
      IRInstruction *first = i->cond_br.then->instructions.first;
      while (first && first->type == IR_PHI) {
        ir_phi_remove_argument(first, block);
        first = first->next;
      }
      first = i->cond_br.else_->instructions.first;
      while (first && first->type == IR_PHI) {
        ir_phi_remove_argument(first, block);
        first = first->next;
      }
    } break;
  }
  i->type = IR_UNREACHABLE;
}

#undef INSERT

