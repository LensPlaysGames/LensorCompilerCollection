#include <codegen/codegen_forward.h>
#include <codegen/intermediate_representation.h>

#include <stdint.h>
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
  fprintf(stderr, "[Use] Removing use of %%%zu in %%%zu\n", usee->id, user->id);
#endif

  VECTOR_REMOVE_ELEMENT_UNORDERED(usee->users, user);
}

bool ir_is_branch(IRInstruction* i) {
  STATIC_ASSERT(IR_COUNT == 28, "Handle all branch types.");
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
 IRPair *pair,
 IRInstruction *lhs,
 IRInstruction *rhs
 )
{
  pair->car = lhs;
  pair->cdr = rhs;
  mark_used(lhs, parent);
  mark_used(rhs, parent);
}

void ir_insert_into_block(IRBlock *block, IRInstruction *instruction) {
  IRInstruction *branch = block->instructions.last;
  if (branch && ir_is_branch(branch)) {
    PANIC("Cannot insert into closed block. Use ir_force_insert_into_block() instead if that was intended.");
  }
  ir_force_insert_into_block(block, instruction);
}

void ir_force_insert_into_block
(IRBlock *block,
 IRInstruction *i
 )
{
  i->block = block;
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
  DLIST_INSERT_BEFORE(before->block->instructions, i, before);
  i->block = before->block;
}

void insert_instruction_after(IRInstruction *i, IRInstruction *after) {
  ASSERT(i && after);
  DLIST_INSERT_AFTER(after->block->instructions, i, after);
  i->block = after->block;
}

void ir_remove(IRInstruction* instruction) {
  if (instruction->users.size) {
    fprintf(stderr, "Cannot remove used instruction."
                    "Instruction:\n");
    ir_set_func_ids(instruction->block->function);
    ir_femit_instruction(stderr, instruction);
    fprintf(stderr, "In function:\n");
    ir_femit_function(stderr, instruction->block->function);
    PANIC("Cannot remove used instruction.");
  }

  DLIST_REMOVE(instruction->block->instructions, instruction);
  VECTOR_DELETE(instruction->users);
  ir_unmark_usees(instruction);
  free(instruction);
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
 IRInstruction *instruction
 )
{
  ASSERT(instruction, "Can not emit NULL instruction to file.");

# define ID_FORMAT "%%%zu | ", instruction->id
  const size_t id_width = 10;
  size_t id_length = (size_t) snprintf(NULL, 0, ID_FORMAT);
  size_t difference = id_width - id_length;
  while (difference--) {
    fputc(' ', file);
  }
  fprintf(file, ID_FORMAT);
# undef ID_FORMAT

# define RESULT_FORMAT "r%d | ", instruction->result
  if (instruction->result) {
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

  STATIC_ASSERT(IR_COUNT == 28);
  switch (instruction->type) {
  case IR_IMMEDIATE:
    fprintf(file, "%"PRId64, instruction->value.immediate);
    break;
  case IR_CALL:
    if (instruction->value.call.tail_call) { fprintf(file, "tail "); }
    switch (instruction->value.call.type) {
    case IR_CALLTYPE_DIRECT:
      fprintf(file, "%s", instruction->value.call.value.name);
      break;
    case IR_CALLTYPE_INDIRECT:
      fprintf(file, "%%%zu", instruction->value.call.value.callee->id);
      break;
    default:
      TODO("Handle %d IRCallType.", instruction->value.call.type);
      break;
    }
    fputc('(', file);
    IRCallArgument *argument = instruction->value.call.arguments;
    if (argument) {
      fprintf(file, "%%%zu", argument->value->id);
      argument = argument->next;
    }
    for (; argument; argument = argument->next) {
      fprintf(file, ", %%%zu", argument->value->id);
    }
    fputc(')', file);
    break;
  case IR_RETURN:
    fprintf(file, "return %%%zu", instruction->value.reference->id);
    break;
  case IR_ADD:
    fprintf(file, "add %%%zu, %%%zu",
            instruction->value.pair.car->id,
            instruction->value.pair.cdr->id);
    break;
  case IR_MULTIPLY:
    fprintf(file, "mul %%%zu, %%%zu",
            instruction->value.pair.car->id,
            instruction->value.pair.cdr->id);
    break;
  case IR_DIVIDE:
    fprintf(file, "div %%%zu, %%%zu",
            instruction->value.pair.car->id,
            instruction->value.pair.cdr->id);
    break;
  case IR_MODULO:
    fprintf(file, "mod %%%zu, %%%zu",
            instruction->value.pair.car->id,
            instruction->value.pair.cdr->id);
    break;
  case IR_SHIFT_LEFT:
    fprintf(file, "shl %%%zu, %%%zu",
            instruction->value.pair.car->id,
            instruction->value.pair.cdr->id);
    break;
  case IR_SHIFT_RIGHT_ARITHMETIC:
    fprintf(file, "sar %%%zu, %%%zu",
            instruction->value.pair.car->id,
            instruction->value.pair.cdr->id);
    break;

  case IR_SUBTRACT:
    fprintf(file, "subtract %%%zu, %%%zu",
            instruction->value.pair.car->id,
            instruction->value.pair.cdr->id);
    break;
  case IR_GLOBAL_LOAD:
    fprintf(file, "g.load %s", instruction->value.name);
    break;
  case IR_GLOBAL_STORE:
    fprintf(file, "g.store %%%zu, %s",
            instruction->value.global_assignment.new_value->id,
            instruction->value.global_assignment.name);
    break;
  case IR_GLOBAL_ADDRESS:
    fprintf(file, "g.address %s", instruction->value.name);
    break;
  case IR_COPY:
    fprintf(file, "copy %%%zu", instruction->value.reference->id);
    break;
  case IR_LOCAL_LOAD:
    fprintf(file, "l.load %%%zu", instruction->value.reference->id);
    break;
  case IR_LOCAL_STORE:
    fprintf(file, "l.store %%%zu, %%%zu",
            instruction->value.pair.car->id,
            instruction->value.pair.cdr->id);
    break;
  case IR_LOCAL_ADDRESS:
    fprintf(file, "l.address %%%zu", instruction->value.reference->id);
    break;
  case IR_PARAMETER_REFERENCE:
    fprintf(file, "parameter.reference %%%"PRId64,
            instruction->value.immediate);
    break;
  case IR_COMPARISON:
    switch (instruction->value.comparison.type) {
    case COMPARE_EQ:
      fprintf(file, "eq");
      break;
    case COMPARE_GE:
      fprintf(file, "ge");
      break;
    case COMPARE_LE:
      fprintf(file, "le");
      break;
    case COMPARE_GT:
      fprintf(file, "gt");
      break;
    case COMPARE_LT:
      fprintf(file, "lt");
      break;
    case COMPARE_NE:
      fprintf(file, "ne");
      break;
    default:
      PANIC("Unhandled comparison type: %d", instruction->value.comparison.type);
      break;
    }
    fprintf(file, " %%%zu, %%%zu",
            instruction->value.comparison.pair.car->id,
            instruction->value.comparison.pair.cdr->id);
    break;
  case IR_BRANCH:
    fprintf(file, "branch bb%zu", instruction->value.block->id);
    break;
  case IR_BRANCH_CONDITIONAL:
    fprintf(file, "branch.conditional %%%zu, bb%zu, bb%zu",
            instruction->value.conditional_branch.condition->id,
            instruction->value.conditional_branch.true_branch->id,
            instruction->value.conditional_branch.false_branch->id);
    break;
  case IR_PHI: {
    fprintf(file, "phi ");
    bool first = true;
    VECTOR_FOREACH_PTR (IRPhiArgument*, arg, instruction->value.phi_arguments) {
      if (first) { first = false; }
      else { fprintf(file, ", "); }
      fprintf(file, "[bb%zu : %%%zu]",
              arg->block->id,
              arg->value->id);
    }
  } break;
  case IR_LOAD:
    fprintf(file, "load %%%zu", instruction->value.reference->id);
    break;
  case IR_STORE:
    fprintf(file, "store %%%zu, %%%zu",
            instruction->value.pair.cdr->id,
            instruction->value.pair.car->id);
    break;
  case IR_REGISTER:
    fprintf(file, "register r%d", instruction->result);
    break;
  case IR_STACK_ALLOCATE:
    fprintf(file, "stack.allocate %"PRId64, instruction->value.immediate);
    break;
  /// No-op
  case IR_UNREACHABLE:
    fprintf(file, "unreachable");
    break;
  default:
    TODO("Handle IRType %d\n", instruction->type);
    break;
  }

#ifdef DEBUG_USES
  /// Print users
  fprintf(file, "\033[60GUsers: ");
  VECTOR_FOREACH_PTR (IRInstruction*, user, instruction->users) {
    fprintf(file, "%%%zu, ", user->id);
  }
#endif

  fputc('\n', file);
}

void ir_femit_block
(FILE *file,
 IRBlock *block
 )
{
  fprintf(file, "  bb%zu\n", block->id);
  DLIST_FOREACH (IRInstruction*, instruction, block->instructions) {
    ir_femit_instruction(file, instruction);
  }
}

void ir_femit_function
(FILE *file,
 IRFunction *function
 )
{
  fprintf(file, "f%zu\n", function->id);
  DLIST_FOREACH (IRBlock*, block, function->blocks) {
    ir_femit_block(file, block);
  }
}

void ir_femit
(FILE *file,
 CodegenContext *context
 )
{
  fprintf(file, "=======================================================================================\n\n");
  VECTOR_FOREACH_PTR (IRFunction*, function, *context->functions) {
    ir_femit_function(file, function);
  }
  putchar('\n');
}

void ir_set_func_ids(IRFunction *f) {
  /// We start counting at 1 so that 0 can indicate an invalid/removed element.
  size_t block_id = 1;
  size_t instruction_id = 1;

  DLIST_FOREACH (IRBlock *, block, f->blocks) {
    block->id = block_id++;
    DLIST_FOREACH (IRInstruction *, instruction, block->instructions) {
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
  IRCallArgument *new_argument = calloc(1, sizeof(IRCallArgument));
  ASSERT(new_argument, "Could not allocate memory for new_argument.");

  new_argument->value = argument;

  if (!call->value.call.arguments) {
    call->value.call.arguments = new_argument;
  } else {
    IRCallArgument *arguments = call->value.call.arguments;
    for (; arguments->next; arguments = arguments->next);
    arguments->next = new_argument;
  }

  mark_used(argument, call);
}

IRBlock *ir_block_create() {
  IRBlock *block = calloc(1, sizeof(IRBlock));
  ASSERT(block, "Could not allocate memory for first IRBlock of new IRFunction.");
  return block;
}

IRInstruction *ir_parameter_reference
(CodegenContext *context,
 int64_t index) {
  INSTRUCTION(param, IR_PARAMETER_REFERENCE)
  param->value.immediate = index;
  INSERT(param);
  return param;
}

void ir_phi_add_argument
(IRInstruction *phi,
 IRPhiArgument *argument)
{
  VECTOR_PUSH(phi->value.phi_arguments, argument);
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

  VECTOR_PUSH(phi->value.phi_arguments, arg);
  mark_used(argument, phi);
}

void ir_phi_remove_argument(IRInstruction *phi, IRBlock *block) {
  VECTOR_FOREACH_PTR (IRPhiArgument*, argument, phi->value.phi_arguments) {
    if (argument->block == block) {
      ir_remove_use(argument->value, phi);
      VECTOR_REMOVE_ELEMENT_UNORDERED(phi->value.phi_arguments, argument);
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

IRFunction *ir_function_create() {
  IRFunction *function = calloc(1, sizeof(IRFunction));
  ASSERT(function, "Could not allocate memory for new IRFunction.");
  return function;
}

IRFunction *ir_function(CodegenContext *context, const char *name) {
  IRFunction *function = ir_function_create();
  function->name = strdup(name);
  // A function *must* contain at least one block, so we start new
  // functions out with an empty block.
  IRBlock *block = ir_block_create();
  context->function = function;
  ir_block_attach(context, block);
  VECTOR_PUSH(*context->functions, function);
  return function;
}

IRInstruction *ir_immediate
(CodegenContext *context,
 int64_t immediate
 )
{
  INSTRUCTION(imm, IR_IMMEDIATE);
  imm->value.immediate = immediate;
  INSERT(imm);
  return imm;
}

IRInstruction *ir_load
(CodegenContext *context,
 IRInstruction *address
 )
{
  INSTRUCTION(load, IR_LOAD);

  load->value.reference = address;
  mark_used(address, load);

  INSERT(load);
  return load;
}

IRInstruction *ir_direct_call
(CodegenContext *context,
 char* function_name
 )
{
  INSTRUCTION(call, IR_CALL);
  call->value.call.type = IR_CALLTYPE_DIRECT;
  call->value.call.value.name = function_name;
  return call;
}

IRInstruction *ir_indirect_call
(CodegenContext *context,
 IRInstruction *function
 )
{
  INSTRUCTION(call, IR_CALL);
  call->value.call.type = IR_CALLTYPE_INDIRECT;

  call->value.call.value.callee = function;
  mark_used(function, call);

  return call;
}

IRInstruction *ir_load_global_address
(CodegenContext *context,
 char *name
 )
{
  INSTRUCTION(global_address, IR_GLOBAL_ADDRESS);
  global_address->value.name = name;
  INSERT(global_address);
  return global_address;
}

IRInstruction *ir_load_local_address
(CodegenContext *context,
 IRInstruction *local
 )
{
  INSTRUCTION(local_address, IR_LOCAL_ADDRESS);

  local_address->value.reference = local;
  mark_used(local, local_address);

  INSERT(local_address);
  return local_address;
}

IRInstruction *ir_load_global
(CodegenContext *context,
 char *name
 )
{
  INSTRUCTION(global_load, IR_GLOBAL_LOAD);
  global_load->value.name = name;
  INSERT(global_load);
  return global_load;
}

IRInstruction *ir_load_local
(CodegenContext *context,
 IRInstruction *local
 )
{
  INSTRUCTION(local_load, IR_LOCAL_LOAD);

  local_load->value.reference = local;
  mark_used(local, local_load);

  INSERT(local_load);
  return local_load;
}

IRInstruction *ir_store_global
(CodegenContext *context,
 IRInstruction *source,
 char *name
 )
{
  INSTRUCTION(global_store, IR_GLOBAL_STORE);

  global_store->value.global_assignment.new_value = source;
  mark_used(source, global_store);

  global_store->value.global_assignment.name = name;
  INSERT(global_store);
  return global_store;
}

IRInstruction *ir_store_local
(CodegenContext *context,
 IRInstruction *source,
 IRInstruction *local
 )
{
  INSTRUCTION(local_store, IR_LOCAL_STORE);
  set_pair_and_mark(local_store, &local_store->value.pair, local, source);
  INSERT(local_store);
  return local_store;
}

IRInstruction *ir_store
(CodegenContext *context,
 IRInstruction *data,
 IRInstruction *address
 )
{
  INSTRUCTION(store, IR_STORE);
  set_pair_and_mark(store, &store->value.pair, address, data);
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

  branch->value.conditional_branch.condition = condition;
  mark_used(condition, branch);

  branch->value.conditional_branch.true_branch = then_block;
  branch->value.conditional_branch.false_branch = otherwise_block;
  INSERT(branch);
  return branch;
}

IRInstruction *ir_branch_into_block
(IRBlock *destination,
 IRBlock *block
 )
{
  INSTRUCTION(branch, IR_BRANCH);
  branch->value.block = destination;
  branch->block = block;
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
  branch->block = context->block;
  branch->value.reference = return_value;
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
  INSTRUCTION(copy, IR_COPY);
  copy->value.reference = source;
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

IRInstruction *ir_comparison
(CodegenContext *context,
 enum ComparisonType type,
 IRInstruction *lhs,
 IRInstruction *rhs
 )
{
  INSTRUCTION(cc, IR_COMPARISON);
  cc->value.comparison.type = type;

  set_pair_and_mark(cc, &cc->value.comparison.pair, lhs, rhs);

  INSERT(cc);
  return cc;
}

IRInstruction *ir_add
(CodegenContext *context,
 IRInstruction *lhs,
 IRInstruction *rhs
 )
{
  INSTRUCTION(add, IR_ADD);
  set_pair_and_mark(add, &add->value.pair, lhs, rhs);
  INSERT(add);
  return add;
}

IRInstruction *ir_subtract
(CodegenContext *context,
 IRInstruction *lhs,
 IRInstruction *rhs
 )
{
  INSTRUCTION(sub, IR_SUBTRACT);
  set_pair_and_mark(sub, &sub->value.pair, lhs, rhs);
  INSERT(sub);
  return sub;
}

IRInstruction *ir_multiply
(CodegenContext *context,
 IRInstruction *lhs,
 IRInstruction *rhs
 )
{
  INSTRUCTION(mul, IR_MULTIPLY);
  set_pair_and_mark(mul, &mul->value.pair, lhs, rhs);
  INSERT(mul);
  return mul;
}

IRInstruction *ir_divide
(CodegenContext *context,
 IRInstruction *lhs,
 IRInstruction *rhs
 )
{
  INSTRUCTION(div, IR_DIVIDE);
  set_pair_and_mark(div, &div->value.pair, lhs, rhs);
  INSERT(div);
  return div;
}

IRInstruction *ir_modulo
(CodegenContext *context,
 IRInstruction *lhs,
 IRInstruction *rhs
 )
{
  INSTRUCTION(mod, IR_MODULO);
  set_pair_and_mark(mod, &mod->value.pair, lhs, rhs);
  INSERT(mod);
  return mod;
}

IRInstruction *ir_shift_left
(CodegenContext *context,
 IRInstruction *lhs,
 IRInstruction *rhs
 )
{
  INSTRUCTION(shl, IR_SHIFT_LEFT);
  set_pair_and_mark(shl, &shl->value.pair, lhs, rhs);
  INSERT(shl);
  return shl;
}

IRInstruction *ir_shift_right_logical
(CodegenContext *context,
 IRInstruction *lhs,
 IRInstruction *rhs)
{
  INSTRUCTION(shr, IR_SHIFT_RIGHT_LOGICAL);
  set_pair_and_mark(shr, &shr->value.pair, lhs, rhs);
  INSERT(shr);
  return shr;
}

IRInstruction *ir_shift_right_arithmetic
(CodegenContext *context,
 IRInstruction *lhs,
 IRInstruction *rhs
 )
{
  INSTRUCTION(sar, IR_SHIFT_RIGHT_ARITHMETIC);
  set_pair_and_mark(sar, &sar->value.pair, lhs, rhs);
  INSERT(sar);
  return sar;
}

IRInstruction *ir_stack_allocate
(CodegenContext *context,
 int64_t size
 )
{
  INSTRUCTION(stack_allocation, IR_STACK_ALLOCATE);
  // TODO: Should we set offset here? Or just wait to calculate it like
  // we do currently?
  stack_allocation->value.stack_allocation.size = (size_t) size; /// FIXME: param should be size_t.
  INSERT(stack_allocation);
  return stack_allocation;
}

IRFunction *ir_get_function
(CodegenContext *context,
 const char *name) {
  VECTOR_FOREACH_PTR (IRFunction*, function, *context->functions) {
    if (strcmp(function->name, name) == 0) {
      return function;
    }
  }
  return NULL;
}

typedef struct {
  IRInstruction *usee;
  IRInstruction *replacement;
} ir_internal_replace_use_t;
static void ir_internal_replace_use(IRInstruction *user, IRInstruction **child, void *data) {
  ir_internal_replace_use_t *replace = data;

#ifdef DEBUG_USES
  fprintf(stderr, "  Replacing uses of %%%zu in %%%zu with %%%zu\n",
    replace->usee->id, user->id, replace->replacement->id);
#endif

  if (user == replace->replacement) {
    return;
  }

  if (*child == replace->usee) {
    *child = replace->replacement;
  }
}

/// Iterate over all children of an instruction.
static void ir_for_each_child(
  IRInstruction *user,
  void callback(IRInstruction *user, IRInstruction **child, void *data),
  void *data
) {
  STATIC_ASSERT(IR_COUNT == 28);
  switch (user->type) {
  case IR_PHI:
    VECTOR_FOREACH_PTR (IRPhiArgument*, arg, user->value.phi_arguments) {
      callback(user, &arg->value, data);
    }
    break;
  case IR_LOAD:
  case IR_LOCAL_ADDRESS:
  case IR_LOCAL_LOAD:
  case IR_COPY:
  case IR_RETURN:
    callback(user, &user->value.reference, data);
    break;
  case IR_GLOBAL_STORE:
    callback(user, &user->value.global_assignment.new_value, data);
    break;
  case IR_STORE:
  case IR_LOCAL_STORE:
  case IR_ADD:
  case IR_SUBTRACT:
  case IR_DIVIDE:
  case IR_MULTIPLY:
  case IR_MODULO:
  case IR_SHIFT_LEFT:
  case IR_SHIFT_RIGHT_ARITHMETIC:
  case IR_SHIFT_RIGHT_LOGICAL:
    callback(user, &user->value.pair.car, data);
    callback(user, &user->value.pair.cdr, data);
    break;
  case IR_CALL:
    if (user->value.call.type == IR_CALLTYPE_INDIRECT) {
      callback(user, &user->value.call.value.callee, data);
    }

    for (IRCallArgument *arg = user->value.call.arguments; arg; arg = arg->next) {
      callback(user, &arg->value, data);
    }
    break;
  case IR_BRANCH_CONDITIONAL:
    callback(user, &user->value.conditional_branch.condition, data);
    break;
  case IR_COMPARISON:
    callback(user, &user->value.comparison.pair.car, data);
    callback(user, &user->value.comparison.pair.cdr, data);
    break;
  case IR_PARAMETER_REFERENCE:
  case IR_GLOBAL_ADDRESS:
  case IR_GLOBAL_LOAD:
  case IR_IMMEDIATE:
  case IR_BRANCH:
  case IR_STACK_ALLOCATE:
  case IR_UNREACHABLE:
    break;
  default:
    TODO("Handle IR instruction type %d", user->type);
    break;
  }
}

void ir_replace_uses(IRInstruction *instruction, IRInstruction *replacement) {
  if (instruction == replacement) { return; }
#ifdef DEBUG_USES
  fprintf(stderr, "[Use] Replacing uses of %%%zu with %%%zu\n", instruction->id, replacement->id);
#endif
  VECTOR_FOREACH_PTR (IRInstruction *, user, instruction->users) {
    ir_internal_replace_use_t replace = { instruction, replacement };
    ir_for_each_child(user, ir_internal_replace_use, &replace);
  }

  VECTOR_APPEND_ALL(replacement->users, instruction->users);
  VECTOR_CLEAR(instruction->users);
}

static void ir_internal_unmark_usee(IRInstruction *user, IRInstruction **child, void *unused) {
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
  STATIC_ASSERT(IR_COUNT == 28, "Handle all branch types");
  IRInstruction *i = block->instructions.last;
  switch (i->type) {
    case IR_BRANCH: {
      IRInstruction *first = i->value.block->instructions.first;
      while (first && first->type == IR_PHI) {
        ir_phi_remove_argument(first, block);
        first = first->next;
      }
    } break;
    case IR_BRANCH_CONDITIONAL: {
      IRInstruction *first = i->value.conditional_branch.true_branch->instructions.first;
      while (first && first->type == IR_PHI) {
        ir_phi_remove_argument(first, block);
        first = first->next;
      }
      first = i->value.conditional_branch.false_branch->instructions.first;
      while (first && first->type == IR_PHI) {
        ir_phi_remove_argument(first, block);
        first = first->next;
      }
    } break;
  }
  i->type = IR_UNREACHABLE;
}

#undef INSERT

