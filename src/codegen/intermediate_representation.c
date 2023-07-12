#include <codegen/intermediate_representation.h>

#include <ast.h>
#include <codegen/codegen_forward.h>
#include <utils.h>

#include <stdlib.h>

//#define DEBUG_USES
void mark_used(IRInstruction *usee, IRInstruction *user) {
  // Don't push duplicate users.
  foreach_val (i_user, usee->users) {
    if (i_user == user) return;
  }
  vector_push(usee->users, user);
}

void ir_remove_use(IRInstruction *usee, IRInstruction *user) {
#ifdef DEBUG_USES
  eprint("[Use] Removing use of %%%u in %%%u\n", usee->id, user->id);
#endif

  vector_remove_element_unordered(usee->users, user);
}

bool ir_is_branch(IRInstruction* i) {
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
  list_push_back(block->instructions, i);
}

void ir_insert
(CodegenContext *context,
 IRInstruction *new_instruction
 )
{
  ASSERT(context->block != NULL, "Can not insert when context has NULL insertion block.");

  /// Handle closed blocks.
  if (ir_is_closed(context->block)) {
    IRBlock *new_block = ir_block_create();
    ir_block_attach(context, new_block);
    context->block = new_block;
    ir_insert(context, new_instruction);
    return;
  }

  ir_insert_into_block(context->block, new_instruction);
}

void insert_instruction_before(IRInstruction *i, IRInstruction *before) {
  ASSERT(i && before);
  list_insert_before(before->parent_block->instructions, i, before);
  i->parent_block = before->parent_block;
}

void insert_instruction_after(IRInstruction *i, IRInstruction *after) {
  ASSERT(i && after);
  list_insert_after(after->parent_block->instructions, i, after);
  i->parent_block = after->parent_block;
}

void ir_remove(IRInstruction* instruction) {
  if (instruction->users.size) {
    eprint("Cannot remove used instruction.\n"
           "Instruction:\n");
    ir_set_func_ids(instruction->parent_block->function);
    ir_femit_instruction(stderr, instruction);
    eprint("In function:\n");
    ir_femit_function(stderr, instruction->parent_block->function);
    ICE("Cannot remove used instruction.");
  }

  /// Remove the instruction if it’s inserted in a block.
  if (instruction->parent_block &&
    (instruction->prev ||
    instruction->next ||
    instruction->parent_block->instructions.first == instruction ||
    instruction->parent_block->instructions.last == instruction)
  ) list_remove(instruction->parent_block->instructions, instruction);
  vector_delete(instruction->users);
  ir_unmark_usees(instruction);

  /// Don’t delete the main poison value of a context, but allow
  /// deleting other poison values.
  if (instruction->kind == IR_POISON && instruction->ctx && instruction == instruction->ctx->poison) return;

  /// Unlink static refs.
  if (instruction->kind == IR_STATIC_REF)
    vector_remove_element_unordered(instruction->static_ref->references, instruction);

  /// Parameters should not be freed here.
  if (instruction->kind != IR_PARAMETER) free(instruction);
  else vector_push(instruction->parent_block->function->context->removed_instructions, instruction);
}

void ir_force_remove(CodegenContext *ctx, IRInstruction *instruction) {
  ir_replace_uses(instruction, ctx->poison);
  ir_remove(instruction);
}

void ir_remove_and_free_block(IRBlock *block) {
  /// Remove all instructions from the block.
  while (block->instructions.last) {
    /// Remove this instruction from PHIs that use it.
    foreach_val (user, block->instructions.last->users) {
      if (user->kind == IR_PHI) ir_phi_remove_argument(user, block);
    }

    /// Remove it from the blocks.
    ir_remove(block->instructions.last);
  }
  list_remove(block->function->blocks, block);
  free(block);
}

void ir_free_function(IRFunction *f) {
  /// Free each block.
  list_foreach (b, f->blocks) {
    /// Free each instruction.
    list_foreach (i, b->instructions) ir_free_instruction_data(i);
    list_delete(b->instructions);

    /// Free the block name.
    free(b->name.data);
  }

  /// Free the name, params, and block list.
  free(f->name.data);
  vector_delete(f->parameters);
  list_delete(f->blocks);

  /// Free the function itself.
  free(f);
}

void ir_free_instruction_data(IRInstruction *i) {
  if (!i) return;
  STATIC_ASSERT(IR_COUNT == 40, "Handle all instruction types.");
  switch (i->kind) {
    case IR_INTRINSIC:
    case IR_CALL:
      vector_delete(i->call.arguments);
      break;

    case IR_PHI:
      foreach_val (arg, i->phi_args) free(arg);
      vector_delete(i->phi_args);
      break;
    default: break;
  }

  /// Free usage data.
  vector_delete(i->users);
}

const char *ir_irtype_string(IRType t) {
  STATIC_ASSERT(IR_COUNT == 40, "Handle all instruction types.");
  switch (t) {
  case IR_IMMEDIATE: return "imm";
  case IR_LIT_INTEGER: return "lit.int";
  case IR_LIT_STRING: return "lit.str";
  case IR_CALL: return "call";
  case IR_INTRINSIC: return "intrinsic";
  case IR_STATIC_REF: return ".ref";
  case IR_FUNC_REF: return "ref";
  case IR_POISON: return "poison";

#define PRINT_BINARY_INSTRUCTION(enumerator, name)  \
    case IR_##enumerator: return #name;
    ALL_BINARY_INSTRUCTION_TYPES(PRINT_BINARY_INSTRUCTION)
#undef PRINT_BINARY_INSTRUCTION

  case IR_NOT: return "not";
  case IR_ZERO_EXTEND: return "z.ext";
  case IR_SIGN_EXTEND: return "s.ext";
  case IR_TRUNCATE: return "truncate";
  case IR_BITCAST: return "bitcast";
  case IR_COPY: return "copy";
  case IR_PARAMETER: return ".param";
  case IR_RETURN: return "ret";
  case IR_BRANCH: return "br";
  case IR_BRANCH_CONDITIONAL: return "br.cond";
  case IR_PHI: return "phi";
  case IR_LOAD: return "load";
  case IR_STORE: return "store";
  case IR_REGISTER: return ".reg";
  case IR_ALLOCA: return "alloca";
  case IR_UNREACHABLE: return "unreachable";
  default: ICE("Invalid IRType %d\n", t);
  }
}

#define INSERT(instruction) ir_insert(context, (instruction))

void ir_femit_instruction
(FILE *file,
 IRInstruction *inst
 )
{
  ASSERT(inst, "Can not emit NULL inst to file.");

  const usz id_max_width = 7;
  if (inst->id) {
    const usz width = number_width(inst->id);
    for (usz i = width; i < id_max_width; ++i) putc(' ', file);
    fprint(file, "%34%%%u %31│ ", inst->id);
  }
  else {
    for (usz i = 0; i < id_max_width; ++i) putc(' ', file);
    fprint(file, "  %31│ ");
  }

  const usz result_max_width = 6;
  if (inst->result) {
    const usz result_length = number_width(inst->result);
    for (usz i = result_length; i < result_max_width; ++i) putc(' ', file);
    fprint(file, "%34r%u %31│ ", inst->result);
  } else {
    for (usz i = 0; i < result_max_width; ++i) putc(' ', file);
    fprint(file, "  %31│ ");
  }

  STATIC_ASSERT(IR_COUNT == 40, "Handle all instruction types.");
  switch (inst->kind) {
  case IR_POISON:
    fprint(file, "%33poison");
    break;

  case IR_IMMEDIATE:
    fprint(file, "%33imm %35%U", inst->imm);
    break;

  case IR_LIT_INTEGER:
    fprint(file, "%33lit.int %35%U", inst->imm);
    break;

  case IR_LIT_STRING:
    fprint(file, "%33lit.str %35%S", inst->str);
    break;

  case IR_INTRINSIC: {
    switch (inst->call.intrinsic) {
      default: fprint(file, "%33intrinsic.%d ", inst->call.intrinsic); break;
      case INTRIN_BUILTIN_SYSCALL: fprint(file, "%33intrinsic.syscall "); break;
      case INTRIN_BUILTIN_DEBUGTRAP: fprint(file, "%33intrinsic.debugtrap "); break;
    }

    fprint(file, "%31(");
    bool first = true;
    foreach_val (i, inst->call.arguments) {
      if (!first) { fprint(file, "%31, "); }
      else first = false;
      fprint(file, "%34%%%u", i->id);
    }
    fprint(file, "%31)");
  } break;

  case IR_CALL: {
    if (inst->call.tail_call) { fprint(file, "%33tail "); }
    if (!inst->call.is_indirect) {
      string name = inst->call.callee_function->name;
      fprint(file, "%33call %32%S", name);
    } else {
      fprint(file, "%33call %34%%%u", inst->call.callee_instruction->id);
    }
    fprint(file, "%31(");
    bool first = true;
    foreach_val (i, inst->call.arguments) {
      if (!first) { fprint(file, "%31, "); }
      else first = false;
      fprint(file, "%34%%%u", i->id);
    }
    fprint(file, "%31)");
  } break;
  case IR_STATIC_REF:
    fprint(file, "%33.ref %m%S", inst->static_ref->name);
    break;
  case IR_FUNC_REF:
    fprint(file, "%31ref %32%S", inst->function_ref->name);
    break;

#define PRINT_BINARY_INSTRUCTION(enumerator, name) case IR_##enumerator: \
    fprint(file, "%33" #name " %34%%%u%31, %34%%%u", inst->lhs->id, inst->rhs->id); break;
    ALL_BINARY_INSTRUCTION_TYPES(PRINT_BINARY_INSTRUCTION)
#undef PRINT_BINARY_INSTRUCTION

  case IR_NOT:
    fprint(file, "%33not %34%%%u", inst->operand->id);
    break;

  case IR_ZERO_EXTEND:
    fprint(file, "%33z.ext %34%%%u", inst->operand->id);
    break;

  case IR_SIGN_EXTEND:
    fprint(file, "%33s.ext %34%%%u", inst->operand->id);
    break;

  case IR_TRUNCATE:
    fprint(file, "%33truncate %34%%%u", inst->operand->id);
    break;

  case IR_BITCAST:
    fprint(file, "%33bitcast %34%%%u", inst->operand->id);
    break;

  case IR_COPY:
    fprint(file, "%33copy %34%%%u", inst->operand->id);
    break;

  case IR_PARAMETER:
    fprint(file, "%31.param %34%%%u", inst->id);
    break;

  case IR_RETURN:
    if (inst->operand) fprint(file, "%33ret %34%%%u", inst->operand->id);
    else fprint(file, "%33ret");
    break;
  case IR_BRANCH:
    fprint(file, "%33br bb%Z", inst->destination_block->id);
    break;
  case IR_BRANCH_CONDITIONAL:
    fprint(file, "%33br.cond %34%%%u%31, %33bb%Z%31, %33bb%Z",
            inst->cond_br.condition->id, inst->cond_br.then->id, inst->cond_br.else_->id);
    break;
  case IR_PHI: {
    fprint(file, "%33phi ");
    bool first = true;
    foreach_val (arg, inst->phi_args) {
      if (first) { first = false; }
      else { fprint(file, "%31, "); }
      fprint(file, "%31[%33bb%Z%31 : %34%%%u%31]", arg->block->id, arg->value->id);
    }
  } break;

  case IR_LOAD:
    fprint(file, "%33load %34%%%u", inst->operand->id);
    break;
  case IR_STORE:
    fprint(file, "%33store into %34%%%u%31, %34%%%u", inst->store.addr->id, inst->store.value->id);
    break;
  case IR_REGISTER:
    fprint(file, "%31.reg %34%%%u", inst->result);
    break;
  case IR_ALLOCA:
    fprint(file, "%33alloca %34%U", inst->imm);
    break;

  /// No-op
  case IR_UNREACHABLE:
    fprint(file, "%33unreachable");
    break;
  default:
    ICE("Invalid IRType %d\n", inst->kind);
  }

  // Print type of instruction.
  if (inst->type) fprint(file, " %31| %T", inst->type);

#ifdef DEBUG_USES
  /// Print users
  fprint(file, "%m\033[60GUsers: ");
  foreach_val (user, inst->users) {
    fprint(file, "%%%u, ", user->id);
  }
#endif

  fprint(file, "%m\n");
}

void ir_femit_block
(FILE *file,
 IRBlock *block
 )
{
  fprint(file, "%33bb%Z%31:\n", block->id);
  list_foreach (instruction, block->instructions) {
    ir_femit_instruction(file, instruction);
  }
  fprint(file, "%m");
}

void ir_femit_function
(FILE *file,
 IRFunction *function
 )
{
  ir_print_defun(file, function);
  if (ir_function_is_definition(function)) {
    fprint(file, " %31{\n");
    Vector(IRBlock*) printed = {0};
    list_foreach (block, function->blocks) {
        ir_femit_block(file, block);
        vector_push(printed, block);
    }
    fprint(file, "%31}");
  }
  fprint(file, "%m\n");
}

void ir_femit
(FILE *file,
 CodegenContext *context
 )
{
  ir_set_ids(context);
  foreach_val (function, context->functions) {
    if (function_ptr != context->functions.data) fprint(file, "\n");
    ir_femit_function(file, function);
  }
  fprint(file, "%m");
}

void ir_set_func_ids(IRFunction *f) {
  /// We start counting at 1 so that 0 can indicate an invalid/removed element.
  usz block_id = 1;
  u32 instruction_id = (u32) f->parameters.size + 1;

  list_foreach (block, f->blocks) {
    block->id = block_id++;
    list_foreach (instruction, block->instructions) {
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

void ir_add_function_call_argument
(CodegenContext *context,
 IRInstruction *call,
 IRInstruction *argument
 )
{
  (void) context;
  vector_push(call->call.arguments, argument);
  mark_used(argument, call);
}

IRBlock *ir_block_create() { return calloc(1, sizeof(IRBlock)); }

IRInstruction *ir_parameter
(CodegenContext *context,
 usz index) {
  ASSERT(context->function, "No function!");
  ASSERT(index < context->function->parameters.size, "Parameter index out of bounds.");
  return context->function->parameters.data[index];
}

/// Add a parameter to a function. This alters the number of
/// parameters the function takes, so use it with caution.
void ir_add_parameter_to_function(IRFunction *f, Type *type) {
  INSTRUCTION(parameter, IR_PARAMETER);
  parameter->imm = f->parameters.size;
  parameter->id = (u32) f->parameters.size;
  parameter->type = type;
  ir_insert(f->context, parameter);
  vector_push(f->parameters, parameter);
}

void ir_phi_add_argument
(IRInstruction *phi,
 IRPhiArgument *argument)
{
  vector_push(phi->phi_args, argument);
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
  ir_phi_add_argument(phi, arg);
}

void ir_phi_remove_argument(IRInstruction *phi, IRBlock *block) {
  foreach_val (argument, phi->phi_args) {
    if (argument->block == block) {
      ir_remove_use(argument->value, phi);
      vector_remove_element_unordered(phi->phi_args, argument);
      return;
    }
  }
}

IRInstruction *ir_phi(CodegenContext *context, Type *type) {
  INSTRUCTION(phi, IR_PHI);
  phi->type = type;
  INSERT(phi);
  return phi;
}

void ir_block_attach_to_function
(IRFunction *function,
 IRBlock *new_block
 )
{
  list_push_back(function->blocks, new_block);
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

IRFunction *ir_function(CodegenContext *context, string name, Type *function_type, SymbolLinkage linkage) {
  ASSERT(function_type->kind == TYPE_FUNCTION, "Cannot create function of non-function type");

  IRFunction *function = calloc(1, sizeof(IRFunction));
  function->name = name;
  function->type = function_type;

  /// Add it to the list of functions.
  function->context = context;
  function->linkage = linkage;
  vector_push(context->functions, function);

  /// A function definition *must* contain at least one block, so we
  /// start new functions out with an empty block.
  if (linkage != LINKAGE_IMPORTED && linkage != LINKAGE_REEXPORTED) {
    IRBlock *block = ir_block_create();
    context->function = function;
    ir_block_attach(context, block);

    /// Generate param refs.
    for (u64 i = 1; i <= function_type->function.parameters.size; ++i) {
      INSTRUCTION(param, IR_PARAMETER);
      param->imm = i - 1;
      param->id = (u32) i;
      param->type = function_type->function.parameters.data[i - 1].type;
      vector_push(function->parameters, param);
      INSERT(param);
    }
  }
  return function;
}

IRInstruction *ir_funcref(CodegenContext *context, IRFunction *function) {
  INSTRUCTION(funcref, IR_FUNC_REF);
  funcref->function_ref = function;
  funcref->type = function->type;
  INSERT(funcref);
  return funcref;
}

IRInstruction *ir_immediate
(CodegenContext *context,
 Type *type,
 u64 immediate
 )
{
  INSTRUCTION(imm, IR_IMMEDIATE);
  imm->imm = immediate;
  imm->type = type;
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

  Type *t = type_canonical(address->type);
  if (!(t && (type_is_pointer(t) || type_is_reference(t)))) {
    //print("address type: %T\n", address->type);
    ir_femit_instruction(stdout, address);
    if (t) ICE("Can not emit IR_LOAD from type %T as it is not a pointer", t);
    else ICE("Can not emit IR_LOAD to NULL canonical type!");
  }
  if (type_is_pointer(t)) load->type = t->pointer.to;
  else if (type_is_reference(t)) load->type = t->reference.to;
  else load->type = t;

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
  INSTRUCTION(call, IR_CALL);
  call->call.callee_function = callee;
  call->type = callee->type->function.return_type;
  ASSERT(call->type);
  return call;
}

IRInstruction *ir_indirect_call
(CodegenContext *context,
 IRInstruction *function
 )
{
  INSTRUCTION(call, IR_CALL);
  call->call.callee_instruction = function;
  call->call.is_indirect = true;
  call->type = function->type->pointer.to->function.return_type;
  ASSERT(call->type);
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
  list_push_back(block->instructions, branch);
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
  if (return_value) mark_used(return_value, branch);
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
  copy->type = source->type;
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
  x->type = source->type;
  mark_used(source, x);
  INSERT(x);
  return x;
}

IRInstruction *ir_zero_extend
(CodegenContext *context,
 Type *result_type,
 IRInstruction *value)
{
  INSTRUCTION(zext, IR_ZERO_EXTEND);
  zext->operand = value;
  zext->type = result_type;
  mark_used(value, zext);
  INSERT(zext);
  return zext;
}

IRInstruction *ir_sign_extend
(CodegenContext *context,
 Type *result_type,
 IRInstruction *value)
{
  INSTRUCTION(sext, IR_SIGN_EXTEND);
  sext->operand = value;
  sext->type = result_type;
  mark_used(value, sext);
  INSERT(sext);
  return sext;
}

IRInstruction *ir_truncate
(CodegenContext *context,
 Type *result_type,
 IRInstruction *value)
{
  INSTRUCTION(trunc, IR_TRUNCATE);
  trunc->operand = value;
  trunc->type = result_type;
  mark_used(value, trunc);
  INSERT(trunc);
  return trunc;
}

IRInstruction *ir_bitcast
(CodegenContext *context,
 Type *to_type,
 IRInstruction *value)
{
  INSTRUCTION(bitcast, IR_BITCAST);
  bitcast->operand = value;
  bitcast->type = to_type;
  mark_used(value, bitcast);
  INSERT(bitcast);
  return bitcast;
}

#define CREATE_BINARY_INSTRUCTION(enumerator, name)                                           \
  IRInstruction *ir_##name(CodegenContext *context, IRInstruction *lhs, IRInstruction *rhs) { \
    INSTRUCTION(x, IR_##enumerator);                                                          \
    x->type = lhs->type;                                                                      \
    set_pair_and_mark(x, lhs, rhs);                                                           \
    INSERT(x);                                                                                \
    return x;                                                                                 \
  }

/// TODO: Should set type to bool once we have that.
#define CREATE_COMPARISON_INSTRUCTION(enumerator, name)                                       \
  IRInstruction *ir_##name(CodegenContext *context, IRInstruction *lhs, IRInstruction *rhs) { \
    INSTRUCTION(x, IR_##enumerator);                                                          \
    x->type = t_integer;                                                                      \
    set_pair_and_mark(x, lhs, rhs);                                                           \
    INSERT(x);                                                                                \
    return x;                                                                                 \
  }

ALL_BINARY_INSTRUCTION_TYPES_EXCEPT_COMPARISONS(CREATE_BINARY_INSTRUCTION)
ALL_BINARY_COMPARISON_TYPES(CREATE_COMPARISON_INSTRUCTION)
#undef CREATE_COMPARISON_INSTRUCTION
#undef CREATE_BINARY_INSTRUCTION

IRInstruction *ir_create_static(CodegenContext *context, Node* decl, Type *type, span name) {
  /// Create the variable.
  IRStaticVariable *v = calloc(1, sizeof *v);
  v->name = string_dup(name);
  v->type = type;
  v->decl = decl;
  vector_push(context->static_vars, v);

  /// Create an instruction to reference it and return it.
  INSTRUCTION(ref, IR_STATIC_REF);
  ref->static_ref = v;
  if (type_is_reference(v->type))
    ref->type = v->type;
  else ref->type = ast_make_type_pointer(context->ast, v->type->source_location, v->type);
  vector_push(v->references, ref);
  INSERT(ref);
  return ref;
}

/// NOTE: Currently unused, but can be used to load a static reference
/// more than once without generating duplicate static variables.
IRInstruction *ir_static_reference(CodegenContext *context, IRStaticVariable *v) {
  INSTRUCTION(ref, IR_STATIC_REF);
  ref->static_ref = v;
  ref->type = ast_make_type_pointer(context->ast, v->type->source_location, v->type);
  vector_push(v->references, ref);
  INSERT(ref);
  return ref;
}

IRInstruction *ir_stack_allocate(CodegenContext *context, Type *type) {
  INSTRUCTION(alloca, IR_ALLOCA);
  alloca->alloca.size = type_sizeof(type);
  alloca->type = ast_make_type_pointer(context->ast, type->source_location, type);
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
  eprint("  Replacing uses of %%%u in %%%u with %%%u\n",
    replace->usee->id, user->id, replace->replacement->id);
#endif

  if (user == replace->replacement) return;
  if (*child == replace->usee) {
    *child = replace->replacement;
    ir_remove_use(replace->usee, user);
    mark_used(replace->replacement, user);
  }
}

void ir_for_each_child(
  IRInstruction *user,
  void callback(IRInstruction *user, IRInstruction **child, void *data),
  void *data
) {
  STATIC_ASSERT(IR_COUNT == 40, "Handle all instruction types.");
  switch (user->kind) {
  case IR_PHI:
      foreach_val (arg, user->phi_args) {
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
    !ir_function_is_definition(f) ? "declare" : "defun",
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

void ir_replace_uses(IRInstruction *instruction, IRInstruction *replacement) {
  if (instruction == replacement) { return; }
#ifdef DEBUG_USES
  eprint("[Use] Replacing uses of %%%u with %%%u\n", instruction->id, replacement->id);
#endif
  while (instruction->users.size) {
    ir_internal_replace_use_t replace = { instruction, replacement };
    ir_for_each_child(instruction->users.data[0], ir_internal_replace_use, &replace);
  }
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

void ir_unmark_usees(IRInstruction *instruction) {
  ir_for_each_child(instruction, ir_internal_unmark_usee, NULL);
}

void ir_mark_unreachable(IRBlock *block) {
  STATIC_ASSERT(IR_COUNT == 40, "Handle all branch types");
  IRInstruction *i = block->instructions.last;
  switch (i->kind) {
    default: break;
    case IR_BRANCH: {
      IRInstruction *first = i->destination_block->instructions.first;
      while (first && first->kind == IR_PHI) {
        ir_phi_remove_argument(first, block);
        first = first->next;
      }
    } break;
    case IR_BRANCH_CONDITIONAL: {
      IRInstruction *first = i->cond_br.then->instructions.first;
      while (first && first->kind == IR_PHI) {
        ir_phi_remove_argument(first, block);
        first = first->next;
      }
      first = i->cond_br.else_->instructions.first;
      while (first && first->kind == IR_PHI) {
        ir_phi_remove_argument(first, block);
        first = first->next;
      }
    } break;
    case IR_UNREACHABLE: break;
    case IR_RETURN:
      ir_remove_use(i->operand, i);
      break;
  }
  i->kind = IR_UNREACHABLE;
}

void ir_set_backend_flag(IRInstruction *const instruction, const int bit_index) {
  instruction->backend_flags |= (1 << bit_index);
}
bool ir_get_backend_flag(const IRInstruction *const instruction, const int bit_index) {
  return instruction->backend_flags & (1 << bit_index);
}

IRInstruction *ir_get_literal_string(CodegenContext *ctx, usz string_index) {
  ASSERT(string_index < ctx->ast->strings.size, "Invalid string index %Z", string_index);
  INSTRUCTION(s, IR_LIT_STRING);
  s->str = ctx->ast->strings.data[string_index];
  s->string_index = string_index;
  return s;
}

Type* ir_call_get_callee_type(IRInstruction* inst) {
    return !inst->call.is_indirect
        ? inst->call.callee_function->type
        : type_is_pointer(inst->call.callee_instruction->type)
            ? inst->call.callee_instruction->type->pointer.to
            : inst->call.callee_instruction->type;
}

IRInstruction *ir_intrinsic(CodegenContext *ctx, Type *t, enum IntrinsicKind intrinsic) {
    (void) ctx;
    INSTRUCTION(i, IR_INTRINSIC);
    i->call.intrinsic = intrinsic;
    i->type = t;
    return i;
}

bool ir_function_is_definition(IRFunction *f) {
  return f->blocks.first != NULL;
}

#undef INSERT

