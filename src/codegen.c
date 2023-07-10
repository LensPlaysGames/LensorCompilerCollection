#include <ast.h>
#include <codegen.h>
#include <codegen/codegen_forward.h>
#include <codegen/intermediate_representation.h>
#include <codegen/ir/ir.h>
#include <codegen/llvm/llvm_target.h>
#include <codegen/opt/opt.h>
#include <codegen/x86_64/arch_x86_64.h>
#include <error.h>
#include <ir_parser.h>
#include <parser.h>
#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>
#include <utils.h>
#include <vector.h>

#define DIAG(sev, loc, ...)                                                                                 \
  do {                                                                                                      \
    ctx->has_err = true;                                                                                    \
    issue_diagnostic(DIAG_ERR, (ctx)->ast->filename.data, as_span((ctx)->ast->source), (loc), __VA_ARGS__); \
    return;                                                                                                 \
  } while (0)

#define ERR(...) DIAG(DIAG_ERR, expr->source_location, __VA_ARGS__)

char codegen_verbose = 1;

/// ===========================================================================
///  Context creation.
/// ===========================================================================
CodegenContext *codegen_context_create
(AST *ast,
 CodegenArchitecture arch,
 CodegenTarget target,
 CodegenCallingConvention call_convention,
 FILE* code
 )
{
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
  foreach_val (f, context->functions) {
    /// Free each block.
    list_foreach (b, f->blocks) {
      /// Free each instruction.
      list_foreach (i, b->instructions) ir_free_instruction_data(i);
      list_delete (b->instructions);

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

  /// Finally, delete the function vector.
  vector_delete(context->functions);

  /// Free static variables.
  foreach_val (var, context->static_vars) {
    free(var->name.data);
    free(var);
  }
  vector_delete(context->static_vars);

  /// Free parameter instructions that were removed, but not freed.
  foreach_val (i, context->removed_instructions) {
    ir_free_instruction_data(i);
    free(i);
  }
  vector_delete(context->removed_instructions);

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

bool parameter_is_in_register(CodegenContext *context, IRFunction *function, usz parameter_index) {
  STATIC_ASSERT(ARCH_COUNT == 2, "Exhaustive handling of architectures");
  switch (context->arch) {
  case ARCH_X86_64: return parameter_is_in_register_x86_64(context, function, parameter_index);
  default: ICE("Unrecognized architecture %d!", context->arch);
  }
  UNREACHABLE();
}

static bool parameter_is_passed_as_pointer(CodegenContext *context, IRFunction *function, usz parameter_index) {
  STATIC_ASSERT(CG_CALL_CONV_COUNT == 2, "Exhaustive handling of calling conventions");
  switch (context->call_convention) {

  case CG_CALL_CONV_MSWIN:
    return type_sizeof(function->parameters.data[parameter_index]->type) > 8;

  case CG_CALL_CONV_SYSV:
    // FIXME: This is not how sysv works, nearly at all. But it's good enough for us right now.
    return type_sizeof(function->parameters.data[parameter_index]->type) > 16;

  default: ICE("Unrecognized calling convention %d!", context->call_convention);
  }
  UNREACHABLE();
}

/// ===========================================================================
///  Code generation.
/// ===========================================================================

static void codegen_expr(CodegenContext *ctx, Node *expr);

// Emit an lvalue.
static void codegen_lvalue(CodegenContext *ctx, Node *lval) {
  if (lval->address) return;
  switch (lval->kind) {
  default: ICE("Unhandled node kind %d", lval->kind);

  /// Variable declaration.
  case NODE_DECLARATION:
    lval->address = lval->declaration.static_
    ? ir_create_static(ctx, lval, lval->type, as_span(lval->declaration.name))
    : ir_stack_allocate(ctx, lval->type);

    /// Emit the initialiser if there is one.
    // TODO: TK_LBRACK aka array literals *may* be known at compile
    // time, if all of the elements are.
    if (lval->declaration.init) {
      if (lval->declaration.static_ &&
          (lval->declaration.init->kind == NODE_LITERAL) &&
          (lval->declaration.init->literal.type != TK_LBRACK)) {
            if (lval->declaration.init->literal.type == TK_NUMBER) {
              INSTRUCTION(i, IR_LIT_INTEGER);
              i->imm = lval->declaration.init->literal.integer;
              lval->address->static_ref->init = i;
            } else if (lval->declaration.init->literal.type == TK_STRING) {
              lval->address->static_ref->init = ir_get_literal_string(ctx, lval->declaration.init->literal.string_index);
            } else ICE("Unhandled literal type for static variable initialisation.");
          } else {
            codegen_expr(ctx, lval->declaration.init);
            ir_store(ctx, lval->declaration.init->ir, lval->address);
          }
    }
    return;

  case NODE_MEMBER_ACCESS: {
    codegen_lvalue(ctx, lval->member_access.struct_);
    // When member has zero byte offset, we can just use the address of the
    // struct with a modified type.
    if (lval->member_access.member->byte_offset)
      lval->address = ir_add(ctx, lval->member_access.struct_->address,
                             ir_immediate(ctx, t_integer, lval->member_access.member->byte_offset));
    else {
      lval->address = ir_copy(ctx, lval->member_access.struct_->address);
      ir_insert(ctx, lval->address);
    }
    lval->address->type = ast_make_type_pointer(ctx->ast, lval->source_location, lval->member_access.member->type);
  } break;

  case NODE_IF:
    TODO("`if` as an lvalue is not yet supported, but it's in the plans bb");
    break;

  case NODE_UNARY: {
    if (!lval->unary.postfix && lval->unary.op == TK_AT) {
      // mutual recursion go brrr
      codegen_expr(ctx, lval->unary.value);
      lval->address = lval->unary.value->ir;
      return;
    } else ICE("Unary operator %s is not an lvalue", token_type_to_string(TK_AT));
  } break;

  case NODE_VARIABLE_REFERENCE:
    ASSERT(lval->var->val.node->address,
           "Cannot reference variable that has not yet been emitted.");
    lval->address = lval->var->val.node->address;
    break;

  case NODE_CAST: {
    codegen_lvalue(ctx, lval->cast.value);
    lval->address = lval->cast.value->address;
  } break;

  // TODO: String literals are lvalues...

  /* TODO: references
  case NODE_BLOCK:
  case NODE_CALL:
  case NODE_CAST:
  */
  }
}

/// Emit an expression.
static void codegen_expr(CodegenContext *ctx, Node *expr) {
  if (expr->emitted) return;
  expr->emitted = true;

  STATIC_ASSERT(NODE_COUNT == 19, "Exhaustive handling of node types during code generation (AST->IR).");
  switch (expr->kind) {
  default: ICE("Unrecognized expression kind: %d", expr->kind);

  /// A function node yields its address.
  case NODE_FUNCTION:
      expr->ir = ir_funcref(ctx, expr->function.ir);
      return;

  case NODE_MODULE_REFERENCE:
    ERR("Module reference must not be used unless to access module exports");

  /// Root node.
  case NODE_ROOT: {
    /// Emit everything that isn’t a function.
    foreach_val (child, expr->root.children) {
      if (child->kind == NODE_FUNCTION) continue;
      codegen_expr(ctx, child);
    }

    /// If the last expression doesn’t return anything, return 0.
    if (!ir_is_closed(ctx->block)) ir_return(ctx, vector_back(expr->root.children)->ir);
    return;
  }

  case NODE_DECLARATION:
    codegen_lvalue(ctx, expr);
    return;

  case NODE_MEMBER_ACCESS:
  case NODE_VARIABLE_REFERENCE:
    codegen_lvalue(ctx, expr);
    expr->ir = ir_load(ctx, expr->address);
    return;

  case NODE_STRUCTURE_DECLARATION:
    return;

  /// If expression.
  ///
  /// Each box is a basic block within intermediate representation,
  /// and edges represent control flow from top to bottom.
  ///
  ///      +---------+
  ///      | current |
  ///      +---------+
  ///     /           \
  /// +------+    +------+
  /// | then |    | else |
  /// +------+    +------+
  ///         \  /
  ///       +------+
  ///       | join |
  ///       +------+
  ///
  case NODE_IF: {
    /// Emit the condition.
    codegen_expr(ctx, expr->if_.condition);

    IRBlock *then_block = ir_block_create();
    IRBlock *last_then_block = then_block;
    IRBlock *else_block = ir_block_create();
    IRBlock *last_else_block = else_block;
    IRBlock *join_block = ir_block_create();

    /// Generate the branch.
    ir_branch_conditional(ctx, expr->if_.condition->ir, then_block, else_block);

    /// Emit the then block.
    ir_block_attach(ctx, then_block);
    codegen_expr(ctx, expr->if_.then);

    /// Branch to the join block to skip the else branch.
    last_then_block = ctx->block;
    if (!ir_is_closed(ctx->block)) ir_branch(ctx, join_block);

    /// Generate the else block if there is one.
    ir_block_attach(ctx, else_block);
    if (expr->if_.else_) {
      codegen_expr(ctx, expr->if_.else_);
      last_else_block = ctx->block;
    }

    /// Branch to the join block from the else branch.
    if (!ir_is_closed(ctx->block)) ir_branch(ctx, join_block);

    /// Attach the join block.
    ir_block_attach(ctx, join_block);

    /// Insert a phi node for the result of the if in the join block.
    if (!type_is_void(expr->type)) {
      IRInstruction *phi = ir_phi(ctx, expr->type);
      ir_phi_argument(phi, last_then_block, expr->if_.then->ir);
      ir_phi_argument(phi, last_else_block, expr->if_.else_->ir);
      expr->ir = phi;
    }
    return;
  }

  /// While expression.
  ///
  /// +---------+
  /// | current |
  /// +---------+        ,---------+
  ///      |             |         |
  /// +--------------------+       |
  /// | compute condition  |       |
  /// | conditional branch |       |
  /// +--------------------+       |
  ///      |             |         |
  ///      |      +------------+   |
  ///      |      | body       |   |
  ///      |      +------------+   |
  ///      |             |         |
  ///      |            ...        |
  ///      |             |         |
  ///  +----------+      `---------+
  ///  | join     |
  ///  +----------+
  case NODE_WHILE: {
    IRBlock *while_cond_block = ir_block_create();
    IRBlock *join_block = ir_block_create();

    /// Branch to the new condition block, then attach that as the current block.
    ir_branch(ctx, while_cond_block);
    ir_block_attach(ctx, while_cond_block);

    // Emit condition
    codegen_expr(ctx, expr->while_.condition);

    /// If while body is empty, don't use body block.
    if (expr->while_.body->block.children.size == 0) {
      ir_branch_conditional(ctx, expr->while_.condition->ir, while_cond_block, join_block);
      ir_block_attach(ctx, join_block);
      return;
    }

    /// Otherwise, emit the body of the while loop.
    IRBlock *while_body_block = ir_block_create();
    ir_branch_conditional(ctx, expr->while_.condition->ir, while_body_block, join_block);
    ir_block_attach(ctx, while_body_block);
    codegen_expr(ctx, expr->while_.body);

    /// Emit a branch to the join block and attach the join block.
    if (!ir_is_closed(ctx->block)) ir_branch(ctx, while_cond_block);
    ir_block_attach(ctx, join_block);
    return;
  }

  /// Block expression.
  case NODE_BLOCK: {
    /// Emit everything that isn’t a function.
    Node *last = NULL;
    foreach_val (child, expr->block.children) {
      if (child->kind == NODE_FUNCTION) continue;
      last = child;
      codegen_expr(ctx, child);
    }

    /// The yield of a block is that of its last expression;
    /// If a block doesn’t yield `void`, then it is guaranteed
    /// to not be empty, which is why we don’t check its size here.
    if (!type_is_void(expr->type)) {
      ASSERT(last && last->ir);
      expr->ir = last->ir;
    }
    return;
  }

  /// Function call.
  case NODE_CALL: {
    ASSERT(expr->call.intrinsic == INTRINSIC_COUNT, "Refusing to codegen intrinsic as a regular call");
    IRInstruction *call = NULL;

    /// Direct call.
    if (expr->call.callee->kind == NODE_FUNCTION) {
      call = ir_direct_call(ctx, expr->call.callee->function.ir);
    }

    /// Indirect call.
    else {
      codegen_expr(ctx, expr->call.callee);
      call = ir_indirect_call(ctx, expr->call.callee->ir);
    }

    /// Emit the arguments.
    foreach_val (arg, expr->call.arguments) {
      if (type_is_reference(arg->type)) {
        codegen_lvalue(ctx, arg);
        ir_add_function_call_argument(ctx, call, arg->address);
      }
      else {
        codegen_expr(ctx, arg);
        ir_add_function_call_argument(ctx, call, arg->ir);
      }
    }

    ir_insert(ctx, call);
    expr->ir = call;
    return;
  }

  /// Intrinsic.
  case NODE_INTRINSIC_CALL: {
    ASSERT(expr->call.callee->kind = NODE_FUNCTION_REFERENCE);
    STATIC_ASSERT(INTRINSIC_COUNT == 1, "Handle all intrinsics in codegen");
    switch (expr->call.intrinsic) {
      case INTRINSIC_COUNT: ICE("Call is not an intrinsic");

      /// System call.
      case INTRINSIC_BUILTIN_SYSCALL: {
        /// Syscalls are not a thing on Windows.
        if (ctx->call_convention == CG_CALL_CONV_MSWIN) {
          ERR("Sorry, syscalls are not supported on Windows.");
        }

        expr->ir = ir_intrinsic(ctx, t_integer, expr->call.intrinsic);
        foreach_val (arg, expr->call.arguments) {
          if (type_is_reference(arg->type)) {
            codegen_lvalue(ctx, arg);
            vector_push(expr->ir->call.arguments, arg->address);
          } else {
            codegen_expr(ctx, arg);
            vector_push(expr->ir->call.arguments, arg->ir);
          }
        }

        ir_insert(ctx, expr->ir);
        return;
      }
    }

    UNREACHABLE();
  }

  /// Typecast.
  case NODE_CAST: {
    Type *t_to = expr->type;
    Type *t_from = expr->cast.value->type;

    usz to_sz = type_sizeof(t_to);
    usz from_sz = type_sizeof(t_from);

    bool from_signed = type_is_signed(t_from);

    codegen_expr(ctx, expr->cast.value);

    if (from_sz == to_sz) {
      expr->ir = ir_bitcast(ctx, t_to, expr->cast.value->ir);
      return;
    }
    else if (from_sz < to_sz) {
      // smaller to larger: sign extend if needed, otherwise zero extend.
      if (from_signed)
        expr->ir = ir_sign_extend(ctx, t_to, expr->cast.value->ir);
      else expr->ir = ir_zero_extend(ctx, t_to, expr->cast.value->ir);
      return;
    }
    else if (from_sz > to_sz) {
      // larger to smaller: truncate.
      expr->ir = ir_truncate(ctx, t_to, expr->cast.value->ir);
      return;
    }
    UNREACHABLE();
  } break;

  /// Binary expression.
  case NODE_BINARY: {
    Node *const lhs = expr->binary.lhs;
    Node *const rhs = expr->binary.rhs;

    /// Assignment needs to be handled separately.
    if (expr->binary.op == TK_COLON_EQ) {
      /// Emit the RHS because we need that in any case.
      codegen_expr(ctx, rhs);
      codegen_lvalue(ctx, lhs);
      expr->ir = ir_store(ctx, rhs->ir, lhs->address);
      return;
    }

    if (expr->binary.op == TK_LBRACK) {
      IRInstruction *subs_lhs = NULL;
      Type *reference_stripped_lhs_type = type_strip_references(lhs->type);
      if (!type_is_array(reference_stripped_lhs_type) && !type_is_pointer(reference_stripped_lhs_type))
        ERR("Subscript operator may only operate on arrays and pointers, which type %T is not", lhs->type);

      if (lhs->kind == NODE_VARIABLE_REFERENCE) {
        IRInstruction *var_decl = lhs->var->val.node->address;
        if (var_decl->kind == IR_PARAMETER || var_decl->kind == IR_STATIC_REF || var_decl->kind == IR_ALLOCA)
          if (type_is_pointer(var_decl->type) && type_is_pointer(var_decl->type->pointer.to))
            subs_lhs = ir_load(ctx, var_decl);
          else subs_lhs = var_decl;
        else {
          ir_femit_instruction(stdout, var_decl);
          ERR("Unhandled variable reference IR instruction kind %i aka %s", (int) var_decl->kind, ir_irtype_string(var_decl->kind));
        }
      } else if (is_lvalue(lhs)) {
        codegen_lvalue(ctx, lhs);
        subs_lhs = lhs->address;
      } else if (lhs->kind == NODE_LITERAL && lhs->literal.type == TK_STRING) {
        codegen_expr(ctx, lhs);
        if (rhs->kind == NODE_LITERAL && rhs->literal.type == TK_NUMBER) {
          string str = ctx->ast->strings.data[lhs->literal.string_index];
          if (rhs->literal.integer >= str.size) {
            ERR("Out of bounds: subscript %U too large for string literal.", rhs->literal.integer);
          }
          if (rhs->literal.integer)
            expr->ir = ir_add(ctx, lhs->ir, ir_immediate(ctx, t_integer, rhs->literal.integer));
          else expr->ir = lhs->ir;
          return;
        }
        subs_lhs = lhs->ir;
      }
      else ERR("LHS of subscript operator has invalid kind %i", (int) lhs->kind);

      // Subscript of array should result in pointer to base type, not pointer to array type.
      if ((type_is_pointer(subs_lhs->type) || type_is_reference(subs_lhs->type)) && type_is_array(subs_lhs->type->pointer.to)) {
        subs_lhs = ir_copy(ctx, subs_lhs);
        subs_lhs->type = ast_make_type_pointer(ctx->ast, subs_lhs->type->source_location,
                                               subs_lhs->type->pointer.to->array.of);
        ir_insert(ctx, subs_lhs);
      }

      if (rhs->kind == NODE_LITERAL && rhs->literal.type == TK_NUMBER && rhs->literal.integer == 0) {
        expr->ir = subs_lhs;
        return;
      }

      codegen_expr(ctx, rhs);

      IRInstruction *scaled_rhs = NULL;
      // An array subscript needs multiplied by the sizeof the array's base type.
      if (type_is_array(reference_stripped_lhs_type)) {
        IRInstruction *immediate = ir_immediate(ctx, t_integer, type_sizeof(reference_stripped_lhs_type->array.of));
        scaled_rhs = ir_mul(ctx, rhs->ir, immediate);
      }
      // A pointer subscript needs multiplied by the sizeof the pointer's base type.
      else if (type_is_pointer(reference_stripped_lhs_type)) {
        IRInstruction *immediate = ir_immediate(ctx, t_integer, type_sizeof(reference_stripped_lhs_type->pointer.to));
        scaled_rhs = ir_mul(ctx, rhs->ir, immediate);
      }
      expr->ir = ir_add(ctx, subs_lhs, scaled_rhs);
      return;
    }

    /// Emit the operands.
    codegen_expr(ctx, lhs);
    codegen_expr(ctx, rhs);

    /// Emit the binary instruction.
    switch (expr->binary.op) {
      default: ICE("Cannot emit binary expression of type %d", expr->binary.op);
      case TK_LBRACK: UNREACHABLE();
      case TK_LT: expr->ir = ir_lt(ctx, lhs->ir, rhs->ir); return;
      case TK_LE: expr->ir = ir_le(ctx, lhs->ir, rhs->ir); return;
      case TK_GT: expr->ir = ir_gt(ctx, lhs->ir, rhs->ir); return;
      case TK_GE: expr->ir = ir_ge(ctx, lhs->ir, rhs->ir); return;
      case TK_EQ: expr->ir = ir_eq(ctx, lhs->ir, rhs->ir); return;
      case TK_NE: expr->ir = ir_ne(ctx, lhs->ir, rhs->ir); return;
      case TK_PLUS: expr->ir = ir_add(ctx, lhs->ir, rhs->ir); return;
      case TK_MINUS: expr->ir = ir_sub(ctx, lhs->ir, rhs->ir); return;
      case TK_STAR: expr->ir = ir_mul(ctx, lhs->ir, rhs->ir); return;
      case TK_SLASH: expr->ir = ir_div(ctx, lhs->ir, rhs->ir); return;
      case TK_PERCENT: expr->ir = ir_mod(ctx, lhs->ir, rhs->ir); return;
      case TK_SHL: expr->ir = ir_shl(ctx, lhs->ir, rhs->ir); return;
      case TK_SHR: expr->ir = ir_sar(ctx, lhs->ir, rhs->ir); return;
      case TK_AMPERSAND: expr->ir = ir_and(ctx, lhs->ir, rhs->ir); return;
      case TK_PIPE: expr->ir = ir_or(ctx, lhs->ir, rhs->ir); return;
    }
  }

  /// Unary expression.
  case NODE_UNARY: {
    /// Addressof expressions are special because we don’t emit their operand.
    if (expr->unary.op == TK_AMPERSAND && !expr->unary.postfix) {
      if (expr->literal.type == TK_STRING) {
        TODO("IR code generation of addressof string literal");
      } else {
        codegen_lvalue(ctx, expr->unary.value);
        expr->ir = expr->unary.value->address;
      }
      return;
    }

    /// Emit the operand.
    codegen_expr(ctx, expr->unary.value);

    /// Prefix expressions.
    if (!expr->unary.postfix) {
      switch (expr->unary.op) {
      default: ICE("Cannot emit unary prefix expression of token type %s", token_type_to_string(expr->unary.op));

        /// Load a value from a pointer.
        case TK_AT:
          if (expr->unary.value->type->kind == TYPE_POINTER && expr->unary.value->type->pointer.to->kind == TYPE_FUNCTION)
            expr->ir = expr->unary.value->ir;
          else expr->ir = ir_load(ctx, expr->unary.value->ir);
          return;

        /// One’s complement negation.
        case TK_TILDE: expr->ir = ir_not(ctx, expr->unary.value->ir); return;
      }
    }

    /// Postfix expressions.
    else {
      switch (expr->unary.op) {
        default: ICE("Cannot emit unary postfix expression of type %d", expr->unary.op);
      }
    }
  }

  /// Literal expression. Only integer literals are supported for now.
  case NODE_LITERAL: {
    switch (expr->literal.type) {

    case TK_NUMBER: {
      expr->ir = ir_immediate(ctx, expr->type, expr->literal.integer);
    } break;

    case TK_STRING: {

      // FIXME: This name shouldn't be needed here, but static
      // variables are required to have names as of right now. We
      // should really have it so that the backend can gracefully
      // handle empty string for static names, and it will
      // automatically generate one (i.e. exactly what we do here).
      char buf[48] = {0};
      static size_t string_literal_count = 0;
      snprintf(buf, 48, "__str_lit%zu", string_literal_count++);

      expr->ir = ir_create_static(ctx, expr, expr->type, as_span(string_create(buf)));
      // Set static initialiser so backend will properly fill in data from string literal.
      expr->ir->static_ref->init = ir_get_literal_string(ctx, expr->literal.string_index);

    } break;

    // Array
    case TK_LBRACK: {
      expr->ir = ir_stack_allocate(ctx, expr->type);

      // Emit a store from each expression in the initialiser as an element in the array.
      IRInstruction *address = ir_copy(ctx, expr->ir);
      address->type = ast_make_type_pointer(ctx->ast, expr->source_location, expr->type->array.of);
      ir_insert(ctx, address);
      usz index = 0;
      foreach_val (node, expr->literal.compound) {
        codegen_expr(ctx, node);
        ir_store(ctx, node->ir, address);
        if (index == expr->literal.compound.size - 1) break;
        // Iterate address
        IRInstruction *element_byte_size = ir_immediate(ctx, t_integer, type_sizeof(expr->type->array.of));
        address = ir_add(ctx, address, element_byte_size);
        ++index;
      }
      expr->ir = ir_load(ctx, expr->ir);
    } break;

    default:
      DIAG(DIAG_SORRY, expr->source_location, "Emitting literals of type %T not supported", expr->type);
    }
  } return;

  case NODE_FOR: {
    /* FOR INIT COND ITER BODY
     *
     * +------------------+
     * | current          |
     * | emit initialiser |
     * +------------------+
     *      |
     *      |             ,-------------+
     *      |             |             |
     * +--------------------+           |
     * | conditional branch |           |
     * +--------------------+           |
     *      |             |             |
     *      |      +----------------+   |
     *      |      | body           |   |
     *      |      | emit iterator  |   |
     *      |      +----------------+   |
     *      |             |             |
     *      |            ...            |
     *      |             |             |
     *  +----------+      `-------------+
     *  | join     |
     *  +----------+
     *
     */

    IRBlock *cond_block = ir_block_create();
    IRBlock *body_block = ir_block_create();
    IRBlock *join_block = ir_block_create();

    codegen_expr(ctx, expr->for_.init);
    ir_branch(ctx, cond_block);

    ir_block_attach(ctx, cond_block);
    codegen_expr(ctx, expr->for_.condition);
    ir_branch_conditional(ctx, expr->for_.condition->ir, body_block, join_block);

    ir_block_attach(ctx, body_block);
    codegen_expr(ctx, expr->for_.body);
    codegen_expr(ctx, expr->for_.iterator);
    ir_branch(ctx, cond_block);

    ir_block_attach(ctx, join_block);

    return;
  }

  case NODE_RETURN: {
    if (expr->return_.value) codegen_expr(ctx, expr->return_.value);
    expr->ir = ir_return(ctx, expr->return_.value ? expr->return_.value->ir : NULL);
    return;
  }

  /// Function reference. These should have all been removed by the semantic analyser.
  case NODE_FUNCTION_REFERENCE: UNREACHABLE();
  }
}

/// Emit a function.
void codegen_function(CodegenContext *ctx, Node *node) {
  ctx->block = node->function.ir->blocks.first;
  ctx->function = node->function.ir;

  /// Next, emit all parameter declarations and store
  /// the initial parameter values in them.
  // TODO: Make this backend dependent?
  foreach_index(i, node->function.param_decls) {
    Node *decl = node->function.param_decls.data[i];
    IRInstruction *p = ir_parameter(ctx, i);
    if (type_is_reference(decl->type))
      decl->address = p;
    else if (parameter_is_passed_as_pointer(ctx, ctx->function, i)) {
      p->type = ast_make_type_pointer(ctx->ast, p->type->source_location, p->type);
      decl->address = p;
    } else {
      /// Allocate a variable for the parameter.
      codegen_lvalue(ctx, decl);
      /// Store the parameter value in the variable.
      ir_store(ctx, p, decl->address);
    }
  }

  /// Emit the function body.
  codegen_expr(ctx, node->function.body);

  /// If we can return from here, and this function doesn’t return void,
  /// then return the return value; otherwise, just return nothing.
  if (!ir_is_closed(ctx->block)) {
    ir_return(ctx, !type_is_void(node->type->function.return_type)
        ? node->function.body->ir
        : NULL);
  }
}

/// ===========================================================================
///  Driver
/// ===========================================================================
void codegen_lower(CodegenContext *context) {
  STATIC_ASSERT(ARCH_COUNT == 2, "Exhaustive handling of architectures");
  switch (context->arch) {
    case ARCH_X86_64:
      codegen_lower_x86_64(context);
      break;
    default:
      TODO("Handle %d code generation architecture.", context->arch);
  }
}

void codegen_emit(CodegenContext *context) {
  STATIC_ASSERT(ARCH_COUNT == 2, "Exhaustive handling of architectures");

  if (context->target == TARGET_LLVM) {
    codegen_emit_llvm(context);
    return;
  }

  switch (context->arch) {
  case ARCH_X86_64: {
    codegen_emit_x86_64(context);
  } break;
  case ARCH_NONE: FALLTHROUGH;
  case ARCH_COUNT: UNREACHABLE();
  }
}

bool codegen
(CodegenLanguage lang,
 CodegenArchitecture arch,
 CodegenTarget target,
 CodegenCallingConvention call_convention,
 const char *infile,
 const char *outfile,
 AST *ast,
 string ir
 )
{
  if (!outfile) ICE("codegen(): outfile can not be NULL!");
  // Open file for writing.
  FILE *code = fopen(outfile, "wb");
  if (!code) ICE("codegen(): failed to open file at path: \"%s\"\n", outfile);

  CodegenContext *context = codegen_context_create(ast, arch, target, call_convention, code);

  switch (lang) {
    /// Parse an IR file.
    case LANG_IR: {
        if (!ir_parse(context, infile, ir)) {
          fclose(code);
          return false;
        }
    } break;

    /// Codegen an Intercept program.
    case LANG_FUN: {
      if (!ast->is_module) {
        /// Create the main function.
        Type* c_int = ast_make_type_integer(ast, (loc){0}, true, context->ffi.cint_size);
        Parameter argc =  {
          .name = string_create("__argc__"),
          .type = c_int,
          .source_location = {0},
        };
        Parameter argv =  {
          .name = string_create("__argv__"),
          .type = ast_make_type_pointer(ast, (loc){0}, ast_make_type_pointer(ast, (loc){0}, t_byte)),
          .source_location = {0},
        };
        Parameter envp =  {
          .name = string_create("__envp__"),
          .type = ast_make_type_pointer(ast, (loc){0}, ast_make_type_pointer(ast, (loc){0}, t_byte)),
          .source_location = {0},
        };

        Parameters main_params = {0};
        vector_push(main_params, argc);
        vector_push(main_params, argv);
        vector_push(main_params, envp);
        // TODO: envp?

        /// FIXME: return type should be int as well, but that currently breaks the x86_64 backend.
        Type *main_type = ast_make_type_function(context->ast, (loc){0}, t_integer, main_params);
        context->entry = ir_function(context, literal_span("main"), main_type);
        context->entry->attr_global = true;
        context->entry->attr_nomangle = true;
      } else {
        Parameters entry_params = {0};
        Type *entry_type = ast_make_type_function(context->ast, (loc){0}, t_void, entry_params);
        string_buffer name = {0};
        format_to(&name, "__module%S_entry", context->ast->module_name);
        context->entry = ir_function(context, as_span(name), entry_type);
        context->entry->attr_global = true;
        context->entry->attr_nomangle = true;
        vector_delete(name);
      }

      /// Create the remaining functions and set the address of each function.
      foreach_val (func, ast->functions) {
        func->function.ir = ir_function(context, as_span(func->function.name), func->type);
        func->function.ir->source_location = func->source_location;

        /// Mark the function as extern if it is.
        if (!func->function.body) func->function.ir->is_extern = true;

        /// Handle attributes
        STATIC_ASSERT(ATTR_COUNT == 4, "Exhaustive handling of function attributes");
        func->function.ir->attr_global = func->type->function.global;
        func->function.ir->attr_nomangle = func->type->function.nomangle;
        func->function.ir->attr_forceinline = func->type->function.alwaysinline;
        // TODO: Should we propagate "discardable" to the IR?
      }

      foreach_val (import, context->ast->imports) {
        foreach_val (n, import->exports) {
          // Declarations need an IR instruction generated, just to match how we
          // deal with regular declarations.
          if (n->kind == NODE_DECLARATION) codegen_lvalue(context, n);
        }
      }

      /// Emit the main function.
      context->block = context->entry->blocks.first;
      context->function = context->entry;
      codegen_expr(context, ast->root);

      /// Emit the remaining functions that aren’t extern.
      foreach_val (func, ast->functions) {
        if (!func->function.body) continue;
        codegen_function(context, func);
      }
    } break;

    /// Anything else is not supported.
    default: ICE("Language %d not supported.", lang);
  }

  /// Don’t codegen a faulty program.
  if (context->has_err) return false;

  /// Perform mandatory inlining.
  if (!codegen_process_inline_calls(context)) return false;

  if (debug_ir || print_ir2) {
    ir_femit(stdout, context);
    if (print_ir2) exit(42);
  }

  if (optimise) {
    codegen_optimise(context);
    if (debug_ir)
      ir_femit(stdout, context);
  }

  /// No need to lower anything if we’re emitting LLVM IR.
  if (target != TARGET_LLVM) {
    codegen_lower(context);

    if (debug_ir) {
      print("After lowering:\n");
      ir_femit(stdout, context);
    }
  }

  codegen_emit(context);

  codegen_context_free(context);

  fclose(code);
  return true;
}


static void mangle_type_to(string_buffer *buf, Type *t) {
  ASSERT(t);
  switch (t->kind) {
    default: TODO("Handle type kind %d in type mangling!", (int)t->kind);

    case TYPE_STRUCT:
      if (t->structure.decl->struct_decl->name.size)
        format_to(buf, "%Z%S", t->structure.decl->struct_decl->name.size, t->structure.decl->struct_decl->name);
      else {
        static usz struct_count = 0;
        format_to(buf, "%Z%Z", number_width(struct_count), struct_count);
        ++struct_count;
      }
      break;

    case TYPE_PRIMITIVE:
      format_to(buf, "%Z%S", t->primitive.name.size, t->primitive.name);
      break;

    case TYPE_NAMED:
      if (!t->named->val.type) format_to(buf, "%Z%S", t->named->name.size, t->named->name);
      else mangle_type_to(buf, t->named->val.type);
      break;

    case TYPE_INTEGER: {
      usz length = 1 + number_width(t->integer.bit_width);
      format_to(buf, "%Z%c%Z", length, t->integer.is_signed ? 's' : 'u', t->integer.bit_width);
    } break;

    case TYPE_POINTER:
      format_to(buf, "P");
      mangle_type_to(buf, t->pointer.to);
      break;

    case TYPE_REFERENCE:
      format_to(buf, "R");
      mangle_type_to(buf, t->reference.to);
      break;

    case TYPE_ARRAY:
      format_to(buf, "A%ZE", t->array.size);
      mangle_type_to(buf, t->array.of);
      break;

    case TYPE_FUNCTION:
      format_to(buf, "F");
      mangle_type_to(buf, t->function.return_type);
      foreach (param, t->function.parameters) mangle_type_to(buf, param->type);
      format_to(buf, "E");
      break;
  }
}

void mangle_function_name(IRFunction *function) {
  if (function->attr_nomangle) return;

  string_buffer buf = {0};
  format_to(&buf, "_XF%Z%S", function->name.size, function->name);
  mangle_type_to(&buf, function->type);
  free(function->name.data);
  function->name = (string){buf.data, buf.size};
}
