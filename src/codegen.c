#include <codegen.h>

#include <codegen/codegen_forward.h>
#include <codegen/intermediate_representation.h>
#include <codegen/x86_64/arch_x86_64.h>
#include <codegen/ir/ir.h>
#include <error.h>
#include <ir_parser.h>
#include <opt.h>
#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>

#define DIAG(sev, loc, ...)                                                                                 \
  do {                                                                                                      \
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
 enum CodegenOutputFormat format,
 enum CodegenCallingConvention call_convention,
 enum CodegenAssemblyDialect dialect,
 FILE* code
 )
{
  CodegenContext *context;

  STATIC_ASSERT(CG_FMT_COUNT == 2, "codegen_context_create_top_level() must exhaustively handle all codegen output formats.");
  STATIC_ASSERT(CG_CALL_CONV_COUNT == 2, "codegen_context_create_top_level() must exhaustively handle all calling conventions.");
  switch (format) {
    case CG_FMT_x86_64_GAS:
      // TODO: Handle call_convention for creating codegen context!
      if (call_convention == CG_CALL_CONV_MSWIN) {
        context = codegen_context_x86_64_mswin_create();
      } else if (call_convention == CG_CALL_CONV_LINUX) {
        context = codegen_context_x86_64_linux_create();
      } else {
        ICE("Unrecognized calling convention!");
      }
      break;
    case CG_FMT_IR:
      context = codegen_context_ir_create();
      break;
    default: UNREACHABLE();
  }

  context->ast = ast;
  context->code = code;
  context->dialect = dialect;
  return context;
}

void codegen_context_free(CodegenContext *context) {
  STATIC_ASSERT(CG_FMT_COUNT == 2, "codegen_context_free() must exhaustively handle all codegen output formats.");
  STATIC_ASSERT(CG_CALL_CONV_COUNT == 2, "codegen_context_free() must exhaustively handle all calling conventions.");

  /// Free all IR Functions.
  foreach_ptr (IRFunction *, f, context->functions) {
    /// Free each block.
    list_foreach (IRBlock*, b, f->blocks) {
      /// Free each instruction.
      list_foreach (IRInstruction *, i, b->instructions) ir_free_instruction_data(i);
      list_delete(IRInstruction, b->instructions);

      /// Free the block name.
      free(b->name.data);
    }

    /// Free the name, params, and block list.
    free(f->name.data);
    vector_delete(f->parameters);
    list_delete(IRBlock, f->blocks);

    /// Free the function itself.
    free(f);
  }

  /// Finally, delete the function vector.
  vector_delete(context->functions);

  /// Free static variables.
  foreach_ptr (IRStaticVariable*, var, context->static_vars) {
    free(var->name.data);
    free(var);
  }
  vector_delete(context->static_vars);

  /// Free parameter instructions that were removed, but not freed.
  foreach_ptr (IRInstruction*, i, context->removed_instructions) {
    ir_free_instruction_data(i);
    free(i);
  }
  vector_delete(context->removed_instructions);

  /// Free backend-specific data.
  switch (context->format) {
    default: UNREACHABLE();

    case CG_FMT_x86_64_GAS:
      if (context->call_convention == CG_CALL_CONV_MSWIN) codegen_context_x86_64_mswin_free(context);
      else if (context->call_convention == CG_CALL_CONV_LINUX) codegen_context_x86_64_linux_free(context);
      else ICE("Unrecognized calling convention!");
      break;

    case CG_FMT_IR:
      codegen_context_ir_free(context);
      break;
  }

  /// Free the context itself.
  free(context);
}

/// ===========================================================================
///  Code generation.
/// ===========================================================================
/// Emit an expression.
static void codegen_expr(CodegenContext *ctx, Node *expr) {
  if (expr->emitted) return;
  expr->emitted = true;

  switch (expr->kind) {
  default: ICE("Unrecognized expression kind: %d", expr->kind);

  /// A function node yields its address.
  case NODE_FUNCTION:
      expr->ir = ir_funcref(ctx, expr->function.ir);
      return;

  /// Root node.
  case NODE_ROOT: {
    /// Emit everything that isn’t a function.
    foreach_ptr (Node *, child, expr->root.children) {
      if (child->kind == NODE_FUNCTION) continue;
      codegen_expr(ctx, child);
    }

    /// If the last expression doesn’t return anything, return 0.
    if (!ir_is_closed(ctx->block)) ir_return(ctx, vector_back(expr->root.children)->ir);
    return;
  }

  /// Variable declaration.
  case NODE_DECLARATION:
      expr->ir = expr->declaration.static_
        ? ir_create_static(ctx, expr->type, as_span(expr->declaration.name))
        : ir_stack_allocate(ctx, type_sizeof(expr->type));

      /// Emit the initialiser if there is one.
      if (expr->declaration.init) {
        codegen_expr(ctx, expr->declaration.init);
        ir_store(ctx, expr->declaration.init->ir, expr->ir);
      }
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
      IRInstruction *phi = ir_phi(ctx);
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

      /// Branch to the condition block and emit the condition.
      ir_branch(ctx, while_cond_block);
      ir_block_attach(ctx, while_cond_block);
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
      foreach_ptr (Node *, child, expr->block.children) {
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
    foreach_ptr (Node*, arg, expr->call.arguments) {
      codegen_expr(ctx, arg);
      ir_add_function_call_argument(ctx, call, arg->ir);
    }

    ir_insert(ctx, call);
    expr->ir = call;
    return;
  }

  /// Typecast.
  case NODE_CAST: { TODO(); }

  /// Binary expression.
  case NODE_BINARY: {
    Node * const lhs = expr->binary.lhs, * const rhs = expr->binary.rhs;

    /// Assignment needs to be handled separately.
    if (expr->binary.op == TK_COLON_EQ) {
      /// Emit the RHS because we need that in any case.
      codegen_expr(ctx, rhs);

      /// If the LHS is a variable, just emit a direct store to the memory address,
      /// which we can get from the declaration which has to have been emitted already.
      if (lhs->kind == NODE_VARIABLE_REFERENCE) {
        expr->ir = ir_store(ctx, rhs->ir, lhs->var->val.node->ir);
        return;
      }

      /// If the LHS is a pointer dereference, emit an indirect store to the argument.
      if (lhs->kind == NODE_UNARY && lhs->unary.op == TK_AT) {
        codegen_expr(ctx, lhs->unary.value);
        expr->ir = ir_store(ctx, rhs->ir, lhs->unary.value->ir);
        return;
      }

      /// Anything else is an error (I think?).
      /// Otherwise, actually emit the LHS and load from that.
      /*codegen_expr(ctx, lhs);
      expr->ir = ir_store(ctx, lhs->ir, rhs->ir);*/
      return;
    }

    /// Emit the operands.
    codegen_expr(ctx, lhs);
    codegen_expr(ctx, rhs);

    /// Emit the binary instruction.
    switch (expr->binary.op) {
      default: ICE("Cannot emit binary expression of type %d", expr->binary.op);
      case TK_LBRACK: expr->ir = ir_add(ctx, lhs->ir, rhs->ir); return;
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
      switch (expr->unary.value->kind) {
        case NODE_DECLARATION: expr->ir = expr->unary.value->ir; return;
        case NODE_VARIABLE_REFERENCE: expr->ir = expr->unary.value->var->val.node->ir; return;
        default: ICE("Cannot take address of expression of type %d", expr->unary.value->kind);
      }
    }

    /// Emit the operand.
    codegen_expr(ctx, expr->unary.value);

    /// Prefix expressions.
    if (!expr->unary.postfix) {
      switch (expr->unary.op) {
        default: ICE("Cannot emit unary prefix expression of type %d", expr->unary.op);

        /// Load a value from an lvalue.
        /// Emitting an lvalue loads it, so we don’t need to do anything here.
        case TK_AT:
          /// TODO: This check for a function pointer is a bit sus. We shouldn’t
          ///       even get here if this is actually a function pointer...
          if (expr->unary.value->type->pointer.to->kind == TYPE_FUNCTION) {
            expr->ir = expr->unary.value->ir;
          } else {
            expr->ir = ir_load(ctx, expr->unary.value->ir);
          }
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
  case NODE_LITERAL:
    if (expr->literal.type != TK_NUMBER) DIAG(DIAG_SORRY, expr->source_location, "Emitting non-integer literals not supported");
    expr->ir = ir_immediate(ctx, expr->literal.integer);
    return;

  /// Variable reference.
  case NODE_VARIABLE_REFERENCE:
    expr->ir = ir_load(ctx, expr->var->val.node->ir);
    return;

  /// Function reference. These should have all been removed by the semantic analyser.
  case NODE_FUNCTION_REFERENCE: UNREACHABLE();
  }
}

/// Emit a function.
void codegen_function(CodegenContext *ctx, Node *node) {
  ctx->block = node->function.ir->blocks.first;
  ctx->function = node->function.ir;

  /// First, emit all parameter declarations and store
  /// the initial parameter values in them.
  foreach_index(i, node->function.param_decls) {
    /// Allocate a variable for the parameter.
    Node *decl = node->function.param_decls.data[i];
    codegen_expr(ctx, decl);

    /// Store the parameter value in the variable.
    IRInstruction *p = ir_parameter(ctx, i);
    ir_store(ctx, p, decl->ir);
  }

  /// Emit the function body.
  codegen_expr(ctx, node->function.body);

  /// If the we can return from here, and this function doesn’t return void,
  /// then return the return value; otherwise, just return nothing.
  if (!ir_is_closed(ctx->block) && !type_is_void(node->type->function.return_type)) {
    ir_return(ctx, node->function.body->ir);
  } else {
    ir_return(ctx, NULL);
  }
}

/// ===========================================================================
///  Driver
/// ===========================================================================
void codegen_lower(CodegenContext *context) {
  switch (context->format) {
    case CG_FMT_x86_64_GAS:
      codegen_lower_x86_64(context);
      break;
    case CG_FMT_IR:
      codegen_lower_ir_backend(context);
      break;
    default:
      TODO("Handle %d code generation format.", context->format);
  }
}

void codegen_emit(CodegenContext *context) {
  switch (context->format) {
    case CG_FMT_x86_64_GAS:
      codegen_emit_x86_64(context);
      break;
    case CG_FMT_IR:
      codegen_emit_ir_backend(context);
      break;
    default:
      TODO("Handle %d code generation format.", context->format);
  }
}

bool codegen
(enum CodegenLanguage lang,
 enum CodegenOutputFormat format,
 enum CodegenCallingConvention call_convention,
 enum CodegenAssemblyDialect dialect,
 const char *infile,
 const char *outfile,
 AST *ast,
 string ir
 )
{
  if (!outfile) ICE("codegen(): outfile can not be NULL!");
  // Open file for writing.
  FILE *code = fopen(outfile, "w");
  if (!code) ICE("codegen(): failed to open file at path: \"%s\"\n", outfile);

  CodegenContext *context = codegen_context_create(ast, format, call_convention, dialect, code);
  switch (lang) {
    /// Parse an IR file.
    case LANG_IR: {
        if (!ir_parse(context, infile, ir)) {
          fclose(code);
          return false;
        }
    } break;

    /// Codegen a FUN program.
    case LANG_FUN: {
      /// Create the main function.
      Parameter argc =  {
        .name = string_create("__argc__"),
        .type = t_integer,
      };
      Parameter argv =  {
        .name = string_create("__argv__"),
        .type = ast_make_type_pointer(ast, (loc){0}, ast_make_type_pointer(ast, (loc){0}, t_integer)),
      };

      Parameters main_params = {0};
      vector_push(main_params, argc);
      vector_push(main_params, argv);

      Type *main_type = ast_make_type_function(context->ast, (loc){0}, t_integer, main_params);
      context->entry = ir_function(context, literal_span("main"), main_type);
      context->entry->attr_global = true;

      /// Create the remaining functions and set the address of each function.
      foreach_ptr (Node*, func, ast->functions) {
          func->function.ir = ir_function(context, as_span(func->function.name),
            func->type);

          /// Mark the function as extern if it is.
          if (!func->function.body) func->function.ir->is_extern = true;

          /// Mark the function as global if it is global.
          if (func->function.global) func->function.ir->attr_global = true;
      }

      /// Emit the main function.
      context->block = context->entry->blocks.first;
      context->function = context->entry;
      codegen_expr(context, ast->root);

      /// Emit the remaining functions that aren’t extern.
      foreach_ptr (Node*, func, ast->functions) {
        if (!func->function.body) continue;
        codegen_function(context, func);
      }
    } break;

    /// Anything else is not supported.
    default: ICE("Language %d not supported.", lang);
  }

  if (optimise) codegen_optimise(context);

  if (debug_ir && codegen_only) {
    ir_femit(stdout, context);
    exit(0);
  }

  codegen_lower(context);

  codegen_emit(context);

  codegen_context_free(context);

  fclose(code);
  return true;
}
