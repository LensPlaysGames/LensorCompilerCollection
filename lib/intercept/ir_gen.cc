#include <intercept/ir_gen.hh>

#include <intercept/ast.hh>
#include <lcc/core.hh>
#include <lcc/ir/ir.hh>
#include <lcc/ir/type.hh>
#include <lcc/ir/printer.hh>
#include <lcc/ir/module.hh>
#include <lcc/context.hh>
#include <lcc/utils.hh>
#include <lcc/utils/rtti.hh>

#include <memory>
#include <vector>
#include <unordered_map>

namespace lcc {

namespace intercept {
void IRGen::insert(lcc::Inst* inst) {
    LCC_ASSERT(inst, "Invalid argument");
    LCC_ASSERT(ctx, "Invalid context");
    LCC_ASSERT(function && block, "Invalid insert point");
    block->insert(inst);
}

lcc::Type* Convert(Context* ctx, Type* in) {
    switch (in->kind()) {
    case Type::Kind::Builtin: {
        switch ((as<BuiltinType>(in))->builtin_kind()) {
        case BuiltinType::BuiltinKind::Bool:
            return lcc::Type::I1Ty;
        case BuiltinType::BuiltinKind::Byte:
        case BuiltinType::BuiltinKind::Int:
            return lcc::IntegerType::Get(ctx, in->size(ctx));
        case BuiltinType::BuiltinKind::Void:
            return lcc::Type::VoidTy;
        case BuiltinType::BuiltinKind::OverloadSet:
        case BuiltinType::BuiltinKind::Unknown:
            Diag::ICE("Invalid builtin kind present during IR generation");
        }
        LCC_UNREACHABLE();
    } break;
    case Type::Kind::FFIType: {
        return lcc::IntegerType::Get(ctx, in->size(ctx));
    } break;
    case Type::Kind::Named: {
        Diag::ICE("Sema failed to resolve named type");
    } break;
    case Type::Kind::Pointer:
    case Type::Kind::Reference: {
        return lcc::Type::PtrTy;
    } break;
    case Type::Kind::Array: {
        const auto& t_array = as<ArrayType>(in);
        return lcc::ArrayType::Get(ctx, t_array->dimension(), Convert(ctx, t_array->element_type()));
    } break;
    case Type::Kind::Function: {
        const auto& t_function = as<FuncType>(in);

        std::vector<lcc::Type*> param_types{};
        for (const auto& p : t_function->params())
            param_types.push_back(Convert(ctx, p.type));

        return lcc::FunctionType::Get(ctx, Convert(ctx, t_function->return_type()), std::move(param_types));
    } break;
    case Type::Kind::Struct: {
        std::vector<lcc::Type*> member_types{};
        for (const auto& m : as<StructType>(in)->members())
            member_types.push_back(Convert(ctx, m.type));

        return lcc::StructType::Get(ctx, std::move(member_types));
    } break;
    case Type::Kind::Integer: {
        return lcc::IntegerType::Get(ctx, in->size(ctx));
    } break;
    }
    LCC_UNREACHABLE();
}

void intercept::IRGen::create_function(intercept::FuncDecl* f) {
    // FIXME: CallConv::Intercept shouldn't be hard-coded.
    generated_ir[f] = new (*module) Function (ctx, f->mangled_name(),
                                              as<FunctionType>(Convert(ctx, f->type())),
                                              f->linkage(),
                                              CallConv::Intercept,
                                              f->location());
}

void intercept::IRGen::generate_lvalue(intercept::Expr* expr) {
    switch (expr->kind()) {
    case Expr::Kind::Call: {
        LCC_ASSERT(false, "TODO: Calls may be an lvalue depending on return type of callee; irgen is not done yet, sorry");
    } break;

    case Expr::Kind::If: {
        LCC_ASSERT(false, "TODO: If expressions may be an lvalue depending on return type of callee; irgen is not done yet, sorry");
    } break;

    case Expr::Kind::NameRef:
    case Expr::Kind::StringLiteral: {
        generate_expression(expr);
    } break;

    // TODO: More lvalue things

    default: LCC_ASSERT(false, "Unhandled expression kind in lvalue codegen, sorry");

    }
}

/// NOTE: If you `new` an /instruction/ (of, or derived from, type `Inst`), you need to `insert()` it.
void intercept::IRGen::generate_expression(intercept::Expr* expr) {
    // Already generated
    if (generated_ir[expr]) return;

    // If we call `generate_expression` on an expression that hasn't already
    // been generated, *and* the block we would be trying to insert to is
    // closed, create a new block to insert into.
    if (block->closed()) {
        // TODO: Unique block name, or something.
        update_block(new (*module) lcc::Block("body.more"));
    }

    switch (expr->kind()) {
    case intercept::Expr::Kind::Block: {
        for (auto e : as<BlockExpr>(expr)->children()) generate_expression(e);
    } break;

    // Will be inlined anywhere it is used; a no-op for actual generation.
    case intercept::Expr::Kind::IntegerLiteral: {
        auto* literal = new (*module) lcc::IntegerConstant(Convert(ctx, expr->type()), as<IntegerLiteral>(expr)->value());
        generated_ir[expr] = literal;
    } break;

    case intercept::Expr::Kind::VarDecl: {
        const auto& decl = as<VarDecl>(expr);
        switch (decl->linkage()) {
        case Linkage::LocalVar: {
            auto* alloca = new (*module) AllocaInst(Convert(ctx, decl->type()), expr->location());
            insert(alloca);
            if (auto* init_expr = decl->init()) {
                generate_expression(init_expr);
                // Store generated init_expr into above inserted declaration
                auto* local_init = new (*module) StoreInst(generated_ir[init_expr], alloca, expr->location());
                insert(local_init);
            }
            generated_ir[expr] = alloca;
        } break;

        case Linkage::Imported:
        case Linkage::Reexported: {
            auto* alloca = new (*module) AllocaInst(Convert(ctx, decl->type()), expr->location());
            insert(alloca);
            generated_ir[expr] = alloca;
        } break;

        case Linkage::Internal:
        case Linkage::Used:
        case Linkage::Exported: {
            auto* global = new (*module) GlobalVariable(Convert(ctx, decl->type()), decl->name(), decl->linkage(), nullptr);
            generated_ir[expr] = global;
        } break;

        default:
            fmt::print("Unhandled VarDecl linkage {} (WIP)\n", (int)decl->linkage());
            break;
        }

    } break;

    case Expr::Kind::Unary: {
        const auto& unary_expr = as<UnaryExpr>(expr);

        generate_expression(unary_expr->operand());

        switch (unary_expr->op()) {
        case TokenKind::Minus: {
            generated_ir[expr] = new (*module) NegInst(generated_ir[unary_expr->operand()], expr->location());
        } break;
        default: LCC_ASSERT(false, "Sorry, but IRGen of unary operator {} has, apparently, not been implemented. Sorry about that.", ToString(unary_expr->op()));
        }

        insert(as<Inst>(generated_ir[expr]));
    } break;

    case intercept::Expr::Kind::Binary: {
        const auto& binary_expr = as<BinaryExpr>(expr);
        const auto& lhs_expr = binary_expr->lhs();
        const auto& rhs_expr = binary_expr->rhs();

        // Assignment
        if (binary_expr->op() == TokenKind::ColonEq) {
            generate_lvalue(lhs_expr);
            auto* lhs = generated_ir[lhs_expr];

            generate_expression(rhs_expr);
            auto* rhs = generated_ir[rhs_expr];

            auto* store = new (*module) StoreInst(rhs, lhs, expr->location());

            generated_ir[expr] = store;
            insert(store);

            break;
        }

        // Subscript
        if (binary_expr->op() == TokenKind::LBrack) {
            generate_lvalue(lhs_expr);
            Value* lhs = generated_ir[lhs_expr];

            if (!lhs) LCC_ASSERT(false, "lvalue codegen for lhs of subscript didn't go as expected; sorry");

            if (/* TODO: ctx->optimise && */ rhs_expr->kind() == Expr::Kind::IntegerLiteral && as<IntegerLiteral>(rhs_expr)->value() == 0) {
                generated_ir[expr] = lhs;
                break;
            }

            generate_expression(rhs_expr);
            auto rhs = generated_ir[rhs_expr];

            Type* lhs_type_stripped = lhs_expr->type()->strip_references();
            if (lhs_type_stripped->is_pointer()) {
                // pointer subscript needs scaled by size of pointer base type
                auto* type_to_scale_by = as<PointerType>(lhs_type_stripped)->element_type();
                auto* gep = new (*module) GEPInst(Convert(ctx, type_to_scale_by), lhs, rhs, expr->location());
                generated_ir[expr] = gep;
                insert(gep);
            } else if (lhs_type_stripped->is_array()) {
                // array literal subscript (cry)
                auto* element_type = as<ArrayType>(lhs_type_stripped)->element_type();
                auto* alloca = new (*module) AllocaInst(Convert(ctx, lhs_type_stripped), expr->location());
                auto* store = new (*module) StoreInst(lhs, alloca, expr->location());
                auto* gep = new (*module) GEPInst(Convert(ctx, element_type), alloca, rhs, expr->location());
                insert(alloca);
                insert(store);
                insert(gep);
                generated_ir[expr] = gep;
            } else LCC_ASSERT(false, "Sorry, but the rhs of the subscript has an unexpected type");

            break;
        }

        generate_expression(binary_expr->lhs());
        generate_expression(binary_expr->rhs());

        auto lhs = generated_ir[binary_expr->lhs()];
        auto rhs = generated_ir[binary_expr->rhs()];

        switch (binary_expr->op()) {

        case TokenKind::Plus: {
            // Arithmetic Addition
            generated_ir[expr] = new (*module) AddInst(lhs, rhs, expr->location());
        } break;

        case TokenKind::Minus: {
            // Arithmetic Subtraction
            generated_ir[expr] = new (*module) SubInst(lhs, rhs, expr->location());
        } break;

        case TokenKind::Star: {
            // Arithmetic Multiplication
            generated_ir[expr] = new (*module) MulInst(lhs, rhs, expr->location());
        } break;

        case TokenKind::Slash: {
            // Arithmetic Division
            if (binary_expr->lhs()->type()->is_signed_int(ctx) ||
                binary_expr->rhs()->type()->is_signed_int(ctx))
                generated_ir[expr] = new (*module) SDivInst(lhs, rhs, expr->location());
            else generated_ir[expr] = new (*module) UDivInst(lhs, rhs, expr->location());
        } break;

        case TokenKind::Percent: {
            // Arithmetic Modulus (remainder)
            if (binary_expr->lhs()->type()->is_signed_int(ctx) ||
                binary_expr->rhs()->type()->is_signed_int(ctx))
                generated_ir[expr] = new (*module) SRemInst(lhs, rhs, expr->location());
            else generated_ir[expr] = new (*module) URemInst(lhs, rhs, expr->location());
        } break;

        // Comparisons
        case TokenKind::Eq: {
            // Equality
            generated_ir[expr] = new (*module) EqInst(lhs, rhs, expr->location());
        } break;
        case TokenKind::Ne: {
            // NOT Equality
            generated_ir[expr] = new (*module) NeInst(lhs, rhs, expr->location());
        } break;
        case TokenKind::Lt: {
            // Less Than
            if (lhs_expr->type()->is_signed_int(ctx) || rhs_expr->type()->is_signed_int(ctx))
                generated_ir[expr] = new (*module) SLtInst(lhs, rhs, expr->location());
            else generated_ir[expr] = new (*module) ULtInst(lhs, rhs, expr->location());
        } break;
        case TokenKind::Gt: {
            // Greater Than
            if (lhs_expr->type()->is_signed_int(ctx) || rhs_expr->type()->is_signed_int(ctx))
                generated_ir[expr] = new (*module) SGtInst(lhs, rhs, expr->location());
            else generated_ir[expr] = new (*module) UGtInst(lhs, rhs, expr->location());
        } break;
        case TokenKind::Le: {
            // Less Than or Equal To
            if (lhs_expr->type()->is_signed_int(ctx) || rhs_expr->type()->is_signed_int(ctx))
                generated_ir[expr] = new (*module) SLeInst(lhs, rhs, expr->location());
            else generated_ir[expr] = new (*module) ULeInst(lhs, rhs, expr->location());
        } break;
        case TokenKind::Ge: {
            // Greater Than or Equal To
            if (lhs_expr->type()->is_signed_int(ctx) || rhs_expr->type()->is_signed_int(ctx))
                generated_ir[expr] = new (*module) SGeInst(lhs, rhs, expr->location());
            else generated_ir[expr] = new (*module) UGeInst(lhs, rhs, expr->location());
        } break;

        // Binary bitwise operations
        case TokenKind::Ampersand: {
            // Bitwise AND
            generated_ir[expr] = new (*module) AndInst(lhs, rhs, expr->location());
        } break;
        case TokenKind::Pipe: {
            // Bitwise OR
            generated_ir[expr] = new (*module) OrInst(lhs, rhs, expr->location());
        } break;
        case TokenKind::Shl: {
            // Bitwise Shift Left
            generated_ir[expr] = new (*module) ShlInst(lhs, rhs, expr->location());
        } break;
        case TokenKind::Shr: {
            // Bitwise Shift Left
            // FIXME: SAR or SHL?
            generated_ir[expr] = new (*module) SarInst(lhs, rhs, expr->location());
        } break;

        // NOT binary operator tokens.
        case TokenKind::ArbitraryInt:
        case TokenKind::As:
        case TokenKind::AsBang:
        case TokenKind::At:
        case TokenKind::Bool:
        case TokenKind::Byte:
        case TokenKind::Caret:
        case TokenKind::Colon:
        case TokenKind::ColonColon:
        case TokenKind::ColonEq: // handled above
        case TokenKind::Comma:
        case TokenKind::Do:
        case TokenKind::Dot:
        case TokenKind::Else:
        case TokenKind::Eof:
        case TokenKind::Exclam:
        case TokenKind::Export:
        case TokenKind::Extern:
        case TokenKind::Expression:
        case TokenKind::For:
        case TokenKind::Gensym:
        case TokenKind::Hash:
        case TokenKind::Ident:
        case TokenKind::If:
        case TokenKind::IntKw:
        case TokenKind::Invalid:
        case TokenKind::LBrace:
        case TokenKind::RBrace:
        case TokenKind::LBrack: // handled above
        case TokenKind::RBrack:
        case TokenKind::LParen:
        case TokenKind::RParen:
        case TokenKind::Lambda:
        case TokenKind::MacroArg:
        case TokenKind::Number:
        case TokenKind::Return:
        case TokenKind::Static:
        case TokenKind::String:
        case TokenKind::Struct:
        case TokenKind::Semicolon:
        case TokenKind::Then:
        case TokenKind::Tilde:
        case TokenKind::Type:
        case TokenKind::Void:
        case TokenKind::While:
            Diag(ctx, Diag::Kind::ICError, expr->location(), fmt::format("Unexpected operator {} in binary expression", ToString(binary_expr->op())));
            break;
        }

        insert(as<Inst>(generated_ir[expr]));
    } break;

    case intercept::Expr::Kind::Cast: {
        const auto& cast = as<CastExpr>(expr);

        generate_expression(cast->operand());

        lcc::Type *t_to = Convert(ctx, cast->type());
        lcc::Type *t_from = generated_ir[cast->operand()]->type();

        // FIXME: Is this okay? Do we need to check is_lvalue_to_rvalue first?
        if (t_to == t_from) {
            generated_ir[expr] = generated_ir[cast->operand()];
            return;
        }

        usz to_sz = t_to->bits();
        usz from_sz = t_from->bits();

        bool from_signed = cast->operand()->type()->is_signed_int(ctx);

        if (cast->is_lvalue_to_rvalue()) {
            auto load = new (*module) LoadInst(t_to, generated_ir[cast->operand()], expr->location());
            generated_ir[expr] = load;
            insert(load);
        } else {
            if (from_sz == to_sz) {
                auto bitcast = new (*module) BitcastInst(generated_ir[cast->operand()], Convert(ctx, cast->type()), expr->location());
                generated_ir[expr] = bitcast;
                insert(bitcast);
            } else if (from_sz < to_sz) {
                // smaller to larger: sign extend if needed, otherwise zero extend.
                if (from_signed) {
                    auto sign_extend = new (*module) SExtInst(generated_ir[cast->operand()], Convert(ctx, cast->type()), expr->location());
                    generated_ir[expr] = sign_extend;
                    insert(sign_extend);
                }
                else {
                    auto zero_extend = new (*module) ZExtInst(generated_ir[cast->operand()], Convert(ctx, cast->type()), expr->location());
                    generated_ir[expr] = zero_extend;
                    insert(zero_extend);
                }
            } else if (from_sz > to_sz) {
                // larger to smaller: truncate.
                auto truncate = new (*module) TruncInst(generated_ir[cast->operand()], Convert(ctx, cast->type()), expr->location());
                generated_ir[expr] = truncate;
                insert(truncate);
            }
        }

    } break;

    case intercept::Expr::Kind::EvaluatedConstant: {
        const auto& constant = as<ConstantExpr>(expr);
        EvalResult result = constant->value();
        if (result.is_null()) {
            // Zero constant... I dunno
            generated_ir[expr] = new (*module) IntegerConstant(Convert(ctx, constant->type()), 0);
        } else if (result.is_i64()) {
            generated_ir[expr] = new (*module) IntegerConstant(Convert(ctx, constant->type()), (uint64_t)result.as_i64());
        } else if (result.is_string()) {
            LCC_ASSERT(false, "TODO: IR generation of constant strings");
        }
    } break;

    case intercept::Expr::Kind::NameRef: {
        const auto& name_ref = as<NameRefExpr>(expr);
        LCC_ASSERT(generated_ir[name_ref->target()], "NameRef references non-IRGenned expression...");
        generated_ir[expr] = generated_ir[name_ref->target()];
    } break;

    case Expr::Kind::Return: {
        const auto& ret_expr = as<ReturnExpr>(expr);
        if (ret_expr->value()) generate_expression(ret_expr->value());
        const auto& ret = new (*module) ReturnInst(generated_ir[ret_expr->value()], expr->location());
        generated_ir[expr] = ret;
        insert(ret);
    } break;

    case Expr::Kind::While: {
        // +---------+
        // | current |
        // +---------+        ,---------+
        //      |             |         |
        // +--------------------+       |
        // | compute condition  |       |
        // | conditional branch |       |
        // +--------------------+       |
        //      |             |         |
        //      |      +------------+   |
        //      |      | body       |   |
        //      |      +------------+   |
        //      |             |         |
        //      |            ...        |
        //      |             |         |
        //  +----------+      `---------+
        //  | exit     |
        //  +----------+

        const auto& while_expr = as<WhileExpr>(expr);

        // TODO: could number whiles to make this easier I guess, for any poor
        // soul who has to debug the IR.

        auto* body = new (*module) lcc::Block("while.body");
        auto* conditional = new (*module) lcc::Block("while.conditional");
        auto* exit = new (*module) lcc::Block("while.exit");

        insert(new (*module) BranchInst(conditional, expr->location()));

        update_block(conditional);
        generate_expression(while_expr->condition());
        insert(new (*module) CondBranchInst(generated_ir[while_expr->condition()], body, exit, expr->location()));

        update_block(body);
        generate_expression(while_expr->body());
        insert(new (*module) BranchInst(conditional, expr->location()));

        update_block(exit);
    } break;

    case Expr::Kind::For: {
        // +------------------+
        // | current          |
        // | emit initialiser |
        // +------------------+
        //      |
        //      |             ,-------------+
        //      |             |             |
        // +--------------------+           |
        // | conditional branch |           |
        // +--------------------+           |
        //      |             |             |
        //      |      +----------------+   |
        //      |      | body           |   |
        //      |      | emit iterator  |   |
        //      |      +----------------+   |
        //      |             |             |
        //      |            ...            |
        //      |             |             |
        //  +----------+      `-------------+
        //  | exit     |
        //  +----------+
        const auto& for_expr = as<ForExpr>(expr);

        auto* conditional = new (*module) lcc::Block("for.conditional");
        auto* body = new (*module) lcc::Block("for.body");
        auto* exit = new (*module) lcc::Block("for.exit");

        generate_expression(for_expr->init());
        insert(new (*module) BranchInst(conditional, expr->location()));

        update_block(conditional);
        generate_expression(for_expr->condition());
        insert(new (*module) CondBranchInst(generated_ir[for_expr->condition()], body, exit, expr->location()));

        update_block(body);
        generate_expression(for_expr->body());
        generate_expression(for_expr->increment());
        insert(new (*module) BranchInst(conditional, expr->location()));

        update_block(exit);
    } break;

    case Expr::Kind::If: {
        ///      +---------+
        ///      | current |
        ///      +---------+
        ///     //         \\
        /// +------+    +------+
        /// | then |    | else |
        /// +------+    +------+
        ///        \\  //
        ///       +------+
        ///       | exit |
        ///       +------+
        ///
        const auto& if_expr = as<IfExpr>(expr);

        auto* then = new (*module) lcc::Block("if.then");
        auto* else_ = new (*module) lcc::Block("if.else");
        auto* exit = new (*module) lcc::Block("if.exit");

        generate_expression(if_expr->condition());
        insert(new (*module) CondBranchInst(generated_ir[if_expr->condition()], then, else_, expr->location()));

        update_block(then);
        generate_expression(if_expr->then());
        insert(new (*module) BranchInst(exit, expr->location()));

        update_block(else_);
        generate_expression(if_expr->else_());
        insert(new (*module) BranchInst(exit, expr->location()));

        update_block(exit);
    } break;

    case Expr::Kind::StringLiteral: {
        const auto& literal_string_expr = as<StringLiteral>(expr);
        std::string& literal_string = int_module.strings.at(literal_string_expr->string_index());
        std::vector<char> data(literal_string.begin(), literal_string.end());
        generated_ir[expr] = new (*module) ArrayConstant(lcc::ArrayType::Get(ctx, literal_string.length(), lcc::IntegerType::Get(ctx, 8)), data);
    } break;

    case Expr::Kind::Call: {
        const auto& call = as<CallExpr>(expr);

        generate_expression(call->callee());

        auto function_type = as<FunctionType>(Convert(ctx, call->callee_type()));

        std::vector<Value*> args{};
        for (const auto& arg : call->args()) {
            generate_expression(arg);
            args.push_back(generated_ir[arg]);
        }

        auto ir_call = new (*module) CallInst(generated_ir[call->callee()], function_type, std::move(args), expr->location());

        generated_ir[expr] = ir_call;
        insert(ir_call);
    } break;

    case Expr::Kind::IntrinsicCall: {

        LCC_ASSERT(false, "TODO: sorry, but IRGen of intrinsic calls isn't yet implemented.");

        const auto& intrinsic = as<IntrinsicCallExpr>(expr);
        switch (intrinsic->intrinsic_kind()) {
        case IntrinsicKind::BuiltinDebugtrap: {
            LCC_ASSERT(intrinsic->args().size() == 0, "No arguments to Debug Trap Builtin");
        } break;
        case IntrinsicKind::BuiltinFilename: {
            LCC_ASSERT(intrinsic->args().size() == 0, "No arguments to Filename Builtin");
        } break;
        case IntrinsicKind::BuiltinLine: {
            LCC_ASSERT(intrinsic->args().size() == 0, "No arguments to File Line Builtin");
        } break;
        case IntrinsicKind::BuiltinInline: {
            LCC_ASSERT(intrinsic->args().size() == 0, "No arguments to Inline Builtin");
        } break;
        case IntrinsicKind::BuiltinMemCopy: {
            LCC_ASSERT(intrinsic->args().size() == 0, "No arguments to Memory Copy Builtin");
        } break;
        case IntrinsicKind::BuiltinMemSet: {
            LCC_ASSERT(intrinsic->args().size() == 0, "No arguments to Memory Set Builtin");
        } break;
        case IntrinsicKind::BuiltinSyscall: {
            LCC_ASSERT(intrinsic->args().size() == 0, "No arguments to Syscall Builtin");
        } break;
        }
    } break;

    case Expr::Kind::MemberAccess: {
        LCC_ASSERT(false, "TODO: sorry, but IRGen of member access isn't yet implemented. I'll get to it.");
    } break;

    case Expr::Kind::CompoundLiteral: {
        // TODO: I need help with this. What IR does it make?
        LCC_ASSERT(false, "TODO: I'm blanking on how to implement compound literal IRGen, so I'm going to wait until I can talk to somebody smarter than me.");
    } break;

    // no-op/handled elsewhere
    case Expr::Kind::StructDecl: break;
    case Expr::Kind::TypeAliasDecl: break;
    case Expr::Kind::FuncDecl: break;
    case Expr::Kind::OverloadSet: break;
    }
}

void IRGen::generate_function(intercept::FuncDecl* f) {
    function = new (*module) lcc::Function
        (ctx,
         f->mangled_name(),
         as<lcc::FunctionType>(Convert(ctx, f->type())),
         f->linkage(),
         lcc::CallConv::Intercept
         );
     module->add_function(function);

    // Hard to generate code for a function without a body.
    if (auto* expr = f->body()) {
        block = new (*module) lcc::Block("body");
        update_block(block);

        unsigned int i = 0;
        for (const auto& param : f->param_decls()) {
            const auto& param_ir = new (*module) ParamInst(Convert(ctx, param->type()), i++, param->location());
            generated_ir[param] = param_ir;
            insert(param_ir);
        }

        // generate body
        generate_expression(expr);
    }
}

auto IRGen::Generate(Context* context, intercept::Module& int_mod) -> lcc::Module* {
    auto ir_gen = IRGen(context, int_mod);

    // We must /create/ *all* functions first, before generating the IR for
    // *any* of them. This is because a NameRef in one function may reference
    // another function, and we want all functions to be resolveable.
    for (auto f : int_mod.functions()) ir_gen.create_function(f);
    for (auto f : int_mod.functions()) ir_gen.generate_function(f);

    return ir_gen.mod();
}


}
}

void lcc::Module::print_ir() {
    for (auto function : code()) {
        fmt::print("{}:\n", function->name());
        for (const auto& b : function->blocks()) {
            fmt::print("  {}:\n", b->name());
            for (const auto& i : b->instructions())
                ValuePrinter::print(i, fmt::format("{:4} | ", ValuePrinter::get_id_raw(i)));
        }
    }
}
