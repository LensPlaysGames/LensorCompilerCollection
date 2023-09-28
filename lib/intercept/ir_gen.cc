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
        auto* t_return = Convert(ctx, as<FuncType>(in)->return_type());

        std::vector<lcc::Type*> param_types{};
        for (const auto& p : as<FuncType>(in)->params())
            param_types.push_back(Convert(ctx, p.type));

        return lcc::FunctionType::Get(ctx, t_return, std::move(param_types));
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

// NOTE: If you `new` an instruction, you need to insert it (somewhere).
void intercept::IRGen::generate_expression(intercept::Expr* expr) {
    switch (expr->kind()) {
    case intercept::Expr::Kind::Block: {
        for (auto e : as<intercept::BlockExpr>(expr)->children()) generate_expression(e);
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
            auto* alloca = new (*module) AllocaInst(Convert(ctx, decl->type()), decl->location());
            insert(alloca);
            if (auto* init_expr = decl->init()) {
                generate_expression(init_expr);
                // Store generated init_expr into above inserted declaration
                auto* local_init = new (*module) StoreInst(generated_ir[init_expr], alloca);
                insert(local_init);
            }
            generated_ir[expr] = alloca;
        } break;

        case Linkage::Imported:
        case Linkage::Reexported: {
            auto* alloca = new (*module) AllocaInst(Convert(ctx, decl->type()), decl->location());
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
            generated_ir[expr] = new (*module) NegInst(generated_ir[unary_expr->operand()]);
        } break;
        default: LCC_ASSERT(false, "Sorry, but IRGen of unary operator {} has, apparently, not been implemented. Sorry about that.", ToString(unary_expr->op()));
        }

        insert(as<Inst>(generated_ir[expr]));
    } break;

    case intercept::Expr::Kind::Binary: {
        const auto& binary_expr = as<BinaryExpr>(expr);

        generate_expression(binary_expr->lhs());
        generate_expression(binary_expr->rhs());

        auto lhs = generated_ir[binary_expr->lhs()];
        auto rhs = generated_ir[binary_expr->rhs()];

        switch (binary_expr->op()) {

        case TokenKind::Plus: {
            // Arithmetic Addition
            generated_ir[expr] = new (*module) AddInst(lhs, rhs);
        } break;

        case TokenKind::Minus: {
            // Arithmetic Subtraction
            generated_ir[expr] = new (*module) SubInst(lhs, rhs);
        } break;

        case TokenKind::Star: {
            // Arithmetic Multiplication
            generated_ir[expr] = new (*module) MulInst(lhs, rhs);
        } break;

        case TokenKind::Slash: {
            // Arithmetic Division
            if (binary_expr->lhs()->type()->is_signed_int(ctx) ||
                binary_expr->rhs()->type()->is_signed_int(ctx))
                generated_ir[expr] = new (*module) SDivInst(lhs, rhs);
            else generated_ir[expr] = new (*module) UDivInst(lhs, rhs);
        } break;

        case TokenKind::Percent: {
            // Arithmetic Modulus (remainder)
            if (binary_expr->lhs()->type()->is_signed_int(ctx) ||
                binary_expr->rhs()->type()->is_signed_int(ctx))
                generated_ir[expr] = new (*module) SRemInst(lhs, rhs);
            else generated_ir[expr] = new (*module) URemInst(lhs, rhs);
        } break;

        // TODO: Binary bitwise operations

        default: {
            LCC_ASSERT(false, "Unhandled IRGen of binary expression operator {}", (int)binary_expr->op());
        } break;
        }

        insert(as<Inst>(generated_ir[expr]));

    } break;

    case intercept::Expr::Kind::Cast: {
        const auto& cast = as<CastExpr>(expr);

        lcc::Type *t_to = Convert(ctx, cast->type());
        lcc::Type *t_from = Convert(ctx, cast->operand()->type());

        usz to_sz = t_to->bits();
        usz from_sz = t_from->bits();

        bool from_signed = cast->operand()->type()->is_signed_int(ctx);

        generate_expression(cast->operand());

        if (cast->is_lvalue_to_rvalue()) {
            auto load = new (*module) LoadInst(t_to, generated_ir[cast->operand()]);
            generated_ir[expr] = load;
            insert(load);
        } else {
            if (from_sz == to_sz) {
                auto bitcast = new (*module) BitcastInst(generated_ir[cast->operand()], Convert(ctx, cast->type()));
                generated_ir[expr] = bitcast;
                insert(bitcast);
            } else if (from_sz < to_sz) {
                // smaller to larger: sign extend if needed, otherwise zero extend.
                if (from_signed) {
                    auto sign_extend = new (*module) SExtInst(generated_ir[cast->operand()], Convert(ctx, cast->type()));
                    generated_ir[expr] = sign_extend;
                    insert(sign_extend);
                }
                else {
                    auto zero_extend = new (*module) ZExtInst(generated_ir[cast->operand()], Convert(ctx, cast->type()));
                    generated_ir[expr] = zero_extend;
                    insert(zero_extend);
                }
            } else if (from_sz > to_sz) {
                // larger to smaller: truncate.
                auto truncate = new (*module) TruncInst(generated_ir[cast->operand()], Convert(ctx, cast->type()));
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
        // TODO: Assert that target has already been ir genned, somehow.
        const auto& name_ref = as<NameRefExpr>(expr);
        generated_ir[expr] = generated_ir[name_ref->target()];
    } break;

    case Expr::Kind::Return: {
        const auto& ret_expr = as<ReturnExpr>(expr);
        const auto& ret = new (*module) ReturnInst(generated_ir[ret_expr->value()]);
        generated_ir[expr] = ret;
        insert(ret);
    }

    default: {
        LCC_ASSERT(false, "Unhandled IRGen of expression kind {} ({})\n", Expr::kind_string(expr->kind()), (int)expr->kind());
    } break;
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

    // Hard to generate code for a function without a body.
    if (auto* expr = f->body()) {
        block = new (*module) lcc::Block("body");
        function->append_block(block);
        generate_expression(expr);
    }

    for (const auto& b : function->blocks())
        for (const auto& i : b->instructions())
            ValuePrinter::print(i, fmt::format("{:4} | ", ValuePrinter::get_id_raw(i)));
}

auto IRGen::Generate(Context* context, intercept::Module& int_mod) -> lcc::Module* {
    auto ir_gen = IRGen(context, int_mod);

    for (auto f : int_mod.functions()) ir_gen.generate_function(f);

    return ir_gen.mod();
}

}
}
