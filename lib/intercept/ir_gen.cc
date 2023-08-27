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

    case intercept::Expr::Kind::Binary: {
        const auto& binary_expr = as<BinaryExpr>(expr);
        switch (binary_expr->op()) {
        default: {
            fmt::print("Unhandled IRGen of binary expression operator {}", (int)binary_expr->op());
        } break;
        }
    } break;

    default: {
        fmt::print("Unhandled IRGen of expression kind {}\n", (int)expr->kind());
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

    usz n = 0;
    for (const auto& b : function->blocks())
        for (const auto& i : b->instructions())
            ValuePrinter::print(i, fmt::format("{:4} | ", n++));
}

auto IRGen::Generate(Context* context, intercept::Module& int_mod) -> lcc::Module* {
    auto ir_gen = IRGen(context, int_mod);

    for (auto f : int_mod.functions()) ir_gen.generate_function(f);

    return ir_gen.mod();
}

}
}
