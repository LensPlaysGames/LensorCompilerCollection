#include <intercept/ir_gen.hh>

#include <intercept/ast.hh>
#include <lcc/core.hh>
#include <lcc/ir/ir.hh>
#include <lcc/ir/type.hh>
#include <lcc/ir/module.hh>
#include <lcc/context.hh>
#include <lcc/utils.hh>
#include <lcc/utils/rtti.hh>

#include <memory>
#include <vector>

namespace lcc {

namespace intercept {
lcc::Type* Convert(Context* ctx, Type* in) {
    switch (in->kind()) {
    case Type::Kind::Builtin: {
        switch ((as<BuiltinType>(in))->builtin_kind()) {
        case BuiltinType::BuiltinKind::Bool: {
            return lcc::Type::I1Ty;
        } break;
        case BuiltinType::BuiltinKind::Byte: {
            return lcc::IntegerType::Get(ctx, in->size(ctx));
        } break;
        case BuiltinType::BuiltinKind::Int: {
            return lcc::IntegerType::Get(ctx, in->size(ctx));
        } break;
        case BuiltinType::BuiltinKind::Void: {
            return lcc::Type::VoidTy;
        } break;
        case BuiltinType::BuiltinKind::OverloadSet:
        case BuiltinType::BuiltinKind::Unknown: {
            Diag::ICE("Invalid builtin kind present during IR generation");
        } break;
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
}

void intercept::IRGen::generate_expression(intercept::Expr* expr, Function& ir_function) {
    switch (expr->kind()) {
    case intercept::Expr::Kind::Block: {
        for (auto e : as<intercept::BlockExpr>(expr)->children()) generate_expression(e, ir_function);
    } break;

    case intercept::Expr::Kind::IntegerLiteral: {
        auto int_literal = new (*module) IntegerConstant(Convert(ctx, expr->type()), (as<IntegerLiteral>(expr))->value());
    } break;

    default: {
        fmt::print("Unhandled IRGen of expression kind {}\n", (int)expr->kind());
        //std::exit(3);
    } break;
    }
}

auto intercept::IRGen::Generate(Context* context, intercept::Module& mod) -> std::unique_ptr<Module> {
    std::unique_ptr<lcc::Module> out(new lcc::Module(context));

    IRGen ir_gen_stupid_instantiation(context, mod);

    for (auto f : mod.functions()) {
        fmt::print("{}\n", f->name());
        ir_gen_stupid_instantiation.function = new (*out) lcc::Function
            (context,
             f->mangled_name(),
             as<lcc::FunctionType>(Convert(context, f->type())),
             f->linkage(),
             lcc::CallConv::Intercept
             );

        if (auto* expr = f->body()) ir_gen_stupid_instantiation.generate_expression(expr, *ir_gen_stupid_instantiation.function);
    }

    return {};
}

}
