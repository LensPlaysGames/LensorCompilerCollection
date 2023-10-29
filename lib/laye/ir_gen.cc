#include <laye/ir_gen.hh>

#include <intercept/ast.hh>
#include <lcc/core.hh>
#include <lcc/ir/ir.hh>
#include <lcc/ir/type.hh>
#include <lcc/ir/module.hh>
#include <lcc/context.hh>
#include <lcc/utils.hh>
#include <lcc/utils/rtti.hh>
#include <lcc/utils/macros.hh>

#include <memory>
#include <vector>
#include <unordered_map>

namespace layec = lcc::laye;

namespace lcc::laye {
lcc::Type* Convert(lcc::Context* ctx, layec::Type* in) {
    switch (in->kind()) {
        default: {
            Diag::ICE("Unhandled IR type conversion for Laye type {}", ToString(in->kind()));
        }

        case Expr::Kind::TypeFunc: {
            const auto& t = as<FuncType>(in);
            std::vector<lcc::Type*> param_types{};
            for (const auto& p : t->param_types())
                param_types.push_back(Convert(ctx, p));
            return lcc::FunctionType::Get(ctx, Convert(ctx, t->return_type()), std::move(param_types));
        }

        case Expr::Kind::TypeVoid: {
            return lcc::Type::VoidTy;
        }

        case Expr::Kind::TypeInt: {
            return lcc::IntegerType::Get(ctx, in->size(ctx));
        }
    }
}
}

auto layec::IRGen::Generate(LayeContext* laye_context, laye::Module* module) -> lcc::Module* {
    auto ir_gen = IRGen{laye_context, module};

    for (auto& tld : module->top_level_decls()) {
        if (auto f = cast<FunctionDecl>(tld))
            ir_gen.CreateIRFunctionValue(f);
    }

    for (auto& tld : module->top_level_decls()) {
        if (auto f = cast<FunctionDecl>(tld))
            ir_gen.GenerateIRFunctionBody(f);
    }

    return ir_gen.mod();
}

void layec::IRGen::CreateIRFunctionValue(FunctionDecl* decl) {
    auto params = decl->params();

    std::vector<layec::Type*> param_types{};
    for (auto& param : params) {
        param_types.push_back(param->type);
    }

    auto func_type = new (*laye_mod()) layec::FuncType{
        decl->location(),
        decl->return_type(),
        param_types
    };

    _ir_values[decl] = new (*mod()) Function(
        mod(),
        decl->mangled_name(),
        as<FunctionType>(Convert(context(), func_type)),
        decl->linkage(),
        decl->calling_convention(),
        decl->location()
    );
}

void layec::IRGen::GenerateIRFunctionBody(FunctionDecl* decl) {
    tempset curr_func = as<Function>(_ir_values[decl]);
    if (decl->body()) {
        auto block = new (*mod()) lcc::Block(fmt::format("body.{}", total_block));
        UpdateBlock(block);

        for (auto [i, param] : vws::enumerate(decl->params())) {
            auto inst = curr_func->param(usz(i));
            
            auto alloca = new (*mod()) AllocaInst(inst->type(), param->location);
            Insert(alloca);

            auto store = new (*mod()) StoreInst(inst, alloca);
            Insert(store);

            _ir_params[param] = alloca;
        }

        GenerateStatement(as<BlockStatement>(decl->body()));
    }
}

void layec::IRGen::GenerateStatement(Statement* statement) {
    using Sk = Statement::Kind;
    switch (statement->kind()) {
        default: {
            LCC_ASSERT(false, "unhandled statement in Laye IR gen {}", ToString(statement->kind()));
        } break;

        case Sk::Block: {
            for (auto child : as<BlockStatement>(statement)->children()) {
                GenerateStatement(child);
            }
        } break;

        case Sk::Return: {
            auto s = as<ReturnStatement>(statement);
            if (s->is_void_return()) {
                Insert(new (*mod()) ReturnInst(nullptr, statement->location()));
            } else {
                LCC_ASSERT(s->value());
                auto return_value = GenerateExpression(s->value());
                Insert(new (*mod()) ReturnInst(return_value, statement->location()));
            }
        } break;
    }
}

lcc::Value* layec::IRGen::GenerateExpression(Expr* expr) {
    using Ek = Expr::Kind;

    if (_ir_values[expr]) return _ir_values[expr];

    switch (expr->kind()) {
        default: {
            LCC_ASSERT(false, "unhandled expr in Laye IR gen {}", ToString(expr->kind()));
        } break;

        case Ek::Constant: {
            auto e = as<ConstantExpr>(expr);
            LCC_ASSERT(e->type());

            auto value = e->value();
            auto type = Convert(context(), e->type());

            if (value.is_i64()) {
                _ir_values[expr] = new (*mod()) lcc::IntegerConstant{type, uint64_t(value.as_i64())};
            } else {
                LCC_TODO();
            }
        } break;
    }

    LCC_ASSERT(_ir_values[expr]);
    return _ir_values[expr];
}
