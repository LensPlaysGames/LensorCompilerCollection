#include <intercept/ir_gen.hh>

#include <intercept/ast.hh>
#include <lcc/core.hh>
#include <lcc/ir/ir.hh>
#include <lcc/ir/module.hh>
#include <lcc/context.hh>
#include <lcc/utils/rtti.hh>

#include <memory>
#include <vector>

namespace lcc {

namespace intercept {
lcc::Type* Convert(intercept::Type* in) {
    switch (in->kind()) {
    default: std::exit(72);
    }
}
}


void generate_expression(intercept::Module& mod, intercept::Expr* expr, Module& out) {
    switch (expr->kind()) {
    case intercept::Expr::Kind::Block: {
        for (auto e : as<intercept::BlockExpr>(expr)->children()) generate_expression(mod, e, out);
    } break;

    case intercept::Expr::Kind::IntegerLiteral: {
        fmt::print("Integer Literal {}", as<intercept::IntegerLiteral>(expr)->value());
    } break;

    default: {
        fmt::print("Unhandled IRGen of expression kind {}\n", (int)expr->kind());
        //std::exit(3);
    } break;
    }
}

auto intercept::IRGen::Generate(Context* context, intercept::Module& mod) -> std::unique_ptr<Module> {
    std::unique_ptr<lcc::Module> out(new lcc::Module(context));

    for (auto f : mod.functions()) {
        fmt::print("{}\n", f->name());

        // Parameter Types
        std::vector<lcc::Type*> ir_param_types{};
        for (auto p_type : f->param_types()) ir_param_types.push_back(lcc::intercept::Convert(p_type));

        // Return Type
        lcc::Type* ir_return_type = lcc::intercept::Convert(f->return_type());

        // Function Type (combine parameter + return)
        lcc::FunctionType* ir_functype = lcc::FunctionType::Get(*context, ir_return_type, ir_param_types);

        lcc::Function* ir_function = new (*out) lcc::Function
            (context,
             f->mangled_name(),
             ir_functype,
             lcc::Linkage::Exported,
             lcc::CallConv::Intercept
             );

        auto* expr = f->body();
        generate_expression(mod, expr, *out);
    }

    return {};
}

}
