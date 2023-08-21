#ifndef INTERCEPT_IR_GEN_HH
#define INTERCEPT_IR_GEN_HH

#include <intercept/ast.hh>
#include <lcc/context.hh>
#include <lcc/ir/ir.hh>
#include <lcc/utils/rtti.hh>
#include <lcc/ir/module.hh>
#include <memory>

namespace lcc::intercept {

class IRGen {
    Context *ctx;
    intercept::Module& int_module;
    std::unique_ptr<lcc::Module> module;

    lcc::Function* function{nullptr};
    lcc::Block* block{nullptr};

    IRGen(Context *c, intercept::Module& m) : ctx(c), int_module(m) {
        module = std::make_unique<lcc::Module>(ctx);
    }

    void insert(lcc::Inst* inst);

    void generate_expression(Expr* expr, lcc::Function& ir_function);

public:
    static auto Generate(Context* context, intercept::Module& mod) -> std::unique_ptr<Module>;
};

}

#endif /* INTERCEPT_IR_GEN_HH */
