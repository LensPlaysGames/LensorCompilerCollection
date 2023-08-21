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
    lcc::Module* module;

    lcc::Function* function{nullptr};
    lcc::Block* block{nullptr};

    IRGen(Context *c, intercept::Module& m) : ctx(c), int_module(m) {
        module = new lcc::Module(ctx);
    }

    void insert(lcc::Inst* inst);

    void generate_expression(intercept::Expr*);

    void generate_function(intercept::FuncDecl*);

public:
    /// NOTE: I would name this module(), but C++ doesn't have properties.
    auto mod() -> lcc::Module* {
        return module;
    }

    static auto Generate(Context*, intercept::Module&) -> lcc::Module*;
};

}

#endif /* INTERCEPT_IR_GEN_HH */
