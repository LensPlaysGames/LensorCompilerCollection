#ifndef INTERCEPT_IR_GEN_HH
#define INTERCEPT_IR_GEN_HH

#include <intercept/ast.hh>
#include <lcc/context.hh>
#include <lcc/ir/ir.hh>
#include <lcc/utils/rtti.hh>
#include <memory>

namespace lcc::intercept {

class IRGen {
    Context *_ctx;
    intercept::Module& _mod;

    IRGen(Context *c, intercept::Module& m) : _ctx(c), _mod(m) {}

public:
    static auto Generate(Context* context, intercept::Module& mod) -> std::unique_ptr<Module>;
};

}

#endif /* INTERCEPT_IR_GEN_HH */
