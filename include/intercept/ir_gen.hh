#ifndef INTERCEPT_IR_GEN_HH
#define INTERCEPT_IR_GEN_HH

#include <intercept/ast.hh>
#include <lcc/context.hh>
#include <lcc/ir/ir.hh>
#include <lcc/utils/rtti.hh>
#include <vector>

namespace lcc::intercept {

class IRGen {
    Context *_ctx;
    Module& _mod;

    IRGen(Context *c, Module& m) : _ctx(c), _mod(m) {}

public:
    static auto Generate(Context* context, Module& mod) -> std::vector<Function>;
};

}

#endif /* INTERCEPT_IR_GEN_HH */
