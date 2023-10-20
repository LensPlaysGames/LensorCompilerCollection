#ifndef LAYE_IR_GEN_HH
#define LAYE_IR_GEN_HH

#include <laye/ast.hh>
#include <lcc/context.hh>
#include <lcc/ir/ir.hh>
#include <lcc/utils/rtti.hh>
#include <lcc/ir/module.hh>
#include <memory>
#include <unordered_map>

namespace lcc::laye {

class IRGen {
    LayeContext* laye_context;
    laye::Module* laye_module;

    lcc::Module* _mod;

    IRGen(LayeContext* laye_context, laye::Module* laye_module)
        : laye_context(laye_context), laye_module(laye_module) {
        _mod = new lcc::Module(laye_context->context());
    }
public:
    auto mod() const { return _mod; }

    static auto Generate(LayeContext* laye_context, laye::Module* laye_module) -> lcc::Module*;
};

};

#endif // LAYE_IR_GEN_HH
