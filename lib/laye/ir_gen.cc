#include <laye/ir_gen.hh>

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
#include <unordered_map>

namespace layec = lcc::laye;

auto layec::IRGen::Generate(LayeContext* laye_context, laye::Module* module) -> lcc::Module* {
    auto ir_gen = IRGen{laye_context, module};

    return ir_gen.mod();
}
