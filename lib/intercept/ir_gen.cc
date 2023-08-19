#include <intercept/ir_gen.hh>

#include <intercept/ast.hh>
#include <lcc/ir/ir.hh>
#include <lcc/context.hh>
#include <lcc/utils/rtti.hh>

#include <memory>
#include <vector>

namespace lcc::intercept {

auto IRGen::Generate(Context* context, Module& mod) -> std::vector<Function> {
    // TODO: For each function in module, generate IR for it.
    return {};
}

}
