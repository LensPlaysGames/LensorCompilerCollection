#include <intercept/ir_gen.hh>

#include <intercept/ast.hh>
#include <lcc/ir/ir.hh>
#include <lcc/ir/module.hh>
#include <lcc/context.hh>
#include <lcc/utils/rtti.hh>

#include <memory>
#include <vector>

namespace lcc {

auto InterceptIRGen::Generate(Context* context, intercept::Module& mod) -> std::unique_ptr<Module> {
    // TODO: For each function in module, generate IR for it.
    return {};
}

}
