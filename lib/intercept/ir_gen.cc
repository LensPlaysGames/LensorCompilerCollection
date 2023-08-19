#include <intercept/ir_gen.hh>

#include <intercept/ast.hh>
#include <lcc/ir/ir.hh>
#include <lcc/ir/module.hh>
#include <lcc/context.hh>
#include <lcc/utils/rtti.hh>

#include <memory>
#include <vector>

namespace lcc {

auto intercept::IRGen::Generate(Context* context, intercept::Module& mod) -> std::unique_ptr<Module> {
    std::unique_ptr<lcc::Module> out(new lcc::Module(context));

    for (auto f : mod.functions()) {
        fmt::print("{}\n", f->name());
        auto* expr = f->body();
    }

    return {};
}

}
