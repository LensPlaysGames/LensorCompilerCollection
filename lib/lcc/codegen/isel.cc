#include <lcc/codegen/isel.hh>
#include <lcc/codegen/isel_patterns_x86_64.hh>
#include <lcc/codegen/mir.hh>
#include <lcc/codegen/x86_64.hh>
#include <lcc/context.hh>
#include <lcc/ir/module.hh>
#include <lcc/target.hh>
#include <lcc/utils.hh>

namespace lcc {

void select_instructions(Module* mod, MFunction& function) {
    if (mod->context()->target()->is_x64())
        function = lcc::isel::x86_64PatternList::rewrite(mod, function);
    // TODO: Mark defining uses by walking CFG.
}

} // namespace lcc
