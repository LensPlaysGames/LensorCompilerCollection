#include <lcc/utils.hh>
#include <lcc/ir/module.hh>
#include <lcc/context.hh>
#include <lcc/target.hh>
#include <lcc/codegen/mir.hh>
#include <lcc/codegen/isel.hh>
#include <lcc/codegen/isel_patterns_x86_64.hh>

namespace lcc {

void select_instructions(Module* mod, MFunction& function) {
    if (mod->context()->target()->is_x64())
        lcc::isel::x86_64PatternList::rewrite(mod, function);
}

} // namespace lcc
