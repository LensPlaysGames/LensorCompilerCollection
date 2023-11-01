#include <lcc/utils.hh>
#include <lcc/context.hh>
#include <lcc/target.hh>
#include <lcc/codegen/mir.hh>
#include <lcc/codegen/isel.hh>
#include <lcc/codegen/isel_patterns_x86_64.hh>

namespace lcc {

void select_instructions(const Context* const ctx, MFunction& function) {
    if (ctx->target()->is_x64())
        lcc::isel::x86_64PatternList::rewrite(function);
}

} // namespace lcc
