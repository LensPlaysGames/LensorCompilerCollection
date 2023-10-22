#include <lcc/ir/module.hh>

#include <fmt/format.h>
#include <lcc/context.hh>
#include <lcc/format.hh>
#include <lcc/utils/ir_printer.hh>
#include <lcc/codegen/gnu_as_att_assembly.hh>

namespace lcc {

void Module::emit() {
    switch (_ctx->format()->format()) {
        case Format::INVALID: LCC_UNREACHABLE();
        case Format::LLVM_TEXTUAL_IR: {
            fmt::print("{}", llvm());
        } break;
        case Format::GNU_AS_ATT_ASSEMBLY: {
            emit_gnu_as_att(_ctx, this);
        } break;
    }
}

} // namespace lcc
