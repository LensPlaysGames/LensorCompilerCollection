#include <lcc/ir/module.hh>

#include <fmt/format.h>
#include <lcc/context.hh>
#include <lcc/target.hh>
#include <lcc/utils/ir_printer.hh>
#include <lcc/codegen/gnu_as_att_assembly.hh>

namespace lcc {

void Module::lower() {
    if (_ctx->target()->is_x64()) {
        if (_ctx->target()->is_windows()) {
            // x64 Calling Convention Lowering
            
        } else if (_ctx->target() == Target::x86_64_linux) {
            // TODO: SysV x86_64 Calling Convention Lowering
        }
    } else {
        LCC_ASSERT(false, "TODO: Lowering of specified arch is not yet supported");
    }
}

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
