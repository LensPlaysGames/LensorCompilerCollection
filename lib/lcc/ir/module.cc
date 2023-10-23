#include <lcc/ir/module.hh>

#include <fmt/format.h>
#include <lcc/context.hh>
#include <lcc/format.hh>
#include <lcc/target.hh>
#include <lcc/utils.hh>
#include <lcc/utils/ir_printer.hh>
#include <lcc/ir/ir.hh>
#include <lcc/diags.hh>

#include <algorithm>
#include <unordered_set>

namespace lcc {

// TODO: Use this, copy it out of `Module::lower()`
static void lower_windows_x64() {
}

void Module::lower() {
    if (_ctx->target()->is_x64()) {
        if (_ctx->target()->is_windows()) {
            // x64 Calling Convention Lowering

            // Volatile means a register may be altered across function calls, and
            // needs to be saved by the caller.
            // RAX                          Volatile     Return value register
            // RCX                          Volatile     First integer argument
            // RDX                          Volatile     Second integer argument
            // R8                           Volatile     Third integer argument
            // R9                           Volatile     Fourth integer argument
            // R10:R11                      Volatile     Must be preserved as needed by caller; used in syscall/sysret instructions
            // R12:R15                      Nonvolatile  Must be preserved by callee
            // RDI                          Nonvolatile  Must be preserved by callee
            // RSI                          Nonvolatile  Must be preserved by callee
            // RBX                          Nonvolatile  Must be preserved by callee
            // RBP                          Nonvolatile  May be used as a frame pointer; must be preserved by callee
            // RSP                          Nonvolatile  Stack pointer
            // XMM0, YMM0                   Volatile     First FP argument; first vector-type argument when __vectorcall is used
            // XMM1, YMM1                   Volatile     Second FP argument; second vector-type argument when __vectorcall is used
            // XMM2, YMM2                   Volatile     Third FP argument; third vector-type argument when __vectorcall is used
            // XMM3, YMM3                   Volatile     Fourth FP argument; fourth vector-type argument when __vectorcall is used
            // XMM4, YMM4                   Volatile     Must be preserved as needed by caller; fifth vector-type argument when __vectorcall is used
            // XMM5, YMM5                   Volatile     Must be preserved as needed by caller; sixth vector-type argument when __vectorcall is used
            // XMM6:XMM15, YMM6:YMM15       Non volatile (XMM), Volatile (upper half of YMM). Must be preserved by callee. YMM registers must be preserved as needed by caller.

            for (auto function : code()) {
                for (auto block : function->blocks()) {
                    std::vector<isz> indicesOfInstructionsToRemove;
                    for (auto [index, instruction] : vws::enumerate(block->instructions())) {
                        switch (instruction->kind()) {
                        case Value::Kind::Load: {
                            auto load = as<LoadInst>(instruction);

                            // Less than or equal to 8 bytes; nothing to change.
                            if (load->type()->bits() <= 64) continue;

                            LCC_ASSERT(false, "TODO: Handle load > 8 bytes lowering");
                        } break;

                        case Value::Kind::Store: {
                            auto store = as<StoreInst>(instruction);

                            // Less than or equal to 8 bytes; nothing to change.
                            if (store->type()->bits() <= 64) continue;

                            LCC_ASSERT(false, "TODO: Handle store > 8 bytes lowering");
                        } break;

                        case Value::Kind::Phi: {
                            auto phi = as<PhiInst>(instruction);

                            // Remove empty phis.
                            if (phi->operands().empty()) {
                                if (not phi->users().empty())
                                    Diag::ICE("Ill-formed IR; empty Phi has users");

                                // Yeet the phi
                                indicesOfInstructionsToRemove.push_back(index);
                            }

                            if (phi->operands().size() == 1) {
                                LCC_ASSERT(false, "TODO: Replace uses of phi with single operand with its operand");
                            }

                            LCC_ASSERT(false, "TODO: Handle PHI lowering");
                        } break;

                        default: break;
                        }
                    }
                    for (isz i : indicesOfInstructionsToRemove | vws::reverse) {
                        block->instructions().erase(block->instructions().begin() + i);
                    }
                }
            }
        } else if (_ctx->target() == Target::x86_64_linux) {
            LCC_ASSERT(false, "TODO: SysV x86_64 Calling Convention Lowering");
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
            LCC_ASSERT(false, "TODO: Emit gnu assembly");
        } break;
    }
}

} // namespace lcc
