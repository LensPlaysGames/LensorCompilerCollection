#include <lcc/ir/module.hh>

#include <fmt/format.h>
#include <lcc/context.hh>
#include <lcc/format.hh>
#include <lcc/target.hh>
#include <lcc/utils.hh>
#include <lcc/utils/ir_printer.hh>
#include <lcc/ir/ir.hh>
#include <lcc/codegen/mir.hh>
#include <lcc/diags.hh>

#include <algorithm>
#include <unordered_set>

namespace lcc {

// TODO: Use this, copy it out of `Module::lower()`
static void lower_windows_x64() {
}

void Module::lower() {
    if (_ctx->target()->is_x64()) {

        for (auto function : code()) {
            for (auto block : function->blocks()) {
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
                    default: break;
                    }
                }
            }
        }

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
            auto machine_ir = mir();

            LCC_ASSERT(false, "TODO: Emit gnu assembly");
        } break;
    }
}

auto Module::mir() -> std::vector<MFunction> {
    usz virtual_register = 0x420;
    std::unordered_map<Value*, usz> virts{};
    const auto assign_virtual_register = [&](Value* v) {
        if (virts[v]) return; // don't double-assign registers
        virts[v] = ++virtual_register;
    };

    // virtual register assignment
    for (auto function : code()) {
        assign_virtual_register(function);
        for (auto block : function->blocks()) {
            assign_virtual_register(block);
            for (auto instruction : block->instructions()) {
                assign_virtual_register(instruction);
                switch (instruction->kind()) {

                    // Non-instructions
                    case Value::Kind::Function:
                    case Value::Kind::Block:
                    case Value::Kind::IntegerConstant:
                    case Value::Kind::ArrayConstant:
                    case Value::Kind::Poison:
                    case Value::Kind::GlobalVariable:
                    case Value::Kind::Parameter:
                        LCC_UNREACHABLE();

                    case Value::Kind::Copy: break;

                    // Instructions with no *IR* operands
                    case Value::Kind::Alloca:
                    case Value::Kind::Unreachable:
                        break;

                    // Block operand handled above in block iteration
                    case Value::Kind::Branch: break;

                    // Block operands handled above in block iteration
                    case Value::Kind::CondBranch: {
                        assign_virtual_register(as<CondBranchInst>(instruction)->cond());
                    } break;

                    case Value::Kind::Return: {
                        auto ret = as<ReturnInst>(instruction);
                        if (ret->has_value()) assign_virtual_register(ret->val());
                    } break;

                    case Value::Kind::Call:
                    case Value::Kind::GetElementPtr:
                    case Value::Kind::Intrinsic:
                        LCC_TODO();

                    case Value::Kind::Load: {
                        assign_virtual_register(as<LoadInst>(instruction)->ptr());
                    } break;

                    case Value::Kind::Store: {
                        auto store = as<StoreInst>(instruction);
                        assign_virtual_register(store->ptr());
                        assign_virtual_register(store->val());
                    } break;

                    case Value::Kind::Phi: {
                        for (auto phi_operand : as<PhiInst>(instruction)->operands()) {
                            // phi_operand.block handled, or will be handled, in block iteration above.
                            assign_virtual_register(phi_operand.value);
                        }
                    } break;

                    // Unary
                    case Value::Kind::Bitcast:
                    case Value::Kind::ZExt:
                    case Value::Kind::SExt:
                    case Value::Kind::Trunc:
                    case Value::Kind::Neg:
                    case Value::Kind::Compl: {
                        auto unary = as<UnaryInstBase>(instruction);
                        assign_virtual_register(unary->operand());
                    } break;

                    // Binary
                    case Value::Kind::Add:
                    case Value::Kind::Sub:
                    case Value::Kind::Mul:
                    case Value::Kind::SDiv:
                    case Value::Kind::UDiv:
                    case Value::Kind::SRem:
                    case Value::Kind::URem:
                    case Value::Kind::Shl:
                    case Value::Kind::Sar:
                    case Value::Kind::Shr:
                    case Value::Kind::And:
                    case Value::Kind::Or:
                    case Value::Kind::Xor:
                    case Value::Kind::Eq:
                    case Value::Kind::Ne:
                    case Value::Kind::SLt:
                    case Value::Kind::SLe:
                    case Value::Kind::SGt:
                    case Value::Kind::SGe:
                    case Value::Kind::ULt:
                    case Value::Kind::ULe:
                    case Value::Kind::UGt:
                    case Value::Kind::UGe: {
                        auto binary = as<BinaryInst>(instruction);
                        assign_virtual_register(binary->lhs());
                        assign_virtual_register(binary->rhs());
                    } break;
                }
            }
        }
    }


    std::vector<MFunction> funcs{};
    for (auto function : code()) {
        funcs.push_back(MFunction());
        auto& f = funcs.back();
        f.name() = function->name();
        for (auto block : function->blocks()) {
            f.add_block(MBlock());
            auto& bb = f.blocks().back();
            for (auto instruction : block->instructions()) {
                switch (instruction->kind()) {

                    // Non-instructions
                    case Value::Kind::Function:
                    case Value::Kind::Block:
                    case Value::Kind::IntegerConstant:
                    case Value::Kind::ArrayConstant:
                    case Value::Kind::Poison:
                    case Value::Kind::GlobalVariable:
                    case Value::Kind::Parameter:
                        LCC_UNREACHABLE();

                    // Instructions
                    case Value::Kind::Copy: break;

                    case Value::Kind::Alloca: {
                        auto alloca_ir = as<AllocaInst>(instruction);
                        auto alloca = MInst(MInst::InstructionKind::Alloca);
                        alloca.add_operand(MOperandImmediate{alloca_ir->allocated_type()->bits()});
                        bb.add_instruction(alloca);
                    } break;

                    case Value::Kind::Call:
                    case Value::Kind::GetElementPtr:
                    case Value::Kind::Intrinsic:
                    case Value::Kind::Load:
                    case Value::Kind::Phi:
                    case Value::Kind::Store:
                    case Value::Kind::Branch:
                    case Value::Kind::CondBranch:
                    case Value::Kind::Return:
                    case Value::Kind::Unreachable:
                    case Value::Kind::ZExt:
                    case Value::Kind::SExt:
                    case Value::Kind::Trunc:
                    case Value::Kind::Bitcast:
                    case Value::Kind::Neg:
                    case Value::Kind::Compl:
                    case Value::Kind::Add:
                    case Value::Kind::Sub:
                    case Value::Kind::Mul:
                    case Value::Kind::SDiv:
                    case Value::Kind::UDiv:
                    case Value::Kind::SRem:
                    case Value::Kind::URem:
                    case Value::Kind::Shl:
                    case Value::Kind::Sar:
                    case Value::Kind::Shr:
                    case Value::Kind::And:
                    case Value::Kind::Or:
                    case Value::Kind::Xor:
                    case Value::Kind::Eq:
                    case Value::Kind::Ne:
                    case Value::Kind::SLt:
                    case Value::Kind::SLe:
                    case Value::Kind::SGt:
                    case Value::Kind::SGe:
                    case Value::Kind::ULt:
                    case Value::Kind::ULe:
                    case Value::Kind::UGt:
                    case Value::Kind::UGe: break;
                }
            }
        }
    }

    return funcs;
}


} // namespace lcc
