#include <algorithm>
#include <fmt/format.h>
#include <lcc/codegen/isel.hh>
#include <lcc/codegen/mir.hh>
#include <lcc/codegen/register_allocation.hh>
#include <lcc/codegen/x86_64.hh>
#include <lcc/codegen/x86_64/assembly.hh>
#include <lcc/context.hh>
#include <lcc/diags.hh>
#include <lcc/format.hh>
#include <lcc/ir/ir.hh>
#include <lcc/ir/module.hh>
#include <lcc/target.hh>
#include <lcc/utils.hh>
#include <lcc/utils/ir_printer.hh>
#include <unordered_set>
#include <variant>

namespace lcc {

u64 operator+(MOperandLocal l) { return static_cast<u64>(l); }

static MInst::Kind ir_nary_inst_kind_to_mir(Value::Kind kind) {
    switch (kind) {
        // Not unary or binary instructions
        case Value::Kind::Function:
        case Value::Kind::Block:
        case Value::Kind::IntegerConstant:
        case Value::Kind::ArrayConstant:
        case Value::Kind::Poison:
        case Value::Kind::GlobalVariable:
        case Value::Kind::Parameter:
        case Value::Kind::Copy:
        case Value::Kind::Alloca:
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
            LCC_UNREACHABLE();

        // Unary
        case Value::Kind::ZExt: return MInst::Kind::ZExt;
        case Value::Kind::SExt: return MInst::Kind::SExt;
        case Value::Kind::Trunc: return MInst::Kind::Trunc;
        case Value::Kind::Bitcast: return MInst::Kind::Bitcast;
        case Value::Kind::Neg: return MInst::Kind::Neg;
        case Value::Kind::Compl: return MInst::Kind::Compl;

        // Binary
        case Value::Kind::Add: return MInst::Kind::Add;
        case Value::Kind::Sub: return MInst::Kind::Sub;
        case Value::Kind::Mul: return MInst::Kind::Mul;
        case Value::Kind::SDiv: return MInst::Kind::SDiv;
        case Value::Kind::UDiv: return MInst::Kind::UDiv;
        case Value::Kind::SRem: return MInst::Kind::SRem;
        case Value::Kind::URem: return MInst::Kind::URem;
        case Value::Kind::Shl: return MInst::Kind::Shl;
        case Value::Kind::Sar: return MInst::Kind::Sar;
        case Value::Kind::Shr: return MInst::Kind::Shr;
        case Value::Kind::And: return MInst::Kind::And;
        case Value::Kind::Or: return MInst::Kind::Or;
        case Value::Kind::Xor: return MInst::Kind::Xor;
        case Value::Kind::Eq: return MInst::Kind::Eq;
        case Value::Kind::Ne: return MInst::Kind::Ne;
        case Value::Kind::SLt: return MInst::Kind::SLt;
        case Value::Kind::SLe: return MInst::Kind::SLe;
        case Value::Kind::SGt: return MInst::Kind::SGt;
        case Value::Kind::SGe: return MInst::Kind::SGe;
        case Value::Kind::ULt: return MInst::Kind::ULt;
        case Value::Kind::ULe: return MInst::Kind::ULe;
        case Value::Kind::UGt: return MInst::Kind::UGt;
        case Value::Kind::UGe: return MInst::Kind::UGe;
    }
    LCC_UNREACHABLE();
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

                            // Possiblities:
                            // - generate builtin memcpy for backend to handle
                            // - unroll into 8 byte loads, temporary pointer stored into then
                            //   incremented
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

    } else {
        LCC_ASSERT(false, "TODO: Lowering of specified arch is not yet supported");
    }
}

void Module::emit(std::filesystem::path output_file_path) {
    switch (_ctx->format()->format()) {
        case Format::INVALID: LCC_UNREACHABLE();
        case Format::LLVM_TEXTUAL_IR: {
            auto llvm_ir = llvm();
            if (output_file_path.empty())
                fmt::print("{}", llvm_ir);
            else {
                File::WriteOrTerminate(llvm_ir.c_str(), llvm_ir.size(), output_file_path);
            }
        } break;
        case Format::GNU_AS_ATT_ASSEMBLY: {
            auto machine_ir = mir();

            for (auto& mfunc : machine_ir)
                select_instructions(this, mfunc);

            // TODO: Register Allocation
            MachineDescription desc{};
            if (_ctx->target()->is_windows()) {
                // Just the volatile registers
                desc.registers = {
                    +x86_64::RegisterId::RAX,
                    +x86_64::RegisterId::RCX,
                    +x86_64::RegisterId::RDX,
                    +x86_64::RegisterId::R8,
                    +x86_64::RegisterId::R9,
                    +x86_64::RegisterId::R10,
                    +x86_64::RegisterId::R11,
                };
            } else {
                // Just the volatile registers
                desc.registers = {
                    +x86_64::RegisterId::RAX,
                    +x86_64::RegisterId::RCX,
                    +x86_64::RegisterId::RDX,
                    +x86_64::RegisterId::RSI,
                    +x86_64::RegisterId::RDI,
                    +x86_64::RegisterId::R8,
                    +x86_64::RegisterId::R9,
                    +x86_64::RegisterId::R10,
                    +x86_64::RegisterId::R11,
                };
            }
            for (auto& mfunc : machine_ir)
                allocate_registers(desc, mfunc);

            if (_ctx->target()->is_x64())
                x86_64::emit_gnu_att_assembly(output_file_path, this, machine_ir);
            else LCC_ASSERT(false, "Unhandled code emission target, sorry");
        } break;
    }
}

auto Module::mir() -> std::vector<MFunction> {
    usz virtual_register = 0x420;
    std::unordered_map<Value*, usz> virts{};
    const auto assign_virtual_register = [&](Value* v) {
        if (virts[v]) return; // don't double-assign registers
        switch (v->kind()) {
            // Non-instructions
            case Value::Kind::Function:
            case Value::Kind::Block:
            case Value::Kind::IntegerConstant:
            case Value::Kind::ArrayConstant:
            case Value::Kind::Poison:
            case Value::Kind::GlobalVariable:
            case Value::Kind::Parameter:
                return;

            // Instructions that can never produce a value
            case Value::Kind::Store:
            case Value::Kind::Branch:
            case Value::Kind::CondBranch:
            case Value::Kind::Return:
            case Value::Kind::Unreachable:
                return;

            default:
                break;
        }
        // TODO: Assign to this value that it is a defining use, somehow, for
        // register allocation purposes.
        virts[v] = ++virtual_register;
    };

    // virtual register assignment
    for (auto& function : code()) {
        for (auto& block : function->blocks()) {
            for (auto& instruction : block->instructions()) {
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

                    case Value::Kind::Branch: break;

                    case Value::Kind::CondBranch: {
                        assign_virtual_register(as<CondBranchInst>(instruction)->cond());
                    } break;

                    case Value::Kind::Return: {
                        auto ret = as<ReturnInst>(instruction);
                        if (ret->has_value()) assign_virtual_register(ret->val());
                    } break;

                    case Value::Kind::Call: {
                        auto call = as<CallInst>(instruction);
                        assign_virtual_register(call->callee());
                        for (auto& arg : call->args()) {
                            assign_virtual_register(arg);
                        }
                    } break;

                    case Value::Kind::GetElementPtr: {
                        auto gep = as<GEPInst>(instruction);
                        assign_virtual_register(gep->ptr());
                        assign_virtual_register(gep->idx());
                    } break;
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

    // Generate MIR
    std::vector<MFunction> funcs{};

    // Find machine instruction based on virtual register.
    const auto MInstByVirtualRegister = [&](usz virtual_register) -> MInst* {
        for (auto& mfunc : funcs) {
            for (auto& mblock : mfunc.blocks()) {
                for (auto& instruction : mblock.instructions()) {
                    if (instruction.reg() == virtual_register)
                        return &instruction;
                }
            }
        }
        return nullptr;
    };

    // Handle inlining of values into operands vs using register references.
    const auto MOperandValueReference = [&](MFunction& f, Value* v) -> MOperand {
        // Find MInst if possible, add to use count.
        usz regsize{0};
        if (auto inst = MInstByVirtualRegister(virts[v])) {
            inst->add_use();
            regsize = inst->regsize();
        }

        switch (v->kind()) {
            case Value::Kind::Function:
                return MOperandFunction(as<Function>(v));

            case Value::Kind::Block:
                return MOperandBlock(as<Block>(v));

            case Value::Kind::Parameter: {
                // Should probably do this some other way or put this somewhere else? Just
                // feels weird here.
                // Parameter Lowering
                // TODO FOR FUN: Functions marked internal we can do all the fucky wucky
                // to, to make more efficient-like.

                // FIXME: What does f.calling_convention() (C, Intercept, Laye) have to do
                // with any of this?

                if (_ctx->target()->is_x64()) {
                    if (_ctx->target()->is_windows()) {
                        // x64 Calling Convention lowering
                        auto parameter = as<Parameter>(v);
                        if (parameter->type()->bytes() <= 8 && parameter->index() < 4) {
                            static constexpr x86_64::RegisterId reg_by_param_index[4]{
                                x86_64::RegisterId::RCX,
                                x86_64::RegisterId::RDX,
                                x86_64::RegisterId::R8,
                                x86_64::RegisterId::R9};
                            return MOperandRegister{+reg_by_param_index[parameter->index()], parameter->type()->bits()};
                        } else {
                            LCC_ASSERT(parameter->type()->bytes() <= 8, "TODO: x64 stack parameter lowering");
                        }
                    } else if (_ctx->target()->is_linux()) {
                        // SysV x86_64 Calling Convention lowering
                        static constexpr x86_64::RegisterId reg_by_integer_param_index[6]{
                            x86_64::RegisterId::RDI,
                            x86_64::RegisterId::RSI,
                            x86_64::RegisterId::RDX,
                            x86_64::RegisterId::RCX,
                            x86_64::RegisterId::R8,
                            x86_64::RegisterId::R9};
                        auto parameter = as<Parameter>(v);
                        // TODO: Actual SysV classification
                        if (parameter->type()->bytes() <= 8 && f.sysv_integer_parameters_seen < 6) {
                            return MOperandRegister(+reg_by_integer_param_index[f.sysv_integer_parameters_seen++], parameter->type()->bits());
                        }
                        return MOperandRegister(0, 0);
                    }
                }

                LCC_UNREACHABLE();
            } break;

            case Value::Kind::Alloca: {
                auto found = std::find(f.locals().begin(), f.locals().end(), as<AllocaInst>(v));
                if (found == f.locals().end()) {
                    Diag::ICE("MIR Generation: encountered reference to local before it has been declared");
                }
                return MOperandLocal{u64(found - f.locals().begin())};
            }

            case Value::Kind::GlobalVariable:
                return MOperandStatic{as<GlobalVariable>(v)};

            case Value::Kind::IntegerConstant:
                return MOperandImmediate{as<IntegerConstant>(v)->value()};

            case Value::Kind::ArrayConstant:
                LCC_ASSERT(false, "TODO: MIR generation from array constant");

            case Value::Kind::Poison:
                Diag::ICE("Cannot generate MIR from poison IR value");

            default: break;
        }
        return MOperandRegister{virts[v], regsize};
    };

    // To avoid iterator invalidation when any of these vectors are resizing,
    // we "pre-construct" functions and blocks.
    for (auto& function : code()) {
        funcs.push_back(MFunction(function->linkage(), function->call_conv()));
        auto& f = funcs.back();
        f.name() = function->name();
        for (auto& block : function->blocks())
            f.add_block(MBlock(block->name()));
    }

    for (auto [f_index, function] : vws::enumerate(code())) {
        auto& f = funcs.at(usz(f_index));
        function->machine_function(&f);
        for (auto [block_index, block] : vws::enumerate(function->blocks())) {
            auto& bb = f.blocks().at(usz(block_index));
            block->machine_block(&bb);
            for (auto& instruction : block->instructions()) {
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

                    // Inlined
                    case Value::Kind::Alloca: {
                        f.add_local(as<AllocaInst>(instruction));
                    } break;

                    case Value::Kind::Phi: {
                        auto phi_ir = as<PhiInst>(instruction);

                        if (phi_ir->operands().empty()) {
                            if (not phi_ir->users().empty()) {
                                Diag::ICE("Cannot generate MIR from ill-formed IR: Phi instruction with no operands must not have any users.");
                            }
                            break;
                        }

                        auto phi = MInst(MInst::Kind::Phi, {virts[instruction], phi_ir->type()->bits()});
                        for (auto& op : phi_ir->operands()) {
                            phi.add_operand(MOperandValueReference(f, op.block));
                            phi.add_operand(MOperandValueReference(f, op.value));
                        }
                        bb.add_instruction(phi);
                    } break;

                    case Value::Kind::Call: {
                        auto call_ir = as<CallInst>(instruction);
                        auto call = MInst(MInst::Kind::Call, {virts[instruction], call_ir->function_type()->ret()->bits()});
                        call.add_operand(MOperandValueReference(f, call_ir->callee()));
                        for (auto& arg : call_ir->args()) {
                            call.add_operand(MOperandValueReference(f, arg));
                        }
                        bb.add_instruction(call);
                    } break;

                    case Value::Kind::Intrinsic: {
                        LCC_ASSERT(false, "TODO: Generate MIR for Intrinsic");
                    } break;

                    case Value::Kind::GetElementPtr: {
                        auto gep_ir = as<GEPInst>(instruction);
                        // FIXME: zero sized register
                        // TODO: ptr-width register.
                        auto gep = MInst(MInst::Kind::GetElementPtr, {virts[instruction], 0});
                        gep.add_operand(MOperandValueReference(f, gep_ir->ptr()));
                        gep.add_operand(MOperandValueReference(f, gep_ir->idx()));
                        bb.add_instruction(gep);
                    } break;

                    case Value::Kind::Branch: {
                        auto branch_ir = as<BranchInst>(instruction);
                        // A branch does not produce a useable value, and as such it's register
                        // size is zero.
                        auto branch = MInst(MInst::Kind::Branch, {virts[instruction], 0});
                        branch.add_operand(MOperandValueReference(f, branch_ir->target()));
                        bb.add_instruction(branch);
                    } break;

                    case Value::Kind::CondBranch: {
                        auto branch_ir = as<CondBranchInst>(instruction);
                        // A branch does not produce a useable value, and as such it's register
                        // size is zero.
                        auto branch = MInst(MInst::Kind::CondBranch, {virts[instruction], 0});
                        branch.add_operand(MOperandValueReference(f, branch_ir->cond()));
                        branch.add_operand(MOperandValueReference(f, branch_ir->then_block()));
                        branch.add_operand(MOperandValueReference(f, branch_ir->else_block()));
                        bb.add_instruction(branch);
                    } break;

                    case Value::Kind::Unreachable: {
                        // Unreachable does not produce a useable value, and as such it's register
                        // size is zero.
                        auto unreachable = MInst(MInst::Kind::Unreachable, {virts[instruction], 0});
                        bb.add_instruction(unreachable);
                    } break;

                    case Value::Kind::Store: {
                        auto store_ir = as<StoreInst>(instruction);
                        // A store does not produce a useable value, and as such it's register
                        // size is zero.
                        auto store = MInst(MInst::Kind::Store, {virts[instruction], 0});
                        store.add_operand(MOperandValueReference(f, store_ir->val()));
                        store.add_operand(MOperandValueReference(f, store_ir->ptr()));
                        bb.add_instruction(store);
                    } break;

                    case Value::Kind::Load: {
                        // FIXME: zero sized register
                        auto load_ir = as<LoadInst>(instruction);
                        auto load = MInst(MInst::Kind::Load, {virts[instruction], load_ir->type()->bits()});
                        load.add_operand(MOperandValueReference(f, load_ir->ptr()));
                        bb.add_instruction(load);
                    } break;

                    case Value::Kind::Return: {
                        auto ret_ir = as<ReturnInst>(instruction);
                        usz regsize = 0;
                        if (ret_ir->has_value()) regsize = ret_ir->val()->type()->bits();
                        auto ret = MInst(MInst::Kind::Return, {virts[instruction], regsize});
                        if (ret_ir->has_value())
                            ret.add_operand(MOperandValueReference(f, ret_ir->val()));
                        bb.add_instruction(ret);
                    } break;

                    // Unary
                    case Value::Kind::ZExt:
                    case Value::Kind::SExt:
                    case Value::Kind::Trunc:
                    case Value::Kind::Bitcast:
                    case Value::Kind::Neg:
                    case Value::Kind::Compl: {
                        auto unary_ir = as<UnaryInstBase>(instruction);
                        auto unary = MInst(ir_nary_inst_kind_to_mir(unary_ir->kind()), {virts[instruction], unary_ir->type()->bits()});
                        unary.add_operand(MOperandValueReference(f, unary_ir->operand()));
                        bb.add_instruction(unary);
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
                        auto binary_ir = as<BinaryInst>(instruction);
                        auto binary = MInst(ir_nary_inst_kind_to_mir(binary_ir->kind()), {virts[instruction], binary_ir->type()->bits()});
                        binary.add_operand(MOperandValueReference(f, binary_ir->lhs()));
                        binary.add_operand(MOperandValueReference(f, binary_ir->rhs()));
                        bb.add_instruction(binary);
                    } break;
                }
            }
        }
    }

    // Lowering
    for (auto& mfunc : funcs) {
        for (auto& mblock : mfunc.blocks()) {
            std::vector<isz> indices_of_instructions_to_remove{};
            for (auto [minst_index, minst] : vws::enumerate(mblock.instructions())) {
                // phi2copy
                if (minst.kind() == MInst::Kind::Phi) {
                    // Insert copy of each operand value into virtual register of phi
                    // MInst within block the value is coming from.
                    Block* block{};
                    MBlock* phi_operand_block{};
                    for (const auto& op : minst.all_operands()) {
                        // Phi's operands are arranged in groups of two in the form of <block, value>.
                        if (!block) {
                            block = std::get<MOperandBlock>(op);
                            phi_operand_block = block->machine_block();
                            LCC_ASSERT(phi_operand_block, "Cannot phi2copy block that has no corresponding machine block");
                            LCC_ASSERT(phi_operand_block != &mblock, "Cannot phi2copy when one of the Phi operands is in the same block as the Phi");
                            continue;
                        }

                        if (std::holds_alternative<MOperandBlock>(op))
                            Diag::ICE("Phi value cannot be a block");

                        auto copy = MInst(MInst::Kind::Copy, {minst.reg(), minst.regsize()});

                        usz uses = minst.use_count();
                        while (uses && uses--) copy.add_use();

                        if (std::holds_alternative<MOperandImmediate>(op)) {
                            copy.add_operand(std::get<MOperandImmediate>(op));
                        } else if (std::holds_alternative<MOperandRegister>(op)) {
                            copy.add_operand(std::get<MOperandRegister>(op));
                        } else if (std::holds_alternative<MOperandLocal>(op)) {
                            copy.add_operand(std::get<MOperandLocal>(op));
                        } else if (std::holds_alternative<MOperandStatic>(op)) {
                            copy.add_operand(std::get<MOperandStatic>(op));
                        } else if (std::holds_alternative<MOperandFunction>(op)) {
                            copy.add_operand(std::get<MOperandFunction>(op));
                        } else if (std::holds_alternative<MOperandBlock>(op)) {
                            copy.add_operand(std::get<MOperandBlock>(op));
                        }

                        phi_operand_block->add_instruction(copy, true);

                        block = nullptr;
                    }

                    LCC_ASSERT(not block, "Phi *must* have an even number of operands: incoming block and value pairs");

                    indices_of_instructions_to_remove.push_back(minst_index);
                }
            }
            for (isz index : indices_of_instructions_to_remove | vws::reverse) {
                mblock.instructions().erase(mblock.instructions().begin() + index);
            }
        }
    }

    fmt::print("{}", PrintMIR(vars(), funcs));

    return funcs;
}

} // namespace lcc
