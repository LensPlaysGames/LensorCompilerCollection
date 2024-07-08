#include <lcc/codegen/mir.hh>
#include <lcc/codegen/x86_64/x86_64.hh>
#include <lcc/context.hh>
#include <lcc/ir/ir.hh>
#include <lcc/ir/module.hh>
#include <lcc/target.hh>
#include <unordered_map>
#include <vector>

namespace lcc {

static auto ir_nary_inst_kind_to_mir(Value::Kind kind) -> MInst::Kind {
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
        case Value::Kind::GetMemberPtr:
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

auto Module::mir() -> std::vector<MFunction> {
    if (_ctx->should_print_mir())
        print_ir(_ctx->use_colour_diagnostics());

    // Begin MIR generation by assigning virtual registers to each and every
    // value in the IR. While the IR can reference the direct result of a
    // value as an operand, machines generally can't. The best a machine can
    // do is put the result of a certain computation in a specific register,
    // and then later, if you haven't done anything to clobber it, that result
    // will still be there. A virtual register is exactly that.
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
        virts[v] = next_vreg();
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

                    // Instructions with no *IR* operands
                    case Value::Kind::Alloca:
                    case Value::Kind::Unreachable:
                        break;

                    case Value::Kind::Branch: break;

                    case Value::Kind::Copy: {
                        assign_virtual_register(as<CopyInst>(instruction)->operand());
                    } break;

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
                        for (auto& arg : call->args())
                            assign_virtual_register(arg);
                    } break;

                    case Value::Kind::GetElementPtr: {
                        auto gep = as<GEPInst>(instruction);
                        assign_virtual_register(gep->ptr());
                        assign_virtual_register(gep->idx());
                    } break;

                    case Value::Kind::GetMemberPtr: {
                        auto gmp = as<GetMemberPtrInst>(instruction);
                        assign_virtual_register(gmp->ptr());
                        assign_virtual_register(gmp->idx());
                    } break;

                    case Value::Kind::Intrinsic:
                        // TODO: Assign virtual register to intrinsic operands, if any
                        break;

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
    const auto MOperandValueReference = [&](Function* f_ir, MFunction& f, Value* v) -> MOperand {
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
                auto* param = as<Parameter>(v);
                // Best effort handling of parameter. May ICE in cases of multiple
                // register parameters, as a single MOperand must not reference both of
                // those registers; this would have to be split into multiple instructions
                // with multiple operands, which we can't do /from here/. So, for the
                // specific cases where we want to handle over-large values, we will.
                // Otherwise, this will handle the general case of single-register
                // parameters and memory parameters being referenced.
                if (_ctx->target()->is_x64()) {
                    if (_ctx->target()->is_windows()) {
                        if (param->type()->bytes() <= 8 and param->index() < 4) {
                            static constexpr x86_64::RegisterId reg_by_param_index[4]{
                                x86_64::RegisterId::RCX,
                                x86_64::RegisterId::RDX,
                                x86_64::RegisterId::R8,
                                x86_64::RegisterId::R9};
                            return MOperandRegister(
                                +reg_by_param_index[param->index()],
                                uint(param->type()->bits())
                            );
                        } else LCC_ASSERT(false, "TODO: Handle x64cc memory parameter");
                    } else if (_ctx->target()->is_linux()) {
                        usz sysv_registers_used = 0;
                        for (usz i = 0; i < param->index(); ++i) {
                            auto p = f_ir->param(i);
                            if (p->type()->bytes() <= 8 and sysv_registers_used < 6)
                                ++sysv_registers_used;
                            else if (p->type()->bytes() <= 16 and sysv_registers_used < 5)
                                sysv_registers_used += 2;
                        }

                        static constexpr x86_64::RegisterId reg_by_param_index[6]{
                            x86_64::RegisterId::RDI,
                            x86_64::RegisterId::RSI,
                            x86_64::RegisterId::RDX,
                            x86_64::RegisterId::RCX,
                            x86_64::RegisterId::R8,
                            x86_64::RegisterId::R9};

                        // Single register parameter
                        if (param->type()->bytes() <= 8 and sysv_registers_used < 6) {
                            return MOperandRegister(
                                +reg_by_param_index[param->index()],
                                uint(param->type()->bits())
                            );
                        }
                        // Multiple register parameter
                        else if (param->type()->bytes() <= 16 and sysv_registers_used < 5)
                            LCC_ASSERT(false, "Cannot handle multiple register parameter in this way");
                        else LCC_ASSERT(false, "TODO: SysV lowering of memory parameter");
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
                return MOperandGlobal{as<GlobalVariable>(v)};

            case Value::Kind::IntegerConstant:
                return MOperandImmediate{*as<IntegerConstant>(v)->value(), uint(v->type()->bits())};

            case Value::Kind::ArrayConstant:
                LCC_ASSERT(false, "TODO: MIR generation from array constant");

            case Value::Kind::Poison:
                Diag::ICE("Cannot generate MIR from poison IR value");

            default: break;
        }
        return MOperandRegister{virts[v], uint(regsize)};
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

    // Now that the vectors won't be resizing, we can put MFunction and MBlock
    // pointers into the Functions and Blocks they originate from.
    // NOTE: I may want to make a "locking" vector that can be locked and
    // never resized again, to assert that all iterators taken will never
    // invalidate.
    for (auto [f_index, function] : vws::enumerate(code())) {
        auto& f = funcs.at(usz(f_index));
        function->machine_function(&f);
        for (auto [block_index, block] : vws::enumerate(function->blocks())) {
            auto& bb = f.blocks().at(usz(block_index));
            block->machine_block(&bb);
        }
    }

    for (auto [f_index, function] : vws::enumerate(code())) {
        auto& f = funcs.at(usz(f_index));
        for (auto [block_index, block] : vws::enumerate(function->blocks())) {
            auto& bb = f.blocks().at(usz(block_index));
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
                    case Value::Kind::Copy: {
                        auto copy_ir = as<CopyInst>(instruction);
                        auto copy = MInst(MInst::Kind::Copy, {virts[instruction], uint(copy_ir->type()->bits())});
                        copy.add_operand(MOperandValueReference(function, f, copy_ir->operand()));
                        bb.add_instruction(copy);
                    } break;

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

                        auto phi = MInst(MInst::Kind::Phi, {virts[instruction], uint(phi_ir->type()->bits())});
                        for (auto& op : phi_ir->operands()) {
                            phi.add_operand(MOperandValueReference(function, f, op.block));
                            phi.add_operand(MOperandValueReference(function, f, op.value));
                        }
                        bb.add_instruction(phi);
                    } break;

                    case Value::Kind::Call: {
                        auto call_ir = as<CallInst>(instruction);

                        if (_ctx->target()->is_x64()) {
                            if (_ctx->target()->is_windows()) {
                                std::vector<usz> arg_regs = {
                                    +x86_64::RegisterId::RCX,
                                    +x86_64::RegisterId::RDX,
                                    +x86_64::RegisterId::R8,
                                    +x86_64::RegisterId::R9,
                                };

                                for (auto [arg_i, arg] : vws::enumerate(call_ir->args())) {
                                    if (usz(arg_i) < arg_regs.size() and arg->type()->bytes() <= 8) {
                                        auto copy = MInst(MInst::Kind::Copy, {arg_regs[usz(arg_i)], 64});
                                        copy.add_operand(MOperandValueReference(function, f, arg));
                                        bb.add_instruction(copy);
                                    } else {
                                        LCC_ASSERT(false, "Handle gMIR lowering of x64 memory arguments");
                                    }
                                }

                            } else if (_ctx->target()->is_linux()) {
                                std::vector<usz> arg_regs = {
                                    +x86_64::RegisterId::RDI,
                                    +x86_64::RegisterId::RSI,
                                    +x86_64::RegisterId::RDX,
                                    +x86_64::RegisterId::RCX,
                                    +x86_64::RegisterId::R8,
                                    +x86_64::RegisterId::R9,
                                };

                                const usz arg_reg_total = arg_regs.size();
                                usz arg_regs_used = 0;
                                for (auto [arg_i, arg] : vws::enumerate(call_ir->args())) {
                                    if (arg_regs_used < arg_reg_total and arg->type()->bytes() <= 8) {
                                        // TODO: May have to quantize arg->type()->bits() to 8, 16, 32, 64
                                        auto copy = MInst(MInst::Kind::Copy, {arg_regs[arg_regs_used++], uint(arg->type()->bits())});
                                        copy.add_operand(MOperandValueReference(function, f, arg));
                                        bb.add_instruction(copy);
                                    } else if (arg_regs_used < arg_reg_total - 1 and arg->type()->bytes() <= 16) {
                                        auto load_a = MInst(MInst::Kind::Load, {arg_regs[arg_regs_used++], 64});
                                        auto load_b = MInst(MInst::Kind::Load, {arg_regs[arg_regs_used++], uint(arg->type()->bits() - 64)});

                                        if (auto load_arg = cast<LoadInst>(arg)) {
                                            if (auto alloca = cast<AllocaInst>(load_arg->ptr())) {
                                                load_a.add_operand(MOperandValueReference(function, f, alloca));

                                                auto add_b = MInst(MInst::Kind::Add, {next_vreg(), 64});
                                                add_b.add_operand(MOperandValueReference(function, f, alloca));
                                                add_b.add_operand(MOperandImmediate(8, 32));

                                                load_b.add_operand(MOperandRegister(add_b.reg(), uint(add_b.regsize())));
                                                bb.add_instruction(add_b);

                                                // In doing the copying and stuff, we have effectively loaded the thing
                                                // manually. So, we remove the load that was there before.
                                                bb.remove_inst_by_reg(virts[load_arg]);

                                            } else LCC_ASSERT(false, "TODO: Create a temporary, store into it, and then treat argument like any other alloca.");
                                        } else if (arg->kind() == Value::Kind::Alloca) {
                                            load_a.add_operand(MOperandValueReference(function, f, arg));

                                            auto add_b = MInst(MInst::Kind::Add, {next_vreg(), 64});
                                            add_b.add_operand(MOperandValueReference(function, f, arg));
                                            add_b.add_operand(MOperandImmediate(8, 32));

                                            load_b.add_operand(MOperandRegister(add_b.reg(), uint(add_b.regsize())));
                                            bb.add_instruction(add_b);
                                        } else {
                                            fmt::print("{}\n", *arg->type());
                                            LCC_ASSERT(false, "Handle gMIR lowering of SysV multiple register argument");
                                        }

                                        bb.add_instruction(load_a);
                                        bb.add_instruction(load_b);
                                    } else {
                                        LCC_ASSERT(false, "Handle gMIR lowering of SysV memory argument");
                                    }
                                }
                            }
                        } else (LCC_ASSERT(false, "Unhandled architecture in gMIR generation from IR call"));

                        auto call = MInst(MInst::Kind::Call, {virts[instruction], uint(call_ir->function_type()->ret()->bits())});
                        call.add_operand(MOperandValueReference(function, f, call_ir->callee()));
                        bb.add_instruction(call);
                    } break;

                    case Value::Kind::Intrinsic: {
                        auto intrinsic = as<IntrinsicInst>(instruction);
                        switch (intrinsic->intrinsic_kind()) {
                            case IntrinsicKind::MemCopy: {
                                LCC_ASSERT(intrinsic->operands().size() == 3, "Invalid number of operands to memcpy intrinsic");

                                std::vector<usz> arg_regs{};
                                if (_ctx->target()->is_windows()) {
                                    arg_regs = {
                                        +x86_64::RegisterId::RCX,
                                        +x86_64::RegisterId::RDX,
                                        +x86_64::RegisterId::R8,
                                        +x86_64::RegisterId::R9,
                                    };
                                } else if (_ctx->target()->is_linux()) {
                                    arg_regs = {
                                        +x86_64::RegisterId::RDI,
                                        +x86_64::RegisterId::RSI,
                                        +x86_64::RegisterId::RDX,
                                        +x86_64::RegisterId::RCX,
                                        +x86_64::RegisterId::R8,
                                        +x86_64::RegisterId::R9,
                                    };
                                } else {
                                    Diag::ICE("Unhandled target in argument lowering for memcpy intrinsic");
                                }

                                usz arg_regs_used = 0;
                                for (auto op : intrinsic->operands()) {
                                    auto copy = MInst(MInst::Kind::Copy, {arg_regs[arg_regs_used++], uint(op->type()->bits())});
                                    copy.add_operand(MOperandValueReference(function, f, op));
                                    bb.add_instruction(copy);
                                }

                                auto call = MInst(MInst::Kind::Call, {0, 0});
                                auto memcpy_ty = FunctionType::Get(
                                    _ctx,
                                    Type::VoidTy,
                                    {Type::PtrTy,
                                     Type::PtrTy,
                                     IntegerType::Get(_ctx, 32)}
                                );
                                auto memcpy_function = new (*this) Function(this, "memcpy", memcpy_ty, Linkage::Imported, CallConv::C);
                                call.add_operand(memcpy_function);
                                bb.add_instruction(call);
                            } break;

                            case IntrinsicKind::DebugTrap:
                            case IntrinsicKind::MemSet:
                            case IntrinsicKind::SystemCall:
                                LCC_TODO("Generate MIR for IntrinsicInst");
                        }
                    } break;

                    case Value::Kind::GetElementPtr: {
                        auto gep_ir = as<GEPInst>(instruction);
                        auto reg = Register{virts[instruction], uint(gep_ir->type()->bits())};

                        if (auto idx = cast<IntegerConstant>(gep_ir->idx())) {
                            usz offset = gep_ir->base_type()->bytes() * idx->value().value();

                            if (not offset) {
                                auto copy = MInst(MInst::Kind::Copy, reg);
                                copy.add_operand(MOperandValueReference(function, f, gep_ir->ptr()));
                                bb.add_instruction(copy);
                                break;
                            }

                            auto add = MInst(MInst::Kind::Add, reg);
                            add.add_operand(MOperandValueReference(function, f, gep_ir->ptr()));
                            add.add_operand(MOperandImmediate(offset, 32));

                            usz use_count = gep_ir->users().size();
                            while (use_count--) add.add_use();

                            bb.add_instruction(add);
                            break;
                        }

                        auto mul = MInst(MInst::Kind::Mul, reg);
                        mul.add_operand(MOperandImmediate(gep_ir->base_type()->bytes(), 32));
                        mul.add_operand(MOperandValueReference(function, f, gep_ir->idx()));

                        auto add = MInst(MInst::Kind::Add, reg);
                        add.add_operand(MOperandValueReference(function, f, gep_ir->ptr()));
                        add.add_operand(reg);

                        usz use_count = gep_ir->users().size();
                        while (use_count--) add.add_use();

                        bb.add_instruction(mul);
                        bb.add_instruction(add);
                    } break;

                    case Value::Kind::GetMemberPtr: {
                        auto gmp_ir = as<GetMemberPtrInst>(instruction);
                        auto reg = Register{virts[instruction], uint(gmp_ir->type()->bits())};

                        LCC_ASSERT(
                            gmp_ir->idx()->kind() == Value::Kind::IntegerConstant,
                            "Sorry, but gMIR lowering of GetMemberPtr requires constant member index"
                        );

                        auto member_index = as<IntegerConstant>(gmp_ir->idx())->value().value();

                        usz offset = 0;
                        for (auto [member_idx, member_ty] : vws::enumerate(as<StructType>(gmp_ir->struct_type())->members())) {
                            if (usz(member_idx) >= member_index) break;
                            offset += member_ty->bytes();
                        }

                        if (not offset) {
                            auto copy = MInst(MInst::Kind::Copy, reg);
                            copy.add_operand(MOperandValueReference(function, f, gmp_ir->ptr()));
                            bb.add_instruction(copy);
                            break;
                        }

                        auto add = MInst(MInst::Kind::Add, reg);
                        add.add_operand(MOperandValueReference(function, f, gmp_ir->ptr()));
                        add.add_operand(MOperandImmediate(offset, 32));

                        bb.add_instruction(add);
                    } break;

                    case Value::Kind::Branch: {
                        auto branch_ir = as<BranchInst>(instruction);
                        // A branch does not produce a useable value, and as such it's register
                        // size is zero.
                        auto branch = MInst(MInst::Kind::Branch, {virts[instruction], 0});
                        auto op = MOperandValueReference(function, f, branch_ir->target());
                        branch.add_operand(op);
                        bb.add_instruction(branch);
                        if (std::holds_alternative<MOperandBlock>(op)) {
                            MOperandBlock block = std::get<MOperandBlock>(op);
                            bb.add_successor(block->machine_block()->name());
                            block->machine_block()->add_predecessor(bb.name());
                        }
                    } break;

                    case Value::Kind::CondBranch: {
                        auto branch_ir = as<CondBranchInst>(instruction);
                        // A branch does not produce a useable value, and as such it's register
                        // size is zero.
                        auto branch = MInst(MInst::Kind::CondBranch, {virts[instruction], 0});
                        branch.add_operand(MOperandValueReference(function, f, branch_ir->cond()));
                        auto then_op = MOperandValueReference(function, f, branch_ir->then_block());
                        auto else_op = MOperandValueReference(function, f, branch_ir->else_block());
                        branch.add_operand(then_op);
                        branch.add_operand(else_op);
                        bb.add_instruction(branch);
                        if (std::holds_alternative<MOperandBlock>(then_op)) {
                            MOperandBlock block = std::get<MOperandBlock>(then_op);
                            bb.add_successor(block->machine_block()->name());
                            block->machine_block()->add_predecessor(bb.name());
                        }
                        if (std::holds_alternative<MOperandBlock>(else_op)) {
                            MOperandBlock block = std::get<MOperandBlock>(else_op);
                            bb.add_successor(block->machine_block()->name());
                            block->machine_block()->add_predecessor(bb.name());
                        }
                    } break;

                    case Value::Kind::Unreachable: {
                        // Unreachable does not produce a useable value, and as such it's register
                        // size is zero.
                        auto unreachable = MInst(MInst::Kind::Unreachable, {virts[instruction], 0});
                        bb.add_instruction(unreachable);
                    } break;

                    case Value::Kind::Store: {
                        auto store_ir = as<StoreInst>(instruction);

                        // Special case lowering of storing multiple register parameter.
                        if (auto param = cast<Parameter>(store_ir->val())) {
                            // TODO FOR FUN: Functions marked internal we can do all the fucky wucky
                            // to, to make more efficient-like.
                            // FIXME: What does f.calling_convention() (C, Intercept, Laye) have to do
                            // with any of this?
                            if (_ctx->target()->is_x64() and _ctx->target()->is_linux()) {
                                usz sysv_registers_used = 0;
                                for (usz i = 0; i < param->index(); ++i) {
                                    auto p = function->param(i);
                                    if (p->type()->bytes() <= 8 and sysv_registers_used < 6)
                                        ++sysv_registers_used;
                                    else if (p->type()->bytes() <= 16 and sysv_registers_used < 5)
                                        sysv_registers_used += 2;
                                }

                                static constexpr x86_64::RegisterId reg_by_param_index[6]{
                                    x86_64::RegisterId::RDI,
                                    x86_64::RegisterId::RSI,
                                    x86_64::RegisterId::RDX,
                                    x86_64::RegisterId::RCX,
                                    x86_64::RegisterId::R8,
                                    x86_64::RegisterId::R9};

                                // Multiple register parameter
                                if (param->type()->bytes() > 8 and param->type()->bytes() <= 16 and sysv_registers_used < 5) {
                                    if (auto alloca = cast<AllocaInst>(store_ir->ptr())) {
                                        // Multiple register parameter stored into alloca
                                        auto reg_a = MOperandRegister(
                                            +reg_by_param_index[sysv_registers_used++],
                                            64
                                        );
                                        auto reg_b = MOperandRegister(
                                            +reg_by_param_index[sysv_registers_used++],
                                            uint(param->type()->bits() - 64)
                                        );

                                        auto store_a = MInst(MInst::Kind::Store, {virts[instruction], 0});
                                        store_a.add_operand(reg_a);
                                        store_a.add_operand(MOperandValueReference(function, f, alloca));

                                        auto add_b = MInst(MInst::Kind::Add, {next_vreg(), 64});
                                        add_b.add_operand(MOperandValueReference(function, f, alloca));
                                        add_b.add_operand(MOperandImmediate(8, 32));

                                        auto store_b = MInst(MInst::Kind::Store, {virts[instruction], 0});
                                        store_b.add_operand(reg_b);
                                        store_b.add_operand(MOperandRegister(add_b.reg(), uint(add_b.regsize())));

                                        bb.add_instruction(store_a);
                                        bb.add_instruction(add_b);
                                        bb.add_instruction(store_b);
                                        break; // Value::Kind::Store
                                    }
                                    LCC_UNREACHABLE();
                                }
                            }
                        }

                        // A store does not produce a useable value, and as such it's register
                        // size is zero.
                        auto store = MInst(MInst::Kind::Store, {virts[instruction], 0});
                        store.add_operand(MOperandValueReference(function, f, store_ir->val()));
                        store.add_operand(MOperandValueReference(function, f, store_ir->ptr()));
                        bb.add_instruction(store);
                    } break;

                    case Value::Kind::Load: {
                        auto load_ir = as<LoadInst>(instruction);
                        auto load = MInst(MInst::Kind::Load, {virts[instruction], uint(load_ir->type()->bits())});
                        load.add_operand(MOperandValueReference(function, f, load_ir->ptr()));
                        bb.add_instruction(load);
                    } break;

                    case Value::Kind::Return: {
                        auto ret_ir = as<ReturnInst>(instruction);
                        auto func_type = as<FunctionType>(function->type());
                        auto ret_type_bytes = func_type->ret()->bytes();

                        // SysV return in two registers
                        if (_ctx->target()->is_linux()) {
                            if (ret_ir->has_value() and ret_type_bytes > 8 and ret_type_bytes <= 16) {
                                auto load_a = MInst(MInst::Kind::Load, {usz(x86_64::RegisterId::RAX), 64});
                                load_a.add_operand(MOperandValueReference(function, f, ret_ir->val()));

                                // Add eight bytes to pointer to load from next.
                                auto add_b = MInst(MInst::Kind::Add, {next_vreg(), 64});
                                add_b.add_operand(MOperandValueReference(function, f, ret_ir->val()));
                                add_b.add_operand(MOperandImmediate(8, 32));

                                auto load_b = MInst(MInst::Kind::Load, {usz(x86_64::RegisterId::RDX), uint(func_type->ret()->bits() - 64)});
                                load_b.add_operand(MOperandRegister(add_b.reg(), uint(add_b.regsize())));

                                bb.add_instruction(load_a);
                                bb.add_instruction(add_b);
                                bb.add_instruction(load_b);
                                bb.add_instruction(MInst(MInst::Kind::Return, {0, 0}));
                                break;
                            }
                        }

                        usz regsize = 0;
                        if (ret_ir->has_value()) regsize = ret_ir->val()->type()->bits();
                        auto ret = MInst(MInst::Kind::Return, {virts[instruction], uint(regsize)});
                        if (ret_ir->has_value())
                            ret.add_operand(MOperandValueReference(function, f, ret_ir->val()));
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
                        auto unary = MInst(ir_nary_inst_kind_to_mir(unary_ir->kind()), {virts[instruction], uint(unary_ir->type()->bits())});
                        unary.add_operand(MOperandValueReference(function, f, unary_ir->operand()));
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
                        auto binary = MInst(ir_nary_inst_kind_to_mir(binary_ir->kind()), {virts[instruction], uint(binary_ir->type()->bits())});
                        binary.add_operand(MOperandValueReference(function, f, binary_ir->lhs()));
                        binary.add_operand(MOperandValueReference(function, f, binary_ir->rhs()));
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

                        auto copy = MInst(MInst::Kind::Copy, {minst.reg(), uint(minst.regsize())});

                        usz uses = minst.use_count();
                        while (uses && uses--) copy.add_use();

                        if (std::holds_alternative<MOperandImmediate>(op)) {
                            copy.add_operand(std::get<MOperandImmediate>(op));
                        } else if (std::holds_alternative<MOperandRegister>(op)) {
                            copy.add_operand(std::get<MOperandRegister>(op));
                        } else if (std::holds_alternative<MOperandLocal>(op)) {
                            copy.add_operand(std::get<MOperandLocal>(op));
                        } else if (std::holds_alternative<MOperandGlobal>(op)) {
                            copy.add_operand(std::get<MOperandGlobal>(op));
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

    return funcs;
}

} // namespace lcc
