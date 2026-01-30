#include <lcc/calling_conventions/ms_x64.hh>
#include <lcc/calling_conventions/sysv_x86_64.hh>
#include <lcc/codegen/mir.hh>
#include <lcc/codegen/x86_64/x86_64.hh>
#include <lcc/context.hh>
#include <lcc/ir/core.hh>
#include <lcc/ir/module.hh>
#include <lcc/target.hh>
#include <lcc/utils.hh>

#include <algorithm>
#include <unordered_map>
#include <variant>
#include <vector>

namespace lcc {

namespace {
constexpr auto ir_nary_inst_kind_to_mir(Value::Kind kind) -> MInst::Kind {
    using Kind = Value::Kind;
    using MKind = MInst::Kind;
    switch (kind) {
        // Not unary or binary instructions
        case Kind::Function:
        case Kind::Block:
        case Kind::IntegerConstant:
        case Kind::FractionalConstant:
        case Kind::ArrayConstant:
        case Kind::Poison:
        case Kind::GlobalVariable:
        case Kind::Parameter:
        case Kind::Copy:
        case Kind::Alloca:
        case Kind::Call:
        case Kind::GetElementPtr:
        case Kind::GetMemberPtr:
        case Kind::Intrinsic:
        case Kind::Load:
        case Kind::Phi:
        case Kind::Store:
        case Kind::Branch:
        case Kind::CondBranch:
        case Kind::Return:
        case Kind::Unreachable:
            LCC_UNREACHABLE();

        // Unary
        case Kind::ZExt: return MKind::ZExt;
        case Kind::SExt: return MKind::SExt;
        case Kind::Trunc: return MKind::Trunc;
        case Kind::Bitcast: return MKind::Bitcast;
        case Kind::Neg: return MKind::Neg;
        case Kind::Compl: return MKind::Compl;

        // Binary
        case Kind::Add: return MKind::Add;
        case Kind::Sub: return MKind::Sub;
        case Kind::Mul: return MKind::Mul;
        case Kind::SDiv: return MKind::SDiv;
        case Kind::UDiv: return MKind::UDiv;
        case Kind::SRem: return MKind::SRem;
        case Kind::URem: return MKind::URem;
        case Kind::Shl: return MKind::Shl;
        case Kind::Sar: return MKind::Sar;
        case Kind::Shr: return MKind::Shr;
        case Kind::And: return MKind::And;
        case Kind::Or: return MKind::Or;
        case Kind::Xor: return MKind::Xor;
        case Kind::Eq: return MKind::Eq;
        case Kind::Ne: return MKind::Ne;
        case Kind::SLt: return MKind::SLt;
        case Kind::SLe: return MKind::SLe;
        case Kind::SGt: return MKind::SGt;
        case Kind::SGe: return MKind::SGe;
        case Kind::ULt: return MKind::ULt;
        case Kind::ULe: return MKind::ULe;
        case Kind::UGt: return MKind::UGt;
        case Kind::UGe: return MKind::UGe;
    }
    LCC_UNREACHABLE();
}
} // namespace

auto Module::mir() -> std::vector<MFunction> {
    if (_ctx->option_print_mir())
        fmt::print("{}", as_lcc_ir(_ctx->option_use_colour()));

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
            // Instructions that can never produce a value
            case Value::Kind::Store:
            case Value::Kind::Branch:
            case Value::Kind::CondBranch:
            case Value::Kind::Return:
            case Value::Kind::Unreachable:
            // Non-instructions
            case Value::Kind::Function:
            case Value::Kind::Block:
            case Value::Kind::IntegerConstant:
            case Value::Kind::ArrayConstant:
            case Value::Kind::Poison:
            case Value::Kind::GlobalVariable:
            case Value::Kind::Parameter:
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
                    case Value::Kind::FractionalConstant:
                    case Value::Kind::ArrayConstant:
                    case Value::Kind::Poison:
                    case Value::Kind::GlobalVariable:
                    case Value::Kind::Parameter:
                        LCC_UNREACHABLE();

                    // Instructions that do not produce a value and do not have operands that
                    // produce a value.
                    case Value::Kind::Branch:
                    // Instructions with zero *IR* operands
                    case Value::Kind::Alloca:
                    case Value::Kind::Unreachable:
                        break;

                    case Value::Kind::Copy:
                        assign_virtual_register(as<CopyInst>(instruction)->operand());
                        break;

                    case Value::Kind::CondBranch:
                        assign_virtual_register(as<CondBranchInst>(instruction)->cond());
                        break;

                    case Value::Kind::Return: {
                        auto* ret = as<ReturnInst>(instruction);
                        if (ret->has_value()) assign_virtual_register(ret->val());
                    } break;

                    case Value::Kind::Call: {
                        auto* call = as<CallInst>(instruction);
                        assign_virtual_register(call->callee());
                        for (auto& arg : call->args())
                            assign_virtual_register(arg);
                    } break;

                    case Value::Kind::GetElementPtr: {
                        auto* gep = as<GEPInst>(instruction);
                        assign_virtual_register(gep->ptr());
                        assign_virtual_register(gep->idx());
                    } break;

                    case Value::Kind::GetMemberPtr: {
                        auto* gmp = as<GetMemberPtrInst>(instruction);
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
                        auto* store = as<StoreInst>(instruction);
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
                        auto* unary = as<UnaryInstBase>(instruction);
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
                        auto* binary = as<BinaryInst>(instruction);
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
    const auto MOperandValueReference
        = [&](Function* f_ir, MFunction& f, Value* v) -> MOperand {
        // Find MInst if possible, add to use count.
        usz regsize{0};
        if (auto* inst = MInstByVirtualRegister(virts[v])) {
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
                if (_ctx->target()->is_arch_x86_64()) {
                    if (_ctx->target()->is_platform_windows()) {
                        auto params_desc = cconv::msx64::parameter_description(f_ir);
                        auto param_info = params_desc.info.at(param->index());
                        switch (param_info.kind()) {
                            case cconv::msx64::ParameterDescription::Parameter::Kinds::SingleRegister:
                            // FIXME: Is this how PointerInRegister parameter is handled (here)?
                            // This may be the perfect spot to insert a dereference, if possible.
                            case cconv::msx64::ParameterDescription::Parameter::Kinds::PointerInRegister: {
                                // FIXME: Magic number. Why 2 registers wide? I believe this is to get
                                // past the stack frame, if present (i.e. saved base pointer), but I don't
                                // remember exactly.
                                i32 offset = 2 * x86_64::GeneralPurposeBytewidth;

                                // Return Local with positive offset into parent stack frame.
                                return MOperandLocal(
                                    MOperandLocal::absolute_index,
                                    offset + (8 * lcc::i32(param->index()))
                                );
                            }

                            case cconv::msx64::ParameterDescription::Parameter::Kinds::Stack: {
                                // Return Local with positive offset into parent stack frame.
                                // To get the actual offset, we need to know how many memory parameters
                                // come before this parameter, as well as their size (and alignment, I'd
                                // think).

                                // FIXME: Magic number. Why 2 registers wide? I believe this is to get
                                // past the stack frame, if present (i.e. saved base pointer), but I don't
                                // remember exactly.
                                i32 offset = 2 * x86_64::GeneralPurposeBytewidth;

                                // x64 calling convention requires caller to designate 32 bytes on the
                                // stack to save parameter registers into.
                                i32 shadow_stack_size = 32;
                                offset += shadow_stack_size;

                                // FIXME: I'm not sure if this should be stack_byte_offset (including size
                                // of current parameter), or if it should be stack_byte_offset_used
                                // (before the current parameter's size has been added).
                                offset += i32(param_info.stack_byte_offset_used);
                                return MOperandLocal(
                                    MOperandLocal::absolute_index,
                                    offset
                                );
                            }
                        }
                    } else if (_ctx->target()->is_cconv_sysv()) {
                        auto params_desc = cconv::sysv::parameter_description(f_ir);
                        auto param_info = params_desc.info.at(param->index());
                        switch (param_info.kind()) {
                            using Kinds = cconv::sysv::ParameterDescription::Parameter::Kinds;
                            case Kinds::SingleRegister:
                                return MOperandRegister(
                                    +cconv::sysv::arg_regs.at(param_info.arg_regs_used),
                                    uint(param->type()->bits())
                                );

                            case Kinds::DoubleRegister:
                                LCC_ASSERT(
                                    false,
                                    "Cannot handle multiple register parameter in this way."
                                    " IIRC you have to lower that out further up the chain in passes on LCC IR, or something."
                                );

                            case Kinds::Memory: {
                                // Return Local with positive offset into parent stack frame.
                                // To get the actual offset, we need to know how many memory parameters
                                // come before this parameter, as well as their size (and alignment, I'd
                                // think).
                                // FIXME: Magic number. Why 2 registers wide? I believe this is to get
                                // past the stack frame, if present (i.e. saved base pointer), but I don't
                                // remember exactly.
                                i32 offset = 2 * x86_64::GeneralPurposeBytewidth;
                                // FIXME: I'm not sure if this should be stack_byte_offset (including size
                                // of current parameter), or if it should be stack_byte_offset_used
                                // (before the current parameter's size has been added).
                                offset += i32(param_info.stack_byte_offset_used);
                                return MOperandLocal(
                                    MOperandLocal::absolute_index,
                                    offset
                                );
                            }
                        }
                    }
                }
                Diag::ICE("It appears we haven't handled the target properly, sorry");
            } break;

            case Value::Kind::Alloca: {
                auto found = std::find(
                    f.locals().begin(),
                    f.locals().end(),
                    as<AllocaInst>(v)
                );
                if (found == f.locals().end()) {
                    v->print();
                    fmt::print("\n");
                    Diag::ICE("MIR Generation: encountered reference to local before it has been declared");
                }
                return MOperandLocal{u32(found - f.locals().begin())};
            }

            case Value::Kind::GlobalVariable:
                return MOperandGlobal{as<GlobalVariable>(v)};

            case Value::Kind::IntegerConstant:
                return MOperandImmediate{
                    (usz) *as<IntegerConstant>(v)->value(),
                    uint(v->type()->bits()) //
                };

            case Value::Kind::FractionalConstant:
                LCC_TODO("MIR generation from fractional constant");

            case Value::Kind::ArrayConstant:
                LCC_TODO("MIR generation from array constant");

            case Value::Kind::Poison:
                Diag::ICE("Cannot generate MIR from poison IR value");

            // These value kinds are referenced through register operands.
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
            case Value::Kind::ZExt:
            case Value::Kind::SExt:
            case Value::Kind::Trunc:
            case Value::Kind::Bitcast:
            case Value::Kind::Neg:
            case Value::Kind::Copy:
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
            case Value::Kind::UGe:
                break;
        }

        auto register_category = Register::Category::DEFAULT;
        if (is<FractionalType>(v->type()))
            register_category = Register::Category::FLOAT;

        return MOperandRegister{
            virts[v],
            uint(regsize),
            register_category
        };
    };

    // NOTE: We cannot add functions to the IR while iterating over them (use-
    // after-free nightmare), so, please, do not alter the IR in the main loop
    // that generates MIR, for here there be dragons.

    // TODO: if memcpy already in module, use that
    auto* memcpy_ty = FunctionType::Get(
        _ctx,
        Type::VoidTy,
        {Type::PtrTy,
         Type::PtrTy,
         IntegerType::Get(_ctx, 32)}
    );
    auto* memcpy_function = new (*this) Function(
        this,
        "memcpy",
        memcpy_ty,
        Linkage::Imported,
        CallConv::C
    );

    // Give all the blocks unique names...
    // LCC MIR wants all blocks to have unique names.
    // It's important to make this change to the IR, since MIR block operands
    // reference back to the IR block.
    {
        usz block_count = 0;
        std::unordered_set<std::string> encountered_block_names{};
        for (auto [f_index, function] : vws::enumerate(code())) {
            for (auto [block_index, block] : vws::enumerate(function->blocks())) {
                // Only update block name if it is a duplicate.
                while (encountered_block_names.contains(block->name()))
                    block->name(block->name() + std::to_string(block_count));

                encountered_block_names.emplace(block->name());
                ++block_count;
            }
        }
    }

    // To avoid iterator invalidation when any of these vectors are resizing,
    // we "pre-construct" functions and blocks.
    for (auto& function : code()) {
        funcs.emplace_back(function->call_conv());
        auto& f = funcs.back();
        f.names() = function->names();
        f.location(function->location());
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

    // MSx64 ONLY: Store register parameters to reserved space (shadow stack)
    if (context()->target()->is_cconv_ms()) {
        constexpr lcc::i32 stack_frame_size = 16;
        for (auto [f_index, function] : vws::enumerate(code())) {
            auto& f = funcs.at(usz(f_index));
            if (f.blocks().empty()) continue;

            auto param_desc = cconv::msx64::parameter_description(function);
            auto& params = as<FunctionType>(function->type())->params();
            for (size_t param_i{0}; param_i < params.size(); ++param_i) {
                auto param_info = param_desc.info.at(param_i);
                if (param_info.arg_regs == 1) {
                    auto move = MInst(+x86_64::Opcode::MoveDereferenceRHS, {});
                    move.add_operand(
                        MOperandRegister(
                            +cconv::msx64::arg_regs.at(param_i),
                            x86_64::GeneralPurposeBitwidth
                        )
                    );
                    move.add_operand(
                        MOperandLocal(
                            MOperandLocal::absolute_index,
                            stack_frame_size + (8 * lcc::i32(param_i))
                        )
                    );
                    f.blocks().at(0).add_instruction(move);
                }
            }
        }
    }

    // The actual generation part
    for (auto [f_index, function] : vws::enumerate(code())) {
        auto& f = funcs.at(usz(f_index));
        for (auto [block_index, block] : vws::enumerate(function->blocks())) {
            auto& bb = f.blocks().at(usz(block_index));
            for (auto& instruction : block->instructions()) {
                auto register_category = Register::Category::UNSPECIFIED;
                if (is<FractionalType>(instruction->type()))
                    register_category = Register::Category::FLOAT;

                switch (instruction->kind()) {
                    // Non-instructions
                    case Value::Kind::Function:
                    case Value::Kind::Block:
                    case Value::Kind::IntegerConstant:
                    case Value::Kind::FractionalConstant:
                    case Value::Kind::ArrayConstant:
                    case Value::Kind::Poison:
                    case Value::Kind::GlobalVariable:
                    case Value::Kind::Parameter:
                        LCC_UNREACHABLE();

                    // Instructions
                    case Value::Kind::Copy: {
                        auto* copy_ir = as<CopyInst>(instruction);
                        auto copy = MInst(
                            MInst::Kind::Copy,
                            {virts[instruction],
                             uint(copy_ir->type()->bits()),
                             register_category}
                        );
                        copy.location(copy_ir->location());
                        copy.add_operand(MOperandValueReference(function, f, copy_ir->operand()));
                        bb.add_instruction(copy);
                    } break;

                    // Inlined
                    case Value::Kind::Alloca: {
                        f.add_local(as<AllocaInst>(instruction));
                    } break;

                    case Value::Kind::Phi: {
                        auto* phi_ir = as<PhiInst>(instruction);

                        if (phi_ir->operands().empty()) {
                            if (not phi_ir->users().empty()) {
                                Diag::ICE("Cannot generate MIR from ill-formed IR: Phi instruction with no operands must not have any users.");
                            }
                            break;
                        }

                        auto phi = MInst(
                            MInst::Kind::Phi,
                            {virts[instruction],
                             uint(phi_ir->type()->bits()),
                             register_category}
                        );
                        phi.location(phi_ir->location());
                        for (const auto& op : phi_ir->operands()) {
                            phi.add_operand(MOperandValueReference(function, f, op.block));
                            phi.add_operand(MOperandValueReference(function, f, op.value));
                        }
                        bb.add_instruction(phi);
                    } break;

                    case Value::Kind::Call: {
                        auto* call_ir = as<CallInst>(instruction);

                        usz arg_stack_bytes_used = 0;
                        if (_ctx->target()->is_arch_x86_64()) {
                            if (_ctx->target()->is_platform_windows()) {
                                std::vector<Type*> arg_types{};
                                rgs::transform(
                                    call_ir->args(),
                                    std::back_inserter(arg_types),
                                    [](auto* v) { return v->type(); }
                                );
                                auto param_desc = cconv::msx64::parameter_description(arg_types);

                                for (auto [arg_i, arg] : vws::enumerate(call_ir->args())) {
                                    auto param_info = param_desc.info.at(usz(arg_i));
                                    switch (param_info.kind()) {
                                        using Kinds = cconv::msx64::ParameterDescription::Parameter::Kinds;
                                        case Kinds::SingleRegister:
                                        // FIXME: Is PointerInRegister handled like this?
                                        case Kinds::PointerInRegister: {
                                            auto copy = MInst(
                                                MInst::Kind::Copy,
                                                {+cconv::msx64::arg_regs.at(usz(arg_i)),
                                                 x86_64::GeneralPurposeBitwidth,
                                                 register_category}
                                            );
                                            copy.location(call_ir->location());
                                            copy.add_operand(MOperandValueReference(function, f, arg));
                                            bb.add_instruction(copy);
                                        } break;

                                        case Kinds::Stack: {
                                            // Basically, if an argument is over-large, we allocate a copy on the
                                            // stack (that way the caller can modify without doing bad bad), and then
                                            // pass the pointer to the stack address in the regular place the argument
                                            // would have gone (either the register the argument would have fit in, or
                                            // on the stack).
                                            // "Structs or unions (larger than 64 bits) are passed as a pointer to
                                            // memory allocated by the caller. For these aggregate types passed as a
                                            // pointer, including __m128, the caller-allocated temporary memory must
                                            // be 16-byte aligned."
                                            LCC_TODO("Handle gMIR lowering of x64 memory arguments");
                                        }
                                    }
                                }
                            } else if (_ctx->target()->is_cconv_sysv()) {
                                std::vector<Type*> arg_types{};
                                rgs::transform(
                                    call_ir->args(),
                                    std::back_inserter(arg_types),
                                    [](auto* v) { return v->type(); }
                                );
                                auto param_desc = cconv::sysv::parameter_description(arg_types);

                                // Handle all arguments that are passed in memory first, before register arguments.
                                for (auto [arg_i, arg] : vws::enumerate(call_ir->args())) {
                                    auto arg_info = param_desc.info.at(usz(arg_i));
                                    // Memory parameter
                                    if (arg_info.is_memory()) {
                                        // Basically just allocate a temporary on the stack, memcpy (or similar)
                                        // into that.

                                        // Remove the original argument; we will be building it by hand.
                                        auto arg_mir = MOperandValueReference(function, f, arg);
                                        LCC_ASSERT(std::holds_alternative<MOperandRegister>(arg_mir));
                                        auto arg_reg = std::get<MOperandRegister>(arg_mir);
                                        bb.remove_inst_by_reg(arg_reg.value);

                                        // Get a reference to a pointer to the argument.
                                        Value* arg_ptr{nullptr};
                                        if (auto* load = cast<LoadInst>(arg))
                                            arg_ptr = load->ptr();
                                        else if (auto* alloca = cast<AllocaInst>(arg))
                                            arg_ptr = alloca;
                                        else LCC_ASSERT(
                                            false,
                                            "Memory argument must be prepared such that MIR generation may fetch the pointer (i.e. a LoadInst or AllocaInst).\n"
                                            "This allows us to copy from the pointer (load operand) onto the stack."
                                        );

                                        auto byte_count = arg->type()->bytes();

                                        // sub $<size>, %rsp
                                        constexpr Register stack_pointer_reg{+x86_64::RegisterId::RSP, 64};
                                        auto sub = MInst(MInst::Kind::Sub, stack_pointer_reg);
                                        sub.location(call_ir->location());
                                        sub.add_operand(stack_pointer_reg);
                                        sub.add_operand(MOperandImmediate(byte_count));
                                        bb.add_instruction(sub);

                                        // Record stack subtraction so we can undo it after the call
                                        arg_stack_bytes_used += byte_count;

                                        // Copy from arg into stack pointer

                                        // TODO: If memcpy sets return register we may end up having a bad time.

                                        { // Destination argument (stack pointer)
                                            auto copy = MInst(
                                                MInst::Kind::Copy,
                                                {+cconv::sysv::arg_regs.at(0), 64}
                                            );
                                            copy.location(call_ir->location());
                                            copy.add_operand(stack_pointer_reg);
                                            bb.add_instruction(copy);
                                        }
                                        { // Source argument
                                            auto copy = MInst(
                                                MInst::Kind::Copy,
                                                {+cconv::sysv::arg_regs.at(1), 64}
                                            );
                                            copy.location(call_ir->location());
                                            copy.add_operand(MOperandValueReference(function, f, arg_ptr));
                                            bb.add_instruction(copy);
                                        }
                                        { // Size argument
                                            auto copy = MInst(
                                                MInst::Kind::Copy,
                                                {+cconv::sysv::arg_regs.at(2), 64}
                                            );
                                            copy.location(call_ir->location());
                                            copy.add_operand(MOperandImmediate(byte_count));
                                            bb.add_instruction(copy);
                                        }

                                        auto call = MInst(
                                            MInst::Kind::Call,
                                            {usz(x86_64::RegisterId::RETURN), 0}
                                        );
                                        call.location(call_ir->location());
                                        call.add_operand(memcpy_function);
                                        bb.add_instruction(call);
                                    }
                                }

                                for (auto [arg_i, arg] : vws::enumerate(call_ir->args())) {
                                    auto param_info = param_desc.info.at(usz(arg_i));
                                    if (param_info.is_single_register()) {
                                        // TODO: May have to quantize arg->type()->bits() to 8, 16, 32, 64
                                        auto copy = MInst(
                                            MInst::Kind::Copy,
                                            {
                                                +cconv::sysv::arg_regs.at(param_info.arg_regs_used),
                                                uint(arg->type()->bits()) //
                                            }
                                        );
                                        copy.location(call_ir->location());
                                        copy.add_operand(MOperandValueReference(function, f, arg));
                                        bb.add_instruction(copy);
                                    } else if (param_info.is_double_register()) {
                                        auto load_a = MInst(
                                            MInst::Kind::Load,
                                            {
                                                +cconv::sysv::arg_regs.at(param_info.arg_regs_used),
                                                x86_64::GeneralPurposeBitwidth //
                                            }
                                        );
                                        auto load_b = MInst(
                                            MInst::Kind::Load,
                                            {
                                                +cconv::sysv::arg_regs.at(param_info.arg_regs_used + 1),
                                                uint(arg->type()->bits() - x86_64::GeneralPurposeBitwidth) //
                                            }
                                        );
                                        load_a.location(call_ir->location());
                                        load_b.location(call_ir->location());

                                        if (auto* load_arg = cast<LoadInst>(arg)) {
                                            if (auto* alloca = cast<AllocaInst>(load_arg->ptr())) {
                                                load_a.add_operand(MOperandValueReference(function, f, alloca));

                                                auto add_b = MInst(
                                                    MInst::Kind::Add,
                                                    {next_vreg(), x86_64::GeneralPurposeBitwidth}
                                                );
                                                add_b.location(call_ir->location());
                                                add_b.add_operand(MOperandValueReference(function, f, alloca));
                                                add_b.add_operand(MOperandImmediate(x86_64::GeneralPurposeBytewidth, 32));

                                                load_b.add_operand(MOperandRegister(add_b.reg(), uint(add_b.regsize())));
                                                bb.add_instruction(add_b);

                                                // In doing the copying and stuff, we have effectively loaded the thing
                                                // manually. So, we remove the load that was there before.
                                                bb.remove_inst_by_reg(virts[load_arg]);

                                            } else {
                                                LCC_TODO(
                                                    "Create a temporary, store into it, and then treat argument like any other alloca."
                                                );
                                            }
                                        } else if (arg->kind() == Value::Kind::Alloca) {
                                            load_a.add_operand(MOperandValueReference(function, f, arg));

                                            auto add_b = MInst(
                                                MInst::Kind::Add,
                                                {next_vreg(), x86_64::GeneralPurposeBitwidth}
                                            );
                                            add_b.location(call_ir->location());
                                            add_b.add_operand(MOperandValueReference(function, f, arg));
                                            add_b.add_operand(MOperandImmediate(x86_64::GeneralPurposeBytewidth, 32));

                                            load_b.add_operand(MOperandRegister(add_b.reg(), uint(add_b.regsize())));
                                            bb.add_instruction(add_b);
                                        } else {
                                            arg->print();
                                            LCC_TODO("Handle gMIR lowering of SysV multiple register argument");
                                        }

                                        bb.add_instruction(load_a);
                                        bb.add_instruction(load_b);
                                    }
                                }
                            }
                        } else (LCC_ASSERT(false, "Unhandled architecture in gMIR generation from IR call"));

                        auto call = MInst(
                            MInst::Kind::Call,
                            {virts[instruction],
                             uint(call_ir->function_type()->ret()->bits()),
                             register_category}
                        );
                        call.location(call_ir->location());
                        call.add_operand(MOperandValueReference(function, f, call_ir->callee()));
                        bb.add_instruction(call);

                        if (arg_stack_bytes_used) {
                            LCC_ASSERT(
                                _ctx->target()->is_arch_x86_64(),
                                "Handle architecture when resetting stack after a call"
                            );
                            auto stack_fixup = MInst(
                                +x86_64::Opcode::Add,
                                {+x86_64::RegisterId::RSP, x86_64::GeneralPurposeBitwidth}
                            );
                            stack_fixup.location(call_ir->location());
                            stack_fixup.add_operand(MOperandImmediate(arg_stack_bytes_used));
                            stack_fixup.add_operand(
                                MOperandRegister{
                                    +x86_64::RegisterId::RSP,
                                    x86_64::GeneralPurposeBitwidth //
                                }
                            );
                            bb.add_instruction(stack_fixup);
                        }
                    } break;

                    case Value::Kind::Intrinsic: {
                        auto intrinsic = as<IntrinsicInst>(instruction);
                        switch (intrinsic->intrinsic_kind()) {
                            case IntrinsicKind::MemCopy: {
                                LCC_ASSERT(intrinsic->operands().size() == 3, "Invalid number of operands to memcpy intrinsic");

                                std::vector<usz> arg_regs{};
                                // TODO: Static assert for handling of targets.
                                if (_ctx->target()->is_platform_windows()) {
                                    rgs::transform(
                                        cconv::msx64::arg_regs,
                                        std::back_inserter(arg_regs),
                                        [](auto r) { return +r; }
                                    );
                                } else if (_ctx->target()->is_cconv_sysv()) {
                                    rgs::transform(
                                        cconv::sysv::arg_regs,
                                        std::back_inserter(arg_regs),
                                        [](auto r) { return +r; }
                                    );
                                } else {
                                    Diag::ICE("Unhandled target in argument lowering for memcpy intrinsic");
                                }

                                usz arg_regs_used = 0;
                                for (auto op : intrinsic->operands()) {
                                    auto copy = MInst(
                                        MInst::Kind::Copy,
                                        {arg_regs.at(arg_regs_used++), uint(op->type()->bits())}
                                    );
                                    copy.location(intrinsic->location());
                                    copy.add_operand(MOperandValueReference(function, f, op));
                                    bb.add_instruction(copy);
                                }

                                auto call = MInst(MInst::Kind::Call, {0, 0});
                                call.location(intrinsic->location());
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
                        auto* gep_ir = as<GEPInst>(instruction);
                        Register reg{
                            virts[instruction],
                            uint(gep_ir->type()->bits()),
                            register_category
                        };

                        if (auto* idx = cast<IntegerConstant>(gep_ir->idx())) {
                            usz offset = gep_ir->base_type()->bytes() * (usz) idx->value().value();

                            if (not offset) {
                                auto copy = MInst(MInst::Kind::Copy, reg);
                                copy.location(gep_ir->location());
                                copy.add_operand(MOperandValueReference(function, f, gep_ir->ptr()));
                                bb.add_instruction(copy);
                                break;
                            }

                            auto add = MInst(MInst::Kind::Add, reg);
                            add.location(gep_ir->location());
                            add.add_operand(MOperandValueReference(function, f, gep_ir->ptr()));
                            add.add_operand(MOperandImmediate(offset, 32));

                            usz use_count = gep_ir->users().size();
                            while (use_count--) add.add_use();

                            bb.add_instruction(add);
                            break;
                        }

                        auto mul = MInst(MInst::Kind::Mul, reg);
                        mul.location(gep_ir->location());
                        mul.add_operand(MOperandImmediate(gep_ir->base_type()->bytes(), 32));
                        mul.add_operand(MOperandValueReference(function, f, gep_ir->idx()));

                        auto add = MInst(MInst::Kind::Add, reg);
                        add.location(gep_ir->location());
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

                        auto maybe_offset = gmp_ir->struct_type()->member_offset((usz) member_index);
                        LCC_ASSERT(maybe_offset);
                        auto offset = maybe_offset.value();

                        if (not offset) {
                            auto copy = MInst(MInst::Kind::Copy, reg);
                            copy.location(gmp_ir->location());
                            copy.add_operand(MOperandValueReference(function, f, gmp_ir->ptr()));
                            bb.add_instruction(copy);
                            break;
                        }

                        auto add = MInst(MInst::Kind::Add, reg);
                        add.location(gmp_ir->location());
                        add.add_operand(MOperandValueReference(function, f, gmp_ir->ptr()));
                        add.add_operand(MOperandImmediate(offset, 32));

                        bb.add_instruction(add);
                    } break;

                    case Value::Kind::Branch: {
                        auto* branch_ir = as<BranchInst>(instruction);
                        // A branch does not produce a useable value, and as such it's register
                        // size is zero.
                        auto branch = MInst(MInst::Kind::Branch, {virts[instruction], 0});
                        branch.location(branch_ir->location());
                        auto op = MOperandValueReference(function, f, branch_ir->target());
                        branch.add_operand(op);
                        bb.add_instruction(branch);

                        // MIR Control Flow Graph
                        if (std::holds_alternative<MOperandBlock>(op)) {
                            MOperandBlock destination_block = std::get<MOperandBlock>(op);
                            bb.add_successor(destination_block->machine_block()->name());
                            destination_block->machine_block()->add_predecessor(bb.name());
                        }
                    } break;

                    case Value::Kind::CondBranch: {
                        auto* branch_ir = as<CondBranchInst>(instruction);
                        // A branch does not produce a useable value, and as such it's register
                        // size is zero.
                        auto branch = MInst(
                            MInst::Kind::CondBranch,
                            {virts[instruction], 0}
                        );
                        branch.location(branch_ir->location());
                        branch.add_operand(MOperandValueReference(function, f, branch_ir->cond()));
                        auto then_op = MOperandValueReference(function, f, branch_ir->then_block());
                        auto else_op = MOperandValueReference(function, f, branch_ir->else_block());
                        branch.add_operand(then_op);
                        branch.add_operand(else_op);
                        bb.add_instruction(branch);

                        // MIR Control Flow Graph
                        if (std::holds_alternative<MOperandBlock>(then_op)) {
                            MOperandBlock then_block = std::get<MOperandBlock>(then_op);
                            bb.add_successor(then_block->machine_block()->name());
                            then_block->machine_block()->add_predecessor(bb.name());
                        }
                        if (std::holds_alternative<MOperandBlock>(else_op)) {
                            MOperandBlock else_block = std::get<MOperandBlock>(else_op);
                            bb.add_successor(else_block->machine_block()->name());
                            else_block->machine_block()->add_predecessor(bb.name());
                        }
                    } break;

                    case Value::Kind::Unreachable: {
                        // Unreachable does not produce a useable value, and as such it's register
                        // size is zero.
                        auto unreachable = MInst(
                            MInst::Kind::Unreachable,
                            {virts[instruction], 0}
                        );
                        unreachable.location(as<UnreachableInst>(instruction)->location());
                        bb.add_instruction(unreachable);
                    } break;

                    case Value::Kind::Store: {
                        auto* store_ir = as<StoreInst>(instruction);

                        // Special case lowering of storing multiple register return value..
                        if (
                            is<CallInst>(store_ir->val())
                            and _ctx->target()->is_arch_x86_64()
                            and _ctx->target()->is_cconv_sysv()
                            and store_ir->val()->type()->bits() > x86_64::GeneralPurposeBitwidth
                            and store_ir->val()->type()->bits() <= 2 * x86_64::GeneralPurposeBitwidth
                        ) {
                            // Multiple register return value stored into store's destination pointer
                            auto reg_a = MOperandRegister(
                                +x86_64::RegisterId::RAX,
                                x86_64::GeneralPurposeBitwidth
                            );
                            auto reg_b = MOperandRegister(
                                +x86_64::RegisterId::RDX,
                                uint(store_ir->val()->type()->bits() - x86_64::GeneralPurposeBitwidth)
                            );

                            auto store_a = MInst(MInst::Kind::Store, {virts[instruction], 0});
                            store_a.location(store_ir->location());
                            store_a.add_operand(reg_a);
                            store_a.add_operand(MOperandValueReference(function, f, store_ir->ptr()));

                            auto add_b = MInst(MInst::Kind::Add, {next_vreg(), 64});
                            add_b.location(store_ir->location());
                            add_b.add_operand(MOperandValueReference(function, f, store_ir->ptr()));
                            add_b.add_operand(MOperandImmediate(x86_64::GeneralPurposeBytewidth, 32));

                            auto store_b = MInst(MInst::Kind::Store, {virts[instruction], 0});
                            store_b.location(store_ir->location());
                            store_b.add_operand(reg_b);
                            store_b.add_operand(MOperandRegister(add_b.reg(), uint(add_b.regsize())));

                            bb.add_instruction(store_a);
                            bb.add_instruction(add_b);
                            bb.add_instruction(store_b);
                            break; // Value::Kind::Store
                        }

                        // Special case lowering of storing multiple register parameter.
                        else if (auto* param = cast<Parameter>(store_ir->val())) {
                            // TODO FOR FUN: Functions marked internal we can do all the fucky wucky
                            // to, to make more efficient-like.
                            // FIXME: What does f.calling_convention() (C, Glint) have to do
                            // with any of this?
                            if (_ctx->target()->is_arch_x86_64() and _ctx->target()->is_cconv_sysv()) {
                                auto param_description = cconv::sysv::parameter_description(function);
                                auto param_info = param_description.info.at(param->index());
                                auto arg_regs_used_before_parameter = param_info.arg_regs_used;

                                // Multiple register parameter
                                if (param_info.is_double_register()) {
                                    if (auto* alloca = cast<AllocaInst>(store_ir->ptr())) {
                                        // Multiple register parameter stored into alloca
                                        auto reg_a = MOperandRegister(
                                            +cconv::sysv::arg_regs.at(arg_regs_used_before_parameter++),
                                            x86_64::GeneralPurposeBitwidth
                                        );
                                        auto reg_b = MOperandRegister(
                                            +cconv::sysv::arg_regs.at(arg_regs_used_before_parameter++),
                                            uint(param->type()->bits() - x86_64::GeneralPurposeBitwidth)
                                        );

                                        auto store_a = MInst(
                                            MInst::Kind::Store,
                                            {virts[instruction], 0}
                                        );
                                        store_a.location(store_ir->location());
                                        store_a.add_operand(reg_a);
                                        store_a.add_operand(MOperandValueReference(function, f, alloca));

                                        auto add_b = MInst(
                                            MInst::Kind::Add,
                                            {next_vreg(), x86_64::GeneralPurposeBitwidth}
                                        );
                                        add_b.location(store_ir->location());
                                        add_b.add_operand(MOperandValueReference(function, f, alloca));
                                        add_b.add_operand(MOperandImmediate(x86_64::GeneralPurposeBytewidth, 32));

                                        auto store_b = MInst(
                                            MInst::Kind::Store,
                                            {virts[instruction], 0}
                                        );
                                        store_b.location(store_ir->location());
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
                        store.location(store_ir->location());
                        store.add_operand(MOperandValueReference(function, f, store_ir->val()));
                        store.add_operand(MOperandValueReference(function, f, store_ir->ptr()));
                        bb.add_instruction(store);
                    } break;

                    case Value::Kind::Load: {
                        auto* load_ir = as<LoadInst>(instruction);
                        auto load = MInst(
                            MInst::Kind::Load,
                            {virts[instruction],
                             uint(load_ir->type()->bits()),
                             register_category}
                        );
                        load.location(load_ir->location());
                        load.add_operand(MOperandValueReference(function, f, load_ir->ptr()));
                        bb.add_instruction(load);
                    } break;

                    case Value::Kind::Return: {
                        auto* ret_ir = as<ReturnInst>(instruction);
                        auto* func_type = as<FunctionType>(function->type());
                        auto ret_type_bytes = func_type->ret()->bytes();

                        // SysV return in two registers
                        if (
                            _ctx->target()->is_cconv_sysv()
                            and ret_ir->has_value()
                            and ret_type_bytes > x86_64::GeneralPurposeBytewidth
                            and ret_type_bytes <= 2 * x86_64::GeneralPurposeBytewidth
                        ) {
                            if (_ctx->target()->is_arch_x86_64()) {
                                // Add eight bytes to pointer to load from next.
                                // Copy pointer
                                auto copy_b = MInst(
                                    MInst::Kind::Copy,
                                    {next_vreg(), x86_64::GeneralPurposeBitwidth}
                                );
                                copy_b.location(ret_ir->location());
                                copy_b.add_operand(MOperandValueReference(function, f, ret_ir->val()));

                                auto add_b = MInst(
                                    MInst::Kind::Add,
                                    {next_vreg(), x86_64::GeneralPurposeBitwidth}
                                );
                                add_b.location(ret_ir->location());
                                add_b.add_operand(MOperandRegister(copy_b.reg(), uint(copy_b.regsize())));
                                add_b.add_operand(MOperandImmediate(x86_64::GeneralPurposeBytewidth, 32));

                                auto load_a = MInst(
                                    MInst::Kind::Load,
                                    {usz(x86_64::RegisterId::RAX), x86_64::GeneralPurposeBitwidth}
                                );
                                load_a.location(ret_ir->location());
                                load_a.add_operand(MOperandValueReference(function, f, ret_ir->val()));

                                auto load_b = MInst(
                                    MInst::Kind::Load,
                                    {
                                        usz(x86_64::RegisterId::RDX),
                                        uint(func_type->ret()->bits() - x86_64::GeneralPurposeBitwidth) //
                                    }
                                );
                                load_b.location(ret_ir->location());
                                load_b.add_operand(MOperandRegister(add_b.reg(), uint(add_b.regsize())));

                                bb.add_instruction(copy_b);
                                bb.add_instruction(add_b);
                                bb.add_instruction(load_a);
                                bb.add_instruction(load_b);
                                bb.add_instruction(MInst(MInst::Kind::Return, {0, 0}));
                                break;
                            } else LCC_ASSERT(false, "Unhandled target architecture in SysV multiple register return");
                        }

                        usz regsize = 0;
                        if (ret_ir->has_value()) regsize = ret_ir->val()->type()->bits();
                        auto ret = MInst(
                            MInst::Kind::Return,
                            {virts[instruction], uint(regsize), register_category}
                        );
                        ret.location(ret_ir->location());
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
                        auto* unary_ir = as<UnaryInstBase>(instruction);
                        auto unary = MInst(
                            ir_nary_inst_kind_to_mir(unary_ir->kind()),
                            {virts[instruction],
                             uint(unary_ir->type()->bits()),
                             register_category}
                        );
                        unary.location(unary_ir->location());
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
                        auto* binary_ir = as<BinaryInst>(instruction);
                        auto binary = MInst(
                            ir_nary_inst_kind_to_mir(binary_ir->kind()),
                            {virts[instruction],
                             uint(binary_ir->type()->bits()),
                             register_category}
                        );
                        binary.location(binary_ir->location());
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
                        if (not block) {
                            block = std::get<MOperandBlock>(op);
                            phi_operand_block = block->machine_block();
                            LCC_ASSERT(phi_operand_block, "Cannot phi2copy block that has no corresponding machine block");
                            LCC_ASSERT(phi_operand_block != &mblock, "Cannot phi2copy when one of the Phi operands is in the same block as the Phi");
                            continue;
                        }

                        if (std::holds_alternative<MOperandBlock>(op))
                            Diag::ICE("Phi value cannot be a block");

                        auto copy = MInst(MInst::Kind::Copy, {minst.reg(), uint(minst.regsize())});
                        copy.location(minst.location());

                        usz uses = minst.use_count();
                        while (uses and uses--) copy.add_use();

                        static_assert(
                            std::variant_size_v<MOperand> == 6,
                            "Exhaustive handling of MOperand alternatives in phi2copy"
                        );
                        if (std::holds_alternative<MOperandImmediate>(op)) {
                            copy.add_operand(std::get<MOperandImmediate>(op));
                        } else if (std::holds_alternative<MOperandRegister>(op)) {
                            auto reg_op = std::get<MOperandRegister>(op);
                            // SEARCH TERMS: zero register, zero size, zero size register, zero sized register, register zero size, register size
                            // Moves between mismatches register sizes are generally no good. And zero
                            // sized registers are also no good. Hope this works!
                            if (not reg_op.size) reg_op.size = uint(minst.regsize());
                            else reg_op.size = std::min(uint(minst.regsize()), reg_op.size);
                            copy.add_operand(reg_op);
                        } else if (std::holds_alternative<MOperandLocal>(op)) {
                            copy.add_operand(std::get<MOperandLocal>(op));
                        } else if (std::holds_alternative<MOperandGlobal>(op)) {
                            copy.add_operand(std::get<MOperandGlobal>(op));
                        } else if (std::holds_alternative<MOperandFunction>(op)) {
                            copy.add_operand(std::get<MOperandFunction>(op));
                        } else if (std::holds_alternative<MOperandBlock>(op)) {
                            copy.add_operand(std::get<MOperandBlock>(op));
                        } else LCC_ASSERT(false, "Unhandled MIR operand alternative");

                        phi_operand_block->add_instruction(copy, true);

                        block = nullptr;
                    }

                    LCC_ASSERT(not block, "Phi *must* have an even number of operands: incoming block and value pairs");

                    indices_of_instructions_to_remove.push_back(minst_index);
                }
            }
            for (isz index : vws::reverse(indices_of_instructions_to_remove))
                mblock.instructions().erase(mblock.instructions().begin() + index);
        }
    }

    return funcs;
}

} // namespace lcc
