#include <fmt/format.h>
#include <fmt/ranges.h>

#include <lcc/calling_conventions/ms_x64.hh>
#include <lcc/calling_conventions/sysv_x86_64.hh>
#include <lcc/codegen/isel.hh>
#include <lcc/codegen/mir.hh>
#include <lcc/codegen/register_allocation.hh>
#include <lcc/codegen/x86_64/assembly.hh>
#include <lcc/codegen/x86_64/object.hh>
#include <lcc/codegen/x86_64/x86_64.hh>
#include <lcc/context.hh>
#include <lcc/diags.hh>
#include <lcc/format.hh>
#include <lcc/ir/core.hh>
#include <lcc/ir/module.hh>
#include <lcc/ir/type.hh>
#include <lcc/target.hh>
#include <lcc/utils.hh>
#include <lcc/utils/fractionals.hh>
#include <lcc/utils/ir_printer.hh>

#include <object/generic.hh>

#include <algorithm>

// NOTE: See module_mir.cc for Machine Instruction Representation (MIR)
// generation.

namespace lcc {

void Module::_x86_64_lower_store(StoreInst* store, Function* function) {
    LCC_ASSERT(is<StoreInst>(store));

    // Less than or equal to size of general purpose register; no change.
    if (store->val()->type()->bits() <= x86_64::GeneralPurposeBitwidth)
        return;

    auto byte_count = store->val()->type()->bytes();

    // Return in multiple registers (handled in MIR generation)
    if (
        context()->target()->is_cconv_sysv()
        and store->val()->type()->bits() <= 2 * x86_64::GeneralPurposeBitwidth
    ) return;

    // Change type of val to pointer, if it isn't already.
    // TODO: Does this apply to values other than Parameter? Does it need to?
    // store->val()->type_reference() = Type::PtrTy;
    if (auto param = cast<Parameter>(store->val())) {
        store->val(
            new (*this) Parameter(Type::PtrTy, param->index())
        );
    }
    if (store->val()->type() != Type::PtrTy) {
        function->print();
        store->print();
        LCC_ASSERT(
            store->val()->type() == Type::PtrTy,
            "Cannot lower store to memcpy when value is not of pointer type (it's {})",
            store->val()->type()->string(context()->option_use_colour())
        );
    }

    // Generate builtin memcpy
    std::vector<Value*> memcpy_operands{
        store->ptr(),
        // The value should have already been lowered to a pointer.
        store->val(),
        new (*this) IntegerConstant(
            IntegerType::Get(context(), x86_64::GeneralPurposeBitwidth),
            byte_count
        )
    };
    auto* memcpy_inst = new (*this) IntrinsicInst(
        IntrinsicKind::MemCopy,
        memcpy_operands,
        store->location()
    );

    store->replace_with(memcpy_inst);
}

void Module::_x86_64_lower_load(LoadInst* load, Function* function) {
    // Less than or equal to size of general purpose register; no change.
    if (load->type()->bits() <= x86_64::GeneralPurposeBitwidth) return;

    // If this is an over-large load but it is used by a call, assume the
    // calling convention allows for it and it will be handled in MIR.
    // NOTE: Taken advantage of by SysV (see both parameter handling above as
    // well as argument handling in MIR generation).
    if (
        not load->users().empty()
        and is<CallInst>(load->users().at(0))
    ) return;

    auto& users = load->users();
    if (users.size() == 1 and is<StoreInst>(users.at(0))) {
        auto* store = as<StoreInst>(users[0]);

        auto* source_ptr = load->ptr();
        auto* dest_ptr = store->ptr();

        LCC_ASSERT(load->type()->bytes() == store->val()->type()->bytes());
        auto byte_count = load->type()->bytes();

        std::vector<Value*> memcpy_operands{
            dest_ptr,
            source_ptr,
            new (*this) IntegerConstant(
                IntegerType::Get(
                    context(),
                    x86_64::GeneralPurposeBitwidth
                ),
                byte_count
            )
        };
        auto memcpy_inst = new (*this) IntrinsicInst(
            IntrinsicKind::MemCopy,
            memcpy_operands,
            load->location()
        );

        store->replace_with(memcpy_inst);
        load->erase();
    } else {
        // Possiblities:
        // - generate builtin memcpy for backend to handle
        // - unroll into 8 byte loads, temporary pointer stored into then
        //   incremented
        // - just copy the ptr instead, and everywhere that uses a load should
        //   handle the fact that over-sized loads will be pointers instead.

        auto* copy = new (*this) CopyInst(load->ptr());
        load->replace_with(copy);

        // Copy to prevent iterator invalidation
        // auto load_users = load->users();
        // for (auto u : load_users) {
        //     u->replace_children<LoadInst>([&](Value* v) -> Value* {
        //         if (v == load) return load->ptr();
        //         return nullptr;
        //     });
        // }

        // load->erase();
    }
}

void Module::_x86_64_sysv_lower_overlarge() {
    // SysV x86_64 Calling Convention Overlarge Type Lowering
    // Basically, things larger than 8 bytes fit in LCC IR virtual registers,
    // but they sure as shit don't fit in x86_64 general purpose registers.
    // This function does a couple things:
    // - Overlarge Return Type -> insert ptr parameter, then convert existing
    //                            returns to memcpy to inserted parameter
    // - Overlarge LoadInst and StoreInst -> converted to memcpy
    LCC_ASSERT(_ctx->target()->is_cconv_sysv());

    for (auto function : code()) {
        FunctionType* function_type = as<FunctionType>(function->type());

        // Alter Function Signature, if need be
        // Add parameter for over-large return types (in-memory ones that alter
        // function signature).
        // SysV x86_64 returns objects larger than eight bytes and less than or
        // equal to sixteen bytes in two registers.
        bool ret_t_is_tworeg
            = function_type->ret()->bytes() > x86_64::GeneralPurposeBytewidth
          and function_type->ret()->bytes() <= 2 * x86_64::GeneralPurposeBytewidth;
        bool ret_t_is_large
            = (not ret_t_is_tworeg)
          and function_type->ret()->bytes() > x86_64::GeneralPurposeBytewidth;
        Value* ret_v_large{nullptr};
        if (ret_t_is_large) {
            // Update function return type to be a pointer.
            function_type->ret_reference() = Type::PtrTy;

            // Prepend return value pointer parameter to both function value and
            // function type.
            function->params().insert(
                function->params().begin(),
                new (*this) Parameter{Type::PtrTy, 0}
            );
            function_type->params().insert(
                function_type->params().begin(),
                Type::PtrTy
            );

            // Update the indices of the rest of the displaced parameters, if any.
            for (usz i = 1; i < function->params().size(); ++i)
                function->params()[i]->index() = u32(i);

            if (
                function->blocks().size()
                and function->blocks().at(0)->instructions().size()
            ) {
                auto start = function->blocks().at(0);
                auto alloca = new (*this) AllocaInst(Type::PtrTy, {});
                auto store = new (*this) StoreInst(function->params().at(0), alloca);
                start->insert_before(alloca, start->instructions().at(0));
                start->insert_after(store, alloca);
                ret_v_large = alloca;
            }

            // Now we should go through and lower all the returns in the function to
            // instead be a memcpy into this pointer.
            // NOTE: To do that, we must also make sure the returns return a pointer,
            // and, to do that, we lower all over-large loads/stores into memcpys, if
            // possible. So, we do that at the same time as those other things in the
            // following outer loop.
        }

        // Convert overlarge ReturnInst, LoadInst, and StoreInst.
        for (auto* block : function->blocks()) {
            for (size_t inst_i = 0; inst_i < block->instructions().size(); ++inst_i) {
                auto*& instruction = block->instructions().at(inst_i);
                switch (instruction->kind()) {
                    default: break;

                    // For large return types, we memcpy the returned value into the pointer
                    // passed as the automatically inserted first arugument.
                    case Value::Kind::Return: {
                        if (not ret_t_is_large) continue;

                        auto* ret = as<ReturnInst>(instruction);
                        // NOTE: ret_v_large assigned above.
                        auto* dest_ptr = ret_v_large;
                        // Copy from whatever the return is returning.
                        auto* source_ptr = ret->val();

                        // Assert that whatever the return is returning is of pointer type.
                        // Ideally, this has been handled by previous store/load lowering.
                        LCC_ASSERT(
                            source_ptr
                                and source_ptr->type()
                                and source_ptr->type()->is_ptr(),
                            "IR ReturnInst returns large value but operand is not of pointer type"
                        );

                        auto byte_count = function_type->ret()->bytes();
                        std::vector<Value*> memcpy_operands{
                            dest_ptr,
                            source_ptr,
                            new (*this) IntegerConstant(
                                IntegerType::Get(
                                    context(),
                                    x86_64::GeneralPurposeBitwidth
                                ),
                                byte_count
                            )
                        };
                        auto* memcpy_inst = new (*this) IntrinsicInst(
                            IntrinsicKind::MemCopy,
                            memcpy_operands,
                            ret->location()
                        );
                        ret->replace_with(memcpy_inst);
                        block->insert_after(
                            new (*this) ReturnInst(dest_ptr),
                            memcpy_inst
                        );
                        // IMPORTANT! Skip instruction inserted above in iteration (lest we get
                        // stuck lowering the same return forever and ever).
                        ++inst_i;
                    } break;

                    case Value::Kind::Load: {
                        auto* load = as<LoadInst>(instruction);
                        _x86_64_lower_load(load, function);
                    } break;

                    case Value::Kind::Store: {
                        auto store = as<StoreInst>(instruction);
                        _x86_64_lower_store(store, function);
                    } break;

                    case Value::Kind::Call: {
                        auto* call = as<CallInst>(instruction);

                        auto callee_t_is_tworeg
                            = call->function_type()->ret()->bytes() > x86_64::GeneralPurposeBytewidth
                          and call->function_type()->ret()->bytes() <= 2 * x86_64::GeneralPurposeBytewidth;

                        auto callee_t_is_large
                            = (not callee_t_is_tworeg)
                          and call->function_type()->ret()->bytes() > x86_64::GeneralPurposeBytewidth;

                        // For large return types, the function actually returns a pointer to the
                        // type in memory.
                        if (callee_t_is_large) call->type_reference() = Type::PtrTy;
                    } break;
                }
            }
        }
    }
}

void Module::_x86_64_sysv_lower_parameters() {
    // SysV x86_64 Calling Convention Parameter Lowering

    /// SysV Memory Parameter Lowering, Stage 1
    /// We expect the form of
    /// foo (internal): glintcc void(i64 %0):
    ///   bb0:
    ///     %1 = alloca i64
    ///     store i64 %0 into %1
    ///     ; ... uses of alloca
    ///
    /// As you can see, the incoming IR is basically assuming the parameter is
    /// an rvalue, and it is making a local and loading/storing from that to A.
    /// get an lvalue and B. prevent overwriting of the original parameter,
    /// wherever it may be stored according to whatever calling convention
    /// (because the LCC Register Allocator is authored by me, and I'm not a
    /// compiler scientist (or am I just a bad one?)).
    /// The problem here, as you can see, is that, memory parameters are, well,
    /// already in /memory/ (hence nomenclature). this unnecessary copy would
    /// turn into a memcpy and we don't really need to do that at all, and it
    /// just generates extra code output for kind of no reason.
    ///
    /// So, the idea is, remove the alloca, remove the store, and replace uses
    /// of the alloca (a pointer to the parameter) with the parameter itself
    /// (which, for memory parameters, is a pointer to the parameter).
    for (auto function : code()) {
        FunctionType* function_type = as<FunctionType>(function->type());
        auto param_desc = cconv::sysv::parameter_description(function);
        LCC_ASSERT(
            function_type->params().size() == param_desc.info.size(),
            "SysV Parameter Description: parameter count doesn't match signature"
        );
        for (
            auto [parameter, param_info, param_t] :
            vws::zip(function->params(), param_desc.info, function_type->params())
        ) {
            if (not param_info.is_memory()) continue;

            // Update parameter type in function type signature
            param_t = Type::PtrTy;

            LCC_ASSERT(
                parameter->users().size() == 1 and is<StoreInst>(parameter->users().at(0)),
                "Expected memory parameter to be used by single store instruction into a preceding alloca..."
            );
            auto store = as<StoreInst>(parameter->users().at(0));

            // ASSERT form of: store <param> into <alloca>
            LCC_ASSERT(
                is<Parameter>(store->val()),
                "SysV IR Parameter Lowering only supports storing FROM memory parameters, not into them."
            );
            LCC_ASSERT(
                is<AllocaInst>(store->ptr()),
                "SysV IR Parameter Lowering only supports storing memory parameters into AllocaInst, sorry. Expect form of store <param> into <alloca>."
            );

            // Replace uses of store->ptr() with (a copy of) store->val()
            auto alloca = as<AllocaInst>(store->ptr());
            // NOTE: Prevent iterator invalidation with intentional COPY.
            auto alloca_users = alloca->users();
            for (auto pointer_user : alloca_users) {
                // Alloca fetched with store->val() is (obviously) used by the store, but
                // we don't want to replace the alloca in the store because we are going
                // to be removing the store anyway..
                if (pointer_user == store) continue;

                // Replace use of Alloca.
                auto copy = new (*this) Parameter(parameter->type(), parameter->index());
                pointer_user->replace_children([&](Value* v) -> Value* {
                    if (v == alloca) return copy;
                    return nullptr;
                });
            }

            // Erase alloca and store, as they are now unused and redundant.
            store->erase();

            // Had some problems with iterator invalidation so now this is here just
            // as a confidence check.
            for (const auto* u : alloca->users()) {
                fmt::print("INVALID USE: ");
                u->print();
                fmt::print("\n");
                if (u->block()) {
                    fmt::print("Within block:\n");
                    u->block()->print();
                    fmt::print("\n");
                }
            }
            LCC_ASSERT(
                alloca->users().empty(),
                "All uses of alloca should have been removed..."
            );

            alloca->erase();
        }
    }
}

void Module::_x86_64_msx64_lower_overlarge() {
    for (auto function : code()) {
        FunctionType* function_type = as<FunctionType>(function->type());

        bool ret_t_is_large
            = function_type->ret()->bytes() > x86_64::GeneralPurposeBytewidth;

        Value* ret_v_large{nullptr};
        if (ret_t_is_large) {
            function_type->ret_reference() = Type::PtrTy;

            // Prepend parameter to both function value and function type.
            function_type->params().insert(function_type->params().begin(), Type::PtrTy);
            function->params().insert(function->params().begin(), new (*this) Parameter{Type::PtrTy, 0});
            // Update the indices of the rest of the displaced parameters, if any.
            for (usz i = 1; i < function->params().size(); ++i)
                function->params()[i]->index() = u32(i);

            if (function->blocks().size() and function->blocks().at(0)->instructions().size()) {
                auto start = function->blocks().at(0);
                auto alloca = new (*this) AllocaInst(Type::PtrTy, {});
                auto store = new (*this) StoreInst(function->params().at(0), alloca);
                start->insert_before(alloca, start->instructions().at(0));
                start->insert_after(store, alloca);
                ret_v_large = alloca;
            }

            // Now we should go through and lower all the returns in the function to
            // instead be a memcpy into this pointer.
        }

        // Convert overlarge ReturnInst, LoadInst, and StoreInst.
        for (auto* block : function->blocks()) {
            for (size_t inst_i = 0; inst_i < block->instructions().size(); ++inst_i) {
                auto*& instruction = block->instructions().at(inst_i);
                switch (instruction->kind()) {
                    default: break;

                    case Value::Kind::Return: {
                        auto* ret = as<ReturnInst>(instruction);

                        if (not ret_t_is_large) continue;
                        // For large return types, we memcpy the returned value into the pointer
                        // passed as the automatically inserted first arugument.

                        // NOTE: ret_v_large assigned above.
                        auto* dest_ptr = ret_v_large;
                        // Copy from whatever the return is returning.
                        auto* source_ptr = ret->val();
                        // Assert that whatever the return is returning is of pointer type.
                        // Ideally, this has been handled by previous store/load lowering.
                        LCC_ASSERT(
                            source_ptr
                                and source_ptr->type()
                                and source_ptr->type()->is_ptr(),
                            "IR ReturnInst returns large value but operand is not of pointer type"
                        );

                        auto byte_count = function_type->ret()->bytes();
                        std::vector<Value*> memcpy_operands{
                            dest_ptr,
                            source_ptr,
                            new (*this) IntegerConstant(
                                IntegerType::Get(
                                    context(),
                                    x86_64::GeneralPurposeBitwidth
                                ),
                                byte_count
                            )
                        };
                        auto* memcpy_inst = new (*this) IntrinsicInst(
                            IntrinsicKind::MemCopy,
                            memcpy_operands,
                            ret->location()
                        );
                        ret->replace_with(memcpy_inst);
                        block->insert_after(
                            new (*this) ReturnInst(dest_ptr),
                            memcpy_inst
                        );
                        // IMPORTANT! Skip instruction inserted above in iteration (lest we get
                        // stuck lowering the same return forever and ever).
                        ++inst_i;
                    } break;

                    case Value::Kind::Load: {
                        auto* load = as<LoadInst>(instruction);
                        _x86_64_lower_load(load, function);
                    } break;

                    case Value::Kind::Store: {
                        auto store = as<StoreInst>(instruction);
                        _x86_64_lower_store(store, function);
                    } break;

                    case Value::Kind::Call: {
                        auto* call = as<CallInst>(instruction);

                        auto callee_t_is_large
                            = call->function_type()->ret()->bytes() > x86_64::GeneralPurposeBytewidth;

                        // For large return types, the function actually returns a pointer to the
                        // type in memory.
                        if (callee_t_is_large) call->type_reference() = Type::PtrTy;
                    } break;
                }
            }
        }
    }
    return;
}

void Module::_x86_64_msx64_lower_parameters() {
    for (auto function : code()) {
        FunctionType* function_type = as<FunctionType>(function->type());
        auto param_desc = cconv::msx64::parameter_description(function);
        LCC_ASSERT(
            function_type->params().size() == param_desc.info.size(),
            "SysV Parameter Description: parameter count doesn't match signature"
        );

        for (
            auto [parameter, param_info, param_t] :
            vws::zip(function->params(), param_desc.info, function_type->params())
        ) {
            // There is always a point in memory where we can find a parameter in
            // msx64 calling convention, so we can remove allocas and stores and
            // things like that for /all/ parameters.

            // x64 calling convention requires the caller to allocate space on the
            // stack for the callee to save parameter registers to, if need be.
            // Because that creates two sources of truth (is the parameter in the
            // already allocated memory or is it in the register?), we just always
            // move the parameter into the memory at the beginning of the function. It
            // appears GCC does something similar to this, as well. This means
            // register parameters are treated the same as memory parameters in this
            // scenario (alloca and store removed, future references to alloca
            // replaced with reference to (a copy of) parameter).

            for (auto* user : parameter->users()) {
                auto store = cast<StoreInst>(user);
                LCC_ASSERT(
                    store,
                    "Unhandled instruction type in replacement of users of memory parameter"
                );

                LCC_ASSERT(
                    is<Parameter>(store->val()),
                    "Use of parameter in destination pointer of store instruction: we don't yet support this, sorry"
                );
                LCC_ASSERT(
                    is<AllocaInst>(store->ptr()),
                    "We only support storing parameter into pointer returned by AllocaInst, sorry"
                );

                // Replace uses of store->ptr() with (a copy of) store->val()
                const auto alloca = as<AllocaInst>(store->ptr());
                // NOTE: We can't alter users while we are looping over it...
                // So, we make a COPY here. That way, we can alter alloca->users() all we
                // want while looping over alloca_users.
                auto alloca_users = alloca->users();
                for (auto pointer_user : alloca_users) {
                    // Alloca fetched with store->val() is (obviously) used by the store, but
                    // we don't want to replace the alloca in the store because we are going
                    // to be removing the store anyway..
                    if (pointer_user == store) continue;

                    // Replace use of Alloca.
                    // TODO: Does PointerInRegister need LoadInst instead of just copying the
                    // parameter directly? Because technically the parameter is a pointer to a
                    // pointer to the parameter (pptr), and we may be expecting just a ptr to
                    // the parameter from the users of the alloca.
                    Value* replacement{nullptr};
                    if (param_info.kind() == cconv::msx64::ParameterDescription::Parameter::Kinds::PointerInRegister) {
                        // For pointer-in-register parameters, replace the alloca with a *load* of
                        // a copy of the parameter. The alloca was a ptr to the parameter, and the
                        // parameter is a /ptr to/ a ptr to the parameter. This is because it
                        // /starts/ as a ptr to the parameter /in a register/. That register's
                        // value gets saved to the shadow stack space where that register's value
                        // is meant to be saved, and the parameter gets lowered into an inlined
                        // local operand referencing that space on the stack within the parent
                        // stack frame. So, it becomes a ptr to a ptr to the parameter.
                        auto load = new (*this) LoadInst(
                            Type::PtrTy,
                            new (*this) Parameter(
                                Type::PtrTy,
                                parameter->index()
                            )
                        );
                        pointer_user->insert_before(load);
                        replacement = load;
                    } else {
                        // For register parameters and memory parameters, just replace the alloca
                        // with a copy of the parameter. The alloca was a ptr to the parameter,
                        // and so is the parameter itself in this calling convention.
                        replacement = new (*this) Parameter(
                            parameter->type(),
                            parameter->index()
                        );
                    }
                    LCC_ASSERT(replacement);

                    pointer_user->replace_children<AllocaInst>(
                        [&](Value* v) -> Value* {
                            if (v == alloca) return replacement;
                            return nullptr;
                        }
                    );
                }
                // Erase store
                store->erase();

                for (const auto* u : alloca->users()) {
                    fmt::print("INVALID USE: ");
                    u->print();
                    fmt::print("\n");
                    if (u->block()) {
                        fmt::print("Within block:\n");
                        u->block()->print();
                        fmt::print("\n");
                    }
                }
                LCC_ASSERT(
                    alloca->users().empty(),
                    "All uses of alloca should have been removed..."
                );

                alloca->erase();
            }
        }
    }
}

void Module::_x86_64_lower_float_constants() {
    for (auto function : code()) {
        for (auto block : function->blocks()) {
            for (size_t inst_i = 0; inst_i < block->instructions().size(); ++inst_i) {
                auto*& instruction = block->instructions().at(inst_i);
                instruction->replace_children([&](Value* c) -> Value* {
                    switch (c->kind()) {
                        default: return nullptr;
                        case Value::Kind::FractionalConstant: {
                            auto f = as<FractionalConstant>(c)->value();

                            auto binary32_value = fixed_to_binary32_float(f);
                            constexpr usz bitwidth = 32;

                            auto float_global_name = fmt::format("fconst{:x}", binary32_value);
                            auto float_global_type = IntegerType::Get(context(), bitwidth);
                            auto float_init = new (*this) IntegerConstant(float_global_type, binary32_value);
                            auto float_global = new (*this) GlobalVariable(
                                this,
                                float_global_type,
                                float_global_name,
                                Linkage::Internal,
                                float_init
                            );

                            auto load = new (*this) LoadInst(
                                FractionalType::Get(context(), bitwidth),
                                float_global,
                                instruction->location()
                            );
                            instruction->insert_before(load);

                            return load;
                        }
                    }
                });
            }
        }
    }
}

void Module::lower() {
    // Lowering not needed for LCC SSA IR or LLVM textual IR...
    if (
        context()->format() == Format::lcc_ssa_ir
        or context()->format() == Format::llvm_textual_ir
        or context()->format() == Format::wasm_textual
    ) return;

    // TODO: Static assert for handling all architectures, calling
    // conventions, etc.
    if (context()->target()->is_arch_x86_64()) {
        _x86_64_lower_float_constants();
        if (context()->target()->is_cconv_sysv()) {
            _x86_64_sysv_lower_parameters();
            _x86_64_sysv_lower_overlarge();
        } else if (context()->target()->is_cconv_ms()) {
            _x86_64_msx64_lower_parameters();
            _x86_64_msx64_lower_overlarge();
        } else LCC_ASSERT(false, "Unhandled calling convention in x86_64 IR lowering");
    } else {
        LCC_ASSERT(false, "TODO: Lowering of specified arch is not yet supported");
    }
}

void Module::emit(std::filesystem::path output_file_path) {
    bool to_stdout = output_file_path.empty() or output_file_path == "-";
    switch (context()->format()->format()) {
        case Format::INVALID: LCC_UNREACHABLE();

        case Format::LCC_IR:
        case Format::LCC_SSA_IR: {
            if (to_stdout)
                fmt::print("{}", as_lcc_ir(context()->option_use_colour()));
            else {
                auto lcc_ir = as_lcc_ir(false);
                File::WriteOrTerminate(lcc_ir.data(), lcc_ir.size(), output_file_path);
            }
        } break;

        case Format::LLVM_TEXTUAL_IR: {
            auto llvm_ir = as_llvm_ir();
            if (to_stdout) fmt::print("{}", llvm_ir);
            else File::WriteOrTerminate(llvm_ir.data(), llvm_ir.size(), output_file_path);
        } break;

        case Format::WASM_TEXTUAL: {
            auto wasm_text = as_wat();
            if (to_stdout) fmt::print("{}", wasm_text);
            else File::WriteOrTerminate(wasm_text.data(), wasm_text.size(), output_file_path);
        } break;

        case Format::COFF_OBJECT:
        case Format::ELF_OBJECT:
        case Format::GNU_AS_ATT_ASSEMBLY: {
            auto machine_ir = mir();

            if (_ctx->option_print_mir())
                fmt::print("{}", PrintMIR(vars(), machine_ir));

            for (auto& mfunc : machine_ir)
                select_instructions(this, mfunc);

            if (_ctx->option_print_mir()) {
                fmt::print("\nAfter ISel\n");
                if (_ctx->target()->is_arch_x86_64()) {
                    for (auto& f : machine_ir)
                        fmt::print("{}", PrintMFunctionImpl(f, x86_64::opcode_to_string));
                } else {
                    fmt::print(
                        "{}",
                        fmt::join(vws::transform(machine_ir, PrintMFunction), "\n")
                    );
                }
            }

            // Register Allocation
            MachineDescription desc{};
            if (_ctx->target()->is_arch_x86_64()) {
                desc.return_register_to_replace = +x86_64::RegisterId::RETURN;
                std::vector<usz> jeep_registers{};
                if (_ctx->target()->is_cconv_ms()) {
                    desc.return_register = +cconv::msx64::return_register;
                    // Just the volatile registers
                    rgs::transform(
                        cconv::msx64::volatile_regs,
                        std::back_inserter(jeep_registers),
                        [](auto r) { return +r; }
                    );
                } else {
                    desc.return_register = +cconv::sysv::return_register;
                    // Just the volatile registers
                    rgs::transform(
                        cconv::sysv::volatile_regs,
                        std::back_inserter(jeep_registers),
                        [](auto r) { return +r; }
                    );
                }
                LCC_ASSERT(
                    jeep_registers.size(),
                    "Must populate general purpose register list"
                );
                desc.registers.emplace_back(
                    +Register::Category::DEFAULT,
                    jeep_registers
                );

                std::vector<usz> scalar_registers{
                    +x86_64::RegisterId::XMM0,
                    +x86_64::RegisterId::XMM1,
                    +x86_64::RegisterId::XMM2,
                    +x86_64::RegisterId::XMM3,
                    +x86_64::RegisterId::XMM4,
                    +x86_64::RegisterId::XMM5,
                    +x86_64::RegisterId::XMM6,
                    +x86_64::RegisterId::XMM7,
                    +x86_64::RegisterId::XMM8,
                    +x86_64::RegisterId::XMM9,
                    +x86_64::RegisterId::XMM10,
                    +x86_64::RegisterId::XMM11,
                    +x86_64::RegisterId::XMM12,
                    +x86_64::RegisterId::XMM13,
                    +x86_64::RegisterId::XMM14,
                    +x86_64::RegisterId::XMM15
                };
                desc.registers.emplace_back(
                    +Register::Category::FLOAT,
                    scalar_registers
                );

            } else LCC_ASSERT(false, "Sorry, unhandled target architecture");

            for (auto& mfunc : machine_ir) {
                (void) allocate_registers(desc, mfunc);
            }

            if (_ctx->option_print_mir()) {
                fmt::print("\nAfter RA\n");
                if (_ctx->target()->is_arch_x86_64()) {
                    for (auto& f : machine_ir)
                        fmt::print("{}", PrintMFunctionImpl(f, x86_64::opcode_to_string));
                } else {
                    fmt::print(
                        "{}",
                        fmt::join(vws::transform(machine_ir, PrintMFunction), "\n")
                    );
                }
            }

            if (_ctx->option_stopat_mir()) std::exit(0);

            if (_ctx->format()->format() == Format::GNU_AS_ATT_ASSEMBLY) {
                if (_ctx->target()->is_arch_x86_64())
                    x86_64::emit_gnu_att_assembly(output_file_path, this, desc, machine_ir);
                else LCC_ASSERT(false, "Unhandled code emission target, sorry");
            } else if (_ctx->format()->format() == Format::ELF_OBJECT) {
                GenericObject gobj{};
                if (_ctx->target()->is_arch_x86_64())
                    gobj = x86_64::emit_mcode_gobj(this, desc, machine_ir);
                else LCC_ASSERT(false, "Unhandled code emission target, sorry");

                fmt::print("{}\n", gobj.print());

                FILE* f = fopen(output_file_path.string().data(), "wb");
                if (not f) Diag::ICE("Could not open output file at {} for writing", output_file_path.string());
                gobj.as_elf(f);
                fclose(f);

            } else if (_ctx->format()->format() == Format::COFF_OBJECT) {
                LCC_TODO("Emit COFF object from generic object format");
            }
        } break;
    }
}

} // namespace lcc
