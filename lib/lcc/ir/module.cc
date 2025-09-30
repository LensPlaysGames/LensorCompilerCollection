#include <fmt/format.h>
#include <lcc/codegen/isel.hh>
#include <lcc/codegen/mir.hh>
#include <lcc/codegen/register_allocation.hh>
#include <lcc/codegen/x86_64/assembly.hh>
#include <lcc/codegen/x86_64/object.hh>
#include <lcc/codegen/x86_64/x86_64.hh>
#include <lcc/context.hh>
#include <lcc/diags.hh>
#include <lcc/format.hh>
#include <lcc/ir/ir.hh>
#include <lcc/ir/module.hh>
#include <lcc/target.hh>
#include <lcc/utils.hh>
#include <lcc/utils/ir_printer.hh>
#include <object/generic.hh>

#include <algorithm>

// NOTE: See module_mir.cc for Machine Instruction Representation (MIR)
// generation.

namespace lcc {

void Module::lower() {
    if (_ctx->target()->is_arch_x86_64()) {
        for (auto function : code()) {
            FunctionType* function_type = as<FunctionType>(function->type());

            // Memory parameters need their type changed to pointer type.
            //
            // Also cache pointer to local for later use in storing from this
            // pointer into local that represents "actual parameter".
            //
            // foo : internal glintcc void(@__struct_0 %0):
            //   bb0:
            //     %1 = alloca @__struct_0
            //     store @__struct_0 %0 into %1
            //     %2 = gmp @__struct_0 from %1 at i64 0
            //     return
            //
            // SHOULD TURN INTO
            //
            // foo : internal glintcc void(ptr %0):
            //   bb0:
            //     %1 = alloca ptr
            //     store ptr %0 into %1
            //     %2 = load ptr from %1
            //     %3 = gmp @__struct_0 from %2 at i64 0
            //     return
            //
            // Basically, we need to change type of alloca and store->val() to ptr,
            // and add `load ptr from alloca` before every use, and replace the use
            // with that load...
            //
            // In C syntax:
            // int foo(big_struct _x) {
            //     big_struct x = _x;
            //     return x.a + x.b;
            // }
            //
            // SHOULD TURN INTO
            //
            // int foo(big_struct* _x) {
            //     big_struct* xptr = _x;
            //     return (*xptr).a + (*xptr).b;
            // }
            //
            for (size_t param_i{0}; param_i < function_type->params().size(); ++param_i) {
                auto*& param = function_type->params().at(param_i);
                // TODO: Calling convention here /may/ be affected by function calling
                // convention (maybe `function->call_conv()`).
                // TODO: This isn't a proper check of whether or not a parameter should go
                // in memory or not. We need to make sure there are registers left, even
                // if it would fit.
                bool sysv_memory_param = _ctx->target()->is_cconv_sysv() and param->bytes() > 16;
                bool x64_memory_param = _ctx->target()->is_cconv_ms() and param->bytes() >= 8;

                // TODO: x64 will also use this style lowering when actually passing stack
                // parameters, not memory parameters.
                if (sysv_memory_param) {
                    param = Type::PtrTy;
                    auto* parameter = function->param(param_i);
                    for (auto* user : parameter->users()) {
                        if (auto store = cast<StoreInst>(user)) {
                            LCC_ASSERT(
                                is<Parameter>(store->val()),
                                "Use of memory parameter in destination pointer of store instruction: we don't yet support this, sorry"
                            );
                            LCC_ASSERT(
                                is<AllocaInst>(store->ptr()),
                                "We only support storing memory parameter into pointer returned by AllocaInst, sorry"
                            );

                            // Replace uses of store->ptr() with (a copy of) store->val()
                            auto alloca = as<AllocaInst>(store->ptr());
                            for (auto pointer_user : alloca->users()) {
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
                            // Erase store
                            store->erase();
                            alloca->erase();

                        } else LCC_ASSERT(false, "Unhandled instruction type in replacement of users of memory parameter");
                    }
                }
                // TODO: This lowering should only ever apply to memory parameters that
                // have their pointer passed in a register.
                else if (x64_memory_param) {
                    param = Type::PtrTy;
                    auto* parameter = function->param(param_i);
                    for (auto* user : parameter->users()) {
                        if (auto store = cast<StoreInst>(user)) {
                            LCC_ASSERT(
                                is<Parameter>(store->val()),
                                "Use of memory parameter in destination pointer of store instruction: we don't yet support this, sorry"
                            );
                            LCC_ASSERT(
                                is<AllocaInst>(store->ptr()),
                                "We only support storing memory parameter into pointer returned by AllocaInst, sorry"
                            );

                            LCC_ASSERT(false, "TODO: MSx64 calling convention memory parameter (change alloca to ptr type, insert load before uses of alloca)");

                        } else LCC_ASSERT(false, "Unhandled instruction type in replacement of users of memory parameter");
                    }
                }
            }

            // Add parameter for over-large return types (in-memory ones that alter
            // function signature).
            // SysV is able to return objects <= 16 bytes in two registers.
            bool ret_t_tworeg = _ctx->target()->is_cconv_sysv() and function_type->ret()->bytes() > 8 and function_type->ret()->bytes() <= 16;
            bool ret_t_large = function_type->ret()->bytes() > 8;
            Value* ret_v_large{nullptr};
            if (not ret_t_tworeg and ret_t_large) {
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

            for (auto* block : function->blocks()) {
                for (size_t inst_i = 0; inst_i < block->instructions().size(); ++inst_i) {
                    auto*& instruction = block->instructions().at(inst_i);
                    switch (instruction->kind()) {
                        case Value::Kind::Return: {
                            auto* ret = as<ReturnInst>(instruction);

                            // For large return types, we memcpy the returned value into the pointer
                            // passed as the hidden first arugument.
                            if (not ret_t_tworeg and ret_t_large) {
                                auto* dest_ptr = ret_v_large;
                                auto* source_ptr = ret->val();
                                if (not source_ptr->type()->is_ptr())
                                    Diag::ICE("IR ReturnInst returns large value but operand is not of pointer type");

                                auto byte_count = function_type->ret()->bytes();
                                std::vector<Value*> memcpy_operands{
                                    dest_ptr,
                                    source_ptr,
                                    new (*this) IntegerConstant(
                                        IntegerType::Get(context(), x86_64::GeneralPurposeBitwidth),
                                        byte_count
                                    ) //
                                };
                                auto* memcpy_inst = new (*this) IntrinsicInst(
                                    IntrinsicKind::MemCopy,
                                    memcpy_operands,
                                    ret->location()
                                );
                                ret->replace_with(memcpy_inst);
                                block->insert_after(new (*this) ReturnInst(nullptr), memcpy_inst);
                            }
                        } break;

                        case Value::Kind::Load: {
                            auto* load = as<LoadInst>(instruction);

                            // Less than or equal to size of general purpose register; no change.
                            if (load->type()->bits() <= x86_64::GeneralPurposeBitwidth) continue;

                            // If this is an over-large load but it is used by a call, assume the
                            // calling convention allows for it and it will be handled in MIR.
                            // NOTE: Taken advantage of by SysV (see both parameter handling above as
                            // well as argument handling in MIR generation).
                            if (
                                not load->users().empty()
                                and is<CallInst>(load->users().at(0))
                            ) continue;

                            auto users = load->users();
                            if (users.size() == 1 and users[0]->kind() == Value::Kind::Store) {
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
                            }
                        } break;

                        case Value::Kind::Store: {
                            auto store = as<StoreInst>(instruction);

                            // Less than or equal to size of general purpose register; no change.
                            if (store->val()->type()->bits() <= x86_64::GeneralPurposeBitwidth)
                                continue;
                            auto byte_count = store->val()->type()->bytes();

                            // Return in multiple registers (handled in MIR generation)
                            if (
                                context()->target()->is_cconv_sysv()
                                and store->val()->type()->bits() <= 2 * x86_64::GeneralPurposeBitwidth
                            ) continue;

                            // Change type of val to pointer, if it isn't already.
                            // TODO: Does this apply to values other than Parameter? Does it need to?
                            if (auto param = cast<Parameter>(store->val())) {
                                store->val(
                                    new (*this) Parameter(Type::PtrTy, param->index())
                                );
                            }

                            if (store->val()->type() != Type::PtrTy) {
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
    bool to_stdout = output_file_path.empty() or output_file_path == "-";
    switch (_ctx->format()->format()) {
        case Format::INVALID: LCC_UNREACHABLE();

        case Format::LCC_IR: {
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
                if (_ctx->target()->is_cconv_ms()) {
                    desc.return_register = +x86_64::RegisterId::RAX;
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
                    desc.return_register = +x86_64::RegisterId::RAX;
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
            } else LCC_ASSERT(false, "Sorry, unhandled target architecture");

            for (auto& mfunc : machine_ir)
                allocate_registers(desc, mfunc);

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
