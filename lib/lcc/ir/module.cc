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
            // TODO: We still don't handle sysv memory parameters "properly
            // properly" (although we are on our way), since the alloca for the
            // original parameter is at the call site AND the pointer to it ISN'T
            // passed in a register like normal. This means that the fixed up "store
            // ptr %0 into %1" can't just treat the parameter like a register but
            // instead has to treat it like a very custom local that is in the parent
            // stack frame (with a positive offset from the base pointer). In MIR,
            // this could just be `mov local(0)+24`, or whatever. This would become
            // two instructions, one to load either the source or destination pointer
            // and one to store the value. I believe we could handle this fixup later,
            // during MIR generation.
            //
            for (size_t param_i{0}; param_i < function_type->params().size(); ++param_i) {
                auto*& param = function_type->params().at(param_i);
                // TODO: Calling convention here /may/ be affected by function calling
                // convention (maybe `function->call_conv()`).
                bool sysv_memory_param = _ctx->target()->is_cconv_sysv() and param->bytes() > 16;
                bool x64_memory_param = _ctx->target()->is_cconv_ms() and param->bytes() >= 8;
                if (sysv_memory_param or x64_memory_param) {
                    param = Type::PtrTy;
                    auto* parameter = function->param(param_i);
                    for (auto* user : parameter->users()) {
                        if (auto store = cast<StoreInst>(user)) {
                            LCC_ASSERT(
                                is<Parameter>(store->val()),
                                "Use of memory parameter in destination pointer of store instruction: we don't yet support this, sorry"
                            );

                            store->val(new (*this) Parameter(Type::PtrTy, parameter->index()));

                            LCC_ASSERT(
                                is<AllocaInst>(store->ptr()),
                                "We only support storing memory parameter into pointer returned by AllocaInst, sorry"
                            );

                            // TODO: We could "just" alter the type of the alloca instruction, rather
                            // than replacing the whole instruction. Right now, this is easier.
                            auto alloca = as<AllocaInst>(store->ptr());
                            auto new_alloca = new (*this) AllocaInst(Type::PtrTy, alloca->location());

                            // Store parameter pointer into local.
                            store->ptr(new_alloca);

                            // Every user of the old alloca is expecting a pointer to the parameter,
                            // but the new alloca now returns a pointer to the pointer to the
                            // parameter; this adds the level of indirection needed.
                            for (auto pointer_user : alloca->users()) {
                                auto load = new (*this) LoadInst(Type::PtrTy, new_alloca);

                                // Insert load just before use instruction.
                                LCC_ASSERT(pointer_user->block(), "IR instruction has no block reference");
                                pointer_user->block()->insert_before(load, pointer_user);

                                // Replace use of alloca with use of newly-inserted load.
                                pointer_user->replace_children([&](Value* v) -> Value* {
                                    if (v == alloca) return load;
                                    return nullptr;
                                });
                            }

                            // Replace allocation of parameter type with an allocation of pointer type.
                            alloca->replace_with(new_alloca);

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

            for (auto block : function->blocks()) {
                for (size_t inst_i = 0; inst_i < block->instructions().size(); ++inst_i) {
                    auto*& instruction = block->instructions().at(inst_i);
                    switch (instruction->kind()) {
                        case Value::Kind::Return: {
                            auto ret = as<ReturnInst>(instruction);
                            // For large return types, we memcpy the returned value into the pointer
                            // passed as the hidden first arugument.
                            if (not ret_t_tworeg and ret_t_large) {
                                auto dest_ptr = ret_v_large;
                                auto source_ptr = ret->val();
                                if (not source_ptr->type()->is_ptr())
                                    Diag::ICE("IR ReturnInst returns large value but operand is not of pointer type");

                                auto byte_count = function_type->ret()->bytes();
                                std::vector<Value*> memcpy_operands{
                                    dest_ptr,
                                    source_ptr,
                                    new (*this) IntegerConstant(IntegerType::Get(context(), 64), byte_count)};
                                auto memcpy_inst = new (*this) IntrinsicInst(IntrinsicKind::MemCopy, memcpy_operands, ret->location());
                                ret->replace_with(memcpy_inst);
                                block->insert_after(new (*this) ReturnInst(nullptr), memcpy_inst);
                            }
                        } break;

                        case Value::Kind::Load: {
                            auto load = as<LoadInst>(instruction);

                            // Less than or equal to 8 bytes; nothing to change.
                            if (load->type()->bits() <= 64) continue;

                            auto users = load->users();
                            if (users.size() == 1 and users[0]->kind() == Value::Kind::Store) {
                                auto store = as<StoreInst>(users[0]);

                                auto source_ptr = load->ptr();
                                auto dest_ptr = store->ptr();

                                LCC_ASSERT(load->type()->bytes() == store->val()->type()->bytes());
                                auto byte_count = load->type()->bytes();

                                std::vector<Value*> memcpy_operands{
                                    dest_ptr,
                                    source_ptr,
                                    new (*this) IntegerConstant(IntegerType::Get(context(), 64), byte_count)};
                                auto memcpy_inst = new (*this) IntrinsicInst(IntrinsicKind::MemCopy, memcpy_operands, load->location());
                                load->replace_with(memcpy_inst);

                                store->erase();
                            } else {
                                // Possiblities:
                                // - generate builtin memcpy for backend to handle
                                // - unroll into 8 byte loads, temporary pointer stored into then
                                //   incremented
                                // - just copy the ptr instead, and everywhere that uses a load should
                                //   handle the fact that over-sized loads will be pointers instead.
                                auto copy = new (*this) CopyInst(load->ptr());
                                load->replace_with(copy);
                            }
                        } break;

                        case Value::Kind::Store: {
                            auto store = as<StoreInst>(instruction);

                            // Less than or equal to 8 bytes; nothing to change.
                            if (store->val()->type()->bits() <= 64) continue;
                            auto byte_count = store->val()->type()->bytes();

                            // Change type of val to pointer, if it isn't already.
                            // TODO: Does this apply to values other than Parameter? Does it need to?
                            if (auto param = cast<Parameter>(store->val()))
                                store->val(new (*this) Parameter(Type::PtrTy, param->index()));

                            LCC_ASSERT(
                                store->val()->type() == Type::PtrTy,
                                "Cannot lower store to memcpy when value is not of pointer type"
                            );

                            // Generate builtin memcpy
                            std::vector<Value*> memcpy_operands{
                                store->ptr(),
                                // The value should have already been lowered to a pointer.
                                store->val(),
                                new (*this) IntegerConstant(
                                    IntegerType::Get(context(), 64),
                                    byte_count
                                )};
                            auto memcpy_inst = new (*this) IntrinsicInst(IntrinsicKind::MemCopy, memcpy_operands, store->location());
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
    switch (_ctx->format()->format()) {
        case Format::INVALID: LCC_UNREACHABLE();

        case Format::LCC_IR: {
            // FIXME: Whoever made this only work on stdout is dumb.
            fmt::print("; We are currently stupid about things and can only print IR to stdout (sorry)\n");
            print_ir(false);
        } break;

        case Format::LLVM_TEXTUAL_IR: {
            auto llvm_ir = llvm();
            if (output_file_path.empty() || output_file_path == "-")
                fmt::print("{}", llvm_ir);
            else File::WriteOrTerminate(llvm_ir.c_str(), llvm_ir.size(), output_file_path);
        } break;

        case Format::COFF_OBJECT:
        case Format::ELF_OBJECT:
        case Format::GNU_AS_ATT_ASSEMBLY: {
            auto machine_ir = mir();

            if (_ctx->should_print_mir())
                fmt::print("{}", PrintMIR(vars(), machine_ir));

            for (auto& mfunc : machine_ir)
                select_instructions(this, mfunc);

            if (_ctx->should_print_mir()) {
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

            if (_ctx->should_print_mir()) {
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

            if (_ctx->stopat_mir()) std::exit(0);

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
