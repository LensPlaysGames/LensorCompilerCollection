#include <algorithm>
#include <fmt/format.h>
#include <lcc/codegen/generic_object.hh>
#include <lcc/codegen/isel.hh>
#include <lcc/codegen/mir.hh>
#include <lcc/codegen/register_allocation.hh>
#include <lcc/codegen/x86_64.hh>
#include <lcc/codegen/x86_64/assembly.hh>
#include <lcc/codegen/x86_64/object.hh>
#include <lcc/context.hh>
#include <lcc/diags.hh>
#include <lcc/format.hh>
#include <lcc/ir/ir.hh>
#include <lcc/ir/module.hh>
#include <lcc/target.hh>
#include <lcc/utils.hh>
#include <lcc/utils/ir_printer.hh>

// NOTE: See module_mir.cc for Machine Instruction Representation (MIR)
// generation.

namespace lcc {

u64 operator+(MOperandLocal l) { return static_cast<u64>(l); }

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

                                // NOTE(local): For now, allowing the IR to otherwise handle big loads like any other type,
                                // relying on the backend to decide what happens.
                                // LCC_ASSERT(false, "TODO: Handle load > 8 bytes lowering");
                            }
                        } break;

                        case Value::Kind::Store: {
                            auto store = as<StoreInst>(instruction);

                            // Less than or equal to 8 bytes; nothing to change.
                            if (store->type()->bits() <= 64) continue;

                            // NOTE(local): For now, allowing the IR to handle big stores like any other type,
                            // relying on the backend to decide what happens.
                            // LCC_ASSERT(false, "TODO: Handle store > 8 bytes lowering");
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
                fmt::print("After ISel\n");
                if (_ctx->target()->is_x64()) {
                    for (auto& f : machine_ir)
                        fmt::print("{}\n", PrintMFunctionImpl(f, x86_64::opcode_to_string));
                } else {
                    fmt::print(
                        "{}\n",
                        fmt::join(vws::transform(machine_ir, PrintMFunction), "\n")
                    );
                }
            }

            // Register Allocation
            MachineDescription desc{};
            if (_ctx->target()->is_x64()) {
                desc.return_register_to_replace = +x86_64::RegisterId::RETURN;
                if (_ctx->target()->is_windows()) {
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
                fmt::print("After RA\n");
                if (_ctx->target()->is_x64()) {
                    for (auto& f : machine_ir)
                        fmt::print("{}\n", PrintMFunctionImpl(f, x86_64::opcode_to_string));
                } else {
                    fmt::print(
                        "{}\n",
                        fmt::join(vws::transform(machine_ir, PrintMFunction), "\n")
                    );
                }
                std::exit(0);
            }

            if (_ctx->format()->format() == Format::GNU_AS_ATT_ASSEMBLY) {
                if (_ctx->target()->is_x64())
                    x86_64::emit_gnu_att_assembly(output_file_path, this, desc, machine_ir);
                else LCC_ASSERT(false, "Unhandled code emission target, sorry");
            } else if (_ctx->format()->format() == Format::ELF_OBJECT) {
                GenericObject gobj{};
                if (_ctx->target()->is_x64())
                    gobj = x86_64::emit_mcode_gobj(this, desc, machine_ir);
                else LCC_ASSERT(false, "Unhandled code emission target, sorry");

                // TODO: Have some way for "frontend" to specify sections in the output
                // object file, such that Intercept can store it's module metadata in
                // there.

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
