#include <fmt/format.h>

#include <lcc/calling_conventions/ms_x64.hh>
#include <lcc/calling_conventions/sysv_x86_64.hh>
#include <lcc/codegen/isel.hh>
#include <lcc/codegen/mir.hh>
#include <lcc/codegen/register_allocation.hh>
#include <lcc/codegen/x86_64/x86_64.hh>
#include <lcc/context.hh>
#include <lcc/format.hh>
#include <lcc/ir/ir.hh>
#include <lcc/ir/module.hh>
#include <lcc/opt/opt.hh>
#include <lcc/target.hh>
#include <lcc/utils.hh>

struct MIRInstructionMatcher {
    // Could be lcc::MInst::Kind or lcc::x86_64::Opcode
    lcc::usz opcode{};
    std::vector<lcc::MOperand> operands{};
    // List of indices into operands that this instruction clobbers.
    std::vector<lcc::usz> operand_clobbers{};
    // List of register values that this instruction clobbers.
    std::vector<lcc::usz> register_clobbers{};

    [[nodiscard]]
    bool match(lcc::MInst input) {
        if (input.opcode() != lcc::usz(opcode)) {
            fmt::print(
                "  Instruction opcode does not match expected...\n"
                "    GOT 0x{:x}, EXPECTED 0x{:x}\n",
                input.opcode(),
                opcode
            );
            return false;
        }

        if (input.all_operands().size() != operands.size()) {
            fmt::print(
                "  Operand count does not match expected...\n"
                "    GOT {}, EXPECTED {}\n",
                input.all_operands().size(),
                operands.size()
            );
            return false;
        }
        for (auto [expected, got] : lcc::vws::zip(operands, input.all_operands())) {
            if (expected.index() != got.index()) {
                fmt::print(
                    "  Operand variant type does not match expected...\n"
                    "    GOT {}, EXPECTED {}\n",
                    got.index(),
                    expected.index()
                );
                return false;
            }

            // TODO: Compare MOperand value?
        }

        if (input.operand_clobbers().size() != operand_clobbers.size()) {
            fmt::print(
                "  Operand clobber count does not match expected...\n"
                "    GOT {}, EXPECTED {}\n",
                input.operand_clobbers().size(),
                operand_clobbers.size()
            );
            return false;
        }
        for (auto [expected, got] : lcc::vws::zip(operand_clobbers, input.operand_clobbers())) {
            if (expected != got) {
                fmt::print(
                    "  Operand clobber values do not match expected...\n"
                    "    GOT {}, EXPECTED {}\n",
                    got,
                    expected
                );
                return false;
            }
        }

        if (input.register_clobbers().size() != register_clobbers.size()) {
            fmt::print(
                "  Register clobber count does not match expected...\n"
                "    GOT {}, EXPECTED {}\n",
                input.register_clobbers().size(),
                register_clobbers.size()
            );
            return false;
        }
        for (auto [expected, got] : lcc::vws::zip(register_clobbers, input.register_clobbers())) {
            if (expected != got) {
                fmt::print(
                    "  Register clobber values do not match expected...\n"
                    "    GOT {}, EXPECTED {}",
                    got,
                    expected
                );
                return false;
            }
        }

        return true;
    }
};

struct MIRBlockMatcher {
    std::vector<MIRInstructionMatcher> instructions{};

    [[nodiscard]]
    bool match(std::vector<lcc::MInst> input) {
        if (input.size() != instructions.size()) {
            fmt::print("  Instruction count did not match expected...\n");
            return false;
        }

        for (auto [expected, got] : lcc::vws::zip(instructions, input)) {
            if (not expected.match(got)) {
                fmt::print("  Instruction did not match expected...\n");
                return false;
            }
        }

        return true;
    }
};

struct MIRFunctionMatcher {
    // TODO: Probably include signature here?
    std::vector<MIRBlockMatcher> blocks{};

    [[nodiscard]]
    bool match(std::vector<lcc::MBlock> input) {
        if (input.size() != blocks.size()) {
            fmt::print("  Block count did not match expected...\n");
            return false;
        }

        for (auto [expected, got] : lcc::vws::zip(blocks, input)) {
            if (not expected.match(got.instructions())) {
                fmt::print("  Block contents did not match expected...\n");
                return false;
            }
        }

        return true;
    }
};

struct MIRMatcher {
    std::vector<MIRFunctionMatcher> functions{};

    [[nodiscard]]
    bool match(std::vector<lcc::MFunction>& mir) {
        if (mir.size() != functions.size()) {
            fmt::print("  MIR function count did not match expected...\n");
            return false;
        }

        for (auto [expected, got] : lcc::vws::zip(functions, mir)) {
            if (not expected.match(got.blocks())) {
                fmt::print("  Blocks of function {} did not match expected...\n", got.names().at(0).name);
                return false;
            }
        }

        return true;
    }
};

[[nodiscard]]
bool run_test(MIRMatcher& matcher, std::string_view test_source, int optimise_level, std::string_view optimisation_passes) {
    auto ctx = lcc::Context{
        lcc::Target::x86_64_linux,
        lcc::Format::gnu_as_att_assembly,
        {
            lcc::Context::UseColour,
            lcc::Context::DoNotPrintAST,
            lcc::Context::DoNotStopatLex,
            lcc::Context::DoNotStopatSyntax,
            lcc::Context::DoNotStopatSema,
            lcc::Context::DoNotPrintMIR,
            lcc::Context::DoNotStopatMIR,
        }
    };

    auto& f = ctx.create_file(
        "test_source.lcc",
        std::vector<char>{test_source.begin(), test_source.end()}
    );

    // Source -> IR
    auto mod = lcc::Module::Parse(&ctx, f);
    if (not mod) return false;

    // fmt::print("PARSED:\n{}\n", mod->as_lcc_ir(true));

    // Optimisation
    // IR -> IR
    {
        if (not optimisation_passes.empty())
            lcc::opt::RunPasses(mod.get(), optimisation_passes);

        if (optimise_level and optimisation_passes.empty())
            lcc::opt::Optimise(mod.get(), optimise_level);
    }

    // IR -> lIR
    mod->lower();

    // lIR -> MIR
    auto machine_ir = mod->mir();

    // Instruction Selection
    // MIR -> lMIR
    for (auto& mfunc : machine_ir)
        select_instructions(mod.get(), mfunc);

    // Register Allocation
    // lMIR -> lMIR
    {
        lcc::MachineDescription desc{};
        if (ctx.target()->is_arch_x86_64()) {
            desc.return_register_to_replace = lcc::operator+(lcc::x86_64::RegisterId::RETURN);
            if (ctx.target()->is_cconv_ms()) {
                desc.return_register = lcc::operator+(lcc::cconv::msx64::return_register);
                // Just the volatile registers
                lcc::rgs::transform(
                    lcc::cconv::msx64::volatile_regs,
                    std::back_inserter(desc.registers),
                    [](auto r) { return lcc::operator+(r); }
                );
            } else {
                desc.return_register = lcc::operator+(lcc::cconv::sysv::return_register);
                // Just the volatile registers
                lcc::rgs::transform(
                    lcc::cconv::sysv::volatile_regs,
                    std::back_inserter(desc.registers),
                    [](auto r) { return lcc::operator+(r); }
                );
            }
        } else LCC_ASSERT(false, "Sorry, unhandled target architecture");

        for (auto& mfunc : machine_ir)
            allocate_registers(desc, mfunc);
    }

    // for (auto& mir_f : machine_ir) {
    //     fmt::print(
    //         "{}",
    //         PrintMFunctionImpl(mir_f, lcc::x86_64::opcode_to_string)
    //     );
    // }

    return matcher.match(machine_ir);
}

int main(int argc, char** argv) {
    // TODO: Get test source(s) from file(s).
    {
        std::string test_name = "x86_64 SysV Parameter Lowering: One i64";
        std::string test_source =
            "; LCC Module 'Test Module'\n"
            "func (exported): ccc i64(i64 %0):\n"
            "  bb0:\n"
            "    return i64 %0\n";

        // TODO: Parse matcher from file
        MIRMatcher matcher{
            .functions = {
                {.blocks = {
                     {.instructions = {
                          {.opcode = lcc::operator+(lcc::x86_64::Opcode::Move),
                           .operands = {
                               {lcc::MOperandRegister(
                                   {.value = lcc::operator+(lcc::cconv::sysv::arg_regs.at(0)), .size = 64}
                               )},
                               {lcc::MOperandRegister(
                                   {.value = lcc::operator+(lcc::cconv::sysv::return_register), .size = 64}
                               )}
                           },
                           .operand_clobbers = {1}},
                          {.opcode = lcc::operator+(lcc::x86_64::Opcode::Return)}
                      }}
                 }},
                {} // inserted memcpy
            }
        };

        if (run_test(matcher, test_source, 0, ""))
            fmt::print("PASSED: ");
        else fmt::print("FAILED: ");
        fmt::print("{}\n", test_name);
    }

    {
        std::string test_name = "x86_64 SysV Parameter Lowering: Two i64";
        std::string test_source =
            "; LCC Module 'Test Module'\n"
            "func (exported): ccc i64(i64 %0, i64 %1):\n"
            "  bb0:\n"
            "    %2 = add i64 %0, %1\n"
            "    return i64 %2\n";

        // TODO: Parse matcher from file
        MIRMatcher matcher{
            .functions = {
                {.blocks = {
                     {.instructions = {
                          {.opcode = lcc::operator+(lcc::x86_64::Opcode::Add),
                           .operands = {
                               {lcc::MOperandRegister(
                                   {.value = lcc::operator+(lcc::cconv::sysv::arg_regs.at(0)), .size = 64}
                               )},
                               {lcc::MOperandRegister(
                                   {.value = lcc::operator+(lcc::cconv::sysv::arg_regs.at(1)), .size = 64}
                               )}
                           },
                           .operand_clobbers = {},
                           .register_clobbers = {}},

                          {.opcode = lcc::operator+(lcc::x86_64::Opcode::Move),                                                           //
                           .operands = {{lcc::MOperandRegister({.value = lcc::operator+(lcc::cconv::sysv::arg_regs.at(1)), .size = 64})}, //
                                        {lcc::MOperandRegister({.value = lcc::operator+(lcc::cconv::sysv::return_register), .size = 64})}},
                           .operand_clobbers = {1}},

                          // Compiler generates an extra move from rax to rax...
                          {.opcode = lcc::operator+(lcc::x86_64::Opcode::Move),                                                            //
                           .operands = {{lcc::MOperandRegister({.value = lcc::operator+(lcc::cconv::sysv::return_register), .size = 64})}, //
                                        {lcc::MOperandRegister({.value = lcc::operator+(lcc::cconv::sysv::return_register), .size = 64})}},
                           .operand_clobbers = {1}},

                          {.opcode = lcc::operator+(lcc::x86_64::Opcode::Return)}
                      }}
                 }},
                {} // inserted memcpy
            }
        };

        if (run_test(matcher, test_source, 0, ""))
            fmt::print("PASSED: ");
        else fmt::print("FAILED: ");
        fmt::print("{}\n", test_name);
    }

    return 0;
}
