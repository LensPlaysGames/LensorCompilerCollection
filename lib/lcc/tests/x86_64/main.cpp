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

    for (auto& mir_f : machine_ir) {
        fmt::print(
            "{}",
            PrintMFunctionImpl(mir_f, lcc::x86_64::opcode_to_string)
        );
    }

    return matcher.match(machine_ir);
}

lcc::x86_64::RegisterId register_operand_value(std::string_view operand) {
    if (operand.starts_with("rdi"))
        return (lcc::x86_64::RegisterId::RDI);
    if (operand.starts_with("rsi"))
        return (lcc::x86_64::RegisterId::RSI);
    if (operand.starts_with("rax"))
        return (lcc::x86_64::RegisterId::RAX);
    if (operand.starts_with("rbx"))
        return (lcc::x86_64::RegisterId::RBX);
    if (operand.starts_with("rcx"))
        return (lcc::x86_64::RegisterId::RCX);
    if (operand.starts_with("rdx"))
        return (lcc::x86_64::RegisterId::RDX);
    if (operand.starts_with("rbp"))
        return (lcc::x86_64::RegisterId::RBP);
    if (operand.starts_with("rsp"))
        return (lcc::x86_64::RegisterId::RSP);
    if (operand.starts_with("rip"))
        return (lcc::x86_64::RegisterId::RIP);
    if (operand.starts_with("r8"))
        return (lcc::x86_64::RegisterId::R8);
    if (operand.starts_with("r9"))
        return (lcc::x86_64::RegisterId::R9);
    if (operand.starts_with("r10"))
        return (lcc::x86_64::RegisterId::R10);
    if (operand.starts_with("r11"))
        return (lcc::x86_64::RegisterId::R11);
    if (operand.starts_with("r12"))
        return (lcc::x86_64::RegisterId::R12);
    if (operand.starts_with("r13"))
        return (lcc::x86_64::RegisterId::R13);
    if (operand.starts_with("r14"))
        return (lcc::x86_64::RegisterId::R14);
    if (operand.starts_with("r15"))
        return (lcc::x86_64::RegisterId::R15);
    return lcc::x86_64::RegisterId::INVALID;
}

lcc::usz register_operand_size(std::string_view operand) {
    if (operand.ends_with(".64"))
        return 64;
    if (operand.ends_with(".32"))
        return 32;
    if (operand.ends_with(".16"))
        return 16;
    if (operand.ends_with(".8"))
        return 8;
    return 0;
}

[[nodiscard]]
MIRInstructionMatcher parse_instruction_matcher(std::span<char> contents, lcc::usz& offset) {
    auto ToNewline = [&]() {
        // Eat everything until '\n'
        while (offset < contents.size() and contents[offset] != '\n')
            ++offset;
    };

    // Move 'i' to the offset of the beginning of the next non-empty line
    auto ToBeginningOfNextLine = [&]() {
        // Eat everything until '\n' (nothing if at '\n')
        ToNewline();
        // Eat all consecutive '\n'
        while (offset < contents.size() and contents[offset] == '\n')
            ++offset;
    };

    auto ToWhitespace = [&]() {
        while (offset < contents.size() and not isspace(contents[offset]))
            ++offset;
    };

    auto ToWhitespaceOr = [&](std::string_view until_chars) {
        while (offset < contents.size() and not isspace(contents[offset]) and not until_chars.contains(contents[offset]))
            ++offset;
    };

    auto SkipWhitespaceWithinLine = [&]() {
        // Skip all whitespace except newline
        while (offset < contents.size() and isspace(contents[offset]) and contents[offset] != '\n')
            ++offset;
    };

    MIRInstructionMatcher out{};

    auto instruction_opcode_begin = offset;
    ToWhitespace();
    std::string instruction_opcode{
        contents.begin() + lcc::isz(instruction_opcode_begin),
        contents.begin() + lcc::isz(offset),
    };
    SkipWhitespaceWithinLine();

    // Translate parsed instruction opcodes into x86_64 opcode values.
    if (instruction_opcode == "ret")
        out.opcode = lcc::operator+(lcc::x86_64::Opcode::Return);
    else if (instruction_opcode == "mov")
        out.opcode = lcc::operator+(lcc::x86_64::Opcode::Move);
    else if (instruction_opcode == "add")
        out.opcode = lcc::operator+(lcc::x86_64::Opcode::Add);
    else {
        fmt::print(
            "ERROR! Parsed invalid instruction opcode {}",
            instruction_opcode
        );
        std::exit(1);
    }

    // Parse operands (until newline)
    while (offset < contents.size() and contents[offset] != '\n') {
        // Parse clobber list, if present
        if (contents[offset] == '{') {
            // Eat '{'
            ++offset;

            SkipWhitespaceWithinLine();
            auto keyword_begin = offset;
            ToWhitespace();
            std::string keyword{
                contents.begin() + lcc::isz(keyword_begin),
                contents.begin() + lcc::isz(offset)
            };
            if (keyword != "CLOBBERS:") {
                // ERROR! Expected "CLOBBERS:"
                fmt::print("ERROR! Parser expected \"CLOBBERS:\" following '{{'");
                std::exit(1);
            }

            while (
                offset < contents.size()
                and contents[offset] != '}'
                and contents[offset] != '\n'
            ) {
                SkipWhitespaceWithinLine();
                auto clobber_begin = offset;
                ToWhitespaceOr("}");
                std::string clobber{
                    contents.begin() + lcc::isz(clobber_begin),
                    contents.begin() + lcc::isz(offset)
                };

                if (clobber.starts_with("op.")) {
                    // Parse number after '.'
                    auto n = std::stoi(clobber.substr(3));
                    if (lcc::usz(n) >= out.operands.size()) {
                        // ERROR! Operand clobber out of range
                        fmt::print("ERROR! Parsed operand clobber that is out of range");
                        std::exit(1);
                    }
                    out.operand_clobbers.emplace_back(n);
                } else if (clobber.starts_with("r")) {
                    // Parse register
                    auto r = lcc::operator+(register_operand_value(clobber));
                    out.register_clobbers.emplace_back(r);
                } else {
                    fmt::print("ERROR! Parser expected clobber operand starting with \"op.\" or 'r'.");
                    std::exit(1);
                }
            }

            if (contents[offset] != '}') {
                // ERROR! Expected '}'
            }
            // Eat '}'
            ++offset;
        }

        auto operand_begin = offset;
        ToWhitespace();
        std::string operand{
            contents.begin() + lcc::isz(operand_begin),
            contents.begin() + lcc::isz(offset)
        };
        SkipWhitespaceWithinLine();

        if (operand.starts_with("r")) {
            lcc::Register r{};

            r.value = lcc::operator+(register_operand_value(operand));
            r.size = (unsigned int) register_operand_size(operand);

            out.operands.emplace_back(lcc::MOperandRegister(r));
        }
    }

    // Move parse state to beginning of next line following successful parsing
    // of an instruction.
    ToBeginningOfNextLine();

    return out;
}

[[nodiscard]]
MIRBlockMatcher parse_block_matcher(std::span<char> contents, lcc::usz& offset) {
    auto ConsumeFourSpaces = [&]() {
        if (offset + 3 >= contents.size())
            return false;
        if (
            contents[offset + 3] != ' '
            or contents[offset + 2] != ' '
            or contents[offset + 1] != ' '
            or contents[offset] != ' '
        )
            return false;
        offset += 4;
        return true;
    };

    auto ToNewline = [&]() {
        // Eat everything until '\n'
        while (offset < contents.size() and contents[offset] != '\n')
            ++offset;
    };

    // Move 'i' to the offset of the beginning of the next non-empty line
    auto ToBeginningOfNextLine = [&]() {
        // Eat everything until '\n' (nothing if at '\n')
        ToNewline();
        // Eat all consecutive '\n'
        while (offset < contents.size() and contents[offset] == '\n')
            ++offset;
    };

    auto ToColonCurrentLine = [&]() {
        while (offset < contents.size() and contents[offset] != ':') {
            if (contents[offset] == '\n') return false;
            ++offset;
        }
        return true;
    };

    MIRBlockMatcher out{};

    auto block_name_begin = offset;

    if (not ToColonCurrentLine()) {
        // ERROR: Expected block name followed by ':'
        return {};
    }

    std::string block_name{
        contents.begin() + lcc::isz(block_name_begin),
        contents.begin() + lcc::isz(offset)
    };

    // The first line with less than four spaces after opening a basic block
    // must either be indented with two spaces and the name of a new block,
    // or indented with no spaces and the name of a new function.
    ToBeginningOfNextLine();
    while (ConsumeFourSpaces()) {
        // Parse instruction
        auto m = parse_instruction_matcher(contents, offset);
        out.instructions.emplace_back(m);
    }

    // Block definition complete.
    return out;
}

[[nodiscard]]
MIRFunctionMatcher parse_function_matcher(std::span<char> contents, lcc::usz& offset) {
    auto ConsumeTwoSpaces = [&]() {
        if (offset + 1 >= contents.size())
            return false;
        if (contents[offset + 1] != ' ' or contents[offset] != ' ')
            return false;
        offset += 2;
        return true;
    };

    auto ToNewline = [&]() {
        // Eat everything until '\n'
        while (offset < contents.size() and contents[offset] != '\n')
            ++offset;
    };

    // Move 'i' to the offset of the beginning of the next non-empty line
    auto ToBeginningOfNextLine = [&]() {
        // Eat everything until '\n' (nothing if at '\n')
        ToNewline();
        // Eat all consecutive '\n'
        while (offset < contents.size() and contents[offset] == '\n')
            ++offset;
    };

    auto ToColonCurrentLine = [&]() {
        while (offset < contents.size() and contents[offset] != ':') {
            if (contents[offset] == '\n') return false;
            ++offset;
        }
        return true;
    };

    MIRFunctionMatcher out{};

    // Parse function name followed by ':' on first line
    auto function_name_begin = offset;

    if (not ToColonCurrentLine()) {
        // ERROR: Expected function name followed by ':'
        // TODO: Return meaningful error
        return {};
    }

    std::string function_name{
        contents.begin() + lcc::isz(function_name_begin),
        contents.begin() + lcc::isz(offset)
    };

    // Eat ':'
    ++offset;

    // TODO: Optionally parse locals list, if present.
    // TODO: Parse two spaces, then basic block name followed by ':' on next line
    ToBeginningOfNextLine();
    while (ConsumeTwoSpaces()) {
        auto m = parse_block_matcher(contents, offset);
        out.blocks.emplace_back(m);
    }

    return out;
}

[[nodiscard]]
MIRMatcher parse_matcher(std::span<char> contents, lcc::usz& offset) {
    MIRMatcher out{};

    while (offset < contents.size()) {
        auto m = parse_function_matcher(contents, offset);
        out.functions.emplace_back(m);
    }

    return out;
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
        std::string test_result =
            "func:\n"
            "  bb0:\n"
            "    mov rdi.64 rax.64 {CLOBBERS: op.1}\n"
            "    ret\n"
            "memcpy:\n";

        lcc::usz i{0};
        auto parsed_matcher = parse_matcher(test_result, i);

        if (run_test(parsed_matcher, test_source, 0, ""))
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

        std::string test_result =
            "func:\n"
            "  bb0:\n"
            "    add rdi.64 rsi.64\n"
            "    mov rsi.64 rax.64 {CLOBBERS: op.1}\n"
            "    mov rax.64 rax.64 {CLOBBERS: op.1}\n"
            "    ret\n"
            "memcpy:\n";

        lcc::usz i{0};
        auto parsed_matcher = parse_matcher(test_result, i);

        if (run_test(parsed_matcher, test_source, 0, ""))
            fmt::print("PASSED: ");
        else fmt::print("FAILED: ");
        fmt::print("{}\n", test_name);
    }

    return 0;
}
