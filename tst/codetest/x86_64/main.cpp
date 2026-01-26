#include <fmt/format.h>

#include <lcc/calling_conventions/ms_x64.hh>
#include <lcc/calling_conventions/sysv_x86_64.hh>
#include <lcc/codegen/isel.hh>
#include <lcc/codegen/mir.hh>
#include <lcc/codegen/register_allocation.hh>
#include <lcc/codegen/x86_64/x86_64.hh>
#include <lcc/context.hh>
#include <lcc/core.hh>
#include <lcc/format.hh>
#include <lcc/ir/core.hh>
#include <lcc/ir/module.hh>
#include <lcc/opt/opt.hh>
#include <lcc/target.hh>
#include <lcc/utils.hh>

#include <span>
#include <string>
#include <string_view>
#include <variant>
#include <vector>

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
                "    GOT {} (0x{:x}), EXPECTED {} (0x{:x})\n",
                lcc::x86_64::opcode_to_string(input.opcode()),
                input.opcode(),
                lcc::x86_64::opcode_to_string(opcode),
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

            static_assert(
                std::variant_size_v<lcc::MOperand> == 6,
                "Exhaustive handling of MOperand kinds in CodeTest"
            );
            if (std::holds_alternative<lcc::MOperandRegister>(got)) {
                auto r_got = std::get<lcc::MOperandRegister>(got);
                auto r_expected = std::get<lcc::MOperandRegister>(expected);
                if (r_got.value != r_expected.value) {
                    fmt::print(
                        "  Register operand value does not match expected...\n"
                        "    GOT {}, EXPECTED {}\n",
                        r_got.value,
                        r_expected.value
                    );
                    return false;
                }
                if (r_got.size != r_expected.size) {
                    fmt::print(
                        "  Register operand size does not match expected...\n"
                        "    GOT {}, EXPECTED {}\n",
                        r_got.size,
                        r_expected.size
                    );
                    return false;
                }
            } else if (std::holds_alternative<lcc::MOperandImmediate>(got)) {
                auto imm_got = std::get<lcc::MOperandImmediate>(got);
                auto imm_expected = std::get<lcc::MOperandImmediate>(expected);
                if (imm_got.value != imm_expected.value) {
                    fmt::print(
                        "  Immediate operand value does not match expected...\n"
                        "    GOT {}, EXPECTED {}\n",
                        imm_got.value,
                        imm_expected.value
                    );
                    return false;
                }
                if (imm_got.size != imm_expected.size) {
                    fmt::print(
                        "  Immediate operand size does not match expected...\n"
                        "    GOT {}, EXPECTED {}\n",
                        imm_got.size,
                        imm_expected.size
                    );
                    return false;
                }
            } else if (std::holds_alternative<lcc::MOperandLocal>(got)) {
                auto local_got = std::get<lcc::MOperandLocal>(got);
                auto local_expected = std::get<lcc::MOperandLocal>(expected);
                if (local_got.index != local_expected.index) {
                    fmt::print(
                        "  Local index does not match expected...\n"
                        "    GOT {}, EXPECTED {}\n",
                        local_got.index,
                        local_expected.index
                    );
                    return false;
                }
                if (local_got.offset != local_expected.offset) {
                    fmt::print(
                        "  Local offset does not match expected...\n"
                        "    GOT {}, EXPECTED {}\n",
                        local_got.offset,
                        local_expected.offset
                    );
                    return false;
                }
            } else LCC_TODO("Implement matcher for MOperand type index {}...", got.index());
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
                fmt::print(
                    "  Instruction did not match expected...\n    {}\n",
                    lcc::PrintMInstImpl(got, lcc::x86_64::opcode_to_string)
                );
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
bool run_test(MIRMatcher& matcher, std::string_view test_source, const lcc::Target* target, const lcc::Format* format, int optimise_level, std::string_view optimisation_passes) {
    auto ctx = lcc::Context{
        target,
        format,
        {
            lcc::Context::UseColour,
            lcc::Context::DoNotPrintStats,
            lcc::Context::DoNotDiagBacktrace,
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

    // Print Source MIR
    // for (auto& mir_f : machine_ir) {
    //     fmt::print(
    //         "{}",
    //         PrintMFunctionImpl(mir_f, lcc::x86_64::opcode_to_string)
    //     );
    // }

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
    else if (instruction_opcode == "push")
        out.opcode = lcc::operator+(lcc::x86_64::Opcode::Push);
    else if (instruction_opcode == "pop")
        out.opcode = lcc::operator+(lcc::x86_64::Opcode::Pop);
    else if (instruction_opcode == "jmp")
        out.opcode = lcc::operator+(lcc::x86_64::Opcode::Jump);
    else if (instruction_opcode == "call")
        out.opcode = lcc::operator+(lcc::x86_64::Opcode::Call);
    else if (instruction_opcode == "mov")
        out.opcode = lcc::operator+(lcc::x86_64::Opcode::Move);
    else if (instruction_opcode == "movsx")
        out.opcode = lcc::operator+(lcc::x86_64::Opcode::MoveSignExtended);
    else if (instruction_opcode == "movzx")
        out.opcode = lcc::operator+(lcc::x86_64::Opcode::MoveZeroExtended);
    else if (instruction_opcode == "mov.dereflhs")
        out.opcode = lcc::operator+(lcc::x86_64::Opcode::MoveDereferenceLHS);
    else if (instruction_opcode == "mov.derefrhs")
        out.opcode = lcc::operator+(lcc::x86_64::Opcode::MoveDereferenceRHS);
    else if (instruction_opcode == "lea")
        out.opcode = lcc::operator+(lcc::x86_64::Opcode::LoadEffectiveAddress);
    else if (instruction_opcode == "not")
        out.opcode = lcc::operator+(lcc::x86_64::Opcode::Not);
    else if (instruction_opcode == "and")
        out.opcode = lcc::operator+(lcc::x86_64::Opcode::And);
    else if (instruction_opcode == "or")
        out.opcode = lcc::operator+(lcc::x86_64::Opcode::Or);
    else if (instruction_opcode == "xor")
        out.opcode = lcc::operator+(lcc::x86_64::Opcode::Xor);
    else if (instruction_opcode == "sar")
        out.opcode = lcc::operator+(lcc::x86_64::Opcode::ShiftRightArithmetic);
    else if (instruction_opcode == "shr")
        out.opcode = lcc::operator+(lcc::x86_64::Opcode::ShiftRightLogical);
    else if (instruction_opcode == "shl")
        out.opcode = lcc::operator+(lcc::x86_64::Opcode::ShiftLeft);
    else if (instruction_opcode == "add")
        out.opcode = lcc::operator+(lcc::x86_64::Opcode::Add);
    else if (instruction_opcode == "mul")
        out.opcode = lcc::operator+(lcc::x86_64::Opcode::Multiply);
    else if (instruction_opcode == "sub")
        out.opcode = lcc::operator+(lcc::x86_64::Opcode::Sub);
    else if (instruction_opcode == "idiv")
        out.opcode = lcc::operator+(lcc::x86_64::Opcode::SignedDivide);
    else if (instruction_opcode == "cmp")
        out.opcode = lcc::operator+(lcc::x86_64::Opcode::Compare);
    else if (instruction_opcode == "test")
        out.opcode = lcc::operator+(lcc::x86_64::Opcode::Test);
    else if (instruction_opcode == "jz")
        out.opcode = lcc::operator+(lcc::x86_64::Opcode::JumpIfZeroFlag);
    else if (instruction_opcode == "sete")
        out.opcode = lcc::operator+(lcc::x86_64::Opcode::SetByteIfEqual);
    else if (instruction_opcode == "setne")
        out.opcode = lcc::operator+(lcc::x86_64::Opcode::SetByteIfNotEqual);
    else if (instruction_opcode == "setbe")
        out.opcode = lcc::operator+(lcc::x86_64::Opcode::SetByteIfEqualOrLessUnsigned);
    else if (instruction_opcode == "setle")
        out.opcode = lcc::operator+(lcc::x86_64::Opcode::SetByteIfEqualOrLessSigned);
    else if (instruction_opcode == "setae")
        out.opcode = lcc::operator+(lcc::x86_64::Opcode::SetByteIfEqualOrGreaterUnsigned);
    else if (instruction_opcode == "setge")
        out.opcode = lcc::operator+(lcc::x86_64::Opcode::SetByteIfEqualOrGreaterSigned);
    else if (instruction_opcode == "setb")
        out.opcode = lcc::operator+(lcc::x86_64::Opcode::SetByteIfLessUnsigned);
    else if (instruction_opcode == "setl")
        out.opcode = lcc::operator+(lcc::x86_64::Opcode::SetByteIfLessSigned);
    else if (instruction_opcode == "seta")
        out.opcode = lcc::operator+(lcc::x86_64::Opcode::SetByteIfGreaterUnsigned);
    else if (instruction_opcode == "setg")
        out.opcode = lcc::operator+(lcc::x86_64::Opcode::SetByteIfGreaterSigned);
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

            while (offset < contents.size() and contents[offset] != '}' and contents[offset] != '\n') {
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

            if (offset >= contents.size() or contents[offset] != '}') {
                // ERROR! Expected '}'
                fmt::print("ERROR! Expected '}}' to close clobber list\n");
                std::exit(1);
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
        } else if (operand.starts_with("local(")) {
            // Looks like "local(3)+0"
            lcc::MOperandLocal local{};
            // Parse number after "local("
            auto index_string = operand.substr(6);
            lcc::u32 i = lcc::MOperandLocal::bad_index;
            if (index_string.starts_with("abs"))
                i = lcc::MOperandLocal::absolute_index;
            else i = (decltype(i)) std::stoi(index_string);

            local.index = lcc::u32(i);

            // Parse numberf after '+'
            auto plus_location = operand.find('+');
            auto offset_value = std::stoi(operand.substr(plus_location));

            local.offset = offset_value;

            out.operands.emplace_back(local);
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
        fmt::print("ERROR! Could not parse test matcher... (Expected colon ':' after block name)\n");
        std::exit(1);
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
        fmt::print("ERROR! Could not parse test matcher... (Expected colon ':' after function name)\n");
        std::exit(1);
    }

    std::string function_name{
        contents.begin() + lcc::isz(function_name_begin),
        contents.begin() + lcc::isz(offset)
    };

    // Eat ':'
    ++offset;

    // TODO: Optionally parse locals list, if present.

    // Parse two spaces, then basic block name followed by ':' on next line
    ToBeginningOfNextLine();
    while (ConsumeTwoSpaces()) {
        auto m = parse_block_matcher(contents, offset);
        out.blocks.emplace_back(m);
    }

    // If there is not two spaces, then there is only one other possibility:
    // another function. So, if we are certain there are not two spaces, but
    // we /are/ at a space, there is an indentation error in the test matcher.
    if (offset < contents.size() and contents[offset] == ' ') {
        fmt::print("ERROR! Invalid indentation: Expected block or function name\n");
        std::exit(1);
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

[[nodiscard]]
MIRMatcher parse_matcher(std::span<char> contents) {
    lcc::usz offset{};
    return parse_matcher(contents, offset);
}

struct CCMatcher {
    const lcc::Target* target{};
    MIRMatcher matcher{};
};

struct Test {
    std::vector<CCMatcher> matchers{};
    std::string source{};

    std::string name{};

    bool should_skip{false};
};

Test parse_test(std::vector<char>& inputs, lcc::usz& i) {
    bool should_skip{false};

    auto ToNewline = [&]() {
        while (i < inputs.size() and inputs.at(i) != '\n')
            ++i;
    };
    auto SkipNewlines = [&]() {
        while (i < inputs.size() and inputs.at(i) == '\n')
            ++i;
    };
    auto ToNextLine = [&]() {
        ToNewline();
        SkipNewlines();
    };
    // Parse test name segment
    if (inputs.at(i) != '=') {
        fmt::print("ERROR! Could not parse tests... (didn't find opening '=' line)\n");
        std::exit(1);
    }
    // Eat opening '=' line
    ToNextLine();

    auto test_name_begin = i;
    ToNewline();
    std::string test_name{
        inputs.begin() + lcc::isz(test_name_begin),
        inputs.begin() + lcc::isz(i)
    };
    SkipNewlines();

    // Parse test specifier
    while (i < inputs.size() and inputs.at(i) == ':') {
        auto specifier_begin = i;
        ToNextLine();
        std::string specifier{
            inputs.begin() + lcc::isz(specifier_begin),
            inputs.begin() + lcc::isz(i)
        };

        if (specifier.starts_with(":skip")) {
            should_skip = true;
        } else {
            fmt::print("ERROR! Invalid test specifier \"{}\"\n", specifier);
            std::exit(1);
        }
    }

    // Eat closing '=' line
    if (i >= inputs.size() or inputs.at(i) != '=') {
        fmt::print("ERROR! Invalid closing name line ('=') for test {}\n", test_name);
        std::exit(1);
    }
    ToNextLine();

    // Parse test source segment
    auto test_source_begin = i;
    //   Skip lines until end of segment.
    while (i < inputs.size() and inputs.at(i) != '-')
        ToNextLine();

    std::string test_source{
        inputs.begin() + lcc::isz(test_source_begin),
        inputs.begin() + lcc::isz(i)
    };

    // Eat source closing '-' line
    std::vector<CCMatcher> matchers{};
    while (i < inputs.size() and inputs.at(i) != '=') {
        auto calling_convention_begin = i;
        ToNextLine();
        std::string calling_convention{
            inputs.begin() + lcc::isz(calling_convention_begin),
            inputs.begin() + lcc::isz(i)
        };

        const lcc::Target* target{};
        if (calling_convention.contains("sysv")) {
            target = lcc::Target::x86_64_linux;
        } else if (calling_convention.contains("ms")) {
            target = lcc::Target::x86_64_windows;
        } else {
            fmt::print(
                "ERROR! Expected calling convention in first line of test segment (containing '-'). Expected ccs: sysv, ms\n"
            );
            std::exit(1);
        }

        // Parse test matcher segment
        auto test_result_begin = i;
        //   Skip lines until end of segment.
        while (i < inputs.size() and inputs.at(i) != '=' and inputs.at(i) != '-')
            ToNextLine();

        std::string test_result{
            inputs.begin() + lcc::isz(test_result_begin),
            inputs.begin() + lcc::isz(i)
        };

        matchers.emplace_back(target, parse_matcher(test_result));
    }

    return {matchers, test_source, test_name, should_skip};
}

std::string_view ToString(const lcc::Target* t) {
    if (t == lcc::Target::x86_64_linux) return "x86_64_linux";
    if (t == lcc::Target::x86_64_windows) return "x86_64_windows";
    LCC_UNREACHABLE();
}

int main(int argc, char** argv) {
    lcc::utils::Colours C{true};
    {
        // Read all files in corpus/
        // TODO: Recursively
        for (const auto& entry : std::filesystem::directory_iterator("corpus")) {
            if (entry.is_regular_file()) {
                fmt::print("{}:\n", entry.path().lexically_normal().string());

                auto inputs = lcc::File::Read(entry.path());
                lcc::usz i{0};
                while (i < inputs.size()) {
                    auto t = parse_test(inputs, i);
                    if (t.should_skip) continue;
                    fmt::print("{}\n", t.name);
                    for (auto m : t.matchers) {
                        fmt::print("  {}: ", ToString(m.target));
                        // Run the test with the given matcher and target,
                        // and print a useful message.
                        if (
                            run_test(
                                m.matcher,
                                t.source,
                                m.target,
                                lcc::Format::gnu_as_att_assembly,
                                0,
                                ""
                            )
                        ) {
                            fmt::print(
                                "{}PASSED{}\n",
                                C(lcc::utils::Colour::BoldGreen),
                                C(lcc::utils::Colour::Reset)
                            );
                        } else fmt::print(
                            "{}FAILED{}\n",
                            C(lcc::utils::Colour::BoldRed),
                            C(lcc::utils::Colour::Reset)
                        );
                    }
                }
            } else if (entry.is_directory()) {
                // TODO: Recurse...
            }
        }
    }

    return 0;
}
