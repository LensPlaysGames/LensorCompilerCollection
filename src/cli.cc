#include <cli.hh>

#include <lcc/utils/twocolumnlayouthelper.hh>

#include <cstdlib>
#include <fmt/format.h>
#include <string>
#include <string_view>
#include <vector>

namespace cli {

[[noreturn]]
void help() {
    // clang-format off
    fmt::print("USAGE: lcc [FLAGS] [OPTIONS] [SOURCE FILES...]\n");
    fmt::print("FLAGS:\n");
    fmt::print("{}", TwoColumnLayoutHelper{{
        {"  -v", "Enable verbose output\n"},
        {"  --ast", "Emit AST in human-readable format\n"},
        {"  --ir", "Emit LCC intermediate representation\n"},
        {"  --mir", "Emit LCC machine instruction representation at various stages\n"},
        {"  --stopat-lex", "Request language does not process input further than lexical analysis\n"},
        {"  --stopat-syntax", "Request language does not process input further than syntactic analysis\n"},
        {"  --stopat-sema", "Request language does not process input further than semantic analysis\n"},
        {"  --stopat-ir", "Do not process input further than LCC's intermediate representation (IR)\n"},
        {"  --stopat-mir", "Do not process input further than LCC's machine instruction representation (MIR)\n"},
        {"  --aluminium", "That special something to spice up your compilation\n"},
    }}.get());
    fmt::print("OPTIONS:\n");
    fmt::print("{}", TwoColumnLayoutHelper{{
        {"  -I", "Add a directory to the include search paths\n"},
        {"  -o", "Path to the output filepath where target code will be stored\n"},
        {"  -O", "Set optimisation level (default 0)\n"},
        {"", "    0, 1, 2, 3\n"},
        {"  --passes", "Comma-separated list of optimisation passes to run\n"},
        {"  --color", "Whether to include colors colours in the output (default: auto)\n"},
        {"", "    always, auto, never\n"},
        {"  -x", "What language to parse input code as (default: extension based)\n"},
        {"", "    glint, ir\n"},
        {"  -t", "What format to emit code in (default: matches building system)\n"},
        {"", "    x86_64_linux, x86_64_windows\n"},
        {"  -f", "What format to emit code in (default: asm)\n"},
        // Basically, the name on the left of the colon will pick a default from
        // one on the right of the colon based on the system the compiler was
        // compiled for.
        {"", "    asm: gnu-as-att,\n"},
        {"", "    obj: elf, coff,\n"},
        {"", "    IR: ir, ssa_ir, llvm\n"},
        {"", "        ir:     LCC's own IR, lowered for target architecture\n"},
        {"", "        ssa_ir: LCC's own IR in SSA form\n"},
        {"", "        llvm:   Textual IR for the Low Level Virtual Machine\n"},
    }}.get());
    // clang-format on
    std::exit(0);
}

auto parse(int argc, const char** argv) -> Options {
    Options o{};
    for (int i = 1; i < argc; ++i) {
        auto next_arg = [&]() {
            if (i + 1 >= argc) {
                fmt::print(
                    stderr,
                    "CLI ERROR: You probably gave an option and didn't supply a value at the very end of the command line\n"
                );
                std::exit(1);
            }
            return std::string_view{argv[++i]};
        };
        const auto arg = std::string_view{argv[i]};
        if (arg.substr(0, 3) == "--h" or arg.substr(0, 2) == "-h") {
            help();
        }

        if (arg == "-v")
            o.verbose = true;
        else if (arg == "--aluminium")
            o.aluminium = true;
        else if (arg == "--ast")
            o.ast = lcc::Context::PrintAST;
        else if (arg == "--stopat-lex")
            o.stopat_lex = lcc::Context::StopatLex;
        else if (arg == "--stopat-syntax")
            o.stopat_syntax = lcc::Context::StopatSyntax;
        else if (arg == "--stopat-sema")
            o.stopat_sema = lcc::Context::StopatSema;
        else if (arg == "--ir")
            o.ir = true;
        else if (arg == "--stopat-ir")
            o.stopat_ir = true;
        else if (arg == "--mir")
            o.mir = lcc::Context::PrintMIR;
        else if (arg == "--stopat-mir")
            o.stopat_mir = lcc::Context::StopatMIR;

        else if (arg == "-I") {
            // Add a directory to the include search paths
            auto include_dir = next_arg();
            o.include_directories.emplace_back(include_dir);
        } else if (arg == "-o") {
            // Path to the output filepath where target code will be stored
            auto output_path = next_arg();
            o.output_filepath = std::string(output_path);
        } else if (arg == "-O") {
            // Set optimisation level (default: 0)
            auto o_level_str = next_arg();

            auto o_level = 0;
            if (o_level_str == "0")
                ;
            else if (o_level_str == "1")
                o_level = 1;
            else if (o_level_str == "2")
                o_level = 2;
            else if (o_level_str == "3")
                o_level = 3;
            else {
                fmt::print("CLI ERROR: Invalid optimisation level {}\n", o_level_str);
                std::exit(1);
            }

            o.optimisation = o_level;
        } else if (arg == "--passes") {
            // Comma-separated list of optimisation passes to run
            auto passes = next_arg();
            o.optimisation_passes = passes;
        } else if (arg == "--color") {
            // Whether to include colours in the output
            auto color = next_arg();
            if (color != "always" and color != "auto" and color != "never") {
                fmt::print("CLI ERROR: Invalid color level {}\n", color);
                std::exit(1);
            }
            o.color = color;
        } else if (arg == "-x") {
            // What language to parse input code as
            auto lang = next_arg();
            if (lang != "glint" and lang != "ir") {
                fmt::print("CLI ERROR: Invalid lang {}\n", lang);
                std::exit(1);
            }
            o.language = lang;
        } else if (arg == "-t" or arg == "--target") {
            // What format to emit code in
            auto target = next_arg();
            if (target != "x86_64_linux" and target != "x86_64_windows") {
                fmt::print("CLI ERROR: Invalid target \"{}\"\n", target);
                std::exit(1);
            }
            o.target = target;
        } else if (arg == "-f") {
            // What format to emit code in
            auto format = next_arg();
            if (
                format != "asm" and format != "gnu-as-att"
                and format != "obj" and format != "elf" and format != "coff"
                and format != "IR"
                and format != "ir" and format != "ssa_ir" and format != "llvm"
            ) {
                fmt::print("CLI ERROR: Invalid format {}\n", format);
                std::exit(1);
            }
            o.format = format;
        } else if (arg.starts_with("-")) {
            fmt::print(
                "CLI ERROR: Unrecognized command line option or flag {}\n"
                "    Prepend ./ or equivalent for file that starts with '-'\n",
                arg
            );
            std::exit(1);
        } else {
            // Otherwise, it's a filepath
            o.input_files.emplace_back(arg);
        }
    }
    return o;
}

} // namespace cli
