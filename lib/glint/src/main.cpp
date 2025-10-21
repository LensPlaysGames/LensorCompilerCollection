#include <glint/ast.hh>
#include <glint/ast_eval.hh>
#include <glint/ir_gen.hh>
#include <glint/parser.hh>
#include <glint/sema.hh>

#include <lcc/file.hh>
#include <lcc/format.hh>
#include <lcc/opt/opt.hh>
#include <lcc/target.hh>
#include <lcc/utils.hh>
#include <lcc/utils/twocolumnlayouthelper.hh>

#include <fmt/format.h>

/// Default target.
const lcc::Target* default_target =
#if defined(LCC_PLATFORM_WINDOWS)
    lcc::Target::x86_64_windows;
#elif defined(__APPLE__) or defined(__linux__)
    lcc::Target::x86_64_linux;
#else
#    error "Unsupported target"
#endif

/// Default format
const lcc::Format* default_format = lcc::Format::lcc_ssa_ir;

struct Options {
    std::vector<std::string> input_paths{};
    std::string output_path{};
    bool optimisation{false};
    bool evaluate{false};
};

void help() {
    fmt::print("USAGE: glintc [FLAGS] [OPTIONS] [SOURCE FILES...]\n");

    // clang-format off
    fmt::print("FLAGS:\n");
    fmt::print("{}", TwoColumnLayoutHelper{{
        {"  -h", "Show this help message\n"},
        {"  --eval", "Attempt to evaluate the input program(s) and print their result\n"},}
    } .get());

    fmt::print("OPTIONS:\n");
    fmt::print("{}", TwoColumnLayoutHelper{{
        {"  -o", "Set the output filepath. Only valid with exactly one input file\n"},}}
    .get());
    // clang-format on

    std::exit(0);
}

Options parse(int argc, char** argv) {
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
        if (arg.substr(0, 3) == "--h" or arg.substr(0, 2) == "-h" or arg.substr(0, 2) == "-?") {
            help();
        }

        if (arg.starts_with("-o")) {
            o.output_path = next_arg();
        } else if (arg.starts_with("--eval")) {
            o.evaluate = true;
        } else if (arg.starts_with("-")) {
            fmt::print(
                "CLI ERROR: Unrecognized command line option or flag {}\n"
                "    Prepend ./ or equivalent for file that starts with '-'\n",
                arg
            );
            std::exit(1);
        } else {
            // Otherwise, it's presumed to be a file path.
            o.input_paths.emplace_back(arg);
        }
    }
    return o;
}

int main(int argc, char** argv) {
    auto options = parse(argc, argv);
    lcc::utils::Colours C{true};
    using lcc::utils::Colour;

    if (options.input_paths.empty()) {
        lcc::Diag::Fatal("{}no input files", C(Colour::Red));
    }

    if (not options.output_path.empty() and options.input_paths.size() != 0) {
        lcc::Diag::Fatal("{}", C(Colour::Red));
    }

    const auto ConvertFileExtensionToOutputFormat = [&](lcc::Context* context, const std::string& path_string) {
        const char* replacement = "";
        switch (context->format()->format()) {
            case lcc::Format::INVALID:
                LCC_UNREACHABLE();

            case lcc::Format::LCC_IR:
            case lcc::Format::LCC_SSA_IR:
                replacement = ".lcc";
                break;

            case lcc::Format::LLVM_TEXTUAL_IR:
                replacement = ".ll";
                break;

            case lcc::Format::GNU_AS_ATT_ASSEMBLY:
                replacement = ".s";
                break;

            case lcc::Format::ELF_OBJECT:
            case lcc::Format::COFF_OBJECT:
                replacement = ".o";
                break;
        }

        return std::filesystem::path{path_string}
            .replace_extension(replacement)
            .string();
    };

    auto context = lcc::Context{
        default_target,
        default_format,
        lcc::Context::Options{
            lcc::Context::DoNotUseColour,
            lcc::Context::DoNotPrintAST,
            lcc::Context::DoNotStopatLex,
            lcc::Context::DoNotStopatSyntax,
            lcc::Context::DoNotStopatSema,
            lcc::Context::DoNotPrintMIR,
            lcc::Context::DoNotStopatMIR
        }
    };

    for (auto p : options.input_paths) {
        if (not std::filesystem::exists(p))
            lcc::Diag::Fatal("input file does not exist: {}", p);

        auto contents = lcc::File::Read(p);
        auto& f = context.create_file(p, contents);

        auto m = lcc::glint::Parser::Parse(&context, f);
        if (not m) return 1;
        if (context.has_error()) return 1;

        lcc::glint::Sema::Analyse(&context, *m);
        if (context.has_error()) return 1;

        if (options.evaluate) {
            {
                auto out = lcc::glint::evaluate(*m, m->top_level_function()->body());
                if (not out) {
                    fmt::print("[New Evaluation Failed]\n");
                } else {
                    auto result = m->ToSource(*out);
                    LCC_ASSERT(result, "Failed to get string representing evaluation result...");
                    fmt::print("{}\n", *result);
                }
            }

            lcc::glint::EvalResult out;
            if (not m->top_level_function()->body()->evaluate(&context, out, false)) {
                fmt::print("[Evaluation Failed]\n");
                return 1;
            }

            if (out.is_int())
                fmt::print("{}\n", out.as_int());
            else if (out.is_string())
                fmt::print("{}\n", m->strings.at(out.as_string()->string_index()));
            else if (out.is_null())
                fmt::print("null\n");
            else LCC_TODO("Implement EvalResult value kind");

            return 0;
        }

        auto* lcc_module = lcc::glint::IRGen::Generate(&context, *m);

        if (options.optimisation)
            lcc::opt::Optimise(lcc_module, int(options.optimisation));

        lcc_module->lower();

        lcc_module->emit(ConvertFileExtensionToOutputFormat(&context, p));
    }

    return 0;
}
