#include <langtest/langtest.hh>

#include <lcc/context.hh>
#include <lcc/format.hh>
#include <lcc/ir/core.hh>
#include <lcc/ir/module.hh>
#include <lcc/target.hh>
#include <lcc/utils.hh>

#include <glint/ast.hh>
#include <glint/ir_gen.hh>
#include <glint/parser.hh>
#include <glint/sema.hh>

#include <algorithm>
#include <filesystem>
#include <string>
#include <string_view>
#include <unordered_map>

static lcc::utils::Colours C{true};
using lcc::utils::Colour;

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
const lcc::Format* default_format = lcc::Format::gnu_as_att_assembly;

bool option_print;
bool option_suppress{true};

struct GlintTest : langtest::Test {
    static bool warning_reported(lcc::Context& ctx) {
        return lcc::rgs::any_of(
            ctx.diagnostics(),
            [](const lcc::Context::DiagnosticReport d) {
                return d.kind == lcc::Diag::Kind::Warning;
            }
        );
    }

    struct ParseInfo {
        std::unique_ptr<lcc::glint::Module> mod{};
        std::vector<lcc::Context::DiagnosticReport> diagnostics{};
    };
    /// Parse test source as Glint
    auto parse(lcc::Context& context) -> ParseInfo {
        // Parse Glint source code using the Glint parser into a Glint module for Glint fun.
        // NOTE: While we can get "location info" by creating a file here, it
        // isn't really useful as it doesn't point to a real file.
        auto& f = context.create_file(
            name,
            std::vector<char>{source.begin(), source.end()}
        );
        auto mod = lcc::glint::Parser::Parse(&context, f);

        // Save diagnostics reported during parsing, and clear the context's
        // diagnostics so that we can separate parse/sema diagnostics.
        auto parse_diagnostics = context.diagnostics();
        context.diagnostics().clear();

        return {std::move(mod), parse_diagnostics};
    };

    auto run() -> bool {
        LCC_ASSERT(not name.empty(), "Refusing to run test with empty name");

        // Parse test source as Glint

        lcc::Context context{
            default_target,
            default_format,
            lcc::Context::Options{
                lcc::Context::DoNotUseColour,
                lcc::Context::DoNotPrintStats,
                lcc::Context::DoNotDiagBacktrace,
                lcc::Context::DoNotPrintAST,
                lcc::Context::DoNotStopatLex,
                lcc::Context::DoNotStopatSyntax,
                lcc::Context::DoNotStopatSema,
                lcc::Context::DoNotPrintMIR,
                lcc::Context::DoNotStopatMIR
            }
        };
        if (option_suppress)
            context.suppress_diagnostics();

        auto parse_info = parse(context);
        bool failed_parse = context.has_error();
        bool warned_parse = warning_reported(context);

        bool do_checking = should_check() and not failed_parse;

        // If we do checking, assume we fail until we don't. If we don't check, we
        // can't fail what we don't do.
        bool failed_check{do_checking};
        bool warned_check{false};
        if (do_checking) {
            // Perform type-checking
            lcc::glint::Sema::Analyse(&context, *parse_info.mod, true);
            failed_check = context.has_error();
            warned_check = warning_reported(context);
        }

        // Only match if test is expected NOT to fail, and hasn't failed parsing
        // or checking (if checking was performed).
        bool do_matching = failure_point == FailurePoint::None
                       and not failed_parse and not failed_check;

        bool ast_matches{true};
        if (do_matching) {
            // Perform AST matching
            auto* root = parse_info.mod->top_level_function()->body();
            ast_matches = perform_match<lcc::glint::Expr>(root, matcher);
        }

        bool do_irgen = not ir.empty()
                    and failure_point == FailurePoint::None
                    and stop_point == StopPoint::None
                    and not failed_parse and not failed_check;

        bool ir_matches{true};
        if (do_irgen) {
            // Perform IR matching
            auto* got_ir = lcc::glint::IRGen::Generate(&context, *parse_info.mod);
            ir_matches = langtest::perform_ir_match(*got_ir, *this);
        }

        bool passed = test_passed(
            failed_parse,
            failed_check,
            warned_parse,
            warned_check,
            ast_matches,
            ir_matches
        );

        // NOTE: Even if we shouldn't print, the parsing/semantic analysis that
        // failed almost certainly printed something of some kind, so we are kind
        // of forced to print something just to delineate what that output came
        // from.
        if (not passed) {
            fmt::print("  {}: {}FAIL{}\n\n", name, C(Colour::Red), C(Colour::Reset));
            if (not ast_matches) {
                std::string expected = matcher.print();
                std::string got = langtest::print_node<lcc::glint::Expr>(parse_info.mod->top_level_function()->body());

                // find_different_from_begin()
                size_t diff_begin{0};
                for (; diff_begin < expected.size() and diff_begin < got.size(); ++diff_begin)
                    if (expected.at(diff_begin) != got.at(diff_begin)) break;

                size_t diff_end{0};
                if (expected.size() and got.size()) {
                    for (; expected.size() - 1 - diff_end and got.size() - 1 - diff_end; ++diff_end) {
                        if (expected.at(expected.size() - 1 - diff_end) != got.at(got.size() - 1 - diff_end))
                            break;
                    }
                }

                // Turn the text back to regular where the expected and got text last
                // differ.
                // We do this first so that we don't invalidate the diff begin offset.
                auto reset_color = C(lcc::utils::Colour::Reset);
                expected.insert(
                    std::max(
                        expected.end() - lcc::isz(diff_end),
                        expected.begin() + lcc::isz(diff_begin) + 1
                    ),
                    reset_color.begin(),
                    reset_color.end()
                );
                // Turn the text green where the expected and got text first differ.
                auto expected_color = C(lcc::utils::Colour::Green);
                expected.insert(
                    expected.begin() + lcc::isz(diff_begin),
                    expected_color.begin(),
                    expected_color.end()
                );

                // Turn the text back to regular where the expected and got text last
                // differ.
                got.insert(
                    std::max(
                        got.end() - lcc::isz(diff_end),
                        got.begin() + lcc::isz(diff_begin) + 1
                    ),
                    reset_color.begin(),
                    reset_color.end()
                );
                // Turn the text red where the expected and got text first differ.
                auto got_color = C(lcc::utils::Colour::Red);
                got.insert(
                    got.begin() + lcc::isz(diff_begin),
                    got_color.begin(),
                    got_color.end()
                );

                fmt::print("EXPECTED: {}\n", expected);
                fmt::print("GOT:      {}\n", got);
            }
        }

        if (option_print and passed) {
            fmt::print("  {}: {}PASS{}\n", name, C(Colour::Green), C(Colour::Reset));
        }

        return passed;
    }
};

void output_ast(std::filesystem::path p) {
    auto contents = lcc::File::Read(p);
    auto source = std::string_view{
        contents.begin(),
        contents.begin() + lcc::isz(contents.size()),
    };

    lcc::Context context{
        default_target,
        default_format,
        lcc::Context::Options{
            lcc::Context::DoNotUseColour,
            lcc::Context::DoNotPrintStats,
            lcc::Context::DoNotDiagBacktrace,
            lcc::Context::DoNotPrintAST,
            lcc::Context::DoNotStopatLex,
            lcc::Context::DoNotStopatSyntax,
            lcc::Context::DoNotStopatSema,
            lcc::Context::DoNotPrintMIR,
            lcc::Context::DoNotStopatMIR //
        } //
    };

    auto mod = lcc::glint::Parser::Parse(&context, source);
    if (context.has_error()) {
        fmt::print(
            stderr,
            "ERROR: Cannot output AST matcher, error encountered during parsing\n"
        );
        return;
    }

    // TODO: Just output un-checked ast, if asked
    {
        auto* root = mod->top_level_function()->body();
        fmt::print(
            "Syntax:\n{}\n",
            langtest::print_node<lcc::glint::Expr>(root)
        );
    }
    // Perform type-checking
    lcc::glint::Sema::Analyse(&context, *mod, true);
    if (context.has_error()) {
        fmt::print(
            stderr,
            "ERROR: Cannot output AST matcher, error encountered during sema\n"
        );
        return;
    }

    {
        auto* root = mod->top_level_function()->body();
        fmt::print("Sema:\n{}\n", langtest::print_node<lcc::glint::Expr>(root));
    }
}

void help() {
    fmt::print(
        "Glint Programming Language Test Runner\n"
        "USAGE: glinttests [FLAGS]\n"
        "FLAGS:\n"
        "  -h, --help  ::  Show this help\n"
        "  -a, --all   ::  Print messages for every test\n"
        "  -c, --count ::  Print counts at the end and for every test file processed\n"
        "  -d, --diags ::  Emit LCC Diagnostics (makes output busy)\n"
        "OPTIONS:\n"
        "  -r, --read <filepath> ::  Output the AST parsed from the given\n"
        "          source file, such that it could be used as the matcher\n"
        "          input for a test\n"
    );
}

void visit_directory(langtest::TestContext& out, std::filesystem::path directory_path, bool option_count) {
    for (const auto& entry : std::filesystem::directory_iterator(directory_path)) {
        if (entry.is_regular_file()) {
            if (option_print or option_count)
                fmt::print("{}:\n", entry.path().lexically_normal().string());

            auto count = langtest::process_ast_test_file<GlintTest>(entry.path());

            if (option_count) {
                fmt::print(
                    "  {}PASSED:  {}/{}{}\n",
                    C(lcc::utils::Colour::Green),
                    count.count_passed(),
                    count.count(),
                    C(lcc::utils::Colour::Reset)
                );
                if (count.count_failed()) {
                    fmt::print(
                        "  {}FAILED:  {}{}\n",
                        C(lcc::utils::Colour::Red),
                        count.count_failed(),
                        C(lcc::utils::Colour::Reset)
                    );
                }
            }

            out.merge(count);
        } else if (entry.is_directory()) {
            visit_directory(out, entry.path(), option_count);
        }
    }
}

int main(int argc, const char** argv) {
    bool option_count{false};
    std::filesystem::path p{""};
    for (int i = 1; i < argc; ++i) {
        std::string_view arg{argv[i]};
        if (arg.starts_with("-h") or arg.starts_with("--h") or arg.starts_with("-?")) {
            help();
            return 0;
        }
        if (arg.starts_with("-a") or arg.starts_with("--all")) {
            option_print = true;
            continue;
        }
        if (arg.starts_with("-c") or arg.starts_with("--count")) {
            option_count = true;
            continue;
        }
        if (arg.starts_with("-d") or arg.starts_with("--diag")) {
            option_suppress = false;
            continue;
        }
        if (arg.starts_with("-r") or arg.starts_with("--read")) {
            if (i + 1 >= argc) {
                fmt::print("ERROR: read option expects a source file input, but no argument was given\n");
                return 1;
            }
            ++i;
            p = argv[i];
            continue;
        }
        if (arg.starts_with("-t") or arg.starts_with(("--target"))) {
            if (i + 1 >= argc) {
                fmt::print("ERROR: target option expects a target input, but no argument was given\n");
                return 1;
            }
            ++i;
            std::string_view target_string{argv[i]};
            // TODO: Exhaustive handling of lcc targets
            if (target_string == "x86_64_linux")
                default_target = lcc::Target::x86_64_linux;
            else if (target_string == "x86_64_windows")
                default_target = lcc::Target::x86_64_windows;
            else {
                lcc::Diag::Fatal(
                    "Invalid argument given to --target: `{}`\n",
                    target_string
                );
            }
            continue;
        }
        if (arg.starts_with("-f") or arg.starts_with(("--format"))) {
            if (i + 1 >= argc) {
                fmt::print("ERROR: format option expects a format input, but no argument was given\n");
                return 1;
            }
            ++i;
            std::string_view format_string{argv[i]};
            // TODO: Exhaustive handling of lcc output formats
            if (format_string == "gnu-as-att")
                default_format = lcc::Format::gnu_as_att_assembly;
            else if (format_string == "elf")
                default_format = lcc::Format::elf_object;
            else if (format_string == "coff")
                default_format = lcc::Format::coff_object;
            else if (format_string == "ssa_ir")
                default_format = lcc::Format::lcc_ssa_ir;
            else if (format_string == "ir")
                default_format = lcc::Format::lcc_ir;
            else if (format_string == "llvm")
                default_format = lcc::Format::llvm_textual_ir;
            else if (format_string == "wat")
                default_format = lcc::Format::wasm_textual;
            else {
                lcc::Diag::Fatal(
                    "Invalid argument given to --format: `{}`\n",
                    format_string
                );
            }
            continue;
        }
        lcc::Diag::Fatal(
            "Unhandled command line option `{}'.\n"
            "Use -h for more info.",
            arg
        );
    }

    if (not p.empty()) {
        if (not std::filesystem::exists(p)) {
            fmt::print(
                "ERROR: read option given a source file input that does not correspond to an existing file!\n"
            );
            return 1;
        }
        output_ast(p);
        return 0;
    }

    langtest::TestContext out{};

    visit_directory(out, "corpus", option_count);

    // Print stats if CLI options request it or if all tests did not pass.
    if (option_print or out.count_passed() != out.count()) {
        fmt::print(
            "STATS:\n"
            "  TESTS:   {}\n"
            "  {}PASSED:  {}{}\n"
            "  {}FAILED:  {}{}\n",
            out.count(),
            C(lcc::utils::Colour::Green),
            out.count_passed(),
            C(lcc::utils::Colour::Reset),
            C(lcc::utils::Colour::Red),
            out.count_failed(),
            C(lcc::utils::Colour::Reset)
        );
    } else {
        fmt::print(
            "~~~~~~~~~~~~~~~~~~~~~~~~\n"
            "{}    ALL TESTS PASSED: {}/{}{}\n"
            "~~~~~~~~~~~~~~~~~~~~~~~~\n",
            C(lcc::utils::Colour::Green),
            out.count(),
            out.count(),
            C(lcc::utils::Colour::Reset)
        );
    }

    return 0;
}
