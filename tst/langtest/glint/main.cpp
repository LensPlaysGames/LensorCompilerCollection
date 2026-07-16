#include <langtest/langtest.hh>

#include <glint/ast.hh>
#include <glint/ir_gen.hh>
#include <glint/parser.hh>
#include <glint/sema.hh>

#include <lcc/defaults.hh>
#include <lcc/format.hh>
#include <lcc/ir/core.hh>
#include <lcc/ir/module.hh>
#include <lcc/target.hh>
#include <lcc/utils/colours.hh>

#include <lccjson/lccjson.hh>

#include <lccbase/context.hh>

#include <algorithm>
#include <filesystem>
#include <memory>
#include <string>
#include <string_view>
#include <utility>
#include <vector>

static lcc::Colours C{true};
using lcc::Colour;

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
        LCC_ASSERT(
            not name.empty(),
            "Refusing to run test with empty name"
        );

        // Parse test source as Glint

        auto context = lcc::default_context();
        if (option_suppress)
            context.suppress_diagnostics();

        auto parse_info = parse(context);
        bool failed_parse = context.has_error();
        bool warned_parse = lcc::rgs::any_of(
            parse_info.diagnostics,
            [](const lcc::Context::DiagnosticReport d) {
                return d.kind == lcc::Diag::Kind::Warning;
            }
        );

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
                std::string got = langtest::print_node<lcc::glint::Expr>(
                    parse_info.mod->top_level_function()->body()
                );

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
                auto reset_color = C(lcc::Colour::Reset);
                expected.insert(
                    std::max(
                        expected.end() - lcc::isz(diff_end),
                        expected.begin() + lcc::isz(diff_begin) + 1
                    ),
                    reset_color.begin(),
                    reset_color.end()
                );
                // Turn the text green where the expected and got text first differ.
                auto expected_color = C(lcc::Colour::Green);
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
                auto got_color = C(lcc::Colour::Red);
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

    auto context = lcc::default_context();

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
                    C(lcc::Colour::Green),
                    count.count_passed(),
                    count.count(),
                    C(lcc::Colour::Reset)
                );
                if (count.count_failed()) {
                    fmt::print(
                        "  {}FAILED:  {}{}\n",
                        C(lcc::Colour::Red),
                        count.count_failed(),
                        C(lcc::Colour::Reset)
                    );
                }
            }

            out.merge(count);
        } else if (entry.is_directory()) {
            visit_directory(out, entry.path(), option_count);
        }
    }
}

void emit_sarif_file(
    const langtest::TestContext& results,
    std::filesystem::path outpath,
    std::string_view command_line = ""
) {
    JSONObject sarif{};
    sarif.add_property(
        "$schema",
        "https://docs.oasis-open.org/sarif/sarif/v2.1.0/errata01/os/schemas/sarif-schema-2.1.0.json"
    );
    sarif.add_property("version", "2.1.0");
    JSONArray runs{};
    JSONObject run{};

    JSONObject tool{};
    JSONObject driver{};
    driver.add_property("name", "lcc.langtest.glint");
    driver.add_property("semanticVersion", "0.420.69");
    driver.add_property(
        "informationUri",
        "https://codeberg.org/LensPlaysGames/LensorCompilerCollection"
    );
    driver.add_property("rules", JSONArray{});
    tool.add_property("driver", std::move(driver));
    run.add_property("tool", std::move(tool));

    JSONObject baseIds{};
    JSONObject pwd{};
    const auto pwd_path = std::filesystem::absolute(std::filesystem::current_path());
    pwd.add_property(
        "uri",
        fmt::format(
            "file://{}/",
            pwd_path.string()
        )
    );
    baseIds.add_property("PWD", std::move(pwd));
    run.add_property("originalUriBaseIds", std::move(baseIds));

    // run artifacts
    JSONArray artifacts{};
    run.add_property("artifacts", std::move(artifacts));

    // SARIF run object invocations array property
    JSONArray invocations{};

    // SARIF invocation object
    JSONObject invocation{};
    // Add "commandLine" property
    invocation.add_property(
        "commandLine",
        std::string(command_line)
    );
    invocations.add_element(std::move(invocation));
    run.add_property("invocations", std::move(invocations));

    // run results
    JSONArray results_out{};

    for (auto& test_name : results.completed_tests()) {
        JSONObject result{};
        result.add_property("ruleId", "LangTest.Glint");
        // Record pass vs fail.
        if (results.test_passes(test_name)) {
            result.add_property("kind", "pass");
        } else {
            result.add_property("level", "error");
        }

        JSONObject message{};
        message.add_property("text", std::string(test_name));
        result.add_property("message", std::move(message));

        results_out.add_element(std::move(result));
    }

    run.add_property("results", std::move(results_out));

    runs.add_element(std::move(run));
    sarif.add_property("runs", std::move(runs));

    // Try to write it, if it doesn't happen it's not the end of the world.
    auto sarif_data = sarif.emit();
    (void) lcc::File::Write(sarif_data.data(), sarif_data.size(), outpath);
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
                lcc::default_target = lcc::Target::x86_64_linux;
            else if (target_string == "x86_64_windows")
                lcc::default_target = lcc::Target::x86_64_windows;
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
                lcc::default_format = lcc::Format::gnu_as_att_assembly;
            else if (format_string == "elf")
                lcc::default_format = lcc::Format::elf_object;
            else if (format_string == "coff")
                lcc::default_format = lcc::Format::coff_object;
            else if (format_string == "ssa_ir")
                lcc::default_format = lcc::Format::lcc_ssa_ir;
            else if (format_string == "ir")
                lcc::default_format = lcc::Format::lcc_ir;
            else if (format_string == "llvm")
                lcc::default_format = lcc::Format::llvm_textual_ir;
            else if (format_string == "wat")
                lcc::default_format = lcc::Format::wasm_textual;
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

    // Emit Results in a SARIF file
    std::string command_line{argv[0]};
    for (auto i = 1; i < argc; ++i) {
        command_line += ' ';
        command_line += argv[i];
    }
    emit_sarif_file(out, "langtest.sarif", command_line);

    // Print stats if CLI options request it or if all tests did not pass.
    if (option_print or out.count_passed() != out.count()) {
        fmt::print(
            "STATS:\n"
            "  TESTS:   {}\n"
            "  {}PASSED:  {}{}\n"
            "  {}FAILED:  {}{}\n",
            out.count(),
            C(lcc::Colour::Green),
            out.count_passed(),
            C(lcc::Colour::Reset),
            C(lcc::Colour::Red),
            out.count_failed(),
            C(lcc::Colour::Reset)
        );
    } else {
        fmt::print(
            "~~~~~~~~~~~~~~~~~~~~~~~~\n"
            "{}    ALL TESTS PASSED: {}/{}{}\n"
            "~~~~~~~~~~~~~~~~~~~~~~~~\n",
            C(lcc::Colour::Green),
            out.count(),
            out.count(),
            C(lcc::Colour::Reset)
        );
    }

#ifdef LCC_TEST_NON_ZERO_EXIT_ON_FAILURE
    if (out.count_failed())
        return 1;
#endif
    return 0;
}
