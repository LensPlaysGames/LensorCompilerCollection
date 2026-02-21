#include "lcc/opt/opt.hh"
#include <cctype>
#include <fmt/format.h>

#include <iterator>
#include <langtest/langtest.hh>

#include <lcc/context.hh>
#include <lcc/core.hh>
#include <lcc/file.hh>
#include <lcc/format.hh>
#include <lcc/ir/core.hh>
#include <lcc/ir/module.hh>
#include <lcc/target.hh>
#include <lcc/utils.hh>

#include <filesystem>

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

static lcc::utils::Colours C{true};

struct TestNameAndResult {
    std::string_view name{};
    bool passed{true};
};

struct TestContext {
    lcc::Context& context;
    std::vector<TestNameAndResult> results{};

    bool option_per_directory_count{true};
};

struct IRTest {
    std::string_view name;
    std::string_view input;
    std::string_view expected;
    int optimise{};
};

auto collect_tests_from_file(
    TestContext& out,
    std::filesystem::path test_file
) -> std::vector<IRTest> {
    std::vector<IRTest> tests{};

    auto file_contents = lcc::File::Read(test_file);
    auto& f = out.context.create_file(test_file, std::move(file_contents));
    auto contents = f.begin();
    unsigned int offset = 0;
    lcc::Location location{};
    while (offset < f.size()) {
        location.pos = offset;
        location.len = 1;
        switch (contents[offset]) {
            default: {
                location.pos = offset;
                lcc::Diag::Error(
                    &out.context,
                    location,
                    "Invalid input in IR test"
                );
                // TODO: Synchronize, if possible (EOF or next line that starts with '=')
                return {};
            }

            case '=': {
                IRTest test{};

                // Until newline
                while (offset < f.size() and contents[++offset] != '\n');
                // Skip newline
                ++offset;

                // Test name begins here
                auto test_name_begin = offset;

                // Until newline
                while (offset < f.size() and contents[++offset] != '\n');
                auto test_name_end = offset;

                test.name = {
                    contents + test_name_begin,
                    contents + test_name_end
                };

                if (test.name.empty()) {
                    location.pos = offset;
                    lcc::Diag::Error(
                        &out.context,
                        location,
                        "Expected test to be given a name"
                    );
                    // TODO: Synchronize
                    return {};
                }

                // Skip newline
                ++offset;

                // Specifiers
                // `:<specifier>` following name line before name closing line of '='
                while (contents[offset] == ':') {
                    // Eat `:`
                    ++offset;
                    auto specifier_begin = offset;
                    location.pos = specifier_begin;
                    // Until newline
                    while (offset < f.size() and contents[++offset] != '\n');
                    auto specifier_end = offset;
                    // Skip newline
                    ++offset;
                    std::string_view specifier{
                        contents + specifier_begin,
                        contents + specifier_end
                    };
                    if (specifier.starts_with("optimise ")) {
                        specifier.remove_prefix(
                            std::string_view{"optimise "}.length()
                        );
                        if (specifier.empty() or not isdigit(specifier.at(0))) {
                            lcc::Diag::Error(
                                &out.context,
                                location,
                                "Invalid optimisation level given in specifier for test `{}`",
                                test.name
                            );
                            return {};
                        }
                        int opt_level = specifier.at(0) - '0';
                        if (opt_level < -1 or opt_level > 3) {
                            lcc::Diag::Error(
                                &out.context,
                                location,
                                "Invalid optimisation level {} given in specifier for test `{}`",
                                opt_level,
                                test.name
                            );
                            return {};
                        }
                        test.optimise = opt_level;
                    } else {
                        lcc::Diag::Error(
                            &out.context,
                            location,
                            "Unrecognized test specifier: `{}`",
                            specifier
                        );
                    }
                }

                if (contents[offset] != '=') {
                    location.pos = offset;
                    lcc::Diag::Error(
                        &out.context,
                        location,
                        "Expected line of '=' to close test name of test {}",
                        test.name
                    );
                    // TODO: Synchronize
                    return {};
                }

                // Until newline
                while (offset < f.size() and contents[++offset] != '\n');
                // Skip newline
                ++offset;

                auto test_ir_input_begin = offset;

                // until EOF or line that starts with `-`...
                {
                    bool at_bol{true};
                    while (offset < f.size()) {
                        if (at_bol and contents[offset] == '-')
                            break;

                        at_bol = contents[offset] == '\n';

                        ++offset;
                    }
                }
                auto test_ir_input_end = offset;

                test.input = {
                    contents + test_ir_input_begin,
                    contents + test_ir_input_end
                };

                if (contents[offset] != '-') {
                    location.pos = offset;
                    lcc::Diag::Error(
                        &out.context,
                        location,
                        "Expected beginning of matcher (line of `-`) following test input of test {}",
                        test.name
                    );
                    // TODO: Synchronize
                    return {};
                }

                // Until newline
                while (offset < f.size() and contents[++offset] != '\n');
                // Skip newline
                ++offset;

                // until EOF or line that starts with `=` (another test)
                auto test_ir_expected_begin = offset;
                {
                    bool at_bol{true};
                    while (offset < f.size()) {
                        if (at_bol and contents[offset] == '=')
                            break;

                        at_bol = contents[offset] == '\n';

                        ++offset;
                    }
                }
                auto test_ir_expected_end = offset;

                test.expected = {
                    contents + test_ir_expected_begin,
                    contents + test_ir_expected_end
                };

                tests.emplace_back(test);

            } break;
        }
    }

    return tests;
}

[[nodiscard]]
auto print_test_passedfailed(const TestNameAndResult& result) -> std::string {
    return fmt::format(
        "  {} {}: {}\n",
        fmt::format(
            "{}{}{}",
            result.passed ? C(lcc::utils::Colour::BoldGreen) : C(lcc::utils::Colour::BoldRed),
            result.passed ? 'O' : 'X',
            C(lcc::utils::Colour::Reset)
        ),
        result.name,
        result.passed ? "PASSED" : "FAILED"
    );
}

[[nodiscard]]
auto print_passedfailed(const std::vector<TestNameAndResult>& results) -> std::string {
    std::string out{};

    unsigned int count_passed{};
    unsigned int count_failed{};
    for (auto result : results) {
        if (result.passed)
            ++count_passed;
        else ++count_failed;
    }
    fmt::format_to(
        std::back_inserter(out),
        "  {}PASSED:  {}/{}{}\n",
        C(lcc::utils::Colour::Green),
        count_passed,
        results.size(),
        C(lcc::utils::Colour::Reset)
    );
    if (count_failed) {
        fmt::format_to(
            std::back_inserter(out),
            "  {}FAILED:  {}{}\n",
            C(lcc::utils::Colour::Red),
            count_failed,
            C(lcc::utils::Colour::Reset)
        );
    }

    return out;
}

void visit_directory(
    TestContext& out,
    std::filesystem::path directory_path
) {
    for (const auto& entry : std::filesystem::directory_iterator(directory_path)) {
        if (entry.is_directory())
            visit_directory(out, entry.path());
        if (not entry.is_regular_file())
            continue;

        if (out.option_per_directory_count)
            fmt::print("{}:\n", entry.path().lexically_normal().string());

        auto tests = collect_tests_from_file(out, entry.path());

        std::vector<TestNameAndResult> results{};
        for (auto t : tests) {
            const auto testpassfail = [&](std::string_view name, bool passed) {
                results.emplace_back(name, passed);

                if (not out.option_per_directory_count)
                    return;

                fmt::print("{}", print_test_passedfailed(results.back()));
            };

            auto& got_f = out.context.create_file(fmt::format("got.{}", t.name), lcc::utils::to_vec(t.input));
            auto got = lcc::Module::Parse(&out.context, got_f);
            if (not got) {
                testpassfail(t.name, false);
                continue;
            }

            if (t.optimise)
                lcc::opt::Optimise(got.get(), t.optimise);

            auto& expected_f = out.context.create_file(fmt::format("expected.{}", t.name), lcc::utils::to_vec(t.expected));
            auto expected = lcc::Module::Parse(&out.context, expected_f);
            if (not expected) {
                lcc::Diag::Error("Test `{}` has malformed expected IR", t.name);
                continue;
            }

            bool passed = langtest::perform_ir_match(*got, *expected);
            testpassfail(t.name, passed);
        }

        if (out.option_per_directory_count)
            fmt::print("{}", print_passedfailed(results));

        out.results.insert(out.results.end(), results.begin(), results.end());
    }
}

int main(int argc, char** argv) {
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
    TestContext test_context{context};
    visit_directory(test_context, "corpus");

    fmt::print(
        "\nFINAL REPORT:\n{}",
        print_passedfailed(test_context.results)
    );

    for (auto result : test_context.results) {
        if (result.passed)
            continue;
        fmt::print("{}", print_test_passedfailed(result));
    }

    return 0;
}
