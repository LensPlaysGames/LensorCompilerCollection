#include <lcc/context.hh>
#include <lcc/format.hh>
#include <lcc/ir/core.hh>
#include <lcc/ir/module.hh>
#include <lcc/target.hh>
#include <lcc/utils.hh>

#include <fmt/format.h>

#include <algorithm>
#include <cctype>
#include <concepts>
#include <filesystem>
#include <span>
#include <string>
#include <string_view>
#include <type_traits>
#include <vector>

// TODO: Would be cool to have >= syntax to start a "continuation" test
// possible instead of `---` and the expected AST. Basically, allow for
// the language to assert that multiple tests have the same output.

// TODO: Would be cool to have a distinction between checked vs unchecked
// ASTs, that way the language could assert that an expression is
// transformed in a certain way after semantic analysis.

namespace langtest {

struct MatchTree {
    std::string_view name{};
    std::vector<MatchTree> children{};

    [[nodiscard]]
    auto print() -> std::string {
        std::string out{};
        out += fmt::format("({}", name);
        for (auto child : children) {
            out += ' ';
            out += child.print();
        }
        out += ')';
        return out;
    }
};

struct Test {
    std::string_view name;
    std::string_view source;
    std::string_view ir;
    MatchTree matcher;

    enum class StopPoint {
        None,
        Syntax
    } stop_point{StopPoint::None};

    enum class FailurePoint {
        None,
        Syntax,
        Sema
    } failure_point{FailurePoint::None};

    enum class WarningPoint {
        None,
        Syntax,
        Sema
    } warning_point{WarningPoint::None};

    // NOTE: You should also not check (analyse semantically) if context has
    // an error after parsing.
    bool should_check() {
        switch (stop_point) {
            case StopPoint::Syntax: return false;
            case StopPoint::None: break;
        }

        switch (failure_point) {
            case FailurePoint::Syntax:
                return false;

            case FailurePoint::None:
            case FailurePoint::Sema:
                break;
        }

        return true;
    }

    bool test_passed(
        bool failed_parse,
        bool failed_check,
        bool warned_parse,
        bool warned_check,
        bool matched_ast,
        bool matched_ir
    ) {
        // Now we just have to determine if the test processed above actually
        // passed. To do this, we will go through all possible fail cases, and
        // return false in those cases, since the test failed. If no failure case
        // returns false, then the test must have passed, and it will return true.

        switch (warning_point) {
            case langtest::Test::WarningPoint::Syntax:
                if (not warned_parse) {
                    fmt::print("Expected warning from parser, but one was not emitted.\n");
                    return false;
                }
                break;

            case langtest::Test::WarningPoint::Sema:
                if (not warned_check) {
                    fmt::print("Expected warning from sema, but one was not emitted.\n");
                    return false;
                }
                break;

            // No warning point means no warnings are expected.
            case langtest::Test::WarningPoint::None:
                if (warned_parse or warned_check) {
                    fmt::print("Expected no warnings, but warnings were emitted.\n");
                    return false;
                }
                break;
        }

        switch (failure_point) {
            case langtest::Test::FailurePoint::Syntax:
                if (not failed_parse) {
                    fmt::print("Expected parse failure, but parsing succeeded.\n");
                    return false;
                }
                break;

            // Failure Point at sema means parsing is expected to succeed.
            case langtest::Test::FailurePoint::Sema:
                if (failed_parse) {
                    fmt::print("Expected sema failure, which implies parsing success, but parsing failed.\n");
                    return false;
                }
                if (not failed_check) {
                    fmt::print("Expected sema failure, but sema succeeded.\n");
                    return false;
                }
                break;

            case langtest::Test::FailurePoint::None:
                // No failure point means parsing and checking is expected to succeed.
                if (failed_parse) {
                    fmt::print("Expected no failures, but parsing failed.\n");
                    return false;
                }
                if (failed_check) {
                    fmt::print("Expected no failures, but sema failed.\n");
                    return false;
                }
                // No failure point means matching is performed, and expected to succeed.
                if (not matched_ast) {
                    fmt::print("Expected AST to match, but AST did not match.\n");
                    return false;
                }
                if (not matched_ir) {
                    fmt::print("Expected IR to match, but IR did not match.\n");
                    return false;
                }
                break;
        }

        // Stop point mostly has to do with how a test is processed, rather than
        // affecting how a test passes.
        switch (stop_point) {
            case langtest::Test::StopPoint::Syntax:
            case langtest::Test::StopPoint::None:
                break;
        }

        return true;
    }
};

class TestContext {
    size_t _count;
    size_t _count_failed;

public:
    [[nodiscard]]
    auto count() const -> size_t { return _count; }
    [[nodiscard]]
    auto count_failed() const -> size_t { return _count_failed; }
    [[nodiscard]]
    auto count_passed() const -> size_t { return _count - count_failed(); }

    void merge(const TestContext& other) {
        _count += other._count;
        _count_failed += other._count_failed;
    }

    void record_test(bool passed) {
        ++_count;
        if (not passed) ++_count_failed;
    }
};

template <typename T>
concept langtest_node_has_name = requires (T node) {
    node.langtest_name();
    requires std::convertible_to<decltype(node.langtest_name()), std::string>;
};
template <typename T>
concept langtest_node_has_children = requires (T node) {
    node.langtest_children();
    requires std::convertible_to<decltype(node.langtest_children()), std::vector<T*>>;
};
template <typename T>
concept langtest_node_requirements
    = langtest_node_has_name<T> and langtest_node_has_children<T>;

/// NOTE: print_node() constructs a matcher from a given AST; may be good to
/// utilise this functionality to create initial expected output of test.
template <typename TNode>
requires langtest_node_requirements<TNode>
[[nodiscard]]
auto print_node(TNode* e) -> std::string {
    std::string out{};
    out += fmt::format("({}", e->langtest_name());
    auto children = e->langtest_children();
    for (auto* child : children) {
        out += ' ';
        out += print_node<TNode>(child);
    }
    out += ')';
    return out;
}

template <typename TNode>
requires langtest_node_requirements<TNode>
[[nodiscard]]
auto perform_match(TNode* e, MatchTree& t) -> bool {
    auto name = e->langtest_name();
    if (name != t.name) {
        // TODO: Record test failure, somewhere/somehow
        fmt::print("\nMISMATCH: node name\n");
        fmt::print("Expected {} but got {}\n", t.name, name);
        return false;
    }

    auto children = e->langtest_children();
    if (children.size() != t.children.size()) {
        fmt::print("\nMISMATCH: child count\n");
        return false;
    }

    LCC_ASSERT(
        children.size() == t.children.size(),
        "Child count of expression and matcher must be the same, otherwise the match mustn't be performed."
    );

    bool children_match{true};
    for (size_t i = 0; i < children.size(); ++i) {
        if (not perform_match<TNode>(children.at(i), t.children.at(i)))
            children_match = false;
    }

    return children_match;
}

bool perform_ir_match_block(
    lcc::Module& got,
    lcc::Module& expected,
    lcc::Block* got_block,
    lcc::Block* expected_block,
    std::unordered_map<lcc::Inst*, lcc::Inst*>& expected_to_got
) {
    lcc::utils::Colours C{
        got.context() and got.context()->option_use_colour()
    };

    // Disallow comparing from/to NULL pointer.
    LCC_ASSERT(
        got_block and expected_block,
        "Cannot perform IR match on NULL IR block"
    );

    // Two blocks without the same number of instructions within them are
    // never equivalent.
    if (expected_block->instructions().size() != got_block->instructions().size()) {
        fmt::print(
            "IR MISMATCH: Instruction count in block {} in function {}\n",
            expected_block->name(),
            expected_block->function()->names().at(0).name
        );

        return false;
    }

    for (size_t inst_i = 0; inst_i < expected_block->instructions().size(); ++inst_i) {
        auto* expected_inst = expected_block->instructions().at(inst_i);
        auto* got_inst = got_block->instructions().at(inst_i);
        expected_to_got[expected_inst] = got_inst;

        if (expected_inst->kind() != got_inst->kind()) {
            // TODO: Maybe have this behind a "--verbose-ir" CLI flag or something
            // fmt::print("\nExpected IR:\n");
            // expected_block->function()->print();
            // fmt::print("Got IR:\n");
            // got_func->print();

            fmt::print(
                "IR MISMATCH: Expected instruction (1) but got instruction (2) in block {} in function {}\n"
                "(1): {}"
                "(2): {}",
                expected_block->name(),
                expected_block->function()->names().at(0).name,
                expected_inst->string(),
                got_inst->string()
            );

            return false;
        }

        // Compare instruction children and ensure they point to equivalent
        // places (i.e. got_inst->child[0] == expected_inst->child[0], but not
        // literally equals, just equals the one that represents. Basically, we
        // will need a map of expected_inst to got_inst so we can do this
        // comparison properly).
        auto expected_children = expected_inst->children();
        auto got_children = got_inst->children();

        size_t child_i = 0;
        while (
            expected_children.begin() != expected_children.end()
            and got_children.begin() != got_children.end()
        ) {
            auto* expected_child = *expected_children.begin();
            auto* got_child = *got_children.begin();

            // If the expected child is an instruction, that means we can look it up
            // in our "expected instruction to got instruction" cache.
            auto* expected_child_inst = lcc::cast<lcc::Inst>(expected_child);
            if (expected_child_inst) {
                // If the expected child instruction does not map to _any_ actual child
                // instruction we got, that is an error in how we are traversing the IR
                // and building our cache.
                LCC_ASSERT(
                    expected_to_got.contains(expected_child_inst),
                    "Expected->Got cache not built correctly"
                );

                // If the expected child instruction does not "map" to the actual child
                // instruction we got, then these IRs do not match.
                if (expected_to_got[expected_child_inst] != got_child) {
                    fmt::print(
                        "IR MISMATCH: Expected operand {} (zero-based) of instruction (1) to reference (2), but it instead references (3)\n"
                        "(1): {}"
                        "(2): {}"
                        "(3): {}",
                        child_i,
                        got_inst->string(),
                        expected_child_inst->string(),
                        expected_to_got[expected_child_inst]->string()
                    );

                    return false;
                }
            }
            // Advance iterators
            ++expected_children.begin();
            ++got_children.begin();
            ++child_i;
        }
    }

    return true;
}

bool perform_ir_match_function(lcc::Module& got, lcc::Module& expected, lcc::Function* expected_function) {
    LCC_ASSERT(
        expected_function,
        "Cannot perform IR match on NULL expected IR function"
    );
    LCC_ASSERT(
        expected_function->names().size(),
        "Expected IR function must have at least one name"
    );

    // We want to find the function in the IR generated by the test by name;
    // that is, we want to find it if any of it's names match the expected
    // function's name (that we parsed from the expected test output).
    auto got_func_in_ir
        = got.function_by_one_of_names(expected_function->names());
    if (not got_func_in_ir) {
        fmt::print(
            "IR MISMATCH: Expected function {} to be in IR, but didn't find it\n"
            "{}",
            expected_function->names().at(0).name,
            got.as_lcc_ir(true)
        );

        return false;
    }
    auto* got_func = *got_func_in_ir;

    if (expected_function->blocks().size() != got_func->blocks().size()) {
        fmt::print(
            "IR MISMATCH: Block count in function {}\n",
            expected_function->names().at(0).name
        );

        return false;
    }

    // Cache of expected instructions mapped to the instruction we actually
    // got in the output.
    //
    // Used to resolve lookups to make sure references are "equivalent".
    //
    // EXPECTED:
    //   %0 = add i32 13, 14
    //   %1 = add i32 %0, 14
    // GOT:
    //   %a = add i32 13, 14
    //   %b = add i32 %a, 14
    // expected_to_got: [ (%0, %a), (%1, %b) ]
    //
    // That way, when we get to %b in the "got" and %1 in "expected", we can
    // lookup the expected first operand, %0, and know that we should expect
    // the first operand to /actually/ be %a, from the output.
    //
    // This needs to be per-function and not per-block because of PHI
    // instructions.
    std::unordered_map<lcc::Inst*, lcc::Inst*> expected_to_got{};

    for (size_t block_i = 0; block_i < expected_function->blocks().size(); ++block_i) {
        auto* expected_block = expected_function->blocks().at(block_i);
        auto* got_block = got_func->blocks().at(block_i);
        if (
            not perform_ir_match_block(
                got,
                expected,
                got_block,
                expected_block,
                expected_to_got
            )
        ) return false;
    }

    return true;
}

// @return true if modules are "equivalent". That is, they contain the
// same functions, blocks, and instructions in the same order.
[[nodiscard]]
bool perform_ir_match(lcc::Module& got, lcc::Module& expected) {
    // For every function in the expected IR, check that the function also
    // exists in the IR we got.
    bool functions_match{true};
    for (auto* expected_function : expected.code()) {
        if (
            not perform_ir_match_function(
                got,
                expected,
                expected_function
            )
        ) functions_match = false;
    }

    return functions_match;
}

// @return true either if the test has no IR matcher, or if the test does have an IR matcher and it matches.
[[nodiscard]]
bool perform_ir_match(lcc::Module& got, langtest::Test& test) {
    if (test.ir.empty())
        return true;

    LCC_ASSERT(got.context(), "NULL context...");

    // Parse expected IRGen IR
    // fmt::print("EXPECTED IR SPAN:\n{}\n", ir);
    std::vector<char> ir_v = {test.ir.begin(), test.ir.end()};
    auto& ir_f = got.context()->create_file("ir_source.lcc", std::move(ir_v));
    auto expected = lcc::Module::Parse(got.context(), ir_f);
    if (not expected) {
        fmt::print("Error parsing expected IR for test {}\n", test.name);
        return false;
    }

    return perform_ir_match(got, *expected);
}

void parse_matchtree(
    std::span<char> contents,
    const size_t fsize,
    size_t& i,
    MatchTree& match
) {
    // Get to beginning of list
    while (i < fsize and contents[i] != '(' and contents[i] != '=')
        ++i;

    if (i >= fsize or contents[i] == '=') {
        // TODO: file location
        fmt::print("ERROR parse_matchtree was called but the test expects nothing (no matcher)\n");
        return;
    }

    // Eat list opening symbol
    ++i;

    // Eat whitespace
    while (i < fsize and isspace(contents[i])) ++i;

    if (i >= fsize or contents[i] == '=') {
        // TODO: file location near list opening symbol
        fmt::print("ERROR expected list closing symbol but got end of input\n");
        return;
    }

    size_t begin_match{i};

    while (
        i < fsize
        and not isspace(contents[i])
        and not std::string_view("()=-").contains(contents[i])
    ) ++i;

    match.name = {
        contents.begin() + lcc::isz(begin_match),
        contents.begin() + lcc::isz(i)
    };

    // Eat whitespace
    while (i < fsize and isspace(contents[i])) ++i;
    if (i >= fsize or contents[i] == '=') {
        fmt::print("ERROR expected list closing symbol but got end of input\n");
        return;
    }

    while (contents[i] != ')') {
        // Parse match children, if necessary
        match.children.push_back({});
        parse_matchtree(contents, fsize, i, match.children.back());

        // Skip whitespace
        while (i < fsize and isspace(contents[i])) ++i;
        if (i >= fsize or contents[i] == '=') {
            fmt::print("ERROR expected list closing symbol but got end of input\n");
            return;
        }
    }
    // Eat list closing symbol
    ++i;
}

/// @param[out] test
auto parse_test(
    std::span<char> contents,
    const size_t fsize,
    size_t& i,
    Test& test
) -> bool {
    auto ToBeginningOfNextTest = [&]() {
        bool bol{true};
        while (i < fsize and not (bol and contents[i] == '=')) {
            bol = contents[i] == '\n';
            ++i;
        }
    };

    auto ToNewline = [&]() {
        // Eat everything until '\n'
        while (i < fsize and contents[i] != '\n')
            ++i;
    };

    // Move 'i' to the offset of the beginning of the next non-empty line
    auto ToBeginningOfNextLine = [&]() {
        ToNewline();
        // Eat all consecutive '\n'
        while (i < fsize and contents[i] == '\n')
            ++i;
    };

    auto ToBeginningOfNextTestSegment = [&]() {
        bool bol{true};
        while (
            i < fsize
            and not (bol and contents[i] == '-')
            and not (bol and contents[i] == '=')
        ) {
            bol = contents[i] == '\n';
            ++i;
        }
    };

    { // Parse test name
        ToBeginningOfNextLine();
        if (i >= fsize) {
            fmt::print("ERROR parsing first line of test\n");
            return false;
        }

        // Parse test name
        size_t begin{i};
        ToNewline();
        if (i >= fsize) {
            fmt::print("ERROR parsing name of test\n");
            return false;
        }

        test.name = {
            contents.begin() + lcc::isz(begin),
            contents.begin() + lcc::isz(i)
        };

        // Eat '\n'
        ToBeginningOfNextLine();

        // Handle lines beginning with `:` following name before ending `=` line
        // (test specifiers).
        while (contents[i] == ':') {
            // eat ':'
            ++i;

            size_t specifier_begin{i};
            ToNewline();

            auto specifier = std::string_view{
                contents.begin() + lcc::isz(specifier_begin),
                contents.begin() + lcc::isz(i)
            };

            // Eat newlines
            ToBeginningOfNextLine();

            if (specifier.starts_with("desc")) {
                // do nothing (test description)
            } else if (specifier == "only_parse" or specifier == "syntax") {
                test.stop_point = Test::StopPoint::Syntax;
            } else if (
                specifier == "warn_parse" or specifier == "warn_syntax"
                or specifier == "parse_warn" or specifier == "syntax_warn"
            ) {
                test.warning_point = Test::WarningPoint::Syntax;
            } else if (
                specifier == "fail_parse"
                or specifier == "error_parse" or specifier == "parse_error"
            ) {
                test.failure_point = Test::FailurePoint::Syntax;
            } else if (
                specifier == "warn_sema" or specifier == "warn_check"
                or specifier == "sema_warn" or specifier == "check_warn"
            ) {
                test.warning_point = Test::WarningPoint::Sema;
            } else if (
                specifier == "fail_sema" or specifier == "fail_check"
                or specifier == "error_sema" or specifier == "sema_error"
            ) {
                test.failure_point = Test::FailurePoint::Sema;
            } else {
                fmt::print("ERROR parsing test specifiers for test {}\n", test.name);
                return false;
            }
        }

        if (contents[i] != '=') {
            fmt::print("ERROR parsing closing name line of test {}. It should be all `=` characters\n", test.name);
            return false;
        }

        ToBeginningOfNextLine();
        if (i >= fsize) {
            fmt::print("ERROR parsing closing line of name of test {}\n", test.name);
            return false;
        }
    }

    { // Parse test source
        // Record beginning of source
        size_t begin_source{i};
        ToBeginningOfNextTestSegment();
        test.source = {
            contents.begin() + lcc::isz(begin_source),
            contents.begin() + lcc::isz(i)
        };

        // Beginning of next test or end of input...
        if (i >= fsize or contents[i] == '=') {
            // Error if the test is not expected to fail (nothing to match if it is specified to fail)
            if (test.failure_point == Test::FailurePoint::None) {
                fmt::print(
                    "ERROR test has no matcher declared but it is not specified to fail... {}\n",
                    test.name
                );
                return false;
            }
            return true;
        }
    }

    { // Parse test expected AST
        // Skip until start of expectation
        while (i < fsize and contents[i] != '(')
            ++i;
        if (i >= fsize) {
            // Got EOF when expected `(` (beginning of expected test output after `---`)
            fmt::print("ERROR parsing expected of test {}\n", test.name);
            return false;
        }

        parse_matchtree(contents, fsize, i, test.matcher);

        ToBeginningOfNextTestSegment();
    }

    // Beginning of next test or end of input...
    if (i >= fsize or contents[i] == '=')
        return true;

    // `---` following the expected AST matcher means there may be expected
    // LCC IR.
    LCC_ASSERT(
        contents[i] == '-',
        "LangTest parser must leave parsing index at EOF, start of next test ('='), or start of next test segment ('-'). Instead, it was left at '{}'",
        contents[i]
    );

    { // Parse test expected IR
        // NOTE: Optional

        // Skip `---` line
        ToBeginningOfNextLine();
        if (i >= fsize) {
            fmt::print("ERROR parsing expected IR of test {}\n", test.name);
            return false;
        }

        // If not at EOF and not at start of test, we are at LCC IR.
        if (contents[i] != '=') {
            // Found LCC IR
            size_t lcc_ir_begin = i;
            ToBeginningOfNextTest();
            size_t lcc_ir_end = i;
            // keep end in-bounds and strip trailing whitespace.
            if (lcc_ir_end >= fsize) lcc_ir_end = fsize - 1;
            while (lcc_ir_end and isspace(contents[lcc_ir_end - 1])) --lcc_ir_end;

            test.ir = {
                contents.begin() + lcc::isz(lcc_ir_begin),
                contents.begin() + lcc::isz(lcc_ir_end)
            };
        }
    }

    ToBeginningOfNextTest();
    return true;
}

template <typename TTest>
concept langtest_test_has_run = requires (TTest test) {
    test.run();
    requires std::convertible_to<decltype(test.run()), bool>;
};
template <typename TTest>
concept langtest_test_derived_from_test = requires (TTest test) {
    requires std::derived_from<TTest, Test>;
};
template <typename TTest>
concept langtest_test_requirements
    = langtest_test_has_run<TTest> and langtest_test_derived_from_test<TTest>;

template <typename TTest>
requires langtest_test_requirements<TTest>
auto parse_and_run_tests(
    std::span<char> contents
) -> TestContext {
    TestContext context{};

    auto fsize = contents.size();
    bool bol = true;
    // Parse tests (first line starting with '=' marks the start of a test)
    for (size_t i = 0; i < fsize; ++i) {
        auto c = contents[i];
        // FIXME: We may want to ensure that the entire line is '=', eventually.
        if (bol and c == '=') {
            TTest test{};
            if (parse_test(contents, fsize, i, test))
                context.record_test(test.run());
            bol = true;
        } else bol = c == '\n';
    }

    return context;
}

template <typename TTest>
requires langtest_test_requirements<TTest>
auto process_ast_test_file(
    const std::filesystem::path& path
) -> TestContext {
    // Read file
    auto path_str = path.string();
    auto* f = fopen(path_str.data(), "rb");
    if (not f) {
        fmt::print("ERROR opening file {}\n", path_str);
        return {};
    }
    fseek(f, 0, SEEK_END);
    auto fsize = size_t(ftell(f));
    fseek(f, 0, SEEK_SET);
    std::vector<char> contents{};
    contents.resize(fsize);
    auto nread = fread(contents.data(), 1, fsize, f);
    if (nread != fsize) {
        fmt::print(
            "ERROR reading file {}\n"
            "    Got {} bytes, expected {}\n",
            path_str,
            nread,
            fsize
        );
        fclose(f);
        exit(1);
    }
    fclose(f);

    return parse_and_run_tests<TTest>(contents);
}

} // namespace langtest
