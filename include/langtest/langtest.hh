#include <fmt/format.h>
#include <lcc/context.hh>
#include <lcc/format.hh>
#include <lcc/target.hh>
#include <lcc/utils.hh>

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
    std::string_view name;
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
    bool should_only_parse{false};
    bool should_fail_parse{false}; // implies should_only_parse
    bool should_fail_check{false};
    bool should_warn_parse{false};
    bool should_warn_check{false};
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
                test.should_only_parse = true;
            } else if (
                specifier == "warn_parse" or specifier == "warn_syntax"
                or specifier == "parse_warn" or specifier == "syntax_warn"
            ) {
                test.should_warn_parse = true;
            } else if (
                specifier == "fail_parse"
                or specifier == "error_parse" or specifier == "parse_error"
            ) {
                test.should_fail_parse = true;
            } else if (
                specifier == "warn_sema" or specifier == "warn_check"
                or specifier == "sema_warn" or specifier == "check_warn"
            ) {
                test.should_warn_parse = true;
            } else if (
                specifier == "fail_sema" or specifier == "fail_check"
                or specifier == "error_sema" or specifier == "sema_error"
            ) {
                test.should_fail_check = true;
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
            if (not (test.should_fail_parse or test.should_fail_check)) {
                fmt::print("ERROR test has no matcher declared but it is not specified to fail... {}\n", test.name);
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
