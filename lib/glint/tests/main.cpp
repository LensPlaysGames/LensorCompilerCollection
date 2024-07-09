#include <langtest/langtest.hh>

#include <lcc/context.hh>
#include <lcc/format.hh>
#include <lcc/target.hh>
#include <lcc/utils.hh>

#include <glint/ast.hh>
#include <glint/parser.hh>
#include <glint/sema.hh>

#include <filesystem>

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

struct GlintTest : Test {
    void run() {
        LCC_ASSERT(not name.empty(), "Refusing to run test with empty name");

        // Parse test source as Glint

        // TODO: Get target from "-t" or "--target" command line option.
        // TODO: Get format from "-f" or "--format" command line option.
        lcc::Context context{
            default_target,
            default_format,
            false,
            false,
            false};

        bool failed_parse{false};
        bool failed_check{false};
        bool matches{true};
        auto mod = lcc::glint::Parser::Parse(&context, source);
        if (not context.has_error()) {
            // Perform type-checking
            lcc::glint::Sema::Analyse(&context, *mod, true);
            if (not context.has_error()) {
                // TODO: Only confirm AST conforms to expected match tree iff test is NOT
                // decorated as expected to fail.

                auto* root = mod->top_level_func()->body();
                matches = perform_match(root, matcher);
            } else failed_check = true;
        } else failed_parse = true;

        fmt::print("  {}: ", name);
        if (not matches or failed_parse or failed_check) {
            fmt::print("{}FAIL{}\n", C(Colour::Red), C(Colour::Reset));
            if (not matches) {
                fmt::print("EXPECTED: {}\n", matcher.print());
                fmt::print("GOT: {}\n", print_node(mod->top_level_func()->body()));
            }
        } else fmt::print("{}PASS{}\n", C(Colour::Green), C(Colour::Reset));
    }
};

int main(int argc, const char** argv) {
    for (int i = 1; i < argc; ++i)
        LCC_ASSERT(false, "No command line options are accepted at the moment");

    for (auto entry : std::filesystem::directory_iterator("ast"))
        if (entry.is_regular_file()) {
            fmt::print("{}:\n", entry.path().lexically_normal().filename().string());
            process_ast_test_file<GlintTest>(entry.path());
        }

    return 0;
}
