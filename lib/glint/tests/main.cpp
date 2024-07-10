#include <langtest/langtest.hh>

#include <lcc/context.hh>
#include <lcc/format.hh>
#include <lcc/ir/ir.hh>
#include <lcc/ir/module.hh>
#include <lcc/target.hh>
#include <lcc/utils.hh>

#include <glint/ast.hh>
#include <glint/ir_gen.hh>
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
        bool ast_matches{true};
        bool ir_matches{true};
        auto mod = lcc::glint::Parser::Parse(&context, source);
        if (not context.has_error()) {
            // Perform type-checking
            lcc::glint::Sema::Analyse(&context, *mod, true);
            if (not context.has_error()) {
                // TODO: Only confirm AST conforms to expected match tree iff test is NOT
                // decorated as expected to fail.

                auto* root = mod->top_level_func()->body();
                ast_matches = perform_match<lcc::glint::Expr>(root, matcher);

                if (not ir.empty()) {
                    // Parse expected IRGen IR
                    // fmt::print("EXPECTED IR SPAN:\n{}\n", ir);
                    auto expected_ir = lcc::Module::Parse(&context, ir);
                    if (expected_ir) {
                        auto got_ir = lcc::glint::IRGen::Generate(&context, *mod);

                        // For every function in the expected IR, check that the function also exists in the IR we got.
                        for (auto* expected_func : expected_ir->code()) {
                            auto got_func_in_ir = std::find_if(got_ir->code().begin(), got_ir->code().end(), [&](lcc::Function* candidate) {
                                return candidate->name() == expected_func->name();
                            });
                            if (got_func_in_ir == got_ir->code().end()) {
                                fmt::print(
                                    "IR MISMATCH: Expected function {} to be in IR, but didn't find it\n",
                                    expected_func->name()
                                );
                                ir_matches = false;
                                // Stop iterating IR functions since they already don't match.
                                break;
                            }
                            auto got_func = *got_func_in_ir;
                            // TODO: There are other ways functions might not be equivalent, but I
                            // don't think we should handle each and every one of those here. We
                            // should implement '==', or something similar, on lcc::Function itself, I
                            // think.

                            if (expected_func->blocks().size() != got_func->blocks().size()) {
                                fmt::print(
                                    "IR MISMATCH: Block count in function {}\n",
                                    expected_func->name()
                                );
                                ir_matches = false;
                                // Stop iterating IR functions since they already don't match.
                                break;
                            }

                            for (size_t block_i = 0; block_i < expected_func->blocks().size(); ++block_i) {
                                auto expected_block = expected_func->blocks().at(block_i);
                                auto got_block = got_func->blocks().at(block_i);

                                if (expected_func->blocks().size() != got_func->blocks().size()) {
                                    fmt::print(
                                        "IR MISMATCH: Instruction count in block {} in function {}\n",
                                        expected_block->name(),
                                        expected_func->name()
                                    );
                                    ir_matches = false;
                                    // Stop iterating IR functions since they already don't match.
                                    break;
                                }

                                for (size_t inst_i = 0; inst_i < expected_block->instructions().size(); ++inst_i) {
                                    auto expected_inst = expected_block->instructions().at(inst_i);
                                    auto got_inst = got_block->instructions().at(inst_i);

                                    if (expected_inst->kind() != got_inst->kind()) {
                                        // TODO: Maybe have this behind a "--verbose-ir" CLI flag or something
                                        // fmt::print("\nExpected IR:\n");
                                        // expected_func->print();
                                        // fmt::print("Got IR:\n");
                                        // got_func->print();

                                        fmt::print("(1): ");
                                        expected_inst->print();
                                        fmt::print("{}\n", C(lcc::utils::Colour::Reset));

                                        fmt::print("(2): ");
                                        got_inst->print();
                                        fmt::print("{}\n", C(lcc::utils::Colour::Reset));

                                        fmt::print(
                                            "IR MISMATCH: Expected instruction (1) but got instruction (2) in block {} in function {}\n",
                                            expected_block->name(),
                                            expected_func->name()
                                        );
                                    }
                                }
                            }
                        }
                    } else fmt::print("Error parsing expected IR for test {}\n", name);
                }

            } else failed_check = true;
        } else failed_parse = true;

        fmt::print("  {}: ", name);
        if (not ast_matches or not ir_matches or failed_parse or failed_check) {
            fmt::print("{}FAIL{}\n", C(Colour::Red), C(Colour::Reset));
            if (not ast_matches) {
                std::string expected = matcher.print();
                std::string got = print_node<lcc::glint::Expr>(mod->top_level_func()->body());

                // find_different_from_begin()
                size_t diff_begin{0};
                for (; diff_begin < expected.size() and diff_begin < got.size(); ++diff_begin)
                    if (expected.at(diff_begin) != got.at(diff_begin)) break;

                auto expected_color = C(lcc::utils::Colour::Green);
                expected.insert(
                    expected.begin() + lcc::isz(diff_begin),
                    expected_color.begin(),
                    expected_color.end()
                );
                expected += C(lcc::utils::Colour::Reset);

                auto got_color = C(lcc::utils::Colour::Red);
                got.insert(
                    got.begin() + lcc::isz(diff_begin),
                    got_color.begin(),
                    got_color.end()
                );
                got += C(lcc::utils::Colour::Reset);

                fmt::print("EXPECTED: {}\n", expected);
                fmt::print("GOT:      {}\n", got);
            }
        } else fmt::print("{}PASS{}\n", C(Colour::Green), C(Colour::Reset));
    }
};

int main(int argc, const char** argv) {
    for (int i = 1; i < argc; ++i)
        LCC_ASSERT(false, "No command line options are accepted at the moment");

    for (auto entry : std::filesystem::directory_iterator("ast")) {
        if (entry.is_regular_file()) {
            fmt::print("{}:\n", entry.path().lexically_normal().filename().string());
            process_ast_test_file<GlintTest>(entry.path());
        }
    }

    return 0;
}