#include <clopts.hh>
#include <intercept/parser.hh>
#include <laye/parser.hh>
#include <lcc/context.hh>
#include <lcc/diags.hh>
#include <lcc/utils.hh>
#include <string>

namespace detail {
void aluminium_handler() {
#if defined(WIN32) || defined(_WIN32) || defined(__WIN32__) || defined(__NT__)
    // Windows
    system("start https://www.youtube.com/watch?v=dQw4w9WgXcQ");
#elif __APPLE__
    // Apple (iOS, OS X, watchOS...)
    system("open https://www.youtube.com/watch?v=dQw4w9WgXcQ");
#elif __linux__ || __unix__
    // Linux or unix-based
    system("xdg-open https://www.youtube.com/watch?v=dQw4w9WgXcQ");
#endif
}

using namespace command_line_options;
using options = clopts< // clang-format off
    help<>,
    option<"-o", "Path to the output filepath where target code will be stored">,
    option<"--output", "Path to the output filepath where target code will be stored">,
    func<"--aluminium", "That special something to spice up your compilation", aluminium_handler>,
    multiple<positional<"filepath", "Path to files that should be compiled", file<std::vector<char>>>>
>; // clang-format on
} // namespace detail
using detail::options;

int main(int argc, char** argv) {
    options::parse(argc, argv);

    lcc::Context context{};

    // Get input files
    auto input_files = options::get<"filepath">();
    fmt::print("Input files:\n");
    for (const auto& input_file : *input_files) {
        auto& file = context.create_file(input_file.path, input_file.contents);

        // TODO: This could all be moved into something like lcc::handle_input_file

        // Intercept
        if (input_file.path.string().ends_with(".int")) {
            // Parsing (syntactic analysis)
            // std::unique_ptr<lcc::intercept::Module> parsed = lcc::intercept::Parser::Parse(&context, file);

            // TODO: Typechecking (semantic analysis)

            // TODO: IR Generation (frontend codegen)
        }

        // Laye
        else if (input_file.path.string().ends_with(".laye")) {
            // Parsing (syntactic analysis)
            auto file_module = lcc::laye::Parser::Parse(&context, file);

            // TODO: Typechecking (semantic analysis)

            // TODO: IR Generation (frontend codegen)
        }

        // Intermediate Representation (textual)
        else if (input_file.path.string().ends_with(".ir")) {
            // TODO: Parse textual IR
            fmt::print("- Laye: {}\n", input_file.path.string());

        }

        else lcc::Diag::Fatal("Unrecognised input filepath");
    }

    lcc::Diag::Fatal("Driver not fully implemented");
}
