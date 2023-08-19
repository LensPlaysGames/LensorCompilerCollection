#include <c/ast.hh>
#include <c/parser.hh>
#include <clopts.hh>
#include <intercept/ast.hh>
#include <intercept/parser.hh>
#include <intercept/sema.hh>
#include <intercept/ir_gen.hh>
#include <laye/parser.hh>
#include <laye/sema.hh>
#include <lcc/context.hh>
#include <lcc/diags.hh>
#include <lcc/target.hh>
#include <lcc/utils.hh>
#include <lcc/ir/module.hh>
#include <string>

namespace detail {
void aluminium_handler() {
#if defined(LCC_PLATFORM_WINDOWS)
    // Windows
    system("start https://www.youtube.com/watch?v=dQw4w9WgXcQ");
#elif defined(__APPLE__)
    // Apple (iOS, OS X, watchOS...)
    system("open https://www.youtube.com/watch?v=dQw4w9WgXcQ");
#elif __linux__ || __unix__
    // Linux or unix-based
    system("xdg-open https://www.youtube.com/watch?v=dQw4w9WgXcQ");
#endif
}

/// Default target.
const lcc::Target* default_target =
#if defined(LCC_PLATFORM_WINDOWS)
    lcc::Target::x86_64_windows;
#elif defined(__APPLE__) or defined(__linux__)
    lcc::Target::x86_64_linux;
#else
#    error "Unsupported target"
#endif

using namespace command_line_options;
using options = clopts< // clang-format off
    help<>,
    option<"-o", "Path to the output filepath where target code will be stored">,
    flag<"-v", "Enable verbose output">,
    flag<"--ast", "Print the AST and exit without generating code">,
    flag<"--syntax-only", "Do not perform semantic analysis">,
    func<"--aluminium", "That special something to spice up your compilation", aluminium_handler>,
    multiple<positional<"filepath", "Path to files that should be compiled", file<std::vector<char>>, true>>
>; // clang-format on
} // namespace detail
using detail::options;

int main(int argc, char** argv) {
    options::parse(argc, argv);

    /// Get input files
    auto& input_files = *options::get<"filepath">();
    if (options::get<"-v">()) {
        fmt::print("Input files:\n");
        for (const auto& input_file : input_files)
            fmt::print("- {}\n", input_file.path.string());
    }

    /// TODO: Handle multiple files.
    if (input_files.empty() or input_files.size() > 1)
        lcc::Diag::Fatal("Expected exactly one input file");

    /// Compile the file.
    lcc::Context context{detail::default_target};
    auto path_str = input_files[0].path.string();
    auto& file = context.create_file(input_files[0].path, input_files[0].contents);

    /// Intercept.
    if (path_str.ends_with(".int")) {
        /// Parse the file.
        auto mod = lcc::intercept::Parser::Parse(&context, file);
        if (options::get<"--syntax-only">()) {
            if (context.has_error()) std::exit(1);
            if (options::get<"--ast">()) mod->print();
            std::exit(0);
        }

        /// Perform semantic analysis.
        lcc::intercept::Sema::Analyse(&context, *mod, true);
        if (options::get<"--ast">()) {
            if (context.has_error()) std::exit(1);
            mod->print();
            std::exit(0);
        }

        auto ir_module = lcc::intercept::IRGen::Generate(&context, *mod);
        (void)ir_module;

        return 42;
    }

    /// Laye.
    if (path_str.ends_with(".laye")) {
        auto laye_context = new lcc::laye::LayeContext{&context};

        /// Parse the file.
        auto mod = laye_context->parse_laye_file(file);
        (void) mod; // temp so I don't get a warning here, I may want this module later

        if (options::get<"--syntax-only">()) {
            if (options::get<"--ast">()) laye_context->print_modules();
            std::exit(0);
        }

        /// Perform semantic analysis.
        lcc::laye::Sema::Analyse(laye_context, true);

        /// Nice.
        return 69;
    }

    /// C.
    if (path_str.ends_with(".c")) {
        /// Parse the file.
        auto c_context = new lcc::c::CContext{&context};
        auto translation_unit = lcc::c::Parser::Parse(c_context, file);

        if (options::get<"--syntax-only">()) {
            if (options::get<"--ast">()) translation_unit->print();
            std::exit(0);
        }

        return 89;
    }

    /// Unknown.
    lcc::Diag::Fatal("Unrecognised input file type");
}
