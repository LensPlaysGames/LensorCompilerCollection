#include <c/ast.hh>
#include <c/parser.hh>
#include <clopts.hh>
#include <intercept/ast.hh>
#include <intercept/ir_gen.hh>
#include <intercept/parser.hh>
#include <intercept/sema.hh>
#include <laye/parser.hh>
#include <laye/sema.hh>
#include <laye/ir_gen.hh>
#include <lcc/context.hh>
#include <lcc/diags.hh>
#include <lcc/ir/module.hh>
#include <lcc/lcc-c.h>
#include <lcc/target.hh>
#include <lcc/format.hh>
#include <lcc/utils.hh>
#include <string>

#ifdef _WIN32
#    include <io.h>
#    define isatty _isatty
#endif

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

/// Default format
const lcc::Format* default_format = lcc::Format::gnu_as_att_assembly;

using namespace command_line_options;
using options = clopts< // clang-format off
    help<>,
    option<"-o", "Path to the output filepath where target code will be stored">,
    option<"-f", "What format to emit code in (default: asm)",
           values<
               "llvm",
               "asm", "gnu-as-att"
               >
           >,
    option<"--color", "Whether to include colours in the output (default: auto)",
           values<"always", "auto", "never">>,
    flag<"-v", "Enable verbose output">,
    flag<"--ast", "Print the AST and exit without generating code">,
    flag<"--syntax-only", "Do not perform semantic analysis">,
    flag<"--ir", "Emit LCC intermediate representation and exit">,
    func<"--aluminium", "That special something to spice up your compilation", aluminium_handler>,
    multiple<positional<"filepath", "Path to files that should be compiled", file<std::vector<char>>, true>>
>; // clang-format on
} // namespace detail
using detail::options;

[[noreturn]] bool HandleOptParseError(std::string&& msg) {
    lcc::Diag::Fatal("{}", msg);
}

int main(int argc, char** argv) {
    auto opts = options::parse(argc, argv, HandleOptParseError);

    /// Determine whether to use colours in the output.
    /// TODO: Enable colours in the console on Windows (for `cmd`).
    /// TODO: Diagnostics should honour this flag.
    auto colour_opt = opts.get_or<"--color">("auto");
    bool use_colour{};
    if (colour_opt == "always") use_colour = true;
    else if (colour_opt == "never") use_colour = false;
    else use_colour = isatty(fileno(stdout));

    /// Get input files
    auto& input_files = *opts.get<"filepath">();
    if (opts.get<"-v">()) {
        fmt::print("Input files:\n");
        for (const auto& input_file : input_files)
            fmt::print("- {}\n", input_file.path.string());
    }

    /// TODO: Handle multiple files.
    if (input_files.empty() or input_files.size() > 1)
        lcc::Diag::Fatal("Expected exactly one input file");

    /// Compile the file.
    // TODO: Get target from "-t" or "--target" command line option.
    auto* format = detail::default_format;
    if (auto format_string = opts.get<"-f">()) {
        if (*format_string == "llvm") {
            format = lcc::Format::llvm_textual_ir;
        } else if (*format_string == "asm" || *format_string == "gnu-as-att") {
            format = lcc::Format::gnu_as_att_assembly;
        }
    }
    lcc::Context context{detail::default_target, format};
    auto path_str = input_files[0].path.string();
    auto& file = context.create_file(
        std::move(input_files[0].path),
        std::move(input_files[0].contents)
    );

    /// Intercept.
    if (path_str.ends_with(".int")) {
        /// Parse the file.
        auto mod = lcc::intercept::Parser::Parse(&context, file);
        if (opts.get<"--syntax-only">()) {
            if (context.has_error()) std::exit(1);
            if (opts.get<"--ast">()) mod->print(use_colour);
            std::exit(0);
        }

        /// Perform semantic analysis.
        lcc::intercept::Sema::Analyse(&context, *mod, true);
        if (opts.get<"--ast">()) {
            if (context.has_error()) std::exit(1);
            mod->print(use_colour);
        }

        auto ir_module = lcc::intercept::IRGen::Generate(&context, *mod);
        if (opts.get<"--ir">()) {
            ir_module->print_ir(use_colour);
            std::exit(0);
        }

        ir_module->lower();
        ir_module->emit();

        return 42;
    }

    /// Laye.
    if (path_str.ends_with(".laye")) {
        auto laye_context = new lcc::laye::LayeContext{&context};

        /// Parse the file.
        auto mod = laye_context->parse_laye_file(file);
        if (opts.get<"--syntax-only">()) {
            if (context.has_error()) std::exit(1);
            if (opts.get<"--ast">()) laye_context->print_modules();
            std::exit(0);
        }

        /// Perform semantic analysis.
        lcc::laye::Sema::Analyse(laye_context, mod, true);
        if (opts.get<"--ast">()) {
            if (context.has_error()) std::exit(1);
            laye_context->print_modules();
        }

        auto ir_module = lcc::laye::IRGen::Generate(laye_context, mod);
        if (opts.get<"--ir">()) {
            ir_module->print_ir(use_colour);
            std::exit(0);
        }

        ir_module->emit();
        return 69;
    }

    /// C.
    if (path_str.ends_with(".c")) {
        /// Parse the file.
        auto c_context = new lcc::c::CContext{&context};
        auto translation_unit = lcc::c::Parser::Parse(c_context, file);

        if (opts.get<"--syntax-only">()) {
            if (opts.get<"--ast">()) translation_unit->print();
            std::exit(0);
        }

        return 99;
    }

    /// Unknown.
    lcc::Diag::Fatal("Unrecognised input file type");
}
