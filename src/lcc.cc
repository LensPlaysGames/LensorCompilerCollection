#include <c/ast.hh>
#include <c/parser.hh>
#include <clopts.hh>
#include <intercept/ast.hh>
#include <intercept/ir_gen.hh>
#include <intercept/parser.hh>
#include <intercept/sema.hh>
#include <laye/ir_gen.hh>
#include <laye/parser.hh>
#include <laye/sema.hh>
#include <lcc/context.hh>
#include <lcc/diags.hh>
#include <lcc/format.hh>
#include <lcc/ir/module.hh>
#include <lcc/lcc-c.h>
#include <lcc/opt/opt.hh>
#include <lcc/target.hh>
#include <lcc/utils.hh>
#include <lcc/utils/platform.hh>
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

/// Default format
const lcc::Format* default_format = lcc::Format::gnu_as_att_assembly;

using namespace command_line_options;
using options = clopts< // clang-format off
    help<>,
    option<"-o", "Path to the output filepath where target code will be stored">,
    multiple<option<"-I", "Add a directory to the include search paths">>,
    option<"-f", "What format to emit code in (default: asm)",
        values<
            "llvm",
            "asm", "gnu-as-att",
            "obj", "elf", "coff"
        >
    >,
    option<"--color", "Whether to include colours in the output (default: auto)",
        values<"always", "auto", "never">
    >,
    option<"--passes", "Comma-separated list of optimisation passes to run">,
    experimental::short_option<"-O", "Set optimisation level (default: 0)", values<0, 1, 2, 3>>,
    flag<"-v", "Enable verbose output">,
    flag<"--ast", "Print the AST and exit without generating code">,
    flag<"--sema", "Run sema only and exit">,
    flag<"--syntax-only", "Do not perform semantic analysis">,
    flag<"--ir", "Emit LCC intermediate representation and exit">,
    flag<"--mir", "Emit LCC machine instruction representation at various stages and exit">,
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
    auto colour_opt = opts.get_or<"--color">("auto");
    bool use_colour{};
    if (colour_opt == "always") use_colour = true;
    else if (colour_opt == "never") use_colour = false;
    else use_colour = lcc::platform::StdoutIsTerminal() or lcc::platform::StderrIsTerminal();

    // Determine whether we should print MIR or not.
    bool should_print_mir{opts.get<"--mir">()};

    /// Get input files
    auto& input_files = *opts.get<"filepath">();
    if (opts.get<"-v">()) {
        fmt::print("Input files:\n");
        for (const auto& input_file : input_files)
            fmt::print("- {}\n", input_file.path.string());
    }

    if (input_files.empty())
        lcc::Diag::Fatal("no input files");

    /// Compile the file.
    // TODO: Get target from "-t" or "--target" command line option.
    auto* format = detail::default_format;
    if (auto format_string = opts.get<"-f">()) {
        if (*format_string == "llvm") {
            format = lcc::Format::llvm_textual_ir;
        } else if (*format_string == "asm" || *format_string == "gnu-as-att") {
            format = lcc::Format::gnu_as_att_assembly;
        } else if (*format_string == "obj") {
#if defined(_MSC_VER)
            format = lcc::Format::coff_object;
#else
            format = lcc::Format::elf_object;
#endif
        } else if (*format_string == "elf") {
            format = lcc::Format::elf_object;
        } else if (*format_string == "coff") {
            format = lcc::Format::coff_object;
        }
    }

    lcc::Context context{
        detail::default_target,
        format,
        use_colour,
        should_print_mir};

    for (const auto& dir : *opts.get<"-I">()) {
        if (opts.get<"-v">()) fmt::print("Added input directory: {}\n", dir);
        context.add_include_directory(dir);
    }

    auto ConvertFileExtensionToOutputFormat = [&](std::string path_string) {
        const char* replacement = ".s";
        if (context.format()->format() == lcc::Format::LLVM_TEXTUAL_IR)
            replacement = ".ll";
        if (context.format()->format() == lcc::Format::ELF_OBJECT or context.format()->format() == lcc::Format::COFF_OBJECT)
            replacement = ".o";

        return std::filesystem::path{path_string}.replace_extension(replacement).string();
    };

    /// Common path after IR gen.
    auto EmitModule = [&](lcc::Module* m, std::string_view input_file_path, std::string_view output_file_path) {
        /// Do NOT do anything else as this means that we
        /// *only* want to run optimisation passes; specifically,
        /// do *not* run lowering if this option was specified.
        if (auto p = opts.get<"--passes">()) {
            lcc::opt::RunPasses(m, *p);
            if (opts.get<"--ir">()) m->print_ir(use_colour);
            return;
        }

        if (auto opt = opts.get_or<"-O">(0))
            lcc::opt::Optimise(m, int(opt));

        if (opts.get<"--ir">()) {
            m->print_ir(use_colour);
            return;
        }

        m->lower();
        m->emit(output_file_path);

        if (opts.get<"-v">()) {
            fmt::print("Generated output from {} at {}\n", input_file_path, output_file_path);
        }
    };

    // NOTE: Moves the input file, so, uhh, don't use that after passing it to
    // this.
    auto GenerateOutputFile = [&](auto& input_file, std::string_view output_file_path) {
        auto path_str = input_file.path.string();
        auto& file = context.create_file(
            std::move(input_file.path),
            std::move(input_file.contents)
        );

        /// LCC IR.
        if (path_str.ends_with(".lcc")) {
            auto mod = lcc::Module::Parse(&context, file);
            if (context.has_error()) return; // the error condition is handled by the caller already
            return EmitModule(mod.get(), path_str, output_file_path);
        }

        /// Intercept.
        else if (path_str.ends_with(".int")) {
            /// Parse the file.
            auto mod = lcc::intercept::Parser::Parse(&context, file);
            if (context.has_error()) return; // the error condition is handled by the caller already
            if (opts.get<"--syntax-only">()) {
                if (opts.get<"--ast">()) mod->print(use_colour);
                return;
            }

            /// Perform semantic analysis.
            lcc::intercept::Sema::Analyse(&context, *mod, true);
            if (context.has_error()) return; // the error condition is handled by the caller already
            if (opts.get<"--ast">()) {
                mod->print(use_colour);
                return;
            }

            /// Stop after sema if requested.
            if (opts.get<"--sema">()) return;

            return EmitModule(lcc::intercept::IRGen::Generate(&context, *mod), path_str, output_file_path);
        }

        /// Laye.
        else if (path_str.ends_with(".laye")) {
            auto laye_context = new lcc::laye::LayeContext{&context};

            /// Parse the file.
            auto mod = laye_context->get_or_load_module(file);
            if (context.has_error()) return; // the error condition is handled by the caller already
            if (opts.get<"--syntax-only">()) {
                if (opts.get<"--ast">()) laye_context->print_modules();
                // stop processing this file, but DO continue to process other files passed in
                return;
            }

            /// Perform semantic analysis.
            lcc::laye::Sema::Analyse(laye_context, mod, true);
            if (context.has_error()) return; // the error condition is handled by the caller already
            if (opts.get<"--ast">()) {
                laye_context->print_modules();
                return;
            }

            /// Stop after sema if requested.
            if (opts.get<"--sema">()) return;

            for (auto module : laye_context->modules()) {
                std::string input_file_path = module->file()->path().string();
                std::string module_output_file_path = ConvertFileExtensionToOutputFormat(module->file()->path().string());
                EmitModule(lcc::laye::IRGen::Generate(laye_context, module), input_file_path, module_output_file_path);
            }

            // TODO(local): after generating all of the modules separate, THEN obey the `output_file_path` when linking
        }

        /// C.
        else if (path_str.ends_with(".c")) {
            /// Parse the file.
            auto c_context = new lcc::c::CContext{&context};
            auto translation_unit = lcc::c::Parser::Parse(c_context, file);

            if (opts.get<"--syntax-only">()) {
                if (opts.get<"--ast">()) translation_unit->print();
                return;
            }

            LCC_TODO();
        }

        /// Unknown.
        else {
            lcc::Diag::Fatal("Unrecognised input file type");
        }
    };

    auto configured_output_file_path = opts.get_or<"-o">("");
    if (input_files.size() == 1) {
        std::string output_file_path = configured_output_file_path;
        if (output_file_path.empty())
            output_file_path = ConvertFileExtensionToOutputFormat(input_files[0].path.string());

        GenerateOutputFile(input_files[0], output_file_path);
        if (context.has_error()) return 1;
        if (opts.get<"-v">()) fmt::print("Generated output at {}\n", output_file_path);
    } else {
        if (not configured_output_file_path.empty()) {
            // TODO(local): you can, but only if we're planning to link these files; handle that later
            lcc::Diag::Fatal("cannot specify -o when generating multiple output files");
        }

        for (auto& input_file : input_files) {
            std::string input_file_path = input_file.path.string();
            std::string output_file_path = ConvertFileExtensionToOutputFormat(input_file_path);
            GenerateOutputFile(input_file, output_file_path);
        }

        // TODO(local): if we do linking, now's the time to link to the output file path, else a.out
        // std::string linked_output_file_path = configured_output_file_path;
    }

    return 0;
}
