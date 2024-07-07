#include <c/ast.hh>
#include <c/parser.hh>
#include <fmt/format.h>
#include <glint/ast.hh>
#include <glint/ir_gen.hh>
#include <glint/parser.hh>
#include <glint/sema.hh>
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

#include <cstdio>
#include <format>
#include <string>
#include <vector>

struct TwoColumnLayoutHelper {
    struct Column {
        const std::string_view a;
        const std::string_view b;
    };
    std::vector<Column> columns;

    /// Gap inserted between A and B columns.
    std::string gap{"    "};

    char padding_character{' '};

    std::string get() const {
        size_t longest_a_side = 0;
        for (const auto& c : columns) {
            if (c.a.size() > longest_a_side)
                longest_a_side = c.a.size();
        }

        std::string out{};
        for (const auto& c : columns) {
            out += c.a;

            if (c.b.size()) {
                // Do padding if needed to line up b side
                for (size_t i = c.a.size(); i < longest_a_side; ++i)
                    out += padding_character;

                out += gap;
                out += c.b;
            }
        }

        return out;
    }

    operator std::string() const {
        return get();
    }
};

namespace detail {
void aluminium_handler() {
#if defined(LCC_PLATFORM_WINDOWS)
    // Windows
    system("start https://www.youtube.com/watch?v=dQw4w9WgXcQ");
#elif defined(__APPLE__)
    // Apple
    system("open https://www.youtube.com/watch?v=dQw4w9WgXcQ");
#elif __linux__ || __unix__
    // Linux-ish
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
} // namespace detail

struct Options {
    bool verbose{false};
    bool ast{false};
    bool ir{false};
    bool mir{false};
    bool stopat_syntax{false};
    bool stopat_sema{false};
    bool stopat_ir{false};
    bool stopat_mir{false};
    bool aluminium{false};

    std::vector<std::string> input_files{};
    std::vector<std::string> include_directories{};
    std::string output_filepath{};
    int optimisation{0};
    std::string optimisation_passes{};
    std::string color{"auto"};
    std::string language{"default"};
    std::string format{"default"};
};

int main(int argc, const char** argv) {
    Options options{};
    for (int i = 1; i < argc; ++i) {
        auto next_arg = [&]() {
            if (i + 1 >= argc) {
                fmt::print("CLI ERROR: You probably gave an option and didn't supply a value at the very end of the command line\n");
                exit(1);
            }
            return std::string_view{argv[++i]};
        };
        const auto arg = std::string_view{argv[i]};
        if (arg.substr(0, 3) == "--h" or arg.substr(0, 2) == "-h") {
            // clang-format off
            fmt::print("USAGE: {} [FLAGS] [OPTIONS] [SOURCE FILES...]\n", argv[0]);
            fmt::print("FLAGS:\n");
            fmt::print("{}", TwoColumnLayoutHelper{{
                {"  -v", "Enable verbose output\n"},
                {"  --ast", "Emit AST in human-readable format\n"},
                {"  --ir", "Emit LCC intermediate representation\n"},
                {"  --mir", "Emit LCC machine instruction representation at various stages\n"},
                {"  --stopat-syntax", "Request language does not process input further than syntactic analysis\n"},
                {"  --stopat-sema", "Request language does not process input further than semantic analysis\n"},
                {"  --stopat-ir", "Do not process input further than LCC's intermediate representation (IR)\n"},
                {"  --stopat-mir", "Do not process input further than LCC's machine instruction representation (MIR)\n"},
                {"  --aluminium", "That special something to spice up your compilation\n"}},
            }.get());
            fmt::print("OPTIONS:\n");
            fmt::print("{}", TwoColumnLayoutHelper{{
                {"  -I", "Add a directory to the include search paths\n"},
                {"  -o", "Path to the output filepath where target code will be stored\n"},
                {"  -O", "Set optimisation level (default 0)\n"},
                {"", "    0, 1, 2, 3\n"},
                {"  --passes", "Comma-separated list of optimisation passes to run\n"},
                {"  --color", "Whether to include colors colours in the output (default: auto)\n"},
                {"", "    always, auto, never\n"},
                {"  -x", "What language to parse input code as (default: extension based)\n"},
                {"", "    c, glint, ir\n"},
                {"  -f", "What format to emit code in (default: asm)\n"},
                {"", "    asm, gnu-as-att, obj, elf, coff, llvm\n"},
                }}.get());
            // clang-format on
            exit(0);
        }

        if (arg == "-v")
            options.verbose = true;
        else if (arg == "--ast")
            options.ast = true;
        else if (arg == "--ir")
            options.ir = true;
        else if (arg == "--mir")
            options.mir = true;
        else if (arg == "--stopat-syntax")
            options.stopat_syntax = true;
        else if (arg == "--stopat-sema")
            options.stopat_sema = true;
        else if (arg == "--stopat-ir")
            options.stopat_ir = true;
        else if (arg == "--stopat-mir")
            options.stopat_mir = true;
        else if (arg == "--aluminium")
            options.aluminium = true;

        else if (arg == "-I") {
            // Add a directory to the include search paths
            auto include_dir = next_arg();
            options.include_directories.push_back(std::string(include_dir));
        } else if (arg == "-o") {
            // Path to the output filepath where target code will be stored
            auto output_path = next_arg();
            options.output_filepath = std::string(output_path);
        } else if (arg == "-O") {
            // Set optimisation level (default: 0)
            auto o_level_str = next_arg();

            auto o_level = 0;
            if (o_level_str == "0")
                ;
            else if (o_level_str == "1")
                o_level = 1;
            else if (o_level_str == "2")
                o_level = 2;
            else if (o_level_str == "3")
                o_level = 3;
            else {
                fmt::print("CLI ERROR: Invalid optimisation level {}\n", o_level_str);
                exit(1);
            }

            options.optimisation = o_level;
        } else if (arg == "--passes") {
            // Comma-separated list of optimisation passes to run
            auto passes = next_arg();
            options.optimisation_passes = passes;
        } else if (arg == "--color") {
            // Whether to include colours in the output
            auto color = next_arg();
            if (color != "always" and color != "auto" and color != "never") {
                fmt::print("CLI ERROR: Invalid color level {}\n", color);
                exit(1);
            }
            options.color = color;
        } else if (arg == "-x") {
            // What language to parse input code as
            auto lang = next_arg();
            if (lang != "c" and lang != "glint" and lang != "ir") {
                fmt::print("CLI ERROR: Invalid lang {}\n", lang);
                exit(1);
            }
            options.language = lang;
        } else if (arg == "-f") {
            // What format to emit code in
            auto format = next_arg();
            if (
                format != "asm" and format != "gnu-as-att"
                and format != "obj" and format != "elf"
                and format != "coff" and format != "llvm"
            ) {
                fmt::print("CLI ERROR: Invalid format {}\n", format);
                exit(1);
            }
            options.format = format;
        } else if (arg.starts_with("-")) {
            fmt::print(
                "CLI ERROR: Unrecognized command line option or flag {}\n"
                "    Prepend ./ or equivalent for file that starts with '-'\n",
                arg
            );
            exit(1);
        } else {
            // Otherwise, it's a filepath
            options.input_files.push_back(std::string(arg));
        }
    }

    /// Determine whether to use colours in the output.
    /// TODO: Enable colours in the console on Windows (for `cmd`).
    auto colour_opt = options.color;
    bool use_colour{};
    if (colour_opt == "always") use_colour = true;
    else if (colour_opt == "never") use_colour = false;
    else use_colour = lcc::platform::StdoutIsTerminal() or lcc::platform::StderrIsTerminal();

    /// Get input files
    auto& input_files = options.input_files;
    if (options.verbose) {
        fmt::print("Input files:\n");
        for (const auto& input_file : input_files)
            fmt::print("- {}\n", input_file);
    }

    if (input_files.empty())
        lcc::Diag::Fatal("no input files");

    /// Compile the file.
    // TODO: Get target from "-t" or "--target" command line option.
    auto* format = detail::default_format;
    if (options.format == "llvm") {
        format = lcc::Format::llvm_textual_ir;
    } else if (options.format == "asm" || options.format == "gnu-as-att") {
        format = lcc::Format::gnu_as_att_assembly;
    } else if (options.format == "obj") {
#if defined(_MSC_VER)
        format = lcc::Format::coff_object;
#else
        format = lcc::Format::elf_object;
#endif
    } else if (options.format == "elf") {
        format = lcc::Format::elf_object;
    } else if (options.format == "coff") {
        format = lcc::Format::coff_object;
    }

    lcc::Context context{
        detail::default_target,
        format,
        use_colour,
        options.mir,
        options.stopat_mir};

    for (const auto& dir : options.include_directories) {
        if (options.verbose) fmt::print("Added input directory: {}\n", dir);
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
        if (options.optimisation_passes.size()) {
            lcc::opt::RunPasses(m, options.optimisation_passes);
            if (options.ir) m->print_ir(use_colour);
            return;
        }

        if (options.ir)
            m->print_ir(use_colour);

        if (options.optimisation)
            lcc::opt::Optimise(m, int(options.optimisation));

        if (options.ir) {
            fmt::print("\nAfter Optimisations:\n");
            m->print_ir(use_colour);
        }

        m->lower();

        if (options.ir) {
            fmt::print("\nAfter Lowering:\n");
            m->print_ir(use_colour);
        }

        if (options.stopat_ir) return;

        m->emit(output_file_path);

        if (options.verbose)
            fmt::print("Generated output from {} at {}\n", input_file_path, output_file_path);
    };

    // NOTE: Moves the input file, so, uhh, don't use that after passing it to
    // this.
    auto specified_language = options.language;
    auto GenerateOutputFile = [&](auto& input_file, std::string_view output_file_path) {
        auto path_str = input_file;
        auto f = fopen(input_file.data(), "rb");
        if (not f) {
            fmt::print("ERROR opening file {}\n", path_str);
            return;
        }
        fseek(f, 0, SEEK_END);
        size_t fsize = size_t(ftell(f));
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
        auto& file = context.create_file(
            std::move(input_file),
            std::move(contents)
        );

        /// LCC IR.
        if (specified_language == "ir" or (specified_language == "default" and path_str.ends_with(".lcc"))) {
            auto mod = lcc::Module::Parse(&context, file);
            if (context.has_error()) return; // the error condition is handled by the caller already
            return EmitModule(mod.get(), path_str, output_file_path);
        }

        /// Glint.
        else if (specified_language == "glint" or (specified_language == "default" and path_str.ends_with(".g"))) {
            /// Parse the file.
            auto mod = lcc::glint::Parser::Parse(&context, file);
            if (context.has_error()) return; // the error condition is handled by the caller already
            if (options.ast) mod->print(use_colour);
            if (options.stopat_syntax) return;

            /// Perform semantic analysis.
            lcc::glint::Sema::Analyse(&context, *mod, true);
            if (context.has_error()) return; // the error condition is handled by the caller already
            if (options.ast) {
                fmt::print("\nAfter Sema:\n");
                mod->print(use_colour);
            }

            /// Stop after sema if requested.
            if (options.stopat_sema) return;

            auto ir = lcc::glint::IRGen::Generate(&context, *mod);

            return EmitModule(ir, path_str, output_file_path);
        }

        /// Intercept.
        else if (specified_language == "int" or (specified_language == "default" and path_str.ends_with(".int"))) {
            /// Parse the file.
            auto mod = lcc::intercept::Parser::Parse(&context, file);
            if (context.has_error()) return; // the error condition is handled by the caller already
            if (options.ast) mod->print(use_colour);
            if (options.stopat_syntax) return;

            /// Perform semantic analysis.
            lcc::intercept::Sema::Analyse(&context, *mod, true);
            if (context.has_error()) return; // the error condition is handled by the caller already
            if (options.ast) mod->print(use_colour);
            if (options.stopat_sema) return;

            return EmitModule(lcc::intercept::IRGen::Generate(&context, *mod), path_str, output_file_path);
        }

        /// Laye.
        else if (specified_language == "laye" or (specified_language == "default" and path_str.ends_with(".laye"))) {
            auto laye_context = new lcc::laye::LayeContext{&context};

            /// Parse the file.
            auto mod = laye_context->get_or_load_module(file);
            if (context.has_error()) return; // the error condition is handled by the caller already
            if (options.ast) laye_context->print_modules();
            // stop processing this file, but DO continue to process other files passed in
            if (options.stopat_syntax) return;

            /// Perform semantic analysis.
            lcc::laye::Sema::Analyse(laye_context, mod, true);
            if (context.has_error()) return; // the error condition is handled by the caller already
            if (options.ast) {
                laye_context->print_modules();
                return;
            }

            /// Stop after sema if requested.
            if (options.stopat_sema) return;

            for (auto module : laye_context->modules()) {
                std::string input_file_path = module->file()->path().string();
                std::string module_output_file_path = ConvertFileExtensionToOutputFormat(module->file()->path().string());
                EmitModule(lcc::laye::IRGen::Generate(laye_context, module), input_file_path, module_output_file_path);
            }

            // TODO(local): after generating all of the modules separate, THEN obey the `output_file_path` when linking
        }

        /// C.
        else if (specified_language == "c" or (specified_language == "default" and path_str.ends_with(".c"))) {
            /// Parse the file.
            auto c_context = new lcc::c::CContext{&context};
            auto translation_unit = lcc::c::Parser::Parse(c_context, file);

            if (options.ast) translation_unit->print();
            if (options.stopat_syntax) return;

            /// Stop after sema if requested.
            if (options.stopat_sema) return;

            LCC_TODO();
        }

        /// Unknown.
        else {
            lcc::Diag::Fatal("Unrecognised input file type");
        }
    };

    auto configured_output_file_path = options.output_filepath;
    if (input_files.size() == 1) {
        std::string output_file_path = configured_output_file_path;
        if (output_file_path.empty())
            output_file_path = ConvertFileExtensionToOutputFormat(input_files[0]);

        GenerateOutputFile(input_files[0], output_file_path);
        if (context.has_error()) return 1;
        if (options.verbose) fmt::print("Generated output at {}\n", output_file_path);
    } else {
        if (not configured_output_file_path.empty()) {
            // TODO(local): you can, but only if we're planning to link these files; handle that later
            lcc::Diag::Fatal("cannot specify -o when generating multiple output files");
        }

        for (auto& input_file : input_files) {
            std::string input_file_path = input_file;
            std::string output_file_path = ConvertFileExtensionToOutputFormat(input_file_path);
            GenerateOutputFile(input_file, output_file_path);
        }

        // TODO(local): if we do linking, now's the time to link to the output file path, else a.out
        // std::string linked_output_file_path = configured_output_file_path;
    }

    return 0;
}
