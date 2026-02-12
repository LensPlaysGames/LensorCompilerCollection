#include <cli.hh>
#include <sarif.hh>
#include <version.hh>

#include <fmt/format.h>

#include <lcc/context.hh>
#include <lcc/diags.hh>
#include <lcc/format.hh>
#include <lcc/ir/module.hh>
#include <lcc/lcc-c.h>
#include <lcc/opt/opt.hh>
#include <lcc/target.hh>
#include <lcc/utils.hh>
#include <lcc/utils/platform.hh>
#include <lcc/utils/twocolumnlayouthelper.hh>

#include <glint/driver.hh>

#include <cstdio>  // fopen and friends
#include <cstdlib> // system
#include <cstring> // strerror
#include <filesystem>
#include <string>
#include <string_view>
#include <vector>

#if defined(__linux__) || defined(__unix__)
#    include <sys/wait.h>
#endif

void aluminium_handler() {
#if defined(LCC_PLATFORM_WINDOWS)
    // Windows
    std::system("start https://www.youtube.com/watch?v=dQw4w9WgXcQ");
#elif defined(__APPLE__)
    // Apple
    std::system("open https://www.youtube.com/watch?v=dQw4w9WgXcQ");
#elif defined(__linux__) || defined(__unix__)
    // Linux-ish
    std::system("xdg-open https://www.youtube.com/watch?v=dQw4w9WgXcQ");
#endif
}

/// Default target.
const lcc::Target* const default_target =
#if defined(LCC_PLATFORM_WINDOWS)
    lcc::Target::x86_64_windows;
#elif defined(__APPLE__) or defined(__linux__)
    lcc::Target::x86_64_linux;
#elif defined(__wasm)
    lcc::Target::x86_64_linux;
#else
#    error "Unsupported target; this is just for a silly default target, so feel free to update this"
#endif

/// Default format
const lcc::Format* const default_format
    = lcc::Format::gnu_as_att_assembly;

[[nodiscard]]
auto ConvertFileExtensionToOutputFormat(
    const lcc::Context& context,
    const std::string& path_string
) -> std::string {
    const char* replacement = "";
    switch (context.format()->format()) {
        case lcc::Format::INVALID:
            LCC_UNREACHABLE();

        case lcc::Format::LCC_IR:
        case lcc::Format::LCC_SSA_IR:
            replacement = ".lcc";
            break;

        case lcc::Format::WASM_TEXTUAL:
            replacement = ".wat";
            break;

        case lcc::Format::LLVM_TEXTUAL_IR:
            replacement = ".ll";
            break;

        case lcc::Format::GNU_AS_ATT_ASSEMBLY:
            replacement = ".s";
            break;

        case lcc::Format::ELF_OBJECT:
        case lcc::Format::COFF_OBJECT:
            replacement = ".o";
            break;
    }

    return std::filesystem::path{path_string}
        .replace_extension(replacement)
        .string();
}

void print_module_stats(lcc::Module* m) {
    if (not m) return;
    lcc::usz block_count{};
    lcc::usz inst_count{};
    lcc::usz most_inst_in_block{};
    lcc::usz most_block_in_function{};
    for (auto f : m->code()) {
        if (f->blocks().size() > most_block_in_function)
            most_block_in_function = f->blocks().size();

        block_count += f->blocks().size();

        for (auto b : f->blocks()) {
            if (b->instructions().size() > most_inst_in_block)
                most_inst_in_block = b->instructions().size();

            inst_count += b->instructions().size();
        }
    }
    fmt::print(
        "{}",
        TwoColumnLayoutHelper{
            {{"\nIR Module Stats:\n", ""},
             {"  Function Count:", fmt::format("{}\n", m->code().size())},
             {"  Block Count:", fmt::format("{}\n", block_count)},
             {"  Instruction Count:", fmt::format("{}\n", inst_count)},
             {"  Most Blocks in Function:", fmt::format("{}\n", most_block_in_function)},
             {"  Most Instructions in Block:", fmt::format("{}\n", most_inst_in_block)},
             {"  Global Count:", fmt::format("{}\n", m->vars().size())},
             {"  Extra Section Count:", fmt::format("{}\n", m->extra_sections().size())}}
        }.get()
    );
}

/// Common path after IR gen.
void EmitModule(
    lcc::Module* m,
    std::string_view input_file_path,
    std::string_view output_file_path,
    const cli::Options& options
) {
    if (not m)
        return;
    LCC_ASSERT(m->context());
    if (m->context()->has_error())
        return;

    if (m->context()->option_print_stats())
        print_module_stats(m);

    if (not options.optimisation_passes.empty()) {
        lcc::opt::RunPasses(m, options.optimisation_passes);
        if (options.ir)
            fmt::print("{}", m->as_lcc_ir(options.use_colour()));
    }

    if (options.ir)
        fmt::print("{}", m->as_lcc_ir(options.use_colour()));

    // NOTE: Only apply full optimisation if specific passes were not requested.
    if (options.optimisation and options.optimisation_passes.empty())
        lcc::opt::Optimise(m, int(options.optimisation));

    if (options.ir) {
        fmt::print(
            "\nAfter Optimisations:\n{}",
            m->as_lcc_ir(options.use_colour())
        );
    }

    m->lower();

    if (options.ir) {
        fmt::print(
            "\nAfter Lowering:\n{}",
            m->as_lcc_ir(options.use_colour())
        );
    }

    if (options.stopat_ir) return;

    m->emit(output_file_path);

    if (options.verbose) {
        fmt::print(
            "Generated output artifact from input component {} at {}\n",
            input_file_path,
            output_file_path
        );
    }
}

[[nodiscard]]
auto read_file_contents(
    const lcc::Context* context,
    std::string_view input_filepath
) -> lcc::Result<std::vector<char>> {
    std::filesystem::path path{input_filepath};
    // Don't use input_file since it may be moved from.
    auto path_str = path.lexically_normal().string();

    if (not std::filesystem::exists(path)) {
        return lcc::Diag::Error(
            "Input file does not exist: {}",
            path.lexically_normal().string()
        );
    }
    if (not std::filesystem::is_regular_file(path)) {
        return lcc::Diag::Error(
            "Input file exists, but is not a regular file: {}",
            path.lexically_normal().string()
        );
    }

    auto* f = fopen(path.string().data(), "rb");
    if (not f) {
        lcc::Diag::Fatal(
            "Could not open file at {}: {}",
            path.lexically_normal().string(),
            std::strerror(errno)
        );
        LCC_UNREACHABLE();
    }

    // Get size of file, restoring cursor to beginning.
    fseek(f, 0, SEEK_END);
    size_t fsize = size_t(ftell(f));
    fseek(f, 0, SEEK_SET);

    std::vector<char> contents{};
    contents.resize(fsize);
    auto nread = fread(contents.data(), 1, fsize, f);
    if (nread != fsize) {
        fclose(f);
        lcc::Diag::Fatal(
            "ERROR reading file {}\n"
            "    Got {} bytes, expected {}\n",
            path.lexically_normal().string(),
            nread,
            fsize
        );
        LCC_UNREACHABLE();
    }
    fclose(f);

    return contents;
}

int run_command(std::string_view s) {
    auto rc = std::system(s.data());
#if defined(__linux__) || defined(__unix__)
    rc = WEXITSTATUS(rc);
#endif
    return rc;
}

void do_link(
    lcc::Context& context,
    std::vector<std::string> input_paths,
    std::string_view output_path
) {
    if (input_paths.empty())
        lcc::Diag::ICE("link: No input paths");
    if (output_path.empty())
        lcc::Diag::ICE("link: Empty output path");

    // Get $CC (C compiler).
    const auto* cc = getenv("CC");
    if (not cc) cc = "gcc";

    // Build link command.
    auto link_command = fmt::format(
        "{} {} {} -o {}",
        cc,
        fmt::join(
            lcc::vws::transform(
                context.include_directories(),
                [](const auto s) {
                    return fmt::format("-I {}", s);
                }
            ),
            " "
        ),
        fmt::join(input_paths, " "),
        output_path
    );

    // Run link command.
    if (context.has_option("verbose"))
        fmt::print("Running link command: `{}`\n", link_command);

    auto rc = run_command(link_command);
    if (rc) {
        lcc::Diag::Fatal(
            "Failed link: `{}`",
            link_command
        );
    }
}

void do_run(
    lcc::Context& context,
    std::string_view binary_path
) {
    if (binary_path.empty())
        lcc::Diag::ICE("run: Empty binary path");

    // Run binary.
    // FIXME: Not sure this is the correct way to execute a generated binary
    // file on every system.
    auto command = fmt::format(
        "{}",
        lcc::fs::absolute(lcc::fs::path(binary_path))
    );

    if (context.has_option("verbose"))
        fmt::print("Running generated binary at `{}`\n", command);

    auto rc = run_command(command);
    fmt::print(
        "{}: Exited with status code {}\n",
        binary_path,
        rc
    );
}

// NOTE: Moves the input file, so, uhh, don't use that after passing it to
// this.
void GenerateOutputFile(
    lcc::Context& context,
    std::string& input_file,
    std::string_view output_file_path,
    const cli::Options& options
) {
    std::filesystem::path path{input_file};
    // Don't use input_file since it may be moved from.
    auto path_str = path.lexically_normal().string();

    if (not std::filesystem::exists(path)) {
        lcc::Diag::Error(
            "Input file does not exist: {}",
            path.lexically_normal().string()
        );
        return;
    }
    if (not std::filesystem::is_regular_file(path)) {
        lcc::Diag::Error(
            "Input file exists, but is not a regular file: {}",
            path.lexically_normal().string()
        );
        return;
    }

    if (
        context.has_option("verbose")
        and std::filesystem::exists(output_file_path)
    ) {
        lcc::Diag::Warning(
            "Generating output at {} overwrites existing file",
            output_file_path
        );
    }

    auto contents = read_file_contents(&context, path.string());
    if (not contents) return;

    auto& file = context.create_file(
        std::move(input_file),
        std::move(*contents)
    );

    if (
        options.language == "ir"
        or (options.language == "default" and path_str.ends_with(".lcc"))
    ) {
        auto mod = lcc::Module::Parse(&context, file);
        EmitModule(mod.get(), path_str, output_file_path, options);
        return;
    }

    if (
        options.language == "glint"
        or (options.language == "default" and path_str.ends_with(".g"))
    ) {
        auto* ir = lcc::glint::produce_module(&context, file);
        EmitModule(ir, path_str, output_file_path, options);
        return;
    }

    lcc::Diag::Fatal(
        "Unrecognised input file type\n"
        "Consider `-x <lang>' to treat the input file(s) as a specific language, or using a recognizable file extension.\n"
        "See help by passing `-h' for more info.\n"
    );
}

auto main(int argc, const char** argv) -> int {
    auto options = cli::parse(argc, argv);

    if (options.aluminium) {
        aluminium_handler();
        return 0;
    }

    /// Determine whether to use colours in the output.
    /// TODO: Enable colours in the console on Windows (for `cmd`).
    auto colour_opt = options.color;

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

    // Get target from "-t" or "--target" command line option, falling back to
    // default.
    auto* target = default_target;
    if (options.target == "default") {
        ;
    } else if (options.target == "x86_64_linux") {
        target = lcc::Target::x86_64_linux;
    } else if (options.target == "x86_64_windows") {
        target = lcc::Target::x86_64_windows;
    } else LCC_ASSERT(false, "Unhandled target");

    // Get format from command line option, falling back to default.
    auto* format = default_format;
    if (options.format == "default") {
        ;
    } else if (options.format == "ir" or options.format == "IR") {
        format = lcc::Format::lcc_ir;
    } else if (options.format == "ssa_ir") {
        format = lcc::Format::lcc_ssa_ir;
    } else if (options.format == "asm" || options.format == "gnu-as-att") {
        format = lcc::Format::gnu_as_att_assembly;
    } else if (options.format == "wat") {
        format = lcc::Format::wasm_textual;
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
    } else if (options.format == "llvm") {
        format = lcc::Format::llvm_textual_ir;
    } else LCC_ASSERT(false, "Unhandled format");

    lcc::Context context{
        target,
        format,
        lcc::Context::Options{
            (lcc::Context::OptionColour) options.use_colour(),
            options.print_stats,
            options.diag_backtrace,
            options.ast,
            options.stopat_lex,
            options.stopat_syntax,
            options.stopat_sema,
            options.mir,
            options.stopat_mir
        }
    };

    if (options.verbose)
        context.add_option("verbose");

    context.add_include_directory(".");
    for (const auto& directory : options.include_directories) {
        if (options.verbose)
            fmt::print("Added input directory: {}\n", directory);
        context.add_include_directory(directory);
    }

    for (const auto& option : options.frontend_options) {
        if (options.verbose)
            fmt::print("Added frontend option: {}\n", option);
        context.add_option(option);
    }

    auto configured_output_file_path = options.output_filepath;
    if (input_files.size() == 1) {
        std::string output_file_path = configured_output_file_path;
        // If no output file path was specified, generate one by converting the
        // input file path extension.
        // If linking is requested, always generate an output path, such as to
        // ensure /this/ stages output file path is different from the final
        // stages. The "configured" output file path---the one provided from the
        // CLI refers to a different file than this output.
        if (options.link or output_file_path.empty()) {
            output_file_path = ConvertFileExtensionToOutputFormat(
                context,
                input_files[0]
            );
        }

        GenerateOutputFile(context, input_files[0], output_file_path, options);

        if (options.emit_sarif) {
            std::string command_line{};
            for (auto i = 0; i < argc; ++i) {
                if (i > 0) command_line += ' ';
                command_line += argv[i];
            }
            auto s = as_sarif(context, command_line);
            auto sarif_path = std::filesystem::path{output_file_path}
                                  .replace_extension(".sarif");
            lcc::File::WriteOrTerminate(s.data(), s.size(), sarif_path);
        }

        if (context.has_error()) return 1;

        if (
            options.verbose
            and not (
                options.stopat_syntax
                or options.stopat_sema
                or options.stopat_ir
                or options.stopat_mir
            )
        ) fmt::print("Generated output at {}\n", output_file_path);

        if (options.link) {
            auto outpath
                = std::filesystem::path(output_file_path)
                      .replace_extension("")
                      .string();
            do_link(context, {output_file_path}, outpath);
            output_file_path = outpath;
        }

        if (
            options.verbose
            and not (
                options.stopat_syntax
                or options.stopat_sema
                or options.stopat_ir
                or options.stopat_mir
            )
        ) fmt::print("Generated final output at {}\n", output_file_path);

        // For `--run`
        configured_output_file_path = output_file_path;

    } else {
        if (not options.link and not configured_output_file_path.empty()) {
            lcc::Diag::Fatal(
                "Cannot specify -o when generating multiple output files (would overwrite the same file with every output).\n"
                "To generate a single output file from multiple inputs, link them together with -b.\n"
                "If you have a suggestion of how you think this should behave, let a developer know."
            );
        }
        if (options.link and configured_output_file_path.empty()) {
            lcc::Diag::Fatal(
                "Must specify -o when linking multiple input files."
            );
        }

        // TODO: Generate SARIF for each file as a separate SARIF "run"...
        std::vector<std::string> output_paths{};
        for (auto& input_file : input_files) {
            std::string input_file_path = input_file;
            std::string output_file_path = ConvertFileExtensionToOutputFormat(context, input_file_path);
            output_paths.emplace_back(output_file_path);
            GenerateOutputFile(context, input_file, output_file_path, options);
        }

        if (context.has_error())
            return 1;

        if (options.link)
            do_link(context, output_paths, configured_output_file_path);
    }

    if (options.run)
        do_run(context, configured_output_file_path);

    return 0;
}
