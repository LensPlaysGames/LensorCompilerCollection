#include <glint/driver.hh>

#include <lcc/context.hh>
#include <lcc/file.hh>
#include <lcc/ir/module.hh>
#include <lcc/utils/twocolumnlayouthelper.hh>

#include <glint/ir_gen.hh>
#include <glint/parser.hh>
#include <glint/sema.hh>

namespace lcc::glint {

auto module_parsed_stats(Module& m) {
    // TODO: Measure max depth of AST
    // TODO: Measure longest mangled name
    // TODO: Measure longest string

    return fmt::format(
        "{}",
        TwoColumnLayoutHelper{
            {{"Glint Module Stats:", "\n"},
             {"  Node Count:", fmt::format("{}\n", m.nodes.size())},
             {"  Type Count:", fmt::format("{}\n", m.types.size())},
             {"  Scope Count:", fmt::format("{}\n", m.scopes.size())},
             {"  String Count:", fmt::format("{}\n", m.strings.size())},
             {"  Is A Module:", fmt::format("{}\n", m.is_module())}}
        }.get()
    );
}

auto produce_module(Context* context, File& source) -> lcc::Module* {
    if (context->option_stopat_lex()) {
        auto tokens = Parser::GetTokens(context, source);

        fmt::print("KIND (LINE:COLUMN)\n");
        for (const auto& t : tokens) {
            auto info = t.location.seek_line_column(context);
            fmt::print("{} ({}:{})\n", ToString(t.kind), info.line, info.col);
        }

        return {};
    }

    // Parse the file.
    auto mod = Parser::Parse(context, source);
    if (context->option_print_ast() and mod) mod->print(context->option_use_colour());
    // The error condition is handled by the caller already.
    if (context->has_error()) return {};
    if (context->option_stopat_syntax()) return {};

    if (context->option_print_stats()) {
        fmt::print("{}", module_parsed_stats(*mod));
    }

    // Perform semantic analysis.
    lcc::glint::Sema::Analyse(
        context,
        *mod,
        context->option_use_colour()
    );
    if (context->option_print_ast()) {
        fmt::print("\nAfter Sema:\n");
        mod->print(context->option_use_colour());
    }

    if (context->option_print_stats()) {
        fmt::print("\nAfter Sema:\n");
        fmt::print("{}", module_parsed_stats(*mod));
    }

    // The error condition is handled by the caller already.
    if (context->has_error()) return {};

    // Stop after sema if requested.
    if (context->option_stopat_sema()) return {};

    auto* ir = IRGen::Generate(context, *mod);
    if (context->has_error()) return {};

    if (mod->is_module()) {
        Section metadata_blob{};
        metadata_blob.name = metadata_section_name;
        metadata_blob.contents() = mod->serialise();

        // TODO: If we are given a CLI option to save module metadata(s) in a
        // separate file in a specific directory, do that. For now just always do
        // it.
        fs::path p{"./"};
        p.replace_filename(mod->name());
        p.replace_extension(metadata_file_extension);
        (void) File::Write(
            metadata_blob.contents().data(),
            metadata_blob.contents().size(),
            p
        );

        if (context->has_option("ast-binary")) {
            // Print bytes like hexdump, kinda
            const usz size = metadata_blob.contents().size();
            usz i = 0;
            if (size > 15) {
                for (; i < size - 16; i += 16) {
                    fmt::print(
                        "      {:02x} {:02x} {:02x} {:02x} {:02x} {:02x} {:02x} {:02x}  {:02x} {:02x} {:02x} {:02x} {:02x} {:02x} {:02x} {:02x}",
                        metadata_blob.contents()[i],
                        metadata_blob.contents()[i + 1],
                        metadata_blob.contents()[i + 2],
                        metadata_blob.contents()[i + 3],
                        metadata_blob.contents()[i + 4],
                        metadata_blob.contents()[i + 5],
                        metadata_blob.contents()[i + 6],
                        metadata_blob.contents()[i + 7],
                        metadata_blob.contents()[i + 8],
                        metadata_blob.contents()[i + 9],
                        metadata_blob.contents()[i + 10],
                        metadata_blob.contents()[i + 11],
                        metadata_blob.contents()[i + 12],
                        metadata_blob.contents()[i + 13],
                        metadata_blob.contents()[i + 14],
                        metadata_blob.contents()[i + 15]
                    );

                    fmt::print("      |");
                    // Loop over 16 bytes starting at `i`.
                    for (usz index = i; index < i + 16; ++index) {
                        char c = char(metadata_blob.contents()[index]);
                        if (c < ' ' or c > '~') c = '.';
                        fmt::print("{}", c);
                    }
                    fmt::print("|\n");
                }
            }

            // Last line
            if (i < size) {
                fmt::print("      ");
                for (usz index = i; index < i + 16; ++index) {
                    // If inbounds, print byte, otherwise print blank space to keep alignment.
                    if (index < size) fmt::print("{:02x}", metadata_blob.contents()[index]);
                    else fmt::print("  ");
                    // Space after every one except for the last.
                    if (index - i != 15) fmt::print(" ");
                    // Inbetween the 7th and 8th (0-based), there is an extra space.
                    if (index - i == 7) fmt::print(" ");
                }
                fmt::print("      |");
                for (usz index = i; index < i + 16; ++index) {
                    if (index < size) {
                        char c = char(metadata_blob.contents()[index]);
                        if (c < ' ' or c > '~') c = '.';
                        fmt::print("{}", c);
                    } else fmt::print(".");
                }
                fmt::print("|\n");
            }
        }

        // Tell LCC to emit this section in the output code.
        ir->add_extra_section(metadata_blob);
    }

    return ir;
};
} // namespace lcc::glint
