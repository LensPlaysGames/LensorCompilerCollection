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

    return ir;
};
} // namespace lcc::glint
