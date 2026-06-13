#include <language_c/driver.hh>

#include <language_c/ast.hh>
#include <language_c/irgen.hh>
#include <language_c/parser.hh>
#include <language_c/sema.hh>

#include <ast2dot/ast2dot.hh>

#include <fmt/format.h>

#include <filesystem>

namespace lcc::language_c {

namespace {
void print_stats(TranslationUnit& tu) {
    // TODO: More stats (total node count, number of included files, highest
    // AST depth encountered, etc)
    fmt::print(
        "C Translation Unit Stats:\n"
        "  Function Count: {}\n"
        "  Global Count: {}\n",
        tu.functions.size(),
        tu.globals.size()
    );
}
} // namespace

auto produce_module(Context* context, File& source) -> lcc::Module* {
    // TODO: Handle stopat_lex (print tokens)
    // This is effectively preprocessing only

    // Parse the file.
    auto mod = Parser::Parse(context, source);

    if (context->option_print_ast()) {
        if (mod.tree) {
            fmt::print("{}", *mod.tree);
            // TODO: I don't think most people will find it helpful to get an entire C
            // program's AST as a graph (noisy, large, etc). I think it would be
            // better to have the user be able to select which particular function
            // body they'd like to emit as a graph...
            auto dot = ast2dot::to_graph(*mod.tree);
            auto dot_path = std::filesystem::absolute(source.path().lexically_normal())
                                .replace_extension("dot");
            (void) File::Write(dot.data(), dot.size(), dot_path);
        } else fmt::print("No AST Produced...\n");
    }

    // The error condition is handled by the caller already.
    if (context->has_error()) return {};
    if (context->option_print_stats())
        print_stats(mod);
    if (context->option_stopat_syntax()) return {};

    // Perform semantic analysis.
    auto makes_sense = Sema::Analyse(context, mod);
    if (context->option_print_ast())
        fmt::print("\nAfter Sema:\n{}", *mod.tree);

    if (context->option_print_stats())
        print_stats(mod);

    // The error condition is handled by the caller already.
    if (not makes_sense or context->has_error()) return {};

    // Stop after sema if requested.
    if (context->option_stopat_sema()) return {};

    auto* ir = IRGen::Generate(context, mod);
    if (context->has_error()) return {};

    return ir;
}

} // namespace lcc::language_c
