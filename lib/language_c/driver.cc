#include <language_c/driver.hh>

#include <language_c/irgen.hh>
#include <language_c/parser.hh>
#include <language_c/sema.hh>

namespace lcc::language_c {

auto produce_module(Context* context, File& source) -> lcc::Module* {
    // TODO: Handle stopat_lex (print tokens)

    // Parse the file.
    auto mod = Parser::Parse(context, source);
    // if (context->option_print_ast())
    //     mod.print(context->option_use_colour());
    // The error condition is handled by the caller already.
    if (context->has_error()) return {};
    if (context->option_stopat_syntax()) return {};

    // TODO: Handle print_stats

    // Perform semantic analysis.
    Sema::Analyse(context, mod);
    // if (context->option_print_ast()) {
    //     fmt::print("\nAfter Sema:\n");
    //     mod->print(context->option_use_colour());
    // }

    // TODO: Handle print_stats (after semantic analysis)

    // The error condition is handled by the caller already.
    if (context->has_error()) return {};

    // Stop after sema if requested.
    if (context->option_stopat_sema()) return {};

    auto* ir = IRGen::Generate(context, mod);
    if (context->has_error()) return {};

    return ir;
}

} // namespace lcc::language_c
