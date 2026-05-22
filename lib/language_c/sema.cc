#include <language_c/sema.hh>

#include <language_c/ast.hh>
#include <language_c/type.hh>

namespace lcc::language_c {

bool Sema::Analyse(Context* context, TranslationUnit& tu) {
    if (not tu.tree)
        Diag::ICE("cannot analyse nullptr");

    if (tu.tree->kind() != NodeKind::Block)
        Diag::ICE("expected block at top level");

    const auto* block = (const Block*) tu.tree;
    for (auto c : block->constituents()) {
        // declaration at top level: ensure it's exported
        if (c->kind() == NodeKind::Declaration) {
            auto d = (const Declaration*) c;
            if (d->type()->kind() == TypeKind::Function) {
                tu.functions.emplace_back(d);
            } else tu.globals.emplace_back(d);
        }
    }

    return true;
}

} // namespace lcc::language_c
