#include <language_c/sema.hh>

#include <language_c/ast.hh>
#include <language_c/type.hh>

namespace lcc::language_c {

auto Sema::analyse(Node* node) -> Result<void> {
    // Don't analyse any node more than once.
    if (analysed.contains(node))
        return {};
    analysed.emplace(node);

    switch (node->kind()) {
        case NodeKind::Group: {
            auto g = (Group*) node;
            for (auto* c : g->constituents()) {
                if (auto d = analyse(c); not d)
                    return d;
            }
            return {};
        }
        case NodeKind::Block: {
            auto b = (Block*) node;
            for (auto* c : b->constituents()) {
                if (auto result = analyse(c); not result)
                    return result;
            }
            return {};
        }

        case NodeKind::Declaration: {
            auto d = (Declaration*) node;

            if (d->initialising_expression()) {
                defining = d;
                auto result = analyse(d->initialising_expression());
                if (not result) return result;
                defining = nullptr;
            }

            return {};

            // TODO: Ensure no duplicate declarations
        }

        case NodeKind::Return: {
            if (not defining)
                return Error(node->location(), "c/unexpected", "Encountered return outside of a function");

            if (defining->type()->kind() != TypeKind::Function)
                Diag::ICE("defining function is not a function");

            auto* r = (Return*) node;

            if (r->expression()) {
                if (((FunctionType*) defining->type())->return_type()->kind() == TypeKind::Void) {
                    return Error(
                        r->expression()->location(),
                        "c/return-value",
                        "Return statement MUST NOT return any value in a function that returns void",
                        *((FunctionType*) defining->type())->return_type()
                    );
                }

                return analyse(r->expression());
            } else {
                if (((FunctionType*) defining->type())->return_type()->kind() != TypeKind::Void)
                    return Error(
                        node->location(),
                        "c/return-value",
                        "Return statement MUST return a value in a function that returns non-void ({} returns {})",
                        defining->name(),
                        *((FunctionType*) defining->type())->return_type()
                    );
            }
            return {};
        }

        case NodeKind::IntegerLiteral:
            return {};

        case NodeKind::Invalid:
        case NodeKind::Count:
            break;
    }
    Diag::ICE("unreachable");
}

bool Sema::Analyse(Context* context, TranslationUnit& tu) {
    if (context->has_error())
        Diag::ICE("cannot analyse when context already has errors");

    if (not tu.tree)
        Diag::ICE("cannot analyse nullptr");

    Sema semantic{context, tu.tree};
    bool passed = semantic.analyse(semantic.root()).is_value();
    if (not passed) return false;

    if (semantic.root()->kind() == NodeKind::Block) {
        auto* block = (Block*) semantic.root();
        for (auto c : block->constituents()) {
            // declaration at top level: ensure it's exported
            if (c->kind() == NodeKind::Declaration) {
                auto d = (const Declaration*) c;
                if (d->type()->kind() == TypeKind::Function)
                    tu.functions.emplace_back(d);
                else tu.globals.emplace_back(d);
            }
        }
    }

    return true;
}

} // namespace lcc::language_c
