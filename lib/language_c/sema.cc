#include <language_c/sema.hh>

#include <language_c/ast.hh>
#include <language_c/parser.hh>
#include <language_c/type.hh>

namespace lcc::language_c {

Result<void> Sema::analyse_declaration(Declaration* d) {
    if (d->initialising_expression()) {
        defining = d;
        auto result = analyse(d->initialising_expression());
        if (not result) return result;

        // Function declaration returning void defined with empty block:
        // -> insert implicit return.
        if (
            d->type()->kind() == TypeKind::Function
            and d->initialising_expression()->kind() == NodeKind::Block
            and ((Block*) (d->initialising_expression()))->constituents().empty()
        ) {
            ((Block*) (d->initialising_expression()))
                ->_constituents
                .emplace_back(
                    new Return(nullptr, {})
                );
        }

        defining = nullptr;
    }

    // TODO: Ensure no duplicate definitions

    return {};
}
Result<void> Sema::analyse_binary(BinaryOperation* b) {
    switch (b->binary_operator()) {
        case TokenKind::OpPlus:
        case TokenKind::OpMinus:
        case TokenKind::OpAsterisk:
        case TokenKind::OpSlash:
        case TokenKind::OpPercent:
        case TokenKind::LeftParenthesis:
        case TokenKind::LeftSquareBracket:
            Diag::ICE("Unhandled binary operator `{}` (sema)", b->binary_operator());

        case TokenKind::Invalid:
        case TokenKind::Identifier:
        case TokenKind::Integer:
        case TokenKind::Fractional:
        case TokenKind::KwVoid:
        case TokenKind::KwInt:
        case TokenKind::KwReturn:
        case TokenKind::OpComma:
        case TokenKind::RightParenthesis:
        case TokenKind::RightSquareBracket:
        case TokenKind::LeftCurlyBrace:
        case TokenKind::RightCurlyBrace:
        case TokenKind::Semicolon:
        case TokenKind::Eof:
        case TokenKind::Count:
            Diag::ICE("Invalid binary operator `{}`", b->binary_operator());
    }
    return {};
}

Result<void> Sema::analyse_return(Return* r) {
    if (not defining or defining->type()->kind() != TypeKind::Function) {
        return Error(
            r->location(),
            "c/unexpected",
            "Encountered return outside of a function"
        );
    }

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
                r->location(),
                "c/return-value",
                "Return statement MUST return a value in a function that returns non-void ({} returns {})",
                defining->name(),
                *((FunctionType*) defining->type())->return_type()
            );
    }
    return {};
}

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

        case NodeKind::Declaration:
            return analyse_declaration((Declaration*) node);

        case NodeKind::Return:
            return analyse_return((Return*) node);

        case NodeKind::BinaryOperation:
            return analyse_binary((BinaryOperation*) node);

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
