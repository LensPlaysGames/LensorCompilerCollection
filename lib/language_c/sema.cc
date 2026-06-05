#include <language_c/sema.hh>

#include <language_c/ast.hh>
#include <language_c/parser.hh>
#include <language_c/type.hh>

namespace lcc::language_c {

void Sema::update_type(Node* n, Type* t) {
    _type_cache[n] = t;
}

Type* Sema::type_of(const Node* n) {
    if (not n) Diag::ICE("nullptr argument");

    // Only get the type of a node once.
    if (_type_cache.contains(n))
        return _type_cache.at(n);

    Type* out{nullptr};
    switch (n->kind()) {
        case NodeKind::BinaryOperation: {
            auto* b = (BinaryOperation*) n;
            switch (b->binary_operator()) {
                case TokenKind::OpPlus:
                case TokenKind::OpMinus:
                case TokenKind::OpAsterisk:
                case TokenKind::OpSlash:
                case TokenKind::OpPercent: {
                    // FIXME: Type promotion, or something.
                    out = type_of(b->lhs());
                    break;
                }

                case TokenKind::LeftSquareBracket:
                    Diag::ICE("sema::type_of subscript");

                case TokenKind::OpEqual:
                case TokenKind::OpLessThan:
                case TokenKind::OpGreaterThan:
                case TokenKind::OpDoublePipe:
                case TokenKind::OpDoubleAmpersand:
                case TokenKind::OpExclamation:
                case TokenKind::OpDot:
                case TokenKind::OpArrow:
                case TokenKind::OpPlusPlus:
                case TokenKind::OpMinusMinus:
                case TokenKind::OpCaret:
                case TokenKind::OpPipe:
                case TokenKind::OpAmpersand:
                case TokenKind::OpTilde:
                case TokenKind::OpShiftLeft:
                case TokenKind::OpShiftRight:
                case TokenKind::OpDoubleEqual:
                case TokenKind::OpLessThanEqual:
                case TokenKind::OpGreaterThanEqual:
                case TokenKind::OpExclamationEqual:
                case TokenKind::OpPlusEqual:
                case TokenKind::OpMinusEqual:
                case TokenKind::OpAsteriskEqual:
                case TokenKind::OpSlashEqual:
                case TokenKind::OpPercentEqual:
                case TokenKind::OpCaretEqual:
                case TokenKind::OpPipeEqual:
                case TokenKind::OpAmpersandEqual:
                case TokenKind::OpShiftLeftEqual:
                case TokenKind::OpShiftRightEqual:
                    Diag::ICE("Handle {} typeof", b->binary_operator());

                case TokenKind::Invalid:
                case TokenKind::Identifier:
                case TokenKind::Integer:
                case TokenKind::Fractional:
                case TokenKind::OpComma:
                case TokenKind::KwVoid:
                case TokenKind::KwInt:
                case TokenKind::KwReturn:
                case TokenKind::KwSizeof:
                case TokenKind::KwAlignof:
                case TokenKind::LeftParenthesis:
                case TokenKind::RightParenthesis:
                case TokenKind::RightSquareBracket:
                case TokenKind::LeftCurlyBrace:
                case TokenKind::RightCurlyBrace:
                case TokenKind::Semicolon:
                case TokenKind::Eof:
                case TokenKind::Count:
                    Diag::ICE("Not a binary operator");
            }
        } break;

        case NodeKind::Declaration:
            out = ((Declaration*) n)->type();
            break;

        case NodeKind::NameReference: {
            Diag::ICE("Handle name-reference typeof...");
        }

        case NodeKind::IntegerLiteral:
            out = new IntType(n->location());
            break;

        case NodeKind::Invalid:
        case NodeKind::Group:
        case NodeKind::Block:
        case NodeKind::Return:
        case NodeKind::Count:
            out = new VoidType(n->location());
            break;
    }

    if (not out)
        Diag::ICE("no node is allowed a null type");

    return out;
}

Result<void> Sema::analyse_declaration(Declaration*& d) {
    if (d->initialising_expression()) {
        defining = d;
        auto result = analyse(d->_initialising_expression);
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

Result<void> Sema::analyse_binary(BinaryOperation*& b) {
    switch (b->binary_operator()) {
        case TokenKind::OpPlus:
        case TokenKind::OpMinus:
        case TokenKind::OpAsterisk:
        case TokenKind::OpSlash:
        case TokenKind::OpPercent:
        case TokenKind::OpCaret:
        case TokenKind::OpPipe:
        case TokenKind::OpAmpersand:
        case TokenKind::OpShiftLeft:
        case TokenKind::OpShiftRight: {
            // FIXME: This is not accurate
            if (type_of(b->lhs())->kind() != TypeKind::Int) {
                return Error(
                    b->lhs()->location(),
                    "c/type-mismatch",
                    "Only integral types are allowed in arithmetic for now, sorry"
                );
            }

            if (
                type_of(b->lhs())->kind() != type_of(b->rhs())->kind()
            ) {
                return Error(
                    b->lhs()->location(),
                    "c/type-mismatch",
                    "Arithmetic must be performed on like types ({} and {} are different)",
                    *type_of(b->lhs()),
                    *type_of(b->rhs())
                );
            }

        } break;

        case TokenKind::OpLessThan:
        case TokenKind::OpGreaterThan:
        case TokenKind::OpDoublePipe:
        case TokenKind::OpDoubleAmpersand:
        case TokenKind::OpDoubleEqual:
            Diag::ICE("Handle binary logical operator `{}` (sema)", b->binary_operator());

        case TokenKind::OpEqual:
            // TODO: Error if lhs isn't lvalue
            // TODO: Error if type of rhs isn't convertible to type of lhs

        case TokenKind::OpDot:
            // TODO: Error if lhs isn't a structure

        case TokenKind::OpArrow:
            // TODO: Error if lhs isn't a structure

        case TokenKind::LeftSquareBracket:
            Diag::ICE("Unhandled binary operator `{}` (sema)", b->binary_operator());

        case TokenKind::OpLessThanEqual:
        case TokenKind::OpGreaterThanEqual:
        case TokenKind::OpExclamationEqual:
        case TokenKind::OpPlusEqual:
        case TokenKind::OpMinusEqual:
        case TokenKind::OpAsteriskEqual:
        case TokenKind::OpSlashEqual:
        case TokenKind::OpPercentEqual:
        case TokenKind::OpCaretEqual:
        case TokenKind::OpPipeEqual:
        case TokenKind::OpAmpersandEqual:
        case TokenKind::OpShiftLeftEqual:
        case TokenKind::OpShiftRightEqual: {
            b = new BinaryOperation(
                TokenKind::Assign,
                b->lhs(),
                new BinaryOperation(
                    b->binary_operator(),
                    b->lhs(),
                    b->rhs(),
                    b->location()
                ),
                b->location()
            );
            // NOTE: We don't technically *have to* recurse here.
            // I just don't want to muddy the control flow for this one case.
            return analyse_binary(b);
        }

        case TokenKind::Invalid:
        case TokenKind::Identifier:
        case TokenKind::Integer:
        case TokenKind::Fractional:
        case TokenKind::KwVoid:
        case TokenKind::KwInt:
        case TokenKind::KwReturn:
        case TokenKind::KwSizeof:
        case TokenKind::KwAlignof:
        case TokenKind::OpPlusPlus:
        case TokenKind::OpMinusMinus:
        case TokenKind::OpComma:
        case TokenKind::OpExclamation:
        case TokenKind::OpTilde:
        case TokenKind::LeftParenthesis:
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

Result<void> Sema::analyse_return(Return*& r) {
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

        return analyse(r->_expression);
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

Result<void> Sema::analyse_name_reference(NameReference*& n) {
    return Error(n->location(), "c/todo", "TODO: Analyse name-reference: {}", n->name());
}

auto Sema::analyse(Node*& node) -> Result<void> {
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

        case NodeKind::NameReference:
            return analyse_name_reference(*(NameReference**) &node);

        case NodeKind::Declaration:
            return analyse_declaration(*(Declaration**) &node);

        case NodeKind::Return:
            return analyse_return(*(Return**) &node);

        case NodeKind::BinaryOperation:
            return analyse_binary(*(BinaryOperation**) &node);

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
    bool passed = semantic.analyse(semantic._root).is_value();
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
