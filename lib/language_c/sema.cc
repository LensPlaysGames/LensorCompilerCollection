#include <language_c/sema.hh>

#include <language_c/ast.hh>
#include <language_c/parser.hh>
#include <language_c/type.hh>

#include <lcc/utils/macros.hh>
#include <lcc/utils/result.hh>

#include <string_view>

namespace lcc::language_c {

void Sema::update_type(const Node* n, Type* t) {
    _type_cache[n] = t;
}

auto Sema::type_of(const Node* n) -> Result<Type*> {
    if (not n) Diag::ICE("nullptr argument");

    // Only get the type of a node once.
    if (_type_cache.contains(n))
        return _type_cache.at(n);

    /** (!) -- Assign Before Use! **/
    auto out = Result<Type*>::Null();

    switch (n->kind()) {
        case NodeKind::BinaryOperation: {
            auto* b = (BinaryOperation*) n;
            // TODO: usual arithmetic conversions | C23 § 6.3.1.8 (promotion)
            switch (b->binary_operator()) {
                case TokenKind::OpPlus: {
                    auto lhs_type = type_of(b->lhs());
                    if (not lhs_type) return lhs_type.diag();
                    auto rhs_type = type_of(b->lhs());
                    if (not rhs_type) return rhs_type.diag();
                    // If one of the operands is a pointer, the other operand must be of
                    // integer type. Otherwise, both operands must be of arithmetic type.
                    if (
                        (lhs_type->kind() == TypeKind::Pointer and not type_is_integral(*rhs_type))
                        or (rhs_type->kind() == TypeKind::Pointer and not type_is_integral(*lhs_type))
                        or (type_is_arithmetic(*lhs_type) and not type_is_arithmetic(*rhs_type))
                    ) {
                        return Error(
                            b->location(),
                            "c/type-mismatch",
                            "For addition, either both operands shall have arithmetic type,"
                            " or one operand shall be a pointer to a complete object type and the other shall have integer type"
                            " | C23 § 6.5.7"
                        );
                    }
                    out = type_of(b->lhs());
                } break;

                // "For subtraction, one of the following shall hold:
                //  — both operands have arithmetic type;
                //  — both operands are pointers to qualified or unqualified versions of
                //    compatible complete object types; or
                //  — the left operand is a pointer to a complete object type and the right
                //    operand has integer type."
                case TokenKind::OpMinus:
                // For multiplicative operations (star, slash, percent)
                // "Each of the operands shall have arithmetic type. The operands of the %
                //  operator shall have integer type."
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
                case TokenKind::String:
                case TokenKind::OpComma:
                case TokenKind::KwVoid:
                case TokenKind::KwBool:
                case TokenKind::KwChar:
                case TokenKind::KwShort:
                case TokenKind::KwInt:
                case TokenKind::KwLong:
                case TokenKind::KwReturn:
                case TokenKind::KwSizeof:
                case TokenKind::KwAlignof:
                case TokenKind::KwConst:
                case TokenKind::KwVolatile:
                case TokenKind::KwRestrict:
                case TokenKind::KwAtomic:
                case TokenKind::KwConstexpr:
                case TokenKind::KwAuto:
                case TokenKind::KwExtern:
                case TokenKind::KwRegister:
                case TokenKind::KwStatic:
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

        case NodeKind::UnaryOperation: {
            auto* u = (UnaryOperation*) n;
            auto operand_type = type_of(u->operand());
            if (not operand_type) return operand_type;
            u->_operand_type = *operand_type;
            switch (u->unary_operator()) {
                // Dereference
                case TokenKind::OpAsterisk: {
                    if (operand_type->kind() != TypeKind::Pointer) {
                        auto e = Error(
                            u->operand()->location(),
                            "c/type-mismatch",
                            "The operand of the unary `*` operator shall have pointer type | C23 § 6.5.4.2"
                        );
                        e.attach(Note(
                            u->operand()->location(),
                            "c/type-mismatch",
                            "Type is {}",
                            **operand_type
                        ));
                        fmt::print("{}", *(Node*) u);
                        return e;
                    }
                    out = ((PointerType*) *operand_type)->element_type();
                    break;
                }

                // Address-of
                case TokenKind::OpAmpersand: {
                    out = new (tu) PointerType(
                        *operand_type,
                        u->location()
                    );
                } break;

                case TokenKind::OpPlus:
                case TokenKind::OpMinus:
                case TokenKind::OpPlusPlus:
                case TokenKind::OpMinusMinus: {
                    out = *operand_type;
                } break;

                case TokenKind::OpTilde: {
                    if (operand_type->kind() != TypeKind::Int) {
                        return Error(
                            u->location(),
                            "c/type-mismatch",
                            "The operand of the unary `~` operator shall have integer type | C23 § 6.5.4.3"
                        );
                    }
                    out = *operand_type;
                } break;

                case TokenKind::OpExclamation: {
                    // TODO: The operand of the unary `!` operator shall have scalar type | C23 § 6.5.4.3
                    out = new (tu) BoolType(u->location());
                } break;

                case TokenKind::Invalid:
                case TokenKind::Identifier:
                case TokenKind::Integer:
                case TokenKind::Fractional:
                case TokenKind::String:
                case TokenKind::KwVoid:
                case TokenKind::KwBool:
                case TokenKind::KwChar:
                case TokenKind::KwShort:
                case TokenKind::KwInt:
                case TokenKind::KwLong:
                case TokenKind::KwReturn:
                case TokenKind::KwSizeof:
                case TokenKind::KwAlignof:
                case TokenKind::KwConst:
                case TokenKind::KwVolatile:
                case TokenKind::KwRestrict:
                case TokenKind::KwAtomic:
                case TokenKind::KwConstexpr:
                case TokenKind::KwAuto:
                case TokenKind::KwExtern:
                case TokenKind::KwRegister:
                case TokenKind::KwStatic:
                case TokenKind::OpEqual:
                case TokenKind::OpLessThan:
                case TokenKind::OpGreaterThan:
                case TokenKind::OpDoublePipe:
                case TokenKind::OpDoubleAmpersand:
                case TokenKind::OpSlash:
                case TokenKind::OpPercent:
                case TokenKind::OpComma:
                case TokenKind::OpDot:
                case TokenKind::OpArrow:
                case TokenKind::OpCaret:
                case TokenKind::OpPipe:
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
                case TokenKind::LeftParenthesis:
                case TokenKind::RightParenthesis:
                case TokenKind::LeftSquareBracket:
                case TokenKind::RightSquareBracket:
                case TokenKind::LeftCurlyBrace:
                case TokenKind::RightCurlyBrace:
                case TokenKind::Semicolon:
                case TokenKind::Eof:
                case TokenKind::Count:
                    Diag::ICE("Not a unary operator");
            }
            // CheckLevel: Debug
            if (not out) Diag::ICE("Unary operator typeof() did not set type...");
        } break;

        case NodeKind::Call: {
            out = type_of(((Call*) n)->callee());
            if (not out) return out.diag();
            if (
                out->kind() == TypeKind::Pointer
                and ((PointerType*) *out)->element_type()
                and ((PointerType*) *out)->element_type()->kind() == TypeKind::Function
            ) out = ((PointerType*) *out)->element_type();
            if (out->kind() != TypeKind::Function)
                Diag::ICE("call of non-function type");
            // The result type of a call expression is the called function type's
            // return type.
            out = ((FunctionType*) *out)->return_type();
        } break;

        case NodeKind::Declaration:
            out = ((Declaration*) n)->type();
            break;

        case NodeKind::IntegerLiteral:
            out = new (tu) IntType(true, n->location());
            break;

        case NodeKind::ArrayLiteral: {
            auto* a = (ArrayLiteral*) n;
            LCC_ASSERT(not a->elements().empty());
            // Attempt to determine element type automatically, if not already set.
            if (not a->_element_type) {
                auto element_type = type_of(a->elements().at(0));
                if (not element_type) return element_type.diag();
                a->_element_type = *element_type;
            }
            for (const auto* element : a->elements()) {
                // Integer literal convertible to any integral type...
                if (
                    element->kind() == NodeKind::IntegerLiteral
                    and type_is_integral(a->_element_type)
                ) continue;
                auto element_type = type_of(element);
                if (not element_type) return element_type.diag();
                // TODO: Better type comparison
                if (element_type->kind() != a->_element_type->kind()) {
                    return Error(
                        element->location(),
                        "c/invalid-literal",
                        "Array literal elements must all be of the same expected type, {}",
                        *a->_element_type
                    );
                }
            }
            out = new (tu) ArrayType(
                a->_element_type,
                new (tu) IntegerLiteral(a->elements().size(), {}),
                n->location()
            );
        } break;

        // We just sort of *don't*...
        // We *could* walk scopes and find the type of the decl, but, that's
        // basically the entire sema for a name reference, so it'd suck to do it
        // twice for no reason. If you want the type of what a name reference
        // refers to, you should analyse it and get the type of the resulting
        // declaration.
        case NodeKind::NameReference:
        case NodeKind::Invalid:
        case NodeKind::Group:
        case NodeKind::Block:
        case NodeKind::Return:
        case NodeKind::Count:
            out = new (tu) VoidType(n->location());
            break;
    }

    if (not out or out.value() == nullptr)
        Diag::ICE("no node is allowed a null type");

    update_type(n, *out);
    return out;
}

Result<void> Sema::analyse_declaration(Declaration*& d) {
    if (d->initialising_expression()) {
        // Function declaration returning void defined with empty block:
        // -> insert implicit return.
        if (
            d->type()->kind() == TypeKind::Function
            and d->initialising_expression()->kind() == NodeKind::Block
            and ((Block*) (d->initialising_expression()))->constituents().empty()
        ) {
            ((Block*) (d->initialising_expression()))
                ->_constituents.emplace_back(
                    new (tu) Return(nullptr, {})
                );
        }
    }

    // TODO: Ensure no duplicate definitions
    // TODO: Warn on shadowing definitions

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
                    **type_of(b->lhs()),
                    **type_of(b->rhs())
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
            b = new (tu) BinaryOperation(
                TokenKind::Assign,
                b->lhs(),
                new (tu) BinaryOperation(
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
        case TokenKind::String:
        case TokenKind::KwVoid:
        case TokenKind::KwBool:
        case TokenKind::KwChar:
        case TokenKind::KwShort:
        case TokenKind::KwInt:
        case TokenKind::KwLong:
        case TokenKind::KwReturn:
        case TokenKind::KwSizeof:
        case TokenKind::KwAlignof:
        case TokenKind::KwConst:
        case TokenKind::KwVolatile:
        case TokenKind::KwRestrict:
        case TokenKind::KwAtomic:
        case TokenKind::KwConstexpr:
        case TokenKind::KwAuto:
        case TokenKind::KwExtern:
        case TokenKind::KwRegister:
        case TokenKind::KwStatic:
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

bool Sema::type_is_arithmetic(const Type* t) {
    switch (t->kind()) {
        case TypeKind::Bool:
        case TypeKind::Char:
        case TypeKind::Short:
        case TypeKind::Int:
        case TypeKind::Long:
        case TypeKind::LongLong:
            return true;

        case TypeKind::Void:
        case TypeKind::Pointer:
        case TypeKind::Function:
        case TypeKind::Array:
            return false;

        case TypeKind::Invalid:
        case TypeKind::Count:
            break;
    }
    Diag::ICE("unreachable");
}

bool Sema::type_is_integral(const Type* t) {
    switch (t->kind()) {
        case TypeKind::Bool:
        case TypeKind::Char:
        case TypeKind::Short:
        case TypeKind::Int:
        case TypeKind::Long:
        case TypeKind::LongLong:
            return true;

        case TypeKind::Void:
        case TypeKind::Pointer:
        case TypeKind::Function:
        case TypeKind::Array:
            return false;

        case TypeKind::Invalid:
        case TypeKind::Count:
            break;
    }
    Diag::ICE("unreachable");
}

Result<void> Sema::analyse_unary(UnaryOperation*& u) {
    auto operand_type = type_of(u->operand());
    if (not operand_type) return operand_type.diag();
    switch (u->unary_operator()) {
        case TokenKind::OpPlus:
        case TokenKind::OpMinus: {
            // Operand type must be of an arithmetic type.
            if (not type_is_arithmetic(*operand_type)) {
                return Error(
                    u->operand()->location(),
                    "c/type-mismatch",
                    "The operand of the unary `+` or `-` operator shall have arithmetic type | C23 § 6.5.4.3"
                );
            }
        } break;

        case TokenKind::OpAsterisk: {
            // Handled in type_of()...
        } break;

        case TokenKind::OpTilde:
        case TokenKind::OpPlusPlus:
        case TokenKind::OpMinusMinus:
        case TokenKind::OpExclamation:
        case TokenKind::OpAmpersand:
            Diag::ICE("Handle unary operator {}", u->unary_operator());

        case TokenKind::Invalid:
        case TokenKind::Identifier:
        case TokenKind::Integer:
        case TokenKind::Fractional:
        case TokenKind::String:
        case TokenKind::KwVoid:
        case TokenKind::KwBool:
        case TokenKind::KwChar:
        case TokenKind::KwShort:
        case TokenKind::KwInt:
        case TokenKind::KwLong:
        case TokenKind::KwReturn:
        case TokenKind::KwSizeof:
        case TokenKind::KwAlignof:
        case TokenKind::KwConst:
        case TokenKind::KwVolatile:
        case TokenKind::KwRestrict:
        case TokenKind::KwAtomic:
        case TokenKind::KwConstexpr:
        case TokenKind::KwAuto:
        case TokenKind::KwExtern:
        case TokenKind::KwRegister:
        case TokenKind::KwStatic:
        case TokenKind::OpEqual:
        case TokenKind::OpLessThan:
        case TokenKind::OpGreaterThan:
        case TokenKind::OpDoublePipe:
        case TokenKind::OpDoubleAmpersand:
        case TokenKind::OpSlash:
        case TokenKind::OpPercent:
        case TokenKind::OpComma:
        case TokenKind::OpDot:
        case TokenKind::OpArrow:
        case TokenKind::OpCaret:
        case TokenKind::OpPipe:
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
        case TokenKind::LeftParenthesis:
        case TokenKind::RightParenthesis:
        case TokenKind::LeftSquareBracket:
        case TokenKind::RightSquareBracket:
        case TokenKind::LeftCurlyBrace:
        case TokenKind::RightCurlyBrace:
        case TokenKind::Semicolon:
        case TokenKind::Eof:
        case TokenKind::Count:
            Diag::ICE("unreachable");
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

Result<Declaration*> Sema::analyse_name_reference(NameReference*& n) {
    if (not n) Diag::ICE("nullptr argument");
    auto* s = n->within_scope();
    const std::string_view name{n->_name};
    if (not s or name.empty()) {
        Diag::ICE(
            "Invalid NameReference: scope={},name=`{}`",
            fmt::ptr(s),
            name
        );
    }

    while (s and not s->declarations.contains(name))
        s = s->parent;

    if (not s)
        return Error(n->location(), "c/symbol-resolution", "Dafuq is dis? `{}`", name);

    // Update name reference scope to the scope that actually contains it
    n->_within_scope = s;

    LCC_ASSERT(s->declarations.contains(name));
    return s->declarations.at(name);
}

Result<void> Sema::analyse_call(Call*& c) {
    if (not c) Diag::ICE("nullptr argument");
    if (not c->callee()) Diag::ICE("malformed call");

    // Analyse callee
    if (
        auto callee_result = analyse(c->_callee);
        not callee_result
    ) return callee_result.diag();

    switch (c->callee()->kind()) {
        case NodeKind::Declaration: {
            auto* d = (Declaration*) c->callee();
            if (d->type()->kind() != TypeKind::Function) {
                return Error(
                    c->location(),
                    "c/type-mismatch",
                    "Cannot call non-function type: {}",
                    *d->type()
                );
            }
            auto* f = (FunctionType*) d->type();

            if (c->arguments().size() != f->parameters().size()) {
                return Error(
                    c->location(),
                    "c/argument-count",
                    "Invalid number of arguments to function"
                );
            }
            for (auto [argument, parameter] : vws::zip(c->arguments(), f->parameters())) {
                // Analyse argument
                if (
                    auto argument_result = analyse(argument);
                    not argument_result
                ) return argument_result.diag();

                // TODO: Better type comparison...
                if (type_of(argument)->kind() != parameter.type->kind()) {
                    auto e = Error(
                        argument->location(),
                        "c/type-mismatch",
                        "Invalid argument type {} (expected {})",
                        **type_of(argument),
                        *parameter.type
                    );
                    e.attach(Note(d->location(), "", "Declared here"));
                    return e;
                }
            }
        } break;

        case NodeKind::Invalid:
        case NodeKind::Group:
        case NodeKind::Block:
        case NodeKind::NameReference:
        case NodeKind::IntegerLiteral:
        case NodeKind::ArrayLiteral:
        case NodeKind::Return:
        case NodeKind::BinaryOperation:
        case NodeKind::UnaryOperation:
        case NodeKind::Call:
        case NodeKind::Count:
            Diag::ICE("unreachable");
    }
    return {};
}

auto Sema::analyse(Node*& node) -> Result<void> {
    // Don't analyse any node more than once.
    if (analysed.contains(node))
        return {};
    analysed.emplace(node);

    // Previsit declarations such that we know the type ahead of time, and
    // know when a variable is used in it's own initialiser.
    if (node->kind() == NodeKind::Declaration) {
        auto* d = (Declaration*) node;

        auto type_makes_sense = type_of(node);
        if (not type_makes_sense) return type_makes_sense.diag();

        if (d->initialising_expression())
            defining = d;
    }

    // Visit children.
    for (auto** child : node->children_ref()) {
        auto child_makes_sense = analyse(*child);
        if (not child_makes_sense) return child_makes_sense.diag();
    }

    if (node->kind() == NodeKind::Declaration) {
        if (((Declaration*) node)->initialising_expression())
            defining = nullptr;
    }

    // Type analysis
    auto type_makes_sense = type_of(node);
    if (not type_makes_sense) return type_makes_sense.diag();

    switch (node->kind()) {
        case NodeKind::Group: {
            auto g = (Group*) node;
            for (auto*& c : g->constituents()) {
                if (auto d = analyse(c); not d)
                    return d;
            }
            return {};
        }
        case NodeKind::Block: {
            auto b = (Block*) node;
            for (auto*& c : b->constituents()) {
                if (auto result = analyse(c); not result)
                    return result;
            }
            return {};
        }

        case NodeKind::Call:
            return analyse_call(*(Call**) &node);

        case NodeKind::NameReference: {
            auto resolved = analyse_name_reference(*(NameReference**) &node);
            if (not resolved) return resolved.diag();
            node = *resolved;
            return {};
        }

        case NodeKind::Declaration:
            return analyse_declaration(*(Declaration**) &node);

        case NodeKind::Return:
            return analyse_return(*(Return**) &node);

        case NodeKind::UnaryOperation:
            return analyse_unary(*(UnaryOperation**) &node);

        case NodeKind::BinaryOperation:
            return analyse_binary(*(BinaryOperation**) &node);

        case NodeKind::ArrayLiteral:
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

    Sema semantic{context, tu};
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
