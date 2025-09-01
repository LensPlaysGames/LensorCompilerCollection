#include <glint/ast.hh>
#include <glint/eval.hh>

#include <lcc/context.hh>
#include <lcc/diags.hh>
#include <lcc/utils.hh>

auto lcc::glint::Expr::evaluate(const Context* ctx, EvalResult& out, bool required) -> bool {
    LCC_ASSERT(
        not required or ok(),
        "Cannot evaluate ill-formed or unchecked expression"
    );
    auto not_a_constant_expr = [&](std::string_view msg = "Not a constant expression") {
        if (required)
            Diag::Error(ctx, location(), "{}", msg);
        return false;
    };
    auto unhandled_constant_expr = [&]() {
        // ICE?
        if (required)
            Diag::Error(ctx, location(), "Constant expression not yet handled in the compile-time evaluator (sorry)");
        return false;
    };
    switch (kind()) {
        /// These are here not necessarily because they are not constant
        /// expressions but rather because evaluating them has not yet
        /// been implemented.
        case Kind::EnumeratorDecl:
        case Kind::FuncDecl:
        case Kind::Module:
        case Kind::OverloadSet:
        case Kind::Type:
        case Kind::TypeAliasDecl:
        case Kind::TypeDecl:
        case Kind::VarDecl:
            return not_a_constant_expr();

        case Kind::Alignof:
        case Kind::Call:
        case Kind::CompoundLiteral:
        case Kind::For:
        case Kind::IntrinsicCall:
        case Kind::MemberAccess:
        case Kind::NameRef:
        case Kind::Return:
        case Kind::Sizeof:
        case Kind::While:
            return unhandled_constant_expr();

        case Kind::IntegerLiteral:
            out = as<IntegerLiteral>(this)->value();
            return true;

        case Kind::StringLiteral:
            out = as<StringLiteral>(this);
            return true;

        case Kind::EvaluatedConstant:
            out = as<ConstantExpr>(this)->value();
            return true;

        case Kind::If: {
            auto* i = as<IfExpr>(this);

            /// Check that this returns a value.
            if (i->type()->is_void()) {
                return not_a_constant_expr(
                    "if expression that does not return a value is not a constant expression"
                );
            }

            /// Evaluate the condition.
            EvalResult res;
            if (not i->condition()->evaluate(ctx, res, required))
                return false;
            if (not res.is_int())
                return not_a_constant_expr("if condition expression is not an int");

            return res.as_int() != 0
                     ? i->then()->evaluate(ctx, out, required)
                     : i->otherwise()->evaluate(ctx, out, required);
        }

        case Kind::Block:
            for (auto* expr : as<BlockExpr>(this)->children())
                if (not expr->evaluate(ctx, out, required))
                    return false;
            return true;

        /// Most casts that create a new value and which can be performed at compile
        /// time are already performed by sema when `Convert()` is called, so we only
        /// check for no-op casts here.
        case Kind::Cast: {
            const auto* c = as<CastExpr>(this);
            if (Type::Equal(c->type(), c->operand()->type()))
                return c->operand()->evaluate(ctx, out, required);
            return not_a_constant_expr("cast expression is not a no-op, and therefore is not a constant expression. Likely an error in sema");
        }

        case Kind::Unary: {
            const auto* u = cast<UnaryExpr>(this);

            EvalResult res;
            if (not u->operand()->evaluate(ctx, res, required))
                return false;

            switch (u->op()) {
                case TokenKind::Minus:
                    if (res.is_int()) {
                        out = -res.as_int();
                        return true;
                    }

                    return not_a_constant_expr();

                case TokenKind::Tilde:
                    if (res.is_int()) {
                        out = ~res.as_int();
                        return true;
                    }

                    return not_a_constant_expr();

                case TokenKind::Exclam:
                    if (res.is_int()) {
                        out = res.as_int() == 0;
                        return true;
                    }

                    return not_a_constant_expr();

                case TokenKind::Colon:      // type expression
                case TokenKind::Hash:       // not used yet
                case TokenKind::Ampersand:  // address of
                case TokenKind::At:         // dereference
                case TokenKind::PlusPlus:   // increment
                case TokenKind::MinusMinus: // decrement
                case TokenKind::Has:        // sum type tag query
                    return unhandled_constant_expr();

                // NOT a unary prefix operator
                case TokenKind::Alignof:
                case TokenKind::AmpersandEq:
                case TokenKind::ArbitraryInt:
                case TokenKind::And:
                case TokenKind::BangLBrace:
                case TokenKind::Bool:
                case TokenKind::Byte:
                case TokenKind::CInt:
                case TokenKind::CLong:
                case TokenKind::CLongLong:
                case TokenKind::CShort:
                case TokenKind::CUInt:
                case TokenKind::CULong:
                case TokenKind::CULongLong:
                case TokenKind::CUShort:
                case TokenKind::Caret:
                case TokenKind::CaretEq:
                case TokenKind::ColonColon:
                case TokenKind::ColonEq:
                case TokenKind::Comma:
                case TokenKind::Dot:
                case TokenKind::Else:
                case TokenKind::Enum:
                case TokenKind::Eof:
                case TokenKind::Eq:
                case TokenKind::Export:
                case TokenKind::Expression:
                case TokenKind::External:
                case TokenKind::False:
                case TokenKind::For:
                case TokenKind::Ge:
                case TokenKind::Gensym:
                case TokenKind::Gt:
                case TokenKind::Ident:
                case TokenKind::If:
                case TokenKind::Int:
                case TokenKind::Invalid:
                case TokenKind::LBrace:
                case TokenKind::LBrack:
                case TokenKind::LParen:
                case TokenKind::Lambda:
                case TokenKind::Le:
                case TokenKind::Lt:
                case TokenKind::MacroArg:
                case TokenKind::MinusEq:
                case TokenKind::Ne:
                case TokenKind::Number:
                case TokenKind::Or:
                case TokenKind::Percent:
                case TokenKind::PercentEq:
                case TokenKind::Pipe:
                case TokenKind::PipeEq:
                case TokenKind::Plus:
                case TokenKind::PlusEq:
                case TokenKind::RBrace:
                case TokenKind::RBrack:
                case TokenKind::RParen:
                case TokenKind::Return:
                case TokenKind::RightArrow:
                case TokenKind::Semicolon:
                case TokenKind::Shl:
                case TokenKind::Shr:
                case TokenKind::Sizeof:
                case TokenKind::Slash:
                case TokenKind::SlashEq:
                case TokenKind::Star:
                case TokenKind::StarEq:
                case TokenKind::StarStar:
                case TokenKind::String:
                case TokenKind::Struct:
                case TokenKind::Supplant:
                case TokenKind::Sum:
                case TokenKind::TildeEq:
                case TokenKind::True:
                case TokenKind::UInt:
                case TokenKind::Union:
                case TokenKind::Void:
                case TokenKind::While:
                    Diag::ICE("Invalid prefix operator '{}'", ToString(u->op()));
                    LCC_UNREACHABLE();
            }
            LCC_UNREACHABLE();
        } break;

        case Kind::Binary: {
            const auto* b = cast<BinaryExpr>(this);

            EvalResult lhs;
            EvalResult rhs;
            if (not b->lhs()->evaluate(ctx, lhs, required)) return false;
            if (not b->rhs()->evaluate(ctx, rhs, required)) return false;

            // Only handle binary expressions between integers (for now).
            if (not lhs.is_int() or not rhs.is_int())
                return not_a_constant_expr();

            switch (b->op()) {
                case TokenKind::Eq:
                    out = lhs.as_int() == rhs.as_int();
                    return true;

                case TokenKind::Ne:
                    out = lhs.as_int() != rhs.as_int();
                    return true;

                case TokenKind::Lt:
                    out = b->type()->is_signed_int(ctx)
                            ? lhs.as_int().slt(rhs.as_int())
                            : lhs.as_int().ult(rhs.as_int());
                    return true;

                case TokenKind::Gt:
                    out = b->type()->is_signed_int(ctx)
                            ? lhs.as_int().sgt(rhs.as_int())
                            : lhs.as_int().ugt(rhs.as_int());
                    return true;

                case TokenKind::Le:
                    out = b->type()->is_signed_int(ctx)
                            ? lhs.as_int().sle(rhs.as_int())
                            : lhs.as_int().ule(rhs.as_int());
                    return true;

                case TokenKind::Ge:
                    out = b->type()->is_signed_int(ctx)
                            ? lhs.as_int().sge(rhs.as_int())
                            : lhs.as_int().uge(rhs.as_int());
                    return true;

                case TokenKind::Star:
                    out = lhs.as_int() * rhs.as_int();
                    return true;

                case TokenKind::Slash:
                    if (rhs.as_int() == 0) {
                        if (required) Diag::Error(ctx, location(), "Division by zero");
                        return false;
                    }

                    out = b->type()->is_signed_int(ctx)
                            ? lhs.as_int().sdiv(rhs.as_int())
                            : lhs.as_int().udiv(rhs.as_int());
                    return true;

                case TokenKind::Percent:
                    if (rhs.as_int() == 0) {
                        if (required) Diag::Error(ctx, location(), "Division by zero");
                        return false;
                    }

                    out = b->type()->is_signed_int(ctx)
                            ? lhs.as_int().srem(rhs.as_int())
                            : lhs.as_int().urem(rhs.as_int());
                    return true;

                case TokenKind::Plus:
                    out = lhs.as_int() + rhs.as_int();
                    return true;

                case TokenKind::Minus:
                    out = lhs.as_int() - rhs.as_int();
                    return true;

                case TokenKind::Shl:
                    out = lhs.as_int() << rhs.as_int();
                    return true;

                case TokenKind::Shr:
                    out = b->type()->is_signed_int(ctx)
                            ? lhs.as_int().sar(rhs.as_int())
                            : lhs.as_int().shr(rhs.as_int());
                    return true;

                case TokenKind::Ampersand:
                    out = lhs.as_int() & rhs.as_int();
                    return true;

                case TokenKind::Pipe:
                    out = lhs.as_int() | rhs.as_int();
                    return true;

                case TokenKind::Caret:
                    out = lhs.as_int() ^ rhs.as_int();
                    return true;

                case TokenKind::LBrack:
                case TokenKind::PlusEq:
                case TokenKind::MinusEq:
                case TokenKind::StarEq:
                case TokenKind::SlashEq:
                case TokenKind::PercentEq:
                case TokenKind::AmpersandEq:
                case TokenKind::PipeEq:
                case TokenKind::CaretEq:
                case TokenKind::TildeEq:
                case TokenKind::ColonEq:
                case TokenKind::ColonColon:
                case TokenKind::RightArrow:
                case TokenKind::And:
                case TokenKind::Or:
                    return unhandled_constant_expr();

                // NOT a binary operator
                case TokenKind::Invalid:
                case TokenKind::Eof:
                case TokenKind::LParen:
                case TokenKind::RParen:
                case TokenKind::RBrack:
                case TokenKind::LBrace:
                case TokenKind::RBrace:
                case TokenKind::BangLBrace:
                case TokenKind::Comma:
                case TokenKind::Colon:
                case TokenKind::Semicolon:
                case TokenKind::Dot:
                case TokenKind::Tilde:
                case TokenKind::Exclam:
                case TokenKind::At:
                case TokenKind::Hash:
                case TokenKind::PlusPlus:
                case TokenKind::MinusMinus:
                case TokenKind::StarStar:
                case TokenKind::Ident:
                case TokenKind::Number:
                case TokenKind::String:
                case TokenKind::If:
                case TokenKind::Else:
                case TokenKind::While:
                case TokenKind::Void:
                case TokenKind::Byte:
                case TokenKind::Bool:
                case TokenKind::External:
                case TokenKind::True:
                case TokenKind::False:
                case TokenKind::Int:
                case TokenKind::UInt:
                case TokenKind::ArbitraryInt:
                case TokenKind::Sizeof:
                case TokenKind::Alignof:
                case TokenKind::Has:
                case TokenKind::For:
                case TokenKind::Return:
                case TokenKind::Export:
                case TokenKind::Struct:
                case TokenKind::Enum:
                case TokenKind::Union:
                case TokenKind::Sum:
                case TokenKind::Lambda:
                case TokenKind::Supplant:
                case TokenKind::CShort:
                case TokenKind::CUShort:
                case TokenKind::CInt:
                case TokenKind::CUInt:
                case TokenKind::CLong:
                case TokenKind::CULong:
                case TokenKind::CLongLong:
                case TokenKind::CULongLong:
                case TokenKind::Gensym:
                case TokenKind::MacroArg:
                case TokenKind::Expression:
                    Diag::ICE("Invalid binary operator '{}'", ToString(b->op()));
                    LCC_UNREACHABLE();
            }
        }
    }

    LCC_UNREACHABLE();
}
