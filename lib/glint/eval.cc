#include <glint/ast.hh>
#include <glint/eval.hh>

bool lcc::glint::Expr::evaluate(const Context* ctx, EvalResult& out, bool required) {
    LCC_ASSERT(!required || ok(), "Cannot evaluate ill-formed or unchecked expression");
    auto not_a_constant_expr = [&]() {
        if (required) Diag::Error(ctx, location(), "Not a constant expression");
        return false;
    };
    switch (kind()) {
        /// These are here not necessarily because they are not constant
        /// expressions but rather because evaluating them has not yet
        /// been implemented.
        case Kind::While:
        case Kind::For:
        case Kind::Return:
        case Kind::TypeDecl:
        case Kind::TypeAliasDecl:
        case Kind::VarDecl:
        case Kind::FuncDecl:
        case Kind::CompoundLiteral:
        case Kind::OverloadSet:
        case Kind::Call:
        case Kind::IntrinsicCall:
        case Kind::NameRef:
        case Kind::MemberAccess:
        case Kind::EnumeratorDecl:
        case Kind::Module:
        case Kind::Type:
        case Kind::Sizeof:
        case Kind::Alignof:
            return not_a_constant_expr();

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
            auto i = as<IfExpr>(this);

            /// Check that this returns a value.
            if (i->type()->is_void()) {
                if (required) Diag::Error(
                    ctx,
                    location(),
                    "if expression that does not return a value is not a constant expression"
                );

                return false;
            }

            /// Evaluate the condition.
            EvalResult res;
            if (not i->condition()->evaluate(ctx, res, required)) return false;
            if (not res.is_int()) return not_a_constant_expr();
            return res.as_int() != 0
                     ? i->then()->evaluate(ctx, out, required)
                     : i->otherwise()->evaluate(ctx, out, required);
        }

        case Kind::Block:
            for (auto expr : as<BlockExpr>(this)->children())
                if (not expr->evaluate(ctx, out, required))
                    return false;
            return true;

        /// Most casts that create a new value and which can be performed at compile
        /// time are already performed by sema when `Convert()` is called, so we only
        /// check for no-op casts here.
        case Kind::Cast: {
            auto c = as<CastExpr>(this);
            if (Type::Equal(c->type(), c->operand()->type()))
                return c->operand()->evaluate(ctx, out, required);
            return not_a_constant_expr();
        }

        case Kind::Unary: {
            auto u = cast<UnaryExpr>(this);
            EvalResult res;
            if (not u->operand()->evaluate(ctx, res, required)) return false;
            switch (u->op()) {
                default: return not_a_constant_expr();
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
            }
        }

        case Kind::Binary: {
            auto b = cast<BinaryExpr>(this);
            EvalResult lhs, rhs;
            if (not b->lhs()->evaluate(ctx, lhs, required)) return false;
            if (not b->rhs()->evaluate(ctx, rhs, required)) return false;
            if (not lhs.is_int() or not rhs.is_int()) return not_a_constant_expr();
            switch (b->op()) {
                default: return not_a_constant_expr();
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
            }
        }
    }

    LCC_UNREACHABLE();
}
