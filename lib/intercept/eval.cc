#include <intercept/ast.hh>
#include <intercept/eval.hh>

bool lcc::intercept::Expr::evaluate(const Context* ctx, EvalResult& out, bool required) {
    LCC_ASSERT(!required || ok(), "Cannot evaluate ill-formed or unchecked expression");
    switch (kind()) {
        case Kind::While:
        case Kind::For:
        case Kind::Return:
        case Kind::StructDecl:
        case Kind::TypeAliasDecl:
        case Kind::VarDecl:
        case Kind::FuncDecl:
        case Kind::CompoundLiteral:
        case Kind::OverloadSet:
        case Kind::Call:
        case Kind::IntrinsicCall:
        case Kind::NameRef:
        case Kind::MemberAccess:
        not_constexpr:
            if (required) Diag::Error(ctx, location(), "Not a constant expression");
            return false;

        case Kind::IntegerLiteral:
            out = i64(as<IntegerLiteral>(this)->value());
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
            return res.is_i64() and res.as_i64()
                     ? i->then()->evaluate(ctx, out, required)
                     : i->else_()->evaluate(ctx, out, required);
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
            goto not_constexpr;
        }

        case Kind::Unary: {
            auto u = cast<UnaryExpr>(this);
            EvalResult res;
            if (not u->operand()->evaluate(ctx, res, required)) return false;
            switch (u->op()) {
                default: goto not_constexpr;
                case TokenKind::Minus:
                    if (res.is_i64()) {
                        out = -res.as_i64();
                        return true;
                    }

                    goto not_constexpr;

                case TokenKind::Tilde:
                    if (res.is_i64()) {
                        out = ~res.as_i64();
                        return true;
                    }

                    goto not_constexpr;

                case TokenKind::Exclam:
                    if (res.is_i64()) {
                        out = res.as_i64() == 0;
                        return true;
                    }

                    goto not_constexpr;
            }
        }

        case Kind::Binary: {
            auto b = cast<BinaryExpr>(this);
            EvalResult lhs, rhs;
            if (not b->lhs()->evaluate(ctx, lhs, required)) return false;
            if (not b->rhs()->evaluate(ctx, rhs, required)) return false;
            if (not lhs.is_i64() or not rhs.is_i64()) goto not_constexpr;
            switch (b->op()) {
                default: goto not_constexpr;
                case TokenKind::Eq:
                    out = lhs.as_i64() == rhs.as_i64();
                    return true;

                case TokenKind::Ne:
                    out = lhs.as_i64() != rhs.as_i64();
                    return true;

                case TokenKind::Lt:
                    out = b->type()->is_signed_int(ctx)
                            ? lhs.as_i64() < rhs.as_i64()
                            : u64(lhs.as_i64()) < u64(rhs.as_i64());
                    return true;

                case TokenKind::Gt:
                    out = b->type()->is_signed_int(ctx)
                            ? lhs.as_i64() > rhs.as_i64()
                            : u64(lhs.as_i64()) > u64(rhs.as_i64());
                    return true;

                case TokenKind::Le:
                    out = b->type()->is_signed_int(ctx)
                            ? lhs.as_i64() <= rhs.as_i64()
                            : u64(lhs.as_i64()) <= u64(rhs.as_i64());
                    return true;

                case TokenKind::Ge:
                    out = b->type()->is_signed_int(ctx)
                            ? lhs.as_i64() >= rhs.as_i64()
                            : u64(lhs.as_i64()) >= u64(rhs.as_i64());
                    return true;

                case TokenKind::Star:
                    out = lhs.as_i64() * rhs.as_i64();
                    return true;

                case TokenKind::Slash:
                    if (rhs.as_i64() == 0) {
                        if (required) Diag::Error(ctx, location(), "Division by zero");
                        return false;
                    }

                    out = lhs.as_i64() / rhs.as_i64();
                    return true;

                case TokenKind::Percent:
                    if (rhs.as_i64() == 0) {
                        if (required) Diag::Error(ctx, location(), "Division by zero");
                        return false;
                    }

                    out = lhs.as_i64() % rhs.as_i64();
                    return true;

                case TokenKind::Plus:
                    out = lhs.as_i64() + rhs.as_i64();
                    return true;

                case TokenKind::Minus:
                    out = lhs.as_i64() - rhs.as_i64();
                    return true;

                case TokenKind::Shl:
                    out = lhs.as_i64() << rhs.as_i64();
                    return true;

                case TokenKind::Shr:
                    out = b->type()->is_signed_int(ctx)
                            ? lhs.as_i64() >> rhs.as_i64()
                            : i64(u64(lhs.as_i64()) >> u64(rhs.as_i64()));
                    return true;

                case TokenKind::Ampersand:
                    out = lhs.as_i64() & rhs.as_i64();
                    return true;

                case TokenKind::Pipe:
                    out = lhs.as_i64() | rhs.as_i64();
                    return true;

                case TokenKind::Caret:
                    out = lhs.as_i64() ^ rhs.as_i64();
                    return true;
            }
        }
    }

    LCC_UNREACHABLE();
}
