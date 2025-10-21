#include <lcc/utils.hh>

#include <glint/ast.hh>
#include <glint/ast_eval.hh>

namespace lcc::glint {

auto ASTEvaluator::eval_expr(Expr* expr) -> Expr* {
    if (not expr)
        return nullptr;

    if (not expr->ok()) {
        Diag::Error("GlintEval cannot evaluate an unchecked AST!");
        return nullptr;
    }

    // TODO: We may want to guarantee that we only ever evaluate an expression
    // once, and return value from some sort of cache if it is present.

    switch (expr->kind()) {
        case Expr::Kind::Return: {
            const auto r = as<ReturnExpr>(expr);
            // If we are not within a function, return means we are done evaluating.
            // If we are within a function (current is not nullptr),
            // then set the current function to the caller, or nullptr if no caller is present.
            if (context.context.current) {
                if (context.context.caller)
                    context.context.current = context.context.caller->current;
                else context.context.current = nullptr;
            }
            return eval_expr(r->value());
        }

        case Expr::Kind::While: {
            const auto w = as<WhileExpr>(expr);

            auto v = eval_expr(w->condition());

            // TODO: We need to find a way to figure out if an Expr* is truthy or not...
            // We could use eval.cc to get an EvalResult, or we could just write a
            // function IsTruthy(Expr*) and handle evaluated constant, integerliteral,
            // stringliteral, etc.

            (void) v;
            LCC_TODO("GlintEval: {}", ToString(expr->kind()));
        } break;

        case Expr::Kind::For: {
            const auto f = as<ForExpr>(expr);

            (void) f;
            LCC_TODO("GlintEval: {}", ToString(expr->kind()));
        } break;

        case Expr::Kind::TypeDecl: {
            const auto t = as<TypeDecl>(expr);

            (void) t;
            LCC_TODO("GlintEval: {}", ToString(expr->kind()));
        } break;

        case Expr::Kind::TypeAliasDecl: {
            const auto a = as<TypeAliasDecl>(expr);

            (void) a;
            LCC_TODO("GlintEval: {}", ToString(expr->kind()));
        } break;

        case Expr::Kind::EnumeratorDecl: {
            const auto e = as<EnumeratorDecl>(expr);

            (void) e;
            LCC_TODO("GlintEval: {}", ToString(expr->kind()));
        } break;

        case Expr::Kind::VarDecl: {
            const auto v = as<VarDecl>(expr);

            auto initial_value = v->init();
            // Zero initialize by default
            if (not initial_value)
                initial_value = new (mod) IntegerLiteral(0, {});
            context.current_scope.declare(v->name(), v->type(), initial_value);

            LCC_TODO("GlintEval: {}", ToString(expr->kind()));
        } break;

        case Expr::Kind::FuncDecl: {
            const auto f = as<FuncDecl>(expr);

            (void) f;
            LCC_TODO("GlintEval: {}", ToString(expr->kind()));
        } break;

        // Self Evaluating...
        case Expr::Kind::IntegerLiteral:
        case Expr::Kind::StringLiteral:
            return expr;

        case Expr::Kind::EvaluatedConstant: {
            const auto e = as<ConstantExpr>(expr);

            if (e->value().is_int())
                return new (mod) IntegerLiteral(e->value().as_int().value(), {});

            if (e->value().is_string()) {
                return new (mod) StringLiteral(
                    mod,
                    mod.strings.at(e->value().as_string()->string_index()),
                    {}
                );
            }

            if (e->value().is_null())
                return new (mod) IntegerLiteral(0, {});

            LCC_UNREACHABLE();
        }

        case Expr::Kind::CompoundLiteral: {
            const auto c = as<CompoundLiteral>(expr);

            std::vector<CompoundLiteral::Member> values{};
            for (auto m : c->values())
                values.emplace_back(m.name, eval_expr(m.value));

            return new (mod) CompoundLiteral(values, {}, c->type());
        }

        case Expr::Kind::OverloadSet: {
            const auto o = as<OverloadSet>(expr);

            (void) o;
            LCC_TODO("GlintEval: {}", ToString(expr->kind()));
        } break;

        case Expr::Kind::If: {
            const auto i = as<IfExpr>(expr);

            (void) i;
            LCC_TODO("GlintEval: {}", ToString(expr->kind()));
        } break;

        case Expr::Kind::Block: {
            const auto b = as<BlockExpr>(expr);
            Expr* last{nullptr};
            for (auto c : b->children())
                last = eval_expr(c);

            return last;
        }

        case Expr::Kind::Call: {
            const auto c = as<CallExpr>(expr);

            auto old_context = &context.context;
            context.context = *(new FunctionContext());
            context.context.caller = old_context;
            context.context.current = c->callee_type();

            // TODO: Open scope
            auto old_scope = &context.current_scope;
            context.current_scope = *(new EvalScope());
            context.current_scope.parent = old_scope;

            // evaluate args, binding them to parameter name
            for (
                auto [arg, param] :
                vws::zip(c->args(), c->callee_type()->params())
            ) {
                auto a = eval_expr(arg);
                context.current_scope.declare(
                    param.name,
                    arg->type(),
                    a
                );
            }

            // evaluate body
            LCC_TODO("GlintEval: Evaluate body in {}", ToString(expr->kind()));
        } break;

        case Expr::Kind::IntrinsicCall: {
            const auto i = as<IntrinsicCallExpr>(expr);

            (void) i;
            LCC_TODO("GlintEval: {}", ToString(expr->kind()));
        } break;

        case Expr::Kind::Cast: {
            const auto c = as<CastExpr>(expr);

            (void) c;
            LCC_TODO("GlintEval: {}", ToString(expr->kind()));
        } break;

        case Expr::Kind::Unary: {
            const auto u = as<UnaryExpr>(expr);
            switch (u->op()) {
                case TokenKind::Ampersand:
                case TokenKind::At:
                case TokenKind::Exclam:
                case TokenKind::Has:
                case TokenKind::MinusMinus:
                case TokenKind::PlusPlus:
                    LCC_ASSERT(false, "TODO: Implement unary operator {}", ToString(u->op()));

                case TokenKind::Invalid:
                case TokenKind::Eof:
                case TokenKind::LParen:
                case TokenKind::RParen:
                case TokenKind::LBrack:
                case TokenKind::RBrack:
                case TokenKind::LBrace:
                case TokenKind::RBrace:
                case TokenKind::BangLBrace:
                case TokenKind::Comma:
                case TokenKind::Colon:
                case TokenKind::Semicolon:
                case TokenKind::Dot:
                case TokenKind::Plus:
                case TokenKind::Minus:
                case TokenKind::Star:
                case TokenKind::Slash:
                case TokenKind::Percent:
                case TokenKind::Pipe:
                case TokenKind::Caret:
                case TokenKind::Tilde:
                case TokenKind::Hash:
                case TokenKind::Shl:
                case TokenKind::Shr:
                case TokenKind::Eq:
                case TokenKind::Ne:
                case TokenKind::Lt:
                case TokenKind::Gt:
                case TokenKind::Le:
                case TokenKind::Ge:
                case TokenKind::StarStar:
                case TokenKind::PlusEq:
                case TokenKind::MinusEq:
                case TokenKind::StarEq:
                case TokenKind::SlashEq:
                case TokenKind::PercentEq:
                case TokenKind::AmpersandEq:
                case TokenKind::PipeEq:
                case TokenKind::CaretEq:
                case TokenKind::TildeEq:
                case TokenKind::LBrackEq:
                case TokenKind::ColonEq:
                case TokenKind::ColonColon:
                case TokenKind::RightArrow:
                case TokenKind::Ident:
                case TokenKind::Number:
                case TokenKind::String:
                case TokenKind::ByteLiteral:
                case TokenKind::If:
                case TokenKind::Else:
                case TokenKind::While:
                case TokenKind::Void:
                case TokenKind::Byte:
                case TokenKind::Bool:
                case TokenKind::External:
                case TokenKind::True:
                case TokenKind::False:
                case TokenKind::And:
                case TokenKind::Or:
                case TokenKind::Int:
                case TokenKind::UInt:
                case TokenKind::ArbitraryInt:
                case TokenKind::Sizeof:
                case TokenKind::Alignof:
                case TokenKind::For:
                case TokenKind::RangedFor:
                case TokenKind::Return:
                case TokenKind::Export:
                case TokenKind::Struct:
                case TokenKind::Enum:
                case TokenKind::Union:
                case TokenKind::Sum:
                case TokenKind::Lambda:
                case TokenKind::Supplant:
                case TokenKind::Match:
                case TokenKind::Print:
                case TokenKind::Template:
                case TokenKind::Typeof:
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
                    LCC_ASSERT(false, "NOT a unary operator: {}", ToString(u->op()));
            }
            LCC_UNREACHABLE();
        } break;

        case Expr::Kind::Binary: {
            const auto b = as<BinaryExpr>(expr);
            switch (b->op()) {
                case TokenKind::LBrack:
                case TokenKind::Plus:
                case TokenKind::Minus:
                case TokenKind::Star:
                case TokenKind::Slash:
                case TokenKind::Percent:
                case TokenKind::Ampersand:
                case TokenKind::Pipe:
                case TokenKind::Caret:
                case TokenKind::Shl:
                case TokenKind::Shr:
                case TokenKind::Eq:
                case TokenKind::Ne:
                case TokenKind::Lt:
                case TokenKind::Gt:
                case TokenKind::Le:
                case TokenKind::Ge:
                case TokenKind::PlusEq:
                case TokenKind::MinusEq:
                case TokenKind::StarEq:
                case TokenKind::SlashEq:
                case TokenKind::PercentEq:
                case TokenKind::AmpersandEq:
                case TokenKind::PipeEq:
                case TokenKind::CaretEq:
                case TokenKind::TildeEq:
                case TokenKind::LBrackEq:
                case TokenKind::ColonEq:
                case TokenKind::RightArrow:
                case TokenKind::And:
                case TokenKind::Or:
                    LCC_ASSERT(false, "TODO: Implement binary operator {}", ToString(b->op()));

                // Handled elsewhere
                case TokenKind::ColonColon:
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
                case TokenKind::RangedFor:
                case TokenKind::Return:
                case TokenKind::Export:
                case TokenKind::Struct:
                case TokenKind::Enum:
                case TokenKind::Union:
                case TokenKind::Sum:
                case TokenKind::Lambda:
                case TokenKind::Supplant:
                case TokenKind::Match:
                case TokenKind::Print:
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
                case TokenKind::ByteLiteral:
                case TokenKind::Template:
                case TokenKind::Typeof:
                    LCC_ASSERT(false, "NOT a binary operator: {}", ToString(b->op()));
            }
            LCC_UNREACHABLE();
        }

        case Expr::Kind::NameRef: {
            const auto n = as<NameRefExpr>(expr);

            auto v = context.current_scope.get_value_recursive(n->name());
            if (v) return v;

            Diag::Error("GlintEval: Ran into name '{}' that is not bound in any current scope.", n->name());
            return nullptr;
        }

        case Expr::Kind::Match: {
            const auto m = as<MatchExpr>(expr);

            (void) m;
            LCC_TODO("GlintEval: {}", ToString(expr->kind()));
        } break;

        case Expr::Kind::Type: {
            const auto t = as<TypeExpr>(expr);

            (void) t;
            LCC_TODO("GlintEval: {}", ToString(expr->kind()));
        } break;

        case Expr::Kind::MemberAccess: {
            const auto m = as<MemberAccessExpr>(expr);

            (void) m;
            LCC_TODO("GlintEval: {}", ToString(expr->kind()));
        } break;

        // Possibly no-op or UNREACHABLE, as it is purely for namespace
        // information during sema.
        case Expr::Kind::Module: {
            const auto m = as<ModuleExpr>(expr);

            (void) m;
            LCC_TODO("GlintEval: {}", ToString(expr->kind()));
        } break;

        // Possibly no-op or UNREACHABLE, since sema is supposed to lower these
        // out.
        case Expr::Kind::Sizeof: {
            const auto s = as<SizeofExpr>(expr);
            // TODO: We need lcc::Context if we want to implement sizeof...

            (void) s;
            LCC_TODO("GlintEval: {}", ToString(expr->kind()));
        } break;

        // Possibly no-op or UNREACHABLE, since sema is supposed to lower these
        // out.
        case Expr::Kind::Alignof: {
            const auto a = as<AlignofExpr>(expr);
            // TODO: We need lcc::Context if we want to implement alignof...

            (void) a;
            LCC_TODO("GlintEval: {}", ToString(expr->kind()));
        } break;

        case Expr::Kind::Template: {
            const auto t = as<TemplateExpr>(expr);

            // Possibly no-op or UNREACHABLE, since sema is supposed to lower these
            // out.

            (void) t;
            LCC_TODO("GlintEval: {}", ToString(expr->kind()));
        } break;

            LCC_ASSERT(
                false,
                "TODO: Implement evaluation of AST node kind {}\n",
                ToString(expr->kind())
            );
    }
    LCC_UNREACHABLE();
}

auto ASTEvaluator::eval() -> Expr* {
    LCC_ASSERT(root);
    return eval_expr(root);
}

auto evaluate(Module& mod, Expr* expr) -> Expr* {
    LCC_ASSERT(expr);

    ASTEvaluator evaluator{mod, expr};
    return evaluator.eval();
}

} // namespace lcc::glint
