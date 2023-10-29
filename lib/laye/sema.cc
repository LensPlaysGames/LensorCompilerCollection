#include <laye/sema.hh>
#include <lcc/context.hh>
#include <lcc/target.hh>
#include <lcc/utils/macros.hh>

namespace layec = lcc::laye;

void layec::Sema::Analyse(LayeContext* laye_context, Module* module, bool use_colours) {
    LCC_ASSERT(laye_context);

    Sema sema{laye_context, module, use_colours};
    sema.Analyse(module);
}

void layec::Sema::Analyse(Module* module) {
    module->set_sema_in_progress();

    /// Analyse all imports first, since we depend on them in our module.
    for (auto& import : module->imports()) {
        auto imported_module = import.module;
        if (imported_module->sema_state() == SemaState::InProgress) {
            Error(import.location, "Circular dependency detected: cannot import this module");
            imported_module->set_sema_errored();
            continue;
        } else if (imported_module->sema_errored()) {
            imported_module->set_sema_errored();
            continue;
        }

        Analyse(imported_module);
        LCC_ASSERT(imported_module->sema_done_or_errored(), "module analysis did not result in a done or errored module state");
    }

    /// Step 1: Continue to analyse type declarations for as long as we need.
    bool all_module_types_resolved_or_errored = false;
    while (not all_module_types_resolved_or_errored) {
        all_module_types_resolved_or_errored = true;

        int attempted = 0;
        for (auto decl : module->top_level_decls()) {
            /// anything that's already been analysed, errors or not, does not need to be done again.
            if (decl->sema_done_or_errored()) continue;
            /// for now, we do not care about functions or bindings. only types (struct, enum, etc.)
            if (is<FunctionDecl, BindingDecl>(decl)) continue;

            attempted++;

            Analyse((Statement*&) (decl));
            LCC_ASSERT(decl->sema_state() != SemaState::NotAnalysed);

            if (not decl->sema_done_or_errored())
                all_module_types_resolved_or_errored = false;
        }

        if (not all_module_types_resolved_or_errored and attempted == 0) {
            Diag::Fatal("Something is wrong in Laye sema, we're heading to infinite type analysis loop");
        }
    }

    LCC_ASSERT(all_module_types_resolved_or_errored, "Failed to analyse module types properly");

    /// Step 2: Analyse function prototypes so we can call functions and have types available.
    for (auto decl : module->top_level_decls()) {
        /// for now, we do not care about functions or bindings. only types (struct, enum, etc.)
        if (auto func_decl = cast<FunctionDecl>(decl)) {
            LCC_ASSERT(func_decl->sema_state() == SemaState::NotAnalysed);
            AnalysePrototype(func_decl);

            LCC_ASSERT(func_decl->return_type()->sema_done_or_errored(), "should have finished function return type analysis");
            for (auto param : func_decl->params()) {
                LCC_ASSERT(param->type->sema_done_or_errored(), "should have finished function param type analysis");
            }
        }
    }

    /// Step 3: Analyse function bodies and global bindings.
    for (auto decl : module->top_level_decls()) {
        /// for now, we do not care about functions or bindings. only types (struct, enum, etc.)
        if (not is<FunctionDecl, BindingDecl>(decl)) continue;

        LCC_ASSERT(decl->sema_state() == SemaState::NotAnalysed);
        Analyse((Statement*&) (decl));
        LCC_ASSERT(decl->sema_done_or_errored(), "should have finished function analysis");
    }

    if (not module->sema_errored()) module->set_sema_done();
}

void layec::Sema::AnalysePrototype(FunctionDecl* func) {
    LCC_ASSERT(func->sema_state() == SemaState::NotAnalysed);

    AnalyseType(func->return_type());
    LCC_ASSERT(func->return_type()->sema_done_or_errored());

    for (auto& param : func->params()) {
        AnalyseType(param->type);
        LCC_ASSERT(param->type->sema_done_or_errored());
    }

    if (func->return_type()->is_void()) {
        if (func->has_mod(TokenKind::Nodiscard))
            Error(func->return_type()->location(), "Void function cannot be 'nodiscard'.");
    }

    if (func->return_type()->is_noreturn()) {
        if (func->has_mod(TokenKind::Nodiscard))
            Error(func->return_type()->location(), "Noreturn function cannot be 'nodiscard'.");

        // TODO(local): noreturn is always impure, if we have purity checks in Laye
    }
}

void layec::Sema::Analyse(Statement*& statement) {
    statement->set_sema_in_progress();

    auto kind = statement->kind();
    switch (kind) {
        case Statement::Kind::DeclFunction: {
            auto s = as<FunctionDecl>(statement);
            if (auto& body = s->body()) {
                if (auto expr_body = cast<ExprStatement>(body)) {
                    std::vector<Statement*> children{};
                    children.push_back(new (*module()) ReturnStatement{body->location(), expr_body->expr()});
                    body = new (*module()) BlockStatement{body->location(), children};
                }

                LCC_ASSERT(is<BlockStatement>(body));

                tempset curr_func = s;
                Analyse(body);
            }
        } break;

        case Statement::Kind::Block: {
            for (auto& child : as<BlockStatement>(statement)->children()) {
                Analyse(child);
            }
        } break;

        case Statement::Kind::Return: {
            LCC_ASSERT(curr_func);
            auto s = as<ReturnStatement>(statement);
            
            if (curr_func->return_type()->is_noreturn()) {
                Error(s->location(), "Cannot return from noreturn function.");
            }

            if (s->is_void_return()) {
                if (not curr_func->return_type()->is_void()) {
                    Error(s->location(), "Nonvoid function requires a return value.");
                }
            } else {
                Analyse(s->value());
                if (curr_func->return_type()->is_void()) {
                    Error(s->location(), "Cannot return a value from a void function.");
                } else {
                    LCC_ASSERT(curr_func->return_type()->sema_done_or_errored());
                    ConvertOrError(s->value(), curr_func->return_type());
                }
            }
        } break;

        default: {
            Warning(statement->location(), "Unhandled statement in Sema::Analyze(Statement*&): {}", ToString(kind));
            statement->set_sema_errored();
        } break;
    }

    if (not statement->sema_done_or_errored())
        statement->set_sema_done();
}

bool layec::Sema::Analyse(Expr*& expr, Type* expected_type) {
    LCC_ASSERT(curr_func);

    if (expr->sema_state() != SemaState::NotAnalysed)
        return expr->sema_ok();
    expr->set_sema_in_progress();

    auto kind = expr->kind();
    switch (kind) {
        case Expr::Kind::Cast: {
            auto e = as<CastExpr>(expr);
            if (e->cast_kind() == CastKind::ImplicitCast) {
                expr->type(e->type());
                expr->set_sema_done();
                break;
            }

            if (not Analyse(e->value(), e->type()))
                break;
            
            if (Convert(e->value(), e->type()))
                break;
            
            LCC_TODO();
        }

        case Expr::Kind::LitInt:
            expr->type(new (*module()) IntType(expr->location(), true, (int)context()->target()->size_of_pointer));
            break;

        default: {
            if (auto t = cast<Type>(expr)) return AnalyseType(t);
            Warning(expr->location(), "Unhandled expression in Sema::Analyze(Expr*&): {}", ToString(kind));
            expr->set_sema_errored();
            expr->type(new (*module()) PoisonType{expr->location()});
        } break;
    }

    if (not expr->sema_done_or_errored())
        expr->set_sema_done();

    LCC_ASSERT(expr->type());
    return expr->sema_ok();
}

bool layec::Sema::AnalyseType(Type*& type) {
    type->set_sema_in_progress();

    auto kind = type->kind();
    switch (kind) {
        case Expr::Kind::TypeInt: {
            auto t = as<IntType>(type);
            if (t->bit_width() <= 0 or t->bit_width() > 65535) {
                Error(type->location(), "Integer type bit width out of range (1 to 65535)");
                type->set_sema_errored();
            }
        } break;

        default: {
            Warning(type->location(), "Unhandled type in Sema::Analyze(Type*&): {}", ToString(kind));
            type->set_sema_errored();
        } break;
    }

    if (not type->sema_done_or_errored())
        type->set_sema_done();

    return type->sema_ok();
}

template <bool PerformConversion>
int layec::Sema::ConvertImpl(Expr*& expr, Type* to) {
    enum : int {
        TypesContainErrors = -2,
        ConversionImpossible = -1,
        NoOp = 0,
    };

    auto from = expr->type();
    if (from->sema_errored() or to->sema_errored()) return TypesContainErrors;

    auto Score = [](int i) {
        LCC_ASSERT(i >= 1, "Score must be 1 or greater. Use the enum constants above for values <= 0");
        return i;
    };

    if (Type::Equal(from, to))
        return NoOp;

    if (from->is_integer() and to->is_bool()) {
        if constexpr (PerformConversion) InsertImplicitCast(expr, to);
        return Score(1);
    }

    if (from->is_integer() and to->is_integer()) {
        EvalResult res;
        if (expr->evaluate(laye_context(), res, false)) {
            auto val = res.as_i64();
            if (val < 0 and not cast<IntType>(to)->is_signed()) {
                return ConversionImpossible;
            }

            auto bits = to->size(context());
            if (not cast<IntType>(from)->is_signed() and bits < 64 and u64(val) > u64(utils::MaxBitValue(bits))) {
                return ConversionImpossible;
            }

            if constexpr (PerformConversion) {
                InsertImplicitCast(expr, to);

                auto type = expr->type();
                LCC_ASSERT(type);

                expr = new (*module()) ConstantExpr(expr, res);
                expr->type(type);
            }

            return Score(1);
        }

        if (
            from->size(context()) <= to->size(context()) and
            (not cast<IntType>(from)->is_signed() or cast<IntType>(to)->is_signed())
        ) {
            if constexpr (PerformConversion)
                InsertImplicitCast(expr, to);

            return Score(1);
        }
    }

    if (from->is_function() and to->is_pointer() and Type::Equal(cast<SingleElementType>(to)->elem_type(), from)) {
        if constexpr (PerformConversion) InsertImplicitCast(expr, to);
        return NoOp;
    }

    return ConversionImpossible;
}

bool layec::Sema::Convert(Expr*& expr, Type* to) {
    if (expr->sema_errored()) return true;
    return ConvertImpl<true>(expr, to) >= 0;
}

void layec::Sema::ConvertOrError(Expr*& expr, Type* to) {
    if (not Convert(expr, to)) Error(
        expr->location(),
        "Expression is not convertible to type {}",
        to->string(use_colours)
    );
}

bool layec::Sema::ConvertToCommonType(Expr*& a, Expr*& b) {
    return Convert(a, b->type()) or Convert(b, a->type());
}

int layec::Sema::TryConvert(Expr*& expr, Type* to) {
    return ConvertImpl<false>(expr, to);
}

void layec::Sema::Discard(Expr*& expr) {
    LCC_ASSERT(false);
}

bool layec::Sema::HasSideEffects(Expr* expr) {
    LCC_ASSERT(false);
}

void layec::Sema::InsertImplicitCast(Expr*& expr, Type* ty) {
    WrapWithCast(expr, ty, CastKind::ImplicitCast);
}

void layec::Sema::InsertPointerToIntegerCast(Expr*& operand) {
    LCC_ASSERT(false);
}

void layec::Sema::WrapWithCast(Expr*& expr, Type* type, CastKind kind) {
    auto wrapper = new (*module()) CastExpr(expr->location(), type, expr, kind);
    Analyse((Expr*&)wrapper);
    expr = wrapper;
}

auto layec::Sema::Ptr(Type* type) -> PointerType* {
    LCC_ASSERT(false);
}
