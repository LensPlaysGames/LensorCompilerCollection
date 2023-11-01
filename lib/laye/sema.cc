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

    LCC_ASSERT(func->sema_state() == SemaState::NotAnalysed);
}

void layec::Sema::Analyse(Statement*& statement) {
    defer {
        if (not statement->sema_done_or_errored())
            statement->set_sema_done();
        
        LCC_ASSERT(statement->sema_done_or_errored());
    };

    statement->set_sema_in_progress();

    auto kind = statement->kind();
    switch (kind) {
        case Statement::Kind::DeclFunction: {
            auto s = as<FunctionDecl>(statement);

            if (s->name() == "main") {
                // TODO(local): check that main is at global scope before adding this
                s->add_mod(DeclModifier{s->location(), TokenKind::Export});
            }

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

        case Statement::Kind::DeclBinding: {
            auto s = as<BindingDecl>(statement);
            if (s->type()->is_infer()) {
                LCC_TODO();
            } else {
                AnalyseType(s->type());
                if (s->init()) {
                    Analyse(s->init(), s->type());
                    ConvertOrError(s->init(), s->type());
                }
            }
        } break;

        case Statement::Kind::DeclAlias: {
            auto s = as<AliasDecl>(statement);
            AnalyseType(s->type());
        } break;

        case Statement::Kind::Block: {
            for (auto& child : as<BlockStatement>(statement)->children()) {
                Analyse(child);
                LCC_ASSERT(child->sema_done_or_errored());
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

        case Statement::Kind::Xyzzy: {
            // Nothing happened.
        } break;

        case Statement::Kind::If: {
            auto s = as<IfStatement>(statement);

            Analyse(s->condition(), Type::Bool);
            ConvertOrError(s->condition(), Type::Bool);

            Analyse(s->pass());
            if (s->fail()) Analyse(s->fail());
        } break;

        case Statement::Kind::Expr: {
            auto s = as<ExprStatement>(statement);
            LCC_ASSERT(s->expr());
            AnalyseAndDiscard(s->expr());
        } break;

        default: {
            Error(statement->location(), "Unhandled statement in Sema::Analyze(Statement*&): {}", ToString(kind));
            statement->set_sema_errored();
        } break;
    }
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
        } break;

        case Expr::Kind::LookupName: {
            auto e = as<NameExpr>(expr);
            auto name = e->name();

            auto scope = e->scope();
            decltype(scope->find(name)) symbols;

            while (scope) {
                //scope->debug_print();
                symbols = scope->find(name);
                scope = scope->parent();
                if (symbols.first != symbols.second) break;
            }

            if (symbols.first == symbols.second) {
                Error(expr->location(), "Unknown symbol '{}' (looking up names through imports is not supported yet.)", name);
                expr->set_sema_errored();
                expr->type(new (*module()) PoisonType{expr->location()});
                break;
            }

            if (auto binding_decl = cast<BindingDecl>(symbols.first->second)) {
                if (binding_decl->sema_state() == SemaState::InProgress) {
                    Error(expr->location(), "Cannot use '{}' in its own initialiser", e->name());
                    expr->set_sema_errored();
                    expr->type(new (*module()) PoisonType{expr->location()});
                    break;
                }

                e->target(binding_decl);
                e->type(binding_decl->type());
            } else if ([[maybe_unused]] auto function_decl = cast<FunctionDecl>(symbols.first->second)) {
                std::vector<FunctionDecl*> overloads;
                auto AppendOverloads = [&overloads](auto&& range) {
                    for (auto it = range.first; it != range.second; it++)
                        overloads.push_back(as<FunctionDecl>(it->second));
                };

                AppendOverloads(symbols);
                for (; scope; scope = scope->parent())
                    AppendOverloads(scope->find(e->name()));

                // TODO(local): include overloads in imported scopes

                if (overloads.size() == 1) {
                    auto resolved_function = overloads[0];
                    e->target(resolved_function);
                    e->type(resolved_function->function_type());
                    break;
                }
                
                LCC_TODO();
            } else {
                LCC_TODO();
            }
        } break;

        case Expr::Kind::LookupPath: {
            auto e = as<PathExpr>(expr);

            auto path_names = e->names();
            LCC_ASSERT(not path_names.empty());

            auto first_name = path_names[0];
            auto import_lookup = module()->lookup_import(first_name);
            if (not import_lookup) {
                Error(expr->location(), "Unknown symbol '{}'", first_name);
                expr->set_sema_errored();
                expr->type(new (*module()) PoisonType{expr->location()});
                break;
            }

            auto curr_module = import_lookup->module;
            for (usz i = 1; i < path_names.size(); i++) {
                const auto& path_name = path_names[i];
                bool is_last_name = i == path_names.size() - 1;

                if (is_last_name) {
                    auto module_exports = curr_module->exports();
                    //module_exports->debug_print();
                    auto exported_symbols = module_exports->find(path_name);

                    if (exported_symbols.first == exported_symbols.second) {
                        Error(expr->location(), "Unknown symbol '{}'", path_name);
                        expr->set_sema_errored();
                        expr->type(new (*module()) PoisonType{expr->location()});
                        break;
                    }

                    if (auto binding_decl = cast<BindingDecl>(exported_symbols.first->second)) {
                        if (binding_decl->sema_state() == SemaState::InProgress) {
                            Error(expr->location(), "Cannot use '{}' in its own initialiser", path_name);
                            expr->set_sema_errored();
                            expr->type(new (*module()) PoisonType{expr->location()});
                            break;
                        }

                        e->target(binding_decl);
                        e->type(binding_decl->type());
                    } else if ([[maybe_unused]] auto function_decl = cast<FunctionDecl>(exported_symbols.first->second)) {
                        std::vector<FunctionDecl*> overloads;
                        auto AppendOverloads = [&overloads](auto&& range) {
                            for (auto it = range.first; it != range.second; it++)
                                overloads.push_back(as<FunctionDecl>(it->second));
                        };

                        AppendOverloads(exported_symbols);

                        // TODO(local): include overloads in imported scopes

                        if (overloads.size() == 1) {
                            auto resolved_function = overloads[0];
                            e->target(resolved_function);
                            e->type(resolved_function->function_type());
                            break;
                        }
                        
                        LCC_TODO();
                    } else {
                        LCC_TODO();
                    }
                } else {
                    LCC_TODO();
                }
            }
        } break;

        case Expr::Kind::Call: {
            auto e = as<CallExpr>(expr);

            for (auto& arg : e->args())
                Analyse(arg);

            if (not Analyse(e->target())) {
                expr->set_sema_errored();
                expr->type(new (*module()) PoisonType{expr->location()});
                break;
            }

            auto handle_lookup = [&](NamedDecl* target) {
                if ([[maybe_unused]] auto overload_set = cast<OverloadSet>(target)) {
                    Diag::ICE("Laye overload resolution is currently not implemented");
                }

                auto callee_type = e->target()->type();
                if (auto function_type = cast<FuncType>(callee_type)) {
                    const auto& param_types = function_type->param_types();

                    if (e->args().size() != param_types.size()) {
                        Error(expr->location(), "Expected {} arguments to call, got {}.",
                            param_types.size(), e->args().size());
                        expr->set_sema_errored();
                        expr->type(new (*module()) PoisonType{expr->location()});
                        return;
                    }

                    for (unsigned i = 0; i < e->args().size(); i++) {
                        auto& arg = e->args()[i];
                        ConvertOrError(arg, param_types[i]);
                    }

                    expr->type(function_type->return_type());
                } else {
                    Error(e->target()->location(), "Cannot call non-function value");
                    expr->set_sema_errored();
                    expr->type(new (*module()) PoisonType{expr->location()});
                    return;
                }
            };

            if (auto named_target = cast<NameExpr>(e->target())) {
                handle_lookup(named_target->target());
            } else if (auto path_target = cast<PathExpr>(e->target())) {
                handle_lookup(path_target->target());
            } else {
                LCC_TODO();
            }
        } break;

        case Expr::Kind::Binary: {
            auto e = as<BinaryExpr>(expr);

            Analyse(e->lhs());
            Analyse(e->rhs());

            switch (e->operator_kind()) {
                default: {
                    LCC_ASSERT(false, "unimplemented binary operator {}", ToString(e->operator_kind()));
                } break;

                case OperatorKind::Equal:
                case OperatorKind::NotEqual: {
                    expr->type(Type::Bool);
                    if (not ConvertToCommonType(e->lhs(), e->rhs())) {
                        expr->set_sema_errored();
                        break;
                    }

                    // TODO(local): actually decide what a valid equality compare is
                } break;
            }
        } break;

        case Expr::Kind::LitString: {
            expr->type(new (*module()) StringType(expr->location()));
        } break;

        case Expr::Kind::LitInt: {
            expr->type(new (*module()) IntType(expr->location(), true, (int)context()->target()->size_of_pointer, true));
        } break;

        default: {
            if (auto t = cast<Type>(expr)) return AnalyseType(t);
            Error(expr->location(), "Unhandled expression in Sema::Analyze(Expr*&): {}", ToString(kind));
            expr->set_sema_errored();
            expr->type(new (*module()) PoisonType{expr->location()});
        } break;
    }

    if (not expr->sema_done_or_errored())
        expr->set_sema_done();

    LCC_ASSERT(expr->type(), "for expr of kind '{}'", ToString(expr->kind()));
    return expr->sema_ok();
}

bool layec::Sema::AnalyseType(Type*& type) {
    type->set_sema_in_progress();

    auto kind = type->kind();
    switch (kind) {
        default: {
            Error(type->location(), "Unhandled type in Sema::Analyze(Type*&): {}", ToString(kind));
            type->set_sema_errored();
        } break;

        case Expr::Kind::TypeVoid: {
        } break;

        case Expr::Kind::TypeBuffer: {
            auto t = as<BufferType>(type);
            if (not AnalyseType(t->elem_type()))
                return false;
            
            if (t->elem_type()->is_void()) {
                Error(t->elem_type()->location(), "Void is not a valid container element type");
                type->set_sema_errored();
            }

            if (t->elem_type()->is_noreturn()) {
                Error(t->elem_type()->location(), "Noreturn is only valid as a function return type");
                type->set_sema_errored();
            }
        } break;

        case Expr::Kind::TypeInt: {
            auto t = as<IntType>(type);
            if (t->is_platform()) {
                t->bit_width(int(context()->target()->size_of_pointer));
            } else if (t->bit_width() <= 0 or t->bit_width() > 65535) {
                Error(type->location(), "Primitive type bit width must be in the range (0, 65535]");
                type->set_sema_errored();
            }
        } break;

        case Expr::Kind::TypeLookupName: {
            auto t = as<NameType>(type);
            auto name = t->name();

            auto scope = t->scope();
            decltype(scope->find(name)) symbols;

            while (scope) {
                //scope->debug_print();
                symbols = scope->find(name);
                scope = scope->parent();
                if (symbols.first != symbols.second) break;
            }

            if (symbols.first == symbols.second) {
                Error(type->location(), "Unknown symbol '{}' (looking up names through imports is not supported yet.)", name);
                type->set_sema_errored();
                break;
            }

            if (auto alias_decl = cast<AliasDecl>(symbols.first->second)) {
                type = alias_decl->type();
            } else {
                LCC_TODO();
            }
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

                expr = new (*module()) ConstantExpr(expr, res);
                expr->type(to);
            }

            return Score(1);
        }

        // TODO(local): special case platform integers

        if (
            from->size(context()) <= to->size(context()) and
            (not cast<IntType>(from)->is_signed() or cast<IntType>(to)->is_signed())
        ) {
            if constexpr (PerformConversion) {
                InsertImplicitCast(expr, to);
            }

            return Score(1);
        }
    }

    if (from->is_function() and to->is_pointer() and Type::Equal(cast<SingleElementType>(to)->elem_type(), from)) {
        if constexpr (PerformConversion) InsertImplicitCast(expr, to);
        return NoOp;
    }

    if (from->is_string() and to->is_buffer()) {
        auto to_buffer = as<BufferType>(to);
        if (
            to_buffer->elem_type()->is_integer() and
            to_buffer->access() == TypeAccess::ReadOnly and
            as<IntType>(to_buffer->elem_type())->bit_width() == 8
        ) {
            EvalResult res;
            if (expr->evaluate(laye_context(), res, false)) {
                if constexpr (PerformConversion) {
                    InsertImplicitCast(expr, to);

                    expr = new (*module()) ConstantExpr(expr, res);
                    expr->type(to);
                }

                return Score(1);
            }

            if constexpr (PerformConversion)
                InsertImplicitCast(expr, to);
            
            return Score(1);
        }
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
    LCC_ASSERT(expr->sema_done_or_errored());
    if (auto call_expr = cast<CallExpr>(expr)) {
        [[maybe_unused]] auto call_target = call_expr->target();
        //Warning(expr->location(), "Do this later (nodiscard in sema) !");
    } else {
        Error(expr->location(), "Nonsense!");
    }
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
