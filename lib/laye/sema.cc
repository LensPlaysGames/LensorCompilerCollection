#include "laye/ast.hh"

#include <laye/sema.hh>
#include <lcc/context.hh>
#include <lcc/target.hh>
#include <lcc/utils.hh>
#include <lcc/utils/macros.hh>

namespace lcc {

template <typename Entity>
class DependencyGraph {
    struct Node {
        Entity* entity;
        std::vector<Entity*> dependencies{};

        Node(Entity* entity) : entity(entity) {}
    };

    std::vector<Node*> nodes;

public:
    struct Result {
        enum class Kind {
            Ok,
            Cycle,
        };

        Kind kind;
        std::vector<Entity*> order{};
        Entity* from{nullptr};
        Entity* to{nullptr};
    };

    DependencyGraph() {}

    auto add_dependency(Entity* entity, Entity* dependency) {
        Node* node;

        auto it = rgs::find_if(nodes, [&](Node* node) { return node->entity == entity; });
        if (it != nodes.end()) {
            node = *it;
        } else {
            node = new Node(entity);
            nodes.push_back(node);
        }

        LCC_ASSERT(node);
        if (dependency) node->dependencies.push_back(dependency);
    }

    auto ensure_tracked(Entity* entity) { add_dependency(entity, nullptr); }

    Result get_resolved_order() {
        std::vector<Entity*> resolved{};
        std::vector<Entity*> seen{};

        std::function<Result(Entity*)> ResolveDependencies;
        ResolveDependencies = [&](Entity* entity) -> Result {
            auto resolved_it = rgs::find(resolved, entity);
            if (resolved_it != resolved.end())
                return Result{Result::Kind::Ok};

            seen.push_back(entity);

            auto it = rgs::find_if(nodes, [&](Node* node) { return node->entity == entity; });
            bool is_resolved = it == nodes.end() or (*it)->dependencies.empty();

            if (not is_resolved) {
                const std::vector<Entity*>& dependencies = (*it)->dependencies;
                for (Entity* dep : dependencies) {
                    auto dep_resolved_it = rgs::find(resolved, dep);
                    if (dep_resolved_it != resolved.end())
                        return Result{Result::Kind::Ok};

                    auto dep_seen_it = rgs::find(seen, dep);
                    if (dep_seen_it != seen.end())
                        return Result{Result::Kind::Cycle, {}, entity, *dep_seen_it};

                    Result result = ResolveDependencies(dep);
                    if (result.kind != Result::Kind::Ok) {
                        return result;
                    }
                }
            }

            resolved.push_back(entity);
            seen.erase(rgs::find(seen, entity));

            return Result{Result::Kind::Ok};
        };

        for (Node* node : nodes) {
            Result result = ResolveDependencies(node->entity);
            if (result.kind != Result::Kind::Ok) {
                return result;
            }
        }

        return Result{Result::Kind::Ok, std::move(resolved)};
    }
};

}; // namespace lcc

namespace lcc::laye {

auto LookupTypeEntity(Module* from_module, Scope* from_scope, const std::string& name) -> NamedDecl* {
    // Note that there shouldn't be any reason to create duplicate "imported" versions
    // of type entities, since the type system and referenced values within
    // the IR should be entirely orthogonal.

    Scope* search_scope = from_scope;
    while (search_scope) {
        auto lookup = search_scope->find(name);

        // if a lookup fails (two identical iterators, 0 range) then we look at the next scope up
        if (lookup.first == lookup.second) {
            search_scope = search_scope->parent();
            continue;
        }

        auto entity = lookup.first->second;

        // if the lookedup entity is of a type declaration, then return it
        if (is<AliasDecl, StructDecl, EnumDecl>(entity))
            return entity;

        // otherwise, we continue up the next scope
    }

    // if we reach here, then nothing within the module is a type entity.
    // time to search through imports.

    std::vector<NamedDecl*> possible_type_entities{};
    for (auto& import_decl : from_module->imports()) {
        if (not import_decl->is_wildcard()) continue;

        auto imported_module = import_decl->target_module();
        auto imported_entity_scope = imported_module->exports();
    }

    LCC_ASSERT(false, "LookupTypeEntity");
}

auto LookupTypeEntity(Module* from_module, Scope* from_scope, const std::vector<std::string>& names, const std::vector<Location>& locations) -> NamedDecl* {
    LCC_ASSERT(false, "LookupTypeEntity");
}

auto LookupValueEntity(Module* from_module, Scope* from_scope, const std::string& name) -> NamedDecl* {
    LCC_ASSERT(false, "LookupValueEntity");
}

auto LookupValueEntity(Module* from_module, Scope* from_scope, const std::vector<std::string>& names, const std::vector<Location>& locations) -> NamedDecl* {
    LCC_ASSERT(false, "LookupValueEntity");
}

void GenerateDependencies(DependencyGraph<NamedDecl>& deps, Module* module, FunctionDecl* decl) {
    deps.ensure_tracked(decl);
}

void GenerateDependencies(DependencyGraph<NamedDecl>& deps, Module* module) {
    if (module->sema_state() == SemaState::InProgress) return;
    module->set_sema_in_progress();

    for (auto im : module->imports()) {
        auto target_module = im->target_module();
        GenerateDependencies(deps, target_module);
    }

    for (auto tld : module->top_level_decls()) {
        switch (tld->kind()) {
            default: LCC_ASSERT(false, "Unhandled top level declaration {} when generating dependencies", ToString(tld->kind()));

            case Statement::Kind::DeclFunction: {
                auto func_decl = as<FunctionDecl>(tld);
                GenerateDependencies(deps, module, func_decl);
            } break;
        }
    }

    if (not module->sema_errored()) module->set_sema_done();
}

}; // namespace lcc::laye

namespace layec = lcc::laye;

void layec::Sema::Analyse(LayeContext* laye_context, Module* module, bool use_colours) {
    LCC_ASSERT(laye_context);

    DependencyGraph<NamedDecl> depgraph{};
    layec::GenerateDependencies(depgraph, module);

    auto order = depgraph.get_resolved_order();
    if (order.kind == DependencyGraph<NamedDecl>::Result::Kind::Cycle) {
        Diag::Note(laye_context->context(), order.to->location(), "Other dependency is here");
        Diag::Error(laye_context->context(), order.from->location(), "Cyclic dependency detected");
        return;
    }

    Sema sema{laye_context, use_colours};
    for (auto& tld : order.order) {
        LCC_ASSERT(tld->module());
        sema.Analyse(tld->module(), (Statement*&)tld);
    }
}

void layec::Sema::Analyse(Module* module, Statement*& statement) {
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
            
            AnalyseType(module, s->return_type());
            LCC_ASSERT(s->return_type()->sema_done_or_errored());

            std::vector<Type*> param_types{};
            for (auto& param : s->params()) {
                Analyse(module, (Statement*&) param);
                LCC_ASSERT(param->sema_done_or_errored());
                param_types.push_back(param->type());
                // TODO(local): attempt to evaluate constants for parameter inits
            }

            if (s->return_type()->is_void()) {
                if (s->has_mod(TokenKind::Nodiscard))
                    Error(s->return_type()->location(), "Void function cannot be 'nodiscard'.");
            }

            if (s->return_type()->is_noreturn()) {
                if (s->has_mod(TokenKind::Nodiscard))
                    Error(s->return_type()->location(), "Noreturn function cannot be 'nodiscard'.");

                // TODO(local): noreturn is always impure, if we have purity checks in Laye
            }

            s->function_type(new (*module) FuncType{s->location(), s->return_type(), param_types});

            if (s->name() == "main") {
                // TODO(local): check that main is at global scope before adding this
                s->add_mod(DeclModifier{s->location(), TokenKind::Export});
                s->add_mod(DeclModifier{s->location(), TokenKind::Foreign, "main"});
                s->add_mod(DeclModifier{s->location(), TokenKind::Callconv, {}, CallConv::C});
            }

            if (auto& body = s->body()) {
                if (auto expr_body = cast<ExprStatement>(body)) {
                    std::vector<Statement*> children{};
                    children.push_back(new (*module) ReturnStatement{body->location(), expr_body->expr()});
                    body = new (*module) BlockStatement{body->location(), children};
                }

                LCC_ASSERT(is<BlockStatement>(body));

                tempset curr_func = s;
                Analyse(module, body);
            }

            MangleName(s);
        } break;

        case Statement::Kind::DeclBinding: {
            auto s = as<BindingDecl>(statement);
            if (s->type()->is_infer()) {
                if (not s->init()) {
                    statement->set_sema_errored();
                    Error(s->location(), "Cannot infer binding type without an initializer.");
                    s->type() = new (*module) PoisonType{s->type()->location()};
                    break;
                }

                Analyse(module, s->init());
                s->type() = s->init()->type();
            } else {
                AnalyseType(module, s->type());
                if (s->init()) {
                    Analyse(module, s->init(), s->type());
                    ConvertOrError(module, s->init(), s->type());
                }
            }

            MangleName(s);
        } break;

        case Statement::Kind::DeclStruct: {
            auto s = as<StructDecl>(statement);

            std::function<StructType*(StructDecl*, StructType*)> CreateStructOrVariantType;
            CreateStructOrVariantType = [&](StructDecl* struct_decl, StructType* parent_struct) {
                std::vector<StructField> fields{};
                for (auto& field : struct_decl->fields()) {
                    Analyse(module, (Statement*&) field);
                    fields.push_back({field->name(), field->type()});
                }

                StructType* struct_type;
                if (parent_struct)
                    struct_type = new (*module) VariantType(struct_decl->location(), parent_struct, struct_decl->name(), std::move(fields));
                else struct_type = new (*module) StructType(struct_decl->location(), struct_decl->name(), std::move(fields));

                if (not struct_decl->variants().empty()) {
                    std::vector<VariantType*> variants{};
                    for (const auto& variant : struct_decl->variants()) {
                        auto variant_type = as<VariantType>(CreateStructOrVariantType(variant, struct_type));
                        variant->type(variant_type);
                        variants.push_back(variant_type);
                    }

                    struct_type->variants(std::move(variants));
                }

                AnalyseType(module, (Type*&) struct_type);
                return struct_type;
            };

            s->type(CreateStructOrVariantType(s, nullptr));
        } break;

        case Statement::Kind::DeclAlias: {
            auto s = as<AliasDecl>(statement);
            AnalyseType(module, s->type());
        } break;

        case Statement::Kind::Block: {
            for (auto& child : as<BlockStatement>(statement)->children()) {
                Analyse(module, child);
                LCC_ASSERT(child->sema_done_or_errored());
            }
        } break;

        case Statement::Kind::Return: {
            LCC_ASSERT(curr_func);
            auto s = as<ReturnStatement>(statement);

            if (curr_func->return_type()->is_noreturn()) {
                statement->set_sema_errored();
                Error(s->location(), "Cannot return from noreturn function.");
            }

            if (s->is_void_return()) {
                if (not curr_func->return_type()->is_void()) {
                    statement->set_sema_errored();
                    Error(s->location(), "Nonvoid function requires a return value.");
                }
            } else {
                Analyse(module, s->value());
                if (curr_func->return_type()->is_void()) {
                    statement->set_sema_errored();
                    Error(s->location(), "Cannot return a value from a void function.");
                } else {
                    LCC_ASSERT(curr_func->return_type()->sema_done_or_errored());
                    ConvertOrError(module, s->value(), curr_func->return_type());
                }
            }
        } break;

        case Statement::Kind::Xyzzy: {
            // Nothing happened.
        } break;

        case Statement::Kind::If: {
            auto s = as<IfStatement>(statement);

            Analyse(module, s->condition(), Type::Bool);
            ConvertOrError(module, s->condition(), Type::Bool);

            Analyse(module, s->pass());
            if (s->fail()) Analyse(module, s->fail());
        } break;

        case Statement::Kind::Assign: {
            auto s = as<AssignStatement>(statement);

            Analyse(module, s->target());
            LCC_ASSERT(s->target()->type());

            Analyse(module, s->value());
            LCC_ASSERT(s->value()->type());

            if (!s->target()->is_lvalue()) {
                Error(s->target()->location(), "Cannot assign to a non-lvalue");
                statement->set_sema_errored();
            } else {
                if (s->target()->type()->is_reference() and s->value()->type()->is_reference()) {
                    if (TryConvert(module, s->value(), s->target()->type()) >= 0)
                        ConvertOrError(module, s->value(), s->target()->type());
                }

                auto nonref_target_type = s->target()->type()->strip_references();
                if (not Convert(module, s->value(), nonref_target_type)) {
                    Error(
                        s->value()->location(),
                        "Expression of type {} is not convertible to type {}",
                        s->value()->type()->string(use_colours),
                        nonref_target_type->string(use_colours)
                    );
                }
            }

            if (not s->target()->sema_ok() or not s->value()->sema_ok()) {
                statement->set_sema_errored();
            }
        } break;

        case Statement::Kind::Expr: {
            auto s = as<ExprStatement>(statement);
            LCC_ASSERT(s->expr());
            AnalyseAndDiscard(module, s->expr());
        } break;

        default: {
            Error(statement->location(), "Unhandled statement in Sema::Analyze(Statement*&): {}", ToString(kind));
            statement->set_sema_errored();
        } break;
    }
}

bool layec::Sema::Analyse(Module* module, Expr*& expr, Type* expected_type) {
    LCC_ASSERT(curr_func);

    if (expr->sema_state() != SemaState::NotAnalysed)
        return expr->sema_ok();
    expr->set_sema_in_progress();

    auto UnknownSymbol = [&](const std::string& symbol_name) {
        Error(expr->location(), "Unknown symbol '{}'", symbol_name);
        expr->set_sema_errored();
        expr->type(new (*module) PoisonType{expr->location()});
    };

    auto kind = expr->kind();
    switch (kind) {
        case Expr::Kind::Cast: {
            auto e = as<CastExpr>(expr);
            AnalyseType(module, e->target_type());
            Analyse(module, e->value(), e->target_type());

            if (e->cast_kind() == CastKind::ImplicitCast or e->cast_kind() == CastKind::LValueToRValueConv) {
                expr->type(e->target_type());
                break;
            }

            if (not Analyse(module, e->value(), e->target_type())) {
                expr->type(new (*module) PoisonType{expr->location()});
                expr->set_sema_errored();
                break;
            }

            if (Convert(module, e->value(), e->target_type())) {
                expr->type(e->target_type());
                break;
            }

            auto from = e->value()->type();
            auto to = e->target_type();

            if (to->is_reference()) {
                Error(e->location(), "Invalid cast of rvalue to reference type");
                expr->set_sema_errored();
                expr->type(new (*module) PoisonType{expr->location()});
                break;
            }

            if (from->is_integer() and to->is_integer()) {
                expr->type(e->target_type());
                break;
            }

            if (from->is_rawptr() and to->is_integer()) {
                expr->type(e->target_type());
                break;
            }

            LCC_ASSERT(false, "Unhandled case for cast expr {} -> {}", e->value()->type()->string(use_colours), e->target_type()->string(use_colours));
        } break;

        case Expr::Kind::LookupName: {
            auto e = as<NameExpr>(expr);

            LCC_ASSERT(false, "Analyse(module, LookupName)");

            // auto entity = LookupManyEntitiesFrom(e->scope(), e->name(), e->location());

            // if (not entity) {
            //     UnknownSymbol(e->name());
            // } else if (auto binding_decl = cast<BindingDecl>(entity)) {
            //     if (binding_decl->sema_state() == SemaState::InProgress) {
            //         Error(expr->location(), "Cannot use '{}' in its own initialiser", e->name());
            //         expr->set_sema_errored();
            //         expr->type(new (*module) PoisonType{expr->location()});
            //         break;
            //     }

            //     e->target(binding_decl);
            //     e->type(Ref(binding_decl->type(), TypeAccess::Mutable));
            // } else if (auto function_decl = cast<FunctionDecl>(entity)) {
            //     e->target(function_decl);
            //     e->type(function_decl->function_type());
            // } else if (auto overload_set = cast<OverloadSet>(entity)) {
            //     // NOTE(local): the case of overload sets is a little trickier on whether or not
            //     e->target(overload_set);
            //     e->type(Type::OverloadSet);
            // } else {
            //     UnknownSymbol(e->name());
            // }
        } break;

        case Expr::Kind::LookupPath: {
            auto e = as<PathExpr>(expr);

            auto entity = LookupValueEntity(module, e->scope(), e->names(), e->locations());
            LCC_ASSERT(false, "Analyse(module, LookupPath)");

            // auto first_name = path_names[0];
            // auto import_lookup = module()->lookup_import(first_name);
            // if (not import_lookup) {
            //     UnknownSymbol(first_name);
            //     break;
            // }

            // auto curr_module = import_lookup->module;
            // for (usz i = 1; i < path_names.size(); i++) {
            //     const auto& path_name = path_names[i];
            //     const auto& path_location = e->locations()[i];
            //     bool is_last_name = i == path_names.size() - 1;

            //     if (is_last_name) {
            //         auto module_exports = curr_module->exports();
            //         auto entity = LookupManyEntitiesWithin(module_exports, path_name, path_location);

            //         if (not entity) {
            //             UnknownSymbol(path_name);
            //         } else if (auto binding_decl = cast<BindingDecl>(entity)) {
            //             if (binding_decl->sema_state() == SemaState::InProgress) {
            //                 Error(expr->location(), "Cannot use '{}' in its own initialiser", path_name);
            //                 expr->set_sema_errored();
            //                 expr->type(new (*module) PoisonType{expr->location()});
            //                 break;
            //             }

            //             e->target(binding_decl);
            //             e->type(binding_decl->type());
            //         } else if (auto function_decl = cast<FunctionDecl>(entity)) {
            //             e->target(function_decl);
            //             e->type(function_decl->function_type());
            //         } else if (auto overload_set = cast<OverloadSet>(entity)) {
            //             e->target(overload_set);
            //             e->type(Type::OverloadSet);
            //         } else {
            //             UnknownSymbol(path_name);
            //         }
            //     } else {
            //         LCC_ASSERT(false, "Sema doesn't know how to go multiple levels deep on paths yet");
            //     }
            // }
        } break;

        case Expr::Kind::FieldIndex: {
            auto e = as<FieldIndexExpr>(expr);
            Analyse(module, e->target());
            LCC_ASSERT(e->target()->type());

            if (!e->target()->is_lvalue()) {
                Error(expr->location(), "Cannot lookup a field from a non-lvalue");
                expr->set_sema_errored();
                expr->type(new (*module) PoisonType{expr->location()});
                break;
            }

            auto nonref_target_type = e->target()->type()->strip_references();
            if (auto struct_type = cast<StructType>(nonref_target_type)) {
                auto lookup = rgs::find_if(struct_type->fields(), [e](StructField field) { return field.name == e->field_name(); });
                if (lookup == struct_type->fields().end()) {
                    Error(expr->location(), "No such field '{}' in {}", e->field_name(), struct_type->string(use_colours));
                    expr->set_sema_errored();
                    expr->type(new (*module) PoisonType{expr->location()});
                    break;
                }

                auto access = as<ReferenceType>(e->target()->type())->access();
                expr->type(Ref(module, lookup->type, access));
            } else {
                Error(expr->location(), "Cannot lookup a field from a non-struct type");
                expr->set_sema_errored();
                expr->type(new (*module) PoisonType{expr->location()});
            }
        } break;

        case Expr::Kind::Call: {
            auto e = as<CallExpr>(expr);

            for (auto& arg : e->args())
                Analyse(module, arg);

            if (not Analyse(module, e->target())) {
                expr->set_sema_errored();
                expr->type(new (*module) PoisonType{expr->location()});
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
                        Error(expr->location(), "Expected {} arguments to call, got {}.", param_types.size(), e->args().size());
                        expr->set_sema_errored();
                        expr->type(new (*module) PoisonType{expr->location()});
                        return;
                    }

                    for (unsigned i = 0; i < e->args().size(); i++) {
                        auto& arg = e->args()[i];
                        ConvertOrError(module, arg, param_types[i]);
                    }

                    LCC_ASSERT(not function_type->return_type()->is_named_type());
                    expr->type(function_type->return_type());
                } else {
                    Error(e->target()->location(), "Cannot call non-function value");
                    expr->set_sema_errored();
                    expr->type(new (*module) PoisonType{expr->location()});
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

        case Expr::Kind::Unary: {
            auto e = as<UnaryExpr>(expr);
            Analyse(module, e->value());

            switch (e->operator_kind()) {
                default: {
                    LCC_ASSERT(false, "unimplemented unary operator {}", ToString(e->operator_kind()));
                } break;

                case OperatorKind::Address: {
                    if (not e->value()->is_lvalue()) {
                        Error(expr->location(), "Cannot take the address of a non-lvalue expression");
                        expr->set_sema_errored();
                        expr->type(new (*module) PoisonType{expr->location()});
                        break;
                    }

                    LCC_ASSERT(e->value()->type()->is_reference());
                    auto ref_type = as<ReferenceType>(e->value()->type());
                    expr->type(Ptr(module, ref_type->elem_type(), ref_type->access()));
                } break;

                case OperatorKind::Deref: {
                    PointerType* pointer_type = nullptr;
                    Type*& value_type = e->value()->type();

                    auto value_type_noref = value_type->strip_references();
                    if (not value_type_noref->is_pointer())
                        goto cannot_dereference_type;

                    // make sure we have that pointer type; if it's the same, it's a noop
                    if (not Convert(module, e->value(), value_type_noref)) {
                        expr->set_sema_errored();
                        expr->type(new (*module) PoisonType{expr->location()});
                        break;
                    }

                    pointer_type = as<PointerType>(value_type_noref);
                    if (pointer_type->elem_type()->is_void() or pointer_type->elem_type()->is_noreturn())
                        goto cannot_dereference_type;

                    expr->type(pointer_type->elem_type());
                    break;

                cannot_dereference_type:
                    Error(expr->location(), "Cannot dereference type {}", e->value()->type()->string(use_colours));
                    expr->set_sema_errored();
                    expr->type(new (*module) PoisonType{expr->location()});
                } break;
            }
        } break;

        case Expr::Kind::Binary: {
            auto e = as<BinaryExpr>(expr);

            Analyse(module, e->lhs());
            Analyse(module, e->rhs());

            switch (e->operator_kind()) {
                default: {
                    LCC_ASSERT(false, "unimplemented binary operator {}", ToString(e->operator_kind()));
                } break;

                case OperatorKind::Add:
                case OperatorKind::Sub:
                case OperatorKind::Mul:
                case OperatorKind::Div:
                case OperatorKind::Mod: {
                    auto lhs_type = LValueToRValue(module, e->lhs());
                    auto rhs_type = LValueToRValue(module, e->rhs());

                    if (not lhs_type->is_number()) {
                        Error(
                            e->lhs()->location(),
                            "Cannot use type {} in operator {}",
                            lhs_type->string(),
                            ToString(e->operator_kind())
                        );
                        expr->set_sema_errored();
                        expr->type(new (*module) PoisonType{expr->location()});
                    } else if (not rhs_type->is_number()) {
                        Error(
                            e->rhs()->location(),
                            "Cannot use type {} in operator {}",
                            rhs_type->string(),
                            ToString(e->operator_kind())
                        );
                        expr->set_sema_errored();
                        expr->type(new (*module) PoisonType{expr->location()});
                    }

                    if (expr->sema_errored())
                        break;

                    if (not ConvertToCommonType(module, e->lhs(), e->rhs())) {
                        expr->set_sema_errored();
                        expr->type(new (*module) PoisonType{expr->location()});
                        break;
                    }

                    LCC_ASSERT(Type::Equal(e->lhs()->type(), e->rhs()->type()));
                    expr->type(e->lhs()->type());
                } break;

                case OperatorKind::And:
                case OperatorKind::Or:
                case OperatorKind::Xor:
                case OperatorKind::Lsh:
                case OperatorKind::Rsh: {
                    if (not e->lhs()->type()->is_integer()) {
                        Error(
                            e->lhs()->location(),
                            "Cannot use type {} in operator {}",
                            e->lhs()->type()->string(),
                            ToString(e->operator_kind())
                        );
                        expr->set_sema_errored();
                        expr->type(new (*module) PoisonType{expr->location()});
                    } else if (not e->rhs()->type()->is_integer()) {
                        Error(
                            e->rhs()->location(),
                            "Cannot use type {} in operator {}",
                            e->rhs()->type()->string(),
                            ToString(e->operator_kind())
                        );
                        expr->set_sema_errored();
                        expr->type(new (*module) PoisonType{expr->location()});
                    }

                    if (expr->sema_errored())
                        break;

                    if (not ConvertToCommonType(module, e->lhs(), e->rhs())) {
                        expr->set_sema_errored();
                        expr->type(new (*module) PoisonType{expr->location()});
                        break;
                    }

                    LCC_ASSERT(Type::Equal(e->lhs()->type(), e->rhs()->type()));
                    expr->type(e->lhs()->type());
                } break;

                case OperatorKind::Equal:
                case OperatorKind::NotEqual: {
                    expr->type(Type::Bool);
                    if (not ConvertToCommonType(module, e->lhs(), e->rhs())) {
                        expr->set_sema_errored();
                        break;
                    }

                    // TODO(local): actually decide what a valid equality compare is
                } break;

                case OperatorKind::Greater:
                case OperatorKind::GreaterEqual:
                case OperatorKind::Less:
                case OperatorKind::LessEqual: {
                    expr->type(Type::Bool);

                    if (not e->lhs()->type()->is_number()) {
                        Error(
                            e->lhs()->location(),
                            "Cannot use type {} in operator {}",
                            e->lhs()->type()->string(),
                            ToString(e->operator_kind())
                        );
                        expr->set_sema_errored();
                        expr->type(new (*module) PoisonType{expr->location()});
                    } else if (not e->rhs()->type()->is_number()) {
                        Error(
                            e->rhs()->location(),
                            "Cannot use type {} in operator {}",
                            e->rhs()->type()->string(),
                            ToString(e->operator_kind())
                        );
                        expr->set_sema_errored();
                        expr->type(new (*module) PoisonType{expr->location()});
                    }

                    if (expr->sema_errored())
                        break;

                    if (not ConvertToCommonType(module, e->lhs(), e->rhs())) {
                        expr->set_sema_errored();
                        expr->type(new (*module) PoisonType{expr->location()});
                        break;
                    }

                    LCC_ASSERT(Type::Equal(e->lhs()->type(), e->rhs()->type()));
                } break;
            }
        } break;

        case Expr::Kind::LitString: {
            expr->type(new (*module) LiteralStringType(expr->location()));
        } break;

        case Expr::Kind::LitInt: {
            expr->type(new (*module) IntType(expr->location(), true, (int) context()->target()->size_of_pointer, true));
        } break;

        default: {
            if (auto t = cast<Type>(expr)) return AnalyseType(module, t);
            Error(expr->location(), "Unhandled expression in Sema::Analyze(Expr*&): {}", ToString(kind));
            expr->set_sema_errored();
            expr->type(new (*module) PoisonType{expr->location()});
        } break;
    }

    if (not expr->sema_done_or_errored())
        expr->set_sema_done();

    LCC_ASSERT(expr->type(), "for expr of kind '{}'", ToString(expr->kind()));
    return expr->sema_ok();
}

bool layec::Sema::AnalyseType(Module* module, Type*& type) {
    if (type->sema_done_or_errored()) return type->sema_ok();
    type->set_sema_in_progress();

    auto kind = type->kind();
    switch (kind) {
        default: {
            Error(type->location(), "Unhandled type in Sema::Analyze(Type*&): {}", ToString(kind));
            type->set_sema_errored();
        } break;

        case Expr::Kind::TypePoison: {
        } break;

        case Expr::Kind::TypeInfer: {
            Error(type->location(), "Invalid context for `var` type.");
            type->set_sema_errored();
        } break;

        case Expr::Kind::TypeNilable: {
            auto t = as<NilableType>(type);
            bool r = AnalyseType(module, t->elem_type());
            if (not r)
                type->set_sema_errored();
            return r;
        }

        case Expr::Kind::TypeErrUnion: {
            LCC_ASSERT(false, "Error Unions need to be updated, then sema can handle them");
        } break;

        case Expr::Kind::TypeLookupName: {
            auto t = as<NameType>(type);

            auto type_entity = LookupTypeEntity(module, t->scope(), t->name());
            if (auto alias_decl = cast<AliasDecl>(type_entity)) {
                LCC_ASSERT(alias_decl->type()->sema_done_or_errored());
                type = alias_decl->type();
            } else if (auto struct_decl = cast<StructDecl>(type_entity)) {
                LCC_ASSERT(struct_decl->type()->sema_done_or_errored());
                type = struct_decl->type();
            } else {
                Error(type->location(), "Unknown type symbol '{}'", t->name());
                type = new (*module) PoisonType(type->location());
            }

            LCC_ASSERT(not type->is_named_type());
        } break;

        case Expr::Kind::TypeLookupPath: {
            auto t = as<PathType>(type);

            LCC_ASSERT(false, "AnalyseType(module, TypeLookupName)");

            // const auto& path_names = t->names();
            // const auto& locations = t->locations();
            // LCC_ASSERT(not path_names.empty());
            // LCC_ASSERT(path_names.size() == locations.size());

            // auto entity = LookupSingleEntityFrom(t->scope(), path_names[0]);
            // if (entity) {
            //     if (auto struct_decl = cast<StructDecl>(entity)) {
            //         auto NoVariantInStruct = [&](Location location, const std::string& name, StructDecl* sd) {
            //             Error(location, "No variant '{}' in struct {}", name, sd->type()->string(use_colours));
            //             type->set_sema_errored();
            //         };

            //         StructDecl* curr_struct_decl = struct_decl;
            //         for (usz i = 1; i < path_names.size(); i++) {
            //             const auto& path_name = path_names[i];
            //             const auto& path_location = locations[i];

            //             const auto& variants = curr_struct_decl->variants();
            //             if (variants.empty()) {
            //                 NoVariantInStruct(path_location, path_name, curr_struct_decl);
            //                 break;
            //             }

            //             auto variant_it = rgs::find_if(variants, [&](StructDecl* v) { return v->name() == path_name; });
            //             if (variant_it == variants.end()) {
            //                 NoVariantInStruct(path_location, path_name, curr_struct_decl);
            //                 break;
            //             }

            //             curr_struct_decl = *variant_it;
            //         }

            //         if (type->sema_done_or_errored())
            //             break;

            //         type = curr_struct_decl->type();
            //         LCC_ASSERT(type);
            //         LCC_ASSERT(type->kind() == Expr::Kind::TypeVariant);
            //         LCC_ASSERT(type->sema_done_or_errored());
            //     }
            // }

            // if (type->sema_done_or_errored())
            //     break;

            // LCC_ASSERT(false, "need to handle type lookup through namespaces");
        } break;

        case Expr::Kind::TypeArray: {
            auto t = as<ArrayType>(type);
            if (not AnalyseType(module, t->elem_type()))
                type->set_sema_errored();

            if (t->elem_type()->is_void()) {
                Error(t->elem_type()->location(), "Void is not a valid container element type");
                type->set_sema_errored();
            }

            if (t->elem_type()->is_noreturn()) {
                Error(t->elem_type()->location(), "Noreturn is only valid as a function return type");
                type->set_sema_errored();
            }

            auto rank_length_exprs = t->rank_lengths();
            for (usz i = 0; i < rank_length_exprs.size(); i++) {
                auto& rank_length_expr = rank_length_exprs[i];

                EvalResult res;
                if (not rank_length_expr->evaluate(laye_context(), res, true)) {
                    type->set_sema_errored();
                    LCC_TODO();
                    continue;
                }

                if (!res.is_i64()) {
                    type->set_sema_errored();
                    LCC_TODO();
                    continue;
                }

                rank_length_expr = new (*module) ConstantExpr(rank_length_expr, res);
            }
        } break;

        case Expr::Kind::TypeSlice:
        case Expr::Kind::TypePointer:
        case Expr::Kind::TypeReference:
        case Expr::Kind::TypeBuffer: {
            auto t = as<SingleElementType>(type);
            if (not AnalyseType(module, t->elem_type()))
                type->set_sema_errored();

            if (t->elem_type()->is_void()) {
                Error(t->elem_type()->location(), "Void is not a valid container element type");
                type->set_sema_errored();
            }

            if (t->elem_type()->is_noreturn()) {
                Error(t->elem_type()->location(), "Noreturn is only valid as a function return type");
                type->set_sema_errored();
            }
        } break;

        case Expr::Kind::TypeFunc: {
            auto t = as<FuncType>(type);
            if (not t->return_type()->is_void() and not t->return_type()->is_noreturn()) {
                if (not AnalyseType(module, t->return_type()))
                    type->set_sema_errored();
            }

            for (auto& param_type : t->param_types()) {
                if (not AnalyseType(module, param_type))
                    type->set_sema_errored();
            }
        } break;

        case Expr::Kind::TypeStruct:
        case Expr::Kind::TypeVariant: {
            auto t = as<StructType>(type);
            for (auto& field : t->fields())
                AnalyseType(module, field.type);
            for (auto& variant : t->variants())
                AnalyseType(module, (Type*&) variant);
        } break;

        case Expr::Kind::TypeNoreturn: {
        } break;

        case Expr::Kind::TypeRawptr: {
        } break;

        case Expr::Kind::TypeVoid: {
        } break;

        case Expr::Kind::TypeBool:
        case Expr::Kind::TypeInt: {
            auto t = as<SizableType>(type);
            if (t->is_platform()) {
                t->bit_width(int(context()->target()->size_of_pointer));
            } else if (t->bit_width() <= 0 or t->bit_width() > 65535) {
                Error(type->location(), "Primitive type bit width must be in the range (0, 65535]");
                type->set_sema_errored();
            }
        } break;

        case Expr::Kind::TypeFloat: {
            auto t = as<FloatType>(type);
            if (t->is_platform()) {
                t->bit_width(64);
            } else if (t->bit_width() <= 0 or t->bit_width() > 65535) {
                Error(type->location(), "Primitive type bit width must be in the range (0, 65535]");
                type->set_sema_errored();
            }
        } break;
    }

    if (not type->sema_done_or_errored())
        type->set_sema_done();

    LCC_ASSERT(not type->is_named_type());
    return type->sema_ok();
}

#if false
auto layec::Sema::LookupSingleEntityWithin(Scope* scope, const std::string& name) -> NamedDecl* {
    auto symbols = scope->find(name);
    if (symbols.first == symbols.second)
        return nullptr;

    NamedDecl* decl = symbols.first->second;
    if (scope->module() != module())
    {
        
    }
}

auto layec::Sema::LookupSingleEntityFrom(Scope* scope, const std::string& name) -> NamedDecl* {
    while (scope) {
        auto lookup = LookupSingleEntityWithin(scope, name);
        if (lookup) return lookup;
        scope = scope->parent();
    }

    return nullptr;
}

auto layec::Sema::LookupManyEntitiesWithin(Scope* scope, const std::string& name, Location location) -> NamedDecl* {
    decltype(scope->find(name)) symbols = scope->find(name);
    if (symbols.first == symbols.second)
        return nullptr;

    auto entity = symbols.first->second;
    if ([[maybe_unused]] auto function_decl = cast<FunctionDecl>(entity)) {
        std::vector<FunctionDecl*> overloads{};
        for (auto it = symbols.first; it != symbols.second; it++)
            overloads.push_back(as<FunctionDecl>(it->second));

        if (overloads.size() == 1)
            return overloads[0];

        entity = new (*module) OverloadSet(module(), location, name, overloads);
        Analyse(module, (Statement*&) entity);
    }

    return entity;
}

auto layec::Sema::LookupManyEntitiesFrom(Scope* scope, const std::string& name, Location location) -> NamedDecl* {
    decltype(scope->find(name)) symbols;

    while (scope) {
        // scope->debug_print();
        symbols = scope->find(name);
        scope = scope->parent();
        if (symbols.first != symbols.second) break;
    }

    if (symbols.first == symbols.second)
        return nullptr;

    auto entity = symbols.first->second;
    if ([[maybe_unused]] auto function_decl = cast<FunctionDecl>(entity)) {
        std::vector<FunctionDecl*> overloads;
        auto AppendOverloads = [&overloads](auto&& range) {
            for (auto it = range.first; it != range.second; it++)
                overloads.push_back(as<FunctionDecl>(it->second));
        };

        AppendOverloads(symbols);
        for (; scope; scope = scope->parent())
            AppendOverloads(scope->find(name));

        // TODO(local): include overloads in imported scopes

        if (overloads.size() == 1)
            return overloads[0];

        entity = new (*module) OverloadSet(module(), location, name, overloads);
        Analyse(module, (Statement*&) entity);
    }

    return entity;
}
#endif

template <bool PerformConversion>
int layec::Sema::ConvertImpl(Module* module, Expr*& expr, Type* to) {
    enum : int {
        TypesContainErrors = -2,
        ConversionImpossible = -1,
        NoOp = 0,
    };

    auto from = expr->type();
    if (from->sema_errored() or to->sema_errored()) return TypesContainErrors;

    /// This is so we don’t forget that we’ve applied lvalue-to-rvalue
    /// conversion and raised the score by one.
    bool requires_lvalue_to_rvalue_conversion = false;
    auto Score = [requires_lvalue_to_rvalue_conversion](int i) {
        LCC_ASSERT(i >= 1, "Score must be 1 or greater. Use the enum constants above for values <= 0");
        return i + int(requires_lvalue_to_rvalue_conversion);
    };

    if (Type::Equal(from, to))
        return NoOp;

    /// Get reference-to-reference conversions out of the way early.
    if (from->is_reference() and to->is_reference()) {
        /// A reference can be converted to the same reference.
        if (Type::Equal(from, to)) return NoOp;

        auto from_ref = as<ReferenceType>(from);
        auto to_ref = as<ReferenceType>(to);

        // If the two reference types have the same element type and compatible type access modifiers, it's a noop
        if (Type::Equal(from_ref->elem_type(), to_ref->elem_type())) {
            // notably, compatible type access means either equal, or the target is stricter
            if (from_ref->access() == to_ref->access() or to_ref->access() == TypeAccess::ReadOnly)
                return NoOp;
        }

        /// References to variants can be converted to references of any parent struct type.
        auto from_variant = cast<VariantType>(from_ref->elem_type());
        auto to_struct = cast<StructType>(to_ref->elem_type());
        if (from_variant and to_struct and from_variant->inherits_from(to_struct)) {
            if constexpr (PerformConversion) InsertImplicitCast(module, expr, to);
            return Score(1);
        }

        /// References to arrays can be converted to references to
        /// the first element.
        auto arr = cast<ArrayType>(as<ReferenceType>(from)->elem_type());
        if (arr and Type::Equal(arr->elem_type(), as<ReferenceType>(to)->elem_type())) {
            if constexpr (PerformConversion) InsertImplicitCast(module, expr, to);
            return Score(1);
        }

        return ConversionImpossible;
    }

    requires_lvalue_to_rvalue_conversion = expr->is_lvalue();
    if constexpr (PerformConversion) from = LValueToRValue(module, expr);
    else from = from->strip_references();

    if (Type::Equal(from, to))
        return NoOp;

    if (from->is_pointer() and to->is_pointer()) {
        auto from_ptr = as<PointerType>(from);
        auto to_ptr = as<PointerType>(to);

        // If the two pointer types have the same element type and compatible type access modifiers, it's a noop
        if (Type::Equal(from_ptr->elem_type(), to_ptr->elem_type())) {
            // notably, compatible type access means either equal, or the target is stricter
            if (from_ptr->access() == to_ptr->access() or to_ptr->access() == TypeAccess::ReadOnly)
                return NoOp;
        }
    }

    if (from->is_integer() and to->is_rawptr()) {
        if constexpr (PerformConversion) InsertImplicitCast(module, expr, to);
        return Score(2);
    }

    if ((from->is_buffer() or from->is_pointer()) and to->is_rawptr()) {
        if constexpr (PerformConversion) InsertImplicitCast(module, expr, to);
        return Score(3);
    }

    if (from->is_rawptr() and (to->is_buffer() or to->is_pointer())) {
        if constexpr (PerformConversion) InsertImplicitCast(module, expr, to);
        return Score(3);
    }

    if (from->is_integer() and to->is_bool()) {
        if constexpr (PerformConversion) InsertImplicitCast(module, expr, to);
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
                InsertImplicitCast(module, expr, to);

                expr = new (*module) ConstantExpr(expr, res);
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
                InsertImplicitCast(module, expr, to);
            }

            return Score(1);
        }
    }

    if (from->is_function() and to->is_pointer() and Type::Equal(cast<SingleElementType>(to)->elem_type(), from)) {
        if constexpr (PerformConversion) InsertImplicitCast(module, expr, to);
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
                    InsertImplicitCast(module, expr, to);

                    expr = new (*module) ConstantExpr(expr, res);
                    expr->type(to);
                }

                return Score(1);
            }

            if constexpr (PerformConversion)
                InsertImplicitCast(module, expr, to);

            return Score(1);
        }
    }

    return ConversionImpossible;
}

bool layec::Sema::Convert(Module* module, Expr*& expr, Type* to) {
    if (expr->sema_errored()) return true;
    return ConvertImpl<true>(module, expr, to) >= 0;
}

void layec::Sema::ConvertOrError(Module* module, Expr*& expr, Type* to) {
    if (not Convert(module, expr, to)) Error(
        expr->location(),
        "Expression of type {} ({}) is not convertible to type {} ({})",
        expr->type()->string(use_colours),
        ToString(expr->type()->kind()),
        to->string(use_colours),
        ToString(to->kind())
    );
}

bool layec::Sema::ConvertToCommonType(Module* module, Expr*& a, Expr*& b) {
    return Convert(module, a, b->type()) or Convert(module, b, a->type());
}

int layec::Sema::TryConvert(Module* module, Expr*& expr, Type* to) {
    return ConvertImpl<false>(module, expr, to);
}

void layec::Sema::Discard(Module* module, Expr*& expr) {
    LCC_ASSERT(expr->sema_done_or_errored());
    if (auto call_expr = cast<CallExpr>(expr)) {
        [[maybe_unused]] auto call_target = call_expr->target();
        // Warning(expr->location(), "Do this later (nodiscard in sema) !");
    } else {
        Error(expr->location(), "Nonsense!");
    }
}

bool layec::Sema::HasSideEffects(Expr* expr) {
    LCC_ASSERT(false);
}

void layec::Sema::InsertImplicitCast(Module* module, Expr*& expr, Type* ty) {
    WrapWithCast(module, expr, ty, CastKind::ImplicitCast);
}

void layec::Sema::InsertPointerToIntegerCast(Module* module, Expr*& operand) {
    LCC_ASSERT(false);
}

void layec::Sema::WrapWithCast(Module* module, Expr*& expr, Type* type, CastKind kind) {
    auto wrapper = new (*module) CastExpr(expr->location(), type, expr, kind);
    Analyse(module, (Expr*&) wrapper);
    expr = wrapper;
}

auto layec::Sema::LValueToRValue(Module* module, Expr*& expr) -> Type* {
    /// Functions are cast to function pointers.
    if (expr->type()->is_function()) {
        auto ty = Ptr(module, expr->type(), TypeAccess::Mutable);
        WrapWithCast(module, expr, ty, CastKind::LValueToRValueConv);
        return ty;
    }

    /// Otherwise, remove references and cast to that.
    auto ty = expr->type()->strip_references();
    if (not Type::Equal(ty, expr->type()))
        WrapWithCast(module, expr, ty, CastKind::LValueToRValueConv);

    return ty;
}

auto layec::Sema::Ptr(Module* module, Type* type, TypeAccess access) -> PointerType* {
    Type* ptr = new (*module) PointerType(type->location(), access, type);
    AnalyseType(module, ptr);
    return as<PointerType>(ptr);
}

auto layec::Sema::Ref(Module* module, Type* type, TypeAccess access) -> ReferenceType* {
    Type* ptr = new (*module) ReferenceType(type->location(), access, type);
    AnalyseType(module, ptr);
    return as<ReferenceType>(ptr);
}

auto layec::Sema::NameToMangledString(std::string_view s) -> std::string {
    return fmt::format("{}_{}", s.size(), s);
}

auto layec::Sema::TypeToMangledString(Type* type) -> std::string {
    switch (type->kind()) {
        default: LCC_TODO();

        case Expr::Kind::TypeStruct: {
            auto t = as<StructType>(type);
            return NameToMangledString(t->name());
        }

        case Expr::Kind::TypeVariant: {
            auto t = as<VariantType>(type);
            std::string result = "V";

            std::function<void(const StructType*)> AppendToResult;
            AppendToResult = [&](const StructType* struct_type) {
                if (auto variant_type = cast<VariantType>(struct_type)) {
                    AppendToResult(variant_type->parent_struct());
                }
                result += NameToMangledString(struct_type->name());
            };

            AppendToResult(t);
            return result + "E";
        }

        case Expr::Kind::TypeNilable: {
            auto t = as<NilableType>(type);
            return fmt::format("n{}", TypeToMangledString(t->elem_type()));
        }

        case Expr::Kind::TypeLookupName: {
            auto t = as<NameType>(type);
            return NameToMangledString(t->name());
        }

        case Expr::Kind::TypeLookupPath: {
            auto t = as<PathType>(type);
            std::string result = "N";
            for (auto& path : t->names()) {
                result += NameToMangledString(path);
            }
            return result + "E";
        }

        case Expr::Kind::TypeLiteralString: LCC_UNREACHABLE();

        case Expr::Kind::TypeArray: {
            auto t = as<ArrayType>(type);
            std::string result = fmt::format("Ca{}{}_", TypeToMangledString(t->elem_type()), t->rank());
            for (auto& len_expr : t->rank_lengths()) {
                i64 len = as<ConstantExpr>(len_expr)->value().as_i64();
                result += fmt::format("{}_", len);
            }
            return result;
        }

        case Expr::Kind::TypeSlice: {
            auto t = as<SliceType>(type);
            return fmt::format("Cs{}", TypeToMangledString(t->elem_type()));
        }

        case Expr::Kind::TypeReference: {
            auto t = as<ReferenceType>(type);
            return fmt::format("Cr{}", TypeToMangledString(t->elem_type()));
        }

        case Expr::Kind::TypePointer: {
            auto t = as<PointerType>(type);
            return fmt::format("Cp{}", TypeToMangledString(t->elem_type()));
        }

        case Expr::Kind::TypeBuffer: {
            auto t = as<BufferType>(type);
            return fmt::format("Cb{}", TypeToMangledString(t->elem_type()));
        }

        case Expr::Kind::TypeFunc: {
            auto t = as<FuncType>(type);
            std::string result = "f";
            result += TypeToMangledString(t->return_type());
            // TODO(local): template params, varargs
            result += "P";
            for (auto& param_type : t->param_types()) {
                result += TypeToMangledString(param_type);
            }
            result += "E";
            return result;
        }

        case Expr::Kind::TypeNoreturn: return "X";
        case Expr::Kind::TypeRawptr: return "x";
        case Expr::Kind::TypeVoid: return "v";

        case Expr::Kind::TypeBool: {
            auto t = as<BoolType>(type);
            if (t->is_platform()) return "b";
            return fmt::format("Sb{}_", t->bit_width());
        }

        case Expr::Kind::TypeInt: {
            auto t = as<IntType>(type);
            auto c = t->is_signed() ? "i" : "u";
            if (t->is_platform()) return c;
            else {
                return fmt::format("S{}{}_", c, t->bit_width());
            }
        }

        case Expr::Kind::TypeFloat: {
            auto t = as<FloatType>(type);
            if (t->is_platform()) return "f";
            return fmt::format("Sf{}_", t->bit_width());
        }
    }
}

void layec::Sema::MangleName(NamedDecl* decl) {
    if (decl->is_foreign())
        return;

    std::string module_name;
    {
        auto file_id = decl->location().file_id;
        const auto& file = *context()->files()[file_id].get();
        module_name = fs::path{file.path()}.filename().replace_extension("").string();
    }

    std::string mangled_name = fmt::format(
        "_LM{}X{}",
        NameToMangledString(module_name),
        NameToMangledString(decl->mangled_name())
    );

    if (auto func_decl = cast<FunctionDecl>(decl)) {
        mangled_name += "F";
        mangled_name += TypeToMangledString(func_decl->return_type());
        // TODO(local): template params, varargs
        mangled_name += "P";
        for (auto& param : func_decl->params()) {
            mangled_name += TypeToMangledString(param->type());
        }
        mangled_name += "E";
    } else if (auto binding_decl = cast<BindingDecl>(decl)) {
        mangled_name += "B";
        mangled_name += TypeToMangledString(binding_decl->type());
    }

    decl->mangled_name(mangled_name);
}
