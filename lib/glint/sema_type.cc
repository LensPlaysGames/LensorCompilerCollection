#include "lcc/utils/platform.hh"
#include <algorithm>
#include <glint/ast.hh>
#include <glint/sema.hh>

#include <lcc/context.hh>

bool lcc::glint::Sema::AnalyseType(Context* ctx, Module& m, Type** type_ptr) {
    LCC_ASSERT(ctx);
    if (ctx->has_error()) return false;
    Sema s{ctx, m, ctx->option_use_colour()};
    return s.Analyse(type_ptr);
}

auto lcc::glint::Sema::Analyse(Type** type_ptr) -> bool {
    auto* type = *type_ptr;

    // Don’t analyse the same type twice.
    if (type->sema() != SemaNode::State::NotAnalysed)
        return type->ok();
    type->set_sema_in_progress();

    switch (type->kind()) {
        // These are marked as done in the constructor.
        case Type::Kind::Builtin: LCC_UNREACHABLE();

        // These are no-ops.
        case Type::Kind::FFIType:
        case Type::Kind::Type: break;

        // Named types need to be resolved to a type.
        case Type::Kind::Named: {
            auto* n = as<NamedType>(type);
            LCC_ASSERT(not n->name().empty(), "NamedType has empty name");
            LCC_ASSERT(n->scope(), "NamedType {} has NULL scope", n->name());

            // `auto` is a no-op...
            if (TemplatedFuncDecl::is_auto(*type)) {
                type->set_sema_done();
                break;
            }

            // This code is similar to name resolution for expressions,
            // except that we don’t need to worry about overloads.
            Type* ty{};
            for (auto* scope = n->scope(); scope; scope = scope->parent()) {
                auto syms = scope->find(n->name());
                // If we don't find the symbol in this scope, continue searching the
                // parent scope.
                if (syms.empty()) continue;
                if (auto* s = cast<TypeDecl>(syms.at(0))) {
                    Expr* e = s;
                    (void) Analyse(&e);
                    ty = s->type();
                    break;
                }

                if (auto* a = cast<TypeAliasDecl>(syms.at(0))) {
                    Expr* e = a;
                    (void) Analyse(&e);
                    ty = a->type();
                    break;
                }

                Error(n->location(), "'{}' is not a type", n->name())
                    .attach(Note(
                        syms.at(0)->location(),
                        "Because of declaration here",
                        n->name()
                    ));

                n->set_sema_errored();
                break;
            }

            if (not ty) {
                Error(n->location(), "'{}' does not name a type", n->name());
                n->set_sema_errored();
            } else {
                *type_ptr = ty;
            }
        } break;

        /// Pointers to any non-reference types are fine.
        case Type::Kind::Pointer: {
            auto* p = as<PointerType>(type);
            LCC_ASSERT(p->element_type(), "PointerType has NULL element type");
            (void) Analyse(&p->element_type());

            auto* elem = p->element_type();
            if (is<ReferenceType>(elem)) {
                if (elem->ok()) Error(
                    p->location(),
                    "Cannot create pointer to reference type {}",
                    elem
                );
                p->set_sema_errored();
            }
        } break;

        /// References to references are collapsed to a single reference.
        case Type::Kind::Reference: {
            auto* r = as<ReferenceType>(type);
            LCC_ASSERT(r->element_type(), "ReferenceType has NULL element type");
            (void) Analyse(&r->element_type());

            /// Collapse refs.
            while (is<ReferenceType>(r->element_type()))
                r->element_type(r->element_type()->elem());
        } break;

        /// Apply decltype decay to the element type and prohibit
        /// arrays of references. Also check the size.
        case Type::Kind::Array: {
            auto* a = as<ArrayType>(type);
            LCC_ASSERT(a->element_type(), "Array has NULL element type");
            if (not Analyse(&a->element_type())) {
                a->set_sema_errored();
                return false;
            }
            a->element_type(DeclTypeDecay(a->element_type()));

            auto* elem = a->element_type();
            if (is<ReferenceType>(elem)) {
                if (elem->ok()) {
                    Error(
                        a->location(),
                        "Cannot create array of reference type {}",
                        elem
                    );
                } else {
                    Error(
                        a->location(),
                        "Cannot create array of reference type"
                    );
                }
                a->set_sema_errored();
            }

            usz size = 0;
            LCC_ASSERT(a->size(), "Array has NULL size expression");
            if (not Analyse(&a->size())) {
                a->set_sema_errored();
                return false;
            }
            LCC_ASSERT(a->size()->ok());

            EvalResult res;
            if (a->size()->evaluate(context, res, false)) {
                if (res.as_int().slt(1)) {
                    Error(a->location(), "Array size must be greater than 0");
                    a->set_sema_errored();
                    return false;
                }

                size = (usz) res.as_int().value();
                a->size() = new (mod) ConstantExpr(a->size(), EvalResult(size));
            } else {
                // Should be an ICE
                Error(a->location(), "Array with variable size should have been made a dynamic array by the parser");
                Diag::ICE("");
            }
        } break;

        // Apply decltype decay to the element type, prohibit arrays of
        // references, and, if there is an initial size expression, analyse that.
        // Also set cached struct type for IRGen by calling struct_type().
        case Type::Kind::ArrayView: {
            auto* a = as<ArrayViewType>(type);
            LCC_ASSERT(a->element_type(), "ArrayViewType has NULL element type");
            (void) Analyse(&a->element_type());
            a->element_type(DeclTypeDecay(a->element_type()));

            auto* elem = a->element_type();
            if (is<ReferenceType>(elem)) {
                if (elem->ok()) Error(
                    a->location(),
                    "Cannot create dynamic array of reference type {}",
                    elem
                );
                a->set_sema_errored();
            }

            // Cache struct type for IRGen.
            // FIXME: Terrible, no-good check for iterator invalidation, and quickfix.
            if (type_ptr >= mod.types.data() and type_ptr < mod.types.data() + mod.types.size()) {
                auto index = type_ptr - mod.types.data();
                LCC_ASSERT(
                    index > 0 and index < (decltype(index)) mod.types.size(),
                    "Attempt to curtail iterator invalidation went severely wrong"
                );
                // Just to create it.
                (void) a->struct_type(mod);
                type_ptr = mod.types.data() + index;
            } else {
                (void) a->struct_type(mod);
            }
            LCC_ASSERT(Analyse((Type**) &a->struct_type()));
        } break;

        // Apply decltype decay to the element type, prohibit arrays of
        // references, and, if there is an initial size expression, analyse that.
        // Also set cached struct type for IRGen by calling struct_type().
        case Type::Kind::DynamicArray: {
            auto* a = as<DynamicArrayType>(type);
            LCC_ASSERT(
                a->element_type(),
                "DynamicArray has NULL element type"
            );
            (void) Analyse(&a->element_type());
            a->element_type(DeclTypeDecay(a->element_type()));

            auto* elem = a->element_type();
            if (is<ReferenceType>(elem)) {
                if (elem->ok()) Error(
                    a->location(),
                    "Cannot create dynamic array of reference type {}",
                    elem
                );
                else Error(
                    a->location(),
                    "Cannot create dynamic array of reference type"
                );
                a->set_sema_errored();
            }

            if (elem->ok() and not a->sema_errored())
                a->set_sema_done();
            else a->set_sema_errored();

            // Cache struct type for IRGen.
            // FIXME: Terrible, no-good check for iterator invalidation, and quickfix.
            if (type_ptr >= mod.types.data() and type_ptr < mod.types.data() + mod.types.size()) {
                auto index = type_ptr - mod.types.data();
                LCC_ASSERT(
                    index > 0 and index < (decltype(index)) mod.types.size(),
                    "Attempt to curtail iterator invalidation went severely wrong"
                );
                // Just to create it.
                (void) a->struct_type(mod);
                type_ptr = mod.types.data() + index;
            } else {
                (void) a->struct_type(mod);
            }
            LCC_ASSERT(Analyse((Type**) &a->struct_type()));

            if (a->initial_size())
                (void) Analyse(&a->initial_size());
        } break;

        // Apply decltype decay to the element type, prohibit arrays of
        // references, and, if there is an initial size expression, analyse that.
        // Also set cached struct type for IRGen by calling struct_type().
        case Type::Kind::Sum: {
            auto* s = as<SumType>(type);
            if (s->members().empty()) {
                Error(
                    s->location(),
                    "Sum type empty!\n"
                    "A sum type must have more than one member (otherwise, use a struct, or something)"
                );
                return false;
            }
            if (s->members().size() == 1) {
                Error(
                    s->location(),
                    "Sum type has a single member.\n"
                    "A sum type must have more than one member (otherwise, use a struct, or something)"
                );
                return false;
            }

            // Finalise members
            for (auto& member : s->members()) {
                // Analyse member type
                (void) Analyse(&member.type);
                member.type = DeclTypeDecay(member.type);
                if (member.type->sema_errored()) {
                    type->set_sema_errored();
                    continue;
                }

                auto msize = member.type->size(context) / 8;
                auto malign = member.type->align(context) / 8;
                // fmt::print("Sum member {}: size:{} align:{}\n", *member.type, msize, malign);
                s->byte_size(std::max(s->byte_size(), msize));
                s->alignment(std::max(s->alignment(), malign));
            }

            // Ensure contiguous sum types in memory are aligned properly (adjust size
            // to alignment).
            s->byte_size(utils::AlignTo(s->byte_size(), s->alignment()));

            // Cache struct type for IRGen.
            // FIXME: Terrible, no-good check for iterator invalidation, and quickfix.
            if (type_ptr >= mod.types.data() and type_ptr < mod.types.data() + mod.types.size()) {
                auto index = type_ptr - mod.types.data();
                LCC_ASSERT(
                    index > 0 and index < (decltype(index)) mod.types.size(),
                    "Attempt to curtail iterator invalidation went severely wrong"
                );
                // Just to create it.
                (void) s->struct_type(mod);
                type_ptr = mod.types.data() + index;
            } else {
                (void) s->struct_type(mod);
            }
            LCC_ASSERT(Analyse((Type**) &s->struct_type()));

            // IMPORTANT: Struct type alignment calculation is probably wrong, since
            // the actual types were punned in the underlying type... So, we must
            // update it ourselves, here. Align underlying struct type to sum type
            // alignment, and recalculate struct size based on new alignment.
            s->struct_type()->alignment(s->alignment());
            s->struct_type()->byte_size(
                utils::AlignTo(
                    s->struct_type()->byte_size(),
                    s->struct_type()->alignment()
                )
            );

            // We set it as done so that we may fetch size and alignment in below
            // assertions.
            s->set_sema_done();
            // These are here so that (sizeof <sum-type>) accurately represents the
            // final size of the sum type.
            LCC_ASSERT(
                s->size(context) == s->struct_type()->size(context),
                "Size of SumType {} does not match size of underlying struct {}"
                " (meaning using `sizeof` will break everything)",
                s->size(context),
                s->struct_type()->size(context)
            );
            LCC_ASSERT(
                s->align(context) == s->struct_type()->align(context),
                "Align of SumType {} does not match align of underlying struct {}",
                s->align(context),
                s->struct_type()->align(context)
            );
        } break;

        case Type::Kind::Union: {
            auto* u = as<UnionType>(type);
            usz byte_size = 0;
            usz alignment = 1;

            // Finalise members
            for (auto& member : u->members()) {
                // Analyse member type
                (void) Analyse(&member.type);
                member.type = DeclTypeDecay(member.type);
                if (member.type->sema_errored()) {
                    type->set_sema_errored();
                    continue;
                }

                auto msize = member.type->size(context) / 8;
                auto malign = member.type->align(context) / 8;
                byte_size = std::max(byte_size, msize);
                alignment = std::max(alignment, malign);
            }

            u->byte_size(byte_size);
            u->alignment(alignment);

            // NOTE: This is compiler-specific, not to do with semantic analysis of
            // the language. Basically, the underlying array type that represents the
            // generic data CANNOT have a size of zero. So, even though a union may
            // technically have no members, the underlying data array cannot have a
            // size of zero.
            u->byte_size(std::max(u->byte_size(), (usz) 1));

            // Ensure contiguous union types in memory are aligned properly (adjust
            // size to alignment).
            u->byte_size(utils::AlignTo(u->byte_size(), u->alignment()));

            // Cache struct type for IRGen
            // FIXME: Terrible, no-good check for iterator invalidation, and quickfix.
            if (type_ptr >= mod.types.data() and type_ptr < mod.types.data() + mod.types.size()) {
                auto index = type_ptr - mod.types.data();
                LCC_ASSERT(
                    index > 0 and index < (decltype(index)) mod.types.size(),
                    "Attempt to curtail iterator invalidation went severely wrong"
                );
                // Just to create it.
                (void) u->array_type(mod);
                type_ptr = mod.types.data() + index;
            } else {
                (void) u->array_type(mod);
            }
            LCC_ASSERT(
                Analyse((Type**) &u->array_type()),
                "UnionType underlying array type failed analysis"
            );
        } break;

        /// Analyse the parameters, the return type, and attributes.
        case Type::Kind::Function: {
            auto* ty = as<FuncType>(type);
            LCC_ASSERT(ty->return_type(), "Function type has NULL return type");
            (void) Analyse(&ty->return_type());

            for (auto& param : ty->params()) {
                LCC_ASSERT(param.type, "Function type has parameter with NULL type");
                param.type = DeclTypeDecay(param.type);
                (void) Analyse(&param.type);
            }

            /// If the function returns void, it must not be marked discardable.
            if (ty->return_type()->ok() and ty->return_type()->is_void()) {
                if (ty->has_attr(FuncAttr::Discardable))
                    Error(type->location(), "Function returning void cannot be 'discardable'");
            }

            /// Noreturn functions always have side effects.
            if (ty->has_attr(FuncAttr::NoReturn)) {
                if (ty->has_attr(FuncAttr::Const)) Error(
                    type->location(),
                    "'noreturn' function cannot be 'const'"
                );

                if (ty->has_attr(FuncAttr::Pure)) Error(
                    type->location(),
                    "'noreturn' function cannot be 'pure'"
                );
            }

            /// Check for conflicting inline/noinline attributes.
            if (ty->has_attr(FuncAttr::Inline) and ty->has_attr(FuncAttr::NoInline))
                Error(type->location(), "Function cannot be both 'inline' and 'noinline'");
        } break;

        /// Bit width may not be 0.
        case Type::Kind::Integer: {
            if (as<IntegerType>(type)->bit_width() == 0) {
                Error(type->location(), "Bit width of integer type cannot be 0");
                type->set_sema_errored();
            }
        } break;

        case Type::Kind::TemplatedStruct: {
            auto* t = as<TemplatedStructType>(type);

            for (auto& m : t->members()) {
                if (is<NamedType>(m.type)) {
                    auto n = as<NamedType>(m.type)->name();
                    if (t->param_by_name(n)) {
                        // We don't have to analyse types that will be replaced in an invocation.
                        // i.e. anything that appears in the parameter list.
                        // TODO: We may want to ensure the parameter is of 'type' type or
                        // something of that nature.
                        continue;
                    }
                }
                if (not Analyse(&m.type)) {
                    type->set_sema_errored();
                    return false;
                }
            }
        } break;

        /// Calculate size, alignment, and member offsets.
        case Type::Kind::Struct: {
            /// TODO: Packed structs should probably be a separate type altogether and
            /// for those, we’ll have to perform all these calculations below in bits
            /// instead. Cereals!
            auto* s = as<StructType>(type);
            usz byte_size = 0;
            usz alignment = 1;

            std::vector<Type*> supplanted_types{};

            /// Finalise all members.
            for (auto& member : s->members()) {
                /// Analyse the member type.
                (void) Analyse(&member.type);
                member.type = DeclTypeDecay(member.type);
                if (member.type->sema_errored()) {
                    type->set_sema_errored();
                    continue;
                }

                // TODO: Should we allow type members?
                // Type members don't actually store any information in the struct at runtime...
                if (is<TypeType>(member.type))
                    continue;

                if (member.supplanted) {
                    // Check if this type has already been supplanted; if so, error.
                    if (rgs::any_of(supplanted_types, [&](Type* already_supplanted_type) {
                            return Type::Equal(already_supplanted_type, member.type);
                        })) {
                        auto e = Error(
                            member.location,
                            "Supplant must only be used once per type within definition of a single type. Multiple supplant of {} within {}.",
                            *member.type,
                            *type
                        );
                        e.attach(Note(type->location(), "Defined here"));
                        type->set_sema_errored();
                        return false;
                    }
                    // Record supplanted type for future duplicate check.
                    supplanted_types.emplace_back(member.type);
                }

                /// Align the member to its alignment.
                auto msize = member.type->size_in_bytes(context);
                auto malign = member.type->align(context) / 8;
                // fmt::print("Member {}: size:{} align:{}\n", *member.type, msize, malign);
                member.byte_offset = utils::AlignTo(byte_size, malign);
                byte_size = member.byte_offset + msize;
                alignment = std::max(alignment, malign);
            }

            /// Align the struct to its alignment.
            s->alignment(alignment);

            /// Empty structs have a size of 0.
            /// Ensure contiguous struct types are aligned properly (adjust size to
            /// alignment). Only applies to non-zero size structs.
            if (byte_size)
                s->byte_size(utils::AlignTo(byte_size, s->alignment()));
            else s->byte_size(0);

            // {
            //     s->set_sema_done();
            //     fmt::print("struct {}: size:{} align:{}\n", *type, s->size_in_bytes(context), s->align(context) / 8);
            // }
        } break;

        /// Calculate enumerator values.
        case Type::Kind::Enum: {
            auto* e = as<EnumType>(type);
            LCC_ASSERT(
                e->underlying_type(),
                "Enum type has NULL underlying type"
            );

            if (not Analyse(&e->underlying_type())) {
                e->set_sema_errored();
                return false;
            }

            if (not e->underlying_type()->is_integer(true)) {
                Error(
                    e->location(),
                    "Disallowed underlying type of enum (sorry!).\n"
                    "Only integer or integer-like types are allowed, currently. We hope to change this in the future."
                );
                e->set_sema_errored();
                return false;
            }

            { // Error on duplicate enumerators.
                std::unordered_set<std::string> names;
                for (auto& val : e->enumerators()) {
                    if (not names.insert(val->name()).second) {
                        Error(val->location(), "Duplicate enumerator '{}'", val->name());
                        e->set_sema_errored();
                        return false;
                    }
                }
            }

            // Assign enumerator values to all enumerators.
            isz next_val = -1; //< For enums with integer underlying type.
            for (auto& val : e->enumerators()) {
                val->type(e);

                // For enums with integer underlying type, set the value if there is none.
                // Easy!
                if (not val->init()) {
                    if (e->underlying_type()->is_integer(true)) {
                        val->init() = new (mod) ConstantExpr(e, ++next_val, val->location());
                        val->set_sema_done();
                        continue;
                    }
                    Error(
                        val->location(),
                        "Unhandled underlying type given no init expression provided.\n"
                        "Compiler is too dumb to make a {}\n",
                        e->underlying_type()
                    );
                    val->set_sema_errored();
                    return false;
                }

                // User provided a value.
                // Harder.

                // Make sure the expression is well-formed, and has a type.
                if (not Analyse(&val->init())) {
                    Error(
                        val->init()->location(),
                        "Invalid init expression for {} within enumerator declaration",
                        val->name()
                    );
                    val->set_sema_errored();
                    return false;
                }

                // Convert the expression to the underlying type of the enum.
                if (not Convert(&val->init(), e->underlying_type())) {
                    // If the enum is associated with a declaration, print that name in the
                    // error message (name association is important for the developer!).
                    if (e->decl()) {
                        Error(
                            val->init()->location(),
                            "Init expression for {} within enumerator declaration {}",
                            val->name(),
                            e->decl()->name()
                        );
                        Note(
                            e->decl()->location(),
                            "Declared here"
                        );
                    } else {
                        Error(
                            val->init()->location(),
                            "Init expression for {} within enumerator definition",
                            val->name()
                        );
                        Note(
                            e->location(),
                            "Defined here"
                        );
                    }

                    val->set_sema_errored();
                    return false;
                }

                // Evaluate the expression at compile-time. If we can't, it's a fatal
                // error---enums are named constants.
                EvalResult res{0};
                if (not val->init()->evaluate(context, res, false)) {
                    Error(
                        val->init()->location(),
                        "Init expression for {} within enumerator is not a constant expression\n"
                        "This means the compiler is unable to calculate the value at compile-time.\n"
                        "Try using an integer constant like `69', if stuck.\n",
                        val->name()
                    );
                    val->set_sema_errored();
                    return false;
                }

                // Replace init expression with the constant expression that represents it
                // (with cached value).
                val->init() = new (mod) ConstantExpr(val->init(), res);
                val->set_sema_done();

                // For enums with integer underlying type, set the next value the compiler
                // will assign automatically if no init expression is provided.
                if (e->underlying_type()->is_integer(true))
                    next_val = decltype(next_val)(res.as_int().value()) + 1;

                // Declare the enumerator member in the enum's scope.
                {
                    auto d = e->scope()->declare(context, std::string(val->name()), val);
                    LCC_ASSERT(d, "Failed to declare enumerator member");
                }
            }
        } break;

        case Type::Kind::Typeof: {
            auto* t = as<TypeofType>(type);
            if (not Analyse(&t->expression())) {
                t->set_sema_errored();
                return false;
            }
            if (is<TypeExpr>(t->expression()))
                *type_ptr = as<TypeExpr>(t->expression())->contained_type();
            else
                *type_ptr = t->expression()->type();
        } break;
    }

    /// Do *not* use `type` here, as it may have been replaced by something else.
    if (not (*type_ptr)->sema_done_or_errored())
        (*type_ptr)->set_sema_done();
    return (*type_ptr)->ok();
}
