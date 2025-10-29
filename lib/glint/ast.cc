#include <lcc/target.hh>
#include <lcc/utils.hh>
#include <lcc/utils/ast_printer.hh>
#include <lcc/utils/macros.hh>
#include <lcc/utils/rtti.hh>

#include <glint/ast.hh>
#include <glint/eval.hh>
#include <glint/module_description.hh>
#include <glint/parser.hh>

#include <bit>
#include <ranges>
#include <string>
#include <string_view>
#include <type_traits>
#include <unordered_set>
#include <utility>
#include <vector>

/// ===========================================================================
///  AST
/// ===========================================================================
lcc::glint::StringLiteral::StringLiteral(
    Module& mod,
    std::string_view value,
    Location location
) : TypedExpr{
        // clang-format off
        Kind::StringLiteral,
        location,
        new(mod) ReferenceType(
            new(mod) ArrayType(
                BuiltinType::Byte(mod),
                new(mod) IntegerLiteral(value.size() + 1, location),
                location
            ),
            location
        ),
    },
    _index{mod.intern(value)} {} // clang-format on

/// Declare a symbol in this scope.
auto lcc::glint::Scope::declare(
    Context* ctx,
    std::string&& name,
    Decl* decl
) -> Result<Decl*> {
    LCC_ASSERT(
        name == decl->name(),
        "The name given to declare() is different to the name from the declaration given to declare()!"
    );

    // If the symbol already exists, then this is an error, (unless that symbol
    // resolves to one or more function declarations, and we are declaring a
    // function).
    auto found = find_recursive(name);
    if (not found.empty()) {
        bool found_all_functions{true};
        for (auto found_decl : found) {
            if (not is<FuncDecl>(found_decl)) {
                found_all_functions = false;
                break;
            }
        }

        if (not found_all_functions or not is<FuncDecl>(decl))
            return Diag::Error(ctx, decl->location(), "Redeclaration of '{}'", name);
    }

    // Otherwise, add the symbol.
    symbols.emplace(std::move(name), decl);
    return decl;
}

auto lcc::glint::Expr::type() const -> Type* {
    if (auto e = cast<TypedExpr>(this)) return e->type();
    return Type::Void;
}

auto lcc::glint::Type::align(const lcc::Context* ctx) const -> usz {
    LCC_ASSERT(sema_done_or_errored());
    if (sema_errored()) return 1;
    switch (kind()) {
        case Kind::Builtin:
            switch (as<BuiltinType>(this)->builtin_kind()) {
                using K = BuiltinType::BuiltinKind;
                case K::Bool: return ctx->target()->glint.align_of_bool;
                case K::Byte: return ctx->target()->glint.align_of_byte;
                case K::UInt:
                case K::Int: return ctx->target()->glint.align_of_int;

                /// Alignment must not be 0, so return 1.
                case K::Unknown:
                case K::Void:
                case K::OverloadSet:
                    return 1;
            }
            LCC_UNREACHABLE();

        case Kind::FFIType:
            switch (as<FFIType>(this)->ffi_kind()) {
                using K = FFIType::FFIKind;
                case K::CChar:
                case K::CSChar:
                case K::CUChar:
                    return ctx->target()->ffi.align_of_char;

                case K::CShort:
                case K::CUShort:
                    return ctx->target()->ffi.align_of_short;

                case K::CInt:
                case K::CUInt:
                    return ctx->target()->ffi.align_of_int;

                case K::CLong:
                case K::CULong:
                    return ctx->target()->ffi.align_of_long;

                case K::CLongLong:
                case K::CULongLong:
                    return ctx->target()->ffi.align_of_long_long;
            }
            LCC_UNREACHABLE();

        /// const_cast is ok because we’re just reading the underlying type.
        case Kind::Enum:
            return const_cast<EnumType*>(as<EnumType>(this))->underlying_type()->align(ctx);

        /// Unresolved named type.
        case Kind::Named: return 1;

        /// Functions have no alignment.
        case Kind::Function: return 1;

        case Kind::Pointer:
        case Kind::Reference:
            return ctx->target()->align_of_pointer;

        // DynamicArray and ArrayView both convert to structs, so we just use the
        // struct's size.
        case Kind::DynamicArray:
            return as<DynamicArrayType>(this)->size(ctx);
        case Kind::ArrayView:
            return as<ArrayViewType>(this)->size(ctx);

        case Kind::Array: return elem()->align(ctx);
        case Kind::Struct: return as<StructType>(this)->alignment() * 8;
        case Kind::Union: return as<UnionType>(this)->alignment() * 8;
        case Kind::Sum: return as<SumType>(this)->alignment() * 8;
        case Kind::Integer: return std::bit_ceil(as<IntegerType>(this)->bit_width());

        case Kind::Typeof:
            LCC_ASSERT(false, "Cannot get align of TypeofType; sema should replace TypeofType with the type of it's contained expression.");
    }

    LCC_UNREACHABLE();
}

auto lcc::glint::Type::elem() const -> Type* {
    switch (kind()) {
        case Kind::Pointer: return as<PointerType>(this)->element_type();
        case Kind::Reference: return as<ReferenceType>(this)->element_type();
        case Kind::Array: return as<ArrayType>(this)->element_type();
        case Kind::DynamicArray:
            return as<DynamicArrayType>(this)->element_type();

        case Kind::ArrayView:
            return as<ArrayViewType>(this)->element_type();

        case Kind::Enum:
            return as<EnumType>(this)->underlying_type();

        case Kind::Builtin:
        case Kind::FFIType:
        case Kind::Named:
        case Kind::Function:
        case Kind::Struct:
        case Kind::Union:
        case Kind::Sum:
        case Kind::Integer:
        case Kind::Typeof:
            Diag::ICE("Type has no element type");
    }
    LCC_UNREACHABLE();
}

namespace {
bool is_builtin(const lcc::glint::Type* t, lcc::glint::BuiltinType::BuiltinKind k) {
    if (auto b = lcc::cast<lcc::glint::BuiltinType>(t)) return b->builtin_kind() == k;
    return false;
}
} // namespace

bool lcc::glint::Type::is_bool() const { return ::is_builtin(this, BuiltinType::BuiltinKind::Bool); }
bool lcc::glint::Type::is_byte() const { return ::is_builtin(this, BuiltinType::BuiltinKind::Byte); }

bool lcc::glint::Type::is_integer(bool include_bool) const {
    return is<IntegerType, FFIType>(this)
        or ::is_builtin(this, BuiltinType::BuiltinKind::UInt)
        or ::is_builtin(this, BuiltinType::BuiltinKind::Int)
        or is_byte()
        or (include_bool and is_bool());
}

bool lcc::glint::Type::is_signed_int(const Context* ctx) const {
    if (auto i = lcc::cast<IntegerType>(this)) return i->is_signed();
    if (auto f = lcc::cast<FFIType>(this)) {
        switch (f->ffi_kind()) {
            using K = FFIType::FFIKind;
            case K::CSChar:
            case K::CShort:
            case K::CInt:
            case K::CLong:
            case K::CLongLong:
                return true;

            case K::CUChar:
            case K::CUShort:
            case K::CUInt:
            case K::CULong:
            case K::CULongLong:
                return false;

            case K::CChar:
                return ctx->target()->ffi.char_is_signed;
        }
    }
    return ::is_builtin(this, BuiltinType::BuiltinKind::Int);
}

bool lcc::glint::Type::is_unknown() const { return ::is_builtin(this, BuiltinType::BuiltinKind::Unknown); }

bool lcc::glint::Type::is_unsigned_int(const Context* ctx) const {
    if (auto i = lcc::cast<IntegerType>(this)) return not i->is_signed();
    if (auto f = lcc::cast<FFIType>(this)) {
        switch (f->ffi_kind()) {
            using K = FFIType::FFIKind;
            case K::CSChar:
            case K::CShort:
            case K::CInt:
            case K::CLong:
            case K::CLongLong:
                return false;

            case K::CUChar:
            case K::CUShort:
            case K::CUInt:
            case K::CULong:
            case K::CULongLong:
                return true;

            case K::CChar:
                return not ctx->target()->ffi.char_is_signed;
        }
    }
    return is_byte();
}

auto lcc::glint::Type::is_void() const -> bool { return ::is_builtin(this, BuiltinType::BuiltinKind::Void); }
/// Check if this is the builtin overload set type.
auto lcc::glint::Type::is_overload_set() const -> bool { return ::is_builtin(this, BuiltinType::BuiltinKind::OverloadSet); };

auto lcc::glint::Type::size(const lcc::Context* ctx) const -> usz {
    LCC_ASSERT(
        sema_done_or_errored(),
        "Type {} has not been analysed",
        this->string(false)
    );
    if (sema_errored()) return 0;
    switch (kind()) {
        case Kind::Builtin:
            switch (as<BuiltinType>(this)->builtin_kind()) {
                using K = BuiltinType::BuiltinKind;
                case K::Bool: return ctx->target()->glint.size_of_bool;
                case K::Byte: return ctx->target()->glint.size_of_byte;
                case K::UInt:
                case K::Int: return ctx->target()->glint.size_of_int;

                case K::Unknown:
                case K::Void:
                case K::OverloadSet:
                    return 0;
            }
            LCC_UNREACHABLE();

        case Kind::FFIType:
            switch (as<FFIType>(this)->ffi_kind()) {
                using K = FFIType::FFIKind;
                case K::CChar:
                case K::CSChar:
                case K::CUChar:
                    return ctx->target()->ffi.size_of_char;

                case K::CShort:
                case K::CUShort:
                    return ctx->target()->ffi.size_of_short;

                case K::CInt:
                case K::CUInt:
                    return ctx->target()->ffi.size_of_int;

                case K::CLong:
                case K::CULong:
                    return ctx->target()->ffi.size_of_long;

                case K::CLongLong:
                case K::CULongLong:
                    return ctx->target()->ffi.size_of_long_long;
            }
            LCC_UNREACHABLE();

        /// const_cast is ok because we’re just reading the underlying type.
        case Kind::Enum:
            return const_cast<EnumType*>(as<EnumType>(this))->underlying_type()->size(ctx);

        case Kind::Named: return 0;
        case Kind::Function: return 0;

        case Kind::Pointer:
        case Kind::Reference:
            return ctx->target()->size_of_pointer;

        case Kind::DynamicArray:
            return Type::VoidPtr->size(ctx) + 2 * DynamicArrayType::IntegerWidth;

        case Kind::ArrayView:
            return Type::VoidPtr->size(ctx) + ArrayViewType::IntegerWidth;

        case Kind::Sum:
            return SumType::IntegerWidth + as<SumType>(this)->byte_size();

        case Kind::Array:
            return as<ArrayType>(this)->dimension() * elem()->size(ctx);

        case Kind::Struct: return as<StructType>(this)->byte_size() * 8;
        case Kind::Union: return as<UnionType>(this)->byte_size() * 8;
        case Kind::Integer: return as<IntegerType>(this)->bit_width();

        case Kind::Typeof:
            LCC_ASSERT(false, "Cannot get size of TypeofType; sema should replace TypeofType with the type of it's contained expression.");
    }

    LCC_UNREACHABLE();
}

auto lcc::glint::Type::strip_pointers_and_references() -> Type* {
    auto ty = strip_references();
    while (is<PointerType>(ty)) ty = ty->elem();
    return ty;
}

auto lcc::glint::Type::strip_references() -> Type* {
    auto ty = this;
    if (is<ReferenceType>(ty)) ty = ty->elem();
    LCC_ASSERT(not is<ReferenceType>(ty), "Double references are not permitted");
    return ty;
}

auto lcc::glint::Type::Equal(const Type* a, const Type* b) -> bool {
    if (a == b) return true;
    if (a->kind() != b->kind()) return false;

    switch (a->kind()) {
        case Kind::Builtin: {
            auto ba = as<BuiltinType>(a);
            auto bb = as<BuiltinType>(b);
            return ba->builtin_kind() == bb->builtin_kind();
        }

        case Kind::FFIType: {
            auto fa = as<FFIType>(a);
            auto fb = as<FFIType>(b);
            return fa->ffi_kind() == fb->ffi_kind();
        }

        /// These are never equal unless they’re the exact same instance.
        case Kind::Named:
        case Kind::Enum:
            return a == b;

        case Kind::Pointer:
        case Kind::Reference:
            return Type::Equal(a->elem(), b->elem());

        case Kind::Array: {
            auto aa = as<ArrayType>(a);
            auto ab = as<ArrayType>(b);
            return aa->dimension() == ab->dimension() and Type::Equal(a->elem(), b->elem());
        }

        case Kind::DynamicArray:
            return Type::Equal(a->elem(), b->elem());

        case Kind::ArrayView:
            return Type::Equal(a->elem(), b->elem());

        case Kind::Function: {
            auto fa = as<FuncType>(a);
            auto fb = as<FuncType>(b);

            /// Compare parameters.
            if (fa->params().size() != fb->params().size()) return false;
            for (usz i = 0; i < fa->params().size(); ++i)
                if (not Type::Equal(fa->params()[i].type, fb->params()[i].type))
                    return false;

            /// Compare return type.
            return Type::Equal(fa->return_type(), fb->return_type());
        }

        /// Anonymous structs are equal if their fields have the same
        /// types. Named structs are never equal.
        case Kind::Struct: {
            auto sa = as<StructType>(a);
            auto sb = as<StructType>(b);

            if (sa->decl() or sb->decl()) return false;

            /// Compare fields.
            if (sa->members().size() != sb->members().size()) return false;
            for (usz i = 0; i < sa->members().size(); ++i)
                if (not Type::Equal(sa->members()[i].type, sb->members()[i].type))
                    return false;
            return true;
        }

        // For now, no sum types are equal.
        case Kind::Sum:
            LCC_TODO("If sum types flatten to the same types, then they are equal");

        // For now, no unions are equal.
        case Kind::Union:
            return false;

        case Kind::Integer: {
            auto ia = as<IntegerType>(a);
            auto ib = as<IntegerType>(b);
            return ia->bit_width() == ib->bit_width() and ia->is_signed() == ib->is_signed();
        }

        case Kind::Typeof:
            LCC_ASSERT(false, "Cannot compare equality of TypeofType; sema should replace TypeofType with the type of it's contained expression.");
    }

    LCC_UNREACHABLE();
}

auto lcc::glint::ArrayType::dimension() const -> usz {
    LCC_ASSERT(ok(), "Can only call dimension() if type has been type checked successfully");
    return usz(as<ConstantExpr>(size())->value().as_int());
}

// NOTE: DO NOT CALL FOR (T v) COMPOUND LITERALS!
auto lcc::glint::CallExpr::callee_type() const -> FuncType* {
    auto ty = callee()->type();
    while (is<PointerType, ReferenceType>(ty)) ty = ty->elem();
    LCC_ASSERT(ty->is_function());
    return as<FuncType>(ty);
}

auto lcc::glint::Expr::CloneImpl(Module& mod, Context* context, Expr* expr, std::unordered_map<Scope*, Scope*>& scope_fixups) -> Expr* {
    LCC_ASSERT(context);

    // Don't pass me nullptr, I won't return it.
    if (not expr) return nullptr;

    const auto Clone = [&](Expr* e) {
        return Expr::CloneImpl(mod, context, e, scope_fixups);
    };

    const auto CloneAll = [&](std::vector<Expr*> exprs) -> std::vector<Expr*> {
        std::vector<Expr*> out{};
        out.reserve(exprs.size());
        for (auto e : exprs)
            out.emplace_back(Clone(e));

        return out;
    };

    // If we have not yet encountered this scope, create a new scope and add a
    // mapping to our scope fixups.
    const auto fixup_scope
        = [](this auto&& self, Module& m, decltype(scope_fixups) scope_fixes, Scope* scope) -> Scope* {
        if (not scope) return nullptr;
        if (not scope_fixes.contains(scope)) {
            auto cloned_scope = new (m) Scope(scope->parent());
            scope_fixes.emplace(scope, cloned_scope);
        }
        return scope_fixes.at(scope);
    };

    switch (expr->kind()) {
        case Kind::While: {
            auto w = as<WhileExpr>(expr);
            return new (mod) WhileExpr(
                Clone(w->condition()),
                Clone(w->body()),
                w->location()
            );
        }
        case Kind::For: {
            auto f = as<ForExpr>(expr);
            // Ensure we clone init before the rest, so that declarations get fixed up
            // properly...
            auto clone_init = Clone(f->init());
            return new (mod) ForExpr(
                clone_init,
                Clone(f->condition()),
                Clone(f->increment()),
                Clone(f->body()),
                f->location()
            );
        }
        case Kind::If: {
            auto i = as<IfExpr>(expr);
            return new (mod) IfExpr(
                Clone(i->condition()),
                Clone(i->then()),
                Clone(i->otherwise()),
                i->location()
            );
        }
        case Kind::Return: {
            auto r = as<ReturnExpr>(expr);
            return new (mod) ReturnExpr(
                Clone(r->value()),
                r->location()
            );
        }
        case Kind::Unary: {
            auto u = as<UnaryExpr>(expr);
            return new (mod) UnaryExpr(
                u->op(),
                Clone(u->operand()),
                u->is_postfix(),
                u->location()
            );
        }
        case Kind::Binary: {
            auto b = as<BinaryExpr>(expr);
            return new (mod) BinaryExpr(
                b->op(),
                Clone(b->lhs()),
                Clone(b->rhs()),
                b->location()
            );
        }
        case Kind::IntegerLiteral: {
            auto i = as<IntegerLiteral>(expr);
            return new (mod) IntegerLiteral(i->value(), i->location());
        }

        case Kind::StringLiteral: {
            auto s = as<StringLiteral>(expr);
            return new (mod) StringLiteral(
                mod,
                mod.strings.at(s->string_index()),
                s->location()
            );
        }

        case Kind::NameRef: {
            auto n = as<NameRefExpr>(expr);
            Scope* s = n->scope();
            // Only fixup scope if we need to. NOTE: This might cause errors if a
            // namerefexpr appears before a declaration...
            if (scope_fixups.contains(s))
                s = fixup_scope(mod, scope_fixups, n->scope());

            return new (mod) NameRefExpr(
                n->name(),
                s,
                n->location()
            );
        }

        case Kind::Sizeof: {
            auto s = as<SizeofExpr>(expr);
            return new (mod) SizeofExpr(Clone(s->expr()), s->location());
        }
        case Kind::Alignof: {
            auto a = as<AlignofExpr>(expr);
            return new (mod) AlignofExpr(Clone(a->expr()), a->location());
        }

        case Kind::Block: {
            auto b = as<BlockExpr>(expr);
            return new (mod) BlockExpr(CloneAll(b->children()), b->location());
        }

        case Kind::Call: {
            auto c = as<CallExpr>(expr);
            return new (mod) CallExpr(
                Clone(c->callee()),
                CloneAll(c->args()),
                c->location()
            );
        }

        case Kind::CompoundLiteral: {
            auto c = as<CompoundLiteral>(expr);
            std::vector<CompoundLiteral::Member> members{};
            members.reserve(c->values().size());
            for (auto m : c->values())
                members.emplace_back(m.name, Clone(m.value));

            return new (mod) CompoundLiteral(members, c->location());
        }

        case Kind::Cast: {
            auto c = as<CastExpr>(expr);
            return new (mod) CastExpr(
                Clone(c->operand()),
                c->type(),
                c->cast_kind(),
                c->location()
            );
        }

        case Kind::MemberAccess: {
            auto m = as<MemberAccessExpr>(expr);
            return new (mod) MemberAccessExpr(
                Clone(m->object()),
                m->name(),
                m->location()
            );
        }

        case Kind::TypeDecl: {
            auto t = as<TypeDecl>(expr);
            auto t_declared = as<DeclaredType>(t->type());
            auto cloned_decl = new (mod) TypeDecl(
                t->module(),
                t->name(),
                t_declared,
                t->location()
            );

            LCC_ASSERT(t_declared->scope());
            fixup_scope(mod, scope_fixups, t_declared->scope());

            return cloned_decl;
        }

        case Kind::TypeAliasDecl: {
            auto a = as<TypeAliasDecl>(expr);
            return new (mod) TypeAliasDecl(
                a->name(),
                a->type(),
                a->location()
            );
        };

        case Kind::EnumeratorDecl: {
            auto e = as<EnumeratorDecl>(expr);
            return new (mod) EnumeratorDecl(
                e->name(),
                Clone(e->init()),
                e->location()
            );
        }

        case Kind::VarDecl: {
            auto v = as<VarDecl>(expr);

            Scope* scope{nullptr};
            for (auto s : mod.scopes) {
                for (auto sym : s->all_symbols()) {
                    // Found encountered decl in existing scope
                    if (sym == v) {
                        scope = s;
                        break;
                    }
                }
                if (scope)
                    break;
            }
            LCC_ASSERT(
                scope,
                "Encountered declaration during cloning that cannot be found in any scope in the given module."
                " If you just need to clone something and could care less about scopes,"
                " create a `new (mod) Scope(nullptr)` and declare the declarations in your clonee AST in that."
            );

            // Declare declaration in fixed up scope.
            auto fixed_scope = fixup_scope(mod, scope_fixups, scope);
            LCC_ASSERT(fixed_scope);

            auto clone = new (mod) VarDecl(
                v->name(),
                v->type(),
                Clone(v->init()),
                v->module(),
                v->linkage(),
                v->location()
            );
            auto cloned_decl
                = fixed_scope->declare(context, std::string{v->name()}, clone);
            LCC_ASSERT(cloned_decl);

            return clone;
        }

        case Kind::FuncDecl: {
            auto f = as<FuncDecl>(expr);
            return new (mod) FuncDecl(
                f->name(),
                f->function_type(),
                Clone(f->body()),
                f->scope(),
                f->module(),
                f->linkage(),
                f->location()
            );
        }

        case Kind::IntrinsicCall: {
            auto i = as<IntrinsicCallExpr>(expr);
            return new (mod) IntrinsicCallExpr(
                i->intrinsic_kind(),
                CloneAll(i->args())
            );
        }

        case Kind::Type:
            // TODO: Handle structs with initialized members...

            if (auto t_fixedarray = cast<ArrayType>(expr->type()); t_fixedarray) {
                return new (mod) TypeExpr(
                    new (mod) ArrayType(
                        t_fixedarray->elem(),
                        Clone(t_fixedarray->size()),
                        {}
                    ),
                    {}
                );
            }
            if (auto t_dynarray = cast<ArrayType>(expr->type()); t_dynarray) {
                return new (mod) TypeExpr(
                    new (mod) DynamicArrayType(
                        t_dynarray->elem(),
                        Clone(t_dynarray->size()),
                        {}
                    ),
                    {}
                );
            }
            if (auto t_typeof = cast<TypeofType>(expr->type()); t_typeof) {
                return new (mod) TypeExpr(
                    new (mod) TypeofType(Clone(t_typeof->expression()), {}),
                    {}
                );
            }
            LCC_ASSERT(
                expr->children().size() == 0,
                "If a type has expressions within it, they need to be cloned properly"
            );
            return new (mod) TypeExpr(expr->type(), expr->location());

        case Kind::Module: {
            auto m = as<ModuleExpr>(expr);
            return new (mod) ModuleExpr(m->mod(), m->location());
        }

        case Kind::Match: {
            auto m = as<MatchExpr>(expr);
            auto m_new = new (mod) MatchExpr(Clone(m->object()), m->location());
            m_new->names() = m->names();
            m_new->bodies() = CloneAll(m->bodies());
            return m_new;
        }

        case Kind::EvaluatedConstant: {
            auto c = as<ConstantExpr>(expr);
            return new (mod) ConstantExpr(c->expr(), c->value());
        }

        case Kind::Template: {
            auto t = as<TemplateExpr>(expr);
            return new (mod) TemplateExpr(
                Clone(t->body()),
                t->params(),
                t->location()
            );
        }

        case Kind::OverloadSet: {
            auto o = as<OverloadSet>(expr);
            std::vector<FuncDecl*> overloads{};
            overloads.reserve(o->overloads().size());
            for (auto f : o->overloads())
                overloads.emplace_back(
                    as<FuncDecl>(Clone(f))
                );

            return new (mod) OverloadSet(overloads, o->location());
        }
    }
    LCC_UNREACHABLE();
}

auto lcc::glint::Expr::Clone(Module& mod, Context* context, Expr* expr) -> Expr* {
    LCC_ASSERT(context);
    // Don't pass me nullptr, I won't return it.
    if (not expr) return nullptr;

    // If we encounter a declaration, we create a new scope, and map the
    // scope it was originally declared in into a new scope Clone() creates.
    std::unordered_map<Scope*, Scope*> scope_fixups{};

    return CloneImpl(mod, context, expr, scope_fixups);
}

auto lcc::glint::Module::function(std::string_view name) -> std::vector<lcc::glint::FuncDecl*> {
    std::vector<FuncDecl*> out{};
    for (auto* foo : _functions) {
        if (foo->name() == name) out.emplace_back(foo);
    }
    return out;
}

std::string lcc::glint::Expr::name() const {
    switch (kind()) {
        case Kind::While:
        case Kind::For:
        case Kind::Return:
        case Kind::IntegerLiteral:
        case Kind::StringLiteral:
        case Kind::CompoundLiteral:
        case Kind::OverloadSet:
        case Kind::EvaluatedConstant:
        case Kind::If:
        case Kind::Block:
        case Kind::MemberAccess:
        case Kind::Module:
        case Kind::Sizeof:
        case Kind::Alignof:
        case Kind::Call:
        case Kind::IntrinsicCall:
        case Kind::Cast:
        case Kind::Match:
        case Kind::Template:
            return ToString(kind());

        case Kind::NameRef:
            return as<NameRefExpr>(this)->name();

        case Kind::TypeDecl:
        case Kind::TypeAliasDecl:
        case Kind::EnumeratorDecl:
        case Kind::VarDecl:
        case Kind::FuncDecl:
            return as<Decl>(this)->name();

        case Kind::Unary: {
            switch (as<UnaryExpr>(this)->op()) {
                default: LCC_ASSERT(
                    false,
                    "Unhandled unary expression operator {}",
                    ToString(as<UnaryExpr>(this)->op())
                );
                case TokenKind::Ampersand: return "unary_addressof";
                case TokenKind::At: return "unary_dereference";
                case TokenKind::Minus: return "unary_negation";
                case TokenKind::Tilde: return "unary_bitnegation";
                case TokenKind::Exclam: return "unary_not";
                case TokenKind::Has: return "unary_has";
                case TokenKind::PlusPlus: return "unary_plusplus";
                case TokenKind::MinusMinus: return "unary_minusminus";
            }
        } break;

        case Kind::Binary: {
            switch (as<BinaryExpr>(this)->op()) {
                default: LCC_ASSERT(
                    false,
                    "Unhandled binary expression operator {}",
                    ToString(as<UnaryExpr>(this)->op())
                );

                case TokenKind::Dot:
                    return "binary_dot";

                /// Call and subscript have higher precedence than unary operators.
                /// Note: Unary operator precedence is 10'000.
                case TokenKind::LBrack: return "binary_subscript";

                case TokenKind::Plus: return "binary_add";
                case TokenKind::Minus: return "binary_subtract";
                case TokenKind::Star: return "binary_multiply";
                case TokenKind::Slash: return "binary_divide";
                case TokenKind::Percent: return "binary_modulo";

                case TokenKind::Shl: return "binary_shl";
                case TokenKind::Shr: return "binary_shr";

                case TokenKind::Ampersand: return "binary_bitand";
                case TokenKind::Pipe: return "binary_bitor";
                case TokenKind::Caret: return "binary_bitxor";

                case TokenKind::Eq: return "binary_equal";
                case TokenKind::Ne: return "binary_notequal";
                case TokenKind::Lt: return "binary_lessthan";
                case TokenKind::Gt: return "binary_greaterthan";
                case TokenKind::Le: return "binary_lessthan_orequal";
                case TokenKind::Ge: return "binary_greaterthan_orequal";

                case TokenKind::And: return "binary_and";
                case TokenKind::Or: return "binary_or";

                case TokenKind::ColonEq: return "binary_assignment";
                case TokenKind::ColonColon: return "declaration_type_inferred";

                case TokenKind::Tilde: return "binary_bitnot";
                case TokenKind::RightArrow: return "binary_rightarrow";

                case TokenKind::PlusEq: return "binary_plus_assign";
                case TokenKind::MinusEq: return "binary_subtract_assign";
                case TokenKind::StarEq: return "binary_multiply_assign";
                case TokenKind::SlashEq: return "binary_divide_assign";
                case TokenKind::PercentEq: return "binary_modulo_assign";
                case TokenKind::AmpersandEq: return "binary_bitand_assign";
                case TokenKind::PipeEq: return "binary_bitor_assign";
                case TokenKind::CaretEq: return "binary_bitxor_assign";
                case TokenKind::TildeEq: return "binary_bitnot_assign";
                case TokenKind::LBrackEq: return "binary_subscript_assign";
            }
        }
        case Kind::Type: {
            auto ty = type();
            switch (ty->kind()) {
                case Type::Kind::Builtin: {
                    switch (as<BuiltinType>(ty)->builtin_kind()) {
                        case BuiltinType::BuiltinKind::Unknown: return "t_unknown";
                        case BuiltinType::BuiltinKind::Bool: return "t_bool";
                        case BuiltinType::BuiltinKind::Byte: return "t_byte";
                        case BuiltinType::BuiltinKind::Int: return "t_int";
                        case BuiltinType::BuiltinKind::UInt: return "t_uint";
                        case BuiltinType::BuiltinKind::Void: return "t_void";
                        case BuiltinType::BuiltinKind::OverloadSet: return "t_overloadset";
                    }
                    LCC_UNREACHABLE();
                }
                case Type::Kind::FFIType: return "t_ffi";
                case Type::Kind::Named: return "t_named";
                case Type::Kind::Pointer: return "t_ptr";
                case Type::Kind::Reference: return "t_ref";
                case Type::Kind::DynamicArray: return "t_dynarray";
                case Type::Kind::ArrayView: return "t_view";
                case Type::Kind::Array: return "t_fixarray";
                case Type::Kind::Function: return "t_function";
                case Type::Kind::Sum: return "t_sum";
                case Type::Kind::Union: return "t_union";
                case Type::Kind::Enum: return "t_enum";
                case Type::Kind::Struct: return "t_struct";
                case Type::Kind::Integer: return "t_int";
                case Type::Kind::Typeof: return "t_typeof";
            }
            LCC_UNREACHABLE();
        } break;
    }
    LCC_UNREACHABLE();
}

auto lcc::glint::Expr::children_ref() -> std::vector<lcc::glint::Expr**> {
    switch (kind()) {
        // These expressions never have children
        case Kind::OverloadSet:
        case Kind::EvaluatedConstant:
        case Kind::TypeDecl:
        case Kind::FuncDecl:
        case Kind::TypeAliasDecl:
        case Kind::EnumeratorDecl:
        case Kind::IntegerLiteral:
        case Kind::StringLiteral:
        case Kind::IntrinsicCall:
        case Kind::Module:
            return {};

        case Kind::Type:
            // TODO: Structs with initialised members?
            if (auto* t_dynarray = cast<DynamicArrayType>(type())) {
                if (t_dynarray->initial_size())
                    return {&t_dynarray->initial_size()};
            }
            if (auto* t_fixarray = cast<ArrayType>(type()))
                return {&t_fixarray->size()};
            if (auto* t_typeof = cast<TypeofType>(type()))
                return {&t_typeof->expression()};
            return {};

        case Kind::While: {
            auto* w = as<lcc::glint::WhileExpr>(this);
            auto out = std::vector<Expr**>{&w->condition()};
            out.emplace_back(&w->body());
            return out;
        }

        case Kind::For: {
            auto* f = as<lcc::glint::ForExpr>(this);
            return {&f->init(), &f->condition(), &f->increment(), &f->body()};
        }

        case Kind::If: {
            auto* i = as<lcc::glint::IfExpr>(this);
            if (i->otherwise())
                return {&i->condition(), &i->then(), &i->otherwise()};
            else return {&i->condition(), &i->then()};
        }

        case Kind::Return: {
            auto* ret = as<lcc::glint::ReturnExpr>(this);
            if (ret->value()) return {&ret->value()};
            return {};
        }

        case Kind::Match: {
            auto* match = as<lcc::glint::MatchExpr>(this);
            std::vector<lcc::glint::Expr**> children{&match->object()};
            for (auto*& b : match->bodies()) children.push_back(&b);
            return children;
        }

        case Kind::MemberAccess:
            return {&as<lcc::glint::MemberAccessExpr>(this)->object()};

        case Kind::CompoundLiteral: {
            auto* c = as<lcc::glint::CompoundLiteral>(this);
            return c->children_ref();
        }

        case Kind::Cast:
            return {&as<lcc::glint::CastExpr>(this)->operand()};

        case Kind::Call: {
            auto* c = as<lcc::glint::CallExpr>(this);
            std::vector<lcc::glint::Expr**> children{&c->callee()};
            for (auto& a : c->args())
                children.emplace_back(&a);
            return children;
        } break;

        case Kind::Sizeof:
            return {as<lcc::glint::SizeofExpr>(this)->expr_ref()};

        case Kind::Alignof:
            return {as<lcc::glint::AlignofExpr>(this)->expr_ref()};

        case Kind::Template: {
            auto* t = as<lcc::glint::TemplateExpr>(this);
            return {t->body_ref()};
        }

        case Kind::VarDecl: {
            auto* v = as<lcc::glint::VarDecl>(this);
            if (v->init()) return {&v->init()};
            return {};
        }

        case Kind::NameRef: {
            auto* n = as<lcc::glint::NameRefExpr>(this);
            if (n->target())
                return {n->target_ref()};
            return {};
        }

        case Kind::Block: {
            auto* b = as<lcc::glint::BlockExpr>(this);
            auto children = vws::transform(b->children(), [](auto*& e) { return &e; });
            return {children.begin(), children.end()};
        }

        case Kind::Unary: {
            auto* u = as<lcc::glint::UnaryExpr>(this);
            return {&u->operand()};
        }

        case Kind::Binary: {
            auto* b = as<lcc::glint::BinaryExpr>(this);
            return {&b->lhs(), &b->rhs()};
        }
    }
    LCC_UNREACHABLE();
}

auto lcc::glint::Expr::children() const -> std::vector<lcc::glint::Expr*> {
    switch (kind()) {
        case Kind::FuncDecl:
        case Kind::OverloadSet:
        case Kind::EvaluatedConstant:
        case Kind::TypeDecl:
        case Kind::TypeAliasDecl:
        case Kind::EnumeratorDecl:
        case Kind::IntegerLiteral:
        case Kind::StringLiteral:
        case Kind::IntrinsicCall:
        case Kind::Module:
            return {};

        case Kind::Type:
            if (auto* t_dynarray = cast<DynamicArrayType>(type())) {
                if (t_dynarray->initial_size())
                    return {t_dynarray->initial_size()};
            }
            if (auto* t_fixarray = cast<ArrayType>(type()))
                return {t_fixarray->size()};

            if (auto* t_typeof = cast<TypeofType>(type()))
                return {t_typeof->expression()};

            return {};

        case Kind::While: {
            const auto* w = as<lcc::glint::WhileExpr>(this);
            return {w->condition(), w->body()};
        }

        case Kind::For: {
            const auto* f = as<lcc::glint::ForExpr>(this);
            return {f->init(), f->condition(), f->increment(), f->body()};
        }

        case Kind::If: {
            const auto* i = as<lcc::glint::IfExpr>(this);
            if (i->otherwise())
                return {i->condition(), i->then(), i->otherwise()};
            else return {i->condition(), i->then()};
        }

        case Kind::Return: {
            const auto* ret = as<lcc::glint::ReturnExpr>(this);
            if (ret->value()) return {ret->value()};
            return {};
        }

        case Kind::Match: {
            const auto* match = as<lcc::glint::MatchExpr>(this);
            std::vector<lcc::glint::Expr*> children{match->object()};
            for (auto* b : match->bodies()) children.push_back(b);
            return children;
        }

        case Kind::MemberAccess:
            return {as<lcc::glint::MemberAccessExpr>(this)->object()};

        case Kind::CompoundLiteral: {
            const auto* c = as<lcc::glint::CompoundLiteral>(this);
            return c->children();
        }

        case Kind::Cast:
            return {as<lcc::glint::CastExpr>(this)->operand()};

        case Kind::Call: {
            const auto* c = as<lcc::glint::CallExpr>(this);
            std::vector<lcc::glint::Expr*> children{c->callee()};
            children.insert(children.end(), c->args().begin(), c->args().end());
            return children;
        } break;

        case Kind::Sizeof:
            return {as<lcc::glint::SizeofExpr>(this)->expr()};

        case Kind::Alignof:
            return {as<lcc::glint::AlignofExpr>(this)->expr()};

        case Kind::Template: {
            const auto* t = as<lcc::glint::TemplateExpr>(this);
            return {t->body()};
        }

        case Kind::VarDecl: {
            const auto* v = as<lcc::glint::VarDecl>(this);
            if (v->init()) return {v->init()};
            return {};
        } break;

        case Kind::NameRef: {
            const auto* n = as<lcc::glint::NameRefExpr>(this);
            if (n->target())
                return {n->target()};
            return {};
        }

        case Kind::Block: {
            const auto* b = as<lcc::glint::BlockExpr>(this);
            return b->children();
        }

        case Kind::Unary: {
            const auto* u = as<lcc::glint::UnaryExpr>(this);
            return {u->operand()};
        }

        case Kind::Binary: {
            const auto* b = as<lcc::glint::BinaryExpr>(this);
            return {b->lhs(), b->rhs()};
        }
    }
    LCC_UNREACHABLE();
}

auto lcc::glint::Expr::langtest_name() const -> std::string {
    switch (kind()) {
        case Kind::While:
        case Kind::For:
        case Kind::Return:
        case Kind::IntegerLiteral:
        case Kind::StringLiteral:
        case Kind::CompoundLiteral:
        case Kind::OverloadSet:
        case Kind::EvaluatedConstant:
        case Kind::If:
        case Kind::Block:
        case Kind::MemberAccess:
        case Kind::Module:
        case Kind::Sizeof:
        case Kind::Alignof:
        case Kind::Call:
        case Kind::IntrinsicCall:
        case Kind::Cast:
        case Kind::Match:
        case Kind::Template:
        case Kind::TypeDecl:
        case Kind::TypeAliasDecl:
        case Kind::EnumeratorDecl:
        case Kind::VarDecl:
        case Kind::FuncDecl:
        case Kind::NameRef:
            return ToString(kind());

        case Kind::Unary:
        case Kind::Binary:
        case Kind::Type:
            return name();
    }
    LCC_UNREACHABLE();
}
auto lcc::glint::Expr::langtest_children() const -> std::vector<lcc::glint::Expr*> {
    // Return function body as child of function declaration.
    if (auto* func_decl = cast<FuncDecl>(this))
        return {func_decl->body()};
    // Do not follow NameRefExpr when testing (lots of duplicate variable declarations).
    if (is<NameRefExpr>(this)) return {};
    return children();
}

auto lcc::glint::EnumeratorDecl::value() const -> aint {
    LCC_ASSERT(ok(), "value() can only be used if the enumerator was analysed successfully");
    return is<ConstantExpr>(init())
             ? as<ConstantExpr>(init())->value().as_int()
             : as<IntegerLiteral>(init())->value();
}

auto lcc::glint::Module::ToSource(const lcc::glint::Type& t) -> lcc::Result<std::string> {
    switch (t.kind()) {
        case lcc::glint::Type::Kind::Pointer: {
            auto elem_t = ToSource(*t.elem());
            if (not elem_t) return elem_t.diag();
            return fmt::format("{}.ptr", *elem_t);
        }

        case lcc::glint::Type::Kind::Reference: {
            auto elem_t = ToSource(*t.elem());
            if (not elem_t) return elem_t.diag();
            return fmt::format("{}.ref", *elem_t);
        }

        case lcc::glint::Type::Kind::Builtin: {
            auto builtin_t = lcc::as<lcc::glint::BuiltinType>(&t);
            switch (builtin_t->builtin_kind()) {
                case lcc::glint::BuiltinType::BuiltinKind::Unknown:
                    return lcc::Diag::Error("Unknown builtin type...");
                case lcc::glint::BuiltinType::BuiltinKind::OverloadSet:
                    return lcc::Diag::Error("Cannot convert overload set to source...");
                case lcc::glint::BuiltinType::BuiltinKind::Bool: return {"bool"};
                case lcc::glint::BuiltinType::BuiltinKind::Byte: return {"byte"};
                case lcc::glint::BuiltinType::BuiltinKind::Int: return {"int"};
                case lcc::glint::BuiltinType::BuiltinKind::UInt: return {"uint"};
                case lcc::glint::BuiltinType::BuiltinKind::Void: return {"void"};
            }
            LCC_UNREACHABLE();
        }
        case lcc::glint::Type::Kind::FFIType: {
            auto ffi_t = lcc::as<lcc::glint::FFIType>(&t);
            switch (ffi_t->ffi_kind()) {
                case lcc::glint::FFIType::FFIKind::CChar: return {"cchar"};
                case lcc::glint::FFIType::FFIKind::CSChar: return {"cschar"};
                case lcc::glint::FFIType::FFIKind::CUChar: return {"cuchar"};
                case lcc::glint::FFIType::FFIKind::CShort: return {"cshort"};
                case lcc::glint::FFIType::FFIKind::CUShort: return {"cushort"};
                case lcc::glint::FFIType::FFIKind::CInt: return {"cint"};
                case lcc::glint::FFIType::FFIKind::CUInt: return {"cuint"};
                case lcc::glint::FFIType::FFIKind::CLong: return {"clong"};
                case lcc::glint::FFIType::FFIKind::CULong: return {"culong"};
                case lcc::glint::FFIType::FFIKind::CLongLong: return {"clonglong"};
                case lcc::glint::FFIType::FFIKind::CULongLong: return {"culonglong"};
            }
            LCC_UNREACHABLE();
        }

        case lcc::glint::Type::Kind::Named:
            return lcc::as<lcc::glint::NamedType>(&t)->name();

        case lcc::glint::Type::Kind::DynamicArray: {
            auto elem_t = ToSource(*t.elem());
            if (not elem_t) return elem_t;
            return fmt::format("[{}]", *elem_t);
        }

        case lcc::glint::Type::Kind::Array: {
            auto array_t = lcc::as<lcc::glint::ArrayType>(&t);
            auto elem_t = ToSource(*array_t->elem());
            if (not elem_t) return elem_t;
            auto size = ToSource(*array_t->size());
            return fmt::format("[{} {}]", *elem_t, *size);
        }

        case lcc::glint::Type::Kind::ArrayView: {
            auto elem_t = ToSource(*t.elem());
            if (not elem_t) return elem_t;
            return fmt::format("[{} view]", *elem_t);
        }

        case lcc::glint::Type::Kind::Function: {
            auto function_t = lcc::as<lcc::glint::FuncType>(&t);
            auto ret_t = ToSource(*function_t->return_type());
            if (not ret_t) return ret_t;
            std::string params_string{};
            bool first_it{true};
            for (auto p : function_t->params()) {
                auto param_t = ToSource(*p.type);
                if (not param_t) return param_t;
                if (not first_it) params_string += ',';
                params_string += fmt::format("{}:{}", p.name, *param_t);

                first_it = false;
            }
            return fmt::format("{}({})", *ret_t, params_string);
        }

        case lcc::glint::Type::Kind::Integer: {
            auto i_t = lcc::as<lcc::glint::IntegerType>(&t);
            return fmt::format(
                "{}{}",
                i_t->is_signed() ? 's' : 'u',
                i_t->bit_width()
            );
        }

        case lcc::glint::Type::Kind::Sum: {
            auto sum_t = lcc::as<lcc::glint::SumType>(&t);
            std::string members_string{};
            for (auto m : sum_t->members()) {
                auto member_t = ToSource(*m.type);
                if (not member_t) return member_t;

                if (m.expr) {
                    auto member_init = ToSource(*m.expr);
                    if (not member_init) return member_init;
                    members_string += fmt::format("{}:{}={};", m.name, *member_t, *member_init);
                } else
                    members_string += fmt::format("{}:{};", m.name, *member_t);
            }
            return fmt::format("sum {{{}}}", members_string);
        }

        case lcc::glint::Type::Kind::Union: {
            auto union_t = lcc::as<lcc::glint::UnionType>(&t);
            std::string members_string{};
            for (auto m : union_t->members()) {
                auto member_t = ToSource(*m.type);
                if (not member_t) return member_t;
                members_string += fmt::format("{}:{};", m.name, *member_t);
            }
            return fmt::format("union {{{}}}", members_string);
        }

        case lcc::glint::Type::Kind::Struct: {
            auto struct_t = lcc::as<lcc::glint::StructType>(&t);
            std::string members_string{};
            for (auto m : struct_t->members()) {
                auto member_t = ToSource(*m.type);
                if (not member_t) return member_t;

                members_string += fmt::format("{}:{};", m.name, *member_t);
            }
            return fmt::format("struct {{{}}}", members_string);
        }

        case lcc::glint::Type::Kind::Enum: {
            auto enum_t = lcc::as<lcc::glint::EnumType>(&t);
            std::string enumerators_string{};
            for (auto e : enum_t->enumerators()) {
                enumerators_string += fmt::format("{}", e->name());
                if (e->init()) {
                    auto e_init = ToSource(*e->init());
                    if (not e_init) return e_init;
                    enumerators_string += fmt::format(" {}", *e_init);
                }
                enumerators_string += ';';
            }
            return fmt::format("enum {{{}}}", enumerators_string);
        }

        case lcc::glint::Type::Kind::Typeof: {
            auto typeof_t = lcc::as<lcc::glint::TypeofType>(&t);
            auto e_typeof = ToSource(*typeof_t->expression());
            if (not e_typeof) return e_typeof;
            return fmt::format("typeof {}", *e_typeof);
        }
    }
    LCC_UNREACHABLE();
}

auto lcc::glint::ToString(lcc::glint::Expr::Kind k) -> std::string {
    switch (k) {
        case lcc::glint::Expr::Kind::Template: return "template";
        case lcc::glint::Expr::Kind::While: return "while";
        case lcc::glint::Expr::Kind::For: return "for";
        case lcc::glint::Expr::Kind::Return: return "return";
        case lcc::glint::Expr::Kind::TypeDecl: return "type_declaration";
        case lcc::glint::Expr::Kind::TypeAliasDecl: return "type_alias_declaration";
        case lcc::glint::Expr::Kind::EnumeratorDecl: return "enum_declaration";
        case lcc::glint::Expr::Kind::VarDecl: return "variable_declaration";
        case lcc::glint::Expr::Kind::FuncDecl: return "function_declaration";
        case lcc::glint::Expr::Kind::IntegerLiteral: return "integer_literal";
        case lcc::glint::Expr::Kind::StringLiteral: return "string_literal";
        case lcc::glint::Expr::Kind::CompoundLiteral: return "compound_literal";
        case lcc::glint::Expr::Kind::OverloadSet: return "overload_set";
        case lcc::glint::Expr::Kind::EvaluatedConstant: return "evaluated_constant";
        case lcc::glint::Expr::Kind::If: return "if";
        case lcc::glint::Expr::Kind::Block: return "block";
        case lcc::glint::Expr::Kind::Call: return "call";
        case lcc::glint::Expr::Kind::IntrinsicCall: return "intrinsic";
        case lcc::glint::Expr::Kind::Cast: return "cast";
        case lcc::glint::Expr::Kind::Unary: return "unary";
        case lcc::glint::Expr::Kind::Binary: return "binary";
        case lcc::glint::Expr::Kind::NameRef: return "name";
        case lcc::glint::Expr::Kind::Type: return "type";
        case lcc::glint::Expr::Kind::MemberAccess: return "member_access";
        case lcc::glint::Expr::Kind::Module: return "module";
        case lcc::glint::Expr::Kind::Sizeof: return "sizeof";
        case lcc::glint::Expr::Kind::Alignof: return "alignof";
        case lcc::glint::Expr::Kind::Match: return "match";
    }
    LCC_UNREACHABLE();
}

auto lcc::glint::Module::ToSource(const Expr& e) -> Result<std::string> {
    switch (e.kind()) {
        case Expr::Kind::While: {
            auto e_while = as<WhileExpr>(&e);
            auto e_condition = ToSource(*e_while->condition());
            if (not e_condition) return e_condition;
            auto e_body = ToSource(*e_while->body());
            if (not e_body) return e_body;
            return fmt::format("while {}, {}", *e_condition, *e_body);
        }

        case Expr::Kind::For: {
            auto e_for = as<ForExpr>(&e);
            auto e_init = ToSource(*e_for->init());
            if (not e_init) return e_init;
            auto e_cond = ToSource(*e_for->condition());
            if (not e_cond) return e_cond;
            auto e_increment = ToSource(*e_for->increment());
            if (not e_increment) return e_increment;
            auto e_body = ToSource(*e_for->body());
            if (not e_body) return e_body;
            return fmt::format("cfor {};{};{};{}", *e_init, *e_cond, *e_increment, *e_body);
        }

        case Expr::Kind::If: {
            auto e_if = as<IfExpr>(&e);
            auto e_cond = ToSource(*e_if->condition());
            if (not e_cond) return e_cond;
            auto e_then = ToSource(*e_if->then());
            if (not e_then) return e_then;
            if (e_if->otherwise()) {
                auto e_otherwise = ToSource(*e_if->then());
                if (not e_otherwise) return e_otherwise;
                return fmt::format("if {},{} else {}", *e_cond, *e_then, *e_otherwise);
            }
            return fmt::format("if {},{}", *e_cond, *e_then);
        }

        case Expr::Kind::Return: {
            auto e_ret = as<ReturnExpr>(&e);
            if (e_ret->value()) {
                auto e_expression = ToSource(*e_ret->value());
                if (not e_expression) return e_expression;
                return fmt::format("return {}", *e_expression);
            }
            return {"return"};
        }

        case Expr::Kind::Template: {
            auto e_t = as<TemplateExpr>(&e);
            auto e_body = ToSource(*e_t->body());
            std::string params_string = fmt::format(
                "{}",
                fmt::join(
                    vws::transform(e_t->params(), [this](const auto& p) {
                        auto e_ptype = ToSource(*p.type);
                        if (not e_ptype) return std::string{"ERROR"};
                        return fmt::format("{}:{}", p.name, *e_ptype);
                    }),
                    ", "
                )
            );
            return fmt::format("template({}) {}", params_string, *e_body);
        }

        case Expr::Kind::Sizeof: {
            auto e_sizeof = as<SizeofExpr>(&e);
            auto e_expression = ToSource(*e_sizeof->expr());
            if (not e_expression) return e_expression;
            return fmt::format("sizeof {}", *e_expression);
        }

        case Expr::Kind::Alignof: {
            auto e_alignof = as<AlignofExpr>(&e);
            auto e_expression = ToSource(*e_alignof->expr());
            if (not e_expression) return e_expression;
            return fmt::format("alignof {}", *e_expression);
        }

        case Expr::Kind::Cast: {
            auto e_cast = as<CastExpr>(&e);
            auto e_expression = ToSource(*e_cast->operand());
            if (not e_expression) return e_expression;
            auto e_type = ToSource(*e.type());
            if (not e_type) return e_type;
            return fmt::format("{} {}", *e_type, *e_expression);
        }

        case Expr::Kind::Type: return ToSource(*e.type());

        case Expr::Kind::Unary: {
            auto e_unary = as<UnaryExpr>(&e);
            auto e_expression = ToSource(*e_unary->operand());
            if (not e_expression) return e_expression;
            return fmt::format("{} {}", ToString(e_unary->op()), *e_expression);
        }

        case Expr::Kind::Binary: {
            auto e_binary = as<BinaryExpr>(&e);
            auto e_lhs = ToSource(*e_binary->lhs());
            if (not e_lhs) return e_lhs;
            auto e_rhs = ToSource(*e_binary->rhs());
            if (not e_rhs) return e_rhs;
            return fmt::format("{} {} {}", *e_lhs, ToString(e_binary->op()), *e_rhs);
        }

        case Expr::Kind::NameRef: {
            auto e_name = as<NameRefExpr>(&e);
            return e_name->name();
        }

        case Expr::Kind::MemberAccess: {
            auto e_access = as<MemberAccessExpr>(&e);
            auto e_object = ToSource(*e_access->object());
            if (not e_object) return e_object;
            return fmt::format("{}.{}", *e_object, e_access->name());
        }

        case Expr::Kind::Module: {
            auto e_module = as<ModuleExpr>(&e);
            return fmt::format("import {}", e_module->mod()->name());
        }

        case Expr::Kind::IntegerLiteral:
            return fmt::format("{}", as<IntegerLiteral>(&e)->value());

        case Expr::Kind::StringLiteral:
            return fmt::format("\"{}\"", strings.at(as<StringLiteral>(&e)->string_index()));

        case Expr::Kind::Block: {
            std::string exprs_string{};
            for (auto child : e.children()) {
                auto expr_string = ToSource(*child);
                if (not expr_string) return expr_string;
                exprs_string += *expr_string;
                exprs_string += ';';
            }
            return fmt::format("{{{}}}", exprs_string);
        }

        case Expr::Kind::Call: {
            auto e_call = as<CallExpr>(&e);
            auto e_callee = ToSource(*e_call->callee());
            if (not e_callee) return e_callee;

            std::string args_string{};
            bool first_it{true};
            for (auto arg : e_call->args()) {
                auto arg_string = ToSource(*arg);
                if (not arg_string) return arg_string;

                // Separate callee and first argument by a space.
                // Separate arguments by a soft separator.
                if (not first_it) args_string += ',';
                else args_string += ' ';

                args_string += *arg_string;

                first_it = false;
            }

            return fmt::format("{}{}", *e_callee, args_string);
        }

        case Expr::Kind::CompoundLiteral: {
            std::string exprs_string{};
            for (auto child : e.children()) {
                auto expr_string = ToSource(*child);
                if (not expr_string) return expr_string;
                exprs_string += *expr_string;
                exprs_string += ',';
            }
            return fmt::format("!{{{}}}", exprs_string);
        }

        case Expr::Kind::VarDecl: {
            auto e_decl = as<VarDecl>(&e);

            auto e_decltype = ToSource(*e_decl->type());
            if (not e_decltype) return e_decltype;

            std::string specifier{};
            switch (e_decl->linkage()) {
                case Linkage::LocalVar:
                case Linkage::Internal:
                case Linkage::Used:
                    break;

                case Linkage::Exported:
                    specifier = "export";
                    break;
                case Linkage::Imported:
                    specifier = "import";
                    break;
                case Linkage::Reexported:
                    specifier = "export import";
                    break;
            }

            if (e_decl->init()) {
                auto e_init = ToSource(*e_decl->init());
                if (not e_init) return e_init;
                return fmt::format("{}:{} {}", e_decl->name(), *e_decltype, *e_init);
            }
            return fmt::format("{}:{}", e_decl->name(), *e_decltype);
        }

        case Expr::Kind::TypeDecl: {
            auto e_decl = as<TypeDecl>(&e);

            auto e_decltype = ToSource(*e_decl->type());
            if (not e_decltype) return e_decltype;

            return fmt::format("{}:{}", e_decl->name(), *e_decltype);
        }

        case Expr::Kind::FuncDecl: {
            auto e_fdecl = as<FuncDecl>(&e);

            auto e_decltype = ToSource(*e_fdecl->function_type());
            if (not e_decltype) return e_decltype;

            return fmt::format("{}:{}", e_fdecl->name(), *e_decltype);
        }

        case Expr::Kind::EnumeratorDecl: {
            auto e_decl = as<EnumeratorDecl>(&e);

            if (e_decl->init()) {
                auto e_init = ToSource(*e_decl->init());
                if (not e_init) return e_init;

                return fmt::format("{}={}", e_decl->name(), *e_init);
            }

            return fmt::format("{}", e_decl->name());
        }

        case Expr::Kind::EvaluatedConstant: {
            auto e_constant = as<ConstantExpr>(&e);
            if (e_constant->expr())
                return ToSource(*e_constant->expr());
            if (e_constant->value().is_int()) return fmt::format("{}", e_constant->value().as_int());
            if (e_constant->value().is_string()) return fmt::format("\"{}\"", strings.at(e_constant->value().as_string()->string_index()));
            if (e_constant->value().is_null()) return {"NULL"};
            LCC_ASSERT(false, "Cannot generate source from ill-formed constant expression");
        }

        case Expr::Kind::Match: {
            auto e_match = as<MatchExpr>(&e);

            auto e_matchee = ToSource(*e_match->object());
            if (not e_matchee) return e_matchee;

            std::string match_exprs_string{};
            for (auto [name, body] : vws::zip(e_match->names(), e_match->bodies())) {
                auto e_body = ToSource(*body);
                if (not e_body) return e_body;
                match_exprs_string += fmt::format(".{}:{};", name, *e_body);
            }

            return fmt::format("match {} {{{}}}", *e_matchee, match_exprs_string);
        }

        case Expr::Kind::TypeAliasDecl:
        case Expr::Kind::IntrinsicCall:
        case Expr::Kind::OverloadSet:
            LCC_TODO("Implement ToSource for expression kind {}", ToString(e.kind()));
    }
    LCC_UNREACHABLE();
}

/// ===========================================================================
///  AST Printing
/// ===========================================================================
namespace {
using lcc::as;
using lcc::cast;
using lcc::is;

struct ASTPrinter : lcc::utils::ASTPrinter<ASTPrinter, lcc::glint::Expr, lcc::glint::Type> {
    // NOTE: Can use to override name_colour from ast_printer.hh for Glint names.
    // static constexpr lcc::utils::Colour name_colour{Green};

    // Used to highlight key details, like binary/unary operators, integer literal values, etc.
    static constexpr lcc::utils::Colour key_detail_colour{Red};

    std::unordered_set<const lcc::glint::FuncDecl*> printed_functions{};
    bool print_children_of_children = true;

    void PrintLValue(const lcc::glint::Expr* e) {
        if (e->is_lvalue()) out += fmt::format(" {}lvalue", C(ASTPrinter::base_colour));
    };

    void PrintBasicGlintNode(std::string_view name, const lcc::glint::Expr* node, lcc::glint::Type* t) {
        PrintBasicNode(name, node, t, false);
        PrintLValue(node);
        out += '\n';
    };

    /// Print the header (name + location + type) of a node.
    void PrintHeader(const lcc::glint::Expr* e) {
        using K = lcc::glint::Expr::Kind;
        switch (e->kind()) {
            case K::FuncDecl: {
                auto* f = as<lcc::glint::FuncDecl>(e);
                PrintLinkage(f->linkage());
                PrintBasicHeader("FuncDecl", e);
                out += fmt::format(
                    " {}{} {}\n",
                    C(name_colour),
                    f->name(),
                    f->type()->string(use_colour)
                );
                return;
            }

            case K::VarDecl: {
                auto* v = as<lcc::glint::VarDecl>(e);
                PrintLinkage(v->linkage());
                PrintBasicHeader("VarDecl", e);
                out += fmt::format(
                    " {}{} {}",
                    C(name_colour),
                    v->name(),
                    v->type()->string(use_colour)
                );
                PrintLValue(e);
                out += '\n';
                return;
            }

            case K::EnumeratorDecl: {
                auto* v = as<lcc::glint::EnumeratorDecl>(e);
                PrintBasicHeader("EnumeratorDecl", e);
                out += fmt::format(
                    " {}{} {}{}\n",
                    C(name_colour),
                    v->name(),
                    C(key_detail_colour),
                    v->ok() ? v->value().str() : "?"
                );
                return;
            }
            case K::Binary: {
                const auto* b = as<lcc::glint::BinaryExpr>(e);
                PrintBasicHeader("BinaryExpr", e);
                out += fmt::format(
                    " {}{} {}",
                    C(key_detail_colour),
                    lcc::glint::ToString(b->op()),
                    b->type()->string(use_colour)
                );
                PrintLValue(e);
                out += '\n';
                return;
            }

            case K::Unary: {
                auto* u = as<lcc::glint::UnaryExpr>(e);
                PrintBasicHeader("UnaryExpr", e);
                out += fmt::format(
                    " {}{} {}",
                    C(key_detail_colour),
                    lcc::glint::ToString(u->op()),
                    u->type()->string(use_colour)
                );
                PrintLValue(e);
                out += '\n';
                return;
            }

            case K::IntegerLiteral: {
                auto* i = as<lcc::glint::IntegerLiteral>(e);
                PrintBasicHeader("IntegerLiteral", e);
                out += fmt::format(
                    " {}{} {}\n",
                    C(key_detail_colour),
                    i->value(),
                    i->type()->string(use_colour)
                );
                return;
            }

            case K::NameRef: {
                auto* n = as<lcc::glint::NameRefExpr>(e);
                PrintBasicHeader("NameRefExpr", e);
                out += fmt::format(
                    " {}{} {}",
                    C(name_colour),
                    n->name(),
                    n->type()->string(use_colour)
                );
                PrintLValue(e);
                out += '\n';
                return;
            }

            case K::Cast: {
                auto* c = as<lcc::glint::CastExpr>(e);
                PrintBasicHeader("CastExpr", e);
                switch (c->cast_kind()) {
                    case lcc::glint::CastKind::SoftCast: out += fmt::format(" {}Soft ", C(key_detail_colour)); break;
                    case lcc::glint::CastKind::HardCast: out += fmt::format(" {}Hard ", C(key_detail_colour)); break;
                    case lcc::glint::CastKind::ImplicitCast: out += fmt::format(" {}Implicit ", C(key_detail_colour)); break;
                    case lcc::glint::CastKind::LValueToRValueConv: out += fmt::format(" {}LValueToRValue ", C(key_detail_colour)); break;
                    case lcc::glint::CastKind::LValueToReference: out += fmt::format(" {}LValueToReference ", C(key_detail_colour)); break;
                    case lcc::glint::CastKind::ReferenceToLValue: out += fmt::format(" {}ReferenceToLValue ", C(key_detail_colour)); break;
                }
                out += e->type()->string(use_colour);
                PrintLValue(e);
                out += '\n';
                return;
            }

            case K::If: {
                PrintBasicHeader("IfExpr", e);
                if (not e->type()->is_void()) out += fmt::format(" {}", e->type()->string(use_colour));
                PrintLValue(e);
                out += '\n';
                return;
            }

            case K::Match: {
                PrintBasicHeader("MatchExpr", e);
                const auto* m = as<lcc::glint::MatchExpr>(e);
                out += fmt::format(
                    " on {}{} {}",
                    C(name_colour),
                    m->object()->name(),
                    m->object()->type()->string(use_colour)
                );
                out += '\n';
                return;
            }

            case K::Template: {
                PrintBasicHeader("TemplateExpr", e);
                const auto* t = as<lcc::glint::TemplateExpr>(e);
                out += fmt::format("{}(", C(Reset));
                out += fmt::format(
                    "{})",
                    fmt::join(
                        lcc::vws::transform(t->params(), [](const auto& p) {
                            return fmt::format("{}:{}", p.name, *p.type);
                        }),
                        ", "
                    )
                );
                out += '\n';
                return;
            }

            case K::OverloadSet: PrintBasicGlintNode("OverloadSet", e, e->type()); return;
            case K::EvaluatedConstant: PrintBasicGlintNode("ConstantExpr", e, e->type()); return;
            case K::Type: PrintBasicGlintNode("TypeExpr", e, e->type()); return;
            case K::TypeDecl: PrintBasicGlintNode("TypeDecl", e, e->type()); return;
            case K::TypeAliasDecl: PrintBasicGlintNode("TypeAliasDecl", e, e->type()); return;
            case K::StringLiteral: PrintBasicGlintNode("StringLiteral", e, e->type()); return;
            case K::CompoundLiteral: PrintBasicGlintNode("CompoundLiteral", e, e->type()); return;
            case K::MemberAccess:
                PrintBasicHeader("MemberAccessExpr", e);

                // Member identifier
                out += fmt::format(
                    " {}.{}",
                    C(name_colour),
                    as<lcc::glint::MemberAccessExpr>(e)->name()
                );

                // Type + lvalue
                out += fmt::format(" {}", e->type()->string(use_colour));
                PrintLValue(e);

                out += '\n';
                return;
            case K::While: PrintBasicGlintNode("WhileExpr", e, nullptr); return;
            case K::For: PrintBasicGlintNode("ForExpr", e, nullptr); return;
            case K::Block: PrintBasicGlintNode("BlockExpr", e, e->type()); return;
            case K::Return: PrintBasicGlintNode("ReturnExpr", e, nullptr); return;
            case K::Call: PrintBasicGlintNode("CallExpr", e, e->type()); return;
            case K::IntrinsicCall: PrintBasicGlintNode("IntrinsicCallExpr", e, e->type()); return;
            case K::Module: PrintBasicGlintNode("ModuleExpr", e, nullptr); return;
            case K::Sizeof: PrintBasicGlintNode("SizeofExpr", e, lcc::glint::Type::Int); return;
            case K::Alignof: PrintBasicGlintNode("AlignofExpr", e, lcc::glint::Type::Int); return;
        }

        PrintBasicGlintNode(R"(<???>)", e, e->type());
    }

    void PrintNodeChildren(const lcc::glint::Expr* e, std::string leading_text = "") {
        if (not print_children_of_children) return;

        // Only print function bodies at the top level.
        if (e->kind() == lcc::glint::Expr::Kind::FuncDecl) return;

        if (auto* n = cast<lcc::glint::NameRefExpr>(e)) {
            // TODO: Get rid of tempset if we don't need it here.
            tempset print_children_of_children = false;
            PrintChildren(n->children(), leading_text);
            return;
        }

        /// Print the children of a node.
        PrintChildren(e->children(), leading_text);

        return;
    }

    /// Print a top-level node.
    void PrintTopLevelNode(const lcc::glint::Expr* e) {
        PrintHeader(e);
        if (const auto* f = cast<lcc::glint::FuncDecl>(e)) {
            printed_functions.insert(f);
            if (auto* body = const_cast<lcc::glint::FuncDecl*>(f)->body()) {
                if (auto* block = cast<lcc::glint::BlockExpr>(body)) {
                    PrintChildren(block->children(), "");
                } else {
                    lcc::glint::Expr* children[] = {const_cast<lcc::glint::FuncDecl*>(f)->body()};
                    PrintChildren(children, "");
                }
            }
        } else {
            PrintNodeChildren(e);
        }
    }

    /// Print a node.
    void operator()(const lcc::glint::Expr* e, std::string leading_text = "") {
        PrintHeader(e);
        PrintNodeChildren(e, std::move(leading_text));
    }

    void print(lcc::glint::Module* mod) {
        printed_functions.insert(mod->top_level_function());
        if (auto* funcbody = cast<lcc::glint::BlockExpr>(mod->top_level_function()->body())) {
            for (auto* node : funcbody->children())
                PrintTopLevelNode(node);
        } else PrintTopLevelNode(mod->top_level_function()->body());

        for (auto* f : mod->functions())
            if (not printed_functions.contains(f))
                PrintTopLevelNode(f);
    }
};
} // namespace

auto lcc::glint::Type::representation(std::unordered_set<const Type*> containing_types) const -> std::string {
    // Special case for handling recursive structs.
    // If this type is a struct type AND a containing type, use the structs
    // name instead of it's member definition.
    if (containing_types.contains(this) and is<StructType>(this)) {
        // FIXME: Should this be a different encoding?
        auto d = as<StructType>(this)->decl();
        return fmt::format("N{}_{}", d->name().size(), d->name());
    }

    if (containing_types.contains(this)) {
        Diag::ICE("Infinite recursion caught in Type::representation(): {}", string());
    }
    containing_types.emplace(this);

    switch (kind()) {
        case Kind::Builtin:
            return fmt::format("B{}", usz(as<BuiltinType>(this)->builtin_kind()));
        case Kind::FFIType:
            return fmt::format("FFI{}", usz(as<FFIType>(this)->ffi_kind()));
        case Kind::Named:
            return fmt::format("N{}_{}", as<NamedType>(this)->name().size(), as<NamedType>(this)->name());
        case Kind::Pointer:
            return fmt::format("P{}", elem()->representation(containing_types));
        case Kind::Reference:
            return fmt::format("R{}", elem()->representation(containing_types));
        case Kind::DynamicArray:
            return fmt::format("DY{}", elem()->representation(containing_types));
        case Kind::Array:
            LCC_ASSERT(
                is<ConstantExpr>(as<ArrayType>(this)->size()),
                "Array type size is not a constant expression"
            );
            return fmt::format("ARR{}_{}", as<ConstantExpr>(as<ArrayType>(this)->size())->value().as_int(), elem()->representation(containing_types));
        case Kind::ArrayView:
            return fmt::format("VW{}", elem()->representation(containing_types));
        case Kind::Function: {
            auto f = as<FuncType>(this);
            auto out = fmt::format("F{}_{}", f->params().size(), f->return_type()->representation(containing_types));
            for (auto p : f->params()) {
                out += '_';
                out += p.type->representation(containing_types);
            }
            return out;
        }
        case Kind::Sum: {
            auto s = as<SumType>(this);
            auto out = fmt::format("SUM{}", s->members().size());
            for (auto m : s->members()) {
                out += '_';
                out += m.type->representation(containing_types);
            }
            return out;
        }
        case Kind::Union: {
            auto u = as<UnionType>(this);
            auto out = fmt::format("UNN{}", u->members().size());
            for (auto m : u->members()) {
                out += '_';
                out += m.type->representation(containing_types);
            }
            return out;
        }
        case Kind::Enum: {
            auto e = as<EnumType>(this);
            auto out = fmt::format(
                "ENM{}_{}",
                e->enumerators().size(),
                e->underlying_type()->representation(containing_types)
            );
            return out;
        }
        case Kind::Struct: {
            auto s = as<StructType>(this);
            auto out = fmt::format("SCT{}", s->members().size());
            for (auto m : s->members()) {
                out += '_';
                out += m.type->representation(containing_types);
            }
            return out;
        }
        case Kind::Integer: {
            return fmt::format("INT{}", as<IntegerType>(this)->size({}));
        }

        case Kind::Typeof:
            LCC_ASSERT(false, "Cannot get representation of TypeofType; sema should replace TypeofType with the type of it's contained expression.");
    }
    LCC_UNREACHABLE();
}

auto lcc::glint::Type::string(bool use_colours) const -> std::string {
    static constexpr lcc::utils::Colour type_colour{lcc::utils::Colour::Cyan};

    lcc::utils::Colours C{use_colours};
    using enum lcc::utils::Colour;

    switch (kind()) {
        case Kind::Named: return fmt::format("{}{}", C(White), as<NamedType>(this)->name());
        case Kind::Pointer: {
            /// If the element type of this pointer contains an array or
            /// function type, we need to use parentheses here to preserve
            /// precedence.
            bool has_arr_or_func = false;
            auto* el = elem();
            for (;;) {
                switch (el->kind()) {
                    default: break;

                    case Kind::Pointer:
                    case Kind::Reference:
                        el = el->elem();
                        continue;

                    case Kind::Array:
                    case Kind::Function:
                        has_arr_or_func = true;
                        break;
                }
                break;
            }

            return fmt::format(
                "{}{}{}{}{}.ptr{}{}{}",
                C(ASTPrinter::base_colour),
                has_arr_or_func ? "(" : "",
                C(type_colour),
                as<PointerType>(this)->element_type()->string(use_colours),
                C(type_colour),
                C(ASTPrinter::base_colour),
                has_arr_or_func ? ")" : "",
                C(Reset)
            );
        }
        case Kind::Reference: {
            bool has_func = false;
            auto* el = elem();
            for (;;) {
                switch (el->kind()) {
                    default: break;

                    case Kind::Pointer:
                    case Kind::Reference:
                        el = el->elem();
                        continue;

                    case Kind::Function:
                        has_func = true;
                        break;
                }
                break;
            }

            return fmt::format(
                "{}{}{}{}{}.ref{}{}{}",
                C(ASTPrinter::base_colour),
                has_func ? "(" : "",
                C(type_colour),
                as<ReferenceType>(this)->element_type()->string(use_colours),
                C(type_colour),
                C(ASTPrinter::base_colour),
                has_func ? ")" : "",
                C(Reset)
            );
        }

        case Kind::Integer: {
            auto* i = as<IntegerType>(this);
            return fmt::format("{}{}{}{}", C(type_colour), i->is_signed() ? "s" : "u", i->bit_width(), C(Reset));
        }

        case Kind::Struct: {
            auto* decl = as<StructType>(this)->decl();
            return fmt::format(
                "{}struct {}{}",
                C(type_colour),
                not decl or decl->name().empty() ? "<anonymous>" : decl->name(),
                C(Reset)
            );
        }

        case Kind::Sum: {
            auto* decl = as<SumType>(this)->decl();
            return fmt::format(
                "{}sum {}{}",
                C(type_colour),
                not decl or decl->name().empty() ? "<anonymous>" : decl->name(),
                C(Reset)
            );
        }

        case Kind::Union: {
            auto* decl = as<UnionType>(this)->decl();
            return fmt::format(
                "{}union {}{}",
                C(type_colour),
                not decl or decl->name().empty() ? "<anonymous>" : decl->name(),
                C(Reset)
            );
        }

        case Kind::Enum: {
            auto* decl = as<EnumType>(this)->decl();
            return fmt::format(
                "{}enum {}{}",
                C(type_colour),
                not decl or decl->name().empty() ? "<anonymous>" : decl->name(),
                C(Reset)
            );
        }

        case Kind::DynamicArray: {
            auto* arr = as<DynamicArrayType>(this);
            return fmt::format(
                "{}[{}{}]{}",
                C(type_colour),
                arr->element_type()->string(use_colours),
                C(type_colour),
                C(Reset)
            );
        }

        case Kind::ArrayView: {
            auto* arr = as<ArrayViewType>(this);
            return fmt::format(
                "{}[{}{} view]{}",
                C(type_colour),
                arr->element_type()->string(use_colours),
                C(type_colour),
                C(Reset)
            );
        }

        case Kind::Array: {
            auto* arr = as<ArrayType>(this);
            LCC_ASSERT(arr->size(), "ArrayType has NULL size expression");
            if (auto* sz = cast<ConstantExpr>(arr->size())) {
                return fmt::format(
                    "{}[{} {}{}{}]{}",
                    C(type_colour),
                    arr->element_type()->string(use_colours),
                    C(ASTPrinter::name_colour),
                    sz->value().as_int(),
                    C(type_colour),
                    C(Reset)
                );
            }
            return fmt::format(
                "{}[{}{}]{}",
                C(type_colour),
                arr->element_type()->string(use_colours),
                C(type_colour),
                C(Reset)
            );
        }

        case Kind::Builtin:
            switch (as<BuiltinType>(this)->builtin_kind()) {
                using K = BuiltinType::BuiltinKind;
                case K::Bool: return fmt::format("{}bool{}", C(type_colour), C(Reset));
                case K::Byte: return fmt::format("{}byte{}", C(type_colour), C(Reset));
                case K::Int: return fmt::format("{}int{}", C(type_colour), C(Reset));
                case K::UInt: return fmt::format("{}uint{}", C(type_colour), C(Reset));
                case K::Unknown: return fmt::format("{}?{}", C(type_colour), C(Reset));
                case K::Void: return fmt::format("{}void{}", C(type_colour), C(Reset));
                case K::OverloadSet: return fmt::format("{}<overload set>{}", C(type_colour), C(Reset));
            }
            LCC_UNREACHABLE();

        case Kind::FFIType:
            switch (as<FFIType>(this)->ffi_kind()) {
                using K = FFIType::FFIKind;
                case K::CChar: return fmt::format("{}__c_char{}", C(type_colour), C(Reset));
                case K::CSChar: return fmt::format("{}__c_schar{}", C(type_colour), C(Reset));
                case K::CUChar: return fmt::format("{}__c_uchar{}", C(type_colour), C(Reset));
                case K::CShort: return fmt::format("{}__c_short{}", C(type_colour), C(Reset));
                case K::CUShort: return fmt::format("{}__c_ushort{}", C(type_colour), C(Reset));
                case K::CInt: return fmt::format("{}__c_int{}", C(type_colour), C(Reset));
                case K::CUInt: return fmt::format("{}__c_uint{}", C(type_colour), C(Reset));
                case K::CLong: return fmt::format("{}__c_long{}", C(type_colour), C(Reset));
                case K::CULong: return fmt::format("{}__c_ulong{}", C(type_colour), C(Reset));
                case K::CLongLong: return fmt::format("{}__c_longlong{}", C(type_colour), C(Reset));
                case K::CULongLong: return fmt::format("{}__c_ulonglong{}", C(type_colour), C(Reset));
            }
            LCC_UNREACHABLE();

        case Kind::Typeof: {
            // TODO: String-ize expression, or something...
            return fmt::format("{}typeof{}", C(type_colour), C(Reset));
        }

        case Kind::Function: {
            const auto* f = as<FuncType>(this);
            std::string out = fmt::format("{}{}(", f->return_type()->string(use_colours), C(ASTPrinter::base_colour));
            for (const auto& arg : f->params()) {
                if (&arg != &f->params().front()) out += fmt::format("{}, ", C(ASTPrinter::base_colour));
                out += fmt::format("{}{}{}", C(ASTPrinter::name_colour), arg.name, C(ASTPrinter::base_colour));
                if (not arg.name.empty()) out += " : ";
                else out += ":";
                out += arg.type->string(use_colours);
            }
            out += fmt::format("{}){}", C(ASTPrinter::base_colour), C(Reset));
            return out;
        }
    }

    LCC_UNREACHABLE();
}

void lcc::glint::Module::print(bool use_colour) {
    ASTPrinter{use_colour}.print(this);
}

void lcc::glint::Expr::print(bool use_colour) const {
    ASTPrinter{use_colour}(this);
}

auto lcc::glint::UnionType::array_type(Module& mod) -> ArrayType* {
    // FIXME: magic number 8
    if (not _cached_type) {
        _cached_type = new (mod) ArrayType(
            new (mod) IntegerType(8, false, {}),
            new (mod) ConstantExpr(
                new (mod) IntegerLiteral(byte_size(), {}),
                EvalResult(aint(byte_size()))
            ),
            location()
        );
        _cached_type->element_type()->set_sema_done();
        _cached_type->size()->set_sema_done();
        _cached_type->set_sema_done();
    }

    return _cached_type;
}

auto lcc::glint::SumType::struct_type(Module& mod) -> StructType* {
    if (not _cached_struct) {
        auto tag_member = new (mod) IntegerType(64, false, {});
        tag_member->set_sema_done();

        auto data_member = new (mod) ArrayType(
            new (mod) IntegerType(8, false, {}),
            new (mod) ConstantExpr(
                new (mod) IntegerLiteral(byte_size(), {}),
                EvalResult(aint(byte_size()))
            ),
            location()
        );
        data_member->elem()->set_sema_done();
        data_member->set_sema_done();

        _cached_struct = new (mod) StructType(
            mod.global_scope(),
            {{"tag", tag_member, {}}, {"data", data_member, {}}},
            location()
        );
        _cached_struct->set_sema_done();
    }

    return _cached_struct;
}

auto lcc::glint::ObjectDecl::mangled_name() const -> std::string {
    // TODO: If we import it from a Glint module, we want to mangle still. If
    // it is external then we don't want to mangle. Currently, both of these
    // just get imported linkage (not enough info).
    if (
        auto* func_decl = cast<FuncDecl>(this);
        func_decl and func_decl->function_type()->has_attr(FuncAttr::NoMangle)
    ) {
        return name();
    }

    return fmt::format("_XGlint{}{}", name(), type()->representation());
}

auto lcc::glint::Module::enclosing_scope(Location l) const -> Scope* {
    Scope* out{scopes.at(0)};
    for (auto s : scopes) {
        if (s->location().is_valid()) {
            if (s->location().pos > l.pos)
                break;
            if (s->location().pos > out->location().pos)
                out = s;
        }
    }
    return out;
}

auto lcc::glint::GetRightmostLocation(lcc::glint::Expr* expr) -> lcc::Location {
    if (not expr) return {};

    lcc::Location location{expr->location()};

    // Attempt to get the location that is as close to where the semi-colon should be.

    // For variable declarations, choose the initializer. If there is no
    // initializer present, choose the type.
    if (expr->kind() == lcc::glint::Expr::Kind::VarDecl) {
        auto var_decl = lcc::as<lcc::glint::VarDecl>(expr);
        if (var_decl->init())
            location = var_decl->init()->location();
        else location = var_decl->type()->location();
    }

    // For function declarations, choose the body. If there is no body
    // present, choose the type.
    if (expr->kind() == lcc::glint::Expr::Kind::FuncDecl) {
        if (lcc::as<lcc::glint::FuncDecl>(expr)->body())
            location = lcc::as<lcc::glint::FuncDecl>(expr)->body()->location();
        else location = lcc::as<lcc::glint::FuncDecl>(expr)->type()->location();
    }

    // Limit location to length of one, discarding the beginning (fold right).
    if (location.len > 1) {
        location.pos += location.len - 1;
        location.len = 1;
    }

    return location;
}
