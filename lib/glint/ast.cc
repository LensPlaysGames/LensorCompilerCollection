#include <bit>
#include <glint/ast.hh>
#include <glint/module_description.hh>
#include <glint/parser.hh>
#include <lcc/target.hh>
#include <lcc/utils.hh>
#include <lcc/utils/ast_printer.hh>
#include <lcc/utils/macros.hh>
#include <lcc/utils/rtti.hh>
#include <linux/limits.h>
#include <type_traits>

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
    const Context* ctx,
    std::string&& name,
    Decl* decl
) -> Result<Decl*> {
    /// If the symbol already exists, then this is an error, unless
    /// that symbol is a function declaration, and this is also a
    /// function declaration.
    if (
        auto it = symbols.find(name);
        it != symbols.end()
        and not is<FuncDecl>(it->second)
        and not is<FuncDecl>(decl)
    ) return Diag::Error(ctx, decl->location(), "Redeclaration of '{}'", name);

    /// TODO: Check that this declaration is hygienic if it’s part of a macro.

    /// Otherwise, add the symbol.
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

        case Kind::DynamicArray:
            return as<DynamicArrayType>(this)->size(ctx);

        case Kind::Array: return elem()->align(ctx);
        case Kind::Struct: return as<StructType>(this)->alignment();
        case Kind::Integer: return std::bit_ceil(as<IntegerType>(this)->bit_width());
    }

    LCC_UNREACHABLE();
}

auto lcc::glint::Type::elem() const -> Type* {
    switch (kind()) {
        case Kind::Pointer: return as<PointerType>(this)->element_type();
        case Kind::Reference: return as<ReferenceType>(this)->element_type();
        case Kind::Array: return as<ArrayType>(this)->element_type();
        case Kind::DynamicArray: return as<DynamicArrayType>(this)->element_type();

        /// const_cast is ok because we’re just reading the underlying type.
        case Kind::Enum:
            return const_cast<EnumType*>(as<EnumType>(this))->underlying_type();

        case Kind::Builtin:
        case Kind::FFIType:
        case Kind::Named:
        case Kind::Function:
        case Kind::Struct:
        case Kind::Integer:
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

bool lcc::glint::Type::is_void() const { return ::is_builtin(this, BuiltinType::BuiltinKind::Void); }

auto lcc::glint::Type::size(const lcc::Context* ctx) const -> usz {
    LCC_ASSERT(sema_done_or_errored());
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
            return Type::VoidPtr->size(ctx) + DynamicArrayType::IntegerWidth * 2;

        case Kind::Array:
            return as<ArrayType>(this)->dimension() * elem()->size(ctx);

        case Kind::Struct: return as<StructType>(this)->byte_size() * 8;
        case Kind::Integer: return as<IntegerType>(this)->bit_width();
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

bool lcc::glint::Type::Equal(const Type* a, const Type* b) {
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

        case Kind::Integer: {
            auto ia = as<IntegerType>(a);
            auto ib = as<IntegerType>(b);
            return ia->bit_width() == ib->bit_width() and ia->is_signed() == ib->is_signed();
        }
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

auto lcc::glint::Expr::Clone(Module& mod, Expr* expr) -> Expr* {
    LCC_ASSERT(false, "TODO: Clone expressions");
}

std::string lcc::glint::Expr::name() const {
    switch (kind()) {
        case Kind::While:
        case Kind::For:
        case Kind::Return:
        case Kind::TypeDecl:
        case Kind::TypeAliasDecl:
        case Kind::EnumeratorDecl:
        case Kind::VarDecl:
        case Kind::FuncDecl:
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
        case Kind::NameRef:
            return ToString(kind());

        case Kind::Unary: {
            switch (as<UnaryExpr>(this)->op()) {
                case TokenKind::Ampersand: return "unary_addressof";
                case TokenKind::At: return "unary_dereference";
                case TokenKind::Minus: return "unary_negation";
                case TokenKind::Tilde: return "unary_negation";
                case TokenKind::Exclam: return "unary_not";
                default: LCC_ASSERT(
                    false,
                    "Unhandled unary expression operator {}",
                    ToString(as<UnaryExpr>(this)->op())
                );
            }
        } break;

        case Kind::Binary: {
            switch (as<BinaryExpr>(this)->op()) {
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

                default: LCC_ASSERT(
                    false,
                    "Unhandled binary expression operator {}",
                    ToString(as<UnaryExpr>(this)->op())
                );
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
                case Type::Kind::Array: return "t_fixarray";
                case Type::Kind::Function: return "t_function";
                case Type::Kind::Enum: return "t_enum";
                case Type::Kind::Struct: return "t_struct";
                case Type::Kind::Integer: return "t_int";
            }
            return type()->string();
        } break;
    }
    LCC_UNREACHABLE();
}

std::vector<lcc::glint::Expr*> lcc::glint::Expr::children() const {
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
        case Kind::Type:
            if (auto t_dynarray = cast<DynamicArrayType>(type())) {
                if (t_dynarray->initial_size())
                    return {t_dynarray->initial_size()};
            }
            if (auto t_fixarray = cast<ArrayType>(type()))
                return {t_fixarray->size()};
            return {};

        case Kind::While: {
            auto w = as<lcc::glint::WhileExpr>(this);
            return {w->condition(), w->body()};
        }

        case Kind::For: {
            auto f = as<lcc::glint::ForExpr>(this);
            return {f->init(), f->condition(), f->increment(), f->body()};
        }

        case Kind::If: {
            auto i = as<lcc::glint::IfExpr>(this);
            if (i->otherwise())
                return {i->condition(), i->then(), i->otherwise()};
            else return {i->condition(), i->then()};
        }

        case Kind::Return: {
            auto ret = as<lcc::glint::ReturnExpr>(this);
            if (ret->value()) return {ret->value()};
            return {};
        }

        case Kind::MemberAccess:
            return {as<lcc::glint::MemberAccessExpr>(this)->object()};

        case Kind::CompoundLiteral:
            return as<lcc::glint::CompoundLiteral>(this)->values();

        case Kind::Cast:
            return {as<lcc::glint::CastExpr>(this)->operand()};

        case Kind::Call: {
            auto c = as<lcc::glint::CallExpr>(this);
            std::vector<lcc::glint::Expr*> children{c->callee()};
            children.insert(children.end(), c->args().begin(), c->args().end());
            return children;
        } break;

        case Kind::Sizeof:
            return {as<lcc::glint::SizeofExpr>(this)->expr()};

        case Kind::Alignof:
            return {as<lcc::glint::AlignofExpr>(this)->expr()};

        case Kind::VarDecl: {
            auto v = as<lcc::glint::VarDecl>(this);
            if (v->init()) return {v->init()};
            return {};
        } break;

        case Kind::NameRef: {
            auto n = as<lcc::glint::NameRefExpr>(this);
            if (n->target())
                return {n->target()};
            return {};
        }

        case Kind::Block: {
            auto b = as<BlockExpr>(this);
            return b->children();
        }

        case Kind::Unary: {
            auto u = as<UnaryExpr>(this);
            return {u->operand()};
        }

        case Kind::Binary: {
            auto b = as<BinaryExpr>(this);
            return {b->lhs(), b->rhs()};
        }
    }
    LCC_UNREACHABLE();
}

auto lcc::glint::EnumeratorDecl::value() const -> aint {
    LCC_ASSERT(ok(), "value() can only be used if the enumerator was analysed successfully");
    return is<ConstantExpr>(init())
             ? as<ConstantExpr>(init())->value().as_int()
             : as<IntegerLiteral>(init())->value();
}

std::string lcc::glint::ToString(lcc::glint::Expr::Kind k) {
    switch (k) {
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
                auto f = as<lcc::glint::FuncDecl>(e);
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
                auto v = as<lcc::glint::VarDecl>(e);
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
                auto v = as<lcc::glint::EnumeratorDecl>(e);
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
                auto b = as<lcc::glint::BinaryExpr>(e);
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
                auto u = as<lcc::glint::UnaryExpr>(e);
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
                auto i = as<lcc::glint::IntegerLiteral>(e);
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
                auto n = as<lcc::glint::NameRefExpr>(e);
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
                auto c = as<lcc::glint::CastExpr>(e);
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

        if (auto n = cast<lcc::glint::NameRefExpr>(e)) {
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
        if (auto f = cast<lcc::glint::FuncDecl>(e)) {
            printed_functions.insert(f);
            if (auto body = const_cast<lcc::glint::FuncDecl*>(f)->body()) {
                if (auto block = cast<lcc::glint::BlockExpr>(body)) {
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
        printed_functions.insert(mod->top_level_func());
        if (auto funcbody = cast<lcc::glint::BlockExpr>(mod->top_level_func()->body())) {
            for (auto* node : funcbody->children())
                PrintTopLevelNode(node);
        } else PrintTopLevelNode(mod->top_level_func()->body());

        for (auto* f : mod->functions())
            if (not printed_functions.contains(f))
                PrintTopLevelNode(f);
    }
};
} // namespace

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
            auto el = elem();
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
            auto el = elem();
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
            auto i = as<IntegerType>(this);
            return fmt::format("{}{}{}{}", C(type_colour), i->is_signed() ? "s" : "u", i->bit_width(), C(Reset));
        }

        case Kind::Struct: {
            auto decl = as<StructType>(this)->decl();
            return fmt::format(
                "{}struct {}{}",
                C(type_colour),
                not decl or decl->name().empty() ? "<anonymous>" : decl->name(),
                C(Reset)
            );
        }

        case Kind::Enum: {
            auto decl = as<EnumType>(this)->decl();
            return fmt::format(
                "{}enum {}{}",
                C(type_colour),
                not decl or decl->name().empty() ? "<anonymous>" : decl->name(),
                C(Reset)
            );
        }

        case Kind::DynamicArray: {
            auto arr = as<DynamicArrayType>(this);
            return fmt::format(
                "{}[{}{}]{}",
                C(type_colour),
                arr->element_type()->string(use_colours),
                C(type_colour),
                C(Reset)
            );
        }

        case Kind::Array: {
            auto arr = as<ArrayType>(this);
            LCC_ASSERT(arr->size(), "ArrayType has NULL size expression");
            if (auto sz = cast<ConstantExpr>(arr->size())) {
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

        case Kind::Function: {
            auto f = as<FuncType>(this);
            std::string out = fmt::format("{}{}(", f->return_type()->string(use_colours), C(ASTPrinter::base_colour));
            for (auto& arg : f->params()) {
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
