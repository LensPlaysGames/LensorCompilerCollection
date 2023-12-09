#include <bit>
#include <intercept/ast.hh>
#include <intercept/module_description.hh>
#include <intercept/parser.hh>
#include <lcc/target.hh>
#include <lcc/utils.hh>
#include <lcc/utils/ast_printer.hh>
#include <lcc/utils/macros.hh>
#include <lcc/utils/rtti.hh>
#include <type_traits>

namespace intc = lcc::intercept;

/// ===========================================================================
///  Module
/// ===========================================================================
intc::Module::Module(
    File* file,
    std::string module_name,
    bool is_logical_module
) : name{std::move(module_name)}, _is_module{is_logical_module}, file{file} {
    FuncType* ty{};

    /// Create the type of the top-level function.
    if (is_logical_module) {
        ty = new (*this) FuncType({}, BuiltinType::Void(*this), {}, {});
    } else {
        auto cchar_ty = FFIType::CChar(*this);
        auto cint_ty = FFIType::CInt(*this);
        auto char_ptr = new (*this) PointerType{new (*this) PointerType{cchar_ty}};
        ty = new (*this) FuncType{
            {
                {"__argc__", cint_ty, {}},
                {"__argv__", char_ptr, {}},
                {"__envp__", char_ptr, {}},
            },

            /// We currently set main() to return `int` since that’s natural for
            /// most programs. In the future, our runtime should define main() and
            /// this function should be something that’s called by it.
            Type::Int,
            {},
            {},
        };
    }

    /// FIXME: What name are we using for module initialisers again?
    top_level_function = new (*this) FuncDecl{
        is_logical_module ? fmt::format(".init.{}", name) : "main",
        ty,
        new (*this) BlockExpr{{}, {}},
        nullptr,
        this,
        Linkage::Exported,
        {},
        CallConv::C,
    };
}

intc::Module::~Module() {
    for (auto* node : nodes) delete node;
    for (auto* type : types) delete type;
    for (auto* scope : scopes) delete scope;
    for (auto& [_, i] : _imports) delete i;
}

void lcc::intercept::Module::add_top_level_expr(Expr* node) {
    as<BlockExpr>(top_level_function->body())->add(node);
}

auto intc::Module::intern(std::string_view str) -> usz {
    auto it = rgs::find(strings, str);
    if (it != strings.end()) { return usz(it - strings.begin()); }
    strings.emplace_back(str);
    return strings.size() - 1;
}

/// ===========================================================================
///  AST
/// ===========================================================================
intc::StringLiteral::StringLiteral(
    Module& mod,
    std::string_view value,
    Location location
) : TypedExpr{
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
    _index{mod.intern(value)} {
}

/// Declare a symbol in this scope.
auto intc::Scope::declare(
    const Context* ctx,
    std::string&& name,
    Decl* decl
) -> Result<Decl*> {
    /// If the symbol already exists, then this is an error, unless
    /// that symbol is a function declaration, and this is also a
    /// function declaration.
    if (
        auto it = symbols.find(name);
        it != symbols.end() and
        not is<FuncDecl>(it->second) and
        not is<FuncDecl>(decl)
    ) return Diag::Error(ctx, decl->location(), "Redeclaration of '{}'", name);

    /// TODO: Check that this declaration is hygienic if it’s part of a macro.

    /// Otherwise, add the symbol.
    symbols.emplace(std::move(name), decl);
    return decl;
}

auto intc::Expr::type() const -> Type* {
    if (auto e = cast<TypedExpr>(this)) return e->type();
    return Type::Void;
}

auto intc::Type::align(const lcc::Context* ctx) const -> usz {
    LCC_ASSERT(sema_done_or_errored());
    if (sema_errored()) return 1;
    switch (kind()) {
        case Kind::Builtin:
            switch (as<BuiltinType>(this)->builtin_kind()) {
                using K = BuiltinType::BuiltinKind;
                case K::Bool: return ctx->target()->intercept.align_of_bool;
                case K::Byte: return ctx->target()->intercept.align_of_byte;
                case K::Int: return ctx->target()->intercept.align_of_int;

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

        case Kind::Array: return elem()->align(ctx);
        case Kind::Struct: return as<StructType>(this)->alignment();
        case Kind::Integer: return std::bit_ceil(as<IntegerType>(this)->bit_width());
    }

    LCC_UNREACHABLE();
}

auto intc::Type::elem() const -> Type* {
    switch (kind()) {
        case Kind::Pointer: return as<PointerType>(this)->element_type();
        case Kind::Reference: return as<ReferenceType>(this)->element_type();
        case Kind::Array: return as<ArrayType>(this)->element_type();

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
bool is_builtin(const intc::Type* t, intc::BuiltinType::BuiltinKind k) {
    if (auto b = lcc::cast<intc::BuiltinType>(t)) return b->builtin_kind() == k;
    return false;
}
} // namespace

bool intc::Type::is_bool() const { return ::is_builtin(this, BuiltinType::BuiltinKind::Bool); }
bool intc::Type::is_byte() const { return ::is_builtin(this, BuiltinType::BuiltinKind::Byte); }

bool intc::Type::is_integer(bool include_bool) const {
    return is<IntegerType, FFIType>(this) or
           ::is_builtin(this, BuiltinType::BuiltinKind::Int) or
           is_byte() or
           (include_bool and is_bool());
}

bool intc::Type::is_signed_int(const Context* ctx) const {
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

bool intc::Type::is_unknown() const { return ::is_builtin(this, BuiltinType::BuiltinKind::Unknown); }

bool intc::Type::is_unsigned_int(const Context* ctx) const {
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

bool intc::Type::is_void() const { return ::is_builtin(this, BuiltinType::BuiltinKind::Void); }

auto intc::Type::size(const lcc::Context* ctx) const -> usz {
    LCC_ASSERT(sema_done_or_errored());
    if (sema_errored()) return 0;
    switch (kind()) {
        case Kind::Builtin:
            switch (as<BuiltinType>(this)->builtin_kind()) {
                using K = BuiltinType::BuiltinKind;
                case K::Bool: return ctx->target()->intercept.size_of_bool;
                case K::Byte: return ctx->target()->intercept.size_of_byte;
                case K::Int: return ctx->target()->intercept.size_of_int;

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

        case Kind::Array:
            return as<ArrayType>(this)->dimension() * elem()->size(ctx);

        case Kind::Struct: return as<StructType>(this)->byte_size() * 8;
        case Kind::Integer: return as<IntegerType>(this)->bit_width();
    }

    LCC_UNREACHABLE();
}

auto intc::Type::strip_pointers_and_references() -> Type* {
    auto ty = strip_references();
    while (is<PointerType>(ty)) ty = ty->elem();
    return ty;
}

auto intc::Type::strip_references() -> Type* {
    auto ty = this;
    if (is<ReferenceType>(ty)) ty = ty->elem();
    LCC_ASSERT(not is<ReferenceType>(ty), "Double references are not permitted");
    return ty;
}

bool intc::Type::Equal(const Type* a, const Type* b) {
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

auto intc::ArrayType::dimension() const -> usz {
    LCC_ASSERT(ok(), "Can only call dimension() if type has been type checked successfully");
    return usz(as<ConstantExpr>(size())->value().as_int());
}

auto intc::CallExpr::callee_type() const -> FuncType* {
    auto ty = callee()->type();
    while (is<PointerType, ReferenceType>(ty)) ty = ty->elem();
    LCC_ASSERT(ty->is_function());
    return as<FuncType>(ty);
}

auto intc::Expr::Clone(Module& mod, Expr* expr) -> Expr* {
    LCC_ASSERT(false, "TODO: Clone expressions");
}

auto intc::EnumeratorDecl::value() const -> aint {
    LCC_ASSERT(ok(), "value() can only be used if the enumerator was analysed successfully");
    return is<ConstantExpr>(init())
             ? as<ConstantExpr>(init())->value().as_int()
             : as<IntegerLiteral>(init())->value();
}

/// ===========================================================================
///  AST Printing
/// ===========================================================================
namespace {
using lcc::as;
using lcc::cast;
using lcc::is;

struct ASTPrinter : lcc::utils::ASTPrinter<ASTPrinter, intc::Expr, intc::Type> {
    std::unordered_set<const intc::FuncDecl*> printed_functions{};
    bool print_children_of_children = true;

    void PrintLValue(const intc::Expr* e) {
        if (e->is_lvalue()) out += fmt::format(" {}lvalue", C(Blue));
        out += '\n';
    };

    void PrintBasicInterceptNode(std::string_view name, const intc::Expr* node, intc::Type* t) {
        PrintBasicNode(name, node, t, false);
        PrintLValue(node);
    };

    /// Print the header (name + location + type) of a node.
    void PrintHeader(const intc::Expr* e) {
        using K = intc::Expr::Kind;
        switch (e->kind()) {
            case K::FuncDecl: {
                auto f = as<intc::FuncDecl>(e);
                PrintLinkage(f->linkage());
                PrintBasicHeader("FuncDecl", e);
                out += fmt::format(
                    " {}{} {}\n",
                    C(Green),
                    f->name(),
                    f->type()->string(use_colour)
                );
                return;
            }

            case K::VarDecl: {
                auto v = as<intc::VarDecl>(e);
                PrintLinkage(v->linkage());
                PrintBasicHeader("VarDecl", e);
                out += fmt::format(
                    " {}{} {} {}lvalue\n",
                    C(White),
                    v->name(),
                    v->type()->string(use_colour),
                    C(Blue)
                );
                return;
            }

            case K::EnumeratorDecl: {
                auto v = as<intc::EnumeratorDecl>(e);
                PrintBasicHeader("EnumeratorDecl", e);
                out += fmt::format(
                    " {}{} {}{}\n",
                    C(Blue),
                    v->name(),
                    C(Magenta),
                    v->ok() ? v->value().str() : "?"
                );
                return;
            }
            case K::Binary: {
                auto b = as<intc::BinaryExpr>(e);
                PrintBasicHeader("BinaryExpr", e);
                out += fmt::format(
                    " {}{} {}",
                    C(Red),
                    intc::ToString(b->op()),
                    b->type()->string(use_colour)
                );
                PrintLValue(e);
                return;
            }

            case K::Unary: {
                auto u = as<intc::UnaryExpr>(e);
                PrintBasicHeader("UnaryExpr", e);
                out += fmt::format(
                    " {}{} {}",
                    C(Red),
                    intc::ToString(u->op()),
                    u->type()->string(use_colour)
                );
                PrintLValue(e);
                return;
            }

            case K::IntegerLiteral: {
                auto i = as<intc::IntegerLiteral>(e);
                PrintBasicHeader("IntegerLiteral", e);
                out += fmt::format(
                    " {}{} {}\n",
                    C(Magenta),
                    i->value(),
                    i->type()->string(use_colour)
                );
                return;
            }

            case K::NameRef: {
                auto n = as<intc::NameRefExpr>(e);
                PrintBasicHeader("NameRefExpr", e);
                out += fmt::format(
                    " {}{} {}",
                    C(White),
                    n->name(),
                    n->type()->string(use_colour)
                );
                PrintLValue(e);
                return;
            }

            case K::Cast: {
                auto c = as<intc::CastExpr>(e);
                PrintBasicHeader("CastExpr", e);
                switch (c->cast_kind()) {
                    case intc::CastKind::SoftCast: out += fmt::format(" {}as ", C(Red)); break;
                    case intc::CastKind::HardCast: out += fmt::format(" {}as! ", C(Red)); break;
                    case intc::CastKind::ImplicitCast: out += fmt::format(" {}Implicit ", C(Red)); break;
                    case intc::CastKind::LValueToRValueConv: out += fmt::format(" {}LValueToRValue ", C(Red)); break;
                    case intc::CastKind::LValueToReference: out += fmt::format(" {}LValueToReference ", C(Red)); break;
                    case intc::CastKind::ReferenceToLValue: out += fmt::format(" {}ReferenceToLValue ", C(Red)); break;
                }
                out += e->type()->string(use_colour);
                PrintLValue(e);
                return;
            }

            case K::If: {
                PrintBasicHeader("IfExpr", e);
                if (not e->type()->is_void()) out += fmt::format(" {}", e->type()->string(use_colour));
                PrintLValue(e);
                return;
            }

            case K::OverloadSet: PrintBasicInterceptNode("OverloadSet", e, e->type()); return;
            case K::EvaluatedConstant: PrintBasicInterceptNode("ConstantExpr", e, e->type()); return;
            case K::TypeDecl: PrintBasicInterceptNode("TypeDecl", e, e->type()); return;
            case K::TypeAliasDecl: PrintBasicInterceptNode("TypeAliasDecl", e, e->type()); return;
            case K::StringLiteral: PrintBasicInterceptNode("StringLiteral", e, e->type()); return;
            case K::CompoundLiteral: PrintBasicInterceptNode("CompoundLiteral", e, e->type()); return;
            case K::While: PrintBasicInterceptNode("WhileExpr", e, nullptr); return;
            case K::For: PrintBasicInterceptNode("ForExpr", e, nullptr); return;
            case K::Block: PrintBasicInterceptNode("BlockExpr", e, e->type()); return;
            case K::Return: PrintBasicInterceptNode("ReturnExpr", e, nullptr); return;
            case K::Call: PrintBasicInterceptNode("CallExpr", e, e->type()); return;
            case K::IntrinsicCall: PrintBasicInterceptNode("IntrinsicCallExpr", e, e->type()); return;
            case K::MemberAccess: PrintBasicInterceptNode("MemberAccessExpr", e, e->type()); return;
        }

        PrintBasicInterceptNode(R"(<???>)", e, e->type());
    }

    void PrintNodeChildren(const intc::Expr* e, std::string leading_text = "") {
        if (not print_children_of_children) return;

        /// Print the children of a node.
        using K = intc::Expr::Kind;
        switch (e->kind()) {
            /// We only print function bodies at the top level.
            case K::FuncDecl: break;

            case K::Binary: {
                auto b = as<intc::BinaryExpr>(e);
                intc::Expr* children[] = {b->lhs(), b->rhs()};
                PrintChildren(children, leading_text);
            } break;

            case K::NameRef: {
                auto n = as<intc::NameRefExpr>(e);
                if (n->target()) {
                    tempset print_children_of_children = false;
                    intc::Expr* children[] = {n->target()};
                    PrintChildren(children, leading_text);
                }
            } break;

            case K::VarDecl: {
                auto v = as<intc::VarDecl>(e);
                if (v->init()) {
                    intc::Expr* children[] = {v->init()};
                    PrintChildren(children, leading_text);
                }
            } break;

            case K::Unary: {
                auto u = as<intc::UnaryExpr>(e);
                intc::Expr* children[] = {u->operand()};
                PrintChildren(children, leading_text);
            } break;

            case K::Call: {
                auto c = as<intc::CallExpr>(e);
                std::vector<intc::Expr*> children{c->callee()};
                children.insert(children.end(), c->args().begin(), c->args().end());
                PrintChildren(children, leading_text);
            } break;

            case K::Cast: {
                intc::Expr* children[] = {as<intc::CastExpr>(e)->operand()};
                PrintChildren(children, leading_text);
            } break;

            case K::CompoundLiteral:
                PrintChildren(as<intc::CompoundLiteral>(e)->values(), leading_text);
                break;

            case K::While: {
                auto w = as<intc::WhileExpr>(e);
                intc::Expr* children[] = {w->condition(), w->body()};
                PrintChildren(children, leading_text);
            } break;

            case K::For: {
                auto f = as<intc::ForExpr>(e);
                intc::Expr* children[] = {f->init(), f->condition(), f->increment(), f->body()};
                PrintChildren(children, leading_text);
            } break;

            case K::If: {
                auto i = as<intc::IfExpr>(e);
                if (i->else_()) {
                    intc::Expr* children[] = {i->condition(), i->then(), i->else_()};
                    PrintChildren(children, leading_text);
                } else {
                    intc::Expr* children[] = {i->condition(), i->then()};
                    PrintChildren(children, leading_text);
                }
            } break;

            case K::Block:
                PrintChildren(as<intc::BlockExpr>(e)->children(), leading_text);
                break;

            case K::Return: {
                auto ret = as<intc::ReturnExpr>(e);
                if (ret->value()) PrintChildren({ret->value()}, leading_text);
            } break;

            case K::OverloadSet:
            case K::EvaluatedConstant:
            case K::TypeDecl:
            case K::TypeAliasDecl:
            case K::EnumeratorDecl:
            case K::IntegerLiteral:
            case K::StringLiteral:
            case K::IntrinsicCall:
            case K::MemberAccess:
                break;
        }
    }

    /// Print a top-level node.
    void PrintTopLevelNode(const intc::Expr* e) {
        PrintHeader(e);
        if (auto f = cast<intc::FuncDecl>(e)) {
            printed_functions.insert(f);
            if (auto body = const_cast<intc::FuncDecl*>(f)->body()) {
                if (auto block = cast<intc::BlockExpr>(body)) {
                    PrintChildren(block->children(), "");
                } else {
                    intc::Expr* children[] = {const_cast<intc::FuncDecl*>(f)->body()};
                    PrintChildren(children, "");
                }
            }
        } else {
            PrintNodeChildren(e);
        }
    }

    /// Print a node.
    void operator()(const intc::Expr* e, std::string leading_text = "") {
        PrintHeader(e);
        PrintNodeChildren(e, std::move(leading_text));
    }

    void print(intc::Module* mod) {
        printed_functions.insert(mod->top_level_func());
        for (auto* node : as<intc::BlockExpr>(mod->top_level_func()->body())->children())
            PrintTopLevelNode(node);

        for (auto* f : mod->functions())
            if (not printed_functions.contains(f))
                PrintTopLevelNode(f);
    }
};
} // namespace

auto intc::Type::string(bool use_colours) const -> std::string {
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
                "{}@{}{}{}{}{}{}",
                C(Red),
                has_arr_or_func ? "(" : "",
                C(Cyan),
                as<PointerType>(this)->element_type()->string(use_colours),
                C(Red),
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
                "{}&{}{}{}{}{}{}",
                C(Red),
                has_func ? "(" : "",
                C(Cyan),
                as<ReferenceType>(this)->element_type()->string(use_colours),
                C(Red),
                has_func ? ")" : "",
                C(Reset)
            );
        }

        case Kind::Integer: {
            auto i = as<IntegerType>(this);
            return fmt::format("{}{}{}{}", C(Cyan), i->is_signed() ? "i" : "u", i->bit_width(), C(Reset));
        }

        case Kind::Struct: {
            auto decl = as<StructType>(this)->decl();
            return fmt::format(
                "{}struct {}{}{}",
                C(Red),
                C(Cyan),
                not decl or decl->name().empty() ? "<anonymous>" : decl->name(),
                C(Reset)
            );
        }

        case Kind::Enum: {
            auto decl = as<EnumType>(this)->decl();
            return fmt::format(
                "{}enum {}{}{}",
                C(Red),
                C(Cyan),
                not decl or decl->name().empty() ? "<anonymous>" : decl->name(),
                C(Reset)
            );
        }

        case Kind::Array: {
            auto arr = as<ArrayType>(this);
            if (auto sz = cast<ConstantExpr>(arr->size())) {
                return fmt::format(
                    "{}{}[{}{}{}]{}",
                    arr->element_type()->string(use_colours),
                    C(Red),
                    C(Magenta),
                    sz->value().as_int(),
                    C(Red),
                    C(Reset)
                );
            } else {
                return fmt::format(
                    "{}{}[{}?{}]{}",
                    arr->element_type()->string(use_colours),
                    C(Red),
                    C(Magenta),
                    C(Red),
                    C(Reset)
                );
            }
        }

        case Kind::Builtin:
            switch (as<BuiltinType>(this)->builtin_kind()) {
                using K = BuiltinType::BuiltinKind;
                case K::Bool: return fmt::format("{}bool{}", C(Cyan), C(Reset));
                case K::Byte: return fmt::format("{}byte{}", C(Cyan), C(Reset));
                case K::Int: return fmt::format("{}int{}", C(Cyan), C(Reset));
                case K::Unknown: return fmt::format("{}<?>{}", C(Cyan), C(Reset));
                case K::Void: return fmt::format("{}void{}", C(Cyan), C(Reset));
                case K::OverloadSet: return fmt::format("{}<overload set>{}", C(Cyan), C(Reset));
            }
            LCC_UNREACHABLE();

        case Kind::FFIType:
            switch (as<FFIType>(this)->ffi_kind()) {
                using K = FFIType::FFIKind;
                case K::CChar: return fmt::format("__c_char{}", C(Cyan), C(Reset));
                case K::CSChar: return fmt::format("__c_schar{}", C(Cyan), C(Reset));
                case K::CUChar: return fmt::format("__c_uchar{}", C(Cyan), C(Reset));
                case K::CShort: return fmt::format("__c_short{}", C(Cyan), C(Reset));
                case K::CUShort: return fmt::format("__c_ushort{}", C(Cyan), C(Reset));
                case K::CInt: return fmt::format("__c_int{}", C(Cyan), C(Reset));
                case K::CUInt: return fmt::format("__c_uint{}", C(Cyan), C(Reset));
                case K::CLong: return fmt::format("__c_long{}", C(Cyan), C(Reset));
                case K::CULong: return fmt::format("__c_ulong{}", C(Cyan), C(Reset));
                case K::CLongLong: return fmt::format("__c_longlong{}", C(Cyan), C(Reset));
                case K::CULongLong: return fmt::format("__c_ulonglong{}", C(Cyan), C(Reset));
            }
            LCC_UNREACHABLE();

        case Kind::Function: {
            auto f = as<FuncType>(this);
            std::string out = fmt::format("{}{}(", f->return_type()->string(use_colours), C(Red));
            for (auto& arg : f->params()) {
                if (&arg != &f->params().front()) out += fmt::format("{}, ", C(Red));
                out += fmt::format("{}{}{}", C(Blue), arg.name, C(Red));
                if (not arg.name.empty()) out += " : ";
                else out += ":";
                out += arg.type->string(use_colours);
            }
            out += fmt::format("{}){}", C(Red), C(Reset));
            return out;
        }
    }

    LCC_UNREACHABLE();
}

void intc::Module::print(bool use_colour) {
    ASTPrinter{use_colour}.print(this);
}


// FIXME: This should probably be backwards for big endian machines, afaik.
template <typename T>
std::array<lcc::u8, sizeof(T) / sizeof(lcc::u8)> to_bytes(const T object) {
    std::array<lcc::u8, sizeof(T)> out{};
    const lcc::u8* begin = reinterpret_cast<const lcc::u8*>(&object);
    const lcc::u8* end = begin + (sizeof(T));
    std::copy(begin, end, out.begin());
    return out;
}

// FIXME: This should probably be backwards for big endian machines, afaik.
// requires std::is_trivially_constructible?
template <typename T>
T from_bytes(std::array<lcc::u8, sizeof(T)> bytes) {
    T out{};
    std::copy(bytes.begin(), bytes.end(), &out);
    return out;
}

lcc::u16 intc::Module::serialise(std::vector<u8>& out, std::vector<Type*>& cache, Type* ty) {
    auto found = rgs::find(cache, ty);
    if (found != cache.end())
        return u16(found - cache.begin());

    LCC_ASSERT(
        cache.size() < 0xffff,
        "Too many types, cannot serialise in binary metadata format version 1"
    );
    auto type_index = ModuleDescription::TypeIndex(cache.size());
    cache.push_back(ty);

    u8 tag = u8(ty->kind());
    out.push_back(tag);

    switch (ty->kind()) {
        // NamedType: length :u32, name :u8[length]
        case Type::Kind::Named: {
            NamedType* type = as<NamedType>(ty);
            u16 name_length = u16(type->name().length());
            auto name_length_bytes = to_bytes(name_length);
            // Write `length: u16`
            out.insert(out.end(), name_length_bytes.begin(), name_length_bytes.end());
            // Write `name : u8[length]`
            out.insert(out.end(), type->name().begin(), type->name().end());
        } break;

        // PointerType, ReferenceType: type_index :TypeIndex
        case Type::Kind::Pointer:
        case Type::Kind::Reference: {
            auto referenced_type_index = ModuleDescription::TypeIndex(-1);
            auto type_index_bytes = to_bytes(referenced_type_index);

            // Write `type_index: u16`, keeping track of byte index into binary
            // metadata blob where it can be found again.
            u32 referenced_type_index_offset = u32(out.size());
            out.insert(out.end(), type_index_bytes.begin(), type_index_bytes.end());

            // Serialise the referenced type.
            referenced_type_index = serialise(out, cache, ty->elem());

            // Go back and fixup the referenced type index from -1 to the actual
            // proper value.
            // FIXME: Is it possible to /not/ do this reinterpret cast? I guess we
            // could manually fuddle with bytes given the index since we know
            // endianness and size. Yeah, that's probably what we should do.
            auto* referenced_type_index_ptr = reinterpret_cast<ModuleDescription::TypeIndex*>(out.data() + referenced_type_index_offset);
            *referenced_type_index_ptr = referenced_type_index;
        } break;

        // IntegerType: bitwidth :u16, is_signed :u8
        case Type::Kind::Integer: {
            IntegerType* type = as<IntegerType>(ty);
            LCC_ASSERT(
                type->bit_width() <= 0xffff,
                "Cannot encode over-large bitwidth of integer type {}",
                *ty
            );

            u16 bitwidth = u16(type->bit_width());
            auto bitwidth_bytes = to_bytes(bitwidth);

            u8 is_signed = type->is_signed() ? 1 : 0;

            out.insert(out.end(), bitwidth_bytes.begin(), bitwidth_bytes.end());
            out.push_back(is_signed);
        } break;

        // BuiltinType: builtin_kind :u8
        case Type::Kind::Builtin: {
            BuiltinType* type = as<BuiltinType>(ty);
            LCC_ASSERT(
                type->builtin_kind() != BuiltinType::BuiltinKind::OverloadSet,
                "Cannot serialise overload sets; sorry"
            );
            u8 builtin_kind = u8(type->builtin_kind());
            out.push_back(builtin_kind);
        } break;

        // FFIType: ffi_kind :u16
        case Type::Kind::FFIType: {
            FFIType* type = as<FFIType>(ty);
            u16 ffi_kind = u16(type->ffi_kind());
            auto ffi_kind_bytes = to_bytes(ffi_kind);
            out.insert(out.end(), ffi_kind_bytes.begin(), ffi_kind_bytes.end());
        } break;

        // FunctionType:
        //     attributes :u32
        //     param_count :u16
        //     param_types :u16[param_count]
        //     return_type :u16
        //     param_names :(param_name_length :u16, param_name :u8[param_name_length])[param_count]
        case Type::Kind::Function: {
            FuncType* type = as<FuncType>(ty);
            u32 attributes{};
            const auto set_attr_bit = [&](FuncAttr f) {
                if (type->has_attr(f)) {
                    static constexpr auto one = decltype(attributes)(1);
                    attributes |= one << u32(f);
                }
            };
            // Probably don't need all of these. If we run out of bits, we could
            // remove some that aren't needed.
            set_attr_bit(FuncAttr::Const);
            set_attr_bit(FuncAttr::Discardable);
            set_attr_bit(FuncAttr::Flatten);
            set_attr_bit(FuncAttr::Inline);
            set_attr_bit(FuncAttr::NoInline);
            set_attr_bit(FuncAttr::NoMangle);
            set_attr_bit(FuncAttr::NoOpt);
            set_attr_bit(FuncAttr::NoReturn);
            set_attr_bit(FuncAttr::Pure);
            set_attr_bit(FuncAttr::ReturnsTwice);
            set_attr_bit(FuncAttr::Used);

            u16 param_count = u16(type->params().size());

            auto attributes_bytes = to_bytes(attributes);
            out.insert(out.end(), attributes_bytes.begin(), attributes_bytes.end());

            auto param_count_bytes = to_bytes(param_count);
            out.insert(out.end(), param_count_bytes.begin(), param_count_bytes.end());

            // Allocate enough room to represent parameter types, once we are able to
            // serialise them, keeping track of where they are so we can fix them up
            // later.
            auto param_types_offset = out.size();
            out.insert(out.end(), param_count * sizeof(ModuleDescription::TypeIndex), 0);

            auto return_type_offset = out.size();
            out.insert(out.end(), sizeof(ModuleDescription::TypeIndex), 0);

            // Write parameter names
            for (auto& param : type->params()) {
                auto name_length = u16(param.name.length());
                auto name_length_bytes = to_bytes(name_length);
                out.insert(out.end(), name_length_bytes.begin(), name_length_bytes.end());
                out.insert(out.end(), param.name.begin(), param.name.end());
            }

            // Serialise parameter types and fixup parameter type indices previously
            // allocated.
            auto* param_types_ptr = reinterpret_cast<ModuleDescription::TypeIndex*>(out.data() + param_types_offset);
            for (auto [index, param] : vws::enumerate(type->params())) {
                *param_types_ptr++ = serialise(out, cache, param.type);
            }

            // Serialise return type and fixup return type index previously allocated.
            auto* return_type_ptr = reinterpret_cast<ModuleDescription::TypeIndex*>(out.data() + return_type_offset);
            *return_type_ptr = serialise(out, cache, type->return_type());
        } break;

        // EnumType:
        //     underlying_type_index :u16
        //     enum_decl_count :u32
        //     enum_decls :u64[enum_decl_count]
        // FIXME: Ideally we should get the size of the value (enum_decls element
        // type) from the underlying type, instead of just assuming it fits in 64
        // bits.
        case Type::Kind::Enum: {
            auto type = as<EnumType>(ty);

            auto underlying_type_offset = out.size();
            out.insert(out.end(), sizeof(ModuleDescription::TypeIndex), 0);

            // TODO: Assert that underlying type can fit in 64 bits. But we don't have
            // context here (yet).

            u32 enum_decl_count = u32(type->enumerators().size());
            auto enum_decl_count_bytes = to_bytes(enum_decl_count);
            out.insert(out.end(), enum_decl_count_bytes.begin(), enum_decl_count_bytes.end());

            for (auto* enum_decl : type->enumerators()) {
                u64 value = enum_decl->value().value();
                auto value_bytes = to_bytes(value);
                out.insert(out.end(), value_bytes.begin(), value_bytes.end());
            }

            auto* underlying_type_ptr = reinterpret_cast<ModuleDescription::TypeIndex*>(out.data() + underlying_type_offset);
            *underlying_type_ptr = serialise(out, cache, type->underlying_type());

        } break;

        // ArrayType: element_type_index :u16, element_count :u64
        case Type::Kind::Array: {
            auto type = as<ArrayType>(ty);

            auto element_type_offset = out.size();
            out.insert(out.end(), sizeof(ModuleDescription::TypeIndex), 0);

            u64 element_count = u64(type->dimension());
            auto element_count_bytes = to_bytes(element_count);
            out.insert(out.end(), element_count_bytes.begin(), element_count_bytes.end());

            auto* element_type_ptr = reinterpret_cast<ModuleDescription::TypeIndex*>(out.data() + element_type_offset);
            *element_type_ptr = serialise(out, cache, type->element_type());
        } break;

        case Type::Kind::Struct: // will be a lot like function
            LCC_TODO("Handle serialisation of type {}", *ty);
    }

    return type_index;
}

std::vector<lcc::u8> intc::Module::serialise() {
    ModuleDescription::Header hdr{};
    std::vector<u8> declarations{};
    std::vector<u8> types{};
    std::vector<u8> serialised_name{};

    // Decls and types collected from exports.
    std::vector<Type*> type_cache{};
    for (auto* decl : exports) {
        // Decl: DeclHeader, length :u8, name :u8[length]

        // Prepare declaration header
        ModuleDescription::TypeIndex type_index = serialise(types, type_cache, decl->type());
        ModuleDescription::DeclarationHeader decl_hdr{
            u16(ModuleDescription::DeclarationHeader::get_kind(decl)),
            type_index};
        auto decl_hdr_bytes = to_bytes(decl_hdr);

        // Prepare length
        LCC_ASSERT(
            decl->name().length() <= 0xff,
            "Exported declaration has over-long name and cannot be encoded in binary format"
        );
        u8 length = u8(decl->name().length());

        declarations.insert(declarations.end(), decl_hdr_bytes.begin(), decl_hdr_bytes.end());
        declarations.push_back(length);
        declarations.insert(declarations.end(), decl->name().begin(), decl->name().end());
    }

    // Make name easily serialisable
    LCC_ASSERT(name.size(), "Cannot serialise unnamed module");
    // TODO: Error if there is a NULL in the module name. That would surely bugger things.
    serialised_name.insert(serialised_name.end(), name.begin(), name.end());
    serialised_name.push_back('\0');

    // Final header fixups now that nothing will change.
    hdr.size = u32(sizeof(ModuleDescription::Header) + declarations.size() + types.size() + serialised_name.size());
    hdr.type_table_offset = u32(sizeof(ModuleDescription::Header) + declarations.size());
    hdr.name_offset = u32(sizeof(ModuleDescription::Header) + declarations.size() + types.size());
    hdr.declaration_count = u16(exports.size());
    hdr.type_count = u16(type_cache.size());

    // Convert header to byte representation that is easy to serialise.
    auto hdr_bytes = to_bytes(hdr);

    std::vector<u8> out{};
    out.insert(out.end(), hdr_bytes.begin(), hdr_bytes.end());
    out.insert(out.end(), declarations.begin(), declarations.end());
    out.insert(out.end(), types.begin(), types.end());
    out.insert(out.end(), serialised_name.begin(), serialised_name.end());
    return out;
}

bool intc::Module::deserialise(lcc::Context* ctx, std::vector<u8> module_metadata_blob) {
    // We need at least enough bytes for a header, for a zero-exports module
    // (if that is even allowed past sema).
    if (module_metadata_blob.size() < sizeof(ModuleDescription::Header))
        return false;

    // Copy the header from the binary metadata blob.
    ModuleDescription::Header hdr{};
    std::memcpy(&hdr, module_metadata_blob.data(), sizeof(ModuleDescription::Header));

    // Verify header has expected values.
    if (hdr.version != 1) {
        fmt::print("ERROR: Could not deserialise: Invalid version {} in header\n", hdr.version);
        return false;
    }
    if (hdr.magic[0] != ModuleDescription::magic_byte0 or hdr.magic[1] != ModuleDescription::magic_byte1 or hdr.magic[2] != ModuleDescription::magic_byte2) {
        fmt::print("ERROR: Could not deserialise: Invalid magic bytes in header: {} {} {}\n", hdr.magic[0], hdr.magic[1], hdr.magic[2]);
        return false;
    }

    // Starting at the type table offset, parse all types. Stop after parsing
    // the amount of types specified in the header.
    auto type_count = hdr.type_count;
    auto type_offset = hdr.type_table_offset;
    auto types_zero_index = types.size();
    types.reserve(types.size() + type_count);

    // NOTE: Big issue here is forward references. i.e. a pointer type at
    // index 3 that stores a pointee type index of 4. So, we need to know the
    // Type* of any given index /before/ we start parsing any of the types.
    // This is sort of difficult in C++ with stupid fucking inheritance.
    struct BasicFixup {
        // Type index of the type that needs fixing up.
        ModuleDescription::TypeIndex fixup_type_index;
    };
    struct PointerFixup : BasicFixup {
        ModuleDescription::TypeIndex pointee_type_index;

        PointerFixup(
            ModuleDescription::TypeIndex fixup_type,
            ModuleDescription::TypeIndex pointee_type
        ) : BasicFixup(fixup_type),
            pointee_type_index(pointee_type) {}
    };
    struct ReferenceFixup : BasicFixup {
        ModuleDescription::TypeIndex referent_type_index;

        ReferenceFixup(
            ModuleDescription::TypeIndex fixup_type,
            ModuleDescription::TypeIndex referent_type
        ) : BasicFixup(fixup_type),
            referent_type_index(referent_type) {}
    };
    std::vector<PointerFixup> ptr_fixups{};
    std::vector<ReferenceFixup> ref_fixups{};
    for (decltype(type_count) type_index = 0; type_index < type_count; ++type_index) {
        auto tag = module_metadata_blob.at(type_offset++);
        auto kind = Type::Kind(tag);
        switch (kind) {
            // NamedType: length :u32, name :u8[length]
            case Type::Kind::Named: {
                static constexpr auto length_size = sizeof(u32);
                std::array<u8, length_size> length_array{};
                for (unsigned i = 0; i < length_size; ++i)
                    length_array[i] = module_metadata_blob.at(type_offset++);
                u32 length = from_bytes<u32>(length_array);

                LCC_ASSERT(name.size(), "Deserialised named type has zero-length name");

                std::string name{};
                for (u32 i = 0; i < length; ++i)
                    name += char(module_metadata_blob.at(type_offset++));

                LCC_ASSERT(name.size(), "Deserialised named type has empty name");

                // FIXME: This may need to be top level scope, not entirely sure the
                // semantics of this yet.
                new (*this) NamedType(name, global_scope(), {});
            } break;

            // BuiltinType: builtin_kind :u8
            case Type::Kind::Builtin: {
                auto builtin_kind_value = module_metadata_blob.at(type_offset++);
                // clang-format off
                LCC_ASSERT(
                    builtin_kind_value == +BuiltinType::BuiltinKind::Bool
                    or builtin_kind_value == +BuiltinType::BuiltinKind::Byte
                    or builtin_kind_value == +BuiltinType::BuiltinKind::Int
                    or builtin_kind_value == +BuiltinType::BuiltinKind::Unknown
                    or builtin_kind_value == +BuiltinType::BuiltinKind::Void
                    or builtin_kind_value == +BuiltinType::BuiltinKind::OverloadSet,
                    "Invalid builtin kind value {}", builtin_kind_value
                );
                // clang-format on
                LCC_ASSERT(
                    builtin_kind_value != +BuiltinType::BuiltinKind::OverloadSet,
                    "Cannot deserialise overload sets; sorry"
                );
                auto builtin_kind = BuiltinType::BuiltinKind(builtin_kind_value);
                BuiltinType::Make(*this, builtin_kind, {});
            } break;

            // FFIType: ffi_kind :u16
            case Type::Kind::FFIType: {
                static constexpr auto ffi_kind_size = sizeof(u16);
                std::array<u8, ffi_kind_size> length_array{};
                for (unsigned i = 0; i < ffi_kind_size; ++i)
                    length_array[i] = module_metadata_blob.at(type_offset++);
                u16 ffi_kind_value = from_bytes<u16>(length_array);
                auto ffi_kind = FFIType::FFIKind(ffi_kind_value);
                FFIType::Make(*this, ffi_kind, {});
            } break;

            // PointerType, ReferenceType: type_index :TypeIndex
            case Type::Kind::Pointer:
            case Type::Kind::Reference: {
                static constexpr auto ref_type_index_size = sizeof(ModuleDescription::TypeIndex);
                std::array<u8, ref_type_index_size> ref_type_index_array{};
                for (unsigned i = 0; i < ref_type_index_size; ++i)
                    ref_type_index_array[i] = module_metadata_blob.at(type_offset++);
                auto ref_type_index = from_bytes<ModuleDescription::TypeIndex>(ref_type_index_array);

                types.push_back(nullptr);
                if (kind == Type::Kind::Pointer)
                    ptr_fixups.push_back({type_index, ref_type_index});
                else if (kind == Type::Kind::Reference)
                    ref_fixups.push_back({type_index, ref_type_index});
            } break;

            case Type::Kind::Array:
            case Type::Kind::Function:
            case Type::Kind::Enum:
            case Type::Kind::Struct:
            case Type::Kind::Integer:
                LCC_TODO("Parse type kind {} from binary module metadata", ToString(kind));
                break;
        }
    }

    for (auto ptr_fixup : ptr_fixups) {
        LCC_ASSERT(ptr_fixup.fixup_type_index < types.size());
        LCC_ASSERT(ptr_fixup.pointee_type_index < types.size());
        types[ptr_fixup.fixup_type_index] = types[ptr_fixup.pointee_type_index];
    }
    for (auto ref_fixup : ref_fixups) {
        LCC_ASSERT(ref_fixup.fixup_type_index < types.size());
        LCC_ASSERT(ref_fixup.referent_type_index < types.size());
        types[ref_fixup.fixup_type_index] = types[ref_fixup.referent_type_index];
    }

    // Starting after the header, begin parsing declarations. Stop after
    // parsing the amount of declarations specified in the header.
    auto offset = sizeof(ModuleDescription::Header);
    auto decl_count = hdr.declaration_count;
    while (decl_count--) {
        ModuleDescription::DeclarationHeader decl_hdr{};
        std::memcpy(
            &decl_hdr,
            module_metadata_blob.data() + offset,
            sizeof(decl_hdr)
        );
        offset += sizeof(decl_hdr);

        u8 name_length = module_metadata_blob.at(offset++);

        std::string name{};
        for (decltype(offset) i = 0; i < name_length; ++i)
            name += char(*(module_metadata_blob.data() + offset + i));
        offset += name_length;

        auto* ty = types.at(types_zero_index + decl_hdr.type_index);

        // FIXME: Should it be top level scope instead of global?
        switch (ModuleDescription::DeclarationHeader::Kind(decl_hdr.kind)) {
            // Created from Expr::Kind::TypeDecl
            case ModuleDescription::DeclarationHeader::Kind::TYPE: {
                LCC_ASSERT(
                    is<DeclaredType>(ty),
                    "Can't make TypeDecl from a Type that is not derived from DeclaredType"
                );
                auto type_decl = new (*this) TypeDecl(this, name, as<DeclaredType>(ty), {});
                auto decl = global_scope()->declare(ctx, std::string(name), type_decl);
            } break;

            // Created from Expr::Kind::TypeAliasDecl
            case ModuleDescription::DeclarationHeader::Kind::TYPE_ALIAS: {
                auto* type_alias_decl = new (*this) TypeAliasDecl(name, ty, {});
                auto decl = global_scope()->declare(ctx, std::string(name), type_alias_decl);
            } break;

            // Created from Expr::Kind::VarDecl
            case ModuleDescription::DeclarationHeader::Kind::VARIABLE: {
                // FIXME: Should possibly be reexported.
                auto* var_decl = new (*this) VarDecl(name, ty, nullptr, this, Linkage::Imported, {});
                auto decl = global_scope()->declare(ctx, std::string(name), var_decl);
            } break;

            // Created from Expr::Kind::FuncDecl
            case ModuleDescription::DeclarationHeader::Kind::FUNCTION: {
                LCC_ASSERT(
                    is<FuncType>(ty),
                    "Cannot create FuncDecl when deserialised type is not a function"
                );
                auto* func_decl = new (*this) FuncDecl(name, as<FuncType>(ty), nullptr, global_scope(), this, Linkage::Imported, {});
                auto decl = global_scope()->declare(ctx, std::string(name), func_decl);
            } break;

            // Created from Expr::Kind::EnumeratorDecl
            // FIXME: Is it possible to export a single enum decl? I'm pretty sure the
            // only place an enum decl can happen is within an enum type.
            case ModuleDescription::DeclarationHeader::Kind::ENUMERATOR: {
                LCC_TODO("Make an EnumeratorDecl in the global scope");
            }

            case ModuleDescription::DeclarationHeader::Kind::INVALID:
            default:
                LCC_ASSERT(false, "Invalid declaration kind in declaration header: {}", decl_hdr.kind);
        }

        LCC_TODO("Create Decl from deserialised info: {}", name);
    }

    LCC_TODO("Deserialise module from binary metadata blob");

    return true;
}
