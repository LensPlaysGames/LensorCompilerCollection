#include <intercept/ast.hh>
#include <intercept/parser.hh>
#include <lcc/utils/ast_printer.hh>
#include <lcc/utils/rtti.hh>

namespace intc = lcc::intercept;

intc::Type* intc::Type::Bool;
intc::Type* intc::Type::Byte;
intc::Type* intc::Type::Integer;
intc::Type* intc::Type::Unknown;
intc::Type* intc::Type::Void;
intc::Type* intc::Type::VoidPtr;
intc::Type* intc::Type::OverloadSet;

/// ===========================================================================
///  Module
/// ===========================================================================
intc::Module::Module(
    File* file,
    std::string module_name,
    bool is_logical_module
) : name{std::move(module_name)}, is_module{is_logical_module}, file{file} {
    FuncType* ty{};

    /// Create the type of the top-level function.
    if (is_logical_module) {
        ty = new (*this) FuncType({}, BuiltinType::Void(this), {}, {});
    } else {
        auto cchar_ty = BuiltinType::CChar(this);
        auto cint_ty = BuiltinType::CInt(this);
        auto char_ptr = new (*this) PointerType{new (*this) PointerType{cchar_ty}};
        ty = new (*this) FuncType{
            {
                {"__argc__", cint_ty, {}},
                {"__argv__", char_ptr, {}},
                {"__envp__", char_ptr, {}},
            },
            cint_ty,
            {},
            {},
        };
    }

    /// FIXME: What name are we using for module initialisers again?
    top_level_function = new (*this) FuncDecl{
        is_logical_module ? fmt::format(".init.{}", name) : "main",
        ty,
        new (*this) BlockExpr{{}, {}},
        this,
        Linkage::Exported,
        {},
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
        new(mod) ArrayType(
            BuiltinType::Byte(&mod),
            new(mod) IntegerLiteral(value.size(), location, Type::Integer),
            location
        ),
    },
    _index{mod.intern(value)} {
}

/// Declare a symbol in this scope.
auto intc::Scope::declare(
    Parser* p,
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
    ) return Diag::Error(p->context, decl->location(), "Redeclaration of '{}'", name);

    /// TODO: Check that this declaration is hygienic if it’s part of a macro.

    /// Otherwise, add the symbol.
    symbols.emplace(std::move(name), decl);
    return decl;
}

bool intc::Expr::is_lvalue() const {
    return is<ReferenceType>(type()) or is<VarDecl, FuncDecl>(this);
}

bool intc::Expr::is_assignable_lvalue() const {
    /// References to anything other than functions are
    /// assignable lvalues.
    if (auto ref = cast<ReferenceType>(type()))
        return not is<FuncType>(ref->element_type());

    /// Variable declarations are assignable lvalues.
    return is<VarDecl>(this);
}

auto intc::Expr::type() const -> Type* {
    if (auto e = cast<TypedExpr>(this)) return e->type();
    return Type::Void;
}

bool intc::Type::is_unknown() const {
    auto builtin = cast<BuiltinType>(this);
    if (not builtin) return false;
    return builtin->builtin_kind() == BuiltinType::BuiltinKind::Unknown;
}

bool intc::Type::is_void() const {
    auto builtin = cast<BuiltinType>(this);
    if (not builtin) return false;
    return builtin->builtin_kind() == BuiltinType::BuiltinKind::Void;
}

auto intc::Expr::Clone(Module& mod, Expr* expr) -> Expr* {
    LCC_ASSERT(false, "TODO: Clone expressions");
}

/// ===========================================================================
///  AST Printing
/// ===========================================================================
namespace {
using lcc::as;
using lcc::cast;
using lcc::is;

struct ASTPrinter : lcc::utils::ASTPrinter<ASTPrinter, intc::Expr, intc::Type> {
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
                    " {}{} {}\n",
                    C(White),
                    v->name(),
                    v->type()->string(use_colour)
                );
                return;
            }

            case K::Binary: {
                auto b = as<intc::BinaryExpr>(e);
                PrintBasicHeader("BinaryExpr", e);
                out += fmt::format(
                    " {}{} {}\n",
                    C(Red),
                    intc::ToString(b->op()),
                    b->type()->string(use_colour)
                );
                return;
            }

            case K::Unary: {
                auto u = as<intc::UnaryExpr>(e);
                PrintBasicHeader("UnaryExpr", e);
                out += fmt::format(
                    " {}{} {}\n",
                    C(Red),
                    intc::ToString(u->op()),
                    u->type()->string(use_colour)
                );
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
                    " {}{} {}\n",
                    C(White),
                    n->name(),
                    n->type()->string(use_colour)
                );
                return;
            }

            case K::StructDecl: PrintBasicNode("StructDecl", e, e->type()); return;
            case K::TypeAliasDecl: PrintBasicNode("TypeAliasDecl", e, e->type()); return;
            case K::StringLiteral: PrintBasicNode("StringLiteral", e, e->type()); return;
            case K::CompoundLiteral: PrintBasicNode("CompoundLiteral", e, e->type()); return;
            case K::If: PrintBasicNode("IfExpr", e, e->type()); return;
            case K::While: PrintBasicNode("WhileExpr", e, nullptr); return;
            case K::For: PrintBasicNode("ForExpr", e, nullptr); return;
            case K::Block: PrintBasicNode("BlockExpr", e, e->type()); return;
            case K::Return: PrintBasicNode("ReturnExpr", e, nullptr); return;
            case K::Call: PrintBasicNode("CallExpr", e, e->type()); return;
            case K::IntrinsicCall: PrintBasicNode("IntrinsicCallExpr", e, e->type()); return;
            case K::Cast: PrintBasicNode("CastExpr", e, e->type()); return;
            case K::MemberAccess: PrintBasicNode("MemberAccessExpr", e, e->type()); return;
        }

        PrintBasicNode(R"(<???>)", e, e->type());
    }

    /// Print a node.
    void operator()(const intc::Expr* e, std::string leading_text = "") {
        PrintHeader(e);

        /// Print the children of a node.
        using K = intc::Expr::Kind;
        switch (e->kind()) {
            case K::FuncDecl: {
                auto f = as<intc::FuncDecl>(e);
                if (auto block = cast<intc::BlockExpr>(const_cast<intc::FuncDecl*>(f)->body())) {
                    PrintChildren(block->children(), leading_text);
                } else {
                    intc::Expr* children[] = {const_cast<intc::FuncDecl*>(f)->body()};
                    PrintChildren(children, leading_text);
                }
            } break;

            case K::Binary: {
                auto b = as<intc::BinaryExpr>(e);
                intc::Expr* children[] = {b->lhs(), b->rhs()};
                PrintChildren(children, leading_text);
            } break;

            case K::NameRef: {
                auto n = as<intc::NameRefExpr>(e);
                if (n->target()) {
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

            case K::While: break;
            case K::For: break;
            case K::Return: break;
            case K::StructDecl: break;
            case K::TypeAliasDecl: break;
            case K::IntegerLiteral: break;
            case K::StringLiteral: break;
            case K::CompoundLiteral: break;
            case K::If: break;
            case K::Block: break;
            case K::IntrinsicCall: break;
            case K::Cast: break;
            case K::MemberAccess: break;
        }
    }
};
} // namespace

auto intc::Type::string(bool use_colours) const -> std::string {
    lcc::utils::Colours C{use_colours};
    using enum lcc::utils::Colour;

    switch (kind()) {
        case Kind::Named: return as<NamedType>(this)->name();
        case Kind::Pointer:
            return fmt::format(
                "{}@{}{}",
                C(Red),
                C(Cyan),
                as<PointerType>(this)->element_type()->string(use_colours)
            );
        case Kind::Reference:
            return fmt::format(
                "{}&{}{}",
                C(Red),
                C(Cyan),
                as<ReferenceType>(this)->element_type()->string(use_colours)
            );

        case Kind::Integer: {
            auto i = as<IntegerType>(this);
            return fmt::format("{}{}{}", C(Cyan), i->is_signed() ? "i" : "u", i->bit_width());
        }

        case Kind::Struct: {
            auto decl = as<StructType>(this)->decl();
            return fmt::format(
                "{}struct {}{}",
                C(Red),
                C(Cyan),
                decl->name().empty() ? "<anonymous>" : decl->name()
            );
        }

        case Kind::Array: {
            auto arr = as<ArrayType>(this);
            if (auto sz = cast<IntegerLiteral>(arr->size())) {
                return fmt::format(
                    "{}{}[{}{}{}]",
                    arr->element_type()->string(use_colours),
                    C(Red),
                    C(Magenta),
                    sz->value(),
                    C(Red)
                );
            } else {
                return fmt::format(
                    "{}{}[{}?{}]",
                    arr->element_type()->string(use_colours),
                    C(Red),
                    C(Magenta),
                    C(Red)
                );
            }
        }

        case Kind::Builtin:
            switch (as<BuiltinType>(this)->builtin_kind()) {
                using K = BuiltinType::BuiltinKind;
                case K::Bool: return fmt::format("{}bool", C(Cyan));
                case K::Byte: return fmt::format("{}byte", C(Cyan));
                case K::CChar: return fmt::format("{}__c_char", C(Cyan));
                case K::CInt: return fmt::format("{}__c_int", C(Cyan));
                case K::Integer: return fmt::format("{}int", C(Cyan));
                case K::Unknown: return fmt::format("{}<?>", C(Cyan));
                case K::Void: return fmt::format("{}void", C(Cyan));
                case K::OverloadSet: return fmt::format("{}<overload set>", C(Cyan));
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
            out += fmt::format("{})", C(Red));
            return out;
        }
    }

    LCC_UNREACHABLE();
}

void intc::Module::print() {
    ASTPrinter p{true};
    for (auto* node : as<BlockExpr>(top_level_function->body())->children())
        p(node);
}
