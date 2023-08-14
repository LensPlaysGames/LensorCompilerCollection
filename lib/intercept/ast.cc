#include <intercept/ast.hh>
#include <intercept/parser.hh>
#include <lcc/utils/rtti.hh>

lcc::intercept::Type* lcc::intercept::Type::Unknown;
lcc::intercept::Type* lcc::intercept::Type::Integer;

lcc::intercept::Module::Module(
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
        nullptr,
        this,
        Linkage::Exported,
        {},
    };
}

lcc::intercept::Module::~Module() {
    for (auto* node : nodes) delete node;
    for (auto* type : types) delete type;
    for (auto* scope : scopes) delete scope;
    for (auto& [_, i] : imports) delete i;
}

auto lcc::intercept::Module::intern(std::string_view str) -> usz {
    auto it = rgs::find(strings, str);
    if (it != strings.end()) { return usz(it - strings.begin()); }
    strings.emplace_back(str);
    return strings.size() - 1;
}

lcc::intercept::StringLiteral::StringLiteral(
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
auto lcc::intercept::Scope::declare(
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

auto lcc::intercept::Expr::Clone(Module& mod, Expr* expr) -> Expr* {
    LCC_ASSERT(false, "TODO: Clone expressions");
}