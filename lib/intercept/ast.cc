#include <intercept/ast.hh>
#include <lcc/utils/rtti.hh>

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
                FuncTypeParam{"__argc__", cint_ty, {}},
                FuncTypeParam{"__argv__", char_ptr, {}},
                FuncTypeParam{"__envp__", char_ptr, {}},
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
    const Context* ctx,
    std::string&& name,
    Expr* expr
) -> Result<Expr*> {
    /// Try to insert the symbol into the map.
    auto [it, inserted] = _symbols.insert_or_assign(name, expr);
    if (inserted) return expr;

    /// If the symbol already exists, and it is a function
    /// declaration or overload set, and the new symbol is
    /// also a function declaration, merge the two into one
    /// overload set.
    if (is<FuncDecl>(expr) and is<FuncDecl, OverloadSet>(it->second)) {
        if (not is<OverloadSet>(it->second)) {
            auto func = as<FuncDecl>(it->second);
            auto mod = func->module();
            auto os = new (*mod) OverloadSet(func->location());
            os->add(func);
            it->second = os;
        }

        auto os = as<OverloadSet>(it->second);
        os->add(as<FuncDecl>(expr));
        return expr;
    }

    /// Any other case is an error.
    return Diag::Error(ctx, expr->location(), "Redeclaration of '{}'", name);
}
