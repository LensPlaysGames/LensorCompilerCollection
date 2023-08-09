#include <intercept/ast.hh>

lcc::intercept::Module::Module(
    File* file,
    std::string module_name,
    bool is_logical_module
) : name{std::move(module_name)}, is_module{is_logical_module}, file{file} {
    Type* ty;

    /// Create the type of the top-level function.
    if (is_logical_module) {
        ty = new (*this) FuncType({}, Type::VoidType, {});
    } else {
        auto char_ptr = new (*this) PointerType{new (*this) PointerType{Type::CCharType}};
        ty = new (*this) FuncType{
            {
                FuncTypeParam{"__argc__", Type::CIntType, {}},
                FuncTypeParam{"__argv__", char_ptr, {}},
                FuncTypeParam{"__envp__", char_ptr, {}},
            },
            Type::CIntType,
            {},
        };
    }

    /// FIXME: What name are we using for module initialisers again?
    top_level_function = new (*this) FuncDecl{
        is_logical_module ? fmt::format(".init.{}", name) : "main",
        ty,
        nullptr,
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
        new(mod) ArrayType(Type::ByteType, value.size(), location),
    },
    _index{mod.intern(value)} {
}
