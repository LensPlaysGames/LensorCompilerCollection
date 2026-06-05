#include <language_c/ast.hh>

#include <lcc/context.hh>
#include <lcc/diags.hh>
#include <lcc/location.hh>
#include <lcc/utils/result.hh>

#include <fmt/base.h>

namespace lcc::language_c {

class Sema {
    lcc::Context* context;
    std::unordered_set<Node*> analysed{};
    Declaration* defining{};
    Node* _root{};

    std::unordered_map<const Node*, Type*> _type_cache{};

    [[nodiscard]]
    Type* type_of(const Node* n);
    void update_type(Node*, Type*);

    [[nodiscard]]
    Result<void> analyse_declaration(Declaration*&);
    [[nodiscard]]
    Result<void> analyse_binary(BinaryOperation*&);
    [[nodiscard]]
    Result<void> analyse_return(Return*&);
    [[nodiscard]]
    Result<void> analyse_name_reference(NameReference*&);

    // @return diagnostic(s) produced by semantic analysis.
    [[nodiscard]]
    Result<void> analyse(Node*&);

    template <typename... Args>
    auto Note(Location where, std::string id, fmt::format_string<Args...> fmt, Args... args) -> Diag {
        return Diag::Note(context, where, id, fmt, std::forward<Args>(args)...);
    }
    template <typename... Args>
    auto Warning(Location where, std::string id, fmt::format_string<Args...> fmt, Args... args) -> Diag {
        return Diag::Warning(context, where, id, fmt, std::forward<Args>(args)...);
    }
    template <typename... Args>
    auto Error(Location where, std::string id, fmt::format_string<Args...> fmt, Args... args) -> Diag {
        return Diag::Error(context, where, id, fmt, std::forward<Args>(args)...);
    }

public:
    Sema(lcc::Context* context_, Node* root)
        : context(context_)
        , _root(root) {}

    [[nodiscard]]
    auto root() { return _root; }

    [[nodiscard]]
    static bool Analyse(lcc::Context*, TranslationUnit&);
};

} // namespace lcc::language_c
