#include <language_c/ast.hh>

#include <lccbase/context.hh>
#include <lccbase/diags.hh>
#include <lccbase/location.hh>
#include <lcc/utils/result.hh>

#include <fmt/base.h>

namespace lcc::language_c {

class Sema {
    TranslationUnit& tu;
    lcc::Context* context;
    std::unordered_set<Node*> analysed{};
    Declaration* defining{};

    std::unordered_map<const Node*, Type*> _type_cache{};

    [[nodiscard]]
    Result<Type*> type_of(const Node* n);
    void update_type(const Node*, Type*);

    static bool type_is_arithmetic(const Type*);
    static bool type_is_integral(const Type*);

    [[nodiscard]]
    Result<void> analyse_declaration(Declaration*&);
    [[nodiscard]]
    Result<void> analyse_unary(UnaryOperation*&);
    [[nodiscard]]
    Result<void> analyse_binary(BinaryOperation*&);
    [[nodiscard]]
    Result<void> analyse_return(Return*&);
    [[nodiscard]]
    Result<Declaration*> analyse_name_reference(NameReference*&);
    [[nodiscard]]
    Result<void> analyse_call(Call*&);

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
    Sema(lcc::Context* context_, TranslationUnit& tu_)
        : tu(tu_)
        , context(context_) {
        if (not context)
            Diag::ICE("context is nullptr");
        if (not tu.tree)
            Diag::ICE("root is nullptr");
    }

    [[nodiscard]]
    auto& root() { return tu.tree; }

    [[nodiscard]]
    static bool Analyse(lcc::Context*, TranslationUnit&);
};

} // namespace lcc::language_c
