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

    Result<void> analyse(Node*);

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
    Sema(lcc::Context* context_, Node* root) : context(context_), _root(root) {}

    auto root() { return _root; }

    static bool Analyse(lcc::Context*, TranslationUnit&);
};

} // namespace lcc::language_c
