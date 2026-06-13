#include <language_c/translation_unit.hh>

#include <language_c/ast.hh>

#include <iterator>
#include <string_view>

namespace lcc::language_c {

auto TranslationUnit::intern(std::string_view str) -> size_t {
    auto begin = string_literals.begin();
    auto found = rgs::find(string_literals, str);
    if (found == string_literals.end())
        string_literals.emplace_back(str);

    return (size_t) std::distance(begin, found);
}

TranslationUnit::~TranslationUnit() {
    for (const auto* const n : allocated_nodes)
        delete n;
    for (const auto* const s : allocated_scopes)
        delete s;
    for (const auto* const t : allocated_types)
        delete t;
}

} // namespace lcc::language_c
