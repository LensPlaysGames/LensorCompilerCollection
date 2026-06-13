#ifndef LCC_LANGUAGE_C_TRANSLATION_UNIT_HH
#define LCC_LANGUAGE_C_TRANSLATION_UNIT_HH

#include <vector>

namespace lcc::language_c {
struct Node;
struct Declaration;
struct Scope;
struct Type;

struct TranslationUnit {
    // Produced by the parser.
    Node* tree{};

    // Book-keeping of memory owned by this translation unit.
    std::vector<const Node*> allocated_nodes{};
    std::vector<const Scope*> allocated_scopes{};
    std::vector<const Type*> allocated_types{};

    // Filled in by semantic analysis.
    std::vector<const Declaration*> functions{};
    std::vector<const Declaration*> globals{};

    // Disable Copying
    TranslationUnit(const TranslationUnit&) = delete;
    TranslationUnit& operator=(const TranslationUnit&) = delete;

    // Enable Moving
    TranslationUnit(TranslationUnit&& other) noexcept = default;
    TranslationUnit& operator=(TranslationUnit&& other) noexcept = default;

    TranslationUnit() {};

    ~TranslationUnit();
};

} // namespace lcc::language_c

#endif /* LCC_LANGUAGE_C_TRANSLATION_UNIT_HH */
