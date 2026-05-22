#ifndef LANGUAGE_C_IRGEN_HH
#define LANGUAGE_C_IRGEN_HH

#include <lcc/context.hh>

#include <language_c/ast.hh>

namespace lcc::language_c {

class IRGen {

public:
    static auto Generate(Context*, TranslationUnit&) -> lcc::Module*;
};

} // namespace lcc::language_c

#endif /* LANGUAGE_C_IRGEN_HH */
