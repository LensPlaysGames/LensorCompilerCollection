#include <lcc/context.hh>

#include <language_c/ast.hh>

namespace lcc::language_c {

class Sema {

public:
    static bool Analyse(lcc::Context*, TranslationUnit&);
};

} // namespace lcc::language_c
