#ifndef LAYE_SEMA_HH
#define LAYE_SEMA_HH

#include <laye/ast.hh>
#include <lcc/utils.hh>

namespace lcc::laye {
class Sema {
public:
    static void Analyse(LayeContext* context, bool use_colours = true);
};
}

#endif // LAYE_SEMA_HH
