#ifndef C_OPTS_HH
#define C_OPTS_HH

#include "lcc/context.hh"

namespace lcc::c {
enum struct StandardVersion {
    C89,
    C99,
    C11,
    C17,
    C23,
};

struct Extensions {
    // MS extensions
    bool ms_tryexcept   : 1 = false;
    bool ms_tryfinally  : 1 = false;

    // GCC extensions
    bool gnu_idents     : 1 = false;

    // Clang extensions
};

struct CompileOptions {
    StandardVersion std = StandardVersion::C17;
    Extensions ext;
};

class CContext {
private:
    Context* _context;

public:
    CompileOptions opts;

    CContext(Context* context)
        : _context(context) {}

    auto lcc_context() const { return _context; }
};
} // namespace lcc::c

#endif // C_OPTS_HH
