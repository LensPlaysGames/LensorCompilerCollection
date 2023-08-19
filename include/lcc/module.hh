#ifndef LCC_MODULE_HH
#define LCC_MODULE_HH

#include <lcc/forward.hh>
#include <lcc/ir/ir.hh>
#include <lcc/utils.hh>

namespace lcc {
/// An LCC IR module.
class Module {
    Context* _ctx;

    Module(Module&) = delete;
    Module(Module&&) = delete;
    Module& operator=(Module&) = delete;
    Module& operator=(Module&&) = delete;

public:
    /// Create a new LCC module.
    ///
    /// It is recommended to allocate these on the heap since
    /// they can’t be moved.
    Module(Context* ctx) : _ctx(ctx) {}

    /// Get the context that owns the module.
    auto context() const -> Context* { return _ctx; }
};
} // namespace lcc

#endif // LCC_MODULE_HH
