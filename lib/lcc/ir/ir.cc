#include <lcc/ir/type.hh>
#include <lcc/context.hh>

namespace lcc {

/// Get or create a function type.
FunctionType* FunctionType::Get(Context& ctx, Type* ret, std::vector<Type*> params) {
    // TODO: Look in ctx type cache
    return new (ctx) FunctionType(ret, params);
}

}
