#include <lcc/ir/type.hh>
#include <lcc/context.hh>

namespace lcc {

/// Get or create a function type.
FunctionType* FunctionType::Get(Context* ctx, Type* ret, std::vector<Type*> params) {
    // TODO: Look in ctx type cache.
    return new (ctx) FunctionType(ret, params);
}

IntegerType* IntegerType::Get(Context* ctx, usz bitwidth) {
    // TODO: Look in ctx type cache.
    return new (ctx) IntegerType(bitwidth);
}

ArrayType* ArrayType::Get(Context* ctx, usz length, Type* element_type) {
    return new (ctx) ArrayType(length, element_type);
}

StructType* StructType::Get(Context* ctx, std::vector<Type*> member_types) {
    return new (ctx) StructType(member_types);
}

}
