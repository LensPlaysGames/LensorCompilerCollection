#include <lcc/ir/ir.hh>
#include <lcc/ir/type.hh>
#include <lcc/context.hh>
#include <lcc/diags.hh>

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

bool is_block_terminator(Inst* inst) {
    return inst && (+inst->kind() >= +Value::Kind::Branch && +inst->kind() <= +Value::Kind::Unreachable);
}

Inst* Block::insert(Inst* i, bool force) {
    if (not force and closed())
        Diag::ICE("Insertion into block that has already been closed.");

    inst_list.push_back(i);

    return i;
}

}
