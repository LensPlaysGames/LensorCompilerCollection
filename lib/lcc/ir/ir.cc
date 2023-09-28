#include <lcc/ir/ir.hh>
#include <lcc/ir/type.hh>
#include <lcc/context.hh>
#include <lcc/diags.hh>
#include <lcc/utils/rtti.hh>

#include <algorithm>
#include <string>

#include <fmt/format.h>

namespace lcc {

usz Type::bits() const {
    switch (kind) {

    case Kind::Unknown:
    case Kind::Void: return 0;

    case Kind::Function:
    case Kind::Pointer: return 64; // FIXME: Target-dependent pointer size

    case Kind::Integer: {
        const auto& i = as<IntegerType>(this);
        return i->bitwidth();
    }

    case Kind::Array: {
        const auto& array = as<ArrayType>(this);
        return array->element_type()->bits() * array->length();
    }

    case Kind::Struct: {
        const auto& struct_ = as<StructType>(this);
        const std::vector<Type*>& members = struct_->members();
        usz sum = 0;
        for (const auto& m : members)
            sum += m->bits();
        return sum;
    }

    }
    LCC_UNREACHABLE();
}

usz Type::bytes() const {
    usz bitwidth = bits();
    return bitwidth / 8 + (bitwidth % 8 ? 1 : 0);
}

auto Type::string() const -> std::string {
    switch (kind) {
    case Kind::Unknown: return "<?>";
    case Kind::Pointer: return "ptr";
    case Kind::Void: return "void";
    case Kind::Array: return "array"; // TODO: element type
    case Kind::Function: return "function"; // TODO: function parameter and return types
    case Kind::Integer: {
        const auto& integer = as<IntegerType>(this);
        return fmt::format("i{}", integer->bitwidth());
    }
    case Kind::Struct: return "struct"; // TODO: name, if it has one? maybe size?
    }
    LCC_UNREACHABLE();
}

/// Get or create a function type.
FunctionType* FunctionType::Get(Context* ctx, Type* ret, std::vector<Type*> params) {
    // Look in ctx type cache.
    const auto& found = rgs::find_if(ctx->function_types, [&](const Type* t) {
        const FunctionType* f = as<FunctionType>(t);
        return f->ret() == ret && rgs::equal(f->params(), params);
    });
    if (found != ctx->function_types.end())
        return as<FunctionType>(*found);

    FunctionType* out = new (ctx) FunctionType(ret, params);
    ctx->function_types.push_back(out);
    return out;
}

IntegerType* IntegerType::Get(Context* ctx, usz bitwidth) {
    // Look in ctx type cache.
    const auto& found = std::find_if(ctx->integer_types.begin(), ctx->integer_types.end(),
                                     [&](const std::pair<usz, Type*>& pair) {
                                         return pair.first == bitwidth;
                                     });
    if (found != ctx->integer_types.end())
        return as<IntegerType>(found->second);

    // Create new type and store in cache.
    IntegerType* out = new (ctx) IntegerType(bitwidth);
    ctx->integer_types[bitwidth] = out;
    return out;
}

ArrayType* ArrayType::Get(Context* ctx, usz length, Type* element_type) {
    // Look in ctx type cache.
    const auto& found = rgs::find_if(ctx->array_types, [&](const Type* t) {
        const ArrayType* a = as<ArrayType>(t);
        return a->length() == length && a->element_type() == element_type;
    });
    if (found != ctx->array_types.end())
        return as<ArrayType>(*found);

    ArrayType* out = new (ctx) ArrayType(length, element_type);
    ctx->array_types.push_back(out);
    return out;
}

StructType* StructType::Get(Context* ctx, std::vector<Type*> member_types) {
    // Look in ctx type cache.
    const auto& found = rgs::find_if(ctx->struct_types, [&](const Type* t) {
        const StructType* s = as<StructType>(t);
        return rgs::equal(s->members(), member_types);
    });
    if (found != ctx->struct_types.end())
        return as<StructType>(*found);

    StructType* out = new (ctx) StructType(member_types);
    ctx->struct_types.push_back(out);
    return out;
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
