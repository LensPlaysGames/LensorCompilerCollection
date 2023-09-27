#include <lcc/ir/ir.hh>
#include <lcc/ir/type.hh>
#include <lcc/context.hh>
#include <lcc/diags.hh>
#include <lcc/utils/rtti.hh>

#include <string>

#include <fmt/format.h>

namespace lcc {

usz Type::size() const {
    switch (kind) {
    case Kind::Unknown: return 0;
    case Kind::Function: [[fallthrough]];
    case Kind::Pointer: return 8; // FIXME: Target-dependent pointer size
    case Kind::Void: return 0;
    case Kind::Array: {
        const auto& array = as<ArrayType>(this);
        return array->element_type()->size() * array->length();
    }
    case Kind::Integer: return 8; // FIXME: Target-dependent integer size
    case Kind::Struct: {
        const auto& struct_ = as<StructType>(this);
        const std::vector<Type*>& members = struct_->members();
        usz sum = 0;
        for (const auto& m : members)
            sum += m->size();
        return sum;
    }
    }
    LCC_UNREACHABLE();
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
