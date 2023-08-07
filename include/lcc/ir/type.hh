#ifndef LCC_IR_TYPE_HH
#define LCC_IR_TYPE_HH

#include <lcc/utils.hh>

namespace lcc {
/// Base class of all IR types.
class Type {
    /// Disallow allocating these directly.
    void* operator new(size_t) = delete;

public:
    /// Builtin types.
    static Type* UnknownTy;
};

/// A function type.
class FunctionType : public Type {
    /// The return type of this function.
    Type* return_type;

    /// The parameter types of this function.
    std::vector<Type*> param_types;

public:
    FunctionType(Type* ret, std::vector<Type*> params)
        : return_type(ret), param_types(std::move(params)) {}

    /// Get the return type of this function.
    Type* ret() const { return return_type; }

    /// Get the parameter types of this function.
    auto params() const -> const std::vector<Type*>& { return param_types; }
};

} // namespace lcc

#endif // LCC_IR_TYPE_HH
