#ifndef LCC_IR_TYPE_HH
#define LCC_IR_TYPE_HH

#include <lcc/utils.hh>
#include <lcc/forward.hh>

namespace lcc {
/// Base class of all IR types.
///
/// Note: IR types are immutable and cached and uniqued in the
/// context. Two IR types are equal, iff their pointers are equal.
class Type {
public:
    enum struct Kind {
        Function,
    };

    const Kind kind;

protected:
    /// Construct a type.
    explicit Type(Kind kind) : kind(kind) {}

public:
    /// Builtin and cached types.
    static Type* UnknownTy; ///< Used because we don’t want null types, ever.
    static Type* I1Ty;      ///< Just an integer type. Used for bools.
    static Type* PtrTy;     ///< Opaque pointer type.
    static Type* VoidTy;    ///< Void type.

    /// Disallow allocating these directly.
    void* operator new(size_t) = delete;

    /// Get the aligment of this type.
    usz align() const;

    /// Get the size of this type.
    usz size() const;

    /// Get a string representation of this type.
    auto string() const -> std::string;
};

/// A function type.
class FunctionType : public Type {
    friend class Context;

    /// The return type of this function.
    Type* return_type;

    /// The parameter types of this function.
    std::vector<Type*> param_types;

private:
    FunctionType(Type* ret, std::vector<Type*> params)
        : Type(Kind::Function),
          return_type(ret),
          param_types(std::move(params)) {}

public:
    /// Get or create a function type.
    auto Get(Context& ctx, Type* ret, std::vector<Type*> params) -> FunctionType*;

    /// Get the return type of this function.
    Type* ret() const { return return_type; }

    /// Get the parameter types of this function.
    auto params() const -> const std::vector<Type*>& { return param_types; }

    /// RTTI.
    static bool classof(const Type* t) { return t->kind == Kind::Function; }
};

} // namespace lcc

/// Formatter for types.
template <>
struct fmt::formatter<lcc::Type> : formatter<string_view> {
    template <typename FormatContext>
    auto format(const lcc::Type& t, FormatContext& ctx) {
        return fmt::format_to(ctx.out(), "{}", t.string());
    }
};

#endif // LCC_IR_TYPE_HH
