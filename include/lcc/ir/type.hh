#ifndef LCC_IR_TYPE_HH
#define LCC_IR_TYPE_HH

#include <lcc/forward.hh>
#include <lcc/utils.hh>
#include <vector>

namespace lcc {
/// Base class of all IR types.
///
/// Note: IR types are immutable and cached and uniqued in the
/// context. Two IR types are equal, iff their pointers are equal.
class Type {
    friend class lcc::Context;

public:
    enum struct Kind {
        Unknown,
        Pointer,
        Void,
        Array,
        Function,
        Integer,
        Struct,
    };

    const Kind kind;

protected:
    /// Construct a type.
    explicit Type(Kind kind) : kind(kind) {}

public:
    /// Builtin and cached types.
    ///
    /// When adding a type here, don’t forget to initialise
    /// it in Context::InitialiseLCCData().
    static Type* UnknownTy; ///< Used because we don’t want null types, ever.
    static Type* PtrTy;     ///< Opaque pointer type.
    static Type* VoidTy;    ///< Void type.
    static Type* I1Ty;      ///< Just an integer type. Used for bools.

    /// Disallow allocating these directly.
    void* operator new(size_t) = delete;
    void* operator new(size_t sz, Context*) { return ::operator new(sz); }

    /// Get the alignment of this type, in bits.
    usz align() const;

    /// Get the alignment of this type, in bytes.
    usz align_bytes() const;

    /// Get the amount of bits required to represent an instance of this type.
    usz bits() const;

    /// Get the minimum amount of bytes required to represent an instance of this type.
    usz bytes() const;

    /// Check if this is `ptr`.
    bool is_ptr() const { return kind == Kind::Pointer; }

    /// Check if this is `void`.
    bool is_void() const { return kind == Kind::Void; }

    /// Get a string representation of this type.
    auto string(bool use_colour = true) const -> std::string;
};

class ArrayType : public Type {
    friend class lcc::Context;

    usz _length;
    Type* _element_type;

private:
    ArrayType(usz length, Type* element_type) : Type(Kind::Array), _length(length), _element_type(element_type) {}

public:
    static auto Get(Context* ctx, usz length, Type* element_type) -> ArrayType*;

    /// Return the element count.
    usz length() const { return _length; }

    // Return the element type.
    Type* element_type() const { return _element_type; }

    /// RTTI.
    static bool classof(const Type* t) { return t->kind == Kind::Array; }
};

/// A function type.
class FunctionType : public Type {
    friend class lcc::Context;

    /// The return type of this function.
    Type* return_type;

    /// The parameter types of this function.
    std::vector<Type*> param_types;

    bool is_variadic;

private:
    FunctionType(Type* ret, std::vector<Type*> params, bool is_variadic = false)
        : Type(Kind::Function),
          return_type(ret),
          param_types(std::move(params)),
          is_variadic(is_variadic) {}

public:
    /// Get or create a function type.
    static auto Get(Context* ctx, Type* ret, std::vector<Type*> params, bool is_variadic = false) -> FunctionType*;

    /// Get the return type of this function.
    Type* ret() const { return return_type; }

    /// Get the parameter types of this function.
    auto params() const -> const std::vector<Type*>& { return param_types; }

    /// True if this function type is (C-style) variadic, false otherwise.
    bool variadic() const { return is_variadic; }

    /// RTTI.
    static bool classof(const Type* t) { return t->kind == Kind::Function; }
};

class IntegerType : public Type {
    friend class lcc::Context;

    usz _width;

private:
    IntegerType(usz width) : Type(Kind::Integer), _width(width) {}

public:
    static auto Get(Context* ctx, usz width) -> IntegerType*;

    usz bitwidth() const {
        return _width;
    }

    /// RTTI.
    static bool classof(const Type* t) { return t->kind == Kind::Integer; }
};

class StructType : public Type {
    friend class lcc::Context;

    std::vector<Type*> _members;
    std::variant<long int, std::string> _id;

private:
    StructType(std::vector<Type*> members, std::string name) : Type(Kind::Struct), _members(std::move(members)), _id(std::move(name)) {}
    StructType(std::vector<Type*> members, long int index) : Type(Kind::Struct), _members(std::move(members)), _id(index) {}

public:
    static auto Get(Context* ctx, std::vector<Type*> member_types, std::string name = {}) -> StructType*;

    /// Return the element count.
    usz member_count() const { return _members.size(); }

    /// Return the types of the members of the struct.
    auto members() const -> const std::vector<Type*>& { return _members; }

    /// The name of this struct type if it is named
    auto name() const -> const std::string& { return std::get<std::string>(_id); }

    /// The global index, within its context, for this struct if it is unnamed
    auto index() const -> long int { return std::get<long int>(_id); }

    /// True if this is a unique, named struct type, false otherwise.
    bool named() const { return std::holds_alternative<std::string>(_id); }

    /// RTTI.
    static bool classof(const Type* t) { return t->kind == Kind::Struct; }
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
