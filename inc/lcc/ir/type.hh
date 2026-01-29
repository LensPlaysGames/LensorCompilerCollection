#ifndef LCC_IR_TYPE_HH
#define LCC_IR_TYPE_HH

#include <lcc/forward.hh>
#include <lcc/utils.hh>

#include <string>
#include <utility>
#include <variant>
#include <vector>

namespace lcc {
/// Base class of all IR types.
///
/// Note: IR types are immutable and cached and uniqued in the
/// context. Two IR types are equal, iff their pointers are equal.
class Type {
    friend class lcc::Init;
    friend class lcc::Context;

public:
    enum struct Kind {
        // Uninitialized or Unknown Type
        Unknown,
        // A pointer type has a value that contains a memory address.
        // A value of pointer type may be loaded from.
        Pointer,
        // A void type represents the absence of a value.
        Void,
        // An array type has a value that represents N elements of the array's
        // element type, contiguously. An array with a dimension (size) of two
        // has two, uniquely addressable points where each of the element's values
        // may be found.
        // An array may contain any non-zero amount of elements.
        // An array's elements may be of any *one* type.
        // All element's in an array have the same size and alignment.
        // @see ArrayConstant
        Array,
        // A value that represents an IR function.
        Function,
        // (ℤ) An integer type has a value that may represent any positive whole
        // number, negative whole number, or zero, with no fractional parts.
        // ..., -3, -2 -1, -0, 0, 1, 2, 3, ...
        // @see IntegerConstant
        // @see AddInst
        Integer,
        // (ℚ) A fractional type has a value that represents a rational number.
        // Rational numbers are numbers that may be represented as `p / q`, where
        // `q` is non-zero.
        // @see FractionalConstant
        // @see AddInst
        Fractional,
        // An aggregate type has a value that represents it's constituent types
        // contiguously. A struct with two `i32` members has two, uniquely
        // addressable points where each of the member's values may be found.
        // A struct may contain any amount of members.
        // A struct's members may be of any type.
        Struct,
    };

    const Kind kind;

protected:
    /// Construct a type.
    explicit constexpr Type(Kind kind_) : kind(kind_) {}

public:
    virtual ~Type() = default;

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
    friend class lcc::Init;
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
    friend class lcc::Init;
    friend class lcc::Context;

    /// The return type of this function.
    Type* return_type;

    /// The parameter types of this function.
    std::vector<Type*> param_types;

    bool _variadic{false};

private:
    FunctionType(Type* ret, std::vector<Type*> params, bool variadic = false)
        : Type(Kind::Function),
          return_type(ret),
          param_types(std::move(params)),
          _variadic(variadic) {}

public:
    /// Get or create a function type.
    static auto Get(Context* ctx, Type* ret, std::vector<Type*> params, bool is_variadic = false) -> FunctionType*;

    /// Get the return type of this function.
    Type* ret() const { return return_type; }

    /// Get the return type of this function.
    Type*& ret_reference() { return return_type; }

    /// Get the parameter types of this function.
    auto params() const -> const std::vector<Type*>& { return param_types; }

    /// Get the parameter types of this function.
    auto params() -> std::vector<Type*>& { return param_types; }

    /// True if this function type is (C-style) variadic, false otherwise.
    bool variadic() const { return _variadic; }

    /// RTTI.
    static bool classof(const Type* t) { return t->kind == Kind::Function; }
};

class IntegerType : public Type {
    friend class lcc::Init;
    friend class lcc::Context;

    usz _width;

private:
    constexpr IntegerType(usz width) : Type(Kind::Integer), _width(width) {}

public:
    static auto Get(Context* ctx, usz width) -> IntegerType*;

    usz bitwidth() const {
        return _width;
    }

    /// RTTI.
    static bool classof(const Type* t) { return t->kind == Kind::Integer; }
};

class FractionalType : public Type {
    friend class lcc::Init;
    friend class lcc::Context;

    usz _width;

private:
    constexpr FractionalType(usz width) : Type(Kind::Fractional), _width(width) {}

public:
    static auto Get(Context* ctx, usz width) -> FractionalType*;

    usz bitwidth() const {
        return _width;
    }

    /// RTTI.
    static bool classof(const Type* t) { return t->kind == Kind::Fractional; }
};

class StructType : public Type {
    friend class lcc::Init;
    friend class lcc::Context;

    std::vector<Type*> _members;
    std::variant<long int, std::string> _id;
    usz _align{AlignNotSet};

private:
    StructType(std::vector<Type*> members, std::string name)
        : Type(Kind::Struct),
          _members(std::move(members)),
          _id(std::move(name)) {}
    StructType(std::vector<Type*> members, long int index)
        : Type(Kind::Struct),
          _members(std::move(members)),
          _id(index) {}

public:
    static constexpr usz AlignNotSet = (usz) -1;

    static auto Get(Context* ctx, std::vector<Type*> member_types, usz align_bits = AlignNotSet, std::string name = {}) -> StructType*;

    /// Return the element count.
    usz member_count() const { return _members.size(); }

    /// Return the types of the members of the struct.
    auto members() const -> const std::vector<Type*>& { return _members; }

    /// A struct member's BYTE offset.
    /// Offset of struct member N is calculated via the following rules:
    /// - struct member 0 -> offset 0
    /// - struct member N -> (struct member N-1 offset) + (struct member N-1 byte
    ///                      size), then ALIGNED TO struct member N alignment.
    ///                      This ensures enough space for the data, as well as preserves alignment
    ///                      for every member.
    ///
    /// @returns optional if member index out of range, otherwise returns
    ///          byte offset of member at index.
    auto member_offset(usz member_index) const -> std::optional<usz> {
        if (member_index > member_count())
            return {};

        if (member_index == 0) return 0;

        // If the index is both non-zero AND less than member count, we know there
        // are at least two members.

        usz offset{};
        for (usz i = 1; i <= member_index; ++i) {
            offset += members().at(i - 1)->bytes();
            offset = utils::AlignTo(
                offset,
                members().at(i)->align_bytes()
            );
        }

        return offset;
    };

    /// The name of this struct type if it is named
    auto name() const -> const std::string& { return std::get<std::string>(_id); }

    /// The global index, within its context, for this struct if it is unnamed
    auto index() const -> long int {
        LCC_ASSERT(std::holds_alternative<long int>(_id));
        return std::get<long int>(_id);
    }

    /// True if this is a unique, named struct type, false otherwise.
    bool named() const { return std::holds_alternative<std::string>(_id); }

    usz alignment() const { return _align; }

    /// RTTI.
    static bool classof(const Type* t) { return t->kind == Kind::Struct; }
};

} // namespace lcc

/// Formatter for types.
template <>
struct fmt::formatter<lcc::Type> : formatter<string_view> {
    template <typename FormatContext>
    auto format(const lcc::Type& t, FormatContext& ctx) const {
        // FIXME: Don't emit colors if LCC context doesn't ask us to...
        return fmt::format_to(ctx.out(), "{}", t.string());
    }
};

#endif // LCC_IR_TYPE_HH
