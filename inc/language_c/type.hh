#ifndef LANGUAGE_C_TYPE_HH
#define LANGUAGE_C_TYPE_HH

#include <language_c/translation_unit.hh>

#include <lcc/location.hh>
#include <lcc/target.hh>
#include <lcc/utils/result.hh>

#include <fmt/base.h>

#include <string>
#include <vector>

namespace lcc::language_c {

enum class TypeKind {
    Invalid,

    Bool,
    Char,
    Short,
    Int,
    Long,
    LongLong,

    Void,
    Pointer,
    Function,
    Array,

    Count
};

struct Node;

struct Type {
private:
    TypeKind _kind{TypeKind::Invalid};
    Location _location{};

public:
    Type(TypeKind kind, Location location)
        : _kind(kind)
        , _location(location) {}

    virtual ~Type() = default;

    void* operator new(size_t) = delete;
    [[nodiscard]]
    void* operator new(size_t size, TranslationUnit& tu) {
        auto ptr = ::operator new(size);
        tu.allocated_types.push_back(static_cast<const Type*>(ptr));
        return ptr;
    };

    usz size_bits(const Target* target) const;
    usz size_bytes(const Target* target) const {
        return size_bits(target) / 8;
    }

    auto kind() const { return _kind; }
    auto location() { return _location; }
};

class IntegralType : public Type {
    bool _is_signed{};

public:
    IntegralType(TypeKind kind, bool is_signed, Location location)
        : Type(kind, location)
        , _is_signed(is_signed) {}

    bool is_signed() const { return _is_signed; }
};

class BoolType : public IntegralType {
public:
    BoolType(Location location)
        : IntegralType(TypeKind::Bool, false, location) {}
};
class CharType : public IntegralType {
public:
    CharType(bool is_signed, Location location)
        : IntegralType(TypeKind::Char, is_signed, location) {}
};
class ShortType : public IntegralType {
public:
    ShortType(bool is_signed, Location location)
        : IntegralType(TypeKind::Short, is_signed, location) {}
};
class IntType : public IntegralType {
public:
    IntType(bool is_signed, Location location)
        : IntegralType(TypeKind::Int, is_signed, location) {}
};
class LongType : public IntegralType {
public:
    LongType(bool is_signed, Location location)
        : IntegralType(TypeKind::Long, is_signed, location) {}
};
class LongLongType : public IntegralType {
public:
    LongLongType(bool is_signed, Location location)
        : IntegralType(TypeKind::LongLong, is_signed, location) {}
};

class VoidType : public Type {
public:
    VoidType(Location location)
        : Type(TypeKind::Void, location) {}
};

class PointerType : public Type {
    Type* _element_type;

public:
    PointerType(Type* element_type, Location location)
        : Type(TypeKind::Pointer, location)
        , _element_type(element_type) {}

    auto element_type() const { return _element_type; };
};

class ArrayType : public Type {
private:
    Type* _element_type;
    Node* _dimension;

public:
    ArrayType(Type* element_type, Node* dimension, Location location)
        : Type(TypeKind::Array, location)
        , _element_type(element_type)
        , _dimension(dimension) {}

    auto element_type() const { return _element_type; };
    auto dimension() const { return _dimension; };
};

class FunctionType : public Type {
public:
    struct Parameter {
        std::string name{};
        Type* type{};
    };

private:
    Type* _return_type;
    std::vector<Parameter> _parameters;

public:
    FunctionType(Type* return_type, decltype(_parameters) parameters, Location location)
        : Type(TypeKind::Function, location)
        , _return_type(return_type)
        , _parameters(parameters) {}

    auto return_type() const { return _return_type; };
    auto parameters() const { return _parameters; };
};

} // namespace lcc::language_c

template <>
struct fmt::formatter<lcc::language_c::FunctionType::Parameter> {
    constexpr auto parse(format_parse_context& ctx) { return ctx.begin(); }
    auto format(const lcc::language_c::FunctionType::Parameter&, format_context& ctx) const
        -> format_context::iterator;
};

template <>
struct fmt::formatter<lcc::language_c::Type> {
    constexpr auto parse(format_parse_context& ctx) { return ctx.begin(); }
    auto format(const lcc::language_c::Type&, format_context& ctx) const
        -> format_context::iterator;
};

template <>
struct fmt::formatter<lcc::Result<lcc::language_c::Type*>> {
    constexpr auto parse(format_parse_context& ctx) { return ctx.begin(); }
    auto format(
        lcc::Result<lcc::language_c::Type*>&,
        format_context& ctx
    ) const -> format_context::iterator;
};

#endif /* LANGUAGE_C_TYPE_HH */
