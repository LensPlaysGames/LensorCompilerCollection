#ifndef LANGUAGE_C_TYPE_HH
#define LANGUAGE_C_TYPE_HH

#include <fmt/base.h>

#include <string>
#include <vector>

#include <lcc/location.hh>

namespace lcc::language_c {

enum class TypeKind {
    Invalid,

    Int,
    Void,
    Pointer,
    Function,
    Array,

    Count
};

struct Node;

class Type {
    TypeKind _kind{TypeKind::Invalid};
    Location _location{};

public:
    Type(TypeKind kind, Location location)
        : _kind(kind)
        , _location(location) {}

    auto kind() const { return _kind; }
    auto location() { return _location; }
};

class IntType : public Type {
public:
    IntType(Location location)
        : Type(TypeKind::Int, location) {}
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

#endif /* LANGUAGE_C_TYPE_HH */
