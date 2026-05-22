#ifndef LANGUAGE_C_TYPE_HH
#define LANGUAGE_C_TYPE_HH

#include <fmt/base.h>

#include <string>
#include <vector>

namespace lcc::language_c {

enum class TypeKind {
    Invalid,

    Int,
    Pointer,
    Function,
    Array,

    Count
};

class Type {
    TypeKind _kind{TypeKind::Invalid};

public:
    Type(TypeKind kind) : _kind(kind) {}

    auto kind() const { return _kind; }
};

class IntType : public Type {
public:
    IntType() : Type(TypeKind::Int) {}
};

class PointerType : public Type {
    Type* _element_type;

public:
    PointerType(Type* element_type)
        : Type(TypeKind::Pointer), _element_type(element_type) {}
};

class ArrayType : public Type {
private:
    Type* _element_type;

public:
    ArrayType(Type* element_type)
        : Type(TypeKind::Array),
          _element_type(element_type) {}

    auto element_type() const { return _element_type; };
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
    FunctionType(Type* return_type, decltype(_parameters) parameters)
        : Type(TypeKind::Function),
          _return_type(return_type),
          _parameters(parameters) {}

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
