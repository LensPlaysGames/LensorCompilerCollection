#ifndef LANGUAGE_C_FORMAT_HH
#define LANGUAGE_C_FORMAT_HH

#include <language_c/type.hh>

#include <fmt/base.h>

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

#endif /* LANGUAGE_C_FORMAT_HH */
