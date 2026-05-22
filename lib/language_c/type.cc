#include <language_c/type.hh>

#include <fmt/base.h>
#include <fmt/format.h>
#include <fmt/std.h>

#include <lcc/diags.hh>

auto fmt::formatter<lcc::language_c::Type>::format(const lcc::language_c::Type& t, format_context& ctx) const
    -> format_context::iterator {
    switch (t.kind()) {
        case lcc::language_c::TypeKind::Void:
            return fmt::format_to(ctx.out(), "void");
        case lcc::language_c::TypeKind::Int:
            return fmt::format_to(ctx.out(), "int");
        case lcc::language_c::TypeKind::Pointer:
            return fmt::format_to(ctx.out(), "pointer");
        case lcc::language_c::TypeKind::Function: {
            const lcc::language_c::FunctionType& f{*(const lcc::language_c::FunctionType*) &t};
            return fmt::format_to(ctx.out(), "{}({})", *f.return_type(), fmt::join(f.parameters(), ", "));
        }
        case lcc::language_c::TypeKind::Array:
            return fmt::format_to(ctx.out(), "array");

        case lcc::language_c::TypeKind::Invalid:
        case lcc::language_c::TypeKind::Count:
            lcc::Diag::ICE("invalid type kind");
    }
    lcc::Diag::ICE("unreachable");
}

auto fmt::formatter<lcc::language_c::FunctionType::Parameter>::format(const lcc::language_c::FunctionType::Parameter& p, format_context& ctx) const
    -> format_context::iterator {
    if (p.type)
        return fmt::format_to(ctx.out(), "{} {}", *p.type, p.name);
    else return fmt::format_to(ctx.out(), "<null-type> {}", p.name);
}
