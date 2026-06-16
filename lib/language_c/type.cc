#include <language_c/type.hh>

#include <lcc/diags.hh>
#include <lcc/target.hh>
#include <lcc/utils/result.hh>

#include <fmt/base.h>
#include <fmt/format.h>
#include <fmt/std.h>

namespace lcc::language_c {
usz Type::size_bits(const Target* target) const {
    if (not target) Diag::ICE("nullptr argument");
    switch (kind()) {
        case TypeKind::Bool:
            return target->ffi.size_of_bool;
        case TypeKind::Char:
            return target->ffi.size_of_char;
        case TypeKind::Short:
            return target->ffi.size_of_short;
        case TypeKind::Int:
            return target->ffi.size_of_int;
        case TypeKind::Long:
            return target->ffi.size_of_long;
        case TypeKind::LongLong:
            return target->ffi.size_of_long_long;
        case TypeKind::Pointer:
            return target->size_of_pointer;

        case TypeKind::Invalid:
        case TypeKind::Void:
        case TypeKind::Function:
        case TypeKind::Array:
        case TypeKind::Count:
            break;
    }
    Diag::ICE("unreachable");
}
} // namespace lcc::language_c

auto fmt::formatter<lcc::language_c::Type>::format(
    const lcc::language_c::Type& t,
    format_context& ctx
) const -> format_context::iterator {
    switch (t.kind()) {
        case lcc::language_c::TypeKind::Bool:
            return fmt::format_to(ctx.out(), "bool");
        case lcc::language_c::TypeKind::Char:
            return fmt::format_to(ctx.out(), "char");
        case lcc::language_c::TypeKind::Short:
            return fmt::format_to(ctx.out(), "short");
        case lcc::language_c::TypeKind::Int:
            return fmt::format_to(ctx.out(), "int");
        case lcc::language_c::TypeKind::Long:
            return fmt::format_to(ctx.out(), "long");
        case lcc::language_c::TypeKind::LongLong:
            return fmt::format_to(ctx.out(), "long long");

        case lcc::language_c::TypeKind::Void:
            return fmt::format_to(ctx.out(), "void");
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

auto fmt::formatter<lcc::language_c::FunctionType::Parameter>::format(
    const lcc::language_c::FunctionType::Parameter& p,
    format_context& ctx
) const -> format_context::iterator {
    if (p.type)
        return fmt::format_to(ctx.out(), "{} {}", *p.type, p.name);
    else return fmt::format_to(ctx.out(), "<null-type> {}", p.name);
}

auto fmt::formatter<lcc::Result<lcc::language_c::Type*>>::format(
    lcc::Result<lcc::language_c::Type*>& r,
    format_context& ctx
) const -> format_context::iterator {
    if (r) return fmt::format_to(ctx.out(), "{}", **r);
    return fmt::format_to(ctx.out(), "<bad-type-result>");
}
