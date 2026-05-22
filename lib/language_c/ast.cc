#include <language_c/ast.hh>

#include <lcc/diags.hh>

#include <fmt/base.h>
#include <fmt/format.h>
#include <type_traits>

auto fmt::formatter<lcc::language_c::Node>::format(
    const lcc::language_c::Node& n,
    format_context& ctx
) const -> format_context::iterator {
    size_t depth = 0;

    if (depth_arg_id != -1) {
        ctx.arg(depth_arg_id).visit([&depth](auto value) {
            if constexpr (std::is_integral_v<decltype(value)>)
                depth = static_cast<size_t>(value);
        });
    }

    fmt::format_to(ctx.out(), "{:{}}", "", depth * indent_width);

    switch (n.kind()) {
        case lcc::language_c::NodeKind::Invalid:
            return fmt::format_to(ctx.out(), "<invalid>");

        case lcc::language_c::NodeKind::Group: {
            return fmt::format_to(
                ctx.out(),
                "<group>\n{}</group>\n",
                fmt::join(
                    std::ranges::views::transform(
                        ((lcc::language_c::Group*) &n)->constituents(),
                        [&](const lcc::language_c::Node* constituent) {
                            return fmt::format("{:{}}", *constituent, depth + 1);
                        }
                    ),
                    "\n"
                )
            );
        }

        case lcc::language_c::NodeKind::Block: {
            return fmt::format_to(
                ctx.out(),
                "<block>\n{}</block>\n",
                fmt::join(
                    std::ranges::views::transform(
                        ((lcc::language_c::Group*) &n)->constituents(),
                        [&](const lcc::language_c::Node* constituent) {
                            return fmt::format("{:{}}", *constituent, depth + 1);
                        }
                    ),
                    "\n"
                )
            );
        }

        case lcc::language_c::NodeKind::IntegerLiteral: {
            return fmt::format_to(
                ctx.out(),
                "<integer-literal>{}</integer-literal>\n",
                (*(lcc::language_c::IntegerLiteral*) &n).value()
            );
        }

        case lcc::language_c::NodeKind::Return: {
            const auto& r = (*(lcc::language_c::Return*) &n);
            if (r.expression()) {
                return fmt::format_to(
                    ctx.out(),
                    "<return>{:{}}</return>",
                    *r.expression(),
                    depth + 1
                );
            }
            return fmt::format_to(ctx.out(), "<return/>");
        }

        case lcc::language_c::NodeKind::Declaration: {
            const auto* d = (const lcc::language_c::Declaration*) &n;
            fmt::format_to(ctx.out(), "<declaration>\n");

            ++depth;

            fmt::format_to(ctx.out(), "{:{}}", "", depth * indent_width);
            fmt::format_to(ctx.out(), "<name>{}</name>\n", d->name());

            fmt::format_to(ctx.out(), "{:{}}", "", depth * indent_width);
            fmt::format_to(ctx.out(), "<type>{}</type>\n", *d->type());

            --depth;

            fmt::format_to(ctx.out(), "{:{}}", "", depth * indent_width);
            fmt::format_to(ctx.out(), "</declaration>\n");
            return ctx.out();
        }

        case lcc::language_c::NodeKind::Count:
            break;
    }
    lcc::Diag::ICE("unreachable");
}
