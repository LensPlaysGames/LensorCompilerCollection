#include <language_c/ast.hh>

#include <lcc/diags.hh>
#include <lcc/location.hh>

#include <fmt/base.h>
#include <fmt/format.h>

#include <type_traits>

auto fmt::formatter<lcc::language_c::Node>::indent(format_context::iterator out, size_t depth) const
    -> format_context::iterator {
    return fmt::format_to(out, "{:{}}", "", depth * indent_width);
}

auto fmt::formatter<lcc::language_c::Node>::tag(format_context::iterator out, size_t depth, std::string_view tag) const
    -> format_context::iterator {
    indent(out, depth);
    return fmt::format_to(out, "{}\n", tag);
}

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

    switch (n.kind()) {
        case lcc::language_c::NodeKind::Invalid:
            return tag(ctx.out(), depth, "<invalid>");

        case lcc::language_c::NodeKind::Group: {
            const auto* g = (const lcc::language_c::Group*) &n;

            if (g->constituents().empty())
                return tag(ctx.out(), depth, "<group/>");

            tag(ctx.out(), depth, "<group>");
            ++depth;
            fmt::format_to(
                ctx.out(),
                "{}",
                fmt::join(
                    std::ranges::views::transform(
                        g->constituents(),
                        [&](const lcc::language_c::Node* constituent) {
                            return fmt::format("{:{}}", *constituent, depth);
                        }
                    ),
                    "\n"
                )
            );
            --depth;
            return tag(ctx.out(), depth, "</group>");
        }

        case lcc::language_c::NodeKind::Block: {
            const auto* b = (const lcc::language_c::Block*) &n;

            if (b->constituents().empty())
                return tag(ctx.out(), depth, "<block/>");

            tag(ctx.out(), depth, "<block>");
            ++depth;
            fmt::format_to(
                ctx.out(),
                "{}",
                fmt::join(
                    std::ranges::views::transform(
                        b->constituents(),
                        [&](const lcc::language_c::Node* constituent) {
                            return fmt::format("{:{}}", *constituent, depth);
                        }
                    ),
                    "\n"
                )
            );
            --depth;
            return tag(ctx.out(), depth, "</block>");
        }

        case lcc::language_c::NodeKind::IntegerLiteral: {
            indent(ctx.out(), depth);
            return fmt::format_to(
                ctx.out(),
                "<integer-literal>{}</integer-literal>\n",
                (*(lcc::language_c::IntegerLiteral*) &n).value()
            );
        }

        case lcc::language_c::NodeKind::Return: {
            const auto& r = (*(lcc::language_c::Return*) &n);
            if (not r.expression())
                return tag(ctx.out(), depth, "<return/>");

            tag(ctx.out(), depth, "<return>");
            fmt::format_to(ctx.out(), "{:{}}", *r.expression(), depth + 1);
            return tag(ctx.out(), depth, "</return>");
        }

        case lcc::language_c::NodeKind::Declaration: {
            const auto* d = (const lcc::language_c::Declaration*) &n;
            tag(ctx.out(), depth, "<declaration>");
            ++depth;

            indent(ctx.out(), depth);
            fmt::format_to(ctx.out(), "<name>{}</name>\n", d->name());

            indent(ctx.out(), depth);
            fmt::format_to(ctx.out(), "<type>{}</type>\n", *d->type());

            if (d->initialising_expression()) {
                tag(ctx.out(), depth, "<initial>");
                fmt::format_to(
                    ctx.out(),
                    "{:{}}",
                    *d->initialising_expression(),
                    depth + 1
                );
                tag(ctx.out(), depth, "</initial>");
            } else tag(ctx.out(), depth, "<initial/>\n");

            --depth;
            return tag(ctx.out(), depth, "</declaration>");
        }

        case lcc::language_c::NodeKind::Count:
            break;
    }
    lcc::Diag::ICE("unreachable");
}

namespace lcc::language_c {

auto Node::get_past_location() -> Location {
    Location out{location()};
    out.pos += out.len;
    out.len = 1;
    return out;
}

#ifdef LCC_LANGTEST
auto Node::langtest_name() -> std::string_view {
    switch (kind()) {
        case NodeKind::Invalid: return "invalid";
        case NodeKind::Group: return "group";
        case NodeKind::Block: return "block";
        case NodeKind::Declaration: return "declaration";
        case NodeKind::IntegerLiteral: return "integer_literal";
        case NodeKind::Return: return "return";
        case NodeKind::Count: break;
    }
    Diag::ICE("unreachable");
}
auto Node::langtest_children() -> std::vector<Node*> {
    switch (kind()) {
        case NodeKind::IntegerLiteral:
            return {};

        case NodeKind::Group: return ((Group*) this)->constituents();
        case NodeKind::Block: return ((Block*) this)->constituents();
        case NodeKind::Declaration: {
            auto d = (Declaration*) this;
            if (d->initialising_expression())
                return {d->initialising_expression()};
            return {};
        };
        case NodeKind::Return: {
            auto r = ((Return*) this);
            if (r->expression())
                return {r->expression()};
            return {};
        }

        case NodeKind::Invalid:
        case NodeKind::Count: break;
    }
    Diag::ICE("unreachable");
}
#endif

auto Node::MaybeToGroup(std::vector<Node*> nodes) -> Node* {
    if (nodes.size() > 1) {
        return new Group(
            nodes,
            {nodes.front()->location(), nodes.back()->location()}
        );
    }

    if (nodes.size() == 1)
        return nodes.at(0);

    return nullptr;
}

} // namespace lcc::language_c
