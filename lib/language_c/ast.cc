#include <language_c/ast.hh>
#include <language_c/parser.hh>

#include <lcc/enum_to_underlying.hh>

#include <lccbase/diags.hh>
#include <lccbase/location.hh>

#include <fmt/base.h>
#include <fmt/format.h>

#include <ranges>
#include <string_view>
#include <type_traits>
#include <vector>

auto fmt::formatter<lcc::language_c::Node>::indent(
    format_context::iterator out,
    size_t depth
) const -> format_context::iterator {
    return fmt::format_to(out, "{:{}}", "", depth * indent_width);
}

auto fmt::formatter<lcc::language_c::Node>::tag(
    format_context::iterator out,
    size_t depth,
    std::string_view tag
) const -> format_context::iterator {
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
            const auto& g = *(const lcc::language_c::Group*) &n;

            if (g.constituents().empty())
                return tag(ctx.out(), depth, "<group/>");

            tag(ctx.out(), depth, "<group>");
            ++depth;
            fmt::format_to(
                ctx.out(),
                "{}",
                fmt::join(
                    std::ranges::views::transform(
                        g.constituents(),
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
            const auto& b = *(const lcc::language_c::Block*) &n;

            if (b.constituents().empty())
                return tag(ctx.out(), depth, "<block/>");

            tag(ctx.out(), depth, "<block>");
            ++depth;
            fmt::format_to(
                ctx.out(),
                "{}",
                fmt::join(
                    std::ranges::views::transform(
                        b.constituents(),
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
            const auto& i = (*(lcc::language_c::IntegerLiteral*) &n);
            indent(ctx.out(), depth);
            return fmt::format_to(
                ctx.out(),
                "<integer-literal>{}</integer-literal>\n",
                i.value()
            );
        }

        case lcc::language_c::NodeKind::ArrayLiteral: {
            const auto& a = (*(lcc::language_c::ArrayLiteral*) &n);
            tag(ctx.out(), depth, "<array-literal>");
            ++depth;
            fmt::format_to(
                ctx.out(),
                "{}",
                fmt::join(
                    std::ranges::views::transform(
                        a.elements(),
                        [&](const lcc::language_c::Node* constituent) {
                            return fmt::format("{:{}}", *constituent, depth);
                        }
                    ),
                    "\n"
                )
            );
            --depth;
            return tag(ctx.out(), depth, "</array-literal>");
        }

        case lcc::language_c::NodeKind::Return: {
            const auto& r = *(lcc::language_c::Return*) &n;
            if (not r.expression())
                return tag(ctx.out(), depth, "<return/>");

            tag(ctx.out(), depth, "<return>");
            fmt::format_to(ctx.out(), "{:{}}", *r.expression(), depth + 1);
            return tag(ctx.out(), depth, "</return>");
        }

        case lcc::language_c::NodeKind::Declaration: {
            const auto& d = *(const lcc::language_c::Declaration*) &n;
            tag(ctx.out(), depth, "<declaration>");
            ++depth;

            indent(ctx.out(), depth);
            fmt::format_to(ctx.out(), "<name>{}</name>\n", d.name());

            indent(ctx.out(), depth);
            fmt::format_to(ctx.out(), "<type>{}</type>\n", *d.type());

            if (d.initialising_expression()) {
                tag(ctx.out(), depth, "<initial>");
                fmt::format_to(
                    ctx.out(),
                    "{:{}}",
                    *d.initialising_expression(),
                    depth + 1
                );
                tag(ctx.out(), depth, "</initial>");
            } else tag(ctx.out(), depth, "<initial/>\n");

            --depth;
            return tag(ctx.out(), depth, "</declaration>");
        }

        case lcc::language_c::NodeKind::UnaryOperation: {
            const auto& u = *(const lcc::language_c::UnaryOperation*) &n;
            tag(ctx.out(), depth, "<unary>");
            ++depth;

            indent(ctx.out(), depth);
            fmt::format_to(ctx.out(), "<operator>{}</operator>\n", u.unary_operator());

            fmt::format_to(ctx.out(), "{:{}}", *u.operand(), depth);

            --depth;
            return tag(ctx.out(), depth, "</unary>");
        }

        case lcc::language_c::NodeKind::BinaryOperation: {
            const auto& b = *(const lcc::language_c::BinaryOperation*) &n;
            tag(ctx.out(), depth, "<binary>");
            ++depth;

            indent(ctx.out(), depth);
            fmt::format_to(ctx.out(), "<operator>{}</operator>\n", b.binary_operator());

            tag(ctx.out(), depth, "<lhs>");
            fmt::format_to(ctx.out(), "{:{}}", *b.lhs(), depth + 1);
            tag(ctx.out(), depth, "</lhs>");
            tag(ctx.out(), depth, "<rhs>");
            fmt::format_to(ctx.out(), "{:{}}", *b.rhs(), depth + 1);
            tag(ctx.out(), depth, "</rhs>");

            --depth;
            return tag(ctx.out(), depth, "</binary>");
        }

        case lcc::language_c::NodeKind::NameReference: {
            const auto& name_ref = *(const lcc::language_c::NameReference*) &n;
            indent(ctx.out(), depth);
            return fmt::format_to(
                ctx.out(),
                "<name>{}</name>\n",
                name_ref.name()
            );
        }

        case lcc::language_c::NodeKind::Call: {
            const auto& call = *(const lcc::language_c::Call*) &n;
            tag(ctx.out(), depth, "<call>");
            ++depth;
            tag(ctx.out(), depth, "<callee>");
            fmt::format_to(ctx.out(), "{:{}}", *call.callee(), depth + 1);
            tag(ctx.out(), depth, "</callee>");
            tag(ctx.out(), depth, "<arguments>");
            for (const auto* arg : call.arguments())
                fmt::format_to(ctx.out(), "{:{}}\n", *arg, depth + 1);
            tag(ctx.out(), depth, "</arguments>");
            --depth;
            return tag(ctx.out(), depth, "</call>");
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

auto Node::name() const -> std::string_view {
    switch (kind()) {
        case NodeKind::Invalid: return "invalid";
        case NodeKind::Group: return "group";
        case NodeKind::Block: return "block";
        case NodeKind::NameReference: return "name";
        case NodeKind::Declaration: return "declaration";
        case NodeKind::ArrayLiteral: return "array_literal";
        case NodeKind::IntegerLiteral: return "integer_literal";
        case NodeKind::Call: return "call";
        case NodeKind::Return: return "return";
        case NodeKind::UnaryOperation: {
            auto* u = ((UnaryOperation*) this);
            switch (u->unary_operator()) {
                case TokenKind::OpPlus: return "unary_positive";
                case TokenKind::OpMinus: return "unary_negative";
                case TokenKind::OpAsterisk: return "unary_dereference";
                case TokenKind::OpAmpersand: return "binary_addressof";
                case TokenKind::OpTilde: return "unary_bit_negation";
                case TokenKind::OpPlusPlus: return "unary_increment";
                case TokenKind::OpMinusMinus: return "unary_decrement";
                case TokenKind::OpExclamation: return "unary_logical_not";

                case TokenKind::OpSlash:
                case TokenKind::OpPercent:
                case TokenKind::LeftSquareBracket:
                case TokenKind::OpEqual:
                case TokenKind::OpLessThan:
                case TokenKind::OpGreaterThan:
                case TokenKind::OpDoublePipe:
                case TokenKind::OpDoubleAmpersand:
                case TokenKind::OpDot:
                case TokenKind::OpArrow:
                case TokenKind::OpCaret:
                case TokenKind::OpPipe:
                case TokenKind::OpShiftLeft:
                case TokenKind::OpShiftRight:
                case TokenKind::OpDoubleEqual:
                case TokenKind::OpLessThanEqual:
                case TokenKind::OpGreaterThanEqual:
                case TokenKind::OpExclamationEqual:
                case TokenKind::OpPlusEqual:
                case TokenKind::OpMinusEqual:
                case TokenKind::OpAsteriskEqual:
                case TokenKind::OpSlashEqual:
                case TokenKind::OpPercentEqual:
                case TokenKind::OpCaretEqual:
                case TokenKind::OpPipeEqual:
                case TokenKind::OpAmpersandEqual:
                case TokenKind::OpShiftLeftEqual:
                case TokenKind::OpShiftRightEqual:
                case TokenKind::Invalid:
                case TokenKind::Identifier:
                case TokenKind::Integer:
                case TokenKind::Fractional:
                case TokenKind::String:
                case TokenKind::KwVoid:
                case TokenKind::KwBool:
                case TokenKind::KwChar:
                case TokenKind::KwShort:
                case TokenKind::KwInt:
                case TokenKind::KwLong:
                case TokenKind::KwReturn:
                case TokenKind::KwSizeof:
                case TokenKind::KwAlignof:
                case TokenKind::KwConst:
                case TokenKind::KwVolatile:
                case TokenKind::KwRestrict:
                case TokenKind::KwAtomic:
                case TokenKind::KwConstexpr:
                case TokenKind::KwAuto:
                case TokenKind::KwExtern:
                case TokenKind::KwRegister:
                case TokenKind::KwStatic:
                case TokenKind::OpComma:
                case TokenKind::LeftParenthesis:
                case TokenKind::RightParenthesis:
                case TokenKind::RightSquareBracket:
                case TokenKind::LeftCurlyBrace:
                case TokenKind::RightCurlyBrace:
                case TokenKind::Semicolon:
                case TokenKind::Eof:
                case TokenKind::Count:
                    Diag::ICE("Invalid unary operator `{}`", u->unary_operator());
            }
            Diag::ICE("unreachable");
        }

        case NodeKind::BinaryOperation: {
            auto* b = ((BinaryOperation*) this);
            switch (b->binary_operator()) {
                case TokenKind::OpPlus: return "binary_add";
                case TokenKind::OpMinus: return "binary_subtract";
                case TokenKind::OpAsterisk: return "binary_multiply";
                case TokenKind::OpSlash: return "binary_divide";
                case TokenKind::OpPercent: return "binary_remainder";
                case TokenKind::LeftSquareBracket: return "binary_subscript";

                case TokenKind::OpEqual: return "binary_assign";
                case TokenKind::OpLessThan: return "binary_less_than";
                case TokenKind::OpGreaterThan: return "binary_greater_than";
                case TokenKind::OpDoublePipe: return "binary_logical_or";
                case TokenKind::OpDoubleAmpersand: return "binary_logical_and";
                case TokenKind::OpDot: return "binary_dot";
                case TokenKind::OpArrow: return "binary_arrow";
                case TokenKind::OpCaret: return "binary_bit_xor";
                case TokenKind::OpPipe: return "binary_bit_or";
                case TokenKind::OpAmpersand: return "binary_bit_and";
                case TokenKind::OpShiftLeft: return "binary_bitshift_left";
                case TokenKind::OpShiftRight: return "binary_bitshift_right";
                case TokenKind::OpDoubleEqual: return "binary_equality";
                case TokenKind::OpLessThanEqual: return "binary__assign";
                case TokenKind::OpGreaterThanEqual: return "binary__assign";
                case TokenKind::OpExclamationEqual: return "binary_inequality";
                case TokenKind::OpPlusEqual: return "binary_add_assign";
                case TokenKind::OpMinusEqual: return "binary_subtract_assign";
                case TokenKind::OpAsteriskEqual: return "binary_multiply_assign";
                case TokenKind::OpSlashEqual: return "binary_divide_assign";
                case TokenKind::OpPercentEqual: return "binary_remainder_assign";
                case TokenKind::OpCaretEqual: return "binary_bit_xor_assign";
                case TokenKind::OpPipeEqual: return "binary_bit_or_assign";
                case TokenKind::OpAmpersandEqual: return "binary_bit_and_assign";
                case TokenKind::OpShiftLeftEqual: return "binary_bitshift_left_assign";
                case TokenKind::OpShiftRightEqual: return "binary_bitshift_right_assign";

                case TokenKind::Invalid:
                case TokenKind::Identifier:
                case TokenKind::Integer:
                case TokenKind::Fractional:
                case TokenKind::String:
                case TokenKind::KwVoid:
                case TokenKind::KwBool:
                case TokenKind::KwChar:
                case TokenKind::KwShort:
                case TokenKind::KwInt:
                case TokenKind::KwLong:
                case TokenKind::KwReturn:
                case TokenKind::KwSizeof:
                case TokenKind::KwAlignof:
                case TokenKind::KwConst:
                case TokenKind::KwVolatile:
                case TokenKind::KwRestrict:
                case TokenKind::KwAtomic:
                case TokenKind::KwConstexpr:
                case TokenKind::KwAuto:
                case TokenKind::KwExtern:
                case TokenKind::KwRegister:
                case TokenKind::KwStatic:
                case TokenKind::OpPlusPlus:
                case TokenKind::OpMinusMinus:
                case TokenKind::OpComma:
                case TokenKind::OpExclamation:
                case TokenKind::OpTilde:
                case TokenKind::LeftParenthesis:
                case TokenKind::RightParenthesis:
                case TokenKind::RightSquareBracket:
                case TokenKind::LeftCurlyBrace:
                case TokenKind::RightCurlyBrace:
                case TokenKind::Semicolon:
                case TokenKind::Eof:
                case TokenKind::Count:
                    Diag::ICE("Invalid binary operator `{}`", b->binary_operator());
            }
            Diag::ICE("unreachable");
        }
        case NodeKind::Count: break;
    }
    Diag::ICE("unreachable: invalid node kind {}", +kind());
}
auto Node::children() const -> std::vector<Node*> {
    std::vector<Node*> out{};
    for (auto& c : children_ref())
        out.emplace_back(*c);

    return out;
}

auto Node::children_ref() const -> std::vector<Node**> {
    switch (kind()) {
        case NodeKind::IntegerLiteral:
        case NodeKind::NameReference:
            return {};

        case NodeKind::Group: {
            std::vector<Node**> out{};
            for (auto& elem : ((Group*) this)->_constituents)
                out.emplace_back(&elem);
            return out;
        }
        case NodeKind::Block: {
            std::vector<Node**> out{};
            for (auto& elem : ((Block*) this)->_constituents)
                out.emplace_back(&elem);
            return out;
        }

        case NodeKind::ArrayLiteral: {
            std::vector<Node**> out{};
            for (auto& elem : ((ArrayLiteral*) this)->_elements)
                out.emplace_back(&elem);
            return out;
        }

        case NodeKind::Call: {
            auto* c = (Call*) this;
            std::vector<Node**> out{&c->_callee};
            for (auto& arg : c->_arguments)
                out.emplace_back(&arg);
            return out;
        }

        case NodeKind::UnaryOperation: {
            auto* b = (UnaryOperation*) this;
            return {&b->_operand};
        }

        case NodeKind::BinaryOperation: {
            auto* b = (BinaryOperation*) this;
            return {&b->_lhs, &b->_rhs};
        }

        case NodeKind::Declaration: {
            auto d = (Declaration*) this;
            if (d->_initialising_expression)
                return {&d->_initialising_expression};
            return {};
        };
        case NodeKind::Return: {
            auto r = ((Return*) this);
            if (r->_expression)
                return {&r->_expression};
            return {};
        }

        case NodeKind::Invalid:
        case NodeKind::Count: break;
    }
    Diag::ICE("unreachable");
}

auto Node::MaybeToGroup(TranslationUnit& tu, std::vector<Node*> nodes) -> Node* {
    if (nodes.size() > 1) {
        return new (tu) Group(
            nodes,
            {nodes.front()->location(), nodes.back()->location()}
        );
    }

    if (nodes.size() == 1)
        return nodes.at(0);

    return nullptr;
}

} // namespace lcc::language_c
