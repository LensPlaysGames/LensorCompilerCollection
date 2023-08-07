#ifndef LCC_DISPLAY_HH
#define LCC_DISPLAY_HH

#include <lcc/utils.hh>

namespace lcc::utils::ast {
/// A node that can print its children.
template <typename NodeType>
concept PrintableWithChildren = requires (const NodeType* n, std::string& out) {
    /// Prints the name of this node and additional data.
    ///
    /// This must not print more than one line of text. If that
    /// is necessary, define print_trailing() instead and use that.
    n->print_header(out, /** Use colours **/ true);

    /// Returns a span of pointers to the children of this node.
    { n->children() } -> std::same_as<std::span<NodeType* const>>;
};

/// A node that has trailing data.
template <typename NodeType>
concept PrintableWithTrailing = requires (const NodeType* n, std::string& out) {
    /// Prints the trailing data of this node.
    ///
    /// The second parameter is leading text, which needs to be printed
    /// at the beginning of each line. The last line printed should end
    /// with a new line.
    n->print_trailing_data(
        out,
        /** Leading text **/ std::string_view{},
        /** Use colours **/ true
    );
};

/// Concept that checks whether an AST node is printable.
template <typename NodeType>
concept Printable = PrintableWithChildren<NodeType> or PrintableWithTrailing<NodeType>;

/// A node that has a location.
template <typename NodeType>
concept HasLocation = requires (const NodeType* n) {
    /// Returns the start position of the location.
    { n->location().start() } -> std::convertible_to<usz>;
};

/// A type that supports a `print()` function.
template <typename Type>
concept PrintableType = requires (const Type* t, std::string& out) {
    /// Prints the type.
    t->print(out, /** Use colours **/ true);
};

/// Print basic information about an AST node.
///
/// This is intended to serve as a default implementation
/// for printing AST nodes that don’t have any special
/// information to print. This can be used to implement
/// the \c print_header function required by the API.
///
/// The information is displayed in the following format:
///
///     <name> <address> "<" <location> ">" [ <type> ]
///
/// \param node The node to print.
/// \param type The type of the node; if null, this is omitted.
/// \param use_colour Whether to use colour.
template <HasLocation Node, PrintableType Type>
void PrintBasicNode(
    std::string& out,
    std::string_view node_name,
    const Node* node,
    const Type* type,
    bool use_colour
) {
    out += fmt::format(
        "{}{} {}{} {}<{}>\n",
        use_colour ? "\033[31m" : "",
        node_name,
        use_colour ? "\033[34m" : "",
        fmt::ptr(node),
        use_colour ? "\033[35m" : "",
        node->location().start()
    );

    /// Print the type if there is one.
    if (type) {
        if (use_colour) out += "\033[36m";
        type->print(out, use_colour);
    }
}

/// \brief Print an AST node and its children.
///
/// The node needs to satisfy either the \c PrintableWithChildren
/// concept or the \c PrintableWithTrailing concept.
///
/// \tparam NodeType The root of the AST node class hierarchy.
/// \param node The node to print.
template <Printable NodeType>
void Print(const NodeType* node, bool use_colour);

/// Implementation namespace.
namespace detail {
template <Printable NodeType>
void PrintImpl(std::string& out, const NodeType* node, std::string leading_text, bool use_colour) {
    /// Always reset colour first.
    if (use_colour) out += "\033[m";

    /// Print node header.
    if constexpr (PrintableWithChildren<NodeType>) {
        node->print_header(out);
        out += "\033[m\n";

        /// Print the children if there are any.
        auto children = node->children();
        for (usz i = 0; i < children.size(); i++) {
            const bool last = i == children.size() - 1;

            /// Print the leading text.
            out += fmt::format("\033[31m{}{}", leading_text, last ? "└─" : "├─");

            /// Print the child.
            PrintImpl(out, children[i], leading_text + (last ? "  " : "│ "), use_colour);
        }
    }

    /// Tell the node to print itself.
    else if constexpr (PrintableWithTrailing<NodeType>) {
        node->print_trailing(out, leading_text);
        out += "\033[m";
    }

    /// Try using a fallback implementation.
    else {
        static_assert(always_false<NodeType>, "Unreachable");
    }
}
} // namespace detail

template <Printable NodeType>
void Print(const NodeType* node, bool use_colour) {
    std::string out;
    PrintImpl<NodeType>(out, node, "", use_colour);
    fmt::print("{}", std::move(out));
}
} // namespace lcc::utils::ast

#endif // LCC_DISPLAY_HH
