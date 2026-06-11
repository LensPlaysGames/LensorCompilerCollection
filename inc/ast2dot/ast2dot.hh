// AST To DOT
//
// Basically, just provide const name() and children() methods on your AST
// node, and this will draw a graph representing the tree in the DOT
// language.

#ifndef LCC_AST2DOT_HH
#define LCC_AST2DOT_HH

#include <concepts>
#include <string>
#include <string_view>
#include <unordered_map>
#include <vector>

#include <fmt/format.h>

namespace ast2dot {

namespace {

template <typename T>
concept ast2dot_node_has_name = requires (T node) {
    node.name();
    requires std::convertible_to<decltype(node.name()), std::string_view>;
};
template <typename T>
concept ast2dot_node_has_children = requires (T node) {
    node.children();
    requires std::convertible_to<decltype(node.children()), std::vector<T*>>;
};
template <typename T>
concept ast2dot_node_requirements
    = ast2dot_node_has_name<T> and ast2dot_node_has_children<T>;

template <typename T>
using IdMap = std::unordered_map<const T*, unsigned int>;

template <typename T>
requires ast2dot_node_requirements<T>
void identify_node(IdMap<T>& ids, const T& node) {
    if (not ids.contains(&node))
        ids[&node] = (unsigned int) ids.size();
}

template <typename T>
requires ast2dot_node_requirements<T>
void to_graph_visitor(
    IdMap<T>& ids,
    std::string& out,
    const T& node
) {
    identify_node(ids, node);
    out += fmt::format("  {} [label=\"{}\"]\n", ids[&node], node.name());
    for (auto* c : node.children()) {
        const auto& child_node = *c;
        identify_node(ids, child_node);
        out += fmt::format("  {} -- {}\n", ids[&node], ids[&child_node]);
        to_graph_visitor(ids, out, child_node);
    }
}

} // namespace

template <typename T>
requires ast2dot_node_requirements<T>
auto to_graph(const T& node) -> std::string {
    std::string out{
        "strict graph {\n"
    };
    IdMap<T> ids{};
    to_graph_visitor(ids, out, node);
    out += "}\n";
    return out;
}

} // namespace ast2dot

#endif /* LCC_AST2DOT_HH */
