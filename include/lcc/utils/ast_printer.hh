#ifndef LCC_AST_PRINTER_HH
#define LCC_AST_PRINTER_HH

#include <lcc/core.hh>
#include <lcc/utils.hh>

namespace lcc::utils {
template <typename Derived, typename NodeTy, typename TypeTy>
struct ASTPrinter {
    using enum Colour;
    using NodeType = NodeTy;
    using TypeType = TypeTy;

    std::string out;
    bool use_colour = true;
    Colours C{use_colour};

    ASTPrinter(bool use_colour) : use_colour{use_colour} {}
    ~ASTPrinter() {
        if (not out.empty()) fmt::print("{}", out);
    }

    /// Print basic information about an AST node.
    void PrintBasicNode(
        std::string_view node_name,
        const NodeType* node,
        const TypeType* type
    ) {
        PrintBasicHeader(node_name, node);

        /// Print the type if there is one.
        if (type) out += fmt::format(" {}", type->string(use_colour));
        out += fmt::format("{}\n", C(Reset));
    }

    /// Print the start of the header of an AST node.
    /// Example: IfExpr 0xdeadbeef <0>
    void PrintBasicHeader(std::string_view node_name, const NodeType* node) {
        out += fmt::format(
            "{}{} {}{} {}<{}>",
            C(Red),
            node_name,
            C(Blue),
            fmt::ptr(node),
            C(Magenta),
            node->location().pos
        );
    }

    /// Print the linkage of a node.
    void PrintLinkage(lcc::Linkage l) {
        out += C(Red);
        switch (l) {
            case lcc::Linkage::LocalVar: out += "Local "; return;
            case lcc::Linkage::Internal: out += "Internal "; return;
            case lcc::Linkage::Used: out += "Used "; return;
            case lcc::Linkage::Exported: out += "Exported "; return;
            case lcc::Linkage::Imported: out += "Imported "; return;
            case lcc::Linkage::Reexported: out += "Reexported "; return;
        }
        LCC_UNREACHABLE();
    }


    /// Print the children of a node.
    void PrintChildren(std::span<NodeType* const> exprs, std::string leading_text = "") {
        for (lcc::usz i = 0; i < exprs.size(); i++) {
            const bool last = i == exprs.size() - 1;

            /// Print the leading text.
            out += fmt::format("{}{}{}", C(Red), leading_text, last ? "└─" : "├─");

            /// Print the child.
            static_cast<Derived*>(this)->operator()(exprs[i], leading_text + (last ? "  " : "│ "));
        }
    }
};

} // namespace lcc::utils

#endif // LCC_AST_PRINTER_HH
