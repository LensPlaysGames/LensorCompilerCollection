#ifndef LCC_AST_PRINTER_HH
#define LCC_AST_PRINTER_HH

#include <lcc/core.hh>
#include <lcc/utils.hh>

#include <span>
#include <string>
#include <string_view>

namespace lcc::utils {
template <typename Derived, typename NodeTy, typename TypeTy>
struct ASTPrinter {
    using enum Colour;
    using NodeType = NodeTy;
    using TypeType = TypeTy;

    static constexpr Colour base_colour{White};
    static constexpr Colour name_colour{Reset};

    std::string out;
    bool use_colour = true;
    Colours C{use_colour};

    ASTPrinter(bool should_use_colour) : use_colour{should_use_colour} {}
    ~ASTPrinter() {
        if (not out.empty()) fmt::print("{}{}", out, C(Reset));
    }

    /// Print basic information about an AST node.
    void PrintBasicNode(
        std::string_view node_name,
        const NodeType* node,
        const TypeType* type,
        bool print_newline = true
    ) {
        PrintBasicHeader(node_name, node);

        /// Print the type if there is one.
        if (type) out += fmt::format(" {}", type->string(use_colour));
        out += fmt::format("{}", C(Reset));
        if (print_newline) out += "\n";
    }

    /// Print the start of the header of an AST node.
    /// Example: IfExpr <69>
    void PrintBasicHeader(std::string_view node_name, const NodeType* node) {
        out += fmt::format(
            "{}{} {}<{}>",
            C(name_colour),
            node_name,
            C(base_colour),
            node->location().pos
        );
    }

    /// Print the linkage of a node.
    void PrintLinkage(lcc::Linkage l) {
        out += C(name_colour);
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
    template <typename TypeToPrint = NodeType>
    void PrintChildren(std::span<TypeToPrint* const> exprs, std::string leading_text, bool handle_last = true) {
        for (lcc::usz i = 0; i < exprs.size(); i++) {
            const bool last = handle_last and i == exprs.size() - 1;

            /// Print the leading text.
            out += fmt::format("{}{}{}", C(base_colour), leading_text, last ? "└─" : "├─");

            /// Print the child.
            static_cast<Derived*>(this)->operator()(exprs[i], leading_text + (last ? "  " : "│ "));
        }
    }

    template <typename TypeToPrint = NodeType, usz n>
    void PrintChildren(TypeToPrint* (&exprs)[n], std::string leading_text, bool handle_last = true) {
        PrintChildren(std::span<TypeToPrint* const>{exprs, n}, std::move(leading_text), handle_last);
    }

    template <typename TypeToPrint = NodeType>
    void PrintChildren(const std::vector<TypeToPrint*>& vec, std::string leading_text, bool handle_last = true) {
        PrintChildren(std::span<TypeToPrint* const>{vec}, std::move(leading_text), handle_last);
    }
};

} // namespace lcc::utils

#endif // LCC_AST_PRINTER_HH
