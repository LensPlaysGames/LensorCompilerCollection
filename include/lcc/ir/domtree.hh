#ifndef LCC_IR_DOMTREE_HH
#define LCC_IR_DOMTREE_HH

#include <lcc/ir/core.hh>

namespace lcc {
class DomTree {
    static constexpr usz RootId = 0;

    /// The function for which this dominator tree was built.
    Function* f;

    /// IDOMs.
    Buffer<usz> idoms{f->blocks().size(), -1zu};

    /// Dominance frontier of each node.
    Buffer<std::vector<Block*>> df{f->blocks().size()};

    /// Children of each node.
    Buffer<std::vector<usz>> children{f->blocks().size()};

public:
    /// Compute the dominator tree for a function.
    DomTree(Function* f, bool compute_dominance_frontiers = true);

    /// Get a representation of this dominator tree in the DOT format.
    auto debug() const -> std::string;

    /// Traverse the dominator tree in DFS order.
    auto dfs_preorder() const -> Generator<Block*> {
        std::vector<Block*> stack;
        Buffer<bool> visited{f->blocks().size()};
        stack.push_back(root());
        visited[RootId] = true;
        while (not stack.empty()) {
            auto b = stack.back();
            stack.pop_back();
            co_yield b;
            for (auto c : children[b->id()]) {
                if (not visited[c]) {
                    stack.push_back(f->blocks()[c]);
                    visited[c] = true;
                }
            }
        }
    }

    /// Get the dominance frontier of a block.
    auto dom_frontier(Block* b) const -> const std::vector<Block*>& { return df[b->id()]; }

    /// Check if a block dominates another.
    auto dominates(Block* dominator, Block* b) const -> bool {
        if (dominator == b) return true;
        return strictly_dominates(dominator, b);
    }

    /// Get the iterated dominance frontier of a set of blocks.
    auto iterated_dom_frontier(rgs::range auto&& blocks) const -> std::unordered_set<Block*> {
        std::unordered_set<Block*> visited;
        std::unordered_set<Block*> idf;

        auto Add = [&](auto& self, Block* x) {
            if (visited.contains(x)) return;
            visited.insert(x);
            auto frontier = dom_frontier(x);
            idf.insert(frontier.begin(), frontier.end());
            for (auto b : frontier) { self(self, b); }
        };

        for (auto b : std::forward<decltype(blocks)>(blocks)) Add(Add, b);
        return idf;
    }

    /// Walk the parents of a block in the dominator tree. If the
    /// block is the root, no values are returned.
    auto parents(Block* of) -> Generator<Block*> {
        for (auto i = of->id(); i != RootId; ) {
            i = idoms[i];
            co_yield f->blocks()[i];
        }
    }

    /// Get the root of the tree.
    auto root() const -> Block* { return f->entry(); }

    /// Check if a block strictly dominates another.
    auto strictly_dominates(Block* dominator, Block* b) const -> bool {
        /// The root strictly dominates everything except itself.
        if (dominator == b) return false;
        if (dominator == root()) return true;

        /// Walk up the dominator tree starting at b’s idom until
        /// we find the dominator or reach the root.
        usz d = dominator->id();
        usz i = b->id();
        do {
            i = idoms[i];
            if (i == d) return true;
        } while (i != RootId);
        return false;
    }
};
} // namespace lcc

#endif // LCC_IR_DOMTREE_HH
