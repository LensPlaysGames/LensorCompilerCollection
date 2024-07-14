#include <lcc/ir/domtree.hh>

namespace lcc {
namespace {
class DisjointSets {
    Buffer<usz> entries;

public:
    explicit DisjointSets(usz entry_count) : entries(entry_count) {
        rgs::generate(entries, [i = 0] mutable { return i++; });
    }

    /// Get all elements of a set.
    auto elements(usz a) -> Generator<usz> {
        for (auto root = find(a); auto [i, e] : vws::enumerate(entries))
            if (find(e) == root)
                co_yield usz(i);
    }

    usz find(usz a) {
        if (entries[a] == a) return a;
        return entries[a] = find(entries[a]);
    }

    void unite(usz a, usz b) {
        auto aset = find(a);
        auto bset = find(b);
        entries[bset] = aset;
    }
};

/// Context for computing the dominator tree of a function.
///
/// The first node in the list is assumed to be the entry node.
/// If the function contains no blocks, nothing is done.
///
/// The algorithm used here is Algorithm GD, version 3, described
/// in Fraczak et. al, Journal of Discrete Algorithms 23, (2013).
///
/// It uses a disjoint-sets data structure to coupled with DFS to
/// build the dominator tree in (almost) linear time without having
/// to bother with the concept of semidominators.
///
/// Note that the paper contains several errors, so make sure to
/// also consult ‘Corrections to “Finding dominators via disjoint
/// set union”’ (2014) by the same authors.
///
/// For a correctness proof and an in-depth explanation of
/// the algorithm, see the paper mentioned above.
struct DomTreeBuilder {
    Buffer<usz>& idoms;
    Buffer<std::vector<usz>>& children;
    Function* f;

    /// Flags indicating whether we’ve already visited a node.
    Buffer<bool> visited{f->blocks().size()};

    /// DFS parents of each node.
    Buffer<usz> parents{f->blocks().size()};

    /// Number of unmarked arcs for each node.
    Buffer<usz> total{f->blocks().size()};

    /// Number of unmarked arcs added in the inner loop for each node.
    Buffer<usz> added{f->blocks().size()};

    /// Nearest common ancestors.
    DisjointSets ncas{f->blocks().size()};

    /// Mapping from original vertices to contracted vertices.
    DisjointSets contr{f->blocks().size()};

    /// Sets of vertices with the same immediate dominator.
    DisjointSets same{f->blocks().size()};

    /// List of outgoing arcs for each vertex.
    std::unordered_multimap<usz, usz> out{};

    /// List of incoming arcs for each vertex.
    std::unordered_multimap<usz, usz> in{};

    /// For each vertex v, the list of arcs (a, b) such that v = nca(a, b).
    std::unordered_multimap<usz, std::pair<usz, usz>> arcs{};

    void Build() {
        idoms[0] = 0;

        for (auto [i, u] : vws::enumerate(f->blocks()))
            total[usz(i)] = u->predecessor_count();

        DFS(0);
    }

private:
    /// Merge two ranges in a multimap.
    void Merge(decltype(out)& map, usz x, usz v) {
        auto [v1, vn] = map.equal_range(v);
        for (auto [_, e] : rgs::subrange(v1, vn)) map.emplace(x, e);
    }

    void DFS(usz u) {
        Previsit(u);

        for (auto s : f->blocks()[u]->successors()) {
            auto v = s->id();
            if (not visited[v]) {
                DFS(v);
                parents[v] = u;
                ncas.unite(u, v);
            }

            arcs.emplace(ncas.find(v), std::pair{u, v});
        }

        Postvisit(u);
    }

    void Previsit(usz u) {
        visited[u] = true;
    }

    void Postvisit(usz u) {
        auto [fst, lst] = arcs.equal_range(u);
        for (auto [_, arc] : rgs::subrange(fst, lst)) {
            auto [x, y] = arc;
            out.emplace(contr.find(x), y);
            in.emplace(contr.find(y), x);
            added[contr.find(y)]++;
        }

        while (out.contains(u)) {
            auto it = out.find(u);
            auto v = contr.find(it->second);
            out.erase(it);

            if (v != u) {
                total[v]--;
                added[v]--;

                if (total[v] == 0) {
                    auto x = contr.find(parents[v]);
                    if (u == x) {
                        for (auto w : same.elements(v)) {
                            idoms[w] = u;
                            children[u].push_back(w);
                        }
                    } else {
                        same.unite(x, v);
                    }

                    contr.unite(parents[v], v);
                    Merge(out, x, v);
                }
            }
        }

        while (in.contains(u)) {
            auto it = in.find(u);
            auto v = contr.find(it->second);
            in.erase(it);

            while (v != u) {
                same.unite(u, v);
                auto x = contr.find(parents[v]);
                contr.unite(parents[v], v);
                Merge(in, x, v);
                Merge(out, x, v);
                total[x] += total[v];
                added[x] += added[v];
                v = x;
            }

            total[u] -= added[u];
            added[u] = 0;
        }
    }
};
} // namespace
} // namespace lcc

lcc::DomTree::DomTree(Function* f, bool compute_dominance_frontiers) : f(f) {
    if (f->blocks().empty()) return;

    /// Build dominator tree.
    DomTreeBuilder{idoms, children, f}.Build();

    /// Compute dominance frontiers.
    if (not compute_dominance_frontiers) return;
    for (auto [i, a] : vws::enumerate(f->blocks())) {
        for (auto b : a->successors()) {
            for (Block* x = a; not strictly_dominates(x, b);) {
                auto xid = x->id();
                df[xid].push_back(b);
                x = f->blocks()[idoms[xid]];
            }
        }
    }
}

auto lcc::DomTree::debug() const -> std::string {
    std::string out;
    out += fmt::format("digraph {} {{\n", f->names().at(0).name);
    for (auto [i, _] : vws::enumerate(f->blocks()))
        out += fmt::format("    {} [label=\"bb{}\"];\n", i, i);
    for (auto [i, _] : vws::enumerate(f->blocks()))
        out += fmt::format("    {} -> {};\n", isz(idoms[usz(i)]), i);
    out += "}\n";
    return out;
}
