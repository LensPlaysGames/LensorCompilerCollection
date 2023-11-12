#ifndef LCC_IR_DOMTREE_HH
#define LCC_IR_DOMTREE_HH

#include <lcc/ir/ir.hh>

namespace lcc {
class DomTree {
    /// The function for which this dominator tree was built.
    Function* f;

    /// IDOMs.
    Buffer<usz> idoms{f->blocks().size()};

public:
    /// Compute the dominator tree for a function.
    DomTree(Function* f);


    /// Get a representation of this dominator tree in the DOT format.
    auto debug() -> std::string;
};
}

#endif // LCC_IR_DOMTREE_HH
