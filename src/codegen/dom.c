#include <codegen/dom.h>
#include <codegen/intermediate_representation.h>

typedef VECTOR(IRBlock*) BlockVector;

/// Perform DFS on the control flow graph to find blocks
/// that are reachable from `block`. If `ignore` is encountered,
/// it is ignored. This allows us to effectively ‘remove’ blocks
/// from the CFG without having to actually remove them.
static BlockVector collect_reachable_blocks(IRBlock *block, IRBlock *ignore) {
  BlockVector reachable = {0};
  VECTOR_PUSH(reachable, block);

  /// Stack for DFS.
  VECTOR(IRBlock*) dfs_stack = {0};
  VECTOR_PUSH(dfs_stack, block);
  while (dfs_stack.size) {
    IRBlock *b = VECTOR_POP(dfs_stack);
    if (b == ignore) continue;

    STATIC_ASSERT(IR_COUNT == 26, "Handle all branch types");
    bool out = false;
    IRInstruction *i = b->instructions.last;
    switch (i->type) {
      default: break;
      case IR_BRANCH:
        VECTOR_CONTAINS(reachable, i->value.block, out);
        if (!out) {
          VECTOR_PUSH(reachable, i->value.block);
          VECTOR_PUSH(dfs_stack, i->value.block);
        }
        break;

      case IR_BRANCH_CONDITIONAL:
        VECTOR_CONTAINS(reachable, i->value.conditional_branch.true_branch, out);
        if (!out) {
          VECTOR_PUSH(reachable, i->value.conditional_branch.true_branch);
          VECTOR_PUSH(dfs_stack, i->value.conditional_branch.true_branch);
        }
        VECTOR_CONTAINS(reachable, i->value.conditional_branch.false_branch, out);
        if (!out) {
          VECTOR_PUSH(reachable, i->value.conditional_branch.false_branch);
          VECTOR_PUSH(dfs_stack, i->value.conditional_branch.false_branch);
        }
        break;
    }
  }
  VECTOR_DELETE(dfs_stack);
  return reachable;
}

void free_dominator_info(DominatorInfo* info) {
  VECTOR_FOREACH (DomTreeNode, n, info->nodes) {
    VECTOR_DELETE(n->dominators);
    VECTOR_DELETE(n->children);
  }
  VECTOR_DELETE(info->nodes);
}

/// Check if a node dominates another node.
bool dominates(DomTreeNode *dominator, DomTreeNode *node) {
  bool out = false;
  VECTOR_CONTAINS(node->dominators, dominator, out);
  return out;
}

/// Check if a node strictly dominates another node.
bool strictly_dominates(DomTreeNode *dominator, DomTreeNode *node) {
  return dominator != node && dominates(dominator, node);
}

/// Build the dominator tree for a function and remove unused blocks.
/// \see struct DominatorInfo
void build_dominator_tree(IRFunction *f, DominatorInfo* info, bool prune) {
  /// Remove any unreachable blocks.
  if (prune) {
    /// Determine all blocks that are reachable from the entry block.
    BlockVector reachable = collect_reachable_blocks(f->blocks.first, NULL);
    BlockVector blocks_to_remove = {0};

    DLIST_FOREACH (IRBlock*, b, f->blocks) {
      bool out = false;
      VECTOR_CONTAINS(reachable, b, out);
      if (!out) VECTOR_PUSH(blocks_to_remove, b);
    }

    /// Remove unreachable blocks and free vectors.
    VECTOR_FOREACH_PTR (IRBlock*, b, blocks_to_remove) ir_remove_and_free_block(b);
    VECTOR_DELETE(reachable);
    VECTOR_DELETE(blocks_to_remove);
  }

  /// Free old dominator tree.
  VECTOR_FOREACH (DomTreeNode, n, info->nodes) {
    VECTOR_DELETE(n->dominators);
    VECTOR_DELETE(n->children);
  }
  VECTOR_CLEAR(info->nodes);

  /// Add a node for each block.
  ASSERT(f->blocks.first);
  DLIST_FOREACH (IRBlock*, b, f->blocks) {
    DomTreeNode node = {0};
    node.block = b;
    VECTOR_PUSH(info->nodes, node);
  }

  /// The only dominator of the root is the root itself.
  /// We assume that the first block in a function is
  /// the entry block.
  VECTOR_PUSH(info->nodes.data[0].dominators, info->nodes.data);
  info->dominator_tree = info->nodes.data;

  /// To find all dominators of a block, remove that block
  /// from the function; then, find all blocks that are still
  /// reachable from the root. Any unreachable are dominated
  /// by the removed block.
  for (DomTreeNode *dominator = (info->nodes).data + 1;
       dominator < (info->nodes).data + (info->nodes).size;
       dominator++) {
    /// Find all blocks that are still reachable from the root.
    BlockVector still_reachable = collect_reachable_blocks(f->blocks.first, dominator->block);

    /// Find all blocks that are no longer reachable.
    VECTOR_FOREACH (DomTreeNode, d, info->nodes) {
      bool out = false;
      VECTOR_CONTAINS(still_reachable, d->block, out);
      if (!out) {
        /// Add the block to the dominators of the current node.
        VECTOR_PUSH(d->dominators, dominator);

        /// Add the current node to the children of the block.
        /// This is used to build the dominator tree below.
        VECTOR_PUSH(dominator->children, d);
      }
    }

    VECTOR_DELETE(still_reachable);
  }

  /// Now that we know the dominators of each block, we can
  /// build the dominator tree. Currently, the children of each
  /// node contains all nodes that are dominated by that node.
  ///
  /// However, we only want all nodes that are immediately
  /// dominated by that node. The algorithm for this is simple:
  /// For each node N, remove from N’s children any nodes that are
  /// also strictly dominated by another child of N.
  VECTOR (DomTreeNode*) to_remove = {0};
  VECTOR_FOREACH (DomTreeNode, n, info->nodes) {
    VECTOR_CLEAR(to_remove);

    /// For each child of N, check if it is strictly dominated by another child.
    VECTOR_FOREACH_PTR (DomTreeNode*, c, n->children) {
      VECTOR_FOREACH_PTR (DomTreeNode*, c2, n->children) {
        if (strictly_dominates(c, c2)) {
          VECTOR_PUSH(to_remove, c2);
          break;
        }
      }
    }

    /// Remove the nodes.
    VECTOR_FOREACH_PTR (DomTreeNode*, c, to_remove) {
      VECTOR_REMOVE_ELEMENT(n->children, c);
    }
  }
  VECTOR_DELETE(to_remove);
}