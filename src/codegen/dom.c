#include <codegen/dom.h>
#include <codegen/intermediate_representation.h>

typedef Vector(IRBlock*) BlockVector;

/// Perform DFS on the control flow graph to find blocks
/// that are reachable from `block`. If `ignore` is encountered,
/// it is ignored. This allows us to effectively ‘remove’ blocks
/// from the CFG without having to actually remove them.
static BlockVector collect_reachable_blocks(IRBlock *block, IRBlock *ignore) {
  BlockVector reachable = {0};
  vector_push(reachable, block);

  /// Stack for DFS.
  Vector(IRBlock*) dfs_stack = {0};
  vector_push(dfs_stack, block);
  while (dfs_stack.size) {
    IRBlock *b = vector_pop(dfs_stack);
    if (b == ignore) continue;

    STATIC_ASSERT(IR_COUNT == 32, "Handle all branch types");
    IRInstruction *i = b->instructions.last;
    switch (i->type) {
      default: break;
      case IR_BRANCH:
        if (!vector_contains(reachable, i->destination_block)) {
          vector_push(reachable, i->destination_block);
          vector_push(dfs_stack, i->destination_block);
        }
        break;

      case IR_BRANCH_CONDITIONAL:
        if (!vector_contains(reachable, i->cond_br.then)) {
          vector_push(reachable, i->cond_br.then);
          vector_push(dfs_stack, i->cond_br.then);
        }

        if (!vector_contains(reachable, i->cond_br.else_)) {
          vector_push(reachable, i->cond_br.else_);
          vector_push(dfs_stack, i->cond_br.else_);
        }

        break;
    }
  }
  vector_delete(dfs_stack);
  return reachable;
}

void free_dominator_info(DominatorInfo* info) {
  foreach (DomTreeNode, n, info->nodes) {
    vector_delete(n->dominators);
    vector_delete(n->children);
  }
  vector_delete(info->nodes);
}

/// Check if a node dominates another node.
bool dominates(DomTreeNode *dominator, DomTreeNode *node) {
  return vector_contains(node->dominators, dominator);
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

    list_foreach (IRBlock*, b, f->blocks) {
      if (!vector_contains(reachable, b)) vector_push(blocks_to_remove, b);
    }

    /// Remove unreachable blocks and free vectors.
    foreach_ptr (IRBlock*, b, blocks_to_remove) ir_remove_and_free_block(b);
    vector_delete(reachable);
    vector_delete(blocks_to_remove);
  }

  /// Free old dominator tree.
  foreach (DomTreeNode, n, info->nodes) {
    vector_delete(n->dominators);
    vector_delete(n->children);
  }
  vector_clear(info->nodes);

  /// Add a node for each block.
  ASSERT(f->blocks.first);
  list_foreach (IRBlock*, b, f->blocks) {
    DomTreeNode node = {0};
    node.block = b;
    vector_push(info->nodes, node);
  }

  /// The only dominator of the root is the root itself.
  /// We assume that the first block in a function is
  /// the entry block.
  vector_push(info->nodes.data[0].dominators, info->nodes.data);
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
    foreach (DomTreeNode, d, info->nodes) {
      if (!vector_contains(still_reachable, d->block)) {
        /// Add the block to the dominators of the current node.
        vector_push(d->dominators, dominator);

        /// Add the current node to the children of the block.
        /// This is used to build the dominator tree below.
        vector_push(dominator->children, d);
      }
    }

    vector_delete(still_reachable);
  }

  /// Now that we know the dominators of each block, we can
  /// build the dominator tree. Currently, the children of each
  /// node contains all nodes that are dominated by that node.
  ///
  /// However, we only want all nodes that are immediately
  /// dominated by that node. The algorithm for this is simple:
  /// For each node N, remove from N’s children any nodes that are
  /// also strictly dominated by another child of N.
  Vector(DomTreeNode*) to_remove = {0};
  foreach (DomTreeNode, n, info->nodes) {
    vector_clear(to_remove);

    /// For each child of N, check if it is strictly dominated by another child.
    foreach_ptr (DomTreeNode*, c, n->children) {
      foreach_ptr (DomTreeNode*, c2, n->children) {
        if (strictly_dominates(c, c2)) {
          vector_push(to_remove, c2);
          break;
        }
      }
    }

    /// Remove the nodes.
    foreach_ptr (DomTreeNode*, c, to_remove) {
      vector_remove_element(n->children, c);
    }
  }
  vector_delete(to_remove);
}