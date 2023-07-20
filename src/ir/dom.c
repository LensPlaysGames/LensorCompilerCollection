#include <ir/dom.h>
#include <ir/ir.h>

/// TODO: This implementation is brand-new, but it is already a pile of
///       jank. It will do for now, but it should be replaced with the
///       algorithm that also computes the loop nesting forest.

typedef Vector(int) IntVector;

struct DomTreeComputeState {
  IRBlockVector verts_by_id;
  Map(IRBlock*, usz) ids_by_vert;

  IRBlockVector label;
  IRBlockVector parent;
  IRBlockVector ancestor;
  IRBlockVector child;
  IRBlockVector ndfs; /// Vertex by DFS number.
  IntVector verts_dfs;
  IntVector semi;
  IntVector size;
  IRBlockVector idom;
  Vector(IRBlockVector) pred;
  Vector(IRBlockVector) bucket;
  IRBlock *aux; /// ‘zero block’.
  int n;
};

/// Get the ID of a block.
usz block_to_id(struct DomTreeComputeState *st, IRBlock* b) {
  return *map_get(st->ids_by_vert, b);
}

/// Map number or vertex to index.
///
/// This abomination of a macro is as horrible as it is because
/// the C standard requires that, for some ungodly reason, *all*
/// subexpressions of a _Generic selection be valid—even discarded
/// ones.
///
/// But since we can’t perform member access on an integer, we have
/// first cast it to the same type that it would have when that branch
/// is taken. This raises a bogus warning if the type actually ends up
/// being int, since we’re casting an int to a pointer. Since C is too
/// stupid to acknowledge that that branch will never be taken, we
/// suppress the warning with a pragma.
#define AT(i) _Generic((i), \
  int: i,                   \
  usz: i,                   \
  IRBlock *: (PUSH_IGNORE_WARNING("-Wint-to-pointer-cast")(block_to_id(st, (IRBlock *) (i))) POP_WARNINGS()) \
)

#define LABEL(v) (st->label.data[AT(v)])
#define PARENT(v) (st->parent.data[AT(v)])
#define ANCESTOR(v) (st->ancestor.data[AT(v)])
#define CHILD(v) (st->child.data[AT(v)])
#define SEMI(v) (st->semi.data[AT(v)])
#define IDOM(v) (st->idom.data[AT(v)])
#define PRED(v) (st->pred.data[AT(v)])
#define BUCKET(v) (st->bucket.data[AT(v)])
#define NDFS(v) (st->ndfs.data[AT(v)])
#define SIZE(v) (st->size.data[AT(v)])
#define COMPRESS(v) dom_compress(st, v)
#define EVAL(v) dom_eval(st, v)
#define LINK(v, w) dom_link(st, v, w)
#define N0 (st->aux)
#define N (st->n)

static void dom_dfs(struct DomTreeComputeState *st, IRBlock *v) {
#define VISIT(w)        \
  do {                  \
    if (SEMI(w) == 0) { \
      PARENT(w) = v;    \
      dom_dfs(st, w);   \
    }                   \
  } while (0)

  SEMI(v) = N;
  NDFS(N) = LABEL(v) = v;
  ANCESTOR(v) = CHILD(v) = N0;
  SIZE(v) = 1;
  N++;

  STATIC_ASSERT(IR_COUNT == 40, "Handle all branch types");
  IRInstruction *br = ir_terminator(v);
  switch (ir_kind(br)) {
    default: break;
    case IR_BRANCH: VISIT(ir_dest(br)); break;
    case IR_BRANCH_CONDITIONAL:
      VISIT(ir_then(br));
      VISIT(ir_else(br));
      break;
  }

#undef VISIT
}

static void dom_compress(struct DomTreeComputeState *st, IRBlock *v) {
  if (ANCESTOR(ANCESTOR(v)) == N0) return;
  COMPRESS(ANCESTOR(v));
  if (SEMI(LABEL(ANCESTOR(v))) < SEMI(LABEL(v)))
    LABEL(v) = LABEL(ANCESTOR(v));
  ANCESTOR(v) = ANCESTOR(ANCESTOR(v));
}

static IRBlock *dom_eval(struct DomTreeComputeState *st, IRBlock *v) {
  if (ANCESTOR(v) == N0) return LABEL(v);
  COMPRESS(v);
  return SEMI(LABEL(ANCESTOR(v))) >= SEMI(LABEL(v))
         ? LABEL(v)
         : LABEL(ANCESTOR(v));
}

static void dom_link(struct DomTreeComputeState *st, IRBlock *v, IRBlock *w) {
  IRBlock *s = w;
  while (SEMI(LABEL(w)) < SEMI(LABEL(CHILD(s)))) {
    if (SIZE(s) + SIZE(CHILD(CHILD(s))) >= 2 * SIZE(CHILD(s))) {
      ANCESTOR(CHILD(s)) = s;
      CHILD(s) = CHILD(CHILD(s));
    } else {
      SIZE(CHILD(s)) = SIZE(s);
      s = ANCESTOR(s) = CHILD(s);
    }
  }

  LABEL(s) = LABEL(w);
  SIZE(v) += SIZE(w);
  if (SIZE(v) < 2 * SIZE(w)) {
    IRBlock *tmp = s;
    s = CHILD(v);
    CHILD(v) = tmp;
  }
  while (s != N0) {
    ANCESTOR(s) = v;
    s = CHILD(s);
  }
}

/// Compute predecessor info from a node.
static void dom_compute_preds(struct DomTreeComputeState *st, IRBlock *v) {
  /// Skip dummy vertex.
  if (v == N0) return;
  STATIC_ASSERT(IR_COUNT == 40, "Handle all branch types");
  IRInstruction *br = ir_terminator(v);
  switch (ir_kind(br)) {
    default: break;
    case IR_BRANCH:
      vector_push(PRED(ir_dest(br)), v);
      break;
    case IR_BRANCH_CONDITIONAL:
      vector_push(PRED(ir_then(br)), v);
      vector_push(PRED(ir_else(br)), v);
      break;
  }
}

/// Compute the dominator tree for a function.
///
/// This function uses the Lengauer-Tarjan dominator tree construction
/// algorithm; this algorithm makes use of semidominators which serve
/// as a good enough approximation of the immediate dominators.
///
/// An in-depth explanation of this algorithm would be rather long,
/// so see (Lengauer, T. and Tarjan, R. E. (1979). ‘A Fast Algorithm
/// for Finding Dominators in a Flowgraph’. In: ACM Transactions on
/// Programming Languages and Systems 1.1, pp. 121–141.) for more
/// information.
DominatorTree dom_tree_build(IRFunction *f) {
  /// Set up state.
  struct DomTreeComputeState _st = {0};
  struct DomTreeComputeState *st = &_st;
  IRBlock * const root = *ir_begin(f);

  /// Collect all vertices (except for the root) and
  /// number them. Also add in an auxiliary vertex.
  vector_reserve(st->verts_by_id, ir_count(f));
  vector_push(st->verts_by_id, N0);
  FOREACH_BLOCK (v, f) {
    /// Skip the root.
    if (v == root) {
      map_set(st->ids_by_vert, v, (usz)0);
      continue;
    }

    map_set(st->ids_by_vert, v, st->verts_by_id.size);
    vector_push(st->verts_by_id, v);
  }

  /// Compute predecessors.
  vector_resize(st->pred, st->verts_by_id.size);
  foreach_val (v, st->verts_by_id) dom_compute_preds(st, v);
  dom_compute_preds(st, root);

  /// Initialise data structures.
  vector_resize(st->label, st->verts_by_id.size);
  vector_resize(st->parent, st->verts_by_id.size);
  vector_resize(st->ancestor, st->verts_by_id.size);
  vector_resize(st->child, st->verts_by_id.size);
  vector_resize(st->verts_dfs, st->verts_by_id.size);
  vector_resize(st->ndfs, st->verts_by_id.size);
  vector_resize(st->semi, st->verts_by_id.size);
  vector_resize(st->size, st->verts_by_id.size);
  vector_resize(st->idom, st->verts_by_id.size);
  vector_resize(st->bucket, st->verts_by_id.size);

  foreach (v, st->label) *v = N0;
  foreach (v, st->parent) *v = N0;
  foreach (v, st->ancestor) *v = N0;
  foreach (v, st->child) *v = N0;
  foreach (v, st->idom) *v = N0;

  /// Perform DFS.
  dom_dfs(st, root);

  /// Step 1.
  foreach_index_rev (i, st->verts_dfs) {
    /// Skip dummy vertex.
    if (i == 0) break;

    /// Compute initial semidominators.
    IRBlock *w = NDFS(i);
    foreach_val (v, PRED(w)) {
      IRBlock *u = EVAL(v);
      if (SEMI(u) < SEMI(w)) SEMI(w) = SEMI(u);
    }

    /// Collect buckets.
    vector_push(BUCKET(NDFS(SEMI(w))), w);
    LINK(PARENT(w), w);

    /// Compute idoms for all the nodes in the bucket.
    while (BUCKET(PARENT(w)).size) {
      IRBlock *v = vector_pop(BUCKET(PARENT(w)));
      IRBlock *u = EVAL(v);
      if (SEMI(u) < SEMI(v)) IDOM(v) = u;
      else IDOM(v) = PARENT(w);
    }
  }

  /// Adjust idoms.
  foreach_index (i, st->verts_dfs) {
    /// Skip dummy vertex.
    if (i == 0) continue;
    IRBlock *w = NDFS(i);
    if (IDOM(w) != NDFS(SEMI(w))) IDOM(w) = IDOM(IDOM(w));
  }

  /// Set the immediate dominator to the root for
  /// all nodes for which it is still N0.
  foreach (v, st->idom) {
    if (v == st->idom.data) *v = NULL;
    if (*v == N0) *v = root;
  }

  /// Create dominator tree.
  DominatorTree dom = {0};
  vector_reserve(dom.doms, st->verts_by_id.size);
  foreach_val (v, st->verts_by_id) map_set(dom.doms, v, IDOM(v));

  /// Delete state.
  vector_delete(st->verts_by_id);
  vector_delete(st->label);
  vector_delete(st->parent);
  vector_delete(st->ancestor);
  vector_delete(st->child);
  vector_delete(st->verts_dfs);
  vector_delete(st->ndfs);
  vector_delete(st->semi);
  vector_delete(st->size);
  vector_delete(st->idom);
  vector_delete(st->pred);
  vector_delete(st->bucket);
  return dom;
}

void dom_tree_delete(DominatorTree *info) {
  vector_delete(info->doms);
}