#ifndef LCC_IR_IR_HH
#define LCC_IR_IR_HH

#include <lcc/core.hh>
#include <lcc/forward.hh>
#include <lcc/ir/type.hh>
#include <lcc/location.hh>
#include <lcc/utils.hh>
#include <lcc/utils/iterator.hh>
#include <lcc/utils/rtti.hh>

namespace lcc {
/// An IR value.
///
/// This may either be an instruction, a block, a function
/// or a constant expression.
class Value {
public:
    enum struct Kind {
        Block,
        Function,

        /// Instructions.
        Alloca,
        Branch,
        Call,
        CondBranch,
    };

private:
    const Kind value_kind;

    /// Every value has a type.
    Type* value_type;

protected:
    Value(Kind kind, Type* ty = Type::UnknownTy)
        : value_kind(kind), value_type(ty) {}

public:
    virtual ~Value() = default;

    /// Disallow allocating these directly.
    void* operator new(size_t) = delete;

    /// Get the kind of this value for RTTI.
    Kind kind() const { return value_kind; }

    /// Get the type of this value.
    Type* type() const { return value_type; }
};

/// IR instruction.
class Inst : public Value {
    /// Associated machine instruction during early codegen.
    MInst* minst;

    /// Users of this instruction.
    std::vector<Inst*> user_list;

    /// The parent block that this instruction is inserted in.
    Block* parent;

    /// Source location of this instruction.
    Location loc;

protected:
    Inst(Kind k, Type* t, Location l = {})
        : Value(k), minst(nullptr), parent(nullptr), loc(l) {}

    /// Remove a use by an instruction.
    static void RemoveUse(Inst* of, Value* by) {
        if (not is<Inst>(by)) return;
        auto it = rgs::find(of->user_list, by);
        if (it == of->user_list.end()) return;
        of->user_list.erase(it);
    }

    /// Add a use by an instruction.
    static void AddUse(Inst* of, Value* by) {
        if (not is<Inst>(by)) return;
        auto it = rgs::find(of->user_list, by);
        if (it == of->user_list.end()) return;
        of->user_list.emplace_back(as<Inst>(by));
    }

public:
    /// Get the parent block.
    auto block() const -> Block* { return parent; }

    /// Get the source location of this instruction.
    auto location() const -> Location { return loc; }

    /// Get the associated machine instruction.
    auto machine_inst() const -> MInst* { return minst; }

    /// Set the associated machine instruction.
    void machine_inst(MInst* m) { minst = m; }

    /// Get the users of this instruction.
    auto users() const -> const std::vector<Inst*>& { return user_list; }

    /// RTTI.
    static bool classof(Value* v) { static_assert(false, "TODO"); }
};

/// A basic block.
class Block : public Value {
    using Iterator = utils::VectorIterator<Inst*>;
    using ConstIterator = utils::VectorConstIterator<Inst*>;

    /// Associated machine instruction block.
    MBlock* mblock{};

    /// The function that this block belongs to.
    Function* parent{};

    /// The instructions in this block.
    std::vector<Inst*> inst_list;

    /// The name of this block.
    std::string block_name;

public:
    Block(std::string n = "")
        : Value(Kind::Block),
          block_name(std::move(n)) {}

    /// Get an iterator to the first instruction in this block.
    auto begin() -> Iterator { return {inst_list, inst_list.begin()}; }
    auto begin() const -> ConstIterator { return {inst_list, inst_list.begin()}; }

    /// Get an iterator to the last instruction in this block.
    auto end() -> Iterator { return {inst_list, inst_list.end()}; }
    auto end() const -> ConstIterator { return {inst_list, inst_list.end()}; }

    /// Get the parent function.
    auto function() const -> Function* { return parent; }

    /// Insert an instruction at the end of this block.
    ///
    /// If `force` is not set to true, the block must not have
    /// a terminator. In general, try not to force-insert any
    /// instructions if you can avoid it because that may lead
    /// to ill-formed IR.
    ///
    /// \param i The instruction to insert.
    /// \param force If true, don’t error if the block has a terminator.
    /// \return The inserted instruction.
    auto insert(Inst* i, bool force = false) -> Inst*;

    /// Get the instructions in this block.
    auto instructions() const -> const std::vector<Inst*>& { return inst_list; }

    /// Get the associated machine block.
    auto machine_block() const -> MBlock* { return mblock; }

    /// Set the associated machine block.
    void machine_block(MBlock* m) { mblock = m; }

    /// Get the name of this block.
    auto name() const -> const std::string& { return block_name; }

    /// Set the name of this block.
    void name(std::string n) { block_name = std::move(n); }

    /// RTTI.
    static bool classof(Value* v) { return v->kind() == Kind::Block; }
};

/// An IR function.
class Function : public Value {
    using Iterator = utils::VectorIterator<Block*>;
    using ConstIterator = utils::VectorConstIterator<Block*>;

    /// The (mangled) name of this function.
    std::string func_name;

    /// The blocks in this function.
    std::vector<Block*> block_list{};

    /// The parameters of this function. Parameter instructions
    /// are never created manually; instead, they always reference
    /// one of the parameter instructions inserted here.
    std::vector<Inst*> param_list{};

    /// The source location of this function.
    Location loc;

    /// The parent context of this function.
    Context* ctx;

    /// The associated machine function.
    MFunction* mfunc{};

    /// The linkage of this function.
    Linkage link;

    /// Function attributes.
    /// TODO.

    /// The calling convention of this function.
    CallConv cc;

public:
    Function(
        Context* ctx,
        std::string mangled_name,
        FunctionType* type,
        Linkage linkage,
        CallConv calling_convention,
        Location loc = {}
    );

    /// Get an iterator to the first block in this function.
    auto begin() { return block_list.begin(); }
    auto begin() const { return block_list.begin(); }

    /// Get the blocks in this function.
    auto blocks() const -> const std::vector<Block*>& { return block_list; }

    /// Get the calling convention of this function.
    auto call_conv() const -> CallConv { return cc; }

    /// Set the calling convention of this function.
    void call_conv(CallConv c) { cc = c; }

    /// Get the parent context of this function.
    auto context() const -> Context* { return ctx; }

    /// Get an iterator to the end of the block list.
    auto end() { return block_list.end(); }
    auto end() const { return block_list.end(); }

    /// Get the linkage of this function.
    auto linkage() const -> Linkage { return link; }

    /// Set the linkage of this function.
    void linkage(Linkage l) { link = l; }

    /// Get the source location of this function.
    auto location() const -> Location { return loc; }

    /// Get the associated machine function.
    auto machine_function() const -> MFunction* { return mfunc; }

    /// Set the associated machine function.
    void machine_function(MFunction* m) { mfunc = m; }

    /// Get the name of this function.
    auto name() const -> const std::string& { return func_name; }

    /// Set the name of this function.
    void name(std::string n) { func_name = std::move(n); }

    /// Get a parameter value.
    ///
    /// \param i The index of the parameter.
    /// \return The parameter value of the i-th parameter upon
    ///     entry to this function.
    auto param(usz i) const -> Value* {
        LCC_ASSERT(i < param_list.size(), "Parameter index out of bounds");
        return param_list[i];
    }

    /// Get the number of parameters.
    auto param_count() const -> size_t { return param_list.size(); }

    /// RTTI.
    static bool classof(Value* v) { return v->kind() == Kind::Function; }
};

/// A call instruction.
class CallInst : public Inst {
    /// The value being called.
    Value* callee_value;

    /// The type of the function being called.
    ///
    /// We can’t just derive this from the callee in case we want
    /// to move to using opaque pointers more, in which case the
    /// function type has to be stored separately.
    ///
    /// FIXME: Once everything is working, determine whether
    ///        we actually ended up needing this.
    FunctionType* callee_type;

    /// The arguments to the call.
    std::vector<Value*> arguments;

    /// Whether the call is a tail call.
    bool tail_call : 1 = false;

    /// Whether this call was annotated with __builtin_inline and
    /// must be inlined regardless of the optimisation level.
    bool force_inline : 1 = false;

public:
    CallInst(
        Value* callee,
        FunctionType* callee_type,
        std::vector<Value*> arguments,
        Location loc = {}
    ) : Inst(Kind::Call, callee_type->ret(), loc),
        callee_value(callee), callee_type(callee_type), arguments(std::move(arguments)) {}

    /// Get the arguments.
    auto args() const -> const std::vector<Value*>& { return arguments; }

    /// Get the callee.
    auto callee() const -> Value* { return callee_value; }

    /// Get the callee type.
    auto function_type() const -> FunctionType* { return callee_type; }

    /// Get whether this is annotated with __builtin_inline.
    auto is_force_inline() const -> bool { return force_inline; }

    /// Get whether this is a tail call.
    auto is_tail_call() const -> bool { return tail_call; }

    /// Mark that this has to be inlined.
    void set_force_inline() { force_inline = true; }

    /// Set whether this is a tail call.
    void set_tail_call() { tail_call = true; }

    /// RTTI.
    static bool classof(Value* v) { return v->kind() == Kind::Call; }
};

/// Unconditional branch instruction.
class BranchInst : public Inst {
    /// The block to branch to.
    Block* target_block;

public:
    BranchInst(Block* target, Location loc = {})
        : Inst(Kind::Branch, Type::UnknownTy, loc), target_block(target) {}

    /// Get the target block.
    auto target() const -> Block* { return target_block; }

    /// Replace the target with another block.
    void target(Block* b) { target_block = b; }

    /// RTTI.
    static bool classof(Value* v) { return v->kind() == Kind::Branch; }
};

/// Conditional branch instruction.
class CondBranchInst : public Inst {
    /// The condition.
    Value* condition;

    /// The block to branch to if the condition is true.
    Block* then_;

    /// The block to branch to if the condition is false.
    Block* else_;

public:
    CondBranchInst(
        Value* cond,
        Block* then_,
        Block* else_,
        Location loc = {}
    ) : Inst(Kind::CondBranch, Type::UnknownTy, loc),
        condition(cond), then_(then_), else_(else_) {}

    /// Get the condition.
    auto cond() const -> Value* { return condition; }

    /// Replace the condition.
    void cond(Value* v);

    /// Get the block to branch to if the condition is false.
    auto else_block() const -> Block* { return else_; }

    /// Replace the else block.
    void else_block(Block* b) { else_ = b; }

    /// Get the block to branch to if the condition is true.
    auto then_block() const -> Block* { return then_; }

    /// Replace the then block.
    void then_block(Block* b) { then_ = b; }

    /// RTTI.
    static bool classof(Value* v) { return v->kind() == Kind::CondBranch; }
};

/// A stack allocation.
class AllocaInst : public Inst {
public:
    AllocaInst(Type* ty, Location loc = {})
        : Inst(Kind::Alloca, ty, loc) {}

    /// RTTI.
    static bool classof(Value* v) { return v->kind() == Kind::Alloca; }
};

/// Base class for binary instructions.
class BinaryInst : public Inst {
    /// The left operand.
    Value* left;

    /// The right operand.
    Value* right;

public:
    BinaryInst(Kind k, Value* l, Value* r, Location loc = {})
        : Inst(k, l->type(), loc), left(l), right(r) {}

    /// Get the left operand.
    auto lhs() const -> Value* { return left; }

    /// Replace the left operand.
    void lhs(Value* v);

    /// Get the right operand.
    auto rhs() const -> Value* { return right; }

    /// Replace the right operand.
    void rhs(Value* v);
};

} // namespace lcc

#endif // LCC_IR_IR_HH
