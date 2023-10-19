#ifndef LCC_IR_IR_HH
#define LCC_IR_IR_HH

#include <algorithm>
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
        // Values
        Block,
        Function,
        IntegerConstant,
        ArrayConstant,
        Poison,
        GlobalVariable,

        /// Instructions
        Alloca,
        Call,
        // Copy,
        GetElementPtr,
        Intrinsic,
        Load,
        Parameter,
        Phi,
        Store,

        /// Terminators
        Branch,
        CondBranch,
        Return,
        Unreachable,

        /// Unary instructions
        ZExt,
        SExt,
        Trunc,
        Bitcast,
        Neg,
        Compl,

        /// Binary instructions
        Add,
        Sub,
        Mul,
        SDiv,
        UDiv,
        SRem,
        URem,
        Shl,
        Sar,
        Shr,
        And,
        Or,
        Xor,

        /// Comparison instructions
        Eq,
        Ne,
        // Signed comparisons
        SLt,
        SLe,
        SGt,
        SGe,
        // Unsigned comparisons
        ULt,
        ULe,
        UGt,
        UGe,
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
    void* operator new(size_t sz, Module&) {
        return ::operator new(sz);
    }

    /// Get the kind of this value for RTTI.
    Kind kind() const { return value_kind; }

    /// Get the type of this value.
    Type* type() const { return value_type; }
};

class GlobalVariable : public Value {
    std::string _name;
    Linkage _linkage;
    Value* _init;

public:
    GlobalVariable(Type* t, std::string name, Linkage linkage, Value* init)
        : Value(Value::Kind::GlobalVariable, t),
          _name(std::move(name)),
          _linkage(linkage),
          _init(init) {}

    const std::string& name() { return _name; }
    Linkage linkage() { return _linkage; }
    Value* init() { return _init; }

    /// RTTI.
    static bool classof(Value* v) { return +v->kind() >= +Kind::GlobalVariable; }
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
        : Value(k, t), minst(nullptr), parent(nullptr), loc(l) {}

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

    /// Check if this is a terminator instruction.
    bool is_terminator() const {
        return kind() >= Value::Kind::Branch and kind() <= Value::Kind::Unreachable;
    }

    /// Get the source location of this instruction.
    auto location() const -> Location { return loc; }

    /// Get the associated machine instruction.
    auto machine_inst() const -> MInst* { return minst; }

    /// Set the associated machine instruction.
    void machine_inst(MInst* m) { minst = m; }

    /// Get the users of this instruction.
    auto users() const -> const std::vector<Inst*>& { return user_list; }

    /// RTTI.
    static bool classof(Value* v) { return +v->kind() >= +Kind::Alloca; }
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

    /// TODO: Blocks and functions should also keep track of their users
    /// to simplify dead code elimination and computing predecessors.

public:
    Block(std::string n = "")
        : Value(Kind::Block),
          block_name(std::move(n)) {}

    /// Get an iterator to the first instruction in this block.
    auto begin() const -> ConstIterator { return {inst_list, inst_list.begin()}; }

    /// Get an iterator to the last instruction in this block.
    auto end() const -> ConstIterator { return {inst_list, inst_list.end()}; }

    /// Check whether this block has a terminator.
    bool closed() const { return terminator() != nullptr; }

    /// Get the parent function.
    auto function() const -> Function* { return parent; }

    /// Check if this block has another block as one
    /// of its predecessors.
    bool has_predecessor(Block* block) const;

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

    /// Get the terminator instruction of this block; may return nullptr.
    auto terminator() const -> Inst* {
        if (inst_list.empty()) return nullptr;
        auto i = inst_list.back();
        if (not i->is_terminator()) return nullptr;
        return i;
    }

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
        FunctionType* ty,
        Linkage linkage,
        CallConv calling_convention,
        Location l = {}
    ) : Value(Kind::Function, ty),
        func_name(std::move(mangled_name)),
        loc(l),
        ctx(ctx),
        link(linkage),
        cc(calling_convention) {}

    /// Get an iterator to the first block in this function.
    auto begin() const { return block_list.begin(); }

    void append_block(Block* b) { block_list.push_back(b); };

    /// Get the blocks in this function.
    auto blocks() const -> const std::vector<Block*>& { return block_list; }

    /// Get the calling convention of this function.
    auto call_conv() const -> CallConv { return cc; }

    /// Set the calling convention of this function.
    void call_conv(CallConv c) { cc = c; }

    /// Get the parent context of this function.
    auto context() const -> Context* { return ctx; }

    /// Get an iterator to the end of the block list.
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

/// A parameter reference.
class Parameter : public Value {
    /// The parameter index.
    u32 i;

    /// Only the Function class should be able to create these.
    friend Function;
    Parameter(Type* ty, u32 idx) : Value(Kind::Parameter, ty), i(idx) {}
public:
    /// Get the parameter index.
    auto index() const -> u32 { return i; }

    /// RTTI.
    static bool classof(Value* v) { return v->kind() == Kind::Parameter; }
};

/// ============================================================================
///  Instructions
/// ============================================================================
/// A stack allocation.
class AllocaInst : public Inst {
    Type* _allocated_type;

public:
    AllocaInst(Type* ty, Location loc = {})
        : Inst(Kind::Alloca, Type::PtrTy, loc), _allocated_type(ty) {}

    Type* allocated_type() { return _allocated_type; }

    /// RTTI.
    static bool classof(Value* v) { return v->kind() == Kind::Alloca; }
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

/// Backend intrinsic.
class IntrinsicInst : public Inst {
    /// The intrinsic ID.
    IntrinsicKind intrinsic;

    /// The operands of the intrinsic.
    std::vector<Value*> operand_list;

public:
    IntrinsicInst(
        IntrinsicKind intrinsic,
        std::vector<Value*> operands,
        Location loc = {}
    ) : Inst(Kind::Intrinsic, Type::UnknownTy, loc),
        intrinsic(intrinsic), operand_list(std::move(operands)) {}

    /// Get the intrinsic ID.
    auto intrinsic_kind() const -> IntrinsicKind { return intrinsic; }

    /// Get the operands.
    auto operands() const -> const std::vector<Value*>& { return operand_list; }

    /// RTTI.
    static bool classof(Value* v) { return v->kind() == Kind::Intrinsic; }
};

/// A "get element pointer", or GEP, instruction.
///
/// For now, GEP stores the element type as it's type, then indexes it's
/// base pointer based on that.
///
/// GEP(T, p, x) = p + (sizeof(T) * x)
///
class GEPInst : public Inst {
    /// The base pointer to index from; "p" in the equation.
    Value* pointer;

    // The index to offset from the base pointer; "x" in the equation.
    Value* index;

public:
    GEPInst(Type* elementType, Value* arrayPointer, Value* arrayIndex, Location loc = {})
        : Inst(Kind::GetElementPtr, elementType, loc), pointer(arrayPointer), index(arrayIndex) {
        LCC_ASSERT(
            pointer->type() == Type::PtrTy,
            "GEPInst may only operate on arrays or opaque pointers, which `{}` is not",
            *pointer->type()
        );
    }

    /// Get the base pointer.
    auto ptr() const -> Value* { return pointer; }

    /// Get the index.
    auto idx() const -> Value* { return index; }

    /// RTTI.
    static bool classof(Value* v) { return v->kind() == Kind::GetElementPtr; }
};

/// A load instruction.
class LoadInst : public Inst {
    /// The pointer to load from.
    Value* pointer;

public:
    LoadInst(Type* ty, Value* ptr, Location loc = {})
        : Inst(Kind::Load, ty, loc), pointer(ptr) {
        LCC_ASSERT(ptr->type() == Type::PtrTy, "LoadInst can only load from pointers");
    }

    /// Get the pointer to load from.
    auto ptr() const -> Value* { return pointer; }

    /// Replace the pointer.
    void ptr(Value* v);

    /// RTTI.
    static bool classof(Value* v) { return v->kind() == Kind::Load; }
};

/// A store instruction.
class StoreInst : public Inst {
    /// The value to store.
    Value* value;

    /// The pointer to store to.
    Value* pointer;

public:
    StoreInst(Value* val, Value* ptr, Location loc = {})
        : Inst(Kind::Store, Type::VoidTy, loc), value(val), pointer(ptr) {
        LCC_ASSERT(ptr->type() == Type::PtrTy, "StoreInst can only store to pointers");
    }

    /// Get the value to store.
    auto val() const -> Value* { return value; }

    /// Replace the value.
    void val(Value* v);

    /// Get the pointer to store to.
    auto ptr() const -> Value* { return pointer; }

    /// Replace the pointer.
    void ptr(Value* v);

    /// RTTI.
    static bool classof(Value* v) { return v->kind() == Kind::Store; }
};

/// PHI instruction.
class PhiInst : public Inst {
    /// An incoming value.
    struct IncomingValue {
        /// The value.
        Value* value;

        /// The block it comes from.
        Block* block;
    };

    /// Iterators.
    using Iterator = utils::VectorIterator<IncomingValue>;
    using ConstIterator = utils::VectorConstIterator<IncomingValue>;

    /// The incoming values.
    std::vector<IncomingValue> incoming{};

public:
    PhiInst(Type* ty, Location loc = {})
        : Inst(Kind::Phi, ty, loc) {}

    /// Get an iterator to the start of the incoming values.
    auto begin() const -> ConstIterator { return {incoming, incoming.begin()}; }

    /// Remove stale incoming values.
    ///
    /// This function removes any incoming values that are registered
    /// from blocks that are not predecessors of the block containing
    /// the PHI. If the PHI is not inserted in a block, this is a no-op.
    void drop_stale_operands() {
        if (not block()) return;
        std::erase_if(incoming, [&](const IncomingValue& elem) {
            return not block()->has_predecessor(elem.block);
        });
    }

    /// Get an iterator to the end of the incoming values.
    auto end() const -> ConstIterator { return {incoming, incoming.end()}; }

    /// Get the incoming values.
    auto operands() const -> const std::vector<IncomingValue>& { return incoming; }

    /// Remove an incoming value for a block.
    ///
    /// If there is an entry for the block in question in the
    /// operand list of this PHI, the corresponding value is
    /// removed.
    ///
    /// \param block The block to remove the value for.
    void remove_incoming(Block* block) {
        std::erase_if(incoming, [&](const IncomingValue& elem) {
            return elem.block == block;
        });
    }

    /// Register an incoming value from a block.
    ///
    /// If the source block ends with an unreachable or return
    /// instruction, this is a no-op. If this PHI already has
    /// a value coming from that block, the value is replaced
    /// with the new value.
    ///
    /// \param value The value to add.
    /// \param block The block it comes from.
    void set_incoming(Value* value, Block* block) {
        auto existing = rgs::find(incoming, block, &IncomingValue::block);
        if (existing != incoming.end())
            *existing = {value, block};
        else incoming.push_back({value, block});
    }

    /// RTTI.
    static bool classof(Value* v) { return v->kind() == Kind::Phi; }
};

/// ============================================================================
///  Terminators
/// ============================================================================
/// NOTE: If you add another one, be sure to update implementation of `is_block_terminator`.

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

/// Return instruction.
class ReturnInst : public Inst {
    /// The value to return.
    Value* value;

public:
    ReturnInst(Value* val, Location loc = {})
        : Inst(Kind::Return, Type::VoidTy, loc), value(val) {}

    /// Check if this instruction returns a value.
    auto has_value() const -> bool { return value != nullptr; }

    /// Get the value to return. This may be null if this is a void return.
    auto val() const -> Value* { return value; }

    /// Replace the value.
    void val(Value* v);

    /// RTTI.
    static bool classof(Value* v) { return v->kind() == Kind::Return; }
};

/// Unreachable instruction.
class UnreachableInst : public Inst {
public:
    UnreachableInst(Location loc = {})
        : Inst(Kind::Unreachable, Type::VoidTy, loc) {}

    /// RTTI.
    static bool classof(Value* v) { return v->kind() == Kind::Unreachable; }
};

/// ============================================================================
///  Binary Instructions and Casts
/// ============================================================================
/// Base class for binary instructions.
class BinaryInst : public Inst {
    /// The left operand.
    Value* left;

    /// The right operand.
    Value* right;

protected:
    /// Assert that two operands have the same type.
    void AssertSameType(Value* l, Value* r) {
        LCC_ASSERT(
            l->type() == r->type(),
            "Operands of `add` instruction must have the same type, but was {} and {}",
            *l->type(),
            *r->type()
        );
    }

public:
    BinaryInst(Kind k, Value* l, Value* r, Type* ty, Location loc = {})
        : Inst(k, ty, loc), left(l), right(r) {}

    /// Get the left operand.
    auto lhs() const -> Value* { return left; }

    /// Replace the left operand.
    void lhs(Value* v);

    /// Get the right operand.
    auto rhs() const -> Value* { return right; }

    /// Replace the right operand.
    void rhs(Value* v);

    /// RTTI.
    static bool classof(Value* v) { return +v->kind() >= +Kind::Add; }
};

/// An add instruction.
class AddInst : public BinaryInst {
public:
    AddInst(Value* l, Value* r, Location loc = {})
        : BinaryInst(Kind::Add, l, r, l->type(), loc) {
        AssertSameType(l, r);
    }

    /// RTTI.
    static bool classof(Value* v) { return v->kind() == Kind::Add; }
};

/// A subtract instruction.
class SubInst : public BinaryInst {
public:
    SubInst(Value* l, Value* r, Location loc = {})
        : BinaryInst(Kind::Sub, l, r, l->type(), loc) {
        AssertSameType(l, r);
    }

    /// RTTI.
    static bool classof(Value* v) { return v->kind() == Kind::Sub; }
};

/// A multiply instruction.
class MulInst : public BinaryInst {
public:
    MulInst(Value* l, Value* r, Location loc = {})
        : BinaryInst(Kind::Mul, l, r, l->type(), loc) {
        AssertSameType(l, r);
    }

    /// RTTI.
    static bool classof(Value* v) { return v->kind() == Kind::Mul; }
};

/// A signed divide instruction.
class SDivInst : public BinaryInst {
public:
    SDivInst(Value* l, Value* r, Location loc = {})
        : BinaryInst(Kind::SDiv, l, r, l->type(), loc) {
        AssertSameType(l, r);
    }

    /// RTTI.
    static bool classof(Value* v) { return v->kind() == Kind::SDiv; }
};

/// A signed remainder instruction.
class SRemInst : public BinaryInst {
public:
    SRemInst(Value* l, Value* r, Location loc = {})
        : BinaryInst(Kind::SRem, l, r, l->type(), loc) {
        AssertSameType(l, r);
    }

    /// RTTI.
    static bool classof(Value* v) { return v->kind() == Kind::SRem; }
};

/// An unsigned divide instruction.
class UDivInst : public BinaryInst {
public:
    UDivInst(Value* l, Value* r, Location loc = {})
        : BinaryInst(Kind::UDiv, l, r, l->type(), loc) {
        AssertSameType(l, r);
    }

    /// RTTI.
    static bool classof(Value* v) { return v->kind() == Kind::UDiv; }
};

/// An unsigned remainder instruction.
class URemInst : public BinaryInst {
public:
    URemInst(Value* l, Value* r, Location loc = {})
        : BinaryInst(Kind::URem, l, r, l->type(), loc) {
        AssertSameType(l, r);
    }

    /// RTTI.
    static bool classof(Value* v) { return v->kind() == Kind::URem; }
};

/// A left shift instruction.
class ShlInst : public BinaryInst {
public:
    ShlInst(Value* l, Value* r, Location loc = {})
        : BinaryInst(Kind::Shl, l, r, l->type(), loc) {
        AssertSameType(l, r);
    }

    /// RTTI.
    static bool classof(Value* v) { return v->kind() == Kind::Shl; }
};

/// An arithmetic right shift.
class SarInst : public BinaryInst {
public:
    SarInst(Value* l, Value* r, Location loc = {})
        : BinaryInst(Kind::Sar, l, r, l->type(), loc) {
        AssertSameType(l, r);
    }

    /// RTTI.
    static bool classof(Value* v) { return v->kind() == Kind::Sar; }
};

/// A logical right shift.
class ShrInst : public BinaryInst {
public:
    ShrInst(Value* l, Value* r, Location loc = {})
        : BinaryInst(Kind::Shr, l, r, l->type(), loc) {
        AssertSameType(l, r);
    }

    /// RTTI.
    static bool classof(Value* v) { return v->kind() == Kind::Shr; }
};

/// A bitwise and instruction.
class AndInst : public BinaryInst {
public:
    AndInst(Value* l, Value* r, Location loc = {})
        : BinaryInst(Kind::And, l, r, l->type(), loc) {
        AssertSameType(l, r);
    }

    /// RTTI.
    static bool classof(Value* v) { return v->kind() == Kind::And; }
};

/// A bitwise or instruction.
class OrInst : public BinaryInst {
public:
    OrInst(Value* l, Value* r, Location loc = {})
        : BinaryInst(Kind::Or, l, r, l->type(), loc) {
        AssertSameType(l, r);
    }

    /// RTTI.
    static bool classof(Value* v) { return v->kind() == Kind::Or; }
};

/// A bitwise xor instruction.
class XorInst : public BinaryInst {
public:
    XorInst(Value* l, Value* r, Location loc = {})
        : BinaryInst(Kind::Or, l, r, l->type(), loc) {
        AssertSameType(l, r);
    }

    /// RTTI.
    static bool classof(Value* v) { return v->kind() == Kind::Xor; }
};

/// Base class for comparison instructions; this is just so we
/// can do is<CompareInst> and because the type is always i1.
class CompareInst : public BinaryInst {
protected:
    CompareInst(Kind k, Value* l, Value* r, Location loc = {})
        : BinaryInst(k, l, r, Type::I1Ty, loc) {}

public:
    /// RTTI.
    static bool classof(Value* v) { return +v->kind() >= +Kind::Eq; }
};

/// Instruction that compares two values for equality.
class EqInst : public CompareInst {
public:
    EqInst(Value* l, Value* r, Location loc = {})
        : CompareInst(Kind::Eq, l, r, loc) {
        AssertSameType(l, r);
    }

    /// RTTI.
    static bool classof(Value* v) { return v->kind() == Kind::Eq; }
};

/// Instruction that compares two values for inequality.
class NeInst : public CompareInst {
public:
    NeInst(Value* l, Value* r, Location loc = {})
        : CompareInst(Kind::Ne, l, r, loc) {
        AssertSameType(l, r);
    }

    /// RTTI.
    static bool classof(Value* v) { return v->kind() == Kind::Ne; }
};

/// Instruction that compares two values for less-than.
class SLtInst : public CompareInst {
public:
    SLtInst(Value* l, Value* r, Location loc = {})
        : CompareInst(Kind::ULt, l, r, loc) {
        // TODO: Assert type of `Value* l` is signed
        AssertSameType(l, r);
    }

    /// RTTI.
    static bool classof(Value* v) { return v->kind() == Kind::SLt; }
};
class ULtInst : public CompareInst {
public:
    ULtInst(Value* l, Value* r, Location loc = {})
        : CompareInst(Kind::SLt, l, r, loc) {
        // TODO: Assert type of `Value* l` is unsigned
        AssertSameType(l, r);
    }

    /// RTTI.
    static bool classof(Value* v) { return v->kind() == Kind::ULt; }
};

/// Instruction that compares two values for less-than-or-equal.
class SLeInst : public CompareInst {
public:
    SLeInst(Value* l, Value* r, Location loc = {})
        : CompareInst(Kind::SLe, l, r, loc) {
        AssertSameType(l, r);
    }

    /// RTTI.
    static bool classof(Value* v) { return v->kind() == Kind::SLe; }
};
class ULeInst : public CompareInst {
public:
    ULeInst(Value* l, Value* r, Location loc = {})
        : CompareInst(Kind::ULe, l, r, loc) {
        AssertSameType(l, r);
    }

    /// RTTI.
    static bool classof(Value* v) { return v->kind() == Kind::ULe; }
};

/// Instruction that compares two values for greater-than.
class SGtInst : public CompareInst {
public:
    SGtInst(Value* l, Value* r, Location loc = {})
        : CompareInst(Kind::SGt, l, r, loc) {
        AssertSameType(l, r);
    }

    /// RTTI.
    static bool classof(Value* v) { return v->kind() == Kind::SGt; }
};
class UGtInst : public CompareInst {
public:
    UGtInst(Value* l, Value* r, Location loc = {})
        : CompareInst(Kind::UGt, l, r, loc) {
        AssertSameType(l, r);
    }

    /// RTTI.
    static bool classof(Value* v) { return v->kind() == Kind::UGt; }
};

/// Instruction that compares two values for greater-than-or-equal.
class SGeInst : public CompareInst {
public:
    SGeInst(Value* l, Value* r, Location loc = {})
        : CompareInst(Kind::SGe, l, r, loc) {
        AssertSameType(l, r);
    }

    /// RTTI.
    static bool classof(Value* v) { return v->kind() == Kind::SGe; }
};
class UGeInst : public CompareInst {
public:
    UGeInst(Value* l, Value* r, Location loc = {})
        : CompareInst(Kind::UGe, l, r, loc) {
        AssertSameType(l, r);
    }

    /// RTTI.
    static bool classof(Value* v) { return v->kind() == Kind::UGe; }
};

/// Unary instructions.
class UnaryInstBase : public Inst {
    Value* op;

protected:
    UnaryInstBase(Kind k, Value* v, Type* ty, Location loc = {})
        : Inst(k, ty, loc), op(v) {}

public:
    /// Get the operand.
    Value* operand() const { return op; }

    /// Set the operand.
    void operand(Value* v) { op = v; }
};

/// Zero-extend an integer value.
class ZExtInst : public UnaryInstBase {
public:
    ZExtInst(Value* v, Type* ty, Location loc = {})
        : UnaryInstBase(Kind::ZExt, v, ty, loc) {}

    /// RTTI.
    static bool classof(Value* v) { return v->kind() == Kind::ZExt; }
};

/// Sign-extend an integer value.
class SExtInst : public UnaryInstBase {
public:
    SExtInst(Value* v, Type* ty, Location loc = {})
        : UnaryInstBase(Kind::SExt, v, ty, loc) {}

    /// RTTI.
    static bool classof(Value* v) { return v->kind() == Kind::SExt; }
};

/// Truncate an integer value.
class TruncInst : public UnaryInstBase {
public:
    TruncInst(Value* v, Type* ty, Location loc = {})
        : UnaryInstBase(Kind::Trunc, v, ty, loc) {}

    /// RTTI.
    static bool classof(Value* v) { return v->kind() == Kind::Trunc; }
};

/// Bitcast a value to another type.
class BitcastInst : public UnaryInstBase {
public:
    BitcastInst(Value* v, Type* ty, Location loc = {})
        : UnaryInstBase(Kind::Bitcast, v, ty, loc) {
        /// Type we’re casting to must have the same size or
        /// be smaller than the type we’re casting from.
        LCC_ASSERT(ty->bits() <= v->type()->bits(), "Cannot bitcast to larger type");
    }

    /// RTTI.
    static bool classof(Value* v) { return v->kind() == Kind::Bitcast; }
};

/// Negate an integer value.
class NegInst : public UnaryInstBase {
public:
    NegInst(Value* v, Location loc = {})
        : UnaryInstBase(Kind::Neg, v, v->type(), loc) {}

    /// RTTI.
    static bool classof(Value* v) { return v->kind() == Kind::Neg; }
};

/// Bitwise complement of an integer value.
class ComplInst : public UnaryInstBase {
public:
    ComplInst(Value* v, Location loc = {})
        : UnaryInstBase(Kind::Compl, v, v->type(), loc) {}

    /// RTTI.
    static bool classof(Value* v) { return v->kind() == Kind::Compl; }
};

/// ============================================================================
///  Constants
/// ============================================================================
/// Immediate value.
class IntegerConstant : public Value {
    uint64_t _value;

public:
    IntegerConstant(Type* ty, uint64_t value)
        : Value(Kind::IntegerConstant, ty), _value(value) {}

    /// Get the value.
    uint64_t value() const { return _value; }

    /// RTTI.
    static bool classof(Value* v) { return v->kind() == Kind::IntegerConstant; }
};

/// Packed array.
///
/// This is used for constant arrays that are known at compile time,
/// such as strings or constant array literals.
class ArrayConstant : public Value {
    std::vector<char> _data;

public:
    ArrayConstant(Type* ty, std::vector<char> _data)
        : Value(Kind::ArrayConstant, ty), _data(std::move(_data)) {}

    /// Get the data.
    auto data() const -> const char* { return _data.data(); }

    /// Get the size.
    auto size() const -> size_t { return _data.size(); }

    /// RTTI.
    static bool classof(Value* v) { return v->kind() == Kind::ArrayConstant; }
};

/// Poison value.
class PoisonValue : public Value {
public:
    PoisonValue(Type* ty)
        : Value(Kind::Poison, ty) {}

    /// RTTI.
    static bool classof(Value* v) { return v->kind() == Kind::Poison; }
};

} // namespace lcc

#endif // LCC_IR_IR_HH
