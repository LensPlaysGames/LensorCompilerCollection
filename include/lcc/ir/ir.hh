#ifndef LCC_IR_IR_HH
#define LCC_IR_IR_HH

#include <algorithm>
#include <lcc/core.hh>
#include <lcc/forward.hh>
#include <lcc/ir/type.hh>
#include <lcc/location.hh>
#include <lcc/utils.hh>
#include <lcc/utils/aint.hh>
#include <lcc/utils/generator.hh>
#include <lcc/utils/iterator.hh>
#include <lcc/utils/rtti.hh>

namespace lcc {
class Parameter;

namespace parser {
class Parser;
}

/// An IR value.
///
/// This may either be an instruction, a block, a function
/// or a constant expression.
class Value {
public:
    enum struct Kind {
        /// Values
        Block,
        Function,
        IntegerConstant,
        ArrayConstant,
        Poison,
        GlobalVariable,
        Parameter,

        /// Instructions
        Alloca,
        Call,
        GetElementPtr,
        GetMemberPtr,
        Intrinsic,
        Load,
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
        Copy,
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

    /// Print this value for debugging.
    void print() const;

    static auto ToString(Value::Kind v) -> std::string {
        using VK = Value::Kind;
        switch (v) {
            /// Values
            case VK::Block: return "Block";
            case VK::Function: return "Function";
            case VK::IntegerConstant: return "IntegerConstant";
            case VK::ArrayConstant: return "ArrayConstant";
            case VK::Poison: return "Poison";
            case VK::GlobalVariable: return "GlobalVariable";
            case VK::Parameter: return "Parameter";

            /// Instructions
            case VK::Copy: return "Copy";
            case VK::Alloca: return "Alloca";
            case VK::Call: return "Call";
            case VK::GetElementPtr: return "GetElementPtr";
            case VK::GetMemberPtr: return "GetMemberPtr";
            case VK::Intrinsic: return "Intrinsic";
            case VK::Load: return "Load";
            case VK::Phi: return "Phi";
            case VK::Store: return "Store";

            /// Terminators
            case VK::Branch: return "Branch";
            case VK::CondBranch: return "CondBranch";
            case VK::Return: return "Return";
            case VK::Unreachable: return "Unreachable";

            /// Unary instructions
            case VK::ZExt: return "ZExt";
            case VK::SExt: return "SExt";
            case VK::Trunc: return "Trunc";
            case VK::Bitcast: return "Bitcast";
            case VK::Neg: return "Neg";
            case VK::Compl: return "Compl";

            /// Binary instructions
            case VK::Add: return "Add";
            case VK::Sub: return "Sub";
            case VK::Mul: return "Mul";
            case VK::SDiv: return "SDiv";
            case VK::UDiv: return "UDiv";
            case VK::SRem: return "SRem";
            case VK::URem: return "URem";
            case VK::Shl: return "Shl";
            case VK::Sar: return "Sar";
            case VK::Shr: return "Shr";
            case VK::And: return "And";
            case VK::Or: return "Or";
            case VK::Xor: return "Xor";

            /// Comparison instructions
            case VK::Eq: return "Eq";
            case VK::Ne: return "Ne";
            // Signed comparisons
            case VK::SLt: return "SLt";
            case VK::SLe: return "SLe";
            case VK::SGt: return "SGt";
            case VK::SGe: return "SGe";
            // Unsigned comparisons
            case VK::ULt: return "ULt";
            case VK::ULe: return "ULe";
            case VK::UGt: return "UGt";
            case VK::UGe: return "UGe";
        }
        LCC_UNREACHABLE();
    }
};

class GlobalVariable : public Value {
    std::string _name;
    Linkage _linkage;
    Value* _init;

public:
    GlobalVariable(Module* mod, Type* t, std::string name, Linkage linkage, Value* init);

    bool imported() const { return IsImportedLinkage(linkage()); }
    Value* init() { return _init; }
    auto name() const -> const std::string& { return _name; }
    Linkage linkage() const { return _linkage; }

    /// RTTI.
    static bool classof(Value* v) { return +v->kind() >= +Kind::GlobalVariable; }

    static GlobalVariable* CreateStringPtr(Module* mod, std::string name, std::string_view string_value);
};

/// IR instruction.
class Inst : public Value {
    /// So that parent can be set upon insertion.
    friend Block;

    /// IR Parser needs to update uses.
    friend parser::Parser;

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

    /// Add a use by an instruction.
    static void AddUse(Value* of_value, Value* by) {
        if (not is<Inst>(of_value)) return;
        if (not is<Inst>(by)) return;
        auto of = as<Inst>(of_value);
        auto it = rgs::find(of->user_list, by);
        if (it != of->user_list.end()) return;
        of->user_list.emplace_back(as<Inst>(by));
    }

    /// Iterate the children of an instruction. This is only
    /// to be used internally as it exposes the values and
    /// can thus be used to break the invariants of the IR if
    /// used carelessly.
    auto Children() -> Generator<Value**>;

    /// Erase this instruction without checking if it
    /// is still used by anything.
    void EraseImpl();

    /// Remove a use by an instruction.
    static void RemoveUse(Value* of_value, Value* by) {
        if (not is<Inst>(of_value)) return;
        if (not is<Inst>(by)) return;
        auto of = as<Inst>(of_value);
        auto it = rgs::find(of->user_list, by);
        if (it == of->user_list.end()) return;
        of->user_list.erase(it);
    }

    /// Replace an operand with another operand and update uses.
    void UpdateOperand(Value*& op, Value* newval) {
        if (op) RemoveUse(op, this);
        AddUse(newval, this);
        op = newval;
    }

public:
    /// Get the parent block.
    auto block() const -> Block* { return parent; }

    /// Iterate over the children of this instruction.
    auto children() const -> Generator<Value*>;

    /// Iterate over all children of a certain instruction type.
    template <std::derived_from<Value> Inst>
    auto children_of_kind() const -> Generator<Inst*> {
        for (auto v : children())
            if (auto c = cast<Inst>(v))
                co_yield c;
    }

    /// Erase this instruction from its parent block.
    void erase();

    /// Erase this instruction and all instructions that use this
    /// instruction. Be careful when using this.
    void erase_cascade();

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

    /// Replace all uses of this instruction with another
    /// value and erase this instruction from its parent
    /// block. If the replacement value is an instruction
    /// that is not inserted anywhere, it will be inserted
    /// before this instruction.
    void replace_with(Value* v);

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

    /// Set the parent function.
    void function(Function* f) { parent = f; }

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

    void insert_before(Inst* to_insert, Inst* before);
    void insert_after(Inst* to_insert, Inst* after);

    /// Get the instructions in this block.
    auto instructions() -> std::vector<Inst*>& { return inst_list; }

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
    std::vector<Parameter*> param_list{};

    /// The source location of this function.
    Location loc;

    /// The parent module of this function.
    Module* mod;

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
        Module* mod,
        std::string mangled_name,
        FunctionType* ty,
        Linkage linkage,
        CallConv calling_convention,
        Location l = {}
    );

    /// Get an iterator to the first block in this function.
    auto begin() const { return block_list.begin(); }

    void append_block(Block* b) {
        block_list.push_back(b);
        b->function(this);
    }

    /// Get the blocks in this function.
    auto blocks() const -> const std::vector<Block*>& { return block_list; }

    /// Get the calling convention of this function.
    auto call_conv() const -> CallConv { return cc; }

    /// Set the calling convention of this function.
    void call_conv(CallConv c) { cc = c; }

    /// Get an iterator to the end of the block list.
    auto end() const { return block_list.end(); }

    /// Whether this function is imported from another module.
    bool imported() const { return IsImportedLinkage(link); }

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

    /// Get the parent module of this function.
    auto module() const -> Module* { return mod; }

    /// Get the name of this function.
    auto name() const -> const std::string& { return func_name; }

    /// Set the name of this function.
    void name(std::string n) { func_name = std::move(n); }

    /// Get a parameter value.
    ///
    /// \param i The index of the parameter.
    /// \return The parameter value of the i-th parameter upon
    ///     entry to this function.
    auto param(usz i) const -> Parameter* {
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
    friend Inst;
    friend parser::Parser;

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
    friend Inst;
    friend parser::Parser;

    /// The value being called.
    Value* callee_value{};

    /// The type of the function being called.
    ///
    /// We can’t just derive this from the callee in case we want
    /// to move to using opaque pointers more, in which case the
    /// function type has to be stored separately.
    ///
    /// FIXME: Once everything is working, determine whether
    ///        we actually ended up needing this.
    FunctionType* callee_type{};

    /// The arguments to the call.
    std::vector<Value*> arguments;

    /// The calling convention of this instruction.
    CallConv cc;

    /// Whether the call is a tail call.
    bool tail_call : 1 = false;

    /// Whether this call was annotated with __builtin_inline and
    /// must be inlined regardless of the optimisation level.
    bool force_inline : 1 = false;

    /// Used by the IR Parser.
    CallInst(FunctionType* ftype, Location loc)
        : Inst(Kind::Call, ftype->ret(), loc),
          callee_type(ftype) {}

public:
    CallInst(
        Value* callee,
        FunctionType* callee_type,
        std::vector<Value*> arguments,
        Location loc = {},
        CallConv cc = {}
    ) : Inst(Kind::Call, callee_type->ret(), loc),
        callee_value(callee),
        callee_type(callee_type),
        arguments(std::move(arguments)),
        cc(cc) {
        AddUse(callee_value, this);
        for (auto a : this->arguments) AddUse(a, this);
    }

    /// Get the arguments.
    auto args() const -> const std::vector<Value*>& { return arguments; }

    /// Get the calling convention.
    auto call_conv() const -> CallConv { return cc; }

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
    friend Inst;
    friend parser::Parser;

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
        intrinsic(intrinsic), operand_list(std::move(operands)) {
        for (auto o : operand_list) AddUse(o, this);
    }

    /// Get the intrinsic ID.
    auto intrinsic_kind() const -> IntrinsicKind { return intrinsic; }

    /// Get the operands.
    auto operands() const -> const std::vector<Value*>& { return operand_list; }

    /// RTTI.
    static bool classof(Value* v) { return v->kind() == Kind::Intrinsic; }
};

/// Base class for GEP/GMP.
class GEPBaseInst : public Inst {
    friend Inst;

protected:
    /// The base pointer to index from; "p" in the equation.
    Value* pointer{};

    /// The index to offset from the base pointer; "x" in the equation.
    Value* index{};

    /// The element type.
    Type* element_type;

    GEPBaseInst(Kind k, Type* elementType, Value* ptr, Value* idx, Location loc = {})
        : Inst(k, Type::PtrTy, loc),
          pointer(ptr),
          index(idx),
          element_type(elementType) {
        /// IR Parser may cause nullptr to be passed here.
        if (pointer) AddUse(pointer, this);
        if (index) AddUse(index, this);
    }

public:
    /// Get the base type of the array.
    auto base_type() const -> Type* { return element_type; }

    /// Get the base pointer.
    auto ptr() const -> Value* { return pointer; }

    /// Get the index.
    auto idx() const -> Value* { return index; }

    /// RTTI.
    static bool classof(Value* v) {
        return v->kind() == Kind::GetElementPtr or
               v->kind() == Kind::GetMemberPtr;
    }
};

/// A "get element pointer", or GEP, instruction.
///
/// For now, GEP stores the element type as it's type, then indexes it's
/// base pointer based on that.
///
/// GEP(T, p, x) = p + (sizeof(T) * x)
///
class GEPInst : public GEPBaseInst {
    friend Inst;
    friend parser::Parser;

    /// Used by the IR parser.
    GEPInst(Type* elem, Location loc)
        : GEPBaseInst(Kind::GetElementPtr, elem, nullptr, nullptr, loc) {}

public:
    GEPInst(Type* elementType, Value* arrayPointer, Value* arrayIndex, Location loc = {})
        : GEPBaseInst(Kind::GetElementPtr, elementType, arrayPointer, arrayIndex, loc) {
        LCC_ASSERT(
            pointer->type() == Type::PtrTy or is<ArrayType>(pointer->type()),
            "GEPInst may only operate on arrays or opaque pointers, which `{}` is not",
            *pointer->type()
        );
    }

    /// RTTI.
    static bool classof(Value* v) { return v->kind() == Kind::GetElementPtr; }
};

// Get the Nth member of struct pointed to by ptr().
class GetMemberPtrInst : public GEPBaseInst {
    friend Inst;
    friend parser::Parser;

    /// TODO: Used by the IR parser.
    GetMemberPtrInst(Type* structType, Location loc)
        : GEPBaseInst(Kind::GetMemberPtr, structType, nullptr, nullptr, loc) {}

public:
    GetMemberPtrInst(Type* structType, Value* structPointer, Value* memberIndex, Location loc = {})
        : GEPBaseInst(Kind::GetMemberPtr, structType, structPointer, memberIndex, loc) {
        LCC_ASSERT(
            pointer->type() == Type::PtrTy,
            "GetMemberInst may only operate on opaque pointers, which `{}` is not",
            *pointer->type()
        );
    }

    /// Get the struct type we are calculating the offset of a member of.
    auto struct_type() const -> StructType* { return as<StructType>(element_type); }

    /// RTTI.
    static bool classof(Value* v) { return v->kind() == Kind::GetMemberPtr; }
};

/// A load instruction.
class LoadInst : public Inst {
    friend Inst;
    friend parser::Parser;

    /// The pointer to load from.
    Value* pointer{};

    /// Used by the IR parser.
    LoadInst(Type* ty, Location loc = {}) : Inst(Kind::Load, ty, loc) {}

public:
    LoadInst(Type* ty, Value* ptr, Location loc = {})
        : Inst(Kind::Load, ty, loc), pointer(ptr) {
        LCC_ASSERT(ptr->type() == Type::PtrTy, "LoadInst can only load from pointers");
        AddUse(pointer, this);
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
    friend Inst;
    friend parser::Parser;

    /// The value to store.
    Value* value{};

    /// The pointer to store to.
    Value* pointer{};

    /// Used by the IR parser.
    StoreInst(Location loc = {}) : Inst(Kind::Store, Type::VoidTy, loc) {}

public:
    StoreInst(Value* val, Value* ptr, Location loc = {})
        : Inst(Kind::Store, Type::VoidTy, loc), value(val), pointer(ptr) {
        LCC_ASSERT(ptr->type() == Type::PtrTy, "StoreInst can only store to pointers");
        AddUse(value, this);
        AddUse(pointer, this);
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
    friend Inst;
    friend parser::Parser;

    /// An incoming value.
    struct IncomingValue {
        /// The value.
        Value* value{};

        /// The block it comes from.
        Block* block{};
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

    /// Remove all operands.
    void clear() {
        for (auto& i : incoming) RemoveUse(i.value, this);
        incoming.clear();
    }

    /// Remove stale incoming values.
    ///
    /// This function removes any incoming values that are registered
    /// from blocks that are not predecessors of the block containing
    /// the PHI. If the PHI is not inserted in a block, this is a no-op.
    void drop_stale_operands() {
        if (not block()) return;
        std::erase_if(incoming, [&](const IncomingValue& elem) {
            auto should_erase = not block()->has_predecessor(elem.block);
            if (should_erase) RemoveUse(elem.value, this);
            return should_erase;
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
        auto it = rgs::find(incoming, block, &IncomingValue::block);
        if (it == incoming.end()) return;
        RemoveUse(it->value, this);
        incoming.erase(it);
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
        if (existing == incoming.end()) incoming.emplace_back(value, block);
        else {
            RemoveUse(existing->value, this);
            existing->value = value;
        }

        /// Doing this in this order ensures that this also works
        /// if someone calls this on a block+value combination that
        /// already exists.
        AddUse(value, this);
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
    friend Inst;
    friend parser::Parser;

    /// The block to branch to.
    Block* target_block{};

    /// Used by the IR parser.
    BranchInst(Location loc = {}) : Inst(Kind::Branch, Type::VoidTy, loc) {}

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
    friend Inst;
    friend parser::Parser;

    /// The condition.
    Value* condition{};

    /// The block to branch to if the condition is true.
    Block* then_{};

    /// The block to branch to if the condition is false.
    Block* else_{};

    /// Used by the IR parser.
    CondBranchInst(Location loc = {}) : Inst(Kind::CondBranch, Type::VoidTy, loc) {}

public:
    CondBranchInst(
        Value* cond,
        Block* then_,
        Block* else_,
        Location loc = {}
    ) : Inst(Kind::CondBranch, Type::UnknownTy, loc),
        condition(cond), then_(then_), else_(else_) {
        AddUse(condition, this);
    }

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
    friend Inst;
    friend parser::Parser;

    /// The value to return.
    Value* value{};

public:
    ReturnInst(Value* val, Location loc = {})
        : Inst(Kind::Return, Type::VoidTy, loc), value(val) {
        if (value) AddUse(value, this);
    }

    /// Check if this instruction returns a value.
    auto has_value() const -> bool { return value != nullptr; }

    /// Get the value to return. This may be null if this is a void return.
    auto val() const -> Value* { return value; }

    /// Replace the value.
    void val(Value* v) { UpdateOperand(value, v); }

    /// RTTI.
    static bool classof(Value* v) { return v->kind() == Kind::Return; }
};

/// Unreachable instruction.
class UnreachableInst : public Inst {
    friend Inst;
    friend parser::Parser;

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
    friend Inst;
    friend parser::Parser;

    /// The left operand.
    Value* left{};

    /// The right operand.
    Value* right{};

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
        : Inst(k, ty, loc), left(l), right(r) {
        /// IR parser may pass nullptr here.
        if (left) AddUse(left, this);
        if (right) AddUse(right, this);
    }

    /// Get the left operand.
    auto lhs() const -> Value* { return left; }

    /// Replace the left operand.
    void lhs(Value* v) { UpdateOperand(left, v); }

    /// Get the right operand.
    auto rhs() const -> Value* { return right; }

    /// Replace the right operand.
    void rhs(Value* v) { UpdateOperand(right, v); }

    /// Swap the LHS and RHS.
    void swap_operands() { std::swap(left, right); }

    /// RTTI.
    static bool classof(Value* v) { return +v->kind() >= +Kind::Add; }
};

/// An add instruction.
class AddInst : public BinaryInst {
    friend parser::Parser;

    /// Used by the IR parser.
    AddInst(Type* ty, Location loc = {}) : BinaryInst(Kind::Add, nullptr, nullptr, ty, loc) {}

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
    friend parser::Parser;

    /// Used by the IR parser.
    SubInst(Type* ty, Location loc = {}) : BinaryInst(Kind::Sub, nullptr, nullptr, ty, loc) {}

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
    friend parser::Parser;

    /// Used by the IR parser.
    MulInst(Type* ty, Location loc = {}) : BinaryInst(Kind::Mul, nullptr, nullptr, ty, loc) {}

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
    friend parser::Parser;

    /// Used by the IR parser.
    SDivInst(Type* ty, Location loc = {}) : BinaryInst(Kind::SDiv, nullptr, nullptr, ty, loc) {}

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
    friend parser::Parser;

    /// Used by the IR parser.
    SRemInst(Type* ty, Location loc = {}) : BinaryInst(Kind::SRem, nullptr, nullptr, ty, loc) {}

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
    friend parser::Parser;

    /// Used by the IR parser.
    UDivInst(Type* ty, Location loc = {}) : BinaryInst(Kind::UDiv, nullptr, nullptr, ty, loc) {}

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
    friend parser::Parser;

    /// Used by the IR parser.
    URemInst(Type* ty, Location loc = {}) : BinaryInst(Kind::URem, nullptr, nullptr, ty, loc) {}

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
    friend parser::Parser;

    /// Used by the IR parser.
    ShlInst(Type* ty, Location loc = {}) : BinaryInst(Kind::Shl, nullptr, nullptr, ty, loc) {}

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
    friend parser::Parser;

    /// Used by the IR parser.
    SarInst(Type* ty, Location loc = {}) : BinaryInst(Kind::Sar, nullptr, nullptr, ty, loc) {}

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
    friend parser::Parser;

    /// Used by the IR parser.
    ShrInst(Type* ty, Location loc = {}) : BinaryInst(Kind::Shr, nullptr, nullptr, ty, loc) {}

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
    friend parser::Parser;

    /// Used by the IR parser.
    AndInst(Type* ty, Location loc = {}) : BinaryInst(Kind::And, nullptr, nullptr, ty, loc) {}

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
    friend parser::Parser;

    /// Used by the IR parser.
    OrInst(Type* ty, Location loc = {}) : BinaryInst(Kind::Or, nullptr, nullptr, ty, loc) {}

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
    friend parser::Parser;

    /// Used by the IR parser.
    XorInst(Type* ty, Location loc = {}) : BinaryInst(Kind::Xor, nullptr, nullptr, ty, loc) {}

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
    friend parser::Parser;

protected:
    CompareInst(Kind k, Value* l, Value* r, Location loc = {})
        : BinaryInst(k, l, r, Type::I1Ty, loc) {}

public:
    /// RTTI.
    static bool classof(Value* v) { return +v->kind() >= +Kind::Eq; }
};

/// Instruction that compares two values for equality.
class EqInst : public CompareInst {
    friend parser::Parser;

    /// Used by the IR parser.
    EqInst(Location loc = {}) : CompareInst(Kind::Eq, nullptr, nullptr, loc) {}

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
    friend parser::Parser;

    /// Used by the IR parser.
    NeInst(Location loc = {}) : CompareInst(Kind::Ne, nullptr, nullptr, loc) {}

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
    friend parser::Parser;

    /// Used by the IR parser.
    SLtInst(Location loc = {}) : CompareInst(Kind::SLt, nullptr, nullptr, loc) {}

public:
    SLtInst(Value* l, Value* r, Location loc = {})
        : CompareInst(Kind::ULt, l, r, loc) {
        AssertSameType(l, r);
    }

    /// RTTI.
    static bool classof(Value* v) { return v->kind() == Kind::SLt; }
};

class ULtInst : public CompareInst {
    friend parser::Parser;

    /// Used by the IR parser.
    ULtInst(Location loc = {}) : CompareInst(Kind::ULt, nullptr, nullptr, loc) {}

public:
    ULtInst(Value* l, Value* r, Location loc = {})
        : CompareInst(Kind::SLt, l, r, loc) {
        AssertSameType(l, r);
    }

    /// RTTI.
    static bool classof(Value* v) { return v->kind() == Kind::ULt; }
};

/// Instruction that compares two values for less-than-or-equal.
class SLeInst : public CompareInst {
    friend parser::Parser;

    /// Used by the IR parser.
    SLeInst(Location loc = {}) : CompareInst(Kind::SLe, nullptr, nullptr, loc) {}
public:
    SLeInst(Value* l, Value* r, Location loc = {})
        : CompareInst(Kind::SLe, l, r, loc) {
        AssertSameType(l, r);
    }

    /// RTTI.
    static bool classof(Value* v) { return v->kind() == Kind::SLe; }
};

class ULeInst : public CompareInst {
    friend parser::Parser;

    /// Used by the IR parser.
    ULeInst(Location loc = {}) : CompareInst(Kind::ULe, nullptr, nullptr, loc) {}
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
    friend parser::Parser;

    /// Used by the IR parser.
    SGtInst(Location loc = {}) : CompareInst(Kind::SGt, nullptr, nullptr, loc) {}

public:
    SGtInst(Value* l, Value* r, Location loc = {})
        : CompareInst(Kind::SGt, l, r, loc) {
        AssertSameType(l, r);
    }

    /// RTTI.
    static bool classof(Value* v) { return v->kind() == Kind::SGt; }
};

class UGtInst : public CompareInst {
    friend parser::Parser;

    /// Used by the IR parser.
    UGtInst(Location loc = {}) : CompareInst(Kind::UGt, nullptr, nullptr, loc) {}

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
    friend parser::Parser;

    /// Used by the IR parser.
    SGeInst(Location loc = {}) : CompareInst(Kind::SGe, nullptr, nullptr, loc) {}

public:
    SGeInst(Value* l, Value* r, Location loc = {})
        : CompareInst(Kind::SGe, l, r, loc) {
        AssertSameType(l, r);
    }

    /// RTTI.
    static bool classof(Value* v) { return v->kind() == Kind::SGe; }
};

class UGeInst : public CompareInst {
    friend parser::Parser;

    /// Used by the IR parser.
    UGeInst(Location loc = {}) : CompareInst(Kind::UGe, nullptr, nullptr, loc) {}

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
    friend Inst;
    friend parser::Parser;

    Value* op{};

protected:
    UnaryInstBase(Kind k, Value* v, Type* ty, Location loc = {})
        : Inst(k, ty, loc), op(v) {
        /// IR parser may pass nullptr here.
        if (op) AddUse(op, this);
    }

public:
    /// Get the operand.
    Value* operand() const { return op; }

    /// Set the operand.
    void operand(Value* v) { op = v; }

    static bool classof(Value* v) {
        return +v->kind() >= +Kind::ZExt and +v->kind() <= +Kind::Compl;
    }
};

/// SSA copy. Used during lowering only.
class CopyInst : public UnaryInstBase {
    friend parser::Parser;

    /// Used by the IR parser.
    CopyInst(Type* ty, Location loc = {}) : UnaryInstBase(Kind::Copy, nullptr, ty, loc) {}

public:
    CopyInst(Value* v, Location loc = {})
        : UnaryInstBase(Kind::Copy, v, v->type(), loc) {}

    /// RTTI.
    static bool classof(Value* v) { return v->kind() == Kind::Copy; }
};

/// Zero-extend an integer value.
class ZExtInst : public UnaryInstBase {
    friend parser::Parser;

    /// Used by the IR parser.
    ZExtInst(Type* ty, Location loc = {}) : UnaryInstBase(Kind::ZExt, nullptr, ty, loc) {}

public:
    ZExtInst(Value* v, Type* ty, Location loc = {})
        : UnaryInstBase(Kind::ZExt, v, ty, loc) {}

    /// RTTI.
    static bool classof(Value* v) { return v->kind() == Kind::ZExt; }
};

/// Sign-extend an integer value.
class SExtInst : public UnaryInstBase {
    friend parser::Parser;

    /// Used by the IR parser.
    SExtInst(Type* ty, Location loc = {}) : UnaryInstBase(Kind::SExt, nullptr, ty, loc) {}

public:
    SExtInst(Value* v, Type* ty, Location loc = {})
        : UnaryInstBase(Kind::SExt, v, ty, loc) {}

    /// RTTI.
    static bool classof(Value* v) { return v->kind() == Kind::SExt; }
};

/// Truncate an integer value.
class TruncInst : public UnaryInstBase {
    friend parser::Parser;

    /// Used by the IR parser.
    TruncInst(Type* ty, Location loc = {}) : UnaryInstBase(Kind::Trunc, nullptr, ty, loc) {}

public:
    TruncInst(Value* v, Type* ty, Location loc = {})
        : UnaryInstBase(Kind::Trunc, v, ty, loc) {}

    /// RTTI.
    static bool classof(Value* v) { return v->kind() == Kind::Trunc; }
};

/// Bitcast a value to another type.
class BitcastInst : public UnaryInstBase {
    friend parser::Parser;

    /// Used by the IR parser.
    BitcastInst(Type* ty, Location loc = {}) : UnaryInstBase(Kind::Bitcast, nullptr, ty, loc) {}

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
    friend parser::Parser;

    /// Used by the IR parser.
    NegInst(Type* ty, Location loc = {}) : UnaryInstBase(Kind::Neg, nullptr, ty, loc) {}

public:
    NegInst(Value* v, Location loc = {})
        : UnaryInstBase(Kind::Neg, v, v->type(), loc) {}

    /// RTTI.
    static bool classof(Value* v) { return v->kind() == Kind::Neg; }
};

/// Bitwise complement of an integer value.
class ComplInst : public UnaryInstBase {
    friend parser::Parser;

    /// Used by the IR parser.
    ComplInst(Type* ty, Location loc = {}) : UnaryInstBase(Kind::Compl, nullptr, ty, loc) {}

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
    aint _value;

public:
    IntegerConstant(Type* ty, aint value)
        : Value(Kind::IntegerConstant, ty), _value(value.trunc(u8(as<IntegerType>(ty)->bits()))) {
        LCC_ASSERT(as<IntegerType>(ty)->bits() <= 64, "Integer constants must be 64 bits or less");
    }

    /// Get the value.
    aint value() const { return _value; }

    /// RTTI.
    static bool classof(Value* v) { return v->kind() == Kind::IntegerConstant; }
};

/// Packed array.
///
/// This is used for constant arrays that are known at compile time,
/// such as strings or constant array literals.
class ArrayConstant : public Value {
    std::vector<char> _data;
    bool _is_string_literal = false;

public:
    ArrayConstant(Type* ty, std::vector<char> _data, bool is_string_literal = false)
        : Value(Kind::ArrayConstant, ty),
          _data(std::move(_data)),
          _is_string_literal(is_string_literal) {}

    /// Get an iterator to the start of the data.
    auto begin() const { return _data.begin(); }

    /// Get the data.
    auto data() const -> const char* { return _data.data(); }

    /// Get an iterator to the end of the data.
    auto end() const { return _data.end(); }

    /// Whether this is a string literal.
    ///
    /// This only affects how this array constant is printed
    /// and should not be used for anything else.
    auto is_string_literal() const -> bool { return _is_string_literal; }

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
