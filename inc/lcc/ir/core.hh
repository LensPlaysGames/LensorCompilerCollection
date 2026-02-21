#ifndef LCC_IR_CORE_HH
#define LCC_IR_CORE_HH

#include <lcc/core.hh>
#include <lcc/forward.hh>
#include <lcc/ir/type.hh>
#include <lcc/location.hh>
#include <lcc/utils.hh>
#include <lcc/utils/aint.hh>
#include <lcc/utils/fractionals.hh>
#include <lcc/utils/generator.hh>
#include <lcc/utils/iterator.hh>
#include <lcc/utils/rtti.hh>

#include <algorithm>
#include <concepts>
#include <functional>
#include <span>
#include <string>
#include <string_view>
#include <type_traits>
#include <utility>
#include <vector>

namespace lcc {
class Parameter;
class PhiInst;

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
        IntegerConstant,
        FractionalConstant,
        ArrayConstant,
        Poison,

        /// Values that track their users.
        Block,
        Function,
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
    Kind value_kind{};

    /// Every value has a type.
    Type* value_type{};

protected:
    explicit Value(Kind kind, Type* ty = Type::UnknownTy)
        : value_kind(kind), value_type(ty) {}

public:
    virtual ~Value() = default;

    /// Disallow allocating these directly.
    auto operator new(size_t) -> void* = delete;
    auto operator new(size_t sz, Module& /*_*/) -> void* {
        return ::operator new(sz);
    }

    /// Get the kind of this value for RTTI.
    [[nodiscard]]
    auto kind() const -> Kind { return value_kind; }

    /// Get the type of this value.
    [[nodiscard]]
    auto type() const -> Type* { return value_type; }

    /// Get a reference to the type of this value.
    /// Sometimes, when lowering, we need to change the type of a value.
    [[nodiscard]]
    Type*& type_reference() { return value_type; }

    /// Print this value for debugging.
    auto string() const -> std::string;
    /// Print this value for debugging.
    void print() const;

    [[nodiscard]]
    static auto ToString(Value::Kind v) -> std::string {
        using VK = Value::Kind;
        switch (v) {
            /// Values
            case VK::Block: return "Block";
            case VK::Function: return "Function";
            case VK::IntegerConstant: return "IntegerConstant";
            case VK::FractionalConstant: return "FractionalConstant";
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

/// Value with a user list.
class UseTrackingValue : public Value {
    friend Inst;

    /// Users of this value.
    std::vector<Inst*> user_list;

protected:
    explicit UseTrackingValue(Kind k, Type* t = Type::UnknownTy) : Value(k, t) {}

public:
    /// Get the users of this value.
    [[nodiscard]]
    auto users() const -> const std::vector<Inst*>& { return user_list; }

    /// RTTI.
    static auto classof(const Value* v) -> bool { return v->kind() >= Value::Kind::Block; }
};

struct IRName {
    std::string name;
    Linkage linkage;
};

class GlobalVariable : public UseTrackingValue {
    std::vector<IRName> _names;
    Value* _init;
    Type* _allocated_type;

public:
    explicit GlobalVariable(
        Module* mod,
        Type* t,
        std::string name,
        Linkage linkage,
        Value* init
    );

    [[nodiscard]]
    auto allocated_type() -> Type* { return _allocated_type; }
    [[nodiscard]]
    auto init() -> Value* { return _init; }
    [[nodiscard]]
    auto names() const -> std::vector<IRName> { return _names; }

    /// RTTI.
    [[nodiscard]]
    static auto classof(Value* v) -> bool { return v->kind() == Kind::GlobalVariable; }

    [[nodiscard]]
    static auto CreateStringPtr(
        Module* mod,
        std::string name,
        std::string_view string_value
    ) -> GlobalVariable*;
};

/// IR instruction.
class Inst : public UseTrackingValue {
    /// So that parent can be set upon insertion.
    friend Block;

    /// IR Parser needs to update uses.
    friend parser::Parser;

    /// Associated machine instruction during early codegen.
    MInst* minst{};

    /// The parent block that this instruction is inserted in.
    Block* parent{};

    /// Source location of this instruction.
    Location _location;

protected:
    explicit Inst(Kind k, Type* t, Location location = {})
        : UseTrackingValue(k, t), _location(location) {}

    /// Add a use by an instruction.
    static void AddUse(Value* of_value, Inst* by) {
        if (not is<UseTrackingValue>(of_value)) return;
        auto* of = as<UseTrackingValue>(of_value);
        auto it = rgs::find(of->user_list, by);
        if (it != of->user_list.end()) return;
        of->user_list.emplace_back(by);
    }

    /// Iterate the children of an instruction. This is only
    /// to be used internally as it exposes the values and
    /// can thus be used to break the invariants of the IR if
    /// used carelessly.
    ///
    /// Unlike `children()`, this function does *not* iterate
    /// over any blocks.
    ///
    /// This is a low-level API. Prefer to use `children()`
    /// instead.
    [[nodiscard]]
    auto Children() -> Generator<Value**>;

    /// Erase this instruction without checking if it
    /// is still used by anything.
    void EraseImpl();

    /// Remove a use by an instruction.
    static void RemoveUse(Value* of_value, Inst* by) {
        LCC_ASSERT(of_value, "Expected non-null ptr");
        if (not is<UseTrackingValue>(of_value)) return;
        auto* of = as<UseTrackingValue>(of_value);

        LCC_ASSERT(by, "Expected non-null ptr");
        auto it = rgs::find(of->user_list, by);
        if (it == of->user_list.end()) return;
        of->user_list.erase(it);
    }

    /// Replace an operand with another operand and update uses.
    template <std::derived_from<Value> T>
    void UpdateOperand(T*& op, T* newval) {
        if (op) RemoveUse(op, this);
        AddUse(newval, this);
        op = newval;
    }

public:
    ~Inst() override = default;

    /// Get the parent block.
    [[nodiscard]]
    auto block() const -> Block* { return parent; }

    /// Iterate over the children of this instruction.
    [[nodiscard]]
    auto children() const -> Generator<Value*>;

    /// Iterate over all children of a certain instruction type.
    template <std::derived_from<Value> Inst>
    [[nodiscard]]
    auto children_of_kind() const -> Generator<Inst*> {
        for (auto* v : children())
            if (auto* c = cast<Inst>(v))
                co_yield c;
    }

    /// Erase this instruction from its parent block.
    void erase();

    /// Erase this instruction and all instructions that use this
    /// instruction. Be careful when using this.
    void erase_cascade();

    void insert_before(Inst* to_insert);

    /// Iterate over all instructions before (and not including) this one.
    auto instructions_before_this() -> std::span<Inst*>;

    /// Check if this is a terminator instruction.
    [[nodiscard]]
    auto is_terminator() const -> bool {
        return kind() >= Value::Kind::Branch and kind() <= Value::Kind::Unreachable;
    }

    /// Get the source location of this instruction.
    [[nodiscard]]
    auto location() const -> Location { return _location; }

    /// Get the associated machine instruction.
    [[nodiscard]]
    auto machine_inst() const -> MInst* { return minst; }

    /// Set the associated machine instruction.
    void machine_inst(MInst* m) { minst = m; }

    /// Replace children of this instruction.
    ///
    /// PLEASE DO NOT INSERT OR REMOVE INSTRUCTIONS IN THE CALLBACK.
    /// If you need to do this, first iterate children(), inserting
    /// instructions as needed, keeping a map of old child -> new child. Once
    /// you iterated all children, call replace_children with a callback that
    /// returns the mapped child, if present, or nullptr.
    /// : std::unordered_map<Value*, Value*> child_replacements{};
    /// : // ... iterate children, insert instructions, etc.
    /// : instruction->replace_children([&](Value* c) -> Value* {
    /// :     if (child_replacements.contains(c))
    /// :         return child_replacements.at(c);
    /// :     return nullptr;
    /// : });
    ///
    /// This iterates over all children of this instruction and
    /// calls a callback to determine whether each one of them
    /// should be replaced. If the callback returns a non-null
    /// Value*, the child is replaced.
    ///
    ///
    /// \tparam InstType If provided, only children of this type
    ///     will be replaced.
    /// \param cb The callback to call for each child.
    template <typename T = Value, typename Callable>
    void replace_children(Callable cb) {
        for (auto** child : Children()) {
            auto c = cast<T>(*child);
            if (not c) continue;
            if (auto replacement = std::invoke(cb, c))
                UpdateOperand(*child, replacement);
        }
    }

    /// Replace all uses of this instruction with another
    /// value and erase this instruction from its parent
    /// block. If the replacement value is an instruction
    /// that is not inserted anywhere, it will be inserted
    /// before this instruction.
    void replace_with(Value* v);

    /// RTTI.
    [[nodiscard]]
    static auto classof(Value* v) -> bool { return +v->kind() >= +Kind::Alloca; }
};
static_assert(
    std::has_virtual_destructor_v<Inst>,
    "IR Inst type must have virtual destructor (so that derived class allocated members may be freed)"
);

/// A basic block.
class Block : public UseTrackingValue {
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
    explicit Block(std::string n = "")
        : UseTrackingValue(Kind::Block),
          block_name(std::move(n)) {}

    /// Get an iterator to the first instruction in this block.
    [[nodiscard]]
    auto begin() const -> ConstIterator { return {inst_list, inst_list.begin()}; }

    /// Get an iterator to the last instruction in this block.
    [[nodiscard]]
    auto end() const -> ConstIterator { return {inst_list, inst_list.end()}; }

    /// Check whether this block has a terminator.
    [[nodiscard]]
    auto closed() const -> bool { return terminator() != nullptr; }

    /// Create a PHI node in this block.
    ///
    /// For scheduling purposes, all PHI nodes in a block
    /// *must* be at the very beginning of the block, so
    /// this will insert a new PHI after all existing PHIs.
    auto create_phi(Type* type, Location location = {}) -> PhiInst*;

    /// Erase this block and all instructions in it.
    void erase();

    /// Get the parent function.
    [[nodiscard]]
    auto function() const -> Function* { return parent; }

    /// Set the parent function.
    void function(Function* f) { parent = f; }

    /// Check if this block has another block as one
    /// of its predecessors.
    [[nodiscard]]
    auto has_predecessor(Block* block) const -> bool;

    /// Get the index of this block in the parent function.
    [[nodiscard]]
    auto id() const -> usz;

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
    [[nodiscard]]
    auto instructions() -> std::vector<Inst*>& { return inst_list; }

    /// Get the associated machine block.
    [[nodiscard]]
    auto machine_block() const -> MBlock* { return mblock; }

    /// Set the associated machine block.
    void machine_block(MBlock* m) { mblock = m; }

    /// Merge another block into this one.
    ///
    /// This operation is only valid if:
    ///
    ///     - The other block has no predecessors, other than
    ///       possibly this block, and
    ///
    ///     - This block has no terminator, or its terminator
    ///       is a direct branch to the other block, and
    ///
    ///     - The two blocks are inserted in different functions;
    ///       however, it is valid for one or neither of the blocks
    ///       to be inserted in a function.
    ///
    /// The other block is removed.
    void merge(Block* b);

    /// Get the name of this block.
    [[nodiscard]]
    auto name() const -> const std::string& { return block_name; }

    /// Set the name of this block.
    void name(std::string n) { block_name = std::move(n); }

    /// Get the number of predecessors of this block.
    [[nodiscard]]
    auto predecessor_count() const -> usz;

    /// Get the successors of this block.
    ///
    /// If the block’s terminator is a conditional branch whose
    /// then and else blocks are the same, only one successor
    /// will be returned.
    [[nodiscard]]
    auto successors() const -> Generator<Block*>;

    /// Get the number of different blocks this block may branch to.
    [[nodiscard]]
    auto successor_count() const -> usz;

    /// Get the terminator instruction of this block; may return nullptr.
    [[nodiscard]]
    auto terminator() const -> Inst* {
        if (inst_list.empty()) return nullptr;
        auto* i = inst_list.back();
        if (not i->is_terminator()) return nullptr;
        return i;
    }

    /// RTTI.
    [[nodiscard]]
    static auto classof(Value* v) -> bool { return v->kind() == Kind::Block; }
};

/// An IR function.
class Function : public UseTrackingValue {
private:
    using Iterator = utils::VectorIterator<Block*>;
    using ConstIterator = utils::VectorConstIterator<Block*>;

    std::vector<IRName> func_names;

    /// The blocks in this function.
    std::vector<Block*> block_list{};

    /// The parameters of this function. Parameter instructions
    /// are never created manually; instead, they always reference
    /// one of the parameter instructions inserted here.
    std::vector<Parameter*> param_list{};

    /// The source location of this function.
    Location _location;

    /// The parent module of this function.
    Module* mod;

    /// The associated machine function.
    MFunction* mfunc{};

    /// Function attributes.
    /// TODO.

    /// The calling convention of this function.
    CallConv cc;

public:
    // TODO: Re-do to take an IRName
    Function(
        Module* mod,
        std::string mangled_name,
        FunctionType* ty,
        Linkage linkage,
        CallConv calling_convention,
        Location location = {}
    );

    /// Get an iterator to the first block in this function.
    [[nodiscard]]
    auto begin() const { return block_list.begin(); }

    void append_block(Block* b) {
        block_list.push_back(b);
        b->function(this);
    }

    /// Get the blocks in this function.
    [[nodiscard]]
    auto blocks() -> std::vector<Block*>& { return block_list; }

    /// Get the calling convention of this function.
    [[nodiscard]]
    auto call_conv() const -> CallConv { return cc; }

    /// Set the calling convention of this function.
    void call_conv(CallConv c) { cc = c; }

    /// Get an iterator to the end of the block list.
    [[nodiscard]]
    auto end() const { return block_list.end(); }

    /// Get the entry block of this function, if any.
    [[nodiscard]]
    auto entry() const -> Block* {
        if (block_list.empty()) return nullptr;
        return block_list.front();
    }

    /// Get the source location of this function.
    [[nodiscard]]
    auto location() const -> Location { return _location; }

    /// Get the associated machine function.
    [[nodiscard]]
    auto machine_function() const -> MFunction* { return mfunc; }

    /// Set the associated machine function.
    void machine_function(MFunction* m) { mfunc = m; }

    /// Get the parent module of this function.
    [[nodiscard]]
    auto module() const -> Module* { return mod; }

    // Get the names of this function.
    [[nodiscard]]
    auto names() const -> std::vector<IRName> { return func_names; }

    [[nodiscard]]
    auto has_name(std::string_view name) const {
        auto found = std::find_if(
            func_names.begin(),
            func_names.end(),
            [&](const IRName& candidate) { return candidate.name == name; }
        );
        return found != func_names.end();
    }

    [[nodiscard]]
    auto has_one_of_names(const std::vector<IRName>& function_names) const {
        return rgs::any_of(function_names, [this](const IRName& n) {
            return has_name(n.name);
        });
    }

    // Add a name to this function.
    void add_name(std::string n, Linkage l) {
        func_names.push_back({std::move(n), l});
    }

    /// Get a parameter value.
    ///
    /// \param i The index of the parameter.
    /// \return The parameter value of the i-th parameter upon
    ///     entry to this function.
    [[nodiscard]]
    auto param(usz i) const -> Parameter* {
        LCC_ASSERT(i < param_list.size(), "Parameter index out of bounds");
        return param_list[i];
    }

    /// Get the number of parameters.
    [[nodiscard]]
    auto param_count() const -> size_t { return param_list.size(); }

    // NOTE: For lowering
    [[nodiscard]]
    auto params() -> std::vector<Parameter*>& { return param_list; }

    /// RTTI.
    [[nodiscard]]
    static auto classof(Value* v) -> bool { return v->kind() == Kind::Function; }
};

/// A parameter reference.
class Parameter : public UseTrackingValue {
    /// Only the Function class should be able to create these.
    friend Function;
    // NOTE: For lowering
    friend Module;

    /// The parameter index.
    u32 i{};

    Parameter(Type* ty, u32 idx) : UseTrackingValue(Kind::Parameter, ty), i(idx) {}
public:
    /// Get the parameter index.
    [[nodiscard]]
    auto index() const -> u32 { return i; }

    /// NOTE: For lowering
    [[nodiscard]]
    auto index() -> u32& { return i; }

    /// RTTI.
    [[nodiscard]]
    static auto classof(Value* v) -> bool { return v->kind() == Kind::Parameter; }
};

/// ============================================================================
///  Instructions
/// ============================================================================
/// A stack allocation.
class AllocaInst : public Inst {
    friend Inst;
    friend parser::Parser;

    Type* _allocated_type{};

public:
    explicit AllocaInst(Type* ty, Location location = {})
        : Inst(Kind::Alloca, Type::PtrTy, location), _allocated_type(ty) {}

    [[nodiscard]]
    auto allocated_type() -> Type* { return _allocated_type; }

    /// RTTI.
    [[nodiscard]]
    static auto classof(Value* v) -> bool { return v->kind() == Kind::Alloca; }
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
    explicit CallInst(FunctionType* ftype, Location location)
        : Inst(Kind::Call, ftype->ret(), location),
          callee_type(ftype), cc(CallConv::C) {}

public:
    explicit CallInst(
        Value* callee,
        FunctionType* callee_ty,
        std::vector<Value*> arguments_,
        Location location = {},
        CallConv calling_convention = {}
    ) : Inst(Kind::Call, callee_ty->ret(), location),
        callee_value(callee),
        callee_type(callee_ty),
        arguments(std::move(arguments_)),
        cc(calling_convention) {
        AddUse(callee_value, this);
        for (auto* a : this->arguments) AddUse(a, this);
    }

    /// Get the arguments.
    [[nodiscard]]
    auto args() const -> const std::vector<Value*>& { return arguments; }

    /// Get the calling convention.
    [[nodiscard]]
    auto call_conv() const -> CallConv { return cc; }

    /// Get the callee.
    [[nodiscard]]
    auto callee() const -> Value* { return callee_value; }

    /// Get the callee type.
    [[nodiscard]]
    auto function_type() const -> FunctionType* { return callee_type; }

    /// Get whether this is annotated with __builtin_inline.
    [[nodiscard]]
    auto is_force_inline() const -> bool { return force_inline; }

    /// Get whether this is a tail call.
    [[nodiscard]]
    auto is_tail_call() const -> bool { return tail_call; }

    /// Mark that this has to be inlined.
    void set_force_inline() { force_inline = true; }

    /// Set whether this is a tail call.
    void set_tail_call() { tail_call = true; }

    /// RTTI.
    [[nodiscard]]
    static auto classof(Value* v) -> bool { return v->kind() == Kind::Call; }
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
    explicit IntrinsicInst(
        IntrinsicKind intrinsic_,
        std::vector<Value*> operands,
        Location location = {}
    ) : Inst(Kind::Intrinsic, Type::UnknownTy, location),
        intrinsic(intrinsic_), operand_list(std::move(operands)) {
        for (auto* o : operand_list) AddUse(o, this);
    }

    /// Get the intrinsic ID.
    [[nodiscard]]
    auto intrinsic_kind() const -> IntrinsicKind { return intrinsic; }

    /// Get the operands.
    [[nodiscard]]
    auto operands() const -> const std::vector<Value*>& { return operand_list; }

    /// RTTI.
    [[nodiscard]]
    static auto classof(Value* v) -> bool { return v->kind() == Kind::Intrinsic; }
};

/// Base class for GEP/GMP.
class GEPBaseInst : public Inst {
    friend Inst;

protected:
    /// The base pointer to index from.
    Value* pointer{};

    /// The index to offset from the base pointer.
    Value* index{};

    /// The element type.
    Type* element_type{};

    explicit GEPBaseInst(Kind k, Type* elementType, Value* ptr, Value* idx, Location location = {})
        : Inst(k, Type::PtrTy, location),
          pointer(ptr),
          index(idx),
          element_type(elementType) {
        /// IR Parser may cause nullptr to be passed here.
        if (pointer) AddUse(pointer, this);
        if (index) AddUse(index, this);
    }

public:
    [[nodiscard]]
    auto base_type() const -> Type* { return element_type; }

    /// Get the base pointer.
    [[nodiscard]]
    auto ptr() const -> Value* { return pointer; }

    /// Get the index.
    [[nodiscard]]
    auto idx() const -> Value* { return index; }

    /// RTTI.
    [[nodiscard]]
    static auto classof(Value* v) -> bool {
        return v->kind() == Kind::GetElementPtr
            or v->kind() == Kind::GetMemberPtr;
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
    explicit GEPInst(Type* elem, Location location)
        : GEPBaseInst(Kind::GetElementPtr, elem, nullptr, nullptr, location) {}

public:
    explicit GEPInst(Type* elementType, Value* arrayPointer, Value* arrayIndex, Location location = {})
        : GEPBaseInst(Kind::GetElementPtr, elementType, arrayPointer, arrayIndex, location) {
        LCC_ASSERT(
            pointer->type() == Type::PtrTy or is<ArrayType>(pointer->type()),
            "GEPInst may only operate on arrays or opaque pointers, which `{}` is not",
            *pointer->type()
        );
    }

    /// RTTI.
    [[nodiscard]]
    static auto classof(Value* v) -> bool { return v->kind() == Kind::GetElementPtr; }
};

// Get the Nth member of struct pointed to by ptr().
class GetMemberPtrInst : public GEPBaseInst {
    friend Inst;
    friend parser::Parser;

    // NOTE: Used by the IR parser.
    explicit GetMemberPtrInst(Type* structType, Location location)
        : GEPBaseInst(Kind::GetMemberPtr, structType, nullptr, nullptr, location) {}

public:
    explicit GetMemberPtrInst(Type* structType, Value* structPointer, Value* memberIndex, Location location = {})
        : GEPBaseInst(Kind::GetMemberPtr, structType, structPointer, memberIndex, location) {
        LCC_ASSERT(
            pointer->type() == Type::PtrTy,
            "GetMemberInst may only operate on opaque pointers, which `{}` is not",
            *pointer->type()
        );
    }

    /// Get the struct type we are calculating the offset of a member of.
    [[nodiscard]]
    auto struct_type() const -> StructType* { return as<StructType>(element_type); }

    /// RTTI.
    [[nodiscard]]
    static auto classof(Value* v) -> bool { return v->kind() == Kind::GetMemberPtr; }
};

/// A load instruction.
class LoadInst : public Inst {
    friend Inst;
    friend parser::Parser;

    /// The pointer to load from.
    Value* pointer{};

    /// Used by the IR parser.
    explicit LoadInst(Type* ty, Location location = {}) : Inst(Kind::Load, ty, location) {}

public:
    explicit LoadInst(Type* ty, Value* ptr, Location location = {})
        : Inst(Kind::Load, ty, location), pointer(ptr) {
        LCC_ASSERT(ptr->type() == Type::PtrTy, "IR: LoadInst can only load from pointers, which {} is not", *ty);
        AddUse(pointer, this);
    }

    /// Get the pointer to load from.
    [[nodiscard]]
    auto ptr() const -> Value* { return pointer; }

    /// Replace the pointer.
    void ptr(Value* v) { UpdateOperand(pointer, v); }

    /// RTTI.
    [[nodiscard]]
    static auto classof(Value* v) -> bool { return v->kind() == Kind::Load; }
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
    explicit StoreInst(Location location = {}) : Inst(Kind::Store, Type::VoidTy, location) {}

public:
    explicit StoreInst(Value* val, Value* ptr, Location location = {})
        : Inst(Kind::Store, Type::VoidTy, location), value(val), pointer(ptr) {
        LCC_ASSERT(ptr->type() == Type::PtrTy, "StoreInst can only store to pointers");
        AddUse(value, this);
        AddUse(pointer, this);
    }

    /// Get the value to store.
    [[nodiscard]]
    auto val() const -> Value* { return value; }

    /// Replace the value.
    void val(Value* v) { UpdateOperand(value, v); }

    /// Get the pointer to store to.
    [[nodiscard]]
    auto ptr() const -> Value* { return pointer; }

    /// Replace the pointer.
    void ptr(Value* v) { UpdateOperand(pointer, v); }

    /// RTTI.
    [[nodiscard]]
    static auto classof(Value* v) -> bool { return v->kind() == Kind::Store; }
};

/// PHI instruction.
class PhiInst : public Inst {
    friend Inst;
    friend parser::Parser;

public:
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

private:
    /// The incoming values.
    std::vector<IncomingValue> incoming{};

public:
    explicit PhiInst(Type* ty, Location location = {})
        : Inst(Kind::Phi, ty, location) {}

    /// Get an iterator to the start of the incoming values.
    [[nodiscard]]
    auto begin() const -> ConstIterator { return {incoming, incoming.begin()}; }

    /// Remove all operands.
    void clear() {
        for (auto& i : incoming) {
            RemoveUse(i.value, this);
            RemoveUse(i.block, this);
        }

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
            if (should_erase) {
                RemoveUse(elem.value, this);
                RemoveUse(elem.block, this);
            }
            return should_erase;
        });
    }

    /// Get an iterator to the end of the incoming values.
    [[nodiscard]]
    auto end() const -> ConstIterator { return {incoming, incoming.end()}; }

    /// Get the incoming values.
    [[nodiscard]]
    auto operands() const -> const std::vector<IncomingValue>& { return incoming; }

    /// Get the incoming value for a block, if any.
    auto get_incoming(Block* b) -> Value* {
        auto it = rgs::find(incoming, b, &IncomingValue::block);
        if (it == incoming.end()) return nullptr;
        return it->value;
    }

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
        RemoveUse(it->block, this);
        incoming.erase(it);
    }

    /// Replace an incoming value for a block.
    ///
    /// This replaces the incoming value with another value and
    /// does nothing if there is no entry in the PHI from that
    /// block.
    void replace_incoming_if_present(Block* from_block, Value* new_val) {
        auto it = rgs::find(incoming, from_block, &IncomingValue::block);
        if (it == incoming.end()) return;
        UpdateOperand(it->value, new_val);
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
        AddUse(block, this);
    }

    /// RTTI.
    [[nodiscard]]
    static auto classof(Value* v) -> bool { return v->kind() == Kind::Phi; }
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
    explicit BranchInst(Location location = {}) : Inst(Kind::Branch, Type::VoidTy, location) {}

public:
    explicit BranchInst(Block* target, Location location = {})
        : Inst(Kind::Branch, Type::UnknownTy, location), target_block(target) {
        AddUse(target_block, this);
    }

    /// Get the target block.
    [[nodiscard]]
    auto target() const -> Block* { return target_block; }

    /// Replace the target with another block.
    void target(Block* b) { UpdateOperand(target_block, b); }

    /// RTTI.
    [[nodiscard]]
    static auto classof(Value* v) -> bool { return v->kind() == Kind::Branch; }
};

/// Conditional branch instruction.
class CondBranchInst : public Inst {
    friend Inst;
    friend parser::Parser;

    /// The condition.
    Value* condition{};

    /// The block to branch to if the condition is true.
    Block* then{};

    /// The block to branch to if the condition is false.
    Block* otherwise{};

    /// Used by the IR parser.
    explicit CondBranchInst(Location location = {}) : Inst(Kind::CondBranch, Type::VoidTy, location) {}

public:
    explicit CondBranchInst(
        Value* cond,
        Block* then_,
        Block* otherwise_,
        Location location = {}
    ) : Inst(Kind::CondBranch, Type::UnknownTy, location),
        condition(cond), then(then_), otherwise(otherwise_) {
        AddUse(condition, this);
        AddUse(then, this);
        AddUse(otherwise, this);
    }

    /// Get the condition.
    [[nodiscard]]
    auto cond() const -> Value* { return condition; }

    /// Replace the condition.
    void cond(Value* v) { UpdateOperand(condition, v); }

    /// Get the block to branch to if the condition is false.
    [[nodiscard]]
    auto else_block() const -> Block* { return otherwise; }

    /// Replace the else block.
    void else_block(Block* b) { UpdateOperand(otherwise, b); }

    /// Get the block to branch to if the condition is true.
    [[nodiscard]]
    auto then_block() const -> Block* { return then; }

    /// Replace the then block.
    void then_block(Block* b) { UpdateOperand(then, b); }

    /// RTTI.
    [[nodiscard]]
    static auto classof(Value* v) -> bool { return v->kind() == Kind::CondBranch; }
};

/// Return instruction.
class ReturnInst : public Inst {
    friend Inst;
    friend parser::Parser;

    /// The value to return.
    Value* value{};

public:
    explicit ReturnInst(Value* val, Location location = {})
        : Inst(Kind::Return, Type::VoidTy, location), value(val) {
        if (value) AddUse(value, this);
    }

    /// Check if this instruction returns a value.
    [[nodiscard]]
    auto has_value() const -> bool { return value != nullptr; }

    /// Get the value to return. This may be null if this is a void return.
    [[nodiscard]]
    auto val() const -> Value* { return value; }

    /// Replace the value.
    void val(Value* v) { UpdateOperand(value, v); }

    /// RTTI.
    [[nodiscard]]
    static auto classof(Value* v) -> bool { return v->kind() == Kind::Return; }
};

/// Unreachable instruction.
class UnreachableInst : public Inst {
    friend Inst;
    friend parser::Parser;

public:
    explicit UnreachableInst(Location location = {})
        : Inst(Kind::Unreachable, Type::VoidTy, location) {}

    /// RTTI.
    [[nodiscard]]
    static auto classof(Value* v) -> bool { return v->kind() == Kind::Unreachable; }
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
    static void AssertSameType(Value* l, Value* r) {
        LCC_ASSERT(
            l->type() == r->type(),
            "Left and right side of binary operand (check backtrace for which one) must have the same type, but was {} and {}",
            *l->type(),
            *r->type()
        );
    }

public:
    explicit BinaryInst(Kind k, Value* lhs, Value* rhs, Type* ty, Location location = {})
        : Inst(k, ty, location), left(lhs), right(rhs) {
        /// IR parser may pass nullptr here.
        if (left) AddUse(left, this);
        if (right) AddUse(right, this);
    }

    /// Get the left operand.
    [[nodiscard]]
    auto lhs() const -> Value* { return left; }

    /// Replace the left operand.
    void lhs(Value* v) { UpdateOperand(left, v); }

    /// Get the right operand.
    [[nodiscard]]
    auto rhs() const -> Value* { return right; }

    /// Replace the right operand.
    void rhs(Value* v) { UpdateOperand(right, v); }

    /// Swap the LHS and RHS.
    void swap_operands() { std::swap(left, right); }

    /// RTTI.
    [[nodiscard]]
    static auto classof(Value* v) -> bool { return +v->kind() >= +Kind::Add; }
};

/// An add instruction.
class AddInst : public BinaryInst {
    friend parser::Parser;

    /// Used by the IR parser.
    explicit AddInst(Type* ty, Location location = {}) : BinaryInst(Kind::Add, nullptr, nullptr, ty, location) {}

public:
    explicit AddInst(Value* lhs, Value* rhs, Location location = {})
        : BinaryInst(Kind::Add, lhs, rhs, lhs->type(), location) {
        AssertSameType(lhs, rhs);
    }

    /// RTTI.
    [[nodiscard]]
    static auto classof(Value* v) -> bool { return v->kind() == Kind::Add; }
};

/// A subtract instruction.
class SubInst : public BinaryInst {
    friend parser::Parser;

    /// Used by the IR parser.
    explicit SubInst(Type* ty, Location location = {}) : BinaryInst(Kind::Sub, nullptr, nullptr, ty, location) {}

public:
    explicit SubInst(Value* lhs, Value* rhs, Location location = {})
        : BinaryInst(Kind::Sub, lhs, rhs, lhs->type(), location) {
        AssertSameType(lhs, rhs);
    }

    /// RTTI.
    [[nodiscard]]
    static auto classof(Value* v) -> bool { return v->kind() == Kind::Sub; }
};

/// A multiply instruction.
class MulInst : public BinaryInst {
    friend parser::Parser;

    /// Used by the IR parser.
    explicit MulInst(Type* ty, Location location = {}) : BinaryInst(Kind::Mul, nullptr, nullptr, ty, location) {}

public:
    explicit MulInst(Value* lhs, Value* rhs, Location location = {})
        : BinaryInst(Kind::Mul, lhs, rhs, lhs->type(), location) {
        AssertSameType(lhs, rhs);
    }

    /// RTTI.
    [[nodiscard]]
    static auto classof(Value* v) -> bool { return v->kind() == Kind::Mul; }
};

/// A signed divide instruction.
class SDivInst : public BinaryInst {
    friend parser::Parser;

    /// Used by the IR parser.
    explicit SDivInst(Type* ty, Location location = {}) : BinaryInst(Kind::SDiv, nullptr, nullptr, ty, location) {}

public:
    explicit SDivInst(Value* lhs, Value* rhs, Location location = {})
        : BinaryInst(Kind::SDiv, lhs, rhs, lhs->type(), location) {
        AssertSameType(lhs, rhs);
    }

    /// RTTI.
    [[nodiscard]]
    static auto classof(Value* v) -> bool { return v->kind() == Kind::SDiv; }
};

/// A signed remainder instruction.
class SRemInst : public BinaryInst {
    friend parser::Parser;

    /// Used by the IR parser.
    explicit SRemInst(Type* ty, Location location = {}) : BinaryInst(Kind::SRem, nullptr, nullptr, ty, location) {}

public:
    explicit SRemInst(Value* lhs, Value* rhs, Location location = {})
        : BinaryInst(Kind::SRem, lhs, rhs, lhs->type(), location) {
        AssertSameType(lhs, rhs);
    }

    /// RTTI.
    [[nodiscard]]
    static auto classof(Value* v) -> bool { return v->kind() == Kind::SRem; }
};

/// An unsigned divide instruction.
class UDivInst : public BinaryInst {
    friend parser::Parser;

    /// Used by the IR parser.
    explicit UDivInst(Type* ty, Location location = {}) : BinaryInst(Kind::UDiv, nullptr, nullptr, ty, location) {}

public:
    explicit UDivInst(Value* lhs, Value* rhs, Location location = {})
        : BinaryInst(Kind::UDiv, lhs, rhs, lhs->type(), location) {
        AssertSameType(lhs, rhs);
    }

    /// RTTI.
    [[nodiscard]]
    static auto classof(Value* v) -> bool { return v->kind() == Kind::UDiv; }
};

/// An unsigned remainder instruction.
class URemInst : public BinaryInst {
    friend parser::Parser;

    /// Used by the IR parser.
    explicit URemInst(Type* ty, Location location = {}) : BinaryInst(Kind::URem, nullptr, nullptr, ty, location) {}

public:
    explicit URemInst(Value* lhs, Value* rhs, Location location = {})
        : BinaryInst(Kind::URem, lhs, rhs, lhs->type(), location) {
        AssertSameType(lhs, rhs);
    }

    /// RTTI.
    [[nodiscard]]
    static auto classof(Value* v) -> bool { return v->kind() == Kind::URem; }
};

/// A left shift instruction.
class ShlInst : public BinaryInst {
    friend parser::Parser;

    /// Used by the IR parser.
    explicit ShlInst(Type* ty, Location location = {}) : BinaryInst(Kind::Shl, nullptr, nullptr, ty, location) {}

public:
    explicit ShlInst(Value* lhs, Value* rhs, Location location = {})
        : BinaryInst(Kind::Shl, lhs, rhs, lhs->type(), location) {
        AssertSameType(lhs, rhs);
    }

    /// RTTI.
    [[nodiscard]]
    static auto classof(Value* v) -> bool { return v->kind() == Kind::Shl; }
};

/// An arithmetic right shift. SIGNED
///
/// given:  0b0100
///              1 SAR
/// result: 0b0010
/// given:  0b1100
///              1 SAR
/// result: 0b1110
///
/// Basically, the original sign bit value is used as the value for bits
/// shifted in, maintaining negative numbers through "division".
class SarInst : public BinaryInst {
    friend parser::Parser;

    /// Used by the IR parser.
    explicit SarInst(Type* ty, Location location = {}) : BinaryInst(Kind::Sar, nullptr, nullptr, ty, location) {}

public:
    explicit SarInst(Value* lhs, Value* rhs, Location location = {})
        : BinaryInst(Kind::Sar, lhs, rhs, lhs->type(), location) {
        AssertSameType(lhs, rhs);
    }

    /// RTTI.
    [[nodiscard]]
    static auto classof(Value* v) -> bool { return v->kind() == Kind::Sar; }
};

/// A logical right shift. UNSIGNED
///
/// given:  0b0100
///              1 SHR
/// result: 0b0010
/// given:  0b1100
///              1 SHR
/// result: 0b0110
///
/// The value of the bit(s) shifted in is always zero.
class ShrInst : public BinaryInst {
    friend parser::Parser;

    /// Used by the IR parser.
    explicit ShrInst(Type* ty, Location location = {})
        : BinaryInst(Kind::Shr, nullptr, nullptr, ty, location) {}

public:
    explicit ShrInst(Value* lhs, Value* rhs, Location location = {})
        : BinaryInst(Kind::Shr, lhs, rhs, lhs->type(), location) {
        AssertSameType(lhs, rhs);
    }

    /// RTTI.
    [[nodiscard]]
    static auto classof(Value* v) -> bool { return v->kind() == Kind::Shr; }
};

/// A bitwise and instruction.
class AndInst : public BinaryInst {
    friend parser::Parser;

    /// Used by the IR parser.
    explicit AndInst(Type* ty, Location location = {}) : BinaryInst(Kind::And, nullptr, nullptr, ty, location) {}

public:
    explicit AndInst(Value* lhs, Value* rhs, Location location = {})
        : BinaryInst(Kind::And, lhs, rhs, lhs->type(), location) {
        AssertSameType(lhs, rhs);
    }

    /// RTTI.
    [[nodiscard]]
    static auto classof(Value* v) -> bool { return v->kind() == Kind::And; }
};

/// A bitwise or instruction.
class OrInst : public BinaryInst {
    friend parser::Parser;

    /// Used by the IR parser.
    explicit OrInst(Type* ty, Location location = {}) : BinaryInst(Kind::Or, nullptr, nullptr, ty, location) {}

public:
    explicit OrInst(Value* lhs, Value* rhs, Location location = {})
        : BinaryInst(Kind::Or, lhs, rhs, lhs->type(), location) {
        AssertSameType(lhs, rhs);
    }

    /// RTTI.
    [[nodiscard]]
    static auto classof(Value* v) -> bool { return v->kind() == Kind::Or; }
};

/// A bitwise xor instruction.
class XorInst : public BinaryInst {
    friend parser::Parser;

    /// Used by the IR parser.
    explicit XorInst(Type* ty, Location location = {}) : BinaryInst(Kind::Xor, nullptr, nullptr, ty, location) {}

public:
    explicit XorInst(Value* lhs, Value* rhs, Location location = {})
        : BinaryInst(Kind::Or, lhs, rhs, lhs->type(), location) {
        AssertSameType(lhs, rhs);
    }

    /// RTTI.
    [[nodiscard]]
    static auto classof(Value* v) -> bool { return v->kind() == Kind::Xor; }
};

/// Base class for comparison instructions; this is just so we
/// can do is<CompareInst> and because the type is always i1.
class CompareInst : public BinaryInst {
    friend parser::Parser;

protected:
    explicit CompareInst(Kind k, Value* lhs, Value* rhs, Location location = {})
        : BinaryInst(k, lhs, rhs, Type::I1Ty, location) {}

public:
    /// RTTI.
    [[nodiscard]]
    static auto classof(Value* v) -> bool { return +v->kind() >= +Kind::Eq; }
};

/// Instruction that compares two values for equality.
class EqInst : public CompareInst {
    friend parser::Parser;

    /// Used by the IR parser.
    explicit EqInst(Location location = {}) : CompareInst(Kind::Eq, nullptr, nullptr, location) {}

public:
    explicit EqInst(Value* lhs, Value* rhs, Location location = {})
        : CompareInst(Kind::Eq, lhs, rhs, location) {
        AssertSameType(lhs, rhs);
    }

    /// RTTI.
    [[nodiscard]]
    static auto classof(Value* v) -> bool { return v->kind() == Kind::Eq; }
};

/// Instruction that compares two values for inequality.
class NeInst : public CompareInst {
    friend parser::Parser;

    /// Used by the IR parser.
    explicit NeInst(Location location = {}) : CompareInst(Kind::Ne, nullptr, nullptr, location) {}

public:
    explicit NeInst(Value* lhs, Value* rhs, Location location = {})
        : CompareInst(Kind::Ne, lhs, rhs, location) {
        AssertSameType(lhs, rhs);
    }

    /// RTTI.
    [[nodiscard]]
    static auto classof(Value* v) -> bool { return v->kind() == Kind::Ne; }
};

/// Instruction that compares two values for less-than.
class SLtInst : public CompareInst {
    friend parser::Parser;

    /// Used by the IR parser.
    explicit SLtInst(Location location = {}) : CompareInst(Kind::SLt, nullptr, nullptr, location) {}

public:
    explicit SLtInst(Value* lhs, Value* rhs, Location location = {})
        : CompareInst(Kind::SLt, lhs, rhs, location) {
        AssertSameType(lhs, rhs);
    }

    /// RTTI.
    [[nodiscard]]
    static auto classof(Value* v) -> bool { return v->kind() == Kind::SLt; }
};

class ULtInst : public CompareInst {
    friend parser::Parser;

    /// Used by the IR parser.
    explicit ULtInst(Location location = {}) : CompareInst(Kind::ULt, nullptr, nullptr, location) {}

public:
    explicit ULtInst(Value* lhs, Value* rhs, Location location = {})
        : CompareInst(Kind::ULt, lhs, rhs, location) {
        AssertSameType(lhs, rhs);
    }

    /// RTTI.
    [[nodiscard]]
    static auto classof(Value* v) -> bool { return v->kind() == Kind::ULt; }
};

/// Instruction that compares two values for less-than-or-equal.
class SLeInst : public CompareInst {
    friend parser::Parser;

    /// Used by the IR parser.
    explicit SLeInst(Location location = {}) : CompareInst(Kind::SLe, nullptr, nullptr, location) {}
public:
    explicit SLeInst(Value* lhs, Value* rhs, Location location = {})
        : CompareInst(Kind::SLe, lhs, rhs, location) {
        AssertSameType(lhs, rhs);
    }

    /// RTTI.
    [[nodiscard]]
    static auto classof(Value* v) -> bool { return v->kind() == Kind::SLe; }
};

class ULeInst : public CompareInst {
    friend parser::Parser;

    /// Used by the IR parser.
    explicit ULeInst(Location location = {}) : CompareInst(Kind::ULe, nullptr, nullptr, location) {}
public:
    explicit ULeInst(Value* lhs, Value* rhs, Location location = {})
        : CompareInst(Kind::ULe, lhs, rhs, location) {
        AssertSameType(lhs, rhs);
    }

    /// RTTI.
    [[nodiscard]]
    static auto classof(Value* v) -> bool { return v->kind() == Kind::ULe; }
};

/// Instruction that compares two values for greater-than.
class SGtInst : public CompareInst {
    friend parser::Parser;

    /// Used by the IR parser.
    explicit SGtInst(Location location = {}) : CompareInst(Kind::SGt, nullptr, nullptr, location) {}

public:
    explicit SGtInst(Value* lhs, Value* rhs, Location location = {})
        : CompareInst(Kind::SGt, lhs, rhs, location) {
        AssertSameType(lhs, rhs);
    }

    /// RTTI.
    [[nodiscard]]
    static auto classof(Value* v) -> bool { return v->kind() == Kind::SGt; }
};

class UGtInst : public CompareInst {
    friend parser::Parser;

    /// Used by the IR parser.
    explicit UGtInst(Location location = {}) : CompareInst(Kind::UGt, nullptr, nullptr, location) {}

public:
    explicit UGtInst(Value* lhs, Value* rhs, Location location = {})
        : CompareInst(Kind::UGt, lhs, rhs, location) {
        AssertSameType(lhs, rhs);
    }

    /// RTTI.
    [[nodiscard]]
    static auto classof(Value* v) -> bool { return v->kind() == Kind::UGt; }
};

/// Instruction that compares two values for greater-than-or-equal.
class SGeInst : public CompareInst {
    friend parser::Parser;

    /// Used by the IR parser.
    explicit SGeInst(Location location = {}) : CompareInst(Kind::SGe, nullptr, nullptr, location) {}

public:
    explicit SGeInst(Value* lhs, Value* rhs, Location location = {})
        : CompareInst(Kind::SGe, lhs, rhs, location) {
        AssertSameType(lhs, rhs);
    }

    /// RTTI.
    [[nodiscard]]
    static auto classof(Value* v) -> bool { return v->kind() == Kind::SGe; }
};

class UGeInst : public CompareInst {
    friend parser::Parser;

    /// Used by the IR parser.
    explicit UGeInst(Location location = {}) : CompareInst(Kind::UGe, nullptr, nullptr, location) {}

public:
    explicit UGeInst(Value* lhs, Value* rhs, Location location = {})
        : CompareInst(Kind::UGe, lhs, rhs, location) {
        AssertSameType(lhs, rhs);
    }

    /// RTTI.
    [[nodiscard]]
    static auto classof(Value* v) -> bool { return v->kind() == Kind::UGe; }
};

/// Unary instructions.
class UnaryInstBase : public Inst {
    friend Inst;
    friend parser::Parser;

    Value* op{};

protected:
    explicit UnaryInstBase(Kind k, Value* v, Type* ty, Location location = {})
        : Inst(k, ty, location), op(v) {
        /// IR parser may pass nullptr here.
        if (op) AddUse(op, this);
    }

public:
    /// Get the operand.
    [[nodiscard]]
    auto operand() const -> Value* { return op; }

    /// Set the operand.
    void operand(Value* v) { UpdateOperand(op, v); }

    [[nodiscard]]
    static auto classof(Value* v) -> bool {
        return +v->kind() >= +Kind::ZExt and +v->kind() <= +Kind::Compl;
    }
};

/// SSA copy. Used during lowering only.
class CopyInst : public UnaryInstBase {
    friend parser::Parser;

    /// Used by the IR parser.
    explicit CopyInst(Type* ty, Location location = {}) : UnaryInstBase(Kind::Copy, nullptr, ty, location) {}

public:
    explicit CopyInst(Value* v, Location location = {})
        : UnaryInstBase(Kind::Copy, v, v->type(), location) {}

    /// RTTI.
    [[nodiscard]]
    static auto classof(Value* v) -> bool { return v->kind() == Kind::Copy; }
};

/// Zero-extend an integer value.
class ZExtInst : public UnaryInstBase {
    friend parser::Parser;

    /// Used by the IR parser.
    explicit ZExtInst(Type* ty, Location location = {}) : UnaryInstBase(Kind::ZExt, nullptr, ty, location) {}

public:
    explicit ZExtInst(Value* v, Type* ty, Location location = {})
        : UnaryInstBase(Kind::ZExt, v, ty, location) {}

    /// RTTI.
    [[nodiscard]]
    static auto classof(Value* v) -> bool { return v->kind() == Kind::ZExt; }
};

/// Sign-extend an integer value.
class SExtInst : public UnaryInstBase {
    friend parser::Parser;

    /// Used by the IR parser.
    explicit SExtInst(Type* ty, Location location = {}) : UnaryInstBase(Kind::SExt, nullptr, ty, location) {}

public:
    explicit SExtInst(Value* v, Type* ty, Location location = {})
        : UnaryInstBase(Kind::SExt, v, ty, location) {}

    /// RTTI.
    [[nodiscard]]
    static auto classof(Value* v) -> bool { return v->kind() == Kind::SExt; }
};

/// Truncate an integer value.
class TruncInst : public UnaryInstBase {
    friend parser::Parser;

    /// Used by the IR parser.
    explicit TruncInst(Type* ty, Location location = {}) : UnaryInstBase(Kind::Trunc, nullptr, ty, location) {}

public:
    explicit TruncInst(Value* v, Type* ty, Location location = {})
        : UnaryInstBase(Kind::Trunc, v, ty, location) {}

    /// RTTI.
    [[nodiscard]]
    static auto classof(Value* v) -> bool { return v->kind() == Kind::Trunc; }
};

/// Bitcast a value to another type.
class BitcastInst : public UnaryInstBase {
    friend parser::Parser;

    /// Used by the IR parser.
    explicit BitcastInst(Type* ty, Location location = {}) : UnaryInstBase(Kind::Bitcast, nullptr, ty, location) {}

public:
    explicit BitcastInst(Value* v, Type* ty, Location location = {})
        : UnaryInstBase(Kind::Bitcast, v, ty, location) {
        /// Type we’re casting to must have the same size or
        /// be smaller than the type we’re casting from.
        LCC_ASSERT(ty->bits() <= v->type()->bits(), "Cannot bitcast to larger type");
    }

    /// RTTI.
    [[nodiscard]]
    static auto classof(Value* v) -> bool { return v->kind() == Kind::Bitcast; }
};

/// Negate an integer value.
/// That is, given a positive value, result a negative value.
/// Given a negative value, result a positive value.
class NegInst : public UnaryInstBase {
    friend parser::Parser;

    /// Used by the IR parser.
    explicit NegInst(Type* ty, Location location = {}) : UnaryInstBase(Kind::Neg, nullptr, ty, location) {}

public:
    explicit NegInst(Value* v, Location location = {})
        : UnaryInstBase(Kind::Neg, v, v->type(), location) {}

    /// RTTI.
    [[nodiscard]]
    static auto classof(Value* v) -> bool { return v->kind() == Kind::Neg; }
};

/// Bitwise complement of an integer value.
class ComplInst : public UnaryInstBase {
    friend parser::Parser;

    /// Used by the IR parser.
    explicit ComplInst(Type* ty, Location location = {}) : UnaryInstBase(Kind::Compl, nullptr, ty, location) {}

public:
    explicit ComplInst(Value* v, Location location = {})
        : UnaryInstBase(Kind::Compl, v, v->type(), location) {}

    /// RTTI.
    [[nodiscard]]
    static auto classof(Value* v) -> bool { return v->kind() == Kind::Compl; }
};

/// ============================================================================
///  Constants
/// ============================================================================
/// Immediate value.
class IntegerConstant : public Value {
    aint _value;

public:
    explicit IntegerConstant(Type* ty, aint value)
        : Value(Kind::IntegerConstant, ty),
          _value(value.trunc(u8(as<IntegerType>(ty)->bits()))) {
        LCC_ASSERT(as<IntegerType>(ty)->bits() <= 64, "Integer constants must be 64 bits or less");
    }

    /// Get the value.
    [[nodiscard]]
    auto value() const -> aint { return _value; }

    /// RTTI.
    [[nodiscard]]
    static auto classof(Value* v) -> bool { return v->kind() == Kind::IntegerConstant; }
};

/// Fractional value.
class FractionalConstant : public Value {
    FixedPointNumber _value;

public:
    explicit FractionalConstant(Type* ty, FixedPointNumber value)
        : Value(Kind::FractionalConstant, ty),
          _value(value) {}

    /// Get the value.
    [[nodiscard]]
    auto value() const -> FixedPointNumber { return _value; }

    /// RTTI.
    [[nodiscard]]
    static auto classof(Value* v) -> bool { return v->kind() == Kind::FractionalConstant; }
};

/// Packed array.
///
/// This is used for constant arrays that are known at compile time,
/// such as strings or constant array literals.
class ArrayConstant : public Value {
    std::vector<char> _data;
    bool _is_string_literal = false;

public:
    explicit ArrayConstant(Type* ty, std::vector<char> data, bool is_string_literal = false)
        : Value(Kind::ArrayConstant, ty),
          _data(std::move(data)),
          _is_string_literal(is_string_literal) {}

    /// Get an iterator to the start of the data.
    [[nodiscard]]
    auto begin() const { return _data.begin(); }

    /// Get the data.
    [[nodiscard]]
    auto data() const -> const char* { return _data.data(); }

    /// Get an iterator to the end of the data.
    [[nodiscard]]
    auto end() const { return _data.end(); }

    /// Whether this is a string literal.
    ///
    /// This only affects how this array constant is printed
    /// and should not be used for anything else.
    [[nodiscard]]
    auto is_string_literal() const -> bool { return _is_string_literal; }

    /// Get the size.
    [[nodiscard]]
    auto size() const -> size_t { return _data.size(); }

    /// RTTI.
    [[nodiscard]]
    static auto classof(Value* v) -> bool { return v->kind() == Kind::ArrayConstant; }
};

/// Poison value.
class PoisonValue : public Value {
public:
    explicit PoisonValue(Type* ty)
        : Value(Kind::Poison, ty) {}

    /// RTTI.
    [[nodiscard]]
    static auto classof(Value* v) -> bool { return v->kind() == Kind::Poison; }
};

} // namespace lcc

#endif // LCC_IR_CORE_HH
