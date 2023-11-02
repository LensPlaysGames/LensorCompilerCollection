#ifndef LCC_CODEGEN_MIR_HH
#define LCC_CODEGEN_MIR_HH

#include <lcc/utils.hh>
#include <lcc/ir/ir.hh>
#include <lcc/ir/module.hh>

#include <vector>

namespace lcc {

// Machine Register Description
struct Register {
    u64 value;
    usz size;
};

// Machine Operand
enum struct MOperandRegister : u64;
u64 operator+ (MOperandRegister r);
using MOperandImmediate = u64;
enum struct MOperandLocal : u64;
u64 operator+ (MOperandLocal l);
using MOperandStatic = GlobalVariable*;
using MOperandFunction = Function*;
using MOperandBlock = Block*;
using MOperand = std::variant<
    MOperandRegister,
    MOperandImmediate,
    MOperandLocal,
    MOperandStatic,
    MOperandFunction,
    MOperandBlock
>;

class MInst {
public:
    enum struct Kind {
        /// Instructions
        Alloca,
        Call,
        Copy,
        GetElementPtr,
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

        ArchStart = 0x420,
    };

private:

    Register _register;

    usz _opcode;

    usz _use_count{0};

    // TODO: Do SSO basically. For operands, instructions in MBlocks, and blocks in MFunctions.
    std::vector<MOperand> operands{};

public:
    MInst(Kind kind, usz virtualRegister)
        : _register(virtualRegister, 0), /// FIXME: Actually pass in register size.
          _opcode(static_cast<usz>(kind)) {};

    MInst(usz opcode, usz virtualRegister)
        : _register(virtualRegister, 0), /// FIXME: Actually pass in register size.
          _opcode(opcode) {};

    usz reg() const { return _register.value; }
    usz regsize() const { return _register.size; }

    void reg(usz newRegister) { _register.value = newRegister; }
    void regsize(usz newSize) { _register.size = newSize; }

    usz opcode() const { return _opcode; }

    Kind kind() const {
        // FIXME: This should be enabled, but I don't have stackframes and can't
        // tell where this is triggering from so... yeah.
        //LCC_ASSERT(_opcode < +Kind::ArchStart, "kind() must only be called for general MIR instructions; for architecture-specific instructions, please call opcode()");
        return static_cast<Kind>(_opcode);
    }

    usz use_count() const { return _use_count; }

    void add_use() {
        _use_count += 1;
    }

    void add_operand(MOperand op) {
        operands.push_back(op);
    }

    MOperand get_operand(usz index) const {
        return operands.at(index);
    }

    MOperand operator[] (usz index) const {
        return get_operand(index);
    }

    std::vector<MOperand>& all_operands() {
        return operands;
    }

    const std::vector<MOperand>& all_operands() const {
        return operands;
    }

    static bool is_terminator(Kind k) {
        // TODO: noreturn calls?
        return k == Kind::Return
            or k == Kind::Branch
            or k == Kind::CondBranch
            or k == Kind::Unreachable;
    }
};

class MBlock {
    std::string _name;

    std::vector<MInst> _instructions;

public:
    MBlock(std::string name) : _name(name) {};

    auto instructions() -> std::vector<MInst>& {
        return _instructions;
    }

    auto instructions() const -> const std::vector<MInst>& {
        return _instructions;
    }

    auto name() -> std::string& {
        return _name;
    }

    auto name() const -> const std::string& {
        return _name;
    }

    bool closed() {
        if (_instructions.empty()) return false;
        return MInst::is_terminator(_instructions.back().kind());
    }

    void add_instruction(const MInst& inst, bool forced = false) {
        LCC_ASSERT(forced or not closed(), "Cannot insert into MBlock that has already been closed.");
        if (forced and closed()) {
            _instructions.insert(_instructions.end() - 1, inst);
            return;
        }
        _instructions.push_back(inst);
    }
    void insert(const MInst& inst) { add_instruction(inst); }
};

class MFunction {
    std::string _name;

    std::vector<MBlock> _blocks;

    std::vector<AllocaInst*> _locals;

public:
    MFunction() = default;

    auto name() -> std::string& {
        return _name;
    }

    auto name() const -> const std::string& {
        return _name;
    }

    auto blocks() -> std::vector<MBlock>& {
        return _blocks;
    }

    auto blocks() const -> const std::vector<MBlock>& {
        return _blocks;
    }

    void add_block(const MBlock& block) {
        _blocks.push_back(block);
    }

    auto locals() -> std::vector<AllocaInst*>& {
        return _locals;
    }

    auto locals() const -> const std::vector<AllocaInst*>& {
        return _locals;
    }

    void add_local(AllocaInst* local) {
        _locals.push_back(local);
    }
};

inline std::string ToString(MInst::Kind k) {
    switch(k) {
        case MInst::Kind::Alloca: return "M.Alloca";
        case MInst::Kind::Call: return "M.Call";
        case MInst::Kind::Copy: return "M.Copy";
        case MInst::Kind::GetElementPtr: return "M.GetElementPtr";
        case MInst::Kind::Intrinsic: return "M.Intrinsic";
        case MInst::Kind::Load: return "M.Load";
        case MInst::Kind::Phi: return "M.Phi";
        case MInst::Kind::Store: return "M.Store";
        case MInst::Kind::Branch: return "M.Branch";
        case MInst::Kind::CondBranch: return "M.CondBranch";
        case MInst::Kind::Return: return "M.Return";
        case MInst::Kind::Unreachable: return "M.Unreachable";
        case MInst::Kind::ZExt: return "M.ZExt";
        case MInst::Kind::SExt: return "M.SExt";
        case MInst::Kind::Trunc: return "M.Trunc";
        case MInst::Kind::Bitcast: return "M.Bitcast";
        case MInst::Kind::Neg: return "M.Neg";
        case MInst::Kind::Compl: return "M.Compl";
        case MInst::Kind::Add: return "M.Add";
        case MInst::Kind::Sub: return "M.Sub";
        case MInst::Kind::Mul: return "M.Mul";
        case MInst::Kind::SDiv: return "M.SDiv";
        case MInst::Kind::UDiv: return "M.UDiv";
        case MInst::Kind::SRem: return "M.SRem";
        case MInst::Kind::URem: return "M.URem";
        case MInst::Kind::Shl: return "M.Shl";
        case MInst::Kind::Sar: return "M.Sar";
        case MInst::Kind::Shr: return "M.Shr";
        case MInst::Kind::And: return "M.And";
        case MInst::Kind::Or: return "M.Or";
        case MInst::Kind::Xor: return "M.Xor";
        case MInst::Kind::Eq: return "M.Eq";
        case MInst::Kind::Ne: return "M.Ne";
        case MInst::Kind::SLt: return "M.SLt";
        case MInst::Kind::SLe: return "M.SLe";
        case MInst::Kind::SGt: return "M.SGt";
        case MInst::Kind::SGe: return "M.SGe";
        case MInst::Kind::ULt: return "M.ULt";
        case MInst::Kind::ULe: return "M.ULe";
        case MInst::Kind::UGt: return "M.UGt";
        case MInst::Kind::UGe: return "M.UGe";
        case MInst::Kind::ArchStart: return "M.ArchStart";
    }
    LCC_UNREACHABLE();
}

auto PrintMOperand(const MOperand& op) -> std::string;
auto PrintMInst(const MInst& inst) -> std::string;
auto PrintMBlock(const MBlock& block) -> std::string;
auto PrintMFunction(const MFunction& function) -> std::string;
auto PrintMIR(std::vector<GlobalVariable*>& vars, std::vector<MFunction>& mcode) -> std::string;


} // namespace lcc

#endif // LCC_CODEGEN_MIR_HH
