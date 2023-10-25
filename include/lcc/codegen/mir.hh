#ifndef LCC_CODEGEN_MIR_HH
#define LCC_CODEGEN_MIR_HH

#include <lcc/utils.hh>
#include <lcc/ir/ir.hh>

#include <vector>

namespace lcc {

// Machine Operand
enum struct MOperandRegister : u64;
u64 operator+ (MOperandRegister r);
using MOperandImmediate = u64;
using MOperandLocal = AllocaInst*;
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

    usz _virtual_register;

    Kind _kind;

    // TODO: Do SSO basically. For operands, instructions in MBlocks, and blocks in MFunctions.
    std::vector<MOperand> operands;

public:
    MInst(Kind kind, usz virtualRegister)
        : _virtual_register(virtualRegister),
          _kind(kind) {};

    usz virtual_register() { return _virtual_register; }
    usz virt() { return virtual_register(); }

    Kind kind() { return _kind; }

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

    auto name() -> std::string& {
        return _name;
    }

    bool closed() {
        if (_instructions.empty()) return false;
        return MInst::is_terminator(_instructions.back().kind());
    }

    void add_instruction(const MInst& inst) {
        LCC_ASSERT(not closed(), "Cannot insert into MBlock that has already been closed.");
        _instructions.push_back(inst);
    }
    void insert(const MInst& inst) { add_instruction(inst); }
};

class MFunction {
    std::string _name;

    std::vector<MBlock> _blocks;

public:
    MFunction() = default;

    auto name() -> std::string& {
        return _name;
    }

    auto blocks() -> std::vector<MBlock>& {
        return _blocks;
    }

    void add_block(const MBlock& block) {
        _blocks.push_back(block);
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

} // namespace lcc

#endif // LCC_CODEGEN_MIR_HH
