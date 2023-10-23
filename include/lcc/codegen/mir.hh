#ifndef LCC_CODEGEN_MIR_HH
#define LCC_CODEGEN_MIR_HH

#include <lcc/utils.hh>
#include <lcc/ir/ir.hh>

#include <vector>

namespace lcc {

// Machine Operand
using MOperandRegister = usz;
using MOperandImmediate = u64;
using MOperandLocal = AllocaInst*;
using MOperandStatic = GlobalVariable*;
using MOperand = std::variant<MOperandRegister, MOperandImmediate, MOperandLocal, MOperandStatic>;

class MInst {
    enum struct InstructionKind {
        /// Instructions
        Alloca,
        Call,
        GetElementPtr,
        Intrinsic,
        Load,
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

    // TODO: Do SSO basically
    std::vector<MOperand> operands;

public:
    MInst() = default;

    void add_operand(MOperand op) {
        operands.push_back(op);
    }

    MOperand get_operand(usz index) const {
        return operands.at(index);
    }

    MOperand operator[] (usz index) const {
        return get_operand(index);
    }
};

} // namespace lcc

#endif // LCC_CODEGEN_MIR_HH
