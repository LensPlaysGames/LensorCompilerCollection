#ifndef LCC_CODEGEN_MIR_HH
#define LCC_CODEGEN_MIR_HH

#include <lcc/utils.hh>
#include <lcc/ir/ir.hh>

#include <vector>

namespace lcc {

// Machine Operand
enum struct MOperandRegister : u64;
using MOperandImmediate = u64;
using MOperandLocal = AllocaInst*;
using MOperandStatic = GlobalVariable*;
using MOperand = std::variant<MOperandRegister, MOperandImmediate, MOperandLocal, MOperandStatic>;

class MInst {
public:
    enum struct Kind {
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

private:

    Kind _kind;

    // TODO: Do SSO basically. For operands, instructions in MBlocks, and blocks in MFunctions.
    std::vector<MOperand> operands;

public:
    MInst(Kind kind) : _kind(kind) {};

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

    static bool is_terminator(Kind k) {
        // TODO: noreturn calls?
        return k == Kind::Return
            or k == Kind::Branch
            or k == Kind::CondBranch
            or k == Kind::Unreachable;
    }
};

class MBlock {
    std::vector<MInst> _instructions;

public:
    MBlock() = default;

    auto instructions() -> std::vector<MInst>& {
        return _instructions;
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

} // namespace lcc

#endif // LCC_CODEGEN_MIR_HH
