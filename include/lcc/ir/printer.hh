#ifndef LCC_IR_PRINTER_HH
#define LCC_IR_PRINTER_HH

#include <vector>
#include <string>
#include <string_view>
#include <lcc/ir/ir.hh>

namespace lcc {

class InstructionPrinter {
public:
    static std::string instruction(Inst* i) {
        if (!i) return "(null)\n";
        switch (i->kind()) {
        case Value::Kind::Block: {
            return "block\n";
        } break;
        case Value::Kind::Function: {
            return "function\n";
        } break;
        case Value::Kind::IntegerConstant: {
            return "constant.integer\n";
        } break;
        case Value::Kind::ArrayConstant: {
            return "constant.array\n";
        } break;
        case Value::Kind::Poison: {
            return "poison\n";
        } break;

        /// Instructions.
        case Value::Kind::Alloca: {
            return "stack.allocate\n";
        } break;
        case Value::Kind::Call: {
            return "funcall\n";
        } break;
        case Value::Kind::Copy: {
            return "copy\n";
        } break;
        case Value::Kind::Intrinsic: {
            return "intrinsic\n";
        } break;
        case Value::Kind::Load: {
            return "load\n";
        } break;
        case Value::Kind::Parameter: {
            return "parameter\n";
        } break;
        case Value::Kind::Phi: {
            return "phi\n";
        } break;
        case Value::Kind::Store: {
            return "store\n";
        } break;

        /// Terminators.
        case Value::Kind::Branch: {
            return "branch\n";
        } break;
        case Value::Kind::CondBranch: {
            return "branch.cond\n";
        } break;
        case Value::Kind::Return: {
            return "branch.return\n";
        } break;
        case Value::Kind::Unreachable: {
            return "unreachable\n";
        } break;

        /// Unary instructions.
        case Value::Kind::ZExt: {
            return "zero.extend\n";
        } break;
        case Value::Kind::SExt: {
            return "sign.extend\n";
        } break;
        case Value::Kind::Trunc: {
            return "truncate\n";
        } break;
        case Value::Kind::Bitcast: {
            return "bitcast\n";
        } break;
        case Value::Kind::Neg: {
            return "negate\n";
        } break;

        /// Binary instructions.
        case Value::Kind::Add: {
            return "add\n";
        } break;
        case Value::Kind::Sub: {
            return "sub\n";
        } break;
        case Value::Kind::Mul: {
            return "mul\n";
        } break;
        case Value::Kind::SDiv: {
            return "s.div\n";
        } break;
        case Value::Kind::UDiv: {
            return "u.div\n";
        } break;
        case Value::Kind::SRem: {
            return "s.mod\n";
        } break;
        case Value::Kind::URem: {
            return "u.mod\n";
        } break;
        case Value::Kind::Shl: {
            return "shl\n";
        } break;
        case Value::Kind::Sar: {
            return "sar\n";
        } break;
        case Value::Kind::Shr: {
            return "shr\n";
        } break;
        case Value::Kind::And: {
            return "and\n";
        } break;
        case Value::Kind::Or: {
            return "or\n";
        } break;

        /// Compare instructions.
        case Value::Kind::Eq: {
            return "eq\n";
        } break;
        case Value::Kind::Ne: {
            return "ne\n";
        } break;
        case Value::Kind::Lt: {
            return "lt\n";
        } break;
        case Value::Kind::Le: {
            return "le\n";
        } break;
        case Value::Kind::Gt: {
            return "gt\n";
        } break;
        case Value::Kind::Ge: {
            return "ge\n";
        } break;
        }

        LCC_UNREACHABLE();
    }

    static void print(Inst* instruction, std::string_view prefix = "") {
        fmt::print("{}{}", prefix, InstructionPrinter::instruction(instruction));
    }

    static void print(std::vector<Inst*> instructions, std::string_view prefix = "") {
        for (auto* i : instructions) fmt::print("{}{}", prefix, InstructionPrinter::instruction(i));
    }
};

}

#endif /* LCC_IR_PRINTER_HH */
