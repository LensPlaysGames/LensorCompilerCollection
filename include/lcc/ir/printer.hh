#ifndef LCC_IR_PRINTER_HH
#define LCC_IR_PRINTER_HH

#include <vector>
#include <string>
#include <string_view>
#include <lcc/ir/ir.hh>

namespace lcc {

class ValuePrinter {
    static std::unordered_map<Value*, usz> ids;

public:
    static void register_value(Value* v) {
        static usz _id{0};

        LCC_ASSERT(v, "Cannot print Value with address of nullptr");
        if (ids.find(v) == ids.end()) {
            // value not seen before
            ids[v] = ++_id;
        }
    }

    static auto get_id_raw(Value* v) -> usz {
        register_value(v);
        return ids[v];
    }

    static auto get_id(Value* v) -> std::string {
        register_value(v);

        // NOTE: ALL "INLINE" VALUES MUST GO HERE
        if (v->kind() == Value::Kind::Block ||
            v->kind() == Value::Kind::Function ||
            v->kind() == Value::Kind::IntegerConstant ||
            v->kind() == Value::Kind::ArrayConstant ||
            v->kind() == Value::Kind::Poison ||
            v->kind() == Value::Kind::GlobalVariable) {
                return value(v);
            }
        else return fmt::format("%{}", ids[v]);
    }

    static std::string instruction_name(Value::Kind k) {
        switch (k) {
        case Value::Kind::Block: return "block";
        case Value::Kind::Function: return "function";
        case Value::Kind::IntegerConstant: return "constant.integer";
        case Value::Kind::ArrayConstant: return "constant.array";
        case Value::Kind::Poison: return "poision";
        case Value::Kind::GlobalVariable: return "global";
        case Value::Kind::Alloca: return "local";
        case Value::Kind::Call: return "call";
        case Value::Kind::Copy: return "copy";
        case Value::Kind::Intrinsic: return "intrinsic";
        case Value::Kind::Load: return "load";
        case Value::Kind::Parameter: return "parameter";
        case Value::Kind::Phi: return "phi";
        case Value::Kind::Store: return "store";
        case Value::Kind::Branch: return "branch";
        case Value::Kind::CondBranch: return "branch.cond";
        case Value::Kind::Return: return "return";
        case Value::Kind::Unreachable: return "unreachable";
        case Value::Kind::ZExt: return "zero.extend";
        case Value::Kind::SExt: return "sign.extend";
        case Value::Kind::Trunc: return "truncate";
        case Value::Kind::Bitcast: return "bitcast";
        case Value::Kind::Neg: return "negate";
        case Value::Kind::Compl: return "complement";
        case Value::Kind::Add: return "+";
        case Value::Kind::Sub: return "-";
        case Value::Kind::Mul: return "*";
        case Value::Kind::SDiv: return "s.div";
        case Value::Kind::UDiv: return "u.div";
        case Value::Kind::SRem: return "s.rem";
        case Value::Kind::URem: return "u.rem";
        case Value::Kind::Shl: return "shl";
        case Value::Kind::Sar: return "sar";
        case Value::Kind::Shr: return "shr";
        case Value::Kind::And: return "&";
        case Value::Kind::Or: return "|";
        case Value::Kind::Xor: return "^";
        case Value::Kind::Eq: return "=";
        case Value::Kind::Ne: return "!=";
        case Value::Kind::Lt: return "<";
        case Value::Kind::Le: return "<=";
        case Value::Kind::Gt: return ">";
        case Value::Kind::Ge: return ">=";
        }
        LCC_UNREACHABLE();
    }

    static std::string value(Value* v) {
        if (!v) return "(null)";
        register_value(v);
        switch (v->kind()) {
        case Value::Kind::Block: {
            const auto& block = as<Block>(v);
            return fmt::format("block {}", block->name());
        } break;
        case Value::Kind::Function: {
            const auto& function = as<Function>(v);
            return fmt::format("function {}", function->name());
        } break;
        case Value::Kind::IntegerConstant: {
            return fmt::format("{}", as<IntegerConstant>(v)->value());
        } break;
        case Value::Kind::ArrayConstant: {
            return "constant.array"; // TODO: value
        } break;
        case Value::Kind::Poison: {
            return instruction_name(v->kind());
        } break;
        case Value::Kind::GlobalVariable: {
            return instruction_name(v->kind());
        } break;

        /// Instructions.
        case Value::Kind::Alloca: {
            const auto& local = as<AllocaInst>(v);
            return fmt::format("{} {} ({}B)", instruction_name(v->kind()), *local->allocated_type(), local->allocated_type()->bytes());
        } break;
        case Value::Kind::Call: {
            return instruction_name(v->kind());
        } break;
        case Value::Kind::Copy: {
            LCC_ASSERT(false, "TODO IR CopyInst");
        } break;
        case Value::Kind::Intrinsic: {
            return instruction_name(v->kind());
        } break;
        case Value::Kind::Load: {
            const auto& load = as<LoadInst>(v);
            return fmt::format("{} {} ({}B) from {}", instruction_name(v->kind()), *load->type(), load->type()->bytes(), get_id(load->ptr()));
        } break;
        case Value::Kind::Parameter: {
            return instruction_name(v->kind());
        } break;
        case Value::Kind::Phi: {
            return instruction_name(v->kind());
        } break;
        case Value::Kind::Store: {
            const auto& store = as<StoreInst>(v);
            LCC_ASSERT(store->val());
            return fmt::format("store {} as {} ({}B) into address {}",
                               get_id(store->val()),
                               *store->val()->type(), store->val()->type()->bytes(),
                               get_id(store->ptr()));
        } break;

        /// Terminators.
        case Value::Kind::Branch: {
            return instruction_name(v->kind());
        } break;
        case Value::Kind::CondBranch: {
            return instruction_name(v->kind());
        } break;
        case Value::Kind::Return: {
            return instruction_name(v->kind());
        } break;
        case Value::Kind::Unreachable: {
            return instruction_name(v->kind());
        } break;

        /// Unary instructions.
        case Value::Kind::Bitcast: {
            const auto& bitcast = as<BitcastInst>(v);
            return fmt::format("{} {} as {}", instruction_name(v->kind()), get_id(bitcast->operand()), *bitcast->operand()->type());
        }

        case Value::Kind::ZExt:
        case Value::Kind::SExt:
        case Value::Kind::Trunc: {
            const auto unary = as<UnaryInstBase>(v);
            return fmt::format("{} {} to {}", instruction_name(v->kind()), get_id(unary->operand()), *unary->type());
        } break;

        case Value::Kind::Neg:
        case Value::Kind::Compl: {
            const auto& unary = as<UnaryInstBase>(v);
            return fmt::format("{} {}", instruction_name(v->kind()), get_id(unary->operand()));
        } break;

        /// Binary instructions.
        case Value::Kind::Add:
        case Value::Kind::Sub:
        case Value::Kind::Mul:
        case Value::Kind::SDiv:
        case Value::Kind::UDiv:
        case Value::Kind::SRem:
        case Value::Kind::URem:
        case Value::Kind::Shl:
        case Value::Kind::Sar:
        case Value::Kind::Shr:
        case Value::Kind::And:
        case Value::Kind::Or:
        case Value::Kind::Xor:
        /// Compare instructions.
        case Value::Kind::Eq:
        case Value::Kind::Ne:
        case Value::Kind::Lt:
        case Value::Kind::Le:
        case Value::Kind::Gt:
        case Value::Kind::Ge: {
            const BinaryInst* binary = as<BinaryInst>(v);
            return fmt::format("{} {} {}", instruction_name(v->kind()), get_id(binary->lhs()), get_id(binary->rhs()));
        } break;
        }


        LCC_UNREACHABLE();
    }

    static void _print(Value* value, std::string_view prefix, std::string_view suffix) {
        fmt::print("{}{}{}", prefix, ValuePrinter::value(value), suffix);
    }

    static void print(Value* value, std::string_view prefix = "", std::string_view suffix = "\n") {
        _print(value, prefix, suffix);
    }

    static void print(std::vector<Value*> values, std::string_view prefix = "", std::string_view suffix = "\n") {
        for (auto* v : values) _print(v, prefix, suffix);
    }
};

}

#endif /* LCC_IR_PRINTER_HH */
