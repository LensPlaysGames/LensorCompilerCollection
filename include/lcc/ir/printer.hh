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
            v->kind() == Value::Kind::GlobalVariable ||
            v->kind() == Value::Kind::Parameter) {
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
        //case Value::Kind::Copy: return "copy";
        case Value::Kind::GetElementPtr: return "gep";
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
        case Value::Kind::SLt: return "<";
        case Value::Kind::SLe: return "s.<=";
        case Value::Kind::SGt: return "s.>";
        case Value::Kind::SGe: return "s.>=";
        case Value::Kind::ULt: return "u.<";
        case Value::Kind::ULe: return "u.<=";
        case Value::Kind::UGt: return "u.>";
        case Value::Kind::UGe: return "u.>=";
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
        }
        case Value::Kind::Function: {
            const auto& function = as<Function>(v);
            return fmt::format("function {}", function->name());
        }
        case Value::Kind::Parameter: {
            const auto& param = as<Parameter>(v);
            return fmt::format("parameter {} : {}", param->index(), *param->type());
        }
        case Value::Kind::IntegerConstant: {
            return fmt::format("{}", as<IntegerConstant>(v)->value());
        }
        case Value::Kind::ArrayConstant: {
            const auto& array = as<ArrayConstant>(v);
            return fmt::format("{} of {} {}",
                               instruction_name(v->kind()),
                               array->size(),
                               *as<ArrayType>(array->type())->element_type());
        }
        case Value::Kind::Poison: {
            return instruction_name(v->kind());
        }
        case Value::Kind::GlobalVariable: {
            return instruction_name(v->kind());
        }

        /// Instructions.
        case Value::Kind::Alloca: {
            const auto& local = as<AllocaInst>(v);
            return fmt::format("{} {} ({}B)", instruction_name(v->kind()), *local->allocated_type(), local->allocated_type()->bytes());
        }
        case Value::Kind::Call: {
            const auto& call = as<CallInst>(v);

            if (call->args().empty())
                return fmt::format("{}()", get_id(call->callee()));

            std::string out = "";
            out += fmt::format("{}(", get_id(call->callee()));
            for (const auto& arg : call->args())
                out += get_id(arg) + " ";

            out[out.length() - 1] = ')'; // replace last space, ' ', with close paren, ')'.
            return out;
        }
        /*case Value::Kind::Copy: {
            LCC_ASSERT(false, "TODO IR CopyInst");
        }*/
        case Value::Kind::GetElementPtr: {
            const auto& gep = as<GEPInst>(v);
            return fmt::format("{} {} {} ({}B ea) from {}",
                               instruction_name(v->kind()),
                               get_id(gep->idx()),
                               *gep->type(), gep->type()->bytes(),
                               get_id(gep->ptr()));
        }
        case Value::Kind::Intrinsic: {
            return instruction_name(v->kind());
        }
        case Value::Kind::Load: {
            const auto& load = as<LoadInst>(v);
            return fmt::format("{} {} ({}B) from {}", instruction_name(v->kind()), *load->type(), load->type()->bytes(), get_id(load->ptr()));
        }
        case Value::Kind::Phi: {
            const auto& phi = as<PhiInst>(v);
            auto out = instruction_name(v->kind());
            for (const auto& operand : phi->operands())
                out += fmt::format(" [{}, {}]", operand.block->name(), get_id(operand.value));
            return out;
        }
        case Value::Kind::Store: {
            const auto& store = as<StoreInst>(v);
            LCC_ASSERT(store->val());
            return fmt::format("store {} as {} ({}B) into address {}",
                               get_id(store->val()),
                               *store->val()->type(), store->val()->type()->bytes(),
                               get_id(store->ptr()));
        }

        /// Terminators.
        case Value::Kind::Branch: {
            const auto& branch = as<BranchInst>(v);
            return fmt::format("{} to {}", instruction_name(v->kind()), get_id(branch->target()));
        }
        case Value::Kind::CondBranch: {
            const auto& branch = as<CondBranchInst>(v);
            return fmt::format("{} if {} then {} otherwise {}", instruction_name(v->kind()),
                               get_id(branch->cond()),
                               get_id(branch->then_block()), get_id(branch->else_block()));
        }
        case Value::Kind::Return: {
            const auto& ret = as<ReturnInst>(v);
            if (ret->val())
                return fmt::format("{} {}", instruction_name(v->kind()), get_id(ret->val()));
            return instruction_name(v->kind());
        }
        case Value::Kind::Unreachable: {
            return instruction_name(v->kind());
        }

        /// Unary instructions.
        case Value::Kind::Bitcast: {
            const auto& bitcast = as<BitcastInst>(v);
            return fmt::format("{} {} as {}", instruction_name(v->kind()), get_id(bitcast->operand()), *bitcast->type());
        }

        case Value::Kind::ZExt:
        case Value::Kind::SExt:
        case Value::Kind::Trunc: {
            const auto unary = as<UnaryInstBase>(v);
            return fmt::format("{} {} to {}", instruction_name(v->kind()), get_id(unary->operand()), *unary->type());
        }

        case Value::Kind::Neg:
        case Value::Kind::Compl: {
            const auto& unary = as<UnaryInstBase>(v);
            return fmt::format("{} {}", instruction_name(v->kind()), get_id(unary->operand()));
        }

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
        case Value::Kind::SLt:
        case Value::Kind::SLe:
        case Value::Kind::SGt:
        case Value::Kind::SGe:
        case Value::Kind::ULt:
        case Value::Kind::ULe:
        case Value::Kind::UGt:
        case Value::Kind::UGe: {
            const BinaryInst* binary = as<BinaryInst>(v);
            return fmt::format("{} {} {}", instruction_name(v->kind()), get_id(binary->lhs()), get_id(binary->rhs()));
        }
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
