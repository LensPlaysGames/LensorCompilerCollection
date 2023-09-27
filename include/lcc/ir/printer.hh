#ifndef LCC_IR_PRINTER_HH
#define LCC_IR_PRINTER_HH

#include <vector>
#include <string>
#include <string_view>
#include <lcc/ir/ir.hh>

namespace lcc {

class ValuePrinter {
    static usz get_id(Value* v) {
        static usz _id{0};
        static std::unordered_map<Value*, usz> ids;

        if (ids.find(v) == ids.end()) {
            // value not seen before
            ids[v] = ++_id;
        }
        return ids[v];
    }

public:
    static std::string value(Value* v) {
        if (!v) return "(null)";
        (void)get_id(v); // just to register value with id.
        switch (v->kind()) {
        case Value::Kind::Block: {
            return "block";
        } break;
        case Value::Kind::Function: {
            return "function";
        } break;
        case Value::Kind::IntegerConstant: {
            return fmt::format("constant.integer {}", as<IntegerConstant>(v)->value());
        } break;
        case Value::Kind::ArrayConstant: {
            return "constant.array";
        } break;
        case Value::Kind::Poison: {
            return "poison";
        } break;
        case Value::Kind::GlobalVariable: {
            return "global";
        } break;

        /// Instructions.
        case Value::Kind::Alloca: {
            return "stack.allocate";
        } break;
        case Value::Kind::Call: {
            return "funcall";
        } break;
        case Value::Kind::Copy: {
            LCC_ASSERT(false, "TODO IR CopyInst");
        } break;
        case Value::Kind::Intrinsic: {
            return "intrinsic";
        } break;
        case Value::Kind::Load: {
            return "load";
        } break;
        case Value::Kind::Parameter: {
            return "parameter";
        } break;
        case Value::Kind::Phi: {
            return "phi";
        } break;
        case Value::Kind::Store: {
            const auto& store = as<StoreInst>(v);
            LCC_ASSERT(store->val());
            return fmt::format("store %{} as {} ({}B) into address %{}",
                               get_id(store->val()),
                               *store->val()->type(), store->val()->type()->size(),
                               get_id(store->ptr()));
        } break;

        /// Terminators.
        case Value::Kind::Branch: {
            return "branch";
        } break;
        case Value::Kind::CondBranch: {
            return "branch.cond";
        } break;
        case Value::Kind::Return: {
            return "branch.return";
        } break;
        case Value::Kind::Unreachable: {
            return "unreachable";
        } break;

        /// Unary instructions.
        case Value::Kind::ZExt: {
            return "zero.extend";
        } break;
        case Value::Kind::SExt: {
            return "sign.extend";
        } break;
        case Value::Kind::Trunc: {
            return "truncate";
        } break;
        case Value::Kind::Bitcast: {
            return "bitcast";
        } break;
        case Value::Kind::Neg: {
            return "negate";
        } break;
        case Value::Kind::Compl: {
            return "compl";
        } break;

        /// Binary instructions.
        case Value::Kind::Add: {
            return "add";
        } break;
        case Value::Kind::Sub: {
            return "sub";
        } break;
        case Value::Kind::Mul: {
            return "mul";
        } break;
        case Value::Kind::SDiv: {
            return "s.div";
        } break;
        case Value::Kind::UDiv: {
            return "u.div";
        } break;
        case Value::Kind::SRem: {
            return "s.mod";
        } break;
        case Value::Kind::URem: {
            return "u.mod";
        } break;
        case Value::Kind::Shl: {
            return "shl";
        } break;
        case Value::Kind::Sar: {
            return "sar";
        } break;
        case Value::Kind::Shr: {
            return "shr";
        } break;
        case Value::Kind::And: {
            return "and";
        } break;
        case Value::Kind::Or: {
            return "or";
        } break;
        case Value::Kind::Xor: {
            return "xor";
        } break;

        /// Compare instructions.
        case Value::Kind::Eq: {
            return "eq";
        } break;
        case Value::Kind::Ne: {
            return "ne";
        } break;
        case Value::Kind::Lt: {
            return "lt";
        } break;
        case Value::Kind::Le: {
            return "le";
        } break;
        case Value::Kind::Gt: {
            return "gt";
        } break;
        case Value::Kind::Ge: {
            return "ge";
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
