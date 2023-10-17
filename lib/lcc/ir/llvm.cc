#include <lcc/ir/module.hh>

namespace lcc {
namespace {
struct LLVMIRPrinter {
    Module* mod;
    std::string s{};
    usz tmp = 0;

    /// Map from blocks and instructions to their indices.
    std::unordered_map<Block*, usz> block_indices{};
    std::unordered_map<Inst*, usz> inst_indices{};

    LLVMIRPrinter(Module* m) : mod(m) {}

    /// Entry.
    auto print() -> std::string {
        bool first = true;
        for (auto f : mod->code()) {
            if (first) first = false;
            else s += '\n';
            PrintFunction(f);
        }

        return std::move(s);
    }

    /// Get an insert iterator to the output string.
    auto It() { return std::back_inserter(s); }
    auto It(std::string& str) { return std::back_inserter(str); }

    /// Emit a block and its containing instructions.
    void PrintBlock(Block* b) {
        fmt::format_to(It(), "bb{}:\n", block_indices[b]);
        for (auto inst : b->instructions()) PrintInst(inst);
    }

    /// Emit a function definition or declaration as LLVM IR.
    void PrintFunction(Function* f) {
        auto ftype = as<FunctionType>(f->type());
        fmt::format_to(
            It(),
            "{} {} @\"{}\" (",
            f->blocks().empty() ? "declare" : "define",
            Ty(ftype->ret()),
            f->name()
        );

        bool first = true;
        for (auto arg : ftype->params()) {
            if (first) first = false;
            else s += ", ";
            fmt::format_to(It(), "{} %{}", Ty(arg), tmp++);
        }

        s += ')';
        if (f->blocks().empty()) {
            s += '\n';
            return;
        }

        s += " {\n";
        for (auto [i, b] : vws::enumerate(f->blocks())) {
            block_indices[b] = usz(i);
            for (auto inst : b->instructions())
                if (RequiresTemporary(inst))
                    inst_indices[inst] = tmp++;
        }
        for (auto b : f->blocks()) PrintBlock(b);
        s += "}\n";
    }

    /// Emit an instruction in a block.
    void PrintInst(Inst* i) {
        /// Print a binary instruction.
        const auto PrintBinary = [&](std::string_view mnemonic) {
            auto b = as<BinaryInst>(i);
            fmt::format_to(
                It(),
                "    %{} = {} {}, {}\n",
                inst_indices[i],
                mnemonic,
                Val(b->lhs(), true),
                Val(b->rhs(), false)
            );
        };

        switch (i->kind()) {
            /// Not an instruction.
            case Value::Kind::Block:
            case Value::Kind::Function:
            case Value::Kind::IntegerConstant:
            case Value::Kind::ArrayConstant:
            case Value::Kind::Poison:
            case Value::Kind::GlobalVariable:
                LCC_UNREACHABLE();

            case Value::Kind::Alloca:
                fmt::format_to(
                    It(),
                    "    %{} = alloca {}, i64 1\n",
                    inst_indices[i],
                    Ty(as<AllocaInst>(i)->allocated_type())
                );
                return;

            case Value::Kind::Store: {
                auto store = as<StoreInst>(i);
                fmt::format_to(
                    It(),
                    "    store {}, {}\n",
                    Val(store->val()),
                    Val(store->ptr())
                );
                return;
            }

            case Value::Kind::Load: {
                auto load = as<LoadInst>(i);
                fmt::format_to(
                    It(),
                    "    %{} = load {}, {}\n",
                    inst_indices[i],
                    Ty(load->type()),
                    Val(load->ptr())
                );
                return;
            }

            case Value::Kind::Return: {
                auto ret = as<ReturnInst>(i);
                if (not ret->has_value()) s += "    ret void\n";
                else fmt::format_to(It(), "    ret {}\n", Val(ret->val()));
                return;
            }

            case Value::Kind::Add: PrintBinary("add"); return;

            case Value::Kind::Call:
            // case Value::Kind::Copy:
            case Value::Kind::GetElementPtr:
            case Value::Kind::Intrinsic:
            case Value::Kind::Parameter:
            case Value::Kind::Phi:
            case Value::Kind::Branch:
            case Value::Kind::CondBranch:
            case Value::Kind::Unreachable:
            case Value::Kind::ZExt:
            case Value::Kind::SExt:
            case Value::Kind::Trunc:
            case Value::Kind::Bitcast:
            case Value::Kind::Neg:
            case Value::Kind::Compl:
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
            case Value::Kind::Eq:
            case Value::Kind::Ne:
            case Value::Kind::SLt:
            case Value::Kind::SLe:
            case Value::Kind::SGt:
            case Value::Kind::SGe:
            case Value::Kind::ULt:
            case Value::Kind::ULe:
            case Value::Kind::UGt:
            case Value::Kind::UGe:
                LCC_TODO();
        }

        LCC_UNREACHABLE();
    }

    /// Check if the LLVM instruction that is emitted for
    /// an LLC instruction requires a temporary.
    static bool RequiresTemporary(Inst* i) {
        switch (i->kind()) {
            /// Not an instruction.
            case Value::Kind::Block:
            case Value::Kind::Function:
            case Value::Kind::IntegerConstant:
            case Value::Kind::ArrayConstant:
            case Value::Kind::Poison:
            case Value::Kind::GlobalVariable:
                LCC_UNREACHABLE();

            /// Instructions that always yield a value.
            case Value::Kind::Alloca:
            case Value::Kind::GetElementPtr:
            case Value::Kind::Load:
            case Value::Kind::Phi:
            case Value::Kind::ZExt:
            case Value::Kind::SExt:
            case Value::Kind::Trunc:
            case Value::Kind::Bitcast:
            case Value::Kind::Neg:
            case Value::Kind::Compl:
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
            case Value::Kind::Eq:
            case Value::Kind::Ne:
            case Value::Kind::SLt:
            case Value::Kind::SLe:
            case Value::Kind::SGt:
            case Value::Kind::SGe:
            case Value::Kind::ULt:
            case Value::Kind::ULe:
            case Value::Kind::UGt:
            case Value::Kind::UGe:
                return true;

            /// Instructions that may yield a value.
            case Value::Kind::Call: return as<CallInst>(i)->type() != Type::VoidTy;
            case Value::Kind::Intrinsic: LCC_TODO();

            /// Instructions that never return a value.
            case Value::Kind::Store:
            case Value::Kind::Branch:
            case Value::Kind::CondBranch:
            case Value::Kind::Return:
            case Value::Kind::Unreachable:
                return false;

            /// Instructions that are no-ops or handled specially in LLVM IR.
            //case Value::Kind::Copy:
            case Value::Kind::Parameter:
                return false;
        }

        LCC_UNREACHABLE();
    }

    /// Get the LLVM representation of a type.
    auto Ty(Type* ty) -> std::string {
        switch (ty->kind) {
            /// Ill-formed.
            case Type::Kind::Unknown: LCC_UNREACHABLE();
            case Type::Kind::Pointer: return "ptr";
            case Type::Kind::Void: return "void";

            /// Function types may only appear in direct calls or
            /// function declarations, in which case they are always
            /// receive special treatment, so if we encounter one
            /// in the wild, it has to be a function pointer.
            case Type::Kind::Function: return "ptr";

            case Type::Kind::Integer:
                return fmt::format("i{}", as<IntegerType>(ty)->bits());

            case Type::Kind::Array:
                return fmt::format(
                    "[{} x {}]",
                    as<ArrayType>(ty)->length(),
                    Ty(as<ArrayType>(ty)->element_type())
                );

            case Type::Kind::Struct: LCC_TODO();
        }

        LCC_UNREACHABLE();
    }

    /// Get the LLVM representation of a value.
    auto Val(Value* v, bool include_type = true) -> std::string {
        const auto Format =
            [&]<typename... Args>(
                fmt::format_string<Args...> fmt,
                Args&&... args
            ) -> std::string {
            std::string val;
            if (include_type) fmt::format_to(It(val), "{} ", Ty(v->type()));
            fmt::format_to(It(val), fmt, std::forward<Args>(args)...);
            return val;
        };

        switch (v->kind()) {
            case Value::Kind::Block:
                return fmt::format("label %bb{}", block_indices[as<Block>(v)]);

            /// A function name in the wild can only be a function pointer.
            case Value::Kind::Function: {
                std::string val;
                if (include_type) val += "ptr ";
                fmt::format_to(It(val), "@\"{}\"", as<Function>(v)->name());
                return val;
            }

            case Value::Kind::GlobalVariable: {
                std::string val;
                if (include_type) val += "ptr ";
                fmt::format_to(It(val), "@\"{}\"", as<GlobalVariable>(v)->name());
                return val;
            }

            case Value::Kind::IntegerConstant:
                return Format("{}", as<IntegerConstant>(v)->value());

            case Value::Kind::Poison:
                return Format("poison");

            /// This one is gonna be painful.
            case Value::Kind::ArrayConstant: LCC_TODO();

            /// For this one, we just use the underlying value.
            // case Value::Kind::Copy: return Val(as<CopyInst>(v)->value(), include_type);

            case Value::Kind::Intrinsic: LCC_TODO();

            /// These always yield a value.
            case Value::Kind::Alloca:
            case Value::Kind::GetElementPtr:
            case Value::Kind::Parameter:
            case Value::Kind::Call:
            case Value::Kind::Load:
            case Value::Kind::Phi:
            case Value::Kind::ZExt:
            case Value::Kind::SExt:
            case Value::Kind::Trunc:
            case Value::Kind::Bitcast:
            case Value::Kind::Neg:
            case Value::Kind::Compl:
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
            case Value::Kind::Eq:
            case Value::Kind::Ne:
            case Value::Kind::SLt:
            case Value::Kind::SLe:
            case Value::Kind::SGt:
            case Value::Kind::SGe:
            case Value::Kind::ULt:
            case Value::Kind::ULe:
            case Value::Kind::UGt:
            case Value::Kind::UGe:
                return Format("%{}", inst_indices[as<Inst>(v)]);

            /// These do not yield a value.
            case Value::Kind::Store:
            case Value::Kind::Branch:
            case Value::Kind::CondBranch:
            case Value::Kind::Return:
            case Value::Kind::Unreachable:
                LCC_UNREACHABLE();
        }

        LCC_UNREACHABLE();
    }
};
} // namespace
} // namespace lcc

auto lcc::Module::llvm() -> std::string {
    return LLVMIRPrinter{this}.print();
}
