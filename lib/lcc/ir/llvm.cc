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
        tmp = 0;
        auto ftype = as<FunctionType>(f->type());
        fmt::format_to(
            It(),
            "{} {} {} @\"{}\" (",
            f->blocks().empty() ? "declare" : "define",
            f->linkage() == Linkage::Internal ? "private" : "external",
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

    /// Print a binary instruction.
    void PrintBinary(Inst* i, std::string_view mnemonic) {
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

    /// Print a comparison.
    void PrintComparison(Inst* i, std::string_view mnemonic) {
        auto c = as<BinaryInst>(i);
        fmt::format_to(
            It(),
            "    %{} = icmp {} {}, {}\n",
            inst_indices[i],
            mnemonic,
            Val(c->lhs(), true),
            Val(c->rhs(), false)
        );
    }

    /// Print a cast instruction.
    void PrintCast(Inst* i, std::string_view mnemonic) {
        auto c = as<UnaryInstBase>(i);
        fmt::format_to(
            It(),
            "    %{} = {} {} to {}\n",
            inst_indices[i],
            mnemonic,
            Val(c->operand(), true),
            Ty(c->type())
        );
    }

    /// Emit an instruction in a block.
    void PrintInst(Inst* i) {
        switch (i->kind()) {
            /// Not an instruction.
            case Value::Kind::Block:
            case Value::Kind::Function:
            case Value::Kind::IntegerConstant:
            case Value::Kind::ArrayConstant:
            case Value::Kind::Poison:
            case Value::Kind::GlobalVariable:
                LCC_UNREACHABLE();

            /// Always emitted in-line.
            case Value::Kind::Parameter:
                return;

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

            case Value::Kind::Add: PrintBinary(i, "add"); return;
            case Value::Kind::Sub: PrintBinary(i, "sub"); return;
            case Value::Kind::Mul: PrintBinary(i, "mul"); return;
            case Value::Kind::SDiv: PrintBinary(i, "sdiv"); return;
            case Value::Kind::UDiv: PrintBinary(i, "udiv"); return;
            case Value::Kind::SRem: PrintBinary(i, "srem"); return;
            case Value::Kind::URem: PrintBinary(i, "urem"); return;
            case Value::Kind::Shl: PrintBinary(i, "shl"); return;
            case Value::Kind::Sar: PrintBinary(i, "ashr"); return;
            case Value::Kind::Shr: PrintBinary(i, "lshr"); return;
            case Value::Kind::And: PrintBinary(i, "and"); return;
            case Value::Kind::Or: PrintBinary(i, "or"); return;
            case Value::Kind::Xor: PrintBinary(i, "xor"); return;

            case Value::Kind::Eq: PrintComparison(i, "eq"); return;
            case Value::Kind::Ne: PrintComparison(i, "ne"); return;
            case Value::Kind::SLt: PrintComparison(i, "slt"); return;
            case Value::Kind::SLe: PrintComparison(i, "sle"); return;
            case Value::Kind::SGt: PrintComparison(i, "sgt"); return;
            case Value::Kind::SGe: PrintComparison(i, "sge"); return;
            case Value::Kind::ULt: PrintComparison(i, "ult"); return;
            case Value::Kind::ULe: PrintComparison(i, "ule"); return;
            case Value::Kind::UGt: PrintComparison(i, "ugt"); return;
            case Value::Kind::UGe: PrintComparison(i, "uge"); return;

            case Value::Kind::ZExt: PrintCast(i, "zext"); return;
            case Value::Kind::SExt: PrintCast(i, "sext"); return;
            case Value::Kind::Trunc: PrintCast(i, "trunc"); return;

            /// Bitcast is special because we need to potentially
            /// do several different things to emit this in LLVM:
            ///
            ///   - Emit a `bitcast ... to`
            ///   - Emit an `inttoptr`
            ///   - Emit a `ptrtoint`
            ///   - ‘Cast’ via alloca-store-load.
            ///
            /// Funnily enough, after the introduction of opaque
            /// pointers, the only remaining use case for LLVM’s
            /// `bitcast` instruction seems to be to cast between
            /// vectors and integers; since we don’t support the
            /// former at the moment, our bitcast is never actually
            /// a `bitcast` instruction.
            case Value::Kind::Bitcast: {
                auto c = cast<BitcastInst>(i);
                auto from = c->operand()->type();
                auto to = c->type();

                /// Int -> Pointer.
                if (is<IntegerType>(from) and to->is_ptr()) PrintCast(i, "inttoptr");

                /// Pointer -> Int.
                else if (from->is_ptr() and is<IntegerType>(to)) PrintCast(i, "ptrtoint");

                /// Scuffed bitcast.
                else {
                    auto idx = inst_indices[i];
                    fmt::format_to(It(), "    %.{}.alloca = alloca {}, i64 1\n", idx, Ty(from));
                    fmt::format_to(It(), "    store {}, ptr %.{}.alloca\n", Val(c->operand()), idx);
                    fmt::format_to(It(), "    %{} = load {}, ptr %.{}.alloca\n", idx, Ty(to), idx);
                }

                return;
            }

            /// Function call.
            case Value::Kind::Call: {
                auto c = as<CallInst>(i);
                auto callee_ty = as<FunctionType>(c->callee()->type());
                fmt::format_to(
                    It(),
                    "    %{} = {}call {} {} (",
                    inst_indices[i],
                    c->is_tail_call() ? "tail " : "",
                    Ty(callee_ty->ret()),
                    Val(c->callee(), false)
                );

                bool first = true;
                for (auto arg : c->args()) {
                    if (first) first = false;
                    else s += ", ";
                    fmt::format_to(It(), "{}", Val(arg));
                }

                s += ")\n";
                return;
            }

            /// There is no negate instruction in LLVM.
            case Value::Kind::Neg: {
                auto n = as<NegInst>(i);
                fmt::format_to(
                    It(),
                    "    %{} = sub {} 0, {}\n",
                    inst_indices[i],
                    Ty(n->type()),
                    Val(n->operand(), false)
                );
                return;
            }

            /// There is no complement instruction in LLVM
            case Value::Kind::Compl: {
                auto c = as<ComplInst>(i);
                fmt::format_to(
                    It(),
                    "    %{} = xor {}, -1\n",
                    inst_indices[i],
                    Val(c->operand())
                );
                return;
            }

            case Value::Kind::CondBranch: {
                auto br = as<CondBranchInst>(i);
                fmt::format_to(
                    It(),
                    "    br {}, {}, {}\n",
                    Val(br->cond()),
                    Val(br->then_block()),
                    Val(br->else_block())
                );
                return;
            }

            case Value::Kind::Branch: {
                auto br = as<BranchInst>(i);
                fmt::format_to(
                    It(),
                    "    br {}\n",
                    Val(br->target())
                );
                return;
            }

            /// Currently, GEPs are only used for single-operand
            /// pointer arithmetic.
            case Value::Kind::GetElementPtr: {
                auto gep = as<GEPInst>(i);
                fmt::format_to(
                    It(),
                    "    %{} = getelementptr {}, {}, {}\n",
                    inst_indices[i],
                    Ty(gep->type()),
                    Val(gep->ptr()),
                    Val(gep->idx())
                );
                return;
            }

            /// Emit an LLVM-compatible PHI node.
            case Value::Kind::Phi: {
                auto phi = as<PhiInst>(i);
                phi->drop_stale_operands();
                LCC_ASSERT(
                    not phi->operands().empty(),
                    "PHI instruction has no valid incoming values"
                );

                const auto FormatPHIVal = [this](auto& val) {
                    return fmt::format(
                        "[{}, {}]",
                        Val(val.value, false),
                        Val(val.block, false)
                    );
                };

                fmt::format_to(
                    It(),
                    "    %{} = phi {} {}\n",
                    inst_indices[i],
                    Ty(phi->type()),
                    fmt::join(vws::transform(phi->operands(), FormatPHIVal), ", ")
                );
                return;
            }

            case Value::Kind::Unreachable: {
                s += "    unreachable\n";
                return;
            }

            case Value::Kind::Intrinsic:
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
                return fmt::format(
                    "{}%bb{}",
                    include_type ? "label " : "",
                    block_indices[as<Block>(v)]
                );

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

            /// Index is the temporary index.
            case Value::Kind::Parameter:
                return Format("%{}", as<Parameter>(v)->index());

            /// Here be dragons.
            case Value::Kind::ArrayConstant: {
                auto a = as<ArrayConstant>(v);
                return Format(
                    "[{}]",
                    fmt::join(
                        std::span<const char>(a->data(), a->size()) //
                            | vws::transform([](auto c) { return fmt::format("i8 {}", u8(c)); }),
                        ", "
                    )
                );
            }

            case Value::Kind::Intrinsic: LCC_TODO();

            /// These always yield a value.
            case Value::Kind::Alloca:
            case Value::Kind::GetElementPtr:
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
