#include <lcc/ir/module.hh>
#include <lcc/utils/ir_printer.hh>

namespace lcc {
namespace {
struct LLVMIRPrinter : IRPrinter<LLVMIRPrinter, 0> {
    /// Emit a function signature
    void PrintFunctionHeader(Function* f) {
        auto ftype = as<FunctionType>(f->type());
        Print(
            "{} {} {} {} (",
            f->imported() ? "declare" : "define",
            f->linkage() == Linkage::Internal ? "private" : "external",
            Ty(ftype->ret()),
            FormatName(f->name())
        );

        bool first = true;
        for (auto [i, arg] : vws::enumerate(ftype->params())) {
            if (first) first = false;
            else Print(", ");
            Print("{} %{}", Ty(arg), i);
        }

        Print(")");
    }

    /// Print start/end of function body.
    void EnterFunctionBody(Function*) { Print(" {{\n"); }
    void ExitFunctionBody(Function*) { Print("}}\n"); }

    /// Print a binary instruction.
    void PrintBinary(Inst* i, std::string_view mnemonic) {
        auto b = as<BinaryInst>(i);
        Print(
            "    %{} = {} {}, {}",
            Index(i),
            mnemonic,
            Val(b->lhs(), true),
            Val(b->rhs(), false)
        );
    };

    /// Print a comparison.
    void PrintComparison(Inst* i, std::string_view mnemonic) {
        auto c = as<BinaryInst>(i);
        Print(
            "    %{} = icmp {} {}, {}",
            Index(i),
            mnemonic,
            Val(c->lhs(), true),
            Val(c->rhs(), false)
        );
    }

    /// Print a cast instruction.
    void PrintCast(Inst* i, std::string_view mnemonic) {
        auto c = as<UnaryInstBase>(i);
        Print(
            "    %{} = {} {} to {}",
            Index(i),
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
            case Value::Kind::Parameter:
                LCC_UNREACHABLE();

            case Value::Kind::Alloca:
                Print(
                    "    %{} = alloca {}, i64 1",
                    Index(i),
                    Ty(as<AllocaInst>(i)->allocated_type())
                );
                return;

            case Value::Kind::Store: {
                auto store = as<StoreInst>(i);
                Print(
                    "    store {}, {}",
                    Val(store->val()),
                    Val(store->ptr())
                );
                return;
            }

            case Value::Kind::Load: {
                auto load = as<LoadInst>(i);
                Print(
                    "    %{} = load {}, {}",
                    Index(i),
                    Ty(load->type()),
                    Val(load->ptr())
                );
                return;
            }

            case Value::Kind::Return: {
                auto ret = as<ReturnInst>(i);
                if (not ret->has_value()) Print("    ret void");
                else Print("    ret {}", Val(ret->val()));
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
                    auto idx = Index(i);
                    Print("    %.{}.alloca = alloca {}, i64 1\n", idx, Ty(from));
                    Print("    store {}, ptr %.{}.alloca\n", Val(c->operand()), idx);
                    Print("    %{} = load {}, ptr %.{}.alloca", idx, Ty(to), idx);
                }

                return;
            }

            /// Function call.
            case Value::Kind::Call: {
                auto c = as<CallInst>(i);
                auto callee_ty = as<FunctionType>(c->callee()->type());
                if (not callee_ty->ret()->is_void()) Print("    %{} = ", Index(i));
                else Print("    ");
                Print(
                    "{}call {} {} (",
                    c->is_tail_call() ? "tail " : "",
                    Ty(callee_ty->ret()),
                    Val(c->callee(), false)
                );

                bool first = true;
                for (auto arg : c->args()) {
                    if (first) first = false;
                    else Print(", ");
                    Print("{}", Val(arg));
                }

                Print(")");
                return;
            }

            /// There is no negate instruction in LLVM.
            case Value::Kind::Neg: {
                auto n = as<NegInst>(i);
                Print(
                    "    %{} = sub {} 0, {}",
                    Index(i),
                    Ty(n->type()),
                    Val(n->operand(), false)
                );
                return;
            }

            /// There is no complement instruction in LLVM
            case Value::Kind::Compl: {
                auto c = as<ComplInst>(i);
                Print(
                    "    %{} = xor {}, -1",
                    Index(i),
                    Val(c->operand())
                );
                return;
            }

            case Value::Kind::CondBranch: {
                auto br = as<CondBranchInst>(i);
                Print(
                    "    br {}, {}, {}",
                    Val(br->cond()),
                    Val(br->then_block()),
                    Val(br->else_block())
                );
                return;
            }

            case Value::Kind::Branch: {
                auto br = as<BranchInst>(i);
                Print(
                    "    br {}",
                    Val(br->target())
                );
                return;
            }

            /// Currently, GEPs are only used for single-operand
            /// pointer arithmetic.
            case Value::Kind::GetElementPtr: {
                auto gep = as<GEPInst>(i);
                Print(
                    "    %{} = getelementptr {}, {}, {}",
                    Index(i),
                    Ty(gep->base_type()),
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

                Print(
                    "    %{} = phi {} {}",
                    Index(i),
                    Ty(phi->type()),
                    fmt::join(vws::transform(phi->operands(), FormatPHIVal), ", ")
                );
                return;
            }

            case Value::Kind::Unreachable: {
                Print("    unreachable");
                return;
            }

            case Value::Kind::Intrinsic:
                LCC_TODO();
        }

        LCC_UNREACHABLE();
    }

    /// Print a global variable declaration or definition.
    void PrintGlobal(GlobalVariable* v) {
        const bool is_string = v->init() and is<ArrayConstant>(v->init()) and as<ArrayConstant>(v->init())->is_string_literal();
        Print(
            "{} = {} {} {} {}, align {}\n",
            FormatName(v->name()),
            v->imported() ? "external" : "private",
            is_string ? "unnamed_addr constant" : "global",
            Ty(v->type()),
            v->init() ? Val(v->init(), false) : "zeroinitializer",
            v->type()->align()
        );
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
            case Value::Kind::Parameter:
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
        }

        LCC_UNREACHABLE();
    }


    /// Format a name for use in LLVM IR.
    auto FormatName (std::string_view name) -> std::string {
        auto printable = rgs::none_of(name, [](auto c) { return std::isspace(c); });
        if (printable) return fmt::format("@{}", name);
        else return fmt::format("@\"{}\"", name);
    };

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
                    Index(as<Block>(v))
                );

            /// A function name in the wild can only be a function pointer.
            case Value::Kind::Function: {
                std::string val;
                if (include_type) val += "ptr ";
                val += FormatName(as<Function>(v)->name());
                return val;
            }

            case Value::Kind::GlobalVariable: {
                std::string val;
                if (include_type) val += "ptr ";
                val += FormatName(as<GlobalVariable>(v)->name());
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

                /// String.
                if (a->is_string_literal()) {
                    static const auto FormatChar = [](u8 c) {
                        return std::isprint(c) and c != '\"'
                            ? std::string{char(c)}
                            : fmt::format("\\{:02X}", u8(c));
                    };

                    return Format(
                        "c\"{}\"",
                        fmt::join(
                            std::span<const char>(a->data(), a->size()) | vws::transform(FormatChar),
                            ""
                        )
                    );
                }

                /// Array.
                else {
                    return Format(
                        "[{}]",
                        fmt::join(
                            std::span<const char>(a->data(), a->size()) //
                                | vws::transform([](auto c) { return fmt::format("i8 {}", u8(c)); }),
                            ", "
                        )
                    );
                }
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
                return Format("%{}", Index(as<Inst>(v)));

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
    return LLVMIRPrinter::Print(this, false);
}
