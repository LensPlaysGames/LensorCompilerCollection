#include <lcc/ir/ir.hh>

#include <fmt/format.h>
#include <lcc/context.hh>
#include <lcc/diags.hh>
#include <lcc/ir/module.hh>
#include <lcc/ir/type.hh>
#include <lcc/utils/ir_printer.hh>
#include <lcc/utils/rtti.hh>

#include <algorithm>
#include <string>

namespace lcc {
Function::Function(
    Module* mod,
    std::string mangled_name,
    FunctionType* ty,
    Linkage linkage,
    CallConv calling_convention,
    Location l
) : Value(Kind::Function, ty),
    func_name(std::move(mangled_name)),
    loc(l),
    mod(mod),
    link(linkage),
    cc(calling_convention) {
    /// Create parameter instructions.
    param_list.reserve(ty->params().size());
    for (auto [i, param] : vws::enumerate(ty->params()))
        param_list.push_back(new (*mod) Parameter(param, u32(i)));

    /// Add the function to the module.
    mod->add_function(this);
}

GlobalVariable::GlobalVariable(Module* mod, Type* t, std::string name, Linkage linkage, Value* init)
    : Value(Value::Kind::GlobalVariable, t),
      _name(std::move(name)),
      _linkage(linkage),
      _init(init) {
    mod->add_var(this);
}

usz Type::bits() const {
    switch (kind) {
        case Kind::Unknown: Diag::ICE("Cannot get size of unknown type");
        case Kind::Void: return 0;

        case Kind::Function:
        case Kind::Pointer: return 64; // FIXME: Target-dependent pointer size

        case Kind::Integer: {
            const auto& i = as<IntegerType>(this);
            return i->bitwidth();
        }

        case Kind::Array: {
            const auto& array = as<ArrayType>(this);
            return array->element_type()->bits() * array->length();
        }

        case Kind::Struct: {
            const auto& struct_ = as<StructType>(this);
            const std::vector<Type*>& members = struct_->members();
            usz sum = 0;
            for (const auto& m : members)
                sum += m->bits();
            return sum;
        }
    }
    LCC_UNREACHABLE();
}

usz Type::bytes() const {
    usz bitwidth = bits();
    return bitwidth / 8 + (bitwidth % 8 ? 1 : 0);
}

usz Type::align() const {
    switch (kind) {
        case Kind::Unknown: Diag::ICE("Cannot get alignment of unknown type");

        case Kind::Function:
        case Kind::Pointer:
            return 64; // FIXME: Target-dependent pointer size

        case Kind::Void: return 1; /// Alignment of 0 is invalid.
        case Kind::Array: return as<ArrayType>(this)->element_type()->align();
        case Kind::Integer: return as<IntegerType>(this)->bitwidth();
        case Kind::Struct: return rgs::max(as<StructType>(this)->members() | vws::transform(&Type::align));
    }
    LCC_UNREACHABLE();
}

usz Type::align_bytes() const {
    usz alignment = align();
    return alignment / 8 + (alignment % 8 ? 1 : 0);
}

auto Type::string(bool use_colour) const -> std::string {
    using enum utils::Colour;
    utils::Colours C{use_colour};
    switch (kind) {
        case Kind::Unknown: return fmt::format("{}<?>", C(Cyan));
        case Kind::Pointer: return fmt::format("{}ptr", C(Cyan));
        case Kind::Void: return fmt::format("{}void", C(Cyan));

        case Kind::Array: {
            auto arr = as<ArrayType>(this);
            return fmt::format(
                "{}{}[{}{}{}]",
                arr->element_type()->string(),
                C(Red),
                C(Magenta),
                arr->length(),
                C(Red)
            );
        }

        case Kind::Integer: {
            auto integer = as<IntegerType>(this);
            return fmt::format("{}i{}", C(Cyan), integer->bitwidth());
        }

        case Kind::Function: {
            auto f = as<FunctionType>(this);
            auto ToString = [&](auto t) { return t->string(use_colour); };
            auto separator = fmt::format("{}, ", C(Red));
            return fmt::format(
                "{}{}({}{})",
                f->ret()->string(),
                C(Red),
                fmt::join(vws::transform(f->params(), ToString), separator),
                C(Red)
            );
        }

        // TODO: name, if it has one; otherwise, all the member types.
        case Kind::Struct: return "<struct>";
    }
    LCC_UNREACHABLE();
}

/// Get or create a function type.
FunctionType* FunctionType::Get(Context* ctx, Type* ret, std::vector<Type*> params) {
    // Look in ctx type cache.
    const auto& found = rgs::find_if(ctx->function_types, [&](const Type* t) {
        const FunctionType* f = as<FunctionType>(t);
        return f->ret() == ret && rgs::equal(f->params(), params);
    });
    if (found != ctx->function_types.end())
        return as<FunctionType>(*found);

    FunctionType* out = new (ctx) FunctionType(ret, params);
    ctx->function_types.push_back(out);
    return out;
}

IntegerType* IntegerType::Get(Context* ctx, usz bitwidth) {
    // Look in ctx type cache.
    const auto& found = std::find_if(ctx->integer_types.begin(), ctx->integer_types.end(), [&](const std::pair<usz, Type*>& pair) {
        return pair.first == bitwidth;
    });
    if (found != ctx->integer_types.end())
        return as<IntegerType>(found->second);

    // Create new type and store in cache.
    IntegerType* out = new (ctx) IntegerType(bitwidth);
    ctx->integer_types[bitwidth] = out;
    return out;
}

ArrayType* ArrayType::Get(Context* ctx, usz length, Type* element_type) {
    // Look in ctx type cache.
    const auto& found = rgs::find_if(ctx->array_types, [&](const Type* t) {
        const ArrayType* a = as<ArrayType>(t);
        return a->length() == length && a->element_type() == element_type;
    });
    if (found != ctx->array_types.end())
        return as<ArrayType>(*found);

    ArrayType* out = new (ctx) ArrayType(length, element_type);
    ctx->array_types.push_back(out);
    return out;
}

StructType* StructType::Get(Context* ctx, std::vector<Type*> member_types) {
    // Look in ctx type cache.
    const auto& found = rgs::find_if(ctx->struct_types, [&](const Type* t) {
        const StructType* s = as<StructType>(t);
        return rgs::equal(s->members(), member_types);
    });
    if (found != ctx->struct_types.end())
        return as<StructType>(*found);

    StructType* out = new (ctx) StructType(member_types);
    ctx->struct_types.push_back(out);
    return out;
}

Inst* Block::insert(Inst* i, bool force) {
    if (not force and closed())
        Diag::ICE("Insertion into block that has already been closed.");

    inst_list.push_back(i);

    return i;
}

bool Block::has_predecessor(Block* block) const {
    auto term = block->terminator();
    if (not term) return false;
    switch (term->kind()) {
        default: LCC_UNREACHABLE();
        case Value::Kind::Unreachable:
        case Value::Kind::Return:
            return false;

        case Value::Kind::Branch:
            return as<BranchInst>(term)->target() == this;

        case Value::Kind::CondBranch: {
            auto cond_branch = as<CondBranchInst>(term);
            return cond_branch->then_block() == this or cond_branch->else_block() == this;
        }
    }
}

namespace {
struct LCCIRPrinter : IRPrinter<LCCIRPrinter, 2> {
    /// Print the function signature.
    void PrintFunctionHeader(Function* f) {
        auto ftype = as<FunctionType>(f->type());
        Print(
            "{}{} {}: {}{}(",
            C(Green),
            f->name(),
            C(Red),
            ftype->ret()->string(use_colour),
            C(Red)
        );

        bool first = true;
        for (auto [i, arg] : vws::enumerate(ftype->params())) {
            if (first) first = false;
            else Print("{}, ", C(Red));
            Print("{} {}%{}", arg->string(use_colour), C(TempColour), i);
        }

        Print("{})", C(Red));
        if (f->imported()) Print(" external");
    }

    /// Print start/end of function body.
    void EnterFunctionBody(Function*) { Print(":\n"); }
    void ExitFunctionBody(Function*) {}

    /// Print the start of a temporary.
    void PrintTemp(Inst* i) {
        Print("    {}%{} {}= {}", C(TempColour), Index(i), C(Red), C(Yellow));
    };

    /// Print a cast instruction.
    void PrintCast(Inst* i, std::string_view mnemonic) {
        auto c = as<UnaryInstBase>(i);
        PrintTemp(i);
        Print(
            "{} {} {}to {}",
            mnemonic,
            Val(c->operand(), true),
            C(Red),
            c->type()->string(use_colour)
        );
    }

    /// Print a binary instruction.
    void PrintBinary(Inst* i, std::string_view mnemonic) {
        auto b = as<BinaryInst>(i);
        PrintTemp(i);
        Print(
            "{} {}{}, {}",
            mnemonic,
            Val(b->lhs(), true),
            C(Red),
            Val(b->rhs(), false)
        );
    };

    /// Print an instruction.
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

            /// Instructions.
            case Value::Kind::Alloca: {
                auto local = as<AllocaInst>(i);
                PrintTemp(i);
                Print("alloca {}", local->allocated_type()->string(use_colour));
                return;
            }

            case Value::Kind::Call: {
                auto c = as<CallInst>(i);
                auto callee_ty = as<FunctionType>(c->callee()->type());
                if (not callee_ty->ret()->is_void()) PrintTemp(i);
                else Print("    {}", C(Yellow));
                Print(
                    "{}call {} {}(",
                    c->is_tail_call() ? "tail " : "",
                    Val(c->callee(), false),
                    C(Red)
                );

                bool first = true;
                for (auto arg : c->args()) {
                    if (first) first = false;
                    else Print("{}, ", C(Red));
                    Print("{}", Val(arg));
                }

                if (callee_ty->ret()->is_void()) Print("{})", C(Red));
                else Print("{}) -> {}", C(Red), callee_ty->ret()->string(use_colour));
                return;
            }

            case Value::Kind::GetElementPtr: {
                auto gep = as<GEPInst>(i);
                PrintTemp(i);
                Print(
                    "gep {} {}from {} {}at {}",
                    gep->base_type()->string(use_colour),
                    C(Red),
                    Val(gep->ptr(), false),
                    C(Red),
                    Val(gep->idx())
                );
                return;
            }

            case Value::Kind::Intrinsic: LCC_TODO();

            case Value::Kind::Load: {
                auto load = as<LoadInst>(i);
                PrintTemp(i);
                Print(
                    "load {} {}from {}",
                    load->type()->string(use_colour),
                    C(Red),
                    Val(load->ptr(), false)
                );
                return;
            }

            case Value::Kind::Phi: {
                auto phi = as<PhiInst>(i);
                const auto separator = fmt::format("{}, ", C(Red));
                const auto FormatPHIVal = [this](auto& val) {
                    return fmt::format(
                        "{}[{} {}: {}{}]",
                        C(Red),
                        Val(val.block, false),
                        C(Red),
                        Val(val.value, false),
                        C(Red)
                    );
                };

                PrintTemp(i);
                Print(
                    "phi {}{}, {}",
                    phi->type()->string(use_colour),
                    C(Red),
                    fmt::join(vws::transform(phi->operands(), FormatPHIVal), separator)
                );
                return;
            }

            case Value::Kind::Store: {
                auto store = as<StoreInst>(i);
                Print(
                    "    {}store {} {}into {}",
                    C(Yellow),
                    Val(store->val()),
                    C(Red),
                    Val(store->ptr(), false)
                );
                return;
            }

            /// Terminators.
            case Value::Kind::Branch: {
                auto branch = as<BranchInst>(i);
                Print(
                    "    {}branch {}to {}",
                    C(Yellow),
                    C(Red),
                    Val(branch->target(), false)
                );
                return;
            }

            case Value::Kind::CondBranch: {
                auto branch = as<CondBranchInst>(i);
                Print(
                    "    {}branch {}on {} {}to {} {}else {}",
                    C(Yellow),
                    C(Red),
                    Val(branch->cond(), false),
                    C(Red),
                    Val(branch->then_block(), false),
                    C(Red),
                    Val(branch->else_block(), false)
                );
                return;
            }

            case Value::Kind::Return: {
                auto ret = as<ReturnInst>(i);
                if (ret->val()) Print("    {}return {}", C(Yellow), Val(ret->val()));
                else Print("    {}return", C(Yellow));
                return;
            }

            case Value::Kind::Unreachable: {
                Print("    {}unreachable", C(Yellow));
                return;
            }

            /// Unary instructions.
            case Value::Kind::Bitcast: PrintCast(i, "bitcast"); return;
            case Value::Kind::ZExt: PrintCast(i, "zero.extend"); return;
            case Value::Kind::SExt: PrintCast(i, "sign.extend"); return;
            case Value::Kind::Trunc: PrintCast(i, "truncate"); return;

            case Value::Kind::Neg: {
                auto neg = as<UnaryInstBase>(i);
                PrintTemp(i);
                Print("negate {}", Index(i), Val(neg->operand()));
                return;
            }

            case Value::Kind::Compl: {
                auto c = as<UnaryInstBase>(i);
                PrintTemp(i);
                Print("compl {}", Index(i), Val(c->operand()));
                return;
            }

            /// Binary instructions.
            case Value::Kind::Add: PrintBinary(i, "add"); return;
            case Value::Kind::Sub: PrintBinary(i, "sub"); return;
            case Value::Kind::Mul: PrintBinary(i, "mul"); return;
            case Value::Kind::SDiv: PrintBinary(i, "s.div"); return;
            case Value::Kind::UDiv: PrintBinary(i, "u.div"); return;
            case Value::Kind::SRem: PrintBinary(i, "s.rem"); return;
            case Value::Kind::URem: PrintBinary(i, "u.rem"); return;
            case Value::Kind::Shl: PrintBinary(i, "shl"); return;
            case Value::Kind::Sar: PrintBinary(i, "sar"); return;
            case Value::Kind::Shr: PrintBinary(i, "shr"); return;
            case Value::Kind::And: PrintBinary(i, "and"); return;
            case Value::Kind::Or: PrintBinary(i, "or"); return;
            case Value::Kind::Xor: PrintBinary(i, "xor"); return;
            case Value::Kind::Eq: PrintBinary(i, "eq"); return;
            case Value::Kind::Ne: PrintBinary(i, "ne"); return;
            case Value::Kind::SLt: PrintBinary(i, "s.lt"); return;
            case Value::Kind::SLe: PrintBinary(i, "s.le"); return;
            case Value::Kind::SGt: PrintBinary(i, "s.gt"); return;
            case Value::Kind::SGe: PrintBinary(i, "s.ge"); return;
            case Value::Kind::ULt: PrintBinary(i, "u.lt"); return;
            case Value::Kind::ULe: PrintBinary(i, "u.le"); return;
            case Value::Kind::UGt: PrintBinary(i, "u.gt"); return;
            case Value::Kind::UGe: PrintBinary(i, "u.ge"); return;
        }

        LCC_UNREACHABLE();
    }

    /// Print a global variable.
    void PrintGlobal(GlobalVariable* v) {
        Print(
            "{}{} {}: {} {}",
            C(TempColour),
            v->name(),
            C(Red),
            v->type()->string(use_colour),
            C(Red)
        );

        if (v->imported()) {
            Print("external\n");
            return;
        }

        if (v->init()) Print("= {}\n", Val(v->init(), false));
        else Print("= {}0\n", C(Magenta));
    }

    /// Get the inline representation of a value.
    ///
    /// In some contexts, the type is obvious (e.g. the address
    /// of a store is always of type \c ptr), so there is no reason
    /// to include it in the printout. The type is omitted if \c false
    /// is passed for \c include_type.
    auto Val(Value* v, bool include_type = true) -> std::string {
        const auto Format =
            [&]<typename... Args>(
                fmt::format_string<Args...> fmt,
                Args&&... args
            ) -> std::string {
            std::string val;
            if (include_type) fmt::format_to(It(val), "{} ", v->type()->string(use_colour));
            fmt::format_to(It(val), fmt, std::forward<Args>(args)...);
            return val;
        };

        switch (v->kind()) {
            case Value::Kind::Function:
                return Format("{}@{}", C(Green), as<Function>(v)->name());

            case Value::Kind::IntegerConstant:
                return Format("{}{}", C(Magenta), as<IntegerConstant>(v)->value());

            case Value::Kind::Poison:
                return Format("{}poison", C(Red));

            case Value::Kind::Parameter:
                return Format("{}%{}", C(TempColour), as<Parameter>(v)->index());

            case Value::Kind::Block: {
                return fmt::format(
                    "{}{}{}%bb{}",
                    C(Cyan),
                    include_type ? "block " : "",
                    C(BlockColour),
                    Index(as<Block>(v))
                );
            }

            case Value::Kind::GlobalVariable: {
                std::string val;
                if (include_type) fmt::format_to(It(val), "{}ptr ", C(Cyan));
                fmt::format_to(It(val), "{}@{}", C(TempColour), as<GlobalVariable>(v)->name());
                return val;
            }

            /// TODO: Format this differently based on the array type?
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
                        "{}\"{}\"",
                        C(Yellow),
                        fmt::join(
                            std::span<const char>(a->data(), a->size()) | vws::transform(FormatChar),
                            ""
                        )
                    );
                }

                /// Array
                else {
                    return Format(
                        "{}[{}{}{}]",
                        C(Red),
                        C(Magenta),
                        fmt::join(
                            std::span<const u8>(reinterpret_cast<const u8*>(a->data()), a->size()),
                            " "
                        ),
                        C(Red)
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
                return Format("{}%{}", C(TempColour), Index(as<Inst>(v)));

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

    /// Check if an instruction requires a temporary.
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
};
} // namespace

void Module::print_ir(bool use_colour) {
    fmt::print("{}", LCCIRPrinter::Print(this, use_colour));
}

} // namespace lcc
