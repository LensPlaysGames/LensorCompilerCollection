#include <lcc/ir/core.hh>

#include <lcc/context.hh>
#include <lcc/core.hh>
#include <lcc/diags.hh>
#include <lcc/ir/domtree.hh>
#include <lcc/ir/module.hh>
#include <lcc/ir/type.hh>
#include <lcc/utils.hh>
#include <lcc/utils/fractionals.hh>
#include <lcc/utils/generator.hh>
#include <lcc/utils/ir_printer.hh>
#include <lcc/utils/rtti.hh>

#include <fmt/format.h>
#include <fmt/ranges.h>

#include <algorithm>
#include <cctype>
#include <functional>
#include <iterator>
#include <ranges>
#include <span>
#include <string>
#include <string_view>
#include <unordered_set>
#include <utility>
#include <vector>

namespace lcc {
Function::Function(
    Module* module,
    std::string mangled_name,
    FunctionType* ty,
    Linkage linkage,
    CallConv calling_convention,
    Location location
) : UseTrackingValue(Kind::Function, ty),
    _location(location),
    mod(module),
    cc(calling_convention) {
    LCC_ASSERT(mod and ty);
    add_name(std::move(mangled_name), linkage);

    /// Create parameter instructions.
    param_list.reserve(ty->params().size());
    for (auto [i, param] : vws::enumerate(ty->params()))
        param_list.push_back(new (*mod) Parameter(param, u32(i)));

    /// Add the function to the module.
    mod->add_function(this);
}

GlobalVariable::GlobalVariable(Module* mod, Type* t, std::string name, Linkage linkage, Value* init)
    : UseTrackingValue(Value::Kind::GlobalVariable, Type::PtrTy),
      _init(init),
      _allocated_type(t) {
    LCC_ASSERT(mod and t);
    _names.push_back({std::move(name), linkage});
    mod->add_var(this);
}

GlobalVariable* GlobalVariable::CreateStringPtr(Module* mod, std::string name, std::string_view str) {
    LCC_ASSERT(mod);
    auto context = mod->context();
    LCC_ASSERT(context);
    // FIXME: magic number 8
    auto ty = lcc::ArrayType::Get(
        context,
        str.length() + 1,
        lcc::IntegerType::Get(context, 8)
    );

    std::vector<char> data{str.begin(), str.end()};
    data.push_back(0);
    auto constant = new (*mod) ArrayConstant(
        ty,
        std::move(data),
        true
    );

    // String literals use a global variable under the hood.
    auto var = new (*mod) GlobalVariable(
        mod,
        ty,
        std::move(name),
        Linkage::Internal,
        constant
    );

    return var;
}

auto Type::bits() const -> usz {
    switch (kind) {
        case Kind::Unknown:
            Diag::ICE("Cannot get size of unknown type");

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
            const auto strict_size = rgs::fold_left(vws::transform(members, &Type::bits), 0, std::plus{});
            return utils::AlignTo(strict_size, struct_->align());
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
        case Kind::Struct: {
            auto s = as<StructType>(this);

            if (s->alignment() != StructType::AlignNotSet)
                return s->alignment();

            return rgs::max(
                vws::transform(s->members(), &Type::align)
            );
        }
    }
    LCC_UNREACHABLE();
}

usz Type::align_bytes() const {
    usz alignment = align();
    return alignment / 8 + (alignment % 8 ? 1 : 0);
}

auto Type::string(bool use_colour) const -> std::string {
    using P = lcc::IRColourPalette;
    utils::Colours C{use_colour};
    switch (kind) {
        case Kind::Unknown: return fmt::format("{}<?>{}", C(P::Type), C(P::Reset));
        case Kind::Pointer: return fmt::format("{}ptr{}", C(P::Type), C(P::Reset));
        case Kind::Void: return fmt::format("{}void{}", C(P::Type), C(P::Reset));

        case Kind::Array: {
            auto arr = as<ArrayType>(this);
            return fmt::format(
                "{}{}[{}{}{}]{}",
                arr->element_type()->string(use_colour),
                C(P::Filler),
                C(P::Literal),
                arr->length(),
                C(P::Filler),
                C(P::Reset)
            );
        }

        case Kind::Integer: {
            auto integer = as<IntegerType>(this);
            return fmt::format("{}i{}{}", C(P::Type), integer->bitwidth(), C(P::Reset));
        }

        case Kind::Function: {
            auto f = as<FunctionType>(this);
            auto ToString = [&](auto t) { return t->string(use_colour); };
            auto separator = fmt::format("{}, ", C(P::Filler));
            auto variadic = f->variadic() ? fmt::format(" {}variadic", C(P::Filler)) : "";
            return fmt::format(
                "{}{}({}{}){}{}",
                f->ret()->string(use_colour),
                C(P::Filler),
                fmt::join(vws::transform(f->params(), ToString), separator),
                C(P::Filler),
                variadic,
                C(P::Reset)
            );
        }

        case Kind::Struct: {
            auto s = as<StructType>(this);
            if (s->named()) return fmt::format("{}{}{}", C(P::Name), s->name(), C(P::Reset));
            return fmt::format("{}__struct_{}{}", C(P::Name), s->index(), C(P::Reset));
            // return fmt::format(
            //     "struct {{{}}}",
            //     fmt::join(
            //         vws::transform(s->members(), [](auto m) { return *m; }),
            //         ","
            //     )
            // );
        }
    }
    LCC_UNREACHABLE();
}

/// Get or create a function type.
FunctionType* FunctionType::Get(Context* ctx, Type* ret, std::vector<Type*> params, bool is_variadic) {
    LCC_ASSERT(ctx and ret);

    // Look in ctx type cache.
    const auto& found = rgs::find_if(ctx->function_types, [&](const Type* t) {
        const FunctionType* f = as<FunctionType>(t);
        return f->ret() == ret && rgs::equal(f->params(), params) && f->variadic() == is_variadic;
    });
    if (found != ctx->function_types.end())
        return as<FunctionType>(*found);

    FunctionType* out = new (ctx) FunctionType(ret, params, is_variadic);
    ctx->function_types.push_back(out);
    return out;
}

IntegerType* IntegerType::Get(Context* ctx, usz bitwidth) {
    LCC_ASSERT(ctx);

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
    LCC_ASSERT(ctx and element_type);

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

StructType* StructType::Get(Context* ctx, std::vector<Type*> member_types, usz align_bits, std::string name) {
    LCC_ASSERT(ctx);

    // Look in ctx type cache.
    const auto& found = rgs::find_if(ctx->struct_types, [&](const Type* t) {
        const StructType* s = as<StructType>(t);
        return s->named()
                 ? s->name() == name and rgs::equal(s->members(), member_types)
                 : rgs::equal(as<StructType>(ctx->struct_types[usz(s->index())])->members(), member_types);
    });
    if (found != ctx->struct_types.end())
        return as<StructType>(*found);

    StructType* out;
    if (not name.empty())
        out = new (ctx) StructType(member_types, name);
    else out = new (ctx) StructType(member_types, (long int) ctx->struct_types.size());

    out->_align = align_bits;

    ctx->struct_types.push_back(out);
    return out;
}

void Block::insert_before(Inst* to_insert, Inst* before) {
    LCC_ASSERT(to_insert and before);
    auto before_it = rgs::find(inst_list, before);
    inst_list.insert(before_it, to_insert);
    to_insert->parent = this;
}

void Block::insert_after(Inst* to_insert, Inst* after) {
    LCC_ASSERT(to_insert and after);
    auto after_it = rgs::find(inst_list, after);
    inst_list.insert(after_it + 1, to_insert);
    to_insert->parent = this;
}

Inst* Block::insert(Inst* i, bool force) {
    LCC_ASSERT(i);
    if (not force and closed())
        Diag::ICE("Insertion into block that has already been closed.");

    inst_list.push_back(i);
    i->parent = this;

    return i;
}

bool Block::has_predecessor(Block* block) const {
    LCC_ASSERT(block);
    auto* term = block->terminator();
    if (not term) return false;
    switch (term->kind()) {
        default: LCC_UNREACHABLE();
        case Value::Kind::Unreachable:
        case Value::Kind::Return:
            return false;

        case Value::Kind::Branch:
            return as<BranchInst>(term)->target() == this;

        case Value::Kind::CondBranch: {
            auto* cond_branch = as<CondBranchInst>(term);
            return cond_branch->then_block() == this or cond_branch->else_block() == this;
        }
    }
}

usz Block::id() const {
    if (not parent) return 0;
    return usz(std::distance(parent->blocks().begin(), rgs::find(parent->blocks(), this)));
}

auto Inst::Children() -> Generator<Value**> {
    switch (kind()) {
        case Kind::Block:
        case Kind::Function:
        case Kind::IntegerConstant:
        case Kind::FractionalConstant:
        case Kind::ArrayConstant:
        case Kind::Poison:
        case Kind::GlobalVariable:
        case Kind::Parameter:
            LCC_UNREACHABLE();

        case Kind::Alloca:
        case Kind::Branch:
        case Kind::Unreachable:
            break;

        case Kind::Call: {
            auto* c = as<CallInst>(this);
            co_yield &c->callee_value;
            for (auto& a : c->arguments) co_yield &a;
        } break;

        case Kind::GetElementPtr: {
            auto* gep = as<GEPInst>(this);
            co_yield &gep->pointer;
            co_yield &gep->index;
        } break;

        case Kind::GetMemberPtr: {
            auto* gmp = as<GetMemberPtrInst>(this);
            co_yield &gmp->pointer;
            co_yield &gmp->index;
        } break;

        case Kind::Intrinsic: {
            auto* i = as<IntrinsicInst>(this);
            for (auto* a : i->operand_list) co_yield &a;
        } break;

        case Kind::Load: {
            auto* load = as<LoadInst>(this);
            co_yield &load->pointer;
        } break;

        case Kind::Phi: {
            auto* phi = as<PhiInst>(this);
            for (auto& [value, _] : phi->incoming) co_yield &value;
        } break;

        case Kind::Store: {
            auto* s = as<StoreInst>(this);
            co_yield &s->pointer;
            co_yield &s->value;
        } break;

        case Kind::CondBranch: {
            auto* br = as<CondBranchInst>(this);
            co_yield &br->condition;
        } break;

        case Kind::Return: {
            auto* ret = as<ReturnInst>(this);
            if (ret->has_value()) co_yield &ret->value;
        } break;

        case Kind::ZExt:
        case Kind::SExt:
        case Kind::Trunc:
        case Kind::Bitcast:
        case Kind::Neg:
        case Kind::Copy:
        case Kind::Compl: {
            auto* u = cast<UnaryInstBase>(this);
            co_yield &u->op;
        } break;

        case Kind::Add:
        case Kind::Sub:
        case Kind::Mul:
        case Kind::SDiv:
        case Kind::UDiv:
        case Kind::SRem:
        case Kind::URem:
        case Kind::Shl:
        case Kind::Sar:
        case Kind::Shr:
        case Kind::And:
        case Kind::Or:
        case Kind::Xor:
        case Kind::Eq:
        case Kind::Ne:
        case Kind::SLt:
        case Kind::SLe:
        case Kind::SGt:
        case Kind::SGe:
        case Kind::ULt:
        case Kind::ULe:
        case Kind::UGt:
        case Kind::UGe: {
            auto* b = cast<BinaryInst>(this);
            co_yield &b->left;
            co_yield &b->right;
        } break;
    }
}

void Inst::EraseImpl() {
    /// Clear usees.
    for (auto* usee : children()) RemoveUse(usee, this);

    /// Erase this instruction.
    if (parent) parent->instructions().erase(rgs::find(parent->instructions(), this));
}

auto Inst::children() const -> Generator<Value*> {
    /// const_cast is fine since this does not mutate the instruction.
    auto* self = const_cast<Inst*>(this);
    for (auto* v : self->Children()) co_yield *v;

    /// Include blocks if there are any.
    if (auto* br = cast<BranchInst>(self)) co_yield br->target();
    else if (auto* cond_br = cast<CondBranchInst>(self)) {
        co_yield cond_br->then_block();
        co_yield cond_br->else_block();
    }
}

void Inst::erase() {
    LCC_ASSERT(users().empty(), "Cannot remove used instruction");
    EraseImpl();
}

void Inst::erase_cascade() {
    EraseImpl();
    while (not users().empty()) users().front()->erase_cascade();
}

auto Inst::instructions_before_this() -> std::span<Inst*> {
    if (not parent) return {};
    auto it = rgs::find(parent->instructions(), this);
    LCC_ASSERT(it != parent->instructions().end(), "Instruction not found in parent");
    return {parent->instructions().begin(), it};
}

void Inst::replace_with(Value* v) {
    LCC_ASSERT(v, "Cannot replace instruction with null Value");
    while (not users().empty()) {
        auto* u = users().front();

        /// Using `Children()` is fine here since we are not a block.
        for (auto* use : u->Children()) {
            if (*use == this) {
                RemoveUse(this, u);
                AddUse(v, u);
                *use = v;
            }
        }
    }

    /// If v is an instruction and not inserted in a block, insert it now.
    if (
        auto* inst = cast<Inst>(v);
        inst and parent and not inst->parent
    ) {
        auto* block = cast<Block>(parent);
        block->insert_before(inst, this);
    }

    erase();
}

auto Block::create_phi(Type* type, Location loc) -> PhiInst* {
    LCC_ASSERT(type);
    auto phi = new (*parent->module()) PhiInst(type, loc);
    auto it = rgs::find_if(inst_list, [](Inst* i) { return not is<PhiInst>(i); });
    inst_list.insert(it, phi);
    phi->parent = this;
    return phi;
}

void Block::erase() {
    LCC_ASSERT(users().empty(), "Cannot remove used block");

    /// Erase all instructions in this block.
    while (not inst_list.empty()) inst_list.back()->erase_cascade();
    auto it = rgs::find(parent->blocks(), this);
    parent->blocks().erase(it);
}

void Block::merge(lcc::Block* b) {
    LCC_ASSERT(b);
    LCC_ASSERT(not closed() or as<BranchInst>(terminator())->target() == b);
    LCC_ASSERT(not parent or not b->parent or parent == b->parent);

    /// Fix PHIs.
    if (parent) {
        for (usz i = 0; i < users().size(); /** No increment! **/) {
            auto phi = cast<PhiInst>(users()[i]);
            if (not phi) {
                i++;
                continue;
            }

            /// Any PHIs in the other block must be replaced with
            /// the value from this block; since the other block
            /// has only us as a predecessor, there must only be
            /// one value in those PHIs.
            if (phi->parent == b) {
                phi->drop_stale_operands();
                LCC_ASSERT(phi->operands().size() <= 1);
                if (auto in = phi->get_incoming(this)) phi->replace_with(in);
                else phi->erase();

                /// Don’t increment since we’ve just removed a user.
                continue;
            }

            /// Any PHIs that use a value from the other block must
            /// be updated to use the value from this block.
            else if (auto in = phi->get_incoming(b)) {
                phi->remove_incoming(b);
                phi->set_incoming(in, this);
                i++;
            }
        }
    }

    /// Erase our terminator only after fixing the PHIs so the call
    /// to drop_stale_operands() doesn’t yeet the one value we care
    /// about.
    if (auto t = terminator()) t->erase();

    /// Set the parent for each instruction to this block and move
    /// them all over. Lastly, delete the block.
    for (auto i : b->inst_list) i->parent = this;
    inst_list.insert(inst_list.end(), b->inst_list.begin(), b->inst_list.end());
    b->inst_list.clear();
    if (b->parent) b->erase();
}

auto lcc::Block::predecessor_count() const -> usz {
    std::unordered_set<Block*> preds;
    for (auto u : users())
        if (is<BranchInst, CondBranchInst>(u) and u->parent)
            preds.insert(u->parent);
    return preds.size();
}

auto lcc::Block::successors() const -> Generator<Block*> {
    if (not closed()) co_return;
    switch (terminator()->kind()) {
        default: break;

        case Kind::Branch:
            co_yield as<BranchInst>(terminator())->target();
            break;

        case Kind::CondBranch:
            auto br = as<CondBranchInst>(terminator());
            co_yield br->then_block();
            if (br->else_block() != br->then_block()) co_yield br->else_block();
            break;
    }
}

auto lcc::Block::successor_count() const -> usz {
    if (not closed()) return 0;
    switch (terminator()->kind()) {
        default: return 0;
        case Kind::Branch: return 1;
        case Kind::CondBranch:
            auto br = as<CondBranchInst>(terminator());
            return br->then_block() == br->else_block() ? 1 : 2;
    }
}

void lcc::Inst::insert_before(Inst* to_insert) {
    LCC_ASSERT(to_insert);
    LCC_ASSERT(block(), "Cannot insert before instruction that has no block reference");
    block()->insert_before(to_insert, this);
}

namespace {
struct LCCIRPrinter : IRPrinter<LCCIRPrinter, 2> {
    using IRPrinter::IRPrinter;

    void PrintHeader(Module* mod) {
        Print("{}; LCC Module '{}'{}\n", C(P::Comment), mod->name(), C(P::Reset));
    }

    std::string Ty(Type* ty) {
        LCC_ASSERT(ty, "Cannot stringify null Type");
        if (auto struct_type = cast<StructType>(ty)) {
            return fmt::format(
                "{}@{}{}",
                C(P::Name),
                struct_type->string(false),
                C(P::Reset)
            );
        } else if (auto arr = cast<ArrayType>(ty)) {
            return fmt::format(
                "{}{}[{}{}{}]{}",
                Ty(arr->element_type()),
                C(P::Filler),
                C(P::Literal),
                arr->length(),
                C(P::Filler),
                C(P::Reset)
            );
        } else if (auto f = cast<FunctionType>(ty)) {
            auto ToString = [&](auto t) { return Ty(t); };
            auto separator = fmt::format("{}, ", C(P::Filler));
            auto variadic = f->variadic() ? fmt::format(" {}variadic", C(P::Filler)) : "";
            return fmt::format(
                "{}{}({}{}){}{}",
                Ty(f->ret()),
                C(P::Filler),
                fmt::join(vws::transform(f->params(), ToString), separator),
                C(P::Filler),
                variadic,
                C(P::Reset)
            );
        }

        return ty->string(_use_colour);
    }

    void PrintStructType(Type* t) {
        LCC_ASSERT(t);
        auto struct_type = as<StructType>(t);
        std::string struct_name = struct_type->string(false);

        Print(
            "{}struct {}{}{} {{",
            C(P::Filler),
            C(P::Name),
            struct_name,
            C(P::Filler)
        );

        if (not struct_type->members().empty()) {
            bool first = true;
            for (auto [i, member] : vws::enumerate(struct_type->members())) {
                if (first) first = false;
                else Print("{},", C(P::Filler));
                Print(" {}", Ty(member));
            }
        }

        Print(" {}}}{}\n", C(P::Filler), C(P::Reset));
    }

    /// Print the function signature.
    void PrintFunctionHeader(Function* f) {
        LCC_ASSERT(f);
        auto ftype = as<FunctionType>(f->type());
        for (auto n : f->names()) {
            if (n.name != f->names().at(0).name)
                Print("{}, ", C(P::Name));
            Print(
                "{}{} {}({})",
                C(P::Name),
                n.name,
                C(P::Filler),
                StringifyEnum(n.linkage)
            );
        }
        Print(
            ": {}{} {}{}(",
            C(P::Filler),
            StringifyEnum(f->call_conv()),
            Ty(ftype->ret()),
            C(P::Filler)
        );

        bool first = true;
        for (auto [i, arg] : vws::enumerate(ftype->params())) {
            if (first) first = false;
            else Print("{}, ", C(P::Filler));
            Print("{} {}%{}", Ty(arg), C(P::Temp), i);
        }

        Print("{})", C(P::Filler));

        if (ftype->variadic()) Print(" {}variadic", C(P::Filler));
    }

    /// Print start/end of function body.
    void EnterFunctionBody(Function*) { Print(":\n"); }
    void ExitFunctionBody(Function*) {}

    /// Print the start of a temporary.
    void PrintTemp(Inst* i) {
        LCC_ASSERT(i);
        Print(
            "    {}%{} {}= {}",
            C(P::Temp),
            Index(i),
            C(P::Filler),
            C(P::Opcode)
        );
    };

    /// Print a cast instruction.
    void PrintCast(Inst* i, std::string_view mnemonic) {
        LCC_ASSERT(i);
        auto* c = as<UnaryInstBase>(i);
        PrintTemp(i);
        Print(
            "{} {} {}to {}",
            mnemonic,
            Val(c->operand(), true),
            C(P::Filler),
            Ty(c->type())
        );
    }

    /// Print a binary instruction.
    void PrintBinary(Inst* i, std::string_view mnemonic) {
        LCC_ASSERT(i);
        auto* b = as<BinaryInst>(i);
        PrintTemp(i);
        Print(
            "{} {}{}, {}",
            mnemonic,
            Val(b->lhs(), true),
            C(P::Filler),
            Val(b->rhs(), false)
        );
    };

    /// Print an instruction.
    void PrintInst(Inst* i) {
        LCC_ASSERT(i);
        switch (i->kind()) {
            /// Not an instruction.
            case Value::Kind::Block:
            case Value::Kind::Function:
            case Value::Kind::IntegerConstant:
            case Value::Kind::FractionalConstant:
            case Value::Kind::ArrayConstant:
            case Value::Kind::Poison:
            case Value::Kind::GlobalVariable:
            case Value::Kind::Parameter:
                LCC_UNREACHABLE();

            /// Instructions.
            case Value::Kind::Alloca: {
                auto* local = as<AllocaInst>(i);
                PrintTemp(i);
                Print("alloca {}", Ty(local->allocated_type()));
                return;
            }

            case Value::Kind::Copy: {
                auto* copy = as<CopyInst>(i);
                PrintTemp(i);
                Print("copy {}", Val(copy->operand()));
                return;
            }

            case Value::Kind::Call: {
                auto* c = as<CallInst>(i);
                auto* callee_ty = as<FunctionType>(c->callee()->type());
                if (not callee_ty->ret()->is_void()) PrintTemp(i);
                else Print("    {}", C(P::Opcode));
                Print(
                    "{}call{}{} {} {}(",
                    c->is_tail_call() ? "tail "sv : ""sv,
                    c->call_conv() == CallConv::C ? ""sv : " "sv,
                    c->call_conv() == CallConv::C ? ""sv : StringifyEnum(c->call_conv()),
                    Val(c->callee(), false),
                    C(P::Filler)
                );

                bool first = true;
                for (auto* arg : c->args()) {
                    if (first) first = false;
                    else Print("{}, ", C(P::Filler));
                    Print("{}", Val(arg));
                }

                auto variadic = callee_ty->variadic() ? fmt::format(" {}variadic", C(P::Filler)) : "";

                if (callee_ty->ret()->is_void()) Print("{}){}", C(P::Filler), variadic);
                else Print("{}){} -> {}", C(P::Filler), variadic, Ty(callee_ty->ret()));
                return;
            }

            case Value::Kind::GetElementPtr: {
                auto* gep = as<GEPInst>(i);
                PrintTemp(i);
                Print(
                    "gep {} {}from {} {}at {}",
                    Ty(gep->base_type()),
                    C(P::Filler),
                    Val(gep->ptr(), false),
                    C(P::Filler),
                    Val(gep->idx())
                );
                return;
            }

            case Value::Kind::GetMemberPtr: {
                auto* gmp = as<GetMemberPtrInst>(i);
                PrintTemp(i);
                Print(
                    "gmp {} {}from {} {}at {}",
                    Ty(gmp->struct_type()),
                    C(P::Filler),
                    Val(gmp->ptr(), false),
                    C(P::Filler),
                    Val(gmp->idx())
                );
                return;
            }

            case Value::Kind::Intrinsic: {
                auto* intrinsic = as<IntrinsicInst>(i);
                auto operands = intrinsic->operands();
                switch (intrinsic->intrinsic_kind()) {
                    default: LCC_ASSERT(false, "Unimplemented intrinsic in LCC IR printer");

                    case IntrinsicKind::MemCopy: {
                        Print(
                            "    {}intrinsic {}@memcpy{}({}{}, {}{}, {}{})",
                            C(P::Opcode),
                            C(P::Name),
                            C(P::Filler),
                            Val(operands[0]),
                            C(P::Filler),
                            Val(operands[1]),
                            C(P::Filler),
                            Val(operands[2]),
                            C(P::Filler)
                        );
                        return;
                    }
                }
            }

            case Value::Kind::Load: {
                auto* load = as<LoadInst>(i);
                PrintTemp(i);
                Print(
                    "load {} {}from {}",
                    Ty(load->type()),
                    C(P::Filler),
                    Val(load->ptr(), false)
                );
                return;
            }

            case Value::Kind::Phi: {
                auto* phi = as<PhiInst>(i);
                const auto separator = fmt::format("{}, ", C(P::Filler));
                const auto FormatPHIVal = [this](auto& val) {
                    return fmt::format(
                        "{}[{} {}: {}{}]",
                        C(P::Filler),
                        Val(val.block, false),
                        C(P::Filler),
                        Val(val.value, false),
                        C(P::Filler)
                    );
                };

                PrintTemp(i);
                Print(
                    "phi {}{}, {}",
                    Ty(phi->type()),
                    C(P::Filler),
                    fmt::join(vws::transform(phi->operands(), FormatPHIVal), separator)
                );
                return;
            }

            case Value::Kind::Store: {
                auto* store = as<StoreInst>(i);
                Print(
                    "    {}store {} {}into {}",
                    C(P::Opcode),
                    Val(store->val()),
                    C(P::Filler),
                    Val(store->ptr(), false)
                );
                return;
            }

            /// Terminators.
            case Value::Kind::Branch: {
                auto* branch = as<BranchInst>(i);
                Print(
                    "    {}branch {}to {}",
                    C(P::Opcode),
                    C(P::Filler),
                    Val(branch->target(), false)
                );
                return;
            }

            case Value::Kind::CondBranch: {
                auto* branch = as<CondBranchInst>(i);
                Print(
                    "    {}branch {}on {} {}to {} {}else {}",
                    C(P::Opcode),
                    C(P::Filler),
                    Val(branch->cond(), false),
                    C(P::Filler),
                    Val(branch->then_block(), false),
                    C(P::Filler),
                    Val(branch->else_block(), false)
                );
                return;
            }

            case Value::Kind::Return: {
                auto* ret = as<ReturnInst>(i);
                if (ret->val()) Print("    {}return {}", C(P::Opcode), Val(ret->val()));
                else Print("    {}return", C(P::Opcode));
                return;
            }

            case Value::Kind::Unreachable: {
                Print("    {}unreachable", C(P::Opcode));
                return;
            }

            /// Unary instructions.
            case Value::Kind::Bitcast: PrintCast(i, "bitcast"); return;
            case Value::Kind::ZExt: PrintCast(i, "zext"); return;
            case Value::Kind::SExt: PrintCast(i, "sext"); return;
            case Value::Kind::Trunc: PrintCast(i, "trunc"); return;

            case Value::Kind::Neg: {
                auto* neg = as<UnaryInstBase>(i);
                PrintTemp(i);
                Print("negate {}", Index(i), Val(neg->operand()));
                return;
            }

            case Value::Kind::Compl: {
                auto* c = as<UnaryInstBase>(i);
                PrintTemp(i);
                Print("compl {}", Index(i), Val(c->operand()));
                return;
            }

            /// Binary instructions.
            case Value::Kind::Add: PrintBinary(i, "add"); return;
            case Value::Kind::Sub: PrintBinary(i, "sub"); return;
            case Value::Kind::Mul: PrintBinary(i, "mul"); return;
            case Value::Kind::SDiv: PrintBinary(i, "sdiv"); return;
            case Value::Kind::UDiv: PrintBinary(i, "udiv"); return;
            case Value::Kind::SRem: PrintBinary(i, "srem"); return;
            case Value::Kind::URem: PrintBinary(i, "urem"); return;
            case Value::Kind::Shl: PrintBinary(i, "shl"); return;
            case Value::Kind::Sar: PrintBinary(i, "sar"); return;
            case Value::Kind::Shr: PrintBinary(i, "shr"); return;
            case Value::Kind::And: PrintBinary(i, "and"); return;
            case Value::Kind::Or: PrintBinary(i, "or"); return;
            case Value::Kind::Xor: PrintBinary(i, "xor"); return;
            case Value::Kind::Eq: PrintBinary(i, "eq"); return;
            case Value::Kind::Ne: PrintBinary(i, "ne"); return;
            case Value::Kind::SLt: PrintBinary(i, "slt"); return;
            case Value::Kind::SLe: PrintBinary(i, "sle"); return;
            case Value::Kind::SGt: PrintBinary(i, "sgt"); return;
            case Value::Kind::SGe: PrintBinary(i, "sge"); return;
            case Value::Kind::ULt: PrintBinary(i, "ult"); return;
            case Value::Kind::ULe: PrintBinary(i, "ule"); return;
            case Value::Kind::UGt: PrintBinary(i, "ugt"); return;
            case Value::Kind::UGe: PrintBinary(i, "uge"); return;
        }

        LCC_UNREACHABLE();
    }

    /// Print a global variable.
    void PrintGlobal(GlobalVariable* v) {
        LCC_ASSERT(v);
        bool imported{false};
        for (const auto& n : v->names()) {
            if (n.name != v->names().front().name)
                Print("{}, ", C(P::Name));
            Print(
                "{}{}",
                C(P::Temp),
                n.name
            );
            if (IsImportedLinkage(n.linkage))
                imported = true;
        }
        Print(
            " {}: {} {}",
            C(P::Filler),
            Ty(v->allocated_type()),
            C(P::Filler)
        );

        if (imported) {
            Print("external\n");
            return;
        }

        if (v->init()) Print("= {}\n", Val(v->init(), false));
        else Print("= {}0\n", C(P::Literal));
    }

    /// Get the inline representation of a value.
    ///
    /// In some contexts, the type is obvious (e.g. the address
    /// of a store is always of type \c ptr), so there is no reason
    /// to include it in the printout. The type is omitted if \c false
    /// is passed for \c include_type.
    auto Val(Value* v, bool include_type = true) -> std::string {
        LCC_ASSERT(v);

        const auto Format =
            [&]<typename... Args>(
                fmt::format_string<Args...> fmt,
                Args&&... args
            ) -> std::string {
            std::string val;
            if (include_type)
                fmt::format_to(It(val), "{} ", Ty(v->type()));
            fmt::format_to(It(val), fmt, std::forward<Args>(args)...);
            return val;
        };

        switch (v->kind()) {
            case Value::Kind::Function:
                return Format("{}@{}", C(P::Name), as<Function>(v)->names().at(0).name);

            case Value::Kind::IntegerConstant:
                return Format("{}{}", C(P::Literal), as<IntegerConstant>(v)->value());

            case Value::Kind::FractionalConstant: {
                auto f = as<FractionalConstant>(v)->value();
                return Format(
                    "{}{}.{}",
                    C(P::Literal),
                    f.whole,
                    fractional_to_whole(f.fractional)
                );
            }

            case Value::Kind::Poison:
                return Format("{}poison", C(P::Filler));

            case Value::Kind::Parameter:
                return Format("{}%{}", C(P::Temp), as<Parameter>(v)->index());

            case Value::Kind::Block: {
                return fmt::format(
                    "{}{}{}%bb{}",
                    C(P::Type),
                    include_type ? "block " : "",
                    C(P::Block),
                    Index(as<Block>(v))
                );
            }

            case Value::Kind::GlobalVariable: {
                std::string val;
                if (include_type) fmt::format_to(It(val), "{}ptr ", C(P::Type));
                fmt::format_to(
                    It(val),
                    "{}@{}",
                    C(P::Temp),
                    as<GlobalVariable>(v)->names().at(0).name
                );
                return val;
            }

            /// TODO: Format this differently based on the array type?
            case Value::Kind::ArrayConstant: {
                auto* a = as<ArrayConstant>(v);

                // String
                if (a->is_string_literal()) {
                    static const auto FormatChar = [](u8 c) {
                        return std::isprint(c) and c != '\"'
                                 ? std::string{char(c)}
                                 : fmt::format("\\{:02X}", u8(c));
                    };

                    return Format(
                        "{}\"{}\"{}",
                        C(P::Literal),
                        fmt::join(
                            std::span<const char>(a->data(), a->size()) | vws::transform(FormatChar),
                            ""
                        ),
                        C(P::Reset)
                    );
                }

                // Array
                return Format(
                    "{}[{}{}{}]",
                    C(P::Filler),
                    C(P::Literal),
                    fmt::join(
                        std::span<const u8>(
                            reinterpret_cast<const u8*>(a->data()),
                            a->size()
                        ),
                        " "
                    ),
                    C(P::Reset)
                );
            }

            case Value::Kind::Intrinsic: {
                auto* intrinsic = as<IntrinsicInst>(v);
                switch (intrinsic->intrinsic_kind()) {
                    case IntrinsicKind::DebugTrap:
                        return Format("intrinsic.debug_trap");

                    case IntrinsicKind::MemCopy:
                        return Format("intrinsic.memcpy");

                    case IntrinsicKind::MemSet:
                        return Format("intrinsic.memset");

                    case IntrinsicKind::SystemCall:
                        return Format("intrinsic.syscall");
                }
                LCC_UNREACHABLE();
            }

            /// These always yield a value.
            case Value::Kind::Copy:
            case Value::Kind::Alloca:
            case Value::Kind::GetElementPtr:
            case Value::Kind::GetMemberPtr:
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
                return Format("{}%{}", C(P::Temp), Index(as<Inst>(v)));

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
            case Value::Kind::FractionalConstant:
            case Value::Kind::ArrayConstant:
            case Value::Kind::Poison:
            case Value::Kind::GlobalVariable:
            case Value::Kind::Parameter:
                LCC_UNREACHABLE();

            /// Instructions that always yield a value.
            case Value::Kind::Copy:
            case Value::Kind::Alloca:
            case Value::Kind::GetElementPtr:
            case Value::Kind::GetMemberPtr:
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
            case Value::Kind::Intrinsic: {
                auto* intrinsic = as<IntrinsicInst>(i);
                switch (intrinsic->intrinsic_kind()) {
                    default: LCC_UNREACHABLE();

                    case IntrinsicKind::MemCopy:
                    case IntrinsicKind::MemSet: return false;

                    case IntrinsicKind::DebugTrap: return false;
                    case IntrinsicKind::SystemCall: return true; // ??
                }
            }

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

    /// Print an individual value.
    ///
    /// This is *not* to be used when printing the rest of
    /// the IR! This is the entry point for printing a single
    /// value only.
    static void PrintValue(const Value* const_value, bool use_colour) {
        LCC_ASSERT(const_value);

        /// Ok because we’re not going to mutate this, but we should
        /// probably refactor the IR printer to use const Value*’s
        /// instead...
        auto* v = const_cast<Value*>(const_value);
        LCCIRPrinter p{use_colour};
        if (auto* b = cast<Block>(v)) {
            if (b->function()) p.SetFunctionIndices(b->function());
            p.PrintBlock(b);
        } else if (auto* f = cast<Function>(v)) {
            p.SetFunctionIndices(f);
            p.PrintFunction(f);
        } else if (auto* i = cast<Inst>(v)) {
            if (i->block() and i->block()->function()) p.SetFunctionIndices(i->block()->function());
            p.PrintInst(i);
        } else if (is<GlobalVariable>(v)) {
            p.PrintGlobal(as<GlobalVariable>(v));
        } else {
            p.Print("{}", p.Val(v, true));
        }
        fmt::print(
            "{}{}",
            p.Output(),
            lcc::utils::Colours{use_colour}(lcc::utils::Colour::Reset)
        );
    }
};
} // namespace

auto Module::as_lcc_ir(bool use_colour) -> std::string {
    return fmt::format(
        "{}{}",
        LCCIRPrinter::Print(this, use_colour),
        lcc::utils::Colours{use_colour}(lcc::utils::Colour::Reset)
    );
}

void Value::print() const {
    LCCIRPrinter::PrintValue(this, true);
}

} // namespace lcc
