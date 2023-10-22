#include <lcc/codegen/gnu_as_att_assembly.hh>

#include <lcc/context.hh>
#include <lcc/diags.hh>
#include <lcc/ir/ir.hh>
#include <lcc/ir/module.hh>
#include <lcc/target.hh>
#include <lcc/format.hh>

namespace lcc {
// file-local
static void emit_instruction(Context *ctx, Module *mod, Inst* inst) {
    switch (inst->kind()) {
        case Value::Kind::Block:
        case Value::Kind::Function:
        case Value::Kind::IntegerConstant:
        case Value::Kind::ArrayConstant:
        case Value::Kind::Poison:
        case Value::Kind::GlobalVariable:
        case Value::Kind::Parameter:
        case Value::Kind::Alloca:
        case Value::Kind::Call:
        case Value::Kind::GetElementPtr:
        case Value::Kind::Intrinsic:
        case Value::Kind::Load:
        case Value::Kind::Phi:
        case Value::Kind::Store:
        case Value::Kind::Branch:
        case Value::Kind::CondBranch:
        case Value::Kind::Return:
        case Value::Kind::Unreachable:
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
    }
}

void emit_gnu_as_att(Context* ctx, Module* mod) {
    LCC_ASSERT(ctx->format() == Format::gnu_as_att_assembly);

    if (ctx->target() == Target::x86_64_windows) {
    } else if (ctx->target() == Target::x86_64_linux) {
    } else Diag::ICE("Unhandled target in GNU as AT&T assembly backend");

    for (auto function : mod->code()) {
        for (auto block : function->blocks()) {
            for (auto inst : block->instructions()) {
                emit_instruction(ctx, mod, inst);
            }
        }
    }
}
} // namespace lcc
