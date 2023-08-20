#include <laye/sema.hh>

namespace layec = lcc::laye;

void layec::Sema::Analyse(LayeContext* laye_context, Module* module, bool use_colours) {
    LCC_ASSERT(laye_context);

    Sema sema{laye_context, module, use_colours};
}

void layec::Sema::Analyse(Module* module) {
}

void layec::Sema::Analyse(Statement* statement) {
}

bool layec::Sema::Analyse(Expr** expr) {
    return false;
}

template <bool PerformConversion>
int layec::Sema::ConvertImpl(Expr** expr, Type* type) {
    enum : int {
        TypesContainErrors = -2,
        ConversionImpossible = -1,
        NoOp = 0,
    };

    return ConversionImpossible;
}

bool layec::Sema::Convert(Expr** expr, Type* type) {
    if ((*expr)->sema_errored()) return true;
    return ConvertImpl<true>(expr, type) >= 0;
}

void layec::Sema::ConvertOrError(Expr** expr, Type* to) {
    if (not Convert(expr, to)) Error(
        (*expr)->location(),
        "Expression is not convertible to type {}",
        to->string(use_colours)
    );
}

bool layec::Sema::ConvertToCommonType(Expr** a, Expr** b) {
    return Convert(a, (*b)->type()) or Convert(b, (*a)->type());
}

int layec::Sema::TryConvert(Expr** expr, Type* type) {
    return ConvertImpl<false>(expr, type);
}

void layec::Sema::Discard(Expr** expr) {
    LCC_ASSERT(false);
}

bool layec::Sema::HasSideEffects(Expr* expr) {
    LCC_ASSERT(false);
}

void layec::Sema::InsertImplicitCast(Expr** expr_ptr, Type* ty) {
    LCC_ASSERT(false);
}

void layec::Sema::InsertPointerToIntegerCast(Expr** operand) {
    LCC_ASSERT(false);
}

void layec::Sema::WrapWithCast(Expr** expr, Type* type, CastKind kind) {
    LCC_ASSERT(false);
}

auto layec::Sema::Ptr(Type* type) -> PointerType* {
    LCC_ASSERT(false);
}
