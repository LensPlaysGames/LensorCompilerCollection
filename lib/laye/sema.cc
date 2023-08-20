#include <laye/sema.hh>

namespace layec = lcc::laye;

void layec::Sema::Analyse(LayeContext* laye_context, Module* module, bool use_colours) {
    LCC_ASSERT(laye_context);

    Sema sema{laye_context, module, use_colours};
    sema.Analyse(module);
}

void layec::Sema::Analyse(Module* module) {
    module->set_sema_in_progress();

    /// Analyse all imports first, since we depend on them in our module.
    for (auto& import : module->imports()) {
        auto imported_module = import.module;
        if (imported_module->sema_state() == SemaState::InProgress) {
            Error(import.location, "Circular dependency detected: cannot import this module");
            imported_module->set_sema_errored();
            continue;
        } else if (imported_module->sema_errored()) {
            imported_module->set_sema_errored();
            continue;
        }

        Analyse(imported_module);
        LCC_ASSERT(imported_module->sema_done_or_errored(), "module analysis did not result in a done or errored module state");
    }

    /// Step 1: Continue to analyse type declarations for as long as we need.
    bool all_module_types_resolved_or_errored = false;
    while (not all_module_types_resolved_or_errored) {
        all_module_types_resolved_or_errored = true;

        int attempted = 0;
        for (auto decl : module->top_level_decls()) {
            /// anything that's already been analysed, errors or not, does not need to be done again.
            if (decl->sema_done_or_errored()) continue;
            /// for now, we do not care about functions or bindings. only types (struct, enum, etc.)
            if (is<FunctionDecl, BindingDecl>(decl)) continue;

            attempted++;

            Analyse(decl);
            LCC_ASSERT(decl->sema_state() != SemaState::NotAnalysed);

            if (not decl->sema_done_or_errored())
                all_module_types_resolved_or_errored = false;
        }

        if (not all_module_types_resolved_or_errored and attempted == 0) {
            Diag::Fatal("Something is wrong in Laye sema, we're heading to infinite type analysis loop");
        }
    }

    LCC_ASSERT(all_module_types_resolved_or_errored, "Failed to analyse module types properly");

    /// Step 2: Analyse function prototypes so we can call functions and have types available.
    for (auto decl : module->top_level_decls()) {
        /// for now, we do not care about functions or bindings. only types (struct, enum, etc.)
        if (auto func_decl = cast<FunctionDecl>(decl)) {
            LCC_ASSERT(func_decl->sema_state() == SemaState::NotAnalysed);
            AnalysePrototype(func_decl);

            LCC_ASSERT(func_decl->return_type()->sema_done_or_errored(), "should have finished function return type analysis");
            for (auto param : func_decl->params()) {
                LCC_ASSERT(param.type->sema_done_or_errored(), "should have finished function param type analysis");
            }
        }
    }

    /// Step 3: Analyse function bodies and global bindings.
    for (auto decl : module->top_level_decls()) {
        /// for now, we do not care about functions or bindings. only types (struct, enum, etc.)
        if (not is<FunctionDecl, BindingDecl>(decl)) continue;

        LCC_ASSERT(decl->sema_state() == SemaState::NotAnalysed);
        Analyse(decl);
        LCC_ASSERT(decl->sema_done_or_errored(), "should have finished function analysis");
    }

    if (not module->sema_errored()) module->set_sema_done();
}

void layec::Sema::AnalysePrototype(FunctionDecl* func) {
    LCC_ASSERT(func->sema_state() == SemaState::NotAnalysed);

    func->return_type()->set_sema_errored();
    for (auto param : func->params()) {
        param.type->set_sema_errored();
    }
}

void layec::Sema::Analyse(Statement* statement) {
    statement->set_sema_in_progress();
    statement->set_sema_errored();
}

bool layec::Sema::Analyse(Expr** expr) {
    (*expr)->set_sema_in_progress();
    (*expr)->set_sema_errored();
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
