#include <intercept/ast.hh>
#include <lcc/context.hh>
#include <lcc/ir/type.hh>

void lcc::Context::InitialiseLCCData() {
    /// Initialise builtin IR types.
    static Type ir_unknown_ty{Type::Kind::Unknown};
    static Type ir_ptr_ty{Type::Kind::Pointer};
    static Type ir_void_ty{Type::Kind::Void};
    static IntegerType ir_i1_ty{1};

    Type::UnknownTy = &ir_unknown_ty;
    Type::PtrTy = &ir_ptr_ty;
    Type::VoidTy = &ir_void_ty;
    Type::I1Ty = &ir_i1_ty;

    /// Initialise default instances of builtin Intercept types.
    static intercept::Type default_type_instances[2]{
        intercept::Type{intercept::Type::Kind::Builtin, {}},
        intercept::Type{intercept::Type::Kind::Builtin, {}},
    };

    intercept::Type::Unknown = default_type_instances;
    intercept::Type::Integer = default_type_instances + 1;
}
