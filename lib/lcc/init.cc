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
    static intercept::BuiltinType int_ty_bool = {intercept::BuiltinType::BuiltinKind::Bool, {}};
    static intercept::BuiltinType int_ty_byte = {intercept::BuiltinType::BuiltinKind::Byte, {}};
    static intercept::BuiltinType int_ty_int = {intercept::BuiltinType::BuiltinKind::Integer, {}};
    static intercept::BuiltinType int_ty_unknown = {intercept::BuiltinType::BuiltinKind::Unknown, {}};
    static intercept::BuiltinType int_ty_void = {intercept::BuiltinType::BuiltinKind::Void, {}};
    static intercept::PointerType int_ty_void_ptr = {&int_ty_void, {}};
    static intercept::BuiltinType int_ty_os = {intercept::BuiltinType::BuiltinKind::OverloadSet, {}};

    int_ty_bool.set_sema_done();
    int_ty_byte.set_sema_done();
    int_ty_int.set_sema_done();
    int_ty_unknown.set_sema_done();
    int_ty_void.set_sema_done();
    int_ty_void_ptr.set_sema_done();
    int_ty_os.set_sema_done();

    intercept::Type::Bool = &int_ty_bool;
    intercept::Type::Byte = &int_ty_byte;
    intercept::Type::Integer = &int_ty_int;
    intercept::Type::Unknown = &int_ty_unknown;
    intercept::Type::Void = &int_ty_void;
    intercept::Type::VoidPtr = &int_ty_void_ptr;
    intercept::Type::OverloadSet = &int_ty_os;
}
