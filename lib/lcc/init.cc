#include <glint/ast.hh>
#include <intercept/ast.hh>
#include <laye/ast.hh>
#include <lcc/context.hh>
#include <lcc/ir/type.hh>
#include <lcc/target.hh>

lcc::Type* lcc::Type::UnknownTy;
lcc::Type* lcc::Type::PtrTy;
lcc::Type* lcc::Type::VoidTy;
lcc::Type* lcc::Type::I1Ty;

lcc::intercept::Type* lcc::intercept::Type::Bool;
lcc::intercept::Type* lcc::intercept::Type::Byte;
lcc::intercept::Type* lcc::intercept::Type::Int;
lcc::intercept::Type* lcc::intercept::Type::Unknown;
lcc::intercept::Type* lcc::intercept::Type::Void;
lcc::intercept::Type* lcc::intercept::Type::VoidPtr;
lcc::intercept::Type* lcc::intercept::Type::OverloadSet;

lcc::glint::Type* lcc::glint::Type::Bool;
lcc::glint::Type* lcc::glint::Type::Byte;
lcc::glint::Type* lcc::glint::Type::Int;
lcc::glint::Type* lcc::glint::Type::UInt;
lcc::glint::Type* lcc::glint::Type::Unknown;
lcc::glint::Type* lcc::glint::Type::Void;
lcc::glint::Type* lcc::glint::Type::VoidPtr;
lcc::glint::Type* lcc::glint::Type::OverloadSet;

lcc::laye::Type* lcc::laye::Type::Bool;
lcc::laye::Type* lcc::laye::Type::Int;
lcc::laye::Type* lcc::laye::Type::UInt;
lcc::laye::Type* lcc::laye::Type::OverloadSet;

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
    static constinit lcc::intercept::BuiltinType int_ty_bool = {
        lcc::intercept::BuiltinType::BuiltinKind::Bool,
        {}};
    static constinit lcc::intercept::BuiltinType int_ty_byte = {
        lcc::intercept::BuiltinType::BuiltinKind::Byte,
        {}};
    static constinit lcc::intercept::BuiltinType int_ty_int = {
        lcc::intercept::BuiltinType::BuiltinKind::Int,
        {}};
    static constinit lcc::intercept::BuiltinType int_ty_unknown = {
        lcc::intercept::BuiltinType::BuiltinKind::Unknown,
        {}};
    static constinit lcc::intercept::BuiltinType int_ty_void = {
        lcc::intercept::BuiltinType::BuiltinKind::Void,
        {}};
    static constinit lcc::intercept::BuiltinType int_ty_os = {
        lcc::intercept::BuiltinType::BuiltinKind::OverloadSet,
        {}};
    static constinit lcc::intercept::PointerType int_ty_void_ptr = [] {
        lcc::intercept::PointerType ty = {&int_ty_void, {}};
        ty.set_sema_done();
        return ty;
    }();

    lcc::intercept::Type::Bool = &int_ty_bool;
    lcc::intercept::Type::Byte = &int_ty_byte;
    lcc::intercept::Type::Int = &int_ty_int;
    lcc::intercept::Type::Unknown = &int_ty_unknown;
    lcc::intercept::Type::Void = &int_ty_void;
    lcc::intercept::Type::OverloadSet = &int_ty_os;
    lcc::intercept::Type::VoidPtr = &int_ty_void_ptr;

    /// Initialise default instances of builtin Glint types.
    static constinit lcc::glint::BuiltinType glint_ty_bool = {
        lcc::glint::BuiltinType::BuiltinKind::Bool,
        {}};
    static constinit lcc::glint::BuiltinType glint_ty_byte = {
        lcc::glint::BuiltinType::BuiltinKind::Byte,
        {}};
    static constinit lcc::glint::BuiltinType glint_ty_int = {
        lcc::glint::BuiltinType::BuiltinKind::Int,
        {}};
    static constinit lcc::glint::BuiltinType glint_ty_uint = {
        lcc::glint::BuiltinType::BuiltinKind::UInt,
        {}};
    static constinit lcc::glint::BuiltinType glint_ty_unknown = {
        lcc::glint::BuiltinType::BuiltinKind::Unknown,
        {}};
    static constinit lcc::glint::BuiltinType glint_ty_void = {
        lcc::glint::BuiltinType::BuiltinKind::Void,
        {}};
    static constinit lcc::glint::BuiltinType glint_ty_os = {
        lcc::glint::BuiltinType::BuiltinKind::OverloadSet,
        {}};
    static constinit lcc::glint::PointerType glint_ty_void_ptr = [] {
        lcc::glint::PointerType ty = {&glint_ty_void, {}};
        ty.set_sema_done();
        return ty;
    }();
    lcc::glint::Type::Bool = &glint_ty_bool;
    lcc::glint::Type::Byte = &glint_ty_byte;
    lcc::glint::Type::Int = &glint_ty_int;
    lcc::glint::Type::UInt = &glint_ty_int;
    lcc::glint::Type::Unknown = &glint_ty_unknown;
    lcc::glint::Type::Void = &glint_ty_void;
    lcc::glint::Type::OverloadSet = &glint_ty_os;
    lcc::glint::Type::VoidPtr = &glint_ty_void_ptr;

    /// Initialise default instances of builtin Laye types.
    static constinit lcc::laye::BoolType laye_ty_bool = {{}, 0, true};
    static constinit lcc::laye::IntType laye_ty_int = {{}, true, 0, true};
    static constinit lcc::laye::IntType laye_ty_uint = {{}, false, 0, true};
    static constinit lcc::laye::OverloadSetType laye_ty_os = {{}};

    lcc::laye::Type::Bool = &laye_ty_bool;
    lcc::laye::Type::Int = &laye_ty_int;
    lcc::laye::Type::UInt = &laye_ty_uint;
    lcc::laye::Type::OverloadSet = &laye_ty_os;
}
