#include <intercept/ast.hh>
#include <laye/ast.hh>
#include <lcc/context.hh>
#include <lcc/ir/type.hh>
#include <lcc/target.hh>

namespace intc = lcc::intercept;
namespace layec = lcc::laye;

/// FIXME(Sirraide): We could probably just use `constinit` for all of this;
///     that way, we wouldn’t even need to call an initialisation function.
///     Note that `constexpr` probably wouldn’t work for types since we’re
///     passing around `Type*`s instead of `const Type*`s in way too many
///     places.

lcc::Type* lcc::Type::UnknownTy;
lcc::Type* lcc::Type::PtrTy;
lcc::Type* lcc::Type::VoidTy;
lcc::Type* lcc::Type::I1Ty;

intc::Type* intc::Type::Bool;
intc::Type* intc::Type::Byte;
intc::Type* intc::Type::Int;
intc::Type* intc::Type::Unknown;
intc::Type* intc::Type::Void;
intc::Type* intc::Type::VoidPtr;
intc::Type* intc::Type::OverloadSet;

layec::Type* layec::Type::Bool;
layec::Type* layec::Type::Int;
layec::Type* layec::Type::UInt;
layec::Type* layec::Type::OverloadSet;

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
    static constinit intc::BuiltinType int_ty_bool = {intc::BuiltinType::BuiltinKind::Bool, {}};
    static constinit intc::BuiltinType int_ty_byte = {intc::BuiltinType::BuiltinKind::Byte, {}};
    static constinit intc::BuiltinType int_ty_int = {intc::BuiltinType::BuiltinKind::Int, {}};
    static constinit intc::BuiltinType int_ty_unknown = {intc::BuiltinType::BuiltinKind::Unknown, {}};
    static constinit intc::BuiltinType int_ty_void = {intc::BuiltinType::BuiltinKind::Void, {}};
    static constinit intc::BuiltinType int_ty_os = {intc::BuiltinType::BuiltinKind::OverloadSet, {}};
    static constinit intc::PointerType int_ty_void_ptr = [] {
        intc::PointerType ty = {&int_ty_void, {}};
        ty.set_sema_done();
        return ty;
    }();

    intc::Type::Bool = &int_ty_bool;
    intc::Type::Byte = &int_ty_byte;
    intc::Type::Int = &int_ty_int;
    intc::Type::Unknown = &int_ty_unknown;
    intc::Type::Void = &int_ty_void;
    intc::Type::OverloadSet = &int_ty_os;
    intc::Type::VoidPtr = &int_ty_void_ptr;

    /// Initialise default instances of builtin Laye types.
    static constinit layec::BoolType laye_ty_bool = {{}, 0, true};
    static constinit layec::IntType laye_ty_int = {{}, true, 0, true};
    static constinit layec::IntType laye_ty_uint = {{}, false, 0, true};
    static constinit layec::OverloadSetType laye_ty_os = {{}};

    layec::Type::Bool = &laye_ty_bool;
    layec::Type::Int = &laye_ty_int;
    layec::Type::UInt = &laye_ty_uint;
    layec::Type::OverloadSet = &laye_ty_os;
}
