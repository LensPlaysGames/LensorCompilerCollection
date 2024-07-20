#include <glint/ast.hh>
#include <intercept/ast.hh>
#include <lcc/context.hh>
#include <lcc/ir/type.hh>
#include <lcc/target.hh>

namespace lcc {

// THE VALUES WHICH THE TYPE POINTERS POINT TO ARE CREATED HERE
class Init {
public:
    static consteval auto ir_unknown() noexcept {
        return Type{Type::Kind::Unknown};
    }
    static consteval auto ir_ptr() noexcept {
        return Type{Type::Kind::Pointer};
    }
    static consteval auto ir_void() noexcept {
        return Type{Type::Kind::Void};
    }
    static consteval auto ir_i1() noexcept {
        return IntegerType{1};
    }

    static consteval auto intercept_unknown() noexcept {
        return intercept::BuiltinType{intercept::BuiltinType::K::Unknown, {}};
    }
    static consteval auto intercept_bool() noexcept {
        return intercept::BuiltinType{intercept::BuiltinType::K::Bool, {}};
    }
    static consteval auto intercept_byte() noexcept {
        return intercept::BuiltinType{intercept::BuiltinType::K::Byte, {}};
    }
    static consteval auto intercept_int() noexcept {
        return intercept::BuiltinType{intercept::BuiltinType::K::Int, {}};
    }
    static consteval auto intercept_overload_set() noexcept {
        return intercept::BuiltinType{intercept::BuiltinType::K::OverloadSet, {}};
    }
    static consteval auto intercept_void() noexcept {
        return intercept::BuiltinType{intercept::BuiltinType::K::Void, {}};
    }
    static consteval auto intercept_void_ptr(intercept::Type* void_ty) noexcept {
        auto t = intercept::PointerType{void_ty};
        t.set_sema_done();
        return t;
    }

    static consteval auto glint_unknown() noexcept {
        return glint::BuiltinType{glint::BuiltinType::K::Unknown, {}};
    }
    static consteval auto glint_bool() noexcept {
        return glint::BuiltinType{glint::BuiltinType::K::Bool, {}};
    }
    static consteval auto glint_byte() noexcept {
        return glint::BuiltinType{glint::BuiltinType::K::Byte, {}};
    }
    static consteval auto glint_int() noexcept {
        return glint::BuiltinType{glint::BuiltinType::K::Int, {}};
    }
    static consteval auto glint_uint() noexcept {
        return glint::BuiltinType{glint::BuiltinType::K::UInt, {}};
    }
    static consteval auto glint_overload_set() noexcept {
        return glint::BuiltinType{glint::BuiltinType::K::OverloadSet, {}};
    }
    static consteval auto glint_void() noexcept {
        return glint::BuiltinType{glint::BuiltinType::K::Void, {}};
    }
    // NOTE: If you pass in anything that isn't glint_void_ty, you are going
    // to have a very bad time. We do this so that we don't require a
    // forward declaration.
    static consteval auto glint_void_ptr(glint::Type* void_ty) noexcept {
        auto t = glint::PointerType{void_ty};
        t.set_sema_done();
        return t;
    }
};
} // namespace lcc

// THE TEMPORARIES WHICH THE POINTERS POINT TO ARE CREATED HERE
namespace {
// LCC IR TYPES
constinit auto ir_unknown_ty = lcc::Init::ir_unknown();
constinit auto ir_ptr_ty = lcc::Init::ir_ptr();
constinit auto ir_void_ty = lcc::Init::ir_void();
constinit auto ir_i1_ty = lcc::Init::ir_i1();

/// Initialise default instances of builtin Intercept types.
constinit auto intercept_bool_ty = lcc::Init::intercept_bool();
constinit auto intercept_byte_ty = lcc::Init::intercept_byte();
constinit auto intercept_int_ty = lcc::Init::intercept_int();
constinit auto intercept_unknown_ty = lcc::Init::intercept_unknown();
constinit auto intercept_void_ty = lcc::Init::intercept_void();
constinit auto intercept_overload_set_ty = lcc::Init::intercept_overload_set();
constinit auto intercept_void_ptr_ty = lcc::Init::intercept_void_ptr(&intercept_void_ty);

// GLINT TYPES
constinit auto glint_unknown_ty = lcc::Init::glint_unknown();
constinit auto glint_bool_ty = lcc::Init::glint_bool();
constinit auto glint_byte_ty = lcc::Init::glint_byte();
constinit auto glint_int_ty = lcc::Init::glint_int();
constinit auto glint_uint_ty = lcc::Init::glint_uint();
constinit auto glint_overload_set_ty = lcc::Init::glint_overload_set();
constinit auto glint_void_ty = lcc::Init::glint_void();
constinit auto glint_void_ptr_ty = lcc::Init::glint_void_ptr(&glint_void_ty);

} // namespace

// THIS IS WHERE THE ACTUAL STATIC MEMBERS ARE DEFINED

// Initialise builtin and/or common IR types.
constinit lcc::Type* lcc::Type::UnknownTy = &ir_unknown_ty;
constinit lcc::Type* lcc::Type::PtrTy = &ir_ptr_ty;
constinit lcc::Type* lcc::Type::VoidTy = &ir_void_ty;
constinit lcc::Type* lcc::Type::I1Ty = &ir_i1_ty;

// Initialise builtin and/or common Intercept types.
constinit lcc::intercept::Type* lcc::intercept::Type::Bool = &intercept_bool_ty;
constinit lcc::intercept::Type* lcc::intercept::Type::Byte = &intercept_byte_ty;
constinit lcc::intercept::Type* lcc::intercept::Type::Int = &intercept_int_ty;
constinit lcc::intercept::Type* lcc::intercept::Type::Unknown = &intercept_unknown_ty;
constinit lcc::intercept::Type* lcc::intercept::Type::Void = &intercept_void_ty;
constinit lcc::intercept::Type* lcc::intercept::Type::VoidPtr = &intercept_void_ptr_ty;
constinit lcc::intercept::Type* lcc::intercept::Type::OverloadSet = &intercept_overload_set_ty;

// Initialise builtin and/or common Glint types
constinit lcc::glint::Type* lcc::glint::Type::Unknown = &glint_unknown_ty;
constinit lcc::glint::Type* lcc::glint::Type::Bool = &glint_bool_ty;
constinit lcc::glint::Type* lcc::glint::Type::Byte = &glint_byte_ty;
constinit lcc::glint::Type* lcc::glint::Type::Int = &glint_int_ty;
constinit lcc::glint::Type* lcc::glint::Type::UInt = &glint_uint_ty;
constinit lcc::glint::Type* lcc::glint::Type::OverloadSet = &glint_overload_set_ty;
constinit lcc::glint::Type* lcc::glint::Type::Void = &glint_void_ty;
constinit lcc::glint::Type* lcc::glint::Type::VoidPtr = &glint_void_ptr_ty;

void lcc::Context::InitialiseLCCData() {
    // no-op (and we'd like to keep it this way, if possible)
}
