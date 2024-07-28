#include <glint/ast.hh>

namespace lcc::glint {

// THE VALUES WHICH THE TYPE POINTERS POINT TO ARE CREATED HERE

class Init {
public:
    static consteval auto glint_unknown() noexcept {
        return BuiltinType{BuiltinType::K::Unknown, {}};
    }
    static consteval auto glint_bool() noexcept {
        return BuiltinType{BuiltinType::K::Bool, {}};
    }
    static consteval auto glint_byte() noexcept {
        return BuiltinType{BuiltinType::K::Byte, {}};
    }
    static consteval auto glint_int() noexcept {
        return BuiltinType{BuiltinType::K::Int, {}};
    }
    static consteval auto glint_uint() noexcept {
        return BuiltinType{BuiltinType::K::UInt, {}};
    }
    static consteval auto glint_overload_set() noexcept {
        return BuiltinType{BuiltinType::K::OverloadSet, {}};
    }
    static consteval auto glint_void() noexcept {
        return BuiltinType{BuiltinType::K::Void, {}};
    }
    // NOTE: If you pass in anything that isn't glint_void_ty, you are going
    // to have a very bad time. We do this so that we don't require a
    // forward declaration.
    static consteval auto glint_void_ptr(Type* const void_ty) noexcept {
        auto t = PointerType{void_ty};
        t.set_sema_done();
        return t;
    }
};

// THE TEMPORARIES WHICH THE POINTERS POINT TO ARE CREATED HERE

// GLINT TYPES
constinit auto glint_unknown_ty = Init::glint_unknown();
constinit auto glint_bool_ty = Init::glint_bool();
constinit auto glint_byte_ty = Init::glint_byte();
constinit auto glint_int_ty = Init::glint_int();
constinit auto glint_uint_ty = Init::glint_uint();
constinit auto glint_overload_set_ty = Init::glint_overload_set();
constinit auto glint_void_ty = Init::glint_void();
constinit auto glint_void_ptr_ty = Init::glint_void_ptr(&glint_void_ty);

// THIS IS WHERE THE ACTUAL STATIC MEMBERS ARE DEFINED

// Initialise builtin and/or common Glint types
constinit Type* const Type::Unknown = &glint_unknown_ty;
constinit Type* const Type::Bool = &glint_bool_ty;
constinit Type* const Type::Byte = &glint_byte_ty;
constinit Type* const Type::Int = &glint_int_ty;
constinit Type* const Type::UInt = &glint_uint_ty;
constinit Type* const Type::OverloadSet = &glint_overload_set_ty;
constinit Type* const Type::Void = &glint_void_ty;
constinit Type* const Type::VoidPtr = &glint_void_ptr_ty;

} // namespace lcc::glint
