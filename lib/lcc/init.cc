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
};
} // namespace lcc

// THE TEMPORARIES WHICH THE POINTERS POINT TO ARE CREATED HERE
namespace {
// LCC IR TYPES
constinit auto ir_unknown_ty = lcc::Init::ir_unknown();
constinit auto ir_ptr_ty = lcc::Init::ir_ptr();
constinit auto ir_void_ty = lcc::Init::ir_void();
constinit auto ir_i1_ty = lcc::Init::ir_i1();

} // namespace

// THIS IS WHERE THE ACTUAL STATIC MEMBERS ARE DEFINED

// Initialise builtin and/or common IR types.
constinit lcc::Type* lcc::Type::UnknownTy = &ir_unknown_ty;
constinit lcc::Type* lcc::Type::PtrTy = &ir_ptr_ty;
constinit lcc::Type* lcc::Type::VoidTy = &ir_void_ty;
constinit lcc::Type* lcc::Type::I1Ty = &ir_i1_ty;

void lcc::Context::InitialiseLCCData() {
    // no-op (and we'd like to keep it this way, if possible)
}
