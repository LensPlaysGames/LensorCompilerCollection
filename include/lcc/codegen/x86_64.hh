#ifndef LCC_CODEGEN_X86_64_HH
#define LCC_CODEGEN_X86_64_HH

#include <lcc/utils.hh>
#include <lcc/codegen/mir.hh>

namespace lcc {
namespace x86_64 {

enum struct Opcode : u32 {
    Poison = u32(lcc::MInst::Kind::ArchStart),
    Return, // ret
    Move,   // mov
};

enum struct RegisterId {
    INVALID,
    RAX,
    RBX,
    RCX,
    RDX,
    R8,
    R9,
    R10,
    R11,
    R12,
    R13,
    R14,
    R15,
    RDI,
    RSI,
    RBP,
    RSP,

    // The function value return register: most likely RAX, but sometimes not.
    RETURN = 0x210,
};

} // namespace x86_64
} // namespace lcc

#endif /* LCC_CODEGEN_X86_64_HH */
