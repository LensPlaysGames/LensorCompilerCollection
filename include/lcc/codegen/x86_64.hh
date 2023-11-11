#ifndef LCC_CODEGEN_X86_64_HH
#define LCC_CODEGEN_X86_64_HH

#include <lcc/codegen/mir.hh>
#include <lcc/utils.hh>

namespace lcc {
namespace x86_64 {

enum struct Opcode : u32 {
    Poison = u32(lcc::MInst::Kind::ArchStart),
    Return,           // ret
    Push,             // push
    Pop,              // pop
    Jump,             // jmp
    Call,             // call
    Move,             // mov
    MoveSignExtended, // movsx

    // Optional third offset operand (default zero).
    MoveDereferenceRHS, // mov <any>, [offset](%register).
    MoveDereferenceLHS, // mov [offset](%register), <any>

    LoadEffectiveAddress, // lea

    Add,      // add
    Multiply, // mul

    Sub, // sub

    Compare,        // cmp
    Test,           // test
    JumpIfZeroFlag, // jz

    SetByteIfEqual,                  // sete (set if equal)
    SetByteIfEqualOrLessUnsigned,    // setbe (set if below or equal, "below" being Intel's way of meaning unsigned less than)
    SetByteIfEqualOrLessSigned,      // setle (set if less or equal, "less" being Intel's way of meaning **signed** less than)
    SetByteIfEqualOrGreaterUnsigned, // setae (set if above or equal, "above" being Intel's way of meaning unsigned greater than)
    SetByteIfEqualOrGreaterSigned,   // setge (set if greater or equal, "greater" being Intel's way of meaning **signed** greater than)
    SetByteIfLessUnsigned,           // setb (set if below)
    SetByteIfLessSigned,             // setl (set if less)
    SetByteIfGreaterUnsigned,        // seta (set if above)
    SetByteIfGreaterSigned,          // setg (set if greater)
};

enum struct RegisterId : u32 {
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
    RIP,

    // The function value return register: most likely RAX, but sometimes not.
    RETURN = 0x210,
};

std::string ToString(Opcode op);
std::string ToString(RegisterId id);
std::string ToString(RegisterId id, usz size);

} // namespace x86_64
} // namespace lcc

#endif /* LCC_CODEGEN_X86_64_HH */
