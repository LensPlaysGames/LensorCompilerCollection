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
    MoveZeroExtended, // movzx

    // Optional third offset operand (default zero).
    MoveDereferenceRHS, // mov <any>, [offset](%register).
    MoveDereferenceLHS, // mov [offset](%register), <any>

    LoadEffectiveAddress, // lea

    // Bitwise
    Not, // One's Complement Negation
    And,
    Or,
    ShiftRightArithmetic,
    ShiftRightLogical,
    ShiftLeft,

    Add,      // add
    Multiply, // mul

    Sub, // sub

    Compare,        // cmp
    Test,           // test
    JumpIfZeroFlag, // jz

    SetByteIfEqual,                  // sete (set if equal)
    SetByteIfNotEqual,               // setne (set if not equal)
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

std::string opcode_to_string(usz opcode);

namespace regs {
template <char r>
constexpr auto LegacyGPR(usz size) -> std::string_view {
    switch (size) {
        default: LCC_ASSERT(false, "Invalid size: {}", size);
        case 64: return ConstexprFormat<"r{}x", r>();
        case 32: return ConstexprFormat<"e{}x", r>();
        case 16: return ConstexprFormat<"{}x", r>();
        case 8: return ConstexprFormat<"{}l", r>();
    }
}

template <int r>
constexpr auto ExtendedGPR(usz size) -> std::string_view {
    switch (size) {
        default: LCC_ASSERT(false, "Invalid size: {}", size);
        case 64: return ConstexprFormat<"r{}", r>();
        case 32: return ConstexprFormat<"r{}d", r>();
        case 16: return ConstexprFormat<"r{}w", r>();
        case 8: return ConstexprFormat<"r{}b", r>();
    }
}

template <detail::static_string s>
constexpr auto Special(usz size) -> std::string_view {
    switch (size) {
        default: LCC_ASSERT(false, "Invalid size: {}", size);
        case 64: return ConstexprFormat<"r{}{}", s[0], s[1]>();
        case 32: return ConstexprFormat<"e{}{}", s[0], s[1]>();
        case 16: return ConstexprFormat<"{}{}", s[0], s[1]>();
        case 8: return ConstexprFormat<"{}{}l", s[0], s[1]>();
    }
}
} // namespace x86_64::regs

constexpr auto ToString(Opcode op) -> std::string_view {
    switch (op) {
        case Opcode::Poison: return "x86_64.poison"; // FIXME: Crash here?
        case Opcode::Return: return "ret";
        case Opcode::Jump: return "jmp";
        case Opcode::Call: return "call";
        case Opcode::MoveDereferenceLHS:
        case Opcode::MoveDereferenceRHS:
        case Opcode::Move: return "mov";
        case Opcode::MoveSignExtended: return "movsx";
        case Opcode::MoveZeroExtended: return "movzx";
        case Opcode::LoadEffectiveAddress: return "lea";
        case Opcode::Not: return "not";
        case Opcode::And: return "and";
        case Opcode::Or: return "or";
        case Opcode::ShiftLeft: return "shl";
        case Opcode::ShiftRightLogical: return "shr";
        case Opcode::ShiftRightArithmetic: return "sar";
        case Opcode::Add: return "add";
        case Opcode::Multiply: return "imul";
        case Opcode::Sub: return "sub";
        case Opcode::Push: return "push";
        case Opcode::Pop: return "pop";
        case Opcode::Test: return "test";
        case Opcode::JumpIfZeroFlag: return "jz";
        case Opcode::Compare: return "cmp";
        case Opcode::SetByteIfEqual: return "sete";
        case Opcode::SetByteIfNotEqual: return "setne";
        case Opcode::SetByteIfLessUnsigned: return "setb";
        case Opcode::SetByteIfLessSigned: return "setl";
        case Opcode::SetByteIfGreaterUnsigned: return "seta";
        case Opcode::SetByteIfGreaterSigned: return "setg";
        case Opcode::SetByteIfEqualOrLessUnsigned: return "setbe";
        case Opcode::SetByteIfEqualOrLessSigned: return "setle";
        case Opcode::SetByteIfEqualOrGreaterUnsigned: return "setae";
        case Opcode::SetByteIfEqualOrGreaterSigned: return "setge";
    }
    LCC_UNREACHABLE();
}

constexpr auto ToString(RegisterId id, usz size) -> std::string_view {
    using namespace regs;
    if (not size) size = 64;
    switch (id) {
        case RegisterId::INVALID: return "x86_64.INVALID";
        case RegisterId::RETURN: return "x86_64.RETURN";
        case RegisterId::RAX: return LegacyGPR<'a'>(size);
        case RegisterId::RBX: return LegacyGPR<'b'>(size);
        case RegisterId::RCX: return LegacyGPR<'c'>(size);
        case RegisterId::RDX: return LegacyGPR<'d'>(size);
        case RegisterId::R8: return ExtendedGPR<8>(size);
        case RegisterId::R9: return ExtendedGPR<9>(size);
        case RegisterId::R10: return ExtendedGPR<10>(size);
        case RegisterId::R11: return ExtendedGPR<11>(size);
        case RegisterId::R12: return ExtendedGPR<12>(size);
        case RegisterId::R13: return ExtendedGPR<13>(size);
        case RegisterId::R14: return ExtendedGPR<14>(size);
        case RegisterId::R15: return ExtendedGPR<15>(size);
        case RegisterId::RDI: return Special<"di">(size);
        case RegisterId::RSI: return Special<"si">(size);
        case RegisterId::RBP: return Special<"bp">(size);
        case RegisterId::RSP: return Special<"sp">(size);
        case RegisterId::RIP: return Special<"ip">(size);
    }
    LCC_UNREACHABLE();
}

constexpr auto ToString(RegisterId id) -> std::string_view {
    return ToString(id, 64);
}

} // namespace x86_64
} // namespace lcc

#endif /* LCC_CODEGEN_X86_64_HH */
