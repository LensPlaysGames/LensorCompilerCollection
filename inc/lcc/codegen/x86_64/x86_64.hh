#ifndef LCC_CODEGEN_X86_64_HH
#define LCC_CODEGEN_X86_64_HH

#include <lcc/codegen/mir.hh>
#include <lcc/utils.hh>

#include <string>
#include <string_view>

namespace lcc::x86_64 {

constexpr usz GeneralPurposeBitwidth = 64;
constexpr usz GeneralPurposeBytewidth = 8;

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
    Xor,
    ShiftRightArithmetic,
    ShiftRightLogical,
    ShiftLeft,

    Add, // add
    Sub, // sub

    Multiply, // mul

    SignedDivide,   // idiv
    UnsignedDivide, // div

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

    // You can re-order these, just not past the fences.
    ScalarFloatFENCEBegin,
    ScalarFloatMove,               // movss/movsd
    ScalarFloatMoveDereferenceLHS, // movss/movsd
    ScalarFloatMoveDereferenceRHS, // movss/movsd
    ScalarFloatAdd,                // addss/addsd
    ScalarFloatSub,                // subss/subsd
    ScalarFloatMul,                // mulss/mulsd
    ScalarFloatDiv,                // divss/divsd
    ScalarFloatFENCEEnd,
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

    XMM0,
    XMM1,
    XMM2,
    XMM3,
    XMM4,
    XMM5,
    XMM6,
    XMM7,
    XMM8,
    XMM9,
    XMM10,
    XMM11,
    XMM12,
    XMM13,
    XMM14,
    XMM15,

    // The function value return register: most likely RAX, but sometimes not.
    RETURN = 0x210,
};

auto opcode_to_string(usz opcode) -> std::string;

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

template <int x>
constexpr auto SSEScalar() -> std::string_view {
    return ConstexprFormat<"xmm{}", x>();
}

} // namespace regs

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
        case Opcode::Xor: return "xor";
        case Opcode::ShiftLeft: return "shl";
        case Opcode::ShiftRightLogical: return "shr";
        case Opcode::ShiftRightArithmetic: return "sar";
        case Opcode::Add: return "add";
        case Opcode::Multiply: return "imul";
        case Opcode::Sub: return "sub";
        case Opcode::SignedDivide: return "idiv";
        case Opcode::UnsignedDivide: return "div";
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
        case Opcode::ScalarFloatMoveDereferenceLHS:
        case Opcode::ScalarFloatMoveDereferenceRHS:
        case Opcode::ScalarFloatMove: return "movs";
        case Opcode::ScalarFloatAdd: return "adds";
        case Opcode::ScalarFloatSub: return "subs";
        case Opcode::ScalarFloatMul: return "muls";
        case Opcode::ScalarFloatDiv: return "divs";
        case Opcode::ScalarFloatFENCEBegin:
        case Opcode::ScalarFloatFENCEEnd:
            LCC_UNREACHABLE();
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
        case RegisterId::XMM0: return SSEScalar<0>();
        case RegisterId::XMM1: return SSEScalar<1>();
        case RegisterId::XMM2: return SSEScalar<2>();
        case RegisterId::XMM3: return SSEScalar<3>();
        case RegisterId::XMM4: return SSEScalar<4>();
        case RegisterId::XMM5: return SSEScalar<5>();
        case RegisterId::XMM6: return SSEScalar<6>();
        case RegisterId::XMM7: return SSEScalar<7>();
        case RegisterId::XMM8: return SSEScalar<8>();
        case RegisterId::XMM9: return SSEScalar<9>();
        case RegisterId::XMM10: return SSEScalar<10>();
        case RegisterId::XMM11: return SSEScalar<11>();
        case RegisterId::XMM12: return SSEScalar<12>();
        case RegisterId::XMM13: return SSEScalar<13>();
        case RegisterId::XMM14: return SSEScalar<14>();
        case RegisterId::XMM15: return SSEScalar<15>();
    }
    LCC_UNREACHABLE();
}

constexpr auto ToString(RegisterId id) -> std::string_view {
    return ToString(id, 64);
}

} // namespace lcc::x86_64

#endif /* LCC_CODEGEN_X86_64_HH */
