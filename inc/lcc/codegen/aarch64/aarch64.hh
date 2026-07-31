#ifndef LCC_CODEGEN_AARCH64_HH
#define LCC_CODEGEN_AARCH64_HH

#include <hdronly/lcc/typedefs.hh>

#include <lcc/codegen/mir.hh>

namespace lcc::aarch64 {

constexpr usz GeneralPurposeBitwidth = 64;
constexpr usz GeneralPurposeBytewidth = 8;

enum struct Opcode : u32 {
    Poison = u32(lcc::MInst::Kind::ArchStart),

    Address,      // adr
    Load,         // ldr
    Store,        // str
    LoadPair,     // ldp
    StorePair,    // stp
    Move,         // mov
    MoveThenKeep, // movk

    Add,            // add
    Sub,            // sub
    Multiply,       // mul
    SignedDivide,   // sdiv
    UnsignedDivide, // udiv
    Negate,         // neg

    LogicalOR,            /// orr
    LogicalAND,           /// and
    LogicalXOR,           /// eor  (Exclusive-OR)
    LogicalANDComplement, /// bic  w/ Complement (aka Bit Instruction Clear)
    LogicalORComplement,  /// orn  w/ Complement
    LogicalXORComplement, /// eon  w/ Complement
    LogicalNOT,           /// mvn  (Logical NOR w/ itself)

    LogicalShiftLeft,     // lsl
    LogicalShiftRight,    // lsr
    ArithmeticShiftRight, // asr

    Compare,                      // cmp
    Branch,                       // b, bal
    BranchIfEqual,                // beq
    BranchIfNotEqual,             // bne
    BranchIfGreaterThan,          // bgt (signed)
    BranchIfLessThan,             // blt (signed)
    BranchIfGreaterThanOrEqual,   // bge (signed)
    BranchIfLessThanOrEqual,      // ble (signed)
    BranchIfUnsignedHigherOrSame, // bhs (Unsigned Greater Than or Equal), bcs (branch carry set)
    BranchIfUnsignedLowerThan,    // blo (Unsigned Less Than), bcc (branch carry clear)
    BranchIfNegative,             // bmi
    BranchIfPositiveOrZero,       // bpl (Not Negative)
    BranchIfSignedOverflow,       // bvs
    BranchIfNotSignedOverflow,    // bvc
    BranchIfUnsignedHigher,       // bhi (Unsigned Greater Than)
    BranchIfUnsignedLowerOrSame,  // bls (Unsigned Less Than or Equal)
    Return,                       // ret

    System, // svc

    Max
};

enum struct RegisterId : u32 {
    INVALID,

    GeneralPurposeFENCEBegin,
    R0,
    R1,
    R2,
    R3,
    R4,
    R5,
    R6,
    R7,
    R8,
    R9,
    R10,
    R11,
    R12,
    R13,
    R14,
    R15,
    R16,
    R17,
    R18,
    R19,
    R20,
    R21,
    R22,
    R23,
    R24,
    R25,
    R26,
    R27,
    R28,
    R29,
    R30,
    R31,
    GeneralPurposeFENCEEnd,

    // The function value return register.
    RETURN = 0x210,
};

constexpr std::string StringifyEnum(Opcode op) {
    switch (op) {
        case Opcode::Poison: return "aarch64.POISON";

        case Opcode::Address: return "adr";
        case Opcode::Load: return "ldr";
        case Opcode::Store: return "str";
        case Opcode::LoadPair: return "ldp";
        case Opcode::StorePair: return "stp";
        case Opcode::Move: return "mov";
        case Opcode::MoveThenKeep: return "movk";

        case Opcode::Add: return "add";
        case Opcode::Sub: return "sub";
        case Opcode::Multiply: return "mul";
        case Opcode::SignedDivide: return "sdiv";
        case Opcode::UnsignedDivide: return "udiv";
        case Opcode::Negate: return "neg";

        case Opcode::LogicalOR: return "orr";
        case Opcode::LogicalAND: return "and";
        case Opcode::LogicalXOR: return "eor";
        case Opcode::LogicalANDComplement: return "bic";
        case Opcode::LogicalORComplement: return "orn";
        case Opcode::LogicalXORComplement: return "eon";
        case Opcode::LogicalNOT: return "mvn";
        case Opcode::LogicalShiftLeft: return "lsl";
        case Opcode::LogicalShiftRight: return "lsr";
        case Opcode::ArithmeticShiftRight: return "asr";

        case Opcode::Compare: return "cmp";
        case Opcode::Branch: return "b";
        case Opcode::BranchIfEqual: return "beq";
        case Opcode::BranchIfNotEqual: return "bne";
        case Opcode::BranchIfGreaterThan: return "bgt";
        case Opcode::BranchIfLessThan: return "blt";
        case Opcode::BranchIfGreaterThanOrEqual: return "bge";
        case Opcode::BranchIfLessThanOrEqual: return "ble";
        case Opcode::BranchIfUnsignedHigherOrSame: return "bhs";
        case Opcode::BranchIfUnsignedLowerThan: return "blo";
        case Opcode::BranchIfNegative: return "bmi";
        case Opcode::BranchIfPositiveOrZero: return "bpl";
        case Opcode::BranchIfSignedOverflow: return "bvs";
        case Opcode::BranchIfNotSignedOverflow: return "bvc";
        case Opcode::BranchIfUnsignedHigher: return "bhi";
        case Opcode::BranchIfUnsignedLowerOrSame: return "bls";
        case Opcode::Return: return "ret";
        case Opcode::System: return "svc";
        case Opcode::Max: std::unreachable();
    }
}

constexpr std::string StringifyEnum(RegisterId id) {
    switch (id) {
        case RegisterId::GeneralPurposeFENCEBegin:
        case RegisterId::GeneralPurposeFENCEEnd:
            std::unreachable();
        case RegisterId::INVALID: return "aarch64.INVALID";
        case RegisterId::RETURN: return "aarch64.RETURN";
        case RegisterId::R0: return "r0";
        case RegisterId::R1: return "r1";
        case RegisterId::R2: return "r2";
        case RegisterId::R3: return "r3";
        case RegisterId::R4: return "r4";
        case RegisterId::R5: return "r5";
        case RegisterId::R6: return "r6";
        case RegisterId::R7: return "r7";
        case RegisterId::R8: return "r8";
        case RegisterId::R9: return "r9";
        case RegisterId::R10: return "r10";
        case RegisterId::R11: return "r11";
        case RegisterId::R12: return "r12";
        case RegisterId::R13: return "r13";
        case RegisterId::R14: return "r14";
        case RegisterId::R15: return "r15";
        case RegisterId::R16: return "r16";
        case RegisterId::R17: return "r17";
        case RegisterId::R18: return "r18";
        case RegisterId::R19: return "r19";
        case RegisterId::R20: return "r20";
        case RegisterId::R21: return "r21";
        case RegisterId::R22: return "r22";
        case RegisterId::R23: return "r23";
        case RegisterId::R24: return "r24";
        case RegisterId::R25: return "r25";
        case RegisterId::R26: return "r26";
        case RegisterId::R27: return "r27";
        case RegisterId::R28: return "r28";
        case RegisterId::R29: return "r29";
        case RegisterId::R30: return "r30";
        case RegisterId::R31: return "r31";
    }
}

constexpr std::string ToString(RegisterId id, usz size) {
    auto out = StringifyEnum(id);
    LCC_ASSERT(out.size());

    if (size <= 32)
        out[0] = 'w';
    else out[0] = 'x';

    return out;
}

} // namespace lcc::aarch64

#endif /* LCC_CODEGEN_AARCH64_HH */
