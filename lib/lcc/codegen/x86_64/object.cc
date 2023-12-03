#include <bit>
#include <lcc/codegen/generic_object.hh>
#include <lcc/codegen/mir.hh>
#include <lcc/codegen/x86_64.hh>
#include <lcc/codegen/x86_64/object.hh>
#include <lcc/context.hh>
#include <lcc/utils.hh>
#include <variant>

namespace lcc {
namespace x86_64 {

// FIXME: Endianness of target architecture, probably, not of compiled
// architecture. This is effectively acting as if target format is always
// native.
static constexpr bool little_endian = std::endian::native == std::endian::little;

static std::vector<u8> as_bytes(u16 value) {
    // 0xffff
    //   __    upper
    //     __  lower
    const u8 upper = u8((value >> 8) & 0xff);
    const u8 lower = u8((value >> 0) & 0xff);
    if constexpr (little_endian)
        return {lower, upper};
    else return {upper, lower};
}
static std::vector<u8> as_bytes(i16 value) {
    return as_bytes(static_cast<u16>(value));
}

static std::vector<u8> as_bytes(u32 value) {
    // 0xffff.ffff
    // a ____
    // b      ____
    const u8 upper_a = u8((value >> 24) & 0xff);
    const u8 lower_a = u8((value >> 16) & 0xff);
    const u8 upper_b = u8((value >> 8) & 0xff);
    const u8 lower_b = u8((value >> 0) & 0xff);
    if constexpr (little_endian)
        return {lower_b, upper_b, lower_a, upper_b};
    else return {upper_a, lower_a, upper_b, lower_b};
}
static std::vector<u8> as_bytes(i32 value) {
    return as_bytes(static_cast<u32>(value));
}

static std::vector<u8> as_bytes(u64 value) {
    const u8 upper_a = u8((value >> 56) & 0xff);
    const u8 lower_a = u8((value >> 48) & 0xff);
    const u8 upper_b = u8((value >> 40) & 0xff);
    const u8 lower_b = u8((value >> 32) & 0xff);
    const u8 upper_c = u8((value >> 24) & 0xff);
    const u8 lower_c = u8((value >> 16) & 0xff);
    const u8 upper_d = u8((value >> 8) & 0xff);
    const u8 lower_d = u8((value >> 0) & 0xff);
    if constexpr (little_endian)
        return {lower_d, upper_d, lower_c, upper_c, lower_b, upper_b, lower_a, upper_b};
    else return {upper_a, lower_a, upper_b, lower_b, upper_c, lower_c, upper_d, lower_d};
}
static std::vector<u8> as_bytes(i64 value) {
    return as_bytes(static_cast<u64>(value));
}

static constexpr std::vector<u8> as_bytes(MOperandImmediate imm) {
    if (imm.size <= 8)
        return {u8(imm.value)};
    if (imm.size <= 16)
        return as_bytes(u16(imm.value));
    if (imm.size <= 32)
        return as_bytes(u32(imm.value));
    if (imm.size <= 64)
        return as_bytes(u64(imm.value));
    LCC_UNREACHABLE();
}

// NOTE: +rw indicates the lower three bits of the opcode byte are used
// to indicate the 16-bit register operand.
// For registers R8 through R15, the REX.b bit also needs set.
[[nodiscard]]
static constexpr u8 rw_encoding(RegisterId id) {
    switch (id) {
        case RegisterId::RAX:
        case RegisterId::R8:
            return 0;

        case RegisterId::RCX:
        case RegisterId::R9:
            return 1;

        case RegisterId::RDX:
        case RegisterId::R10:
            return 2;

        case RegisterId::RBX:
        case RegisterId::R11:
            return 3;

        case RegisterId::RSP:
        case RegisterId::R12:
            return 4;

        case RegisterId::RBP:
        case RegisterId::R13:
            return 5;

        case RegisterId::RSI:
        case RegisterId::R14:
            return 6;

        case RegisterId::RDI:
        case RegisterId::R15:
            return 7;

        // Not encodable in rw.
        case RegisterId::RIP: break;

        // Not actual registers.
        case RegisterId::RETURN:
        case RegisterId::INVALID: break;
    }
    LCC_UNREACHABLE();
}
static constexpr u8 rw_encoding(Register reg) {
    return rw_encoding(RegisterId(reg.value));
}
[[nodiscard]]
static constexpr u8 rw_encoding(MOperand op) {
    // LCC_ASSERT(std::holds_alternative<MOperandRegister>(op));
    return rw_encoding(RegisterId(std::get<MOperandRegister>(op).value));
}

// NOTE: +rd indicates the lower three bits of the opcode byte are used
// to indicate the 32 or 64-bit register operand.
// For registers R8 through R15, the REX.b bit also needs set.
// EAX: 0
// ECX: 1
// EDX: 2
// EBX: 3
// ESP: 4
// EBP: 5
// ESI: 6
// EDI: 7
// R8D: REX.B, 0
// R9D: REX.B, 1
// R10D: REX.B, 2
// R11D: REX.B, 3
// R12D: REX.B, 4
// R13D: REX.B, 5
// R14D: REX.B, 6
// R15D: REX.B, 7
[[nodiscard]]
static constexpr u8 rd_encoding(RegisterId id) {
    return rw_encoding(id);
}
[[nodiscard]]
static constexpr u8 rd_encoding(Register reg) {
    return rw_encoding(reg);
}
[[nodiscard]]
static constexpr u8 rd_encoding(MOperand op) {
    return rw_encoding(op);
}

// NOTE: +rb indicates the lower three bits of the opcode byte are used
// to indicate the 32 or 64-bit register operand.
// For registers R8 through R15, the REX.b bit also needs set.
// AL: 0
// CL: 1
// DL: 2
// BL: 3
// SPL: REX.b, 4
// BPL: REX.b, 5
// SIL: REX.b, 6
// DIL: REX.b, 7
// R8B: REX.b, 0
// R9B: REX.b, 1
// R10B: REX.b, 2
// R11B: REX.b, 3
// R12B: REX.b, 4
// R13B: REX.b, 5
// R14B: REX.b, 6
// R15B: REX.b, 7
[[nodiscard]]
static constexpr u8 rb_encoding(RegisterId id) {
    return rw_encoding(id);
}
[[nodiscard]]
static constexpr u8 rb_encoding(Register reg) {
    return rw_encoding(reg);
}
[[nodiscard]]
static constexpr u8 rb_encoding(MOperand op) {
    return rw_encoding(op);
}

/// REX.W == extend to 64 bit operation
/// REX.R == top bit of reg modrm field
/// REX.X == top bit of index sib field
/// REX.B == top bit of r/m modrm field
[[nodiscard]]
static constexpr u8 rex_byte(bool w, bool r, bool x, bool b) {
    return u8(0b01000000 | ((w ? 1 : 0) << 3) | ((r ? 1 : 0) << 2) | ((x ? 1 : 0) << 1) | (b ? 1 : 0));
}
/// REX.W prefix is commonly used to promote a 32-bit operation to 64-bit.
[[nodiscard]]
static constexpr u8 rexw_byte() {
    return rex_byte(true, false, false, false);
}

static constexpr u8 regbits(RegisterId id) {
    switch (id) {
        case RegisterId::RAX: return 0b0000;
        case RegisterId::RCX: return 0b0001;
        case RegisterId::RDX: return 0b0010;
        case RegisterId::RBX: return 0b0011;
        case RegisterId::RSP: return 0b0100;
        case RegisterId::RBP: return 0b0101;
        case RegisterId::RSI: return 0b0110;
        case RegisterId::RDI: return 0b0111;
        case RegisterId::R8: return 0b1000;
        case RegisterId::R9: return 0b1001;
        case RegisterId::R10: return 0b1010;
        case RegisterId::R11: return 0b1011;
        case RegisterId::R12: return 0b1100;
        case RegisterId::R13: return 0b1101;
        case RegisterId::R14: return 0b1110;
        case RegisterId::R15: return 0b1111;
        default: break;
    }
    Diag::ICE("Unhandled register in regbits: %s\n", ToString(id));
}
static constexpr u8 regbits(Register reg) {
    return regbits(RegisterId(reg.value));
}

static constexpr bool regbits_top(u8 bits) {
    return bits & 0b1000;
}
static constexpr bool reg_topbit(RegisterId id) {
    return regbits_top(regbits(id));
}
static constexpr bool reg_topbit(Register reg) {
    return regbits_top(regbits(RegisterId(reg.value)));
}

static constexpr u8 modrm_byte(u8 mod, u8 reg, u8 rm) {
    // Ensure no bits above the amount expected are set.
    LCC_ASSERT((mod & (~0b11)) == 0);
    // Top bit of register stored in REX bit(s), but may still be present here.
    LCC_ASSERT((reg & (~0b1111)) == 0);
    LCC_ASSERT((rm & (~0b1111)) == 0);
    return u8((mod << 6) | ((reg & 0b111) << 3) | (rm & 0b111));
}

/// A SIB byte is needed in the following cases:
///   a. If a memory operand has two pointer or index registers,
///   b. If a memory operand has a scaled index register,
///   c. If a memory operand has the stack pointer (ESP or RSP) as base,
///   d. If a memory operand in 64-bit mode uses a 32-bit sign-extended
///      direct memory address rather than a RIP-relative address.
/// A SIB byte cannot be used in 16-bit addressing mode.
static constexpr u8 sib_byte(u8 scale_factor, u8 index, u8 base) {
    // Ensure no bits above the amount expected are set.
    LCC_ASSERT((scale_factor & (~0b11)) == 0);
    LCC_ASSERT((index & (~0b1111)) == 0);
    LCC_ASSERT((base & (~0b1111)) == 0);
    return u8((scale_factor << 6) | ((index & 0b111) << 3) | (base & 0b111));
}

// TODO/FIXME: There are lots of issues regarding Chapter 2, Volume 2,
// Table 2-5 "Special Cases of REX Encodings" of the Intel SDM.
// - SIB byte also required for R12-based addressing.
//   I *think*, based on other things in the table, that this only
//   applies when mod != 0b11.
// - Using RBP or R13 without displacement must be done using mod = 01
//   with a displacement of 0.
// - SIB Byte base = 0101(EBP)
//   Base register is unused if mod = 0.
//   This requires explicit displacement to be used with EBP/RBP or
//   R13.
//   NOTE: I have no clue what this one means.

/// Should be used after every modrm byte with a mod not equal to 0b11
/// is written that may contain r12 in the r/m field.
static constexpr void mcode_sib_if_r12(Section& text, RegisterId address_register, u8 modrm) {
    if (address_register == RegisterId::R12 && (modrm & 0b11000000) != 0b11000000)
        text.contents.push_back(sib_byte(0b00, 0b100, 0b100));
}

// Must be MOperand* types
template <typename Op>
bool is_one_operand(MInst& inst) {
    // clang-format off
    return inst.all_operands().size() == 1
           and std::holds_alternative<Op>(inst.get_operand(0));
    // clang-format on
}

bool is_imm(MInst& inst) {
    return is_one_operand<MOperandImmediate>(inst);
}
auto extract_imm(MInst& inst) {
    return std::get<MOperandImmediate>(inst.get_operand(0));
}
bool is_reg(MInst& inst) {
    return is_one_operand<MOperandRegister>(inst);
}
auto extract_reg(MInst& inst) {
    return std::get<MOperandRegister>(inst.get_operand(0));
}
bool is_local(MInst& inst) {
    return is_one_operand<MOperandLocal>(inst);
}
auto extract_local(MInst& inst) {
    return std::get<MOperandLocal>(inst.get_operand(0));
}
bool is_global(MInst& inst) {
    return is_one_operand<MOperandGlobal>(inst);
}
auto extract_global(MInst& inst) {
    return std::get<MOperandGlobal>(inst.get_operand(0));
}

template <typename Op1, typename Op2>
bool is_two_operand(MInst& inst) {
    // clang-format off
    return inst.all_operands().size() == 2
           and std::holds_alternative<Op1>(inst.get_operand(0))
           and std::holds_alternative<Op2>(inst.get_operand(1));
    // clang-format on
}
template <typename Op1, typename Op2>
std::tuple<Op1, Op2> extract_two_operand(MInst& inst) {
    return {
        std::get<Op1>(inst.get_operand(0)),
        std::get<Op2>(inst.get_operand(1))};
}

bool is_reg_imm(MInst& inst) {
    return is_two_operand<MOperandRegister, MOperandImmediate>(inst);
}
auto extract_reg_imm(MInst& inst) {
    return extract_two_operand<MOperandRegister, MOperandImmediate>(inst);
}
bool is_reg_reg(MInst& inst) {
    return is_two_operand<MOperandRegister, MOperandRegister>(inst);
}
auto extract_reg_reg(MInst& inst) {
    return extract_two_operand<MOperandRegister, MOperandRegister>(inst);
}
bool is_reg_local(MInst& inst) {
    return is_two_operand<MOperandRegister, MOperandLocal>(inst);
}
auto extract_reg_local(MInst& inst) {
    return extract_two_operand<MOperandRegister, MOperandLocal>(inst);
}
bool is_imm_reg(MInst& inst) {
    return is_two_operand<MOperandImmediate, MOperandRegister>(inst);
}
auto extract_imm_reg(MInst& inst) {
    return extract_two_operand<MOperandImmediate, MOperandRegister>(inst);
}
bool is_imm_local(MInst& inst) {
    return is_two_operand<MOperandImmediate, MOperandLocal>(inst);
}
auto extract_imm_local(MInst& inst) {
    return extract_two_operand<MOperandImmediate, MOperandLocal>(inst);
}
bool is_local_imm(MInst& inst) {
    return is_two_operand<MOperandLocal, MOperandImmediate>(inst);
}
auto extract_local_imm(MInst& inst) {
    return extract_two_operand<MOperandLocal, MOperandImmediate>(inst);
}
bool is_local_reg(MInst& inst) {
    return is_two_operand<MOperandLocal, MOperandRegister>(inst);
}
auto extract_local_reg(MInst& inst) {
    return extract_two_operand<MOperandLocal, MOperandRegister>(inst);
}

template <usz... ints>
constexpr bool is_one_of(usz value) {
    return ((ints == value) or ...);
}

static constexpr u8 prefix16 = 0x66;
static void assemble_inst(MFunction& func, MInst& inst, Section& text) {
    switch (Opcode(inst.opcode())) {
        case Opcode::Return:
            text += 0xc3;
            break;

        case Opcode::Push: {
            //       0x50+rw  |  PUSH r16  |  O
            // REX.W 0x50+rd  |  PUSH r64  |  O
            if (is_reg(inst)) {
                auto reg = extract_reg(inst);

                LCC_ASSERT(
                    (is_one_of<16, 64>(reg.size)),
                    "x86_64 only supports pushing 16 and 64 bit register onto the stack: got {}",
                    reg.size
                );

                if (reg.size == 16) text += prefix16;
                // If reg size is 64, we of course need rexw byte to encode that.
                // But, we also need rex byte (without w bit set) to encode x64
                // registers (r8-r15), even when pushing the 16 bit versions of those
                // registers, to accomodate the top bit of the register encoding.
                if (reg.size == 64 or reg_topbit(reg))
                    text += rex_byte(reg.size == 64, false, false, reg_topbit(reg));
                text += 0x50 + rd_encoding(reg);
            }
            //       0x6a ib  |  PUSH imm8   |  I
            //       0x68 iw  |  PUSH imm16  |  I
            //       0x68 id  |  PUSH imm32  |  I
            else if (is_imm(inst)) {
                auto imm = extract_imm(inst);

                if (imm.size <= 8)
                    text += {0x6a, u8(imm.value)};
                else if (imm.size <= 32) {
                    text += 0x68;
                    text += as_bytes(imm);
                } else Diag::ICE("x86_64 only supports pushing immediates sized 32, 16, or 8 bits: got {}", imm.size);
            }
        } break;

        case Opcode::Move: {
            // GNU syntax (src, dst operands)
            //       0x88 /r  |  MOV r8, r/m8     |  MR
            //  0x66 0x89 /r  |  MOV r16, r/m16   |  MR
            //       0x89 /r  |  MOV r32, r/m32   |  MR
            // REX.W 0x89 /r  |  MOV r64, r/m64   |  MR
            // "/r" means that register/memory operands are encoded in modrm byte.
            // "MR" means that the source operand goes in reg field of modrm and the
            // destination operand goes in the r/m field.
            if (is_reg_reg(inst)) {
                auto [src, dst] = extract_reg_reg(inst);

                LCC_ASSERT(
                    src.size == dst.size,
                    "x86_64 only supports register to register moves when the registers are of the same size: got {} and {}",
                    src.size,
                    dst.size
                );

                // 1 isn't a valid register, but we treat it as 1 byte. Makes lowering of
                // booleans much easier.
                LCC_ASSERT(
                    (is_one_of<1, 8, 16, 32, 64>(src.size)),
                    "x86_64: invalid register size"
                );

                u8 op = u8(-1);
                u8 modrm = modrm_byte(0b11, regbits(src), regbits(dst));
                if (src.size == 1 or src.size == 8)
                    op = 0x88;
                else op = 0x89;

                if (src.size == 16) text += prefix16;
                if (src.size == 64 or reg_topbit(src) or reg_topbit(dst))
                    text += rex_byte(src.size == 64, reg_topbit(src), false, reg_topbit(dst));
                text += {op, modrm};
            }
            // GNU syntax (src, dst operands)
            //        0xb0+rb ib | MOV imm8, r8   | OI
            //   0x66 0xb8+rw iw | MOV imm16, r16 | OI
            //        0xb8+rd id | MOV imm32, r32 | OI
            //  REX.W 0xb8+rd io | MOV imm64, r64 | OI
            // "OI" means source operand is an immediate and destination operand is
            // part of the opcode byte itself (bottom 3 bits).
            else if (is_imm_reg(inst)) {
                auto [imm, dst] = extract_imm_reg(inst);

                // 1 isn't a valid register, but we treat it as 1 byte. Makes lowering of
                // booleans much easier.
                LCC_ASSERT(
                    (is_one_of<1, 8, 16, 32, 64>(dst.size)),
                    "x86_64: invalid register size"
                );

                // TODO: Optimisations; can reduce code size if 64-bit immediate fits into
                // 32-bit register, as moving into 32-bit register clears the top half as
                // well.

                u8 op = u8(-1);
                if (dst.size == 1 or dst.size == 8)
                    op = 0xb0 + rb_encoding(dst);
                else op = 0xb8 + rd_encoding(dst);

                if (dst.size == 16) text += prefix16;
                if (dst.size == 64 or reg_topbit(dst))
                    text += rex_byte(dst.size == 64, false, false, reg_topbit(dst));
                text += op;
                text += as_bytes(imm);

            } else Diag::ICE(
                "Sorry, unhandled form of move\n    {}\n",
                PrintMInstImpl(inst, opcode_to_string)
            );
        } break;

        case Opcode::MoveDereferenceRHS: {
            // GNU syntax (src, dst operands)
            //        0x88 /r | MOV r8, r/m8   | MR
            //   0x66 0x89 /r | MOV r16, r/m16 | MR
            //        0x89 /r | MOV r32, r/m32 | MR
            //  REX.W 0x89 /r | MOV r64, r/m64 | MR
            // "MR" means that the source operand goes in reg field of modrm and the
            // destination operand goes in the r/m field.
            if (is_reg_local(inst)) {
                auto [reg, local] = extract_reg_local(inst);
                isz offset = func.local_offset(local);

                LCC_ASSERT((is_one_of<1, 8, 16, 32, 64>(reg.size)));

                u8 op = 0x89;
                if (reg.size == 1 or reg.size == 8)
                    op = 0x88;

                u8 modrm = modrm_byte(0b10, regbits(reg), regbits(RegisterId::RBP));

                if (reg.size == 16) text += prefix16;
                if (reg.size == 64 || reg_topbit(reg))
                    text += rex_byte(reg.size == 64, reg_topbit(reg), false, false);
                text += {op, modrm};
                text += as_bytes(i32(offset));
                // TODO: r12 nonsense
            }
            // GNU syntax (src, dst operands)
            //        0xc6 /0 ib | MOV imm8, r/m8   | MI
            //   0x66 0xc7 /0 iw | MOV imm16, r/m16 | MI
            //        0xc7 /0 id | MOV imm32, r/m32 | MI
            //  REX.W 0xc7 /0 id | MOV imm32, r/m64 | MI // sign extended
            // "/0" is a "/digit", which is a digit between 0 and 7 that indicates
            // that the modrm byte uses only the r/m operand, and the reg field should
            // contain this digit.
            // "MI" means that the destination operand goes in the r/m field.
            else if (is_imm_local(inst)) {
                auto [imm, local] = extract_imm_local(inst);
                isz offset = func.local_offset(local);

                u8 op = 0xc7;
                if (imm.size <= 8)
                    op = 0xc6;

                u8 modrm = modrm_byte(0b10, 0, regbits(RegisterId::RBP));

                if (imm.size > 8 and imm.size <= 16)
                    text += prefix16;
                if (imm.size > 32 or reg_topbit(RegisterId::RBP))
                    text += rex_byte(imm.size > 32, reg_topbit(RegisterId::RBP), false, false);
                text += {op, modrm};
                // modrm 0b10 disp32 offset
                text += as_bytes(i32(offset));
                text += as_bytes(imm);
            }
        } break;

        case Opcode::MoveDereferenceLHS: {
            // GNU syntax (src, dst operands)
            //        0x8a /r | MOV r/m8, r8   | RM
            //   0x66 0x8b /r | MOV r/m16, r16 | RM
            //        0x8b /r | MOV r/m32, r32 | RM
            //  REX.W 0x8b /r | MOV r/m64, r64 | RM
            // "RM" means the source operand is in the r/m field of the modrm byte and
            // the destination operand is in the reg field of the modrm byte.
            if (is_local_reg(inst)) {
                auto [local, reg] = extract_local_reg(inst);
                isz offset = func.local_offset(local);

                LCC_ASSERT((is_one_of<1, 8, 16, 32, 64>(reg.size)));

                u8 op = 0x8a;
                if (reg.size == 1 or reg.size == 8)
                    op = 0x8b;

                u8 modrm = modrm_byte(0b10, regbits(reg), regbits(RegisterId::RBP));

                if (reg.size == 16) text += prefix16;
                if (reg.size == 64 || reg_topbit(reg))
                    text += rex_byte(reg.size == 64, reg_topbit(reg), false, false);
                text += {op, modrm};
                text += as_bytes(i32(offset));
            }
        } break;

        case Opcode::Pop:
        case Opcode::Jump:
        case Opcode::Call:
        case Opcode::MoveSignExtended:
        case Opcode::MoveZeroExtended:
        case Opcode::LoadEffectiveAddress:
        case Opcode::And:
        case Opcode::ShiftRightArithmetic:
        case Opcode::ShiftRightLogical:
        case Opcode::ShiftLeft:
        case Opcode::Add:
        case Opcode::Multiply:
        case Opcode::Sub:
        case Opcode::Compare:
        case Opcode::Test:
        case Opcode::JumpIfZeroFlag:
        case Opcode::SetByteIfEqual:
        case Opcode::SetByteIfNotEqual:
        case Opcode::SetByteIfEqualOrLessUnsigned:
        case Opcode::SetByteIfEqualOrLessSigned:
        case Opcode::SetByteIfEqualOrGreaterUnsigned:
        case Opcode::SetByteIfEqualOrGreaterSigned:
        case Opcode::SetByteIfLessUnsigned:
        case Opcode::SetByteIfLessSigned:
        case Opcode::SetByteIfGreaterUnsigned:
        case Opcode::SetByteIfGreaterSigned: break;

        case Opcode::Poison: LCC_UNREACHABLE();
    }
}

static void assemble(MFunction& func, Section& text) {
    for (auto& block : func.blocks()) {
        for (auto& inst : block.instructions()) {
            assemble_inst(func, inst, text);
        }
    }
}

GenericObject emit_mcode_gobj(Module* module, const MachineDescription& desc, std::vector<MFunction>& mir) {
    GenericObject out{};

    Section text_{".text", {}, 0, 0, false};
    Section data_{".data", {}, 0, 0, false};
    Section bss_{".bss", {}, 0, 0, true};
    out.sections.push_back(text_);
    out.sections.push_back(data_);
    out.sections.push_back(bss_);

    Section& text = out.section(".text");
    // Section& data = out.section(".data");
    // Section& bss = out.section(".bss");

    for (auto* var : module->vars())
        out.symbol_from_global(var);

    for (auto& func : mir) {
        const bool imported = func.linkage() == Linkage::Imported || func.linkage() == Linkage::Reexported;
        // const bool exported = func.linkage() == Linkage::Exported || func.linkage() == Linkage::Reexported;

        if (imported) {
            Symbol sym{};
            sym.kind = Symbol::Kind::EXTERNAL;
            sym.name = func.name();
            // FIXME: Is section name or byte offset needed?
            out.symbols.push_back(sym);
        } else {
            Symbol sym{};
            sym.kind = Symbol::Kind::FUNCTION;
            sym.name = func.name();
            sym.section_name = text.name;
            sym.byte_offset = text.contents.size();
            out.symbols.push_back(sym);
        }

        // Assemble function into machine code.
        assemble(func, text);
    }

    // LCC_TODO("Actually assemble into machine code, populate symbols, etc");

    return out;
}

} // namespace x86_64
} // namespace lcc
