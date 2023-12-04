#include <bit>
#include <lcc/codegen/generic_object.hh>
#include <lcc/codegen/mir.hh>
#include <lcc/codegen/mir_utils.hh>
#include <lcc/codegen/x86_64.hh>
#include <lcc/codegen/x86_64/object.hh>
#include <lcc/context.hh>
#include <lcc/utils.hh>
#include <variant>

namespace lcc {
namespace x86_64 {

// /digit - A digit between 0 and 7 indicates that the ModR/M byte of the
//     instruction uses only the r/m (register or memory) operand. The reg
//     field contains the digit that provides an extension to the
//     instruction's opcode.
// /r - Indicates that the ModR/M byte of the instruction contains a
//     register operand and an r/m operand.
// ib, iw, id, io - A 1-byte (ib), 2-byte (iw), 4-byte (id) or 8-byte (io)
//     immediate operand to the instruction that follows the opcode, ModR/M
//     bytes or scale-indexing bytes. The opcode determines if the operand is
//     a signed value. All words, doublewords, and quadwords are given with
//     the low-order byte first.
// +rb, +rw, +rd, +ro - Indicated the lower 3 bits of the opcode byte is
//     used to encode the register operand without a modR/M byte. The
//     instruction lists the corresponding hexadecimal value of the opcode
//     byte with low 3 bits as 000b. In non-64-bit mode, a register code, from
//     0 through 7, is added to the hexadecimal value of the opcode byte. In
//     64-bit mode, indicates the four bit field of REX.b and opcode[2:0]
//     field encodes the register operand of the instruction.

static std::vector<u8> as_bytes(u16 value) {
    // 0xffff
    //   __    upper
    //     __  lower
    const u8 upper = u8((value >> 8) & 0xff);
    const u8 lower = u8((value >> 0) & 0xff);
    return {lower, upper};
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
    return {lower_b, upper_b, lower_a, upper_a};
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
    return {lower_d, upper_d, lower_c, upper_c, lower_b, upper_b, lower_a, upper_a};
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

static constexpr std::vector<u8> as_bytes_cap32(MOperandImmediate imm) {
    if (imm.size > 32) imm.size = 32;
    return as_bytes(imm);
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

template <usz... ints>
constexpr bool is_one_of(usz value) {
    return ((ints == value) or ...);
}

static constexpr u8 prefix16 = 0x66;

// /r means register and r/m operand referenced by modrm.
// Pattern must be:
//           0x88 /r
//      0x66 0x89 /r
//           0x89 /r
//     REX.W 0x89 /r
// but where 0x88 and 0x89 are switched out with some other opcodes. MR vs
// RM operand encoding handles it per opcode.
static void opcode_slash_r(GenericObject& gobj, MFunction& func, MInst& inst, u8 opcode, Section& text) {
    // GNU syntax (src, dst operands)
    //
    //       0x88 /r  |  MOV r8, r/m8     |  MR
    //  0x66 0x89 /r  |  MOV r16, r/m16   |  MR
    //       0x89 /r  |  MOV r32, r/m32   |  MR
    // REX.W 0x89 /r  |  MOV r64, r/m64   |  MR
    //
    // GNU syntax (src, dst operands)
    //       0x38 /r | CMP r8, r/m8   | MR
    //  0x66 0x39 /r | CMP r16, r/m16 | MR
    //       0x39 /r | CMP r32, r/m32 | MR
    // REX.W 0x39 /r | CMP r64, r/m64 | MR
    //
    // "/r" means that register/memory operands are encoded in modrm byte.
    //
    // "MR" means that the source operand goes in reg field of modrm and the
    // destination operand goes in the r/m field.
    if (is_reg_reg(inst)) {
        auto [src, dst] = extract_reg_reg(inst);

        LCC_ASSERT(
            src.size == dst.size,
            "x86_64 only supports register to register modrm encoding when the registers are of the same size: got {} and {}",
            src.size,
            dst.size
        );

        // 1 isn't a valid register, but we treat it as 1 byte. Makes lowering of
        // booleans much easier.
        LCC_ASSERT(
            (is_one_of<1, 8, 16, 32, 64>(src.size)),
            "x86_64: invalid register size"
        );

        u8 op = opcode + 1;
        if (src.size == 1 or src.size == 8)
            op -= 1;

        u8 modrm = modrm_byte(0b11, regbits(src), regbits(dst));

        if (src.size == 16) text += prefix16;
        if (src.size == 64 or reg_topbit(src) or reg_topbit(dst))
            text += rex_byte(src.size == 64, reg_topbit(src), false, reg_topbit(dst));
        text += {op, modrm};
    }
}

static void assemble_inst(GenericObject& gobj, MFunction& func, MInst& inst, Section& text) {
    // TODO: Once I write code to assemble all the instructions, start to
    // consolidate and de-duplicate code by looking at "pattern" of
    // instruction encoding as if opcode can be switched out.
    // For example, (mov reg, reg) and (mov_dereference_rhs reg, local) both
    // share the same pattern, and due to the MR vs RM operand ordering, the
    // same code can handle both. So, we could make like a `emit_opcode_modrm`
    // function or something that can handle both of these just by switching
    // out the opcode it writes.

    // i.e. called with 0x94, would encode:
    // 0x0f 0x94 | SETE r/m8 | M
    // "M" means that the operand is encoded in r/m field of modrm byte.
    auto setcc = [&](u8 opcode) {
        if (is_reg(inst)) {
            auto reg = extract_reg(inst);
            LCC_ASSERT(
                reg.size == 1 or reg.size == 8,
                "x86_64 SETcc requires 1-byte operand"
            );
            u8 modrm = modrm_byte(0b11, 0, regbits(reg));
            if (reg_topbit(reg))
                text += rex_byte(false, false, false, reg_topbit(reg));
            text += {0x0f, opcode, modrm};
        } else Diag::ICE(
            "Sorry, unhandled form\n    {}\n",
            PrintMInstImpl(inst, opcode_to_string)
        );
    };

    switch (Opcode(inst.opcode())) {
        case Opcode::Return: {
            // TODO: Stack frame kinds
            // GNU syntax (src, dst operands)
            // mov %rbp, %rsp
            // pop %rbp
            auto mov_rbp_into_rsp = MInst(usz(Opcode::Move), {0, 0});
            mov_rbp_into_rsp.add_operand(MOperandRegister(usz(RegisterId::RBP), 64));
            mov_rbp_into_rsp.add_operand(MOperandRegister(usz(RegisterId::RSP), 64));
            auto pop_rbp = MInst(usz(Opcode::Pop), {0, 0});
            pop_rbp.add_operand(MOperandRegister(usz(RegisterId::RBP), 64));
            assemble_inst(gobj, func, mov_rbp_into_rsp, text);
            assemble_inst(gobj, func, pop_rbp, text);

            text += 0xc3;
        } break;

        case Opcode::Push: {
            // 0x50+rw  |  PUSH r16  |  O
            // 0x50+rd  |  PUSH r64  |  O
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
                if (reg_topbit(reg))
                    text += rex_byte(false, false, false, reg_topbit(reg));
                text += 0x50 + rd_encoding(reg);
            }
            // 0x6a ib  |  PUSH imm8   |  I
            // 0x68 iw  |  PUSH imm16  |  I
            // 0x68 id  |  PUSH imm32  |  I
            else if (is_imm(inst)) {
                auto imm = extract_imm(inst);

                if (imm.size <= 8)
                    text += {0x6a, u8(imm.value)};
                else if (imm.size <= 32) {
                    text += 0x68;
                    text += as_bytes(imm);
                } else Diag::ICE("x86_64 only supports pushing immediates sized 32, 16, or 8 bits: got {}", imm.size);
            } else Diag::ICE(
                "Sorry, unhandled form\n    {}\n",
                PrintMInstImpl(inst, opcode_to_string)
            );
        } break;

        case Opcode::Pop: {
            // 0x58+rw  |  POP r16  |  O
            // 0x58+rd  |  POP r64  |  O
            if (is_reg(inst)) {
                auto reg = extract_reg(inst);

                LCC_ASSERT(
                    (is_one_of<16, 64>(reg.size)),
                    "x86_64 only supports pushing 16 and 64 bit register onto the stack: got {}",
                    reg.size
                );

                if (reg.size == 16) text += prefix16;
                if (reg_topbit(reg))
                    text += rex_byte(false, false, false, reg_topbit(reg));
                text += 0x58 + rd_encoding(reg);
            } else Diag::ICE(
                "Sorry, unhandled form\n    {}\n",
                PrintMInstImpl(inst, opcode_to_string)
            );
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
                // OPT: Don't emit moves from a register into itself
                auto [src, dst] = extract_reg_reg(inst);
                if (src.value != dst.value)
                    opcode_slash_r(gobj, func, inst, 0x88, text);
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

                // Code size reduction: If immediate is sized larger than 32 bits but it's
                // value can actually fit into 32 bits, reduce size of immediate and
                // destination register to 32 bits. Prevents encoding 8 bytes of zeros for
                // zeroing a register out, for example.
                if (imm.size > 32 and imm.value <= 0xffffffff) {
                    imm.size = 32;
                    dst.size = 32;
                }

                u8 op = 0xb8 + rd_encoding(dst);
                if (dst.size == 1 or dst.size == 8)
                    op -= 8; // op = 0xb0 + rb_encoding(dst);

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
                if (reg.size == 64 or reg_topbit(reg))
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
                text += as_bytes_cap32(imm);
            } else Diag::ICE(
                "Sorry, unhandled form of move (deref rhs)\n    {}\n",
                PrintMInstImpl(inst, opcode_to_string)
            );
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

                u8 op = 0x8b;
                if (reg.size == 1 or reg.size == 8)
                    op = 0x8a;

                u8 modrm = modrm_byte(0b10, regbits(reg), regbits(RegisterId::RBP));

                if (reg.size == 16) text += prefix16;
                if (reg.size == 64 || reg_topbit(reg))
                    text += rex_byte(reg.size == 64, reg_topbit(reg), false, false);
                text += {op, modrm};
                text += as_bytes(i32(offset));
            } else Diag::ICE(
                "Sorry, unhandled form of move (deref lhs)\n    {}\n",
                PrintMInstImpl(inst, opcode_to_string)
            );
        } break;

        case Opcode::ShiftLeft:
        case Opcode::ShiftRightArithmetic: {
            // /digit where digit is:
            //     ShiftLeft:            /4
            //     ShiftRightArithmetic: /7
            //
            //       0xd2 /4  |  SAL %cl, r/m8   |  MC
            // 0x66  0xd3 /4  |  SAL %cl, r/m16  |  MC
            //       0xd3 /4  |  SAL %cl, r/m32  |  MC
            // REX.W 0xd3 /4  |  SAL %cl, r/m64  |  MC
            if (is_reg_reg(inst)) {
                auto [src, dst] = extract_reg_reg(inst);

                LCC_ASSERT(
                    src.value == +RegisterId::RCX and src.size == 8,
                    "x86_64 only supports shifting by a register value when that register is CL: got {}",
                    ToString(RegisterId(src.value), src.size)
                );

                LCC_ASSERT(
                    (is_one_of<1, 8, 16, 32, 64>(dst.size)),
                    "x86_64: Invalid register size: got {}",
                    dst.size
                );

                u8 op = 0xd3;
                if (dst.size == 1 or dst.size == 8)
                    op = 0xd2;

                u8 opcode_extension = 4;
                if (inst.opcode() == +Opcode::ShiftRightArithmetic)
                    opcode_extension = 7;
                u8 modrm = modrm_byte(0b11, opcode_extension, regbits(dst));

                if (dst.size == 16) text += prefix16;
                if (dst.size == 64 or reg_topbit(dst))
                    text += rex_byte(dst.size == 64, false, false, reg_topbit(dst));
                text += {op, modrm};
            } else Diag::ICE(
                "Sorry, unhandled form\n    {}\n",
                PrintMInstImpl(inst, opcode_to_string)
            );
        } break;

        case Opcode::LoadEffectiveAddress: {
            // GNU syntax (src, dst operands)
            //  0x66 0x8d /r | LEA m, r16 | RM
            //       0x8d /r | LEA m, r32 | RM
            // REX.W 0x8d /r | LEA m, r64 | RM
            // "RM" means source operand goes in r/m field of modrm byte and
            // destination operand goes in reg field.
            if (is_global_reg(inst)) {
                auto [global, dst] = extract_global_reg(inst);

                LCC_ASSERT(
                    (is_one_of<16, 32, 64>(dst.size)),
                    "x86_64 lea only supports 16, 32, or 64 bit register destination operand: got {}",
                    dst.size
                );

                u8 modrm = modrm_byte(0b00, regbits(dst), 0b101);

                if (dst.size == 16) text += prefix16;
                if (dst.size == 64 or reg_topbit(dst))
                    text += rex_byte(dst.size == 64, reg_topbit(dst), false, false);
                text += {0x8d, modrm};
                // Make RIP-relative disp32 relocation
                Relocation reloc{};
                reloc.symbol.byte_offset = text.contents.size();
                reloc.symbol.name = global->name();
                reloc.symbol.section_name = text.name;
                reloc.kind = Relocation::Kind::DISPLACEMENT32_PCREL;
                gobj.relocations.push_back(reloc);

                text += as_bytes(u32(0));

            } else Diag::ICE(
                "Sorry, invalid form\n    {}\n",
                PrintMInstImpl(inst, opcode_to_string)
            );
        } break;

        case Opcode::Call: {
            // 0xe8 cd | CALL rel32 | D
            // "D" means operand is encoded as literal offset.
            if (is_function(inst)) {
                auto func = extract_function(inst);

                text += {0xe8};
                // Make RIP-relative disp32 relocation
                Relocation reloc{};
                reloc.symbol.kind = Symbol::Kind::FUNCTION;
                reloc.symbol.byte_offset = text.contents.size();
                reloc.symbol.name = func->name();
                reloc.symbol.section_name = text.name;
                reloc.kind = Relocation::Kind::DISPLACEMENT32_PCREL;
                gobj.relocations.push_back(reloc);

                text += as_bytes(u32(0));

            } else Diag::ICE(
                "Sorry, unhandled form\n    {}\n",
                PrintMInstImpl(inst, opcode_to_string)
            );
        } break;

        case Opcode::Compare: {
            // GNU syntax (src, dst operands)
            //       0x38 /r | CMP r8, r/m8   | MR
            //  0x66 0x39 /r | CMP r16, r/m16 | MR
            //       0x39 /r | CMP r32, r/m32 | MR
            // REX.W 0x39 /r | CMP r64, r/m64 | MR
            // "MR" means that the source operand goes in reg field of modrm and the
            // destination operand goes in the r/m field.
            if (is_reg_reg(inst)) opcode_slash_r(gobj, func, inst, 0x38, text);
            else Diag::ICE(
                "Sorry, unhandled form\n    {}\n",
                PrintMInstImpl(inst, opcode_to_string)
            );
        } break;

        case Opcode::Test: {
            // GNU syntax (src, dst operands)
            //       0x84 /r | TEST r8, r/m8 | MR
            //  0x66 0x85 /r | TEST r8, r/m8 | MR
            //       0x85 /r | TEST r8, r/m8 | MR
            // REX.W 0x85 /r | TEST r8, r/m8 | MR
            if (is_reg_reg(inst)) opcode_slash_r(gobj, func, inst, 0x84, text);
            else Diag::ICE(
                "Sorry, unhandled form\n    {}\n",
                PrintMInstImpl(inst, opcode_to_string)
            );
        } break;

        case Opcode::SetByteIfLessUnsigned: {
            // 0x0f 0x92 | SETB r/m8 | M
            // "M" means that the operand is encoded in r/m field of modrm byte.
            setcc(0x92);
        } break;
        case Opcode::SetByteIfEqualOrGreaterUnsigned: {
            // 0x0f 0x93 | SETAE r/m8 | M
            // "M" means that the operand is encoded in r/m field of modrm byte.
            setcc(0x93);
        } break;
        case Opcode::SetByteIfEqual: {
            // 0x0f 0x94 | SETE r/m8 | M
            // "M" means that the operand is encoded in r/m field of modrm byte.
            setcc(0x94);
        } break;
        case Opcode::SetByteIfNotEqual: {
            // 0x0f 0x95 | SETNE r/m8 | M
            // "M" means that the operand is encoded in r/m field of modrm byte.
            setcc(0x95);
        } break;
        case Opcode::SetByteIfEqualOrLessUnsigned: {
            // 0x0f 0x96 | SETBE r/m8 | M
            // "M" means that the operand is encoded in r/m field of modrm byte.
            setcc(0x96);
        } break;
        case Opcode::SetByteIfGreaterUnsigned: {
            // 0x0f 0x97 | SETA r/m8 | M
            // "M" means that the operand is encoded in r/m field of modrm byte.
            setcc(0x97);
        } break;
        case Opcode::SetByteIfLessSigned: {
            // 0x0f 0x9c | SETL r/m8 | M
            // "M" means that the operand is encoded in r/m field of modrm byte.
            setcc(0x9c);
        } break;
        case Opcode::SetByteIfEqualOrGreaterSigned: {
            // 0x0f 0x9d | SETGE r/m8 | M
            // "M" means that the operand is encoded in r/m field of modrm byte.
            setcc(0x9d);
        } break;
        case Opcode::SetByteIfEqualOrLessSigned: {
            // 0x0f 0x9e | SETLE r/m8 | M
            // "M" means that the operand is encoded in r/m field of modrm byte.
            setcc(0x9e);
        } break;
        case Opcode::SetByteIfGreaterSigned: {
            // 0x0f 0x9f | SETG r/m8 | M
            // "M" means that the operand is encoded in r/m field of modrm byte.
            setcc(0x9f);
        } break;

        case Opcode::Jump: {
            // Just do 32-bit for now. Could technically do smaller jumps if we know we
            // aren't jumping far.
            // 0xe9 cd | JMP rel32 | D
            // "D" means offset is encoded after opcode.
            if (is_block(inst)) {
                auto block = extract_block(inst);

                text += 0xe9;
                // RELOCATION
                Relocation reloc{};
                reloc.symbol.byte_offset = text.contents.size();
                reloc.symbol.name = block->name();
                reloc.symbol.section_name = text.name;
                reloc.kind = Relocation::Kind::DISPLACEMENT32_PCREL;
                gobj.relocations.push_back(reloc);

                text += as_bytes(u32(0));
            } else if (is_function(inst)) {
                auto function = extract_function(inst);

                text += 0xe9;
                // RELOCATION
                Relocation reloc{};
                reloc.symbol.kind = Symbol::Kind::FUNCTION;
                reloc.symbol.byte_offset = text.contents.size();
                reloc.symbol.name = function->name();
                reloc.symbol.section_name = text.name;
                reloc.kind = Relocation::Kind::DISPLACEMENT32_PCREL;
                gobj.relocations.push_back(reloc);

                text += as_bytes(u32(0));
            } else Diag::ICE(
                "Sorry, unhandled form\n    {}\n",
                PrintMInstImpl(inst, opcode_to_string)
            );
        } break;

        case Opcode::JumpIfZeroFlag: {
            // Just do 32-bit for now. Could technically do smaller jumps if we know we
            // aren't jumping far.
            // 0x0f 0x84 cd | JZ rel32 | D
            if (is_block(inst)) {
                auto block = extract_block(inst);

                text += {0x0f, 0x84};
                // RELOCATION
                Relocation reloc{};
                reloc.symbol.kind = Symbol::Kind::FUNCTION;
                reloc.symbol.byte_offset = text.contents.size();
                reloc.symbol.name = block->name();
                reloc.symbol.section_name = text.name;
                reloc.kind = Relocation::Kind::DISPLACEMENT32_PCREL;
                gobj.relocations.push_back(reloc);

                text += as_bytes(u32(0));
            } else if (is_function(inst)) {
                auto function = extract_function(inst);

                text += {0x0f, 0x84};
                // RELOCATION
                Relocation reloc{};
                reloc.symbol.kind = Symbol::Kind::FUNCTION;
                reloc.symbol.byte_offset = text.contents.size();
                reloc.symbol.name = function->name();
                reloc.symbol.section_name = text.name;
                reloc.kind = Relocation::Kind::DISPLACEMENT32_PCREL;
                gobj.relocations.push_back(reloc);

                text += as_bytes(u32(0));
            } else Diag::ICE(
                "Sorry, unhandled form\n    {}\n",
                PrintMInstImpl(inst, opcode_to_string)
            );
        } break;

        case Opcode::Sub: {
            // GNU syntax (src, dst operands)
            //       0x28 /r | SUB r8, r/m8   | MR
            //  0x66 0x29 /r | SUB r16, r/m16 | MR
            //       0x29 /r | SUB r32, r/m32 | MR
            // REX.W 0x29 /r | SUB r64, r/m64 | MR
            if (is_reg_reg(inst)) opcode_slash_r(gobj, func, inst, 0x28, text);
            else Diag::ICE(
                "Sorry, unhandled form\n    {}\n",
                PrintMInstImpl(inst, opcode_to_string)
            );
        } break;

        case Opcode::Add: {
            if (is_reg_reg(inst)) opcode_slash_r(gobj, func, inst, 0x00, text);
            else Diag::ICE(
                "Sorry, unhandled form\n    {}\n",
                PrintMInstImpl(inst, opcode_to_string)
            );
        } break;

        case Opcode::And: {
            if (is_reg_reg(inst)) opcode_slash_r(gobj, func, inst, 0x20, text);
            else Diag::ICE(
                "Sorry, unhandled form\n    {}\n",
                PrintMInstImpl(inst, opcode_to_string)
            );
        } break;

    case Opcode::Multiply:
        case Opcode::MoveSignExtended:
        case Opcode::MoveZeroExtended:
        case Opcode::ShiftRightLogical:
            LCC_TODO("Assemble {}\n", PrintMInstImpl(inst, opcode_to_string));

        case Opcode::Poison: LCC_UNREACHABLE();
    }
}

static void assemble(GenericObject& gobj, MFunction& func, Section& text) {
    // TODO: Stack frame kinds.
    // GNU syntax (src, dst operands)
    // push %rbp
    // mov %rsp, %rbp
    auto push_rbp = MInst(usz(Opcode::Push), {0, 0});
    push_rbp.add_operand(MOperandRegister(usz(RegisterId::RBP), 64));
    auto mov_rsp_into_rbp = MInst(usz(Opcode::Move), {0, 0});
    mov_rsp_into_rbp.add_operand(MOperandRegister(usz(RegisterId::RSP), 64));
    mov_rsp_into_rbp.add_operand(MOperandRegister(usz(RegisterId::RBP), 64));
    assemble_inst(gobj, func, push_rbp, text);
    assemble_inst(gobj, func, mov_rsp_into_rbp, text);

    for (auto& block : func.blocks()) {
        gobj.symbols.push_back(
            {Symbol::Kind::STATIC,
             block.name(),
             text.name,
             text.contents.size()}
        );

        for (auto& inst : block.instructions())
            assemble_inst(gobj, func, inst, text);
    }
}

GenericObject emit_mcode_gobj(Module* module, const MachineDescription& desc, std::vector<MFunction>& mir) {
    GenericObject out{};

    Section text_{".text"};
    Section data_{".data"};
    Section bss_{".bss"};
    text_.attribute(Section::Attribute::LOAD, true);
    text_.attribute(Section::Attribute::EXECUTABLE, true);
    data_.attribute(Section::Attribute::LOAD, true);
    data_.attribute(Section::Attribute::WRITABLE, true);
    bss_.attribute(Section::Attribute::LOAD, true);
    bss_.attribute(Section::Attribute::WRITABLE, true);
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
        assemble(out, func, text);
    }

    // TODO: Resolve local label ".Lxxxx" relocations.

    return out;
}

} // namespace x86_64
} // namespace lcc
