#include <lcc/codegen/generic_object.hh>
#include <lcc/codegen/mir.hh>
#include <lcc/codegen/x86_64.hh>
#include <lcc/codegen/x86_64/object.hh>
#include <lcc/context.hh>
#include <lcc/utils.hh>
#include <variant>

namespace lcc {
namespace x86_64 {

// NOTE: +rw indicates the lower three bits of the opcode byte are used
// to indicate the 16-bit register operand.
// For registers R8 through R15, the REX.b bit also needs set.
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

// NOTE: +rw indicates the lower three bits of the opcode byte are used
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
static constexpr u8 rd_encoding(RegisterId id) {
    return rw_encoding(id);
}

// NOTE: +rw indicates the lower three bits of the opcode byte are used
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
static constexpr u8 rb_encoding(RegisterId id) {
    return rw_encoding(id);
}

// Don't use me directly!
static constexpr u8 rex_byte(bool w, bool r, bool x, bool b) {
    return u8(0b01000000 | ((w ? 1 : 0) << 3) | ((r ? 1 : 0) << 2) | ((x ? 1 : 0) << 1) | (b ? 1 : 0));
}
/// REX.W prefix is commonly used to promote a 32-bit operation to 64-bit.
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

static constexpr bool regbits_top(u8 bits) {
    return bits & 0b1000;
}
static constexpr bool reg_topbit(RegisterId id) {
    return regbits_top(regbits(id));
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
template <typename Op1, typename Op2>
bool is_two_operand(MInst& inst) {
    // clang-format off
    return inst.all_operands().size() == 2
           and std::holds_alternative<Op1>(inst.get_operand(0))
           and std::holds_alternative<Op2>(inst.get_operand(1));
    // clang-format on
}

bool is_reg_imm(MInst& inst) {
    return is_two_operand<MOperandRegister, MOperandImmediate>(inst);
}
bool is_reg_reg(MInst& inst) {
    return is_two_operand<MOperandRegister, MOperandRegister>(inst);
}
bool is_reg_local(MInst& inst) {
    return is_two_operand<MOperandRegister, MOperandLocal>(inst);
}
bool is_imm_imm(MInst& inst) {
    return is_two_operand<MOperandImmediate, MOperandImmediate>(inst);
}
bool is_imm_reg(MInst& inst) {
    return is_two_operand<MOperandImmediate, MOperandRegister>(inst);
}
bool is_imm_local(MInst& inst) {
    return is_two_operand<MOperandImmediate, MOperandLocal>(inst);
}
bool is_local_imm(MInst& inst) {
    return is_two_operand<MOperandLocal, MOperandImmediate>(inst);
}
bool is_local_reg(MInst& inst) {
    return is_two_operand<MOperandLocal, MOperandRegister>(inst);
}

static void assemble_inst(MInst& inst, Section& text) {
    (void) inst;
    (void) text;
}

static void assemble(MFunction& func, Section& text) {
    for (auto& block : func.blocks()) {
        for (auto& inst : block.instructions()) {
            assemble_inst(inst, text);
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
    Section& data = out.section(".data");
    Section& bss = out.section(".bss");

    for (auto* var : module->vars())
        out.symbol_from_global(var);

    for (auto& func : mir) {
        const bool exported = func.linkage() == Linkage::Exported || func.linkage() == Linkage::Reexported;
        const bool imported = func.linkage() == Linkage::Imported || func.linkage() == Linkage::Reexported;

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
