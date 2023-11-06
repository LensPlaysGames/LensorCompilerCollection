#include <lcc/codegen/x86_64.hh>

namespace lcc {
namespace x86_64 {

std::string ToString(Opcode op) {
    switch (op) {
        case Opcode::Poison: return "x86_64.poison";
        case Opcode::Return: return "ret";
        case Opcode::Move: return "mov";
    }
    LCC_UNREACHABLE();
}

std::string ToString(RegisterId id) {
    switch (id) {
        case RegisterId::INVALID: return "x86_64.INVALID";
        case RegisterId::RAX: return "rax";
        case RegisterId::RBX: return "rbx";
        case RegisterId::RCX: return "rcx";
        case RegisterId::RDX: return "rdx";
        case RegisterId::R8: return "r8";
        case RegisterId::R9: return "r9";
        case RegisterId::R10: return "r10";
        case RegisterId::R11: return "r11";
        case RegisterId::R12: return "r12";
        case RegisterId::R13: return "r13";
        case RegisterId::R14: return "r14";
        case RegisterId::R15: return "r15";
        case RegisterId::RDI: return "rdi";
        case RegisterId::RSI: return "rsi";
        case RegisterId::RBP: return "rbp";
        case RegisterId::RSP: return "rsp";
        case RegisterId::RIP: return "rip";
        case RegisterId::RETURN: return "x86_64.RETURN";
    }
    LCC_UNREACHABLE();
}

std::string ToString(RegisterId id, usz size) {
    switch (id) {
        case RegisterId::INVALID: return "x86_64.INVALID";
        case RegisterId::RAX: {
            if (size == 64) return "rax";
            if (size == 32) return "eax";
            if (size == 16) return "ax";
            if (size == 8) return "al";
            LCC_ASSERT(false, "Invalid size");
        }
        case RegisterId::RBX: {
            if (size == 64) return "rbx";
            if (size == 32) return "ebx";
            if (size == 16) return "bx";
            if (size == 8) return "bl";
            LCC_ASSERT(false, "Invalid size");
        }
        case RegisterId::RCX: {
            if (size == 64) return "rcx";
            if (size == 32) return "ecx";
            if (size == 16) return "cx";
            if (size == 8) return "cl";
            LCC_ASSERT(false, "Invalid size");
        }
        case RegisterId::RDX: {
            if (size == 64) return "rdx";
            if (size == 32) return "edx";
            if (size == 16) return "dx";
            if (size == 8) return "dl";
            LCC_ASSERT(false, "Invalid size");
        }
        case RegisterId::R8: {
            if (size == 64) return "r8";
            if (size == 32) return "r8d";
            if (size == 16) return "r8w";
            if (size == 8) return "r8b";
            LCC_ASSERT(false, "Invalid size");
        }
        case RegisterId::R9: {
            if (size == 64) return "r9";
            if (size == 32) return "r9d";
            if (size == 16) return "r9w";
            if (size == 8) return "r9b";
            LCC_ASSERT(false, "Invalid size");
        }
        case RegisterId::R10: {
            if (size == 64) return "r10";
            if (size == 32) return "r10d";
            if (size == 16) return "r10w";
            if (size == 8) return "r10b";
            LCC_ASSERT(false, "Invalid size");
        }
        case RegisterId::R11: {
            if (size == 64) return "r11";
            if (size == 32) return "r11d";
            if (size == 16) return "r11w";
            if (size == 8) return "r11b";
            LCC_ASSERT(false, "Invalid size");
        }
        case RegisterId::R12: {
            if (size == 64) return "r12";
            if (size == 32) return "r12d";
            if (size == 16) return "12w";
            if (size == 8) return "r12b";
            LCC_ASSERT(false, "Invalid size");
        }
        case RegisterId::R13: {
            if (size == 64) return "r13";
            if (size == 32) return "r13d";
            if (size == 16) return "r13w";
            if (size == 8) return "r13b";
            LCC_ASSERT(false, "Invalid size");
        }
        case RegisterId::R14: {
            if (size == 64) return "r14";
            if (size == 32) return "r14d";
            if (size == 16) return "r14w";
            if (size == 8) return "r14b";
            LCC_ASSERT(false, "Invalid size");
        }
        case RegisterId::R15: {
            if (size == 64) return "r15";
            if (size == 32) return "r15w";
            if (size == 16) return "r15d";
            if (size == 8) return "r15b";
            LCC_ASSERT(false, "Invalid size");
        }
        case RegisterId::RDI: {
            if (size == 64) return "rdi";
            if (size == 32) return "edi";
            if (size == 16) return "di";
            LCC_ASSERT(false, "Invalid size");
        }
        case RegisterId::RSI: {
            if (size == 64) return "rsi";
            if (size == 32) return "esi";
            if (size == 16) return "si";
            LCC_ASSERT(false, "Invalid size");
        }
        case RegisterId::RBP: {
            if (size == 64) return "rbp";
            if (size == 32) return "ebp";
            if (size == 16) return "bp";
            LCC_ASSERT(false, "Invalid size");
        }
        case RegisterId::RSP: {
            if (size == 64) return "rsp";
            if (size == 32) return "esp";
            if (size == 16) return "sp";
            LCC_ASSERT(false, "Invalid size");
        }
        case RegisterId::RIP: {
            if (size == 64) return "rip";
            if (size == 32) return "eip";
            if (size == 16) return "ip";
            LCC_ASSERT(false, "Invalid size");
        }
        case RegisterId::RETURN: return "x86_64.RETURN";
    }
    LCC_UNREACHABLE();
}

} // namespace x86_64
} // namespace lcc
