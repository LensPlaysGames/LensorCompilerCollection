#include <lcc/codegen/x86_64.hh>

namespace lcc {
namespace x86_64 {

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
        case RegisterId::RETURN: return "x86_64.RETURN";
    }
    LCC_UNREACHABLE();
}

} // namespace x86_64
} // namespace lcc
