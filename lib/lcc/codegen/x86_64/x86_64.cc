#include <lcc/codegen/x86_64/x86_64.hh>

#include <lcc/codegen/mir.hh>

namespace lcc::x86_64 {

std::string opcode_to_string(usz opcode) {
    if (opcode >= +MInst::Kind::ArchStart)
        return std::string{ToString(static_cast<Opcode>(opcode))};
    return MInstOpcodeToString(opcode);
}
} // namespace lcc::x86_64
