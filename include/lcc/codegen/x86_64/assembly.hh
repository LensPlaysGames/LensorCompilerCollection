#ifndef LCC_CODEGEN_X86_64_ASSEMBLY_HH
#define LCC_CODEGEN_X86_64_ASSEMBLY_HH

#include <lcc/codegen/mir.hh>
#include <lcc/codegen/register_allocation.hh>
#include <lcc/utils.hh>

#include <filesystem>
#include <vector>

namespace lcc::x86_64 {

void emit_gnu_att_assembly(
    const fs::path&,
    lcc::Module*,
    const MachineDescription&,
    std::vector<MFunction>&
);

} // namespace lcc::x86_64

#endif /* LCC_CODEGEN_X86_64_ASSEMBLY_HH */
