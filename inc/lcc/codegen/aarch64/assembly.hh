#ifndef LCC_CODEGEN_AARCH64_ASSEMBLY_HH
#define LCC_CODEGEN_AARCH64_ASSEMBLY_HH

#include <lcc/codegen/mir.hh>
#include <lcc/codegen/register_allocation.hh>
#include <lcc/ir/core.hh>

#include <filesystem>
#include <vector>

namespace lcc::aarch64 {

void emit_gnu_assembly(
    const fs::path&,
    lcc::Module*,
    const MachineDescription&,
    std::vector<MFunction>&
);

} // namespace lcc::aarch64

#endif /* LCC_CODEGEN_AARCH64_ASSEMBLY_HH */
