#ifndef LCC_CODEGEN_X86_64_ASSEMBLY_HH
#define LCC_CODEGEN_X86_64_ASSEMBLY_HH

#include <lcc/codegen/mir.hh>

namespace lcc {
namespace x86_64 {

void emit_gnu_att_assembly(std::filesystem::path, Module*, std::vector<MFunction>&);

} // namespace x86_64
} // namespace lcc

#endif /* LCC_CODEGEN_X86_64_ASSEMBLY_HH */
