#ifndef LCC_CODEGEN_X86_64_OBJECT_HH
#define LCC_CODEGEN_X86_64_OBJECT_HH

#include <lcc/codegen/mir.hh>
#include <lcc/codegen/register_allocation.hh>
#include <object/generic.hh>

namespace lcc {
namespace x86_64 {

GenericObject emit_mcode_gobj(Module*, const MachineDescription&, std::vector<MFunction>&);

} // namespace x86_64
} // namespace lcc

#endif /* LCC_CODEGEN_X86_64_OBJECT_HH */
