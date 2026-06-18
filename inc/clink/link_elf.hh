#ifndef CLINK_LINK_ELF_HH
#define CLINK_LINK_ELF_HH

#include <object/generic.hh>

#include <span>

namespace clink {
// Collect defined symbols and relocations from blob, treating it as a
// binary ELF file.
[[nodiscard]]
lcc::GenericObject collect_elf(std::span<char> blob);
}

#endif /* CLINK_LINK_ELF_HH */
