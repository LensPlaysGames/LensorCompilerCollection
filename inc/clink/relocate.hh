#ifndef CLINK_RELOCATE_HH
#define CLINK_RELOCATE_HH

#include <clink/layout.hh>

#include <object/generic.hh>

#include <lcc/context.hh>

#include <string>
#include <vector>

namespace clink {

bool perform_relocation(
    lcc::Context& context,
    std::vector<char>& blob,
    const lcc::GenericObject& object,
    const lcc::Relocation& relocation,
    std::vector<std::string>& global_offset_table,
    const Layout& layout
);

} // namespace clink

#endif /* CLINK_RELOCATE_HH */
