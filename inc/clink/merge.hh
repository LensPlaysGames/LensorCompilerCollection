#ifndef CLINK_MERGE_HH
#define CLINK_MERGE_HH

#include <object/generic.hh>

#include <vector>

namespace clink {

// Merge alike sections, build global symbol table (out.symbols)
// - Concatenate .text, .data, etc (shared names)
// - Increase size of .bss
// - Merge Symbol Tables, ensuring no duplicate definitions
// - Collect global relocations
// - Minor: don't duplicate exact .comment sections
bool merge(std::vector<lcc::GenericObject>& parsed_objects, lcc::GenericObject& out);

} // namespace clink

#endif /* CLINK_MERGE_HH */
