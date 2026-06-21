#include "lcc/utils.hh"
#include <clink/layout.hh>

#include <string>
#include <string_view>
#include <vector>

namespace clink {

void Layout::place_section(const lcc::Section& section) {
    // Only loaded sections get placed in memory.
    if (not section.attribute(lcc::Section::Attribute::LOAD))
        return;

    for (auto& s : segments) {
        // NOTE: Is this the right way to compare attributes?
        if (s.attributes == section.attributes) {
            s.sections.emplace_back(section.name, section.size());
            return;
        }
    }
    MemorySegment new_segment{};
    new_segment.attributes = section.attributes;
    new_segment.sections.emplace_back(section.name, section.size());
    segments.emplace_back(new_segment);
};

void Layout::address_assignment(uint64_t dot, uint32_t page_size) {
    // Align dot.
    for (auto& s : segments) {
        dot = lcc::utils::AlignTo(dot, (decltype(dot)) page_size);
        s.address = dot;
        dot += s.size();
    }
}

auto Layout::address(std::string_view section_name) const
    -> decltype(MemorySegment::address) {
    for (auto& segment : segments) {
        decltype(MemorySegment::address) base = segment.address;
        for (const auto& section : segment.sections) {
            if (section.name == section_name)
                return base;
            base += section.size;
        }
    }
    return (decltype(MemorySegment::address)) -1;
}

auto layout(const lcc::GenericObject& object) -> Layout {
    Layout memory_layout{};
    for (auto& s : object.sections)
        memory_layout.place_section(s);

    // TODO: Target page size for alignment.
    memory_layout.address_assignment(0x00400000, 0x1000);

    return memory_layout;
}

} // namespace clink
