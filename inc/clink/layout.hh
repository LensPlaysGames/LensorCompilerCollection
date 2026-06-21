#ifndef CLINK_LAYOUT_HH
#define CLINK_LAYOUT_HH

#include <object/generic.hh>

#include <string>
#include <string_view>
#include <vector>

namespace clink {
struct SectionDescription {
    std::string_view name{};
    uint32_t size{};
};

struct MemorySegment {
    // The permissions/attributes related to this memory segment.
    // i.e. whether or not it's writable, executable, etc.
    decltype(lcc::Section::attributes) attributes{};
    // Memory address where this segment resides.
    uint64_t address{};
    // Sections contained within this memory segment.
    std::vector<SectionDescription> sections{};

    auto size() const -> uint64_t {
        uint64_t out{};
        for (const auto& s : sections)
            out += s.size;
        return out;
    }
};

struct Layout {
    std::vector<MemorySegment> segments{};

    void place_section(const lcc::Section&);
    // Once no more sections are going to be added to any segments, we can
    // assign addresses to each memory segment, starting at @param dot.
    // Align the start of every memory segment to @param align.
    void address_assignment(uint64_t dot, uint32_t align);

    // Get the address of a given section (by name). Returns garbage if you
    // have not yet assigned addresses for the layout (since placing sections).
    auto address(std::string_view section_name) const -> decltype(MemorySegment::address);
    auto address(const lcc::Section& section) const -> decltype(MemorySegment::address) {
        return address(section.name);
    }
};

auto layout(const lcc::GenericObject&) -> Layout;

} // namespace clink

#endif /* CLINK_LAYOUT_HH */
