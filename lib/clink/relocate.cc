#include <clink/relocate.hh>

#include <clink/layout.hh>

#include <object/generic.hh>

#include <lccbase/context.hh>
#include <lcc/target.hh>
#include <lcc/typedefs.hh>

#include <algorithm>
#include <bit>
#include <iterator>
#include <limits>
#include <string>
#include <vector>

namespace clink {

namespace {

void perform_relocation__abs64(
    char* relocation_position,
    const lcc::Relocation& relocation,
    const lcc::Symbol& resolved_symbol,
    // base address of the section resolved_symbol is defined within
    uint64_t base
) {
    uint64_t calculated_value = base
                              + resolved_symbol.byte_offset
                              + (lcc::usz) relocation.addend;
    // Always write little endian
    if constexpr (std::endian::native == std::endian::big)
        calculated_value = std::byteswap(calculated_value);
    memcpy(relocation_position, &calculated_value, sizeof(calculated_value));
}

void perform_relocation__abs32(
    char* relocation_position,
    const lcc::Relocation& relocation,
    const lcc::Symbol& resolved_symbol,
    // base address of the section resolved_symbol is defined within
    uint32_t base
) {
    uint32_t calculated_value = base
                              + (uint32_t) resolved_symbol.byte_offset
                              + (uint32_t) relocation.addend;
    // Always write little endian
    if constexpr (std::endian::native == std::endian::big)
        calculated_value = std::byteswap(calculated_value);
    memcpy(relocation_position, &calculated_value, sizeof(calculated_value));
}

void perform_relocation__rel64(
    char* relocation_position,
    const lcc::Relocation& relocation,
    const lcc::Symbol& resolved_symbol,
    uint64_t symbol_section_offset,
    uint64_t relocation_section_offset
) {
    uint64_t S = symbol_section_offset
               + (uint64_t) resolved_symbol.byte_offset;
    uint64_t A = (uint64_t) relocation.addend;
    uint64_t P = relocation_section_offset
               + (uint64_t) relocation.symbol.byte_offset;

    fmt::print(
        "Performing DISP32_PCREL (S + A - P) `{}` ({} + {:+} - {})\n",
        resolved_symbol.name,
        S,
        (int64_t) A,
        P
    );

    uint64_t calculated_value = S + A - P;

    // Always write little endian
    if constexpr (std::endian::native == std::endian::big)
        calculated_value = std::byteswap(calculated_value);

    memcpy(relocation_position, &calculated_value, sizeof(calculated_value));
}

void perform_relocation__rel32(
    char* relocation_position,
    const lcc::Relocation& relocation,
    const lcc::Symbol& resolved_symbol,
    // This is the virtual address of the *start* of the section where the
    // resolved symbol is defined.
    uint64_t symbol_section_address,
    // This is the virtual address of the *start* of the section where the
    // relocation is being performed.
    uint64_t relocation_section_address
) {
    uint32_t S = (uint32_t) symbol_section_address
               + (uint32_t) resolved_symbol.byte_offset;

    uint32_t A = (uint32_t) relocation.addend;

    uint32_t P = (uint32_t) relocation_section_address
               + (uint32_t) relocation.symbol.byte_offset;

    // fmt::print(
    //     "Performing DISP32_PCREL (S + A - P) `{}` ({} + {:+} - {}) = {}\n",
    //     resolved_symbol.name,
    //     S,
    //     (int32_t) A,
    //     P,
    //     S + A - P
    // );

    uint32_t calculated_value = S + A - P;

    // Always write little endian
    if constexpr (std::endian::native == std::endian::big)
        calculated_value = std::byteswap(calculated_value);

    memcpy(relocation_position, &calculated_value, sizeof(calculated_value));
}

template <typename T>
bool fits(auto v) {
    return v <= std::numeric_limits<T>::max();
}

} // namespace

bool perform_relocation(
    lcc::Context& context,
    std::vector<char>& blob,
    const lcc::GenericObject& object,
    const lcc::Relocation& relocation,
    std::vector<std::string>& global_offset_table,
    const Layout& layout
) {
#define CLINK_FAIL_RELOC_STR "clink: cannot perform relocation: "
    auto found_sym = object.find_symbol(relocation.symbol.name);
    if (not found_sym) {
        // NOTE: Not an undefined reference because we may have just not found
        // it *as of yet*.
        if (context.has_option("verbose"))
            fmt::print(CLINK_FAIL_RELOC_STR "didn't find symbol {}\n", relocation.symbol.name);
        return false;
    }
    auto& found = found_sym->get();

    // Wait for global symbol resolution pass...
    // Without the merged global object file, we can't perform relocations
    // across object files.
    if (found.kind == lcc::Symbol::Kind::EXTERNAL) {
        if (context.has_option("verbose"))
            fmt::print(CLINK_FAIL_RELOC_STR "undefined symbol {}\n", found.print());
        return false;
    }

    // This is the section we are going to perform the relocation within.
    auto& relevant_section = object.section(relocation.symbol.section_name);

    // The offset of the section that we are performing the relocation within.
    auto relocation_section_offset = layout.section_offset(
        relocation.symbol.section_name
    );
    if (relocation_section_offset == 0) {
        fmt::print(
            stderr,
            CLINK_FAIL_RELOC_STR
            "relocation references symbol `{}` that resides in section `{}` with zero recorded section offset\n",
            relocation.symbol.name,
            relocation.symbol.section_name
        );
        return false;
    }
    auto relocation_position_offset
        = relocation_section_offset + relocation.symbol.byte_offset;

    auto* relocation_position
        = blob.data() + relocation_position_offset;

    if (not relevant_section.attribute(lcc::Section::Attribute::LOAD)) {
        // relocations within unloaded sections don't _need_ to be performed :)
        // if (context.has_option("verbose"))
        //     fmt::print(CLINK_FAIL_RELOC_STR "ignoring relocation within unloaded section: {}", found.print());
        return true;
    }

    // fmt::print("{}", relocation.print());
    // fmt::print(
    //     "  offset of section `{}` at {}\n",
    //     relocation.symbol.section_name,
    //     relocation_position_offset
    // );
    // fmt::print("  writing to offset at 0x{:08x}\n", relocation_position_offset);

    { // Confidence
        if (relevant_section.is_fill) {
            fmt::print(
                stderr,
                CLINK_FAIL_RELOC_STR
                "relocation references symbol `{}` that resides in is_fill section (cannot relocate)\n",
                relocation.symbol.name
            );
            return false;
        }
        // Ensure contents is big enough, or something
        // TODO: uint32_t will change based on machine/relocation kind pair.
        if (relocation_position_offset + sizeof(uint32_t) > blob.size()) {
            fmt::print(
                stderr,
                CLINK_FAIL_RELOC_STR
                "relocation offset {} for `{}` out-of-bounds of section `{}` contents (size:{})\n",
                relocation_position_offset,
                relocation.symbol.name,
                relevant_section.name,
                relevant_section.size()
            );
            return false;
        }
    }

    uint64_t relocation_section_address = layout.address(
        relocation.symbol.section_name
    );

    uint64_t symbol_definition_section_address{};
    if (found.section_name.size()) {
        symbol_definition_section_address = layout.address(
            found.section_name
        );
    } else {
        // TODO: linker defined symbols end up here.
        // fmt::print(
        //     stderr,
        //     "clink: suspicious symbol `{}`: not within any section...\n",
        //     found.name
        // );
    }

    switch (relocation.kind) {
        case lcc::Relocation::Kind::NONE:
            break;

        case lcc::Relocation::Kind::DISPLACEMENT64: {
            perform_relocation__abs64(
                relocation_position,
                relocation,
                found,
                layout.address(found.section_name)
            );
        } break;

        case lcc::Relocation::Kind::DISPLACEMENT32: {
            auto base_address = layout.address(found.section_name);
            if (not fits<uint32_t>(base_address)) {
                lcc::Diag::ICE(
                    CLINK_FAIL_RELOC_STR
                    "oversized address {:x} for disp32 relocation of {} within {}\n",
                    base_address,
                    found.name,
                    relevant_section.name
                );
            }
            perform_relocation__abs32(relocation_position, relocation, found, (uint32_t) base_address);
        } break;

        case lcc::Relocation::Kind::DISPLACEMENT32_PCREL: {
            perform_relocation__rel32(relocation_position, relocation, found, symbol_definition_section_address, relocation_section_address);
        } break;

        case lcc::Relocation::Kind::DISPLACEMENT32_GOTPCREL: {
            // Always try to relax first
            // Relaxing involves the linker rewriting assembled machine code to
            // perform optimisations. For example, in a static linking situation,
            // sometimes the compiler is linking in libraries that were compiled as
            // position independent code. That code couldn't make assumptions about
            // how external symbols were going to be resolved (i.e. dynamic vs
            // static), so they went with the safe route and emitted instructions that
            // load a pointer from a section in memory, and then use that pointer as
            // the symbol address. The problem lies in that it is needlessly
            // inefficient to store a fixed pointer into the binary at link time when
            // we could just rewrite the `mov` into a `lea` and be done with it.
            // NOTE: Trickiness due to possible REX prefix
            // - `48 8b 3d 00 00 00 00` -> `48 8d 3d 00 00 00 00`
            //   Change 8b to 8d (Transforms mov to lea).
            // - Replace `ff 15 00 00 00 00` with `e8 00 00 00 00 90`
            //   Change ff 15 to e8 (Transforms indirect far call to direct relative call)
            // TODO: When relaxing, we may? want to remove the entry from the global
            // offset table entirely, if no other relocations reference it... We could
            // just do that in a separate pass, though.
            if (
                context.has_option("clink-relax")
                and context.target()->is_arch_x86_64()
                and relocation.symbol.byte_offset >= 2
            ) {
                if (*(relocation_position - 2) == (char) 0x8b) {
                    // Update opcode
                    *(relocation_position - 2) = (char) 0x8d;

                    perform_relocation__rel32(
                        relocation_position,
                        relocation,
                        found,
                        symbol_definition_section_address,
                        relocation_section_address
                    );

                    fmt::print(
                        "clink: Relaxed load from GOT of address of {} to lea\n",
                        found.name
                    );

                    return true;
                }
                if (
                    *(relocation_position - 2) == (char) 0xff
                    and *(relocation_position - 1) == (char) 0x15 // indirect call
                ) {
                    // ff 15 -> e8  moves address over by one
                    *(relocation_position - 2) = (char) 0xe8; // relative call
                    *(relocation_position + 3) = (char) 0x90; // NOP

                    auto adjusted_relocation = relocation;
                    --adjusted_relocation.symbol.byte_offset;
                    perform_relocation__rel32(
                        relocation_position,
                        adjusted_relocation,
                        found,
                        symbol_definition_section_address,
                        relocation_section_address
                    );

                    // If there was a REX prefix, we have to get rid of it. There should never
                    // be one, no modern compiler would ever emit this, but, it's technically
                    // possible.
                    if (
                        relocation.symbol.byte_offset >= 3
                        and *(relocation_position - 3) >= (char) 0x40
                        and *(relocation_position - 3) <= (char) 0x4f
                    ) *(relocation_position - 3) = (char) 0x90;

                    fmt::print(
                        "clink: Relaxed indirect call to {} to direct call + NOP\n",
                        found.name,
                        symbol_definition_section_address,
                        relocation_section_address
                    );
                    return true;
                }
                if (
                    *(relocation_position - 2) == (char) 0xff
                    and *(relocation_position - 1) == (char) 0x25 // indirect jmp
                ) {
                    // ff 25 -> e9  moves address over by one
                    *(relocation_position - 2) = (char) 0xe9; // direct jmp
                    *(relocation_position + 3) = (char) 0x90; // NOP

                    auto adjusted_relocation = relocation;
                    --adjusted_relocation.symbol.byte_offset;

                    perform_relocation__rel32(
                        relocation_position,
                        adjusted_relocation,
                        found,
                        symbol_definition_section_address,
                        relocation_section_address
                    );

                    // If there was a REX prefix, we have to get rid of it.
                    if (
                        relocation.symbol.byte_offset >= 3
                        and *(relocation_position - 3) >= (char) 0x40
                        and *(relocation_position - 3) <= (char) 0x4f
                    ) *(relocation_position - 3) = (char) 0x90;

                    fmt::print(
                        "clink: Relaxed indirect jmp to {} to direct jmp + NOP\n",
                        found.name
                    );

                    return true;
                }
            }

            // fmt::print("Laying out GOT for `{}`\n", relocation.symbol.name);

            // RIP-relative between sections. Basically, we need to take the
            // difference between the relevant section's memory addresses, and then
            // add to that the difference between their byte offsets.

            auto got_iterator = std::ranges::find(global_offset_table, relocation.symbol.name);
            uint32_t got_slot_index = (uint32_t) std::distance(global_offset_table.begin(), got_iterator);
            uint32_t got_slot_address
                = (uint32_t) layout.address(".got")
                + got_slot_index
                      * ((uint32_t) context.target()->size_of_pointer
                         / (uint32_t) context.target()->ffi.size_of_char);
            uint32_t relocation_address = (uint32_t) layout.address(relevant_section.name)
                                        + (uint32_t) relocation.symbol.byte_offset;

            // fmt::print("  .got at 0x{:08x}\n", layout.address(".got"));
            // fmt::print("  slot at 0x{:08x}\n", got_slot_address);
            // fmt::print("  relocation at 0x{:08x}\n", relocation_address);

            uint32_t calculated_value = (uint32_t) ((got_slot_address
                                                     + (lcc::usz) relocation.addend)
                                                    - relocation_address);

            // Always write little endian bytes
            if constexpr (std::endian::native == std::endian::big)
                calculated_value = std::byteswap(calculated_value);

            memcpy(relocation_position, &calculated_value, sizeof(calculated_value));
            return true;
        }
    }
#undef CLINK_FAIL_RELOC_STR

    return true;
}

} // namespace clink
