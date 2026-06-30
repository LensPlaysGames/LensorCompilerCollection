#include <object/coff.h>
#include <object/generic.hh>

#include <lccbase/assert.hh>

#include <algorithm>
#include <array>
#include <cstdint>
#include <cstdio>
#include <cstring>
#include <limits>
#include <numeric>
#include <string>
#include <vector>

static_assert(sizeof(coff_header) == 20);
static_assert(sizeof(coff_section_header) == 40);

namespace lcc {
namespace {

constexpr auto default_header() -> coff_header {
    coff_header hdr{};
    hdr.machine = IMAGE_FILE_MACHINE_AMD64;
    hdr.characteristics |= IMAGE_FILE_LINE_NUMBERS_STRIPPED;
    hdr.characteristics |= IMAGE_FILE_DEBUG_STRIPPED;
    return hdr;
}

} // namespace

auto GenericObject::as_coff(
    clink::Layout& layout,
    EmitRelocations _emit_relocations
) -> std::vector<char> {
    const bool emit_relocations = +_emit_relocations;

    auto coff_header = default_header();

    std::vector<std::string> strings{};
    uint32_t strings_length{4};
    const auto coff_string = [&](std::string_view in) {
        auto string_table_index = strings.size();
        strings.emplace_back(in);
        // +1 -> null terminator
        strings_length += (uint32_t) in.size() + 1;
        return string_table_index;
    };
    const auto coff_name = [&](std::string_view name) -> std::array<char, 8> {
        std::array<char, 8> out{};
        if (name.length() <= 8) {
            std::ranges::copy(name, out.begin());
            return out;
        }

        uint32_t string_table_offset = strings_length;
        std::memcpy(out.data() + 4, &string_table_offset, 4);
        (void) coff_string(name);
        return out;
    };
    const auto section_position_by_name
        = [&](std::string_view name) -> int16_t {
        for (auto [i, s] : std::ranges::views::enumerate(sections)) {
            if (s.name == name)
                return (int16_t) i + 1;
        }
        Diag::ICE(
            "Generic2COFF: Could not find section {}",
            name
        );
    };

    // NOTE: (!) 1-based
    int16_t code_section_position{};
    for (auto [i, s] : std::ranges::views::enumerate(sections)) {
        if (s.attribute(Section::Attribute::EXECUTABLE)) {
            code_section_position = (int16_t) i + 1;
            break;
        }
    }
    LCC_ASSERT(
        code_section_position,
        "Generic2COFF: Could not find code section"
    );

    std::vector<coff_symbol_entry> coff_symbols{};
    for (const auto& sym : symbols) {
        switch (sym.kind) {
            case Symbol::Kind::NONE:
                LCC_UNREACHABLE();

            case Symbol::Kind::EXTERNAL: {
                coff_symbol_entry coff_sym{};
                coff_sym.section = N_UNDEF;

                auto sym_name = coff_name(sym.name);
                std::ranges::copy(sym_name, coff_sym.name);

                coff_sym.storage_class = IMAGE_SYM_CLASS_EXTERNAL;

                coff_symbols.emplace_back(coff_sym);
            } break;

            case Symbol::Kind::FUNCTION: {
                coff_symbol_entry coff_sym{};
                coff_sym.section = code_section_position;

                auto sym_name = coff_name(sym.name);
                std::ranges::copy(sym_name, coff_sym.name);

                coff_sym.storage_class = IMAGE_SYM_CLASS_STATIC;

                coff_sym.type = 0x20;

                coff_sym.section = code_section_position;

                LCC_ASSERT(
                    sym.byte_offset <= std::numeric_limits<uint32_t>::max(),
                    "Symbol byte offset cannot be encoded in COFF format"
                );
                coff_sym.value = (uint32_t) sym.byte_offset;

                coff_symbols.emplace_back(coff_sym);

                // TODO: .bf, .lf, .ef sections

                // Function definition aux entry
                coff_symbol_aux_function_definition_entry coff_aux{};
                static_assert(sizeof(coff_aux) == sizeof(coff_symbol_entry));
                // TODO: Symbol index of `.bf` (begin function) entry
                coff_aux.tag_index = 0;
                // TODO: Size of assembled code for function
                coff_aux.total_size = 0;
                // Record aux entry
                memcpy(&coff_sym, &coff_aux, sizeof(coff_sym));
                coff_symbols.emplace_back(coff_sym);
            } break;

            case Symbol::Kind::STATIC: {
                coff_symbol_entry coff_sym{};

                auto sym_name = coff_name(sym.name);
                std::ranges::copy(sym_name, coff_sym.name);

                coff_sym.storage_class = IMAGE_SYM_CLASS_STATIC;

                coff_sym.section = section_position_by_name(sym.section_name);

                LCC_ASSERT(
                    sym.byte_offset <= std::numeric_limits<uint32_t>::max(),
                    "Symbol byte offset cannot be encoded in COFF format"
                );
                coff_sym.value = (uint32_t) sym.byte_offset;

                coff_symbols.emplace_back(coff_sym);
            } break;

            case Symbol::Kind::WEAK: {
                coff_symbol_entry coff_sym{};

                auto sym_name = coff_name(sym.name);
                std::ranges::copy(sym_name, coff_sym.name);

                coff_sym.storage_class = IMAGE_SYM_CLASS_WEAK_EXTERNAL;
                coff_sym.section = N_UNDEF;

                LCC_ASSERT(
                    sym.byte_offset <= std::numeric_limits<uint32_t>::max(),
                    "Symbol byte offset cannot be encoded in COFF format"
                );
                coff_sym.value = (uint32_t) sym.byte_offset;

                coff_symbols.emplace_back(coff_sym);

                // Record aux symbol entry for weak external...
                coff_symbol_aux_weak_external_entry coff_aux{};

                // TODO: This needs to point to the definition of the symbol that will be
                // used if this symbol doesn't get defined strongly.
                // coff_aux.tag_index = 0;
                static_assert(sizeof(coff_aux) == sizeof(coff_sym));
                memcpy(&coff_sym, &coff_aux, sizeof(coff_sym));

                LCC_TODO(
                    "Generic2COFF: Handle weak symbol\n"
                );
            } break;

            case Symbol::Kind::EXPORT: {
                coff_symbol_entry coff_sym{};

                auto sym_name = coff_name(sym.name);
                std::ranges::copy(sym_name, coff_sym.name);

                coff_sym.storage_class = IMAGE_SYM_CLASS_STATIC;

                coff_sym.section = section_position_by_name(sym.section_name);

                LCC_ASSERT(
                    sym.byte_offset <= std::numeric_limits<uint32_t>::max(),
                    "Symbol byte offset cannot be encoded in COFF format"
                );
                coff_sym.value = (uint32_t) sym.byte_offset;

                coff_symbols.emplace_back(coff_sym);

                LCC_TODO(
                    "Generic2COFF: Handle exported symbol\n"
                );
            } break;
        }
    }

    coff_header.number_of_sections = (uint16_t) sections.size();
    // TODO: aux entries
    coff_header.number_of_symbols = (uint32_t) coff_symbols.size();

    uint32_t total_section_data_size = std::ranges::fold_left(
        std::ranges::views::transform(
            sections,
            [](const auto& s) -> uint32_t {
                // Fill sections (uninitialized) don't get stored in the file.
                // FIXME: We should probably specially handle sections with non-zero fill
                // values here.
                if (s.is_fill) return 0;
                return (uint32_t) s.contents().size();
            }
        ),
        0,
        std::plus{}
    );
    coff_header.symbol_table_offset
        = (uint32_t) (sizeof(coff_header)
                      + (sizeof(coff_section_header) * coff_header.number_of_sections)
                      + total_section_data_size);

    std::vector<coff_section_header> section_headers{};
    section_headers.reserve(sections.size());

    auto data_offset
        = (uint32_t) (sizeof(coff_header)
                      + (sizeof(coff_section_header) * coff_header.number_of_sections));
    for (const auto& s : sections) {
        coff_section_header section_header{};

        auto section_name = coff_name(s.name);
        std::memcpy(
            &section_header.name,
            section_name.data(),
            section_name.size()
        );

        if (s.is_fill) {
            LCC_ASSERT(
                s.value() == 0,
                "Generic2COFF: Handle fill section with non-zero value"
            );
            section_header.virtual_size = s.length();
        } else {
            section_header.size_in_bytes = (uint32_t) s.contents().size();
            section_header.data_offset = data_offset;
            data_offset += section_header.size_in_bytes;
        }

        section_header.flags |= IMAGE_SCN_MEM_READ;

        // TODO: static assert for section attribute handling
        if (s.attribute(Section::Attribute::EXECUTABLE)) {
            section_header.flags |= IMAGE_SCN_CNT_CODE;
            section_header.flags |= IMAGE_SCN_MEM_EXECUTE;
        }

        if (s.attribute(Section::Attribute::WRITABLE))
            section_header.flags |= IMAGE_SCN_MEM_WRITE;

        if (not s.attribute(Section::Attribute::LOAD))
            section_header.flags |= IMAGE_SCN_LNK_REMOVE;

        section_headers.emplace_back(section_header);
    }

    // Now that all the section's sizes are known, we know the offsets of
    // section's relocation entries.
    // Map of section position (1-based position within index table) to
    // a list of associated relocation entries.
    std::unordered_map<
        int16_t,
        std::vector<coff_relocation_entry>>
        coff_relocations{};
    if (emit_relocations) {
        for (auto r : relocations) {
            coff_relocation_entry coff_relocation{};
            switch (r.kind) {
                case Relocation::Kind::NONE:
                    LCC_UNREACHABLE();

                case Relocation::Kind::DISPLACEMENT64:
                    coff_relocation.type = IMAGE_REL_AMD64_ADDR64;
                    break;
                case Relocation::Kind::DISPLACEMENT32:
                    coff_relocation.type = IMAGE_REL_AMD64_ADDR32;
                    break;
                case Relocation::Kind::DISPLACEMENT32_PCREL:
                    coff_relocation.type = IMAGE_REL_AMD64_REL32;
                    break;
                case Relocation::Kind::DISPLACEMENT32_GOTPCREL:
                    Diag::ICE("Unhandled relocation type in COFF output format (related to Global Offset Table)");
                    break;
            }

            // Find symbol with matching name.
            auto found = std::ranges::find_if(
                symbols,
                [&](const auto& sym) {
                    return sym.name == r.symbol.name;
                }
            );
            if (found == symbols.end()) {
                Diag::ICE(
                    "Could not find symbol {} referenced by relocation",
                    r.symbol.name
                );
            }
            auto sym_index = found - symbols.begin();
            coff_relocation.reference_address
                = (int32_t) ((isz) r.symbol.byte_offset + r.addend);
            coff_relocation.symbol_index = (uint32_t) sym_index;

            // Find section symbol is defined in.
            int16_t symbol_section_position
                = section_position_by_name(r.symbol.section_name);

            coff_relocations[symbol_section_position].emplace_back(coff_relocation);
        }
    }

    uint32_t string_table_size = (uint32_t) std::ranges::fold_left(
        std::ranges::views::transform(
            strings,
            [](const auto& s) { return s.size() + 1; }
        ),
        4, // empty COFF string table is 4 bytes long...
        std::plus{}
    );

    if (emit_relocations) {
        // 18 -> sizeof coff_symbol_entry (without padding)
        auto relocation_offset = coff_header.symbol_table_offset
                               + (18 * coff_header.number_of_symbols)
                               + string_table_size;
        for (auto [i, s] : vws::enumerate(section_headers)) {
            int16_t s_position = (int16_t) i + 1;
            if (not coff_relocations.contains(s_position))
                continue;

            auto& this_sections_coff_relocations = coff_relocations.at(s_position);
            s.relocation_table_offset = relocation_offset;
            s.number_of_relocation_entries = (uint16_t) this_sections_coff_relocations.size();
            // 10 -> sizeof coff_relocation_entry (without padding)
            relocation_offset += 10 * s.number_of_relocation_entries;
        }
    }

    // TODO: If executable, MS-DOS stub
    if (kind == Kind::EXECUTABLE) {
        fmt::print(stderr, "Generic2Coff: TODO: emit MS-DOS stub for PE32+ executable file\n");
    }

    std::vector<char> out{};
    const auto write_bytes = [&](auto& v) {
        const auto* start = reinterpret_cast<const uint8_t*>(&v);
        out.reserve(out.size() + sizeof(v));
        std::copy(start, start + sizeof(v), std::back_inserter(out));
    };
    const auto write_range = [&](auto& r) {
        const auto* start = reinterpret_cast<const uint8_t*>(r.data());
        out.reserve(out.size() + r.size() * sizeof(*r.data()));
        std::copy(start, start + r.size(), std::back_inserter(out));
    };

    // Ah, struct padding :<
    const auto write_symbol_entry = [&](const coff_symbol_entry& sym) {
        write_bytes(sym.name);
        write_bytes(sym.value);
        write_bytes(sym.section);
        write_bytes(sym.type);
        write_bytes(sym.storage_class);
        write_bytes(sym.auxiliary_count);
    };

    const auto write_relocation_entry = [&](const coff_relocation_entry& reloc) {
        write_bytes(reloc.reference_address);
        write_bytes(reloc.symbol_index);
        write_bytes(reloc.type);
    };

    // Header
    write_bytes(coff_header);

    // TODO: If executable, optional headers...
    if (kind == Kind::EXECUTABLE) {
        fmt::print(stderr, "Generic2Coff: TODO: emit optional headers for PE32+ executable file\n");
    }

    // Section Table (Section Headers)
    static_assert(sizeof(coff_section_header) == 40);
    for (const auto& s : section_headers)
        write_bytes(s);

    // Section data
    for (const auto& s : sections) {
        if (s.is_fill) continue;
        write_range(s.contents());
    }

    // Symbol table
    for (const auto& sym : coff_symbols)
        write_symbol_entry(sym);

    // String table
    write_bytes(string_table_size);
    for (const auto& s : strings) {
        write_range(s);
        constexpr auto null_terminator = '\0';
        write_bytes(null_terminator);
    }

    // Section relocation tables
    if (emit_relocations) {
        for (auto [i, s] : vws::enumerate(section_headers)) {
            int16_t s_position = (int16_t) i + 1;
            if (not coff_relocations.contains(s_position))
                continue;

            auto& this_sections_coff_relocations = coff_relocations.at(s_position);
            for (const auto& r : this_sections_coff_relocations)
                write_relocation_entry(r);
        }
    }
    return out;
}

} // namespace lcc
