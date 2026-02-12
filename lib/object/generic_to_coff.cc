#include <cstring>
#include <object/coff.h>
#include <object/generic.hh>

#include <lcc/utils.hh>

#include <algorithm>
#include <array>
#include <cstdint>
#include <cstdio>
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

auto write_symbol_entry(
    const coff_symbol_entry& sym,
    FILE* f
) -> void {
    fwrite(&sym.name, sizeof(sym.name), 1, f);
    fwrite(&sym.value, sizeof(sym.value), 1, f);
    fwrite(&sym.section, sizeof(sym.section), 1, f);
    fwrite(&sym.type, sizeof(sym.type), 1, f);
    fwrite(&sym.storage_class, sizeof(sym.storage_class), 1, f);
    fwrite(&sym.auxiliary_count, sizeof(sym.auxiliary_count), 1, f);
}

auto write_relocation_entry(
    const coff_relocation_entry& reloc,
    FILE* f
) -> void {
    fwrite(&reloc.reference_address, sizeof(reloc.reference_address), 1, f);
    fwrite(&reloc.symbol_index, sizeof(reloc.symbol_index), 1, f);
    fwrite(&reloc.type, sizeof(reloc.type), 1, f);
}

} // namespace

void GenericObject::as_coff(FILE* f) {
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

                coff_sym.storage_class = C_EXT;

                coff_symbols.emplace_back(coff_sym);
            } break;

            case Symbol::Kind::FUNCTION: {
                coff_symbol_entry coff_sym{};
                coff_sym.section = code_section_position;

                auto sym_name = coff_name(sym.name);
                std::ranges::copy(sym_name, coff_sym.name);

                coff_sym.storage_class = C_STAT;

                coff_sym.type = 0x20;

                coff_sym.section = code_section_position;

                LCC_ASSERT(
                    sym.byte_offset <= std::numeric_limits<uint32_t>::max(),
                    "Symbol byte offset cannot be encoded in COFF format"
                );
                coff_sym.value = (uint32_t) sym.byte_offset;

                coff_symbols.emplace_back(coff_sym);
            } break;

            case Symbol::Kind::STATIC: {
                coff_symbol_entry coff_sym{};

                auto sym_name = coff_name(sym.name);
                std::ranges::copy(sym_name, coff_sym.name);

                coff_sym.storage_class = C_STAT;

                coff_sym.section = section_position_by_name(sym.section_name);

                LCC_ASSERT(
                    sym.byte_offset <= std::numeric_limits<uint32_t>::max(),
                    "Symbol byte offset cannot be encoded in COFF format"
                );
                coff_sym.value = (uint32_t) sym.byte_offset;

                coff_symbols.emplace_back(coff_sym);
            } break;

            case Symbol::Kind::EXPORT: {
                coff_symbol_entry coff_sym{};

                auto sym_name = coff_name(sym.name);
                std::ranges::copy(sym_name, coff_sym.name);

                coff_sym.storage_class = C_STAT;

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
            }
        }
    }

    coff_header.number_of_sections = (uint16_t) sections.size();
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
    for (auto r : relocations) {
        coff_relocation_entry coff_relocation{};
        switch (r.kind) {
            case Relocation::Kind::NONE:
                LCC_UNREACHABLE();

            case Relocation::Kind::DISPLACEMENT32:
                coff_relocation.type = IMAGE_REL_AMD64_ADDR32;
                break;
            case Relocation::Kind::DISPLACEMENT32_PCREL:
                coff_relocation.type = IMAGE_REL_AMD64_REL32;
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

        // FIXME: r.symbol.byte_offset? r.addend? It's been too long, and I don't
        // have Windows or ReactOS.
        coff_relocation.reference_address
            = (int32_t) ((isz) r.symbol.byte_offset + r.addend);

        coff_relocation.symbol_index = (uint32_t) sym_index;

        // Find section symbol is defined in.
        int16_t symbol_section_position
            = section_position_by_name(r.symbol.section_name);

        coff_relocations[symbol_section_position].emplace_back(coff_relocation);
    }

    uint32_t string_table_size = (uint32_t) std::ranges::fold_left(
        std::ranges::views::transform(
            strings,
            [](const auto& s) { return s.size() + 1; }
        ),
        4, // empty COFF string table is 4 bytes long...
        std::plus{}
    );
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

    // TODO: If executable, MS-DOS stub

    // Header
    fwrite(&coff_header, sizeof(coff_header), 1, f);

    // TODO: If executable, optional headers...

    // Section Table (Section Headers)
    for (const auto& s : section_headers)
        fwrite(&s, sizeof(s), 1, f);

    // Section data
    for (const auto& s : sections) {
        if (s.is_fill) continue;
        fwrite(s.contents().data(), 1, s.contents().size(), f);
    }

    // Symbol table
    for (const auto& sym : coff_symbols)
        write_symbol_entry(sym, f);

    // String table
    fwrite(&string_table_size, sizeof(string_table_size), 1, f);
    for (const auto& s : strings) {
        fwrite(s.data(), 1, s.size(), f);
        constexpr auto null_terminator = '\0';
        fwrite(&null_terminator, sizeof(null_terminator), 1, f);
    }

    // Section relocation tables
    for (auto [i, s] : vws::enumerate(section_headers)) {
        int16_t s_position = (int16_t) i + 1;
        if (not coff_relocations.contains(s_position))
            continue;

        auto& this_sections_coff_relocations = coff_relocations.at(s_position);
        for (const auto& r : this_sections_coff_relocations)
            write_relocation_entry(r, f);
    }
}

} // namespace lcc
