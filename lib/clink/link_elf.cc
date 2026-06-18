#include <clink/link_elf.hh>

#include <cstddef>
#include <object/elf.h>
#include <object/elf.hh>
#include <object/generic.hh>

#include <cstddef>
#include <span>
#include <unordered_map>

namespace clink {

lcc::GenericObject collect_elf(std::span<char> blob) {
    lcc::GenericObject out{};

    if (blob.size() < sizeof(elf64_header))
        return {};

    elf64_header hdr;
    memcpy(&hdr, blob.data(), sizeof(hdr));

    auto valid_header = lcc::elf::validate_header(hdr);
    if (not valid_header.first) {
        fmt::print(
            "clink: invalid ELF header: {}\n",
            valid_header.second
        );
    }

    if (sizeof(elf64_header) != hdr.e_shentsize)
        return {};

    const auto section_header_begin = reinterpret_cast<elf64_shdr*>(blob.data() + hdr.e_shoff);
    const auto section_header_end = section_header_begin + hdr.e_shnum;

    auto section_name_header = section_header_begin + hdr.e_shstrndx;
    const char* section_names_begin = blob.data() + section_name_header->sh_offset;

    std::unordered_map<ptrdiff_t, lcc::Symbol> indexed_symbols{};

    // Collect symbols first...
    for (
        auto section_header_iter = section_header_begin;
        section_header_iter < section_header_end;
        ++section_header_iter
    ) {
        const auto& section_header = *section_header_iter;
        if (section_header.sh_type == SHT_SYMTAB) {
            const auto* const symbol_begin
                = reinterpret_cast<elf64_sym*>(blob.data() + section_header.sh_offset);
            const auto* const symbol_end
                = symbol_begin + (section_header.sh_size / section_header.sh_entsize);

            auto* string_table_header = section_header_begin + section_header.sh_link;
            const char* string_table = blob.data() + string_table_header->sh_offset;

            for (auto* symbol_iter = symbol_begin; symbol_iter < symbol_end; ++symbol_iter) {
                auto& elf_symbol = *symbol_iter;
                auto symbol_type = ELF64_ST_TYPE(elf_symbol.st_info);
                auto symbol_binding = ELF64_ST_BIND(elf_symbol.st_info);

                // Skip symbols with no type (i.e. NULL entries).
                if (symbol_type == STT_NOTYPE) continue;

                lcc::Symbol symbol{};

                symbol.byte_offset = elf_symbol.st_value;
                if (elf_symbol.st_shndx != SHN_UNDEF and elf_symbol.st_shndx < SHN_LORESERVE) {
                    auto* relevant_header = section_header_begin + elf_symbol.st_shndx;
                    symbol.section_name = section_names_begin + relevant_header->sh_name;
                }

                // Section symbols don't have st_name set.
                if (symbol_type == STT_SECTION)
                    symbol.name = symbol.section_name;
                else symbol.name = string_table + elf_symbol.st_name;

                if (elf_symbol.st_shndx == SHN_UNDEF)
                    symbol.kind = lcc::Symbol::Kind::EXTERNAL;
                else if (symbol_type == STT_FUNC)
                    symbol.kind = lcc::Symbol::Kind::FUNCTION;
                else if (symbol_binding == STB_LOCAL)
                    symbol.kind = lcc::Symbol::Kind::STATIC;
                else symbol.kind = lcc::Symbol::Kind::EXPORT;
                out.symbols.emplace_back(symbol);
                indexed_symbols[symbol_iter - symbol_begin] = symbol;
            }
            break;
        }
    }

    for (
        auto section_header_iter = section_header_begin;
        section_header_iter < section_header_end;
        ++section_header_iter
    ) {
        const auto& section_header = *section_header_iter;

        // Skip NULL sections
        // Also skip symbol table, since we've already collected symbols.
        if (
            section_header.sh_type == SHT_NULL
            or section_header.sh_type == SHT_SYMTAB
            or section_header.sh_type == SHT_SYMTAB_SHNDX
            or section_header.sh_type == SHT_STRTAB
        ) continue;

        lcc::Section section{};
        section.name = section_names_begin + section_header.sh_name;

        if (section_header.sh_type == SHT_NOBITS) {
            section.is_fill = true;
            section.length() = (lcc::u32) section_header.sh_size;
        } else {
            const auto* content_begin = blob.data() + section_header.sh_offset;
            section.contents() = {
                content_begin,
                content_begin + section_header.sh_size
            };
        }

        if (section.name.starts_with(".rela")) {
            // RELocations (with Addend)
            auto* relocation_begin = reinterpret_cast<elf64_rela*>(section.contents().data());
            const auto* relocation_end
                = relocation_begin + (section_header.sh_size / section_header.sh_entsize);
            for (; relocation_begin < relocation_end; ++relocation_begin) {
                auto& elf_relocation = *relocation_begin;
                auto symbol_index = ELF64_R_SYM(elf_relocation.r_info);
                auto relocation_type = ELF64_R_TYPE(elf_relocation.r_info);

                // TODO: Machine-dependent
                // Skip NULL entries
                if (relocation_type == R_X86_64_NONE) continue;

                lcc::Relocation relocation{};

                relocation.symbol = indexed_symbols.at((ptrdiff_t) symbol_index);
                relocation.symbol.section_name = section.name.substr(
                    std::string_view(".rela").length()
                );
                relocation.symbol.byte_offset = elf_relocation.r_offset;

                relocation.addend = elf_relocation.r_addend;
                if (
                    relocation_type == R_X86_64_PC32
                    or relocation_type == R_X86_64_PLT32
                ) relocation.kind = lcc::Relocation::Kind::DISPLACEMENT32_PCREL;
                else relocation.kind = lcc::Relocation::Kind::DISPLACEMENT32;
                out.relocations.emplace_back(relocation);
            }
            continue;
        }

        out.sections.emplace_back(section);
    }

    return out;
}

} // namespace clink
