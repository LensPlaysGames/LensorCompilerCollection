#include <clink/link_elf.hh>

#include <object/elf.h>
#include <object/elf.hh>
#include <object/generic.hh>

#include <fmt/format.h>

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
    if (section_name_header->sh_offset + section_name_header->sh_size > blob.size()) {
        fmt::print(stderr, "clink: error in ELF file: section name section out-of-bounds of file\n");
        section_names_begin = nullptr;
    }

    std::unordered_map<ptrdiff_t, lcc::Symbol> indexed_symbols{};

    // Collect symbols first...
    for (
        auto section_header_iter = section_header_begin;
        section_header_iter < section_header_end;
        ++section_header_iter
    ) {
        const auto& section_header = *section_header_iter;
        // We are only interested in the symbol table.
        if (section_header.sh_type != SHT_SYMTAB)
            continue;

        const auto symbol_count = section_header.sh_size / section_header.sh_entsize;
        if (
            section_header.sh_offset
                + symbol_count * sizeof(elf64_sym)
            >= blob.size()
        ) {
            fmt::print(
                stderr,
                "clink: error in ELF file: symbol table section header offset larger than file size ({} > {})\n",
                section_header.sh_offset,
                blob.size()
            );
            continue;
        }
        const auto* const symbol_begin
            = reinterpret_cast<elf64_sym*>(blob.data() + section_header.sh_offset);

        // I don't like copying for no reason, but, we get the following error,
        // otherwise...
        // `reference binding to misaligned address 0x... for type 'const struct
        //  elf64_sym', which requires 8 byte alignment`
        std::vector<elf64_sym> symbols;
        symbols.resize(symbol_count);
        memcpy(symbols.data(), symbol_begin, symbol_count * sizeof(*symbol_begin));

        auto* string_table_header = section_header_begin + section_header.sh_link;
        const char* string_table = blob.data() + string_table_header->sh_offset;
        if (string_table_header->sh_offset + string_table_header->sh_size > blob.size()) {
            fmt::print(stderr, "clink: error in ELF file: string table out-of-bounds of file\n");

            string_table = nullptr;
        }

        for (auto [i, elf_symbol] : std::ranges::views::enumerate(symbols)) {
            auto symbol_type = ELF64_ST_TYPE(elf_symbol.st_info);
            auto symbol_binding = ELF64_ST_BIND(elf_symbol.st_info);

            lcc::Symbol symbol{};
            symbol.byte_offset = elf_symbol.st_value;

            // Get symbol's section name, if applicable
            if (elf_symbol.st_shndx != SHN_UNDEF and elf_symbol.st_shndx < SHN_LORESERVE) {
                auto* relevant_header = section_header_begin + elf_symbol.st_shndx;
                if (section_names_begin) {
                    symbol.section_name = std::string{
                        section_names_begin + relevant_header->sh_name
                    };
                }
            }

            // Get symbol name
            // Section symbols don't have st_name set.
            if (symbol_type == STT_SECTION)
                symbol.name = symbol.section_name;
            else {
                if (string_table)
                    symbol.name = string_table + elf_symbol.st_name;
                else symbol.name = "<nostrings>";
            }

            // Determine LCC Symbol kind
            switch (symbol_binding) {
                case STB_WEAK:
                    // weak reference or weak definition
                    symbol.kind = lcc::Symbol::Kind::WEAK;
                    break;
                case STB_LOCAL:
                    symbol.kind = lcc::Symbol::Kind::STATIC;
                    break;

                case STB_GLOBAL:
                    // external reference
                    if (elf_symbol.st_shndx == SHN_UNDEF)
                        symbol.kind = lcc::Symbol::Kind::EXTERNAL;
                    // global function definition
                    else if (symbol_type == STT_FUNC)
                        symbol.kind = lcc::Symbol::Kind::FUNCTION;
                    // global variable
                    else symbol.kind = lcc::Symbol::Kind::EXPORT;
                    break;

                default:
                    lcc::Diag::ICE("clink: unhandled ELF binding {}\n", symbol_binding);
            }

            out.symbols.emplace_back(symbol);
            indexed_symbols[i] = symbol;
        }
        break;
    }

    // fmt::print("collected indexed symbols:");
    // for (auto [i, sym] : indexed_symbols)
    //     fmt::print(" {}:{}", i, sym.name);
    // fmt::print("\n");

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

        if (section_header.sh_offset >= blob.size_bytes()) {
            fmt::print(
                stderr,
                "clink: section header has offset greater than file size ({} > {})\n",
                section_header.sh_offset,
                blob.size_bytes()
            );
        }
        if (section_header.sh_offset + section_header.sh_size >= blob.size_bytes()) {
            fmt::print(
                stderr,
                "clink: section header has bounds that exceed file size ({} > {})\n",
                section_header.sh_offset + section_header.sh_size,
                blob.size_bytes()
            );
        }

        lcc::Section section{};
        if (section_names_begin) {
            section.name = std::string{
                section_names_begin + section_header.sh_name
            };
        }
        section.attribute(
            lcc::Section::Attribute::WRITABLE,
            section_header.sh_flags & SHF_WRITE
        );
        section.attribute(
            lcc::Section::Attribute::LOAD,
            section_header.sh_flags & SHF_ALLOC
        );
        section.attribute(
            lcc::Section::Attribute::EXECUTABLE,
            section_header.sh_flags & SHF_EXECINSTR
        );

        if (section_header.sh_type == SHT_NOBITS) {
            section.is_fill = true;
            section.length() = (lcc::u32) section_header.sh_size;
        } else {
            if (section_header.sh_offset + section_header.sh_size < blob.size()) {
                const auto* content_begin = blob.data() + section_header.sh_offset;
                section.contents() = {
                    content_begin,
                    content_begin + section_header.sh_size
                };
            }
        }

        // RELocations (with Addend)
        if (section.name.starts_with(".rela")) {
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

                // fmt::print("Relocation looking for symbol index {}\n", symbol_index);
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
                else if (relocation_type == R_X86_64_32)
                    relocation.kind = lcc::Relocation::Kind::DISPLACEMENT32;
                else if (
                    relocation_type == R_X86_64_GOTPCREL
                    or relocation_type == R_X86_64_GOTPCRELX
                    or relocation_type == R_X86_64_REX_GOTPCRELX
                ) relocation.kind = lcc::Relocation::Kind::DISPLACEMENT32_GOTPCREL;
                else if (
                    relocation_type == R_X86_64_64
                ) relocation.kind = lcc::Relocation::Kind::DISPLACEMENT64;
                else if (
                    relocation_type == R_X86_64_DPTMOD64
                    or relocation_type == R_X86_64_DTPOFF64
                    or relocation_type == R_X86_64_TPOFF64
                    or relocation_type == R_X86_64_TLSGD
                    or relocation_type == R_X86_64_TLSLD
                    or relocation_type == R_X86_64_DTPOFF32
                    or relocation_type == R_X86_64_GOTTPOFF
                    or relocation_type == R_X86_64_TPOFF32
                ) {
                    fmt::print(
                        stderr,
                        "\nclink: Unhandled ELF relocation type {} wrt thread local storage... ignoring (symbol {} in section {}). Linking will likely fail\n\n",
                        +relocation_type,
                        relocation.symbol.name,
                        relocation.symbol.section_name
                    );
                    continue;
                } else {
                    fmt::print(
                        stderr,
                        "\nclink: Unhandled ELF relocation type {}... ignoring (symbol {} in section {}). Linking will likely fail\n\n",
                        +relocation_type,
                        relocation.symbol.name,
                        relocation.symbol.section_name
                    );
                    continue;
                }
                out.relocations.emplace_back(relocation);
            }
            continue;
        }

        out.sections.emplace_back(section);
    }

    return out;
}

} // namespace clink
