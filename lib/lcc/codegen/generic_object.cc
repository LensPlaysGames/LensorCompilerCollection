#include <cstdio>
#include <cstring>
#include <lcc/codegen/elf.h>
#include <lcc/codegen/generic_object.hh>
#include <lcc/utils.hh>

namespace lcc {

// Don't forget to eventually set e_shnum and e_shstrndx
static elf64_header default_header() {
    elf64_header hdr{};
    hdr.e_ident[EI_MAG0] = 0x7f;
    hdr.e_ident[EI_MAG1] = 'E';
    hdr.e_ident[EI_MAG2] = 'L';
    hdr.e_ident[EI_MAG3] = 'F';
    hdr.e_ident[EI_CLASS] = EI_CLASS_64BIT;
    hdr.e_ident[EI_DATA] = EI_DATA_LITTLE_ENDIAN;
    hdr.e_ident[EI_VERSION] = 1;
    hdr.e_ident[EI_OSABI] = EI_OSABI_SYSV;
    hdr.e_ident[EI_ABIVERSION] = 0;
    hdr.e_type = ET_REL;
    hdr.e_machine = EM_X86_64;
    hdr.e_version = 1;
    hdr.e_entry = 0;
    hdr.e_phoff = 0;
    hdr.e_shoff = sizeof(elf64_header);
    hdr.e_flags = 0;
    hdr.e_ehsize = sizeof(elf64_header);
    hdr.e_phentsize = 0;
    hdr.e_phnum = 0;
    hdr.e_shentsize = sizeof(elf64_shdr);
    return hdr;
}

void GenericObject::as_elf(FILE* f) {
    elf64_header hdr = default_header();
    // Section header table entry count
    // NULL entry + GObj sections + ".strtab" + ".symtab" + ".rela.text"
    hdr.e_shnum = u16(sections.size() + 4);
    // Index of the section header table entry that contains the section
    // names.
    // Set down below.
    hdr.e_shstrndx = 0;

    // Build string table
    std::vector<u8> string_table;
    // NULL entry
    string_table.push_back(0);

    /// Append string to given byte buffer and return the index at the
    /// beginning of it.
    auto elf_add_string = [&](const char* new_string) {
        LCC_ASSERT(new_string, "Invalid argument (cannot add NULL string to ELF string table)");
        usz out = string_table.size();
        while (*new_string)
            string_table.push_back(u8(*new_string++));
        string_table.push_back(0); // NULL terminator
        return u32(out);
    };

    using Attr = Section::Attribute;
    // Section headers go in here.
    std::vector<elf64_shdr> shdrs{};
    // NULL entry
    shdrs.push_back(elf64_shdr());

    // Symbols go in here.
    std::vector<elf64_sym> syms{};
    // NULL entry.
    syms.push_back(elf64_sym());

    // Byte offset within file that section's data may be placed (must be
    // after all section headers).
    // AKA Number of bytes to skip until after the section header table.
    // Used in loop just below.
    usz data_offset = hdr.e_shoff + (sizeof(elf64_shdr) * hdr.e_shnum);

    // Build section headers from sections in this generic object file.
    // NOTE: If you add other sections first or otherwise change the fact that
    // the ordering of sections in the generic object file matches exactly the
    // ordering of section headers in the ELF file. This fact is used down
    // below to calculate ELF section indices.
    for (auto& section : sections) {
        elf64_shdr shdr{};

        if (section.is_fill) {
            shdr.sh_type = SHT_NOBITS;
            shdr.sh_size = section.length();
        } else {
            shdr.sh_type = SHT_PROGBITS;
            shdr.sh_size = section.contents().size();
            shdr.sh_offset = data_offset;
            data_offset += shdr.sh_size;
        }

        if (section.attribute(Attr::WRITABLE))
            shdr.sh_flags |= SHF_WRITE;
        if (section.attribute(Attr::EXECUTABLE))
            shdr.sh_flags |= SHF_EXECINSTR;
        if (section.attribute(Attr::LOAD))
            shdr.sh_flags |= SHF_ALLOC;

        shdr.sh_name = elf_add_string(section.name.data());

        // Create symbol for this section
        {
            elf64_sym sym{};
            sym.st_name = shdr.sh_name;
            sym.st_shndx = u16(shdrs.size());
            sym.st_info = ELF64_ST_INFO(STB_LOCAL, STT_SECTION);
            syms.push_back(sym);
        }

        shdrs.push_back(shdr);
    }

    for (auto& sym : symbols) {
        elf64_sym elf_sym{};
        elf_sym.st_name = elf_add_string(sym.name.data());

        if (sym.kind != Symbol::Kind::EXTERNAL) {
            // Get index of section by name
            bool found{false};
            usz elf_section_index{1}; // "1" because we need to start past NULL entry
            for (auto& section : sections) {
                if (section.name == sym.section_name) {
                    found = true;
                    break;
                }
                ++elf_section_index;
            }
            if (not found) Diag::ICE(
                "[GObj]: Could not find section {} mention by symbol {}",
                sym.section_name,
                sym.name
            );
            elf_sym.st_shndx = u16(elf_section_index);
        }

        elf_sym.st_value = sym.byte_offset;

        switch (sym.kind) {
            case Symbol::Kind::STATIC:
                elf_sym.st_info = ELF64_ST_INFO(STB_LOCAL, STT_OBJECT);
                break;

            case Symbol::Kind::EXPORT:
                elf_sym.st_info = ELF64_ST_INFO(STB_GLOBAL, STT_OBJECT);
                break;

            case Symbol::Kind::FUNCTION:
                elf_sym.st_info = ELF64_ST_INFO(STB_GLOBAL, STT_FUNC);
                // TODO: set elf_sym.st_size to size of assembled function.
                break;

            case Symbol::Kind::EXTERNAL:
                elf_sym.st_info = ELF64_ST_INFO(STB_GLOBAL, STT_NOTYPE);
                break;

            case Symbol::Kind::NONE: LCC_UNREACHABLE();
        }

        // All STB_LOCAL symbols go here
        if (elf_sym.st_info == ELF64_ST_INFO(STB_LOCAL, STT_OBJECT))
            syms.insert(syms.begin() + 1, elf_sym);
        else syms.push_back(elf_sym);
    }

    // Index needed by relocation section header(s)
    usz symbol_table_sh_index = shdrs.size();
    // Skip NULL entry and symbol table entry in section header table.
    usz string_table_sh_index = sections.size() + 3;

    // Symbol Table Section Header
    // shoutout https://stackoverflow.com/q/62497285
    {
        elf64_shdr shdr{};
        shdr.sh_type = SHT_SYMTAB;
        shdr.sh_name = elf_add_string(".symtab");
        shdr.sh_size = syms.size() * sizeof(elf64_sym);
        shdr.sh_offset = data_offset;

        shdr.sh_link = u32(string_table_sh_index);
        // One greater than the symbol table index of the last local symbol
        usz last_local_index = 0;
        for (auto [i, elf_sym] : vws::enumerate(syms)) {
            if (ELF64_ST_BIND(elf_sym.st_info) == STB_LOCAL)
                last_local_index = usz(i);
        }
        shdr.sh_info = u32(last_local_index + 1);

        shdr.sh_entsize = sizeof(elf64_sym);

        shdrs.push_back(shdr);
        data_offset += shdr.sh_size;
    }

    // ".text" relocations section headaer: ".rela.text"
    {
        elf64_shdr shdr{};
        shdr.sh_type = SHT_RELA;
        shdr.sh_name = elf_add_string(".rela.text");
        // "If the file has a loadable segment that includes relocation,
        // the sections’ attributes will include the SHF_ALLOC bit;
        // otherwise, that bit will be off."
        shdr.sh_flags |= SHF_ALLOC;

        /// The section header index of the associated symbol table.
        shdr.sh_link = (uint32_t) symbol_table_sh_index;
        /// The section header index of the section to which the relocation
        // applies. (.text in GenericObjectFile is always at index 0, so it's
        // guaranteed that section 1 (first section) is ".text").
        shdr.sh_info = 1;

        shdr.sh_size = relocations.size() * sizeof(elf64_rela);
        shdr.sh_offset = data_offset;
        shdr.sh_entsize = sizeof(elf64_rela);

        shdrs.push_back(shdr);
        data_offset += shdr.sh_size;
    }

    // String Table Section Header
    {
        elf64_shdr shdr = {};
        shdr.sh_type = SHT_STRTAB;
        shdr.sh_name = elf_add_string(".strtab");
        // FIXME: Ensure no more strings are added to table after this.
        shdr.sh_size = string_table.size();
        shdr.sh_offset = data_offset;
        hdr.e_shstrndx = u16(shdrs.size());

        shdrs.push_back(shdr);
        data_offset += shdr.sh_size;
    }

    // Build elf64_rela relocations
    std::vector<elf64_rela> elf_relocations{};
    for (auto& reloc : relocations) {
        // Find symbol with matching name.
        auto found = std::find_if(syms.begin(), syms.end(), [&](elf64_sym elf_sym) {
            char* sym_name = (char*) string_table.data() + elf_sym.st_name;
            return strcmp(sym_name, reloc.symbol.name.data()) == 0;
        });
        if (found == syms.end()) Diag::ICE(
            "Could not find symbol {} referenced by relocation",
            reloc.symbol.name
        );
        usz sym_index = usz(found - syms.begin());

        elf64_rela elf_reloc{};
        elf_reloc.r_offset = reloc.symbol.byte_offset;
        switch (reloc.kind) {
            case Relocation::Kind::DISPLACEMENT32_PCREL: {
                auto found = std::find_if(symbols.begin(), symbols.end(), [&](Symbol& symbol) {
                    return symbol.name == reloc.symbol.name;
                });
                if (found == symbols.end()) Diag::ICE(
                    "Could not find symbol {} referenced by relocation",
                    reloc.symbol.name
                );

                if (reloc.symbol.kind == Symbol::Kind::FUNCTION)
                    elf_reloc.r_info = ELF64_R_INFO(sym_index, R_X86_64_PLT32);
                else elf_reloc.r_info = ELF64_R_INFO(sym_index, R_X86_64_PC32);

                // DISP*32* -> 32-bit displacement -> 4 byte offset
                elf_reloc.r_addend = -4;

            } break;

            case Relocation::Kind::DISPLACEMENT32:
                elf_reloc.r_info = ELF64_R_INFO(sym_index, R_X86_64_32);
                break;

            default: LCC_UNREACHABLE();
        }
        elf_relocations.push_back(elf_reloc);
    }

    // Write ELF header
    fwrite(&hdr, 1, sizeof(hdr), f);
    // Write section header table
    for (auto& shdr : shdrs)
        fwrite(&shdr, 1, sizeof(shdr), f);
    // Write section's data
    for (auto& section : sections) {
        if (section.is_fill) {
            // Skips .bss
            if (section.attribute(Attr::LOAD)) {
                for (usz n = section.length(); n; --n)
                    fwrite(&section.value(), 1, 1, f);
            }
        } else fwrite(section.contents().data(), 1, section.contents().size(), f);
    }
    // Write symbol table ".symtab"
    fwrite(syms.data(), sizeof(*syms.data()), syms.size(), f);
    // Write text section relocations ".rela.text"
    fwrite(elf_relocations.data(), sizeof(*elf_relocations.data()), elf_relocations.size(), f);
    // Write string table ".strtab"
    fwrite(string_table.data(), 1, string_table.size(), f);
}

} // namespace lcc
