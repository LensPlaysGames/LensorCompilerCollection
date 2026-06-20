#include <object/elf.h>
#include <object/generic.hh>

#include <lcc/assert.hh>

#include <algorithm>
#include <cstdio>
#include <cstring>
#include <iterator>
#include <ranges>
#include <vector>

namespace lcc {

namespace {
// Don't forget to eventually set e_shnum and e_shstrndx, as well as e_phnum, e_phoff, e_shoff if needed
constexpr auto default_header() -> elf64_header {
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
    hdr.e_phentsize = sizeof(elf64_phdr);
    hdr.e_phnum = 0;
    hdr.e_shentsize = sizeof(elf64_shdr);
    return hdr;
}

/// Append string to given byte buffer and return the index at the
/// beginning of it.
auto elf_add_string(std::vector<u8>& string_table, const char* new_string) {
    LCC_ASSERT(new_string, "Invalid argument (cannot add NULL string to ELF string table)");
    usz out = string_table.size();
    while (*new_string)
        string_table.push_back(u8(*new_string++));
    string_table.push_back(0); // NULL terminator
    return u32(out);
}
auto elf_add_string(std::vector<u8>& string_table, std::string_view new_string) {
    usz out = string_table.size();
    string_table.reserve(new_string.size());
    string_table.insert(string_table.end(), new_string.begin(), new_string.end());
    string_table.push_back(0); // NULL terminator
    return u32(out);
}

uint16_t header_type(GenericObject::Kind k) {
    switch (k) {
        case GenericObject::Kind::STATIC: return ET_REL;
        case GenericObject::Kind::SHARED: return ET_DYN;
        case GenericObject::Kind::EXECUTABLE: return ET_EXEC;
        case GenericObject::Kind::COUNT: break;
    }
    LCC_UNREACHABLE();
}

} // namespace

void GenericObject::as_elf(FILE* f) const {
    constexpr uint64_t BASE{0x400000};

    std::vector<Section> sorted_sections{};
    for (auto& s : sections) {
// hack to make sure sorted_sections is always used
#define sections static_assert(false);
        auto placement_iterator = sorted_sections.begin();
        // Place loaded sections before all other sections.
        if (not s.attribute(Section::Attribute::LOAD)) {
            while (
                placement_iterator < sorted_sections.end()
                and placement_iterator->attribute(Section::Attribute::LOAD)
            ) ++placement_iterator;
        } else {
            // Place executable sections before all other sections.
            if (not s.attribute(Section::Attribute::EXECUTABLE)) {
                while (
                    placement_iterator < sorted_sections.end()
                    and placement_iterator->attribute(Section::Attribute::EXECUTABLE)
                ) ++placement_iterator;
            }
            // Place writable sections after all non-writable sections.
            if (s.attribute(Section::Attribute::WRITABLE)) {
                while (
                    placement_iterator < sorted_sections.end()
                    and placement_iterator->attribute(Section::Attribute::LOAD)
                    and not placement_iterator->attribute(Section::Attribute::WRITABLE)
                ) ++placement_iterator;
            }
        }

        sorted_sections.insert(placement_iterator, s);
    }

    elf64_header hdr = default_header();
    hdr.e_type = header_type(kind);
    // Section header table entry count
    // NULL entry + GObj sections + ".strtab" + ".symtab" + ".rela.text"
    hdr.e_shnum = u16(sorted_sections.size() + 4);
    if (kind == GenericObject::Kind::EXECUTABLE) {
        // ".rela.text" not added
        --hdr.e_shnum;
    }
    // Index of the section header table entry that contains the section
    // names.
    // Set down below.
    hdr.e_shstrndx = 0;

    // Build string table
    std::vector<u8> string_table;
    // NULL entry
    string_table.push_back(0);

    using Attr = Section::Attribute;
    // Section headers go in here.
    std::vector<elf64_shdr> shdrs{};
    // NULL entry
    shdrs.emplace_back();

    // Symbols go in here.
    std::vector<elf64_sym> syms{};
    // NULL entry.
    syms.emplace_back();

    // fmt::print("!!as_elf!!\n{}\n", print());

    // Count Program Headers
    lcc::usz phdrs_count{};
    if (kind == Kind::EXECUTABLE) {
        std::optional<decltype(Section::attributes)> current_attributes{};
        // PT_LOAD program headers (for loaded sections)
        for (auto& s : sorted_sections) {
            // Skip unloaded sections
            if (not s.attribute(Section::Attribute::LOAD)) break;

            // Skip empty sections
            if (
                (s.is_fill and s.length() == 0)
                or ((not s.is_fill) and s.contents().empty())
            ) continue;

            if (
                not current_attributes.has_value()
                or s.attributes != current_attributes.value()
            ) {
                // fmt::print("Encountered new attributes, creating new program header\n{}\n", s.print());
                ++phdrs_count;
                current_attributes = s.attributes;
            }
        }
    }
    hdr.e_phnum = (uint16_t) phdrs_count;
    hdr.e_phoff = sizeof(elf64_header);
    hdr.e_shoff += phdrs_count * sizeof(elf64_phdr);

    // Byte offset within file that section's data may be placed (must be
    // after all section headers).
    // AKA Number of bytes to skip until after the section header table.
    // Used in loop just below.
    auto data_offset = hdr.e_shoff + (sizeof(elf64_shdr) * hdr.e_shnum);
    // Byte offset within memory where section's data will be loaded at
    // execution time (virtual address).
    lcc::usz memory_offset = data_offset;

    // TODO: When we decide to place a segment at a specific virtual address,
    // it must pad the output binary file with zeros until the Offset matches
    // the page-offset of that virtual address.

    // Build section headers from sections in this generic object file.
    // NOTE: If you add other sections first or otherwise change the fact that
    // the ordering of sections in the generic object file matches exactly the
    // ordering of section headers in the ELF file. This fact is used down
    // below to calculate ELF section indices.
    {
        std::optional<decltype(Section::attributes)> current_attributes{};
        for (auto& section : sorted_sections) {
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

            // Memory offset grows for every loaded section
            if (section.attribute(Attr::LOAD)) {
                if (kind == GenericObject::Kind::EXECUTABLE) {
                    // FIXME: align needed all the time? page size (of target)?
                    shdr.sh_addralign = 0x1000;

                    // If this is the start of a new span of permissions (other than the
                    // first), ensure we are past the previous page boundary so that the
                    // permissions don't overwrite each other.
                    if (
                        current_attributes.has_value()
                        and section.attributes != current_attributes.value()
                        and memory_offset % 0x1000 != 0
                    ) {
                        memory_offset += 0x1000;
                        fmt::print("Section {} changed permissions, memory offset:0x{:08x}\n", section.name, memory_offset);
                    }
                    shdr.sh_addr = BASE + memory_offset;
                    current_attributes = section.attributes;
                }

                // FIXME: BIG BUG: For not is_fill sections, data_offset and memory_offset
                // become mismatched, but, they have to stay the same (or at least the
                // same modulo page size).
                // Basically, if data_offset < memory_offset, we need to emit a bunch of
                // zeroes into the binary...

                memory_offset += shdr.sh_size;
            }

            if (section.attribute(Attr::WRITABLE))
                shdr.sh_flags |= SHF_WRITE;
            if (section.attribute(Attr::EXECUTABLE))
                shdr.sh_flags |= SHF_EXECINSTR;
            if (section.attribute(Attr::LOAD))
                shdr.sh_flags |= SHF_ALLOC;

            shdr.sh_name = elf_add_string(string_table, section.name);

            // fmt::print(
            //     "clink: Section {}: byte offset {}, size {}, address {:08x}, align {}\n",
            //     section.name,
            //     shdr.sh_offset,
            //     shdr.sh_size,
            //     shdr.sh_addr,
            //     shdr.sh_addralign
            // );

            // Create symbol for this section
            {
                elf64_sym sym{};
                sym.st_name = shdr.sh_name;
                sym.st_shndx = u16(shdrs.size());
                sym.st_info = ELF64_ST_INFO(STB_LOCAL, STT_SECTION);
                syms.push_back(sym);
            }

            shdrs.emplace_back(shdr);
        }
    }

    for (auto& sym : symbols) {
        elf64_sym elf_sym{};
        elf_sym.st_name = elf_add_string(string_table, sym.name);

        if (sym.kind != Symbol::Kind::EXTERNAL and not sym.section_name.empty()) {
            // Get index of section by name
            bool found{false};
            usz elf_section_index{1}; // "1" because we need to start past NULL entry
            for (auto& section : sorted_sections) {
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

        if (kind == GenericObject::Kind::EXECUTABLE) {
            elf_sym.st_value += shdrs.at(elf_sym.st_shndx).sh_addr;
            // fmt::print(
            //     "Adjusting symbol {} section {} byte offset {} to 0x{:08x}\n",
            //     sym.name,
            //     sym.section_name,
            //     sym.byte_offset,
            //     elf_sym.st_value
            // );
        }

        if (
            kind == GenericObject::Kind::EXECUTABLE
            and sym.name == "_start"
            and sym.kind != Symbol::Kind::EXTERNAL
            and sym.section_name == ".text"
        ) {
            hdr.e_entry = elf_sym.st_value;
            // fmt::print("clink: Setting entry to `{}` at 0x{:08x}\n", sym.name, hdr.e_entry);
        }

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
    // Index of Section Header of symbol table (.symtab section)
    usz symbol_table_sh_index = shdrs.size();
    // Section header index of String Table (.strtab section)
    // Skip NULL entry and symbol table entry in section header table.
    usz string_table_sh_index = sorted_sections.size() + 3;
    // Include .rela.text for non-executables
    if (kind == GenericObject::Kind::EXECUTABLE) {
        // ".rela.text" not added
        --string_table_sh_index;
    }

    // Symbol Table Section Header
    // shoutout https://stackoverflow.com/q/62497285
    {
        elf64_shdr shdr{};
        shdr.sh_type = SHT_SYMTAB;
        shdr.sh_name = elf_add_string(string_table, ".symtab");
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

        shdrs.emplace_back(shdr);
        data_offset += shdr.sh_size;
    }

    // ".text" relocations section header: ".rela.text"
    if (kind != Kind::EXECUTABLE) {
        elf64_shdr shdr{};
        shdr.sh_type = SHT_RELA;
        shdr.sh_name = elf_add_string(string_table, ".rela.text");
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

        shdrs.emplace_back(shdr);
        data_offset += shdr.sh_size;
        memory_offset += shdr.sh_size;
    }

    // String Table Section Header
    {
        elf64_shdr shdr = {};
        shdr.sh_type = SHT_STRTAB;
        shdr.sh_name = elf_add_string(string_table, ".strtab");
        // FIXME: Ensure no more strings are added to table after this.
        // TODO: Locking vector sort of container needed here as well.
        shdr.sh_size = string_table.size();
        shdr.sh_offset = data_offset;
        // shdr.sh_addr = shdr.sh_offset;
        hdr.e_shstrndx = u16(shdrs.size());

        shdrs.emplace_back(shdr);
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
        u64 sym_index = u64(found - syms.begin());

        elf64_rela elf_reloc{};
        elf_reloc.r_offset = reloc.symbol.byte_offset;
        switch (reloc.kind) {
            case Relocation::Kind::DISPLACEMENT32_PCREL: {
                auto found_symbol = std::find_if(
                    symbols.begin(),
                    symbols.end(),
                    [&](const Symbol& symbol) {
                        return symbol.name == reloc.symbol.name;
                    }
                );
                if (found_symbol == symbols.end()) {
                    Diag::ICE(
                        "Could not find symbol {} referenced by relocation",
                        reloc.symbol.name
                    );
                }

                if (reloc.symbol.kind == Symbol::Kind::FUNCTION)
                    elf_reloc.r_info = ELF64_R_INFO(sym_index, R_X86_64_PLT32);
                else elf_reloc.r_info = ELF64_R_INFO(sym_index, R_X86_64_PC32);

                // DISP*32* -> 32-bit displacement -> 4 byte offset
                // The 4 bytes is because the *4 byte* address goes at the very end of the
                // binary instruction encoding, and, by the time the CPU is _executing_
                // the instruction that takes this address as an operand (at the very end
                // of the instruction's encoding), the program counter/instruction pointer
                // is already at the beginning of the *next* instruction. This 4 byte
                // offset takes into account the fact that what we are going to calculate
                // involves the distance between the symbol definition and the exact
                // location within the section we are overwriting, but that is not how the
                // CPU will end up doing it at execution time.
                elf_reloc.r_addend = -4;
            } break;

            case Relocation::Kind::DISPLACEMENT32:
                elf_reloc.r_info = ELF64_R_INFO(sym_index, R_X86_64_32);
                break;

            default: LCC_UNREACHABLE();
        }
        elf_relocations.push_back(elf_reloc);
    }

    // Program Headers
    // Basically, we want to create program headers for every span of sections
    // with alike permissions...
    std::vector<elf64_phdr> phdrs{};
    if (kind == Kind::EXECUTABLE) {
        // PT_LOAD program headers (for loaded sections)
        std::optional<decltype(elf64_shdr::sh_flags)> current_attributes{};
        elf64_phdr phdr{};
        phdr.p_type = PT_NULL;
        bool skip_null = true;
        for (const auto& s : shdrs) {
            if (skip_null) {
                skip_null = false;
                continue;
            }
            // Detect change in attributes, boundary between permission spans.
            if (
                phdr.p_type != PT_NULL
                and current_attributes.has_value()
                and s.sh_flags != current_attributes.value()
            ) {
                phdrs.emplace_back(phdr);
                phdr.p_type = PT_NULL;
            }

            // Because sections are sorted, all loaded sections come first. That means
            // we can stop at the first unloaded section.
            if (not (s.sh_flags & SHF_ALLOC)) break;

            // Skip empty sections
            if (s.sh_size == 0) continue;

            if (current_attributes.has_value() and s.sh_flags == current_attributes.value()) {
                phdr.p_filesz += s.sh_size;
                phdr.p_memsz += s.sh_size;
                continue;
            }

            current_attributes = s.sh_flags;

            phdr.p_type = PT_LOAD;
            // File and Memory Offsets
            phdr.p_offset = s.sh_offset;
            phdr.p_filesz = s.sh_size;
            phdr.p_memsz = phdr.p_filesz;
            // Flags
            phdr.p_flags = PF_R;
            if (s.sh_flags & SHF_WRITE)
                phdr.p_flags |= PF_W;
            if (s.sh_flags & SHF_EXECINSTR)
                phdr.p_flags |= PF_X;
            // Address
            phdr.p_vaddr = s.sh_addr;
            phdr.p_paddr = phdr.p_vaddr;
            phdr.p_align = s.sh_addralign;

            if (phdr.p_align) {
                LCC_ASSERT(
                    phdr.p_offset % phdr.p_align == phdr.p_vaddr % phdr.p_align,
                    "p_offset:{},p_vaddr:0x{:08x},p.align:{}   {} != {} ({} difference)",
                    phdr.p_offset,
                    phdr.p_vaddr,
                    phdr.p_align,
                    phdr.p_offset % phdr.p_align,
                    phdr.p_vaddr % phdr.p_align,
                    phdr.p_offset % phdr.p_align - phdr.p_vaddr % phdr.p_align
                );
            }
        }

        LCC_ASSERT(
            phdrs.size() == phdrs_count,
            "{} != {}",
            phdrs.size(),
            phdrs_count
        );
    }

    // Write ELF header
    fwrite(&hdr, 1, sizeof(hdr), f);
    // Write program header table
    for (auto& phdr : phdrs)
        fwrite(&phdr, 1, sizeof(phdr), f);
    // Write section header table
    for (auto& shdr : shdrs)
        fwrite(&shdr, 1, sizeof(shdr), f);
    // Write section's data
    for (auto& section : sorted_sections) {
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
#undef sections
}

} // namespace lcc
