#include <object/elf.h>
#include <object/generic.hh>

#include <lccbase/assert.hh>

#include <clink/layout.hh>

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
    LCC_ASSERT(
        new_string,
        "Invalid argument (cannot add NULL string to ELF string table)"
    );
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

auto GenericObject::as_elf(
    clink::Layout& layout,
    EmitRelocations _emit_relocations
) -> std::vector<char> {
    const bool emit_relocations
        = +_emit_relocations and not relocations.empty();

    elf64_header hdr = default_header();
    hdr.e_type = header_type(kind);
    // Section header table entry count
    // +3 because of NULL entry + ".strtab" + ".symtab"
    hdr.e_shnum = u16(sections.size() + 3);
    if (emit_relocations) {
        // TODO: relocation section count
        // ".rela.text" added
        ++hdr.e_shnum;
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
        // TODO: only if (musl or not static-executable)
        // PT_PHDR
        ++phdrs_count;

        std::optional<decltype(Section::attributes)> current_attributes{};
        // PT_LOAD program headers (for loaded sections)
        for (auto& s : sections) {
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
        hdr.e_phoff = sizeof(elf64_header);
    }
    hdr.e_phnum = (uint16_t) phdrs_count;
    hdr.e_shoff += phdrs_count * sizeof(elf64_phdr);

    // Byte offset within file that section's data may be placed (must be
    // after all section headers).
    // AKA Number of bytes to skip until after the section header table.
    // Used in loop just below.
    auto data_offset = hdr.e_shoff + (sizeof(elf64_shdr) * hdr.e_shnum);

    // This is where section data *actually* starts.
    // Adjust layout to reflect reality.
    // TODO: No magic numbers
    constexpr uint64_t memory_base = 0x00400000ull;
    uint64_t dot = kind == Kind::EXECUTABLE
                     ? memory_base
                     : 0ull;
    for (auto& s : layout.segments) {
        dot += data_offset;
        s.address = dot;
        dot += s.size();
        dot = lcc::utils::AlignTo(dot, (decltype(dot)) 0x1000);
    }

    // fmt::print("\nEXPECTED LAYOUT:\n");
    // for (auto& s : layout.segments) {
    //     fmt::print(
    //         "SEGMENT: attributes={} size={} address=0x{:08x}\n",
    //         s.attributes,
    //         s.size(),
    //         s.address
    //     );
    //     for (auto& section : s.sections) {
    //         fmt::print(
    //             "  SECTION: `{}` size={} address=0x{:08x}\n",
    //             section.name,
    //             section.size,
    //             layout.address(section.name)
    //         );
    //     }
    // }

    // Build section headers from sections in this generic object file.
    // NOTE: If you add other sections first or otherwise change the fact that
    // the ordering of sections in the generic object file matches exactly the
    // ordering of section headers in the ELF file. This fact is used down
    // below to calculate ELF section indices.
    {
        std::optional<decltype(Section::attributes)> current_attributes{};
        for (auto& s : sections) {
            if (current_attributes.has_value() and s.attributes and s.attributes != current_attributes) {
                constexpr usz page_size{0x1000};
                auto address_page_offset = layout.address(s) % page_size;
                auto offset_page_offset = data_offset % page_size;
                if (address_page_offset < offset_page_offset)
                    address_page_offset += page_size;
                const auto offset = address_page_offset - offset_page_offset;
                // fmt::print("v address_page_offset=0x{:08x}\n", address_page_offset);
                // fmt::print("v offset_page_offset=0x{:08x}\n", offset_page_offset);
                // fmt::print(
                //     "advancing data offset from {} to {} ({} difference)\n",
                //     data_offset,
                //     data_offset + offset,
                //     offset
                // );
                data_offset += offset;
            }
            current_attributes = s.attributes;

            elf64_shdr shdr{};

            // fmt::print(
            //     "Data offset of section `{}` (size {}): {}:{}  %4096({}:{})  permissions:{}\n",
            //     s.name,
            //     s.size(),
            //     data_offset,
            //     layout.address(s),
            //     data_offset % 0x1000,
            //     layout.address(s) % 0x1000,
            //     s.attributes
            // );

            shdr.sh_size = s.size();
            shdr.sh_offset = data_offset;

            if (s.is_fill)
                shdr.sh_type = SHT_NOBITS;
            else {
                shdr.sh_type = SHT_PROGBITS;
                data_offset += shdr.sh_size;
            }

            // Memory offset grows for every loaded section
            if (
                s.attribute(Attr::LOAD)
                and kind == GenericObject::Kind::EXECUTABLE
            ) {
                // FIXME: align needed all the time? page size (of target)?
                shdr.sh_addralign = 0x1000;
                shdr.sh_addr = layout.address(s);

                // fmt::print("Placed `{}` address=0x{:08x} offset=0x{:08x}\n", s.name, shdr.sh_addr, shdr.sh_offset);

                // addr % align MUST equal offset % align
                // A % x -> a
                // B % x -> b
                // a == b
                // if a == 42 and b == 108, we need to add (b - a) to A.
                auto address_page_offset = shdr.sh_addr % shdr.sh_addralign;
                auto offset_page_offset = shdr.sh_offset % shdr.sh_addralign;
                if (
                    // OS ignores offset/address page congruency for NOBITS sections...
                    shdr.sh_type != SHT_NOBITS
                    and address_page_offset != offset_page_offset
                ) {
                    if (address_page_offset < offset_page_offset)
                        address_page_offset += shdr.sh_addralign;
                    const auto offset = address_page_offset - offset_page_offset;
                    fmt::print(
                        "Generic2ELF: Fixing up section `{}` address (offset {} doesn't match address offset {})\n"
                        "  NOTE: If you are seeing this, linking is likely broken\n",
                        s.name,
                        offset_page_offset,
                        address_page_offset
                    );
                    // Adjust memory layout, symbols, and relocations, because section has
                    // been relocated by `offset` bytes.
                    shdr.sh_addr += offset;
                }
            }

            if (s.attribute(Attr::WRITABLE))
                shdr.sh_flags |= SHF_WRITE;
            if (s.attribute(Attr::EXECUTABLE))
                shdr.sh_flags |= SHF_EXECINSTR;
            if (s.attribute(Attr::LOAD))
                shdr.sh_flags |= SHF_ALLOC;

            shdr.sh_name = elf_add_string(string_table, s.name);

            // fmt::print(
            //     "clink: Section {}: byte offset 0x{:x}, size {}, address 0x{:08x}, align {}\n",
            //     s.name,
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

            layout.section_offset(s.name, (uint32_t) shdr.sh_offset);
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
            for (auto& s : sections) {
                if (s.name == sym.section_name) {
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

            case Symbol::Kind::WEAK:
                // TODO: Not always an object
                elf_sym.st_info = ELF64_ST_INFO(STB_WEAK, STT_OBJECT);
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
    // Skip symbol table entry in section header table.
    usz string_table_sh_index = shdrs.size() + 1;
    if (emit_relocations) {
        // TODO: relocation section count, not just shove everything in ".rela.text"
        // ".rela.text" added
        ++string_table_sh_index;
    }

    // Symbol Table Section Header
    // shoutout https://stackoverflow.com/q/62497285
    {
        elf64_shdr shdr{};
        shdr.sh_type = SHT_SYMTAB;
        constexpr auto symbol_table_section_name = ".symtab";
        shdr.sh_name = elf_add_string(string_table, symbol_table_section_name);
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

        LCC_ASSERT(
            symbol_table_sh_index == shdrs.size(),
            "AoT calculated symbol table index {} does not match actual string table index {}",
            symbol_table_sh_index,
            shdrs.size()
        );

        layout.section_offset(symbol_table_section_name, (uint32_t) shdr.sh_offset);
        shdrs.emplace_back(shdr);
        data_offset += shdr.sh_size;
    }

    // ".text" relocations section header: ".rela.text"
    if (emit_relocations) {
        // TODO: For executables, we need (to make sure we have) a program header
        // that covers this section...
        // In general, the way that we are handling relocations is ASS. STRAIGHT ASS!

        elf64_shdr shdr{};
        shdr.sh_type = SHT_RELA;
        // TODO: Dynamic, built from actual section name the relocation operates
        // within...
        constexpr auto relocation_section_name = ".rela.text";
        shdr.sh_name = elf_add_string(string_table, relocation_section_name);
        // "If the file has a loadable segment that includes relocation,
        // the sections’ attributes will include the SHF_ALLOC bit;
        // otherwise, that bit will be off."
        // Basically, we should only set this bit if the relocation references a
        // section that is actually loaded. Most of the time, this is the case.
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

        layout.section_offset(relocation_section_name, (uint32_t) shdr.sh_offset);
        shdrs.emplace_back(shdr);
        data_offset += shdr.sh_size;
    }

    // String Table Section Header
    {
        elf64_shdr shdr = {};
        shdr.sh_type = SHT_STRTAB;
        constexpr auto string_table_section_name = ".strtab";
        shdr.sh_name = elf_add_string(string_table, string_table_section_name);
        // FIXME: Ensure no more strings are added to table after this.
        // TODO: Locking vector sort of container needed here as well.
        shdr.sh_size = string_table.size();
        shdr.sh_offset = data_offset;
        // shdr.sh_addr = shdr.sh_offset;
        hdr.e_shstrndx = u16(shdrs.size());

        LCC_ASSERT(
            string_table_sh_index == shdrs.size(),
            "AoT calculated string table index {} does not match actual string table index {}",
            string_table_sh_index,
            shdrs.size()
        );

        layout.section_offset(string_table_section_name, (uint32_t) shdr.sh_offset);
        shdrs.emplace_back(shdr);
        data_offset += shdr.sh_size;
    }

    // Build elf64_rela relocations
    std::vector<elf64_rela> elf_relocations{};
    if (emit_relocations) {
        for (auto& reloc : relocations) {
            // Skip null/invalid entries
            if (reloc.kind == Relocation::Kind::NONE) continue;

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
                case Relocation::Kind::NONE:
                    LCC_UNREACHABLE();

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

                case Relocation::Kind::DISPLACEMENT64:
                    elf_reloc.r_info = ELF64_R_INFO(sym_index, R_X86_64_64);
                    break;

                case Relocation::Kind::DISPLACEMENT32:
                    elf_reloc.r_info = ELF64_R_INFO(sym_index, R_X86_64_32);
                    break;

                case Relocation::Kind::DISPLACEMENT32_GOTPCREL:
                    elf_reloc.r_info = ELF64_R_INFO(sym_index, R_X86_64_GOTPCREL);
                    break;
            }
            elf_relocations.push_back(elf_reloc);
        }
    }

    // Program Headers
    // Basically, we want to create program headers for every span of sections
    // with alike permissions...
    std::vector<elf64_phdr> phdrs{};
    if (kind == Kind::EXECUTABLE) {
        // PT_PHDR program header (tricks kernel into populating auxv[AT_PHDR]
        // with address, required by musl libc).
        // TODO: if (musl or (executable and not static))
        {
            elf64_phdr phdr{};
            phdr.p_type = PT_PHDR;
            phdr.p_offset = hdr.e_phoff;
            phdr.p_vaddr = memory_base + hdr.e_phoff;
            phdr.p_paddr = phdr.p_vaddr;
            phdr.p_align = 8;
            phdr.p_filesz = hdr.e_phnum * hdr.e_phentsize;
            phdr.p_memsz = phdr.p_filesz;
            phdr.p_flags = PF_R;

            phdrs.emplace_back(phdr);
        }

        // PT_LOAD program headers (for loaded sections)
        std::optional<decltype(elf64_shdr::sh_flags)> current_attributes{};
        elf64_phdr phdr{};
        phdr.p_type = PT_NULL;
        bool skip_null{true};
        /** (!) -- This breaks (silently!) if the sections aren't sorted first
         **  by permissions, and then by progbits/nobits within those permission
         **  groups.
         **/
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

            if (
                current_attributes.has_value()
                and s.sh_flags == current_attributes.value()
            ) {
                phdr.p_memsz += s.sh_size;
                if (s.sh_type == SHT_PROGBITS)
                    phdr.p_filesz += s.sh_size;
                continue;
            }

            phdr.p_type = PT_LOAD;
            // File and Memory Offsets
            phdr.p_offset = s.sh_offset;
            phdr.p_memsz = s.sh_size;
            if (s.sh_type == SHT_PROGBITS)
                phdr.p_filesz = phdr.p_memsz;
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

            // First program header. If PT_PHDR present, we want this to include everything from the memory base including
            if (not current_attributes.has_value()) {
                // Include the entire beginning of the file.
                phdr.p_offset = 0;
                phdr.p_vaddr = memory_base;

                phdr.p_filesz += s.sh_offset;

                phdr.p_paddr = phdr.p_vaddr;
                phdr.p_memsz = phdr.p_filesz;
            }

            current_attributes = s.sh_flags;

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

    std::vector<char> out{};
    const auto write_bytes = [&](const auto& v) {
        auto value = v;
        if constexpr (std::endian::native == std::endian::big)
            value = std::byteswap(v);
        const auto* start = reinterpret_cast<const uint8_t*>(&value);
        out.reserve(out.size() + sizeof(value));
        std::copy(start, start + sizeof(value), std::back_inserter(out));
    };
    const auto write_range = [&](const auto& r) {
        const auto* start = reinterpret_cast<const uint8_t*>(r.data());
        const auto size_bytes = r.size() * sizeof(*r.data());
        out.reserve(out.size() + size_bytes);
        std::copy(start, start + size_bytes, std::back_inserter(out));
    };

    // Write ELF header
    write_bytes(hdr);
    // Write program header table
    for (auto& phdr : phdrs)
        write_bytes(phdr);
    // Write section header table
    // fmt::print("section headers at {}, (expect {})\n", out.size(), hdr.e_shoff);
    for (auto& shdr : shdrs)
        write_bytes(shdr);
    // Write sections' data
    // Also emit zero padding for permission boundaries
    {
        std::optional<decltype(Section::attributes)> current_attributes{};
        for (auto [i, s] : std::ranges::views::enumerate(sections)) {
            // +1 skips NULL entry
            auto& shdr = shdrs.at((usz) i + 1);

            if (current_attributes and s.attributes and s.attributes != current_attributes) {
                // Since sh_offset of the current section is already "correct", we
                // actually need to calculate the amount of padding via the offset of
                // the *last* section header + it's size...
                auto& last_shdr = shdrs.at((usz) i);

                constexpr usz page_size{0x1000};
                auto address_page_offset = layout.address(s) % page_size;
                auto offset_page_offset = (last_shdr.sh_offset + last_shdr.sh_size) % page_size;
                if (address_page_offset < offset_page_offset)
                    address_page_offset += page_size;
                const auto offset = address_page_offset - offset_page_offset;
                // fmt::print(
                //     "Permissions changed at section `{}`, emitting {} bytes of padding\n",
                //     s.name,
                //     offset
                // );
                out.resize(out.size() + offset);
            }
            current_attributes = s.attributes;

            if (out.size() != shdr.sh_offset) {
                fmt::print(
                    "File offset {} in section header of section `{}` does not match actual file offset {} ({} difference)\n",
                    shdr.sh_offset,
                    s.name,
                    out.size(),
                    (isz) (shdr.sh_offset - out.size())
                );
            }

            if (
                s.is_fill
                and s.attribute(Attr::LOAD)
            ) {
                if (shdr.sh_type != SHT_NOBITS) {
                    for (usz n = s.length(); n; --n)
                        out.emplace_back(s.value());
                }
            } else write_range(s.contents());
        }
    }

    // Validate symbol table
    {
        auto& symbol_table_header = shdrs.at(symbol_table_sh_index);
        if (symbol_table_header.sh_offset != out.size()) {
            lcc::Diag::ICE(
                "ELF: Actual symbol table offset {} differs from the offset in the symbol table section's header, {}",
                out.size(),
                symbol_table_header.sh_offset
            );
        }
    }
    // Write symbol table ".symtab"
    write_range(syms);

    // Write text section relocations ".rela.text"
    if (emit_relocations)
        write_range(elf_relocations);

    // Validate string table
    {
        auto& string_table_header = shdrs.at(string_table_sh_index);
        if (string_table_header.sh_offset != out.size()) {
            lcc::Diag::ICE(
                "ELF: Actual string table offset {} differs from the offset in the string table section's header, {}",
                out.size(),
                string_table_header.sh_offset
            );
        }
    }
    // Write string table ".strtab"
    // fmt::print("string table starting at {} (header offset {})\n", out.size(), shdrs.at(string_table_sh_index).sh_offset);
    write_range(string_table);
    return out;
}

} // namespace lcc
