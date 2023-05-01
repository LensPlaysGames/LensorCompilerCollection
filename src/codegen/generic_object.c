#include <codegen/generic_object.h>

#include <codegen/coff.h>
#include <codegen/elf.h>
#include <vector.h>

Section *code_section(GenericObjectFile *object) {
  return object ? object->sections.data : NULL;
}

Section *get_section_by_name(const Sections sections, const char *name) {
  if (!name) return NULL;
  foreach(Section, s, sections) {
    if (s->name && strcmp(s->name, name) == 0)
      return s;
  }
  return NULL;
}

/// Write 1 byte of data to code.
void sec_write_1(Section *section, uint8_t value) {
  vector_push(section->data.bytes, value);
}
/// Write 2 bytes of data to code.
void sec_write_2(Section *section, uint8_t value0, uint8_t value1) {
  vector_push(section->data.bytes, value0);
  vector_push(section->data.bytes, value1);
}
/// Write 3 bytes of data to code.
void sec_write_3(Section *section, uint8_t value0, uint8_t value1, uint8_t value2) {
  vector_push(section->data.bytes, value0);
  vector_push(section->data.bytes, value1);
  vector_push(section->data.bytes, value2);
}
/// Write 4 bytes of data to code.
void sec_write_4(Section *section, uint8_t value0, uint8_t value1, uint8_t value2, uint8_t value3) {
  vector_push(section->data.bytes, value0);
  vector_push(section->data.bytes, value1);
  vector_push(section->data.bytes, value2);
  vector_push(section->data.bytes, value3);
}
/// Write n bytes of data from buffer to code.
void sec_write_n(Section *section, void* buffer_, size_t n) {
  // TODO: Test if this manual unrolling is actually any faster.
  uint8_t *buffer = (uint8_t*)buffer_;
  for (; n >= 4; n -= 4) {
    vector_push(section->data.bytes, *buffer++);
    vector_push(section->data.bytes, *buffer++);
    vector_push(section->data.bytes, *buffer++);
    vector_push(section->data.bytes, *buffer++);
  }
  for (; n; --n)
    vector_push(section->data.bytes, *buffer++);
}

/// Write 1 byte of data to code.
void mcode_1(GenericObjectFile *object, uint8_t value) {
  sec_write_1(code_section(object), value);
}
/// Write 2 bytes of data to code.
void mcode_2(GenericObjectFile *object, uint8_t value0, uint8_t value1) {
  sec_write_2(code_section(object), value0, value1);
}
/// Write 3 bytes of data to code.
void mcode_3(GenericObjectFile *object, uint8_t value0, uint8_t value1, uint8_t value2) {
  sec_write_3(code_section(object), value0, value1, value2);
}
/// Write 4 bytes of data to code.
void mcode_4(GenericObjectFile *object, uint8_t value0, uint8_t value1, uint8_t value2, uint8_t value3) {
  sec_write_4(code_section(object), value0, value1, value2, value3);
}
/// Write n bytes of data from buffer to code.
void mcode_n(GenericObjectFile *object, void* buffer, size_t n) {
  sec_write_n(code_section(object), buffer, n);
}

/// Append string to given byte buffer and return the index at the
/// beginning of it.
size_t elf_add_string(ByteBuffer* buffer, const char *new_string) {
  ASSERT(buffer && new_string, "Invalid arguments");
  size_t out = buffer->size;
  while (*new_string) vector_push(*buffer, (uint8_t)*new_string++);
  vector_push(*buffer, (uint8_t)*new_string);
  return out;
}

/// Write the given generic object file in ELF object file format into
/// a file at the given path.
void generic_object_as_elf_x86_64(GenericObjectFile *object, const char *path) {
  ASSERT(object && path, "Invalid arguments");
  FILE *f = fopen(path, "wb");
  if (!f) {
    print("ERROR: Could not open file at \"%s\"\n", path);
    return;
  }

  elf64_header hdr = {0};
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
  // Section header table entry count.
  // NULL entry + GObj sections + ".strtab" + ".symtab" + ".rela.text"
  hdr.e_shnum = (uint16_t)(object->sections.size + 4);
  // Index of the section header table entry that contains the section
  // names is set down below.
  hdr.e_shstrndx = 0;

  // NOTE: In object files, there are no program headers. Just sections.

  // Build String Table
  ByteBuffer string_table = {0};
  // NULL entry.
  vector_push(string_table, 0);

  // Collect section headers.
  Vector(elf64_shdr) shdrs = {0};
  // NULL entry
  {
    elf64_shdr shdr = {0};
    vector_push(shdrs, shdr);
  }

  // Byte offset within file that section's data may be placed (must be after all sections).
  // Skip until after the section header table
  size_t data_offset = hdr.e_shoff + (sizeof(elf64_shdr) * hdr.e_shnum);

  // Collect symbols.
  Vector(elf64_sym) syms = {0};
  // NULL entry.
  {
    elf64_sym sym = {0};
    vector_push(syms, sym);
  }

  // Section headers from the given generic object file.
  foreach (Section, s, object->sections) {
    elf64_shdr shdr = {0};

    if (s->attributes & SEC_ATTR_SPAN_FILL) {
      shdr.sh_type = SHT_NOBITS;
      shdr.sh_size = s->data.fill.amount;
    } else {
      shdr.sh_type = SHT_PROGBITS;
      shdr.sh_size = s->data.bytes.size;
      shdr.sh_offset = data_offset;
      data_offset += shdr.sh_size;
    }

    // Assign flags
    if (strcmp(s->name, ".text") == 0)
      shdr.sh_flags |= SHF_ALLOC | SHF_EXECINSTR;
    else if (strcmp(s->name, ".bss") == 0 || strcmp(s->name, ".data") == 0)
      shdr.sh_flags = SHF_ALLOC | SHF_WRITE;
    else ICE("[GObj]:ELF: Unrecognized section in GenericObjectFile: \"%s\"", s->name);

    uint32_t section_name = (uint32_t)elf_add_string(&string_table, s->name);
    shdr.sh_name = section_name;

    // Create symbol for this section.
    {
      elf64_sym sh_sym = {0};
      sh_sym.st_name = section_name;
      sh_sym.st_shndx = (uint16_t)shdrs.size;
      sh_sym.st_info = ELF64_ST_INFO(STB_LOCAL, STT_SECTION);
      vector_push(syms, sh_sym);
    }

    vector_push(shdrs, shdr);
  }

  foreach (GObjSymbol, sym, object->symbols) {
    elf64_sym elf_sym = {0};
    elf_sym.st_name = (uint32_t)elf_add_string(&string_table, sym->name);
    // Get index of section by name
    size_t section_index = 0;
    foreach_index (i, object->sections) {
      if (strcmp(object->sections.data[i].name, sym->section_name) == 0) {
        // Skip NULL entry
        section_index = i + 1;
        break;
      }
    }
    if (!section_index) ICE("Could not find section mentioned by symbol: \"%s\"", sym->section_name);
    elf_sym.st_shndx = (uint16_t)section_index;
    switch (sym->type) {
    case GOBJ_SYMTYPE_STATIC:
      elf_sym.st_info = ELF64_ST_INFO(STB_LOCAL, STT_OBJECT);
      break;
    case GOBJ_SYMTYPE_EXTERNAL:
      elf_sym.st_shndx = 0;
      elf_sym.st_info = ELF64_ST_INFO(STB_GLOBAL, STT_NOTYPE);
      break;
    case GOBJ_SYMTYPE_FUNCTION:
      elf_sym.st_info = ELF64_ST_INFO(STB_GLOBAL, STT_NOTYPE);
      elf_sym.st_value = sym->byte_offset;
      break;
    }
    vector_push(syms, elf_sym);
  }

  // Symbol Table Section Header
  // Index needed by relocation section header(s)
  size_t symbol_table_sh_index = shdrs.size;
  // Skip NULL entry and symbol table entry in section header table.
  size_t string_table_sh_index = object->sections.size + 3;
  {
    elf64_shdr shdr = {0};
    shdr.sh_type = SHT_SYMTAB;
    shdr.sh_name = (uint32_t)elf_add_string(&string_table, ".symtab");
    // FIXME: Ensure no more symbols are added to table after this.
    shdr.sh_size = syms.size * sizeof(elf64_sym);
    shdr.sh_offset = data_offset;

    shdr.sh_link = (uint32_t)string_table_sh_index;
    // One greater than the symbol table index of the last local symbol.
    size_t last_local_index = 0;
    foreach_index (i, syms) {
      elf64_sym *sym = syms.data + i;
      if (ELF64_ST_BIND(sym->st_info) == STB_LOCAL)
        last_local_index = i;
    }
    shdr.sh_info = (uint32_t)last_local_index + 1;

    shdr.sh_entsize = sizeof(elf64_sym);

    vector_push(shdrs, shdr);
    data_offset += shdr.sh_size;
  }

  // ".text" relocations section header: ".rela.text"
  {
    elf64_shdr shdr = {0};
    shdr.sh_type = SHT_RELA;
    shdr.sh_name = (uint32_t)elf_add_string(&string_table, ".rela.text");
    // "If the file has a loadable segment that includes relocation,
    // the sections’ attributes will include the SHF_ALLOC bit;
    // otherwise, that bit will be off."
    shdr.sh_flags |= SHF_ALLOC;

    /// The section header index of the associated symbol table.
    shdr.sh_link = (uint32_t)symbol_table_sh_index;
    /// The section header index of the section to which the relocation
    // applies. (.text in GenericObjectFile is always at index 0, so it's
    // guaranteed that section 1 (first section) is ".text").
    shdr.sh_info = 1;

    shdr.sh_size = object->relocs.size * sizeof(elf64_rela);
    shdr.sh_offset = data_offset;
    shdr.sh_entsize = sizeof(elf64_rela);

    vector_push(shdrs, shdr);
    data_offset += shdr.sh_size;
  }

  // String Table Section Header
  {
    elf64_shdr shdr = {0};
    shdr.sh_type = SHT_STRTAB;
    shdr.sh_name = (uint32_t)elf_add_string(&string_table, ".strtab");
    // FIXME: Ensure no more strings are added to table after this.
    shdr.sh_size = string_table.size;
    shdr.sh_offset = data_offset;

    hdr.e_shstrndx = (uint16_t)shdrs.size;
    vector_push(shdrs, shdr);
    data_offset += shdr.sh_size;
  }

  // Build elf64_rela relocations
  Vector(elf64_rela) relocations = {0};
  foreach (RelocationEntry, reloc, object->relocs) {
    // Find symbol with matching name.
    size_t sym_index = 0;
    foreach_index (i, syms) {
      elf64_sym *sym = syms.data + i;
      char *sym_name = (char*)string_table.data + sym->st_name;
      if (strcmp(sym_name, reloc->sym.name) == 0) {
        sym_index = i;
        break;
      }
    }
    if (sym_index == 0) ICE("Could not find symbol referenced by relocation: \"%s\"", reloc->sym.name);


    elf64_rela elf_reloc = {0};
    elf_reloc.r_offset = reloc->sym.byte_offset;
    switch (reloc->type) {
    case RELOC_DISP32_PCREL:
      elf_reloc.r_info = ELF64_R_INFO(sym_index, R_X86_64_PC32);
      elf_reloc.r_addend = -4;
      break;
    case RELOC_DISP32:
      elf_reloc.r_info = ELF64_R_INFO(sym_index, R_X86_64_32);
      break;
    default: ICE("[GObj]:ELF: Unrecognised relocation type %d\n", (int)reloc->type);
    }
    vector_push(relocations, elf_reloc);
  }

  fwrite(&hdr, 1, sizeof(hdr), f);
  foreach (elf64_shdr, shdr, shdrs) {
    fwrite(shdr, 1, sizeof(*shdr), f);
  }
  // Write data of GObj sections
  foreach (Section, s, object->sections) {
    if (s->attributes & SEC_ATTR_SPAN_FILL) {
      if (strcmp(s->name, ".bss") == 0) continue;
      for (size_t n = s->data.fill.amount; n; --n) {
        fwrite(&s->data.fill.value, 1, 1, f);
      }
    } else {
      fwrite(s->data.bytes.data, 1 ,s->data.bytes.size, f);
    }
  }
  // Write symbol table (".symtab").
  foreach (elf64_sym, sym, syms) {
    fwrite(sym, 1, sizeof(*sym), f);
  }
  // Write text section relocations (".rela.text").
  foreach (elf64_rela, reloc, relocations) {
    fwrite(reloc, 1, sizeof(*reloc), f);
  }
  // Write string table (".strtab").
  foreach (uint8_t, c, string_table) {
    fwrite(c, 1, 1, f);
  }

  fclose(f);

  // Cleanup
  vector_delete(string_table);
  vector_delete(syms);
  vector_delete(shdrs);
}

/// Write the given generic object file in COFF object file format into
/// a file at the given path.
void generic_object_as_coff_x86_64(GenericObjectFile *object, const char *path) {
  ASSERT(object && path, "Invalid arguments");
  FILE *f = fopen(path, "wb");
  if (!f) {
    print("ERROR: Could not open file at \"%s\"\n", path);
    return;
  }

  // PREPARE STRING TABLE
  ByteBuffer string_table = {0};
  // At the beginning of the COFF string table are 4 bytes that contain
  // the total size (in bytes) of the rest of the string table. This size
  // includes the size field itself, so that the value in this location
  // would be 4 if no strings were present.
  vector_push(string_table, 0);
  vector_push(string_table, 0);
  vector_push(string_table, 0);
  vector_push(string_table, 0);

  // PREPARE HEADER
  coff_header hdr = {0};
  // x86_64 magic bytes
  hdr.f_machine = COFF_MACHINE_AMD64;
  // Number of sections in section table
  hdr.f_nscns = (uint16_t)object->sections.size;

  size_t header_size = sizeof(coff_header);
  if (hdr.f_opthdr) header_size += sizeof(coff_opt_header);
  size_t section_table_size = hdr.f_nscns * sizeof(coff_section_header);
  //size_t section_table_offset = header_size;
  size_t section_table_end = header_size + section_table_size;

  // data_start is a byte offset within `out.o` that is past all COFF
  // headers and entries and stuff. This is where actual section data
  // can be written, as well as relocations and line number info.
  size_t data_start = section_table_end;
  size_t data_offset = data_start;
  foreach (Section, s, object->sections) {
    size_t size = 0;
    if (s->attributes & SEC_ATTR_SPAN_FILL)
      size = s->data.fill.amount;
    else size = s->data.bytes.size;
    // TODO: Would be smart to cache section offset somewhere, or something.
    data_offset += size;
  }
  size_t data_end = data_offset;
  // Put symbol table past all data
  hdr.f_symptr = (int32_t)data_end;

  // PREPARE SECTION HEADER TABLE
  Vector(coff_section_header) shdrs = {0};
  data_offset = data_start;
  foreach (Section, s, object->sections) {
    coff_section_header shdr = {0};

    // If first four bytes are zero, it's an offset into the string table.
    // Some implementations use a `/<decimal-digits>` format instead.
    // Otherwise, it's the literal name.
    size_t sec_name_length = strlen(s->name);
    if (sec_name_length <= sizeof(shdr.s_name))
      strncpy(shdr.s_name, s->name, sizeof(shdr.s_name));
    else {
      shdr.s_name_offset = (uint32_t)string_table.size;
      uint8_t *sym_name_it = (uint8_t*)s->name;
      for (size_t i = 0; i < sec_name_length; ++i)
        vector_push(string_table, *sym_name_it++);
      vector_push(string_table, 0);
    }

    size_t size = 0;
    if (s->attributes & SEC_ATTR_SPAN_FILL)
      size = s->data.fill.amount;
    else size = s->data.bytes.size;

    shdr.s_size = (int32_t)size;
    shdr.s_scnptr = (int32_t)data_offset;
    shdr.s_flags = 0;
    if (strcmp(s->name, ".text") == 0)
      shdr.s_flags |= STYP_TEXT | COFF_SCN_MEM_READ | COFF_SCN_MEM_EXECUTE | COFF_SCN_CNT_CODE;
    else if (strcmp(s->name, ".data") == 0)
      shdr.s_flags |= STYP_DATA | COFF_SCN_MEM_READ | COFF_SCN_MEM_WRITE | COFF_SCN_CNT_INIT_DATA;
    else if (strcmp(s->name, ".bss") == 0)
      shdr.s_flags |= STYP_BSS | COFF_SCN_MEM_READ | COFF_SCN_MEM_WRITE | COFF_SCN_CNT_UNINIT_DATA;

    // Calculate number of relocations for this section.
    uint16_t relocation_count = 0;
    foreach (RelocationEntry, reloc, object->relocs) {
      if (strcmp(reloc->sym.section_name, s->name) == 0)
        ++relocation_count;
    }
    shdr.s_nreloc = relocation_count;

    vector_push(shdrs, shdr);
    data_offset += size;
  }

  // PREPARE SYMBOL TABLE
  Vector(coff_symbol_entry) symbol_table = {0};
  // Some symbols have aux entries. This doesn't include those, unlike `symbol_table.size`.
  size_t symbol_count = 0;
  // Create a symbol entry for each section
  size_t section_header_index = 1; // 1-based
  foreach (Section, s, object->sections) {
    coff_symbol_entry symbol_entry = {0};
    coff_symbol_entry aux_section_data = {0};
    coff_aux_section *aux_section = (coff_aux_section*)&aux_section_data;
    // If first four bytes are zero, it's an offset into the string table.
    // Some implementations use a `/<decimal-digits>` format instead.
    // Otherwise, it's the literal name.
    size_t sec_name_length = strlen(s->name);
    if (sec_name_length <= sizeof(symbol_entry.n_name))
      strncpy(symbol_entry.n_name, s->name, sizeof(symbol_entry.n_name));
    else {
      symbol_entry.n_name_offset = (uint32_t)string_table.size;
      uint8_t *sym_name_it = (uint8_t*)s->name;
      for (size_t i = 0; i < sec_name_length; ++i)
        vector_push(string_table, *sym_name_it++);
      vector_push(string_table, 0);
    }
    symbol_entry.n_scnum = (int16_t)section_header_index++;
    symbol_entry.n_sclass = COFF_STORAGE_CLASS_STAT;
    symbol_entry.n_numaux = 1;

    size_t size = 0;
    if (s->attributes & SEC_ATTR_SPAN_FILL)
      size = s->data.fill.amount;
    else size = s->data.bytes.size;
    aux_section->length = (uint32_t)size;

    // Calculate number of relocations for this section.
    uint16_t relocation_count = 0;
    foreach (RelocationEntry, reloc, object->relocs) {
      if (strcmp(reloc->sym.section_name, s->name) == 0)
        ++relocation_count;
    }
    aux_section->number_relocations = relocation_count;

    vector_push(symbol_table, symbol_entry);
    vector_push(symbol_table, aux_section_data);

    ++symbol_count;
  }
  size_t symbol_count_after_sections = symbol_count;

  foreach (GObjSymbol, sym, object->symbols) {
    coff_symbol_entry entry = {0};
    // If first four bytes are zero, it's an offset into the string table.
    // Some implementations use a `/<decimal-digits>` format instead.
    // Otherwise, it's the literal name.
    size_t sym_name_length = strlen(sym->name);
    if (sym_name_length <= sizeof(entry.n_name))
      strncpy(entry.n_name, sym->name, sizeof(entry.n_name));
    else {
      entry.n_name_offset = (uint32_t)string_table.size;
      uint8_t *sym_name_it = (uint8_t*)sym->name;
      for (size_t i = 0; i < sym_name_length; ++i)
        vector_push(string_table, *sym_name_it++);
      vector_push(string_table, 0);
    }
    // Section number.
    int16_t i = 0;
    foreach_index (idx, object->sections) {
      if (strcmp(object->sections.data[i].name, sym->section_name) == 0) break;
      i = (int16_t)idx;
    }
    if (i == (int16_t)object->sections.size) ICE("[GObj]: Couldn't find section mentioned by symbol: \"%s\" (%Z sections)", sym->name, object->sections.size);
    entry.n_scnum = i + 1;

    // MS sets this field to 0x20 for functions, or 0 in all other cases.
    entry.n_type = 0;
    // n_value is only used when n_scnum == N_ABS
    entry.n_value = 0;
    // Number of auxiliary symbol entries that follow this symbol entry.
    entry.n_numaux = 0;

    // Storage class.
    // MS sets this field to four different values, and uses
    // proprietary Visual C++ debug format for most symbolic
    // information: EXTERNAL, STATIC, FUNCTION, and FILE.
    // FIXME: Actually assign based on type of symbol
    switch (sym->type) {
    case GOBJ_SYMTYPE_STATIC: {
      entry.n_sclass = COFF_STORAGE_CLASS_STAT;
      // Byte offset within .bss/.data section where static object lies.
      entry.n_value = (int32_t)sym->byte_offset;
    } break;
    case GOBJ_SYMTYPE_FUNCTION: {
      entry.n_sclass = COFF_STORAGE_CLASS_EXT;
      // Byte offset within text section where entry point lies.
      entry.n_value = (int32_t)sym->byte_offset;
    } break;
    case GOBJ_SYMTYPE_EXTERNAL: {
      entry.n_scnum = 0;
      entry.n_sclass = COFF_STORAGE_CLASS_EXT;
    } break;
    }

    vector_push(symbol_table, entry);
    ++symbol_count;
  }

  hdr.f_nsyms = (int32_t)symbol_table.size;

  // Now that we know the string table won't grow anymore, we may
  // actually know the offset of the relocations that follow it.
  size_t string_table_offset = (size_t)hdr.f_symptr + ((size_t)symbol_table.size * sizeof(*symbol_table.data));
  size_t relocations_offset = string_table_offset + string_table.size;
  foreach (coff_section_header, shdr, shdrs) {
    if (shdr->s_nreloc) {
      shdr->s_relptr = (int32_t)relocations_offset;
      relocations_offset += shdr->s_nreloc + sizeof(coff_relocation_entry);
    }
  }

  // PREPARE RELOCATIONS
  Vector(coff_relocation_entry) relocations = {0};
  foreach (RelocationEntry, reloc, object->relocs) {
    coff_relocation_entry entry = {0};
    // Zero-based index within symbol table to which the reference refers.
    uint32_t i = 0;
    foreach_index (idx, object->symbols) {
      i = (uint32_t)idx;
      GObjSymbol *sym = object->symbols.data + i;
      // FIXME: Use proper symbol comparison or something, like `gobj_symbol_equals(a, b)`.
      if (strcmp(sym->name, reloc->sym.name) == 0) break;
    }
    if (i == object->symbols.size) ICE("[GObj]: Couldn't find symbol mentioned by relocation: \"%s\" (%Z symbols)", reloc->sym.name, object->symbols.size);
    entry.r_vaddr = (uint32_t)(reloc->sym.byte_offset);
    entry.r_symndx = (uint32_t)((symbol_count_after_sections * 2) + i);
    switch (reloc->type) {
    case RELOC_DISP32: entry.r_type = COFF_REL_AMD64_ADDR32; break;
    case RELOC_DISP32_PCREL: entry.r_type = COFF_REL_AMD64_REL32; break;
    default: ICE("Unhandled relocation type: %d\n", (int)reloc->type);
    }

    vector_push(relocations, entry);
  }

  // HEADER
  fwrite(&hdr, 1, sizeof(hdr), f);
  // SECTION HEADER TABLE
  fwrite(shdrs.data, shdrs.size, sizeof(*shdrs.data), f);
  // SECTION DATA
  foreach (Section, s, object->sections) {
    if (s->attributes & SEC_ATTR_SPAN_FILL) {
      for (size_t n = s->data.fill.amount; n; --n) {
        fwrite(&s->data.fill.value, 1, 1, f);
      }
    } else {
      fwrite(s->data.bytes.data, 1, s->data.bytes.size, f);
    }
  }
  // SYMBOL TABLE
  fwrite(symbol_table.data, symbol_table.size, sizeof(*symbol_table.data), f);
  // STRING TABLE
  uint32_t string_table_size = (uint32_t)string_table.size;
  fwrite(&string_table_size, 1, 4, f);
  fwrite(string_table.data + 4, 1, string_table_size - 4, f);
  // RELOCATIONS
  fwrite(relocations.data, relocations.size, sizeof(*relocations.data), f);

  fclose(f);

  vector_delete(relocations);
  vector_delete(string_table);
  vector_delete(symbol_table);
  vector_delete(shdrs);
}

