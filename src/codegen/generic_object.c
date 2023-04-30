#include <codegen/generic_object.h>

#include <codegen/coff.h>

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

/// Write the given generic object file in ELF object file format into
/// a file at the given path.
void generic_object_as_elf_x86_64(GenericObjectFile *object, const char *path) {
  ASSERT(object && path, "Invalid arguments");
  FILE *f = fopen(path, "wb");
  if (!f) {
    print("ERROR: Could not open file at \"%s\"\n", path);
    return;
  }

  TODO("Write an ELF object file...");

  fclose(f);
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

  coff_header hdr = {0};
  // x86_64 magic bytes
  hdr.f_machine = COFF_MACHINE_AMD64;
  // Number of sections in section table
  hdr.f_nscns = (uint16_t)object->sections.size;
  // Number of symbols in symbol table
  // 2 for each section, 1 for each symbol (TODO: Somehow prep symbol
  // table before everything else so that we can know these things for
  // certain, even with aux entries).
  hdr.f_nsyms = (int32_t)((2 * object->sections.size) + object->symbols.size);

  size_t header_size = sizeof(coff_header);
  if (hdr.f_opthdr) header_size += sizeof(coff_opt_header);
  size_t section_table_size = hdr.f_nscns * sizeof(coff_section_header);
  size_t section_table_offset = header_size;
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
  // Skip symbol table; that's where relocations may begin.
  size_t code_relocations_start = data_end + ((unsigned)hdr.f_nsyms * sizeof(coff_symbol_entry));

  // HEADER
  fwrite(&hdr, 1, sizeof(hdr), f);

  // SECTION HEADER TABLE
  data_offset = data_start;
  size_t relocations_offset = code_relocations_start;
  foreach (Section, s, object->sections) {
    coff_section_header shdr = {0};

    strncpy(shdr.s_name, s->name, sizeof(shdr.s_name));

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
    shdr.s_relptr = (int32_t)relocations_offset;
    relocations_offset += relocation_count * sizeof(coff_relocation_entry);

    fwrite(&shdr, 1, sizeof(shdr), f);
  }

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
  size_t symbol_count = 0;
  // Create a symbol entry for each section
  size_t section_header_index = 1; // 1-based
  foreach (Section, s, object->sections) {
    coff_symbol_entry symbol_entry = {0};
    coff_aux_section aux_section = {0};
    strncpy(symbol_entry.n_name, s->name, sizeof(symbol_entry.n_name));
    symbol_entry.n_scnum = (int16_t)section_header_index++;
    symbol_entry.n_sclass = COFF_STORAGE_CLASS_STAT;
    symbol_entry.n_numaux = 1;

    size_t size = 0;
    if (s->attributes & SEC_ATTR_SPAN_FILL)
      size = s->data.fill.amount;
    else size = s->data.bytes.size;
    aux_section.length = (uint32_t)size;

    // Calculate number of relocations for this section.
    uint16_t relocation_count = 0;
    foreach (RelocationEntry, reloc, object->relocs) {
      if (strcmp(reloc->sym.section_name, s->name) == 0)
        ++relocation_count;
    }
    aux_section.number_relocations = relocation_count;

    fwrite(&symbol_entry, 1, sizeof(symbol_entry), f);
    fwrite(&aux_section, 1, sizeof(aux_section), f);
    ++symbol_count;
  }

  size_t symbol_count_after_sections = symbol_count;
  foreach (GObjSymbol, sym, object->symbols) {
    coff_symbol_entry entry = {0};
    // If first four bytes are zero, it's an offset into the string table.
    // Some implementations use a `/<decimal-digits>` format instead.
    // Otherwise, it's the literal name.
    // FIXME: FOR NOW, JUST TRUNCATE (bad bad)
    strncpy(entry.n_name, sym->name, sizeof(entry.n_name));
    // Section number.
    int16_t i = 0;
    size_t sec_data_offset = 0;
    foreach_index (idx, object->sections) {
      if (strcmp(object->sections.data[i].name, sym->section_name) == 0) break;
      size_t size = 0;
      if (object->sections.data[i].attributes & SEC_ATTR_SPAN_FILL)
        size = object->sections.data[i].data.fill.amount;
      else size = object->sections.data[i].data.bytes.size;
      sec_data_offset += size;
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
      // Byte offset within file of this symbol.
      entry.n_value = (int32_t)(data_start + sec_data_offset + sym->byte_offset);
    } break;
    case GOBJ_SYMTYPE_FUNCTION: {
      entry.n_sclass = COFF_STORAGE_CLASS_EXT;
      // Byte offset within text section where entry point lies.
      entry.n_value = (int32_t)sym->byte_offset;
    } break;
    }

    fwrite(&entry, 1, sizeof(entry), f);
    ++symbol_count;
  }

  // TODO: STRING TABLE

  // RELOCATIONS
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

    fwrite(&entry, 1, sizeof(entry), f);
  }

  fclose(f);
}

