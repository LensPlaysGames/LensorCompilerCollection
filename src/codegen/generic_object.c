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
void generic_object_as_elf(GenericObjectFile *object, const char *path) {
  ASSERT(object && path, "Invalid arguments");
  FILE *f = fopen(path, "wb");
  if (!f) {
    print("ERROR: Could not open file at \"%s\"\n", path);
    return;
  }

  // TODO: Write an ELF object file...

  fclose(f);
}

/// Write the given generic object file in COFF object file format into
/// a file at the given path.
void generic_object_as_coff(GenericObjectFile *object, const char *path) {
  ASSERT(object && path, "Invalid arguments");
  FILE *f = fopen(path, "wb");
  if (!f) {
    print("ERROR: Could not open file at \"%s\"\n", path);
    return;
  }

  coff_header hdr = {0};
  // x86_64 magic bytes
  hdr.f_magic = 0x8664;
  // Number of sections in section table
  hdr.f_nscns = (uint16_t)object->sections.size;
  // Number of symbols in symbol table
  hdr.f_nsyms = (uint16_t)object->symbols.size;
  hdr.f_flags |= HDR_RELOC_STRIPPED | HDR_LINE_NUMS_STRIPPED;

  size_t header_size = sizeof(coff_header);
  if (hdr.f_opthdr) header_size += sizeof(coff_opt_header);
  size_t section_table_size = hdr.f_nscns * sizeof(coff_section_header);
  size_t section_table_offset = header_size;
  size_t section_table_end = header_size + section_table_size;

  // data_start is a byte offset within `out.o` that is past all COFF
  // headers and entries and stuff. This is where actual section data
  // can be written, as well as relocations and line number info.
  size_t data_start = section_table_end;

  // HEADER
  fwrite(&hdr, 1, sizeof(hdr), f);

  // SECTION HEADER TABLE
  size_t data_offset = data_start;
  foreach (Section, s, object->sections) {
    coff_section_header shdr = {0};

    strncpy(shdr.s_name, s->name, sizeof(shdr.s_name));

    size_t size = 0;
    if (s->attributes & SEC_ATTR_SPAN_FILL)
      size = s->data.fill.amount;
    else size = s->data.bytes.size;

    shdr.s_size = (int32_t)size;
    shdr.s_scnptr = (int32_t)data_offset;
    shdr.s_flags = SCN_MEM_READ;
    if (strcmp(s->name, ".text") == 0)
      shdr.s_flags |= STYP_TEXT | SCN_MEM_EXECUTE;
    else if (strcmp(s->name, ".data") == 0)
      shdr.s_flags |= STYP_DATA | SCN_MEM_WRITE;
    else if (strcmp(s->name, ".bss") == 0)
      shdr.s_flags |= STYP_BSS | SCN_MEM_WRITE;

    fwrite(&shdr, 1, sizeof(shdr), f);

    data_offset += size;
  }
  size_t data_end = data_offset;

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
  // Put symbol table past all data
  hdr.f_symptr = (int32_t)data_end;
  // Create a symbol entry for each section
  size_t section_header_index = 1; // 1-based
  foreach (Section, s, object->sections) {
    coff_symbol_entry symbol_entry = {0};
    coff_aux_section aux_section = {0};
    strncpy(symbol_entry.n_name, s->name, sizeof(symbol_entry.n_name));
    symbol_entry.n_scnum = (int16_t)section_header_index;
    symbol_entry.n_sclass = C_STAT;
    symbol_entry.n_numaux = 1;

    size_t size = 0;
    if (s->attributes & SEC_ATTR_SPAN_FILL)
      size = s->data.fill.amount;
    else size = s->data.bytes.size;
    aux_section.length = (uint32_t)size;

    fwrite(&symbol_entry, 1, sizeof(symbol_entry), f);
    fwrite(&aux_section, 1, sizeof(aux_section), f);
  }

  // TODO: foreach (Symbol, sym, object->symbols) emit_coff_symbol(sym)

  // TODO: foreach (RelocationEntry, reloc, object->relocs) emit_coff_relocation(reloc)

  fclose(f);
}

