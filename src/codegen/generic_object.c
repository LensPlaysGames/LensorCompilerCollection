#include <codegen/generic_object.h>

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
