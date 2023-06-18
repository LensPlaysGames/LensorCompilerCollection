#ifndef ELF_H
#define ELF_H

#include <stdint.h>

#define EI_CLASS_32BIT 1
#define EI_CLASS_64BIT 2

#define EI_DATA_LITTLE_ENDIAN 1
#define EI_DATA_BIG_ENDIAN 2

#define EI_OSABI_SYSV 0x00

/// Use EI_* macros as an index into elf*_header.e_ident field.

/// Always 0x7f
#define EI_MAG0 0
/// Always 0x45 or 'E'
#define EI_MAG1 1
/// Always 0x4c or 'L'
#define EI_MAG2 2
/// Always 0x46 or 'F'
#define EI_MAG3 3
/// 1 = 32-bit, 2 = 64-bit
#define EI_CLASS 4
/// 1 = little endian, 2 = big endian
#define EI_DATA 5
/// ELF header version
#define EI_VERSION 6
/// See OSABI_* macros for more info.
#define EI_OSABI 7
/// Version field whose meaning changes based on OSABI.
#define EI_ABIVERSION 8
#define EI_PAD 9

/// Unknown
#define ET_NONE 0x00
/// Relocatable
#define ET_REL 0x01
/// Executable
#define ET_EXEC 0x02
/// Shared
#define ET_DYN 0x03
/// CORE
#define ET_CORE 0x04

/// No specific instruction set
#define EM_NONE 0x00
/// x86_64
#define EM_X86_64 0x3e

/// Section header table entry unused
#define SHT_NULL 0x0
///  Program data
#define SHT_PROGBITS 0x1
///  Symbol table
#define SHT_SYMTAB 0x2
///  String table
#define SHT_STRTAB 0x3
///  Relocation entries with addends
#define SHT_RELA 0x4
///  Symbol hash table
#define SHT_HASH 0x5
///  Dynamic linking information
#define SHT_DYNAMIC 0x6
///  Notes
#define SHT_NOTE 0x7
///  Program space with no data (bss)
#define SHT_NOBITS 0x8
///  Relocation entries, no addends
#define SHT_REL 0x9
///  Reserved
#define SHT_SHLIB 0x0A
///  Dynamic linker symbol table
#define SHT_DYNSYM 0x0B
///  Array of constructors
#define SHT_INIT_ARRAY 0x0E
///  Array of destructors
#define SHT_FINI_ARRAY 0x0F
///  Array of pre-constructors
#define SHT_PREINIT_ARRAY 0x10
///  Section group
#define SHT_GROUP 0x11
///  Extended section indices
#define SHT_SYMTAB_SHNDX 0x12
///  Number of defined types.
#define SHT_NUM 0x13
///  Start OS-specific.
#define SHT_LOOS 0x60000000

/// Writable
#define SHF_WRITE 0x1
/// Occupies memory during execution
#define SHF_ALLOC 0x2
/// Executable
#define SHF_EXECINSTR 0x4
/// Might be merged
#define SHF_MERGE 0x10
/// Contains null-terminated strings
#define SHF_STRINGS 0x20
/// 'sh_info' contains SHT index
#define SHF_INFO_LINK 0x40
/// Preserve order after combining
#define SHF_LINK_ORDER 0x80
/// Non-standard OS specific handling required
#define SHF_OS_NONCONFORMING 0x100
/// Section is member of a group
#define SHF_GROUP 0x200
/// Section hold thread-local data
#define SHF_TLS 0x400
/// OS-specific
#define SHF_MASKOS 0x0FF00000
/// Processor-specific
#define SHF_MASKPROC 0xF0000000
/// Special ordering requirement (Solaris)
#define SHF_ORDERED 0x4000000
/// Section is excluded unless referenced or allocated (Solaris)
#define SHF_EXCLUDE 0x8000000

typedef struct elf64_header {
  /// Identifying information. See EI_* macros for more info.
  uint8_t e_ident[16];
  /// Type of file (executable, object, etc). See ET_* macros for more info.
  uint16_t e_type;
  /// Machine that is targeted (instruction set architecture).
  /// See EM_* macros for more info.
  uint16_t e_machine;
  /// Version of ELF this is based upon; set to 1 for original version of ELF.
  uint32_t e_version;
  /// Virtual address of the entry point of the program
  uint64_t e_entry;
  /// Byte offset within file of the program header table.
  uint64_t e_phoff;
  /// Byte offset within file of the section header table.
  uint64_t e_shoff;
  /// Target-architecture dependent flags.
  uint32_t e_flags;
  /// Size of this header (64).
  uint16_t e_ehsize;
  /// Size of each entry within the program header table.
  uint16_t e_phentsize;
  /// Amount of entries present in the program header table.
  uint16_t e_phnum;
  /// Size of each entry within the section header table.
  uint16_t e_shentsize;
  /// Amount of entries present in the section header table.
  uint16_t e_shnum;
  /// Index of the section header table entry that contains the section names.
  uint16_t e_shstrndx;
} elf64_header;

/// Executable
#define PF_X 0b001
/// Writable
#define PF_W 0b010
/// Readable
#define PF_R 0b100
/// OS Mask (these bits are reserved per-os)
#define PF_MASKOS 0x0ff00000
/// Processor mask (these bits are reserved per ISA)
#define PF_MASKPROC 0xf0000000

typedef struct elf64_phdr {
  /// Type of segment. See PT_* macros for more info.
  uint32_t p_type;
  /// Segment-dependent flags. See PF_* macros for more info.
  uint32_t p_flags;
  /// Byte offset of the segment in the file.
  uint64_t p_offset;
  /// Virtual address of the segment in memory.
  uint64_t p_vaddr;
  /// Physical address of the segment in memory.
  uint64_t p_paddr;
  /// Size in bytes of the segment within the file (may be 0).
  uint64_t p_filesz;
  /// Size in bytes of the segment within memory (may be 0).
  /// Segments may be larger in memory than in the file; taken to the
  /// extreme, some segments (like `.bss`) have 0 bytes in the file and a
  /// varying amount in memory.
  uint64_t p_memsz;
  /// 0 and 1 specify NO alignment.
  /// Otherwise should be a positive integral power of 2, with p_vaddr
  /// equating p_offset modulus p_align.
  uint64_t p_align;
} elf64_phdr;

typedef struct elf64_shdr {
  /// Offset into the `.shstrtab` section that represents the name of this section.
  uint32_t sh_name;
  /// Type of this section.
  /// See SHT_* macros for more info.
  uint32_t sh_type;
  /// Attributes of the section.
  /// See SHF_* macros for more info.
  uint64_t sh_flags;
  /// Virtual address of the section in memory (only for sections that are loaded).
  uint64_t sh_addr;
  /// Byte offset of the section data within the file.
  uint64_t sh_offset;
  /// Size in bytes of the section in the file (may be 0).
  uint64_t sh_size;
  /// Contains the section index of an associated section. This field
  /// is used for several purposes, depending on the type of section.
  uint32_t sh_link;
  /// Contains extra information about the section. This field is used
  /// for several purposes, depending on the type of section.
  uint32_t sh_info;
  /// Contains the required alignment of the section. This field must
  /// be a power of two.
  uint64_t sh_addralign;
  /// Contains the size, in bytes, of each entry, for sections that
  /// contain fixed-size entries. Otherwise, this field contains zero.
  uint64_t sh_entsize;
} elf64_shdr;

#define STT_NOTYPE 0x0
/// Symbol is associated with data object
#define STT_OBJECT 0x1
/// Associated with function/executable code
#define STT_FUNC 0x2
/// Associated with a section (normally have STB_LOCAL bindings)
#define STT_SECTION 0x3
/// Symbol's name gives the name of the source file associated with the
/// object file. Must have STB_LOCAL bindings, its section index is
/// SHN_ABS, and it preceds the other STB_LOCAL symbols of the file, if it
/// is present.
#define STT_FILE 0x4
/// Processor specific reserved types (inclusive range)
#define STT_LOPROC 0xe
#define STT_HIPROC 0xf
/// Local symbols are not visible outside the object file containing
/// their definition. Local symbols of the same name may exist in multiple
/// files without interfering with each other.
#define STB_LOCAL 0x0
/// Global symbols are visible to all object files being combined. One
/// file's definition of a global symbol will satisfy another file's
/// undefined reference to the same symbol.
#define STB_GLOBAL 0x1
// Weak symbols resemble global symbols, but their definitions have
// lower precedence.
#define STB_WEAK 0x2
/// Processor specific reserved bindings (inclusive range)
#define STB_LOPROC 0xe
#define STB_HIPROC 0xf

#define ELF64_ST_BIND(st_info) ((st_info) >> 4)
#define ELF64_ST_TYPE(st_info) ((st_info) & 0b1111)
#define ELF64_ST_INFO(binding, type) (((binding) << 4) | ((type) & 0b1111))

// An object file’s symbol table holds information needed to locate and
// relocate a program’s symbolic definitions and references. A symbol
// table index is a subscript into this array. Index 0 both designates the
// first entry in the table and serves as the undefined symbol index.
typedef struct elf64_sym {
  /// Index into symbol string table ".strtab", or zero for unnamed.
  uint32_t st_name;
  /// Contains both type and binding of symbol. See ELF64_ST_*, STT_*,
  // and STB_* macros for more info.
  uint8_t st_info;
  /// Currently zero and has no meaning.
  uint8_t st_other;
  /// Index of section header within section header table that this
  /// symbol is associated with.
  uint16_t st_shndx;
  uint64_t st_value;
  uint64_t st_size;
} elf64_sym;



#define ELF64_R_SYM(r_info) ((r_info) >> 32)
#define ELF64_R_TYPE(r_info) ((uint32_t)(r_info))
#define ELF64_R_INFO(sym, type) (((sym) << 32) | (uint32_t)(type))

/// A: Addend of relocation entries that have them (*_rela).
/// B: Image base where the shared object was loaded in process virtual address space.
/// G: Offset to the GOT relative to the address of the correspondent relocation entry’s symbol.
/// GOT: Address of the Global Offset Table
/// L: Section offset or address of the procedure linkage table (PLT, .got.plt).
/// P: The section offset or address of the storage unit being relocated (or PC).
/// S: Relocation entry’s correspondent symbol value.
/// Z: Size of Relocations entry’s symbol.

#define R_X86_64_NONE 0
// qword S + A
#define R_X86_64_64 1
// dword S + A – P
#define R_X86_64_PC32 2
// dword S + A
#define R_X86_64_32	10
// dword L + A – P
// ffs...
// https://github.com/torvalds/linux/commit/b21ebf2fb4cde1618915a97cc773e287ff49173e
// > As for symbol resolution, PLT32 and PC32 relocations are almost
// > interchangeable. But when linker sees PLT32 relocation against a
// > protected symbol, it can resolved locally at link-time since it is used
// > on a branch instruction. Linker can't do that for PC32 relocation
#define R_X86_64_PLT32 4

///     A relocation section references two other sections: a symbol
/// table and a section to modify. The section header’s sh_info and
/// sh_link members specify these relationships. Relocation entries for
/// different object files have slightly different interpretations for
/// the r_offset member.
///     In relocatable files, r_offset holds a section offset. That is,
/// the relocation section itself describes how to modify another
/// section in the file; relocation offsets designate a storage unit
/// within the second section.
///     In executable and shared object files, r_offset holds a virtual
/// address. To make these files’ relocation entries more useful for
/// the dynamic linker, the section offset (file interpretation) gives
/// way to a virtual address (memory interpretation).
typedef struct elf64_rela {
  uint64_t r_offset;
  /// See ELF64_R_* macros for more info.
  uint64_t r_info;
  int64_t r_addend;
} elf64_rela;

#endif /* ELF_H */
