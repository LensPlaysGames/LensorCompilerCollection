#ifndef ELF_H
#define ELF_H

#include <stdint.h>

#define EI_CLASS_32BIT 1
#define EI_CLASS_64BIT 2

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
  /// Type of file (executable, object, etc).
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
  /// Index of the section header tanble entry that contains the section names.
  uint16_t e_shstrndx;
} elf64_header;

typedef struct elf64_phdr {
  /// Type of segment. See PT_* macros for more info.
  uint32_t p_type;
  /// Segment-dependent flags.
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

#endif /* ELF_H */
