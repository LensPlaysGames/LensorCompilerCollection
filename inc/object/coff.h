#ifndef COFF_H
#define COFF_H

#include <stdint.h>

// Layout:
// - Header
// - Optional Header
// - Section Header
// The section table (array of section headers) must immediately follow
// any and all headers present.
//
// - Section Relocation Table
// Can be placed anywhere, offset is directly stored in each section's
// header.
// - Section Line Number Table
// Can be placed anywhere, offset is directly stored in each section's
// header.
//
// - Symbol Table
// Can be placed anywhere, offset is directly stored in file header.
// - String Table
// The string table must immediately follow the symbol table, as it is
// found by taking the amount of symbols past the symbol table pointer.
//
// The string table begins with 4 bytes that contain the total size in
// bytes of the rest of the string table, including these 4 bytes. An
// empty string table consists of four bytes, the last of which has the
// value 4. Following the size are null-terminated strings (referenced by
// index via symbols).
//
// Here in LCC, we choose to layout things like the following:
// - HEADERS
// - SECTION'S DATA
// - SYMBOL TABLE
// - STRING TABLE
// - SECTION RELOCATION TABLES

// Machine
// Applicable to any machine
#define IMAGE_FILE_MACHINE_UNKNOWN 0x0
// Machine
// x86_64, x64 x8664
#define IMAGE_FILE_MACHINE_AMD64 0x8664
// Machine
// little endian
#define IMAGE_FILE_MACHINE_ARM64 0xaa64

// Characteristic
#define IMAGE_FILE_RELOCATIONS_STRIPPED 0x0001
// Characteristic
#define IMAGE_FILE_IS_EXECUTABLE 0x0002
// Characteristic
#define IMAGE_FILE_LINE_NUMBERS_STRIPPED 0x0004
// Characteristic
#define IMAGE_FILE_LOCAL_SYMBOLS_STRIPPED 0x0008
// Characteristic
// Addresses >2gb
#define IMAGE_FILE_LARGE_ADDRESS_AWARE 0x0020
// Characteristic
#define IMAGE_FILE_DEBUG_STRIPPED 0x0200
// Characteristic
#define IMAGE_FILE_DLL 0x2000

// Section Flag
// Section contains executable code.
#define IMAGE_SCN_CNT_CODE 0x20
// Section Flag
// Section contains initialised data.
#define IMAGE_SCN_CNT_INITIALIZED_DATA 0x40
// Section Flag
// Section contains uninitialised data.
#define IMAGE_SCN_CNT_UNINITIALIZED_DATA 0x80
// Section Flag
// Section contains comments or other info.
// Not valid for executable images.
#define IMAGE_SCN_LNK_INFO 0x200
// Section Flag
// Section will be removed at link time.
// Not valid for executable images.
#define IMAGE_SCN_LNK_REMOVE 0x800
// Section Flag
// Section may be shared in memory.
#define IMAGE_SCN_MEM_SHARED 0x10000000
// Section Flag
// Section may be executed as code.
#define IMAGE_SCN_MEM_EXECUTE 0x20000000
// Section Flag
// Section may be read.
#define IMAGE_SCN_MEM_READ 0x40000000
// Section Flag
// Section may be written to.
#define IMAGE_SCN_MEM_WRITE 0x80000000

typedef struct coff_header {
    // IMAGE_FILE_MACHINE_*
    uint16_t machine;
    uint16_t number_of_sections;
    uint32_t time_and_date;
    uint32_t symbol_table_offset;
    uint32_t number_of_symbols;
    // zero for object files
    uint16_t size_of_optional_header;
    // IMAGE_FILE_*
    uint16_t characteristics;
} coff_header;

// Only for executables
typedef struct coff_optional_header_pe32plus {
    // PE32:  0x010b
    // PE32+: 0x020b
    uint16_t magic;
    uint8_t major_linker_version;
    uint8_t minor_linker_version;
    // Sum of the size of all code sections.
    uint32_t code_size;
    // Sum of the size of all initialised data sections.
    uint32_t data_size;
    // Sum of the size of all *un*initialised data sections.
    uint32_t bss_size;
    uint32_t entry_point;
    // Address of entry point relative to image base when executable is loaded
    // into memory. For program images, this _is_ the starting address.
    uint32_t text_start;
} coff_optional_header_pe32plus;

typedef struct coff_optional_header_pe32 {
    coff_optional_header_pe32plus base_optional_header;
    // Address relative to image base of the beginning of the code section
    // when it is loaded into memory.
    uint32_t data_start;
} coff_optional_header_pe32;

#define IMAGE_SUBSYSTEM_UNKNOWN                 0
#define IMAGE_SUBSYSTEM_NATIVE                  1
#define IMAGE_SUBSYSTEM_WINDOWS_GUI             2
#define IMAGE_SUBSYSTEM_WINDOWS_CUI             3
#define IMAGE_SUBSYSTEM_POSIX_CUI               7
#define IMAGE_SUBSYSTEM_EFI_APPLICATION         10
#define IMAGE_SUBSYSTEM_EFI_DRIVER_WITH_BOOT    11
#define IMAGE_SUBSYSTEM_EFI_DRIVER_WITH_RUNTIME 12
#define IMAGE_SUBSYSTEM_EFI_ROM                 13

// Additional extension to optional header
typedef struct coff_optional_header_pe32plus_windows {
    // Preferred address of first byte of image when loaded into memory; must
    // be a multiple of 64 kilobytes. The default for DLLs is 0x10000000.
    uint64_t base_address;
    // Align in bytes of sections when loaded into memory.
    // Must be >= file_align
    uint32_t section_align;
    // Align of raw data of sections in image file. Default is 512.
    uint32_t file_align;
    uint16_t major_os_version;
    uint16_t minor_os_version;
    uint16_t major_image_version;
    uint16_t minor_image_version;
    uint16_t major_subsystem_version;
    uint16_t minor_subsystem_version;
    uint32_t reserved0;
    // Size in bytes of entire image (all headers), as it would be loaded into
    // memory. Must be a multiple of 'section_align'.
    uint32_t image_size;
    // Combined size of MS-DOS stub, PE header, and section headers rounded up
    // to a multiple of 'file_align'.
    uint32_t header_size;
    uint32_t checksum;
    // IMAGE_SUBSYSTEM_*
    uint16_t subsystem;
    uint16_t dll_characteristics;
    uint64_t stack_size_max;
    uint64_t stack_size_initial;
    uint64_t heap_size_max;
    uint64_t heap_size_initial;
    uint32_t reserved1;
    uint32_t number_of_data_dir_entries;
} coff_optional_header_pe32plus_windows;

typedef struct coff_data_dir_entry {
    // Address to the table relative to image's base address when the table is
    // loaded.
    uint32_t virtual_address;
    uint32_t size;
} coff_data_dir_entry;

typedef struct coff_section_header {
    // The $ character has a special interpretation in section names in object
    // files. The linker discards the "$" and all characters that follow it. A
    // section named .text$foo is actually placed in the .text section in the
    // image. The characters following the "$" determine the ordering of the
    // subsections, lexically.
    char name[8];
    // NOTE: Only valid for executable images; otherwise it should be zero.
    // The total size of the section when loaded into memory. If this value is
    // greater than size_in_bytes, the section is zero-padded.
    uint32_t virtual_size;
    // For executable images, the address of the first byte of the section
    // relative to the image base when the section is loaded into memory.
    // Basically, the offset from the image base that this section will be
    // loaded at. "Compilers should set this to zero".
    uint32_t virtual_address;
    // Amount of bytes at data_offset that are part of this section.
    uint32_t size_in_bytes;
    // For executable images, must be a multiple of file_align.
    // For object files, the value should be 4-byte aligned.
    // For uninitialised sections, this should be zero.
    uint32_t data_offset;
    // For executable images, must be zero.
    uint32_t relocation_table_offset;
    // Deprecated; should always be zero.
    uint32_t line_number_table_offset;
    // For executable images, must be zero.
    uint16_t number_of_relocation_entries;
    uint16_t number_of_line_number_entries;
    uint32_t flags;
} coff_section_header;

// Section Type: Text
#define STYP_TEXT 0x0020
// Section Type: Data
#define STYP_DATA 0x0040
// Section Type: BSS
#define STYP_BSS 0x0080

// 32-bit virtual address
#define IMAGE_REL_AMD64_ADDR32 0x0002
// 32-bit address without an image base (RVA)
#define IMAGE_REL_AMD64_ADDR32NB 0x0003
// 32-bit relative address
#define IMAGE_REL_AMD64_REL32 0x0004

typedef struct coff_relocation_entry {
    int32_t reference_address;
    uint32_t symbol_index;
    // One of IMAGE_REL_*
    uint16_t type;
} coff_relocation_entry;

typedef struct coff_line_number_entry {
    union {
        uint32_t symbol_index;
        uint32_t physical_address;
    } address;
    uint16_t line_number;
} coff_line_number_entry;

// Storage Class: External (C_EXT)
//   section == 0:
//     value == 0: unresolved external symbol (please linker help)
//     value > 0:  Uninitialised global (not in BSS).
//                 Value is size of variable in bytes.
//   section == .text:
//     value == offset into section: function entry point.
//   section == .data:
//     value == offset into section: initialised global variable.
// Storage Class: Static (C_STAT)
//   section == .text or .data or .bss
//     value == 0: section symbol indicating start of section.
//   section == .data
//     value > 0: initialised static variable.
//   section == .bss
//     value > 0: uninitialised static variable.
typedef struct coff_symbol_entry {
    char name[8];
    uint32_t value;
    // When positive, a 1-based position ("index") into the section table
    // where this symbol is defined.
    // N_DEBUG (-2) -> Debugging symbol.
    // N_ABS   (-1) -> Absolute symbol. Value field is absolute.
    // N_UNDEF  (0) -> External symbol.
    int16_t section;
    // MS tools set this to 0x20 for functions or 0x0 for anything else.
    uint16_t type;
    // C_EXT -> External
    // C_STAT -> Static
    uint8_t storage_class;
    uint8_t auxiliary_count;
} coff_symbol_entry;

// Symbol Section Value
// Debugging symbol.
#define N_DEBUG -2
// Symbol Section Value
// Absolute symbol. Value field is absolute, and is not "within" any
// section.
#define N_ABS -1
// Symbol Section Value
// External symbol (not defined in this image).
#define N_UNDEF 0

// Storage Class: External
#define C_EXT 2
// Storage Class: Static
#define C_STAT 3

typedef struct coff_string_entry {
    union {
        char name[8];
        struct
        {
            uint32_t zeroes;
            uint32_t offset;
        };
    };
} coff_string_entry;

#endif /* COFF_H */
