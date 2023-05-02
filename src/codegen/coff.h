#ifndef COFF_H
#define COFF_H

#include <stdint.h>

// 0x0001 IMAGE_FILE_RELOCS_STRIPPED
#define HDR_RELOC_STRIPPED     0b0000000000000001
// 0x0002 IMAGE_FILE_EXECUTABLE_IMAGE
#define HDR_EXECUTABLE         0b0000000000000010
// 0x0004 IMAGE_FILE_LINE_NUMS_STRIPPED
#define HDR_LINE_NUMS_STRIPPED 0b0000000000000100
// 0x0008 IMAGE_FILE_LOCAL_SYMS_STRIPPED
#define HDR_LOCALS_STRIPPED    0b0000000000001000
// 0x0010 IMAGE_FILE_AGGRESSIVE_WS_TRIM
// This flag is deprecated for Windows 2000 and later and must be zero.
#define HDR_AGGRESSIVE_WS_TRIM 0b0000000000010000
// 0x0020 IMAGE_FILE_LARGE_ADDRESS_AWARE
// Application can handle > 2-GB addresses.
#define HDR_LARGE_ADDRESSES    0b0000000000100000
// NOTE: 0x0040 is reserved for future use
// 0x0080 IMAGE_FILE_BYTES_REVERSED_LO Little endian
// This flag is deprecated and should be zero.
#define HDR_LITTLE_ENDIAN      0b0000000010000000
// 0x0100 IMAGE_FILE_32BIT_MACHINE
#define HDR_32BIT              0b0000000100000000
// 0x0200 IMAGE_FILE_DEBUG_STRIPPED
// Debugging information is removed from the image file.
#define HDR_DEBUG_STRIPPED     0b0000001000000000
// 0x0400 IMAGE_FILE_REMOVABLE_RUN_FROM_SWAP
// If the image is on removable media, fully load it and copy it to the swap file.
#define HDR_REMOVABLE_DO_SWAP  0b0000010000000000
// 0x0800 IMAGE_FILE_NET_RUN_FROM_SWAP
// If the image is on network media, fully load it and copy it to the swap file.
#define HDR_NET_DO_SWAP        0b0000100000000000
// 0x1000 IMAGE_FILE_SYSTEM
// The image file is a system file, not a user program.
#define HDR_SYSTEM             0b0001000000000000
// 0x2000 IMAGE_FILE_DLL
// The image file is a dynamic-link library (DLL). Such files are
// considered executable files for almost all purposes, although they
// cannot be directly run.
#define HDR_DLL                0b0010000000000000
// 0x8000 IMAGE_FILE_BYTES_REVERSED_HI Big endian
// This flag is deprecated and should be zero.
#define HDR_BIG_ENDIAN         0b1000000000000000

#define COFF_MACHINE_UNKNOWN 0
#define COFF_MACHINE_AMD64 0x8664

typedef struct coff_header {
  // Machine type
  // 0x0    IMAGE_FILE_MACHINE_UNKNOWN
  // 0x184  IMAGE_FILE_MACHINE_ALPHA Alpha AXP, 32-bit address space
  // 0x284  IMAGE_FILE_MACHINE_ALPHA64 Alpha 64, 64-bit address space
  // 0x1d3  IMAGE_FILE_MACHINE_AM33 Matsushita AM33
  // 0x8664 IMAGE_FILE_MACHINE_AMD64 x64
  // 0x1c0  IMAGE_FILE_MACHINE_ARM ARM little endian
  // 0xaa64 IMAGE_FILE_MACHINE_ARM64 ARM64 little endian
  // 0x1c4  IMAGE_FILE_MACHINE_ARMNT ARM Thumb-2 little endian
  // 0x284  IMAGE_FILE_MACHINE_AXP64 AXP 64 (Same as Alpha 64)
  // 0xebc  IMAGE_FILE_MACHINE_EBC EFI byte code
  // 0x14c  IMAGE_FILE_MACHINE_I386 Intel 386 or later processors and compatible processors
  // 0x200  IMAGE_FILE_MACHINE_IA64 Intel Itanium processor family
  // 0x6232 IMAGE_FILE_MACHINE_LOONGARCH32 LoongArch 32-bit processor family
  // 0x6264 IMAGE_FILE_MACHINE_LOONGARCH64 LoongArch 64-bit processor family
  // 0x9041 IMAGE_FILE_MACHINE_M32R Mitsubishi M32R little endian
  // 0x266  IMAGE_FILE_MACHINE_MIPS16 MIPS16
  // 0x366  IMAGE_FILE_MACHINE_MIPSFPU MIPS with FPU
  // 0x466  IMAGE_FILE_MACHINE_MIPSFPU16 MIPS16 with FPU
  // 0x1f0  IMAGE_FILE_MACHINE_POWERPC Power PC little endian
  // 0x1f1  IMAGE_FILE_MACHINE_POWERPCFP Power PC with floating point support
  // 0x166  IMAGE_FILE_MACHINE_R4000 MIPS little endian
  // 0x5032 IMAGE_FILE_MACHINE_RISCV32 RISC-V 32-bit address space
  // 0x5064 IMAGE_FILE_MACHINE_RISCV64 RISC-V 64-bit address space
  // 0x5128 IMAGE_FILE_MACHINE_RISCV128 RISC-V 128-bit address space
  // 0x1a2  IMAGE_FILE_MACHINE_SH3 Hitachi SH3
  // 0x1a3  IMAGE_FILE_MACHINE_SH3DSP Hitachi SH3 DSP
  // 0x1a6  IMAGE_FILE_MACHINE_SH4 Hitachi SH4
  // 0x1a8  IMAGE_FILE_MACHINE_SH5 Hitachi SH5
  // 0x1c2  IMAGE_FILE_MACHINE_THUMB Thumb
  // 0x169  IMAGE_FILE_MACHINE_WCEMIPSV2 MIPS little-endian WCE v2
  uint16_t f_machine;
  /// Number of sections in section table.
  uint16_t f_nscns;
  /// time_t creation time.
  int32_t f_timdat;
  /// Offset within file where symbol table begins.
  int32_t f_symptr;
  /// Number of symbols in symbol table.
  int32_t f_nsyms;
  /// If non-zero, an optional header can be found right after this
  /// header.
  uint16_t f_opthdr;
  // NOTE: Also known as "characteristics" field in some docs.
  // See HDR_* defines above
  uint16_t f_flags;
} __attribute__((packed)) coff_header;

typedef struct coff_opt_header {
  uint16_t magic;
  /// Version stamp.
  uint16_t vstamp;
  /// Text size in bytes.
  uint32_t tsize;
  /// Data size in bytes.
  uint32_t dsize;
  /// BSS size in bytes.
  uint32_t bsize;
  uint32_t entry;
  uint32_t text_start;
  uint32_t data_start;
} __attribute__((packed)) coff_opt_header;

// Regular section (allocated, relocated, loaded)
#define STYP_REG    0b00000000
// Dummy section (not allocated, relocated, not loaded)
#define STYP_DSECT  0b00000001
// Noload section (allocated, relocated, not loaded)
#define STYP_NOLOAD 0b00000010
// Grouped section (formed from input sections)
#define STYP_GROUP  0b00000100
// Padding section (not allocated, not relocated, loaded)
#define STYP_PAD    0b00001000
// Copy section ("for a decision function used in updating fields"; not
// allocated, not relocated, loaded, relocation and line number entries
// processed normally)
#define STYP_COPY   0b00010000
// Section contains exectuable text
#define STYP_TEXT   0b00100000
// Section contains initialised data
#define STYP_DATA   0b01000000
// Section contains uninitialised data
#define STYP_BSS    0b10000000
// Comment section (not allocated, not relocated, not loaded)
#define STYP_INFO   (0b00000001 << 8)
// Overlay section (relocated, not allocated, not loaded)
#define STYP_OVER   (0b00000010 << 8)
// For .lib section (treated like STYP_INFO)
#define STYP_LIB    (0b00000100 << 8)

// 0x00000020 IMAGE_SCN_CNT_CODE
// The section contains executable code.
#define COFF_SCN_CNT_CODE 0x00000020
#define COFF_SCN_CNT_INIT_DATA 0x00000040
#define COFF_SCN_CNT_UNINIT_DATA 0x00000080
// 0x10000000 IMAGE_SCN_MEM_SHARED
// The section can be shared in memory.
#define COFF_SCN_MEM_SHARED 0x10000000
// 0x20000000 IMAGE_SCN_MEM_EXECUTE
// The section can be executed as code.
#define COFF_SCN_MEM_EXECUTE 0x20000000
// 0x40000000 IMAGE_SCN_MEM_READ
// The section can be read.
#define COFF_SCN_MEM_READ 0x40000000
// 0x80000000 IMAGE_SCN_MEM_WRITE
// The section can be written to.
#define COFF_SCN_MEM_WRITE 0x80000000

typedef struct coff_section_header {
  /// Section Name
  union {
    char s_name[8];
    struct {
      uint32_t s_name_zeroes;
      uint32_t s_name_offset;
    };
  };
  /// Physical Address
  int32_t s_paddr;
  /// Virtual Address
  int32_t s_vaddr;
  /// Section Size in Bytes
  int32_t s_size;
  /// File offset to the Section data
  int32_t s_scnptr;
  /// File offset to the Relocation table for this Section
  int32_t s_relptr;
  /// File offset to the Line Number table for this Section
  int32_t s_lnnoptr;
  /// Number of Relocation table entries
  uint16_t s_nreloc;
  /// Number of Line Number table entries
  uint16_t s_nlnno;
  int32_t s_flags;
} __attribute__((packed)) coff_section_header;

// x86_64 Absolute Relocation (the relocation is ignored)
#define COFF_REL_AMD64_ABS 0x0000
// 0x0001 IMAGE_REL_AMD64_ADDR64
// The 64-bit VA of the relocation target.
#define COFF_REL_AMD64_ADDR64 0x0001
// 0x0002 IMAGE_REL_AMD64_ADDR32
// The 32-bit VA of the relocation target.
#define COFF_REL_AMD64_ADDR32 0x0002
// 0x0003 IMAGE_REL_AMD64_ADDR32NB
// The 32-bit address without an image base (RVA).
#define COFF_REL_AMD64_ADDR32NB 0x0003
// 0x0004 IMAGE_REL_AMD64_REL32
// The 32-bit relative address from the byte following the relocation.
#define COFF_REL_AMD64_REL32 0x0004
// 0x0005 IMAGE_REL_AMD64_REL32_1
// The 32-bit address relative to byte distance 1 from the relocation.
#define COFF_REL_AMD64_1 0x0005
// 0x0006 IMAGE_REL_AMD64_REL32_2
// The 32-bit address relative to byte distance 2 from the relocation.
#define COFF_REL_AMD64_2 0x0006
// 0x0007 IMAGE_REL_AMD64_REL32_3
// The 32-bit address relative to byte distance 3 from the relocation.
#define COFF_REL_AMD64_3 0x0007
// 0x0008 IMAGE_REL_AMD64_REL32_4
// The 32-bit address relative to byte distance 4 from the relocation.
#define COFF_REL_AMD64_4 0x0008
// 0x0009 IMAGE_REL_AMD64_REL32_5
// The 32-bit address relative to byte distance 5 from the relocation.
#define COFF_REL_AMD64_5 0x0009
// 0x000A IMAGE_REL_AMD64_SECTION
// The 16-bit section index of the section that contains the target.
// This is used to support debugging information.
#define COFF_REL_AMD64_SECTION 0x000A
// 0x000B IMAGE_REL_AMD64_SECREL
// The 32-bit offset of the target from the beginning of its section.
// This is used to support debugging information and static thread local storage.
#define COFF_REL_AMD64_SECREL 0x000B
// 0x000C IMAGE_REL_AMD64_SECREL7
// A 7-bit unsigned offset from the base of the section that contains the target.
#define COFF_REL_AMD64_SECREL7 0x000C
// 0x000D IMAGE_REL_AMD64_TOKEN
// CLR tokens.
#define COFF_REL_AMD64_TOKEN 0x000D
// 0x000E IMAGE_REL_AMD64_SREL32
// A 32-bit signed span-dependent value emitted into the object.
#define COFF_REL_AMD64_SREL32 0x000E
// 0x000F IMAGE_REL_AMD64_PAIR
// A pair that must immediately follow every span-dependent value.
#define COFF_REL_AMD64_PAIR 0x000F
// 0x0010 IMAGE_REL_AMD64_SSPAN32
// A 32-bit signed span-dependent value that is applied at link time.
#define COFF_REL_AMD64_SSPAN32 0x0010

typedef struct coff_relocation_entry {
  /// Reference Virtual Address
  uint32_t r_vaddr;
  /// Index of symbol
  uint32_t r_symndx;
  /// Type of relocation
  uint16_t r_type;
} __attribute__((packed)) coff_relocation_entry;

typedef struct coff_line_number_entry {
  union {
    /// Index of symbol
    uint32_t l_symndx;
    /// Physical address
    uint32_t l_paddr;
  } l_addr;
  /// Line number
  uint16_t l_lnno;
} __attribute__((packed)) coff_line_number_entry;

// No assigned storage class.
#define COFF_STORAGE_CLASS_NULL 0
// The automatic (stack) variable. The Value field specifies the stack frame offset.
#define COFF_STORAGE_CLASS_AUTO 1
// A value that Microsoft tools use for external symbols. The Value
// field indicates the size if the section number is IMAGE_SYM_UNDEFINED (0).
// If the section number is not zero, then the Value field specifies
// the offset within the section.
#define COFF_STORAGE_CLASS_EXT 2
// The offset of the symbol within the section. If the Value field is
// zero, then the symbol represents a section name.
#define COFF_STORAGE_CLASS_STAT 3
// A register variable. The Value field specifies the register number.
#define COFF_STORAGE_CLASS_REG 4
// A symbol that is defined externally.
#define COFF_STORAGE_CLASS_EXTDEF 5
// A code label that is defined within the module. The Value field
// specifies the offset of the symbol within the section.
#define COFF_STORAGE_CLASS_LABEL 6
// A reference to a code label that is not defined.
#define COFF_STORAGE_CLASS_ULABEL 7
// The structure member. The Value field specifies the n th member.
#define COFF_STORAGE_CLASS_MOS 8
// 9 IMAGE_SYM_CLASS_ARGUMENT
// A formal argument (parameter) of a function. The Value field specifies the n th argument.
#define COFF_STORAGE_CLASS_ARG 9
// 10 IMAGE_SYM_CLASS_STRUCT_TAG
// The structure tag-name entry.
#define COFF_STORAGE_CLASS_STRUCT_TAG 10
// 11 IMAGE_SYM_CLASS_MEMBER_OF_UNION
// A union member. The Value field specifies the n th member.
#define COFF_STORAGE_CLASS_MOU 11
// 12 IMAGE_SYM_CLASS_UNION_TAG
// The Union tag-name entry.
#define COFF_STORAGE_CLASS_UNION_TAG 12
// 13 IMAGE_SYM_CLASS_TYPE_DEFINITION
// A Typedef entry.
#define COFF_STORAGE_CLASS_TYPEDEF 13
// 14 IMAGE_SYM_CLASS_UNDEFINED_STATIC
// A static data declaration.
#define COFF_STORAGE_CLASS_USTATIC 14
// 15 IMAGE_SYM_CLASS_ENUM_TAG
// An enumerated type tagname entry.
#define COFF_STORAGE_CLASS_ENUM_TAG 15
// 16 IMAGE_SYM_CLASS_MEMBER_OF_ENUM
// A member of an enumeration. The Value field specifies the n th member.
#define COFF_STORAGE_CLASS_MOE 16
// 17 IMAGE_SYM_CLASS_REGISTER_PARAM
// A register parameter.
#define COFF_STORAGE_CLASS_REGPARAM 17
// 18 IMAGE_SYM_CLASS_BIT_FIELD
// A bit-field reference. The Value field specifies the n th bit in the bit field.
#define COFF_STORAGE_CLASS_BITFIELD 18
// 100 IMAGE_SYM_CLASS_BLOCK
// A .bb (beginning of block) or .eb (end of block) record. The Value field is the relocatable address of the code location.
#define COFF_STORAGE_CLASS_BLOCK 100
// 101 IMAGE_SYM_CLASS_FUNCTION
// A value that Microsoft tools use for symbol records that define the extent of a function: begin function (.bf ), end function ( .ef ), and lines in function ( .lf ). For .lf records, the Value field gives the number of source lines in the function. For .ef records, the Value field gives the size of the function code.
#define COFF_STORAGE_CLASS_FUNCTION 101
// 102 IMAGE_SYM_CLASS_END_OF_STRUCT
// An end-of-structure entry.
#define COFF_STORAGE_CLASS_EOS 102
// 103 IMAGE_SYM_CLASS_FILE
// A value that Microsoft tools, as well as traditional COFF format, use for the source-file symbol record. The symbol is followed by auxiliary records that name the file.
#define COFF_STORAGE_CLASS_FILE 103
// 104 IMAGE_SYM_CLASS_SECTION
// A definition of a section (Microsoft tools use STATIC storage class instead).
#define COFF_STORAGE_CLASS_SECTION 104
// 105 IMAGE_SYM_CLASS_WEAK_EXTERNAL
// A weak external. For more information, see Auxiliary Format 3: Weak Externals.
#define COFF_STORAGE_CLASS_WEAKEXT 105
// 107 IMAGE_SYM_CLASS_CLR_TOKEN
// A CLR token symbol. The name is an ASCII string that consists of the hexadecimal value of the token. For more information, see CLR Token Definition (Object Only).
#define COFF_STORAGE_CLASS_TOKEN 107

#define T_NULL 0b0000
#define T_VOID 0b0001
#define T_CHAR 0b0010
#define T_SHORT 0b0011
#define T_INT 0b0100
#define T_LONG 0b0101
#define T_FLOAT 0b0110
#define T_DOUBLE 0b0111
#define T_STRUCT0b1000
#define T_UNION 0b1001
#define T_ENUM 0b1010
/// Member of enumeration.
#define T_MOE 0b1011
#define T_UCHAR 0b1100
#define T_USHORT 0b1101
#define T_UINT 0b1110
#define T_ULONG 0b1111

#define T_LNGDBL 0b10000

#define DT_NON 0b00000000
#define DT_PTR 0b00010000
#define DT_FCN 0b00100000
#define DT_ARY 0b00110000


/**
 * The Section table name field and the Symbol table name field are
 * actually more complicated than was detailed above, they in fact look
 * more like this:
 *
 * union {
 *   char name[8];
 *   struct {
 *     unsigned long zeroes;
 *     unsigned long offset;
 *   };
 * };
 *
 * If the name is eight characters or fewer, then the field "zeroes"
 * will be non-zero, and "name" should be interpretted as a character
 * array. Note that this field is not null-terminated unless it is
 * fewer than eight characters in length.
 *
 * If the name is more than eight characters, the "zeroes" field (the
 * first four bytes of "name") will be zero. In this case the "offset"
 * field should be used as an offset value into the String table to
 * locate the Symbol or Section name.
 *
 *
 * C_EXT  == 2
 * C_STAT == 3
 *
 * typical use
 *   n_sclass
 *   n_scnum
 *   n_value
 *   meaning of n_value
 *
 * Unresolved external symbol:
 *   C_EXT
 *   0
 *   0
 *   none
 *
 * Uninitialized global variable (not included in BSS):
 *   C_EXT
 *   0
 *   greater than 0
 *   size of variable
 *
 * Function entry point:
 *   C_EXT
 *   .text
 *   any
 *   offset into section
 *
 * Initialized global variable:
 *   C_EXT
 *   .data
 *   any
 *   offset into section
 *
 * Section symbol indicating start of section:
 *   C_STAT
 *   .text, .data, .bss
 *   0
 *   none
 *
 * Initialized static variable:
 *   C_STAT
 *   .data
 *   any
 *   offset into section
 *
 * Uninitialized static variable:
 *   C_STAT
 *   .bss
 *   any
 *   offset into section
 */
typedef struct coff_symbol_entry {
  /// Symbol Name
  union {
    char n_name[8];
    struct {
      uint32_t n_name_zeroes;
      uint32_t n_name_offset;
    };
  };
  /// Value of Symbol
  int32_t n_value;
  /// Section Number
  /// 2 == N_DEBUG | A debugging symbol
  /// 1 == N_ABS   | An absolute symbol (n_value).
  /// 0 == N_UNDEF | An undefined external symbol.
  int16_t n_scnum;
  /// Symbol Type
  /// MS sets this field to 0x20 for functions, or 0 in all other cases.
  uint16_t n_type;
  /// Storage Class
  char n_sclass;
  /// Auxiliary Count
  char n_numaux;
} __attribute__((packed)) coff_symbol_entry;

typedef struct coff_aux_section {
  uint32_t length;
  uint16_t number_relocations;
  uint16_t number_line_numbers;
  uint32_t checksum;
  uint16_t number_low;
  uint8_t  selection;
  uint8_t  unused;
  uint16_t number_high;
} __attribute__((packed)) coff_aux_section;

typedef struct coff_aux_function_definition {
  uint32_t tag_index;
  uint32_t total_size;
  uint32_t pointer_to_line_number;
  uint32_t pointer_to_next_function;
  char unused[2];
} __attribute__((packed)) coff_aux_function_definition;

typedef struct coff_aux_file {
  char filename[18];
} __attribute__((packed)) coff_aux_file;

/// String Table Offset = File Header.f_symptr + File Header.f_nsyms * sizeof(coff_symbol_entry)
typedef struct coff_string_entry {
  union {
    char name[8];
    struct {
      uint32_t zeroes;
      uint32_t offset;
    };
  };
} __attribute__((packed)) coff_string_entry;


#endif /* COFF_H */

