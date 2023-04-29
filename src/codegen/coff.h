#ifndef COFF_H
#define COFF_H

#include <stdint.h>

#define HDR_RELOC_STRIPPED     0b0000000000000001
#define HDR_EXECUTABLE         0b0000000000000010
#define HDR_LINE_NUMS_STRIPPED 0b0000000000000100
#define HDR_32BIT              0b0000000100000000
#define HDR_DEBUG_STRIPPED     0b0000001000000000
#define HDR_SYSTEM             0b0001000000000000
#define HDR_DLL                0b0010000000000000

typedef struct coff_header {
  uint16_t f_magic;
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

#define STYP_TEXT 0b00100000
#define STYP_DATA 0b01000000
#define STYP_BSS  0b10000000
#define SCN_MEM_SHARED  (0b1 << 28)
#define SCN_MEM_EXECUTE (0b1 << 29)
#define SCN_MEM_READ    (0b1 << 30)
#define SCN_MEM_WRITE   (0b1 << 31)

typedef struct coff_section_header {
  /// Section Name
  char s_name[8];
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

#define C_NULL    0
#define C_AUTO    1
#define C_EXT     2
#define C_STAT    3
#define C_REG     4
#define C_EXTDEF  5
#define C_LABEL   6
#define C_ULABEL  7
/// Member of structure.
#define C_MOS     8
#define C_ARG     9
#define C_STRTAG  10
#define C_MOU     11
#define C_UNTAG   12
#define C_TPDEF   13
#define C_USTATIC 14
#define C_ENTAG   15
#define C_MOE     16
#define C_REGPARM 17
#define C_FIELD   18
#define C_AUTOARG 19
#define C_LASTENT 20
#define C_BLOCK   100
#define C_FCN     101
#define C_EOS     102
#define C_FILE    103
#define C_LINE    104
#define C_ALIAS   105
#define C_HIDDEN  106
#define C_EFCN    255


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
 * actaully more complicated than was detailed above, they in fact look
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
  char n_name[8];
  /// Value of Symbol
  int32_t n_value;
  /// Section Number
  /// 2 == N_DEBUG | A debugging symbol
  /// 1 == N_ABS   | An absolute symbol (n_value).
  /// 0 == N_UNDEF | An undefined external symbol.
  short n_scnum;
  /// Symbol Type
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

