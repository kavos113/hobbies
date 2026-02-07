#ifndef PE_H
#define PE_H

#include <stdint.h>

#define DOS_STUB_PE_HEADER_ADDRESS 0x3C

typedef struct _COFFHeader
{
  uint16_t machine;
  uint16_t number_of_sections;
  uint32_t time_date_stamp;
  uint32_t pointer_to_symbol_table;
  uint32_t number_of_symbols;
  uint16_t size_of_optional_header;
  uint16_t characteristics;
} COFFHeader;

// ----- machine -----
#define IMAGE_FILE_MACHINE_UNKNOWN     0x0
#define IMAGE_FILE_MACHINE_ALPHA       0x184
#define IMAGE_FILE_MACHINE_ALPHA64     0x284
#define IMAGE_FILE_MACHINE_AM33        0x1d3
#define IMAGE_FILE_MACHINE_AMD64       0x8664
#define IMAGE_FILE_MACHINE_ARM         0x1c0
#define IMAGE_FILE_MACHINE_ARM64       0xaa64
#define IMAGE_FILE_MACHINE_ARM64EC     0xa641
#define IMAGE_FILE_MACHINE_ARM64X      0xa64e
#define IMAGE_FILE_MACHINE_ARMNT       0x1c4
#define IMAGE_FILE_MACHINE_AXP64       0x284
#define IMAGE_FILE_MACHINE_EBC         0xebc
#define IMAGE_FILE_MACHINE_I386        0x14c
#define IMAGE_FILE_MACHINE_IA64        0x200
#define IMAGE_FILE_MACHINE_LOONGARCH32 0x6232
#define IMAGE_FILE_MACHINE_LOONGARCH64 0x6264
#define IMAGE_FILE_MACHINE_M32R        0x9041
#define IMAGE_FILE_MACHINE_MIPS16      0x266
#define IMAGE_FILE_MACHINE_MIPSFPU     0x366
#define IMAGE_FILE_MACHINE_MIPSFPU16   0x466
#define IMAGE_FILE_MACHINE_POWERPC     0x1f0
#define IMAGE_FILE_MACHINE_POWERPCFP   0x1f1
#define IMAGE_FILE_MACHINE_R3000BE     0x160
#define IMAGE_FILE_MACHINE_R3000       0x162
#define IMAGE_FILE_MACHINE_R4000       0x166
#define IMAGE_FILE_MACHINE_R10000      0x168
#define IMAGE_FILE_MACHINE_RISCV32     0x5032
#define IMAGE_FILE_MACHINE_RISCV64     0x5064
#define IMAGE_FILE_MACHINE_RISCV128    0x5128
#define IMAGE_FILE_MACHINE_SH3         0x1a2
#define IMAGE_FILE_MACHINE_SH3DSP      0x1a3
#define IMAGE_FILE_MACHINE_SH4         0x1a6
#define IMAGE_FILE_MACHINE_SH5         0x1a8
#define IMAGE_FILE_MACHINE_THUMB       0x1c2
#define IMAGE_FILE_MACHINE_WCEMIPSV2   0x169

// ----- characteristics -----
#define IMAGE_FILE_RELOCS_STRIPPED         0x0001
#define IMAGE_FILE_EXECUTABLE_IMAGE        0x0002
#define IMAGE_FILE_LINE_NUMS_STRIPPED      0x0004
#define IMAGE_FILE_LOCAL_SYMS_STRIPPED     0x0008
#define IMAGE_FILE_AGGRESSIVE_WS_TRIM      0x0010
#define IMAGE_FILE_LARGE_ADDRESS_AWARE     0x0020
#define IMAGE_FILE_BYTES_REVERSED_LO       0x0080
#define IMAGE_FILE_32BIT_MACHINE           0x0100
#define IMAGE_FILE_DEBUG_STRIPPED          0x0200
#define IMAGE_FILE_REMOVABLE_RUN_FROM_SWAP 0x0400
#define IMAGE_FILE_NET_RUN_FROM_SWAP       0x0800
#define IMAGE_FILE_SYSTEM                  0x1000
#define IMAGE_FILE_DLL                     0x2000
#define IMAGE_FILE_UP_SYSTEM_ONLY          0x4000
#define IMAGE_FILE_BYTES_REVERSED_HI       0x8000

#define PE_OPTIONAL_HEADER_MAGIC_PE32     0x10b
#define PE_OPTIONAL_HEADER_MAGIC_PE32PLUS 0x20b

typedef struct _OptionalHeaderStandardPE32
{
  uint16_t magic;
  uint8_t major_linker_version;
  uint8_t minor_linker_version;
  uint32_t size_of_code;
  uint32_t size_of_initialized_data;
  uint32_t size_of_bss_data;
  uint32_t address_of_entry_point;
  uint32_t base_of_code;
  uint32_t base_of_data;
} OptionalHeaderStandardPE32;

typedef struct _OptionalHeaderStandardPE32Plus
{
  uint16_t magic;
  uint8_t major_linker_version;
  uint8_t minor_linker_version;
  uint32_t size_of_code;
  uint32_t size_of_initialized_data;
  uint32_t size_of_bss_data;
  uint32_t address_of_entry_point;
  uint32_t base_of_code;
} OptionalHeaderStandardPE32Plus;

typedef struct _OptionalHeaderWindowsFieldPE32
{
  uint32_t image_base;
  uint32_t section_alignment;
  uint32_t file_alignment;
  uint16_t major_os_version;
  uint16_t minor_os_version;
  uint16_t major_image_version;
  uint16_t minor_image_version;
  uint16_t major_subsystem_version;
  uint16_t minor_subsystem_version;
  uint32_t win32_version_value;
  uint32_t size_of_image;
  uint32_t size_of_headers;
  uint32_t check_sum;
  uint16_t subsystem;
  uint16_t dll_characteristics;
  uint32_t size_of_stack_reserve;
  uint32_t size_of_heap_reserve;
  uint32_t size_of_heap_commit;
  uint32_t loader_flags;
  uint32_t number_of_rva_and_sizes;
} OptionalHeaderWindowsFieldPE32;

typedef struct _OptionalHeaderWindowsFieldPE32Plus
{
  uint64_t image_base;
  uint32_t section_alignment;
  uint32_t file_alignment;
  uint16_t major_os_version;
  uint16_t minor_os_version;
  uint16_t major_image_version;
  uint16_t minor_image_version;
  uint16_t major_subsystem_version;
  uint16_t minor_subsystem_version;
  uint32_t win32_version_value;
  uint32_t size_of_image;
  uint32_t size_of_headers;
  uint32_t check_sum;
  uint16_t subsystem;
  uint16_t dll_characteristics;
  uint64_t size_of_stack_reserve;
  uint64_t size_of_heap_reserve;
  uint64_t size_of_heap_commit;
  uint32_t loader_flags;
  uint32_t number_of_rva_and_sizes;
} OptionalHeaderWindowsFieldPE32Plus;

typedef struct _PEHeader
{
  char signature[4];
  COFFHeader coff_header;
} PEHeader;


#endif // PE_H