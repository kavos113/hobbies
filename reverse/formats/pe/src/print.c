#include "print.h"

#include <stdio.h>
#include <time.h>

void print_signature(char *signature);
void print_coff_header(COFFHeader *coff_header);

void print_pe_header(PEHeader *pe_header)
{
  if (pe_header == NULL)
  {
    fprintf(stderr, "Invalid PE header pointer.\n");
    return;
  }

  printf("PE Header:\n");
  print_signature(pe_header->signature);
  print_coff_header(&pe_header->coff_header);
}

void print_pretty_char(char c)
{
  switch (c)
  {
  case 0:
    printf("\\0");
    break;
  case '\n':
    printf("\\n");
    break;
  case '\r':
    printf("\\r");
    break;
  case '\t':
    printf("\\t");
    break;
  case '\\':
    printf("\\\\");
    break;
  case ' ':
    printf(" ");
    break;
  }

  if (c >= 0x20 && c <= 0x7E)
  {
    printf("%c", c);
  }
  else
  {
    printf("\\x%02x", (unsigned char)c);
  }
}

void print_signature(char *signature)
{
  printf("Signature: ");
  for (int i = 0; i < 4; i++)
  {
    print_pretty_char(signature[i]);
    printf(" ");
  }
  printf("\n");
}

void print_coff_machine(uint16_t machine);
void print_coff_characteristics(uint16_t characteristics, int indent_size);
void print_coff_timestamp(uint32_t time_date_stamp);

void print_coff_header(COFFHeader *coff_header)
{
  if (coff_header == NULL)
  {
    fprintf(stderr, "Invalid COFF header pointer.\n");
    return;
  }

  printf("\n");
  printf("--------- COFF Header ---------\n");
  printf("Machine:                 ");
  print_coff_machine(coff_header->machine);
  printf("Number of Sections:      %u\n", coff_header->number_of_sections);
  printf("Latest Timestamp:        ");
  print_coff_timestamp(coff_header->time_date_stamp);
  printf("Pointer to Symbol Table: %u\n", coff_header->pointer_to_symbol_table);
  printf("Number of Symbols:       %u\n", coff_header->number_of_symbols);
  printf("Size of Optional Header: %u\n", coff_header->size_of_optional_header);
  printf("Characteristics:\n");
  print_coff_characteristics(coff_header->characteristics, 2);
  printf("-------------------------------\n");
}

void print_coff_machine(uint16_t machine)
{
  switch (machine)
  {
  case IMAGE_FILE_MACHINE_UNKNOWN:
    printf("Unknown\n");
    break;
  case IMAGE_FILE_MACHINE_ALPHA:
    printf("Alpha AXP, 32-bit\n");
    break;
  case IMAGE_FILE_MACHINE_ALPHA64:
    printf("Alpha 64, 64-bit\n");
    break;
  case IMAGE_FILE_MACHINE_AM33:
    printf("Matsushita AM33\n");
    break;
  case IMAGE_FILE_MACHINE_AMD64:
    printf("x64\n");
    break;
  case IMAGE_FILE_MACHINE_ARM:
    printf("ARM Little-Endian\n");
    break;
  case IMAGE_FILE_MACHINE_ARM64:
    printf("ARM64 Little-Endian\n");
    break;
  case IMAGE_FILE_MACHINE_ARM64EC:
    printf("interoperable between native ARM64 and emulated x64\n");
    break;
  case IMAGE_FILE_MACHINE_ARM64X:
    printf("ARM64 and ARM64EC");
    break;
  case IMAGE_FILE_MACHINE_ARMNT:
    printf("ARM Thumb-2 Little-Endian\n");
    break;
  case IMAGE_FILE_MACHINE_EBC:
    printf("EFI Byte Code\n");
    break;
  case IMAGE_FILE_MACHINE_I386:
    printf("i386\n");
    break;
  case IMAGE_FILE_MACHINE_IA64:
    printf("Intel Itanium\n");
    break;
  case IMAGE_FILE_MACHINE_LOONGARCH32:
    printf("LoongArch 32-bit\n");
    break;
  case IMAGE_FILE_MACHINE_LOONGARCH64:
    printf("LoongArch 64-bit\n");
    break;
  case IMAGE_FILE_MACHINE_M32R:
    printf("Mitsubishi M32R Little-Endian\n");
    break;
  case IMAGE_FILE_MACHINE_MIPS16:
    printf("MIPS16\n");
    break;
  case IMAGE_FILE_MACHINE_MIPSFPU:
    printf("MIPS with FPU\n");
    break;
  case IMAGE_FILE_MACHINE_MIPSFPU16:
    printf("MIPS16 with FPU\n");
    break;
  case IMAGE_FILE_MACHINE_POWERPC:
    printf("Power PC Little-Endian\n");
    break;
  case IMAGE_FILE_MACHINE_POWERPCFP:
    printf("Power PC with Floating Point Support\n");
    break;
  case IMAGE_FILE_MACHINE_R3000BE:
    printf("MIPS I compatible 32-bit big-endian\n");
    break;
  case IMAGE_FILE_MACHINE_R3000:
    printf("MIPS I compatible 32-bit little-endian\n");
    break;
  case IMAGE_FILE_MACHINE_R4000:
    printf("MIPS III compatible 64-bit little-endian\n");
    break;
  case IMAGE_FILE_MACHINE_R10000:
    printf("MIPS IV compatible 64-bit little-endian\n");
    break;
  case IMAGE_FILE_MACHINE_RISCV32:
    printf("RISC-V 32-bit\n");
    break;
  case IMAGE_FILE_MACHINE_RISCV64:
    printf("RISC-V 64-bit\n");
    break;
  case IMAGE_FILE_MACHINE_RISCV128:
    printf("RISC-V 128-bit\n");
    break;
  case IMAGE_FILE_MACHINE_SH3:
    printf("Hitachi SH3\n");
    break;
  case IMAGE_FILE_MACHINE_SH3DSP:
    printf("Hitachi SH3 DSP\n");
    break;
  case IMAGE_FILE_MACHINE_SH4:
    printf("Hitachi SH4\n");
    break;
  case IMAGE_FILE_MACHINE_SH5:
    printf("Hitachi SH5\n");
    break;
  case IMAGE_FILE_MACHINE_THUMB:
    printf("ARM Thumb-2 Little-Endian\n");
    break;
  case IMAGE_FILE_MACHINE_WCEMIPSV2:
    printf("MIPS V2 Little-Endian WCE v2\n");
    break;
  default:
    printf("Unknown (0x%04x)\n", machine);
    break;
  }
}

void print_coff_characteristic_flag(const char *description, int indent_size)
{
  for (int i = 0; i < indent_size; i++)
  {
    printf(" ");
  }
  printf("- %s\n", description);
}

void print_coff_characteristics(uint16_t characteristics, int indent_size)
{
  if (characteristics & IMAGE_FILE_RELOCS_STRIPPED)
  {
    print_coff_characteristic_flag(
        "Relocation stripped", indent_size);
  }

  if (characteristics & IMAGE_FILE_EXECUTABLE_IMAGE)
  {
    print_coff_characteristic_flag(
        "Executable file", indent_size);
  }

  if (characteristics & IMAGE_FILE_LINE_NUMS_STRIPPED)
  {
    print_coff_characteristic_flag(
        "Line numbers stripped", indent_size);
  }

  if (characteristics & IMAGE_FILE_LOCAL_SYMS_STRIPPED)
  {
    print_coff_characteristic_flag(
        "Local symbols stripped", indent_size);
  }

  if (characteristics & IMAGE_FILE_AGGRESSIVE_WS_TRIM)
  {
    print_coff_characteristic_flag(
        "Aggressively trim working set", indent_size);
  }

  if (characteristics & IMAGE_FILE_LARGE_ADDRESS_AWARE)
  {
    print_coff_characteristic_flag(
        "App can handle >2GB addresses", indent_size);
  }

  if (characteristics & IMAGE_FILE_BYTES_REVERSED_LO)
  {
    print_coff_characteristic_flag(
        "Little endian", indent_size);
  }

  if (characteristics & IMAGE_FILE_32BIT_MACHINE)
  {
    print_coff_characteristic_flag(
        "32-bit word architecture", indent_size);
  }

  if (characteristics & IMAGE_FILE_DEBUG_STRIPPED)
  {
    print_coff_characteristic_flag(
        "Debug info stripped", indent_size);
  }

  if (characteristics & IMAGE_FILE_REMOVABLE_RUN_FROM_SWAP)
  {
    print_coff_characteristic_flag(
        "If Image is on removable media, copy and run from swap", indent_size);
  }

  if (characteristics & IMAGE_FILE_NET_RUN_FROM_SWAP)
  {
    print_coff_characteristic_flag(
        "If Image is on Network media, copy and run from swap", indent_size);
  }

  if (characteristics & IMAGE_FILE_SYSTEM)
  {
    print_coff_characteristic_flag(
        "System file", indent_size);
  }

  if (characteristics & IMAGE_FILE_DLL)
  {
    print_coff_characteristic_flag(
        "DLL file", indent_size);
  }

  if (characteristics & IMAGE_FILE_UP_SYSTEM_ONLY)
  {
    print_coff_characteristic_flag(
        "Only run on a UniProcessor machine", indent_size);
  }

  if (characteristics & IMAGE_FILE_BYTES_REVERSED_HI)
  {
    print_coff_characteristic_flag(
        "Big endian", indent_size);
  }
}

void print_coff_timestamp(uint32_t time_date_stamp)
{
  time_t raw_time = (time_t)time_date_stamp;

  struct tm *time_info = localtime(&raw_time);
  if (time_info == NULL)
  {
    printf("Invalid timestamp: %u\n", time_date_stamp);
    return;
  }

  char buffer[26];
  if (strftime(buffer, sizeof(buffer), "%Y-%m-%d %H:%M:%S", time_info) == 0)
  {
    printf("Invalid timestamp: %u\n", time_date_stamp);
    return;
  }
  printf("%s\n", buffer);
}