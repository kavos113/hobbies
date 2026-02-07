#include "header.h"
#include "error.h"

#include <stdint.h>
#include <stdlib.h>

PEError seek_pe_header(FILE *file);
PEError read_signature(FILE *file, char *signature);
// expected file pointer to be at the start of COFF header
PEError read_coff_header(FILE *file, COFFHeader *coff_header);

PEHeader *read_pe_header(FILE *file)
{
  PEHeader *pe_header = malloc(sizeof(PEHeader));
  if (pe_header == NULL)
  {
    fprintf(stderr, "Memory allocation failed.\n");
    return NULL;
  }

  PEError err = seek_pe_header(file);
  if (err != ERROR_NONE)
  {
    free(pe_header);
    return NULL;
  }

  err = read_signature(file, pe_header->signature);
  if (err != ERROR_NONE)
  {
    free(pe_header);
    return NULL;
  }

  err = read_coff_header(file, &pe_header->coff_header);
  if (err != ERROR_NONE)
  {
    free(pe_header);
    return NULL;
  }

  return pe_header;
}

PEError seek_pe_header(FILE *file)
{
  if (file == NULL)
  {
    fprintf(stderr, "Invalid file pointer.\n");
    return ERROR_INVALID_FILE_POINTER;
  }

  if (fseek(file, DOS_STUB_PE_HEADER_ADDRESS, SEEK_SET) != 0)
  {
    fprintf(stderr, "Failed to seek to PE header address.\n");
    return ERROR_FILE_SEEK_FAILED;
  }

  uint32_t pe_header_offset;
  size_t read_size = fread(&pe_header_offset, sizeof(uint32_t), 1, file);
  if (read_size != 1)
  {
    fprintf(stderr, "Failed to read PE header offset.\n");
    return ERROR_FILE_READ_FAILED;
  }

  if (fseek(file, pe_header_offset, SEEK_SET) != 0)
  {
    fprintf(stderr, "Failed to seek to PE header.\n");
    return ERROR_FILE_SEEK_FAILED;
  }

  return ERROR_NONE;
}

PEError read_signature(FILE *file, char *signature)
{
  if (file == NULL)
  {
    fprintf(stderr, "Invalid file pointer.\n");
    return ERROR_INVALID_FILE_POINTER;
  }

  size_t read_size = fread(signature, sizeof(char), 4, file);
  if (read_size != 4)
  {
    fprintf(stderr, "Failed to read PE signature.\n");
    return ERROR_FILE_READ_FAILED;
  }

  return ERROR_NONE;
}

PEError read_coff_header(FILE *file, COFFHeader *coff_header)
{
  if (file == NULL)
  {
    fprintf(stderr, "Invalid file pointer.\n");
    return ERROR_INVALID_FILE_POINTER;
  }

  size_t read_size = fread(coff_header, sizeof(COFFHeader), 1, file);
  if (read_size != 1)
  {
    fprintf(stderr, "Failed to read COFF header.\n");
    return ERROR_FILE_READ_FAILED;
  }

  return ERROR_NONE;
}