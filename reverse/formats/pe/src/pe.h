#ifndef PE_H
#define PE_H

#define DOS_STUB_PE_HEADER_ADDRESS 0x3C

typedef struct PEHeader
{
  char signature[4];
} PEHeader;

#endif // PE_H