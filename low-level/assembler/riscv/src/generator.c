#include "generator.h"

#include <stdlib.h>
#include <stdio.h>
#include <stdarg.h>

void error(char *fmt, ...)
{
  va_list ap;
  va_start(ap, fmt);
  vfprintf(stderr, fmt, ap);
  fprintf(stderr, "\n");
  exit(1);
}

typedef struct Opcode
{
  char opcode;
  char func3;
  char func7;
  Mnemonic mnemonic;
} Opcode;

Opcode opcode_table[1] = {
  {0b0110011, 0b000, 0b0000000, M_ADD},
};

Opcode *find_opcode(InstInfo *info);
unsigned int generate_instruction(Instruction *inst);

Opcode *find_opcode(InstInfo *info)
{
  for (int i = 0; i < 1; i++)
    if (opcode_table[i].mnemonic == info->mnemonic)
      return &opcode_table[i];
  
  error("unexpected instruction");
}

unsigned int generate_instruction(Instruction *inst)
{
  Opcode *op = find_opcode(inst->info);
  unsigned int result = op->opcode;

  switch(inst->info->fmt)
  {
  case FM_R:
    result = result
      | ((unsigned int)(inst->rd) << 6)
      | ((unsigned int)(op->func3) << 11)
      | ((unsigned int)(inst->rs1) << 14)
      | ((unsigned int)(inst->rs2) << 19)
      | ((unsigned int)(op->func7) << 24);

  case FM_I:
    result = result
      | ((unsigned int)(inst->rd) << 6)
      | ((unsigned int)(op->func3) << 11)
      | ((unsigned int)(inst->rs1) << 14)
      | ((unsigned int)(inst->imm) << 19);
    break;

  case FM_S:
    result = result
      | ((unsigned int)(inst->imm & 0x1f) << 6)
      | ((unsigned int)(op->func3) << 11)
      | ((unsigned int)(inst->rs1) << 14)
      | ((unsigned int)(inst->rs2) << 19)
      | ((unsigned int)(inst->imm & 0xfe0) << 24);
    break;

  case FM_SB:
    result = result 
      | ((unsigned int)((inst->imm >> 11) & 0x1) << 6) // imm[11]
      | ((unsigned int)((inst->imm >> 1) & 0xf) << 7)  // imm[4:1]
      | ((unsigned int)(op->func3) << 11)
      | ((unsigned int)(inst->rs1) << 14)
      | ((unsigned int)(inst->rs2) << 19)
      | ((unsigned int)((inst->imm >> 5) & 0x3f) << 24)  // imm[10:5]
      | ((unsigned int)((inst->imm >> 12) & 0x1) << 30); // imm[12]
    break;

  case FM_U:
    result = result
      | ((unsigned int)(inst->rd) << 6)
      | ((unsigned int)(inst->imm) << 11);
    break;

  case FM_UJ:
    result = result
      | ((unsigned int)(inst->rd) << 6)
      | ((unsigned int)((inst->imm >> 12) & 0xff) << 11) // imm[19:12]
      | ((unsigned int)((inst->imm >> 11) & 0x1) << 19)  // imm[11]
      | ((unsigned int)((inst->imm >> 1) & 0x3ff) << 29) // imm[10:1]
      | ((unsigned int)((inst->imm >> 20) & 0x1) << 30); // imm[20]
    break;
  }

  return result;
}