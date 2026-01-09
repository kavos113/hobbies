#include "parser.h"
#include "riscv.h"
#include "tokenizer.h"

#include <stdbool.h>
#include <stdio.h>
#include <string.h>

// cannot found: return -1
Register parse_register(Token *tok);
InstInfo *parse_instruction(Token *tok);
void need_symbol(Token *tok, char c);

Instruction *parse_line(Token *tok);

char *register_name[32] = {
  "zero",
  "ra",
  "sp",
  "gp",
  "tp",
  "t0",
  "t1",
  "t2",
  "s0",
  "s1",
  "a0",
  "a1",
  "a2",
  "a3",
  "a4",
  "a5",
  "a6",
  "a7",
  "s2",
  "s3",
  "s4",
  "s5",
  "s6",
  "s7",
  "s8",
  "s9",
  "s10",
  "s11",
  "t3",
  "t4",
  "t5",
  "t6"
};

InstInfo *instructions[1] = {
  {"add", 3, FM_R}
};

Register parse_register(Token *tok)
{
  if (tok->type != T_STR)
    return -1;

  for (int i = 0; i < 32; i++)
    if (strncmp(tok->str, register_name[i], tok->len) == 0)
      return i;
  
  return -1;
}

InstInfo *parse_instruction(Token *tok)
{
  if (tok->type != T_STR)
    return NULL;

  for (int i = 0; i < 1; i++)
    if (strncmp(tok->str, instructions[i]->name, tok->len) == 0)
      return instructions[i];
  
  return NULL;
}

void need_symbol(Token *tok, char c)
{
  if (tok->type != T_SYM)
  {
    fprintf(stderr, "unexpected token: %.*s\n", tok->len, tok->str);
    exit(1);
  }

  if (tok->len != 1 || *(tok->str) != c)
  {
    fprintf(stderr, "unexpected token: %.*s\n", tok->len, tok->str);
    exit(1);
  }
}

Instruction *parse_line(Token *tok)
{
  
}