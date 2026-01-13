#include "parser.h"

#include <stdbool.h>
#include <stdlib.h>
#include <string.h>

#include "util.h"

// cannot found: error
Register parse_register(Token *tok);
int parse_immidiate(Token *tok);
// cannot found: null
InstInfo *parse_instruction(Token *tok);
void need_symbol(Token *tok, char c);
bool is_eof(Token *tok);

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

InstInfo instructions[1] = {
  {"add", 3, M_ADD, FM_R}
};

Register parse_register(Token *tok)
{
  if (tok->type != T_STR)
    error("unexpected register: %.*s", tok->len, tok->str);

  for (int i = 0; i < 32; i++)
    if (strncmp(tok->str, register_name[i], tok->len) == 0)
      return i;
  
  error("unexpected register: %.*s", tok->len, tok->str);
}

int parse_immidiate(Token *tok)
{
  if (tok->type != T_NUM)
    error("unexpected immidiate: %.*s", tok->len, tok->str);
  
  return tok->val;
}

InstInfo *parse_instruction(Token *tok)
{
  if (tok->type != T_STR)
    return NULL;

  for (int i = 0; i < 1; i++)
    if (strncmp(tok->str, instructions[i].name, tok->len) == 0)
      return &instructions[i];
  
  return NULL;
}

void need_symbol(Token *tok, char c)
{
  if (tok->type != T_SYM)
    error("unexpected token: %.*s", tok->len, tok->str);

  if (tok->len != 1 || *(tok->str) != c)
    error("unexpected token: %.*s", tok->len, tok->str);
}

bool is_eof(Token *tok)
{
  return tok->type == T_EOF;
}

// read token
Instruction *parse_line(Token *tok)
{
  InstInfo *info = parse_instruction(tok);
  if (!info)
    error("need instruction: %*.s", tok->len, tok->str);

  Instruction *inst = calloc(1, sizeof(Instruction));
  inst->info = info;

  tok = tok->next;

  switch(info->fmt)
  {
  case FM_R:
    inst->rd = parse_register(tok);
    tok = tok->next;
    inst->rs1 = parse_register(tok);
    tok = tok->next;
    inst->rs2 = parse_register(tok);
    tok = tok->next;
    break;
  case FM_I:
    inst->rd = parse_register(tok);
    tok = tok->next;
    inst->rs1 = parse_register(tok);
    tok = tok->next;
    inst->imm = parse_immidiate(tok);
    tok = tok->next;
    break;
  case FM_S:
  case FM_SB:
    inst->rs1 = parse_register(tok);
    tok = tok->next;
    inst->rs2 = parse_register(tok);
    tok = tok->next;
    inst->imm = parse_immidiate(tok);
    tok = tok->next;
    break;
  case FM_U:
  case FM_UJ:
    inst->rd = parse_register(tok);
    tok = tok->next;
    inst->imm = parse_immidiate(tok);
    tok = tok->next;
    break;
  }

  if (tok->type != T_EOL)
    error("too many tokens: %.*s", tok->len, tok->str);

  tok = tok->next;
}

Instruction *parse(Token *tok)
{
  Instruction head;
  head.next = NULL;
  Instruction *last = &head;

  while(!is_eof(tok))
  {
    Instruction *new_inst = parse_line(tok);
    last->next = new_inst;
    last = new_inst;
  }

  return head.next;
}