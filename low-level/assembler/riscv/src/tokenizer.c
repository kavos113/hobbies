#include "tokenizer.h"

#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include <string.h>
#include <ctype.h>

bool is_str(char c);
Token *new_token(TokenType type, char *str, int len, Token *last);

bool is_str(char c)
{
  return ('a' <= c && c <= 'z')
      || ('A' <= c && c <= 'Z')
      || ('0' <= c && c <= '9');
}

Token *new_token(TokenType type, char *str, int len, Token *last)
{
  Token *tok = calloc(1, sizeof(Token));
  tok->type = type;
  tok->str = str;
  tok->len = len;
  last->next = tok;

  return tok;
}

Token *tokenize(char *p)
{
  Token head;
  head.next = NULL;
  Token *last = &head;

  while (*p)
  {
    if (*p == '\n')
    {
      last = new_token(T_EOL, p, 0, last);
      p++;
      continue;
    }

    if (isspace(*p))
    {
      p++;
      continue;
    }

    if (isdigit(*p))
    {
      char *base = p;

      last = new_token(T_NUM, base, 0, last);
      last->val = strtol(p, &p, 10);
      last->len = p - base;
      continue;
    }

    if (strchr(",:", *p))
    {
      last = new_token(T_SYM, p, 1, last);
      p++;
      continue;
    }

    if (is_str(*p))
    {
      int len = 0;
      char *start = p;
      while(is_str(*p))
      {
        len++;
        p++;
      }

      last = new_token(T_STR, start, len, last);
      continue;
    }
  }

  last = new_token(T_EOF, p, 0, last);
  return head.next;
}

void print_token(Token *tok)
{
  if (!tok)
    return;

  switch (tok->type)
  {
    case T_STR:
      printf("T_STR: %*.s\n", tok->len, tok->str);
      break;
    case T_NUM:
      printf("T_NUM: %*.s\n", tok->len, tok->str);
      break;
    case T_SYM:
      printf("T_SYM: %*.s\n", tok->len, tok->str);
      break;
    case T_EOL:
      printf("T_EOL\n");
      break;
    case T_EOF:
      printf("T_EOF\n");
      break;
  }

  print_token(tok->next);
}