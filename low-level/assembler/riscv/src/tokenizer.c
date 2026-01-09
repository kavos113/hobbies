#include "tokenizer.h"

#include <stdlib.h>
#include <stdbool.h>
#include <string.h>
#include <ctype.h>

bool is_alnum(char c);
Token *new_token(TokenType type, char *str, int len, Token *last);

bool is_alnum(char c)
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

    if (strchr(",:", *p))
    {
      last = new_token(T_SYM, p, 1, last);
      p++;
      continue;
    }

    if (is_alnum(*p))
    {
      int len = 0;
      char *start = p;
      while(is_alnum(*p))
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