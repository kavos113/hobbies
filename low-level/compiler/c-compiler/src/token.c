#include "token.h"

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <ctype.h>

#include "util.h"

Token *new_token(TokenKind kind, Token *current, char *str, int len);

bool is_token_str(char c)
{
  return ('a' <= c && c <= 'z')
      || ('A' <= c && c <= 'Z')
      || ('0' <= c && c <= '9')
      || (c == '_');
}

static Token *token;

void set_token(Token *tok)
{
  token = tok;
}

// if b starts with a
bool starts_with(char *a, char *b)
{
  return memcmp(a, b, strlen(a)) == 0;
}

bool consume_op(char *op)
{
  // fprintf(stdout, "consume_op: token %s, len %d, op %s\n", token->str, token->len, op);
  if (token->kind != TK_RESERVED 
    || strlen(op) != token->len
    || memcmp(token->str, op, token->len)
  )
    return false;

  token = token->next;
  return true;
}

Token *consume_ident()
{
  if (token->kind != TK_IDENT)
    return NULL;

  Token *prev = token;

  token = token->next;
  return prev;
}

bool consume(TokenKind kind)
{
  if (token->kind != kind)
    return false;

  token = token->next;
  return true;
}

void expect_op(char *op)
{
  if (token->kind != TK_RESERVED 
    || strlen(op) != token->len
    || memcmp(token->str, op, token->len)
  )
    error_at(token->str, "op is not '%s'", op);

  token = token->next;
}

int expect_number() 
{
  if (token->kind != TK_NUMBER)
    error_at(token->str, "not number");
  
  int val = token->val;
  token = token->next;
  return val;
}

bool at_eof()
{
  return token->kind == TK_EOF;
}

Token *new_token(TokenKind kind, Token *current, char *str, int len)
{
  Token *tok = calloc(1, sizeof(Token));
  tok->kind = kind;
  tok->str = str;
  tok->len = len;
  current->next = tok;
  return tok;
}

Token *tokenize(char *p)
{
  Token head;
  head.next = NULL;
  Token *current = &head;

  while (*p) 
  {
    // skip space
    if (isspace(*p))
    {
      p++;
      continue;
    }

    // multi chars operator
    if (starts_with("==", p) || starts_with("!=", p) || starts_with("<=", p) || starts_with(">=", p))
    { 
      current = new_token(TK_RESERVED, current, p, 2);
      p += 2;
      continue;
    }

    // single char operator
    if (strchr("+-*/()<>;=", *p))
    {
      current = new_token(TK_RESERVED, current, p++, 1);
      continue;
    }

    // number
    if (isdigit(*p))
    {
      char *base = p;

      current = new_token(TK_NUMBER, current, p, 0);
      current->val = strtol(p, &p, 10);
      current->len = p - base;
      continue;
    }

    // return
    if (strncmp(p, "return", 6) == 0 && !is_token_str(p[6]))
    {
      current = new_token(TK_RETURN, current, p, 6);
      p += 6;
      continue;
    }

    // if
    if (strncmp(p, "if", 2) == 0 && !is_token_str(p[2]))
    {
      current = new_token(TK_IF, current, p, 2);
      p += 2;
      continue;
    }

    // else
    if (strncmp(p, "else", 4) == 0 && !is_token_str(p[4]))
    {
      current = new_token(TK_ELSE, current, p, 4);
      p += 4;
      continue;
    }

    // while
    if (strncmp(p, "while", 5) == 0 && !is_token_str(p[5]))
    {
      current = new_token(TK_WHILE, current, p, 5);
      p += 5;
      continue;
    }

    // for
    if (strncmp(p, "for", 3) == 0 && !is_token_str(p[3]))
    {
      current = new_token(TK_FOR, current, p, 3);
      p += 3;
      continue;
    }

    // variable
    if (is_token_str(*p))
    {
      char *ident_first = p;

      int len = 1;
      p++;

      while (is_token_str(*p))
      {
        len++;
        p++;
      }

      current = new_token(TK_IDENT, current, ident_first, len);
      continue;
    }

    error("cannot tokenize: %s", p);
  }

  new_token(TK_EOF, current, p, 0);
  return head.next;
}

void print_token(Token *token, FILE* s)
{
  switch (token->kind)
  {
  case TK_RESERVED:
    fprintf(s, "TK_RESERVED: ");
    break;
  case TK_NUMBER:
    fprintf(s, "TK_NUMBER: ");
    break;
  case TK_IDENT:
    fprintf(s, "TK_IDENT: ");
    break;
  case TK_RETURN:
    fprintf(s, "TK_RETURN: ");
    break;
  case TK_EOF:
    fprintf(s, "TK_EOF: ");
    break;
  case TK_IF:
    fprintf(s, "TK_IF: ");
    break;
  case TK_ELSE:
    fprintf(s, "TK_ELSE: ");
    break;
  case TK_WHILE:
    fprintf(s, "TK_WHILE: ");
    break;
  case TK_FOR:
    fprintf(s, "TK_FOR: ");
    break;
  }

  fprintf(stdout, "%.*s\n", token->len, token->str);

  if (token->next)
    print_token(token->next, s);
}