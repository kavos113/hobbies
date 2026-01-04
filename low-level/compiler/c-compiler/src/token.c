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

bool consume_return()
{
  if (token->kind != TK_RETURN)
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
    error_at(token->str, token->str, "op is not '%s'", op);
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
      current = new_token(TK_NUMBER, current, p, 0);
      current->val = strtol(p, &p, 10);
      continue;
    }

    // variable
    if (isalpha(*p))
    {
      char *ident_first = p;

      int len = 1;
      p++;

      while (isalpha(*p))
      {
        len++;
        p++;
      }

      current = new_token(TK_IDENT, current, ident_first, len);
      p++;
      continue;
    }

    // return
    if (strncmp(p, "return", 6) == 0 && !is_token_str(p[6]))
    {
      current = new_token(TK_RETURN, current, p, 6);
      p += 6;
      continue;
    }

    error("cannot tokenize");
  }

  new_token(TK_EOF, current, p, 0);
  return head.next;
}

void print_token(Token *token)
{
  fprintf(stdout, "%s, \n", token->str);

  if (token->next)
    print_token(token->next);
}