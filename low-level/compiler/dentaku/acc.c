#include <ctype.h>
#include <stdarg.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

typedef enum
{
  TK_RESERVED,
  TK_NUMBER,
  TK_EOF,
} TokenKind;

typedef struct Token Token;

struct Token
{
  TokenKind kind;
  Token *next;
  int val;
  char *str;
};

Token *token;

void error(char *fmt, ...)
{
  va_list ap;
  va_start(ap, fmt);
  vfprintf(stderr, fmt, ap);
  fprintf(stderr, "\n");
  exit(1);
}

bool consume_op(char op)
{
  if (token->kind != TK_RESERVED || token->str[0] != op)
    return false;
  
  token = token->next;
  return true;
}

void expect_op(char op)
{
  if (token->kind != TK_RESERVED || token->str[0] != op)
    error("op is not '%c'", op);
  token = token->next;
}

int expect_number() 
{
  if (token->kind != TK_NUMBER)
    error("not number");
  
  int val = token->val;
  token = token->next;
  return val;
}

bool at_eof()
{
  return token->kind == TK_EOF;
}

Token *new_token(TokenKind kind, Token *current, char *str)
{
  Token *tok = calloc(1, sizeof(Token));
  tok->kind = kind;
  tok->str = str;
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

    if (*p == '+' || *p == '-')
    {
      current = new_token(TK_RESERVED, current, p++);
      continue;
    }

    if (isdigit(*p))
    {
      current = new_token(TK_NUMBER, current, p);
      current->val = strtol(p, &p, 10);
      continue;
    }

    error("cannot tokenize");
  }

  new_token(TK_EOF, current, p);
  return head.next;
}

int main(int argc, char **argv)
{
  if (argc != 2)
  {
    fprintf(stderr, "arg: number");
    return 1;
  }

  token = tokenize(argv[1]);

  printf(".intel_syntax noprefix\n");
  printf(".global main\n");
  printf("main:\n");

  printf("  mov rax, %d\n", expect_number());

  while (!at_eof())
  {
    if (consume_op('+'))
    {
      printf("  add rax, %d\n", expect_number());
    }

    expect_op('-');
    printf("  sub rax, %d\n", expect_number());
  }

  printf("  ret\n");
  return 0;
}