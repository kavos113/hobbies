#ifndef TOKEN_H
#define TOKEN_H

#include <stdbool.h>

typedef enum
{
  TK_RESERVED,
  TK_IDENT,
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
  int len;
};

void set_token(Token *tok);

bool consume_op(char *op);
void expect_op(char *op);
int expect_number();
bool at_eof();

Token *new_token(TokenKind kind, Token *current, char *str, int len);
Token *tokenize(char *p);

void print_token(Token *token);

#endif