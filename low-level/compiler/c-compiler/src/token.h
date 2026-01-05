#ifndef TOKEN_H
#define TOKEN_H

#include <stdbool.h>
#include <stdio.h>

typedef enum
{
  TK_RESERVED,
  TK_IDENT,
  TK_NUMBER,
  TK_EOF,
  TK_RETURN,
  TK_IF,
  TK_ELSE,
  TK_WHILE,
  TK_FOR,
} TokenKind;

typedef struct Token Token;

struct Token
{
  TokenKind kind;
  Token *next;
  int val; // only TK_NUMBER
  char *str;
  int len; // 0 if TK_NUMBER
};

void set_token(Token *tok);

bool consume_op(char *op);
Token *consume_ident();
bool consume(TokenKind kind);
void expect_op(char *op);
int expect_number();
bool at_eof();

Token *tokenize(char *p);

void print_token(Token *token, FILE* s);

#endif