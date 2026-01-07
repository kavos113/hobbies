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
  TK_KEYWORD,
} TokenKind;

typedef enum
{
  KW_RETURN,
  KW_IF,
  KW_ELSE,
  KW_WHILE,
  KW_FOR,
  KW_INT,
} Keyword;

typedef struct Token Token;

struct Token
{
  TokenKind kind;
  Token *next;
  int val; // only TK_NUMBER
  char *str;
  int len; // 0 if TK_NUMBER
  Keyword kwd; // TK_KEYWORD
};

void set_token(Token *tok);

bool consume_reserved(char *op);
Token *consume_ident();
bool consume_keyword(Keyword kwd);
bool consume(TokenKind kind);
void expect_reserved(char *op);
int expect_number();
bool at_eof();

Token *tokenize(char *p);

void print_token(Token *token, FILE* s);

#endif