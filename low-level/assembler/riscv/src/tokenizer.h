#ifndef TOKENIZER_H
#define TOKENIZER_H

typedef enum 
{
  T_STR,
  T_NUM,
  T_SYM,
  T_EOL,
  T_EOF,   
} TokenType;

typedef struct Token
{
  TokenType type;
  char *str;
  int len;
  int val;

  struct Token *next;
} Token;

Token *tokenize(char *p);

void print_token(Token *tok);

#endif // TOKENIZER_H