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
  int len;
};

Token *token;

char *user_input;

void error(char *fmt, ...)
{
  va_list ap;
  va_start(ap, fmt);
  vfprintf(stderr, fmt, ap);
  fprintf(stderr, "\n");
  exit(1);
}

void error_at(char *loc, char *fmt, ...)
{
  va_list ap;
  va_start(ap, fmt);

  int pos = loc - user_input;
  fprintf(stderr, "%s\n", user_input);
  fprintf(stderr, "%*s", pos, " ");
  fprintf(stderr, "^ ");
  vfprintf(stderr, fmt, ap);
  fprintf(stderr, "\n");
  exit(1);
}

// if b starts with a
bool starts_with(char *a, char *b)
{
  return memcmp(a, b, strlen(a)) == 0;
}

bool consume_op(char *op)
{
  if (token->kind != TK_RESERVED 
    || strlen(op) != token->len
    || memcmp(token->str, op, token->len)
  )
    return false;
  
  token = token->next;
  return true;
}

void expect_op(char op)
{
  if (token->kind != TK_RESERVED 
    || strlen(op) != token->len
    || memcmp(token->str, op, token->len)
  )
    error_at(token->str, token->str, "op is not '%c'", op);
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

    if (starts_with("==", p) || starts_with("!=", p) || starts_with("<=", p) || starts_with(">=", p))
    { 
      current = new_token(TK_RESERVED, current, p);
      p += 2;
      continue;
    }

    if (strchr("+-*/()<>", *p))
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

void print_token(Token *token)
{
  fprintf(stdout, "%s, \n", token->str);

  if (token->next)
    print_token(token->next);
}

/*

grammer rules

expr    = mul ("+" mul | "-" mul)*
mul     = unary ("*" unary | "/" unary)*
unary   = ("+" | "-")? primary
primary = num | "(" expr ")"

*/
typedef enum {
  ND_ADD,
  ND_SUB,
  ND_MUL,
  ND_DIV,
  ND_NUM,
} NodeKind;

typedef struct Node Node;

struct Node 
{
  NodeKind kind;
  Node *lhs;
  Node *rhs;
  int val;
};

Node *new_node_op(NodeKind kind, Node *lhs, Node *rhs)
{
  Node *node = calloc(1, sizeof(Node));
  node->kind = kind;
  node->lhs = lhs;
  node->rhs = rhs;
  return node;
}

Node *new_node_num(int val)
{
  Node *node = calloc(1, sizeof(Node));
  node->kind = ND_NUM;
  node->val = val;
  return node;
}

Node *expr();

Node *primary()
{
  // "(" expr ")"
  if (consume_op("("))
  {
    Node *node = expr();
    expect_op(")");
    return node;
  }

  // num
  return new_node_num(expect_number());
}

Node *unary()
{
  // "+"?
  if (consume_op("+"))
    return primary();
  
  // "-"?
  if (consume_op("-"))
    return new_node_op(ND_SUB, new_node_num(0), primary());

  return primary();
}

Node *mul()
{
  // primary
  Node *node = unary();

  // ()*
  for (;;)
  {
    // "*" primary
    if (consume_op("*"))
      node = new_node_op(ND_MUL, node, unary());
    // "/" primary
    else if (consume_op("/"))
      node = new_node_op(ND_DIV, node, unary());
    else 
      return node;
  }
}

Node *expr() 
{
  // mul
  Node *node = mul();

  // ()*
  for (;;)
  {
    // "+" mul
    if (consume_op("+"))
      node = new_node_op(ND_ADD, node, mul());
    // "-" mul
    else if (consume_op("-"))
      node = new_node_op(ND_SUB, node, mul());
    else 
      return node;
  }
}

void generate(Node *node) 
{
  if (node->kind == ND_NUM)
  {
    printf("  push %d\n", node->val);
    return;
  }

  generate(node->lhs);
  generate(node->rhs);

  printf("  pop rdi\n");
  printf("  pop rax\n");

  switch (node->kind)
  {
  case ND_ADD:
    printf("  add rax, rdi\n");
    break;
  case ND_SUB:
    printf("  sub rax, rdi\n");
    break;
  case ND_MUL:
    printf("  imul rax, rdi\n");
    break;
  case ND_DIV:
    printf("  cqo\n");
    printf("  idiv rdi\n");
    break;
  }

  printf("  push rax\n");
}

int main(int argc, char **argv)
{
  if (argc != 2)
  {
    fprintf(stderr, "arg: number");
    return 1;
  }

  token = tokenize(argv[1]);
  user_input = argv[1];
  Node *node = expr();

  printf(".intel_syntax noprefix\n");
  printf(".global main\n");
  printf("main:\n");

  generate(node);

  printf("  pop rax\n");
  printf("  ret\n");
  return 0;
}