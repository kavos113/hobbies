#ifndef GENERATE_H
#define GENERATE_H

#include <stdio.h>

/*

grammer rules

expr    = equal
equal   = compare ("==" compare | "!= compare")*
compare = add ("<" add | ">" add | "<=" add | ">=" add)*
add     = mul ("+" mul | "-" mul)*
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
  ND_LESS,   // <
  ND_LESSEQ, // <=
  ND_EQ,
  ND_NEQ,
} NodeKind;

typedef struct Node Node;

struct Node 
{
  NodeKind kind;
  Node *lhs;
  Node *rhs;
  int val;
};

void print_node(Node *node, int depth, FILE *s);
Node *new_node_op(NodeKind kind, Node *lhs, Node *rhs);
Node *new_node_num(int val);

Node *expr();
Node *equal();
Node *compare();
Node *add();
Node *mul();
Node *unary();
Node *primary();

void generate(Node *node);

#endif