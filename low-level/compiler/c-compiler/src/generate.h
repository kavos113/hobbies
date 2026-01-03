#ifndef GENERATE_H
#define GENERATE_H

#include <stdio.h>

/*

grammer rules

program = stmt*
stmt    = expr ";"
expr    = assign
assign  = equal ("=" assign)?
equal   = compare ("==" compare | "!= compare")*
compare = add ("<" add | ">" add | "<=" add | ">=" add)*
add     = mul ("+" mul | "-" mul)*
mul     = unary ("*" unary | "/" unary)*
unary   = ("+" | "-")? primary
primary = num | ident | "(" expr ")"

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
  ND_ASSIGN,
  ND_LVAR, // local variable
} NodeKind;

typedef struct Node Node;

struct Node 
{
  NodeKind kind;
  Node *lhs;
  Node *rhs;
  int val; // only ND_NUM
  int offset; // only ND_LVAR, offset from rbp
};

void print_node(Node *node, int depth, FILE *s);
Node *new_node_op(NodeKind kind, Node *lhs, Node *rhs);
Node *new_node_num(int val);

void program(Node **dst);
Node *stmt();
Node *expr();
Node *assign();
Node *equal();
Node *compare();
Node *add();
Node *mul();
Node *unary();
Node *primary();

void generate(Node *node);
void generate_lval(Node *node);

#endif