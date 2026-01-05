#ifndef GENERATE_H
#define GENERATE_H

#include <stdio.h>

/*

grammer rules

program = stmt*
stmt    = expr ";"
          | "return" expr ";"
          | "if" "(" expr ")" stmt ("else" stmt)?
          | "while" "(" expr ")" stmt
          | "for" "(" expr? ";" expr? ";" expr? ")" stmt
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
  ND_NUM,    // val: number value. lhs=rhs=null
  ND_LESS,   // <
  ND_LESSEQ, // <=
  ND_EQ,
  ND_NEQ,
  ND_ASSIGN, // lhs: variable, rhs: value
  ND_LVAR,   // offset: offset from rbp. lhs=rhs=null
  ND_RETURN, // lhs: expr, rhs=null
  ND_IF,     // cond: condition, lhs: if stmt, rhs: else stmt(nullable)
  ND_WHILE,  // cond: condition, lhs: stmt
  ND_FOR,    // cond: condition, lhs: stmt, rhs: update, init: init
} NodeKind;

typedef struct Node Node;

struct Node 
{
  NodeKind kind;
  Node *lhs;
  Node *rhs;
  int val; // only ND_NUM
  int offset; // only ND_LVAR
  Node *cond; // only ND_IF, WHILE, FOR
  Node *init; // only ND_FOR
};

void print_node(Node *node, int depth, FILE *s);

typedef struct LVar LVar;

struct LVar
{
  LVar *next;
  char *name;
  int len;
  int offset;
};

int get_offsets();

void program(Node **dst);
void generate(Node *node);

#endif