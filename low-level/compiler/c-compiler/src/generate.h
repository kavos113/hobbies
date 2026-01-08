#ifndef GENERATE_H
#define GENERATE_H

#include <stdio.h>

/*

grammer rules

program = func+
func    = "int" ident "(" (type ident ",")* ")" "{" stmt* "}"
stmt    = expr ";"
          | "return" expr ";"
          | "if" "(" expr ")" stmt ("else" stmt)?
          | "while" "(" expr ")" stmt
          | "for" "(" expr? ";" expr? ";" expr? ")" stmt
          | "{" stmt* "}"
          | type ident ("[" num "]")? ";"
expr    = assign
assign  = equal ("=" assign)?
equal   = compare ("==" compare | "!= compare")*
compare = add ("<" add | ">" add | "<=" add | ">=" add)*
add     = mul ("+" mul | "-" mul)*
mul     = unary ("*" unary | "/" unary)*
unary   = ("+" | "-")? primary
          | ("*" | "&") unary
          | "sizeof" unary
primary = num
          | ident ("(" (expr ",")* ")")?
          | ident "[" num "]"
          | "(" expr ")"

type    = "int" "*"*

*/
typedef enum {
  ND_ADD,
  ND_SUB,
  ND_MUL,
  ND_DIV,
  ND_NUM,     // val: number value. lhs=rhs=null
  ND_LESS,    // <
  ND_LESSEQ,  // <=
  ND_EQ,
  ND_NEQ,
  ND_ADDR,   // &, lhs
  ND_DEREF,  // *, lhs
  ND_ASSIGN,  // lhs: variable, rhs: value
  ND_LVAR,    // offset: offset from rbp. type
  ND_RETURN,  // lhs: expr, rhs=null
  ND_IF,      // cond: condition, lhs: if stmt, rhs: else stmt(nullable)
  ND_WHILE,   // cond: condition, lhs: stmt
  ND_FOR,     // cond: condition, lhs: stmt, rhs: update, init: init
  ND_BLOCK,   // next: stmt head (continue to next, null: end)
  ND_FNCL,    // name, name_len: func name, next: args head
  ND_FNARG, // next: next arg(null: end), lhs(call only), offset(def only)
  ND_FNDEF,   // name, name_len: func name, next: args head, offset: total offset
  ND_SIZEOF,  // lhs
} NodeKind;

typedef struct Node Node;

typedef struct Type
{
  enum
  {
    INT,
    PTR,
    ARRAY,
  } type;
  struct Type *base;
  size_t array_size;
} Type;

struct Node 
{
  NodeKind kind;
  Node *lhs;
  Node *rhs;
  int val; // only ND_NUM, ND_FNARG
  int offset; // only ND_LVAR, ND_FNARG, ND_FNDEF
  Node *cond; // only ND_IF, WHILE, FOR
  Node *init; // only ND_FOR
  Node *next; // only ND_BLOCK, ND_FUNC, ND_FNARG
  char *name; // only ND_FNCL, ND_FNDEF
  int name_len; // only ND_FNCL, ND_FNDEF
  Node **stmts; // only ND_FNDEF
  Type *type;
};

void print_node(Node *node, int depth, FILE *s);
void print_lvar();

typedef struct Variable
{
  struct Variable *next;
  enum
  {
    LOCAL,
    GLOBAL
  } scope;
  char *name;
  int len;
  int offset; // size if global
  Type *type;
} Variable;

int get_offsets();

void program(Node **dst);
void generate(Node *node);

#endif