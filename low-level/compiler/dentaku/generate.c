#include "generate.h"

#include <stdlib.h>

#include "token.h"

void print_node(Node *node, int depth, FILE *s)
{
  fprintf(s, "%*s", depth, " ");
  switch (node->kind)
  {
  case ND_ADD:
    fprintf(s, "ND_ADD\n");
    break;
  case ND_SUB:
    fprintf(s, "ND_SUB\n");
    break;
  case ND_MUL:
    fprintf(s, "ND_MUL\n");
    break;
  case ND_DIV:
    fprintf(s, "ND_DIV\n");
    break;
  case ND_NUM:
    fprintf(s, "ND_NUM: %d\n", node->val);
    break;
  case ND_LESS:
    fprintf(s, "ND_LESS\n");
    break;
  case ND_LESSEQ:
    fprintf(s, "ND_LESSEQ\n");
    break;
  case ND_EQ:
    fprintf(s, "ND_EQ\n");
    break;
  case ND_NEQ:
    fprintf(s, "ND_NEQ\n");
    break;
  }

  if (node->lhs)
    print_node(node->lhs, depth + 2, s);
  if (node->rhs)
    print_node(node->rhs, depth + 2, s);
}

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

Node *expr()
{
  return equal();
}

Node *equal()
{
  Node *node = compare();

  for (;;)
  {
    if (consume_op("=="))
      node = new_node_op(ND_EQ, node, compare());
    else if (consume_op("!="))
      node = new_node_op(ND_NEQ, node, compare());
    else 
      return node;
  }
}

Node *compare()
{
  Node *node = add();

  for (;;)
  {
    if (consume_op("<"))
      node = new_node_op(ND_LESS, node, add());
    else if (consume_op("<="))
      node = new_node_op(ND_LESSEQ, node, add());
    else if (consume_op(">"))
      node = new_node_op(ND_LESS, add(), node);
    else if (consume_op(">="))
      node = new_node_op(ND_LESSEQ, add(), node);
    else 
      return node;
  }
}

Node *add() 
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
  case ND_LESS:
    printf("  cmp rax, rdi\n");
    printf("  setl al\n");
    printf("  movzb rax, al\n");
    break;
  case ND_LESSEQ:
    printf("  cmp rax, rdi\n");
    printf("  setle al\n");
    printf("  movzb rax, al\n");
    break;
  case ND_EQ:
    printf("  cmp rax, rdi\n");
    printf("  sete al\n");
    printf("  movzb rax, al\n");
    break;
  case ND_NEQ:
    printf("  cmp rax, rdi\n");
    printf("  setne al\n");
    printf("  movzb rax, al\n");
    break;
  }

  printf("  push rax\n");
}