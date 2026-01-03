#include "generate.h"

#include <stdlib.h>

#include "token.h"
#include "util.h"

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
  case ND_ASSIGN:
    fprintf(s, "ND_ASSIGN\n");
    break;
  case ND_LVAR:
    fprintf(s, "ND_LVAR\n");
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

void program(Node **dst)
{
  int i = 0;
  while (!at_eof())
    dst[i++] = stmt();
  dst[i] = NULL;
}

Node* stmt()
{
  Node *node = expr();
  expect_op(";");
  return node;
}

Node *expr()
{
  return assign();
}

Node* assign()
{
  Node *node = equal();

  if (consume_op("="))
    node = new_node_op(ND_ASSIGN, node, assign());
  return node;
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

  // ident
  Token *tok = consume_ident();
  if (tok)
  {
    Node *node = calloc(1, sizeof(Node));
    node->kind = ND_LVAR;
    node->offset = (tok->str[0] - 'a' + 1) * 8;
    return node;
  }

  // num
  return new_node_num(expect_number());
}

void generate(Node *node) 
{
  switch (node->kind)
  {
  case ND_NUM:
    printf("  push %d\n", node->val);
    return;
  case ND_LVAR:
    // 変数の中身をスタックにpush
    generate_lval(node);
    printf("  pop rax\n");
    printf("  mov rax, [rax]\n");
    printf("  push rax");
    return;
  case ND_ASSIGN:
    generate_lval(node->lhs);
    generate(node->rhs);

    printf("  pop rdi\n");
    printf("  pop rax\n");
    printf("  mov [rax], rdi\n");
    printf("  push rdi");
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

// 変数のアドレスをpush
void generate_lval(Node* node)
{
  if (node->kind)
    error("not left value");

  printf("  mov rax, rbp\n");
  printf("  sub rax, %d\n", node->offset);
  printf("  push rax\n");
}
