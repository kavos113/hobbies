#include "generate.h"

#include <stdlib.h>
#include <string.h>

#include "util.h"
#include "io.h"
#include "token.h"

Node *new_node_op(NodeKind kind, Node *lhs, Node *rhs);
Node *new_node_num(int val);
LVar *find_lvar(Token *tok);
LVar *new_lvar(Token *tok);

Node *func();
Node *stmt();
Node *expr();
Node *assign();
Node *equal();
Node *compare();
Node *add();
Node *mul();
Node *unary();
Node *primary();

void generate_lval(Node *node);

static LVar *locals;
static unsigned int label_latest = 0;

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
  case ND_RETURN:
    fprintf(s, "ND_RETURN\n");
    break;
  case ND_IF:
    fprintf(s, "ND_IF\n");
    break;
  case ND_WHILE:
    fprintf(s, "ND_WHILE\n");
    break;
  case ND_FOR:
    fprintf(s, "ND_FOR\n");
    break;
  case ND_BLOCK:
    fprintf(s, "ND_BLOCK\n");
    break;
  case ND_FNCL:
    fprintf(s, "ND_FNCL\n");
    break;
  case ND_FNCLARG:
    fprintf(s, "ND_FNCLARG\n");
    break;
  case ND_FNDEF:
    fprintf(s, "ND_FNDEF\n");
    break;
  }

  if (node->lhs)
    print_node(node->lhs, depth + 2, s);
  if (node->rhs)
    print_node(node->rhs, depth + 2, s);
  if (node->next)
    print_node(node->next, depth, s);
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

LVar *find_lvar(Token* tok)
{
  for (LVar *var = locals; var; var = var->next)
    if (var->len == tok->len && !memcmp(tok->str, var->name, var->len))
      return var;

  return NULL;
}

LVar* new_lvar(Token* tok)
{
  LVar *var = calloc(1, sizeof(LVar));

  if (locals)
  {
    var->next = locals;
    var->offset = locals->offset + 8;
  }

  var->name = tok->str;
  var->len = tok->len;
  locals = var;

  return var;
}

int get_offsets()
{
  if (locals)
    return locals->offset;
  else
    return 0;
}

void program(Node** dst)
{
  int i = 0;
  while (!at_eof())
    dst[i++] = func();
  dst[i] = NULL;
}

Node* func()
{
  Token *func_ident = consume_ident();
  if (!func_ident)
    error("first must be function identifier\n");

  Node *node = calloc(1, sizeof(Node));
  node->kind = ND_FNDEF;
  node->stmts = calloc(100, sizeof(Node *));
  node->name = func_ident->str;
  node->name_len = func_ident->len;

  expect_reserved("(");
  expect_reserved(")");
  expect_reserved("{");

  int i = 0;
  while (!consume_reserved("}"))
  {
    node->stmts[i] = stmt();
    i++;
  }

  return node;
}

Node* stmt()
{
  // "return" expr ";"
  if (consume(TK_RETURN))
  {
    Node *node = calloc(1, sizeof(Node));
    node->kind = ND_RETURN;
    node->lhs = expr();

    expect_reserved(";");
    return node;
  }

  // "{" stmt* "}"
  if (consume_reserved("{"))
  {
    Node *node = calloc(1, sizeof(Node));
    node->kind = ND_BLOCK;

    Node **next = &node->next;
    while (1)
    {
      if (consume_reserved("}"))
        break;

      Node *content = stmt();
      *next = content;
      next = &content->next;
    }

    return node;
  }

  // "if" "(" expr ")" stmt ("else" stmt)?
  if (consume(TK_IF))
  {
    expect_reserved("(");
    Node *cond = expr();
    expect_reserved(")");

    Node *node = calloc(1, sizeof(Node));
    node->kind = ND_IF;
    node->cond = cond;
    node->lhs = stmt();

    if (consume(TK_ELSE))
      node->rhs = stmt();

    return node;
  }

  // "while" "(" expr ")" stmt
  if (consume(TK_WHILE))
  {
    expect_reserved("(");
    Node *cond = expr();
    expect_reserved(")");

    Node *node = calloc(1, sizeof(Node));
    node->kind = ND_WHILE;
    node->cond = cond;
    node->lhs = stmt();

    return node;
  }

  // "for" "(" expr? ";" expr? ";" expr? ")" stmt
  if (consume(TK_FOR))
  {
    expect_reserved("(");
    Node *init = NULL;
    Node *cond = NULL;
    Node *update = NULL;
    if (!consume_reserved(";"))
    {
      init = expr();
      expect_reserved(";");
    }
    if (!consume_reserved(";"))
    {
      cond = expr();
      expect_reserved(";");
    }
    if (!consume_reserved(")"))
    {
      update = expr();
      expect_reserved(")");
    }

    Node *node = calloc(1, sizeof(Node));
    node->kind = ND_FOR;
    node->cond = cond;
    node->lhs = stmt();
    node->rhs = update;
    node->init = init;

    return node;
  }

  // expr ";"
  Node *node = expr();

  expect_reserved(";");
  return node;
}

Node *expr()
{
  return assign();
}

Node* assign()
{
  Node *node = equal();

  if (consume_reserved("="))
    node = new_node_op(ND_ASSIGN, node, assign());
  return node;
}

Node *equal()
{
  Node *node = compare();

  for (;;)
  {
    if (consume_reserved("=="))
      node = new_node_op(ND_EQ, node, compare());
    else if (consume_reserved("!="))
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
    if (consume_reserved("<"))
      node = new_node_op(ND_LESS, node, add());
    else if (consume_reserved("<="))
      node = new_node_op(ND_LESSEQ, node, add());
    else if (consume_reserved(">"))
      node = new_node_op(ND_LESS, add(), node);
    else if (consume_reserved(">="))
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
    if (consume_reserved("+"))
      node = new_node_op(ND_ADD, node, mul());
    // "-" mul
    else if (consume_reserved("-"))
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
    if (consume_reserved("*"))
      node = new_node_op(ND_MUL, node, unary());
    // "/" primary
    else if (consume_reserved("/"))
      node = new_node_op(ND_DIV, node, unary());
    else 
      return node;
  }
}

Node *unary()
{
  // "+"?
  if (consume_reserved("+"))
    return primary();
  
  // "-"?
  if (consume_reserved("-"))
    return new_node_op(ND_SUB, new_node_num(0), primary());

  return primary();
}

Node *primary()
{
  // "(" expr ")"
  if (consume_reserved("("))
  {
    Node *node = expr();
    expect_reserved(")");
    return node;
  }

  // ident ("(" ")")?
  Token *tok = consume_ident();
  if (tok)
  {
    // "(" (num ",")* ")"
    if (consume_reserved("("))
    {
      Node *node = calloc(1, sizeof(Node));
      node->kind = ND_FNCL;
      node->name = tok->str;
      node->name_len = tok->len;

      Node **lastarg = &node->next;
      bool is_first = true;
      while (!consume_reserved(")"))
      {
        if (is_first)
          is_first = false;
        else
          expect_reserved(",");

        int argval = expect_number();

        Node *arg = calloc(1, sizeof(Node));
        arg->kind = ND_FNCLARG;
        arg->val = argval;
        *lastarg = arg;
        lastarg = &arg->next;
      }

      return node;
    }

    Node *node = calloc(1, sizeof(Node));
    node->kind = ND_LVAR;

    LVar *var = find_lvar(tok);
    if (var)
    {
      node->offset = var->offset;
    }
    else
    {
      var = new_lvar(tok);
      node->offset = var->offset;
    }

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
    write_output("  push %d\n", node->val);
    return;
  case ND_LVAR:
    // 変数の中身をスタックにpush
    generate_lval(node);
    write_output("  pop rax\n");
    write_output("  mov rax, [rax]\n");
    write_output("  push rax\n");
    return;
  case ND_ASSIGN:
    generate_lval(node->lhs);
    generate(node->rhs);

    write_output("  pop rdi\n");
    write_output("  pop rax\n");
    write_output("  mov [rax], rdi\n");
    write_output("  push rdi\n");
    return;
  case ND_RETURN:
    generate(node->lhs);

    write_output("  pop rax\n");
    write_output("  mov rsp, rbp\n");
    write_output("  pop rbp\n");
    write_output("  ret\n");
    return;
  case ND_IF:
    // if-else
    if (node->rhs)
    {
      generate(node->cond);

      unsigned int end_label = label_latest++;
      unsigned int else_label = label_latest++;

      write_output("  pop rax\n");
      write_output("  cmp rax, 0\n");
      write_output("  je .Lelse%d\n", else_label);
      generate(node->lhs);
      write_output("  jmp .Lend%d\n", end_label);
      write_output(".Lelse%d:\n", else_label);
      generate(node->rhs);
      write_output(".Lend%d:\n", end_label);
    }
    // if
    else
    {
      generate(node->cond);

      unsigned int end_label = label_latest++;

      write_output("  pop rax\n");
      write_output("  cmp rax, 0\n");
      write_output("  je .Lend%d\n", end_label);
      generate(node->lhs);
      write_output(".Lend%d:\n", end_label);
    }
    return;
  case ND_WHILE:
  {
    unsigned int loop_label = label_latest++;
    unsigned int end_label = label_latest++;

    write_output(".Lloop%d:\n", loop_label);
    generate(node->cond);
    write_output("  pop rax\n");
    write_output("  cmp rax, 0\n");
    write_output("  je .Lend%d\n", end_label);
    generate(node->lhs);
    write_output("  jmp .Lloop%d\n", loop_label);
    write_output(".Lend%d:\n", end_label);
  }
    return;
  case ND_FOR:
  {
    unsigned int loop_label = label_latest++;
    unsigned int end_label = label_latest++;

    if (node->init)
      generate(node->init);

    write_output(".Lloop%d:\n", loop_label);
    if (node->cond)
      generate(node->cond);
    write_output("  pop rax\n");
    write_output("  cmp rax, 0\n");
    write_output("  je .Lend%d\n", end_label);
    generate(node->lhs);
    if (node->rhs)
      generate(node->rhs);
    write_output("  jmp .Lloop%d\n", loop_label);
    write_output(".Lend%d:\n", end_label);
  }
    return;
  case ND_BLOCK:
    while (node->next)
    {
      generate(node->next);
      write_output("  pop rax\n");
      node = node->next;
    }
    return;
  case ND_FNCL:
  {
    write_output("  pop rax\n");

    char *regs[6] = {"rdi", "rsi", "rdx", "rcx", "r8", "r9"};
    Node *arg = node->next;
    for (int i = 0; i < 6; i++)
    {
      if (!arg) break;

      write_output("  mov %s, %d\n", regs[i], arg->val);
      arg = arg->next;
    }

    write_output("  call %.*s\n", node->name_len, node->name);
    write_output("  push rax\n");
  }
    return;
  case ND_FNDEF:
  {
    write_output("%.*s:\n", node->name_len, node->name);

    write_output("  push rbp\n");
    write_output("  mov rbp, rsp\n");
    write_output("  sub rsp, %d\n", get_offsets());
    for (int i = 0; 1; i++)
    {
      if (!node->stmts[i])
        break;
      generate(node->stmts[i]);
    }
  }
    return;
  }

  generate(node->lhs);
  generate(node->rhs);

  write_output("  pop rdi\n");
  write_output("  pop rax\n");

  switch (node->kind)
  {
  case ND_ADD:
    write_output("  add rax, rdi\n");
    break;
  case ND_SUB:
    write_output("  sub rax, rdi\n");
    break;
  case ND_MUL:
    write_output("  imul rax, rdi\n");
    break;
  case ND_DIV:
    write_output("  cqo\n");
    write_output("  idiv rdi\n");
    break;
  case ND_LESS:
    write_output("  cmp rax, rdi\n");
    write_output("  setl al\n");
    write_output("  movzb rax, al\n");
    break;
  case ND_LESSEQ:
    write_output("  cmp rax, rdi\n");
    write_output("  setle al\n");
    write_output("  movzb rax, al\n");
    break;
  case ND_EQ:
    write_output("  cmp rax, rdi\n");
    write_output("  sete al\n");
    write_output("  movzb rax, al\n");
    break;
  case ND_NEQ:
    write_output("  cmp rax, rdi\n");
    write_output("  setne al\n");
    write_output("  movzb rax, al\n");
    break;
  }

  write_output("  push rax\n");
}

// 変数のアドレスをpush
void generate_lval(Node* node)
{
  if (node->kind != ND_LVAR)
    error("not left value");

  write_output("  mov rax, rbp\n");
  write_output("  sub rax, %d\n", node->offset);
  write_output("  push rax\n");
}
