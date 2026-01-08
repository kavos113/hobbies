#include "generate.h"

#include <stdlib.h>
#include <string.h>

#include "util.h"
#include "io.h"
#include "token.h"

Node *new_node_op(NodeKind kind, Node *lhs, Node *rhs);
Node *new_node_num(int val);
Variable *find_lvar(Token *tok);
Variable *new_lvar(Token *tok, size_t size);
Variable *find_gvar(Token *tok);
Variable *new_gvar(Token *tok, size_t size);
Variable *find_var(Token *tok);

Node *func();
Node *stmt();
Type *type();
Node *expr();
Node *assign();
Node *equal();
Node *compare();
Node *add();
Node *mul();
Node *unary();
Node *primary();

void add_type(Node *node);
bool is_equal_type(Type *t1, Type *t2);
int type_size(Type *type);

void generate_val_addr(Node *node);

static Variable *locals;
static Variable *globals;
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
  case ND_FNARG:
    fprintf(s, "ND_FNARG\n");
    break;
  case ND_FNDEF:
    fprintf(s, "ND_FNDEF\n");
    break;
  case ND_ADDR:
    fprintf(s, "ND_ADDR\n");
    break;
  case ND_DEREF:
    fprintf(s, "ND_DEREF\n");
    break;
  case ND_SIZEOF:
    fprintf(s, "ND_SIZEOF\n");
    break;
  }

  if (node->lhs)
    print_node(node->lhs, depth + 2, s);
  if (node->rhs)
    print_node(node->rhs, depth + 2, s);
  if (node->next)
    print_node(node->next, depth, s);
  if (node->stmts)
  {
    int i = 0;
    while (node->stmts[i])
    {
      print_node(node->stmts[i++], depth + 2, s);
      fprintf(s, "\n");
    }
  }
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

Variable *find_var(Token *tok)
{
  Variable *var = find_lvar(tok);
  if (var)
    return var;

  return find_gvar(tok);
}

Variable *find_lvar(Token* tok)
{
  for (Variable *var = locals; var; var = var->next)
    if (var->len == tok->len && !memcmp(tok->str, var->name, var->len))
      return var;

  return NULL;
}

Variable* new_lvar(Token* tok, size_t size)
{
  Variable *var = calloc(1, sizeof(Variable));

  if (locals)
  {
    var->next = locals;
    var->offset = locals->offset + size;
  }
  else
    var->offset = size;

  var->name = tok->str;
  var->len = tok->len;
  var->scope = LOCAL;
  locals = var;

  return var;
}

Variable* find_gvar(Token* tok)
{
  for (Variable *var = globals; var; var = var->next)
    if (var->len == tok->len && !memcmp(tok->str, var->name, var->len))
      return var;

  return NULL;
}

Variable* new_gvar(Token* tok, size_t size)
{
  Variable *var = calloc(1, sizeof(Variable));
  
  if (globals)
    var->next = globals;
  
  var->name = tok->str;
  var->len = tok->len;
  var->offset = size;
  var->scope = GLOBAL;
  globals = var;
  
  return var;
}

int get_offsets()
{
  if (locals)
    return locals->offset;
  else
    return 0;
}

void print_lvar_internal(Variable *lvar)
{
  if (lvar)
  {
    fprintf(stdout, "%.*s: %d\n", lvar->len, lvar->name, lvar->offset);
    print_lvar_internal(lvar->next);
  }
}

void print_lvar()
{
  print_lvar_internal(locals);
}

void program(Node** dst)
{
  int i = 0;
  while (!at_eof())
  {
    dst[i] = func();
    add_type(dst[i]);
    i++;
  }
  dst[i] = NULL;
}

Node* func()
{
  if (!consume_keyword(KW_INT))
    error("first must be INT keyword\n");

  Token *func_ident = consume_ident();
  if (!func_ident)
    error("first must be function identifier\n");

  Node *node = calloc(1, sizeof(Node));
  node->kind = ND_FNDEF;
  node->stmts = calloc(100, sizeof(Node *));
  node->name = func_ident->str;
  node->name_len = func_ident->len;

  expect_reserved("(");

  Node **lastarg = &node->next;
  bool is_first = true;
  while (!consume_reserved(")"))
  {
    if (is_first)
      is_first = false;
    else
      expect_reserved(",");

    Type *ty = type();
    if (!ty)
      error("function argument need type keyword\n");

    Token *argident = consume_ident();
    if (!argident)
      error("no argument identifier\n");

    Node *arg = calloc(1, sizeof(Node));
    arg->kind = ND_FNARG;
    arg->type = ty;

    Variable *var = new_lvar(argident, 8);
    var->type = ty;
    arg->offset = var->offset;

    *lastarg = arg;
    lastarg = &arg->next;
  }

  expect_reserved("{");

  int i = 0;
  while (!consume_reserved("}"))
  {
    node->stmts[i] = stmt();
    i++;
  }

  node->offset = get_offsets();
  locals = NULL;

  return node;
}

Node* stmt()
{
  // "return" expr ";"
  if (consume_keyword(KW_RETURN))
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
  if (consume_keyword(KW_IF))
  {
    expect_reserved("(");
    Node *cond = expr();
    expect_reserved(")");

    Node *node = calloc(1, sizeof(Node));
    node->kind = ND_IF;
    node->cond = cond;
    node->lhs = stmt();

    if (consume_keyword(KW_ELSE))
      node->rhs = stmt();

    return node;
  }

  // "while" "(" expr ")" stmt
  if (consume_keyword(KW_WHILE))
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
  if (consume_keyword(KW_FOR))
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

  // type ident ";"
  Type *ty = type();
  if (ty)
  {
    Token *tok = consume_ident();

    if (find_var(tok))
      error_at(tok->str, "this var is already defined\n");

    Variable *var;
    if (consume_reserved("["))
    {
      int size = expect_number();
      expect_reserved("]");

      Type *atype = calloc(1, sizeof(Node));
      atype->type = ARRAY;
      atype->array_size = size;
      atype->base = ty;

      ty = atype;

      var = new_lvar(tok, type_size(ty));
      var->type = ty;
    }
    else
    {
      var = new_lvar(tok, 8);
      var->type = ty;
    }

    Node *node = calloc(1, sizeof(Node));
    node->kind = ND_LVAR;
    node->offset = var->offset;
    node->type = var->type;

    expect_reserved(";");

    return node;
  }

  // expr ";"
  Node *node = expr();

  expect_reserved(";");
  return node;
}

Type* type()
{
  if (!consume_keyword(KW_INT))
    return NULL;

  Type *type = calloc(1, sizeof(Type));
  type->type = INT;

  while (consume_reserved("*"))
  {
    Type *t = calloc(1, sizeof(Type));
    t->type = PTR;
    t->base = type;

    type = t;
  }

  return type;
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

  if (consume_reserved("*"))
    return new_node_op(ND_DEREF, unary(), NULL);

  if (consume_reserved("&"))
    return new_node_op(ND_ADDR, unary(), NULL);

  if (consume_keyword(KW_SIZEOF))
    return new_node_op(ND_SIZEOF, unary(), NULL);

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

        Node *arg = calloc(1, sizeof(Node));
        arg->kind = ND_FNARG;
        arg->lhs = expr();
        arg->offset = -1;
        *lastarg = arg;
        lastarg = &arg->next;
      }

      return node;
    }

    if (consume_reserved("["))
    {
      int index = expect_number();
      expect_reserved("]");

      Variable *array = find_var(tok);
      if (!array)
        error_at(tok->str, "undefined array\n");

      Node *node = calloc(1, sizeof(Node));
      node->kind = ND_DEREF;

      Node *addr = calloc(1, sizeof(Node));
      addr->kind = ND_ADD;
      addr->lhs = calloc(1, sizeof(Node));
      addr->lhs->kind = ND_LVAR;
      addr->lhs->offset = array->offset;
      addr->lhs->type = array->type;
      addr->rhs = calloc(1, sizeof(Node));
      addr->rhs->kind = ND_NUM;
      addr->rhs->type = calloc(1, sizeof(Type));
      addr->rhs->type->type = INT;
      addr->rhs->val = index;

      node->lhs = addr;

      return node;
    }

    Node *node = calloc(1, sizeof(Node));
    node->kind = ND_LVAR;

    Variable *var = find_var(tok);
    if (var)
    {
      node->offset = var->offset;
      node->type = var->type;
    }
    else
      error_at(tok->str, "undefined variable\n");

    return node;
  }

  // num
  return new_node_num(expect_number());
}

void add_type(Node* node)
{
  if (!node || node->type)
    return;

  add_type(node->lhs);
  add_type(node->rhs);
  add_type(node->next);
  add_type(node->cond);
  add_type(node->init);

  int i = 0;
  while (node->stmts && node->stmts[i])
    add_type(node->stmts[i++]);

  // ND_LVAR: add type when create node
  // ND_IF, WHILE, FOR, BLOCK, FN: no type
  switch (node->kind)
  {
  case ND_ADD:
  case ND_SUB:
  case ND_MUL:
  case ND_DIV:
    if (node->lhs->type->type == PTR)
      node->type = node->lhs->type;
    else if (node->lhs->type->type == ARRAY)
    {
      Type *ty = calloc(1, sizeof(Type));
      ty->type = PTR;
      ty->base = node->lhs->type->base;
      node->type = ty;
    }
    else if (node->rhs->type->type == PTR)
      node->type = node->rhs->type;
    else if (node->rhs->type->type == ARRAY)
    {
      Type *ty = calloc(1, sizeof(Type));
      ty->type = PTR;
      ty->base = node->rhs->type->base;
      node->type = ty;
    }
    else
    {
      Type *ty = calloc(1, sizeof(Type));
      ty->type = INT;
      node->type = ty;
    }
    return;

  case ND_NUM:
  case ND_LESS:
  case ND_LESSEQ:
  case ND_EQ:
  case ND_NEQ:
  case ND_SIZEOF:
  case ND_FNCL:
    {
      Type *ty = calloc(1, sizeof(Type));
      ty->type = INT;
      node->type = ty;
    }
    return;

  case ND_ADDR:
    {
      Type *ty = calloc(1, sizeof(Type));
      ty->type = PTR;
      ty->base = node->lhs->type;
      node->type = ty;
    }
    return;

  case ND_DEREF:
    if (node->lhs->type->type != PTR && node->lhs->type->type != ARRAY)
      error("Type Error: cannot dereference non-pointer variable\n");
    else
      node->type = node->lhs->type->base;
    return;

  case ND_ASSIGN:
    if (!is_equal_type(node->lhs->type, node->rhs->type))
      error("Type Error: cannot assign different type\n");
    else
      node->type = node->lhs->type;
    return;

  case ND_RETURN:
    node->type = node->lhs->type;
    return;
  }
}

bool is_equal_type(Type* t1, Type* t2)
{
  if (t1->type != t2->type)
    return false;

  if (t1->type == INT && t2->type == INT)
    return true;

  return is_equal_type(t1->base, t2->base);
}

int type_size(Type* type)
{
  if (type->type == PTR)
    return 8;
  else if (type->type == INT)
    return 4;
  else
    return type->array_size * type_size(type->base);
}

void generate(Node *node)
{
  switch (node->kind)
  {
  case ND_NUM:
    write_output("  push %d\n", node->val);
    return;
  case ND_LVAR:
    generate_val_addr(node);
    if (node->type->type == ARRAY)
      return;

    // 変数の中身をスタックにpush
    write_output("  pop rax\n");
    write_output("  mov rax, [rax]\n");
    write_output("  push rax\n");
    return;
  case ND_ASSIGN:
    if (node->lhs->kind == ND_DEREF)
      generate(node->lhs->lhs);
    else
      generate_val_addr(node->lhs);

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
    // write_output("  pop rax\n");

    char *regs[6] = {"rdi", "rsi", "rdx", "rcx", "r8", "r9"};
    Node *arg = node->next;
    for (int i = 0; i < 6; i++)
    {
      if (!arg) break;

      generate(arg->lhs);
      write_output("  pop rax\n");
      write_output("  mov %s, rax\n", regs[i]);
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
    write_output("  sub rsp, %d\n", node->offset);

    char *regs[6] = {"rdi", "rsi", "rdx", "rcx", "r8", "r9"};
    Node *arg = node->next;
    for (int i = 0; i < 6; i++)
    {
      if (!arg) break;

      write_output("  mov rax, rbp\n");
      write_output("  sub rax, %d\n", arg->offset);
      write_output("  mov [rax], %s\n", regs[i]);

      arg = arg->next;
    }

    for (int i = 0; 1; i++)
    {
      if (!node->stmts[i])
        break;
      generate(node->stmts[i]);
    }
  }
    return;
  case ND_ADDR:
    generate_val_addr(node->lhs);
    return;
  case ND_DEREF:
    generate(node->lhs);
    write_output("  pop rax\n");
    write_output("  mov rax, [rax]\n");
    write_output("  push rax\n");
    return;
  case ND_SIZEOF:
    if (!node->lhs->type)
      error("Type Error: cannot 'sizeof' for no-value\n");
    else
      write_output("  push %d\n", type_size(node->lhs->type));
    return;
  }

  generate(node->lhs);
  generate(node->rhs);

  write_output("  pop rdi\n"); // rhs
  write_output("  pop rax\n"); // lhs

  switch (node->kind)
  {
  case ND_ADD:
    if (node->lhs->type->type == PTR)
      write_output("  imul rax, %d\n", type_size(node->lhs->type));
    else if (node->rhs->type->type == PTR)
      write_output("  imul rdi, %d\n", type_size(node->rhs->type));
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
void generate_val_addr(Node* node)
{
  if (node->kind != ND_LVAR)
  {
    print_node(node, 0, stdout);
    error("not left value\n");
  }

  write_output("  mov rax, rbp\n");
  write_output("  sub rax, %d\n", node->offset);
  write_output("  push rax\n");
}
