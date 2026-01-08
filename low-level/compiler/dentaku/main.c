#include <stdio.h>

#include "token.h"
#include "generate.h"
#include "util.h"

int main(int argc, char **argv)
{
  if (argc != 2)
  {
    fprintf(stderr, "arg: number");
    return 1;
  }

  set_token(tokenize(argv[1]));
  set_user_input(argv[1]);
  Node *node = expr();

  printf(".intel_syntax noprefix\n");
  printf(".global main\n");
  printf("main:\n");

  generate(node);

  printf("  pop rax\n");
  printf("  ret\n");
  return 0;
}