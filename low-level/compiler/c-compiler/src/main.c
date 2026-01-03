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

  Node *code[100];
  program(code);

  printf(".intel_syntax noprefix\n");
  printf(".global main\n");
  printf("main:\n");

  printf("  push rbp\n");
  printf("  mov rbp, rsp\n");
  printf("  sub rsp, %d\n", 26 * 8);

  for (int i = 0; code[i]; i++)
  {
    generate(code[i]);
    printf("  pop rax\n");
  }

  printf("  mov rsp, rbp\n");
  printf("  pop rbp\n");
  printf("  ret\n");
  return 0;
}