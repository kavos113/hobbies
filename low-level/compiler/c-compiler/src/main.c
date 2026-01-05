#include <stdio.h>

#include "token.h"
#include "generate.h"
#include "util.h"
#include "io.h"

int main(int argc, char **argv)
{
  if (argc != 3)
  {
    fprintf(stderr, "usage: acc <file_path> <output_path>");
    return 1;
  }
  
  char *source = read_file(argv[1]); 

  set_token(tokenize(source));
  set_user_input(source);

  Node *code[100];
  program(code);

  open_output_file(argv[2]);

  write_output(".intel_syntax noprefix\n");
  write_output(".global main\n");
  write_output("main:\n");

  write_output("  push rbp\n");
  write_output("  mov rbp, rsp\n");
  write_output("  sub rsp, %d\n", get_offsets());

  for (int i = 0; code[i]; i++)
  {
    generate(code[i]);
    write_output("  pop rax\n");
  }

  close_file();
  return 0;
}