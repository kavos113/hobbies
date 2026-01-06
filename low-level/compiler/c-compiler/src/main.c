#include <stdio.h>

#include "token.h"
#include "generate.h"
#include "util.h"
#include "io.h"

int main(int argc, char **argv)
{
  if (argc != 3)
  {
    fprintf(stderr, "usage: acc <file_path> <output_path>\n");
    return 1;
  }
  
  char *source = read_file(argv[1]); 

  set_token(tokenize(source));
  set_user_input(source);

  Node *node[100];
  program(node);

  open_output_file(argv[2]);

  write_output(".intel_syntax noprefix\n");
  write_output(".global main\n");

  for (int i = 0; node[i]; i++)
  {
    generate(node[i]);
    // print_node(node[i], 0, stdout);
  }

  close_file();
  return 0;
}