#include <stdio.h>

#include "tokenizer.h"
#include "parser.h"
#include "generator.h"

int main(int argc, char **argv)
{
  char *program = "add t0, t0, t1\n";
  
  char *output_path = "out";
  // FILE *f = fopen(output_path, "w");
  // if (!f)
  // {
  //   fprintf(stderr, "cannot open file\n");
  //   exit(1);
  // }

  Token *tok = tokenize(program);
  print_token(tok);
  Instruction *inst = parse(tok);
  while(inst)
  {
    unsigned int result = generate_instruction(inst);
    printf("%x\n", result);

    inst = inst->next;
  }
}