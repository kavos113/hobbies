#include <stdio.h>
#include <stdlib.h>

#include "tokenizer.h"
#include "parser.h"
#include "generator.h"

int main(int argc, char **argv)
{
  char *program = "add t0, t0, t1\n";
  
  char *output_path = "out";
  FILE *f = fopen(output_path, "wb");
  if (!f)
  {
    fprintf(stderr, "cannot open file\n");
    exit(1);
  }

  Token *tok = tokenize(program);
  Instruction *inst = parse(tok);
  while(inst)
  {
    unsigned int result = generate_instruction(inst);
    fwrite(&result, sizeof(unsigned int), 1, f);

    inst = inst->next;
  }

  fclose(f);

  return 0;
}