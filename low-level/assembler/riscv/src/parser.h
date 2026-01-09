#ifndef PARSER_H
#define PARSER_H

#include "riscv.h"
#include "tokenizer.h"

Instruction *parse(Token *tok);

#endif // PARSER_H