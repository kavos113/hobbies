#ifndef UTIL_H
#define UTIL_H

#include <stdbool.h>

void set_user_input(char *input);

void error_at(char *loc, char *fmt, ...);
void error(char *fmt, ...);

#endif