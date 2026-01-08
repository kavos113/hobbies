#include "io.h"

#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>

#include "util.h"

char* read_file(char* path)
{
  FILE *f = fopen(path, "r");
  if (!f)
  {
    error("failed to open %s\n", path);
    return NULL;
  }

  if (fseek(f, 0, SEEK_END))
  {
    error("failed to seek %s\n", path);
    return NULL;
  }

  long size = ftell(f);
  if (size < 0)
  {
    error("failed to get size: %s\n", path);
  }
  rewind(f);

  char *content = calloc(1, size + 1);

  size_t read = fread(content, 1, size, f);
  content[read] = 0;

  fclose(f);

  return content;
}

static FILE *output_file;

void open_output_file(char* path)
{
  output_file = fopen(path, "w");
  if (!output_file)
  {
    error("failed to open %s\n", path);
    exit(1);
  }
}

void write_output(char* fmt, ...)
{
  va_list ap;
  va_start(ap, fmt);
  vfprintf(output_file, fmt, ap);
}

void close_file()
{
  fclose(output_file);
}
