#ifndef SRC_IO_H
#define SRC_IO_H

char *read_file(char *path);

void open_output_file(char *path);
void write_output(char *fmt, ...);
void close_file();

#endif //SRC_IO_H