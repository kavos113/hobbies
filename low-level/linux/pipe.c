#include <stdio.h>
#include <unistd.h>

int
main()
{
  int pipe_fd[2];

  if (pipe(pipe_fd) < 0)
  {
    fprintf(stderr, "pipe create error");
    return 1;
  }

  pid_t child = fork();
  if (child < 0)
  {
    fprintf(stderr, "fork error");
    return 1;
  }
  else if (child == 0)
  {
    close(pipe_fd[0]);

    write(pipe_fd[1], "hello from child\n", 17);
    return 0;
  }

  close(pipe_fd[1]);
  
  char buf[256];
  ssize_t n = read(pipe_fd[0], buf, 256);
  if (n < 0)
  {
    fprintf(stderr, "read error");
    return 1;
  }

  write(STDOUT_FILENO, buf, n);
  return 0;
}