#include <poll.h>
#include <stdio.h>
#include <unistd.h>

int
main()
{
  struct pollfd fd = {
      .fd = STDIN_FILENO,
      .events = POLLIN,
  };

  int n = poll(&fd, 1, 3000);

  if (n < 0)
  {
    fprintf(stderr, "poll failed\n");
    return 1;
  }
  else if (n == 0)
  {
    fprintf(stdout, "time out\n");
    return 1;
  }
  else 
  {
    if (fd.revents & (POLLERR|POLLHUP|POLLNVAL))
    {
      fprintf(stderr, "recieve error\n");
      return 1;
    }
    if (fd.revents & POLLIN)
    {
      fprintf(stdout, "input from STDIN\n");
    }
  }

  return 0;
}