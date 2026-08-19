#include <poll.h>
#include <pthread.h>
#include <stdint.h>
#include <stdio.h>
#include <sys/eventfd.h>
#include <unistd.h>

int efd;

void *
worker_thread(void *arg)
{
  sleep(1);

  printf("[worker] notify main thread\n");

  uint64_t u = 1;
  write(efd, &u, sizeof(uint64_t));

  return NULL;
}

int
main()
{
  efd = eventfd(0, EFD_NONBLOCK | EFD_CLOEXEC);
  if (efd == -1)
  {
    fprintf(stderr, "error eventfd");
    return 1;
  }

  pthread_t th;
  pthread_create(&th, NULL, worker_thread, NULL);

  struct pollfd fd = {
      .fd = efd,
      .events = POLLIN,
  };

  printf("[main] start polling...\n");
  poll(&fd, 1, -1);

  if (fd.events & POLLIN)
  {
    uint64_t count;

    read(efd, &count, sizeof(uint64_t));
    printf("[main] recieved efd, value = %ld\n", count);
  }

  pthread_join(th, NULL);
  close(efd);
  return 0;
}