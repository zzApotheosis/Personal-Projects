#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <sys/epoll.h>

#include "epoll-app.h"

#define MAX_EVENTS 10

#define BADSTATE(m, l)                                  \
  do {                                                  \
    fprintf(stderr, "[%s:%d] ", __FILE__, __LINE__);    \
    perror(m);                                          \
    goto l;                                             \
  } while (0)

int
do_epoll_app(const int argc, char * argv[])
{
  (void) argc;
  (void) argv;

  int fd = 0;
  FILE * stream = NULL;
  struct epoll_event ev, events[MAX_EVENTS];

  if (argc < 2)
    {
      fprintf(stderr, "Usage: %s <file>\n", argv[0]);
      return EXIT_FAILURE;
    }

  stream = fopen(argv[1], "r");
  if (stream == NULL)
    BADSTATE("fopen", bad_fopen);

  fd = epoll_create1(0);
  if (fd < 0)
    BADSTATE ("epoll_create1", bad_epoll_create1);

  ev.events = EPOLLIN;
  ev.data.fd = fileno(stream);
  if (epoll_ctl(fd, EPOLL_CTL_ADD, ev.data.fd, &ev) < 0)
    BADSTATE("epoll_ctl", bad_epoll_ctl);

  int nfds = 0;
  for (;;)
    {
      nfds = epoll_wait(fd, events, MAX_EVENTS, -1);
      if (nfds < 0)
        BADSTATE("epoll_wait", bad_epoll_wait);

      for (int i = 0; i < nfds; i++)
        {
          char buffer[4096] = {0};
          fgets(buffer, sizeof(buffer), stream);
          fwrite(buffer, 1, sizeof(buffer), stderr);
        }
    }

  close(fd);
  fclose(stream);

  return EXIT_SUCCESS;

 bad_epoll_wait:
 bad_epoll_ctl:
  close(fd);
  
 bad_epoll_create1:
  fclose(stream);

 bad_fopen:
  return EXIT_FAILURE;
}

