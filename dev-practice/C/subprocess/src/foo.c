#include <stdlib.h>
#include <stdio.h>
#include <unistd.h>
#include <sys/wait.h>

#include "foo.h"

void library_function(const int x)
{
  fprintf(stderr, "Library function received integer: %d\n", x);
}

void
app (void)
{
  int pipedes[2] = {0};
  pipe(pipedes);

  int cpid = fork();
  if (cpid == 0)
    {
      sleep(5);
      exit(0);
    }

  close(pipedes[0]);
  close(pipedes[1]);

  waitpid(cpid, NULL, 0);
}
