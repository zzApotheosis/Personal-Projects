#include <unistd.h>
#include <stdio.h>
#include <errno.h>
#include <string.h>

/*
 * Try modifying these functions to see how they affect the Lisp implementation :)
 */

int my_add(const int a, const int b) {
  return a + b;
}

static int data = 0;

void set_data(const int a) {
  data = a;
}

int get_data(void) {
  return data;
}

void _execvp(void)
{
  char * cmd = "sbcl";
  char * args[] = {cmd, "--version", NULL};
  int result = execvp(cmd, args);
  if (result != 0)
    {
      fprintf(stderr, "Error occurred during execvp(): %s\n", strerror(errno));
    }
}
