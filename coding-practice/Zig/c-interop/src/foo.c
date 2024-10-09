#include <stdlib.h>
#include <stdio.h>
#include <unistd.h>
#include <time.h>

void c_foo(const unsigned int x) {
  fprintf(stdout, "Hello from C!\n");
  const char * msg = NULL;
  if (x < 10) {
    msg = "A pretty small number :)";
  } else if (x < 100) {
    msg = "A nice-sized number!";
  } else {
    msg = "You really had to prove something, didn't you...";
  }
  fprintf(stdout, "%u? %s\n", x, msg);
}
