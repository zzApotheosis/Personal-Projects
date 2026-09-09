#include <stdlib.h>
#include <stdio.h>
#include <unistd.h>

#include "foo.h"

int main(int argc, char * argv[])
{
  (void) argc;
  (void) argv;

  fprintf(stderr, "Hello world!\n");
  foo();
}
