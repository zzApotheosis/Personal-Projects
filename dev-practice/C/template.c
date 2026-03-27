#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>

#include "template.h"

int
main(int argc, char * argv[])
{
  /* Define function variables preferably near the start of the function */
  const char * const msg = "Hello, world!";

  /* Do cool code stuff */
  for (int i = 0; i < argc; i++)
    fprintf(stdout, "argv[%d] = '%s'\n", i, argv[i]);
  fprintf(stdout, "%s\n", msg);

  /* Invoke function defined in header */
  const int snoop_dogg = 420;
  const int hhg2g = 42;
  fprintf(stdout, "%d + %d = %d\n", snoop_dogg, hhg2g, foo(snoop_dogg, hhg2g));
}

int
foo(const int a, const int b)
{
  return a + b;
}
