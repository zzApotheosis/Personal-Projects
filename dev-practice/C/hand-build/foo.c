#include <stdio.h>

#include "foo.h"

int square(const int x)
{
  return x * x;
}

void greet(const char * const name)
{
  // UNSAFE CODE! Can you explain why?
  fprintf(stderr, "Hello, %s!\n", name);
}
