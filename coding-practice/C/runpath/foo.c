#include <stdio.h>

#include "foo.h"

void bar(void)
{
  fprintf(stderr, "The dynamic loader found me!\n");
}
