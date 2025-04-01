#include <stdio.h>
#include <string.h>
#include <errno.h>

#include "foo.h"

int add(const int a, const int b)
{
  return a + b;
}

void out(const char * const data, const unsigned long len)
{
  unsigned long written = 0;
  written = fwrite(data, 1, len, stdout);
  //fprintf(stderr, "%lu/%lu\n", len, written);
  if (written != len)
    {
      fprintf(stderr, "%s\n", strerror(errno));
    }
}
