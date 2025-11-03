#include <stdlib.h>
#include <stdio.h>
#include <unistd.h>

#include "foo.h"

void library_function(const int x)
{
  FILE * fh = fopen("my-file.txt", "w+");
  if (fh == NULL)
    abort();
  fprintf(fh, "Library function received integer: %d\n", x);
  fclose(fh);
}
