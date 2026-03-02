#include <stdlib.h>
#include <stdio.h>
#include <unistd.h>

int app(int argc, char * argv[])
{
  for (int i = 0; i < argc; i++)
    fprintf(stderr, " [%d]: %s\n", i, argv[i]);
  return EXIT_SUCCESS;
}
