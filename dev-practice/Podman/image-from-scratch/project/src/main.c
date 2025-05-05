#include <stdlib.h>
#include <stdio.h>
#include <stdint.h>

int main(int argc, char * argv[])
{
  for (size_t i = 0; i < argc; i++)
  {
    fprintf(stderr, "argv[%lu] = %s\n", i, argv[i]);
  }
}
