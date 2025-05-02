#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>

int main(int argc, char * argv[])
{
  fprintf(stderr, "Hello, Meson and Podman!\n");

  for (size_t i = 0; i < argc; i++)
  {
    fprintf(stderr, "argv[%u] = %s\n", i, argv[i]);
  }
}
