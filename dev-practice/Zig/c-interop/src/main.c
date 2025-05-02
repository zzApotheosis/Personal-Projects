#include <stdlib.h>
#include <stdio.h>
#include <unistd.h>
#include <time.h>

void zig_foo(const unsigned int);

int main(int argc, char * argv[]) {
  fprintf(stdout, "This the main function in C!\n");
  srand(time(NULL));
  const unsigned int x = rand() % 111;
  zig_foo(x);
  return EXIT_SUCCESS;
}
