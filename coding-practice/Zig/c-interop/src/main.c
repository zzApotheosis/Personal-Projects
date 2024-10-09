#include <stdlib.h>
#include <stdio.h>
#include <unistd.h>

void zig_foo(const unsigned int);

int main(int argc, char * argv[]) {
  const unsigned int x = 25;
  zig_foo(x);
  return EXIT_SUCCESS;
}
