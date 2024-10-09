#include <stdlib.h>
#include <unistd.h>
#include <time.h>

void zig_foo(const unsigned int);

int main(int argc, char * argv[]) {
  srand(time(NULL));
  const unsigned int x = rand() % 200;
  zig_foo(x);
  return EXIT_SUCCESS;
}
