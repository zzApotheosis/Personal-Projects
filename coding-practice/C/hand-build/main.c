#include <stdio.h>
#include "foo.h"

int main(void)
{
  const int num = 5;
  const int squared = square(num);
  fprintf(stderr, "%d^2 = %d\n", num, squared);
  greet("Manual Build");
}
