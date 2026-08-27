#include <stdio.h>

// See https://en.wikipedia.org/wiki/Hygienic_macro

#define INCI(i) { int a = 0; ++i; }
int main(void)
{
  int a = 4, b = 8;
  INCI(a);
  INCI(b);
  fprintf(stderr, "a is now %d, b is now %d\n", a, b);
  // a is now 4, b is now 9
}
