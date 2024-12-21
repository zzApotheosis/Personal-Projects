#include "foo.h"

int add(const int a, const int b) {
  return a + b;
}

int bad_add(const int a, const int b) {
  // return a + b; // Uncomment this to "fix" the implementation
  return a - 1 + b;
}
