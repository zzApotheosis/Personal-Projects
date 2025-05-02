#include "foo.h"

int add(const int a, const int b) {
  return a + b;
}

int bad_add(const int a, const int b) {
  // return a + b; // Uncomment this to "fix" the implementation
  return a - 1 + b;
}


// Unit tests
void test_add(int * const result) {
  *result = 1; // Assume fail
  int a = 10;
  int b = 5;
  if (add(a, b) == 15)
    *result = 0;
}

void test_bad_add(int * const result) {
  *result = 1; // Assume fail
  int a = 10;
  int b = 5;
  if (bad_add(a, b) == 15)
    *result = 0;
}

