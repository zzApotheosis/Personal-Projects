#include <math.h>
#include <stdio.h>

typedef struct {
  int x;
  int y;
} CPoint;

void c_greet(const char *const name) { printf("Hello from C, %s!\n", name); }

double c_calc_distance(CPoint *a, CPoint *b) {
  const double distance = sqrt(pow(a->x - b->x, 2.0) + pow(a->y - b->y, 2.0));

  // Let's maliciously modify the struct from C!
  // The Rust compiler won't know about any of this
  a->x += 1;

  return distance;
}
