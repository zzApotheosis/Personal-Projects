#include <stdlib.h>
#include <stdio.h>
#include <unistd.h>
#include <time.h>

#include "foo.h"

struct Point {
  int x;
  int y;
  int z;
};

static void Point_init(Point * const point, const int x, const int y, const int z) {
  if (point == NULL)
    return;
  point->x = x;
  point->y = y;
  point->z = z;
}

Point * Point_new() {
  Point * self = (Point *) malloc(1 * sizeof(Point));
  Point_init(self, 0, 0, 0);
  return self;
}

void Point_destroy(Point * const self) {
  free(self);
}

Point * Point_set_x(Point * const self, const int x) {
  if (self == NULL)
    return NULL;
  self->x = x;
  return self;
}

Point * Point_set_y(Point * const self, const int y) {
  if (self == NULL)
    return NULL;
  self->y = y;
  return self;
}

Point * Point_set_z(Point * const self, const int z) {
  if (self == NULL)
    return NULL;
  self->z = z;
  return self;
}

int Point_get_x(Point * const self) {
  if (self == NULL)
    abort();
  return self->x;
}

int Point_get_y(Point * const self) {
  if (self == NULL)
    abort();
  return self->y;
}

int Point_get_z(Point * const self) {
  if (self == NULL)
    abort();
  return self->z;
}

void c_foo(const unsigned int x) {
  fprintf(stdout, "Hello from C!\n");
  const char * msg = NULL;
  if (x < 60) {
    msg = "No offense, but you suck.";
  } else if (x < 70) {
    msg = "Do better next time.";
  } else if (x < 80) {
    msg = "Not bad, but could be better.";
  } else if (x < 90) {
    msg = "Good job!";
  } else if (x < 100) {
    msg = "Outstanding!";
  } else if (x < 110) {
    msg = "Okay, now you're just showing off.";
  } else if (x == 110) {
    msg = "You really had to prove something, didn't you... freaking nerd.";
  }
  fprintf(stdout, "%u? %s\n", x, msg);
}
