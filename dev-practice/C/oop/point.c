#include <stdlib.h>
#include <stdio.h>
#include "point.h"

/* Class structure */
struct Point {
  int x;
  int y;
};

/* Constructor (without allocation) */
static void Point_init(Point * const self, const int x, const int y)
{
  self->x = x;
  self->y = y;
}

/* Allocation + initialization (equivalent to "new Point(x, y)") */
Point * Point_new(const int x, const int y)
{
  Point * self = (Point *) malloc(sizeof(Point));
  if (self == NULL)
    abort();
  Point_init(self, x, y);
  return self;
}

Point * Point_new_vector(const unsigned int len, const int x, const int y)
{
  Point * vector = (Point *) malloc(len * sizeof(Point));
  if (vector == NULL)
    abort();
  for (unsigned int i = 0; i < len; i++)
    {
      Point_init(&vector[i], x, y);
    }
  return vector;
}

Point * Point_get_vector_index(Point * const vector, const unsigned int index)
{
  if (vector == NULL)
    abort();
  return &vector[index];
}

/* Destructor (without deallocation) */
static void Point_reset(Point * const self)
{
  /* This is where "destructor" code would go */
}

/* Destructor + deallocation (equivalent to "delete point") */
void Point_destroy(Point ** const self)
{
  if (self == NULL)
    abort();
  
  Point_reset(*self);
  free(*self);
  *self = NULL;
}

/* BEGIN GETTERS */
int Point_get_x(const Point * const self)
{
  return self->x;
}
int Point_get_y(const Point * const self)
{
  return self->y;
}
/* END GETTERS */

/* BEGIN SETTERS */
void Point_set_x(Point * const self, const int x)
{
  self->x = x;
}
void Point_set_y(Point * const self, const int y)
{
  self->y = y;
}
/* END SETTERS */

