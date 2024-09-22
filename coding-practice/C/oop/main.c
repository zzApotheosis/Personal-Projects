#include <stdlib.h>
#include <stdio.h>
#include <assert.h>
#include "main.h"
#include "point.h"

#define NUM_POINTS 5
#define INITIAL_VALUE 10

int main(int argc, char * argv[])
{
  /* Satisfy the compiler complaining about uNuSeD vaRiAblEs */
  (void) argc;
  (void) argv;

  /* Make some Point objects */
  Point * points = Point_new_vector(NUM_POINTS, 0, 0);
  Point * obj = NULL;
  int n = INITIAL_VALUE;
  for (unsigned int i = 0; i < NUM_POINTS; i++)
    {
      obj = Point_get_vector_index(points, i);
      Point_set_x(obj, n++);
      Point_set_y(obj, n++);
    }

  /* Demonstrate objects that can encapsulate data independently of each other */
  for (unsigned int i = 0; i < NUM_POINTS; i++)
    {
      obj = Point_get_vector_index(points, i);
      fprintf(stdout, "Point %u, x=%d, y=%d\n", i, Point_get_x(obj), Point_get_y(obj));
    }

  /* Clean up */
  Point_destroy(&points);

  assert(points == NULL);
  return EXIT_SUCCESS;
}

