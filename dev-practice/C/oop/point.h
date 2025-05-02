#ifndef POINT_H
#define POINT_H

/* Class structure */
typedef struct Point Point;
Point * Point_new(const int, const int);
Point * Point_new_vector(const unsigned int, const int, const int);
Point * Point_get_vector_index(Point * const, const unsigned int);
void Point_destroy(Point ** const);

/* Setters */
int Point_get_x(const Point * const);
int Point_get_y(const Point * const);

/* Getters */
void Point_set_x(Point * const, const int);
void Point_set_y(Point * const, const int);

#endif
