#ifndef FOO_H
#define FOO_H

typedef struct Point Point;
Point * Point_new(void);
void Point_destroy(Point * const);
Point * Point_set_x(Point * const, const int);
Point * Point_set_y(Point * const, const int);
Point * Point_set_z(Point * const, const int);
int Point_get_x(Point * const);
int Point_get_y(Point * const);
int Point_get_z(Point * const);

void c_foo(const unsigned int);

#endif
