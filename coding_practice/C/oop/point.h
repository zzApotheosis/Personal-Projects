#ifndef POINT_H
#define POINT_H

/* Class structure */
struct Point;
struct Point* Point_new(int, int);
void Point_destroy(struct Point*);

/* Setters */
int Point_getX(struct Point*);
int Point_getY(struct Point*);

/* Getters */
void Point_setX(struct Point*, int);
void Point_setY(struct Point*, int);

#endif
