#include <stdlib.h>
#include <stdio.h>
#include "point.h"

/* Class structure */
struct Point {
    int x;
    int y;
};

/* Constructor (without allocation) */
static void Point_init(struct Point* self, int x, int y) {
    self->x = x;
    self->y = y;
}

/* Allocation + initialization (equivalent to "new Point(x, y)") */
struct Point* Point_new(int x, int y) {
    struct Point* new = (struct Point*) malloc(sizeof(struct Point));
    Point_init(new, x, y);
    return new;
}

/* Destructor (without deallocation) */
static void Point_reset(struct Point* self) {
}

/* Destructor + deallocation (equivalent to "delete point") */
void Point_destroy(struct Point* self) {
    if (self) {
        Point_reset(self);
        free(self);
    }
}

/* BEGIN GETTERS */
int Point_getX(struct Point* self) {
    return self->x;
}
int Point_getY(struct Point* self) {
    return self->y;
}
/* END GETTERS */

/* BEGIN SETTERS */
void Point_setX(struct Point* self, int x) {
    self->x = x;
}
void Point_setY(struct Point* self, int y) {
    self->y = y;
}
/* END SETTERS */

