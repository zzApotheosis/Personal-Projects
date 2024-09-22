#include <stdlib.h>
#include <stdio.h>
#include <unistd.h>
#include <string.h>

#define POINT_NAME_SIZE 32

struct point {
    char name[POINT_NAME_SIZE];
    int x;
    int y;
};
typedef struct point * point;

point point_new() {
    point instance = (point) malloc(1 * sizeof(struct point));
    memset(instance, 0, sizeof(struct point));
    return instance;
}

point point_new_with(int new_x, int new_y, const char new_name[]) {
    point instance = (point) malloc(1 * sizeof(struct point));
    instance->x = new_x;
    instance->y = new_y;
    strncpy(instance->name, new_name, POINT_NAME_SIZE);
    return instance;
}

void point_free(point instance) {
    if (instance == NULL)
        return;
    free(instance);
}

void point_print(const point self) {
    if (self == NULL)
        return;
    fprintf(stdout, "Point %s: {%d, %d}\n", self->name, self->x, self->y);
}

void point_print_x(const point self) {
    if (self == NULL)
        return;
    fprintf(stdout, "Point %s: x=%d\n", self->name, self->x);
}

void point_print_y(const point self) {
    if (self == NULL)
        return;
    fprintf(stdout, "Point %s: y=%d\n", self->name, self->y);
}

void point_print_slope(const point self, const point other) {
    if (self == NULL)
        return;
    if (other == NULL)
        return;
    float slope = ((float) self->y - other->y) / (self->x - other->x);
    fprintf(stdout, "Slope between [%s, %s]: %f\n", self->name, other->name, slope);
}

void point_print_area(const point self, const point other) {
    // Assuming the two points define a rectangular area
    if (self == NULL)
        return;
    if (other == NULL)
        return;
    int area = (self->x - other->x) * (self->y - other->y);
    if (area < 0)
        area *= -1;
    fprintf(stdout, "Area of rectangle defined by corner points [%s, %s]: %d\n", self->name, other->name, area);
}

int main(int argc, char * argv[]) {
    point p1 = point_new_with(2, -4, "p1");
    point p2 = point_new_with(-8, -1, "p2");
    void (* func_ptr)(const point);

    func_ptr = point_print;
    func_ptr(p1);
    func_ptr(p2);
    func_ptr = point_print_x;
    func_ptr(p1);
    func_ptr(p2);
    func_ptr = point_print_y;
    func_ptr(p1);
    func_ptr(p2);

    point_print_slope(p1, p2);
    point_print_area(p1, p2);

    point_free(p1);
    point_free(p2);

    return EXIT_SUCCESS;
}

