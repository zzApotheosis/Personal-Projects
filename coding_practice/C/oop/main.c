#include <stdlib.h>
#include <stdio.h>
#include "main.h"

int main(int argc, char** argv) {
    struct Point* p1 = Point_new(2, 3);
    struct Point* p2 = Point_new(4, 5);
    struct Point* p3 = Point_new(-4, 20);
    
    fprintf(stdout, "Point 1, x=%d, y=%d\n", Point_getX(p1), Point_getY(p1));
    fprintf(stdout, "Point 2, x=%d, y=%d\n", Point_getX(p2), Point_getY(p2));
    fprintf(stdout, "Point 3, x=%d, y=%d\n", Point_getX(p3), Point_getY(p3));
    
    Point_destroy(p1);
    Point_destroy(p2);
    Point_destroy(p3);
    
    return EXIT_SUCCESS;
}

