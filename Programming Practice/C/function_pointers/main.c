#include <stdlib.h>
#include <stdio.h>

struct point {
    int x;
    int y;
    void (*print)(const struct point*);
};

void print_x(const struct point* p) {
    fprintf(stdout, "x=%d\n", p->x);
}

void print_y(const struct point* p) {
    fprintf(stdout, "y=%d\n", p->y);
}

int main(int argc, char** argv) {
    struct point p1 = {2, 4, print_x};
    struct point p2 = {7, 1, print_y};

    p1.print(&p1);
    p2.print(&p2);

    return EXIT_SUCCESS;
}

