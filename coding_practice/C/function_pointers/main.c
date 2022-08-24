#include <stdlib.h>
#include <stdio.h>

struct point {
    int x;
    int y;
};

void print_point(const struct point * p, const unsigned char name[]) {
    fprintf(stdout, "Point %s: {%d, %d}\n", name, p->x, p->y);
}

void print_x(const struct point * p, const unsigned char name[]) {
    fprintf(stdout, "Point %s: x=%d\n", name, p->x);
}

void print_y(const struct point * p, const unsigned char name[]) {
    fprintf(stdout, "Point %s: y=%d\n", name, p->y);
}

int main(int argc, char * argv[]) {
    struct point p1 = {2, 4};
    struct point p2 = {7, 1};
    void (* func_ptr)(const struct point *, const unsigned char *);

    func_ptr = print_point;
    func_ptr(&p1, "p1");
    func_ptr(&p2, "p2");
    func_ptr = print_x;
    func_ptr(&p1, "p1");
    func_ptr(&p2, "p2");
    func_ptr = print_y;
    func_ptr(&p1, "p1");
    func_ptr(&p2, "p2");

    return EXIT_SUCCESS;
}

