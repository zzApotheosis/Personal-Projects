#include <stdlib.h>
#include <stdio.h>

#include "cool.h"

static int x = 0;

struct CoolStruct {
    int x;
    int y;
};

void cool_function(int i, char c, CoolStruct * const cs) {
    fprintf(stdout, "i = %d\n", i);
    fprintf(stdout, "c = %c\n", c);
    x = i;
    fprintf(stdout, "static x = %d\n", x);
    if (cs != NULL) {
        fprintf(stdout, "x = %d\n", cs->x);
        fprintf(stdout, "y = %d\n", cs->y);
    }
    fprintf(stdout, "\n");
}

void set_x(const int new_x) {
    fprintf(stdout, "Setting C static x = %u\n", new_x);
    x = new_x;
}

int get_x(void) {
    return x;
}
