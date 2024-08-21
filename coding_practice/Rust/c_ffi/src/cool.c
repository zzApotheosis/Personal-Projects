#include <stdlib.h>
#include <stdio.h>

#include "cool.h"

struct CoolStruct {
    int x;
    int y;
};

void cool_function(int i, char c, CoolStruct * cs) {
    fprintf(stdout, "i = %d\n", i);
    fprintf(stdout, "c = %c\n", c);
    if (cs != NULL) {
        fprintf(stdout, "x = %d\n", cs->x);
        fprintf(stdout, "y = %d\n", cs->y);
    }
}
