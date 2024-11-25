#include <stdlib.h>
#include <stdio.h>
#include <gmp.h>

#include "cool.h"

static int x = 0;

struct CoolStruct {
    int x;
    int y;
};

CoolStruct * CoolStruct_new(void) {
    CoolStruct * const self = (CoolStruct * const) malloc(1 * sizeof(CoolStruct));
    if (self == NULL)
        abort();
    self->x = 0;
    self->y = 0;
    return self;
}

void CoolStruct_destroy(CoolStruct ** const self) {
    if (self == NULL)
        abort();
    if (*self == NULL)
        abort();
    free(*self);
    *self = NULL;
}

int CoolStruct_get_x(CoolStruct * const self) {
    if (self == NULL)
        abort();
    return self->x;
}

int CoolStruct_get_y(CoolStruct * const self) {
    if (self == NULL)
        abort();
    return self->y;
}

void CoolStruct_set_x(CoolStruct * self, const int x) {
    if (self == NULL)
        abort();
    self->x = x;
}

void CoolStruct_set_y(CoolStruct * self, const int y) {
    if (self == NULL)
        abort();
    self->y = y;
}

void cool_function(int i, char c, CoolStruct * const cs) {
    fprintf(stdout, "i = %d\n", i);
    fprintf(stdout, "c = %c\n", c);
    x = i;
    fprintf(stdout, "static x = %d\n", x);
    if (cs != NULL) {
        fprintf(stdout, "x = %d\n", cs->x);
        fprintf(stdout, "y = %d\n", cs->y);
    }

    mpz_t gmp_num;
    mpz_init(gmp_num);
    gmp_printf("gmp_num = %Zd\n", gmp_num);

    fprintf(stdout, "\n");
}

void set_x(const int new_x) {
    fprintf(stdout, "In C, setting static x = %u\n", new_x);
    x = new_x;
}

int get_x(void) {
    return x;
}
