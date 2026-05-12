#include <gmp-x86_64.h>
#include <stdlib.h>
#include <stdio.h>
#include <gmp.h>
#include <string.h>

#include "cool.h"

#define MSG_SIZE 1024

static int x = 0;
static CoolStruct * static_instance = NULL;

struct CoolStruct {
    int x;
    int y;
    char msg[MSG_SIZE];
};

CoolStruct * CoolStruct_new(void) {
    CoolStruct * const self = (CoolStruct * const) malloc(1 * sizeof(CoolStruct));
    if (self == NULL)
        abort();
    memset(self, 0, 1 * sizeof(CoolStruct));
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

void CoolStruct_get_msg(CoolStruct * const self, char * const buffer) {
    if (self == NULL)
        abort();
    if (buffer == NULL)
        abort();
    memcpy(buffer, self->msg, MSG_SIZE);
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

void CoolStruct_set_msg(CoolStruct * self, const char * const msg) {
    if (self == NULL)
        abort();
    size_t index = 0;
    while (index < MSG_SIZE) {
        if (msg[index] == '\0')
            break;
        self->msg[index] = msg[index];
        index++;
    }
    while (index < MSG_SIZE) {
        self->msg[index] = '\0';
        index++;
    }
}

CoolStruct * get_static_coolstruct(void) {
    return static_instance;
}

void set_static_coolstruct(CoolStruct * const instance) {
    static_instance = instance;
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
    mpz_clear(gmp_num);

    fprintf(stdout, "\n");
}

void set_x(const int new_x) {
    fprintf(stdout, "In C, setting static x = %u\n", new_x);
    x = new_x;
}

int get_x(void) {
    return x;
}

void print_bignum(const char * const s) {
    if (s == NULL)
        abort();
    mpz_t bignum;
    mpz_init(bignum);
    mpz_init_set_str(bignum, s, 10);
    gmp_printf("bignum = %Zd\n", bignum);
    mpz_clear(bignum);
}
