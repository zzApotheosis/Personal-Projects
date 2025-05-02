#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include "include/util.h"

int* util_int_to_heap(int i) {
    int* ptr = (int*) malloc(sizeof(int));
    *ptr = i;
    return ptr;
}

float* util_float_to_heap(float f) {
    float* ptr = (float*) malloc(sizeof(float));
    *ptr = f;
    return ptr;
}

double* util_double_to_heap(double d) {
    double* ptr = (double*) malloc(sizeof(double));
    *ptr = d;
    return ptr;
}

unsigned* util_unsigned_to_heap(unsigned u) {
    unsigned* ptr = (unsigned*) malloc(sizeof(unsigned));
    *ptr = u;
    return ptr;
}

long* util_long_to_heap(long l) {
    long* ptr = (long*) malloc(sizeof(long));
    *ptr = l;
    return ptr;
}

char* util_char_to_heap(char c) {
    char* ptr = (char*) malloc(sizeof(char));
    *ptr = c;
    return ptr;
}

char* util_str_to_heap(char* s) {
    char* ptr = (char*) malloc(util_strlen(s) * sizeof(char));
    strcpy(ptr, s);
    return ptr;
}

