#include <stdlib.h>
#include <stdio.h>

int* intToHeap(int i) {
    int* ptr = (int*) malloc(sizeof(int));
    *ptr = i;
    return ptr;
}

float* floatToHeap(float f) {
    float* ptr = (float*) malloc(sizeof(float));
    *ptr = f;
    return ptr;
}

double* doubleToHeap(double d) {
    double* ptr = (double*) malloc(sizeof(double));
    *ptr = d;
    return ptr;
}

unsigned* unsignedToHeap(unsigned u) {
    unsigned* ptr = (unsigned*) malloc(sizeof(unsigned));
    *ptr = u;
    return ptr;
}

long* longToHeap(long l) {
    long* ptr = (long*) malloc(sizeof(long));
    *ptr = l;
    return ptr;
}

