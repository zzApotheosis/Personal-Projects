/*
 * This is meant to be a "function pointer function", hence the acronym fpf.c
 */

#include <stdlib.h>
#include <stdio.h>

float add(int a, int b) {
    return(1.0f * a + b);
}

float (* func(int val))(int, int) {
    fprintf(stdout, "val = %d\n", val);
    float (* f)(int, int) = &add;
    //float (* f)(int, int) = add; // Also valid
    return(f);
}

int main(int argc, char** argv) {
    int num1 = 4, num2 = 17;
    fprintf(stdout, "%d + %d = %f", num1, num2, (func(10))(num1, num2));

    return(EXIT_SUCCESS);
}

