/*
 * This is a sample program to show the usage of the restrict
 * keyword in C. This keyword does not exist in C++.
 *
 * The restrict keyword was introduced in the C99 standard. It
 * provides no new functionality or logic for use in code. The
 * restrict keyword is intended to be used when the programmer
 * wishes to inform the compiler that the pointer variable
 * modified by the restrict keyword exclusively points to an
 * object. In other words, the programmer should inform the
 * compiler that the restricted pointer is the ONLY pointer
 * that points to that object.
 *
 * It is undefined behavior if the programmer fails to adhere
 * to this rule.
 */
#include <stdlib.h>
#include <stdio.h>
#include <unistd.h>

void use(int * a, int * b, int * restrict c) {
    *a += *c;

    /*
     * Since the c pointer is restricted, the compiler will
     * not reload the value at address c in the assembly code.
     * Therefore, the generated assembly code is more
     * optimized.
     */
    *b += *c;
}

int main(int argc, char * argv[]) {
    int a = 50, b = 60, c = 70;
    use(&a, &b, &c);
    fprintf(stdout, "%d %d %d\n", a, b, c);
    return(EXIT_SUCCESS);
}
