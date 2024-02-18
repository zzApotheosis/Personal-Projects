#include <stdlib.h>
#include <stdio.h>


float add(int a, int b) {
    return(1.0f * a + b);
}

float sub(int a, int b) {
    return(1.0f * a - b);
}

float multi(int a, int b) {
    return(1.0f * a * b);
}

float divide(int a, int b) {
    return(1.0f * a / b);
}

int main(int argc, char** argv) {
    float (* math_functions[])(int, int) = {
        add,
        sub,
        multi,
        divide
    };
    unsigned char operator[] = {'+', '-', '*', '/'};
    int num1 = 4, num2 = 17;

    fprintf(stdout, "num1 = %d\n", num1);
    fprintf(stdout, "num2 = %d\n", num2);
    for (int i = 0; i < 4; i++) {
        fprintf(stdout, "%d %c %d = %f\n", num1, operator[i], num2, math_functions[i](num1, num2));
    }
    
    return(EXIT_SUCCESS);
}

