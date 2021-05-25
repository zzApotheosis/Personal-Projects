#include <stdlib.h>
#include <stdio.h>

int main(int argc, char** argv) {
    int a = 10;
LOOP:
    do {
        if (a == 15) {
            a++;
            goto LOOP;
        }

        fprintf(stdout, "Value of a: %d\n", a);
        a++;
    } while (a < 20);

    return EXIT_SUCCESS;
}

