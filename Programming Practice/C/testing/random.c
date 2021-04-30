#include <stdlib.h>
#include <stdio.h>
#include <time.h>

int main(int argc, char** argv) {
    srand(time(0));
    for (int i = 0; i < 10; i++) {
        fprintf(stdout, "%d\n", rand() % 10);
    }
    return EXIT_SUCCESS;
}

