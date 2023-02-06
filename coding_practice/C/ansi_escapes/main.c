#include <stdlib.h>
#include <stdio.h>

int main(int argc, char * argv[]) {
    int i, j, n;
    for (i = 0; i < 11; i++) {
        for (j = 0; j < 10; j++) {
            n = 10 * i + j;
            if (n > 108) break;
            fprintf(stdout, "\033[%dm%3d\033[m", n, n);
        }
        fprintf(stdout, "\n");
    }
    return(EXIT_SUCCESS);
}
