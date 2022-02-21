#include <stdlib.h>
#include <stdio.h>
#include <unistd.h>
#include <time.h>

int main(int argc, char** argv) {
    time_t t = 0;
    char buf[32];

    /* Set seed */
    srand(time(&t));

    /* Get 32 random bytes */
    for (int i = 0; i < sizeof(buf); i++) {
        buf[i] = rand() & 0xFF;
    }

    FILE* fh = fopen("out.dat", "w");
    fwrite(buf, 1, sizeof(buf), fh);
    fclose(fh);

    /* Done */
    return EXIT_SUCCESS;
}

