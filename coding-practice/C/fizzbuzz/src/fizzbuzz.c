#include <stdlib.h>
#include <stdio.h>
#include <unistd.h>
#include <string.h>

int main(int argc, char * argv[]) {
    char buffer[16];
    for (int i = 1; i <= 100; i++) {
        memset(buffer, 0, sizeof(buffer));
        if (i % 3 == 0) {
            strncat(buffer, "fizz", sizeof(buffer) - 1);
        }
        if (i % 5 == 0) {
            strncat(buffer, "buzz", sizeof(buffer) - 1);
        }
        if (buffer[0] == '\0') {
            snprintf(buffer, sizeof(buffer), "%d", i);
        }
        fprintf(stdout, "%s\n", buffer);
    }
    return EXIT_SUCCESS;
}
