#include <stdlib.h>
#include <stdio.h>
#include <unistd.h>
#include <string.h>

int main(int argc, char * argv[]) {
    char buffer[16];
    for (int i = 0; i <= 100; i++) {
        memset(buffer, 0, sizeof(buffer));
        if (i % 3 == 0) {
            snprintf(buffer, sizeof(buffer), "fizz");
        }
        if (i % 5 == 0) {
            snprintf(buffer, sizeof(buffer), "buzz");
        }
        if (buffer[0] == '\0') {
            snprintf(buffer, sizeof(buffer), "%d", i);
        }
        fprintf(stdout, "%s\n", buffer);
    }
    return EXIT_SUCCESS;
}
