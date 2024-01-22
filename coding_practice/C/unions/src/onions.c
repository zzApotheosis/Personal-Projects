#include <stdlib.h>
#include <stdio.h>
#include <unistd.h>

#include "unions.h"

#define PROJECT_NAME "unions"

int main(int argc, char * argv[]) {
        if (argc != 1) {
                printf("%s takes no arguments.\n", argv[0]);
                return EXIT_FAILURE;;
        }

        fprintf(stdout, "This is project %s.\n", PROJECT_NAME);

        ogres_have_layers(67, "cool string");

        return EXIT_SUCCESS;
}
