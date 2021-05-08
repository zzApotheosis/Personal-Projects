#include <stdlib.h>
#include <stdio.h>
#include "client.h"

int main(int argc, char** argv) {
    logger_set_socket("\0journaldlistener");
    return EXIT_SUCCESS;
}

