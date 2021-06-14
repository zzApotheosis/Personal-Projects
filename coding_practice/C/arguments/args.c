#include <stdlib.h>
#include <stdio.h>
#include <getopt.h>
#include "args.h"

static struct option long_options[] = {
    {"test",    no_argument,    0,  'a' }
};

static struct ParsedArgs {
    
};

void parseArgs(int c, char** v) {
    int opt = 0;
    int long_index = 0;
    int test = 0;

    while ((opt = getopt_long(c, v, "a", long_options, &long_index)) != -1) {
        switch (opt) {
            case 'a':
                test = 1;
                break;
            default:
                break;
        }
    }
    
    fprintf(stdout, "test = %d\n", test);
}

