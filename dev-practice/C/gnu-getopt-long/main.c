#include <stdlib.h>
#include <stdio.h>
#include <unistd.h>
#include <getopt.h>

/*
 * This is the example code found in the getopt_long man page with minor tweaks.
 * The GNU Project/FSF did a great job with this example code.
 */

void parse_args(int argc, char * argv[]) {
    int c = 0;
    int digit_optind = 0;
    int this_option_optind = optind ? optind : 1;
    int option_index = 0;
    static const struct option long_options[] = {
        {"add",     required_argument, 0,  0 },
        {"append",  no_argument,       0,  0 },
        {"delete",  required_argument, 0,  0 },
        {"verbose", no_argument,       0,  0 },
        {"create",  required_argument, 0, 'c'},
        {"file",    required_argument, 0,  0 },
        {0,         0,                 0,  0 }
    };

    while (1) {
        c = getopt_long(argc, argv, "abc:d:012", long_options, &option_index);
        if (c == -1)
            break;

        switch (c) {
            case 0:
                fprintf(stdout, "option %s", long_options[option_index].name);
                if (optarg)
                    fprintf(stdout, " with arg %s", optarg);
                fprintf(stdout, "\n");
                break;
            case '0':
            case '1':
            case '2':
                if (digit_optind != 0 && digit_optind != this_option_optind)
                    fprintf(stdout, "digits occur in two different argv-elements.\n");
                digit_optind = this_option_optind;
                fprintf(stdout, "option %c\n", c);
                break;
            case 'a':
                fprintf(stdout, "option a\n");
                break;
            case 'b':
                fprintf(stdout, "option b\n");
                break;
            case 'c':
                fprintf(stdout, "option c with value '%s'\n", optarg);
                break;
            case 'd':
                fprintf(stdout, "option d with value '%s'\n", optarg);
                break;
            case '?':
                // This runs when an unknown option is supplied to the program
                break;
            default:
                fprintf(stdout, "?? getopt returned character code 0%o ??\n", c);
                break;
        }
    }

    if (optind < argc) {
        fprintf(stdout, "non-option ARGV-elements: ");
        while (optind < argc)
            fprintf(stdout, "%s ", argv[optind++]);
        fprintf(stdout, "\n");
    }
}

int main(int argc, char * argv[]) {
    parse_args(argc, argv);
    return(EXIT_SUCCESS);
}
