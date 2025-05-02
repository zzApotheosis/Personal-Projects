#include <stdlib.h>
#include <stdio.h>
#include <unistd.h>

/*
 * Certain lines have been commented out for demonstration purposes.
 * Try to uncomment some lines and see what your compiler says about them.
 */

static char message[] = "The quick brown fox jumps over the lazy dog.";
static char other_message[] = "Hello world!";

int main(int argc, char * argv[]) {
    const char * ccp = message;
    ccp = other_message;                 // const <type> * variables can point elsewhere
    //ccp[0] = 'A';                      // const <type> * variables cannot change the pointed data
    fprintf(stdout, "%s\n", ccp);

    char * const cpc = message;
    //cpc = other_message;               // <type> * const variables cannot point elsewhere
    cpc[0] = 'A';                        // <type> * const variables can change the pointed data
    fprintf(stdout, "%s\n", cpc);
    cpc[0] = 'T';

    const char * const ccpc = message;
    //ccpc = other_message;              // const <type> * const variables cannot point elsewhere
    //ccpc[0] = 'A';                     // const <type> * const variables cannot change the pointed data
    fprintf(stdout, "%s\n", ccpc);

    return(EXIT_SUCCESS);
}
