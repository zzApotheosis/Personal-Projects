#include <stdlib.h>
// #include <stdio.h>
#include <unistd.h>

#include <common-macros.h>

#include "functionlist.h"
#include "funcnode.h"
#include "gpgme-example.h"


int main(int argc, char * argv[]) {
    (void) argc;
    (void) argv;

    gpgme_example_init();

    functionlist * flist = NULL;
    funcnode * fnode = NULL;
    int result = 0;

    /*
     * Define functionlist to run
     */
    flist = functionlist_new();
    functionlist_push(flist, listkeys);
    functionlist_push(flist, symm_encrypt);
    functionlist_push(flist, asym_encrypt);
    functionlist_push(flist, sign);
    functionlist_push(flist, verify);
    
    /*
     * Iterate functions
     */
    fnode = functionlist_pop(flist);
    for (;fnode != NULL;) {
        result = funcnode_get_f(fnode)();
        if (result)
            fprintf(stderr, "%s:%d fnode function failed. (%p)\n", __FILE__, __LINE__, (void *) fnode);
        funcnode_destroy(&fnode);
        fnode = functionlist_pop(flist);
    }

    functionlist_destroy(&flist);

    gpgme_example_deinit();

    return EXIT_SUCCESS;
}
