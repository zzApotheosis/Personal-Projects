#include <stdlib.h>
#include <assert.h>
#include <limits.h>

#include "math.h"

int main(void) {
        assert(multi(INT_MAX, 2) > 0);

        return EXIT_SUCCESS;
}
