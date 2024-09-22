#include <stdlib.h>
#include <assert.h>
#include <limits.h>

#include "math.h"

int main(void) {
        assert(add(INT_MAX, 1) == INT_MIN);

        return EXIT_SUCCESS;
}
