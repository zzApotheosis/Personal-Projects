#include <stdlib.h>
#include <assert.h>
#include <limits.h>

#include "math.h"

int main(void) {
        assert(sub(INT_MIN, 1) < 0);

        return EXIT_SUCCESS;
}
