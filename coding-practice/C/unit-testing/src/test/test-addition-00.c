#include <stdlib.h>
#include <assert.h>

#include "math.h"

int main(void) {
        assert(add(50, 49) == 99);
        assert(add(100, -100) == 0);
        assert(add(1, 1) == 2);

        return EXIT_SUCCESS;
}
