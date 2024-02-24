#include <stdlib.h>
#include <assert.h>

#include "math.h"

int main(void) {
        assert(divide(10, 5) == 2);
        assert(divide(5, 10) == 0);

        return EXIT_SUCCESS;
}
