#include <stdlib.h>
#include <assert.h>

#include "math.h"

int main(void) {
        int previous = 0;
        int current = 0;
        for (int i = 1; i <= 100; i++) {
                previous = current;
                current = factorial(i);
                assert(previous < current);
        }

        return EXIT_SUCCESS;
}
