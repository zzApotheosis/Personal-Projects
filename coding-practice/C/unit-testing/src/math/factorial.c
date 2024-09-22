#include <stdlib.h>

#include "factorial.h"
#include "common-macros.h"

long int factorial(const long int a) {
        if (a < 0)
                die("cannot perform factorial with negative integers");
        if (a <= 1)
                return 1;
        return a * factorial(a - 1);
}
