#include "exponent.h"

int power(const int a, const int b) {
        int n = a;
        for (int i = 1; i < b; i++) {
                n *= a;
        }
        return n;
}
