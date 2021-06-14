#include <stdio.h>
#include <stdlib.h>
#include "sieve.h"

unsigned char* satkins(unsigned long int limit) {
    // Define method variables
    unsigned long int wlimit;
    unsigned long int i, j, x, y, z;
    unsigned char* sieb;
    
    // 
    sieb = (unsigned char*) calloc(limit, sizeof(unsigned char));
    
    // Check for low-value cases
    if (limit == 2) {
        sieb[2] = 1;
        return sieb;
    }
    if (limit == 3) {
        sieb[2] = 1;
        sieb[3] = 1;
        return sieb;
    }
    if (limit == 5) {
        sieb[2] = 1;
        sieb[3] = 1;
        sieb[5] = 1;
        return sieb;
    }

    // 
    wlimit = sqrt(limit);

    for (x = 1; x <= wlimit; x++) {
        for (y = 1; y <= wlimit; y++) {
            z = 4 * x * x + y * y;
            if (z <= limit && (z % 60 == 1  ||
                               z % 60 == 13 ||
                               z % 60 == 17 ||
                               z % 60 == 29 ||
                               z % 60 == 37 ||
                               z % 60 == 41 ||
                               z % 60 == 49 ||
                               z % 60 == 53)) {
                sieb[z] = !sieb[z];
            }
            z = 3 * x * x + y * y;
            if (z <= limit && (z % 60 == 7  ||
                               z % 60 == 19 ||
                               z % 60 == 31 ||
                               z % 60 == 43)) {
                sieb[z] = !sieb[z];
            }
            z = 3 * x * x - y * y;
            if (x > y && z <= limit && (z % 60 == 11 ||
                                        z % 60 == 23 ||
                                        z % 60 == 47 ||
                                        z % 60 == 59)) {
                sieb[z] = !sieb[z];
            }
        }
    }

    for (i = 5; i <= wlimit; i++) {
        if (sieb[i] == 1) {
            for (j = 1; j * i * i <= limit; j++) {
                sieb[j * i * i] = 0;
            }
        }
    }
    
    sieb[2] = 1;
    sieb[3] = 1;
    sieb[5] = 1;

    return sieb;
}

