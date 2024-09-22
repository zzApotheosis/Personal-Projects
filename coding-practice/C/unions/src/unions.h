#ifndef UNIONS_H
#define UNIONS_H

#define MAX_S_SIZE 16

/*
 * What makes unions different from structs is that the data defined in the union occupies the same
 * space in memory. The total size of the union is determined by the largest item in the union.
 *
 * In a struct, the total size is the summation of all the sizes of each item in the struct because
 * each item in a struct occupies its own space in memory.
 */

union cool_union {
        char s[MAX_S_SIZE];
        int n;
};

struct cool_struct {
        char s[MAX_S_SIZE];
        int n;
};

void ogres_have_layers(const int, const char * const);

#endif
