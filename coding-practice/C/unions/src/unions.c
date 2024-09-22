#include <stdlib.h>
#include <stdio.h>
#include <unistd.h>
#include <string.h>

#include "unions.h"

static union cool_union cool_union;
static struct cool_struct cool_struct;

static void dump_union(const union cool_union d) {
        fprintf(stdout, "%s:%d Dumping cool_union\n", __FILE__, __LINE__);
        unsigned char * ptr = (unsigned char *) &d;
        for (unsigned int i = 0; i < sizeof(union cool_union); i++) {
                fprintf(stdout, "[0x%08x] = decimal(%03d), hex(0x%02x), char(%c)\n", i, *ptr, *ptr, *ptr);
                ptr++;
        }
}

static void dump_struct(const struct cool_struct d) {
        fprintf(stdout, "%s:%d Dumping cool_struct\n", __FILE__, __LINE__);
        unsigned char * ptr = (unsigned char *) &d;
        for (unsigned int i = 0; i < sizeof(struct cool_struct); i++) {
                fprintf(stdout, "[0x%08x] = decimal(%03d), hex(0x%02x), char(%c)\n", i, *ptr, *ptr, *ptr);
                ptr++;
        }
}

void ogres_have_layers(const int n, const char * const s) {
        fprintf(stdout, "%s:%d received args: %d, %s\n", __FILE__, __LINE__, n, s);

        /* Reset cool union */
        memset(&cool_union, 0, sizeof(union cool_union));
        memcpy(cool_union.s, s, strnlen(s, MAX_S_SIZE));
        cool_union.n = n;

        /* Reset cool struct */
        memset(&cool_struct, 0, sizeof(struct cool_struct));
        memcpy(cool_struct.s, s, strnlen(s, MAX_S_SIZE));
        cool_struct.n = n;
        
        /* Dump the data for both the union and the struct to show their differences */
        dump_union(cool_union);
        dump_struct(cool_struct);
}
