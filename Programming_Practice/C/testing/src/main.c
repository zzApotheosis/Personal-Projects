#include <stdlib.h>
#include <stdio.h>
#include "main.h"

int main(int argc, char** argv) {
    // Ligma
    struct LinkedList* test = linkedlist_new();
    linkedlist_append(test, util_int_to_heap(1337));
    fprintf(stdout, "Value = %d\n", *(int*)linkedlist_value_at(test, 0));
    linkedlist_destroy(test);
    return EXIT_SUCCESS;
}

