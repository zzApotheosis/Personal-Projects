#include <stdlib.h>
#include <stdio.h>
#include "main.h"

int main(int argc, char** argv) {
    //*(int*)NULL = 1;
    struct LinkedList* list = NULL;
    list = linkedlist_new();
    linkedlist_append(list, intToHeap(1337));

    fprintf(stdout, "List length: %d\n", linkedlist_length(list));
    linkedlist_destroy(list);

    return EXIT_SUCCESS;
}
