#include <stdlib.h>
#include <stdio.h>
#include "main.h"

int main(int argc, char** argv) {
    struct LinkedList* list = NULL;
    list = linkedlist_new();
    linkedlist_append(list, intToHeap(1337));

    for (int i = 20; i < 40; i += 5) {
        linkedlist_append(list, intToHeap(i));
    }

    linkedlist_destroy(list);

    return EXIT_SUCCESS;
}
