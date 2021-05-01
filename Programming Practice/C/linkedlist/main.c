#include <stdlib.h>
#include <stdio.h>
#include <gmodule.h>
#include "main.h"

//void testFunction(void (*)(void*));
//void dummyFunction(void*);

int main(int argc, char** argv) {
//    struct LinkedList* list = NULL;
//    list = LinkedListNew();
//    LinkedListAppend(list, 1337);
//    testFunction(&dummyFunction);
    GSList* list = NULL;
    int i = 20;
    list = g_slist_append(list, &i);
    for (guint i = 0; i < g_slist_length(list); i++) {
        fprintf(stdout, "%d\n", *(int*)g_slist_nth_data(list, i));
    }
    return EXIT_SUCCESS;
}

//void testFunction(void (*fnptr)(void*)) {
//    (*fnptr)("test");
//}

//void dummyFunction(void* a) {
//    fprintf(stdout, "%s\n", (char*) a);
//}

