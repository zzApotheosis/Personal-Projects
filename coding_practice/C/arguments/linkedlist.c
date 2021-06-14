#include <stdlib.h>
#include <stdio.h>
#include "linkedlist.h"

struct Node {
    void* key;
    void* value;
    struct Node* next;
};

struct LinkedList {
    struct Node* head;
    int size;
};

void a

