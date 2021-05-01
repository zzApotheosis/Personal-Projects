#include <stdlib.h>
#include <stdio.h>
#include "linkedlist.h"

/* Class structure */
struct LinkedList {
    int size;
    struct Node* head;
};

static void LinkedListInit(struct LinkedList* self) {
    self->size = 0;
    self->head = NULL;
}

struct LinkedList* LinkedListNew() {
    struct LinkedList* new = (struct LinkedList*) malloc(sizeof(struct LinkedList));
    LinkedListInit(new);
    return new;
}

static void LinkedListReset(struct LinkedList* self) {
    // TODO
}

void LinkedListDestroy(struct LinkedList* self) {
    if (self) {
        LinkedListReset(self);
        free(self);
    }
}

/* Object Functions */
void LinkedListAppend(struct LinkedList* self, void* newValue) {
    struct Node* node;
    int* newKey = (int*) malloc(sizeof(int));

    if (LinkedListGetSize(self) == 0) {
        *newKey = 0;
        self->head = NodeNew(newKey, newValue);
    } else {
        node = self->head;
        while (NodeGetNext(node) != NULL) {
            node = NodeGetNext(node);
        }
        *newKey = LinkedListGetSize(self) + 1;
        NodeSetNext(node, NodeNew(newKey, newValue));
    }
    LinkedListSetSize(self, LinkedListGetSize(self) + 1);
}

void* LinkedListValueAt(struct LinkedList* self, int index) {
    if (index >= LinkedListGetSize(self)) {
        return NULL;
    }
    struct Node* node = LinkedListGetHead(self);
    for (int i = 0; i < index; i++) {
        node = NodeGetNext(node);
    }
    if (node != NULL) {
        return NodeGetValue(node);
    } else {
        return NULL;
    }
}

/* Setters and Getters */
void LinkedListSetSize(struct LinkedList* self, int newSize) {
    self->size = newSize;
}

void LinkedListSetHead(struct LinkedList* self, struct Node* newHead) {
    self->head = newHead;
}

int LinkedListGetSize(struct LinkedList* self) {
    return self->size;
}

struct Node* LinkedListGetHead(struct LinkedList* self) {
    return self->head;
}

