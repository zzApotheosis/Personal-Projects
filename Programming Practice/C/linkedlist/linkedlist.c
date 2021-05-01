#include <stdlib.h>
#include <stdio.h>
#include "linkedlist.h"

/* Forward declare static functions */
static void LinkedListRecursiveDestroy(struct Node*);

/* Class structure */
struct LinkedList {
    int size;
    struct Node* head;
};

static void LinkedListInit(struct LinkedList* self) {
    self->size = 0;
    self->head = NULL;
}

struct LinkedList* linkedlist_new() {
    struct LinkedList* new = (struct LinkedList*) malloc(sizeof(struct LinkedList));
    LinkedListInit(new);
    return new;
}

static void LinkedListReset(struct LinkedList* self) {
    LinkedListRecursiveDestroy(self->head);
}

static void LinkedListRecursiveDestroy(struct Node* node) {
    if (node_get_next(node) != NULL) {
        LinkedListRecursiveDestroy(node_get_next(node));
    }
    node_destroy(node);
}

void linkedlist_destroy(struct LinkedList* self) {
    if (self) {
        LinkedListReset(self);
        free(self);
    }
}

/* Object Functions */
void linkedlist_append(struct LinkedList* self, void* newValue) {
    struct Node* node;
    int* newKey = (int*) malloc(sizeof(int));

    if (linkedlist_get_size(self) == 0) {
        *newKey = 0;
        self->head = node_new(newKey, newValue);
    } else {
        node = self->head;
        while (node_get_next(node) != NULL) {
            node = node_get_next(node);
        }
        *newKey = linkedlist_get_size(self);
        node_set_next(node, node_new(newKey, newValue));
    }
    linkedlist_set_size(self, linkedlist_get_size(self) + 1);
}

void* linkedlist_value_at(struct LinkedList* self, int index) {
    if (index >= linkedlist_get_size(self)) {
        return NULL;
    }
    struct Node* node = linkedlist_get_head(self);
    for (int i = 0; i < index; i++) {
        node = node_get_next(node);
    }
    if (node != NULL) {
        return node_get_value(node);
    } else {
        return NULL;
    }
}

/* Setters and Getters */
void linkedlist_set_size(struct LinkedList* self, int newSize) {
    self->size = newSize;
}

void linkedlist_set_head(struct LinkedList* self, struct Node* newHead) {
    self->head = newHead;
}

int linkedlist_get_size(struct LinkedList* self) {
    return self->size;
}

struct Node* linkedlist_get_head(struct LinkedList* self) {
    return self->head;
}
