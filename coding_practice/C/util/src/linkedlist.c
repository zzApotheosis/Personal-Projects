#include <stdlib.h>
#include <stdio.h>
#include "include/util.h"

/* Forward declare static functions */
static void LinkedListRecursiveDestroy(struct Node*);

/* Class structure */
struct LinkedList {
    struct Node* head;
};

static void LinkedListInit(struct LinkedList* self) {
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
    if (self == NULL) {
        return;
    }
    LinkedListReset(self);
    free(self);
}

/* Object Functions */
void linkedlist_append(struct LinkedList* self, void* newValue) {
    struct Node* node;

    // Check for NULL
    if (self == NULL) {
        return;
    }
    
    // Append new node
    if (linkedlist_length(self) == 0) {
        self->head = node_new(newValue);
    } else {
        node = self->head;
        while (node_get_next(node) != NULL) {
            node = node_get_next(node);
        }
        node_set_next(node, node_new(newValue));
    }
}

void* linkedlist_value_at(struct LinkedList* self, int index) {
    if (index >= linkedlist_length(self)) {
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

int linkedlist_length(struct LinkedList* self) {
    int i = 0;
    struct Node* node = NULL;
    if (self == NULL) {
        return i;
    }
    node = self->head;
    while (node != NULL) {
        i++;
        node = node_get_next(node);
    }
    return i;
}

/* Setters and Getters */
void linkedlist_set_head(struct LinkedList* self, struct Node* newHead) {
    self->head = newHead;
}

struct Node* linkedlist_get_head(struct LinkedList* self) {
    return self->head;
}
