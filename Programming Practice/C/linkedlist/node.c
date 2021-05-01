#include <stdlib.h>
#include <stdio.h>
#include "node.h"

/* Begin Class Structure */

struct Node {
    void* key;
    void* value;
    struct Node* next;
};

// Constructor
static void NodeInit(struct Node* self) {
    self->key = NULL;
    self->value = NULL;
    self->next = NULL;
}

// Allocation + Constructor
struct Node* NodeNew(void* key, void* value) {
    struct Node* new = (struct Node*) malloc(sizeof(struct Node));
    NodeInit(new);
    return new;
}

// Pre-deconstruction
static void NodeReset(struct Node* self) {
    // Do nothing
}

// Deconstructor
void NodeDestroy(struct Node* self) {
    if (self) {
        NodeReset(self);
        free(self);
    }
}

/* End Class Structure */

/* Begin Setters and Getters */

void NodeSetKey(struct Node* self, void* newKey) {
    self->key = newKey;
}

void NodeSetValue(struct Node* self, void* newValue) {
    self->value = newValue;
}

void NodeSetNext(struct Node* self, struct Node* newNext) {
    self->next = newNext;
}

void* NodeGetKey(struct Node* self) {
    return self->key;
}

void* NodeGetValue(struct Node* self) {
    return self->value;
}

struct Node* NodeGetNext(struct Node* self) {
    return self->next;
}

/* End Setters and Getters */

