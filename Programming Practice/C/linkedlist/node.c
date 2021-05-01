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
static void NodeInit(struct Node* self, void* key, void* value) {
    self->key = key;
    self->value = value;
    self->next = NULL;
}

// Allocation + Constructor
struct Node* node_new(void* key, void* value) {
    struct Node* new = (struct Node*) malloc(sizeof(struct Node));
    NodeInit(new, key, value);
    return new;
}

// Pre-deconstruction
static void NodeReset(struct Node* self) {
    free(self->key);
    free(self->value);
}

// Deconstructor
void node_destroy(struct Node* self) {
    if (self) {
        NodeReset(self);
        free(self);
    }
}

/* End Class Structure */

/* Begin Setters and Getters */

void node_set_key(struct Node* self, void* newKey) {
    if (self == NULL) {
        return;
    } else {
        self->key = newKey;
    }
}

void node_set_value(struct Node* self, void* newValue) {
    if (self == NULL) {
        return;
    } else {
        self->value = newValue;
    }
}

void node_set_next(struct Node* self, struct Node* newNext) {
    if (self == NULL) {
        return;
    } else {
        self->next = newNext;
    }
}

void* node_get_key(struct Node* self) {
    if (self == NULL) {
        return NULL;
    } else {
        return self->key;
    }
}

void* node_get_value(struct Node* self) {
    if (self == NULL) {
        return NULL;
    } else {
        return self->value;
    }
}

struct Node* node_get_next(struct Node* self) {
    if (self == NULL) {
        return NULL;
    } else {
        return self->next;
    }
}

/* End Setters and Getters */
