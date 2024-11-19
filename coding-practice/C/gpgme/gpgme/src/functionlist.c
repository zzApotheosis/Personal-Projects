#include <stdlib.h>

#include <common-macros.h>

#include "functionlist.h"

struct functionlist {
    funcnode * head;
};

functionlist * functionlist_new(void) {
    functionlist * self = (functionlist *) malloc(1 * sizeof(functionlist));
    self->head = NULL;
    return self;
}

void functionlist_destroy(functionlist ** const self) {
    if (self == NULL)
        return;
    if (*self == NULL)
        return;
    free(*self);
    *self = NULL;
}

void functionlist_push(functionlist * const self, int (* const f)(void)) {
    if (self == NULL) {
        warn("NULL functionlist");
        return;
    }
    funcnode * fnode = funcnode_new();
    funcnode_set_next(fnode, self->head);
    funcnode_set_f(fnode, f);
    self->head = fnode;
}

funcnode * functionlist_pop(functionlist * const self) {
    if (self == NULL) {
        warn("NULL functionlist");
        return NULL;
    }
    funcnode * fnode = self->head;
    self->head = funcnode_get_next(self->head);
    return fnode;
}
