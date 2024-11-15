#include <stdlib.h>
#include "funcnode.h"

struct funcnode {
    int (* f)(void);
    funcnode * next;
};

funcnode * funcnode_new(void) {
    funcnode * self = (funcnode *) malloc(1 * sizeof(funcnode));
    self->f = NULL;
    self->next = NULL;
    return self;
}

void funcnode_destroy(funcnode ** const self) {
    if (self == NULL)
        return;
    if (*self == NULL)
        return;
    free(*self);
    *self = NULL;
}

void funcnode_set_f(funcnode * const self, int (* f)(void)) {
    if (self == NULL)
        return;
    self->f = f;
}

int (* funcnode_get_f(const funcnode * const self))(void) {
    if (self == NULL)
        return NULL;
    return self->f;
}

void funcnode_set_next(funcnode * const self, funcnode * const next) {
    if (self == NULL)
        return;
    self->next = next;
}

funcnode * funcnode_get_next(const funcnode * const self) {
    if (self == NULL)
        return NULL;
    return self->next;
}