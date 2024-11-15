#ifndef FUNCTIONLIST_H
#define FUNCTIONLIST_H

#include "funcnode.h"

typedef struct functionlist functionlist;

functionlist * functionlist_new(void);
void functionlist_destroy(functionlist ** const);
void functionlist_push(functionlist * const, int (* const)(void));
funcnode * functionlist_pop(functionlist * const);

#endif
