#ifndef FUNCNODE_H
#define FUNCNODE_H

typedef struct funcnode funcnode;

funcnode * funcnode_new(void);
void funcnode_destroy(funcnode ** const);
void funcnode_set_f(funcnode * const, int (*)(void));
int (* funcnode_get_f(const funcnode * const))(void);
void funcnode_set_next(funcnode * const, funcnode * const);
funcnode * funcnode_get_next(const funcnode * const);

#endif
