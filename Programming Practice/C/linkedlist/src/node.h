#ifndef NODE_H
#define NODE_H

/* Class Structure */
struct Node;
struct Node* node_new(void*);
void node_destroy(struct Node*);

/* Setters and Getters */
void         node_set_value(struct Node*, void*);
void         node_set_next(struct Node*, struct Node*);
void*        node_get_value(struct Node*);
struct Node* node_get_next(struct Node*);

#endif

