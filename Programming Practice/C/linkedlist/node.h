#ifndef NODE_H
#define NODE_H

/* Class Structure */
struct Node;
struct Node* NodeNew(void*, void*);
void NodeDestroy(struct Node*);

/* Setters and Getters */
void         NodeSetKey(struct Node*, void*);
void         NodeSetValue(struct Node*, void*);
void         NodeSetNext(struct Node*, struct Node*);
void*        NodeGetKey(struct Node*);
void*        NodeGetValue(struct Node*);
struct Node* NodeGetNext(struct Node*);

#endif

