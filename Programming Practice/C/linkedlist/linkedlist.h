#ifndef LINKEDLIST_H
#define LINKEDLIST_H

/* Includes */
#include "node.h"

/* Class Structure */
struct LinkedList;
struct LinkedList* LinkedListNew(void);
void LinkedListDestroy(struct LinkedList*);

/* Object Functions */
void LinkedListAppend(struct LinkedList*, void*);
void* LinkedListValueAt(struct LinkedList*, int);

/* Setters and Getters */
void LinkedListSetSize(struct LinkedList*, int);
void LinkedListSetHead(struct LinkedList*, struct Node*);
int LinkedListGetSize(struct LinkedList*);
struct Node* LinkedListGetHead(struct LinkedList*);

#endif

