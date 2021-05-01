#ifndef LINKEDLIST_H
#define LINKEDLIST_H

/* Includes */
#include "node.h"

/* Class Structure */
struct LinkedList;
struct LinkedList* linkedlist_new(void);
void linkedlist_destroy(struct LinkedList*);

/* Object Functions */
void linkedlist_append(struct LinkedList*, void*);
void* linkedlist_value_at(struct LinkedList*, int);
int linkedlist_length(struct LinkedList*);

/* Setters and Getters */
void linkedlist_set_head(struct LinkedList*, struct Node*);
struct Node* linkedlist_get_head(struct LinkedList*);

#endif
