#ifndef UTIL_H
#define UTIL_H

#define UTIL_STR_MAX_LEN 65536

/* Utility Functions */
int* util_int_to_heap(int);
float* util_float_to_heap(float);
double* util_double_to_heap(double);
unsigned* util_unsigned_to_heap(unsigned);
long* util_long_to_heap(long);
char* util_char_to_heap(char);
char* util_str_to_heap(char*);
size_t util_strlen(char*);

/* Node Class Structure */
struct Node;
struct Node* node_new(void*);
void node_destroy(struct Node*);

/* Node Setters and Getters */
void         node_set_value(struct Node*, void*);
void         node_set_next(struct Node*, struct Node*);
void*        node_get_value(struct Node*);
struct Node* node_get_next(struct Node*);

/* LinkedList Class Structure */
struct LinkedList;
struct LinkedList* linkedlist_new(void);
void linkedlist_destroy(struct LinkedList*);

/* LinkedList Object Functions */
void linkedlist_append(struct LinkedList*, void*);
void* linkedlist_value_at(struct LinkedList*, int);
int linkedlist_length(struct LinkedList*);

/* LinkedList Setters and Getters */
void linkedlist_set_head(struct LinkedList*, struct Node*);
struct Node* linkedlist_get_head(struct LinkedList*);

#endif

