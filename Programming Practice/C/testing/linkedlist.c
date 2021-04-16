#include <stdlib.h>
#include <stdio.h>

/*
 * A linked list node
 */
struct Node {
    void *data;
    struct Node *next;
};

/*
 * Function to add a node at the beginning of the linked list.
 * This function expects a pointer to the data to be added and
 * size of the data type
 */
void push(struct Node **head_ref, void *new_data, size_t data_size) {
    // Allocate memory for the new node
    struct Node *new_node = (struct Node*) malloc(sizeof(struct Node));
    new_node->data = malloc(data_size);
    new_node->next = (*head_ref);

    // Copy contents of new_data to newly allocated memory
    // Assumption: char takes 1 byte
    for (size_t i = 0; i < data_size; i++) {
        *(char *)(new_node->data + i) = *(char *)(new_data + i);
    }

    // Changed head pointer as new node is added at the beginning
    (*head_ref) = new_node;
}

/*
 * Function to print nodes in a given linked list. fpitr is used
 * to access the function to be used for printing current node data.
 * Note that the different data types need different specifier in printf()
 */
void printList(struct Node *node, void (*fptr)(void *)) {
    
}

