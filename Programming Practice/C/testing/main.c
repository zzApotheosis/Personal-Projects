#include <stdlib.h>
#include <stdio.h>

struct Node {
    unsigned int* index;
    void* data;
    struct Node* next;
};

struct LinkedList {
    size_t* dataSize;
    struct Node* headRef;
    int* size;
};

struct Node* newNode(size_t s, unsigned int i) {
    // Allocate memory for new Node
    struct Node* node = (struct Node*) malloc(sizeof(struct Node));
    node->index = malloc(sizeof(unsigned int));
    node->next = NULL;
    
    // Initialize struct members
    *(node->index) = i;
    node->data = malloc(s);

    // Done initializing
    return node;
}

struct LinkedList* newLinkedList(size_t s) {
    // Allocate memory for new Linked List
    struct LinkedList* linkedList = (struct LinkedList*) malloc(sizeof(struct LinkedList));
    linkedList->dataSize = malloc(sizeof(size_t));
    linkedList->size = malloc(sizeof(int));
    
    // Initialize struct members
    *(linkedList->dataSize) = s;
    linkedList->headRef = NULL;
    *(linkedList->size) = 0;
    
    // Done initializing
    return linkedList;
}

void destroyNode(struct Node* n) {
    free(n->index);
    free(n->data);
    if (n->next != NULL) {
        destroyNode(n->next);
    }
    free(n);
}

void destroyLinkedList(struct LinkedList* l) {
    free(l->dataSize);
    if (l->headRef != NULL) {
        destroyNode(l->headRef);
        free(l->headRef);
    }
    free(l->size);
    free(l);
}

void push(struct LinkedList* l, void* d, size_t s) {
    struct Node* node = l->headRef;
    // Get the last node in the list
    while (node->next != NULL) {
        node = node->next;
    }
    // Make a new node in the LinkedList
    node->next = newNode();
}

int main(int argc, char** argv) {
    // Ligma
    struct LinkedList* test = newLinkedList(sizeof(int));
    destroyLinkedList(test);
}

