/*
 * This example of a singly linked list does NOT implement any linked list from scratch.
 * Instead, this example uses the sys/queue.h library already provided on *nix systems.
 * It achieves the same concept of a linked list without reinventing the wheel.
 * 
 * See slist(3).
 * https://man.archlinux.org/man/slist.3
 */

#include <stdlib.h>
#include <stdio.h>
#include <unistd.h>
#include <sys/queue.h>

/* User-defined struct for the nodes in the slist */
struct node {
  int data;
  SLIST_ENTRY(node) nodes;
};

/* Utilizing a macro to define slisthead as a struct to represent the head of the slist */
SLIST_HEAD(slisthead, node);

/* Main function */
int main(void) {
  /* Define some variables */
  struct node * n = NULL;
  struct slisthead list;
  int num_nodes = 10;
  SLIST_INIT(&list);

  /* Create nodes for the slist */
  for (int i = 0; i < num_nodes; i++) {
    n = malloc(1 * sizeof(struct node));
    n->data = (i + 1) * 100;
    SLIST_INSERT_HEAD(&list, n, nodes);
  }

  /*
   * Do something with the slist to show that it is properly holding data in memory.
   * Notice how the slist currently only has the ability to append new nodes to the
   * head of the list, not the end. Note, this does not affect time complexity of the standard
   * singly linked list implementation. It is still O(n) to operate on specific nodes.
   * An insertion operation has a time complexity of O(1).
   */
  n = SLIST_FIRST(&list);
  while (n != NULL) {
    fprintf(stdout, "Node contains: %d\n", n->data);
    n = SLIST_NEXT(n, nodes);
  }
  fflush(stdout);

  /*
   * Clear the slist and free the memory.
   * Note, the memory must be freed in this order. First, get a pointer to the head node of the slist.
   * Second, use the macro to remove the head of the slist. Third and finally, free the memory pointed
   * to by the pointer from the first step. Swapping the second and third step will cause a segfault.
   */
  while (!SLIST_EMPTY(&list)) {
    n = SLIST_FIRST(&list);
    SLIST_REMOVE_HEAD(&list, nodes);
    free(n);
  }
  
  /* Done :) */
  return EXIT_SUCCESS;
}
