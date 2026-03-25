#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <sys/queue.h>

#include "stailq.h"

#define EXAMPLE_INDEX 5

struct list_entry
{
  void * data;
  STAILQ_ENTRY(list_entry) list_entries;
};

STAILQ_HEAD(list_head, list_entry);

static struct list_entry *
list_entry_new(void * const data)
{
  struct list_entry * const new_entry = (struct list_entry * const) calloc(1, sizeof(struct list_entry));
  if (new_entry == NULL)
    abort();
  new_entry->data = data;
  return new_entry;
}

static struct list_entry *
list_entry_at(const struct list_head * const head, const size_t index)
{
  if (head == NULL)
    abort();
  struct list_entry * list_entry = STAILQ_FIRST(head);
  for (size_t i = 0; i < index; i++)
    list_entry = STAILQ_NEXT(list_entry, list_entries);
  return list_entry;
}

void
do_stailq_example(const int argc, char * argv[])
{
  (void) argc;
  (void) argv;

  struct list_entry * entry = NULL;
  struct list_head head;

  assert(EXAMPLE_INDEX < NUM_NODES);

  STAILQ_INIT(&head);

  /* Adding dummy data to the list */
  for (unsigned long int i = 0; i < NUM_NODES; i++)
    {
      entry = list_entry_new((void * const) i);
      STAILQ_INSERT_TAIL(&head, entry, list_entries);
    }

  /* Iterating over the list */
  STAILQ_FOREACH(entry, &head, list_entries)
  fprintf(stderr, "Node data: [%p]\n", entry->data);

  /* Accessing specific indexes in the list */
  entry = list_entry_at(&head, EXAMPLE_INDEX);
  fprintf(stderr, "Node[%u] data = [%p]\n", EXAMPLE_INDEX, entry->data);

  /* Freeing the list */
  for (; !STAILQ_EMPTY(&head);)
    {
      entry = STAILQ_FIRST(&head);
      STAILQ_REMOVE_HEAD(&head, list_entries);
      free(entry);
    }

  /* Initialize the list before exiting (I think this is good practice? They do it in the example code) */
  STAILQ_INIT(&head);
}
