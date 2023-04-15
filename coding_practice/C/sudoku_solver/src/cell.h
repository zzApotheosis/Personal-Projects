#ifndef CELL_H
#define CELL_H

#include <stddef.h>

#define POSSIBLE_VALUES_MIN 1
#define POSSIBLE_VALUES_MAX 9
#define POSSIBLE_VALUE   1
#define IMPOSSIBLE_VALUE 0

struct cell {
  unsigned char value;
  unsigned char given_value;
  unsigned char possible_values[POSSIBLE_VALUES_MAX];
};

/* Instance functions */
struct cell * cell_new(void);
void cell_destroy(struct cell * const);

/* Utility functions */
unsigned char cell_value_is_possible(const struct cell * const, const size_t);
unsigned char cell_get_num_possible_values(struct cell * const);
unsigned char cell_get_next_possible_value(struct cell * const);

/* Setters and Getters */
void cell_set_value(struct cell * const, const unsigned char);
void cell_set_given_value(struct cell * const, const unsigned char);
void cell_set_possible_value(struct cell * const, const unsigned char);
void cell_set_impossible_value(struct cell * const, const unsigned char);
unsigned char cell_get_value(const struct cell * const);
unsigned char cell_get_given_value(const struct cell * const);
unsigned char * cell_get_possible_values_vector(struct cell * const);

#endif
