#include <stdlib.h>
#include <stdio.h>
#include <unistd.h>
#include <string.h>

#include "cell.h"

/* Instance functions */

struct cell * cell_new() {
    struct cell * instance = (struct cell *) malloc(1 * sizeof(struct cell));
    memset(instance, 0, sizeof(struct cell));
    return instance;
}

void cell_destroy(struct cell * const instance) {
    if (instance == NULL)
        return;
    free(instance);
}

/* Utility functions */
unsigned char cell_value_is_possible(const struct cell * const self, const size_t value) {
    if (self == NULL)
        return 0;
    if (value < POSSIBLE_VALUES_MIN || value > POSSIBLE_VALUES_MAX)
        return 0;
    return self->possible_values[value - 1]; /* We expect to pass in an actual value, 1 - 9, but the array is indexed at 0 - 8, so we subtract one from the passed in value */
}

unsigned char cell_get_num_possible_values(struct cell * const self) {
    if (self == NULL)
        return 0;
    const unsigned char * const possible_values_vector = cell_get_possible_values_vector(self);
    unsigned char count = 0u;
    for (size_t i = 0u; i < POSSIBLE_VALUES_MAX; i++) {
        if (possible_values_vector[i])
            count++;
    }
    return count;
}

unsigned char cell_get_next_possible_value(struct cell * const self) {
    /* This function is expected to return a value between 1 - 9, representing the next possible. If it returns 0, then either an error has occurred or it cannot determine the next possible value, e.g. the current value is 9 and cannot exceed 9, e.g. the given value is equal to the current value and therefore should never change */
    if (self == NULL)
        return 0;
    if (cell_get_given_value(self) > 0)
        return 0;
    unsigned char value = 0;
    unsigned char * possible_values_vector = cell_get_possible_values_vector(self);
    for (unsigned char i = cell_get_value(self) + 1; i <= POSSIBLE_VALUES_MAX; i++) {
        if (possible_values_vector[i - 1] == POSSIBLE_VALUE) {
            value = i;
            break;
        }
    }
    return value;
}

/* Setters and Getters */

void cell_set_value(struct cell * const self, const unsigned char new_value) {
    if (self == NULL)
        return;
    self->value = new_value;
}

void cell_set_given_value(struct cell * const self, const unsigned char new_given_value) {
    if (self == NULL)
        return;
    self->given_value = new_given_value;
}

void cell_set_possible_value(struct cell * const self, const unsigned char possible_value) {
    if (self == NULL)
        return;
    if (possible_value == 0 || possible_value >= POSSIBLE_VALUES_MAX)
        return;
    unsigned char * possible_values_vector = cell_get_possible_values_vector(self);
    possible_values_vector[possible_value - 1] = POSSIBLE_VALUE;
}

void cell_set_impossible_value(struct cell * const self, const unsigned char impossible_value) {
    if (self == NULL)
        return;
    if (impossible_value == 0 || impossible_value > POSSIBLE_VALUES_MAX)
        return;
    unsigned char * possible_values_vector = cell_get_possible_values_vector(self);
    possible_values_vector[impossible_value - 1] = IMPOSSIBLE_VALUE;
}

unsigned char cell_get_value(const struct cell * const self) {
    if (self == NULL)
        return 0;
    return self->value;
}

unsigned char cell_get_given_value(const struct cell * const self) {
    if (self == NULL)
        return 0;
    return self->given_value;
}

unsigned char * cell_get_possible_values_vector(struct cell * const self) {
    return self->possible_values;
}
