#ifndef CELLGROUP_H
#define CELLGROUP_H

#include "cell.h"

struct cellgroup {
    size_t x_size;
    size_t y_size;
    struct cell * cells;
};

/* Instance functions */
struct cellgroup * cellgroup_new(void);
void cellgroup_destroy(struct cellgroup * const);

/* Utility functions */
void cellgroup_init(struct cellgroup * const);
struct cell * const cellgroup_get_cell(const struct cellgroup * const, const unsigned int, const unsigned int);
unsigned char cellgroup_get_cell_value(const struct cellgroup * const, const unsigned int, const unsigned int);

/* Setters and Getters */
void cellgroup_set_x_size(struct cellgroup * const, size_t);
void cellgroup_set_y_size(struct cellgroup * const, size_t);
size_t cellgroup_get_x_size(const struct cellgroup * const);
size_t cellgroup_get_y_size(const struct cellgroup * const);
size_t cellgroup_get_total_size(const struct cellgroup * const);

/* Debug functions */
void debug_cellgroup_dump_all(const struct cellgroup * const);

#endif
