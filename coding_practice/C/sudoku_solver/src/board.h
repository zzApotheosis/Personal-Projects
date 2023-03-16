#ifndef BOARD_H
#define BOARD_H

#include "cellgroup.h"

struct board {
    size_t x_size;
    size_t y_size;
    size_t cellgroup_x_size;
    size_t cellgroup_y_size;
    struct cellgroup * cellgroups;
};

/* Instance functions */
struct board * board_new(void);
void board_destroy(struct board * const);

/* Utility functions */
void board_init(struct board * const);
struct cellgroup * board_get_cellgroups(const struct board * const, const size_t, const size_t);
unsigned char board_get_cell_value(const struct board * const, const size_t, const size_t);
void board_print(FILE *, struct board * const);

/* Setters and Getters */
void board_set_x_size(struct board * const, const size_t);
void board_set_y_size(struct board * const, const size_t);
void board_set_cellgroup_x_size(struct board * const, const size_t);
void board_set_cellgroup_y_size(struct board * const, const size_t);
void board_set_cellgroup(struct board * const, const struct cellgroup * const, const size_t, const size_t);
void board_set_cellgroup_from_given(struct board * const, const char * const, const size_t, const size_t);
void board_set_cellgroup_from_given_raw(struct board * const, const char * const, const size_t, const size_t);
size_t board_get_x_size(const struct board * const);
size_t board_get_y_size(const struct board * const);
size_t board_get_cellgroup_x_size(const struct board * const);
size_t board_get_cellgroup_y_size(const struct board * const);
size_t board_get_total_x_size(const struct board * const);
size_t board_get_total_y_size(const struct board * const);
struct cellgroup * board_get_cellgroup(const struct board * const, const size_t, const size_t);
struct cell * board_get_cell(const struct board * const, const size_t, const size_t);

/* Debug functions */
void debug_board_dump_all(const struct board * const);

#endif
