#include <stdlib.h>
#include <stdio.h>
#include <unistd.h>
#include <string.h>

#include "cellgroup.h"
#include "board.h"

/* Instance functions */

struct board * board_new() {
    struct board * instance = malloc(1 * sizeof(struct board));
    memset(instance, 0, sizeof(struct board));
    return instance;
}

void board_destroy(struct board * const instance) {
    if (instance == NULL)
        return;
    if (instance->cellgroups != NULL) {
        for (size_t i = 0; i < board_get_x_size(instance) * board_get_y_size(instance); i++) {
            if (instance->cellgroups[i].cells != NULL)
                free(instance->cellgroups[i].cells);
        }
        free(instance->cellgroups);
    }
    free(instance);
}

/* Utility functions */

void board_init(struct board * const self) {
    if (self == NULL)
        return; /* Nothing to initialize */
    if (self->cellgroups != NULL)
        return; /* Already initialized */
    self->cellgroups = (struct cellgroup *) malloc(board_get_x_size(self) * board_get_y_size(self) * sizeof(struct cellgroup));
    memset(self->cellgroups, 0, board_get_y_size(self) * board_get_x_size(self) * sizeof(struct cellgroup));
    struct cellgroup * cg = NULL;
    struct cell * c = NULL;
    for (size_t i = 0; i < board_get_cellgroup_y_size(self); i++) {
        for (size_t j = 0; j < board_get_cellgroup_x_size(self); j++) {
            cg = board_get_cellgroup(self, j, i);
            cellgroup_set_x_size(cg, board_get_cellgroup_x_size(self));
            cellgroup_set_y_size(cg, board_get_cellgroup_y_size(self));
            cellgroup_init(cg);
        }
    }
    for (unsigned int i = 0; i < board_get_total_y_size(self); i++) {
        for (unsigned int j = 0; j < board_get_total_x_size(self); j++) {
            c = board_get_cell(self, j, i);
            memset(cell_get_possible_values_vector(c), 1u, POSSIBLE_VALUES_MAX);
        }
    }
}

struct cellgroup * board_get_cellgroups(const struct board * const self, const size_t x, const size_t y) {
    if (self == NULL)
        return NULL;
    return self->cellgroups;
}

unsigned char board_get_cell_value(const struct board * const self, const size_t x, const size_t y) {
    if (self == NULL)
        return 0;
    return cell_get_value(board_get_cell(self, x, y));
}

void board_print(FILE * fp, struct board * const self) {
    struct cell * cell = NULL;
    for (unsigned int i = 0; i < board_get_total_y_size(self); i++) {
        for (unsigned int j = 0; j < board_get_total_x_size(self); j++) {
            cell = board_get_cell(self, j, i);
            fprintf(fp, "%u", cell_get_value(cell));
        }
        fprintf(fp, "\n");
    }
}

/* Setters and Getters */
void board_set_x_size(struct board * const self, const size_t new_x_size) {
    if (self == NULL)
        return;
    self->x_size = new_x_size;
}

void board_set_y_size(struct board * const self, const size_t new_y_size) {
    if (self == NULL)
        return;
    self->y_size = new_y_size;
}

void board_set_cellgroup_x_size(struct board * const self, const size_t new_cellgroup_x_size) {
    if (self == NULL)
        return;
    self->cellgroup_x_size = new_cellgroup_x_size;
}

void board_set_cellgroup_y_size(struct board * const self, const size_t new_cellgroup_y_size) {
    if (self == NULL)
        return;
    self->cellgroup_y_size = new_cellgroup_y_size;
}

void board_set_cellgroup(struct board * const self, const struct cellgroup * const new_cellgroup, const size_t x, const size_t y) {
    if (self == NULL)
        return;
    if (self->cellgroups == NULL)
        return;
    memcpy(&self->cellgroups[board_get_y_size(self) * y + x], new_cellgroup, sizeof(struct cellgroup));
}

void board_set_cellgroup_from_given(struct board * const self, const char given[], const size_t x, const size_t y) {
    if (self == NULL)
        return;
    if (self->cellgroups == NULL)
        return;
    /* TODO: Check for minimum length of given[] */
    char translation[9]; /* SHOULDN'T BE HARDCODED */
    memset(translation, 0, sizeof(translation));
    for (size_t i = 0; i < 9; i++) { /* SHOULDN'T BE HARDCODED */
        translation[i] = given[i] - '0';
    }
    board_set_cellgroup_from_given_raw(self, translation, x, y);
}

void board_set_cellgroup_from_given_raw(struct board * const self, const char given[], const size_t x, const size_t y) {
    if (self == NULL)
        return;
    if (self->cellgroups == NULL)
        return;
    struct cellgroup * cg = board_get_cellgroup(self, x, y);
    struct cell * c = NULL;
    for (size_t i = 0; i < cellgroup_get_y_size(cg); i++) {
        for (size_t j = 0; j < cellgroup_get_x_size(cg); j++) {
            c = cellgroup_get_cell(cg, j, i);
            cell_set_value(c, given[cellgroup_get_y_size(cg) * i + j]);
            cell_set_given_value(c, given[cellgroup_get_y_size(cg) * i + j]);
        }
    }
}

size_t board_get_x_size(const struct board * const self) {
    if (self == NULL)
        return 0;
    return self->x_size;
}

size_t board_get_y_size(const struct board * const self) {
    if (self == NULL)
        return 0;
    return self->y_size;
}

size_t board_get_cellgroup_x_size(const struct board * const self) {
    if (self == NULL)
        return 0;
    return self->cellgroup_x_size;
}

size_t board_get_cellgroup_y_size(const struct board * const self) {
    if (self == NULL)
        return 0;
    return self->cellgroup_y_size;
}

size_t board_get_total_x_size(const struct board * const self) {
    if (self == NULL)
        return 0;
    return board_get_x_size(self) * board_get_cellgroup_x_size(self);
}

size_t board_get_total_y_size(const struct board * const self) {
    if (self == NULL)
        return 0;
    return board_get_y_size(self) * board_get_cellgroup_y_size(self);
}

struct cellgroup * board_get_cellgroup(const struct board * const self, const size_t x, const size_t y) {
    if (self == NULL)
        return NULL;
    return &self->cellgroups[board_get_y_size(self) * y + x];
}

struct cell * board_get_cell(const struct board * const self, const size_t x, const size_t y) {
    if (self == NULL)
        return NULL;
    struct cellgroup * cg = board_get_cellgroup(self, x / board_get_cellgroup_x_size(self), y / board_get_cellgroup_y_size(self));
    struct cell * c = cellgroup_get_cell(cg, x % cellgroup_get_x_size(cg), y % cellgroup_get_y_size(cg));
    return c;
}

/* Debug functions */

void debug_board_dump_all(const struct board * const self) {
    if (self == NULL)
        return;
    for (size_t i = 0; i < board_get_cellgroup_y_size(self); i++) {
        for (size_t j = 0; j < board_get_cellgroup_x_size(self); j++) {
            fprintf(stderr, "DUMPING CELLGROUP AT POS: (%zu, %zu)\n", i, j);
            debug_cellgroup_dump_all(board_get_cellgroup(self, j, i));
        }
    }
}
