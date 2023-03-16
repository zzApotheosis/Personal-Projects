#include <stdlib.h>
#include <stdio.h>
#include <unistd.h>
#include <string.h>

#include "cellgroup.h"

/* Instance functions */

struct cellgroup * cellgroup_new() {
    struct cellgroup * instance = (struct cellgroup *) malloc(1 * sizeof(struct cellgroup));
    memset(instance, 0, sizeof(struct cellgroup));
    return instance;
}

void cellgroup_destroy(struct cellgroup * const instance) {
    if (instance == NULL)
        return;
    if (instance->cells != NULL)
        free(instance->cells);
    free(instance);
}

/* Utility functions */

void cellgroup_init(struct cellgroup * const self) {
    if (self == NULL)
        return;
    if (self->cells == NULL)
        self->cells = (struct cell *) malloc(cellgroup_get_x_size(self) * cellgroup_get_y_size(self) * sizeof(struct cell));
    memset(self->cells, 0, cellgroup_get_x_size(self) * cellgroup_get_y_size(self) * sizeof(struct cell)); /* All cells initialize to 0 */
}

struct cell * const cellgroup_get_cell(const struct cellgroup * const self, const unsigned int x, const unsigned int y) {
    if (self == NULL)
        return NULL;
    return &self->cells[cellgroup_get_y_size(self) * y + x];
}

unsigned char cellgroup_get_cell_value(const struct cellgroup * const self, const unsigned int x, const unsigned int y) {
    if (self == NULL)
        return 0;
    return self->cells[cellgroup_get_y_size(self) * y + x].value;
}

/* Setters and Getters */

void cellgroup_set_x_size(struct cellgroup * const self, size_t new_x_size) {
    if (self != NULL)
        self->x_size = new_x_size;
}

void cellgroup_set_y_size(struct cellgroup * const self, size_t new_y_size) {
    if (self != NULL)
        self->y_size = new_y_size;
}

size_t cellgroup_get_x_size(const struct cellgroup * const self) {
    if (self == NULL)
        return -1;
    return self->x_size;
}

size_t cellgroup_get_y_size(const struct cellgroup * const self) {
    if (self == NULL)
        return -1;
    return self->y_size;
}

size_t cellgroup_get_total_size(const struct cellgroup * const self) {
    if (self == NULL)
        return -1;
    return self->y_size * self->x_size;
}

/* Debug functions */
void debug_cellgroup_dump_all(const struct cellgroup * const self) {
    if (self == NULL)
        return;
    for (size_t i = 0; i < cellgroup_get_y_size(self); i++) {
        for (size_t j = 0; j < cellgroup_get_x_size(self); j++) {
            fprintf(stderr, "DUMPING CELL AT POS (%zu, %zu): %u\n", j, i, cell_get_value(cellgroup_get_cell(self, j, i)));
        }
    }
}
