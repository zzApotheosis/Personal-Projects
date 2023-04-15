#include <stdlib.h>
#include <stdio.h>
#include <unistd.h>

#include "board.h"
#include "cellvector.h"

/* It is expected and assumed that the buffer size will always be 9, since a sudoku row/column always holds a total of 9 numbers */

void cellvector_get_row(const struct board * const b, const size_t target, const size_t buffer_size, unsigned char * const buffer) {
  if (b == NULL)
    return;
  for (size_t i = 0; i < buffer_size; i++) {
    buffer[i] = cell_get_value(board_get_cell(b, i, target));
  }
}

void cellvector_get_column(const struct board * const b, const size_t target, const size_t buffer_size, unsigned char * const buffer) {
  if (b == NULL)
    return;
  for (size_t i = 0; i < buffer_size; i++) {
    buffer[i] = cell_get_value(board_get_cell(b, target, i));
  }
}

void cellvector_get_cellgroup(const struct board * const b, const size_t x, const size_t y, const size_t buffer_size, unsigned char * const buffer) {
  if (b == NULL)
    return;
  struct cellgroup * cellgroup = board_get_cellgroup(b, x, y);
  if (cellgroup_get_total_size(cellgroup) > POSSIBLE_VALUES_MAX)
    abort();
  for (size_t i = 0; i < cellgroup_get_total_size(cellgroup); i++) {
    buffer[i] = cellgroup_get_cell_value(cellgroup, i % cellgroup_get_x_size(cellgroup), i / cellgroup_get_y_size(cellgroup));
  }
}
