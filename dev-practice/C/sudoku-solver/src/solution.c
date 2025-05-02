#include <stdlib.h>
#include <stdio.h>
#include <unistd.h>
#include <string.h>

#include "solution.h"
#include "board.h"
#include "validity.h"

int solution_iterate_guaranteed(struct board * const board) {
  struct cell * cell = NULL;
  unsigned char value = 0;
  int result = SOLUTION_STATUS_NON_LINEAR;

  determine_board_possibilities(board);
  for (size_t i = 0; i < board_get_total_y_size(board); i++) {
    for (size_t j = 0; j < board_get_total_x_size(board); j++) {
      cell = board_get_cell(board, j, i);
      
      /* If there are no possible values, we assume that it already contains a value */
      if (cell_get_num_possible_values(cell) <= 0)
        continue;

      /* If there is only one possible value, we set it */
      if (cell_get_num_possible_values(cell) <= 1) {
        value = cell_get_next_possible_value(cell);
        cell_set_value(cell, value);
        result = SOLUTION_STATUS_NORMAL; /* This iteration resulted in a positive change */
        continue;
      }

      if (cell_get_num_possible_values(cell) <= 2) {
        fprintf(stdout, "Found two-way possibility candidate in cell (%zu, %zu)\n", j, i);
        continue;
      }
    }
  }
  determine_board_possibilities(board);
  return result;
}

int solution_brute_force(struct board * const board) {
  struct cell * cell = NULL;
  /* TODO */
  return 0;
}
