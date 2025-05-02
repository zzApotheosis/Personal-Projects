#include <stdlib.h>
#include <stdio.h>
#include <unistd.h>
#include <string.h>

#include "validity.h"
#include "board.h"
#include "cellvector.h"

static void determine_row_possibilities(struct board * const);
static void determine_column_possibilities(struct board * const);
static void determine_cellgroup_possibilities(struct board * const);
static void determine_special_possibilities(struct board * const); /* NOTE: May not implement */

void determine_board_possibilities(struct board * const board) {
  if (board == NULL)
    return;
  determine_row_possibilities(board);
  determine_column_possibilities(board);
  determine_cellgroup_possibilities(board);
}

void determine_row_possibilities(struct board * const board) {
  if (board == NULL)
    return;
  struct cell * cell = NULL;
  unsigned char row[POSSIBLE_VALUES_MAX];
  unsigned char known_values[POSSIBLE_VALUES_MAX];
  memset(row, 0, POSSIBLE_VALUES_MAX);
  memset(known_values, 0, POSSIBLE_VALUES_MAX);
  for (size_t i = 0; i < board_get_y_size(board) * board_get_cellgroup_y_size(board); i++) {
    memset(known_values, 0, POSSIBLE_VALUES_MAX);
    cellvector_get_row(board, i, POSSIBLE_VALUES_MAX, row);
    for (int j = 0; j < POSSIBLE_VALUES_MAX; j++) {
      if (row[j] == 0)
        continue;
      known_values[row[j] - 1] = 1; /* Track all the currently marked values in this row */
    }
    for (int j = 0; j < POSSIBLE_VALUES_MAX; j++) {
      /* Begin iterating over the row and updating the cells' possible values vectors */
      cell = board_get_cell(board, j, i);
      for (int k = 0; k < POSSIBLE_VALUES_MAX; k++) {
        /* First check if there's already a marked value in the cell (non-zero) */
        if (cell_get_value(cell) > 0) {
          /* If a value already exists in this cell, then every value is impossible (zeroize the vector) */
          memset(cell_get_possible_values_vector(cell), 0, POSSIBLE_VALUES_MAX);
          break; /* Break the inner loop and go to the next cell */
        } else {
          if (known_values[k]) { /* If the k value is known, then the current cell cannot possibly contain this value */
            cell_set_impossible_value(cell, k + 1);
          }
        }
      }
    }
  }
}

void determine_column_possibilities(struct board * const board) {
  if (board == NULL)
    return;
  struct cell * cell = NULL;
  unsigned char column[POSSIBLE_VALUES_MAX];
  unsigned char known_values[POSSIBLE_VALUES_MAX];
  memset(column, 0, POSSIBLE_VALUES_MAX);
  memset(known_values, 0, POSSIBLE_VALUES_MAX);
  for (size_t i = 0; i < board_get_x_size(board) * board_get_cellgroup_x_size(board); i++) {
    memset(known_values, 0, POSSIBLE_VALUES_MAX);
    cellvector_get_column(board, i, POSSIBLE_VALUES_MAX, column);
    for (int j = 0; j < POSSIBLE_VALUES_MAX; j++) {
      if (column[j] == 0)
        continue;
      known_values[column[j] - 1] = 1; /* Track all the currently marked values in this column */
    }
    for (int j = 0; j < POSSIBLE_VALUES_MAX; j++) {
      /* Begin iterating over the column and updating the cells' possible values vectors */
      cell = board_get_cell(board, i, j);
      for (int k = 0; k < POSSIBLE_VALUES_MAX; k++) {
        /* First check if there's already a marked value in the cell (non-zero) */
        if (cell_get_value(cell) > 0) {
          /* If a value already exists in this cell, then every value is impossible (zeroize the vector) */
          memset(cell_get_possible_values_vector(cell), 0, POSSIBLE_VALUES_MAX);
          break; /* Break the inner loop and go to the next cell */
        } else {
          if (known_values[k]) { /* If the k value is known, then the current cell cannot possibly contain this value */
            cell_set_impossible_value(cell, k + 1);
          }
        }
      }
    }
  }
}

void determine_cellgroup_possibilities(struct board * const board) {
  if (board == NULL)
    return;
  struct cell * cell = NULL;
  struct cellgroup * cellgroup = NULL;
  unsigned char cellgroup_vector[POSSIBLE_VALUES_MAX];
  unsigned char known_values[POSSIBLE_VALUES_MAX];
  memset(cellgroup_vector, 0, POSSIBLE_VALUES_MAX);
  memset(known_values, 0, POSSIBLE_VALUES_MAX);
  
  for (size_t i = 0; i < board_get_y_size(board); i++) {
    for (size_t j = 0; j < board_get_x_size(board); j++) {
      memset(known_values, 0, POSSIBLE_VALUES_MAX);
      cellgroup = board_get_cellgroup(board, j, i);
      cellvector_get_cellgroup(board, j, i, POSSIBLE_VALUES_MAX, cellgroup_vector);
      for (size_t k = 0; k < POSSIBLE_VALUES_MAX; k++) {
        if (cellgroup_vector[k] == 0)
          continue;
        known_values[cellgroup_vector[k] - 1] = 1;
      }
      for (size_t k = 0; k < POSSIBLE_VALUES_MAX; k++) {
        cell = cellgroup_get_cell(cellgroup, k % cellgroup_get_x_size(cellgroup), k / cellgroup_get_y_size(cellgroup));
        for (size_t l = 0; l < POSSIBLE_VALUES_MAX; l++) {
          if (cell_get_value(cell) > 0) {
            memset(cell_get_possible_values_vector(cell), 0, POSSIBLE_VALUES_MAX);
            break;
          } else {
            if (known_values[l]) {
              cell_set_impossible_value(cell, l + 1);
            }
          }
        }
      }
    }
  }
}
