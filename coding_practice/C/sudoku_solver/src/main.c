#include <stdlib.h>
#include <stdio.h>
#include <unistd.h>
#include <string.h>

#include "board.h"
#include "cellvector.h"
#include "validity.h"
#include "solution.h"

#define BOARD_X_SIZE 3
#define BOARD_Y_SIZE 3
#define CELLGROUP_X_SIZE 3
#define CELLGROUP_Y_SIZE 3

int main(int argc, char * argv[]) {
    struct board * board = board_new();
    board_set_x_size(board, BOARD_X_SIZE);
    board_set_y_size(board, BOARD_Y_SIZE);
    board_set_cellgroup_x_size(board, CELLGROUP_X_SIZE);
    board_set_cellgroup_y_size(board, CELLGROUP_Y_SIZE);
    board_init(board);
    
    size_t test_cell_x = 0u;
    size_t test_cell_y = 0u;
    unsigned char buffer[POSSIBLE_VALUES_LIMIT];
    memset(buffer, 0, POSSIBLE_VALUES_LIMIT);

    // Extreme
    //board_set_cellgroup_from_given(board, "000070000", 0, 0);
    //board_set_cellgroup_from_given(board, "000500079", 1, 0);
    //board_set_cellgroup_from_given(board, "074020000", 2, 0);
    //board_set_cellgroup_from_given(board, "050002098", 0, 1);
    //board_set_cellgroup_from_given(board, "040901020", 1, 1);
    //board_set_cellgroup_from_given(board, "390400060", 2, 1);
    //board_set_cellgroup_from_given(board, "000030980", 0, 2);
    //board_set_cellgroup_from_given(board, "610007000", 1, 2);
    //board_set_cellgroup_from_given(board, "000050000", 2, 2);

    // Easy
    board_set_cellgroup_from_given(board, "930010507", 0, 0);
    board_set_cellgroup_from_given(board, "007004006", 1, 0);
    board_set_cellgroup_from_given(board, "800905010", 2, 0);
    board_set_cellgroup_from_given(board, "806200005", 0, 1);
    board_set_cellgroup_from_given(board, "050409070", 1, 1);
    board_set_cellgroup_from_given(board, "700008201", 2, 1);
    board_set_cellgroup_from_given(board, "050402003", 0, 2);
    board_set_cellgroup_from_given(board, "700100800", 1, 2);
    board_set_cellgroup_from_given(board, "304060092", 2, 2);

    // Medium
    //board_set_cellgroup_from_given(board, "070806005", 0, 0);
    //board_set_cellgroup_from_given(board, "900005086", 1, 0);
    //board_set_cellgroup_from_given(board, "060000001", 2, 0);
    //board_set_cellgroup_from_given(board, "300601000", 0, 1);
    //board_set_cellgroup_from_given(board, "000000000", 1, 1);
    //board_set_cellgroup_from_given(board, "000302006", 2, 1);
    //board_set_cellgroup_from_given(board, "400000020", 0, 2);
    //board_set_cellgroup_from_given(board, "620400003", 1, 2);
    //board_set_cellgroup_from_given(board, "900205070", 2, 2);
    
    
    int result = SOLUTION_STATUS_NORMAL;
    board_print(stdout, board);
    fprintf(stdout, "\n");
    while (result == SOLUTION_STATUS_NORMAL) {
        result = solution_iterate_guaranteed(board);
        board_print(stdout, board);
        fprintf(stdout, "\n");
    }

    if (result == SOLUTION_STATUS_NON_LINEAR) {
        fprintf(stdout, "Board solution is now non-linear\n");
        solution_brute_force(board);
    }
    
    board_destroy(board);
    return EXIT_SUCCESS;
}
