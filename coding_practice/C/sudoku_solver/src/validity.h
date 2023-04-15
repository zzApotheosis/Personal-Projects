#ifndef VALIDITY_H
#define VALIDITY_H

#include "board.h"

/*
 * This function needs to accept a board object and recursively determine every single possible value in every empty cell. There are a few algorithmic ways to evaluate each empty cell:
 * 1. No identical numbers can possibly be in the same row. This means that we can check the current values in every other cell in the same row and eliminate the possibility that those numbers can occupy the currently checked cell.
 * 2. No identical numbers can possibly be in the same column. This means that we can check the current values in every other cell in the same column and eliminate the possibility that those numbers can occupy the currently checked cell.
 * 3. No identical numbers can possibly be in the same cell group. This means that we can check the current values in every other cell in the same cell group and eliminate the possibility that those numbers can occupy the currently checked cell.
 *
 * These three main principles are what eliminate the vast majority of possible solutions. Depending on the difficulty of the puzzle (and the possibility of having multiple solutions), the remaining possible solutions may be evaluated either by brute-force, a form of binary search tree, or other solving algorithm.
 */
void determine_board_possibilities(struct board * const);

#endif
