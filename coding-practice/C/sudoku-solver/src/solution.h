#ifndef SOLUTION_H
#define SOLUTION_H

#include "board.h"

#define SOLUTION_STATUS_ERROR -1
#define SOLUTION_STATUS_NORMAL 0
#define SOLUTION_STATUS_NON_LINEAR 1

int solution_iterate_guaranteed(struct board * const);
int solution_brute_force(struct board * const);

#endif
