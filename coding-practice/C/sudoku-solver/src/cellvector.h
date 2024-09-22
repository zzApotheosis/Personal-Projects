#ifndef CELLVECTOR_H
#define CELLVECTOR_H

#include "board.h"

void cellvector_get_row(const struct board * const, const size_t, const size_t, unsigned char * const);
void cellvector_get_column(const struct board * const, const size_t, const size_t, unsigned char * const);
void cellvector_get_cellgroup(const struct board * const, const size_t, const size_t, const size_t, unsigned char * const);

#endif
