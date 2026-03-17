#ifndef COOL_H
#define COOL_H

typedef struct CoolStruct CoolStruct;

CoolStruct * CoolStruct_new(void);
void CoolStruct_destroy(CoolStruct ** const);
int CoolStruct_get_x(CoolStruct * const);
int CoolStruct_get_y(CoolStruct * const);
void CoolStruct_get_msg(CoolStruct * const, char * const);
void CoolStruct_set_x(CoolStruct *, const int);
void CoolStruct_set_y(CoolStruct *, const int);
void CoolStruct_set_msg(CoolStruct *, const char * const);

CoolStruct * get_static_coolstruct(void);
void set_static_coolstruct(CoolStruct * const);

void cool_function(int, char, CoolStruct * const);

void set_x(const int);
int get_x(void);

void print_bignum(const char * const);

#endif
