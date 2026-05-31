#ifndef FOO_H
#define FOO_H

#ifdef __cplusplus
extern "C" {
#endif
void library_function(const int);
int do_app(int, char *[]);
#ifdef __cplusplus
}
#endif

#endif
