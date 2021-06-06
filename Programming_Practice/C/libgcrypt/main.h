#ifndef MAIN_H
#define MAIN_H

#define NEED_LIBGCRYPT_VERSION "1.9.3"

gcry_error_t init_gcry(void);
gcry_error_t version_check(void);
int printhex(void*, int);

#endif

