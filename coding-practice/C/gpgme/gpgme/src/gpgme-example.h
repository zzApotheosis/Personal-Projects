#ifndef GPGME_EXAMPLE_H
#define GPGME_EXAMPLE_H

void before_each(void);
void after_each(void);

int listkeys(void);
int symm_encrypt(void);
int asym_encrypt(void);
int sign(void);
int verify(void);

#endif
