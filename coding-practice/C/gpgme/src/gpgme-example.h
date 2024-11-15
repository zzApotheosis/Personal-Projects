#ifndef GPGME_EXAMPLE_H
#define GPGME_EXAMPLE_H

void gpgme_example_init(void);
void gpgme_example_deinit(void);

int listkeys(void);
int symm_encrypt(void);
int asym_encrypt(void);
int sign(void);
int verify(void);

#endif
