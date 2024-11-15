// #include <stdlib.h>
#include <gpg-error.h>
#include <gpgme-64.h>
#include <stdio.h>
#include <unistd.h>
#include <gpgme.h>

#include <common-macros.h>

#include "gpgme-example.h"

#define GPGME_VERSION "1.15.1"

#define EXPECT_EQ(e, v) \
    do { \
        if (e != v) { \
            die("Assertion failed"); \
        } \
    } while (0)

static gpgme_ctx_t ctx = NULL;

static void check_version(void) {
    if (gpgme_check_version(GPGME_VERSION) == NULL) {
        die("gpgme_check_version() failed");
    }
}

void gpgme_example_init(void) {
    check_version();
    if (gpgme_new(&ctx) != GPG_ERR_NO_ERROR)
        die("gpgme_new() error");
}

void gpgme_example_deinit(void) {
    gpgme_release(ctx);
}

int listkeys(void) {
    gpgme_key_t key = NULL;
    gpgme_error_t e = 0;

    e = gpgme_op_keylist_start (ctx, NULL, 0);
    while (!e) {
        e = gpgme_op_keylist_next(ctx, &key);
        if (e)
            break;
        fprintf(stderr, "%s:", key->subkeys->keyid);
        if (key->uids && key->uids->name)
            fprintf(stderr, " %s", key->uids->name);
        if (key->uids && key->uids->email)
            fprintf(stderr, " <%s>", key->uids->email);
        fprintf(stderr, "\n");
        gpgme_key_release(key);
    }

    if (gpg_err_code(e) != GPG_ERR_EOF) {
        fprintf(stderr, "Cannot list keys: %s\n", gpgme_strerror(e));
        return 1;
    }

    return 0;
}
