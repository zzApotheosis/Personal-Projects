// #include <stdlib.h>
#include <gpg-error.h>
#include <gpgme-64.h>
#include <stdio.h>
#include <string.h>
#include <unistd.h>
#include <gpgme.h>

#include <common-macros.h>

#include "gpgme-example.h"

#define GPGME_MIN_VERSION "1.15.1"

static gpgme_ctx_t ctx = NULL;

static void check_version(void) {
    if (gpgme_check_version(GPGME_MIN_VERSION) == NULL) {
        die("gpgme_check_version() failed");
    }
}

static void gpgme_example_init(void) {
    check_version();
    if (gpgme_new(&ctx) != GPG_ERR_NO_ERROR)
        die("gpgme_new() error");
}

static void gpgme_example_deinit(void) {
    gpgme_release(ctx);
}

void before_each(void) {
    gpgme_example_init();
}

void after_each(void) {
    gpgme_example_deinit();
}

int listkeys(void) {
    fprintf(stderr, "\nRunning %s()\n", __func__);
    gpgme_key_t key = NULL;
    gpgme_error_t e = 0;

    fprintf(stderr, "ALL KEYS:\n");
    e = gpgme_op_keylist_start(ctx, NULL, 0);
    for (;!e;) {
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

    fprintf(stderr, "SECRET KEYS:\n");
    e = gpgme_op_keylist_start(ctx, NULL, 1);
    for (;!e;) {
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

int symm_encrypt(void) {
    fprintf(stderr, "\nRunning %s()\n", __func__);
    const char plaintext_string[] = "Super cool data bro\n";
    const char ciphertext_filename[] = "symmetric-ciphertext.gpg";
    gpgme_error_t status = 0;
    
    gpgme_data_t ciphertext_file = NULL;
    gpgme_data_new(&ciphertext_file);
    gpgme_data_set_file_name(ciphertext_file, ciphertext_filename);

    gpgme_data_t plaintext = NULL;
    status = gpgme_data_new_from_mem(&plaintext, plaintext_string, sizeof(plaintext_string), 0);
    if (status != GPG_ERR_NO_ERROR) {
        fprintf(stderr, "error: %s\n", gpgme_strerror(status));
        die("NOT GOOD");
    }
    
    gpgme_data_t ciphertext = NULL;
    gpgme_data_new(&ciphertext);

    // Encryption operation
    status = gpgme_op_encrypt(ctx, NULL, GPGME_ENCRYPT_SYMMETRIC, plaintext, ciphertext);
    if (status != GPG_ERR_NO_ERROR) {
        fprintf(stderr, "error: %s\n", gpgme_strerror(status));
        die("NOT GOOD");
    }
    gpgme_data_seek(ciphertext, 0, SEEK_SET);
    
    char ciphertext_buffer[32];
    FILE * fh = fopen(ciphertext_filename, "w+b");
    if (fh == NULL)
        die("NOT GOOD");
    ssize_t data_result = 0;
ciphertext_read:
    memset(ciphertext_buffer, 0, sizeof(ciphertext_buffer));
    if ((data_result = gpgme_data_read(ciphertext, ciphertext_buffer, sizeof(ciphertext_buffer))) > 0) {
        fwrite(ciphertext_buffer, data_result, 1, fh);
        goto ciphertext_read;
    } else if (data_result < 0) {
        fclose(fh);
        die("NOT GOOD");
    }
    fclose(fh);

    
    gpgme_data_release(ciphertext);
    gpgme_data_release(plaintext);
    gpgme_data_release(ciphertext_file);
    return 0;
}

int asym_encrypt(void) {
    fprintf(stderr, "\nRunning %s()\n", __func__);
    // TODO
    const char filename[] = "asymmetric-ciphtext.gpg";
    fprintf(stderr, "TODO\n");
    return 0;
}

int sign(void) {
    fprintf(stderr, "\nRunning %s()\n", __func__);
    // TODO
    fprintf(stderr, "TODO\n");
    return 0;
}

int verify(void) {
    fprintf(stderr, "\nRunning %s()\n", __func__);
    // TODO
    fprintf(stderr, "TODO\n");
    return 0;
}
