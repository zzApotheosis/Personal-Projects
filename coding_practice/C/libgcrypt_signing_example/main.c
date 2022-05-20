#include <stdlib.h>
#include <stdio.h>
#include <unistd.h>
#include <stdint.h>
#include <string.h>
#include <gcrypt.h>

void dump_sexp_to_file(const unsigned char f[], gcry_sexp_t s) {
    FILE * fh = NULL;
    size_t buf_len;
    unsigned char * buf;
    buf_len = gcry_sexp_sprint(s, GCRYSEXP_FMT_DEFAULT, NULL, 0);
    buf = (unsigned char *) malloc(buf_len * sizeof(unsigned char));
    gcry_sexp_sprint(s, GCRYSEXP_FMT_DEFAULT, buf, buf_len);
    fh = fopen(f, "w+b");
    if (fh != NULL) {
        fwrite(buf, sizeof(unsigned char), buf_len, fh);
        fclose(fh);
    } else {
        fprintf(stderr, "ERROR OPENING FILE HANDLE\n");
    }
    free(buf);
}

int main(int argc, char ** argv) {
    gcry_error_t e;
    gcry_sexp_t key_parms;
    gcry_sexp_t keypair;
    gcry_sexp_t priv_key;
    gcry_sexp_t pub_key;
    gcry_sexp_t data_sexp;
    gcry_sexp_t dummy_sexp;
    gcry_sexp_t signature_sexp;
    size_t erroff;
    unsigned char plaintext[16] = "HAHAHAHA ligma";
    unsigned char dummy_data[16] = "DUMMY LUL";
    unsigned char buf[4096];
    
    // Generate a keypair to use for this demonstration
    fprintf(stdout, "[*] GENERATING NEW KEYPAIR\n");
    e = gcry_sexp_build(&key_parms, &erroff, "(genkey(rsa(nbits 4:2048)))");
    e = gcry_pk_genkey(&keypair, key_parms);
    fprintf(stdout, "[*] KEYPAIR GENERATED (bit length = %u)\n", gcry_pk_get_nbits(keypair));
    //gcry_sexp_dump(keypair);

    // Retrieve the private key
    priv_key = gcry_sexp_find_token(keypair, "private-key", 11);
    //gcry_sexp_dump(priv_key);
    
    // Retrive the public key
    pub_key = gcry_sexp_find_token(keypair, "public-key", 10);
    //gcry_sexp_dump(pub_key);
    
    // Place the plaintext data in the s-expression
    //e = gcry_sexp_build(&plaintext_sexp, &erroff, "(data(value %s))", plaintext);// This was the example in the API but it's possible to just pass straight data
    gcry_sexp_build(&data_sexp, &erroff, "%s", plaintext);
    //gcry_sexp_dump(plaintext_sexp);
    
    // Generate signature of plaintext data
    fprintf(stdout, "[*] SIGNING PLAINTEXT DATA\n");
    fprintf(stdout, "    Plaintext: %s\n", plaintext);
    e = gcry_pk_sign(&signature_sexp, data_sexp, priv_key);
    //gcry_sexp_dump(signature_sexp);
    
    // Verify signature against public key
    fprintf(stdout, "[*] VERIFYING SIGNATURE\n");
    e = gcry_pk_verify(signature_sexp, data_sexp, pub_key);
    if (e == 0) {
        fprintf(stdout, "    Signature has been successfully verified against the public key!\n");
    } else {
        fprintf(stdout, "    Bro wtf how does this even happen?\n");
        abort();
    }

    // Try to verify the signature from the real data against some dummy data!
    fprintf(stdout, "[*] VERIFYING SIGNATURE AGAINST DUMMY DATA\n");
    gcry_sexp_build(&dummy_sexp, &erroff, dummy_data);
    e = gcry_pk_verify(signature_sexp, dummy_sexp, pub_key);
    if (e != 0) {
        fprintf(stdout, "    The signature for the real data does NOT match the dummy data. THIS IS EXPECTED!\n");
    } else {
        fprintf(stdout, "    Bro wtf how does this even happen?\n");
    }
    
    // Dump stuff to files
    fprintf(stdout, "[*] DUMPING FILES\n");
    dump_sexp_to_file("private.key", priv_key);
    dump_sexp_to_file("public.key", pub_key);
    dump_sexp_to_file("keypair.key", keypair);
    dump_sexp_to_file("data.txt", data_sexp);
    dump_sexp_to_file("dummy.txt", dummy_sexp);
    dump_sexp_to_file("signature.bin", signature_sexp);

    // Release memory
    gcry_sexp_release(key_parms);
    gcry_sexp_release(keypair);
    gcry_sexp_release(priv_key);
    gcry_sexp_release(pub_key);
    gcry_sexp_release(data_sexp);
    gcry_sexp_release(dummy_sexp);
    gcry_sexp_release(signature_sexp);
    
    // There's still a memory leak due to gcry_pk_genkey()
    
    // Done
    return(EXIT_SUCCESS);
}
