#include <stdlib.h>
#include <stdio.h>
#include <unistd.h>
#include <stdint.h>
#include <string.h>
#include <gcrypt.h>

int main(int argc, char ** argv) {
    gcry_error_t e;
    gcry_sexp_t key;
    gcry_sexp_t key_parms;
    gcry_sexp_t plaintext_sexp;
    gcry_sexp_t ciphertext_sexp;
    gcry_sexp_t decrypted_sexp;
    size_t erroff;
    unsigned char plaintext[16] = "Hello!";
    
    // Generate a keypair to use for this demonstration
    fprintf(stdout, "[*] GENERATING NEW KEYPAIR\n");
    e = gcry_sexp_build(&key_parms, &erroff, "(genkey(rsa(nbits 4:2048)))");
    e = gcry_pk_genkey(&key, key_parms);
    fprintf(stdout, "[*] KEYPAIR GENERATED (bit length = %u)\n", gcry_pk_get_nbits(key));
    //gcry_sexp_dump(ciphertext_sexp);
    
    // Place the plaintext data in the s-expression
    e = gcry_sexp_build(&plaintext_sexp, &erroff, "(data"/*(flags pkcs1)*/"(value %s))", plaintext);
    
    // Perform the encryption with the plaintext and key s-expressions
    fprintf(stdout, "[*] ENCRYPTING DATA\n");
    fprintf(stdout, "    Plaintext: %s\n", plaintext);
    e = gcry_pk_encrypt(&ciphertext_sexp, plaintext_sexp, key);
    fprintf(stdout, "[*] DATA ENCRYPTED\n");
    //gcry_sexp_dump(ciphertext_sexp);
    
    // Perform decryption
    fprintf(stdout, "[*] DECRYPTING DATA\n");
    e = gcry_pk_decrypt(&decrypted_sexp, ciphertext_sexp, key);
    fprintf(stdout, "[*] DATA DECRYPTED\n");
    //gcry_sexp_dump(decrypted_sexp);
    
    // Retrieve decrypted text from s-expression
    size_t buf_len;
    const unsigned char * buf = gcry_sexp_nth_data(decrypted_sexp, 0, &buf_len);
    fprintf(stdout, "    Decrypted: %s\n", buf);
    
    // Done
    return(EXIT_SUCCESS);
}
