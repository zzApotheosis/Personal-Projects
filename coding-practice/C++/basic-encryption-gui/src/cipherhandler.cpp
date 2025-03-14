#include <cstdlib>
#include <gcrypt.h>
#include <iostream>
#include <sstream>
#include <unistd.h>
#include <vector>
#include <cstring>
#include <limits>

#include <QObject>
#include <QRandomGenerator>

#include "cipherhandler.hpp"

#define NEED_LIBGCRYPT_VERSION "1.10.2"
#define SALT_SIZE 8
#define KEY_SIZE 32
#define BLOCK_SIZE 32
#define IV_SIZE BLOCK_SIZE
#define PLAINTEXT_IDENTIFIER "PLAINTEXT"

void dump_vector(const std::vector<unsigned char> & v) {
        char buffer[4];
        for (unsigned int i = 0; i < v.size(); i++) {
                std::snprintf(buffer, sizeof(buffer), "%02x", v[i]);
                std::cout << buffer << " ";
        }
        std::cout << std::endl;
}

bool CipherHandler::initialized = false;

void CipherHandler::initialize() {
        if (CipherHandler::initialized)
                return;
        if (!gcry_check_version(NEED_LIBGCRYPT_VERSION)) {
                std::cerr << "libgcrypt is too old (need "
                          << NEED_LIBGCRYPT_VERSION << ", have "
                          << gcry_check_version(0) << ")" << std::endl;
                exit(EXIT_FAILURE);
        }
        gcry_control(GCRYCTL_SUSPEND_SECMEM_WARN);
        gcry_control(GCRYCTL_INIT_SECMEM, 16384, 0);
        gcry_control(GCRYCTL_RESUME_SECMEM_WARN);
        gcry_control(GCRYCTL_INITIALIZATION_FINISHED);
        CipherHandler::initialized = true;
}

std::string CipherHandler::to_string(const std::vector<unsigned char> & v) {
        std::stringstream s;
        for (unsigned int i = 0; i < v.size(); i++) {
                s << v[i];
        }
        return s.str();
}

std::vector<unsigned char> CipherHandler::from_string(const std::string & s) {
        std::vector<unsigned char> v;
        for (unsigned int i = 0; i < s.size(); i++) {
                v.push_back(s[i]);
        }
        return v;
}

bool CipherHandler::payload_is_safe(const std::vector<unsigned char> & p) {
        // Check if the payload is the absolute bare minimum length
        if (p.size() <= sizeof(unsigned int)) {
                return false;
        }
        
        unsigned int l = 0;
        unsigned int index = 0;
        
        // The first 4 bytes is an unsigned integer describing the size of the packed salt
        for (unsigned int i = 0; i < sizeof(unsigned int); i++) {
                l << 8;
                l |= p[index++];
        }
        index += l;

        // Check if the current index is valid in the payload
        if (p.size() <= index) {
                return false;
        }
        
        // The next 4 bytes is an unsigned integer describing the size of the IV
        l = 0; // TODO: Figure out why the below for loop doesn't just cycle out all the bits normally, that is, figure out why this variable needs to be reset to 0
        for (unsigned int i = 0; i < sizeof(unsigned int); i++) {
                l << 8;
                l |= p[index++];
        }
        index += l;

        // Check if the current index is valid in the payload
        if (p.size() <= index) {
                return false;
        }

        // At this point, let's assume our payload is valid for decryption
        // I'm sure there are other checks we can perform, but this is just a basic
        // example.
        return true;
}

CipherHandler::CipherHandler() :
        QObject(),
        passphrase(""),
        salt(std::vector<unsigned char>()),
        key(std::vector<unsigned char>()),
        iv(std::vector<unsigned char>()),
        ch(0)
{
        gpg_error_t err = gcry_cipher_open(&this->ch, GCRY_CIPHER_AES256, GCRY_CIPHER_MODE_GCM, GCRY_CIPHER_SECURE);
        if (err) {
                std::cerr << "GOT ERROR ON gcry_cipher_open" << std::endl;
                exit(err);
        }
}

CipherHandler::~CipherHandler() {
        gcry_cipher_close(this->ch);
}

void CipherHandler::set_passphrase(const std::string & new_passphrase) {
        this->passphrase = new_passphrase;
}

void CipherHandler::set_salt(const std::vector<unsigned char> & new_salt) {
        this->salt = new_salt;
}

void CipherHandler::set_iv(const std::vector<unsigned char> & new_iv) {
        this->iv = new_iv;
}

void CipherHandler::generate_salt() {
        this->salt.clear();
        QRandomGenerator * rng = QRandomGenerator::global();
        for (int i = 0; i < SALT_SIZE; i++) {
                this->salt.push_back(rng->generate() % (std::numeric_limits<unsigned char>::max() + 1));
        }
}

void CipherHandler::generate_iv() {
        this->iv.clear();
        QRandomGenerator * rng = QRandomGenerator::global();
        for (int i = 0; i < IV_SIZE; i++) {
                this->iv.push_back(rng->generate() % (std::numeric_limits<unsigned char>::max() + 1));
        }
}

void CipherHandler::derive_key() {
        unsigned char key[KEY_SIZE];
        std::memset(key, 0, KEY_SIZE);
        unsigned char salt[SALT_SIZE];
        std::memset(salt, 0, SALT_SIZE);
        for (int i = 0; i < SALT_SIZE; i++) {
                salt[i] = this->salt[i]; // No guarantee that this doesn't illegally access memory in this->salt
        }
        gpg_error_t err = gcry_kdf_derive(this->passphrase.c_str(), this->passphrase.size(), GCRY_KDF_ITERSALTED_S2K, GCRY_MD_SHA3_512, salt, 8, 10, sizeof(key), key);
        if (err) {
                std::cerr << "GOT ERROR ON gcry_kdf_derive" << std::endl;
                exit(err);
        }
        for (int i = 0; i < KEY_SIZE; i++) {
                this->key.push_back(key[i]);
        }
        
        // Set the symmetric key
        err = gcry_cipher_setkey(this->ch, key, sizeof(key));
        if (err) {
                fprintf(stderr, "GOT ERROR ON gcry_cipher_setkey\n");
                exit(err);
        }
}

std::vector<unsigned char> CipherHandler::encrypt(const std::vector<unsigned char> & plaintext) {
        // Reset the cipher handle
        gpg_error_t err = gcry_cipher_reset(this->ch);
        if (err) {
                std::cerr << "GOT ERROR ON gcry_cipher_reset" << std::endl;
                exit(err);
        }

        // Generate new salt, IV, and derived key
        this->generate_salt();
        this->generate_iv();
        this->derive_key();

        // Set the Initialization Vector
        unsigned char ivbuffer[IV_SIZE];
        for (int i = 0; i < this->iv.size(); i++) {
                ivbuffer[i] = this->iv[i];
        }
        err = gcry_cipher_setiv(this->ch, ivbuffer, IV_SIZE);
        if (err) {
                std::cerr << "GOT ERROR ON gcry_cipher_setiv" << std::endl;
                exit(err);
        }

        // Pack the salt and IV into the ciphertext
        std::vector<unsigned char> ciphertext;
        for (int i = 0; i < sizeof(unsigned int); i++) {
                // The first 32 bits is an unsigned integer describing how many of the next N bytes are the salt
                ciphertext.push_back((SALT_SIZE >> (((sizeof(unsigned int) - 1) * 8) - 8 * i)) & 0xFF);
        }
        for (int i = 0; i < this->salt.size(); i++) {
                ciphertext.push_back(this->salt[i]);
        }
        for (int i = 0; i < sizeof(unsigned int); i++) {
                // The next 32 bits is an unsigned integer describing how many of the next N bytes are the IV
                ciphertext.push_back((IV_SIZE >> (((sizeof(unsigned int) - 1) * 8) - 8 * i)) & 0xFF);
        }
        for (int i = 0; i < this->iv.size(); i++) {
                ciphertext.push_back(this->iv[i]);
        }
        // The rest of the ciphertext vector is the ciphertext payload

        // The first block will always be the PLAINTEXT_IDENTIFIER string
        unsigned char plaintextbuffer[BLOCK_SIZE];
        unsigned char ciphertextbuffer[BLOCK_SIZE];
        int index = 0;
        int block_size = 0;
        std::memset(plaintextbuffer, 0, sizeof(plaintextbuffer));
        std::memset(ciphertextbuffer, 0, sizeof(ciphertextbuffer));
        
        // Prepend the PLAINTEXT_IDENTIFIER string
        const std::string plaintext_identifier(PLAINTEXT_IDENTIFIER);
        for (int i = 0; i < plaintext_identifier.size(); i++) {
                plaintextbuffer[i] = plaintext_identifier[i];
        }
        err = gcry_cipher_encrypt(this->ch, ciphertextbuffer, sizeof(ciphertextbuffer), plaintextbuffer, sizeof(plaintextbuffer));
        if (err) {
                std::cerr << "GOT ERROR ON gcry_cipher_encrypt" << std::endl;
                exit(err);
        }
        for (int i = 0; i < sizeof(ciphertextbuffer); i++) {
                ciphertext.push_back(ciphertextbuffer[i]);
        }


        // Encrypt the given plaintext data
        while (index < plaintext.size()) {
                // Reset buffers
                std::memset(plaintextbuffer, 0, sizeof(plaintextbuffer));
                std::memset(ciphertextbuffer, 0, sizeof(ciphertextbuffer));
                
                // Get the next block of data
                block_size = plaintext.size() - index;
                if (block_size > sizeof(plaintextbuffer))
                        block_size = sizeof(plaintextbuffer);
                for (int i = 0; i < block_size; i++) {
                        plaintextbuffer[i] = plaintext[index++];
                }

                // Perform encryption
                err = gcry_cipher_encrypt(this->ch, ciphertextbuffer, sizeof(ciphertextbuffer), plaintextbuffer, sizeof(plaintextbuffer));
                if (err) {
                        std::cerr << "GOT ERROR ON gcry_cipher_encrypt" << std::endl;
                        exit(err);
                }
                for (int i = 0; i < sizeof(ciphertextbuffer); i++) {
                        ciphertext.push_back(ciphertextbuffer[i]);
                }
        }

        return ciphertext;
}

std::vector<unsigned char> CipherHandler::decrypt(const std::vector<unsigned char> & ciphertext) {
        // Check if the input data is valid for decryption
        if (!CipherHandler::payload_is_safe(ciphertext)) {
                return {0};
        }

        // Reset the cipher handle
        gpg_error_t err = gcry_cipher_reset(this->ch);
        if (err) {
                std::cerr << "GOT ERROR ON gcry_cipher_reset" << std::endl;
                exit(err);
        }

        std::vector<unsigned char> byte_array;
        unsigned int l = 0;
        unsigned int index = 0;
        // Get the salt from the ciphertext
        // The first 4 bytes is an unsigned integer describing the size of the packed salt
        for (unsigned int i = 0; i < sizeof(unsigned int); i++) {
                l << 8;
                l |= ciphertext[index++];
        }
        for (unsigned int i = 0; i < l; i++) {
                byte_array.push_back(ciphertext[index++]);
        }
        this->set_salt(byte_array);
        
        // Get the IV from the ciphertext
        // The next 4 bytes is an unsigned integer describing the size of the IV
        byte_array.clear();
        l = 0; // TODO: Figure out why the below for loop doesn't just cycle out all the bits normally, that is, figure out why this variable needs to be reset to 0
        for (unsigned int i = 0; i < sizeof(unsigned int); i++) {
                l << 8;
                l |= ciphertext[index++];
        }
        for (unsigned int i = 0; i < l; i++) {
                byte_array.push_back(ciphertext[index++]);
        }
        this->set_iv(byte_array);
        byte_array.clear();

        // Derive the key based on the passphrase and the salt
        this->derive_key();
        
        // Set the initialization vector
        unsigned char ivbuffer[IV_SIZE];
        for (int i = 0; i < this->iv.size(); i++) {
                ivbuffer[i] = this->iv[i];
        }
        err = gcry_cipher_setiv(this->ch, ivbuffer, IV_SIZE);
        if (err) {
                std::cerr << "GOT ERROR ON gcry_cipher_setiv" << std::endl;
                exit(err);
        }

        // Decrypt the plaintext identifier string
        // This is a test designed to ensure the rest of the payload is nearly guarantee that the rest of the payload will be decrypted successfully
        std::vector<unsigned char> plaintext;
        unsigned char ciphertextbuffer[BLOCK_SIZE];
        unsigned char plaintextbuffer[BLOCK_SIZE];
        int block_size = 0;
        for (int i = 0; i < BLOCK_SIZE; i++) {
                ciphertextbuffer[i] = ciphertext[index++];
        }
        err = gcry_cipher_decrypt(this->ch, plaintextbuffer, sizeof(plaintextbuffer), ciphertextbuffer, sizeof(ciphertextbuffer));
        if (err) {
                std::cerr << "GOT ERROR ON gcry_cipher_decrypt" << std::endl;
                exit(err);
        }
        std::string test_string;
        for (unsigned int i = 0; i < sizeof(plaintextbuffer); i++) {
                if (plaintextbuffer[i] == '\0')
                        break;
                test_string += plaintextbuffer[i];
        }
        if (test_string.compare(std::string(PLAINTEXT_IDENTIFIER))) {
                emit decryption_failed();
                return std::vector<unsigned char>();
        }


        // The rest of the ciphertext is the payload to decrypt
        while (index < ciphertext.size()) {
                // Reset buffers
                std::memset(plaintextbuffer, 0, sizeof(plaintextbuffer));
                std::memset(ciphertextbuffer, 0, sizeof(ciphertextbuffer));
                
                // Get the next block of data
                block_size = ciphertext.size() - index;
                if (block_size > sizeof(ciphertextbuffer))
                        block_size = sizeof(ciphertextbuffer);
                for (int i = 0; i < block_size; i++) {
                        ciphertextbuffer[i] = ciphertext[index++];
                }

                // Perform decryption
                err = gcry_cipher_decrypt(this->ch, plaintextbuffer, sizeof(plaintextbuffer), ciphertextbuffer, sizeof(ciphertextbuffer));
                if (err) {
                        std::cerr << "GOT ERROR ON gcry_cipher_decrypt" << std::endl;
                        exit(err);
                }
                for (int i = 0; i < sizeof(plaintextbuffer); i++) {
                        plaintext.push_back(plaintextbuffer[i]);
                }
        }

        // Trim the trailing nul characters (this happens due to mismatch between BLOCK_SIZE and plaintext length)
        while (plaintext[plaintext.size() - 1] == '\0') {
                plaintext.pop_back();
        }

        return plaintext;
}
