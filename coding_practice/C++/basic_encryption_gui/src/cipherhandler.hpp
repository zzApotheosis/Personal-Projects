#ifndef CIPHERHANDLER_HPP
#define CIPHERHANDLER_HPP

#include <gcrypt.h>
#include <QObject>

class CipherHandler : public QObject {
        Q_OBJECT

      public:
        static void initialize(void);
        static std::string to_string(const std::vector<unsigned char> &);
        static std::vector<unsigned char> from_string(const std::string &);
        static bool payload_is_safe(const std::vector<unsigned char> &);
        CipherHandler(void);
        virtual ~CipherHandler(void);

        void set_passphrase(const std::string &);

        std::vector<unsigned char> encrypt(const std::vector<unsigned char> &);
        std::vector<unsigned char> decrypt(const std::vector<unsigned char> &);

      private:
        static bool initialized;
        std::string passphrase; // String to be used in key derivation function
        std::vector<unsigned char> salt;
        std::vector<unsigned char> key;
        std::vector<unsigned char> iv;
        gcry_cipher_hd_t ch;
        
        void set_salt(const std::vector<unsigned char> &);
        void set_iv(const std::vector<unsigned char> &);
        void generate_salt(void);
        void generate_iv(void);
        void derive_key(void);
};

#endif // CIPHERHANDLER_HPP
