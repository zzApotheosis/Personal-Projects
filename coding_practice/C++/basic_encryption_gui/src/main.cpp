#include <vector>
#include <iostream>

#include "cipherhandler.hpp"
#include "mainwindow.hpp"

#include <QApplication>

int main(int argc, char *argv[]) {
        QApplication app(argc, argv);

        CipherHandler::initialize();
        CipherHandler cipherhandler;
        cipherhandler.set_passphrase("test passphrase");
        
        std::vector<unsigned char> plaintext = CipherHandler::from_string("LIGMA BALLS");
        std::vector<unsigned char> ciphertext = cipherhandler.encrypt(plaintext);

        //cipherhandler.set_passphrase("other passphrase");

        std::vector<unsigned char> decrypted = cipherhandler.decrypt(ciphertext);
        std::string decrypted_string = CipherHandler::to_string(decrypted);
        std::cout << decrypted_string << std::endl;

        MainWindow w;
        w.show();

        return app.exec();
}
