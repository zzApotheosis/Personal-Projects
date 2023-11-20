#include <iostream>
#include <sstream>
#include <QMainWindow>
#include <QWidget>

#include "cipherhandler.hpp"

#include "mainwindow.hpp"
#include "ui_mainwindow.h"

MainWindow::MainWindow(QWidget * parent) :
        QMainWindow(parent),
        ui(new Ui::MainWindow)
{
        ui->setupUi(this);
        CipherHandler::initialize();
}

MainWindow::~MainWindow() { delete ui; }

bool MainWindow::passphrase_check() {
        return ui->passphrase_lineedit->text().size() > 0;
}

void MainWindow::on_actionReset_triggered() {
        ui->passphrase_lineedit->clear();
        ui->plaintext_textedit->clear();
        ui->ciphertext_textedit->clear();
}

void MainWindow::on_encrypt_button_clicked() {
        if (!this->passphrase_check()) {
                ui->statusbar->showMessage("Please enter a passphrase before using this tool");
                return;
        }

        if (ui->plaintext_textedit->toPlainText().size() == 0) {
                return;
        }
        
        std::string passphrase = ui->passphrase_lineedit->text().toStdString();
        ch.set_passphrase(passphrase);

        std::string plaintext_string = ui->plaintext_textedit->toPlainText().toStdString();

        std::vector<unsigned char> plaintext;
        for (int i = 0; i < plaintext_string.size(); i++) {
                plaintext.push_back(plaintext_string[i]);
        }
        std::vector<unsigned char> ciphertext = ch.encrypt(plaintext);
        QByteArray ciphertext_array;
        for (int i = 0; i < ciphertext.size(); i++) {
                ciphertext_array.append(ciphertext[i]);
        }
        QString ciphertext_string = QString(ciphertext_array.toBase64());
        ui->ciphertext_textedit->setPlainText(ciphertext_string);
}

void MainWindow::on_decrypt_button_clicked() {
        if (!this->passphrase_check()) {
                ui->statusbar->showMessage("Please enter a passphrase before using this tool");
                return;
        }

        if (ui->ciphertext_textedit->toPlainText().size() == 0) {
                return;
        }
        
        std::string passphrase = ui->passphrase_lineedit->text().toStdString();
        ch.set_passphrase(passphrase);

        QString ciphertext_string_base64 = ui->ciphertext_textedit->toPlainText();
        QByteArray ciphertext_base64_array;
        ciphertext_base64_array.append(ciphertext_string_base64.toUtf8());
        QByteArray ciphertext_array = QByteArray::fromBase64(ciphertext_base64_array);

        std::vector<unsigned char> ciphertext;
        for (int i = 0; i < ciphertext_array.size(); i++) {
                ciphertext.push_back(ciphertext_array[i]);
        }
        std::vector<unsigned char> plaintext = ch.decrypt(ciphertext);
        std::stringstream s;
        for (int i = 0; i < plaintext.size(); i++) {
                s << plaintext[i];
        }
        ui->plaintext_textedit->setPlainText(QString::fromStdString(s.str()));
}

void MainWindow::on_show_passphrase_checkbox_stateChanged(int state) {
        switch(state) {
        case Qt::Unchecked:
                ui->passphrase_lineedit->setEchoMode(QLineEdit::Password);
                break;
        case Qt::PartiallyChecked:
        case Qt::Checked:
                ui->passphrase_lineedit->setEchoMode(QLineEdit::Normal);
                break;
        default:
                // Do nothing? sheeeeit
                break;
        }
}

