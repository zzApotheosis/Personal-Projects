#ifndef MAINWINDOW_HPP
#define MAINWINDOW_HPP

#include <QMainWindow>

#include "cipherhandler.hpp"

QT_BEGIN_NAMESPACE
namespace Ui {
class MainWindow;
}
QT_END_NAMESPACE

class MainWindow : public QMainWindow {
        Q_OBJECT

      public:
        MainWindow(QWidget * parent = nullptr);
        ~MainWindow();

      private:
        Ui::MainWindow * ui;
        CipherHandler ch;
        bool passphrase_check(void);

      private slots:
        void on_actionReset_triggered(void);
        void on_encrypt_button_clicked(void);
        void on_decrypt_button_clicked(void);
        void on_show_passphrase_checkbox_stateChanged(int);
};

#endif // MAINWINDOW_HPP
