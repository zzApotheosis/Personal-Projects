#include "cipherhandler.hpp"
#include "mainwindow.hpp"

#include <QApplication>

int main(int argc, char *argv[]) {
        QApplication app(argc, argv);

        MainWindow w;
        w.show();

        return app.exec();
}
