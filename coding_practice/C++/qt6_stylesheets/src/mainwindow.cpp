#include <iostream>

#include "mainwindow.hpp"
#include "ui_mainwindow.h"

static QString blue_css =
        "QPushButton { "
        "       background-color: white;"
        "       border-style: outset;"
        "       border-width: 2px;"
        "       border-radius: 10px;"
        "       border-color: blue;"
        "       font: bold 14px;"
        "       min-width: 10em;"
        "       padding: 6px;"
        "}"
        ""
        "QPushButton:pressed {"
        "       background-color: #c0c0ff;"
        "}"
        ""
        "QPushButton:hover:!pressed {"
        "       background-color: qlineargradient(x1: 0, y1: 0, x2: 0, y2: 1, stop: 0 #ffffff, stop: 1 #c0c0c0);"
        "}"
        ""
        "QLabel {"
        "       color: blue;"
        "}";

static QString red_css =
        "QPushButton { "
        "       background-color: white;"
        "       border-style: outset;"
        "       border-width: 2px;"
        "       border-radius: 10px;"
        "       border-color: red;"
        "       font: bold 14px;"
        "       min-width: 10em;"
        "       padding: 6px;"
        "}"
        ""
        "QPushButton:pressed {"
        "       background-color: #ffc0c0;"
        "}"
        ""
        "QPushButton:hover:!pressed {"
        "       background-color: qlineargradient(x1: 0, y1: 0, x2: 0, y2: 1, stop: 0 #ffffff, stop: 1 #c0c0c0);"
        "}"
        ""
        "QLabel {"
        "       color: red;"
        "}";


MainWindow::MainWindow(QWidget *parent) :
        QMainWindow(parent),
        ui(new Ui::MainWindow)
{
        qApp->setStyleSheet(blue_css);
        ui->setupUi(this);
}

MainWindow::~MainWindow() {
        delete ui;
}

/*
 * NOTE: The Qt build system is smart enough to know that slot functions declared in the format
 *       on_OBJECT_SIGNAL() should be connected to the appropriate signal specified in the function
 *       declaration. That's why these functions don't need to be explicitly connected to their
 *       signals with QObject::connect().
 */
void MainWindow::on_pushButton_blue_clicked() {
        qApp->setStyleSheet(blue_css);
}

void MainWindow::on_pushButton_red_clicked() {
        qApp->setStyleSheet(red_css);
}
