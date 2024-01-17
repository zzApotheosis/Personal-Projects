#include <iostream>

#include "mainwindow.hpp"
#include "ui_mainwindow.h"

MainWindow::MainWindow(QWidget *parent) :
        QMainWindow(parent),
        ui(new Ui::MainWindow)
{
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
        qApp->setStyleSheet("QLabel { color: blue }");
}

void MainWindow::on_pushButton_red_clicked() {
        qApp->setStyleSheet("QLabel { color: red }");
}
