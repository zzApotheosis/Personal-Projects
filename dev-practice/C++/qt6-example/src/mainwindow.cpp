#include <iostream>

#include "game.hpp"

#include "mainwindow.hpp"
#include "ui_mainwindow.h"

MainWindow::MainWindow(QWidget *parent) :
        QMainWindow(parent),
        ui(new Ui::MainWindow)
{
        Game * game = Game::get_instance();
        QString prompt = "Let's play a game! Try and guess the number between " + QString::number(game->get_low_limit()) + " and " + QString::number(game->get_high_limit()) + ".";
        ui->setupUi(this);
        ui->label->setText(prompt);
        QObject::connect(ui->lineEdit, SIGNAL(returnPressed(void)), this, SLOT(lineEditTriggered(void)));
        QObject::connect(ui->pushButton, SIGNAL(pressed(void)), this, SLOT(lineEditTriggered(void)));
        QObject::connect(game, SIGNAL(guess_too_low(void)), this, SLOT(guess_too_low(void)));
        QObject::connect(game, SIGNAL(guess_too_high(void)), this, SLOT(guess_too_high(void)));
        QObject::connect(game, SIGNAL(guess_match(int &)), this, SLOT(guess_match(int &)));
}

MainWindow::~MainWindow() {
        Game * game = Game::get_instance();
        QObject::disconnect(ui->lineEdit, SIGNAL(returnPressed(void)), this, SLOT(lineEditTriggered(void)));
        QObject::disconnect(ui->pushButton, SIGNAL(pressed(void)), this, SLOT(lineEditTriggered(void)));
        QObject::disconnect(game, SIGNAL(guess_too_low(void)), this, SLOT(guess_too_low(void)));
        QObject::disconnect(game, SIGNAL(guess_too_high(void)), this, SLOT(guess_too_high(void)));
        QObject::disconnect(game, SIGNAL(guess_match(int &)), this, SLOT(guess_match(int &)));
        delete ui;
}

void MainWindow::lineEditTriggered() {
        Game * game = Game::get_instance();
        int guess = ui->lineEdit->text().toInt();
        ui->lineEdit->clear();
        game->guess(guess);
}

void MainWindow::guess_too_low() {
        ui->statusbar->showMessage("Too low... try again.");
}

void MainWindow::guess_too_high() {
        ui->statusbar->showMessage("Too high... try again.");
}

void MainWindow::guess_match(int & n) {
        ui->statusbar->showMessage("You got it in " + QString::number(n) + " tries!");
}
