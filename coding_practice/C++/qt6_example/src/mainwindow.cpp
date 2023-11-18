#include <iostream>

#include "game.hpp"

#include "mainwindow.hpp"
#include "ui_mainwindow.h"

MainWindow::MainWindow(QWidget *parent)
    : QMainWindow(parent)
    , ui(new Ui::MainWindow)
{
    Game * game = Game::get_instance();
    QString prompt = "Let's play a game! Try and guess the number between " + QString::number(game->get_low_limit()) + " and " + QString::number(game->get_high_limit()) + ".";
    ui->setupUi(this);
    ui->label->setText(prompt);
    QObject::connect(ui->lineEdit, &QLineEdit::returnPressed, this, &MainWindow::lineEditTriggered);
    QObject::connect(ui->pushButton, &QPushButton::pressed, this, &MainWindow::lineEditTriggered);
    QObject::connect(game, &Game::guess_too_low, this, &MainWindow::guess_too_low);
    QObject::connect(game, &Game::guess_too_high, this, &MainWindow::guess_too_high);
    QObject::connect(game, &Game::guess_match, this, &MainWindow::guess_match);
}

MainWindow::~MainWindow()
{
    Game * game = Game::get_instance();
    QObject::disconnect(ui->lineEdit, &QLineEdit::returnPressed, this, &MainWindow::lineEditTriggered);
    QObject::disconnect(ui->pushButton, &QPushButton::pressed, this, &MainWindow::lineEditTriggered);
    QObject::disconnect(game, &Game::guess_too_low, this, &MainWindow::guess_too_low);
    QObject::disconnect(game, &Game::guess_too_high, this, &MainWindow::guess_too_high);
    QObject::disconnect(game, &Game::guess_match, this, &MainWindow::guess_match);
    delete ui;
}

void MainWindow::lineEditTriggered()
{
    Game * game = Game::get_instance();
    int guess = ui->lineEdit->text().toInt();
    ui->lineEdit->clear();
    game->guess(guess);
}

void MainWindow::guess_too_low()
{
    ui->statusbar->showMessage("Too low... try again.");
}

void MainWindow::guess_too_high()
{
    ui->statusbar->showMessage("Too high... try again.");
}

void MainWindow::guess_match(int & n)
{
    ui->statusbar->showMessage("You got it in " + QString::number(n) + " tries!");
}
