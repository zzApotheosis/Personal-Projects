#include "mainwindow.hpp"
#include "game.hpp"

#include <QApplication>

int main(int argc, char *argv[])
{
    QApplication app(argc, argv);

    Game * game = Game::get_instance();
    game->generate_number();

    MainWindow w;
    w.show();

    return app.exec();
}
