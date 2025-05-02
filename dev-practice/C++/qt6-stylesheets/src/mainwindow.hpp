#ifndef MAINWINDOW_HPP
#define MAINWINDOW_HPP

#include <QMainWindow>

QT_BEGIN_NAMESPACE
namespace Ui {
        class MainWindow;
}
QT_END_NAMESPACE

class MainWindow : public QMainWindow {
        Q_OBJECT

public:
        MainWindow(QWidget *parent = nullptr);
        ~MainWindow();

private:
        Ui::MainWindow * ui;

private slots:
        void on_pushButton_blue_clicked(void);
        void on_pushButton_red_clicked(void);
};
#endif // MAINWINDOW_HPP
