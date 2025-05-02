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
    void lineEditTriggered(void);
    void guess_too_low(void);
    void guess_too_high(void);
    void guess_match(int &);
};
#endif // MAINWINDOW_HPP
