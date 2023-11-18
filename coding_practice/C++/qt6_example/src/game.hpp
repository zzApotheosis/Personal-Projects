#ifndef GAME_HPP
#define GAME_HPP

#include <QObject>

class Game: public QObject {
    Q_OBJECT

public:
    static Game * get_instance(void);
    void generate_number(void);
    void guess(int);
    int get_low_limit(void);
    int get_high_limit(void);

signals:
    void guess_too_low(void);
    void guess_too_high(void);
    void guess_match(int &);

private:
    Game(void);
    ~Game(void);
    int number;
    int low_limit;
    int high_limit;
    int num_guesses;
};

#endif // GAME_HPP
