#include <cstdlib>
#include <iostream>
#include <unistd.h>

#include <QRandomGenerator>

#include "game.hpp"

Game::Game() :
        QObject(),
        number(0),
        low_limit(0),
        high_limit(100),
        num_guesses(0)
{}

Game::~Game() {}

Game * Game::get_instance() {
        static Game * instance = nullptr;
        if (instance == nullptr)
                instance = new Game();
        return instance;
}

int Game::get_low_limit() {
        return low_limit;
}

int Game::get_high_limit() {
        return high_limit;
}

void Game::generate_number() {
        QRandomGenerator * rng = QRandomGenerator::global();
        number = rng->bounded(low_limit, high_limit);
}

void Game::guess(int guess) {
        num_guesses++;
        std::cout << "You have guessed " << num_guesses << " times" << std::endl;
        if (guess < number)
                emit guess_too_low();
        else if (guess > number)
                emit guess_too_high();
        else if (guess == number)
                emit guess_match(num_guesses);
        else
                throw std::runtime_error("How the fuck did you manage this?");
}
