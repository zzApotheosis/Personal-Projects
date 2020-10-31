// Created by Steven Jennings
// Date: 17 June 2016
//
// Write a program that finds the 2016th prime number.
// This problem is optional and not required for full credit.

#include <iostream>
//#include <unistd.h>

using namespace std;

int determineProperGrammar(int num);

int main() {
    printf("This program will find the nth prime number.\n");
    printf("Be warned, exceptionally large values will take more time to complete.\n");
    printf("Try entering 2016.\n");

    int n;
    int counter = 0;
    int answer;
    int analyze = 0;
    int factors = 0;

    scanf("%d", &n);

    if (n < 0) {
        printf("Invalid input. Setting to 0.\n\n");
        n = 0;
    }

    while (true) {
        ++analyze;

        for (int i = 1; i <= analyze; ++i) {
            if (analyze % i == 0) {
                ++factors;
            }
        }

        if (factors == 2) {
            ++counter;
        }

        factors = 0;

        if (counter == n) {
            answer = analyze;
            break;
        }
    }

    if (determineProperGrammar(counter) == 1) {
        printf("The %dst prime number is %d.\n", n, answer);
    } else if (determineProperGrammar(counter) == 2) {
        printf("The %dnd prime number is %d.\n", n, answer);
    } else if (determineProperGrammar(counter) == 3) {
        printf("The %drd prime number is %d.\n", n, answer);
    } else if (determineProperGrammar(counter) == 4) {
        printf("The %dth prime number is %d.\n", n, answer);
    } else {
        printf("The %dth prime number is %d.\n", n, answer);
    }

    getchar();
    getchar();
}

int determineProperGrammar(int num) {
    int initial = num;

    if (initial > 100) {
        while (initial > 100) {
            initial -= 100;
        }
    }

    if (initial == 11 || initial == 12 || initial == 13) {
        return 4;
    }

    while (initial > 10) {
        initial -= 10;
    }

    return initial;
}