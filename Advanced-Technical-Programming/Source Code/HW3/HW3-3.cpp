// Created by Steven Jennings
// Date: 30 June 2016
//
// Write a program that prompts the user for a string and then counts
// all the vowels in the string. The program should print out the count.

#include <iostream>

using namespace std;

int main() {
    printf("This program will count all the vowels in a string that the user inputs.\n");

    char string[64];

    printf("Tell me how awesome the Broncos are in under 64 characters!\n");
    gets(string);

    int counter1 = 0;
    int counter2 = 0;
    int hasY = 0;
    for (int i = 0; i < 64; ++i) {
        if (string[i] == 'a' || string[i] == 'e' || string[i] == 'i' || string[i] == 'o' || string[i] == 'u') {
            ++counter1;
        }
        if (string[i] == 'A' || string[i] == 'E' || string[i] == 'I' || string[i] == 'O' || string[i] == 'U') {
            ++counter1;
        }
        if (string[i] == 'y' || string[i] == 'Y') {
            hasY = 1;
            ++counter2;
        }
    }

    if (hasY == 0) {
        printf("Number of vowels in your message: %d", counter1);
    } else {
        printf("Number of vowels in your message: %d\n%d if you count \"y\" as a vowel.\n", counter1, counter1 + counter2);
    }

    getchar();
    getchar();
}