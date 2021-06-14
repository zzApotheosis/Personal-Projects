// Created by Steven Jennings
// Date: 30 June 2016
//
// Write a program that prompts the user for a string and then tests to see if it is a palindrome.
// The program should print a message based on the results.
// (A palindrome is a word or phrase that is spelled the same forwards as backwards.
// For example “racecar” is a palindrome.)

#include <iostream>
#include <string.h>

using namespace std;

bool determinePalindrome(char[]);

int main() {
    printf("This program accepts a string from the user,"
                   "\nthen determines whether or not the given string is a palindrome.\n");

    char input[256];
    printf("Enter any string under 256 characters.\n");
    scanf("%s", input);

    bool palindrome = determinePalindrome(input);

    if (!palindrome) {
        printf("Your input \"%s\" is not a palindrome.\n", input);
    } else {
        printf("Your input \"%s\" is a palindrome.\n", input);
    }

    getchar();
    getchar();
}

bool determinePalindrome(char in[]) {
    bool valid = true;
    for (int i = 0; i < strlen(in); ++i) {
        if (in[i] != in[strlen(in) - 1 - i]) {
            valid = false;
            break;
        }
    }
    return valid;
}