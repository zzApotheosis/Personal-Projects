// Created by Steven Jennings
// Date: 16 June 2016
//
// Write a program that prompts the user for a password and then prints
// a welcome message if the typed password matches one that you have coded into your program.
// If the password does not match the program should continue to prompt until it does.
// The password can contain letters and numbers so you will want to use a string variable.

#include <iostream>
#include <cstring>

int main() {

    char password[256] = "GoBroncos2015";
    char attempt[256] = "";
    int match;

    while (true) {
        printf("Password\n");
        gets(attempt);

        match = strcmp(password, attempt); // Had to Google for this code (strcmp).

//        printf("%d", match); //Debug info

        if (match == 0) {
            printf("Welcome, Broncos fan.\n");
            break;
        } else {
            printf("Incorrect, you are not a Broncos fan.\n\n");
        }

//        printf("Password: %s\n", password); //Debug info
//        printf("Attempt: %s\n", attempt); //Debug info
    }

    getchar();
    getchar();
}