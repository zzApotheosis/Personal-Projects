// Created by Steven Jennings
// Date: 30 June 2016
//
// Write a program that allows the user to enter values into an array.
// The program should then find the first pair of adjacent elements that are the same.
// Print out the value of the identical elements and the position of the first element that is a match.

#include <iostream>

using namespace std;

void search(int[]);

int main() {
    printf("This program will find the first pair of identical adjacent elements\n");
    printf("in an array that the user manually assigns values.\n");
    printf("\nEnter 10 integers of your choice.\n");
    printf("Obviously, try to enter in two consecutive identical values somewhere.\n\n");

    int array[10];

    for (int i = 0; i < 10; ++i) {
        printf("Index %d: ", i);
        scanf("%d", &array[i]);
    }

    search(array);

    getchar();
    getchar();
}

void search(int arr[]) {
    int found = 0;
    for (int i = 0; i < 10 - 1; ++i) {
        if (arr[i] == arr[i + 1]) {
            printf("Found adjacent identical values at indexes %d and %d.\n", i, i + 1);
            printf("The value at both of these indexes is %d.\n\n", arr[i]);
            found = 1;
            break;
        }
    }
    if (!found) {
        printf("Did not find any adjacent identical values.\n");
    }
}