// Created by Steven Jennings
// Date: 29 June 2016
//
// Write a program that fills an array with random integers and then prints the array.
// The program should then reverse the elements in the array and print it again.
// Make sure you reverse the array in-place. Don’t create a second array and don’t just print it in reverse.
//
// And just a reminder, this was written on JetBrains' (Awesome) CLion instead of Visual Studio.

#include <iostream>

using namespace std;

void reverseArray(int[]);

int array[10];

int main() {
    printf("This program fills an array of size 10 with random integers and then prints the array.\n");
    printf("Then it reverses the original array and prints the array again.\n");
    printf("Press enter to execute the program.\n");

    getchar();

    srand(time(NULL));

    for (int i = 0; i < 10; ++i) {
        array[i] = rand();
        printf("%d: %d\n", i, array[i]);
    }

    reverseArray(array);

    printf("\n");
    for (int i = 0; i < 10; ++i) {
        printf("%d: %d\n", i, array[i]);
    }

    getchar();
    getchar();
}

void reverseArray(int arr[]) {
    int temp;
    for (int i = 0; i < 10/2; ++i) {
        temp = arr[i];
        arr[i] = arr[9 - i];
        arr[9 - i] = temp;
    }
}