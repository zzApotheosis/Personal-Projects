// Created by Steven Jennings
// Date: 16 June 2016
//
// Write a program that fills an array with random integers and then
// find the position of the smallest element in the array.
// It should also print the value of the smallest number.
//
// Assuming only positive integers.
//
// Also, I contacted JetBrains and they told me they offer all of their IDEs free of charge for students.
// So as a result, this is the first C++ program I've written on JetBrains' CLion IDE. Good stuff.
// Although it did take a little bit of hassle to set up Cygwin for the compiler.

#include "stdio.h"
#include "stdlib.h"
#include "time.h"
#include "limits.h"

void findSmallest(int a[], int size);

int main() {
    printf("This program fills an array with random [positive] integers and then\n");
    printf("finds the index of the smallest element in the array.\n");
    printf("Then it outputs the smallest integer in the array.\n\n");

    int arraySize;
    srand (time(NULL));
    printf("Enter the size of your array.\n");
    scanf("%d", &arraySize);
    int array[arraySize];

    for (int i = 0; i < arraySize; ++i) {
        array[i] = rand();
        printf("Array Index: %d --- Index Value: %d\n", i, array[i]);
    }

    printf("\n");

    findSmallest(array, arraySize);

    getchar();
    getchar();
}

void findSmallest(int a[], int size) {
    int smallestIndex;
    int smallestValue = INT_MAX;

    for (int i = 0; i < size; ++i) {
        if (a[i] < smallestValue) {
            smallestValue = a[i];
            smallestIndex = i;
        }
    }

    printf("The smallest value in the array is %d at Index %d.\n", smallestValue, smallestIndex);
}