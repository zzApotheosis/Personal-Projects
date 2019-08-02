// Created by Steven Jennings
// Date: 30 June 2016
//
// Write a function that accepts an array of integers and an upper limit as its parameters
// and returns the average of all the elements with values less than or equal to the upper limit.
// For example: If the array contains {3, 8, 4, 12, 17, 11, 8, 9} and the upper limit is 10,
// the values that should be included in the average are 3, 8, 4, 8, 9
// since they are all less than or equal to 10.

#include <iostream>

using namespace std;

void explain();
void getArray();
void getLimit();
double getAverage(int);

int upperLimit;
int array[10];

int main() {
    explain();

    getArray();
    getLimit();

    double average = getAverage(upperLimit); //Here's the function that meets assignment requirements.

    printf("The average of all elements under limit %d is %.3lf.", upperLimit, average);

    getchar();
    getchar();
}

void explain() {
    printf("This program accepts an array of integers (Size 10) from the user.\n");
    printf("Then the program accepts an upper integer limit from the user.\n");
    printf("The program will then calculate the average of all values in the array\n"
                   "that are within the limit set by the user.\n\n");
}

void getArray() {
    printf("Enter values for the array.\n");
    for (int i = 0; i < 10; ++i) {
        printf("Index %d: ", i);
        scanf("%d", &array[i]);
    }
}

void getLimit() {
    printf("Next, set the upper limit for the average.\n");
    scanf("%d", &upperLimit);
}

double getAverage(int limit) {
    double sum = 0;
    int counter = 0;
    for (int i = 0; i < 10; ++i) {
        if (array[i] <= limit) {
            sum += array[i];
            ++counter;
        }
    }
    return sum/counter;
}