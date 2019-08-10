// Created by Steven Jennings
// Date: 30 June 2016
//
// Write a function that calculates the hypotenuse of a right triangle given the two legs.
// The legs should be the parameters of the function and the length of the hypotenuse
// should be the returned value. Remember: a^2 + b^2 = c^2
// Use decimal numbers for the values.

#include <iostream>
#include <string.h>
#include <math.h>

using namespace std;

void explain();

void getLegs();

double pythagTheorem(double, double);

void calcPerim();

void calcArea();

void displayInfo();

double leg1;
double leg2;
double hypotenuse;
double perimeter;
double area;

int main() {
    explain();

    getLegs();
    hypotenuse = pythagTheorem(leg1, leg2); //Here's the function that meets assignment requirements.
    calcPerim();
    calcArea();
    displayInfo();

    getchar();
    getchar();
}

void explain() {
    printf("This program is a pythagorean theorem calculator.\n");
    printf("Given two legs of a right triangle,\n"
                   "this program can calculate the hypotenuse.\n");
    printf("This program will also calculate extra information about the triangle.\n\n");
}

void getLegs() {
    printf("Enter the length of the first leg.\n");
    scanf("%lf", &leg1);
    printf("Enter the length of the second leg.\n");
    scanf("%lf", &leg2);
}

double pythagTheorem(double a, double b) {
    return sqrt(a * a + b * b);
}

void calcPerim() {
    perimeter = leg1 + leg2 + hypotenuse;
}

void calcArea() {
    area = 0.5 * leg1 * leg2;
}

void displayInfo() {
    printf("Leg A: %.3lf\n", leg1);
    printf("Leg B: %.3lf\n", leg2);
    printf("Hypotenuse: %.3lf\n", hypotenuse);
    printf("Perimeter: %.3lf\n", perimeter);
    printf("Area: %.3lf\n", area);
}