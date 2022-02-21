// Created by Steven Jennings
// Date: 30 June 2016
//
// Write a function that calculates the result of some engineering formula of your choice.
// You may choose the formula.  The function should accept 1 or more parameters and return a value.
// Make sure you put a comment at the beginning of the function to explain the formula and the parameters.
//
// Chosen formula: Resonance frequency

#include <iostream>
#include <math.h>

#define M_PI acos(-1.0)

using namespace std;

void explain();

double getCapacitance();

double getInductance();

double getResonanceHz(double, double);

double getResonanceRads(double, double);

void displayInfo();

int LScale;
int CScale;
double inductance;
double capacitance;
double resonanceHz;
double resonanceRads;

int main() {
    explain();

    printf("Enter the scale of capacitance as an integer.\n");
    printf("For example: Milli = 3, Micro = 6, etc.\n");
    scanf("%d", &CScale);
    printf("Enter the scale of inductance as an integer.\n");
    printf("For example: Milli = 3, Micro = 6, etc.\n");
    scanf("%d", &LScale);

    capacitance = getCapacitance();
    inductance = getInductance();
    resonanceRads = getResonanceRads(capacitance, inductance); //Here's the function that meets assignment requirements.
    resonanceHz = getResonanceHz(capacitance, inductance); //This one too.

    displayInfo();

    getchar();
    getchar();
}

void explain() {
    printf("This program calculates the resonance frequency of an RLC circuit.\n");
}

double getCapacitance() {
    int capScale = 1;
    for (int i = 0; i < CScale; ++i) {
        capScale *= 10;
//        printf("New capacitance scale = %d\n", capScale); //Debug info
    }

    printf("Enter in the capacitance as a double with respect to the scale you entered in earlier.\n");
    printf("For example, if you entered 6, you will enter the capacitance in micro Farads.\n");
    scanf("%lf", &capacitance);

    return capacitance / capScale;
}

double getInductance() {
    int indScale = 1;
    for (int i = 0; i < LScale; ++i) {
        indScale *= 10;
//        printf("New inductance scale = %d\n", indScale); //Debug info
    }

    printf("Enter in the inductance as a double with respect to the scale you entered in earlier.\n");
    printf("For example, if you entered 3, you will enter the inductance in milli Henries.\n");
    scanf("%lf", &inductance);

    return inductance / indScale;
}

double getResonanceHz(double C, double L) {
    return 1 / (sqrt(C * L));
}

double getResonanceRads(double C, double L) {
    return 1 / (2 * M_PI * sqrt(C * L));
}

void displayInfo() {
    printf("Capacitance: %.12lf Farads\n", capacitance);
    printf("Inductance: %.12lf Henries\n", inductance);
    printf("Resonance Frequency: %.3lf Radians/s\n", resonanceRads);
    printf("Resonance Frequency: %.3lf Hz\n", resonanceHz);
}