// Created by Steven Jennings
// Date: 15 June 2016
//
// Write a program that prompts the user for the value of a resistor (call it R1.)
// The program should then prompt the user for a range of values and a step for a second resistor (R2.)
// For example: The user might enter 150 for R1 and a range of 50 to 100 in steps of 5 for R2.
// The program should then print out all the possible series and parallel resistance combinations for R1 and each R2.
// Format your output in a readable manner.

#include "stdafx.h"

void calculateSeries(double R1, double R2);
void calculateParallel(double R1, double R2);

int main()
{
	printf("This program does some magic with resistors.\n\n");
	
	double R1;
	double R2lower;
	double R2upper;
	int step;
	
	printf("Enter the value for R1.\n");
	scanf("%lf", &R1);
	printf("Enter the lower limit of R2.\n");
	scanf("%lf", &R2lower);
	printf("Enter the upper limit of R2.\n");
	scanf("%lf", &R2upper);
	printf("Enter the step size.\n");
	scanf("%d", &step);

	if (step <= 0) {
		printf("Step size cannot be less than 1. Setting it to 5.");
		step = 5;
	}

	printf("\nR1 = %lf\n", R1);
	printf("R2 lower = %lf\n", R2lower);
	printf("R2 upper = %lf\n", R2upper);
	printf("Step size = %d\n\n", step);

	printf("Press enter to run calculations.\n");

	getchar();
	getchar();

	printf("----------------------------------------------------\n\n");

	while (R2lower <= R2upper) {
		printf("For R2 = %lf\n", R2lower);
		calculateSeries(R1, R2lower);
		calculateParallel(R1, R2lower);
		printf("\n");
		R2lower += step;
	}

	getchar();
	getchar();

}

void calculateSeries(double R1, double R2) {
	double Req = R1 + R2;
	printf("Series: R1 + R2 = %lf\n", Req);
}

void calculateParallel(double R1, double R2) {
	double Req = 1 / ((1 / R1) + (1 / R2));
	printf("Parallel: R1 || R2 = %lf\n", Req);
}