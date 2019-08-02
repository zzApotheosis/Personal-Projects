// Created by Steven Jennings
// Date: 15 June 2016
//
// Write a program that prompts the user for a positive integer
// and then prints all the multiples of 8 that do not exceed the
// entered number. In other words, count by 8's up to the number.
// For example: if 20 is entered the program should output 0, 8, 16

#include "stdafx.h"


int _tmain(int argc, _TCHAR* argv[])
{
	printf("This program accepts a positive integer from the user.\n");
	printf("The program will then count in eights until it gets to the value\n");
	printf("at which the next count will exceed the input integer.\n\n");
	printf("For example, inputting 20 will have the program count 0, 8, 16 without exceeding 20.\n\n");

	int input;
	int start = 0;
	printf("Input any integer.\n");
	scanf("%d", &input);

	while (start <= input) {
		printf("%d\n", start);
		start += 8;
	}

	printf("As you can see, the program started at 0 and counted in eights to %d\n", start - 8);
	printf("such that the counter did not exceed %d.", input);

	getchar();
	getchar();
}