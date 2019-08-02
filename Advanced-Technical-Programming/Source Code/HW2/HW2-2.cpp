// Created by Steven Jennings
// Date: 15 June 2016
//
// Write a program that prompts the user for a positive integer
// and then displays all of the factors of that integer.
// For example: the factors of 12 are 1, 2, 3, 4, 6, 12

#include "stdafx.h"


int _tmain(int argc, _TCHAR* argv[])
{
	printf("This program accepts a positive integer from the user.\n");
	printf("The program then determines all factors of the integer,\n");
	printf("and outputs them to the screen.\n\n");
	printf("For example, inputting 10 would output 1, 2, 5, and 10 as factors of 10.\n\n");

	int input;
	printf("Enter any positive integer.\n");
	scanf("%d", &input);

	printf("\nBehold, the factors of %d!\n", input);

	for (int i = 1; i <= input; ++i) {
		if (input % i == 0) {
			printf("%d\n", i);
		}
	}

	getchar();
	getchar();
}
