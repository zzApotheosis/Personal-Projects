// Created by Steven Jennings
// Date: 15 June 2016
//
// Write a program that prompts the user for an integer value
// and then tells the user if the number is prime or composite.

#include "stdafx.h"


int _tmain(int argc, _TCHAR* argv[])
{
	printf("This program accepts any positive integer from the user.\n");
	printf("The program then determines if the input integer is prime or composite.\n");
	printf("By definition, a prime number is any number whose factors is 1 and itself.\n");
	printf("Likewise, a composite number is any number whose factors include 1, itself, and anything else.\n\n");

	int input;
	int factors = 0;
	printf("Enter any positive integer.\n");
	scanf("%d", &input);

	for (int i = 1; i <= input; ++i) {
		if (input % i == 0) {
			++factors;
		}
	}

	if (factors == 2) {
		printf("\nYour number is a prime number.\n");
	}
	else {
		printf("\nYour number is a composite number.\n");
	}
	
	getchar();
	getchar();
}