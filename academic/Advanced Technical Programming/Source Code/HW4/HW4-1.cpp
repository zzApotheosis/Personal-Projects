// Created by Steven Jennings
// Date: 18 July 2016
//
// Write a program that simulates rolling a single six sided die.
// Find the expected number of rolls (average) required to roll four 3’s in a row.

#include <iostream>
#include <time.h>

int roll();

int counter = 0;
//int temp = 0; //Debug info

int main()
{
	printf("This program simulates a six-sided die and\ndetermines the expected number of rolls (average) required to roll four 3’s in a row.\n");
	printf("The program will roll a six-sided die until it has rolled four 3's in a row n times, where n is given by the user.\n\n");
	
	int input;
	printf("Enter an integer for n.\n");
	scanf("%d", &input);

	int maincounter = 0;
	int roll1;
	int roll2;
	int roll3;
	int roll4;
	double average;

	srand(time(0));

	while (maincounter < input) {
		roll1 = roll();
		if (roll1 == 3) {
			roll2 = roll();
			if (roll2 == 3) {
				roll3 = roll();
				if (roll3 == 3) {
					roll4 = roll();
					if (roll4 == 3) {
						//printf("Rolled four 3's in a row in %d rolls.\n", temp); //Debug info
						//temp = 0; //Debug info
						++maincounter;
					}
				}
			}
		}
	}

	average = (double) counter / maincounter;

	printf("The average number of rolls to get four 3's in a row %d times is %lf", input, average);

	getchar();
	getchar();
	
}

int roll() {
	++counter;
	//++temp; //Debug info
	return rand() % 6 + 1; 
}
