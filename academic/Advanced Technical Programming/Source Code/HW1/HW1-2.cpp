// Created by Steven Jennings
// Date: 09 June 2016
//
// Slowly moving up the totem pole. This task includes everything from HW1-1,
// except user input is now included.
//
// Happy coding.

#include "stdafx.h"


int main()
{
	printf("This program will print a date in mm/dd/yyyy format based on user input.\n");
	printf("\n");
	
	int month;
	int day;
	int year;

	printf("Enter a month as an integer. (I.E. June = 6)\n");
	scanf("%d", &month);
	printf("Enter a day of the month as an integer.\n");
	scanf("%d", &day);
	printf("Enter a year by integer.\n");
	scanf("%d", &year);

	printf("Your date is %d/%d/%d\n", month, day, year);
	getchar();
	getchar();
}

