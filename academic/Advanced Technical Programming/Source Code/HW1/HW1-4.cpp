// Created by Steven Jennings
// Date: 09 June 2016
//
// For the fourth task, this program accepts any number of seconds from the user in an integer variable.
// The program then converts the amount of time given to hour:minute:second format.
// The example given in the assignment is this: 5000s = 1:23:20
//
// So far, so good. Haven't had any problems with this assignment. Having this Java background definitely helps.
// I can't believe my CS1 professor failed me. What the hell.
//
// I'm so accustomed to using Ctrl+/ to comment out a line because that's how it works in Eclipse and IntelliJ IDEA.
// What a bummer Microsoft hasn't implemented that!

#include "stdafx.h"


int main()
{
	printf("This program converts any number of seconds into hour:minute:second format.\n");
	printf("\n");
	
	int input;
	int seconds;
	int minutes;
	int hours;

	printf("Enter any number of seconds as an integer.\n");
	scanf("%d", &input);

	printf("\n");

	// Calculating values here.
	seconds = input % 60;
	minutes = input / 60;
	while (minutes >= 60) {
		minutes -= 60;
		//printf("Minutes = %d\n", minutes); //Debug info.
	}
	hours = input / (60 * 60);

	printf("Hours = %d\n", hours);
	printf("Minutes = %d\n", minutes);
	printf("Seconds = %d\n", seconds);
	// If "days" was part of the format, I would have formatted the hours further,
	// but since it's not part of the format, I let hours increase indefinitely.
	// Entering a value 86400 or greater will not format the output to include days.
	
	//printf("seconds = %d\n", seconds); //Debug info.
	//printf("minutes = %d\n", minutes); //Debug info.
	//printf("hours = %d\n", hours); //Debug info.

	printf("\n");
	printf("%d:%d:%d\n", hours, minutes, seconds);
	getchar();
	getchar();
}

