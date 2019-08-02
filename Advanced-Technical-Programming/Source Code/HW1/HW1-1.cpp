// Created by Steven Jennings
// Date: 09 June 2016
//
// Simple enough. The task was to print out, in a neat fashion, my schedule for the semester.
// The program only uses the printf() statement and getchar().
//
// Also, it's a bummer that there is no Community Edition for JetBrains' CLion IDE.
// I'd rather use JetBrains' software since IntelliJ IDEA is so brilliant.


#include "stdafx.h"


int main()
{
	printf("Steven Jennings' Summer 2016 Semester Schedule\n");
	printf("\n");
	printf("Course #     Name                                   Days      Time         \n");
	printf("---------------------------------------------------------------------------\n");
	printf("EET2350      Advanced Technical Programming         MW        12:15 - 14:45\n");
	printf("EET3740      Programmable Logic Controllers         MW        14:55 - 17:25\n");
	printf("\n");
	printf("And yes, I am taking the same classes as Jakeob Twogood!\n");
	printf("Also, go Broncos.");
	getchar();
}