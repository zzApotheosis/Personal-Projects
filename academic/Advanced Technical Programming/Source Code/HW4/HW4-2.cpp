// Created by Steven Jennings
// Date: 25 July 2016
//
// Write a program that creates a structure (a C struct) to hold student information.
// The information should include Student number (let's use an int), Last Name, First Name,
// and Number of credits completed. The main program should allow the user to enter information
// on a student and then print out the information that was entered.

#include "stdafx.h"
#include <string.h>

struct Student {
	int ID;
	char Lname[50];
	char Fname[50];
	double credits;
};


int _tmain(int argc, _TCHAR* argv[])
{
	printf("This program allows the user to enter student information.\n");

	struct Student student;
	char temp[50];

	printf("Enter the student's ID number (integer).\n");
	scanf("%d", &student.ID);

	printf("Enter the student's last name.\n");
	scanf("%s", temp);
	strcpy(student.Lname, temp);
	
	printf("Enter the student's first name.\n");
	scanf("%s", temp);
	strcpy(student.Fname, temp);

	printf("Enter the student's credit count.\n");
	scanf("%lf", &student.credits);
	
	printf("\nStudent Information\n");
	printf("-------------------\n");
	printf("ID: %d\n", student.ID);
	printf("Name: %s, %s\n", student.Lname, student.Fname);
	printf("Credits Completed: %.3lf\n", student.credits);

	getchar();
	getchar();
}