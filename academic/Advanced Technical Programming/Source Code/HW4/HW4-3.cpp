// Created by Steven Jennings
// Date: 25 July 2016
//
// Write a program that allows the user to enter in multiple students
// (use the structure from problem 3 and create an array of students.)
// The program should then print the names and student number of all
// students with 128 credits or more.

#include "stdafx.h"

struct Student {
	int ID;
	char Lname[50];
	char Fname[50];
	double credits;
};

int _tmain(int argc, _TCHAR* argv[])
{
	printf("This program is a more complex version of the previous program.\n");
	printf("This program will create an array of 3 students, each with different properties.\n");
	printf("Then the program will print the student information of all students with 128 credits or more.\n");

	struct Student students[3];
	char temp[50];

	for (int i = 0; i < 3; ++i) {
		printf("\n");

		printf("Enter the ID number of student %d\n", i + 1);
		scanf("%d", &students[i].ID);

		printf("Enter the last name of student %d\n", i + 1);
		scanf("%s", students[i].Lname);

		printf("Enter the first name of student %d\n", i + 1);
		scanf("%s", students[i].Fname);

		printf("Enter the credit count of student %d\n", i + 1);
		scanf("%lf", &students[i].credits);
	}

	for (int i = 0; i < 3; ++i) {
		if (students[i].credits >= 128) {
			printf("\n");
			printf("Student %d Information\n", i + 1);
			printf("----------------------\n");
			printf("ID: %d\n", students[i].ID);
			printf("Name: %s, %s\n", students[i].Lname, students[i].Fname);
			printf("Credits Completed: %.3lf\n", students[i].credits);
		}
	}

	getchar();
	getchar();
}

