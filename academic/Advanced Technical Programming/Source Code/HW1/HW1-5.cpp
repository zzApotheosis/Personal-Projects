// Created by Steven Jennings
// Date: 09 June 2016
//
// The last task of this homework assignment is to get two integers from the user,
// and then evaluate which of the two integers is smaller.
// One thing comes to mind: If/Else statements.
//
// Wow, for once, I didn't procrastinate. This assignment is due on Wednesday, 15 June 2016.

#include "stdafx.h"


int main()
{
	printf("This program accepts two integers from the user,\n");
	printf("then determines which of the two input integers is the smallest.\n");

	printf("\n");

	int num1;
	int num2;

	printf("Enter the first integer x.\n");
	scanf("%d", &num1);
	printf("Enter the second integer y.\n");
	scanf("%d", &num2);

	printf("\n");
	printf("x = %d\n", num1);
	printf("y = %d\n", num2);
	printf("\n");

	if (num1 < num2) {
		printf("The smaller of the two integers is x (%d).\n", num1);
	}
	else if (num2 < num1) {
		printf("The smaller of the two integers is y (%d).\n", num2);
	}
	else if (num2 == num1) {
		printf("The two integers are equal. Neither are smaller.\n");
	}
	else {
		printf("Not sure what happened here...\n");
	}

	getchar();
	getchar();
}

