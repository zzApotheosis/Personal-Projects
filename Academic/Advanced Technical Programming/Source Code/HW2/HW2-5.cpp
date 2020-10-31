// Created by Steven Jennings
// Date: 16 June 2016
//
// Write a program that plays a simple guessing game.
// The program should first generate a random number between 1 and 100.
// The program should then prompt the user for a guess.
// If the guess does not match the generated number a message
// should be printed that says “Too high” or “Too low” and the user
// should be given another chance to guess. If the guess is correct
// a message should be printed letting the user know they were correct.
// The total number of guesses they used should also be displayed.
// The program should terminate after the user guesses correctly.

#include "stdafx.h"
#include "stdlib.h"
#include "time.h"


int main()
{
	printf("Guess the number!\n\n");
	printf("This program generates a random number between 1 and 100 inclusive.\n");
	printf("It's your job to guess what that number is.\n");
	printf("You have unlimited attempts, but try to guess it with the fewest guesses possible!\n");
	printf("Try to guess it on the first try! That's only a 1%% chance,\n");
	printf("but still better than your chances with the lottery!\n\n");

	int answer = 0;
	int guess = 0;
	int counter = 0;
	
	srand (time(NULL)); //Had to Google for this line of code. It appears in C, rand() isn't enough for consistently random numbers, which is an oxymoron.
	answer = rand() % 100 + 1;

//	printf("Answer is %d\n\n", answer); //Debug info
	
	while (true) {
		++counter;
		printf("What's your guess?\n");
		scanf("%d", &guess);

		if (guess < answer) {
			printf("You guessed too low. Try again!\n\n");
		}
		else if (guess > answer) {
			printf("You guessed too high. Try again!\n\n");
		}
		else if (guess == answer) {
			if (counter == 1) {
				printf("You guessed the answer in 1 try! Go buy a lottery ticket!\n");
				break;
			}
			else {
				printf("You guessed the answer in %d tries!\n", counter);
				break;
			}
		}
		else {
			printf("You broke something...\n");
		}
	}

	getchar();
	getchar();
}