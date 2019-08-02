// BOM.cpp : Defines the entry point for the console application.
//

#include <stdio.h>
#include <stdlib.h>

struct Item {
	int quantity;
	float cost;
	char partNum[10];
};

const int MAX = 100;

struct Item BOM[MAX];

void printItem(struct Item* pi);
void enterItem(struct Item* pi);

void main(int argc, char* argv[])
{
	int count;
	int i;

	printf("How many items? ");
	scanf("%d", &count);
	
	for (i = 0; i < count; ++i) {
		enterItem(&BOM[i]);		
	}

	printf("\n\nPrinting BOM\n");
	for (i = 0; i < count; ++i) {
		printItem(&BOM[i]);
	}

	getchar();
}

void printItem(struct Item* pi) {
	printf("Type: %d\n", pi->quantity);
	printf("Cost: $%.2f\n", pi->cost);
	printf("Part # %s\n", pi->partNum);
	printf("\n\n");
}

void enterItem(struct Item* pi) {
	printf("Quantity? ");
	scanf("%d", &pi->quantity);
	printf("Cost? ");
	scanf("%f", &pi->cost);
	getchar();   //need to clear out the carriage return from typing in the cost
	printf("Part Number? ");
	gets(pi->partNum);
}


