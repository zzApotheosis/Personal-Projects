// Created by Steven Jennings
// Date: 27 July 2016
//
// Awesome semester. I still prefer Java, but it'll be nice to have C under my belt
// when employers want me! Thanks for the great semester.

#include <stdio.h>
#include <stdlib.h>

struct Item {
	int quantity;
	float cost;
	char partNum[10];
	bool InStock;
};

const int MAX = 100;

struct Item BOM[MAX];

void printItem(struct Item* pi);
void enterItem(struct Item* pi);
void createPO(int n);

void main(int argc, char* argv[])
{
	int count;
	int i;

	printf("How many items? ");
	scanf("%d", &count);
	
	for (i = 0; i < count; ++i) {
		enterItem(&BOM[i]);		
	}

	printf("\n\nPrinting BOM\n\n");
	for (i = 0; i < count; ++i) {
		printItem(&BOM[i]);
	}

	createPO(count); //Exam 2 - Part 2 (Programming) - Section 2- Problem e.

	getchar();
	getchar();
}

void printItem(struct Item* pi) {
	printf("Type: %d\n", pi->quantity);
	printf("Cost: $%.2f\n", pi->cost);
	printf("Part Number: %s\n", pi->partNum);
	if (pi->InStock == true) {
		printf("Item in stock.\n");
	} else {
		printf("Item not in stock.\n");
	}
	printf("\n\n");
}

void enterItem(struct Item* pi) {
	printf("\n");
	printf("Quantity? ");
	scanf("%d", &pi->quantity);
	printf("Cost? ");
	scanf("%f", &pi->cost);
	getchar();   //need to clear out the carriage return from typing in the cost
	printf("Part Number? ");
	gets(pi->partNum);
	printf("In stock? (0/1) ");
	scanf("%d", &pi->InStock);
}

void createPO(int n) {
	FILE *fp;
	fp = fopen("Purchase Order.txt", "w");
	fprintf(fp, "Part Number - Quantity - Cost - In Stock?\n");
	fprintf(fp, "================================================\n");
	for (int i = 0; i < n; ++i) {
		fprintf(fp, "%s - %d - $%.2f - ", BOM[i].partNum, BOM[i].quantity, BOM[i].cost);
		fprintf(fp, "%s\n", BOM[i].InStock ? "X" : "");
	}
	fclose(fp);
}