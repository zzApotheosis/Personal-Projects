#include <stdio.h>
#include <stdlib.h>

int sharedFunction() {
	int i = 1336;
	printf("This is a big pog inside libfoo!\n");
	return ++i;
}

