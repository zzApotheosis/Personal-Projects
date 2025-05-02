#include <stdio.h>
#include <stdlib.h>
#include "foo.h"

int main(int argc, char* argv[]) {
	int exitCode = 0;
	printf("This is inside main!\n");
	sharedFunction();
	return exitCode;
}

