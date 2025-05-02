#include <stdlib.h>
#include <stdio.h>
#include <unistd.h>
#include <string.h>
#include <sys/wait.h>

void child_main(int argc, char * argv[]) {
    fprintf(stdout, "This is the child process!\n");
    exit(EXIT_SUCCESS);
}

void parent_main(int argc, char * argv[]) {
    sleep(1);
    fprintf(stdout, "This is the parent process!\n");
}

int main(int argc, char ** argv) {
    int cpid = 0;

    cpid = fork();
    
    if (cpid)
        parent_main(argc, argv);
    else
        child_main(argc, argv);
        
    waitpid(cpid, 0, 0);
    fprintf(stdout, "End main()\n");

    return(EXIT_SUCCESS);
}
