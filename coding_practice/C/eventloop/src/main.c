#include <unistd.h>
#include <stdlib.h>
#include <stdio.h>
#include <sys/eventfd.h>
#include <sys/stat.h>
#include <sys/wait.h>
#include <string.h>
#include <errno.h>
#include <signal.h>
#define handle_error(msg) \
    do { perror(msg); exit(EXIT_FAILURE); } while (0)

#define SIGNAL_QUIT 1
#define SIGNAL_FILE_DETECTED 2

int emit(int efd, unsigned long int signal) {
    return(write(efd, &signal, sizeof(unsigned long int)));
}

unsigned long int catch(int efd) {
    unsigned long int cs = 0;
    int status = read(efd, &cs, sizeof(unsigned long int));
    fprintf(stdout, "cs = %d\n", cs);
    return(cs);
}

int file_detected_loop(int efd, const unsigned char file_name[]) {
    struct stat s;
    while (1) {
        sleep(1);
        if (stat(file_name, &s) == 0) {
            emit(efd, SIGNAL_FILE_DETECTED);
        }
        if (stat("end.txt", &s) == 0) {
            emit(efd, SIGNAL_QUIT);
            break;
        }
    }
    exit(EXIT_SUCCESS); // Die child process
}

static const unsigned char file[] = "test.txt";

int process_signal(unsigned long int signal) {
    switch (signal) {
        case SIGNAL_FILE_DETECTED:
            return(unlink(file));
            break;
        case SIGNAL_QUIT:
            unlink("end.txt");
            break;
        default:
            return(0);
            break;
    }
    return(0);
}

int main(int argc, char *argv[]) {
    int efd;
    int cpid = 0;
    
    efd = eventfd(0, 0);
    if (efd == -1)
        handle_error("eventfd");
    
    cpid = fork();
    if (cpid == 0) {
        /* This is the child */
        file_detected_loop(efd, file);
    } else {
        /* This is the parent */
        while (1) {
            fprintf(stdout, "Waiting for signal...\n");
            unsigned long int signal = catch(efd);
            process_signal(signal);
            if (signal == SIGNAL_QUIT)
                break;
        }
        waitpid(cpid, 0, 0);
    }
    printf("Done\n");
    return(EXIT_SUCCESS);
}
