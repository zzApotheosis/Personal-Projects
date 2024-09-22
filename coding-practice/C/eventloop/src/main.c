#include <unistd.h>
#include <stdlib.h>
#include <stdio.h>
#include <sys/eventfd.h>
#include <sys/stat.h>
#include <sys/wait.h>
#include <string.h>
#include <errno.h>
#include <signal.h>
#include <time.h>
#define handle_error(msg) \
    do { perror(msg); exit(EXIT_FAILURE); } while (0)

#define EVENT_QUIT          0b001
#define EVENT_FILE_DETECTED 0b010
#define EVENT_HELLO         0b100

int emit(int efd, unsigned long int signal) {
    return(write(efd, &signal, sizeof(unsigned long int)));
}

unsigned long int catch(int efd) {
    unsigned long int event = 0;
    int status = read(efd, &event, sizeof(unsigned long int));
    if (status != sizeof(unsigned long int))
        perror("read() error");
    return(event);
}

void dump_file(const char file_name[]) {
    struct stat s;
    memset(&s, 0, sizeof(struct stat));
    if (stat(file_name, &s) == 0) {
        size_t file_size = s.st_size;
        char * file_data = (char *) malloc(file_size * sizeof(char));
        memset(file_data, 0, file_size);
        FILE * fp = fopen(file_name, "r");
        if (fp == NULL) {
            perror("fopen() error");
            free(file_data);
            return;
        }
        if (fread(file_data, sizeof(char), file_size, fp) != sizeof(char) * file_size)
            perror("read() error");
        fwrite(file_data, sizeof(char), file_size, stdout);
        fclose(fp);
        free(file_data);
    } else {
        perror("stat() error");
    }
}

int file_detected_loop(const int efd, const char file_name[]) {
    struct stat s;
    while (1) {
        sleep(1);
        if (stat(file_name, &s) == 0) {
            emit(efd, EVENT_FILE_DETECTED);
        }
        if (stat("end.txt", &s) == 0) {
            emit(efd, EVENT_QUIT);
            break;
        }
    }
    sleep(5);
    exit(EXIT_SUCCESS); // Die child process
}

int random_hello(const int efd) {
    srand(time(NULL));
    while (1) {
        sleep(rand() % 10);
        emit(efd, EVENT_HELLO);
    }
}

static const char file[] = "test.txt";
static const char end_file[] = "end.txt";

int process_signal(const unsigned long int signal) {
    switch (signal) {
        case EVENT_FILE_DETECTED:
            dump_file(file);
            return(unlink(file));
            break;
        case EVENT_QUIT:
            unlink("end.txt");
            break;
        case EVENT_HELLO:
            fprintf(stdout, "Received hello event! Why, hello!\n");
            break;
        default:
            break;
    }
    return(0);
}

int main(const int argc, char * argv[]) {
    int efd;
    int cpid0 = 0, cpid1 = 0;
    
    efd = eventfd(0, 0);
    if (efd == -1)
        handle_error("eventfd");
    
    cpid0 = fork();
    if (cpid0 == 0) {
        /* This is the child */
        file_detected_loop(efd, file);
    }
    
    cpid1 = fork();
    if (cpid1 == 0) {
        random_hello(efd);
    }

    
    /* This is the parent performing the event loop */
    while (1) {
        fprintf(stdout, "Waiting for event...");
        fflush(stdout);
        unsigned long int signal = catch(efd);
        fprintf(stdout, " caught %lu\n", signal);
        process_signal(signal);
        if (signal == EVENT_QUIT)
            break;
    }
    waitpid(cpid0, 0, 0);
    kill(cpid1, SIGTERM);
    waitpid(cpid1, 0, 0);
    
    printf("Done\n");
    return(EXIT_SUCCESS);
}
